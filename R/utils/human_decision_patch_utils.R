# ============================================================
# Universal SR-MA Agent Toolkit
# R/utils/human_decision_patch_utils.R
#
# Shared helpers for Human Decision normalization, patch planning,
# nested-list operations, response persistence, backups, and audits.
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

hd_as_character_vector <- function(x) {
  if (is.null(x)) return(character(0))
  as.character(unlist(x, recursive = TRUE, use.names = FALSE))
}

hd_sanitize_json_control_characters <- function(x) {
  if (is.null(x) || length(x) == 0L) return(x)

  x <- as.character(x)

  vapply(
    x,
    function(value) {
      if (is.na(value)) return(NA_character_)

      value <- iconv(value, from = "", to = "UTF-8", sub = "")
      if (is.na(value)) value <- ""

      bytes <- charToRaw(value)
      ints <- as.integer(bytes)

      if (length(ints) == 0L) return("")

      out <- raw(0L)
      in_string <- FALSE
      escaped <- FALSE

      for (i in seq_along(ints)) {
        b <- ints[[i]]

        if (in_string) {
          if (escaped) {
            out <- c(out, as.raw(b))
            escaped <- FALSE
            next
          }

          if (b == 92L) {  # backslash
            out <- c(out, as.raw(b))
            escaped <- TRUE
            next
          }

          if (b == 34L) {  # double quote
            out <- c(out, as.raw(b))
            in_string <- FALSE
            next
          }

          # JSON forbids unescaped U+0000-U+001F inside strings.
          # Replace with a single space so adjacent words do not collapse.
          if (b < 32L) {
            out <- c(out, charToRaw(" "))
            next
          }

          out <- c(out, as.raw(b))
          next
        }

        if (b == 34L) {  # opening double quote
          out <- c(out, as.raw(b))
          in_string <- TRUE
          escaped <- FALSE
          next
        }

        # Outside strings, preserve standard JSON whitespace only.
        if (b < 32L && !(b %in% c(9L, 10L, 13L))) {
          next
        }

        out <- c(out, as.raw(b))
      }

      rawToChar(out, multiple = FALSE)
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}

hd_decode_json_value <- function(value_json) {
  if (is.null(value_json) || length(value_json) == 0L) {
    return(NULL)
  }

  value_json <- as.character(value_json[[1]])

  if (!nzchar(value_json)) {
    return(NULL)
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(
      value_json,
      simplifyVector = FALSE
    ),
    error = function(e) e
  )

  if (!inherits(parsed, "error")) {
    return(parsed)
  }

  sanitized <- hd_sanitize_json_control_characters(value_json)

  if (identical(sanitized, value_json)) {
    stop(
      "Failed to decode Human Decision JSON patch value: ",
      conditionMessage(parsed),
      call. = FALSE
    )
  }

  reparsed <- tryCatch(
    jsonlite::fromJSON(
      sanitized,
      simplifyVector = FALSE
    ),
    error = function(e) e
  )

  if (inherits(reparsed, "error")) {
    stop(
      "Failed to decode Human Decision JSON patch value after removing ",
      "invalid control characters. Original error: ",
      conditionMessage(parsed),
      "\nSanitized parse error: ",
      conditionMessage(reparsed),
      call. = FALSE
    )
  }

  warning(
    "A Human Decision JSON patch value contained invalid control ",
    "character(s); they were replaced with spaces before JSON decoding.",
    call. = FALSE
  )

  reparsed
}

normalize_hd_operation_records <- function(x) {
  if (is.null(x) || length(x) == 0L) return(list())

  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) {
      row <- as.list(x[i, , drop = FALSE])
      lapply(row, function(v) {
        if (is.list(v) && length(v) == 1L) v[[1]] else v
      })
    }))
  }

  if (!is.list(x)) {
    stop("HD operations must be a list or data frame.")
  }

  # Column-oriented list produced by YAML/data-frame conversions.
  if (!is.null(names(x)) &&
      all(c("target", "path", "operation") %in% names(x)) &&
      length(x$target) > 1L) {
    n <- length(x$target)
    return(lapply(seq_len(n), function(i) {
      list(
        target = as.character(x$target[[i]] %||% ""),
        path = hd_as_character_vector(x$path[[i]] %||% character(0)),
        operation = as.character(x$operation[[i]] %||% ""),
        value_json = as.character(x$value_json[[i]] %||% ""),
        match_key = as.character(x$match_key[[i]] %||% ""),
        match_value_json = as.character(
          x$match_value_json[[i]] %||% ""
        ),
        rationale = as.character(x$rationale[[i]] %||% "")
      )
    }))
  }

  lapply(x, function(item) {
    if (is.data.frame(item)) {
      item <- as.list(item[1, , drop = FALSE])
    }

    list(
      target = as.character(item$target %||% ""),
      path = hd_as_character_vector(item$path %||% character(0)),
      operation = as.character(item$operation %||% ""),
      value_json = as.character(item$value_json %||% ""),
      match_key = as.character(item$match_key %||% ""),
      match_value_json = as.character(
        item$match_value_json %||% ""
      ),
      rationale = as.character(item$rationale %||% "")
    )
  })
}

normalize_hd_option_patch_records <- function(x) {
  if (is.null(x) || length(x) == 0L) return(list())

  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) {
      list(
        option = as.character(x$option[[i]] %||% ""),
        operations = normalize_hd_operation_records(
          x$operations[[i]] %||% list()
        )
      )
    }))
  }

  if (!is.list(x)) {
    stop("option_patches must be a list or data frame.")
  }

  # Column-oriented list.
  if (!is.null(names(x)) &&
      all(c("option", "operations") %in% names(x)) &&
      length(x$option) > 1L) {
    return(lapply(seq_along(x$option), function(i) {
      list(
        option = as.character(x$option[[i]] %||% ""),
        operations = normalize_hd_operation_records(
          x$operations[[i]] %||% list()
        )
      )
    }))
  }

  lapply(x, function(item) {
    if (is.data.frame(item)) {
      item <- as.list(item[1, , drop = FALSE])
    }

    list(
      option = as.character(item$option %||% ""),
      operations = normalize_hd_operation_records(
        item$operations %||% list()
      )
    )
  })
}

normalize_hd_records <- function(x) {
  if (is.null(x) || length(x) == 0L) return(list())

  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) {
      list(
        decision_id = as.character(x$decision_id[[i]] %||% ""),
        question = as.character(x$question[[i]] %||% ""),
        why_it_matters = as.character(x$why_it_matters[[i]] %||% ""),
        suggested_options = hd_as_character_vector(
          x$suggested_options[[i]] %||% character(0)
        ),
        recommended_option = as.character(
          x$recommended_option[[i]] %||% ""
        ),
        recommendation_rationale = as.character(
          x$recommendation_rationale[[i]] %||% ""
        ),
        affected_components = hd_as_character_vector(
          x$affected_components[[i]] %||% character(0)
        ),
        option_patches = normalize_hd_option_patch_records(
          x$option_patches[[i]] %||% list()
        )
      )
    }))
  }

  if (!is.list(x)) {
    stop("human_decisions_required must be a list or data frame.")
  }

  lapply(x, function(item) {
    if (is.data.frame(item)) {
      item <- as.list(item[1, , drop = FALSE])
    }

    list(
      decision_id = as.character(item$decision_id %||% ""),
      question = as.character(item$question %||% ""),
      why_it_matters = as.character(item$why_it_matters %||% ""),
      suggested_options = hd_as_character_vector(
        item$suggested_options %||% character(0)
      ),
      recommended_option = as.character(
        item$recommended_option %||% ""
      ),
      recommendation_rationale = as.character(
        item$recommendation_rationale %||% ""
      ),
      affected_components = hd_as_character_vector(
        item$affected_components %||% character(0)
      ),
      option_patches = normalize_hd_option_patch_records(
        item$option_patches %||% list()
      )
    )
  })
}

normalize_hd_response_records <- function(x) {
  if (is.null(x) || length(x) == 0L) return(list())

  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) {
      list(
        decision_id = as.character(x$decision_id[[i]] %||% ""),
        selected_option = as.character(
          x$selected_option[[i]] %||% ""
        ),
        status = as.character(x$status[[i]] %||% "pending"),
        approved_at = as.character(x$approved_at[[i]] %||% ""),
        approval_source = as.character(
          x$approval_source[[i]] %||% ""
        )
      )
    }))
  }

  # A single response object rather than list-of-records.
  if (!is.null(names(x)) && "decision_id" %in% names(x)) {
    x <- list(x)
  }

  lapply(x, function(item) {
    if (is.data.frame(item)) {
      item <- as.list(item[1, , drop = FALSE])
    }

    list(
      decision_id = as.character(item$decision_id %||% ""),
      selected_option = as.character(
        item$selected_option %||% ""
      ),
      status = as.character(item$status %||% "pending"),
      approved_at = as.character(item$approved_at %||% ""),
      approval_source = as.character(
        item$approval_source %||% ""
      )
    )
  })
}

hd_find_response <- function(responses, decision_id) {
  idx <- which(vapply(
    responses,
    function(x) identical(x$decision_id, decision_id),
    logical(1)
  ))

  if (length(idx) == 0L) return(NULL)
  responses[[idx[[1]]]]
}

hd_upsert_response <- function(responses, response) {
  idx <- which(vapply(
    responses,
    function(x) identical(x$decision_id, response$decision_id),
    logical(1)
  ))

  if (length(idx) == 0L) {
    return(c(responses, list(response)))
  }

  responses[[idx[[1]]]] <- response
  responses
}

save_human_decision_responses <- function(
    responses,
    output_file
) {
  dir.create(
    dirname(output_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  yaml::write_yaml(
    list(
      human_decision_responses = responses
    ),
    output_file
  )

  invisible(output_file)
}

hd_get_nested <- function(x, path) {
  if (length(path) == 0L) return(x)

  current <- x

  for (key in path) {
    if (!is.list(current) || is.null(current[[key]])) {
      return(NULL)
    }
    current <- current[[key]]
  }

  current
}

hd_set_nested <- function(x, path, value) {
  if (length(path) == 0L) return(value)

  key <- path[[1]]

  if (is.null(x) || !is.list(x)) {
    x <- list()
  }

  if (length(path) == 1L) {
    x[[key]] <- value
    return(x)
  }

  x[[key]] <- hd_set_nested(
    x[[key]],
    path[-1],
    value
  )

  x
}

hd_values_equal <- function(a, b) {
  identical(
    jsonlite::toJSON(a, auto_unbox = TRUE, null = "null"),
    jsonlite::toJSON(b, auto_unbox = TRUE, null = "null")
  )
}

create_human_decision_backup <- function(
    files,
    backup_directory
) {
  timestamp <- format(
    Sys.time(),
    "%Y%m%d_%H%M%S"
  )

  run_dir <- file.path(
    backup_directory,
    paste0("hd_finalization_", timestamp)
  )

  dir.create(
    run_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  records <- lapply(files, function(file) {
    existed <- file.exists(file)
    backup_file <- file.path(run_dir, basename(file))

    if (existed) {
      copied <- file.copy(
        file,
        backup_file,
        overwrite = TRUE
      )

      if (!isTRUE(copied)) {
        stop("Failed to back up: ", file)
      }
    }

    list(
      original = file,
      backup = if (existed) backup_file else "",
      existed = existed
    )
  })

  list(
    created_at = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    ),
    backup_directory = run_dir,
    files = records
  )
}

new_human_decision_audit_context <- function(
    decisions,
    responses,
    backup_manifest
) {
  list(
    workflow = "human_decision_finalization",
    status = "started",
    started_at = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    ),
    decision_count = length(decisions),
    decisions = lapply(decisions, function(decision) {
      response <- hd_find_response(
        responses,
        decision$decision_id
      )

      list(
        decision_id = decision$decision_id,
        recommended_option = decision$recommended_option,
        selected_option = response$selected_option %||% "",
        status = response$status %||% ""
      )
    }),
    backup_manifest = backup_manifest
  )
}

write_human_decision_audit_log <- function(
    audit_context,
    audit_directory
) {
  dir.create(
    audit_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  stamp <- format(
    Sys.time(),
    "%Y%m%d_%H%M%S"
  )

  output_file <- file.path(
    audit_directory,
    paste0("human_decision_audit_", stamp, ".yml")
  )

  yaml::write_yaml(
    audit_context,
    output_file
  )

  output_file
}
