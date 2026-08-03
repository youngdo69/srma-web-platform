# ============================================================
# Universal SR-MA Agent Toolkit
# R/internal/verify_human_decision_application.R
# ============================================================

verify_single_hd_operation <- function(
    document,
    operation
) {
  current <- hd_get_nested(
    document,
    operation$path
  )

  value <- hd_decode_json_value(
    operation$value_json
  )

  operation_name <- operation$operation

  if (identical(operation_name, "set")) {
    return(hd_values_equal(current, value))
  }

  if (identical(operation_name, "append_unique") ||
      identical(operation_name, "append_record")) {
    if (is.null(current)) return(FALSE)
    if (!is.list(current)) current <- as.list(current)

    return(any(vapply(
      current,
      function(item) hd_values_equal(item, value),
      logical(1)
    )))
  }

  if (identical(operation_name, "remove_value")) {
    if (is.null(current)) return(TRUE)
    if (!is.list(current)) current <- as.list(current)

    return(!any(vapply(
      current,
      function(item) hd_values_equal(item, value),
      logical(1)
    )))
  }

  if (identical(
    operation_name,
    "remove_record_by_key"
  )) {
    if (is.null(current)) return(TRUE)
    if (!is.list(current)) return(FALSE)

    match_key <- operation$match_key
    match_value <- hd_decode_json_value(
      operation$match_value_json
    )

    return(!any(vapply(
      current,
      function(record) {
        is.list(record) &&
          !is.null(record[[match_key]]) &&
          hd_values_equal(
            record[[match_key]],
            match_value
          )
      },
      logical(1)
    )))
  }

  FALSE
}

find_placeholder_paths <- function(
    x,
    path = "root"
) {
  hits <- character(0)

  if (is.character(x)) {
    bad <- grepl(
      "REPLACE_WITH|TODO|TBD|PLACEHOLDER",
      x,
      ignore.case = TRUE
    )

    if (any(bad)) {
      hits <- c(
        hits,
        paste0(path, "[", which(bad), "]")
      )
    }

    return(hits)
  }

  if (!is.list(x)) return(hits)

  nms <- names(x)

  for (i in seq_along(x)) {
    child_path <- if (!is.null(nms) &&
                      nzchar(nms[[i]])) {
      paste0(path, "$", nms[[i]])
    } else {
      paste0(path, "[[", i, "]]")
    }

    hits <- c(
      hits,
      find_placeholder_paths(
        x[[i]],
        child_path
      )
    )
  }

  unique(hits)
}

verify_human_decision_application <- function(
    decisions,
    responses,
    patch_plan,
    patch_result,
    project_config_file,
    search_terms_file
) {
  checks <- list()

  checks$all_decisions_approved <- tryCatch(
    {
      validate_human_decision_approvals(
        decisions,
        responses
      )
      TRUE
    },
    error = function(e) FALSE
  )

  checks$resolved_files_exist <- all(
    file.exists(
      c(
        project_config_file,
        search_terms_file
      )
    )
  )

  operation_checks <- lapply(
    patch_plan$operations,
    function(operation) {
      document <- if (
        identical(operation$target, "project_config")
      ) {
        patch_result$project_config
      } else if (
        identical(operation$target, "search_terms")
      ) {
        patch_result$search_terms
      } else {
        NULL
      }

      passed <- if (is.null(document)) {
        FALSE
      } else {
        verify_single_hd_operation(
          document,
          operation
        )
      }

      list(
        decision_id = operation$decision_id,
        selected_option = operation$selected_option,
        target = operation$target,
        path = operation$path,
        operation = operation$operation,
        passed = passed
      )
    }
  )

  checks$all_patch_operations_verified <- if (
    length(operation_checks) == 0L
  ) {
    TRUE
  } else {
    all(vapply(
      operation_checks,
      function(x) isTRUE(x$passed),
      logical(1)
    ))
  }

  project_placeholders <- find_placeholder_paths(
    patch_result$project_config,
    "project_config"
  )

  search_placeholders <- find_placeholder_paths(
    patch_result$search_terms,
    "search_terms"
  )

  checks$no_placeholders <- (
    length(project_placeholders) == 0L &&
    length(search_placeholders) == 0L
  )

  checks$project_config_nonempty <- (
    is.list(patch_result$project_config) &&
    length(patch_result$project_config) > 0L
  )

  checks$search_terms_nonempty <- (
    is.list(patch_result$search_terms) &&
    length(patch_result$search_terms) > 0L
  )

  passed <- all(vapply(
    checks,
    isTRUE,
    logical(1)
  ))

  list(
    passed = passed,
    verified_at = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    ),
    checks = checks,
    operation_checks = operation_checks,
    unresolved_placeholders = c(
      project_placeholders,
      search_placeholders
    ),
    summary = list(
      decision_count = length(decisions),
      patch_operation_count =
        length(patch_plan$operations),
      verified_operation_count = sum(vapply(
        operation_checks,
        function(x) isTRUE(x$passed),
        logical(1)
      ))
    )
  )
}
