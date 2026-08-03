# ============================================================
# Universal SR-MA Agent Toolkit
# R/internal/approve_human_decisions.R
# ============================================================

approve_all_recommended_options <- function(
    decisions,
    responses
) {
  approved_at <- format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )

  for (decision in decisions) {
    response <- list(
      decision_id = decision$decision_id,
      selected_option = decision$recommended_option,
      status = "approved",
      approved_at = approved_at,
      approval_source = "approve_all_recommended"
    )

    responses <- hd_upsert_response(
      responses,
      response
    )
  }

  responses
}

review_human_decisions_interactively <- function(
    decisions,
    responses
) {
  for (i in seq_along(decisions)) {
    decision <- decisions[[i]]
    existing <- hd_find_response(
      responses,
      decision$decision_id
    )

    cat(
      "\n",
      paste(rep("-", 68L), collapse = ""),
      "\n",
      "[", i, "/", length(decisions), "] ",
      decision$decision_id,
      "\n",
      decision$question,
      "\n\nWhy it matters:\n",
      decision$why_it_matters,
      "\n\nRecommended option:\n",
      decision$recommended_option,
      "\n\nRationale:\n",
      decision$recommendation_rationale,
      "\n\nOptions:\n",
      sep = ""
    )

    options <- decision$suggested_options

    for (j in seq_along(options)) {
      marker <- if (
        identical(options[[j]], decision$recommended_option)
      ) {
        " [recommended]"
      } else {
        ""
      }

      cat(
        "[", j, "] ",
        options[[j]],
        marker,
        "\n",
        sep = ""
      )
    }

    current_selection <- existing$selected_option %||% ""

    if (nzchar(current_selection)) {
      cat(
        "Current selection: ",
        current_selection,
        "\n",
        sep = ""
      )
    }

    recommended_index <- match(
      decision$recommended_option,
      options
    )

    repeat {
      answer <- trimws(
        readline(
          paste0(
            "Press Enter for recommendation",
            if (!is.na(recommended_index)) {
              paste0(" [", recommended_index, "]")
            } else {
              ""
            },
            ", or enter option number: "
          )
        )
      )

      if (!nzchar(answer)) {
        selected <- decision$recommended_option
        break
      }

      choice <- suppressWarnings(as.integer(answer))

      if (
        !is.na(choice) &&
        choice >= 1L &&
        choice <= length(options)
      ) {
        selected <- options[[choice]]
        break
      }

      cat("Invalid option. Please try again.\n")
    }

    response <- list(
      decision_id = decision$decision_id,
      selected_option = selected,
      status = "approved",
      approved_at = format(
        Sys.time(),
        "%Y-%m-%d %H:%M:%S"
      ),
      approval_source = "interactive_review"
    )

    responses <- hd_upsert_response(
      responses,
      response
    )
  }

  responses
}

print_human_decision_summary <- function(
    decisions,
    responses
) {
  cat(
    "\n",
    paste(rep("-", 68L), collapse = ""),
    "\nSelection summary\n",
    paste(rep("-", 68L), collapse = ""),
    "\n",
    sep = ""
  )

  for (decision in decisions) {
    response <- hd_find_response(
      responses,
      decision$decision_id
    )

    selected <- response$selected_option %||% "<unresolved>"
    recommendation_marker <- if (
      identical(selected, decision$recommended_option)
    ) {
      " [recommended]"
    } else {
      " [modified]"
    }

    cat(
      "- ",
      decision$decision_id,
      ": ",
      selected,
      recommendation_marker,
      "\n",
      sep = ""
    )
  }
}

validate_human_decision_approvals <- function(
    decisions,
    responses
) {
  decision_ids <- vapply(
    decisions,
    function(x) x$decision_id,
    character(1)
  )

  if (anyDuplicated(decision_ids)) {
    stop("Duplicate decision_id values were found.")
  }

  problems <- character(0)

  for (decision in decisions) {
    response <- hd_find_response(
      responses,
      decision$decision_id
    )

    if (is.null(response)) {
      problems <- c(
        problems,
        paste0(decision$decision_id, ": missing response")
      )
      next
    }

    if (!identical(response$status, "approved")) {
      problems <- c(
        problems,
        paste0(decision$decision_id, ": status is not approved")
      )
    }

    if (!nzchar(response$selected_option %||% "")) {
      problems <- c(
        problems,
        paste0(decision$decision_id, ": selected_option is empty")
      )
    } else if (
      !response$selected_option %in% decision$suggested_options
    ) {
      problems <- c(
        problems,
        paste0(
          decision$decision_id,
          ": selected_option is not one of suggested_options"
        )
      )
    }

    available_patch_options <- vapply(
      decision$option_patches,
      function(x) x$option,
      character(1)
    )

    if (
      nzchar(response$selected_option %||% "") &&
      !response$selected_option %in% available_patch_options
    ) {
      problems <- c(
        problems,
        paste0(
          decision$decision_id,
          ": no option_patch matches selected_option"
        )
      )
    }
  }

  if (length(problems) > 0L) {
    stop(
      "Human Decision approval validation failed:\n- ",
      paste(problems, collapse = "\n- ")
    )
  }

  invisible(TRUE)
}
