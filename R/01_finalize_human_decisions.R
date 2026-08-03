# ============================================================
# Universal SR-MA Agent Toolkit
# R/01_finalize_human_decisions.R
# ============================================================

required_packages <- c("yaml", "jsonlite")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing_packages) > 0L) {
  stop("Missing required packages: ", paste(missing_packages, collapse = ", "))
}

required_module_files <- c(
  "R/utils/human_decision_patch_utils.R",
  "R/internal/approve_human_decisions.R",
  "R/internal/apply_human_decision_patches.R",
  "R/internal/verify_human_decision_application.R",
  "R/internal/rollback_human_decision_changes.R"
)
missing_module_files <- required_module_files[!file.exists(required_module_files)]
if (length(missing_module_files) > 0L) {
  stop("Missing Human Decision module files:\n- ", paste(missing_module_files, collapse = "\n- "))
}
invisible(lapply(required_module_files, source, local = FALSE))

hd_paths <- list(
  decisions_required = "config/generated/initialization_decisions_required.yml",
  responses = "config/generated/human_decision_responses.yml",
  project_draft = "config/generated/project_config_draft.yml",
  search_draft = "config/generated/search_terms_draft.yml",
  project_resolved = "config/generated/project_config_resolved.yml",
  search_resolved = "config/generated/search_terms_resolved.yml",
  backup_directory = "config/generated/backups",
  audit_directory = "results/audit",
  verification_report = "config/generated/human_decision_verification.yml",
  resolved_decisions_archive = "config/generated/human_decisions_resolved.yml",
  activation_script = "R/02_activate_project_configuration.R"
)

print_hd_header <- function(title) {
  cat("\n", paste(rep("=", 68L), collapse = ""), "\n", title, "\n",
      paste(rep("=", 68L), collapse = ""), "\n", sep = "")
}

read_menu_choice <- function(prompt, valid_choices) {
  repeat {
    answer <- trimws(readline(prompt))
    if (answer %in% valid_choices) return(answer)
    cat("Invalid selection. Enter one of: ", paste(valid_choices, collapse = ", "), "\n", sep = "")
  }
}

load_finalization_inputs <- function(paths = hd_paths) {
  required_files <- c(paths$decisions_required, paths$responses, paths$project_draft, paths$search_draft)
  missing_files <- required_files[!file.exists(required_files)]
  if (length(missing_files) > 0L) {
    stop("Missing required files:\n- ", paste(missing_files, collapse = "\n- "))
  }

  decisions_document <- yaml::read_yaml(paths$decisions_required)
  responses_document <- yaml::read_yaml(paths$responses)

  decisions <- normalize_hd_records(decisions_document$human_decisions_required)
  responses <- normalize_hd_response_records(
    responses_document$human_decision_responses %||%
      responses_document$decisions %||%
      responses_document
  )

  if (length(decisions) == 0L) stop("No Human Decisions were found.")

  list(
    decisions = decisions,
    responses = responses,
    project_config = yaml::read_yaml(paths$project_draft),
    search_terms = yaml::read_yaml(paths$search_draft)
  )
}

run_interactive_human_decision_finalization <- function(paths = hd_paths) {
  if (!interactive()) {
    stop("Run this script in an interactive RStudio or R console session.")
  }

  print_hd_header("Universal SR-MA Toolkit: Human Decision Finalization")
  inputs <- load_finalization_inputs(paths)
  decisions <- inputs$decisions
  responses <- inputs$responses

  cat(length(decisions), " human decisions require final approval.\n\n", sep = "")

  repeat {
    cat(
      "[1] Approve all recommended options\n",
      "[2] Review or modify individual decisions\n",
      "[3] Cancel finalization\n\n",
      sep = ""
    )

    workflow_choice <- read_menu_choice("Enter selection: ", c("1", "2", "3"))

    if (workflow_choice == "3") {
      cat("\nFinalization cancelled. No files were changed.\n")
      return(invisible(FALSE))
    }

    if (workflow_choice == "1") {
      responses <- approve_all_recommended_options(decisions, responses)
    } else {
      responses <- review_human_decisions_interactively(decisions, responses)
    }

    print_human_decision_summary(decisions, responses)

    cat(
      "\n[1] Approve these selections and continue\n",
      "[2] Return to decision review\n",
      "[3] Cancel finalization\n",
      sep = ""
    )
    confirmation <- read_menu_choice("Enter selection: ", c("1", "2", "3"))

    if (confirmation == "2") next
    if (confirmation == "3") {
      cat("\nFinalization cancelled. No files were changed.\n")
      return(invisible(FALSE))
    }
    break
  }

  validate_human_decision_approvals(decisions, responses)
  save_human_decision_responses(responses, paths$responses)

  backup_manifest <- create_human_decision_backup(
    files = c(
      paths$project_draft,
      paths$search_draft,
      paths$responses,
      paths$decisions_required
    ),
    backup_directory = paths$backup_directory
  )

  audit_context <- new_human_decision_audit_context(
    decisions = decisions,
    responses = responses,
    backup_manifest = backup_manifest
  )

  finalization_error <- NULL

  tryCatch({
    patch_plan <- build_selected_patch_plan(decisions, responses)
    patch_result <- apply_human_decision_patch_plan(
      project_config = inputs$project_config,
      search_terms = inputs$search_terms,
      patch_plan = patch_plan
    )

    yaml::write_yaml(patch_result$project_config, paths$project_resolved)
    yaml::write_yaml(patch_result$search_terms, paths$search_resolved)

    verification <- verify_human_decision_application(
      decisions = decisions,
      responses = responses,
      patch_plan = patch_plan,
      patch_result = patch_result,
      project_config_file = paths$project_resolved,
      search_terms_file = paths$search_resolved
    )
    yaml::write_yaml(verification, paths$verification_report)

    if (!isTRUE(verification$passed)) {
      stop("Human Decision verification failed. Activation was blocked.")
    }

    # --------------------------------------------------------
    # Mark all Human Decisions as resolved before activation.
    #
    # The activation script intentionally blocks whenever
    # initialization_decisions_required.yml still contains
    # unresolved records. Preserve the full resolved history
    # in a separate archive, then replace the active unresolved
    # list with an empty list.
    # --------------------------------------------------------

    resolved_records <- lapply(
      decisions,
      function(decision) {
        response <- hd_find_response(
          responses,
          decision$decision_id
        )

        list(
          decision_id = decision$decision_id,
          question = decision$question,
          recommended_option = decision$recommended_option,
          selected_option = response$selected_option,
          status = response$status,
          approved_at = response$approved_at,
          approval_source = response$approval_source
        )
      }
    )

    yaml::write_yaml(
      list(
        resolved_human_decisions = resolved_records
      ),
      paths$resolved_decisions_archive
    )

    yaml::write_yaml(
      list(
        human_decisions_required = list()
      ),
      paths$decisions_required
    )

    if (!file.exists(paths$activation_script)) {
      stop("Activation script not found: ", paths$activation_script)
    }

    activation_environment <- new.env(parent = globalenv())
    activation_environment$project_config_source <- paths$project_resolved
    activation_environment$search_terms_source <- paths$search_resolved
    sys.source(paths$activation_script, envir = activation_environment)

    audit_context$status <- "completed"
    audit_context$completed_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    audit_context$patch_operation_count <- length(patch_plan$operations)
    audit_context$verification <- verification
    audit_file <- write_human_decision_audit_log(audit_context, paths$audit_directory)

    print_hd_header("Human Decision Finalization Completed")
    cat(
      length(decisions), "/", length(decisions), " human decisions approved.\n",
      length(patch_plan$operations), " patch operations applied.\n",
      "Patch verification passed.\n",
      "Project configuration activated.\n",
      "Audit log: ", audit_file, "\n",
      sep = ""
    )
  }, error = function(e) {
    finalization_error <<- e
    rollback_human_decision_changes(
      backup_manifest = backup_manifest,
      generated_files = c(
        paths$project_resolved,
        paths$search_resolved,
        paths$verification_report,
        paths$resolved_decisions_archive
      )
    )
    audit_context$status <- "failed"
    audit_context$completed_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    audit_context$error_message <- conditionMessage(e)
    audit_file <- write_human_decision_audit_log(audit_context, paths$audit_directory)
    cat("\nFinalization failed. Draft files were restored.\nAudit log: ", audit_file, "\n", sep = "")
  })

  if (!is.null(finalization_error)) stop(finalization_error)
  invisible(TRUE)
}

run_interactive_human_decision_finalization()
