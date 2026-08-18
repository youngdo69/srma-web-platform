# ============================================================
# Universal SR-MA Agent Toolkit
# TASK-016 — Controlled Resume/Recovery Read-Only Audit
#
# PURPOSE
# ------------------------------------------------------------
# Read-only validation of the canonical Runner against the
# CURRENT repository artifacts and saved workflow state.
#
# IMPORTANT
# ------------------------------------------------------------
# - Does NOT execute analytical modules.
# - Does NOT write/modify universal_workflow_state.yml.
# - Does NOT advance Human Gates.
# - Sources the Runner with autorun disabled.
# - Produces an audit table showing what would be recovered,
#   what remains gated, and what would require execution.
# ============================================================

if (!file.exists("R/run_universal_srma_workflow.R")) {
  stop(
    "Run this audit from the repository root. Missing: R/run_universal_srma_workflow.R",
    call. = FALSE
  )
}

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop('Package "yaml" is required.', call. = FALSE)
}
if (!requireNamespace("readr", quietly = TRUE)) {
  stop('Package "readr" is required.', call. = FALSE)
}
if (!requireNamespace("tibble", quietly = TRUE)) {
  stop('Package "tibble" is required.', call. = FALSE)
}

old_autorun <- getOption("srma.runner.autorun", TRUE)
options(srma.runner.autorun = FALSE)
on.exit(options(srma.runner.autorun = old_autorun), add = TRUE)

source("R/run_universal_srma_workflow.R", local = .GlobalEnv)

paths <- runner_paths()

# Use the normal/default final workflow scope.
steps <- runner_steps(
  run_scope_advisory = TRUE,
  run_additional_analysis = FALSE
)

state <- runner_load_state(
  paths$state_file,
  resume = TRUE
)

rows <- vector("list", length(steps))

for (i in seq_along(steps)) {
  step <- steps[[i]]

  validation <- tryCatch(
    runner_validate_step(step, paths),
    error = function(e) {
      list(
        passed = FALSE,
        message = paste0("VALIDATOR ERROR: ", conditionMessage(e))
      )
    }
  )

  passed <- isTRUE(validation$passed)
  in_state <- step$id %in% runner_as_character_vector(state$completed_steps)

  predicted_action <- if (!isTRUE(step$enabled)) {
    "SKIP_OPTION_DISABLED"
  } else if (in_state && passed) {
    "ALREADY_COMPLETE"
  } else if (isTRUE(step$recovery) && passed) {
    "RECOVER_FROM_ARTIFACT"
  } else if (identical(step$type, "gate")) {
    "PAUSE_AT_HUMAN_GATE"
  } else {
    "WOULD_EXECUTE"
  }

  rows[[i]] <- tibble::tibble(
    order = i,
    step_id = step$id,
    label = step$label,
    category = step$category,
    type = step$type,
    enabled = isTRUE(step$enabled),
    recovery_allowed = isTRUE(step$recovery),
    state_completed = in_state,
    artifact_validation_passed = passed,
    predicted_runner_action = predicted_action,
    validation_message = as.character(validation$message %||% "")
  )
}

audit <- dplyr::bind_rows(rows)

action_counts <- c(
  ALREADY_COMPLETE = sum(audit$predicted_runner_action == "ALREADY_COMPLETE"),
  RECOVER_FROM_ARTIFACT = sum(audit$predicted_runner_action == "RECOVER_FROM_ARTIFACT"),
  PAUSE_AT_HUMAN_GATE = sum(audit$predicted_runner_action == "PAUSE_AT_HUMAN_GATE"),
  WOULD_EXECUTE = sum(audit$predicted_runner_action == "WOULD_EXECUTE"),
  SKIP_OPTION_DISABLED = sum(audit$predicted_runner_action == "SKIP_OPTION_DISABLED")
)

expected_counts <- c(
  ALREADY_COMPLETE = 14L,
  RECOVER_FROM_ARTIFACT = 19L,
  PAUSE_AT_HUMAN_GATE = 0L,
  WOULD_EXECUTE = 0L,
  SKIP_OPTION_DISABLED = 1L
)

counts_match_expected <- identical(
  as.integer(action_counts[names(expected_counts)]),
  as.integer(expected_counts)
)

out_dir <- "results/workflow"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

csv_file <- file.path(
  out_dir,
  "task016_controlled_resume_recovery_audit.csv"
)

txt_file <- file.path(
  out_dir,
  "task016_controlled_resume_recovery_audit.txt"
)

readr::write_csv(audit, csv_file, na = "")

summary_lines <- c(
  "============================================================",
  "TASK-016 Controlled Resume/Recovery Read-Only Audit",
  "============================================================",
  "",
  paste0("Runner version: ", state$runner_version %||% ""),
  paste0("Saved workflow status: ", state$status %||% ""),
  paste0("Saved current step: ", state$current_step %||% ""),
  paste0("Saved completed steps: ", length(runner_as_character_vector(state$completed_steps))),
  "",
  paste0(
    "ALREADY_COMPLETE: ",
    action_counts[["ALREADY_COMPLETE"]]
  ),
  paste0(
    "RECOVER_FROM_ARTIFACT: ",
    action_counts[["RECOVER_FROM_ARTIFACT"]]
  ),
  paste0(
    "PAUSE_AT_HUMAN_GATE: ",
    action_counts[["PAUSE_AT_HUMAN_GATE"]]
  ),
  paste0(
    "WOULD_EXECUTE: ",
    action_counts[["WOULD_EXECUTE"]]
  ),
  paste0(
    "SKIP_OPTION_DISABLED: ",
    action_counts[["SKIP_OPTION_DISABLED"]]
  ),
  paste0(
    "Expected 14 / 19 / 0 / 0 / 1 match: ",
    if (counts_match_expected) "YES" else "NO"
  ),
  "",
  "IMPORTANT",
  "------------------------------------------------------------",
  "This audit is read-only.",
  "No analytical module was executed.",
  "No Human Gate was advanced.",
  "No workflow state was modified.",
  "",
  "CSV:",
  csv_file,
  "",
  "Rows requiring attention:",
  "------------------------------------------------------------"
)

attention <- audit[
  audit$predicted_runner_action %in% c(
    "PAUSE_AT_HUMAN_GATE",
    "WOULD_EXECUTE"
  ),
  ,
  drop = FALSE
]

if (nrow(attention) == 0L) {
  summary_lines <- c(
    summary_lines,
    "None."
  )
} else {
  for (i in seq_len(nrow(attention))) {
    summary_lines <- c(
      summary_lines,
      paste0(
        "- ",
        attention$step_id[[i]],
        " | ",
        attention$predicted_runner_action[[i]],
        " | ",
        attention$validation_message[[i]]
      )
    )
  }
}

writeLines(summary_lines, txt_file, useBytes = TRUE)

cat(paste(summary_lines, collapse = "\n"), "\n")

if (!counts_match_expected) {
  stop(
    paste0(
      "TASK-016 recovery audit counts do not match expected 14 / 19 / 0 / 0 / 1. ",
      "Review the regenerated CSV/TXT before Reviewer resubmission."
    ),
    call. = FALSE
  )
}

cat("\nPASS: read-only resume/recovery audit completed and expected counts matched.\n")
cat("No Runner state or analytical outputs were modified.\n")

invisible(
  list(
    audit = audit,
    state = state,
    csv = csv_file,
    report = txt_file
  )
)
