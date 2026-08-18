# ============================================================
# Universal SR-MA Agent Toolkit
# TASK-017 — Completed-Project Operational Resume Verification
#
# This is an ACTUAL canonical Runner invocation, but it is guarded
# by a read-only preflight that requires every enabled stage to be
# already complete/recoverable before the Runner is called.
#
# Expected on the completed validation project:
#   33 recovered
#    1 skipped
#    0 completed
#    0 failed
#    0 paused
#
# Scientific modules must NOT execute during this verification.
# ============================================================

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop('Package "yaml" is required.', call. = FALSE)
}
if (!requireNamespace("readr", quietly = TRUE)) {
  stop('Package "readr" is required.', call. = FALSE)
}
if (!requireNamespace("tibble", quietly = TRUE)) {
  stop('Package "tibble" is required.', call. = FALSE)
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop('Package "dplyr" is required.', call. = FALSE)
}

if (!file.exists("R/run_universal_srma_workflow.R")) {
  stop(
    "Run this verification from the repository root. Missing canonical Runner.",
    call. = FALSE
  )
}

old_autorun <- getOption("srma.runner.autorun", TRUE)
options(srma.runner.autorun = FALSE)
on.exit(options(srma.runner.autorun = old_autorun), add = TRUE)

source(
  "R/run_universal_srma_workflow.R",
  local = .GlobalEnv
)

steps <- runner_steps(
  run_scope_advisory = TRUE,
  run_additional_analysis = FALSE
)

if (length(steps) != 34L) {
  stop(
    "TASK-017 requires the final 34-stage canonical Runner. Found: ",
    length(steps),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 1. Safety preflight
# ------------------------------------------------------------

preflight_rows <- vector("list", length(steps))

for (i in seq_along(steps)) {
  step <- steps[[i]]

  validation <- if (isTRUE(step$enabled)) {
    tryCatch(
      runner_validate_step(step, runner_paths()),
      error = function(e) {
        list(
          passed = FALSE,
          message = paste0(
            "VALIDATOR ERROR: ",
            conditionMessage(e)
          )
        )
      }
    )
  } else {
    list(
      passed = TRUE,
      message = "Optional stage disabled by Runner option."
    )
  }

  preflight_rows[[i]] <- tibble::tibble(
    order = i,
    step_id = step$id,
    category = step$category,
    type = step$type,
    enabled = isTRUE(step$enabled),
    validation_passed = isTRUE(validation$passed),
    validation_message = as.character(
      validation$message %||% ""
    )
  )
}

preflight <- dplyr::bind_rows(preflight_rows)

unsafe_enabled <- preflight[
  preflight$enabled &
    !preflight$validation_passed,
  ,
  drop = FALSE
]

if (nrow(unsafe_enabled) > 0L) {
  cat("\nTASK-017 SAFETY ABORT\n")
  cat("The actual Runner was NOT invoked.\n")
  cat(
    "Enabled stage(s) not currently recoverable:\n"
  )
  for (i in seq_len(nrow(unsafe_enabled))) {
    cat(
      "- ",
      unsafe_enabled$step_id[[i]],
      " | ",
      unsafe_enabled$validation_message[[i]],
      "\n",
      sep = ""
    )
  }

  stop(
    "Operational resume verification aborted before module execution risk.",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 2. Actual canonical Runner invocation
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("TASK-017 — Actual Completed-Project Resume Verification\n")
cat("============================================================\n\n")
cat(
  "Preflight PASS: every enabled stage is already recoverable.\n"
)
cat(
  "The Runner may now traverse the full graph without executing scientific modules.\n\n"
)

result <- run_universal_srma_workflow(
  resume = TRUE,
  run_scope_advisory = TRUE,
  run_additional_analysis = FALSE
)

if (!is.list(result)) {
  stop(
    "Canonical Runner did not return its expected result object.",
    call. = FALSE
  )
}

run_log_file <- as.character(
  result$run_log_file %||% ""
)

if (!nzchar(run_log_file) || !file.exists(run_log_file)) {
  stop(
    "Canonical Runner did not create a run log.",
    call. = FALSE
  )
}

run_log <- yaml::read_yaml(
  run_log_file
)

state_file <- runner_paths()$state_file

if (!file.exists(state_file)) {
  stop(
    "Canonical workflow state file is missing after the operational run.",
    call. = FALSE
  )
}

state <- runner_load_state(
  state_file,
  resume = TRUE
)

# ------------------------------------------------------------
# 3. Parse actual run-log outcomes
# ------------------------------------------------------------

step_ids <- vapply(
  steps,
  `[[`,
  character(1),
  "id"
)

run_step_records <- run_log$steps %||% list()

status_for <- function(step_id) {
  record <- run_step_records[[step_id]]

  if (is.null(record)) {
    return("MISSING")
  }

  toupper(
    trimws(
      as.character(
        record$status %||% ""
      )
    )
  )
}

actual_status <- vapply(
  step_ids,
  status_for,
  character(1)
)

evidence <- tibble::tibble(
  order = seq_along(step_ids),
  step_id = step_ids,
  category = vapply(
    steps,
    `[[`,
    character(1),
    "category"
  ),
  enabled = vapply(
    steps,
    function(x) isTRUE(x$enabled),
    logical(1)
  ),
  run_log_status = actual_status
)

count_status <- function(x) {
  sum(
    evidence$run_log_status == x
  )
}

recovered_n <- count_status("RECOVERED")
skipped_n <- count_status("SKIPPED")
completed_n <- count_status("COMPLETED")
failed_n <- count_status("FAILED")
paused_n <- count_status("PAUSED")
missing_n <- count_status("MISSING")

unexpected_statuses <- setdiff(
  unique(evidence$run_log_status),
  c(
    "RECOVERED",
    "SKIPPED"
  )
)

runner_version_state <- as.character(
  state$runner_version %||% ""
)

runner_version_log <- as.character(
  run_log$runner_version %||% ""
)

runner_version_expected <- as.character(
  runner_empty_state()$runner_version
)

state_completed_steps <- runner_as_character_vector(
  state$completed_steps
)

all_stage_ids_completed <- setequal(
  state_completed_steps,
  step_ids
)

pass <- (
  identical(
    as.character(run_log$status %||% ""),
    "completed"
  ) &&
    identical(
      as.character(state$status %||% ""),
      "completed"
    ) &&
    recovered_n == 33L &&
    skipped_n == 1L &&
    completed_n == 0L &&
    failed_n == 0L &&
    paused_n == 0L &&
    missing_n == 0L &&
    length(unexpected_statuses) == 0L &&
    all_stage_ids_completed &&
    identical(
      runner_version_state,
      runner_version_expected
    ) &&
    identical(
      runner_version_log,
      runner_version_expected
    )
)

# ------------------------------------------------------------
# 4. Evidence files
# ------------------------------------------------------------

out_dir <- "results/workflow"
dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

csv_file <- file.path(
  out_dir,
  "task017_operational_resume_verification.csv"
)

txt_file <- file.path(
  out_dir,
  "task017_operational_resume_verification.txt"
)

readr::write_csv(
  evidence,
  csv_file,
  na = ""
)

report_lines <- c(
  "============================================================",
  "TASK-017 Completed-Project Operational Resume Verification",
  "============================================================",
  "",
  paste0(
    "Canonical Runner version: ",
    runner_version_expected
  ),
  paste0(
    "Run log version: ",
    runner_version_log
  ),
  paste0(
    "State version: ",
    runner_version_state
  ),
  paste0(
    "Runner status: ",
    as.character(run_log$status %||% "")
  ),
  paste0(
    "Workflow state status: ",
    as.character(state$status %||% "")
  ),
  "",
  paste0("RECOVERED: ", recovered_n),
  paste0("SKIPPED: ", skipped_n),
  paste0("COMPLETED: ", completed_n),
  paste0("FAILED: ", failed_n),
  paste0("PAUSED: ", paused_n),
  paste0("MISSING: ", missing_n),
  "",
  paste0(
    "All 34 stage IDs completed in final state: ",
    if (all_stage_ids_completed) "YES" else "NO"
  ),
  paste0(
    "Scientific modules newly executed according to run log: ",
    if (completed_n == 0L) "NO" else "YES"
  ),
  "",
  paste0(
    "Operational resume verification: ",
    if (pass) "PASS" else "FAIL"
  ),
  "",
  paste0("Actual Runner run log: ", run_log_file),
  paste0("Evidence CSV: ", csv_file)
)

writeLines(
  report_lines,
  txt_file,
  useBytes = TRUE
)

cat(
  paste(
    report_lines,
    collapse = "\n"
  ),
  "\n"
)

if (!pass) {
  stop(
    "TASK-017 operational resume verification FAILED. Review the generated evidence before proceeding.",
    call. = FALSE
  )
}

cat("\n")
cat(
  "PASS: TASK-017 completed-project operational resume verification succeeded.\n"
)
cat(
  "No scientific module was newly executed according to the actual Runner run log.\n"
)

invisible(
  list(
    status = "PASS",
    evidence = evidence,
    run_log = run_log_file,
    csv = csv_file,
    report = txt_file
  )
)
