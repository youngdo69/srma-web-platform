# ============================================================
# Universal SR-MA Agent Toolkit
# TASK-016 — Final Runner Integration Structural Smoke Test
#
# PURPOSE
# ------------------------------------------------------------
# Validate the integrated Runner contract WITHOUT executing any
# scientific workflow module.
#
# Usage from repository root:
#   options(srma.runner.autorun = FALSE)
#   source("R/run_universal_srma_workflow.R")
#   source("R/TASK016_validate_runner_integration.R")
#
# Or run this file directly after copying it under R/.
# Optional runner override:
#   Sys.setenv(SRMA_RUNNER_FILE = "R/run_universal_srma_workflow.R")
# ============================================================

runner_file <- Sys.getenv(
  "SRMA_RUNNER_FILE",
  unset = "R/run_universal_srma_workflow.R"
)

if (!file.exists(runner_file)) {
  stop(
    "TASK-016 smoke test cannot find Runner: ",
    runner_file,
    call. = FALSE
  )
}

old_autorun <- getOption("srma.runner.autorun")
on.exit(
  {
    if (is.null(old_autorun)) {
      options(srma.runner.autorun = NULL)
    } else {
      options(srma.runner.autorun = old_autorun)
    }
  },
  add = TRUE
)

options(srma.runner.autorun = FALSE)
source(runner_file, local = globalenv())

failures <- character(0)
passes <- character(0)

check_true <- function(condition, label, detail = "") {
  if (isTRUE(condition)) {
    passes <<- c(
      passes,
      paste0(label, if (nzchar(detail)) paste0(" — ", detail) else "")
    )
  } else {
    failures <<- c(
      failures,
      paste0(label, if (nzchar(detail)) paste0(" — ", detail) else "")
    )
  }
  invisible(condition)
}

required_functions <- c(
  "runner_steps",
  "runner_empty_state",
  "runner_load_state",
  "runner_write_state",
  "runner_record_step",
  "runner_validate_step",
  "runner_execute_script",
  "runner_execute_prospero",
  "runner_route_r29_uploads",
  "runner_pause",
  "run_universal_srma_workflow"
)

function_status <- vapply(
  required_functions,
  exists,
  logical(1),
  mode = "function",
  inherits = TRUE
)

check_true(
  all(function_status),
  "Required Runner functions are defined",
  if (all(function_status)) {
    paste(length(required_functions), "functions")
  } else {
    paste("missing:", paste(names(function_status)[!function_status], collapse = ", "))
  }
)

steps <- runner_steps(
  run_scope_advisory = TRUE,
  run_additional_analysis = TRUE
)

step_ids <- vapply(
  steps,
  `[[`,
  character(1),
  "id"
)

expected_ids <- c(
  "initialization_agent",
  "initialization_validation",
  "human_decision_review",
  "human_decision_finalization",
  "prospero_preparation",
  "search_generation",
  "search_validation",
  "pubmed_download",
  "database_input_gate",
  "merge_deduplication",
  "screening_workbook",
  "agent_screening",
  "agent_validation",
  "human_reviewer_2_gate",
  "human_adjudication_gate",
  "screening_finalization",
  "full_text_registry",
  "full_text_agent_eligibility",
  "full_text_author_gate",
  "final_included_studies",
  "prisma",
  "multimodal_extraction",
  "manuscript_tables",
  "scope_advisory",
  "targeted_verification_prep",
  "extraction_human_gate",
  "human_verified_finalization",
  "meta_analysis",
  "meta_analysis_diagnostics",
  "additional_analysis",
  "forest_plots",
  "risk_of_bias",
  "rob_author_gate",
  "rob_finalization"
)

check_true(
  identical(step_ids, expected_ids),
  "Final stage graph order",
  paste(length(step_ids), "stages")
)

check_true(
  !anyDuplicated(step_ids),
  "Stage IDs are unique"
)

index_of <- function(id) match(id, step_ids)

check_true(
  index_of("prospero_preparation") < index_of("search_generation"),
  "PROSPERO preparation precedes search"
)

prospero_body <- paste(
  deparse(body(runner_validate_prospero)),
  collapse = "\n"
)

check_true(
  grepl("context\\$paths\\$project_root", prospero_body, perl = TRUE) &&
    !grepl("runner_detect_project_slug", prospero_body, fixed = TRUE) &&
    !grepl("runner_find_files", prospero_body, fixed = TRUE) &&
    !grepl("runner_find_nonempty", prospero_body, fixed = TRUE) &&
    !grepl('file.path\\("projects"', prospero_body, perl = TRUE),
  "PROSPERO recovery uses active-project canonical path",
  "results/registration/prospero only; no projects/* rediscovery"
)

prospero_test_root <- tempfile("task016_prospero_isolation_")

dir.create(
  file.path(
    prospero_test_root,
    "projects",
    "other_project",
    "registration",
    "prospero"
  ),
  recursive = TRUE,
  showWarnings = FALSE
)

writeLines(
  "dummy",
  file.path(
    prospero_test_root,
    "projects",
    "other_project",
    "registration",
    "prospero",
    "prospero_final.xlsx"
  )
)

active_candidates <- runner_prospero_canonical_candidates(
  prospero_test_root
)

other_project_cannot_satisfy <- !any(
  vapply(
    active_candidates,
    runner_file_nonempty,
    logical(1)
  )
)

check_true(
  other_project_cannot_satisfy &&
    all(
      grepl(
        file.path(
          prospero_test_root,
          "results",
          "registration",
          "prospero"
        ),
        active_candidates,
        fixed = TRUE
      )
    ),
  "PROSPERO canonical path isolation regression",
  "same-named artifact in another dummy project cannot satisfy active project"
)

unlink(
  prospero_test_root,
  recursive = TRUE,
  force = TRUE
)

check_true(
  index_of("meta_analysis") <
    index_of("meta_analysis_diagnostics") &&
    index_of("meta_analysis_diagnostics") <
    index_of("additional_analysis"),
  "R/28 diagnostics are between R/27 and R/29"
)

category_map <- setNames(
  vapply(steps, `[[`, character(1), "category"),
  step_ids
)

gate_ids <- c(
  "human_decision_finalization",
  "database_input_gate",
  "human_reviewer_2_gate",
  "human_adjudication_gate",
  "full_text_author_gate",
  "extraction_human_gate",
  "rob_author_gate"
)

check_true(
  all(category_map[gate_ids] == "HUMAN GATE"),
  "Human gates preserve HUMAN GATE classification"
)

check_true(
  identical(category_map[["scope_advisory"]], "OPTIONAL") &&
    identical(category_map[["additional_analysis"]], "OPTIONAL"),
  "Optional R/25A and R/29 classifications"
)

check_true(
  identical(category_map[["meta_analysis_diagnostics"]], "RUN"),
  "R/28 is a RUN stage"
)

step_script_text <- paste(
  unlist(lapply(steps, `[[`, "scripts"), use.names = FALSE),
  collapse = "\n"
)

check_true(
  !grepl("R/30_", step_script_text, fixed = TRUE) &&
    !grepl("R/31_", step_script_text, fixed = TRUE),
  "R/30-R/31 maintenance modules are excluded"
)

check_true(
  !grepl("R/32_", step_script_text, fixed = TRUE) &&
    exists("runner_route_r29_uploads", mode = "function"),
  "R/32 is service-only, not a sequential stage"
)

# Validate that each executable stage has at least one current script candidate.
script_stage <- vapply(
  steps,
  function(x) x$type %in% c("script", "script_success", "external_script", "r29"),
  logical(1)
)

resolved_scripts <- vapply(
  steps[script_stage],
  function(x) runner_resolve_script(x$scripts),
  character(1)
)

missing_script_ids <- step_ids[script_stage][!nzchar(resolved_scripts)]

check_true(
  length(missing_script_ids) == 0L,
  "All executable stages resolve to a canonical module file",
  if (length(missing_script_ids) == 0L) {
    paste(length(resolved_scripts), "resolved modules")
  } else {
    paste("missing:", paste(missing_script_ids, collapse = ", "))
  }
)

# Verify the validator dispatch includes every stage except R/29, whose
# state machine is intentionally handled directly by the Runner.
validator_text <- paste(deparse(body(runner_validate_step)), collapse = "\n")
validator_required <- setdiff(step_ids, "additional_analysis")
validator_covered <- vapply(
  validator_required,
  function(id) grepl(id, validator_text, fixed = TRUE),
  logical(1)
)

check_true(
  all(validator_covered),
  "Completion validator dispatch covers every non-R/29 stage",
  if (all(validator_covered)) {
    paste(length(validator_required), "validator routes")
  } else {
    paste("missing:", paste(validator_required[!validator_covered], collapse = ", "))
  }
)

# Topic-neutrality guard for the Runner itself.
runner_text <- paste(
  readLines(runner_file, warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

topic_tokens <- c(
  "SGLT",
  "cardiac",
  "SCR_000"
)

runner_text_lower <- tolower(runner_text)

topic_hits <- topic_tokens[
  vapply(
    topic_tokens,
    function(x) grepl(tolower(x), runner_text_lower, fixed = TRUE),
    logical(1)
  )
]

check_true(
  length(topic_hits) == 0L,
  "Runner core is topic-neutral",
  if (length(topic_hits) == 0L) "no project-specific tokens" else paste(topic_hits, collapse = ", ")
)

# Canonical-path isolation guard: normal recovery must not use repository-wide
# newest-file discovery or generic project-tree artifact searches.
banned_recovery_helpers <- c(
  "runner_find_files <- function",
  "runner_find_nonempty <- function",
  "runner_choose_newest <- function"
)

banned_hits <- banned_recovery_helpers[
  vapply(
    banned_recovery_helpers,
    function(x) grepl(x, runner_text, fixed = TRUE),
    logical(1)
  )
]

check_true(
  length(banned_hits) == 0L,
  "Repository-wide newest-file recovery helpers are absent",
  if (length(banned_hits) == 0L) {
    "active-project canonical paths only"
  } else {
    paste("found:", paste(banned_hits, collapse = ", "))
  }
)

# Negative regression: same-named R/21 artifacts in another dummy project
# must not satisfy the active project's canonical R/21 completion validator.
isolation_root <- tempfile("task016_path_isolation_")
dir.create(isolation_root, recursive = TRUE, showWarnings = FALSE)

active_root <- file.path(isolation_root, "active_project")
other_root <- file.path(isolation_root, "projects", "dummy_other")

active_screening_results <- file.path(active_root, "results", "screening")
active_processed_screening <- file.path(active_root, "data", "processed", "screening")
active_logs <- file.path(active_root, "results", "logs")

dir.create(active_screening_results, recursive = TRUE, showWarnings = FALSE)
dir.create(active_processed_screening, recursive = TRUE, showWarnings = FALSE)
dir.create(active_logs, recursive = TRUE, showWarnings = FALSE)

dir.create(file.path(other_root, "results", "screening"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(other_root, "data", "processed", "screening"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(other_root, "results", "logs"), recursive = TRUE, showWarnings = FALSE)

# Create valid-looking same-named artifacts ONLY in the other project.
writeLines(
  "dummy workbook artifact",
  file.path(other_root, "results", "screening", "title_abstract_screening_completed.xlsx")
)
writeLines(
  "screening_id,final_decision\nX,Include",
  file.path(other_root, "data", "processed", "screening", "final_title_abstract_screening.csv")
)
writeLines(
  "screening_id,final_decision\nX,Include",
  file.path(other_root, "data", "processed", "screening", "included_for_full_text_review.csv")
)
writeLines(
  "PASS",
  file.path(other_root, "results", "logs", "final_title_abstract_screening_report.txt")
)

fake_context <- list(
  environment = new.env(parent = globalenv()),
  config = list(project = list(project_id = "active_project")),
  paths = list(
    project_root = normalizePath(active_root, winslash = "/", mustWork = TRUE),
    completed_screening_workbook = file.path(active_screening_results, "title_abstract_screening_completed.xlsx"),
    processed_screening_directory = active_processed_screening,
    screening_results_directory = active_screening_results,
    logs_directory = active_logs
  )
)

original_resolve_project_context <- runner_resolve_project_context
assign(
  "runner_resolve_project_context",
  function() fake_context,
  envir = globalenv()
)

r21_isolation <- tryCatch(
  runner_validate_phase1_finalization(runner_paths()),
  error = function(e) list(passed = FALSE, message = conditionMessage(e))
)

human2_isolation <- tryCatch(
  runner_validate_human_reviewer_2(runner_paths()),
  error = function(e) list(passed = FALSE, message = conditionMessage(e))
)

adjudication_isolation <- tryCatch(
  runner_validate_human_adjudication(runner_paths()),
  error = function(e) list(passed = FALSE, message = conditionMessage(e))
)

assign(
  "runner_resolve_project_context",
  original_resolve_project_context,
  envir = globalenv()
)

check_true(
  !isTRUE(r21_isolation$passed),
  "Canonical R/21 path isolation regression",
  "other-project R/21 artifacts cannot complete the active project"
)

check_true(
  !isTRUE(human2_isolation$passed) &&
    !isTRUE(adjudication_isolation$passed),
  "Human Reviewer 2/adjudication recovery isolation",
  "other-project R/21 artifacts cannot recover Human Gates"
)

unlink(
  isolation_root,
  recursive = TRUE,
  force = TRUE
)

# Dependency fail-closed regression for R/19 completion validation.
agent_test_root <- tempfile("task016_agent_fail_closed_")
dir.create(agent_test_root, recursive = TRUE, showWarnings = FALSE)

agent_test_paths <- list(
  workbook = file.path(agent_test_root, "title_abstract_screening_agent.xlsx"),
  decisions = file.path(agent_test_root, "agent_decisions.csv"),
  failures = file.path(agent_test_root, "agent_failures.csv"),
  checkpoint = file.path(agent_test_root, "agent_checkpoint.csv"),
  report = file.path(agent_test_root, "agent_report.txt")
)

for (path_i in unname(unlist(agent_test_paths))) {
  dir.create(
    dirname(path_i),
    recursive = TRUE,
    showWarnings = FALSE
  )
  writeLines("nonempty", path_i)
}

original_agent_paths <- runner_agent_paths
original_dependency_available <- runner_dependency_available

assign(
  "runner_agent_paths",
  function() agent_test_paths,
  envir = globalenv()
)
assign(
  "runner_dependency_available",
  function(package_name) FALSE,
  envir = globalenv()
)

agent_fail_closed <- tryCatch(
  runner_validate_agent_screening(runner_paths()),
  error = function(e) list(passed = FALSE, message = conditionMessage(e))
)

assign("runner_agent_paths", original_agent_paths, envir = globalenv())
assign("runner_dependency_available", original_dependency_available, envir = globalenv())

check_true(
  !isTRUE(agent_fail_closed$passed),
  "R/19 completion validation is fail-closed",
  "missing validation dependencies cannot produce PASS"
)

unlink(
  agent_test_root,
  recursive = TRUE,
  force = TRUE
)

# State serialization test uses a temporary directory only.
state_dir <- tempfile("task016_runner_state_")
dir.create(state_dir, recursive = TRUE, showWarnings = FALSE)

state_file <- file.path(state_dir, "state.yml")
state0 <- runner_empty_state()
runner_write_state(state0, state_file)
state1 <- runner_load_state(state_file, resume = TRUE)

check_true(
  file.exists(state_file) &&
    identical(state1$workflow, state0$workflow) &&
    identical(state1$runner_version, state0$runner_version) &&
    identical(state1$status, state0$status),
  "Workflow state write/load contract",
  paste0(
    "temporary state round-trip; runner_version=",
    state1$runner_version
  )
)

unlink(
  state_dir,
  recursive = TRUE,
  force = TRUE
)


# Run-log version regression test:
# create a real run log in a temporary workflow path while exposing
# zero executable stages, so no scientific module is executed.
original_runner_paths <- runner_paths
original_runner_steps <- runner_steps

runlog_test_root <- tempfile("task016_runlog_version_")
dir.create(runlog_test_root, recursive = TRUE, showWarnings = FALSE)

runlog_test_paths <- list(
  state_directory = runlog_test_root,
  state_file = file.path(runlog_test_root, "universal_workflow_state.yml"),
  run_log_directory = file.path(runlog_test_root, "runs"),
  generated_directory = file.path(runlog_test_root, "generated"),
  decisions_required = file.path(runlog_test_root, "generated", "initialization_decisions_required.yml"),
  project_draft = file.path(runlog_test_root, "generated", "project_config_draft.yml"),
  search_draft = file.path(runlog_test_root, "generated", "search_terms_draft.yml"),
  responses = file.path(runlog_test_root, "generated", "human_decision_responses.yml"),
  verification_report = file.path(runlog_test_root, "generated", "human_decision_verification.yml"),
  active_project = "config/project_config.yml",
  active_search = "config/search_terms.yml"
)

assign(
  "runner_paths",
  function() runlog_test_paths,
  envir = globalenv()
)

assign(
  "runner_steps",
  function(run_scope_advisory = TRUE, run_additional_analysis = FALSE) list(),
  envir = globalenv()
)

runlog_result <- tryCatch(
  run_universal_srma_workflow(
    resume = FALSE,
    run_scope_advisory = FALSE,
    run_additional_analysis = FALSE
  ),
  error = function(e) e
)

assign("runner_paths", original_runner_paths, envir = globalenv())
assign("runner_steps", original_runner_steps, envir = globalenv())

runlog_file_created <- (
  !inherits(runlog_result, "error") &&
  is.list(runlog_result) &&
  file.exists(runlog_result$run_log_file)
)

runlog_data <- if (runlog_file_created) {
  yaml::read_yaml(runlog_result$run_log_file)
} else {
  list()
}

state_version_for_regression <- runner_empty_state()$runner_version
runlog_version_for_regression <- as.character(
  runlog_data$runner_version %||% ""
)

check_true(
  runlog_file_created &&
    identical(
      runlog_version_for_regression,
      state_version_for_regression
    ),
  "Runner version consistency across state and new run log",
  paste0(
    "runner_empty_state=",
    state_version_for_regression,
    "; run_log=",
    runlog_version_for_regression
  )
)

unlink(
  runlog_test_root,
  recursive = TRUE,
  force = TRUE
)

state2 <- runner_record_step(
  state1,
  "initialization_agent",
  "completed",
  started_at = runner_now(),
  completed_at = runner_now(),
  message = "smoke test"
)

check_true(
  "initialization_agent" %in% state2$completed_steps,
  "Completed-step state recording"
)

state3 <- runner_record_step(
  state2,
  "database_input_gate",
  "paused",
  message = "smoke test pause"
)

check_true(
  !"database_input_gate" %in% state3$completed_steps,
  "Paused gate is not falsely recorded as completed"
)

# Save an auditable local smoke-test result without executing workflow stages.
output_dir <- file.path("results", "workflow")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

result_table <- data.frame(
  check = c(passes, failures),
  status = c(
    rep("PASS", length(passes)),
    rep("FAIL", length(failures))
  ),
  stringsAsFactors = FALSE
)

csv_file <- file.path(
  output_dir,
  "task016_runner_integration_smoke_test.csv"
)

utils::write.csv(
  result_table,
  csv_file,
  row.names = FALSE,
  na = ""
)

report_file <- file.path(
  output_dir,
  "task016_runner_integration_smoke_test.txt"
)

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "TASK-016 — Final Runner Integration Structural Smoke Test",
  "",
  paste0("Runner: ", runner_file),
  paste0("Generated at: ", runner_now()),
  paste0("PASS checks: ", length(passes)),
  paste0("FAIL checks: ", length(failures)),
  "",
  "IMPORTANT",
  "--------------------------------------------------",
  "No scientific workflow module was executed by this test.",
  "This test validates Runner structure, module resolution, canonical-path isolation, fail-closed recovery, and state helpers only.",
  "",
  "STATUS",
  "--------------------------------------------------",
  if (length(failures) == 0L) "PASS" else "FAIL"
)

writeLines(report_lines, report_file, useBytes = TRUE)

cat("\n")
cat("============================================================\n")
cat("TASK-016 Runner Integration Smoke Test\n")
cat("============================================================\n\n")
cat("PASS checks: ", length(passes), "\n", sep = "")
cat("FAIL checks: ", length(failures), "\n", sep = "")
cat("CSV: ", csv_file, "\n", sep = "")
cat("Report: ", report_file, "\n\n", sep = "")

if (length(failures) > 0L) {
  cat("FAILED CHECKS:\n")
  cat(paste0("- ", failures, "\n"), sep = "")
  stop(
    "TASK-016 Runner integration smoke test FAILED.",
    call. = FALSE
  )
}

cat("PASS: TASK-016 Runner integration structural smoke test completed.\n")
invisible(
  list(
    status = "PASS",
    passes = passes,
    csv = csv_file,
    report = report_file
  )
)
