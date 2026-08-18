# ============================================================
# Universal SR-MA Agent Toolkit
# R/run_universal_srma_workflow.R
#
# TASK-009: Phase 1 Universal Workflow Runner Integration
#
# Scope
# ------------------------------------------------------------
# 1. Single execution entry point
# 2. Stepwise progress messages
# 3. Safe stop on failure
# 4. Resume from validated workflow state
# 5. Preserve Human Decision controls
# 6. Orchestrate existing verified R scripts only
#
# Orchestrated sequence
# ------------------------------------------------------------
# R/01_validate_project_initialization.R
# R/01b_prepare_human_decision_review.R
# R/01_finalize_human_decisions.R
# R/11_generate_search_strategies.R
# R/12_validate_search_strategies.R
# R/16_download_pubmed_records.R
# [Gate] Enabled database export inputs
# R/17_import_merge_deduplicate.R
# R/18_prepare_screening_workbook.R
# R/19_run_agent_title_abstract_screening.R
# R/20_validate_agent_screening.R
# [Gate] Human Reviewer 2
# [Gate] Human Adjudication
# R/21_finalize_title_abstract_screening.R
#
# Usage
# ------------------------------------------------------------
# Default:
#   source("R/run_universal_srma_workflow.R")
#
# Resume explicitly:
#   options(srma.runner.autorun = FALSE)
#   source("R/run_universal_srma_workflow.R")
#   run_universal_srma_workflow(resume = TRUE)
#
# Re-run from a downstream step:
#   run_universal_srma_workflow(
#     resume = TRUE,
#     from_step = "search_generation"
#   )
#
# Valid from_step values:
#   initialization_validation
#   human_decision_review
#   human_decision_finalization
#   search_generation
#   search_validation
#   pubmed_download
#   database_input_gate
#   merge_deduplication
#   screening_workbook
#   agent_screening
#   agent_validation
#   human_reviewer_2_gate
#   human_adjudication_gate
#   screening_finalization
# ============================================================


# ============================================================
# 1. General helpers
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}


runner_now <- function() {
  format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )
}


runner_timestamp <- function() {
  format(
    Sys.time(),
    "%Y%m%d_%H%M%S"
  )
}


runner_is_nonempty_character <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    nzchar(trimws(x))
}


runner_as_character_vector <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(character(0))
  }

  values <- unlist(
    x,
    recursive = TRUE,
    use.names = FALSE
  )

  values <- as.character(values)
  values <- trimws(values)

  unique(
    values[
      !is.na(values) &
        nzchar(values)
    ]
  )
}


runner_as_logical <- function(
    x,
    default = FALSE
) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }

  value <- unlist(
    x,
    recursive = TRUE,
    use.names = FALSE
  )[[1]]

  if (is.logical(value)) {
    return(isTRUE(value))
  }

  normalized <- tolower(
    trimws(
      as.character(value)
    )
  )

  if (
    normalized %in% c(
      "true",
      "t",
      "yes",
      "y",
      "1",
      "pass",
      "passed"
    )
  ) {
    return(TRUE)
  }

  if (
    normalized %in% c(
      "false",
      "f",
      "no",
      "n",
      "0",
      "fail",
      "failed"
    )
  ) {
    return(FALSE)
  }

  default
}


runner_print_header <- function(title) {
  width <- max(
    60L,
    nchar(title) + 4L
  )

  cat("\n")
  cat(
    paste(rep("=", width), collapse = ""),
    "\n",
    sep = ""
  )
  cat(title, "\n")
  cat(
    paste(rep("=", width), collapse = ""),
    "\n",
    sep = ""
  )
}


runner_print_step <- function(
    index,
    total,
    label,
    status = NULL
) {
  prefix <- paste0(
    "[",
    index,
    "/",
    total,
    "] "
  )

  if (is.null(status)) {
    cat("\n", prefix, label, "...\n", sep = "")
  } else {
    cat(
      prefix,
      label,
      " — ",
      status,
      "\n",
      sep = ""
    )
  }
}


runner_require_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop(
      "Required package is not installed: ",
      package_name,
      "\nInstall it once with:\n",
      'install.packages("',
      package_name,
      '")',
      call. = FALSE
    )
  }

  invisible(TRUE)
}


runner_assert_project_root <- function() {
  required_root_files <- c(
    "config/initialization_agent.yml",
    "config/agent_screening.yml",
    "R/01_validate_project_initialization.R",
    "R/01b_prepare_human_decision_review.R",
    "R/01_finalize_human_decisions.R",
    "R/11_generate_search_strategies.R",
    "R/12_validate_search_strategies.R",
    "R/16_download_pubmed_records.R",
    "R/17_import_merge_deduplicate.R",
    "R/18_prepare_screening_workbook.R",
    "R/19_run_agent_title_abstract_screening.R",
    "R/20_validate_agent_screening.R",
    "R/21_finalize_title_abstract_screening.R"
  )

  missing <- required_root_files[
    !file.exists(required_root_files)
  ]

  if (length(missing) > 0L) {
    stop(
      "The workflow runner must be executed from the toolkit root.\n",
      "Missing required file(s):\n- ",
      paste(missing, collapse = "\n- "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


runner_file_nonempty <- function(file_path) {
  file.exists(file_path) &&
    !is.na(file.info(file_path)$size) &&
    file.info(file_path)$size > 0
}


# ============================================================
# 2. Runner paths and workflow definition
# ============================================================

runner_paths <- function() {
  agent_document <- yaml::read_yaml(
    "config/initialization_agent.yml"
  )

  agent_config <- agent_document$initialization_agent

  if (is.null(agent_config) || !is.list(agent_config)) {
    stop(
      "Invalid config/initialization_agent.yml: ",
      "missing initialization_agent section.",
      call. = FALSE
    )
  }

  generated_directory <-
    agent_config$output$generated_directory %||%
    "config/generated"

  decisions_name <-
    agent_config$output$decisions_required %||%
    "initialization_decisions_required.yml"

  project_draft_name <-
    agent_config$output$project_config_draft %||%
    "project_config_draft.yml"

  search_draft_name <-
    agent_config$output$search_terms_draft %||%
    "search_terms_draft.yml"

  state_directory <- "results/workflow"

  list(
    state_directory = state_directory,
    state_file = file.path(
      state_directory,
      "universal_workflow_state.yml"
    ),
    run_log_directory = file.path(
      state_directory,
      "runs"
    ),
    generated_directory = generated_directory,
    decisions_required = file.path(
      generated_directory,
      decisions_name
    ),
    project_draft = file.path(
      generated_directory,
      project_draft_name
    ),
    search_draft = file.path(
      generated_directory,
      search_draft_name
    ),
    responses = file.path(
      generated_directory,
      "human_decision_responses.yml"
    ),
    project_resolved = file.path(
      generated_directory,
      "project_config_resolved.yml"
    ),
    search_resolved = file.path(
      generated_directory,
      "search_terms_resolved.yml"
    ),
    verification_report = file.path(
      generated_directory,
      "human_decision_verification.yml"
    ),
    resolved_decisions = file.path(
      generated_directory,
      "human_decisions_resolved.yml"
    ),
    active_project = "config/project_config.yml",
    active_search = "config/search_terms.yml"
  )
}


runner_steps <- function() {
  list(
    list(
      id = "initialization_validation",
      label = "Validate initialization drafts",
      script = "R/01_validate_project_initialization.R",
      type = "script",
      recovery = TRUE
    ),
    list(
      id = "human_decision_review",
      label = "Prepare Human Decision review",
      script = "R/01b_prepare_human_decision_review.R",
      type = "script",
      recovery = TRUE
    ),
    list(
      id = "human_decision_finalization",
      label = paste(
        "Review Human Decisions, apply patches, verify,",
        "and activate configuration"
      ),
      script = "R/01_finalize_human_decisions.R",
      type = "script",
      recovery = TRUE
    ),
    list(
      id = "search_generation",
      label = "Generate database search strategies",
      script = "R/11_generate_search_strategies.R",
      type = "script",
      recovery = TRUE
    ),
    list(
      id = "search_validation",
      label = "Validate database search strategies",
      script = "R/12_validate_search_strategies.R",
      type = "script",
      recovery = TRUE
    ),
    list(
      id = "pubmed_download",
      label = "Download canonical PubMed records",
      script = "R/16_download_pubmed_records.R",
      type = "script",
      recovery = TRUE
    ),
    list(
      id = "database_input_gate",
      label = "Confirm enabled database export inputs",
      script = "",
      type = "gate",
      recovery = TRUE,
      gate_message = paste(
        "Manual database export input is required before merge/deduplication.",
        "Place every enabled database file at its canonical path and rerun the Runner."
      )
    ),
    list(
      id = "merge_deduplication",
      label = "Merge and deduplicate database records",
      script = "R/17_import_merge_deduplicate.R",
      type = "script",
      recovery = TRUE
    ),
    list(
      id = "screening_workbook",
      label = "Prepare title/abstract screening workbook",
      script = "R/18_prepare_screening_workbook.R",
      type = "script",
      recovery = TRUE
    ),
    list(
      id = "agent_screening",
      label = "Run Agent Reviewer 1 title/abstract screening",
      script = "R/19_run_agent_title_abstract_screening.R",
      type = "script",
      recovery = TRUE
    ),
    list(
      id = "agent_validation",
      label = "Validate Agent Reviewer 1 stage",
      script = "R/20_validate_agent_screening.R",
      type = "script_success",
      recovery = TRUE
    ),
    list(
      id = "human_reviewer_2_gate",
      label = "Human Reviewer 2 completion gate",
      script = "",
      type = "gate",
      recovery = TRUE,
      gate_message = paste(
        "Human Reviewer 2 must independently complete the Reviewer_2 sheet",
        "in the agent screening workbook, save it, and rerun the Runner."
      )
    ),
    list(
      id = "human_adjudication_gate",
      label = "Human adjudication completion gate",
      script = "",
      type = "gate",
      recovery = TRUE,
      gate_message = paste(
        "A human adjudicator must resolve every reviewer conflict in",
        "Conflict_Resolution, save the workbook, and rerun the Runner."
      )
    ),
    list(
      id = "screening_finalization",
      label = "Finalize title/abstract screening and full-text-review handoff",
      script = "R/21_finalize_title_abstract_screening.R",
      type = "script",
      recovery = TRUE
    )
  )
}


# ============================================================
# 3. Workflow-state management
# ============================================================

runner_empty_state <- function() {
  list(
    workflow = "universal_srma_phase1",
    runner_version = "2.0.0",
    status = "not_started",
    current_step = "",
    started_at = "",
    updated_at = runner_now(),
    completed_at = "",
    last_error = "",
    completed_steps = character(0),
    step_records = list()
  )
}


runner_normalize_state <- function(state) {
  template <- runner_empty_state()

  if (is.null(state) || !is.list(state)) {
    return(template)
  }

  for (field_name in names(template)) {
    if (is.null(state[[field_name]])) {
      state[[field_name]] <- template[[field_name]]
    }
  }

  state$completed_steps <- runner_as_character_vector(
    state$completed_steps
  )

  if (!is.list(state$step_records)) {
    state$step_records <- list()
  }

  state
}


runner_load_state <- function(
    state_file,
    resume = TRUE
) {
  if (!isTRUE(resume) || !file.exists(state_file)) {
    return(
      runner_empty_state()
    )
  }

  runner_normalize_state(
    yaml::read_yaml(state_file)
  )
}


runner_write_state <- function(
    state,
    state_file
) {
  state$updated_at <- runner_now()

  state_directory <- dirname(state_file)

  if (!dir.exists(state_directory)) {
    dir.create(
      state_directory,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  temporary_file <- paste0(
    state_file,
    ".tmp"
  )

  yaml::write_yaml(
    state,
    temporary_file
  )

  copied <- file.copy(
    temporary_file,
    state_file,
    overwrite = TRUE
  )

  unlink(
    temporary_file,
    force = TRUE
  )

  if (!isTRUE(copied) || !file.exists(state_file)) {
    stop(
      "Could not write workflow state: ",
      state_file,
      call. = FALSE
    )
  }

  invisible(state)
}


runner_record_step <- function(
    state,
    step_id,
    status,
    started_at = "",
    completed_at = "",
    message = "",
    validation = list()
) {
  state$step_records[[step_id]] <- list(
    status = status,
    started_at = started_at,
    completed_at = completed_at,
    message = message,
    validation = validation
  )

  if (identical(status, "completed")) {
    state$completed_steps <- unique(
      c(
        state$completed_steps,
        step_id
      )
    )
  } else {
    state$completed_steps <- setdiff(
      state$completed_steps,
      step_id
    )
  }

  state
}


# ============================================================
# 4. Artifact and completion validators
# ============================================================

runner_read_decisions <- function(paths) {
  if (!file.exists(paths$decisions_required)) {
    return(NULL)
  }

  document <- yaml::read_yaml(
    paths$decisions_required
  )

  decisions <- document$human_decisions_required

  if (is.null(decisions)) {
    decisions <- list()
  }

  decisions
}


runner_read_responses <- function(paths) {
  if (!file.exists(paths$responses)) {
    return(NULL)
  }

  document <- yaml::read_yaml(
    paths$responses
  )

  responses <- document$human_decision_responses

  if (is.null(responses)) {
    responses <- list()
  }

  responses
}


runner_draft_logs_directory <- function(paths) {
  if (!file.exists(paths$project_draft)) {
    return("results/logs")
  }

  draft <- tryCatch(
    yaml::read_yaml(
      paths$project_draft
    ),
    error = function(error) NULL
  )

  configured_directory <- draft$paths$logs_directory

  if (!runner_is_nonempty_character(configured_directory)) {
    return("results/logs")
  }

  configured_directory
}


runner_validate_initialization <- function(paths) {
  summary_file <- file.path(
    runner_draft_logs_directory(paths),
    "project_initialization_validation_summary.csv"
  )

  if (!runner_file_nonempty(summary_file)) {
    return(
      list(
        passed = FALSE,
        message = paste(
          "Initialization validation summary is missing or empty:",
          summary_file
        )
      )
    )
  }

  summary_data <- tryCatch(
    utils::read.csv(
      summary_file,
      stringsAsFactors = FALSE
    ),
    error = function(error) NULL
  )

  if (is.null(summary_data) || nrow(summary_data) == 0L) {
    return(
      list(
        passed = FALSE,
        message = "Initialization validation summary could not be read."
      )
    )
  }

  metric_column <- intersect(
    c("metric", "item"),
    names(summary_data)
  )

  if (
    length(metric_column) == 0L ||
    !"value" %in% names(summary_data)
  ) {
    return(
      list(
        passed = FALSE,
        message = paste(
          "Initialization validation summary does not contain",
          "metric/item and value columns."
        )
      )
    )
  }

  error_rows <- summary_data[
    summary_data[[metric_column[[1]]]] == "errors",
    ,
    drop = FALSE
  ]

  passed <- (
    nrow(error_rows) == 1L &&
    !is.na(error_rows$value[[1]]) &&
    as.numeric(error_rows$value[[1]]) == 0
  )

  list(
    passed = passed,
    message = if (passed) {
      "Initialization validation reports zero errors."
    } else {
      "Initialization validation did not report zero errors."
    },
    summary_file = summary_file
  )
}


runner_validate_review_preparation <- function(paths) {
  decisions <- runner_read_decisions(paths)

  if (is.null(decisions)) {
    return(
      list(
        passed = FALSE,
        message = paste(
          "Human Decision source file is missing:",
          paths$decisions_required
        )
      )
    )
  }

  if (length(decisions) == 0L) {
    return(
      list(
        passed = TRUE,
        message = "No unresolved Human Decisions remain."
      )
    )
  }

  responses <- runner_read_responses(paths)

  if (is.null(responses)) {
    return(
      list(
        passed = FALSE,
        message = paste(
          "Human Decision response file is missing:",
          paths$responses
        )
      )
    )
  }

  decision_ids <- vapply(
    decisions,
    function(x) as.character(x$decision_id %||% ""),
    character(1)
  )

  response_ids <- vapply(
    responses,
    function(x) as.character(x$decision_id %||% ""),
    character(1)
  )

  passed <- (
    length(responses) == length(decisions) &&
    setequal(decision_ids, response_ids)
  )

  list(
    passed = passed,
    message = if (passed) {
      paste0(
        length(responses),
        " Human Decision response record(s) are prepared."
      )
    } else {
      paste(
        "Human Decision response records do not match",
        "the unresolved decision records."
      )
    }
  )
}


runner_validate_finalization <- function(paths) {
  decisions <- runner_read_decisions(paths)

  if (is.null(decisions)) {
    return(
      list(
        passed = FALSE,
        message = paste(
          "Human Decision source file is missing:",
          paths$decisions_required
        )
      )
    )
  }

  if (length(decisions) > 0L) {
    return(
      list(
        passed = FALSE,
        message = paste0(
          length(decisions),
          " unresolved Human Decision(s) remain."
        )
      )
    )
  }

  verification_passed <- FALSE

  if (file.exists(paths$verification_report)) {
    verification <- yaml::read_yaml(
      paths$verification_report
    )

    verification_passed <- runner_as_logical(
      verification$passed,
      default = FALSE
    )
  }

  active_files_present <- all(
    vapply(
      c(
        paths$active_project,
        paths$active_search
      ),
      runner_file_nonempty,
      logical(1)
    )
  )

  resolved_files_present <- all(
    vapply(
      c(
        paths$project_resolved,
        paths$search_resolved
      ),
      runner_file_nonempty,
      logical(1)
    )
  )

  draft_files_present <- all(
    vapply(
      c(
        paths$project_draft,
        paths$search_draft
      ),
      runner_file_nonempty,
      logical(1)
    )
  )

  active_matches_draft <- FALSE

  if (
    active_files_present &&
    draft_files_present
  ) {
    active_md5 <- unname(
      tools::md5sum(
        c(
          paths$active_project,
          paths$active_search
        )
      )
    )

    draft_md5 <- unname(
      tools::md5sum(
        c(
          paths$project_draft,
          paths$search_draft
        )
      )
    )

    active_matches_draft <- identical(
      active_md5,
      draft_md5
    )
  }

  resolved_record_present <- runner_file_nonempty(
    paths$resolved_decisions
  )

  passed <- (
    length(decisions) == 0L &&
    verification_passed &&
    active_files_present &&
    draft_files_present &&
    resolved_files_present &&
    resolved_record_present &&
    active_matches_draft
  )

  failure_reasons <- character(0)

  if (length(decisions) > 0L) {
    failure_reasons <- c(
      failure_reasons,
      paste0(
        length(decisions),
        " unresolved Human Decision(s) remain"
      )
    )
  }

  if (!verification_passed) {
    failure_reasons <- c(
      failure_reasons,
      "verification report is not PASS"
    )
  }

  if (!resolved_record_present) {
    failure_reasons <- c(
      failure_reasons,
      "resolved Human Decision record is missing or empty"
    )
  }

  if (!resolved_files_present) {
    failure_reasons <- c(
      failure_reasons,
      "resolved configuration files are missing or empty"
    )
  }

  if (!active_files_present) {
    failure_reasons <- c(
      failure_reasons,
      "active configuration files are missing or empty"
    )
  }

  if (!draft_files_present) {
    failure_reasons <- c(
      failure_reasons,
      "reviewed draft configuration files are missing or empty"
    )
  }

  if (
    draft_files_present &&
    active_files_present &&
    !active_matches_draft
  ) {
    failure_reasons <- c(
      failure_reasons,
      "active configurations do not match the reviewed drafts used for activation"
    )
  }

  list(
    passed = passed,
    message = if (passed) {
      paste(
        "Human Decisions are resolved, the resolved record exists,",
        "patch verification passed, and active configurations",
        "match the reviewed drafts used for activation."
      )
    } else {
      paste(
        "Human Decision finalization is not complete:",
        paste(
          failure_reasons,
          collapse = "; "
        )
      )
    },
    unresolved_decision_count = length(decisions),
    verification_passed = verification_passed,
    resolved_record_present = resolved_record_present,
    active_files_present = active_files_present,
    draft_files_present = draft_files_present,
    resolved_files_present = resolved_files_present,
    active_matches_reviewed_drafts = active_matches_draft
  )
}


runner_resolve_project_paths <- function() {
  environment <- new.env(
    parent = globalenv()
  )

  sys.source(
    "R/utils/config_utils.R",
    envir = environment
  )

  sys.source(
    "R/utils/path_utils.R",
    envir = environment
  )

  config <- environment$load_project_config(
    "config/project_config.yml",
    validate = TRUE,
    quiet = TRUE
  )

  environment$build_project_paths(
    config = config,
    create_directories = TRUE,
    validate = TRUE
  )
}


runner_validate_search_generation <- function(paths) {
  project_paths <- tryCatch(
    runner_resolve_project_paths(),
    error = function(error) NULL
  )

  if (is.null(project_paths)) {
    return(
      list(
        passed = FALSE,
        message = "Canonical project paths could not be resolved."
      )
    )
  }

  output_files <- c(
    pubmed_search = project_paths$pubmed_search,
    embase_search = project_paths$embase_search,
    central_search = project_paths$central_search,
    strategy_summary = project_paths$search_strategy_summary,
    concept_summary = file.path(
      project_paths$search_strategies_directory,
      "search_concept_summary.csv"
    )
  )

  nonempty <- vapply(
    output_files,
    runner_file_nonempty,
    logical(1)
  )

  passed <- all(nonempty)

  list(
    passed = passed,
    message = if (passed) {
      "All R/11 search-strategy outputs exist and are non-empty."
    } else {
      paste(
        "Missing or empty R/11 output(s):",
        paste(
          names(output_files)[!nonempty],
          collapse = ", "
        )
      )
    },
    files = as.list(output_files),
    nonempty = as.list(nonempty)
  )
}


runner_validate_search_validation <- function(paths) {
  project_paths <- tryCatch(
    runner_resolve_project_paths(),
    error = function(error) NULL
  )

  if (is.null(project_paths)) {
    return(
      list(
        passed = FALSE,
        message = "Canonical project paths could not be resolved."
      )
    )
  }

  summary_file <- file.path(
    project_paths$logs_directory,
    "search_strategy_validation.csv"
  )

  if (!runner_file_nonempty(summary_file)) {
    return(
      list(
        passed = FALSE,
        message = paste(
          "R/12 validation result is missing or empty:",
          summary_file
        )
      )
    )
  }

  validation_data <- tryCatch(
    utils::read.csv(
      summary_file,
      stringsAsFactors = FALSE
    ),
    error = function(error) NULL
  )

  if (
    is.null(validation_data) ||
    nrow(validation_data) == 0L ||
    !"passed" %in% names(validation_data)
  ) {
    return(
      list(
        passed = FALSE,
        message = "R/12 validation result could not be interpreted."
      )
    )
  }

  passed_values <- validation_data$passed

  if (!is.logical(passed_values)) {
    passed_values <- tolower(
      trimws(
        as.character(passed_values)
      )
    ) %in% c(
      "true",
      "t",
      "1",
      "yes"
    )
  }

  passed <- all(
    !is.na(passed_values) &
      passed_values
  )

  list(
    passed = passed,
    message = if (passed) {
      paste0(
        "All ",
        nrow(validation_data),
        " R/12 validation check(s) passed."
      )
    } else {
      paste0(
        sum(!passed_values | is.na(passed_values)),
        " R/12 validation check(s) failed."
      )
    },
    validation_file = summary_file
  )
}



runner_resolve_project_context <- function() {
  environment <- new.env(
    parent = globalenv()
  )

  sys.source(
    "R/utils/config_utils.R",
    envir = environment
  )

  sys.source(
    "R/utils/path_utils.R",
    envir = environment
  )

  config <- environment$load_project_config(
    "config/project_config.yml",
    validate = TRUE,
    quiet = TRUE
  )

  project_paths <- environment$build_project_paths(
    config = config,
    create_directories = TRUE,
    validate = TRUE
  )

  list(
    environment = environment,
    config = config,
    paths = project_paths
  )
}


runner_validate_pubmed_download <- function(paths) {
  context <- tryCatch(
    runner_resolve_project_context(),
    error = function(error) NULL
  )

  if (is.null(context)) {
    return(
      list(
        passed = FALSE,
        message = "Canonical project context could not be resolved."
      )
    )
  }

  output_files <- c(
    pubmed_xml = context$paths$pubmed_xml,
    pubmed_csv = context$paths$pubmed_csv,
    pubmed_report = context$paths$pubmed_download_report
  )

  ok <- vapply(
    output_files,
    runner_file_nonempty,
    logical(1)
  )

  list(
    passed = all(ok),
    message = if (all(ok)) {
      "Canonical PubMed download outputs are complete."
    } else {
      paste(
        "Missing or empty PubMed output(s):",
        paste(names(output_files)[!ok], collapse = ", ")
      )
    },
    files = as.list(output_files)
  )
}


runner_validate_database_inputs <- function(paths) {
  context <- tryCatch(
    runner_resolve_project_context(),
    error = function(error) NULL
  )

  if (is.null(context)) {
    return(
      list(
        passed = FALSE,
        message = "Canonical project context could not be resolved."
      )
    )
  }

  enabled <- context$environment$get_enabled_databases(
    context$config,
    validate = FALSE
  )

  input_map <- c(
    pubmed = context$paths$pubmed_csv,
    embase = context$paths$embase_ris,
    central = context$paths$central_ris
  )

  supported_enabled <- intersect(
    enabled,
    names(input_map)
  )

  if (length(supported_enabled) == 0L) {
    return(
      list(
        passed = FALSE,
        message = "No supported database inputs are enabled."
      )
    )
  }

  required_files <- input_map[
    supported_enabled
  ]

  ok <- vapply(
    required_files,
    runner_file_nonempty,
    logical(1)
  )

  list(
    passed = all(ok),
    message = if (all(ok)) {
      paste(
        "All enabled database inputs are present:",
        paste(supported_enabled, collapse = ", ")
      )
    } else {
      paste(
        "Missing enabled database input(s):",
        paste(
          paste0(
            names(required_files)[!ok],
            " -> ",
            required_files[!ok]
          ),
          collapse = "; "
        )
      )
    },
    enabled_databases = as.list(enabled),
    files = as.list(required_files)
  )
}


runner_validate_merge_deduplication <- function(paths) {
  context <- tryCatch(
    runner_resolve_project_context(),
    error = function(error) NULL
  )

  if (is.null(context)) {
    return(
      list(
        passed = FALSE,
        message = "Canonical project context could not be resolved."
      )
    )
  }

  output_files <- c(
    merged_records = context$paths$merged_records_csv,
    deduplicated_records = context$paths$deduplicated_records_csv,
    removed_duplicates = context$paths$removed_duplicates_csv,
    deduplication_map = context$paths$deduplication_map_csv,
    prisma_counts = context$paths$prisma_counts_csv,
    report = context$paths$import_merge_deduplication_report
  )

  ok <- vapply(
    output_files,
    runner_file_nonempty,
    logical(1)
  )

  list(
    passed = all(ok),
    message = if (all(ok)) {
      "R/17 merge/deduplication outputs are complete."
    } else {
      paste(
        "Missing or empty R/17 output(s):",
        paste(names(output_files)[!ok], collapse = ", ")
      )
    },
    files = as.list(output_files)
  )
}


runner_validate_screening_workbook <- function(paths) {
  context <- tryCatch(
    runner_resolve_project_context(),
    error = function(error) NULL
  )

  if (is.null(context)) {
    return(
      list(
        passed = FALSE,
        message = "Canonical project context could not be resolved."
      )
    )
  }

  workbook_file <- context$paths$screening_workbook
  report_file <- context$paths$screening_workbook_report

  if (
    !runner_file_nonempty(workbook_file) ||
    !runner_file_nonempty(report_file)
  ) {
    return(
      list(
        passed = FALSE,
        message = "R/18 screening workbook or report is missing/empty."
      )
    )
  }

  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    return(
      list(
        passed = FALSE,
        message = "Package 'openxlsx' is required to validate the screening workbook."
      )
    )
  }

  sheets <- tryCatch(
    openxlsx::getSheetNames(workbook_file),
    error = function(error) character(0)
  )

  required_sheets <- c(
    "Instructions",
    "Reviewer_1",
    "Reviewer_2",
    "Conflict_Resolution",
    "Source_Summary",
    "Validation_Lists"
  )

  missing <- setdiff(
    required_sheets,
    sheets
  )

  list(
    passed = length(missing) == 0L,
    message = if (length(missing) == 0L) {
      "R/18 screening workbook structure is complete."
    } else {
      paste(
        "R/18 screening workbook is missing sheet(s):",
        paste(missing, collapse = ", ")
      )
    },
    workbook = workbook_file
  )
}


runner_agent_paths <- function() {
  context <- runner_resolve_project_context()

  agent_document <- yaml::read_yaml(
    "config/agent_screening.yml"
  )

  agent_config <- agent_document$agent_screening

  if (is.null(agent_config) || !is.list(agent_config)) {
    stop(
      "Invalid config/agent_screening.yml.",
      call. = FALSE
    )
  }

  list(
    context = context,
    config = agent_config,
    workbook = file.path(
      context$paths$screening_results_directory,
      paste0(
        agent_config$output$workbook_basename,
        ".xlsx"
      )
    ),
    decisions = file.path(
      context$paths$logs_directory,
      paste0(
        agent_config$output$decisions_basename,
        ".csv"
      )
    ),
    failures = file.path(
      context$paths$logs_directory,
      paste0(
        agent_config$output$failures_basename,
        ".csv"
      )
    ),
    checkpoint = file.path(
      context$paths$logs_directory,
      paste0(
        agent_config$output$checkpoint_basename,
        ".csv"
      )
    ),
    report = file.path(
      context$paths$logs_directory,
      paste0(
        agent_config$output$report_basename,
        ".txt"
      )
    )
  )
}


runner_validate_agent_screening <- function(paths) {
  agent_paths <- tryCatch(
    runner_agent_paths(),
    error = function(error) NULL
  )

  if (is.null(agent_paths)) {
    return(
      list(
        passed = FALSE,
        message = "Agent-screening paths could not be resolved."
      )
    )
  }

  output_files <- c(
    workbook = agent_paths$workbook,
    decisions = agent_paths$decisions,
    failures = agent_paths$failures,
    checkpoint = agent_paths$checkpoint,
    report = agent_paths$report
  )

  ok <- vapply(
    output_files,
    runner_file_nonempty,
    logical(1)
  )

  if (!all(ok)) {
    return(
      list(
        passed = FALSE,
        message = paste(
          "Missing or empty R/19 output(s):",
          paste(names(output_files)[!ok], collapse = ", ")
        )
      )
    )
  }

  if (
    !requireNamespace("openxlsx", quietly = TRUE) ||
    !requireNamespace("readr", quietly = TRUE)
  ) {
    return(
      list(
        passed = FALSE,
        message = "Packages 'openxlsx' and 'readr' are required for R/19 validation."
      )
    )
  }

  reviewer_1 <- tryCatch(
    openxlsx::readWorkbook(
      agent_paths$workbook,
      sheet = "Reviewer_1",
      startRow = 4,
      colNames = TRUE,
      skipEmptyRows = FALSE
    ),
    error = function(error) NULL
  )

  if (
    is.null(reviewer_1) ||
    !"decision" %in% names(reviewer_1) ||
    nrow(reviewer_1) == 0L
  ) {
    return(
      list(
        passed = FALSE,
        message = "Reviewer_1 could not be validated from the R/19 workbook."
      )
    )
  }

  decision_values <- trimws(
    as.character(reviewer_1$decision)
  )

  reviewer_complete <- all(
    !is.na(decision_values) &
      nzchar(decision_values)
  )

  failures <- tryCatch(
    readr::read_csv(
      agent_paths$failures,
      show_col_types = FALSE,
      col_types = readr::cols(
        .default = readr::col_character()
      )
    ),
    error = function(error) NULL
  )

  failure_count <- if (
    is.null(failures) ||
    nrow(failures) == 0L
  ) {
    0L
  } else if (
    "screening_id" %in% names(failures)
  ) {
    sum(
      !is.na(failures$screening_id) &
        nzchar(trimws(failures$screening_id))
    )
  } else {
    nrow(failures)
  }

  passed <- reviewer_complete &&
    failure_count == 0L

  list(
    passed = passed,
    message = if (passed) {
      "R/19 Agent Reviewer 1 screening outputs are complete."
    } else {
      paste0(
        "R/19 outputs are incomplete. Reviewer complete: ",
        reviewer_complete,
        "; failures: ",
        failure_count
      )
    },
    workbook = agent_paths$workbook
  )
}


runner_validate_phase1_finalization <- function(paths) {
  context <- tryCatch(
    runner_resolve_project_context(),
    error = function(error) NULL
  )

  if (is.null(context)) {
    return(
      list(
        passed = FALSE,
        message = "Canonical project context could not be resolved."
      )
    )
  }

  completed_workbook <- context$paths$completed_screening_workbook
  final_csv <- file.path(
    context$paths$processed_screening_directory,
    "final_title_abstract_screening.csv"
  )
  included_csv <- file.path(
    context$paths$processed_screening_directory,
    "included_for_full_text_review.csv"
  )
  report_file <- file.path(
    context$paths$logs_directory,
    "final_title_abstract_screening_report.txt"
  )

  output_files <- c(
    completed_workbook = completed_workbook,
    final_screening_csv = final_csv,
    included_for_full_text_review_csv = included_csv,
    final_report = report_file
  )

  ok <- vapply(
    output_files,
    runner_file_nonempty,
    logical(1)
  )

  if (!all(ok)) {
    return(
      list(
        passed = FALSE,
        message = paste(
          "Missing or empty R/21 output(s):",
          paste(names(output_files)[!ok], collapse = ", ")
        )
      )
    )
  }

  sheet_ok <- FALSE

  if (requireNamespace("openxlsx", quietly = TRUE)) {
    sheets <- tryCatch(
      openxlsx::getSheetNames(completed_workbook),
      error = function(error) character(0)
    )

    sheet_ok <- "Included_for_Full_Text_Review" %in%
      sheets
  }

  list(
    passed = sheet_ok,
    message = if (sheet_ok) {
      "R/21 final screening outputs and full-text-review handoff are complete."
    } else {
      "Completed workbook does not contain Included_for_Full_Text_Review."
    },
    files = as.list(output_files)
  )
}


runner_validate_agent_validation <- function(paths) {
  downstream <- runner_validate_phase1_finalization(paths)

  if (isTRUE(downstream$passed)) {
    return(
      list(
        passed = TRUE,
        message = paste(
          "R/20 historical PASS recovered from validated downstream",
          "R/21 finalization artifacts."
        ),
        recovered_from_downstream = TRUE
      )
    )
  }

  list(
    passed = FALSE,
    message = paste(
      "R/20 is a historical agent-stage validator and has no",
      "standalone completion marker yet."
    )
  )
}


runner_validate_human_reviewer_2_gate <- function(paths) {
  downstream <- runner_validate_phase1_finalization(paths)

  if (isTRUE(downstream$passed)) {
    return(
      list(
        passed = TRUE,
        message = "Human Reviewer 2 completion recovered from R/21 finalization."
      )
    )
  }

  agent_paths <- tryCatch(
    runner_agent_paths(),
    error = function(error) NULL
  )

  if (
    is.null(agent_paths) ||
    !runner_file_nonempty(agent_paths$workbook) ||
    !requireNamespace("openxlsx", quietly = TRUE)
  ) {
    return(
      list(
        passed = FALSE,
        message = "Agent workbook is not available for Human Reviewer 2."
      )
    )
  }

  reviewer_2 <- tryCatch(
    openxlsx::readWorkbook(
      agent_paths$workbook,
      sheet = "Reviewer_2",
      startRow = 4,
      colNames = TRUE,
      skipEmptyRows = FALSE
    ),
    error = function(error) NULL
  )

  if (
    is.null(reviewer_2) ||
    !"decision" %in% names(reviewer_2) ||
    nrow(reviewer_2) == 0L
  ) {
    return(
      list(
        passed = FALSE,
        message = "Reviewer_2 sheet could not be read."
      )
    )
  }

  values <- trimws(
    as.character(reviewer_2$decision)
  )

  complete <- all(
    !is.na(values) &
      nzchar(values)
  )

  list(
    passed = complete,
    message = if (complete) {
      "Human Reviewer 2 screening is complete."
    } else {
      paste0(
        sum(is.na(values) | !nzchar(values)),
        " Reviewer_2 decision(s) remain incomplete."
      )
    },
    workbook = agent_paths$workbook
  )
}


runner_validate_human_adjudication_gate <- function(paths) {
  downstream <- runner_validate_phase1_finalization(paths)

  if (isTRUE(downstream$passed)) {
    return(
      list(
        passed = TRUE,
        message = "Human adjudication completion recovered from R/21 finalization."
      )
    )
  }

  agent_paths <- tryCatch(
    runner_agent_paths(),
    error = function(error) NULL
  )

  if (
    is.null(agent_paths) ||
    !runner_file_nonempty(agent_paths$workbook) ||
    !requireNamespace("openxlsx", quietly = TRUE)
  ) {
    return(
      list(
        passed = FALSE,
        message = "Agent workbook is not available for human adjudication."
      )
    )
  }

  reviewer_1 <- tryCatch(
    openxlsx::readWorkbook(
      agent_paths$workbook,
      sheet = "Reviewer_1",
      startRow = 4,
      colNames = TRUE,
      skipEmptyRows = FALSE
    ),
    error = function(error) NULL
  )

  reviewer_2 <- tryCatch(
    openxlsx::readWorkbook(
      agent_paths$workbook,
      sheet = "Reviewer_2",
      startRow = 4,
      colNames = TRUE,
      skipEmptyRows = FALSE
    ),
    error = function(error) NULL
  )

  conflicts <- tryCatch(
    openxlsx::readWorkbook(
      agent_paths$workbook,
      sheet = "Conflict_Resolution",
      startRow = 3,
      colNames = TRUE,
      skipEmptyRows = FALSE
    ),
    error = function(error) NULL
  )

  if (
    is.null(reviewer_1) ||
    is.null(reviewer_2) ||
    is.null(conflicts)
  ) {
    return(
      list(
        passed = FALSE,
        message = "Reviewer or Conflict_Resolution sheets could not be read."
      )
    )
  }

  required_conflict_columns <- c(
    "screening_id",
    "final_decision",
    "final_exclusion_reason"
  )

  if (
    !all(required_conflict_columns %in% names(conflicts)) ||
    !"decision" %in% names(reviewer_1) ||
    !"decision" %in% names(reviewer_2) ||
    !"exclusion_reason" %in% names(reviewer_1) ||
    !"exclusion_reason" %in% names(reviewer_2)
  ) {
    return(
      list(
        passed = FALSE,
        message = "Required adjudication fields are missing."
      )
    )
  }

  clean <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x) | !nzchar(x)] <- NA_character_
    x
  }

  r1_decision <- clean(reviewer_1$decision)
  r2_decision <- clean(reviewer_2$decision)
  r1_reason <- clean(reviewer_1$exclusion_reason)
  r2_reason <- clean(reviewer_2$exclusion_reason)

  if (
    length(r1_decision) != length(r2_decision) ||
    nrow(conflicts) != length(r1_decision)
  ) {
    return(
      list(
        passed = FALSE,
        message = "Reviewer and Conflict_Resolution row counts are not aligned."
      )
    )
  }

  decision_conflict <- r1_decision != r2_decision
  decision_conflict[
    is.na(decision_conflict)
  ] <- TRUE

  both_exclude <- (
    r1_decision == "Exclude" &
      r2_decision == "Exclude"
  )
  both_exclude[
    is.na(both_exclude)
  ] <- FALSE

  reason_conflict <- both_exclude & (
    is.na(r1_reason) |
      is.na(r2_reason) |
      r1_reason != r2_reason
  )
  reason_conflict[
    is.na(reason_conflict)
  ] <- TRUE

  conflict_required <- decision_conflict |
    reason_conflict

  final_decision <- clean(
    conflicts$final_decision
  )
  final_reason <- clean(
    conflicts$final_exclusion_reason
  )

  unresolved <- conflict_required &
    is.na(final_decision)

  exclude_without_reason <- conflict_required &
    final_decision == "Exclude" &
    is.na(final_reason)

  exclude_without_reason[
    is.na(exclude_without_reason)
  ] <- FALSE

  passed <- !any(unresolved) &&
    !any(exclude_without_reason)

  list(
    passed = passed,
    message = if (passed) {
      paste0(
        "Human adjudication is complete for ",
        sum(conflict_required),
        " conflict(s)."
      )
    } else {
      paste0(
        sum(unresolved),
        " conflict(s) lack final_decision; ",
        sum(exclude_without_reason),
        " adjudicated Exclude conflict(s) lack final_exclusion_reason."
      )
    },
    conflict_count = sum(conflict_required)
  )
}


runner_validate_step <- function(
    step_id,
    paths
) {
  switch(
    step_id,
    initialization_validation =
      runner_validate_initialization(paths),
    human_decision_review =
      runner_validate_review_preparation(paths),
    human_decision_finalization =
      runner_validate_finalization(paths),
    search_generation =
      runner_validate_search_generation(paths),
    search_validation =
      runner_validate_search_validation(paths),
    pubmed_download =
      runner_validate_pubmed_download(paths),
    database_input_gate =
      runner_validate_database_inputs(paths),
    merge_deduplication =
      runner_validate_merge_deduplication(paths),
    screening_workbook =
      runner_validate_screening_workbook(paths),
    agent_screening =
      runner_validate_agent_screening(paths),
    agent_validation =
      runner_validate_agent_validation(paths),
    human_reviewer_2_gate =
      runner_validate_human_reviewer_2_gate(paths),
    human_adjudication_gate =
      runner_validate_human_adjudication_gate(paths),
    screening_finalization =
      runner_validate_phase1_finalization(paths),
    stop(
      "Unsupported runner step: ",
      step_id,
      call. = FALSE
    )
  )
}


# ============================================================
# 5. Script execution
# ============================================================

runner_execute_script <- function(script_path) {
  if (!file.exists(script_path)) {
    stop(
      "Workflow script was not found: ",
      script_path,
      call. = FALSE
    )
  }

  step_environment <- new.env(
    parent = globalenv()
  )

  sys.source(
    script_path,
    envir = step_environment
  )

  invisible(TRUE)
}


runner_should_skip_review_script <- function(paths) {
  validation <- runner_validate_review_preparation(paths)
  isTRUE(validation$passed)
}


runner_should_skip_finalization_script <- function(paths) {
  validation <- runner_validate_finalization(paths)
  isTRUE(validation$passed)
}


# ============================================================
# 6. Main runner
# ============================================================

run_universal_srma_workflow <- function(
    resume = TRUE,
    from_step = NULL
) {
  runner_require_package("yaml")
  runner_assert_project_root()

  paths <- runner_paths()
  steps <- runner_steps()
  step_ids <- vapply(
    steps,
    `[[`,
    character(1),
    "id"
  )

  if (!is.null(from_step)) {
    if (
      !runner_is_nonempty_character(from_step) ||
      !from_step %in% step_ids
    ) {
      stop(
        "Invalid from_step. Choose one of:\n- ",
        paste(step_ids, collapse = "\n- "),
        call. = FALSE
      )
    }
  }

  dir.create(
    paths$state_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  dir.create(
    paths$run_log_directory,
    recursive = TRUE,
    showWarnings = FALSE
  )

  state <- runner_load_state(
    paths$state_file,
    resume = resume
  )

  state$workflow <- "universal_srma_phase1"
  state$runner_version <- "2.0.0"

  if (!isTRUE(resume)) {
    state <- runner_empty_state()
  }

  run_id <- paste0(
    "workflow_",
    runner_timestamp()
  )

  run_log_file <- file.path(
    paths$run_log_directory,
    paste0(
      run_id,
      ".yml"
    )
  )

  run_log <- list(
    run_id = run_id,
    workflow = state$workflow,
    runner_version = state$runner_version,
    started_at = runner_now(),
    completed_at = "",
    status = "running",
    resume_requested = isTRUE(resume),
    from_step = from_step %||% "",
    steps = list(),
    error = ""
  )

  runner_print_header(
    "Universal SR-MA Workflow Runner"
  )

  cat(
    "Run ID: ",
    run_id,
    "\n",
    "Resume: ",
    isTRUE(resume),
    "\n",
    "State file: ",
    paths$state_file,
    "\n",
    sep = ""
  )

  if (!nzchar(state$started_at %||% "")) {
    state$started_at <- runner_now()
  }

  state$status <- "running"
  state$completed_at <- ""
  state$last_error <- ""

  runner_write_state(
    state,
    paths$state_file
  )

  start_index <- 1L

  if (!is.null(from_step)) {
    start_index <- match(
      from_step,
      step_ids
    )

    downstream_steps <- step_ids[
      seq.int(
        start_index,
        length(step_ids)
      )
    ]

    state$completed_steps <- setdiff(
      state$completed_steps,
      downstream_steps
    )

    for (step_id in downstream_steps) {
      state$step_records[[step_id]] <- NULL
    }

    runner_write_state(
      state,
      paths$state_file
    )
  }

  total_steps <- length(steps)

  for (index in seq_along(steps)) {
    step <- steps[[index]]

    if (index < start_index) {
      runner_print_step(
        index,
        total_steps,
        step$label,
        "OUTSIDE REQUESTED RANGE"
      )

      next
    }

    completed_in_state <- step$id %in%
      state$completed_steps

    # Historical PASS conditions (notably R/20 and human gates)
    # are authoritative once recorded in runner state. They must
    # not be revalidated after downstream human work changes the
    # workbook state.
    if (isTRUE(resume) && completed_in_state) {
      runner_print_step(
        index,
        total_steps,
        step$label,
        "SKIPPED — already completed"
      )

      run_log$steps[[step$id]] <- list(
        status = "skipped",
        reason = "already completed in runner state"
      )

      next
    }

    existing_validation <- runner_validate_step(
      step$id,
      paths
    )

    recoverable_from_artifacts <- isTRUE(
      step$recovery %||% TRUE
    ) && isTRUE(
      existing_validation$passed
    )

    if (
      isTRUE(resume) &&
      recoverable_from_artifacts
    ) {
      runner_print_step(
        index,
        total_steps,
        step$label,
        "RECOVERED — valid artifacts found"
      )

      state <- runner_record_step(
        state = state,
        step_id = step$id,
        status = "completed",
        started_at = "",
        completed_at = runner_now(),
        message = existing_validation$message,
        validation = existing_validation
      )

      run_log$steps[[step$id]] <- list(
        status = "recovered",
        validation = existing_validation
      )

      runner_write_state(
        state,
        paths$state_file
      )

      next
    }

    if (identical(step$type, "gate")) {
      waiting_message <- paste(
        step$gate_message %||% "Human/manual action is required.",
        existing_validation$message
      )

      state$status <- "waiting_for_human"
      state$current_step <- step$id
      state$last_error <- ""

      state <- runner_record_step(
        state = state,
        step_id = step$id,
        status = "waiting_for_human",
        started_at = runner_now(),
        completed_at = "",
        message = waiting_message,
        validation = existing_validation
      )

      runner_write_state(
        state,
        paths$state_file
      )

      run_log$status <- "waiting_for_human"
      run_log$completed_at <- runner_now()
      run_log$steps[[step$id]] <- list(
        status = "waiting_for_human",
        message = waiting_message,
        validation = existing_validation
      )

      yaml::write_yaml(
        run_log,
        run_log_file
      )

      runner_print_step(
        index,
        total_steps,
        step$label,
        "WAITING FOR HUMAN/MANUAL ACTION"
      )

      cat(
        "\nWorkflow paused safely at a manual gate.\n",
        waiting_message,
        "\n\nAfter completing the required action, resume with:\n",
        'source("R/run_universal_srma_workflow.R")',
        "\n",
        sep = ""
      )

      return(
        invisible(
          list(
            passed = FALSE,
            waiting_for_human = TRUE,
            waiting_step = step$id,
            state = state,
            state_file = paths$state_file,
            run_log_file = run_log_file
          )
        )
      )
    }

    runner_print_step(
      index,
      total_steps,
      step$label
    )

    step_started_at <- runner_now()

    state$status <- "running"
    state$current_step <- step$id
    state <- runner_record_step(
      state = state,
      step_id = step$id,
      status = "running",
      started_at = step_started_at,
      message = "Step execution started."
    )

    runner_write_state(
      state,
      paths$state_file
    )

    step_error <- NULL

    tryCatch(
      {
        if (
          identical(
            step$id,
            "human_decision_review"
          ) &&
          runner_should_skip_review_script(paths)
        ) {
          cat(
            "Human Decision response file is already valid; ",
            "preparation script was not rerun.\n",
            sep = ""
          )

        } else if (
          identical(
            step$id,
            "human_decision_finalization"
          )
        ) {
          remaining_decisions <- runner_read_decisions(
            paths
          )

          if (
            !is.null(remaining_decisions) &&
            length(remaining_decisions) == 0L
          ) {
            finalization_validation <-
              runner_validate_finalization(paths)

            if (!isTRUE(finalization_validation$passed)) {
              stop(
                paste(
                  "No unresolved Human Decisions remain, but",
                  "finalization artifacts are incomplete.",
                  finalization_validation$message
                ),
                call. = FALSE
              )
            }

            cat(
              "Human Decision finalization is already complete; ",
              "interactive finalization was not rerun.\n",
              sep = ""
            )

          } else {
            runner_execute_script(
              step$script
            )
          }

        } else {
          runner_execute_script(
            step$script
          )
        }
      },
      error = function(error) {
        step_error <<- error
      }
    )

    if (!is.null(step_error)) {
      error_message <- conditionMessage(
        step_error
      )

      state$status <- "failed"
      state$current_step <- step$id
      state$last_error <- error_message

      state <- runner_record_step(
        state = state,
        step_id = step$id,
        status = "failed",
        started_at = step_started_at,
        completed_at = runner_now(),
        message = error_message
      )

      runner_write_state(
        state,
        paths$state_file
      )

      run_log$status <- "failed"
      run_log$completed_at <- runner_now()
      run_log$error <- error_message
      run_log$steps[[step$id]] <- list(
        status = "failed",
        error = error_message
      )

      yaml::write_yaml(
        run_log,
        run_log_file
      )

      runner_print_step(
        index,
        total_steps,
        step$label,
        "FAILED"
      )

      cat(
        "\nWorkflow stopped safely.\n",
        "Failed step: ",
        step$id,
        "\n",
        "Reason: ",
        error_message,
        "\n",
        "Resume with:\n",
        'source("R/run_universal_srma_workflow.R")',
        "\n",
        sep = ""
      )

      stop(
        "Universal SR-MA Phase 1 workflow failed at step '",
        step$id,
        "'.",
        call. = FALSE
      )
    }

    post_validation <- if (
      identical(step$type, "script_success")
    ) {
      list(
        passed = TRUE,
        message = paste(
          step$label,
          "executed successfully and its historical PASS was recorded."
        ),
        historical_pass = TRUE
      )
    } else {
      runner_validate_step(
        step$id,
        paths
      )
    }

    if (!isTRUE(post_validation$passed)) {
      validation_message <- post_validation$message

      state$status <- "failed"
      state$current_step <- step$id
      state$last_error <- validation_message

      state <- runner_record_step(
        state = state,
        step_id = step$id,
        status = "failed",
        started_at = step_started_at,
        completed_at = runner_now(),
        message = validation_message,
        validation = post_validation
      )

      runner_write_state(
        state,
        paths$state_file
      )

      run_log$status <- "failed"
      run_log$completed_at <- runner_now()
      run_log$error <- validation_message
      run_log$steps[[step$id]] <- list(
        status = "failed",
        validation = post_validation
      )

      yaml::write_yaml(
        run_log,
        run_log_file
      )

      runner_print_step(
        index,
        total_steps,
        step$label,
        "FAILED VALIDATION"
      )

      stop(
        "Step completed without an R error, but its outputs ",
        "did not pass validation: ",
        validation_message,
        call. = FALSE
      )
    }

    state$current_step <- ""
    state <- runner_record_step(
      state = state,
      step_id = step$id,
      status = "completed",
      started_at = step_started_at,
      completed_at = runner_now(),
      message = post_validation$message,
      validation = post_validation
    )

    runner_write_state(
      state,
      paths$state_file
    )

    run_log$steps[[step$id]] <- list(
      status = "completed",
      started_at = step_started_at,
      completed_at = runner_now(),
      validation = post_validation
    )

    runner_print_step(
      index,
      total_steps,
      step$label,
      "PASS"
    )
  }

  required_step_ids <- vapply(
    steps,
    `[[`,
    character(1),
    "id"
  )

  missing_completed_steps <- setdiff(
    required_step_ids,
    state$completed_steps
  )

  final_phase1_validation <- runner_validate_phase1_finalization(
    paths
  )

  if (
    length(missing_completed_steps) > 0L ||
    !isTRUE(final_phase1_validation$passed)
  ) {
    state$status <- "failed"
    state$last_error <- paste(
      "Phase 1 completion validation failed.",
      if (length(missing_completed_steps) > 0L) {
        paste(
          "Missing completed step(s):",
          paste(missing_completed_steps, collapse = ", ")
        )
      } else {
        ""
      },
      if (!isTRUE(final_phase1_validation$passed)) {
        final_phase1_validation$message
      } else {
        ""
      }
    )

    runner_write_state(
      state,
      paths$state_file
    )

    stop(
      state$last_error,
      call. = FALSE
    )
  }

  state$status <- "completed"
  state$current_step <- ""
  state$completed_at <- runner_now()
  state$last_error <- ""

  runner_write_state(
    state,
    paths$state_file
  )

  run_log$status <- "completed"
  run_log$completed_at <- runner_now()

  yaml::write_yaml(
    run_log,
    run_log_file
  )

  runner_print_header(
    "Universal SR-MA Phase 1 Workflow Completed"
  )

  cat(
    "PASS: Phase 1 end-to-end workflow is complete through R/21.\n",
    "State file: ",
    paths$state_file,
    "\n",
    "Run log: ",
    run_log_file,
    "\n",
    sep = ""
  )

  invisible(
    list(
      passed = TRUE,
      state = state,
      state_file = paths$state_file,
      run_log_file = run_log_file
    )
  )
}


# ============================================================
# 7. Default single-entry execution
# ============================================================

if (
  isTRUE(
    getOption(
      "srma.runner.autorun",
      TRUE
    )
  )
) {
  run_universal_srma_workflow(
    resume = TRUE
  )
}
