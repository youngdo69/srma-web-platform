# ============================================================
# Universal SR-MA Agent Toolkit
# R/run_universal_srma_workflow.R
#
# TASK-004: Universal Workflow Runner
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
    "R/01_validate_project_initialization.R",
    "R/01b_prepare_human_decision_review.R",
    "R/01_finalize_human_decisions.R",
    "R/11_generate_search_strategies.R",
    "R/12_validate_search_strategies.R"
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
      script = "R/01_validate_project_initialization.R"
    ),
    list(
      id = "human_decision_review",
      label = "Prepare Human Decision review",
      script = "R/01b_prepare_human_decision_review.R"
    ),
    list(
      id = "human_decision_finalization",
      label = paste(
        "Review Human Decisions, apply patches, verify,",
        "and activate configuration"
      ),
      script = "R/01_finalize_human_decisions.R"
    ),
    list(
      id = "search_generation",
      label = "Generate database search strategies",
      script = "R/11_generate_search_strategies.R"
    ),
    list(
      id = "search_validation",
      label = "Validate database search strategies",
      script = "R/12_validate_search_strategies.R"
    )
  )
}


# ============================================================
# 3. Workflow-state management
# ============================================================

runner_empty_state <- function() {
  list(
    workflow = "universal_srma_initialization_to_search",
    runner_version = "1.0.2",
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

    existing_validation <- runner_validate_step(
      step$id,
      paths
    )

    completed_in_state <- step$id %in%
      state$completed_steps

    recoverable_from_artifacts <- isTRUE(
      existing_validation$passed
    )

    if (
      isTRUE(resume) &&
      (
        completed_in_state ||
        recoverable_from_artifacts
      ) &&
      recoverable_from_artifacts
    ) {
      status_text <- if (completed_in_state) {
        "SKIPPED — already completed"
      } else {
        "RECOVERED — valid artifacts found"
      }

      runner_print_step(
        index,
        total_steps,
        step$label,
        status_text
      )

      state <- runner_record_step(
        state = state,
        step_id = step$id,
        status = "completed",
        started_at = state$
          step_records[[step$id]]$
          started_at %||% "",
        completed_at = state$
          step_records[[step$id]]$
          completed_at %||% runner_now(),
        message = existing_validation$message,
        validation = existing_validation
      )

      run_log$steps[[step$id]] <- list(
        status = "skipped",
        reason = status_text,
        validation = existing_validation
      )

      runner_write_state(
        state,
        paths$state_file
      )

      next
    }

    runner_print_step(
      index,
      total_steps,
      step$label
    )

    step_started_at <- runner_now()

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
              runner_validate_finalization(
                paths
              )

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

          } else if (
            runner_should_skip_finalization_script(
              paths
            )
          ) {
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
        "Universal SR-MA workflow failed at step '",
        step$id,
        "'.",
        call. = FALSE
      )
    }

    post_validation <- runner_validate_step(
      step$id,
      paths
    )

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

  final_validation <- vapply(
    steps,
    function(step) {
      isTRUE(
        runner_validate_step(
          step$id,
          paths
        )$passed
      )
    },
    logical(1)
  )

  if (!all(final_validation)) {
    failed_final_steps <- step_ids[
      !final_validation
    ]

    state$status <- "failed"
    state$last_error <- paste(
      "Final workflow validation failed:",
      paste(
        failed_final_steps,
        collapse = ", "
      )
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
    "Universal SR-MA Workflow Completed"
  )

  cat(
    "PASS: All orchestrated workflow steps are complete.\n",
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
