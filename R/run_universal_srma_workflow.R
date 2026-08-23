# ============================================================
# Universal SR-MA Agent Toolkit
# R/run_universal_srma_workflow.R
#
# TASK-016 — Final Universal Runner Integration
# VERSION: 3.0.6
#
# PURPOSE
# ------------------------------------------------------------
# Single orchestration entry point across the validated SR-MA
# workflow while preserving:
#   - standalone module execution
#   - Human Review / Verification gates
#   - safe stop / pause / resume / recovery
#   - auditable state and per-run logs
#   - mandatory pre-search PROSPERO preparation
#   - optional R/25A and R/29 branches
#   - mandatory R/28 post-meta-analysis diagnostics before R/29
#   - R/32 as an upload-routing service only
#   - exclusion of R/30-R/31 maintenance modules
#
# IMPORTANT
# ------------------------------------------------------------
# The Runner orchestrates. It does NOT reproduce analytical,
# screening, extraction, eligibility, RoB, or plotting logic.
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

runner_now <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")
runner_timestamp <- function() format(Sys.time(), "%Y%m%d_%H%M%S")

runner_is_nonempty_character <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
}

runner_as_character_vector <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character(0))
  x <- trimws(as.character(unlist(x, recursive = TRUE, use.names = FALSE)))
  unique(x[!is.na(x) & nzchar(x)])
}

runner_as_logical <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0L) return(default)
  value <- unlist(x, recursive = TRUE, use.names = FALSE)[[1]]
  if (is.logical(value)) return(isTRUE(value))
  value <- tolower(trimws(as.character(value)))
  if (value %in% c("true", "t", "yes", "y", "1", "pass", "passed")) return(TRUE)
  if (value %in% c("false", "f", "no", "n", "0", "fail", "failed")) return(FALSE)
  default
}

runner_require_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    stop(
      "Required package is not installed: ", package_name,
      "\nInstall it once with install.packages(\"", package_name, "\")",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

runner_file_nonempty <- function(path) {
  file.exists(path) && !is.na(file.info(path)$size) && file.info(path)$size > 0
}

runner_print_header <- function(title) {
  width <- max(60L, nchar(title) + 4L)
  cat("\n", paste(rep("=", width), collapse = ""), "\n", sep = "")
  cat(title, "\n")
  cat(paste(rep("=", width), collapse = ""), "\n", sep = "")
}

runner_print_step <- function(index, total, label, status = NULL) {
  prefix <- paste0("[", index, "/", total, "] ")
  if (is.null(status)) {
    cat("\n", prefix, label, "...\n", sep = "")
  } else {
    cat(prefix, label, " — ", status, "\n", sep = "")
  }
}

runner_normalize_slash <- function(x) gsub("\\\\", "/", as.character(x))

runner_assert_project_root <- function() {
  required <- c(
    "config/initialization_agent.yml",
    "R/utils/config_utils.R",
    "R/utils/path_utils.R"
  )
  missing <- required[!file.exists(required)]
  if (length(missing) > 0L) {
    stop(
      "Run the Universal Runner from the toolkit repository root.\nMissing:\n- ",
      paste(missing, collapse = "\n- "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# ------------------------------------------------------------
# Validation helpers
# ------------------------------------------------------------
# Normal workflow completion/recovery MUST be decided only from the
# active project context resolved through config_utils.R + path_utils.R.
# Repository-wide newest-file discovery is intentionally prohibited.
runner_dependency_available <- function(package_name) {
  requireNamespace(package_name, quietly = TRUE)
}

runner_read_text <- function(path) {
  if (!runner_file_nonempty(path)) return("")
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

runner_read_csv_character <- function(path) {
  if (!runner_file_nonempty(path)) return(NULL)

  if (runner_dependency_available("readr")) {
    return(
      tryCatch(
        readr::read_csv(
          path,
          show_col_types = FALSE,
          col_types = readr::cols(.default = readr::col_character())
        ),
        error = function(e) NULL
      )
    )
  }

  tryCatch(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      colClasses = "character",
      check.names = FALSE
    ),
    error = function(e) NULL
  )
}

runner_clean_scalar <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) return("")
  trimws(as.character(x[[1]]))
}

runner_path_is_absolute <- function(path) {
  p <- runner_normalize_slash(runner_clean_scalar(path))
  nzchar(p) && (
    grepl("^[A-Za-z]:/", p) ||
      grepl("^/", p) ||
      grepl("^//", p)
  )
}

runner_normalize_path_safe <- function(path, must_work = FALSE) {
  if (!runner_is_nonempty_character(as.character(path))) return("")
  tryCatch(
    normalizePath(path, winslash = "/", mustWork = must_work),
    error = function(e) ""
  )
}

runner_path_within <- function(path, directory) {
  path_n <- runner_normalize_path_safe(path, must_work = FALSE)
  dir_n <- runner_normalize_path_safe(directory, must_work = FALSE)
  if (!nzchar(path_n) || !nzchar(dir_n)) return(FALSE)

  path_cmp <- tolower(runner_normalize_slash(path_n))
  dir_cmp <- sub("/+$", "", tolower(runner_normalize_slash(dir_n)))

  identical(path_cmp, dir_cmp) || startsWith(path_cmp, paste0(dir_cmp, "/"))
}

# ============================================================
# 1. Paths / state
# ============================================================
runner_paths <- function() {
  agent_document <- yaml::read_yaml("config/initialization_agent.yml")
  agent_config <- agent_document$initialization_agent %||% list()
  generated_directory <- agent_config$output$generated_directory %||% "config/generated"
  decisions_name <- agent_config$output$decisions_required %||% "initialization_decisions_required.yml"
  project_draft_name <- agent_config$output$project_config_draft %||% "project_config_draft.yml"
  search_draft_name <- agent_config$output$search_terms_draft %||% "search_terms_draft.yml"

  state_directory <- "results/workflow"
  list(
    state_directory = state_directory,
    state_file = file.path(state_directory, "universal_workflow_state.yml"),
    run_log_directory = file.path(state_directory, "runs"),
    generated_directory = generated_directory,
    decisions_required = file.path(generated_directory, decisions_name),
    project_draft = file.path(generated_directory, project_draft_name),
    search_draft = file.path(generated_directory, search_draft_name),
    responses = file.path(generated_directory, "human_decision_responses.yml"),
    verification_report = file.path(generated_directory, "human_decision_verification.yml"),
    active_project = "config/project_config.yml",
    active_search = "config/search_terms.yml"
  )
}

runner_empty_state <- function() {
  list(
    workflow = "universal_srma_final",
    runner_version = "3.0.6",
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
  if (is.null(state) || !is.list(state)) return(template)
  for (name in names(template)) if (is.null(state[[name]])) state[[name]] <- template[[name]]
  state$workflow <- "universal_srma_final"
  state$runner_version <- "3.0.6"
  state$completed_steps <- runner_as_character_vector(state$completed_steps)
  if (!is.list(state$step_records)) state$step_records <- list()
  state
}

runner_load_state <- function(state_file, resume = TRUE) {
  if (!isTRUE(resume) || !file.exists(state_file)) return(runner_empty_state())
  runner_normalize_state(yaml::read_yaml(state_file))
}

runner_write_state <- function(state, state_file) {
  state$updated_at <- runner_now()
  dir.create(dirname(state_file), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(state_file, ".tmp")
  yaml::write_yaml(state, tmp)
  ok <- file.copy(tmp, state_file, overwrite = TRUE)
  unlink(tmp, force = TRUE)
  if (!isTRUE(ok) || !file.exists(state_file)) stop("Could not write workflow state: ", state_file, call. = FALSE)
  invisible(state)
}

runner_record_step <- function(state, step_id, status, started_at = "", completed_at = "", message = "", validation = list()) {
  state$step_records[[step_id]] <- list(
    status = status,
    started_at = started_at,
    completed_at = completed_at,
    message = message,
    validation = validation
  )
  if (status %in% c("completed", "skipped")) {
    state$completed_steps <- unique(c(state$completed_steps, step_id))
  } else {
    state$completed_steps <- setdiff(state$completed_steps, step_id)
  }
  state
}

# ============================================================
# 2. Script registry and Final Integration Graph
# ============================================================
runner_steps <- function(run_scope_advisory = TRUE, run_additional_analysis = FALSE) {
  list(
    list(id="initialization_agent", label="Run Project Initialization Agent", category="RUN", type="script", scripts=c("R/00_run_project_initialization_agent.R"), enabled=TRUE, recovery=TRUE),
    list(id="initialization_validation", label="Validate initialization drafts", category="RUN", type="script", scripts=c("R/01_validate_project_initialization.R"), enabled=TRUE, recovery=TRUE),
    list(id="human_decision_review", label="Prepare Human Decision review", category="RUN", type="script", scripts=c("R/01b_prepare_human_decision_review.R"), enabled=TRUE, recovery=TRUE),
    list(id="human_decision_finalization", label="Human Decision finalization and activation", category="HUMAN GATE", type="script", scripts=c("R/01_finalize_human_decisions.R"), enabled=TRUE, recovery=TRUE),
    list(id="prospero_preparation", label="Prepare PROSPERO registration information", category="RUN", type="external_script", scripts=c("R/create_and_autofill_prospero.R", "create_and_autofill_prospero.R", "R/create_and_autofill_prospero_AUTO_PROJECT.R", "create_and_autofill_prospero_AUTO_PROJECT.R"), enabled=TRUE, recovery=TRUE),
    list(id="search_generation", label="Generate database search strategies", category="RUN", type="script", scripts=c("R/11_generate_search_strategies.R"), enabled=TRUE, recovery=TRUE),
    list(id="search_validation", label="Validate database search strategies", category="RUN", type="script", scripts=c("R/12_validate_search_strategies.R"), enabled=TRUE, recovery=TRUE),
    list(id="pubmed_download", label="Download canonical PubMed records", category="RUN", type="script", scripts=c("R/16_download_pubmed_records.R"), enabled=TRUE, recovery=TRUE),
    list(id="database_input_gate", label="Enabled database export availability", category="HUMAN GATE", type="gate", scripts=character(0), enabled=TRUE, recovery=TRUE),
    list(id="merge_deduplication", label="Merge and deduplicate database records", category="RUN", type="script", scripts=c("R/17_import_merge_deduplicate.R"), enabled=TRUE, recovery=TRUE),
    list(id="screening_workbook", label="Prepare title/abstract screening workbook", category="RUN", type="script", scripts=c("R/18_prepare_screening_workbook.R"), enabled=TRUE, recovery=TRUE),
    list(id="agent_screening", label="Run Agent Reviewer 1 title/abstract screening", category="RUN", type="script", scripts=c("R/19_run_agent_title_abstract_screening.R"), enabled=TRUE, recovery=TRUE),
    list(id="agent_validation", label="Validate Agent Reviewer 1 stage", category="RUN", type="script_success", scripts=c("R/20_validate_agent_screening.R"), enabled=TRUE, recovery=TRUE),
    list(id="human_reviewer_2_gate", label="Human Reviewer 2 completion", category="HUMAN GATE", type="gate", scripts=character(0), enabled=TRUE, recovery=TRUE),
    list(id="human_adjudication_gate", label="Human adjudication completion", category="HUMAN GATE", type="gate", scripts=character(0), enabled=TRUE, recovery=TRUE),
    list(id="screening_finalization", label="Finalize title/abstract screening", category="RUN", type="script", scripts=c("R/21_finalize_title_abstract_screening.R"), enabled=TRUE, recovery=TRUE),
    list(id="full_text_registry", label="Prepare full-text registry and PDF intake", category="RUN", type="script", scripts=c("R/22_prepare_full_text_registry.R"), enabled=TRUE, recovery=TRUE),
    list(id="full_text_agent_eligibility", label="Run multimodal full-text eligibility Agent", category="RUN", type="script", scripts=c("R/23_run_multimodal_full_text_eligibility_2stage.R"), enabled=TRUE, recovery=TRUE),
    list(id="full_text_author_gate", label="1st Author final full-text eligibility review", category="HUMAN GATE", type="gate", scripts=character(0), enabled=TRUE, recovery=TRUE),
    list(id="final_included_studies", label="Finalize authoritative Included Studies", category="RUN", type="script", scripts=c("R/24_finalize_included_studies.R"), enabled=TRUE, recovery=TRUE),
    list(id="prisma", label="Generate PRISMA 2020 flow diagram", category="RUN", type="script", scripts=c("R/35_generate_prisma_flow_diagram.R"), enabled=TRUE, recovery=TRUE),
    list(id="multimodal_extraction", label="Run multimodal data extraction", category="RUN", type="script", scripts=c("R/25_run_multimodal_data_extraction.R"), enabled=TRUE, recovery=TRUE),
    list(id="manuscript_tables", label="Generate editable manuscript characteristic tables", category="RUN", type="script", scripts=c("R/34_generate_manuscript_tables.R"), enabled=TRUE, recovery=TRUE),
    list(id="scope_advisory", label="Generate Research Scope Advisory", category="OPTIONAL", type="script", scripts=c("R/25A_generate_research_scope_advisory.R"), enabled=isTRUE(run_scope_advisory), recovery=TRUE),
    list(id="targeted_verification_prep", label="Prepare targeted Human Verification workbook", category="RUN", type="script", scripts=c("R/25B_prepare_targeted_human_verification.R"), enabled=isTRUE(run_scope_advisory), recovery=TRUE),
    list(id="extraction_human_gate", label="Human Verification of planned analysis data", category="HUMAN GATE", type="gate", scripts=character(0), enabled=isTRUE(run_scope_advisory), recovery=TRUE),
    list(id="human_verified_finalization", label="Finalize authoritative human-verified analysis data", category="RUN", type="script", scripts=c("R/26_finalize_human_verified_extraction.R"), enabled=isTRUE(run_scope_advisory), recovery=TRUE),
    list(id="meta_analysis", label="Run universal meta-analysis", category="RUN", type="script", scripts=c("R/27_run_universal_meta_analysis.R"), enabled=isTRUE(run_scope_advisory), recovery=TRUE),
    list(id="meta_analysis_diagnostics", label="Run post-meta-analysis diagnostics", category="RUN", type="script", scripts=c("R/28_run_meta_analysis_diagnostics.R"), enabled=isTRUE(run_scope_advisory), recovery=TRUE),
    list(id="additional_analysis", label="Author-directed additional analysis", category="OPTIONAL", type="r29", scripts=c("R/29_run_author_directed_additional_analysis.R"), enabled=isTRUE(run_scope_advisory) && isTRUE(run_additional_analysis), recovery=FALSE),
    list(id="forest_plots", label="Generate publication forest plots and editable meta-analysis tables", category="RUN", type="script", scripts=c("R/33_generate_forest_plots_and_editable_tables.R"), enabled=isTRUE(run_scope_advisory), recovery=TRUE),
    list(id="risk_of_bias", label="Generate provisional result-level risk-of-bias assessments", category="RUN", type="script", scripts=c("R/36_generate_risk_of_bias.R"), enabled=isTRUE(run_scope_advisory), recovery=TRUE),
    list(id="rob_author_gate", label="1st Author final risk-of-bias review", category="HUMAN GATE", type="gate", scripts=character(0), enabled=isTRUE(run_scope_advisory), recovery=TRUE),
    list(id="rob_finalization", label="Finalize risk-of-bias and generate figures", category="RUN", type="script", scripts=c("R/37_finalize_risk_of_bias_and_generate_figures.R", "R/37_generate_risk_of_bias_figures.R"), enabled=isTRUE(run_scope_advisory), recovery=TRUE)
  )
}

runner_resolve_script <- function(candidates) {
  candidates <- runner_as_character_vector(candidates)
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0L) return("")
  hit[[1]]
}

# ============================================================
# 3. Completion validators
# ============================================================
runner_validation <- function(passed, message, ...) {
  c(list(passed = isTRUE(passed), message = as.character(message)), list(...))
}

# Canonical active-project context. This intentionally preserves the
# Phase-1 config/path contract and is the sole normal source of workflow
# artifact locations.
runner_resolve_project_context <- function() {
  environment <- new.env(parent = globalenv())

  sys.source("R/utils/config_utils.R", envir = environment)
  sys.source("R/utils/path_utils.R", envir = environment)

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

runner_project_context_or_null <- function() {
  tryCatch(
    runner_resolve_project_context(),
    error = function(e) NULL
  )
}

runner_active_root_path <- function(context, ...) {
  file.path(context$paths$project_root, ...)
}

runner_validate_initialization_agent <- function(paths) {
  files <- c(paths$project_draft, paths$search_draft, paths$decisions_required)
  ok <- vapply(files, runner_file_nonempty, logical(1))
  runner_validation(
    all(ok),
    if (all(ok)) {
      "Initialization Agent drafts are present."
    } else {
      paste("Missing initialization artifacts:", paste(basename(files[!ok]), collapse = ", "))
    },
    files = as.list(files)
  )
}

runner_draft_logs_directory <- function(paths) {
  if (!file.exists(paths$project_draft)) return("results/logs")
  draft <- tryCatch(yaml::read_yaml(paths$project_draft), error = function(e) NULL)
  configured <- if (is.list(draft)) draft$paths$logs_directory else NULL
  if (!runner_is_nonempty_character(configured)) return("results/logs")
  as.character(configured)
}

runner_validate_initialization <- function(paths) {
  summary_file <- file.path(
    runner_draft_logs_directory(paths),
    "project_initialization_validation_summary.csv"
  )

  if (!runner_file_nonempty(summary_file)) {
    return(runner_validation(FALSE, "Initialization validation summary is missing.", file = summary_file))
  }

  d <- runner_read_csv_character(summary_file)
  if (is.null(d) || nrow(d) == 0L) {
    return(runner_validation(FALSE, "Initialization validation summary cannot be interpreted.", file = summary_file))
  }

  metric_col <- intersect(c("metric", "item"), names(d))
  if (length(metric_col) == 0L || !"value" %in% names(d)) {
    return(runner_validation(FALSE, "Initialization validation summary lacks metric/item and value columns.", file = summary_file))
  }

  error_rows <- d[d[[metric_col[[1]]]] == "errors", , drop = FALSE]
  ok <- nrow(error_rows) == 1L && suppressWarnings(as.numeric(error_rows$value[[1]])) == 0
  runner_validation(ok, if (ok) "Initialization validation reports zero errors." else "Initialization validation did not report zero errors.", file = summary_file)
}

runner_unresolved_human_decisions <- function(path) {
  if (!file.exists(path)) return(NA_integer_)
  doc <- tryCatch(yaml::read_yaml(path), error = function(e) NULL)
  if (is.null(doc)) return(NA_integer_)
  x <- doc$human_decisions_required
  if (is.null(x)) return(0L)
  if (is.data.frame(x)) return(nrow(x))
  length(x)
}

runner_validate_review_preparation <- function(paths) {
  finalization <- runner_validate_finalization(paths)
  if (isTRUE(finalization$passed)) {
    return(runner_validation(TRUE, "Human Decision review preparation recovered from finalized canonical configuration."))
  }

  unresolved <- runner_unresolved_human_decisions(paths$decisions_required)
  if (is.na(unresolved)) return(runner_validation(FALSE, "Human Decision source file is missing or unreadable."))
  if (unresolved == 0L) return(runner_validation(TRUE, "No unresolved Human Decisions require a review-response file.", unresolved = 0L))
  if (!runner_file_nonempty(paths$responses)) return(runner_validation(FALSE, "Human Decision response/review file has not been prepared.", unresolved = unresolved))

  response_doc <- tryCatch(yaml::read_yaml(paths$responses), error = function(e) NULL)
  responses <- if (is.list(response_doc)) response_doc$human_decision_responses else NULL
  response_n <- if (is.null(responses)) 0L else if (is.data.frame(responses)) nrow(responses) else length(responses)
  ok <- identical(as.integer(response_n), as.integer(unresolved))

  runner_validation(
    ok,
    if (ok) "Human Decision review-response file is prepared for every unresolved decision." else paste0("Human Decision response count does not match unresolved decisions (", response_n, " vs ", unresolved, ")."),
    unresolved = unresolved,
    response_count = response_n,
    file = paths$responses
  )
}

runner_validate_finalization <- function(paths) {
  unresolved <- runner_unresolved_human_decisions(paths$decisions_required)
  passed <- runner_file_nonempty(paths$active_project) &&
    runner_file_nonempty(paths$active_search) &&
    !is.na(unresolved) && unresolved == 0L

  runner_validation(
    passed,
    if (passed) "Human decisions are finalized and canonical configuration is active." else "Human Decision finalization/activation remains incomplete.",
    unresolved = unresolved
  )
}

# Project-folder lookup is identity-only. It may inspect immediate project
# metadata, but never searches project artifacts and never accepts a folder
# merely because it is the only one present.
runner_detect_project_slug <- function() {
  context <- runner_project_context_or_null()
  if (is.null(context) || !dir.exists("projects")) return("")

  project_id <- runner_clean_scalar(context$config$project$project_id)
  if (!nzchar(project_id)) return("")

  dirs <- list.dirs("projects", recursive = FALSE, full.names = FALSE)
  dirs <- dirs[
    nzchar(dirs) &
      !tolower(dirs) %in% c("archive", "archived", "_archive", "templates", "_templates")
  ]
  if (length(dirs) == 0L) return("")

  direct <- dirs[tolower(dirs) == tolower(project_id)]
  if (length(direct) == 1L) {
    config_file <- file.path("projects", direct[[1]], "config", "project_config.yml")
    if (!file.exists(config_file)) return(direct[[1]])
    cfg <- tryCatch(yaml::read_yaml(config_file), error = function(e) NULL)
    id2 <- runner_clean_scalar(if (is.list(cfg)) cfg$project$project_id else NULL)
    if (identical(tolower(id2), tolower(project_id))) return(direct[[1]])
  }

  matches <- character(0)
  for (d in dirs) {
    config_file <- file.path("projects", d, "config", "project_config.yml")
    if (!file.exists(config_file)) next
    cfg <- tryCatch(yaml::read_yaml(config_file), error = function(e) NULL)
    id2 <- runner_clean_scalar(if (is.list(cfg)) cfg$project$project_id else NULL)
    if (nzchar(id2) && identical(tolower(id2), tolower(project_id))) {
      matches <- c(matches, d)
    }
  }

  if (length(unique(matches)) == 1L) unique(matches)[[1]] else ""
}

runner_prospero_canonical_candidates <- function(project_root) {
  project_root <- runner_clean_scalar(project_root)

  if (!nzchar(project_root)) {
    return(character(0))
  }

  prospero_dir <- file.path(
    project_root,
    "results",
    "registration",
    "prospero"
  )

  c(
    file.path(prospero_dir, "prospero_final.xlsx"),
    file.path(prospero_dir, "prospero_autofilled.xlsx")
  )
}

runner_validate_prospero <- function(paths) {
  if (runner_as_logical(Sys.getenv("SRMA_PROSPERO_ALREADY_REGISTERED", unset = ""), FALSE)) {
    return(
      runner_validation(
        TRUE,
        "Existing PROSPERO registration explicitly declared by environment."
      )
    )
  }

  context <- runner_project_context_or_null()
  if (is.null(context)) {
    return(
      runner_validation(
        FALSE,
        "Canonical active-project context could not be resolved for PROSPERO validation."
      )
    )
  }

  # Existing registration evidence is accepted only from the active config.
  active_text <- runner_read_text(paths$active_project)
  if (
    grepl("PROSPERO", active_text, ignore.case = TRUE) &&
    grepl("CRD[0-9]{6,}", active_text, ignore.case = TRUE, perl = TRUE)
  ) {
    return(
      runner_validation(
        TRUE,
        "Existing PROSPERO registration evidence is present in the active project configuration.",
        file = paths$active_project
      )
    )
  }

  # Canonical prepared-form evidence is intentionally restricted to the
  # active project's root returned by build_project_paths().  No projects/*
  # search, newest-file discovery, slug inference, or cross-project fallback
  # participates in normal completion.
  project_root <- runner_clean_scalar(
    context$paths$project_root
  )

  candidates <- runner_prospero_canonical_candidates(
    project_root
  )

  if (length(candidates) == 0L) {
    return(
      runner_validation(
        FALSE,
        "Canonical active-project root could not be resolved for PROSPERO validation."
      )
    )
  }

  ok <- vapply(
    candidates,
    runner_file_nonempty,
    logical(1)
  )
  hit <- candidates[ok]

  runner_validation(
    length(hit) > 0L,
    if (length(hit) > 0L) {
      "PROSPERO preparation artifact is present at the active-project canonical path."
    } else {
      paste(
        "Mandatory pre-search PROSPERO preparation has not been documented",
        "at results/registration/prospero for the active project."
      )
    },
    file = if (length(hit) > 0L) hit[[1]] else "",
    project_root = project_root
  )
}
runner_validate_search_generation <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))

  output_files <- c(
    pubmed_search = context$paths$pubmed_search,
    embase_search = context$paths$embase_search,
    central_search = context$paths$central_search,
    strategy_summary = context$paths$search_strategy_summary,
    concept_summary = file.path(context$paths$search_strategies_directory, "search_concept_summary.csv")
  )
  ok <- vapply(output_files, runner_file_nonempty, logical(1))
  runner_validation(all(ok), if (all(ok)) "All canonical R/11 search-strategy outputs are complete." else paste("Missing or empty R/11 output(s):", paste(names(output_files)[!ok], collapse = ", ")), files = as.list(output_files))
}

runner_validate_search_validation <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))

  f <- file.path(context$paths$logs_directory, "search_strategy_validation.csv")
  if (!runner_file_nonempty(f)) return(runner_validation(FALSE, "Search-strategy validation CSV is missing.", file = f))
  d <- runner_read_csv_character(f)
  if (is.null(d) || nrow(d) == 0L || !"passed" %in% names(d)) return(runner_validation(FALSE, "Search-strategy validation CSV cannot be interpreted.", file = f))
  vals <- tolower(trimws(as.character(d$passed))) %in% c("true", "t", "1", "yes", "pass", "passed")
  ok <- all(!is.na(vals) & vals)
  runner_validation(ok, if (ok) "All search-strategy validation checks passed." else "One or more search-strategy checks failed.", file = f)
}

runner_validate_pubmed <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))

  output_files <- c(
    pubmed_xml = context$paths$pubmed_xml,
    pubmed_csv = context$paths$pubmed_csv,
    pubmed_report = context$paths$pubmed_download_report
  )
  ok <- vapply(output_files, runner_file_nonempty, logical(1))
  runner_validation(all(ok), if (all(ok)) "Canonical PubMed download outputs are complete." else paste("Missing or empty PubMed output(s):", paste(names(output_files)[!ok], collapse = ", ")), files = as.list(output_files))
}

runner_validate_database_inputs <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) {
    return(
      runner_validation(
        FALSE,
        "Canonical active-project context could not be resolved."
      )
    )
  }

  enabled <- tryCatch(
    context$environment$get_enabled_databases(
      context$config,
      validate = FALSE
    ),
    error = function(e) character(0)
  )

  enabled <- tolower(
    runner_as_character_vector(enabled)
  )

  input_map <- c(
    pubmed = context$paths$pubmed_csv,
    embase = context$paths$embase_ris,
    central = context$paths$central_ris
  )

  strategy_map <- c(
    pubmed = context$paths$pubmed_search,
    embase = context$paths$embase_search,
    central = context$paths$central_search
  )

  supported <- intersect(
    enabled,
    names(input_map)
  )

  if (length(supported) == 0L) {
    return(
      runner_validation(
        FALSE,
        "No supported database inputs are enabled."
      )
    )
  }

  required_files <- input_map[supported]
  present <- vapply(
    required_files,
    runner_file_nonempty,
    logical(1)
  )

  if (all(present)) {
    return(
      runner_validation(
        TRUE,
        paste(
          "All enabled database inputs are present:",
          paste(supported, collapse = ", ")
        ),
        enabled_databases = as.list(enabled),
        files = as.list(required_files)
      )
    )
  }

  missing <- names(required_files)[!present]

  if ("pubmed" %in% missing) {
    return(
      runner_validation(
        FALSE,
        paste(
          "Canonical PubMed input is unexpectedly missing after",
          "the PubMed download stage:",
          required_files[["pubmed"]]
        ),
        enabled_databases = as.list(enabled),
        files = as.list(required_files)
      )
    )
  }

  manual_missing <- intersect(
    missing,
    c("embase", "central")
  )

  handoff <- c(
    "Manual database retrieval/export is required.",
    "",
    "The toolkit has already generated the database-specific search strategies.",
    "Open or copy the query from the path shown below, run it in the database,",
    "then export RIS to the canonical destination shown below.",
    ""
  )

  for (db in manual_missing) {
    strategy <- strategy_map[[db]]
    destination <- required_files[[db]]

    handoff <- c(
      handoff,
      toupper(db),
      paste0(
        "Search strategy:",
        "\n  ",
        runner_normalize_slash(strategy)
      ),
      paste0(
        "Export RIS to:",
        "\n  ",
        runner_normalize_slash(destination)
      ),
      ""
    )
  }

  handoff <- c(
    handoff,
    "After all required RIS files have been exported, rerun:",
    '  source("R/run_universal_srma_workflow.R")'
  )

  runner_validation(
    FALSE,
    paste(handoff, collapse = "\n"),
    enabled_databases = as.list(enabled),
    files = as.list(required_files),
    search_strategies = as.list(
      strategy_map[manual_missing]
    )
  )
}

runner_validate_merge <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))

  output_files <- c(
    merged_records = context$paths$merged_records_csv,
    deduplicated_records = context$paths$deduplicated_records_csv,
    removed_duplicates = context$paths$removed_duplicates_csv,
    deduplication_map = context$paths$deduplication_map_csv,
    prisma_counts = context$paths$prisma_counts_csv,
    report = context$paths$import_merge_deduplication_report
  )
  ok <- vapply(output_files, runner_file_nonempty, logical(1))
  runner_validation(all(ok), if (all(ok)) "R/17 merge/deduplication outputs are complete." else paste("Missing or empty R/17 output(s):", paste(names(output_files)[!ok], collapse = ", ")), files = as.list(output_files))
}

runner_validate_screening_workbook <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))

  workbook <- context$paths$screening_workbook
  report <- context$paths$screening_workbook_report
  if (!runner_file_nonempty(workbook) || !runner_file_nonempty(report)) {
    return(runner_validation(FALSE, "R/18 screening workbook or report is missing/empty.", workbook = workbook, report = report))
  }
  if (!runner_dependency_available("openxlsx")) {
    return(runner_validation(FALSE, "Package 'openxlsx' is required to validate the screening workbook.", workbook = workbook))
  }

  sheets <- tryCatch(openxlsx::getSheetNames(workbook), error = function(e) character(0))
  required <- c("Instructions", "Reviewer_1", "Reviewer_2", "Conflict_Resolution", "Source_Summary", "Validation_Lists")
  missing <- setdiff(required, sheets)
  runner_validation(length(missing) == 0L, if (length(missing) == 0L) "R/18 screening workbook structure is complete." else paste("R/18 screening workbook is missing sheet(s):", paste(missing, collapse = ", ")), workbook = workbook)
}

runner_agent_paths <- function() {
  context <- runner_resolve_project_context()
  agent_document <- yaml::read_yaml("config/agent_screening.yml")
  agent_config <- agent_document$agent_screening
  if (is.null(agent_config) || !is.list(agent_config)) stop("Invalid config/agent_screening.yml.", call. = FALSE)

  list(
    context = context,
    config = agent_config,
    workbook = file.path(context$paths$screening_results_directory, paste0(agent_config$output$workbook_basename, ".xlsx")),
    decisions = file.path(context$paths$logs_directory, paste0(agent_config$output$decisions_basename, ".csv")),
    failures = file.path(context$paths$logs_directory, paste0(agent_config$output$failures_basename, ".csv")),
    checkpoint = file.path(context$paths$logs_directory, paste0(agent_config$output$checkpoint_basename, ".csv")),
    report = file.path(context$paths$logs_directory, paste0(agent_config$output$report_basename, ".txt"))
  )
}

runner_agent_workbook <- function() {
  p <- tryCatch(runner_agent_paths(), error = function(e) NULL)
  if (is.null(p)) "" else p$workbook
}

runner_validate_agent_screening <- function(paths) {
  agent_paths <- tryCatch(runner_agent_paths(), error = function(e) NULL)
  if (is.null(agent_paths)) return(runner_validation(FALSE, "Agent-screening paths could not be resolved."))

  output_files <- c(
    workbook = agent_paths$workbook,
    decisions = agent_paths$decisions,
    failures = agent_paths$failures,
    checkpoint = agent_paths$checkpoint,
    report = agent_paths$report
  )
  ok <- vapply(output_files, runner_file_nonempty, logical(1))
  if (!all(ok)) return(runner_validation(FALSE, paste("Missing or empty R/19 output(s):", paste(names(output_files)[!ok], collapse = ", ")), files = as.list(output_files)))

  # FAIL-CLOSED: do not infer completion from file existence when the
  # workbook/decision validation dependencies are unavailable.
  if (!runner_dependency_available("openxlsx") || !runner_dependency_available("readr")) {
    return(runner_validation(FALSE, "Packages 'openxlsx' and 'readr' are required for R/19 completion validation.", workbook = agent_paths$workbook))
  }

  reviewer_1 <- tryCatch(
    openxlsx::readWorkbook(agent_paths$workbook, sheet = "Reviewer_1", startRow = 4, colNames = TRUE, skipEmptyRows = FALSE),
    error = function(e) NULL
  )
  if (is.null(reviewer_1) || nrow(reviewer_1) == 0L || !"decision" %in% names(reviewer_1)) {
    return(runner_validation(FALSE, "Reviewer_1 could not be validated from the canonical R/19 workbook."))
  }

  decision_values <- trimws(as.character(reviewer_1$decision))
  reviewer_complete <- all(!is.na(decision_values) & nzchar(decision_values))

  failures <- tryCatch(
    readr::read_csv(agent_paths$failures, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character())),
    error = function(e) NULL
  )
  if (is.null(failures)) return(runner_validation(FALSE, "R/19 failure log could not be validated.", file = agent_paths$failures))

  failure_count <- if (nrow(failures) == 0L) {
    0L
  } else if ("screening_id" %in% names(failures)) {
    sum(!is.na(failures$screening_id) & nzchar(trimws(failures$screening_id)))
  } else {
    nrow(failures)
  }

  passed <- reviewer_complete && failure_count == 0L
  runner_validation(
    passed,
    if (passed) "R/19 Agent Reviewer 1 screening outputs are complete." else paste0("R/19 outputs are incomplete. Reviewer complete: ", reviewer_complete, "; failures: ", failure_count),
    workbook = agent_paths$workbook,
    failure_count = failure_count
  )
}

runner_validate_phase1_finalization <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))

  completed_workbook <- context$paths$completed_screening_workbook
  final_csv <- file.path(context$paths$processed_screening_directory, "final_title_abstract_screening.csv")
  included_csv <- file.path(context$paths$processed_screening_directory, "included_for_full_text_review.csv")
  report_file <- file.path(context$paths$logs_directory, "final_title_abstract_screening_report.txt")

  output_files <- c(
    completed_workbook = completed_workbook,
    final_screening_csv = final_csv,
    included_for_full_text_review_csv = included_csv,
    final_report = report_file
  )
  ok <- vapply(output_files, runner_file_nonempty, logical(1))
  if (!all(ok)) return(runner_validation(FALSE, paste("Missing or empty canonical R/21 output(s):", paste(names(output_files)[!ok], collapse = ", ")), files = as.list(output_files)))

  if (!runner_dependency_available("openxlsx")) {
    return(runner_validation(FALSE, "Package 'openxlsx' is required to validate canonical R/21 finalization.", workbook = completed_workbook))
  }
  sheets <- tryCatch(openxlsx::getSheetNames(completed_workbook), error = function(e) character(0))
  sheet_ok <- "Included_for_Full_Text_Review" %in% sheets

  runner_validation(sheet_ok, if (sheet_ok) "R/21 final screening outputs and full-text-review handoff are complete for the active project." else "Completed canonical R/21 workbook does not contain Included_for_Full_Text_Review.", files = as.list(output_files))
}

runner_validate_agent_validation <- function(paths) {
  downstream <- runner_validate_phase1_finalization(paths)
  if (isTRUE(downstream$passed)) return(runner_validation(TRUE, "R/20 historical PASS recovered only from the active project's canonical R/21 finalization artifacts.", recovered_from_downstream = TRUE))
  runner_validation(FALSE, "R/20 requires execution; no independent durable completion marker was found.")
}

runner_validate_human_reviewer_2 <- function(paths) {
  downstream <- runner_validate_phase1_finalization(paths)
  if (isTRUE(downstream$passed)) return(runner_validation(TRUE, "Human Reviewer 2 completion recovered only from the active project's canonical R/21 finalization."))

  agent_paths <- tryCatch(runner_agent_paths(), error = function(e) NULL)
  if (is.null(agent_paths) || !runner_file_nonempty(agent_paths$workbook) || !runner_dependency_available("openxlsx")) {
    return(runner_validation(FALSE, "Reviewer_2 workbook is unavailable for the active project."))
  }
  d <- tryCatch(openxlsx::readWorkbook(agent_paths$workbook, sheet = "Reviewer_2", startRow = 4, colNames = TRUE, skipEmptyRows = FALSE), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0L || !"decision" %in% names(d)) return(runner_validation(FALSE, "Reviewer_2 sheet cannot be read."))
  vals <- trimws(as.character(d$decision))
  ok <- all(!is.na(vals) & nzchar(vals))
  runner_validation(ok, if (ok) "Human Reviewer 2 screening is complete." else paste0(sum(is.na(vals) | !nzchar(vals)), " Reviewer_2 decision(s) remain incomplete."), file = agent_paths$workbook)
}

runner_validate_human_adjudication <- function(paths) {
  downstream <- runner_validate_phase1_finalization(paths)
  if (isTRUE(downstream$passed)) return(runner_validation(TRUE, "Human adjudication completion recovered only from the active project's canonical R/21 finalization."))

  agent_paths <- tryCatch(runner_agent_paths(), error = function(e) NULL)
  if (is.null(agent_paths) || !runner_file_nonempty(agent_paths$workbook) || !runner_dependency_available("openxlsx")) {
    return(runner_validation(FALSE, "Adjudication workbook is unavailable for the active project."))
  }

  conflicts <- tryCatch(openxlsx::readWorkbook(agent_paths$workbook, sheet = "Conflict_Resolution", startRow = 3, colNames = TRUE, skipEmptyRows = FALSE), error = function(e) NULL)
  if (is.null(conflicts)) return(runner_validation(FALSE, "Conflict_Resolution sheet cannot be read."))
  if (nrow(conflicts) == 0L) return(runner_validation(TRUE, "No title/abstract reviewer conflicts require adjudication."))
  if (!"final_decision" %in% names(conflicts)) return(runner_validation(FALSE, "Conflict_Resolution lacks final_decision."))

  dec <- trimws(as.character(conflicts$final_decision))
  active <- if ("screening_id" %in% names(conflicts)) !is.na(conflicts$screening_id) & nzchar(trimws(conflicts$screening_id)) else rep(TRUE, nrow(conflicts))
  ok <- all(!active | (!is.na(dec) & nzchar(dec)))
  if (ok && "final_exclusion_reason" %in% names(conflicts)) {
    ex <- active & tolower(dec) == "exclude"
    reason <- trimws(as.character(conflicts$final_exclusion_reason))
    ok <- all(!ex | (!is.na(reason) & nzchar(reason)))
  }

  runner_validation(ok, if (ok) "All reviewer conflicts are adjudicated." else "Human adjudication remains incomplete.", file = agent_paths$workbook)
}

runner_full_text_paths <- function(context) {
  list(
    processed = runner_active_root_path(context, "data", "processed", "full_text"),
    results = runner_active_root_path(context, "results", "full_text"),
    logs = context$paths$logs_directory
  )
}

runner_validate_full_text_registry <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))
  p <- runner_full_text_paths(context)
  files <- c(
    registry_csv = file.path(p$processed, "full_text_registry.csv"),
    registry_workbook = file.path(p$results, "full_text_registry.xlsx"),
    intake_report = file.path(p$logs, "full_text_registry_intake_report.txt")
  )
  ok <- vapply(files, runner_file_nonempty, logical(1))
  runner_validation(all(ok), if (all(ok)) "R/22 full-text registry/intake outputs are complete for the active project." else paste("Missing or empty canonical R/22 output(s):", paste(names(files)[!ok], collapse = ", ")), files = as.list(files))
}

runner_full_text_agent_paths <- function() {
  context <- runner_resolve_project_context()
  p <- runner_full_text_paths(context)
  list(
    context = context,
    workbook = file.path(p$results, "full_text_eligibility_agent.xlsx"),
    decisions = file.path(p$logs, "full_text_eligibility_agent_decisions.csv"),
    failures = file.path(p$logs, "full_text_eligibility_agent_failures.csv"),
    checkpoint = file.path(p$logs, "full_text_eligibility_agent_checkpoint.csv"),
    report = file.path(p$logs, "full_text_eligibility_agent_report.txt")
  )
}

runner_full_text_agent_workbook <- function() {
  p <- tryCatch(runner_full_text_agent_paths(), error = function(e) NULL)
  if (is.null(p)) "" else p$workbook
}

runner_validate_full_text_agent <- function(paths) {
  p <- tryCatch(runner_full_text_agent_paths(), error = function(e) NULL)
  if (is.null(p)) return(runner_validation(FALSE, "Canonical R/23 paths could not be resolved."))

  files <- c(workbook = p$workbook, decisions = p$decisions, failures = p$failures, checkpoint = p$checkpoint, report = p$report)
  ok <- vapply(files, runner_file_nonempty, logical(1))
  if (!all(ok)) return(runner_validation(FALSE, paste("Missing or empty canonical R/23 output(s):", paste(names(files)[!ok], collapse = ", ")), files = as.list(files)))

  txt <- runner_read_text(p$report)
  passed <- grepl("TASK-011 Agent Stage: PASS", txt, fixed = TRUE) || grepl("PASS: R/23 multimodal full-text eligibility Agent Reviewer 1 completed successfully.", txt, fixed = TRUE)
  runner_validation(passed, if (passed) "R/23 Agent full-text eligibility outputs are complete for the active project." else "R/23 report does not document PASS.", files = as.list(files))
}

runner_validate_full_text_author_gate <- function(paths) {
  wb <- runner_full_text_agent_workbook()
  if (!nzchar(wb) || !runner_file_nonempty(wb) || !runner_dependency_available("openxlsx")) {
    return(runner_validation(FALSE, "Full-text author-review workbook is unavailable for the active project."))
  }

  d <- tryCatch(openxlsx::readWorkbook(wb, sheet = "Author_Final_Review", startRow = 3, colNames = TRUE, skipEmptyRows = FALSE), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0L || !"author_final_decision" %in% names(d)) return(runner_validation(FALSE, "Author_Final_Review cannot be validated."))

  ids <- if ("screening_id" %in% names(d)) trimws(as.character(d$screening_id)) else rep("x", nrow(d))
  active <- !is.na(ids) & nzchar(ids)
  decisions <- trimws(as.character(d$author_final_decision))
  ok <- all(!active | decisions %in% c("Include", "Exclude"))

  if (ok && "author_final_exclusion_reason" %in% names(d)) {
    reasons <- trimws(as.character(d$author_final_exclusion_reason))
    ex <- active & decisions == "Exclude"
    ok <- all(!ex | (!is.na(reasons) & nzchar(reasons)))
  }

  runner_validation(ok, if (ok) "1st Author final full-text eligibility review is complete." else "1st Author full-text review remains incomplete.", file = wb)
}

runner_validate_included_studies <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))
  p <- runner_full_text_paths(context)
  files <- c(
    included_studies = file.path(p$processed, "included_studies.csv"),
    final_full_text_eligibility = file.path(p$processed, "final_full_text_eligibility.csv"),
    excluded_after_full_text = file.path(p$processed, "excluded_after_full_text.csv")
  )
  ok <- vapply(files, runner_file_nonempty, logical(1))
  runner_validation(all(ok), if (all(ok)) "Authoritative R/24 Included Studies and full-text audit outputs are present for the active project." else paste("Missing or empty canonical R/24 output(s):", paste(names(files)[!ok], collapse = ", ")), files = as.list(files))
}

runner_validate_prisma <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))
  root <- runner_active_root_path(context, "results", "prisma")
  files <- c(
    pdf = file.path(root, "prisma_2020_flow_diagram.pdf"),
    png = file.path(root, "prisma_2020_flow_diagram.png"),
    audit = file.path(root, "prisma_flow_audit.csv")
  )
  ok <- vapply(files, runner_file_nonempty, logical(1))
  runner_validation(all(ok), if (all(ok)) "PRISMA PDF/PNG and audit are complete for the active project." else paste("Missing or empty canonical R/35 output(s):", paste(names(files)[!ok], collapse = ", ")), files = as.list(files))
}

runner_validate_extraction <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))
  processed <- runner_active_root_path(context, "data", "processed", "extraction")
  results <- runner_active_root_path(context, "results", "extraction")
  files <- c(
    study = file.path(processed, "study_characteristics_agent.csv"),
    group = file.path(processed, "group_characteristics_agent.csv"),
    outcome = file.path(processed, "outcome_data_agent.csv"),
    workbook = file.path(results, "multimodal_data_extraction_agent.xlsx")
  )
  ok <- vapply(files, runner_file_nonempty, logical(1))
  runner_validation(all(ok), if (all(ok)) "Multimodal extraction outputs are complete for the active project." else paste("Missing or empty canonical R/25 output(s):", paste(names(files)[!ok], collapse = ", ")), files = as.list(files))
}

runner_validate_manuscript_tables <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))
  root <- runner_active_root_path(context, "results", "manuscript", "tables")
  files <- c(
    table1 = file.path(root, "table1_study_characteristics.csv"),
    table2 = file.path(root, "table2_patient_characteristics.csv"),
    table3 = file.path(root, "table3_intervention_characteristics.csv"),
    workbook = file.path(root, "manuscript_tables_generated.xlsx")
  )
  ok <- vapply(files, runner_file_nonempty, logical(1))
  runner_validation(all(ok), if (all(ok)) "Three standard R/34 manuscript tables are complete for the active project." else paste("Missing or empty canonical R/34 output(s):", paste(names(files)[!ok], collapse = ", ")), files = as.list(files))
}

runner_validate_scope_advisory <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))
  root <- runner_active_root_path(context, "results", "extraction", "research_scope_advisory")
  files <- c(
    json = file.path(root, "research_scope_advisory.json"),
    meta_sets = file.path(root, "candidate_meta_analysis_sets.csv"),
    harmonization = file.path(root, "outcome_harmonization.csv"),
    study_roles = file.path(root, "study_roles.csv")
  )
  ok <- vapply(files, runner_file_nonempty, logical(1))
  runner_validation(all(ok), if (all(ok)) "Research Scope Advisory outputs are complete for the active project." else paste("Missing or empty canonical R/25A output(s):", paste(names(files)[!ok], collapse = ", ")), files = as.list(files))
}

runner_targeted_workbook <- function() {
  context <- runner_project_context_or_null()
  if (is.null(context)) return("")
  runner_active_root_path(context, "results", "extraction", "targeted_human_verification", "targeted_human_verification.xlsx")
}

runner_validate_targeted_prep <- function(paths) {
  wb <- runner_targeted_workbook()
  runner_validation(runner_file_nonempty(wb), if (runner_file_nonempty(wb)) "Targeted Human Verification workbook is present for the active project." else "R/25B targeted verification workbook is missing for the active project.", file = wb)
}

runner_validate_extraction_human_gate <- function(paths) {
  wb <- runner_targeted_workbook()
  if (!runner_file_nonempty(wb) || !runner_dependency_available("openxlsx")) return(runner_validation(FALSE, "Targeted Human Verification workbook is unavailable for the active project."))

  d <- tryCatch(openxlsx::readWorkbook(wb, sheet = "Target_Outcome_Verification", colNames = TRUE, skipEmptyRows = TRUE), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0L) return(runner_validation(FALSE, "Target_Outcome_Verification cannot be read."))
  required <- c("human_include_in_planned_analysis", "verification_status")
  if (!all(required %in% names(d))) return(runner_validation(FALSE, "Target_Outcome_Verification lacks required human verification columns."))

  include <- trimws(as.character(d$human_include_in_planned_analysis))
  planned <- !is.na(include) & include == "Yes"
  if (!any(planned)) return(runner_validation(FALSE, "No planned analysis row is marked Yes yet."))
  status <- trimws(as.character(d$verification_status))
  ok <- all(!planned | status %in% c("Verified", "Corrected"))
  runner_validation(ok, if (ok) "Human Verification of all planned analysis rows is complete." else "One or more planned analysis rows are not Verified/Corrected.", file = wb)
}

runner_resolve_manifest_dataset_path <- function(raw_path, context, canonical_directory) {
  value <- runner_clean_scalar(raw_path)
  if (!nzchar(value)) return("")

  candidate <- if (runner_path_is_absolute(value)) {
    value
  } else if (grepl("[/\\\\]", value)) {
    file.path(context$paths$project_root, value)
  } else {
    file.path(canonical_directory, value)
  }

  normalized <- runner_normalize_path_safe(candidate, must_work = FALSE)
  if (!nzchar(normalized) || !runner_path_within(normalized, canonical_directory)) return("")
  normalized
}

runner_validate_human_verified_finalization <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))

  canonical_dir <- runner_active_root_path(context, "data", "processed", "meta_analysis_ready")
  manifest <- file.path(canonical_dir, "meta_analysis_manifest.csv")
  if (!runner_file_nonempty(manifest)) return(runner_validation(FALSE, "Canonical R/26 authoritative analysis manifest is missing.", manifest = manifest))

  manifest_data <- runner_read_csv_character(manifest)
  if (is.null(manifest_data) || nrow(manifest_data) == 0L) return(runner_validation(FALSE, "Canonical R/26 analysis manifest cannot be interpreted.", manifest = manifest))

  dataset_col <- intersect(c("dataset_file", "file", "path"), names(manifest_data))
  if (length(dataset_col) == 0L) return(runner_validation(FALSE, "R/26 analysis manifest does not contain a dataset path column.", manifest = manifest))

  raw_paths <- trimws(as.character(manifest_data[[dataset_col[[1]]]]))
  raw_paths <- raw_paths[!is.na(raw_paths) & nzchar(raw_paths)]
  if (length(raw_paths) == 0L) return(runner_validation(FALSE, "R/26 analysis manifest contains no dataset paths.", manifest = manifest))

  dataset_paths <- vapply(
    raw_paths,
    runner_resolve_manifest_dataset_path,
    character(1),
    context = context,
    canonical_directory = canonical_dir,
    USE.NAMES = FALSE
  )

  in_scope <- nzchar(dataset_paths)
  if (!all(in_scope)) {
    return(runner_validation(FALSE, paste0("R/26 manifest references dataset path(s) outside the active canonical meta-analysis-ready directory: ", paste(raw_paths[!in_scope], collapse = ", ")), manifest = manifest))
  }

  dataset_ok <- vapply(dataset_paths, runner_file_nonempty, logical(1))
  if (!all(dataset_ok)) return(runner_validation(FALSE, paste0("R/26 canonical manifest references missing or empty dataset(s): ", paste(dataset_paths[!dataset_ok], collapse = ", ")), manifest = manifest))

  audit_workbook <- runner_active_root_path(context, "results", "meta_analysis_ready", "meta_analysis_ready_datasets.xlsx")
  runner_validation(
    TRUE,
    if (runner_file_nonempty(audit_workbook)) "R/26 canonical manifest, referenced datasets, and final audit workbook are complete." else "R/26 canonical manifest and all referenced active-project datasets are complete; audit workbook is not required for downstream analytical recovery.",
    manifest = manifest,
    workbook = if (runner_file_nonempty(audit_workbook)) audit_workbook else ""
  )
}

runner_validate_meta_analysis <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))
  f <- runner_active_root_path(context, "results", "meta_analysis", "meta_analysis_execution_summary.csv")
  if (!runner_file_nonempty(f)) return(runner_validation(FALSE, "Meta-analysis execution summary is missing for the active project.", file = f))

  d <- runner_read_csv_character(f)
  if (is.null(d) || nrow(d) == 0L || !"status" %in% names(d)) return(runner_validation(FALSE, "Meta-analysis execution summary cannot be interpreted.", file = f))
  st <- toupper(trimws(as.character(d$status)))
  ok <- !any(st == "FAIL") && any(st == "PASS")
  runner_validation(ok, if (ok) "Meta-analysis completed with no FAIL groups." else "Meta-analysis has FAIL groups or no PASS group.", file = f)
}

runner_validate_meta_analysis_diagnostics <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))
  root <- runner_active_root_path(context, "results", "meta_analysis_diagnostics")
  summary_file <- file.path(root, "meta_analysis_diagnostics_summary.csv")
  report_file <- file.path(context$paths$logs_directory, "meta_analysis_diagnostics_report.txt")
  workbook <- file.path(root, "meta_analysis_diagnostics.xlsx")

  required <- c(summary = summary_file, report = report_file, workbook = workbook)
  ok_files <- vapply(required, runner_file_nonempty, logical(1))
  if (!all(ok_files)) return(runner_validation(FALSE, paste("R/28 canonical diagnostic output(s) are incomplete:", paste(names(required)[!ok_files], collapse = ", ")), files = as.list(required)))

  d <- runner_read_csv_character(summary_file)
  if (is.null(d) || nrow(d) == 0L || !"diagnostic_status" %in% names(d)) return(runner_validation(FALSE, "R/28 diagnostics summary cannot be interpreted.", file = summary_file))
  st <- toupper(trimws(as.character(d$diagnostic_status)))
  ok <- length(st) > 0L && all(!is.na(st) & st == "PASS")
  runner_validation(ok, if (ok) "R/28 post-meta-analysis diagnostics completed with no FAIL group." else "R/28 diagnostics contain a FAIL or incomplete group.", file = summary_file, report = report_file, workbook = workbook)
}

runner_validate_forest_plots <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))
  report <- runner_active_root_path(context, "results", "meta_analysis", "logs", "r33_forest_and_tables_report.txt")
  if (!runner_file_nonempty(report)) return(runner_validation(FALSE, "Canonical R/33 report is missing.", file = report))
  txt <- runner_read_text(report)
  ok <- grepl("PASS: R/33 completed all executable forest-plot/table outputs", txt, fixed = TRUE) && !grepl("PARTIAL PASS", txt, fixed = TRUE)
  runner_validation(ok, if (ok) "R/33 publication forest/table outputs passed for the active project." else "R/33 did not achieve complete PASS.", file = report)
}

runner_risk_of_bias_root <- function(context) {
  runner_active_root_path(context, "results", "risk_of_bias")
}

runner_validate_risk_of_bias <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))
  root <- runner_risk_of_bias_root(context)
  files <- c(
    manifest = file.path(root, "rob_study_result_manifest.csv"),
    tasks = file.path(root, "rob_agent_tasks.csv"),
    assessments = file.path(root, "rob_agent_assessments.csv"),
    workbook = file.path(root, "risk_of_bias_author_editable.xlsx"),
    report = file.path(root, "rob_validation_report.txt")
  )
  ok <- vapply(files, runner_file_nonempty, logical(1))
  runner_validation(all(ok), if (all(ok)) "R/36 provisional RoB assessments are complete and the author-review workbook is ready for the active project." else paste("Missing or empty canonical R/36 output(s):", paste(names(files)[!ok], collapse = ", ")), files = as.list(files))
}

runner_rob_workbook <- function() {
  context <- runner_project_context_or_null()
  if (is.null(context)) return("")
  file.path(runner_risk_of_bias_root(context), "risk_of_bias_author_editable.xlsx")
}

runner_validate_rob_author_gate <- function(paths) {
  wb <- runner_rob_workbook()
  if (!runner_file_nonempty(wb) || !runner_dependency_available("openxlsx")) return(runner_validation(FALSE, "RoB author-review workbook is unavailable for the active project."))

  domains <- tryCatch(openxlsx::read.xlsx(wb, sheet = "Domain_Review", check.names = FALSE), error = function(e) NULL)
  overall <- tryCatch(openxlsx::read.xlsx(wb, sheet = "Overall_Review", check.names = FALSE), error = function(e) NULL)
  if (is.null(domains) || is.null(overall)) return(runner_validation(FALSE, "Domain_Review or Overall_Review cannot be read."))
  if (!"author_domain_judgement" %in% names(domains) || !"author_final_overall_judgement" %in% names(overall)) return(runner_validation(FALSE, "Required author-final RoB judgement columns are missing."))

  dj <- trimws(as.character(domains$author_domain_judgement))
  oj <- trimws(as.character(overall$author_final_overall_judgement))
  ok <- nrow(domains) > 0L && nrow(overall) > 0L && all(!is.na(dj) & nzchar(dj)) && all(!is.na(oj) & nzchar(oj))
  runner_validation(ok, if (ok) "All author-final RoB domain and overall judgements are complete." else "Author-final RoB review remains incomplete.", file = wb)
}

runner_validate_rob_finalization <- function(paths) {
  context <- runner_project_context_or_null()
  if (is.null(context)) return(runner_validation(FALSE, "Canonical active-project context could not be resolved."))
  root <- runner_risk_of_bias_root(context)
  figs <- file.path(root, "figures")
  files <- c(
    combined = file.path(root, "risk_of_bias_final.csv"),
    report = file.path(figs, "rob_figure_validation_report.txt"),
    rob2_traffic_light = file.path(figs, "rob2_traffic_light.pdf"),
    robins_i_traffic_light = file.path(figs, "robins_i_traffic_light.pdf")
  )
  ok <- vapply(files, runner_file_nonempty, logical(1))
  runner_validation(all(ok), if (all(ok)) "Final author-reviewed RoB data and publication figures are complete for the active project." else paste("Missing or empty canonical R/37 output(s):", paste(names(files)[!ok], collapse = ", ")), files = as.list(files))
}

runner_validate_step <- function(step, paths) {
  id <- step$id
  switch(
    id,
    initialization_agent = runner_validate_initialization_agent(paths),
    initialization_validation = runner_validate_initialization(paths),
    human_decision_review = runner_validate_review_preparation(paths),
    human_decision_finalization = runner_validate_finalization(paths),
    prospero_preparation = runner_validate_prospero(paths),
    search_generation = runner_validate_search_generation(paths),
    search_validation = runner_validate_search_validation(paths),
    pubmed_download = runner_validate_pubmed(paths),
    database_input_gate = runner_validate_database_inputs(paths),
    merge_deduplication = runner_validate_merge(paths),
    screening_workbook = runner_validate_screening_workbook(paths),
    agent_screening = runner_validate_agent_screening(paths),
    agent_validation = runner_validate_agent_validation(paths),
    human_reviewer_2_gate = runner_validate_human_reviewer_2(paths),
    human_adjudication_gate = runner_validate_human_adjudication(paths),
    screening_finalization = runner_validate_phase1_finalization(paths),
    full_text_registry = runner_validate_full_text_registry(paths),
    full_text_agent_eligibility = runner_validate_full_text_agent(paths),
    full_text_author_gate = runner_validate_full_text_author_gate(paths),
    final_included_studies = runner_validate_included_studies(paths),
    prisma = runner_validate_prisma(paths),
    multimodal_extraction = runner_validate_extraction(paths),
    manuscript_tables = runner_validate_manuscript_tables(paths),
    scope_advisory = runner_validate_scope_advisory(paths),
    targeted_verification_prep = runner_validate_targeted_prep(paths),
    extraction_human_gate = runner_validate_extraction_human_gate(paths),
    human_verified_finalization = runner_validate_human_verified_finalization(paths),
    meta_analysis = runner_validate_meta_analysis(paths),
    meta_analysis_diagnostics = runner_validate_meta_analysis_diagnostics(paths),
    forest_plots = runner_validate_forest_plots(paths),
    risk_of_bias = runner_validate_risk_of_bias(paths),
    rob_author_gate = runner_validate_rob_author_gate(paths),
    rob_finalization = runner_validate_rob_finalization(paths),
    additional_analysis = runner_validation(FALSE, "R/29 state is evaluated directly by the Runner."),
    runner_validation(FALSE, paste("No validator registered for", id))
  )
}

# ============================================================
# 4. Script / service execution
# ============================================================
runner_execute_script <- function(script_path) {
  if (!file.exists(script_path)) stop("Workflow script was not found: ", script_path, call. = FALSE)
  env <- new.env(parent = globalenv())
  value <- sys.source(script_path, envir = env)
  list(value=value, environment=env)
}

runner_execute_prospero <- function(script_path) {
  rscript <- Sys.which("Rscript")
  if (!nzchar(rscript)) stop("Rscript executable was not found; cannot isolate the PROSPERO module safely.", call. = FALSE)
  slug <- runner_detect_project_slug()
  args <- c("--vanilla", script_path)
  if (nzchar(slug) && grepl("create_and_autofill_prospero\\.R$", script_path, ignore.case=TRUE)) {
    args <- c(args, paste0("--project=", slug))
  }
  status <- system2(rscript, args=args)
  if (!identical(as.integer(status), 0L)) stop("PROSPERO preparation module exited with status ", status, call. = FALSE)
  invisible(TRUE)
}

runner_route_r29_uploads <- function(uploaded_files) {
  uploaded_files <- runner_as_character_vector(uploaded_files)
  if (length(uploaded_files)==0L) return(invisible(NULL))
  missing <- uploaded_files[!file.exists(uploaded_files)]
  if (length(missing)>0L) stop("Uploaded file(s) do not exist:\n- ", paste(missing, collapse="\n- "), call. = FALSE)
  script <- runner_resolve_script(c("R/32_route_runner_uploads_to_active_analysis.R"))
  if (!nzchar(script)) stop("R/32 upload-routing service is missing.", call. = FALSE)

  old <- Sys.getenv("SRMA_UPLOADED_FILES", unset=NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("SRMA_UPLOADED_FILES") else Sys.setenv(SRMA_UPLOADED_FILES=old)
  }, add=TRUE)
  Sys.setenv(SRMA_UPLOADED_FILES="")
  env <- new.env(parent=globalenv())
  sys.source(script, envir=env)
  if (!exists("route_uploaded_files_to_active_analysis", envir=env, mode="function")) stop("R/32 routing function was not loaded.", call. = FALSE)
  env$route_uploaded_files_to_active_analysis(uploaded_files)
}

runner_extract_r29_status <- function(value) {
  if (is.null(value)) return("")
  if (is.list(value)) {
    if (runner_is_nonempty_character(value$status)) return(toupper(trimws(value$status)))
    if (is.list(value$state) && runner_is_nonempty_character(value$state$status)) return(toupper(trimws(value$state$status)))
  }
  ""
}

runner_additional_analysis_results_root <- function() {
  context <- runner_project_context_or_null()
  if (is.null(context)) return("")

  # R/31 may have added a canonical configured location. Use it only
  # through the active project context; otherwise use the validated
  # default under the active project root.
  if (
    exists("resolve_configured_project_path", envir = context$environment, mode = "function") &&
      !is.null(context$config$canonical_paths$additional_analysis_results)
  ) {
    resolved <- tryCatch(
      context$environment$resolve_configured_project_path(
        config = context$config,
        project_root = context$paths$project_root,
        key = "additional_analysis_results",
        create = FALSE
      ),
      error = function(e) ""
    )
    if (nzchar(resolved) && runner_path_within(resolved, context$paths$project_root)) return(resolved)
  }

  runner_active_root_path(context, "results", "additional_analysis")
}

runner_read_active_r29_state <- function() {
  root <- runner_additional_analysis_results_root()
  if (!nzchar(root)) return(NULL)

  active_file <- file.path(root, "ACTIVE_ANALYSIS_ID.txt")
  if (!runner_file_nonempty(active_file)) return(NULL)

  lines <- readLines(active_file, warn = FALSE, encoding = "UTF-8")
  if (length(lines) == 0L) return(NULL)
  id <- trimws(lines[[1]])
  if (!nzchar(id) || grepl("[\\\\/:*?\"<>|]", id)) return(NULL)

  state_file <- file.path(root, id, "analysis_state.json")
  if (!runner_file_nonempty(state_file) || !runner_dependency_available("jsonlite")) return(NULL)

  tryCatch(jsonlite::read_json(state_file, simplifyVector = TRUE), error = function(e) NULL)
}

runner_pause <- function(state, step, paths, run_log, run_log_file, message, validation=list()) {
  state$status <- "paused"
  state$current_step <- step$id
  state$last_error <- ""
  state <- runner_record_step(state, step$id, "paused", message=message, validation=validation)
  runner_write_state(state, paths$state_file)
  run_log$status <- "paused"
  run_log$completed_at <- runner_now()
  run_log$steps[[step$id]] <- list(status="paused", message=message, validation=validation)
  yaml::write_yaml(run_log, run_log_file)
  cat("\nWorkflow paused safely.\nStep: ", step$id, "\nReason: ", message, "\nResume by running the same Runner again.\n", sep="")
  invisible(list(passed=FALSE, paused=TRUE, state=state, state_file=paths$state_file, run_log_file=run_log_file, message=message))
}

# ============================================================
# 5. Main Runner
# ============================================================
run_universal_srma_workflow <- function(
    resume = TRUE,
    from_step = NULL,
    run_scope_advisory = TRUE,
    run_additional_analysis = FALSE,
    additional_evidence_role = NULL,
    additional_purpose = NULL,
    additional_effect_measure = "AUTO",
    uploaded_files = NULL
) {
  runner_require_package("yaml")
  runner_assert_project_root()

  paths <- runner_paths()
  steps <- runner_steps(run_scope_advisory, run_additional_analysis)
  step_ids <- vapply(steps, `[[`, character(1), "id")

  if (!is.null(from_step) && (!runner_is_nonempty_character(from_step) || !from_step %in% step_ids)) {
    stop("Invalid from_step. Choose one of:\n- ", paste(step_ids, collapse="\n- "), call. = FALSE)
  }

  dir.create(paths$state_directory, recursive=TRUE, showWarnings=FALSE)
  dir.create(paths$run_log_directory, recursive=TRUE, showWarnings=FALSE)

  state <- runner_load_state(paths$state_file, resume=resume)
  if (!isTRUE(resume)) state <- runner_empty_state()

  run_id <- paste0("workflow_", runner_timestamp())
  run_log_file <- file.path(paths$run_log_directory, paste0(run_id, ".yml"))
  run_log <- list(
    run_id=run_id,
    workflow="universal_srma_final",
    runner_version=runner_empty_state()$runner_version,
    started_at=runner_now(),
    completed_at="",
    status="running",
    resume_requested=isTRUE(resume),
    from_step=from_step %||% "",
    options=list(scope_advisory=isTRUE(run_scope_advisory), additional_analysis=isTRUE(run_additional_analysis)),
    steps=list(),
    error=""
  )

  runner_print_header("Universal SR-MA Final Workflow Runner — TASK-016")
  cat("Run ID: ", run_id, "\nResume: ", isTRUE(resume), "\nState: ", paths$state_file, "\n", sep="")

  if (!nzchar(state$started_at %||% "")) state$started_at <- runner_now()
  state$status <- "running"
  state$completed_at <- ""
  state$last_error <- ""
  runner_write_state(state, paths$state_file)

  start_index <- 1L
  if (!is.null(from_step)) {
    start_index <- match(from_step, step_ids)
    downstream <- step_ids[seq.int(start_index, length(step_ids))]
    state$completed_steps <- setdiff(state$completed_steps, downstream)
    for (id in downstream) state$step_records[[id]] <- NULL
    runner_write_state(state, paths$state_file)
  }

  total <- length(steps)

  for (index in seq_along(steps)) {
    step <- steps[[index]]

    if (index < start_index) {
      runner_print_step(index, total, step$label, "OUTSIDE REQUESTED RANGE")
      next
    }

    if (!isTRUE(step$enabled)) {
      state <- runner_record_step(state, step$id, "skipped", completed_at=runner_now(), message=paste(step$category, "branch disabled by Runner option."))
      runner_write_state(state, paths$state_file)
      run_log$steps[[step$id]] <- list(status="skipped", category=step$category)
      runner_print_step(index, total, step$label, "SKIPPED")
      next
    }

    # A completed state is trusted only if the completion validator still passes.
    pre <- runner_validate_step(step, paths)
    if (step$id %in% state$completed_steps && isTRUE(pre$passed)) {
      runner_print_step(index, total, step$label, "ALREADY COMPLETE")
      run_log$steps[[step$id]] <- list(status="recovered", validation=pre)
      next
    }

    if (isTRUE(step$recovery) && isTRUE(pre$passed)) {
      state <- runner_record_step(state, step$id, "completed", completed_at=runner_now(), message=paste("Recovered from validated artifacts:", pre$message), validation=pre)
      runner_write_state(state, paths$state_file)
      run_log$steps[[step$id]] <- list(status="recovered", validation=pre)
      runner_print_step(index, total, step$label, "RECOVERED")
      next
    }

    runner_print_step(index, total, step$label)
    started <- runner_now()
  state$current_step <- step$id
  runner_write_state(state, paths$state_file)

  # PROSPERO preparation requires genuine interactive terminal input.
  # Do not launch it as a child Rscript process from inside the Runner,
  # because stdin may not be available and readline() can loop indefinitely.
  # A prepared canonical artifact or existing-registration evidence is
  # recovered above before this point.
  if (identical(step$id, "prospero_preparation")) {
    return(
      runner_pause(
        state,
        step,
        paths,
        run_log,
        run_log_file,
        paste(
          pre$message,
          "PROSPERO preparation requires interactive user input.",
          "From the repository root, run:",
          'source("R/create_and_autofill_prospero.R")',
          "Then rerun the same Universal Runner."
        ),
        pre
      )
    )
  }

  if (identical(step$type, "gate")) {
    return(runner_pause(state, step, paths, run_log, run_log_file, pre$message, pre))
  }

    if (identical(step$type, "r29")) {
      # SERVICE: route uploaded files only when explicitly supplied and an active R/29 exists.
      upload_vector <- runner_as_character_vector(uploaded_files)
      if (length(upload_vector)==0L) {
        env_uploads <- trimws(Sys.getenv("SRMA_UPLOADED_FILES", unset=""))
        if (nzchar(env_uploads)) upload_vector <- trimws(strsplit(env_uploads, ";", fixed=TRUE)[[1]])
      }
      if (length(upload_vector)>0L) runner_route_r29_uploads(upload_vector)

      active <- runner_read_active_r29_state()
      active_status <- if (is.list(active)) toupper(trimws(as.character(active$status %||% ""))) else ""
      if (!interactive() && !nzchar(active_status)) {
        role <- trimws(as.character(additional_evidence_role %||% Sys.getenv("SRMA_ADDITIONAL_EVIDENCE_ROLE", unset="")))
        purpose <- trimws(as.character(additional_purpose %||% Sys.getenv("SRMA_ADDITIONAL_PURPOSE", unset="")))
        if (!nzchar(role) || !nzchar(purpose)) {
          return(runner_pause(state, step, paths, run_log, run_log_file, "R/29 is enabled, but a new non-interactive additional analysis requires additional_evidence_role and additional_purpose."))
        }
        Sys.setenv(SRMA_ADDITIONAL_ANALYSIS="yes", SRMA_ADDITIONAL_EVIDENCE_ROLE=role, SRMA_ADDITIONAL_PURPOSE=purpose, SRMA_ADDITIONAL_EFFECT_MEASURE=additional_effect_measure %||% "AUTO")
      }

      script <- runner_resolve_script(step$scripts)
      if (!nzchar(script)) stop("Required R/29 script is missing.", call. = FALSE)
      execution <- tryCatch(runner_execute_script(script), error=function(e) e)
      if (inherits(execution, "error")) {
        state$status <- "failed"; state$last_error <- conditionMessage(execution)
        state <- runner_record_step(state, step$id, "failed", started_at=started, completed_at=runner_now(), message=state$last_error)
        runner_write_state(state, paths$state_file)
        run_log$status <- "failed"; run_log$error <- state$last_error; run_log$completed_at <- runner_now(); run_log$steps[[step$id]] <- list(status="failed", error=state$last_error); yaml::write_yaml(run_log, run_log_file)
        stop("Workflow failed at R/29: ", state$last_error, call. = FALSE)
      }
      status <- runner_extract_r29_status(execution$value)
      if (!nzchar(status)) {
        active2 <- runner_read_active_r29_state()
        if (is.list(active2)) status <- toupper(trimws(as.character(active2$status %||% "")))
      }
      if (status %in% c("WAITING_FOR_FILES","FILES_AVAILABLE")) {
        return(runner_pause(state, step, paths, run_log, run_log_file, paste("R/29 state:", status, "— upload source files and resume."), list(r29_status=status)))
      }
      if (status %in% c("SKIPPED","CANCELLED","COMPLETE") || !nzchar(status)) {
        state$current_step <- ""
        state <- runner_record_step(state, step$id, "completed", started_at=started, completed_at=runner_now(), message=paste("R/29 terminal state:", ifelse(nzchar(status),status,"EXECUTED")), validation=list(passed=TRUE,r29_status=status))
        runner_write_state(state, paths$state_file)
        run_log$steps[[step$id]] <- list(status="completed", r29_status=status)
        runner_print_step(index,total,step$label, ifelse(status=="SKIPPED","SKIPPED","PASS"))
        next
      }
      return(runner_pause(state, step, paths, run_log, run_log_file, paste("R/29 returned resumable/nonterminal state:", status), list(r29_status=status)))
    }

    script <- runner_resolve_script(step$scripts)
    if (!nzchar(script)) {
      msg <- paste("No canonical script candidate was found for", step$id, "Expected:", paste(step$scripts, collapse=", "))
      state$status <- "failed"; state$last_error <- msg
      state <- runner_record_step(state, step$id, "failed", started_at=started, completed_at=runner_now(), message=msg)
      runner_write_state(state, paths$state_file)
      run_log$status <- "failed"; run_log$error <- msg; run_log$completed_at <- runner_now(); run_log$steps[[step$id]] <- list(status="failed", error=msg); yaml::write_yaml(run_log, run_log_file)
      stop(msg, call. = FALSE)
    }

    execution_error <- NULL
    tryCatch({
      if (identical(step$id, "scope_advisory")) {
        old_opt <- getOption("srma.scope_advisory", NULL)
        on.exit(options(srma.scope_advisory=old_opt), add=TRUE)
        options(srma.scope_advisory="yes")
      }
      if (identical(step$type, "external_script")) runner_execute_prospero(script) else runner_execute_script(script)
    }, error=function(e) execution_error <<- e)

    if (!is.null(execution_error)) {
      msg <- conditionMessage(execution_error)
      state$status <- "failed"; state$last_error <- msg
      state <- runner_record_step(state, step$id, "failed", started_at=started, completed_at=runner_now(), message=msg)
      runner_write_state(state, paths$state_file)
      run_log$status <- "failed"; run_log$error <- msg; run_log$completed_at <- runner_now(); run_log$steps[[step$id]] <- list(status="failed", error=msg); yaml::write_yaml(run_log, run_log_file)
      runner_print_step(index,total,step$label,"FAILED")
      stop("Universal SR-MA workflow failed at step '", step$id, "': ", msg, call. = FALSE)
    }

    post <- if (identical(step$type,"script_success")) runner_validation(TRUE, paste(step$label, "executed successfully."), historical_pass=TRUE) else runner_validate_step(step, paths)

  if (!isTRUE(post$passed)) {
  # Human Decision finalization is an interactive Human Gate.
  # If finalization remains incomplete after the script returns normally
  # (for example, because the user cancelled approval), pause safely
  # instead of treating the workflow as failed.
  if (identical(step$id, "human_decision_finalization")) {
    return(
      runner_pause(
        state,
        step,
        paths,
        run_log,
        run_log_file,
        post$message,
        post
      )
    )
  }

  # Registration preparation may legitimately require a later resume;
  # do not label it as an analytical failure.
  if (identical(step$id, "prospero_preparation")) {
    return(
      runner_pause(
        state,
        step,
        paths,
        run_log,
        run_log_file,
        post$message,
        post
      )
    )
  }

  msg <- post$message
  state$status <- "failed"
  state$last_error <- msg

  state <- runner_record_step(
    state,
    step$id,
    "failed",
    started_at = started,
    completed_at = runner_now(),
    message = msg,
    validation = post
  )

  runner_write_state(state, paths$state_file)

  run_log$status <- "failed"
  run_log$error <- msg
  run_log$completed_at <- runner_now()
  run_log$steps[[step$id]] <- list(
    status = "failed",
    validation = post
  )
  yaml::write_yaml(run_log, run_log_file)

  runner_print_step(
    index,
    total,
    step$label,
    "FAILED VALIDATION"
  )

  stop(
    "Step executed but completion validation failed: ",
    msg,
    call. = FALSE
  )
}

    state$current_step <- ""
    state <- runner_record_step(state, step$id, "completed", started_at=started, completed_at=runner_now(), message=post$message, validation=post)
    runner_write_state(state, paths$state_file)
    run_log$steps[[step$id]] <- list(status="completed", started_at=started, completed_at=runner_now(), validation=post)
    runner_print_step(index,total,step$label,"PASS")
  }

  active_steps <- vapply(steps, function(s) isTRUE(s$enabled), logical(1))
  required_ids <- vapply(steps[active_steps], `[[`, character(1), "id")
  missing <- setdiff(required_ids, state$completed_steps)
  if (length(missing)>0L) {
    state$status <- "failed"; state$last_error <- paste("Final workflow validation found incomplete active step(s):", paste(missing, collapse=", "))
    runner_write_state(state, paths$state_file)
    stop(state$last_error, call. = FALSE)
  }

  state$status <- "completed"
  state$current_step <- ""
  state$completed_at <- runner_now()
  state$last_error <- ""
  runner_write_state(state, paths$state_file)
  run_log$status <- "completed"
  run_log$completed_at <- runner_now()
  yaml::write_yaml(run_log, run_log_file)

  runner_print_header("Universal SR-MA Final Workflow Completed")
  cat("PASS: all active TASK-016 Runner stages are complete.\nState: ", paths$state_file, "\nRun log: ", run_log_file, "\n", sep="")
  invisible(list(passed=TRUE, paused=FALSE, state=state, state_file=paths$state_file, run_log_file=run_log_file))
}

# ============================================================
# 6. Default entry point
# ============================================================
if (isTRUE(getOption("srma.runner.autorun", TRUE))) {
  run_universal_srma_workflow(resume = TRUE)
}
