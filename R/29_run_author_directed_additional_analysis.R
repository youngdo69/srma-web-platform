# ============================================================
# Universal SR-MA Agent Toolkit
# R/29_run_author_directed_additional_analysis.R
# Revision: OpenAI Files API + file_id; DOCX/DOC supplementary files supported; curl multipart upload; automatic pooling; diagnostic API errors
#
# PURPOSE
# ------------------------------------------------------------
# Stateful Author-Directed Additional Analysis workflow after R/28.
#
# USER EXPERIENCE
# ------------------------------------------------------------
# R/28 PASS
#    ↓
# R/29
#    ↓
# Would you like an additional analysis?
#    ↓
# Select evidence role:
#   [1] Primary randomized evidence
#   [2] Sensitivity analysis of randomized evidence
#   [3] Observational evidence
#   [4] Other / exploratory evidence
#   [0] Skip
#    ↓
# Describe purpose
#    ↓
# analysis_id + workspace AUTO-CREATED
#    ↓
# "Please attach CSV/XLSX/PDF/DOCX/DOC file(s)"
#    ↓
# [OK]  <- the ONLY post-upload confirmation
#    ↓
# SAME R/29 workflow continues automatically
#    ↓
# detect files
#    ↓
# structured intake / multimodal extraction when needed
#    ↓
# automatic validation + harmonization
#    ↓
# pooled analysis
#    ↓
# print + save results
#
# IMPORTANT
# ------------------------------------------------------------
# - The user NEVER needs to know or type <analysis_id>.
# - The user NEVER needs to manually rerun R/29 in the intended
#   Runner/UI workflow.
# - Source files may already be present in the automatically created
#   intake folder; R/29 will detect and use them.
# - Supported intake extensions: CSV, XLSX, PDF, DOCX, DOC.
#   Word supplementary documents use the same structured-extraction
#   pathway as PDFs.
# - R/29 maintains a checkpoint state so a future Runner button/UI
#   can pause for uploads and automatically resume the same analysis.
# - PDFs are NOT pooled directly. They are first converted into a
#   structured extraction with source provenance, then automatically
#   validated/harmonized and pooled without a second Author OK gate.
# - R/29 never alters R/26 Human Verification or R/27 primary outputs.
# - R/29 results are additional analyses and have their own provenance.
#
# CURRENT IMPLEMENTATION
# ------------------------------------------------------------
# Interactive RStudio:
#   R/29 waits at the console while the user copies/attaches files,
#   then the user types OK.
#
# Future Runner/UI:
#   Runner can resume a WAITING_FOR_FILES state automatically.
#   After files are available, R/29 continues through extraction,
#   validation, pooling, and reporting without another approval gate.
#
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "purrr",
  "openxlsx",
  "metafor",
  "jsonlite"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

# API-related packages are checked only if multimodal / non-canonical
# extraction is actually required. Base64 encoding uses jsonlite,
# avoiding an unnecessary base64enc dependency.


# ============================================================
# 1. General helpers
# ============================================================

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_squish(x)
  x[is.na(x) | x == ""] <- NA_character_
  x
}

safe_chr <- function(x) {
  x <- clean_text(x)

  if (length(x) == 0L || is.na(x[[1]])) {
    return("")
  }

  as.character(x[[1]])
}

safe_numeric <- function(x) {
  suppressWarnings(
    as.numeric(
      clean_text(x)
    )
  )
}

normalize_yes_no <- function(x) {
  y <- tolower(
    clean_text(x)
  )

  dplyr::case_when(
    y %in% c("yes", "y", "true", "1", "include", "included") ~ "Yes",
    y %in% c("no", "n", "false", "0", "exclude", "excluded") ~ "No",
    TRUE ~ clean_text(x)
  )
}

now_text <- function() {
  format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )
}

timestamp_id <- function() {
  format(
    Sys.time(),
    "%Y%m%d_%H%M%S"
  )
}

slugify <- function(x, max_length = 45L) {
  x <- tolower(
    safe_chr(x)
  )

  x <- stringr::str_replace_all(
    x,
    "[^a-z0-9]+",
    "_"
  )

  x <- stringr::str_replace_all(
    x,
    "_+",
    "_"
  )

  x <- stringr::str_replace_all(
    x,
    "^_|_$",
    ""
  )

  if (x == "") {
    x <- "additional_analysis"
  }

  substr(
    x,
    1,
    max_length
  )
}

safe_file_component <- function(x) {
  slugify(
    x,
    max_length = 80L
  )
}

first_existing_column <- function(data, candidates) {
  hit <- candidates[
    candidates %in% names(data)
  ]

  if (length(hit) == 0L) {
    return(NA_character_)
  }

  hit[[1]]
}

normalize_effect_measure <- function(x) {
  raw <- clean_text(x)
  y <- toupper(raw)

  y <- stringr::str_replace_all(
    y,
    "[^A-Z0-9]+",
    ""
  )

  dplyr::case_when(
    y %in% c(
      "RR",
      "RISKRATIO",
      "RELATIVERISK"
    ) ~ "RR",

    y %in% c(
      "OR",
      "ODDSRATIO"
    ) ~ "OR",

    y %in% c(
      "HR",
      "HAZARDRATIO"
    ) ~ "HR",

    y %in% c(
      "RD",
      "RISKDIFFERENCE"
    ) ~ "RD",

    y %in% c(
      "MD",
      "MEANDIFFERENCE"
    ) ~ "MD",

    y %in% c(
      "SMD",
      "STANDARDIZEDMEANDIFFERENCE",
      "STANDARDISEDMEANDIFFERENCE"
    ) ~ "SMD",

    TRUE ~ NA_character_
  )
}

supported_effect_measures <- c(
  "RR",
  "OR",
  "HR",
  "RD",
  "MD",
  "SMD"
)

is_ratio_measure <- function(measure) {
  normalize_effect_measure(
    measure
  ) %in% c(
    "RR",
    "OR",
    "HR"
  )
}

inverse_transform <- function(x, measure) {
  if (
    is_ratio_measure(
      measure
    )
  ) {
    return(
      exp(x)
    )
  }

  x
}

format_num <- function(x, digits = 3L) {
  if (
    length(x) == 0L ||
      is.na(x)
  ) {
    return("NA")
  }

  formatC(
    x,
    digits = digits,
    format = "f"
  )
}

mime_type_for_file <- function(path) {
  ext <- tolower(
    tools::file_ext(
      path
    )
  )

  switch(
    ext,
    pdf = "application/pdf",
    csv = "text/csv",
    xlsx = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    doc = "application/msword",
    "application/octet-stream"
  )
}


# ============================================================
# 2. Project context
# ============================================================

source("R/utils/config_utils.R")
source("R/utils/path_utils.R")

config <- load_project_config(
  "config/project_config.yml",
  validate = TRUE,
  quiet = FALSE
)

paths <- build_project_paths(
  config = config,
  create_directories = TRUE,
  validate = TRUE
)

project_id <- get_project_id(
  config
)

project_title <- get_project_title(
  config
)


# ============================================================
# 3. Canonical directories
# ============================================================

r28_report <- file.path(
  paths$logs_directory,
  "meta_analysis_diagnostics_report.txt"
)

if (!file.exists(r28_report)) {
  stop(
    "R/28 diagnostic report was not found. ",
    "Run R/28 successfully before starting R/29.",
    call. = FALSE
  )
}

raw_root <- resolve_configured_project_path(
  config = config,
  project_root = paths$project_root,
  key = "additional_analysis_raw",
  create = TRUE
)

processed_root <- resolve_configured_project_path(
  config = config,
  project_root = paths$project_root,
  key = "additional_analysis_processed",
  create = TRUE
)

results_root <- resolve_configured_project_path(
  config = config,
  project_root = paths$project_root,
  key = "additional_analysis_results",
  create = TRUE
)

dir.create(
  raw_root,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  processed_root,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  results_root,
  recursive = TRUE,
  showWarnings = FALSE
)

active_id_file <- file.path(
  results_root,
  "ACTIVE_ANALYSIS_ID.txt"
)

registry_file <- file.path(
  results_root,
  "additional_analysis_registry.csv"
)


# ============================================================
# 4. State helpers
# ============================================================

write_state <- function(state, state_file) {
  jsonlite::write_json(
    state,
    state_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )
}

read_state <- function(state_file) {
  jsonlite::read_json(
    state_file,
    simplifyVector = TRUE
  )
}

update_registry <- function(state) {

  row <- tibble::tibble(
    analysis_id = as.character(
      safe_chr(
        state$analysis_id
      )
    ),
    project_id = as.character(
      safe_chr(
        state$project_id
      )
    ),
    evidence_role = as.character(
      safe_chr(
        state$evidence_role
      )
    ),
    purpose = as.character(
      safe_chr(
        state$purpose
      )
    ),
    preferred_effect_measure = as.character(
      safe_chr(
        state$preferred_effect_measure
      )
    ),
    status = as.character(
      safe_chr(
        state$status
      )
    ),
    created_at = as.character(
      safe_chr(
        state$created_at
      )
    ),
    updated_at = as.character(
      safe_chr(
        state$updated_at
      )
    )
  )

  if (
    file.exists(
      registry_file
    )
  ) {

    registry <- readr::read_csv(
      registry_file,
      show_col_types = FALSE,
      col_types = readr::cols(
        .default = readr::col_character()
      )
    )

    registry <- registry |>
      dplyr::filter(
        analysis_id != row$analysis_id
      )

    registry <- dplyr::bind_rows(
      registry,
      row
    )

  } else {

    registry <- row
  }

  readr::write_csv(
    registry,
    registry_file,
    na = ""
  )
}

set_state_status <- function(
    state,
    new_status,
    state_file,
    extra = list()
) {

  state$status <- new_status
  state$updated_at <- now_text()

  if (length(extra) > 0L) {
    for (nm in names(extra)) {
      state[[nm]] <- extra[[nm]]
    }
  }

  write_state(
    state,
    state_file
  )

  update_registry(
    state
  )

  state
}

get_active_state <- function() {

  if (
    !file.exists(
      active_id_file
    )
  ) {
    return(NULL)
  }

  analysis_id <- safe_chr(
    readLines(
      active_id_file,
      warn = FALSE
    )
  )

  if (analysis_id == "") {
    return(NULL)
  }

  state_file <- file.path(
    results_root,
    analysis_id,
    "analysis_state.json"
  )

  if (
    !file.exists(
      state_file
    )
  ) {
    return(NULL)
  }

  state <- read_state(
    state_file
  )

  if (
    safe_chr(
      state$status
    ) %in% c(
      "COMPLETE",
      "CANCELLED",
      "SKIPPED"
    )
  ) {
    return(NULL)
  }

  list(
    state = state,
    state_file = state_file
  )
}

clear_active_analysis <- function() {
  if (
    file.exists(
      active_id_file
    )
  ) {
    unlink(
      active_id_file
    )
  }
}


# ============================================================
# 5. User/Runner input helpers
# ============================================================

ask_yes_no <- function(
    prompt,
    env_name = NULL,
    default = "yes"
) {

  if (
    !is.null(env_name)
  ) {

    env_value <- tolower(
      safe_chr(
        Sys.getenv(
          env_name,
          unset = ""
        )
      )
    )

    if (
      env_value %in% c(
        "yes",
        "y",
        "true",
        "1",
        "no",
        "n",
        "false",
        "0"
      )
    ) {
      return(
        env_value %in% c(
          "yes",
          "y",
          "true",
          "1"
        )
      )
    }
  }

  if (!interactive()) {
    return(
      identical(
        default,
        "yes"
      )
    )
  }

  repeat {

    answer <- tolower(
      trimws(
        readline(
          prompt
        )
      )
    )

    if (
      answer == "" &&
        default != ""
    ) {
      answer <- default
    }

    if (
      answer %in% c(
        "yes",
        "y"
      )
    ) {
      return(TRUE)
    }

    if (
      answer %in% c(
        "no",
        "n"
      )
    ) {
      return(FALSE)
    }

    cat(
      "Please answer Yes or No.\n"
    )
  }
}

select_evidence_role <- function() {

  env_value <- tolower(
    safe_chr(
      Sys.getenv(
        "SRMA_ADDITIONAL_EVIDENCE_ROLE",
        unset = ""
      )
    )
  )

  allowed_env <- c(
    "primary_randomized",
    "sensitivity_randomized",
    "observational",
    "other"
  )

  if (
    env_value %in% allowed_env
  ) {
    return(env_value)
  }

  if (!interactive()) {
    stop(
      "Non-interactive R/29 requires SRMA_ADDITIONAL_EVIDENCE_ROLE.",
      call. = FALSE
    )
  }

  cat("\n")
  cat("Select evidence role:\n")
  cat("  [1] Primary randomized evidence\n")
  cat("  [2] Sensitivity analysis of randomized evidence\n")
  cat("  [3] Observational evidence\n")
  cat("  [4] Other / exploratory evidence\n")
  cat("  [0] Skip\n\n")

  repeat {

    answer <- trimws(
      readline(
        "Selection: "
      )
    )

    result <- switch(
      answer,
      "1" = "primary_randomized",
      "2" = "sensitivity_randomized",
      "3" = "observational",
      "4" = "other",
      "0" = "skip",
      NULL
    )

    if (!is.null(result)) {
      return(result)
    }

    cat(
      "Please select 0, 1, 2, 3, or 4.\n"
    )
  }
}

ask_purpose <- function() {

  env_value <- safe_chr(
    Sys.getenv(
      "SRMA_ADDITIONAL_PURPOSE",
      unset = ""
    )
  )

  if (env_value != "") {
    return(env_value)
  }

  if (!interactive()) {
    stop(
      "Non-interactive R/29 requires SRMA_ADDITIONAL_PURPOSE.",
      call. = FALSE
    )
  }

  repeat {

    purpose <- trimws(
      readline(
        paste0(
          "Please describe the purpose of this additional analysis\n",
          "(e.g., Creatinine-only AKI sensitivity analysis): "
        )
      )
    )

    if (purpose != "") {
      return(purpose)
    }

    cat(
      "Purpose cannot be blank.\n"
    )
  }
}

ask_effect_measure <- function() {

  env_value <- toupper(
    safe_chr(
      Sys.getenv(
        "SRMA_ADDITIONAL_EFFECT_MEASURE",
        unset = ""
      )
    )
  )

  if (
    env_value %in% c(
      "AUTO",
      supported_effect_measures
    )
  ) {
    return(env_value)
  }

  if (!interactive()) {
    return("AUTO")
  }

  repeat {

    answer <- toupper(
      trimws(
        readline(
          "Preferred effect measure [AUTO/RR/OR/HR/RD/MD/SMD] (default AUTO): "
        )
      )
    )

    if (answer == "") {
      answer <- "AUTO"
    }

    if (
      answer %in% c(
        "AUTO",
        supported_effect_measures
      )
    ) {
      return(answer)
    }

    cat(
      "Unsupported measure. Select AUTO, RR, OR, HR, RD, MD, or SMD.\n"
    )
  }
}


# ============================================================
# 6. Create a new analysis request
# ============================================================

create_new_analysis <- function() {

  proceed <- ask_yes_no(
    "Would you like to perform an author-directed additional analysis? [Yes/No]: ",
    env_name = "SRMA_ADDITIONAL_ANALYSIS",
    default = "yes"
  )

  if (!proceed) {

    state <- list(
      analysis_id = paste0(
        "AA_",
        timestamp_id(),
        "_skipped"
      ),
      project_id = project_id,
      project_title = project_title,
      evidence_role = "",
      purpose = "",
      preferred_effect_measure = "",
      status = "SKIPPED",
      created_at = now_text(),
      updated_at = now_text()
    )

    update_registry(
      state
    )

    cat(
      "\nR/29 skipped by Author decision.\n"
    )

    return(
      list(
        terminal = TRUE,
        state = state
      )
    )
  }

  evidence_role <- select_evidence_role()

  if (evidence_role == "skip") {

    state <- list(
      analysis_id = paste0(
        "AA_",
        timestamp_id(),
        "_skipped"
      ),
      project_id = project_id,
      project_title = project_title,
      evidence_role = "",
      purpose = "",
      preferred_effect_measure = "",
      status = "SKIPPED",
      created_at = now_text(),
      updated_at = now_text()
    )

    update_registry(
      state
    )

    cat(
      "\nR/29 skipped by Author decision.\n"
    )

    return(
      list(
        terminal = TRUE,
        state = state
      )
    )
  }

  purpose <- ask_purpose()
  preferred_effect_measure <- ask_effect_measure()

  analysis_id_base <- paste0(
    "AA_",
    timestamp_id(),
    "_",
    slugify(
      purpose
    )
  )

  analysis_id <- analysis_id_base
  suffix <- 1L

  while (
    dir.exists(
      file.path(
        results_root,
        analysis_id
      )
    )
  ) {
    suffix <- suffix + 1L

    analysis_id <- paste0(
      analysis_id_base,
      "_",
      suffix
    )
  }

  raw_dir <- file.path(
    raw_root,
    analysis_id
  )

  processed_dir <- file.path(
    processed_root,
    analysis_id
  )

  result_dir <- file.path(
    results_root,
    analysis_id
  )

  dir.create(
    raw_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  dir.create(
    processed_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  dir.create(
    result_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  state_file <- file.path(
    result_dir,
    "analysis_state.json"
  )

  state <- list(
    analysis_id = analysis_id,
    project_id = project_id,
    project_title = project_title,
    evidence_role = evidence_role,
    purpose = purpose,
    preferred_effect_measure = preferred_effect_measure,
    status = "WAITING_FOR_FILES",
    created_at = now_text(),
    updated_at = now_text(),
    raw_directory = raw_dir,
    processed_directory = processed_dir,
    result_directory = result_dir,
    source_files = character(0),
    extraction_method = "",
    final_effect_measure = ""
  )

  write_state(
    state,
    state_file
  )

  update_registry(
    state
  )

  writeLines(
    analysis_id,
    active_id_file
  )

  list(
    terminal = FALSE,
    state = state,
    state_file = state_file
  )
}


# ============================================================
# 7. File gate
# ============================================================

accepted_extensions <- c(
  "csv",
  "xlsx",
  "pdf",
  "docx",
  "doc"
)

scan_source_files <- function(raw_dir) {

  files <- list.files(
    raw_dir,
    full.names = TRUE,
    recursive = FALSE,
    all.files = FALSE
  )

  files <- files[
    !stringr::str_detect(
      basename(files),
      "^~\\$"
    )
  ]

  ext <- tolower(
    tools::file_ext(
      files
    )
  )

  files[
    ext %in% accepted_extensions
  ]
}

wait_for_files <- function(state, state_file) {

  raw_dir <- safe_chr(
    state$raw_directory
  )

  repeat {

    files <- scan_source_files(
      raw_dir
    )

    if (length(files) > 0L) {

      source_manifest <- tibble::tibble(
        source_file = basename(files),
        full_path = normalizePath(
          files,
          winslash = "/",
          mustWork = FALSE
        ),
        extension = tolower(
          tools::file_ext(
            files
          )
        ),
        size_bytes = file.info(
          files
        )$size
      )

      readr::write_csv(
        source_manifest,
        file.path(
          state$result_directory,
          "source_manifest.csv"
        ),
        na = ""
      )

      state <- set_state_status(
        state,
        "FILES_AVAILABLE",
        state_file,
        extra = list(
          source_files = as.list(
            normalizePath(
              files,
              winslash = "/",
              mustWork = FALSE
            )
          )
        )
      )

      cat(
        "\nFiles detected:\n"
      )

      for (f in files) {
        cat(
          "  - ",
          basename(f),
          "\n",
          sep = ""
        )
      }

      return(
        list(
          ready = TRUE,
          state = state,
          files = files
        )
      )
    }

    if (!interactive()) {

      state <- set_state_status(
        state,
        "WAITING_FOR_FILES",
        state_file
      )

      cat("\n")
      cat("R/29 status: WAITING_FOR_FILES\n")
      cat(
        "Upload/copy CSV, XLSX, PDF, DOCX, or DOC source file(s) to:\n",
        raw_dir,
        "\n",
        sep = ""
      )
      cat(
        "The Runner/UI can resume the same R/29 analysis automatically after upload.\n"
      )

      return(
        list(
          ready = FALSE,
          state = state,
          files = character(0)
        )
      )
    }

    cat("\n")
    cat("============================================================\n")
    cat("R/29 — File Attachment Gate\n")
    cat("============================================================\n")
    cat(
      "Please attach/copy the supporting CSV, XLSX, PDF, DOCX, or DOC file(s) into:\n\n",
      raw_dir,
      "\n\n",
      sep = ""
    )
    cat(
      "After the file(s) are attached, type OK.\n"
    )
    cat(
      "You do NOT need to rerun R/29.\n\n"
    )

    answer <- toupper(
      trimws(
        readline(
          "Type OK after attaching files, or CANCEL: "
        )
      )
    )

    if (answer == "CANCEL") {

      state <- set_state_status(
        state,
        "CANCELLED",
        state_file
      )

      clear_active_analysis()

      return(
        list(
          ready = FALSE,
          cancelled = TRUE,
          state = state,
          files = character(0)
        )
      )
    }

    if (answer != "OK") {
      cat(
        "Please type OK after attaching files, or CANCEL.\n"
      )

      next
    }

    # Loop continues and scans again without requiring R/29 rerun.
  }
}


# ============================================================
# 8. Canonical structured-data mapping
# ============================================================

canonical_columns <- c(
  "study_id",
  "study_label",
  "study_design",
  "outcome_name",
  "outcome_definition",
  "timepoint",
  "data_type",
  "effect_measure",
  "effect_estimate",
  "lower_ci",
  "upper_ci",
  "intervention_events",
  "intervention_total",
  "comparator_events",
  "comparator_total",
  "intervention_mean",
  "intervention_sd",
  "intervention_n",
  "comparator_mean",
  "comparator_sd",
  "comparator_n",
  "adjusted",
  "adjustment_variables",
  "source_file",
  "source_location",
  "extraction_note",
  "certainty"
)

candidate_map <- list(
  study_id = c(
    "study_id",
    "screening_id",
    "id"
  ),
  study_label = c(
    "study_label",
    "author_year",
    "citation",
    "study_name"
  ),
  study_design = c(
    "study_design",
    "design",
    "study_type"
  ),
  outcome_name = c(
    "outcome_name",
    "outcome"
  ),
  outcome_definition = c(
    "outcome_definition",
    "definition"
  ),
  timepoint = c(
    "timepoint",
    "follow_up",
    "followup",
    "time_window"
  ),
  data_type = c(
    "data_type",
    "outcome_data_type",
    "meta_input_type"
  ),
  effect_measure = c(
    "effect_measure",
    "measure"
  ),
  effect_estimate = c(
    "effect_estimate",
    "estimate",
    "rr",
    "or",
    "hr"
  ),
  lower_ci = c(
    "lower_ci",
    "ci_lower",
    "lower"
  ),
  upper_ci = c(
    "upper_ci",
    "ci_upper",
    "upper"
  ),
  intervention_events = c(
    "intervention_events",
    "events_intervention",
    "treatment_events",
    "events_treatment"
  ),
  intervention_total = c(
    "intervention_total",
    "total_intervention",
    "treatment_total",
    "n_intervention"
  ),
  comparator_events = c(
    "comparator_events",
    "events_comparator",
    "control_events",
    "events_control"
  ),
  comparator_total = c(
    "comparator_total",
    "total_comparator",
    "control_total",
    "n_control"
  ),
  intervention_mean = c(
    "intervention_mean",
    "mean_intervention",
    "treatment_mean"
  ),
  intervention_sd = c(
    "intervention_sd",
    "sd_intervention",
    "treatment_sd"
  ),
  intervention_n = c(
    "intervention_n",
    "n_intervention",
    "treatment_n"
  ),
  comparator_mean = c(
    "comparator_mean",
    "mean_comparator",
    "control_mean"
  ),
  comparator_sd = c(
    "comparator_sd",
    "sd_comparator",
    "control_sd"
  ),
  comparator_n = c(
    "comparator_n",
    "n_comparator",
    "control_n"
  ),
  adjusted = c(
    "adjusted",
    "is_adjusted"
  ),
  adjustment_variables = c(
    "adjustment_variables",
    "adjusted_for",
    "covariates"
  ),
  source_location = c(
    "source_location",
    "page_table",
    "source"
  )
)

standardize_structured_data <- function(
    data,
    source_file
) {

  names(data) <- tolower(
    names(data)
  )

  names(data) <- stringr::str_replace_all(
    names(data),
    "[^a-z0-9]+",
    "_"
  )

  out <- tibble::tibble(
    .rows = nrow(data)
  )

  for (target in names(candidate_map)) {

    source_col <- first_existing_column(
      data,
      candidate_map[[target]]
    )

    if (is.na(source_col)) {
      out[[target]] <- NA_character_
    } else {
      out[[target]] <- clean_text(
        data[[source_col]]
      )
    }
  }

  # Infer effect measure from explicitly named RR/OR/HR columns
  # only when the source has exactly one such dedicated column.
  if (
    all(
      is.na(
        out$effect_measure
      )
    )
  ) {

    dedicated <- intersect(
      c(
        "rr",
        "or",
        "hr"
      ),
      names(data)
    )

    if (length(dedicated) == 1L) {
      out$effect_measure <- toupper(
        dedicated[[1]]
      )
    }
  }

  out$source_file <- basename(
    source_file
  )

  out$extraction_note <- "Direct structured file import"
  out$certainty <- "Author-provided structured data"

  # Basic data type inference.
  out$data_type <- dplyr::case_when(
    !is.na(
      safe_numeric(
        out$intervention_events
      )
    ) &
      !is.na(
        safe_numeric(
          out$intervention_total
        )
      ) &
      !is.na(
        safe_numeric(
          out$comparator_events
        )
      ) &
      !is.na(
        safe_numeric(
          out$comparator_total
        )
      ) ~ "dichotomous",

    !is.na(
      safe_numeric(
        out$effect_estimate
      )
    ) &
      !is.na(
        safe_numeric(
          out$lower_ci
        )
      ) &
      !is.na(
        safe_numeric(
          out$upper_ci
        )
      ) ~ "reported_effect",

    !is.na(
      safe_numeric(
        out$intervention_mean
      )
    ) &
      !is.na(
        safe_numeric(
          out$intervention_sd
        )
      ) ~ "continuous",

    TRUE ~ clean_text(
      out$data_type
    )
  )

  out
}

read_xlsx_candidate <- function(path) {

  sheets <- openxlsx::getSheetNames(
    path
  )

  candidates <- list()

  for (sheet in sheets) {

    dat <- tryCatch(
      openxlsx::readWorkbook(
        path,
        sheet = sheet,
        colNames = TRUE,
        skipEmptyRows = TRUE
      ),
      error = function(e) {
        NULL
      }
    )

    if (
      is.null(dat) ||
        nrow(dat) == 0L
    ) {
      next
    }

    names_lower <- tolower(
      names(dat)
    )

    names_lower <- stringr::str_replace_all(
      names_lower,
      "[^a-z0-9]+",
      "_"
    )

    score <- sum(
      names_lower %in% unique(
        unlist(
          candidate_map
        )
      )
    )

    candidates[[sheet]] <- list(
      data = dat,
      score = score
    )
  }

  if (length(candidates) == 0L) {
    return(NULL)
  }

  scores <- vapply(
    candidates,
    function(x) {
      x$score
    },
    FUN.VALUE = numeric(1)
  )

  best <- names(
    which.max(
      scores
    )
  )

  candidates[[best]]$data
}

try_direct_structured_import <- function(files) {

  unstructured_extensions <- c(
    "pdf",
    "docx",
    "doc"
  )

  if (
    any(
      tolower(
        tools::file_ext(
          files
        )
      ) %in% unstructured_extensions
    )
  ) {
    # PDF and Word documents are treated as source documents rather
    # than canonical tabular inputs. Route the complete file set
    # through structured extraction so supplementary tables/text can
    # be interpreted together with the main articles.
    return(NULL)
  }

  imported <- list()

  for (f in files) {

    ext <- tolower(
      tools::file_ext(
        f
      )
    )

    dat <- if (
      ext == "csv"
    ) {
      tryCatch(
        readr::read_csv(
          f,
          show_col_types = FALSE
        ),
        error = function(e) {
          NULL
        }
      )
    } else if (
      ext == "xlsx"
    ) {
      read_xlsx_candidate(
        f
      )
    } else {
      NULL
    }

    if (
      is.null(dat) ||
        nrow(dat) == 0L
    ) {
      return(NULL)
    }

    standardized <- standardize_structured_data(
      dat,
      source_file = f
    )

    usable_row <- (
      !is.na(
        standardized$study_id
      ) |
        !is.na(
          standardized$study_label
        )
    ) &
      (
        (
          !is.na(
            safe_numeric(
              standardized$effect_estimate
            )
          ) &
            !is.na(
              safe_numeric(
                standardized$lower_ci
              )
            ) &
            !is.na(
              safe_numeric(
                standardized$upper_ci
              )
            )
        ) |
          (
            !is.na(
              safe_numeric(
                standardized$intervention_events
              )
            ) &
              !is.na(
                safe_numeric(
                  standardized$intervention_total
                )
              ) &
              !is.na(
                safe_numeric(
                  standardized$comparator_events
                )
              ) &
              !is.na(
                safe_numeric(
                  standardized$comparator_total
                )
              )
          ) |
          (
            !is.na(
              safe_numeric(
                standardized$intervention_mean
              )
            ) &
              !is.na(
                safe_numeric(
                  standardized$intervention_sd
                )
              ) &
              !is.na(
                safe_numeric(
                  standardized$intervention_n
                )
              ) &
              !is.na(
                safe_numeric(
                  standardized$comparator_mean
                )
              ) &
              !is.na(
                safe_numeric(
                  standardized$comparator_sd
                )
              ) &
              !is.na(
                safe_numeric(
                  standardized$comparator_n
                )
              )
          )
      )

    standardized <- standardized[
      usable_row,
      ,
      drop = FALSE
    ]

    if (nrow(standardized) == 0L) {
      return(NULL)
    }

    imported[[basename(f)]] <- standardized
  }

  dplyr::bind_rows(
    imported
  )
}


# ============================================================
# 9. Multimodal / unstructured extraction via OpenAI API
# ============================================================
#
# Architecture:
#   local CSV/XLSX/PDF
#       ↓
#   OpenAI Files API upload (purpose = "user_data")
#       ↓
#   file_id(s)
#       ↓
#   Responses API input_file items
#       ↓
#   strict structured extraction
#
# This design intentionally avoids embedding large base64 file bodies
# inside the Responses JSON payload.
#
# API failures are NEVER reduced to only "HTTP 400".
# The server response body is printed and saved for diagnosis.
# ============================================================

require_api_packages <- function() {

  pkgs <- c(
    "httr2",
    "curl"
  )

  missing <- pkgs[
    !vapply(
      pkgs,
      requireNamespace,
      quietly = TRUE,
      FUN.VALUE = logical(1)
    )
  ]

  if (length(missing) > 0L) {
    stop(
      "Multimodal/unstructured extraction requires package(s): ",
      paste(
        missing,
        collapse = ", "
      ),
      call. = FALSE
    )
  }
}


resolve_additional_analysis_model <- function() {

  model <- safe_chr(
    Sys.getenv(
      "SRMA_ADDITIONAL_ANALYSIS_MODEL",
      unset = ""
    )
  )

  if (model != "") {
    return(model)
  }

  "gpt-5.6"
}


build_extraction_schema <- function() {

  record_properties <- list()

  string_fields <- canonical_columns

  for (nm in string_fields) {
    record_properties[[nm]] <- list(
      type = "string"
    )
  }

  list(
    type = "object",
    additionalProperties = FALSE,
    properties = list(
      analysis_purpose_interpreted = list(
        type = "string"
      ),
      evidence_role_interpreted = list(
        type = "string"
      ),
      records = list(
        type = "array",
        items = list(
          type = "object",
          additionalProperties = FALSE,
          properties = record_properties,
          required = string_fields
        )
      ),
      exclusions_or_uncertainties = list(
        type = "array",
        items = list(
          type = "string"
        )
      )
    ),
    required = c(
      "analysis_purpose_interpreted",
      "evidence_role_interpreted",
      "records",
      "exclusions_or_uncertainties"
    )
  )
}


build_extraction_prompt <- function(
    state,
    files
) {

  role_text <- switch(
    safe_chr(
      state$evidence_role
    ),
    primary_randomized = "Primary randomized evidence",
    sensitivity_randomized = "Sensitivity analysis of randomized evidence",
    observational = "Observational evidence",
    other = "Other / exploratory evidence",
    safe_chr(
      state$evidence_role
    )
  )

  preferred_measure <- safe_chr(
    state$preferred_effect_measure
  )

  paste0(
    "You are extracting quantitative evidence for an AUTHOR-DIRECTED ",
    "additional meta-analysis within a systematic review.

",

    "PROJECT TITLE:
",
    project_title,
    "

",

    "EVIDENCE ROLE REQUESTED:
",
    role_text,
    "

",

    "AUTHOR-STATED PURPOSE:
",
    safe_chr(
      state$purpose
    ),
    "

",

    "PREFERRED EFFECT MEASURE:
",
    preferred_measure,
    "

",

    "SOURCE FILES:
",
    paste(
      paste0(
        "- ",
        basename(files)
      ),
      collapse = "
"
    ),
    "

",

    "EXTRACTION RULES:
",

    "1. Extract ONLY outcome data directly relevant to the Author-stated purpose.
",

    "2. Use one record per independent study/outcome contribution intended for pooling.
",

    "3. Do NOT infer or manufacture a value that is not explicitly supported by the source.
",

    "4. Search the entire supplied file, including tables, figures, appendices and supplementary ",
    "material contained in that file, before deciding that the requested endpoint is unavailable.
",

    "4a. Word documents (DOCX/DOC) may contain supplementary tables that modify or clarify values reported in the main PDF and must be treated as equally eligible source material.\n",

    "5. If an outcome definition is unclear or the requested endpoint cannot be isolated, ",
    "do NOT force inclusion. Record the reason in exclusions_or_uncertainties.
",

    "6. For randomized evidence, prefer raw arm-level event/total data when clearly reported ",
    "for the requested endpoint.
",

    "7. For observational evidence, prefer the study's adjusted, weighted, propensity-based, ",
    "or other causal effect estimate with its 95% CI when such an estimate is reported.
",

    "8. If both adjusted and unadjusted estimates exist in an observational study, select the ",
    "estimate most appropriate for causal interpretation and explain that choice in extraction_note.
",

    "9. Preserve the source-reported effect measure. Never silently convert OR or HR to RR.
",

    "10. For raw dichotomous randomized data, leave effect_measure blank if the source gives ",
    "only event counts; the downstream meta-analysis engine may calculate RR if RR is selected.
",

    "11. source_file MUST match the filename supplied to the API.
",

    "12. source_location SHOULD identify page, table, figure, supplementary table, or section ",
    "whenever possible.
",

    "13. outcome_definition should preserve the operational definition relevant to this analysis, ",
    "for example serum-creatinine-only AKI versus combined KDIGO creatinine/urine-output AKI.
",

    "14. certainty must be High, Moderate, or Low according to how explicit the source is.
",

    "15. Use empty strings for fields that are genuinely unavailable.

",

    "Return only the structured output matching the supplied JSON schema."
  )
}


extract_response_output_text <- function(body) {

  if (
    !is.null(
      body$output_text
    )
  ) {
    direct <- safe_chr(
      body$output_text
    )

    if (direct != "") {
      return(direct)
    }
  }

  if (
    is.null(
      body$output
    )
  ) {
    return("")
  }

  pieces <- character(0)

  for (item in body$output) {

    if (
      is.null(
        item$content
      )
    ) {
      next
    }

    for (content_item in item$content) {

      if (
        !is.null(
          content_item$type
        ) &&
          identical(
            content_item$type,
            "output_text"
          ) &&
          !is.null(
            content_item$text
          )
      ) {

        pieces <- c(
          pieces,
          as.character(
            content_item$text
          )
        )
      }
    }
  }

  paste(
    pieces,
    collapse = "
"
  )
}


perform_openai_request <- function(
    req,
    state,
    diagnostic_name
) {

  # Do not let httr2 abort before we can read the OpenAI error body.
  req <- req |>
    httr2::req_error(
      is_error = function(resp) FALSE
    )

  resp <- httr2::req_perform(
    req
  )

  status <- httr2::resp_status(
    resp
  )

  body_text <- tryCatch(
    httr2::resp_body_string(
      resp
    ),
    error = function(e) {
      paste0(
        "<Unable to decode response body: ",
        conditionMessage(e),
        ">"
      )
    }
  )

  diagnostic_file <- file.path(
    state$result_directory,
    paste0(
      safe_file_component(
        diagnostic_name
      ),
      "_http_response.txt"
    )
  )

  writeLines(
    c(
      paste0(
        "HTTP STATUS: ",
        status
      ),
      "",
      body_text
    ),
    diagnostic_file
  )

  if (status >= 400L) {

    cat("
")
    cat("============================================================
")
    cat("OPENAI API ERROR
")
    cat("============================================================
")
    cat(
      "Request stage: ",
      diagnostic_name,
      "
",
      sep = ""
    )
    cat(
      "HTTP STATUS: ",
      status,
      "

",
      sep = ""
    )
    cat("OPENAI SERVER RESPONSE:
")
    cat(
      body_text,
      "

",
      sep = ""
    )
    cat(
      "Diagnostic response saved to:
",
      diagnostic_file,
      "

",
      sep = ""
    )

    stop(
      paste0(
        "OpenAI API request failed during ",
        diagnostic_name,
        ". See OPENAI SERVER RESPONSE above."
      ),
      call. = FALSE
    )
  }

  resp
}


upload_file_to_openai <- function(
    path,
    api_key,
    state
) {

  if (
    !file.exists(
      path
    )
  ) {
    stop(
      "Source file does not exist: ",
      path,
      call. = FALSE
    )
  }

  file_size <- file.info(
    path
  )$size

  if (
    length(file_size) != 1L ||
      is.na(file_size) ||
      file_size <= 0
  ) {
    stop(
      "Source file is empty or unreadable: ",
      path,
      call. = FALSE
    )
  }

  cat(
    "Uploading source file to OpenAI Files API: ",
    basename(path),
    "
",
    sep = ""
  )

  req <- httr2::request(
    "https://api.openai.com/v1/files"
  ) |>
    httr2::req_headers(
      Authorization = paste(
        "Bearer",
        api_key
      )
    ) |>
    httr2::req_body_multipart(
      purpose = "user_data",
      file = curl::form_file(
        path
      )
    ) |>
    httr2::req_timeout(
      seconds = 300
    )

  resp <- perform_openai_request(
    req = req,
    state = state,
    diagnostic_name = paste0(
      "file_upload_",
      basename(path)
    )
  )

  body <- httr2::resp_body_json(
    resp,
    simplifyVector = TRUE
  )

  file_id <- safe_chr(
    body$id
  )

  if (file_id == "") {
    stop(
      "OpenAI Files API succeeded but returned no file id for: ",
      basename(path),
      call. = FALSE
    )
  }

  tibble::tibble(
    source_file = basename(
      path
    ),
    local_path = normalizePath(
      path,
      winslash = "/",
      mustWork = FALSE
    ),
    openai_file_id = file_id,
    bytes = as.numeric(
      file_size
    ),
    uploaded_at = now_text()
  )
}


upload_source_files_to_openai <- function(
    files,
    api_key,
    state
) {

  uploaded <- lapply(
    files,
    function(f) {
      upload_file_to_openai(
        path = f,
        api_key = api_key,
        state = state
      )
    }
  )

  manifest <- dplyr::bind_rows(
    uploaded
  )

  upload_manifest_file <- file.path(
    state$result_directory,
    "openai_file_upload_manifest.csv"
  )

  readr::write_csv(
    manifest,
    upload_manifest_file,
    na = ""
  )

  manifest
}


run_multimodal_extraction <- function(
    state,
    files
) {

  require_api_packages()

  api_key <- safe_chr(
    Sys.getenv(
      "OPENAI_API_KEY",
      unset = ""
    )
  )

  if (api_key == "") {
    stop(
      "OPENAI_API_KEY is required for PDF or non-canonical file extraction.",
      call. = FALSE
    )
  }

  model <- resolve_additional_analysis_model()

  prompt <- build_extraction_prompt(
    state,
    files
  )

  # ----------------------------------------------------------
  # Step A. Upload local files and obtain OpenAI file_id values.
  # ----------------------------------------------------------

  upload_manifest <- upload_source_files_to_openai(
    files = files,
    api_key = api_key,
    state = state
  )

  # ----------------------------------------------------------
  # Step B. Build a compact Responses request using file_id only.
  # GPT-5.6 automatically processes PDF text + page images.
  # ----------------------------------------------------------

  content_items <- list(
    list(
      type = "input_text",
      text = prompt
    )
  )

  for (
    i in seq_len(
      nrow(
        upload_manifest
      )
    )
  ) {

    content_items <- c(
      content_items,
      list(
        list(
          type = "input_file",
          file_id = safe_chr(
            upload_manifest$openai_file_id[[i]]
          )
        )
      )
    )
  }

  schema <- build_extraction_schema()

  request_body <- list(
    model = model,
    input = list(
      list(
        role = "user",
        content = content_items
      )
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "srma_additional_analysis_extraction",
        description = paste0(
          "Structured study-level extraction for an ",
          "author-directed additional meta-analysis."
        ),
        schema = schema,
        strict = TRUE
      )
    )
  )

  request_payload_file <- file.path(
    state$result_directory,
    "responses_request_payload_without_file_contents.json"
  )

  jsonlite::write_json(
    request_body,
    request_payload_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  req <- httr2::request(
    "https://api.openai.com/v1/responses"
  ) |>
    httr2::req_headers(
      Authorization = paste(
        "Bearer",
        api_key
      ),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(
      request_body,
      auto_unbox = TRUE,
      null = "null"
    ) |>
    httr2::req_timeout(
      seconds = 600
    )

  resp <- perform_openai_request(
    req = req,
    state = state,
    diagnostic_name = "responses_structured_extraction"
  )

  body <- httr2::resp_body_json(
    resp,
    simplifyVector = FALSE
  )

  raw_response_file <- file.path(
    state$result_directory,
    "multimodal_extraction_response.json"
  )

  jsonlite::write_json(
    body,
    raw_response_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  output_text <- extract_response_output_text(
    body
  )

  if (output_text == "") {

    response_status <- safe_chr(
      body$status
    )

    incomplete_reason <- ""

    if (
      !is.null(
        body$incomplete_details
      ) &&
        !is.null(
          body$incomplete_details$reason
        )
    ) {
      incomplete_reason <- safe_chr(
        body$incomplete_details$reason
      )
    }

    stop(
      paste0(
        "Responses API returned no structured output text.",
        if (response_status != "") {
          paste0(
            " Response status: ",
            response_status,
            "."
          )
        } else {
          ""
        },
        if (incomplete_reason != "") {
          paste0(
            " Incomplete reason: ",
            incomplete_reason,
            "."
          )
        } else {
          ""
        },
        " Inspect: ",
        raw_response_file
      ),
      call. = FALSE
    )
  }

  parsed <- tryCatch(
    jsonlite::fromJSON(
      output_text,
      simplifyDataFrame = TRUE
    ),
    error = function(e) {
      stop(
        "Structured output could not be parsed as JSON: ",
        conditionMessage(e),
        "
Raw model output was:
",
        output_text,
        call. = FALSE
      )
    }
  )

  if (
    is.null(
      parsed$records
    ) ||
      !is.data.frame(
        parsed$records
      ) ||
      nrow(
        parsed$records
      ) == 0L
  ) {

    uncertainty_text <- ""

    if (
      !is.null(
        parsed$exclusions_or_uncertainties
      )
    ) {
      uncertainty_text <- paste(
        unlist(
          parsed$exclusions_or_uncertainties
        ),
        collapse = " | "
      )
    }

    stop(
      paste0(
        "No poolable records were extracted.",
        if (uncertainty_text != "") {
          paste0(
            " Extraction notes: ",
            uncertainty_text
          )
        } else {
          ""
        }
      ),
      call. = FALSE
    )
  }

  records <- tibble::as_tibble(
    parsed$records
  )

  for (nm in canonical_columns) {

    if (
      !nm %in% names(
        records
      )
    ) {
      records[[nm]] <- NA_character_
    }
  }

  records <- records[
    ,
    canonical_columns,
    drop = FALSE
  ]

  records[] <- lapply(
    records,
    clean_text
  )

  uncertainties <- character(0)

  if (
    !is.null(
      parsed$exclusions_or_uncertainties
    )
  ) {
    uncertainties <- as.character(
      unlist(
        parsed$exclusions_or_uncertainties
      )
    )
  }

  list(
    records = records,
    model = model,
    interpreted_purpose = safe_chr(
      parsed$analysis_purpose_interpreted
    ),
    interpreted_role = safe_chr(
      parsed$evidence_role_interpreted
    ),
    uncertainties = uncertainties,
    openai_upload_manifest = upload_manifest
  )
}


# ============================================================
# 10. Extract / import source data
# ============================================================

prepare_source_records <- function(
    state,
    state_file,
    files
) {

  direct <- try_direct_structured_import(
    files
  )

  if (!is.null(direct)) {

    records <- direct
    extraction_method <- "direct_structured_import"
    model_used <- NA_character_
    uncertainties <- character(0)

  } else {

    cat(
      "\nSource files require structured multimodal/unstructured extraction.\n"
    )

    extraction <- run_multimodal_extraction(
      state,
      files
    )

    records <- extraction$records
    extraction_method <- "multimodal_structured_extraction"
    model_used <- extraction$model
    uncertainties <- extraction$uncertainties
  }

  if (nrow(records) == 0L) {
    stop(
      "No study-level records were available after source processing.",
      call. = FALSE
    )
  }

  if (
    all(
      is.na(
        records$study_id
      )
    )
  ) {

    records$study_id <- ifelse(
      !is.na(
        records$study_label
      ),
      records$study_label,
      paste0(
        "ADD_",
        seq_len(
          nrow(
            records
          )
        )
      )
    )
  }

  if (
    all(
      is.na(
        records$study_label
      )
    )
  ) {
    records$study_label <- records$study_id
  }

  records$record_id <- paste0(
    state$analysis_id,
    "_ROW_",
    sprintf(
      "%03d",
      seq_len(
        nrow(
          records
        )
      )
    )
  )

  extracted_csv <- file.path(
    state$processed_directory,
    "additional_analysis_extracted_records.csv"
  )

  readr::write_csv(
    records,
    extracted_csv,
    na = ""
  )

  provenance_note <- c(
    "R/29 automated additional-analysis provenance",
    paste0("Analysis ID: ", state$analysis_id),
    paste0("Purpose: ", state$purpose),
    paste0("Evidence role: ", state$evidence_role),
    paste0("Extraction method: ", extraction_method),
    paste0("Extraction model: ", ifelse(is.na(model_used), "", model_used)),
    paste0("Created: ", now_text()),
    "",
    "Policy:",
    "After the file-attachment gate, extracted records proceed automatically",
    "to validation, harmonization, pooling, and reporting.",
    "No second Author approval/OK gate is required.",
    "Source files, source locations, extraction notes, and uncertainty records",
    "remain preserved for auditability."
  )

  writeLines(
    provenance_note,
    file.path(
      state$result_directory,
      "automated_analysis_provenance.txt"
    )
  )

  if (length(uncertainties) > 0L) {

    writeLines(
      as.character(
        uncertainties
      ),
      file.path(
        state$result_directory,
        "extraction_uncertainties.txt"
      )
    )
  }

  state <- set_state_status(
    state,
    "EXTRACTED_READY_FOR_ANALYSIS",
    state_file,
    extra = list(
      extraction_method = extraction_method,
      extraction_model = model_used,
      extracted_records_file = extracted_csv
    )
  )

  list(
    state = state,
    records = records,
    uncertainties = uncertainties
  )
}


# ============================================================
# 11. Extraction preview / provenance (NON-BLOCKING)
# ============================================================

print_extraction_preview <- function(
    state,
    records
) {

  preview <- records |>
    dplyr::select(
      dplyr::any_of(
        c(
          "study_id",
          "study_label",
          "study_design",
          "outcome_name",
          "outcome_definition",
          "timepoint",
          "data_type",
          "effect_measure",
          "effect_estimate",
          "lower_ci",
          "upper_ci",
          "intervention_events",
          "intervention_total",
          "comparator_events",
          "comparator_total",
          "adjusted",
          "source_file",
          "source_location",
          "certainty"
        )
      )
    )

  cat("
")
  cat("============================================================
")
  cat("R/29 — Extracted Data Preview (automatic continuation)
")
  cat("============================================================
")
  cat(
    "Purpose: ",
    state$purpose,
    "
",
    sep = ""
  )
  cat(
    "Evidence role: ",
    state$evidence_role,
    "

",
    sep = ""
  )

  print(
    preview,
    width = Inf
  )

  cat("
")
  cat(
    "No additional automatic continuation is required. ",
    "R/29 will now validate, harmonize, and pool these records automatically.

",
    sep = ""
  )
}


# ============================================================
# 12. Resolve effect measure + validate extracted rows
# ============================================================

resolve_analysis_measure <- function(
    data,
    preferred_measure
) {

  preferred_measure <- toupper(
    safe_chr(
      preferred_measure
    )
  )

  row_measures <- normalize_effect_measure(
    data$effect_measure
  )

  unique_row_measures <- unique(
    row_measures[
      !is.na(
        row_measures
      )
    ]
  )

  if (
    preferred_measure != "AUTO"
  ) {

    if (
      !preferred_measure %in% supported_effect_measures
    ) {
      stop(
        "Unsupported preferred effect measure: ",
        preferred_measure,
        call. = FALSE
      )
    }

    if (
      length(
        unique_row_measures
      ) > 0L &&
        any(
          unique_row_measures != preferred_measure
        )
    ) {
      stop(
        "Author-selected effect measure ",
        preferred_measure,
        " conflicts with source-reported measure(s): ",
        paste(
          unique_row_measures,
          collapse = ", "
        ),
        ". R/29 will not silently convert effect measures.",
        call. = FALSE
      )
    }

    return(
      preferred_measure
    )
  }

  if (
    length(
      unique_row_measures
    ) > 1L
  ) {
    stop(
      "Mixed supported effect measures were found: ",
      paste(
        unique_row_measures,
        collapse = ", "
      ),
      ". Select a harmonized measure or provide corrected data.",
      call. = FALSE
    )
  }

  if (
    length(
      unique_row_measures
    ) == 1L
  ) {
    return(
      unique_row_measures[[1]]
    )
  }

  # If no explicit effect measure exists and all rows contain
  # dichotomous raw counts, RR is the transparent default.
  have_raw_counts <- (
    is.finite(
      safe_numeric(
        data$intervention_events
      )
    ) &
      is.finite(
        safe_numeric(
          data$intervention_total
        )
      ) &
      is.finite(
        safe_numeric(
          data$comparator_events
        )
      ) &
      is.finite(
        safe_numeric(
          data$comparator_total
        )
      )
  )

  if (
    all(
      have_raw_counts
    )
  ) {
    return("RR")
  }

  stop(
    "No single effect measure could be resolved. ",
    "Specify RR/OR/HR/RD/MD/SMD explicitly.",
    call. = FALSE
  )
}

validate_and_classify_rows <- function(
    data,
    measure,
    evidence_role
) {

  result <- data
  result$meta_input_type <- NA_character_
  result$validation_status <- "PASS"
  result$validation_note <- NA_character_

  for (
    i in seq_len(
      nrow(
        result
      )
    )
  ) {

    est <- safe_numeric(
      result$effect_estimate[[i]]
    )

    lower <- safe_numeric(
      result$lower_ci[[i]]
    )

    upper <- safe_numeric(
      result$upper_ci[[i]]
    )

    ai <- safe_numeric(
      result$intervention_events[[i]]
    )

    n1i <- safe_numeric(
      result$intervention_total[[i]]
    )

    ci <- safe_numeric(
      result$comparator_events[[i]]
    )

    n2i <- safe_numeric(
      result$comparator_total[[i]]
    )

    m1i <- safe_numeric(
      result$intervention_mean[[i]]
    )

    sd1i <- safe_numeric(
      result$intervention_sd[[i]]
    )

    cn1i <- safe_numeric(
      result$intervention_n[[i]]
    )

    m2i <- safe_numeric(
      result$comparator_mean[[i]]
    )

    sd2i <- safe_numeric(
      result$comparator_sd[[i]]
    )

    cn2i <- safe_numeric(
      result$comparator_n[[i]]
    )

    reported_ok <- (
      length(est) == 1L &&
        length(lower) == 1L &&
        length(upper) == 1L &&
        is.finite(est) &&
        is.finite(lower) &&
        is.finite(upper) &&
        upper > lower
    )

    if (
      measure %in% c(
        "RR",
        "OR",
        "HR"
      ) &&
        reported_ok
    ) {
      reported_ok <- (
        est > 0 &&
          lower > 0 &&
          upper > 0
      )
    }

    raw_binary_ok <- (
      length(ai) == 1L &&
        length(n1i) == 1L &&
        length(ci) == 1L &&
        length(n2i) == 1L &&
        is.finite(ai) &&
        is.finite(n1i) &&
        is.finite(ci) &&
        is.finite(n2i) &&
        ai >= 0 &&
        ci >= 0 &&
        n1i > 0 &&
        n2i > 0 &&
        ai <= n1i &&
        ci <= n2i
    )

    raw_continuous_ok <- (
      length(m1i) == 1L &&
        length(sd1i) == 1L &&
        length(cn1i) == 1L &&
        length(m2i) == 1L &&
        length(sd2i) == 1L &&
        length(cn2i) == 1L &&
        is.finite(m1i) &&
        is.finite(sd1i) &&
        is.finite(cn1i) &&
        is.finite(m2i) &&
        is.finite(sd2i) &&
        is.finite(cn2i) &&
        sd1i >= 0 &&
        sd2i >= 0 &&
        cn1i > 0 &&
        cn2i > 0
    )

    observational <- identical(
      safe_chr(
        evidence_role
      ),
      "observational"
    )

    adjusted_i <- tolower(
      safe_chr(
        result$adjusted[[i]]
      )
    )

    # For observational analyses, prefer reported adjusted/causal
    # estimates whenever present. Raw counts remain possible only
    # after explicit automatic continuation; a warning is preserved.
    if (
      observational &&
        reported_ok
    ) {
      result$meta_input_type[[i]] <- "reported_effect"

      if (
        !adjusted_i %in% c(
          "yes",
          "y",
          "true",
          "adjusted",
          "causal",
          "weighted"
        )
      ) {
        result$validation_note[[i]] <- paste0(
          "Observational reported effect is not clearly marked adjusted/causal."
        )
      }

      next
    }

    if (
      reported_ok
    ) {
      result$meta_input_type[[i]] <- "reported_effect"
      next
    }

    if (
      raw_binary_ok &&
        measure %in% c(
          "RR",
          "OR",
          "RD"
        )
    ) {
      result$meta_input_type[[i]] <- "raw_dichotomous"

      if (observational) {
        result$validation_note[[i]] <- paste0(
          "Observational row uses crude raw counts; interpret as unadjusted."
        )
      }

      next
    }

    if (
      raw_continuous_ok &&
        measure %in% c(
          "MD",
          "SMD"
        )
    ) {
      result$meta_input_type[[i]] <- "raw_continuous"
      next
    }

    result$validation_status[[i]] <- "FAIL"
    result$validation_note[[i]] <- paste0(
      "No valid meta-analysis input could be constructed for ",
      safe_chr(
        result$study_id[[i]]
      )
    )
  }

  bad <- result |>
    dplyr::filter(
      validation_status != "PASS"
    )

  if (nrow(bad) > 0L) {
    stop(
      "R/29 validation failed for ",
      nrow(bad),
      " row(s):\n- ",
      paste(
        bad$validation_note,
        collapse = "\n- "
      ),
      call. = FALSE
    )
  }

  # Protect against dependent duplicate contributions.
  duplicate_studies <- result |>
    dplyr::count(
      study_id,
      name = "n_rows"
    ) |>
    dplyr::filter(
      n_rows > 1L
    )

  if (
    nrow(
      duplicate_studies
    ) > 0L
  ) {
    stop(
      "More than one contribution from the same study was selected: ",
      paste(
        duplicate_studies$study_id,
        collapse = ", "
      ),
      ". R/29 requires one independent contribution per study unless ",
      "a dependent-effects method is explicitly implemented.",
      call. = FALSE
    )
  }

  result
}


# ============================================================
# 13. Effect-size preparation
# ============================================================

prepare_reported_effect <- function(
    row,
    measure
) {

  est <- safe_numeric(
    row$effect_estimate
  )

  lower <- safe_numeric(
    row$lower_ci
  )

  upper <- safe_numeric(
    row$upper_ci
  )

  if (
    is_ratio_measure(
      measure
    )
  ) {

    yi <- log(
      est
    )

    sei <- (
      log(
        upper
      ) -
        log(
          lower
        )
    ) /
      (
        2 *
          stats::qnorm(
            0.975
          )
      )

  } else {

    yi <- est

    sei <- (
      upper -
        lower
    ) /
      (
        2 *
          stats::qnorm(
            0.975
          )
      )
  }

  tibble::tibble(
    yi = yi,
    vi = sei^2,
    sei = sei,
    effect_input_source = "reported_effect"
  )
}

prepare_raw_dichotomous <- function(
    row,
    measure
) {

  esc_measure <- switch(
    measure,
    RR = "RR",
    OR = "OR",
    RD = "RD",
    NA_character_
  )

  if (
    is.na(
      esc_measure
    )
  ) {
    stop(
      "Raw dichotomous data cannot be used directly for ",
      measure,
      ".",
      call. = FALSE
    )
  }

  esc <- metafor::escalc(
    measure = esc_measure,
    ai = safe_numeric(
      row$intervention_events
    ),
    n1i = safe_numeric(
      row$intervention_total
    ),
    ci = safe_numeric(
      row$comparator_events
    ),
    n2i = safe_numeric(
      row$comparator_total
    ),
    add = 0.5,
    to = "only0",
    drop00 = FALSE
  )

  tibble::tibble(
    yi = as.numeric(
      esc$yi
    ),
    vi = as.numeric(
      esc$vi
    ),
    sei = sqrt(
      as.numeric(
        esc$vi
      )
    ),
    effect_input_source = "raw_dichotomous"
  )
}

prepare_raw_continuous <- function(
    row,
    measure
) {

  esc_measure <- switch(
    measure,
    MD = "MD",
    SMD = "SMD",
    NA_character_
  )

  if (
    is.na(
      esc_measure
    )
  ) {
    stop(
      "Raw continuous data require MD or SMD.",
      call. = FALSE
    )
  }

  esc <- metafor::escalc(
    measure = esc_measure,
    m1i = safe_numeric(
      row$intervention_mean
    ),
    sd1i = safe_numeric(
      row$intervention_sd
    ),
    n1i = safe_numeric(
      row$intervention_n
    ),
    m2i = safe_numeric(
      row$comparator_mean
    ),
    sd2i = safe_numeric(
      row$comparator_sd
    ),
    n2i = safe_numeric(
      row$comparator_n
    )
  )

  tibble::tibble(
    yi = as.numeric(
      esc$yi
    ),
    vi = as.numeric(
      esc$vi
    ),
    sei = sqrt(
      as.numeric(
        esc$vi
      )
    ),
    effect_input_source = "raw_continuous"
  )
}

prepare_effects <- function(
    data,
    measure
) {

  rows <- lapply(
    seq_len(
      nrow(
        data
      )
    ),
    function(i) {

      one <- data[
        i,
        ,
        drop = FALSE
      ]

      type_i <- safe_chr(
        one$meta_input_type
      )

      switch(
        type_i,
        reported_effect = prepare_reported_effect(
          one,
          measure
        ),
        raw_dichotomous = prepare_raw_dichotomous(
          one,
          measure
        ),
        raw_continuous = prepare_raw_continuous(
          one,
          measure
        ),
        stop(
          "Unsupported meta_input_type: ",
          type_i,
          call. = FALSE
        )
      )
    }
  )

  dplyr::bind_rows(
    rows
  )
}


# ============================================================
# 14. Run pooled analysis
# ============================================================

run_pooled_analysis <- function(
    state,
    state_file,
    data
) {

  if (nrow(data) < 2L) {
    stop(
      "At least 2 independent studies are required for pooled analysis.",
      call. = FALSE
    )
  }

  measure <- resolve_analysis_measure(
    data,
    state$preferred_effect_measure
  )

  data$effect_measure <- ifelse(
    is.na(
      normalize_effect_measure(
        data$effect_measure
      )
    ),
    measure,
    normalize_effect_measure(
      data$effect_measure
    )
  )

  validated <- validate_and_classify_rows(
    data,
    measure,
    state$evidence_role
  )

  effects <- prepare_effects(
    validated,
    measure
  )

  analysis_data <- dplyr::bind_cols(
    validated,
    effects
  )

  if (
    any(
      !is.finite(
        analysis_data$yi
      ) |
        !is.finite(
          analysis_data$vi
        ) |
        analysis_data$vi <= 0
    )
  ) {
    stop(
      "Invalid yi/vi values were produced.",
      call. = FALSE
    )
  }

  labels <- clean_text(
    analysis_data$study_label
  )

  labels[
    is.na(
      labels
    )
  ] <- clean_text(
    analysis_data$study_id[
      is.na(
        labels
      )
    ]
  )

  labels[
    is.na(
      labels
    )
  ] <- paste0(
    "Study ",
    which(
      is.na(
        labels
      )
    )
  )

  labels <- make.unique(
    labels
  )

  fitted_model <- metafor::rma.uni(
    yi = analysis_data$yi,
    vi = analysis_data$vi,
    method = "REML",
    test = "knha",
    slab = labels
  )

  model_k <- as.integer(
    fitted_model$k
  )

  pooled_effect <- inverse_transform(
    as.numeric(
      fitted_model$b[
        1
      ]
    ),
    measure
  )

  lower_ci <- inverse_transform(
    as.numeric(
      fitted_model$ci.lb[
        1
      ]
    ),
    measure
  )

  upper_ci <- inverse_transform(
    as.numeric(
      fitted_model$ci.ub[
        1
      ]
    ),
    measure
  )

  model_p_value <- as.numeric(
    fitted_model$pval[
      1
    ]
  )

  model_tau2 <- as.numeric(
    fitted_model$tau2
  )

  model_i2 <- as.numeric(
    fitted_model$I2
  )

  model_q <- as.numeric(
    fitted_model$QE
  )

  model_q_p <- as.numeric(
    fitted_model$QEp
  )

  pred <- tryCatch(
    predict(
      fitted_model
    ),
    error = function(e) {
      NULL
    }
  )

  prediction_lower <- NA_real_
  prediction_upper <- NA_real_

  if (
    !is.null(
      pred
    ) &&
      !is.null(
        pred$pi.lb
      ) &&
      !is.null(
        pred$pi.ub
      )
  ) {

    prediction_lower <- inverse_transform(
      as.numeric(
        pred$pi.lb[
          1
        ]
      ),
      measure
    )

    prediction_upper <- inverse_transform(
      as.numeric(
        pred$pi.ub[
          1
        ]
      ),
      measure
    )
  }

  analysis_data$study_effect <- inverse_transform(
    analysis_data$yi,
    measure
  )

  analysis_data$study_lower_ci <- inverse_transform(
    analysis_data$yi -
      stats::qnorm(
        0.975
      ) *
        analysis_data$sei,
    measure
  )

  analysis_data$study_upper_ci <- inverse_transform(
    analysis_data$yi +
      stats::qnorm(
        0.975
      ) *
        analysis_data$sei,
    measure
  )

  analysis_data$random_effect_weight_percent <- as.numeric(
    stats::weights(
      fitted_model
    )
  )

  ready_file <- file.path(
    state$processed_directory,
    "additional_analysis_meta_analysis_ready.csv"
  )

  readr::write_csv(
    analysis_data,
    ready_file,
    na = ""
  )

  forest_file <- file.path(
    state$result_directory,
    "forest_plot.pdf"
  )

  grDevices::pdf(
    forest_file,
    width = 8.5,
    height = max(
      6,
      2.8 +
        0.45 *
          nrow(
            analysis_data
          )
    )
  )

  if (
    is_ratio_measure(
      measure
    )
  ) {

    metafor::forest(
      fitted_model,
      atransf = exp,
      xlab = measure,
      slab = labels,
      header = c(
        "Study",
        measure
      ),
      addpred = (
        model_k >= 5L
      )
    )

  } else {

    metafor::forest(
      fitted_model,
      xlab = measure,
      slab = labels,
      header = c(
        "Study",
        measure
      ),
      addpred = (
        model_k >= 5L
      )
    )
  }

  graphics::title(
    main = safe_chr(
      state$purpose
    )
  )

  grDevices::dev.off()

  model_output_file <- file.path(
    state$result_directory,
    "model_output.txt"
  )

  capture.output(
    {
      cat(
        "Universal SR-MA Agent Toolkit\n"
      )
      cat(
        "R/29 Author-Directed Additional Analysis\n\n"
      )
      cat(
        "Analysis ID: ",
        state$analysis_id,
        "\n",
        sep = ""
      )
      cat(
        "Purpose: ",
        state$purpose,
        "\n",
        sep = ""
      )
      cat(
        "Evidence role: ",
        state$evidence_role,
        "\n",
        sep = ""
      )
      cat(
        "Effect measure: ",
        measure,
        "\n"
      )
      cat(
        "Model: Random-effects, REML, Knapp-Hartung\n\n"
      )

      print(
        fitted_model
      )

      if (!is.null(pred)) {
        cat(
          "\nPrediction:\n"
        )
        print(
          pred
        )
      }
    },
    file = model_output_file
  )

  summary <- tibble::tibble(
    analysis_id = state$analysis_id,
    purpose = state$purpose,
    evidence_role = state$evidence_role,
    status = "PASS",
    studies = model_k,
    effect_measure = measure,
    model_name = "Random-effects",
    tau2_method = "REML",
    inference_test = "Knapp-Hartung",
    pooled_effect = pooled_effect,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    p_value = model_p_value,
    tau2 = model_tau2,
    i2 = model_i2,
    q = model_q,
    q_df = model_k - 1L,
    q_p_value = model_q_p,
    prediction_lower = ifelse(
      model_k >= 5L,
      prediction_lower,
      NA_real_
    ),
    prediction_upper = ifelse(
      model_k >= 5L,
      prediction_upper,
      NA_real_
    ),
    prediction_interval_note = ifelse(
      model_k >= 5L,
      "Prediction interval reported with usual assumptions/caution.",
      "Prediction interval not emphasized because fewer than 5 studies."
    ),
    extraction_method = safe_chr(
      state$extraction_method
    ),
    source_count = length(
      unlist(
        state$source_files
      )
    ),
    forest_plot = forest_file
  )

  summary_file <- file.path(
    state$result_directory,
    "meta_analysis_summary.csv"
  )

  readr::write_csv(
    summary,
    summary_file,
    na = ""
  )

  # Neutral manuscript-ready result sentence.
  result_sentence <- paste0(
    "In an author-directed additional analysis of ",
    model_k,
    " studies, the pooled ",
    measure,
    " was ",
    format_num(
      pooled_effect,
      2
    ),
    " (95% CI, ",
    format_num(
      lower_ci,
      2
    ),
    "\u2013",
    format_num(
      upper_ci,
      2
    ),
    "; I\u00B2=",
    format_num(
      model_i2,
      1
    ),
    "%), using a random-effects model with REML estimation and Knapp\u2013Hartung inference."
  )

  manuscript_file <- file.path(
    state$result_directory,
    "manuscript_result_sentence.txt"
  )

  writeLines(
    result_sentence,
    manuscript_file
  )

  report_file <- file.path(
    state$result_directory,
    "additional_analysis_report.txt"
  )

  report_lines <- c(
    "Universal SR-MA Agent Toolkit",
    "R/29 — Author-Directed Additional Analysis",
    "",
    paste0(
      "Analysis ID: ",
      state$analysis_id
    ),
    paste0(
      "Project: ",
      project_title
    ),
    paste0(
      "Evidence role: ",
      state$evidence_role
    ),
    paste0(
      "Purpose: ",
      state$purpose
    ),
    paste0(
      "Source files: ",
      paste(
        basename(
          unlist(
            state$source_files
          )
        ),
        collapse = "; "
      )
    ),
    paste0(
      "Extraction method: ",
      state$extraction_method
    ),
    "",
    "POOLED RESULT",
    "--------------------------------------------------",
    paste0(
      "Studies: ",
      model_k
    ),
    paste0(
      "Effect measure: ",
      measure
    ),
    paste0(
      "Pooled effect: ",
      pooled_effect
    ),
    paste0(
      "95% CI: ",
      lower_ci,
      " to ",
      upper_ci
    ),
    paste0(
      "P value: ",
      model_p_value
    ),
    paste0(
      "Tau-squared: ",
      model_tau2
    ),
    paste0(
      "I-squared: ",
      model_i2,
      "%"
    ),
    paste0(
      "Q: ",
      model_q,
      "; p=",
      model_q_p
    ),
    "",
    "MODEL",
    "--------------------------------------------------",
    "Random-effects",
    "Tau-squared estimator: REML",
    "Inference: Knapp-Hartung",
    "",
    "MANUSCRIPT RESULT SENTENCE",
    "--------------------------------------------------",
    result_sentence,
    "",
    "GOVERNANCE",
    "--------------------------------------------------",
    "This result is an Author-Directed Additional Analysis.",
    "It does not replace or overwrite the canonical R/27 primary analyses.",
    "Source provenance and the exact extracted analysis input are retained in this analysis workspace.",
    "",
    "STATUS",
    "--------------------------------------------------",
    "R/29: PASS"
  )

  writeLines(
    report_lines,
    report_file
  )

  state <- set_state_status(
    state,
    "COMPLETE",
    state_file,
    extra = list(
      final_effect_measure = measure,
      meta_analysis_ready_file = ready_file,
      summary_file = summary_file,
      forest_plot = forest_file,
      report_file = report_file,
      manuscript_result_sentence_file = manuscript_file
    )
  )

  clear_active_analysis()

  list(
    state = state,
    summary = summary,
    analysis_data = analysis_data,
    result_sentence = result_sentence
  )
}


# ============================================================
# 15. Main state machine
# ============================================================

run_r29 <- function() {

  cat("\n")
  cat("============================================================\n")
  cat("R/29 — Author-Directed Additional Analysis\n")
  cat("============================================================\n\n")

  active <- get_active_state()

  if (is.null(active)) {

    created <- create_new_analysis()

    if (
      isTRUE(
        created$terminal
      )
    ) {
      return(
        invisible(
          created$state
        )
      )
    }

    state <- created$state
    state_file <- created$state_file

  } else {

    state <- active$state
    state_file <- active$state_file

    cat(
      "Resuming active analysis automatically:\n"
    )
    cat(
      "  Analysis ID: ",
      state$analysis_id,
      "\n",
      sep = ""
    )
    cat(
      "  Status: ",
      state$status,
      "\n",
      sep = ""
    )
    cat(
      "  Purpose: ",
      state$purpose,
      "\n\n",
      sep = ""
    )
  }

  # ----------------------------------------------------------
  # FILE GATE
  # ----------------------------------------------------------

  if (
    safe_chr(
      state$status
    ) %in% c(
      "WAITING_FOR_FILES",
      "FILES_AVAILABLE"
    )
  ) {

    gate <- wait_for_files(
      state,
      state_file
    )

    state <- gate$state

    if (
      !isTRUE(
        gate$ready
      )
    ) {
      return(
        invisible(
          list(
            status = state$status,
            state = state
          )
        )
      )
    }

    files <- gate$files

    extracted <- prepare_source_records(
      state,
      state_file,
      files
    )

    state <- extracted$state
    records <- extracted$records
    uncertainties <- extracted$uncertainties

  } else {

    records <- NULL
    uncertainties <- character(0)
  }

  # ----------------------------------------------------------
  # AUTOMATIC EXTRACTION -> ANALYSIS CONTINUATION
  # ----------------------------------------------------------

  if (
    safe_chr(
      state$status
    ) %in% c(
      "EXTRACTED_READY_FOR_ANALYSIS",
      "ANALYZING",
      "ANALYSIS_FAILED"
    )
  ) {

    if (is.null(records)) {

      extracted_file <- safe_chr(
        state$extracted_records_file
      )

      if (
        extracted_file == "" ||
          !file.exists(
            extracted_file
          )
      ) {
        stop(
          "Cannot continue R/29: extracted records file is missing.",
          call. = FALSE
        )
      }

      records <- readr::read_csv(
        extracted_file,
        show_col_types = FALSE
      )
    }

    if (
      nrow(
        records
      ) == 0L
    ) {
      stop(
        "No extracted rows are available for additional analysis.",
        call. = FALSE
      )
    }

    print_extraction_preview(
      state,
      records
    )

    included <- records

  } else if (
    safe_chr(
      state$status
    ) == "COMPLETE"
  ) {

    cat(
      "R/29 analysis is already COMPLETE.
"
    )

    return(
      invisible(
        list(
          status = "COMPLETE",
          state = state
        )
      )
    )

  } else {

    stop(
      "Unsupported R/29 state: ",
      safe_chr(
        state$status
      ),
      call. = FALSE
    )
  }

  # ----------------------------------------------------------
  # ANALYSIS
  # ----------------------------------------------------------

  state <- set_state_status(
    state,
    "ANALYZING",
    state_file
  )

  result <- tryCatch(
    run_pooled_analysis(
      state,
      state_file,
      included
    ),
    error = function(e) {

      state_failed <- set_state_status(
        state,
        "ANALYSIS_FAILED",
        state_file,
        extra = list(
          error_message = conditionMessage(
            e
          )
        )
      )

      stop(
        conditionMessage(
          e
        ),
        call. = FALSE
      )
    }
  )

  summary <- result$summary[
    1,
    ,
    drop = FALSE
  ]

  cat("\n")
  cat("============================================================\n")
  cat("R/29 — Additional Analysis Result\n")
  cat("============================================================\n\n")

  cat(
    "Purpose: ",
    summary$purpose,
    "\n",
    sep = ""
  )

  cat(
    "Evidence role: ",
    summary$evidence_role,
    "\n",
    sep = ""
  )

  cat(
    "Studies: ",
    summary$studies,
    "\n",
    sep = ""
  )

  cat(
    "Model: Random-effects / REML / Knapp-Hartung\n\n"
  )

  cat(
    summary$effect_measure,
    ": ",
    format_num(
      summary$pooled_effect,
      3
    ),
    " [",
    format_num(
      summary$lower_ci,
      3
    ),
    ", ",
    format_num(
      summary$upper_ci,
      3
    ),
    "]\n",
    sep = ""
  )

  cat(
    "I2: ",
    format_num(
      summary$i2,
      1
    ),
    "%\n",
    sep = ""
  )

  cat(
    "Tau2: ",
    format_num(
      summary$tau2,
      3
    ),
    "\n",
    sep = ""
  )

  cat(
    "P value: ",
    format_num(
      summary$p_value,
      4
    ),
    "\n\n",
    sep = ""
  )

  cat(
    "PASS: R/29 Author-Directed Additional Analysis completed.\n"
  )

  cat(
    "Result workspace:\n",
    result$state$result_directory,
    "\n\n",
    sep = ""
  )

  cat(
    "Manuscript-ready neutral result sentence:\n",
    result$result_sentence,
    "\n",
    sep = ""
  )

  invisible(
    result
  )
}


# ============================================================
# 16. Execute
# ============================================================

r29_result <- run_r29()

invisible(
  r29_result
)
