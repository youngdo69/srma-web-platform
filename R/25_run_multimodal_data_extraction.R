# ============================================================
# Universal SR-MA Agent Toolkit
# R/25_run_multimodal_data_extraction.R
#
# TASK-013 — Multimodal Data Extraction
#
# Architecture (DEC-008)
# ------------------------------------------------------------
# Stage 1: Multimodal Agent Data Extraction
# Stage 2: Human Verification (TASK-014)
#
# Purpose
# ------------------------------------------------------------
# 1. Read the authoritative TASK-012 included_studies.csv.
# 2. Read a topic-neutral extraction schema.
# 3. Read project-specific outcomes dynamically from project_config.yml.
# 4. Use full-text PDF as primary evidence when available.
# 5. Use metadata-only fallback only when PDF is unavailable.
# 6. Extract study characteristics, group characteristics, and
#    prespecified outcome data into structured datasets.
# 7. Preserve source location/evidence for audit and human checking.
# 8. Prepare human-verification worksheets without populating
#    human-only verification fields.
#
# Important
# ------------------------------------------------------------
# - Never invent or impute unreported values.
# - Never calculate effect estimates during TASK-013.
# - Blank means not reported / not extractable.
# - Agent output is NOT authoritative for meta-analysis.
# - TASK-014 Human Verification creates the authoritative dataset.
# - Runner integration is out of scope.
# ============================================================


# ============================================================
# 1. Required packages
# ============================================================

required_packages <- c(
  "yaml",
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "jsonlite",
  "httr2",
  "curl",
  "dotenv",
  "openxlsx"
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
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))",
    call. = FALSE
  )
}


# ============================================================
# 2. Helpers
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_squish(x)
  x[is.na(x) | x == ""] <- NA_character_
  x
}

safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return("")
  }
  as.character(x[[1]])
}

now_text <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

as_nonempty_character_vector <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }

  values <- as.character(
    unlist(
      x,
      recursive = TRUE,
      use.names = FALSE
    )
  )

  values <- trimws(values)

  unique(
    values[
      !is.na(values) &
        nzchar(values)
    ]
  )
}

blank_if_na <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

safe_json <- function(x) {

  # jsonlite::toJSON() returns an object with class "json".
  # Checkpoint columns are ordinary character vectors, so the
  # serialized JSON must be explicitly converted to character
  # before dplyr::bind_rows() combines checkpoint rows.
  as.character(
    jsonlite::toJSON(
      x,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA
    )
  )
}


# ============================================================
# 3. Load canonical project configuration and paths
# ============================================================

source("R/utils/config_utils.R")
source("R/utils/path_utils.R")
source("R/utils/llm_provider_utils.R")

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

project_id <- get_project_id(config)
project_title <- get_project_title(config)


# ============================================================
# 4. Validate DEC-008 minimal configuration
# ============================================================

if (
  is.null(config$data_extraction) ||
  !is.list(config$data_extraction)
) {
  stop(
    "Missing configuration section: data_extraction",
    call. = FALSE
  )
}

if (
  !isTRUE(config$data_extraction$agent_extraction) ||
  !isTRUE(config$data_extraction$human_verification)
) {
  stop(
    "TASK-013 requires DEC-008 configuration:\n",
    "data_extraction:\n",
    "  agent_extraction: yes\n",
    "  human_verification: yes",
    call. = FALSE
  )
}


# ============================================================
# 5. Read and validate extraction schema
# ============================================================

schema_file <- "config/data_extraction_schema.yml"

if (!file.exists(schema_file)) {
  stop(
    "Data extraction schema not found: ",
    schema_file,
    call. = FALSE
  )
}

extraction_schema <- yaml::read_yaml(schema_file)

if (
  is.null(extraction_schema$architecture) ||
  !isTRUE(extraction_schema$architecture$agent_extraction) ||
  !isTRUE(extraction_schema$architecture$human_verification)
) {
  stop(
    "data_extraction_schema.yml does not match DEC-008 architecture.",
    call. = FALSE
  )
}

schema_version <- safe_chr(
  extraction_schema$schema_version
)

if (!nzchar(schema_version)) {
  stop(
    "data_extraction_schema.yml is missing schema_version.",
    call. = FALSE
  )
}

allowed_data_types <- as_nonempty_character_vector(
  extraction_schema$outcome_data$allowed_data_types
)

if (length(allowed_data_types) == 0L) {
  stop(
    "No allowed outcome data types were found in data_extraction_schema.yml.",
    call. = FALSE
  )
}

verification_status_options <- as_nonempty_character_vector(
  extraction_schema$human_verification$allowed_status
)

if (length(verification_status_options) == 0L) {
  verification_status_options <- c(
    "Verified",
    "Corrected",
    "Needs clarification"
  )
}


# ============================================================
# 6. Read canonical project outcomes dynamically
# ============================================================

primary_outcomes <- as_nonempty_character_vector(
  config$research_question$outcomes$primary
)

secondary_outcomes <- as_nonempty_character_vector(
  config$research_question$outcomes$secondary
)

project_outcomes <- unique(
  c(
    primary_outcomes,
    secondary_outcomes
  )
)

if (length(project_outcomes) == 0L) {
  stop(
    "No project outcomes were found in research_question.outcomes.",
    call. = FALSE
  )
}

outcome_priority <- tibble::tibble(
  outcome_name = project_outcomes,
  outcome_priority = c(
    rep("Primary", length(primary_outcomes)),
    rep("Secondary", length(secondary_outcomes))
  )
)


# ============================================================
# 7. Provider configuration
# ============================================================
# Reuse the validated provider/model configuration from
# config/agent_screening.yml without modifying Phase 1 files.
# ============================================================

agent_config_file <- "config/agent_screening.yml"

if (!file.exists(agent_config_file)) {
  stop(
    "Agent configuration not found: ",
    agent_config_file,
    call. = FALSE
  )
}

agent_document <- yaml::read_yaml(agent_config_file)
agent_config <- agent_document$agent_screening

if (
  is.null(agent_config) ||
  !is.list(agent_config) ||
  !isTRUE(agent_config$enabled)
) {
  stop(
    "agent_screening configuration is missing, invalid, or disabled.",
    call. = FALSE
  )
}

load_project_env(".env")

provider_config <- agent_config$provider
provider_name <- tolower(
  safe_chr(provider_config$name)
)
model_name <- safe_chr(
  provider_config$model
)

if (!identical(provider_name, "openai")) {
  stop(
    "R/25 multimodal extraction currently supports the existing ",
    "OpenAI provider configuration only.\nCurrent provider: ",
    provider_name,
    call. = FALSE
  )
}

if (!nzchar(model_name)) {
  stop(
    "The configured OpenAI model name is empty.",
    call. = FALSE
  )
}

api_key_env <- provider_config$api_key_env %||% "OPENAI_API_KEY"
api_key <- get_required_environment_value(api_key_env)

base_url <- provider_config$base_url %||% "https://api.openai.com/v1"
base_url <- sub("/+$", "", base_url)

timeout_seconds <- as.numeric(
  agent_config$processing$timeout_seconds %||% 240
)

max_retries <- as.integer(
  agent_config$processing$max_retries %||% 3L
)

checkpoint_every <- as.integer(
  agent_config$processing$checkpoint_every %||% 3L
)

pause_seconds <- as.numeric(
  agent_config$processing$request_pause_seconds %||% 0
)

resume_enabled <- isTRUE(
  agent_config$processing$resume_from_checkpoint
)

prompt_version <- "multimodal_data_extraction_v1"


# ============================================================
# 8. TASK-013 paths
# ============================================================

processed_full_text_directory <- file.path(
  paths$project_root,
  "data",
  "processed",
  "full_text"
)

data_extraction_directory <- file.path(
  paths$project_root,
  "data",
  "processed",
  "extraction"
)

extraction_results_directory <- file.path(
  paths$project_root,
  "results",
  "extraction"
)

included_studies_csv <- file.path(
  processed_full_text_directory,
  "included_studies.csv"
)

study_characteristics_csv <- file.path(
  data_extraction_directory,
  "study_characteristics_agent.csv"
)

group_characteristics_csv <- file.path(
  data_extraction_directory,
  "group_characteristics_agent.csv"
)

outcome_data_csv <- file.path(
  data_extraction_directory,
  "outcome_data_agent.csv"
)

checkpoint_file <- file.path(
  paths$logs_directory,
  "multimodal_data_extraction_checkpoint.csv"
)

failure_file <- file.path(
  paths$logs_directory,
  "multimodal_data_extraction_failures.csv"
)

workbook_file <- file.path(
  extraction_results_directory,
  "multimodal_data_extraction_agent.xlsx"
)

report_file <- file.path(
  paths$logs_directory,
  "multimodal_data_extraction_report.txt"
)

ensure_directories(
  c(
    processed_full_text_directory,
    data_extraction_directory,
    extraction_results_directory,
    paths$logs_directory
  )
)

assert_file_exists(
  included_studies_csv,
  "TASK-012 authoritative Included Studies dataset"
)


# ============================================================
# 9. Read and validate Included Studies
# ============================================================

included_studies <- readr::read_csv(
  included_studies_csv,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
)

required_included_columns <- c(
  "screening_id",
  "title",
  "authors",
  "year",
  "journal",
  "doi",
  "pmid",
  "publication_type",
  "language",
  "url",
  "pdf_status",
  "pdf_path",
  "final_decision"
)

missing_included_columns <- setdiff(
  required_included_columns,
  names(included_studies)
)

if (length(missing_included_columns) > 0L) {
  stop(
    "included_studies.csv is missing required column(s):\n- ",
    paste(
      missing_included_columns,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}

included_studies$screening_id <- clean_text(
  included_studies$screening_id
)

included_studies$pdf_status <- clean_text(
  included_studies$pdf_status
)

included_studies$pdf_path <- clean_text(
  included_studies$pdf_path
)

included_studies$final_decision <- clean_text(
  included_studies$final_decision
)

if (nrow(included_studies) == 0L) {
  stop(
    "included_studies.csv contains zero studies.",
    call. = FALSE
  )
}

if (any(is.na(included_studies$screening_id))) {
  stop(
    "Included Studies contains missing screening_id values.",
    call. = FALSE
  )
}

if (anyDuplicated(included_studies$screening_id) > 0L) {
  stop(
    "Included Studies contains duplicated screening_id values.",
    call. = FALSE
  )
}

if (any(included_studies$final_decision != "Include")) {
  stop(
    "included_studies.csv contains a record whose final_decision is not Include.",
    call. = FALSE
  )
}


# ============================================================
# 10. Determine evidence source and current PDF checksum
# ============================================================

pdf_is_usable <- (
  included_studies$pdf_status == "Available" &
    !is.na(included_studies$pdf_path) &
    file.exists(included_studies$pdf_path)
)

included_studies$agent_evidence_source <- ifelse(
  pdf_is_usable,
  "Full-text PDF",
  "Metadata only"
)

included_studies$current_pdf_checksum_md5 <- NA_character_

if (any(pdf_is_usable)) {
  included_studies$current_pdf_checksum_md5[pdf_is_usable] <-
    unname(
      tools::md5sum(
        included_studies$pdf_path[pdf_is_usable]
      )
    )
}


# ============================================================
# 11. Strict structured-output JSON schema
# ============================================================

string_field <- function() {
  list(type = "string")
}

object_schema_from_fields <- function(fields) {
  list(
    type = "object",
    additionalProperties = FALSE,
    required = unname(fields),
    properties = setNames(
      lapply(fields, function(x) string_field()),
      fields
    )
  )
}

study_fields <- as_nonempty_character_vector(
  extraction_schema$study_characteristics$fields
)

group_fields <- as_nonempty_character_vector(
  extraction_schema$group_characteristics$fields
)

outcome_fields <- as_nonempty_character_vector(
  extraction_schema$outcome_data$fields
)

if (
  length(study_fields) == 0L ||
  length(group_fields) == 0L ||
  length(outcome_fields) == 0L
) {
  stop(
    "Extraction schema contains an empty field group.",
    call. = FALSE
  )
}

study_schema <- object_schema_from_fields(
  study_fields
)

group_schema <- object_schema_from_fields(
  group_fields
)

outcome_properties <- setNames(
  lapply(
    outcome_fields,
    function(x) string_field()
  ),
  outcome_fields
)

outcome_properties$outcome_name <- list(
  type = "string",
  enum = unname(project_outcomes)
)

outcome_properties$data_type <- list(
  type = "string",
  enum = unname(allowed_data_types)
)

outcome_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  required = unname(outcome_fields),
  properties = outcome_properties
)

json_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  required = c(
    "screening_id",
    "extraction_status",
    "evidence_source",
    "study_characteristics",
    "groups",
    "outcomes",
    "overall_extraction_notes"
  ),
  properties = list(
    screening_id = list(type = "string"),
    extraction_status = list(
      type = "string",
      enum = c(
        "Complete",
        "Partial — metadata only",
        "Partial — insufficient source"
      )
    ),
    evidence_source = list(
      type = "string",
      enum = c(
        "Full-text PDF",
        "Metadata only"
      )
    ),
    study_characteristics = study_schema,
    groups = list(
      type = "array",
      items = group_schema
    ),
    outcomes = list(
      type = "array",
      items = outcome_schema
    ),
    overall_extraction_notes = list(
      type = "string"
    )
  )
)


# ============================================================
# 12. Prompt builder
# ============================================================

format_project_outcomes <- function() {
  rows <- c(
    paste0(
      "- Primary: ",
      primary_outcomes
    ),
    paste0(
      "- Secondary: ",
      secondary_outcomes
    )
  )

  paste(
    rows,
    collapse = "\n"
  )
}

format_metadata <- function(record) {
  paste0(
    "screening_id: ", safe_chr(record$screening_id), "\n",
    "title: ", safe_chr(record$title), "\n",
    "authors: ", safe_chr(record$authors), "\n",
    "year: ", safe_chr(record$year), "\n",
    "journal: ", safe_chr(record$journal), "\n",
    "DOI: ", safe_chr(record$doi), "\n",
    "PMID: ", safe_chr(record$pmid), "\n",
    "publication_type: ", safe_chr(record$publication_type), "\n",
    "language: ", safe_chr(record$language), "\n",
    "URL: ", safe_chr(record$url), "\n",
    "PDF status: ", safe_chr(record$pdf_status)
  )
}

build_extraction_prompt <- function(
    record,
    evidence_source
) {
  evidence_rule <- if (
    identical(evidence_source, "Full-text PDF")
  ) {
    paste(
      "A full-text PDF is attached.",
      "Use the complete PDF as the primary source.",
      "Inspect Methods, Results, tables, figures, and supplementary-style",
      "material embedded in the PDF when present."
    )
  } else {
    paste(
      "No usable full-text PDF is attached.",
      "Use bibliographic metadata only.",
      "Do not infer analysis-critical values that are not explicitly available.",
      "Use extraction_status 'Partial — metadata only' unless the source is so",
      "limited that 'Partial — insufficient source' is more accurate."
    )
  }

  paste0(
    "You are a systematic-review data extraction agent.\n\n",

    "PROJECT\n",
    "Project ID: ", project_id, "\n",
    "Project title: ", project_title, "\n\n",

    "PRESPECIFIED OUTCOMES\n",
    format_project_outcomes(),
    "\n\n",

    "STUDY METADATA\n",
    format_metadata(record),
    "\n\n",

    "EVIDENCE MODE\n",
    evidence_rule,
    "\n\n",

    "EXTRACTION RULES\n",
    "1. Extract only values explicitly supported by the source.\n",
    "2. Never invent, impute, convert, or calculate missing values.\n",
    "3. Use an empty string for values that are unreported or not extractable.\n",
    "4. Preserve the source's reported units and timepoints.\n",
    "5. Extract all reported analysis-ready representations relevant to the ",
    "prespecified outcomes. For example, if both raw event counts and an adjusted ",
    "effect are reported, keep them as separate outcome rows when appropriate.\n",
    "6. outcome_name must map to exactly one canonical prespecified outcome listed ",
    "above; preserve the paper's exact definition in outcome_definition.\n",
    "7. For dichotomous outcomes, preferentially extract event counts and group ",
    "denominators when explicitly reported.\n",
    "8. For continuous outcomes, extract mean, SD, and N only when explicitly ",
    "reported. Do not convert median/IQR to mean/SD.\n",
    "9. For time-to-event/reported-effect results, preserve effect measure, estimate, ",
    "CI, whether adjusted, and adjustment variables.\n",
    "10. source_location should identify the source location such as Table 2, ",
    "Figure 3, Results section, or Supplementary Table S1 when identifiable.\n",
    "11. source_evidence must be a concise paraphrase of the supporting evidence, ",
    "not a long quotation.\n",
    "12. Do not perform risk-of-bias assessment in TASK-013.\n",
    "13. Return only JSON conforming to the schema.\n"
  )
}


# ============================================================
# 13. OpenAI PDF adapter
# ============================================================

extract_output_text <- function(response_body) {
  if (!is.list(response_body$output)) {
    stop(
      "OpenAI response did not contain an output array.",
      call. = FALSE
    )
  }

  for (item in response_body$output) {
    if (!is.list(item$content)) {
      next
    }

    for (content_item in item$content) {
      if (
        identical(content_item$type, "output_text") &&
        is.character(content_item$text) &&
        length(content_item$text) == 1L
      ) {
        return(content_item$text)
      }

      if (identical(content_item$type, "refusal")) {
        stop(
          "The model refused the extraction request: ",
          content_item$refusal,
          call. = FALSE
        )
      }
    }
  }

  stop(
    "No output_text item was found in the OpenAI response.",
    call. = FALSE
  )
}

upload_pdf <- function(file_path) {
  response <- httr2::request(
    paste0(base_url, "/files")
  ) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key)
    ) |>
    httr2::req_body_multipart(
      purpose = "user_data",
      file = curl::form_file(
        file_path,
        type = "application/pdf"
      )
    ) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_error(
      is_error = function(resp) FALSE
    ) |>
    httr2::req_perform()

  status <- httr2::resp_status(response)
  response_text <- httr2::resp_body_string(response)

  if (status < 200L || status >= 300L) {
    stop(
      "OpenAI PDF upload failed with HTTP ",
      status,
      ".\n",
      substr(response_text, 1L, 3000L),
      call. = FALSE
    )
  }

  body <- jsonlite::fromJSON(
    response_text,
    simplifyVector = FALSE
  )

  file_id <- safe_chr(body$id)

  if (!nzchar(file_id)) {
    stop(
      "OpenAI PDF upload response did not contain a file id.",
      call. = FALSE
    )
  }

  file_id
}

delete_uploaded_file <- function(file_id) {
  if (!nzchar(file_id)) {
    return(invisible(FALSE))
  }

  try(
    httr2::request(
      paste0(base_url, "/files/", file_id)
    ) |>
      httr2::req_method("DELETE") |>
      httr2::req_headers(
        Authorization = paste("Bearer", api_key)
      ) |>
      httr2::req_timeout(timeout_seconds) |>
      httr2::req_error(
        is_error = function(resp) FALSE
      ) |>
      httr2::req_perform(),
    silent = TRUE
  )

  invisible(TRUE)
}

call_extraction_model <- function(
    prompt,
    pdf_path = NULL
) {
  uploaded_file_id <- ""

  on.exit(
    {
      if (nzchar(uploaded_file_id)) {
        delete_uploaded_file(uploaded_file_id)
      }
    },
    add = TRUE
  )

  content_items <- list(
    list(
      type = "input_text",
      text = prompt
    )
  )

  if (
    !is.null(pdf_path) &&
    length(pdf_path) == 1L &&
    !is.na(pdf_path) &&
    nzchar(pdf_path)
  ) {
    uploaded_file_id <- upload_pdf(pdf_path)

    content_items <- c(
      content_items,
      list(
        list(
          type = "input_file",
          file_id = uploaded_file_id
        )
      )
    )
  }

  request_body <- list(
    model = model_name,
    input = list(
      list(
        role = "user",
        content = content_items
      )
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "srma_multimodal_data_extraction",
        strict = TRUE,
        schema = json_schema
      )
    )
  )

  response <- httr2::request(
    paste0(base_url, "/responses")
  ) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(
      request_body,
      auto_unbox = TRUE
    ) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_error(
      is_error = function(resp) FALSE
    ) |>
    httr2::req_perform()

  status <- httr2::resp_status(response)
  response_text <- httr2::resp_body_string(response)

  if (status < 200L || status >= 300L) {
    stop(
      "OpenAI Responses API request failed with HTTP ",
      status,
      ".\n",
      substr(response_text, 1L, 3000L),
      call. = FALSE
    )
  }

  response_body <- jsonlite::fromJSON(
    response_text,
    simplifyVector = FALSE
  )

  output_text <- extract_output_text(response_body)

  parsed <- jsonlite::fromJSON(
    output_text,
    simplifyVector = FALSE
  )

  list(
    parsed = parsed,
    response_id = safe_chr(response_body$id),
    model = safe_chr(response_body$model),
    raw_output_text = output_text
  )
}


# ============================================================
# 14. Structured-result validation
# ============================================================

validate_result <- function(
    result,
    expected_id,
    expected_evidence
) {
  required_top <- c(
    "screening_id",
    "extraction_status",
    "evidence_source",
    "study_characteristics",
    "groups",
    "outcomes",
    "overall_extraction_notes"
  )

  if (!all(required_top %in% names(result))) {
    return("missing_required_top_fields")
  }

  if (!identical(safe_chr(result$screening_id), expected_id)) {
    return("screening_id_mismatch")
  }

  if (
    !safe_chr(result$extraction_status) %in% c(
      "Complete",
      "Partial — metadata only",
      "Partial — insufficient source"
    )
  ) {
    return("invalid_extraction_status")
  }

  if (!identical(safe_chr(result$evidence_source), expected_evidence)) {
    return("evidence_source_mismatch")
  }

  if (!is.list(result$study_characteristics)) {
    return("invalid_study_characteristics")
  }

  if (!all(study_fields %in% names(result$study_characteristics))) {
    return("missing_study_fields")
  }

  if (!is.list(result$groups)) {
    return("invalid_groups")
  }

  if (!is.list(result$outcomes)) {
    return("invalid_outcomes")
  }

  if (length(result$groups) > 0L) {
    group_valid <- vapply(
      result$groups,
      function(item) {
        is.list(item) &&
          all(group_fields %in% names(item))
      },
      logical(1)
    )

    if (!all(group_valid)) {
      return("invalid_group_record")
    }
  }

  if (length(result$outcomes) > 0L) {
    outcome_valid <- vapply(
      result$outcomes,
      function(item) {
        is.list(item) &&
          all(outcome_fields %in% names(item)) &&
          safe_chr(item$outcome_name) %in% project_outcomes &&
          safe_chr(item$data_type) %in% allowed_data_types
      },
      logical(1)
    )

    if (!all(outcome_valid)) {
      return("invalid_outcome_record")
    }
  }

  "valid"
}


# ============================================================
# 15. Checkpoint
# ============================================================

empty_checkpoint <- function() {
  tibble::tibble(
    screening_id = character(),
    evidence_source = character(),
    pdf_checksum_md5 = character(),
    schema_version = character(),
    prompt_version = character(),
    extraction_status = character(),
    response_id = character(),
    model = character(),
    completed_at = character(),
    validation_status = character(),
    extraction_json = character()
  )
}

empty_failures <- function() {
  tibble::tibble(
    screening_id = character(),
    evidence_source = character(),
    attempts = integer(),
    error = character(),
    failed_at = character()
  )
}

checkpoint <- empty_checkpoint()
failures <- empty_failures()

if (
  resume_enabled &&
  file.exists(checkpoint_file)
) {
  old_checkpoint <- readr::read_csv(
    checkpoint_file,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )

  if (
    all(
      names(empty_checkpoint()) %in%
        names(old_checkpoint)
    )
  ) {
    checkpoint <- old_checkpoint

    # Normalize all checkpoint columns to ordinary character vectors.
    # This prevents classed JSON/vctrs columns from older runs from
    # causing bind_rows() type conflicts.
    for (column_name in names(empty_checkpoint())) {
      checkpoint[[column_name]] <- as.character(
        checkpoint[[column_name]]
      )
    }
  }
}


# ============================================================
# 16. Revalidate current checkpoint records
# ============================================================

current_valid_ids <- character(0)

if (nrow(checkpoint) > 0L) {
  keep <- rep(FALSE, nrow(checkpoint))

  for (i in seq_len(nrow(checkpoint))) {
    id <- safe_chr(checkpoint$screening_id[[i]])
    idx <- match(
      id,
      included_studies$screening_id
    )

    if (is.na(idx)) {
      next
    }

    record <- included_studies[
      idx,
      ,
      drop = FALSE
    ]

    expected_evidence <- safe_chr(
      record$agent_evidence_source
    )

    current_checksum <- safe_chr(
      record$current_pdf_checksum_md5
    )

    stored_checksum <- safe_chr(
      checkpoint$pdf_checksum_md5[[i]]
    )

    checksum_ok <- if (
      identical(expected_evidence, "Full-text PDF")
    ) {
      nzchar(current_checksum) &&
        identical(
          current_checksum,
          stored_checksum
        )
    } else {
      TRUE
    }

    version_ok <- identical(
      safe_chr(checkpoint$schema_version[[i]]),
      schema_version
    ) &&
      identical(
        safe_chr(checkpoint$prompt_version[[i]]),
        prompt_version
      )

    parsed_ok <- FALSE

    if (
      identical(
        safe_chr(checkpoint$validation_status[[i]]),
        "valid"
      ) &&
      checksum_ok &&
      version_ok
    ) {
      parsed <- tryCatch(
        jsonlite::fromJSON(
          checkpoint$extraction_json[[i]],
          simplifyVector = FALSE
        ),
        error = function(e) NULL
      )

      if (!is.null(parsed)) {
        parsed_ok <- identical(
          validate_result(
            parsed,
            expected_id = id,
            expected_evidence = expected_evidence
          ),
          "valid"
        )
      }
    }

    keep[[i]] <- parsed_ok
  }

  checkpoint <- checkpoint[
    keep,
    ,
    drop = FALSE
  ]

  if (anyDuplicated(checkpoint$screening_id) > 0L) {
    checkpoint <- checkpoint[
      !duplicated(
        checkpoint$screening_id,
        fromLast = TRUE
      ),
      ,
      drop = FALSE
    ]
  }

  current_valid_ids <- checkpoint$screening_id

  if (length(current_valid_ids) > 0L) {
    cat(
      "Recovered ",
      length(current_valid_ids),
      " current valid extraction checkpoint(s).\n",
      sep = ""
    )
  }
}


# ============================================================
# 17. Extraction loop
# ============================================================

cat("\n")
cat("============================================================\n")
cat("TASK-013 — Multimodal Data Extraction\n")
cat("============================================================\n\n")

cat("Project: ", project_title, "\n", sep = "")
cat("Included studies: ", nrow(included_studies), "\n", sep = "")
cat("Configured outcomes: ", length(project_outcomes), "\n", sep = "")
cat("Provider: ", provider_name, "\n", sep = "")
cat("Model: ", model_name, "\n\n", sep = "")

processed_this_run <- 0L

for (i in seq_len(nrow(included_studies))) {
  record <- included_studies[
    i,
    ,
    drop = FALSE
  ]

  screening_id <- safe_chr(
    record$screening_id
  )

  if (screening_id %in% current_valid_ids) {
    cat(
      "Skipping checkpointed study: ",
      screening_id,
      "\n",
      sep = ""
    )
    next
  }

  evidence_source <- safe_chr(
    record$agent_evidence_source
  )

  pdf_path <- if (
    identical(evidence_source, "Full-text PDF")
  ) {
    safe_chr(record$pdf_path)
  } else {
    NULL
  }

  prompt <- build_extraction_prompt(
    record,
    evidence_source
  )

  success <- FALSE
  last_error <- ""

  for (attempt in seq_len(max_retries)) {
    outcome <- tryCatch(
      {
        response <- call_extraction_model(
          prompt = prompt,
          pdf_path = pdf_path
        )

        validation_status <- validate_result(
          response$parsed,
          expected_id = screening_id,
          expected_evidence = evidence_source
        )

        if (!identical(validation_status, "valid")) {
          stop(
            "Structured extraction validation failed: ",
            validation_status,
            call. = FALSE
          )
        }

        new_row <- tibble::tibble(
          screening_id = as.character(screening_id),
          evidence_source = as.character(evidence_source),
          pdf_checksum_md5 = as.character(
            safe_chr(
              record$current_pdf_checksum_md5
            )
          ),
          schema_version = as.character(schema_version),
          prompt_version = as.character(prompt_version),
          extraction_status = as.character(
            safe_chr(
              response$parsed$extraction_status
            )
          ),
          response_id = as.character(
            safe_chr(
              response$response_id
            )
          ),
          model = as.character(
            if (
              nzchar(safe_chr(response$model))
            ) {
              safe_chr(response$model)
            } else {
              model_name
            }
          ),
          completed_at = as.character(now_text()),
          validation_status = "valid",
          extraction_json = as.character(
            safe_json(
              response$parsed
            )
          )
        )

        checkpoint <- checkpoint[
          checkpoint$screening_id != screening_id,
          ,
          drop = FALSE
        ]

        checkpoint <- dplyr::bind_rows(
          checkpoint,
          new_row
        )

        success <- TRUE
        TRUE
      },
      error = function(e) {
        last_error <<- conditionMessage(e)
        FALSE
      }
    )

    if (isTRUE(outcome)) {
      break
    }

    if (attempt < max_retries) {
      Sys.sleep(
        min(
          2 ^ (attempt - 1L),
          10
        )
      )
    }
  }

  if (success) {
    cat(
      "Extracted: ",
      screening_id,
      " [",
      evidence_source,
      "]\n",
      sep = ""
    )
  } else {
    failures <- dplyr::bind_rows(
      failures,
      tibble::tibble(
        screening_id = screening_id,
        evidence_source = evidence_source,
        attempts = max_retries,
        error = last_error,
        failed_at = now_text()
      )
    )

    cat(
      "FAILED: ",
      screening_id,
      " — ",
      last_error,
      "\n",
      sep = ""
    )
  }

  processed_this_run <- processed_this_run + 1L

  if (
    checkpoint_every > 0L &&
    processed_this_run %% checkpoint_every == 0L
  ) {
    readr::write_csv(
      checkpoint,
      checkpoint_file,
      na = ""
    )

    cat(
      "Checkpoint saved: ",
      nrow(checkpoint),
      " valid extraction(s).\n",
      sep = ""
    )
  }

  if (pause_seconds > 0) {
    Sys.sleep(pause_seconds)
  }
}

readr::write_csv(
  checkpoint,
  checkpoint_file,
  na = ""
)

readr::write_csv(
  failures,
  failure_file,
  na = ""
)


# ============================================================
# 18. Require complete Agent processing
# ============================================================

valid_ids <- unique(
  checkpoint$screening_id[
    checkpoint$validation_status == "valid"
  ]
)

missing_ids <- setdiff(
  included_studies$screening_id,
  valid_ids
)

stage_complete <- (
  length(missing_ids) == 0L &&
    nrow(failures) == 0L
)


# ============================================================
# 19. Normalize structured checkpoint into analysis tables
# ============================================================

study_rows <- list()
group_rows <- list()
outcome_rows <- list()

study_counter <- 0L
group_counter <- 0L
outcome_counter <- 0L

for (i in seq_len(nrow(checkpoint))) {
  if (!identical(
    safe_chr(checkpoint$validation_status[[i]]),
    "valid"
  )) {
    next
  }

  parsed <- jsonlite::fromJSON(
    checkpoint$extraction_json[[i]],
    simplifyVector = FALSE
  )

  screening_id <- safe_chr(
    checkpoint$screening_id[[i]]
  )

  source_record <- included_studies[
    match(
      screening_id,
      included_studies$screening_id
    ),
    ,
    drop = FALSE
  ]

  study_counter <- study_counter + 1L

  study_data <- parsed$study_characteristics

  study_rows[[study_counter]] <- tibble::as_tibble(
    c(
      list(
        screening_id = screening_id,
        title = safe_chr(source_record$title),
        authors = safe_chr(source_record$authors),
        year = safe_chr(source_record$year),
        journal = safe_chr(source_record$journal),
        doi = safe_chr(source_record$doi),
        pmid = safe_chr(source_record$pmid),
        agent_evidence_source = safe_chr(
          parsed$evidence_source
        ),
        agent_extraction_status = safe_chr(
          parsed$extraction_status
        ),
        pdf_path = safe_chr(source_record$pdf_path),
        pdf_checksum_md5 = safe_chr(
          checkpoint$pdf_checksum_md5[[i]]
        )
      ),
      lapply(
        study_fields,
        function(field_name) {
          safe_chr(
            study_data[[field_name]]
          )
        }
      ) |>
        setNames(study_fields),
      list(
        overall_extraction_notes = safe_chr(
          parsed$overall_extraction_notes
        )
      )
    )
  )

  if (length(parsed$groups) > 0L) {
    for (g in seq_along(parsed$groups)) {
      group_counter <- group_counter + 1L

      group_item <- parsed$groups[[g]]

      group_rows[[group_counter]] <- tibble::as_tibble(
        c(
          list(
            screening_id = screening_id,
            group_index = as.character(g)
          ),
          lapply(
            group_fields,
            function(field_name) {
              safe_chr(
                group_item[[field_name]]
              )
            }
          ) |>
            setNames(group_fields)
        )
      )
    }
  }

  if (length(parsed$outcomes) > 0L) {
    for (o in seq_along(parsed$outcomes)) {
      outcome_counter <- outcome_counter + 1L

      outcome_item <- parsed$outcomes[[o]]

      outcome_name <- safe_chr(
        outcome_item$outcome_name
      )

      priority <- outcome_priority$outcome_priority[
        match(
          outcome_name,
          outcome_priority$outcome_name
        )
      ]

      outcome_rows[[outcome_counter]] <- tibble::as_tibble(
        c(
          list(
            screening_id = screening_id,
            outcome_row_id = paste0(
              screening_id,
              "_OUT_",
              sprintf("%03d", o)
            ),
            outcome_priority = safe_chr(priority)
          ),
          lapply(
            outcome_fields,
            function(field_name) {
              safe_chr(
                outcome_item[[field_name]]
              )
            }
          ) |>
            setNames(outcome_fields)
        )
      )
    }
  }
}

study_characteristics <- if (
  length(study_rows) == 0L
) {
  tibble::tibble()
} else {
  dplyr::bind_rows(study_rows)
}

group_characteristics <- if (
  length(group_rows) == 0L
) {
  tibble::tibble(
    screening_id = character(),
    group_index = character()
  )
} else {
  dplyr::bind_rows(group_rows)
}

outcome_data <- if (
  length(outcome_rows) == 0L
) {
  empty_outcomes <- c(
    "screening_id",
    "outcome_row_id",
    "outcome_priority",
    outcome_fields
  )

  tibble::as_tibble(
    setNames(
      replicate(
        length(empty_outcomes),
        character(0),
        simplify = FALSE
      ),
      empty_outcomes
    )
  )
} else {
  dplyr::bind_rows(outcome_rows)
}


# ============================================================
# 20. Validate normalized Agent outputs
# ============================================================

if (
  stage_complete &&
  nrow(study_characteristics) !=
    nrow(included_studies)
) {
  stop(
    "Study-characteristics row count does not match Included Studies.",
    call. = FALSE
  )
}

if (
  nrow(study_characteristics) > 0L &&
  anyDuplicated(
    study_characteristics$screening_id
  ) > 0L
) {
  stop(
    "Study-characteristics dataset contains duplicated screening_id values.",
    call. = FALSE
  )
}

if (nrow(outcome_data) > 0L) {
  invalid_names <- setdiff(
    unique(outcome_data$outcome_name),
    project_outcomes
  )

  if (length(invalid_names) > 0L) {
    stop(
      "Outcome extraction contains noncanonical outcome name(s): ",
      paste(invalid_names, collapse = " | "),
      call. = FALSE
    )
  }

  if (anyDuplicated(outcome_data$outcome_row_id) > 0L) {
    stop(
      "Outcome extraction contains duplicated outcome_row_id values.",
      call. = FALSE
    )
  }
}


# ============================================================
# 21. Save Agent CSVs
# ============================================================

readr::write_csv(
  study_characteristics,
  study_characteristics_csv,
  na = ""
)

readr::write_csv(
  group_characteristics,
  group_characteristics_csv,
  na = ""
)

readr::write_csv(
  outcome_data,
  outcome_data_csv,
  na = ""
)


# ============================================================
# 22. Build Human Verification views
# ============================================================
# Agent values remain unchanged. Human verification fields are
# appended and intentionally blank.
# ============================================================

human_study <- study_characteristics |>
  dplyr::mutate(
    verification_status = NA_character_,
    corrected_value_notes = NA_character_,
    verifier = NA_character_,
    verified_at = NA_character_
  )

human_outcomes <- outcome_data |>
  dplyr::mutate(
    verification_status = NA_character_,
    corrected_value_notes = NA_character_,
    verifier = NA_character_,
    verified_at = NA_character_
  )


# ============================================================
# 23. Workbook helpers
# ============================================================

wb <- openxlsx::createWorkbook(
  creator = "Universal SR-MA Agent Toolkit"
)

title_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fontSize = 14,
  valign = "center"
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "Bottom"
)

wrap_style <- openxlsx::createStyle(
  valign = "top",
  wrapText = TRUE
)

write_table_sheet <- function(
    workbook,
    sheet_name,
    title,
    data,
    wide_columns = character(0)
) {
  openxlsx::addWorksheet(
    workbook,
    sheet_name
  )

  openxlsx::writeData(
    workbook,
    sheet_name,
    title,
    startRow = 1,
    startCol = 1
  )

  merge_cols <- max(
    1L,
    ncol(data)
  )

  if (merge_cols > 1L) {
    openxlsx::mergeCells(
      workbook,
      sheet_name,
      cols = 1:merge_cols,
      rows = 1
    )
  }

  openxlsx::addStyle(
    workbook,
    sheet_name,
    title_style,
    rows = 1,
    cols = 1
  )

  openxlsx::writeData(
    workbook,
    sheet_name,
    data,
    startRow = 3,
    startCol = 1,
    colNames = TRUE,
    rowNames = FALSE,
    withFilter = TRUE,
    keepNA = FALSE
  )

  if (ncol(data) > 0L) {
    openxlsx::addStyle(
      workbook,
      sheet_name,
      header_style,
      rows = 3,
      cols = seq_len(ncol(data)),
      gridExpand = TRUE
    )

    openxlsx::setColWidths(
      workbook,
      sheet_name,
      cols = seq_len(ncol(data)),
      widths = "auto"
    )
  }

  if (
    nrow(data) > 0L &&
    ncol(data) > 0L
  ) {
    openxlsx::addStyle(
      workbook,
      sheet_name,
      wrap_style,
      rows = 4:(nrow(data) + 3),
      cols = seq_len(ncol(data)),
      gridExpand = TRUE
    )
  }

  openxlsx::freezePane(
    workbook,
    sheet_name,
    firstActiveRow = 4,
    firstActiveCol = 2
  )

  for (
    wide_col in intersect(
      wide_columns,
      names(data)
    )
  ) {
    openxlsx::setColWidths(
      workbook,
      sheet_name,
      cols = which(
        names(data) == wide_col
      ),
      widths = 50
    )
  }

  invisible(TRUE)
}


# ============================================================
# 24. Instructions sheet
# ============================================================

openxlsx::addWorksheet(
  wb,
  "Instructions"
)

instructions <- tibble::tibble(
  Section = c(
    "Architecture",
    "Canonical input",
    "Agent evidence",
    "Study characteristics",
    "Outcome data",
    "Missing values",
    "Source provenance",
    "Human Verification",
    "Authoritative data",
    "Next stage"
  ),
  Instruction = c(
    "DEC-008: Multimodal Agent Extraction followed by Human Verification.",
    "TASK-012 data/processed/full_text/included_studies.csv.",
    "Full-text PDF is primary evidence when available; metadata-only extraction is explicitly labeled.",
    "Study_Characteristics_Agent contains one Agent extraction record per Included Study.",
    "Outcome_Data_Agent uses long format; one study may have multiple rows per outcome/timepoint/effect representation.",
    "Blank means unreported or non-extractable. TASK-013 does not impute, calculate, or convert missing values.",
    "source_location and source_evidence support rapid checking against the source article.",
    "Human_Verification_Study and Human_Verification_Outcomes copy Agent data and leave verification fields blank for TASK-014.",
    "Agent extraction is not authoritative. The human-verified TASK-014 dataset is authoritative for meta-analysis.",
    "After R/25 standalone PASS, proceed to TASK-014 Human Verification."
  )
)

openxlsx::writeData(
  wb,
  "Instructions",
  paste0(
    project_title,
    " — Multimodal Data Extraction"
  ),
  startRow = 1,
  startCol = 1
)

openxlsx::mergeCells(
  wb,
  "Instructions",
  cols = 1:2,
  rows = 1
)

openxlsx::addStyle(
  wb,
  "Instructions",
  title_style,
  rows = 1,
  cols = 1
)

openxlsx::writeData(
  wb,
  "Instructions",
  instructions,
  startRow = 3,
  startCol = 1
)

openxlsx::addStyle(
  wb,
  "Instructions",
  header_style,
  rows = 3,
  cols = 1:2,
  gridExpand = TRUE
)

openxlsx::setColWidths(
  wb,
  "Instructions",
  cols = 1,
  widths = 28
)

openxlsx::setColWidths(
  wb,
  "Instructions",
  cols = 2,
  widths = 95
)


# ============================================================
# 25. Agent and human-verification sheets
# ============================================================

write_table_sheet(
  wb,
  "Study_Characteristics_Agent",
  paste0(
    project_title,
    " — Agent Study Characteristics"
  ),
  study_characteristics,
  wide_columns = c(
    "title",
    "pdf_path",
    "extraction_notes",
    "overall_extraction_notes"
  )
)

write_table_sheet(
  wb,
  "Group_Characteristics_Agent",
  paste0(
    project_title,
    " — Agent Group Characteristics"
  ),
  group_characteristics,
  wide_columns = c(
    "baseline_characteristics_notes"
  )
)

write_table_sheet(
  wb,
  "Outcome_Data_Agent",
  paste0(
    project_title,
    " — Agent Outcome Data"
  ),
  outcome_data,
  wide_columns = c(
    "outcome_definition",
    "adjustment_variables",
    "source_location",
    "source_evidence",
    "extraction_notes"
  )
)

write_table_sheet(
  wb,
  "Human_Verification_Study",
  paste0(
    project_title,
    " — Human Verification: Study Data"
  ),
  human_study,
  wide_columns = c(
    "title",
    "pdf_path",
    "extraction_notes",
    "overall_extraction_notes",
    "corrected_value_notes"
  )
)

write_table_sheet(
  wb,
  "Human_Verification_Outcomes",
  paste0(
    project_title,
    " — Human Verification: Outcome Data"
  ),
  human_outcomes,
  wide_columns = c(
    "outcome_definition",
    "adjustment_variables",
    "source_location",
    "source_evidence",
    "extraction_notes",
    "corrected_value_notes"
  )
)


# ============================================================
# 26. Validation lists and dropdowns
# ============================================================

openxlsx::addWorksheet(
  wb,
  "Validation_Lists"
)

openxlsx::writeData(
  wb,
  "Validation_Lists",
  tibble::tibble(
    verification_status = verification_status_options
  ),
  startRow = 1,
  startCol = 1
)

if (nrow(human_study) > 0L) {
  status_col <- which(
    names(human_study) == "verification_status"
  )

  openxlsx::dataValidation(
    wb,
    "Human_Verification_Study",
    cols = status_col,
    rows = 4:(nrow(human_study) + 3),
    type = "list",
    value = paste0(
      "'Validation_Lists'!$A$2:$A$",
      length(verification_status_options) + 1L
    )
  )
}

if (nrow(human_outcomes) > 0L) {
  status_col <- which(
    names(human_outcomes) == "verification_status"
  )

  openxlsx::dataValidation(
    wb,
    "Human_Verification_Outcomes",
    cols = status_col,
    rows = 4:(nrow(human_outcomes) + 3),
    type = "list",
    value = paste0(
      "'Validation_Lists'!$A$2:$A$",
      length(verification_status_options) + 1L
    )
  )
}


# ============================================================
# 27. Extraction Summary sheet
# ============================================================

openxlsx::addWorksheet(
  wb,
  "Extraction_Summary"
)

complete_count <- sum(
  checkpoint$extraction_status == "Complete"
)

partial_metadata_count <- sum(
  checkpoint$extraction_status == "Partial — metadata only"
)

partial_source_count <- sum(
  checkpoint$extraction_status == "Partial — insufficient source"
)

pdf_evidence_count <- sum(
  checkpoint$evidence_source == "Full-text PDF"
)

metadata_evidence_count <- sum(
  checkpoint$evidence_source == "Metadata only"
)

summary_table <- tibble::tibble(
  Metric = c(
    "Project ID",
    "Included Studies",
    "Valid Agent extractions",
    "Complete extractions",
    "Partial — metadata only",
    "Partial — insufficient source",
    "Full-text PDF evidence",
    "Metadata-only evidence",
    "Extracted group rows",
    "Extracted outcome rows",
    "Unresolved failures",
    "TASK-013 Agent stage complete",
    "Schema version",
    "Generated at"
  ),
  Value = c(
    project_id,
    as.character(nrow(included_studies)),
    as.character(nrow(checkpoint)),
    as.character(complete_count),
    as.character(partial_metadata_count),
    as.character(partial_source_count),
    as.character(pdf_evidence_count),
    as.character(metadata_evidence_count),
    as.character(nrow(group_characteristics)),
    as.character(nrow(outcome_data)),
    as.character(nrow(failures)),
    if (stage_complete) "YES" else "NO",
    schema_version,
    now_text()
  )
)

openxlsx::writeData(
  wb,
  "Extraction_Summary",
  paste0(
    project_title,
    " — TASK-013 Extraction Summary"
  ),
  startRow = 1,
  startCol = 1
)

openxlsx::mergeCells(
  wb,
  "Extraction_Summary",
  cols = 1:2,
  rows = 1
)

openxlsx::addStyle(
  wb,
  "Extraction_Summary",
  title_style,
  rows = 1,
  cols = 1
)

openxlsx::writeData(
  wb,
  "Extraction_Summary",
  summary_table,
  startRow = 3,
  startCol = 1
)

openxlsx::addStyle(
  wb,
  "Extraction_Summary",
  header_style,
  rows = 3,
  cols = 1:2,
  gridExpand = TRUE
)

openxlsx::setColWidths(
  wb,
  "Extraction_Summary",
  cols = 1,
  widths = 42
)

openxlsx::setColWidths(
  wb,
  "Extraction_Summary",
  cols = 2,
  widths = 60
)


# ============================================================
# 28. Save and revalidate workbook
# ============================================================

openxlsx::saveWorkbook(
  wb,
  workbook_file,
  overwrite = TRUE
)

if (
  !file.exists(workbook_file) ||
  is.na(file.info(workbook_file)$size) ||
  file.info(workbook_file)$size <= 0
) {
  stop(
    "TASK-013 extraction workbook was not created successfully.",
    call. = FALSE
  )
}

required_sheets <- c(
  "Instructions",
  "Study_Characteristics_Agent",
  "Group_Characteristics_Agent",
  "Outcome_Data_Agent",
  "Human_Verification_Study",
  "Human_Verification_Outcomes",
  "Validation_Lists",
  "Extraction_Summary"
)

saved_sheets <- openxlsx::getSheetNames(
  workbook_file
)

missing_sheets <- setdiff(
  required_sheets,
  saved_sheets
)

if (length(missing_sheets) > 0L) {
  stop(
    "TASK-013 workbook is missing required sheet(s): ",
    paste(missing_sheets, collapse = ", "),
    call. = FALSE
  )
}

saved_human_study <- openxlsx::readWorkbook(
  workbook_file,
  sheet = "Human_Verification_Study",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

human_only_fields <- c(
  "verification_status",
  "corrected_value_notes",
  "verifier",
  "verified_at"
)

for (field_name in human_only_fields) {
  if (!field_name %in% names(saved_human_study)) {
    stop(
      "Human verification field missing from study sheet: ",
      field_name,
      call. = FALSE
    )
  }

  values <- clean_text(
    saved_human_study[[field_name]]
  )

  if (any(!is.na(values))) {
    stop(
      "R/25 unexpectedly populated human-only field: ",
      field_name,
      call. = FALSE
    )
  }
}


# ============================================================
# 29. Report
# ============================================================

stage_status <- if (
  stage_complete
) {
  "PASS"
} else {
  "INCOMPLETE"
}

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "TASK-013 — Multimodal Data Extraction Report",
  "",
  paste0("Generated at: ", now_text()),
  paste0("Project ID: ", project_id),
  paste0("Project title: ", project_title),
  "",
  "ARCHITECTURE",
  "--------------------------------------------------",
  "DEC-008: Multimodal Agent Extraction -> Human Verification",
  "Agent extraction authoritative: NO",
  "Human verification populated by R/25: NO",
  "Runner integration performed: NO",
  "",
  "CANONICAL INPUT",
  "--------------------------------------------------",
  paste0("Included Studies: ", included_studies_csv),
  paste0("Included study count: ", nrow(included_studies)),
  paste0("Extraction schema: ", schema_file),
  paste0("Schema version: ", schema_version),
  "",
  "AGENT EXTRACTION",
  "--------------------------------------------------",
  paste0("Valid Agent extractions: ", nrow(checkpoint)),
  paste0("Complete extractions: ", complete_count),
  paste0("Partial — metadata only: ", partial_metadata_count),
  paste0("Partial — insufficient source: ", partial_source_count),
  paste0("Full-text PDF evidence: ", pdf_evidence_count),
  paste0("Metadata-only evidence: ", metadata_evidence_count),
  paste0("Study-characteristics rows: ", nrow(study_characteristics)),
  paste0("Group rows: ", nrow(group_characteristics)),
  paste0("Outcome rows: ", nrow(outcome_data)),
  paste0("Unresolved failures: ", nrow(failures)),
  paste0("Missing study extractions: ", length(missing_ids)),
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0("Study characteristics CSV: ", study_characteristics_csv),
  paste0("Group characteristics CSV: ", group_characteristics_csv),
  paste0("Outcome data CSV: ", outcome_data_csv),
  paste0("Human verification workbook: ", workbook_file),
  paste0("Checkpoint: ", checkpoint_file),
  paste0("Failure log: ", failure_file),
  paste0("Report: ", report_file),
  "",
  "STAGE BOUNDARY",
  "--------------------------------------------------",
  "Meta-analysis-ready authoritative dataset created: NO",
  "Next stage after PASS: TASK-014 — Human Verification",
  "",
  "TASK STATUS",
  "--------------------------------------------------",
  paste0("TASK-013: ", stage_status)
)

writeLines(
  report_lines,
  con = report_file,
  useBytes = TRUE
)

if (
  !file.exists(report_file) ||
  is.na(file.info(report_file)$size) ||
  file.info(report_file)$size <= 0
) {
  stop(
    "TASK-013 report was not created successfully.",
    call. = FALSE
  )
}


# ============================================================
# 30. Console completion
# ============================================================

cat("\n")
cat("============================================================\n")
cat("TASK-013 — Multimodal Data Extraction\n")
cat("============================================================\n\n")

cat(
  "Included Studies: ",
  nrow(included_studies),
  "\n",
  sep = ""
)

cat(
  "Valid Agent extractions: ",
  nrow(checkpoint),
  "\n",
  sep = ""
)

cat(
  "Full-text PDF evidence: ",
  pdf_evidence_count,
  "\n",
  sep = ""
)

cat(
  "Metadata-only evidence: ",
  metadata_evidence_count,
  "\n",
  sep = ""
)

cat(
  "Outcome rows extracted: ",
  nrow(outcome_data),
  "\n",
  sep = ""
)

cat(
  "Failures: ",
  nrow(failures),
  "\n\n",
  sep = ""
)

cat(
  "Workbook:\n- ",
  workbook_file,
  "\n",
  sep = ""
)

cat(
  "Outcome data CSV:\n- ",
  outcome_data_csv,
  "\n",
  sep = ""
)

cat(
  "Report:\n- ",
  report_file,
  "\n\n",
  sep = ""
)

if (stage_complete) {
  cat(
    "PASS: TASK-013 multimodal Agent data extraction completed successfully.\n"
  )

  cat(
    "Next PM-gated stage: TASK-014 — Human Verification.\n"
  )

} else {
  cat(
    "TASK-013 STATUS: INCOMPLETE\n"
  )

  cat(
    "Resolve failures or missing Agent extraction records and rerun R/25.\n"
  )
}

invisible(
  list(
    status = stage_status,
    study_characteristics = study_characteristics,
    group_characteristics = group_characteristics,
    outcome_data = outcome_data,
    checkpoint = checkpoint,
    failures = failures,
    workbook = workbook_file,
    report = report_file
  )
)
