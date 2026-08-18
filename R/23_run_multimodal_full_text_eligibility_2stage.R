# ============================================================
# Universal SR-MA Agent Toolkit
# R/23_run_multimodal_full_text_eligibility_2stage.R
#
# TASK-011 — Multimodal Full-Text Eligibility
#
# Purpose
# ------------------------------------------------------------
# 1. Read TASK-010 authoritative full-text registry.
# 2. Use PDF full text as the primary evidence source when available.
# 3. Use bibliographic metadata only when the PDF is Missing/Invalid.
# 4. Run Agent Reviewer 1 eligibility assessment.
# 5. Create an Author Final Review worksheet for the 1st author.
# 6. Produce an auditable eligibility workbook, decision log,
#    failure log, checkpoint, and stage report.
#
# IMPORTANT STAGE BOUNDARY
# ------------------------------------------------------------
# - R/23 is the AGENT REVIEW stage of a 2-stage architecture:
#       Stage 1: Reviewer Agent
#       Stage 2: 1st Author Final Review
# - It does NOT create the authoritative Included Studies dataset.
# - There is NO second independent human reviewer in this workflow.
# - There is NO conflict/adjudication stage.
# - The 1st author directly reviews the agent assessment and enters
#   the authoritative final Include/Exclude decision.
# - TASK-012 will validate the author's final decisions and create
#   the authoritative Included Studies dataset.
# - This script remains independently executable and may also be
#   invoked by the canonical Universal Runner.
#
# Decision vocabulary
# ------------------------------------------------------------
# Include
# Exclude
# Human Review Required
#
# Evidence policy
# ------------------------------------------------------------
# PDF Available:
#   -> PDF is sent to the multimodal model together with metadata.
#
# PDF Missing / Invalid PDF:
#   -> metadata-only assessment.
#   -> the agent may Exclude only when metadata clearly establishes
#      ineligibility.
#   -> otherwise use Human Review Required.
#
# Missing PDF is NOT automatically "Full text not available".
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
# 2. Utilities and canonical project context
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

review_question <- config$research_question$question
inclusion_criteria <- config$eligibility$inclusion
exclusion_reason_table <- get_exclusion_reasons(config)

if (
  is.null(review_question) ||
  !is.character(review_question) ||
  length(review_question) != 1L ||
  !nzchar(trimws(review_question))
) {
  stop(
    "Canonical research_question.question is missing or empty.",
    call. = FALSE
  )
}

if (
  is.null(inclusion_criteria) ||
  length(inclusion_criteria) == 0L
) {
  stop(
    "Canonical eligibility.inclusion is missing or empty.",
    call. = FALSE
  )
}

if (
  is.null(exclusion_reason_table) ||
  nrow(exclusion_reason_table) == 0L
) {
  stop(
    "No canonical exclusion reasons were found in project_config.yml.",
    call. = FALSE
  )
}


# ============================================================
# 3. Agent provider configuration
# ============================================================
# Reuse the already validated provider/model settings from
# config/agent_screening.yml, but keep Phase 2 output names local
# to this script so Phase 1 artifacts remain untouched.
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
    "agent_screening is missing, invalid, or disabled.",
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
    "R/23 multimodal PDF eligibility currently supports the ",
    "existing OpenAI provider configuration only.\n",
    "Current provider: ",
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
  agent_config$processing$timeout_seconds %||% 180
)

max_retries <- as.integer(
  agent_config$processing$max_retries %||% 3L
)

checkpoint_every <- as.integer(
  agent_config$processing$checkpoint_every %||% 5L
)

pause_seconds <- as.numeric(
  agent_config$processing$request_pause_seconds %||% 0
)

resume_enabled <- isTRUE(
  agent_config$processing$resume_from_checkpoint
)

start_record <- as.integer(
  agent_config$processing$start_record %||% 1L
)

max_records <- agent_config$processing$max_records

reviewer_name <- paste0(
  agent_config$reviewer$name %||% "Agent Reviewer 1",
  " — Full Text"
)

author_reviewer_name <- get_reviewer_name(
  config,
  reviewer = "reviewer_1",
  fallback = "1st Author"
)

prompt_version <- "full_text_eligibility_v2_two_stage"


# ============================================================
# 4. TASK-011 canonical Phase 2 paths
# ============================================================

processed_full_text_directory <- file.path(
  paths$project_root,
  "data",
  "processed",
  "full_text"
)

full_text_results_directory <- file.path(
  paths$project_root,
  "results",
  "full_text"
)

registry_csv <- file.path(
  processed_full_text_directory,
  "full_text_registry.csv"
)

output_workbook <- file.path(
  full_text_results_directory,
  "full_text_eligibility_agent.xlsx"
)

decisions_file <- file.path(
  paths$logs_directory,
  "full_text_eligibility_agent_decisions.csv"
)

failures_file <- file.path(
  paths$logs_directory,
  "full_text_eligibility_agent_failures.csv"
)

checkpoint_file <- file.path(
  paths$logs_directory,
  "full_text_eligibility_agent_checkpoint.csv"
)

report_file <- file.path(
  paths$logs_directory,
  "full_text_eligibility_agent_report.txt"
)

ensure_directories(
  c(
    processed_full_text_directory,
    full_text_results_directory,
    paths$logs_directory
  )
)

assert_file_exists(
  registry_csv,
  "TASK-010 full-text registry"
)


# ============================================================
# 5. Read and validate TASK-010 registry
# ============================================================

registry <- readr::read_csv(
  registry_csv,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
)

required_registry_columns <- c(
  "screening_id",
  "title",
  "abstract",
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
  "pdf_checksum_md5",
  "intake_status"
)

missing_registry_columns <- setdiff(
  required_registry_columns,
  names(registry)
)

if (length(missing_registry_columns) > 0L) {
  stop(
    "TASK-010 registry is missing required column(s):\n- ",
    paste(
      missing_registry_columns,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}

registry$screening_id <- clean_text(registry$screening_id)
registry$pdf_status <- clean_text(registry$pdf_status)
registry$pdf_path <- clean_text(registry$pdf_path)

if (nrow(registry) == 0L) {
  stop(
    "TASK-010 registry contains zero records.",
    call. = FALSE
  )
}

if (any(is.na(registry$screening_id))) {
  stop(
    "TASK-010 registry contains missing screening_id values.",
    call. = FALSE
  )
}

if (anyDuplicated(registry$screening_id) > 0L) {
  stop(
    "TASK-010 registry contains duplicated screening_id values.",
    call. = FALSE
  )
}

allowed_pdf_status <- c(
  "Available",
  "Missing",
  "Invalid PDF"
)

invalid_pdf_status <- setdiff(
  unique(registry$pdf_status),
  allowed_pdf_status
)

invalid_pdf_status <- invalid_pdf_status[
  !is.na(invalid_pdf_status)
]

if (length(invalid_pdf_status) > 0L) {
  stop(
    "TASK-010 registry contains unsupported pdf_status value(s): ",
    paste(invalid_pdf_status, collapse = ", "),
    call. = FALSE
  )
}

available_rows <- which(
  registry$pdf_status == "Available"
)

if (length(available_rows) > 0L) {
  missing_available_files <- available_rows[
    !file.exists(registry$pdf_path[available_rows])
  ]

  if (length(missing_available_files) > 0L) {
    stop(
      "Registry marks PDF(s) Available but file(s) are missing:\n- ",
      paste(
        registry$screening_id[missing_available_files],
        collapse = "\n- "
      ),
      call. = FALSE
    )
  }
}


# ============================================================
# 6. Full-text eligibility decision schema
# ============================================================

decision_options <- c(
  "Include",
  "Exclude",
  "Human Review Required"
)

confidence_options <- c(
  "High",
  "Moderate",
  "Low"
)

evidence_source_options <- c(
  "Full-text PDF",
  "Metadata only"
)

json_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  required = c(
    "screening_id",
    "decision",
    "exclusion_reason_code",
    "exclusion_reason",
    "confidence",
    "evidence_source",
    "rationale",
    "eligibility_evidence"
  ),
  properties = list(
    screening_id = list(
      type = "string"
    ),
    decision = list(
      type = "string",
      enum = unname(decision_options)
    ),
    exclusion_reason_code = list(
      type = "string",
      enum = unname(
        c(
          "",
          exclusion_reason_table$code
        )
      )
    ),
    exclusion_reason = list(
      type = "string",
      enum = unname(
        c(
          "",
          exclusion_reason_table$label
        )
      )
    ),
    confidence = list(
      type = "string",
      enum = unname(confidence_options)
    ),
    evidence_source = list(
      type = "string",
      enum = unname(evidence_source_options)
    ),
    rationale = list(
      type = "string"
    ),
    eligibility_evidence = list(
      type = "string"
    )
  )
)


# ============================================================
# 7. Prompt builder
# ============================================================

format_inclusion_criteria <- function(x) {
  values <- as.character(
    unlist(
      x,
      recursive = TRUE,
      use.names = FALSE
    )
  )

  values <- trimws(values)
  values <- values[
    !is.na(values) &
      nzchar(values)
  ]

  if (length(values) == 0L) {
    return("- Apply the finalized review protocol.")
  }

  paste0(
    "- ",
    values,
    collapse = "\n"
  )
}


format_exclusion_reasons <- function(reason_table) {
  paste(
    paste0(
      "- ",
      reason_table$code,
      ": ",
      reason_table$label
    ),
    collapse = "\n"
  )
}


build_full_text_prompt <- function(
    record,
    evidence_mode
) {
  metadata_block <- paste0(
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
    "abstract: ", safe_chr(record$abstract), "\n",
    "PDF status: ", safe_chr(record$pdf_status)
  )

  evidence_instruction <- if (
    identical(evidence_mode, "Full-text PDF")
  ) {
    paste(
      "A full-text PDF is attached to this request.",
      "Use the PDF as the primary eligibility evidence.",
      "Inspect the methods, population, intervention/exposure, comparator,",
      "study design, outcome reporting, and any relevant tables/figures.",
      "Do not rely only on the abstract when the PDF provides better evidence."
    )
  } else {
    paste(
      "No valid full-text PDF is attached.",
      "Use metadata only.",
      "You may choose Exclude only when the supplied metadata itself",
      "clearly establishes ineligibility.",
      "If eligibility cannot be established confidently without full text,",
      "choose Human Review Required.",
      "Do not automatically use 'Full text not available' merely because",
      "the PDF is currently missing."
    )
  }

  paste0(
    "You are Agent Reviewer 1 performing FULL-TEXT ELIGIBILITY screening ",
    "for a systematic review and meta-analysis.\n\n",

    "PROJECT\n",
    "Project ID: ", project_id, "\n",
    "Project title: ", project_title, "\n",
    "Research question: ", review_question, "\n\n",

    "INCLUSION CRITERIA\n",
    format_inclusion_criteria(inclusion_criteria),
    "\n\n",

    "CANONICAL EXCLUSION REASONS\n",
    format_exclusion_reasons(exclusion_reason_table),
    "\n\n",

    "RECORD METADATA\n",
    metadata_block,
    "\n\n",

    "EVIDENCE MODE\n",
    evidence_instruction,
    "\n\n",

    "DECISION RULES\n",
    "1. Include only if the available evidence supports eligibility.\n",
    "2. Exclude only with exactly one canonical exclusion reason.\n",
    "3. For Include, exclusion_reason_code and exclusion_reason must be empty.\n",
    "4. For Human Review Required, exclusion_reason_code and exclusion_reason ",
    "must be empty.\n",
    "5. Human Review Required is appropriate for unresolved ambiguity, ",
    "missing decisive full-text evidence, overlapping reports needing linkage, ",
    "or any case requiring expert judgement.\n",
    "6. eligibility_evidence must identify the decisive evidence in concise ",
    "audit-friendly language. When a PDF is attached, mention the relevant ",
    "section/table/figure/page if clearly identifiable; do not invent page ",
    "numbers or locations.\n",
    "7. Return only the structured JSON required by the schema.\n"
  )
}


# ============================================================
# 8. OpenAI multimodal PDF adapter
# ============================================================
#
# httr2 multipart uploads require curl::form_file() objects.
# Do not use httr2::upload_file(); it is not an exported httr2
# function in current httr2 releases.
# ============================================================

extract_openai_output_text_r23 <- function(response_body) {
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
          "The model refused the eligibility request: ",
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


upload_openai_pdf <- function(
    file_path
) {
  endpoint <- paste0(
    base_url,
    "/files"
  )

  response <- httr2::request(endpoint) |>
    httr2::req_headers(
      Authorization = paste(
        "Bearer",
        api_key
      )
    ) |>
    httr2::req_body_multipart(
      purpose = "user_data",
      file = curl::form_file(
        file_path,
        type = "application/pdf"
      )
    ) |>
    httr2::req_timeout(
      timeout_seconds
    ) |>
    httr2::req_error(
      is_error = function(resp) FALSE
    ) |>
    httr2::req_perform()

  status <- httr2::resp_status(
    response
  )

  response_text <- httr2::resp_body_string(
    response
  )

  if (
    status < 200L ||
    status >= 300L
  ) {
    stop(
      "OpenAI PDF upload failed with HTTP ",
      status,
      ".\n",
      substr(
        response_text,
        1L,
        3000L
      ),
      call. = FALSE
    )
  }

  body <- jsonlite::fromJSON(
    response_text,
    simplifyVector = FALSE
  )

  file_id <- safe_chr(
    body$id
  )

  if (!nzchar(file_id)) {
    stop(
      "OpenAI PDF upload response did not contain a file id.",
      call. = FALSE
    )
  }

  file_id
}


delete_openai_file <- function(
    file_id
) {
  if (!nzchar(file_id)) {
    return(invisible(FALSE))
  }

  endpoint <- paste0(
    base_url,
    "/files/",
    file_id
  )

  try(
    httr2::request(endpoint) |>
      httr2::req_method("DELETE") |>
      httr2::req_headers(
        Authorization = paste(
          "Bearer",
          api_key
        )
      ) |>
      httr2::req_timeout(
        timeout_seconds
      ) |>
      httr2::req_error(
        is_error = function(resp) FALSE
      ) |>
      httr2::req_perform(),
    silent = TRUE
  )

  invisible(TRUE)
}


call_openai_full_text_eligibility <- function(
    prompt,
    pdf_path = NULL
) {
  uploaded_file_id <- ""

  on.exit(
    {
      if (nzchar(uploaded_file_id)) {
        delete_openai_file(
          uploaded_file_id
        )
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
    uploaded_file_id <- upload_openai_pdf(
      pdf_path
    )

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
        name = "full_text_eligibility_decision",
        strict = TRUE,
        schema = json_schema
      )
    )
  )

  endpoint <- paste0(
    base_url,
    "/responses"
  )

  response <- httr2::request(endpoint) |>
    httr2::req_headers(
      Authorization = paste(
        "Bearer",
        api_key
      ),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(
      request_body,
      auto_unbox = TRUE
    ) |>
    httr2::req_timeout(
      timeout_seconds
    ) |>
    httr2::req_error(
      is_error = function(resp) FALSE
    ) |>
    httr2::req_perform()

  status <- httr2::resp_status(
    response
  )

  response_text <- httr2::resp_body_string(
    response
  )

  if (
    status < 200L ||
    status >= 300L
  ) {
    stop(
      "OpenAI Responses API request failed with HTTP ",
      status,
      ".\n",
      substr(
        response_text,
        1L,
        3000L
      ),
      call. = FALSE
    )
  }

  response_body <- jsonlite::fromJSON(
    response_text,
    simplifyVector = FALSE
  )

  output_text <- extract_openai_output_text_r23(
    response_body
  )

  parsed <- jsonlite::fromJSON(
    output_text,
    simplifyVector = TRUE
  )

  list(
    parsed = parsed,
    response_id = safe_chr(response_body$id),
    model = safe_chr(response_body$model) %||% model_name,
    raw_output_text = output_text,
    raw_response = response_text,
    uploaded_file_id = uploaded_file_id
  )
}


# ============================================================
# 9. Result validation
# ============================================================

validate_agent_result <- function(
    result,
    expected_screening_id,
    expected_evidence_mode
) {
  required_fields <- c(
    "screening_id",
    "decision",
    "exclusion_reason_code",
    "exclusion_reason",
    "confidence",
    "evidence_source",
    "rationale",
    "eligibility_evidence"
  )

  if (!all(required_fields %in% names(result))) {
    return(
      "missing_required_fields"
    )
  }

  screening_id <- safe_chr(
    result$screening_id
  )

  decision <- safe_chr(
    result$decision
  )

  reason_code <- safe_chr(
    result$exclusion_reason_code
  )

  reason_label <- safe_chr(
    result$exclusion_reason
  )

  confidence <- safe_chr(
    result$confidence
  )

  evidence_source <- safe_chr(
    result$evidence_source
  )

  rationale <- safe_chr(
    result$rationale
  )

  eligibility_evidence <- safe_chr(
    result$eligibility_evidence
  )

  if (!identical(screening_id, expected_screening_id)) {
    return(
      "screening_id_mismatch"
    )
  }

  if (!decision %in% decision_options) {
    return(
      "invalid_decision"
    )
  }

  if (!confidence %in% confidence_options) {
    return(
      "invalid_confidence"
    )
  }

  if (!identical(evidence_source, expected_evidence_mode)) {
    return(
      "evidence_source_mismatch"
    )
  }

  if (!nzchar(trimws(rationale))) {
    return(
      "missing_rationale"
    )
  }

  if (!nzchar(trimws(eligibility_evidence))) {
    return(
      "missing_eligibility_evidence"
    )
  }

  if (identical(decision, "Exclude")) {
    matching_reason <- exclusion_reason_table[
      exclusion_reason_table$code == reason_code &
        exclusion_reason_table$label == reason_label,
      ,
      drop = FALSE
    ]

    if (nrow(matching_reason) != 1L) {
      return(
        "invalid_exclusion_reason"
      )
    }

  } else {
    if (
      nzchar(trimws(reason_code)) ||
      nzchar(trimws(reason_label))
    ) {
      return(
        "reason_present_for_nonexclude"
      )
    }
  }

  "valid"
}


# ============================================================
# 10. Checkpoint structures
# ============================================================

empty_decision_log <- function() {
  tibble::tibble(
    screening_id = character(),
    decision = character(),
    exclusion_reason_code = character(),
    exclusion_reason = character(),
    confidence = character(),
    evidence_source = character(),
    rationale = character(),
    eligibility_evidence = character(),
    reviewer = character(),
    provider = character(),
    model = character(),
    prompt_version = character(),
    pdf_status = character(),
    pdf_checksum_md5 = character(),
    response_id = character(),
    attempt = character(),
    started_at = character(),
    completed_at = character(),
    elapsed_seconds = character(),
    validation_status = character(),
    raw_output_text = character()
  )
}

empty_failure_log <- function() {
  tibble::tibble(
    screening_id = character(),
    row_index = integer(),
    pdf_status = character(),
    evidence_source = character(),
    provider = character(),
    model = character(),
    attempts = integer(),
    error = character(),
    failed_at = character()
  )
}

decision_log <- empty_decision_log()
failure_log <- empty_failure_log()

if (
  resume_enabled &&
  file.exists(checkpoint_file)
) {
  decision_log <- readr::read_csv(
    checkpoint_file,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )
}


# ============================================================
# 11. Revalidate checkpoint against current registry/policy
# ============================================================

completed_ids <- character(0)

if (nrow(decision_log) > 0L) {
  required_checkpoint_columns <- names(
    empty_decision_log()
  )

  missing_checkpoint_columns <- setdiff(
    required_checkpoint_columns,
    names(decision_log)
  )

  if (length(missing_checkpoint_columns) > 0L) {
    cat(
      "Checkpoint schema is outdated; checkpoint will be ignored.\n"
    )

    decision_log <- empty_decision_log()

  } else {
    valid_rows <- decision_log[
      decision_log$validation_status == "valid",
      ,
      drop = FALSE
    ]

    if (nrow(valid_rows) > 0L) {
      registry_index <- match(
        valid_rows$screening_id,
        registry$screening_id
      )

      checkpoint_current <- !is.na(
        registry_index
      )

      for (i in seq_len(nrow(valid_rows))) {
        if (!checkpoint_current[[i]]) {
          next
        }

        registry_row <- registry[
          registry_index[[i]],
          ,
          drop = FALSE
        ]

        expected_mode <- if (
          identical(
            safe_chr(registry_row$pdf_status),
            "Available"
          )
        ) {
          "Full-text PDF"
        } else {
          "Metadata only"
        }

        stored_checksum <- safe_chr(
          valid_rows$pdf_checksum_md5[[i]]
        )

        current_checksum <- safe_chr(
          registry_row$pdf_checksum_md5
        )

        checksum_ok <- if (
          identical(expected_mode, "Full-text PDF")
        ) {
          identical(
            stored_checksum,
            current_checksum
          ) &&
            nzchar(current_checksum)
        } else {
          TRUE
        }

        policy_check <- validate_agent_result(
          result = as.list(
            valid_rows[i, , drop = FALSE]
          ),
          expected_screening_id = safe_chr(
            valid_rows$screening_id[[i]]
          ),
          expected_evidence_mode = expected_mode
        )

        checkpoint_current[[i]] <-
          identical(
            policy_check,
            "valid"
          ) &&
          checksum_ok
      }

      valid_rows <- valid_rows[
        checkpoint_current,
        ,
        drop = FALSE
      ]

      if (
        anyDuplicated(
          valid_rows$screening_id
        ) > 0L
      ) {
        stop(
          "Checkpoint contains duplicated valid screening IDs.",
          call. = FALSE
        )
      }

      decision_log <- valid_rows
      completed_ids <- unique(
        valid_rows$screening_id
      )

      if (length(completed_ids) > 0L) {
        cat(
          "Recovered ",
          length(completed_ids),
          " current valid full-text eligibility checkpoint decision(s).\n",
          sep = ""
        )
      }
    } else {
      decision_log <- empty_decision_log()
    }
  }
}


# ============================================================
# 12. Determine records scheduled for this run
# ============================================================

end_record <- nrow(
  registry
)

if (
  !is.null(max_records) &&
  length(max_records) > 0L &&
  !is.na(max_records)
) {
  end_record <- min(
    end_record,
    start_record +
      as.integer(max_records) -
      1L
  )
}

if (
  start_record < 1L ||
  start_record > nrow(registry)
) {
  stop(
    "agent_screening.processing.start_record is outside the R/23 registry.",
    call. = FALSE
  )
}

record_indices <- seq.int(
  start_record,
  end_record
)


# ============================================================
# 13. Agent screening loop
# ============================================================

cat("\n")
cat("============================================================\n")
cat("TASK-011 — Multimodal Full-Text Eligibility\n")
cat("============================================================\n\n")

cat("Project: ", project_title, "\n", sep = "")
cat("Provider: ", provider_name, "\n", sep = "")
cat("Model: ", model_name, "\n", sep = "")
cat("Agent reviewer: ", reviewer_name, "\n", sep = "")
cat("Final human reviewer: ", author_reviewer_name, "\n", sep = "")
cat("Registry records: ", nrow(registry), "\n", sep = "")
cat("Records scheduled: ", length(record_indices), "\n\n", sep = "")

processed_in_session <- 0L

for (row_index in record_indices) {
  record <- registry[
    row_index,
    ,
    drop = FALSE
  ]

  screening_id <- safe_chr(
    record$screening_id
  )

  if (screening_id %in% completed_ids) {
    cat(
      "Skipping checkpointed record: ",
      screening_id,
      "\n",
      sep = ""
    )
    next
  }

  pdf_status <- safe_chr(
    record$pdf_status
  )

  evidence_mode <- if (
    identical(
      pdf_status,
      "Available"
    )
  ) {
    "Full-text PDF"
  } else {
    "Metadata only"
  }

  pdf_path <- if (
    identical(
      evidence_mode,
      "Full-text PDF"
    )
  ) {
    safe_chr(
      record$pdf_path
    )
  } else {
    NULL
  }

  if (
    identical(
      evidence_mode,
      "Full-text PDF"
    ) &&
    (
      is.null(pdf_path) ||
      !nzchar(pdf_path) ||
      !file.exists(pdf_path)
    )
  ) {
    stop(
      "PDF status is Available but the PDF cannot be opened for screening_id: ",
      screening_id,
      call. = FALSE
    )
  }

  prompt <- build_full_text_prompt(
    record = record,
    evidence_mode = evidence_mode
  )

  success <- FALSE
  last_error <- ""

  for (attempt in seq_len(max_retries)) {
    started_at <- Sys.time()

    outcome <- tryCatch(
      {
        response <- call_openai_full_text_eligibility(
          prompt = prompt,
          pdf_path = pdf_path
        )

        validation_status <- validate_agent_result(
          result = response$parsed,
          expected_screening_id = screening_id,
          expected_evidence_mode = evidence_mode
        )

        if (!identical(validation_status, "valid")) {
          stop(
            "Agent result validation failed: ",
            validation_status,
            call. = FALSE
          )
        }

        finished_at <- Sys.time()
        result <- response$parsed

        current_row <- tibble::tibble(
          screening_id = screening_id,
          decision = safe_chr(result$decision),
          exclusion_reason_code = safe_chr(
            result$exclusion_reason_code
          ),
          exclusion_reason = safe_chr(
            result$exclusion_reason
          ),
          confidence = safe_chr(
            result$confidence
          ),
          evidence_source = safe_chr(
            result$evidence_source
          ),
          rationale = safe_chr(
            result$rationale
          ),
          eligibility_evidence = safe_chr(
            result$eligibility_evidence
          ),
          reviewer = reviewer_name,
          provider = provider_name,
          model = if (
            nzchar(
              safe_chr(response$model)
            )
          ) {
            safe_chr(response$model)
          } else {
            model_name
          },
          prompt_version = prompt_version,
          pdf_status = pdf_status,
          pdf_checksum_md5 = safe_chr(
            record$pdf_checksum_md5
          ),
          response_id = safe_chr(
            response$response_id
          ),
          attempt = as.character(
            attempt
          ),
          started_at = format(
            started_at,
            "%Y-%m-%d %H:%M:%S"
          ),
          completed_at = format(
            finished_at,
            "%Y-%m-%d %H:%M:%S"
          ),
          elapsed_seconds = as.character(
            round(
              as.numeric(
                difftime(
                  finished_at,
                  started_at,
                  units = "secs"
                )
              ),
              3
            )
          ),
          validation_status = "valid",
          raw_output_text = safe_chr(
            response$raw_output_text
          )
        )

        decision_log <- decision_log[
          decision_log$screening_id != screening_id,
          ,
          drop = FALSE
        ]

        decision_log <- dplyr::bind_rows(
          decision_log,
          current_row
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
          2 ^ (
            attempt - 1L
          ),
          10
        )
      )
    }
  }

  if (!success) {
    failure_log <- dplyr::bind_rows(
      failure_log,
      tibble::tibble(
        screening_id = screening_id,
        row_index = row_index,
        pdf_status = pdf_status,
        evidence_source = evidence_mode,
        provider = provider_name,
        model = model_name,
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

  } else {
    cat(
      "Completed: ",
      screening_id,
      " [",
      evidence_mode,
      "]\n",
      sep = ""
    )
  }

  processed_in_session <- processed_in_session + 1L

  if (
    checkpoint_every > 0L &&
    processed_in_session %% checkpoint_every == 0L
  ) {
    readr::write_csv(
      decision_log,
      checkpoint_file,
      na = ""
    )

    cat(
      "Checkpoint saved: ",
      nrow(decision_log),
      " valid decision(s).\n",
      sep = ""
    )
  }

  if (
    pause_seconds > 0
  ) {
    Sys.sleep(
      pause_seconds
    )
  }
}


# ============================================================
# 14. Save checkpoint and logs
# ============================================================

readr::write_csv(
  decision_log,
  checkpoint_file,
  na = ""
)

readr::write_csv(
  decision_log,
  decisions_file,
  na = ""
)

readr::write_csv(
  failure_log,
  failures_file,
  na = ""
)


# ============================================================
# 15. Validate agent-stage completeness
# ============================================================

valid_decisions <- decision_log[
  decision_log$validation_status == "valid",
  ,
  drop = FALSE
]

valid_ids <- unique(
  valid_decisions$screening_id
)

missing_agent_ids <- setdiff(
  registry$screening_id,
  valid_ids
)

if (length(missing_agent_ids) > 0L) {
  cat("\n")
  cat(
    "Agent eligibility is incomplete. Missing valid decision(s): ",
    length(missing_agent_ids),
    "\n",
    sep = ""
  )
}

if (nrow(failure_log) > 0L) {
  cat(
    "Unresolved agent failure(s): ",
    nrow(failure_log),
    "\n",
    sep = ""
  )
}


# ============================================================
# 16. Build 2-stage eligibility workbook data
# ============================================================

decision_index <- match(
  registry$screening_id,
  valid_decisions$screening_id
)

agent_decision <- rep(
  NA_character_,
  nrow(registry)
)

agent_reason_code <- rep(
  NA_character_,
  nrow(registry)
)

agent_reason <- rep(
  NA_character_,
  nrow(registry)
)

agent_confidence <- rep(
  NA_character_,
  nrow(registry)
)

agent_evidence_source <- rep(
  NA_character_,
  nrow(registry)
)

agent_rationale <- rep(
  NA_character_,
  nrow(registry)
)

agent_eligibility_evidence <- rep(
  NA_character_,
  nrow(registry)
)

agent_screened_at <- rep(
  NA_character_,
  nrow(registry)
)

matched_rows <- which(
  !is.na(decision_index)
)

if (length(matched_rows) > 0L) {

  matched_decisions <- valid_decisions[
    decision_index[matched_rows],
    ,
    drop = FALSE
  ]

  agent_decision[matched_rows] <-
    matched_decisions$decision

  agent_reason_code[matched_rows] <-
    matched_decisions$exclusion_reason_code

  agent_reason[matched_rows] <-
    matched_decisions$exclusion_reason

  agent_confidence[matched_rows] <-
    matched_decisions$confidence

  agent_evidence_source[matched_rows] <-
    matched_decisions$evidence_source

  agent_rationale[matched_rows] <-
    matched_decisions$rationale

  agent_eligibility_evidence[matched_rows] <-
    matched_decisions$eligibility_evidence

  agent_screened_at[matched_rows] <-
    matched_decisions$completed_at
}


agent_review <- tibble::tibble(
  screening_id = registry$screening_id,
  title = registry$title,
  authors = registry$authors,
  year = registry$year,
  journal = registry$journal,
  doi = registry$doi,
  pmid = registry$pmid,
  publication_type = registry$publication_type,
  language = registry$language,
  url = registry$url,
  pdf_status = registry$pdf_status,
  pdf_path = registry$pdf_path,
  agent_reviewer = reviewer_name,
  agent_decision = agent_decision,
  agent_exclusion_reason_code = agent_reason_code,
  agent_exclusion_reason = agent_reason,
  agent_confidence = agent_confidence,
  agent_evidence_source = agent_evidence_source,
  agent_eligibility_evidence = agent_eligibility_evidence,
  agent_rationale = agent_rationale,
  agent_screened_at = agent_screened_at
)


author_final_review <- tibble::tibble(
  screening_id = registry$screening_id,
  title = registry$title,
  authors = registry$authors,
  year = registry$year,
  journal = registry$journal,
  doi = registry$doi,
  pmid = registry$pmid,
  publication_type = registry$publication_type,
  language = registry$language,
  url = registry$url,
  pdf_status = registry$pdf_status,
  pdf_path = registry$pdf_path,

  agent_decision = agent_decision,
  agent_exclusion_reason = agent_reason,
  agent_confidence = agent_confidence,
  agent_evidence_source = agent_evidence_source,
  agent_eligibility_evidence = agent_eligibility_evidence,
  agent_rationale = agent_rationale,

  author_reviewer = author_reviewer_name,
  author_final_decision = NA_character_,
  author_final_exclusion_reason = NA_character_,
  author_notes = NA_character_,
  author_reviewed_at = NA_character_
)


# ============================================================
# 16.1 Two-stage workbook contract validation
# ============================================================

if (nrow(agent_review) != nrow(registry)) {
  stop(
    "Agent_Review row count does not match the TASK-010 registry.",
    call. = FALSE
  )
}

if (nrow(author_final_review) != nrow(registry)) {
  stop(
    "Author_Final_Review row count does not match the TASK-010 registry.",
    call. = FALSE
  )
}

if (
  !identical(
    as.character(agent_review$screening_id),
    as.character(author_final_review$screening_id)
  )
) {
  stop(
    "Agent_Review and Author_Final_Review screening IDs are not aligned.",
    call. = FALSE
  )
}

# ============================================================
# 17. Build workbook
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


# ------------------------------------------------------------
# Instructions
# ------------------------------------------------------------

openxlsx::addWorksheet(
  wb,
  "Instructions"
)

instructions <- tibble::tibble(
  Section = c(
    "Architecture",
    "Stage 1 — Reviewer Agent",
    "Stage 2 — 1st Author Final Review",
    "Authoritative decision",
    "PDF evidence",
    "Missing PDF",
    "Agent decision options",
    "Author decision options",
    "Author Exclude",
    "Agent Human Review Required",
    "Next stage"
  ),
  Instruction = c(
    "This workbook uses a 2-stage full-text eligibility architecture: Reviewer Agent followed by 1st Author Final Review.",
    "R/23 populates Agent_Review using the full-text PDF when available and metadata-only assessment otherwise.",
    "The 1st author reviews the agent assessment and directly enters the final Include/Exclude decision in Author_Final_Review.",
    "The 1st author's author_final_decision is authoritative. There is no second reviewer conflict/adjudication stage.",
    "When pdf_status is Available, the PDF is the primary evidence source for the Reviewer Agent.",
    "Missing/Invalid PDF is not an automatic exclusion. The agent should use Human Review Required unless metadata clearly proves exclusion.",
    "Include / Exclude / Human Review Required",
    "Include / Exclude",
    "Every author_final_decision = Exclude must use exactly one canonical exclusion reason from project_config.yml.",
    "If the agent selects Human Review Required, the 1st author resolves the case directly as Include or Exclude.",
    "TASK-012 validates completed author decisions and creates the authoritative Included Studies dataset."
  )
)

openxlsx::writeData(
  wb,
  "Instructions",
  paste0(
    project_title,
    " — Full-Text Eligibility"
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
  widths = 25
)

openxlsx::setColWidths(
  wb,
  "Instructions",
  cols = 2,
  widths = 95
)


write_table_sheet <- function(
    workbook,
    sheet_name,
    title,
    data
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

  if (ncol(data) > 1L) {
    openxlsx::mergeCells(
      workbook,
      sheet_name,
      cols = 1:ncol(data),
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

  openxlsx::addStyle(
    workbook,
    sheet_name,
    header_style,
    rows = 3,
    cols = seq_len(ncol(data)),
    gridExpand = TRUE
  )

  if (nrow(data) > 0L) {
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

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )

  if ("title" %in% names(data)) {
    openxlsx::setColWidths(
      workbook,
      sheet_name,
      cols = which(names(data) == "title"),
      widths = 55
    )
  }

  for (
    wide_col in intersect(
      c(
        "pdf_path",
        "eligibility_evidence",
        "rationale",
        "notes",
        "resolution_notes"
      ),
      names(data)
    )
  ) {
    openxlsx::setColWidths(
      workbook,
      sheet_name,
      cols = which(names(data) == wide_col),
      widths = 55
    )
  }
}


write_table_sheet(
  wb,
  "Agent_Review",
  paste0(
    project_title,
    " — Reviewer Agent Full-Text Eligibility"
  ),
  agent_review
)

write_table_sheet(
  wb,
  "Author_Final_Review",
  paste0(
    project_title,
    " — 1st Author Final Full-Text Review"
  ),
  author_final_review
)


# ------------------------------------------------------------
# Validation lists
# ------------------------------------------------------------

openxlsx::addWorksheet(
  wb,
  "Validation_Lists"
)

validation_decisions <- tibble::tibble(
  decision = decision_options
)

validation_confidence <- tibble::tibble(
  confidence = confidence_options
)

validation_reasons <- exclusion_reason_table |>
  dplyr::select(
    code,
    label
  )

openxlsx::writeData(
  wb,
  "Validation_Lists",
  validation_decisions,
  startRow = 1,
  startCol = 1
)

openxlsx::writeData(
  wb,
  "Validation_Lists",
  validation_confidence,
  startRow = 1,
  startCol = 3
)

openxlsx::writeData(
  wb,
  "Validation_Lists",
  validation_reasons,
  startRow = 1,
  startCol = 5
)


# ------------------------------------------------------------
# 1st Author Final Review data validation
# ------------------------------------------------------------

author_decision_options <- c(
  "Include",
  "Exclude"
)

if (nrow(author_final_review) > 0L) {

  author_decision_col <- which(
    names(author_final_review) == "author_final_decision"
  )

  author_reason_col <- which(
    names(author_final_review) == "author_final_exclusion_reason"
  )

  openxlsx::writeData(
    wb,
    "Validation_Lists",
    tibble::tibble(
      author_final_decision = author_decision_options
    ),
    startRow = 1,
    startCol = 8
  )

  author_decision_end_row <- 1L + length(
    author_decision_options
  )

  reason_end_row <- 1L + nrow(
    validation_reasons
  )

  openxlsx::dataValidation(
    wb,
    "Author_Final_Review",
    cols = author_decision_col,
    rows = 4:(nrow(author_final_review) + 3),
    type = "list",
    value = paste0(
      "'Validation_Lists'!$H$2:$H$",
      author_decision_end_row
    )
  )

  openxlsx::dataValidation(
    wb,
    "Author_Final_Review",
    cols = author_reason_col,
    rows = 4:(nrow(author_final_review) + 3),
    type = "list",
    value = paste0(
      "'Validation_Lists'!$F$2:$F$",
      reason_end_row
    )
  )
}


# ============================================================
# 18. Save workbook and validate stage artifacts
# ============================================================

openxlsx::saveWorkbook(
  wb,
  output_workbook,
  overwrite = TRUE
)

required_outputs <- c(
  workbook = output_workbook,
  decisions = decisions_file,
  failures = failures_file,
  checkpoint = checkpoint_file
)

output_ok <- vapply(
  required_outputs,
  function(file_path) {
    file.exists(file_path) &&
      !is.na(file.info(file_path)$size) &&
      file.info(file_path)$size >= 0
  },
  logical(1)
)

if (!all(output_ok)) {
  stop(
    "R/23 failed to create required artifact(s): ",
    paste(
      names(required_outputs)[!output_ok],
      collapse = ", "
    ),
    call. = FALSE
  )
}

saved_sheets <- openxlsx::getSheetNames(
  output_workbook
)

required_sheets <- c(
  "Instructions",
  "Agent_Review",
  "Author_Final_Review",
  "Validation_Lists"
)

missing_sheets <- setdiff(
  required_sheets,
  saved_sheets
)

if (length(missing_sheets) > 0L) {
  stop(
    "Saved R/23 workbook is missing required sheet(s): ",
    paste(
      missing_sheets,
      collapse = ", "
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 18.1 Validate 2-stage workbook contract after save
# ------------------------------------------------------------

saved_agent_review <- openxlsx::readWorkbook(
  output_workbook,
  sheet = "Agent_Review",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

saved_author_review <- openxlsx::readWorkbook(
  output_workbook,
  sheet = "Author_Final_Review",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

if (nrow(saved_agent_review) != nrow(agent_review)) {
  stop(
    "Saved Agent_Review row count does not match the authoritative agent dataset.",
    call. = FALSE
  )
}

if (nrow(saved_author_review) != nrow(author_final_review)) {
  stop(
    "Saved Author_Final_Review row count does not match the authoritative author-review dataset.",
    call. = FALSE
  )
}

if (
  !"screening_id" %in% names(saved_agent_review) ||
  !"screening_id" %in% names(saved_author_review)
) {
  stop(
    "Saved full-text eligibility workbook is missing screening_id in a required sheet.",
    call. = FALSE
  )
}

if (
  !identical(
    as.character(saved_agent_review$screening_id),
    as.character(agent_review$screening_id)
  ) ||
  !identical(
    as.character(saved_author_review$screening_id),
    as.character(author_final_review$screening_id)
  )
) {
  stop(
    "Saved 2-stage workbook screening IDs do not match the authoritative registry.",
    call. = FALSE
  )
}

author_final_fields <- c(
  "author_final_decision",
  "author_final_exclusion_reason",
  "author_notes",
  "author_reviewed_at"
)

missing_author_fields <- setdiff(
  author_final_fields,
  names(saved_author_review)
)

if (length(missing_author_fields) > 0L) {
  stop(
    "Saved Author_Final_Review is missing required author field(s): ",
    paste(missing_author_fields, collapse = ", "),
    call. = FALSE
  )
}

# R/23 must leave the final-author fields untouched.
for (field_name in author_final_fields) {
  field_values <- clean_text(
    saved_author_review[[field_name]]
  )

  if (any(!is.na(field_values))) {
    stop(
      "R/23 populated an author-only field unexpectedly: ",
      field_name,
      call. = FALSE
    )
  }
}


# ============================================================
# 19. Final stage report
# ============================================================

include_count <- sum(
  valid_decisions$decision == "Include"
)

exclude_count <- sum(
  valid_decisions$decision == "Exclude"
)

human_review_count <- sum(
  valid_decisions$decision == "Human Review Required"
)

pdf_based_count <- sum(
  valid_decisions$evidence_source == "Full-text PDF"
)

metadata_based_count <- sum(
  valid_decisions$evidence_source == "Metadata only"
)

stage_complete <- (
  length(missing_agent_ids) == 0L &&
    nrow(failure_log) == 0L &&
    nrow(valid_decisions) == nrow(registry)
)

stage_status <- if (
  stage_complete
) {
  "PASS"
} else {
  "INCOMPLETE"
}

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "TASK-011 — Multimodal Full-Text Eligibility",
  "",
  paste0("Generated at: ", now_text()),
  paste0("Project ID: ", project_id),
  paste0("Project title: ", project_title),
  "",
  "CANONICAL INPUT",
  "--------------------------------------------------",
  paste0("Full-text registry: ", registry_csv),
  paste0("Registry records: ", nrow(registry)),
  "",
  "AGENT REVIEW",
  "--------------------------------------------------",
  paste0("Agent Reviewer 1: ", reviewer_name),
  paste0("Provider: ", provider_name),
  paste0("Model: ", model_name),
  paste0("Valid decisions: ", nrow(valid_decisions)),
  paste0("Include: ", include_count),
  paste0("Exclude: ", exclude_count),
  paste0("Human Review Required: ", human_review_count),
  paste0("Full-text PDF evidence: ", pdf_based_count),
  paste0("Metadata-only evidence: ", metadata_based_count),
  paste0("Unresolved failures: ", nrow(failure_log)),
  paste0("Missing agent decisions: ", length(missing_agent_ids)),
  "",
  "TWO-STAGE ROLE PRESERVATION",
  "--------------------------------------------------",
  paste0("Final human reviewer: ", author_reviewer_name),
  "Author final decision fields populated by R/23: NO",
  "Second independent human reviewer used: NO",
  "Conflict/adjudication stage used: NO",
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0("Eligibility workbook: ", output_workbook),
  paste0("Decision log: ", decisions_file),
  paste0("Failure log: ", failures_file),
  paste0("Checkpoint: ", checkpoint_file),
  paste0("Report: ", report_file),
  "",
  "STAGE BOUNDARY",
  "--------------------------------------------------",
  "Authoritative Included Studies created: NO",
  "Runner integration performed: NO",
  "Next action after PASS: 1st Author completes Author_Final_Review",
  "Next script after author review: TASK-012 — Final Included Studies",
  "",
  "TASK STATUS",
  "--------------------------------------------------",
  paste0("TASK-011 Agent Stage: ", stage_status)
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
    "R/23 report was not created successfully.",
    call. = FALSE
  )
}


# ============================================================
# 20. Console completion
# ============================================================

cat("\n")
cat("============================================================\n")
cat("R/23 Full-Text Eligibility Agent Stage Summary\n")
cat("============================================================\n\n")

cat("Registry records: ", nrow(registry), "\n", sep = "")
cat("Valid agent decisions: ", nrow(valid_decisions), "\n", sep = "")
cat("Include: ", include_count, "\n", sep = "")
cat("Exclude: ", exclude_count, "\n", sep = "")
cat("Human Review Required: ", human_review_count, "\n", sep = "")
cat("Full-text PDF evidence: ", pdf_based_count, "\n", sep = "")
cat("Metadata-only evidence: ", metadata_based_count, "\n", sep = "")
cat("Failures: ", nrow(failure_log), "\n\n", sep = "")

cat("Workbook:\n- ", output_workbook, "\n", sep = "")
cat("Decision log:\n- ", decisions_file, "\n", sep = "")
cat("Report:\n- ", report_file, "\n\n", sep = "")

if (stage_complete) {
  cat(
    "PASS: R/23 multimodal full-text eligibility Agent Reviewer 1 completed successfully.\n"
  )

  cat(
    "Next action: the 1st author reviews Agent_Review and completes the ",
    "Author_Final_Review sheet.\n",
    sep = ""
  )

  cat(
    "The author's final Include/Exclude decision will be authoritative.\n"
  )

  cat(
    "Do NOT create Included Studies until TASK-012 finalization.\n"
  )

} else {
  cat(
    "R/23 STATUS: INCOMPLETE\n"
  )

  cat(
    "Resolve the failure log or missing agent decisions, then rerun R/23.\n"
  )
}


invisible(
  list(
    status = stage_status,
    registry = registry,
    decisions = valid_decisions,
    failures = failure_log,
    workbook = output_workbook,
    decisions_file = decisions_file,
    failures_file = failures_file,
    checkpoint_file = checkpoint_file,
    report_file = report_file
  )
)
