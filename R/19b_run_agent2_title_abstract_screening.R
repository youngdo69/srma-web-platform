# ============================================================
# Universal SR-MA Agent Toolkit
# R/19b_run_agent2_title_abstract_screening.R
#
# Purpose
# ------------------------------------------------------------
# 1. Confirm Reviewer Agent 1 has completed title/abstract screening.
# 2. Cascade-accept only Reviewer Agent 1 HIGH-confidence Exclude without an API call.
# 3. Send Reviewer Agent 1 Moderate/Low-confidence Exclude plus
#    Include/Uncertainty records to Reviewer Agent 2.
# 4. Never expose Reviewer Agent 1 decisions to Reviewer Agent 2 prompts.
# 5. Support checkpoint/resume with a separate Reviewer 2 audit trail.
# 6. Compare only the API-reviewed subset for disagreement/conflict.
# 7. Preserve downstream R/21 compatibility by filling Reviewer_2 for all rows.
# 8. Preserve all human adjudication/final-decision fields untouched.
#
# Universal-toolkit policy
# ------------------------------------------------------------
# - Topic-specific logic is read from canonical project configuration.
# - Reviewer Agent 1 HIGH-confidence Exclude is carried forward without
#   Reviewer Agent 2 API review.
# - Reviewer Agent 2 independently re-reviews Reviewer Agent 1
#   Moderate/Low-confidence Exclude, Include, and Uncertainty records
#   without seeing Reviewer Agent 1 decisions.
# - Human adjudication is reserved only for mismatches within that routed subset.
# - This script is standalone-validatable before Runner integration.
# ============================================================


# ============================================================
# 1. Required packages
# ============================================================

required_packages <- c(
  "yaml",
  "openxlsx",
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "jsonlite",
  "httr2",
  "dotenv"
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
    "\nRun source(\"START_HERE.R\") to restore the toolkit environment.",
    call. = FALSE
  )
}


# ============================================================
# 2. Canonical utilities and helpers
# ============================================================

source("R/utils/config_utils.R")
source("R/utils/path_utils.R")
source("R/utils/screening_prompt_utils.R")
source("R/utils/llm_provider_utils.R")

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) {
    if (
      is.null(x) ||
      length(x) == 0L ||
      all(is.na(x))
    ) {
      y
    } else {
      x
    }
  }
}

safe_chr <- function(x) {
  if (
    is.null(x) ||
    length(x) == 0L ||
    is.na(x[[1]])
  ) {
    return("")
  }

  trimws(as.character(x[[1]]))
}

clean_chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

now_text <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

is_nonretryable_api_billing_error <- function(message) {
  message <- tolower(
    paste(
      as.character(message),
      collapse = " "
    )
  )

  patterns <- c(
    "credit_balance_exhausted",
    "insufficient_quota",
    "no credits remaining",
    "organization_usage_limit_exceeded",
    "organization_spend_limit_exceeded",
    "project_spend_limit_exceeded"
  )

  any(
    vapply(
      patterns,
      function(pattern) {
        grepl(
          pattern,
          message,
          fixed = TRUE
        )
      },
      logical(1)
    )
  )
}


estimate_text_tokens <- function(
    text,
    chars_per_token = 4
) {
  if (
    is.null(text) ||
    length(text) == 0L
  ) {
    return(0L)
  }

  text <- paste(
    as.character(text),
    collapse = "\n"
  )

  if (!nzchar(text)) {
    return(0L)
  }

  max(
    1L,
    as.integer(
      ceiling(
        nchar(
          text,
          type = "chars",
          allowNA = TRUE,
          keepNA = FALSE
        ) / chars_per_token
      )
    )
  )
}


get_builtin_text_token_pricing <- function(model_name) {
  model_key <- tolower(
    trimws(
      as.character(model_name)
    )
  )

  # Built-in fallback snapshot:
  # OpenAI public API pricing checked 2026-09-20.
  # Config overrides take precedence over this table.
  pricing <- list(
    "gpt-5" = c(input = 1.25, output = 10.00),
    "gpt-5-chat-latest" = c(input = 1.25, output = 10.00),
    "gpt-5.1" = c(input = 1.25, output = 10.00),
    "gpt-5.1-chat-latest" = c(input = 1.25, output = 10.00),
    "gpt-5.2" = c(input = 1.75, output = 14.00),
    "gpt-5.2-chat-latest" = c(input = 1.75, output = 14.00),
    "gpt-5.4" = c(input = 2.50, output = 15.00),
    "gpt-5.6" = c(input = 4.00, output = 20.00),
    "gpt-5.6-sol" = c(input = 4.00, output = 20.00),
    "gpt-5.6-terra" = c(input = 2.00, output = 12.00),
    "gpt-5.6-luna" = c(input = 0.20, output = 1.20)
  )

  if (!model_key %in% names(pricing)) {
    return(NULL)
  }

  list(
    input_usd_per_million = unname(
      pricing[[model_key]][["input"]]
    ),
    output_usd_per_million = unname(
      pricing[[model_key]][["output"]]
    ),
    source = "built_in_snapshot_2026-09-20"
  )
}


# ============================================================
# 3. Project and agent configuration
# ============================================================

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

agent_config_file <- "config/agent_screening.yml"

if (!file.exists(agent_config_file)) {
  stop(
    "Agent-screening config not found: ",
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

project_id <- get_project_id(config)
project_title <- get_project_title(config)

decision_options <- get_screening_decisions(config)
exclusion_reason_table <- get_exclusion_reasons(config)

review_question <-
  config$review$question %||%
  config$eligibility$review_question %||%
  config$project$review_question %||%
  "Not explicitly specified in project_config.yml"

inclusion_criteria <-
  config$eligibility$inclusion %||%
  config$screening$inclusion_criteria %||%
  config$review$inclusion_criteria %||%
  "Apply the finalized review protocol."


# ============================================================
# 4. Reviewer Agent 2 configuration
# ============================================================
#
# Optional future config extension:
#
# agent_screening:
#   reviewer_2:
#     name: "Agent Reviewer 2"
#     provider:
#       name: openai
#       model: <optional different model>
#
# If reviewer_2/provider is absent, the already validated Reviewer 1
# provider/model configuration is reused. Independence is maintained
# because Reviewer 1 decisions are never included in Reviewer 2 prompts.
# ============================================================

reviewer_2_config <-
  agent_config$reviewer_2 %||%
  agent_config$second_reviewer %||%
  list()

provider_config <-
  reviewer_2_config$provider %||%
  agent_config$provider

provider_config$timeout_seconds <-
  reviewer_2_config$processing$timeout_seconds %||%
  agent_config$processing$timeout_seconds %||%
  120

provider_name <- safe_chr(provider_config$name)
model_name <- safe_chr(provider_config$model)

if (!nzchar(provider_name)) {
  stop(
    "Reviewer Agent 2 provider name is empty.",
    call. = FALSE
  )
}

if (!nzchar(model_name)) {
  stop(
    "Reviewer Agent 2 model name is empty.",
    call. = FALSE
  )
}

reviewer_name <-
  safe_chr(reviewer_2_config$name)

if (!nzchar(reviewer_name)) {
  reviewer_name <- "Agent Reviewer 2"
}

prompt_version <- paste0(
  safe_chr(agent_config$prompt$version %||% "unknown"),
  "_reviewer2"
)

cascade_reviewer_label <- "Cascade Policy — Reviewer 1 High-confidence Exclude"
cascade_note <- paste(
  "[Cascade policy]",
  "Reviewer Agent 1 high-confidence exclusion accepted without Reviewer Agent 2 API review."
)

# Optional universal override:
#
# agent_screening:
#   cost_estimation:
#     enabled: true
#     input_usd_per_million: 1.25
#     output_usd_per_million: 10
#     chars_per_token: 4
#     assumed_output_tokens_per_record: 180
#     request_overhead_tokens: 120
#     contingency_fraction: 0.25
#
# reviewer_2.cost_estimation can override the shared block.
cost_config <-
  reviewer_2_config$cost_estimation %||%
  agent_config$cost_estimation %||%
  list()

cost_estimation_enabled <- !identical(
  cost_config$enabled,
  FALSE
)

chars_per_token <- as.numeric(
  cost_config$chars_per_token %||%
  4
)

if (
  !is.finite(chars_per_token) ||
  chars_per_token <= 0
) {
  chars_per_token <- 4
}

assumed_output_tokens_per_record <- as.integer(
  cost_config$assumed_output_tokens_per_record %||%
  180L
)

if (
  is.na(assumed_output_tokens_per_record) ||
  assumed_output_tokens_per_record < 1L
) {
  assumed_output_tokens_per_record <- 180L
}

request_overhead_tokens <- as.integer(
  cost_config$request_overhead_tokens %||%
  120L
)

if (
  is.na(request_overhead_tokens) ||
  request_overhead_tokens < 0L
) {
  request_overhead_tokens <- 120L
}

contingency_fraction <- as.numeric(
  cost_config$contingency_fraction %||%
  0.25
)

if (
  !is.finite(contingency_fraction) ||
  contingency_fraction < 0
) {
  contingency_fraction <- 0.25
}

pricing_info <- NULL

config_input_rate <- suppressWarnings(
  as.numeric(
    cost_config$input_usd_per_million %||%
    NA_real_
  )
)

config_output_rate <- suppressWarnings(
  as.numeric(
    cost_config$output_usd_per_million %||%
    NA_real_
  )
)

if (
  is.finite(config_input_rate) &&
  config_input_rate >= 0 &&
  is.finite(config_output_rate) &&
  config_output_rate >= 0
) {
  pricing_info <- list(
    input_usd_per_million = config_input_rate,
    output_usd_per_million = config_output_rate,
    source = "config_override"
  )
} else {
  pricing_info <- get_builtin_text_token_pricing(
    model_name
  )
}

prompt_template_file <- agent_config$prompt$template_file

if (!file.exists(prompt_template_file)) {
  stop(
    "Prompt template not found: ",
    prompt_template_file,
    call. = FALSE
  )
}

prompt_template <- paste(
  readLines(
    prompt_template_file,
    warn = FALSE,
    encoding = "UTF-8"
  ),
  collapse = "\n"
)


# ============================================================
# 5. Canonical Phase-1 paths
# ============================================================

screening_output_directory <- paths$screening_results_directory
logs_output_directory <- paths$logs_directory

output_workbook <- file.path(
  screening_output_directory,
  paste0(
    agent_config$output$workbook_basename,
    ".xlsx"
  )
)

if (!file.exists(output_workbook)) {
  stop(
    "Reviewer Agent 1 output workbook not found:\n",
    output_workbook,
    "\nRun R/19_run_agent_title_abstract_screening.R first.",
    call. = FALSE
  )
}

base_decisions_name <-
  agent_config$output$decisions_basename %||%
  "title_abstract_agent_decisions"

base_failures_name <-
  agent_config$output$failures_basename %||%
  "title_abstract_agent_failures"

base_checkpoint_name <-
  agent_config$output$checkpoint_basename %||%
  "title_abstract_agent_checkpoint"

base_report_name <-
  agent_config$output$report_basename %||%
  "title_abstract_agent_report"

decisions_file <- file.path(
  logs_output_directory,
  paste0(base_decisions_name, "_reviewer2.csv")
)

failures_file <- file.path(
  logs_output_directory,
  paste0(base_failures_name, "_reviewer2.csv")
)

checkpoint_file <- file.path(
  logs_output_directory,
  paste0(base_checkpoint_name, "_reviewer2.csv")
)

report_file <- file.path(
  logs_output_directory,
  paste0(base_report_name, "_reviewer2.txt")
)

conflicts_file <- file.path(
  logs_output_directory,
  paste0(base_decisions_name, "_agent_conflicts.csv")
)


# ============================================================
# 6. Read workbook contracts
# ============================================================

reviewer_1 <- openxlsx::readWorkbook(
  output_workbook,
  sheet = "Reviewer_1",
  startRow = 4,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

reviewer_2 <- openxlsx::readWorkbook(
  output_workbook,
  sheet = "Reviewer_2",
  startRow = 4,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

conflict_data <- openxlsx::readWorkbook(
  output_workbook,
  sheet = "Conflict_Resolution",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

required_reviewer_columns <- c(
  "screening_id",
  "title",
  "abstract",
  "authors",
  "year",
  "journal",
  "publication_type",
  "language",
  "reviewer",
  "decision",
  "exclusion_reason",
  "confidence",
  "notes",
  "screened_at"
)

missing_r1 <- setdiff(
  required_reviewer_columns,
  names(reviewer_1)
)

missing_r2 <- setdiff(
  required_reviewer_columns,
  names(reviewer_2)
)

if (length(missing_r1) > 0L) {
  stop(
    "Reviewer_1 sheet is missing columns:\n- ",
    paste(missing_r1, collapse = "\n- "),
    call. = FALSE
  )
}

if (length(missing_r2) > 0L) {
  stop(
    "Reviewer_2 sheet is missing columns:\n- ",
    paste(missing_r2, collapse = "\n- "),
    call. = FALSE
  )
}

required_conflict_columns <- c(
  "screening_id",
  "reviewer_1_decision",
  "reviewer_2_decision",
  "agreement_status",
  "conflict_status",
  "final_decision",
  "final_exclusion_reason",
  "adjudicator",
  "resolution_notes",
  "resolved_at"
)

missing_conflict_columns <- setdiff(
  required_conflict_columns,
  names(conflict_data)
)

if (length(missing_conflict_columns) > 0L) {
  stop(
    "Conflict_Resolution sheet is missing columns:\n- ",
    paste(missing_conflict_columns, collapse = "\n- "),
    call. = FALSE
  )
}

if (nrow(reviewer_1) != nrow(reviewer_2)) {
  stop(
    "Reviewer_1 and Reviewer_2 row counts do not match.",
    call. = FALSE
  )
}

if (
  !identical(
    clean_chr(reviewer_1$screening_id),
    clean_chr(reviewer_2$screening_id)
  )
) {
  stop(
    "Reviewer_1 and Reviewer_2 screening_id order does not match.",
    call. = FALSE
  )
}

if (
  nrow(conflict_data) != nrow(reviewer_1) ||
  !identical(
    clean_chr(conflict_data$screening_id),
    clean_chr(reviewer_1$screening_id)
  )
) {
  stop(
    "Conflict_Resolution screening_id contract does not match reviewer sheets.",
    call. = FALSE
  )
}


# ============================================================
# 7. Verify Reviewer Agent 1 is complete
# ============================================================

r1_decisions <- clean_chr(reviewer_1$decision)
r1_reasons <- clean_chr(reviewer_1$exclusion_reason)

r1_missing <- which(!nzchar(r1_decisions))

if (length(r1_missing) > 0L) {
  stop(
    "Reviewer Agent 1 is incomplete: ",
    length(r1_missing),
    " decision(s) are blank.",
    call. = FALSE
  )
}

r1_invalid_decisions <- which(
  !r1_decisions %in% decision_options
)

if (length(r1_invalid_decisions) > 0L) {
  stop(
    "Reviewer Agent 1 contains invalid decision values.",
    call. = FALSE
  )
}

r1_exclude <- r1_decisions == "Exclude"

if (
  any(
    r1_exclude &
      !r1_reasons %in% exclusion_reason_table$label
  )
) {
  stop(
    "Reviewer Agent 1 contains invalid/missing exclusion reasons.",
    call. = FALSE
  )
}

if (
  any(
    !r1_exclude &
      nzchar(r1_reasons)
  )
) {
  stop(
    "Reviewer Agent 1 contains exclusion reasons on non-Exclude rows.",
    call. = FALSE
  )
}


# ============================================================
# 7b. Reviewer Agent 1 decision summary for console UX
# ============================================================

uncertainty_options <- setdiff(
  decision_options,
  c("Include", "Exclude")
)

uncertainty_label <- if (
  length(uncertainty_options) == 1L
) {
  uncertainty_options[[1]]
} else if (
  "Unclear" %in% decision_options
) {
  "Unclear"
} else {
  paste(
    uncertainty_options,
    collapse = " / "
  )
}

r1_include_count <- sum(
  r1_decisions == "Include"
)

r1_exclude_count <- sum(
  r1_decisions == "Exclude"
)

r1_unclear_count <- sum(
  r1_decisions %in% uncertainty_options
)

# Fixed cascade policy:
# - Reviewer Agent 1 Exclude + High confidence -> cascade auto-exclude.
# - Reviewer Agent 1 Exclude + Moderate/Low confidence -> Reviewer Agent 2.
# - Reviewer Agent 1 Include/Uncertainty -> Reviewer Agent 2.
# - Missing/unrecognized confidence on an Exclude is routed to Agent 2
#   rather than silently auto-excluded.
r1_confidence <- clean_chr(
  reviewer_1$confidence
)

r1_high_confidence <- (
  tolower(r1_confidence) == "high"
)

cascade_auto_exclude_rows <- which(
  r1_decisions == "Exclude" &
    r1_high_confidence
)

agent2_route_rows <- which(
  !(r1_decisions == "Exclude" &
      r1_high_confidence)
)

cascade_auto_exclude_ids <- clean_chr(
  reviewer_1$screening_id[
    cascade_auto_exclude_rows
  ]
)

agent2_route_ids <- clean_chr(
  reviewer_1$screening_id[
    agent2_route_rows
  ]
)

cascade_auto_exclude_count <- length(
  cascade_auto_exclude_rows
)

routed_nonhigh_exclude_rows <- which(
  r1_decisions == "Exclude" &
    !r1_high_confidence
)

routed_nonhigh_exclude_count <- length(
  routed_nonhigh_exclude_rows
)

routed_include_count <- sum(
  r1_decisions[agent2_route_rows] == "Include"
)

routed_uncertainty_count <- sum(
  r1_decisions[agent2_route_rows] %in% uncertainty_options
)

agent2_route_count <- length(
  agent2_route_rows
)


# ============================================================
# 8. Protect pre-existing human decisions
# ============================================================

existing_r2_decisions <- clean_chr(reviewer_2$decision)
existing_r2_reviewer <- clean_chr(reviewer_2$reviewer)

recognized_automated_r2 <- (
  grepl(
    "^Agent Reviewer 2",
    existing_r2_reviewer,
    ignore.case = TRUE
  ) |
  existing_r2_reviewer == cascade_reviewer_label
)

manual_r2_rows <- which(
  nzchar(existing_r2_decisions) &
    !recognized_automated_r2
)

if (length(manual_r2_rows) > 0L) {
  stop(
    "Reviewer_2 already contains ",
    length(manual_r2_rows),
    " non-toolkit decision(s). ",
    "The toolkit will not overwrite possible human work automatically.",
    call. = FALSE
  )
}

human_decision_fields <- c(
  "final_decision",
  "final_exclusion_reason",
  "resolution_notes",
  "resolved_at"
)

for (field_name in human_decision_fields) {
  values <- clean_chr(
    conflict_data[[field_name]]
  )

  if (any(nzchar(values))) {
    stop(
      "Conflict_Resolution already contains human adjudication data in field: ",
      field_name,
      ". Reviewer Agent 2 will not overwrite an adjudicated workbook.",
      call. = FALSE
    )
  }
}

# The adjudicator column may contain a prefilled role/name placeholder.
# A nonblank adjudicator value alone is NOT treated as completed human
# adjudication unless one of the actual decision/resolution fields above
# is also populated. Preserve the adjudicator column exactly as found.
adjudicator_before <- clean_chr(
  conflict_data$adjudicator
)


# ============================================================
# 9. Reviewer 2 result validation
# ============================================================

json_schema <- build_screening_json_schema(
  decision_options = decision_options,
  exclusion_reason_codes = exclusion_reason_table$code,
  exclusion_reason_labels = exclusion_reason_table$label,
  confidence_options = agent_config$decision_policy$allowed_confidence
)

validate_agent_result <- function(
    result,
    expected_screening_id
) {
  required <- c(
    "screening_id",
    "decision",
    "exclusion_reason_code",
    "exclusion_reason",
    "confidence",
    "rationale"
  )

  if (!all(required %in% names(result))) {
    return("missing_required_fields")
  }

  result_screening_id <- safe_chr(
    result$screening_id
  )

  result_decision <- safe_chr(
    result$decision
  )

  result_reason_code <- safe_chr(
    result$exclusion_reason_code
  )

  result_reason <- safe_chr(
    result$exclusion_reason
  )

  result_confidence <- safe_chr(
    result$confidence
  )

  result_rationale <- safe_chr(
    result$rationale
  )

  if (!identical(
    result_screening_id,
    expected_screening_id
  )) {
    return("screening_id_mismatch")
  }

  if (!result_decision %in% decision_options) {
    return("invalid_decision")
  }

  if (
    !result_confidence %in%
      agent_config$decision_policy$allowed_confidence
  ) {
    return("invalid_confidence")
  }

  if (identical(result_decision, "Exclude")) {
    matching_reason <- exclusion_reason_table[
      exclusion_reason_table$code == result_reason_code &
        exclusion_reason_table$label == result_reason,
      ,
      drop = FALSE
    ]

    if (nrow(matching_reason) != 1L) {
      return("invalid_exclusion_reason")
    }
  } else {
    if (
      nzchar(result_reason_code) ||
      nzchar(result_reason)
    ) {
      return("reason_present_for_nonexclude")
    }
  }

  if (!nzchar(result_rationale)) {
    return("missing_rationale")
  }

  "valid"
}


# ============================================================
# 10. Processing controls
# ============================================================

start_record <- as.integer(
  reviewer_2_config$processing$start_record %||%
  agent_config$processing$start_record %||%
  1L
)

max_records <-
  reviewer_2_config$processing$max_records %||%
  agent_config$processing$max_records

end_record <- nrow(reviewer_2)

if (
  !is.null(max_records) &&
  length(max_records) > 0L &&
  !is.na(max_records)
) {
  end_record <- min(
    end_record,
    start_record + as.integer(max_records) - 1L
  )
}

if (
  start_record < 1L ||
  start_record > nrow(reviewer_2)
) {
  stop(
    "Reviewer Agent 2 start_record is outside the workbook.",
    call. = FALSE
  )
}

record_indices <- seq.int(
  start_record,
  end_record
)

resume_enabled <- isTRUE(
  reviewer_2_config$processing$resume_from_checkpoint %||%
  agent_config$processing$resume_from_checkpoint
)

checkpoint_every <- as.integer(
  reviewer_2_config$processing$checkpoint_every %||%
  agent_config$processing$checkpoint_every %||%
  10L
)

max_retries <- as.integer(
  reviewer_2_config$processing$max_retries %||%
  agent_config$processing$max_retries %||%
  3L
)

pause_seconds <- as.numeric(
  reviewer_2_config$processing$request_pause_seconds %||%
  agent_config$processing$request_pause_seconds %||%
  0
)


# ============================================================
# 11. Cascade auto-accept Reviewer Agent 1 high-confidence exclusions
# ============================================================

if (cascade_auto_exclude_count > 0L) {
  reviewer_2$reviewer[cascade_auto_exclude_rows] <-
    cascade_reviewer_label

  reviewer_2$decision[cascade_auto_exclude_rows] <-
    "Exclude"

  reviewer_2$exclusion_reason[cascade_auto_exclude_rows] <-
    reviewer_1$exclusion_reason[cascade_auto_exclude_rows]

  reviewer_2$confidence[cascade_auto_exclude_rows] <-
    reviewer_1$confidence[cascade_auto_exclude_rows]

  reviewer_2$notes[cascade_auto_exclude_rows] <-
    cascade_note

  reviewer_2$screened_at[cascade_auto_exclude_rows] <-
    now_text()
}


# ============================================================
# 12. Reviewer 2 checkpoint recovery — routed subset only
# ============================================================

decision_log <- tibble::tibble()
failure_log <- tibble::tibble()

if (
  resume_enabled &&
  file.exists(checkpoint_file)
) {
  checkpoint <- readr::read_csv(
    checkpoint_file,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )

  if (
    nrow(checkpoint) > 0L &&
    all(
      c(
        "screening_id",
        "decision",
        "exclusion_reason_code",
        "exclusion_reason",
        "confidence",
        "rationale",
        "validation_status"
      ) %in% names(checkpoint)
    )
  ) {
    checkpoint <- checkpoint[
      checkpoint$validation_status == "valid",
      ,
      drop = FALSE
    ]

    if (nrow(checkpoint) > 0L) {
      checkpoint <- checkpoint[
        !duplicated(
          checkpoint$screening_id,
          fromLast = TRUE
        ),
        ,
        drop = FALSE
      ]

      valid_checkpoint <- logical(
        nrow(checkpoint)
      )

      for (i in seq_len(nrow(checkpoint))) {
        sid <- safe_chr(
          checkpoint$screening_id[[i]]
        )

        row_index <- match(
          sid,
          reviewer_2$screening_id
        )

        if (
          is.na(row_index) ||
          !row_index %in% agent2_route_rows
        ) {
          next
        }

        validation_status <- validate_agent_result(
          as.list(
            checkpoint[i, , drop = FALSE]
          ),
          sid
        )

        if (!identical(
          validation_status,
          "valid"
        )) {
          next
        }

        reviewer_2$reviewer[row_index] <- reviewer_name
        reviewer_2$decision[row_index] <-
          checkpoint$decision[[i]]

        reviewer_2$exclusion_reason[row_index] <-
          if (
            identical(
              safe_chr(checkpoint$decision[[i]]),
              "Exclude"
            )
          ) {
            checkpoint$exclusion_reason[[i]]
          } else {
            NA_character_
          }

        reviewer_2$confidence[row_index] <-
          checkpoint$confidence[[i]]

        reviewer_2$notes[row_index] <- paste0(
          "[Agent Reviewer 2 rationale] ",
          safe_chr(checkpoint$rationale[[i]]),
          " [Reason code: ",
          ifelse(
            nzchar(
              safe_chr(
                checkpoint$exclusion_reason_code[[i]]
              )
            ),
            safe_chr(
              checkpoint$exclusion_reason_code[[i]]
            ),
            "NA"
          ),
          "]"
        )

        reviewer_2$screened_at[row_index] <-
          checkpoint$completed_at[[i]] %||%
          checkpoint$started_at[[i]] %||%
          now_text()

        valid_checkpoint[[i]] <- TRUE
      }

      checkpoint <- checkpoint[
        valid_checkpoint,
        ,
        drop = FALSE
      ]

      decision_log <- tibble::as_tibble(
        checkpoint
      )

      if (nrow(decision_log) > 0L) {
        cat(
          "Recovered ",
          nrow(decision_log),
          " valid Reviewer Agent 2 checkpoint decision(s).\n",
          sep = ""
        )
      }
    }
  }
}

completed_ids <- if (
  nrow(decision_log) > 0L
) {
  unique(
    clean_chr(
      decision_log$screening_id
    )
  )
} else {
  character(0)
}


# ============================================================
# 11b. API cost preflight for remaining Reviewer Agent 2 work
# ============================================================

configured_route_rows <- intersect(
  record_indices,
  agent2_route_rows
)

scheduled_ids <- clean_chr(
  reviewer_2$screening_id[
    configured_route_rows
  ]
)

already_completed_for_schedule <- (
  scheduled_ids %in% completed_ids |
  nzchar(
    clean_chr(
      reviewer_2$decision[
        configured_route_rows
      ]
    )
  )
)

remaining_record_indices <- configured_route_rows[
  !already_completed_for_schedule
]

remaining_record_count <- length(
  remaining_record_indices
)

estimated_input_tokens <- 0L
estimated_output_tokens <- (
  remaining_record_count *
  assumed_output_tokens_per_record
)

schema_text_for_estimate <- tryCatch(
  jsonlite::toJSON(
    json_schema,
    auto_unbox = TRUE,
    null = "null"
  ),
  error = function(e) ""
)

schema_tokens_per_request <- estimate_text_tokens(
  schema_text_for_estimate,
  chars_per_token = chars_per_token
)

if (
  isTRUE(cost_estimation_enabled) &&
  remaining_record_count > 0L
) {
  input_token_estimates <- integer(
    remaining_record_count
  )

  for (
    estimate_index in seq_along(
      remaining_record_indices
    )
  ) {
    row_index <- remaining_record_indices[[
      estimate_index
    ]]

    estimate_record <- reviewer_2[
      row_index,
      ,
      drop = FALSE
    ]

    estimate_prompt <- build_agent_screening_prompt(
      template_text = prompt_template,
      record = as.list(
        estimate_record[1, , drop = FALSE]
      ),
      project_id = project_id,
      project_title = project_title,
      review_question = review_question,
      inclusion_criteria = inclusion_criteria,
      exclusion_reason_table = exclusion_reason_table
    )

    input_token_estimates[[
      estimate_index
    ]] <- (
      estimate_text_tokens(
        estimate_prompt,
        chars_per_token = chars_per_token
      ) +
      schema_tokens_per_request +
      request_overhead_tokens
    )
  }

  estimated_input_tokens <- sum(
    input_token_estimates
  )
}

estimated_input_cost_usd <- NA_real_
estimated_output_cost_usd <- NA_real_
estimated_total_cost_usd <- NA_real_
estimated_total_with_contingency_usd <- NA_real_

if (
  isTRUE(cost_estimation_enabled) &&
  !is.null(pricing_info)
) {
  estimated_input_cost_usd <- (
    estimated_input_tokens /
    1000000
  ) * pricing_info$input_usd_per_million

  estimated_output_cost_usd <- (
    estimated_output_tokens /
    1000000
  ) * pricing_info$output_usd_per_million

  estimated_total_cost_usd <- (
    estimated_input_cost_usd +
    estimated_output_cost_usd
  )

  estimated_total_with_contingency_usd <- (
    estimated_total_cost_usd *
    (1 + contingency_fraction)
  )
}


# ============================================================
# 12. Console transition: Reviewer 1 -> Reviewer 2
# ============================================================

cat("\n")
cat("============================================================\n")
cat("TITLE/ABSTRACT SCREENING — REVIEWER AGENT 2\n")
cat("============================================================\n\n")

cat(
  "Reviewer Agent 1 review has finished successfully.\n"
)

cat(
  "Reviewer Agent 1 records completed: ",
  nrow(reviewer_1),
  "\n",
  sep = ""
)

cat("\nReviewer Agent 1 decision summary:\n")
cat("  Include: ", r1_include_count, "\n", sep = "")
cat("  Exclude: ", r1_exclude_count, "\n", sep = "")
cat("  Uncertainty (", uncertainty_label, "): ", r1_unclear_count, "\n", sep = "")
cat("  Total: ", nrow(reviewer_1), "\n\n", sep = "")

cat("Selective Reviewer Agent 2 routing policy:\n")
cat(
  "  Reviewer Agent 1 High-confidence Exclude -> cascade auto-exclude, no API review: ",
  cascade_auto_exclude_count,
  "\n",
  sep = ""
)
cat(
  "  Reviewer Agent 1 Moderate/Low-confidence Exclude -> Reviewer Agent 2: ",
  routed_nonhigh_exclude_count,
  "\n",
  sep = ""
)
cat(
  "  Reviewer Agent 1 Include -> Reviewer Agent 2: ",
  routed_include_count,
  "\n",
  sep = ""
)
cat(
  "  Reviewer Agent 1 Uncertainty -> Reviewer Agent 2: ",
  routed_uncertainty_count,
  "\n",
  sep = ""
)
cat(
  "  Total routed to Reviewer Agent 2 API review: ",
  agent2_route_count,
  "\n\n",
  sep = ""
)

cat(
  "Now starting Reviewer Agent 2's targeted independent review.\n"
)

cat(
  "Reviewer Agent 2 will not see Reviewer Agent 1 decisions.\n"
)

cat(
  "Retry policy: transient API errors may be retried; billing/quota exhaustion triggers an immediate safe pause.\n"
)

cat("\n")
cat("------------------------------------------------------------\n")
cat("API COST PREFLIGHT — REVIEWER AGENT 2\n")
cat("------------------------------------------------------------\n")

cat(
  "Configured model: ",
  model_name,
  "\n",
  sep = ""
)

cat(
  "Routed records already completed/recovered: ",
  length(
    intersect(
      scheduled_ids,
      completed_ids
    )
  ),
  "\n",
  sep = ""
)

cat(
  "Routed records remaining for API review: ",
  remaining_record_count,
  "\n",
  sep = ""
)

if (isTRUE(cost_estimation_enabled)) {
  cat(
    "Estimated input tokens: ",
    format(
      estimated_input_tokens,
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )

  cat(
    "Assumed output tokens per record: ",
    assumed_output_tokens_per_record,
    "\n",
    sep = ""
  )

  cat(
    "Estimated output tokens: ",
    format(
      estimated_output_tokens,
      big.mark = ",",
      scientific = FALSE
    ),
    "\n",
    sep = ""
  )

  if (!is.null(pricing_info)) {
    cat(
      sprintf(
        "Input price: $%.4f / 1M tokens\n",
        pricing_info$input_usd_per_million
      )
    )

    cat(
      sprintf(
        "Output price: $%.4f / 1M tokens\n",
        pricing_info$output_usd_per_million
      )
    )

    cat(
      sprintf(
        "Estimated input cost: $%.4f\n",
        estimated_input_cost_usd
      )
    )

    cat(
      sprintf(
        "Estimated output cost: $%.4f\n",
        estimated_output_cost_usd
      )
    )

    cat(
      sprintf(
        "Estimated API cost for remaining records: $%.4f\n",
        estimated_total_cost_usd
      )
    )

    cat(
      sprintf(
        "Suggested credit headroom (+%.0f%%): $%.4f\n",
        100 * contingency_fraction,
        estimated_total_with_contingency_usd
      )
    )

    cat(
      "Pricing source: ",
      pricing_info$source,
      "\n",
      sep = ""
    )
  } else {
    cat(
      "Token volume estimated, but no price is known for this model.\n"
    )

    cat(
      "Add input_usd_per_million and output_usd_per_million ",
      "under agent_screening.cost_estimation to enable dollar estimation.\n",
      sep = ""
    )
  }

  cat(
    "Estimate note: token counts are approximate and assume no prompt-caching discount or retries.\n"
  )
} else {
  cat(
    "API cost estimation is disabled by configuration.\n"
  )
}

cat("------------------------------------------------------------\n")

if (any(nzchar(adjudicator_before))) {
  cat(
    "Existing adjudicator role/name entries detected and preserved; ",
    "no completed human adjudication decisions were found.\n"
  )
}

cat("\n")

cat(
  "Project: ",
  project_title,
  "\n",
  sep = ""
)

cat(
  "Provider: ",
  provider_name,
  "\n",
  sep = ""
)

cat(
  "Model: ",
  model_name,
  "\n",
  sep = ""
)

cat(
  "Reviewer: ",
  reviewer_name,
  "\n",
  sep = ""
)

cat(
  "Records in configured screening range: ",
  length(record_indices),
  "\n",
  sep = ""
)

cat(
  "Records routed to Reviewer Agent 2: ",
  length(configured_route_rows),
  "\n",
  sep = ""
)

cat(
  "Routed records remaining after checkpoint recovery: ",
  remaining_record_count,
  "\n\n",
  sep = ""
)


# ============================================================
# 13. Independent Reviewer Agent 2 screening loop
# ============================================================

processed_in_session <- 0L

for (row_index in configured_route_rows) {

  record <- reviewer_2[
    row_index,
    ,
    drop = FALSE
  ]

  screening_id <- safe_chr(
    record$screening_id[[1]]
  )

  if (screening_id %in% completed_ids) {
    cat(
      "Reviewer Agent 2 checkpoint already completed: ",
      screening_id,
      "\n",
      sep = ""
    )
    next
  }

  existing_decision <- safe_chr(
    record$decision[[1]]
  )

  if (nzchar(existing_decision)) {
    existing_reviewer <- safe_chr(
      record$reviewer[[1]]
    )

    if (
      grepl(
        "^Agent Reviewer 2",
        existing_reviewer,
        ignore.case = TRUE
      )
    ) {
      cat(
        "Reviewer Agent 2 existing decision preserved: ",
        screening_id,
        "\n",
        sep = ""
      )
      next
    }

    stop(
      "Reviewer_2 contains an existing non-agent decision for ",
      screening_id,
      ". Refusing to overwrite possible human work.",
      call. = FALSE
    )
  }

  # IMPORTANT:
  # This prompt uses ONLY Reviewer_2 record metadata.
  # Reviewer_1 decision/reason/confidence/rationale are never supplied.
  prompt <- build_agent_screening_prompt(
    template_text = prompt_template,
    record = as.list(
      record[1, , drop = FALSE]
    ),
    project_id = project_id,
    project_title = project_title,
    review_question = review_question,
    inclusion_criteria = inclusion_criteria,
    exclusion_reason_table = exclusion_reason_table
  )

  success <- FALSE
  last_error <- ""
  nonretryable_billing_error <- FALSE

  for (attempt in seq_len(max_retries)) {

    started_at <- Sys.time()

    outcome <- tryCatch(
      {
        response <- call_llm_screening_provider(
          provider_name = provider_name,
          prompt = prompt,
          provider_config = provider_config,
          json_schema = json_schema
        )

        validation_status <- validate_agent_result(
          response$parsed,
          screening_id
        )

        if (!identical(
          validation_status,
          "valid"
        )) {
          stop(
            "Reviewer Agent 2 result validation failed: ",
            validation_status,
            call. = FALSE
          )
        }

        finished_at <- Sys.time()
        result <- response$parsed

        current_row <- tibble::tibble(
          screening_id = screening_id,
          decision = safe_chr(result$decision),
          exclusion_reason_code =
            safe_chr(result$exclusion_reason_code),
          exclusion_reason =
            safe_chr(result$exclusion_reason),
          confidence = safe_chr(result$confidence),
          rationale = safe_chr(result$rationale),
          reviewer = reviewer_name,
          reviewer_role = "Reviewer_2",
          provider = provider_name,
          model = safe_chr(response$model),
          prompt_version = prompt_version,
          response_id = safe_chr(response$response_id),
          attempt = as.character(attempt),
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

        decision_log <- dplyr::bind_rows(
          decision_log,
          current_row
        )

        reviewer_2$reviewer[row_index] <-
          reviewer_name

        reviewer_2$decision[row_index] <-
          safe_chr(result$decision)

        reviewer_2$exclusion_reason[row_index] <-
          if (
            identical(
              safe_chr(result$decision),
              "Exclude"
            )
          ) {
            safe_chr(result$exclusion_reason)
          } else {
            NA_character_
          }

        reviewer_2$confidence[row_index] <-
          safe_chr(result$confidence)

        reviewer_2$notes[row_index] <-
          paste0(
            "[Agent Reviewer 2 rationale] ",
            safe_chr(result$rationale),
            " [Reason code: ",
            ifelse(
              nzchar(
                safe_chr(
                  result$exclusion_reason_code
                )
              ),
              safe_chr(
                result$exclusion_reason_code
              ),
              "NA"
            ),
            "]"
          )

        reviewer_2$screened_at[row_index] <-
          format(
            finished_at,
            "%Y-%m-%d %H:%M:%S"
          )

        success <- TRUE
        TRUE
      },
      error = function(e) {
        last_error <<- conditionMessage(e)

        if (
          is_nonretryable_api_billing_error(
            last_error
          )
        ) {
          nonretryable_billing_error <<- TRUE
        }

        FALSE
      }
    )

    if (isTRUE(outcome)) {
      break
    }

    if (isTRUE(nonretryable_billing_error)) {
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

  if (!success) {
    failure_log <- dplyr::bind_rows(
      failure_log,
      tibble::tibble(
        screening_id = screening_id,
        row_index = row_index,
        reviewer = reviewer_name,
        provider = provider_name,
        model = model_name,
        attempts = if (
          isTRUE(nonretryable_billing_error)
        ) {
          1L
        } else {
          max_retries
        },
        error = last_error,
        failed_at = now_text()
      )
    )

    cat(
      "Reviewer Agent 2 FAILED: ",
      screening_id,
      " — ",
      last_error,
      "\n",
      sep = ""
    )

    if (isTRUE(nonretryable_billing_error)) {
      # Persist every valid decision accumulated in memory, including
      # records completed since the most recent periodic checkpoint.
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

      cat("\n")
      cat("============================================================\n")
      cat("REVIEWER AGENT 2 — SAFE PAUSE\n")
      cat("============================================================\n\n")
      cat(
        "API billing/quota access is currently unavailable.\n"
      )
      cat(
        "All completed Reviewer Agent 2 decisions have been checkpointed.\n"
      )
      cat(
        "No completed screening work needs to be repeated.\n"
      )
      cat(
        "Current record: ",
        screening_id,
        "\n",
        sep = ""
      )
      cat(
        "Action: restore API credits/limits, then rerun the same R/19b command.\n"
      )
      cat(
        "The toolkit will recover valid Reviewer Agent 2 decisions and continue from the first unfinished record.\n"
      )

      stop(
        "Reviewer Agent 2 paused safely because API credits/limits are unavailable. ",
        "Completed decisions were checkpointed. Restore API access and rerun R/19b.",
        call. = FALSE
      )
    }
  } else {
    cat(
      "Reviewer Agent 2 completed: ",
      screening_id,
      "\n",
      sep = ""
    )
  }

  processed_in_session <- processed_in_session + 1L

  if (
    processed_in_session %% checkpoint_every == 0L
  ) {
    readr::write_csv(
      decision_log,
      checkpoint_file,
      na = ""
    )

    if (nrow(failure_log) > 0L) {
      readr::write_csv(
        failure_log,
        failures_file,
        na = ""
      )
    }

    cat(
      "Reviewer Agent 2 checkpoint saved after ",
      processed_in_session,
      " record(s).\n",
      sep = ""
    )
  }

  if (pause_seconds > 0) {
    Sys.sleep(pause_seconds)
  }
}


# ============================================================
# 14. Save Reviewer Agent 2 audit outputs
# ============================================================

readr::write_csv(
  decision_log,
  decisions_file,
  na = ""
)

readr::write_csv(
  decision_log,
  checkpoint_file,
  na = ""
)

if (nrow(failure_log) > 0L) {
  readr::write_csv(
    failure_log,
    failures_file,
    na = ""
  )
} else {
  failure_log <- tibble::tibble(
    screening_id = character(0),
    row_index = integer(0),
    reviewer = character(0),
    provider = character(0),
    model = character(0),
    attempts = integer(0),
    error = character(0),
    failed_at = character(0)
  )

  readr::write_csv(
    failure_log,
    failures_file,
    na = ""
  )
}


# ============================================================
# 15. Validate completeness before comparison
# ============================================================

r2_decisions <- clean_chr(
  reviewer_2$decision
)

r2_reasons <- clean_chr(
  reviewer_2$exclusion_reason
)

missing_r2_decisions <- which(
  !nzchar(r2_decisions)
)

invalid_r2_decisions <- which(
  nzchar(r2_decisions) &
    !r2_decisions %in% decision_options
)

r2_exclude <- r2_decisions == "Exclude"

invalid_r2_reasons <- which(
  r2_exclude &
    !r2_reasons %in% exclusion_reason_table$label
)

nonexclude_r2_reasons <- which(
  !r2_exclude &
    nzchar(r2_reasons)
)

valid_decisions <- decision_log |>
  dplyr::filter(
    validation_status == "valid"
  )

stage_complete <- (
  length(missing_r2_decisions) == 0L &&
  length(invalid_r2_decisions) == 0L &&
  length(invalid_r2_reasons) == 0L &&
  length(nonexclude_r2_reasons) == 0L &&
  nrow(failure_log) == 0L
)

if (!stage_complete) {
  stop(
    "Reviewer Agent 2 stage is incomplete.\n",
    "Missing decisions: ",
    length(missing_r2_decisions),
    "\nInvalid decisions: ",
    length(invalid_r2_decisions),
    "\nInvalid exclusion reasons: ",
    length(invalid_r2_reasons),
    "\nNon-Exclude rows with reason: ",
    length(nonexclude_r2_reasons),
    "\nAgent failures: ",
    nrow(failure_log),
    "\nRerun R/19b after resolving the logged issue(s).",
    call. = FALSE
  )
}


# ============================================================
# 15b. Reviewer Agent 2 decision summary
# ============================================================

r2_include_count <- sum(
  r2_decisions[agent2_route_rows] == "Include"
)

r2_exclude_count <- sum(
  r2_decisions[agent2_route_rows] == "Exclude"
)

r2_unclear_count <- sum(
  r2_decisions[agent2_route_rows] %in% uncertainty_options
)


# ============================================================
# 16. Compare Reviewer Agent 1 vs Reviewer Agent 2
# ============================================================

same_decision <- (
  r1_decisions == r2_decisions
)

same_exclusion_reason <- rep(
  TRUE,
  nrow(reviewer_1)
)

both_exclude <- (
  r1_decisions == "Exclude" &
  r2_decisions == "Exclude"
)

same_exclusion_reason[both_exclude] <- (
  r1_reasons[both_exclude] ==
    r2_reasons[both_exclude]
)

agreement <- (
  same_decision &
  same_exclusion_reason
)

agreement_status <- ifelse(
  agreement,
  "Agree",
  "Disagree"
)

conflict_status <- ifelse(
  agreement,
  "No conflict",
  "Conflict"
)

# Cascade rows are not independent two-agent agreements.
# Keep them audit-visible while retaining downstream R/21 compatibility.
if (cascade_auto_exclude_count > 0L) {
  agreement_status[cascade_auto_exclude_rows] <-
    "Cascade auto-exclude"

  conflict_status[cascade_auto_exclude_rows] <-
    "No conflict"
}

routed_conflict <- rep(
  FALSE,
  nrow(reviewer_1)
)

routed_conflict[agent2_route_rows] <- (
  !agreement[agent2_route_rows]
)

# Human adjudication is intentionally limited to the Reviewer Agent 2
# routed subset. A mismatch means either:
#   1) Reviewer 1 vs Reviewer 2 decision category differs, or
#   2) both are Exclude but the exclusion reason differs.
# High-confidence Reviewer 1 Exclude rows never enter this adjudication set.

agreement_count <- sum(
  agreement[agent2_route_rows]
)

conflict_count <- sum(
  routed_conflict
)


# ============================================================
# 17. Build conflict-only audit table
# ============================================================

conflict_audit <- tibble::tibble(
  screening_id = clean_chr(
    reviewer_1$screening_id
  ),
  title = clean_chr(
    reviewer_1$title
  ),
  year = clean_chr(
    reviewer_1$year
  ),
  journal = clean_chr(
    reviewer_1$journal
  ),
  reviewer_1_decision = r1_decisions,
  reviewer_1_exclusion_reason = r1_reasons,
  reviewer_2_decision = r2_decisions,
  reviewer_2_exclusion_reason = r2_reasons,
  agreement_status = agreement_status,
  conflict_status = conflict_status,
  routing_source = ifelse(
    r1_decisions == "Exclude",
    "High-confidence cascade auto-exclude — no Agent 2 API review",
    "Reviewer Agent 2 API review"
  )
) |>
  dplyr::filter(
    routed_conflict
  )

readr::write_csv(
  conflict_audit,
  conflicts_file,
  na = ""
)


# ============================================================
# 18. Update workbook without touching human fields
# ============================================================

reviewer_1_before <- reviewer_1

workbook <- openxlsx::loadWorkbook(
  output_workbook
)

openxlsx::writeData(
  workbook,
  sheet = "Reviewer_2",
  x = reviewer_2,
  startRow = 4,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE,
  withFilter = TRUE,
  keepNA = FALSE
)

conflict_data$reviewer_1_decision <-
  r1_decisions

conflict_data$reviewer_2_decision <-
  r2_decisions

conflict_data$agreement_status <-
  agreement_status

conflict_data$conflict_status <-
  conflict_status

comparison_columns <- c(
  "reviewer_1_decision",
  "reviewer_2_decision",
  "agreement_status",
  "conflict_status"
)

for (column_name in comparison_columns) {
  column_index <- match(
    column_name,
    names(conflict_data)
  )

  openxlsx::writeData(
    workbook,
    sheet = "Conflict_Resolution",
    x = conflict_data[[column_name]],
    startRow = 4,
    startCol = column_index,
    colNames = FALSE,
    rowNames = FALSE,
    keepNA = FALSE
  )
}

openxlsx::saveWorkbook(
  workbook,
  output_workbook,
  overwrite = TRUE
)


# ============================================================
# 19. Standalone post-save validation
# ============================================================

saved_reviewer_1 <- openxlsx::readWorkbook(
  output_workbook,
  sheet = "Reviewer_1",
  startRow = 4,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

saved_reviewer_2 <- openxlsx::readWorkbook(
  output_workbook,
  sheet = "Reviewer_2",
  startRow = 4,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

saved_conflicts <- openxlsx::readWorkbook(
  output_workbook,
  sheet = "Conflict_Resolution",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

r1_preservation_fields <- c(
  "screening_id",
  "reviewer",
  "decision",
  "exclusion_reason",
  "confidence",
  "notes",
  "screened_at"
)

for (field_name in r1_preservation_fields) {
  before_values <- clean_chr(
    reviewer_1_before[[field_name]]
  )

  after_values <- clean_chr(
    saved_reviewer_1[[field_name]]
  )

  if (!identical(
    before_values,
    after_values
  )) {
    stop(
      "Reviewer Agent 2 unexpectedly modified Reviewer_1 field: ",
      field_name,
      call. = FALSE
    )
  }
}

saved_r2_decisions <- clean_chr(
  saved_reviewer_2$decision
)

if (
  length(saved_r2_decisions) != nrow(reviewer_2) ||
  any(!nzchar(saved_r2_decisions))
) {
  stop(
    "Saved Reviewer_2 worksheet is incomplete.",
    call. = FALSE
  )
}

for (field_name in human_decision_fields) {
  values <- clean_chr(
    saved_conflicts[[field_name]]
  )

  if (any(nzchar(values))) {
    stop(
      "Reviewer Agent 2 populated a human-only field unexpectedly: ",
      field_name,
      call. = FALSE
    )
  }
}

saved_adjudicator <- clean_chr(
  saved_conflicts$adjudicator
)

if (!identical(
  adjudicator_before,
  saved_adjudicator
)) {
  stop(
    "Reviewer Agent 2 unexpectedly modified the adjudicator field.",
    call. = FALSE
  )
}

saved_conflict_status <- clean_chr(
  saved_conflicts$conflict_status
)

if (
  sum(saved_conflict_status == "Conflict") !=
  conflict_count
) {
  stop(
    "Saved conflict count does not match the R-computed comparison.",
    call. = FALSE
  )
}


# ============================================================
# 20. Report
# ============================================================

decision_summary <- tibble::tibble(
  decision = r2_decisions
) |>
  dplyr::count(
    decision,
    name = "count"
  )

confidence_summary <- tibble::tibble(
  confidence = clean_chr(
    reviewer_2$confidence
  )
) |>
  dplyr::count(
    confidence,
    name = "count"
  )

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "Reviewer Agent 2 — Selective Title/Abstract Re-screening",
  "",
  paste0("Generated at: ", now_text()),
  paste0("Project ID: ", project_id),
  paste0("Review title: ", project_title),
  "",
  "SELECTIVE ROUTING POLICY",
  "--------------------------------------------------",
  "Reviewer Agent 1 Exclude -> cascade auto-exclude; no Agent 2 API review.",
  "Reviewer Agent 1 Include/Uncertainty -> Reviewer Agent 2 API review.",
  "Human adjudication -> routed disagreements only.",
  "",
  "REVIEWER AGENT 1 SUMMARY",
  "--------------------------------------------------",
  paste0("Include: ", r1_include_count),
  paste0("Exclude: ", r1_exclude_count),
  paste0("Uncertainty (", uncertainty_label, "): ", r1_unclear_count),
  paste0("Total: ", nrow(reviewer_1)),
  "",
  "REVIEWER AGENT 2",
  "--------------------------------------------------",
  paste0("Reviewer: ", reviewer_name),
  paste0("Provider: ", provider_name),
  paste0("Model: ", model_name),
  paste0("Prompt version: ", prompt_version),
  paste0("High-confidence Exclude cascade auto-accepted: ", cascade_auto_exclude_count),
  paste0("Moderate/Low-confidence Exclude routed to Reviewer Agent 2: ", routed_nonhigh_exclude_count),
  paste0("Include routed to Reviewer Agent 2: ", routed_include_count),
  paste0("Uncertainty routed to Reviewer Agent 2: ", routed_uncertainty_count),
  paste0("Total records routed to Reviewer Agent 2: ", agent2_route_count),
  paste0("Reviewer Agent 2 API-reviewed records: ", agent2_route_count),
  paste0("Valid Reviewer 2 API decisions: ", agent2_route_count),
  paste0("Failures: ", nrow(failure_log)),
  paste0("Include: ", r2_include_count),
  paste0("Exclude: ", r2_exclude_count),
  paste0("Uncertainty (", uncertainty_label, "): ", r2_unclear_count),
  "",
  "DECISIONS",
  "--------------------------------------------------",
  if (nrow(decision_summary) > 0L) {
    paste0(
      decision_summary$decision,
      ": ",
      decision_summary$count
    )
  } else {
    "No decisions"
  },
  "",
  "API COST PREFLIGHT",
  "--------------------------------------------------",
  paste0("Model: ", model_name),
  paste0(
    "Remaining routed records estimated: ",
    remaining_record_count
  ),
  paste0(
    "Estimated input tokens: ",
    estimated_input_tokens
  ),
  paste0(
    "Assumed output tokens per record: ",
    assumed_output_tokens_per_record
  ),
  paste0(
    "Estimated output tokens: ",
    estimated_output_tokens
  ),
  if (!is.null(pricing_info)) {
    paste0(
      "Input USD per 1M tokens: ",
      pricing_info$input_usd_per_million
    )
  } else {
    "Input USD per 1M tokens: UNKNOWN"
  },
  if (!is.null(pricing_info)) {
    paste0(
      "Output USD per 1M tokens: ",
      pricing_info$output_usd_per_million
    )
  } else {
    "Output USD per 1M tokens: UNKNOWN"
  },
  if (is.finite(estimated_total_cost_usd)) {
    paste0(
      "Estimated remaining API cost USD: ",
      format(
        estimated_total_cost_usd,
        digits = 6
      )
    )
  } else {
    "Estimated remaining API cost USD: UNKNOWN"
  },
  if (is.finite(estimated_total_with_contingency_usd)) {
    paste0(
      "Suggested credit headroom USD: ",
      format(
        estimated_total_with_contingency_usd,
        digits = 6
      )
    )
  } else {
    "Suggested credit headroom USD: UNKNOWN"
  },
  paste0(
    "Pricing source: ",
    if (
      is.null(pricing_info)
    ) {
      "unknown"
    } else {
      pricing_info$source
    }
  ),
  "Estimate assumes no prompt-caching discount and no retries.",
  "",
  "TWO-AGENT COMPARISON",
  "--------------------------------------------------",
  paste0("High-confidence cascade auto-excluded records: ", cascade_auto_exclude_count),
  paste0("Agent 2 routed records: ", agent2_route_count),
  paste0("Agreement within Agent 2 routed subset: ", agreement_count),
  paste0("Human-adjudication conflicts within routed subset: ", conflict_count),
  paste0(
    "Agreement rate within routed subset: ",
    if (agent2_route_count > 0L) {
      round(
        100 * agreement_count / agent2_route_count,
        1
      )
    } else {
      100
    },
    "%"
  ),
  "",
  "HUMAN OVERSIGHT",
  "--------------------------------------------------",
  "Reviewer Agent 2 full-record screening required: NO",
  paste0(
    "Human adjudication required only for conflicts in the Agent 2 routed subset: ",
    conflict_count
  ),
  "Only Reviewer Agent 1 High-confidence Exclude records bypass Agent 2 and human adjudication.",
  "Agreement records in the routed subset require no manual adjudicator entry.",
  "Human final-decision fields populated by Reviewer Agent 2: NO",
  "",
  "OUTPUT FILES",
  "--------------------------------------------------",
  output_workbook,
  decisions_file,
  failures_file,
  checkpoint_file,
  conflicts_file,
  report_file
)

writeLines(
  report_lines,
  report_file,
  useBytes = TRUE
)


# ============================================================
# 21. Console completion and next-step announcement
# ============================================================

cat("\n")
cat("============================================================\n")
cat("REVIEWER AGENT 2 — TARGETED REVIEW COMPLETE\n")
cat("============================================================\n\n")

cat(
  "Reviewer Agent 2 review completed successfully.\n"
)

cat(
  "High-confidence Exclude cascade auto-excluded without Agent 2 API review: ",
  cascade_auto_exclude_count,
  "\n",
  sep = ""
)

cat(
  "Records actually reviewed by Reviewer Agent 2: ",
  agent2_route_count,
  "\n",
  sep = ""
)

cat(
  "Agent failures: ",
  nrow(failure_log),
  "\n\n",
  sep = ""
)

cat("Reviewer Agent 2 decision summary — routed subset only:\n")
cat("  Include: ", r2_include_count, "\n", sep = "")
cat("  Exclude: ", r2_exclude_count, "\n", sep = "")
cat("  Uncertainty (", uncertainty_label, "): ", r2_unclear_count, "\n", sep = "")
cat("  Total API-reviewed: ", agent2_route_count, "\n\n", sep = "")

cat(
  "Now comparing Reviewer Agent 1 and Reviewer Agent 2 decisions...\n\n"
)

cat(
  "Agreement within routed subset: ",
  agreement_count,
  "\n",
  sep = ""
)

cat(
  "Disagreement requiring human adjudication: ",
  conflict_count,
  "\n",
  sep = ""
)

cat(
  "Agreement rate within routed subset: ",
  if (agent2_route_count > 0L) {
    round(
      100 * agreement_count / agent2_route_count,
      1
    )
  } else {
    100
  },
  "%\n\n",
  sep = ""
)
cat(
  "Human adjudication scope: only Reviewer Agent 2 routed records whose ",
  "final decision/reason does not match Reviewer Agent 1.\n\n",
  sep = ""
)

if (conflict_count > 0L) {
  cat(
    "Automatic action: Reviewer Agent 1 High-confidence Exclude records are already finalized ",
    "by cascade policy; agreements in the routed subset require no further screening.\n",
    sep = ""
  )

  cat(
    "Human review required: only ",
    conflict_count,
    " routed conflict record(s).\n",
    sep = ""
  )

  cat(
    "Next step: the human adjudicator reviews only the Reviewer Agent 2 ",
    "conflict rows in the Conflict_Resolution sheet.\n",
    sep = ""
  )

  cat("\n")
  cat("============================================================\n")
  cat("HUMAN ADJUDICATION HANDOFF\n")
  cat("============================================================\n\n")
  cat("Open this Excel workbook:\n- ", output_workbook, "\n\n", sep = "")
  cat("Open worksheet:\n- Conflict_Resolution\n\n")
  cat("Review only rows where conflict_status = \"Conflict\".\n\n")
  cat(
    "Required human fields:\n",
    "  1. final_decision — required for every conflict row\n",
    "  2. final_exclusion_reason — required when final_decision = Exclude\n",
    sep = ""
  )
  cat(
    "\nRecommended audit fields:\n",
    "  3. adjudicator\n",
    "  4. resolution_notes\n",
    "  5. resolved_at\n",
    sep = ""
  )
  cat("\nAfter adjudication, save the SAME workbook at the SAME path.\n")
  cat("Do not create a separate CSV or a new Excel filename for R/21.\n")
  cat(
    "\nThen run:\n",
    '  source("R/21_finalize_title_abstract_screening.R")\n',
    sep = ""
  )
} else {
  cat(
    "No reviewer conflicts were detected.\n"
  )

  cat(
    "No human conflict adjudication is required; title/abstract ",
    "screening can proceed directly to finalization.\n",
    sep = ""
  )
}

cat(
  "\nPASS: Reviewer Agent 2 selective screening completed successfully.\n"
)

cat(
  "Conflict-only audit file:\n- ",
  conflicts_file,
  "\n",
  sep = ""
)


# ============================================================
# 22. Return stage result
# ============================================================

invisible(
  list(
    status = "PASS",
    reviewer = reviewer_name,
    provider = provider_name,
    model = model_name,
    records = agent2_route_count,
    cascade_auto_exclude_count = cascade_auto_exclude_count,
    routed_nonhigh_exclude_count = routed_nonhigh_exclude_count,
    routed_include_count = routed_include_count,
    routed_uncertainty_count = routed_uncertainty_count,
    agent2_route_count = agent2_route_count,
    reviewer_1_summary = list(
      include = r1_include_count,
      exclude = r1_exclude_count,
      unclear = r1_unclear_count
    ),
    reviewer_2_summary = list(
      include = r2_include_count,
      exclude = r2_exclude_count,
      unclear = r2_unclear_count
    ),
    cost_preflight = list(
      remaining_records = remaining_record_count,
      estimated_input_tokens = estimated_input_tokens,
      assumed_output_tokens_per_record =
        assumed_output_tokens_per_record,
      estimated_output_tokens = estimated_output_tokens,
      input_usd_per_million = if (
        is.null(pricing_info)
      ) {
        NA_real_
      } else {
        pricing_info$input_usd_per_million
      },
      output_usd_per_million = if (
        is.null(pricing_info)
      ) {
        NA_real_
      } else {
        pricing_info$output_usd_per_million
      },
      estimated_total_cost_usd =
        estimated_total_cost_usd,
      suggested_credit_headroom_usd =
        estimated_total_with_contingency_usd,
      pricing_source = if (
        is.null(pricing_info)
      ) {
        "unknown"
      } else {
        pricing_info$source
      }
    ),
    agreement_count = agreement_count,
    conflict_count = conflict_count,
    agreement_rate = agreement_count / nrow(reviewer_1),
    workbook = output_workbook,
    decisions_file = decisions_file,
    failures_file = failures_file,
    checkpoint_file = checkpoint_file,
    conflicts_file = conflicts_file,
    report_file = report_file
  )
)
