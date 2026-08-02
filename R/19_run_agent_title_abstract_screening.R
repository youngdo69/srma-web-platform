# ============================================================
# Universal SR-MA Agent Toolkit
# R/19_run_agent_title_abstract_screening.R
#
# Purpose
# ------------------------------------------------------------
# 1. Read project and agent-screening configuration
# 2. Read the title/abstract screening workbook from R/18
# 3. Run Agent Reviewer 1 through a configurable LLM provider
# 4. Validate and checkpoint every decision
# 5. Preserve Reviewer 2 and human adjudication fields
# 6. Save the agent-reviewed workbook, audit CSVs, and summary
# ============================================================

required_packages <- c(
  "yaml", "openxlsx", "readr", "dplyr", "tibble",
  "stringr", "jsonlite", "httr2", "dotenv"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ", paste(missing_packages, collapse = ", "),
    "\nInstall them with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))"
  )
}

source("R/utils/config_utils.R")
source("R/utils/path_utils.R")
source("R/utils/screening_prompt_utils.R")
source("R/utils/llm_provider_utils.R")

config <- load_project_config(quiet = FALSE)
paths <- build_project_paths(config, create_directories = TRUE)

agent_config_file <- "config/agent_screening.yml"
if (!file.exists(agent_config_file)) {
  stop("Agent-screening config not found: ", agent_config_file)
}

agent_document <- yaml::read_yaml(agent_config_file)
agent_config <- agent_document$agent_screening

if (!isTRUE(agent_config$enabled)) {
  stop("Agent screening is disabled in config/agent_screening.yml.")
}

load_project_env(".env")

project_id <- get_project_id(config)
project_title <- get_project_title(config)
decision_options <- get_screening_decisions(config)
exclusion_reason_table <- get_exclusion_reasons(config)

review_question <- config$review$question %||%
  config$eligibility$review_question %||%
  config$project$review_question %||%
  "Not explicitly specified in project_config.yml"

inclusion_criteria <- config$eligibility$inclusion %||%
  config$screening$inclusion_criteria %||%
  config$review$inclusion_criteria %||%
  "Apply the finalized review protocol."

provider_config <- agent_config$provider
provider_config$timeout_seconds <- agent_config$processing$timeout_seconds %||% 120

provider_name <- provider_config$name
model_name <- provider_config$model
reviewer_name <- agent_config$reviewer$name %||% "Agent Reviewer 1"
prompt_version <- agent_config$prompt$version %||% "unknown"

input_workbook <- paths$screening_workbook

screening_output_directory <- paths$screening_results_directory
logs_output_directory <- paths$logs_directory

output_workbook <- file.path(
  screening_output_directory,
  paste0(agent_config$output$workbook_basename, ".xlsx")
)

decisions_file <- file.path(
  logs_output_directory,
  paste0(agent_config$output$decisions_basename, ".csv")
)

failures_file <- file.path(
  logs_output_directory,
  paste0(agent_config$output$failures_basename, ".csv")
)

checkpoint_file <- file.path(
  logs_output_directory,
  paste0(agent_config$output$checkpoint_basename, ".csv")
)

report_file <- file.path(
  logs_output_directory,
  paste0(agent_config$output$report_basename, ".txt")
)

prompt_template_file <- agent_config$prompt$template_file

if (!file.exists(input_workbook)) {
  stop(
    "Input screening workbook not found: ", input_workbook, "\n",
    "Run R/18_prepare_screening_workbook.R first."
  )
}

if (!file.exists(prompt_template_file)) {
  stop("Prompt template not found: ", prompt_template_file)
}

prompt_template <- paste(
  readLines(prompt_template_file, warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

reviewer_data <- openxlsx::readWorkbook(
  input_workbook,
  sheet = "Reviewer_1",
  startRow = 4,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

required_columns <- c(
  "screening_id", "title", "abstract", "authors", "year", "journal",
  "publication_type", "language", "reviewer", "decision",
  "exclusion_reason", "confidence", "notes", "screened_at"
)

missing_columns <- setdiff(required_columns, names(reviewer_data))
if (length(missing_columns) > 0L) {
  stop(
    "Reviewer_1 sheet is missing columns:\n- ",
    paste(missing_columns, collapse = "\n- ")
  )
}

reviewer_data$reviewer <- reviewer_name

start_record <- as.integer(agent_config$processing$start_record %||% 1L)
max_records <- agent_config$processing$max_records
end_record <- nrow(reviewer_data)

if (!is.null(max_records) && !is.na(max_records)) {
  end_record <- min(end_record, start_record + as.integer(max_records) - 1L)
}

record_indices <- seq.int(start_record, end_record)

resume_enabled <- isTRUE(agent_config$processing$resume_from_checkpoint)
overwrite_existing <- isTRUE(
  agent_config$processing$overwrite_existing_agent_decisions
)

decision_log <- tibble::tibble()
failure_log <- tibble::tibble()

if (resume_enabled && file.exists(checkpoint_file)) {
  decision_log <- readr::read_csv(
    checkpoint_file,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
}

completed_ids <- if (nrow(decision_log) > 0L) {
  unique(decision_log$screening_id[decision_log$validation_status == "valid"])
} else {
  character(0)
}

json_schema <- build_screening_json_schema(
  decision_options = decision_options,
  exclusion_reason_codes = exclusion_reason_table$code,
  exclusion_reason_labels = exclusion_reason_table$label,
  confidence_options = agent_config$decision_policy$allowed_confidence
)

validate_agent_result <- function(result, expected_screening_id) {
  required <- c(
    "screening_id", "decision", "exclusion_reason_code",
    "exclusion_reason", "confidence", "rationale"
  )

  if (!all(required %in% names(result))) {
    return("missing_required_fields")
  }

  if (!identical(as.character(result$screening_id), expected_screening_id)) {
    return("screening_id_mismatch")
  }

  if (!result$decision %in% decision_options) {
    return("invalid_decision")
  }

  if (!result$confidence %in% agent_config$decision_policy$allowed_confidence) {
    return("invalid_confidence")
  }

  if (identical(result$decision, "Exclude")) {
    matching_reason <- exclusion_reason_table[
      exclusion_reason_table$code == result$exclusion_reason_code &
        exclusion_reason_table$label == result$exclusion_reason,
      ,
      drop = FALSE
    ]

    if (nrow(matching_reason) != 1L) {
      return("invalid_exclusion_reason")
    }
  } else {
    if (nzchar(result$exclusion_reason_code) ||
        nzchar(result$exclusion_reason)) {
      return("reason_present_for_nonexclude")
    }
  }

  if (!nzchar(trimws(result$rationale))) {
    return("missing_rationale")
  }

  "valid"
}

checkpoint_every <- as.integer(
  agent_config$processing$checkpoint_every %||% 10L
)

max_retries <- as.integer(
  agent_config$processing$max_retries %||% 3L
)

pause_seconds <- as.numeric(
  agent_config$processing$request_pause_seconds %||% 0
)

cat("\n====================================================\n")
cat("Agent Title/Abstract Screening\n")
cat("====================================================\n\n")
cat("Project:", project_title, "\n")
cat("Provider:", provider_name, "\n")
cat("Model:", model_name, "\n")
cat("Reviewer:", reviewer_name, "\n")
cat("Records scheduled:", length(record_indices), "\n\n")

processed_in_session <- 0L

for (row_index in record_indices) {
  record <- reviewer_data[row_index, , drop = FALSE]
  screening_id <- as.character(record$screening_id[[1]])

  if (screening_id %in% completed_ids) {
    cat("Skipping checkpointed record:", screening_id, "\n")
    next
  }

  existing_decision <- as.character(record$decision[[1]])
  if (!overwrite_existing &&
      !is.na(existing_decision) &&
      nzchar(trimws(existing_decision))) {
    cat("Skipping existing decision:", screening_id, "\n")
    next
  }

  prompt <- build_agent_screening_prompt(
    template_text = prompt_template,
    record = as.list(record[1, , drop = FALSE]),
    project_id = project_id,
    project_title = project_title,
    review_question = review_question,
    inclusion_criteria = inclusion_criteria,
    exclusion_reason_table = exclusion_reason_table
  )

  success <- FALSE
  last_error <- NA_character_

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

        if (!identical(validation_status, "valid")) {
          stop("Agent result validation failed: ", validation_status)
        }

        finished_at <- Sys.time()
        result <- response$parsed

        decision_log <- dplyr::bind_rows(
          decision_log,
          tibble::tibble(
            screening_id = screening_id,
            decision = as.character(result$decision),
            exclusion_reason_code = as.character(result$exclusion_reason_code),
            exclusion_reason = as.character(result$exclusion_reason),
            confidence = as.character(result$confidence),
            rationale = as.character(result$rationale),
            reviewer = reviewer_name,
            provider = provider_name,
            model = as.character(response$model),
            prompt_version = prompt_version,
            response_id = as.character(response$response_id),
            attempt = as.character(attempt),
            started_at = format(started_at, "%Y-%m-%d %H:%M:%S"),
            completed_at = format(finished_at, "%Y-%m-%d %H:%M:%S"),
            elapsed_seconds = as.character(
              round(as.numeric(difftime(finished_at, started_at, units = "secs")), 3)
            ),
            validation_status = "valid",
            raw_output_text = response$raw_output_text
          )
        )

        reviewer_data$reviewer[row_index] <- reviewer_name
        reviewer_data$decision[row_index] <- result$decision
        reviewer_data$exclusion_reason[row_index] <- if (
          identical(result$decision, "Exclude")
        ) result$exclusion_reason else NA_character_
        reviewer_data$confidence[row_index] <- result$confidence
        reviewer_data$notes[row_index] <- paste0(
          "[Agent rationale] ", result$rationale,
          " [Reason code: ",
          ifelse(nzchar(result$exclusion_reason_code),
                 result$exclusion_reason_code, "NA"),
          "]"
        )
        reviewer_data$screened_at[row_index] <- format(
          finished_at,
          "%Y-%m-%d %H:%M:%S"
        )

        success <- TRUE
        TRUE
      },
      error = function(e) {
        last_error <<- conditionMessage(e)
        FALSE
      }
    )

    if (isTRUE(outcome)) break

    if (attempt < max_retries) {
      Sys.sleep(min(2 ^ (attempt - 1L), 10))
    }
  }

  if (!success) {
    failure_log <- dplyr::bind_rows(
      failure_log,
      tibble::tibble(
        screening_id = screening_id,
        row_index = row_index,
        provider = provider_name,
        model = model_name,
        attempts = max_retries,
        error = last_error,
        failed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    )
    cat("FAILED:", screening_id, "-", last_error, "\n")
  } else {
    cat("Completed:", screening_id, "\n")
  }

  processed_in_session <- processed_in_session + 1L

  if (processed_in_session %% checkpoint_every == 0L) {
    readr::write_csv(decision_log, checkpoint_file, na = "")
    if (nrow(failure_log) > 0L) {
      readr::write_csv(failure_log, failures_file, na = "")
    }
    cat("Checkpoint saved after", processed_in_session, "records.\n")
  }

  if (pause_seconds > 0) Sys.sleep(pause_seconds)
}

readr::write_csv(decision_log, decisions_file, na = "")
readr::write_csv(decision_log, checkpoint_file, na = "")

if (nrow(failure_log) > 0L) {
  readr::write_csv(failure_log, failures_file, na = "")
} else {
  readr::write_csv(
    tibble::tibble(
      screening_id = character(0),
      row_index = integer(0),
      provider = character(0),
      model = character(0),
      attempts = integer(0),
      error = character(0),
      failed_at = character(0)
    ),
    failures_file,
    na = ""
  )
}

file.copy(input_workbook, output_workbook, overwrite = TRUE)

workbook <- openxlsx::loadWorkbook(output_workbook)
openxlsx::writeData(
  workbook,
  sheet = "Reviewer_1",
  x = reviewer_data,
  startRow = 4,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE,
  withFilter = TRUE,
  keepNA = FALSE
)
openxlsx::saveWorkbook(workbook, output_workbook, overwrite = TRUE)

valid_decisions <- decision_log |>
  dplyr::filter(validation_status == "valid")

decision_summary <- valid_decisions |>
  dplyr::count(decision, name = "count")

reason_summary <- valid_decisions |>
  dplyr::filter(decision == "Exclude") |>
  dplyr::count(exclusion_reason_code, exclusion_reason, name = "count") |>
  dplyr::arrange(dplyr::desc(count))

confidence_summary <- valid_decisions |>
  dplyr::count(confidence, name = "count")

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "Agent Title/Abstract Screening Report",
  "",
  paste0("Project ID: ", project_id),
  paste0("Review title: ", project_title),
  paste0("Agent reviewer: ", reviewer_name),
  paste0("Provider: ", provider_name),
  paste0("Model: ", model_name),
  paste0("Prompt version: ", prompt_version),
  paste0("Generated at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "SUMMARY",
  "--------------------------------------------------",
  paste0("Records in Reviewer_1 sheet: ", nrow(reviewer_data)),
  paste0("Valid agent decisions logged: ", nrow(valid_decisions)),
  paste0("Failures: ", nrow(failure_log)),
  "",
  "DECISIONS",
  "--------------------------------------------------",
  if (nrow(decision_summary) > 0L)
    paste0(decision_summary$decision, ": ", decision_summary$count)
  else "No valid decisions",
  "",
  "CONFIDENCE",
  "--------------------------------------------------",
  if (nrow(confidence_summary) > 0L)
    paste0(confidence_summary$confidence, ": ", confidence_summary$count)
  else "No confidence results",
  "",
  "EXCLUSION REASONS",
  "--------------------------------------------------",
  if (nrow(reason_summary) > 0L)
    paste0(
      reason_summary$exclusion_reason_code, " | ",
      reason_summary$exclusion_reason, ": ",
      reason_summary$count
    )
  else "No exclusion reasons",
  "",
  "OUTPUT FILES",
  "--------------------------------------------------",
  output_workbook,
  decisions_file,
  failures_file,
  checkpoint_file,
  report_file,
  "",
  "HUMAN OVERSIGHT",
  "--------------------------------------------------",
  "Reviewer 2 remains a human reviewer.",
  "Conflict resolution and adjudication remain human responsibilities.",
  "Agent decisions require review under the finalized protocol."
)

writeLines(report_lines, report_file, useBytes = TRUE)

cat("\n====================================================\n")
cat("Agent Screening Complete\n")
cat("====================================================\n\n")
cat("Workbook:", output_workbook, "\n")
cat("Decision log:", decisions_file, "\n")
cat("Failure log:", failures_file, "\n")
cat("Report:", report_file, "\n")
