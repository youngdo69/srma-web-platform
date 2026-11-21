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

review_question <- config$research_question$question

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

inclusion_criteria <- config$eligibility$inclusion

if (
  is.null(inclusion_criteria) ||
  length(inclusion_criteria) == 0L
) {
  stop(
    "Canonical eligibility.inclusion is missing or empty.",
    call. = FALSE
  )
}

provider_config <- agent_config$provider
provider_config$timeout_seconds <- agent_config$processing$timeout_seconds %||% 120

provider_name <- provider_config$name
model_name <- provider_config$model
rate_card <- agent_config$cost_optimization$rate_card %||% NULL
reviewer_name <- agent_config$reviewer$name %||% "Agent Reviewer 1"
prompt_version <- agent_config$prompt$version %||% "unknown"

human_reviewer_2_name <- get_reviewer_name(
  config,
  reviewer = "reviewer_2",
  fallback = "Human Reviewer 2"
)

human_adjudicator_name <- get_reviewer_name(
  config,
  reviewer = "third_reviewer",
  fallback = "Human Adjudicator"
)

if (
  identical(
    trimws(tolower(reviewer_name)),
    trimws(tolower(human_reviewer_2_name))
  ) ||
  identical(
    trimws(tolower(reviewer_name)),
    trimws(tolower(human_adjudicator_name))
  )
) {
  stop(
    "Agent Reviewer 1 must be distinct from the human Reviewer 2 ",
    "and human adjudicator identities.",
    call. = FALSE
  )
}

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

# completed_ids will be finalized only after checkpoint rows are
# revalidated against the CURRENT project screening policy.
completed_ids <- character(0)


# ============================================================
# Resume hydration
# ------------------------------------------------------------
# When resuming from a checkpoint, Reviewer_1 is reloaded from
# the pristine R/18 workbook. Therefore previously completed
# checkpoint decisions must be restored into reviewer_data
# before those records are skipped in the main loop.
# ============================================================

if (
  resume_enabled &&
  nrow(decision_log) > 0L
) {

  valid_checkpoint_rows <- decision_log[
    decision_log$validation_status == "valid",
    ,
    drop = FALSE
  ]

  # Revalidate historical checkpoint rows against the CURRENT
  # decision and exclusion-reason policy before restoring them.
  current_reason_pairs <- paste(
    exclusion_reason_table$code,
    exclusion_reason_table$label,
    sep = "||"
  )

  checkpoint_policy_valid <- vapply(
    seq_len(nrow(valid_checkpoint_rows)),
    function(i) {
      row <- valid_checkpoint_rows[i, , drop = FALSE]

      decision <- as.character(row$decision[[1]])
      reason_code <- as.character(row$exclusion_reason_code[[1]])
      reason_label <- as.character(row$exclusion_reason[[1]])
      confidence <- as.character(row$confidence[[1]])

      # readr treats empty CSV fields as NA by default. Non-Exclude
      # decisions legitimately have blank reason fields; normalize those
      # missing reason values before validating the saved decision.
      if (is.na(reason_code)) reason_code <- ""
      if (is.na(reason_label)) reason_label <- ""

      if (!decision %in% decision_options) {
        return(FALSE)
      }

      if (!confidence %in% agent_config$decision_policy$allowed_confidence) {
        return(FALSE)
      }

      if (identical(decision, "Exclude")) {
        return(
          paste(reason_code, reason_label, sep = "||") %in%
            current_reason_pairs
        )
      }

      !nzchar(trimws(reason_code)) &&
        !nzchar(trimws(reason_label))
    },
    logical(1)
  )

  invalid_checkpoint_rows <- valid_checkpoint_rows[
    !checkpoint_policy_valid,
    ,
    drop = FALSE
  ]

  if (nrow(invalid_checkpoint_rows) > 0L) {
    cat(
      "Checkpoint policy refresh: ",
      nrow(invalid_checkpoint_rows),
      " previously valid record(s) will be re-screened because ",
      "they no longer satisfy the current screening policy.\n",
      sep = ""
    )
  }

  valid_checkpoint_rows <- valid_checkpoint_rows[
    checkpoint_policy_valid,
    ,
    drop = FALSE
  ]

  # Keep only currently valid checkpoint rows in the in-memory log.
  # Re-screened IDs will replace stale historical rows in this run.
  decision_log <- valid_checkpoint_rows

  completed_ids <- unique(
    valid_checkpoint_rows$screening_id
  )

  if (anyDuplicated(valid_checkpoint_rows$screening_id) > 0L) {
    stop(
      "Checkpoint contains duplicated valid screening IDs.",
      call. = FALSE
    )
  }

  checkpoint_index <- match(
    reviewer_data$screening_id,
    valid_checkpoint_rows$screening_id
  )

  restore_rows <- which(
    !is.na(checkpoint_index)
  )

  if (length(restore_rows) > 0L) {

    checkpoint_rows <- valid_checkpoint_rows[
      checkpoint_index[restore_rows],
      ,
      drop = FALSE
    ]

    reviewer_data$reviewer[restore_rows] <-
      checkpoint_rows$reviewer

    reviewer_data$decision[restore_rows] <-
      checkpoint_rows$decision

    reviewer_data$exclusion_reason[restore_rows] <-
      ifelse(
        checkpoint_rows$decision == "Exclude",
        checkpoint_rows$exclusion_reason,
        NA_character_
      )

    reviewer_data$confidence[restore_rows] <-
      checkpoint_rows$confidence

    reviewer_data$notes[restore_rows] <- paste0(
      "[Agent rationale] ",
      checkpoint_rows$rationale,
      " [Reason code: ",
      ifelse(
        nzchar(checkpoint_rows$exclusion_reason_code),
        checkpoint_rows$exclusion_reason_code,
        "NA"
      ),
      "]"
    )

    reviewer_data$screened_at[restore_rows] <-
      checkpoint_rows$completed_at

    cat(
      "Restored ",
      length(restore_rows),
      " valid checkpoint decision(s) into Reviewer_1.\n",
      sep = ""
    )
  }
}

json_schema <- build_screening_json_schema(
  decision_options = decision_options,
  exclusion_reason_codes = exclusion_reason_table$code,
  exclusion_reason_labels = exclusion_reason_table$label,
  confidence_options = agent_config$decision_policy$allowed_confidence
)

validate_agent_result_universal <- function(
    result,
    expected_screening_id,
    decision_options,
    exclusion_reason_table,
    allowed_confidence
) {
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

  if (!result$confidence %in% allowed_confidence) {
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

# IMPORTANT FOR PSOCK PARALLELISM:
# R/19 is also executed by the Web adapter through sys.source(..., envir = <private env>).
# A function defined in that private environment can otherwise carry the entire
# mutable source environment when serialized to PSOCK workers. Once that environment
# contains the cluster object itself, serializing the worker function can traverse
# live socket connections and fail with errors such as "invalid connection".
#
# These worker-safe functions depend only on explicit arguments plus provider
# utilities sourced into .GlobalEnv. Give them a clean global enclosure before any
# cluster object is created so no live connection can become part of their closure.
environment(validate_agent_result_universal) <- globalenv()

validate_agent_result <- function(result, expected_screening_id) {
  validate_agent_result_universal(
    result = result,
    expected_screening_id = expected_screening_id,
    decision_options = decision_options,
    exclusion_reason_table = exclusion_reason_table,
    allowed_confidence = agent_config$decision_policy$allowed_confidence
  )
}

checkpoint_every <- as.integer(
  agent_config$processing$checkpoint_every %||% 10L
)

if (is.na(checkpoint_every) || checkpoint_every < 1L) {
  checkpoint_every <- 10L
}

max_retries <- as.integer(
  agent_config$processing$max_retries %||% 3L
)

if (is.na(max_retries) || max_retries < 1L) {
  max_retries <- 3L
}

pause_seconds <- as.numeric(
  agent_config$processing$request_pause_seconds %||% 0
)

if (!is.finite(pause_seconds) || pause_seconds < 0) {
  pause_seconds <- 0
}

parallel_enabled <- isTRUE(
  agent_config$processing$parallel_enabled %||% FALSE
)

parallel_workers <- as.integer(
  agent_config$processing$parallel_workers %||% 1L
)

parallel_max_workers <- as.integer(
  agent_config$processing$parallel_max_workers %||% 6L
)

if (is.na(parallel_workers) || parallel_workers < 1L) {
  parallel_workers <- 1L
}

if (is.na(parallel_max_workers) || parallel_max_workers < 1L) {
  parallel_max_workers <- 6L
}

parallel_workers <- min(
  parallel_workers,
  parallel_max_workers
)

if (!parallel_enabled) {
  parallel_workers <- 1L
}

# Keep parallelism deliberately bounded. Title/abstract screening is network-bound,
# but every record remains an independent provider request with its own response ID,
# validation, retry history, and checkpoint row.
parallel_workers <- max(1L, min(parallel_workers, 6L))


# ============================================================
# 14. Universal per-record screening task
# ------------------------------------------------------------
# The scientific contract remains record-level:
#   one record -> one prompt -> one structured response -> one validation result.
# Parallelism changes wall-clock scheduling only; it does not batch multiple studies
# into one LLM judgment and therefore preserves record-level provenance and retries.
# ============================================================

screen_record_task <- function(
    task,
    provider_name,
    provider_config,
    json_schema,
    decision_options,
    exclusion_reason_table,
    allowed_confidence,
    validator,
    max_retries,
    pause_seconds
) {
  last_error <- NA_character_

  for (attempt in seq_len(max_retries)) {
    started_at <- Sys.time()

    outcome <- tryCatch(
      {
        response <- call_llm_screening_provider(
          provider_name = provider_name,
          prompt = task$prompt,
          provider_config = provider_config,
          json_schema = json_schema
        )

        validation_status <- validator(
          result = response$parsed,
          expected_screening_id = task$screening_id,
          decision_options = decision_options,
          exclusion_reason_table = exclusion_reason_table,
          allowed_confidence = allowed_confidence
        )

        if (!identical(validation_status, "valid")) {
          stop(
            "Agent result validation failed: ",
            validation_status,
            call. = FALSE
          )
        }

        finished_at <- Sys.time()

        list(
          success = TRUE,
          row_index = task$row_index,
          screening_id = task$screening_id,
          result = response$parsed,
          response_id = as.character(response$response_id),
          response_model = as.character(response$model),
          raw_output_text = as.character(response$raw_output_text),
          usage = response$usage,
          attempt = attempt,
          started_at = format(started_at, "%Y-%m-%d %H:%M:%S"),
          completed_at = format(finished_at, "%Y-%m-%d %H:%M:%S"),
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
          error = NA_character_
        )
      },
      error = function(e) {
        # Quota exhaustion is not a record-specific screening failure.
        # Return a serializable signal instead of retrying this record.
        if (inherits(e, "srma_ai_quota_exhausted")) {
          return(list(
            success = FALSE,
            fatal_type = "insufficient_quota",
            row_index = task$row_index,
            screening_id = task$screening_id,
            result = NULL,
            response_id = NA_character_,
            response_model = NA_character_,
            raw_output_text = NA_character_,
            attempt = attempt,
            started_at = format(started_at, "%Y-%m-%d %H:%M:%S"),
            completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            elapsed_seconds = NA_character_,
            error = conditionMessage(e)
          ))
        }
        last_error <<- conditionMessage(e)
        NULL
      }
    )

    if (!is.null(outcome)) {
      if (pause_seconds > 0) {
        Sys.sleep(pause_seconds)
      }
      return(outcome)
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

  if (pause_seconds > 0) {
    Sys.sleep(pause_seconds)
  }

  list(
    success = FALSE,
    row_index = task$row_index,
    screening_id = task$screening_id,
    result = NULL,
    response_id = NA_character_,
    response_model = NA_character_,
    raw_output_text = NA_character_,
    attempt = max_retries,
    started_at = NA_character_,
    completed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    elapsed_seconds = NA_character_,
    error = last_error
  )
}

# Keep the record-level worker closure free of the private sys.source environment
# for the same reason as validate_agent_result_universal above.
environment(screen_record_task) <- globalenv()


# ============================================================
# 15. Build pending record tasks
# ============================================================

pending_indices <- integer(0)

for (row_index in record_indices) {
  screening_id <- as.character(
    reviewer_data$screening_id[[row_index]]
  )

  if (screening_id %in% completed_ids) {
    next
  }

  existing_decision <- as.character(
    reviewer_data$decision[[row_index]]
  )

  if (
    !overwrite_existing &&
    !is.na(existing_decision) &&
    nzchar(trimws(existing_decision))
  ) {
    next
  }

  pending_indices <- c(
    pending_indices,
    row_index
  )
}

cat("\n====================================================\n")
cat("Agent Title/Abstract Screening\n")
cat("====================================================\n\n")
cat("Project:", project_title, "\n")
cat("Provider:", provider_name, "\n")
cat("Model:", model_name, "\n")
cat("Reviewer:", reviewer_name, "\n")
cat("Records in configured range:", length(record_indices), "\n")
cat("Valid checkpoint decisions restored:", length(completed_ids), "\n")
cat("Records remaining in this run:", length(pending_indices), "\n")
cat("Parallel scheduling:", if (parallel_workers > 1L) "ENABLED" else "DISABLED", "\n")
cat("Parallel workers:", parallel_workers, "\n\n")

if (length(pending_indices) == 0L) {
  cat("No new records require Agent Reviewer 1 screening.\n")
}


# ============================================================
# 16. Start bounded parallel cluster
# ============================================================

cluster <- NULL
parallel_active <- FALSE

if (
  length(pending_indices) > 0L &&
  parallel_workers > 1L
) {
  cluster <- tryCatch(
    parallel::makePSOCKcluster(
      parallel_workers
    ),
    error = function(e) e
  )

  if (inherits(cluster, "error")) {
    warning(
      "Parallel screening could not start; falling back to sequential mode. ",
      conditionMessage(cluster),
      call. = FALSE
    )
    cluster <- NULL
    parallel_workers <- 1L
  } else {
    worker_root <- getwd()

    setup_ok <- tryCatch(
      {
        parallel::clusterCall(
          cluster,
          function(path) {
            setwd(path)
            invisible(getwd())
          },
          worker_root
        )

        parallel::clusterEvalQ(
          cluster,
          {
            source("R/utils/llm_provider_utils.R")
            NULL
          }
        )

        TRUE
      },
      error = function(e) {
        warning(
          "Parallel worker initialization failed; falling back to sequential mode. ",
          conditionMessage(e),
          call. = FALSE
        )
        FALSE
      }
    )

    if (!isTRUE(setup_ok)) {
      try(
        parallel::stopCluster(cluster),
        silent = TRUE
      )
      cluster <- NULL
      parallel_workers <- 1L
    } else {
      parallel_active <- TRUE
    }
  }
}


# ============================================================
# 17. Execute screening in small bounded batches
# ------------------------------------------------------------
# A batch is only a scheduling/checkpoint unit. Each study is still
# sent as an independent LLM request. Small batches allow frequent
# checkpointing and limit work lost after interruption.
# ============================================================

# Wall-clock throughput measures scheduling performance rather than just
# per-request latency. Historical checkpoint rows are excluded from the run rate.
# Rate/ETA are advisory; they never affect eligibility, retries or checkpoint IDs.
session_started_at <- Sys.time()
processed_in_session <- 0L
successful_in_session <- 0L
processed_since_checkpoint <- 0L
batch_size <- max(1L, parallel_workers)

srma_screening_throughput <- function(attempted, succeeded, pending, elapsed_seconds) {
  attempted <- as.integer(attempted)
  succeeded <- as.integer(succeeded)
  pending <- as.integer(pending)
  elapsed_seconds <- as.numeric(elapsed_seconds)
  if (length(attempted) != 1L || length(succeeded) != 1L ||
      length(pending) != 1L || length(elapsed_seconds) != 1L ||
      anyNA(c(attempted,succeeded,pending,elapsed_seconds)) ||
      attempted < 0L || succeeded < 0L || succeeded > attempted ||
      pending < 0L || !is.finite(elapsed_seconds) || elapsed_seconds <= 0) {
    return(list(attempts_per_min = NA_real_, successes_per_min = NA_real_,
                eta_min = NA_real_))
  }
  rate <- 60 * attempted / elapsed_seconds
  success_rate <- 60 * succeeded / elapsed_seconds
  remaining <- max(0L, pending - attempted)
  list(attempts_per_min = rate, successes_per_min = success_rate,
       eta_min = if (rate > 0) remaining / rate else NA_real_)
}

make_task <- function(row_index) {
  record <- reviewer_data[
    row_index,
    ,
    drop = FALSE
  ]

  screening_id <- as.character(
    record$screening_id[[1]]
  )

  prompt <- build_agent_screening_prompt(
    template_text = prompt_template,
    record = as.list(
      record[
        1,
        ,
        drop = FALSE
      ]
    ),
    project_id = project_id,
    project_title = project_title,
    review_question = review_question,
    inclusion_criteria = inclusion_criteria,
    exclusion_reason_table = exclusion_reason_table
  )

  list(
    row_index = row_index,
    screening_id = screening_id,
    prompt = prompt
  )
}

persist_checkpoint <- function() {
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

  invisible(TRUE)
}

if (length(pending_indices) > 0L) {
  batch_starts <- seq.int(
    from = 1L,
    to = length(pending_indices),
    by = batch_size
  )

  for (batch_number in seq_along(batch_starts)) {
    batch_start <- batch_starts[[batch_number]]
    batch_end <- min(
      length(pending_indices),
      batch_start + batch_size - 1L
    )

    batch_indices <- pending_indices[
      batch_start:batch_end
    ]

    tasks <- lapply(
      batch_indices,
      make_task
    )

    run_sequential_batch <- function(tasks) {
      lapply(
        tasks,
        screen_record_task,
        provider_name = provider_name,
        provider_config = provider_config,
        json_schema = json_schema,
        decision_options = decision_options,
        exclusion_reason_table = exclusion_reason_table,
        allowed_confidence = agent_config$decision_policy$allowed_confidence,
        validator = validate_agent_result_universal,
        max_retries = max_retries,
        pause_seconds = pause_seconds
      )
    }

    if (isTRUE(parallel_active)) {
      parallel_attempt <- tryCatch(
        parallel::parLapplyLB(
          cluster,
          tasks,
          screen_record_task,
          provider_name = provider_name,
          provider_config = provider_config,
          json_schema = json_schema,
          decision_options = decision_options,
          exclusion_reason_table = exclusion_reason_table,
          allowed_confidence = agent_config$decision_policy$allowed_confidence,
          validator = validate_agent_result_universal,
          max_retries = max_retries,
          pause_seconds = pause_seconds
        ),
        error = function(e) e
      )

      if (inherits(parallel_attempt, "error")) {
        warning(
          "Parallel screening transport failed; continuing this run sequentially ",
          "from the same preserved checkpoint. ",
          conditionMessage(parallel_attempt),
          call. = FALSE
        )

        try(
          parallel::stopCluster(cluster),
          silent = TRUE
        )

        cluster <- NULL
        parallel_active <- FALSE
        parallel_workers <- 1L

        batch_results <- run_sequential_batch(tasks)
      } else {
        batch_results <- parallel_attempt
      }
    } else {
      batch_results <- run_sequential_batch(tasks)
    }

    for (outcome in batch_results) {
      row_index <- as.integer(
        outcome$row_index
      )
      screening_id <- as.character(
        outcome$screening_id
      )

      if (isTRUE(outcome$success)) {
        result <- outcome$result

        decision_log <- dplyr::bind_rows(
          decision_log,
          tibble::tibble(
            screening_id = screening_id,
            decision = as.character(result$decision),
            exclusion_reason_code = as.character(
              result$exclusion_reason_code
            ),
            exclusion_reason = as.character(
              result$exclusion_reason
            ),
            confidence = as.character(
              result$confidence
            ),
            rationale = as.character(
              result$rationale
            ),
            reviewer = reviewer_name,
            provider = provider_name,
            model = as.character(
              outcome$response_model
            ),
            prompt_version = prompt_version,
            response_id = as.character(
              outcome$response_id
            ),
            attempt = as.character(
              outcome$attempt
            ),
            started_at = as.character(
              outcome$started_at
            ),
            completed_at = as.character(
              outcome$completed_at
            ),
            elapsed_seconds = as.character(
              outcome$elapsed_seconds
            ),
            validation_status = "valid",
            raw_output_text = as.character(
              outcome$raw_output_text
            ),
            input_tokens = as.character(outcome$usage$input_tokens %||% NA_real_),
            cached_input_tokens = as.character(outcome$usage$cached_input_tokens %||% NA_real_),
            cache_write_tokens = as.character(outcome$usage$cache_write_tokens %||% NA_real_),
            output_tokens = as.character(outcome$usage$output_tokens %||% NA_real_),
            reasoning_tokens = as.character(outcome$usage$reasoning_tokens %||% NA_real_),
            estimated_cost_usd = as.character(estimate_openai_screening_usd(
              outcome$usage, rate_card, outcome$response_model
            )),
            reasoning_effort = as.character(provider_config$reasoning_effort %||% NA_character_),
            max_output_tokens = as.character(provider_config$max_output_tokens %||% NA_integer_)
          )
        )

        reviewer_data$reviewer[row_index] <- reviewer_name
        reviewer_data$decision[row_index] <- as.character(
          result$decision
        )
        reviewer_data$exclusion_reason[row_index] <- if (
          identical(
            as.character(result$decision),
            "Exclude"
          )
        ) {
          as.character(
            result$exclusion_reason
          )
        } else {
          NA_character_
        }

        reviewer_data$confidence[row_index] <- as.character(
          result$confidence
        )

        reviewer_data$notes[row_index] <- paste0(
          "[Agent rationale] ",
          as.character(result$rationale),
          " [Reason code: ",
          ifelse(
            nzchar(
              as.character(
                result$exclusion_reason_code
              )
            ),
            as.character(
              result$exclusion_reason_code
            ),
            "NA"
          ),
          "]"
        )

        reviewer_data$screened_at[row_index] <- as.character(
          outcome$completed_at
        )

        cat(
          "Completed: ",
          screening_id,
          " | attempt ",
          outcome$attempt,
          " | ",
          outcome$elapsed_seconds,
          " sec\n",
          sep = ""
        )
      } else {
        failure_log <- dplyr::bind_rows(
          failure_log,
          tibble::tibble(
            screening_id = screening_id,
            row_index = row_index,
            provider = provider_name,
            model = model_name,
            attempts = if (is.null(outcome$attempt)) max_retries else outcome$attempt,
            error = as.character(
              outcome$error
            ),
            failed_at = as.character(
              outcome$completed_at
            )
          )
        )

        cat(
          "FAILED: ",
          screening_id,
          " - ",
          as.character(outcome$error),
          "\n",
          sep = ""
        )
      }
    }

    # Commit every successful record in this completed batch, then stop the
    # entire screening run on a non-retryable billing/quota failure. In a
    # four-worker batch, other workers may have returned valid decisions.
    quota_exhausted <- any(vapply(
      batch_results,
      function(outcome) identical(outcome$fatal_type, "insufficient_quota"),
      logical(1)
    ))
    if (quota_exhausted) {
      persist_checkpoint()
      if (!is.null(cluster)) {
        try(parallel::stopCluster(cluster), silent = TRUE)
        cluster <- NULL
        parallel_active <- FALSE
      }
      stop(structure(
        list(message = paste0(
          "OpenAI API credits exhausted (HTTP 429: insufficient_quota). ",
          "Saved valid screening decisions to checkpoint; ",
          "add API credits before retrying."
        ), call = NULL),
        class = c("srma_ai_quota_exhausted", "error", "condition")
      ))
    }

    processed_in_session <- processed_in_session +
      length(batch_results)
    successful_in_session <- successful_in_session +
      sum(vapply(batch_results, function(x) isTRUE(x$success), logical(1)))

    processed_since_checkpoint <- processed_since_checkpoint +
      length(batch_results)

    if (
      processed_since_checkpoint >= checkpoint_every ||
      batch_number == length(batch_starts)
    ) {
      persist_checkpoint()

      cat(
        "Checkpoint saved: ",
        nrow(decision_log),
        " valid decision(s) recorded; ",
        processed_in_session,
        " record(s) attempted in this session.\n",
        sep = ""
      )
      speed <- srma_screening_throughput(
        attempted = processed_in_session,
        succeeded = successful_in_session,
        pending = length(pending_indices),
        elapsed_seconds = as.numeric(difftime(Sys.time(), session_started_at, units="secs"))
      )
      if (is.finite(speed$attempts_per_min)) {
        cat(sprintf(
          "SCREENING_THROUGHPUT: attempted=%.1f/min; successful=%.1f/min; estimated_remaining=%.1f min (advisory)\n",
          speed$attempts_per_min, speed$successes_per_min, speed$eta_min
        ))
      }

      processed_since_checkpoint <- 0L
    }
  }
}

if (!is.null(cluster)) {
  try(
    parallel::stopCluster(cluster),
    silent = TRUE
  )
  cluster <- NULL
  parallel_active <- FALSE
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
  paste0("Parallel scheduling enabled: ", parallel_enabled),
  paste0("Parallel workers used: ", parallel_workers),
  paste0("Current-run attempts: ", processed_in_session),
  paste0("Current-run successful decisions: ", successful_in_session),
  paste0("Current-run wall-clock seconds: ",
    round(as.numeric(difftime(Sys.time(),session_started_at,units="secs")),2)),
  paste0("Generated at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "API USAGE TELEMETRY (SUCCESSFUL RESPONSES ONLY)",
  "--------------------------------------------------",
  "Historical and failed/retried requests may be missing: not an invoice.",
  paste0("Successful records with usage: ", sum(!is.na(suppressWarnings(
    as.numeric(decision_log$input_tokens %||% numeric(0)))))),
  paste0("Recorded input tokens: ", sum(suppressWarnings(
    as.numeric(decision_log$input_tokens %||% numeric(0))), na.rm = TRUE)),
  paste0("Recorded output tokens: ", sum(suppressWarnings(
    as.numeric(decision_log$output_tokens %||% numeric(0))), na.rm = TRUE)),
  paste0("Recorded reasoning tokens: ", sum(suppressWarnings(
    as.numeric(decision_log$reasoning_tokens %||% numeric(0))), na.rm = TRUE)),
  paste0("Records with estimable cost: ", sum(!is.na(suppressWarnings(
    as.numeric(decision_log$estimated_cost_usd %||% numeric(0)))))),
  paste0("Partial estimated successful-request USD (known records only): ",
    if (sum(!is.na(suppressWarnings(as.numeric(
      decision_log$estimated_cost_usd %||% numeric(0))))) > 0L)
      sprintf("%.6f", sum(suppressWarnings(
        as.numeric(decision_log$estimated_cost_usd)), na.rm = TRUE))
    else "UNKNOWN"),
  "No total spend or hard spending limit is implied by partial telemetry.",
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

# ============================================================
# TASK-008 R/19 standalone validation
# ============================================================

required_outputs <- c(
  agent_workbook = output_workbook,
  decision_log = decisions_file,
  failure_log = failures_file,
  checkpoint = checkpoint_file,
  report = report_file
)

output_nonempty <- vapply(
  required_outputs,
  function(file_path) {
    file.exists(file_path) &&
      !is.na(file.info(file_path)$size) &&
      file.info(file_path)$size > 0
  },
  logical(1)
)

if (!all(output_nonempty)) {
  failed_outputs <- names(required_outputs)[!output_nonempty]

  stop(
    "TASK-008 R/19 standalone validation failed. Missing or empty output(s): ",
    paste(failed_outputs, collapse = ", "),
    call. = FALSE
  )
}

if (nrow(failure_log) > 0L) {
  stop(
    "TASK-008 R/19 standalone validation failed: ",
    nrow(failure_log),
    " record(s) failed agent screening.",
    call. = FALSE
  )
}

agent_nonblank <- !is.na(reviewer_data$decision) &
  nzchar(trimws(as.character(reviewer_data$decision)))

if (!all(agent_nonblank)) {
  stop(
    "TASK-008 R/19 standalone validation failed: ",
    sum(!agent_nonblank),
    " Reviewer_1 record(s) remain unscreened.",
    call. = FALSE
  )
}

cat("\n====================================================\n")
cat("Agent Screening Complete\n")
cat("====================================================\n\n")
cat("Workbook:", output_workbook, "\n")
cat("Decision log:", decisions_file, "\n")
cat("Failure log:", failures_file, "\n")
cat("Report:", report_file, "\n")
cat("\nPASS: TASK-008 R/19 Agent Reviewer 1 screening completed successfully.\n")
