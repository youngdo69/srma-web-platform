# ============================================================
# Universal SR-MA Agent Toolkit
# R/25A_generate_research_scope_advisory.R
#
# OPTIONAL Research Scope Advisory
#
# Workflow position:
# TASK-013 -> [R/25A optional advisory] -> TASK-014
#
# This is NOT a new TASK.
#
# Standalone:
#   source("R/25A_generate_research_scope_advisory.R")
#
# Runner-ready control:
#   Sys.setenv(SRMA_SCOPE_ADVISORY = "yes")  # run advisory
#   Sys.setenv(SRMA_SCOPE_ADVISORY = "no")   # skip advisory
#
# Optional model override:
#   Sys.setenv(SRMA_ADVISORY_MODEL = "gpt-5.6")
#
# Governance boundary:
# - Advisory only.
# - Does not change Included Studies.
# - Does not change extracted values.
# - Does not perform TASK-014 Human Verification.
# - Does not remove studies from the systematic review.
# ============================================================

required_packages <- c(
  "yaml", "readr", "dplyr", "tibble", "stringr",
  "jsonlite", "httr2", "dotenv", "openxlsx"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) return("")
  as.character(x[[1]])
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_squish(x)
  x[is.na(x) | x == ""] <- NA_character_
  x
}

now_text <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

json_chr <- function(x, pretty = FALSE) {
  as.character(
    jsonlite::toJSON(
      x,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = NA,
      pretty = pretty
    )
  )
}

as_chr_vec <- function(x) {
  if (is.null(x)) return(character(0))
  y <- as.character(unlist(x, recursive = TRUE, use.names = FALSE))
  y <- trimws(y)
  unique(y[!is.na(y) & nzchar(y)])
}

collapse_unique <- function(x) {
  y <- sort(unique(clean_text(x)))
  y <- y[!is.na(y)]
  if (length(y) == 0L) "" else paste(y, collapse = " | ")
}

normalize_yes_no <- function(x) {
  x <- tolower(trimws(safe_chr(x)))
  if (x %in% c("yes", "y", "true", "1")) return("yes")
  if (x %in% c("no", "n", "false", "0")) return("no")
  ""
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

if (
  is.null(config$data_extraction) ||
  !isTRUE(config$data_extraction$agent_extraction) ||
  !isTRUE(config$data_extraction$human_verification)
) {
  stop(
    "R/25A requires DEC-008 configuration:\n",
    "data_extraction:\n",
    "  agent_extraction: yes\n",
    "  human_verification: yes",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Optional Yes/No gate
# Priority:
# 1) R option
# 2) environment variable
# 3) interactive question
# 4) non-interactive safe skip
# ------------------------------------------------------------

resolve_choice <- function() {
  x <- normalize_yes_no(getOption("srma.scope_advisory", ""))
  if (nzchar(x)) return(list(choice = x, source = "R option"))

  x <- normalize_yes_no(Sys.getenv("SRMA_SCOPE_ADVISORY", unset = ""))
  if (nzchar(x)) return(list(choice = x, source = "environment"))

  if (!interactive()) {
    return(list(choice = "no", source = "non-interactive safe skip"))
  }

  cat("\n")
  cat("Would you like to receive an expert advisory on the optimal\n")
  cat("research question and endpoints based on the extracted data?\n")
  cat("[Yes/No]: ")

  x <- normalize_yes_no(readline())

  while (!nzchar(x)) {
    cat("Please enter Yes or No: ")
    x <- normalize_yes_no(readline())
  }

  list(choice = x, source = "interactive user response")
}

choice <- resolve_choice()

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

included_file <- file.path(
  paths$project_root,
  "data", "processed", "full_text", "included_studies.csv"
)

extraction_dir <- file.path(
  paths$project_root,
  "data", "processed", "extraction"
)

study_file <- file.path(extraction_dir, "study_characteristics_agent.csv")
group_file <- file.path(extraction_dir, "group_characteristics_agent.csv")
outcome_file <- file.path(extraction_dir, "outcome_data_agent.csv")

output_dir <- file.path(
  paths$project_root,
  "results", "extraction", "research_scope_advisory"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

json_file <- file.path(output_dir, "research_scope_advisory.json")
md_file <- file.path(output_dir, "research_scope_advisory.md")
xlsx_file <- file.path(output_dir, "research_scope_advisory.xlsx")
candidate_file <- file.path(output_dir, "candidate_topics.csv")
harmonization_file <- file.path(output_dir, "outcome_harmonization.csv")
sets_file <- file.path(output_dir, "candidate_meta_analysis_sets.csv")
roles_file <- file.path(output_dir, "study_roles.csv")
report_file <- file.path(
  paths$logs_directory,
  "research_scope_advisory_report.txt"
)

if (identical(choice$choice, "no")) {
  writeLines(
    c(
      "Universal SR-MA Agent Toolkit",
      "Optional Research Scope Advisory",
      "",
      paste0("Generated at: ", now_text()),
      paste0("Project ID: ", project_id),
      paste0("Choice source: ", choice$source),
      "",
      "STATUS: SKIPPED",
      "Proceed to TASK-014 — Human Verification.",
      "Runner integration performed here: NO"
    ),
    report_file
  )

  cat("\nSKIPPED: Optional Research Scope Advisory.\n")
  cat("Proceed to TASK-014 — Human Verification.\n")

} else {

# ------------------------------------------------------------
# Read TASK-013 outputs
# ------------------------------------------------------------

required_files <- c(included_file, study_file, group_file, outcome_file)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0L) {
  stop(
    "Missing TASK-013 output(s):\n- ",
    paste(missing_files, collapse = "\n- "),
    call. = FALSE
  )
}

read_agent_csv <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character())
  )
}

included <- read_agent_csv(included_file)
study <- read_agent_csv(study_file)
groups <- read_agent_csv(group_file)
outcomes <- read_agent_csv(outcome_file)

if (nrow(included) == 0L || nrow(study) == 0L || nrow(outcomes) == 0L) {
  stop("R/25A requires non-empty TASK-013 extraction outputs.", call. = FALSE)
}

required_outcome_cols <- c(
  "screening_id", "outcome_name", "outcome_definition",
  "timepoint", "data_type"
)

missing_cols <- setdiff(required_outcome_cols, names(outcomes))

if (length(missing_cols) > 0L) {
  stop(
    "Outcome data missing required column(s): ",
    paste(missing_cols, collapse = ", "),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Canonical project PICO/outcomes
# ------------------------------------------------------------

population <- safe_chr(config$research_question$population)
intervention <- safe_chr(config$research_question$intervention)
comparator <- safe_chr(config$research_question$comparator)

primary_outcomes <- as_chr_vec(config$research_question$outcomes$primary)
secondary_outcomes <- as_chr_vec(config$research_question$outcomes$secondary)
canonical_outcomes <- unique(c(primary_outcomes, secondary_outcomes))

# ------------------------------------------------------------
# Deterministic evidence summary
# ------------------------------------------------------------

effect_fields <- intersect(
  c(
    "intervention_events", "intervention_total",
    "comparator_events", "comparator_total",
    "intervention_mean", "intervention_sd", "intervention_n",
    "comparator_mean", "comparator_sd", "comparator_n",
    "effect_measure", "effect_estimate", "lower_ci", "upper_ci"
  ),
  names(outcomes)
)

if (length(effect_fields) > 0L) {
  outcomes$has_effect_data <- apply(
    outcomes[, effect_fields, drop = FALSE],
    1,
    function(z) any(!is.na(clean_text(z)))
  )
} else {
  outcomes$has_effect_data <- FALSE
}

outcome_summary <- outcomes |>
  dplyr::group_by(outcome_name) |>
  dplyr::summarise(
    unique_studies = dplyr::n_distinct(screening_id),
    total_rows = dplyr::n(),
    rows_with_effect_data = sum(has_effect_data),
    unique_definitions = dplyr::n_distinct(
      clean_text(outcome_definition),
      na.rm = TRUE
    ),
    unique_timepoints = dplyr::n_distinct(
      clean_text(timepoint),
      na.rm = TRUE
    ),
    data_types = collapse_unique(data_type),
    timepoints = collapse_unique(timepoint),
    .groups = "drop"
  ) |>
  dplyr::arrange(dplyr::desc(unique_studies), outcome_name)

if ("study_design" %in% names(study)) {
  study_design_summary <- study |>
    dplyr::mutate(study_design = clean_text(study_design)) |>
    dplyr::count(study_design, name = "study_count", sort = TRUE)
} else {
  study_design_summary <- tibble::tibble()
}

study_fields <- intersect(
  c(
    "screening_id", "title", "authors", "year", "journal",
    "study_design", "country", "analysis_population",
    "total_sample_size", "surgery_or_procedure",
    "intervention_or_exposure", "exposure_timing",
    "comparator", "follow_up",
    "agent_evidence_source", "agent_extraction_status"
  ),
  names(study)
)

outcome_fields <- intersect(
  c(
    "screening_id", "outcome_row_id", "outcome_priority",
    "outcome_name", "outcome_definition", "timepoint", "data_type",
    "intervention_events", "intervention_total",
    "comparator_events", "comparator_total",
    "intervention_mean", "intervention_sd", "intervention_n",
    "comparator_mean", "comparator_sd", "comparator_n",
    "unit", "effect_measure", "effect_estimate",
    "lower_ci", "upper_ci", "adjusted",
    "adjustment_variables", "source_location",
    "source_evidence", "extraction_notes"
  ),
  names(outcomes)
)

group_fields <- intersect(
  c(
    "screening_id", "group_index", "group_name",
    "group_role", "sample_size", "baseline_characteristics_notes"
  ),
  names(groups)
)

study_compact <- study[, study_fields, drop = FALSE]
outcome_compact <- outcomes[, outcome_fields, drop = FALSE]
group_compact <- groups[, group_fields, drop = FALSE]

# ------------------------------------------------------------
# Provider/model
# ------------------------------------------------------------

agent_config_file <- "config/agent_screening.yml"

if (!file.exists(agent_config_file)) {
  stop("Missing config/agent_screening.yml", call. = FALSE)
}

agent_doc <- yaml::read_yaml(agent_config_file)
agent_cfg <- agent_doc$agent_screening

if (is.null(agent_cfg) || !isTRUE(agent_cfg$enabled)) {
  stop("agent_screening provider configuration is unavailable.", call. = FALSE)
}

load_project_env(".env")

provider_cfg <- agent_cfg$provider
provider_name <- tolower(safe_chr(provider_cfg$name))

if (!identical(provider_name, "openai")) {
  stop("R/25A currently supports the OpenAI provider.", call. = FALSE)
}

api_key_env <- provider_cfg$api_key_env %||% "OPENAI_API_KEY"
api_key <- get_required_environment_value(api_key_env)

base_url <- sub(
  "/+$",
  "",
  provider_cfg$base_url %||% "https://api.openai.com/v1"
)

configured_model <- safe_chr(provider_cfg$model)

advisory_model <- safe_chr(
  Sys.getenv("SRMA_ADVISORY_MODEL", unset = configured_model)
)

if (!nzchar(advisory_model)) {
  stop("No advisory model configured.", call. = FALSE)
}

reasoning_effort <- tolower(
  safe_chr(
    Sys.getenv(
      "SRMA_ADVISORY_REASONING_EFFORT",
      unset = "high"
    )
  )
)

if (!reasoning_effort %in% c("none", "low", "medium", "high", "xhigh", "max")) {
  reasoning_effort <- "high"
}

timeout_seconds <- max(
  300,
  as.numeric(agent_cfg$processing$timeout_seconds %||% 300)
)

max_retries <- max(
  1L,
  as.integer(agent_cfg$processing$max_retries %||% 3L)
)

# ------------------------------------------------------------
# Strict Structured Output schema
# ------------------------------------------------------------

S <- function() list(type = "string")
I <- function() list(type = "integer")
SA <- function() list(type = "array", items = S())

candidate_topic_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  required = c(
    "rank", "topic", "clinical_question", "primary_endpoint",
    "rationale", "unique_studies_reporting_endpoint",
    "estimated_poolable_studies", "clinical_coherence",
    "methodological_coherence", "publication_value",
    "major_limitations"
  ),
  properties = list(
    rank = I(),
    topic = S(),
    clinical_question = S(),
    primary_endpoint = S(),
    rationale = S(),
    unique_studies_reporting_endpoint = I(),
    estimated_poolable_studies = I(),
    clinical_coherence = S(),
    methodological_coherence = S(),
    publication_value = S(),
    major_limitations = SA()
  )
)

harmonization_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  required = c(
    "canonical_outcome", "recommended_harmonized_endpoint",
    "recommended_time_window", "compatible_definitions",
    "definitions_or_timepoints_to_separate",
    "preferred_effect_measure", "unique_studies_reporting",
    "estimated_poolable_studies", "recommendation"
  ),
  properties = list(
    canonical_outcome = S(),
    recommended_harmonized_endpoint = S(),
    recommended_time_window = S(),
    compatible_definitions = SA(),
    definitions_or_timepoints_to_separate = SA(),
    preferred_effect_measure = S(),
    unique_studies_reporting = I(),
    estimated_poolable_studies = I(),
    recommendation = S()
  )
)

meta_set_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  required = c(
    "analysis_name", "role", "study_design_scope",
    "endpoint", "time_window", "effect_measure",
    "eligible_study_ids", "study_count",
    "why_poolable", "important_caveats"
  ),
  properties = list(
    analysis_name = S(),
    role = list(
      type = "string",
      enum = c("Primary", "Secondary", "Sensitivity", "Exploratory", "Narrative only")
    ),
    study_design_scope = S(),
    endpoint = S(),
    time_window = S(),
    effect_measure = S(),
    eligible_study_ids = SA(),
    study_count = I(),
    why_poolable = S(),
    important_caveats = SA()
  )
)

study_role_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  required = c(
    "screening_id", "systematic_review_status",
    "recommended_meta_analysis_role", "reason"
  ),
  properties = list(
    screening_id = S(),
    systematic_review_status = list(
      type = "string",
      # IMPORTANT:
      # req_body_json(..., auto_unbox = TRUE) converts a length-1
      # atomic vector into a scalar JSON string. JSON Schema requires
      # "enum" to remain an array, even when it contains one value.
      # A one-element R list preserves the JSON array:
      #   "enum": ["Remain included"]
      enum = list("Remain included")
    ),
    recommended_meta_analysis_role = S(),
    reason = S()
  )
)

json_schema <- list(
  type = "object",
  additionalProperties = FALSE,
  required = c(
    "data_status_warning",
    "executive_recommendation",
    "recommended_topic",
    "recommended_title",
    "recommended_primary_question",
    "recommended_pico",
    "recommended_primary_endpoint",
    "recommended_secondary_endpoints",
    "candidate_topics",
    "outcome_harmonization",
    "candidate_meta_analysis_sets",
    "study_roles",
    "heterogeneity_drivers",
    "outcomes_better_for_narrative_synthesis",
    "protocol_and_reporting_cautions",
    "next_human_actions",
    "overall_confidence"
  ),
  properties = list(
    data_status_warning = S(),
    executive_recommendation = S(),
    recommended_topic = S(),
    recommended_title = S(),
    recommended_primary_question = S(),
    recommended_pico = list(
      type = "object",
      additionalProperties = FALSE,
      required = c(
        "population", "intervention_or_exposure", "comparator",
        "outcome", "time_window", "study_design"
      ),
      properties = list(
        population = S(),
        intervention_or_exposure = S(),
        comparator = S(),
        outcome = S(),
        time_window = S(),
        study_design = S()
      )
    ),
    recommended_primary_endpoint = S(),
    recommended_secondary_endpoints = SA(),
    candidate_topics = list(type = "array", items = candidate_topic_schema),
    outcome_harmonization = list(type = "array", items = harmonization_schema),
    candidate_meta_analysis_sets = list(type = "array", items = meta_set_schema),
    study_roles = list(type = "array", items = study_role_schema),
    heterogeneity_drivers = SA(),
    outcomes_better_for_narrative_synthesis = SA(),
    protocol_and_reporting_cautions = SA(),
    next_human_actions = SA(),
    overall_confidence = list(
      type = "string",
      enum = c("High", "Moderate", "Low")
    )
  )
)

# ------------------------------------------------------------
# Production advisory prompt
# ------------------------------------------------------------

project_context <- paste0(
  "Project ID: ", project_id, "\n",
  "Project title: ", project_title, "\n",
  "Population: ", population, "\n",
  "Intervention/exposure: ", intervention, "\n",
  "Comparator: ", comparator, "\n",
  "Current primary outcomes: ", paste(primary_outcomes, collapse = " | "), "\n",
  "Current secondary outcomes: ", paste(secondary_outcomes, collapse = " | ")
)

deterministic_summary <- list(
  total_included_studies = nrow(included),
  total_study_rows = nrow(study),
  total_group_rows = nrow(groups),
  total_outcome_rows = nrow(outcomes),
  outcome_summary = outcome_summary,
  study_design_summary = study_design_summary
)

prompt <- paste0(
"ROLE
You are a senior clinical investigator, systematic-review methodologist, and
meta-analysis expert. Your task is to identify the most clinically coherent,
methodologically defensible, and publication-worthy meta-analysis question
supported by the extracted dataset.

DATA STATUS
The data were produced by an AI multimodal extraction agent and have NOT yet
undergone TASK-014 Human Verification. Treat every value as provisional.
Do not silently correct questionable values.

OBJECTIVE
Recommend whether the original scope should remain broad or be narrowed.
If narrowing is useful, recommend the best topic, primary endpoint,
endpoint-harmonization rules, and exact study subsets for forest plots.

NON-NEGOTIABLE RULES
1. Explicitly distinguish:
   a) studies included in the systematic review,
   b) studies reporting an outcome,
   c) studies compatible for pooling,
   d) studies contributing to a specific forest plot.
2. NEVER remove a study from the systematic review merely because it does not
   contribute to the preferred meta-analysis.
3. Every study in study_roles must have systematic_review_status='Remain included'.
4. Prioritize clinical coherence over maximizing study count.
5. A smaller coherent meta-analysis is preferable to a larger heterogeneous one.
6. Do not pool different clinical constructs merely because they share a broad label.
7. Evaluate outcome definition, time window, intervention/exposure timing,
   comparator, surgery/procedure, study design, effect-data availability,
   and adjusted versus unadjusted estimates.
8. Separate randomized and observational evidence when appropriate.
9. For every candidate forest plot, list exact screening_ids.
10. study_count must equal the number of eligible_study_ids.
11. A study should contribute only once to a single pooled analysis unless
    independent cohorts are clearly supported.
12. Recommend an operational endpoint definition and time window.
13. Identify definitions/timepoints that should remain separate.
14. Consider zero-event patterns, absent comparators, incompatible effect measures,
    and missing analysis-ready data.
15. Do not perform a statistical meta-analysis here.
16. Do not use external literature or facts; this advisory is dataset-driven.
17. State what TASK-014 must verify before investigators lock the narrowed scope.

ANALYTIC CRITERIA
For every important candidate outcome assess:
- unique studies reporting it,
- likely poolable studies,
- definition consistency,
- timepoint consistency,
- intervention/exposure timing consistency,
- design compatibility,
- effect-data availability,
- clinical importance,
- interpretability,
- suitability for a clear manuscript and forest plot.

PROJECT CONTEXT
", project_context,
"\n\nDETERMINISTIC SUMMARY COMPUTED BY R\n",
json_chr(deterministic_summary, pretty = TRUE),
"\n\nAGENT-EXTRACTED STUDY DATA\n",
json_chr(study_compact),
"\n\nAGENT-EXTRACTED GROUP DATA\n",
json_chr(group_compact),
"\n\nAGENT-EXTRACTED OUTCOME DATA\n",
json_chr(outcome_compact),
"\n\nReturn only JSON conforming to the required schema."
)

# ------------------------------------------------------------
# Responses API
# ------------------------------------------------------------

extract_output_text <- function(body) {
  if (!is.list(body$output)) {
    stop("Responses API returned no output array.", call. = FALSE)
  }

  for (item in body$output) {
    if (!is.list(item$content)) next

    for (content_item in item$content) {
      if (
        identical(content_item$type, "output_text") &&
        is.character(content_item$text)
      ) {
        return(content_item$text)
      }

      if (identical(content_item$type, "refusal")) {
        stop(
          "Model refusal: ",
          safe_chr(content_item$refusal),
          call. = FALSE
        )
      }
    }
  }

  stop("No output_text found in Responses API output.", call. = FALSE)
}

perform_request <- function(include_reasoning = TRUE) {
  body <- list(
    model = advisory_model,
    input = list(
      list(
        role = "developer",
        content = list(
          list(
            type = "input_text",
            text = paste(
              "Follow the systematic-review governance rules exactly.",
              "Do not reveal chain-of-thought.",
              "Return only the required structured result."
            )
          )
        )
      ),
      list(
        role = "user",
        content = list(
          list(type = "input_text", text = prompt)
        )
      )
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "srma_research_scope_advisory",
        strict = TRUE,
        schema = json_schema
      )
    )
  )

  if (isTRUE(include_reasoning)) {
    body$reasoning <- list(effort = reasoning_effort)
  }

  response <- httr2::request(paste0(base_url, "/responses")) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(timeout_seconds) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_perform()

  list(
    status = httr2::resp_status(response),
    text = httr2::resp_body_string(response)
  )
}

call_model <- function() {
  last_error <- ""

  for (attempt in seq_len(max_retries)) {
    result <- tryCatch(
      {
        r <- perform_request(TRUE)

        if (
          r$status >= 400L &&
          r$status < 500L &&
          grepl("reasoning", r$text, ignore.case = TRUE)
        ) {
          r <- perform_request(FALSE)
        }

        if (r$status < 200L || r$status >= 300L) {
          stop(
            "Responses API HTTP ",
            r$status,
            ": ",
            substr(r$text, 1L, 4000L),
            call. = FALSE
          )
        }

        body <- jsonlite::fromJSON(r$text, simplifyVector = FALSE)
        out_text <- extract_output_text(body)
        parsed <- jsonlite::fromJSON(out_text, simplifyVector = FALSE)

        list(
          parsed = parsed,
          response_id = safe_chr(body$id),
          response_model = safe_chr(body$model)
        )
      },
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      }
    )

    if (!is.null(result)) return(result)

    if (attempt < max_retries) {
      Sys.sleep(min(2^(attempt - 1L), 10))
    }
  }

  stop(
    "Research Scope Advisory failed after ",
    max_retries,
    " attempt(s).\nLast error: ",
    last_error,
    call. = FALSE
  )
}

cat("\nGenerating Optional Research Scope Advisory...\n")
cat("Model: ", advisory_model, "\n", sep = "")
cat("Reasoning effort: ", reasoning_effort, "\n", sep = "")

response <- call_model()
advisory <- response$parsed

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

included_ids <- sort(unique(clean_text(included$screening_id)))
included_ids <- included_ids[!is.na(included_ids)]

role_ids <- sort(
  unique(
    vapply(
      advisory$study_roles,
      function(x) safe_chr(x$screening_id),
      character(1)
    )
  )
)

if (!identical(role_ids, included_ids)) {
  stop(
    "Advisory study_roles does not match Included Studies exactly.",
    call. = FALSE
  )
}

invalid_status <- vapply(
  advisory$study_roles,
  function(x) {
    !identical(
      safe_chr(x$systematic_review_status),
      "Remain included"
    )
  },
  logical(1)
)

if (any(invalid_status)) {
  stop(
    "Advisory attempted to alter systematic-review inclusion.",
    call. = FALSE
  )
}

for (x in advisory$candidate_meta_analysis_sets) {
  ids <- as_chr_vec(x$eligible_study_ids)

  if (as.integer(x$study_count) != length(ids)) {
    stop(
      "study_count mismatch in meta-analysis set: ",
      safe_chr(x$analysis_name),
      call. = FALSE
    )
  }

  unknown <- setdiff(ids, included_ids)

  if (length(unknown) > 0L) {
    stop(
      "Unknown study ID(s) in advisory: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
}

# ------------------------------------------------------------
# Flatten tables
# ------------------------------------------------------------

candidate_topics <- dplyr::bind_rows(
  lapply(
    advisory$candidate_topics,
    function(x) {
      tibble::tibble(
        rank = as.integer(x$rank),
        topic = safe_chr(x$topic),
        clinical_question = safe_chr(x$clinical_question),
        primary_endpoint = safe_chr(x$primary_endpoint),
        rationale = safe_chr(x$rationale),
        unique_studies_reporting_endpoint = as.integer(
          x$unique_studies_reporting_endpoint
        ),
        estimated_poolable_studies = as.integer(
          x$estimated_poolable_studies
        ),
        clinical_coherence = safe_chr(x$clinical_coherence),
        methodological_coherence = safe_chr(x$methodological_coherence),
        publication_value = safe_chr(x$publication_value),
        major_limitations = paste(
          as_chr_vec(x$major_limitations),
          collapse = " | "
        )
      )
    }
  )
)

harmonization <- dplyr::bind_rows(
  lapply(
    advisory$outcome_harmonization,
    function(x) {
      tibble::tibble(
        canonical_outcome = safe_chr(x$canonical_outcome),
        recommended_harmonized_endpoint = safe_chr(
          x$recommended_harmonized_endpoint
        ),
        recommended_time_window = safe_chr(x$recommended_time_window),
        compatible_definitions = paste(
          as_chr_vec(x$compatible_definitions),
          collapse = " | "
        ),
        definitions_or_timepoints_to_separate = paste(
          as_chr_vec(x$definitions_or_timepoints_to_separate),
          collapse = " | "
        ),
        preferred_effect_measure = safe_chr(x$preferred_effect_measure),
        unique_studies_reporting = as.integer(x$unique_studies_reporting),
        estimated_poolable_studies = as.integer(x$estimated_poolable_studies),
        recommendation = safe_chr(x$recommendation)
      )
    }
  )
)

meta_sets <- dplyr::bind_rows(
  lapply(
    advisory$candidate_meta_analysis_sets,
    function(x) {
      tibble::tibble(
        analysis_name = safe_chr(x$analysis_name),
        role = safe_chr(x$role),
        study_design_scope = safe_chr(x$study_design_scope),
        endpoint = safe_chr(x$endpoint),
        time_window = safe_chr(x$time_window),
        effect_measure = safe_chr(x$effect_measure),
        eligible_study_ids = paste(
          as_chr_vec(x$eligible_study_ids),
          collapse = " | "
        ),
        study_count = as.integer(x$study_count),
        why_poolable = safe_chr(x$why_poolable),
        important_caveats = paste(
          as_chr_vec(x$important_caveats),
          collapse = " | "
        )
      )
    }
  )
)

study_roles <- dplyr::bind_rows(
  lapply(
    advisory$study_roles,
    function(x) {
      tibble::tibble(
        screening_id = safe_chr(x$screening_id),
        systematic_review_status = safe_chr(x$systematic_review_status),
        recommended_meta_analysis_role = safe_chr(
          x$recommended_meta_analysis_role
        ),
        reason = safe_chr(x$reason)
      )
    }
  )
)

# ------------------------------------------------------------
# Save machine-readable outputs
# ------------------------------------------------------------

writeLines(json_chr(advisory, pretty = TRUE), json_file)

readr::write_csv(candidate_topics, candidate_file, na = "")
readr::write_csv(harmonization, harmonization_file, na = "")
readr::write_csv(meta_sets, sets_file, na = "")
readr::write_csv(study_roles, roles_file, na = "")

# ------------------------------------------------------------
# Human-readable Markdown
# ------------------------------------------------------------

bullet_lines <- function(x) {
  y <- as_chr_vec(x)
  if (length(y) == 0L) "- None" else paste0("- ", y)
}

md <- c(
  paste0("# Research Scope Advisory — ", project_title),
  "",
  paste0("**Generated:** ", now_text()),
  paste0("**Model:** ", advisory_model),
  paste0("**Response ID:** ", response$response_id),
  "",
  "> **Important:** This advisory is based on unverified Agent-extracted data.",
  "> It does not alter systematic-review inclusion or extracted values.",
  "",
  "## Executive Recommendation",
  "",
  safe_chr(advisory$executive_recommendation),
  "",
  "## Recommended Research Scope",
  "",
  paste0("**Topic:** ", safe_chr(advisory$recommended_topic)),
  "",
  paste0("**Suggested title:** ", safe_chr(advisory$recommended_title)),
  "",
  paste0(
    "**Primary question:** ",
    safe_chr(advisory$recommended_primary_question)
  ),
  "",
  paste0(
    "**Primary endpoint:** ",
    safe_chr(advisory$recommended_primary_endpoint)
  ),
  "",
  "## Recommended PICO",
  "",
  paste0("- Population: ", safe_chr(advisory$recommended_pico$population)),
  paste0(
    "- Intervention/exposure: ",
    safe_chr(advisory$recommended_pico$intervention_or_exposure)
  ),
  paste0("- Comparator: ", safe_chr(advisory$recommended_pico$comparator)),
  paste0("- Outcome: ", safe_chr(advisory$recommended_pico$outcome)),
  paste0("- Time window: ", safe_chr(advisory$recommended_pico$time_window)),
  paste0("- Study design: ", safe_chr(advisory$recommended_pico$study_design)),
  "",
  "## Secondary Endpoints",
  "",
  bullet_lines(advisory$recommended_secondary_endpoints),
  "",
  "## Candidate Meta-analysis Sets",
  ""
)

if (nrow(meta_sets) > 0L) {
  for (i in seq_len(nrow(meta_sets))) {
    md <- c(
      md,
      paste0(
        "### ",
        meta_sets$role[[i]],
        " — ",
        meta_sets$analysis_name[[i]]
      ),
      "",
      paste0("- Endpoint: ", meta_sets$endpoint[[i]]),
      paste0("- Design: ", meta_sets$study_design_scope[[i]]),
      paste0("- Time window: ", meta_sets$time_window[[i]]),
      paste0("- Effect measure: ", meta_sets$effect_measure[[i]]),
      paste0("- Study count: ", meta_sets$study_count[[i]]),
      paste0("- Study IDs: ", meta_sets$eligible_study_ids[[i]]),
      paste0("- Why poolable: ", meta_sets$why_poolable[[i]]),
      paste0("- Caveats: ", meta_sets$important_caveats[[i]]),
      ""
    )
  }
}

md <- c(
  md,
  "## Major Heterogeneity Drivers",
  "",
  bullet_lines(advisory$heterogeneity_drivers),
  "",
  "## Better Suited to Narrative Synthesis",
  "",
  bullet_lines(advisory$outcomes_better_for_narrative_synthesis),
  "",
  "## Protocol / Reporting Cautions",
  "",
  bullet_lines(advisory$protocol_and_reporting_cautions),
  "",
  "## TASK-014 Verification Priorities",
  "",
  bullet_lines(advisory$next_human_actions),
  "",
  paste0(
    "**Overall confidence:** ",
    safe_chr(advisory$overall_confidence)
  )
)

writeLines(md, md_file)

# ------------------------------------------------------------
# Excel advisory workbook
# ------------------------------------------------------------

wb <- openxlsx::createWorkbook(
  creator = "Universal SR-MA Agent Toolkit"
)

write_sheet <- function(name, data) {
  openxlsx::addWorksheet(wb, name)
  openxlsx::writeData(wb, name, data, withFilter = TRUE)
  if (ncol(data) > 0L) {
    openxlsx::setColWidths(
      wb,
      name,
      cols = seq_len(ncol(data)),
      widths = "auto"
    )
  }
  openxlsx::freezePane(wb, name, firstActiveRow = 2)
}

summary_sheet <- tibble::tibble(
  field = c(
    "data_status_warning",
    "executive_recommendation",
    "recommended_topic",
    "recommended_title",
    "recommended_primary_question",
    "recommended_primary_endpoint",
    "overall_confidence",
    "model",
    "response_id"
  ),
  value = c(
    safe_chr(advisory$data_status_warning),
    safe_chr(advisory$executive_recommendation),
    safe_chr(advisory$recommended_topic),
    safe_chr(advisory$recommended_title),
    safe_chr(advisory$recommended_primary_question),
    safe_chr(advisory$recommended_primary_endpoint),
    safe_chr(advisory$overall_confidence),
    advisory_model,
    response$response_id
  )
)

write_sheet("Advisory_Summary", summary_sheet)
write_sheet("Outcome_Frequency", outcome_summary)
write_sheet("Candidate_Topics", candidate_topics)
write_sheet("Outcome_Harmonization", harmonization)
write_sheet("Meta_Analysis_Sets", meta_sets)
write_sheet("Study_Roles", study_roles)

openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)

# ------------------------------------------------------------
# Final report
# ------------------------------------------------------------

required_outputs <- c(
  json_file, md_file, xlsx_file,
  candidate_file, harmonization_file,
  sets_file, roles_file
)

bad <- required_outputs[
  !file.exists(required_outputs) |
    is.na(file.info(required_outputs)$size) |
    file.info(required_outputs)$size <= 0
]

if (length(bad) > 0L) {
  stop(
    "Advisory output validation failed:\n- ",
    paste(bad, collapse = "\n- "),
    call. = FALSE
  )
}

writeLines(
  c(
    "Universal SR-MA Agent Toolkit",
    "Optional Research Scope Advisory",
    "",
    paste0("Generated at: ", now_text()),
    paste0("Project ID: ", project_id),
    paste0("Choice source: ", choice$source),
    "",
    "ARCHITECTURE",
    "Position: after TASK-013, before TASK-014",
    "New TASK number created: NO",
    "Changes Included Studies: NO",
    "Changes extracted values: NO",
    "Performs Human Verification: NO",
    "Runner integration performed here: NO",
    "Runner-ready Yes/No hook prepared: YES",
    "",
    "INPUT",
    paste0("Included Studies: ", nrow(included)),
    paste0("Outcome rows: ", nrow(outcomes)),
    "",
    "MODEL",
    paste0("Configured model: ", configured_model),
    paste0("Advisory model used: ", advisory_model),
    paste0("Reasoning effort requested: ", reasoning_effort),
    paste0("Response ID: ", response$response_id),
    "",
    "STATUS: PASS",
    "Proceed to TASK-014 — Human Verification.",
    "Recommendations remain provisional until TASK-014."
  ),
  report_file
)

# ------------------------------------------------------------
# Action-oriented Terminal summary
# ------------------------------------------------------------

get_ids_by_role <- function(role_name) {
  matched <- advisory$candidate_meta_analysis_sets[
    vapply(
      advisory$candidate_meta_analysis_sets,
      function(x) identical(safe_chr(x$role), role_name),
      logical(1)
    )
  ]

  if (length(matched) == 0L) {
    return(character(0))
  }

  unique(
    unlist(
      lapply(
        matched,
        function(x) as_chr_vec(x$eligible_study_ids)
      ),
      use.names = FALSE
    )
  )
}

primary_ids <- get_ids_by_role("Primary")
sensitivity_ids <- get_ids_by_role("Sensitivity")
secondary_ids <- get_ids_by_role("Secondary")

# Avoid repeating primary studies when the sensitivity set contains
# the primary set plus one or more additional studies.
sensitivity_only_ids <- setdiff(
  sensitivity_ids,
  primary_ids
)

# Identify explicit overlap / duplicate-cohort audit instructions
# from next_human_actions.
audit_actions <- as_chr_vec(
  advisory$next_human_actions
)

overlap_actions <- audit_actions[
  grepl(
    "overlap|double count|duplicate",
    audit_actions,
    ignore.case = TRUE
  )
]

extract_screening_ids <- function(x) {
  if (length(x) == 0L) {
    return(character(0))
  }

  hits <- unlist(
    regmatches(
      x,
      gregexpr(
        "SCR_[0-9]+",
        x,
        perl = TRUE
      )
    ),
    use.names = FALSE
  )

  unique(hits[nzchar(hits)])
}

audit_ids <- extract_screening_ids(
  overlap_actions
)

cat("\n")
cat("============================================================\n")
cat("Optional Research Scope Advisory\n")
cat("============================================================\n\n")

cat("Recommended topic:\n")
cat(safe_chr(advisory$recommended_topic), "\n\n")

cat("Recommended primary endpoint:\n")
cat(safe_chr(advisory$recommended_primary_endpoint), "\n\n")

cat("Recommended primary meta-analysis:\n")
if (length(primary_ids) > 0L) {
  cat("RCT-centered primary synthesis\n\n")
} else {
  cat("See advisory report for the recommended primary synthesis.\n\n")
}

cat("Primary studies for Human Verification:\n")
if (length(primary_ids) > 0L) {
  for (id in primary_ids) {
    cat("- ", id, "\n", sep = "")
  }
} else {
  cat("- None identified\n")
}
cat("\n")

cat("Sensitivity studies for Human Verification:\n")
if (length(sensitivity_only_ids) > 0L) {
  for (id in sensitivity_only_ids) {
    cat("- ", id, "\n", sep = "")
  }
} else if (length(sensitivity_ids) > 0L) {
  for (id in sensitivity_ids) {
    cat("- ", id, "\n", sep = "")
  }
} else {
  cat("- None identified\n")
}
cat("\n")

cat("Observational secondary-analysis studies:\n")
if (length(secondary_ids) > 0L) {
  for (id in secondary_ids) {
    cat("- ", id, "\n", sep = "")
  }
} else {
  cat("- None identified\n")
}
cat("\n")

cat("Additional audit check:\n")
if (length(audit_ids) > 0L) {
  for (id in audit_ids) {
    cat("- ", id, "\n", sep = "")
  }

  if (length(overlap_actions) > 0L) {
    for (action in overlap_actions) {
      cat("  ", action, "\n", sep = "")
    }
  }
} else {
  cat("- No specific overlap/duplicate-cohort audit identified\n")
}
cat("\n")

cat("Human Verification priority:\n")
human_actions <- as_chr_vec(
  advisory$next_human_actions
)

if (length(human_actions) > 0L) {
  for (i in seq_along(human_actions)) {
    cat(i, ". ", human_actions[[i]], "\n", sep = "")
  }
} else {
  cat("1. Verify analysis-critical study and outcome data against source PDFs.\n")
}
cat("\n")

cat("Systematic-review Included Studies remain unchanged:\n")
cat(nrow(included), " studies\n\n", sep = "")

cat(
  "Quantitative synthesis will use only the endpoint-specific ",
  "clinically compatible subset.\n\n",
  sep = ""
)

cat("PASS: Optional Research Scope Advisory generated successfully.\n")
cat("Proceed to targeted Human Verification before TASK-014 finalization.\n")
cat("Markdown: ", md_file, "\n", sep = "")
cat("Workbook: ", xlsx_file, "\n", sep = "")

} # end YES branch
