# ============================================================
# Universal SR-MA Agent Toolkit
# R/00_run_project_initialization_agent.R
# ============================================================

required_packages <- c("yaml", "jsonlite", "httr2", "dotenv")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]
if (length(missing_packages) > 0L) {
  stop(
    "Missing packages: ", paste(missing_packages, collapse = ", "),
    "\nInstall them before running project initialization."
  )
}

source("R/utils/llm_provider_utils.R")
source("R/utils/initialization_prompt_utils.R")

seed_file <- "config/project_seed.yml"
agent_file <- "config/initialization_agent.yml"

if (!file.exists(seed_file)) stop("Missing project seed: ", seed_file)
if (!file.exists(agent_file)) stop("Missing initialization config: ", agent_file)

seed_document <- yaml::read_yaml(seed_file)
agent_document <- yaml::read_yaml(agent_file)

project_seed <- seed_document$project_seed
agent_config <- agent_document$initialization_agent

if (!isTRUE(agent_config$enabled)) {
  stop("Initialization agent is disabled.")
}

if (is.null(project_seed$topic) ||
    !nzchar(trimws(project_seed$topic)) ||
    identical(project_seed$topic, "REPLACE_WITH_RESEARCH_TOPIC")) {
  stop("Enter a research topic in config/project_seed.yml.")
}

load_project_env(".env")

template_file <- agent_config$prompt$template_file
if (!file.exists(template_file)) stop("Missing prompt template: ", template_file)

template_text <- paste(
  readLines(template_file, warn = FALSE, encoding = "UTF-8"),
  collapse = "\n"
)

prompt <- render_initialization_prompt(template_text, project_seed)
schema <- build_initialization_json_schema()

required_hd_fields_v3 <- c(
  "decision_id",
  "question",
  "why_it_matters",
  "suggested_options",
  "recommended_option",
  "recommendation_rationale",
  "affected_components",
  "option_patches"
)

schema_hd_fields <- schema$
  properties$
  human_decisions_required$
  items$
  required

missing_schema_fields <- setdiff(
  required_hd_fields_v3,
  schema_hd_fields
)

if (length(missing_schema_fields) > 0L) {
  stop(
    "Initialization schema is outdated. Missing HD fields: ",
    paste(missing_schema_fields, collapse = ", ")
  )
}

provider_config <- agent_config$provider
provider_config$timeout_seconds <- agent_config$processing$timeout_seconds %||% 180
provider_config$store <- isTRUE(provider_config$store)

response <- call_llm_screening_provider(
  provider_name = provider_config$name,
  prompt = prompt,
  provider_config = provider_config,
  json_schema = schema,
  schema_name = "project_initialization_v3"
)

result <- response$parsed

# ------------------------------------------------------------
# Preserve complete schema-validated human-decision objects
# ------------------------------------------------------------

human_decisions <- result$human_decisions_required %||% list()

required_hd_fields_v3 <- c(
  "decision_id",
  "question",
  "why_it_matters",
  "suggested_options",
  "recommended_option",
  "recommendation_rationale",
  "affected_components",
  "option_patches"
)

if (length(human_decisions) > 0L) {
  missing_hd_fields <- setdiff(
    required_hd_fields_v3,
    names(human_decisions[[1]])
  )

  if (length(missing_hd_fields) > 0L) {
    stop(
      "The LLM response is missing HD schema-v3 fields: ",
      paste(missing_hd_fields, collapse = ", ")
    )
  }
}

result$human_decisions_required <- human_decisions

output_dir <- agent_config$output$generated_directory
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

project_draft_file <- file.path(
  output_dir, agent_config$output$project_config_draft
)
search_draft_file <- file.path(
  output_dir, agent_config$output$search_terms_draft
)
decisions_file <- file.path(
  output_dir, agent_config$output$decisions_required
)
raw_file <- file.path(
  output_dir, agent_config$output$raw_response
)
schema_sent_file <- file.path(
  output_dir, "initialization_schema_actually_sent.json"
)
model_output_file <- file.path(
  output_dir, "project_initialization_model_output.json"
)
api_raw_file <- file.path(
  output_dir, "project_initialization_api_raw_response.json"
)
report_file <- file.path(
  output_dir, agent_config$output$report
)

yaml::write_yaml(result$project_config, project_draft_file)
yaml::write_yaml(result$search_terms, search_draft_file)
yaml::write_yaml(
  list(human_decisions_required = human_decisions),
  decisions_file
)

jsonlite::write_json(
  schema,
  schema_sent_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)

writeLines(
  response$raw_output_text,
  model_output_file,
  useBytes = TRUE
)

writeLines(
  response$raw_response,
  api_raw_file,
  useBytes = TRUE
)

jsonlite::write_json(
  list(
    response_id = response$response_id,
    model = response$model,
    prompt_version = agent_config$prompt$version,
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    output = result
  ),
  raw_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)


report_lines <- c(
  "# Project Initialization Report",
  "",
  paste0("- Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  paste0("- Model: ", response$model),
  paste0("- Prompt version: ", agent_config$prompt$version),
  "",
  "## Summary",
  "",
  result$report$summary,
  "",
  "## Assumptions",
  "",
  if (length(result$report$assumptions) > 0L)
    paste0("- ", result$report$assumptions)
  else "- None recorded",
  "",
  "## Warnings",
  "",
  if (length(result$report$warnings) > 0L)
    paste0("- ", result$report$warnings)
  else "- None recorded",
  "",
  "## Human decisions required",
  "",
  if (length(human_decisions) > 0L) {
    unlist(lapply(human_decisions, function(x) {
      c(
        paste0("### ", x$decision_id),
        "",
        paste0("**Question:** ", x$question),
        "",
        paste0("**Why it matters:** ", x$why_it_matters),
        "",
        paste0("**Recommended option:** ", x$recommended_option),
        "",
        paste0(
          "**Recommendation rationale:** ",
          x$recommendation_rationale
        ),
        "",
        "**Suggested options:**",
        paste0("- ", x$suggested_options),
        "",
        "**Affected components:**",
        paste0("- ", x$affected_components),
        ""
      )
    }))
  } else {
    "No material unresolved decisions were reported."
  },
  "",
  "## Draft files",
  "",
  paste0("- `", project_draft_file, "`"),
  paste0("- `", search_draft_file, "`"),
  paste0("- `", decisions_file, "`"),
  "",
  "> These are drafts. Do not activate them before human review."
)

writeLines(report_lines, report_file, useBytes = TRUE)

cat("\nProject initialization drafts generated.\n")
cat("Project config draft:", project_draft_file, "\n")
cat("Search terms draft:", search_draft_file, "\n")
cat("Human decisions:", decisions_file, "\n")
cat("Schema actually sent:", schema_sent_file, "\n")
cat("Exact model output:", model_output_file, "\n")
cat("API raw response:", api_raw_file, "\n")
cat("Report:", report_file, "\n")
