# ============================================================
# Universal SR-MA Agent Toolkit
# R/01a_migrate_existing_initialization_draft.R
# One-time migration for pre-schema-v2 project_config_draft.yml
# ============================================================

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop("Missing package: yaml")
}

agent_document <- yaml::read_yaml(
  "config/initialization_agent.yml"
)

agent_config <- agent_document$initialization_agent
output_dir <- agent_config$output$generated_directory

project_file <- file.path(
  output_dir,
  agent_config$output$project_config_draft
)

if (!file.exists(project_file)) {
  stop("Missing project draft: ", project_file)
}

project_config <- yaml::read_yaml(project_file)

old_decisions <- unlist(
  project_config$screening$decisions,
  recursive = TRUE,
  use.names = FALSE
)

include_rules <- old_decisions[
  grepl("^include\\b", tolower(trimws(old_decisions)))
]

exclude_rules <- old_decisions[
  grepl("^exclude\\b", tolower(trimws(old_decisions)))
]

unclear_rules <- old_decisions[
  grepl(
    "^(unclear|unsure|uncertain)\\b",
    tolower(trimws(old_decisions))
  )
]

clean_rule <- function(x, prefixes) {
  result <- x

  for (prefix in prefixes) {
    result <- sub(
      paste0("^", prefix, "\\s*[-/:]?\\s*"),
      "",
      result,
      ignore.case = TRUE
    )
  }

  trimws(result)
}

include_rules <- clean_rule(
  include_rules,
  c("include")
)

exclude_rules <- clean_rule(
  exclude_rules,
  c("exclude")
)

unclear_rules <- clean_rule(
  unclear_rules,
  c("unclear", "unsure", "uncertain")
)

project_config$screening$decisions <- c(
  "Include",
  "Exclude",
  "Unclear"
)

project_config$screening$decision_rules <- list(
  include = unique(include_rules[nzchar(include_rules)]),
  exclude = unique(exclude_rules[nzchar(exclude_rules)]),
  unclear = unique(unclear_rules[nzchar(unclear_rules)])
)

archive_dir <- file.path(
  output_dir,
  "migration_archive"
)

dir.create(
  archive_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

archive_file <- file.path(
  archive_dir,
  paste0(
    "project_config_draft_before_schema_v2_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".yml"
  )
)

if (!file.copy(
  project_file,
  archive_file,
  overwrite = FALSE
)) {
  stop("Could not archive the existing project draft.")
}

yaml::write_yaml(
  project_config,
  project_file
)

cat("\nExisting project draft migrated to schema v2.\n")
cat("Updated file:", project_file, "\n")
cat("Archive:", archive_file, "\n")
