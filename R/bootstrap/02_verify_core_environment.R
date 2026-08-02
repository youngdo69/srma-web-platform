# ============================================================
# Universal SR-MA Agent Toolkit
# R/bootstrap/02_verify_core_environment.R
# ============================================================

required_packages <- c(
  "meta",
  "metafor",
  "survival",
  "survminer",
  "readxl",
  "writexl",
  "openxlsx",
  "readr",
  "dplyr",
  "tidyr",
  "tibble",
  "stringr",
  "purrr",
  "janitor",
  "ggplot2",
  "patchwork",
  "scales",
  "broom",
  "gtsummary",
  "gt",
  "flextable",
  "rmarkdown",
  "knitr",
  "here",
  "fs",
  "cli",
  "glue",
  "yaml",
  "jsonlite",
  "httr2",
  "dotenv"
)

required_directories <- c(
  "R",
  "R/bootstrap",
  "R/utils",
  "config",
  "config/generated",
  "config/archive",
  "prompts",
  "data/raw",
  "data/processed",
  "results/figures",
  "results/tables",
  "results/logs",
  "results/screening",
  "search_strategies"
)

required_files <- c(
  "config/project_seed.yml",
  "config/initialization_agent.yml",
  "prompts/project_initialization_prompt.md",
  "R/00_run_project_initialization_agent.R",
  "R/01_validate_project_initialization.R",
  "R/02_activate_project_configuration.R",
  "R/utils/initialization_prompt_utils.R",
  "R/utils/llm_provider_utils.R"
)

package_available <- vapply(
  required_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)

package_versions <- vapply(
  required_packages,
  function(package_name) {
    if (requireNamespace(package_name, quietly = TRUE)) {
      as.character(utils::packageVersion(package_name))
    } else {
      NA_character_
    }
  },
  character(1)
)

package_report <- data.frame(
  package = required_packages,
  available = unname(package_available),
  version = unname(package_versions),
  stringsAsFactors = FALSE
)

directory_report <- data.frame(
  directory = required_directories,
  exists = vapply(required_directories, dir.exists, logical(1)),
  stringsAsFactors = FALSE
)

file_report <- data.frame(
  file = required_files,
  exists = vapply(required_files, file.exists, logical(1)),
  stringsAsFactors = FALSE
)

renv_library_detected <- any(
  grepl(
    "renv/library",
    .libPaths(),
    fixed = TRUE
  )
)

environment_summary <- list(
  checked_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  R_version = R.version.string,
  working_directory = getwd(),
  library_paths = .libPaths(),
  renv_library_detected = renv_library_detected,
  package_report = package_report,
  directory_report = directory_report,
  file_report = file_report
)

dir.create(
  "results/logs",
  recursive = TRUE,
  showWarnings = FALSE
)

utils::write.csv(
  package_report,
  file = "results/logs/package_environment_report.csv",
  row.names = FALSE
)

utils::write.csv(
  directory_report,
  file = "results/logs/directory_environment_report.csv",
  row.names = FALSE
)

utils::write.csv(
  file_report,
  file = "results/logs/bootstrap_file_environment_report.csv",
  row.names = FALSE
)

jsonlite::write_json(
  environment_summary,
  path = "results/logs/environment_summary.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cat("\n")
cat("========================================\n")
cat("Universal SR-MA Agent Toolkit\n")
cat("Core environment verification\n")
cat("========================================\n\n")

cat("R version:\n")
cat(R.version.string, "\n\n")

cat("Working directory:\n")
cat(getwd(), "\n\n")

cat("renv project library detected:\n")
cat(renv_library_detected, "\n\n")

cat("Package status:\n")
print(package_report, row.names = FALSE)

cat("\nDirectory status:\n")
print(directory_report, row.names = FALSE)

cat("\nRequired initialization files:\n")
print(file_report, row.names = FALSE)

core_ready <- all(package_available)
directories_ready <- all(directory_report$exists)
files_ready <- all(file_report$exists)

cat("\n")

if (
  core_ready &&
  directories_ready &&
  files_ready &&
  renv_library_detected
) {
  cat("PASS: The core SR-MA agent environment is ready.\n")
} else {
  cat("CHECK: Some packages, directories, files, or renv settings require attention.\n")
}

if (!renv_library_detected) {
  cat(
    "\nNOTICE: renv project library was not detected.\n",
    "Run renv::init() only if this project has not yet been initialized with renv.\n",
    sep = ""
  )
}
