# Cardiac Surgery SR-MA Toolkit v2.0
# Environment verification

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
  "jsonlite"
)

required_directories <- c(
  "R",
  "data/raw",
  "data/processed",
  "results/figures",
  "results/tables",
  "results/logs"
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

directory_available <- vapply(
  required_directories,
  dir.exists,
  logical(1)
)

directory_report <- data.frame(
  directory = required_directories,
  exists = unname(directory_available),
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
  checked_at = as.character(Sys.time()),
  R_version = R.version.string,
  working_directory = getwd(),
  library_paths = .libPaths(),
  renv_library_detected = renv_library_detected,
  package_report = package_report,
  directory_report = directory_report
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

jsonlite::write_json(
  environment_summary,
  path = "results/logs/environment_summary.json",
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

cat("\n")
cat("========================================\n")
cat("Cardiac Surgery SR-MA Toolkit v2.0\n")
cat("Environment verification\n")
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

cat("\n")

if (
  all(package_available) &&
  all(directory_available) &&
  renv_library_detected
) {
  cat("PASS: The core SR-MA environment is ready.\n")
} else {
  cat("CHECK: Some packages, directories, or renv settings require attention.\n")
}