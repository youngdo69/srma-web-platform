# ============================================================
# Universal SR-MA Agent Toolkit
# R/bootstrap/01_install_core_packages.R
# ============================================================

options(
  repos = c(CRAN = "https://cloud.r-project.org")
)

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

core_packages <- c(
  # Meta-analysis
  "meta",
  "metafor",

  # Survival analysis
  "survival",
  "survminer",

  # Data import and export
  "readxl",
  "writexl",
  "openxlsx",
  "readr",

  # Data management
  "dplyr",
  "tidyr",
  "tibble",
  "stringr",
  "purrr",
  "janitor",

  # Graphics
  "ggplot2",
  "patchwork",
  "scales",

  # Tables and reporting
  "broom",
  "gtsummary",
  "gt",
  "flextable",
  "rmarkdown",
  "knitr",

  # Project utilities
  "here",
  "fs",
  "cli",
  "glue",
  "yaml",
  "jsonlite",

  # API and environment
  "httr2",
  "dotenv"
)

install_one_package <- function(package_name) {

  if (requireNamespace(package_name, quietly = TRUE)) {

    package_version <- as.character(
      utils::packageVersion(package_name)
    )

    message(
      "[Already installed] ",
      package_name,
      " ",
      package_version
    )

    return(TRUE)
  }

  message("[Installing] ", package_name)

  tryCatch(
    {
      renv::install(package_name)

      if (!requireNamespace(package_name, quietly = TRUE)) {
        stop("The package could not be loaded after installation.")
      }

      message("[Installation complete] ", package_name)
      TRUE
    },
    error = function(error) {
      message(
        "[Installation failed] ",
        package_name,
        ": ",
        conditionMessage(error)
      )
      FALSE
    }
  )
}

installation_results <- vapply(
  core_packages,
  install_one_package,
  logical(1)
)

installation_report <- data.frame(
  package = core_packages,
  installed = unname(installation_results),
  stringsAsFactors = FALSE
)

dir.create(
  "results/logs",
  recursive = TRUE,
  showWarnings = FALSE
)

utils::write.csv(
  installation_report,
  file = "results/logs/package_installation_report.csv",
  row.names = FALSE
)

cat("\n")
cat("========================================\n")
cat("Core package installation completed\n")
cat("Successful:", sum(installation_results), "\n")
cat("Failed:", sum(!installation_results), "\n")
cat("========================================\n")
