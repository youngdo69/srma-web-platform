# Cardiac Surgery SR-MA Toolkit v2.0
# Advanced package installation

options(
  repos = c(CRAN = "https://cloud.r-project.org")
)

advanced_packages <- c(
  # Diagnostic accuracy meta-analysis
  "mada",
  "diagmeta",

  # Network meta-analysis
  "netmeta",
  "gemtc",

  # Risk-of-bias visualization and PRISMA
  "robvis",
  "PRISMA2020",

  # Publication-bias and robust inference
  "weightr",
  "clubSandwich",

  # Sequential and adaptive-design methods
  "rpact"
)

install_advanced_package <- function(package_name) {

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

advanced_installation_results <- vapply(
  advanced_packages,
  install_advanced_package,
  logical(1)
)

advanced_installation_report <- data.frame(
  package = advanced_packages,
  installed = unname(advanced_installation_results),
  stringsAsFactors = FALSE
)

dir.create(
  "results/logs",
  recursive = TRUE,
  showWarnings = FALSE
)

utils::write.csv(
  advanced_installation_report,
  file = "results/logs/advanced_package_installation_report.csv",
  row.names = FALSE
)

cat("\n")
cat("========================================\n")
cat("Advanced SR-MA package installation completed\n")
cat("Successful:", sum(advanced_installation_results), "\n")
cat("Failed:", sum(!advanced_installation_results), "\n")
cat("========================================\n")