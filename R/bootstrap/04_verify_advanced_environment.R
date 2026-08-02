# ============================================================
# Universal SR-MA Agent Toolkit
# R/bootstrap/04_verify_advanced_environment.R
# ============================================================

advanced_packages <- c(
  "mada",
  "diagmeta",
  "netmeta",
  "gemtc",
  "robvis",
  "PRISMA2020",
  "weightr",
  "clubSandwich",
  "rpact"
)

advanced_package_available <- vapply(
  advanced_packages,
  requireNamespace,
  logical(1),
  quietly = TRUE
)

advanced_package_versions <- vapply(
  advanced_packages,
  function(package_name) {
    if (requireNamespace(package_name, quietly = TRUE)) {
      as.character(utils::packageVersion(package_name))
    } else {
      NA_character_
    }
  },
  character(1)
)

advanced_package_report <- data.frame(
  package = advanced_packages,
  available = unname(advanced_package_available),
  version = unname(advanced_package_versions),
  stringsAsFactors = FALSE
)

dir.create(
  "results/logs",
  recursive = TRUE,
  showWarnings = FALSE
)

utils::write.csv(
  advanced_package_report,
  file = "results/logs/advanced_environment_report.csv",
  row.names = FALSE
)

cat("\n")
cat("========================================\n")
cat("Universal SR-MA Agent Toolkit\n")
cat("Advanced environment verification\n")
cat("========================================\n\n")

print(
  advanced_package_report,
  row.names = FALSE
)

cat("\n")

if (all(advanced_package_available)) {
  cat("PASS: The advanced SR-MA environment is ready.\n")
} else {
  missing_packages <- advanced_packages[
    !advanced_package_available
  ]

  cat("CHECK: Some optional advanced packages are missing.\n")
  cat(
    "Missing packages:",
    paste(missing_packages, collapse = ", "),
    "\n"
  )
  cat(
    "These packages are not required for project initialization ",
    "or title/abstract screening.\n",
    sep = ""
  )
}
