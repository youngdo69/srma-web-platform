# ============================================================
# Universal SR-MA Agent Toolkit
# R/bootstrap/00_setup_project.R
# ============================================================

options(
  repos = c(CRAN = "https://cloud.r-project.org")
)

required_directories <- c(
  "R",
  "R/bootstrap",
  "R/utils",
  "R/manual",
  "config",
  "config/generated",
  "config/archive",
  "prompts",
  "templates/extraction",
  "templates/protocol",
  "templates/risk-of-bias",
  "templates/manuscript",
  "data/raw",
  "data/processed",
  "data/screening",
  "data/full_text",
  "data/extraction",
  "results/figures",
  "results/tables",
  "results/logs",
  "results/screening",
  "results/full_text",
  "search_strategies",
  "examples"
)

for (directory in required_directories) {
  dir.create(
    directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

cat("\n")
cat("========================================\n")
cat("Universal SR-MA Agent Toolkit\n")
cat("Project folders are ready\n")
cat("========================================\n")

cat("\nWorking directory:\n")
cat(getwd(), "\n")

cat("\nDirectories checked or created:\n")
cat(paste0("- ", required_directories), sep = "\n")
cat("\n")
