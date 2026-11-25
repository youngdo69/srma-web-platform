# Cardiac Surgery SR-MA Toolkit v2.0
# Basic project setup

options(
  repos = c(CRAN = "https://cloud.r-project.org")
)

required_directories <- c(
  "R",
  "config",
  "data/raw",
  "data/processed",
  "results/figures",
  "results/tables",
  "results/logs",
  "search_strategies"
)

for (directory in required_directories) {
  dir.create(
    directory,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

message("SR-MA project folders are ready.")