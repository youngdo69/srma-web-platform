# Platform dependencies are separate from the canonical scientific renv environment.
web_packages <- c("shiny", "bslib", "DBI", "RSQLite", "RPostgreSQL", "jsonlite", "digest", "yaml", "dplyr", "readr")
# RSQLite is the compatible local/test strategy; production target is PostgreSQL.
