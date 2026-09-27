# Cardiac Surgery SR-MA Toolkit v2.0
# Verify systematic-review search-strategy templates

required_files <- c(
  "templates/protocol/search_strategy_template.md",
  "templates/protocol/search_log_template.csv"
)

file_report <- data.frame(
  file = required_files,
  exists = file.exists(required_files),
  stringsAsFactors = FALSE
)

cat("\n")
cat("========================================\n")
cat("Search-strategy template verification\n")
cat("========================================\n\n")

print(file_report, row.names = FALSE)

if (!all(file_report$exists)) {
  missing_files <- file_report$file[!file_report$exists]

  stop(
    "Missing search-strategy files: ",
    paste(missing_files, collapse = ", ")
  )
}

search_log <- readr::read_csv(
  "templates/protocol/search_log_template.csv",
  show_col_types = FALSE
)

required_search_log_columns <- c(
  "search_id",
  "database",
  "platform",
  "search_date",
  "searcher",
  "search_version",
  "full_search_string",
  "records_retrieved",
  "export_format",
  "export_filename",
  "export_location",
  "notes"
)

missing_columns <- setdiff(
  required_search_log_columns,
  names(search_log)
)

strategy_text <- readLines(
  "templates/protocol/search_strategy_template.md",
  warn = FALSE,
  encoding = "UTF-8"
)

required_sections <- c(
  "PubMed/MEDLINE",
  "Embase",
  "Cochrane CENTRAL",
  "ClinicalTrials.gov",
  "Search validation",
  "Search-update policy"
)

missing_sections <- required_sections[
  !vapply(
    required_sections,
    function(section) {
      any(grepl(section, strategy_text, fixed = TRUE))
    },
    logical(1)
  )
]

cat("\nNumber of search-log columns:\n")
cat(ncol(search_log), "\n")

cat("\n")

if (length(missing_columns) == 0) {
  cat("PASS: All required search-log columns are present.\n")
} else {
  cat("CHECK: Missing search-log columns:\n")
  cat(paste(missing_columns, collapse = ", "), "\n")
}

if (length(missing_sections) == 0) {
  cat("PASS: All required search-strategy sections are present.\n")
} else {
  cat("CHECK: Missing search-strategy sections:\n")
  cat(paste(missing_sections, collapse = ", "), "\n")
}

if (
  length(missing_columns) == 0 &&
  length(missing_sections) == 0
) {
  cat("\nPASS: Search-strategy templates are ready.\n")
} else {
  cat("\nCHECK: Search-strategy templates require revision.\n")
}