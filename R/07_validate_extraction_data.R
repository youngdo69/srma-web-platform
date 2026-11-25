# Cardiac Surgery SR-MA Toolkit v2.0
# Validate the imported extraction dataset

source_file <- "data/raw/SGLT2i_data_extraction.csv"

if (!file.exists(source_file)) {
  stop(
    "The extraction file does not exist: ",
    source_file
  )
}

extraction_data <- readr::read_csv(
  source_file,
  show_col_types = FALSE,
  na = c("", "NA", "N/A", "Not reported", "NR")
)

required_columns <- c(
  "study_id",
  "first_author",
  "publication_year",
  "study_design",
  "n_intervention",
  "n_control",
  "outcome_name",
  "timepoint",
  "event_intervention",
  "total_intervention",
  "event_control",
  "total_control",
  "mean_intervention",
  "sd_intervention",
  "mean_control",
  "sd_control",
  "effect_measure",
  "effect_estimate",
  "lower_ci",
  "upper_ci",
  "risk_of_bias_judgment",
  "extraction_status"
)

missing_columns <- setdiff(
  required_columns,
  names(extraction_data)
)

duplicated_column_names <- names(extraction_data)[
  duplicated(names(extraction_data))
]

validation_summary <- data.frame(
  check = c(
    "Source file exists",
    "Required columns present",
    "No duplicated column names",
    "Dataset contains at least one row"
  ),
  passed = c(
    file.exists(source_file),
    length(missing_columns) == 0,
    length(duplicated_column_names) == 0,
    nrow(extraction_data) > 0
  ),
  stringsAsFactors = FALSE
)

dir.create(
  "results/logs",
  recursive = TRUE,
  showWarnings = FALSE
)

utils::write.csv(
  validation_summary,
  file = "results/logs/data_validation_summary.csv",
  row.names = FALSE
)

cat("\n")
cat("========================================\n")
cat("Extraction data validation\n")
cat("========================================\n\n")

print(validation_summary, row.names = FALSE)

if (length(missing_columns) > 0) {
  cat("\nMissing required columns:\n")
  cat(paste(missing_columns, collapse = ", "), "\n")
}

if (length(duplicated_column_names) > 0) {
  cat("\nDuplicated column names:\n")
  cat(paste(duplicated_column_names, collapse = ", "), "\n")
}

if (nrow(extraction_data) == 0) {
  cat(
    "\nNOTICE: The template structure is valid, ",
    "but no study data have been entered yet.\n",
    sep = ""
  )
}

if (
  length(missing_columns) == 0 &&
  length(duplicated_column_names) == 0
) {
  cat("\nPASS: The extraction-data structure is valid.\n")
} else {
  cat("\nCHECK: The extraction-data structure requires revision.\n")
}