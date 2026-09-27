# Cardiac Surgery SR-MA Toolkit v2.0
# Verify project configuration and extraction templates

required_files <- c(
  "config/project.yml",
  "templates/extraction/data_extraction_template.csv",
  "templates/extraction/data_dictionary.csv"
)

file_status <- data.frame(
  file = required_files,
  exists = file.exists(required_files),
  stringsAsFactors = FALSE
)

cat("\n")
cat("========================================\n")
cat("Project template verification\n")
cat("========================================\n\n")

print(file_status, row.names = FALSE)

if (!all(file_status$exists)) {

  missing_files <- file_status$file[!file_status$exists]

  stop(
    "Missing files: ",
    paste(missing_files, collapse = ", ")
  )
}

project_config <- yaml::read_yaml(
  "config/project.yml"
)

extraction_template <- readr::read_csv(
  "templates/extraction/data_extraction_template.csv",
  show_col_types = FALSE
)

data_dictionary <- readr::read_csv(
  "templates/extraction/data_dictionary.csv",
  show_col_types = FALSE
)

required_extraction_columns <- c(
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
  required_extraction_columns,
  names(extraction_template)
)

dictionary_variables <- data_dictionary$variable

missing_dictionary_entries <- setdiff(
  required_extraction_columns,
  dictionary_variables
)

cat("\nProject title:\n")
cat(project_config$project$title, "\n")

cat("\nNumber of extraction-template columns:\n")
cat(ncol(extraction_template), "\n")

cat("\nNumber of data-dictionary entries:\n")
cat(nrow(data_dictionary), "\n")

cat("\n")

if (length(missing_columns) == 0) {
  cat("PASS: All required extraction columns are present.\n")
} else {
  cat("CHECK: Required extraction columns are missing:\n")
  cat(paste(missing_columns, collapse = ", "), "\n")
}

if (length(missing_dictionary_entries) == 0) {
  cat("PASS: All required columns are documented in the data dictionary.\n")
} else {
  cat("CHECK: Some required columns are missing from the data dictionary:\n")
  cat(paste(missing_dictionary_entries, collapse = ", "), "\n")
}

if (
  length(missing_columns) == 0 &&
  length(missing_dictionary_entries) == 0
) {
  cat("\nPASS: Project configuration and extraction templates are ready.\n")
} else {
  cat("\nCHECK: Project templates require revision.\n")
}