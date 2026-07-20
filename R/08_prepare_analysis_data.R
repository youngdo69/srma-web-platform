# Cardiac Surgery SR-MA Toolkit v2.0
# Prepare a cleaned analysis dataset

source_file <- "data/raw/SGLT2i_data_extraction.csv"
output_file <- "data/processed/SGLT2i_analysis_data.csv"

extraction_data <- readr::read_csv(
  source_file,
  show_col_types = FALSE,
  na = c("", "NA", "N/A", "Not reported", "NR")
)

analysis_data <- extraction_data |>
  dplyr::mutate(
    study_id = stringr::str_trim(study_id),
    first_author = stringr::str_trim(first_author),
    study_design = stringr::str_to_lower(
      stringr::str_trim(study_design)
    ),
    outcome_name = stringr::str_to_lower(
      stringr::str_trim(outcome_name)
    ),
    timepoint = stringr::str_to_lower(
      stringr::str_trim(timepoint)
    ),
    extraction_status = stringr::str_to_lower(
      stringr::str_trim(extraction_status)
    )
  )

dir.create(
  "data/processed",
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  analysis_data,
  output_file,
  na = ""
)

cat("\n")
cat("Analysis dataset saved:\n")
cat(output_file, "\n")

cat("\nNumber of rows:\n")
cat(nrow(analysis_data), "\n")

cat("\nNumber of columns:\n")
cat(ncol(analysis_data), "\n")