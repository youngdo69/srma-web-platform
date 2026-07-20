# Cardiac Surgery SR-MA Toolkit v2.0
# Import the working data-extraction file

source_file <- "data/raw/SGLT2i_data_extraction.csv"

if (!file.exists(source_file)) {
  stop(
    "The data-extraction file does not exist: ",
    source_file
  )
}

extraction_data <- readr::read_csv(
  source_file,
  show_col_types = FALSE,
  na = c("", "NA", "N/A", "Not reported", "NR")
)

cat("\n")
cat("========================================\n")
cat("Data extraction file imported\n")
cat("========================================\n\n")

cat("File:\n")
cat(source_file, "\n\n")

cat("Number of rows:\n")
cat(nrow(extraction_data), "\n\n")

cat("Number of columns:\n")
cat(ncol(extraction_data), "\n\n")

cat("Column names:\n")
print(names(extraction_data))

assign(
  "extraction_data",
  extraction_data,
  envir = .GlobalEnv
)