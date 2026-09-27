# Cardiac Surgery SR-MA Toolkit v2.0
# Test the finalized PubMed search directly through NCBI ESearch
#
# This script:
# 1. Reads the generated PubMed search strategy
# 2. Sends it to PubMed through NCBI E-utilities
# 3. Retrieves the total result count
# 4. Records PubMed's translated query
# 5. Saves a reproducible live-search log

# ---------------------------------------------------------
# File paths
# ---------------------------------------------------------

search_file <- "search_strategies/pubmed_search.txt"

log_file <- "results/logs/live_search_test_log.csv"

translation_file <-
  "search_strategies/pubmed_query_translation.txt"

response_file <-
  "results/logs/pubmed_esearch_response.json"

# ---------------------------------------------------------
# Preliminary checks
# ---------------------------------------------------------

if (!file.exists(search_file)) {
  stop(
    "PubMed search file does not exist: ",
    search_file,
    "\nRun R/11_generate_search_strategies.R first."
  )
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop(
    "The jsonlite package is required. ",
    "Run R/01_install_packages.R first."
  )
}

dir.create(
  "results/logs",
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "search_strategies",
  recursive = TRUE,
  showWarnings = FALSE
)

# ---------------------------------------------------------
# Read the PubMed strategy
# ---------------------------------------------------------

pubmed_search <- readLines(
  search_file,
  warn = FALSE,
  encoding = "UTF-8"
)

pubmed_search <- paste(
  pubmed_search,
  collapse = "\n"
)

pubmed_search <- trimws(pubmed_search)

if (pubmed_search == "") {
  stop("The PubMed search strategy is empty.")
}

# ---------------------------------------------------------
# Optional NCBI identification settings
#
# These may later be placed in the .Renviron file:
#
# NCBI_EMAIL=your_email@example.com
# NCBI_API_KEY=your_api_key
# ---------------------------------------------------------

ncbi_email <- Sys.getenv(
  "NCBI_EMAIL",
  unset = ""
)

ncbi_api_key <- Sys.getenv(
  "NCBI_API_KEY",
  unset = ""
)

tool_name <- "cardiac-surgery-srma-toolkit"

# ---------------------------------------------------------
# URL helper
# ---------------------------------------------------------

encode_parameter <- function(name, value) {

  paste0(
    name,
    "=",
    utils::URLencode(
      as.character(value),
      reserved = TRUE
    )
  )
}

parameters <- c(
  encode_parameter("db", "pubmed"),
  encode_parameter("term", pubmed_search),
  encode_parameter("retmode", "json"),
  encode_parameter("retmax", "0"),
  encode_parameter("usehistory", "y"),
  encode_parameter("tool", tool_name)
)

if (ncbi_email != "") {
  parameters <- c(
    parameters,
    encode_parameter("email", ncbi_email)
  )
}

if (ncbi_api_key != "") {
  parameters <- c(
    parameters,
    encode_parameter("api_key", ncbi_api_key)
  )
}

base_url <- paste0(
  "https://eutils.ncbi.nlm.nih.gov/",
  "entrez/eutils/esearch.fcgi"
)

request_url <- paste0(
  base_url,
  "?",
  paste(parameters, collapse = "&")
)

# ---------------------------------------------------------
# Submit the live PubMed search
# ---------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("Live PubMed Search Test\n")
cat("====================================================\n\n")

cat("Submitting the search to PubMed...\n")

search_response <- tryCatch(
  {
    jsonlite::fromJSON(
      request_url,
      simplifyVector = TRUE
    )
  },
  error = function(error) {

    stop(
      "The PubMed request failed: ",
      conditionMessage(error)
    )
  }
)

# Save the complete server response
jsonlite::write_json(
  search_response,
  path = response_file,
  pretty = TRUE,
  auto_unbox = TRUE
)

# ---------------------------------------------------------
# Check the returned structure
# ---------------------------------------------------------

if (!is.null(search_response$error)) {
  stop(
    "PubMed returned an error: ",
    search_response$error
  )
}

if (is.null(search_response$esearchresult)) {
  stop(
    "The PubMed response did not contain ",
    "an esearchresult object."
  )
}

result <- search_response$esearchresult

if (
  !is.null(result$errorlist) &&
  length(result$errorlist) > 0
) {

  stop(
    "PubMed search error: ",
    paste(
      unlist(result$errorlist),
      collapse = "; "
    )
  )
}

# ---------------------------------------------------------
# Extract the results
# ---------------------------------------------------------

record_count <- suppressWarnings(
  as.integer(result$count)
)

query_translation <- if (
  !is.null(result$querytranslation)
) {
  as.character(result$querytranslation)
} else {
  NA_character_
}

web_environment <- if (
  !is.null(result$webenv)
) {
  as.character(result$webenv)
} else {
  NA_character_
}

query_key <- if (
  !is.null(result$querykey)
) {
  as.character(result$querykey)
} else {
  NA_character_
}

if (is.na(record_count)) {
  stop(
    "The number of PubMed records could not be determined."
  )
}

# ---------------------------------------------------------
# Save PubMed's translated query
# ---------------------------------------------------------

translation_lines <- c(
  "PubMed Query Translation",
  paste0("Tested at: ", Sys.time()),
  "",
  "ORIGINAL SEARCH",
  "--------------------------------------------------",
  pubmed_search,
  "",
  "PUBMED TRANSLATED QUERY",
  "--------------------------------------------------",
  query_translation
)

writeLines(
  translation_lines,
  con = translation_file,
  useBytes = TRUE
)

# ---------------------------------------------------------
# Create the search log entry
# ---------------------------------------------------------

new_log_entry <- data.frame(
  database = "PubMed",
  platform = "NCBI PubMed",
  search_file = search_file,
  test_date = as.character(Sys.time()),
  record_count = record_count,
  request_successful = TRUE,
  query_translation_available =
    !is.na(query_translation),
  api_key_used = ncbi_api_key != "",
  email_supplied = ncbi_email != "",
  response_file = response_file,
  notes = if (record_count == 0) {
    "The search ran successfully but retrieved zero records."
  } else {
    "The search was accepted by PubMed."
  },
  stringsAsFactors = FALSE
)

if (file.exists(log_file)) {

  previous_log <- readr::read_csv(
    log_file,
    show_col_types = FALSE
  )

  updated_log <- dplyr::bind_rows(
    previous_log,
    new_log_entry
  )

} else {

  updated_log <- new_log_entry
}

readr::write_csv(
  updated_log,
  log_file
)

# ---------------------------------------------------------
# Console report
# ---------------------------------------------------------

cat("\nSearch accepted by PubMed:\n")
cat("TRUE\n")

cat("\nNumber of records retrieved:\n")
cat(record_count, "\n")

cat("\nPubMed query translation:\n")
cat(query_translation, "\n")

cat("\nFiles saved:\n")
cat("-", response_file, "\n")
cat("-", translation_file, "\n")
cat("-", log_file, "\n")

if (record_count == 0) {

  cat(
    "\nCHECK: PubMed accepted the search, ",
    "but no records were retrieved.\n",
    sep = ""
  )

  cat(
    "Review the concepts and controlled vocabulary ",
    "before proceeding.\n"
  )

} else {

  cat(
    "\nPASS: The PubMed search was accepted ",
    "and returned records.\n",
    sep = ""
  )

  cat(
    "\nNext step:",
    "\n1. Review PubMed's translated query.",
    "\n2. Check whether known relevant studies were retrieved.",
    "\n3. Proceed to the Embase live-search test.",
    "\n"
  )
}