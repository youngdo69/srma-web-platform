# Cardiac Surgery SR-MA Toolkit v2.0
# Test and document the Embase search strategy
#
# This script:
# 1. Reads the generated Elsevier Embase search
# 2. Performs final local checks
# 3. Displays and copies the search for manual testing
# 4. Records whether Embase accepted the search
# 5. Records the number of retrieved records
#
# Embase Search API access is restricted.
# Therefore, this script uses a reproducible semi-automated workflow.

# ---------------------------------------------------------
# File paths
# ---------------------------------------------------------

search_file <- "search_strategies/embase_search.txt"

test_report_file <-
  "results/logs/embase_live_search_test.txt"

log_file <-
  "results/logs/live_search_test_log.csv"

final_search_file <-
  "search_strategies/embase_search_final.txt"

# ---------------------------------------------------------
# Preliminary checks
# ---------------------------------------------------------

if (!file.exists(search_file)) {
  stop(
    "Embase search file does not exist: ",
    search_file,
    "\nRun R/11_generate_search_strategies.R first."
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
# Read the Embase strategy
# ---------------------------------------------------------

embase_search_lines <- readLines(
  search_file,
  warn = FALSE,
  encoding = "UTF-8"
)

embase_search <- paste(
  embase_search_lines,
  collapse = "\n"
)

embase_search <- trimws(embase_search)

if (embase_search == "") {
  stop("The Embase search strategy is empty.")
}

# ---------------------------------------------------------
# Final structural checks
# ---------------------------------------------------------

count_fixed <- function(text, pattern) {

  matches <- gregexpr(
    pattern,
    text,
    fixed = TRUE
  )[[1]]

  if (identical(matches, -1L)) {
    return(0L)
  }

  length(matches)
}

opening_parentheses <- count_fixed(
  embase_search,
  "("
)

closing_parentheses <- count_fixed(
  embase_search,
  ")"
)

single_quote_count <- count_fixed(
  embase_search,
  "'"
)

has_text_field <- grepl(
  ":ti,ab,kw",
  embase_search,
  fixed = TRUE
)

has_emtree_term <- grepl(
  "'/exp",
  embase_search,
  fixed = TRUE
)

local_structure_valid <- all(
  opening_parentheses == closing_parentheses,
  single_quote_count %% 2 == 0,
  has_text_field
)

cat("\n")
cat("====================================================\n")
cat("Embase Live Search Test\n")
cat("====================================================\n\n")

cat("Local structural checks:\n")
cat(
  "- Parentheses balanced:",
  opening_parentheses == closing_parentheses,
  "\n"
)

cat(
  "- Single quotes balanced:",
  single_quote_count %% 2 == 0,
  "\n"
)

cat(
  "- :ti,ab,kw present:",
  has_text_field,
  "\n"
)

cat(
  "- Exploded Emtree term present:",
  has_emtree_term,
  "\n\n"
)

if (!local_structure_valid) {
  stop(
    "The Embase strategy failed local structural checks. ",
    "Run R/12_validate_search_strategies.R again."
  )
}

# ---------------------------------------------------------
# Display the strategy
# ---------------------------------------------------------

cat("EMBASE SEARCH STRATEGY\n")
cat("----------------------------------------------------\n")
cat(embase_search)
cat("\n----------------------------------------------------\n\n")

# ---------------------------------------------------------
# Copy to Windows clipboard when possible
# ---------------------------------------------------------

clipboard_success <- FALSE

if (.Platform$OS.type == "windows") {

  clipboard_success <- tryCatch(
    {
      writeClipboard(
        embase_search,
        format = 1
      )

      TRUE
    },
    error = function(error) {
      FALSE
    }
  )
}

if (clipboard_success) {

  cat(
    "The Embase search strategy has been copied ",
    "to the Windows clipboard.\n\n",
    sep = ""
  )

} else {

  cat(
    "Automatic clipboard copying was unavailable.\n",
    "Copy the displayed strategy manually.\n\n"
  )
}

# ---------------------------------------------------------
# Optional browser opening
# ---------------------------------------------------------

open_embase <- readline(
  prompt = paste0(
    "Open the Embase website in your browser? ",
    "[Y/n]: "
  )
)

open_embase <- tolower(
  trimws(open_embase)
)

if (
  open_embase == "" ||
  open_embase %in% c("y", "yes")
) {

  utils::browseURL(
    "https://www.embase.com/"
  )
}

# ---------------------------------------------------------
# Manual test instructions
# ---------------------------------------------------------

cat("\nIn Embase:\n")
cat("1. Log in through your institutional account.\n")
cat("2. Open Advanced Search or Query Builder.\n")
cat("3. Paste the generated search strategy.\n")
cat("4. Run the search.\n")
cat("5. Check whether Embase reports a syntax error.\n")
cat("6. Record the total number of retrieved results.\n\n")

# ---------------------------------------------------------
# Collect the live-test result
# ---------------------------------------------------------

accepted_answer <- readline(
  prompt = "Was the search accepted without a syntax error? [y/N]: "
)

accepted_answer <- tolower(
  trimws(accepted_answer)
)

search_accepted <- accepted_answer %in% c(
  "y",
  "yes"
)

if (search_accepted) {

  record_count_input <- readline(
    prompt = "Enter the number of Embase records retrieved: "
  )

  record_count <- suppressWarnings(
    as.integer(
      gsub(
        "[^0-9]",
        "",
        record_count_input
      )
    )
  )

  if (is.na(record_count)) {
    warning(
      "The result count was not recognized as an integer."
    )
  }

  error_message <- NA_character_

} else {

  record_count <- NA_integer_

  error_message <- readline(
    prompt = paste0(
      "Copy and enter the Embase syntax-error message: "
    )
  )

  if (trimws(error_message) == "") {
    error_message <- "Syntax error reported; exact message not recorded."
  }
}

# ---------------------------------------------------------
# Record platform and search date
# ---------------------------------------------------------

platform_answer <- readline(
  prompt = paste0(
    "Embase platform ",
    "[Elsevier Embase / Ovid Embase]: "
  )
)

platform_answer <- trimws(platform_answer)

if (platform_answer == "") {
  platform_answer <- "Elsevier Embase"
}

search_date <- format(
  Sys.Date(),
  "%Y-%m-%d"
)

tester_name <- readline(
  prompt = "Searcher or reviewer name: "
)

tester_name <- trimws(tester_name)

# ---------------------------------------------------------
# Save accepted strategy as final candidate
# ---------------------------------------------------------

if (search_accepted) {

  writeLines(
    embase_search_lines,
    con = final_search_file,
    useBytes = TRUE
  )
}

# ---------------------------------------------------------
# Create log entry
# ---------------------------------------------------------

new_log_entry <- data.frame(
  database = "Embase",
  platform = platform_answer,
  search_file = search_file,
  test_date = as.character(Sys.time()),
  searcher = tester_name,
  record_count = record_count,
  request_successful = search_accepted,
  query_translation_available = FALSE,
  api_key_used = FALSE,
  email_supplied = FALSE,
  response_file = test_report_file,
  notes = if (search_accepted) {
    "The search was accepted by the Embase web interface."
  } else {
    paste0(
      "The search was rejected by Embase: ",
      error_message
    )
  },
  stringsAsFactors = FALSE
)

if (file.exists(log_file)) {

  previous_log <- readr::read_csv(
    log_file,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_guess(),
      test_date = readr::col_character()
    )
  )

  new_log_entry <- new_log_entry |>
    dplyr::mutate(
      test_date = as.character(test_date)
    )

  updated_log <- dplyr::bind_rows(
    previous_log,
    new_log_entry
  )

} else {

  updated_log <- new_log_entry |>
    dplyr::mutate(
      test_date = as.character(test_date)
    )
}

# ---------------------------------------------------------
# Save detailed test report
# ---------------------------------------------------------

report_lines <- c(
  "Cardiac Surgery SR-MA Toolkit v2.0",
  "Embase Live Search Test Report",
  paste0("Tested at: ", Sys.time()),
  paste0("Searcher: ", tester_name),
  paste0("Platform: ", platform_answer),
  "",
  "LOCAL CHECKS",
  "--------------------------------------------------",
  paste0(
    "Parentheses balanced: ",
    opening_parentheses == closing_parentheses
  ),
  paste0(
    "Single quotes balanced: ",
    single_quote_count %% 2 == 0
  ),
  paste0(
    ":ti,ab,kw present: ",
    has_text_field
  ),
  paste0(
    "Exploded Emtree term present: ",
    has_emtree_term
  ),
  "",
  "LIVE DATABASE TEST",
  "--------------------------------------------------",
  paste0(
    "Search accepted: ",
    search_accepted
  ),
  paste0(
    "Records retrieved: ",
    ifelse(
      is.na(record_count),
      "Not available",
      record_count
    )
  ),
  paste0(
    "Error message: ",
    ifelse(
      is.na(error_message),
      "None",
      error_message
    )
  ),
  "",
  "SEARCH STRATEGY",
  "--------------------------------------------------",
  embase_search
)

writeLines(
  report_lines,
  con = test_report_file,
  useBytes = TRUE
)

# ---------------------------------------------------------
# Final console output
# ---------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("Embase Search Test Result\n")
cat("====================================================\n\n")

cat("Search accepted:\n")
cat(search_accepted, "\n\n")

cat("Records retrieved:\n")

if (is.na(record_count)) {
  cat("Not available\n")
} else {
  cat(record_count, "\n")
}

cat("\nFiles saved:\n")
cat("-", test_report_file, "\n")
cat("-", log_file, "\n")

if (search_accepted) {

  cat("-", final_search_file, "\n")

  cat(
    "\nPASS: The Embase search was accepted ",
    "by the database interface.\n",
    sep = ""
  )

  cat(
    "\nNext step:",
    "\n1. Review the number and relevance of retrieved records.",
    "\n2. Proceed to the CENTRAL live-search test.",
    "\n"
  )

} else {

  cat(
    "\nCHECK: The Embase search was not accepted.\n"
  )

  cat(
    "\nNext step:",
    "\n1. Review the saved Embase error message.",
    "\n2. Correct search_strategies/embase_search.txt.",
    "\n3. Run this test again.",
    "\n"
  )
}