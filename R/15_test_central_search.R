# Cardiac Surgery SR-MA Toolkit v2.0
# CENTRAL live-search test
#
# This script:
# 1. Reads the generated CENTRAL search strategy
# 2. Performs final local structural checks
# 3. Copies the search strategy to the Windows clipboard
# 4. Opens the Cochrane Library website
# 5. Records whether the search was accepted
# 6. Records the number of retrieved CENTRAL records
# 7. Saves the finalized search strategy
# 8. Appends the result to the shared live-search log
#
# CENTRAL does not provide a generally available search API
# for this workflow. Therefore, the live database test is
# performed through a reproducible semi-automated process.

# =========================================================
# 1. File paths
# =========================================================

search_file <-
  "search_strategies/central_search.txt"

final_search_file <-
  "search_strategies/central_search_final.txt"

test_report_file <-
  "results/logs/central_live_search_test.txt"

log_file <-
  "results/logs/live_search_test_log.csv"


# =========================================================
# 2. Preliminary checks
# =========================================================

if (!file.exists(search_file)) {

  stop(
    "CENTRAL search file does not exist: ",
    search_file,
    "\nRun R/11_generate_search_strategies.R first."
  )
}

required_packages <- c(
  "readr",
  "dplyr"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0) {

  stop(
    "The following required packages are missing: ",
    paste(missing_packages, collapse = ", "),
    "\nRun R/01_install_packages.R first."
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


# =========================================================
# 3. Read the CENTRAL search strategy
# =========================================================

central_search_lines <- readLines(
  search_file,
  warn = FALSE,
  encoding = "UTF-8"
)

central_search <- paste(
  central_search_lines,
  collapse = "\n"
)

central_search <- trimws(
  central_search
)

if (central_search == "") {

  stop(
    "The CENTRAL search strategy is empty."
  )
}


# =========================================================
# 4. Helper function
# =========================================================

count_fixed <- function(text, pattern) {

  matches <- gregexpr(
    pattern,
    text,
    fixed = TRUE
  )[[1]]

  if (
    length(matches) == 1 &&
    matches[1] == -1
  ) {
    return(0L)
  }

  length(matches)
}


# =========================================================
# 5. Final structural checks
# =========================================================

opening_parentheses <- count_fixed(
  central_search,
  "("
)

closing_parentheses <- count_fixed(
  central_search,
  ")"
)

double_quote_count <- count_fixed(
  central_search,
  '"'
)

parentheses_balanced <-
  opening_parentheses == closing_parentheses

double_quotes_balanced <-
  double_quote_count %% 2 == 0

has_boolean_and <- grepl(
  "\\bAND\\b",
  central_search,
  ignore.case = FALSE,
  perl = TRUE
)

has_boolean_or <- grepl(
  "\\bOR\\b",
  central_search,
  ignore.case = FALSE,
  perl = TRUE
)

has_title_abstract_keyword_field <-
  grepl(
    ":ti,ab,kw",
    central_search,
    fixed = TRUE
  )

has_mesh_descriptor <-
  grepl(
    '\\[mh\\s+"[^"]+"\\]',
    central_search,
    perl = TRUE
  )

local_structure_valid <- all(
  parentheses_balanced,
  double_quotes_balanced,
  has_boolean_and,
  has_boolean_or,
  has_mesh_descriptor
)


# =========================================================
# 6. Display local validation results
# =========================================================

cat("\n")
cat("====================================================\n")
cat("CENTRAL Live Search Test\n")
cat("====================================================\n\n")

cat("Local structural checks:\n")

cat(
  "- Parentheses balanced:",
  parentheses_balanced,
  "\n"
)

cat(
  "- Double quotes balanced:",
  double_quotes_balanced,
  "\n"
)

cat(
  "- AND operator present:",
  has_boolean_and,
  "\n"
)

cat(
  "- OR operator present:",
  has_boolean_or,
  "\n"
)

cat(
  "- :ti,ab,kw field present:",
  has_title_abstract_keyword_field,
  "\n"
)

cat(
  '- CENTRAL MeSH syntax [mh "..."] present:',
  has_mesh_descriptor,
  "\n\n"
)

if (!local_structure_valid) {

  stop(
    "The CENTRAL strategy failed one or more ",
    "required local structural checks.\n",
    "Run R/12_validate_search_strategies.R again ",
    "and correct the failed item."
  )
}


# =========================================================
# 7. Display the CENTRAL strategy
# =========================================================

cat("CENTRAL SEARCH STRATEGY\n")
cat("----------------------------------------------------\n")
cat(central_search)
cat("\n----------------------------------------------------\n\n")


# =========================================================
# 8. Copy to Windows clipboard
# =========================================================

clipboard_success <- FALSE

if (.Platform$OS.type == "windows") {

  clipboard_success <- tryCatch(
    {

      writeClipboard(
        central_search,
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
    "The CENTRAL search strategy has been copied ",
    "to the Windows clipboard.\n\n",
    sep = ""
  )

} else {

  cat(
    "Automatic clipboard copying was unavailable.\n",
    "Copy the displayed strategy manually.\n\n"
  )
}


# =========================================================
# 9. Optionally open Cochrane Library
# =========================================================

open_central <- readline(
  prompt = paste0(
    "Open the Cochrane Library website ",
    "in your browser? [Y/n]: "
  )
)

open_central <- tolower(
  trimws(open_central)
)

if (
  open_central == "" ||
  open_central %in% c("y", "yes")
) {

  utils::browseURL(
    "https://www.cochranelibrary.com/"
  )
}


# =========================================================
# 10. Manual live-search instructions
# =========================================================

cat("\nIn the Cochrane Library:\n")
cat("1. Sign in if required.\n")
cat("2. Open Advanced Search or Search Manager.\n")
cat("3. Paste the generated CENTRAL search strategy.\n")
cat("4. Run the search.\n")
cat("5. Check whether a syntax error is reported.\n")
cat("6. Select CENTRAL as the database or content type.\n")
cat("7. Record the total number of CENTRAL records.\n\n")


# =========================================================
# 11. Collect the live-test result
# =========================================================

accepted_answer <- readline(
  prompt = paste0(
    "Was the CENTRAL search accepted ",
    "without a syntax error? [y/N]: "
  )
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
    prompt = paste0(
      "Enter the number of CENTRAL records retrieved: "
    )
  )

  cleaned_count <- gsub(
    "[^0-9]",
    "",
    record_count_input
  )

  if (cleaned_count == "") {

    record_count <- NA_integer_

    warning(
      "The CENTRAL result count was empty ",
      "or could not be recognized."
    )

  } else {

    record_count <- suppressWarnings(
      as.integer(cleaned_count)
    )

    if (is.na(record_count)) {

      warning(
        "The CENTRAL result count could not ",
        "be converted to an integer."
      )
    }
  }

  error_message <- NA_character_

} else {

  record_count <- NA_integer_

  error_message <- readline(
    prompt = paste0(
      "Copy and enter the CENTRAL ",
      "syntax-error message: "
    )
  )

  error_message <- trimws(
    error_message
  )

  if (error_message == "") {

    error_message <-
      paste0(
        "Syntax error reported; ",
        "exact message not recorded."
      )
  }
}


# =========================================================
# 12. Record platform and searcher information
# =========================================================

platform_answer <- readline(
  prompt = paste0(
    "CENTRAL platform ",
    "[Cochrane Library]: "
  )
)

platform_answer <- trimws(
  platform_answer
)

if (platform_answer == "") {

  platform_answer <- "Cochrane Library"
}

tester_name <- readline(
  prompt = "Searcher or reviewer name: "
)

tester_name <- trimws(
  tester_name
)

if (tester_name == "") {

  tester_name <- NA_character_
}


# =========================================================
# 13. Save the accepted strategy as final candidate
# =========================================================

if (search_accepted) {

  writeLines(
    central_search_lines,
    con = final_search_file,
    useBytes = TRUE
  )
}


# =========================================================
# 14. Create the new log entry
#
# Important:
# test_date is explicitly saved as character to prevent
# datetime-character conflicts during bind_rows().
# =========================================================

new_log_entry <- data.frame(
  database = "CENTRAL",
  platform = platform_answer,
  search_file = search_file,
  test_date = format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  ),
  searcher = tester_name,
  record_count = record_count,
  request_successful = search_accepted,
  query_translation_available = FALSE,
  api_key_used = FALSE,
  email_supplied = FALSE,
  response_file = test_report_file,
  notes = if (search_accepted) {

    if (is.na(record_count)) {

      paste0(
        "The search was accepted by the CENTRAL interface, ",
        "but the record count was not recorded."
      )

    } else if (record_count == 0) {

      paste0(
        "The search was accepted by the CENTRAL interface, ",
        "but zero records were retrieved."
      )

    } else {

      paste0(
        "The search was accepted by the CENTRAL interface."
      )
    }

  } else {

    paste0(
      "The search was rejected by CENTRAL: ",
      error_message
    )
  },
  stringsAsFactors = FALSE
)

new_log_entry <- new_log_entry |>
  dplyr::mutate(
    test_date = as.character(test_date)
  )


# =========================================================
# 15. Read the existing shared log safely
#
# The test_date column is forced to character so that:
#
# existing log: datetime
# new row:      character
#
# does not cause a dplyr::bind_rows() error.
# =========================================================

if (file.exists(log_file)) {

  previous_log <- readr::read_csv(
    log_file,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_guess(),
      test_date = readr::col_character()
    )
  )

  previous_log <- previous_log |>
    dplyr::mutate(
      test_date = as.character(test_date)
    )

  updated_log <- dplyr::bind_rows(
    previous_log,
    new_log_entry
  )

} else {

  updated_log <- new_log_entry
}


# =========================================================
# 16. Save the updated shared log
# =========================================================

readr::write_csv(
  updated_log,
  log_file,
  na = ""
)


# =========================================================
# 17. Save the detailed CENTRAL test report
# =========================================================

report_lines <- c(
  "Cardiac Surgery SR-MA Toolkit v2.0",
  "CENTRAL Live Search Test Report",
  paste0(
    "Tested at: ",
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    )
  ),
  paste0(
    "Searcher: ",
    ifelse(
      is.na(tester_name),
      "Not recorded",
      tester_name
    )
  ),
  paste0(
    "Platform: ",
    platform_answer
  ),
  "",
  "LOCAL CHECKS",
  "--------------------------------------------------",
  paste0(
    "Parentheses balanced: ",
    parentheses_balanced
  ),
  paste0(
    "Double quotes balanced: ",
    double_quotes_balanced
  ),
  paste0(
    "AND operator present: ",
    has_boolean_and
  ),
  paste0(
    "OR operator present: ",
    has_boolean_or
  ),
  paste0(
    ":ti,ab,kw present: ",
    has_title_abstract_keyword_field
  ),
 paste0(
  'CENTRAL MeSH syntax [mh "..."] present: ',
  has_mesh_descriptor
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
      as.character(record_count)
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
  central_search
)

writeLines(
  report_lines,
  con = test_report_file,
  useBytes = TRUE
)


# =========================================================
# 18. Final console output
# =========================================================

cat("\n")
cat("====================================================\n")
cat("CENTRAL Search Test Result\n")
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

  if (is.na(record_count)) {

    cat(
      "\nCHECK: The CENTRAL search was accepted, ",
      "but the record count was not recorded.\n",
      sep = ""
    )

  } else if (record_count == 0) {

    cat(
      "\nCHECK: The CENTRAL search was accepted, ",
      "but zero records were retrieved.\n",
      sep = ""
    )

  } else {

    cat(
      "\nPASS: The CENTRAL search was accepted ",
      "and returned records.\n",
      sep = ""
    )
  }

  cat(
    "\nNext step:",
    "\n1. Review the number and relevance of CENTRAL records.",
    "\n2. Export the detailed CENTRAL citation records.",
    "\n3. Finalize all database search strategies.",
    "\n4. Import and merge PubMed, Embase, and CENTRAL records.",
    "\n"
  )

} else {

  cat(
    "\nCHECK: The CENTRAL search was not accepted.\n"
  )

  cat(
    "\nNext step:",
    "\n1. Review the saved CENTRAL error message.",
    "\n2. Correct search_strategies/central_search.txt.",
    "\n3. Run this script again.",
    "\n"
  )
}