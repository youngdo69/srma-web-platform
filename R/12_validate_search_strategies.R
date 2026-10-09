# ============================================================
# Universal SR-MA Agent Toolkit
# R/12_validate_search_strategies.R
#
# Purpose
# ------------------------------------------------------------
# 1. Load the active canonical project configuration
# 2. Resolve search-strategy and log paths through path_utils.R
# 3. Validate only databases enabled in project_config.yml
# 4. Preserve established checks and validate CENTRAL as a guided Cochrane UI plan
#
# IMPORTANT
# ------------------------------------------------------------
# This script performs local structural and syntax-pattern
# checks only. Final validity must still be confirmed by
# running each query in the corresponding database interface.
# ============================================================


# ---------------------------------------------------------
# Load canonical project configuration and paths
# ---------------------------------------------------------

source("R/utils/config_utils.R")
source("R/utils/path_utils.R")

config <- load_project_config(
  "config/project_config.yml",
  validate = TRUE,
  quiet = TRUE
)

paths <- build_project_paths(
  config = config,
  create_directories = TRUE,
  validate = TRUE
)

enabled_databases <- get_enabled_databases(
  config,
  validate = FALSE
)

database_display_names <- c(
  pubmed = "PubMed",
  embase = "Embase",
  central = "CENTRAL"
)

canonical_strategy_files <- c(
  pubmed = paths$pubmed_search,
  embase = paths$embase_search,
  central = paths$central_search
)

supported_enabled_databases <- intersect(
  enabled_databases,
  names(canonical_strategy_files)
)

if (length(supported_enabled_databases) == 0L) {
  stop(
    "No supported databases are enabled in project_config.yml.\n",
    "Supported databases: pubmed, embase, central"
  )
}

unsupported_enabled_databases <- setdiff(
  enabled_databases,
  names(canonical_strategy_files)
)

if (length(unsupported_enabled_databases) > 0L) {
  warning(
    "Enabled databases without an R/12 validator were skipped: ",
    paste(unsupported_enabled_databases, collapse = ", ")
  )
}

strategy_files <- canonical_strategy_files[
  supported_enabled_databases
]

names(strategy_files) <- unname(
  database_display_names[
    supported_enabled_databases
  ]
)

output_csv <- file.path(
  paths$logs_directory,
  "search_strategy_validation.csv"
)

output_txt <- file.path(
  paths$logs_directory,
  "search_strategy_validation_report.txt"
)

# ---------------------------------------------------------
# General helper functions
# ---------------------------------------------------------

read_strategy <- function(file_path) {

  if (!file.exists(file_path)) {
    return(NA_character_)
  }

  lines <- readLines(
    file_path,
    warn = FALSE,
    encoding = "UTF-8"
  )

  paste(lines, collapse = "\n")
}

count_fixed <- function(text, pattern) {

  if (
    length(text) == 0 ||
    is.na(text) ||
    text == ""
  ) {
    return(0L)
  }

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

count_regex <- function(text, pattern) {

  if (
    length(text) == 0 ||
    is.na(text) ||
    text == ""
  ) {
    return(0L)
  }

  matches <- gregexpr(
    pattern,
    text,
    perl = TRUE
  )[[1]]

  if (identical(matches, -1L)) {
    return(0L)
  }

  length(matches)
}

has_regex <- function(text, pattern) {

  if (
    length(text) == 0 ||
    is.na(text) ||
    text == ""
  ) {
    return(FALSE)
  }

  grepl(
    pattern,
    text,
    perl = TRUE
  )
}

normalize_line <- function(x) {

  x |>
    trimws() |>
    tolower() |>
    gsub("\\s+", " ", x = _, perl = TRUE)
}

# ---------------------------------------------------------
# Balanced-character checks
# ---------------------------------------------------------

check_parentheses <- function(text) {

  opening_count <- count_fixed(text, "(")
  closing_count <- count_fixed(text, ")")

  list(
    passed = opening_count == closing_count,
    opening = opening_count,
    closing = closing_count,
    message = paste0(
      "Opening parentheses: ",
      opening_count,
      "; closing parentheses: ",
      closing_count
    )
  )
}

check_double_quotes <- function(text) {

  quote_count <- count_fixed(text, '"')

  list(
    passed = quote_count %% 2 == 0,
    count = quote_count,
    message = paste0(
      "Double-quote count: ",
      quote_count
    )
  )
}

check_single_quotes <- function(text) {

  quote_count <- count_fixed(text, "'")

  list(
    passed = quote_count %% 2 == 0,
    count = quote_count,
    message = paste0(
      "Single-quote count: ",
      quote_count
    )
  )
}

# ---------------------------------------------------------
# Boolean checks
# ---------------------------------------------------------

check_boolean_placement <- function(text) {

  errors <- character(0)

  if (has_regex(text, "^\\s*(AND|OR)\\b")) {
    errors <- c(
      errors,
      "The search begins with AND or OR."
    )
  }

  if (has_regex(text, "\\b(AND|OR)\\s*$")) {
    errors <- c(
      errors,
      "The search ends with AND or OR."
    )
  }

  if (
    has_regex(
      text,
      "\\b(AND|OR)\\s+(AND|OR)\\b"
    )
  ) {
    errors <- c(
      errors,
      "Consecutive Boolean operators were detected."
    )
  }

  if (
    has_regex(
      text,
      "\\(\\s*(AND|OR)\\b"
    )
  ) {
    errors <- c(
      errors,
      "A Boolean operator appears immediately after an opening parenthesis."
    )
  }

  if (
    has_regex(
      text,
      "\\b(AND|OR)\\s*\\)"
    )
  ) {
    errors <- c(
      errors,
      "A Boolean operator appears immediately before a closing parenthesis."
    )
  }

  list(
    passed = length(errors) == 0,
    message = if (length(errors) == 0) {
      "Boolean placement appears valid."
    } else {
      paste(errors, collapse = " ")
    }
  )
}

check_boolean_presence <- function(text) {

  and_count <- count_regex(
    text,
    "\\bAND\\b"
  )

  or_count <- count_regex(
    text,
    "\\bOR\\b"
  )

  list(
    passed = and_count >= 1 && or_count >= 1,
    and_count = and_count,
    or_count = or_count,
    message = paste0(
      "AND count: ",
      and_count,
      "; OR count: ",
      or_count
    )
  )
}

# ---------------------------------------------------------
# Duplicate-line check
# ---------------------------------------------------------

check_duplicate_lines <- function(text) {

  # R/11 builds logical concept blocks and joins them with
  # a standalone AND. The same controlled-vocabulary term
  # may legitimately occur in different concept blocks.
  blocks <- unlist(
    strsplit(
      text,
      split = "\\n\\s*AND\\s*\\n",
      perl = TRUE
    )
  )

  duplicate_records <- character(0)

  for (block_index in seq_along(blocks)) {

    lines <- unlist(
      strsplit(
        blocks[[block_index]],
        split = "\n",
        fixed = TRUE
      )
    )

    lines <- trimws(lines)

    lines <- lines[
      lines != "" &
        !lines %in% c("(", ")", "AND", "OR")
    ]

    if (length(lines) == 0L) {
      next
    }

    # Ignore a leading Boolean connector when comparing terms.
    # This also detects a duplicate when the first occurrence
    # is the first term in a block and the second begins with OR.
    lines <- sub(
      "^(AND|OR)\\s+",
      "",
      lines,
      ignore.case = TRUE,
      perl = TRUE
    )

    normalized <- vapply(
      lines,
      normalize_line,
      character(1)
    )

    duplicated_values <- unique(
      normalized[duplicated(normalized)]
    )

    if (length(duplicated_values) > 0L) {
      duplicate_records <- c(
        duplicate_records,
        paste0(
          "concept block ",
          block_index,
          ": ",
          duplicated_values
        )
      )
    }
  }

  list(
    passed = length(duplicate_records) == 0L,
    duplicates = duplicate_records,
    message = if (length(duplicate_records) == 0L) {
      "No duplicated search lines were detected within logical concept blocks."
    } else {
      paste0(
        "Duplicated lines within the same logical concept block: ",
        paste(duplicate_records, collapse = " | ")
      )
    }
  )
}

# ---------------------------------------------------------
# PubMed-specific checks
# ---------------------------------------------------------

validate_pubmed <- function(text) {

  checks <- list()

  checks$field_title_abstract <- list(
    passed = has_regex(
      text,
      "\\[Title/Abstract\\]"
    ),
    message = "At least one [Title/Abstract] field tag is required."
  )

  checks$mesh_tag <- list(
    passed = has_regex(
      text,
      "\\[(Mesh|MeSH Terms)\\]"
    ),
    message = paste(
      "No MeSH field tag was detected.",
      "This may be acceptable only if intentionally omitted."
    )
  )

  checks$no_central_tags <- list(
    passed = !has_regex(
      text,
      ":ti,ab,kw"
    ),
    message = "CENTRAL-style :ti,ab,kw syntax was detected in PubMed."
  )

  checks$no_embase_tags <- list(
    passed = !has_regex(
      text,
      "'/exp|:ti,ab,kw"
    ),
    message = "Embase-style syntax may be present in PubMed."
  )

  checks$bracket_balance <- list(
    passed = (
      count_fixed(text, "[") ==
        count_fixed(text, "]")
    ),
    message = paste0(
      "Opening square brackets: ",
      count_fixed(text, "["),
      "; closing square brackets: ",
      count_fixed(text, "]")
    )
  )

  checks
}

# ---------------------------------------------------------
# Elsevier Embase-specific checks
# ---------------------------------------------------------

validate_embase <- function(text) {

  checks <- list()

  checks$text_fields <- list(
    passed = has_regex(
      text,
      ":ti,ab,kw"
    ),
    message = "At least one :ti,ab,kw field restriction is required."
  )

  checks$emtree_descriptor <- list(
    passed = (
      has_regex(
        text,
        "'/exp"
      ) ||
      has_regex(
        text,
        "'/de"
      )
    ),
    message = paste(
      "No Emtree descriptor using '/exp or '/de was detected.",
      "Either exploded (/exp) or non-exploded (/de) Emtree syntax is acceptable."
    )
  )

  checks$no_pubmed_tags <- list(
    passed = !has_regex(
      text,
      "\\[(Title/Abstract|Mesh|MeSH Terms)\\]"
    ),
    message = "PubMed-style field tags were detected in Embase."
  )

  checks$no_central_mesh <- list(
  passed = !has_regex(
    text,
    '\\[mh\\s+"[^"]+"\\]'
  ),
  message = 'CENTRAL MeSH syntax [mh "..."] was detected in Embase.'
)

  checks$no_and_not <- list(
    passed = !has_regex(
      text,
      "\\bAND\\s+NOT\\b"
    ),
    message = paste(
      "Embase uses NOT directly for exclusion.",
      "Replace AND NOT with NOT."
    )
  )

  checks
}

# ---------------------------------------------------------
# CENTRAL-specific checks
# ---------------------------------------------------------

check_central_line_references <- function(text) {
  lines <- trimws(
    unlist(
      strsplit(text, "\n", fixed = TRUE)
    )
  )

  defined <- integer(0)
  current_line <- NA_integer_
  errors <- character(0)

  for (line in lines) {
    if (grepl("^SEARCH_MANAGER_LINE: #[0-9]+$", line, perl = TRUE)) {
      current_line <- suppressWarnings(
        as.integer(
          sub("^SEARCH_MANAGER_LINE: #", "", line)
        )
      )

      if (is.na(current_line)) {
        errors <- c(errors, paste0("Invalid Search Manager line: ", line))
        next
      }

      if (current_line %in% defined) {
        errors <- c(
          errors,
          paste0("Search Manager line is defined more than once: #", current_line)
        )
      }

      if (length(defined) > 0L && current_line <= max(defined)) {
        errors <- c(
          errors,
          paste0("Search Manager lines are not strictly increasing at #", current_line)
        )
      }

      defined <- c(defined, current_line)
      next
    }

    if (grepl("^COMBINE:", line, perl = TRUE)) {
      refs <- regmatches(
        line,
        gregexpr("#[0-9]+", line, perl = TRUE)
      )[[1]]

      if (identical(refs, character(0)) || identical(refs, "")) {
        errors <- c(errors, paste0("Combination has no line references: ", line))
        next
      }

      ref_numbers <- suppressWarnings(
        as.integer(sub("^#", "", refs))
      )

      if (is.na(current_line)) {
        errors <- c(errors, paste0("Combination has no assigned output line: ", line))
        next
      }

      bad_refs <- ref_numbers[
        is.na(ref_numbers) |
          !(ref_numbers %in% defined) |
          ref_numbers >= current_line
      ]

      if (length(bad_refs) > 0L) {
        errors <- c(
          errors,
          paste0(
            "Combination references undefined or non-prior lines: ",
            paste(unique(bad_refs), collapse = ", ")
          )
        )
      }
    }
  }

  list(
    passed = length(errors) == 0L && length(defined) >= 2L,
    message = if (length(errors) == 0L && length(defined) >= 2L) {
      paste0(
        "Search Manager line references are ordered and internally consistent; ",
        length(defined),
        " line(s) defined."
      )
    } else if (length(errors) > 0L) {
      paste(errors, collapse = " ")
    } else {
      "At least two Search Manager lines must be defined."
    }
  )
}

validate_central <- function(text) {

  checks <- list()

  checks$guided_plan_marker <- list(
    passed = grepl(
      "CENTRAL GUIDED SEARCH PLAN",
      text,
      fixed = TRUE
    ),
    message = paste(
      "CENTRAL must use the guided Advanced Search UI plan emitted by R/11,",
      "not a single paste-ready query string."
    )
  )

  checks$execution_mode <- list(
    passed = grepl(
      "EXECUTION_MODE: COCHRANE_LIBRARY_ADVANCED_SEARCH_UI",
      text,
      fixed = TRUE
    ),
    message = "CENTRAL execution mode must be COCHRANE_LIBRARY_ADVANCED_SEARCH_UI."
  )

  checks$no_slash <- list(
    passed = !grepl("/", text, fixed = TRUE),
    message = paste(
      "CENTRAL guided plan must not contain '/'.",
      "Qualified PubMed/Embase syntax must not be carried into CENTRAL."
    )
  )

  checks$no_direct_field_suffix <- list(
    passed = !has_regex(
      text,
      ":ti,ab,kw"
    ),
    message = paste(
      "Do not type :ti,ab,kw directly into CENTRAL query text.",
      "Use Field = Title Abstract Keyword in the Search UI."
    )
  )

  checks$no_direct_mesh_syntax <- list(
    passed = !has_regex(
      text,
      '\\[mh\\s+|MeSH\\s+descriptor\\s*:'
    ),
    message = paste(
      "Do not paste [mh ...] or MeSH descriptor: syntax into Search Manager.",
      "Resolve MeSH through Medical terms (MeSH) and add it to Search Manager."
    )
  )

  checks$mesh_guidance <- list(
    passed = grepl(
      "MESH_CANDIDATE:",
      text,
      fixed = TRUE
    ),
    message = "CENTRAL plan must explicitly record MeSH candidate handling."
  )

  has_free_text_query <- has_regex(
    text,
    "SEARCH_TEXT:\s*\("
  )

  checks$free_text_guidance <- list(
    passed = grepl(
      "SEARCH_TEXT:",
      text,
      fixed = TRUE
    ),
    message = "CENTRAL plan must explicitly record free-text handling."
  )

  checks$field_selection <- list(
    passed = (
      !has_free_text_query ||
        grepl(
          "FIELD_SELECTION: Title Abstract Keyword",
          text,
          fixed = TRUE
        )
    ),
    message = paste(
      "Every CENTRAL free-text query must be sent from Search with",
      "Field = Title Abstract Keyword."
    )
  )

  checks$search_manager_lines <- check_central_line_references(text)

  checks$final_combination <- list(
    passed = (
      grepl(
        "FINAL_SEARCH_MANAGER_COMBINATION",
        text,
        fixed = TRUE
      ) &&
        has_regex(
          text,
          "COMBINE:\s*#[0-9]+(?:\s+AND\s+#[0-9]+)+"
        )
    ),
    message = paste(
      "CENTRAL plan must end with a Search Manager combination",
      "that joins concept-result lines with AND."
    )
  )

  checks$no_pubmed_tags <- list(
    passed = !has_regex(
      text,
      "\\[(Title/Abstract|Mesh|MeSH Terms)\\]"
    ),
    message = "PubMed-style field tags were detected in CENTRAL."
  )

  checks$no_embase_explosion <- list(
    passed = !has_regex(
      text,
      "'/exp|'/de"
    ),
    message = "Embase descriptor syntax was detected in CENTRAL."
  )

  checks
}

# ---------------------------------------------------------
# Convert check list to data frame
# ---------------------------------------------------------

checks_to_dataframe <- function(
  database,
  category,
  checks
) {

  data.frame(
    database = database,
    category = category,
    check = names(checks),
    passed = vapply(
      checks,
      function(x) isTRUE(x$passed),
      logical(1)
    ),
    message = vapply(
      checks,
      function(x) x$message,
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------
# Validate one strategy
# ---------------------------------------------------------

validate_strategy <- function(
  database,
  file_path
) {

  file_exists <- file.exists(file_path)

  if (!file_exists) {

    return(
      data.frame(
        database = database,
        category = "file",
        check = "file_exists",
        passed = FALSE,
        message = paste0(
          "Missing file: ",
          file_path
        ),
        stringsAsFactors = FALSE
      )
    )
  }

  text <- read_strategy(file_path)

  common_general_checks <- list(

    file_not_empty = list(
      passed = (
        !is.na(text) &&
          nchar(trimws(text)) > 0
      ),
      message = "The search-strategy file must not be empty."
    ),

    minimum_length = list(
      passed = (
        !is.na(text) &&
          nchar(text) >= 20
      ),
      message = paste0(
        "Search length: ",
        ifelse(is.na(text), 0, nchar(text)),
        " characters."
      )
    ),

    parentheses = check_parentheses(text)
  )

  # CENTRAL is now a guided UI execution plan rather than a single
  # database query. Generic Boolean-placement and duplicate-line checks
  # would incorrectly treat repeated instruction labels as query terms.
  # CENTRAL-specific structure is validated below instead.
  general_checks <- if (
    identical(database, "CENTRAL") &&
      grepl("CENTRAL GUIDED SEARCH PLAN", text, fixed = TRUE)
  ) {
    common_general_checks
  } else {
    c(
      common_general_checks,
      list(
        boolean_placement = check_boolean_placement(text),
        boolean_presence = check_boolean_presence(text),
        duplicate_lines = check_duplicate_lines(text)
      )
    )
  }

  general_report <- checks_to_dataframe(
    database = database,
    category = "general",
    checks = general_checks
  )

  database_checks <- switch(

  database,

  PubMed = c(
    list(
      double_quotes = check_double_quotes(text)
    ),
    validate_pubmed(text)
  ),

  Embase = c(
    list(
      single_quotes = check_single_quotes(text)
    ),
    validate_embase(text)
  ),

  CENTRAL = c(
    list(
      double_quotes = check_double_quotes(text)
    ),
    validate_central(text)
  ),

  stop(
    "Unsupported database: ",
    database
  )
)

  database_report <- checks_to_dataframe(
    database = database,
    category = "database_specific",
    checks = database_checks
  )

  rbind(
    general_report,
    database_report
  )
}

# ---------------------------------------------------------
# Display canonical inputs
# ---------------------------------------------------------

cat("\n")
cat("Canonical search-strategy inputs\n")
cat("--------------------------------\n")

for (database in names(strategy_files)) {
  cat(
    database,
    ": ",
    strategy_files[[database]],
    "\n",
    sep = ""
  )
}


# ---------------------------------------------------------
# Run validation
# ---------------------------------------------------------

validation_results <- do.call(
  rbind,
  lapply(
    names(strategy_files),
    function(database) {

      validate_strategy(
        database = database,
        file_path = strategy_files[[database]]
      )
    }
  )
)

row.names(validation_results) <- NULL

# ---------------------------------------------------------
# Summary
# ---------------------------------------------------------

validation_summary <- validation_results |>
  dplyr::group_by(database) |>
  dplyr::summarise(
    total_checks = dplyr::n(),
    passed_checks = sum(passed),
    failed_checks = sum(!passed),
    overall_pass = all(passed),
    .groups = "drop"
  )

# ---------------------------------------------------------
# Save reports
# ---------------------------------------------------------

readr::write_csv(
  validation_results,
  output_csv
)

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "Search Strategy Validation Report",
  paste0("Generated: ", Sys.time()),
  "",
  "SUMMARY",
  "--------------------------------------------------",
  capture.output(
    print(
      validation_summary,
      row.names = FALSE
    )
  ),
  "",
  "DETAILED RESULTS",
  "--------------------------------------------------",
  capture.output(
    print(
      validation_results,
      row.names = FALSE
    )
  ),
  "",
  "IMPORTANT",
  paste(
    "This report performs local structural checks only.",
    "It does not guarantee acceptance by PubMed, Embase,",
    "or successful execution in the Cochrane Library search interface."
  )
)

writeLines(
  report_lines,
  con = output_txt,
  useBytes = TRUE
)

# ---------------------------------------------------------
# Console output
# ---------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("Search Strategy Validation\n")
cat("====================================================\n\n")

print(
  validation_summary,
  row.names = FALSE
)

cat("\n")

failed_results <- validation_results |>
  dplyr::filter(!passed)

if (nrow(failed_results) == 0) {

  cat(
    "PASS: All local search-strategy checks passed.\n"
  )

} else {

  cat(
    "CHECK: Some search-strategy checks failed.\n\n"
  )

  print(
    failed_results,
    row.names = FALSE
  )
}

cat("\nReports saved:\n")
cat("-", output_csv, "\n")
cat("-", output_txt, "\n")

if (nrow(failed_results) == 0) {

  cat(
    "\nNext step:",
    "\n1. Test each search in its actual database.",
    "\n2. Record the search date and number of results.",
    "\n3. Save the finalized search strategies.",
    "\n"
  )

  cat(
    "\nPASS: R/12 canonical-path validation completed successfully.\n"
  )

} else {

  cat(
    "\nNext step:",
    "\n1. Review every failed item listed above.",
    "\n2. Correct the corresponding search strategy.",
    "\n3. Run this validation script again.",
    "\n"
  )

  stop(
    "R/12 canonical-path validation failed: ",
    nrow(failed_results),
    " check(s) did not pass."
  )
}
