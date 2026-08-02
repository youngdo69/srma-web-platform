# ============================================================
# SR-MA Agent Toolkit v2.0
# R/18_prepare_screening_workbook.R
#
# Purpose
# ------------------------------------------------------------
# 1. Load project configuration and shared paths
# 2. Import deduplicated records prepared by R/17
# 3. Generate independent title/abstract screening sheets
# 4. Add config-driven decision and exclusion-reason lists
# 5. Compare Reviewer 1 and Reviewer 2 decisions
# 6. Create a conflict-resolution sheet
# 7. Save and validate a reproducible Excel workbook
#
# Project-specific settings are defined in:
# config/project_config.yml
# ============================================================


# ============================================================
# 1. Required packages
# ============================================================

required_packages <- c(
  "openxlsx",
  "readr",
  "dplyr",
  "stringr",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0L) {

  stop(
    "The following packages are required but not installed: ",
    paste(
      missing_packages,
      collapse = ", "
    ),
    "\n\nInstall them with:\n",
    "install.packages(c(",
    paste(
      paste0(
        '"',
        missing_packages,
        '"'
      ),
      collapse = ", "
    ),
    "))"
  )
}


# ============================================================
# 2. Load project configuration and paths
# ============================================================

source(
  "R/utils/config_utils.R"
)

source(
  "R/utils/path_utils.R"
)

config <- load_project_config(
  quiet = FALSE
)

paths <- build_project_paths(
  config,
  create_directories = TRUE
)

project_id <- get_project_id(
  config
)

project_title <- get_project_title(
  config
)

project_short_name <- get_project_short_name(
  config
)


# ============================================================
# 3. Input and output paths
# ============================================================

input_file <-
  paths$deduplicated_records_csv

output_file <-
  paths$screening_workbook

report_file <-
  paths$screening_workbook_report

validate_screening_workbook_inputs(
  paths
)


# ============================================================
# 4. Config-driven screening settings
# ============================================================

decision_options <- get_screening_decisions(
  config
)

exclusion_reason_table <- get_exclusion_reasons(
  config
)

exclusion_reason_options <-
  exclusion_reason_table$label

reviewer_1_name <- get_reviewer_name(
  config,
  reviewer = "reviewer_1",
  fallback = "Reviewer 1"
)

reviewer_2_name <- get_reviewer_name(
  config,
  reviewer = "reviewer_2",
  fallback = "Reviewer 2"
)

third_reviewer_name <- get_reviewer_name(
  config,
  reviewer = "third_reviewer",
  fallback = "Third Reviewer"
)


# Final conflict-resolution choices remain workflow-level
# settings during Phase 1. They may be moved into config later.

final_decision_options <- c(
  "Include",
  "Exclude",
  "Awaiting full-text clarification"
)

confidence_options <- c(
  "High",
  "Moderate",
  "Low"
)


# ============================================================
# 5. Validate screening configuration
# ============================================================

if (length(decision_options) == 0L) {

  stop(
    "No screening decisions were found in ",
    "config/project_config.yml."
  )
}

if (length(exclusion_reason_options) == 0L) {

  stop(
    "No screening exclusion reasons were found in ",
    "config/project_config.yml."
  )
}

required_decisions <- c(
  "Include",
  "Exclude",
  "Unclear"
)

missing_required_decisions <- setdiff(
  required_decisions,
  decision_options
)

if (length(missing_required_decisions) > 0L) {

  stop(
    "The following required screening decisions are missing:\n- ",
    paste(
      missing_required_decisions,
      collapse = "\n- "
    )
  )
}


# ============================================================
# 6. Helper functions
# ============================================================

first_existing_column <- function(
    data,
    candidates,
    default = NA_character_
) {

  matching_columns <- candidates[
    candidates %in% names(data)
  ]

  if (length(matching_columns) == 0L) {

    return(
      rep(
        default,
        nrow(data)
      )
    )
  }

  data[[matching_columns[1]]]
}


clean_character <- function(x) {

  x <- as.character(x)

  x <- stringr::str_replace_all(
    x,
    "\u00A0",
    " "
  )

  x <- stringr::str_squish(
    x
  )

  x[
    is.na(x) |
      x == ""
  ] <- NA_character_

  x
}


excel_column_name <- function(column_number) {

  if (
    length(column_number) != 1L ||
      is.na(column_number) ||
      column_number < 1L
  ) {

    stop(
      "column_number must be one positive integer."
    )
  }

  column_number <- as.integer(
    column_number
  )

  column_name <- ""

  while (column_number > 0L) {

    remainder <- (
      column_number - 1L
    ) %% 26L

    column_name <- paste0(
      LETTERS[
        remainder + 1L
      ],
      column_name
    )

    column_number <- (
      column_number -
        remainder -
        1L
    ) %/% 26L
  }

  column_name
}


pad_character_vector <- function(
    values,
    target_length
) {

  values <- as.character(values)

  padding_length <-
    target_length -
    length(values)

  if (padding_length > 0L) {

    values <- c(
      values,
      rep(
        NA_character_,
        padding_length
      )
    )
  }

  values
}


build_validation_range <- function(
    column_letter,
    number_of_options
) {

  if (number_of_options < 1L) {

    stop(
      "number_of_options must be at least 1."
    )
  }

  paste0(
    "'Validation_Lists'!$",
    column_letter,
    "$2:$",
    column_letter,
    "$",
    number_of_options + 1L
  )
}


# ============================================================
# 7. Import deduplicated records
# ============================================================

screening_source <- readr::read_csv(
  input_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default =
      readr::col_character()
  )
)

if (nrow(screening_source) == 0L) {

  stop(
    "The deduplicated screening dataset contains no records."
  )
}


# ============================================================
# 8. Create standardized screening dataset
# ============================================================

screening_id <- first_existing_column(
  screening_source,
  c(
    "screening_id",
    "internal_record_id",
    "source_record_id"
  )
)

missing_screening_id <- (
  is.na(screening_id) |
    trimws(screening_id) == ""
)

screening_id[
  missing_screening_id
] <- paste0(
  "SCR_",
  sprintf(
    "%06d",
    which(
      missing_screening_id
    )
  )
)


screening_data <- tibble::tibble(
  screening_id = clean_character(
    screening_id
  ),

  source_databases = clean_character(
    first_existing_column(
      screening_source,
      c(
        "source_databases",
        "database"
      )
    )
  ),

  title = clean_character(
    first_existing_column(
      screening_source,
      c(
        "title",
        "article_title"
      )
    )
  ),

  abstract = clean_character(
    first_existing_column(
      screening_source,
      "abstract"
    )
  ),

  authors = clean_character(
    first_existing_column(
      screening_source,
      c(
        "authors",
        "author"
      )
    )
  ),

  year = clean_character(
    first_existing_column(
      screening_source,
      c(
        "year",
        "publication_year"
      )
    )
  ),

  journal = clean_character(
    first_existing_column(
      screening_source,
      c(
        "journal",
        "journal_title"
      )
    )
  ),

  doi = clean_character(
    first_existing_column(
      screening_source,
      "doi"
    )
  ),

  pmid = clean_character(
    first_existing_column(
      screening_source,
      "pmid"
    )
  ),

  publication_type = clean_character(
    first_existing_column(
      screening_source,
      "publication_type"
    )
  ),

  language = clean_character(
    first_existing_column(
      screening_source,
      "language"
    )
  ),

  url = clean_character(
    first_existing_column(
      screening_source,
      "url"
    )
  )
)


screening_data <- screening_data |>
  dplyr::distinct(
    screening_id,
    .keep_all = TRUE
  ) |>
  dplyr::arrange(
    screening_id
  )


if (anyDuplicated(screening_data$screening_id) > 0L) {

  stop(
    "Duplicate screening IDs remain after standardization."
  )
}


# ============================================================
# 9. Create reviewer datasets
# ============================================================

create_reviewer_data <- function(
    data,
    reviewer_name
) {

  data |>
    dplyr::mutate(
      reviewer =
        reviewer_name,

      decision =
        NA_character_,

      exclusion_reason =
        NA_character_,

      confidence =
        NA_character_,

      notes =
        NA_character_,

      screened_at =
        NA_character_
    ) |>
    dplyr::select(
      screening_id,
      source_databases,
      title,
      abstract,
      authors,
      year,
      journal,
      doi,
      pmid,
      publication_type,
      language,
      url,
      reviewer,
      decision,
      exclusion_reason,
      confidence,
      notes,
      screened_at
    )
}


reviewer_1_data <- create_reviewer_data(
  screening_data,
  reviewer_1_name
)

reviewer_2_data <- create_reviewer_data(
  screening_data,
  reviewer_2_name
)


# ============================================================
# 10. Create conflict-resolution dataset
# ============================================================

conflict_data <- screening_data |>
  dplyr::transmute(
    screening_id,
    title,
    year,
    journal,

    reviewer_1_decision =
      NA_character_,

    reviewer_2_decision =
      NA_character_,

    agreement_status =
      NA_character_,

    conflict_status =
      NA_character_,

    final_decision =
      NA_character_,

    final_exclusion_reason =
      NA_character_,

    adjudicator =
      third_reviewer_name,

    resolution_notes =
      NA_character_,

    resolved_at =
      NA_character_
  )


# ============================================================
# 11. Create source summary
# ============================================================

source_summary <- screening_data |>
  dplyr::mutate(
    source_databases = dplyr::if_else(
      is.na(source_databases),
      "Unknown",
      source_databases
    )
  ) |>
  dplyr::count(
    source_databases,
    name = "record_count"
  ) |>
  dplyr::arrange(
    dplyr::desc(
      record_count
    )
  )


source_summary <- dplyr::bind_rows(
  source_summary,

  tibble::tibble(
    source_databases =
      "Total unique records",

    record_count =
      nrow(screening_data)
  )
)


# ============================================================
# 12. Create instruction content
# ============================================================

instructions <- tibble::tibble(
  Section = c(
    "Project",
    "Project ID",
    "Purpose",
    "Independent review",
    "Reviewer 1",
    "Reviewer 2",
    "Decision: Include",
    "Decision: Exclude",
    "Decision: Unclear",
    "Exclusion reason",
    "Conflict resolution",
    "Protocol authority",
    "Missing abstract",
    "Audit trail",
    "Workbook generated",
    "Source dataset"
  ),

  Instruction = c(
    project_title,

    project_id,

    paste(
      "This workbook is used for independent title and abstract",
      "screening of records identified for this systematic review."
    ),

    paste(
      reviewer_1_name,
      "and",
      reviewer_2_name,
      "must screen records independently.",
      "Do not review or modify the other reviewer's decisions."
    ),

    reviewer_1_name,

    reviewer_2_name,

    paste(
      "Select Include when the title or abstract appears potentially",
      "eligible according to the review protocol."
    ),

    paste(
      "Select Exclude only when the record clearly fails an",
      "eligibility criterion at the title/abstract stage."
    ),

    paste(
      "Select Unclear when eligibility cannot be determined",
      "from the available title and abstract."
    ),

    paste(
      "When Exclude is selected, choose the single most appropriate",
      "exclusion reason. Detailed full-text reasons will be recorded later."
    ),

    paste(
      "The Conflict_Resolution sheet automatically compares decisions.",
      "Records marked Conflict require discussion or adjudication."
    ),

    paste(
      "The finalized protocol and predefined eligibility criteria",
      "take precedence over the generic categories in this workbook."
    ),

    paste(
      "Do not automatically exclude a record solely because the abstract",
      "is missing. Use title and available citation data and select Unclear",
      "when necessary."
    ),

    paste(
      "Do not delete records or rows. Enter decisions only in the designated",
      "screening columns."
    ),

    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    ),

    input_file
  )
)


# ============================================================
# 13. Create workbook
# ============================================================

workbook <- openxlsx::createWorkbook(
  creator = "SR-MA Agent Toolkit v2.0"
)

openxlsx::addWorksheet(
  workbook,
  "Instructions",
  gridLines = FALSE
)

openxlsx::addWorksheet(
  workbook,
  "Reviewer_1",
  gridLines = FALSE
)

openxlsx::addWorksheet(
  workbook,
  "Reviewer_2",
  gridLines = FALSE
)

openxlsx::addWorksheet(
  workbook,
  "Conflict_Resolution",
  gridLines = FALSE
)

openxlsx::addWorksheet(
  workbook,
  "Source_Summary",
  gridLines = FALSE
)

openxlsx::addWorksheet(
  workbook,
  "Validation_Lists",
  gridLines = FALSE,
  visible = FALSE
)


# ============================================================
# 14. Define workbook styles
# ============================================================

title_style <- openxlsx::createStyle(
  fontSize = 16,
  textDecoration = "bold",
  halign = "left",
  valign = "center"
)

subtitle_style <- openxlsx::createStyle(
  fontSize = 10,
  textDecoration = "italic",
  wrapText = TRUE,
  valign = "top"
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "TopBottomLeftRight"
)

body_style <- openxlsx::createStyle(
  valign = "top",
  wrapText = TRUE,
  border = "TopBottomLeftRight"
)

screening_input_style <- openxlsx::createStyle(
  valign = "top",
  wrapText = TRUE,
  border = "TopBottomLeftRight"
)

conflict_style <- openxlsx::createStyle(
  textDecoration = "bold"
)


# ============================================================
# 15. Write validation lists
# ============================================================

maximum_list_length <- max(
  length(decision_options),
  length(exclusion_reason_options),
  length(final_decision_options),
  length(confidence_options)
)


validation_lists <- tibble::tibble(
  decisions = pad_character_vector(
    decision_options,
    maximum_list_length
  ),

  exclusion_reasons = pad_character_vector(
    exclusion_reason_options,
    maximum_list_length
  ),

  final_decisions = pad_character_vector(
    final_decision_options,
    maximum_list_length
  ),

  confidence = pad_character_vector(
    confidence_options,
    maximum_list_length
  )
)


openxlsx::writeData(
  workbook,
  "Validation_Lists",
  validation_lists,
  withFilter = FALSE
)


decision_validation_range <- build_validation_range(
  "A",
  length(decision_options)
)

exclusion_validation_range <- build_validation_range(
  "B",
  length(exclusion_reason_options)
)

final_decision_validation_range <- build_validation_range(
  "C",
  length(final_decision_options)
)

confidence_validation_range <- build_validation_range(
  "D",
  length(confidence_options)
)


# ============================================================
# 16. Write Instructions sheet
# ============================================================

openxlsx::writeData(
  workbook,
  "Instructions",
  paste0(
    project_title,
    " — Title and Abstract Screening Workbook"
  ),
  startRow = 1,
  startCol = 1
)

openxlsx::mergeCells(
  workbook,
  "Instructions",
  cols = 1:2,
  rows = 1
)

openxlsx::addStyle(
  workbook,
  "Instructions",
  title_style,
  rows = 1,
  cols = 1:2,
  gridExpand = TRUE
)

openxlsx::writeData(
  workbook,
  "Instructions",
  instructions,
  startRow = 3,
  startCol = 1,
  headerStyle = header_style,
  withFilter = FALSE
)

openxlsx::addStyle(
  workbook,
  "Instructions",
  body_style,
  rows = 4:(
    nrow(instructions) + 3L
  ),
  cols = 1:2,
  gridExpand = TRUE
)

openxlsx::setColWidths(
  workbook,
  "Instructions",
  cols = 1,
  widths = 24
)

openxlsx::setColWidths(
  workbook,
  "Instructions",
  cols = 2,
  widths = 100
)

openxlsx::freezePane(
  workbook,
  "Instructions",
  firstActiveRow = 4
)


# ============================================================
# 17. Reviewer-sheet writer
# ============================================================

write_reviewer_sheet <- function(
    workbook,
    sheet_name,
    reviewer_data,
    reviewer_name
) {

  openxlsx::writeData(
    workbook,
    sheet_name,
    paste0(
      project_title,
      " — Independent Screening"
    ),
    startRow = 1,
    startCol = 1
  )

  openxlsx::mergeCells(
    workbook,
    sheet_name,
    cols = 1:ncol(reviewer_data),
    rows = 1
  )

  openxlsx::addStyle(
    workbook,
    sheet_name,
    title_style,
    rows = 1,
    cols = 1:ncol(reviewer_data),
    gridExpand = TRUE
  )

  openxlsx::writeData(
    workbook,
    sheet_name,
    paste0(
      "Reviewer: ",
      reviewer_name,
      ". Select Include, Exclude, or Unclear. ",
      "When Exclude is selected, record one exclusion reason."
    ),
    startRow = 2,
    startCol = 1
  )

  openxlsx::mergeCells(
    workbook,
    sheet_name,
    cols = 1:ncol(reviewer_data),
    rows = 2
  )

  openxlsx::addStyle(
    workbook,
    sheet_name,
    subtitle_style,
    rows = 2,
    cols = 1:ncol(reviewer_data),
    gridExpand = TRUE
  )

  openxlsx::writeData(
    workbook,
    sheet_name,
    reviewer_data,
    startRow = 4,
    startCol = 1,
    headerStyle = header_style,
    withFilter = TRUE,
    keepNA = FALSE
  )

  first_data_row <- 5L

  last_data_row <-
    nrow(reviewer_data) +
    4L

  openxlsx::addStyle(
    workbook,
    sheet_name,
    body_style,
    rows = first_data_row:last_data_row,
    cols = 1:ncol(reviewer_data),
    gridExpand = TRUE
  )

  decision_column <- which(
    names(reviewer_data) ==
      "decision"
  )

  exclusion_column <- which(
    names(reviewer_data) ==
      "exclusion_reason"
  )

  confidence_column <- which(
    names(reviewer_data) ==
      "confidence"
  )

  screening_entry_columns <- which(
    names(reviewer_data) %in%
      c(
        "decision",
        "exclusion_reason",
        "confidence",
        "notes",
        "screened_at"
      )
  )

  openxlsx::addStyle(
    workbook,
    sheet_name,
    screening_input_style,
    rows = first_data_row:last_data_row,
    cols = screening_entry_columns,
    gridExpand = TRUE
  )

  openxlsx::dataValidation(
    workbook,
    sheet_name,
    cols = decision_column,
    rows = first_data_row:last_data_row,
    type = "list",
    value = decision_validation_range
  )

  openxlsx::dataValidation(
    workbook,
    sheet_name,
    cols = exclusion_column,
    rows = first_data_row:last_data_row,
    type = "list",
    value = exclusion_validation_range
  )

  openxlsx::dataValidation(
    workbook,
    sheet_name,
    cols = confidence_column,
    rows = first_data_row:last_data_row,
    type = "list",
    value = confidence_validation_range
  )

  openxlsx::freezePane(
    workbook,
    sheet_name,
    firstActiveRow = 5,
    firstActiveCol = 3
  )

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = 1,
    widths = 14
  )

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = 2,
    widths = 20
  )

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = 3,
    widths = 65
  )

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = 4,
    widths = 100
  )

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = 5,
    widths = 32
  )

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = 6:12,
    widths = 16
  )

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = 13:18,
    widths = 22
  )

  openxlsx::setRowHeights(
    workbook,
    sheet_name,
    rows = first_data_row:last_data_row,
    heights = 70
  )

  decision_letter <- excel_column_name(
    decision_column
  )

  exclusion_letter <- excel_column_name(
    exclusion_column
  )

  openxlsx::conditionalFormatting(
    workbook,
    sheet_name,
    cols = decision_column,
    rows = first_data_row:last_data_row,
    rule = paste0(
      "$",
      decision_letter,
      first_data_row,
      '="Include"'
    ),
    style = openxlsx::createStyle(
      textDecoration = "bold"
    )
  )

  openxlsx::conditionalFormatting(
    workbook,
    sheet_name,
    cols = decision_column,
    rows = first_data_row:last_data_row,
    rule = paste0(
      "$",
      decision_letter,
      first_data_row,
      '="Exclude"'
    ),
    style = openxlsx::createStyle(
      textDecoration = "bold"
    )
  )

  openxlsx::conditionalFormatting(
    workbook,
    sheet_name,
    cols = exclusion_column,
    rows = first_data_row:last_data_row,
    rule = paste0(
      "AND($",
      decision_letter,
      first_data_row,
      '="Exclude",$',
      exclusion_letter,
      first_data_row,
      '="")'
    ),
    style = conflict_style
  )
}


write_reviewer_sheet(
  workbook,
  "Reviewer_1",
  reviewer_1_data,
  reviewer_1_name
)

write_reviewer_sheet(
  workbook,
  "Reviewer_2",
  reviewer_2_data,
  reviewer_2_name
)


# ============================================================
# 18. Write Conflict Resolution sheet
# ============================================================

openxlsx::writeData(
  workbook,
  "Conflict_Resolution",
  paste0(
    project_title,
    " — Reviewer Agreement and Conflict Resolution"
  ),
  startRow = 1,
  startCol = 1
)

openxlsx::mergeCells(
  workbook,
  "Conflict_Resolution",
  cols = 1:ncol(conflict_data),
  rows = 1
)

openxlsx::addStyle(
  workbook,
  "Conflict_Resolution",
  title_style,
  rows = 1,
  cols = 1:ncol(conflict_data),
  gridExpand = TRUE
)

openxlsx::writeData(
  workbook,
  "Conflict_Resolution",
  conflict_data,
  startRow = 3,
  startCol = 1,
  headerStyle = header_style,
  withFilter = TRUE
)

conflict_first_row <- 4L

conflict_last_row <-
  nrow(conflict_data) +
  3L

reviewer_1_decision_column <- which(
  names(reviewer_1_data) ==
    "decision"
)

reviewer_2_decision_column <- which(
  names(reviewer_2_data) ==
    "decision"
)

reviewer_1_decision_letter <- excel_column_name(
  reviewer_1_decision_column
)

reviewer_2_decision_letter <- excel_column_name(
  reviewer_2_decision_column
)


for (
  row_number in
  conflict_first_row:conflict_last_row
) {

  # Reviewer sheets start data on row 5.
  # Conflict sheet starts data on row 4.

  reviewer_row <-
    row_number +
    1L

  openxlsx::writeFormula(
    workbook,
    "Conflict_Resolution",
    x = paste0(
      "='Reviewer_1'!",
      reviewer_1_decision_letter,
      reviewer_row
    ),
    startCol = 5,
    startRow = row_number
  )

  openxlsx::writeFormula(
    workbook,
    "Conflict_Resolution",
    x = paste0(
      "='Reviewer_2'!",
      reviewer_2_decision_letter,
      reviewer_row
    ),
    startCol = 6,
    startRow = row_number
  )

  openxlsx::writeFormula(
    workbook,
    "Conflict_Resolution",
    x = paste0(
      '=IF(OR(E',
      row_number,
      '="",F',
      row_number,
      '=""),"Pending",',
      'IF(E',
      row_number,
      '=F',
      row_number,
      ',"Agree","Disagree"))'
    ),
    startCol = 7,
    startRow = row_number
  )

  openxlsx::writeFormula(
    workbook,
    "Conflict_Resolution",
    x = paste0(
      '=IF(G',
      row_number,
      '="Disagree","Conflict",',
      'IF(G',
      row_number,
      '="Agree","No conflict","Pending"))'
    ),
    startCol = 8,
    startRow = row_number
  )
}


openxlsx::dataValidation(
  workbook,
  "Conflict_Resolution",
  cols = 9,
  rows = conflict_first_row:conflict_last_row,
  type = "list",
  value = final_decision_validation_range
)

openxlsx::dataValidation(
  workbook,
  "Conflict_Resolution",
  cols = 10,
  rows = conflict_first_row:conflict_last_row,
  type = "list",
  value = exclusion_validation_range
)

openxlsx::addStyle(
  workbook,
  "Conflict_Resolution",
  body_style,
  rows = conflict_first_row:conflict_last_row,
  cols = 1:ncol(conflict_data),
  gridExpand = TRUE
)

openxlsx::freezePane(
  workbook,
  "Conflict_Resolution",
  firstActiveRow = 4,
  firstActiveCol = 3
)

openxlsx::setColWidths(
  workbook,
  "Conflict_Resolution",
  cols = 1,
  widths = 14
)

openxlsx::setColWidths(
  workbook,
  "Conflict_Resolution",
  cols = 2,
  widths = 70
)

openxlsx::setColWidths(
  workbook,
  "Conflict_Resolution",
  cols = 3:4,
  widths = 14
)

openxlsx::setColWidths(
  workbook,
  "Conflict_Resolution",
  cols = 5:13,
  widths = 24
)

openxlsx::conditionalFormatting(
  workbook,
  "Conflict_Resolution",
  cols = 8,
  rows = conflict_first_row:conflict_last_row,
  rule = paste0(
    "$H",
    conflict_first_row,
    '="Conflict"'
  ),
  style = conflict_style
)


# ============================================================
# 19. Write Source Summary sheet
# ============================================================

openxlsx::writeData(
  workbook,
  "Source_Summary",
  paste0(
    project_short_name,
    " — Search Source Summary"
  ),
  startRow = 1,
  startCol = 1
)

openxlsx::mergeCells(
  workbook,
  "Source_Summary",
  cols = 1:2,
  rows = 1
)

openxlsx::addStyle(
  workbook,
  "Source_Summary",
  title_style,
  rows = 1,
  cols = 1:2,
  gridExpand = TRUE
)

openxlsx::writeData(
  workbook,
  "Source_Summary",
  source_summary,
  startRow = 3,
  startCol = 1,
  headerStyle = header_style,
  withFilter = FALSE
)

openxlsx::addStyle(
  workbook,
  "Source_Summary",
  body_style,
  rows = 4:(
    nrow(source_summary) +
      3L
  ),
  cols = 1:2,
  gridExpand = TRUE
)

openxlsx::setColWidths(
  workbook,
  "Source_Summary",
  cols = 1,
  widths = 45
)

openxlsx::setColWidths(
  workbook,
  "Source_Summary",
  cols = 2,
  widths = 20
)


# ============================================================
# 20. Save workbook
# ============================================================

openxlsx::saveWorkbook(
  workbook,
  output_file,
  overwrite = TRUE
)


# ============================================================
# 21. Validate saved workbook
# ============================================================

if (!file.exists(output_file)) {

  stop(
    "The screening workbook was not created successfully:\n",
    output_file
  )
}


expected_sheets <- c(
  "Instructions",
  "Reviewer_1",
  "Reviewer_2",
  "Conflict_Resolution",
  "Source_Summary",
  "Validation_Lists"
)


workbook_sheets <- openxlsx::getSheetNames(
  output_file
)


missing_sheets <- setdiff(
  expected_sheets,
  workbook_sheets
)

if (length(missing_sheets) > 0L) {

  stop(
    "The workbook is missing the following sheets:\n- ",
    paste(
      missing_sheets,
      collapse = "\n- "
    )
  )
}


saved_reviewer_1 <- openxlsx::readWorkbook(
  output_file,
  sheet = "Reviewer_1",
  startRow = 4,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

saved_reviewer_2 <- openxlsx::readWorkbook(
  output_file,
  sheet = "Reviewer_2",
  startRow = 4,
  colNames = TRUE,
  skipEmptyRows = FALSE
)


if (
  nrow(saved_reviewer_1) !=
    nrow(screening_data)
) {

  stop(
    "Reviewer_1 sheet record count does not match ",
    "the deduplicated screening dataset.\n",
    "Expected: ",
    nrow(screening_data),
    "\nObserved: ",
    nrow(saved_reviewer_1)
  )
}


if (
  nrow(saved_reviewer_2) !=
    nrow(screening_data)
) {

  stop(
    "Reviewer_2 sheet record count does not match ",
    "the deduplicated screening dataset.\n",
    "Expected: ",
    nrow(screening_data),
    "\nObserved: ",
    nrow(saved_reviewer_2)
  )
}


# ============================================================
# 22. Save generation report
# ============================================================

report_lines <- c(
  "SR-MA Agent Toolkit v2.0",
  "Title and Abstract Screening Workbook Report",
  "",

  paste0(
    "Project ID: ",
    project_id
  ),

  paste0(
    "Review title: ",
    project_title
  ),

  paste0(
    "Project short name: ",
    project_short_name
  ),

  paste0(
    "Configuration file: ",
    attr(
      config,
      "config_file"
    )
  ),

  paste0(
    "Generated at: ",
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    )
  ),

  paste0(
    "Input file: ",
    input_file
  ),

  paste0(
    "Records included in workbook: ",
    nrow(screening_data)
  ),

  paste0(
    "Reviewer 1: ",
    reviewer_1_name
  ),

  paste0(
    "Reviewer 2: ",
    reviewer_2_name
  ),

  paste0(
    "Third reviewer: ",
    third_reviewer_name
  ),

  paste0(
    "Workbook file: ",
    output_file
  ),

  "",
  "VALIDATION",
  "--------------------------------------------------",

  paste0(
    "Workbook exists: ",
    file.exists(output_file)
  ),

  paste0(
    "All expected worksheets present: ",
    length(missing_sheets) == 0L
  ),

  paste0(
    "Reviewer_1 row count valid: ",
    nrow(saved_reviewer_1) ==
      nrow(screening_data)
  ),

  paste0(
    "Reviewer_2 row count valid: ",
    nrow(saved_reviewer_2) ==
      nrow(screening_data)
  ),

  "",
  "WORKSHEETS",
  "--------------------------------------------------",

  expected_sheets,

  "",
  "DECISION OPTIONS",
  "--------------------------------------------------",

  decision_options,

  "",
  "EXCLUSION REASONS",
  "--------------------------------------------------",

  paste0(
    exclusion_reason_table$code,
    ": ",
    exclusion_reason_table$label
  ),

  "",
  "FINAL DECISION OPTIONS",
  "--------------------------------------------------",

  final_decision_options,

  "",
  "CONFIDENCE OPTIONS",
  "--------------------------------------------------",

  confidence_options
)


writeLines(
  report_lines,
  con = report_file,
  useBytes = TRUE
)


# ============================================================
# 23. Console output
# ============================================================

cat("\n")
cat("====================================================\n")
cat("Screening Workbook Created Successfully\n")
cat("====================================================\n\n")

cat("Project ID:\n")
cat(project_id, "\n\n")

cat("Review title:\n")
cat(project_title, "\n\n")

cat("Reviewer 1:\n")
cat(reviewer_1_name, "\n\n")

cat("Reviewer 2:\n")
cat(reviewer_2_name, "\n\n")

cat(
  "Records prepared for screening:",
  nrow(screening_data),
  "\n\n"
)

cat("Workbook saved:\n")
cat("-", output_file, "\n\n")

cat("Report saved:\n")
cat("-", report_file, "\n\n")

cat("Workbook sheets:\n")

for (sheet_name in expected_sheets) {

  cat(
    "-",
    sheet_name,
    "\n"
  )
}

cat(
  "\nNext step:",
  "\n1. Open the regenerated screening workbook.",
  "\n2. Confirm the project title and reviewer names.",
  "\n3. Confirm the decision dropdowns.",
  "\n4. Confirm the exclusion-reason dropdowns.",
  "\n5. Confirm the number of records matches R/17 output.",
  "\n6. Begin independent title and abstract screening.",
  "\n"
)