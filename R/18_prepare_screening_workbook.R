# ============================================================
# Cardiac Surgery SR-MA Toolkit v2.0
# R/18_prepare_screening_workbook.R
#
# Purpose
# ------------------------------------------------------------
# 1. Import deduplicated records prepared by R/17
# 2. Generate independent title/abstract screening sheets
# 3. Add standardized decision and exclusion-reason lists
# 4. Compare Reviewer 1 and Reviewer 2 decisions
# 5. Create a conflict-resolution sheet
# 6. Save a reproducible Excel screening workbook
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

if (length(missing_packages) > 0) {

  stop(
    "The following packages are required but not installed: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them with:\n",
    "install.packages(c(",
    paste(
      paste0('"', missing_packages, '"'),
      collapse = ", "
    ),
    "))"
  )
}


# ============================================================
# 2. File paths
# ============================================================

input_file <-
  "data/processed/search/deduplicated_records_for_screening.csv"

output_directory <-
  "results/screening"

output_file <- file.path(
  output_directory,
  "title_abstract_screening_workbook.xlsx"
)

report_file <-
  "results/logs/screening_workbook_report.txt"


# ============================================================
# 3. Preliminary checks
# ============================================================

if (!file.exists(input_file)) {

  stop(
    "The deduplicated screening dataset was not found:\n",
    input_file,
    "\nRun R/17_import_merge_deduplicate.R first."
  )
}

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "results/logs",
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 4. Helper functions
# ============================================================

first_existing_column <- function(
    data,
    candidates,
    default = NA_character_
) {

  matching_columns <- candidates[
    candidates %in% names(data)
  ]

  if (length(matching_columns) == 0) {

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

  column_name <- ""

  while (column_number > 0) {

    remainder <- (
      column_number - 1
    ) %% 26

    column_name <- paste0(
      LETTERS[
        remainder + 1
      ],
      column_name
    )

    column_number <- (
      column_number -
        remainder -
        1
    ) %/% 26
  }

  column_name
}


# ============================================================
# 5. Import deduplicated records
# ============================================================

screening_source <- readr::read_csv(
  input_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
)

if (nrow(screening_source) == 0) {

  stop(
    "The deduplicated screening dataset contains no records."
  )
}


# ============================================================
# 6. Create standardized screening dataset
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
      c(
        "abstract"
      )
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
      c(
        "doi"
      )
    )
  ),

  pmid = clean_character(
    first_existing_column(
      screening_source,
      c(
        "pmid"
      )
    )
  ),

  publication_type = clean_character(
    first_existing_column(
      screening_source,
      c(
        "publication_type"
      )
    )
  ),

  language = clean_character(
    first_existing_column(
      screening_source,
      c(
        "language"
      )
    )
  ),

  url = clean_character(
    first_existing_column(
      screening_source,
      c(
        "url"
      )
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


# ============================================================
# 7. Define screening choices
# ============================================================

decision_options <- c(
  "Include",
  "Exclude",
  "Unclear"
)

exclusion_reason_options <- c(
  "Not cardiac surgery",
  "No SGLT2 inhibitor exposure",
  "Wrong population",
  "Wrong comparator",
  "Wrong study design",
  "No relevant clinical outcome",
  "Animal or laboratory study",
  "Protocol, editorial, commentary, or review",
  "Duplicate or secondary publication",
  "Conference abstract with insufficient information",
  "Other"
)

final_decision_options <- c(
  "Include",
  "Exclude",
  "Awaiting full-text clarification"
)


# ============================================================
# 8. Create reviewer sheets
# ============================================================

create_reviewer_data <- function(
    data,
    reviewer_name
) {

  data |>
    dplyr::mutate(
      reviewer = reviewer_name,
      decision = NA_character_,
      exclusion_reason = NA_character_,
      confidence = NA_character_,
      notes = NA_character_,
      screened_at = NA_character_
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
  "Reviewer 1"
)

reviewer_2_data <- create_reviewer_data(
  screening_data,
  "Reviewer 2"
)


# ============================================================
# 9. Create conflict-resolution data
# ============================================================

conflict_data <- screening_data |>
  dplyr::transmute(
    screening_id,
    title,
    year,
    journal,
    reviewer_1_decision = NA_character_,
    reviewer_2_decision = NA_character_,
    agreement_status = NA_character_,
    conflict_status = NA_character_,
    final_decision = NA_character_,
    final_exclusion_reason = NA_character_,
    adjudicator = NA_character_,
    resolution_notes = NA_character_,
    resolved_at = NA_character_
  )


# ============================================================
# 10. Create source summary
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
    dplyr::desc(record_count)
  )

source_summary <- dplyr::bind_rows(
  source_summary,
  tibble::tibble(
    source_databases = "Total unique records",
    record_count = nrow(
      screening_data
    )
  )
)


# ============================================================
# 11. Create instruction content
# ============================================================

instructions <- tibble::tibble(
  Section = c(
    "Purpose",
    "Independent review",
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
    paste(
      "This workbook is used for independent title and abstract",
      "screening of records identified for the systematic review."
    ),

    paste(
      "Reviewer 1 and Reviewer 2 must screen records independently.",
      "Do not review or modify the other reviewer's decisions."
    ),

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
      "Records marked Conflict require adjudication."
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
# 12. Create workbook
# ============================================================

workbook <- openxlsx::createWorkbook(
  creator = "Cardiac Surgery SR-MA Toolkit v2.0"
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
# 13. Define workbook styles
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

center_style <- openxlsx::createStyle(
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "TopBottomLeftRight"
)

conflict_style <- openxlsx::createStyle(
  textDecoration = "bold"
)


# ============================================================
# 14. Write validation lists
# ============================================================

maximum_list_length <- max(
  length(decision_options),
  length(exclusion_reason_options),
  length(final_decision_options),
  3L
)

pad_character_vector <- function(
    values,
    target_length
) {

  values <- as.character(values)

  padding_length <- target_length - length(values)

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
    c(
      "High",
      "Moderate",
      "Low"
    ),
    maximum_list_length
  )
)

openxlsx::writeData(
  workbook,
  "Validation_Lists",
  validation_lists,
  withFilter = FALSE
)

# ============================================================
# 15. Write Instructions sheet
# ============================================================

openxlsx::writeData(
  workbook,
  "Instructions",
  "Title and Abstract Screening Workbook",
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
    nrow(instructions) + 3
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
# 16. Reviewer-sheet writer
# ============================================================

write_reviewer_sheet <- function(
    workbook,
    sheet_name,
    reviewer_data,
    reviewer_number
) {

  openxlsx::writeData(
    workbook,
    sheet_name,
    paste0(
      "Independent Title and Abstract Screening — Reviewer ",
      reviewer_number
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
      "Select Include, Exclude, or Unclear. ",
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

  first_data_row <- 5
  last_data_row <- nrow(
    reviewer_data
  ) + 4

  openxlsx::addStyle(
    workbook,
    sheet_name,
    body_style,
    rows = first_data_row:last_data_row,
    cols = 1:ncol(reviewer_data),
    gridExpand = TRUE
  )

  decision_column <- which(
    names(reviewer_data) == "decision"
  )

  exclusion_column <- which(
    names(reviewer_data) == "exclusion_reason"
  )

  confidence_column <- which(
    names(reviewer_data) == "confidence"
  )

  openxlsx::addStyle(
    workbook,
    sheet_name,
    screening_input_style,
    rows = first_data_row:last_data_row,
    cols = c(
      decision_column,
      exclusion_column,
      confidence_column,
      which(
        names(reviewer_data) %in%
          c(
            "notes",
            "screened_at"
          )
      )
    ),
    gridExpand = TRUE
  )

  openxlsx::dataValidation(
    workbook,
    sheet_name,
    cols = decision_column,
    rows = first_data_row:last_data_row,
    type = "list",
    value = "'Validation_Lists'!$A$2:$A$4"
  )

  openxlsx::dataValidation(
    workbook,
    sheet_name,
    cols = exclusion_column,
    rows = first_data_row:last_data_row,
    type = "list",
    value = paste0(
      "'Validation_Lists'!$B$2:$B$",
      length(exclusion_reason_options) + 1
    )
  )

  openxlsx::dataValidation(
    workbook,
    sheet_name,
    cols = confidence_column,
    rows = first_data_row:last_data_row,
    type = "list",
    value = "'Validation_Lists'!$D$2:$D$4"
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
      '$',
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
      '$',
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
      'AND($',
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
  1
)

write_reviewer_sheet(
  workbook,
  "Reviewer_2",
  reviewer_2_data,
  2
)


# ============================================================
# 17. Write Conflict Resolution sheet
# ============================================================

openxlsx::writeData(
  workbook,
  "Conflict_Resolution",
  "Reviewer Agreement and Conflict Resolution",
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

conflict_first_row <- 4
conflict_last_row <- nrow(
  conflict_data
) + 3

reviewer_1_decision_column <- which(
  names(reviewer_1_data) == "decision"
)

reviewer_2_decision_column <- which(
  names(reviewer_2_data) == "decision"
)

reviewer_1_decision_letter <- excel_column_name(
  reviewer_1_decision_column
)

reviewer_2_decision_letter <- excel_column_name(
  reviewer_2_decision_column
)

for (row_number in conflict_first_row:conflict_last_row) {

  reviewer_row <- row_number + 1

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
  value = "'Validation_Lists'!$C$2:$C$4"
)

openxlsx::dataValidation(
  workbook,
  "Conflict_Resolution",
  cols = 10,
  rows = conflict_first_row:conflict_last_row,
  type = "list",
  value = paste0(
    "'Validation_Lists'!$B$2:$B$",
    length(exclusion_reason_options) + 1
  )
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
    '$H',
    conflict_first_row,
    '="Conflict"'
  ),
  style = conflict_style
)


# ============================================================
# 18. Write Source Summary sheet
# ============================================================

openxlsx::writeData(
  workbook,
  "Source_Summary",
  "Search Source Summary",
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
    nrow(source_summary) + 3
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
# 19. Workbook properties and save
# ============================================================


openxlsx::saveWorkbook(
  workbook,
  output_file,
  overwrite = TRUE
)


# ============================================================
# 20. Validate saved workbook
# ============================================================

if (!file.exists(output_file)) {

  stop(
    "The screening workbook was not created successfully."
  )
}

workbook_sheets <- openxlsx::getSheetNames(
  output_file
)

expected_sheets <- c(
  "Instructions",
  "Reviewer_1",
  "Reviewer_2",
  "Conflict_Resolution",
  "Source_Summary",
  "Validation_Lists"
)

missing_sheets <- setdiff(
  expected_sheets,
  workbook_sheets
)

if (length(missing_sheets) > 0) {

  stop(
    "The workbook is missing the following sheets: ",
    paste(
      missing_sheets,
      collapse = ", "
    )
  )
}


# ============================================================
# 21. Save generation report
# ============================================================

report_lines <- c(
  "Cardiac Surgery SR-MA Toolkit v2.0",
  "Title and Abstract Screening Workbook Report",
  "",
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
    "Workbook file: ",
    output_file
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
  exclusion_reason_options
)

writeLines(
  report_lines,
  con = report_file,
  useBytes = TRUE
)


# ============================================================
# 22. Console output
# ============================================================

cat("\n")
cat("====================================================\n")
cat("Screening Workbook Created Successfully\n")
cat("====================================================\n\n")

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
  "\n1. Open the screening workbook.",
  "\n2. Confirm the title and abstract display correctly.",
  "\n3. Enter the names of Reviewer 1 and Reviewer 2 if needed.",
  "\n4. Conduct independent title and abstract screening.",
  "\n5. Review disagreements in Conflict_Resolution.",
  "\n"
)