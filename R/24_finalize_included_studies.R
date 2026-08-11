# ============================================================
# Universal SR-MA Agent Toolkit
# R/24_finalize_included_studies.R
#
# TASK-012 — Final Included Studies
#
# Purpose
# ------------------------------------------------------------
# 1. Read the completed 2-stage full-text eligibility workbook.
# 2. Treat 1st Author final decisions as authoritative.
# 3. Validate final Include/Exclude decisions and exclusion reasons.
# 4. Create authoritative Included Studies and exclusion audit datasets.
# 5. Preserve the original human-reviewed workbook unchanged.
# 6. Create a finalized workbook copy with Included_Studies,
#    Excluded_After_Full_Text, and Final_Eligibility_Summary sheets.
#
# Stage boundary
# ------------------------------------------------------------
# - Agent results are advisory provenance only.
# - 1st Author final decisions are authoritative.
# - No second reviewer or adjudication stage is used.
# - R/24 does not rerun R/23.
# - Runner integration is out of scope.
# ============================================================

required_packages <- c(
  "readr", "dplyr", "tibble", "stringr", "openxlsx"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_squish(x)
  x[is.na(x) | x == ""] <- NA_character_
  x
}

now_text <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

source("R/utils/config_utils.R")
source("R/utils/path_utils.R")

config <- load_project_config(
  "config/project_config.yml",
  validate = TRUE,
  quiet = FALSE
)

paths <- build_project_paths(
  config = config,
  create_directories = TRUE,
  validate = TRUE
)

project_id <- get_project_id(config)
project_title <- get_project_title(config)
exclusion_reason_table <- get_exclusion_reasons(config)

if (is.null(exclusion_reason_table) || nrow(exclusion_reason_table) == 0L) {
  stop(
    "No canonical exclusion reasons were found in project_config.yml.",
    call. = FALSE
  )
}

full_text_results_directory <- file.path(
  paths$project_root, "results", "full_text"
)

processed_full_text_directory <- file.path(
  paths$project_root, "data", "processed", "full_text"
)

input_workbook <- file.path(
  full_text_results_directory,
  "full_text_eligibility_agent.xlsx"
)

finalized_workbook <- file.path(
  full_text_results_directory,
  "full_text_eligibility_final.xlsx"
)

included_studies_csv <- file.path(
  processed_full_text_directory,
  "included_studies.csv"
)

excluded_studies_csv <- file.path(
  processed_full_text_directory,
  "excluded_after_full_text.csv"
)

final_decisions_csv <- file.path(
  processed_full_text_directory,
  "final_full_text_eligibility.csv"
)

report_file <- file.path(
  paths$logs_directory,
  "included_studies_finalization_report.txt"
)

ensure_directories(
  c(
    full_text_results_directory,
    processed_full_text_directory,
    paths$logs_directory
  )
)

assert_file_exists(
  input_workbook,
  "Completed full-text eligibility workbook"
)

sheet_names <- openxlsx::getSheetNames(input_workbook)

required_sheets <- c(
  "Instructions",
  "Agent_Review",
  "Author_Final_Review",
  "Validation_Lists"
)

missing_sheets <- setdiff(required_sheets, sheet_names)

if (length(missing_sheets) > 0L) {
  stop(
    "The full-text eligibility workbook is missing required sheet(s):\n- ",
    paste(missing_sheets, collapse = "\n- "),
    call. = FALSE
  )
}

author_review <- openxlsx::readWorkbook(
  input_workbook,
  sheet = "Author_Final_Review",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

required_author_columns <- c(
  "screening_id",
  "title",
  "authors",
  "year",
  "journal",
  "doi",
  "pmid",
  "publication_type",
  "language",
  "url",
  "pdf_status",
  "pdf_path",
  "agent_decision",
  "agent_exclusion_reason",
  "agent_confidence",
  "agent_evidence_source",
  "agent_eligibility_evidence",
  "agent_rationale",
  "author_reviewer",
  "author_final_decision",
  "author_final_exclusion_reason",
  "author_notes",
  "author_reviewed_at"
)

missing_author_columns <- setdiff(
  required_author_columns,
  names(author_review)
)

if (length(missing_author_columns) > 0L) {
  stop(
    "Author_Final_Review is missing required column(s):\n- ",
    paste(missing_author_columns, collapse = "\n- "),
    call. = FALSE
  )
}

author_review$screening_id <- clean_text(author_review$screening_id)
author_review$author_final_decision <- clean_text(author_review$author_final_decision)
author_review$author_final_exclusion_reason <- clean_text(
  author_review$author_final_exclusion_reason
)
author_review$author_notes <- clean_text(author_review$author_notes)
author_review$author_reviewed_at <- clean_text(author_review$author_reviewed_at)

validation_errors <- character(0)

if (nrow(author_review) == 0L) {
  validation_errors <- c(
    validation_errors,
    "Author_Final_Review contains zero records."
  )
}

if (any(is.na(author_review$screening_id))) {
  validation_errors <- c(
    validation_errors,
    "Missing screening_id values are present."
  )
}

if (anyDuplicated(author_review$screening_id) > 0L) {
  validation_errors <- c(
    validation_errors,
    "Duplicated screening_id values are present."
  )
}

allowed_author_decisions <- c("Include", "Exclude")

invalid_decision_rows <- which(
  is.na(author_review$author_final_decision) |
    !author_review$author_final_decision %in% allowed_author_decisions
)

if (length(invalid_decision_rows) > 0L) {
  validation_errors <- c(
    validation_errors,
    paste0(
      length(invalid_decision_rows),
      " record(s) have missing or invalid author_final_decision."
    )
  )
}

exclude_rows <- which(
  author_review$author_final_decision == "Exclude"
)

if (length(exclude_rows) > 0L) {
  exclude_reasons <- author_review$author_final_exclusion_reason[
    exclude_rows
  ]

  missing_reason_rows <- exclude_rows[
    is.na(exclude_reasons)
  ]

  if (length(missing_reason_rows) > 0L) {
    validation_errors <- c(
      validation_errors,
      paste0(
        length(missing_reason_rows),
        " Exclude record(s) are missing author_final_exclusion_reason."
      )
    )
  }

  supplied_reasons <- exclude_reasons[!is.na(exclude_reasons)]

  invalid_reasons <- setdiff(
    unique(supplied_reasons),
    exclusion_reason_table$label
  )

  if (length(invalid_reasons) > 0L) {
    validation_errors <- c(
      validation_errors,
      paste0(
        "Invalid canonical exclusion reason(s): ",
        paste(invalid_reasons, collapse = " | ")
      )
    )
  }
}

include_rows <- which(
  author_review$author_final_decision == "Include"
)

if (length(include_rows) > 0L) {
  include_reason_rows <- include_rows[
    !is.na(
      author_review$author_final_exclusion_reason[
        include_rows
      ]
    )
  ]

  if (length(include_reason_rows) > 0L) {
    validation_errors <- c(
      validation_errors,
      paste0(
        length(include_reason_rows),
        " Include record(s) have a nonblank exclusion reason."
      )
    )
  }
}

if (length(validation_errors) > 0L) {
  stop(
    "TASK-012 author final-review validation failed:\n- ",
    paste(validation_errors, collapse = "\n- "),
    call. = FALSE
  )
}

final_eligibility <- author_review |>
  dplyr::mutate(
    final_decision = author_final_decision,
    final_exclusion_reason = author_final_exclusion_reason,
    final_decision_source = "1st Author Final Review"
  ) |>
  dplyr::select(
    screening_id,
    title,
    authors,
    year,
    journal,
    doi,
    pmid,
    publication_type,
    language,
    url,
    pdf_status,
    pdf_path,
    agent_decision,
    agent_exclusion_reason,
    agent_confidence,
    agent_evidence_source,
    agent_eligibility_evidence,
    agent_rationale,
    author_reviewer,
    author_final_decision,
    author_final_exclusion_reason,
    author_notes,
    author_reviewed_at,
    final_decision,
    final_exclusion_reason,
    final_decision_source
  )

included_studies <- final_eligibility |>
  dplyr::filter(final_decision == "Include") |>
  dplyr::select(
    screening_id,
    title,
    authors,
    year,
    journal,
    doi,
    pmid,
    publication_type,
    language,
    url,
    pdf_status,
    pdf_path,
    agent_decision,
    agent_confidence,
    agent_evidence_source,
    agent_eligibility_evidence,
    agent_rationale,
    author_reviewer,
    author_notes,
    author_reviewed_at,
    final_decision,
    final_decision_source
  )

excluded_studies <- final_eligibility |>
  dplyr::filter(final_decision == "Exclude") |>
  dplyr::select(
    screening_id,
    title,
    authors,
    year,
    journal,
    doi,
    pmid,
    publication_type,
    language,
    url,
    pdf_status,
    pdf_path,
    agent_decision,
    agent_exclusion_reason,
    agent_confidence,
    agent_evidence_source,
    agent_eligibility_evidence,
    agent_rationale,
    author_reviewer,
    author_final_exclusion_reason,
    author_notes,
    author_reviewed_at,
    final_decision,
    final_exclusion_reason,
    final_decision_source
  )

if (
  nrow(included_studies) +
    nrow(excluded_studies) !=
    nrow(final_eligibility)
) {
  stop(
    "Final eligibility partition does not equal total author-reviewed records.",
    call. = FALSE
  )
}

if (
  nrow(included_studies) > 0L &&
  any(included_studies$final_decision != "Include")
) {
  stop(
    "Included Studies contains a non-Include final decision.",
    call. = FALSE
  )
}

if (
  nrow(excluded_studies) > 0L &&
  any(is.na(excluded_studies$final_exclusion_reason))
) {
  stop(
    "Excluded-study audit contains missing final exclusion reasons.",
    call. = FALSE
  )
}

readr::write_csv(
  final_eligibility,
  final_decisions_csv,
  na = ""
)

readr::write_csv(
  included_studies,
  included_studies_csv,
  na = ""
)

readr::write_csv(
  excluded_studies,
  excluded_studies_csv,
  na = ""
)

required_csv_outputs <- c(
  final_decisions_csv,
  included_studies_csv,
  excluded_studies_csv
)

for (file_path in required_csv_outputs) {
  if (!file.exists(file_path) || is.na(file.info(file_path)$size)) {
    stop(
      "Required CSV output was not created: ",
      file_path,
      call. = FALSE
    )
  }
}

copied <- file.copy(
  input_workbook,
  finalized_workbook,
  overwrite = TRUE
)

if (!isTRUE(copied)) {
  stop(
    "Failed to create finalized full-text workbook copy.",
    call. = FALSE
  )
}

wb <- openxlsx::loadWorkbook(finalized_workbook)

title_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fontSize = 14,
  valign = "center"
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "Bottom"
)

wrap_style <- openxlsx::createStyle(
  valign = "top",
  wrapText = TRUE
)

write_final_sheet <- function(
    workbook,
    sheet_name,
    title,
    data,
    wide_columns = character(0)
) {
  if (sheet_name %in% names(workbook)) {
    openxlsx::removeWorksheet(workbook, sheet_name)
  }

  openxlsx::addWorksheet(workbook, sheet_name)

  openxlsx::writeData(
    workbook,
    sheet_name,
    title,
    startRow = 1,
    startCol = 1
  )

  if (ncol(data) > 1L) {
    openxlsx::mergeCells(
      workbook,
      sheet_name,
      cols = 1:ncol(data),
      rows = 1
    )
  }

  openxlsx::addStyle(
    workbook,
    sheet_name,
    title_style,
    rows = 1,
    cols = 1
  )

  openxlsx::writeData(
    workbook,
    sheet_name,
    data,
    startRow = 3,
    startCol = 1,
    colNames = TRUE,
    rowNames = FALSE,
    withFilter = TRUE,
    keepNA = FALSE
  )

  openxlsx::addStyle(
    workbook,
    sheet_name,
    header_style,
    rows = 3,
    cols = seq_len(ncol(data)),
    gridExpand = TRUE
  )

  if (nrow(data) > 0L) {
    openxlsx::addStyle(
      workbook,
      sheet_name,
      wrap_style,
      rows = 4:(nrow(data) + 3),
      cols = seq_len(ncol(data)),
      gridExpand = TRUE
    )
  }

  openxlsx::freezePane(
    workbook,
    sheet_name,
    firstActiveRow = 4,
    firstActiveCol = 2
  )

  openxlsx::setColWidths(
    workbook,
    sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )

  if ("title" %in% names(data)) {
    openxlsx::setColWidths(
      workbook,
      sheet_name,
      cols = which(names(data) == "title"),
      widths = 55
    )
  }

  for (
    wide_col in intersect(
      wide_columns,
      names(data)
    )
  ) {
    openxlsx::setColWidths(
      workbook,
      sheet_name,
      cols = which(names(data) == wide_col),
      widths = 50
    )
  }

  invisible(TRUE)
}

write_final_sheet(
  wb,
  "Included_Studies",
  paste0(
    project_title,
    " — Final Included Studies"
  ),
  included_studies,
  wide_columns = c(
    "pdf_path",
    "agent_eligibility_evidence",
    "agent_rationale",
    "author_notes"
  )
)

write_final_sheet(
  wb,
  "Excluded_After_Full_Text",
  paste0(
    project_title,
    " — Excluded After Full-Text Review"
  ),
  excluded_studies,
  wide_columns = c(
    "pdf_path",
    "agent_eligibility_evidence",
    "agent_rationale",
    "author_notes"
  )
)

summary_sheet_name <- "Final_Eligibility_Summary"

if (summary_sheet_name %in% names(wb)) {
  openxlsx::removeWorksheet(wb, summary_sheet_name)
}

openxlsx::addWorksheet(wb, summary_sheet_name)

summary_table <- tibble::tibble(
  Metric = c(
    "Project ID",
    "Total full-text candidate records",
    "Final Included Studies",
    "Excluded after full-text review",
    "Author final decisions complete",
    "Authoritative decision source",
    "Finalized at"
  ),
  Value = c(
    project_id,
    as.character(nrow(final_eligibility)),
    as.character(nrow(included_studies)),
    as.character(nrow(excluded_studies)),
    "YES",
    "1st Author Final Review",
    now_text()
  )
)

openxlsx::writeData(
  wb,
  summary_sheet_name,
  paste0(
    project_title,
    " — Final Full-Text Eligibility Summary"
  ),
  startRow = 1,
  startCol = 1
)

openxlsx::mergeCells(
  wb,
  summary_sheet_name,
  cols = 1:2,
  rows = 1
)

openxlsx::addStyle(
  wb,
  summary_sheet_name,
  title_style,
  rows = 1,
  cols = 1
)

openxlsx::writeData(
  wb,
  summary_sheet_name,
  summary_table,
  startRow = 3,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE
)

openxlsx::addStyle(
  wb,
  summary_sheet_name,
  header_style,
  rows = 3,
  cols = 1:2,
  gridExpand = TRUE
)

openxlsx::setColWidths(
  wb,
  summary_sheet_name,
  cols = 1,
  widths = 45
)

openxlsx::setColWidths(
  wb,
  summary_sheet_name,
  cols = 2,
  widths = 55
)

openxlsx::saveWorkbook(
  wb,
  finalized_workbook,
  overwrite = TRUE
)

if (
  !file.exists(finalized_workbook) ||
  is.na(file.info(finalized_workbook)$size) ||
  file.info(finalized_workbook)$size <= 0
) {
  stop(
    "Finalized full-text eligibility workbook was not created successfully.",
    call. = FALSE
  )
}

final_sheet_names <- openxlsx::getSheetNames(
  finalized_workbook
)

required_final_sheets <- c(
  "Instructions",
  "Agent_Review",
  "Author_Final_Review",
  "Validation_Lists",
  "Included_Studies",
  "Excluded_After_Full_Text",
  "Final_Eligibility_Summary"
)

missing_final_sheets <- setdiff(
  required_final_sheets,
  final_sheet_names
)

if (length(missing_final_sheets) > 0L) {
  stop(
    "Finalized workbook is missing required sheet(s):\n- ",
    paste(missing_final_sheets, collapse = "\n- "),
    call. = FALSE
  )
}

saved_included <- openxlsx::readWorkbook(
  finalized_workbook,
  sheet = "Included_Studies",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

if (nrow(saved_included) != nrow(included_studies)) {
  stop(
    "Saved Included_Studies row count does not match authoritative included dataset.",
    call. = FALSE
  )
}

if (
  nrow(included_studies) > 0L &&
  !identical(
    as.character(saved_included$screening_id),
    as.character(included_studies$screening_id)
  )
) {
  stop(
    "Saved Included_Studies screening IDs do not match authoritative included dataset.",
    call. = FALSE
  )
}

saved_excluded <- openxlsx::readWorkbook(
  finalized_workbook,
  sheet = "Excluded_After_Full_Text",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

if (nrow(saved_excluded) != nrow(excluded_studies)) {
  stop(
    "Saved Excluded_After_Full_Text row count does not match authoritative excluded dataset.",
    call. = FALSE
  )
}

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "TASK-012 — Final Included Studies Report",
  "",
  paste0("Generated at: ", now_text()),
  paste0("Project ID: ", project_id),
  paste0("Project title: ", project_title),
  "",
  "AUTHORITATIVE SOURCE",
  "--------------------------------------------------",
  paste0("Input workbook: ", input_workbook),
  "Authoritative decision: 1st Author Final Review",
  "Second reviewer/adjudication used: NO",
  "",
  "FINAL COUNTS",
  "--------------------------------------------------",
  paste0(
    "Total full-text candidate records: ",
    nrow(final_eligibility)
  ),
  paste0(
    "Final Included Studies: ",
    nrow(included_studies)
  ),
  paste0(
    "Excluded after full-text review: ",
    nrow(excluded_studies)
  ),
  "",
  "VALIDATION",
  "--------------------------------------------------",
  "All author_final_decision values complete: PASS",
  "Allowed author decision vocabulary: PASS",
  "Exclude reason required and canonical: PASS",
  "Include reason blank: PASS",
  "Final partition consistency: PASS",
  "Included_Studies workbook validation: PASS",
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0("Finalized workbook: ", finalized_workbook),
  paste0("Included Studies CSV: ", included_studies_csv),
  paste0("Excluded after full-text CSV: ", excluded_studies_csv),
  paste0("Final eligibility CSV: ", final_decisions_csv),
  paste0("Report: ", report_file),
  "",
  "STAGE BOUNDARY",
  "--------------------------------------------------",
  "Authoritative Included Studies created: YES",
  "Runner integration performed: NO",
  "",
  "TASK STATUS",
  "--------------------------------------------------",
  "TASK-012: PASS"
)

writeLines(
  report_lines,
  con = report_file,
  useBytes = TRUE
)

if (
  !file.exists(report_file) ||
  is.na(file.info(report_file)$size) ||
  file.info(report_file)$size <= 0
) {
  stop(
    "TASK-012 report was not created successfully.",
    call. = FALSE
  )
}

cat("\n")
cat("============================================================\n")
cat("TASK-012 — Final Included Studies\n")
cat("============================================================\n\n")

cat(
  "Total full-text candidate records: ",
  nrow(final_eligibility),
  "\n",
  sep = ""
)

cat(
  "Final Included Studies: ",
  nrow(included_studies),
  "\n",
  sep = ""
)

cat(
  "Excluded after full-text review: ",
  nrow(excluded_studies),
  "\n\n",
  sep = ""
)

cat(
  "Finalized workbook:\n- ",
  finalized_workbook,
  "\n",
  sep = ""
)

cat(
  "Included Studies CSV:\n- ",
  included_studies_csv,
  "\n",
  sep = ""
)

cat(
  "Report:\n- ",
  report_file,
  "\n\n",
  sep = ""
)

cat(
  "PASS: TASK-012 final Included Studies dataset created successfully.\n"
)

cat(
  "The 1st Author final decisions are authoritative.\n"
)

cat(
  "Next PM-gated stage: TASK-013 — Multimodal Data Extraction.\n"
)

invisible(
  list(
    status = "PASS",
    final_eligibility = final_eligibility,
    included_studies = included_studies,
    excluded_studies = excluded_studies,
    finalized_workbook = finalized_workbook,
    included_studies_csv = included_studies_csv,
    excluded_studies_csv = excluded_studies_csv,
    final_decisions_csv = final_decisions_csv,
    report_file = report_file
  )
)
