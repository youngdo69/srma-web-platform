# ============================================================
# Universal SR-MA Agent Toolkit
# R/21_finalize_title_abstract_screening.R
#
# Purpose
# ------------------------------------------------------------
# 1. Read the completed Agent Reviewer 1 + Human Reviewer 2
#    screening workbook produced through R/18-R/20 workflow.
# 2. Recompute reviewer agreement/conflict deterministically
#    from Reviewer_1 and Reviewer_2 sheets.
# 3. Automatically inherit agreement decisions.
# 4. Require human adjudicator input only for true conflicts.
# 5. Validate exclusion reasons and resolve final decisions.
# 6. Rebuild and validate Source_Summary from record provenance.
# 7. Save an authoritative completed screening workbook,
#    final screening CSV, and finalization report.
#
# Design boundary
# ------------------------------------------------------------
# - R/20 remains the agent-stage validator.
# - R/21 is the final title/abstract screening finalizer/validator.
# - The original agent workbook is never overwritten.
# - Excel formula values are NOT treated as authoritative.
#   Agreement/conflict status is recomputed in R.
# ============================================================

required_packages <- c(
  "yaml", "openxlsx", "readr", "dplyr", "tibble", "stringr"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required packages: ", paste(missing_packages, collapse = ", "),
    "\nInstall them with:\ninstall.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))",
    call. = FALSE
  )
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

allowed_reviewer_decisions <- get_screening_decisions(
  config,
  validate = FALSE
)

exclusion_reason_table <- get_exclusion_reasons(config)
allowed_exclusion_reasons <- exclusion_reason_table$label

if (length(allowed_reviewer_decisions) == 0L) {
  stop("No screening decision options are configured.", call. = FALSE)
}

if (length(allowed_exclusion_reasons) == 0L) {
  stop("No exclusion reasons are configured.", call. = FALSE)
}

agent_config_file <- "config/agent_screening.yml"

if (!file.exists(agent_config_file)) {
  stop("Agent-screening config not found: ", agent_config_file, call. = FALSE)
}

agent_document <- yaml::read_yaml(agent_config_file)
agent_config <- agent_document$agent_screening

if (is.null(agent_config) || !is.list(agent_config)) {
  stop("Invalid agent_screening configuration.", call. = FALSE)
}

input_workbook <- file.path(
  paths$screening_results_directory,
  paste0(agent_config$output$workbook_basename, ".xlsx")
)

completed_workbook <- paths$completed_screening_workbook

final_csv <- file.path(
  paths$processed_screening_directory,
  "final_title_abstract_screening.csv"
)

included_full_text_csv <- file.path(
  paths$processed_screening_directory,
  "included_for_full_text_review.csv"
)

report_file <- file.path(
  paths$logs_directory,
  "final_title_abstract_screening_report.txt"
)

if (!file.exists(input_workbook)) {
  stop(
    "Completed human-review workbook was not found:\n", input_workbook,
    "\nExpected the R/19 agent workbook after Human Reviewer 2 ",
    "and human adjudication have been saved.",
    call. = FALSE
  )
}

dir.create(paths$processed_screening_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$screening_results_directory, recursive = TRUE, showWarnings = FALSE)
dir.create(paths$logs_directory, recursive = TRUE, showWarnings = FALSE)

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_squish(x)
  x[is.na(x) | x == ""] <- NA_character_
  x
}

normalize_decision <- function(x) clean_text(x)
normalize_reason <- function(x) clean_text(x)

final_decision_from_reviewer_decision <- function(x) {
  x <- normalize_decision(x)
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(x == "Unclear", "Awaiting full-text clarification", x)
  )
}

safe_equal <- function(x, y) {
  x <- clean_text(x)
  y <- clean_text(y)
  (is.na(x) & is.na(y)) | (!is.na(x) & !is.na(y) & x == y)
}

validate_reviewer_sheet <- function(data, reviewer_label) {
  required_columns <- c(
    "screening_id", "source_databases", "title", "abstract", "authors",
    "year", "journal", "doi", "pmid", "publication_type", "language",
    "url", "reviewer", "decision", "exclusion_reason", "confidence",
    "notes", "screened_at"
  )

  missing_columns <- setdiff(required_columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      reviewer_label, " is missing required column(s): ",
      paste(missing_columns, collapse = ", "), call. = FALSE
    )
  }

  if (anyDuplicated(data$screening_id) > 0L) {
    stop(reviewer_label, " contains duplicated screening_id values.", call. = FALSE)
  }

  decisions <- normalize_decision(data$decision)

  if (any(is.na(decisions))) {
    stop(
      reviewer_label, " contains ", sum(is.na(decisions)),
      " unscreened record(s).", call. = FALSE
    )
  }

  invalid_decisions <- !decisions %in% allowed_reviewer_decisions
  if (any(invalid_decisions)) {
    stop(
      reviewer_label, " contains invalid decision value(s): ",
      paste(unique(decisions[invalid_decisions]), collapse = ", "),
      call. = FALSE
    )
  }

  reasons <- normalize_reason(data$exclusion_reason)
  excluded <- decisions == "Exclude"

  invalid_exclude_reason <- excluded & (
    is.na(reasons) | !reasons %in% allowed_exclusion_reasons
  )

  if (any(invalid_exclude_reason)) {
    stop(
      reviewer_label, " contains ", sum(invalid_exclude_reason),
      " Exclude record(s) with missing or invalid exclusion reason.",
      call. = FALSE
    )
  }

  reason_on_nonexclude <- !excluded & !is.na(reasons)
  if (any(reason_on_nonexclude)) {
    stop(
      reviewer_label,
      " contains exclusion reason(s) on non-Exclude record(s).",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

workbook_sheets <- openxlsx::getSheetNames(input_workbook)
required_sheets <- c(
  "Reviewer_1", "Reviewer_2", "Conflict_Resolution", "Source_Summary"
)
missing_sheets <- setdiff(required_sheets, workbook_sheets)

if (length(missing_sheets) > 0L) {
  stop(
    "Input workbook is missing required sheet(s): ",
    paste(missing_sheets, collapse = ", "), call. = FALSE
  )
}

reviewer_1 <- openxlsx::readWorkbook(
  input_workbook,
  sheet = "Reviewer_1",
  startRow = 4,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

reviewer_2 <- openxlsx::readWorkbook(
  input_workbook,
  sheet = "Reviewer_2",
  startRow = 4,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

conflict_input <- openxlsx::readWorkbook(
  input_workbook,
  sheet = "Conflict_Resolution",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

source_summary_input <- openxlsx::readWorkbook(
  input_workbook,
  sheet = "Source_Summary",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

validate_reviewer_sheet(reviewer_1, "Reviewer_1")
validate_reviewer_sheet(reviewer_2, "Reviewer_2")

if (nrow(reviewer_1) != nrow(reviewer_2)) {
  stop("Reviewer_1 and Reviewer_2 row counts differ.", call. = FALSE)
}

if (!identical(as.character(reviewer_1$screening_id), as.character(reviewer_2$screening_id))) {
  stop("Reviewer_1 and Reviewer_2 screening_id order is not aligned.", call. = FALSE)
}

screening_ids <- as.character(reviewer_1$screening_id)

if (nrow(conflict_input) != nrow(reviewer_1)) {
  stop("Conflict_Resolution row count does not match reviewer sheets.", call. = FALSE)
}

if (!"screening_id" %in% names(conflict_input) ||
    !identical(as.character(conflict_input$screening_id), screening_ids)) {
  stop(
    "Conflict_Resolution screening_id values are not aligned with reviewer sheets.",
    call. = FALSE
  )
}

r1_decision <- normalize_decision(reviewer_1$decision)
r2_decision <- normalize_decision(reviewer_2$decision)
r1_reason <- normalize_reason(reviewer_1$exclusion_reason)
r2_reason <- normalize_reason(reviewer_2$exclusion_reason)

decision_agreement <- safe_equal(r1_decision, r2_decision)
exclude_reason_agreement <- rep(TRUE, length(screening_ids))
both_exclude <- (r1_decision == "Exclude" & r2_decision == "Exclude")

exclude_reason_agreement[both_exclude] <- safe_equal(
  r1_reason[both_exclude],
  r2_reason[both_exclude]
)

true_agreement <- decision_agreement & exclude_reason_agreement
conflict_required <- !true_agreement

agreement_status <- ifelse(true_agreement, "Agree", "Disagree")
conflict_status <- ifelse(conflict_required, "Conflict", "No conflict")
conflict_type <- ifelse(
  !decision_agreement,
  "Decision conflict",
  ifelse(
    decision_agreement & both_exclude & !exclude_reason_agreement,
    "Exclusion-reason conflict",
    "None"
  )
)

required_conflict_columns <- c(
  "screening_id", "final_decision", "final_exclusion_reason",
  "adjudicator", "resolution_notes", "resolved_at"
)
missing_conflict_columns <- setdiff(required_conflict_columns, names(conflict_input))

if (length(missing_conflict_columns) > 0L) {
  stop(
    "Conflict_Resolution is missing adjudication column(s): ",
    paste(missing_conflict_columns, collapse = ", "), call. = FALSE
  )
}

adjudicated_decision <- normalize_decision(conflict_input$final_decision)
adjudicated_reason <- normalize_reason(conflict_input$final_exclusion_reason)
adjudicator <- clean_text(conflict_input$adjudicator)
resolution_notes <- clean_text(conflict_input$resolution_notes)
resolved_at <- clean_text(conflict_input$resolved_at)

allowed_final_decisions <- c(
  "Include", "Exclude", "Awaiting full-text clarification"
)

adjudicated_decision[adjudicated_decision == "Unclear"] <-
  "Awaiting full-text clarification"

invalid_adjudicated_decision <- conflict_required &
  !is.na(adjudicated_decision) &
  !adjudicated_decision %in% allowed_final_decisions

if (any(invalid_adjudicated_decision)) {
  stop(
    "Conflict_Resolution contains invalid adjudicated final decision value(s): ",
    paste(unique(adjudicated_decision[invalid_adjudicated_decision]), collapse = ", "),
    call. = FALSE
  )
}

unresolved_conflict <- conflict_required & is.na(adjudicated_decision)

if (any(unresolved_conflict)) {
  unresolved_ids <- screening_ids[unresolved_conflict]
  stop(
    "Final title/abstract screening is incomplete. ",
    length(unresolved_ids), " conflict(s) still require human adjudication.\n",
    "Unresolved screening_id(s): ", paste(unresolved_ids, collapse = ", "),
    call. = FALSE
  )
}

conflict_exclude_missing_reason <- conflict_required &
  adjudicated_decision == "Exclude" &
  (is.na(adjudicated_reason) | !adjudicated_reason %in% allowed_exclusion_reasons)
conflict_exclude_missing_reason[is.na(conflict_exclude_missing_reason)] <- FALSE

if (any(conflict_exclude_missing_reason)) {
  stop(
    "Adjudicated Exclude decision(s) contain missing or invalid final exclusion reason.",
    call. = FALSE
  )
}

conflict_nonexclude_with_reason <- conflict_required &
  adjudicated_decision != "Exclude" & !is.na(adjudicated_reason)
conflict_nonexclude_with_reason[is.na(conflict_nonexclude_with_reason)] <- FALSE

if (any(conflict_nonexclude_with_reason)) {
  stop(
    "Adjudicated non-Exclude decision(s) contain final exclusion reason.",
    call. = FALSE
  )
}

final_decision <- rep(NA_character_, length(screening_ids))
final_exclusion_reason <- rep(NA_character_, length(screening_ids))
final_decision_source <- ifelse(true_agreement, "agreement", "adjudication")

agreement_rows <- which(true_agreement)
if (length(agreement_rows) > 0L) {
  final_decision[agreement_rows] <- final_decision_from_reviewer_decision(
    r1_decision[agreement_rows]
  )

  agreement_exclude_rows <- agreement_rows[r1_decision[agreement_rows] == "Exclude"]
  if (length(agreement_exclude_rows) > 0L) {
    final_exclusion_reason[agreement_exclude_rows] <- r1_reason[agreement_exclude_rows]
  }
}

conflict_rows <- which(conflict_required)
if (length(conflict_rows) > 0L) {
  final_decision[conflict_rows] <- adjudicated_decision[conflict_rows]

  conflict_final_exclude <- conflict_rows[final_decision[conflict_rows] == "Exclude"]
  if (length(conflict_final_exclude) > 0L) {
    final_exclusion_reason[conflict_final_exclude] <-
      adjudicated_reason[conflict_final_exclude]
  }
}

if (any(is.na(final_decision))) {
  stop(
    "Internal finalization error: one or more final decisions remain missing.",
    call. = FALSE
  )
}

final_screening <- tibble::tibble(
  screening_id = screening_ids,
  source_databases = clean_text(reviewer_1$source_databases),
  title = clean_text(reviewer_1$title),
  abstract = clean_text(reviewer_1$abstract),
  authors = clean_text(reviewer_1$authors),
  year = clean_text(reviewer_1$year),
  journal = clean_text(reviewer_1$journal),
  doi = clean_text(reviewer_1$doi),
  pmid = clean_text(reviewer_1$pmid),
  publication_type = clean_text(reviewer_1$publication_type),
  language = clean_text(reviewer_1$language),
  url = clean_text(reviewer_1$url),
  reviewer_1_decision = r1_decision,
  reviewer_1_exclusion_reason = r1_reason,
  reviewer_2_decision = r2_decision,
  reviewer_2_exclusion_reason = r2_reason,
  agreement_status = agreement_status,
  conflict_status = conflict_status,
  conflict_type = conflict_type,
  final_decision = final_decision,
  final_exclusion_reason = final_exclusion_reason,
  final_decision_source = final_decision_source,
  adjudicator = ifelse(conflict_required, adjudicator, NA_character_),
  resolution_notes = ifelse(conflict_required, resolution_notes, NA_character_),
  resolved_at = ifelse(conflict_required, resolved_at, NA_character_)
)

if (nrow(final_screening) != nrow(reviewer_1)) {
  stop("Final screening row count does not match reviewer input.", call. = FALSE)
}

if (anyDuplicated(final_screening$screening_id) > 0L) {
  stop("Final screening dataset contains duplicated screening_id values.", call. = FALSE)
}

final_exclude <- final_screening$final_decision == "Exclude"
invalid_final_reason <- final_exclude & (
  is.na(final_screening$final_exclusion_reason) |
    !final_screening$final_exclusion_reason %in% allowed_exclusion_reasons
)

if (any(invalid_final_reason)) {
  stop(
    "Final Exclude records contain missing or invalid exclusion reason.",
    call. = FALSE
  )
}

final_nonexclude_reason <- !final_exclude &
  !is.na(final_screening$final_exclusion_reason)

if (any(final_nonexclude_reason)) {
  stop("Final non-Exclude records contain exclusion reasons.", call. = FALSE)
}

if (any(final_screening$conflict_status == "Conflict" &
        final_screening$final_decision_source != "adjudication")) {
  stop("Conflict record exists without adjudication provenance.", call. = FALSE)
}

if (any(final_screening$conflict_status == "No conflict" &
        final_screening$final_decision_source != "agreement")) {
  stop("Agreement record exists without agreement provenance.", call. = FALSE)
}

# ============================================================
# Source_Summary validation and rebuild
# ------------------------------------------------------------
# R/18 Source_Summary means unique-record provenance counts by
# the exact source_databases combination. R/21 preserves that
# meaning, validates it against reviewer records, then appends
# final screening outcome summaries in separate sections.
# ============================================================

source_summary_expected <- final_screening |>
  dplyr::mutate(
    source_databases = dplyr::if_else(
      is.na(source_databases), "Unknown", source_databases
    )
  ) |>
  dplyr::count(source_databases, name = "record_count") |>
  dplyr::arrange(dplyr::desc(record_count), source_databases)

source_summary_expected <- dplyr::bind_rows(
  source_summary_expected,
  tibble::tibble(
    source_databases = "Total unique records",
    record_count = nrow(final_screening)
  )
)

if (!all(c("source_databases", "record_count") %in% names(source_summary_input))) {
  stop(
    "Source_Summary does not contain the expected source_databases and record_count columns.",
    call. = FALSE
  )
}

source_summary_existing <- source_summary_input |>
  dplyr::transmute(
    source_databases = clean_text(source_databases),
    record_count = suppressWarnings(as.integer(record_count))
  ) |>
  dplyr::filter(!is.na(source_databases))

existing_total <- source_summary_existing$record_count[
  source_summary_existing$source_databases == "Total unique records"
]

if (length(existing_total) != 1L || is.na(existing_total) ||
    existing_total != nrow(final_screening)) {
  stop(
    "Existing Source_Summary total unique record count is inconsistent with the reviewer sheets.",
    call. = FALSE
  )
}

expected_detail <- source_summary_expected |>
  dplyr::filter(source_databases != "Total unique records") |>
  dplyr::arrange(source_databases)

existing_detail <- source_summary_existing |>
  dplyr::filter(source_databases != "Total unique records") |>
  dplyr::arrange(source_databases)

if (nrow(expected_detail) != nrow(existing_detail) ||
    !identical(expected_detail$source_databases, existing_detail$source_databases) ||
    !identical(as.integer(expected_detail$record_count), as.integer(existing_detail$record_count))) {
  stop(
    "Existing Source_Summary provenance counts are inconsistent with the screening records.",
    call. = FALSE
  )
}

final_decision_summary <- final_screening |>
  dplyr::count(final_decision, name = "record_count") |>
  dplyr::arrange(dplyr::desc(record_count), final_decision)

# ============================================================
# Included-for-full-text-review handoff
# ============================================================
#
# IMPORTANT:
# These records have passed TITLE/ABSTRACT screening only.
# They are candidates for FULL-TEXT REVIEW and must not be
# interpreted as the final meta-analysis Included Studies.
# A true Included Studies dataset will be created only after
# full-text screening has been completed.
# ============================================================

included_for_full_text_review <- final_screening |>
  dplyr::filter(
    final_decision == "Include"
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
    reviewer_1_decision,
    reviewer_1_exclusion_reason,
    reviewer_2_decision,
    reviewer_2_exclusion_reason,
    agreement_status,
    conflict_status,
    conflict_type,
    final_decision,
    final_decision_source,
    adjudicator,
    resolution_notes,
    resolved_at
  )

if (
  any(
    included_for_full_text_review$final_decision != "Include"
  )
) {
  stop(
    "Internal handoff error: Included_for_Full_Text_Review ",
    "contains a record that is not finally included.",
    call. = FALSE
  )
}

workflow_summary <- tibble::tibble(
  metric = c(
    "Total unique records",
    "Agreement records",
    "Adjudicated conflict records",
    "Unresolved conflicts",
    "Final Include",
    "Final Exclude",
    "Final awaiting full-text clarification"
  ),
  record_count = c(
    nrow(final_screening),
    sum(final_screening$final_decision_source == "agreement"),
    sum(final_screening$final_decision_source == "adjudication"),
    0L,
    sum(final_screening$final_decision == "Include"),
    sum(final_screening$final_decision == "Exclude"),
    sum(final_screening$final_decision == "Awaiting full-text clarification")
  )
)

if (workflow_summary$record_count[workflow_summary$metric == "Agreement records"] +
    workflow_summary$record_count[workflow_summary$metric == "Adjudicated conflict records"] !=
    nrow(final_screening)) {
  stop("Agreement + adjudication counts do not equal total records.", call. = FALSE)
}

if (sum(final_decision_summary$record_count) != nrow(final_screening)) {
  stop("Final decision counts do not equal total records.", call. = FALSE)
}

readr::write_csv(final_screening, final_csv, na = "")

if (!file.exists(final_csv) || is.na(file.info(final_csv)$size) ||
    file.info(final_csv)$size <= 0) {
  stop("Final title/abstract screening CSV was not created successfully.", call. = FALSE)
}

readr::write_csv(
  included_for_full_text_review,
  included_full_text_csv,
  na = ""
)

if (!file.exists(included_full_text_csv) ||
    is.na(file.info(included_full_text_csv)$size) ||
    file.info(included_full_text_csv)$size <= 0) {
  stop(
    "Included-for-full-text-review CSV was not created successfully.",
    call. = FALSE
  )
}

copied <- file.copy(input_workbook, completed_workbook, overwrite = TRUE)

if (!isTRUE(copied)) {
  stop("Failed to create completed screening workbook:\n", completed_workbook, call. = FALSE)
}

workbook <- openxlsx::loadWorkbook(completed_workbook)

conflict_output <- tibble::tibble(
  screening_id = final_screening$screening_id,
  title = final_screening$title,
  year = final_screening$year,
  journal = final_screening$journal,
  reviewer_1_decision = final_screening$reviewer_1_decision,
  reviewer_2_decision = final_screening$reviewer_2_decision,
  agreement_status = final_screening$agreement_status,
  conflict_status = final_screening$conflict_status,
  final_decision = final_screening$final_decision,
  final_exclusion_reason = final_screening$final_exclusion_reason,
  adjudicator = final_screening$adjudicator,
  resolution_notes = final_screening$resolution_notes,
  resolved_at = final_screening$resolved_at
)

openxlsx::deleteData(
  workbook,
  sheet = "Conflict_Resolution",
  cols = 1:ncol(conflict_output),
  rows = 3:(nrow(conflict_output) + 4),
  gridExpand = TRUE
)

openxlsx::writeData(
  workbook,
  sheet = "Conflict_Resolution",
  x = conflict_output,
  startRow = 3,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE,
  withFilter = TRUE,
  keepNA = FALSE
)

openxlsx::deleteData(
  workbook,
  sheet = "Source_Summary",
  cols = 1:4,
  rows = 1:max(200L, nrow(source_summary_expected) + nrow(workflow_summary) + 20L),
  gridExpand = TRUE
)

openxlsx::writeData(
  workbook,
  "Source_Summary",
  paste0(project_title, " — Search Source Summary"),
  startRow = 1,
  startCol = 1
)

# A1:B1 is already merged by R/18. deleteData() clears cell
# contents but preserves merge metadata, so R/21 must not
# attempt to merge A1:B1 again.

openxlsx::writeData(
  workbook,
  "Source_Summary",
  source_summary_expected,
  startRow = 3,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE,
  withFilter = FALSE
)

screening_summary_title_row <- nrow(source_summary_expected) + 6L

openxlsx::writeData(
  workbook,
  "Source_Summary",
  "Final Title/Abstract Screening Summary",
  startRow = screening_summary_title_row,
  startCol = 1
)

openxlsx::mergeCells(
  workbook,
  "Source_Summary",
  cols = 1:2,
  rows = screening_summary_title_row
)

openxlsx::writeData(
  workbook,
  "Source_Summary",
  workflow_summary,
  startRow = screening_summary_title_row + 2L,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE,
  withFilter = FALSE
)

decision_summary_title_row <- screening_summary_title_row + nrow(workflow_summary) + 5L

openxlsx::writeData(
  workbook,
  "Source_Summary",
  "Final Decision Distribution",
  startRow = decision_summary_title_row,
  startCol = 1
)

openxlsx::mergeCells(
  workbook,
  "Source_Summary",
  cols = 1:2,
  rows = decision_summary_title_row
)

openxlsx::writeData(
  workbook,
  "Source_Summary",
  final_decision_summary,
  startRow = decision_summary_title_row + 2L,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE,
  withFilter = FALSE
)

openxlsx::setColWidths(workbook, "Source_Summary", cols = 1, widths = 55)
openxlsx::setColWidths(workbook, "Source_Summary", cols = 2, widths = 22)

included_sheet_name <- "Included_for_Full_Text_Review"

if (included_sheet_name %in% names(workbook)) {
  openxlsx::removeWorksheet(
    workbook,
    included_sheet_name
  )
}

openxlsx::addWorksheet(
  workbook,
  included_sheet_name
)

openxlsx::writeData(
  workbook,
  included_sheet_name,
  paste0(
    project_title,
    " — Included for Full-Text Review"
  ),
  startRow = 1,
  startCol = 1
)

if (ncol(included_for_full_text_review) > 1L) {
  openxlsx::mergeCells(
    workbook,
    included_sheet_name,
    cols = 1:ncol(included_for_full_text_review),
    rows = 1
  )
}

openxlsx::writeData(
  workbook,
  included_sheet_name,
  included_for_full_text_review,
  startRow = 3,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE,
  withFilter = TRUE,
  keepNA = FALSE
)

openxlsx::freezePane(
  workbook,
  included_sheet_name,
  firstActiveRow = 4,
  firstActiveCol = 1
)

if (ncol(included_for_full_text_review) > 0L) {
  openxlsx::setColWidths(
    workbook,
    included_sheet_name,
    cols = 1:ncol(included_for_full_text_review),
    widths = "auto"
  )
}

if ("title" %in% names(included_for_full_text_review)) {
  openxlsx::setColWidths(
    workbook,
    included_sheet_name,
    cols = which(names(included_for_full_text_review) == "title"),
    widths = 50
  )
}

if ("abstract" %in% names(included_for_full_text_review)) {
  openxlsx::setColWidths(
    workbook,
    included_sheet_name,
    cols = which(names(included_for_full_text_review) == "abstract"),
    widths = 80
  )
}

openxlsx::saveWorkbook(workbook, completed_workbook, overwrite = TRUE)

if (!file.exists(completed_workbook) || is.na(file.info(completed_workbook)$size) ||
    file.info(completed_workbook)$size <= 0) {
  stop("Completed screening workbook was not saved successfully.", call. = FALSE)
}

completed_sheets <- openxlsx::getSheetNames(completed_workbook)
required_completed_sheets <- c(
  "Instructions", "Reviewer_1", "Reviewer_2", "Conflict_Resolution",
  "Source_Summary", "Validation_Lists",
  "Included_for_Full_Text_Review"
)
missing_completed_sheets <- setdiff(required_completed_sheets, completed_sheets)

if (length(missing_completed_sheets) > 0L) {
  stop(
    "Completed workbook is missing sheet(s): ",
    paste(missing_completed_sheets, collapse = ", "), call. = FALSE
  )
}

conflict_saved <- openxlsx::readWorkbook(
  completed_workbook,
  sheet = "Conflict_Resolution",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

if (nrow(conflict_saved) != nrow(final_screening) ||
    !identical(as.character(conflict_saved$screening_id), final_screening$screening_id)) {
  stop(
    "Saved Conflict_Resolution sheet does not match final screening dataset.",
    call. = FALSE
  )
}

saved_final_decision <- clean_text(conflict_saved$final_decision)
if (!identical(saved_final_decision, final_screening$final_decision)) {
  stop(
    "Saved Conflict_Resolution final_decision values do not match authoritative final dataset.",
    call. = FALSE
  )
}

included_saved <- openxlsx::readWorkbook(
  completed_workbook,
  sheet = "Included_for_Full_Text_Review",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

if (nrow(included_saved) != nrow(included_for_full_text_review)) {
  stop(
    "Saved Included_for_Full_Text_Review row count does not match ",
    "the authoritative handoff dataset.",
    call. = FALSE
  )
}

if (nrow(included_for_full_text_review) > 0L) {

  if (!"screening_id" %in% names(included_saved) ||
      !identical(
        as.character(included_saved$screening_id),
        as.character(included_for_full_text_review$screening_id)
      )) {
    stop(
      "Saved Included_for_Full_Text_Review screening IDs do not match ",
      "the authoritative handoff dataset.",
      call. = FALSE
    )
  }

  if (!"final_decision" %in% names(included_saved) ||
      !all(clean_text(included_saved$final_decision) == "Include")) {
    stop(
      "Saved Included_for_Full_Text_Review contains a record whose ",
      "final_decision is not Include.",
      call. = FALSE
    )
  }
}

source_saved <- openxlsx::readWorkbook(
  completed_workbook,
  sheet = "Source_Summary",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = TRUE,
  rows = 3:(nrow(source_summary_expected) + 3)
)

if (!all(c("source_databases", "record_count") %in% names(source_saved))) {
  stop("Completed Source_Summary could not be validated.", call. = FALSE)
}

source_saved <- source_saved |>
  dplyr::select(source_databases, record_count) |>
  dplyr::filter(!is.na(source_databases))

if (nrow(source_saved) != nrow(source_summary_expected) ||
    !identical(as.character(source_saved$source_databases), as.character(source_summary_expected$source_databases)) ||
    !identical(as.integer(source_saved$record_count), as.integer(source_summary_expected$record_count))) {
  stop(
    "Completed Source_Summary provenance counts do not match final screening records.",
    call. = FALSE
  )
}

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "Final Title/Abstract Screening Report",
  "",
  paste0("Project ID: ", project_id),
  paste0("Project title: ", project_title),
  paste0("Finalized at: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  "",
  "INPUT",
  "--------------------------------------------------",
  paste0("Human-reviewed workbook: ", input_workbook),
  "",
  "FINAL SCREENING COUNTS",
  "--------------------------------------------------",
  paste0(workflow_summary$metric, ": ", workflow_summary$record_count),
  "",
  "FINAL DECISION DISTRIBUTION",
  "--------------------------------------------------",
  paste0(final_decision_summary$final_decision, ": ", final_decision_summary$record_count),
  "",
  "SOURCE SUMMARY VALIDATION",
  "--------------------------------------------------",
  paste0(
    "Source_Summary provenance categories validated: ",
    nrow(source_summary_expected) - 1L
  ),
  paste0("Total unique records validated: ", nrow(final_screening)),
  "",
  "",
  "FULL-TEXT REVIEW HANDOFF",
  "--------------------------------------------------",
  paste0(
    "Records included for full-text review: ",
    nrow(included_for_full_text_review)
  ),
  "These records are not yet the final meta-analysis Included Studies.",
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0("Completed workbook: ", completed_workbook),
  paste0("Final screening CSV: ", final_csv),
  paste0(
    "Included-for-full-text-review CSV: ",
    included_full_text_csv
  ),
  paste0("Finalization report: ", report_file),
  "",
  "VALIDATION",
  "--------------------------------------------------",
  "Reviewer_1 complete: PASS",
  "Reviewer_2 complete: PASS",
  "Reviewer alignment: PASS",
  "Exclusion reason rules: PASS",
  "Agreement inheritance: PASS",
  "Conflict adjudication completeness: PASS",
  "Unresolved conflicts: 0",
  "Source_Summary consistency: PASS",
  "Included_for_Full_Text_Review handoff: PASS",
  "Final record-count consistency: PASS"
)

writeLines(report_lines, report_file, useBytes = TRUE)

if (!file.exists(report_file) || is.na(file.info(report_file)$size) ||
    file.info(report_file)$size <= 0) {
  stop("Finalization report was not created successfully.", call. = FALSE)
}

cat("\n====================================================\n")
cat("Final Title/Abstract Screening Completed\n")
cat("====================================================\n\n")
cat("Project:", project_title, "\n")
cat("Total records:", nrow(final_screening), "\n")
cat("Agreement records:", sum(final_decision_source == "agreement"), "\n")
cat("Adjudicated conflicts:", sum(final_decision_source == "adjudication"), "\n")
cat("Unresolved conflicts: 0\n\n")
cat("Completed workbook:\n- ", completed_workbook, "\n", sep = "")
cat("Final screening CSV:\n- ", final_csv, "\n", sep = "")
cat(
  "Included for full-text review:",
  nrow(included_for_full_text_review),
  "\n"
)
cat(
  "Included-for-full-text-review CSV:\n- ",
  included_full_text_csv,
  "\n",
  sep = ""
)
cat("Report:\n- ", report_file, "\n\n", sep = "")
cat(
  "PASS: R/21 final title/abstract screening finalization ",
  "and validation completed successfully.\n",
  sep = ""
)
cat(
  "\nNext step:\n",
  "Proceed to the PM-defined full-text screening stage.\n",
  sep = ""
)
