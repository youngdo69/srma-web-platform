# ============================================================
# Universal SR-MA Agent Toolkit
# R/22_prepare_full_text_registry.R
#
# TASK-010 — Full-Text Registry & PDF Intake
#
# Purpose
# ------------------------------------------------------------
# 1. Use the authoritative Phase 1 handoff:
#      data/processed/screening/included_for_full_text_review.csv
# 2. Create a topic-neutral registry for full-text acquisition.
# 3. Assign one deterministic PDF filename per screening_id.
# 4. Scan the canonical PDF intake directory.
# 5. Validate and classify PDF acquisition state:
#      Available / Missing / Invalid PDF.
# 6. Create CSV/XLSX/report outputs for the full-text intake stage.
# 7. PASS TASK-010 when every candidate is registered and its
#    acquisition state is explicitly recorded, even if some PDFs
#    are still Missing or Invalid.
# 8. Preserve the boundary between PDF intake and eligibility.
#
# IMPORTANT
# ------------------------------------------------------------
# - This script DOES NOT perform full-text eligibility decisions.
# - This script DOES NOT exclude studies because a PDF is missing.
# - Missing or Invalid PDF is an intake/acquisition state only.
# - TASK-010 can PASS with Missing/Invalid PDFs provided the registry
#   is complete and every acquisition state is explicitly recorded.
# - "Full text not available" remains an eligibility reason for a
#   later full-text screening/finalization stage.
# - The true "Included Studies" dataset is NOT created here.
# - This stage is intentionally standalone and is NOT connected
#   to the Universal Workflow Runner.
#
# Canonical Phase 2 directory convention used by TASK-010
# ------------------------------------------------------------
# data/raw/full_text/pdfs/
# data/processed/full_text/
# results/full_text/
#
# These Phase 2 paths are derived from paths$project_root because
# the currently validated project_config.yml/path_utils.R contract
# ends at Phase 1. No Phase 1 configuration is modified here.
# ============================================================


# ============================================================
# 1. Required packages
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "openxlsx"
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
    paste(missing_packages, collapse = ", "),
    "\nInstall them once with:\n",
    "install.packages(c(",
    paste(
      paste0('"', missing_packages, '"'),
      collapse = ", "
    ),
    "))",
    call. = FALSE
  )
}


# ============================================================
# 2. Load existing canonical configuration and Phase 1 paths
# ============================================================

source(
  "R/utils/config_utils.R"
)

source(
  "R/utils/path_utils.R"
)

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

project_id <- get_project_id(
  config
)

project_title <- get_project_title(
  config
)


# ============================================================
# 3. TASK-010 canonical paths
# ============================================================

input_file <- file.path(
  paths$processed_screening_directory,
  "included_for_full_text_review.csv"
)

full_text_raw_directory <- file.path(
  paths$project_root,
  "data",
  "raw",
  "full_text"
)

pdf_directory <- file.path(
  full_text_raw_directory,
  "pdfs"
)

processed_full_text_directory <- file.path(
  paths$project_root,
  "data",
  "processed",
  "full_text"
)

full_text_results_directory <- file.path(
  paths$project_root,
  "results",
  "full_text"
)

registry_csv <- file.path(
  processed_full_text_directory,
  "full_text_registry.csv"
)

registry_workbook <- file.path(
  full_text_results_directory,
  "full_text_registry.xlsx"
)

intake_report <- file.path(
  paths$logs_directory,
  "full_text_registry_intake_report.txt"
)

unmatched_pdf_csv <- file.path(
  processed_full_text_directory,
  "unmatched_full_text_pdfs.csv"
)


# ============================================================
# 4. Create Phase 2 intake directories
# ============================================================

ensure_directories(
  c(
    full_text_raw_directory,
    pdf_directory,
    processed_full_text_directory,
    full_text_results_directory,
    paths$logs_directory
  )
)

full_text_raw_directory <- normalize_project_path(
  full_text_raw_directory,
  must_work = TRUE
)

pdf_directory <- normalize_project_path(
  pdf_directory,
  must_work = TRUE
)

processed_full_text_directory <- normalize_project_path(
  processed_full_text_directory,
  must_work = TRUE
)

full_text_results_directory <- normalize_project_path(
  full_text_results_directory,
  must_work = TRUE
)


# ============================================================
# 5. Helper functions
# ============================================================

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

  data[[matching_columns[[1]]]]
}


sanitize_screening_id_for_filename <- function(x) {

  x <- clean_character(x)

  if (any(is.na(x))) {

    stop(
      "screening_id contains missing or empty values.",
      call. = FALSE
    )
  }

  safe <- stringr::str_replace_all(
    x,
    "[\\\\/:*?\"<>|]",
    "_"
  )

  safe <- stringr::str_replace_all(
    safe,
    "[[:space:]]+",
    "_"
  )

  safe <- stringr::str_replace_all(
    safe,
    "_+",
    "_"
  )

  safe <- stringr::str_replace_all(
    safe,
    "^_+|_+$",
    ""
  )

  if (any(!nzchar(safe))) {

    stop(
      "At least one screening_id cannot be converted to a safe PDF filename.",
      call. = FALSE
    )
  }

  safe
}


is_pdf_signature_valid <- function(file_path) {

  if (
    !file.exists(file_path) ||
    is.na(file.info(file_path)$size) ||
    file.info(file_path)$size < 5
  ) {

    return(FALSE)
  }

  signature <- tryCatch(
    {
      connection <- file(
        file_path,
        open = "rb"
      )

      on.exit(
        close(connection),
        add = TRUE
      )

      raw_signature <- readBin(
        connection,
        what = "raw",
        n = 5
      )

      rawToChar(
        raw_signature
      )
    },
    error = function(e) {
      ""
    }
  )

  identical(
    signature,
    "%PDF-"
  )
}


safe_md5 <- function(file_path) {

  if (!file.exists(file_path)) {

    return(
      NA_character_
    )
  }

  result <- tryCatch(
    unname(
      tools::md5sum(
        file_path
      )
    ),
    error = function(e) {
      NA_character_
    }
  )

  as.character(
    result
  )
}


format_file_time <- function(x) {

  if (
    length(x) == 0L ||
    is.na(x)
  ) {

    return(
      NA_character_
    )
  }

  format(
    x,
    "%Y-%m-%d %H:%M:%S"
  )
}


excel_column_name <- function(column_number) {

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


# ============================================================
# 6. Validate canonical Phase 1 handoff
# ============================================================

assert_file_exists(
  input_file,
  "TASK-008 canonical handoff"
)

handoff <- readr::read_csv(
  input_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
)

if (nrow(handoff) == 0L) {

  stop(
    "The TASK-008 handoff contains zero records.\n",
    "Full-text registry intake cannot proceed.",
    call. = FALSE
  )
}


required_handoff_columns <- c(
  "screening_id",
  "title",
  "final_decision"
)

missing_handoff_columns <- setdiff(
  required_handoff_columns,
  names(handoff)
)

if (length(missing_handoff_columns) > 0L) {

  stop(
    "The TASK-008 handoff is missing required column(s):\n- ",
    paste(
      missing_handoff_columns,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}


handoff$screening_id <- clean_character(
  handoff$screening_id
)

handoff$final_decision <- clean_character(
  handoff$final_decision
)


if (any(is.na(handoff$screening_id))) {

  stop(
    "The TASK-008 handoff contains missing screening_id values.",
    call. = FALSE
  )
}


if (anyDuplicated(handoff$screening_id) > 0L) {

  duplicated_ids <- unique(
    handoff$screening_id[
      duplicated(
        handoff$screening_id
      )
    ]
  )

  stop(
    "The TASK-008 handoff contains duplicated screening_id values:\n- ",
    paste(
      duplicated_ids,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}


invalid_final_decisions <- handoff[
  is.na(handoff$final_decision) |
    handoff$final_decision != "Include",
  ,
  drop = FALSE
]

if (nrow(invalid_final_decisions) > 0L) {

  stop(
    "The canonical full-text handoff must contain only records with ",
    "final_decision == 'Include'.\n",
    "Invalid record count: ",
    nrow(invalid_final_decisions),
    call. = FALSE
  )
}


# ============================================================
# 7. Standardize bibliographic intake fields
# ============================================================

registry_base <- tibble::tibble(
  screening_id = handoff$screening_id,

  source_databases = clean_character(
    first_existing_column(
      handoff,
      c(
        "source_databases",
        "database"
      )
    )
  ),

  title = clean_character(
    first_existing_column(
      handoff,
      c(
        "title",
        "article_title"
      )
    )
  ),

  abstract = clean_character(
    first_existing_column(
      handoff,
      c(
        "abstract"
      )
    )
  ),

  authors = clean_character(
    first_existing_column(
      handoff,
      c(
        "authors",
        "author"
      )
    )
  ),

  year = clean_character(
    first_existing_column(
      handoff,
      c(
        "year",
        "publication_year"
      )
    )
  ),

  journal = clean_character(
    first_existing_column(
      handoff,
      c(
        "journal",
        "journal_title"
      )
    )
  ),

  doi = clean_character(
    first_existing_column(
      handoff,
      c(
        "doi",
        "DOI"
      )
    )
  ),

  pmid = clean_character(
    first_existing_column(
      handoff,
      c(
        "pmid",
        "PMID"
      )
    )
  ),

  publication_type = clean_character(
    first_existing_column(
      handoff,
      c(
        "publication_type"
      )
    )
  ),

  language = clean_character(
    first_existing_column(
      handoff,
      c(
        "language"
      )
    )
  ),

  url = clean_character(
    first_existing_column(
      handoff,
      c(
        "url",
        "URL"
      )
    )
  )
)


# ============================================================
# 8. Assign deterministic expected PDF filenames
# ============================================================

safe_screening_ids <- sanitize_screening_id_for_filename(
  registry_base$screening_id
)

pdf_expected_filename <- paste0(
  safe_screening_ids,
  ".pdf"
)

if (anyDuplicated(pdf_expected_filename) > 0L) {

  duplicated_pdf_names <- unique(
    pdf_expected_filename[
      duplicated(
        pdf_expected_filename
      )
    ]
  )

  stop(
    "Expected PDF filenames are not unique after screening_id sanitization:\n- ",
    paste(
      duplicated_pdf_names,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}


expected_pdf_paths <- file.path(
  pdf_directory,
  pdf_expected_filename
)

expected_pdf_paths <- vapply(
  expected_pdf_paths,
  normalize_project_path,
  character(1),
  must_work = FALSE,
  USE.NAMES = FALSE
)


# ============================================================
# 9. Scan expected PDFs
# ============================================================

scan_time <- format(
  Sys.time(),
  "%Y-%m-%d %H:%M:%S"
)


pdf_exists <- file.exists(
  expected_pdf_paths
)

pdf_file_size <- rep(
  NA_real_,
  length(
    expected_pdf_paths
  )
)

pdf_modified_at <- rep(
  NA_character_,
  length(
    expected_pdf_paths
  )
)

pdf_signature_valid <- rep(
  FALSE,
  length(
    expected_pdf_paths
  )
)

pdf_checksum_md5 <- rep(
  NA_character_,
  length(
    expected_pdf_paths
  )
)


for (i in seq_along(expected_pdf_paths)) {

  current_file <- expected_pdf_paths[[i]]

  if (!file.exists(current_file)) {

    next
  }


  file_information <- file.info(
    current_file
  )

  pdf_file_size[[i]] <- as.numeric(
    file_information$size
  )

  pdf_modified_at[[i]] <- format_file_time(
    file_information$mtime
  )

  pdf_signature_valid[[i]] <- is_pdf_signature_valid(
    current_file
  )

  if (
    isTRUE(
      pdf_signature_valid[[i]]
    )
  ) {

    pdf_checksum_md5[[i]] <- safe_md5(
      current_file
    )
  }
}


pdf_status <- dplyr::case_when(
  !pdf_exists ~ "Missing",
  pdf_exists & (
    is.na(pdf_file_size) |
      pdf_file_size <= 0
  ) ~ "Invalid PDF",
  pdf_exists & !pdf_signature_valid ~ "Invalid PDF",
  TRUE ~ "Available"
)


intake_status <- dplyr::case_when(
  pdf_status == "Available" ~ "Ready for full-text eligibility",
  pdf_status == "Missing" ~ "Awaiting PDF",
  TRUE ~ "Invalid file"
)


# ============================================================
# 10. Detect unmatched PDFs in intake directory
# ============================================================

all_pdf_files <- list.files(
  pdf_directory,
  pattern = "\\.pdf$",
  full.names = TRUE,
  ignore.case = TRUE,
  recursive = FALSE
)

all_pdf_files <- if (
  length(all_pdf_files) == 0L
) {
  character(0)
} else {
  vapply(
    all_pdf_files,
    normalize_project_path,
    character(1),
    must_work = TRUE,
    USE.NAMES = FALSE
  )
}


expected_normalized <- expected_pdf_paths[
  pdf_exists
]

unmatched_pdf_paths <- setdiff(
  all_pdf_files,
  expected_normalized
)


unmatched_pdf_data <- if (
  length(unmatched_pdf_paths) == 0L
) {

  tibble::tibble(
    pdf_filename = character(0),
    pdf_path = character(0),
    file_size_bytes = numeric(0),
    modified_at = character(0),
    pdf_signature_valid = logical(0),
    checksum_md5 = character(0),
    note = character(0)
  )

} else {

  unmatched_information <- file.info(
    unmatched_pdf_paths
  )

  tibble::tibble(
    pdf_filename = basename(
      unmatched_pdf_paths
    ),

    pdf_path = unmatched_pdf_paths,

    file_size_bytes = as.numeric(
      unmatched_information$size
    ),

    modified_at = vapply(
      unmatched_information$mtime,
      format_file_time,
      character(1),
      USE.NAMES = FALSE
    ),

    pdf_signature_valid = vapply(
      unmatched_pdf_paths,
      is_pdf_signature_valid,
      logical(1),
      USE.NAMES = FALSE
    ),

    checksum_md5 = vapply(
      unmatched_pdf_paths,
      safe_md5,
      character(1),
      USE.NAMES = FALSE
    ),

    note = rep(
      "PDF filename does not match any expected screening_id-based filename.",
      length(
        unmatched_pdf_paths
      )
    )
  )
}


# ============================================================
# 11. Build authoritative TASK-010 registry
# ============================================================

registry <- registry_base |>
  dplyr::mutate(
    pdf_expected_filename = pdf_expected_filename,
    pdf_status = pdf_status,
    pdf_path = expected_pdf_paths,
    pdf_file_size_bytes = pdf_file_size,
    pdf_modified_at = pdf_modified_at,
    pdf_signature_valid = pdf_signature_valid,
    pdf_checksum_md5 = pdf_checksum_md5,
    intake_status = intake_status,
    intake_scanned_at = scan_time,
    intake_notes = NA_character_
  )


# ============================================================
# 12. Registry validation
# ============================================================

validation_errors <- character(0)


if (nrow(registry) != nrow(handoff)) {

  validation_errors <- c(
    validation_errors,
    "Registry row count does not match canonical TASK-008 handoff."
  )
}


if (anyDuplicated(registry$screening_id) > 0L) {

  validation_errors <- c(
    validation_errors,
    "Registry contains duplicated screening_id values."
  )
}


if (anyDuplicated(registry$pdf_expected_filename) > 0L) {

  validation_errors <- c(
    validation_errors,
    "Registry contains duplicated expected PDF filenames."
  )
}


invalid_available_rows <- registry[
  registry$pdf_status == "Available" &
    (
      is.na(registry$pdf_file_size_bytes) |
        registry$pdf_file_size_bytes <= 0 |
        !registry$pdf_signature_valid |
        is.na(registry$pdf_checksum_md5) |
        !nzchar(registry$pdf_checksum_md5)
    ),
  ,
  drop = FALSE
]

if (nrow(invalid_available_rows) > 0L) {

  validation_errors <- c(
    validation_errors,
    "One or more PDFs marked Available failed file validation."
  )
}


invalid_missing_rows <- registry[
  registry$pdf_status == "Missing" &
    file.exists(registry$pdf_path),
  ,
  drop = FALSE
]

if (nrow(invalid_missing_rows) > 0L) {

  validation_errors <- c(
    validation_errors,
    "One or more PDFs marked Missing actually exist."
  )
}


if (length(validation_errors) > 0L) {

  stop(
    "TASK-010 registry validation failed:\n- ",
    paste(
      validation_errors,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}


# ============================================================
# 13. Save canonical CSV outputs
# ============================================================

readr::write_csv(
  registry,
  registry_csv,
  na = ""
)


if (
  !file.exists(registry_csv) ||
  is.na(file.info(registry_csv)$size) ||
  file.info(registry_csv)$size <= 0
) {

  stop(
    "Full-text registry CSV was not created successfully.",
    call. = FALSE
  )
}


readr::write_csv(
  unmatched_pdf_data,
  unmatched_pdf_csv,
  na = ""
)


if (
  !file.exists(unmatched_pdf_csv) ||
  is.na(file.info(unmatched_pdf_csv)$size)
) {

  stop(
    "Unmatched-PDF audit CSV was not created successfully.",
    call. = FALSE
  )
}


# ============================================================
# 14. Build registry workbook
# ============================================================

workbook <- openxlsx::createWorkbook(
  creator = "Universal SR-MA Agent Toolkit"
)


# ------------------------------------------------------------
# 14.1 Styles
# ------------------------------------------------------------

title_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fontSize = 14,
  halign = "left",
  valign = "center"
)

section_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fontSize = 11,
  halign = "left",
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


# ------------------------------------------------------------
# 14.2 Instructions
# ------------------------------------------------------------

openxlsx::addWorksheet(
  workbook,
  "Instructions"
)

instructions <- tibble::tibble(
  Section = c(
    "Purpose",
    "Canonical input",
    "PDF folder",
    "Filename rule",
    "How to add a PDF",
    "Available",
    "Missing",
    "Invalid PDF",
    "Unmatched PDFs",
    "Eligibility boundary",
    "Final Included Studies",
    "Rerun behavior"
  ),

  Instruction = c(
    paste(
      "This workbook is the TASK-010 full-text registry and PDF intake record.",
      "It does not perform full-text eligibility screening."
    ),

    "The authoritative input is included_for_full_text_review.csv from TASK-008/R/21.",

    paste0(
      "Place acquired full-text PDFs in: ",
      pdf_directory
    ),

    paste(
      "Each PDF must use the deterministic screening_id-based filename shown in",
      "pdf_expected_filename."
    ),

    paste(
      "Download or otherwise lawfully obtain the article PDF, rename it exactly",
      "to pdf_expected_filename, place it in the PDF folder, and rerun R/22."
    ),

    paste(
      "Available means the expected file exists, is non-empty, begins with a valid",
      "%PDF- signature, and has an MD5 checksum."
    ),

    paste(
      "Missing means the expected PDF is not currently present.",
      "Missing status is an acquisition state, not an eligibility exclusion."
    ),

    paste(
      "Invalid PDF means a file exists at the expected path but is empty or does",
      "not have a valid PDF signature."
    ),

    paste(
      "PDF files in the intake directory that do not match an expected filename",
      "are listed separately for manual reconciliation."
    ),

    paste(
      "Do not make Include/Exclude decisions in TASK-010.",
      "Full-text eligibility belongs to TASK-011/TASK-012."
    ),

    paste(
      "The authoritative Included Studies dataset is created only after",
      "full-text eligibility is completed."
    ),

    paste(
      "R/22 is idempotent. Rerunning it rescans the PDF directory and rebuilds",
      "the registry from the canonical TASK-008 handoff."
    )
  )
)


openxlsx::writeData(
  workbook,
  "Instructions",
  paste0(
    project_title,
    " — Full-Text Registry & PDF Intake"
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
  cols = 1
)

openxlsx::writeData(
  workbook,
  "Instructions",
  instructions,
  startRow = 3,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE
)

openxlsx::addStyle(
  workbook,
  "Instructions",
  header_style,
  rows = 3,
  cols = 1:2,
  gridExpand = TRUE
)

openxlsx::addStyle(
  workbook,
  "Instructions",
  wrap_style,
  rows = 4:(nrow(instructions) + 3),
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
  widths = 90
)


# ------------------------------------------------------------
# 14.3 Registry
# ------------------------------------------------------------

openxlsx::addWorksheet(
  workbook,
  "Full_Text_Registry"
)

openxlsx::writeData(
  workbook,
  "Full_Text_Registry",
  paste0(
    project_title,
    " — Full-Text Registry"
  ),
  startRow = 1,
  startCol = 1
)

if (ncol(registry) > 1L) {

  openxlsx::mergeCells(
    workbook,
    "Full_Text_Registry",
    cols = 1:ncol(registry),
    rows = 1
  )
}

openxlsx::addStyle(
  workbook,
  "Full_Text_Registry",
  title_style,
  rows = 1,
  cols = 1
)

openxlsx::writeData(
  workbook,
  "Full_Text_Registry",
  registry,
  startRow = 3,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE,
  withFilter = TRUE
)

openxlsx::addStyle(
  workbook,
  "Full_Text_Registry",
  header_style,
  rows = 3,
  cols = seq_len(
    ncol(registry)
  ),
  gridExpand = TRUE
)

openxlsx::addStyle(
  workbook,
  "Full_Text_Registry",
  wrap_style,
  rows = 4:(nrow(registry) + 3),
  cols = seq_len(
    ncol(registry)
  ),
  gridExpand = TRUE
)

openxlsx::freezePane(
  workbook,
  "Full_Text_Registry",
  firstActiveRow = 4,
  firstActiveCol = 2
)

openxlsx::setColWidths(
  workbook,
  "Full_Text_Registry",
  cols = seq_len(
    ncol(registry)
  ),
  widths = "auto"
)


for (
  wide_column in intersect(
    c(
      "title",
      "abstract",
      "authors",
      "url",
      "pdf_path",
      "intake_notes"
    ),
    names(registry)
  )
) {

  column_number <- which(
    names(registry) == wide_column
  )

  width_value <- if (
    wide_column == "abstract"
  ) {
    80
  } else if (
    wide_column %in% c(
      "title",
      "pdf_path"
    )
  ) {
    55
  } else {
    40
  }

  openxlsx::setColWidths(
    workbook,
    "Full_Text_Registry",
    cols = column_number,
    widths = width_value
  )
}


# ------------------------------------------------------------
# 14.4 Missing / invalid PDFs
# ------------------------------------------------------------

openxlsx::addWorksheet(
  workbook,
  "PDFs_Needing_Action"
)

pdfs_needing_action <- registry |>
  dplyr::filter(
    pdf_status != "Available"
  ) |>
  dplyr::select(
    screening_id,
    title,
    authors,
    year,
    journal,
    doi,
    pmid,
    url,
    pdf_expected_filename,
    pdf_status,
    pdf_path,
    intake_status
  )


openxlsx::writeData(
  workbook,
  "PDFs_Needing_Action",
  paste0(
    project_title,
    " — PDFs Needing Action"
  ),
  startRow = 1,
  startCol = 1
)

if (ncol(pdfs_needing_action) > 1L) {

  openxlsx::mergeCells(
    workbook,
    "PDFs_Needing_Action",
    cols = 1:ncol(pdfs_needing_action),
    rows = 1
  )
}

openxlsx::addStyle(
  workbook,
  "PDFs_Needing_Action",
  title_style,
  rows = 1,
  cols = 1
)

openxlsx::writeData(
  workbook,
  "PDFs_Needing_Action",
  pdfs_needing_action,
  startRow = 3,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE,
  withFilter = TRUE
)

openxlsx::addStyle(
  workbook,
  "PDFs_Needing_Action",
  header_style,
  rows = 3,
  cols = seq_len(
    ncol(pdfs_needing_action)
  ),
  gridExpand = TRUE
)

if (nrow(pdfs_needing_action) > 0L) {

  openxlsx::addStyle(
    workbook,
    "PDFs_Needing_Action",
    wrap_style,
    rows = 4:(nrow(pdfs_needing_action) + 3),
    cols = seq_len(
      ncol(pdfs_needing_action)
    ),
    gridExpand = TRUE
  )
}

openxlsx::freezePane(
  workbook,
  "PDFs_Needing_Action",
  firstActiveRow = 4,
  firstActiveCol = 2
)

openxlsx::setColWidths(
  workbook,
  "PDFs_Needing_Action",
  cols = seq_len(
    ncol(pdfs_needing_action)
  ),
  widths = "auto"
)

if ("title" %in% names(pdfs_needing_action)) {

  openxlsx::setColWidths(
    workbook,
    "PDFs_Needing_Action",
    cols = which(
      names(pdfs_needing_action) == "title"
    ),
    widths = 55
  )
}


# ------------------------------------------------------------
# 14.5 Unmatched PDFs
# ------------------------------------------------------------

openxlsx::addWorksheet(
  workbook,
  "Unmatched_PDFs"
)

openxlsx::writeData(
  workbook,
  "Unmatched_PDFs",
  paste0(
    project_title,
    " — Unmatched PDFs"
  ),
  startRow = 1,
  startCol = 1
)

if (ncol(unmatched_pdf_data) > 1L) {

  openxlsx::mergeCells(
    workbook,
    "Unmatched_PDFs",
    cols = 1:ncol(unmatched_pdf_data),
    rows = 1
  )
}

openxlsx::addStyle(
  workbook,
  "Unmatched_PDFs",
  title_style,
  rows = 1,
  cols = 1
)

openxlsx::writeData(
  workbook,
  "Unmatched_PDFs",
  unmatched_pdf_data,
  startRow = 3,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE,
  withFilter = TRUE
)

openxlsx::addStyle(
  workbook,
  "Unmatched_PDFs",
  header_style,
  rows = 3,
  cols = seq_len(
    ncol(unmatched_pdf_data)
  ),
  gridExpand = TRUE
)

if (nrow(unmatched_pdf_data) > 0L) {

  openxlsx::addStyle(
    workbook,
    "Unmatched_PDFs",
    wrap_style,
    rows = 4:(nrow(unmatched_pdf_data) + 3),
    cols = seq_len(
      ncol(unmatched_pdf_data)
    ),
    gridExpand = TRUE
  )
}

openxlsx::setColWidths(
  workbook,
  "Unmatched_PDFs",
  cols = seq_len(
    ncol(unmatched_pdf_data)
  ),
  widths = "auto"
)


# ------------------------------------------------------------
# 14.6 Intake Summary
# ------------------------------------------------------------

openxlsx::addWorksheet(
  workbook,
  "Intake_Summary"
)


available_count <- sum(
  registry$pdf_status == "Available"
)

missing_count <- sum(
  registry$pdf_status == "Missing"
)

invalid_count <- sum(
  registry$pdf_status == "Invalid PDF"
)

unmatched_count <- nrow(
  unmatched_pdf_data
)

pdf_acquisition_complete <- (
  available_count == nrow(registry) &&
    missing_count == 0L &&
    invalid_count == 0L
)

task_010_registry_complete <- (
  nrow(registry) == nrow(handoff) &&
    !anyDuplicated(registry$screening_id) &&
    !anyDuplicated(registry$pdf_expected_filename) &&
    all(registry$pdf_status %in% c("Available", "Missing", "Invalid PDF")) &&
    all(!is.na(registry$intake_status) & nzchar(registry$intake_status))
)


intake_summary <- tibble::tibble(
  Metric = c(
    "Project ID",
    "Total records expected for full-text review",
    "Valid PDFs available",
    "Missing expected PDFs",
    "Invalid expected PDFs",
    "Unmatched PDFs in intake directory",
    "PDF acquisition complete",
    "TASK-010 registry/intake status",
    "Scan timestamp"
  ),

  Value = c(
    project_id,
    as.character(nrow(registry)),
    as.character(available_count),
    as.character(missing_count),
    as.character(invalid_count),
    as.character(unmatched_count),
    if (pdf_acquisition_complete) "YES" else "NO",
    if (task_010_registry_complete) "PASS" else "FAIL",
    scan_time
  )
)


openxlsx::writeData(
  workbook,
  "Intake_Summary",
  paste0(
    project_title,
    " — Full-Text Intake Summary"
  ),
  startRow = 1,
  startCol = 1
)

openxlsx::mergeCells(
  workbook,
  "Intake_Summary",
  cols = 1:2,
  rows = 1
)

openxlsx::addStyle(
  workbook,
  "Intake_Summary",
  title_style,
  rows = 1,
  cols = 1
)

openxlsx::writeData(
  workbook,
  "Intake_Summary",
  intake_summary,
  startRow = 3,
  startCol = 1,
  colNames = TRUE,
  rowNames = FALSE
)

openxlsx::addStyle(
  workbook,
  "Intake_Summary",
  header_style,
  rows = 3,
  cols = 1:2,
  gridExpand = TRUE
)

openxlsx::setColWidths(
  workbook,
  "Intake_Summary",
  cols = 1,
  widths = 48
)

openxlsx::setColWidths(
  workbook,
  "Intake_Summary",
  cols = 2,
  widths = 60
)


# ============================================================
# 15. Save and validate workbook
# ============================================================

openxlsx::saveWorkbook(
  workbook,
  registry_workbook,
  overwrite = TRUE
)


if (
  !file.exists(registry_workbook) ||
  is.na(file.info(registry_workbook)$size) ||
  file.info(registry_workbook)$size <= 0
) {

  stop(
    "Full-text registry workbook was not created successfully.",
    call. = FALSE
  )
}


saved_sheets <- openxlsx::getSheetNames(
  registry_workbook
)

required_sheets <- c(
  "Instructions",
  "Full_Text_Registry",
  "PDFs_Needing_Action",
  "Unmatched_PDFs",
  "Intake_Summary"
)

missing_sheets <- setdiff(
  required_sheets,
  saved_sheets
)

if (length(missing_sheets) > 0L) {

  stop(
    "The saved full-text registry workbook is missing sheet(s):\n- ",
    paste(
      missing_sheets,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}


saved_registry <- openxlsx::readWorkbook(
  registry_workbook,
  sheet = "Full_Text_Registry",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)


if (nrow(saved_registry) != nrow(registry)) {

  stop(
    "Saved Full_Text_Registry row count does not match the authoritative registry.",
    call. = FALSE
  )
}


if (
  !"screening_id" %in% names(saved_registry) ||
  !identical(
    as.character(
      saved_registry$screening_id
    ),
    as.character(
      registry$screening_id
    )
  )
) {

  stop(
    "Saved Full_Text_Registry screening IDs do not match the authoritative registry.",
    call. = FALSE
  )
}


# ============================================================
# 16. Final TASK-010 intake status
# ============================================================

available_count <- sum(
  registry$pdf_status == "Available"
)

missing_count <- sum(
  registry$pdf_status == "Missing"
)

invalid_count <- sum(
  registry$pdf_status == "Invalid PDF"
)

unmatched_count <- nrow(
  unmatched_pdf_data
)

pdf_acquisition_complete <- (
  available_count == nrow(registry) &&
    missing_count == 0L &&
    invalid_count == 0L
)

task_010_registry_complete <- (
  nrow(registry) == nrow(handoff) &&
    !anyDuplicated(registry$screening_id) &&
    !anyDuplicated(registry$pdf_expected_filename) &&
    all(registry$pdf_status %in% c("Available", "Missing", "Invalid PDF")) &&
    all(!is.na(registry$intake_status) & nzchar(registry$intake_status))
)


task_status <- if (task_010_registry_complete) "PASS" else "FAIL"


# ============================================================
# 17. Write audit/report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "TASK-010 — Full-Text Registry & PDF Intake Report",
  "",
  paste0(
    "Generated at: ",
    scan_time
  ),
  paste0(
    "Project ID: ",
    project_id
  ),
  paste0(
    "Project title: ",
    project_title
  ),
  "",
  "CANONICAL INPUT",
  "--------------------------------------------------",
  paste0(
    "TASK-008 handoff: ",
    normalize_project_path(
      input_file,
      must_work = TRUE
    )
  ),
  paste0(
    "Records received: ",
    nrow(handoff)
  ),
  "",
  "TASK-010 DIRECTORIES",
  "--------------------------------------------------",
  paste0(
    "PDF intake directory: ",
    pdf_directory
  ),
  paste0(
    "Processed full-text directory: ",
    processed_full_text_directory
  ),
  paste0(
    "Full-text results directory: ",
    full_text_results_directory
  ),
  "",
  "PDF INTAKE",
  "--------------------------------------------------",
  paste0(
    "Expected full-text PDFs: ",
    nrow(registry)
  ),
  paste0(
    "Valid PDFs available: ",
    available_count
  ),
  paste0(
    "Missing expected PDFs: ",
    missing_count
  ),
  paste0(
    "Invalid expected PDFs: ",
    invalid_count
  ),
  paste0(
    "Unmatched PDFs: ",
    unmatched_count
  ),
  paste0(
    "PDF acquisition complete: ",
    if (pdf_acquisition_complete) "YES" else "NO"
  ),
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0(
    "Registry CSV: ",
    registry_csv
  ),
  paste0(
    "Registry workbook: ",
    registry_workbook
  ),
  paste0(
    "Unmatched-PDF audit CSV: ",
    unmatched_pdf_csv
  ),
  paste0(
    "Intake report: ",
    intake_report
  ),
  "",
  "VALIDATION",
  "--------------------------------------------------",
  "Canonical handoff validation: PASS",
  "One registry row per screening_id: PASS",
  "Expected PDF filename uniqueness: PASS",
  "Available PDF file validation: PASS",
  "Missing PDFs explicitly recorded: PASS",
  "Invalid PDF files explicitly recorded: PASS",
  "Workbook structure validation: PASS",
  "Eligibility decision performed: NO",
  "Runner integration performed: NO",
  "",
  "TASK STATUS",
  "--------------------------------------------------",
  paste0(
    "TASK-010: ",
    task_status
  )
)


if (!pdf_acquisition_complete) {

  report_lines <- c(
    report_lines,
    "",
    "PDF ACQUISITION NOTE",
    "--------------------------------------------------",
    paste(
      "TASK-010 can PASS even though PDF acquisition is incomplete.",
      "Missing/invalid PDF states are preserved in the registry for downstream",
      "full-text eligibility handling. Obtain additional PDFs when needed and",
      "rerun R/22 to refresh the acquisition state."
    )
  )
}


writeLines(
  report_lines,
  con = intake_report,
  useBytes = TRUE
)


if (
  !file.exists(intake_report) ||
  is.na(file.info(intake_report)$size) ||
  file.info(intake_report)$size <= 0
) {

  stop(
    "TASK-010 intake report was not created successfully.",
    call. = FALSE
  )
}


# ============================================================
# 18. Console output
# ============================================================

cat("\n")
cat("============================================================\n")
cat("TASK-010 — Full-Text Registry & PDF Intake\n")
cat("============================================================\n\n")

cat(
  "Canonical records received:",
  nrow(registry),
  "\n"
)

cat(
  "Valid PDFs available:",
  available_count,
  "\n"
)

cat(
  "Missing PDFs:",
  missing_count,
  "\n"
)

cat(
  "Invalid PDFs:",
  invalid_count,
  "\n"
)

cat(
  "Unmatched PDFs:",
  unmatched_count,
  "\n\n"
)

cat(
  "PDF intake directory:\n- ",
  pdf_directory,
  "\n\n",
  sep = ""
)

cat(
  "Registry CSV:\n- ",
  registry_csv,
  "\n",
  sep = ""
)

cat(
  "Registry workbook:\n- ",
  registry_workbook,
  "\n",
  sep = ""
)

cat(
  "Report:\n- ",
  intake_report,
  "\n\n",
  sep = ""
)


if (task_010_registry_complete) {

  cat(
    "PASS: TASK-010 Full-Text Registry & PDF Intake completed successfully.\n"
  )

  if (pdf_acquisition_complete) {
    cat("All expected full-text PDFs are present and validated.\n")
  } else {
    cat(
      "PDF acquisition is incomplete, but all candidate records are registered ",
      "and their acquisition states are explicitly recorded.\n",
      sep = ""
    )
    cat("Missing PDFs:", missing_count, "| Invalid PDFs:", invalid_count, "\n")
  }

  cat(
    "Next PM-gated stage: TASK-011 — Multimodal Full-Text Eligibility.\n"
  )

} else {

  cat(
    "FAIL: TASK-010 registry/intake validation did not complete successfully.\n"
  )
}


invisible(
  list(
    status = task_status,
    pdf_acquisition_complete = pdf_acquisition_complete,
    registry = registry,
    registry_csv = registry_csv,
    registry_workbook = registry_workbook,
    pdf_directory = pdf_directory,
    unmatched_pdf_csv = unmatched_pdf_csv,
    report = intake_report
  )
)
