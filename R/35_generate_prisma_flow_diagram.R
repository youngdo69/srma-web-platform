# ============================================================
# Universal SR-MA Agent Toolkit
# R/35_generate_prisma_flow_diagram.R
#
# VERSION: UNIVERSAL-R35-2026-09-25D
#
# PURPOSE
# ------------------------------------------------------------
# Create a publication-ready PRISMA 2020 flow diagram by
# automatically importing canonical R/16-R/24 records.
#
# NON-NEGOTIABLE RULE
# ------------------------------------------------------------
# - Manual count input is prohibited.
# - prisma_counts.csv is NOT required.
# - Counts are derived from row-level canonical files.
# - Full-text exclusion reasons are derived automatically.
# - Arithmetic inconsistencies cause FAIL.
#
# CANONICAL INPUTS
# ------------------------------------------------------------
# Search / deduplication:
#   all_database_records_before_deduplication.csv
#   deduplicated_records_for_screening.csv
#   removed_duplicate_records.csv
#
# Screening:
#   final_title_abstract_screening.csv
#   included_for_full_text_review.csv
#
# Full-text:
#   full_text_registry.csv
#   final_full_text_eligibility.csv
#   excluded_after_full_text.csv
#   included_studies.csv
#
# OUTPUTS
# ------------------------------------------------------------
# results/prisma/
#   prisma_2020_flow_diagram.pdf
#   prisma_2020_flow_diagram.png
#   prisma_2020_flow_diagram.svg
#   prisma_flow_audit.csv
#   prisma_full_text_exclusion_reasons.csv
#   prisma_source_registry.csv
#   r35_prisma_report.txt
# ============================================================


# ============================================================
# 0. Required packages
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "grid"
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
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}


# ============================================================
# 1. Project root
# ============================================================

repo_root <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

normalize_slash <- function(x) {
  stringr::str_replace_all(
    as.character(x),
    "\\\\",
    "/"
  )
}


# ============================================================
# 2. Canonical-file discovery
# ============================================================

# Search the whole repository so R/35 works whether the active project keeps
# canonical data at repo-root/data/... or projects/<project>/data/....
#
# Canonical paths are preferred over results, backups, archives, and old runs.

path_score <- function(path, expected_subdir = NULL) {

  p <- tolower(
    normalize_slash(path)
  )

  score <- 0L

  if (grepl("/data/processed/", p, fixed = TRUE)) {
    score <- score + 100L
  }

  if (!is.null(expected_subdir) &&
      grepl(
        paste0("/data/processed/", expected_subdir, "/"),
        p,
        fixed = TRUE
      )) {
    score <- score + 50L
  }

  if (grepl("/projects/", p, fixed = TRUE)) {
    score <- score + 10L
  }

  if (grepl("/results/", p, fixed = TRUE)) {
    score <- score - 30L
  }

  if (grepl(
    "/archive/|/archived/|/backup/|/old/|/tmp/|/temp/|additional_analysis",
    p,
    perl = TRUE
  )) {
    score <- score - 100L
  }

  score
}


find_canonical_file <- function(
  filename,
  expected_subdir = NULL
) {

  hits <- list.files(
    repo_root,
    pattern = paste0(
      "^",
      gsub(
        "([.])",
        "\\\\\\1",
        filename
      ),
      "$"
    ),
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = FALSE
  )

  hits <- unique(
    normalize_slash(hits)
  )

  if (length(hits) == 0L) {
    stop(
      "Required canonical file not found: ",
      filename,
      call. = FALSE
    )
  }

  scores <- vapply(
    hits,
    path_score,
    expected_subdir = expected_subdir,
    FUN.VALUE = integer(1)
  )

  best_score <- max(scores)
  best_hits <- hits[scores == best_score]

  # Prefer the shortest canonical path if multiple copies have equal score.
  # This avoids selecting nested derivative copies.
  chosen <- best_hits[
    which.min(
      nchar(best_hits)
    )
  ]

  chosen
}


required_file_specs <- tibble::tribble(
  ~logical_name, ~filename, ~expected_subdir,
  "all_database_records", "all_database_records_before_deduplication.csv", "search",
  "deduplicated_records", "deduplicated_records_for_screening.csv", "search",
  "removed_duplicates", "removed_duplicate_records.csv", "search",
  "final_title_abstract_screening", "final_title_abstract_screening.csv", "screening",
  "included_for_full_text", "included_for_full_text_review.csv", "screening",
  "full_text_registry", "full_text_registry.csv", "full_text",
  "final_full_text_eligibility", "final_full_text_eligibility.csv", "full_text",
  "excluded_after_full_text", "excluded_after_full_text.csv", "full_text",
  "included_studies", "included_studies.csv", "full_text"
)

source_registry <- required_file_specs |>
  dplyr::rowwise() |>
  dplyr::mutate(
    path = find_canonical_file(
      filename,
      expected_subdir
    )
  ) |>
  dplyr::ungroup()


# ============================================================
# 3. Read canonical records
# ============================================================

safe_read_csv <- function(path) {

  readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE,
    guess_max = 10000
  )
}

read_source <- function(name) {

  path <- source_registry$path[
    source_registry$logical_name == name
  ]

  if (length(path) != 1L) {
    stop(
      "Source registry resolution failed for: ",
      name,
      call. = FALSE
    )
  }

  safe_read_csv(
    path[[1]]
  )
}

all_db <- read_source(
  "all_database_records"
)

deduplicated <- read_source(
  "deduplicated_records"
)

removed_duplicates <- read_source(
  "removed_duplicates"
)

ta_final <- read_source(
  "final_title_abstract_screening"
)

full_text_handoff <- read_source(
  "included_for_full_text"
)

full_text_registry <- read_source(
  "full_text_registry"
)

full_text_final <- read_source(
  "final_full_text_eligibility"
)

excluded_full_text <- read_source(
  "excluded_after_full_text"
)

included_studies <- read_source(
  "included_studies"
)


# ============================================================
# 4. Required-column validation
# ============================================================

require_columns <- function(
  df,
  required,
  source_name
) {

  missing <- setdiff(
    required,
    names(df)
  )

  if (length(missing) > 0L) {
    stop(
      source_name,
      " is missing required column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
}

require_columns(
  all_db,
  c("database"),
  "all_database_records_before_deduplication.csv"
)

require_columns(
  ta_final,
  c("final_decision"),
  "final_title_abstract_screening.csv"
)

require_columns(
  full_text_final,
  c("final_decision"),
  "final_full_text_eligibility.csv"
)

require_columns(
  excluded_full_text,
  c("final_exclusion_reason"),
  "excluded_after_full_text.csv"
)

require_columns(
  included_studies,
  c("final_decision"),
  "included_studies.csv"
)


# ============================================================
# 5. Automatically derive PRISMA counts
# ============================================================

# Identification counts come directly from row-level search records.
database_counts <- all_db |>
  dplyr::mutate(
    database = trimws(
      as.character(database)
    ),
    database = dplyr::if_else(
      is.na(database) |
        database == "",
      "Unknown database",
      database
    )
  ) |>
  dplyr::count(
    database,
    name = "count"
  ) |>
  dplyr::arrange(
    database
  )

records_identified_n <- nrow(
  all_db
)

duplicates_removed_n <- nrow(
  removed_duplicates
)

records_after_dedup_n <- nrow(
  deduplicated
)

# TASK-022 PRISMA LEGACY FULL-TEXT BRIDGE
# ------------------------------------------------------------
# Older R/21 versions could retain records as
# 'Awaiting full-text clarification' in the authoritative
# title/abstract final dataset while routing only literal
# final_decision == 'Include' rows into the full-text handoff.
#
# If those legacy awaiting records are absent from both the
# handoff and final_full_text_eligibility.csv, R/35 treats them
# transparently as legacy human full-text exclusions rather
# than falsely classifying them as reports not retrieved.
#
# The final included count remains authoritative from R/24
# included_studies.csv.

records_screened_n <- nrow(
  ta_final
)

normalize_prisma_decision_key <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x[is.na(x)] <- ""
  x <- gsub("[^a-z]+", " ", x)
  trimws(gsub("[[:space:]]+", " ", x))
}

if (!"screening_id" %in% names(ta_final)) {
  stop(
    "PRISMA compatibility validation requires screening_id in final_title_abstract_screening.csv.",
    call. = FALSE
  )
}

if (!"screening_id" %in% names(full_text_handoff)) {
  stop(
    "PRISMA compatibility validation requires screening_id in included_for_full_text_review.csv.",
    call. = FALSE
  )
}

if (!"screening_id" %in% names(full_text_final)) {
  stop(
    "PRISMA compatibility validation requires screening_id in final_full_text_eligibility.csv.",
    call. = FALSE
  )
}

ta_decision_key <- normalize_prisma_decision_key(
  ta_final$final_decision
)

ta_ids <- trimws(as.character(ta_final$screening_id))
handoff_ids <- unique(
  trimws(as.character(full_text_handoff$screening_id))
)
full_text_final_ids <- unique(
  trimws(as.character(full_text_final$screening_id))
)

records_excluded_n <- sum(
  ta_decision_key == "exclude",
  na.rm = TRUE
)

legacy_awaiting_mask <-
  ta_decision_key == "awaiting full text clarification" &
  !(ta_ids %in% handoff_ids) &
  !(ta_ids %in% full_text_final_ids)

legacy_awaiting_ids <- unique(
  ta_ids[legacy_awaiting_mask]
)
legacy_awaiting_ids <- legacy_awaiting_ids[
  !is.na(legacy_awaiting_ids) & nzchar(legacy_awaiting_ids)
]

legacy_full_text_excluded_n <- length(
  legacy_awaiting_ids
)

nonexclude_ids <- unique(
  ta_ids[
    nzchar(ta_decision_key) &
      ta_decision_key != "exclude"
  ]
)

accounted_nonexclude_ids <- unique(
  c(
    handoff_ids,
    legacy_awaiting_ids
  )
)

unexplained_nonexclude_ids <- setdiff(
  nonexclude_ids,
  accounted_nonexclude_ids
)

if (length(unexplained_nonexclude_ids) > 0L) {
  stop(
    paste0(
      "PRISMA title/abstract routing contains ",
      length(unexplained_nonexclude_ids),
      " non-Exclude record(s) that are neither in the canonical ",
      "full-text handoff nor recognized as legacy Awaiting full-text clarification records. ",
      "Example screening_id(s): ",
      paste(head(unexplained_nonexclude_ids, 10L), collapse = ", ")
    ),
    call. = FALSE
  )
}

reports_sought_n <-
  nrow(full_text_handoff) +
  legacy_full_text_excluded_n

reports_assessed_n <-
  nrow(full_text_final) +
  legacy_full_text_excluded_n

reports_not_retrieved_n <-
  reports_sought_n -
  reports_assessed_n

if (reports_not_retrieved_n < 0L) {
  stop(
    "PRISMA inconsistency: reports assessed exceeds reports sought.",
    call. = FALSE
  )
}

full_text_decision_key <- normalize_prisma_decision_key(
  full_text_final$final_decision
)

canonical_full_text_excluded_n <- sum(
  full_text_decision_key == "exclude",
  na.rm = TRUE
)

reports_excluded_n <-
  canonical_full_text_excluded_n +
  legacy_full_text_excluded_n

studies_included_n <- nrow(
  included_studies
)

if (legacy_full_text_excluded_n > 0L) {
  cat("\n")
  cat("PRISMA legacy full-text compatibility bridge\n")
  cat("------------------------------------------------------------\n")
  cat(
    "Legacy Awaiting full-text clarification records treated as human full-text exclusions: ",
    legacy_full_text_excluded_n,
    "\n",
    sep = ""
  )
  cat(
    "These records are NOT counted as reports not retrieved.\n"
  )
  cat(
    "Final included studies remain authoritative from included_studies.csv: ",
    studies_included_n,
    "\n\n",
    sep = ""
  )
}

# ============================================================
# 6. Full-text exclusion reasons
# ============================================================

reason_counts <- excluded_full_text |>
  dplyr::mutate(
    final_exclusion_reason = trimws(
      as.character(
        final_exclusion_reason
      )
    )
  ) |>
  dplyr::filter(
    !is.na(final_exclusion_reason),
    nzchar(final_exclusion_reason)
  ) |>
  dplyr::count(
    final_exclusion_reason,
    name = "count"
  ) |>
  dplyr::arrange(
    dplyr::desc(count),
    final_exclusion_reason
  )

if (legacy_full_text_excluded_n > 0L) {
  legacy_reason_label <- paste(
    "Legacy human full-text exclusion",
    "(individual reason not captured in canonical artifact)"
  )

  reason_counts <- dplyr::bind_rows(
    reason_counts,
    tibble::tibble(
      final_exclusion_reason = legacy_reason_label,
      count = legacy_full_text_excluded_n
    )
  ) |>
    dplyr::group_by(
      final_exclusion_reason
    ) |>
    dplyr::summarise(
      count = sum(count),
      .groups = "drop"
    ) |>
    dplyr::arrange(
      dplyr::desc(count),
      final_exclusion_reason
    )
}

# ============================================================
# 7. Arithmetic validation — FAIL on inconsistency
# ============================================================

validation_checks <- tibble::tibble(
  check = c(
    "identified - duplicates = after deduplication",
    "after deduplication = title/abstract records",
    "screened - excluded = full-text handoff",
    "reports sought = assessed + not retrieved",
    "assessed - full-text excluded = included",
    "full-text exclusion reasons sum to full-text excluded"
  ),
  lhs = c(
    records_identified_n - duplicates_removed_n,
    records_after_dedup_n,
    records_screened_n - records_excluded_n,
    reports_sought_n,
    reports_assessed_n - reports_excluded_n,
    sum(reason_counts$count)
  ),
  rhs = c(
    records_after_dedup_n,
    records_screened_n,
    reports_sought_n,
    reports_assessed_n + reports_not_retrieved_n,
    studies_included_n,
    reports_excluded_n
  )
) |>
  dplyr::mutate(
    pass = lhs == rhs
  )

if (
  reports_not_retrieved_n < 0L ||
  any(
    !validation_checks$pass
  )
) {

  failed <- validation_checks |>
    dplyr::filter(
      !pass
    )

  stop(
    paste0(
      "PRISMA arithmetic validation failed.\n",
      paste(
        paste0(
          failed$check,
          ": ",
          failed$lhs,
          " != ",
          failed$rhs
        ),
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 8. Optional retrieval-status audit
# ============================================================

metadata_only_n <- NA_integer_
missing_pdf_n <- NA_integer_

if (
  "agent_evidence_source" %in% names(
    full_text_final
  )
) {

  metadata_only_n <- sum(
    tolower(
      trimws(
        as.character(
          full_text_final$agent_evidence_source
        )
      )
    ) == "metadata only",
    na.rm = TRUE
  )
}

if (
  "pdf_status" %in% names(
    full_text_final
  )
) {

  missing_pdf_n <- sum(
    tolower(
      trimws(
        as.character(
          full_text_final$pdf_status
        )
      )
    ) == "missing",
    na.rm = TRUE
  )
}


# ============================================================
# 9. Output directory
# ============================================================

output_dir <- file.path(
  repo_root,
  "results",
  "prisma"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

pdf_file <- file.path(
  output_dir,
  "prisma_2020_flow_diagram.pdf"
)

png_file <- file.path(
  output_dir,
  "prisma_2020_flow_diagram.png"
)

svg_file <- file.path(
  output_dir,
  "prisma_2020_flow_diagram.svg"
)

audit_file <- file.path(
  output_dir,
  "prisma_flow_audit.csv"
)

reason_file <- file.path(
  output_dir,
  "prisma_full_text_exclusion_reasons.csv"
)

source_registry_file <- file.path(
  output_dir,
  "prisma_source_registry.csv"
)

report_file <- file.path(
  output_dir,
  "r35_prisma_report.txt"
)


# ============================================================
# 10. Save audit files
# ============================================================

audit_df <- tibble::tibble(
  item = c(
    "Records identified",
    "Duplicate records removed",
    "Records after deduplication",
    "Records screened",
    "Records excluded at title/abstract",
    "Reports sought for retrieval",
    "Reports not retrieved",
    "Reports assessed for eligibility",
    "Reports excluded after eligibility assessment",
    "Studies included in review",
    "Metadata-only eligibility assessments",
    "Missing PDF at final eligibility"
  ),
  count = c(
    records_identified_n,
    duplicates_removed_n,
    records_after_dedup_n,
    records_screened_n,
    records_excluded_n,
    reports_sought_n,
    reports_not_retrieved_n,
    reports_assessed_n,
    reports_excluded_n,
    studies_included_n,
    metadata_only_n,
    missing_pdf_n
  )
)

readr::write_csv(
  audit_df,
  audit_file,
  na = ""
)

readr::write_csv(
  reason_counts,
  reason_file,
  na = ""
)

readr::write_csv(
  source_registry |>
    dplyr::select(
      logical_name,
      filename,
      path
    ),
  source_registry_file,
  na = ""
)


# ============================================================
# 11. Diagram text helpers
# ============================================================

wrap_text <- function(
  x,
  width = 38
) {

  paste(
    strwrap(
      as.character(x),
      width = width
    ),
    collapse = "\n"
  )
}


database_text <- paste(
  paste0(
    database_counts$database,
    " (n = ",
    database_counts$count,
    ")"
  ),
  collapse = "\n"
)

identification_text <- paste0(
  "Records identified through database search\n",
  database_text,
  "\n\nTotal (n = ",
  records_identified_n,
  ")"
)

removed_text <- paste0(
  "Records removed before screening\n",
  "Duplicate records removed (n = ",
  duplicates_removed_n,
  ")"
)

screened_text <- paste0(
  "Records screened\n",
  "(n = ",
  records_screened_n,
  ")"
)

screen_excluded_text <- paste0(
  "Records excluded\n",
  "(n = ",
  records_excluded_n,
  ")"
)

sought_text <- paste0(
  "Reports sought for retrieval\n",
  "(n = ",
  reports_sought_n,
  ")"
)

not_retrieved_text <- paste0(
  "Reports not retrieved\n",
  "(n = ",
  reports_not_retrieved_n,
  ")"
)

assessed_text <- paste0(
  "Reports assessed for eligibility\n",
  "(n = ",
  reports_assessed_n,
  ")"
)

reason_lines <- paste0(
  reason_counts$final_exclusion_reason,
  " (n = ",
  reason_counts$count,
  ")"
)

excluded_text <- paste0(
  "Reports excluded:\n",
  paste(
    reason_lines,
    collapse = "\n"
  ),
  "\n\nTotal (n = ",
  reports_excluded_n,
  ")"
)

included_text <- paste0(
  "Studies included in review\n",
  "(n = ",
  studies_included_n,
  ")"
)


# ============================================================
# 12. Publication-style PRISMA drawing functions
# ============================================================

col_phase <- "#63B7AD"
col_box <- "#E8EDF3"
col_edge <- "#49586B"
col_text <- "#263444"

draw_rect <- function(
  x,
  y,
  width,
  height,
  fill,
  border = col_edge,
  lwd = 1.2
) {

  grid::grid.rect(
    x = grid::unit(x, "npc"),
    y = grid::unit(y, "npc"),
    width = grid::unit(width, "npc"),
    height = grid::unit(height, "npc"),
    just = c("left", "bottom"),
    gp = grid::gpar(
      fill = fill,
      col = border,
      lwd = lwd
    )
  )
}


draw_text_box <- function(
  x,
  y,
  width,
  height,
  text,
  fontsize = 10,
  wrap_width = 40
) {

  draw_rect(
    x,
    y,
    width,
    height,
    fill = col_box
  )

  wrapped <- paste(
    vapply(
      strsplit(
        text,
        "\n",
        fixed = TRUE
      )[[1]],
      function(z) {
        if (trimws(z) == "") {
          return("")
        }

        wrap_text(
          z,
          width = wrap_width
        )
      },
      character(1)
    ),
    collapse = "\n"
  )

  grid::grid.text(
    wrapped,
    x = grid::unit(
      x + 0.018,
      "npc"
    ),
    y = grid::unit(
      y + height / 2,
      "npc"
    ),
    just = c(
      "left",
      "center"
    ),
    gp = grid::gpar(
      fontsize = fontsize,
      col = col_text,
      lineheight = 1.15
    )
  )
}


draw_phase <- function(
  x,
  y,
  width,
  height,
  label
) {

  draw_rect(
    x,
    y,
    width,
    height,
    fill = col_phase
  )

  grid::grid.text(
    label,
    x = grid::unit(
      x + width / 2,
      "npc"
    ),
    y = grid::unit(
      y + height / 2,
      "npc"
    ),
    rot = 90,
    gp = grid::gpar(
      fontsize = 10.5,
      col = "white",
      fontface = "bold"
    )
  )
}


draw_arrow <- function(
  x1,
  y1,
  x2,
  y2
) {

  grid::grid.lines(
    x = grid::unit(
      c(
        x1,
        x2
      ),
      "npc"
    ),
    y = grid::unit(
      c(
        y1,
        y2
      ),
      "npc"
    ),
    arrow = grid::arrow(
      type = "closed",
      length = grid::unit(
        2.4,
        "mm"
      )
    ),
    gp = grid::gpar(
      col = col_edge,
      lwd = 1.2
    )
  )
}


draw_prisma <- function() {

  grid::grid.newpage()

  # Phase bars
  draw_phase(
    0.035,
    0.77,
    0.055,
    0.18,
    "Identification"
  )

  draw_phase(
    0.035,
    0.49,
    0.055,
    0.25,
    "Screening"
  )

  draw_phase(
    0.035,
    0.22,
    0.055,
    0.23,
    "Eligibility"
  )

  # Universal terminal-lane geometry:
  # Keep the terminal included node vertically centred on the Included phase
  # bar regardless of project counts or exclusion-box height.
  included_phase_y <- 0.065
  included_phase_height <- 0.115
  included_box_height <- 0.095
  included_box_y <- included_phase_y +
    (included_phase_height - included_box_height) / 2

  draw_phase(
    0.035,
    included_phase_y,
    0.055,
    included_phase_height,
    "Included"
  )

  # Identification
  draw_text_box(
    0.14,
    0.79,
    0.35,
    0.145,
    identification_text,
    fontsize = 9.6,
    wrap_width = 40
  )

  draw_text_box(
    0.60,
    0.81,
    0.30,
    0.105,
    removed_text,
    fontsize = 9.6,
    wrap_width = 34
  )

  # Screening
  draw_text_box(
    0.34,
    0.64,
    0.32,
    0.105,
    screened_text,
    fontsize = 10,
    wrap_width = 34
  )

  draw_text_box(
    0.70,
    0.64,
    0.25,
    0.105,
    screen_excluded_text,
    fontsize = 10,
    wrap_width = 29
  )

  draw_text_box(
    0.34,
    0.50,
    0.32,
    0.105,
    sought_text,
    fontsize = 10,
    wrap_width = 34
  )

  draw_text_box(
    0.70,
    0.515,
    0.25,
    0.075,
    not_retrieved_text,
    fontsize = 9.7,
    wrap_width = 29
  )

  # Eligibility
  draw_text_box(
    0.34,
    0.35,
    0.32,
    0.105,
    assessed_text,
    fontsize = 10,
    wrap_width = 34
  )

  # Dynamic exclusion-box height based on number of reasons
  reason_n <- nrow(
    reason_counts
  )

  reason_height <- min(
    0.235,
    max(
      0.16,
      0.09 + reason_n * 0.018
    )
  )

  reason_y <- 0.385 -
    reason_height / 2

  draw_text_box(
    0.69,
    reason_y,
    0.285,
    reason_height,
    excluded_text,
    fontsize = if (
      reason_n <= 6L
    ) {
      8.8
    } else {
      8.2
    },
    wrap_width = 36
  )

  # Included
  # The terminal node is centred on the Included phase lane rather than
  # positioned by a project-specific absolute y coordinate.
  draw_text_box(
    0.34,
    included_box_y,
    0.32,
    included_box_height,
    included_text,
    fontsize = 10.2,
    wrap_width = 34
  )

  # Arrows
  draw_arrow(
    0.49,
    0.862,
    0.595,
    0.862
  )

  draw_arrow(
    0.50,
    0.79,
    0.50,
    0.752
  )

  draw_arrow(
    0.66,
    0.692,
    0.695,
    0.692
  )

  draw_arrow(
    0.50,
    0.64,
    0.50,
    0.61
  )

  draw_arrow(
    0.66,
    0.552,
    0.695,
    0.552
  )

  draw_arrow(
    0.50,
    0.50,
    0.50,
    0.458
  )

  draw_arrow(
    0.66,
    0.402,
    0.685,
    0.402
  )

  draw_arrow(
    0.50,
    0.35,
    0.50,
    included_box_y + included_box_height + 0.003
  )


}


# ============================================================
# 13. Render PDF / PNG / SVG
# ============================================================

render_with_device <- function(
  output_path,
  device = c("pdf", "png", "svg")
) {

  device <- match.arg(device)

  if (device == "pdf") {

    grDevices::pdf(
      file = output_path,
      width = 8.5,
      height = 10.8,
      useDingbats = FALSE,
      onefile = TRUE
    )

  } else if (device == "png") {

    png_type <- if (capabilities("cairo")) {
      "cairo"
    } else {
      getOption("bitmapType")
    }

    grDevices::png(
      filename = output_path,
      width = 2550,
      height = 3240,
      units = "px",
      res = 300,
      bg = "white",
      type = png_type
    )

  } else if (device == "svg") {

    if (!capabilities("cairo")) {
      return(FALSE)
    }

    grDevices::svg(
      filename = output_path,
      width = 8.5,
      height = 10.8,
      bg = "white"
    )
  }

  tryCatch(
    {
      draw_prisma()
      grDevices::dev.off()
    },
    error = function(e) {

      if (grDevices::dev.cur() > 1L) {
        try(
          grDevices::dev.off(),
          silent = TRUE
        )
      }

      stop(
        paste0(
          "Failed to render PRISMA ",
          toupper(device),
          ": ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  if (
    !file.exists(output_path) ||
    is.na(file.info(output_path)$size) ||
    file.info(output_path)$size <= 0
  ) {
    stop(
      paste0(
        "PRISMA ",
        toupper(device),
        " was not created correctly: ",
        normalize_slash(output_path)
      ),
      call. = FALSE
    )
  }

  TRUE
}


pdf_created <- render_with_device(
  pdf_file,
  "pdf"
)

png_created <- render_with_device(
  png_file,
  "png"
)

svg_created <- tryCatch(
  {
    render_with_device(
      svg_file,
      "svg"
    )
  },
  error = function(e) {

    warning(
      paste0(
        "SVG creation skipped/failed: ",
        conditionMessage(e)
      ),
      call. = FALSE
    )

    FALSE
  }
)

if (
  !isTRUE(pdf_created) ||
  !isTRUE(png_created)
) {
  stop(
    "PRISMA publication image generation failed.",
    call. = FALSE
  )
}


# ============================================================
# 14. Report
# ============================================================

database_report_lines <- paste0(
  "  ",
  database_counts$database,
  ": ",
  database_counts$count
)

validation_report_lines <- paste0(
  "  ",
  validation_checks$check,
  ": PASS"
)

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/35 — PRISMA 2020 Flow Diagram",
  "",
  "SOURCE POLICY",
  "--------------------------------------------------",
  "Manual count input: PROHIBITED",
  "prisma_counts.csv: NOT REQUIRED",
  "Counts derived from canonical row-level R/16-R/24 records.",
  "",
  "DATABASE IDENTIFICATION",
  "--------------------------------------------------",
  database_report_lines,
  paste0(
    "  Total: ",
    records_identified_n
  ),
  "",
  "PRISMA COUNTS",
  "--------------------------------------------------",
  paste0(
    "Duplicates removed: ",
    duplicates_removed_n
  ),
  paste0(
    "Records screened: ",
    records_screened_n
  ),
  paste0(
    "Records excluded: ",
    records_excluded_n
  ),
  paste0(
    "Reports sought for retrieval: ",
    reports_sought_n
  ),
  paste0(
    "Reports not retrieved: ",
    reports_not_retrieved_n
  ),
  paste0(
    "Reports assessed for eligibility: ",
    reports_assessed_n
  ),
  paste0(
    "Reports excluded: ",
    reports_excluded_n
  ),
  paste0(
    "Studies included: ",
    studies_included_n
  ),
  "",
  "ARITHMETIC VALIDATION",
  "--------------------------------------------------",
  validation_report_lines,
  "",
  "LAYOUT CONTRACT",
  "--------------------------------------------------",
  "  Terminal included node: vertically centred on the Included phase lane",
  "",
  "OPTIONAL RETRIEVAL AUDIT",
  "--------------------------------------------------",
  paste0(
    "Metadata-only eligibility assessments: ",
    ifelse(
      is.na(metadata_only_n),
      "NA",
      metadata_only_n
    )
  ),
  paste0(
    "Missing PDF at final eligibility: ",
    ifelse(
      is.na(missing_pdf_n),
      "NA",
      missing_pdf_n
    )
  ),
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0(
    "PDF: ",
    normalize_slash(pdf_file),
    " [",
    ifelse(pdf_created, "CREATED", "FAILED"),
    "]"
  ),
  paste0(
    "PNG: ",
    normalize_slash(png_file),
    " [",
    ifelse(png_created, "CREATED", "FAILED"),
    "]"
  ),
  paste0(
    "SVG: ",
    normalize_slash(svg_file),
    " [",
    ifelse(svg_created, "CREATED", "SKIPPED"),
    "]"
  ),
  "",
  "PASS: R/35 generated the PRISMA 2020 flow diagram automatically."
)

writeLines(
  report_lines,
  report_file
)


# ============================================================
# 15. Console summary
# ============================================================

cat("============================================================\n")
cat("R/35 — PRISMA 2020 Flow Diagram\n")
cat("============================================================\n\n")

cat(
  "Manual count input: PROHIBITED\n"
)

cat(
  "prisma_counts.csv: NOT REQUIRED\n\n"
)

cat(
  "Records identified: ",
  records_identified_n,
  "\n",
  sep = ""
)

for (
  i in seq_len(
    nrow(
      database_counts
    )
  )
) {

  cat(
    "  ",
    database_counts$database[[i]],
    ": ",
    database_counts$count[[i]],
    "\n",
    sep = ""
  )
}

cat(
  "Duplicates removed: ",
  duplicates_removed_n,
  "\n",
  sep = ""
)

cat(
  "Records screened: ",
  records_screened_n,
  "\n",
  sep = ""
)

cat(
  "Records excluded: ",
  records_excluded_n,
  "\n",
  sep = ""
)

cat(
  "Reports sought: ",
  reports_sought_n,
  "\n",
  sep = ""
)

cat(
  "Reports not retrieved: ",
  reports_not_retrieved_n,
  "\n",
  sep = ""
)

cat(
  "Reports assessed: ",
  reports_assessed_n,
  "\n",
  sep = ""
)

cat(
  "Reports excluded: ",
  reports_excluded_n,
  "\n",
  sep = ""
)

cat(
  "Studies included: ",
  studies_included_n,
  "\n\n",
  sep = ""
)

cat(
  "PDF: ",
  normalize_slash(pdf_file),
  " [",
  ifelse(pdf_created, "CREATED", "FAILED"),
  "]\n",
  sep = ""
)

cat(
  "PNG: ",
  normalize_slash(png_file),
  " [",
  ifelse(png_created, "CREATED", "FAILED"),
  "]\n",
  sep = ""
)

cat(
  "SVG: ",
  normalize_slash(svg_file),
  " [",
  ifelse(svg_created, "CREATED", "SKIPPED"),
  "]\n\n",
  sep = ""
)

cat(
  "PASS: R/35 generated the PRISMA 2020 flow diagram automatically.\n"
)
