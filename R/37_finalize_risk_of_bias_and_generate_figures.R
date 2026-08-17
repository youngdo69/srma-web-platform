# ============================================================
# Universal SR-MA Agent Toolkit
# R/37_generate_risk_of_bias_figures.R
#
# VERSION: UNIVERSAL-R37-2026-08-16G
#
# PURPOSE
# ------------------------------------------------------------
# Finalize author-reviewed risk-of-bias judgements and generate
# publication-ready risk-of-bias figures:
#
#   (A) Traffic-light plot by study and domain
#   (B) Domain-level stacked summary plot
#
# Separate figures are created for:
#   - RoB 2 (2019) randomized trials
#   - ROBINS-I (2016) non-randomized studies
#
# MANUAL COUNT INPUT IS NOT USED.
# Figures are derived from final author judgements only.
#
# INPUT / FINALIZATION POLICY
# ------------------------------------------------------------
# Primary source:
#   results/risk_of_bias/risk_of_bias_author_editable.xlsx
#
# Required sheets:
#   Domain_Review
#   Overall_Review
#   Study_Result_Manifest
#
# R/37 validates that the FIRST AUTHOR has completed every required
# final domain and overall judgement. Only then does it:
#
#   1. export rob2_final.csv / robins_i_final.csv
#   2. export combined final RoB data
#   3. generate traffic-light and summary figures
#
# Existing final CSVs are used only as a fallback when the author workbook
# is unavailable. The workbook is authoritative whenever it exists.
#
# IMPORTANT
# ------------------------------------------------------------
# - Provisional Agent judgements are NOT promoted automatically.
# - Author-final domain judgements are required.
# - Author-final overall judgements are required.
# - If final judgements are incomplete, R/37 FAILS.
#
# OUTPUTS
# ------------------------------------------------------------
# results/risk_of_bias/figures/
#   rob2_traffic_light_and_summary.pdf
#   rob2_traffic_light_and_summary.png
#   rob2_traffic_light.pdf
#   rob2_traffic_light.png
#   rob2_summary.pdf
#   rob2_summary.png
#   robins_i_traffic_light_and_summary.pdf
#   robins_i_traffic_light_and_summary.png
#   robins_i_traffic_light.pdf
#   robins_i_traffic_light.png
#   robins_i_summary.pdf
#   robins_i_summary.png
#   rob_figure_data.csv
#   rob_figure_validation_report.txt
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tidyr",
  "tibble",
  "stringr",
  "ggplot2",
  "openxlsx",
  "patchwork",
  "scales",
  "purrr"
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
# 1. Root / output
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

rob_dir <- file.path(
  repo_root,
  "results",
  "risk_of_bias"
)

figure_dir <- file.path(
  rob_dir,
  "figures"
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

rob2_final_file <- file.path(
  rob_dir,
  "rob2_final.csv"
)

robins_final_file <- file.path(
  rob_dir,
  "robins_i_final.csv"
)

author_workbook <- file.path(
  rob_dir,
  "risk_of_bias_author_editable.xlsx"
)

final_combined_file <- file.path(
  rob_dir,
  "risk_of_bias_final.csv"
)

figure_data_file <- file.path(
  figure_dir,
  "rob_figure_data.csv"
)

validation_report_file <- file.path(
  figure_dir,
  "rob_figure_validation_report.txt"
)


# ============================================================
# 2. Helpers
# ============================================================

clean_chr <- function(x) {

  x <- trimws(
    as.character(x)
  )

  x[
    is.na(x) |
      x == "" |
      tolower(x) %in% c(
        "na",
        "n/a",
        "null"
      )
  ] <- NA_character_

  x
}


first_existing_col <- function(
  df,
  candidates
) {

  hit <- candidates[
    candidates %in% names(df)
  ]

  if (length(hit) == 0L) {
    return(NA_character_)
  }

  hit[[1]]
}


canonicalize_judgement <- function(
  x,
  tool
) {

  x <- clean_chr(x)

  x_lower <- tolower(
    x
  )

  if (tool == "RoB 2") {

    out <- dplyr::case_when(
      x_lower %in% c(
        "low",
        "low risk",
        "low risk of bias"
      ) ~ "Low",
      x_lower %in% c(
        "some concerns",
        "some concern",
        "concerns"
      ) ~ "Some concerns",
      x_lower %in% c(
        "high",
        "high risk",
        "high risk of bias"
      ) ~ "High",
      TRUE ~ NA_character_
    )

  } else {

    out <- dplyr::case_when(
      x_lower %in% c(
        "low",
        "low risk",
        "low risk of bias"
      ) ~ "Low",
      x_lower %in% c(
        "moderate",
        "moderate risk",
        "moderate risk of bias"
      ) ~ "Moderate",
      x_lower %in% c(
        "serious",
        "serious risk",
        "serious risk of bias"
      ) ~ "Serious",
      x_lower %in% c(
        "critical",
        "critical risk",
        "critical risk of bias"
      ) ~ "Critical",
      x_lower %in% c(
        "no information",
        "no info",
        "insufficient information"
      ) ~ "No information",
      TRUE ~ NA_character_
    )
  }

  out
}


strip_first_author <- function(x) {

  x <- clean_chr(x)

  x <- stringr::str_replace_all(
    x,
    stringr::regex(
      "\\s+et\\s+al\\.?\\s*",
      ignore_case = TRUE
    ),
    " "
  )

  x <- stringr::str_split_fixed(
    x,
    ";",
    2
  )[, 1]

  x <- stringr::str_split_fixed(
    x,
    ",",
    2
  )[, 1]

  x <- stringr::str_replace(
    x,
    "\\s+(?:[A-Z]\\.?){1,5}$",
    ""
  )

  x <- stringr::str_replace(
    x,
    "\\s+[A-Z]{1,5}$",
    ""
  )

  stringr::str_squish(
    x
  )
}


extract_year <- function(x) {

  stringr::str_extract(
    as.character(x),
    "(?:19|20)[0-9]{2}"
  )
}


# ============================================================
# 3. Study-label discovery
# ============================================================

discover_study_labels <- function(
  screening_ids
) {

  candidate_files <- list.files(
    repo_root,
    pattern = "included_studies\\.csv$|final_full_text_eligibility\\.csv$|full_text_registry\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = FALSE
  )

  candidate_files <- unique(
    normalize_slash(
      candidate_files
    )
  )

  rows <- list()

  for (
    path_i in candidate_files
  ) {

    df <- tryCatch(
      readr::read_csv(
        path_i,
        show_col_types = FALSE,
        progress = FALSE
      ),
      error = function(e) {
        NULL
      }
    )

    if (
      is.null(df) ||
      !"screening_id" %in% names(df)
    ) {
      next
    }

    author_col <- first_existing_col(
      df,
      c(
        "authors",
        "author",
        "first_author",
        "first_author_name"
      )
    )

    year_col <- first_existing_col(
      df,
      c(
        "year",
        "publication_year",
        "pub_year"
      )
    )

    if (
      is.na(author_col) ||
      is.na(year_col)
    ) {
      next
    }

    rows[[length(rows) + 1L]] <- df |>
      dplyr::transmute(
        screening_id = as.character(
          screening_id
        ),
        first_author = strip_first_author(
          .data[[author_col]]
        ),
        year = extract_year(
          .data[[year_col]]
        )
      )
  }

  if (length(rows) == 0L) {

    return(
      tibble::tibble(
        screening_id = screening_ids,
        study_label = screening_ids
      )
    )
  }

  label_pool <- dplyr::bind_rows(
    rows
  ) |>
    dplyr::filter(
      screening_id %in% screening_ids
    ) |>
    dplyr::mutate(
      study_label = dplyr::if_else(
        !is.na(first_author) &
          first_author != "" &
          !is.na(year) &
          year != "",
        paste(
          first_author,
          year
        ),
        screening_id
      )
    ) |>
    dplyr::distinct(
      screening_id,
      .keep_all = TRUE
    ) |>
    dplyr::select(
      screening_id,
      study_label
    )

  missing_ids <- setdiff(
    screening_ids,
    label_pool$screening_id
  )

  if (length(missing_ids) > 0L) {

    label_pool <- dplyr::bind_rows(
      label_pool,
      tibble::tibble(
        screening_id = missing_ids,
        study_label = missing_ids
      )
    )
  }

  label_pool
}


# ============================================================
# 4. Tool domain definitions
# ============================================================

rob2_domains <- tibble::tribble(
  ~domain_id, ~domain_short, ~domain_full,
  "D1", "D1", "Randomization process",
  "D2", "D2", "Deviations from intended interventions",
  "D3", "D3", "Missing outcome data",
  "D4", "D4", "Measurement of the outcome",
  "D5", "D5", "Selection of the reported result",
  "Overall", "Overall", "Overall risk of bias"
)

robins_domains <- tibble::tribble(
  ~domain_id, ~domain_short, ~domain_full,
  "D1", "D1", "Bias due to confounding",
  "D2", "D2", "Bias in selection of participants",
  "D3", "D3", "Bias in classification of interventions",
  "D4", "D4", "Bias due to deviations from intended interventions",
  "D5", "D5", "Bias due to missing data",
  "D6", "D6", "Bias in measurement of outcomes",
  "D7", "D7", "Bias in selection of the reported result",
  "Overall", "Overall", "Overall risk of bias"
)


# ============================================================
# 5. Import and finalize author-reviewed judgements
# ============================================================

standardize_final_csv <- function(
  path,
  tool
) {

  df <- readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )

  screening_col <- first_existing_col(
    df,
    c(
      "screening_id",
      "study_id"
    )
  )

  domain_col <- first_existing_col(
    df,
    c(
      "domain_id",
      "domain"
    )
  )

  judgement_col <- first_existing_col(
    df,
    c(
      "author_final_domain_judgement",
      "author_domain_judgement",
      "final_domain_judgement",
      "final_judgement",
      "judgement"
    )
  )

  if (
    is.na(screening_col) ||
    is.na(domain_col) ||
    is.na(judgement_col)
  ) {
    stop(
      "Final RoB CSV lacks required study/domain/judgement columns: ",
      normalize_slash(path),
      call. = FALSE
    )
  }

  df |>
    dplyr::transmute(
      screening_id = as.character(
        .data[[screening_col]]
      ),
      domain_id = as.character(
        .data[[domain_col]]
      ),
      judgement = canonicalize_judgement(
        .data[[judgement_col]],
        tool
      ),
      rationale = NA_character_,
      rob_tool = tool,
      source = "existing_final_csv"
    )
}


import_and_finalize_author_workbook <- function(
  workbook
) {

  required_sheets <- c(
    "Domain_Review",
    "Overall_Review",
    "Study_Result_Manifest"
  )

  available_sheets <- openxlsx::getSheetNames(
    workbook
  )

  missing_sheets <- setdiff(
    required_sheets,
    available_sheets
  )

  if (length(missing_sheets) > 0L) {
    stop(
      "Author workbook is missing required sheet(s): ",
      paste(
        missing_sheets,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  domain_review <- openxlsx::read.xlsx(
    workbook,
    sheet = "Domain_Review",
    check.names = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
      )
    )

  overall_review <- openxlsx::read.xlsx(
    workbook,
    sheet = "Overall_Review",
    check.names = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
      )
    )

  manifest <- openxlsx::read.xlsx(
    workbook,
    sheet = "Study_Result_Manifest",
    check.names = FALSE
  ) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        as.character
      )
    )

  required_domain_cols <- c(
    "screening_id",
    "rob_tool",
    "domain_id",
    "author_domain_judgement"
  )

  missing_domain_cols <- setdiff(
    required_domain_cols,
    names(domain_review)
  )

  if (length(missing_domain_cols) > 0L) {
    stop(
      "Domain_Review is missing required column(s): ",
      paste(
        missing_domain_cols,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  required_overall_cols <- c(
    "screening_id",
    "author_final_overall_judgement"
  )

  missing_overall_cols <- setdiff(
    required_overall_cols,
    names(overall_review)
  )

  if (length(missing_overall_cols) > 0L) {
    stop(
      "Overall_Review is missing required column(s): ",
      paste(
        missing_overall_cols,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  if (!"rob_tool" %in% names(manifest)) {
    stop(
      "Study_Result_Manifest is missing rob_tool.",
      call. = FALSE
    )
  }

  manifest_tool <- manifest |>
    dplyr::transmute(
      screening_id = as.character(
        screening_id
      ),
      rob_tool_manifest = dplyr::case_when(
        grepl(
          "RoB 2",
          rob_tool,
          fixed = TRUE
        ) ~ "RoB 2",
        grepl(
          "ROBINS-I",
          rob_tool,
          fixed = TRUE
        ) ~ "ROBINS-I",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::distinct()

  domain_final <- domain_review |>
    dplyr::mutate(
      tool = dplyr::case_when(
        grepl(
          "RoB 2",
          rob_tool,
          fixed = TRUE
        ) ~ "RoB 2",
        grepl(
          "ROBINS-I",
          rob_tool,
          fixed = TRUE
        ) ~ "ROBINS-I",
        TRUE ~ NA_character_
      ),
      judgement = purrr::map2_chr(
        author_domain_judgement,
        tool,
        canonicalize_judgement
      ),
      rationale = if (
        "author_domain_rationale" %in% names(domain_review)
      ) {
        clean_chr(
          author_domain_rationale
        )
      } else {
        NA_character_
      }
    ) |>
    dplyr::transmute(
      screening_id = as.character(
        screening_id
      ),
      domain_id = as.character(
        domain_id
      ),
      judgement,
      rationale,
      rob_tool = tool,
      source = "author_workbook"
    )

  overall_final <- overall_review |>
    dplyr::left_join(
      manifest_tool,
      by = "screening_id"
    ) |>
    dplyr::mutate(
      judgement = purrr::map2_chr(
        author_final_overall_judgement,
        rob_tool_manifest,
        canonicalize_judgement
      ),
      rationale = if (
        "author_final_overall_rationale" %in% names(overall_review)
      ) {
        clean_chr(
          author_final_overall_rationale
        )
      } else {
        NA_character_
      }
    ) |>
    dplyr::transmute(
      screening_id = as.character(
        screening_id
      ),
      domain_id = "Overall",
      judgement,
      rationale,
      rob_tool = rob_tool_manifest,
      source = "author_workbook"
    )

  final_data <- dplyr::bind_rows(
    domain_final,
    overall_final
  )

  incomplete <- final_data |>
    dplyr::filter(
      is.na(judgement) |
        judgement == ""
    )

  if (nrow(incomplete) > 0L) {
    stop(
      paste0(
        "Author review is incomplete. Complete all final RoB judgements in ",
        basename(workbook),
        " before running R/37. Incomplete rows: ",
        paste(
          paste0(
            incomplete$screening_id,
            "/",
            incomplete$domain_id
          ),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  final_data
}


author_workbook_used <- FALSE

if (file.exists(author_workbook)) {

  final_data <- import_and_finalize_author_workbook(
    author_workbook
  )

  author_workbook_used <- TRUE

} else {

  final_parts <- list()

  if (file.exists(rob2_final_file)) {

    final_parts[[length(final_parts) + 1L]] <- standardize_final_csv(
      rob2_final_file,
      "RoB 2"
    )
  }

  if (file.exists(robins_final_file)) {

    final_parts[[length(final_parts) + 1L]] <- standardize_final_csv(
      robins_final_file,
      "ROBINS-I"
    )
  }

  if (length(final_parts) == 0L) {
    stop(
      paste0(
        "No author-review workbook or final RoB CSVs were found. Expected: ",
        normalize_slash(author_workbook)
      ),
      call. = FALSE
    )
  }

  final_data <- dplyr::bind_rows(
    final_parts
  )
}


# Export finalized author-reviewed data BEFORE plotting.
readr::write_csv(
  final_data,
  final_combined_file,
  na = ""
)

rob2_final_export <- final_data |>
  dplyr::filter(
    rob_tool == "RoB 2"
  )

robins_final_export <- final_data |>
  dplyr::filter(
    rob_tool == "ROBINS-I"
  )

if (nrow(rob2_final_export) > 0L) {
  readr::write_csv(
    rob2_final_export,
    rob2_final_file,
    na = ""
  )
}

if (nrow(robins_final_export) > 0L) {
  readr::write_csv(
    robins_final_export,
    robins_final_file,
    na = ""
  )
}


# ============================================================
# 6. Final-data validation
# ============================================================

final_data <- final_data |>
  dplyr::filter(
    !is.na(screening_id),
    screening_id != "",
    !is.na(domain_id),
    domain_id != ""
  )

if (
  any(
    is.na(
      final_data$judgement
    )
  )
) {

  bad <- final_data |>
    dplyr::filter(
      is.na(judgement)
    )

  stop(
    paste0(
      "Final RoB judgements are incomplete or use unsupported labels. ",
      "Incomplete rows: ",
      paste(
        paste0(
          bad$screening_id,
          "/",
          bad$domain_id
        ),
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


validate_tool_domains <- function(
  df,
  tool,
  domain_map
) {

  x <- df |>
    dplyr::filter(
      rob_tool == !!tool
    )

  if (nrow(x) == 0L) {
    return(
      x
    )
  }

  expected_domains <- domain_map$domain_id

  study_check <- x |>
    dplyr::count(
      screening_id,
      domain_id
    )

  duplicates <- study_check |>
    dplyr::filter(
      n != 1L
    )

  if (nrow(duplicates) > 0L) {
    stop(
      tool,
      ": duplicate or missing repeated domain rows detected.",
      call. = FALSE
    )
  }

  missing_grid <- tidyr::expand_grid(
    screening_id = unique(
      x$screening_id
    ),
    domain_id = expected_domains
  ) |>
    dplyr::anti_join(
      x |>
        dplyr::select(
          screening_id,
          domain_id
        ),
      by = c(
        "screening_id",
        "domain_id"
      )
    )

  if (nrow(missing_grid) > 0L) {
    stop(
      tool,
      ": final judgements are incomplete. Missing: ",
      paste(
        paste0(
          missing_grid$screening_id,
          "/",
          missing_grid$domain_id
        ),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  unexpected <- setdiff(
    unique(
      x$domain_id
    ),
    expected_domains
  )

  if (length(unexpected) > 0L) {
    stop(
      tool,
      ": unexpected domain ID(s): ",
      paste(
        unexpected,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  x
}


rob2_data <- validate_tool_domains(
  final_data,
  "RoB 2",
  rob2_domains
)

robins_data <- validate_tool_domains(
  final_data,
  "ROBINS-I",
  robins_domains
)


# ============================================================
# 7. Add publication study labels
# ============================================================

all_ids <- unique(
  final_data$screening_id
)

study_labels <- discover_study_labels(
  all_ids
)

final_data <- final_data |>
  dplyr::left_join(
    study_labels,
    by = "screening_id"
  ) |>
  dplyr::mutate(
    study_label = dplyr::if_else(
      is.na(study_label) |
        study_label == "",
      screening_id,
      study_label
    )
  )

readr::write_csv(
  final_data,
  figure_data_file,
  na = ""
)


# ============================================================
# 8. Plot style definitions
# ============================================================

rob2_palette <- c(
  "Low" = "#19B51E",
  "Some concerns" = "#F2E312",
  "High" = "#D62728"
)

rob2_symbols <- c(
  "Low" = "+",
  "Some concerns" = "!",
  "High" = "×"
)

robins_palette <- c(
  "Low" = "#19B51E",
  "Moderate" = "#F2E312",
  "Serious" = "#F28E2B",
  "Critical" = "#D62728",
  "No information" = "#BDBDBD"
)

robins_symbols <- c(
  "Low" = "+",
  "Moderate" = "−",
  "Serious" = "!",
  "Critical" = "×",
  "No information" = "?"
)


# ============================================================
# 9. Traffic-light plot
# ============================================================

make_traffic_plot <- function(
  df,
  tool,
  domain_map,
  palette,
  symbols
) {

  x <- df |>
    dplyr::filter(
      rob_tool == !!tool
    ) |>
    dplyr::left_join(
      domain_map,
      by = "domain_id"
    )

  if (nrow(x) == 0L) {
    return(NULL)
  }

  study_order <- x |>
    dplyr::filter(
      domain_id == "Overall"
    ) |>
    dplyr::arrange(
      study_label
    ) |>
    dplyr::pull(
      study_label
    )

  x <- x |>
    dplyr::mutate(
      domain_short = factor(
        domain_short,
        levels = domain_map$domain_short
      ),
      study_label = factor(
        study_label,
        levels = rev(
          unique(
            study_order
          )
        )
      ),
      symbol = unname(
        symbols[judgement]
      )
    )

  ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = domain_short,
      y = study_label
    )
  ) +
    ggplot2::geom_tile(
      fill = "#BEBEBE",
      color = "#6E6E6E",
      linewidth = 0.4,
      width = 1.00,
      height = 1.00
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        fill = judgement
      ),
      shape = 21,
      size = 8.0,
      stroke = 0.7,
      color = "#303030"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = symbol
      ),
      size = 5.1,
      fontface = "bold",
      color = "#202020",
      vjust = 0.48
    ) +
    ggplot2::scale_fill_manual(
      values = palette,
      drop = FALSE
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "Risk of bias domains",
      fill = "Judgement"
    ) +
    ggplot2::theme_minimal(
      base_size = 11
    ) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        face = "bold",
        color = "#222222"
      ),
      axis.text.y = ggplot2::element_text(
        color = "#222222",
        size = 10
      ),
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        face = "bold",
        size = 12
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(
        face = "bold"
      ),
      plot.margin = ggplot2::margin(
        8,
        8,
        8,
        8
      )
    )
}


# ============================================================
# 10. Domain-summary plot
# ============================================================

make_summary_plot <- function(
  df,
  tool,
  domain_map,
  palette,
  judgement_order
) {

  x <- df |>
    dplyr::filter(
      rob_tool == !!tool
    ) |>
    dplyr::left_join(
      domain_map,
      by = "domain_id"
    )

  if (nrow(x) == 0L) {
    return(NULL)
  }

  summary_df <- x |>
    dplyr::count(
      domain_id,
      domain_full,
      judgement,
      name = "n"
    ) |>
    dplyr::group_by(
      domain_id,
      domain_full
    ) |>
    dplyr::mutate(
      proportion = n / sum(n)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      judgement = factor(
        judgement,
        levels = judgement_order
      ),
      domain_full = factor(
        domain_full,
        levels = rev(
          domain_map$domain_full
        )
      )
    )

  ggplot2::ggplot(
    summary_df,
    ggplot2::aes(
      x = proportion,
      y = domain_full,
      fill = judgement
    )
  ) +
    ggplot2::geom_col(
      width = 0.72,
      color = "#303030",
      linewidth = 0.35
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(
        accuracy = 1
      ),
      limits = c(
        0,
        1
      ),
      breaks = seq(
        0,
        1,
        by = 0.25
      ),
      expand = c(
        0,
        0
      )
    ) +
    ggplot2::scale_fill_manual(
      values = palette,
      drop = FALSE
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      fill = "Judgement"
    ) +
    ggplot2::theme_minimal(
      base_size = 10.5
    ) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        linewidth = 0.25,
        color = "#D0D0D0"
      ),
      axis.text.y = ggplot2::element_text(
        color = "#222222"
      ),
      axis.text.x = ggplot2::element_text(
        color = "#444444"
      ),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(
        face = "bold"
      ),
      plot.margin = ggplot2::margin(
        8,
        8,
        8,
        8
      )
    )
}


# ============================================================
# 11. Domain legend / footnote panel
# ============================================================

make_domain_key_plot <- function(
  domain_map
) {

  domain_text <- paste0(
    domain_map$domain_short[
      domain_map$domain_id != "Overall"
    ],
    ": ",
    domain_map$domain_full[
      domain_map$domain_id != "Overall"
    ],
    collapse = "\n"
  )

  ggplot2::ggplot() +
    ggplot2::annotate(
      "text",
      x = 0,
      y = 1,
      label = domain_text,
      hjust = 0,
      vjust = 1,
      size = 3.3,
      color = "#222222",
      lineheight = 1.08
    ) +
    ggplot2::xlim(
      0,
      1
    ) +
    ggplot2::ylim(
      0,
      1
    ) +
    ggplot2::theme_void()
}


# ============================================================
# 12. Save plot helper
# ============================================================

save_plot_pair <- function(
  plot,
  stem,
  width,
  height
) {

  pdf_path <- file.path(
    figure_dir,
    paste0(
      stem,
      ".pdf"
    )
  )

  png_path <- file.path(
    figure_dir,
    paste0(
      stem,
      ".png"
    )
  )

  ggplot2::ggsave(
    filename = pdf_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    device = grDevices::cairo_pdf,
    bg = "white"
  )

  ggplot2::ggsave(
    filename = png_path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  if (
    !file.exists(pdf_path) ||
    !file.exists(png_path)
  ) {
    stop(
      "Failed to create RoB figure output: ",
      stem,
      call. = FALSE
    )
  }

  list(
    pdf = pdf_path,
    png = png_path
  )
}


# ============================================================
# 13. Generate RoB 2 figures
# ============================================================

rob2_outputs <- NULL

if (nrow(rob2_data) > 0L) {

  p_rob2_traffic <- make_traffic_plot(
    final_data,
    "RoB 2",
    rob2_domains,
    rob2_palette,
    rob2_symbols
  )

  p_rob2_summary <- make_summary_plot(
    final_data,
    "RoB 2",
    rob2_domains,
    rob2_palette,
    c(
      "Low",
      "Some concerns",
      "High"
    )
  )

  p_rob2_key <- make_domain_key_plot(
    rob2_domains
  )

  n_rob2 <- dplyr::n_distinct(
    rob2_data$screening_id
  )

  traffic_height <- max(
    3.9,
    2.0 + n_rob2 * 0.42
  )

  rob2_combined <- (
    p_rob2_traffic /
      p_rob2_key /
      p_rob2_summary
  ) +
    patchwork::plot_layout(
      heights = c(
        3.5,
        1.4,
        2.1
      )
    ) +
    patchwork::plot_annotation(
      tag_levels = "A"
    )

  rob2_outputs <- list(
    combined = save_plot_pair(
      rob2_combined,
      "rob2_traffic_light_and_summary",
      width = 8.3,
      height = traffic_height + 3.8
    ),
    traffic = save_plot_pair(
      p_rob2_traffic,
      "rob2_traffic_light",
      width = 8.1,
      height = traffic_height
    ),
    summary = save_plot_pair(
      p_rob2_summary,
      "rob2_summary",
      width = 8.8,
      height = 4.7
    )
  )
}


# ============================================================
# 14. Generate ROBINS-I figures
# ============================================================

robins_outputs <- NULL

if (nrow(robins_data) > 0L) {

  p_robins_traffic <- make_traffic_plot(
    final_data,
    "ROBINS-I",
    robins_domains,
    robins_palette,
    robins_symbols
  )

  p_robins_summary <- make_summary_plot(
    final_data,
    "ROBINS-I",
    robins_domains,
    robins_palette,
    c(
      "Low",
      "Moderate",
      "Serious",
      "Critical",
      "No information"
    )
  )

  p_robins_key <- make_domain_key_plot(
    robins_domains
  )

  n_robins <- dplyr::n_distinct(
    robins_data$screening_id
  )

  traffic_height <- max(
    3.9,
    2.0 + n_robins * 0.44
  )

  robins_combined <- (
    p_robins_traffic /
      p_robins_key /
      p_robins_summary
  ) +
    patchwork::plot_layout(
      heights = c(
        3.4,
        1.8,
        2.2
      )
    ) +
    patchwork::plot_annotation(
      tag_levels = "A"
    )

  robins_outputs <- list(
    combined = save_plot_pair(
      robins_combined,
      "robins_i_traffic_light_and_summary",
      width = 8.5,
      height = traffic_height + 4.1
    ),
    traffic = save_plot_pair(
      p_robins_traffic,
      "robins_i_traffic_light",
      width = 8.3,
      height = traffic_height
    ),
    summary = save_plot_pair(
      p_robins_summary,
      "robins_i_summary",
      width = 9.0,
      height = 5.2
    )
  )
}


# ============================================================
# 15. Validation report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/37 — Risk-of-Bias Figures",
  "",
  paste0(
    "RoB 2 studies: ",
    dplyr::n_distinct(
      final_data$screening_id[
        final_data$rob_tool == "RoB 2"
      ]
    )
  ),
  paste0(
    "ROBINS-I studies: ",
    dplyr::n_distinct(
      final_data$screening_id[
        final_data$rob_tool == "ROBINS-I"
      ]
    )
  ),
  "",
  "INPUT / FINALIZATION POLICY",
  "--------------------------------------------------",
  paste0(
    "Author workbook used: ",
    author_workbook_used
  ),
  "Only author-final judgements are finalized and plotted.",
  "Provisional Agent judgements are not promoted automatically.",
  "Final CSVs are exported only after complete author review.",
  "",
  "FINAL DATA OUTPUTS",
  "--------------------------------------------------",
  normalize_slash(
    final_combined_file
  ),
  normalize_slash(
    rob2_final_file
  ),
  normalize_slash(
    robins_final_file
  ),
  "",
  "FIGURE OUTPUT DIRECTORY",
  "--------------------------------------------------",
  normalize_slash(
    figure_dir
  ),
  "",
  "STATUS",
  "--------------------------------------------------",
  "PASS: publication-ready traffic-light and summary plots generated."
)

writeLines(
  report_lines,
  validation_report_file
)


# ============================================================
# 16. Console summary
# ============================================================

cat("============================================================\n")
cat("R/37 — Risk-of-Bias Figures\n")
cat("============================================================\n\n")

cat(
  "RoB 2 studies: ",
  dplyr::n_distinct(
    final_data$screening_id[
      final_data$rob_tool == "RoB 2"
    ]
  ),
  "\n",
  sep = ""
)

cat(
  "ROBINS-I studies: ",
  dplyr::n_distinct(
    final_data$screening_id[
      final_data$rob_tool == "ROBINS-I"
    ]
  ),
  "\n",
  sep = ""
)

cat(
  "Output directory: ",
  normalize_slash(
    figure_dir
  ),
  "\n\n",
  sep = ""
)

cat(
  "PASS: R/37 finalized author-reviewed RoB judgements and generated publication-ready figures.\n"
)
