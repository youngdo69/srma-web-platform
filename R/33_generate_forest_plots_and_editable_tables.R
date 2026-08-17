# ============================================================
# Universal SR-MA Agent Toolkit
# R/33_generate_forest_plots_and_editable_tables.R
#
# REVISION
# ------------------------------------------------------------
# - Compact manuscript-width forest layout and reduce horizontal aspect ratio\n# - Tighten Events/Total, forest, RR, CI, and Weight column spacing\n# - Reduce study-row vertical spacing and figure height\n# - Strip trailing author initials so Study labels are strictly FirstAuthor + Year\n# - Refine dynamic column spacing for Study / Events / Total / effect / CI / weight
# - Use wider output devices for binary-count forest plots
# - Dynamically scale fonts and vertical spacing by number of included studies
# - Show Events / Total columns for each comparison group when binary-count pooling is available
# - Resolve additional-analysis labels via screening_id parsed from source_file
# - Prevent trial acronyms / 'et al.' labels when upstream author metadata is available
# - Standardize forest Study labels strictly as FirstAuthor + Year
# - Remove 'et al.' from all forest-plot study labels
# - Robust union-based discovery: manifest + recursive processed fallback
# - Detect R/29 ready datasets in either results/ or data/processed/
# - Accept version-flexible completion artifacts without hardcoded filenames
# - Discover latest COMPLETED R/29 additional analyses by purpose
# - Preserve analysis purpose in figure titles and filenames
# - Ignore interrupted/older duplicate additional-analysis workspaces
# - Fix row-wise Study label fallback to prevent unnecessary Study_1 labels
# - Prefer final_* / human_verified_* canonical columns over raw fields
# - Normalize RR synonyms and reject numeric effect-measure contamination
# - Support mixed row-level inputs (reported effect + raw dichotomous counts)
# - Recognize intervention_events/intervention_total/comparator_* columns
# - Use current meta arguments (method.random.ci, label.e, label.c)
# - Avoid base graphics::title() after grid-based meta::forest()
# - Prevent duplicate analysis-group figure filenames from overwriting
# - Prefer first-author style study labels and improve plot spacing / colors
# - Support auditable study-label crosswalk (first author + publication year)
# - Add rows below pooled result to prevent heterogeneity/test text overprinting
# - Reduce excess page whitespace while preserving publication-quality spacing
#
# VERSION: UNIVERSAL-R33-2026-08-16H
#
# PURPOSE
# ------------------------------------------------------------
# Create publication-quality forest plots and editable tables
# from the canonical meta-analysis-ready datasets / manifest.
#
# UNIVERSAL TOOLKIT PRINCIPLES
# ------------------------------------------------------------
# - Works from canonical project_config.yml + project root
# - Uses manifest-driven discovery when available
# - Preserves standalone operation
# - Creates reusable / editable outputs (CSV + XLSX)
# - Generates figure files suitable for manuscript drafting
# - Does NOT overwrite core meta-analysis scripts
# - Requires no manually prepared study-label crosswalk
#
# PRIMARY OUTPUTS
# ------------------------------------------------------------
# results/meta_analysis/figures/
#   - forest_<analysis_group>.png
#   - forest_<analysis_group>.pdf
#
# results/meta_analysis/tables/
#   - meta_analysis_summary.csv
#   - study_level_effects.csv
#   - meta_analysis_outputs.xlsx
#
# results/meta_analysis/logs/
#   - r33_forest_and_tables_report.txt
#
# EXPECTED INPUT
# ------------------------------------------------------------
# Preferred:
#   A canonical manifest produced by R/26 / R/27
#
# Fallback:
#   One or more meta-analysis-ready CSV files in common locations
#   whose names contain 'meta_analysis_ready'
# ============================================================


# ============================================================
# 0. Required packages
# ============================================================

required_packages <- c(
  "yaml",
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "purrr",
  "meta",
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
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}


# ============================================================
# 1. Helpers
# ============================================================

normalize_slash <- function(x) {
  stringr::str_replace_all(
    as.character(x),
    "\\\\",
    "/"
  )
}

safe_chr <- function(x, default = "") {
  if (length(x) == 0L || is.null(x) || all(is.na(x))) {
    return(default)
  }
  out <- as.character(x[[1]])
  if (is.na(out)) {
    return(default)
  }
  trimws(out)
}

first_existing_col <- function(data, candidates) {
  hits <- candidates[candidates %in% names(data)]
  if (length(hits) == 0L) {
    return(NA_character_)
  }
  hits[[1]]
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

as_flag <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x %in% c("true", "t", "yes", "y", "1", "pass", "passed")
}

slugify <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  x <- stringr::str_replace_all(x, "^_|_$", "")
  x
}

now_text <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

safe_read_csv <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE
  )
}




normalize_forest_study_label <- function(author_value, year_value = NA_character_) {

  author_value <- trimws(as.character(author_value))
  year_value <- trimws(as.character(year_value))

  # Remove "et al." / "et al" wherever present.
  author_value <- stringr::str_replace_all(
    author_value,
    stringr::regex("\\s+et\\s+al\\.?\\s*", ignore_case = TRUE),
    " "
  )

  # If a citation-like author field still contains multiple authors,
  # retain only the first author.
  author_value <- stringr::str_split_fixed(
    author_value,
    pattern = ";",
    n = 2
  )[, 1]

  author_value <- stringr::str_split_fixed(
    author_value,
    pattern = ",",
    n = 2
  )[, 1]

  # Remove an embedded publication year from the author string so that
  # the output is always formed once as "FirstAuthor Year".
  embedded_year <- stringr::str_extract(
    author_value,
    "(?:19|20)[0-9]{2}"
  )

  if (
    (is.na(year_value) || year_value == "") &&
    !is.na(embedded_year)
  ) {
    year_value <- embedded_year
  }

  author_value <- stringr::str_replace_all(
    author_value,
    "(?:19|20)[0-9]{2}",
    ""
  )

  # Remove common citation punctuation/brackets left behind.
  author_value <- stringr::str_replace_all(
    author_value,
    "[\\(\\)\\[\\]]",
    " "
  )

  author_value <- stringr::str_replace(
    author_value,
    "\\s+(?:[A-Z]\\.?){1,5}$",
    ""
  )

  author_value <- stringr::str_replace(
    author_value,
    "\\s+[A-Z]{1,5}$",
    ""
  )

  author_value <- stringr::str_squish(
    author_value
  )

  if (is.na(author_value) || author_value == "") {
    return(NA_character_)
  }

  if (!is.na(year_value) && year_value != "") {
    return(
      paste(
        author_value,
        year_value
      )
    )
  }

  author_value
}

build_study_labels <- function(df) {

  n <- nrow(df)
  out <- rep(NA_character_, n)

  author_col <- first_existing_col(
    df,
    c(
      "first_author",
      "first_author_name",
      "lead_author",
      "author"
    )
  )

  year_col <- first_existing_col(
    df,
    c(
      "publication_year",
      "pub_year",
      "year"
    )
  )

  author_year_col <- first_existing_col(
    df,
    c(
      "first_author_year",
      "author_year",
      "citation_short",
      "short_citation"
    )
  )

  # Prefer separate canonical author/year fields whenever available.
  if (!is.na(author_col)) {

    author_i <- as.character(
      df[[author_col]]
    )

    year_i <- if (!is.na(year_col)) {
      as.character(
        df[[year_col]]
      )
    } else {
      rep(NA_character_, n)
    }

    for (i in seq_len(n)) {
      out[[i]] <- normalize_forest_study_label(
        author_value = author_i[[i]],
        year_value = year_i[[i]]
      )
    }
  }

  # Fallback to an author-year/citation field, but normalize it to
  # FirstAuthor + Year and remove "et al.".
  if (!is.na(author_year_col)) {

    citation_i <- as.character(
      df[[author_year_col]]
    )

    for (i in seq_len(n)) {

      if (!is.na(out[[i]]) && out[[i]] != "") {
        next
      }

      detected_year <- extract_publication_year(
        citation_i[[i]]
      )

      out[[i]] <- normalize_forest_study_label(
        author_value = citation_i[[i]],
        year_value = detected_year
      )
    }
  }

  # Execution-only fallback. R/33 remains runnable when upstream
  # bibliographic metadata cannot be resolved, but the auto-generated
  # crosswalk will mark those records USER REQUIRED.
  generic_col <- first_existing_col(
    df,
    c(
      "study_label",
      "study_name",
      "trial_name",
      "study",
      "study_code",
      "study_id",
      "screening_id"
    )
  )

  if (!is.na(generic_col)) {

    candidate <- trimws(
      as.character(
        df[[generic_col]]
      )
    )

    candidate <- stringr::str_replace(
      candidate,
      stringr::regex(
        "\\s+et\\s+al\\.?\\b.*$",
        ignore_case = TRUE
      ),
      ""
    )

    candidate <- stringr::str_squish(
      candidate
    )

    use <- (
      is.na(out) |
      out == ""
    ) &
      !is.na(candidate) &
      candidate != ""

    out[use] <- candidate[use]
  }

  empty <- is.na(out) | out == ""

  out[empty] <- paste0(
    "Study_",
    seq_len(n)
  )[empty]

  out
}


load_study_label_crosswalk <- function(repo_root) {

  candidates <- c(
    file.path(
      repo_root,
      "data",
      "processed",
      "meta_analysis",
      "study_label_crosswalk.csv"
    ),
    file.path(
      repo_root,
      "results",
      "meta_analysis",
      "tables",
      "study_label_crosswalk.csv"
    )
  )

  hit <- candidates[file.exists(candidates)]

  if (length(hit) == 0L) {
    return(NULL)
  }

  cw <- safe_read_csv(hit[[1]])

  required <- c(
    "key_type",
    "key_value",
    "first_author",
    "publication_year"
  )

  if (!all(required %in% names(cw))) {
    warning(
      paste0(
        "Study-label crosswalk found but required columns are missing. ",
        "Required: ",
        paste(required, collapse = ", ")
      ),
      call. = FALSE
    )
    return(NULL)
  }

  cw |>
    dplyr::mutate(
      key_type = tolower(trimws(as.character(key_type))),
      key_value = trimws(as.character(key_value)),
      first_author = trimws(as.character(first_author)),
      publication_year = trimws(as.character(publication_year))
    ) |>
    dplyr::filter(
      key_type != "",
      key_value != "",
      first_author != ""
    )
}

apply_study_label_crosswalk <- function(df, crosswalk) {

  if (is.null(crosswalk) || nrow(crosswalk) == 0L) {
    return(df)
  }

  # Create / preserve a canonical display field.
  if (!"first_author_year" %in% names(df)) {
    df$first_author_year <- NA_character_
  }

  key_candidates <- list(
    screening_id = c("screening_id"),
    study_id = c("study_id"),
    study_label = c("study_label"),
    study_code = c("study_code")
  )

  for (key_type_i in names(key_candidates)) {

    cols <- key_candidates[[key_type_i]]
    col_i <- cols[cols %in% names(df)]

    if (length(col_i) == 0L) {
      next
    }

    col_i <- col_i[[1]]

    cw_i <- crosswalk |>
      dplyr::filter(key_type == key_type_i)

    if (nrow(cw_i) == 0L) {
      next
    }

    lookup <- stats::setNames(
      ifelse(
        is.na(cw_i$publication_year) |
          cw_i$publication_year == "",
        cw_i$first_author,
        paste(
          cw_i$first_author,
          cw_i$publication_year
        )
      ),
      cw_i$key_value
    )

    row_keys <- trimws(as.character(df[[col_i]]))
    mapped <- unname(lookup[row_keys])

    use <- (
      is.na(df$first_author_year) |
        trimws(df$first_author_year) == ""
    ) &
      !is.na(mapped) &
      mapped != ""

    df$first_author_year[use] <- mapped[use]
  }


  # If the additional-analysis dataset lacks screening_id but carries
  # source_file/source_document, derive screening_id and use it to resolve
  # FirstAuthor + Year from the automatically generated crosswalk.
  source_id_col <- first_existing_col(
    df,
    c(
      "source_file",
      "source_document",
      "pdf_file",
      "source_location"
    )
  )

  if (!is.na(source_id_col)) {

    derived_ids <- extract_screening_id_from_text(
      df[[source_id_col]]
    )

    cw_i <- crosswalk |>
      dplyr::filter(
        key_type == "screening_id"
      )

    if (nrow(cw_i) > 0L) {

      lookup <- stats::setNames(
        ifelse(
          is.na(cw_i$publication_year) |
            cw_i$publication_year == "",
          cw_i$first_author,
          paste(
            cw_i$first_author,
            cw_i$publication_year
          )
        ),
        cw_i$key_value
      )

      mapped <- unname(
        lookup[derived_ids]
      )

      use <- (
        is.na(df$first_author_year) |
          trimws(df$first_author_year) == ""
      ) &
        !is.na(mapped) &
        mapped != ""

      df$first_author_year[use] <- mapped[use]
    }
  }

  df
}



extract_screening_id_from_text <- function(x) {

  x <- as.character(x)

  out <- stringr::str_extract(
    x,
    stringr::regex(
      "SCR[_-]?[0-9]{3,}",
      ignore_case = TRUE
    )
  )

  out <- toupper(out)
  out <- stringr::str_replace(
    out,
    "^SCR-",
    "SCR_"
  )

  # Normalize forms such as SCR000024 to SCR_000024.
  no_sep <- !is.na(out) &
    grepl(
      "^SCR[0-9]+$",
      out
    )

  if (any(no_sep)) {
    digits <- stringr::str_remove(
      out[no_sep],
      "^SCR"
    )

    out[no_sep] <- paste0(
      "SCR_",
      digits
    )
  }

  out[is.na(out) | out == ""] <- NA_character_
  out
}

read_tabular_metadata <- function(path) {

  ext <- tolower(tools::file_ext(path))

  tryCatch(
    {
      if (ext == "csv") {
        return(
          readr::read_csv(
            path,
            show_col_types = FALSE,
            progress = FALSE
          )
        )
      }

      if (ext %in% c("xlsx", "xlsm")) {
        return(
          openxlsx::read.xlsx(
            path,
            sheet = 1,
            detectDates = TRUE
          ) |>
            tibble::as_tibble()
        )
      }

      if (ext == "rds") {
        obj <- readRDS(path)
        if (is.data.frame(obj)) {
          return(tibble::as_tibble(obj))
        }
      }

      NULL
    },
    error = function(e) NULL
  )
}

extract_publication_year <- function(x) {
  out <- stringr::str_extract(
    as.character(x),
    "(?:19|20)[0-9]{2}"
  )
  out[is.na(out) | out == ""] <- NA_character_
  out
}

extract_first_author_name <- function(x) {

  x <- trimws(as.character(x))
  x[is.na(x) | x == ""] <- NA_character_

  x <- stringr::str_replace(
    x,
    stringr::regex("\\bet\\s+al\\.?\\b.*$", ignore_case = TRUE),
    ""
  )

  # First listed author only.
  x <- stringr::str_split_fixed(x, ";", 2)[, 1]
  x <- stringr::str_split_fixed(x, ",", 2)[, 1]

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

  trimws(x)
}

generate_study_label_crosswalk <- function(
  repo_root,
  dataset_registry,
  output_file
) {

  key_specs <- list(
    screening_id = c("screening_id"),
    study_id = c("study_id"),
    pmid = c("pmid", "pubmed_id"),
    doi = c("doi"),
    trial_registration = c(
      "trial_registration",
      "registration_number",
      "registry_id",
      "nct_id",
      "clinicaltrials_id"
    ),
    study_title = c(
      "study_title",
      "article_title",
      "title"
    ),
    study_label = c(
      "study_label",
      "study_name",
      "trial_name"
    ),
    study_code = c("study_code")
  )

  author_candidates <- c(
    "first_author",
    "first_author_name",
    "lead_author",
    "author",
    "authors",
    "author_list",
    "citation_short",
    "citation"
  )

  year_candidates <- c(
    "publication_year",
    "pub_year",
    "year",
    "publication_date",
    "pub_date",
    "date",
    "citation_short",
    "citation"
  )

  # ----------------------------------------------------------
  # A. Collect all study keys required by the active analyses.
  # ----------------------------------------------------------
  requested_keys <- list()

  for (dataset_path_i in dataset_registry$dataset_path) {

    x <- safe_read_csv(dataset_path_i)

    if (nrow(x) == 0L) {
      next
    }

    for (key_type_i in names(key_specs)) {

      key_col <- first_existing_col(
        x,
        key_specs[[key_type_i]]
      )

      if (is.na(key_col)) {
        next
      }

      key_values <- unique(
        trimws(as.character(x[[key_col]]))
      )

      key_values <- key_values[
        !is.na(key_values) &
          key_values != ""
      ]

      if (length(key_values) == 0L) {
        next
      }

      requested_keys[[length(requested_keys) + 1L]] <- tibble::tibble(
        key_type = key_type_i,
        key_value = key_values
      )
    }


    # Additional-analysis datasets often preserve the original source file
    # rather than screening_id itself. Recover screening_id from values such
    # as "SCR_000024.pdf" or "SCR_000024 ... Supplementary Tables.docx".
    source_id_col <- first_existing_col(
      x,
      c(
        "source_file",
        "source_document",
        "pdf_file",
        "source_location"
      )
    )

    if (!is.na(source_id_col)) {

      derived_screening_ids <- unique(
        extract_screening_id_from_text(
          x[[source_id_col]]
        )
      )

      derived_screening_ids <- derived_screening_ids[
        !is.na(derived_screening_ids) &
          derived_screening_ids != ""
      ]

      if (length(derived_screening_ids) > 0L) {
        requested_keys[[length(requested_keys) + 1L]] <- tibble::tibble(
          key_type = "screening_id",
          key_value = derived_screening_ids
        )
      }
    }
  }

  if (length(requested_keys) == 0L) {
    return(tibble::tibble())
  }

  requested_keys <- dplyr::bind_rows(
    requested_keys
  ) |>
    dplyr::distinct()

  # ----------------------------------------------------------
  # B. Index upstream bibliographic metadata automatically.
  # ----------------------------------------------------------
  metadata_roots <- c(
    file.path(repo_root, "data", "processed"),
    file.path(repo_root, "data", "raw"),
    file.path(repo_root, "results")
  )

  metadata_roots <- metadata_roots[
    file.exists(metadata_roots)
  ]

  metadata_files <- unique(unlist(
    lapply(
      metadata_roots,
      function(root_i) {
        list.files(
          root_i,
          pattern = "\\.(csv|xlsx|xlsm|rds)$",
          recursive = TRUE,
          full.names = TRUE,
          ignore.case = TRUE
        )
      }
    )
  ))

  metadata_files <- normalize_slash(metadata_files)

  active_datasets <- normalize_slash(
    dataset_registry$dataset_path
  )

  metadata_files <- metadata_files[
    !metadata_files %in% active_datasets
  ]

  metadata_files <- metadata_files[
    !grepl(
      "study_label_crosswalk\\.csv$",
      metadata_files,
      ignore.case = TRUE
    )
  ]

  metadata_files <- metadata_files[
    !grepl(
      "/results/meta_analysis/(tables|figures|logs)/",
      metadata_files,
      ignore.case = TRUE
    )
  ]

  metadata_rows <- list()

  for (metadata_file_i in metadata_files) {

    x <- read_tabular_metadata(
      metadata_file_i
    )

    if (is.null(x) || nrow(x) == 0L) {
      next
    }

    author_col <- first_existing_col(
      x,
      author_candidates
    )

    year_col <- first_existing_col(
      x,
      year_candidates
    )

    if (is.na(author_col)) {
      next
    }

    first_author <- extract_first_author_name(
      x[[author_col]]
    )

    publication_year <- if (!is.na(year_col)) {
      extract_publication_year(
        x[[year_col]]
      )
    } else {
      extract_publication_year(
        x[[author_col]]
      )
    }

    for (key_type_i in names(key_specs)) {

      key_col <- first_existing_col(
        x,
        key_specs[[key_type_i]]
      )

      if (is.na(key_col)) {
        next
      }

      key_value <- trimws(
        as.character(x[[key_col]])
      )

      keep <- !is.na(key_value) &
        key_value != "" &
        !is.na(first_author) &
        first_author != ""

      if (!any(keep)) {
        next
      }

      metadata_rows[[length(metadata_rows) + 1L]] <- tibble::tibble(
        key_type = key_type_i,
        key_value = key_value[keep],
        first_author = first_author[keep],
        publication_year = publication_year[keep],
        source_file = normalize_slash(
          metadata_file_i
        )
      )
    }
  }

  metadata_index <- if (
    length(metadata_rows) == 0L
  ) {
    tibble::tibble(
      key_type = character(),
      key_value = character(),
      first_author = character(),
      publication_year = character(),
      source_file = character()
    )
  } else {
    dplyr::bind_rows(metadata_rows) |>
      dplyr::distinct()
  }

  # ----------------------------------------------------------
  # C. Resolve each requested key conservatively.
  # ----------------------------------------------------------
  resolved_rows <- lapply(
    seq_len(nrow(requested_keys)),
    function(i) {

      key_type_i <- requested_keys$key_type[[i]]
      key_value_i <- requested_keys$key_value[[i]]

      candidates <- metadata_index |>
        dplyr::filter(
          key_type == key_type_i,
          key_value == key_value_i
        ) |>
        dplyr::distinct(
          first_author,
          publication_year,
          .keep_all = TRUE
        )

      if (nrow(candidates) == 1L) {

        return(
          tibble::tibble(
            key_type = key_type_i,
            key_value = key_value_i,
            first_author = candidates$first_author[[1]],
            publication_year = candidates$publication_year[[1]],
            status = "AUTO",
            source_file = candidates$source_file[[1]]
          )
        )
      }

      if (nrow(candidates) > 1L) {

        return(
          tibble::tibble(
            key_type = key_type_i,
            key_value = key_value_i,
            first_author = NA_character_,
            publication_year = NA_character_,
            status = "USER REQUIRED",
            source_file = "CONFLICTING UPSTREAM METADATA"
          )
        )
      }

      tibble::tibble(
        key_type = key_type_i,
        key_value = key_value_i,
        first_author = NA_character_,
        publication_year = NA_character_,
        status = "USER REQUIRED",
        source_file = NA_character_
      )
    }
  )

  crosswalk <- dplyr::bind_rows(
    resolved_rows
  )

  dir.create(
    dirname(output_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(
    crosswalk,
    output_file,
    na = ""
  )

  crosswalk
}

normalize_effect_measure <- function(x) {

  x <- toupper(trimws(as.character(x)))
  x[x %in% c("", "NA", "N/A", "NULL")] <- NA_character_

  # Canonical ratio measures
  x[x %in% c(
    "RR",
    "RISK RATIO",
    "RELATIVE RISK",
    "RISK-RATIO",
    "RELATIVE-RISK"
  )] <- "RR"

  x[x %in% c(
    "OR",
    "ODDS RATIO",
    "ODDS-RATIO"
  )] <- "OR"

  x[x %in% c(
    "HR",
    "HAZARD RATIO",
    "HAZARD-RATIO"
  )] <- "HR"

  # Numeric / placeholder contamination such as "64" is not an effect measure.
  numeric_like <- !is.na(x) & grepl("^[+-]?[0-9]+(?:\\.[0-9]+)?$", x)
  x[numeric_like] <- NA_character_

  x
}

build_meta_object_from_dataset <- function(df, analysis_group_label = "Analysis") {

  # ---- common columns -------------------------------------------------------
  study_col <- first_existing_col(
    df,
    c(
      "first_author_year",
      "author_year",
      "citation_short",
      "short_citation",
      "first_author",
      "first_author_name",
      "author",
      "lead_author",
      "study_label",
      "study_name",
      "study",
      "study_code",
      "study_id",
      "screening_id"
    )
  )

  if (is.na(study_col)) {
    stop(
      "No study identifier column found in the dataset.",
      call. = FALSE
    )
  }

  design_col <- first_existing_col(
    df,
    c(
      "final_study_design",
      "study_design",
      "design"
    )
  )

  # IMPORTANT:
  # Final / verified canonical columns must take precedence over raw extraction
  # columns. This prevents placeholder contamination (e.g., "64") from being
  # interpreted as an effect measure.
  measure_col <- first_existing_col(
    df,
    c(
      "final_effect_measure",
      "human_verified_effect_measure",
      "effect_measure",
      "measure",
      "summary_measure"
    )
  )

  effect_col <- first_existing_col(
    df,
    c(
      "final_effect_estimate",
      "human_verified_effect_estimate",
      "effect_value",
      "effect_estimate",
      "estimate",
      "study_effect",
      "rr",
      "or",
      "hr"
    )
  )

  lower_col <- first_existing_col(
    df,
    c(
      "final_lower_ci",
      "human_verified_lower_ci",
      "lower_ci",
      "study_lower_ci",
      "lcl",
      "ci_lower",
      "lower",
      "lower95"
    )
  )

  upper_col <- first_existing_col(
    df,
    c(
      "final_upper_ci",
      "human_verified_upper_ci",
      "upper_ci",
      "study_upper_ci",
      "ucl",
      "ci_upper",
      "upper",
      "upper95"
    )
  )

  events_e_col <- first_existing_col(
    df,
    c(
      "final_intervention_events",
      "human_verified_intervention_events",
      "intervention_events",
      "events_exposed",
      "event_experimental",
      "event_treatment",
      "events_treatment",
      "s_event"
    )
  )

  n_e_col <- first_existing_col(
    df,
    c(
      "final_intervention_total",
      "human_verified_intervention_total",
      "intervention_total",
      "n_exposed",
      "n_experimental",
      "n_treatment",
      "total_treatment",
      "s_total"
    )
  )

  events_c_col <- first_existing_col(
    df,
    c(
      "final_comparator_events",
      "human_verified_comparator_events",
      "comparator_events",
      "events_control",
      "event_control",
      "c_event"
    )
  )

  n_c_col <- first_existing_col(
    df,
    c(
      "final_comparator_total",
      "human_verified_comparator_total",
      "comparator_total",
      "n_control",
      "total_control",
      "c_total"
    )
  )

  treat_label_col <- first_existing_col(
    df,
    c(
      "experimental_label",
      "treatment_label",
      "intervention_label"
    )
  )

  control_label_col <- first_existing_col(
    df,
    c(
      "control_label",
      "comparator_label"
    )
  )

  # ---- study labels ---------------------------------------------------------
  studlab <- build_study_labels(df)

  study_design <- if (!is.na(design_col)) {
    as.character(df[[design_col]])
  } else {
    rep(NA_character_, nrow(df))
  }

  # ---- effect measure -------------------------------------------------------
  effect_measure <- rep(NA_character_, nrow(df))

  if (!is.na(measure_col)) {
    effect_measure <- normalize_effect_measure(df[[measure_col]])
  }

  distinct_measures <- unique(stats::na.omit(effect_measure))

  # If no usable explicit measure remains, infer RR only when dichotomous
  # treatment/control counts are structurally available. This preserves the
  # existing universal RR fallback without treating numeric placeholders as
  # valid measures.
  count_cols_present <- all(
    !is.na(c(
      events_e_col,
      n_e_col,
      events_c_col,
      n_c_col
    ))
  )

  if (length(distinct_measures) == 0L) {
    if (count_cols_present) {
      summary_measure <- "RR"
      effect_measure <- rep("RR", nrow(df))
    } else {
      stop(
        "No valid effect measure was available after canonical normalization.",
        call. = FALSE
      )
    }
  } else {

    if (length(distinct_measures) > 1L) {
      stop(
        "Mixed effect measures detected after normalization: ",
        paste(distinct_measures, collapse = ", "),
        call. = FALSE
      )
    }

    summary_measure <- distinct_measures[[1]]
    effect_measure[is.na(effect_measure)] <- summary_measure
  }

  # ---- assemble row-level TE / seTE ----------------------------------------
  # A single canonical analysis dataset may legitimately contain mixed INPUT
  # TYPES (e.g., raw dichotomous counts for some RCTs and a reported RR+CI for
  # another RCT). We therefore build study-level log effects row by row and
  # then pool all usable studies with metagen().
  #
  # Priority:
  #   1) valid final/reported effect estimate + CI
  #   2) otherwise valid binary counts
  #
  # This ensures reported adjusted/verified effects are not unnecessarily
  # replaced by recalculation from raw counts.

  n_rows <- nrow(df)

  TE <- rep(NA_real_, n_rows)
  seTE <- rep(NA_real_, n_rows)
  input_source <- rep(NA_character_, n_rows)

  effect_value <- rep(NA_real_, n_rows)
  lower_ci <- rep(NA_real_, n_rows)
  upper_ci <- rep(NA_real_, n_rows)

  if (
    !is.na(effect_col) &&
    !is.na(lower_col) &&
    !is.na(upper_col)
  ) {
    effect_value <- safe_num(df[[effect_col]])
    lower_ci <- safe_num(df[[lower_col]])
    upper_ci <- safe_num(df[[upper_col]])

    reported_keep <- !is.na(effect_value) &
      !is.na(lower_ci) &
      !is.na(upper_ci) &
      effect_value > 0 &
      lower_ci > 0 &
      upper_ci > 0 &
      lower_ci <= upper_ci

    if (any(reported_keep)) {
      TE[reported_keep] <- log(effect_value[reported_keep])
      seTE[reported_keep] <- (
        log(upper_ci[reported_keep]) -
          log(lower_ci[reported_keep])
      ) / 3.92
      input_source[reported_keep] <- "reported_effect"
    }
  }

  # Binary counts are used only for rows not already populated from a valid
  # reported/final effect estimate.
  event.e <- n.e <- event.c <- n.c <- rep(NA_real_, n_rows)

  if (count_cols_present && summary_measure %in% c("RR", "OR")) {

    event.e <- safe_num(df[[events_e_col]])
    n.e <- safe_num(df[[n_e_col]])
    event.c <- safe_num(df[[events_c_col]])
    n.c <- safe_num(df[[n_c_col]])

    binary_keep <- is.na(TE) &
      !is.na(event.e) &
      !is.na(n.e) &
      !is.na(event.c) &
      !is.na(n.c) &
      n.e > 0 &
      n.c > 0 &
      event.e >= 0 &
      event.c >= 0 &
      event.e <= n.e &
      event.c <= n.c

    if (any(binary_keep)) {

      binary_meta <- meta::metabin(
        event.e = event.e[binary_keep],
        n.e = n.e[binary_keep],
        event.c = event.c[binary_keep],
        n.c = n.c[binary_keep],
        studlab = studlab[binary_keep],
        sm = summary_measure,
        method = "Inverse",
        common = FALSE,
        random = FALSE,
        prediction = FALSE
      )

      TE[binary_keep] <- as.numeric(binary_meta$TE)
      seTE[binary_keep] <- as.numeric(binary_meta$seTE)
      input_source[binary_keep] <- "raw_dichotomous"
    }
  }

  keep <- !is.na(TE) &
    !is.na(seTE) &
    is.finite(TE) &
    is.finite(seTE) &
    seTE > 0

  if (sum(keep) == 0L) {
    stop(
      paste0(
        "No usable study-level effect rows were available after checking ",
        "canonical effect estimates/CIs and binary counts."
      ),
      call. = FALSE
    )
  }

  # ---- pooled meta-analysis -------------------------------------------------
  meta_obj <- meta::metagen(
    TE = TE[keep],
    seTE = seTE[keep],
    studlab = studlab[keep],
    sm = summary_measure,
    method.tau = "REML",
    method.random.ci = "HK",
    random = TRUE,
    common = FALSE,
    prediction = FALSE
  )

  w_random <- as.numeric(meta_obj$w.random)

  if (
    length(w_random) == meta_obj$k &&
    sum(w_random, na.rm = TRUE) > 0
  ) {
    weights_percent <- w_random / sum(
      w_random,
      na.rm = TRUE
    ) * 100
  } else {
    weights_percent <- rep(NA_real_, meta_obj$k)
  }

  study_table <- tibble::tibble(
    analysis_group = analysis_group_label,
    study_label = studlab[keep],
    study_design = study_design[keep],
    effect_measure = summary_measure,
    effect_value = exp(as.numeric(meta_obj$TE)),
    lower_ci = exp(as.numeric(meta_obj$lower)),
    upper_ci = exp(as.numeric(meta_obj$upper)),
    log_effect = as.numeric(meta_obj$TE),
    se_log_effect = as.numeric(meta_obj$seTE),
    random_effect_weight_percent = weights_percent,
    effect_input_source = input_source[keep]
  )

  # Add count columns when they exist. Reported-effect rows simply retain NA.
  if (count_cols_present) {
    study_table$events_exposed <- event.e[keep]
    study_table$n_exposed <- n.e[keep]
    study_table$events_control <- event.c[keep]
    study_table$n_control <- n.c[keep]

    # Also attach these canonical columns to the meta object so forest.meta()
    # can render Events / Total columns for each comparison group whenever
    # the pooled estimate was derived from dichotomous-count criteria.
    meta_obj$event.e <- event.e[keep]
    meta_obj$n.e <- n.e[keep]
    meta_obj$event.c <- event.c[keep]
    meta_obj$n.c <- n.c[keep]
  }

  treat_label <- if (!is.na(treat_label_col)) {
    safe_chr(
      df[[treat_label_col]],
      "SGLT2i"
    )
  } else {
    "SGLT2i"
  }

  control_label <- if (!is.na(control_label_col)) {
    safe_chr(
      df[[control_label_col]],
      "Control"
    )
  } else {
    "Control"
  }

  pooled_row <- tibble::tibble(
    analysis_group = analysis_group_label,
    summary_measure = summary_measure,
    n_studies = meta_obj$k,
    pooled_effect = exp(as.numeric(meta_obj$TE.random)),
    pooled_lower_ci = exp(as.numeric(meta_obj$lower.random)),
    pooled_upper_ci = exp(as.numeric(meta_obj$upper.random)),
    p_value = as.numeric(meta_obj$pval.random),
    heterogeneity_i2 = as.numeric(meta_obj$I2),
    heterogeneity_tau2 = as.numeric(meta_obj$tau2),
    heterogeneity_q = as.numeric(meta_obj$Q),
    heterogeneity_p = as.numeric(meta_obj$pval.Q),
    model = "Random-effects REML + Hartung-Knapp",
    treatment_label = treat_label,
    control_label = control_label
  )

  list(
    meta_object = meta_obj,
    study_table = study_table,
    summary_table = pooled_row,
    treatment_label = treat_label,
    control_label = control_label,
    summary_measure = summary_measure
  )
}


save_forest_plot <- function(meta_bundle, analysis_group, output_base) {

  meta_obj <- meta_bundle$meta_object
  sm <- meta_bundle$summary_measure
  display_title <- tools::toTitleCase(gsub("_", " ", analysis_group))

  png_file <- paste0(output_base, ".png")
  pdf_file <- paste0(output_base, ".pdf")

  n_studies <- max(1L, as.integer(meta_obj$k))

  has_binary_counts <- all(
    c("event.e", "n.e", "event.c", "n.c") %in% names(meta_obj)
  ) &&
    any(!is.na(meta_obj$event.e)) &&
    any(!is.na(meta_obj$n.e)) &&
    any(!is.na(meta_obj$event.c)) &&
    any(!is.na(meta_obj$n.c))

  if (has_binary_counts) {

    leftcols <- c(
      "studlab",
      "event.e",
      "n.e",
      "event.c",
      "n.c"
    )

    leftlabs <- c(
      "Study",
      "Events",
      "Total",
      "Events",
      "Total"
    )

    png_width <- 3000L
    pdf_width <- 11.6
    plot_width_mm <- 60

    gap_left_mm <- 2.2
    gap_studlab_mm <- 3.0
    gap_forest_left_mm <- 5.0
    gap_forest_right_mm <- 4.5
    gap_right_mm <- 3.0

  } else {

    leftcols <- c("studlab")
    leftlabs <- c("Study")

    png_width <- 2700L
    pdf_width <- 10.5
    plot_width_mm <- 62

    gap_left_mm <- 2.5
    gap_studlab_mm <- 3.0
    gap_forest_left_mm <- 5.5
    gap_forest_right_mm <- 5.0
    gap_right_mm <- 3.2
  }

  png_height <- max(
    1250L,
    870L + n_studies * 125L
  )

  pdf_height <- max(
    4.8,
    3.8 + n_studies * 0.31
  )

  base_fontsize <- if (n_studies <= 8L) {
    10.0
  } else if (n_studies <= 15L) {
    9.4
  } else {
    8.8
  }

  study_fontsize <- base_fontsize
  heading_fontsize <- base_fontsize + 0.2
  axis_fontsize <- max(8.4, base_fontsize - 0.6)
  het_fontsize <- max(8.4, base_fontsize - 0.7)

  col_square <- "#4C78A8"
  col_square_line <- "#2F5597"
  col_diamond <- "#F28E2B"
  col_diamond_line <- "#B35C00"
  col_text <- "#1F2937"

  common_args <- list(
    x = meta_obj,
    sortvar = meta_obj$TE,
    common = FALSE,
    random = TRUE,

    leftcols = leftcols,
    leftlabs = leftlabs,
    rightcols = c(
      "effect",
      "ci",
      "w.random"
    ),
    rightlabs = c(
      sm,
      "95% CI",
      "Weight"
    ),

    label.e = meta_bundle$treatment_label,
    label.c = meta_bundle$control_label,

    title = display_title,
    smlab = paste0(
      sm,
      " (random-effects)"
    ),
    text.random = "Pooled effect",

    test.overall = TRUE,
    test.subgroup = FALSE,
    test.overall.random = TRUE,
    print.I2 = TRUE,
    print.tau2 = TRUE,
    print.pval.Q = TRUE,
    overall = TRUE,
    overall.hetstat = TRUE,

    digits = 2,
    digits.se = 2,
    digits.pval = 3,

    col.square = col_square,
    col.square.lines = col_square_line,
    col.diamond = col_diamond,
    col.diamond.lines = col_diamond_line,
    col.predict = col_diamond,
    col.study = col_text,
    col.label.right = col_text,
    col.label.left = col_text,
    squaresize = 0.88,
    lwd = 1.05,

    colgap.left = grid::unit(
      gap_left_mm,
      "mm"
    ),
    colgap.right = grid::unit(
      gap_right_mm,
      "mm"
    ),
    colgap.studlab = grid::unit(
      gap_studlab_mm,
      "mm"
    ),
    colgap.forest.left = grid::unit(
      gap_forest_left_mm,
      "mm"
    ),
    colgap.forest.right = grid::unit(
      gap_forest_right_mm,
      "mm"
    ),
    plotwidth = grid::unit(
      plot_width_mm,
      "mm"
    ),

    spacing = 0.92,
    addrow.overall = TRUE,
    addrows.below.overall = 1,

    fontsize = base_fontsize,
    fs.study = study_fontsize,
    fs.axis = axis_fontsize,
    fs.random = heading_fontsize,
    fs.hetstat = het_fontsize,
    fs.heading = heading_fontsize,
    fs.lab = base_fontsize,
    just.studlab = "left"
  )

  render_plot <- function(open_device) {

    open_device()

    tryCatch(
      {
        do.call(
          meta::forest,
          common_args
        )
      },
      finally = {
        grDevices::dev.off()
      }
    )
  }

  render_plot(
    function() {
      grDevices::png(
        filename = png_file,
        width = png_width,
        height = png_height,
        res = 300,
        bg = "white"
      )
    }
  )

  render_plot(
    function() {
      grDevices::pdf(
        file = pdf_file,
        width = pdf_width,
        height = pdf_height
      )
    }
  )

  list(
    png = png_file,
    pdf = pdf_file
  )
}


# ============================================================
# 2. Project root + config
# ============================================================

repo_root <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

config_file <- file.path(
  repo_root,
  "config",
  "project_config.yml"
)

if (!file.exists(config_file)) {
  stop(
    "config/project_config.yml was not found. Run R/33 from the repository root.",
    call. = FALSE
  )
}

config <- yaml::read_yaml(config_file)

if (is.null(config)) {
  stop(
    "project_config.yml could not be parsed.",
    call. = FALSE
  )
}




# ============================================================
# 3. Canonical output directories
# ============================================================

results_root <- file.path(
  repo_root,
  "results",
  "meta_analysis"
)

figures_dir <- file.path(
  results_root,
  "figures"
)

tables_dir <- file.path(
  results_root,
  "tables"
)

logs_dir <- file.path(
  results_root,
  "logs"
)

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

report_file <- file.path(
  logs_dir,
  "r33_forest_and_tables_report.txt"
)

summary_csv <- file.path(
  tables_dir,
  "meta_analysis_summary.csv"
)

study_csv <- file.path(
  tables_dir,
  "study_level_effects.csv"
)

xlsx_file <- file.path(
  tables_dir,
  "meta_analysis_outputs.xlsx"
)



analysis_purpose_from_workspace <- function(workspace_name) {

  x <- basename(normalize_slash(workspace_name))

  # Canonical R/29 workspace:
  # AA_YYYYMMDD_HHMMSS_<purpose_slug>
  x <- stringr::str_replace(
    x,
    "^AA_[0-9]{8}_[0-9]{6}_",
    ""
  )

  if (x == "" || is.na(x)) {
    return("additional_analysis")
  }

  x
}

analysis_timestamp_from_workspace <- function(workspace_name) {

  x <- basename(normalize_slash(workspace_name))

  stamp <- stringr::str_match(
    x,
    "^AA_([0-9]{8})_([0-9]{6})_"
  )

  if (is.na(stamp[1, 2]) || is.na(stamp[1, 3])) {
    return(NA_character_)
  }

  paste0(stamp[1, 2], stamp[1, 3])
}

discover_completed_additional_analyses <- function(repo_root) {

  result_root <- file.path(
    repo_root,
    "results",
    "additional_analysis"
  )

  processed_root <- file.path(
    repo_root,
    "data",
    "processed",
    "additional_analysis"
  )

  result_workspaces <- if (dir.exists(result_root)) {
    list.dirs(
      result_root,
      recursive = FALSE,
      full.names = TRUE
    )
  } else {
    character()
  }

  processed_workspaces <- if (dir.exists(processed_root)) {
    list.dirs(
      processed_root,
      recursive = FALSE,
      full.names = TRUE
    )
  } else {
    character()
  }

  workspace_names <- unique(
    c(
      basename(result_workspaces),
      basename(processed_workspaces)
    )
  )

  workspace_names <- workspace_names[
    grepl(
      "^AA_[0-9]{8}_[0-9]{6}_",
      workspace_names
    )
  ]

  if (length(workspace_names) == 0L) {
    return(
      tibble::tibble(
        analysis_group = character(),
        dataset_path = character(),
        analysis_type = character(),
        analysis_purpose = character(),
        source_workspace = character(),
        source_summary = character()
      )
    )
  }

  rows <- lapply(
    workspace_names,
    function(workspace_name_i) {

      result_workspace <- file.path(
        result_root,
        workspace_name_i
      )

      processed_workspace <- file.path(
        processed_root,
        workspace_name_i
      )

      # The ready dataset may live in results/ or data/processed/,
      # depending on the R/29 version and routing stage.
      ready_candidates <- c(
        file.path(
          result_workspace,
          "additional_analysis_meta_analysis_ready.csv"
        ),
        file.path(
          processed_workspace,
          "additional_analysis_meta_analysis_ready.csv"
        )
      )

      ready_candidates <- ready_candidates[
        file.exists(ready_candidates)
      ]

      if (length(ready_candidates) == 0L) {
        return(NULL)
      }

      ready_file <- ready_candidates[[1]]

      # Completion evidence is discovered flexibly because earlier toolkit
      # versions used slightly different output naming. We require at least
      # one meta-analysis summary/result artifact in the result workspace,
      # rather than hard-coding every filename.
      result_files <- if (dir.exists(result_workspace)) {
        list.files(
          result_workspace,
          recursive = TRUE,
          full.names = TRUE
        )
      } else {
        character()
      }

      result_basenames <- tolower(
        basename(result_files)
      )

      has_summary <- any(
        grepl(
          "meta.*analysis.*summary.*\\.(csv|xlsx)$",
          result_basenames
        )
      )

      has_result_sentence <- any(
        grepl(
          "manuscript.*result.*sentence.*\\.txt$",
          result_basenames
        )
      )

      has_model_output <- any(
        grepl(
          "model.*output.*\\.txt$",
          result_basenames
        )
      )

      has_forest <- any(
        grepl(
          "forest.*\\.(pdf|png)$",
          result_basenames
        )
      )

      # At least one real downstream meta-analysis artifact must exist.
      completed <- has_summary ||
        has_result_sentence ||
        has_model_output ||
        has_forest

      if (!completed) {
        return(NULL)
      }

      summary_hits <- result_files[
        grepl(
          "meta.*analysis.*summary.*\\.(csv|xlsx)$",
          tolower(basename(result_files))
        )
      ]

      summary_file <- if (
        length(summary_hits) > 0L
      ) {
        summary_hits[[1]]
      } else {
        NA_character_
      }

      purpose_slug <- analysis_purpose_from_workspace(
        workspace_name_i
      )

      timestamp <- analysis_timestamp_from_workspace(
        workspace_name_i
      )

      tibble::tibble(
        analysis_group = purpose_slug,
        dataset_path = normalize_slash(ready_file),
        analysis_type = "additional_analysis",
        analysis_purpose = purpose_slug,
        source_workspace = normalize_slash(
          if (dir.exists(result_workspace)) {
            result_workspace
          } else {
            processed_workspace
          }
        ),
        source_summary = if (!is.na(summary_file)) {
          normalize_slash(summary_file)
        } else {
          NA_character_
        },
        workspace_timestamp = timestamp
      )
    }
  )

  rows <- rows[
    !vapply(
      rows,
      is.null,
      logical(1)
    )
  ]

  if (length(rows) == 0L) {
    return(
      tibble::tibble(
        analysis_group = character(),
        dataset_path = character(),
        analysis_type = character(),
        analysis_purpose = character(),
        source_workspace = character(),
        source_summary = character()
      )
    )
  }

  out <- dplyr::bind_rows(rows)

  # For repeated runs of the same purpose, use only the newest completed run.
  out |>
    dplyr::arrange(
      analysis_purpose,
      dplyr::desc(workspace_timestamp)
    ) |>
    dplyr::group_by(
      analysis_purpose
    ) |>
    dplyr::slice(1L) |>
    dplyr::ungroup() |>
    dplyr::select(
      -workspace_timestamp
    )
}

is_generic_additional_analysis_row <- function(
  analysis_group,
  dataset_path
) {

  group_text <- tolower(
    trimws(as.character(analysis_group))
  )

  path_text <- tolower(
    normalize_slash(dataset_path)
  )

  grepl(
    "additional_analysis",
    group_text
  ) ||
    grepl(
      "/additional_analysis/",
      path_text,
      fixed = TRUE
    )
}

# ============================================================
# 4. Locate canonical analysis datasets
# ============================================================

candidate_manifest_paths <- c(
  file.path(
    repo_root,
    "data",
    "processed",
    "meta_analysis",
    "meta_analysis_manifest.csv"
  ),
  file.path(
    repo_root,
    "results",
    "meta_analysis",
    "meta_analysis_manifest.csv"
  ),
  file.path(
    repo_root,
    "meta_analysis_manifest.csv"
  ),
  file.path(
    repo_root,
    "data",
    "processed",
    "extraction",
    "meta_analysis_manifest.csv"
  )
)

manifest_path <- candidate_manifest_paths[
  file.exists(candidate_manifest_paths)
]

manifest <- NULL

manifest_registry <- tibble::tibble(
  analysis_group = character(),
  dataset_path = character()
)

if (length(manifest_path) > 0L) {

  manifest_path <- manifest_path[[1]]
  manifest <- safe_read_csv(manifest_path)

  path_col <- first_existing_col(
    manifest,
    c(
      "dataset_path",
      "source_file",
      "analysis_dataset",
      "input_file",
      "csv_path",
      "file_path"
    )
  )

  group_col <- first_existing_col(
    manifest,
    c(
      "analysis_group",
      "group_name",
      "analysis_set",
      "label"
    )
  )

  status_col <- first_existing_col(
    manifest,
    c(
      "status",
      "analysis_status",
      "execution_status"
    )
  )

  if (!is.na(path_col)) {

    manifest_registry <- manifest |>
      dplyr::mutate(
        analysis_group = if (!is.na(group_col)) {
          as.character(
            .data[[group_col]]
          )
        } else {
          paste0(
            "Group_",
            dplyr::row_number()
          )
        },
        dataset_path = as.character(
          .data[[path_col]]
        ),
        keep = if (!is.na(status_col)) {
          tolower(
            trimws(
              as.character(
                .data[[status_col]]
              )
            )
          ) %in% c(
            "pass",
            "passed",
            "ready",
            "active",
            "include",
            "included",
            "complete",
            "completed"
          )
        } else {
          TRUE
        }
      ) |>
      dplyr::filter(keep) |>
      dplyr::select(
        analysis_group,
        dataset_path
      )
  }
}

# Always perform a recursive fallback search in data/processed.
# This protects the toolkit against stale, incomplete, or version-specific
# manifests while keeping dataset discovery reproducible.
processed_root <- file.path(
  repo_root,
  "data",
  "processed"
)

fallback_files <- if (
  dir.exists(processed_root)
) {
  list.files(
    processed_root,
    pattern = "meta_analysis_ready.*\\.csv$|.*meta_analysis_ready\\.csv$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
} else {
  character()
}

fallback_files <- normalize_slash(
  fallback_files
)

# Additional analyses are handled separately using R/29 workspaces so
# their purpose and rerun version are preserved.
main_fallback_files <- fallback_files[
  !grepl(
    "/additional_analysis/",
    tolower(fallback_files),
    fixed = TRUE
  )
]

fallback_registry <- tibble::tibble(
  analysis_group = tools::file_path_sans_ext(
    basename(main_fallback_files)
  ),
  dataset_path = main_fallback_files
)

main_registry <- dplyr::bind_rows(
  manifest_registry,
  fallback_registry
) |>
  dplyr::filter(
    !is.na(dataset_path),
    trimws(dataset_path) != ""
  ) |>
  dplyr::distinct(
    dataset_path,
    .keep_all = TRUE
  )

# Remove generic additional-analysis rows that may remain in an older manifest.
if (nrow(main_registry) > 0L) {

  generic_additional <- vapply(
    seq_len(nrow(main_registry)),
    function(i) {
      is_generic_additional_analysis_row(
        main_registry$analysis_group[[i]],
        main_registry$dataset_path[[i]]
      )
    },
    logical(1)
  )

  main_registry <- main_registry[
    !generic_additional,
    ,
    drop = FALSE
  ]
}

main_registry <- main_registry |>
  dplyr::mutate(
    analysis_type = "main_analysis",
    analysis_purpose = analysis_group,
    source_workspace = NA_character_,
    source_summary = NA_character_
  )

additional_registry <- discover_completed_additional_analyses(
  repo_root
)

dataset_registry <- dplyr::bind_rows(
  main_registry,
  additional_registry
) |>
  dplyr::distinct(
    analysis_group,
    dataset_path,
    .keep_all = TRUE
  )

if (nrow(dataset_registry) == 0L) {

  stop(
    paste0(
      "No executable analysis dataset was found.\n",
      "Searched recursively under data/processed for canonical ",
      "meta-analysis-ready datasets and under results/data/processed ",
      "additional_analysis for completed R/29 workspaces."
    ),
    call. = FALSE
  )
}

cat(
  "Main analysis datasets detected: ",
  sum(
    dataset_registry$analysis_type == "main_analysis",
    na.rm = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Completed additional analyses detected: ",
  sum(
    dataset_registry$analysis_type == "additional_analysis",
    na.rm = TRUE
  ),
  "\n\n",
  sep = ""
)

# ============================================================
# 5. Normalize dataset paths
# ============================================================

resolve_dataset_path <- function(x) {

  x <- safe_chr(x)

  if (x == "") {
    return(NA_character_)
  }

  x <- normalize_slash(x)

  if (grepl("^[A-Za-z]:/", x) || grepl("^/", x)) {
    resolved <- x
  } else {
    resolved <- file.path(repo_root, x)
  }

  resolved <- normalizePath(
    resolved,
    winslash = "/",
    mustWork = FALSE
  )

  if (!file.exists(resolved)) {
    return(NA_character_)
  }

  resolved
}

dataset_registry <- dataset_registry |>
  dplyr::mutate(
    dataset_path = vapply(dataset_path, resolve_dataset_path, character(1))
  ) |>
  dplyr::filter(!is.na(dataset_path))

if (nrow(dataset_registry) == 0L) {
  stop(
    "No valid analysis dataset could be resolved from the manifest/fallback search.",
    call. = FALSE
  )
}


# ============================================================
# 5A. Auto-generate canonical study-label crosswalk
# ============================================================

study_label_crosswalk_file <- file.path(
  repo_root,
  "data",
  "processed",
  "meta_analysis",
  "study_label_crosswalk.csv"
)

generated_crosswalk <- generate_study_label_crosswalk(
  repo_root = repo_root,
  dataset_registry = dataset_registry,
  output_file = study_label_crosswalk_file
)

study_label_crosswalk <- load_study_label_crosswalk(
  repo_root
)

auto_label_n <- if (
  is.null(generated_crosswalk) ||
  nrow(generated_crosswalk) == 0L
) {
  0L
} else {
  sum(
    generated_crosswalk$status == "AUTO",
    na.rm = TRUE
  )
}

user_required_label_n <- if (
  is.null(generated_crosswalk) ||
  nrow(generated_crosswalk) == 0L
) {
  0L
} else {
  sum(
    generated_crosswalk$status == "USER REQUIRED",
    na.rm = TRUE
  )
}

cat(
  "Study-label crosswalk generated automatically.\n"
)
cat(
  "AUTO: ",
  auto_label_n,
  "  USER REQUIRED: ",
  user_required_label_n,
  "\n",
  sep = ""
)
cat(
  "Crosswalk: ",
  normalize_slash(study_label_crosswalk_file),
  "\n\n",
  sep = ""
)


# ============================================================
# 6. Build forest plots + tables for each group
# ============================================================

summary_rows <- list()
study_rows <- list()
plot_rows <- list()
fail_rows <- list()

for (i in seq_len(nrow(dataset_registry))) {

  analysis_group <- safe_chr(dataset_registry$analysis_group[[i]], paste0("Group_", i))
  dataset_path <- dataset_registry$dataset_path[[i]]

  cat("------------------------------------------------------------\n")
  cat("R/33 analysis group: ", analysis_group, "\n", sep = "")
  cat("------------------------------------------------------------\n")
  cat("Source dataset: ", normalize_slash(dataset_path), "\n", sep = "")

  tryCatch(

    {
      df <- safe_read_csv(dataset_path)

      df <- apply_study_label_crosswalk(
        df = df,
        crosswalk = study_label_crosswalk
      )

      if (nrow(df) == 0L) {
        stop("Dataset has zero rows.", call. = FALSE)
      }

      meta_bundle <- build_meta_object_from_dataset(
        df = df,
        analysis_group_label = analysis_group
      )

      plot_slug <- slugify(analysis_group)

      # Purpose-aware registry should normally guarantee unique names.
      # Retain a deterministic suffix only if a genuine duplicate remains.
      if (sum(dataset_registry$analysis_group == analysis_group) > 1L) {
        plot_slug <- paste0(
          plot_slug,
          "_",
          slugify(
            safe_chr(
              dataset_registry$analysis_type[[i]],
              paste0("group_", i)
            )
          )
        )
      }

      plot_base <- file.path(
        figures_dir,
        paste0("forest_", plot_slug)
      )

      plot_files <- save_forest_plot(
        meta_bundle = meta_bundle,
        analysis_group = analysis_group,
        output_base = plot_base
      )

      summary_rows[[length(summary_rows) + 1L]] <- meta_bundle$summary_table |>
        dplyr::mutate(
          analysis_type = safe_chr(
            dataset_registry$analysis_type[[i]]
          ),
          analysis_purpose = safe_chr(
            dataset_registry$analysis_purpose[[i]]
          ),
          source_workspace = safe_chr(
            dataset_registry$source_workspace[[i]]
          ),
          source_r29_summary = safe_chr(
            dataset_registry$source_summary[[i]]
          ),
          source_dataset = normalize_slash(dataset_path),
          forest_plot_png = normalize_slash(plot_files$png),
          forest_plot_pdf = normalize_slash(plot_files$pdf)
        )

      study_rows[[length(study_rows) + 1L]] <- meta_bundle$study_table |>
        dplyr::mutate(
          analysis_type = safe_chr(
            dataset_registry$analysis_type[[i]]
          ),
          analysis_purpose = safe_chr(
            dataset_registry$analysis_purpose[[i]]
          ),
          source_dataset = normalize_slash(dataset_path)
        )

      plot_rows[[length(plot_rows) + 1L]] <- tibble::tibble(
        analysis_group = analysis_group,
        analysis_type = safe_chr(
          dataset_registry$analysis_type[[i]]
        ),
        analysis_purpose = safe_chr(
          dataset_registry$analysis_purpose[[i]]
        ),
        dataset_path = normalize_slash(dataset_path),
        source_workspace = safe_chr(
          dataset_registry$source_workspace[[i]]
        ),
        png_file = normalize_slash(plot_files$png),
        pdf_file = normalize_slash(plot_files$pdf),
        status = "PASS"
      )

      cat("Status: PASS\n\n")
    },

    error = function(e) {

      fail_rows[[length(fail_rows) + 1L]] <<- tibble::tibble(
        analysis_group = analysis_group,
        dataset_path = normalize_slash(dataset_path),
        error_message = safe_chr(conditionMessage(e))
      )

      cat("Status: FAIL\n")
      cat("Reason: ", conditionMessage(e), "\n\n", sep = "")
    }
  )
}


summary_table <- if (length(summary_rows) == 0L) {
  tibble::tibble(
    analysis_group = character(),
    summary_measure = character(),
    n_studies = integer(),
    pooled_effect = numeric(),
    pooled_lower_ci = numeric(),
    pooled_upper_ci = numeric(),
    p_value = numeric(),
    heterogeneity_i2 = numeric(),
    heterogeneity_tau2 = numeric(),
    heterogeneity_q = numeric(),
    heterogeneity_p = numeric(),
    model = character(),
    treatment_label = character(),
    control_label = character(),
    source_dataset = character(),
    forest_plot_png = character(),
    forest_plot_pdf = character()
  )
} else {
  dplyr::bind_rows(summary_rows)
}

study_table <- if (length(study_rows) == 0L) {
  tibble::tibble(
    analysis_group = character(),
    study_label = character(),
    study_design = character(),
    effect_measure = character(),
    effect_value = numeric(),
    lower_ci = numeric(),
    upper_ci = numeric(),
    log_effect = numeric(),
    se_log_effect = numeric(),
    random_effect_weight_percent = numeric(),
    source_dataset = character()
  )
} else {
  dplyr::bind_rows(study_rows)
}

plot_manifest <- if (length(plot_rows) == 0L) {
  tibble::tibble(
    analysis_group = character(),
    dataset_path = character(),
    png_file = character(),
    pdf_file = character(),
    status = character()
  )
} else {
  dplyr::bind_rows(plot_rows)
}

fail_table <- if (length(fail_rows) == 0L) {
  tibble::tibble(
    analysis_group = character(),
    dataset_path = character(),
    error_message = character()
  )
} else {
  dplyr::bind_rows(fail_rows)
}


# ============================================================
# 7. Save editable tables
# ============================================================

readr::write_csv(
  summary_table,
  summary_csv,
  na = ""
)

readr::write_csv(
  study_table,
  study_csv,
  na = ""
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb, "Meta_Analysis_Summary")
openxlsx::writeData(wb, "Meta_Analysis_Summary", summary_table)

openxlsx::addWorksheet(wb, "Study_Level_Effects")
openxlsx::writeData(wb, "Study_Level_Effects", study_table)

openxlsx::addWorksheet(wb, "Figure_Manifest")
openxlsx::writeData(wb, "Figure_Manifest", plot_manifest)


openxlsx::addWorksheet(wb, "Analysis_Registry")
openxlsx::writeData(
  wb,
  "Analysis_Registry",
  dataset_registry
)


if (
  exists("generated_crosswalk") &&
  !is.null(generated_crosswalk)
) {
  openxlsx::addWorksheet(
    wb,
    "Study_Label_Crosswalk"
  )
  openxlsx::writeData(
    wb,
    "Study_Label_Crosswalk",
    generated_crosswalk
  )
}

if (nrow(fail_table) > 0L) {
  openxlsx::addWorksheet(wb, "Failures")
  openxlsx::writeData(wb, "Failures", fail_table)
}

if (!is.null(manifest)) {
  openxlsx::addWorksheet(wb, "Input_Manifest")
  openxlsx::writeData(wb, "Input_Manifest", manifest)
}

openxlsx::saveWorkbook(
  wb,
  file = xlsx_file,
  overwrite = TRUE
)


# ============================================================
# 8. Write report
# ============================================================

pass_n <- nrow(summary_table)
fail_n <- nrow(fail_table)
total_n <- nrow(dataset_registry)

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/33 — Forest Plots and Editable Tables",
  "",
  paste0("Run time: ", now_text()),
  paste0("Repository root: ", normalize_slash(repo_root)),
  "",
  "INPUT",
  "--------------------------------------------------",
  if (!is.null(manifest)) {
    paste0("Manifest used: ", normalize_slash(manifest_path))
  } else {
    "Manifest used: NO (fallback dataset discovery)"
  },
  paste0("Analysis groups detected: ", total_n),
  paste0(
    "Completed additional analyses detected: ",
    sum(
      dataset_registry$analysis_type == "additional_analysis",
      na.rm = TRUE
    )
  ),
  paste0("PASS groups: ", pass_n),
  paste0("FAIL groups: ", fail_n),
  paste0("Study labels AUTO-resolved: ", auto_label_n),
  paste0("Study labels USER REQUIRED: ", user_required_label_n),
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0("Summary CSV: ", normalize_slash(summary_csv)),
  paste0("Study CSV: ", normalize_slash(study_csv)),
  paste0("Workbook: ", normalize_slash(xlsx_file)),
  paste0(
    "Study-label crosswalk: ",
    normalize_slash(study_label_crosswalk_file)
  ),
  paste0("Figures directory: ", normalize_slash(figures_dir)),
  "",
  "FIGURE OUTPUT POLICY",
  "--------------------------------------------------",
  "For each successful analysis group, R/33 created:",
  "- a high-resolution PNG forest plot",
  "- a vector PDF forest plot",
  "",
  "TABLE OUTPUT POLICY",
  "--------------------------------------------------",
  "Editable outputs include:",
  "- pooled summary table",
  "- study-level effects table",
  "- figure manifest",
  if (nrow(fail_table) > 0L) "- failure log sheet" else "- no failures were recorded",
  "",
  "STATUS",
  "--------------------------------------------------",
  if (pass_n > 0L && fail_n == 0L) {
    "PASS: R/33 completed all executable forest-plot/table outputs."
  } else if (pass_n > 0L && fail_n > 0L) {
    "PARTIAL PASS: R/33 generated outputs for one or more groups, but some groups failed."
  } else {
    "FAIL: R/33 did not generate any successful analysis output."
  }
)

writeLines(
  report_lines,
  report_file
)


# ============================================================
# 9. Console summary
# ============================================================

cat("============================================================\n")
cat("R/33 — Forest Plots and Editable Tables\n")
cat("============================================================\n\n")

cat("Analysis groups: ", total_n, "\n", sep = "")
cat("PASS: ", pass_n, "  FAIL: ", fail_n, "\n\n", sep = "")

if (nrow(summary_table) > 0L) {
  print(summary_table, n = Inf, width = Inf)
  cat("\n")
}

if (nrow(fail_table) > 0L) {
  cat("Failure summary:\n")
  print(fail_table, n = Inf, width = Inf)
  cat("\n")
}

if (pass_n > 0L && fail_n == 0L) {
  cat("PASS: R/33 completed all executable forest-plot/table outputs.\n")
} else if (pass_n > 0L && fail_n > 0L) {
  cat("PARTIAL PASS: R/33 generated outputs for one or more groups, but some groups failed.\n")
} else {
  stop(
    "R/33 FAIL: no forest plot or summary table was generated successfully.",
    call. = FALSE
  )
}

cat("Summary CSV: ", normalize_slash(summary_csv), "\n", sep = "")
cat("Study CSV: ", normalize_slash(study_csv), "\n", sep = "")
cat("Workbook: ", normalize_slash(xlsx_file), "\n", sep = "")
cat("Report: ", normalize_slash(report_file), "\n", sep = "")
