# ============================================================
# Universal SR-MA Agent Toolkit
# R/34_generate_manuscript_tables.R
#
# VERSION: UNIVERSAL-R34-2026-09-25E
#
# PURPOSE
# ------------------------------------------------------------
# Generate three standard, editable manuscript tables from the
# canonical INCLUDED-STUDY universe and structured extraction data:
#
#   Table 1. Study characteristics
#   Table 2. Patient / baseline characteristics
#   Table 3. Intervention / exposure characteristics
#
# DESIGN PRINCIPLES
# ------------------------------------------------------------
# - Universal / topic-agnostic core
# - One row per included study
# - Never builds manuscript tables by blindly row-binding derived
#   meta-analysis or additional-analysis result files
# - Uses stable study identifiers to join included studies to extraction data
# - Uses purpose-specific field resolution for study, patient, and
#   intervention characteristics
# - Human-verified/final fields are preferred when present
# - Missing values remain NR rather than being invented
# - Authors may freely edit and save the author-editable workbook
# - The author-editable workbook is refreshed automatically only when it still matches
#   the prior generated workbook exactly; otherwise author edits are preserved
#
# OUTPUTS
# ------------------------------------------------------------
# results/manuscript/tables/
#   table1_study_characteristics.csv
#   table2_patient_characteristics.csv
#   table3_intervention_characteristics.csv
#   manuscript_tables_generated.xlsx
#   manuscript_tables_author_editable.xlsx
#   r34_manuscript_tables_report.txt
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
# 1. General helpers
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

  x <- as.character(x)
  x <- x[!is.na(x) & trimws(x) != ""]

  if (length(x) == 0L) {
    return(default)
  }

  trimws(x[[1]])
}

first_existing_col <- function(data, candidates) {
  hits <- candidates[candidates %in% names(data)]

  if (length(hits) == 0L) {
    return(NA_character_)
  }

  hits[[1]]
}

as_character_table <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }

  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ as.character(.x)
      )
    )
}

is_valid_xlsx_container <- function(path) {

  if (!file.exists(path)) {
    return(FALSE)
  }

  ext <- tolower(tools::file_ext(path))

  if (!ext %in% c("xlsx", "xlsm")) {
    return(TRUE)
  }

  con <- file(path, open = "rb")

  on.exit(
    close(con),
    add = TRUE
  )

  sig <- tryCatch(
    readBin(
      con,
      what = "raw",
      n = 2L
    ),
    error = function(e) {
      raw(0)
    }
  )

  length(sig) == 2L &&
    identical(
      as.integer(sig),
      c(80L, 75L)
    )
}

read_tabular <- function(path) {

  ext <- tolower(
    tools::file_ext(path)
  )

  tryCatch(
    {
      if (ext == "csv") {
        return(
          readr::read_csv(
            path,
            show_col_types = FALSE,
            progress = FALSE,
            guess_max = 10000
          ) |>
            as_character_table()
        )
      }

      if (ext %in% c("xlsx", "xlsm")) {

        if (!is_valid_xlsx_container(path)) {
          return(NULL)
        }

        x <- suppressWarnings(
          openxlsx::read.xlsx(
            path,
            sheet = 1,
            detectDates = TRUE,
            check.names = FALSE
          )
        )

        return(
          tibble::as_tibble(x) |>
            as_character_table()
        )
      }

      if (ext == "rds") {

        obj <- readRDS(path)

        if (is.data.frame(obj)) {
          return(
            tibble::as_tibble(obj) |>
              as_character_table()
          )
        }
      }

      NULL
    },
    warning = function(w) {
      NULL
    },
    error = function(e) {
      NULL
    }
  )
}

clean_value <- function(x) {

  x <- trimws(as.character(x))

  bad <- is.na(x) |
    x == "" |
    tolower(x) %in% c(
      "na",
      "n/a",
      "nan",
      "null",
      "unknown",
      "not available",
      "not reported"
    )

  x[bad] <- NA_character_
  x
}

to_nr <- function(x) {

  x <- clean_value(x)
  x[is.na(x)] <- "NR"
  x
}

extract_year <- function(x) {

  out <- stringr::str_extract(
    as.character(x),
    "(?:19|20)[0-9]{2}"
  )

  out[is.na(out) | out == ""] <- NA_character_
  out
}

strip_first_author <- function(x) {

  x <- trimws(as.character(x))

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

  # Remove trailing initials: Snel LIP -> Snel
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

  stringr::str_squish(x)
}

collapse_unique <- function(x, sep = "; ") {

  x <- clean_value(x)
  x <- unique(x[!is.na(x)])

  if (length(x) == 0L) {
    return(NA_character_)
  }

  paste(x, collapse = sep)
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
    "config/project_config.yml was not found. Run R/34 from the repository root.",
    call. = FALSE
  )
}

config <- yaml::read_yaml(
  config_file
)

if (is.null(config)) {
  stop(
    "project_config.yml could not be parsed.",
    call. = FALSE
  )
}


# ============================================================
# 3. Canonical study identity
# ============================================================

study_key_specs <- list(
  screening_id = c(
    "screening_id",
    "screen_id"
  ),
  study_id = c(
    "study_id",
    "study_code"
  ),
  pmid = c(
    "pmid",
    "pubmed_id"
  ),
  doi = c(
    "doi"
  ),
  registration_id = c(
    "trial_registration",
    "registration_number",
    "registry_id",
    "nct_id",
    "clinicaltrials_id"
  ),
  title = c(
    "study_title",
    "article_title",
    "title"
  )
)

make_study_key <- function(df) {

  n <- nrow(df)
  key <- rep(NA_character_, n)
  key_type <- rep(NA_character_, n)

  for (key_type_i in names(study_key_specs)) {

    col_i <- first_existing_col(
      df,
      study_key_specs[[key_type_i]]
    )

    if (is.na(col_i)) {
      next
    }

    values <- clean_value(
      df[[col_i]]
    )

    use <- is.na(key) &
      !is.na(values)

    if (key_type_i == "doi") {
      values <- tolower(values)
    }

    key[use] <- paste0(
      key_type_i,
      "::",
      values[use]
    )

    key_type[use] <- key_type_i
  }

  list(
    key = key,
    key_type = key_type
  )
}


# ============================================================
# 4. Discover canonical included-study registry
# ============================================================

discover_included_registry <- function(repo_root) {

  search_roots <- c(
    file.path(
      repo_root,
      "data",
      "processed",
      "full_text"
    ),
    file.path(
      repo_root,
      "data",
      "processed"
    )
  )

  search_roots <- search_roots[
    dir.exists(search_roots)
  ]

  candidates <- unique(unlist(
    lapply(
      search_roots,
      function(root_i) {

        list.files(
          root_i,
          pattern = "included.*stud.*\\.csv$|final.*included.*\\.csv$",
          recursive = TRUE,
          full.names = TRUE,
          ignore.case = TRUE
        )
      }
    )
  ))

  candidates <- normalize_slash(
    candidates
  )

  if (length(candidates) == 0L) {
    return(NULL)
  }

  scored <- lapply(
    candidates,
    function(path_i) {

      x <- read_tabular(
        path_i
      )

      if (
        is.null(x) ||
        nrow(x) == 0L
      ) {
        return(NULL)
      }

      id_cols <- sum(
        vapply(
          unlist(study_key_specs),
          function(nm) {
            nm %in% names(x)
          },
          logical(1)
        )
      )

      list(
        path = path_i,
        data = x,
        score = id_cols + 10
      )
    }
  )

  scored <- scored[
    !vapply(
      scored,
      is.null,
      logical(1)
    )
  ]

  if (length(scored) == 0L) {
    return(NULL)
  }

  ord <- order(
    vapply(
      scored,
      function(z) {
        z$score
      },
      numeric(1)
    ),
    decreasing = TRUE
  )

  chosen <- scored[[ord[[1]]]]

  x <- chosen$data

  key_bundle <- make_study_key(
    x
  )

  x$.study_key <- key_bundle$key
  x$.study_key_type <- key_bundle$key_type
  x$.included_registry_source <- chosen$path

  # Keep only rows with resolvable study identity.
  x <- x |>
    dplyr::filter(
      !is.na(.study_key),
      .study_key != ""
    ) |>
    dplyr::distinct(
      .study_key,
      .keep_all = TRUE
    )

  if (nrow(x) == 0L) {
    return(NULL)
  }

  x
}


# ============================================================
# 5. Discover extraction-layer tables only
# ============================================================

discover_extraction_tables <- function(repo_root) {

  extraction_roots <- c(
    file.path(
      repo_root,
      "data",
      "processed",
      "extraction"
    ),
    file.path(
      repo_root,
      "results",
      "extraction",
      "targeted_human_verification"
    )
  )

  extraction_roots <- extraction_roots[
    dir.exists(extraction_roots)
  ]

  if (length(extraction_roots) == 0L) {
    return(
      tibble::tibble()
    )
  }

  files <- unique(unlist(
    lapply(
      extraction_roots,
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

  files <- normalize_slash(
    files
  )

  # Exclude explicitly outcome-only files when constructing characteristics.
  # They may be meta-analysis inputs but should not generate 100+ manuscript
  # characteristic rows.
  files <- files[
    !grepl(
      "outcome_data|meta_analysis|additional_analysis|forest|summary",
      basename(files),
      ignore.case = TRUE
    )
  ]

  rows <- lapply(
    files,
    function(path_i) {

      x <- read_tabular(
        path_i
      )

      if (
        is.null(x) ||
        nrow(x) == 0L
      ) {
        return(NULL)
      }

      key_bundle <- make_study_key(
        x
      )

      x$.study_key <- key_bundle$key
      x$.study_key_type <- key_bundle$key_type
      x$.source_file <- path_i

      # Prefer human-verified/final characteristic tables, then structured
      # study/group characteristic tables.
      fname <- tolower(
        basename(path_i)
      )

      source_rank <- dplyr::case_when(
        grepl(
          "human|verified|final",
          fname
        ) ~ 1L,
        grepl(
          "study_character",
          fname
        ) ~ 2L,
        grepl(
          "group_character|patient|baseline",
          fname
        ) ~ 3L,
        grepl(
          "intervention|exposure|treatment",
          fname
        ) ~ 4L,
        TRUE ~ 5L
      )

      x$.source_rank <- source_rank

      x
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
      tibble::tibble()
    )
  }

  # All imported fields are character, so this union is type-safe.
  dplyr::bind_rows(
    rows
  ) |>
    dplyr::filter(
      !is.na(.study_key),
      .study_key != ""
    )
}


# ============================================================
# 6. Fallback included-study universe if registry unavailable
# ============================================================

derive_included_registry_from_extraction <- function(extraction_pool) {

  if (
    is.null(extraction_pool) ||
    nrow(extraction_pool) == 0L
  ) {
    stop(
      "No included-studies registry and no usable extraction data were found.",
      call. = FALSE
    )
  }

  status_col <- first_existing_col(
    extraction_pool,
    c(
      "author_final_review",
      "final_decision",
      "eligibility_decision",
      "include",
      "included",
      "final_include"
    )
  )

  x <- extraction_pool

  if (!is.na(status_col)) {

    decision <- tolower(
      trimws(
        as.character(
          x[[status_col]]
        )
      )
    )

    keep <- decision %in% c(
      "include",
      "included",
      "yes",
      "y",
      "true",
      "1"
    )

    if (any(keep, na.rm = TRUE)) {
      x <- x[keep, , drop = FALSE]
    }
  }

  x |>
    dplyr::arrange(
      .source_rank
    ) |>
    dplyr::distinct(
      .study_key,
      .keep_all = TRUE
    )
}


# ============================================================
# 7. Flexible semantic field resolver
# ============================================================

field_specs <- list(

  # ----- bibliographic / study -------------------------------------
  first_author = list(
    exact = c(
      "first_author",
      "first_author_name",
      "lead_author",
      "authors",
      "author"
    ),
    regex = c(
      "^first.*author$",
      "^lead.*author$"
    ),
    exclude = c()
  ),

  publication_year = list(
    exact = c(
      "publication_year",
      "pub_year",
      "year"
    ),
    regex = c(
      "publication.*year",
      "^pub.*year$"
    ),
    exclude = c()
  ),

  country = list(
    exact = c(
      "country",
      "study_country",
      "country_region",
      "region"
    ),
    regex = c(
      "country",
      "geographic.*region"
    ),
    exclude = c()
  ),

  centers = list(
    exact = c(
      "number_of_centers",
      "n_centers",
      "centers",
      "centre_count",
      "center_count"
    ),
    regex = c(
      "number.*cent",
      "n.*cent",
      "multi.*cent",
      "single.*cent"
    ),
    exclude = c(
      "country"
    )
  ),

  study_period = list(
    exact = c(
      "study_period",
      "recruitment_period",
      "study_years",
      "period"
    ),
    regex = c(
      "study.*period",
      "recruit.*period",
      "enrol.*period"
    ),
    exclude = c()
  ),

  study_design = list(
    exact = c(
      "final_study_design",
      "study_design",
      "design"
    ),
    regex = c(
      "study.*design",
      "^design$"
    ),
    exclude = c()
  ),

  sample_size = list(
    exact = c(
      "total_sample_size",
      "sample_size",
      "n_total",
      "total_n"
    ),
    regex = c(
      "sample.*size",
      "^total.*n$",
      "^n.*total$"
    ),
    exclude = c(
      "event",
      "outcome"
    )
  ),

  adjustment = list(
    exact = c(
      "adjustment_method",
      "matching_method",
      "propensity_method",
      "adjustment",
      "final_adjustment_variables"
    ),
    regex = c(
      "adjust",
      "matching",
      "propensity"
    ),
    exclude = c(
      "outcome"
    )
  ),

  follow_up = list(
    exact = c(
      "follow_up",
      "followup",
      "follow_up_duration",
      "followup_duration"
    ),
    regex = c(
      "follow.*up"
    ),
    exclude = c()
  ),

  # ----- patient / baseline ----------------------------------------
  intervention_n = list(
    exact = c(
      "intervention_n",
      "n_intervention",
      "n_treatment",
      "n_exposed"
    ),
    regex = c(
      "intervention.*n$",
      "treatment.*n$",
      "exposed.*n$"
    ),
    exclude = c(
      "event",
      "outcome",
      "total.*event"
    )
  ),

  comparator_n = list(
    exact = c(
      "comparator_n",
      "n_comparator",
      "n_control"
    ),
    regex = c(
      "comparator.*n$",
      "control.*n$"
    ),
    exclude = c(
      "event",
      "outcome"
    )
  ),

  intervention_age = list(
    exact = c(
      "intervention_age",
      "treatment_age",
      "exposed_age"
    ),
    regex = c(
      "intervention.*age",
      "treatment.*age",
      "exposed.*age"
    ),
    exclude = c()
  ),

  comparator_age = list(
    exact = c(
      "comparator_age",
      "control_age"
    ),
    regex = c(
      "comparator.*age",
      "control.*age"
    ),
    exclude = c()
  ),

  intervention_female = list(
    exact = c(
      "intervention_female_percent",
      "treatment_female_percent"
    ),
    regex = c(
      "intervention.*female",
      "treatment.*female"
    ),
    exclude = c()
  ),

  comparator_female = list(
    exact = c(
      "comparator_female_percent",
      "control_female_percent"
    ),
    regex = c(
      "comparator.*female",
      "control.*female"
    ),
    exclude = c()
  ),

  intervention_diabetes = list(
    exact = c(
      "intervention_diabetes_percent",
      "treatment_diabetes_percent"
    ),
    regex = c(
      "intervention.*diabet",
      "treatment.*diabet"
    ),
    exclude = c()
  ),

  comparator_diabetes = list(
    exact = c(
      "comparator_diabetes_percent",
      "control_diabetes_percent"
    ),
    regex = c(
      "comparator.*diabet",
      "control.*diabet"
    ),
    exclude = c()
  ),

  intervention_hypertension = list(
    exact = c(
      "intervention_hypertension_percent",
      "treatment_hypertension_percent"
    ),
    regex = c(
      "intervention.*hypert",
      "treatment.*hypert"
    ),
    exclude = c()
  ),

  comparator_hypertension = list(
    exact = c(
      "comparator_hypertension_percent",
      "control_hypertension_percent"
    ),
    regex = c(
      "comparator.*hypert",
      "control.*hypert"
    ),
    exclude = c()
  ),

  intervention_renal = list(
    exact = c(
      "intervention_ckd",
      "intervention_renal_function",
      "intervention_egfr",
      "treatment_egfr"
    ),
    regex = c(
      "intervention.*ckd",
      "intervention.*egfr",
      "intervention.*renal",
      "treatment.*egfr"
    ),
    exclude = c(
      "outcome",
      "aki"
    )
  ),

  comparator_renal = list(
    exact = c(
      "comparator_ckd",
      "comparator_renal_function",
      "comparator_egfr",
      "control_egfr"
    ),
    regex = c(
      "comparator.*ckd",
      "comparator.*egfr",
      "comparator.*renal",
      "control.*egfr"
    ),
    exclude = c(
      "outcome",
      "aki"
    )
  ),

  intervention_lvef = list(
    exact = c(
      "intervention_lvef",
      "treatment_lvef",
      "intervention_hf"
    ),
    regex = c(
      "intervention.*lvef",
      "treatment.*lvef",
      "intervention.*heart.*failure"
    ),
    exclude = c()
  ),

  comparator_lvef = list(
    exact = c(
      "comparator_lvef",
      "control_lvef",
      "comparator_hf"
    ),
    regex = c(
      "comparator.*lvef",
      "control.*lvef",
      "comparator.*heart.*failure"
    ),
    exclude = c()
  ),

  # ----- intervention / exposure -----------------------------------
  intervention_name = list(
    exact = c(
      "intervention_or_exposure",
      "surgery_or_procedure",
      "intervention_name",
      "intervention_description",
      "treatment_description",
      "exposure_description",
      "drug_name",
      "intervention",
      "treatment",
      "exposure"
    ),
    regex = c(
      "^intervention.*(name|description|regimen)$",
      "^treatment.*(name|description|regimen)$",
      "^exposure.*(name|description)$",
      "^drug.*name$"
    ),
    exclude = c(
      "event",
      "total",
      "mean",
      "sd",
      "outcome",
      "effect",
      "label"
    )
  ),

  dose = list(
    exact = c(
      "dose",
      "intervention_dose",
      "treatment_dose",
      "dose_regimen",
      "exposure_intensity"
    ),
    regex = c(
      "dose",
      "dosing",
      "intensity"
    ),
    exclude = c(
      "outcome"
    )
  ),

  prior_use = list(
    exact = c(
      "pre_existing_use",
      "preexisting_use",
      "prior_use",
      "baseline_exposure_status",
      "de_novo_initiation"
    ),
    regex = c(
      "pre.*existing.*use",
      "prior.*use",
      "baseline.*use",
      "de.*novo"
    ),
    exclude = c()
  ),

  initiation_timing = list(
    exact = c(
      "exposure_timing",
      "initiation_timing",
      "intervention_start_timing",
      "treatment_start",
      "exposure_start",
      "start_timing"
    ),
    regex = c(
      "initiat.*tim",
      "start.*tim",
      "timing.*initiat",
      "preop.*start"
    ),
    exclude = c(
      "outcome"
    )
  ),

  preprocedure_management = list(
    exact = c(
      "preoperative_management",
      "pre_procedure_management",
      "withholding_strategy",
      "continuation_strategy",
      "preoperative_continuation",
      "preoperative_withholding"
    ),
    regex = c(
      "preop.*management",
      "pre.*procedure.*management",
      "withhold",
      "continuation.*strategy",
      "discontinu"
    ),
    exclude = c(
      "outcome"
    )
  ),

  postprocedure_management = list(
    exact = c(
      "postoperative_resumption",
      "postoperative_initiation",
      "post_procedure_management",
      "resumption_timing",
      "restart_timing"
    ),
    regex = c(
      "postop.*resum",
      "postop.*initiat",
      "post.*procedure.*management",
      "restart",
      "resum.*tim"
    ),
    exclude = c(
      "outcome"
    )
  ),

  intervention_duration = list(
    exact = c(
      "intervention_duration",
      "treatment_duration",
      "exposure_duration",
      "duration"
    ),
    regex = c(
      "intervention.*duration",
      "treatment.*duration",
      "exposure.*duration"
    ),
    exclude = c(
      "follow",
      "outcome"
    )
  ),

  comparator_description = list(
    exact = c(
      "comparator_description",
      "control_description",
      "comparator",
      "control_group",
      "control"
    ),
    regex = c(
      "^comparator.*description$",
      "^control.*description$",
      "^comparator$",
      "^control.*group$"
    ),
    exclude = c(
      "event",
      "total",
      "mean",
      "sd",
      "outcome",
      "effect",
      "label"
    )
  ),

  cointerventions = list(
    exact = c(
      "co_interventions",
      "cointerventions",
      "concomitant_treatment"
    ),
    regex = c(
      "co.?intervention",
      "concomitant.*treat"
    ),
    exclude = c()
  )
)

candidate_columns <- function(df, spec) {

  nm <- names(df)
  nm_lower <- tolower(nm)

  exact_hits <- spec$exact[
    tolower(spec$exact) %in% nm_lower
  ]

  exact_cols <- character()

  for (hit in exact_hits) {
    idx <- which(
      nm_lower == tolower(hit)
    )

    if (length(idx) > 0L) {
      exact_cols <- c(
        exact_cols,
        nm[idx[[1]]]
      )
    }
  }

  regex_cols <- character()

  for (pat in spec$regex) {
    idx <- grepl(
      pat,
      nm_lower,
      perl = TRUE
    )

    regex_cols <- c(
      regex_cols,
      nm[idx]
    )
  }

  cols <- unique(
    c(
      exact_cols,
      regex_cols
    )
  )

  if (length(spec$exclude) > 0L && length(cols) > 0L) {

    bad <- rep(
      FALSE,
      length(cols)
    )

    for (pat in spec$exclude) {
      bad <- bad |
        grepl(
          pat,
          tolower(cols),
          perl = TRUE
        )
    }

    cols <- cols[!bad]
  }

  cols
}

resolve_field <- function(study_rows, field_name) {

  if (
    is.null(study_rows) ||
    nrow(study_rows) == 0L
  ) {
    return(
      list(
        value = NA_character_,
        source = NA_character_,
        column = NA_character_
      )
    )
  }

  spec <- field_specs[[field_name]]

  if (is.null(spec)) {
    return(
      list(
        value = NA_character_,
        source = NA_character_,
        column = NA_character_
      )
    )
  }

  study_rows <- study_rows |>
    dplyr::arrange(
      .source_rank
    )

  cols <- candidate_columns(
    study_rows,
    spec
  )

  if (length(cols) == 0L) {
    return(
      list(
        value = NA_character_,
        source = NA_character_,
        column = NA_character_
      )
    )
  }

  for (col_i in cols) {

    values <- clean_value(
      study_rows[[col_i]]
    )

    ok <- which(
      !is.na(values)
    )

    if (length(ok) == 0L) {
      next
    }

    row_i <- ok[[1]]

    return(
      list(
        value = values[[row_i]],
        source = study_rows$.source_file[[row_i]],
        column = col_i
      )
    )
  }

  list(
    value = NA_character_,
    source = NA_character_,
    column = NA_character_
  )
}



# ============================================================
# 7B. Role-aware / derived characteristic helpers
# ============================================================

first_nonmissing_from_cols <- function(df, candidates) {

  if (is.null(df) || nrow(df) == 0L) {
    return(NA_character_)
  }

  cols <- candidates[candidates %in% names(df)]

  if (length(cols) == 0L) {
    return(NA_character_)
  }

  for (col_i in cols) {
    values <- clean_value(df[[col_i]])
    ok <- which(!is.na(values))
    if (length(ok) > 0L) {
      return(values[[ok[[1]]]])
    }
  }

  NA_character_
}


normalize_group_role <- function(x) {

  x <- tolower(stringr::str_squish(as.character(x)))

  out <- rep(NA_character_, length(x))

  out[
    grepl(
      "intervention|treatment|treated|exposed|experimental",
      x,
      perl = TRUE
    )
  ] <- "intervention"

  out[
    grepl(
      "comparator|control|reference|usual care|standard care",
      x,
      perl = TRUE
    )
  ] <- "comparator"

  out
}


group_rows_for_role <- function(study_rows, role) {

  if (is.null(study_rows) || nrow(study_rows) == 0L) {
    return(tibble::tibble())
  }

  role_col <- first_existing_col(
    study_rows,
    c(
      "group_role",
      "arm_role",
      "role"
    )
  )

  if (is.na(role_col)) {
    return(tibble::tibble())
  }

  role_values <- normalize_group_role(
    study_rows[[role_col]]
  )

  out <- study_rows[
    !is.na(role_values) & role_values == role,
    ,
    drop = FALSE
  ]

  if (nrow(out) == 0L) {
    return(tibble::tibble())
  }

  out |>
    dplyr::arrange(.source_rank)
}


plain_number <- function(x) {

  x <- clean_value(x)

  if (length(x) == 0L || is.na(x[[1]])) {
    return(FALSE)
  }

  grepl(
    "^[-+]?[0-9]+(?:\\.[0-9]+)?$",
    x[[1]],
    perl = TRUE
  )
}


first_numeric_token <- function(x) {

  x <- clean_value(x)

  if (length(x) == 0L || is.na(x[[1]])) {
    return(NA_real_)
  }

  token <- stringr::str_extract(
    x[[1]],
    "[0-9]+(?:\\.[0-9]+)?"
  )

  suppressWarnings(
    as.numeric(token)
  )
}


format_age_from_group <- function(group_rows) {

  age <- first_nonmissing_from_cols(
    group_rows,
    c(
      "age",
      "age_mean",
      "mean_age",
      "age_median",
      "baseline_age"
    )
  )

  if (is.na(age)) {
    return(NA_character_)
  }

  sd <- first_nonmissing_from_cols(
    group_rows,
    c(
      "age_sd",
      "sd_age"
    )
  )

  if (
    !is.na(sd) &&
    plain_number(age) &&
    plain_number(sd)
  ) {
    return(
      paste0(age, " ± ", sd)
    )
  }

  age
}


format_female_from_group <- function(group_rows) {

  n_text <- first_nonmissing_from_cols(
    group_rows,
    c(
      "sample_size",
      "group_n",
      "n"
    )
  )

  female <- first_nonmissing_from_cols(
    group_rows,
    c(
      "female_n",
      "female",
      "women_n",
      "women"
    )
  )

  if (!is.na(female)) {

    # Preserve already-informative count/percentage strings.
    if (
      grepl("%|/", female, perl = TRUE) ||
      !plain_number(female)
    ) {
      return(female)
    }

    n_value <- first_numeric_token(n_text)
    female_value <- suppressWarnings(as.numeric(female))

    if (
      !is.na(n_value) &&
      n_value > 0 &&
      !is.na(female_value) &&
      female_value >= 0 &&
      female_value <= n_value
    ) {
      pct <- 100 * female_value / n_value
      return(
        sprintf(
          "%g/%g (%.1f%%)",
          female_value,
          n_value,
          pct
        )
      )
    }

    return(female)
  }

  # Deterministic fallback: derive female count from total N and male count
  # only when both are explicitly extractable.
  male <- first_nonmissing_from_cols(
    group_rows,
    c(
      "male_n",
      "male",
      "men_n",
      "men"
    )
  )

  n_value <- first_numeric_token(n_text)
  male_value <- first_numeric_token(male)

  if (
    !is.na(n_value) &&
    n_value > 0 &&
    !is.na(male_value) &&
    male_value >= 0 &&
    male_value <= n_value
  ) {

    female_value <- n_value - male_value
    pct <- 100 * female_value / n_value

    return(
      sprintf(
        "%g/%g (%.1f%%)",
        female_value,
        n_value,
        pct
      )
    )
  }

  NA_character_
}


extract_note_fragment <- function(x, keyword_pattern) {

  x <- clean_value(x)

  if (length(x) == 0L || all(is.na(x))) {
    return(NA_character_)
  }

  x <- x[!is.na(x)]

  for (txt in x) {

    parts <- unlist(
      strsplit(
        txt,
        "(?<=[.;])\\s+",
        perl = TRUE
      )
    )

    hit <- parts[
      grepl(
        keyword_pattern,
        parts,
        ignore.case = TRUE,
        perl = TRUE
      )
    ]

    if (length(hit) > 0L) {
      return(
        stringr::str_squish(hit[[1]])
      )
    }
  }

  NA_character_
}


derive_centers <- function(study_rows, explicit_value, study_design) {

  if (!is.na(explicit_value) && explicit_value != "") {
    return(explicit_value)
  }

  setting <- first_nonmissing_from_cols(
    study_rows,
    c(
      "setting",
      "study_setting",
      "site_setting"
    )
  )

  combined <- paste(
    c(setting, study_design),
    collapse = " "
  )

  explicit_count <- stringr::str_extract(
    combined,
    "\\b[0-9]+\\s*(?:centers?|centres?)\\b"
  )

  if (!is.na(explicit_count) && explicit_count != "") {
    return(explicit_count)
  }

  if (
    grepl(
      "single[- ]?cent(?:er|re)",
      combined,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    return("1")
  }

  if (
    grepl(
      "multi[- ]?cent(?:er|re)",
      combined,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    return("Multicentre; number NR")
  }

  NA_character_
}


derive_adjustment <- function(study_rows, explicit_value) {

  if (!is.na(explicit_value) && explicit_value != "") {
    return(explicit_value)
  }

  candidates <- c(
    first_nonmissing_from_cols(
      study_rows,
      c(
        "study_design",
        "analysis_population",
        "extraction_notes",
        "overall_extraction_notes"
      )
    ),
    first_nonmissing_from_cols(
      study_rows,
      c(
        "baseline_characteristics_notes"
      )
    )
  )

  txt <- paste(
    candidates[!is.na(candidates)],
    collapse = " "
  )

  if (txt == "") {
    return(NA_character_)
  }

  if (
    grepl(
      "overlap weighting",
      txt,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    return("Propensity score overlap weighting")
  }

  if (
    grepl(
      "propensity score.*weight|propensity.*weight",
      txt,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    return("Propensity score weighting")
  }

  if (
    grepl(
      "propensity score matching|\\bPSM\\b",
      txt,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    return("Propensity score matching (PSM)")
  }

  if (
    grepl(
      "inverse probability.*weight|IPTW",
      txt,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    return("Inverse-probability weighting")
  }

  if (
    grepl(
      "multivariable|multivariate",
      txt,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    return("Multivariable adjustment")
  }

  NA_character_
}


resolve_group_baseline <- function(group_rows) {

  if (is.null(group_rows) || nrow(group_rows) == 0L) {
    return(
      list(
        n = NA_character_,
        age = NA_character_,
        female = NA_character_,
        diabetes = NA_character_,
        hypertension = NA_character_,
        renal = NA_character_,
        lvef_hf = NA_character_
      )
    )
  }

  notes <- first_nonmissing_from_cols(
    group_rows,
    c(
      "baseline_characteristics_notes",
      "baseline_notes",
      "notes"
    )
  )

  diabetes <- first_nonmissing_from_cols(
    group_rows,
    c(
      "baseline_diabetes_status",
      "diabetes_status",
      "diabetes",
      "diabetes_n",
      "diabetes_percent"
    )
  )

  hypertension <- first_nonmissing_from_cols(
    group_rows,
    c(
      "baseline_hypertension_status",
      "hypertension_status",
      "hypertension",
      "hypertension_n",
      "hypertension_percent"
    )
  )

  if (is.na(hypertension) && !is.na(notes)) {
    hypertension <- extract_note_fragment(
      notes,
      "hypertension|\\bHTN\\b"
    )
  }

  renal <- first_nonmissing_from_cols(
    group_rows,
    c(
      "baseline_renal_function",
      "renal_function",
      "baseline_ckd",
      "ckd",
      "egfr",
      "creatinine"
    )
  )

  lvef_hf <- first_nonmissing_from_cols(
    group_rows,
    c(
      "baseline_lvef",
      "lvef",
      "baseline_heart_failure",
      "heart_failure",
      "hf_status"
    )
  )

  if (is.na(lvef_hf) && !is.na(notes)) {
    lvef_hf <- extract_note_fragment(
      notes,
      "LVEF|ejection fraction|heart failure|\\bHF\\b"
    )
  }

  list(
    n = first_nonmissing_from_cols(
      group_rows,
      c(
        "sample_size",
        "group_n",
        "n"
      )
    ),
    age = format_age_from_group(group_rows),
    female = format_female_from_group(group_rows),
    diabetes = diabetes,
    hypertension = hypertension,
    renal = renal,
    lvef_hf = lvef_hf
  )
}


group_source_has_baseline_values <- function(group_rows) {

  if (is.null(group_rows) || nrow(group_rows) == 0L) {
    return(FALSE)
  }

  candidates <- c(
    "sample_size",
    "group_n",
    "n",
    "age",
    "age_mean",
    "age_median",
    "female_n",
    "female",
    "male_n",
    "male",
    "baseline_renal_function",
    "baseline_diabetes_status",
    "baseline_hypertension_status",
    "baseline_lvef",
    "lvef",
    "baseline_characteristics_notes"
  )

  cols <- candidates[candidates %in% names(group_rows)]

  if (length(cols) == 0L) {
    return(FALSE)
  }

  any(
    vapply(
      cols,
      function(col_i) {
        any(!is.na(clean_value(group_rows[[col_i]])))
      },
      logical(1)
    )
  )
}


# ============================================================
# 8. Load included universe + extraction pool
# ============================================================

extraction_pool <- discover_extraction_tables(
  repo_root
)

included_registry <- discover_included_registry(
  repo_root
)

if (is.null(included_registry)) {

  included_registry <- derive_included_registry_from_extraction(
    extraction_pool
  )

  registry_mode <- "FALLBACK_FROM_EXTRACTION"

} else {

  registry_mode <- "CANONICAL_INCLUDED_STUDIES"
}

included_keys <- included_registry$.study_key

# Restrict characteristic extraction strictly to included studies.
extraction_pool <- extraction_pool |>
  dplyr::filter(
    .study_key %in% included_keys
  )

if (nrow(extraction_pool) == 0L) {
  stop(
    "Included studies were identified, but no matching structured extraction records were found.",
    call. = FALSE
  )
}


# ============================================================
# 9. Resolve one canonical row per included study
# ============================================================

resolved_rows <- lapply(
  included_keys,
  function(key_i) {

    study_rows <- extraction_pool |>
      dplyr::filter(
        .study_key == key_i
      )

    registry_row <- included_registry |>
      dplyr::filter(
        .study_key == key_i
      ) |>
      dplyr::slice(1L)

    get_value <- function(field) {
      resolve_field(
        study_rows,
        field
      )$value
    }

    first_author <- get_value(
      "first_author"
    )

    publication_year <- get_value(
      "publication_year"
    )

    # Registry-level bibliographic fallback.
    if (
      is.na(first_author) ||
      first_author == ""
    ) {
      author_col <- first_existing_col(
        registry_row,
        c(
          "first_author",
          "first_author_name",
          "lead_author",
          "authors",
          "author",
          "citation_short"
        )
      )

      if (!is.na(author_col)) {
        first_author <- safe_chr(
          strip_first_author(
            registry_row[[author_col]]
          ),
          NA_character_
        )
      }
    }

    if (
      !is.na(first_author) &&
      first_author != ""
    ) {
      first_author <- strip_first_author(
        first_author
      )
    }

    if (
      is.na(publication_year) ||
      publication_year == ""
    ) {
      year_col <- first_existing_col(
        registry_row,
        c(
          "publication_year",
          "pub_year",
          "year",
          "citation_short"
        )
      )

      if (!is.na(year_col)) {
        publication_year <- safe_chr(
          extract_year(
            registry_row[[year_col]]
          ),
          NA_character_
        )
      }
    }

    study_design <- get_value(
      "study_design"
    )

    centers <- derive_centers(
      study_rows,
      get_value("centers"),
      study_design
    )

    adjustment <- derive_adjustment(
      study_rows,
      get_value("adjustment")
    )

    intervention_group <- group_rows_for_role(
      study_rows,
      "intervention"
    )

    comparator_group <- group_rows_for_role(
      study_rows,
      "comparator"
    )

    intervention_baseline <- resolve_group_baseline(
      intervention_group
    )

    comparator_baseline <- resolve_group_baseline(
      comparator_group
    )

    study_label <- if (
      !is.na(first_author) &&
      first_author != "" &&
      !is.na(publication_year) &&
      publication_year != ""
    ) {
      paste(
        first_author,
        publication_year
      )
    } else if (
      !is.na(first_author) &&
      first_author != ""
    ) {
      first_author
    } else {
      key_i
    }

    source_authors <- first_nonmissing_from_cols(
      study_rows,
      c(
        "authors",
        "author",
        "first_author",
        "lead_author"
      )
    )

    source_intervention <- first_nonmissing_from_cols(
      study_rows,
      c(
        "intervention_or_exposure",
        "surgery_or_procedure",
        "intervention_name",
        "intervention_description",
        "treatment_description"
      )
    )

    tibble::tibble(
      .study_key = key_i,
      first_author = first_author,
      publication_year = publication_year,
      study_label = study_label,

      country = get_value("country"),
      centers = centers,
      study_period = get_value("study_period"),
      study_design = study_design,
      sample_size = get_value("sample_size"),
      adjustment = adjustment,
      follow_up = get_value("follow_up"),

      intervention_n = intervention_baseline$n,
      comparator_n = comparator_baseline$n,
      intervention_age = intervention_baseline$age,
      comparator_age = comparator_baseline$age,
      intervention_female = intervention_baseline$female,
      comparator_female = comparator_baseline$female,
      intervention_diabetes = intervention_baseline$diabetes,
      comparator_diabetes = comparator_baseline$diabetes,
      intervention_hypertension = intervention_baseline$hypertension,
      comparator_hypertension = comparator_baseline$hypertension,
      intervention_renal = intervention_baseline$renal,
      comparator_renal = comparator_baseline$renal,
      intervention_lvef = intervention_baseline$lvef_hf,
      comparator_lvef = comparator_baseline$lvef_hf,

      intervention_name = get_value("intervention_name"),
      dose = get_value("dose"),
      prior_use = get_value("prior_use"),
      initiation_timing = get_value("initiation_timing"),
      preprocedure_management = get_value("preprocedure_management"),
      postprocedure_management = get_value("postprocedure_management"),
      intervention_duration = get_value("intervention_duration"),
      comparator_description = get_value("comparator_description"),
      cointerventions = get_value("cointerventions"),

      .source_authors_present = !is.na(source_authors),
      .source_group_baseline_present =
        group_source_has_baseline_values(intervention_group) ||
        group_source_has_baseline_values(comparator_group),
      .source_intervention_present = !is.na(source_intervention)
    )
  }
)

resolved <- dplyr::bind_rows(
  resolved_rows
)


# ============================================================
# 10. Table 1 — Study characteristics
# ============================================================

table1 <- resolved |>
  dplyr::transmute(
    `First author` = to_nr(
      strip_first_author(first_author)
    ),
    `Year` = to_nr(
      publication_year
    ),
    `Country / region` = to_nr(
      country
    ),
    `Centers` = to_nr(
      centers
    ),
    `Study period` = to_nr(
      study_period
    ),
    `Study design` = to_nr(
      study_design
    ),
    `Sample size` = to_nr(
      sample_size
    ),
    `Adjustment / matching` = to_nr(
      adjustment
    ),
    `Follow-up` = to_nr(
      follow_up
    )
  )


# ============================================================
# 11. Table 2 — Patient / baseline characteristics
# ============================================================

table2 <- resolved |>
  dplyr::transmute(
    `Study` = to_nr(
      study_label
    ),
    `Intervention N` = to_nr(
      intervention_n
    ),
    `Comparator N` = to_nr(
      comparator_n
    ),
    `Intervention age` = to_nr(
      intervention_age
    ),
    `Comparator age` = to_nr(
      comparator_age
    ),
    `Intervention female (%)` = to_nr(
      intervention_female
    ),
    `Comparator female (%)` = to_nr(
      comparator_female
    ),
    `Intervention diabetes (%)` = to_nr(
      intervention_diabetes
    ),
    `Comparator diabetes (%)` = to_nr(
      comparator_diabetes
    ),
    `Intervention hypertension (%)` = to_nr(
      intervention_hypertension
    ),
    `Comparator hypertension (%)` = to_nr(
      comparator_hypertension
    ),
    `Intervention CKD / renal function` = to_nr(
      intervention_renal
    ),
    `Comparator CKD / renal function` = to_nr(
      comparator_renal
    ),
    `Intervention LVEF / HF` = to_nr(
      intervention_lvef
    ),
    `Comparator LVEF / HF` = to_nr(
      comparator_lvef
    )
  )


# ============================================================
# 12. Table 3 — Intervention / exposure characteristics
# ============================================================

# IMPORTANT:
# This table is built only from included-study extraction records.
# Outcome tables, meta-analysis outputs, and old additional-analysis
# workspaces are never used to create intervention rows.

table3_full <- resolved |>
  dplyr::transmute(
    `Study` = to_nr(
      study_label
    ),
    `Intervention / exposure` = to_nr(
      intervention_name
    ),
    `Dose / intensity` = to_nr(
      dose
    ),
    `Pre-existing use / de novo initiation` = to_nr(
      prior_use
    ),
    `Initiation timing` = to_nr(
      initiation_timing
    ),
    `Pre-procedure management` = to_nr(
      preprocedure_management
    ),
    `Post-procedure resumption / initiation` = to_nr(
      postprocedure_management
    ),
    `Duration` = to_nr(
      intervention_duration
    ),
    `Comparator` = to_nr(
      comparator_description
    ),
    `Co-interventions` = to_nr(
      cointerventions
    )
  )

# Universal evidence-adaptive presentation:
# - Study, Intervention / exposure, and Comparator are always retained.
# - Optional intervention-characteristic columns are retained only when at
#   least one included study has a defensible non-NR value.
# This avoids topic-specific hardcoding and prevents pharmacologic-only
# columns from cluttering device/procedure reviews when no such data exist.
table3_required_cols <- c(
  "Study",
  "Intervention / exposure",
  "Comparator"
)

table3_optional_cols <- setdiff(
  names(table3_full),
  table3_required_cols
)

table3_informative_optional_cols <- table3_optional_cols[
  vapply(
    table3_full[table3_optional_cols],
    function(x) {
      any(
        !is.na(x) &
          trimws(as.character(x)) != "" &
          as.character(x) != "NR"
      )
    },
    logical(1)
  )
]

table3_keep_cols <- names(table3_full)[
  names(table3_full) %in%
    c(
      table3_required_cols,
      table3_informative_optional_cols
    )
]

table3 <- table3_full |>
  dplyr::select(
    dplyr::all_of(table3_keep_cols)
  )



# ============================================================
# 12B. Structured-data mapping integrity gate
# ============================================================

mapping_failures <- character()

for (i in seq_len(nrow(resolved))) {

  key_i <- resolved$.study_key[[i]]

  if (
    isTRUE(resolved$.source_authors_present[[i]]) &&
    identical(table1$`First author`[[i]], "NR")
  ) {
    mapping_failures <- c(
      mapping_failures,
      paste0(
        key_i,
        ": author metadata exists upstream but Table 1 First author resolved to NR."
      )
    )
  }

  if (
    isTRUE(resolved$.source_group_baseline_present[[i]]) &&
    all(
      unlist(
        table2[i, setdiff(names(table2), "Study"), drop = FALSE],
        use.names = FALSE
      ) == "NR"
    )
  ) {
    mapping_failures <- c(
      mapping_failures,
      paste0(
        key_i,
        ": group/baseline data exist upstream but all Table 2 characteristic cells resolved to NR."
      )
    )
  }

  if (
    isTRUE(resolved$.source_intervention_present[[i]]) &&
    identical(table3$`Intervention / exposure`[[i]], "NR")
  ) {
    mapping_failures <- c(
      mapping_failures,
      paste0(
        key_i,
        ": intervention/procedure data exist upstream but Table 3 Intervention / exposure resolved to NR."
      )
    )
  }
}

if (length(mapping_failures) > 0L) {
  stop(
    paste(
      c(
        "R/34 manuscript-table mapping integrity gate FAILED.",
        "Structured extraction values were detected upstream but were lost during manuscript-table construction.",
        "",
        mapping_failures
      ),
      collapse = "\n"
    ),
    call. = FALSE
  )
}


# ============================================================
# 13. Output directories
# ============================================================

output_dir <- file.path(
  repo_root,
  "results",
  "manuscript",
  "tables"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

table1_csv <- file.path(
  output_dir,
  "table1_study_characteristics.csv"
)

table2_csv <- file.path(
  output_dir,
  "table2_patient_characteristics.csv"
)

table3_csv <- file.path(
  output_dir,
  "table3_intervention_characteristics.csv"
)

generated_xlsx <- file.path(
  output_dir,
  "manuscript_tables_generated.xlsx"
)

author_xlsx <- file.path(
  output_dir,
  "manuscript_tables_author_editable.xlsx"
)

report_file <- file.path(
  output_dir,
  "r34_manuscript_tables_report.txt"
)


# ============================================================
# 14. Save CSVs
# ============================================================

readr::write_csv(
  table1,
  table1_csv,
  na = ""
)

readr::write_csv(
  table2,
  table2_csv,
  na = ""
)

readr::write_csv(
  table3,
  table3_csv,
  na = ""
)


# ============================================================
# 15. Build generated XLSX
# ============================================================

wb <- openxlsx::createWorkbook()

title_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fontSize = 12
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "bottom"
)

body_style <- openxlsx::createStyle(
  valign = "top",
  wrapText = TRUE
)

write_sheet <- function(
  wb,
  sheet_name,
  title,
  data
) {

  openxlsx::addWorksheet(
    wb,
    sheet_name
  )

  openxlsx::writeData(
    wb,
    sheet_name,
    title,
    startRow = 1,
    startCol = 1
  )

  openxlsx::addStyle(
    wb,
    sheet_name,
    title_style,
    rows = 1,
    cols = 1,
    gridExpand = TRUE
  )

  openxlsx::writeData(
    wb,
    sheet_name,
    data,
    startRow = 3,
    startCol = 1,
    headerStyle = header_style
  )

  if (nrow(data) > 0L) {

    openxlsx::addStyle(
      wb,
      sheet_name,
      body_style,
      rows = 4:(3 + nrow(data)),
      cols = seq_len(ncol(data)),
      gridExpand = TRUE,
      stack = TRUE
    )
  }

  openxlsx::freezePane(
    wb,
    sheet_name,
    firstActiveRow = 4,
    firstActiveCol = 2
  )

  # Controlled widths for manuscript editing.
  for (j in seq_len(ncol(data))) {

    width <- if (
      j == 1L
    ) {
      20
    } else if (
      grepl(
        "Intervention|Comparator|management|Co-interventions|Study design",
        names(data)[j],
        ignore.case = TRUE
      )
    ) {
      28
    } else {
      18
    }

    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = j,
      widths = width
    )
  }

  openxlsx::setRowHeights(
    wb,
    sheet_name,
    rows = 3,
    heights = 32
  )
}

write_sheet(
  wb,
  "Table1_Study",
  "Table 1. Study characteristics",
  table1
)

write_sheet(
  wb,
  "Table2_Patients",
  "Table 2. Patient / baseline characteristics",
  table2
)

write_sheet(
  wb,
  "Table3_Intervention",
  "Table 3. Intervention / exposure characteristics",
  table3
)

openxlsx::addWorksheet(
  wb,
  "Included_Study_Registry"
)

openxlsx::writeData(
  wb,
  "Included_Study_Registry",
  included_registry
)

openxlsx::addWorksheet(
  wb,
  "Resolved_Data_Audit"
)

openxlsx::writeData(
  wb,
  "Resolved_Data_Audit",
  resolved
)

openxlsx::addWorksheet(
  wb,
  "Author_Instructions"
)

instructions <- tibble::tibble(
  Item = c(
    "Generated workbook",
    "Author-editable workbook",
    "NR",
    "Corrections",
    "Reruns"
  ),
  Guidance = c(
    "manuscript_tables_generated.xlsx is generated from the current structured Toolkit data and may be overwritten on rerun.",
    "manuscript_tables_author_editable.xlsx is the author working copy. It is refreshed automatically only when it still matches the prior generated workbook exactly; otherwise author edits are preserved.",
    "NR means the structured extraction data did not provide a defensible value. No value is invented.",
    "Authors may directly correct, add, delete, reformat, merge headers, and add footnotes or references in the author-editable workbook.",
    "After authors have begun manual editing, use the author-editable workbook as the manuscript working file; rerunning R/34 updates only the generated workbook."
  )
)

openxlsx::writeData(
  wb,
  "Author_Instructions",
  instructions
)

# Determine whether the existing author-editable workbook was still an
# untouched copy of the previously generated workbook. If so, it is safe to
# refresh it automatically after regeneration; if not, preserve author edits.
author_was_in_sync <- FALSE

if (
  file.exists(author_xlsx) &&
  file.exists(generated_xlsx)
) {
  author_md5_before <- unname(
    tools::md5sum(author_xlsx)
  )

  generated_md5_before <- unname(
    tools::md5sum(generated_xlsx)
  )

  author_was_in_sync <- identical(
    author_md5_before,
    generated_md5_before
  )
}

openxlsx::saveWorkbook(
  wb,
  generated_xlsx,
  overwrite = TRUE
)


# ============================================================
# 16. Author-editable copy
# ============================================================

author_copy_created <- FALSE
author_copy_refreshed <- FALSE

if (!file.exists(author_xlsx)) {

  author_copy_created <- isTRUE(
    file.copy(
      generated_xlsx,
      author_xlsx,
      overwrite = FALSE
    )
  )

} else if (isTRUE(author_was_in_sync)) {

  author_copy_refreshed <- isTRUE(
    file.copy(
      generated_xlsx,
      author_xlsx,
      overwrite = TRUE
    )
  )
}


# ============================================================
# 17. Report
# ============================================================

nr_count_table1 <- sum(
  table1 == "NR",
  na.rm = TRUE
)

nr_count_table2 <- sum(
  table2 == "NR",
  na.rm = TRUE
)

nr_count_table3 <- sum(
  table3 == "NR",
  na.rm = TRUE
)

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/34 — Editable Manuscript Tables",
  "",
  paste0(
    "Repository root: ",
    normalize_slash(repo_root)
  ),
  paste0(
    "Included-study registry mode: ",
    registry_mode
  ),
  paste0(
    "Included studies: ",
    nrow(resolved)
  ),
  "",
  "TABLE ROW COUNTS",
  "--------------------------------------------------",
  paste0(
    "Table 1 rows: ",
    nrow(table1)
  ),
  paste0(
    "Table 2 rows: ",
    nrow(table2)
  ),
  paste0(
    "Table 3 rows: ",
    nrow(table3)
  ),
  "",
  "TABLE QUALITY CHECK",
  "--------------------------------------------------",
  paste0(
    "Table 1 NR cells: ",
    nr_count_table1
  ),
  paste0(
    "Table 2 NR cells: ",
    nr_count_table2
  ),
  paste0(
    "Table 3 NR cells: ",
    nr_count_table3
  ),
  "Mapping integrity gate: PASS — no detected upstream-to-table value loss.",
  paste0(
    "Table 3 retained columns: ",
    paste(names(table3), collapse = " | ")
  ),
  "Table 3 optional columns that are entirely NR are omitted automatically.",
  "Table 3 is constructed only from included-study extraction records.",
  "Meta-analysis outputs and additional-analysis workspaces are excluded.",
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0(
    "Generated workbook: ",
    normalize_slash(generated_xlsx)
  ),
  paste0(
    "Author-editable workbook: ",
    normalize_slash(author_xlsx)
  ),
  "",
  "STATUS",
  "--------------------------------------------------",
  "PASS: R/34 generated one-row-per-included-study manuscript tables."
)

writeLines(
  report_lines,
  report_file
)


# ============================================================
# 18. Console summary
# ============================================================

cat("============================================================\n")
cat("R/34 — Editable Manuscript Tables\n")
cat("============================================================\n\n")

cat(
  "Included-study registry mode: ",
  registry_mode,
  "\n",
  sep = ""
)

cat(
  "Included studies: ",
  nrow(resolved),
  "\n",
  sep = ""
)

cat(
  "Table 1 rows: ",
  nrow(table1),
  "\n",
  sep = ""
)

cat(
  "Table 2 rows: ",
  nrow(table2),
  "\n",
  sep = ""
)

cat(
  "Table 3 rows: ",
  nrow(table3),
  "\n",
  sep = ""
)

cat(
  "Table 1 NR cells: ",
  nr_count_table1,
  "\n",
  sep = ""
)

cat(
  "Table 2 NR cells: ",
  nr_count_table2,
  "\n",
  sep = ""
)

cat(
  "Table 3 NR cells: ",
  nr_count_table3,
  "\n\n",
  sep = ""
)

cat(
  "Generated workbook: ",
  normalize_slash(generated_xlsx),
  "\n",
  sep = ""
)

cat(
  "Author-editable workbook: ",
  normalize_slash(author_xlsx),
  "\n",
  sep = ""
)

if (author_copy_created) {
  cat(
    "Author-editable workbook created.\n"
  )
} else if (author_copy_refreshed) {
  cat(
    "Author-editable workbook refreshed automatically because it matched the prior generated workbook exactly.\n"
  )
} else {
  cat(
    "Existing author-editable workbook preserved because it may contain author edits.\n"
  )
}

cat(
  "PASS: R/34 generated one-row-per-included-study manuscript tables.\n"
)
