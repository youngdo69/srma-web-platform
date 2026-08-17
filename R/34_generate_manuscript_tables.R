# ============================================================
# Universal SR-MA Agent Toolkit
# R/34_generate_manuscript_tables.R
#
# VERSION: UNIVERSAL-R34-2026-08-16C
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
# - The author-editable workbook is never overwritten automatically
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
      "lead_author"
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

    study_label <- if (
      !is.na(first_author) &&
      first_author != "" &&
      !is.na(publication_year) &&
      publication_year != ""
    ) {
      paste(
        strip_first_author(first_author),
        publication_year
      )
    } else if (
      !is.na(first_author) &&
      first_author != ""
    ) {
      strip_first_author(first_author)
    } else {
      key_i
    }

    tibble::tibble(
      .study_key = key_i,
      first_author = first_author,
      publication_year = publication_year,
      study_label = study_label,

      country = get_value("country"),
      centers = get_value("centers"),
      study_period = get_value("study_period"),
      study_design = get_value("study_design"),
      sample_size = get_value("sample_size"),
      adjustment = get_value("adjustment"),
      follow_up = get_value("follow_up"),

      intervention_n = get_value("intervention_n"),
      comparator_n = get_value("comparator_n"),
      intervention_age = get_value("intervention_age"),
      comparator_age = get_value("comparator_age"),
      intervention_female = get_value("intervention_female"),
      comparator_female = get_value("comparator_female"),
      intervention_diabetes = get_value("intervention_diabetes"),
      comparator_diabetes = get_value("comparator_diabetes"),
      intervention_hypertension = get_value("intervention_hypertension"),
      comparator_hypertension = get_value("comparator_hypertension"),
      intervention_renal = get_value("intervention_renal"),
      comparator_renal = get_value("comparator_renal"),
      intervention_lvef = get_value("intervention_lvef"),
      comparator_lvef = get_value("comparator_lvef"),

      intervention_name = get_value("intervention_name"),
      dose = get_value("dose"),
      prior_use = get_value("prior_use"),
      initiation_timing = get_value("initiation_timing"),
      preprocedure_management = get_value("preprocedure_management"),
      postprocedure_management = get_value("postprocedure_management"),
      intervention_duration = get_value("intervention_duration"),
      comparator_description = get_value("comparator_description"),
      cointerventions = get_value("cointerventions")
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

table3 <- resolved |>
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
    "manuscript_tables_author_editable.xlsx is the author working copy and is never overwritten automatically once created.",
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

openxlsx::saveWorkbook(
  wb,
  generated_xlsx,
  overwrite = TRUE
)


# ============================================================
# 16. Author-editable copy
# ============================================================

author_copy_created <- FALSE

if (!file.exists(author_xlsx)) {

  author_copy_created <- isTRUE(
    file.copy(
      generated_xlsx,
      author_xlsx,
      overwrite = FALSE
    )
  )
}


# ============================================================
# 17. Report
# ============================================================

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
  "TABLE 3 QUALITY CHECK",
  "--------------------------------------------------",
  paste0(
    "Table 3 NR cells: ",
    nr_count_table3
  ),
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
} else {
  cat(
    "Existing author-editable workbook preserved; it was NOT overwritten.\n"
  )
}

cat(
  "PASS: R/34 generated one-row-per-included-study manuscript tables.\n"
)
