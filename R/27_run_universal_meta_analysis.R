# ============================================================
# Universal SR-MA Agent Toolkit
# R/27_run_universal_meta_analysis.R
# Revision: 2026-08-13 universal robustness fix
#   - rejects invalid numeric effect-measure contamination
#   - resolves measure from human/final/manifest safely
#   - uses stats::weights() S3 dispatch for rma objects
#   - removes tibble model-object shadowing bug
#
# PURPOSE
# ------------------------------------------------------------
# Canonical universal meta-analysis execution layer.
#
# INPUTS
#   data/processed/meta_analysis_ready/meta_analysis_manifest.csv
#   data/processed/meta_analysis_ready/meta_analysis_ready_<group>.csv
#
# OUTPUTS
#   results/meta_analysis/<analysis_group>/
#       meta_analysis_summary.csv
#       study_effects_used.csv
#       forest_plot.pdf
#       model_output.txt
#   results/meta_analysis/
#       meta_analysis_execution_summary.csv
#       meta_analysis_execution_summary.xlsx
#   results/logs/universal_meta_analysis_execution_report.txt
#
# GOVERNANCE
# ------------------------------------------------------------
# 1. Each analysis_group is analyzed independently.
# 2. RCT and observational evidence are never merged merely because
#    they share an outcome.
# 3. Mixed/incompatible effect measures are not silently transformed.
# 4. Human-verified, R/26-finalized datasets are the only quantitative
#    inputs.
# 5. Default model = random-effects using REML.
# 6. R/27 does not alter study inclusion decisions.
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "purrr",
  "openxlsx",
  "metafor"
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
    "\nInstall before running R/27.",
    call. = FALSE
  )
}


# ============================================================
# 1. Helpers
# ============================================================

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_squish(x)
  x[is.na(x) | x == ""] <- NA_character_
  x
}

safe_chr <- function(x) {
  x <- clean_text(x)

  if (length(x) == 0L || is.na(x[[1]])) {
    return("")
  }

  as.character(x[[1]])
}

safe_numeric <- function(x) {
  suppressWarnings(
    as.numeric(
      clean_text(x)
    )
  )
}

now_text <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

safe_file_component <- function(x) {
  x <- safe_chr(x)

  if (x == "") {
    x <- "unnamed_analysis"
  }

  x <- stringr::str_replace_all(
    x,
    "[^A-Za-z0-9._-]+",
    "_"
  )

  x <- stringr::str_replace_all(
    x,
    "_+",
    "_"
  )

  x <- stringr::str_replace_all(
    x,
    "^_|_$",
    ""
  )

  tolower(x)
}

first_existing_column <- function(data, candidates) {
  found <- candidates[
    candidates %in% names(data)
  ]

  if (length(found) == 0L) {
    return(NA_character_)
  }

  found[[1]]
}

normalize_effect_measure <- function(x) {
  raw <- safe_chr(x)

  if (raw == "") {
    return(NA_character_)
  }

  y <- toupper(raw)

  y <- stringr::str_replace_all(
    y,
    "[^A-Z0-9]+",
    ""
  )

  # IMPORTANT:
  # Never return arbitrary source text here. A numeric value such as
  # "64" can arise from a shifted/mis-mapped workbook field and must
  # never be interpreted as an effect measure.
  dplyr::case_when(
    y %in% c(
      "RR",
      "RISKRATIO",
      "RELATIVERISK"
    ) ~ "RR",

    y %in% c(
      "OR",
      "ODDSRATIO"
    ) ~ "OR",

    y %in% c(
      "HR",
      "HAZARDRATIO"
    ) ~ "HR",

    y %in% c(
      "RD",
      "RISKDIFFERENCE"
    ) ~ "RD",

    y %in% c(
      "MD",
      "MEANDIFFERENCE"
    ) ~ "MD",

    y %in% c(
      "SMD",
      "STANDARDIZEDMEANDIFFERENCE",
      "STANDARDISEDMEANDIFFERENCE"
    ) ~ "SMD",

    TRUE ~ NA_character_
  )
}

supported_effect_measures <- c(
  "RR",
  "OR",
  "HR",
  "RD",
  "MD",
  "SMD"
)

is_supported_effect_measure <- function(x) {
  z <- normalize_effect_measure(x)

  !is.na(z) &&
    z %in% supported_effect_measures
}

is_ratio_measure <- function(measure) {
  normalize_effect_measure(measure) %in% c(
    "RR",
    "OR",
    "HR"
  )
}

is_identity_measure <- function(measure) {
  normalize_effect_measure(measure) %in% c(
    "RD",
    "MD",
    "SMD"
  )
}

inverse_transform <- function(x, measure) {
  if (is_ratio_measure(measure)) {
    return(exp(x))
  }

  x
}

format_num <- function(x, digits = 3) {
  ifelse(
    is.na(x),
    NA_character_,
    formatC(
      x,
      digits = digits,
      format = "f"
    )
  )
}

value_from_candidates <- function(data, candidates) {
  col <- first_existing_column(
    data,
    candidates
  )

  if (is.na(col)) {
    return(
      rep(
        NA_character_,
        nrow(data)
      )
    )
  }

  data[[col]]
}


# ============================================================
# 2. Project context
# ============================================================

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


# ============================================================
# 3. Paths
# ============================================================

input_directory <- file.path(
  paths$project_root,
  "data",
  "processed",
  "meta_analysis_ready"
)

manifest_file <- file.path(
  input_directory,
  "meta_analysis_manifest.csv"
)

output_root <- file.path(
  paths$project_root,
  "results",
  "meta_analysis"
)

dir.create(
  output_root,
  recursive = TRUE,
  showWarnings = FALSE
)

execution_summary_csv <- file.path(
  output_root,
  "meta_analysis_execution_summary.csv"
)

execution_summary_xlsx <- file.path(
  output_root,
  "meta_analysis_execution_summary.xlsx"
)

report_file <- file.path(
  paths$logs_directory,
  "universal_meta_analysis_execution_report.txt"
)

if (!file.exists(manifest_file)) {
  stop(
    "R/26 manifest not found:\n",
    manifest_file,
    "\nRun R/26 successfully before R/27.",
    call. = FALSE
  )
}


# ============================================================
# 4. Read and validate manifest
# ============================================================

manifest <- readr::read_csv(
  manifest_file,
  show_col_types = FALSE
)

required_manifest_columns <- c(
  "analysis_group",
  "meta_analysis_role",
  "study_count",
  "effect_measure",
  "dataset_file"
)

missing_manifest_columns <- setdiff(
  required_manifest_columns,
  names(manifest)
)

if (length(missing_manifest_columns) > 0L) {
  stop(
    "meta_analysis_manifest.csv is missing required column(s):\n- ",
    paste(
      missing_manifest_columns,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}

if (nrow(manifest) == 0L) {
  stop(
    "meta_analysis_manifest.csv contains no analysis groups.",
    call. = FALSE
  )
}


# ============================================================
# 5. Effect-size preparation
# ============================================================

prepare_reported_effect <- function(data, measure) {

  est <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_effect_estimate",
        "effect_estimate"
      )
    )
  )

  lower <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_lower_ci",
        "lower_ci"
      )
    )
  )

  upper <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_upper_ci",
        "upper_ci"
      )
    )
  )

  if (
    any(
      !is.finite(est) |
        !is.finite(lower) |
        !is.finite(upper)
    )
  ) {
    stop(
      "Reported-effect input contains missing/non-numeric effect estimates or CIs.",
      call. = FALSE
    )
  }

  if (
    any(
      upper <= lower
    )
  ) {
    stop(
      "Reported-effect input contains invalid confidence intervals.",
      call. = FALSE
    )
  }

  if (is_ratio_measure(measure)) {

    if (
      any(
        est <= 0 |
          lower <= 0 |
          upper <= 0
      )
    ) {
      stop(
        "Ratio effect measures require positive estimate and CI values.",
        call. = FALSE
      )
    }

    yi <- log(est)

    sei <- (
      log(upper) -
        log(lower)
    ) /
      (2 * 1.96)

    vi <- sei^2

  } else if (is_identity_measure(measure)) {

    yi <- est

    sei <- (
      upper -
        lower
    ) /
      (2 * 1.96)

    vi <- sei^2

  } else {

    stop(
      "Unsupported reported effect measure: ",
      measure,
      call. = FALSE
    )
  }

  tibble::tibble(
    yi = yi,
    vi = vi,
    sei = sqrt(vi),
    effect_input_source = "reported_effect"
  )
}


prepare_raw_dichotomous <- function(data, measure) {

  ai <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_intervention_events",
        "intervention_events"
      )
    )
  )

  n1i <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_intervention_total",
        "intervention_total"
      )
    )
  )

  ci <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_comparator_events",
        "comparator_events"
      )
    )
  )

  n2i <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_comparator_total",
        "comparator_total"
      )
    )
  )

  if (
    any(
      !is.finite(ai) |
        !is.finite(n1i) |
        !is.finite(ci) |
        !is.finite(n2i)
    )
  ) {
    stop(
      "Raw dichotomous input contains missing/non-numeric event counts or totals.",
      call. = FALSE
    )
  }

  if (
    any(
      ai < 0 |
        ci < 0 |
        n1i <= 0 |
        n2i <= 0 |
        ai > n1i |
        ci > n2i
    )
  ) {
    stop(
      "Raw dichotomous input contains impossible counts or denominators.",
      call. = FALSE
    )
  }

  metafor_measure <- dplyr::case_when(
    measure == "RR" ~ "RR",
    measure == "OR" ~ "OR",
    measure == "RD" ~ "RD",
    TRUE ~ NA_character_
  )

  if (is.na(metafor_measure)) {
    stop(
      "Raw dichotomous data are not supported for effect measure ",
      measure,
      ".",
      call. = FALSE
    )
  }

  esc <- metafor::escalc(
    measure = metafor_measure,
    ai = ai,
    n1i = n1i,
    ci = ci,
    n2i = n2i,
    add = 0.5,
    to = "only0",
    drop00 = FALSE
  )

  tibble::tibble(
    yi = as.numeric(
      esc$yi
    ),
    vi = as.numeric(
      esc$vi
    ),
    sei = sqrt(
      as.numeric(
        esc$vi
      )
    ),
    effect_input_source = "raw_dichotomous"
  )
}


prepare_raw_continuous <- function(data, measure) {

  m1i <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_intervention_mean",
        "intervention_mean"
      )
    )
  )

  sd1i <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_intervention_sd",
        "intervention_sd"
      )
    )
  )

  n1i <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_intervention_total",
        "intervention_total",
        "final_intervention_n",
        "intervention_n"
      )
    )
  )

  m2i <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_comparator_mean",
        "comparator_mean"
      )
    )
  )

  sd2i <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_comparator_sd",
        "comparator_sd"
      )
    )
  )

  n2i <- safe_numeric(
    value_from_candidates(
      data,
      c(
        "final_comparator_total",
        "comparator_total",
        "final_comparator_n",
        "comparator_n"
      )
    )
  )

  if (
    any(
      !is.finite(m1i) |
        !is.finite(sd1i) |
        !is.finite(n1i) |
        !is.finite(m2i) |
        !is.finite(sd2i) |
        !is.finite(n2i)
    )
  ) {
    stop(
      "Raw continuous input contains missing/non-numeric means, SDs, or sample sizes.",
      call. = FALSE
    )
  }

  metafor_measure <- dplyr::case_when(
    measure == "MD" ~ "MD",
    measure == "SMD" ~ "SMD",
    TRUE ~ NA_character_
  )

  if (is.na(metafor_measure)) {
    stop(
      "Raw continuous data are not supported for effect measure ",
      measure,
      ".",
      call. = FALSE
    )
  }

  esc <- metafor::escalc(
    measure = metafor_measure,
    m1i = m1i,
    sd1i = sd1i,
    n1i = n1i,
    m2i = m2i,
    sd2i = sd2i,
    n2i = n2i
  )

  tibble::tibble(
    yi = as.numeric(
      esc$yi
    ),
    vi = as.numeric(
      esc$vi
    ),
    sei = sqrt(
      as.numeric(
        esc$vi
      )
    ),
    effect_input_source = "raw_continuous"
  )
}


prepare_effects <- function(data, measure) {

  measure <- normalize_effect_measure(
    measure
  )

  input_type <- clean_text(
    value_from_candidates(
      data,
      c(
        "meta_input_type"
      )
    )
  )

  if (
    all(
      input_type %in% c(
        "reported_effect",
        NA_character_
      )
    )
  ) {
    return(
      prepare_reported_effect(
        data,
        measure
      )
    )
  }

  if (
    all(
      input_type %in% c(
        "raw_dichotomous",
        NA_character_
      )
    )
  ) {
    return(
      prepare_raw_dichotomous(
        data,
        measure
      )
    )
  }

  if (
    all(
      input_type %in% c(
        "continuous",
        "raw_continuous",
        NA_character_
      )
    )
  ) {
    return(
      prepare_raw_continuous(
        data,
        measure
      )
    )
  }

  # Mixed raw dichotomous + reported effects can still be harmonized
  # when they share the SAME effect measure.
  if (
    all(
      input_type %in% c(
        "raw_dichotomous",
        "reported_effect",
        NA_character_
      )
    )
  ) {

    rows <- lapply(
      seq_len(
        nrow(data)
      ),
      function(i) {

        one <- data[
          i,
          ,
          drop = FALSE
        ]

        type_i <- safe_chr(
          one$meta_input_type
        )

        if (type_i == "raw_dichotomous") {
          return(
            prepare_raw_dichotomous(
              one,
              measure
            )
          )
        }

        prepare_reported_effect(
          one,
          measure
        )
      }
    )

    return(
      dplyr::bind_rows(
        rows
      )
    )
  }

  stop(
    "Incompatible meta_input_type values within analysis group: ",
    paste(
      unique(
        input_type
      ),
      collapse = ", "
    ),
    call. = FALSE
  )
}


# ============================================================
# 6. Study labels
# ============================================================

make_study_labels <- function(data) {

  label_col <- first_existing_column(
    data,
    c(
      "study_label",
      "citation",
      "author_year",
      "study_name",
      "screening_id"
    )
  )

  if (is.na(label_col)) {
    return(
      paste0(
        "Study ",
        seq_len(
          nrow(data)
        )
      )
    )
  }

  labels <- clean_text(
    data[[label_col]]
  )

  labels[
    is.na(labels)
  ] <- paste0(
    "Study ",
    which(
      is.na(labels)
    )
  )

  make.unique(
    labels
  )
}


# ============================================================
# 7. Analyze one group
# ============================================================

run_one_group <- function(manifest_row) {

  group_name <- safe_chr(
    manifest_row$analysis_group
  )

  role <- safe_chr(
    manifest_row$meta_analysis_role
  )

  manifest_measure <- normalize_effect_measure(
    manifest_row$effect_measure
  )

  dataset_file <- safe_chr(
    manifest_row$dataset_file
  )

  group_slug <- safe_file_component(
    group_name
  )

  group_output_dir <- file.path(
    output_root,
    group_slug
  )

  dir.create(
    group_output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  summary_file <- file.path(
    group_output_dir,
    "meta_analysis_summary.csv"
  )

  study_effect_file <- file.path(
    group_output_dir,
    "study_effects_used.csv"
  )

  forest_file <- file.path(
    group_output_dir,
    "forest_plot.pdf"
  )

  model_text_file <- file.path(
    group_output_dir,
    "model_output.txt"
  )

  if (!file.exists(dataset_file)) {

    return(
      tibble::tibble(
        analysis_group = group_name,
        meta_analysis_role = role,
        status = "FAIL",
        reason = paste0(
          "Dataset not found: ",
          dataset_file
        ),
        studies = NA_integer_,
        effect_measure = manifest_measure,
        pooled_effect = NA_real_,
        lower_ci = NA_real_,
        upper_ci = NA_real_,
        p_value = NA_real_,
        tau2 = NA_real_,
        i2 = NA_real_,
        q = NA_real_,
        q_p_value = NA_real_,
        prediction_lower = NA_real_,
        prediction_upper = NA_real_,
        forest_plot = NA_character_
      )
    )
  }

  data <- readr::read_csv(
    dataset_file,
    show_col_types = FALSE
  )

  if (nrow(data) < 2L) {

    return(
      tibble::tibble(
        analysis_group = group_name,
        meta_analysis_role = role,
        status = "SKIP",
        reason = "Fewer than 2 studies/outcome rows; pooling not performed.",
        studies = nrow(data),
        effect_measure = manifest_measure,
        pooled_effect = NA_real_,
        lower_ci = NA_real_,
        upper_ci = NA_real_,
        p_value = NA_real_,
        tau2 = NA_real_,
        i2 = NA_real_,
        q = NA_real_,
        q_p_value = NA_real_,
        prediction_lower = NA_real_,
        prediction_upper = NA_real_,
        forest_plot = NA_character_
      )
    )
  }

  # ----------------------------------------------------------
  # Resolve the effect measure robustly.
  #
  # Priority:
  #   1. valid row-level human/final/agent effect measure
  #   2. valid manifest effect measure
  #
  # Invalid row values (for example "64") are treated as data
  # contamination/misalignment and are ignored as effect-measure
  # labels. Valid but genuinely conflicting measures still FAIL.
  # ----------------------------------------------------------

  row_measure_raw <- value_from_candidates(
    data,
    c(
      "human_verified_effect_measure",
      "final_effect_measure",
      "effect_measure"
    )
  )

  row_measures <- vapply(
    row_measure_raw,
    normalize_effect_measure,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  valid_row_measures <- unique(
    row_measures[
      !is.na(row_measures)
    ]
  )

  manifest_measure_valid <- normalize_effect_measure(
    manifest_measure
  )

  if (length(valid_row_measures) > 1L) {

    return(
      tibble::tibble(
        analysis_group = group_name,
        meta_analysis_role = role,
        status = "FAIL",
        reason = paste0(
          "Genuinely mixed supported effect measures: ",
          paste(
            valid_row_measures,
            collapse = ", "
          )
        ),
        studies = nrow(data),
        effect_measure = "Mixed",
        pooled_effect = NA_real_,
        lower_ci = NA_real_,
        upper_ci = NA_real_,
        p_value = NA_real_,
        tau2 = NA_real_,
        i2 = NA_real_,
        q = NA_real_,
        q_p_value = NA_real_,
        prediction_lower = NA_real_,
        prediction_upper = NA_real_,
        forest_plot = NA_character_
      )
    )
  }

  if (length(valid_row_measures) == 1L) {

    measure <- valid_row_measures[[1]]

    if (
      !is.na(manifest_measure_valid) &&
        manifest_measure_valid != measure
    ) {
      return(
        tibble::tibble(
          analysis_group = group_name,
          meta_analysis_role = role,
          status = "FAIL",
          reason = paste0(
            "Manifest effect measure (",
            manifest_measure_valid,
            ") conflicts with valid row-level measure (",
            measure,
            ")."
          ),
          studies = nrow(data),
          effect_measure = "Conflict",
          pooled_effect = NA_real_,
          lower_ci = NA_real_,
          upper_ci = NA_real_,
          p_value = NA_real_,
          tau2 = NA_real_,
          i2 = NA_real_,
          q = NA_real_,
          q_p_value = NA_real_,
          prediction_lower = NA_real_,
          prediction_upper = NA_real_,
          forest_plot = NA_character_
        )
      )
    }

  } else {

    measure <- manifest_measure_valid
  }

  if (
    is.na(measure) ||
      !measure %in% supported_effect_measures
  ) {

    invalid_measure_values <- unique(
      clean_text(
        row_measure_raw
      )
    )

    invalid_measure_values <- invalid_measure_values[
      !is.na(
        invalid_measure_values
      )
    ]

    return(
      tibble::tibble(
        analysis_group = group_name,
        meta_analysis_role = role,
        status = "FAIL",
        reason = paste0(
          "No supported effect measure could be resolved. ",
          if (length(invalid_measure_values) > 0L) {
            paste0(
              "Raw row value(s): ",
              paste(
                invalid_measure_values,
                collapse = ", "
              ),
              ". "
            )
          } else {
            ""
          },
          "Check R/26 finalization / Target_Outcome_Verification mapping."
        ),
        studies = nrow(data),
        effect_measure = NA_character_,
        pooled_effect = NA_real_,
        lower_ci = NA_real_,
        upper_ci = NA_real_,
        p_value = NA_real_,
        tau2 = NA_real_,
        i2 = NA_real_,
        q = NA_real_,
        q_p_value = NA_real_,
        prediction_lower = NA_real_,
        prediction_upper = NA_real_,
        forest_plot = NA_character_
      )
    )
  }


  effects <- tryCatch(
    prepare_effects(
      data,
      measure
    ),
    error = function(e) {
      e
    }
  )

  if (inherits(effects, "error")) {

    return(
      tibble::tibble(
        analysis_group = group_name,
        meta_analysis_role = role,
        status = "FAIL",
        reason = conditionMessage(
          effects
        ),
        studies = nrow(data),
        effect_measure = measure,
        pooled_effect = NA_real_,
        lower_ci = NA_real_,
        upper_ci = NA_real_,
        p_value = NA_real_,
        tau2 = NA_real_,
        i2 = NA_real_,
        q = NA_real_,
        q_p_value = NA_real_,
        prediction_lower = NA_real_,
        prediction_upper = NA_real_,
        forest_plot = NA_character_
      )
    )
  }

  if (
    any(
      !is.finite(
        effects$yi
      ) |
        !is.finite(
          effects$vi
        ) |
        effects$vi <= 0
    )
  ) {

    return(
      tibble::tibble(
        analysis_group = group_name,
        meta_analysis_role = role,
        status = "FAIL",
        reason = "Effect-size preparation produced invalid yi/vi values.",
        studies = nrow(data),
        effect_measure = measure,
        pooled_effect = NA_real_,
        lower_ci = NA_real_,
        upper_ci = NA_real_,
        p_value = NA_real_,
        tau2 = NA_real_,
        i2 = NA_real_,
        q = NA_real_,
        q_p_value = NA_real_,
        prediction_lower = NA_real_,
        prediction_upper = NA_real_,
        forest_plot = NA_character_
      )
    )
  }

  labels <- make_study_labels(
    data
  )

  analysis_data <- dplyr::bind_cols(
    data,
    effects
  ) |>
    dplyr::mutate(
      study_label_for_meta = labels
    )

  model <- tryCatch(
    metafor::rma.uni(
      yi = analysis_data$yi,
      vi = analysis_data$vi,
      method = "REML",
      test = "knha",
      slab = analysis_data$study_label_for_meta
    ),
    error = function(e) {
      e
    }
  )

  if (inherits(model, "error")) {

    return(
      tibble::tibble(
        analysis_group = group_name,
        meta_analysis_role = role,
        status = "FAIL",
        reason = paste0(
          "Model fitting failed: ",
          conditionMessage(
            model
          )
        ),
        studies = nrow(data),
        effect_measure = measure,
        pooled_effect = NA_real_,
        lower_ci = NA_real_,
        upper_ci = NA_real_,
        p_value = NA_real_,
        tau2 = NA_real_,
        i2 = NA_real_,
        q = NA_real_,
        q_p_value = NA_real_,
        prediction_lower = NA_real_,
        prediction_upper = NA_real_,
        forest_plot = NA_character_
      )
    )
  }

  pred <- tryCatch(
    predict(
      model
    ),
    error = function(e) {
      NULL
    }
  )

  pooled_transformed <- as.numeric(
    model$b[
      1
    ]
  )

  ci_lb_transformed <- as.numeric(
    model$ci.lb[
      1
    ]
  )

  ci_ub_transformed <- as.numeric(
    model$ci.ub[
      1
    ]
  )

  pooled_effect <- inverse_transform(
    pooled_transformed,
    measure
  )

  lower_ci <- inverse_transform(
    ci_lb_transformed,
    measure
  )

  upper_ci <- inverse_transform(
    ci_ub_transformed,
    measure
  )

  prediction_lower <- NA_real_
  prediction_upper <- NA_real_

  if (
    !is.null(pred) &&
      !is.null(pred$pi.lb) &&
      !is.null(pred$pi.ub)
  ) {

    prediction_lower <- inverse_transform(
      as.numeric(
        pred$pi.lb[
          1
        ]
      ),
      measure
    )

    prediction_upper <- inverse_transform(
      as.numeric(
        pred$pi.ub[
          1
        ]
      ),
      measure
    )
  }

  analysis_data <- analysis_data |>
    dplyr::mutate(
      study_effect = inverse_transform(
        yi,
        measure
      ),
      study_lower_ci = inverse_transform(
        yi -
          1.96 * sei,
        measure
      ),
      study_upper_ci = inverse_transform(
        yi +
          1.96 * sei,
        measure
      ),
      random_effect_weight_percent = as.numeric(
        stats::weights(
          model
        )
      )
    )

  readr::write_csv(
    analysis_data,
    study_effect_file,
    na = ""
  )

  # Extract model statistics BEFORE constructing the tibble.
  #
  # This avoids a subtle dplyr/tibble data-mask bug: if a column named
  # `model` is created first inside tibble(), later expressions such as
  # model$pval can resolve to that character column instead of the
  # fitted metafor object.

  model_k <- as.integer(
    model$k
  )

  model_p_value <- as.numeric(
    model$pval[
      1
    ]
  )

  model_tau2 <- as.numeric(
    model$tau2
  )

  model_i2 <- as.numeric(
    model$I2
  )

  model_q <- as.numeric(
    model$QE
  )

  model_q_df <- as.numeric(
    model_k -
      1L
  )

  model_q_p_value <- as.numeric(
    model$QEp
  )

  summary_row <- tibble::tibble(
    analysis_group = group_name,
    meta_analysis_role = role,
    status = "PASS",
    reason = NA_character_,
    studies = model_k,
    effect_measure = measure,
    model_name = "Random-effects",
    tau2_method = "REML",
    inference_test = "Knapp-Hartung",
    pooled_effect = pooled_effect,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    p_value = model_p_value,
    tau2 = model_tau2,
    i2 = model_i2,
    q = model_q,
    q_df = model_q_df,
    q_p_value = model_q_p_value,
    prediction_lower = prediction_lower,
    prediction_upper = prediction_upper,
    dataset_file = dataset_file,
    forest_plot = forest_file
  )

  readr::write_csv(
    summary_row,
    summary_file,
    na = ""
  )

  capture.output(
    {
      cat(
        "Universal SR-MA Agent Toolkit\n"
      )

      cat(
        "R/27 Meta-analysis Model Output\n\n"
      )

      cat(
        "Project: ",
        project_title,
        "\n",
        sep = ""
      )

      cat(
        "Analysis group: ",
        group_name,
        "\n",
        sep = ""
      )

      cat(
        "Role: ",
        role,
        "\n",
        sep = ""
      )

      cat(
        "Effect measure: ",
        measure,
        "\n"
      )

      cat(
        "Model: Random-effects, REML, Knapp-Hartung inference\n\n"
      )

      print(
        model
      )

      if (!is.null(pred)) {
        cat(
          "\nPrediction:\n"
        )

        print(
          pred
        )
      }
    },
    file = model_text_file
  )

  grDevices::pdf(
    forest_file,
    width = 8.5,
    height = max(
      6,
      2.5 +
        0.45 * nrow(
          analysis_data
        )
    )
  )

  if (is_ratio_measure(measure)) {

    metafor::forest(
      model,
      atransf = exp,
      xlab = measure,
      slab = analysis_data$study_label_for_meta,
      header = c(
        "Study",
        measure
      ),
      addpred = TRUE
    )

  } else {

    metafor::forest(
      model,
      xlab = measure,
      slab = analysis_data$study_label_for_meta,
      header = c(
        "Study",
        measure
      ),
      addpred = TRUE
    )
  }

  graphics::title(
    main = paste0(
      group_name,
      "\n",
      role
    )
  )

  grDevices::dev.off()

  summary_row
}


# ============================================================
# 8. Execute all groups
# ============================================================

group_results <- lapply(
  seq_len(
    nrow(
      manifest
    )
  ),
  function(i) {

    cat(
      "\n------------------------------------------------------------\n"
    )

    cat(
      "R/27 analysis group: ",
      manifest$analysis_group[[i]],
      "\n",
      sep = ""
    )

    cat(
      "------------------------------------------------------------\n"
    )

    result <- run_one_group(
      manifest[
        i,
        ,
        drop = FALSE
      ]
    )

    cat(
      "Status: ",
      result$status[[1]],
      "\n",
      sep = ""
    )

    if (
      !is.na(
        result$reason[[1]]
      )
    ) {
      cat(
        "Reason: ",
        result$reason[[1]],
        "\n",
        sep = ""
      )
    }

    result
  }
)

execution_summary <- dplyr::bind_rows(
  group_results
)


# ============================================================
# 9. Cross-group interpretation flags
# ============================================================

execution_summary <- execution_summary |>
  dplyr::mutate(
    interpretation_rule = dplyr::case_when(
      stringr::str_detect(
        stringr::str_to_lower(
          meta_analysis_role
        ),
        "causal|primary.*rct|random"
      ) ~ "Primary/randomized evidence: interpret as the main efficacy signal according to the prespecified role.",

      stringr::str_detect(
        stringr::str_to_lower(
          meta_analysis_role
        ),
        "observ|real.world|real world"
      ) ~ "Observational evidence: interpret separately as complementary real-world consistency/support.",

      stringr::str_detect(
        stringr::str_to_lower(
          meta_analysis_role
        ),
        "sensitiv|robust"
      ) ~ "Sensitivity evidence: use to assess robustness of the corresponding primary analysis.",

      TRUE ~ "Interpret according to the prespecified meta_analysis_role; do not merge across groups without explicit justification."
    )
  )


# ============================================================
# 10. Save execution summary
# ============================================================

readr::write_csv(
  execution_summary,
  execution_summary_csv,
  na = ""
)

wb <- openxlsx::createWorkbook(
  creator = "Universal SR-MA Agent Toolkit"
)

openxlsx::addWorksheet(
  wb,
  "Execution_Summary"
)

openxlsx::writeData(
  wb,
  "Execution_Summary",
  execution_summary,
  withFilter = TRUE
)

openxlsx::freezePane(
  wb,
  "Execution_Summary",
  firstActiveRow = 2
)

openxlsx::setColWidths(
  wb,
  "Execution_Summary",
  cols = seq_len(
    ncol(
      execution_summary
    )
  ),
  widths = "auto"
)

openxlsx::addWorksheet(
  wb,
  "Manifest"
)

openxlsx::writeData(
  wb,
  "Manifest",
  manifest,
  withFilter = TRUE
)

openxlsx::freezePane(
  wb,
  "Manifest",
  firstActiveRow = 2
)

openxlsx::setColWidths(
  wb,
  "Manifest",
  cols = seq_len(
    ncol(
      manifest
    )
  ),
  widths = "auto"
)

openxlsx::saveWorkbook(
  wb,
  execution_summary_xlsx,
  overwrite = TRUE
)


# ============================================================
# 11. Overall status
# ============================================================

pass_count <- sum(
  execution_summary$status == "PASS",
  na.rm = TRUE
)

skip_count <- sum(
  execution_summary$status == "SKIP",
  na.rm = TRUE
)

fail_count <- sum(
  execution_summary$status == "FAIL",
  na.rm = TRUE
)

overall_status <- if (
  fail_count > 0L
) {
  "FAIL"
} else if (
  pass_count == 0L
) {
  "NO_META_ANALYSIS_RUN"
} else {
  "PASS"
}


# ============================================================
# 12. Report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/27 — Universal Meta-analysis Execution Report",
  "",
  paste0(
    "Generated at: ",
    now_text()
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
  "INPUT",
  "--------------------------------------------------",
  paste0(
    "Manifest: ",
    manifest_file
  ),
  paste0(
    "Analysis groups requested: ",
    nrow(
      manifest
    )
  ),
  "",
  "EXECUTION",
  "--------------------------------------------------",
  paste0(
    "PASS groups: ",
    pass_count
  ),
  paste0(
    "SKIP groups: ",
    skip_count
  ),
  paste0(
    "FAIL groups: ",
    fail_count
  ),
  "",
  "UNIVERSAL ANALYSIS RULES",
  "--------------------------------------------------",
  "Each analysis_group is analyzed independently.",
  "Random-effects model is the default.",
  "Tau-squared estimator: REML.",
  "Inference: Knapp-Hartung.",
  "Mixed effect measures are not silently combined.",
  "RCT and observational evidence are not automatically pooled together.",
  "",
  "EXECUTION SUMMARY",
  "--------------------------------------------------",
  paste(
    capture.output(
      print(
        execution_summary
      )
    ),
    collapse = "\n"
  ),
  "",
  "OVERALL STATUS",
  "--------------------------------------------------",
  overall_status,
  "",
  "NEXT",
  "--------------------------------------------------",
  if (
    overall_status == "PASS"
  ) {
    "Review forest plots and pooled estimates, then proceed to synthesis interpretation/reporting."
  } else {
    "Resolve FAIL groups before declaring the meta-analysis stage complete."
  }
)

writeLines(
  report_lines,
  report_file,
  useBytes = TRUE
)


# ============================================================
# 13. Console
# ============================================================

cat("\n")
cat("============================================================\n")
cat("R/27 — Universal Meta-analysis Execution\n")
cat("============================================================\n\n")

cat(
  "Analysis groups: ",
  nrow(
    manifest
  ),
  "\n",
  sep = ""
)

cat(
  "PASS: ",
  pass_count,
  "\n",
  sep = ""
)

cat(
  "SKIP: ",
  skip_count,
  "\n",
  sep = ""
)

cat(
  "FAIL: ",
  fail_count,
  "\n\n",
  sep = ""
)

for (i in seq_len(nrow(execution_summary))) {

  x <- execution_summary[
    i,
    ,
    drop = FALSE
  ]

  cat(
    "[",
    x$status[[1]],
    "] ",
    x$analysis_group[[1]],
    "\n",
    sep = ""
  )

  if (
    x$status[[1]] == "PASS"
  ) {

    cat(
      "    Studies: ",
      x$studies[[1]],
      "\n",
      sep = ""
    )

    cat(
      "    ",
      x$effect_measure[[1]],
      ": ",
      format_num(
        x$pooled_effect[[1]]
      ),
      " [",
      format_num(
        x$lower_ci[[1]]
      ),
      ", ",
      format_num(
        x$upper_ci[[1]]
      ),
      "]\n",
      sep = ""
    )

    cat(
      "    I2: ",
      format_num(
        x$i2[[1]],
        digits = 1
      ),
      "%\n",
      sep = ""
    )

  } else {

    cat(
      "    ",
      safe_chr(
        x$reason[[1]]
      ),
      "\n",
      sep = ""
    )
  }
}

cat("\n")

if (overall_status == "PASS") {

  cat(
    "PASS: R/27 completed all executable meta-analysis groups.\n"
  )

  cat(
    "NEXT: Review results by evidence role before reporting.\n"
  )

} else {

  cat(
    "R/27 overall status: ",
    overall_status,
    "\n",
    sep = ""
  )
}

invisible(
  list(
    status = overall_status,
    manifest = manifest,
    execution_summary = execution_summary,
    report = report_file
  )
)
