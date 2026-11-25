# ============================================================
# Universal SR-MA Agent Toolkit
# R/28_run_meta_analysis_diagnostics.R
#
# PURPOSE
# ------------------------------------------------------------
# Generic post-meta-analysis diagnostics after R/27.
#
# This module does NOT attempt to explain clinical heterogeneity.
# It identifies statistical signals that warrant Author review.
#
# INPUTS
# ------------------------------------------------------------
# data/processed/meta_analysis_ready/meta_analysis_manifest.csv
# results/meta_analysis/meta_analysis_execution_summary.csv
# results/meta_analysis/<analysis_group>/study_effects_used.csv
#
# OUTPUTS
# ------------------------------------------------------------
# results/meta_analysis_diagnostics/
#   meta_analysis_diagnostics_summary.csv
#   sensitivity_comparison.csv
#   meta_analysis_diagnostics.xlsx
#   <analysis_group>/
#       leave_one_out.csv
#       baujat_plot.pdf
#
# results/logs/meta_analysis_diagnostics_report.txt
#
# UNIVERSAL PRINCIPLES
# ------------------------------------------------------------
# 1. Diagnostics are descriptive flags, not automatic causal explanations.
# 2. Leave-one-out analysis is performed when >= 3 studies are available.
# 3. Composite sensitivity sets are compared with their parent analysis.
# 4. Meta-regression is NOT automatically performed; it is only suggested
#    for consideration when the number of studies is sufficient.
# 5. Prediction intervals are not emphasized when very few studies are
#    available.
# 6. No Human study-inclusion decision is changed by R/28.
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

now_text <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

normalize_effect_measure <- function(x) {
  y <- toupper(
    safe_chr(x)
  )

  y <- stringr::str_replace_all(
    y,
    "[^A-Z0-9]+",
    ""
  )

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

    TRUE ~ safe_chr(x)
  )
}

is_ratio_measure <- function(measure) {
  normalize_effect_measure(measure) %in% c(
    "RR",
    "OR",
    "HR"
  )
}

inverse_transform <- function(x, measure) {
  if (is_ratio_measure(measure)) {
    return(exp(x))
  }

  x
}

effect_direction <- function(effect, measure) {
  if (
    is.na(effect)
  ) {
    return(NA_character_)
  }

  if (is_ratio_measure(measure)) {

    if (effect < 1) {
      return("Below null")
    }

    if (effect > 1) {
      return("Above null")
    }

    return("At null")
  }

  if (effect < 0) {
    return("Below null")
  }

  if (effect > 0) {
    return("Above null")
  }

  "At null"
}

ci_excludes_null <- function(lower, upper, measure) {
  if (
    is.na(lower) ||
      is.na(upper)
  ) {
    return(NA)
  }

  null_value <- if (
    is_ratio_measure(measure)
  ) {
    1
  } else {
    0
  }

  (
    upper < null_value
  ) ||
    (
      lower > null_value
    )
}

study_label_vector <- function(data) {

  candidates <- c(
    "study_label_for_meta",
    "study_label",
    "author_year",
    "citation",
    "study_name",
    "screening_id"
  )

  label_col <- candidates[
    candidates %in% names(data)
  ]

  if (length(label_col) == 0L) {
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
    data[[label_col[[1]]]]
  )

  labels[
    is.na(labels)
  ] <- paste0(
    "Study ",
    which(
      is.na(labels)
    )
  )

  make.unique(labels)
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

meta_ready_directory <- file.path(
  paths$project_root,
  "data",
  "processed",
  "meta_analysis_ready"
)

manifest_file <- file.path(
  meta_ready_directory,
  "meta_analysis_manifest.csv"
)

meta_analysis_directory <- file.path(
  paths$project_root,
  "results",
  "meta_analysis"
)

execution_summary_file <- file.path(
  meta_analysis_directory,
  "meta_analysis_execution_summary.csv"
)

diagnostics_root <- file.path(
  paths$project_root,
  "results",
  "meta_analysis_diagnostics"
)

dir.create(
  diagnostics_root,
  recursive = TRUE,
  showWarnings = FALSE
)

diagnostics_summary_file <- file.path(
  diagnostics_root,
  "meta_analysis_diagnostics_summary.csv"
)

sensitivity_comparison_file <- file.path(
  diagnostics_root,
  "sensitivity_comparison.csv"
)

diagnostics_workbook <- file.path(
  diagnostics_root,
  "meta_analysis_diagnostics.xlsx"
)

report_file <- file.path(
  paths$logs_directory,
  "meta_analysis_diagnostics_report.txt"
)

if (!file.exists(manifest_file)) {
  stop(
    "R/26 manifest not found: ",
    manifest_file,
    call. = FALSE
  )
}

if (!file.exists(execution_summary_file)) {
  stop(
    "R/27 execution summary not found: ",
    execution_summary_file,
    call. = FALSE
  )
}


# ============================================================
# 4. Read R/26 + R/27 outputs
# ============================================================

manifest <- readr::read_csv(
  manifest_file,
  show_col_types = FALSE
)

execution_summary <- readr::read_csv(
  execution_summary_file,
  show_col_types = FALSE
)

required_execution_columns <- c(
  "analysis_group",
  "status",
  "studies",
  "effect_measure",
  "pooled_effect",
  "lower_ci",
  "upper_ci",
  "tau2",
  "i2"
)

missing_execution_columns <- setdiff(
  required_execution_columns,
  names(execution_summary)
)

if (length(missing_execution_columns) > 0L) {
  stop(
    "R/27 execution summary is missing required column(s): ",
    paste(
      missing_execution_columns,
      collapse = ", "
    ),
    call. = FALSE
  )
}

pass_groups <- execution_summary |>
  dplyr::filter(
    status == "PASS"
  )

if (nrow(pass_groups) == 0L) {
  stop(
    "R/28 found no PASS meta-analysis groups from R/27.",
    call. = FALSE
  )
}


# ============================================================
# 5. Diagnostics for one PASS group
# ============================================================

diagnose_one_group <- function(execution_row) {

  group_name <- safe_chr(
    execution_row$analysis_group
  )

  group_slug <- safe_file_component(
    group_name
  )

  group_meta_dir <- file.path(
    meta_analysis_directory,
    group_slug
  )

  study_effect_file <- file.path(
    group_meta_dir,
    "study_effects_used.csv"
  )

  group_diag_dir <- file.path(
    diagnostics_root,
    group_slug
  )

  dir.create(
    group_diag_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  leave_one_out_file <- file.path(
    group_diag_dir,
    "leave_one_out.csv"
  )

  baujat_file <- file.path(
    group_diag_dir,
    "baujat_plot.pdf"
  )

  if (!file.exists(study_effect_file)) {

    return(
      list(
        summary = tibble::tibble(
          analysis_group = group_name,
          diagnostic_status = "FAIL",
          reason = paste0(
            "study_effects_used.csv not found: ",
            study_effect_file
          ),
          studies = safe_numeric(
            execution_row$studies
          ),
          effect_measure = safe_chr(
            execution_row$effect_measure
          ),
          pooled_effect = safe_numeric(
            execution_row$pooled_effect
          ),
          lower_ci = safe_numeric(
            execution_row$lower_ci
          ),
          upper_ci = safe_numeric(
            execution_row$upper_ci
          ),
          tau2 = safe_numeric(
            execution_row$tau2
          ),
          i2 = safe_numeric(
            execution_row$i2
          ),
          heterogeneity_review_flag = NA_character_,
          leave_one_out_performed = "No",
          most_influential_study_by_effect_shift = NA_character_,
          max_relative_effect_shift_percent = NA_real_,
          max_i2_reduction_percentage_points = NA_real_,
          prediction_interval_emphasis = NA_character_,
          meta_regression_consideration = NA_character_,
          recommended_human_action = "Resolve missing R/27 study-effects output."
        ),
        loo = NULL
      )
    )
  }

  data <- readr::read_csv(
    study_effect_file,
    show_col_types = FALSE
  )

  if (
    !all(
      c(
        "yi",
        "vi"
      ) %in% names(data)
    )
  ) {
    stop(
      "study_effects_used.csv for ",
      group_name,
      " is missing yi and/or vi.",
      call. = FALSE
    )
  }

  yi <- safe_numeric(
    data$yi
  )

  vi <- safe_numeric(
    data$vi
  )

  valid <- is.finite(yi) &
    is.finite(vi) &
    vi > 0

  if (!all(valid)) {
    stop(
      "Invalid yi/vi values detected in ",
      group_name,
      ".",
      call. = FALSE
    )
  }

  labels <- study_label_vector(
    data
  )

  measure <- normalize_effect_measure(
    execution_row$effect_measure
  )

  full_model <- metafor::rma.uni(
    yi = yi,
    vi = vi,
    method = "REML",
    test = "knha",
    slab = labels
  )

  k <- as.integer(
    full_model$k
  )

  pooled_effect <- inverse_transform(
    as.numeric(
      full_model$b[1]
    ),
    measure
  )

  lower_ci <- inverse_transform(
    as.numeric(
      full_model$ci.lb[1]
    ),
    measure
  )

  upper_ci <- inverse_transform(
    as.numeric(
      full_model$ci.ub[1]
    ),
    measure
  )

  full_i2 <- as.numeric(
    full_model$I2
  )

  full_tau2 <- as.numeric(
    full_model$tau2
  )

  heterogeneity_review_flag <- dplyr::case_when(
    is.na(full_i2) ~ "Unavailable",
    full_i2 >= 50 ~ "Review recommended",
    TRUE ~ "No automatic high-heterogeneity flag"
  )

  prediction_interval_emphasis <- if (
    k >= 5L
  ) {
    "May be emphasized with appropriate assumptions/caution"
  } else {
    "Do not emphasize: very few studies"
  }

  meta_regression_consideration <- if (
    k >= 10L
  ) {
    "May be considered if scientifically prespecified and covariates are adequate"
  } else {
    "Not recommended automatically: fewer than 10 studies"
  }

  loo <- NULL
  most_influential <- NA_character_
  max_relative_shift <- NA_real_
  max_i2_reduction <- NA_real_

  if (k >= 3L) {

    loo_rows <- lapply(
      seq_len(k),
      function(i) {

        keep <- seq_len(k) != i

        loo_model <- tryCatch(
          metafor::rma.uni(
            yi = yi[keep],
            vi = vi[keep],
            method = "REML",
            test = "knha",
            slab = labels[keep]
          ),
          error = function(e) {
            e
          }
        )

        if (inherits(loo_model, "error")) {

          return(
            tibble::tibble(
              omitted_study = labels[[i]],
              status = "FAIL",
              reason = conditionMessage(
                loo_model
              ),
              studies_remaining = sum(keep),
              pooled_effect = NA_real_,
              lower_ci = NA_real_,
              upper_ci = NA_real_,
              tau2 = NA_real_,
              i2 = NA_real_,
              relative_effect_shift_percent = NA_real_,
              i2_change_percentage_points = NA_real_
            )
          )
        }

        loo_effect <- inverse_transform(
          as.numeric(
            loo_model$b[1]
          ),
          measure
        )

        loo_lower <- inverse_transform(
          as.numeric(
            loo_model$ci.lb[1]
          ),
          measure
        )

        loo_upper <- inverse_transform(
          as.numeric(
            loo_model$ci.ub[1]
          ),
          measure
        )

        relative_shift <- if (
          is.finite(
            pooled_effect
          ) &&
            pooled_effect != 0
        ) {
          100 * (
            loo_effect -
              pooled_effect
          ) /
            abs(
              pooled_effect
            )
        } else {
          NA_real_
        }

        tibble::tibble(
          omitted_study = labels[[i]],
          status = "PASS",
          reason = NA_character_,
          studies_remaining = sum(keep),
          pooled_effect = loo_effect,
          lower_ci = loo_lower,
          upper_ci = loo_upper,
          tau2 = as.numeric(
            loo_model$tau2
          ),
          i2 = as.numeric(
            loo_model$I2
          ),
          relative_effect_shift_percent = relative_shift,
          i2_change_percentage_points = as.numeric(
            loo_model$I2
          ) -
            full_i2
        )
      }
    )

    loo <- dplyr::bind_rows(
      loo_rows
    )

    readr::write_csv(
      loo,
      leave_one_out_file,
      na = ""
    )

    valid_loo <- loo |>
      dplyr::filter(
        status == "PASS",
        is.finite(
          relative_effect_shift_percent
        )
      )

    if (nrow(valid_loo) > 0L) {

      influence_row <- valid_loo |>
        dplyr::slice_max(
          order_by = abs(
            relative_effect_shift_percent
          ),
          n = 1,
          with_ties = FALSE
        )

      most_influential <- safe_chr(
        influence_row$omitted_study
      )

      max_relative_shift <- abs(
        safe_numeric(
          influence_row$relative_effect_shift_percent
        )
      )

      max_i2_reduction <- max(
        0,
        full_i2 -
          min(
            valid_loo$i2,
            na.rm = TRUE
          )
      )
    }

    # Baujat plot is a diagnostic visualization only.
    # Failure to create it does not invalidate R/28.
    try(
      {
        grDevices::pdf(
          baujat_file,
          width = 7.5,
          height = 6.5
        )

        metafor::baujat(
          full_model,
          main = paste0(
            "Baujat Plot — ",
            group_name
          )
        )

        grDevices::dev.off()
      },
      silent = TRUE
    )
  }

  human_action <- dplyr::case_when(
    k < 3L &&
      full_i2 >= 50 ~ paste0(
        "Evidence is sparse and heterogeneous. ",
        "Inspect study-level clinical/methodological differences; ",
        "do not attempt automatic moderator analysis."
      ),

    k >= 3L &&
      full_i2 >= 50 ~ paste0(
        "Inspect leave-one-out results and study-level clinical/methodological ",
        "differences. Any subgroup explanation should be prespecified or clearly ",
        "identified as exploratory."
      ),

    k >= 3L ~ paste0(
        "Review leave-one-out stability. No automatic clinical explanation is generated."
      ),

    TRUE ~ paste0(
        "No additional statistical diagnostic is required automatically; interpret clinically."
      )
  )

  summary <- tibble::tibble(
    analysis_group = group_name,
    diagnostic_status = "PASS",
    reason = NA_character_,
    studies = k,
    effect_measure = measure,
    pooled_effect = pooled_effect,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    tau2 = full_tau2,
    i2 = full_i2,
    heterogeneity_review_flag = heterogeneity_review_flag,
    leave_one_out_performed = ifelse(
      k >= 3L,
      "Yes",
      "No"
    ),
    most_influential_study_by_effect_shift = most_influential,
    max_relative_effect_shift_percent = max_relative_shift,
    max_i2_reduction_percentage_points = max_i2_reduction,
    prediction_interval_emphasis = prediction_interval_emphasis,
    meta_regression_consideration = meta_regression_consideration,
    recommended_human_action = human_action
  )

  list(
    summary = summary,
    loo = loo
  )
}


# ============================================================
# 6. Run diagnostics for all PASS groups
# ============================================================

diagnostic_objects <- lapply(
  seq_len(
    nrow(
      pass_groups
    )
  ),
  function(i) {

    cat(
      "\n------------------------------------------------------------\n"
    )

    cat(
      "R/28 diagnostic group: ",
      pass_groups$analysis_group[[i]],
      "\n",
      sep = ""
    )

    cat(
      "------------------------------------------------------------\n"
    )

    diagnose_one_group(
      pass_groups[
        i,
        ,
        drop = FALSE
      ]
    )
  }
)

diagnostics_summary <- dplyr::bind_rows(
  lapply(
    diagnostic_objects,
    function(x) {
      x$summary
    }
  )
)

readr::write_csv(
  diagnostics_summary,
  diagnostics_summary_file,
  na = ""
)


# ============================================================
# 7. Primary vs composite sensitivity comparisons
# ============================================================
# R/26 manifest may contain:
#   is_composite == "Yes"
#   parent_analysis_group
#
# R/28 compares results descriptively. It does not declare that a
# sensitivity analysis "passes" or "fails" based on an arbitrary
# numerical threshold.
# ============================================================

sensitivity_comparison <- tibble::tibble()

if (
  all(
    c(
      "is_composite",
      "parent_analysis_group"
    ) %in% names(manifest)
  )
) {

  sensitivity_sets <- manifest |>
    dplyr::filter(
      tolower(
        clean_text(
          is_composite
        )
      ) == "yes",
      !is.na(
        clean_text(
          parent_analysis_group
        )
      )
    )

  if (nrow(sensitivity_sets) > 0L) {

    comparison_rows <- lapply(
      seq_len(
        nrow(
          sensitivity_sets
        )
      ),
      function(i) {

        sensitivity_group <- safe_chr(
          sensitivity_sets$analysis_group[[i]]
        )

        parent_group <- safe_chr(
          sensitivity_sets$parent_analysis_group[[i]]
        )

        sensitivity_result <- execution_summary |>
          dplyr::filter(
            analysis_group == sensitivity_group,
            status == "PASS"
          )

        parent_result <- execution_summary |>
          dplyr::filter(
            analysis_group == parent_group,
            status == "PASS"
          )

        if (
          nrow(
            sensitivity_result
          ) != 1L ||
            nrow(
              parent_result
            ) != 1L
        ) {
          return(
            tibble::tibble(
              parent_analysis_group = parent_group,
              sensitivity_analysis_group = sensitivity_group,
              status = "NOT_COMPARABLE",
              reason = "Parent or sensitivity analysis did not have exactly one PASS result."
            )
          )
        }

        parent_effect <- safe_numeric(
          parent_result$pooled_effect[[1]]
        )

        sensitivity_effect <- safe_numeric(
          sensitivity_result$pooled_effect[[1]]
        )

        measure_parent <- normalize_effect_measure(
          parent_result$effect_measure[[1]]
        )

        measure_sensitivity <- normalize_effect_measure(
          sensitivity_result$effect_measure[[1]]
        )

        if (
          measure_parent != measure_sensitivity
        ) {
          return(
            tibble::tibble(
              parent_analysis_group = parent_group,
              sensitivity_analysis_group = sensitivity_group,
              status = "NOT_COMPARABLE",
              reason = paste0(
                "Effect measures differ: ",
                measure_parent,
                " vs ",
                measure_sensitivity
              )
            )
          )
        }

        relative_shift_percent <- if (
          is.finite(
            parent_effect
          ) &&
            parent_effect != 0
        ) {
          100 * (
            sensitivity_effect -
              parent_effect
          ) /
            abs(
              parent_effect
            )
        } else {
          NA_real_
        }

        parent_direction <- effect_direction(
          parent_effect,
          measure_parent
        )

        sensitivity_direction <- effect_direction(
          sensitivity_effect,
          measure_sensitivity
        )

        tibble::tibble(
          parent_analysis_group = parent_group,
          sensitivity_analysis_group = sensitivity_group,
          status = "PASS",
          reason = NA_character_,
          effect_measure = measure_parent,
          parent_studies = safe_numeric(
            parent_result$studies[[1]]
          ),
          sensitivity_studies = safe_numeric(
            sensitivity_result$studies[[1]]
          ),
          parent_pooled_effect = parent_effect,
          parent_lower_ci = safe_numeric(
            parent_result$lower_ci[[1]]
          ),
          parent_upper_ci = safe_numeric(
            parent_result$upper_ci[[1]]
          ),
          sensitivity_pooled_effect = sensitivity_effect,
          sensitivity_lower_ci = safe_numeric(
            sensitivity_result$lower_ci[[1]]
          ),
          sensitivity_upper_ci = safe_numeric(
            sensitivity_result$upper_ci[[1]]
          ),
          relative_pooled_effect_shift_percent = relative_shift_percent,
          parent_direction = parent_direction,
          sensitivity_direction = sensitivity_direction,
          direction_consistent = ifelse(
            parent_direction ==
              sensitivity_direction,
            "Yes",
            "No"
          ),
          parent_ci_excludes_null = ci_excludes_null(
            safe_numeric(
              parent_result$lower_ci[[1]]
            ),
            safe_numeric(
              parent_result$upper_ci[[1]]
            ),
            measure_parent
          ),
          sensitivity_ci_excludes_null = ci_excludes_null(
            safe_numeric(
              sensitivity_result$lower_ci[[1]]
            ),
            safe_numeric(
              sensitivity_result$upper_ci[[1]]
            ),
            measure_sensitivity
          ),
          parent_i2 = safe_numeric(
            parent_result$i2[[1]]
          ),
          sensitivity_i2 = safe_numeric(
            sensitivity_result$i2[[1]]
          ),
          i2_change_percentage_points = safe_numeric(
            sensitivity_result$i2[[1]]
          ) -
            safe_numeric(
              parent_result$i2[[1]]
            ),
          interpretation = paste0(
            "Descriptive robustness comparison only. ",
            "Scientific interpretation remains an Author decision."
          )
        )
      }
    )

    sensitivity_comparison <- dplyr::bind_rows(
      comparison_rows
    )
  }
}

readr::write_csv(
  sensitivity_comparison,
  sensitivity_comparison_file,
  na = ""
)


# ============================================================
# 8. Workbook
# ============================================================

wb <- openxlsx::createWorkbook(
  creator = "Universal SR-MA Agent Toolkit"
)

add_sheet <- function(
    sheet_name,
    data
) {

  openxlsx::addWorksheet(
    wb,
    sheet_name
  )

  if (nrow(data) == 0L) {
    openxlsx::writeData(
      wb,
      sheet_name,
      data.frame(
        Message = "No rows generated for this diagnostic.",
        stringsAsFactors = FALSE
      )
    )

    return(
      invisible(
        TRUE
      )
    )
  }

  openxlsx::writeData(
    wb,
    sheet_name,
    data,
    withFilter = TRUE
  )

  openxlsx::freezePane(
    wb,
    sheet_name,
    firstActiveRow = 2
  )

  openxlsx::setColWidths(
    wb,
    sheet_name,
    cols = seq_len(
      ncol(
        data
      )
    ),
    widths = "auto"
  )

  invisible(
    TRUE
  )
}

add_sheet(
  "Diagnostics_Summary",
  diagnostics_summary
)

add_sheet(
  "Sensitivity_Comparison",
  sensitivity_comparison
)

add_sheet(
  "R27_Execution_Summary",
  execution_summary
)

add_sheet(
  "R26_Manifest",
  manifest
)

openxlsx::saveWorkbook(
  wb,
  diagnostics_workbook,
  overwrite = TRUE
)


# ============================================================
# 9. Overall status
# ============================================================

diagnostic_fail_count <- sum(
  diagnostics_summary$diagnostic_status == "FAIL",
  na.rm = TRUE
)

overall_status <- if (
  diagnostic_fail_count > 0L
) {
  "FAIL"
} else {
  "PASS"
}


# ============================================================
# 10. Report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/28 — Meta-analysis Diagnostics Report",
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
  "UNIVERSAL DIAGNOSTIC POLICY",
  "--------------------------------------------------",
  "Diagnostics generate statistical flags, not clinical explanations.",
  "Leave-one-out analysis: performed when >=3 studies.",
  "Composite sensitivity sets: descriptively compared with parent analysis.",
  "Meta-regression: not automatically performed; only suggested for consideration when >=10 studies.",
  "Prediction intervals: not emphasized when fewer than 5 studies.",
  "Human/Author scientific interpretation remains mandatory.",
  "",
  "DIAGNOSTIC SUMMARY",
  "--------------------------------------------------",
  paste(
    capture.output(
      print(
        diagnostics_summary
      )
    ),
    collapse = "\n"
  ),
  "",
  "SENSITIVITY COMPARISON",
  "--------------------------------------------------",
  if (
    nrow(
      sensitivity_comparison
    ) > 0L
  ) {
    paste(
      capture.output(
        print(
          sensitivity_comparison
        )
      ),
      collapse = "\n"
    )
  } else {
    "No composite sensitivity comparison generated."
  },
  "",
  "STATUS",
  "--------------------------------------------------",
  overall_status,
  "",
  "NEXT",
  "--------------------------------------------------",
  "Review flagged groups scientifically. Do not infer the cause of heterogeneity from statistical diagnostics alone."
)

writeLines(
  report_lines,
  report_file,
  useBytes = TRUE
)


# ============================================================
# 11. Console
# ============================================================

cat("\n")
cat("============================================================\n")
cat("R/28 — Universal Meta-analysis Diagnostics\n")
cat("============================================================\n\n")

cat(
  "PASS meta-analysis groups reviewed: ",
  nrow(
    diagnostics_summary
  ),
  "\n",
  sep = ""
)

cat(
  "Composite sensitivity comparisons: ",
  nrow(
    sensitivity_comparison
  ),
  "\n\n",
  sep = ""
)

for (i in seq_len(nrow(diagnostics_summary))) {

  x <- diagnostics_summary[
    i,
    ,
    drop = FALSE
  ]

  cat(
    "[",
    x$diagnostic_status[[1]],
    "] ",
    x$analysis_group[[1]],
    "\n",
    sep = ""
  )

  cat(
    "    Studies: ",
    x$studies[[1]],
    "\n",
    sep = ""
  )

  cat(
    "    I2: ",
    round(
      x$i2[[1]],
      1
    ),
    "%\n",
    sep = ""
  )

  cat(
    "    Heterogeneity flag: ",
    x$heterogeneity_review_flag[[1]],
    "\n",
    sep = ""
  )

  cat(
    "    Leave-one-out: ",
    x$leave_one_out_performed[[1]],
    "\n",
    sep = ""
  )

  if (
    !is.na(
      x$most_influential_study_by_effect_shift[[1]]
    )
  ) {
    cat(
      "    Largest leave-one-out effect shift: ",
      x$most_influential_study_by_effect_shift[[1]],
      "\n",
      sep = ""
    )
  }

  cat(
    "    Human action: ",
    x$recommended_human_action[[1]],
    "\n\n",
    sep = ""
  )
}

if (
  nrow(
    sensitivity_comparison
  ) > 0L
) {

  cat(
    "Sensitivity robustness comparisons:\n"
  )

  for (
    i in seq_len(
      nrow(
        sensitivity_comparison
      )
    )
  ) {

    x <- sensitivity_comparison[
      i,
      ,
      drop = FALSE
    ]

    cat(
      "  ",
      x$parent_analysis_group[[1]],
      " -> ",
      x$sensitivity_analysis_group[[1]],
      "\n",
      sep = ""
    )

    if (
      "relative_pooled_effect_shift_percent" %in% names(x) &&
        !is.na(
          x$relative_pooled_effect_shift_percent[[1]]
        )
    ) {

      cat(
        "    Relative pooled-effect shift: ",
        round(
          x$relative_pooled_effect_shift_percent[[1]],
          1
        ),
        "%\n",
        sep = ""
      )

      cat(
        "    Direction consistent: ",
        x$direction_consistent[[1]],
        "\n",
        sep = ""
      )

      cat(
        "    I2 change: ",
        round(
          x$i2_change_percentage_points[[1]],
          1
        ),
        " percentage points\n",
        sep = ""
      )
    }
  }

  cat("\n")
}

cat(
  "R/28 overall status: ",
  overall_status,
  "\n",
  sep = ""
)

cat(
  "NEXT: Author scientific review of flagged heterogeneity and robustness signals.\n"
)

invisible(
  list(
    status = overall_status,
    diagnostics_summary = diagnostics_summary,
    sensitivity_comparison = sensitivity_comparison,
    report = report_file,
    workbook = diagnostics_workbook
  )
)
