# ============================================================
# Universal SR-MA Agent Toolkit
# R/33_generate_forest_plots_and_editable_tables.R
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

build_meta_object_from_dataset <- function(df, analysis_group_label = "Analysis") {

  # ---- common columns -------------------------------------------------------
  study_col <- first_existing_col(
    df,
    c(
      "study_id",
      "screening_id",
      "study_code",
      "study_label",
      "study",
      "study_name",
      "citation_short",
      "first_author_year",
      "author_year"
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
    c("study_design", "design")
  )

  measure_col <- first_existing_col(
    df,
    c("effect_measure", "measure", "summary_measure")
  )

  effect_col <- first_existing_col(
    df,
    c("effect_value", "effect_estimate", "estimate", "rr", "or", "hr")
  )

  lower_col <- first_existing_col(
    df,
    c("lower_ci", "lcl", "ci_lower", "lower", "lower95")
  )

  upper_col <- first_existing_col(
    df,
    c("upper_ci", "ucl", "ci_upper", "upper", "upper95")
  )

  events_e_col <- first_existing_col(
    df,
    c("events_exposed", "event_experimental", "event_treatment", "events_treatment", "s_event")
  )

  n_e_col <- first_existing_col(
    df,
    c("n_exposed", "n_experimental", "n_treatment", "total_treatment", "s_total")
  )

  events_c_col <- first_existing_col(
    df,
    c("events_control", "event_control", "c_event")
  )

  n_c_col <- first_existing_col(
    df,
    c("n_control", "total_control", "c_total")
  )

  treat_label_col <- first_existing_col(
    df,
    c("experimental_label", "treatment_label", "intervention_label")
  )

  control_label_col <- first_existing_col(
    df,
    c("control_label", "comparator_label")
  )

  # ---- study labels ---------------------------------------------------------
  studlab <- trimws(as.character(df[[study_col]]))
  studlab[is.na(studlab) | studlab == ""] <- paste0("Study_", seq_len(nrow(df)))

  study_design <- if (!is.na(design_col)) as.character(df[[design_col]]) else rep(NA_character_, nrow(df))

  # ---- effect measure -------------------------------------------------------
  effect_measure <- rep(NA_character_, nrow(df))
  if (!is.na(measure_col)) {
    effect_measure <- toupper(trimws(as.character(df[[measure_col]])))
  } else if (!is.na(effect_col)) {
    # if there is no explicit measure column but current universal pipeline
    # is mostly RR-oriented, set to RR as a practical default.
    effect_measure <- rep("RR", nrow(df))
  }

  effect_measure[effect_measure == ""] <- NA_character_

  distinct_measures <- unique(stats::na.omit(effect_measure))

  if (length(distinct_measures) == 0L) {
    distinct_measures <- "RR"
    effect_measure <- rep("RR", nrow(df))
  }

  if (length(distinct_measures) > 1L) {
    stop(
      "Mixed effect measures detected in one analysis dataset: ",
      paste(distinct_measures, collapse = ", "),
      call. = FALSE
    )
  }

  summary_measure <- distinct_measures[[1]]

  # ---- choose modelling path ------------------------------------------------
  can_use_binary_counts <- all(
    !is.na(c(events_e_col, n_e_col, events_c_col, n_c_col))
  )

  if (can_use_binary_counts && summary_measure %in% c("RR", "OR")) {

    event.e <- safe_num(df[[events_e_col]])
    n.e <- safe_num(df[[n_e_col]])
    event.c <- safe_num(df[[events_c_col]])
    n.c <- safe_num(df[[n_c_col]])

    keep <- !is.na(event.e) & !is.na(n.e) & !is.na(event.c) & !is.na(n.c)

    if (sum(keep) == 0L) {
      stop(
        "Binary-count columns were found, but no complete rows were available.",
        call. = FALSE
      )
    }

    meta_obj <- meta::metabin(
      event.e = event.e[keep],
      n.e = n.e[keep],
      event.c = event.c[keep],
      n.c = n.c[keep],
      studlab = studlab[keep],
      sm = summary_measure,
      method = "Inverse",
      method.tau = "REML",
      hakn = TRUE,
      random = TRUE,
      common = FALSE,
      prediction = FALSE
    )

    weights_percent <- as.numeric(meta_obj$w.random) / sum(as.numeric(meta_obj$w.random)) * 100

    study_table <- tibble::tibble(
      analysis_group = analysis_group_label,
      study_label = studlab[keep],
      study_design = study_design[keep],
      effect_measure = summary_measure,
      effect_value = exp(meta_obj$TE),
      lower_ci = exp(meta_obj$lower),
      upper_ci = exp(meta_obj$upper),
      log_effect = as.numeric(meta_obj$TE),
      se_log_effect = as.numeric(meta_obj$seTE),
      random_effect_weight_percent = weights_percent,
      events_exposed = event.e[keep],
      n_exposed = n.e[keep],
      events_control = event.c[keep],
      n_control = n.c[keep]
    )

  } else {

    if (is.na(effect_col) || is.na(lower_col) || is.na(upper_col)) {
      stop(
        paste0(
          "No complete effect-size input was found. Required either:\n",
          "- binary count columns for exposed/control groups, or\n",
          "- effect_value + lower_ci + upper_ci columns."
        ),
        call. = FALSE
      )
    }

    effect_value <- safe_num(df[[effect_col]])
    lower_ci <- safe_num(df[[lower_col]])
    upper_ci <- safe_num(df[[upper_col]])

    keep <- !is.na(effect_value) & !is.na(lower_ci) & !is.na(upper_ci) &
      effect_value > 0 & lower_ci > 0 & upper_ci > 0

    if (sum(keep) == 0L) {
      stop(
        "No complete positive effect-size rows were available.",
        call. = FALSE
      )
    }

    TE <- log(effect_value[keep])
    seTE <- (log(upper_ci[keep]) - log(lower_ci[keep])) / 3.92

    meta_obj <- meta::metagen(
      TE = TE,
      seTE = seTE,
      studlab = studlab[keep],
      sm = summary_measure,
      method.tau = "REML",
      hakn = TRUE,
      random = TRUE,
      common = FALSE,
      prediction = FALSE
    )

    weights_percent <- as.numeric(meta_obj$w.random) / sum(as.numeric(meta_obj$w.random)) * 100

    study_table <- tibble::tibble(
      analysis_group = analysis_group_label,
      study_label = studlab[keep],
      study_design = study_design[keep],
      effect_measure = summary_measure,
      effect_value = exp(meta_obj$TE),
      lower_ci = exp(meta_obj$lower),
      upper_ci = exp(meta_obj$upper),
      log_effect = as.numeric(meta_obj$TE),
      se_log_effect = as.numeric(meta_obj$seTE),
      random_effect_weight_percent = weights_percent
    )

    if (!is.na(events_e_col)) study_table$events_exposed <- safe_num(df[[events_e_col]])[keep]
    if (!is.na(n_e_col)) study_table$n_exposed <- safe_num(df[[n_e_col]])[keep]
    if (!is.na(events_c_col)) study_table$events_control <- safe_num(df[[events_c_col]])[keep]
    if (!is.na(n_c_col)) study_table$n_control <- safe_num(df[[n_c_col]])[keep]
  }

  treat_label <- if (!is.na(treat_label_col)) safe_chr(df[[treat_label_col]], "SGLT2i") else "SGLT2i"
  control_label <- if (!is.na(control_label_col)) safe_chr(df[[control_label_col]], "Control") else "Control"

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
    model = "Random-effects REML + Knapp-Hartung",
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

  png_file <- paste0(output_base, ".png")
  pdf_file <- paste0(output_base, ".pdf")

  common_args <- list(
    x = meta_obj,
    sortvar = meta_obj$TE,
    comb.fixed = FALSE,
    comb.random = TRUE,
    leftcols = c("studlab"),
    leftlabs = c("Study"),
    rightcols = c("effect", "ci", "w.random"),
    rightlabs = c(
      sm,
      "95% CI",
      "Weight"
    ),
    smlab = paste0(sm, " (random-effects)"),
    lab.e = meta_bundle$treatment_label,
    lab.c = meta_bundle$control_label,
    text.random = "Pooled effect",
    test.overall = TRUE,
    test.subgroup = FALSE,
    test.overall.random = TRUE,
    print.I2 = TRUE,
    print.tau2 = TRUE,
    print.pval.Q = TRUE,
    digits = 2,
    digits.se = 2,
    digits.pval = 3,
    col.square = "black",
    col.diamond = "black",
    col.diamond.lines = "black",
    col.predict = "black",
    col.study = "black",
    col.label.right = "black",
    col.label.left = "black",
    overall = TRUE,
    overall.hetstat = TRUE
  )

  grDevices::png(
    filename = png_file,
    width = 2400,
    height = 1800,
    res = 300
  )

  do.call(
    meta::forest,
    common_args
  )

  graphics::title(
    main = analysis_group
  )

  grDevices::dev.off()

  grDevices::pdf(
    file = pdf_file,
    width = 11,
    height = 8.5
  )

  do.call(
    meta::forest,
    common_args
  )

  graphics::title(
    main = analysis_group
  )

  grDevices::dev.off()

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


# ============================================================
# 4. Locate manifest / datasets
# ============================================================

candidate_manifest_paths <- c(
  file.path(repo_root, "data", "processed", "meta_analysis", "meta_analysis_manifest.csv"),
  file.path(repo_root, "results", "meta_analysis", "meta_analysis_manifest.csv"),
  file.path(repo_root, "meta_analysis_manifest.csv"),
  file.path(repo_root, "data", "processed", "extraction", "meta_analysis_manifest.csv")
)

manifest_path <- candidate_manifest_paths[
  file.exists(candidate_manifest_paths)
]

manifest <- NULL
dataset_registry <- NULL

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

  if (is.na(path_col)) {
    stop(
      "Manifest was found, but no dataset-path column was detected.",
      call. = FALSE
    )
  }

  dataset_registry <- manifest |>
    dplyr::mutate(
      analysis_group = if (!is.na(group_col)) as.character(.data[[group_col]]) else paste0("Group_", dplyr::row_number()),
      dataset_path = as.character(.data[[path_col]]),
      keep = if (!is.na(status_col)) {
        tolower(trimws(as.character(.data[[status_col]]))) %in% c("pass", "passed", "ready", "active", "include", "included")
      } else {
        TRUE
      }
    ) |>
    dplyr::filter(keep) |>
    dplyr::select(analysis_group, dataset_path)

} else {

  fallback_dirs <- c(
    file.path(repo_root, "data", "processed", "meta_analysis"),
    file.path(repo_root, "data", "processed", "extraction"),
    repo_root
  )

  fallback_files <- unique(unlist(
    lapply(
      fallback_dirs[file.exists(fallback_dirs)],
      function(dir_i) {
        list.files(
          dir_i,
          pattern = "meta_analysis_ready.*\\.csv$",
          full.names = TRUE,
          recursive = TRUE
        )
      }
    )
  ))

  if (length(fallback_files) == 0L) {
    stop(
      paste0(
        "No manifest and no fallback meta-analysis-ready CSV files were found.\n",
        "Expected a manifest or files named like 'meta_analysis_ready_*.csv'."
      ),
      call. = FALSE
    )
  }

  dataset_registry <- tibble::tibble(
    analysis_group = tools::file_path_sans_ext(basename(fallback_files)),
    dataset_path = fallback_files
  )
}


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

      if (nrow(df) == 0L) {
        stop("Dataset has zero rows.", call. = FALSE)
      }

      meta_bundle <- build_meta_object_from_dataset(
        df = df,
        analysis_group_label = analysis_group
      )

      plot_base <- file.path(
        figures_dir,
        paste0("forest_", slugify(analysis_group))
      )

      plot_files <- save_forest_plot(
        meta_bundle = meta_bundle,
        analysis_group = analysis_group,
        output_base = plot_base
      )

      summary_rows[[length(summary_rows) + 1L]] <- meta_bundle$summary_table |>
        dplyr::mutate(
          source_dataset = normalize_slash(dataset_path),
          forest_plot_png = normalize_slash(plot_files$png),
          forest_plot_pdf = normalize_slash(plot_files$pdf)
        )

      study_rows[[length(study_rows) + 1L]] <- meta_bundle$study_table |>
        dplyr::mutate(
          source_dataset = normalize_slash(dataset_path)
        )

      plot_rows[[length(plot_rows) + 1L]] <- tibble::tibble(
        analysis_group = analysis_group,
        dataset_path = normalize_slash(dataset_path),
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
  paste0("PASS groups: ", pass_n),
  paste0("FAIL groups: ", fail_n),
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0("Summary CSV: ", normalize_slash(summary_csv)),
  paste0("Study CSV: ", normalize_slash(study_csv)),
  paste0("Workbook: ", normalize_slash(xlsx_file)),
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
