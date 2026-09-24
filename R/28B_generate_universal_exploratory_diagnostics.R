# ============================================================
# Universal SR-MA Agent Toolkit
# R/28B_generate_universal_exploratory_diagnostics.R
#
# VERSION: 1.0.0-production-candidate
#
# PURPOSE
# ------------------------------------------------------------
# Optional, topic-agnostic exploratory diagnostics downstream of R/27.
# The module never changes canonical R/27 results.
#
# Supported diagnostics
#   - Funnel plot
#   - Egger regression test
#   - Meta-regression + weighted bubble plot
#   - Leave-one-out analysis + plot
#
# Supported effect families
#   - Ratio: RR, OR, HR, IRR, ROM
#   - Difference: MD, SMD, RD
#   - Generic analysis-scale effects (yi + vi / SE)
#
# Effect-source priority
#   1) Canonical R/27 study_effects_used.csv when it contains usable
#      analysis-scale or reported effect information.
#   2) Canonical R/26 meta-analysis-ready dataset.
#   3) Derivation from binary, continuous, or incidence-rate raw data.
#
# SCIENTIFIC SAFETY CONTRACT
# ------------------------------------------------------------
# - R/27 remains the canonical statistical engine.
# - R/28B is OPTIONAL and exploratory.
# - If R/28B cannot reconstruct the same number of studies as R/27,
#   diagnostics are blocked by default (CONTRACT_MISMATCH).
# - Small-k outputs are labelled explicitly:
#       STANDARD_ELIGIBLE
#       EXPLORATORY_UNDERPOWERED
#       DESCRIPTIVE_ONLY
#       NOT_ESTIMABLE
#       CONTRACT_MISMATCH
#       FAIL
# - No topic-, disease-, intervention-, comparator-, author-, or
#   project-specific values are hard-coded.
# OPTIONAL CONFIG CONTRACT
# ------------------------------------------------------------
# The module works without this section, but future projects may define:
#
# analysis:
#   exploratory_diagnostics:
#     allow_partial_effects: false
#     publication_bias:
#       standard_min_k: 10
#     meta_regression:
#       standard_min_k: 10
#       allow_auto_moderators: true
#       candidate_moderators:
#         - publication_year
#         - mean_age
#
# Project-specific moderator names belong in project_config.yml,
# not in this R file.
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
  "metafor",
  "openxlsx"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}


# ============================================================
# 1. Generic helpers
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

safe_chr <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) return(default)
  out <- trimws(as.character(x[[1]]))
  if (is.na(out) || !nzchar(out)) default else out
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

as_flag <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) return(default)
  if (is.logical(x)) return(isTRUE(x[[1]]))
  value <- tolower(trimws(as.character(x[[1]])))
  if (value %in% c("true", "t", "yes", "y", "1", "on")) return(TRUE)
  if (value %in% c("false", "f", "no", "n", "0", "off")) return(FALSE)
  default
}

normalize_slash <- function(x) stringr::str_replace_all(as.character(x), "\\\\", "/")

slugify <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  stringr::str_replace_all(x, "^_|_$", "")
}

normalize_key <- function(x) slugify(x)

normalize_name <- function(x) {
  tolower(gsub("[^a-z0-9]", "", as.character(x)))
}

now_text <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S")

file_nonempty <- function(path) {
  file.exists(path) && !is.na(file.info(path)$size) && file.info(path)$size > 0
}

safe_read_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
}

first_existing_col <- function(data, candidates) {
  hits <- candidates[candidates %in% names(data)]
  if (length(hits) == 0L) NA_character_ else hits[[1]]
}

first_existing_col_normalized <- function(data, requested) {
  if (length(requested) == 0L) return(NA_character_)
  actual_norm <- normalize_name(names(data))
  requested_norm <- normalize_name(requested)
  for (item in requested_norm) {
    hit <- which(actual_norm == item)
    if (length(hit) > 0L) return(names(data)[hit[[1]]])
  }
  NA_character_
}

resolve_path <- function(x, root) {
  x <- safe_chr(x)
  if (!nzchar(x)) return("")
  x <- normalize_slash(x)
  candidate <- if (grepl("^[A-Za-z]:/", x) || grepl("^/", x)) x else file.path(root, x)
  normalizePath(candidate, winslash = "/", mustWork = FALSE)
}

format_num <- function(x, digits = 3) {
  x <- safe_num(x)
  if (length(x) == 0L || is.na(x[[1]])) return("")
  formatC(x[[1]], format = "fg", digits = digits, flag = "#")
}

pick_numeric_scalar <- function(data, candidates, default = NA_real_) {
  col <- first_existing_col(data, candidates)
  if (is.na(col)) return(default)
  values <- safe_num(data[[col]])
  values <- values[!is.na(values)]
  if (length(values) == 0L) default else values[[1]]
}

pick_character_scalar <- function(data, candidates, default = "") {
  col <- first_existing_col(data, candidates)
  if (is.na(col)) return(default)
  safe_chr(data[[col]], default)
}

open_device <- function(type, path, width = 2400, height = 1800, res = 300,
                        pdf_width = 9, pdf_height = 7) {
  if (identical(type, "png")) {
    grDevices::png(filename = path, width = width, height = height, res = res)
  } else {
    grDevices::pdf(file = path, width = pdf_width, height = pdf_height)
  }
}

write_placeholder_plot <- function(png_path, pdf_path, title, message) {
  draw <- function() {
    graphics::plot.new()
    graphics::title(main = title)
    graphics::text(0.5, 0.57, message, cex = 1.0)
    graphics::text(0.5, 0.43, "No inferential result should be interpreted from this panel.", cex = 0.78)
  }
  open_device("png", png_path); draw(); grDevices::dev.off()
  open_device("pdf", pdf_path); draw(); grDevices::dev.off()
}


# ============================================================
# 2. Effect-measure helpers
# ============================================================

normalize_measure <- function(x, default = "GENERIC") {
  value <- toupper(trimws(safe_chr(x, default)))
  aliases <- c(
    "RISK RATIO" = "RR",
    "RELATIVE RISK" = "RR",
    "RATIO OF RISKS" = "RR",
    "ODDS RATIO" = "OR",
    "HAZARD RATIO" = "HR",
    "INCIDENCE RATE RATIO" = "IRR",
    "RATE RATIO" = "IRR",
    "MEAN DIFFERENCE" = "MD",
    "WEIGHTED MEAN DIFFERENCE" = "MD",
    "STANDARDIZED MEAN DIFFERENCE" = "SMD",
    "STANDARDISED MEAN DIFFERENCE" = "SMD",
    "RISK DIFFERENCE" = "RD",
    "RATIO OF MEANS" = "ROM"
  )
  if (value %in% names(aliases)) value <- aliases[[value]]
  if (!value %in% c("RR", "OR", "HR", "IRR", "ROM", "MD", "SMD", "RD", "GENERIC")) {
    value <- "GENERIC"
  }
  value
}

ratio_measure <- function(measure) {
  normalize_measure(measure) %in% c("RR", "OR", "HR", "IRR", "ROM")
}

null_value <- function(measure) if (ratio_measure(measure)) 1 else 0

analysis_to_display <- function(yi, vi, measure, level = 0.95) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  se <- sqrt(vi)
  if (ratio_measure(measure)) {
    tibble::tibble(
      display_effect = exp(yi),
      display_lower = exp(yi - z * se),
      display_upper = exp(yi + z * se)
    )
  } else {
    tibble::tibble(
      display_effect = yi,
      display_lower = yi - z * se,
      display_upper = yi + z * se
    )
  }
}

reported_to_analysis <- function(effect, lower, upper, measure, level = 0.95) {
  z <- stats::qnorm(1 - (1 - level) / 2)
  if (ratio_measure(measure)) {
    valid <- !is.na(effect) & !is.na(lower) & !is.na(upper) & effect > 0 & lower > 0 & upper > 0
    yi <- rep(NA_real_, length(effect))
    sei <- rep(NA_real_, length(effect))
    yi[valid] <- log(effect[valid])
    sei[valid] <- (log(upper[valid]) - log(lower[valid])) / (2 * z)
  } else {
    valid <- !is.na(effect) & !is.na(lower) & !is.na(upper)
    yi <- effect
    sei <- rep(NA_real_, length(effect))
    sei[valid] <- (upper[valid] - lower[valid]) / (2 * z)
  }
  list(yi = yi, vi = sei^2, valid = valid & is.finite(sei) & sei > 0)
}


# ============================================================
# 3. Project + policy configuration
# ============================================================

repo_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
config_file <- file.path(repo_root, "config", "project_config.yml")

if (!file.exists(config_file)) {
  stop("config/project_config.yml was not found. Run R/28B from the project root.", call. = FALSE)
}

config <- yaml::read_yaml(config_file)
if (is.null(config)) stop("project_config.yml could not be parsed.", call. = FALSE)

analysis_config <- config$analysis %||% list()
exploratory_config <- analysis_config$exploratory_diagnostics %||% list()

publication_bias_min_k <- suppressWarnings(as.integer(
  exploratory_config$publication_bias$standard_min_k %||%
    analysis_config$publication_bias$minimum_number_of_studies %||% 10L
))
if (is.na(publication_bias_min_k) || publication_bias_min_k < 3L) publication_bias_min_k <- 10L

meta_regression_min_k <- suppressWarnings(as.integer(
  exploratory_config$meta_regression$standard_min_k %||%
    analysis_config$meta_regression$minimum_number_of_studies %||% 10L
))
if (is.na(meta_regression_min_k) || meta_regression_min_k < 3L) meta_regression_min_k <- 10L

allow_partial_effects <- as_flag(exploratory_config$allow_partial_effects, FALSE)
allow_auto_moderators <- as_flag(exploratory_config$meta_regression$allow_auto_moderators, TRUE)

configured_moderators <- exploratory_config$meta_regression$candidate_moderators %||%
  analysis_config$meta_regression$candidate_moderators %||% character(0)
configured_moderators <- unique(trimws(as.character(unlist(configured_moderators, use.names = FALSE))))
configured_moderators <- configured_moderators[nzchar(configured_moderators)]

confidence_level <- suppressWarnings(as.numeric(
  analysis_config$statistical_significance$confidence_interval %||% 0.95
))
if (is.na(confidence_level) || confidence_level <= 0 || confidence_level >= 1) confidence_level <- 0.95

continuity_correction <- suppressWarnings(as.numeric(
  analysis_config$zero_event_handling$continuity_correction %||% 0.5
))
if (is.na(continuity_correction) || continuity_correction < 0) continuity_correction <- 0.5

tau_method <- toupper(trimws(safe_chr(
  analysis_config$heterogeneity$tau_squared_estimator,
  "REML"
)))
if (!tau_method %in% c("REML", "ML", "DL", "HE", "SJ", "EB", "PM", "GENQ")) tau_method <- "REML"

hartung_knapp <- as_flag(
  analysis_config$heterogeneity$hartung_knapp_adjustment,
  TRUE
)
rma_test <- if (hartung_knapp) "knha" else "z"

default_binary_measure <- normalize_measure(analysis_config$binary_effect_measure %||% "RR", "RR")
default_continuous_measure <- normalize_measure(analysis_config$continuous_effect_measure %||% "MD", "MD")


# ============================================================
# 4. Canonical artifact discovery
# ============================================================

execution_summary_file <- file.path(
  repo_root, "results", "meta_analysis", "meta_analysis_execution_summary.csv"
)

if (!file_nonempty(execution_summary_file)) {
  stop(
    paste0("Canonical R/27 execution summary is missing or empty:\n", normalize_slash(execution_summary_file)),
    call. = FALSE
  )
}

execution_summary_raw <- safe_read_csv(execution_summary_file)
exec_group_col <- first_existing_col(
  execution_summary_raw,
  c("analysis_group", "group_name", "analysis_set", "analysis_name", "label")
)
exec_status_col <- first_existing_col(
  execution_summary_raw,
  c("status", "analysis_status", "execution_status", "result")
)

if (is.na(exec_group_col)) stop("R/27 execution summary lacks an analysis-group column.", call. = FALSE)

execution_summary <- execution_summary_raw |>
  dplyr::mutate(
    analysis_group = as.character(.data[[exec_group_col]]),
    group_key = normalize_key(analysis_group),
    status_normalized = if (!is.na(exec_status_col)) {
      toupper(trimws(as.character(.data[[exec_status_col]])))
    } else {
      "PASS"
    }
  ) |>
  dplyr::filter(status_normalized %in% c("PASS", "PASSED", "SUCCESS", "COMPLETE", "COMPLETED"))

if (nrow(execution_summary) == 0L) stop("No R/27 PASS analysis group was found.", call. = FALSE)

manifest_candidates <- c(
  file.path(repo_root, "data", "processed", "meta_analysis_ready", "meta_analysis_manifest.csv"),
  file.path(repo_root, "data", "processed", "meta_analysis", "meta_analysis_manifest.csv"),
  file.path(repo_root, "results", "meta_analysis", "meta_analysis_manifest.csv")
)
manifest_file <- manifest_candidates[file.exists(manifest_candidates)]
manifest_file <- if (length(manifest_file) > 0L) manifest_file[[1]] else ""

analysis_manifest <- if (nzchar(manifest_file)) safe_read_csv(manifest_file) else NULL

dataset_registry <- tibble::tibble(
  analysis_group = character(),
  group_key = character(),
  dataset_path = character()
)

if (!is.null(analysis_manifest) && nrow(analysis_manifest) > 0L) {
  mg <- first_existing_col(analysis_manifest, c("analysis_group", "group_name", "analysis_set", "label"))
  mp <- first_existing_col(analysis_manifest, c("dataset_path", "analysis_dataset", "source_file", "input_file", "csv_path", "file_path"))
  if (!is.na(mg) && !is.na(mp)) {
    dataset_registry <- tibble::tibble(
      analysis_group = as.character(analysis_manifest[[mg]]),
      group_key = normalize_key(analysis_manifest[[mg]]),
      dataset_path = vapply(analysis_manifest[[mp]], resolve_path, character(1), root = repo_root)
    ) |>
      dplyr::filter(nzchar(dataset_path), file.exists(dataset_path)) |>
      dplyr::distinct(group_key, .keep_all = TRUE)
  }
}

if (nrow(dataset_registry) == 0L) {
  ready_root <- file.path(repo_root, "data", "processed", "meta_analysis_ready")
  ready_files <- if (dir.exists(ready_root)) {
    list.files(ready_root, pattern = "^meta_analysis_ready_.*\\.csv$", full.names = TRUE)
  } else character(0)
  if (length(ready_files) > 0L) {
    names_from_file <- sub(
      "^meta_analysis_ready_", "",
      tools::file_path_sans_ext(basename(ready_files))
    )
    dataset_registry <- tibble::tibble(
      analysis_group = names_from_file,
      group_key = normalize_key(names_from_file),
      dataset_path = normalizePath(ready_files, winslash = "/", mustWork = FALSE)
    )
  }
}

meta_results_root <- file.path(repo_root, "results", "meta_analysis")
summary_files <- list.files(
  meta_results_root,
  pattern = "^meta_analysis_summary\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)
summary_files <- summary_files[
  !grepl("/tables/", normalize_slash(summary_files), fixed = TRUE)
]

r27_registry_rows <- list()
for (summary_path in summary_files) {
  summary_data <- tryCatch(safe_read_csv(summary_path), error = function(e) NULL)
  if (is.null(summary_data) || nrow(summary_data) == 0L) next
  group_dir <- dirname(summary_path)
  sg <- first_existing_col(summary_data, c("analysis_group", "group_name", "analysis_set", "analysis_name", "label"))
  group_name <- if (!is.na(sg)) safe_chr(summary_data[[sg]], basename(group_dir)) else basename(group_dir)
  r27_registry_rows[[length(r27_registry_rows) + 1L]] <- tibble::tibble(
    analysis_group = group_name,
    group_key = normalize_key(group_name),
    r27_summary_file = normalize_slash(summary_path),
    r27_study_file = normalize_slash(file.path(group_dir, "study_effects_used.csv")),
    r27_forest_file = normalize_slash(file.path(group_dir, "forest_plot.pdf")),
    r27_model_file = normalize_slash(file.path(group_dir, "model_output.txt"))
  )
}

r27_registry <- if (length(r27_registry_rows) > 0L) {
  dplyr::bind_rows(r27_registry_rows)
} else {
  tibble::tibble(
    analysis_group = character(), group_key = character(),
    r27_summary_file = character(), r27_study_file = character(),
    r27_forest_file = character(), r27_model_file = character()
  )
}

resolve_group_artifact <- function(analysis_group, group_key) {
  hits <- which(r27_registry$group_key == group_key)
  if (length(hits) == 0L && nrow(r27_registry) == 1L && nrow(execution_summary) == 1L) hits <- 1L
  if (length(hits) == 0L) return(NULL)
  if (length(hits) > 1L) {
    exact <- which(r27_registry$analysis_group[hits] == analysis_group)
    if (length(exact) == 1L) hits <- hits[exact]
  }
  if (length(hits) != 1L) return(NULL)
  as.list(r27_registry[hits[[1]], , drop = FALSE])
}

resolve_dataset <- function(group_key) {
  hits <- which(dataset_registry$group_key == group_key)
  if (length(hits) == 0L && nrow(dataset_registry) == 1L && nrow(execution_summary) == 1L) hits <- 1L
  if (length(hits) != 1L) return("")
  dataset_registry$dataset_path[[hits[[1]]]]
}


# ============================================================
# 5. Study identifiers + effect extraction
# ============================================================

study_id_candidates <- c(
  "screening_id", "study_id", "study_code", "record_id", "id"
)
study_label_candidates <- c(
  "study_label", "studlab", "study", "first_author_year", "author_year",
  "citation_short", "study_name", "study_id", "screening_id", "study_code"
)

make_study_identity <- function(data) {
  id_col <- first_existing_col(data, study_id_candidates)
  label_col <- first_existing_col(data, study_label_candidates)
  ids <- if (!is.na(id_col)) trimws(as.character(data[[id_col]])) else rep("", nrow(data))
  labels <- if (!is.na(label_col)) trimws(as.character(data[[label_col]])) else rep("", nrow(data))
  ids[is.na(ids)] <- ""
  labels[is.na(labels)] <- ""
  missing_label <- !nzchar(labels)
  labels[missing_label] <- ifelse(nzchar(ids[missing_label]), ids[missing_label], paste0("Study_", which(missing_label)))
  keys <- ifelse(nzchar(ids), normalize_key(ids), normalize_key(labels))
  tibble::tibble(study_id = ids, study_label = labels, study_key = keys)
}

infer_measure_vector <- function(data, fallback = default_binary_measure) {
  measure_col <- first_existing_col(data, c("effect_measure", "summary_measure", "measure", "sm"))
  if (is.na(measure_col)) return(rep(normalize_measure(fallback), nrow(data)))
  values <- vapply(data[[measure_col]], normalize_measure, character(1), default = fallback)
  values[is.na(values) | !nzchar(values)] <- normalize_measure(fallback)
  values
}

build_effects_from_analysis_scale <- function(data, source_name, fallback_measure) {
  identity <- make_study_identity(data)
  measure <- infer_measure_vector(data, fallback_measure)
  distinct_measure <- unique(measure)
  if (length(distinct_measure) != 1L) return(NULL)
  measure <- distinct_measure[[1]]

  yi_col <- first_existing_col(data, c("yi", "TE", "te", "log_effect", "analysis_effect", "effect_analysis"))
  vi_col <- first_existing_col(data, c("vi", "variance", "var_effect", "varTE", "var_te"))
  sei_col <- first_existing_col(data, c("sei", "se", "seTE", "sete", "se_log_effect", "standard_error", "effect_se"))
  if (is.na(yi_col) || (is.na(vi_col) && is.na(sei_col))) return(NULL)

  yi <- safe_num(data[[yi_col]])
  vi <- if (!is.na(vi_col)) safe_num(data[[vi_col]]) else safe_num(data[[sei_col]])^2
  valid <- is.finite(yi) & is.finite(vi) & vi > 0
  if (!any(valid)) return(NULL)

  display <- analysis_to_display(yi[valid], vi[valid], measure, confidence_level)
  tibble::tibble(
    study_id = identity$study_id[valid],
    study_label = identity$study_label[valid],
    study_key = identity$study_key[valid],
    measure = measure,
    yi = yi[valid],
    vi = vi[valid],
    sei = sqrt(vi[valid]),
    display_effect = display$display_effect,
    display_lower = display$display_lower,
    display_upper = display$display_upper,
    effect_source = source_name,
    effect_derivation = "ANALYSIS_SCALE",
    source_row = which(valid)
  )
}

build_effects_from_reported <- function(data, source_name, fallback_measure) {
  identity <- make_study_identity(data)
  measure_vec <- infer_measure_vector(data, fallback_measure)
  distinct_measure <- unique(measure_vec)
  if (length(distinct_measure) != 1L) return(NULL)
  measure <- distinct_measure[[1]]

  effect_col <- first_existing_col(data, c(
    "effect_value", "effect_estimate", "estimate", "rr", "or", "hr", "irr",
    "mean_difference", "standardized_mean_difference", "smd", "risk_difference", "rd"
  ))
  lower_col <- first_existing_col(data, c("lower_ci", "ci_lower", "lcl", "lower", "lower95", "ci_lb"))
  upper_col <- first_existing_col(data, c("upper_ci", "ci_upper", "ucl", "upper", "upper95", "ci_ub"))
  if (is.na(effect_col) || is.na(lower_col) || is.na(upper_col)) return(NULL)

  effect <- safe_num(data[[effect_col]])
  lower <- safe_num(data[[lower_col]])
  upper <- safe_num(data[[upper_col]])
  converted <- reported_to_analysis(effect, lower, upper, measure, confidence_level)
  valid <- converted$valid
  if (!any(valid)) return(NULL)

  tibble::tibble(
    study_id = identity$study_id[valid],
    study_label = identity$study_label[valid],
    study_key = identity$study_key[valid],
    measure = measure,
    yi = converted$yi[valid],
    vi = converted$vi[valid],
    sei = sqrt(converted$vi[valid]),
    display_effect = effect[valid],
    display_lower = lower[valid],
    display_upper = upper[valid],
    effect_source = source_name,
    effect_derivation = "REPORTED_EFFECT_AND_CI",
    source_row = which(valid)
  )
}

build_effects_from_effect_se <- function(data, source_name, fallback_measure) {
  identity <- make_study_identity(data)
  measure_vec <- infer_measure_vector(data, fallback_measure)
  distinct_measure <- unique(measure_vec)
  if (length(distinct_measure) != 1L) return(NULL)
  measure <- distinct_measure[[1]]

  effect_col <- first_existing_col(data, c(
    "effect_value", "effect_estimate", "estimate", "effect", "rr", "or", "hr", "irr",
    "mean_difference", "standardized_mean_difference", "smd", "risk_difference", "rd"
  ))
  se_col <- first_existing_col(data, c(
    "se_log_effect", "log_effect_se", "standard_error_log", "effect_se", "standard_error", "se"
  ))
  if (is.na(effect_col) || is.na(se_col)) return(NULL)

  effect <- safe_num(data[[effect_col]])
  sei <- safe_num(data[[se_col]])
  valid <- is.finite(effect) & is.finite(sei) & sei > 0
  if (!any(valid)) return(NULL)

  se_name <- tolower(se_col)
  se_is_log <- grepl("log", se_name, fixed = TRUE)

  if (ratio_measure(measure)) {
    # Ratio measures are accepted with effect+SE only when the SE is explicitly
    # identified as a log-scale SE. Otherwise the scale is ambiguous and this
    # builder yields to effect+CI or raw-data derivation.
    if (!se_is_log) return(NULL)
    valid <- valid & effect > 0
    if (!any(valid)) return(NULL)
    yi <- log(effect[valid])
    vi <- sei[valid]^2
  } else {
    yi <- effect[valid]
    vi <- sei[valid]^2
  }

  display <- analysis_to_display(yi, vi, measure, confidence_level)
  tibble::tibble(
    study_id = identity$study_id[valid],
    study_label = identity$study_label[valid],
    study_key = identity$study_key[valid],
    measure = measure,
    yi = yi,
    vi = vi,
    sei = sqrt(vi),
    display_effect = display$display_effect,
    display_lower = display$display_lower,
    display_upper = display$display_upper,
    effect_source = source_name,
    effect_derivation = "REPORTED_EFFECT_AND_SE",
    source_row = which(valid)
  )
}


build_effects_from_binary <- function(data, source_name, fallback_measure) {
  measure_vec <- infer_measure_vector(data, fallback_measure)
  distinct_measure <- unique(measure_vec)
  if (length(distinct_measure) != 1L) return(NULL)
  measure <- distinct_measure[[1]]
  if (!measure %in% c("RR", "OR", "RD")) return(NULL)

  e1_col <- first_existing_col(data, c(
    "intervention_events", "events_intervention", "events_exposed", "event_treatment",
    "events_treatment", "event_experimental", "treatment_events", "s_event"
  ))
  n1_col <- first_existing_col(data, c(
    "intervention_total", "n_intervention", "n_exposed", "n_treatment",
    "total_treatment", "n_experimental", "s_total"
  ))
  e0_col <- first_existing_col(data, c(
    "comparator_events", "events_comparator", "events_control", "event_control",
    "control_events", "c_event"
  ))
  n0_col <- first_existing_col(data, c(
    "comparator_total", "n_comparator", "n_control", "total_control", "c_total"
  ))
  if (any(is.na(c(e1_col, n1_col, e0_col, n0_col)))) return(NULL)

  ai <- safe_num(data[[e1_col]])
  n1i <- safe_num(data[[n1_col]])
  ci <- safe_num(data[[e0_col]])
  n2i <- safe_num(data[[n0_col]])
  valid <- is.finite(ai) & is.finite(n1i) & is.finite(ci) & is.finite(n2i) &
    ai >= 0 & ci >= 0 & n1i > 0 & n2i > 0 & ai <= n1i & ci <= n2i
  if (!any(valid)) return(NULL)

  bi <- n1i[valid] - ai[valid]
  di <- n2i[valid] - ci[valid]
  escalc_measure <- measure
  es <- metafor::escalc(
    measure = escalc_measure,
    ai = ai[valid], bi = bi,
    ci = ci[valid], di = di,
    add = continuity_correction,
    to = "only0",
    drop00 = TRUE
  )
  yi <- safe_num(es$yi)
  vi <- safe_num(es$vi)
  keep <- is.finite(yi) & is.finite(vi) & vi > 0
  if (!any(keep)) return(NULL)

  identity <- make_study_identity(data)
  rows <- which(valid)[keep]
  display <- analysis_to_display(yi[keep], vi[keep], measure, confidence_level)
  tibble::tibble(
    study_id = identity$study_id[rows],
    study_label = identity$study_label[rows],
    study_key = identity$study_key[rows],
    measure = measure,
    yi = yi[keep],
    vi = vi[keep],
    sei = sqrt(vi[keep]),
    display_effect = display$display_effect,
    display_lower = display$display_lower,
    display_upper = display$display_upper,
    effect_source = source_name,
    effect_derivation = "BINARY_COUNTS",
    source_row = rows
  )
}

build_effects_from_continuous <- function(data, source_name, fallback_measure) {
  measure_vec <- infer_measure_vector(data, fallback_measure)
  distinct_measure <- unique(measure_vec)
  if (length(distinct_measure) != 1L) return(NULL)
  measure <- distinct_measure[[1]]
  if (!measure %in% c("MD", "SMD")) return(NULL)

  m1_col <- first_existing_col(data, c(
    "intervention_mean", "mean_intervention", "mean_exposed", "mean_treatment",
    "treatment_mean", "experimental_mean"
  ))
  sd1_col <- first_existing_col(data, c(
    "intervention_sd", "sd_intervention", "sd_exposed", "sd_treatment",
    "treatment_sd", "experimental_sd"
  ))
  n1_col <- first_existing_col(data, c(
    "intervention_n", "n_intervention", "n_exposed", "n_treatment",
    "treatment_n", "experimental_n"
  ))
  m0_col <- first_existing_col(data, c(
    "comparator_mean", "mean_comparator", "mean_control", "control_mean"
  ))
  sd0_col <- first_existing_col(data, c(
    "comparator_sd", "sd_comparator", "sd_control", "control_sd"
  ))
  n0_col <- first_existing_col(data, c(
    "comparator_n", "n_comparator", "n_control", "control_n"
  ))
  if (any(is.na(c(m1_col, sd1_col, n1_col, m0_col, sd0_col, n0_col)))) return(NULL)

  m1 <- safe_num(data[[m1_col]]); sd1 <- safe_num(data[[sd1_col]]); n1 <- safe_num(data[[n1_col]])
  m0 <- safe_num(data[[m0_col]]); sd0 <- safe_num(data[[sd0_col]]); n0 <- safe_num(data[[n0_col]])
  valid <- is.finite(m1) & is.finite(sd1) & is.finite(n1) & is.finite(m0) & is.finite(sd0) & is.finite(n0) &
    sd1 >= 0 & sd0 >= 0 & n1 > 1 & n0 > 1
  if (!any(valid)) return(NULL)

  es <- metafor::escalc(
    measure = measure,
    m1i = m1[valid], sd1i = sd1[valid], n1i = n1[valid],
    m2i = m0[valid], sd2i = sd0[valid], n2i = n0[valid]
  )
  yi <- safe_num(es$yi); vi <- safe_num(es$vi)
  keep <- is.finite(yi) & is.finite(vi) & vi > 0
  if (!any(keep)) return(NULL)

  identity <- make_study_identity(data)
  rows <- which(valid)[keep]
  display <- analysis_to_display(yi[keep], vi[keep], measure, confidence_level)
  tibble::tibble(
    study_id = identity$study_id[rows],
    study_label = identity$study_label[rows],
    study_key = identity$study_key[rows],
    measure = measure,
    yi = yi[keep],
    vi = vi[keep],
    sei = sqrt(vi[keep]),
    display_effect = display$display_effect,
    display_lower = display$display_lower,
    display_upper = display$display_upper,
    effect_source = source_name,
    effect_derivation = paste0("CONTINUOUS_", measure),
    source_row = rows
  )
}

build_effects_from_rates <- function(data, source_name, fallback_measure) {
  measure_vec <- infer_measure_vector(data, fallback_measure)
  distinct_measure <- unique(measure_vec)
  if (length(distinct_measure) != 1L) return(NULL)
  measure <- distinct_measure[[1]]
  if (!identical(measure, "IRR")) return(NULL)

  x1_col <- first_existing_col(data, c("intervention_events", "events_intervention", "events_exposed", "treatment_events"))
  t1_col <- first_existing_col(data, c("intervention_person_time", "person_time_intervention", "treatment_person_time", "exposed_person_time"))
  x0_col <- first_existing_col(data, c("comparator_events", "events_comparator", "events_control", "control_events"))
  t0_col <- first_existing_col(data, c("comparator_person_time", "person_time_comparator", "control_person_time"))
  if (any(is.na(c(x1_col, t1_col, x0_col, t0_col)))) return(NULL)

  x1 <- safe_num(data[[x1_col]]); t1 <- safe_num(data[[t1_col]])
  x0 <- safe_num(data[[x0_col]]); t0 <- safe_num(data[[t0_col]])
  valid <- is.finite(x1) & is.finite(t1) & is.finite(x0) & is.finite(t0) & x1 >= 0 & x0 >= 0 & t1 > 0 & t0 > 0
  if (!any(valid)) return(NULL)

  es <- metafor::escalc(
    measure = "IRR",
    x1i = x1[valid], t1i = t1[valid],
    x2i = x0[valid], t2i = t0[valid],
    add = continuity_correction,
    to = "only0"
  )
  yi <- safe_num(es$yi); vi <- safe_num(es$vi)
  keep <- is.finite(yi) & is.finite(vi) & vi > 0
  if (!any(keep)) return(NULL)

  identity <- make_study_identity(data)
  rows <- which(valid)[keep]
  display <- analysis_to_display(yi[keep], vi[keep], measure, confidence_level)
  tibble::tibble(
    study_id = identity$study_id[rows],
    study_label = identity$study_label[rows],
    study_key = identity$study_key[rows],
    measure = measure,
    yi = yi[keep],
    vi = vi[keep],
    sei = sqrt(vi[keep]),
    display_effect = display$display_effect,
    display_lower = display$display_lower,
    display_upper = display$display_upper,
    effect_source = source_name,
    effect_derivation = "INCIDENCE_RATE_COUNTS",
    source_row = rows
  )
}

extract_effect_candidate <- function(data, source_name, fallback_measure) {
  if (is.null(data) || nrow(data) == 0L) return(NULL)
  builders <- list(
    build_effects_from_analysis_scale,
    build_effects_from_reported,
    build_effects_from_effect_se,
    build_effects_from_binary,
    build_effects_from_continuous,
    build_effects_from_rates
  )
  for (builder in builders) {
    result <- tryCatch(builder(data, source_name, fallback_measure), error = function(e) NULL)
    if (!is.null(result) && nrow(result) > 0L) return(result)
  }
  NULL
}

canonical_summary_info <- function(summary_data, fallback_measure) {
  if (is.null(summary_data) || nrow(summary_data) == 0L) {
    return(list(k = NA_integer_, measure = normalize_measure(fallback_measure), effect = NA_real_, lower = NA_real_, upper = NA_real_))
  }
  list(
    k = as.integer(pick_numeric_scalar(summary_data, c("n_studies", "k", "studies"), NA_real_)),
    measure = normalize_measure(pick_character_scalar(summary_data, c("summary_measure", "effect_measure", "measure", "sm"), fallback_measure), fallback_measure),
    effect = pick_numeric_scalar(summary_data, c("pooled_effect", "pooled_estimate", "random_effect", "random_effect_estimate", "estimate"), NA_real_),
    lower = pick_numeric_scalar(summary_data, c("pooled_lower_ci", "lower_ci", "random_lower_ci", "lower", "ci_lb"), NA_real_),
    upper = pick_numeric_scalar(summary_data, c("pooled_upper_ci", "upper_ci", "random_upper_ci", "upper", "ci_ub"), NA_real_)
  )
}

choose_effect_candidate <- function(candidates, canonical_k) {
  candidates <- candidates[!vapply(candidates, is.null, logical(1))]
  if (length(candidates) == 0L) return(NULL)
  if (!is.na(canonical_k)) {
    exact <- which(vapply(candidates, nrow, integer(1)) == canonical_k)
    if (length(exact) > 0L) return(candidates[[exact[[1]]]])
  }
  sizes <- vapply(candidates, nrow, integer(1))
  candidates[[which.max(sizes)]]
}


# ============================================================
# 6. Moderator discovery
# ============================================================

generic_moderator_aliases <- list(
  publication_year = c("publication_year", "study_year", "year"),
  mean_age = c("mean_age", "age_mean", "mean_patient_age", "average_age"),
  female_proportion = c("female_proportion", "proportion_female", "female_percent", "percent_female", "female_percentage"),
  follow_up = c("followup_months", "follow_up_months", "followup_days", "follow_up_days", "followup_years", "follow_up_years"),
  baseline_risk = c("baseline_risk", "control_event_rate", "comparator_event_rate")
)

match_effects_to_dataset_rows <- function(effects, dataset) {
  identity <- make_study_identity(dataset)
  match_rows <- match(effects$study_key, identity$study_key)
  if (all(is.na(match_rows)) && identical(unique(effects$effect_source), "ANALYSIS_DATASET")) {
    candidate <- effects$source_row
    candidate[candidate < 1L | candidate > nrow(dataset)] <- NA_integer_
    match_rows <- candidate
  }
  match_rows
}

candidate_moderator_columns <- function(dataset) {
  out <- list()
  for (item in configured_moderators) {
    col <- first_existing_col_normalized(dataset, item)
    if (!is.na(col)) out[[col]] <- "CONFIGURED"
  }
  if (allow_auto_moderators) {
    for (alias_group in generic_moderator_aliases) {
      col <- first_existing_col_normalized(dataset, alias_group)
      if (!is.na(col) && is.null(out[[col]])) out[[col]] <- "AUTO_INFERRED"
    }
  }
  out
}

build_moderator_registry <- function(dataset, effects) {
  cols <- candidate_moderator_columns(dataset)
  if (length(cols) == 0L) {
    return(tibble::tibble(
      moderator = character(), moderator_source = character(), moderator_type = character(),
      k_available = integer(), unique_values = integer(), usable = logical()
    ))
  }
  rows <- match_effects_to_dataset_rows(effects, dataset)
  registry <- list()
  for (col in names(cols)) {
    raw <- dataset[[col]][rows]
    numeric_values <- safe_num(raw)
    numeric_nonmissing <- sum(!is.na(numeric_values))
    raw_nonmissing <- sum(!is.na(raw) & nzchar(trimws(as.character(raw))))
    if (numeric_nonmissing >= max(2L, ceiling(0.8 * raw_nonmissing))) {
      moderator_type <- "NUMERIC"
      values <- numeric_values
      uniq <- length(unique(values[!is.na(values)]))
      k_avail <- sum(!is.na(values))
    } else {
      moderator_type <- "CATEGORICAL"
      values <- trimws(as.character(raw))
      values[is.na(values) | !nzchar(values)] <- NA_character_
      uniq <- length(unique(values[!is.na(values)]))
      k_avail <- sum(!is.na(values))
    }
    registry[[length(registry) + 1L]] <- tibble::tibble(
      moderator = col,
      moderator_source = cols[[col]],
      moderator_type = moderator_type,
      k_available = k_avail,
      unique_values = uniq,
      usable = k_avail >= 2L && uniq >= 2L
    )
  }
  dplyr::bind_rows(registry)
}


# ============================================================
# 7. Diagnostic model helpers
# ============================================================

fit_random_effects <- function(yi, vi) {
  metafor::rma(yi = yi, vi = vi, method = tau_method, test = rma_test)
}

canonical_center_on_analysis_scale <- function(summary_info, effects) {
  if (!is.na(summary_info$effect)) {
    if (ratio_measure(summary_info$measure)) {
      if (summary_info$effect > 0) {
        return(list(value = log(summary_info$effect), source = "R27_CANONICAL_POOLED"))
      }
    } else {
      return(list(value = summary_info$effect, source = "R27_CANONICAL_POOLED"))
    }
  }
  value <- stats::weighted.mean(effects$yi, w = 1 / effects$vi, na.rm = TRUE)
  list(value = value, source = "IV_FIXED_CENTER_FALLBACK")
}


# ============================================================
# 8. Funnel + Egger
# ============================================================

run_funnel <- function(effects, summary_info, group_dir, analysis_group) {
  png_path <- file.path(group_dir, "funnel_plot.png")
  pdf_path <- file.path(group_dir, "funnel_plot.pdf")
  k <- nrow(effects)
  if (k < 2L) {
    write_placeholder_plot(png_path, pdf_path, paste0("Funnel plot — ", analysis_group), "Fewer than 2 study effects available")
    return(list(status = "NOT_ESTIMABLE", center_source = "NONE", png = png_path, pdf = pdf_path))
  }
  status <- if (k >= publication_bias_min_k) "STANDARD_ELIGIBLE" else "DESCRIPTIVE_ONLY"
  center <- canonical_center_on_analysis_scale(summary_info, effects)

  draw <- function() {
    x <- effects$yi; y <- effects$sei
    ylim <- rev(range(c(0, y), finite = TRUE))
    graphics::plot(
      x, y, pch = 19,
      xlab = paste0("Study effect (", effects$measure[[1]], if (ratio_measure(effects$measure[[1]])) ", analysis log scale" else "", ")"),
      ylab = "Standard error",
      main = paste0("Funnel plot — ", analysis_group),
      ylim = ylim
    )
    graphics::abline(v = center$value, lty = 2)
    se_grid <- seq(0, max(y, na.rm = TRUE) * 1.08, length.out = 100)
    z <- stats::qnorm(1 - (1 - confidence_level) / 2)
    graphics::lines(center$value - z * se_grid, se_grid, lty = 3)
    graphics::lines(center$value + z * se_grid, se_grid, lty = 3)
    graphics::text(x, y, labels = effects$study_label, pos = 4, cex = 0.72)
    graphics::mtext(
      paste0("k=", k, "; ", status, "; center=", center$source,
             if (k < publication_bias_min_k) paste0("; configured standard threshold=", publication_bias_min_k) else ""),
      side = 1, line = 3.2, cex = 0.68
    )
  }
  open_device("png", png_path); draw(); grDevices::dev.off()
  open_device("pdf", pdf_path); draw(); grDevices::dev.off()
  list(status = status, center_source = center$source, png = png_path, pdf = pdf_path)
}

run_egger <- function(effects, group_dir) {
  output_file <- file.path(group_dir, "egger_test.csv")
  k <- nrow(effects)
  if (k < 3L) {
    result <- tibble::tibble(
      k = k, status = "NOT_ESTIMABLE", intercept_or_bias_estimate = NA_real_,
      statistic = NA_real_, p_value = NA_real_, standard_min_k = publication_bias_min_k,
      interpretation = paste0("Egger regression requires at least 3 studies for estimation; available k=", k, ".")
    )
    readr::write_csv(result, output_file, na = "")
    return(result)
  }
  fit <- tryCatch(
    metafor::regtest(yi = effects$yi, sei = effects$sei, model = "lm", predictor = "sei"),
    error = function(e) e
  )
  if (inherits(fit, "error")) {
    result <- tibble::tibble(
      k = k, status = "NOT_ESTIMABLE", intercept_or_bias_estimate = NA_real_,
      statistic = NA_real_, p_value = NA_real_, standard_min_k = publication_bias_min_k,
      interpretation = conditionMessage(fit)
    )
  } else {
    estimate <- suppressWarnings(as.numeric(fit$est))
    statistic <- suppressWarnings(as.numeric(fit$zval %||% fit$tval))
    p_value <- suppressWarnings(as.numeric(fit$pval))
    result <- tibble::tibble(
      k = k,
      status = if (k >= publication_bias_min_k) "STANDARD_ELIGIBLE" else "EXPLORATORY_UNDERPOWERED",
      intercept_or_bias_estimate = if (length(estimate) > 0L) estimate[[1]] else NA_real_,
      statistic = if (length(statistic) > 0L) statistic[[1]] else NA_real_,
      p_value = if (length(p_value) > 0L) p_value[[1]] else NA_real_,
      standard_min_k = publication_bias_min_k,
      interpretation = if (k >= publication_bias_min_k) {
        "Egger regression estimated at or above the configured standard threshold."
      } else {
        paste0("Egger regression estimated only as exploratory/underpowered evidence; k=", k,
               " is below the configured standard threshold of ", publication_bias_min_k, ".")
      }
    )
  }
  readr::write_csv(result, output_file, na = "")
  result
}


# ============================================================
# 9. Meta-regression + moderator plots
# ============================================================

run_one_meta_regression <- function(dataset, effects, moderator_row, group_dir, analysis_group) {
  moderator <- moderator_row$moderator[[1]]
  moderator_source <- moderator_row$moderator_source[[1]]
  moderator_type <- moderator_row$moderator_type[[1]]
  rows <- match_effects_to_dataset_rows(effects, dataset)
  raw <- dataset[[moderator]][rows]

  result_file <- file.path(group_dir, paste0("meta_regression_", slugify(moderator), ".csv"))
  png_path <- file.path(group_dir, paste0("bubble_plot_", slugify(moderator), ".png"))
  pdf_path <- file.path(group_dir, paste0("bubble_plot_", slugify(moderator), ".pdf"))

  if (moderator_type == "NUMERIC") {
    x <- safe_num(raw)
    valid <- is.finite(x) & is.finite(effects$yi) & is.finite(effects$vi) & effects$vi > 0
    x <- x[valid]; yi <- effects$yi[valid]; vi <- effects$vi[valid]; labels <- effects$study_label[valid]
    k <- length(yi)
    if (k < 2L || length(unique(x)) < 2L) {
      result <- tibble::tibble(
        moderator = moderator, moderator_source = moderator_source, moderator_type = moderator_type,
        k = k, status = "NOT_ESTIMABLE", coefficient = NA_real_, se = NA_real_, p_value = NA_real_,
        standard_min_k = meta_regression_min_k,
        interpretation = "Insufficient non-missing studies or moderator variation."
      )
      readr::write_csv(result, result_file, na = "")
      write_placeholder_plot(png_path, pdf_path, paste0("Moderator plot — ", moderator), "Insufficient moderator data")
      return(list(result = result, png = png_path, pdf = pdf_path))
    }

    weights <- 1 / vi
    if (k == 2L) {
      descriptive_fit <- stats::lm(yi ~ x, weights = weights)
      coef_value <- suppressWarnings(as.numeric(stats::coef(descriptive_fit)[[2]]))
      result <- tibble::tibble(
        moderator = moderator, moderator_source = moderator_source, moderator_type = moderator_type,
        k = k, status = "DESCRIPTIVE_ONLY", coefficient = coef_value, se = NA_real_, p_value = NA_real_,
        standard_min_k = meta_regression_min_k,
        interpretation = "Weighted line shown descriptively; inferential meta-regression is not estimable with k=2."
      )
      prediction_fun <- function(new_x) stats::predict(descriptive_fit, newdata = data.frame(x = new_x))
    } else {
      fit <- tryCatch(
        metafor::rma(yi = yi, vi = vi, mods = ~ x, method = tau_method, test = rma_test),
        error = function(e) e
      )
      if (inherits(fit, "error")) {
        result <- tibble::tibble(
          moderator = moderator, moderator_source = moderator_source, moderator_type = moderator_type,
          k = k, status = "NOT_ESTIMABLE", coefficient = NA_real_, se = NA_real_, p_value = NA_real_,
          standard_min_k = meta_regression_min_k, interpretation = conditionMessage(fit)
        )
        prediction_fun <- NULL
      } else {
        beta <- as.numeric(fit$beta); se <- as.numeric(fit$se); pval <- as.numeric(fit$pval)
        result <- tibble::tibble(
          moderator = moderator, moderator_source = moderator_source, moderator_type = moderator_type,
          k = k,
          status = if (k >= meta_regression_min_k) "STANDARD_ELIGIBLE" else "EXPLORATORY_UNDERPOWERED",
          coefficient = if (length(beta) >= 2L) beta[[2]] else NA_real_,
          se = if (length(se) >= 2L) se[[2]] else NA_real_,
          p_value = if (length(pval) >= 2L) pval[[2]] else NA_real_,
          standard_min_k = meta_regression_min_k,
          interpretation = if (k >= meta_regression_min_k) {
            "Random-effects meta-regression estimated at or above the configured standard threshold."
          } else {
            paste0("Random-effects meta-regression is exploratory/underpowered; k=", k,
                   " is below the configured standard threshold of ", meta_regression_min_k, ".")
          }
        )
        prediction_fun <- function(new_x) {
          as.numeric(metafor::predict.rma(fit, newmods = matrix(new_x, ncol = 1))$pred)
        }
      }
    }

    readr::write_csv(result, result_file, na = "")
    bubble_cex <- 0.9 + 2.2 * sqrt(weights / max(weights, na.rm = TRUE))
    draw <- function() {
      graphics::plot(
        x, yi, pch = 21, cex = bubble_cex,
        xlab = moderator,
        ylab = paste0("Study effect (", effects$measure[[1]], if (ratio_measure(effects$measure[[1]])) ", analysis log scale" else "", ")"),
        main = paste0("Meta-regression / bubble plot — ", analysis_group)
      )
      graphics::text(x, yi, labels = labels, pos = 4, cex = 0.70)
      if (!is.null(prediction_fun)) {
        x_grid <- seq(min(x), max(x), length.out = 100)
        graphics::lines(x_grid, prediction_fun(x_grid), lwd = 1.4)
      }
      graphics::mtext(
        paste0(result$status[[1]], "; moderator=", moderator, "; source=", moderator_source, "; k=", k),
        side = 1, line = 3.2, cex = 0.67
      )
    }
    open_device("png", png_path); draw(); grDevices::dev.off()
    open_device("pdf", pdf_path); draw(); grDevices::dev.off()
    return(list(result = result, png = png_path, pdf = pdf_path))
  }

  # Categorical moderators are supported inferentially when degrees of freedom exist,
  # but the visual is a weighted categorical moderator plot rather than a numeric bubble trend.
  x <- trimws(as.character(raw))
  x[is.na(x) | !nzchar(x)] <- NA_character_
  valid <- !is.na(x) & is.finite(effects$yi) & is.finite(effects$vi) & effects$vi > 0
  x <- factor(x[valid]); yi <- effects$yi[valid]; vi <- effects$vi[valid]; labels <- effects$study_label[valid]
  k <- length(yi); levels_n <- nlevels(x)
  if (k < 3L || levels_n < 2L || k <= levels_n) {
    result <- tibble::tibble(
      moderator = moderator, moderator_source = moderator_source, moderator_type = moderator_type,
      k = k, status = if (k >= 2L && levels_n >= 2L) "DESCRIPTIVE_ONLY" else "NOT_ESTIMABLE",
      coefficient = NA_real_, se = NA_real_, p_value = NA_real_, standard_min_k = meta_regression_min_k,
      interpretation = "Categorical moderator lacks sufficient residual degrees of freedom for formal meta-regression."
    )
  } else {
    fit <- tryCatch(
      metafor::rma(yi = yi, vi = vi, mods = ~ x, method = tau_method, test = rma_test),
      error = function(e) e
    )
    if (inherits(fit, "error")) {
      result <- tibble::tibble(
        moderator = moderator, moderator_source = moderator_source, moderator_type = moderator_type,
        k = k, status = "NOT_ESTIMABLE", coefficient = NA_real_, se = NA_real_, p_value = NA_real_,
        standard_min_k = meta_regression_min_k, interpretation = conditionMessage(fit)
      )
    } else {
      omnibus_p <- suppressWarnings(as.numeric(fit$QMp))
      result <- tibble::tibble(
        moderator = moderator, moderator_source = moderator_source, moderator_type = moderator_type,
        k = k, status = if (k >= meta_regression_min_k) "STANDARD_ELIGIBLE" else "EXPLORATORY_UNDERPOWERED",
        coefficient = NA_real_, se = NA_real_, p_value = if (length(omnibus_p) > 0L) omnibus_p[[1]] else NA_real_,
        standard_min_k = meta_regression_min_k,
        interpretation = "p_value is the omnibus moderator test for the categorical moderator."
      )
    }
  }
  readr::write_csv(result, result_file, na = "")

  weights <- 1 / vi
  cex <- 0.9 + 2.2 * sqrt(weights / max(weights, na.rm = TRUE))
  x_num <- as.numeric(x)
  draw <- function() {
    graphics::plot(
      jitter(x_num, amount = 0.08), yi, pch = 21, cex = cex,
      xaxt = "n", xlab = moderator,
      ylab = paste0("Study effect (", effects$measure[[1]], if (ratio_measure(effects$measure[[1]])) ", analysis log scale" else "", ")"),
      main = paste0("Categorical moderator plot — ", analysis_group)
    )
    graphics::axis(1, at = seq_along(levels(x)), labels = levels(x))
    graphics::text(jitter(x_num, amount = 0.08), yi, labels = labels, pos = 4, cex = 0.68)
    graphics::mtext(paste0(result$status[[1]], "; moderator=", moderator, "; source=", moderator_source, "; k=", k), side = 1, line = 3.2, cex = 0.67)
  }
  open_device("png", png_path); draw(); grDevices::dev.off()
  open_device("pdf", pdf_path); draw(); grDevices::dev.off()
  list(result = result, png = png_path, pdf = pdf_path)
}

run_meta_regressions <- function(dataset, effects, moderator_registry, group_dir, analysis_group) {
  usable <- moderator_registry |>
    dplyr::filter(usable)
  if (nrow(usable) == 0L) {
    result <- tibble::tibble(
      moderator = "", moderator_source = "NONE", moderator_type = "",
      k = nrow(effects), status = "NOT_ESTIMABLE", coefficient = NA_real_, se = NA_real_, p_value = NA_real_,
      standard_min_k = meta_regression_min_k,
      interpretation = "No configured or automatically recognized moderator has sufficient non-missing variation."
    )
    readr::write_csv(result, file.path(group_dir, "meta_regression_results.csv"), na = "")
    png <- file.path(group_dir, "bubble_plot_no_usable_moderator.png")
    pdf <- file.path(group_dir, "bubble_plot_no_usable_moderator.pdf")
    write_placeholder_plot(png, pdf, paste0("Meta-regression — ", analysis_group), "No usable moderator available")
    manifest <- tibble::tibble(moderator = "", png_file = normalize_slash(png), pdf_file = normalize_slash(pdf), status = "NOT_ESTIMABLE")
    return(list(results = result, manifest = manifest))
  }

  result_rows <- list(); manifest_rows <- list()
  for (i in seq_len(nrow(usable))) {
    one <- run_one_meta_regression(dataset, effects, usable[i, , drop = FALSE], group_dir, analysis_group)
    result_rows[[length(result_rows) + 1L]] <- one$result
    manifest_rows[[length(manifest_rows) + 1L]] <- tibble::tibble(
      moderator = one$result$moderator[[1]],
      png_file = normalize_slash(one$png),
      pdf_file = normalize_slash(one$pdf),
      status = one$result$status[[1]]
    )
  }
  results <- dplyr::bind_rows(result_rows)
  manifest <- dplyr::bind_rows(manifest_rows)
  readr::write_csv(results, file.path(group_dir, "meta_regression_results.csv"), na = "")
  readr::write_csv(manifest, file.path(group_dir, "meta_regression_figure_manifest.csv"), na = "")
  list(results = results, manifest = manifest)
}


# ============================================================
# 10. Leave-one-out
# ============================================================

run_leave_one_out <- function(effects, summary_info, group_dir, analysis_group) {
  csv_path <- file.path(group_dir, "leave_one_out.csv")
  png_path <- file.path(group_dir, "leave_one_out_plot.png")
  pdf_path <- file.path(group_dir, "leave_one_out_plot.pdf")
  k <- nrow(effects)
  rows <- list()

  rows[[1]] <- tibble::tibble(
    omitted_study = "None",
    studies_remaining = k,
    status = "CANONICAL_ALL_STUDIES",
    effect = summary_info$effect,
    lower_ci = summary_info$lower,
    upper_ci = summary_info$upper,
    source = "R27_CANONICAL_SUMMARY"
  )

  if (k < 2L) {
    result <- dplyr::bind_rows(rows)
    readr::write_csv(result, csv_path, na = "")
    write_placeholder_plot(png_path, pdf_path, paste0("Leave-one-out — ", analysis_group), "Fewer than 2 study effects available")
    return(list(result = result, status = "NOT_ESTIMABLE", png = png_path, pdf = pdf_path))
  }

  for (i in seq_len(k)) {
    keep <- setdiff(seq_len(k), i)
    if (length(keep) == 1L) {
      display <- analysis_to_display(effects$yi[keep], effects$vi[keep], effects$measure[[1]], confidence_level)
      rows[[length(rows) + 1L]] <- tibble::tibble(
        omitted_study = effects$study_label[[i]],
        studies_remaining = 1L,
        status = "SINGLE_STUDY_DESCRIPTIVE",
        effect = display$display_effect[[1]], lower_ci = display$display_lower[[1]], upper_ci = display$display_upper[[1]],
        source = "REMAINING_SINGLE_STUDY"
      )
    } else {
      fit <- tryCatch(fit_random_effects(effects$yi[keep], effects$vi[keep]), error = function(e) e)
      if (inherits(fit, "error")) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          omitted_study = effects$study_label[[i]], studies_remaining = length(keep), status = "NOT_ESTIMABLE",
          effect = NA_real_, lower_ci = NA_real_, upper_ci = NA_real_, source = "EXPLORATORY_REFIT_FAILED"
        )
      } else {
        yi <- as.numeric(fit$b[[1]]); vi_approx <- as.numeric(fit$se[[1]])^2
        display <- analysis_to_display(yi, vi_approx, effects$measure[[1]], confidence_level)
        rows[[length(rows) + 1L]] <- tibble::tibble(
          omitted_study = effects$study_label[[i]], studies_remaining = length(keep),
          status = if (k <= 3L) "EXPLORATORY_SMALL_K" else "ESTIMATED",
          effect = display$display_effect[[1]], lower_ci = display$display_lower[[1]], upper_ci = display$display_upper[[1]],
          source = "EXPLORATORY_RANDOM_EFFECTS_REFIT"
        )
      }
    }
  }

  result <- dplyr::bind_rows(rows)
  readr::write_csv(result, csv_path, na = "")
  plot_data <- result |>
    dplyr::filter(omitted_study != "None", is.finite(effect), is.finite(lower_ci), is.finite(upper_ci))

  if (nrow(plot_data) == 0L) {
    write_placeholder_plot(png_path, pdf_path, paste0("Leave-one-out — ", analysis_group), "No estimable leave-one-out rows")
  } else {
    draw <- function() {
      y <- rev(seq_len(nrow(plot_data)))
      vals <- c(plot_data$lower_ci, plot_data$upper_ci, summary_info$effect)
      vals <- vals[is.finite(vals)]
      if (ratio_measure(effects$measure[[1]])) {
        vals <- vals[vals > 0]
        xlim <- range(vals, finite = TRUE)
        xlim <- c(xlim[[1]] / 1.25, xlim[[2]] * 1.25)
        graphics::plot(NA, xlim = xlim, ylim = c(0.5, nrow(plot_data) + 0.5), log = "x", yaxt = "n",
                       ylab = "", xlab = paste0(effects$measure[[1]], " (95% CI)"),
                       main = paste0("Leave-one-out — ", analysis_group))
        graphics::abline(v = 1, lty = 3)
      } else {
        xlim <- range(vals, finite = TRUE); span <- diff(xlim); if (!is.finite(span) || span == 0) span <- 1
        xlim <- c(xlim[[1]] - 0.1 * span, xlim[[2]] + 0.1 * span)
        graphics::plot(NA, xlim = xlim, ylim = c(0.5, nrow(plot_data) + 0.5), yaxt = "n",
                       ylab = "", xlab = paste0(effects$measure[[1]], " (95% CI)"),
                       main = paste0("Leave-one-out — ", analysis_group))
        graphics::abline(v = 0, lty = 3)
      }
      if (is.finite(summary_info$effect)) graphics::abline(v = summary_info$effect, lty = 2)
      graphics::segments(plot_data$lower_ci, y, plot_data$upper_ci, y)
      graphics::points(plot_data$effect, y, pch = 15)
      graphics::axis(2, at = y, labels = paste0("Omit ", plot_data$omitted_study), las = 1, cex.axis = 0.72)
      graphics::mtext(
        if (k == 2L) "k=2: each omission leaves one study; descriptive only" else paste0("Original k=", k, "; exploratory refits"),
        side = 1, line = 3.2, cex = 0.68
      )
    }
    open_device("png", png_path, height = max(1500, 600 + 320 * nrow(plot_data))); draw(); grDevices::dev.off()
    open_device("pdf", pdf_path, pdf_height = max(5, 2.8 + 0.55 * nrow(plot_data))); draw(); grDevices::dev.off()
  }

  list(
    result = result,
    status = if (k == 2L) "DESCRIPTIVE_ONLY" else if (k == 3L) "EXPLORATORY_SMALL_K" else "GENERATED",
    png = png_path,
    pdf = pdf_path
  )
}


# ============================================================
# 11. Output directories
# ============================================================

output_root <- file.path(repo_root, "results", "meta_analysis_exploratory_diagnostics")
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

combined_summary_file <- file.path(output_root, "exploratory_diagnostics_summary.csv")
combined_contract_file <- file.path(output_root, "exploratory_effect_contract.csv")
combined_workbook_file <- file.path(output_root, "exploratory_diagnostics.xlsx")
report_file <- file.path(output_root, "exploratory_diagnostics_report.txt")

summary_rows <- list()
contract_rows <- list()
workbook_payload <- list()


# ============================================================
# 12. Run each canonical R/27 PASS group
# ============================================================

for (i in seq_len(nrow(execution_summary))) {
  analysis_group <- execution_summary$analysis_group[[i]]
  group_key <- execution_summary$group_key[[i]]
  group_dir <- file.path(output_root, slugify(analysis_group))
  dir.create(group_dir, recursive = TRUE, showWarnings = FALSE)

  cat("------------------------------------------------------------\n",
      "R/28B universal exploratory diagnostics: ", analysis_group, "\n",
      "------------------------------------------------------------\n", sep = "")

  artifacts <- resolve_group_artifact(analysis_group, group_key)
  dataset_path <- resolve_dataset(group_key)
  dataset <- if (nzchar(dataset_path) && file_nonempty(dataset_path)) safe_read_csv(dataset_path) else NULL
  r27_summary <- if (!is.null(artifacts) && file_nonempty(artifacts$r27_summary_file)) safe_read_csv(artifacts$r27_summary_file) else NULL
  r27_studies <- if (!is.null(artifacts) && file_nonempty(artifacts$r27_study_file)) safe_read_csv(artifacts$r27_study_file) else NULL

  fallback_measure <- if (!is.null(r27_summary)) {
    pick_character_scalar(r27_summary, c("summary_measure", "effect_measure", "measure", "sm"), default_binary_measure)
  } else default_binary_measure
  summary_info <- canonical_summary_info(r27_summary, fallback_measure)
  if (is.na(summary_info$k)) {
    exec_k_col <- first_existing_col(execution_summary, c("n_studies", "k", "studies"))
    if (!is.na(exec_k_col)) {
      exec_k_value <- suppressWarnings(as.integer(execution_summary[[exec_k_col]][[i]]))
      if (!is.na(exec_k_value)) summary_info$k <- exec_k_value
    }
  }

  candidates <- list()
  if (!is.null(r27_studies)) {
    candidates[[length(candidates) + 1L]] <- extract_effect_candidate(r27_studies, "R27_STUDY_EFFECTS", summary_info$measure)
  }
  if (!is.null(dataset)) {
    candidates[[length(candidates) + 1L]] <- extract_effect_candidate(dataset, "ANALYSIS_DATASET", summary_info$measure)
  }
  effects <- choose_effect_candidate(candidates, summary_info$k)

  if (is.null(effects) || nrow(effects) == 0L) {
    status <- tibble::tibble(
      analysis_group = analysis_group, canonical_k = summary_info$k, reconstructed_k = 0L,
      effect_measure = summary_info$measure, effect_contract_status = "FAIL",
      funnel_status = "NOT_ESTIMABLE", egger_status = "NOT_ESTIMABLE",
      meta_regression_status = "NOT_ESTIMABLE", leave_one_out_status = "NOT_ESTIMABLE",
      canonical_meta_analysis_modified = FALSE,
      message = "Study-level effect sizes could not be reconstructed from canonical R/27 artifacts or the meta-analysis-ready dataset."
    )
    readr::write_csv(status, file.path(group_dir, "diagnostic_status.csv"), na = "")
    summary_rows[[length(summary_rows) + 1L]] <- status
    contract_rows[[length(contract_rows) + 1L]] <- tibble::tibble(
      analysis_group = analysis_group, canonical_k = summary_info$k, reconstructed_k = 0L,
      contract_status = "FAIL", allow_partial_effects = allow_partial_effects,
      effect_source = "", effect_derivation = ""
    )
    cat("Status: FAIL — no study-level effects reconstructed.\n\n")
    next
  }

  effects <- effects |>
    dplyr::filter(is.finite(yi), is.finite(vi), vi > 0) |>
    dplyr::distinct(study_key, .keep_all = TRUE)

  reconstructed_k <- nrow(effects)
  canonical_k <- summary_info$k
  contract_match <- is.na(canonical_k) || reconstructed_k == canonical_k
  contract_status <- if (contract_match) "PASS" else "CONTRACT_MISMATCH"

  contract_row <- tibble::tibble(
    analysis_group = analysis_group,
    canonical_k = canonical_k,
    reconstructed_k = reconstructed_k,
    contract_status = contract_status,
    allow_partial_effects = allow_partial_effects,
    effect_source = paste(unique(effects$effect_source), collapse = ";"),
    effect_derivation = paste(unique(effects$effect_derivation), collapse = ";"),
    effect_measure = effects$measure[[1]],
    r27_summary_file = if (!is.null(artifacts)) normalize_slash(artifacts$r27_summary_file) else "",
    r27_study_file = if (!is.null(artifacts)) normalize_slash(artifacts$r27_study_file) else "",
    analysis_dataset = normalize_slash(dataset_path)
  )
  readr::write_csv(contract_row, file.path(group_dir, "effect_contract.csv"), na = "")
  readr::write_csv(effects, file.path(group_dir, "study_effects_for_exploratory_diagnostics.csv"), na = "")
  contract_rows[[length(contract_rows) + 1L]] <- contract_row

  if (!contract_match && !allow_partial_effects) {
    status <- tibble::tibble(
      analysis_group = analysis_group, canonical_k = canonical_k, reconstructed_k = reconstructed_k,
      effect_measure = effects$measure[[1]], effect_contract_status = "CONTRACT_MISMATCH",
      funnel_status = "CONTRACT_MISMATCH", egger_status = "CONTRACT_MISMATCH",
      meta_regression_status = "CONTRACT_MISMATCH", leave_one_out_status = "CONTRACT_MISMATCH",
      canonical_meta_analysis_modified = FALSE,
      message = "Diagnostics blocked because reconstructed study count does not match canonical R/27 study count."
    )
    readr::write_csv(status, file.path(group_dir, "diagnostic_status.csv"), na = "")
    summary_rows[[length(summary_rows) + 1L]] <- status
    cat("Status: CONTRACT_MISMATCH — diagnostics blocked.\n\n")
    next
  }

  funnel <- tryCatch(
    run_funnel(effects, summary_info, group_dir, analysis_group),
    error = function(e) {
      png <- file.path(group_dir, "funnel_plot.png")
      pdf <- file.path(group_dir, "funnel_plot.pdf")
      write_placeholder_plot(png, pdf, paste0("Funnel plot — ", analysis_group), conditionMessage(e))
      list(status = "FAIL", center_source = "NONE", png = png, pdf = pdf)
    }
  )

  egger <- tryCatch(
    run_egger(effects, group_dir),
    error = function(e) {
      result <- tibble::tibble(
        k = nrow(effects), status = "FAIL", intercept_or_bias_estimate = NA_real_,
        statistic = NA_real_, p_value = NA_real_, standard_min_k = publication_bias_min_k,
        interpretation = conditionMessage(e)
      )
      readr::write_csv(result, file.path(group_dir, "egger_test.csv"), na = "")
      result
    }
  )

  moderator_registry <- if (!is.null(dataset)) build_moderator_registry(dataset, effects) else tibble::tibble(
    moderator = character(), moderator_source = character(), moderator_type = character(),
    k_available = integer(), unique_values = integer(), usable = logical()
  )
  readr::write_csv(moderator_registry, file.path(group_dir, "moderator_registry.csv"), na = "")

  meta_reg <- if (!is.null(dataset)) {
    tryCatch(
      run_meta_regressions(dataset, effects, moderator_registry, group_dir, analysis_group),
      error = function(e) {
        result <- tibble::tibble(
          moderator = "", moderator_source = "NONE", moderator_type = "", k = reconstructed_k,
          status = "FAIL", coefficient = NA_real_, se = NA_real_, p_value = NA_real_,
          standard_min_k = meta_regression_min_k, interpretation = conditionMessage(e)
        )
        readr::write_csv(result, file.path(group_dir, "meta_regression_results.csv"), na = "")
        png <- file.path(group_dir, "bubble_plot_failed.png")
        pdf <- file.path(group_dir, "bubble_plot_failed.pdf")
        write_placeholder_plot(png, pdf, paste0("Meta-regression — ", analysis_group), conditionMessage(e))
        list(results = result, manifest = tibble::tibble(moderator = "", png_file = normalize_slash(png), pdf_file = normalize_slash(pdf), status = "FAIL"))
      }
    )
  } else {
    result <- tibble::tibble(
      moderator = "", moderator_source = "NONE", moderator_type = "", k = reconstructed_k,
      status = "NOT_ESTIMABLE", coefficient = NA_real_, se = NA_real_, p_value = NA_real_,
      standard_min_k = meta_regression_min_k, interpretation = "No analysis dataset was available for moderator extraction."
    )
    readr::write_csv(result, file.path(group_dir, "meta_regression_results.csv"), na = "")
    png <- file.path(group_dir, "bubble_plot_no_dataset.png"); pdf <- file.path(group_dir, "bubble_plot_no_dataset.pdf")
    write_placeholder_plot(png, pdf, paste0("Meta-regression — ", analysis_group), "No analysis dataset available")
    list(results = result, manifest = tibble::tibble(moderator = "", png_file = normalize_slash(png), pdf_file = normalize_slash(pdf), status = "NOT_ESTIMABLE"))
  }

  loo <- tryCatch(
    run_leave_one_out(effects, summary_info, group_dir, analysis_group),
    error = function(e) {
      result <- tibble::tibble(
        omitted_study = "", studies_remaining = NA_integer_, status = "FAIL",
        effect = NA_real_, lower_ci = NA_real_, upper_ci = NA_real_, source = conditionMessage(e)
      )
      readr::write_csv(result, file.path(group_dir, "leave_one_out.csv"), na = "")
      png <- file.path(group_dir, "leave_one_out_plot.png")
      pdf <- file.path(group_dir, "leave_one_out_plot.pdf")
      write_placeholder_plot(png, pdf, paste0("Leave-one-out — ", analysis_group), conditionMessage(e))
      list(result = result, status = "FAIL", png = png, pdf = pdf)
    }
  )

  meta_status_values <- unique(meta_reg$results$status)
  meta_overall_status <- if (any(meta_status_values == "STANDARD_ELIGIBLE")) {
    "STANDARD_ELIGIBLE"
  } else if (any(meta_status_values == "EXPLORATORY_UNDERPOWERED")) {
    "EXPLORATORY_UNDERPOWERED"
  } else if (any(meta_status_values == "DESCRIPTIVE_ONLY")) {
    "DESCRIPTIVE_ONLY"
  } else if (any(meta_status_values == "FAIL")) {
    "FAIL"
  } else {
    "NOT_ESTIMABLE"
  }

  status <- tibble::tibble(
    analysis_group = analysis_group,
    canonical_k = canonical_k,
    reconstructed_k = reconstructed_k,
    effect_measure = effects$measure[[1]],
    effect_contract_status = contract_status,
    funnel_status = funnel$status,
    funnel_center_source = funnel$center_source,
    egger_status = egger$status[[1]],
    meta_regression_status = meta_overall_status,
    moderators_configured = paste(configured_moderators, collapse = ";"),
    moderators_usable = paste(moderator_registry$moderator[moderator_registry$usable], collapse = ";"),
    leave_one_out_status = loo$status,
    publication_bias_standard_min_k = publication_bias_min_k,
    meta_regression_standard_min_k = meta_regression_min_k,
    canonical_meta_analysis_modified = FALSE,
    message = if (contract_match) {
      "Exploratory diagnostics generated without modifying canonical R/27 results."
    } else {
      "Exploratory diagnostics generated using partial effects because allow_partial_effects=TRUE."
    }
  )
  readr::write_csv(status, file.path(group_dir, "diagnostic_status.csv"), na = "")
  summary_rows[[length(summary_rows) + 1L]] <- status

  workbook_payload[[paste0("Status_", substr(slugify(analysis_group), 1, 20))]] <- status
  workbook_payload[[paste0("Effects_", substr(slugify(analysis_group), 1, 19))]] <- effects
  workbook_payload[[paste0("Mods_", substr(slugify(analysis_group), 1, 22))]] <- moderator_registry
  workbook_payload[[paste0("Egger_", substr(slugify(analysis_group), 1, 21))]] <- egger
  workbook_payload[[paste0("MetaReg_", substr(slugify(analysis_group), 1, 19))]] <- meta_reg$results
  workbook_payload[[paste0("LOO_", substr(slugify(analysis_group), 1, 23))]] <- loo$result

  cat(
    "Canonical k: ", canonical_k, "\n",
    "Reconstructed k: ", reconstructed_k, "\n",
    "Effect contract: ", contract_status, "\n",
    "Effect source: ", paste(unique(effects$effect_source), collapse = ";"), "\n",
    "Effect derivation: ", paste(unique(effects$effect_derivation), collapse = ";"), "\n",
    "Funnel: ", funnel$status, "\n",
    "Egger: ", egger$status[[1]], "\n",
    "Meta-regression: ", meta_overall_status, "\n",
    "Leave-one-out: ", loo$status, "\n\n",
    sep = ""
  )
}


# ============================================================
# 13. Combined outputs
# ============================================================

combined_summary <- if (length(summary_rows) > 0L) dplyr::bind_rows(summary_rows) else tibble::tibble(
  analysis_group = character(), canonical_k = integer(), reconstructed_k = integer(),
  effect_contract_status = character(), funnel_status = character(), egger_status = character(),
  meta_regression_status = character(), leave_one_out_status = character(), message = character()
)
combined_contract <- if (length(contract_rows) > 0L) dplyr::bind_rows(contract_rows) else tibble::tibble(
  analysis_group = character(), canonical_k = integer(), reconstructed_k = integer(), contract_status = character()
)

readr::write_csv(combined_summary, combined_summary_file, na = "")
readr::write_csv(combined_contract, combined_contract_file, na = "")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Summary")
openxlsx::writeData(wb, "Summary", combined_summary)
openxlsx::addWorksheet(wb, "Effect_Contract")
openxlsx::writeData(wb, "Effect_Contract", combined_contract)
openxlsx::addWorksheet(wb, "R27_Execution")
openxlsx::writeData(wb, "R27_Execution", execution_summary_raw)
if (!is.null(analysis_manifest)) {
  openxlsx::addWorksheet(wb, "Input_Manifest")
  openxlsx::writeData(wb, "Input_Manifest", analysis_manifest)
}

used_sheet_names <- c("Summary", "Effect_Contract", "R27_Execution", if (!is.null(analysis_manifest)) "Input_Manifest" else character(0))
for (requested_name in names(workbook_payload)) {
  sheet_name <- substr(requested_name, 1, 31)
  base_name <- sheet_name
  suffix <- 1L
  while (sheet_name %in% used_sheet_names) {
    suffix <- suffix + 1L
    suffix_text <- paste0("_", suffix)
    sheet_name <- paste0(substr(base_name, 1, 31 - nchar(suffix_text)), suffix_text)
  }
  used_sheet_names <- c(used_sheet_names, sheet_name)
  openxlsx::addWorksheet(wb, sheet_name)
  openxlsx::writeData(wb, sheet_name, workbook_payload[[requested_name]])
}

openxlsx::saveWorkbook(wb, combined_workbook_file, overwrite = TRUE)


# ============================================================
# 14. Report + validation
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/28B — Universal Exploratory Meta-analysis Diagnostics",
  "",
  paste0("Run time: ", now_text()),
  paste0("Project root: ", normalize_slash(repo_root)),
  "",
  "ARCHITECTURE",
  "--------------------------------------------------",
  "Canonical statistical engine: R/27",
  "R/28B modifies canonical R/27 results: NO",
  "R/28B role: OPTIONAL exploratory diagnostics",
  "",
  "EFFECT CONTRACT",
  "--------------------------------------------------",
  "Preferred source: canonical R/27 study_effects_used.csv",
  "Fallback source: R/26 meta-analysis-ready dataset",
  "Supported reported measures: RR, OR, HR, IRR, ROM, MD, SMD, RD, generic effects",
  "Supported raw derivation: binary counts (RR/OR/RD), continuous mean/SD/N (MD/SMD), incidence-rate counts/person-time (IRR)",
  paste0("Partial effect sets permitted: ", allow_partial_effects),
  "",
  "ELIGIBILITY POLICY",
  "--------------------------------------------------",
  paste0("Publication-bias standard threshold k: ", publication_bias_min_k),
  paste0("Meta-regression standard threshold k: ", meta_regression_min_k),
  "Funnel: generated at k>=2; descriptive below standard threshold.",
  "Egger: not estimable at k<3; exploratory/underpowered below standard threshold.",
  "Meta-regression: k=2 numeric moderators produce descriptive weighted line only; k>=3 can be fitted; below standard threshold is underpowered.",
  "Leave-one-out: k=2 yields single-study descriptive rows; k>=3 performs exploratory random-effects refits.",
  "",
  "MODERATOR POLICY",
  "--------------------------------------------------",
  paste0("Configured moderators: ", if (length(configured_moderators) > 0L) paste(configured_moderators, collapse = "; ") else "none"),
  paste0("Automatic generic moderator discovery enabled: ", allow_auto_moderators),
  "Automatic candidates are restricted to generic interpretable fields (publication year, mean age, female proportion, follow-up, baseline risk).",
  "Sample size is not silently selected as a moderator.",
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0("Summary CSV: ", normalize_slash(combined_summary_file)),
  paste0("Effect contract CSV: ", normalize_slash(combined_contract_file)),
  paste0("Workbook: ", normalize_slash(combined_workbook_file)),
  paste0("Output root: ", normalize_slash(output_root)),
  "",
  "STATUS",
  "--------------------------------------------------",
  if (nrow(combined_summary) > 0L) {
    "PASS: R/28B completed its exploratory diagnostic audit. Individual diagnostics may appropriately be NOT_ESTIMABLE or CONTRACT_MISMATCH."
  } else {
    "FAIL: No canonical R/27 PASS group was processed."
  }
)
writeLines(report_lines, report_file)

required_outputs <- c(combined_summary_file, combined_contract_file, combined_workbook_file, report_file)
output_ok <- vapply(required_outputs, file_nonempty, logical(1))
if (!all(output_ok)) {
  stop("R/28B failed to create one or more required combined outputs: ",
       paste(basename(required_outputs[!output_ok]), collapse = ", "), call. = FALSE)
}

cat(
  "============================================================\n",
  "R/28B — Universal Exploratory Meta-analysis Diagnostics\n",
  "============================================================\n\n",
  sep = ""
)
print(combined_summary, n = Inf, width = Inf)
cat(
  "\nPASS: R/28B exploratory diagnostic audit completed.\n",
  "Canonical R/27 results modified: NO\n",
  "Summary: ", normalize_slash(combined_summary_file), "\n",
  "Effect contract: ", normalize_slash(combined_contract_file), "\n",
  "Workbook: ", normalize_slash(combined_workbook_file), "\n",
  "Report: ", normalize_slash(report_file), "\n",
  sep = ""
)
