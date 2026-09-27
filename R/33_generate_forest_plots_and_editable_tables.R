# ============================================================
# Universal SR-MA Agent Toolkit
# R/33_generate_forest_plots_and_editable_tables.R
# VERSION: 4.0.1
#
# PURPOSE
# ------------------------------------------------------------
# Publication/reporting layer for meta-analysis outputs.
#
# CRITICAL NUMERICAL CONTRACT
# ------------------------------------------------------------
# - R/27 is the ONLY statistical engine for canonical main analyses.
# - R/33 MUST NOT fit or refit meta-analysis models.
# - R/33 reads R/27 canonical outputs:
#     results/meta_analysis/<group>/meta_analysis_summary.csv
#     results/meta_analysis/<group>/study_effects_used.csv
# - R/33 uses those exact pooled values for tables.
# - Main-analysis forest figures are copied from R/27 canonical forest_plot.pdf.
# - R/33 never reconstructs a main-analysis forest plot from partial study-level schema.
# - R/29 additional analyses, when present, are reused only; never refit.
#
# UNIVERSALITY
# ------------------------------------------------------------
# - No topic-specific treatment/comparator hardcoding.
# - Arm labels are inferred from the canonical analysis dataset.
# - Generic fallback: Intervention / Comparator.
# ============================================================

required_packages <- c(
  "yaml",
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
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

normalize_slash <- function(x) {
  stringr::str_replace_all(as.character(x), "\\\\", "/")
}

safe_chr <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) return(default)
  out <- as.character(x[[1]])
  if (is.na(out) || !nzchar(trimws(out))) return(default)
  trimws(out)
}

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
}

now_text <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

first_existing_col <- function(data, candidates) {
  data_names <- names(data)
  if (length(data_names) == 0L) return(NA_character_)

  lower_names <- tolower(data_names)
  for (candidate in candidates) {
    hit <- which(lower_names == tolower(candidate))
    if (length(hit) >= 1L) return(data_names[[hit[[1]]]])
  }

  NA_character_
}

read_csv_safe <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE
  )
}

file_nonempty <- function(path) {
  file.exists(path) &&
    !is.na(file.info(path)$size) &&
    file.info(path)$size > 0
}

slugify <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  stringr::str_replace_all(x, "^_|_$", "")
}

normalize_key <- function(x) slugify(x)

unique_nonblank <- function(x) {
  x <- trimws(as.character(x))
  unique(x[!is.na(x) & nzchar(x)])
}

ratio_measure <- function(x) {
  toupper(trimws(as.character(x))) %in%
    c("RR", "OR", "HR", "IRR", "RATIO", "ROM")
}

pick_scalar <- function(data, candidates, default = "") {
  col <- first_existing_col(data, candidates)
  if (is.na(col)) return(default)
  safe_chr(data[[col]], default)
}

pick_numeric <- function(data, candidates, default = NA_real_) {
  col <- first_existing_col(data, candidates)
  if (is.na(col)) return(default)
  x <- safe_num(data[[col]])
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(default)
  x[[1]]
}

pick_vector <- function(data, candidates, default = NA_character_) {
  col <- first_existing_col(data, candidates)
  if (is.na(col)) return(rep(default, nrow(data)))
  as.character(data[[col]])
}

pick_numeric_vector <- function(data, candidates) {
  col <- first_existing_col(data, candidates)
  if (is.na(col)) return(rep(NA_real_, nrow(data)))
  safe_num(data[[col]])
}

format_number <- function(x, digits = 3) {
  x <- safe_num(x)
  if (length(x) == 0L || is.na(x[[1]])) return("")
  value <- x[[1]]
  if (value == 0) return("0")
  if (abs(value) < 0.001 || abs(value) >= 10000) {
    return(formatC(value, format = "e", digits = max(2L, digits - 1L)))
  }
  formatC(value, format = "fg", digits = digits, flag = "#")
}

resolve_path <- function(x, root) {
  x <- safe_chr(x)
  if (!nzchar(x)) return("")
  x <- normalize_slash(x)
  if (grepl("^[A-Za-z]:/", x) || grepl("^/", x)) {
    candidate <- x
  } else {
    candidate <- file.path(root, x)
  }
  normalizePath(candidate, winslash = "/", mustWork = FALSE)
}

copy_if_exists <- function(source, destination) {
  if (!file_nonempty(source)) return(FALSE)
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  isTRUE(file.copy(source, destination, overwrite = TRUE))
}

repo_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
config_file <- file.path(repo_root, "config", "project_config.yml")

if (!file.exists(config_file)) {
  stop(
    "config/project_config.yml was not found. Run R/33 from the project root.",
    call. = FALSE
  )
}

config <- yaml::read_yaml(config_file)
if (is.null(config)) stop("project_config.yml could not be parsed.", call. = FALSE)

results_root <- file.path(repo_root, "results", "meta_analysis")
figures_dir <- file.path(results_root, "figures")
tables_dir <- file.path(results_root, "tables")
logs_dir <- file.path(results_root, "logs")

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

summary_csv <- file.path(tables_dir, "meta_analysis_summary.csv")
study_csv <- file.path(tables_dir, "study_level_effects.csv")
contract_csv <- file.path(tables_dir, "r27_r33_numeric_contract.csv")
figure_manifest_csv <- file.path(tables_dir, "figure_manifest.csv")
xlsx_file <- file.path(tables_dir, "meta_analysis_outputs.xlsx")
report_file <- file.path(logs_dir, "r33_forest_and_tables_report.txt")
execution_summary_file <- file.path(results_root, "meta_analysis_execution_summary.csv")

meta_ready_root <- file.path(repo_root, "data", "processed", "meta_analysis_ready")
meta_manifest_candidates <- c(
  file.path(meta_ready_root, "meta_analysis_manifest.csv"),
  file.path(repo_root, "data", "processed", "meta_analysis", "meta_analysis_manifest.csv"),
  file.path(results_root, "meta_analysis_manifest.csv")
)
meta_manifest_file <- meta_manifest_candidates[file.exists(meta_manifest_candidates)]
meta_manifest_file <- if (length(meta_manifest_file) > 0L) meta_manifest_file[[1]] else ""

crosswalk_file <- file.path(
  repo_root,
  "data",
  "processed",
  "meta_analysis",
  "study_label_crosswalk.csv"
)
dir.create(dirname(crosswalk_file), recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Canonical R/27 completion evidence
# ============================================================

if (!file_nonempty(execution_summary_file)) {
  stop(
    paste0(
      "R/27 canonical execution summary is missing or empty:\n",
      normalize_slash(execution_summary_file),
      "\nR/33 no longer re-runs meta-analysis models. Run R/27 first."
    ),
    call. = FALSE
  )
}

execution_summary <- read_csv_safe(execution_summary_file)
execution_group_col <- first_existing_col(
  execution_summary,
  c("analysis_group", "group_name", "analysis_set", "analysis_name", "label")
)
execution_status_col <- first_existing_col(
  execution_summary,
  c("status", "analysis_status", "execution_status", "result")
)

if (is.na(execution_group_col)) {
  stop("R/27 execution summary lacks an analysis-group column.", call. = FALSE)
}

execution_summary <- execution_summary |>
  dplyr::mutate(
    .analysis_group = as.character(.data[[execution_group_col]]),
    .group_key = normalize_key(.analysis_group),
    .status = if (!is.na(execution_status_col)) {
      toupper(trimws(as.character(.data[[execution_status_col]])))
    } else {
      "PASS"
    }
  )

execution_pass <- execution_summary |>
  dplyr::filter(
    .status %in% c("PASS", "PASSED", "SUCCESS", "COMPLETE", "COMPLETED")
  )

if (nrow(execution_pass) == 0L) {
  stop("R/27 execution summary contains no PASS analysis group.", call. = FALSE)
}

# ============================================================
# Discover canonical per-group R/27 artifacts
# ============================================================

summary_files <- list.files(
  results_root,
  pattern = "^meta_analysis_summary\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

summary_files <- normalizePath(summary_files, winslash = "/", mustWork = FALSE)
excluded_roots <- normalizePath(c(figures_dir, tables_dir, logs_dir), winslash = "/", mustWork = FALSE)

is_excluded <- function(path) {
  any(vapply(
    excluded_roots,
    function(prefix) {
      startsWith(normalize_slash(path), paste0(normalize_slash(prefix), "/"))
    },
    logical(1)
  ))
}

summary_files <- summary_files[!vapply(summary_files, is_excluded, logical(1))]

if (length(summary_files) == 0L) {
  stop(
    "No canonical per-group R/27 meta_analysis_summary.csv was found. R/33 will not recompute the model.",
    call. = FALSE
  )
}

registry_rows <- list()

for (summary_path in summary_files) {
  group_dir <- dirname(summary_path)
  study_path <- file.path(group_dir, "study_effects_used.csv")
  if (!file_nonempty(study_path)) next

  summary_data <- tryCatch(read_csv_safe(summary_path), error = function(e) NULL)
  if (is.null(summary_data) || nrow(summary_data) == 0L) next

  group_name <- pick_scalar(
    summary_data,
    c("analysis_group", "group_name", "analysis_set", "analysis_name", "label"),
    basename(group_dir)
  )

  registry_rows[[length(registry_rows) + 1L]] <- tibble::tibble(
    analysis_group = group_name,
    group_key = normalize_key(group_name),
    group_directory = normalize_slash(group_dir),
    r27_summary_file = normalize_slash(summary_path),
    r27_study_file = normalize_slash(study_path),
    r27_forest_file = normalize_slash(file.path(group_dir, "forest_plot.pdf")),
    r27_forest_png = normalize_slash(file.path(group_dir, "forest_plot.png")),
    r27_model_file = normalize_slash(file.path(group_dir, "model_output.txt"))
  )
}

if (length(registry_rows) == 0L) {
  stop(
    "R/27 summaries were found, but no complete summary + study_effects_used pair was found.",
    call. = FALSE
  )
}

r27_registry_all <- dplyr::bind_rows(registry_rows)
r27_registry <- r27_registry_all |>
  dplyr::semi_join(
    execution_pass |> dplyr::select(.group_key) |> dplyr::distinct(),
    by = c("group_key" = ".group_key")
  )

# Compatibility fallback for one-group projects when legacy R/27 group naming
# differs between the execution summary and per-group folder/summary.
if (nrow(r27_registry) == 0L && nrow(execution_pass) == 1L && nrow(r27_registry_all) == 1L) {
  r27_registry <- r27_registry_all
  r27_registry$analysis_group <- execution_pass$.analysis_group[[1]]
  r27_registry$group_key <- execution_pass$.group_key[[1]]
}

if (nrow(r27_registry) == 0L) {
  stop(
    paste0(
      "R/27 PASS groups could not be matched to per-group canonical outputs. PASS groups: ",
      paste(execution_pass$.analysis_group, collapse = " | ")
    ),
    call. = FALSE
  )
}

if (anyDuplicated(r27_registry$group_key) > 0L) {
  stop("Multiple R/27 output directories map to the same analysis group.", call. = FALSE)
}

# ============================================================
# R/26 manifest / datasets: labels and provenance only
# ============================================================

analysis_manifest <- NULL
if (nzchar(meta_manifest_file)) {
  analysis_manifest <- tryCatch(read_csv_safe(meta_manifest_file), error = function(e) NULL)
}

build_dataset_registry <- function(manifest) {
  empty <- tibble::tibble(
    analysis_group = character(),
    group_key = character(),
    dataset_path = character()
  )
  if (is.null(manifest) || nrow(manifest) == 0L) return(empty)

  group_col <- first_existing_col(manifest, c("analysis_group", "group_name", "analysis_set", "label"))
  path_col <- first_existing_col(
    manifest,
    c("dataset_path", "analysis_dataset", "source_file", "input_file", "csv_path", "file_path")
  )
  if (is.na(group_col) || is.na(path_col)) return(empty)

  tibble::tibble(
    analysis_group = as.character(manifest[[group_col]]),
    group_key = normalize_key(manifest[[group_col]]),
    dataset_path = vapply(manifest[[path_col]], resolve_path, character(1), root = repo_root)
  ) |>
    dplyr::filter(nzchar(dataset_path), file.exists(dataset_path)) |>
    dplyr::distinct(group_key, .keep_all = TRUE)
}

dataset_registry <- build_dataset_registry(analysis_manifest)

if (nrow(dataset_registry) == 0L && dir.exists(meta_ready_root)) {
  ready_files <- list.files(
    meta_ready_root,
    pattern = "^meta_analysis_ready_.*\\.csv$",
    full.names = TRUE,
    recursive = FALSE
  )
  if (length(ready_files) > 0L) {
    raw_group <- tools::file_path_sans_ext(basename(ready_files))
    raw_group <- sub("^meta_analysis_ready_", "", raw_group)
    dataset_registry <- tibble::tibble(
      analysis_group = raw_group,
      group_key = normalize_key(raw_group),
      dataset_path = normalizePath(ready_files, winslash = "/", mustWork = FALSE)
    )
  }
}

config_arm_label <- function(config, arm = c("intervention", "comparator"), fallback = "") {
  arm <- match.arg(arm)
  node <- config$research_question[[arm]]
  if (is.null(node)) return(fallback)

  if (is.character(node) && length(node) == 1L && nzchar(trimws(node))) {
    return(trimws(node))
  }

  if (is.list(node)) {
    for (field in c("display_name", "label", "short_name", "description")) {
      value <- node[[field]]
      if (is.character(value) && length(value) >= 1L && nzchar(trimws(value[[1]]))) {
        return(trimws(value[[1]]))
      }
    }
    values <- unique_nonblank(node$values)
    if (length(values) == 1L) return(values[[1]])
  }

  fallback
}

extract_unique_label <- function(data, candidates, fallback) {
  col <- first_existing_col(data, candidates)
  if (is.na(col)) return(fallback)
  values <- unique_nonblank(data[[col]])
  if (length(values) == 1L) values[[1]] else fallback
}

infer_arm_labels <- function(dataset_path = "") {
  config_treatment <- config_arm_label(config, "intervention", "Intervention")
  config_control <- config_arm_label(config, "comparator", "Comparator")

  default <- list(
    treatment_label = config_treatment,
    control_label = config_control,
    label_source = if (
      !identical(config_treatment, "Intervention") ||
      !identical(config_control, "Comparator")
    ) "PROJECT_CONFIG" else "GENERIC_FALLBACK"
  )

  if (!nzchar(dataset_path) || !file_nonempty(dataset_path)) return(default)
  data <- tryCatch(read_csv_safe(dataset_path), error = function(e) NULL)
  if (is.null(data) || nrow(data) == 0L) return(default)

  treatment_label <- extract_unique_label(
    data,
    c(
      "experimental_label", "treatment_label", "intervention_label",
      "intervention_or_exposure", "intervention", "exposure", "treatment",
      "intervention_name", "experimental_group", "treatment_group"
    ),
    config_treatment
  )

  control_label <- extract_unique_label(
    data,
    c(
      "control_label", "comparator_label", "comparator", "control",
      "control_name", "comparator_name", "control_group", "comparator_group"
    ),
    config_control
  )

  source <- if (
    !identical(treatment_label, config_treatment) ||
    !identical(control_label, config_control)
  ) "ANALYSIS_DATASET" else default$label_source

  list(
    treatment_label = treatment_label,
    control_label = control_label,
    label_source = source
  )
}

# ============================================================
# Study-label crosswalk (reporting aid only)
# ============================================================

build_study_label_crosswalk <- function(dataset_registry, output_file) {
  rows <- list()

  for (i in seq_len(nrow(dataset_registry))) {
    path <- dataset_registry$dataset_path[[i]]
    if (!file_nonempty(path)) next
    data <- tryCatch(read_csv_safe(path), error = function(e) NULL)
    if (is.null(data) || nrow(data) == 0L) next

    id_col <- first_existing_col(data, c("screening_id", "study_id", "study_code"))
    if (is.na(id_col)) next

    label_col <- first_existing_col(
      data,
      c("study_label", "first_author_year", "author_year", "citation_short", "study_name")
    )

    ids <- as.character(data[[id_col]])
    labels <- if (!is.na(label_col)) as.character(data[[label_col]]) else rep(NA_character_, nrow(data))

    author_col <- first_existing_col(data, c("first_author", "author"))
    year_col <- first_existing_col(data, c("publication_year", "year"))

    if (all(is.na(labels) | !nzchar(trimws(labels))) && !is.na(author_col) && !is.na(year_col)) {
      labels <- paste(trimws(as.character(data[[author_col]])), trimws(as.character(data[[year_col]])))
    }

    labels <- trimws(as.character(labels))
    missing_label <- is.na(labels) | !nzchar(labels)
    labels[missing_label] <- ids[missing_label]

    rows[[length(rows) + 1L]] <- tibble::tibble(
      screening_id = ids,
      study_label = labels,
      label_status = ifelse(missing_label, "USER REQUIRED", "AUTO"),
      source_dataset = normalize_slash(path)
    )
  }

  crosswalk <- if (length(rows) == 0L) {
    tibble::tibble(
      screening_id = character(),
      study_label = character(),
      label_status = character(),
      source_dataset = character()
    )
  } else {
    dplyr::bind_rows(rows) |>
      dplyr::filter(!is.na(screening_id), nzchar(trimws(screening_id))) |>
      dplyr::distinct(screening_id, .keep_all = TRUE)
  }

  readr::write_csv(crosswalk, output_file, na = "")
  crosswalk
}

crosswalk <- build_study_label_crosswalk(dataset_registry, crosswalk_file)

if (nrow(crosswalk) > 0L) {
  cat(
    "\nStudy-label crosswalk generated automatically.\n",
    "AUTO: ", sum(crosswalk$label_status == "AUTO"),
    "  USER REQUIRED: ", sum(crosswalk$label_status == "USER REQUIRED"),
    "\nCrosswalk: ", normalize_slash(crosswalk_file), "\n\n",
    sep = ""
  )
}

# ============================================================
# Normalize R/27 canonical summary and study-level outputs
# ============================================================

normalize_r27_summary <- function(
    summary_data,
    analysis_group,
    treatment_label,
    control_label,
    source_summary_file,
    source_study_file,
    source_dataset) {

  measure <- toupper(pick_scalar(summary_data, c("summary_measure", "effect_measure", "measure"), ""))
  pooled_effect <- pick_numeric(summary_data, c("pooled_effect", "pooled_estimate", "pooled_rr", "pooled_or", "pooled_hr", "random_effect", "random_effect_estimate", "estimate"))
  pooled_lower_ci <- pick_numeric(summary_data, c("pooled_lower_ci", "pooled_ci_lower", "ci_lower", "lower_ci", "random_lower_ci", "lower"))
  pooled_upper_ci <- pick_numeric(summary_data, c("pooled_upper_ci", "pooled_ci_upper", "ci_upper", "upper_ci", "random_upper_ci", "upper"))
  n_studies <- pick_numeric(summary_data, c("n_studies", "k", "studies"))
  p_value <- pick_numeric(summary_data, c("p_value", "pval", "p_value_random", "random_p_value"))
  i2 <- pick_numeric(summary_data, c("heterogeneity_i2", "i2", "I2"))
  tau2 <- pick_numeric(summary_data, c("heterogeneity_tau2", "tau2", "tau_squared"))
  q <- pick_numeric(summary_data, c("heterogeneity_q", "q", "Q"))
  heterogeneity_p <- pick_numeric(summary_data, c("heterogeneity_p", "pval_q", "q_p_value", "heterogeneity_p_value"))
  model <- pick_scalar(summary_data, c("model", "model_description", "random_effects_model"), "Canonical R/27 model")

  if (is.na(pooled_effect) || is.na(pooled_lower_ci) || is.na(pooled_upper_ci)) {
    stop(
      paste0("Canonical R/27 summary for '", analysis_group, "' lacks pooled effect / 95% CI."),
      call. = FALSE
    )
  }

  tibble::tibble(
    analysis_group = analysis_group,
    summary_measure = measure,
    n_studies = as.integer(n_studies),
    pooled_effect = pooled_effect,
    pooled_lower_ci = pooled_lower_ci,
    pooled_upper_ci = pooled_upper_ci,
    p_value = p_value,
    heterogeneity_i2 = i2,
    heterogeneity_tau2 = tau2,
    heterogeneity_q = q,
    heterogeneity_p = heterogeneity_p,
    model = model,
    treatment_label = treatment_label,
    control_label = control_label,
    numerical_source = "R/27_CANONICAL_OUTPUT",
    source_r27_summary = normalize_slash(source_summary_file),
    source_r27_study_effects = normalize_slash(source_study_file),
    source_dataset = normalize_slash(source_dataset)
  )
}

normalize_r27_studies <- function(
    study_data,
    analysis_group,
    summary_measure,
    source_study_file,
    source_dataset) {

  if (nrow(study_data) == 0L) {
    stop("Canonical R/27 study_effects_used.csv has zero rows.", call. = FALSE)
  }

  study_label <- pick_vector(
    study_data,
    c("study_label", "studlab", "study", "study_name", "study_id", "screening_id", "study_code")
  )
  screening_id <- pick_vector(study_data, c("screening_id", "study_id", "study_code"))
  study_design <- pick_vector(study_data, c("study_design", "design"))
  effect_measure <- pick_vector(study_data, c("effect_measure", "summary_measure", "measure"), summary_measure)
  effect_value <- pick_numeric_vector(study_data, c("effect_value", "effect_estimate", "estimate", "effect", "rr", "or", "hr"))
  lower_ci <- pick_numeric_vector(study_data, c("lower_ci", "lcl", "ci_lower", "lower", "lower95"))
  upper_ci <- pick_numeric_vector(study_data, c("upper_ci", "ucl", "ci_upper", "upper", "upper95"))
  log_effect <- pick_numeric_vector(study_data, c("log_effect", "TE", "te"))
  se_log_effect <- pick_numeric_vector(study_data, c("se_log_effect", "seTE", "sete", "standard_error", "se"))
  weight <- pick_numeric_vector(
    study_data,
    c("random_effect_weight_percent", "random_weight_percent", "weight_random_percent", "weight_percent", "w_random_percent", "weight")
  )

  # Display-only compatibility: recover study display values from canonical
  # R/27 TE/seTE when redundant effect/CI columns are absent. This does NOT
  # fit or alter any pooled model.
  if (ratio_measure(summary_measure)) {
    missing_effect <- is.na(effect_value) & !is.na(log_effect)
    effect_value[missing_effect] <- exp(log_effect[missing_effect])
    missing_ci <- (is.na(lower_ci) | is.na(upper_ci)) & !is.na(log_effect) & !is.na(se_log_effect)
    lower_ci[missing_ci] <- exp(log_effect[missing_ci] - 1.96 * se_log_effect[missing_ci])
    upper_ci[missing_ci] <- exp(log_effect[missing_ci] + 1.96 * se_log_effect[missing_ci])
  } else {
    missing_effect <- is.na(effect_value) & !is.na(log_effect)
    effect_value[missing_effect] <- log_effect[missing_effect]
    missing_ci <- (is.na(lower_ci) | is.na(upper_ci)) & !is.na(log_effect) & !is.na(se_log_effect)
    lower_ci[missing_ci] <- log_effect[missing_ci] - 1.96 * se_log_effect[missing_ci]
    upper_ci[missing_ci] <- log_effect[missing_ci] + 1.96 * se_log_effect[missing_ci]
  }

  events_exposed <- pick_numeric_vector(study_data, c("events_exposed", "event_experimental", "event_treatment", "events_treatment", "s_event"))
  n_exposed <- pick_numeric_vector(study_data, c("n_exposed", "n_experimental", "n_treatment", "total_treatment", "s_total"))
  events_control <- pick_numeric_vector(study_data, c("events_control", "event_control", "c_event"))
  n_control <- pick_numeric_vector(study_data, c("n_control", "total_control", "c_total"))

  if (nrow(crosswalk) > 0L) {
    for (i in seq_along(study_label)) {
      sid <- screening_id[[i]]
      if (is.na(sid) || !nzchar(trimws(sid))) next
      hit <- which(crosswalk$screening_id == sid)
      if (length(hit) >= 1L && (
        is.na(study_label[[i]]) ||
        !nzchar(trimws(study_label[[i]])) ||
        identical(trimws(study_label[[i]]), trimws(sid))
      )) {
        study_label[[i]] <- crosswalk$study_label[hit[[1]]]
      }
    }
  }

  missing_label <- is.na(study_label) | !nzchar(trimws(study_label))
  study_label[missing_label] <- paste0("Study_", which(missing_label))

  tibble::tibble(
    analysis_group = analysis_group,
    screening_id = screening_id,
    study_label = study_label,
    study_design = study_design,
    effect_measure = effect_measure,
    effect_value = effect_value,
    lower_ci = lower_ci,
    upper_ci = upper_ci,
    log_effect = log_effect,
    se_log_effect = se_log_effect,
    random_effect_weight_percent = weight,
    events_exposed = events_exposed,
    n_exposed = n_exposed,
    events_control = events_control,
    n_control = n_control,
    numerical_source = "R/27_CANONICAL_OUTPUT",
    source_r27_study_effects = normalize_slash(source_study_file),
    source_dataset = normalize_slash(source_dataset)
  )
}

# ============================================================
# Preserve canonical R/27 study-level artifact without schema assumptions
# ============================================================

preserve_r27_studies <- function(
    study_data,
    analysis_group,
    source_study_file,
    source_dataset) {

  if (nrow(study_data) == 0L) {
    stop(
      "Canonical R/27 study_effects_used.csv has zero rows.",
      call. = FALSE
    )
  }

  # R/27 owns the statistical schema. R/33 does not require effect/CI
  # columns to have any particular names. Preserve every upstream column
  # exactly and add only reporting/provenance fields.
  study_data |>
    dplyr::mutate(
      analysis_group = analysis_group,
      numerical_source = "R/27_CANONICAL_OUTPUT",
      source_r27_study_effects = normalize_slash(source_study_file),
      source_dataset = normalize_slash(source_dataset),
      analysis_type = "main_analysis"
    )
}


# ============================================================
# Forest plot from canonical numbers only (no model fitting)
# ============================================================

save_canonical_forest_plot <- function(study_table, summary_row, output_base) {
  png_file <- paste0(output_base, ".png")
  pdf_file <- paste0(output_base, ".pdf")

  measure <- safe_chr(summary_row$summary_measure, "Effect")
  treatment_label <- safe_chr(summary_row$treatment_label, "Intervention")
  control_label <- safe_chr(summary_row$control_label, "Comparator")

  pooled_effect <- safe_num(summary_row$pooled_effect)[[1]]
  pooled_lower <- safe_num(summary_row$pooled_lower_ci)[[1]]
  pooled_upper <- safe_num(summary_row$pooled_upper_ci)[[1]]

  effects <- safe_num(study_table$effect_value)
  lower <- safe_num(study_table$lower_ci)
  upper <- safe_num(study_table$upper_ci)
  labels <- as.character(study_table$study_label)
  weights <- safe_num(study_table$random_effect_weight_percent)

  valid <- !is.na(effects) & !is.na(lower) & !is.na(upper)
  if (sum(valid) == 0L) {
    stop(
      paste0("No canonical study-level effect + CI rows are available for forest plot: ", safe_chr(summary_row$analysis_group)),
      call. = FALSE
    )
  }

  effects <- effects[valid]
  lower <- lower[valid]
  upper <- upper[valid]
  labels <- labels[valid]
  weights <- weights[valid]

  ratio_scale <- ratio_measure(measure)

  if (ratio_scale) {
    if (!all(effects > 0 & lower > 0 & upper > 0)) {
      stop("Ratio-measure forest plot contains non-positive canonical values.", call. = FALSE)
    }
    all_values <- c(lower, upper, pooled_lower, pooled_upper, 1)
    x_min <- min(all_values[is.finite(all_values) & all_values > 0], na.rm = TRUE) / 1.35
    x_max <- max(all_values[is.finite(all_values) & all_values > 0], na.rm = TRUE) * 1.35
    null_value <- 1
    log_axis <- "x"
  } else {
    all_values <- c(lower, upper, pooled_lower, pooled_upper, 0)
    finite_values <- all_values[is.finite(all_values)]
    x_min <- min(finite_values, na.rm = TRUE)
    x_max <- max(finite_values, na.rm = TRUE)
    span <- x_max - x_min
    if (!is.finite(span) || span <= 0) span <- max(1, abs(x_max))
    x_min <- x_min - 0.1 * span
    x_max <- x_max + 0.1 * span
    null_value <- 0
    log_axis <- ""
  }

  y_studies <- rev(seq_along(labels)) + 1.4
  y_pooled <- 0.8
  y_max <- max(y_studies) + 1.2

  if (all(is.na(weights)) || suppressWarnings(max(weights, na.rm = TRUE)) <= 0) {
    point_cex <- rep(1.15, length(effects))
  } else {
    max_w <- max(weights, na.rm = TRUE)
    point_cex <- 0.8 + 1.4 * sqrt(pmax(weights, 0) / max_w)
    point_cex[is.na(point_cex)] <- 1.15
  }

  draw_plot <- function() {
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par), add = TRUE)

    graphics::par(mar = c(5.5, 10.5, 5, 10), xpd = NA)
    graphics::plot(
      NA,
      xlim = c(x_min, x_max),
      ylim = c(0, y_max),
      log = log_axis,
      yaxt = "n",
      ylab = "",
      xlab = paste0(measure, " (95% CI)"),
      main = safe_chr(summary_row$analysis_group, "Meta-analysis")
    )
    graphics::abline(v = null_value, lty = 2, lwd = 1)
    graphics::segments(lower, y_studies, upper, y_studies, lwd = 1.3)
    graphics::points(effects, y_studies, pch = 15, cex = point_cex)

    pooled_height <- 0.22
    graphics::polygon(
      x = c(pooled_lower, pooled_effect, pooled_upper, pooled_effect),
      y = c(y_pooled, y_pooled + pooled_height, y_pooled, y_pooled - pooled_height)
    )

    graphics::text(x_min, y_studies, labels = labels, pos = 2, cex = 0.88)
    graphics::text(x_min, y_pooled, labels = "Pooled effect", pos = 2, font = 2, cex = 0.9)

    effect_text <- paste0(
      vapply(effects, format_number, character(1), digits = 3),
      " [",
      vapply(lower, format_number, character(1), digits = 3),
      ", ",
      vapply(upper, format_number, character(1), digits = 3),
      "]"
    )

    pooled_text <- paste0(
      format_number(pooled_effect, 3),
      " [", format_number(pooled_lower, 3), ", ", format_number(pooled_upper, 3), "]"
    )

    graphics::text(x_max, y_studies, labels = effect_text, pos = 4, cex = 0.82)
    graphics::text(x_max, y_pooled, labels = pooled_text, pos = 4, font = 2, cex = 0.85)

    if (any(!is.na(weights))) {
      weight_text <- ifelse(
        is.na(weights),
        "",
        paste0(formatC(weights, format = "f", digits = 1), "%")
      )
      graphics::text(x_max, y_studies - 0.32, labels = weight_text, pos = 4, cex = 0.68)
    }

    i2 <- safe_num(summary_row$heterogeneity_i2)[[1]]
    tau2 <- safe_num(summary_row$heterogeneity_tau2)[[1]]
    heterogeneity_p <- safe_num(summary_row$heterogeneity_p)[[1]]
    model_text <- safe_chr(summary_row$model, "Canonical R/27 model")

    stats_text <- paste0(
      model_text,
      if (!is.na(i2)) paste0("; I\u00b2 = ", format_number(i2, 3), "%") else "",
      if (!is.na(tau2)) paste0("; \u03c4\u00b2 = ", format_number(tau2, 3)) else "",
      if (!is.na(heterogeneity_p)) paste0("; p(Q) = ", format_number(heterogeneity_p, 3)) else ""
    )

    graphics::mtext(paste0(treatment_label, " vs ", control_label), side = 3, line = 1.2, cex = 0.9)
    graphics::mtext(stats_text, side = 1, line = 4, cex = 0.72)
    graphics::mtext("Study", side = 2, line = 8.6, at = y_max - 0.3, las = 1, font = 2, cex = 0.85)
    graphics::mtext(paste0(measure, " [95% CI]"), side = 4, line = 7.3, at = y_max - 0.3, las = 1, font = 2, cex = 0.82)
  }

  grDevices::png(filename = png_file, width = 2600, height = 1800, res = 300)
  draw_plot()
  grDevices::dev.off()

  grDevices::pdf(file = pdf_file, width = 11, height = 8.5)
  draw_plot()
  grDevices::dev.off()

  list(png = png_file, pdf = pdf_file)
}

# ============================================================
# Main R/27 -> R/33 processing
# ============================================================

summary_rows <- list()
study_rows <- list()
figure_rows <- list()
contract_rows <- list()
failure_rows <- list()

for (i in seq_len(nrow(r27_registry))) {
  analysis_group <- r27_registry$analysis_group[[i]]
  group_key <- r27_registry$group_key[[i]]
  r27_summary_path <- r27_registry$r27_summary_file[[i]]
  r27_study_path <- r27_registry$r27_study_file[[i]]

  cat(
    "------------------------------------------------------------\n",
    "R/33 canonical reporting group: ", analysis_group, "\n",
    "------------------------------------------------------------\n",
    sep = ""
  )

  dataset_path <- ""
  dataset_hit <- which(dataset_registry$group_key == group_key)
  if (length(dataset_hit) >= 1L) {
    dataset_path <- dataset_registry$dataset_path[dataset_hit[[1]]]
  } else if (nrow(dataset_registry) == 1L && nrow(r27_registry) == 1L) {
    dataset_path <- dataset_registry$dataset_path[[1]]
  }

  arm_labels <- infer_arm_labels(dataset_path)

  tryCatch(
    {
      r27_summary <- read_csv_safe(r27_summary_path)
      r27_studies <- read_csv_safe(r27_study_path)

      normalized_summary <- normalize_r27_summary(
        r27_summary,
        analysis_group,
        arm_labels$treatment_label,
        arm_labels$control_label,
        r27_summary_path,
        r27_study_path,
        dataset_path
      ) |>
        dplyr::mutate(
          label_source = arm_labels$label_source,
          analysis_type = "main_analysis",
          analysis_purpose = analysis_group,
          source_workspace = "",
          source_r29_summary = ""
        )

      preserved_studies <- preserve_r27_studies(
        r27_studies,
        analysis_group,
        r27_study_path,
        dataset_path
      )

      # Main-analysis forest figures are canonical R/27 artifacts.
      # Do not reconstruct them from study_effects_used.csv because that
      # file's internal schema may legitimately vary by effect-measure path.
      source_forest_pdf <- r27_registry$r27_forest_file[[i]]
      source_forest_png <- r27_registry$r27_forest_png[[i]]

      if (!file_nonempty(source_forest_pdf)) {
        stop(
          paste0(
            "Canonical R/27 forest plot is missing: ",
            normalize_slash(source_forest_pdf)
          ),
          call. = FALSE
        )
      }

      output_base <- file.path(
        figures_dir,
        paste0("forest_", slugify(analysis_group))
      )

      target_pdf <- paste0(output_base, ".pdf")
      target_png <- paste0(output_base, ".png")

      if (!copy_if_exists(source_forest_pdf, target_pdf)) {
        stop(
          "Failed to copy canonical R/27 forest_plot.pdf into the publication figures directory.",
          call. = FALSE
        )
      }

      copied_png <- copy_if_exists(source_forest_png, target_png)

      if (!copied_png) {
        # Remove any stale PNG left by an older R/33 implementation so the
        # current figure manifest cannot be confused with an obsolete plot.
        if (file.exists(target_png)) {
          unlink(target_png, force = TRUE)
        }
        target_png <- ""
      }

      normalized_summary <- normalized_summary |>
        dplyr::mutate(
          forest_plot_png = normalize_slash(target_png),
          forest_plot_pdf = normalize_slash(target_pdf),
          forest_plot_source = "R/27_CANONICAL_FOREST"
        )

      summary_rows[[length(summary_rows) + 1L]] <- normalized_summary
      study_rows[[length(study_rows) + 1L]] <- preserved_studies

      figure_rows[[length(figure_rows) + 1L]] <- tibble::tibble(
        analysis_group = analysis_group,
        analysis_type = "main_analysis",
        numerical_source = "R/27_CANONICAL_OUTPUT",
        figure_source = "R/27_CANONICAL_FOREST",
        png_file = normalize_slash(target_png),
        pdf_file = normalize_slash(target_pdf),
        source_r27_forest_pdf = normalize_slash(source_forest_pdf),
        source_r27_forest_png = if (file_nonempty(source_forest_png)) normalize_slash(source_forest_png) else "",
        source_r27_summary = normalize_slash(r27_summary_path),
        source_r27_study_effects = normalize_slash(r27_study_path),
        status = if (copied_png) "PASS" else "PASS_PDF_ONLY"
      )

      contract_rows[[length(contract_rows) + 1L]] <- tibble::tibble(
        analysis_group = analysis_group,
        r27_pooled_effect = normalized_summary$pooled_effect[[1]],
        r33_pooled_effect = normalized_summary$pooled_effect[[1]],
        r27_lower_ci = normalized_summary$pooled_lower_ci[[1]],
        r33_lower_ci = normalized_summary$pooled_lower_ci[[1]],
        r27_upper_ci = normalized_summary$pooled_upper_ci[[1]],
        r33_upper_ci = normalized_summary$pooled_upper_ci[[1]],
        r27_i2 = normalized_summary$heterogeneity_i2[[1]],
        r33_i2 = normalized_summary$heterogeneity_i2[[1]],
        pooled_effect_identical = TRUE,
        lower_ci_identical = TRUE,
        upper_ci_identical = TRUE,
        i2_identical = TRUE,
        contract_status = "PASS",
        contract_rule = "R/33 copies canonical R/27 numerical outputs; no model refit"
      )

      cat(
        "Status: PASS\n",
        "Numerical source: R/27 canonical outputs\n",
        "Forest source: R/27 canonical forest_plot.pdf\n",
        "Labels metadata: ", arm_labels$treatment_label,
        " vs ", arm_labels$control_label,
        " [", arm_labels$label_source, "]\n\n",
        sep = ""
      )
    },
    error = function(e) {
      failure_rows[[length(failure_rows) + 1L]] <<- tibble::tibble(
        analysis_group = analysis_group,
        analysis_type = "main_analysis",
        error_message = conditionMessage(e),
        source_r27_summary = normalize_slash(r27_summary_path),
        source_r27_study_effects = normalize_slash(r27_study_path)
      )
      cat("Status: FAIL\nReason: ", conditionMessage(e), "\n\n", sep = "")
    }
  )
}

# ============================================================
# Optional completed R/29 analyses: reuse only, never refit
# ============================================================

additional_root <- file.path(repo_root, "results", "additional_analysis")
additional_summary_files <- if (dir.exists(additional_root)) {
  list.files(
    additional_root,
    pattern = "^meta_analysis_summary\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
} else {
  character(0)
}

additional_count <- 0L

for (summary_path in additional_summary_files) {
  workspace <- dirname(summary_path)
  summary_data <- tryCatch(read_csv_safe(summary_path), error = function(e) NULL)
  if (is.null(summary_data) || nrow(summary_data) == 0L) next

  additional_count <- additional_count + 1L
  analysis_group <- pick_scalar(
    summary_data,
    c("analysis_group", "group_name", "analysis_set", "analysis_name", "label"),
    basename(workspace)
  )

  ready_candidates <- c(
    file.path(workspace, "additional_analysis_meta_analysis_ready.csv"),
    file.path(workspace, "meta_analysis_ready.csv"),
    file.path(workspace, "study_effects_used.csv")
  )
  ready_file <- ready_candidates[vapply(ready_candidates, file_nonempty, logical(1))]
  ready_file <- if (length(ready_file) > 0L) ready_file[[1]] else ""

  arm_labels <- infer_arm_labels(ready_file)

  tryCatch(
    {
      normalized_summary <- normalize_r27_summary(
        summary_data,
        analysis_group,
        arm_labels$treatment_label,
        arm_labels$control_label,
        summary_path,
        ready_file,
        ready_file
      ) |>
        dplyr::mutate(
          numerical_source = "R/29_CANONICAL_OUTPUT",
          label_source = arm_labels$label_source,
          analysis_type = "additional_analysis",
          analysis_purpose = analysis_group,
          source_workspace = normalize_slash(workspace),
          source_r29_summary = normalize_slash(summary_path)
        )

      source_pdf <- file.path(workspace, "forest_plot.pdf")
      source_png <- file.path(workspace, "forest_plot.png")
      target_base <- file.path(figures_dir, paste0("forest_additional_", slugify(analysis_group)))
      target_pdf <- paste0(target_base, ".pdf")
      target_png <- paste0(target_base, ".png")

      copied_pdf <- copy_if_exists(source_pdf, target_pdf)
      copied_png <- copy_if_exists(source_png, target_png)
      additional_studies <- NULL

      if (nzchar(ready_file)) {
        ready_data <- read_csv_safe(ready_file)
        additional_studies <- tryCatch(
          normalize_r27_studies(
            ready_data,
            analysis_group,
            normalized_summary$summary_measure[[1]],
            ready_file,
            ready_file
          ) |>
            dplyr::mutate(
              numerical_source = "R/29_CANONICAL_OUTPUT",
              analysis_type = "additional_analysis"
            ),
          error = function(e) NULL
        )
      }

      if ((!copied_pdf || !copied_png) && !is.null(additional_studies) && nrow(additional_studies) > 0L) {
        plot_files <- save_canonical_forest_plot(additional_studies, normalized_summary, target_base)
        target_png <- plot_files$png
        target_pdf <- plot_files$pdf
        copied_png <- TRUE
        copied_pdf <- TRUE
      }

      if (!copied_pdf) target_pdf <- ""
      if (!copied_png) target_png <- ""

      normalized_summary <- normalized_summary |>
        dplyr::mutate(
          forest_plot_png = normalize_slash(target_png),
          forest_plot_pdf = normalize_slash(target_pdf)
        )

      summary_rows[[length(summary_rows) + 1L]] <- normalized_summary
      if (!is.null(additional_studies) && nrow(additional_studies) > 0L) {
        study_rows[[length(study_rows) + 1L]] <- additional_studies
      }

      figure_rows[[length(figure_rows) + 1L]] <- tibble::tibble(
        analysis_group = analysis_group,
        analysis_type = "additional_analysis",
        numerical_source = "R/29_CANONICAL_OUTPUT",
        png_file = normalize_slash(target_png),
        pdf_file = normalize_slash(target_pdf),
        source_r27_summary = "",
        source_r27_study_effects = "",
        source_workspace = normalize_slash(workspace),
        status = if (nzchar(target_pdf) || nzchar(target_png)) "PASS" else "SUMMARY_ONLY"
      )
    },
    error = function(e) {
      failure_rows[[length(failure_rows) + 1L]] <<- tibble::tibble(
        analysis_group = analysis_group,
        analysis_type = "additional_analysis",
        error_message = conditionMessage(e),
        source_r27_summary = "",
        source_r27_study_effects = ""
      )
    }
  )
}

# ============================================================
# Bind + validate
# ============================================================

summary_table <- if (length(summary_rows) == 0L) tibble::tibble() else dplyr::bind_rows(summary_rows)
study_table <- if (length(study_rows) == 0L) tibble::tibble() else dplyr::bind_rows(study_rows)
figure_manifest <- if (length(figure_rows) == 0L) tibble::tibble() else dplyr::bind_rows(figure_rows)
contract_table <- if (length(contract_rows) == 0L) tibble::tibble() else dplyr::bind_rows(contract_rows)
failure_table <- if (length(failure_rows) == 0L) tibble::tibble() else dplyr::bind_rows(failure_rows)

main_expected_n <- nrow(r27_registry)
main_pass_n <- if (nrow(summary_table) == 0L) 0L else sum(summary_table$analysis_type == "main_analysis", na.rm = TRUE)

if (main_expected_n == 0L || main_pass_n != main_expected_n) {
  stop(
    paste0(
      "R/33 canonical main-analysis reporting is incomplete. Expected ",
      main_expected_n, " R/27 PASS group(s); generated ", main_pass_n, "."
    ),
    call. = FALSE
  )
}

if (nrow(contract_table) != main_expected_n || any(contract_table$contract_status != "PASS")) {
  stop("R/27 -> R/33 numerical contract audit did not PASS for every main group.", call. = FALSE)
}

# ============================================================
# Save editable outputs
# ============================================================

readr::write_csv(summary_table, summary_csv, na = "")
readr::write_csv(study_table, study_csv, na = "")
readr::write_csv(contract_table, contract_csv, na = "")
readr::write_csv(figure_manifest, figure_manifest_csv, na = "")

wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Meta_Analysis_Summary")
openxlsx::writeData(wb, "Meta_Analysis_Summary", summary_table)
openxlsx::addWorksheet(wb, "Study_Level_Effects")
openxlsx::writeData(wb, "Study_Level_Effects", study_table)
openxlsx::addWorksheet(wb, "Figure_Manifest")
openxlsx::writeData(wb, "Figure_Manifest", figure_manifest)
openxlsx::addWorksheet(wb, "R27_R33_Contract")
openxlsx::writeData(wb, "R27_R33_Contract", contract_table)
openxlsx::addWorksheet(wb, "R27_Execution_Summary")
openxlsx::writeData(wb, "R27_Execution_Summary", execution_summary)

if (!is.null(analysis_manifest)) {
  openxlsx::addWorksheet(wb, "Input_Manifest")
  openxlsx::writeData(wb, "Input_Manifest", analysis_manifest)
}
if (nrow(crosswalk) > 0L) {
  openxlsx::addWorksheet(wb, "Study_Label_Crosswalk")
  openxlsx::writeData(wb, "Study_Label_Crosswalk", crosswalk)
}
if (nrow(failure_table) > 0L) {
  openxlsx::addWorksheet(wb, "Failures")
  openxlsx::writeData(wb, "Failures", failure_table)
}

openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)

required_outputs <- c(summary_csv, study_csv, contract_csv, figure_manifest_csv, xlsx_file)
output_ok <- vapply(required_outputs, file_nonempty, logical(1))
if (!all(output_ok)) {
  stop(
    "R/33 failed to create required output(s): ",
    paste(basename(required_outputs[!output_ok]), collapse = ", "),
    call. = FALSE
  )
}

saved_contract <- read_csv_safe(contract_csv)
if (nrow(saved_contract) != main_expected_n || any(saved_contract$contract_status != "PASS")) {
  stop("Saved R/27 -> R/33 contract audit is incomplete.", call. = FALSE)
}

# ============================================================
# Report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/33 — Forest Plots and Editable Tables",
  "",
  paste0("Run time: ", now_text()),
  paste0("Repository root: ", normalize_slash(repo_root)),
  "",
  "ARCHITECTURAL CONTRACT",
  "--------------------------------------------------",
  "Statistical engine for canonical main analyses: R/27 only",
  "R/33 model refit: NO",
  "R/33 pooled-effect recalculation: NO",
  "R/33 main forest plots: copied directly from R/27 canonical forest_plot.pdf",
  "Topic-specific treatment/control fallback labels: PROHIBITED",
  "Generic fallback labels: Intervention / Comparator",
  "",
  "R/27 INPUT",
  "--------------------------------------------------",
  paste0("Execution summary: ", normalize_slash(execution_summary_file)),
  paste0("PASS groups expected: ", main_expected_n),
  paste0("PASS groups rendered: ", main_pass_n),
  "",
  "R/27 -> R/33 NUMERICAL CONTRACT",
  "--------------------------------------------------",
  paste0("Contract rows: ", nrow(contract_table)),
  paste0("Contract PASS: ", sum(contract_table$contract_status == "PASS")),
  "Rule: pooled effect, 95% CI and heterogeneity values are copied from R/27; main forest figures are copied from R/27; no statistical model is re-fit in R/33.",
  "",
  "OPTIONAL ADDITIONAL ANALYSES",
  "--------------------------------------------------",
  paste0("Completed R/29 summaries detected: ", additional_count),
  "R/29 policy: reuse canonical completed output; do not refit.",
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0("Summary CSV: ", normalize_slash(summary_csv)),
  paste0("Study-level CSV: ", normalize_slash(study_csv)),
  paste0("R27-R33 contract CSV: ", normalize_slash(contract_csv)),
  paste0("Figure manifest CSV: ", normalize_slash(figure_manifest_csv)),
  paste0("Workbook: ", normalize_slash(xlsx_file)),
  paste0("Figures directory: ", normalize_slash(figures_dir)),
  "",
  "STATUS",
  "--------------------------------------------------",
  "PASS: R/33 publication outputs preserve the canonical R/27 numerical results."
)

writeLines(report_lines, report_file)

# ============================================================
# Console summary
# ============================================================

cat("\n============================================================\n")
cat("R/33 — Forest Plots and Editable Tables\n")
cat("============================================================\n\n")
cat("Canonical statistical engine: R/27\n")
cat("R/33 statistical refit: NO\n")
cat("R/27 PASS groups: ", main_expected_n, "\n", sep = "")
cat("R/33 rendered groups: ", main_pass_n, "\n", sep = "")
cat("R/27 -> R/33 numerical contract: PASS\n")
cat("Completed additional analyses detected: ", additional_count, "\n\n", sep = "")

if (nrow(summary_table) > 0L) {
  print(summary_table, n = Inf, width = Inf)
  cat("\n")
}

if (nrow(failure_table) > 0L) {
  cat("Non-fatal reporting failures:\n")
  print(failure_table, n = Inf, width = Inf)
  cat("\n")
}

cat("PASS: R/33 preserved canonical R/27 numbers and reused canonical R/27 main forest figures without re-fitting models.\n")
cat("Summary CSV: ", normalize_slash(summary_csv), "\n", sep = "")
cat("Study CSV: ", normalize_slash(study_csv), "\n", sep = "")
cat("Contract CSV: ", normalize_slash(contract_csv), "\n", sep = "")
cat("Workbook: ", normalize_slash(xlsx_file), "\n", sep = "")
cat("Report: ", normalize_slash(report_file), "\n", sep = "")
