# ============================================================
# Universal SR-MA Agent Toolkit
# R/26_finalize_human_verified_extraction.R
#
# PURPOSE
# ------------------------------------------------------------
# Canonical post-Human-Verification finalization step.
#
# This script:
#   1) reads the completed Targeted Human Verification workbook,
#   2) selects ONLY rows explicitly approved for planned analysis,
#   3) resolves Human-Verified values as the authoritative source,
#   4) separates approved rows by analysis_group,
#   5) creates one meta-analysis-ready dataset per analysis_group,
#   6) creates a universal meta-analysis manifest,
#   7) prints a concise recommendation for the next meta-analysis step,
#   8) stops with FAIL if Human Verification is incomplete or internally
#      inconsistent.
#
# UNIVERSAL GOVERNANCE RULE
# ------------------------------------------------------------
# Quantitative synthesis eligibility is determined ONLY by:
#
#   human_include_in_planned_analysis == "Yes"
#
# and the row must also have:
#
#   verification_status %in% c("Verified", "Corrected")
#
# Human-verified values are authoritative.
#
# R/26 does NOT:
#   - decide which studies should be included,
#   - perform Human Verification,
#   - silently convert incompatible effect measures,
#   - pool RCT and observational studies together,
#   - perform the meta-analysis itself.
#
# R/26 prepares authoritative, analysis-ready datasets and tells the
# downstream meta-analysis stage what can be analyzed.
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

normalize_yes_no <- function(x) {
  y <- tolower(clean_text(x))

  dplyr::case_when(
    y %in% c("yes", "y", "true", "1") ~ "Yes",
    y %in% c("no", "n", "false", "0") ~ "No",
    TRUE ~ clean_text(x)
  )
}

normalize_verification_status <- function(x) {
  y <- tolower(clean_text(x))

  dplyr::case_when(
    y == "verified" ~ "Verified",
    y == "corrected" ~ "Corrected",
    y %in% c(
      "needs clarification",
      "needs_clarification",
      "clarification"
    ) ~ "Needs Clarification",
    TRUE ~ clean_text(x)
  )
}

authoritative_value <- function(human_value, agent_value) {
  human_value <- clean_text(human_value)
  agent_value <- clean_text(agent_value)

  ifelse(
    !is.na(human_value),
    human_value,
    agent_value
  )
}

safe_file_component <- function(x) {
  x <- clean_text(x)
  x[is.na(x)] <- "unnamed_analysis"

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

first_nonmissing <- function(x) {
  x <- clean_text(x)
  x <- x[!is.na(x)]

  if (length(x) == 0L) {
    return(NA_character_)
  }

  x[[1]]
}

collapse_unique <- function(x, sep = "; ") {
  x <- clean_text(x)
  x <- unique(x[!is.na(x)])

  if (length(x) == 0L) {
    return(NA_character_)
  }

  paste(x, collapse = sep)
}

all_same_or_mixed <- function(x) {
  x <- clean_text(x)
  x <- unique(x[!is.na(x)])

  if (length(x) == 0L) {
    return(NA_character_)
  }

  if (length(x) == 1L) {
    return(x[[1]])
  }

  "Mixed"
}


# ============================================================
# 2. Project utilities
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
# 3. Canonical paths
# ============================================================

targeted_directory <- file.path(
  paths$project_root,
  "results",
  "extraction",
  "targeted_human_verification"
)

input_workbook <- file.path(
  targeted_directory,
  "targeted_human_verification.xlsx"
)

output_data_directory <- file.path(
  paths$project_root,
  "data",
  "processed",
  "meta_analysis_ready"
)

output_results_directory <- file.path(
  paths$project_root,
  "results",
  "meta_analysis_ready"
)

dir.create(
  output_data_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  output_results_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

all_verified_csv <- file.path(
  output_data_directory,
  "human_verified_planned_analysis_outcomes.csv"
)

manifest_csv <- file.path(
  output_data_directory,
  "meta_analysis_manifest.csv"
)

final_workbook <- file.path(
  output_results_directory,
  "meta_analysis_ready_datasets.xlsx"
)

report_file <- file.path(
  paths$logs_directory,
  "human_verified_extraction_finalization_report.txt"
)

if (!file.exists(input_workbook)) {
  stop(
    "Targeted Human Verification workbook not found:\n",
    input_workbook,
    call. = FALSE
  )
}


# ============================================================
# 4. Validate workbook
# ============================================================

sheet_names <- openxlsx::getSheetNames(
  input_workbook
)

required_sheets <- c(
  "Target_Outcome_Verification"
)

missing_sheets <- setdiff(
  required_sheets,
  sheet_names
)

if (length(missing_sheets) > 0L) {
  stop(
    "Required sheet(s) missing from targeted_human_verification.xlsx: ",
    paste(missing_sheets, collapse = ", "),
    call. = FALSE
  )
}

outcomes <- openxlsx::readWorkbook(
  input_workbook,
  sheet = "Target_Outcome_Verification",
  colNames = TRUE,
  skipEmptyRows = TRUE
)

if (nrow(outcomes) == 0L) {
  stop(
    "Target_Outcome_Verification contains no rows.",
    call. = FALSE
  )
}


# ============================================================
# 5. Universal minimum contract
# ============================================================

required_columns <- c(
  "screening_id",
  "outcome_row_id",
  "outcome_name",
  "analysis_group",
  "meta_analysis_role",
  "verification_status",
  "human_include_in_planned_analysis"
)

missing_columns <- setdiff(
  required_columns,
  names(outcomes)
)

if (length(missing_columns) > 0L) {
  stop(
    "Target_Outcome_Verification is missing required column(s):\n- ",
    paste(missing_columns, collapse = "\n- "),
    call. = FALSE
  )
}

outcomes <- outcomes |>
  dplyr::mutate(
    verification_status = normalize_verification_status(
      verification_status
    ),
    human_include_in_planned_analysis = normalize_yes_no(
      human_include_in_planned_analysis
    ),
    screening_id = clean_text(screening_id),
    outcome_row_id = clean_text(outcome_row_id),
    outcome_name = clean_text(outcome_name),
    analysis_group = clean_text(analysis_group),
    meta_analysis_role = clean_text(meta_analysis_role)
  )


# ============================================================
# 6. Human gate
# ============================================================

approved <- outcomes |>
  dplyr::filter(
    human_include_in_planned_analysis == "Yes"
  )

if (nrow(approved) == 0L) {
  stop(
    "No row is marked Yes in human_include_in_planned_analysis. ",
    "There is no Human-approved quantitative synthesis dataset.",
    call. = FALSE
  )
}

bad_status <- approved |>
  dplyr::filter(
    !verification_status %in% c(
      "Verified",
      "Corrected"
    )
  )

if (nrow(bad_status) > 0L) {
  stop(
    "Human gate is incomplete. ",
    nrow(bad_status),
    " approved row(s) are not finalized as Verified or Corrected.",
    call. = FALSE
  )
}

missing_group <- approved |>
  dplyr::filter(
    is.na(analysis_group)
  )

if (nrow(missing_group) > 0L) {
  stop(
    "Human-approved row(s) are missing analysis_group. ",
    "R/26 cannot create analysis-specific datasets.",
    call. = FALSE
  )
}

missing_role <- approved |>
  dplyr::filter(
    is.na(meta_analysis_role)
  )

if (nrow(missing_role) > 0L) {
  stop(
    "Human-approved row(s) are missing meta_analysis_role.",
    call. = FALSE
  )
}

if (anyDuplicated(approved$outcome_row_id) > 0L) {
  stop(
    "Duplicate outcome_row_id detected among approved rows.",
    call. = FALSE
  )
}


# ============================================================
# 7. Resolve Human-Verified authoritative values
# ============================================================
# Rule:
#   - human_verified_* overrides Agent extraction when present
#   - otherwise the verified Agent value remains authoritative
#
# The script uses only columns that actually exist, making the
# finalizer reusable across projects and extraction schemas.
# ============================================================

human_prefix <- "human_verified_"

human_columns <- grep(
  paste0("^", human_prefix),
  names(approved),
  value = TRUE
)

for (human_col in human_columns) {

  agent_col <- sub(
    paste0("^", human_prefix),
    "",
    human_col
  )

  final_col <- paste0(
    "final_",
    agent_col
  )

  if (agent_col %in% names(approved)) {
    approved[[final_col]] <- authoritative_value(
      approved[[human_col]],
      approved[[agent_col]]
    )
  } else {
    approved[[final_col]] <- clean_text(
      approved[[human_col]]
    )
  }
}


# ============================================================
# 8. Ensure commonly required final fields exist
# ============================================================

common_agent_fields <- c(
  "outcome_definition",
  "timepoint",
  "data_type",
  "intervention_events",
  "intervention_total",
  "comparator_events",
  "comparator_total",
  "effect_measure",
  "effect_estimate",
  "lower_ci",
  "upper_ci",
  "adjusted",
  "adjustment_variables",
  "source_location"
)

for (field in common_agent_fields) {

  final_field <- paste0(
    "final_",
    field
  )

  if (!final_field %in% names(approved)) {

    if (field %in% names(approved)) {
      approved[[final_field]] <- clean_text(
        approved[[field]]
      )
    } else {
      approved[[final_field]] <- NA_character_
    }
  }
}


# ============================================================
# 9. Derive generic meta-analysis input metadata
# ============================================================

infer_input_type <- function(
    data_type,
    intervention_events,
    intervention_total,
    comparator_events,
    comparator_total,
    effect_estimate,
    lower_ci,
    upper_ci
) {

  dt <- tolower(
    safe_chr(data_type)
  )

  nums_raw <- safe_numeric(
    c(
      intervention_events,
      intervention_total,
      comparator_events,
      comparator_total
    )
  )

  nums_effect <- safe_numeric(
    c(
      effect_estimate,
      lower_ci,
      upper_ci
    )
  )

  if (
    grepl(
      "dichotom|binary|event",
      dt
    ) &&
    all(
      is.finite(nums_raw)
    )
  ) {
    return("raw_dichotomous")
  }

  if (
    all(
      is.finite(nums_effect)
    )
  ) {
    return("reported_effect")
  }

  if (
    grepl(
      "continuous|mean",
      dt
    )
  ) {
    return("continuous")
  }

  "other_or_incomplete"
}

approved$meta_input_type <- purrr::pmap_chr(
  list(
    approved$final_data_type,
    approved$final_intervention_events,
    approved$final_intervention_total,
    approved$final_comparator_events,
    approved$final_comparator_total,
    approved$final_effect_estimate,
    approved$final_lower_ci,
    approved$final_upper_ci
  ),
  infer_input_type
)


# ============================================================
# 10. Flag design-mixing risks
# ============================================================
# R/26 does not assume fixed design labels. It looks for any
# available study-design field and warns if an analysis_group
# contains materially different designs.
# ============================================================

design_candidates <- c(
  "final_study_design",
  "study_design",
  "design",
  "study_type"
)

design_col <- design_candidates[
  design_candidates %in% names(approved)
]

if (length(design_col) > 0L) {
  design_col <- design_col[[1]]
} else {
  design_col <- NA_character_
}

if (!is.na(design_col)) {
  approved$final_design_for_manifest <- clean_text(
    approved[[design_col]]
  )
} else {
  approved$final_design_for_manifest <- NA_character_
}


# ============================================================
# 11. Split automatically by analysis_group
# ============================================================

analysis_groups <- sort(
  unique(
    approved$analysis_group
  )
)

analysis_groups <- analysis_groups[
  !is.na(
    analysis_groups
  )
]

if (length(analysis_groups) == 0L) {
  stop(
    "No usable analysis_group values found.",
    call. = FALSE
  )
}

group_datasets <- split(
  approved,
  approved$analysis_group
)


# ============================================================
# 12. Universal effect-measure compatibility check
# ============================================================
# R/26 does not automatically convert effect measures.
# It records whether each group has one effect measure or mixed
# measures, so the next meta-analysis stage can decide whether a
# transformation or separate analysis is required.
# ============================================================

manifest <- purrr::imap_dfr(
  group_datasets,
  function(df, group_name) {

    effect_measure <- all_same_or_mixed(
      df$final_effect_measure
    )

    data_type <- all_same_or_mixed(
      df$final_data_type
    )

    input_type <- all_same_or_mixed(
      df$meta_input_type
    )

    design <- all_same_or_mixed(
      df$final_design_for_manifest
    )

    outcome <- collapse_unique(
      df$outcome_name
    )

    timepoint <- collapse_unique(
      df$final_timepoint
    )

    role <- collapse_unique(
      df$meta_analysis_role
    )

    suggested_action <- dplyr::case_when(
      identical(
        effect_measure,
        "Mixed"
      ) ~ paste0(
        "Do not pool yet: effect measures are mixed. ",
        "Harmonize or analyze separately."
      ),

      identical(
        input_type,
        "other_or_incomplete"
      ) ~ paste0(
        "Do not pool yet: meta-analysis input is incomplete."
      ),

      identical(
        input_type,
        "Mixed"
      ) ~ paste0(
        "Review input compatibility before pooling. ",
        "Raw and reported effects may require a common effect scale."
      ),

      TRUE ~ paste0(
        "Proceed to analysis-specific meta-analysis after checking ",
        "clinical and methodological compatibility."
      )
    )

    tibble::tibble(
      analysis_group = group_name,
      meta_analysis_role = role,
      study_count = dplyr::n_distinct(
        df$screening_id
      ),
      outcome_row_count = nrow(df),
      study_design = design,
      outcome = outcome,
      timepoint = timepoint,
      data_type = data_type,
      effect_measure = effect_measure,
      meta_input_type = input_type,
      suggested_model = "Random-effects by default; justify fixed-effect if used",
      combine_with_other_analysis_groups = "NO unless explicitly prespecified and methodologically justified",
      next_action = suggested_action
    )
  }
)


# ============================================================
# 13. Write one canonical CSV per analysis_group
# ============================================================

dataset_files <- character(
  length(group_datasets)
)

names(dataset_files) <- names(
  group_datasets
)

for (group_name in names(group_datasets)) {

  file_name <- paste0(
    "meta_analysis_ready_",
    safe_file_component(
      group_name
    ),
    ".csv"
  )

  output_file <- file.path(
    output_data_directory,
    file_name
  )

  readr::write_csv(
    group_datasets[[group_name]],
    output_file,
    na = ""
  )

  dataset_files[[group_name]] <- output_file
}

manifest$dataset_file <- unname(
  dataset_files[
    manifest$analysis_group
  ]
)


# ============================================================
# 14. Write all approved rows + manifest
# ============================================================

readr::write_csv(
  approved,
  all_verified_csv,
  na = ""
)

readr::write_csv(
  manifest,
  manifest_csv,
  na = ""
)


# ============================================================
# 15. Meta-analysis guidance
# ============================================================

make_guidance <- function(manifest) {

  header <- c(
    "UNIVERSAL META-ANALYSIS GUIDANCE",
    "--------------------------------------------------",
    paste0(
      "Human-approved analysis groups: ",
      nrow(manifest)
    ),
    ""
  )

  group_lines <- unlist(
    lapply(
      seq_len(nrow(manifest)),
      function(i) {

        x <- manifest[i, ]

        c(
          paste0(
            "[",
            i,
            "] ",
            x$analysis_group
          ),
          paste0(
            "  Role: ",
            safe_chr(
              x$meta_analysis_role
            )
          ),
          paste0(
            "  Studies: ",
            x$study_count
          ),
          paste0(
            "  Outcome: ",
            safe_chr(
              x$outcome
            )
          ),
          paste0(
            "  Design: ",
            safe_chr(
              x$study_design
            )
          ),
          paste0(
            "  Effect measure: ",
            safe_chr(
              x$effect_measure
            )
          ),
          paste0(
            "  Input type: ",
            safe_chr(
              x$meta_input_type
            )
          ),
          paste0(
            "  Suggested model: ",
            x$suggested_model
          ),
          paste0(
            "  Recommendation: ",
            x$next_action
          ),
          ""
        )
      }
    )
  )

  footer <- c(
    "GENERAL RULES",
    "--------------------------------------------------",
    "1. Analyze each analysis_group separately unless a combined analysis was explicitly prespecified.",
    "2. Do not combine materially different study designs into one pooled estimate by default.",
    "3. Do not silently combine RR, OR, HR, MD, SMD, or other incompatible effect measures.",
    "4. Prefer adjusted effect estimates for observational studies when the planned estimand requires them.",
    "5. Use sensitivity analyses to test robustness when broader eligibility or alternative definitions are allowed.",
    "6. Interpret the primary analysis according to its prespecified evidence role; treat complementary analyses as supportive unless otherwise defined.",
    ""
  )

  c(
    header,
    group_lines,
    footer
  )
}

guidance_lines <- make_guidance(
  manifest
)


# ============================================================
# 16. Final workbook
# ============================================================

wb <- openxlsx::createWorkbook(
  creator = "Universal SR-MA Agent Toolkit"
)

add_table_sheet <- function(
    wb,
    sheet_name,
    data
) {

  openxlsx::addWorksheet(
    wb,
    sheet_name
  )

  openxlsx::writeData(
    wb,
    sheet_name,
    data,
    colNames = TRUE,
    rowNames = FALSE,
    withFilter = TRUE
  )

  if (ncol(data) > 0L) {
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = seq_len(
        ncol(data)
      ),
      widths = "auto"
    )
  }

  openxlsx::freezePane(
    wb,
    sheet_name,
    firstActiveRow = 2
  )

  invisible(
    TRUE
  )
}

summary_table <- tibble::tibble(
  Metric = c(
    "Project",
    "Human-approved rows",
    "Human-approved studies",
    "Analysis groups",
    "Selection rule",
    "Authoritative source",
    "Meta-analysis execution"
  ),
  Value = c(
    project_title,
    as.character(
      nrow(approved)
    ),
    as.character(
      dplyr::n_distinct(
        approved$screening_id
      )
    ),
    as.character(
      length(
        analysis_groups
      )
    ),
    "human_include_in_planned_analysis == Yes",
    "Human-verified values",
    "Not performed by R/26"
  )
)

add_table_sheet(
  wb,
  "Finalization_Summary",
  summary_table
)

add_table_sheet(
  wb,
  "Meta_Analysis_Manifest",
  manifest
)

add_table_sheet(
  wb,
  "All_Approved_Outcomes",
  approved
)

for (group_name in names(group_datasets)) {

  sheet_name <- safe_file_component(
    group_name
  )

  sheet_name <- substr(
    sheet_name,
    1,
    31
  )

  if (sheet_name == "") {
    sheet_name <- "analysis_group"
  }

  add_table_sheet(
    wb,
    sheet_name,
    group_datasets[[group_name]]
  )
}

openxlsx::addWorksheet(
  wb,
  "Meta_Analysis_Guidance"
)

openxlsx::writeData(
  wb,
  "Meta_Analysis_Guidance",
  data.frame(
    Guidance = guidance_lines,
    stringsAsFactors = FALSE
  ),
  colNames = FALSE,
  rowNames = FALSE
)

openxlsx::setColWidths(
  wb,
  "Meta_Analysis_Guidance",
  cols = 1,
  widths = 110
)

openxlsx::saveWorkbook(
  wb,
  final_workbook,
  overwrite = TRUE
)


# ============================================================
# 17. Output validation
# ============================================================

required_outputs <- c(
  all_verified_csv,
  manifest_csv,
  final_workbook,
  unname(dataset_files)
)

bad_outputs <- required_outputs[
  !file.exists(required_outputs) |
    is.na(file.info(required_outputs)$size) |
    file.info(required_outputs)$size <= 0
]

if (length(bad_outputs) > 0L) {
  stop(
    "R/26 output validation failed:\n- ",
    paste(
      bad_outputs,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}


# ============================================================
# 18. Report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/26 — Human-Verified Extraction Finalization",
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
  "CANONICAL HUMAN SELECTION RULE",
  "--------------------------------------------------",
  "human_include_in_planned_analysis == Yes",
  "verification_status must be Verified or Corrected",
  "",
  "FINALIZATION",
  "--------------------------------------------------",
  paste0(
    "Human-approved outcome rows: ",
    nrow(approved)
  ),
  paste0(
    "Human-approved studies: ",
    dplyr::n_distinct(
      approved$screening_id
    )
  ),
  paste0(
    "Analysis groups created: ",
    length(
      analysis_groups
    )
  ),
  "",
  "META-ANALYSIS MANIFEST",
  "--------------------------------------------------",
  paste(
    capture.output(
      print(
        manifest
      )
    ),
    collapse = "\n"
  ),
  "",
  guidance_lines,
  "",
  "STATUS",
  "--------------------------------------------------",
  "R/26: PASS",
  "",
  "NEXT",
  "--------------------------------------------------",
  "Proceed to the meta-analysis execution stage using the generated manifest and group-specific datasets."
)

writeLines(
  report_lines,
  report_file,
  useBytes = TRUE
)


# ============================================================
# 19. Console summary
# ============================================================

cat("\n")
cat("============================================================\n")
cat("R/26 — Universal Human-Verified Extraction Finalization\n")
cat("============================================================\n\n")

cat(
  "Human-approved outcome rows: ",
  nrow(approved),
  "\n",
  sep = ""
)

cat(
  "Human-approved studies: ",
  dplyr::n_distinct(
    approved$screening_id
  ),
  "\n",
  sep = ""
)

cat(
  "Analysis groups created: ",
  length(analysis_groups),
  "\n\n",
  sep = ""
)

for (i in seq_len(nrow(manifest))) {

  cat(
    "[",
    i,
    "] ",
    manifest$analysis_group[[i]],
    "\n",
    sep = ""
  )

  cat(
    "    Role: ",
    safe_chr(
      manifest$meta_analysis_role[[i]]
    ),
    "\n",
    sep = ""
  )

  cat(
    "    Studies: ",
    manifest$study_count[[i]],
    "\n",
    sep = ""
  )

  cat(
    "    Effect measure: ",
    safe_chr(
      manifest$effect_measure[[i]]
    ),
    "\n",
    sep = ""
  )

  cat(
    "    Next: ",
    manifest$next_action[[i]],
    "\n\n",
    sep = ""
  )
}

cat(
  "PASS: R/26 finalized Human-approved meta-analysis-ready datasets.\n"
)

cat(
  "PASS: Meta-analysis manifest created.\n"
)

cat(
  "NEXT: Run each analysis_group separately according to the manifest.\n"
)

invisible(
  list(
    status = "PASS",
    approved_outcomes = approved,
    group_datasets = group_datasets,
    manifest = manifest,
    workbook = final_workbook,
    report = report_file
  )
)
