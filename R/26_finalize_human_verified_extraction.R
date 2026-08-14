# ============================================================
# Universal SR-MA Agent Toolkit
# R/26_finalize_human_verified_extraction.R
#
# Revision: Universal analysis-set finalization
#
# PURPOSE
# ------------------------------------------------------------
# Canonical post-Human-Verification finalization step.
#
# R/26:
#   1) reads completed targeted_human_verification.xlsx,
#   2) selects ONLY Human-approved quantitative rows,
#   3) resolves Human-verified values as authoritative,
#   4) sanitizes non-applicable / invalid extraction fields,
#   5) validates meta-analysis input compatibility,
#   6) creates analysis SETS rather than merely raw groups,
#   7) supports composite sensitivity sets:
#
#        sensitivity set = parent primary set + sensitivity-only rows
#
#   8) writes one meta-analysis-ready CSV per final analysis set,
#   9) writes meta_analysis_manifest.csv,
#  10) prints the recommended downstream analysis structure.
#
# UNIVERSAL HUMAN GATE
# ------------------------------------------------------------
# Quantitative analysis requires:
#
#   human_include_in_planned_analysis == "Yes"
#
# AND
#
#   verification_status %in% c("Verified", "Corrected")
#
# Human-verified values are authoritative.
#
# UNIVERSAL ANALYSIS-SET RULE
# ------------------------------------------------------------
# analysis_group describes the row's assigned role/group.
# R/26 converts those groups into FINAL ANALYSIS SETS.
#
# Preferred explicit relation:
#   parent_analysis_group
#
# Backward-compatible fallback:
#   if a group is clearly a sensitivity group and no explicit parent
#   exists, R/26 tries to infer ONE compatible primary parent using:
#     - meta_analysis_role / analysis_group semantics,
#     - outcome compatibility,
#     - broad study-design compatibility.
#
# If parent inference is ambiguous, R/26 stops rather than guessing.
#
# IMPORTANT
# ------------------------------------------------------------
# R/26 does NOT:
#   - perform meta-analysis,
#   - silently pool different study designs,
#   - silently convert RR <-> OR <-> HR,
#   - change Human Include/Exclude decisions.
#
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

first_existing_column <- function(data, candidates) {
  hit <- candidates[
    candidates %in% names(data)
  ]

  if (length(hit) == 0L) {
    return(NA_character_)
  }

  hit[[1]]
}

normalize_effect_measure <- function(x) {
  raw <- clean_text(x)
  y <- toupper(raw)

  y <- stringr::str_replace_all(
    y,
    "[^A-Z0-9]+",
    ""
  )

  out <- dplyr::case_when(
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

  out
}

normalize_data_type <- function(x) {
  raw <- clean_text(x)
  y <- tolower(raw)

  dplyr::case_when(
    stringr::str_detect(
      y,
      "reported.*effect|effect.*estimate"
    ) ~ "reported_effect",

    stringr::str_detect(
      y,
      "dichotom|binary|event"
    ) ~ "dichotomous",

    stringr::str_detect(
      y,
      "continuous|mean"
    ) ~ "continuous",

    TRUE ~ raw
  )
}

infer_design_class <- function(x) {
  y <- tolower(
    clean_text(x)
  )

  dplyr::case_when(
    stringr::str_detect(
      y,
      "randomized|randomised|\\brct\\b"
    ) ~ "RCT",

    stringr::str_detect(
      y,
      "observational|retrospective|prospective cohort|cohort|case.control|case-control|propensity|registry"
    ) ~ "Observational",

    is.na(y) ~ NA_character_,

    TRUE ~ "Other_or_Unclear"
  )
}

role_class <- function(group_name, role) {
  txt <- tolower(
    paste(
      safe_chr(group_name),
      safe_chr(role)
    )
  )

  if (
    stringr::str_detect(
      txt,
      "sensitiv|robust"
    )
  ) {
    return("Sensitivity")
  }

  if (
    stringr::str_detect(
      txt,
      "primary"
    )
  ) {
    return("Primary")
  }

  if (
    stringr::str_detect(
      txt,
      "observ|real.world|real world"
    )
  ) {
    return("Observational")
  }

  "Standalone"
}

compatible_for_sensitivity <- function(
    sensitivity_df,
    primary_df
) {

  sensitivity_outcomes <- unique(
    clean_text(
      sensitivity_df$outcome_name
    )
  )

  sensitivity_outcomes <- sensitivity_outcomes[
    !is.na(
      sensitivity_outcomes
    )
  ]

  primary_outcomes <- unique(
    clean_text(
      primary_df$outcome_name
    )
  )

  primary_outcomes <- primary_outcomes[
    !is.na(
      primary_outcomes
    )
  ]

  outcome_ok <- (
    length(sensitivity_outcomes) == 0L ||
      length(primary_outcomes) == 0L ||
      length(
        intersect(
          sensitivity_outcomes,
          primary_outcomes
        )
      ) > 0L
  )

  sensitivity_design <- unique(
    clean_text(
      sensitivity_df$design_class
    )
  )

  sensitivity_design <- sensitivity_design[
    !is.na(
      sensitivity_design
    )
  ]

  primary_design <- unique(
    clean_text(
      primary_df$design_class
    )
  )

  primary_design <- primary_design[
    !is.na(
      primary_design
    )
  ]

  design_ok <- (
    length(sensitivity_design) == 0L ||
      length(primary_design) == 0L ||
      length(
        intersect(
          sensitivity_design,
          primary_design
        )
      ) > 0L
  )

  outcome_ok && design_ok
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
# 4. Read workbook
# ============================================================

sheet_names <- openxlsx::getSheetNames(
  input_workbook
)

if (
  !"Target_Outcome_Verification" %in% sheet_names
) {
  stop(
    "Required sheet missing: Target_Outcome_Verification",
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

study <- NULL

if (
  "Target_Study_Verification" %in% sheet_names
) {
  study <- openxlsx::readWorkbook(
    input_workbook,
    sheet = "Target_Study_Verification",
    colNames = TRUE,
    skipEmptyRows = TRUE
  )
}


# ============================================================
# 5. Minimum contract
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
    paste(
      missing_columns,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}

outcomes <- outcomes |>
  dplyr::mutate(
    screening_id = clean_text(screening_id),
    outcome_row_id = clean_text(outcome_row_id),
    outcome_name = clean_text(outcome_name),
    analysis_group = clean_text(analysis_group),
    meta_analysis_role = clean_text(meta_analysis_role),
    verification_status = normalize_verification_status(
      verification_status
    ),
    human_include_in_planned_analysis = normalize_yes_no(
      human_include_in_planned_analysis
    )
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
    "No row is marked Yes in human_include_in_planned_analysis.",
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
    "Human gate incomplete: ",
    nrow(bad_status),
    " Human-approved row(s) are not Verified/Corrected.",
    call. = FALSE
  )
}

if (
  any(
    is.na(
      approved$analysis_group
    )
  )
) {
  stop(
    "Human-approved row(s) are missing analysis_group.",
    call. = FALSE
  )
}

if (
  any(
    is.na(
      approved$meta_analysis_role
    )
  )
) {
  stop(
    "Human-approved row(s) are missing meta_analysis_role.",
    call. = FALSE
  )
}

if (
  anyDuplicated(
    approved$outcome_row_id
  ) > 0L
) {
  stop(
    "Duplicate outcome_row_id detected among Human-approved rows.",
    call. = FALSE
  )
}


# ============================================================
# 7. Resolve Human-verified authoritative values
# ============================================================

human_prefix <- "human_verified_"

human_columns <- grep(
  paste0(
    "^",
    human_prefix
  ),
  names(approved),
  value = TRUE
)

for (human_col in human_columns) {

  agent_col <- sub(
    paste0(
      "^",
      human_prefix
    ),
    "",
    human_col
  )

  final_col <- paste0(
    "final_",
    agent_col
  )

  if (
    agent_col %in% names(approved)
  ) {
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

common_fields <- c(
  "outcome_definition",
  "timepoint",
  "data_type",
  "intervention_events",
  "intervention_total",
  "comparator_events",
  "comparator_total",
  "intervention_mean",
  "intervention_sd",
  "intervention_n",
  "comparator_mean",
  "comparator_sd",
  "comparator_n",
  "unit",
  "effect_measure",
  "effect_estimate",
  "lower_ci",
  "upper_ci",
  "adjusted",
  "adjustment_variables",
  "source_location"
)

for (field in common_fields) {

  final_field <- paste0(
    "final_",
    field
  )

  if (
    !final_field %in% names(approved)
  ) {

    if (
      field %in% names(approved)
    ) {
      approved[[final_field]] <- clean_text(
        approved[[field]]
      )
    } else {
      approved[[final_field]] <- NA_character_
    }
  }
}


# ============================================================
# 8. Add authoritative study-design information
# ============================================================

approved$final_study_design <- NA_character_

if (
  !is.null(study) &&
    "screening_id" %in% names(study)
) {

  study$screening_id <- clean_text(
    study$screening_id
  )

  design_source <- NULL

  if (
    "human_verified_study_design" %in% names(study) &&
      "study_design" %in% names(study)
  ) {
    design_source <- authoritative_value(
      study$human_verified_study_design,
      study$study_design
    )
  } else if (
    "human_verified_study_design" %in% names(study)
  ) {
    design_source <- clean_text(
      study$human_verified_study_design
    )
  } else if (
    "study_design" %in% names(study)
  ) {
    design_source <- clean_text(
      study$study_design
    )
  }

  if (!is.null(design_source)) {

    study_design_lookup <- tibble::tibble(
      screening_id = study$screening_id,
      study_design_from_study_sheet = design_source
    ) |>
      dplyr::distinct(
        screening_id,
        .keep_all = TRUE
      )

    approved <- approved |>
      dplyr::left_join(
        study_design_lookup,
        by = "screening_id"
      )

    approved$final_study_design <- clean_text(
      approved$study_design_from_study_sheet
    )

    approved$study_design_from_study_sheet <- NULL
  }
}

approved$design_class <- infer_design_class(
  approved$final_study_design
)


# ============================================================
# 9. Sanitize data type and effect measure
# ============================================================

approved$final_data_type <- normalize_data_type(
  approved$final_data_type
)

raw_effect_measure <- approved$final_effect_measure

approved$final_effect_measure <- normalize_effect_measure(
  approved$final_effect_measure
)

approved$effect_measure_source <- ifelse(
  !is.na(
    approved$final_effect_measure
  ),
  "verified_explicit",
  NA_character_
)

# Universal default for COMPLETE raw dichotomous data:
# RR is used only when no valid explicit measure exists.
# This is recorded transparently in effect_measure_source.
for (i in seq_len(nrow(approved))) {

  data_type_i <- safe_chr(
    approved$final_data_type[[i]]
  )

  measure_i <- safe_chr(
    approved$final_effect_measure[[i]]
  )

  if (
    data_type_i == "dichotomous" &&
      measure_i == ""
  ) {
    approved$final_effect_measure[[i]] <- "RR"
    approved$effect_measure_source[[i]] <- "default_RR_for_raw_dichotomous"
  }
}


# ============================================================
# 10. Remove structurally non-applicable placeholder fields
# ============================================================
# This is deliberately semantic rather than value-specific.
#
# Example:
# A reported-effect row does not use raw event-count or mean/SD
# fields. Therefore arbitrary placeholder values in those cells are
# removed automatically, regardless of whether the placeholder was
# "64", "999", or something else.
# ============================================================

event_fields <- c(
  "final_intervention_events",
  "final_intervention_total",
  "final_comparator_events",
  "final_comparator_total"
)

continuous_fields <- c(
  "final_intervention_mean",
  "final_intervention_sd",
  "final_intervention_n",
  "final_comparator_mean",
  "final_comparator_sd",
  "final_comparator_n",
  "final_unit"
)

reported_effect_fields <- c(
  "final_effect_estimate",
  "final_lower_ci",
  "final_upper_ci"
)

sanitized_cell_count <- 0L

for (i in seq_len(nrow(approved))) {

  data_type_i <- safe_chr(
    approved$final_data_type[[i]]
  )

  if (
    data_type_i == "reported_effect"
  ) {

    for (field in c(
      event_fields,
      continuous_fields
    )) {

      if (
        field %in% names(approved) &&
          !is.na(
            clean_text(
              approved[[field]][[i]]
            )
          )
      ) {
        sanitized_cell_count <- sanitized_cell_count + 1L
      }

      if (
        field %in% names(approved)
      ) {
        approved[[field]][[i]] <- NA_character_
      }
    }
  }

  if (
    data_type_i == "dichotomous"
  ) {

    for (field in continuous_fields) {

      if (
        field %in% names(approved) &&
          !is.na(
            clean_text(
              approved[[field]][[i]]
            )
          )
      ) {
        sanitized_cell_count <- sanitized_cell_count + 1L
      }

      if (
        field %in% names(approved)
      ) {
        approved[[field]][[i]] <- NA_character_
      }
    }

    # If the original effect-measure field was invalid, any associated
    # reported estimate/CI cells are not trustworthy for this raw-count
    # analysis row and are cleared.
    original_measure_valid <- !is.na(
      normalize_effect_measure(
        raw_effect_measure[[i]]
      )
    )

    if (!original_measure_valid) {

      for (field in reported_effect_fields) {

        if (
          field %in% names(approved) &&
            !is.na(
              clean_text(
                approved[[field]][[i]]
              )
            )
        ) {
          sanitized_cell_count <- sanitized_cell_count + 1L
        }

        if (
          field %in% names(approved)
        ) {
          approved[[field]][[i]] <- NA_character_
        }
      }
    }
  }
}


# ============================================================
# 11. Validate and classify meta-analysis input
# ============================================================

approved$meta_input_type <- NA_character_
approved$input_validation_status <- "PASS"
approved$input_validation_note <- NA_character_

for (i in seq_len(nrow(approved))) {

  dt <- safe_chr(
    approved$final_data_type[[i]]
  )

  measure <- safe_chr(
    approved$final_effect_measure[[i]]
  )

  if (
    dt == "dichotomous"
  ) {

    a <- safe_numeric(
      approved$final_intervention_events[[i]]
    )

    n1 <- safe_numeric(
      approved$final_intervention_total[[i]]
    )

    c_event <- safe_numeric(
      approved$final_comparator_events[[i]]
    )

    n0 <- safe_numeric(
      approved$final_comparator_total[[i]]
    )

    valid_counts <- (
      length(a) == 1L &&
        length(n1) == 1L &&
        length(c_event) == 1L &&
        length(n0) == 1L &&
        is.finite(a) &&
        is.finite(n1) &&
        is.finite(c_event) &&
        is.finite(n0) &&
        a >= 0 &&
        c_event >= 0 &&
        n1 > 0 &&
        n0 > 0 &&
        a <= n1 &&
        c_event <= n0
    )

    if (!valid_counts) {
      approved$input_validation_status[[i]] <- "FAIL"
      approved$input_validation_note[[i]] <- paste0(
        "Invalid raw dichotomous counts/denominators for ",
        approved$screening_id[[i]]
      )
    } else if (
      !measure %in% c(
        "RR",
        "OR",
        "RD"
      )
    ) {
      approved$input_validation_status[[i]] <- "FAIL"
      approved$input_validation_note[[i]] <- paste0(
        "Unsupported raw dichotomous effect measure: ",
        measure
      )
    } else {
      approved$meta_input_type[[i]] <- "raw_dichotomous"
    }
  }

  if (
    dt == "reported_effect"
  ) {

    est <- safe_numeric(
      approved$final_effect_estimate[[i]]
    )

    lower <- safe_numeric(
      approved$final_lower_ci[[i]]
    )

    upper <- safe_numeric(
      approved$final_upper_ci[[i]]
    )

    valid_measure <- measure %in% c(
      "RR",
      "OR",
      "HR",
      "RD",
      "MD",
      "SMD"
    )

    valid_ci <- (
      length(est) == 1L &&
        length(lower) == 1L &&
        length(upper) == 1L &&
        is.finite(est) &&
        is.finite(lower) &&
        is.finite(upper) &&
        upper > lower
    )

    if (
      measure %in% c(
        "RR",
        "OR",
        "HR"
      )
    ) {
      valid_ci <- (
        valid_ci &&
          est > 0 &&
          lower > 0 &&
          upper > 0
      )
    }

    if (!valid_measure) {
      approved$input_validation_status[[i]] <- "FAIL"
      approved$input_validation_note[[i]] <- paste0(
        "Reported effect has no supported effect measure for ",
        approved$screening_id[[i]]
      )
    } else if (!valid_ci) {
      approved$input_validation_status[[i]] <- "FAIL"
      approved$input_validation_note[[i]] <- paste0(
        "Invalid reported effect estimate/CI for ",
        approved$screening_id[[i]]
      )
    } else {
      approved$meta_input_type[[i]] <- "reported_effect"
    }
  }

  if (
    dt == "continuous"
  ) {

    m1 <- safe_numeric(
      approved$final_intervention_mean[[i]]
    )

    sd1 <- safe_numeric(
      approved$final_intervention_sd[[i]]
    )

    n1 <- safe_numeric(
      approved$final_intervention_n[[i]]
    )

    m0 <- safe_numeric(
      approved$final_comparator_mean[[i]]
    )

    sd0 <- safe_numeric(
      approved$final_comparator_sd[[i]]
    )

    n0 <- safe_numeric(
      approved$final_comparator_n[[i]]
    )

    valid_raw_continuous <- (
      length(m1) == 1L &&
        length(sd1) == 1L &&
        length(n1) == 1L &&
        length(m0) == 1L &&
        length(sd0) == 1L &&
        length(n0) == 1L &&
        is.finite(m1) &&
        is.finite(sd1) &&
        is.finite(n1) &&
        is.finite(m0) &&
        is.finite(sd0) &&
        is.finite(n0) &&
        sd1 >= 0 &&
        sd0 >= 0 &&
        n1 > 0 &&
        n0 > 0
    )

    if (!valid_raw_continuous) {
      approved$input_validation_status[[i]] <- "FAIL"
      approved$input_validation_note[[i]] <- paste0(
        "Invalid raw continuous mean/SD/N data for ",
        approved$screening_id[[i]]
      )
    } else if (
      !measure %in% c(
        "MD",
        "SMD"
      )
    ) {
      approved$input_validation_status[[i]] <- "FAIL"
      approved$input_validation_note[[i]] <- paste0(
        "Continuous outcome requires explicit MD or SMD."
      )
    } else {
      approved$meta_input_type[[i]] <- "raw_continuous"
    }
  }

  if (
    !dt %in% c(
      "dichotomous",
      "reported_effect",
      "continuous"
    )
  ) {
    approved$input_validation_status[[i]] <- "FAIL"
    approved$input_validation_note[[i]] <- paste0(
      "Unsupported or unrecognized final_data_type: ",
      dt
    )
  }
}

bad_input <- approved |>
  dplyr::filter(
    input_validation_status != "PASS"
  )

if (nrow(bad_input) > 0L) {
  stop(
    "R/26 meta-analysis input validation failed for ",
    nrow(bad_input),
    " Human-approved row(s):\n- ",
    paste(
      paste0(
        bad_input$screening_id,
        ": ",
        bad_input$input_validation_note
      ),
      collapse = "\n- "
    ),
    call. = FALSE
  )
}


# ============================================================
# 12. Build BASE groups
# ============================================================

base_groups <- split(
  approved,
  approved$analysis_group
)

base_group_names <- names(
  base_groups
)

if (
  length(base_group_names) == 0L
) {
  stop(
    "No analysis_group values were available after Human Verification.",
    call. = FALSE
  )
}

base_group_info <- purrr::imap_dfr(
  base_groups,
  function(df, group_name) {

    role <- collapse_unique(
      df$meta_analysis_role
    )

    tibble::tibble(
      analysis_group = group_name,
      meta_analysis_role = role,
      role_class = role_class(
        group_name,
        role
      ),
      study_design = all_same_or_mixed(
        df$design_class
      ),
      outcome = collapse_unique(
        df$outcome_name
      ),
      base_study_count = dplyr::n_distinct(
        df$screening_id
      )
    )
  }
)


# ============================================================
# 13. Resolve explicit parent-analysis relation if present
# ============================================================

parent_col <- first_existing_column(
  approved,
  c(
    "parent_analysis_group",
    "analysis_parent_group",
    "base_analysis_group"
  )
)

explicit_parent_lookup <- list()

if (!is.na(parent_col)) {

  for (group_name in base_group_names) {

    vals <- unique(
      clean_text(
        base_groups[[group_name]][[parent_col]]
      )
    )

    vals <- vals[
      !is.na(vals)
    ]

    if (length(vals) > 1L) {
      stop(
        "Analysis group ",
        group_name,
        " contains more than one explicit parent_analysis_group.",
        call. = FALSE
      )
    }

    if (length(vals) == 1L) {
      explicit_parent_lookup[[group_name]] <- vals[[1]]
    }
  }
}


# ============================================================
# 14. Convert BASE groups into FINAL ANALYSIS SETS
# ============================================================
# Standalone / primary / observational groups:
#   final set = own rows
#
# Sensitivity groups:
#   final set = compatible parent primary rows + sensitivity-only rows
# ============================================================

analysis_sets <- list()
analysis_set_metadata <- list()

for (group_name in base_group_names) {

  df <- base_groups[[group_name]]

  role <- collapse_unique(
    df$meta_analysis_role
  )

  class_i <- role_class(
    group_name,
    role
  )

  if (class_i != "Sensitivity") {

    analysis_sets[[group_name]] <- df

    analysis_set_metadata[[group_name]] <- list(
      analysis_set_type = "standalone",
      parent_analysis_group = NA_character_,
      source_analysis_groups = group_name,
      is_composite = "No"
    )

    next
  }

  explicit_parent <- explicit_parent_lookup[[group_name]]

  if (!is.null(explicit_parent)) {

    if (
      !explicit_parent %in% base_group_names
    ) {
      stop(
        "Sensitivity group ",
        group_name,
        " specifies parent_analysis_group '",
        explicit_parent,
        "', but that group does not exist.",
        call. = FALSE
      )
    }

    if (
      !compatible_for_sensitivity(
        df,
        base_groups[[explicit_parent]]
      )
    ) {
      stop(
        "Sensitivity group ",
        group_name,
        " is not outcome/design-compatible with explicit parent ",
        explicit_parent,
        ".",
        call. = FALSE
      )
    }

    parent_group <- explicit_parent

  } else {

    primary_candidates <- base_group_info |>
      dplyr::filter(
        role_class == "Primary"
      ) |>
      dplyr::pull(
        analysis_group
      )

    compatible_candidates <- primary_candidates[
      vapply(
        primary_candidates,
        function(candidate) {
          compatible_for_sensitivity(
            df,
            base_groups[[candidate]]
          )
        },
        FUN.VALUE = logical(1)
      )
    ]

    if (
      length(
        compatible_candidates
      ) == 0L
    ) {
      stop(
        "Sensitivity group ",
        group_name,
        " has no compatible Primary parent. ",
        "Add parent_analysis_group explicitly in the verification workflow.",
        call. = FALSE
      )
    }

    if (
      length(
        compatible_candidates
      ) > 1L
    ) {
      stop(
        "Sensitivity group ",
        group_name,
        " has multiple compatible Primary parents: ",
        paste(
          compatible_candidates,
          collapse = ", "
        ),
        ". Add parent_analysis_group explicitly.",
        call. = FALSE
      )
    }

    parent_group <- compatible_candidates[[1]]
  }

  combined <- dplyr::bind_rows(
    base_groups[[parent_group]],
    df
  )

  combined <- combined |>
    dplyr::distinct(
      outcome_row_id,
      .keep_all = TRUE
    )

  duplicate_studies <- combined |>
    dplyr::count(
      screening_id,
      name = "n_rows"
    ) |>
    dplyr::filter(
      n_rows > 1L
    )

  if (nrow(duplicate_studies) > 0L) {
    stop(
      "Composite sensitivity set ",
      group_name,
      " contains multiple dependent outcome rows from the same study: ",
      paste(
        duplicate_studies$screening_id,
        collapse = ", "
      ),
      ". Explicit dependent-effect handling is required.",
      call. = FALSE
    )
  }

  analysis_sets[[group_name]] <- combined

  analysis_set_metadata[[group_name]] <- list(
    analysis_set_type = "composite_sensitivity",
    parent_analysis_group = parent_group,
    source_analysis_groups = paste(
      c(
        parent_group,
        group_name
      ),
      collapse = " + "
    ),
    is_composite = "Yes"
  )
}


# ============================================================
# 15. Validate final analysis-set compatibility
# ============================================================

manifest <- purrr::imap_dfr(
  analysis_sets,
  function(df, set_name) {

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
      df$design_class
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

    metadata <- analysis_set_metadata[[set_name]]

    next_action <- dplyr::case_when(
      identical(
        effect_measure,
        "Mixed"
      ) ~ paste0(
        "Do not pool yet: effect measures are mixed. ",
        "Harmonize or separate before R/27."
      ),

      identical(
        design,
        "Mixed"
      ) ~ paste0(
        "Do not automatically pool: study designs are mixed. ",
        "Confirm that the mixed-design analysis was explicitly planned."
      ),

      TRUE ~ paste0(
        "Proceed to R/27 after confirming clinical compatibility."
      )
    )

    tibble::tibble(
      analysis_group = set_name,
      meta_analysis_role = role,
      analysis_set_type = metadata$analysis_set_type,
      is_composite = metadata$is_composite,
      parent_analysis_group = metadata$parent_analysis_group,
      source_analysis_groups = metadata$source_analysis_groups,
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
      suggested_model = "Random-effects by default; final choice belongs to the analysis stage",
      combine_with_other_analysis_groups = "NO unless explicitly prespecified and methodologically justified",
      next_action = next_action
    )
  }
)

bad_sets <- manifest |>
  dplyr::filter(
    effect_measure == "Mixed" |
      study_design == "Mixed"
  )

if (nrow(bad_sets) > 0L) {
  warning(
    "One or more analysis sets require methodological review before pooling: ",
    paste(
      bad_sets$analysis_group,
      collapse = ", "
    ),
    call. = FALSE
  )
}


# ============================================================
# 16. Write one canonical CSV per FINAL analysis set
# ============================================================

dataset_files <- character(
  length(
    analysis_sets
  )
)

names(dataset_files) <- names(
  analysis_sets
)

for (set_name in names(analysis_sets)) {

  output_file <- file.path(
    output_data_directory,
    paste0(
      "meta_analysis_ready_",
      safe_file_component(
        set_name
      ),
      ".csv"
    )
  )

  readr::write_csv(
    analysis_sets[[set_name]],
    output_file,
    na = ""
  )

  dataset_files[[set_name]] <- output_file
}

manifest$dataset_file <- unname(
  dataset_files[
    manifest$analysis_group
  ]
)

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
# 17. Human-readable meta-analysis guidance
# ============================================================

guidance_lines <- c(
  "UNIVERSAL META-ANALYSIS READY GUIDANCE",
  "--------------------------------------------------",
  paste0(
    "Human-approved studies: ",
    dplyr::n_distinct(
      approved$screening_id
    )
  ),
  paste0(
    "Final analysis sets: ",
    nrow(
      manifest
    )
  ),
  "",
  "Selection rule:",
  "  human_include_in_planned_analysis == Yes",
  "  AND verification_status == Verified / Corrected",
  "",
  "Analysis-set rule:",
  "  Sensitivity sets include the compatible parent primary set plus sensitivity-only rows.",
  "",
  "Downstream rule:",
  "  Analyze each final analysis set separately.",
  "  Do not combine different evidence designs or effect measures without explicit methodological justification.",
  ""
)

for (i in seq_len(nrow(manifest))) {

  x <- manifest[
    i,
    ,
    drop = FALSE
  ]

  guidance_lines <- c(
    guidance_lines,
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
      "  Set type: ",
      x$analysis_set_type
    ),
    paste0(
      "  Source group(s): ",
      x$source_analysis_groups
    ),
    paste0(
      "  Studies: ",
      x$study_count
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
      "  Next: ",
      x$next_action
    ),
    ""
  )
}


# ============================================================
# 18. Final workbook
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
    "Human-approved outcome rows",
    "Human-approved studies",
    "Final analysis sets",
    "Sanitized non-applicable cells",
    "Selection rule",
    "Authoritative source",
    "Meta-analysis execution"
  ),
  Value = c(
    project_title,
    as.character(
      nrow(
        approved
      )
    ),
    as.character(
      dplyr::n_distinct(
        approved$screening_id
      )
    ),
    as.character(
      length(
        analysis_sets
      )
    ),
    as.character(
      sanitized_cell_count
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
  "Base_Group_Summary",
  base_group_info
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

used_sheet_names <- c(
  "Finalization_Summary",
  "Base_Group_Summary",
  "Meta_Analysis_Manifest",
  "All_Approved_Outcomes"
)

for (set_name in names(analysis_sets)) {

  base_sheet_name <- substr(
    safe_file_component(
      set_name
    ),
    1,
    31
  )

  if (
    base_sheet_name == ""
  ) {
    base_sheet_name <- "analysis_set"
  }

  sheet_name <- base_sheet_name
  suffix <- 1L

  while (
    sheet_name %in% used_sheet_names
  ) {
    suffix <- suffix + 1L

    sheet_name <- substr(
      paste0(
        base_sheet_name,
        "_",
        suffix
      ),
      1,
      31
    )
  }

  used_sheet_names <- c(
    used_sheet_names,
    sheet_name
  )

  add_table_sheet(
    wb,
    sheet_name,
    analysis_sets[[set_name]]
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
# 19. Output validation
# ============================================================

required_outputs <- c(
  all_verified_csv,
  manifest_csv,
  final_workbook,
  unname(
    dataset_files
  )
)

bad_outputs <- required_outputs[
  !file.exists(
    required_outputs
  ) |
    is.na(
      file.info(
        required_outputs
      )$size
    ) |
    file.info(
      required_outputs
    )$size <= 0
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
# 20. Report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/26 — Human-Verified Meta-analysis-ready Finalization",
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
  "HUMAN GATE",
  "--------------------------------------------------",
  "human_include_in_planned_analysis == Yes",
  "verification_status must be Verified or Corrected",
  "",
  "FINALIZATION",
  "--------------------------------------------------",
  paste0(
    "Human-approved rows: ",
    nrow(
      approved
    )
  ),
  paste0(
    "Human-approved studies: ",
    dplyr::n_distinct(
      approved$screening_id
    )
  ),
  paste0(
    "Sanitized non-applicable cells: ",
    sanitized_cell_count
  ),
  paste0(
    "Final analysis sets: ",
    length(
      analysis_sets
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
  "Run R/27 using the regenerated meta_analysis_manifest.csv and final analysis-set datasets."
)

writeLines(
  report_lines,
  report_file,
  useBytes = TRUE
)


# ============================================================
# 21. Console
# ============================================================

cat("\n")
cat("============================================================\n")
cat("R/26 — Universal Meta-analysis-ready Finalization\n")
cat("============================================================\n\n")

cat(
  "Human-approved studies: ",
  dplyr::n_distinct(
    approved$screening_id
  ),
  "\n",
  sep = ""
)

cat(
  "Sanitized non-applicable cells: ",
  sanitized_cell_count,
  "\n",
  sep = ""
)

cat(
  "Final analysis sets: ",
  length(
    analysis_sets
  ),
  "\n\n",
  sep = ""
)

for (i in seq_len(nrow(manifest))) {

  x <- manifest[
    i,
    ,
    drop = FALSE
  ]

  cat(
    "[",
    i,
    "] ",
    x$analysis_group,
    "\n",
    sep = ""
  )

  cat(
    "    Type: ",
    x$analysis_set_type,
    "\n",
    sep = ""
  )

  cat(
    "    Source: ",
    x$source_analysis_groups,
    "\n",
    sep = ""
  )

  cat(
    "    Studies: ",
    x$study_count,
    "\n",
    sep = ""
  )

  cat(
    "    Design: ",
    safe_chr(
      x$study_design
    ),
    "\n",
    sep = ""
  )

  cat(
    "    Effect measure: ",
    safe_chr(
      x$effect_measure
    ),
    "\n\n",
    sep = ""
  )
}

cat(
  "PASS: R/26 finalized Human-approved analysis SETS.\n"
)

cat(
  "PASS: Composite sensitivity sets were resolved before R/27.\n"
)

cat(
  "NEXT: Re-run R/27 with the regenerated manifest and datasets.\n"
)

invisible(
  list(
    status = "PASS",
    approved_outcomes = approved,
    base_groups = base_groups,
    analysis_sets = analysis_sets,
    manifest = manifest,
    workbook = final_workbook,
    report = report_file
  )
)
