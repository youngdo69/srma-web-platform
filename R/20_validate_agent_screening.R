# ============================================================
# Universal SR-MA Agent Toolkit
# R/20_validate_agent_screening.R
#
# TASK-008 — Title/Abstract Screening Verification
#
# Validation scope
# ------------------------------------------------------------
# 1. Validate Agent Reviewer 1 decisions and audit log
# 2. Confirm one-to-one agreement between workbook and decision log
# 3. Confirm no agent-screening failures
# 4. Confirm Human Reviewer 2 remains untouched
# 5. Confirm human adjudication fields remain available and unfilled
# 6. Preserve the three-role screening workflow:
#      Agent Reviewer 1
#      Human Reviewer 2
#      Human Adjudicator
# ============================================================


# ============================================================
# 1. Required packages
# ============================================================

required_packages <- c(
  "yaml",
  "openxlsx",
  "readr",
  "dplyr"
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
    "Missing packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}


`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}


# ============================================================
# 2. Canonical configuration and paths
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

agent_config_file <- "config/agent_screening.yml"

if (!file.exists(agent_config_file)) {
  stop(
    "Agent-screening config not found: ",
    agent_config_file,
    call. = FALSE
  )
}

agent_document <- yaml::read_yaml(
  agent_config_file
)

agent_config <- agent_document$agent_screening

if (is.null(agent_config) || !is.list(agent_config)) {
  stop(
    "Invalid agent_screening configuration.",
    call. = FALSE
  )
}

agent_reviewer_name <-
  agent_config$reviewer$name %||%
  "Agent Reviewer 1"

human_reviewer_2_name <- get_reviewer_name(
  config,
  reviewer = "reviewer_2",
  fallback = "Human Reviewer 2"
)

human_adjudicator_name <- get_reviewer_name(
  config,
  reviewer = "third_reviewer",
  fallback = "Human Adjudicator"
)


# ============================================================
# 3. Agent-screening outputs
# ============================================================

output_workbook <- file.path(
  paths$screening_results_directory,
  paste0(
    agent_config$output$workbook_basename,
    ".xlsx"
  )
)

decisions_file <- file.path(
  paths$logs_directory,
  paste0(
    agent_config$output$decisions_basename,
    ".csv"
  )
)

failures_file <- file.path(
  paths$logs_directory,
  paste0(
    agent_config$output$failures_basename,
    ".csv"
  )
)

required_files <- c(
  agent_workbook = output_workbook,
  decision_log = decisions_file,
  failure_log = failures_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    "Required TASK-008 output file(s) are missing:\n- ",
    paste(missing_files, collapse = "\n- "),
    call. = FALSE
  )
}


# ============================================================
# 4. Read workbook and logs
# ============================================================

workbook_sheets <- openxlsx::getSheetNames(
  output_workbook
)

required_sheets <- c(
  "Reviewer_1",
  "Reviewer_2",
  "Conflict_Resolution"
)

missing_sheets <- setdiff(
  required_sheets,
  workbook_sheets
)

if (length(missing_sheets) > 0L) {
  stop(
    "Agent workbook is missing required sheet(s): ",
    paste(missing_sheets, collapse = ", "),
    call. = FALSE
  )
}

reviewer_1 <- openxlsx::readWorkbook(
  output_workbook,
  sheet = "Reviewer_1",
  startRow = 4,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

reviewer_2 <- openxlsx::readWorkbook(
  output_workbook,
  sheet = "Reviewer_2",
  startRow = 4,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

conflicts <- openxlsx::readWorkbook(
  output_workbook,
  sheet = "Conflict_Resolution",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

decisions <- readr::read_csv(
  decisions_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
)

failures <- readr::read_csv(
  failures_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character()
  )
)


# ============================================================
# 5. Validation helpers
# ============================================================

is_filled <- function(x) {
  !is.na(x) &
    nzchar(trimws(as.character(x)))
}

allowed_decisions <- get_screening_decisions(
  config
)

reasons <- get_exclusion_reasons(
  config
)

errors <- character(0)


# ============================================================
# 6. Structural identity checks
# ============================================================

if (nrow(reviewer_1) != nrow(reviewer_2)) {
  errors <- c(
    errors,
    "Reviewer_1 and Reviewer_2 row counts differ."
  )
}

if (nrow(reviewer_1) != nrow(conflicts)) {
  errors <- c(
    errors,
    "Reviewer_1 and Conflict_Resolution row counts differ."
  )
}

if (anyDuplicated(reviewer_1$screening_id) > 0L) {
  errors <- c(
    errors,
    "Reviewer_1 contains duplicated screening IDs."
  )
}

if (anyDuplicated(reviewer_2$screening_id) > 0L) {
  errors <- c(
    errors,
    "Reviewer_2 contains duplicated screening IDs."
  )
}

if (
  !identical(
    as.character(reviewer_1$screening_id),
    as.character(reviewer_2$screening_id)
  )
) {
  errors <- c(
    errors,
    "Reviewer_1 and Reviewer_2 screening IDs are not aligned."
  )
}

if (
  !identical(
    as.character(reviewer_1$screening_id),
    as.character(conflicts$screening_id)
  )
) {
  errors <- c(
    errors,
    "Conflict_Resolution screening IDs are not aligned with Reviewer_1."
  )
}


# ============================================================
# 7. Agent Reviewer 1 validation
# ============================================================

invalid_decisions <- reviewer_1[
  is_filled(reviewer_1$decision) &
    !reviewer_1$decision %in% allowed_decisions,
  ,
  drop = FALSE
]

if (nrow(invalid_decisions) > 0L) {
  errors <- c(
    errors,
    "Reviewer_1 contains invalid decision values."
  )
}

unscreened_agent_rows <- reviewer_1[
  !is_filled(reviewer_1$decision),
  ,
  drop = FALSE
]

if (nrow(unscreened_agent_rows) > 0L) {
  errors <- c(
    errors,
    paste0(
      "Reviewer_1 contains ",
      nrow(unscreened_agent_rows),
      " unscreened record(s)."
    )
  )
}

excluded_rows <- reviewer_1$decision == "Exclude"
excluded_rows[is.na(excluded_rows)] <- FALSE

invalid_exclusion_reasons <- reviewer_1[
  excluded_rows &
    (
      !is_filled(reviewer_1$exclusion_reason) |
      !reviewer_1$exclusion_reason %in% reasons$label
    ),
  ,
  drop = FALSE
]

if (nrow(invalid_exclusion_reasons) > 0L) {
  invalid_reason_values <- unique(
    ifelse(
      is.na(invalid_exclusion_reasons$exclusion_reason),
      "<missing>",
      as.character(invalid_exclusion_reasons$exclusion_reason)
    )
  )

  errors <- c(
    errors,
    paste0(
      "Excluded Reviewer_1 records contain invalid or missing exclusion reasons. ",
      "Affected records: ",
      nrow(invalid_exclusion_reasons),
      ". Observed value(s): ",
      paste(invalid_reason_values, collapse = " | ")
    )
  )
}

nonexclude_with_reason <- reviewer_1[
  !excluded_rows &
    is_filled(reviewer_1$exclusion_reason),
  ,
  drop = FALSE
]

if (nrow(nonexclude_with_reason) > 0L) {
  errors <- c(
    errors,
    "Include/Unclear Reviewer_1 rows contain exclusion reasons."
  )
}

reviewer_identity_values <- unique(
  trimws(
    as.character(
      reviewer_1$reviewer[
        is_filled(reviewer_1$reviewer)
      ]
    )
  )
)

if (
  length(reviewer_identity_values) != 1L ||
  !identical(
    reviewer_identity_values[[1]],
    agent_reviewer_name
  )
) {
  errors <- c(
    errors,
    "Reviewer_1 identity does not match the configured Agent Reviewer 1."
  )
}


# ============================================================
# 8. Decision-log reconciliation
# ============================================================

valid_logged_decisions <- decisions[
  decisions$validation_status == "valid",
  ,
  drop = FALSE
]

if (anyDuplicated(valid_logged_decisions$screening_id) > 0L) {
  errors <- c(
    errors,
    "Decision log contains duplicated valid screening IDs."
  )
}

if (nrow(valid_logged_decisions) != nrow(reviewer_1)) {
  errors <- c(
    errors,
    paste0(
      "Valid decision-log count (",
      nrow(valid_logged_decisions),
      ") does not equal Reviewer_1 record count (",
      nrow(reviewer_1),
      ")."
    )
  )
}

log_match <- merge(
  reviewer_1[
    ,
    c(
      "screening_id",
      "decision",
      "exclusion_reason",
      "confidence"
    ),
    drop = FALSE
  ],
  valid_logged_decisions[
    ,
    c(
      "screening_id",
      "decision",
      "exclusion_reason",
      "confidence"
    ),
    drop = FALSE
  ],
  by = "screening_id",
  suffixes = c(
    "_workbook",
    "_log"
  ),
  all = TRUE
)

if (nrow(log_match) != nrow(reviewer_1)) {
  errors <- c(
    errors,
    "Workbook and decision log do not contain the same screening IDs."
  )
} else {
  compare_field <- function(left, right) {
    left <- ifelse(is.na(left), "", trimws(as.character(left)))
    right <- ifelse(is.na(right), "", trimws(as.character(right)))
    identical(left, right)
  }

  if (
    !compare_field(
      log_match$decision_workbook,
      log_match$decision_log
    )
  ) {
    errors <- c(
      errors,
      "Reviewer_1 workbook decisions do not match the decision log."
    )
  }

  if (
    !compare_field(
      log_match$exclusion_reason_workbook,
      log_match$exclusion_reason_log
    )
  ) {
    errors <- c(
      errors,
      "Reviewer_1 exclusion reasons do not match the decision log."
    )
  }

  if (
    !compare_field(
      log_match$confidence_workbook,
      log_match$confidence_log
    )
  ) {
    errors <- c(
      errors,
      "Reviewer_1 confidence values do not match the decision log."
    )
  }
}


# ============================================================
# 9. Failure-log validation
# ============================================================

failure_count <- if (
  nrow(failures) == 0L
) {
  0L
} else if (
  "screening_id" %in% names(failures)
) {
  sum(
    is_filled(
      failures$screening_id
    )
  )
} else {
  nrow(failures)
}

if (failure_count > 0L) {
  errors <- c(
    errors,
    paste0(
      failure_count,
      " agent-screening failure(s) remain."
    )
  )
}


# ============================================================
# 10. Human Reviewer 2 preservation
# ============================================================

reviewer_2_human_fields <- c(
  "decision",
  "exclusion_reason",
  "confidence",
  "notes",
  "screened_at"
)

filled_reviewer_2_fields <- vapply(
  reviewer_2_human_fields,
  function(field_name) {
    if (!field_name %in% names(reviewer_2)) {
      return(TRUE)
    }

    any(
      is_filled(
        reviewer_2[[field_name]]
      )
    )
  },
  logical(1)
)

if (any(filled_reviewer_2_fields)) {
  errors <- c(
    errors,
    paste0(
      "Human Reviewer 2 fields were modified during Agent Reviewer 1 screening: ",
      paste(
        reviewer_2_human_fields[
          filled_reviewer_2_fields
        ],
        collapse = ", "
      )
    )
  )
}

reviewer_2_identity_values <- unique(
  trimws(
    as.character(
      reviewer_2$reviewer[
        is_filled(reviewer_2$reviewer)
      ]
    )
  )
)

if (length(reviewer_2_identity_values) != 1L) {
  errors <- c(
    errors,
    "Reviewer_2 must contain one consistent human reviewer identity."
  )
} else if (
  identical(
    tolower(reviewer_2_identity_values[[1]]),
    tolower(agent_reviewer_name)
  )
) {
  errors <- c(
    errors,
    "Reviewer_2 identity must remain distinct from Agent Reviewer 1."
  )
}


# ============================================================
# 11. Human adjudication preservation
# ============================================================

required_conflict_columns <- c(
  "screening_id",
  "reviewer_1_decision",
  "reviewer_2_decision",
  "agreement_status",
  "conflict_status",
  "final_decision",
  "final_exclusion_reason",
  "adjudicator",
  "resolution_notes",
  "resolved_at"
)

missing_conflict_columns <- setdiff(
  required_conflict_columns,
  names(conflicts)
)

if (length(missing_conflict_columns) > 0L) {
  errors <- c(
    errors,
    paste0(
      "Conflict_Resolution sheet is missing columns: ",
      paste(
        missing_conflict_columns,
        collapse = ", "
      )
    )
  )
} else {
  human_adjudication_fields <- c(
    "final_decision",
    "final_exclusion_reason",
    "resolution_notes",
    "resolved_at"
  )

  modified_adjudication_fields <- vapply(
    human_adjudication_fields,
    function(field_name) {
      any(
        is_filled(
          conflicts[[field_name]]
        )
      )
    },
    logical(1)
  )

  if (any(modified_adjudication_fields)) {
    errors <- c(
      errors,
      paste0(
        "Human adjudication fields were modified before adjudication: ",
        paste(
          human_adjudication_fields[
            modified_adjudication_fields
          ],
          collapse = ", "
        )
      )
    )
  }

  adjudicator_values <- unique(
    trimws(
      as.character(
        conflicts$adjudicator[
          is_filled(conflicts$adjudicator)
        ]
      )
    )
  )

  if (length(adjudicator_values) != 1L) {
    errors <- c(
      errors,
      "Conflict_Resolution must contain one consistent human adjudicator identity."
    )
  } else if (
    identical(
      tolower(adjudicator_values[[1]]),
      tolower(agent_reviewer_name)
    )
  ) {
    errors <- c(
      errors,
      "Human adjudicator identity must remain distinct from Agent Reviewer 1."
    )
  }
}


# ============================================================
# 12. Final result
# ============================================================

if (length(errors) > 0L) {
  stop(
    "TASK-008 title/abstract screening validation failed:\n- ",
    paste(errors, collapse = "\n- "),
    call. = FALSE
  )
}

cat("\n====================================================\n")
cat("TASK-008 Title/Abstract Screening Validation\n")
cat("====================================================\n\n")
cat("Agent Reviewer 1:", agent_reviewer_name, "\n")
cat("Human Reviewer 2:", human_reviewer_2_name, "\n")
cat("Human Adjudicator:", human_adjudicator_name, "\n")
cat("Reviewer_1 records:", nrow(reviewer_1), "\n")
cat("Valid logged decisions:", nrow(valid_logged_decisions), "\n")
cat("Agent failures:", failure_count, "\n")
cat("Reviewer_2 preserved for human review: TRUE\n")
cat("Adjudication fields preserved for human resolution: TRUE\n")
cat("\nPASS: TASK-008 Agent Reviewer 1 screening verification completed successfully.\n")
cat("\nNext step:\n")
cat("1. Human Reviewer 2 completes Reviewer_2 independently.\n")
cat("2. Human adjudicator resolves only disagreements/conflicts.\n")
cat("3. Final title/abstract screening completion is verified after human review.\n")
