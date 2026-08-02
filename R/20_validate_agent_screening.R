# ============================================================
# Universal SR-MA Agent Toolkit
# R/20_validate_agent_screening.R
# ============================================================

required_packages <- c("yaml", "openxlsx", "readr", "dplyr")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))
]

if (length(missing_packages) > 0L) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "))
}

source("R/utils/config_utils.R")
source("R/utils/path_utils.R")

config <- load_project_config(quiet = FALSE)
paths <- build_project_paths(config, create_directories = TRUE)
agent_document <- yaml::read_yaml("config/agent_screening.yml")
agent_config <- agent_document$agent_screening

output_workbook <- file.path(
  paths$screening_results_directory,
  paste0(agent_config$output$workbook_basename, ".xlsx")
)

decisions_file <- file.path(
  paths$logs_directory,
  paste0(agent_config$output$decisions_basename, ".csv")
)

if (!file.exists(output_workbook)) stop("Agent workbook not found: ", output_workbook)
if (!file.exists(decisions_file)) stop("Decision log not found: ", decisions_file)

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

decisions <- readr::read_csv(
  decisions_file,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

allowed_decisions <- get_screening_decisions(config)
reasons <- get_exclusion_reasons(config)

errors <- character(0)

invalid_decisions <- reviewer_1[
  !is.na(reviewer_1$decision) &
    nzchar(reviewer_1$decision) &
    !reviewer_1$decision %in% allowed_decisions,
  ,
  drop = FALSE
]

if (nrow(invalid_decisions) > 0L) {
  errors <- c(errors, "Reviewer_1 contains invalid decision values.")
}

excluded_rows <- reviewer_1$decision == "Exclude"
excluded_rows[is.na(excluded_rows)] <- FALSE

invalid_exclusion_reasons <- reviewer_1[
  excluded_rows &
    (
      is.na(reviewer_1$exclusion_reason) |
      !reviewer_1$exclusion_reason %in% reasons$label
    ),
  ,
  drop = FALSE
]

if (nrow(invalid_exclusion_reasons) > 0L) {
  errors <- c(errors, "Excluded records contain invalid or missing exclusion reasons.")
}

nonexclude_with_reason <- reviewer_1[
  !excluded_rows &
    !is.na(reviewer_1$exclusion_reason) &
    nzchar(reviewer_1$exclusion_reason),
  ,
  drop = FALSE
]

if (nrow(nonexclude_with_reason) > 0L) {
  errors <- c(errors, "Include/Unclear rows contain exclusion reasons.")
}

reviewer_2_decision_values <- reviewer_2$decision[
  !is.na(reviewer_2$decision) & nzchar(reviewer_2$decision)
]

if (length(reviewer_2_decision_values) > 0L) {
  errors <- c(errors, "Reviewer_2 is not blank; human-review preservation check failed.")
}

if (anyDuplicated(reviewer_1$screening_id) > 0L) {
  errors <- c(errors, "Reviewer_1 contains duplicated screening IDs.")
}

if (length(errors) > 0L) {
  stop(
    "Agent screening validation failed:\n- ",
    paste(errors, collapse = "\n- ")
  )
}

cat("\nAgent screening validation passed.\n")
cat("Reviewer_1 records:", nrow(reviewer_1), "\n")
cat("Logged decisions:", nrow(decisions), "\n")
cat("Reviewer_2 remains blank: TRUE\n")
