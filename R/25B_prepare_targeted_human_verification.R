# ============================================================
# Universal SR-MA Agent Toolkit
# R/25B_prepare_targeted_human_verification.R
#
# TASK-014 preparation helper
#
# Workflow position
# ------------------------------------------------------------
# TASK-013 — R/25 Multimodal Agent Extraction
#      ↓
# OPTIONAL — R/25A Research Scope Advisory
#      ↓
# R/25B Targeted Human Verification Workbook
#      ↓
# Human reviewer verifies targeted studies/outcome rows
#      ↓
# R/26 finalize_human_verified_extraction.R
#
# Purpose
# ------------------------------------------------------------
# 1. Read R/25A advisory outputs.
# 2. Automatically identify:
#      - Primary meta-analysis studies
#      - Sensitivity studies
#      - Secondary observational studies
#      - Audit-only studies (e.g., overlap/duplicate-cohort check)
# 3. Read R/25 Agent extraction outputs.
# 4. Extract only the studies/outcome rows relevant to the
#    Advisory-recommended meta-analysis question.
# 5. Create a focused Human Verification workbook.
# 6. Preserve Agent values unchanged and add blank human-verification
#    fields for TASK-014.
#
# Important
# ------------------------------------------------------------
# - Study IDs are NOT hard-coded.
# - R/25B is connected to R/25A via:
#      research_scope_advisory.json
#      candidate_meta_analysis_sets.csv
#      outcome_harmonization.csv
#      study_roles.csv
# - R/25B does NOT alter systematic-review inclusion.
# - R/25B does NOT perform Human Verification.
# - R/25B does NOT modify Agent extraction values.
# - R/25B does NOT perform meta-analysis.
# - Runner integration is not performed here, but the script is
#   intentionally non-interactive and runner-ready.
# ============================================================


# ============================================================
# 1. Required packages
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "jsonlite",
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
    "Missing required packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}


# ============================================================
# 2. Helpers
# ============================================================

clean_text <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\u00A0", " ")
  x <- stringr::str_squish(x)
  x[is.na(x) | x == ""] <- NA_character_
  x
}

safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1]])) {
    return("")
  }
  as.character(x[[1]])
}

now_text <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

as_chr_vec <- function(x) {
  if (is.null(x)) {
    return(character(0))
  }

  y <- as.character(
    unlist(
      x,
      recursive = TRUE,
      use.names = FALSE
    )
  )

  y <- trimws(y)

  unique(
    y[
      !is.na(y) &
        nzchar(y)
    ]
  )
}

split_pipe_ids <- function(x) {
  values <- clean_text(x)

  if (length(values) == 0L || all(is.na(values))) {
    return(character(0))
  }

  values <- values[!is.na(values)]

  ids <- unlist(
    strsplit(
      values,
      "\\s*\\|\\s*"
    ),
    use.names = FALSE
  )

  ids <- trimws(ids)

  unique(
    ids[
      nzchar(ids)
    ]
  )
}

extract_screening_ids <- function(x) {
  x <- as_chr_vec(x)

  if (length(x) == 0L) {
    return(character(0))
  }

  hits <- unlist(
    regmatches(
      x,
      gregexpr(
        "SCR_[0-9]+",
        x,
        perl = TRUE
      )
    ),
    use.names = FALSE
  )

  unique(
    hits[
      nzchar(hits)
    ]
  )
}

normalize_keyword <- function(x) {
  x <- tolower(safe_chr(x))
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- trimws(x)
  x
}

keyword_tokens <- function(x) {
  x <- normalize_keyword(x)

  if (!nzchar(x)) {
    return(character(0))
  }

  tokens <- unlist(
    strsplit(x, "\\s+"),
    use.names = FALSE
  )

  stop_words <- c(
    "the", "and", "or", "of", "in", "within", "after", "before",
    "postoperative", "perioperative", "defined", "stage",
    "adult", "adults", "cardiac", "surgery", "days", "day"
  )

  unique(
    tokens[
      nzchar(tokens) &
        !tokens %in% stop_words
    ]
  )
}

text_similarity_score <- function(a, b) {
  a_tokens <- keyword_tokens(a)
  b_tokens <- keyword_tokens(b)

  if (length(a_tokens) == 0L || length(b_tokens) == 0L) {
    return(0)
  }

  length(
    intersect(
      a_tokens,
      b_tokens
    )
  ) /
    max(
      1L,
      length(
        union(
          a_tokens,
          b_tokens
        )
      )
    )
}

read_character_csv <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  )
}


# ============================================================
# 3. Canonical project context
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
# 4. Paths
# ============================================================

extraction_data_directory <- file.path(
  paths$project_root,
  "data",
  "processed",
  "extraction"
)

advisory_directory <- file.path(
  paths$project_root,
  "results",
  "extraction",
  "research_scope_advisory"
)

targeted_directory <- file.path(
  paths$project_root,
  "results",
  "extraction",
  "targeted_human_verification"
)

dir.create(
  targeted_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

advisory_json_file <- file.path(
  advisory_directory,
  "research_scope_advisory.json"
)

meta_sets_file <- file.path(
  advisory_directory,
  "candidate_meta_analysis_sets.csv"
)

harmonization_file <- file.path(
  advisory_directory,
  "outcome_harmonization.csv"
)

study_roles_file <- file.path(
  advisory_directory,
  "study_roles.csv"
)

study_agent_file <- file.path(
  extraction_data_directory,
  "study_characteristics_agent.csv"
)

group_agent_file <- file.path(
  extraction_data_directory,
  "group_characteristics_agent.csv"
)

outcome_agent_file <- file.path(
  extraction_data_directory,
  "outcome_data_agent.csv"
)

target_workbook_file <- file.path(
  targeted_directory,
  "targeted_human_verification.xlsx"
)

verification_plan_csv <- file.path(
  targeted_directory,
  "targeted_verification_plan.csv"
)

target_study_csv <- file.path(
  targeted_directory,
  "target_study_data_agent.csv"
)

target_outcome_csv <- file.path(
  targeted_directory,
  "target_outcome_data_agent.csv"
)

audit_csv <- file.path(
  targeted_directory,
  "target_audit_checks.csv"
)

report_file <- file.path(
  paths$logs_directory,
  "targeted_human_verification_preparation_report.txt"
)


# ============================================================
# 5. Validate R/25A and R/25 outputs
# ============================================================

required_files <- c(
  advisory_json_file,
  meta_sets_file,
  harmonization_file,
  study_roles_file,
  study_agent_file,
  group_agent_file,
  outcome_agent_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0L) {
  stop(
    "R/25B requires completed R/25 and R/25A outputs.\n",
    "Missing file(s):\n- ",
    paste(
      missing_files,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}


# ============================================================
# 6. Read R/25A Advisory outputs
# ============================================================

advisory <- jsonlite::fromJSON(
  advisory_json_file,
  simplifyVector = FALSE
)

meta_sets <- read_character_csv(
  meta_sets_file
)

harmonization <- read_character_csv(
  harmonization_file
)

study_roles <- read_character_csv(
  study_roles_file
)

required_meta_columns <- c(
  "analysis_name",
  "role",
  "study_design_scope",
  "endpoint",
  "time_window",
  "effect_measure",
  "eligible_study_ids",
  "study_count"
)

missing_meta_columns <- setdiff(
  required_meta_columns,
  names(meta_sets)
)

if (length(missing_meta_columns) > 0L) {
  stop(
    "candidate_meta_analysis_sets.csv is missing required column(s): ",
    paste(
      missing_meta_columns,
      collapse = ", "
    ),
    call. = FALSE
  )
}

recommended_topic <- safe_chr(
  advisory$recommended_topic
)

recommended_primary_endpoint <- safe_chr(
  advisory$recommended_primary_endpoint
)

recommended_primary_question <- safe_chr(
  advisory$recommended_primary_question
)


# ============================================================
# 7. Derive verification study sets from R/25A
# ============================================================

get_role_ids <- function(role_name) {

  rows <- meta_sets[
    clean_text(meta_sets$role) == role_name,
    ,
    drop = FALSE
  ]

  if (nrow(rows) == 0L) {
    return(character(0))
  }

  unique(
    unlist(
      lapply(
        rows$eligible_study_ids,
        split_pipe_ids
      ),
      use.names = FALSE
    )
  )
}

primary_ids <- get_role_ids(
  "Primary"
)

sensitivity_ids_all <- get_role_ids(
  "Sensitivity"
)

secondary_ids <- get_role_ids(
  "Secondary"
)

sensitivity_only_ids <- setdiff(
  sensitivity_ids_all,
  primary_ids
)

# Audit-only studies are extracted from explicit overlap/duplicate
# instructions in the Advisory next_human_actions.
human_actions <- as_chr_vec(
  advisory$next_human_actions
)

overlap_actions <- human_actions[
  grepl(
    "overlap|duplicate|double count",
    human_actions,
    ignore.case = TRUE
  )
]

audit_ids <- extract_screening_ids(
  overlap_actions
)

# Do not classify a quantitative-analysis study as audit-only.
quantitative_ids <- unique(
  c(
    primary_ids,
    sensitivity_only_ids,
    secondary_ids
  )
)

audit_only_ids <- setdiff(
  audit_ids,
  quantitative_ids
)

target_ids <- unique(
  c(
    primary_ids,
    sensitivity_only_ids,
    secondary_ids,
    audit_only_ids
  )
)

if (length(primary_ids) == 0L) {
  stop(
    "R/25A did not identify any Primary meta-analysis study.",
    call. = FALSE
  )
}

if (length(target_ids) == 0L) {
  stop(
    "No targeted Human Verification studies could be derived from R/25A.",
    call. = FALSE
  )
}


# ============================================================
# 8. Build verification plan
# ============================================================

make_plan_rows <- function(
    ids,
    verification_tier,
    meta_analysis_role
) {

  if (length(ids) == 0L) {
    return(
      tibble::tibble()
    )
  }

  tibble::tibble(
    screening_id = ids,
    verification_tier = verification_tier,
    meta_analysis_role = meta_analysis_role
  )
}

verification_plan <- dplyr::bind_rows(
  make_plan_rows(
    primary_ids,
    "Tier 1",
    "Primary meta-analysis"
  ),
  make_plan_rows(
    sensitivity_only_ids,
    "Tier 2",
    "Sensitivity analysis"
  ),
  make_plan_rows(
    secondary_ids,
    "Tier 3",
    "Observational secondary analysis"
  ),
  make_plan_rows(
    audit_only_ids,
    "Audit",
    "Overlap / duplicate-cohort audit only"
  )
)

if (anyDuplicated(verification_plan$screening_id) > 0L) {
  # Preserve the highest-priority role if a study appears in more than one set.
  tier_order <- c(
    "Tier 1" = 1L,
    "Tier 2" = 2L,
    "Tier 3" = 3L,
    "Audit" = 4L
  )

  verification_plan$tier_order <- tier_order[
    verification_plan$verification_tier
  ]

  verification_plan <- verification_plan |>
    dplyr::arrange(
      tier_order
    ) |>
    dplyr::distinct(
      screening_id,
      .keep_all = TRUE
    ) |>
    dplyr::select(
      -tier_order
    )
}


# ============================================================
# 9. Read R/25 extraction outputs
# ============================================================

study_agent <- read_character_csv(
  study_agent_file
)

group_agent <- read_character_csv(
  group_agent_file
)

outcome_agent <- read_character_csv(
  outcome_agent_file
)

if (
  !"screening_id" %in% names(study_agent) ||
  !"screening_id" %in% names(outcome_agent)
) {
  stop(
    "R/25 Agent datasets are missing screening_id.",
    call. = FALSE
  )
}

study_agent$screening_id <- clean_text(
  study_agent$screening_id
)

outcome_agent$screening_id <- clean_text(
  outcome_agent$screening_id
)

if ("screening_id" %in% names(group_agent)) {
  group_agent$screening_id <- clean_text(
    group_agent$screening_id
  )
}

missing_target_studies <- setdiff(
  target_ids,
  study_agent$screening_id
)

if (length(missing_target_studies) > 0L) {
  stop(
    "Targeted study ID(s) are missing from study_characteristics_agent.csv: ",
    paste(
      missing_target_studies,
      collapse = ", "
    ),
    call. = FALSE
  )
}


# ============================================================
# 10. Determine canonical target outcome(s)
# ============================================================
# R/25A meta-analysis set endpoints may be more specific than the
# canonical R/25 outcome_name. We therefore use Advisory
# outcome_harmonization to identify the best canonical outcome label.
# ============================================================

canonical_target_outcomes <- character(0)

if (
  nrow(harmonization) > 0L &&
  all(
    c(
      "canonical_outcome",
      "recommended_harmonized_endpoint"
    ) %in% names(harmonization)
  )
) {

  similarity <- vapply(
    seq_len(nrow(harmonization)),
    function(i) {

      score_1 <- text_similarity_score(
        harmonization$recommended_harmonized_endpoint[[i]],
        recommended_primary_endpoint
      )

      score_2 <- text_similarity_score(
        harmonization$canonical_outcome[[i]],
        recommended_primary_endpoint
      )

      max(
        score_1,
        score_2
      )
    },
    numeric(1)
  )

  if (
    length(similarity) > 0L &&
    max(similarity, na.rm = TRUE) > 0
  ) {
    best_score <- max(
      similarity,
      na.rm = TRUE
    )

    canonical_target_outcomes <- clean_text(
      harmonization$canonical_outcome[
        similarity == best_score
      ]
    )

    canonical_target_outcomes <- unique(
      canonical_target_outcomes[
        !is.na(
          canonical_target_outcomes
        )
      ]
    )
  }
}

# Fallback:
# Find outcome_name with greatest keyword overlap to the recommended
# primary endpoint.
if (
  length(canonical_target_outcomes) == 0L &&
  "outcome_name" %in% names(outcome_agent)
) {

  outcome_names <- unique(
    clean_text(
      outcome_agent$outcome_name
    )
  )

  outcome_names <- outcome_names[
    !is.na(outcome_names)
  ]

  scores <- vapply(
    outcome_names,
    function(x) {
      text_similarity_score(
        x,
        recommended_primary_endpoint
      )
    },
    numeric(1)
  )

  if (
    length(scores) > 0L &&
    max(scores, na.rm = TRUE) > 0
  ) {
    canonical_target_outcomes <- outcome_names[
      scores == max(scores, na.rm = TRUE)
    ]
  }
}

if (length(canonical_target_outcomes) == 0L) {
  stop(
    "R/25B could not map the Advisory primary endpoint to a canonical ",
    "Agent-extracted outcome_name.\n",
    "Recommended primary endpoint: ",
    recommended_primary_endpoint,
    call. = FALSE
  )
}


# ============================================================
# 11. Target study data
# ============================================================

target_study_data <- study_agent |>
  dplyr::filter(
    screening_id %in% target_ids
  ) |>
  dplyr::left_join(
    verification_plan,
    by = "screening_id"
  ) |>
  dplyr::mutate(
    verification_status = NA_character_,
    human_verified_study_design = NA_character_,
    human_verified_analysis_population = NA_character_,
    human_verified_surgery_or_procedure = NA_character_,
    human_verified_intervention_or_exposure = NA_character_,
    human_verified_exposure_timing = NA_character_,
    human_verified_comparator = NA_character_,
    human_study_notes = NA_character_,
    verifier = NA_character_,
    verified_at = NA_character_
  )

tier_rank <- c(
  "Tier 1" = 1L,
  "Tier 2" = 2L,
  "Tier 3" = 3L,
  "Audit" = 4L
)

target_study_data$.tier_order <- tier_rank[
  target_study_data$verification_tier
]

target_study_data <- target_study_data |>
  dplyr::arrange(
    .tier_order,
    screening_id
  ) |>
  dplyr::select(
    -.tier_order
  )


# ============================================================
# 12. Target outcome data
# ============================================================

if (!"outcome_name" %in% names(outcome_agent)) {
  stop(
    "outcome_data_agent.csv is missing outcome_name.",
    call. = FALSE
  )
}

target_outcome_data <- outcome_agent |>
  dplyr::filter(
    screening_id %in% quantitative_ids,
    outcome_name %in% canonical_target_outcomes
  ) |>
  dplyr::left_join(
    verification_plan,
    by = "screening_id"
  ) |>
  dplyr::mutate(
    verification_status = NA_character_,
    human_include_in_planned_analysis = NA_character_,
    human_verified_outcome_definition = NA_character_,
    human_verified_timepoint = NA_character_,
    human_verified_data_type = NA_character_,
    human_verified_intervention_events = NA_character_,
    human_verified_intervention_total = NA_character_,
    human_verified_comparator_events = NA_character_,
    human_verified_comparator_total = NA_character_,
    human_verified_effect_measure = NA_character_,
    human_verified_effect_estimate = NA_character_,
    human_verified_lower_ci = NA_character_,
    human_verified_upper_ci = NA_character_,
    human_verified_adjusted = NA_character_,
    human_verified_adjustment_variables = NA_character_,
    human_verified_source_location = NA_character_,
    human_outcome_notes = NA_character_,
    verifier = NA_character_,
    verified_at = NA_character_
  )

if (nrow(target_outcome_data) == 0L) {
  stop(
    "No targeted outcome rows were found for the Advisory-recommended ",
    "primary endpoint.\nCanonical mapped outcome(s): ",
    paste(
      canonical_target_outcomes,
      collapse = " | "
    ),
    call. = FALSE
  )
}

target_outcome_data$.tier_order <- tier_rank[
  target_outcome_data$verification_tier
]

target_outcome_data <- target_outcome_data |>
  dplyr::arrange(
    .tier_order,
    screening_id
  ) |>
  dplyr::select(
    -.tier_order
  )


# ============================================================
# 13. Audit checks
# ============================================================

audit_checks <- tibble::tibble(
  audit_id = character(),
  screening_id = character(),
  related_screening_ids = character(),
  audit_question = character(),
  audit_status = character(),
  human_conclusion = character(),
  verifier = character(),
  verified_at = character()
)

if (length(overlap_actions) > 0L) {

  audit_rows <- lapply(
    seq_along(overlap_actions),
    function(i) {

      ids <- extract_screening_ids(
        overlap_actions[[i]]
      )

      tibble::tibble(
        audit_id = paste0(
          "AUDIT_",
          sprintf("%03d", i)
        ),
        screening_id = if (
          length(ids) >= 1L
        ) {
          ids[[1]]
        } else {
          ""
        },
        related_screening_ids = if (
          length(ids) >= 2L
        ) {
          paste(
            ids[-1L],
            collapse = " | "
          )
        } else {
          ""
        },
        audit_question = overlap_actions[[i]],
        audit_status = NA_character_,
        human_conclusion = NA_character_,
        verifier = NA_character_,
        verified_at = NA_character_
      )
    }
  )

  audit_checks <- dplyr::bind_rows(
    audit_rows
  )
}


# ============================================================
# 14. Meta-analysis set reference table
# ============================================================

meta_reference <- meta_sets |>
  dplyr::filter(
    role %in% c(
      "Primary",
      "Sensitivity",
      "Secondary"
    )
  )

if (nrow(meta_reference) == 0L) {
  stop(
    "R/25A contains no Primary/Sensitivity/Secondary meta-analysis set.",
    call. = FALSE
  )
}


# ============================================================
# 15. Save machine-readable preparation outputs
# ============================================================

readr::write_csv(
  verification_plan,
  verification_plan_csv,
  na = ""
)

readr::write_csv(
  target_study_data,
  target_study_csv,
  na = ""
)

readr::write_csv(
  target_outcome_data,
  target_outcome_csv,
  na = ""
)

readr::write_csv(
  audit_checks,
  audit_csv,
  na = ""
)


# ============================================================
# 16. Workbook styles
# ============================================================

wb <- openxlsx::createWorkbook(
  creator = "Universal SR-MA Agent Toolkit"
)

title_style <- openxlsx::createStyle(
  textDecoration = "bold",
  fontSize = 14,
  valign = "center"
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "Bottom"
)

wrap_style <- openxlsx::createStyle(
  valign = "top",
  wrapText = TRUE
)

human_header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "Bottom",
  fgFill = "#FFF2CC"
)

tier1_style <- openxlsx::createStyle(
  fgFill = "#E2F0D9"
)

tier2_style <- openxlsx::createStyle(
  fgFill = "#DDEBF7"
)

tier3_style <- openxlsx::createStyle(
  fgFill = "#FCE4D6"
)

audit_style <- openxlsx::createStyle(
  fgFill = "#EDEDED"
)


# ============================================================
# 17. Generic workbook writer
# ============================================================

write_table_sheet <- function(
    sheet_name,
    title,
    data,
    wide_columns = character(0),
    highlight_human_columns = TRUE
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

  if (ncol(data) > 1L) {
    openxlsx::mergeCells(
      wb,
      sheet_name,
      cols = seq_len(ncol(data)),
      rows = 1
    )
  }

  openxlsx::addStyle(
    wb,
    sheet_name,
    title_style,
    rows = 1,
    cols = 1
  )

  openxlsx::writeData(
    wb,
    sheet_name,
    data,
    startRow = 3,
    startCol = 1,
    colNames = TRUE,
    rowNames = FALSE,
    withFilter = TRUE,
    keepNA = FALSE
  )

  if (ncol(data) > 0L) {

    openxlsx::addStyle(
      wb,
      sheet_name,
      header_style,
      rows = 3,
      cols = seq_len(ncol(data)),
      gridExpand = TRUE
    )

    human_cols <- grep(
      "^(verification_status|human_|verifier$|verified_at$|audit_status$)",
      names(data)
    )

    if (
      isTRUE(highlight_human_columns) &&
      length(human_cols) > 0L
    ) {
      openxlsx::addStyle(
        wb,
        sheet_name,
        human_header_style,
        rows = 3,
        cols = human_cols,
        gridExpand = TRUE,
        stack = TRUE
      )
    }

    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = seq_len(ncol(data)),
      widths = "auto"
    )
  }

  if (
    nrow(data) > 0L &&
    ncol(data) > 0L
  ) {

    openxlsx::addStyle(
      wb,
      sheet_name,
      wrap_style,
      rows = 4:(nrow(data) + 3),
      cols = seq_len(ncol(data)),
      gridExpand = TRUE
    )
  }

  for (
    col_name in intersect(
      wide_columns,
      names(data)
    )
  ) {
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols = which(
        names(data) == col_name
      ),
      widths = 52
    )
  }

  openxlsx::freezePane(
    wb,
    sheet_name,
    firstActiveRow = 4,
    firstActiveCol = 2
  )

  invisible(TRUE)
}


# ============================================================
# 18. Instructions
# ============================================================

openxlsx::addWorksheet(
  wb,
  "Instructions"
)

instructions <- tibble::tibble(
  Section = c(
    "Purpose",
    "Connection to R/25A",
    "Systematic-review inclusion",
    "Tier 1",
    "Tier 2",
    "Tier 3",
    "Audit",
    "Target outcome",
    "Agent values",
    "Human fields",
    "Corrected values",
    "Analysis inclusion",
    "Completion rule",
    "Next step"
  ),
  Instruction = c(
    "This workbook focuses Human Verification on studies selected by the R/25A Research Scope Advisory.",
    "Study IDs and analysis roles are derived automatically from research_scope_advisory.json and candidate_meta_analysis_sets.csv.",
    "R/25B does not exclude any study from the systematic review. It only identifies the subset relevant to the planned quantitative synthesis.",
    "Tier 1 = studies contributing to the Advisory-recommended primary meta-analysis.",
    "Tier 2 = additional study/studies used only in sensitivity analysis.",
    "Tier 3 = observational studies used in a separate secondary synthesis.",
    "Audit = study/studies requiring overlap, duplicate-cohort, or other non-quantitative checks.",
    paste0(
      "Mapped primary outcome: ",
      paste(
        canonical_target_outcomes,
        collapse = " | "
      ),
      ". Advisory endpoint: ",
      recommended_primary_endpoint
    ),
    "All Agent-extracted columns must remain unchanged for provenance.",
    "Yellow-header human fields are completed by the human reviewer.",
    "If an Agent value is wrong, enter the corrected value in the corresponding human_verified_* field and explain in human_*_notes.",
    "For each outcome row, set human_include_in_planned_analysis to Yes or No after checking the source paper and the Advisory analysis definition.",
    "Quantitative-analysis rows should finish as Verified or Corrected. Needs clarification is not final.",
    "After Human Verification is complete, R/26 will validate and finalize the authoritative meta-analysis-ready dataset."
  )
)

openxlsx::writeData(
  wb,
  "Instructions",
  paste0(
    project_title,
    " — Targeted Human Verification"
  ),
  startRow = 1,
  startCol = 1
)

openxlsx::mergeCells(
  wb,
  "Instructions",
  cols = 1:2,
  rows = 1
)

openxlsx::addStyle(
  wb,
  "Instructions",
  title_style,
  rows = 1,
  cols = 1
)

openxlsx::writeData(
  wb,
  "Instructions",
  instructions,
  startRow = 3,
  startCol = 1,
  colNames = TRUE
)

openxlsx::addStyle(
  wb,
  "Instructions",
  header_style,
  rows = 3,
  cols = 1:2,
  gridExpand = TRUE
)

openxlsx::setColWidths(
  wb,
  "Instructions",
  cols = 1,
  widths = 28
)

openxlsx::setColWidths(
  wb,
  "Instructions",
  cols = 2,
  widths = 100
)


# ============================================================
# 19. Advisory Summary
# ============================================================

advisory_summary <- tibble::tibble(
  Field = c(
    "Recommended topic",
    "Recommended primary question",
    "Recommended primary endpoint",
    "Mapped canonical target outcome",
    "Primary studies",
    "Sensitivity-only studies",
    "Secondary observational studies",
    "Audit-only studies",
    "Total targeted studies"
  ),
  Value = c(
    recommended_topic,
    recommended_primary_question,
    recommended_primary_endpoint,
    paste(
      canonical_target_outcomes,
      collapse = " | "
    ),
    paste(
      primary_ids,
      collapse = " | "
    ),
    paste(
      sensitivity_only_ids,
      collapse = " | "
    ),
    paste(
      secondary_ids,
      collapse = " | "
    ),
    paste(
      audit_only_ids,
      collapse = " | "
    ),
    as.character(
      length(target_ids)
    )
  )
)

write_table_sheet(
  "Advisory_Summary",
  "R/25A Advisory → R/25B Verification Scope",
  advisory_summary,
  wide_columns = "Value",
  highlight_human_columns = FALSE
)


# ============================================================
# 20. Verification Plan
# ============================================================

write_table_sheet(
  "Verification_Plan",
  "Targeted Human Verification Plan",
  verification_plan,
  wide_columns = c(
    "meta_analysis_role"
  ),
  highlight_human_columns = FALSE
)


# ============================================================
# 21. Meta-analysis Set Reference
# ============================================================

write_table_sheet(
  "Meta_Analysis_Sets",
  "R/25A Recommended Meta-analysis Sets",
  meta_reference,
  wide_columns = c(
    "analysis_name",
    "endpoint",
    "eligible_study_ids",
    "why_poolable",
    "important_caveats"
  ),
  highlight_human_columns = FALSE
)


# ============================================================
# 22. Target Study Verification
# ============================================================

write_table_sheet(
  "Target_Study_Verification",
  "Targeted Study-level Human Verification",
  target_study_data,
  wide_columns = c(
    "title",
    "pdf_path",
    "extraction_notes",
    "overall_extraction_notes",
    "human_study_notes"
  )
)


# ============================================================
# 23. Target Outcome Verification
# ============================================================

write_table_sheet(
  "Target_Outcome_Verification",
  "Targeted Outcome-level Human Verification",
  target_outcome_data,
  wide_columns = c(
    "outcome_definition",
    "adjustment_variables",
    "source_location",
    "source_evidence",
    "extraction_notes",
    "human_verified_outcome_definition",
    "human_verified_adjustment_variables",
    "human_verified_source_location",
    "human_outcome_notes"
  )
)


# ============================================================
# 24. Audit Checks
# ============================================================

write_table_sheet(
  "Audit_Checks",
  "Additional Human Audit Checks",
  audit_checks,
  wide_columns = c(
    "related_screening_ids",
    "audit_question",
    "human_conclusion"
  )
)


# ============================================================
# 25. Validation lists
# ============================================================

openxlsx::addWorksheet(
  wb,
  "Validation_Lists"
)

validation_lists <- tibble::tibble(
  verification_status = c(
    "Verified",
    "Corrected",
    "Needs clarification"
  ),
  yes_no = c(
    "Yes",
    "No",
    NA_character_
  ),
  audit_status = c(
    "Confirmed",
    "No overlap",
    "Needs clarification"
  )
)

openxlsx::writeData(
  wb,
  "Validation_Lists",
  validation_lists,
  startRow = 1,
  startCol = 1
)


# ============================================================
# 26. Data validation dropdowns
# ============================================================

add_dropdown <- function(
    sheet,
    data,
    column_name,
    source_column_letter,
    source_last_row
) {

  if (
    nrow(data) == 0L ||
    !column_name %in% names(data)
  ) {
    return(invisible(FALSE))
  }

  target_col <- which(
    names(data) == column_name
  )

  openxlsx::dataValidation(
    wb,
    sheet,
    cols = target_col,
    rows = 4:(nrow(data) + 3),
    type = "list",
    value = paste0(
      "'Validation_Lists'!$",
      source_column_letter,
      "$2:$",
      source_column_letter,
      "$",
      source_last_row
    )
  )

  invisible(TRUE)
}

add_dropdown(
  "Target_Study_Verification",
  target_study_data,
  "verification_status",
  "A",
  4
)

add_dropdown(
  "Target_Outcome_Verification",
  target_outcome_data,
  "verification_status",
  "A",
  4
)

add_dropdown(
  "Target_Outcome_Verification",
  target_outcome_data,
  "human_include_in_planned_analysis",
  "B",
  3
)

add_dropdown(
  "Audit_Checks",
  audit_checks,
  "audit_status",
  "C",
  4
)


# ============================================================
# 27. Tier highlighting
# ============================================================

apply_tier_highlight <- function(
    sheet,
    data
) {

  if (
    nrow(data) == 0L ||
    !"verification_tier" %in% names(data)
  ) {
    return(invisible(FALSE))
  }

  tier_col <- which(
    names(data) == "verification_tier"
  )

  for (i in seq_len(nrow(data))) {

    row_excel <- i + 3L
    tier <- safe_chr(
      data$verification_tier[[i]]
    )

    style <- switch(
      tier,
      "Tier 1" = tier1_style,
      "Tier 2" = tier2_style,
      "Tier 3" = tier3_style,
      "Audit" = audit_style,
      NULL
    )

    if (!is.null(style)) {
      openxlsx::addStyle(
        wb,
        sheet,
        style,
        rows = row_excel,
        cols = tier_col,
        stack = TRUE
      )
    }
  }

  invisible(TRUE)
}

apply_tier_highlight(
  "Target_Study_Verification",
  target_study_data
)

apply_tier_highlight(
  "Target_Outcome_Verification",
  target_outcome_data
)


# ============================================================
# 28. Save workbook
# ============================================================

openxlsx::saveWorkbook(
  wb,
  target_workbook_file,
  overwrite = TRUE
)

if (
  !file.exists(target_workbook_file) ||
  is.na(file.info(target_workbook_file)$size) ||
  file.info(target_workbook_file)$size <= 0
) {
  stop(
    "Targeted Human Verification workbook was not created successfully.",
    call. = FALSE
  )
}


# ============================================================
# 29. Re-open and validate workbook
# ============================================================

required_sheets <- c(
  "Instructions",
  "Advisory_Summary",
  "Verification_Plan",
  "Meta_Analysis_Sets",
  "Target_Study_Verification",
  "Target_Outcome_Verification",
  "Audit_Checks",
  "Validation_Lists"
)

saved_sheets <- openxlsx::getSheetNames(
  target_workbook_file
)

missing_sheets <- setdiff(
  required_sheets,
  saved_sheets
)

if (length(missing_sheets) > 0L) {
  stop(
    "Targeted Human Verification workbook is missing sheet(s): ",
    paste(
      missing_sheets,
      collapse = ", "
    ),
    call. = FALSE
  )
}

saved_study <- openxlsx::readWorkbook(
  target_workbook_file,
  sheet = "Target_Study_Verification",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

saved_outcome <- openxlsx::readWorkbook(
  target_workbook_file,
  sheet = "Target_Outcome_Verification",
  startRow = 3,
  colNames = TRUE,
  skipEmptyRows = FALSE
)

if (nrow(saved_study) != nrow(target_study_data)) {
  stop(
    "Saved Target_Study_Verification row count mismatch.",
    call. = FALSE
  )
}

if (nrow(saved_outcome) != nrow(target_outcome_data)) {
  stop(
    "Saved Target_Outcome_Verification row count mismatch.",
    call. = FALSE
  )
}

human_fields_to_be_blank <- c(
  "verification_status",
  "human_include_in_planned_analysis",
  "human_verified_outcome_definition",
  "human_verified_timepoint",
  "human_verified_data_type",
  "human_verified_intervention_events",
  "human_verified_intervention_total",
  "human_verified_comparator_events",
  "human_verified_comparator_total",
  "human_verified_effect_measure",
  "human_verified_effect_estimate",
  "human_verified_lower_ci",
  "human_verified_upper_ci",
  "human_verified_adjusted",
  "human_verified_adjustment_variables",
  "human_verified_source_location",
  "human_outcome_notes",
  "verifier",
  "verified_at"
)

for (
  field_name in intersect(
    human_fields_to_be_blank,
    names(saved_outcome)
  )
) {

  values <- clean_text(
    saved_outcome[[field_name]]
  )

  if (any(!is.na(values))) {
    stop(
      "R/25B unexpectedly populated human-only field: ",
      field_name,
      call. = FALSE
    )
  }
}


# ============================================================
# 30. Report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/25B — Targeted Human Verification Preparation Report",
  "",
  paste0("Generated at: ", now_text()),
  paste0("Project ID: ", project_id),
  paste0("Project title: ", project_title),
  "",
  "CONNECTION",
  "--------------------------------------------------",
  "Upstream Agent extraction: R/25",
  "Upstream scope advisory: R/25A",
  "R/25A advisory JSON loaded: YES",
  "R/25A candidate meta-analysis sets loaded: YES",
  "Study IDs hard-coded: NO",
  "",
  "RESEARCH SCOPE",
  "--------------------------------------------------",
  paste0("Recommended topic: ", recommended_topic),
  paste0("Primary endpoint: ", recommended_primary_endpoint),
  paste0(
    "Mapped canonical outcome: ",
    paste(
      canonical_target_outcomes,
      collapse = " | "
    )
  ),
  "",
  "TARGETED HUMAN VERIFICATION",
  "--------------------------------------------------",
  paste0(
    "Primary studies: ",
    length(primary_ids),
    " — ",
    paste(primary_ids, collapse = " | ")
  ),
  paste0(
    "Sensitivity-only studies: ",
    length(sensitivity_only_ids),
    " — ",
    paste(
      sensitivity_only_ids,
      collapse = " | "
    )
  ),
  paste0(
    "Secondary observational studies: ",
    length(secondary_ids),
    " — ",
    paste(
      secondary_ids,
      collapse = " | "
    )
  ),
  paste0(
    "Audit-only studies: ",
    length(audit_only_ids),
    " — ",
    paste(
      audit_only_ids,
      collapse = " | "
    )
  ),
  paste0(
    "Total targeted study rows: ",
    nrow(target_study_data)
  ),
  paste0(
    "Targeted outcome rows: ",
    nrow(target_outcome_data)
  ),
  paste0(
    "Audit checks: ",
    nrow(audit_checks)
  ),
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0(
    "Workbook: ",
    target_workbook_file
  ),
  paste0(
    "Verification plan CSV: ",
    verification_plan_csv
  ),
  paste0(
    "Target study CSV: ",
    target_study_csv
  ),
  paste0(
    "Target outcome CSV: ",
    target_outcome_csv
  ),
  paste0(
    "Audit CSV: ",
    audit_csv
  ),
  "",
  "STAGE BOUNDARY",
  "--------------------------------------------------",
  "Human Verification performed by R/25B: NO",
  "Systematic-review inclusion modified: NO",
  "Agent values modified: NO",
  "Meta-analysis performed: NO",
  "Runner integration performed: NO",
  "Runner-ready connection to R/25A: YES",
  "",
  "STATUS",
  "--------------------------------------------------",
  "R/25B: PASS",
  "",
  "NEXT STEP",
  "--------------------------------------------------",
  "Open targeted_human_verification.xlsx and complete the human fields.",
  "After completion, run R/26 to validate and finalize the human-verified dataset."
)

writeLines(
  report_lines,
  report_file,
  useBytes = TRUE
)


# ============================================================
# 31. Console output
# ============================================================

cat("\n")
cat("============================================================\n")
cat("R/25B — Targeted Human Verification\n")
cat("============================================================\n\n")

cat("Research topic:\n")
cat(recommended_topic, "\n\n")

cat("Primary endpoint:\n")
cat(recommended_primary_endpoint, "\n\n")

cat("Primary studies:\n")
for (id in primary_ids) {
  cat("- ", id, "\n", sep = "")
}
cat("\n")

cat("Sensitivity-only studies:\n")
if (length(sensitivity_only_ids) > 0L) {
  for (id in sensitivity_only_ids) {
    cat("- ", id, "\n", sep = "")
  }
} else {
  cat("- None\n")
}
cat("\n")

cat("Secondary observational studies:\n")
if (length(secondary_ids) > 0L) {
  for (id in secondary_ids) {
    cat("- ", id, "\n", sep = "")
  }
} else {
  cat("- None\n")
}
cat("\n")

cat("Audit-only studies:\n")
if (length(audit_only_ids) > 0L) {
  for (id in audit_only_ids) {
    cat("- ", id, "\n", sep = "")
  }
} else {
  cat("- None\n")
}
cat("\n")

cat(
  "Targeted study rows: ",
  nrow(target_study_data),
  "\n",
  sep = ""
)

cat(
  "Targeted outcome rows: ",
  nrow(target_outcome_data),
  "\n\n",
  sep = ""
)

cat(
  "Workbook:\n- ",
  target_workbook_file,
  "\n\n",
  sep = ""
)

cat(
  "PASS: R/25B targeted Human Verification workbook created successfully.\n"
)

cat(
  "Next: Human reviewer completes the workbook, then R/26 finalizes the verified dataset.\n"
)

invisible(
  list(
    status = "PASS",
    advisory_json = advisory_json_file,
    primary_ids = primary_ids,
    sensitivity_only_ids = sensitivity_only_ids,
    secondary_ids = secondary_ids,
    audit_only_ids = audit_only_ids,
    canonical_target_outcomes = canonical_target_outcomes,
    verification_plan = verification_plan,
    target_study_data = target_study_data,
    target_outcome_data = target_outcome_data,
    audit_checks = audit_checks,
    workbook = target_workbook_file,
    report = report_file
  )
)
