# ============================================================
# Cardiac Surgery SR-MA Toolkit v2.0
# R/utils/config_utils.R
#
# Purpose
# ------------------------------------------------------------
# 1. Locate and load config/project_config.yml
# 2. Validate required configuration sections and values
# 3. Provide reusable configuration helper functions
# 4. Prevent R/16-R/18 from directly parsing YAML repeatedly
#
# This file must remain topic-neutral.
# Topic-specific information belongs in project_config.yml.
# ============================================================


# ============================================================
# 1. Package check
# ============================================================

if (!requireNamespace("yaml", quietly = TRUE)) {

  stop(
    "The 'yaml' package is required.\n",
    "Install it once with:\n",
    'install.packages("yaml")'
  )
}


# ============================================================
# 2. General helper functions
# ============================================================

is_nonempty_character <- function(x) {

  is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    nzchar(trimws(x))
}


is_nonempty_list <- function(x) {

  is.list(x) &&
    length(x) > 0L
}


as_character_vector <- function(x) {

  if (is.null(x)) {
    return(character(0))
  }

  values <- unlist(
    x,
    recursive = TRUE,
    use.names = FALSE
  )

  values <- as.character(values)
  values <- trimws(values)

  values[
    !is.na(values) &
      nzchar(values)
  ]
}


assert_config_section <- function(
    config,
    section_name
) {

  if (
    is.null(config[[section_name]]) ||
    !is.list(config[[section_name]])
  ) {

    stop(
      "Missing or invalid configuration section: ",
      section_name,
      "\nExpected section in config/project_config.yml:\n",
      section_name,
      ":"
    )
  }

  invisible(TRUE)
}


assert_nonempty_config_value <- function(
    value,
    field_name
) {

  if (!is_nonempty_character(value)) {

    stop(
      "Missing or empty configuration value: ",
      field_name
    )
  }

  invisible(TRUE)
}


# ============================================================
# 3. Find configuration file
# ============================================================

find_project_config <- function(
    config_file = "config/project_config.yml"
) {

  if (!is_nonempty_character(config_file)) {

    stop(
      "config_file must be a non-empty file path."
    )
  }

  if (!file.exists(config_file)) {

    stop(
      "Project configuration file was not found:\n",
      normalizePath(
        config_file,
        winslash = "/",
        mustWork = FALSE
      ),
      "\n\nExpected file:\n",
      "config/project_config.yml\n\n",
      "Confirm that R is running from the project root directory."
    )
  }

  normalizePath(
    config_file,
    winslash = "/",
    mustWork = TRUE
  )
}


# ============================================================
# 4. Configuration validation
# ============================================================

validate_project_config <- function(config) {

  if (!is.list(config)) {

    stop(
      "The project configuration could not be read as a YAML list."
    )
  }


  # ----------------------------------------------------------
  # 4.1 Required top-level sections
  # ----------------------------------------------------------

  required_sections <- c(
    "project",
    "research_question",
    "eligibility",
    "search",
    "screening",
    "naming",
    "paths"
  )

  missing_sections <- required_sections[
    !vapply(
      required_sections,
      function(section_name) {

        !is.null(config[[section_name]]) &&
          is.list(config[[section_name]])
      },
      logical(1)
    )
  ]

  if (length(missing_sections) > 0L) {

    stop(
      "The following required configuration sections are missing ",
      "or invalid:\n- ",
      paste(
        missing_sections,
        collapse = "\n- "
      )
    )
  }


  # ----------------------------------------------------------
  # 4.2 Project metadata
  # ----------------------------------------------------------

  required_project_fields <- c(
    "project_id",
    "title",
    "short_name",
    "output_prefix",
    "version"
  )

  missing_project_fields <- required_project_fields[
    !vapply(
      required_project_fields,
      function(field_name) {

        is_nonempty_character(
          config$project[[field_name]]
        )
      },
      logical(1)
    )
  ]

  if (length(missing_project_fields) > 0L) {

    stop(
      "The following project fields are missing or empty:\n- ",
      paste(
        paste0(
          "project.",
          missing_project_fields
        ),
        collapse = "\n- "
      )
    )
  }


  # project_id and output_prefix should be filename-safe

  filename_safe_pattern <- "^[A-Za-z0-9][A-Za-z0-9_-]*$"

  if (
    !grepl(
      filename_safe_pattern,
      config$project$project_id
    )
  ) {

    stop(
      "project.project_id contains unsupported characters.\n",
      "Use letters, numbers, underscores, or hyphens only.\n",
      "Current value: ",
      config$project$project_id
    )
  }

  if (
    !grepl(
      filename_safe_pattern,
      config$project$output_prefix
    )
  ) {

    stop(
      "project.output_prefix contains unsupported characters.\n",
      "Use letters, numbers, underscores, or hyphens only.\n",
      "Current value: ",
      config$project$output_prefix
    )
  }


  # ----------------------------------------------------------
  # 4.3 Research question
  # ----------------------------------------------------------

  if (
    !is_nonempty_character(
      config$research_question$framework
    )
  ) {

    stop(
      "Missing configuration value: ",
      "research_question.framework"
    )
  }

  required_question_sections <- c(
    "population",
    "intervention",
    "comparator",
    "outcomes"
  )

  missing_question_sections <- required_question_sections[
    !vapply(
      required_question_sections,
      function(section_name) {

        !is.null(
          config$research_question[[section_name]]
        )
      },
      logical(1)
    )
  ]

  if (length(missing_question_sections) > 0L) {

    stop(
      "The following research-question components are missing:\n- ",
      paste(
        paste0(
          "research_question.",
          missing_question_sections
        ),
        collapse = "\n- "
      )
    )
  }


  # ----------------------------------------------------------
  # 4.4 Database selection
  # ----------------------------------------------------------

  database_config <- config$search$databases

  if (
    is.null(database_config) ||
    length(database_config) == 0L
  ) {

    stop(
      "No database configuration was found at search.databases."
    )
  }

  enabled_databases <- get_enabled_databases(
    config,
    validate = FALSE
  )

  if (length(enabled_databases) == 0L) {

    stop(
      "At least one database must be enabled in ",
      "search.databases."
    )
  }


  # ----------------------------------------------------------
  # 4.5 Screening decisions
  # ----------------------------------------------------------

  decisions <- get_screening_decisions(
    config,
    validate = FALSE
  )

  if (length(decisions) == 0L) {

    stop(
      "No screening decisions were defined at ",
      "screening.decisions."
    )
  }

  required_decisions <- c(
    "Include",
    "Exclude",
    "Unclear"
  )

  missing_decisions <- setdiff(
    required_decisions,
    decisions
  )

  if (length(missing_decisions) > 0L) {

    stop(
      "The following required screening decisions are missing:\n- ",
      paste(
        missing_decisions,
        collapse = "\n- "
      )
    )
  }


  # ----------------------------------------------------------
  # 4.6 Exclusion reasons
  # ----------------------------------------------------------

  exclusion_reasons <- get_exclusion_reasons(
    config,
    validate = FALSE
  )

  if (nrow(exclusion_reasons) == 0L) {

    stop(
      "No exclusion reasons were defined at ",
      "screening.exclusion_reasons."
    )
  }

  if (anyDuplicated(exclusion_reasons$code) > 0L) {

    duplicated_codes <- unique(
      exclusion_reasons$code[
        duplicated(
          exclusion_reasons$code
        )
      ]
    )

    stop(
      "Duplicate exclusion-reason codes were found:\n- ",
      paste(
        duplicated_codes,
        collapse = "\n- "
      )
    )
  }

  if (anyDuplicated(exclusion_reasons$label) > 0L) {

    duplicated_labels <- unique(
      exclusion_reasons$label[
        duplicated(
          exclusion_reasons$label
        )
      ]
    )

    stop(
      "Duplicate exclusion-reason labels were found:\n- ",
      paste(
        duplicated_labels,
        collapse = "\n- "
      )
    )
  }


  # ----------------------------------------------------------
  # 4.7 Naming configuration
  # ----------------------------------------------------------

  required_naming_fields <- c(
    "pubmed_basename",
    "embase_basename",
    "central_basename",
    "merged_records_basename",
    "deduplicated_records_basename",
    "removed_duplicates_basename",
    "deduplication_map_basename",
    "prisma_counts_basename",
    "screening_workbook_basename",
    "completed_screening_workbook_basename"
  )

  missing_naming_fields <- required_naming_fields[
    !vapply(
      required_naming_fields,
      function(field_name) {

        is_nonempty_character(
          config$naming[[field_name]]
        )
      },
      logical(1)
    )
  ]

  if (length(missing_naming_fields) > 0L) {

    stop(
      "The following naming fields are missing or empty:\n- ",
      paste(
        paste0(
          "naming.",
          missing_naming_fields
        ),
        collapse = "\n- "
      )
    )
  }


  # ----------------------------------------------------------
  # 4.8 Path configuration
  # ----------------------------------------------------------

  required_path_fields <- c(
    "search_terms_file",
    "search_strategies_directory",
    "raw_search_directory",
    "pubmed_directory",
    "embase_directory",
    "central_directory",
    "processed_search_directory",
    "processed_screening_directory",
    "screening_results_directory",
    "logs_directory"
  )

  missing_path_fields <- required_path_fields[
    !vapply(
      required_path_fields,
      function(field_name) {

        is_nonempty_character(
          config$paths[[field_name]]
        )
      },
      logical(1)
    )
  ]

  if (length(missing_path_fields) > 0L) {

    stop(
      "The following path fields are missing or empty:\n- ",
      paste(
        paste0(
          "paths.",
          missing_path_fields
        ),
        collapse = "\n- "
      )
    )
  }


  # Search terms file must already exist

  if (
    !file.exists(
      config$paths$search_terms_file
    )
  ) {

    stop(
      "The configured search-terms file does not exist:\n",
      config$paths$search_terms_file
    )
  }


  invisible(TRUE)
}


# ============================================================
# 5. Load project configuration
# ============================================================

load_project_config <- function(
    config_file = "config/project_config.yml",
    validate = TRUE,
    quiet = FALSE
) {

  resolved_config_file <- find_project_config(
    config_file
  )

  config <- tryCatch(
    {

      yaml::read_yaml(
        resolved_config_file
      )
    },
    error = function(error) {

      stop(
        "Failed to read project configuration:\n",
        resolved_config_file,
        "\n\nYAML error:\n",
        conditionMessage(error)
      )
    }
  )

  if (isTRUE(validate)) {

    validate_project_config(
      config
    )
  }

  attr(
    config,
    "config_file"
  ) <- resolved_config_file

  attr(
    config,
    "loaded_at"
  ) <- format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )

  if (!isTRUE(quiet)) {

    cat("\n")
    cat("Project configuration loaded successfully.\n")
    cat("------------------------------------------\n")
    cat("Project ID:", config$project$project_id, "\n")
    cat("Title:", config$project$title, "\n")
    cat(
      "Enabled databases:",
      paste(
        get_enabled_databases(
          config,
          validate = FALSE
        ),
        collapse = ", "
      ),
      "\n"
    )
    cat("Configuration file:", resolved_config_file, "\n")
  }

  config
}


# ============================================================
# 6. Database helpers
# ============================================================

get_enabled_databases <- function(
    config,
    validate = TRUE
) {

  if (isTRUE(validate)) {
    assert_config_section(
      config,
      "search"
    )
  }

  database_config <- config$search$databases

  if (
    is.null(database_config) ||
    length(database_config) == 0L
  ) {

    return(character(0))
  }


  # Preferred format:
  #
  # databases:
  #   pubmed:
  #     enabled: true
  #   embase:
  #     enabled: true
  #   central:
  #     enabled: true

  if (
    is.list(database_config) &&
    !is.null(names(database_config)) &&
    all(
      nzchar(
        names(database_config)
      )
    )
  ) {

    enabled <- vapply(
      database_config,
      function(database) {

        if (is.logical(database)) {

          return(
            isTRUE(database)
          )
        }

        if (is.list(database)) {

          return(
            isTRUE(database$enabled)
          )
        }

        FALSE
      },
      logical(1)
    )

    return(
      names(database_config)[enabled]
    )
  }


  # Temporary backward-compatible support for old project.yml:
  #
  # databases:
  #   - "PubMed/MEDLINE"
  #   - "Embase"
  #   - "Cochrane CENTRAL"

  database_values <- as_character_vector(
    database_config
  )

  normalized <- tolower(
    database_values
  )

  database_names <- character(0)

  if (
    any(
      grepl(
        "pubmed|medline",
        normalized
      )
    )
  ) {

    database_names <- c(
      database_names,
      "pubmed"
    )
  }

  if (
    any(
      grepl(
        "embase",
        normalized
      )
    )
  ) {

    database_names <- c(
      database_names,
      "embase"
    )
  }

  if (
    any(
      grepl(
        "central|cochrane",
        normalized
      )
    )
  ) {

    database_names <- c(
      database_names,
      "central"
    )
  }

  unique(
    database_names
  )
}


is_database_enabled <- function(
    config,
    database_name
) {

  if (!is_nonempty_character(database_name)) {

    stop(
      "database_name must be a non-empty character value."
    )
  }

  database_name <- tolower(
    trimws(database_name)
  )

  database_name %in%
    get_enabled_databases(
      config,
      validate = FALSE
    )
}


# ============================================================
# 7. Screening helpers
# ============================================================

get_screening_decisions <- function(
    config,
    validate = TRUE
) {

  if (isTRUE(validate)) {
    assert_config_section(
      config,
      "screening"
    )
  }

  decisions <- as_character_vector(
    config$screening$decisions
  )

  unique(
    decisions
  )
}


get_exclusion_reasons <- function(
    config,
    validate = TRUE
) {

  if (isTRUE(validate)) {
    assert_config_section(
      config,
      "screening"
    )
  }

  reasons <- config$screening$exclusion_reasons

  if (
    is.null(reasons) ||
    length(reasons) == 0L
  ) {

    return(
      data.frame(
        code = character(0),
        label = character(0),
        stringsAsFactors = FALSE
      )
    )
  }


  # Preferred structured format:
  #
  # exclusion_reasons:
  #   - code: "E01"
  #     label: "Wrong population"

  if (
    is.list(reasons) &&
    all(
      vapply(
        reasons,
        is.list,
        logical(1)
      )
    )
  ) {

    reason_codes <- vapply(
      seq_along(reasons),
      function(index) {

        reason <- reasons[[index]]

        code <- reason$code

        if (!is_nonempty_character(code)) {

          code <- paste0(
            "E",
            sprintf(
              "%02d",
              index
            )
          )
        }

        trimws(code)
      },
      character(1)
    )

    reason_labels <- vapply(
      reasons,
      function(reason) {

        label <- reason$label

        if (!is_nonempty_character(label)) {

          return(NA_character_)
        }

        trimws(label)
      },
      character(1)
    )

    if (any(is.na(reason_labels))) {

      stop(
        "Each screening exclusion reason must contain ",
        "a non-empty label."
      )
    }

    return(
      data.frame(
        code = reason_codes,
        label = reason_labels,
        stringsAsFactors = FALSE
      )
    )
  }


  # Backward-compatible support for a simple character list

  reason_labels <- as_character_vector(
    reasons
  )

  data.frame(
    code = paste0(
      "E",
      sprintf(
        "%02d",
        seq_along(reason_labels)
      )
    ),
    label = reason_labels,
    stringsAsFactors = FALSE
  )
}


get_exclusion_reason_labels <- function(
    config
) {

  get_exclusion_reasons(
    config
  )$label
}


get_exclusion_reason_codes <- function(
    config
) {

  get_exclusion_reasons(
    config
  )$code
}


get_reviewer_name <- function(
    config,
    reviewer = c(
      "reviewer_1",
      "reviewer_2",
      "third_reviewer"
    ),
    fallback = NULL
) {

  reviewer <- match.arg(
    reviewer
  )

  reviewer_config <-
    config$screening$reviewers[[reviewer]]

  if (is.null(reviewer_config)) {

    if (!is.null(fallback)) {
      return(fallback)
    }

    return(
      gsub(
        "_",
        " ",
        tools::toTitleCase(
          reviewer
        )
      )
    )
  }

  if (
    is.character(reviewer_config) &&
    length(reviewer_config) == 1L
  ) {

    reviewer_name <- trimws(
      reviewer_config
    )

  } else {

    reviewer_name <- reviewer_config$name
  }

  if (!is_nonempty_character(reviewer_name)) {

    if (!is.null(fallback)) {
      return(fallback)
    }

    reviewer_name <- gsub(
      "_",
      " ",
      tools::toTitleCase(
        reviewer
      )
    )
  }

  reviewer_name
}


# ============================================================
# 8. Project metadata helpers
# ============================================================

get_project_id <- function(config) {

  assert_nonempty_config_value(
    config$project$project_id,
    "project.project_id"
  )

  config$project$project_id
}


get_project_title <- function(config) {

  assert_nonempty_config_value(
    config$project$title,
    "project.title"
  )

  config$project$title
}


get_project_short_name <- function(config) {

  assert_nonempty_config_value(
    config$project$short_name,
    "project.short_name"
  )

  config$project$short_name
}


get_output_prefix <- function(config) {

  assert_nonempty_config_value(
    config$project$output_prefix,
    "project.output_prefix"
  )

  config$project$output_prefix
}


# ============================================================
# 9. Configuration summary
# ============================================================

print_project_config_summary <- function(config) {

  exclusion_reasons <- get_exclusion_reasons(
    config
  )

  cat("\n")
  cat("====================================================\n")
  cat("SR-MA Project Configuration Summary\n")
  cat("====================================================\n\n")

  cat("Project ID:\n")
  cat("-", get_project_id(config), "\n\n")

  cat("Project title:\n")
  cat("-", get_project_title(config), "\n\n")

  cat("Review framework:\n")
  cat(
    "-",
    config$research_question$framework,
    "\n\n"
  )

  cat("Enabled databases:\n")

  for (
    database_name in
    get_enabled_databases(config)
  ) {

    cat(
      "-",
      database_name,
      "\n"
    )
  }

  cat("\nScreening decisions:\n")

  for (
    decision in
    get_screening_decisions(config)
  ) {

    cat(
      "-",
      decision,
      "\n"
    )
  }

  cat("\nExclusion reasons:\n")

  for (
    row_number in
    seq_len(
      nrow(
        exclusion_reasons
      )
    )
  ) {

    cat(
      "-",
      exclusion_reasons$code[row_number],
      ":",
      exclusion_reasons$label[row_number],
      "\n"
    )
  }

  cat("\nConfiguration validated successfully.\n")

  invisible(config)
}