# ============================================================
# Cardiac Surgery SR-MA Toolkit v2.0
# R/utils/path_utils.R
#
# Purpose
# ------------------------------------------------------------
# 1. Build all project file paths from project_config.yml
# 2. Centralize input/output naming for R/16-R/18
# 3. Create required directories safely
# 4. Validate configured paths and basenames
#
# This file must remain topic-neutral.
# Topic-specific filenames and directories belong in:
# config/project_config.yml
# ============================================================


# ============================================================
# 1. Preliminary checks
# ============================================================

if (!exists("is_nonempty_character", mode = "function")) {

  stop(
    "Required helper functions were not found.\n",
    "Run the following before sourcing path_utils.R:\n",
    'source("R/utils/config_utils.R")'
  )
}


# ============================================================
# 2. General path helper functions
# ============================================================

normalize_project_path <- function(
    path,
    must_work = FALSE
) {

  if (!is_nonempty_character(path)) {

    stop(
      "A non-empty path value is required."
    )
  }

  normalizePath(
    path,
    winslash = "/",
    mustWork = must_work
  )
}


assert_safe_basename <- function(
    basename,
    field_name = "basename"
) {

  if (!is_nonempty_character(basename)) {

    stop(
      "Missing or empty basename: ",
      field_name
    )
  }

  unsafe_pattern <- "[\\\\/:*?\"<>|]"

  if (
    grepl(
      unsafe_pattern,
      basename
    )
  ) {

    stop(
      "Unsupported filename character detected in ",
      field_name,
      ":\n",
      basename,
      "\nAvoid: \\ / : * ? \" < > |"
    )
  }

  invisible(TRUE)
}


ensure_directory <- function(
    directory,
    recursive = TRUE
) {

  if (!is_nonempty_character(directory)) {

    stop(
      "A non-empty directory path is required."
    )
  }

  if (!dir.exists(directory)) {

    created <- dir.create(
      directory,
      recursive = recursive,
      showWarnings = FALSE
    )

    if (
      !isTRUE(created) &&
      !dir.exists(directory)
    ) {

      stop(
        "Failed to create directory:\n",
        normalize_project_path(
          directory,
          must_work = FALSE
        )
      )
    }
  }

  normalize_project_path(
    directory,
    must_work = TRUE
  )
}


ensure_directories <- function(directories) {

  directories <- as.character(
    directories
  )

  directories <- trimws(
    directories
  )

  directories <- unique(
    directories[
      !is.na(directories) &
        nzchar(directories)
    ]
  )

  if (length(directories) == 0L) {

    return(
      character(0)
    )
  }

  vapply(
    directories,
    ensure_directory,
    character(1),
    USE.NAMES = TRUE
  )
}


assert_file_exists <- function(
    file_path,
    label = "Required file"
) {

  if (!is_nonempty_character(file_path)) {

    stop(
      label,
      " path is missing or empty."
    )
  }

  if (!file.exists(file_path)) {

    stop(
      label,
      " was not found:\n",
      normalize_project_path(
        file_path,
        must_work = FALSE
      )
    )
  }

  invisible(TRUE)
}


file_path_with_extension <- function(
    directory,
    basename,
    extension
) {

  assert_safe_basename(
    basename
  )

  if (!is_nonempty_character(extension)) {

    stop(
      "A non-empty file extension is required."
    )
  }

  extension <- sub(
    "^\\.",
    "",
    extension
  )

  file.path(
    directory,
    paste0(
      basename,
      ".",
      extension
    )
  )
}


# ============================================================
# 3. Validate naming and path configuration
# ============================================================

validate_path_configuration <- function(config) {

  if (
    is.null(config$paths) ||
    !is.list(config$paths)
  ) {

    stop(
      "Missing or invalid configuration section: paths"
    )
  }

  if (
    is.null(config$naming) ||
    !is.list(config$naming)
  ) {

    stop(
      "Missing or invalid configuration section: naming"
    )
  }


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
      "The following path settings are missing or empty:\n- ",
      paste(
        paste0(
          "paths.",
          missing_path_fields
        ),
        collapse = "\n- "
      )
    )
  }


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
      "The following naming settings are missing or empty:\n- ",
      paste(
        paste0(
          "naming.",
          missing_naming_fields
        ),
        collapse = "\n- "
      )
    )
  }


  for (field_name in required_naming_fields) {

    assert_safe_basename(
      config$naming[[field_name]],
      paste0(
        "naming.",
        field_name
      )
    )
  }

  invisible(TRUE)
}


# ============================================================
# 4. Build all project paths
# ============================================================

build_project_paths <- function(
    config,
    create_directories = TRUE,
    validate = TRUE
) {

  if (isTRUE(validate)) {

    validate_path_configuration(
      config
    )
  }


  # ----------------------------------------------------------
  # 4.1 Directory paths
  # ----------------------------------------------------------

  paths <- list(
    project_root = normalize_project_path(
      ".",
      must_work = TRUE
    ),

    config_file = attr(
      config,
      "config_file"
    ),

    search_terms_file =
      config$paths$search_terms_file,

    search_strategies_directory =
      config$paths$search_strategies_directory,

    raw_search_directory =
      config$paths$raw_search_directory,

    pubmed_directory =
      config$paths$pubmed_directory,

    embase_directory =
      config$paths$embase_directory,

    central_directory =
      config$paths$central_directory,

    processed_search_directory =
      config$paths$processed_search_directory,

    processed_screening_directory =
      config$paths$processed_screening_directory,

    screening_results_directory =
      config$paths$screening_results_directory,

    logs_directory =
      config$paths$logs_directory
  )


  optional_directory_fields <- c(
    figures_directory = "figures_directory",
    tables_directory = "tables_directory",
    manuscript_directory = "manuscript_directory"
  )

  for (
    output_name in
    names(optional_directory_fields)
  ) {

    config_field <-
      optional_directory_fields[[output_name]]

    configured_value <-
      config$paths[[config_field]]

    if (is_nonempty_character(configured_value)) {

      paths[[output_name]] <-
        configured_value
    }
  }


  # ----------------------------------------------------------
  # 4.2 Search-strategy files
  # ----------------------------------------------------------
  # These filenames preserve the existing R/11-R/12 contract.
  # Only the parent directory is controlled by project_config.yml.

  paths$pubmed_search <- file.path(
    paths$search_strategies_directory,
    "pubmed_search.txt"
  )

  paths$embase_search <- file.path(
    paths$search_strategies_directory,
    "embase_search.txt"
  )

  paths$central_search <- file.path(
    paths$search_strategies_directory,
    "central_search.txt"
  )

  paths$search_strategy_summary <- file.path(
    paths$search_strategies_directory,
    "search_strategy_summary.csv"
  )


  # ----------------------------------------------------------
  # 4.3 PubMed input/output files
  # ----------------------------------------------------------

  paths$pubmed_xml <- file_path_with_extension(
    paths$pubmed_directory,
    config$naming$pubmed_basename,
    "xml"
  )

  paths$pubmed_csv <- file_path_with_extension(
    paths$pubmed_directory,
    config$naming$pubmed_basename,
    "csv"
  )

  paths$pubmed_download_report <- file.path(
    paths$logs_directory,
    paste0(
      config$naming$pubmed_basename,
      "_download_report.txt"
    )
  )


  # ----------------------------------------------------------
  # 4.4 Embase and CENTRAL files
  # ----------------------------------------------------------

  paths$embase_ris <- file_path_with_extension(
    paths$embase_directory,
    config$naming$embase_basename,
    "ris"
  )

  paths$central_ris <- file_path_with_extension(
    paths$central_directory,
    config$naming$central_basename,
    "ris"
  )


  # ----------------------------------------------------------
  # 4.5 R/17 merge and deduplication outputs
  # ----------------------------------------------------------

  paths$merged_records_csv <- file_path_with_extension(
    paths$processed_search_directory,
    config$naming$merged_records_basename,
    "csv"
  )

  paths$deduplicated_records_csv <-
    file_path_with_extension(
      paths$processed_search_directory,
      config$naming$deduplicated_records_basename,
      "csv"
    )

  paths$removed_duplicates_csv <-
    file_path_with_extension(
      paths$processed_search_directory,
      config$naming$removed_duplicates_basename,
      "csv"
    )

  paths$deduplication_map_csv <-
    file_path_with_extension(
      paths$processed_search_directory,
      config$naming$deduplication_map_basename,
      "csv"
    )

  paths$prisma_counts_csv <-
    file_path_with_extension(
      paths$processed_search_directory,
      config$naming$prisma_counts_basename,
      "csv"
    )

  paths$import_merge_deduplication_report <- file.path(
    paths$logs_directory,
    "import_merge_deduplication_report.txt"
  )


  # ----------------------------------------------------------
  # 4.6 R/18 screening workbook outputs
  # ----------------------------------------------------------

  paths$screening_workbook <- file_path_with_extension(
    paths$screening_results_directory,
    config$naming$screening_workbook_basename,
    "xlsx"
  )

  paths$completed_screening_workbook <-
    file_path_with_extension(
      paths$screening_results_directory,
      config$naming$completed_screening_workbook_basename,
      "xlsx"
    )

  paths$screening_workbook_report <- file.path(
    paths$logs_directory,
    "screening_workbook_report.txt"
  )


  # ----------------------------------------------------------
  # 4.7 Create required directories
  # ----------------------------------------------------------

  if (isTRUE(create_directories)) {

    required_directories <- c(
      paths$search_strategies_directory,
      paths$raw_search_directory,
      paths$pubmed_directory,
      paths$embase_directory,
      paths$central_directory,
      paths$processed_search_directory,
      paths$processed_screening_directory,
      paths$screening_results_directory,
      paths$logs_directory
    )

    optional_directories <- unlist(
      paths[
        intersect(
          c(
            "figures_directory",
            "tables_directory",
            "manuscript_directory"
          ),
          names(paths)
        )
      ],
      use.names = FALSE
    )

    ensure_directories(
      c(
        required_directories,
        optional_directories
      )
    )
  }


  # ----------------------------------------------------------
  # 4.8 Normalize directory paths after creation
  # ----------------------------------------------------------

  directory_names <- c(
    "search_strategies_directory",
    "raw_search_directory",
    "pubmed_directory",
    "embase_directory",
    "central_directory",
    "processed_search_directory",
    "processed_screening_directory",
    "screening_results_directory",
    "logs_directory",
    "figures_directory",
    "tables_directory",
    "manuscript_directory"
  )

  directory_names <- intersect(
    directory_names,
    names(paths)
  )

  for (directory_name in directory_names) {

    paths[[directory_name]] <-
      normalize_project_path(
        paths[[directory_name]],
        must_work = TRUE
      )
  }


  # ----------------------------------------------------------
  # 4.9 Normalize file paths without requiring existence
  # ----------------------------------------------------------

  file_names <- setdiff(
    names(paths),
    c(
      "project_root",
      directory_names
    )
  )

  for (file_name in file_names) {

    file_value <- paths[[file_name]]

    if (is_nonempty_character(file_value)) {

      paths[[file_name]] <-
        normalize_project_path(
          file_value,
          must_work = FALSE
        )
    }
  }


  class(paths) <- c(
    "srma_project_paths",
    "list"
  )

  paths
}


# ============================================================
# 5. Input file validation for R/16-R/18
# ============================================================

validate_pubmed_download_inputs <- function(paths) {

  assert_file_exists(
    paths$search_terms_file,
    "Search-terms configuration file"
  )

  invisible(TRUE)
}


validate_merge_inputs <- function(
    config,
    paths
) {

  enabled_databases <- get_enabled_databases(
    config
  )

  missing_files <- character(0)


  if ("pubmed" %in% enabled_databases) {

    if (!file.exists(paths$pubmed_csv)) {

      missing_files <- c(
        missing_files,
        paths$pubmed_csv
      )
    }
  }


  if ("embase" %in% enabled_databases) {

    if (!file.exists(paths$embase_ris)) {

      missing_files <- c(
        missing_files,
        paths$embase_ris
      )
    }
  }


  if ("central" %in% enabled_databases) {

    if (!file.exists(paths$central_ris)) {

      missing_files <- c(
        missing_files,
        paths$central_ris
      )
    }
  }


  if (length(missing_files) > 0L) {

    stop(
      "The following enabled-database input files are missing:\n- ",
      paste(
        missing_files,
        collapse = "\n- "
      )
    )
  }

  invisible(TRUE)
}


validate_screening_workbook_inputs <- function(paths) {

  assert_file_exists(
    paths$deduplicated_records_csv,
    "Deduplicated screening dataset"
  )

  invisible(TRUE)
}


# ============================================================
# 6. Project path summary
# ============================================================

print_project_paths <- function(paths) {

  if (!inherits(paths, "srma_project_paths")) {

    warning(
      "The supplied object was not created by build_project_paths()."
    )
  }

  cat("\n")
  cat("====================================================\n")
  cat("SR-MA Project Path Summary\n")
  cat("====================================================\n\n")

  path_groups <- list(
    "CONFIGURATION" = c(
      "config_file",
      "search_terms_file"
    ),

    "DATABASE INPUTS" = c(
      "pubmed_csv",
      "pubmed_xml",
      "embase_ris",
      "central_ris"
    ),

    "MERGE AND DEDUPLICATION OUTPUTS" = c(
      "merged_records_csv",
      "deduplicated_records_csv",
      "removed_duplicates_csv",
      "deduplication_map_csv",
      "prisma_counts_csv"
    ),

    "SCREENING OUTPUTS" = c(
      "screening_workbook",
      "completed_screening_workbook"
    ),

    "REPORTS" = c(
      "pubmed_download_report",
      "import_merge_deduplication_report",
      "screening_workbook_report"
    )
  )


  for (group_name in names(path_groups)) {

    cat(group_name, "\n")
    cat(
      paste(
        rep(
          "-",
          nchar(group_name)
        ),
        collapse = ""
      ),
      "\n",
      sep = ""
    )

    group_paths <- intersect(
      path_groups[[group_name]],
      names(paths)
    )

    for (path_name in group_paths) {

      cat(
        path_name,
        ":\n  ",
        paths[[path_name]],
        "\n",
        sep = ""
      )
    }

    cat("\n")
  }

  invisible(paths)
}


# ============================================================
# Config-driven canonical project path resolver
# Added by R/31_refactor_config_driven_canonical_paths.R
# ============================================================

resolve_configured_project_path <- function(
    config,
    project_root,
    key,
    create = FALSE
) {

  if (is.null(config$canonical_paths)) {
    stop(
      "project_config.yml is missing canonical_paths.",
      call. = FALSE
    )
  }

  value <- config$canonical_paths[[key]]

  if (is.null(value) || length(value) != 1L || is.na(value) || trimws(value) == "") {
    stop(
      "Missing canonical path key: ",
      key,
      call. = FALSE
    )
  }

  value <- gsub("\\\\", "/", trimws(as.character(value)))

  if (grepl("^[A-Za-z]:/", value) || grepl("^/", value) || grepl("^//", value)) {
    stop(
      "Canonical path must be relative to project root: ",
      key,
      " = ",
      value,
      call. = FALSE
    )
  }

  parts <- strsplit(value, "/", fixed = TRUE)[[1]]

  if (any(parts == "..")) {
    stop(
      "Canonical path cannot contain '..': ",
      key,
      " = ",
      value,
      call. = FALSE
    )
  }

  value <- gsub("/+", "/", value)
  value <- sub("^\\./", "", value)
  value <- sub("/$", "", value)

  root_normalized <- normalizePath(
    project_root,
    winslash = "/",
    mustWork = TRUE
  )

  resolved <- file.path(
    root_normalized,
    value
  )

  resolved <- gsub("\\\\", "/", resolved)

  if (isTRUE(create)) {
    dir.create(
      resolved,
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  resolved
}

