# ============================================================
# Universal SR-MA Agent Toolkit
# R/31_refactor_config_driven_canonical_paths.R
#
# R/31 — Config-driven Canonical Path Refactor
#
# PURPOSE
# ------------------------------------------------------------
# Convert R/29 additional-analysis workspace paths from internal
# hard-coded relative strings to project_config.yml-driven paths.
#
# This script performs a ONE-TIME SAFE REFACTOR:
#
#   1. Verify repository root and canonical files.
#   2. Add canonical_paths to config/project_config.yml if absent.
#   3. Add a reusable config-driven path resolver to path_utils.R.
#   4. Refactor R/29 to obtain all additional-analysis roots from config.
#   5. Validate that configured paths are RELATIVE and remain inside
#      the current project root.
#   6. Create canonical directories if missing.
#   7. Preserve backups before modifying files.
#   8. Save a refactor report.
#
# IMPORTANT
# ------------------------------------------------------------
# - No username, drive letter, or absolute machine path is stored.
# - Users are NOT asked to enter folder names.
# - project_config.yml is the authoritative source of canonical paths.
# - The actual project root is resolved at runtime.
# - R/29 remains portable across Windows/macOS/Linux installations.
#
# DEFAULT CONFIG KEYS ADDED
# ------------------------------------------------------------
# canonical_paths:
#   additional_analysis_raw: data/raw/additional_analysis
#   additional_analysis_processed: data/processed/additional_analysis
#   additional_analysis_results: results/additional_analysis
#
# ============================================================


# ============================================================
# 0. Required packages
# ============================================================

required_packages <- c(
  "yaml",
  "stringr",
  "readr",
  "tibble",
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


is_absolute_path <- function(x) {

  x <- normalize_slash(
    trimws(
      as.character(x)
    )
  )

  windows_absolute <- stringr::str_detect(
    x,
    "^[A-Za-z]:/"
  )

  unc_absolute <- stringr::str_detect(
    x,
    "^//"
  )

  unix_absolute <- stringr::str_detect(
    x,
    "^/"
  )

  windows_absolute |
    unc_absolute |
    unix_absolute
}


has_parent_escape <- function(x) {

  x <- normalize_slash(
    trimws(
      as.character(x)
    )
  )

  parts <- strsplit(
    x,
    "/",
    fixed = TRUE
  )[[1]]

  any(
    parts == ".."
  )
}


validate_relative_config_path <- function(
    value,
    key
) {

  if (
    length(value) != 1L ||
      is.na(value) ||
      trimws(value) == ""
  ) {
    stop(
      "Invalid canonical path for '",
      key,
      "': value is blank.",
      call. = FALSE
    )
  }

  value <- normalize_slash(
    trimws(value)
  )

  if (is_absolute_path(value)) {
    stop(
      "Invalid canonical path for '",
      key,
      "': absolute paths are not allowed: ",
      value,
      call. = FALSE
    )
  }

  if (has_parent_escape(value)) {
    stop(
      "Invalid canonical path for '",
      key,
      "': '..' path traversal is not allowed: ",
      value,
      call. = FALSE
    )
  }

  value <- stringr::str_replace_all(
    value,
    "/+",
    "/"
  )

  value <- stringr::str_replace(
    value,
    "^\\./",
    ""
  )

  value <- stringr::str_replace(
    value,
    "/$",
    ""
  )

  value
}


safe_backup <- function(
    path,
    backup_dir
) {

  if (!file.exists(path)) {
    stop(
      "Cannot back up missing file: ",
      path,
      call. = FALSE
    )
  }

  dir.create(
    backup_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  destination <- file.path(
    backup_dir,
    basename(path)
  )

  ok <- file.copy(
    path,
    destination,
    overwrite = TRUE
  )

  if (!isTRUE(ok)) {
    stop(
      "Failed to create backup for: ",
      path,
      call. = FALSE
    )
  }

  destination
}


write_text_utf8 <- function(
    text,
    path
) {

  con <- file(
    path,
    open = "w",
    encoding = "UTF-8"
  )

  on.exit(
    close(con),
    add = TRUE
  )

  writeLines(
    text,
    con = con,
    useBytes = TRUE
  )
}


safe_read_text <- function(path) {

  paste(
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
}


# ============================================================
# 2. Resolve repository root
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

path_utils_file <- file.path(
  repo_root,
  "R",
  "utils",
  "path_utils.R"
)

r29_file <- file.path(
  repo_root,
  "R",
  "29_run_author_directed_additional_analysis.R"
)

required_project_files <- c(
  config_file,
  path_utils_file,
  r29_file
)

missing_project_files <- required_project_files[
  !file.exists(
    required_project_files
  )
]

if (length(missing_project_files) > 0L) {
  stop(
    paste0(
      "R/31 cannot continue because required project file(s) are missing:\n- ",
      paste(
        normalize_slash(missing_project_files),
        collapse = "\n- "
      ),
      "\n\nRun R/31 from the repository root."
    ),
    call. = FALSE
  )
}


# ============================================================
# 3. Output / backup directories
# ============================================================

timestamp <- format(
  Sys.time(),
  "%Y%m%d_%H%M%S"
)

result_dir <- file.path(
  repo_root,
  "results",
  "portability"
)

backup_dir <- file.path(
  result_dir,
  "r31_backups",
  timestamp
)

dir.create(
  result_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  backup_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

report_file <- file.path(
  result_dir,
  "r31_config_driven_path_refactor_report.txt"
)

validation_file <- file.path(
  result_dir,
  "r31_configured_paths.csv"
)


# ============================================================
# 4. Read and validate current config
# ============================================================

config_before <- yaml::read_yaml(
  config_file
)

if (is.null(config_before)) {
  stop(
    "project_config.yml could not be parsed.",
    call. = FALSE
  )
}


canonical_defaults <- list(
  additional_analysis_raw = "data/raw/additional_analysis",
  additional_analysis_processed = "data/processed/additional_analysis",
  additional_analysis_results = "results/additional_analysis"
)


# ============================================================
# 5. Back up files BEFORE modification
# ============================================================

config_backup <- safe_backup(
  config_file,
  backup_dir
)

path_utils_backup <- safe_backup(
  path_utils_file,
  backup_dir
)

r29_backup <- safe_backup(
  r29_file,
  backup_dir
)


# ============================================================
# 6. Add canonical_paths to project_config.yml
# ============================================================

config_text <- safe_read_text(
  config_file
)

config_changed <- FALSE

if (is.null(config_before$canonical_paths)) {

  append_block <- c(
    "",
    "",
    "# ------------------------------------------------------------",
    "# Canonical toolkit paths",
    "# Paths are relative to the runtime project root.",
    "# Do not replace these with user-specific absolute paths.",
    "# ------------------------------------------------------------",
    "canonical_paths:",
    paste0(
      "  additional_analysis_raw: ",
      canonical_defaults$additional_analysis_raw
    ),
    paste0(
      "  additional_analysis_processed: ",
      canonical_defaults$additional_analysis_processed
    ),
    paste0(
      "  additional_analysis_results: ",
      canonical_defaults$additional_analysis_results
    )
  )

  existing_lines <- readLines(
    config_file,
    warn = FALSE,
    encoding = "UTF-8"
  )

  writeLines(
    c(
      existing_lines,
      append_block
    ),
    config_file,
    useBytes = TRUE
  )

  config_changed <- TRUE

} else {

  existing_keys <- names(
    config_before$canonical_paths
  )

  required_keys <- names(
    canonical_defaults
  )

  missing_keys <- setdiff(
    required_keys,
    existing_keys
  )

  if (length(missing_keys) > 0L) {
    stop(
      paste0(
        "project_config.yml already contains canonical_paths, but it is incomplete.\n",
        "Missing key(s): ",
        paste(missing_keys, collapse = ", "),
        "\n\n",
        "R/31 intentionally does not rewrite an existing YAML section automatically ",
        "because doing so could alter comments/formatting. Add the missing key(s), ",
        "then rerun R/31."
      ),
      call. = FALSE
    )
  }
}


# ============================================================
# 7. Reload config after edit and validate canonical paths
# ============================================================

config_after <- yaml::read_yaml(
  config_file
)

if (is.null(config_after$canonical_paths)) {
  stop(
    "R/31 failed to create/read canonical_paths in project_config.yml.",
    call. = FALSE
  )
}


canonical_values <- lapply(
  names(canonical_defaults),
  function(key) {

    validate_relative_config_path(
      config_after$canonical_paths[[key]],
      key
    )
  }
)

names(canonical_values) <- names(
  canonical_defaults
)


# ============================================================
# 8. Add reusable resolver to path_utils.R
# ============================================================

path_utils_text <- safe_read_text(
  path_utils_file
)

resolver_function_name <- "resolve_configured_project_path"

resolver_already_present <- stringr::str_detect(
  path_utils_text,
  stringr::fixed(
    paste0(
      resolver_function_name,
      " <- function"
    )
  )
)

path_utils_changed <- FALSE

if (!resolver_already_present) {

  resolver_code <- c(
    "",
    "",
    "# ============================================================",
    "# Config-driven canonical project path resolver",
    "# Added by R/31_refactor_config_driven_canonical_paths.R",
    "# ============================================================",
    "",
    "resolve_configured_project_path <- function(",
    "    config,",
    "    project_root,",
    "    key,",
    "    create = FALSE",
    ") {",
    "",
    "  if (is.null(config$canonical_paths)) {",
    "    stop(",
    "      \"project_config.yml is missing canonical_paths.\",",
    "      call. = FALSE",
    "    )",
    "  }",
    "",
    "  value <- config$canonical_paths[[key]]",
    "",
    "  if (is.null(value) || length(value) != 1L || is.na(value) || trimws(value) == \"\") {",
    "    stop(",
    "      \"Missing canonical path key: \",",
    "      key,",
    "      call. = FALSE",
    "    )",
    "  }",
    "",
    "  value <- gsub(\"\\\\\\\\\", \"/\", trimws(as.character(value)))",
    "",
    "  if (grepl(\"^[A-Za-z]:/\", value) || grepl(\"^/\", value) || grepl(\"^//\", value)) {",
    "    stop(",
    "      \"Canonical path must be relative to project root: \",",
    "      key,",
    "      \" = \",",
    "      value,",
    "      call. = FALSE",
    "    )",
    "  }",
    "",
    "  parts <- strsplit(value, \"/\", fixed = TRUE)[[1]]",
    "",
    "  if (any(parts == \"..\")) {",
    "    stop(",
    "      \"Canonical path cannot contain '..': \",",
    "      key,",
    "      \" = \",",
    "      value,",
    "      call. = FALSE",
    "    )",
    "  }",
    "",
    "  value <- gsub(\"/+\", \"/\", value)",
    "  value <- sub(\"^\\\\./\", \"\", value)",
    "  value <- sub(\"/$\", \"\", value)",
    "",
    "  root_normalized <- normalizePath(",
    "    project_root,",
    "    winslash = \"/\",",
    "    mustWork = TRUE",
    "  )",
    "",
    "  resolved <- file.path(",
    "    root_normalized,",
    "    value",
    "  )",
    "",
    "  resolved <- gsub(\"\\\\\\\\\", \"/\", resolved)",
    "",
    "  if (isTRUE(create)) {",
    "    dir.create(",
    "      resolved,",
    "      recursive = TRUE,",
    "      showWarnings = FALSE",
    "    )",
    "  }",
    "",
    "  resolved",
    "}",
    ""
  )

  existing_lines <- readLines(
    path_utils_file,
    warn = FALSE,
    encoding = "UTF-8"
  )

  writeLines(
    c(
      existing_lines,
      resolver_code
    ),
    path_utils_file,
    useBytes = TRUE
  )

  path_utils_changed <- TRUE
}


# ============================================================
# 9. Refactor R/29 hard-coded roots
# ============================================================

r29_text <- safe_read_text(
  r29_file
)

old_raw_block <- paste(
  c(
    'raw_root <- file.path(',
    '  paths$project_root,',
    '  "data",',
    '  "raw",',
    '  "additional_analysis"',
    ')'
  ),
  collapse = "\n"
)

old_processed_block <- paste(
  c(
    'processed_root <- file.path(',
    '  paths$project_root,',
    '  "data",',
    '  "processed",',
    '  "additional_analysis"',
    ')'
  ),
  collapse = "\n"
)

old_results_block <- paste(
  c(
    'results_root <- file.path(',
    '  paths$project_root,',
    '  "results",',
    '  "additional_analysis"',
    ')'
  ),
  collapse = "\n"
)


new_raw_block <- paste(
  c(
    'raw_root <- resolve_configured_project_path(',
    '  config = config,',
    '  project_root = paths$project_root,',
    '  key = "additional_analysis_raw",',
    '  create = TRUE',
    ')'
  ),
  collapse = "\n"
)

new_processed_block <- paste(
  c(
    'processed_root <- resolve_configured_project_path(',
    '  config = config,',
    '  project_root = paths$project_root,',
    '  key = "additional_analysis_processed",',
    '  create = TRUE',
    ')'
  ),
  collapse = "\n"
)

new_results_block <- paste(
  c(
    'results_root <- resolve_configured_project_path(',
    '  config = config,',
    '  project_root = paths$project_root,',
    '  key = "additional_analysis_results",',
    '  create = TRUE',
    ')'
  ),
  collapse = "\n"
)


r29_changed <- FALSE

# raw
if (stringr::str_detect(
  r29_text,
  stringr::fixed(old_raw_block)
)) {

  r29_text <- stringr::str_replace(
    r29_text,
    stringr::fixed(old_raw_block),
    new_raw_block
  )

  r29_changed <- TRUE

} else if (!stringr::str_detect(
  r29_text,
  stringr::fixed(
    'key = "additional_analysis_raw"'
  )
)) {

  stop(
    paste0(
      "R/31 could not locate the expected raw_root block in R/29.\n",
      "No changes were applied to that block.\n",
      "Restore from backup if needed: ",
      r29_backup
    ),
    call. = FALSE
  )
}


# processed
if (stringr::str_detect(
  r29_text,
  stringr::fixed(old_processed_block)
)) {

  r29_text <- stringr::str_replace(
    r29_text,
    stringr::fixed(old_processed_block),
    new_processed_block
  )

  r29_changed <- TRUE

} else if (!stringr::str_detect(
  r29_text,
  stringr::fixed(
    'key = "additional_analysis_processed"'
  )
)) {

  stop(
    "R/31 could not locate the expected processed_root block in R/29.",
    call. = FALSE
  )
}


# results
if (stringr::str_detect(
  r29_text,
  stringr::fixed(old_results_block)
)) {

  r29_text <- stringr::str_replace(
    r29_text,
    stringr::fixed(old_results_block),
    new_results_block
  )

  r29_changed <- TRUE

} else if (!stringr::str_detect(
  r29_text,
  stringr::fixed(
    'key = "additional_analysis_results"'
  )
)) {

  stop(
    "R/31 could not locate the expected results_root block in R/29.",
    call. = FALSE
  )
}


if (r29_changed) {

  write_text_utf8(
    r29_text,
    r29_file
  )
}


# ============================================================
# 10. Runtime validation using new resolver
# ============================================================

# Reload utilities so the newly appended resolver is available.
source(
  path_utils_file,
  local = FALSE
)

if (!exists(
  resolver_function_name,
  mode = "function"
)) {
  stop(
    "R/31 could not load resolve_configured_project_path() from path_utils.R.",
    call. = FALSE
  )
}


resolved_paths <- tibble::tibble(
  key = names(canonical_values),
  configured_relative_path = unlist(
    canonical_values,
    use.names = FALSE
  ),
  resolved_runtime_path = c(
    resolve_configured_project_path(
      config_after,
      repo_root,
      "additional_analysis_raw",
      create = TRUE
    ),
    resolve_configured_project_path(
      config_after,
      repo_root,
      "additional_analysis_processed",
      create = TRUE
    ),
    resolve_configured_project_path(
      config_after,
      repo_root,
      "additional_analysis_results",
      create = TRUE
    )
  )
)


# ============================================================
# 11. Ensure all resolved paths remain under project root
# ============================================================

root_prefix <- paste0(
  normalize_slash(
    normalizePath(
      repo_root,
      winslash = "/",
      mustWork = TRUE
    )
  ),
  "/"
)

resolved_paths <- resolved_paths |>
  dplyr::mutate(
    resolved_runtime_path = normalize_slash(
      resolved_runtime_path
    ),
    inside_project_root = startsWith(
      resolved_runtime_path,
      root_prefix
    ) |
      resolved_runtime_path == normalize_slash(repo_root),
    directory_exists = dir.exists(
      resolved_runtime_path
    )
  )


if (any(!resolved_paths$inside_project_root)) {
  stop(
    "R/31 validation failed: one or more configured paths resolve outside the project root.",
    call. = FALSE
  )
}


if (any(!resolved_paths$directory_exists)) {
  stop(
    "R/31 validation failed: one or more canonical directories were not created.",
    call. = FALSE
  )
}


readr::write_csv(
  resolved_paths,
  validation_file,
  na = ""
)


# ============================================================
# 12. Validate R/29 no longer contains old hard-coded blocks
# ============================================================

r29_after <- safe_read_text(
  r29_file
)

old_blocks_remaining <- c(
  stringr::str_detect(
    r29_after,
    stringr::fixed(old_raw_block)
  ),
  stringr::str_detect(
    r29_after,
    stringr::fixed(old_processed_block)
  ),
  stringr::str_detect(
    r29_after,
    stringr::fixed(old_results_block)
  )
)

if (any(old_blocks_remaining)) {
  stop(
    "R/31 FAIL: one or more old R/29 hard-coded path blocks remain.",
    call. = FALSE
  )
}


required_r29_keys <- c(
  "additional_analysis_raw",
  "additional_analysis_processed",
  "additional_analysis_results"
)

missing_r29_keys <- required_r29_keys[
  !vapply(
    required_r29_keys,
    function(key) {
      stringr::str_detect(
        r29_after,
        stringr::fixed(
          paste0(
            'key = "',
            key,
            '"'
          )
        )
      )
    },
    FUN.VALUE = logical(1)
  )
]

if (length(missing_r29_keys) > 0L) {
  stop(
    "R/31 FAIL: R/29 is missing config path key(s): ",
    paste(
      missing_r29_keys,
      collapse = ", "
    ),
    call. = FALSE
  )
}


# ============================================================
# 13. Portability guard
# ============================================================

forbidden_operational_patterns <- c(
  "(?i)C:[/\\\\]Users[/\\\\][^/\\\\\\s\"']+",
  "(?i)/Users/[^/\\s\"']+",
  "(?i)/home/[^/\\s\"']+"
)

files_to_guard <- c(
  config_file,
  path_utils_file,
  r29_file
)

guard_findings <- list()

for (file_i in files_to_guard) {

  lines_i <- readLines(
    file_i,
    warn = FALSE,
    encoding = "UTF-8"
  )

  for (pattern_i in forbidden_operational_patterns) {

    hit_i <- which(
      stringr::str_detect(
        lines_i,
        stringr::regex(
          pattern_i
        )
      )
    )

    if (length(hit_i) > 0L) {

      # Ignore generic explanatory/pattern lines.
      for (line_i in hit_i) {

        line_text_i <- stringr::str_squish(
          lines_i[[line_i]]
        )

        if (stringr::str_detect(
          line_text_i,
          stringr::regex(
            "(?i)(pattern|example|hard-coded|absolute path|do not)"
          )
        )) {
          next
        }

        guard_findings[[length(guard_findings) + 1L]] <- tibble::tibble(
          file = normalize_slash(
            file_i
          ),
          line = line_i,
          text = line_text_i
        )
      }
    }
  }
}


guard_df <- if (length(guard_findings) == 0L) {
  tibble::tibble(
    file = character(),
    line = integer(),
    text = character()
  )
} else {
  dplyr::bind_rows(
    guard_findings
  )
}


if (nrow(guard_df) > 0L) {
  stop(
    paste0(
      "R/31 portability guard detected user-specific absolute path(s):\n",
      paste(
        paste0(
          guard_df$file,
          ":",
          guard_df$line,
          " — ",
          guard_df$text
        ),
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 14. Write report
# ============================================================

status <- "PASS"

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/31 — Config-driven Canonical Path Refactor",
  "",
  paste0(
    "Runtime repository root: ",
    normalize_slash(repo_root)
  ),
  "",
  "CHANGES",
  "--------------------------------------------------",
  paste0(
    "project_config.yml canonical_paths added/confirmed: ",
    ifelse(config_changed, "ADDED", "ALREADY PRESENT")
  ),
  paste0(
    "path_utils.R resolver added/confirmed: ",
    ifelse(path_utils_changed, "ADDED", "ALREADY PRESENT")
  ),
  paste0(
    "R/29 path blocks refactored/confirmed: ",
    ifelse(r29_changed, "REFACTORED", "ALREADY CONFIG-DRIVEN")
  ),
  "",
  "CANONICAL CONFIG",
  "--------------------------------------------------",
  paste0(
    "additional_analysis_raw: ",
    canonical_values$additional_analysis_raw
  ),
  paste0(
    "additional_analysis_processed: ",
    canonical_values$additional_analysis_processed
  ),
  paste0(
    "additional_analysis_results: ",
    canonical_values$additional_analysis_results
  ),
  "",
  "VALIDATION",
  "--------------------------------------------------",
  "Configured paths are relative: PASS",
  "No '..' traversal: PASS",
  "All resolved paths remain under project root: PASS",
  "Canonical directories exist: PASS",
  "R/29 old hard-coded additional-analysis path blocks removed: PASS",
  "R/29 reads all three canonical path keys: PASS",
  "User-specific absolute path guard: PASS",
  "",
  "BACKUPS",
  "--------------------------------------------------",
  paste0(
    "Config backup: ",
    normalize_slash(config_backup)
  ),
  paste0(
    "path_utils backup: ",
    normalize_slash(path_utils_backup)
  ),
  paste0(
    "R/29 backup: ",
    normalize_slash(r29_backup)
  ),
  "",
  "USER EXPERIENCE",
  "--------------------------------------------------",
  "The user does not need to know or enter any folder name.",
  "R/29 obtains the current project root at runtime and reads canonical",
  "workspace paths from project_config.yml.",
  "",
  "NEXT ARCHITECTURE STEP",
  "--------------------------------------------------",
  "Runner upload routing -> current active analysis workspace.",
  "",
  "STATUS",
  "--------------------------------------------------",
  paste0(
    "R/31: ",
    status
  )
)

writeLines(
  report_lines,
  report_file
)


# ============================================================
# 15. Console summary
# ============================================================

cat("\n")
cat("============================================================\n")
cat("R/31 — Config-driven Canonical Path Refactor\n")
cat("============================================================\n\n")

cat(
  "project_config.yml: ",
  ifelse(
    config_changed,
    "canonical_paths ADDED",
    "canonical_paths already present"
  ),
  "\n",
  sep = ""
)

cat(
  "path_utils.R: ",
  ifelse(
    path_utils_changed,
    "resolver ADDED",
    "resolver already present"
  ),
  "\n",
  sep = ""
)

cat(
  "R/29: ",
  ifelse(
    r29_changed,
    "REFRACTORED to config-driven paths",
    "already config-driven"
  ),
  "\n\n",
  sep = ""
)

print(
  resolved_paths,
  n = Inf,
  width = Inf
)

cat("\n")
cat("PASS: R/31 completed successfully.\n")
cat(
  "Report: ",
  normalize_slash(report_file),
  "\n",
  sep = ""
)

cat(
  "Backup directory: ",
  normalize_slash(backup_dir),
  "\n",
  sep = ""
)

cat("\n")
cat(
  "Next: Runner upload routing to the active analysis workspace.\n"
)

invisible(
  list(
    status = status,
    configured_paths = resolved_paths,
    report = report_file,
    backup_directory = backup_dir
  )
)
