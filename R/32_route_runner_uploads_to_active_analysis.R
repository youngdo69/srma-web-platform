# ============================================================
# Universal SR-MA Agent Toolkit
# R/32_route_runner_uploads_to_active_analysis.R
#
# R/32 — Runner Upload Routing to Active Analysis Workspace
#
# PURPOSE
# ------------------------------------------------------------
# Route files uploaded through the future Runner/UI into the
# currently active R/29 additional-analysis workspace WITHOUT
# asking the user to view, type, or select any folder path.
#
# CANONICAL FLOW
# ------------------------------------------------------------
# Runner/UI upload action
#      ↓
# Runner receives temporary uploaded file path(s)
#      ↓
# R/32 route_uploaded_files_to_active_analysis()
#      ↓
# Read project_config.yml
#      ↓
# Resolve config-driven additional-analysis raw root
#      ↓
# Read ACTIVE_ANALYSIS_ID.txt
#      ↓
# Validate active analysis_state.json
#      ↓
# Copy uploaded files into:
#   <configured additional_analysis_raw>/<active_analysis_id>/
#      ↓
# Write/update upload manifest
#      ↓
# Return routed file paths to Runner
#      ↓
# Runner continues/resumes R/29 automatically
#
# IMPORTANT
# ------------------------------------------------------------
# - User never sees or enters a folder name.
# - No user-specific absolute path is stored in config.
# - R/32 accepts only Runner-provided temporary/local file paths.
# - Supported files:
#     CSV, XLSX, PDF, DOCX, DOC
# - Duplicate filenames are handled deterministically.
# - Existing source files are never silently overwritten.
# - Upload provenance is recorded.
#
# RUNNER INTEGRATION CONTRACT
# ------------------------------------------------------------
# Preferred:
#
#   routed <- route_uploaded_files_to_active_analysis(
#     uploaded_files = runner_temp_paths
#   )
#
#   # then resume R/29
#
# Standalone validation:
#
#   Sys.setenv(
#     SRMA_UPLOADED_FILES = paste(
#       c("C:/temp/file1.pdf", "C:/temp/supplement.docx"),
#       collapse = ";"
#     )
#   )
#
#   source("R/32_route_runner_uploads_to_active_analysis.R")
#
# If SRMA_UPLOADED_FILES is absent, sourcing R/32 only loads the
# routing functions and performs architecture checks.
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
  "jsonlite",
  "digest"
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


safe_chr <- function(x) {

  if (
    length(x) == 0L ||
      is.null(x) ||
      is.na(x[[1]])
  ) {
    return("")
  }

  trimws(
    as.character(
      x[[1]]
    )
  )
}


now_text <- function() {
  format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )
}


safe_file_name <- function(x) {

  name <- basename(
    normalize_slash(
      x
    )
  )

  name <- stringr::str_replace_all(
    name,
    "[\\x00-\\x1F]",
    ""
  )

  name <- stringr::str_replace_all(
    name,
    "[<>:\"/\\\\|?*]",
    "_"
  )

  name <- stringr::str_squish(
    name
  )

  if (
    name == "" ||
      name %in% c(
        ".",
        ".."
      )
  ) {
    stop(
      "Invalid uploaded filename.",
      call. = FALSE
    )
  }

  name
}


is_absolute_path <- function(x) {

  x <- normalize_slash(
    safe_chr(
      x
    )
  )

  stringr::str_detect(
    x,
    "^[A-Za-z]:/"
  ) ||
    stringr::str_detect(
      x,
      "^//"
    ) ||
    stringr::str_detect(
      x,
      "^/"
    )
}


# ============================================================
# 2. Resolve repository root and canonical files
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

if (!file.exists(config_file)) {
  stop(
    paste0(
      "config/project_config.yml was not found.\n",
      "Run R/32 from the repository root.\n",
      "Current directory: ",
      repo_root
    ),
    call. = FALSE
  )
}

if (!file.exists(path_utils_file)) {
  stop(
    "R/utils/path_utils.R was not found.",
    call. = FALSE
  )
}


# ============================================================
# 3. Load config + path resolver
# ============================================================

config <- yaml::read_yaml(
  config_file
)

if (is.null(config)) {
  stop(
    "project_config.yml could not be parsed.",
    call. = FALSE
  )
}

source(
  path_utils_file,
  local = FALSE
)

if (!exists(
  "resolve_configured_project_path",
  mode = "function"
)) {
  stop(
    paste0(
      "resolve_configured_project_path() was not found in path_utils.R.\n",
      "R/31 must PASS before R/32."
    ),
    call. = FALSE
  )
}


# ============================================================
# 4. Resolve canonical additional-analysis roots
# ============================================================

additional_raw_root <- resolve_configured_project_path(
  config = config,
  project_root = repo_root,
  key = "additional_analysis_raw",
  create = TRUE
)

additional_results_root <- resolve_configured_project_path(
  config = config,
  project_root = repo_root,
  key = "additional_analysis_results",
  create = TRUE
)

active_id_file <- file.path(
  additional_results_root,
  "ACTIVE_ANALYSIS_ID.txt"
)


# ============================================================
# 5. Supported file types
# ============================================================

supported_extensions <- c(
  "csv",
  "xlsx",
  "pdf",
  "docx",
  "doc"
)


validate_uploaded_file <- function(path) {

  if (
    length(path) != 1L ||
      is.na(path) ||
      trimws(path) == ""
  ) {
    stop(
      "Uploaded file path is blank.",
      call. = FALSE
    )
  }

  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )

  info <- file.info(
    path
  )

  if (
    isTRUE(
      info$isdir
    )
  ) {
    stop(
      "Uploaded path is a directory, not a file: ",
      path,
      call. = FALSE
    )
  }

  if (
    is.na(
      info$size
    ) ||
      info$size <= 0
  ) {
    stop(
      "Uploaded file is empty or unreadable: ",
      path,
      call. = FALSE
    )
  }

  ext <- tolower(
    tools::file_ext(
      path
    )
  )

  if (!ext %in% supported_extensions) {
    stop(
      paste0(
        "Unsupported uploaded file type: .",
        ext,
        "\nSupported: ",
        paste(
          toupper(
            supported_extensions
          ),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  list(
    path = path,
    extension = ext,
    size_bytes = as.numeric(
      info$size
    )
  )
}


# ============================================================
# 6. Active-analysis resolution
# ============================================================

read_active_analysis <- function() {

  if (!file.exists(active_id_file)) {
    stop(
      paste0(
        "No active additional analysis was found.\n",
        "R/29 must create an active analysis before files can be routed."
      ),
      call. = FALSE
    )
  }

  analysis_id <- safe_chr(
    readLines(
      active_id_file,
      warn = FALSE,
      encoding = "UTF-8"
    )
  )

  if (analysis_id == "") {
    stop(
      "ACTIVE_ANALYSIS_ID.txt is blank.",
      call. = FALSE
    )
  }

  # analysis_id must be a simple internal identifier, never a path.
  if (
    stringr::str_detect(
      analysis_id,
      "[/\\\\]"
    ) ||
      analysis_id %in% c(
        ".",
        ".."
      )
  ) {
    stop(
      "Invalid active analysis ID: ",
      analysis_id,
      call. = FALSE
    )
  }

  raw_workspace <- file.path(
    additional_raw_root,
    analysis_id
  )

  results_workspace <- file.path(
    additional_results_root,
    analysis_id
  )

  state_file <- file.path(
    results_workspace,
    "analysis_state.json"
  )

  if (!dir.exists(results_workspace)) {
    stop(
      paste0(
        "Active analysis results workspace does not exist: ",
        results_workspace
      ),
      call. = FALSE
    )
  }

  if (!file.exists(state_file)) {
    stop(
      paste0(
        "Active analysis state file is missing: ",
        state_file
      ),
      call. = FALSE
    )
  }

  state <- jsonlite::read_json(
    state_file,
    simplifyVector = TRUE
  )

  state_analysis_id <- safe_chr(
    state$analysis_id
  )

  if (
    state_analysis_id == "" ||
      state_analysis_id != analysis_id
  ) {
    stop(
      paste0(
        "Active analysis ID mismatch.\n",
        "ACTIVE_ANALYSIS_ID.txt: ",
        analysis_id,
        "\nanalysis_state.json: ",
        state_analysis_id
      ),
      call. = FALSE
    )
  }

  state_status <- safe_chr(
    state$status
  )

  allowed_upload_states <- c(
    "WAITING_FOR_FILES",
    "FILES_AVAILABLE"
  )

  if (!state_status %in% allowed_upload_states) {
    stop(
      paste0(
        "The active analysis is not currently accepting uploads.\n",
        "Analysis ID: ",
        analysis_id,
        "\nState: ",
        state_status,
        "\nAllowed upload states: ",
        paste(
          allowed_upload_states,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  dir.create(
    raw_workspace,
    recursive = TRUE,
    showWarnings = FALSE
  )

  list(
    analysis_id = analysis_id,
    state = state,
    state_file = state_file,
    raw_workspace = normalizePath(
      raw_workspace,
      winslash = "/",
      mustWork = TRUE
    ),
    results_workspace = normalizePath(
      results_workspace,
      winslash = "/",
      mustWork = TRUE
    )
  )
}


# ============================================================
# 7. Collision-safe target naming
# ============================================================

build_target_path <- function(
    source_path,
    destination_dir
) {

  original_name <- safe_file_name(
    source_path
  )

  candidate <- file.path(
    destination_dir,
    original_name
  )

  if (!file.exists(candidate)) {
    return(
      candidate
    )
  }

  # If exact same content is already present, reuse it rather than
  # generating a duplicate copy.
  source_hash <- digest::digest(
    file = source_path,
    algo = "sha256"
  )

  existing_hash <- digest::digest(
    file = candidate,
    algo = "sha256"
  )

  if (identical(
    source_hash,
    existing_hash
  )) {
    return(
      candidate
    )
  }

  stem <- tools::file_path_sans_ext(
    original_name
  )

  ext <- tools::file_ext(
    original_name
  )

  counter <- 2L

  repeat {

    new_name <- if (ext == "") {
      paste0(
        stem,
        "_",
        counter
      )
    } else {
      paste0(
        stem,
        "_",
        counter,
        ".",
        ext
      )
    }

    candidate <- file.path(
      destination_dir,
      new_name
    )

    if (!file.exists(candidate)) {
      return(
        candidate
      )
    }

    counter <- counter + 1L
  }
}


# ============================================================
# 8. Upload manifest helpers
# ============================================================

upload_manifest_path <- function(
    results_workspace
) {
  file.path(
    results_workspace,
    "runner_upload_manifest.csv"
  )
}


read_existing_manifest <- function(
    path
) {

  if (!file.exists(path)) {
    return(
      tibble::tibble(
        analysis_id = character(),
        uploaded_at = character(),
        original_filename = character(),
        routed_filename = character(),
        extension = character(),
        size_bytes = numeric(),
        sha256 = character(),
        routing_status = character()
      )
    )
  }

  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default = readr::col_character(),
      size_bytes = readr::col_double()
    )
  )
}


# ============================================================
# 9. Core routing function
# ============================================================

route_uploaded_files_to_active_analysis <- function(
    uploaded_files
) {

  if (
    missing(uploaded_files) ||
      is.null(uploaded_files) ||
      length(uploaded_files) == 0L
  ) {
    stop(
      "No uploaded file path was supplied to R/32.",
      call. = FALSE
    )
  }

  uploaded_files <- as.character(
    uploaded_files
  )

  uploaded_files <- uploaded_files[
    !is.na(
      uploaded_files
    ) &
      trimws(
        uploaded_files
      ) != ""
  ]

  if (length(uploaded_files) == 0L) {
    stop(
      "No valid uploaded file path was supplied.",
      call. = FALSE
    )
  }

  active <- read_active_analysis()

  cat("\n")
  cat("============================================================\n")
  cat("R/32 — Runner Upload Routing\n")
  cat("============================================================\n\n")

  cat(
    "Active analysis: ",
    active$analysis_id,
    "\n",
    sep = ""
  )

  cat(
    "Files received from Runner: ",
    length(uploaded_files),
    "\n\n",
    sep = ""
  )

  routed_rows <- list()

  for (
    i in seq_along(
      uploaded_files
    )
  ) {

    validated <- validate_uploaded_file(
      uploaded_files[[i]]
    )

    source_path <- validated$path

    target_path <- build_target_path(
      source_path,
      active$raw_workspace
    )

    source_hash <- digest::digest(
      file = source_path,
      algo = "sha256"
    )

    already_same_file <- FALSE

    if (file.exists(target_path)) {

      target_hash_before <- digest::digest(
        file = target_path,
        algo = "sha256"
      )

      already_same_file <- identical(
        source_hash,
        target_hash_before
      )
    }

    if (!already_same_file) {

      copied <- file.copy(
        from = source_path,
        to = target_path,
        overwrite = FALSE,
        copy.mode = TRUE,
        copy.date = TRUE
      )

      if (!isTRUE(copied)) {
        stop(
          "Failed to route uploaded file: ",
          basename(source_path),
          call. = FALSE
        )
      }
    }

    target_path <- normalizePath(
      target_path,
      winslash = "/",
      mustWork = TRUE
    )

    target_hash <- digest::digest(
      file = target_path,
      algo = "sha256"
    )

    if (!identical(
      source_hash,
      target_hash
    )) {
      stop(
        "SHA-256 verification failed after routing: ",
        basename(source_path),
        call. = FALSE
      )
    }

    routing_status <- if (
      already_same_file
    ) {
      "ALREADY_PRESENT_IDENTICAL"
    } else {
      "ROUTED"
    }

    routed_rows[[length(routed_rows) + 1L]] <- tibble::tibble(
      analysis_id = active$analysis_id,
      uploaded_at = now_text(),
      original_filename = basename(
        source_path
      ),
      routed_filename = basename(
        target_path
      ),
      extension = validated$extension,
      size_bytes = as.numeric(
        file.info(
          target_path
        )$size
      ),
      sha256 = target_hash,
      routing_status = routing_status
    )

    cat(
      "[",
      routing_status,
      "] ",
      basename(source_path),
      " -> ",
      basename(target_path),
      "\n",
      sep = ""
    )
  }

  routed_manifest <- dplyr::bind_rows(
    routed_rows
  )

  manifest_file <- upload_manifest_path(
    active$results_workspace
  )

  existing_manifest <- read_existing_manifest(
    manifest_file
  )

  combined_manifest <- dplyr::bind_rows(
    existing_manifest,
    routed_manifest
  )

  readr::write_csv(
    combined_manifest,
    manifest_file,
    na = ""
  )

  routed_paths <- file.path(
    active$raw_workspace,
    routed_manifest$routed_filename
  )

  routed_paths <- normalizePath(
    routed_paths,
    winslash = "/",
    mustWork = TRUE
  )

  cat("\n")
  cat(
    "PASS: ",
    length(routed_paths),
    " uploaded file(s) routed to the active analysis workspace.\n",
    sep = ""
  )

  cat(
    "Runner may now resume R/29 automatically.\n"
  )

  invisible(
    list(
      status = "PASS",
      analysis_id = active$analysis_id,
      routed_files = routed_paths,
      manifest_file = manifest_file,
      active_state = active$state$status
    )
  )
}


# ============================================================
# 10. Architecture validation
# ============================================================

validate_r32_architecture <- function() {

  checks <- tibble::tibble(
    check = c(
      "project_config.yml exists",
      "path_utils.R exists",
      "resolve_configured_project_path available",
      "additional_analysis_raw resolves",
      "additional_analysis_results resolves",
      "No hard-coded user-specific root required"
    ),
    pass = c(
      file.exists(config_file),
      file.exists(path_utils_file),
      exists(
        "resolve_configured_project_path",
        mode = "function"
      ),
      dir.exists(additional_raw_root),
      dir.exists(additional_results_root),
      TRUE
    )
  )

  if (any(!checks$pass)) {
    stop(
      paste0(
        "R/32 architecture validation failed:\n",
        paste(
          checks$check[
            !checks$pass
          ],
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }

  checks
}


r32_architecture_checks <- validate_r32_architecture()


# ============================================================
# 11. Optional standalone / Runner environment invocation
# ============================================================

parse_uploaded_files_env <- function() {

  raw <- safe_chr(
    Sys.getenv(
      "SRMA_UPLOADED_FILES",
      unset = ""
    )
  )

  if (raw == "") {
    return(
      character(0)
    )
  }

  # Semicolon is used deliberately because Windows absolute paths
  # contain ':' and may contain spaces.
  files <- strsplit(
    raw,
    ";",
    fixed = TRUE
  )[[1]]

  files <- trimws(
    files
  )

  files[
    files != ""
  ]
}


env_uploaded_files <- parse_uploaded_files_env()


if (length(env_uploaded_files) > 0L) {

  r32_result <- route_uploaded_files_to_active_analysis(
    uploaded_files = env_uploaded_files
  )

  invisible(
    r32_result
  )

} else {

  cat("\n")
  cat("============================================================\n")
  cat("R/32 — Runner Upload Routing Module\n")
  cat("============================================================\n\n")

  cat(
    "PASS: R/32 module loaded and architecture validated.\n"
  )

  cat(
    "No SRMA_UPLOADED_FILES value was supplied, so no file was routed.\n"
  )

  cat(
    "Runner integration function:\n"
  )

  cat(
    "  route_uploaded_files_to_active_analysis(uploaded_files)\n\n"
  )

  cat(
    "The user does not need to know any folder path.\n"
  )

  invisible(
    list(
      status = "READY",
      architecture_checks = r32_architecture_checks
    )
  )
}
