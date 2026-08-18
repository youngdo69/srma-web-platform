# ============================================================
# TASK-016 — One-time PROSPERO legacy-artifact migration helper
#
# Purpose:
#   Copy an explicitly selected legacy PROSPERO prepared form
#   into the active project's canonical recovery path:
#
#   results/registration/prospero/
#
# This script performs NO repository-wide artifact discovery.
# The source file must be supplied explicitly by the user.
# ============================================================

if (!requireNamespace("yaml", quietly = TRUE)) {
  stop('Package "yaml" is required.', call. = FALSE)
}

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

source_file <- trimws(
  Sys.getenv(
    "SRMA_LEGACY_PROSPERO_FILE",
    unset = ""
  )
)

if (!nzchar(source_file)) {
  source_file <- trimws(
    readline(
      paste0(
        "Exact legacy PROSPERO file to migrate ",
        "(prospero_final.xlsx or prospero_autofilled.xlsx): "
      )
    )
  )
}

if (!nzchar(source_file) || !file.exists(source_file)) {
  stop(
    "The explicitly supplied legacy PROSPERO file does not exist: ",
    source_file,
    call. = FALSE
  )
}

source_name <- basename(source_file)

if (!source_name %in% c(
  "prospero_final.xlsx",
  "prospero_autofilled.xlsx"
)) {
  stop(
    "Legacy source basename must be prospero_final.xlsx or prospero_autofilled.xlsx.",
    call. = FALSE
  )
}

if (
  is.na(file.info(source_file)$size) ||
  file.info(source_file)$size <= 0
) {
  stop(
    "Legacy PROSPERO source file is empty.",
    call. = FALSE
  )
}

target_dir <- file.path(
  paths$project_root,
  "results",
  "registration",
  "prospero"
)

dir.create(
  target_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

target_file <- file.path(
  target_dir,
  source_name
)

ok <- file.copy(
  source_file,
  target_file,
  overwrite = TRUE,
  copy.mode = TRUE,
  copy.date = TRUE
)

if (!isTRUE(ok) || !file.exists(target_file)) {
  stop(
    "Failed to copy PROSPERO artifact to canonical active-project path.",
    call. = FALSE
  )
}

source_md5 <- unname(tools::md5sum(source_file))
target_md5 <- unname(tools::md5sum(target_file))

if (!identical(source_md5, target_md5)) {
  stop(
    "PROSPERO migration checksum mismatch.",
    call. = FALSE
  )
}

cat("\nPASS: PROSPERO legacy artifact migrated to canonical active-project path.\n")
cat("Project ID: ", get_project_id(config), "\n", sep = "")
cat("Source: ", normalizePath(source_file, winslash = "/", mustWork = TRUE), "\n", sep = "")
cat("Target: ", normalizePath(target_file, winslash = "/", mustWork = TRUE), "\n", sep = "")
cat("MD5: ", target_md5, "\n", sep = "")

invisible(
  list(
    project_id = get_project_id(config),
    source = source_file,
    target = target_file,
    md5 = target_md5
  )
)
