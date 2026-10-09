# ============================================================
# Universal SR-MA Agent Toolkit
# ONE-TIME MAINTAINER UTILITY
# Sync canonical document/PROSPERO dependencies into renv.lock
#
# Direct workflow dependencies covered here:
#   - officer
#   - flextable
#   - openxlsx
#
# Run from the MASTER TOOLKIT ROOT only.
# This is maintenance, not a workflow stage.
# ============================================================

required_packages <- c(
  "officer",
  "flextable",
  "openxlsx"
)

repo_root <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

lockfile <- file.path(
  repo_root,
  "renv.lock"
)

runner_file <- file.path(
  repo_root,
  "R",
  "run_universal_srma_workflow.R"
)

r41_candidates <- list.files(
  file.path(repo_root, "R"),
  pattern = "^41_.*\\.R$",
  full.names = TRUE
)

prospero_candidates <- c(
  file.path(repo_root, "R", "create_and_autofill_prospero.R"),
  file.path(repo_root, "web", "adapter", "prospero.R")
)

if (
  !file.exists(lockfile) ||
    !file.exists(runner_file) ||
    length(r41_candidates) == 0L ||
    !any(file.exists(prospero_candidates))
) {
  stop(
    paste0(
      "Run this utility from the MASTER Universal SR-MA Toolkit root.\n",
      "Expected: renv.lock, R/run_universal_srma_workflow.R, an R/41*.R module, ",
      "and a canonical/Web PROSPERO module."
    ),
    call. = FALSE
  )
}

if (!requireNamespace("renv", quietly = TRUE)) {
  cat("Installing renv bootstrap package...\n")
  install.packages(
    "renv",
    repos = "https://cloud.r-project.org"
  )
}

if (!requireNamespace("renv", quietly = TRUE)) {
  stop(
    "The renv bootstrap package could not be installed.",
    call. = FALSE
  )
}

cat(
  "\n============================================================\n",
  "Universal SR-MA Toolkit — Canonical Dependency Sync\n",
  "============================================================\n",
  "Toolkit root: ", repo_root, "\n",
  "Direct workflow packages: ",
  paste(required_packages, collapse = ", "),
  "\n\n",
  sep = ""
)

renv::load(
  project = repo_root,
  quiet = TRUE
)

cat(
  "Installing/updating required packages inside the toolkit renv library...\n"
)

renv::install(
  required_packages,
  project = repo_root,
  prompt = FALSE
)

missing_after_install <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_after_install) > 0L) {
  stop(
    "Dependency installation failed for:\n- ",
    paste(
      missing_after_install,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}

cat(
  "Updating renv.lock for the direct workflow dependency set only...\n"
)

renv::snapshot(
  project = repo_root,
  packages = required_packages,
  prompt = FALSE
)

if (!file.exists(lockfile)) {
  stop(
    "renv::snapshot() did not produce renv.lock.",
    call. = FALSE
  )
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop(
    "jsonlite is required to verify renv.lock after snapshot.",
    call. = FALSE
  )
}

lock <- jsonlite::read_json(
  lockfile,
  simplifyVector = FALSE
)

locked_packages <- names(lock$Packages)

missing_from_lock <- setdiff(
  required_packages,
  locked_packages
)

if (length(missing_from_lock) > 0L) {
  stop(
    paste0(
      "renv.lock still does not contain required package(s):\n- ",
      paste(
        missing_from_lock,
        collapse = "\n- "
      )
    ),
    call. = FALSE
  )
}

locked_versions <- vapply(
  required_packages,
  function(pkg) {
    as.character(lock$Packages[[pkg]]$Version)
  },
  character(1)
)

cat(
  "\nPASS: Canonical renv.lock contains all direct document/PROSPERO dependencies.\n",
  paste0(
    "  ",
    required_packages,
    " ",
    locked_versions,
    collapse = "\n"
  ),
  "\n\nReview the renv.lock diff before committing the canonical dependency update.\n",
  sep = ""
)
