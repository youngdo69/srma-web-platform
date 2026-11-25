# ============================================================
# Universal SR-MA Agent Toolkit
# ONE-TIME MAINTAINER UTILITY
# Sync DOCX core dependencies into renv.lock
#
# Purpose:
#   - install officer + flextable into the toolkit renv project library
#   - refresh the canonical renv.lock from the current toolkit sources
#   - verify both packages are present in the project library and lockfile
#
# Run from the MASTER TOOLKIT ROOT only.
# After PASS, this helper can be deleted; it is not a workflow stage.
# ============================================================

required_docx_packages <- c(
  "officer",
  "flextable"
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
  file.path(
    repo_root,
    "R"
  ),
  pattern = "^41_.*\\.R$",
  full.names = TRUE
)

if (
  !file.exists(lockfile) ||
    !file.exists(runner_file) ||
    length(r41_candidates) == 0L
) {
  stop(
    paste0(
      "This utility must be run from the MASTER Universal SR-MA Toolkit root.\n",
      "Expected: renv.lock, R/run_universal_srma_workflow.R, and an R/41*.R module."
    ),
    call. = FALSE
  )
}

if (!requireNamespace("renv", quietly = TRUE)) {
  cat(
    "Installing renv bootstrap package...\n"
  )

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
  "Universal SR-MA Toolkit — Dependency Lockfile Sync\n",
  "============================================================\n",
  "Toolkit root: ",
  repo_root,
  "\n",
  "Target core DOCX packages: ",
  paste(
    required_docx_packages,
    collapse = ", "
  ),
  "\n\n",
  sep = ""
)

renv::load(
  project = repo_root,
  quiet = TRUE
)

cat(
  "Installing/updating the required DOCX packages inside the toolkit renv library...\n"
)

renv::install(
  required_docx_packages,
  project = repo_root,
  prompt = FALSE
)

missing_after_install <- required_docx_packages[
  !vapply(
    required_docx_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_after_install) > 0L) {
  stop(
    "DOCX dependency installation failed for:\n- ",
    paste(
      missing_after_install,
      collapse = "\n- "
    ),
    call. = FALSE
  )
}

cat(
  "Refreshing canonical renv.lock from the current toolkit dependency graph...\n"
)

renv::snapshot(
  project = repo_root,
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
    paste0(
      "jsonlite is required to verify renv.lock after snapshot. ",
      "It should already be part of the toolkit environment."
    ),
    call. = FALSE
  )
}

lock <- jsonlite::read_json(
  lockfile,
  simplifyVector = FALSE
)

locked_packages <- names(
  lock$Packages
)

missing_from_lock <- setdiff(
  required_docx_packages,
  locked_packages
)

if (length(missing_from_lock) > 0L) {
  stop(
    paste0(
      "renv.lock was updated but still does not contain required DOCX package(s):\n- ",
      paste(
        missing_from_lock,
        collapse = "\n- "
      ),
      "\nCheck that the canonical R/41 module is present under R/ and rerun."
    ),
    call. = FALSE
  )
}

locked_versions <- vapply(
  required_docx_packages,
  function(pkg) {
    as.character(
      lock$Packages[[pkg]]$Version
    )
  },
  character(1)
)

cat(
  "\nPASS: Canonical renv.lock now contains the R/41 DOCX dependencies.\n",
  paste0(
    "  ",
    required_docx_packages,
    " ",
    locked_versions,
    collapse = "\n"
  ),
  "\n\nNext:\n",
  "  1. Replace START_HERE.R with the dependency-fixed version.\n",
  "  2. Run: options(srma.start_here.environment_only = TRUE)\n",
  "  3. Run: source(\"START_HERE.R\")\n",
  "  4. Confirm required workflow packages PASS.\n",
  "  5. Run: options(srma.start_here.environment_only = NULL)\n",
  sep = ""
)
