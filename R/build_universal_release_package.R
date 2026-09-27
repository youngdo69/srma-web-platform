# ============================================================
# Universal SR-MA Agent Toolkit
# R/build_universal_release_package.R
#
# Builds a neutral distributable toolkit without project/runtime
# artifacts. It does not modify the working repository.
# ============================================================

srma_release_find_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    if (
      file.exists(file.path(current, "START_HERE.R")) &&
      file.exists(file.path(current, "R", "run_universal_srma_workflow.R"))
    ) {
      return(current)
    }

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop("Could not find toolkit root.", call. = FALSE)
    }
    current <- parent
  }
}

srma_release_copy_tree <- function(source, destination) {
  if (!dir.exists(source)) return(invisible(FALSE))

  files <- list.files(
    source,
    recursive = TRUE,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )

  files <- files[!dir.exists(files)]

  for (src in files) {
    rel <- substring(
      normalizePath(src, winslash = "/", mustWork = TRUE),
      nchar(normalizePath(source, winslash = "/", mustWork = TRUE)) + 2L
    )
    dst <- file.path(destination, rel)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(src, dst, overwrite = TRUE, copy.mode = TRUE)) {
      stop("Failed to copy release file: ", src, call. = FALSE)
    }
  }

  invisible(TRUE)
}

build_universal_release_package <- function(
    output_directory = file.path("dist", "srma-agent-toolkit-release"),
    overwrite = TRUE
) {
  root <- srma_release_find_root()
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)

  output_directory <- normalizePath(
    output_directory,
    winslash = "/",
    mustWork = FALSE
  )

  if (dir.exists(output_directory)) {
    if (!isTRUE(overwrite)) {
      stop("Release output already exists: ", output_directory, call. = FALSE)
    }
    unlink(output_directory, recursive = TRUE, force = TRUE)
  }

  dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

  # Universal source directories. Runtime/project directories are intentionally
  # absent: data/, results/, projects/, search_strategies/, tasks/, plans/,
  # reviews/, config/generated/, config/archive/, and backups.
  source_directories <- c(
    "R",
    "agents",
    "governance",
    "harnesses",
    "prompts",
    "templates",
    "examples",
    "renv"
  )

  for (directory in source_directories) {
    srma_release_copy_tree(
      file.path(root, directory),
      file.path(output_directory, directory)
    )
  }

  # Remove local renv package libraries/caches if they exist.
  unlink(
    file.path(output_directory, "renv", "library"),
    recursive = TRUE,
    force = TRUE
  )
  unlink(
    file.path(output_directory, "renv", "staging"),
    recursive = TRUE,
    force = TRUE
  )

  # Selective universal config only. Never ship active project/search config.
  config_source <- file.path(root, "config")
  config_destination <- file.path(output_directory, "config")
  dir.create(config_destination, recursive = TRUE, showWarnings = FALSE)

  if (dir.exists(config_source)) {
    config_files <- list.files(
      config_source,
      full.names = TRUE,
      recursive = FALSE,
      all.files = FALSE
    )
    config_files <- config_files[!dir.exists(config_files)]

    excluded_config <- c(
      "project_config.yml",
      "search_terms.yml",
      "project_seed.yml"
    )

    config_files <- config_files[
      !basename(config_files) %in% excluded_config
    ]

    for (src in config_files) {
      file.copy(
        src,
        file.path(config_destination, basename(src)),
        overwrite = TRUE
      )
    }
  }

  neutral_seed <- c(
    "# Universal SR-MA Toolkit — new project seed",
    "project_seed:",
    '  topic: "REPLACE_WITH_RESEARCH_TOPIC"'
  )
  writeLines(
    neutral_seed,
    file.path(config_destination, "project_seed.yml"),
    useBytes = TRUE
  )

  root_files <- c(
    "START_HERE.R",
    "renv.lock",
    ".Rprofile",
    ".env.example",
    ".gitignore",
    "README.md",
    "AGENTS.md",
    "CLAUDE.md"
  )

  for (name in root_files[file.exists(root_files)]) {
    file.copy(
      file.path(root, name),
      file.path(output_directory, name),
      overwrite = TRUE
    )
  }

  # Explicitly ensure secrets and runtime state cannot be present.
  forbidden_files <- c(
    ".env",
    "config/project_config.yml",
    "config/search_terms.yml"
  )

  for (f in forbidden_files) {
    candidate <- file.path(output_directory, f)
    if (file.exists(candidate)) unlink(candidate, force = TRUE)
  }

  forbidden_directories <- c(
    "data",
    "results",
    "projects",
    "search_strategies",
    "tasks",
    "plans",
    "reviews",
    ".task017_backups",
    "config/generated",
    "config/archive",
    "config/generated_schema_v2_archive"
  )

  for (d in forbidden_directories) {
    candidate <- file.path(output_directory, d)
    if (dir.exists(candidate)) {
      unlink(candidate, recursive = TRUE, force = TRUE)
    }
  }

  cat(
    "\nUniversal release package created:\n  ",
    output_directory,
    "\n",
    sep = ""
  )

  invisible(output_directory)
}

if (!identical(getOption("srma.release.autorun"), FALSE)) {
  build_universal_release_package()
}

