# ============================================================
# Universal SR-MA Agent Toolkit
# START_HERE.R
#
# First-run / resume entry point.
# The onboarding layer performs only environment/preflight work,
# then hands workflow control to the canonical Runner.
#
# VERSION: 1.1.0
#
# DEPENDENCY CONTRACT
# ------------------------------------------------------------
# - renv.lock is the canonical package source of truth.
# - START_HERE restores the complete lockfile environment, not just a small
#   bootstrap subset.
# - Workflow-critical direct dependencies are validated after restore.
# - R/41 DOCX dependencies (officer, flextable) are core dependencies.
# - Optional figure-rendering packages remain on-demand inside R/41.
# - Set options(srma.start_here.environment_only = TRUE) for dependency-only QC.
# ============================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

srma_find_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    markers <- c(
      file.path(current, "R", "run_universal_srma_workflow.R"),
      file.path(current, "config", "initialization_agent.yml")
    )

    if (all(file.exists(markers))) return(current)

    parent <- dirname(current)
    if (identical(parent, current)) {
      stop(
        "Could not find the Universal SR-MA Toolkit root.\n",
        "Open R/RStudio anywhere inside the toolkit folder and run ",
        'source("START_HERE.R").',
        call. = FALSE
      )
    }

    current <- parent
  }
}

srma_assert_writable <- function(root) {
  probe <- file.path(
    root,
    paste0(".srma_write_test_", Sys.getpid(), "_", as.integer(Sys.time()))
  )

  ok <- tryCatch(
    {
      writeLines("ok", probe, useBytes = TRUE)
      file.exists(probe)
    },
    error = function(e) FALSE
  )

  if (file.exists(probe)) unlink(probe, force = TRUE)

  if (!isTRUE(ok)) {
    stop(
      "Toolkit directory is not writable:\n  ",
      root,
      "\nMove the toolkit to a writable user folder and retry.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

srma_required_packages <- function() {
  # Direct packages required by the canonical workflow and final manuscript
  # production/QC path. Transitive dependencies are owned by renv.lock.
  c(
    "yaml",
    "jsonlite",
    "httr2",
    "dotenv",
    "readr",
    "dplyr",
    "tibble",
    "stringr",
    "xml2",
    "openxlsx",
    "digest",
    "officer",
    "flextable"
  )
}

srma_missing_packages <- function(packages) {
  packages[
    !vapply(
      packages,
      requireNamespace,
      quietly = TRUE,
      FUN.VALUE = logical(1)
    )
  ]
}

srma_ensure_renv <- function(root) {
  lockfile <- file.path(root, "renv.lock")
  required_packages <- srma_required_packages()

  if (!file.exists(lockfile)) {
    missing_required <- srma_missing_packages(required_packages)

    if (length(missing_required) > 0L) {
      stop(
        "renv.lock is not present and required workflow package(s) are missing:\n- ",
        paste(missing_required, collapse = "\n- "),
        "\n\nA release copy of the Universal SR-MA Toolkit should include renv.lock.",
        "\nRestore the canonical lockfile or install the required packages before continuing.",
        call. = FALSE
      )
    }

    cat(
      "renv.lock: not present; current R library satisfies the required package preflight.\n"
    )
    return(invisible(TRUE))
  }

  if (!requireNamespace("renv", quietly = TRUE)) {
    cat("Installing the renv bootstrap package...\n")
    install.packages(
      "renv",
      repos = "https://cloud.r-project.org",
      quiet = TRUE
    )
  }

  if (!requireNamespace("renv", quietly = TRUE)) {
    stop(
      "The renv package could not be installed.",
      call. = FALSE
    )
  }

  tryCatch(
    renv::load(project = root, quiet = TRUE),
    error = function(e) {
      stop(
        "Could not activate the project renv environment:\n",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  auto_restore <- !identical(
    getOption("srma.start_here.auto_restore"),
    FALSE
  )

  if (isTRUE(auto_restore)) {
    cat(
      "Checking/restoring the canonical R environment from renv.lock...\n"
    )

    tryCatch(
      renv::restore(
        project = root,
        prompt = FALSE
      ),
      error = function(e) {
        stop(
          "renv::restore() failed:\n",
          conditionMessage(e),
          "\n\nResolve the package/CRAN/network issue and rerun ",
          'source("START_HERE.R").',
          call. = FALSE
        )
      }
    )
  } else {
    cat(
      "Automatic renv restore: DISABLED by option srma.start_here.auto_restore=FALSE\n"
    )
  }

  missing_required <- srma_missing_packages(required_packages)

  if (length(missing_required) > 0L) {
    stop(
      "Toolkit dependency preflight FAILED after renv activation",
      if (isTRUE(auto_restore)) " / restore" else "",
      ".\n\nMissing workflow-required package(s):\n- ",
      paste(missing_required, collapse = "\n- "),
      "\n\nThis usually means the canonical renv.lock is incomplete or out of sync ",
      "with the current toolkit code.",
      "\n\nMAINTAINER ACTION — from the toolkit root:\n",
      "  renv::install(c(",
      paste(sprintf('"%s"', missing_required), collapse = ", "),
      "))\n",
      "  renv::snapshot(prompt = FALSE)\n",
      "  source(\"START_HERE.R\")\n",
      "\nDo not work around this by installing packages ad hoc inside an individual ",
      "workflow module.",
      call. = FALSE
    )
  }

  cat(
    "Required workflow packages: PASS (",
    length(required_packages),
    " direct dependencies)\n",
    sep = ""
  )
  cat("R environment / renv: PASS\n")

  invisible(TRUE)
}

srma_load_dotenv <- function(root) {
  env_file <- file.path(root, ".env")

  if (file.exists(env_file) && requireNamespace("dotenv", quietly = TRUE)) {
    dotenv::load_dot_env(file = env_file)
  }

  invisible(env_file)
}

srma_provider_preflight <- function(root) {
  agent_file <- file.path(root, "config", "initialization_agent.yml")

  if (!file.exists(agent_file) || !requireNamespace("yaml", quietly = TRUE)) {
    return(invisible(TRUE))
  }

  doc <- yaml::read_yaml(agent_file)
  provider <- doc$initialization_agent$provider %||% list()

  provider_name <- tolower(trimws(as.character(provider$name %||% "")))
  api_env <- trimws(as.character(provider$api_key_env %||% "OPENAI_API_KEY"))
  base_url <- sub(
    "/+$",
    "",
    trimws(as.character(provider$base_url %||% "https://api.openai.com/v1"))
  )

  srma_load_dotenv(root)
  api_key <- Sys.getenv(api_env, unset = "")

  if (!nzchar(api_key)) {
    env_file <- file.path(root, ".env")
    example <- file.path(root, ".env.example")

    if (!file.exists(env_file) && file.exists(example)) {
      file.copy(example, env_file, overwrite = FALSE)
    }

    cat(
      "\nUSER ACTION REQUIRED — API credential\n",
      "Add a valid ", api_env, " value to:\n  ",
      env_file,
      "\nThen rerun:\n  source(\"START_HERE.R\")\n",
      sep = ""
    )

    return(FALSE)
  }

  check_api <- !identical(
    getOption("srma.start_here.check_api"),
    FALSE
  )

  if (
    isTRUE(check_api) &&
    identical(provider_name, "openai") &&
    requireNamespace("httr2", quietly = TRUE)
  ) {
    result <- tryCatch(
      {
        req <- httr2::request(paste0(base_url, "/models"))
        req <- httr2::req_auth_bearer_token(req, api_key)
        req <- httr2::req_timeout(req, seconds = 15)
        resp <- httr2::req_perform(req)
        httr2::resp_status(resp)
      },
      error = function(e) NA_integer_
    )

    if (!identical(result, 200L)) {
      cat(
        "\nAPI credential was found, but connectivity/authentication ",
        "could not be confirmed.\n",
        "Check network access and the API credential, then rerun ",
        'source("START_HERE.R").\n',
        sep = ""
      )
      return(FALSE)
    }

    cat("API connectivity/authentication: PASS\n")
  } else {
    cat("API credential: PRESENT\n")
  }

  invisible(TRUE)
}

srma_prepare_project_seed <- function(root) {
  active_config <- file.path(root, "config", "project_config.yml")
  draft_config <- file.path(
    root,
    "config",
    "generated",
    "project_config_draft.yml"
  )

  if (file.exists(active_config) || file.exists(draft_config)) {
    return(invisible(TRUE))
  }

  seed_file <- file.path(root, "config", "project_seed.yml")
  seed <- list(project_seed = list(topic = "REPLACE_WITH_RESEARCH_TOPIC"))

  if (file.exists(seed_file) && requireNamespace("yaml", quietly = TRUE)) {
    existing <- tryCatch(
      yaml::read_yaml(seed_file),
      error = function(e) NULL
    )
    if (is.list(existing)) seed <- existing
  }

  topic <- trimws(as.character(seed$project_seed$topic %||% ""))

  if (
    !nzchar(topic) ||
    identical(topic, "REPLACE_WITH_RESEARCH_TOPIC")
  ) {
    if (!interactive()) {
      stop(
        "A new project requires a research topic in config/project_seed.yml.",
        call. = FALSE
      )
    }

    cat(
      "\nNEW SR-MA PROJECT\n",
      "Enter the research topic/question. The Initialization Agent will ",
      "infer the remaining project structure automatically.\n",
      sep = ""
    )

    topic <- trimws(readline("Research topic: "))

    if (!nzchar(topic)) {
      cat("No topic entered. START_HERE stopped without changing workflow state.\n")
      return(FALSE)
    }

    if (is.null(seed$project_seed) || !is.list(seed$project_seed)) {
      seed$project_seed <- list()
    }

    seed$project_seed$topic <- topic

    yaml::write_yaml(
      seed,
      seed_file
    )

    cat(
      "Project seed updated:\n  ",
      normalizePath(seed_file, winslash = "/", mustWork = TRUE),
      "\n",
      sep = ""
    )
  }

  invisible(TRUE)
}

srma_start_here <- function() {
  root <- srma_find_root()

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(root)

  cat(
    "\n============================================================\n",
    "Universal SR-MA Agent Toolkit — START HERE\n",
    "============================================================\n",
    "Toolkit root: ", root, "\n",
    "R version: ", R.version.string, "\n",
    sep = ""
  )

  srma_assert_writable(root)
  cat("Repository / writable workspace: PASS\n")

  srma_ensure_renv(root)

  environment_only <- identical(
    getOption("srma.start_here.environment_only"),
    TRUE
  )

  if (isTRUE(environment_only)) {
    cat(
      "\nSTART_HERE environment preflight: PASS\n",
      "Project initialization, API preflight, and Runner hand-off were skipped ",
      "because srma.start_here.environment_only=TRUE.\n",
      sep = ""
    )
    return(invisible(TRUE))
  }

  if (!isTRUE(srma_provider_preflight(root))) {
    return(invisible(FALSE))
  }

  if (!isTRUE(srma_prepare_project_seed(root))) {
    return(invisible(FALSE))
  }

  runner_file <- file.path(
    root,
    "R",
    "run_universal_srma_workflow.R"
  )

  parse(file = runner_file)
  cat("Canonical Runner parse: PASS\n")

  autorun <- !identical(
    getOption("srma.start_here.autorun"),
    FALSE
  )

  if (!isTRUE(autorun)) {
    cat(
      "\nSTART_HERE preflight: PASS\n",
      "Runner autorun is disabled for this check.\n",
      sep = ""
    )
    return(invisible(TRUE))
  }

  cat(
    "\nPreflight complete. Handing control to the canonical Runner...\n\n"
  )

  source(
    runner_file,
    local = .GlobalEnv
  )

  invisible(TRUE)
}

srma_start_here()

