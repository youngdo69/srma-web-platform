# ============================================================
# Universal SR-MA Agent Toolkit
# R/02_activate_project_configuration.R
#
# Purpose:
#   Validate and activate reviewed project/search configuration
#   drafts after all Human Decisions have been resolved.
#
# Safety features:
#   - Human approval remains mandatory.
#   - Uses a numbered menu instead of masked text input.
#   - Verifies unresolved Human Decisions are empty.
#   - Archives existing active configuration files.
#   - Restores active files if activation is only partially completed.
#   - Verifies activated files match the reviewed drafts.
# ============================================================

required_packages <- c("yaml")

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
    "Missing package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

activation_scalar_character <- function(x, field_name) {
  if (is.null(x) || length(x) != 1L || !nzchar(trimws(as.character(x)))) {
    stop(
      "Missing or invalid initialization-agent setting: ",
      field_name,
      call. = FALSE
    )
  }

  trimws(as.character(x))
}

activation_copy_checked <- function(from, to, overwrite = TRUE, label = "file") {
  if (!file.exists(from)) {
    stop("Missing source ", label, ": ", from, call. = FALSE)
  }

  target_directory <- dirname(to)

  if (!dir.exists(target_directory)) {
    created <- dir.create(
      target_directory,
      recursive = TRUE,
      showWarnings = FALSE
    )

    if (!isTRUE(created) && !dir.exists(target_directory)) {
      stop(
        "Could not create target directory for ",
        label,
        ": ",
        target_directory,
        call. = FALSE
      )
    }
  }

  copied <- file.copy(
    from = from,
    to = to,
    overwrite = overwrite,
    copy.mode = TRUE,
    copy.date = TRUE
  )

  if (!isTRUE(copied) || !file.exists(to)) {
    stop(
      "Failed to copy ",
      label,
      " from ",
      from,
      " to ",
      to,
      ".",
      call. = FALSE
    )
  }

  invisible(to)
}

activation_files_identical <- function(file_a, file_b) {
  if (!file.exists(file_a) || !file.exists(file_b)) {
    return(FALSE)
  }

  identical(
    unname(tools::md5sum(file_a)),
    unname(tools::md5sum(file_b))
  )
}

activation_confirm <- function() {
  if (!interactive()) {
    stop(
      paste0(
        "Activation requires an interactive R session. ",
        "Run source(\"R/02_activate_project_configuration.R\") ",
        "from the R console."
      ),
      call. = FALSE
    )
  }

  cat(
    "\n",
    paste(rep("-", 68L), collapse = ""),
    "\nConfiguration activation\n",
    paste(rep("-", 68L), collapse = ""),
    "\n",
    "The current active configuration files will be archived.\n",
    "The reviewed draft files will then become the active configuration.\n\n",
    sep = ""
  )

  selection <- utils::menu(
    choices = c(
      "Archive current configs and activate reviewed drafts",
      "Cancel activation"
    ),
    title = "Select an action:"
  )

  if (!identical(selection, 1L)) {
    stop("Activation cancelled.", call. = FALSE)
  }

  invisible(TRUE)
}

# ------------------------------------------------------------
# Load and validate initialization-agent configuration
# ------------------------------------------------------------

agent_file <- "config/initialization_agent.yml"

if (!file.exists(agent_file)) {
  stop(
    "Missing initialization-agent configuration: ",
    agent_file,
    call. = FALSE
  )
}

agent_document <- yaml::read_yaml(agent_file)
agent_config <- agent_document$initialization_agent

if (is.null(agent_config) || !is.list(agent_config)) {
  stop(
    "Invalid structure in ",
    agent_file,
    ": expected top-level key 'initialization_agent'.",
    call. = FALSE
  )
}

if (!isTRUE(agent_config$activation$require_human_approval)) {
  stop(
    "Human approval must remain required for activation.",
    call. = FALSE
  )
}

output_dir <- activation_scalar_character(
  agent_config$output$generated_directory,
  "initialization_agent.output.generated_directory"
)

project_draft_name <- activation_scalar_character(
  agent_config$output$project_config_draft,
  "initialization_agent.output.project_config_draft"
)

search_draft_name <- activation_scalar_character(
  agent_config$output$search_terms_draft,
  "initialization_agent.output.search_terms_draft"
)

decisions_name <- activation_scalar_character(
  agent_config$output$decisions_required,
  "initialization_agent.output.decisions_required"
)

archive_dir <- activation_scalar_character(
  agent_config$activation$archive_directory,
  "initialization_agent.activation.archive_directory"
)

active_project <- activation_scalar_character(
  agent_config$activation$active_project_config,
  "initialization_agent.activation.active_project_config"
)

active_search <- activation_scalar_character(
  agent_config$activation$active_search_terms,
  "initialization_agent.activation.active_search_terms"
)

project_draft <- file.path(output_dir, project_draft_name)
search_draft <- file.path(output_dir, search_draft_name)
decisions_file <- file.path(output_dir, decisions_name)

required_input_files <- c(
  project_draft = project_draft,
  search_draft = search_draft,
  decisions_file = decisions_file
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    "Missing required activation input(s):\n- ",
    paste(
      paste0(names(missing_input_files), ": ", missing_input_files),
      collapse = "\n- "
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# Confirm that no unresolved Human Decisions remain
# ------------------------------------------------------------

decisions_document <- yaml::read_yaml(decisions_file)

if (is.null(decisions_document) || !is.list(decisions_document)) {
  stop(
    "Could not read a valid Human Decision YAML document: ",
    decisions_file,
    call. = FALSE
  )
}

if (!"human_decisions_required" %in% names(decisions_document)) {
  stop(
    "Invalid Human Decision file: missing top-level key ",
    "'human_decisions_required' in ",
    decisions_file,
    ".",
    call. = FALSE
  )
}

unresolved_decisions <- decisions_document$human_decisions_required

if (is.null(unresolved_decisions)) {
  unresolved_decisions <- list()
}

if (length(unresolved_decisions) > 0L) {
  unresolved_ids <- vapply(
    unresolved_decisions,
    function(decision) {
      if (is.list(decision) && !is.null(decision$decision_id)) {
        as.character(decision$decision_id)
      } else {
        "<unknown>"
      }
    },
    FUN.VALUE = character(1)
  )

  stop(
    "Activation blocked: ",
    length(unresolved_decisions),
    " unresolved Human Decision(s) remain in ",
    decisions_file,
    ".\nUnresolved IDs: ",
    paste(unresolved_ids, collapse = ", "),
    "\nResolve them before activation.",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# One visible numbered confirmation
# ------------------------------------------------------------

activation_confirm()

# ------------------------------------------------------------
# Prepare archive and rollback state
# ------------------------------------------------------------

if (!dir.exists(archive_dir)) {
  created <- dir.create(
    archive_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  if (!isTRUE(created) && !dir.exists(archive_dir)) {
    stop(
      "Could not create archive directory: ",
      archive_dir,
      call. = FALSE
    )
  }
}

stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

archive_project <- file.path(
  archive_dir,
  paste0("project_config_", stamp, ".yml")
)

archive_search <- file.path(
  archive_dir,
  paste0("search_terms_", stamp, ".yml")
)

project_existed_before <- file.exists(active_project)
search_existed_before <- file.exists(active_search)

project_archived <- FALSE
search_archived <- FALSE
activation_completed <- FALSE

# ------------------------------------------------------------
# Archive and activate transaction
# ------------------------------------------------------------

tryCatch(
  {
    if (project_existed_before) {
      activation_copy_checked(
        from = active_project,
        to = archive_project,
        overwrite = FALSE,
        label = "active project configuration archive"
      )
      project_archived <- TRUE
    }

    if (search_existed_before) {
      activation_copy_checked(
        from = active_search,
        to = archive_search,
        overwrite = FALSE,
        label = "active search-terms archive"
      )
      search_archived <- TRUE
    }

    activation_copy_checked(
      from = project_draft,
      to = active_project,
      overwrite = TRUE,
      label = "reviewed project configuration"
    )

    activation_copy_checked(
      from = search_draft,
      to = active_search,
      overwrite = TRUE,
      label = "reviewed search terms"
    )

    if (!activation_files_identical(project_draft, active_project)) {
      stop(
        "Activated project configuration does not match its reviewed draft.",
        call. = FALSE
      )
    }

    if (!activation_files_identical(search_draft, active_search)) {
      stop(
        "Activated search terms do not match their reviewed draft.",
        call. = FALSE
      )
    }

    activation_completed <- TRUE
  },
  error = function(error_condition) {
    rollback_messages <- character()

    if (project_existed_before && project_archived && file.exists(archive_project)) {
      restored <- file.copy(
        archive_project,
        active_project,
        overwrite = TRUE,
        copy.mode = TRUE,
        copy.date = TRUE
      )

      rollback_messages <- c(
        rollback_messages,
        if (isTRUE(restored)) {
          "Project configuration restored."
        } else {
          "WARNING: project configuration restoration failed."
        }
      )
    } else if (!project_existed_before && file.exists(active_project)) {
      removed <- file.remove(active_project)

      rollback_messages <- c(
        rollback_messages,
        if (isTRUE(removed)) {
          "New project configuration removed."
        } else {
          "WARNING: new project configuration could not be removed."
        }
      )
    }

    if (search_existed_before && search_archived && file.exists(archive_search)) {
      restored <- file.copy(
        archive_search,
        active_search,
        overwrite = TRUE,
        copy.mode = TRUE,
        copy.date = TRUE
      )

      rollback_messages <- c(
        rollback_messages,
        if (isTRUE(restored)) {
          "Search terms restored."
        } else {
          "WARNING: search-terms restoration failed."
        }
      )
    } else if (!search_existed_before && file.exists(active_search)) {
      removed <- file.remove(active_search)

      rollback_messages <- c(
        rollback_messages,
        if (isTRUE(removed)) {
          "New search-terms file removed."
        } else {
          "WARNING: new search-terms file could not be removed."
        }
      )
    }

    stop(
      paste0(
        "Configuration activation failed: ",
        conditionMessage(error_condition),
        if (length(rollback_messages) > 0L) {
          paste0("\nRollback: ", paste(rollback_messages, collapse = " "))
        } else {
          ""
        }
      ),
      call. = FALSE
    )
  }
)

# ------------------------------------------------------------
# Completion summary
# ------------------------------------------------------------

if (!isTRUE(activation_completed)) {
  stop(
    "Configuration activation did not complete.",
    call. = FALSE
  )
}

cat(
  "\n",
  paste(rep("=", 68L), collapse = ""),
  "\nConfiguration activation complete\n",
  paste(rep("=", 68L), collapse = ""),
  "\n",
  "Active project config: ",
  active_project,
  "\n",
  "Active search terms:   ",
  active_search,
  "\n",
  "Archive directory:     ",
  archive_dir,
  "\n",
  sep = ""
)

if (project_archived) {
  cat("Archived project config: ", archive_project, "\n", sep = "")
}

if (search_archived) {
  cat("Archived search terms:   ", archive_search, "\n", sep = "")
}

cat("\n")
