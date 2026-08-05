# ============================================================
# Universal SR-MA Agent Toolkit
# R/01_validate_project_initialization.R
# Robust validator for variable YAML list shapes
# ============================================================

required_packages <- c("yaml", "jsonlite")
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
    paste(missing_packages, collapse = ", ")
  )
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

as_character_vector <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(character(0))
  }

  values <- unlist(x, recursive = TRUE, use.names = FALSE)
  values <- as.character(values)
  values <- trimws(values)
  values[nzchar(values)]
}

first_scalar_character <- function(x, default = "") {
  values <- as_character_vector(x)
  if (length(values) == 0L) default else values[[1]]
}

is_nonempty_scalar_character <- function(x) {
  is.character(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    nzchar(trimws(x))
}

first_scalar_logical <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }

  value <- unlist(x, recursive = TRUE, use.names = FALSE)[[1]]

  if (is.logical(value)) {
    return(isTRUE(value))
  }

  value_text <- tolower(trimws(as.character(value)))

  if (value_text %in% c("true", "t", "yes", "y", "1", "required")) {
    return(TRUE)
  }

  if (value_text %in% c("false", "f", "no", "n", "0", "optional")) {
    return(FALSE)
  }

  default
}

normalize_decision_values <- function(x) {
  values <- as_character_vector(x)

  if (length(values) == 0L && is.list(x) && !is.null(names(x))) {
    values <- names(x)
  }

  canonical <- vapply(
    values,
    function(value) {
      normalized <- tolower(gsub("[^a-z]", "", value))

      if (normalized %in% c("include", "included", "yes")) {
        return("Include")
      }

      if (normalized %in% c("exclude", "excluded", "no")) {
        return("Exclude")
      }

      if (normalized %in% c(
        "unclear",
        "uncertain",
        "maybe",
        "unsure",
        "potentiallyeligible"
      )) {
        return("Unclear")
      }

      trimws(value)
    },
    character(1)
  )

  unique(canonical[nzchar(canonical)])
}

normalize_record_array <- function(
  x,
  fields,
  object_name,
  field_normalizers = list()
) {
  if (is.null(x) || length(x) == 0L) {
    return(list())
  }

  normalize_field <- function(field, value) {
    normalizer <- field_normalizers[[field]]

    if (is.null(normalizer)) {
      return(value)
    }

    normalizer(value)
  }

  # Case 1: data frame, one record per row
  if (is.data.frame(x)) {
    missing_fields <- setdiff(fields, names(x))

    if (length(missing_fields) > 0L) {
      stop(
        object_name,
        " is missing field(s): ",
        paste(missing_fields, collapse = ", ")
      )
    }

    return(
      lapply(
        seq_len(nrow(x)),
        function(i) {
          record <- lapply(
            fields,
            function(field) {
              normalize_field(field, x[[field]][[i]])
            }
          )
          names(record) <- fields
          record
        }
      )
    )
  }

  # Case 2: column-oriented named list
  if (
    is.list(x) &&
    !is.null(names(x)) &&
    all(fields %in% names(x))
  ) {
    field_lengths <- vapply(
      fields,
      function(field) {
        value <- x[[field]]

        # Nested object fields may represent a shared structure.
        if (
          is.list(value) &&
          !is.data.frame(value) &&
          !is.null(names(value))
        ) {
          return(1L)
        }

        length(value)
      },
      integer(1)
    )

    record_count <- max(field_lengths)

    incompatible <- field_lengths[
      !(field_lengths %in% c(1L, record_count))
    ]

    if (length(incompatible) > 0L) {
      stop(
        object_name,
        " has incompatible field lengths: ",
        paste(
          paste0(names(field_lengths), "=", field_lengths),
          collapse = ", "
        )
      )
    }

    return(
      lapply(
        seq_len(record_count),
        function(i) {
          record <- lapply(
            fields,
            function(field) {
              value <- x[[field]]
              field_length <- field_lengths[[field]]

              if (field_length == 1L) {
                item <- value
              } else {
                item <- value[[i]]
              }

              normalize_field(field, item)
            }
          )

          names(record) <- fields
          record
        }
      )
    )
  }

  # Case 3: row-oriented list of records
  if (is.list(x)) {
    return(
      lapply(
        seq_along(x),
        function(i) {
          item <- x[[i]]

          if (is.data.frame(item)) {
            item <- as.list(item[1, , drop = FALSE])
          }

          if (!is.list(item)) {
            stop(
              object_name,
              " record ",
              i,
              " has unsupported class: ",
              paste(class(item), collapse = ", ")
            )
          }

          missing_fields <- setdiff(fields, names(item))

          if (length(missing_fields) > 0L) {
            stop(
              object_name,
              " record ",
              i,
              " is missing field(s): ",
              paste(missing_fields, collapse = ", ")
            )
          }

          record <- lapply(
            fields,
            function(field) {
              normalize_field(field, item[[field]])
            }
          )

          names(record) <- fields
          record
        }
      )
    )
  }

  stop(
    object_name,
    " has unsupported class: ",
    paste(class(x), collapse = ", ")
  )
}

normalize_controlled_vocabulary <- function(x) {
  empty_value <- list(
    pubmed_mesh_candidates = character(0),
    embase_emtree_candidates = character(0)
  )

  if (is.null(x) || length(x) == 0L) {
    return(empty_value)
  }

  if (is.data.frame(x)) {
    x <- as.list(x)
  }

  if (is.list(x) && !is.null(names(x))) {
    pubmed_value <- x$pubmed_mesh_candidates %||%
      x$pubmed %||%
      x$mesh %||%
      x$pubmed_mesh %||%
      character(0)

    embase_value <- x$embase_emtree_candidates %||%
      x$embase %||%
      x$emtree %||%
      x$embase_emtree %||%
      character(0)

    return(
      list(
        pubmed_mesh_candidates = as_character_vector(pubmed_value),
        embase_emtree_candidates = as_character_vector(embase_value)
      )
    )
  }

  # Fallback for unnamed two-element structures
  if (is.list(x) && length(x) == 2L) {
    return(
      list(
        pubmed_mesh_candidates = as_character_vector(x[[1]]),
        embase_emtree_candidates = as_character_vector(x[[2]])
      )
    )
  }

  empty_value
}

normalize_exclusion_reasons <- function(x) {
  normalize_record_array(
    x = x,
    fields = c("code", "label", "description"),
    object_name = "screening.exclusion_reasons",
    field_normalizers = list(
      code = function(value) first_scalar_character(value),
      label = function(value) first_scalar_character(value),
      description = function(value) first_scalar_character(value)
    )
  )
}

normalize_search_concepts <- function(x) {
  base_fields <- c(
    "concept_id",
    "label",
    "role",
    "required",
    "free_text_terms",
    "abbreviations",
    "spelling_variants"
  )

  concepts <- normalize_record_array(
    x = x,
    fields = base_fields,
    object_name = "search.concepts",
    field_normalizers = list(
      concept_id = function(value) first_scalar_character(value),
      label = function(value) first_scalar_character(value),
      role = function(value) first_scalar_character(value),
      required = function(value) first_scalar_logical(value),
      free_text_terms = as_character_vector,
      abbreviations = as_character_vector,
      spelling_variants = as_character_vector
    )
  )

  for (i in seq_along(concepts)) {
    concepts[[i]]$controlled_vocabulary <- list(
      pubmed_mesh_candidates = character(0),
      embase_emtree_candidates = character(0)
    )
  }

  raw_cv <- NULL

  # Column-oriented search.concepts
  if (
    is.list(x) &&
    !is.null(names(x)) &&
    "controlled_vocabulary" %in% names(x)
  ) {
    raw_cv <- x$controlled_vocabulary
  }

  # Row-oriented search.concepts
  if (
    is.null(raw_cv) &&
    is.list(x) &&
    is.null(names(x))
  ) {
    raw_cv <- lapply(
      x,
      function(item) {
        if (is.list(item)) {
          item$controlled_vocabulary %||% NULL
        } else {
          NULL
        }
      }
    )
  }

  if (length(concepts) == 0L || is.null(raw_cv)) {
    return(concepts)
  }

  # One controlled-vocabulary object per concept
  if (
    is.list(raw_cv) &&
    length(raw_cv) == length(concepts) &&
    (
      is.null(names(raw_cv)) ||
      !any(
        c(
          "pubmed_mesh_candidates",
          "pubmed",
          "mesh",
          "embase_emtree_candidates",
          "embase",
          "emtree"
        ) %in% names(raw_cv)
      )
    )
  ) {
    for (i in seq_along(concepts)) {
      concepts[[i]]$controlled_vocabulary <-
        normalize_controlled_vocabulary(raw_cv[[i]])
    }

    return(concepts)
  }

  # Database-column structure shared across concepts
  if (
    is.list(raw_cv) &&
    !is.null(names(raw_cv))
  ) {
    pubmed_all <- raw_cv$pubmed_mesh_candidates %||%
      raw_cv$pubmed %||%
      raw_cv$mesh %||%
      list()

    embase_all <- raw_cv$embase_emtree_candidates %||%
      raw_cv$embase %||%
      raw_cv$emtree %||%
      list()

    for (i in seq_along(concepts)) {
      pubmed_item <- if (length(pubmed_all) >= i) {
        pubmed_all[[i]]
      } else {
        character(0)
      }

      embase_item <- if (length(embase_all) >= i) {
        embase_all[[i]]
      } else {
        character(0)
      }

      concepts[[i]]$controlled_vocabulary <- list(
        pubmed_mesh_candidates = as_character_vector(pubmed_item),
        embase_emtree_candidates = as_character_vector(embase_item)
      )
    }

    return(concepts)
  }

  # Fallback: use the same structure for every concept only when necessary
  shared_cv <- normalize_controlled_vocabulary(raw_cv)

  for (i in seq_along(concepts)) {
    concepts[[i]]$controlled_vocabulary <- shared_cv
  }

  concepts
}

normalize_human_decisions <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(list())
  }

  normalize_record_array(
    x = x,
    fields = c(
      "decision_id",
      "question",
      "why_it_matters",
      "suggested_options"
    ),
    object_name = "human_decisions_required",
    field_normalizers = list(
      decision_id = function(value) first_scalar_character(value),
      question = function(value) first_scalar_character(value),
      why_it_matters = function(value) first_scalar_character(value),
      suggested_options = as_character_vector
    )
  )
}

agent_file <- "config/initialization_agent.yml"

if (!file.exists(agent_file)) {
  stop("Missing initialization configuration: ", agent_file)
}

agent_document <- yaml::read_yaml(agent_file)
agent_config <- agent_document$initialization_agent

if (is.null(agent_config$output$generated_directory)) {
  stop(
    "initialization_agent.output.generated_directory is missing in ",
    agent_file
  )
}

output_dir <- agent_config$output$generated_directory

project_file <- file.path(
  output_dir,
  agent_config$output$project_config_draft
)
search_file <- file.path(
  output_dir,
  agent_config$output$search_terms_draft
)
decisions_file <- file.path(
  output_dir,
  agent_config$output$decisions_required
)

required_files <- c(
  project_file,
  search_file,
  decisions_file
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) > 0L) {
  stop(
    "Missing initialization draft files:\n- ",
    paste(missing_files, collapse = "\n- ")
  )
}

project_config <- yaml::read_yaml(project_file)
search_terms <- yaml::read_yaml(search_file)
decisions_document <- yaml::read_yaml(decisions_file)

errors <- character(0)
warnings <- character(0)

# ------------------------------------------------------------
# Validate canonical project-config contract
# ------------------------------------------------------------
# This validator must match the active configuration contract
# consumed by config_utils.R, path_utils.R, and R/11-R/20.

required_project_sections <- c(
  "schema_version",
  "project",
  "research_question",
  "eligibility",
  "search",
  "screening",
  "data_extraction",
  "risk_of_bias",
  "certainty_of_evidence",
  "reporting",
  "naming",
  "paths"
)

missing_project_sections <- setdiff(
  required_project_sections,
  names(project_config)
)

if (length(missing_project_sections) > 0L) {
  errors <- c(
    errors,
    paste(
      "Missing canonical project sections:",
      paste(missing_project_sections, collapse = ", ")
    )
  )
}


# ------------------------------------------------------------
# Validate project metadata required downstream
# ------------------------------------------------------------

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
      is_nonempty_scalar_character(
        project_config$project[[field_name]]
      )
    },
    logical(1)
  )
]

if (length(missing_project_fields) > 0L) {
  errors <- c(
    errors,
    paste(
      "Missing or empty project fields:",
      paste(
        paste0("project.", missing_project_fields),
        collapse = ", "
      )
    )
  )
}

filename_safe_pattern <- "^[A-Za-z0-9][A-Za-z0-9_-]*$"

for (field_name in c("project_id", "output_prefix")) {
  field_value <- project_config$project[[field_name]]

  if (
    is_nonempty_scalar_character(field_value) &&
    !grepl(filename_safe_pattern, field_value)
  ) {
    errors <- c(
      errors,
      paste0(
        "project.",
        field_name,
        " contains unsupported filename characters: ",
        field_value
      )
    )
  }
}


# ------------------------------------------------------------
# Validate research-question compatibility fields
# ------------------------------------------------------------

required_research_question_fields <- c(
  "framework",
  "question",
  "population",
  "intervention",
  "comparator",
  "outcomes",
  "framework_elements"
)

missing_research_question_fields <-
  required_research_question_fields[
    vapply(
      required_research_question_fields,
      function(field_name) {
        is.null(
          project_config$research_question[[field_name]]
        )
      },
      logical(1)
    )
  ]

if (length(missing_research_question_fields) > 0L) {
  errors <- c(
    errors,
    paste(
      "Missing research-question fields:",
      paste(
        paste0(
          "research_question.",
          missing_research_question_fields
        ),
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# Validate configured databases
# ------------------------------------------------------------

database_config <- project_config$search$databases

if (is.null(database_config) || length(database_config) == 0L) {
  errors <- c(
    errors,
    "No database configuration was found at search.databases."
  )
} else {
  enabled_database_names <- names(database_config)[
    vapply(
      database_config,
      function(database) {
        if (is.logical(database)) {
          return(isTRUE(database))
        }

        if (is.list(database)) {
          return(isTRUE(database$enabled))
        }

        FALSE
      },
      logical(1)
    )
  ]

  if (length(enabled_database_names) == 0L) {
    errors <- c(
      errors,
      "At least one database must be enabled at search.databases."
    )
  }
}


# ------------------------------------------------------------
# Validate naming contract used by path_utils.R
# ------------------------------------------------------------

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
      is_nonempty_scalar_character(
        project_config$naming[[field_name]]
      )
    },
    logical(1)
  )
]

if (length(missing_naming_fields) > 0L) {
  errors <- c(
    errors,
    paste(
      "Missing or empty naming fields:",
      paste(
        paste0("naming.", missing_naming_fields),
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# Validate path contract used by path_utils.R
# ------------------------------------------------------------

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
      is_nonempty_scalar_character(
        project_config$paths[[field_name]]
      )
    },
    logical(1)
  )
]

if (length(missing_path_fields) > 0L) {
  errors <- c(
    errors,
    paste(
      "Missing or empty path fields:",
      paste(
        paste0("paths.", missing_path_fields),
        collapse = ", "
      )
    )
  )
}


# ------------------------------------------------------------
# Validate cross-file project identity and search-term path
# ------------------------------------------------------------

project_id <- first_scalar_character(
  project_config$project$project_id
)

search_project_id <- first_scalar_character(
  search_terms$project$id
)

if (
  nzchar(project_id) &&
  nzchar(search_project_id) &&
  !identical(project_id, search_project_id)
) {
  errors <- c(
    errors,
    paste0(
      "Project ID mismatch between project_config and search_terms: ",
      project_id,
      " vs ",
      search_project_id
    )
  )
}

project_title <- first_scalar_character(
  project_config$project$title
)

search_project_title <- first_scalar_character(
  search_terms$project$title
)

if (
  nzchar(project_title) &&
  nzchar(search_project_title) &&
  !identical(project_title, search_project_title)
) {
  errors <- c(
    errors,
    "Project title mismatch between project_config and search_terms."
  )
}

project_short_name <- first_scalar_character(
  project_config$project$short_name
)

search_short_name <- first_scalar_character(
  search_terms$project$short_name
)

if (
  nzchar(project_short_name) &&
  nzchar(search_short_name) &&
  !identical(project_short_name, search_short_name)
) {
  errors <- c(
    errors,
    "Project short name mismatch between project_config and search_terms."
  )
}

configured_search_terms_file <- first_scalar_character(
  project_config$paths$search_terms_file
)

declared_search_terms_file <- first_scalar_character(
  project_config$search$terms_file
)

if (
  nzchar(configured_search_terms_file) &&
  nzchar(declared_search_terms_file) &&
  !identical(
    configured_search_terms_file,
    declared_search_terms_file
  )
) {
  errors <- c(
    errors,
    paste0(
      "Search-terms path mismatch: paths.search_terms_file = ",
      configured_search_terms_file,
      "; search.terms_file = ",
      declared_search_terms_file
    )
  )
}

screening_decisions <- normalize_decision_values(
  project_config$screening$decisions
)

required_decisions <- c(
  "Include",
  "Exclude",
  "Unclear"
)

missing_decisions <- setdiff(
  required_decisions,
  screening_decisions
)

if (length(missing_decisions) > 0L) {
  errors <- c(
    errors,
    paste0(
      "Screening decisions are missing: ",
      paste(missing_decisions, collapse = ", "),
      ". Values found: ",
      if (length(screening_decisions) > 0L) {
        paste(screening_decisions, collapse = ", ")
      } else {
        "<none>"
      }
    )
  )
}

reason_table <- tryCatch(
  normalize_exclusion_reasons(
    project_config$screening$exclusion_reasons
  ),
  error = function(error) {
    errors <<- c(
      errors,
      paste0(
        "Exclusion-reason normalization failed: ",
        conditionMessage(error)
      )
    )
    list()
  }
)

if (length(reason_table) == 0L) {
  errors <- c(
    errors,
    "At least one exclusion reason is required."
  )
} else {
  codes <- vapply(
    reason_table,
    `[[`,
    character(1),
    "code"
  )

  labels <- vapply(
    reason_table,
    `[[`,
    character(1),
    "label"
  )

  descriptions <- vapply(
    reason_table,
    `[[`,
    character(1),
    "description"
  )

  if (any(!nzchar(codes))) {
    errors <- c(
      errors,
      "One or more exclusion-reason codes are empty."
    )
  }

  if (any(!nzchar(labels))) {
    errors <- c(
      errors,
      "One or more exclusion-reason labels are empty."
    )
  }

  if (any(!nzchar(descriptions))) {
    warnings <- c(
      warnings,
      "One or more exclusion-reason descriptions are empty."
    )
  }

  if (anyDuplicated(codes)) {
    errors <- c(
      errors,
      "Exclusion-reason codes are duplicated."
    )
  }

  if (anyDuplicated(labels)) {
    errors <- c(
      errors,
      "Exclusion-reason labels are duplicated."
    )
  }
}

concepts <- tryCatch(
  normalize_search_concepts(
    search_terms$search$concepts
  ),
  error = function(error) {
    errors <<- c(
      errors,
      paste0(
        "Search-concept normalization failed: ",
        conditionMessage(error)
      )
    )
    list()
  }
)

if (length(concepts) < 2L) {
  warnings <- c(
    warnings,
    "Fewer than two search concepts were generated."
  )
}

if (length(concepts) > 0L) {
  concept_ids <- vapply(
    concepts,
    `[[`,
    character(1),
    "concept_id"
  )

  concept_labels <- vapply(
    concepts,
    `[[`,
    character(1),
    "label"
  )

  if (any(!nzchar(concept_ids))) {
    errors <- c(
      errors,
      "One or more search-concept IDs are empty."
    )
  }

  if (any(!nzchar(concept_labels))) {
    errors <- c(
      errors,
      "One or more search-concept labels are empty."
    )
  }

  if (anyDuplicated(concept_ids)) {
    errors <- c(
      errors,
      "Search-concept IDs are duplicated."
    )
  }

  empty_required <- vapply(
    concepts,
    function(concept) {
      all_terms <- unique(
        c(
          concept$free_text_terms,
          concept$abbreviations,
          concept$spelling_variants
        )
      )

      isTRUE(concept$required) &&
        length(all_terms[nzchar(all_terms)]) == 0L
    },
    logical(1)
  )

  if (any(empty_required)) {
    errors <- c(
      errors,
      paste0(
        "Required search concepts contain no free-text terms: ",
        paste(
          concept_ids[empty_required],
          collapse = ", "
        )
      )
    )
  }

  missing_pubmed_cv <- vapply(
    concepts,
    function(concept) {
      length(
        concept$controlled_vocabulary$pubmed_mesh_candidates
      ) == 0L
    },
    logical(1)
  )

  missing_embase_cv <- vapply(
    concepts,
    function(concept) {
      length(
        concept$controlled_vocabulary$embase_emtree_candidates
      ) == 0L
    },
    logical(1)
  )

  if (any(missing_pubmed_cv)) {
    warnings <- c(
      warnings,
      paste0(
        "No PubMed MeSH candidates were generated for: ",
        paste(
          concept_ids[missing_pubmed_cv],
          collapse = ", "
        )
      )
    )
  }

  if (any(missing_embase_cv)) {
    warnings <- c(
      warnings,
      paste0(
        "No Embase Emtree candidates were generated for: ",
        paste(
          concept_ids[missing_embase_cv],
          collapse = ", "
        )
      )
    )
  }
}

controlled_vocabulary_flag <- first_scalar_logical(
  search_terms$validation$controlled_vocabulary_requires_verification,
  default = FALSE
)

if (!controlled_vocabulary_flag) {
  warnings <- c(
    warnings,
    paste(
      "Controlled-vocabulary candidates should be marked",
      "as requiring verification."
    )
  )
}

human_decisions <- tryCatch(
  normalize_human_decisions(
    decisions_document$human_decisions_required
  ),
  error = function(error) {
    errors <<- c(
      errors,
      paste0(
        "Human-decision normalization failed: ",
        conditionMessage(error)
      )
    )
    list()
  }
)

if (length(human_decisions) > 0L) {
  warnings <- c(
    warnings,
    paste0(
      length(human_decisions),
      " human decision(s) remain unresolved."
    )
  )
}

report <- data.frame(
  severity = c(
    rep("ERROR", length(errors)),
    rep("WARNING", length(warnings))
  ),
  message = c(
    errors,
    warnings
  ),
  stringsAsFactors = FALSE
)

summary_report <- data.frame(
  metric = c(
    "project_sections",
    "screening_decisions",
    "exclusion_reasons",
    "search_concepts",
    "human_decisions",
    "errors",
    "warnings"
  ),
  value = c(
    length(intersect(required_project_sections, names(project_config))),
    length(screening_decisions),
    length(reason_table),
    length(concepts),
    length(human_decisions),
    length(errors),
    length(warnings)
  ),
  stringsAsFactors = FALSE
)

normalized_snapshot <- list(
  screening_decisions = screening_decisions,
  exclusion_reasons = reason_table,
  search_concepts = concepts,
  human_decisions_required = human_decisions
)

validation_logs_directory <- first_scalar_character(
  project_config$paths$logs_directory,
  default = "results/logs"
)

dir.create(
  validation_logs_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

validation_report_file <- file.path(
  validation_logs_directory,
  "project_initialization_validation.csv"
)

validation_summary_file <- file.path(
  validation_logs_directory,
  "project_initialization_validation_summary.csv"
)

normalized_snapshot_file <- file.path(
  validation_logs_directory,
  "project_initialization_normalized_snapshot.json"
)

utils::write.csv(
  report,
  validation_report_file,
  row.names = FALSE
)

utils::write.csv(
  summary_report,
  validation_summary_file,
  row.names = FALSE
)

jsonlite::write_json(
  normalized_snapshot,
  normalized_snapshot_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)

cat("\n")
cat("========================================\n")
cat("Project initialization validation\n")
cat("========================================\n")
cat("Screening decisions:", length(screening_decisions), "\n")
cat("Exclusion reasons:", length(reason_table), "\n")
cat("Search concepts:", length(concepts), "\n")
cat("Human decisions:", length(human_decisions), "\n")
cat("Errors:", length(errors), "\n")
cat("Warnings:", length(warnings), "\n")

if (length(errors) > 0L) {
  stop(
    "Project initialization validation failed:\n- ",
    paste(errors, collapse = "\n- ")
  )
}

cat("\nPASS: Initialization drafts are structurally valid.\n")

if (length(warnings) > 0L) {
  cat(
    "\nWarnings requiring review:\n- ",
    paste(warnings, collapse = "\n- "),
    "\n"
  )
}

cat("\nValidation reports saved:\n")
cat("- ", validation_report_file, "\n", sep = "")
cat("- ", validation_summary_file, "\n", sep = "")
cat("- ", normalized_snapshot_file, "\n", sep = "")

cat(
  "\nDo not activate the drafts until human decisions are resolved.\n"
)
