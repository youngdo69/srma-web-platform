# Cardiac Surgery SR-MA Toolkit v2.0
# Generate PubMed, Embase, and CENTRAL search strategies

# ---------------------------------------------------------
# Load project configuration and canonical paths
# ---------------------------------------------------------

source("R/utils/config_utils.R")
source("R/utils/path_utils.R")

config <- load_project_config(
  "config/project_config.yml"
)

paths <- build_project_paths(
  config = config,
  create_directories = TRUE,
  validate = TRUE
)

config_file <- paths$search_terms_file

if (!file.exists(config_file)) {
  stop(
    "Search-term configuration file does not exist:\n",
    config_file,
    "\nRun the search-term collection or initialization workflow first."
  )
}

search_terms <- yaml::read_yaml(
  config_file
)

# ---------------------------------------------------------
# Helper functions
# ---------------------------------------------------------

remove_empty <- function(x) {

  if (is.null(x)) {
    return(character(0))
  }

  x <- as.character(x)
  x <- trimws(x)
  x <- x[!is.na(x)]
  x <- x[x != ""]

  unique(x)
}

escape_quotes <- function(x) {
  gsub('"', '\\"', x, fixed = TRUE)
}

quote_term <- function(x) {
  paste0('"', escape_quotes(x), '"')
}

is_single_word <- function(x) {
  !grepl("\\s", trimws(x))
}

combine_or_block <- function(terms) {

  terms <- remove_empty(terms)

  if (length(terms) == 0) {
    return("")
  }

  paste0(
    "(\n  ",
    paste(terms, collapse = "\n  OR "),
    "\n)"
  )
}

# ---------------------------------------------------------
# PubMed formatting
# ---------------------------------------------------------

format_pubmed_free_text <- function(term) {

  term <- trimws(term)

  if (is_single_word(term)) {
    paste0(term, "[Title/Abstract]")
  } else {
    paste0(quote_term(term), "[Title/Abstract]")
  }
}

format_pubmed_mesh <- function(term) {
  paste0(quote_term(term), "[Mesh]")
}

# ---------------------------------------------------------
# Embase formatting
# Elsevier Embase style
# ---------------------------------------------------------

format_embase_free_text <- function(term) {

  term <- tolower(trimws(term))

  paste0(
    "'",
    gsub("'", "\\\\'", term, fixed = TRUE),
    "':ti,ab,kw"
  )
}

format_embase_emtree <- function(term) {

  term <- tolower(trimws(term))

  paste0(
    "'",
    gsub("'", "\\\\'", term, fixed = TRUE),
    "'/exp"
  )
}

# ---------------------------------------------------------
# CENTRAL formatting
# ---------------------------------------------------------

format_central_free_text <- function(term) {

  term <- trimws(term)

  if (is_single_word(term)) {
    paste0(term, ":ti,ab,kw")
  } else {
    paste0(quote_term(term), ":ti,ab,kw")
  }
}

format_central_mesh <- function(term) {

  term <- trimws(term)

  paste0(
    '[mh "',
    escape_quotes(term),
    '"]'
  )
}

# ---------------------------------------------------------
# Normalize search concepts
# ---------------------------------------------------------

collect_concept_terms <- function(concept) {

  remove_empty(
    c(
      concept$free_text_terms,
      concept$abbreviations,
      concept$spelling_variants
    )
  )
}


normalize_new_concept_schema <- function(search_terms) {

  concepts <- search_terms$search$concepts

  if (
    is.null(concepts) ||
    !is.list(concepts) ||
    length(concepts) == 0L
  ) {
    return(list())
  }

  normalized <- lapply(
    seq_along(concepts),
    function(i) {

      concept <- concepts[[i]]

      concept_id <- remove_empty(
        concept$concept_id
      )

      if (length(concept_id) == 0L) {
        concept_id <- paste0(
          "concept_",
          i
        )
      } else {
        concept_id <- concept_id[[1]]
      }

      label <- remove_empty(
        concept$label
      )

      if (length(label) == 0L) {
        label <- concept_id
      } else {
        label <- label[[1]]
      }

      role <- remove_empty(
        concept$role
      )

      if (length(role) == 0L) {
        role <- "Unspecified"
      } else {
        role <- role[[1]]
      }

      list(
        concept_id = concept_id,
        label = label,
        role = role,
        required = isTRUE(
          concept$required
        ),

        free_text_terms =
          collect_concept_terms(
            concept
          ),

        pubmed_mesh = remove_empty(
          concept$
            controlled_vocabulary$
            pubmed_mesh_candidates
        ),

        embase_emtree = remove_empty(
          concept$
            controlled_vocabulary$
            embase_emtree_candidates
        )
      )
    }
  )

  names(normalized) <- vapply(
    normalized,
    function(x) x$concept_id,
    character(1)
  )

  normalized
}


normalize_legacy_concept_schema <- function(search_terms) {

  intervention_terms <- remove_empty(
    c(
      search_terms$intervention$class_terms,
      search_terms$intervention$drug_terms
    )
  )

  procedure_terms <- remove_empty(
    c(
      search_terms$procedure$general_terms,
      search_terms$procedure$specific_terms
    )
  )

  perioperative_terms <- remove_empty(
    search_terms$optional$perioperative_terms
  )

  use_perioperative_terms <- isTRUE(
    search_terms$
      optional$
      use_perioperative_terms
  )

  concepts <- list(
    intervention = list(
      concept_id = "intervention",
      label = "Intervention or exposure",
      role = "Intervention/Exposure",
      required = TRUE,
      free_text_terms = intervention_terms,

      pubmed_mesh = remove_empty(
        search_terms$
          controlled_vocabulary$
          pubmed$
          intervention_mesh
      ),

      embase_emtree = remove_empty(
        search_terms$
          controlled_vocabulary$
          embase$
          intervention_emtree
      )
    ),

    procedure = list(
      concept_id = "procedure",
      label = "Population or procedure",
      role = "Population/Procedure",
      required = TRUE,
      free_text_terms = procedure_terms,

      pubmed_mesh = remove_empty(
        search_terms$
          controlled_vocabulary$
          pubmed$
          procedure_mesh
      ),

      embase_emtree = remove_empty(
        search_terms$
          controlled_vocabulary$
          embase$
          procedure_emtree
      )
    )
  )

  if (
    use_perioperative_terms &&
    length(perioperative_terms) > 0L
  ) {

    concepts$perioperative <- list(
      concept_id = "perioperative",
      label = "Perioperative context",
      role = "Context",
      required = FALSE,
      free_text_terms = perioperative_terms,
      pubmed_mesh = character(0),
      embase_emtree = character(0)
    )
  }

  concepts
}


normalize_search_concepts <- function(search_terms) {

  has_new_schema <- (
    is.list(search_terms$search) &&
      is.list(search_terms$search$concepts) &&
      length(search_terms$search$concepts) > 0L
  )

  has_legacy_schema <- (
    is.list(search_terms$intervention) ||
      is.list(search_terms$procedure)
  )

  if (has_new_schema) {

    concepts <- normalize_new_concept_schema(
      search_terms
    )

    schema_type <- "concept_based"

  } else if (has_legacy_schema) {

    concepts <- normalize_legacy_concept_schema(
      search_terms
    )

    schema_type <- "legacy"

  } else {

    stop(
      "Unsupported search-term configuration schema.\n",
      "Expected either:\n",
      "- search.concepts, or\n",
      "- intervention/procedure legacy sections."
    )
  }

  list(
    schema_type = schema_type,
    concepts = concepts
  )
}


# ---------------------------------------------------------
# Collect canonical concepts
# ---------------------------------------------------------

normalized_search <- normalize_search_concepts(
  search_terms
)

search_concepts <- normalized_search$concepts

if (length(search_concepts) == 0L) {
  stop(
    "No search concepts were provided."
  )
}


# ---------------------------------------------------------
# Validate concepts
# ---------------------------------------------------------

concept_has_terms <- vapply(
  search_concepts,
  function(concept) {

    length(
      remove_empty(
        c(
          concept$free_text_terms,
          concept$pubmed_mesh,
          concept$embase_emtree
        )
      )
    ) > 0L
  },
  logical(1)
)

required_concepts <- vapply(
  search_concepts,
  function(concept) {

    isTRUE(
      concept$required
    )
  },
  logical(1)
)

missing_required <- names(search_concepts)[
  required_concepts &
    !concept_has_terms
]

if (length(missing_required) > 0L) {

  missing_labels <- vapply(
    search_concepts[missing_required],
    function(concept) {

      paste0(
        concept$label,
        " [",
        concept$concept_id,
        "]"
      )
    },
    character(1)
  )

  stop(
    "Required search concepts contain no usable terms:\n- ",
    paste(
      missing_labels,
      collapse = "\n- "
    )
  )
}


# Remove empty optional concepts
search_concepts <- search_concepts[
  concept_has_terms
]

if (length(search_concepts) < 2L) {

  stop(
    "At least two non-empty search concepts are required ",
    "to generate a Boolean search strategy."
  )
}


cat(
  "\nSearch-term schema:",
  normalized_search$schema_type,
  "\n"
)

cat(
  "Usable search concepts:",
  length(search_concepts),
  "\n"
)

# ---------------------------------------------------------
# Generate PubMed search
# ---------------------------------------------------------

pubmed_blocks <- vapply(
  search_concepts,
  function(concept) {

    block_terms <- c(
      vapply(
        concept$pubmed_mesh,
        format_pubmed_mesh,
        character(1)
      ),

      vapply(
        concept$free_text_terms,
        format_pubmed_free_text,
        character(1)
      )
    )

    combine_or_block(
      block_terms
    )
  },
  character(1)
)

pubmed_blocks <- pubmed_blocks[
  nzchar(pubmed_blocks)
]

pubmed_search <- paste(
  pubmed_blocks,
  collapse = "\nAND\n"
)

# ---------------------------------------------------------
# Generate Embase search
# ---------------------------------------------------------

embase_blocks <- vapply(
  search_concepts,
  function(concept) {

    block_terms <- c(
      vapply(
        concept$embase_emtree,
        format_embase_emtree,
        character(1)
      ),

      vapply(
        concept$free_text_terms,
        format_embase_free_text,
        character(1)
      )
    )

    combine_or_block(
      block_terms
    )
  },
  character(1)
)

embase_blocks <- embase_blocks[
  nzchar(embase_blocks)
]

embase_search <- paste(
  embase_blocks,
  collapse = "\nAND\n"
)

# ---------------------------------------------------------
# Generate CENTRAL search
# CENTRAL uses PubMed MeSH concepts as starting candidates.
# The final descriptor names must be checked in CENTRAL.
# ---------------------------------------------------------

central_blocks <- vapply(
  search_concepts,
  function(concept) {

    block_terms <- c(
      vapply(
        concept$pubmed_mesh,
        format_central_mesh,
        character(1)
      ),

      vapply(
        concept$free_text_terms,
        format_central_free_text,
        character(1)
      )
    )

    combine_or_block(
      block_terms
    )
  },
  character(1)
)

central_blocks <- central_blocks[
  nzchar(central_blocks)
]

central_search <- paste(
  central_blocks,
  collapse = "\nAND\n"
)

# ---------------------------------------------------------
# Save generated strategies using canonical paths
# ---------------------------------------------------------

ensure_directory(
  paths$search_strategies_directory
)

writeLines(
  pubmed_search,
  con = paths$pubmed_search,
  useBytes = TRUE
)

writeLines(
  embase_search,
  con = paths$embase_search,
  useBytes = TRUE
)

writeLines(
  central_search,
  con = paths$central_search,
  useBytes = TRUE
)

# ---------------------------------------------------------
# Save strategy and concept summaries
# ---------------------------------------------------------

strategy_summary <- data.frame(
  database = c(
    "PubMed",
    "Embase",
    "CENTRAL"
  ),

  output_file = c(
    paths$pubmed_search,
    paths$embase_search,
    paths$central_search
  ),

  schema_type = normalized_search$schema_type,
  concept_count = length(search_concepts),
  generated_at = as.character(Sys.time()),
  stringsAsFactors = FALSE
)


concept_summary <- data.frame(
  concept_id = vapply(
    search_concepts,
    function(x) x$concept_id,
    character(1)
  ),

  concept_label = vapply(
    search_concepts,
    function(x) x$label,
    character(1)
  ),

  concept_role = vapply(
    search_concepts,
    function(x) x$role,
    character(1)
  ),

  required = vapply(
    search_concepts,
    function(x) isTRUE(x$required),
    logical(1)
  ),

  free_text_term_count = vapply(
    search_concepts,
    function(x) length(x$free_text_terms),
    integer(1)
  ),

  pubmed_mesh_count = vapply(
    search_concepts,
    function(x) length(x$pubmed_mesh),
    integer(1)
  ),

  embase_emtree_count = vapply(
    search_concepts,
    function(x) length(x$embase_emtree),
    integer(1)
  ),

  stringsAsFactors = FALSE
)


readr::write_csv(
  strategy_summary,
  paths$search_strategy_summary
)

readr::write_csv(
  concept_summary,
  file.path(
    paths$search_strategies_directory,
    "search_concept_summary.csv"
  )
)

# ---------------------------------------------------------
# Display
# ---------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("Search strategies generated successfully\n")
cat("====================================================\n")

cat("\nPUBMED SEARCH\n")
cat("----------------------------------------------------\n")
cat(pubmed_search, "\n")

cat("\nEMBASE SEARCH\n")
cat("----------------------------------------------------\n")
cat(embase_search, "\n")

cat("\nCENTRAL SEARCH\n")
cat("----------------------------------------------------\n")
cat(central_search, "\n")

cat("\nGenerated files:\n")
cat("- ", paths$pubmed_search, "\n", sep = "")
cat("- ", paths$embase_search, "\n", sep = "")
cat("- ", paths$central_search, "\n", sep = "")
cat("- ", paths$search_strategy_summary, "\n", sep = "")

cat(
  "\nThese strategies are drafts.",
  "\nDatabase-specific syntax and controlled vocabulary",
  "\nmust be validated before the searches are executed.\n"
)