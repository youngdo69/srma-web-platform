# ============================================================
# Universal SR-MA Agent Toolkit
# R/11_generate_search_strategies.R
#
# v0.8.2 purpose
# ------------------------------------------------------------
# 1. Read canonical concept-based search terms
# 2. Generate PubMed / Embase / CENTRAL strategies
# 3. Detect logical-overlap / ambiguous-acronym risks
# 4. Obtain a PubMed count BEFORE download
# 5. Use the existing LLM provider to judge whether the search
#    is plausibly over-broad for the review question
# 6. If needed, refine the strategy at the CONCEPT-TERM level
#    so all database strategies remain coherent
# 7. Optionally generate a supplementary sensitivity search
# 8. Save an auditable search-quality report for R/16
#
# Important
# ------------------------------------------------------------
# - No fixed "too many records" threshold is used.
# - LLM refinement can only KEEP / DROP existing canonical terms.
#   It cannot silently invent new controlled vocabulary.
# - Cross-block reuse is allowed by R/12 validation, but exact
#   term reuse across required AND blocks is surfaced here as a
#   semantic/logical quality risk.
# ============================================================

required_packages <- c(
  "yaml",
  "readr",
  "rentrez",
  "jsonlite",
  "dotenv",
  "httr2"
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
    "R/11 requires packages recorded in renv.lock but currently unavailable: ",
    paste(missing_packages, collapse = ", "),
    "\nRun source(\"START_HERE.R\") so the complete renv environment is restored.",
    call. = FALSE
  )
}

source("R/utils/config_utils.R")
source("R/utils/path_utils.R")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

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

config_file <- paths$search_terms_file

if (!file.exists(config_file)) {
  stop(
    "Search-term configuration file does not exist:\n",
    config_file,
    "\nRun the initialization / search-term workflow first.",
    call. = FALSE
  )
}

search_terms <- yaml::read_yaml(config_file)

# ============================================================
# Helpers
# ============================================================

remove_empty <- function(x) {
  if (is.null(x)) return(character(0))

  x <- as.character(x)
  x <- trimws(x)
  x <- x[!is.na(x) & nzchar(x)]

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

  if (length(terms) == 0L) return("")

  paste0(
    "(\n  ",
    paste(terms, collapse = "\n  OR "),
    "\n)"
  )
}

format_pubmed_free_text <- function(term) {
  term <- trimws(term)

  if (is_single_word(term)) {
    paste0(term, "[Title/Abstract]")
  } else {
    paste0(quote_term(term), "[Title/Abstract]")
  }
}

format_pubmed_mesh <- function(term) {
  paste0(quote_term(trimws(term)), "[Mesh]")
}

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

# CENTRAL does not accept '/' in the generated query.
# For free text, replace slash characters with spaces.
sanitize_central_free_text <- function(term) {
  term <- trimws(term)
  term <- gsub("/", " ", term, fixed = TRUE)
  term <- gsub("\\s+", " ", term, perl = TRUE)
  trimws(term)
}

# PubMed MeSH qualifiers such as "Aortic Valve/surgery" cannot be
# translated safely into CENTRAL by simply retaining '/'.
# To avoid invalid syntax AND avoid broadening the concept by silently
# converting a qualified heading into its broad parent heading, omit
# qualified descriptors from CENTRAL. Existing concept free-text terms
# remain available in the same OR block.
sanitize_central_mesh_term <- function(term) {
  term <- trimws(term)

  if (grepl("/", term, fixed = TRUE)) {
    return("")
  }

  term
}

format_central_free_text <- function(term) {
  term <- sanitize_central_free_text(term)

  if (!nzchar(term)) return("")

  if (is_single_word(term)) {
    paste0(term, ":ti,ab,kw")
  } else {
    paste0(quote_term(term), ":ti,ab,kw")
  }
}

format_central_mesh <- function(term) {
  term <- sanitize_central_mesh_term(term)

  if (!nzchar(term)) return("")

  paste0(
    '[mh "',
    escape_quotes(term),
    '"]'
  )
}

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

      concept_id <- remove_empty(concept$concept_id)
      if (length(concept_id) == 0L) {
        concept_id <- paste0("concept_", i)
      } else {
        concept_id <- concept_id[[1]]
      }

      label <- remove_empty(concept$label)
      if (length(label) == 0L) {
        label <- concept_id
      } else {
        label <- label[[1]]
      }

      role <- remove_empty(concept$role)
      if (length(role) == 0L) {
        role <- "Unspecified"
      } else {
        role <- role[[1]]
      }

      list(
        concept_id = concept_id,
        label = label,
        role = role,
        required = isTRUE(concept$required),
        include_in_primary = TRUE,
        free_text_terms = collect_concept_terms(concept),
        pubmed_mesh = remove_empty(
          concept$controlled_vocabulary$pubmed_mesh_candidates
        ),
        embase_emtree = remove_empty(
          concept$controlled_vocabulary$embase_emtree_candidates
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

  concepts <- list(
    intervention = list(
      concept_id = "intervention",
      label = "Intervention or exposure",
      role = "Intervention/Exposure",
      required = TRUE,
      include_in_primary = TRUE,
      free_text_terms = intervention_terms,
      pubmed_mesh = remove_empty(
        search_terms$controlled_vocabulary$pubmed$intervention_mesh
      ),
      embase_emtree = remove_empty(
        search_terms$controlled_vocabulary$embase$intervention_emtree
      )
    ),
    procedure = list(
      concept_id = "procedure",
      label = "Population or procedure",
      role = "Population/Procedure",
      required = TRUE,
      include_in_primary = TRUE,
      free_text_terms = procedure_terms,
      pubmed_mesh = remove_empty(
        search_terms$controlled_vocabulary$pubmed$procedure_mesh
      ),
      embase_emtree = remove_empty(
        search_terms$controlled_vocabulary$embase$procedure_emtree
      )
    )
  )

  perioperative_terms <- remove_empty(
    search_terms$optional$perioperative_terms
  )

  if (
    isTRUE(search_terms$optional$use_perioperative_terms) &&
    length(perioperative_terms) > 0L
  ) {
    concepts$perioperative <- list(
      concept_id = "perioperative",
      label = "Perioperative context",
      role = "Context",
      required = FALSE,
      include_in_primary = TRUE,
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
    list(
      schema_type = "concept_based",
      concepts = normalize_new_concept_schema(search_terms)
    )
  } else if (has_legacy_schema) {
    list(
      schema_type = "legacy",
      concepts = normalize_legacy_concept_schema(search_terms)
    )
  } else {
    stop(
      "Unsupported search-term configuration schema.\n",
      "Expected search.concepts or the supported legacy schema.",
      call. = FALSE
    )
  }
}

concept_has_terms <- function(concept) {
  length(
    remove_empty(
      c(
        concept$free_text_terms,
        concept$pubmed_mesh,
        concept$embase_emtree
      )
    )
  ) > 0L
}

build_database_searches <- function(concepts) {
  included <- concepts[
    vapply(
      concepts,
      function(x) isTRUE(x$include_in_primary) && concept_has_terms(x),
      logical(1)
    )
  ]

  if (length(included) < 2L) {
    stop(
      "Search refinement left fewer than two usable primary concepts.",
      call. = FALSE
    )
  }

  pubmed_blocks <- vapply(
    included,
    function(concept) {
      combine_or_block(
        c(
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
      )
    },
    character(1)
  )

  embase_blocks <- vapply(
    included,
    function(concept) {
      combine_or_block(
        c(
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
      )
    },
    character(1)
  )

  central_blocks <- vapply(
    included,
    function(concept) {
      combine_or_block(
        c(
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
      )
    },
    character(1)
  )

  central_query <- paste(
    central_blocks[nzchar(central_blocks)],
    collapse = "\nAND\n"
  )

  # CENTRAL generation guard: '/' must never reach the output query.
  if (grepl("/", central_query, fixed = TRUE)) {
    stop(
      "CENTRAL search generation produced an unsupported '/' character.",
      call. = FALSE
    )
  }

  list(
    pubmed = paste(pubmed_blocks[nzchar(pubmed_blocks)], collapse = "\nAND\n"),
    embase = paste(embase_blocks[nzchar(embase_blocks)], collapse = "\nAND\n"),
    central = central_query,
    concepts = included
  )
}

normalize_term_key <- function(x) {
  tolower(trimws(gsub("\\s+", " ", x)))
}

find_shared_terms <- function(concepts, field) {
  rows <- list()

  for (concept in concepts) {
    values <- remove_empty(concept[[field]])

    if (length(values) == 0L) next

    for (value in values) {
      rows[[length(rows) + 1L]] <- data.frame(
        concept_id = concept$concept_id,
        required = isTRUE(concept$required),
        term = value,
        key = normalize_term_key(value),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0L) {
    return(data.frame())
  }

  tab <- do.call(rbind, rows)
  split_rows <- split(tab, tab$key)

  hits <- lapply(
    split_rows,
    function(x) {
      unique_concepts <- unique(x$concept_id)

      if (length(unique_concepts) < 2L) return(NULL)

      data.frame(
        term = x$term[[1]],
        concept_ids = paste(unique_concepts, collapse = "; "),
        required_concepts = sum(
          vapply(
            unique_concepts,
            function(id) {
              isTRUE(
                concepts[[id]]$required
              )
            },
            logical(1)
          )
        ),
        stringsAsFactors = FALSE
      )
    }
  )

  hits <- hits[!vapply(hits, is.null, logical(1))]

  if (length(hits) == 0L) data.frame() else do.call(rbind, hits)
}

find_ambiguous_acronyms <- function(concepts) {
  rows <- list()

  for (concept in concepts) {
    terms <- remove_empty(concept$free_text_terms)

    risky <- terms[
      grepl("^[A-Z]{2,4}$", terms)
    ]

    if (length(risky) == 0L) next

    rows[[length(rows) + 1L]] <- data.frame(
      concept_id = concept$concept_id,
      term = risky,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0L) data.frame() else do.call(rbind, rows)
}

pubmed_count <- function(query) {
  if (!nzchar(trimws(query))) return(NA_integer_)

  api_key <- Sys.getenv("NCBI_API_KEY", unset = "")

  if (nzchar(api_key)) {
    rentrez::set_entrez_key(api_key)
  }

  result <- tryCatch(
    rentrez::entrez_search(
      db = "pubmed",
      term = query,
      retmax = 0,
      use_history = FALSE
    ),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    warning(
      "PubMed count-only preflight failed: ",
      conditionMessage(result),
      call. = FALSE
    )
    return(NA_integer_)
  }

  suppressWarnings(as.integer(result$count))
}

load_search_provider <- function() {
  source("R/utils/llm_provider_utils.R")

  agent_file <- file.path("config", "initialization_agent.yml")

  if (!file.exists(agent_file)) {
    stop(
      "Missing provider configuration: ",
      agent_file,
      call. = FALSE
    )
  }

  agent_document <- yaml::read_yaml(agent_file)
  agent_config <- agent_document$initialization_agent

  if (
    is.null(agent_config) ||
    !is.list(agent_config) ||
    !isTRUE(agent_config$enabled)
  ) {
    stop(
      "initialization_agent provider configuration is unavailable or disabled.",
      call. = FALSE
    )
  }

  if (file.exists(".env")) {
    load_project_env(".env")
  }

  provider_config <- agent_config$provider %||% list()
  provider_config$timeout_seconds <-
    agent_config$processing$timeout_seconds %||% 180

  list(
    name = provider_config$name %||% "openai",
    config = provider_config
  )
}

build_search_qc_schema <- function() {
  list(
    type = "object",
    additionalProperties = FALSE,
    required = c(
      "action",
      "rationale",
      "concept_plan",
      "needs_supplementary_search",
      "supplementary_concept_ids",
      "supplementary_rationale"
    ),
    properties = list(
      action = list(
        type = "string",
        enum = c("ACCEPT", "REFINE")
      ),
      rationale = list(type = "string"),
      concept_plan = list(
        type = "array",
        items = list(
          type = "object",
          additionalProperties = FALSE,
          required = c(
            "concept_id",
            "include_in_primary",
            "keep_free_text_terms",
            "keep_pubmed_mesh",
            "keep_embase_emtree",
            "rationale"
          ),
          properties = list(
            concept_id = list(type = "string"),
            include_in_primary = list(type = "boolean"),
            keep_free_text_terms = list(
              type = "array",
              items = list(type = "string")
            ),
            keep_pubmed_mesh = list(
              type = "array",
              items = list(type = "string")
            ),
            keep_embase_emtree = list(
              type = "array",
              items = list(type = "string")
            ),
            rationale = list(type = "string")
          )
        )
      ),
      needs_supplementary_search = list(type = "boolean"),
      supplementary_concept_ids = list(
        type = "array",
        items = list(type = "string")
      ),
      supplementary_rationale = list(type = "string")
    )
  )
}

concepts_for_prompt <- function(concepts) {
  lapply(
    concepts,
    function(x) {
      list(
        concept_id = x$concept_id,
        label = x$label,
        role = x$role,
        required = isTRUE(x$required),
        free_text_terms = x$free_text_terms,
        pubmed_mesh = x$pubmed_mesh,
        embase_emtree = x$embase_emtree
      )
    }
  )
}

build_search_qc_prompt <- function(
    title,
    review_question,
    concepts,
    pubmed_query,
    hit_count,
    shared_mesh,
    shared_emtree,
    shared_free_text,
    ambiguous_acronyms,
    prior_feedback = ""
) {
  context <- list(
    review_title = title,
    review_question = review_question,
    pubmed_hit_count = if (is.na(hit_count)) NULL else hit_count,
    concepts = concepts_for_prompt(concepts),
    current_pubmed_query = pubmed_query,
    prior_candidate_feedback = prior_feedback,
    deterministic_quality_signals = list(
      shared_pubmed_mesh_across_concepts = shared_mesh,
      shared_embase_emtree_across_concepts = shared_emtree,
      shared_free_text_across_concepts = shared_free_text,
      short_uppercase_acronyms = ambiguous_acronyms
    )
  )

  paste(
    "You are the Universal SR-MA Toolkit search-strategy quality-control agent.",
    "",
    "Decide whether the CURRENT primary search is defensibly sensitive yet unnecessarily broad for THIS review question.",
    "There is deliberately NO fixed record-count threshold. Interpret the PubMed hit count in the context of the review scope, concepts, and query structure.",
    "",
    "Important methodological rules:",
    "1. Preserve systematic-review recall. Do not narrow merely to reduce workload.",
    "2. Exact controlled-vocabulary reuse across different required AND blocks can create semantic collapse: the same generic term may satisfy multiple concepts. Treat this as a quality risk, not an automatic error.",
    "3. Short acronyms such as AR or AVR can be ambiguous; specific acronyms such as TAVR or SAVR may still be appropriate. Judge them contextually.",
    "4. If action=REFINE, you may ONLY keep or drop terms already present in each concept. Do NOT invent new MeSH, Emtree, or free-text terms.",
    "5. Every concept_id in the input must appear exactly once in concept_plan.",
    "6. keep_* lists must be exact subsets of the corresponding original term lists.",
    "7. include_in_primary may be FALSE only when omitting that concept improves methodological sensitivity/coherence and the review can still be searched with at least two concepts.",
    "8. A supplementary search is optional. Use it only when a focused device/exposure/subgroup concept needs a separate sensitivity search to protect recall without making the primary query logically incoherent.",
    "9. If the current search is acceptable, action=ACCEPT and return each concept unchanged.",
    "10. After any prior refinement, reassess the CURRENT query from scratch. Do not ACCEPT merely because the record count decreased.",
    "11. ACCEPT only when both the Boolean concept structure and the remaining term set are methodologically coherent for the stated review question and the observed PubMed volume is plausible for that scope.",
    "12. When a narrowly framed review explicitly requires a direct comparison, each treatment/exposure/comparator block must contain role-discriminating terms. A generic parent-procedure term in one arm that can also describe the other arm creates semantic cross-block contamination even when the exact string is not duplicated.",
    "13. In comparative reviews, generic umbrella terms that do not distinguish one required arm from the other should usually be removed from the affected arm when more specific synonyms/controlled vocabulary remain to preserve recall.",
    "14. A term may be over-broad because of meaning, not only because the same exact term appears in two blocks. Assess semantic nesting and parent-child procedure relationships.",
    "15. When prior_candidate_feedback is supplied, generate a NEW proposal from the ORIGINAL current concept set; do not cumulatively narrow a previously rejected candidate.",
    "16. Do not claim to have searched external sources. Base the decision only on the supplied context and PubMed count.",
    "",
    "Return only the requested structured JSON.",
    "",
    jsonlite::toJSON(
      context,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    ),
    sep = "\n"
  )
}

validate_refinement_plan <- function(parsed, concepts) {
  if (
    is.null(parsed$concept_plan) ||
    !is.list(parsed$concept_plan)
  ) {
    return(list(ok = FALSE, reason = "Missing concept_plan."))
  }

  original_ids <- names(concepts)

  plan_ids <- vapply(
    parsed$concept_plan,
    function(x) as.character(x$concept_id %||% ""),
    character(1)
  )

  if (
    length(plan_ids) != length(original_ids) ||
    anyDuplicated(plan_ids) ||
    !setequal(plan_ids, original_ids)
  ) {
    return(list(
      ok = FALSE,
      reason = "concept_plan must contain every canonical concept exactly once."
    ))
  }

  refined <- concepts

  exact_subset <- function(candidate, original) {
    candidate <- remove_empty(candidate)
    original <- remove_empty(original)

    all(candidate %in% original)
  }

  for (item in parsed$concept_plan) {
    id <- as.character(item$concept_id)
    original <- concepts[[id]]

    if (
      !exact_subset(item$keep_free_text_terms, original$free_text_terms) ||
      !exact_subset(item$keep_pubmed_mesh, original$pubmed_mesh) ||
      !exact_subset(item$keep_embase_emtree, original$embase_emtree)
    ) {
      return(list(
        ok = FALSE,
        reason = paste0(
          "Refinement attempted to introduce a non-canonical term for concept: ",
          id
        )
      ))
    }

    if (
      isTRUE(original$required) &&
      !isTRUE(item$include_in_primary)
    ) {
      return(list(
        ok = FALSE,
        reason = paste0(
          "A required canonical search concept cannot be removed from the primary strategy: ",
          id
        )
      ))
    }

    refined[[id]]$include_in_primary <- isTRUE(item$include_in_primary)
    refined[[id]]$free_text_terms <- remove_empty(item$keep_free_text_terms)
    refined[[id]]$pubmed_mesh <- remove_empty(item$keep_pubmed_mesh)
    refined[[id]]$embase_emtree <- remove_empty(item$keep_embase_emtree)

    if (
      isTRUE(refined[[id]]$include_in_primary) &&
      !concept_has_terms(refined[[id]])
    ) {
      return(list(
        ok = FALSE,
        reason = paste0(
          "Included primary concept has no retained terms: ",
          id
        )
      ))
    }
  }

  included_count <- sum(
    vapply(
      refined,
      function(x) isTRUE(x$include_in_primary) && concept_has_terms(x),
      logical(1)
    )
  )

  if (included_count < 2L) {
    return(list(
      ok = FALSE,
      reason = "Refinement would leave fewer than two primary concepts."
    ))
  }

  list(
    ok = TRUE,
    reason = "",
    concepts = refined
  )
}

run_llm_search_qc <- function(
    title,
    review_question,
    concepts,
    baseline_query,
    baseline_count,
    shared_mesh,
    shared_emtree,
    shared_free_text,
    ambiguous_acronyms,
    prior_feedback = ""
) {
  tryCatch(
    {
      provider <- load_search_provider()

      prompt <- build_search_qc_prompt(
        title = title,
        review_question = review_question,
        concepts = concepts,
        pubmed_query = baseline_query,
        hit_count = baseline_count,
        shared_mesh = shared_mesh,
        shared_emtree = shared_emtree,
        shared_free_text = shared_free_text,
        ambiguous_acronyms = ambiguous_acronyms,
        prior_feedback = prior_feedback
      )

      response <- call_llm_screening_provider(
        provider_name = provider$name,
        prompt = prompt,
        provider_config = provider$config,
        json_schema = build_search_qc_schema(),
        schema_name = "srma_search_strategy_qc_v1"
      )

      list(
        ok = TRUE,
        parsed = response$parsed,
        response_id = response$response_id %||% "",
        model = response$model %||% provider$config$model %||% "",
        raw_response = response$raw_response %||% ""
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        parsed = NULL,
        response_id = "",
        model = "",
        raw_response = "",
        error = conditionMessage(e)
      )
    }
  )
}


build_candidate_review_schema <- function() {
  list(
    type = "object",
    additionalProperties = FALSE,
    required = c(
      "decision",
      "recall_risk",
      "role_discrimination_ok",
      "semantic_cross_block_risk",
      "rationale",
      "coverage_assessment",
      "role_discrimination_assessment",
      "dropped_term_assessment"
    ),
    properties = list(
      decision = list(
        type = "string",
        enum = c(
          "ACCEPT_CANDIDATE",
          "REJECT_OVER_NARROW",
          "REJECT_STILL_BROAD"
        )
      ),
      recall_risk = list(
        type = "string",
        enum = c("LOW", "MODERATE", "HIGH")
      ),
      role_discrimination_ok = list(type = "boolean"),
      semantic_cross_block_risk = list(
        type = "string",
        enum = c("LOW", "MODERATE", "HIGH")
      ),
      rationale = list(type = "string"),
      coverage_assessment = list(type = "string"),
      role_discrimination_assessment = list(type = "string"),
      dropped_term_assessment = list(type = "string")
    )
  )
}

term_difference_summary <- function(original_concepts, candidate_concepts) {
  out <- list()

  for (id in names(original_concepts)) {
    original <- original_concepts[[id]]
    candidate <- candidate_concepts[[id]]

    out[[id]] <- list(
      concept_id = id,
      label = original$label,
      role = original$role,
      required = isTRUE(original$required),
      dropped_free_text_terms = setdiff(
        remove_empty(original$free_text_terms),
        remove_empty(candidate$free_text_terms)
      ),
      dropped_pubmed_mesh = setdiff(
        remove_empty(original$pubmed_mesh),
        remove_empty(candidate$pubmed_mesh)
      ),
      dropped_embase_emtree = setdiff(
        remove_empty(original$embase_emtree),
        remove_empty(candidate$embase_emtree)
      ),
      retained_free_text_terms = remove_empty(
        candidate$free_text_terms
      ),
      retained_pubmed_mesh = remove_empty(
        candidate$pubmed_mesh
      ),
      retained_embase_emtree = remove_empty(
        candidate$embase_emtree
      )
    )
  }

  out
}

build_candidate_review_prompt <- function(
    title,
    review_question,
    original_concepts,
    original_query,
    original_count,
    candidate_concepts,
    candidate_query,
    candidate_count,
    candidate_required_overlap
) {
  context <- list(
    review_title = title,
    review_question = review_question,
    original_pubmed_count = if (is.na(original_count)) NULL else original_count,
    candidate_pubmed_count = if (is.na(candidate_count)) NULL else candidate_count,
    original_concepts = concepts_for_prompt(original_concepts),
    candidate_concepts = concepts_for_prompt(candidate_concepts),
    term_changes = term_difference_summary(
      original_concepts,
      candidate_concepts
    ),
    retained_required_arm_terms = collect_arm_term_inventory(
      candidate_concepts
    ),
    original_pubmed_query = original_query,
    candidate_pubmed_query = candidate_query,
    deterministic_guardrails = list(
      candidate_required_controlled_term_overlap = candidate_required_overlap
    )
  )

  paste(
    "You are the independent candidate-review stage of the Universal SR-MA Toolkit search engine.",
    "",
    "A separate agent has proposed a narrower search. Your job is NOT to make the result count smaller. Your job is to protect recall while deciding whether the candidate is methodologically preferable to the original query.",
    "",
    "Rules:",
    "1. Evaluate the candidate against the stated review question, not against a target number of records.",
    "2. Compare concept coverage and every dropped term with the original strategy.",
    "3. Large count reduction is a warning signal, not proof of over-narrowing or improvement.",
    "4. Reject as REJECT_OVER_NARROW when clinically/methodologically important synonyms, specific controlled vocabulary, device/intervention names, or required concept coverage have been lost without adequate redundancy.",
    "5. Reject as REJECT_STILL_BROAD when the candidate preserves recall but still contains generic terms or Boolean structure that clearly retrieves a much broader literature universe than the review question.",
    "6. For comparative reviews, explicitly audit EVERY retained term listed in retained_required_arm_terms for ROLE DISCRIMINATION: can a paper describing treatment/exposure arm A satisfy the comparator/arm-B block merely because that term is a generic parent-procedure, umbrella heading, anatomy/surgery heading, or cross-arm phrase? If yes, role_discrimination_ok must be FALSE and the candidate must not be accepted.",
    "7. Semantic cross-block contamination includes parent-child or umbrella-specific relationships even when there is no exact duplicate term. A controlled-vocabulary qualifier such as /surgery, /therapy, or /methods does not by itself make a term arm-specific.",
    "8. ACCEPT_CANDIDATE only when the candidate preserves defensible systematic-review sensitivity, materially improves specificity/coherence, AND every retained required-arm term is role-compatible.",
    "9. If recall risk is HIGH or semantic_cross_block_risk is HIGH, do not ACCEPT_CANDIDATE.",
    "10. A MODERATE recall risk can be acceptable only when redundancy is clearly documented and role discrimination is intact.",
    "11. If any retained required-arm term is an umbrella or can reasonably match the competing required arm, use REJECT_STILL_BROAD and explain the offending term(s) in role_discrimination_assessment.",
    "12. Do not perform or claim external searching. Use only the supplied query structure, concepts, term changes, and PubMed counts.",
    "",
    "Return only the requested structured JSON.",
    "",
    jsonlite::toJSON(
      context,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    ),
    sep = "\n"
  )
}

run_llm_candidate_review <- function(
    title,
    review_question,
    original_concepts,
    original_query,
    original_count,
    candidate_concepts,
    candidate_query,
    candidate_count,
    candidate_required_overlap
) {
  provider <- tryCatch(
    load_search_provider(),
    error = function(e) e
  )

  if (inherits(provider, "error")) {
    return(list(
      ok = FALSE,
      parsed = NULL,
      response_id = "",
      model = "",
      raw_response = "",
      error = conditionMessage(provider)
    ))
  }

  prompt <- build_candidate_review_prompt(
    title = title,
    review_question = review_question,
    original_concepts = original_concepts,
    original_query = original_query,
    original_count = original_count,
    candidate_concepts = candidate_concepts,
    candidate_query = candidate_query,
    candidate_count = candidate_count,
    candidate_required_overlap = candidate_required_overlap
  )

  last_error <- ""

  for (review_attempt in 1:2) {
    result <- tryCatch(
      {
        response <- call_llm_screening_provider(
          provider_name = provider$name,
          prompt = prompt,
          provider_config = provider$config,
          json_schema = build_candidate_review_schema(),
          schema_name = "srma_search_candidate_review_v2"
        )

        list(
          ok = TRUE,
          parsed = response$parsed,
          response_id = response$response_id %||% "",
          model = response$model %||% provider$config$model %||% "",
          raw_response = response$raw_response %||% "",
          review_attempt = review_attempt
        )
      },
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      }
    )

    if (!is.null(result)) {
      return(result)
    }

    if (review_attempt == 1L) {
      cat(
        "Candidate review call failed; retrying once. Reason: ",
        last_error,
        "\n",
        sep = ""
      )
      Sys.sleep(2)
    }
  }

  list(
    ok = FALSE,
    parsed = NULL,
    response_id = "",
    model = "",
    raw_response = "",
    error = last_error
  )
}



is_comparative_arm_role <- function(role) {
  role <- tolower(trimws(as.character(role %||% "")))

  grepl(
    "intervention|exposure|treatment|comparator|control",
    role
  )
}

collect_arm_term_inventory <- function(concepts) {
  rows <- list()

  for (concept in concepts) {
    if (
      !isTRUE(concept$required) ||
      !is_comparative_arm_role(concept$role)
    ) {
      next
    }

    add_rows <- function(values, term_type) {
      values <- remove_empty(values)

      if (length(values) == 0L) return(NULL)

      data.frame(
        concept_id = rep(concept$concept_id, length(values)),
        concept_label = rep(concept$label, length(values)),
        concept_role = rep(concept$role, length(values)),
        term_type = rep(term_type, length(values)),
        term = values,
        stringsAsFactors = FALSE
      )
    }

    rows[[length(rows) + 1L]] <- add_rows(
      concept$free_text_terms,
      "free_text"
    )

    rows[[length(rows) + 1L]] <- add_rows(
      concept$pubmed_mesh,
      "pubmed_mesh"
    )

    rows[[length(rows) + 1L]] <- add_rows(
      concept$embase_emtree,
      "embase_emtree"
    )
  }

  rows <- rows[
    !vapply(rows, is.null, logical(1))
  ]

  if (length(rows) == 0L) {
    return(data.frame())
  }

  unique(
    do.call(rbind, rows)
  )
}

build_arm_term_audit_schema <- function() {
  list(
    type = "object",
    additionalProperties = FALSE,
    required = c(
      "overall_role_discrimination_ok",
      "rationale",
      "assessments"
    ),
    properties = list(
      overall_role_discrimination_ok = list(
        type = "boolean"
      ),
      rationale = list(type = "string"),
      assessments = list(
        type = "array",
        items = list(
          type = "object",
          additionalProperties = FALSE,
          required = c(
            "concept_id",
            "term_type",
            "term",
            "classification",
            "role_specific",
            "can_match_other_required_arm",
            "keep_recommended",
            "rationale"
          ),
          properties = list(
            concept_id = list(type = "string"),
            term_type = list(
              type = "string",
              enum = c(
                "free_text",
                "pubmed_mesh",
                "embase_emtree"
              )
            ),
            term = list(type = "string"),
            classification = list(
              type = "string",
              enum = c(
                "ROLE_SPECIFIC",
                "UMBRELLA",
                "CROSS_ARM",
                "CONTEXT_ONLY"
              )
            ),
            role_specific = list(type = "boolean"),
            can_match_other_required_arm = list(type = "boolean"),
            keep_recommended = list(type = "boolean"),
            rationale = list(type = "string")
          )
        )
      )
    )
  )
}

build_arm_term_audit_prompt <- function(
    title,
    review_question,
    concepts,
    term_inventory
) {
  context <- list(
    review_title = title,
    review_question = review_question,
    required_comparative_concepts = concepts_for_prompt(
      concepts[
        vapply(
          concepts,
          function(x) {
            isTRUE(x$required) &&
              is_comparative_arm_role(x$role)
          },
          logical(1)
        )
      ]
    ),
    retained_terms_to_audit = term_inventory
  )

  paste(
    "You are the arm-discrimination audit stage of the Universal SR-MA Toolkit search engine.",
    "",
    "Audit EACH retained term in each required intervention/exposure/treatment/comparator/control concept.",
    "The purpose is to detect semantic cross-arm contamination that exact-string duplicate checks cannot detect.",
    "",
    "Classification rules:",
    "1. ROLE_SPECIFIC: the term clearly identifies the assigned arm or modality and does not ordinarily denote the competing required arm.",
    "2. UMBRELLA: the term is a parent, generic procedure, generic anatomy/therapy label, or broad controlled-vocabulary term that can include more than one required arm.",
    "3. CROSS_ARM: the term is assigned to one arm but can directly describe or lexically/semantically subsume another required arm.",
    "4. CONTEXT_ONLY: the term describes context rather than the assigned comparative arm.",
    "5. For example, in a comparison of two treatment modalities, a generic parent-procedure term must not be treated as role-specific merely because it appears in the comparator concept.",
    "6. Controlled-vocabulary qualifiers such as /surgery, /therapy, or /methods are not automatically role-specific; judge whether the resulting heading actually distinguishes the assigned arm from the competing arm.",
    "7. Set can_match_other_required_arm=TRUE whenever a record about a competing required arm could reasonably satisfy this term without actually representing the assigned arm.",
    "8. overall_role_discrimination_ok may be TRUE only if every retained term in every required comparative arm is role-compatible and no UMBRELLA/CROSS_ARM/CONTEXT_ONLY term creates cross-arm contamination.",
    "9. Do not optimize toward a target record count and do not perform external searching.",
    "10. Return one assessment for EVERY supplied inventory row, preserving concept_id, term_type, and term exactly.",
    "",
    "Return only the requested structured JSON.",
    "",
    jsonlite::toJSON(
      context,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    ),
    sep = "\n"
  )
}

validate_arm_term_audit <- function(
    audit_result,
    term_inventory
) {
  if (!isTRUE(audit_result$ok)) {
    return(list(
      ok = FALSE,
      role_ok = FALSE,
      offending = data.frame(),
      reason = audit_result$error %||% "Arm-term audit unavailable."
    ))
  }

  assessments <- audit_result$parsed$assessments

  if (
    is.null(assessments) ||
    !is.list(assessments)
  ) {
    return(list(
      ok = FALSE,
      role_ok = FALSE,
      offending = data.frame(),
      reason = "Arm-term audit returned no assessment array."
    ))
  }

  input_keys <- paste(
    term_inventory$concept_id,
    term_inventory$term_type,
    term_inventory$term,
    sep = "|||"
  )

  output_keys <- vapply(
    assessments,
    function(x) {
      paste(
        as.character(x$concept_id %||% ""),
        as.character(x$term_type %||% ""),
        as.character(x$term %||% ""),
        sep = "|||"
      )
    },
    character(1)
  )

  if (
    length(output_keys) != length(input_keys) ||
    anyDuplicated(output_keys) ||
    !setequal(output_keys, input_keys)
  ) {
    return(list(
      ok = FALSE,
      role_ok = FALSE,
      offending = data.frame(),
      reason = paste(
        "Arm-term audit did not return every retained term exactly once."
      )
    ))
  }

  audit_df <- do.call(
    rbind,
    lapply(
      assessments,
      function(x) {
        data.frame(
          concept_id = as.character(x$concept_id),
          term_type = as.character(x$term_type),
          term = as.character(x$term),
          classification = toupper(
            as.character(x$classification)
          ),
          role_specific = isTRUE(x$role_specific),
          can_match_other_required_arm =
            isTRUE(x$can_match_other_required_arm),
          keep_recommended = isTRUE(x$keep_recommended),
          rationale = as.character(x$rationale %||% ""),
          stringsAsFactors = FALSE
        )
      }
    )
  )

  offending <- audit_df[
    audit_df$classification %in% c(
      "UMBRELLA",
      "CROSS_ARM",
      "CONTEXT_ONLY"
    ) |
      !audit_df$role_specific |
      audit_df$can_match_other_required_arm,
    ,
    drop = FALSE
  ]

  role_ok <- (
    isTRUE(
      audit_result$parsed$overall_role_discrimination_ok
    ) &&
      nrow(offending) == 0L
  )

  list(
    ok = TRUE,
    role_ok = role_ok,
    offending = offending,
    audit = audit_df,
    reason = as.character(
      audit_result$parsed$rationale %||% ""
    )
  )
}

run_llm_arm_term_audit <- function(
    title,
    review_question,
    concepts
) {
  inventory <- collect_arm_term_inventory(
    concepts
  )

  if (nrow(inventory) == 0L) {
    return(list(
      ok = TRUE,
      parsed = list(
        overall_role_discrimination_ok = TRUE,
        rationale = "No required comparative-arm terms required auditing.",
        assessments = list()
      ),
      inventory = inventory,
      response_id = "",
      model = "",
      raw_response = ""
    ))
  }

  tryCatch(
    {
      provider <- load_search_provider()

      prompt <- build_arm_term_audit_prompt(
        title = title,
        review_question = review_question,
        concepts = concepts,
        term_inventory = inventory
      )

      response <- call_llm_screening_provider(
        provider_name = provider$name,
        prompt = prompt,
        provider_config = provider$config,
        json_schema = build_arm_term_audit_schema(),
        schema_name = "srma_search_arm_term_audit_v1"
      )

      list(
        ok = TRUE,
        parsed = response$parsed,
        inventory = inventory,
        response_id = response$response_id %||% "",
        model = response$model %||% provider$config$model %||% "",
        raw_response = response$raw_response %||% ""
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        parsed = NULL,
        inventory = inventory,
        response_id = "",
        model = "",
        raw_response = "",
        error = conditionMessage(e)
      )
    }
  )
}

format_offending_arm_terms <- function(offending) {
  if (
    is.null(offending) ||
    !is.data.frame(offending) ||
    nrow(offending) == 0L
  ) {
    return("")
  }

  paste(
    apply(
      offending,
      1,
      function(row) {
        paste0(
          row[["concept_id"]],
          " :: ",
          row[["term_type"]],
          " :: ",
          row[["term"]],
          " [",
          row[["classification"]],
          "]"
        )
      }
    ),
    collapse = "; "
  )
}

write_query_file <- function(text, path) {
  writeLines(
    text,
    con = path,
    useBytes = TRUE
  )
}

# ============================================================
# Normalize / validate canonical concepts
# ============================================================

normalized_search <- normalize_search_concepts(search_terms)
search_concepts <- normalized_search$concepts

if (length(search_concepts) == 0L) {
  stop("No search concepts were provided.", call. = FALSE)
}

has_terms <- vapply(
  search_concepts,
  concept_has_terms,
  logical(1)
)

required_concepts <- vapply(
  search_concepts,
  function(concept) isTRUE(concept$required),
  logical(1)
)

missing_required <- names(search_concepts)[
  required_concepts & !has_terms
]

if (length(missing_required) > 0L) {
  stop(
    "Required search concepts contain no usable terms:\n- ",
    paste(missing_required, collapse = "\n- "),
    call. = FALSE
  )
}

search_concepts <- search_concepts[has_terms]

if (length(search_concepts) < 2L) {
  stop(
    "At least two non-empty search concepts are required.",
    call. = FALSE
  )
}

cat(
  "\nSearch-term schema: ",
  normalized_search$schema_type,
  "\nUsable search concepts: ",
  length(search_concepts),
  "\n",
  sep = ""
)

# ============================================================
# Baseline strategy + deterministic QC
# ============================================================

baseline <- build_database_searches(search_concepts)

shared_mesh <- find_shared_terms(search_concepts, "pubmed_mesh")
shared_emtree <- find_shared_terms(search_concepts, "embase_emtree")
shared_free_text <- find_shared_terms(search_concepts, "free_text_terms")
ambiguous_acronyms <- find_ambiguous_acronyms(search_concepts)

cat("\nPubMed pre-download count check...\n")
baseline_count <- pubmed_count(baseline$pubmed)

cat(
  "Baseline PubMed records: ",
  ifelse(is.na(baseline_count), "UNAVAILABLE", baseline_count),
  "\n",
  sep = ""
)

# ============================================================
# LLM-assisted semantic / volume QC — guarded candidate v0.8.2
# ============================================================

project_title <- get_project_title(config)
review_question <- config$research_question$question %||% project_title

# Design principle:
# ------------------------------------------------------------
# A proposed narrower query is NEVER accepted merely because its PubMed
# count is lower. Each proposal is built independently from the ORIGINAL
# canonical concept set and then reviewed by a second LLM pass that compares
# coverage and dropped terms with the original query.
#
# At most two independent candidate proposals are attempted. Rejected
# candidates are not used as the starting point for further narrowing.
# This prevents runaway cumulative term deletion such as:
#   broad -> narrower -> very narrow -> over-narrow.
#
# If no candidate is defensibly accepted, the original query is restored
# and the quality state becomes REVIEW_REQUIRED. R/16 will therefore not
# download records until the upstream strategy is resolved.

max_candidate_attempts <- 2L

quality_status <- "REVIEW_REQUIRED"
qc_action <- "FALLBACK"
qc_rationale <- ""
refinement_applied <- FALSE

final_concepts <- search_concepts
final_strategy <- baseline
final_count <- baseline_count

qc_history <- list()
prior_feedback <- ""

accepted_qc <- NULL
accepted_candidate_review <- NULL
last_qc <- NULL
last_candidate_review <- NULL

severe_deterministic_risk <- FALSE

if (nrow(shared_mesh) > 0L) {
  severe_deterministic_risk <- any(
    shared_mesh$required_concepts >= 2L
  )
}

if (!isTRUE(severe_deterministic_risk) && nrow(shared_emtree) > 0L) {
  severe_deterministic_risk <- any(
    shared_emtree$required_concepts >= 2L
  )
}

ensure_directory(paths$search_strategies_directory)

for (attempt in seq_len(max_candidate_attempts)) {

  cat(
    "\nSearch QC proposal ",
    attempt,
    " of ",
    max_candidate_attempts,
    " — original PubMed records: ",
    ifelse(is.na(baseline_count), "UNAVAILABLE", baseline_count),
    "\n",
    sep = ""
  )

  qc <- run_llm_search_qc(
    title = project_title,
    review_question = review_question,
    concepts = search_concepts,
    baseline_query = baseline$pubmed,
    baseline_count = baseline_count,
    shared_mesh = shared_mesh,
    shared_emtree = shared_emtree,
    shared_free_text = shared_free_text,
    ambiguous_acronyms = ambiguous_acronyms,
    prior_feedback = prior_feedback
  )

  last_qc <- qc

  if (!isTRUE(qc$ok)) {
    qc_action <- "FALLBACK"
    qc_rationale <- paste0(
      "LLM search QC unavailable. ",
      qc$error %||% "Unknown provider error."
    )

    qc_history[[attempt]] <- list(
      attempt = attempt,
      proposal_action = qc_action,
      proposal_rationale = qc_rationale,
      candidate_pubmed_count = NA_integer_,
      candidate_review_decision = "NOT_RUN",
      candidate_recall_risk = "UNKNOWN",
      accepted = FALSE
    )

    # No automatic refinement can be trusted if proposal generation fails.
    quality_status <- "REVIEW_REQUIRED"
    break
  }

  parsed <- qc$parsed

  qc_action <- toupper(
    trimws(
      as.character(
        parsed$action %||% "ACCEPT"
      )
    )
  )

  proposal_rationale <- as.character(
    parsed$rationale %||% ""
  )

  plan_validation <- validate_refinement_plan(
    parsed,
    search_concepts
  )

  if (!isTRUE(plan_validation$ok)) {
    qc_rationale <- paste(
      proposal_rationale,
      "Proposal rejected by deterministic guardrail:",
      plan_validation$reason
    )

    qc_history[[attempt]] <- list(
      attempt = attempt,
      proposal_action = "INVALID_PLAN",
      proposal_rationale = qc_rationale,
      candidate_pubmed_count = NA_integer_,
      candidate_review_decision = "NOT_RUN",
      candidate_recall_risk = "UNKNOWN",
      accepted = FALSE
    )

    prior_feedback <- qc_rationale
    next
  }

  if (identical(qc_action, "ACCEPT")) {

    if (isTRUE(severe_deterministic_risk)) {
      qc_rationale <- paste(
        proposal_rationale,
        "Deterministic guardrail rejected ACCEPT because the original strategy still contains an exact controlled-vocabulary term across two or more required AND concepts."
      )

      qc_history[[attempt]] <- list(
        attempt = attempt,
        proposal_action = "ACCEPT_REJECTED_BY_GUARDRAIL",
        proposal_rationale = qc_rationale,
        candidate_pubmed_count = baseline_count,
        candidate_review_decision = "NOT_RUN",
        candidate_recall_risk = "UNKNOWN",
        accepted = FALSE
      )

      prior_feedback <- qc_rationale
      next
    }

    quality_status <- "PASS"
    qc_rationale <- proposal_rationale
    accepted_qc <- qc

    qc_history[[attempt]] <- list(
      attempt = attempt,
      proposal_action = "ACCEPT_ORIGINAL",
      proposal_rationale = proposal_rationale,
      candidate_pubmed_count = baseline_count,
      candidate_review_decision = "ORIGINAL_ACCEPTED",
      candidate_recall_risk = "LOW",
      accepted = TRUE
    )

    break
  }

  if (!identical(qc_action, "REFINE")) {
    qc_rationale <- paste(
      proposal_rationale,
      "Unexpected proposal action:",
      qc_action
    )

    qc_history[[attempt]] <- list(
      attempt = attempt,
      proposal_action = qc_action,
      proposal_rationale = qc_rationale,
      candidate_pubmed_count = NA_integer_,
      candidate_review_decision = "NOT_RUN",
      candidate_recall_risk = "UNKNOWN",
      accepted = FALSE
    )

    prior_feedback <- qc_rationale
    next
  }

  candidate_concepts <- plan_validation$concepts
  candidate_strategy <- build_database_searches(
    candidate_concepts
  )

  candidate_count <- pubmed_count(
    candidate_strategy$pubmed
  )

  candidate_pubmed_path <- file.path(
    paths$search_strategies_directory,
    paste0(
      "pubmed_search_candidate_",
      attempt,
      ".txt"
    )
  )

  write_query_file(
    candidate_strategy$pubmed,
    candidate_pubmed_path
  )

  if (!is.na(candidate_count) && candidate_count == 0L) {
    qc_rationale <- paste(
      proposal_rationale,
      "Candidate returned zero PubMed records and was rejected."
    )

    qc_history[[attempt]] <- list(
      attempt = attempt,
      proposal_action = "REFINE",
      proposal_rationale = proposal_rationale,
      candidate_pubmed_count = candidate_count,
      candidate_review_decision = "REJECT_ZERO_RESULT",
      candidate_recall_risk = "HIGH",
      accepted = FALSE
    )

    prior_feedback <- qc_rationale
    next
  }

  candidate_shared_mesh <- find_shared_terms(
    candidate_concepts,
    "pubmed_mesh"
  )

  candidate_shared_emtree <- find_shared_terms(
    candidate_concepts,
    "embase_emtree"
  )

  candidate_required_overlap <- FALSE

  if (nrow(candidate_shared_mesh) > 0L) {
    candidate_required_overlap <- any(
      candidate_shared_mesh$required_concepts >= 2L
    )
  }

  if (
    !isTRUE(candidate_required_overlap) &&
    nrow(candidate_shared_emtree) > 0L
  ) {
    candidate_required_overlap <- any(
      candidate_shared_emtree$required_concepts >= 2L
    )
  }

  cat(
    "Candidate ",
    attempt,
    " PubMed records: ",
    ifelse(is.na(candidate_count), "UNAVAILABLE", candidate_count),
    "\n",
    sep = ""
  )

  # ----------------------------------------------------------
  # Candidate semantic review
  # ----------------------------------------------------------
  # v0.8.2 v5 folds the retained-term audit into the candidate reviewer.
  # This avoids a third sequential LLM call per candidate and prevents
  # transient provider timeouts from forcing rollback of an otherwise
  # reviewable candidate.
  candidate_review <- run_llm_candidate_review(
    title = project_title,
    review_question = review_question,
    original_concepts = search_concepts,
    original_query = baseline$pubmed,
    original_count = baseline_count,
    candidate_concepts = candidate_concepts,
    candidate_query = candidate_strategy$pubmed,
    candidate_count = candidate_count,
    candidate_required_overlap = candidate_required_overlap
  )

  last_candidate_review <- candidate_review

  if (!isTRUE(candidate_review$ok)) {
    qc_rationale <- paste0(
      "Candidate review unavailable; candidate was not auto-accepted. ",
      candidate_review$error %||% "Unknown provider error."
    )

    qc_history[[attempt]] <- list(
      attempt = attempt,
      proposal_action = "REFINE",
      proposal_rationale = proposal_rationale,
      candidate_pubmed_count = candidate_count,
      candidate_review_decision = "REVIEW_UNAVAILABLE",
      candidate_recall_risk = "UNKNOWN",
      accepted = FALSE
    )

    prior_feedback <- qc_rationale
    next
  }

  review_decision <- toupper(
    trimws(
      as.character(
        candidate_review$parsed$decision %||% ""
      )
    )
  )

  recall_risk <- toupper(
    trimws(
      as.character(
        candidate_review$parsed$recall_risk %||% "HIGH"
      )
    )
  )

  role_discrimination_ok <- isTRUE(
    candidate_review$parsed$role_discrimination_ok
  )

  semantic_cross_block_risk <- toupper(
    trimws(
      as.character(
        candidate_review$parsed$semantic_cross_block_risk %||% "HIGH"
      )
    )
  )

  review_rationale <- as.character(
    candidate_review$parsed$rationale %||% ""
  )

  coverage_assessment <- as.character(
    candidate_review$parsed$coverage_assessment %||% ""
  )

  role_discrimination_assessment <- as.character(
    candidate_review$parsed$role_discrimination_assessment %||% ""
  )

  dropped_term_assessment <- as.character(
    candidate_review$parsed$dropped_term_assessment %||% ""
  )

  accepted <- (
    identical(review_decision, "ACCEPT_CANDIDATE") &&
    !identical(recall_risk, "HIGH") &&
    !identical(semantic_cross_block_risk, "HIGH") &&
    isTRUE(role_discrimination_ok) &&
    !isTRUE(candidate_required_overlap)
  )

  qc_history[[attempt]] <- list(
    attempt = attempt,
    proposal_action = "REFINE",
    proposal_rationale = proposal_rationale,
    candidate_pubmed_count = candidate_count,
    candidate_review_decision = review_decision,
    candidate_recall_risk = recall_risk,
    semantic_term_audit = "INTEGRATED_IN_CANDIDATE_REVIEW",
    role_discrimination_ok = role_discrimination_ok,
    semantic_cross_block_risk = semantic_cross_block_risk,
    coverage_assessment = coverage_assessment,
    role_discrimination_assessment = role_discrimination_assessment,
    dropped_term_assessment = dropped_term_assessment,
    candidate_required_controlled_term_overlap = candidate_required_overlap,
    accepted = accepted
  )

  if (isTRUE(accepted)) {
    final_concepts <- candidate_concepts
    final_strategy <- candidate_strategy
    final_count <- candidate_count
    refinement_applied <- TRUE
    quality_status <- "PASS"
    qc_rationale <- review_rationale
    accepted_qc <- qc
    accepted_candidate_review <- candidate_review
    break
  }

  rejection_reason <- paste(
    "Candidate",
    attempt,
    "was not accepted.",
    "Decision:",
    review_decision,
    "Recall risk:",
    recall_risk,
    "Role discrimination OK:",
    role_discrimination_ok,
    "Semantic cross-block risk:",
    semantic_cross_block_risk,
    if (isTRUE(candidate_required_overlap)) {
      "Deterministic guardrail also detected required-concept controlled-term overlap."
    } else {
      ""
    },
    "Role-discrimination assessment:",
    role_discrimination_assessment,
    "Reviewer rationale:",
    review_rationale
  )

  prior_feedback <- rejection_reason
  qc_rationale <- rejection_reason
}

if (!identical(quality_status, "PASS")) {
  # Roll back to the original canonical strategy rather than silently
  # retaining a rejected over-narrow or still-broad candidate.
  final_concepts <- search_concepts
  final_strategy <- baseline
  final_count <- baseline_count
  refinement_applied <- FALSE

  if (!nzchar(qc_rationale)) {
    qc_rationale <- paste(
      "No automated search candidate was defensibly accepted.",
      "The original canonical strategy was restored and requires review."
    )
  } else {
    qc_rationale <- paste(
      qc_rationale,
      "No automated candidate was defensibly accepted; the original canonical strategy was restored."
    )
  }
}

# Final deterministic re-check.
final_shared_mesh <- find_shared_terms(
  final_concepts,
  "pubmed_mesh"
)

final_shared_emtree <- find_shared_terms(
  final_concepts,
  "embase_emtree"
)

final_required_overlap <- FALSE

if (nrow(final_shared_mesh) > 0L) {
  final_required_overlap <- any(
    final_shared_mesh$required_concepts >= 2L
  )
}

if (!isTRUE(final_required_overlap) && nrow(final_shared_emtree) > 0L) {
  final_required_overlap <- any(
    final_shared_emtree$required_concepts >= 2L
  )
}

if (
  identical(quality_status, "PASS") &&
  isTRUE(final_required_overlap)
) {
  quality_status <- "REVIEW_REQUIRED"
  qc_rationale <- paste(
    qc_rationale,
    "Final deterministic QC detected an exact controlled-vocabulary term across two or more required concepts; PASS was revoked."
  )
}

# Use the accepted proposal for supplementary-search advice only.
qc <- if (!is.null(accepted_qc)) {
  accepted_qc
} else if (!is.null(last_qc)) {
  last_qc
} else {
  list(
    ok = FALSE,
    parsed = NULL,
    response_id = "",
    model = "",
    raw_response = ""
  )
}

candidate_review_decision <- if (!is.null(accepted_candidate_review)) {
  as.character(
    accepted_candidate_review$parsed$decision %||% ""
  )
} else if (!is.null(last_candidate_review) && isTRUE(last_candidate_review$ok)) {
  as.character(
    last_candidate_review$parsed$decision %||% ""
  )
} else {
  ""
}

candidate_recall_risk <- if (!is.null(accepted_candidate_review)) {
  as.character(
    accepted_candidate_review$parsed$recall_risk %||% ""
  )
} else if (!is.null(last_candidate_review) && isTRUE(last_candidate_review$ok)) {
  as.character(
    last_candidate_review$parsed$recall_risk %||% ""
  )
} else {
  ""
}

candidate_role_discrimination_ok <- if (!is.null(accepted_candidate_review)) {
  isTRUE(accepted_candidate_review$parsed$role_discrimination_ok)
} else if (!is.null(last_candidate_review) && isTRUE(last_candidate_review$ok)) {
  isTRUE(last_candidate_review$parsed$role_discrimination_ok)
} else {
  FALSE
}

candidate_semantic_cross_block_risk <- if (!is.null(accepted_candidate_review)) {
  as.character(
    accepted_candidate_review$parsed$semantic_cross_block_risk %||% ""
  )
} else if (!is.null(last_candidate_review) && isTRUE(last_candidate_review$ok)) {
  as.character(
    last_candidate_review$parsed$semantic_cross_block_risk %||% ""
  )
} else {
  ""
}

# ============================================================
# Optional supplementary sensitivity search
# ============================================================

supplementary <- NULL
supplementary_ids <- character(0)
supplementary_rationale <- ""

if (isTRUE(qc$ok)) {
  supplementary_ids <- remove_empty(
    qc$parsed$supplementary_concept_ids
  )

  supplementary_ids <- supplementary_ids[
    supplementary_ids %in% names(final_concepts)
  ]

  if (
    isTRUE(qc$parsed$needs_supplementary_search) &&
    length(unique(supplementary_ids)) >= 2L
  ) {
    supplementary_concepts <- final_concepts[
      unique(supplementary_ids)
    ]

    for (id in names(supplementary_concepts)) {
      supplementary_concepts[[id]]$include_in_primary <- TRUE
    }

    supplementary <- build_database_searches(
      supplementary_concepts
    )

    supplementary_rationale <-
      as.character(qc$parsed$supplementary_rationale %||% "")
  }
}

# ============================================================
# Save final strategies + audit artifacts
# ============================================================

ensure_directory(paths$search_strategies_directory)

baseline_pubmed_path <- file.path(
  paths$search_strategies_directory,
  "pubmed_search_baseline.txt"
)

quality_report_path <- file.path(
  paths$search_strategies_directory,
  "search_quality_report.yml"
)

write_query_file(
  baseline$pubmed,
  baseline_pubmed_path
)

write_query_file(
  final_strategy$pubmed,
  paths$pubmed_search
)

write_query_file(
  final_strategy$embase,
  paths$embase_search
)

write_query_file(
  final_strategy$central,
  paths$central_search
)

supplementary_paths <- list()

if (!is.null(supplementary)) {
  supplementary_paths <- list(
    pubmed = file.path(
      paths$search_strategies_directory,
      "pubmed_search_supplementary.txt"
    ),
    embase = file.path(
      paths$search_strategies_directory,
      "embase_search_supplementary.txt"
    ),
    central = file.path(
      paths$search_strategies_directory,
      "central_search_supplementary.txt"
    )
  )

  write_query_file(
    supplementary$pubmed,
    supplementary_paths$pubmed
  )
  write_query_file(
    supplementary$embase,
    supplementary_paths$embase
  )
  write_query_file(
    supplementary$central,
    supplementary_paths$central
  )
}

final_hash <- unname(
  tools::md5sum(paths$pubmed_search)
)[[1]]

quality_report <- list(
  search_quality = list(
    version = "v0.8.2-search-qc-v5-integrated-semantic-review",
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    project_id = get_project_id(config),
    project_title = project_title,
    review_question = review_question,
    schema_type = normalized_search$schema_type,
    status = quality_status,
    llm_qc_available = isTRUE(qc$ok),
    action = qc_action,
    rationale = qc_rationale,
    refinement_applied = refinement_applied,
    candidate_attempts_completed = length(qc_history),
    candidate_history = qc_history,
    candidate_review_decision = candidate_review_decision,
    candidate_recall_risk = candidate_recall_risk,
    candidate_role_discrimination_ok = candidate_role_discrimination_ok,
    candidate_semantic_cross_block_risk = candidate_semantic_cross_block_risk,
    baseline_pubmed_count = if (is.na(baseline_count)) NA else baseline_count,
    final_pubmed_count = if (is.na(final_count)) NA else final_count,
    final_pubmed_md5 = final_hash,
    severe_cross_block_controlled_term_overlap = severe_deterministic_risk,
    final_required_controlled_term_overlap = final_required_overlap,
    ambiguous_acronym_count = nrow(ambiguous_acronyms),
    shared_pubmed_mesh_count = nrow(shared_mesh),
    shared_embase_emtree_count = nrow(shared_emtree),
    shared_free_text_count = nrow(shared_free_text),
    supplementary_search_generated = !is.null(supplementary),
    supplementary_concept_ids = supplementary_ids,
    supplementary_rationale = supplementary_rationale,
    llm_model = qc$model %||% "",
    llm_response_id = qc$response_id %||% ""
  )
)

yaml::write_yaml(
  quality_report,
  quality_report_path
)

if (
  isTRUE(qc$ok) &&
  nzchar(qc$raw_response %||% "")
) {
  writeLines(
    qc$raw_response,
    file.path(
      paths$search_strategies_directory,
      "search_quality_llm_raw_response.json"
    ),
    useBytes = TRUE
  )
}

# ============================================================
# Save summaries
# ============================================================

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
  concept_count = length(final_strategy$concepts),
  quality_status = quality_status,
  baseline_pubmed_count = baseline_count,
  final_pubmed_count = final_count,
  refinement_applied = refinement_applied,
  generated_at = as.character(Sys.time()),
  stringsAsFactors = FALSE
)

concept_summary <- data.frame(
  concept_id = vapply(
    final_concepts,
    function(x) x$concept_id,
    character(1)
  ),
  concept_label = vapply(
    final_concepts,
    function(x) x$label,
    character(1)
  ),
  concept_role = vapply(
    final_concepts,
    function(x) x$role,
    character(1)
  ),
  required = vapply(
    final_concepts,
    function(x) isTRUE(x$required),
    logical(1)
  ),
  include_in_primary = vapply(
    final_concepts,
    function(x) isTRUE(x$include_in_primary),
    logical(1)
  ),
  free_text_term_count = vapply(
    final_concepts,
    function(x) length(x$free_text_terms),
    integer(1)
  ),
  pubmed_mesh_count = vapply(
    final_concepts,
    function(x) length(x$pubmed_mesh),
    integer(1)
  ),
  embase_emtree_count = vapply(
    final_concepts,
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

# ============================================================
# Console report
# ============================================================

cat("\n")
cat("====================================================\n")
cat("Search Strategy Generation + Intelligent QC\n")
cat("====================================================\n")

cat("\nQuality status: ", quality_status, "\n", sep = "")
cat("Final proposal action: ", qc_action, "\n", sep = "")
cat("Candidate attempts completed: ", length(qc_history), "\n", sep = "")

if (length(qc_history) > 0L) {
  for (item in qc_history) {
    cat(
      "  Candidate ",
      item$attempt,
      ": ",
      ifelse(is.na(item$candidate_pubmed_count), "UNAVAILABLE", item$candidate_pubmed_count),
      " [proposal=",
      item$proposal_action,
      "; term_audit=",
      item$semantic_term_audit %||% "INTEGRATED",
      "; review=",
      item$candidate_review_decision,
      "; recall_risk=",
      item$candidate_recall_risk,
      "; role_ok=",
      item$role_discrimination_ok %||% NA,
      "; semantic_risk=",
      item$semantic_cross_block_risk %||% "NA",
      "; accepted=",
      item$accepted,
      "]\n",
      sep = ""
    )
  }
}

cat(
  "PubMed count: ",
  ifelse(is.na(baseline_count), "UNAVAILABLE", baseline_count),
  " -> ",
  ifelse(is.na(final_count), "UNAVAILABLE", final_count),
  "\n",
  sep = ""
)

if (nzchar(qc_rationale)) {
  cat("QC rationale:\n", qc_rationale, "\n", sep = "")
}

cat("\nPUBMED SEARCH\n")
cat("----------------------------------------------------\n")
cat(final_strategy$pubmed, "\n")

cat("\nEMBASE SEARCH\n")
cat("----------------------------------------------------\n")
cat(final_strategy$embase, "\n")

cat("\nCENTRAL SEARCH\n")
cat("----------------------------------------------------\n")
cat(final_strategy$central, "\n")

if (!is.null(supplementary)) {
  cat("\nSupplementary sensitivity search: GENERATED\n")
  cat(
    "Concepts: ",
    paste(supplementary_ids, collapse = ", "),
    "\n",
    sep = ""
  )
}

cat("\nGenerated files:\n")
cat("- ", paths$pubmed_search, "\n", sep = "")
cat("- ", paths$embase_search, "\n", sep = "")
cat("- ", paths$central_search, "\n", sep = "")
cat("- ", paths$search_strategy_summary, "\n", sep = "")
cat("- ", quality_report_path, "\n", sep = "")

if (identical(quality_status, "REVIEW_REQUIRED")) {
  cat(
    "\nSEARCH QUALITY REVIEW REQUIRED:\n",
    "The strategy files were generated for audit, but R/16 will not download ",
    "PubMed records until R/11 is rerun successfully or the underlying ",
    "canonical search terms are corrected.\n",
    sep = ""
  )
} else {
  cat(
    "\nPASS: Search strategies generated with pre-download semantic/volume QC.\n"
  )
}
