#!/usr/bin/env Rscript

cat("PROSPERO MODULE VERSION: 2026-08-31-UNIVERSAL-LLM-DRAFTING-v2\n")

suppressPackageStartupMessages({
  library(openxlsx)
  library(yaml)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

# ============================================================
# 1. Repository / interaction helpers
# ============================================================

find_repo_root <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = TRUE)

  repeat {
    markers_ok <- all(file.exists(c(
      file.path(current, "R", "run_universal_srma_workflow.R"),
      file.path(current, "R", "utils", "config_utils.R"),
      file.path(current, "R", "utils", "path_utils.R"),
      file.path(current, "R", "utils", "llm_provider_utils.R")
    )))

    directories_ok <- dir.exists(file.path(current, "config")) &&
      dir.exists(file.path(current, "templates"))

    if (markers_ok && directories_ok) return(current)

    parent <- dirname(current)

    if (identical(parent, current)) {
      stop(
        "Could not find the Universal SR-MA Toolkit root.\n",
        "Run this module from inside the repository.",
        call. = FALSE
      )
    }

    current <- parent
  }
}

ask_yes_no <- function(prompt) {
  if (!interactive()) {
    stop(
      "PROSPERO author review/finalization requires an interactive R/RStudio session ",
      "when prospero_final.xlsx has not yet been created.",
      call. = FALSE
    )
  }

  prompt <- trimws(prompt)
  prompt <- sub("\\s*\\[Yes/No\\]:?\\s*$", "", prompt, ignore.case = TRUE)

  if (nzchar(prompt)) {
    cat("\n", prompt, "\n", sep = "")
  }

  repeat {
    ans <- trimws(tolower(readline("Response [Yes/No]: ")))

    if (ans %in% c("y", "yes", "예", "네")) return(TRUE)
    if (ans %in% c("n", "no", "아니오", "아니요")) return(FALSE)

    cat("Please enter Yes or No.\n")
  }
}

as_flag <- function(x, default = FALSE) {
  if (is.logical(x) && length(x) == 1L && !is.na(x)) return(x)

  value <- tolower(trimws(as.character(x %||% "")))

  if (value %in% c("1", "true", "t", "yes", "y", "on")) return(TRUE)
  if (value %in% c("0", "false", "f", "no", "n", "off")) return(FALSE)

  default
}

# ============================================================
# 2. Text / config helpers
# ============================================================

normalize_utf8 <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")

  x <- paste(as.character(x), collapse = "\n")
  x <- iconv(x, from = "", to = "UTF-8", sub = " ")
  if (is.na(x)) x <- ""

  # Keep TAB/LF/CR, remove other C0 controls and DEL.
  x <- gsub(
    "[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F]",
    " ",
    x,
    perl = TRUE
  )

  x
}

sanitize_prospero_text <- function(x, preserve_newlines = TRUE) {
  x <- normalize_utf8(x)
  if (!nzchar(x)) return("")

  x <- gsub("\r\n?", "\n", x, perl = TRUE)

  if (!isTRUE(preserve_newlines)) {
    x <- gsub("[\t\n]+", " ", x, perl = TRUE)
    x <- gsub(" {2,}", " ", x, perl = TRUE)
    return(trimws(x))
  }

  lines <- unlist(strsplit(x, "\n", fixed = TRUE))
  lines <- gsub("[\t ]+", " ", lines, perl = TRUE)
  lines <- trimws(lines)

  # Keep paragraph breaks but collapse long blank runs.
  out <- paste(lines, collapse = "\n")
  out <- gsub("\n{3,}", "\n\n", out, perl = TRUE)
  trimws(out)
}

is_blank <- function(x) {
  !nzchar(sanitize_prospero_text(x, preserve_newlines = FALSE))
}

value_to_text <- function(x) {
  if (is.null(x) || length(x) == 0L) return("")

  if (is.atomic(x)) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) return("")
    return(
      sanitize_prospero_text(
        paste(as.character(x), collapse = "; "),
        preserve_newlines = TRUE
      )
    )
  }

  if (is.list(x)) {
    if (!is.null(names(x)) && any(nzchar(names(x)))) {
      pieces <- character(0)

      for (nm in names(x)) {
        value <- value_to_text(x[[nm]])
        if (nzchar(value)) {
          pieces <- c(pieces, paste0(nm, ": ", value))
        }
      }

      return(
        sanitize_prospero_text(
          paste(pieces, collapse = "; "),
          preserve_newlines = TRUE
        )
      )
    }

    flat <- unlist(x, recursive = TRUE, use.names = FALSE)
    flat <- flat[!is.na(flat)]

    return(
      sanitize_prospero_text(
        paste(as.character(flat), collapse = "; "),
        preserve_newlines = TRUE
      )
    )
  }

  sanitize_prospero_text(as.character(x), preserve_newlines = TRUE)
}

get_nested_value <- function(x, dotted_path) {
  parts <- strsplit(dotted_path, "\\.")[[1]]
  cur <- x

  for (p in parts) {
    if (!is.list(cur) || is.null(cur[[p]])) return(NULL)
    cur <- cur[[p]]
  }

  cur
}

pick_config_with_source <- function(cfg, paths) {
  for (p in paths) {
    value <- get_nested_value(cfg, p)

    if (!is_blank(value)) {
      return(list(
        value = value,
        source = paste0("config/project_config.yml:", p)
      ))
    }
  }

  list(value = NULL, source = "")
}

first_nonblank <- function(...) {
  values <- list(...)

  for (value in values) {
    if (!is_blank(value)) return(value)
  }

  NULL
}

read_text_safely <- function(path, max_chars = 40000L) {
  lines <- tryCatch(
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8",
      skipNul = TRUE
    ),
    error = function(e) character(0)
  )

  if (length(lines) == 0L) return("")

  lines <- iconv(lines, from = "UTF-8", to = "UTF-8", sub = " ")
  lines[is.na(lines)] <- ""

  txt <- sanitize_prospero_text(
    paste(lines, collapse = "\n"),
    preserve_newlines = TRUE
  )

  n <- nchar(txt, type = "chars", allowNA = TRUE, keepNA = FALSE)
  if (!is.finite(n)) n <- 0L

  if (n > max_chars) txt <- substr(txt, 1L, max_chars)

  txt
}

collect_active_project_context <- function(repo_root, project_root) {
  # Explicit active-project / canonical inputs only.
  # No repo-wide newest-file search and no foreign-project discovery.
  explicit_files <- c(
    file.path(repo_root, "config", "project_config.yml"),
    file.path(repo_root, "config", "search_terms.yml"),
    file.path(repo_root, "config", "generated", "human_decisions_resolved.yml"),
    file.path(repo_root, "config", "generated", "human_decision_responses.yml"),
    file.path(repo_root, "config", "generated", "human_decision_verification.yml"),
    file.path(project_root, "README.md"),
    file.path(project_root, "PROTOCOL.md"),
    file.path(project_root, "protocol.md"),
    file.path(project_root, "config", "project_seed.yml")
  )

  protocol_dirs <- c(
    file.path(project_root, "protocol"),
    file.path(project_root, "plans")
  )

  protocol_files <- character(0)

  for (directory in protocol_dirs[dir.exists(protocol_dirs)]) {
    protocol_files <- c(
      protocol_files,
      list.files(
        directory,
        pattern = "\\.(md|txt|yml|yaml)$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
    )
  }

  files <- unique(c(
    explicit_files[file.exists(explicit_files)],
    protocol_files
  ))

  if (length(files) == 0L) return("")

  blocks <- vapply(
    files,
    function(path) {
      txt <- read_text_safely(path)
      if (!nzchar(txt)) return("")

      rel <- tryCatch(
        substring(
          normalizePath(path, winslash = "/", mustWork = TRUE),
          nchar(normalizePath(repo_root, winslash = "/", mustWork = TRUE)) + 2L
        ),
        error = function(e) basename(path)
      )

      paste0("### FILE: ", rel, "\n", txt)
    },
    character(1)
  )

  combined <- paste(blocks[nzchar(blocks)], collapse = "\n\n")

  if (nchar(combined) > 120000L) {
    combined <- substr(combined, 1L, 120000L)
  }

  combined
}

extract_after_label <- function(text, labels, max_chars = 4000L) {
  text <- normalize_utf8(text)
  if (!nzchar(text)) return("")

  lines <- unlist(strsplit(text, "\n", fixed = TRUE))
  low <- tolower(lines)

  for (label in labels) {
    label <- sanitize_prospero_text(label, preserve_newlines = FALSE)
    idx <- grep(tolower(label), low, fixed = TRUE)

    if (length(idx) == 0L) next

    i <- idx[[1]]
    candidate <- sub(
      paste0("^.*", label, "\\s*[:=-]?\\s*"),
      "",
      lines[[i]],
      ignore.case = TRUE
    )
    candidate <- trimws(candidate)

    if (nzchar(candidate)) {
      return(substr(candidate, 1L, max_chars))
    }
  }

  ""
}

# ============================================================
# 3. Deterministic protocol drafting + provenance
# ============================================================

new_draft_state <- function() {
  list(
    info = list(),
    provenance = list()
  )
}

set_draft_field <- function(
    state,
    key,
    value,
    status,
    source,
    note = ""
) {
  value <- sanitize_prospero_text(value, preserve_newlines = TRUE)

  state$info[[key]] <- value
  state$provenance[[key]] <- list(
    status = status,
    source = source,
    note = sanitize_prospero_text(note, preserve_newlines = FALSE)
  )

  state
}

set_from_config_or_context <- function(
    state,
    cfg,
    project_text,
    key,
    config_paths,
    labels = character(0),
    fallback = NULL,
    fallback_status = "AUTO_POLICY",
    fallback_source = "universal_policy"
) {
  hit <- pick_config_with_source(cfg, config_paths)

  if (!is_blank(hit$value)) {
    return(
      set_draft_field(
        state,
        key,
        value_to_text(hit$value),
        "AUTO_INFERRED",
        hit$source,
        "Directly derived from the canonical active project configuration."
      )
    )
  }

  if (length(labels) > 0L) {
    inferred <- extract_after_label(project_text, labels)

    if (nzchar(inferred)) {
      return(
        set_draft_field(
          state,
          key,
          inferred,
          "AUTO_INFERRED",
          "bounded_active_project_context",
          "Derived from explicitly bounded active-project protocol/planning text."
        )
      )
    }
  }

  if (!is_blank(fallback)) {
    return(
      set_draft_field(
        state,
        key,
        fallback,
        fallback_status,
        fallback_source,
        "Universal methodological policy/default; project-level explicit values take precedence."
      )
    )
  }

  set_draft_field(
    state,
    key,
    "",
    "USER_REQUIRED",
    "not_defensibly_inferable",
    "No defensible value was found in canonical configuration or bounded active-project context."
  )
}

build_deterministic_draft <- function(cfg, project_text) {
  state <- new_draft_state()

  state <- set_from_config_or_context(
    state, cfg, project_text, "title",
    c("review.title", "project.title", "title"),
    c("Review title", "Title")
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "review_type",
    c("review.type", "review_type"),
    c("Review type"),
    "Systematic review with meta-analysis where quantitative synthesis is appropriate."
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "domain",
    c("review.domain", "condition", "domain", "condition_or_domain"),
    c("Condition or domain", "Clinical domain", "Domain")
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "rationale",
    c("review.rationale", "protocol.rationale", "rationale"),
    c("Rationale for the review", "Rationale", "Background")
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "objectives",
    c(
      "research_question.question",
      "review.objectives",
      "review.question",
      "research_question",
      "objective",
      "objectives"
    ),
    c("Review objectives", "Objective", "Research question")
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "keywords",
    c("review.keywords", "keywords", "search.keywords"),
    c("Keywords")
  )

  # Identity / administrative facts: structured config only.
  country_hit <- pick_config_with_source(cfg, c("project.country", "country"))

  if (!is_blank(country_hit$value)) {
    state <- set_draft_field(
      state, "country", value_to_text(country_hit$value),
      "AUTO_INFERRED", country_hit$source
    )
  } else {
    state <- set_draft_field(
      state, "country", "", "USER_REQUIRED", "structured_config_required"
    )
  }

  state <- set_from_config_or_context(
    state, cfg, project_text, "population",
    c(
      "research_question.population",
      "research_question.pico.population",
      "pico.population",
      "eligibility.population",
      "population"
    ),
    c("Population")
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "intervention",
    c(
      "research_question.intervention",
      "research_question.exposure",
      "research_question.pico.intervention",
      "research_question.pico.exposure",
      "pico.intervention",
      "pico.exposure",
      "eligibility.intervention",
      "intervention",
      "exposure"
    ),
    c("Intervention", "Exposure")
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "comparator",
    c(
      "research_question.comparator",
      "research_question.control",
      "research_question.pico.comparator",
      "pico.comparator",
      "eligibility.comparator",
      "comparator",
      "control"
    ),
    c("Comparator", "Control")
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "study_design",
    c(
      "eligibility.study_designs",
      "eligibility.study_design",
      "study_designs",
      "study_design"
    ),
    c("Study design")
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "context",
    c("eligibility.context", "research_question.context", "context"),
    c("Context")
  )

  start_hit <- pick_config_with_source(
    cfg,
    c("project.start_date", "review.start_date", "start_date")
  )
  end_hit <- pick_config_with_source(
    cfg,
    c(
      "project.end_date",
      "review.end_date",
      "anticipated_completion_date"
    )
  )

  timeline_parts <- c()

  if (!is_blank(start_hit$value)) {
    timeline_parts <- c(
      timeline_parts,
      paste0("Review start: ", value_to_text(start_hit$value))
    )
  }

  if (!is_blank(end_hit$value)) {
    timeline_parts <- c(
      timeline_parts,
      paste0("Anticipated completion: ", value_to_text(end_hit$value))
    )
  }

  if (length(timeline_parts) > 0L) {
    state <- set_draft_field(
      state,
      "timeline",
      paste(timeline_parts, collapse = "; "),
      "AUTO_INFERRED",
      paste(
        c(start_hit$source, end_hit$source)[
          nzchar(c(start_hit$source, end_hit$source))
        ],
        collapse = "; "
      )
    )
  } else {
    state <- set_draft_field(
      state,
      "timeline",
      "",
      "USER_REQUIRED",
      "not_defensibly_inferable"
    )
  }

  state <- set_from_config_or_context(
    state, cfg, project_text, "protocol_availability",
    c("protocol.availability", "protocol.url", "protocol.doi"),
    c("Protocol availability"),
    paste(
      "A full protocol is maintained within the active project records.",
      "A public link or DOI will be supplied if one is available."
    )
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "unpublished_search",
    c("search.unpublished_studies", "search.grey_literature"),
    c("Unpublished studies", "Grey literature"),
    paste(
      "Unpublished or ongoing studies will be considered when identified",
      "through prespecified information sources or supplementary searching."
    )
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "databases",
    c("search.databases", "databases", "search.sources"),
    c("Databases", "Main sources", "Information sources")
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "language_restrictions",
    c(
      "search.language_restrictions",
      "eligibility.language_restrictions"
    ),
    c("Language restrictions"),
    "No language restrictions are planned unless explicitly specified in the canonical project configuration."
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "date_restrictions",
    c(
      "search.date_restrictions",
      "search.start_date",
      "eligibility.date_restrictions"
    ),
    c("Date restrictions", "Search start date"),
    "No publication-date restriction is planned unless explicitly specified in the canonical project configuration."
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "other_identification",
    c("search.other_methods", "search.supplementary_methods"),
    c("Other methods of identifying studies"),
    paste(
      "Reference lists of included studies and relevant reviews will be checked.",
      "Additional targeted searching may be used when needed to identify eligible reports."
    )
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "selection_process",
    c("screening.method", "selection_process"),
    c("Selection process"),
    paste(
      "Records will be deduplicated and screened against prespecified eligibility criteria.",
      "The toolkit may assist screening, but human reviewer verification/adjudication remains authoritative.",
      "Full-text exclusion reasons will be documented."
    )
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "search_other",
    c("search.other_information"),
    c("Other relevant information about searching and screening"),
    paste(
      "Database-specific strategies, validation outputs, deduplication records,",
      "and screening decisions will be retained in the project audit trail."
    )
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "extraction",
    c("extraction.method", "data_extraction.method"),
    c("Data extraction"),
    paste(
      "Data will be extracted using prespecified structured fields.",
      "Agent-assisted extraction may be used, with human verification of analysis-ready values before quantitative synthesis."
    )
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "risk_of_bias",
    c("risk_of_bias.method", "risk_of_bias", "rob.method"),
    c("Risk of bias"),
    paste(
      "Risk of bias will be assessed with design-appropriate tools.",
      "RoB 2 will be used for randomized trials and ROBINS-I for non-randomized intervention studies when applicable.",
      "Author review remains authoritative."
    )
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "reporting_bias",
    c("analysis.reporting_bias", "reporting_bias"),
    c("Reporting bias"),
    paste(
      "Reporting/publication bias will be assessed when the number and structure of included studies permit meaningful evaluation.",
      "Funnel-plot or small-study-effect methods will not be overinterpreted when evidence is sparse."
    )
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "certainty",
    c("analysis.certainty_assessment", "certainty_assessment"),
    c("Certainty assessment"),
    "Certainty of evidence will be evaluated using GRADE or another design-appropriate framework when applicable."
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "primary_outcomes",
    c(
      "research_question.outcomes.primary",
      "outcomes.primary",
      "primary_outcomes",
      "outcome.primary"
    ),
    c("Primary outcome", "Main outcomes")
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "secondary_outcomes",
    c(
      "research_question.outcomes.secondary",
      "outcomes.secondary",
      "secondary_outcomes",
      "outcome.secondary"
    ),
    c("Secondary outcome", "Additional outcomes")
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "synthesis",
    c(
      "analysis.synthesis",
      "analysis.plan",
      "meta_analysis.plan"
    ),
    c("Data synthesis", "Meta-analysis"),
    paste(
      "Where studies are sufficiently comparable, quantitative synthesis will use outcome-appropriate effect measures.",
      "The default quantitative policy is a random-effects model, with between-study variance estimated using REML and Hartung-Knapp adjustment where supported and appropriate.",
      "Heterogeneity will be characterized using tau-squared, I-squared, and the Q test.",
      "Prespecified subgroup and sensitivity analyses will be performed only when supported by the evidence structure.",
      "Otherwise results will be synthesized narratively."
    )
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "review_stage",
    c("registration.review_stage", "review.stage"),
    c("Review stage"),
    "Protocol development / review preparation before formal database searching."
  )

  state <- set_from_config_or_context(
    state, cfg, project_text, "publication_status",
    c("registration.publication_status"),
    c("Publication status"),
    "Review results have not yet been published."
  )

  for (key in c("team", "affiliation")) {
    paths <- if (key == "team") {
      c("team", "review.team", "authors")
    } else {
      c("affiliation", "review.affiliation", "project.affiliation")
    }

    hit <- pick_config_with_source(cfg, paths)

    if (!is_blank(hit$value)) {
      state <- set_draft_field(
        state, key, value_to_text(hit$value),
        "AUTO_INFERRED", hit$source
      )
    } else {
      state <- set_draft_field(
        state, key, "", "USER_REQUIRED", "structured_config_required",
        "Identity fields are never invented from free text."
      )
    }
  }

  funding_hit <- pick_config_with_source(
    cfg,
    c("funding", "funding.source", "project.funding")
  )

  funding_value <- if (!is_blank(funding_hit$value)) {
    value_to_text(funding_hit$value)
  } else {
    extract_after_label(project_text, c("Funding", "Funding source"))
  }

  if (nzchar(funding_value)) {
    state <- set_draft_field(
      state,
      "funding",
      funding_value,
      "AUTO_INFERRED",
      if (nzchar(funding_hit$source)) funding_hit$source else "bounded_active_project_context"
    )
  } else {
    state <- set_draft_field(
      state,
      "funding",
      "",
      "USER_REQUIRED",
      "not_defensibly_inferable",
      "Funding status is not fabricated."
    )
  }

  peer_hit <- pick_config_with_source(
    cfg,
    c("peer_review", "review.peer_review")
  )

  if (!is_blank(peer_hit$value)) {
    state <- set_draft_field(
      state, "peer_review", value_to_text(peer_hit$value),
      "AUTO_INFERRED", peer_hit$source
    )
  } else {
    state <- set_draft_field(
      state, "peer_review", "", "USER_REQUIRED", "structured_config_required",
      "Peer-review status is an administrative fact and is not fabricated."
    )
  }

  state <- set_from_config_or_context(
    state, cfg, project_text, "additional_information",
    c(
      "registration.additional_information",
      "additional_information"
    ),
    c("Additional information"),
    "No additional protocol information has been specified at this stage."
  )

  conflict_hit <- pick_config_with_source(
    cfg,
    c("conflicts", "conflict_of_interest", "review.conflicts")
  )

  conflict_value <- if (!is_blank(conflict_hit$value)) {
    value_to_text(conflict_hit$value)
  } else {
    extract_after_label(
      project_text,
      c("Conflict of interest", "Conflicts of interest")
    )
  }

  if (nzchar(conflict_value)) {
    state <- set_draft_field(
      state,
      "conflicts",
      conflict_value,
      "AUTO_INFERRED",
      if (nzchar(conflict_hit$source)) conflict_hit$source else "bounded_active_project_context"
    )
  } else {
    state <- set_draft_field(
      state,
      "conflicts",
      "",
      "USER_REQUIRED",
      "not_defensibly_inferable",
      "Conflict-of-interest status is not fabricated."
    )
  }

  state <- set_from_config_or_context(
    state, cfg, project_text, "mesh_terms",
    c("search.mesh_terms", "search.mesh", "mesh_terms"),
    c("Medical Subject Headings", "MeSH")
  )

  # Registration-critical fields that require external/user action.
  state <- set_draft_field(
    state,
    "similar_reviews",
    paste(
      "AUTHOR CHECK REQUIRED:",
      "Search PROSPERO for similar ongoing or completed records before submission",
      "and summarize overlap/novelty when relevant."
    ),
    "USER_REQUIRED",
    "external_registration_check_required"
  )

  state <- set_draft_field(
    state,
    "search_strategy_link",
    paste(
      "AUTHOR CHECK REQUIRED:",
      "Add a final public URL/DOI for the search strategy if PROSPERO requires one."
    ),
    "USER_REQUIRED",
    "public_link_required"
  )

  state <- set_draft_field(
    state,
    "first_submission_date",
    format(Sys.Date(), "%Y-%m-%d"),
    "SYSTEM_OUTPUT",
    "system_date"
  )

  state <- set_draft_field(
    state,
    "disclaimer",
    paste(
      "This workbook is generated by the Universal SR-MA Toolkit as a protocol-drafting aid.",
      "The review authors remain responsible for verifying all registration content and for the final PROSPERO submission."
    ),
    "SYSTEM_OUTPUT",
    "toolkit_system_output"
  )

  state
}

# ============================================================
# 4. LLM-assisted protocol drafting
# ============================================================

llm_draftable_keys <- function() {
  c(
    "domain",
    "rationale",
    "objectives",
    "keywords",
    "population",
    "intervention",
    "comparator",
    "study_design",
    "context",
    "protocol_availability",
    "unpublished_search",
    "databases",
    "language_restrictions",
    "date_restrictions",
    "other_identification",
    "selection_process",
    "search_other",
    "extraction",
    "risk_of_bias",
    "reporting_bias",
    "certainty",
    "primary_outcomes",
    "secondary_outcomes",
    "synthesis",
    "review_stage",
    "publication_status",
    "additional_information",
    "mesh_terms"
  )
}

build_prospero_llm_schema <- function() {
  keys <- llm_draftable_keys()

  draft_properties <- setNames(
    lapply(
      keys,
      function(key) list(type = "string")
    ),
    keys
  )

  list(
    type = "object",
    additionalProperties = FALSE,
    required = c("draft", "drafting_notes"),
    properties = list(
      draft = list(
        type = "object",
        additionalProperties = FALSE,
        required = keys,
        properties = draft_properties
      ),
      drafting_notes = list(type = "string")
    )
  )
}

build_prospero_llm_prompt <- function(
    cfg,
    search_terms,
    project_context,
    baseline_info
) {
  baseline_subset <- baseline_info[llm_draftable_keys()]

  cfg_text <- tryCatch(
    yaml::as.yaml(cfg),
    error = function(e) value_to_text(cfg)
  )

  search_text <- tryCatch(
    yaml::as.yaml(search_terms),
    error = function(e) value_to_text(search_terms)
  )

  baseline_text <- tryCatch(
    yaml::as.yaml(baseline_subset),
    error = function(e) value_to_text(baseline_subset)
  )

  policy_text <- paste(
    "UNIVERSAL METHODOLOGICAL POLICY:",
    "- Treat canonical project configuration as authoritative and never contradict explicit project-level choices.",
    "- Prefer project-specific inference when defensible; otherwise use conditional methodological wording rather than inventing facts.",
    "- For quantitative synthesis, use outcome-appropriate effect measures and a random-effects framework by default; REML and Hartung-Knapp are the toolkit defaults when supported/appropriate.",
    "- Characterize heterogeneity with tau-squared, I-squared, and Q when meta-analysis is performed.",
    "- Use RoB 2 for randomized trials and ROBINS-I for non-randomized intervention studies when applicable; otherwise use a design-appropriate risk-of-bias tool.",
    "- Assess reporting/publication bias only when the evidence structure supports meaningful assessment.",
    "- Use GRADE or another appropriate certainty framework when applicable.",
    "- Screening/extraction may be agent-assisted, but human verification/adjudication remains authoritative.",
    sep = "\n"
  )

  instructions <- paste(
    "You are the Universal SR-MA Toolkit PROSPERO protocol drafting engine.",
    "Create concise, registration-ready protocol prose from ONLY the supplied canonical/bounded context.",
    "",
    "NON-NEGOTIABLE RULES:",
    "1. Do not invent authors, affiliation, country, funding, conflicts, registration IDs, URLs, study results, dates, or external facts.",
    "2. Do not perform or claim an external PROSPERO/web search.",
    "3. Do not change explicit eligibility, comparator, date/language restrictions, outcomes, or analysis decisions in the canonical configuration.",
    "4. If a requested field cannot be defensibly inferred, return an empty string for that field.",
    "5. Improve wording and specificity, but do not introduce unsupported thresholds or clinical definitions.",
    "6. For primary and secondary outcomes, where the project context allows, write each outcome with: outcome name; definition/measurement concept; timing/follow-up principle; effect measure principle; harmonization rule across study definitions. If details are unavailable, use cautious protocol-level wording rather than fabricating them.",
    "7. For Population / Intervention / Comparator / Study design / Context, produce explicit eligibility-ready prose, not just isolated keywords.",
    "8. Keep each field concise enough for a registration form.",
    "9. Return structured JSON matching the supplied schema only.",
    sep = "\n"
  )

  paste(
    instructions,
    "",
    policy_text,
    "",
    "BASELINE DETERMINISTIC DRAFT:",
    baseline_text,
    "",
    "CANONICAL PROJECT CONFIGURATION:",
    cfg_text,
    "",
    "CANONICAL SEARCH TERMS:",
    search_text,
    "",
    "BOUNDED ACTIVE-PROJECT CONTEXT:",
    project_context,
    sep = "\n"
  )
}

load_prospero_provider_config <- function(repo_root) {
  agent_file <- file.path(repo_root, "config", "initialization_agent.yml")

  if (!file.exists(agent_file)) {
    stop("Missing initialization-agent configuration: ", agent_file, call. = FALSE)
  }

  document <- yaml::read_yaml(agent_file)
  agent_config <- document$initialization_agent

  if (is.null(agent_config) || !is.list(agent_config)) {
    stop(
      "Invalid config/initialization_agent.yml: missing initialization_agent.",
      call. = FALSE
    )
  }

  provider_config <- agent_config$provider %||% list()
  provider_config$timeout_seconds <-
    agent_config$processing$timeout_seconds %||% 180

  list(
    provider_name = provider_config$name %||% "openai",
    provider_config = provider_config
  )
}

run_llm_protocol_drafting <- function(
    repo_root,
    cfg,
    search_terms,
    project_context,
    baseline_info,
    out_dir
) {
  disable_llm <- identical(
    getOption("srma.prospero.use_llm"),
    FALSE
  ) || as_flag(
    Sys.getenv("SRMA_PROSPERO_DISABLE_LLM", unset = ""),
    FALSE
  )

  if (isTRUE(disable_llm)) {
    return(list(
      ok = FALSE,
      reason = "LLM drafting disabled by option/environment.",
      draft = list(),
      response = NULL
    ))
  }

  result <- tryCatch(
    {
      source(file.path(repo_root, "R", "utils", "llm_provider_utils.R"))

      env_file <- file.path(repo_root, ".env")

      if (file.exists(env_file)) {
        load_project_env(env_file)
      }

      provider <- load_prospero_provider_config(repo_root)

      schema <- build_prospero_llm_schema()
      prompt <- build_prospero_llm_prompt(
        cfg = cfg,
        search_terms = search_terms,
        project_context = project_context,
        baseline_info = baseline_info
      )

      response <- call_llm_screening_provider(
        provider_name = provider$provider_name,
        prompt = prompt,
        provider_config = provider$provider_config,
        json_schema = schema,
        schema_name = "prospero_protocol_draft_v2"
      )

      parsed <- response$parsed

      if (
        is.null(parsed$draft) ||
        !is.list(parsed$draft)
      ) {
        stop(
          "LLM response did not contain the expected draft object.",
          call. = FALSE
        )
      }

      # Preserve an auditable, non-secret record of the drafting call.
      if (requireNamespace("jsonlite", quietly = TRUE)) {
        jsonlite::write_json(
          parsed,
          file.path(out_dir, "prospero_llm_model_output.json"),
          pretty = TRUE,
          auto_unbox = TRUE,
          null = "null"
        )

        writeLines(
          response$raw_response %||% "",
          file.path(out_dir, "prospero_llm_api_raw_response.json"),
          useBytes = TRUE
        )
      }

      metadata <- list(
        prospero_llm_drafting = list(
          generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          provider = provider$provider_name,
          model = response$model %||% provider$provider_config$model %||% "",
          response_id = response$response_id %||% "",
          schema = "prospero_protocol_draft_v2",
          drafting_notes = parsed$drafting_notes %||% ""
        )
      )

      yaml::write_yaml(
        metadata,
        file.path(out_dir, "prospero_llm_drafting_metadata.yml")
      )

      list(
        ok = TRUE,
        reason = "",
        draft = parsed$draft,
        response = response
      )
    },
    error = function(e) {
      list(
        ok = FALSE,
        reason = conditionMessage(e),
        draft = list(),
        response = NULL
      )
    }
  )

  result
}

merge_llm_draft <- function(state, llm_result) {
  if (!isTRUE(llm_result$ok)) return(state)

  for (key in llm_draftable_keys()) {
    llm_value <- sanitize_prospero_text(
      llm_result$draft[[key]] %||% "",
      preserve_newlines = TRUE
    )

    if (!nzchar(llm_value)) next

    previous <- state$provenance[[key]] %||% list()
    previous_status <- previous$status %||% "AUTO_INFERRED"

    status <- if (identical(previous_status, "AUTO_POLICY")) {
      "AUTO_POLICY"
    } else {
      "AUTO_INFERRED"
    }

    state <- set_draft_field(
      state,
      key,
      llm_value,
      status,
      "llm_synthesis_from_canonical_context",
      "LLM-polished/inferred protocol prose constrained by canonical config, search terms, bounded project context, and universal policy."
    )
  }

  state
}

# ============================================================
# 5. PROSPERO workbook schema
# ============================================================

prospero_sections <- function() {
  list(
    "REVIEW TITLE AND BASIC DETAILS" = list(
      c("PR001", "Review title", "title", "AUTO_INFERRED"),
      c("PR002", "Review type", "review_type", "AUTO_POLICY"),
      c("PR003", "Condition or domain being studied", "domain", "AUTO_INFERRED"),
      c("PR004", "Rationale for the review", "rationale", "AUTO_INFERRED"),
      c("PR005", "Review objectives", "objectives", "AUTO_INFERRED"),
      c("PR006", "Keywords", "keywords", "AUTO_INFERRED"),
      c("PR007", "Country", "country", "USER_REQUIRED")
    ),
    "ELIGIBILITY CRITERIA" = list(
      c("PR008", "Population", "population", "AUTO_INFERRED"),
      c("PR009", "Intervention(s) or exposure(s)", "intervention", "AUTO_INFERRED"),
      c("PR010", "Comparator(s) or control(s)", "comparator", "AUTO_INFERRED"),
      c("PR011", "Study design", "study_design", "AUTO_INFERRED"),
      c("PR012", "Context", "context", "AUTO_INFERRED")
    ),
    "SIMILAR REVIEWS" = list(
      c("PR013", "Check for similar records already in PROSPERO", "similar_reviews", "USER_REQUIRED")
    ),
    "TIMELINE OF THE REVIEW" = list(
      c("PR014", "Date of first submission to PROSPERO", "first_submission_date", "SYSTEM_OUTPUT"),
      c("PR015", "Review timeline", "timeline", "AUTO_INFERRED")
    ),
    "AVAILABILITY OF FULL PROTOCOL" = list(
      c("PR017", "Availability of full protocol", "protocol_availability", "AUTO_POLICY")
    ),
    "SEARCHING AND SCREENING" = list(
      c("PR018", "Search for unpublished studies", "unpublished_search", "AUTO_POLICY"),
      c("PR019", "Main sources that will be searched", "databases", "AUTO_INFERRED"),
      c("PR020", "Search language restrictions", "language_restrictions", "AUTO_POLICY"),
      c("PR021", "Search date restrictions", "date_restrictions", "AUTO_POLICY"),
      c("PR022", "Other methods of identifying studies", "other_identification", "AUTO_POLICY"),
      c("PR023", "Link to search strategy", "search_strategy_link", "USER_REQUIRED"),
      c("PR024", "Selection process", "selection_process", "AUTO_POLICY"),
      c("PR025", "Other relevant information about searching and screening", "search_other", "AUTO_POLICY")
    ),
    "DATA COLLECTION PROCESS" = list(
      c("PR026", "Data extraction from published articles and reports", "extraction", "AUTO_POLICY"),
      c("PR027", "Study risk of bias or quality assessment", "risk_of_bias", "AUTO_POLICY"),
      c("PR028", "Reporting bias assessment", "reporting_bias", "AUTO_POLICY"),
      c("PR029", "Certainty assessment", "certainty", "AUTO_POLICY")
    ),
    "OUTCOMES TO BE ANALYSED" = list(
      c("PR030", "Main outcomes", "primary_outcomes", "AUTO_INFERRED"),
      c("PR031", "Additional outcomes", "secondary_outcomes", "AUTO_INFERRED")
    ),
    "PLANNED DATA SYNTHESIS" = list(
      c("PR032", "Strategy for data synthesis", "synthesis", "AUTO_POLICY")
    ),
    "CURRENT REVIEW STAGE" = list(
      c("PR033", "Stage of the review at this submission", "review_stage", "AUTO_POLICY"),
      c("PR034", "Publication of review results", "publication_status", "AUTO_POLICY")
    ),
    "REVIEW AFFILIATION, FUNDING AND PEER REVIEW" = list(
      c("PR035", "Review team members", "team", "USER_REQUIRED"),
      c("PR036", "Review affiliation", "affiliation", "USER_REQUIRED"),
      c("PR037", "Funding source", "funding", "USER_REQUIRED"),
      c("PR038", "Peer review", "peer_review", "USER_REQUIRED")
    ),
    "ADDITIONAL INFORMATION" = list(
      c("PR039", "Additional information", "additional_information", "AUTO_POLICY"),
      c("PR040", "Review conflict of interest", "conflicts", "USER_REQUIRED"),
      c("PR041", "Medical Subject Headings", "mesh_terms", "AUTO_INFERRED"),
      c("PR043", "Disclaimer", "disclaimer", "SYSTEM_OUTPUT")
    )
  )
}

parse_sheet_cell <- function(ref) {
  bits <- strsplit(ref, "!", fixed = TRUE)[[1]]
  list(sheet = bits[[1]], cell = bits[[2]])
}

cell_row <- function(cell) {
  as.integer(gsub("[^0-9]", "", cell))
}

cell_col <- function(cell) {
  openxlsx::convertFromExcelRef(gsub("[^A-Za-z]", "", cell))
}

create_prospero_template <- function(template_path) {
  dir.create(
    dirname(template_path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  wb <- createWorkbook()

  addWorksheet(wb, "PROSPERO_Form", gridLines = FALSE)
  addWorksheet(wb, "Field_Map", gridLines = TRUE)
  addWorksheet(wb, "Drafting_Audit", gridLines = TRUE)

  title_style <- createStyle(
    fgFill = "#1F4E78",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    fontSize = 14
  )

  section_style <- createStyle(
    fgFill = "#D9EAF7",
    fontColour = "#1F4E79",
    textDecoration = "bold"
  )

  label_style <- createStyle(
    textDecoration = "bold",
    wrapText = TRUE
  )

  answer_style <- createStyle(
    fgFill = "#FFFDF2",
    wrapText = TRUE,
    valign = "top",
    border = "TopBottomLeftRight",
    borderColour = "#C9C9C9"
  )

  meta_style <- createStyle(
    fgFill = "#F2F2F2",
    fontColour = "#666666",
    fontSize = 9,
    wrapText = TRUE
  )

  header_style <- createStyle(
    fgFill = "#1F4E78",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    halign = "center",
    wrapText = TRUE
  )

  writeData(
    wb,
    "PROSPERO_Form",
    "PROSPERO Universal Registration Form",
    startRow = 1,
    startCol = 1
  )

  mergeCells(wb, "PROSPERO_Form", cols = 1:6, rows = 1)

  addStyle(
    wb,
    "PROSPERO_Form",
    title_style,
    rows = 1,
    cols = 1:6,
    gridExpand = TRUE
  )

  fmap <- data.frame(
    Field_ID = character(),
    Section = character(),
    PROSPERO.field = character(),
    Answer_Cell = character(),
    Auto.fill.key = character(),
    Default.status = character(),
    User.editable = character(),
    stringsAsFactors = FALSE
  )

  row <- 3L

  for (section in names(prospero_sections())) {
    writeData(
      wb,
      "PROSPERO_Form",
      section,
      startRow = row,
      startCol = 1
    )

    mergeCells(
      wb,
      "PROSPERO_Form",
      cols = 1:6,
      rows = row
    )

    addStyle(
      wb,
      "PROSPERO_Form",
      section_style,
      rows = row,
      cols = 1:6,
      gridExpand = TRUE
    )

    row <- row + 1L

    for (item in prospero_sections()[[section]]) {
      label_row <- row
      answer_row <- row + 1L

      writeData(
        wb,
        "PROSPERO_Form",
        item[[2]],
        startRow = label_row,
        startCol = 1
      )

      mergeCells(
        wb,
        "PROSPERO_Form",
        cols = 1:6,
        rows = label_row
      )

      addStyle(
        wb,
        "PROSPERO_Form",
        label_style,
        rows = label_row,
        cols = 1:6,
        gridExpand = TRUE
      )

      mergeCells(
        wb,
        "PROSPERO_Form",
        cols = 1:4,
        rows = answer_row
      )

      addStyle(
        wb,
        "PROSPERO_Form",
        answer_style,
        rows = answer_row,
        cols = 1:4,
        gridExpand = TRUE
      )

      writeData(
        wb,
        "PROSPERO_Form",
        "Status",
        startRow = answer_row,
        startCol = 5
      )

      writeData(
        wb,
        "PROSPERO_Form",
        item[[4]],
        startRow = answer_row,
        startCol = 6
      )

      addStyle(
        wb,
        "PROSPERO_Form",
        meta_style,
        rows = answer_row,
        cols = 5:6,
        gridExpand = TRUE
      )

      dataValidation(
        wb,
        "PROSPERO_Form",
        col = 6,
        rows = answer_row,
        type = "list",
        value = '"AUTO_POLICY,AUTO_INFERRED,AUTO+EDITED,USER_REQUIRED,SYSTEM_OUTPUT"'
      )

      fmap <- rbind(
        fmap,
        data.frame(
          Field_ID = item[[1]],
          Section = section,
          PROSPERO.field = item[[2]],
          Answer_Cell = paste0("PROSPERO_Form!A", answer_row),
          Auto.fill.key = item[[3]],
          Default.status = item[[4]],
          User.editable = ifelse(
            item[[4]] == "SYSTEM_OUTPUT",
            "No",
            "Yes"
          ),
          stringsAsFactors = FALSE
        )
      )

      row <- row + 3L
    }
  }

  setColWidths(wb, "PROSPERO_Form", cols = 1, widths = 28)
  setColWidths(wb, "PROSPERO_Form", cols = 2:4, widths = 16)
  setColWidths(wb, "PROSPERO_Form", cols = 5, widths = 12)
  setColWidths(wb, "PROSPERO_Form", cols = 6, widths = 22)

  writeData(
    wb,
    "Field_Map",
    fmap,
    headerStyle = header_style
  )

  audit_header <- data.frame(
    Field_ID = character(),
    Auto.fill.key = character(),
    Status = character(),
    Source = character(),
    Note = character(),
    Generated_value = character(),
    stringsAsFactors = FALSE
  )

  writeData(
    wb,
    "Drafting_Audit",
    audit_header,
    headerStyle = header_style
  )

  setColWidths(
    wb,
    "Drafting_Audit",
    cols = 1:6,
    widths = c(12, 24, 18, 32, 40, 70)
  )

  saveWorkbook(
    wb,
    template_path,
    overwrite = TRUE
  )

  invisible(loadWorkbook(template_path))
}

# ============================================================
# 6. Autofill / audit / author-edit detection
# ============================================================

autofill_prospero <- function(
    template_path,
    output_path,
    state
) {
  ok <- file.copy(
    template_path,
    output_path,
    overwrite = TRUE
  )

  if (!isTRUE(ok)) {
    stop(
      "Failed to create PROSPERO working workbook: ",
      output_path,
      call. = FALSE
    )
  }

  wb <- loadWorkbook(output_path)
  fmap <- read.xlsx(output_path, sheet = "Field_Map")

  audit_rows <- list()
  counts <- c(
    AUTO_POLICY = 0L,
    AUTO_INFERRED = 0L,
    `AUTO+EDITED` = 0L,
    USER_REQUIRED = 0L,
    SYSTEM_OUTPUT = 0L
  )

  for (i in seq_len(nrow(fmap))) {
    ref <- parse_sheet_cell(fmap$Answer_Cell[[i]])
    key <- fmap$Auto.fill.key[[i]]

    value <- sanitize_prospero_text(
      state$info[[key]] %||% "",
      preserve_newlines = TRUE
    )

    provenance <- state$provenance[[key]] %||% list(
      status = "USER_REQUIRED",
      source = "not_defensibly_inferable",
      note = ""
    )

    status <- provenance$status %||% "USER_REQUIRED"

    if (!nzchar(value) && !identical(status, "SYSTEM_OUTPUT")) {
      status <- "USER_REQUIRED"
    }

    if (nzchar(value)) {
      writeData(
        wb,
        ref$sheet,
        value,
        startCol = cell_col(ref$cell),
        startRow = cell_row(ref$cell),
        colNames = FALSE,
        rowNames = FALSE
      )
    }

    writeData(
      wb,
      ref$sheet,
      status,
      startCol = 6,
      startRow = cell_row(ref$cell),
      colNames = FALSE,
      rowNames = FALSE
    )

    if (!status %in% names(counts)) {
      counts[[status]] <- 0L
    }

    counts[[status]] <- counts[[status]] + 1L

    audit_rows[[length(audit_rows) + 1L]] <- data.frame(
      Field_ID = fmap$Field_ID[[i]],
      Auto.fill.key = key,
      Status = status,
      Source = provenance$source %||% "",
      Note = provenance$note %||% "",
      Generated_value = value,
      stringsAsFactors = FALSE
    )
  }

  audit <- do.call(rbind, audit_rows)

  writeData(
    wb,
    "Drafting_Audit",
    audit,
    startRow = 1,
    startCol = 1,
    colNames = TRUE,
    rowNames = FALSE
  )

  saveWorkbook(
    wb,
    output_path,
    overwrite = TRUE
  )

  invisible(loadWorkbook(output_path))

  list(
    counts = counts,
    audit = audit
  )
}

read_single_excel_cell <- function(path, sheet, row, col) {
  value <- tryCatch(
    read.xlsx(
      path,
      sheet = sheet,
      rows = row,
      cols = col,
      colNames = FALSE,
      skipEmptyRows = FALSE,
      skipEmptyCols = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(value) || nrow(value) == 0L || ncol(value) == 0L) {
    return("")
  }

  sanitize_prospero_text(
    value[[1]][[1]] %||% "",
    preserve_newlines = TRUE
  )
}

mark_author_edits <- function(workbook_path) {
  fmap <- read.xlsx(workbook_path, sheet = "Field_Map")
  audit <- read.xlsx(workbook_path, sheet = "Drafting_Audit")

  if (
    nrow(fmap) == 0L ||
    nrow(audit) == 0L
  ) {
    return(invisible(workbook_path))
  }

  wb <- loadWorkbook(workbook_path)

  for (i in seq_len(nrow(fmap))) {
    field_id <- as.character(fmap$Field_ID[[i]])
    audit_idx <- match(field_id, as.character(audit$Field_ID))

    if (is.na(audit_idx)) next

    ref <- parse_sheet_cell(fmap$Answer_Cell[[i]])
    row <- cell_row(ref$cell)
    col <- cell_col(ref$cell)

    current_value <- read_single_excel_cell(
      workbook_path,
      ref$sheet,
      row,
      col
    )

    generated_value <- sanitize_prospero_text(
      audit$Generated_value[[audit_idx]] %||% "",
      preserve_newlines = TRUE
    )

    generated_status <- sanitize_prospero_text(
      audit$Status[[audit_idx]] %||% "",
      preserve_newlines = FALSE
    )

    if (
      generated_status %in% c("AUTO_POLICY", "AUTO_INFERRED") &&
      !identical(current_value, generated_value)
    ) {
      writeData(
        wb,
        ref$sheet,
        "AUTO+EDITED",
        startCol = 6,
        startRow = row,
        colNames = FALSE,
        rowNames = FALSE
      )
    }
  }

  saveWorkbook(
    wb,
    workbook_path,
    overwrite = TRUE
  )

  invisible(loadWorkbook(workbook_path))
}

write_provenance_file <- function(
    out_dir,
    project_slug,
    state,
    llm_result
) {
  provenance_doc <- list(
    prospero_drafting = list(
      project = project_slug,
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      llm_attempted = TRUE,
      llm_succeeded = isTRUE(llm_result$ok),
      llm_failure_reason = llm_result$reason %||% "",
      statuses = list(
        AUTO_POLICY = "Universal methodological policy/default, subject to project-level override.",
        AUTO_INFERRED = "Derived or synthesized from canonical configuration and bounded active-project context.",
        `AUTO+EDITED` = "An automatically drafted value subsequently edited by the author.",
        USER_REQUIRED = "The toolkit cannot responsibly determine this field; author/external action is required.",
        SYSTEM_OUTPUT = "Workflow-generated administrative/system value."
      ),
      fields = state$provenance
    )
  )

  yaml::write_yaml(
    provenance_doc,
    file.path(out_dir, "prospero_drafting_provenance.yml")
  )

  invisible(provenance_doc)
}

# ============================================================
# 7. Main
# ============================================================

run_prospero_preparation <- function() {
  repo_root <- find_repo_root()

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(repo_root)

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

  project_slug <- get_project_id(config)
  project_root <- paths$project_root

  cat(
    "\nActive project: ",
    project_slug,
    "\nProject root: ",
    project_root,
    "\n",
    sep = ""
  )

  out_dir <- file.path(
    project_root,
    "results",
    "registration",
    "prospero"
  )

  dir.create(
    out_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  template_path <- file.path(
    repo_root,
    "templates",
    "registration",
    "Universal_PROSPERO_Form_Template.xlsx"
  )

  autofilled_path <- file.path(
    out_dir,
    "prospero_autofilled.xlsx"
  )

  final_path <- file.path(
    out_dir,
    "prospero_final.xlsx"
  )

  if (file.exists(final_path) && file.info(final_path)$size > 0L) {
    cat(
      "\nPROSPERO preparation already complete:\n",
      normalizePath(final_path, winslash = "/", mustWork = TRUE),
      "\n",
      sep = ""
    )

    return(invisible(TRUE))
  }

  regenerate <- isTRUE(
    getOption("srma.prospero.regenerate")
  ) || as_flag(
    Sys.getenv("SRMA_PROSPERO_REGENERATE", unset = ""),
    FALSE
  )

  if (!file.exists(autofilled_path) || isTRUE(regenerate)) {
    cat("\nCreating canonical PROSPERO template...\n")
    create_prospero_template(template_path)
    cat("PASS: template created and re-opened successfully.\n")

    cfg <- yaml::read_yaml(
      file.path(repo_root, "config", "project_config.yml")
    )

    search_terms_path <- file.path(
      repo_root,
      "config",
      "search_terms.yml"
    )

    search_terms <- if (file.exists(search_terms_path)) {
      yaml::read_yaml(search_terms_path)
    } else {
      list()
    }

    project_context <- collect_active_project_context(
      repo_root = repo_root,
      project_root = project_root
    )

    state <- build_deterministic_draft(
      cfg = cfg,
      project_text = project_context
    )

    cat("\nDrafting PROSPERO protocol text with LLM assistance...\n")

    llm_result <- run_llm_protocol_drafting(
      repo_root = repo_root,
      cfg = cfg,
      search_terms = search_terms,
      project_context = project_context,
      baseline_info = state$info,
      out_dir = out_dir
    )

    if (isTRUE(llm_result$ok)) {
      cat("LLM-assisted protocol drafting: PASS\n")
      state <- merge_llm_draft(state, llm_result)
    } else {
      cat(
        "LLM-assisted protocol drafting: FALLBACK\n",
        "Reason: ",
        llm_result$reason,
        "\n",
        "Continuing with deterministic canonical/policy drafting.\n",
        sep = ""
      )
    }

    summary <- autofill_prospero(
      template_path = template_path,
      output_path = autofilled_path,
      state = state
    )

    write_provenance_file(
      out_dir = out_dir,
      project_slug = project_slug,
      state = state,
      llm_result = llm_result
    )

    cat("\nPROSPERO AUTO-FILL SUMMARY\n")

    for (status in c(
      "AUTO_POLICY",
      "AUTO_INFERRED",
      "AUTO+EDITED",
      "USER_REQUIRED",
      "SYSTEM_OUTPUT"
    )) {
      count <- summary$counts[[status]] %||% 0L
      cat(status, ": ", count, "\n", sep = "")
    }

    cat(
      "Created: ",
      normalizePath(
        autofilled_path,
        winslash = "/",
        mustWork = TRUE
      ),
      "\n",
      sep = ""
    )
  } else {
    cat(
      "\nExisting PROSPERO working draft preserved:\n",
      normalizePath(
        autofilled_path,
        winslash = "/",
        mustWork = TRUE
      ),
      "\n",
      "Set options(srma.prospero.regenerate = TRUE) only if deliberate regeneration is needed.\n",
      sep = ""
    )
  }

  cat(
    "\nPlease review/edit the working workbook now. ",
    "Focus on USER_REQUIRED items and any AUTO field you wish to override.\n",
    "The Drafting_Audit sheet records field-level provenance.\n",
    sep = ""
  )

  finalize <- ask_yes_no(
    paste0(
      "이 내용을 실제 PROSPERO 등록에 사용할 최종 양식으로 ",
      "확정하시겠습니까? [Yes/No]: "
    )
  )

  if (!finalize) {
    cat(
      "Working file kept for further editing. ",
      "The Universal Runner may be resumed after review.\n",
      sep = ""
    )
    return(invisible(FALSE))
  }

  mark_author_edits(autofilled_path)

  ok <- file.copy(
    autofilled_path,
    final_path,
    overwrite = TRUE
  )

  if (!isTRUE(ok) || !file.exists(final_path)) {
    stop(
      "Failed to create prospero_final.xlsx.",
      call. = FALSE
    )
  }

  invisible(loadWorkbook(final_path))

  cat("\nPASS: prospero_final.xlsx created.\n")
  cat(
    "Canonical artifact: ",
    normalizePath(
      final_path,
      winslash = "/",
      mustWork = TRUE
    ),
    "\n",
    sep = ""
  )
  cat(
    "The Runner will next require explicit confirmation of actual PROSPERO registration before search generation.\n"
  )

  invisible(TRUE)
}

run_prospero_preparation()
