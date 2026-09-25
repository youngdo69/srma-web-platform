# ============================================================
# Universal SR-MA Agent Toolkit
# R/39_generate_manuscript_draft_with_verified_literature.R
#
# VERSION: 2.4.0
#
# PURPOSE
# ------------------------------------------------------------
# Generate an actual manuscript draft from:
#   A) the frozen R/38 manuscript evidence package, and
#   B) an externally searched and VERIFIED literature library.
#
# This module is designed for topic-agnostic scientific SR-MA manuscripts.
#
# CRITICAL GOVERNANCE
# ------------------------------------------------------------
# 1. Methods/Results factual claims must come from R/38 frozen evidence.
# 2. Introduction/Discussion require external literature search/review.
# 3. References are NEVER invented by the drafting model.
# 4. PubMed is the authoritative literature-discovery/verification source
#    for this module.
# 5. The drafting model may cite ONLY PMID tokens supplied by this script:
#       [[PMID:12345678]]
# 6. Every cited PMID must exist in the verified literature library.
# 7. Introduction + Discussion must cite >= 25 UNIQUE verified references
#    by default. This is a hard gate unless changed in project config.
# 8. R/39 performs NO meta-analysis recalculation.
# 9. If R/38 status is DRAFT_READY_PENDING_ROB, R/39 may generate a
#    provisional draft but must not present final RoB-dependent claims.
#
# OUTPUT ROOT
# ------------------------------------------------------------
# results/manuscript/production/drafting/
#   literature_search_plan.json
#   pubmed_search_log.csv
#   literature_candidates.csv
#   literature_selected.csv
#   literature_selected.xlsx
#   introduction_draft.md
#   methods_draft.md
#   results_draft.md
#   discussion_draft.md
#   abstract_draft.md
#   manuscript_draft_with_pmid_tokens.md
#   manuscript_draft.md
#   references_vancouver.md
#   citation_audit.csv
#   citation_number_map.csv
#   manuscript_drafting_state.yml
#   manuscript_drafting_report.txt
#   agent_raw/
#
# OPTIONAL CONFIG
# ------------------------------------------------------------
# analysis:
#   manuscript:
#     model: "gpt-5.6"
#     minimum_unique_references: 25
#     minimum_intro_references: 8
#     minimum_discussion_references: 15
#     literature_candidate_target: 70
#     literature_selected_target: 35
#     pubmed_retmax_per_query: 25
#     literature_queries: 6
#     search_recent_years: 8
#     target_journal: ""
#
# Environment:
#   OPENAI_API_KEY                 required
#   SRMA_MANUSCRIPT_MODEL          optional override
#   NCBI_EMAIL                     recommended
#   NCBI_API_KEY                   optional
# ============================================================


# ============================================================
# 0. Required packages
# ============================================================

required_packages <- c(
  "yaml",
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "jsonlite",
  "httr2",
  "xml2",
  "openxlsx",
  "digest"
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
    "Missing required package(s): ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}


# ============================================================
# 1. Helpers
# ============================================================

normalize_slash <- function(x) {
  stringr::str_replace_all(
    as.character(x),
    "\\\\",
    "/"
  )
}

safe_chr <- function(x, default = "") {
  if (
    is.null(x) ||
    length(x) == 0L ||
    all(is.na(x))
  ) {
    return(default)
  }

  out <- trimws(as.character(x[[1]]))

  if (
    is.na(out) ||
    !nzchar(out)
  ) {
    return(default)
  }

  out
}

safe_int <- function(x, default) {
  out <- suppressWarnings(
    as.integer(x)
  )

  if (
    length(out) == 0L ||
    is.na(out[[1]])
  ) {
    return(as.integer(default))
  }

  out[[1]]
}

file_nonempty <- function(path) {
  # Vector-safe file existence/size check.
  # Returns one logical value per supplied path; scalar input remains scalar.
  path <- as.character(path)

  exists <- file.exists(path)
  size <- rep(NA_real_, length(path))

  if (any(exists)) {
    size[exists] <- file.info(path[exists])$size
  }

  exists & !is.na(size) & size > 0
}

first_existing_col <- function(data, candidates) {
  hits <- candidates[
    candidates %in% names(data)
  ]

  if (length(hits) == 0L) {
    return(NA_character_)
  }

  hits[[1]]
}

read_text_file <- function(path) {
  if (!file_nonempty(path)) {
    return("")
  }

  paste(
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    collapse = "\n"
  )
}

write_text_file <- function(text, path) {
  writeLines(
    enc2utf8(
      as.character(text)
    ),
    path,
    useBytes = TRUE
  )
}

now_text <- function() {
  format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )
}

sha256_file <- function(path) {
  if (!file_nonempty(path)) {
    return("")
  }

  digest::digest(
    file = path,
    algo = "sha256",
    serialize = FALSE
  )
}

slugify <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- stringr::str_replace_all(
    x,
    "[^a-z0-9]+",
    "_"
  )
  x <- stringr::str_replace_all(
    x,
    "_+",
    "_"
  )
  stringr::str_replace_all(
    x,
    "^_|_$",
    ""
  )
}

pick_config_value <- function(
    x,
    path,
    default = NULL
) {
  keys <- strsplit(
    path,
    "\\."
  )[[1]]

  cur <- x

  for (key in keys) {
    if (
      !is.list(cur) ||
      is.null(cur[[key]])
    ) {
      return(default)
    }

    cur <- cur[[key]]
  }

  cur
}

first_config_value <- function(
    config,
    paths,
    default = NULL
) {
  for (path in paths) {
    value <- pick_config_value(
      config,
      path,
      default = NULL
    )

    if (
      !is.null(value) &&
      length(value) > 0L
    ) {
      return(value)
    }
  }

  default
}

strip_markdown_fence <- function(x) {
  x <- trimws(
    as.character(x)
  )

  x <- sub(
    "^```(?:json|markdown|md|text)?\\s*",
    "",
    x,
    perl = TRUE,
    ignore.case = TRUE
  )

  x <- sub(
    "\\s*```$",
    "",
    x,
    perl = TRUE
  )

  # Some model responses can contain a legacy literal `nn###` / `nn##`
  # boundary where two intended newlines were flattened before a Markdown
  # heading. Restore only this highly specific heading-boundary pattern.
  x <- gsub(
    "[[:space:]]+nn(?=#{1,6}[[:space:]]+)",
    "\n\n",
    x,
    perl = TRUE
  )

  trimws(x)
}


# ------------------------------------------------------------
# Publication-facing text hygiene
# ------------------------------------------------------------
# Scientific content is authored upstream. This layer only normalizes visible
# manuscript prose so internal workflow vocabulary, stale display references,
# flattened Markdown boundaries, and simple LaTeX math do not leak into the
# author-facing manuscript. R/40 remains the independent numerical/citation
# consistency gate after R/39.

normalize_publication_text <- function(x) {
  x <- as.character(x)

  # Recover a legacy flattened two-newline + Markdown-heading boundary.
  x <- gsub(
    "[[:space:]]+nn(?=#{1,6}[[:space:]]+)",
    "\n\n",
    x,
    perl = TRUE
  )

  # Journal-neutral plain-text math. Keep the scientific value unchanged while
  # removing Markdown/LaTeX delimiters that Word export may render literally.
  fixed_math <- c(
    "\\(\\tau^2\\)" = "τ²",
    "\\(I^2\\)" = "I²",
    "\\(Q\\)" = "Q",
    "\\(P\\)" = "P",
    "\\tau^2" = "τ²",
    "I^2" = "I²"
  )

  for (needle in names(fixed_math)) {
    x <- gsub(
      needle,
      fixed_math[[needle]],
      x,
      fixed = TRUE
    )
  }

  # Machine-style eligibility labels -> normal scholarly prose.
  x <- gsub("\\bDESCRIPTIVEONLY\\b", "descriptive only", x, perl = TRUE)
  x <- gsub("\\bNOT[_]?ESTIMABLE\\b", "not estimable", x, perl = TRUE)

  # A few toolkit-provenance phrases have no place in visible publication text.
  # These replacements are presentation-only and do not change scientific facts.
  x <- gsub("canonical quantitative synthesis", "quantitative synthesis", x,
            ignore.case = TRUE, fixed = TRUE)
  x <- gsub("canonical pooled estimates", "pooled estimates", x,
            ignore.case = TRUE, fixed = TRUE)
  x <- gsub("canonical inferential model", "prespecified meta-analytic model", x,
            ignore.case = TRUE, fixed = TRUE)
  x <- gsub("canonical search-stage records", "study records", x,
            ignore.case = TRUE, fixed = TRUE)
  x <- gsub("targeted Human Verification", "prespecified data verification", x,
            ignore.case = TRUE, fixed = TRUE)
  x <- gsub("preserved human-review gates", "prespecified review procedures", x,
            ignore.case = TRUE, fixed = TRUE)
  x <- gsub("author-approved assessment outputs", "finalized risk-of-bias assessments", x,
            ignore.case = TRUE, fixed = TRUE)

  # R/41 owns final Table/Figure numbering. Remove only simple parenthetical
  # display-number callouts; do not touch scientific table/figure terminology.
  x <- gsub(
    "[[:space:]]*\\((?:(?:Table|Figure)[[:space:]]+[0-9]+)(?:[[:space:]]*[,;][[:space:]]*(?:Table|Figure)[[:space:]]+[0-9]+)*\\)",
    "",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )

  trimws(x)
}

publication_hygiene_issues <- function(x) {
  x <- as.character(x)

  failed <- character(0)

  regex_checks <- c(
    flattened_markdown_heading = "[[:space:]]+nn#{2,6}[[:space:]]+",
    internal_module_name = "\\bR/[0-9]+(?:B)?\\b",
    machine_status_label = "\\b(?:DESCRIPTIVEONLY|NOT[_]?ESTIMABLE)\\b",
    machine_study_identifier = "\\b(?:SCR_[0-9]+|screening_id(?:::|\\b))",
    human_gate_vocabulary = "\\b(?:Human Verification|human-review gate|workflow state)\\b",
    internal_canonical_phrase = "\\bcanonical[[:space:]]+(?:output|workflow|evidence|inferential|quantitative|pooled|search-stage|model)\\b",
    premature_display_number = "\\b(?:Table|Figure)[[:space:]]+[0-9]+\\b"
  )

  regex_failed <- names(regex_checks)[
    vapply(
      regex_checks,
      function(pattern) {
        grepl(pattern, x, ignore.case = TRUE, perl = TRUE)
      },
      logical(1)
    )
  ]

  failed <- c(failed, regex_failed)

  if (
    grepl("\\(", x, fixed = TRUE) ||
      grepl("\\)", x, fixed = TRUE) ||
      grepl("\\tau", x, fixed = TRUE)
  ) {
    failed <- c(failed, "latex_inline_math")
  }

  unique(failed)
}

load_dotenv_simple <- function(
    path = ".env"
) {
  if (!file.exists(path)) {
    return(invisible(FALSE))
  }

  lines <- readLines(
    path,
    warn = FALSE,
    encoding = "UTF-8"
  )

  for (line in lines) {
    line <- trimws(line)

    if (
      !nzchar(line) ||
      startsWith(line, "#")
    ) {
      next
    }

    if (startsWith(line, "export ")) {
      line <- substring(
        line,
        8
      )
    }

    pos <- regexpr(
      "=",
      line,
      fixed = TRUE
    )

    if (pos[[1]] < 2L) {
      next
    }

    key <- trimws(
      substr(
        line,
        1L,
        pos[[1]] - 1L
      )
    )

    value <- trimws(
      substr(
        line,
        pos[[1]] + 1L,
        nchar(line)
      )
    )

    value <- sub(
      '^"(.*)"$',
      "\\1",
      value
    )

    value <- sub(
      "^'(.*)'$",
      "\\1",
      value
    )

    if (
      nzchar(key) &&
      !nzchar(
        Sys.getenv(
          key,
          unset = ""
        )
      )
    ) {
      do.call(
        Sys.setenv,
        stats::setNames(
          list(value),
          key
        )
      )
    }
  }

  invisible(TRUE)
}

collapse_nonblank <- function(
    x,
    sep = "; "
) {
  x <- trimws(
    as.character(x)
  )

  x <- x[
    !is.na(x) &
      nzchar(x)
  ]

  if (length(x) == 0L) {
    return("")
  }

  paste(
    unique(x),
    collapse = sep
  )
}


# ============================================================
# 2. Project context
# ============================================================

repo_root <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

config_file <- file.path(
  repo_root,
  "config",
  "project_config.yml"
)

if (!file.exists(config_file)) {
  stop(
    "config/project_config.yml was not found. Run R/39 from the project root.",
    call. = FALSE
  )
}

config <- yaml::read_yaml(
  config_file
)

if (is.null(config)) {
  stop(
    "project_config.yml could not be parsed.",
    call. = FALSE
  )
}

load_dotenv_simple(
  file.path(
    repo_root,
    ".env"
  )
)

project_id <- safe_chr(
  first_config_value(
    config,
    c(
      "project.id",
      "project_id"
    ),
    basename(repo_root)
  ),
  basename(repo_root)
)

project_title <- safe_chr(
  first_config_value(
    config,
    c(
      "project.title",
      "title"
    ),
    project_id
  ),
  project_id
)

manuscript_cfg <- first_config_value(
  config,
  c(
    "analysis.manuscript",
    "manuscript"
  ),
  list()
)

if (!is.list(manuscript_cfg)) {
  manuscript_cfg <- list()
}

minimum_unique_references <- safe_int(
  manuscript_cfg$minimum_unique_references,
  25L
)

minimum_intro_references <- safe_int(
  manuscript_cfg$minimum_intro_references,
  8L
)

minimum_discussion_references <- safe_int(
  manuscript_cfg$minimum_discussion_references,
  15L
)

literature_candidate_target <- safe_int(
  manuscript_cfg$literature_candidate_target,
  70L
)

literature_selected_target <- max(
  safe_int(
    manuscript_cfg$literature_selected_target,
    35L
  ),
  minimum_unique_references
)

pubmed_retmax_per_query <- safe_int(
  manuscript_cfg$pubmed_retmax_per_query,
  25L
)

literature_query_count <- safe_int(
  manuscript_cfg$literature_queries,
  6L
)

search_recent_years <- safe_int(
  manuscript_cfg$search_recent_years,
  8L
)

target_journal <- safe_chr(
  manuscript_cfg$target_journal,
  safe_chr(
    first_config_value(
      config,
      c(
        "reporting.target_journal"
      ),
      ""
    ),
    ""
  )
)

model_name <- safe_chr(
  Sys.getenv(
    "SRMA_MANUSCRIPT_MODEL",
    unset = ""
  ),
  safe_chr(
    manuscript_cfg$model,
    "gpt-5.6"
  )
)

openai_api_key <- Sys.getenv(
  "OPENAI_API_KEY",
  unset = ""
)

if (!nzchar(openai_api_key)) {
  stop(
    "OPENAI_API_KEY is required for R/39 manuscript drafting.",
    call. = FALSE
  )
}

ncbi_email <- Sys.getenv(
  "NCBI_EMAIL",
  unset = ""
)

ncbi_api_key <- Sys.getenv(
  "NCBI_API_KEY",
  unset = ""
)


# ============================================================
# 3. R/38 manuscript evidence package
# ============================================================

production_root <- file.path(
  repo_root,
  "results",
  "manuscript",
  "production"
)

r38_state_file <- file.path(
  production_root,
  "manuscript_production_state.yml"
)

r38_registry_file <- file.path(
  production_root,
  "manuscript_evidence_registry.csv"
)

r38_artifact_registry_file <- file.path(
  production_root,
  "manuscript_artifact_registry.csv"
)

r38_methods_file <- file.path(
  production_root,
  "methods_factual_blocks.md"
)

r38_results_file <- file.path(
  production_root,
  "results_factual_blocks.md"
)

r38_scaffold_file <- file.path(
  production_root,
  "manuscript_scaffold.md"
)

required_r38 <- c(
  state = r38_state_file,
  registry = r38_registry_file,
  artifact_registry = r38_artifact_registry_file,
  methods = r38_methods_file,
  results = r38_results_file,
  scaffold = r38_scaffold_file
)

r38_ok <- vapply(
  required_r38,
  file_nonempty,
  logical(1)
)

if (!all(r38_ok)) {
  stop(
    "R/38 manuscript package is incomplete. Missing: ",
    paste(
      names(required_r38)[
        !r38_ok
      ],
      collapse = ", "
    ),
    call. = FALSE
  )
}

r38_state <- yaml::read_yaml(
  r38_state_file
)

readiness_status <- safe_chr(
  r38_state$manuscript_production$status,
  "NOT_READY"
)

if (
  !readiness_status %in%
  c(
    "DRAFT_READY_PENDING_ROB",
    "FINAL_EVIDENCE_READY"
  )
) {
  stop(
    "R/39 requires R/38 status DRAFT_READY_PENDING_ROB or FINAL_EVIDENCE_READY. Current status: ",
    readiness_status,
    call. = FALSE
  )
}

methods_factual_blocks <- read_text_file(
  r38_methods_file
)

results_factual_blocks <- read_text_file(
  r38_results_file
)

manuscript_scaffold <- read_text_file(
  r38_scaffold_file
)

artifact_registry <- safe_read_csv(
  r38_artifact_registry_file
)

required_artifact_columns <- c(
  "artifact_id",
  "artifact_type",
  "title",
  "manuscript_role",
  "manuscript_priority",
  "coverage_required",
  "coverage_token",
  "source_relative_path"
)

missing_artifact_columns <- setdiff(
  required_artifact_columns,
  names(artifact_registry)
)

if (length(missing_artifact_columns) > 0L) {
  stop(
    paste0(
      "R/38 manuscript_artifact_registry.csv is missing required column(s): ",
      paste(
        missing_artifact_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

artifact_registry$coverage_required <- tolower(
  trimws(
    as.character(
      artifact_registry$coverage_required
    )
  )
) %in% c(
  "true",
  "t",
  "1",
  "yes",
  "y"
)

required_coverage_artifacts <- artifact_registry |>
  dplyr::filter(
    coverage_required
  ) |>
  dplyr::arrange(
    artifact_id
  )

artifact_catalog_text <- if (
  nrow(required_coverage_artifacts) > 0L
) {
  paste(
    vapply(
      seq_len(nrow(required_coverage_artifacts)),
      function(i) {
        row <- required_coverage_artifacts[
          i,
          ,
          drop = FALSE
        ]

        paste0(
          row$coverage_token[[1]],
          " | ",
          row$title[[1]],
          " | type=",
          row$artifact_type[[1]],
          " | role=",
          row$manuscript_role[[1]],
          " | source=",
          row$source_relative_path[[1]]
        )
      },
      character(1)
    ),
    collapse = "\n"
  )
} else {
  "No artifact is marked coverage_required=TRUE."
}

extract_artifact_markers <- function(text) {
  matches <- stringr::str_match_all(
    text,
    "<!--\\s*ARTIFACT:\\s*([A-Za-z0-9_-]+)\\s*-->"
  )[[1]]

  if (
    is.null(matches) ||
      nrow(matches) == 0L
  ) {
    return(character(0))
  }

  unique(
    matches[, 2]
  )
}


artifact_marker_context <- function(
    text,
    coverage_token,
    window = 1200L
) {
  location <- stringr::str_locate(
    text,
    stringr::fixed(
      coverage_token
    )
  )

  if (
    is.null(location) ||
      nrow(location) == 0L ||
      any(is.na(location[1, ]))
  ) {
    return("")
  }

  marker_start <- as.integer(
    location[1, "start"]
  )

  context_start <- max(
    1L,
    marker_start - window
  )

  context <- substr(
    text,
    context_start,
    marker_start - 1L
  )

  # Coverage is paragraph/cluster based, not one-marker-one-sentence based.
  # Multiple artifacts can legitimately support the same substantive paragraph.
  # Therefore remove earlier coverage markers from the context rather than
  # truncating everything before the immediately preceding marker.
  context <- gsub(
    "<!--\\s*ARTIFACT:\\s*[A-Za-z0-9_-]+\\s*-->",
    " ",
    context,
    perl = TRUE
  )

  # Prefer the most recent prose paragraph/cluster when available.
  paragraph_parts <- strsplit(
    context,
    "\\n\\s*\\n",
    perl = TRUE
  )[[1]]

  paragraph_parts <- trimws(
    paragraph_parts
  )

  paragraph_parts <- paragraph_parts[
    nzchar(paragraph_parts)
  ]

  if (length(paragraph_parts) > 0L) {
    context <- tail(
      paragraph_parts,
      1L
    )
  }

  trimws(context)
}


artifact_marker_context_self_test <- function() {
  demo <- paste0(
    "A substantive paragraph describing two related figures and their shared finding in adequate detail. ",
    "<!-- ARTIFACT: A_ONE --> ",
    "<!-- ARTIFACT: A_TWO -->"
  )

  c1 <- artifact_marker_context(
    demo,
    "<!-- ARTIFACT: A_ONE -->"
  )

  c2 <- artifact_marker_context(
    demo,
    "<!-- ARTIFACT: A_TWO -->"
  )

  if (
    nchar(gsub("[^[:alpha:]]", "", c1)) < 40L ||
      nchar(gsub("[^[:alpha:]]", "", c2)) < 40L
  ) {
    stop(
      "Internal artifact-marker shared-context self-test failed.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

artifact_marker_context_self_test()


artifact_ids_needing_results_coverage <- function(
    text,
    required_artifacts
) {
  if (
    is.null(required_artifacts) ||
      nrow(required_artifacts) == 0L
  ) {
    return(character(0))
  }

  failed <- vapply(
    seq_len(nrow(required_artifacts)),
    function(i) {
      row <- required_artifacts[
        i,
        ,
        drop = FALSE
      ]

      context <- artifact_marker_context(
        text,
        row$coverage_token[[1]]
      )

      marker_present <- row$artifact_id[[1]] %in%
        extract_artifact_markers(text)

      substantive <- nchar(
        gsub(
          "[^[:alpha:]]",
          "",
          context
        )
      ) >= 40L

      !marker_present ||
        !substantive
    },
    logical(1)
  )

  required_artifacts$artifact_id[
    failed
  ]
}


# ============================================================
# 4. Output paths
# ============================================================

draft_root <- file.path(
  production_root,
  "drafting"
)

raw_root <- file.path(
  draft_root,
  "agent_raw"
)

dir.create(
  draft_root,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  raw_root,
  recursive = TRUE,
  showWarnings = FALSE
)

search_plan_file <- file.path(
  draft_root,
  "literature_search_plan.json"
)

search_log_file <- file.path(
  draft_root,
  "pubmed_search_log.csv"
)

candidate_file <- file.path(
  draft_root,
  "literature_candidates.csv"
)

selected_file <- file.path(
  draft_root,
  "literature_selected.csv"
)

selected_xlsx <- file.path(
  draft_root,
  "literature_selected.xlsx"
)

intro_file <- file.path(
  draft_root,
  "introduction_draft.md"
)

methods_file <- file.path(
  draft_root,
  "methods_draft.md"
)

results_file <- file.path(
  draft_root,
  "results_draft.md"
)

discussion_file <- file.path(
  draft_root,
  "discussion_draft.md"
)

abstract_file <- file.path(
  draft_root,
  "abstract_draft.md"
)

token_manuscript_file <- file.path(
  draft_root,
  "manuscript_draft_with_pmid_tokens.md"
)

final_manuscript_file <- file.path(
  draft_root,
  "manuscript_draft.md"
)

references_file <- file.path(
  draft_root,
  "references_vancouver.md"
)

citation_audit_file <- file.path(
  draft_root,
  "citation_audit.csv"
)

citation_map_file <- file.path(
  draft_root,
  "citation_number_map.csv"
)

state_file <- file.path(
  draft_root,
  "manuscript_drafting_state.yml"
)

report_file <- file.path(
  draft_root,
  "manuscript_drafting_report.txt"
)


# ============================================================
# 5. OpenAI Responses API
# ============================================================

extract_response_text <- function(response_json) {
  if (
    !is.null(response_json$output_text) &&
    length(response_json$output_text) > 0L
  ) {
    return(
      paste(
        as.character(
          response_json$output_text
        ),
        collapse = "\n"
      )
    )
  }

  output <- response_json$output

  if (is.null(output)) {
    return("")
  }

  texts <- character(0)

  for (item in output) {
    if (
      is.list(item) &&
      identical(
        safe_chr(item$type),
        "message"
      ) &&
      !is.null(item$content)
    ) {
      for (content in item$content) {
        if (
          is.list(content) &&
          identical(
            safe_chr(content$type),
            "output_text"
          ) &&
          !is.null(content$text)
        ) {
          texts <- c(
            texts,
            as.character(
              content$text
            )
          )
        }
      }
    }
  }

  paste(
    texts,
    collapse = "\n"
  )
}


call_openai <- function(
    system_prompt,
    user_prompt,
    raw_name,
    max_output_tokens = 12000L
) {
  payload <- list(
    model = model_name,
    input = list(
      list(
        role = "system",
        content = list(
          list(
            type = "input_text",
            text = system_prompt
          )
        )
      ),
      list(
        role = "user",
        content = list(
          list(
            type = "input_text",
            text = user_prompt
          )
        )
      )
    ),
    max_output_tokens = as.integer(
      max_output_tokens
    )
  )

  response <- httr2::request(
    "https://api.openai.com/v1/responses"
  ) |>
    httr2::req_headers(
      Authorization = paste(
        "Bearer",
        openai_api_key
      ),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(
      payload,
      auto_unbox = TRUE
    ) |>
    httr2::req_retry(
      max_tries = 3
    ) |>
    httr2::req_timeout(
      seconds = 180
    ) |>
    httr2::req_perform()

  response_json <- httr2::resp_body_json(
    response,
    simplifyVector = FALSE
  )

  raw_path <- file.path(
    raw_root,
    paste0(
      raw_name,
      ".json"
    )
  )

  jsonlite::write_json(
    response_json,
    raw_path,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )

  text <- extract_response_text(
    response_json
  )

  if (!nzchar(trimws(text))) {
    stop(
      "OpenAI response contained no usable output text for: ",
      raw_name,
      call. = FALSE
    )
  }

  text
}


# ------------------------------------------------------------
# Conservative post-generation evidence-conformance repair
# ------------------------------------------------------------
# This is deliberately narrower than drafting. It does not create a new section;
# it edits only unsupported/over-broad statements against the frozen R/38
# evidence contract. R/40 remains the independent final validator.

repair_section_for_evidence_conformance <- function(
    section_name,
    section_text,
    evidence_text,
    literature_text = "",
    preserve_artifact_tokens = FALSE
) {
  repair_system <- paste(
    "You are a conservative evidence-conformance editor for a systematic-review manuscript.",
    "Your task is NOT to rewrite the section broadly. Make the minimum edits needed so every current-review claim is directly supported by the supplied frozen evidence.",
    "Do not add any new scientific claim, number, statistic, PMID, citation, subgroup, outcome, or interpretation.",
    "Do not perform arithmetic or derive totals, percentages, ratios, averages, ranges, or rounded values from component numbers.",
    "Preserve all correct wording, headings, and citation tokens unless a sentence itself is unsupported and must be removed or narrowed.",
    "If a population descriptor in an included study is broader or mixed relative to the review-level label, preserve that study-specific qualification and do not claim uniformity across all included studies.",
    "Do not infer technical feasibility, technical success, procedural feasibility, procedural success, safety, or effectiveness merely because a procedure was performed. Such conclusions require explicit endpoint evidence in the frozen results.",
    "A verified literature record marked Included in current review: YES must never be used to import extra current-review outcomes, effect directions, significance tests, or procedural findings that are absent from the frozen R/38 Results evidence.",
    "For risk-of-bias prose, never use bare numeric domain identifiers such as domain 1 or domains 1-4. Use named domains only if their names are explicitly supported; otherwise retain only the overall judgement or a non-numeric summary.",
    "Keep uncertainty language conservative.",
    if (isTRUE(preserve_artifact_tokens)) {
      "Preserve every exact <!-- ARTIFACT: ... --> token already present, unchanged and adjacent to the same substantive evidence paragraph."
    } else {
      "Do not introduce artifact tokens."
    },
    "Preserve valid [[PMID:...]] tokens when the supported sentence remains; if an unsupported sentence is removed, its citation token may be removed with it.",
    "Return ONLY the repaired section text, with no commentary or code fence."
  )

  repair_user <- paste0(
    "SECTION: ", section_name, "\n\n",
    "FROZEN R/38 EVIDENCE:\n", evidence_text, "\n\n",
    if (nzchar(literature_text)) {
      paste0("VERIFIED LITERATURE LIBRARY (records explicitly flag whether they are included in the current review):\n", literature_text, "\n\n")
    } else {
      ""
    },
    "CURRENT SECTION:\n", section_text
  )

  repaired <- call_openai(
    repair_system,
    repair_user,
    paste0("evidence_conformance_", tolower(section_name)),
    max_output_tokens = max(6000L, as.integer(nchar(section_text) / 2L + 4000L))
  )

  normalize_publication_text(
    strip_markdown_fence(repaired)
  )
}

rob_numeric_domain_leakage <- function(x) {
  x <- as.character(x)
  grepl(
    "(?i)\\b(?:risk[- ]of[- ]bias|ROBINS[- ]?I|RoB(?:\\s?2)?)?[^\\n.]{0,120}\\bdomains?\\s+(?:[0-9]+(?:\\s*[–—-]\\s*[0-9]+)?)(?:\\s*(?:,|and|&)\\s*[0-9]+(?:\\s*[–—-]\\s*[0-9]+)?)*",
    x,
    perl = TRUE
  )
}


# ============================================================
# 6. Build literature-search context
# ============================================================

get_config_text <- function(
    paths,
    default = ""
) {
  value <- first_config_value(
    config,
    paths,
    default
  )

  if (
    is.null(value) ||
    length(value) == 0L
  ) {
    return(default)
  }

  if (is.atomic(value)) {
    return(
      collapse_nonblank(value)
    )
  }

  default
}

# Resolve manuscript-literature search context from the canonical project
# research_question tree first, while retaining compatibility with older
# eligibility/root-level config layouts. This keeps R/39 aligned with R/25
# and other modules that treat research_question as the primary PICO/PICOS
# source of truth.
context_dimension_specs <- list(
  Population = c(
    "research_question.population",
    "eligibility.population",
    "population"
  ),
  Intervention = c(
    "research_question.intervention",
    "eligibility.intervention",
    "intervention"
  ),
  Comparator = c(
    "research_question.comparator",
    "eligibility.comparator",
    "comparator"
  ),
  Exposure = c(
    "research_question.exposure",
    "eligibility.exposure",
    "exposure"
  ),
  `Index test` = c(
    "research_question.index_test",
    "eligibility.index_test",
    "index_test"
  ),
  `Reference standard` = c(
    "research_question.reference_standard",
    "eligibility.reference_standard",
    "reference_standard"
  ),
  `Prognostic factor` = c(
    "research_question.prognostic_factor",
    "eligibility.prognostic_factor",
    "prognostic_factor"
  ),
  `Primary outcomes` = c(
    "research_question.outcomes.primary",
    "outcomes.primary",
    "primary_outcomes"
  ),
  `Secondary outcomes` = c(
    "research_question.outcomes.secondary",
    "outcomes.secondary",
    "secondary_outcomes"
  ),
  `Study design` = c(
    "research_question.study_design",
    "research_question.study_designs",
    "eligibility.study_design",
    "eligibility.study_designs"
  )
)

context_dimension_lines <- vapply(
  names(context_dimension_specs),
  function(label) {
    value <- get_config_text(
      context_dimension_specs[[label]]
    )

    if (!nzchar(value)) {
      return("")
    }

    paste0(
      label,
      ": ",
      value
    )
  },
  character(1)
)

context_dimension_lines <- context_dimension_lines[
  nzchar(context_dimension_lines)
]

search_context <- paste(
  c(
    paste0(
      "Review title: ",
      project_title
    ),
    context_dimension_lines
  ),
  collapse = "\n"
)


# ============================================================
# 7. Agent-generated PubMed search plan
# ============================================================

search_system <- paste(
  "You are a systematic-review manuscript literature-search planner.",
  "Create PubMed queries for the Introduction and Discussion of a scientific manuscript.",
  "The search is NOT the systematic-review eligibility search; it is a manuscript-context literature review.",
  "Cover the domain background and burden/context as applicable, authoritative standards/guidelines or consensus when relevant, evolution of the relevant intervention/exposure/test/prognostic construct/method, primary evidence of the design appropriate to this review, important outcomes or associations, prior systematic reviews/meta-analyses, and unresolved evidence gaps.",
  "Do not force an intervention-comparison search structure when the project concerns diagnosis, prognosis, prevalence, exposure, etiology, correlation, methodology, or another review architecture.",
  "Include both recent literature and landmark literature.",
  "Do not invent citations.",
  "Return STRICT JSON only."
)

search_user <- paste0(
  search_context,
  "\n\n",
  "Generate exactly ",
  literature_query_count,
  " complementary PubMed queries.\n",
  "At least one query should emphasize recent literature from approximately the last ",
  search_recent_years,
  " years, and at least one should permit landmark older studies.\n",
  "Return JSON with this shape:\n",
  "{\n",
  '  "queries": [\n',
  '    {"query_id":"Q1","purpose":"...","query":"...","section":"Introduction|Discussion|Both"}\n',
  "  ]\n",
  "}\n",
  "Use PubMed-compatible Boolean syntax. Do not include commentary outside JSON."
)

search_plan_text <- call_openai(
  search_system,
  search_user,
  "01_literature_search_plan",
  max_output_tokens = 5000L
)

search_plan_text <- strip_markdown_fence(
  search_plan_text
)

search_plan <- tryCatch(
  jsonlite::fromJSON(
    search_plan_text,
    simplifyVector = TRUE
  ),
  error = function(e) NULL
)

if (
  is.null(search_plan) ||
  is.null(search_plan$queries)
) {
  stop(
    "Literature search-plan JSON could not be parsed.",
    call. = FALSE
  )
}

queries <- as.data.frame(
  search_plan$queries,
  stringsAsFactors = FALSE
)

required_query_cols <- c(
  "query_id",
  "purpose",
  "query",
  "section"
)

if (
  !all(
    required_query_cols %in%
      names(queries)
  )
) {
  stop(
    "Search-plan JSON is missing required fields.",
    call. = FALSE
  )
}

queries <- queries |>
  dplyr::filter(
    !is.na(query),
    nzchar(trimws(query))
  ) |>
  dplyr::distinct(
    query,
    .keep_all = TRUE
  )

if (nrow(queries) == 0L) {
  stop(
    "No usable PubMed query was generated.",
    call. = FALSE
  )
}

jsonlite::write_json(
  list(
    project_id = project_id,
    project_title = project_title,
    generated_at = now_text(),
    queries = queries
  ),
  search_plan_file,
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)


# ============================================================
# 8. PubMed ESearch
# ============================================================

ncbi_base <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils"

ncbi_request <- function(
    endpoint,
    query
) {
  req <- httr2::request(
    paste0(
      ncbi_base,
      "/",
      endpoint
    )
  ) |>
    httr2::req_url_query(
      !!!query
    ) |>
    httr2::req_user_agent(
      if (nzchar(ncbi_email)) {
        paste0(
          "Universal-SRMA-Toolkit/1.0 (",
          ncbi_email,
          ")"
        )
      } else {
        "Universal-SRMA-Toolkit/1.0"
      }
    ) |>
    httr2::req_retry(
      max_tries = 3
    ) |>
    httr2::req_timeout(
      seconds = 60
    )

  response <- httr2::req_perform(
    req
  )

  if (nzchar(ncbi_api_key)) {
    Sys.sleep(0.12)
  } else {
    Sys.sleep(0.36)
  }

  response
}


pubmed_search <- function(
    query_text,
    retmax
) {
  params <- list(
    db = "pubmed",
    term = query_text,
    retmode = "json",
    retmax = as.integer(retmax),
    sort = "relevance"
  )

  if (nzchar(ncbi_email)) {
    params$email <- ncbi_email
  }

  if (nzchar(ncbi_api_key)) {
    params$api_key <- ncbi_api_key
  }

  response <- ncbi_request(
    "esearch.fcgi",
    params
  )

  body <- httr2::resp_body_json(
    response,
    simplifyVector = TRUE
  )

  result <- body$esearchresult

  if (is.null(result)) {
    stop(
      "PubMed ESearch returned no esearchresult.",
      call. = FALSE
    )
  }

  ids <- as.character(
    result$idlist
  )

  ids <- ids[
    !is.na(ids) &
      nzchar(ids)
  ]

  list(
    count = suppressWarnings(
      as.integer(
        result$count
      )
    ),
    ids = ids,
    query_translation = safe_chr(
      result$querytranslation,
      ""
    )
  )
}


search_log_rows <- list()
query_pmids <- list()

for (i in seq_len(nrow(queries))) {
  q <- queries$query[[i]]

  result <- tryCatch(
    pubmed_search(
      q,
      pubmed_retmax_per_query
    ),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    search_log_rows[[
      length(search_log_rows) + 1L
    ]] <- tibble::tibble(
      query_id = queries$query_id[[i]],
      purpose = queries$purpose[[i]],
      section = queries$section[[i]],
      query = q,
      pubmed_count = NA_integer_,
      retrieved_pmids = 0L,
      query_translation = "",
      status = "FAIL",
      message = conditionMessage(result)
    )
    next
  }

  query_pmids[[
    queries$query_id[[i]]
  ]] <- result$ids

  search_log_rows[[
    length(search_log_rows) + 1L
  ]] <- tibble::tibble(
    query_id = queries$query_id[[i]],
    purpose = queries$purpose[[i]],
    section = queries$section[[i]],
    query = q,
    pubmed_count = result$count,
    retrieved_pmids = length(result$ids),
    query_translation = result$query_translation,
    status = "PASS",
    message = ""
  )
}

search_log <- dplyr::bind_rows(
  search_log_rows
)

readr::write_csv(
  search_log,
  search_log_file,
  na = ""
)

all_pmids <- unique(
  unlist(
    query_pmids,
    use.names = FALSE
  )
)

all_pmids <- all_pmids[
  !is.na(all_pmids) &
    nzchar(all_pmids)
]

if (length(all_pmids) == 0L) {
  stop(
    "PubMed literature search retrieved no PMIDs.",
    call. = FALSE
  )
}


# ============================================================
# 9. Add included-study PMIDs when available
# ============================================================

# Keep an explicit universal set of included-study PMIDs. This allows later
# drafting/repair stages to distinguish current-review studies from truly
# external contextual literature and prevents importing extra outcomes from an
# included publication when those outcomes are absent from frozen R/38 evidence.
included_pmids <- character(0)

included_candidates <- c(
  file.path(
    repo_root,
    "data",
    "processed",
    "full_text",
    "included_studies.csv"
  ),
  file.path(
    production_root,
    "evidence_package",
    "study_selection",
    "included_studies.csv"
  )
)

included_candidate_ok <- file_nonempty(
  included_candidates
)

included_file <- included_candidates[
  included_candidate_ok
]

if (length(included_file) > 0L) {
  included_data <- tryCatch(
    readr::read_csv(
      included_file[[1]],
      show_col_types = FALSE,
      progress = FALSE
    ),
    error = function(e) NULL
  )

  if (!is.null(included_data)) {
    pmid_col <- first_existing_col(
      included_data,
      c(
        "pmid",
        "PMID",
        "pubmed_id"
      )
    )

    if (!is.na(pmid_col)) {
      included_pmids <- trimws(
        as.character(
          included_data[[pmid_col]]
        )
      )

      included_pmids <- included_pmids[
        !is.na(included_pmids) &
          grepl(
            "^[0-9]+$",
            included_pmids
          )
      ]

      all_pmids <- unique(
        c(
          included_pmids,
          all_pmids
        )
      )
    }
  }
}

if (
  length(all_pmids) >
    literature_candidate_target
) {
  all_pmids <- all_pmids[
    seq_len(
      literature_candidate_target
    )
  ]
}


# ============================================================
# 10. PubMed EFetch + verified bibliographic parsing
# ============================================================

pubmed_fetch_xml <- function(pmids) {
  params <- list(
    db = "pubmed",
    id = paste(
      pmids,
      collapse = ","
    ),
    retmode = "xml"
  )

  if (nzchar(ncbi_email)) {
    params$email <- ncbi_email
  }

  if (nzchar(ncbi_api_key)) {
    params$api_key <- ncbi_api_key
  }

  response <- ncbi_request(
    "efetch.fcgi",
    params
  )

  httr2::resp_body_string(
    response
  )
}


xml_text_one <- function(
    node,
    xpath
) {
  hit <- xml2::xml_find_first(
    node,
    xpath
  )

  if (
    inherits(hit, "xml_missing")
  ) {
    return("")
  }

  trimws(
    xml2::xml_text(
      hit
    )
  )
}


xml_text_all <- function(
    node,
    xpath,
    sep = "; "
) {
  hits <- xml2::xml_find_all(
    node,
    xpath
  )

  if (length(hits) == 0L) {
    return("")
  }

  values <- trimws(
    xml2::xml_text(
      hits
    )
  )

  values <- values[
    !is.na(values) &
      nzchar(values)
  ]

  paste(
    values,
    collapse = sep
  )
}


parse_pubmed_articles <- function(xml_text) {
  doc <- xml2::read_xml(
    xml_text
  )

  articles <- xml2::xml_find_all(
    doc,
    ".//PubmedArticle"
  )

  if (length(articles) == 0L) {
    return(
      tibble::tibble()
    )
  }

  rows <- lapply(
    seq_along(articles),
    function(i) {
      node <- articles[[i]]

      pmid <- xml_text_one(
        node,
        ".//MedlineCitation/PMID"
      )

      title <- xml_text_one(
        node,
        ".//Article/ArticleTitle"
      )

      journal <- xml_text_one(
        node,
        ".//Article/Journal/Title"
      )

      journal_abbrev <- xml_text_one(
        node,
        ".//Article/Journal/ISOAbbreviation"
      )

      year <- xml_text_one(
        node,
        ".//Article/Journal/JournalIssue/PubDate/Year"
      )

      if (!nzchar(year)) {
        medline_date <- xml_text_one(
          node,
          ".//Article/Journal/JournalIssue/PubDate/MedlineDate"
        )

        year_match <- stringr::str_extract(
          medline_date,
          "\\b(19|20)[0-9]{2}\\b"
        )

        year <- safe_chr(
          year_match,
          ""
        )
      }

      volume <- xml_text_one(
        node,
        ".//Article/Journal/JournalIssue/Volume"
      )

      issue <- xml_text_one(
        node,
        ".//Article/Journal/JournalIssue/Issue"
      )

      pages <- xml_text_one(
        node,
        ".//Article/Pagination/MedlinePgn"
      )

      abstract_parts <- xml2::xml_find_all(
        node,
        ".//Article/Abstract/AbstractText"
      )

      abstract <- ""

      if (length(abstract_parts) > 0L) {
        part_text <- vapply(
          abstract_parts,
          function(part) {
            label <- xml2::xml_attr(
              part,
              "Label"
            )

            txt <- trimws(
              xml2::xml_text(
                part
              )
            )

            if (
              !is.na(label) &&
              nzchar(label)
            ) {
              paste0(
                label,
                ": ",
                txt
              )
            } else {
              txt
            }
          },
          character(1)
        )

        abstract <- paste(
          part_text,
          collapse = " "
        )
      }

      author_nodes <- xml2::xml_find_all(
        node,
        ".//Article/AuthorList/Author"
      )

      authors <- character(0)

      if (length(author_nodes) > 0L) {
        authors <- vapply(
          author_nodes,
          function(author) {
            collective <- xml_text_one(
              author,
              "./CollectiveName"
            )

            if (nzchar(collective)) {
              return(collective)
            }

            last <- xml_text_one(
              author,
              "./LastName"
            )

            initials <- xml_text_one(
              author,
              "./Initials"
            )

            trimws(
              paste(
                last,
                initials
              )
            )
          },
          character(1)
        )

        authors <- authors[
          nzchar(authors)
        ]
      }

      doi_nodes <- xml2::xml_find_all(
        node,
        ".//PubmedData/ArticleIdList/ArticleId[@IdType='doi']"
      )

      doi <- if (length(doi_nodes) > 0L) {
        trimws(
          xml2::xml_text(
            doi_nodes[[1]]
          )
        )
      } else {
        ""
      }

      publication_types <- xml_text_all(
        node,
        ".//Article/PublicationTypeList/PublicationType",
        sep = "; "
      )

      mesh_terms <- xml_text_all(
        node,
        ".//MedlineCitation/MeshHeadingList/MeshHeading/DescriptorName",
        sep = "; "
      )

      tibble::tibble(
        pmid = pmid,
        title = title,
        authors = paste(
          authors,
          collapse = "; "
        ),
        first_author = if (
          length(authors) > 0L
        ) {
          authors[[1]]
        } else {
          ""
        },
        journal = journal,
        journal_abbrev = journal_abbrev,
        year = year,
        volume = volume,
        issue = issue,
        pages = pages,
        doi = doi,
        publication_types = publication_types,
        mesh_terms = mesh_terms,
        abstract = abstract,
        pubmed_url = if (
          nzchar(pmid)
        ) {
          paste0(
            "https://pubmed.ncbi.nlm.nih.gov/",
            pmid,
            "/"
          )
        } else {
          ""
        },
        metadata_verified = nzchar(pmid) &&
          nzchar(title)
      )
    }
  )

  dplyr::bind_rows(
    rows
  )
}


candidate_rows <- list()

batch_size <- 100L

batches <- split(
  all_pmids,
  ceiling(
    seq_along(all_pmids) /
      batch_size
  )
)

for (batch in batches) {
  xml_text <- pubmed_fetch_xml(
    batch
  )

  parsed <- parse_pubmed_articles(
    xml_text
  )

  if (nrow(parsed) > 0L) {
    candidate_rows[[
      length(candidate_rows) + 1L
    ]] <- parsed
  }
}

candidates <- dplyr::bind_rows(
  candidate_rows
) |>
  dplyr::filter(
    metadata_verified
  ) |>
  dplyr::distinct(
    pmid,
    .keep_all = TRUE
  )

if (
  nrow(candidates) <
    minimum_unique_references
) {
  stop(
    "Verified PubMed candidate library contains only ",
    nrow(candidates),
    " unique references, below the required minimum of ",
    minimum_unique_references,
    ". Broaden the manuscript literature search before drafting.",
    call. = FALSE
  )
}


# ============================================================
# 11. Query provenance for each candidate
# ============================================================

query_membership <- lapply(
  candidates$pmid,
  function(pmid) {
    hits <- names(
      query_pmids
    )[
      vapply(
        query_pmids,
        function(ids) {
          pmid %in% ids
        },
        logical(1)
      )
    ]

    paste(
      hits,
      collapse = ";"
    )
  }
)

candidates$query_ids <- unlist(
  query_membership,
  use.names = FALSE
)

readr::write_csv(
  candidates,
  candidate_file,
  na = ""
)


# ============================================================
# 12. Literature-review selection agent
# ============================================================

candidate_prompt_rows <- vapply(
  seq_len(
    nrow(candidates)
  ),
  function(i) {
    row <- candidates[
      i,
      ,
      drop = FALSE
    ]

    abstract_excerpt <- safe_chr(
      row$abstract,
      ""
    )

    if (
      nchar(
        abstract_excerpt
      ) > 1600L
    ) {
      abstract_excerpt <- paste0(
        substr(
          abstract_excerpt,
          1,
          1600L
        ),
        "..."
      )
    }

    paste0(
      "PMID ",
      row$pmid,
      " | ",
      row$year,
      " | ",
      row$title,
      " | ",
      row$journal_abbrev,
      if (nzchar(row$doi)) {
        paste0(
          " | DOI ",
          row$doi
        )
      } else {
        ""
      },
      "\nAbstract: ",
      abstract_excerpt
    )
  },
  character(1)
)

selection_system <- paste(
  "You are the literature-review component of a systematic-review manuscript workflow.",
  "Select references ONLY from the supplied verified PubMed candidates.",
  "Do not create or alter PMIDs.",
  "Prioritize domain relevance, authoritative guidelines/consensus documents when applicable, landmark studies, recent high-quality primary studies of the design relevant to this review, previous systematic reviews/meta-analyses, and literature needed to interpret the current review.",
  "Do not preferentially select trials or comparative clinical studies unless that design matches the project question.",
  "Avoid filling the library with redundant low-value papers.",
  "Return STRICT JSON only."
)

selection_user <- paste0(
  search_context,
  "\n\nCURRENT SR-MA FACTUAL RESULTS:\n",
  results_factual_blocks,
  "\n\nVERIFIED PUBMED CANDIDATES:\n\n",
  paste(
    candidate_prompt_rows,
    collapse = "\n\n"
  ),
  "\n\nSelect approximately ",
  literature_selected_target,
  " references, but never fewer than ",
  minimum_unique_references,
  " if that many relevant references are available.\n",
  "Every selected item MUST use a PMID from the supplied list.\n",
  "Assign relevance for Introduction, Discussion, or Both and a short rationale.\n",
  "Return JSON exactly in this shape:\n",
  "{\n",
  '  "selected": [\n',
  '    {"pmid":"12345678","section":"Introduction|Discussion|Both","priority":"core|supporting","rationale":"..."}\n',
  "  ]\n",
  "}\n"
)

selection_text <- call_openai(
  selection_system,
  selection_user,
  "02_literature_selection",
  max_output_tokens = 10000L
)

selection_text <- strip_markdown_fence(
  selection_text
)

selection_json <- tryCatch(
  jsonlite::fromJSON(
    selection_text,
    simplifyVector = TRUE
  ),
  error = function(e) NULL
)

if (
  is.null(selection_json) ||
  is.null(selection_json$selected)
) {
  stop(
    "Literature-selection JSON could not be parsed.",
    call. = FALSE
  )
}

selected_agent <- as.data.frame(
  selection_json$selected,
  stringsAsFactors = FALSE
)

required_selection_cols <- c(
  "pmid",
  "section",
  "priority",
  "rationale"
)

if (
  !all(
    required_selection_cols %in%
      names(selected_agent)
  )
) {
  stop(
    "Literature-selection output lacks required fields.",
    call. = FALSE
  )
}

selected_agent$pmid <- trimws(
  as.character(
    selected_agent$pmid
  )
)

invalid_selected <- setdiff(
  selected_agent$pmid,
  candidates$pmid
)

if (length(invalid_selected) > 0L) {
  stop(
    "Literature-selection agent returned PMID(s) not present in verified PubMed candidates: ",
    paste(
      invalid_selected,
      collapse = ", "
    ),
    call. = FALSE
  )
}

selected_agent <- selected_agent |>
  dplyr::distinct(
    pmid,
    .keep_all = TRUE
  )

if (
  nrow(selected_agent) <
    minimum_unique_references
) {
  stop(
    "Literature-selection agent selected only ",
    nrow(selected_agent),
    " references; minimum required is ",
    minimum_unique_references,
    ".",
    call. = FALSE
  )
}

selected <- selected_agent |>
  dplyr::left_join(
    candidates,
    by = "pmid"
  ) |>
  dplyr::mutate(
    verified_for_manuscript = metadata_verified
  )

if (
  any(
    !selected$verified_for_manuscript
  )
) {
  stop(
    "Selected literature contains an unverified reference.",
    call. = FALSE
  )
}

readr::write_csv(
  selected,
  selected_file,
  na = ""
)

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb,
  "Selected_Literature"
)

openxlsx::writeData(
  wb,
  "Selected_Literature",
  selected
)

openxlsx::addWorksheet(
  wb,
  "Search_Queries"
)

openxlsx::writeData(
  wb,
  "Search_Queries",
  queries
)

openxlsx::addWorksheet(
  wb,
  "PubMed_Search_Log"
)

openxlsx::writeData(
  wb,
  "PubMed_Search_Log",
  search_log
)

openxlsx::saveWorkbook(
  wb,
  selected_xlsx,
  overwrite = TRUE
)


# ============================================================
# 13. Create literature library supplied to drafting agents
# ============================================================

library_rows <- vapply(
  seq_len(
    nrow(selected)
  ),
  function(i) {
    row <- selected[
      i,
      ,
      drop = FALSE
    ]

    abstract_excerpt <- safe_chr(
      row$abstract,
      ""
    )

    if (
      nchar(
        abstract_excerpt
      ) > 2200L
    ) {
      abstract_excerpt <- paste0(
        substr(
          abstract_excerpt,
          1,
          2200L
        ),
        "..."
      )
    }

    included_flag <- if (
      safe_chr(row$pmid, "") %in% included_pmids
    ) {
      "YES"
    } else {
      "NO"
    }

    paste0(
      "[[PMID:",
      row$pmid,
      "]]\n",
      "Included in current review: ",
      included_flag,
      "\n",
      "Title: ",
      row$title,
      "\n",
      "Authors: ",
      row$authors,
      "\n",
      "Journal: ",
      row$journal,
      "\n",
      "Year: ",
      row$year,
      "\n",
      "Publication type: ",
      row$publication_types,
      "\n",
      "Section relevance: ",
      row$section,
      "\n",
      "Priority: ",
      row$priority,
      "\n",
      "Selection rationale: ",
      row$rationale,
      "\n",
      "Abstract: ",
      abstract_excerpt
    )
  },
  character(1)
)

verified_library_text <- paste(
  library_rows,
  collapse = "\n\n-----------------------------\n\n"
)


# ============================================================
# 14. Draft Introduction
# ============================================================

intro_system <- paste(
  "You are drafting the Introduction of a peer-reviewed systematic review and meta-analysis manuscript.",
  "Use ONLY the verified literature records supplied in the prompt for external factual claims.",
  "Cite sources using the exact token format [[PMID:12345678]].",
  "Never invent, alter, or cite a PMID not supplied.",
  "Every substantive literature-dependent statement should have an appropriate citation.",
  "Do not describe the current meta-analysis results in the Introduction.",
  "Write concise scholarly English suitable for a peer-reviewed journal in the project domain.",
  "No reference list; only PMID citation tokens."
)

intro_user <- paste0(
  "PROJECT CONTEXT:\n",
  search_context,
  "\n\n",
  "TARGET JOURNAL: ",
  if (nzchar(target_journal)) {
    target_journal
  } else {
    "[not specified]"
  },
  "\n\n",
  "VERIFIED LITERATURE LIBRARY:\n",
  verified_library_text,
  "\n\n",
  "Write the Introduction in approximately 4-6 paragraphs.",
  " Establish the domain-specific problem or evidence context, define the relevant population/condition/exposure/intervention/test/prognostic factor as applicable to THIS project, summarize the current evidence landscape and unresolved gap, and end with the objective of this SR-MA.",
  " Do not force a treatment-comparison or clinical-intervention framing when the project concerns diagnosis, prognosis, prevalence, exposure, etiology, methodology, or another review type.",
  " Cite at least ",
  minimum_intro_references,
  " UNIQUE verified PMIDs across the Introduction.",
  " Use only [[PMID:...]] tokens from the supplied library."
)

intro_text <- call_openai(
  intro_system,
  intro_user,
  "03_introduction",
  max_output_tokens = 10000L
)

intro_text <- normalize_publication_text(
  strip_markdown_fence(
    intro_text
  )
)

write_text_file(
  intro_text,
  intro_file
)


# ============================================================
# 15. Draft Methods from R/38 only
# ============================================================

methods_system <- paste(
  "You are drafting the Methods section of a peer-reviewed systematic review and meta-analysis.",
  "Use ONLY the supplied frozen R/38 factual blocks and project information.",
  "Do not add methods that are not explicitly supported by the supplied configuration or executed evidence provenance.",
  "Translate internal provenance into publication-facing methodology. Never expose toolkit module names such as R/27, R/28, or R/28B; workflow state labels; artifact IDs; screening IDs; or internal phrases such as canonical output or Human Verification in manuscript prose.",
  "Do not invent registration IDs, dates, reviewers, software versions, thresholds, transformations, continuity corrections, subgroup analyses, sensitivity analyses, publication-bias tests, or meta-regressions.",
  "The Statistical analysis subsection must be publication-manuscript quality and comparable in completeness to a strong peer-reviewed meta-analysis in the project domain.",
  "It should explicitly cover, when supported: executed analysis family (binary, continuous, time-to-event, rate, proportion/prevalence, correlation, diagnostic-accuracy, or generic effect estimate); effect measure/transformation; pooling model; between-study variance estimator; confidence-interval or Hartung-Knapp/Knapp-Hartung inference; heterogeneity statistics; sensitivity/diagnostic analyses; publication-bias procedures and minimum-k eligibility; meta-regression/moderator procedures; and publication-appropriate software or analytical implementation details when explicitly supported.",
  "If project_config.yml is silent but executed evidence outputs explicitly document a method, use the executed evidence as the source of truth while describing the method in normal scholarly language.",
  "Never write the literal placeholder [CONFIG NOT SPECIFIED].",
  "If an optional method is absent from all canonical executed evidence, omit that method rather than guessing.",
  "Do not report analyses that were only considered but not executed.",
  "Preserve the meaning of exploratory, underpowered, descriptive-only, and not-estimable classifications, but render internal machine-style labels such as DESCRIPTIVEONLY or NOT_ESTIMABLE as normal scholarly prose.",
  "Write polished scholarly English with enough methodological detail for reproducibility, but avoid padding unsupported content.",
  "Do not cite external literature unless explicitly present in the supplied factual blocks.",
  "Do not assign Table or Figure numbers; the final publication-export layer owns all display numbering and cross-references.",
  "Use publication-safe plain Unicode for simple statistical notation (for example τ² and I²); do not emit LaTeX delimiters; use plain Unicode statistical notation instead."
)

methods_user <- paste0(
  "PROJECT TITLE:\n",
  project_title,
  "\n\n",
  "R/38 METHODS EVIDENCE DOSSIER:\n",
  methods_factual_blocks,
  "\n\n",
  "Draft a complete Methods section with appropriate subheadings.\n",
  "The Statistical analysis subsection is a priority: synthesize ALL actually executed statistical variables and methods exposed by the dossier, including supported model and diagnostic details, without exposing internal provenance labels or toolkit module names.\n",
  "When several analysis groups used the same method, describe the common method once and note exceptions only when supported.\n",
  "Do not reproduce raw console output or evidence tables in the final prose.\n",
  "Do not leave [CONFIG NOT SPECIFIED] in the manuscript."
)

methods_text <- call_openai(
  methods_system,
  methods_user,
  "04_methods",
  max_output_tokens = 16000L
)

methods_text <- normalize_publication_text(
  strip_markdown_fence(
    methods_text
  )
)

write_text_file(
  methods_text,
  methods_file
)


# ============================================================
# 16. Draft Results from R/38 only
# ============================================================

results_system <- paste(
  "You are drafting the Results section of a peer-reviewed systematic review and meta-analysis.",
  "Use ONLY the supplied frozen R/38 Results evidence dossier and artifact registry.",
  "Write publication-facing prose. Never expose toolkit module names (for example R/27 or R/28B), workflow state labels, artifact IDs, screening IDs, or internal phrases such as canonical output or Human Verification in the visible manuscript. Coverage tokens are the only internal tokens allowed here, solely because they are stripped later by R/40.",
  "Translate internal machine-style status labels such as DESCRIPTIVEONLY, NOTESTIMABLE, and NOT_ESTIMABLE into ordinary scholarly phrases such as descriptive only or not estimable.",
  "Do not recalculate statistics and do not manufacture totals, ranges, P values, subgroup effects, or sensitivity results.",
  "Do not create any arithmetic-derived current-review value that is not explicitly stated in the supplied evidence dossier. In particular, never sum subgroup or arm counts to create a new total, never derive a percentage, mean, ratio, range, or rounded estimate from other reported numbers, and never combine denominators across studies. If a useful aggregate is not literally present in the dossier, report the component counts or describe the evidence without that aggregate.",
  "Do not change numerical precision beyond normal sentence formatting.",
  "This is a UNIVERSAL drafting task: do not assume that the review concerns treatments, patients, procedures, or a pairwise intervention comparison unless the evidence dossier explicitly does so.",
  "Adapt terminology to the actual review architecture (for example intervention/comparator, exposure/outcome, index test/reference standard, prognostic factor/outcome, prevalence/proportion, rate, correlation, time-to-event, or generic effect estimate).",
  "The Results must be artifact-aware. Every semantic evidence unit represented by an artifact marked coverage_required=TRUE must be substantively represented in the narrative.",
  "Immediately after the paragraph or sentence cluster that covers a required artifact, reproduce its exact HTML coverage token, for example <!-- ARTIFACT: A0001 -->. Do not alter, merge, or invent artifact IDs.",
  "For descriptive/reporting tables, summarize the most informative study-level patterns supported by the columns actually present. Do not force participant, intervention, procedure, diagnostic, exposure, or prognostic terminology unless those concepts exist in that table.",
  "For quantitative syntheses, report the number of studies and sample size when explicitly available, the executed effect measure and pooled estimate, 95% CI or other reported interval, overall-effect P value when available, and heterogeneity metrics when available.",
  "For forest plots, describe study-level direction, precision, and consistency only from supplied backing data; never infer values by visually reading the image.",
  "For sensitivity, leave-one-out, publication-bias, meta-regression, moderator, influence, or other diagnostics, describe only analyses actually executed and preserve their scientific eligibility/status meaning in normal scholarly prose.",
  "For risk of bias, summarize only author-approved outputs and use the terminology of the actual RoB tool present.",
  "Risk-of-bias prose must be evidence-selective: describe a domain as a concern only when the final author-approved assessment actually shows a non-low/non-clean judgement for at least one relevant study. Do not enumerate generic tool domains merely because they exist in the instrument.",
  "Never identify risk-of-bias domains by bare numeric labels (for example domain 1, domains 1-4, or domains 5 and 6). Numeric domain identifiers are internal assessment coordinates, not scientific numeric results. Prefer named domains when explicitly supported; otherwise report only the overall judgement and a non-numeric summary such as several domains.",
  "When reporting study-selection or retrieval flow, distinguish PRISMA accounting categories from physical-document availability. A report that is not classified as 'not retrieved' must not automatically be described as having an available PDF or full-text file when the evidence dossier documents metadata-only assessment, missing PDF, abstract-only evidence, registry data, or another non-PDF source.",
  "Prefer human-readable study labels (author/year, citation, or study label) in manuscript prose. Do not expose internal machine identifiers, artifact IDs, database keys, or screening IDs unless no human-readable identifier exists and the identifier is scientifically necessary.",
  "Statistical interpretation in Results is limited to what the evidence directly supports: direction, interval precision, null crossing where appropriate for the effect measure, heterogeneity, and robustness to executed diagnostics.",
  "Do not provide mechanistic or causal explanations in Results.",
  "Do not use superiority, equivalence, noninferiority, causality, diagnostic utility, prognostic importance, or absence-of-effect language unless the supplied evidence and design justify it.",
  "If a registry artifact is supplementary and coverage_required=FALSE, it may be omitted unless it materially helps explain a reported analysis.",
  "Do not assign or cite literal Table/Figure numbers in Results. Coverage tokens identify artifacts internally; R/41 will create the final numbered cross-references after R/40 validation.",
  "Use publication-safe plain Unicode for simple statistical notation (for example τ² and I²); do not emit LaTeX delimiters.",
  "Write polished scholarly English with a level of detail comparable to a strong peer-reviewed meta-analysis in the relevant domain."
)

results_user <- paste0(
  "PROJECT TITLE:\n",
  project_title,
  "\n\n",
  "R/38 STATUS:\n",
  readiness_status,
  "\n\n",
  "REQUIRED ARTIFACT COVERAGE CATALOG:\n",
  artifact_catalog_text,
  "\n\n",
  "R/38 RESULTS EVIDENCE DOSSIER:\n",
  results_factual_blocks,
  "\n\n",
  "Draft a complete Results section with subheadings determined by the evidence itself, not by a fixed clinical template.\n",
  "Before writing, silently inventory study-selection evidence, every coverage-required table/figure, every executed quantitative analysis family, diagnostics/sensitivity results, and final risk-of-bias evidence.\n",
  "Every coverage-required semantic evidence unit must have its representative artifact's exact coverage token placed immediately after the prose that represents it. Multiple tokens may follow the same paragraph when that paragraph genuinely covers multiple evidence units.\n",
  "Do not merely list artifacts: explain the substantive findings they support, without adding unsupported inference.\n",
  "Every quantitative claim must remain traceable to the supplied dossier."
)

results_text <- call_openai(
  results_system,
  results_user,
  "05_results",
  max_output_tokens = 18000L
)

results_text <- normalize_publication_text(
  strip_markdown_fence(
    results_text
  )
)

required_artifact_ids <- required_coverage_artifacts$artifact_id
present_artifact_ids <- extract_artifact_markers(
  results_text
)
missing_artifact_ids <- artifact_ids_needing_results_coverage(
  results_text,
  required_coverage_artifacts
)

if (length(missing_artifact_ids) > 0L) {
  missing_catalog <- required_coverage_artifacts |>
    dplyr::filter(
      artifact_id %in% missing_artifact_ids
    )

  repair_user <- paste0(
    "The draft Results below omitted required artifact coverage markers. Revise the Results so that every missing artifact is substantively represented using ONLY the supplied evidence dossier. Preserve all correct existing numerical statements. Do not add new statistics. Place each exact token immediately after the prose covering that artifact.\n\n",
    "MISSING ARTIFACTS:\n",
    paste(
      paste0(
        missing_catalog$coverage_token,
        " | ",
        missing_catalog$title,
        " | role=",
        missing_catalog$manuscript_role,
        " | source=",
        missing_catalog$source_relative_path
      ),
      collapse = "\n"
    ),
    "\n\nCURRENT RESULTS DRAFT:\n",
    results_text,
    "\n\nR/38 RESULTS EVIDENCE DOSSIER:\n",
    results_factual_blocks
  )

  results_text <- call_openai(
    results_system,
    repair_user,
    "05b_results_artifact_coverage_repair",
    max_output_tokens = 18000L
  )

  results_text <- normalize_publication_text(
    strip_markdown_fence(
      results_text
    )
  )

  present_artifact_ids <- extract_artifact_markers(
    results_text
  )

  missing_artifact_ids <- artifact_ids_needing_results_coverage(
    results_text,
    required_coverage_artifacts
  )
}

# Conservative evidence-conformance pass after artifact-coverage repair. This is
# intentionally post-generation so stochastic drafting cannot silently broaden
# population scope, infer procedural feasibility, or expose numeric RoB domain IDs.
results_text <- repair_section_for_evidence_conformance(
  section_name = "Results",
  section_text = results_text,
  evidence_text = results_factual_blocks,
  preserve_artifact_tokens = TRUE
)

present_artifact_ids <- extract_artifact_markers(
  results_text
)
missing_artifact_ids <- artifact_ids_needing_results_coverage(
  results_text,
  required_coverage_artifacts
)

if (rob_numeric_domain_leakage(results_text)) {
  stop(
    "R/39 evidence-conformance gate failed: numbered risk-of-bias domain identifiers remain in Results.",
    call. = FALSE
  )
}

if (length(missing_artifact_ids) > 0L) {
  stop(
    paste0(
      "R/39 Results artifact-coverage gate failed. Missing or non-substantively covered artifact ID(s): ",
      paste(
        missing_artifact_ids,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

write_text_file(
  results_text,
  results_file
)


# ============================================================
# 17. Draft Discussion using Results + verified literature
# ============================================================

discussion_system <- paste(
  "You are drafting the Discussion of a peer-reviewed systematic review and meta-analysis.",
  "The current review's findings must come ONLY from the supplied R/38 Results factual blocks.",
  "For current-review findings, use only numerical values that are explicitly stated in those factual blocks. Do not calculate or infer new totals, pooled sample sizes, percentages, averages, ratios, ranges, or rounded values from component numbers. In particular, do not sum treatment-arm counts or study denominators; if an aggregate is not literally present, state the component values or use non-numeric wording.",
  "Translate all internal evidence provenance into publication-facing prose. Do not expose toolkit module names, workflow state labels, artifact or screening identifiers, or machine-style labels in the Discussion.",
  "External comparison/interpretation must use ONLY the verified literature library.",
  "Each verified literature record is explicitly marked Included in current review: YES or NO. A record marked YES is NOT independent external context. Do not import any additional outcome, direction, significance test, procedural-success result, safety result, or other finding from such an included publication unless that exact finding is also present in the frozen R/38 Results factual blocks.",
  "Do not generalize a narrower review-level population label to every included study when the frozen tables show a broader or mixed population in any study. Preserve study-specific population scope and qualify the review-level summary accordingly.",
  "Do not conclude that the current review demonstrates technical feasibility, technical success, procedural feasibility, procedural success, safety, or effectiveness unless the frozen R/38 Results factual blocks contain direct canonical endpoint evidence supporting that conclusion. Mere performance of a procedure is not sufficient evidence of feasibility or success.",
  "When discussing risk of bias, never use bare numeric domain identifiers. Use domain names only when explicitly supplied in the frozen evidence; otherwise summarize the overall judgement without numbered-domain detail.",
  "Use exact citation tokens [[PMID:12345678]].",
  "Never invent or alter PMIDs.",
  "Clearly distinguish the current review's findings from prior literature.",
  "Do not overstate null findings when confidence intervals are wide.",
  "Do not infer causality from observational evidence.",
  "Address domain-appropriate implications (clinical, diagnostic, prognostic, epidemiologic, methodological, policy, or scientific as applicable), uncertainty, heterogeneity where relevant, risk of bias, limitations, and research gaps.",
  "When discussing risk of bias, carry forward only concerns explicitly supported by the final author-approved RoB evidence. Never broaden the concern list by naming additional domains that were low/clean in all relevant studies.",
  "When discussing retrieval or evidence availability, preserve distinctions documented in the frozen evidence package (for example PRISMA not-retrieved status versus metadata-only assessment or missing PDF) rather than collapsing them into a stronger statement.",
  "If final RoB is pending, mark RoB-dependent interpretations as provisional.",
  "Do not assign or cite literal Table/Figure numbers; the publication-export layer owns final display numbering.",
  "Use normal scholarly prose rather than toolkit provenance vocabulary or machine-style status labels.",
  "Use publication-safe plain Unicode for simple statistical notation (for example τ² and I²).",
  "No reference list."
)

discussion_user <- paste0(
  "PROJECT CONTEXT:\n",
  search_context,
  "\n\n",
  "R/38 STATUS:\n",
  readiness_status,
  "\n\n",
  "CURRENT SR-MA RESULTS — CANONICAL FACTUAL BLOCKS:\n",
  results_factual_blocks,
  "\n\n",
  "VERIFIED EXTERNAL LITERATURE LIBRARY:\n",
  verified_library_text,
  "\n\n",
  "Write a comprehensive Discussion with these functions:",
  "\n1. principal findings;",
  "\n2. comparison with prior evidence;",
  "\n3. plausible interpretation without causal overreach;",
  "\n4. domain-appropriate implications;",
  "\n5. strengths;",
  "\n6. limitations;",
  "\n7. evidence gaps and future research;",
  "\n8. balanced concluding paragraph.",
  "\nCite at least ",
  minimum_discussion_references,
  " UNIQUE verified PMIDs in the Discussion.",
  "\nAcross Introduction + Discussion, the final manuscript must contain at least ",
  minimum_unique_references,
  " UNIQUE verified references."
)

discussion_text <- call_openai(
  discussion_system,
  discussion_user,
  "06_discussion",
  max_output_tokens = 16000L
)

discussion_text <- normalize_publication_text(
  strip_markdown_fence(
    discussion_text
  )
)

discussion_text <- repair_section_for_evidence_conformance(
  section_name = "Discussion",
  section_text = discussion_text,
  evidence_text = results_factual_blocks,
  literature_text = verified_library_text,
  preserve_artifact_tokens = FALSE
)

if (rob_numeric_domain_leakage(discussion_text)) {
  stop(
    "R/39 evidence-conformance gate failed: numbered risk-of-bias domain identifiers remain in Discussion.",
    call. = FALSE
  )
}

write_text_file(
  discussion_text,
  discussion_file
)


# ============================================================
# 18. Citation-token extraction and hard gate
# ============================================================

extract_pmids <- function(text) {
  matches <- stringr::str_match_all(
    text,
    "\\[\\[PMID:([0-9]+)\\]\\]"
  )[[1]]

  if (
    is.null(matches) ||
    nrow(matches) == 0L
  ) {
    return(character(0))
  }

  unique(
    matches[, 2]
  )
}

intro_pmids <- extract_pmids(
  intro_text
)

discussion_pmids <- extract_pmids(
  discussion_text
)

combined_pmids <- unique(
  c(
    intro_pmids,
    discussion_pmids
  )
)

invalid_intro <- setdiff(
  intro_pmids,
  selected$pmid
)

invalid_discussion <- setdiff(
  discussion_pmids,
  selected$pmid
)

if (
  length(invalid_intro) > 0L ||
  length(invalid_discussion) > 0L
) {
  stop(
    "Draft contains PMID citation(s) not present in the verified selected literature library.",
    call. = FALSE
  )
}

citation_gate_pass <- (
  length(intro_pmids) >=
    minimum_intro_references &&
  length(discussion_pmids) >=
    minimum_discussion_references &&
  length(combined_pmids) >=
    minimum_unique_references
)

if (!citation_gate_pass) {
  stop(
    paste0(
      "Reference gate FAILED. Introduction unique refs=",
      length(intro_pmids),
      " (minimum ",
      minimum_intro_references,
      "); Discussion unique refs=",
      length(discussion_pmids),
      " (minimum ",
      minimum_discussion_references,
      "); Combined unique refs=",
      length(combined_pmids),
      " (minimum ",
      minimum_unique_references,
      ")."
    ),
    call. = FALSE
  )
}


# ============================================================
# 19. Draft Abstract after main sections
# ============================================================

abstract_system <- paste(
  "You are drafting a structured abstract for a systematic review and meta-analysis.",
  "Use ONLY the supplied Introduction/Methods/Results/Discussion drafts.",
  "Do not introduce new numerical claims or new references.",
  "Do not perform arithmetic on numbers appearing in the supplied drafts. Never create a new total, percentage, pooled sample size, average, ratio, range, or rounded estimate by combining component values. A number may appear in the abstract only if that same scientific value is explicitly stated in at least one supplied draft.",
  "Keep conclusions proportional to uncertainty.",
  "Do not generalize a narrower review-level population label to every included study when the supplied Results show that one or more included studies used a broader or mixed population; preserve that qualification in the abstract summary.",
  "Do not state that the current review demonstrates technical feasibility, technical success, procedural feasibility, procedural success, safety, or effectiveness unless the supplied Results contain direct endpoint evidence for that conclusion.",
  "When summarizing risk of bias, do not use bare numeric domain identifiers; use only supported overall judgements or explicitly named domains.",
  "If final risk of bias is pending, keep the conclusion explicitly provisional.",
  "Use publication-facing language only: no toolkit module names, workflow-state labels, canonical-output terminology, Human Verification wording, artifact/screening identifiers, or Table/Figure numbers.",
  "Use publication-safe plain Unicode for simple statistical notation (for example τ² and I²); do not emit LaTeX delimiters.",
  "Return only the abstract text."
)

abstract_user <- paste0(
  "PROJECT TITLE:\n",
  project_title,
  "\n\n",
  "INTRODUCTION:\n",
  intro_text,
  "\n\n",
  "METHODS:\n",
  methods_text,
  "\n\n",
  "RESULTS:\n",
  results_text,
  "\n\n",
  "DISCUSSION:\n",
  discussion_text,
  "\n\n",
  "Write a structured abstract with Background, Methods, Results, and Conclusions.",
  " Do not include PMID tokens or a reference list."
)

abstract_text <- call_openai(
  abstract_system,
  abstract_user,
  "07_abstract",
  max_output_tokens = 5000L
)

abstract_text <- normalize_publication_text(
  strip_markdown_fence(
    abstract_text
  )
)

abstract_text <- repair_section_for_evidence_conformance(
  section_name = "Abstract",
  section_text = abstract_text,
  evidence_text = paste(
    methods_factual_blocks,
    results_factual_blocks,
    sep = "\n\n"
  ),
  preserve_artifact_tokens = FALSE
)

if (rob_numeric_domain_leakage(abstract_text)) {
  stop(
    "R/39 evidence-conformance gate failed: numbered risk-of-bias domain identifiers remain in Abstract.",
    call. = FALSE
  )
}

write_text_file(
  abstract_text,
  abstract_file
)


# ============================================================
# 20. Assemble token manuscript
# ============================================================

token_manuscript <- paste(
  c(
    paste0(
      "# ",
      project_title
    ),
    "",
    "## Abstract",
    "",
    abstract_text,
    "",
    "## Introduction",
    "",
    intro_text,
    "",
    "## Methods",
    "",
    methods_text,
    "",
    "## Results",
    "",
    results_text,
    "",
    "## Discussion",
    "",
    discussion_text
  ),
  collapse = "\n"
)

write_text_file(
  token_manuscript,
  token_manuscript_file
)


# ============================================================
# 21. Citation numbering in order of first appearance
# ============================================================

all_token_matches <- stringr::str_match_all(
  token_manuscript,
  "\\[\\[PMID:([0-9]+)\\]\\]"
)[[1]]

citation_order <- if (
  is.null(all_token_matches) ||
  nrow(all_token_matches) == 0L
) {
  character(0)
} else {
  unique(
    all_token_matches[, 2]
  )
}

citation_map <- tibble::tibble(
  citation_number = seq_along(
    citation_order
  ),
  pmid = citation_order
) |>
  dplyr::left_join(
    selected,
    by = "pmid"
  )

if (
  nrow(citation_map) <
    minimum_unique_references
) {
  stop(
    "Final citation numbering contains fewer than the required unique references.",
    call. = FALSE
  )
}

replace_tokens_with_numbers <- function(
    text,
    citation_map
) {
  out <- text

  for (
    i in seq_len(
      nrow(citation_map)
    )
  ) {
    token <- paste0(
      "[[PMID:",
      citation_map$pmid[[i]],
      "]]"
    )

    replacement <- paste0(
      "[",
      citation_map$citation_number[[i]],
      "]"
    )

    out <- stringr::str_replace_all(
      out,
      stringr::fixed(token),
      replacement
    )
  }

  out
}

numbered_manuscript <- replace_tokens_with_numbers(
  token_manuscript,
  citation_map
)

# R/41 is the only layer allowed to assign Table/Figure display numbers. R/39
# must also leave no internal workflow vocabulary or Markdown/LaTeX leakage in
# the visible manuscript body. Artifact coverage tokens are intentionally
# excluded from this gate because R/40 removes them after validation.
final_hygiene_failures <- publication_hygiene_issues(
  numbered_manuscript
)

if (length(final_hygiene_failures) > 0L) {
  stop(
    paste0(
      "R/39 publication-prose hygiene gate failed: ",
      paste(final_hygiene_failures, collapse = ", "),
      ". Regenerate R/39 with the current script before R/40 validation."
    ),
    call. = FALSE
  )
}


# ============================================================
# 22. Vancouver-style reference list
# ============================================================

format_author_list_vancouver <- function(
    author_string,
    max_authors = 6L
) {
  authors <- trimws(
    unlist(
      strsplit(
        safe_chr(
          author_string,
          ""
        ),
        ";",
        fixed = TRUE
      )
    )
  )

  authors <- authors[
    nzchar(authors)
  ]

  if (length(authors) == 0L) {
    return("")
  }

  if (length(authors) > max_authors) {
    paste0(
      paste(
        authors[
          seq_len(
            max_authors
          )
        ],
        collapse = ", "
      ),
      ", et al."
    )
  } else {
    paste(
      authors,
      collapse = ", "
    )
  }
}


format_vancouver_reference <- function(row) {
  authors <- format_author_list_vancouver(
    row$authors
  )

  journal_name <- safe_chr(
    row$journal_abbrev,
    safe_chr(
      row$journal,
      ""
    )
  )

  year <- safe_chr(
    row$year,
    ""
  )

  volume <- safe_chr(
    row$volume,
    ""
  )

  issue <- safe_chr(
    row$issue,
    ""
  )

  pages <- safe_chr(
    row$pages,
    ""
  )

  journal_part <- journal_name

  if (nzchar(year)) {
    journal_part <- paste0(
      journal_part,
      ". ",
      year
    )
  }

  if (nzchar(volume)) {
    journal_part <- paste0(
      journal_part,
      ";",
      volume
    )
  }

  if (nzchar(issue)) {
    journal_part <- paste0(
      journal_part,
      "(",
      issue,
      ")"
    )
  }

  if (nzchar(pages)) {
    journal_part <- paste0(
      journal_part,
      ":",
      pages
    )
  }

  journal_part <- paste0(
    journal_part,
    "."
  )

  doi_part <- if (
    nzchar(
      safe_chr(
        row$doi,
        ""
      )
    )
  ) {
    paste0(
      " doi:",
      row$doi,
      "."
    )
  } else {
    ""
  }

  pmid_part <- paste0(
    " PMID:",
    row$pmid,
    "."
  )

  paste0(
    authors,
    if (nzchar(authors)) {
      ". "
    } else {
      ""
    },
    row$title,
    ". ",
    journal_part,
    doi_part,
    pmid_part
  )
}


reference_lines <- vapply(
  seq_len(
    nrow(citation_map)
  ),
  function(i) {
    paste0(
      citation_map$citation_number[[i]],
      ". ",
      format_vancouver_reference(
        citation_map[
          i,
          ,
          drop = FALSE
        ]
      )
    )
  },
  character(1)
)

references_text <- paste(
  c(
    "## References",
    "",
    reference_lines
  ),
  collapse = "\n"
)

write_text_file(
  references_text,
  references_file
)

final_manuscript <- paste0(
  numbered_manuscript,
  "\n\n",
  references_text,
  "\n"
)

write_text_file(
  final_manuscript,
  final_manuscript_file
)

readr::write_csv(
  citation_map,
  citation_map_file,
  na = ""
)


# ============================================================
# 23. Citation audit
# ============================================================

citation_audit <- selected |>
  dplyr::transmute(
    pmid,
    title,
    year,
    doi,
    selected_section = section,
    priority,
    cited_in_introduction = pmid %in%
      intro_pmids,
    cited_in_discussion = pmid %in%
      discussion_pmids,
    cited_anywhere = pmid %in%
      citation_order,
    citation_number = match(
      pmid,
      citation_order
    ),
    metadata_verified = metadata_verified,
    pubmed_url
  )

readr::write_csv(
  citation_audit,
  citation_audit_file,
  na = ""
)


# ============================================================
# 24. Final manuscript guards
# ============================================================

unresolved_pmid_tokens <- stringr::str_detect(
  final_manuscript,
  "\\[\\[PMID:"
)

if (unresolved_pmid_tokens) {
  stop(
    "Final manuscript still contains unresolved PMID tokens.",
    call. = FALSE
  )
}

unique_cited_n <- sum(
  citation_audit$cited_anywhere
)

if (
  unique_cited_n <
    minimum_unique_references
) {
  stop(
    "Final manuscript does not satisfy the minimum verified-reference gate.",
    call. = FALSE
  )
}

uncited_selected_n <- sum(
  !citation_audit$cited_anywhere
)

draft_status <- if (
  identical(
    readiness_status,
    "FINAL_EVIDENCE_READY"
  )
) {
  "DRAFT_COMPLETE_READY_FOR_R40_VALIDATION"
} else {
  "PROVISIONAL_DRAFT_PENDING_FINAL_ROB"
}


# ============================================================
# 25. Drafting state
# ============================================================

state <- list(
  manuscript_drafting = list(
    version = "2.4.0",
    generated_at = now_text(),
    project_id = project_id,
    project_title = project_title,
    r38_readiness_status = readiness_status,
    drafting_status = draft_status,
    model = model_name,
    external_literature_source = "PubMed via NCBI E-utilities",
    references_invented_by_agent = FALSE,
    minimum_unique_references = minimum_unique_references,
    minimum_intro_references = minimum_intro_references,
    minimum_discussion_references = minimum_discussion_references,
    verified_candidate_references = nrow(
      candidates
    ),
    selected_verified_references = nrow(
      selected
    ),
    introduction_unique_references = length(
      intro_pmids
    ),
    discussion_unique_references = length(
      discussion_pmids
    ),
    final_unique_cited_references = unique_cited_n,
    citation_gate_pass = citation_gate_pass,
    manuscript_file = normalize_slash(
      final_manuscript_file
    ),
    citation_audit_file = normalize_slash(
      citation_audit_file
    ),
    selected_literature_file = normalize_slash(
      selected_file
    ),
    statistical_recalculation = FALSE,
    coverage_required_artifacts = length(required_artifact_ids),
    coverage_markers_present = length(
      intersect(
        required_artifact_ids,
        extract_artifact_markers(results_text)
      )
    ),
    coverage_artifacts_substantively_covered = length(required_artifact_ids) -
      length(missing_artifact_ids),
    artifact_coverage_gate_pass = length(missing_artifact_ids) == 0L
  )
)

yaml::write_yaml(
  state,
  state_file
)


# ============================================================
# 26. Report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/39 — Manuscript Drafting with Verified Literature",
  "",
  paste0(
    "Run time: ",
    now_text()
  ),
  paste0(
    "Project ID: ",
    project_id
  ),
  paste0(
    "Project title: ",
    project_title
  ),
  paste0(
    "Model: ",
    model_name
  ),
  "",
  "EVIDENCE CONTRACT",
  "--------------------------------------------------",
  paste0(
    "R/38 status: ",
    readiness_status
  ),
  "Methods/Results source: frozen R/38 evidence only",
  "External literature source: PubMed via NCBI E-utilities",
  "Statistical recalculation in R/39: NO",
  "Agent-generated references permitted: NO",
  "",
  "ARTIFACT COVERAGE",
  "--------------------------------------------------",
  paste0(
    "Coverage-required artifacts: ",
    length(required_artifact_ids)
  ),
  paste0(
    "Coverage markers present: ",
    length(
      intersect(
        required_artifact_ids,
        extract_artifact_markers(results_text)
      )
    )
  ),
  paste0(
    "Substantively covered artifacts: ",
    length(required_artifact_ids) - length(missing_artifact_ids)
  ),
  paste0(
    "Artifact coverage gate PASS: ",
    length(missing_artifact_ids) == 0L
  ),
  "",
  "LITERATURE SEARCH",
  "--------------------------------------------------",
  paste0(
    "Search queries: ",
    nrow(queries)
  ),
  paste0(
    "Verified PubMed candidates: ",
    nrow(candidates)
  ),
  paste0(
    "Selected verified references: ",
    nrow(selected)
  ),
  "",
  "REFERENCE GATE",
  "--------------------------------------------------",
  paste0(
    "Required total unique references: ",
    minimum_unique_references
  ),
  paste0(
    "Required Introduction unique references: ",
    minimum_intro_references
  ),
  paste0(
    "Required Discussion unique references: ",
    minimum_discussion_references
  ),
  paste0(
    "Introduction unique references: ",
    length(intro_pmids)
  ),
  paste0(
    "Discussion unique references: ",
    length(discussion_pmids)
  ),
  paste0(
    "Final unique cited references: ",
    unique_cited_n
  ),
  paste0(
    "Citation gate PASS: ",
    citation_gate_pass
  ),
  paste0(
    "Selected but uncited references: ",
    uncited_selected_n
  ),
  "",
  "OUTPUT",
  "--------------------------------------------------",
  paste0(
    "Final manuscript: ",
    normalize_slash(
      final_manuscript_file
    )
  ),
  paste0(
    "Reference list: ",
    normalize_slash(
      references_file
    )
  ),
  paste0(
    "Citation audit: ",
    normalize_slash(
      citation_audit_file
    )
  ),
  paste0(
    "Selected literature: ",
    normalize_slash(
      selected_file
    )
  ),
  paste0(
    "State: ",
    normalize_slash(
      state_file
    )
  ),
  "",
  "STATUS",
  "--------------------------------------------------",
  if (
    identical(
      draft_status,
      "DRAFT_COMPLETE_READY_FOR_R40_VALIDATION"
    )
  ) {
    "PASS: Manuscript draft completed with verified literature and is ready for R/40 consistency validation."
  } else {
    "PASS WITH GATE: Provisional manuscript draft completed; final RoB-dependent wording remains pending R/37/R/38 refresh before R/40 final validation."
  }
)

write_text_file(
  report_lines,
  report_file
)


# ============================================================
# 27. Console summary
# ============================================================

cat(
  "\n============================================================\n",
  "R/39 — Manuscript Drafting with Verified Literature\n",
  "============================================================\n\n",
  sep = ""
)

cat(
  "Project: ",
  project_title,
  "\n",
  "R/38 status: ",
  readiness_status,
  "\n",
  "Model: ",
  model_name,
  "\n",
  "PubMed candidates verified: ",
  nrow(candidates),
  "\n",
  "References selected: ",
  nrow(selected),
  "\n",
  "Introduction unique references: ",
  length(intro_pmids),
  "\n",
  "Discussion unique references: ",
  length(discussion_pmids),
  "\n",
  "Final unique cited references: ",
  unique_cited_n,
  "\n",
  "Minimum required: ",
  minimum_unique_references,
  "\n",
  "Reference gate: PASS\n",
  "Statistical recalculation: NO\n",
  sep = ""
)

cat(
  "\nManuscript: ",
  normalize_slash(
    final_manuscript_file
  ),
  "\n",
  "Citation audit: ",
  normalize_slash(
    citation_audit_file
  ),
  "\n",
  "Selected literature: ",
  normalize_slash(
    selected_file
  ),
  "\n",
  "Report: ",
  normalize_slash(
    report_file
  ),
  "\n",
  sep = ""
)

if (
  identical(
    draft_status,
    "PROVISIONAL_DRAFT_PENDING_FINAL_ROB"
  )
) {
  cat(
    "\nPASS WITH GATE: Draft is provisional until final R/37 RoB is completed and R/38/R/39 are refreshed.\n"
  )
} else {
  cat(
    "\nPASS: Draft is ready for R/40 numerical/citation consistency validation.\n"
  )
}
