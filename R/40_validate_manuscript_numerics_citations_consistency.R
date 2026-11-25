# ============================================================
# Universal SR-MA Agent Toolkit
# R/40_validate_manuscript_numerics_citations_consistency.R
#
# VERSION: 1.6.0
#
# PURPOSE
# ------------------------------------------------------------
# Final manuscript quality-control gate.
#
# R/40 validates:
#   1) NUMERICAL CONSISTENCY
#      - current-review numerical claims in Abstract / Results /
#        Discussion are mapped to canonical R/27 + R/38 facts;
#      - normal rounding is allowed;
#      - unsupported or mismatched current-review numbers are flagged.
#
#   2) CITATION / REFERENCE INTEGRITY
#      - every numbered in-text citation maps to a reference;
#      - every reference maps to R/39 citation_number_map.csv;
#      - each PMID exists in the verified R/39 literature library;
#      - cited PMIDs are re-verified against PubMed;
#      - title / DOI metadata drift is audited.
#
#   3) CROSS-SECTION CONSISTENCY
#      - Abstract, Results, and Discussion are semantically checked
#        for contradiction, unsupported claims, direction reversals,
#        and overstatement relative to canonical findings.
##   4) ARTIFACT COVERAGE
#      - every R/38 table/figure artifact marked coverage_required=TRUE
#        must be represented in the Results section by its exact R/39
#        machine-readable coverage marker;
#      - markers are stripped from the PASS-only submission candidate.
#
#   5) DESCRIPTIVE-EVIDENCE / IDENTIFIER INTEGRITY
#      - manuscript claims are checked against the full frozen R/38 Results
#        evidence dossier, including descriptive tables and NR/N/A states;
#      - contradictions between Table 1-3 evidence and manuscript prose are
#        hard-gated as contradictions/unsupported claims;
#      - internal screening/artifact identifiers are prohibited from
#        submission-visible manuscript prose.
#
# GOVERNANCE
# ------------------------------------------------------------
# - R/27 is the canonical statistical engine.
# - R/38 is the canonical manuscript evidence freeze.
# - R/39 is the manuscript drafting stage.
# - R/40 performs NO meta-analysis recalculation.
# - R/40 does NOT silently rewrite the manuscript.
# - Final PASS requires deterministic audits plus semantic audit.
# - If R/38/R/39 remain provisional pending final RoB, R/40 cannot
#   declare the manuscript final-ready.
#
# OUTPUT ROOT
# ------------------------------------------------------------
# results/manuscript/production/qc/
#   canonical_numeric_fact_registry.csv
#   manuscript_numeric_sentences.csv
#   numeric_claim_mapping.csv
#   numeric_claim_audit.csv
#   citation_integrity_audit.csv
#   pubmed_reference_reverification.csv
#   cross_section_consistency_audit.csv
#   artifact_coverage_audit.csv
#   machine_identifier_leak_audit.csv
#   qc_summary.csv
#   manuscript_qc.xlsx
#   issues_for_author_review.md
#   manuscript_submission_candidate.md    [PASS only]
#   manuscript_qc_state.yml
#   manuscript_qc_report.txt
#   agent_raw/
#
# OPTIONAL CONFIG
# ------------------------------------------------------------
# analysis:
#   manuscript:
#     qc_model: "gpt-5.6"
#     reverify_pubmed_references: true
#     numeric_relative_tolerance: 1e-6
#     semantic_fail_on_potential_contradiction: false
#
# Environment:
#   OPENAI_API_KEY      required for semantic QC
#   SRMA_MANUSCRIPT_QC_MODEL optional override
#   NCBI_EMAIL          recommended
#   NCBI_API_KEY        optional
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

file_nonempty <- function(path) {
  path <- as.character(path)
  exists <- file.exists(path)
  size <- rep(NA_real_, length(path))

  if (any(exists)) {
    size[exists] <- file.info(path[exists])$size
  }

  exists & !is.na(size) & size > 0
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

safe_num <- function(x) {
  suppressWarnings(
    as.numeric(x)
  )
}

safe_logical <- function(x, default = FALSE) {
  if (
    is.null(x) ||
    length(x) == 0L ||
    is.na(x[[1]])
  ) {
    return(isTRUE(default))
  }

  if (is.logical(x)) {
    return(isTRUE(x[[1]]))
  }

  text <- tolower(
    trimws(
      as.character(
        x[[1]]
      )
    )
  )

  if (text %in% c("true", "yes", "y", "1", "on")) {
    return(TRUE)
  }

  if (text %in% c("false", "no", "n", "0", "off")) {
    return(FALSE)
  }

  isTRUE(default)
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

safe_read_csv <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    progress = FALSE
  )
}

now_text <- function() {
  format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S"
  )
}

strip_machine_numeric_metadata <- function(text) {
  x <- as.character(text)
  x <- gsub("<!--.*?-->", " ", x, perl = TRUE)
  x <- gsub("\\[\\[[^]]+\\]\\]", " ", x, perl = TRUE)
  x <- gsub("https?://[^[:space:]]+", " ", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub("\\b10\\.[0-9]{4,9}/[^[:space:];,]+", " ", x, ignore.case = TRUE, perl = TRUE)
  x <- gsub(
    "\\b(?=[A-Za-z0-9_-]*[A-Za-z])(?=[A-Za-z0-9_-]*[0-9])[A-Za-z][A-Za-z0-9_-]*_[A-Za-z0-9_-]+\\b",
    " ",
    x,
    perl = TRUE
  )
  trimws(gsub("[[:space:]]+", " ", x, perl = TRUE))
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

  trimws(x)
}

normalize_title <- function(x) {
  x <- tolower(
    enc2utf8(
      as.character(x)
    )
  )

  x <- stringr::str_replace_all(
    x,
    "[^[:alnum:]]+",
    " "
  )

  stringr::str_squish(x)
}

normalize_doi <- function(x) {
  x <- tolower(
    trimws(
      as.character(x)
    )
  )

  x <- stringr::str_replace(
    x,
    "^https?://(dx\\.)?doi\\.org/",
    ""
  )

  x <- stringr::str_replace(
    x,
    "^doi:\\s*",
    ""
  )

  x
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


# ============================================================
# 1.1 Internal helper / regex preflight
# ============================================================

helper_self_test_ok <- TRUE

helper_self_test_ok <- helper_self_test_ok &&
  identical(
    strip_markdown_fence(
      "```json\n{}\n```"
    ),
    "{}"
  )

helper_self_test_ok <- helper_self_test_ok &&
  identical(
    normalize_doi(
      "https://doi.org/10.1000/ABC"
    ),
    "10.1000/abc"
  )

helper_self_test_ok <- helper_self_test_ok &&
  identical(
    slugify(
      "Primary Meta-Analysis"
    ),
    "primary_meta_analysis"
  )

if (!isTRUE(helper_self_test_ok)) {
  stop(
    "Internal R/40 helper preflight failed.",
    call. = FALSE
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
    "config/project_config.yml was not found. Run R/40 from the project root.",
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

reverify_pubmed <- safe_logical(
  manuscript_cfg$reverify_pubmed_references,
  TRUE
)

semantic_fail_on_potential <- safe_logical(
  manuscript_cfg$semantic_fail_on_potential_contradiction,
  FALSE
)

numeric_relative_tolerance <- safe_num(
  manuscript_cfg$numeric_relative_tolerance
)

if (
  length(numeric_relative_tolerance) == 0L ||
  is.na(numeric_relative_tolerance[[1]]) ||
  numeric_relative_tolerance[[1]] <= 0
) {
  numeric_relative_tolerance <- 1e-6
} else {
  numeric_relative_tolerance <- numeric_relative_tolerance[[1]]
}

qc_model <- safe_chr(
  Sys.getenv(
    "SRMA_MANUSCRIPT_QC_MODEL",
    unset = ""
  ),
  safe_chr(
    manuscript_cfg$qc_model,
    safe_chr(
      manuscript_cfg$model,
      "gpt-5.6"
    )
  )
)

openai_api_key <- Sys.getenv(
  "OPENAI_API_KEY",
  unset = ""
)

if (!nzchar(openai_api_key)) {
  stop(
    "OPENAI_API_KEY is required for R/40 semantic cross-section QC.",
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
# 3. Input paths
# ============================================================

production_root <- file.path(
  repo_root,
  "results",
  "manuscript",
  "production"
)

draft_root <- file.path(
  production_root,
  "drafting"
)

r38_state_file <- file.path(
  production_root,
  "manuscript_production_state.yml"
)

r38_results_file <- file.path(
  production_root,
  "results_factual_blocks.md"
)

r38_registry_file <- file.path(
  production_root,
  "manuscript_evidence_registry.csv"
)

r38_artifact_registry_file <- file.path(
  production_root,
  "manuscript_artifact_registry.csv"
)

r39_state_file <- file.path(
  draft_root,
  "manuscript_drafting_state.yml"
)

manuscript_file <- file.path(
  draft_root,
  "manuscript_draft.md"
)

token_manuscript_file <- file.path(
  draft_root,
  "manuscript_draft_with_pmid_tokens.md"
)

citation_map_file <- file.path(
  draft_root,
  "citation_number_map.csv"
)

citation_audit_r39_file <- file.path(
  draft_root,
  "citation_audit.csv"
)

selected_literature_file <- file.path(
  draft_root,
  "literature_selected.csv"
)

required_inputs <- c(
  r38_state = r38_state_file,
  r38_results = r38_results_file,
  r38_registry = r38_registry_file,
  r38_artifact_registry = r38_artifact_registry_file,
  r39_state = r39_state_file,
  manuscript = manuscript_file,
  token_manuscript = token_manuscript_file,
  citation_map = citation_map_file,
  citation_audit = citation_audit_r39_file,
  literature_selected = selected_literature_file
)

input_ok <- vapply(
  required_inputs,
  file_nonempty,
  logical(1)
)

if (!all(input_ok)) {
  stop(
    "R/40 required input(s) missing: ",
    paste(
      names(required_inputs)[
        !input_ok
      ],
      collapse = ", "
    ),
    call. = FALSE
  )
}

r38_state <- yaml::read_yaml(
  r38_state_file
)

r39_state <- yaml::read_yaml(
  r39_state_file
)

r38_status <- safe_chr(
  r38_state$manuscript_production$status,
  "NOT_READY"
)

r39_status <- safe_chr(
  r39_state$manuscript_drafting$drafting_status,
  ""
)

manuscript_text <- read_text_file(
  manuscript_file
)

token_manuscript_text <- read_text_file(
  token_manuscript_file
)

r38_results_text <- read_text_file(
  r38_results_file
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
      "R/38 artifact registry is missing required column(s): ",
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

citation_map <- safe_read_csv(
  citation_map_file
)

r39_citation_audit <- safe_read_csv(
  citation_audit_r39_file
)

selected_literature <- safe_read_csv(
  selected_literature_file
)

required_selected_columns <- c(
  "pmid",
  "title",
  "doi"
)

missing_selected_columns <- setdiff(
  required_selected_columns,
  names(selected_literature)
)

if (length(missing_selected_columns) > 0L) {
  stop(
    paste0(
      "R/39 literature_selected.csv is missing required column(s): ",
      paste(
        missing_selected_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

if (
  any(
    is.na(selected_literature$pmid) |
      !nzchar(trimws(as.character(selected_literature$pmid)))
  )
) {
  stop(
    "R/39 literature_selected.csv contains blank PMID values.",
    call. = FALSE
  )
}


# ============================================================
# 4. Output paths
# ============================================================

qc_root <- file.path(
  production_root,
  "qc"
)

raw_root <- file.path(
  qc_root,
  "agent_raw"
)

dir.create(
  qc_root,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  raw_root,
  recursive = TRUE,
  showWarnings = FALSE
)

canonical_fact_file <- file.path(
  qc_root,
  "canonical_numeric_fact_registry.csv"
)

numeric_sentence_file <- file.path(
  qc_root,
  "manuscript_numeric_sentences.csv"
)

numeric_mapping_file <- file.path(
  qc_root,
  "numeric_claim_mapping.csv"
)

numeric_audit_file <- file.path(
  qc_root,
  "numeric_claim_audit.csv"
)

citation_integrity_file <- file.path(
  qc_root,
  "citation_integrity_audit.csv"
)

pubmed_reverify_file <- file.path(
  qc_root,
  "pubmed_reference_reverification.csv"
)

consistency_audit_file <- file.path(
  qc_root,
  "cross_section_consistency_audit.csv"
)

artifact_coverage_file <- file.path(
  qc_root,
  "artifact_coverage_audit.csv"
)

machine_identifier_audit_file <- file.path(
  qc_root,
  "machine_identifier_leak_audit.csv"
)

qc_summary_file <- file.path(
  qc_root,
  "qc_summary.csv"
)

qc_workbook_file <- file.path(
  qc_root,
  "manuscript_qc.xlsx"
)

issues_file <- file.path(
  qc_root,
  "issues_for_author_review.md"
)

submission_candidate_file <- file.path(
  qc_root,
  "manuscript_submission_candidate.md"
)

state_file <- file.path(
  qc_root,
  "manuscript_qc_state.yml"
)

report_file <- file.path(
  qc_root,
  "manuscript_qc_report.txt"
)


# ============================================================
# 5. Manuscript section extraction
# ============================================================

# R/39 normally uses Markdown headings, but R/40 must tolerate editorial
# changes in heading depth/case and must not depend on dynamically built
# regular expressions. Heading recognition is therefore normalization-based.

standard_manuscript_headings <- c(
  "abstract",
  "introduction",
  "methods",
  "results",
  "discussion",
  "conclusions",
  "conclusion",
  "references",
  "acknowledgments",
  "acknowledgements",
  "funding",
  "disclosures",
  "conflicts of interest",
  "supplementary material"
)

normalize_heading_line <- function(line) {
  x <- trimws(as.character(line))

  if (!nzchar(x)) {
    return("")
  }

  # Remove Markdown heading marks (# ...), if present.
  x <- sub(
    "^#{1,6}[[:space:]]+",
    "",
    x,
    perl = TRUE
  )

  # Remove optional trailing Markdown heading marks.
  x <- sub(
    "[[:space:]]+#+[[:space:]]*$",
    "",
    x,
    perl = TRUE
  )

  x <- trimws(x)

  # Remove simple bold wrappers, e.g. **Abstract**.
  if (
    nchar(x) >= 4L &&
    startsWith(x, "**") &&
    endsWith(x, "**")
  ) {
    x <- substr(
      x,
      3L,
      nchar(x) - 2L
    )
  }

  x <- tolower(
    trimws(x)
  )

  # Normalize repeated whitespace only.
  x <- gsub(
    "[[:space:]]+",
    " ",
    x,
    perl = TRUE
  )

  x
}

heading_positions <- function(text) {
  lines <- strsplit(
    text,
    "\n",
    fixed = TRUE
  )[[1]]

  normalized <- vapply(
    lines,
    normalize_heading_line,
    character(1)
  )

  is_heading <- normalized %in%
    standard_manuscript_headings

  tibble::tibble(
    line_number = seq_along(lines),
    raw_line = lines,
    normalized_heading = normalized,
    is_heading = is_heading
  )
}

extract_section <- function(
    text,
    heading
) {
  lines <- strsplit(
    text,
    "\n",
    fixed = TRUE
  )[[1]]

  if (length(lines) == 0L) {
    return("")
  }

  positions <- heading_positions(text)
  target <- tolower(
    trimws(
      as.character(heading)
    )
  )

  hits <- positions$line_number[
    positions$is_heading &
      positions$normalized_heading == target
  ]

  if (length(hits) == 0L) {
    return("")
  }

  start_line <- hits[[1]] + 1L

  later_headings <- positions$line_number[
    positions$is_heading &
      positions$line_number > hits[[1]]
  ]

  end_line <- if (length(later_headings) > 0L) {
    later_headings[[1]] - 1L
  } else {
    length(lines)
  }

  if (start_line > end_line) {
    return("")
  }

  trimws(
    paste(
      lines[start_line:end_line],
      collapse = "\n"
    )
  )
}

remove_references_and_after <- function(text) {
  lines <- strsplit(
    text,
    "\n",
    fixed = TRUE
  )[[1]]

  if (length(lines) == 0L) {
    return("")
  }

  positions <- heading_positions(text)

  ref_hits <- positions$line_number[
    positions$is_heading &
      positions$normalized_heading == "references"
  ]

  if (length(ref_hits) == 0L) {
    return(text)
  }

  ref_line <- ref_hits[[1]]

  if (ref_line <= 1L) {
    return("")
  }

  paste(
    lines[seq_len(ref_line - 1L)],
    collapse = "\n"
  )
}

r39_abstract_file <- file.path(
  draft_root,
  "abstract_draft.md"
)

r39_results_file <- file.path(
  draft_root,
  "results_draft.md"
)

r39_discussion_file <- file.path(
  draft_root,
  "discussion_draft.md"
)

r39_references_file <- file.path(
  draft_root,
  "references_vancouver.md"
)

abstract_text <- extract_section(
  manuscript_text,
  "Abstract"
)

results_text <- extract_section(
  manuscript_text,
  "Results"
)

discussion_text <- extract_section(
  manuscript_text,
  "Discussion"
)

section_sources <- c(
  Abstract = "manuscript_draft.md",
  Results = "manuscript_draft.md",
  Discussion = "manuscript_draft.md"
)

# Canonical R/39 section-file fallback.
if (!nzchar(abstract_text) && file_nonempty(r39_abstract_file)) {
  abstract_text <- trimws(
    read_text_file(
      r39_abstract_file
    )
  )
  section_sources[["Abstract"]] <- "abstract_draft.md"
}

if (!nzchar(results_text) && file_nonempty(r39_results_file)) {
  results_text <- trimws(
    read_text_file(
      r39_results_file
    )
  )
  section_sources[["Results"]] <- "results_draft.md"
}

if (!nzchar(discussion_text) && file_nonempty(r39_discussion_file)) {
  discussion_text <- trimws(
    read_text_file(
      r39_discussion_file
    )
  )
  section_sources[["Discussion"]] <- "discussion_draft.md"
}

missing_sections <- names(
  section_sources
)[
  !c(
    nzchar(abstract_text),
    nzchar(results_text),
    nzchar(discussion_text)
  )
]

if (length(missing_sections) > 0L) {
  stop(
    paste0(
      "R/40 could not resolve required manuscript section(s): ",
      paste(
        missing_sections,
        collapse = ", "
      ),
      ". Checked both manuscript_draft.md and the canonical R/39 section files."
    ),
    call. = FALSE
  )
}

# Internal parser self-checks. These are deterministic and catch future
# regressions before real manuscript QC begins.
section_self_test_1 <- extract_section(
  paste(
    "# ABSTRACT",
    "Alpha.",
    "## Results",
    "Beta.",
    sep = "\n"
  ),
  "Abstract"
)

section_self_test_2 <- extract_section(
  paste(
    "**Discussion**",
    "Gamma.",
    "## References",
    "1. Ref.",
    sep = "\n"
  ),
  "Discussion"
)

section_self_test_3 <- remove_references_and_after(
  paste(
    "## Results",
    "Delta.",
    "# REFERENCES",
    "1. Ref.",
    sep = "\n"
  )
)

if (
  !identical(section_self_test_1, "Alpha.") ||
    !identical(section_self_test_2, "Gamma.") ||
    grepl(
      "Ref",
      section_self_test_3,
      fixed = TRUE
    )
) {
  stop(
    "Internal manuscript section-parser self-test failed.",
    call. = FALSE
  )
}

cat(
  "R/40 section sources: ",
  paste(
    paste0(
      names(section_sources),
      "=",
      unname(section_sources)
    ),
    collapse = "; "
  ),
  "\n",
  sep = ""
)

# ============================================================
# 5.1 Required table/figure artifact coverage audit
# ============================================================

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

required_artifacts <- artifact_registry |>
  dplyr::filter(
    coverage_required
  ) |>
  dplyr::arrange(
    artifact_id
  )

results_artifact_markers <- extract_artifact_markers(
  results_text
)

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
    "A substantive Results paragraph can legitimately cover multiple related artifacts with one shared interpretation. ",
    "<!-- ARTIFACT: A_ONE --> ",
    "<!-- ARTIFACT: A_TWO -->"
  )

  contexts <- c(
    artifact_marker_context(
      demo,
      "<!-- ARTIFACT: A_ONE -->"
    ),
    artifact_marker_context(
      demo,
      "<!-- ARTIFACT: A_TWO -->"
    )
  )

  alpha_counts <- nchar(
    gsub(
      "[^[:alpha:]]",
      "",
      contexts
    )
  )

  if (any(alpha_counts < 40L)) {
    stop(
      "Internal R/40 artifact-marker shared-context self-test failed.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

artifact_marker_context_self_test()


artifact_coverage_audit <- if (
  nrow(required_artifacts) == 0L
) {
  tibble::tibble(
    artifact_id = character(0),
    artifact_type = character(0),
    title = character(0),
    manuscript_role = character(0),
    manuscript_priority = character(0),
    source_relative_path = character(0),
    marker_present_in_results = logical(0),
    substantive_context_present = logical(0),
    context_excerpt = character(0),
    status = character(0),
    issue = character(0)
  )
} else {
  required_artifacts |>
    dplyr::rowwise() |>
    dplyr::mutate(
      marker_present_in_results = artifact_id %in%
        results_artifact_markers,
      context_excerpt = if (
        marker_present_in_results
      ) {
        artifact_marker_context(
          results_text,
          coverage_token
        )
      } else {
        ""
      },
      substantive_context_present = if (
        marker_present_in_results
      ) {
        nchar(
          gsub(
            "[^[:alpha:]]",
            "",
            context_excerpt
          )
        ) >= 40L
      } else {
        FALSE
      },
      status = ifelse(
        marker_present_in_results &&
          substantive_context_present,
        "PASS",
        "FAIL"
      ),
      issue = dplyr::case_when(
        !marker_present_in_results ~ paste0(
          "Required artifact is not represented by its coverage marker in the Results section: ",
          coverage_token
        ),
        !substantive_context_present ~ paste0(
          "Coverage marker is present but no substantive Results prose was detected immediately before it: ",
          coverage_token
        ),
        TRUE ~ ""
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      artifact_id,
      artifact_type,
      title,
      manuscript_role,
      manuscript_priority,
      source_relative_path,
      marker_present_in_results,
      substantive_context_present,
      context_excerpt,
      status,
      issue
    )
}

readr::write_csv(
  artifact_coverage_audit,
  artifact_coverage_file,
  na = ""
)


# ============================================================
# 5.2 Submission-visible internal identifier leakage audit
# ============================================================
# Coverage markers are expected internal provenance tokens during R/39/R/40
# and are stripped from the PASS-only submission candidate. They are removed
# before this audit so that only accidental manuscript-visible identifiers fail.

submission_visible_text_for_id_audit <- gsub(
  "<!--\\s*ARTIFACT:\\s*[A-Za-z0-9_-]+\\s*-->",
  " ",
  manuscript_text,
  perl = TRUE
)

submission_visible_text_for_id_audit <- remove_references_and_after(
  submission_visible_text_for_id_audit
)

machine_identifier_patterns <- c(
  screening_code = "\\bSCR_[A-Za-z0-9_-]+\\b",
  screening_key_value = "\\bscreening_id\\s*(?:::|[:=])\\s*[A-Za-z0-9_:-]+",
  compound_internal_label = "\\b(?:screening_id|study_id|record_id|report_id)::[A-Za-z0-9_:-]+\\b",
  artifact_id = "\\bA_[A-Fa-f0-9]{8,32}\\b"
)

machine_identifier_rows <- list()

for (pattern_name in names(machine_identifier_patterns)) {
  pattern <- machine_identifier_patterns[[pattern_name]]

  hits <- stringr::str_extract_all(
    submission_visible_text_for_id_audit,
    stringr::regex(
      pattern,
      ignore_case = TRUE
    )
  )[[1]]

  hits <- unique(
    trimws(
      as.character(hits)
    )
  )

  hits <- hits[
    !is.na(hits) &
      nzchar(hits)
  ]

  if (length(hits) == 0L) {
    next
  }

  for (hit in hits) {
    machine_identifier_rows[[
      length(machine_identifier_rows) + 1L
    ]] <- tibble::tibble(
      pattern_name = pattern_name,
      matched_text = hit,
      status = "FAIL",
      issue = paste0(
        "Internal machine identifier leaked into submission-visible manuscript text: ",
        hit
      )
    )
  }
}

machine_identifier_audit <- if (
  length(machine_identifier_rows) == 0L
) {
  tibble::tibble(
    pattern_name = character(0),
    matched_text = character(0),
    status = character(0),
    issue = character(0)
  )
} else {
  dplyr::bind_rows(
    machine_identifier_rows
  ) |>
    dplyr::distinct(
      pattern_name,
      matched_text,
      .keep_all = TRUE
    )
}

readr::write_csv(
  machine_identifier_audit,
  machine_identifier_audit_file,
  na = ""
)


# ============================================================
# 6. OpenAI semantic QC helper
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
    model = qc_model,
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
      "OpenAI semantic QC returned no usable text for: ",
      raw_name,
      call. = FALSE
    )
  }

  text
}


# ============================================================
# 7. Build canonical numeric fact registry
# ============================================================

r27_root <- file.path(
  repo_root,
  "results",
  "meta_analysis"
)

r27_summary_files <- if (dir.exists(r27_root)) {
  list.files(
    r27_root,
    pattern = "^meta_analysis_summary\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
} else {
  character(0)
}

r27_summary_files <- r27_summary_files[
  !grepl(
    "/tables/",
    normalize_slash(
      r27_summary_files
    ),
    fixed = TRUE
  )
]

fact_rows <- list()

add_fact <- function(
    source,
    analysis_group,
    metric,
    value,
    unit = "",
    source_text = ""
) {
  value_num <- safe_num(value)

  if (
    length(value_num) == 0L ||
    is.na(value_num[[1]])
  ) {
    return(NULL)
  }

  tibble::tibble(
    fact_id = paste0(
      "F",
      sprintf(
        "%04d",
        length(fact_rows) + 1L
      )
    ),
    source = source,
    analysis_group = analysis_group,
    metric = metric,
    canonical_value = value_num[[1]],
    unit = unit,
    source_text = source_text
  )
}

for (summary_path in r27_summary_files) {
  d <- tryCatch(
    safe_read_csv(
      summary_path
    ),
    error = function(e) NULL
  )

  if (
    is.null(d) ||
    nrow(d) == 0L
  ) {
    next
  }

  for (i in seq_len(nrow(d))) {
    row <- d[
      i,
      ,
      drop = FALSE
    ]

    group_col <- first_existing_col(
      row,
      c(
        "analysis_group",
        "group_name",
        "analysis_set",
        "analysis_name",
        "label"
      )
    )

    group_name <- if (!is.na(group_col)) {
      safe_chr(
        row[[group_col]],
        basename(dirname(summary_path))
      )
    } else {
      basename(dirname(summary_path))
    }

    metric_map <- list(
      n_studies = c(
        "n_studies",
        "k",
        "studies"
      ),
      pooled_effect = c(
        "pooled_effect",
        "pooled_estimate",
        "estimate"
      ),
      pooled_lower_ci = c(
        "pooled_lower_ci",
        "lower_ci",
        "lower"
      ),
      pooled_upper_ci = c(
        "pooled_upper_ci",
        "upper_ci",
        "upper"
      ),
      p_value = c(
        "p_value",
        "overall_p",
        "p"
      ),
      heterogeneity_i2 = c(
        "heterogeneity_i2",
        "i2",
        "I2"
      ),
      heterogeneity_tau2 = c(
        "heterogeneity_tau2",
        "tau2"
      ),
      heterogeneity_q = c(
        "heterogeneity_q",
        "q",
        "Q"
      ),
      heterogeneity_p = c(
        "heterogeneity_p",
        "q_p",
        "QEp"
      )
    )

    for (metric in names(metric_map)) {
      col <- first_existing_col(
        row,
        metric_map[[metric]]
      )

      if (is.na(col)) {
        next
      }

      unit <- if (
        identical(
          metric,
          "heterogeneity_i2"
        )
      ) {
        "%"
      } else {
        ""
      }

      new_fact <- add_fact(
        source = normalize_slash(
          summary_path
        ),
        analysis_group = group_name,
        metric = metric,
        value = row[[col]][[1]],
        unit = unit,
        source_text = paste0(
          metric,
          "=",
          safe_chr(
            row[[col]],
            ""
          )
        )
      )

      if (!is.null(new_fact)) {
        fact_rows[[
          length(fact_rows) + 1L
        ]] <- new_fact
      }
    }
  }
}


# Add study-level and aggregate count facts from canonical R/27
# study_effects_used.csv files. This is deterministic bookkeeping only:
# no effect estimate, variance, model, or inferential statistic is recalculated.
normalize_count_colname <- function(x) {
  x <- tolower(
    gsub(
      "[^A-Za-z0-9]+",
      "_",
      as.character(x)
    )
  )
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

is_count_like_column <- function(name) {
  n <- normalize_count_colname(name)
  if (!nzchar(n)) return(FALSE)

  exclusion_terms <- c(
    "effect","estimate","ratio","risk_ratio","odds_ratio","hazard_ratio",
    "log","lower","upper","confidence","variance","standard_error",
    "stderr","se","weight","mean","median","sd","standard_deviation",
    "i2","tau","heterogeneity","p_value","pvalue","year","age",
    "follow_up","followup","duration","time"
  )

  excluded <- any(
    vapply(
      exclusion_terms,
      function(term) {
        grepl(
          paste0("(^|_)", term, "($|_)"),
          n,
          perl = TRUE
        )
      },
      logical(1)
    )
  )

  if (excluded) return(FALSE)

  grepl(
    "(^|_)(n|total|sample|sample_size|sample_n|events?|cases?|participants?|patients?|subjects?|persons?|observations?)($|_)",
    n,
    perl = TRUE
  )
}

is_arm_denominator_column <- function(name) {
  n <- normalize_count_colname(name)

  role_pattern <- paste0(
    "(^|_)(?:",
    paste(
      c(
        "intervention","treatment","treated","experimental","exposed",
        "index","comparator","control","reference","unexposed",
        "standard","arm[0-9]*","group[0-9]*"
      ),
      collapse = "|"
    ),
    ")(?:_|$)"
  )

  denominator_pattern <- paste0(
    "(^|_)(total|n|sample|sample_size|sample_n|",
    "participants?|patients?|subjects?|persons?)($|_)"
  )

  grepl(role_pattern, n, perl = TRUE) &&
    grepl(denominator_pattern, n, perl = TRUE)
}

is_event_count_column <- function(name) {
  n <- normalize_count_colname(name)
  grepl(
    "(^|_)(events?|cases?)($|_)",
    n,
    perl = TRUE
  )
}

is_group_denominator_column <- function(name) {
  n <- normalize_count_colname(name)

  if (
    !is_count_like_column(n) ||
      is_event_count_column(n)
  ) {
    return(FALSE)
  }

  tokens <- unlist(
    strsplit(
      n,
      "_",
      fixed = TRUE
    )
  )

  tokens <- tokens[
    nzchar(tokens)
  ]

  if (length(tokens) < 2L) {
    return(FALSE)
  }

  denominator_token <- grepl(
    "^(?:n[0-9]*|total|sample|size|participants?|patients?|subjects?|persons?|observations?)$",
    tokens,
    perl = TRUE
  )

  if (!any(denominator_token)) {
    return(FALSE)
  }

  # A role/group token is deliberately generic. It may be intervention/control,
  # exposed/unexposed, test/reference, arm labels, treatment names, or any
  # project-defined group label. Exclude only structural words that do not
  # identify a mutually distinguishable group.
  structural_tokens <- c(
    "n",
    "total",
    "sample",
    "size",
    "participant",
    "participants",
    "patient",
    "patients",
    "subject",
    "subjects",
    "person",
    "persons",
    "observation",
    "observations",
    "study",
    "studies",
    "overall",
    "all",
    "combined",
    "pooled",
    "analysis",
    "analyzed",
    "analysed",
    "included",
    "eligible",
    "population"
  )

  candidate_group_tokens <- tokens[
    !denominator_token &
      !(tokens %in% structural_tokens)
  ]

  length(candidate_group_tokens) >= 1L
}


study_effect_files <- if (dir.exists(r27_root)) {
  list.files(
    r27_root,
    pattern = "^study_effects_used\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
} else {
  character(0)
}

for (study_path in study_effect_files) {
  study_data <- tryCatch(
    safe_read_csv(study_path),
    error = function(e) NULL
  )

  if (is.null(study_data) || nrow(study_data) == 0L) next

  analysis_group_name <- basename(dirname(study_path))

  count_cols <- names(study_data)[
    vapply(
      names(study_data),
      is_count_like_column,
      logical(1)
    )
  ]

  if (length(count_cols) == 0L) next

  human_label_col <- first_existing_col(
    study_data,
    c(
      "study_label","citation","author_year","first_author_year",
      "study_name","first_author","study_id"
    )
  )

  for (count_col in count_cols) {
    count_values <- safe_num(study_data[[count_col]])
    valid <- !is.na(count_values)

    if (!any(valid)) next

    for (row_index in which(valid)) {
      study_label <- if (!is.na(human_label_col)) {
        safe_chr(
          study_data[[human_label_col]][[row_index]],
          paste0("row_", row_index)
        )
      } else {
        paste0("row_", row_index)
      }

      new_fact <- add_fact(
        source = normalize_slash(study_path),
        analysis_group = analysis_group_name,
        metric = paste0(
          "study_count::",
          normalize_count_colname(count_col)
        ),
        value = count_values[[row_index]],
        source_text = paste(
          "Study", study_label,
          "in analysis group", analysis_group_name,
          "has verified participant/sample count",
          count_col, "=", count_values[[row_index]]
        )
      )

      if (!is.null(new_fact)) {
        fact_rows[[length(fact_rows) + 1L]] <- new_fact
      }
    }

    aggregate_value <- sum(count_values[valid])

    aggregate_fact <- add_fact(
      source = normalize_slash(study_path),
      analysis_group = analysis_group_name,
      metric = paste0(
        "aggregate_count::",
        normalize_count_colname(count_col)
      ),
      value = aggregate_value,
      source_text = paste(
        "Analysis group", analysis_group_name,
        "aggregate verified participant/sample count for",
        count_col, "across", sum(valid),
        "study rows =", aggregate_value
      )
    )

    if (!is.null(aggregate_fact)) {
      fact_rows[[length(fact_rows) + 1L]] <- aggregate_fact
    }
  }

  explicit_denominator_cols <- count_cols[
    vapply(
      count_cols,
      is_arm_denominator_column,
      logical(1)
    ) &
      !vapply(
        count_cols,
        is_event_count_column,
        logical(1)
      )
  ]

  generic_group_denominator_cols <- count_cols[
    vapply(
      count_cols,
      is_group_denominator_column,
      logical(1)
    ) &
      !vapply(
        count_cols,
        is_event_count_column,
        logical(1)
      )
  ]

  # Use both sources of evidence. The explicit vocabulary remains useful,
  # while the generic rule allows completely different group labels.
  denominator_cols <- unique(
    c(
      explicit_denominator_cols,
      generic_group_denominator_cols
    )
  )

  if (length(denominator_cols) == 2L) {
    denominator_sums <- vapply(
      denominator_cols,
      function(col) {
        values <- safe_num(study_data[[col]])
        if (all(is.na(values))) return(NA_real_)
        sum(values[!is.na(values)])
      },
      numeric(1)
    )

    denominator_sums <- denominator_sums[!is.na(denominator_sums)]

    if (length(denominator_sums) == 2L) {
      total_sample <- sum(denominator_sums)

      total_fact <- add_fact(
        source = normalize_slash(study_path),
        analysis_group = analysis_group_name,
        metric = "aggregate_total_sample_across_arms",
        value = total_sample,
        source_text = paste(
          "Analysis group", analysis_group_name,
          "total participants/sample across two verified complementary group denominators =",
          total_sample,
          "; complementary denominator columns:",
          paste(names(denominator_sums), collapse = ", "),
          "; component aggregate counts:",
          paste(denominator_sums, collapse = ", ")
        )
      )

      if (!is.null(total_fact)) {
        fact_rows[[length(fact_rows) + 1L]] <- total_fact
      }
    }
  }
}

count_column_self_test <- c(
  intervention_total = TRUE,
  comparator_n = TRUE,
  arm1_sample_size = TRUE,
  treatment_events = TRUE,
  pooled_effect = FALSE,
  mean_age = FALSE,
  lower_ci = FALSE
)

observed_count_self_test <- vapply(
  names(count_column_self_test),
  is_count_like_column,
  logical(1)
)

if (
  !identical(
    unname(observed_count_self_test),
    unname(count_column_self_test)
  )
) {
  stop(
    "Internal canonical count-column classification self-test failed.",
    call. = FALSE
  )
}


group_denominator_self_test <- c(
  n_groupa = TRUE,
  groupb_n = TRUE,
  total_exposed = TRUE,
  reference_total = TRUE,
  sample_size_armx = TRUE,
  overall_total = FALSE,
  study_total = FALSE,
  sample_size = FALSE,
  treatment_events = FALSE,
  mean_age = FALSE
)

observed_group_denominator_test <- vapply(
  names(group_denominator_self_test),
  is_group_denominator_column,
  logical(1)
)

if (
  !identical(
    unname(observed_group_denominator_test),
    unname(group_denominator_self_test)
  )
) {
  stop(
    "Internal generic complementary-denominator classification self-test failed.",
    call. = FALSE
  )
}

# Verify deterministic total bookkeeping without using any project-specific
# values or labels.
complementary_total_self_test <- sum(
  c(
    137,
    83
  )
)

if (!identical(complementary_total_self_test, 220)) {
  stop(
    "Internal complementary-denominator total self-test failed.",
    call. = FALSE
  )
}


# Add numerical bullet lines from frozen R/38 Results as auditable text facts.
r38_lines <- strsplit(
  r38_results_text,
  "\n",
  fixed = TRUE
)[[1]]

r38_numeric_lines <- r38_lines[
  grepl(
    "[0-9]",
    r38_lines
  )
]

r38_numeric_lines <- trimws(
  r38_numeric_lines
)

r38_numeric_lines <- r38_numeric_lines[
  nzchar(
    r38_numeric_lines
  )
]

for (line in r38_numeric_lines) {
  evidence_line <- strip_machine_numeric_metadata(line)
  if (!grepl("[0-9]", evidence_line)) next

  numeric_line <- gsub(
    "−",
    "-",
    evidence_line,
    fixed = TRUE
  )

  values <- stringr::str_extract_all(
    numeric_line,
    "(?<![A-Za-z0-9.])[-+]?(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)(?:[eE][-+]?[0-9]+)?"
  )[[1]]

  values_num <- safe_num(values)

  values_num <- values_num[
    !is.na(values_num)
  ]

  if (length(values_num) == 0L) {
    next
  }

  for (j in seq_along(values_num)) {
    fact_rows[[
      length(fact_rows) + 1L
    ]] <- tibble::tibble(
      fact_id = paste0(
        "F",
        sprintf(
          "%04d",
          length(fact_rows) + 1L
        )
      ),
      source = normalize_slash(
        r38_results_file
      ),
      analysis_group = "R38_RESULTS_BLOCK",
      metric = paste0(
        "r38_text_value_",
        j
      ),
      canonical_value = values_num[[j]],
      unit = if (
        grepl(
          "%",
          line,
          fixed = TRUE
        )
      ) {
        "%"
      } else {
        ""
      },
      source_text = evidence_line
    )
  }
}

canonical_facts <- if (
  length(fact_rows) == 0L
) {
  tibble::tibble(
    fact_id = character(),
    source = character(),
    analysis_group = character(),
    metric = character(),
    canonical_value = numeric(),
    unit = character(),
    source_text = character()
  )
} else {
  dplyr::bind_rows(
    fact_rows
  )
}

if (nrow(canonical_facts) == 0L) {
  stop(
    "No canonical numerical facts could be constructed from R/27/R/38.",
    call. = FALSE
  )
}

readr::write_csv(
  canonical_facts,
  canonical_fact_file,
  na = ""
)


# ============================================================
# 8. Extract manuscript sentences containing numbers
# ============================================================

strip_numeric_citations <- function(text) {
  x <- strip_machine_numeric_metadata(text)
  stringr::str_replace_all(
    x,
    "\\[(?:[0-9]+(?:\\s*[-–—,]\\s*[0-9]+)*)\\]",
    ""
  )
}

split_sentences <- function(
    text,
    section
) {
  clean <- strip_numeric_citations(
    text
  )

  clean <- stringr::str_replace_all(
    clean,
    "\n+",
    " "
  )

  sentences <- unlist(
    strsplit(
      clean,
      "(?<=[.!?])\\s+",
      perl = TRUE
    )
  )

  sentences <- trimws(
    sentences
  )

  sentences <- sentences[
    nzchar(sentences)
  ]

  standalone_numeric_pattern <- paste0(
    "(?<![A-Za-z0-9.])",
    "[-+]?",
    "(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)",
    "(?:[eE][-+]?[0-9]+)?"
  )

  # Domain labels/codes such as D1, D2, G3, or other alphanumeric
  # identifiers are not standalone scientific numeric claims.
  keep <- vapply(
    sentences,
    function(sentence) {
      cleaned_sentence <- strip_numeric_citations(sentence)

      grepl(
        standalone_numeric_pattern,
        cleaned_sentence,
        perl = TRUE
      )
    },
    logical(1)
  )

  sentences <- sentences[keep]

  if (length(sentences) == 0L) {
    return(
      tibble::tibble(
        sentence_id = character(),
        section = character(),
        sentence = character()
      )
    )
  }

  tibble::tibble(
    sentence_id = paste0(
      substr(
        toupper(section),
        1,
        3
      ),
      "_",
      sprintf(
        "%03d",
        seq_along(sentences)
      )
    ),
    section = section,
    sentence = sentences
  )
}


numeric_sentence_filter_self_test <- split_sentences(
  paste(
    "Domains D1, D2, and D7 were rated moderate.",
    "A total of 321 participants contributed data.",
    sep = " "
  ),
  "Results"
)

if (
  nrow(numeric_sentence_filter_self_test) != 1L ||
    !grepl(
      "321 participants",
      numeric_sentence_filter_self_test$sentence[[1]],
      fixed = TRUE
    )
) {
  stop(
    "Internal standalone-numeric sentence filter self-test failed.",
    call. = FALSE
  )
}


numeric_sentences <- dplyr::bind_rows(
  split_sentences(
    abstract_text,
    "Abstract"
  ),
  split_sentences(
    results_text,
    "Results"
  ),
  split_sentences(
    discussion_text,
    "Discussion"
  )
)

readr::write_csv(
  numeric_sentences,
  numeric_sentence_file,
  na = ""
)

if (nrow(numeric_sentences) == 0L) {
  stop(
    paste(
      "R/40 found no numerical sentences in Abstract, Results, or Discussion.",
      "A quantitative SR-MA manuscript cannot receive numerical QC without numerical claims."
    ),
    call. = FALSE
  )
}


# ============================================================
# 9. Semantic map of numeric claims to canonical facts
# ============================================================

facts_prompt <- apply(
  canonical_facts,
  1,
  function(row) {
    paste0(
      row[["fact_id"]],
      " | group=",
      row[["analysis_group"]],
      " | metric=",
      row[["metric"]],
      " | value=",
      row[["canonical_value"]],
      row[["unit"]],
      " | source_text=",
      row[["source_text"]]
    )
  }
)

sentences_prompt <- if (
  nrow(numeric_sentences) > 0L
) {
  apply(
    numeric_sentences,
    1,
    function(row) {
      paste0(
        row[["sentence_id"]],
        " | ",
        row[["section"]],
        " | ",
        row[["sentence"]]
      )
    }
  )
} else {
  character(0)
}

numeric_system <- paste(
  "You are auditing numerical claims in a systematic-review/meta-analysis manuscript.",
  "Classify each supplied sentence that contains numbers.",
  "CURRENT_REVIEW means the number describes this review's search, included studies, meta-analysis, heterogeneity, diagnostics, or risk-of-bias results.",
  "EXTERNAL_LITERATURE means the number describes another publication or external background evidence.",
  "NON_RESULT means the number is a methodological convention, citation artifact, generic year, section label, or otherwise not a current-review result.",
  "For CURRENT_REVIEW claims, map the sentence to the best matching canonical fact IDs supplied.",
  "Do not invent fact IDs.",
  "Return STRICT JSON only."
)

numeric_user <- paste0(
  "PROJECT TITLE:\n",
  project_title,
  "\n\nCANONICAL FACTS:\n",
  paste(
    facts_prompt,
    collapse = "\n"
  ),
  "\n\nMANUSCRIPT SENTENCES WITH NUMBERS:\n",
  if (
    length(sentences_prompt) > 0L
  ) {
    paste(
      sentences_prompt,
      collapse = "\n"
    )
  } else {
    "[none]"
  },
  "\n\nReturn JSON exactly:\n",
  "{\n",
  '  "claims": [\n',
  '    {"sentence_id":"RES_001","classification":"CURRENT_REVIEW|EXTERNAL_LITERATURE|NON_RESULT","fact_ids":["F0001"],"reason":"..."}\n',
  "  ]\n",
  "}\n",
  "Every supplied sentence_id must appear exactly once."
)

numeric_mapping_text <- call_openai(
  numeric_system,
  numeric_user,
  "01_numeric_claim_mapping",
  max_output_tokens = 12000L
)

numeric_mapping_text <- strip_markdown_fence(
  numeric_mapping_text
)

numeric_mapping_json <- tryCatch(
  jsonlite::fromJSON(
    numeric_mapping_text,
    simplifyVector = FALSE
  ),
  error = function(e) NULL
)

if (
  is.null(numeric_mapping_json) ||
  is.null(numeric_mapping_json$claims)
) {
  stop(
    "Numeric claim mapping JSON could not be parsed.",
    call. = FALSE
  )
}

mapping_rows <- lapply(
  numeric_mapping_json$claims,
  function(item) {
    fact_ids <- item$fact_ids

    if (is.null(fact_ids)) {
      fact_ids <- character(0)
    }

    tibble::tibble(
      sentence_id = safe_chr(
        item$sentence_id,
        ""
      ),
      classification = safe_chr(
        item$classification,
        ""
      ),
      fact_ids = paste(
        as.character(
          unlist(
            fact_ids
          )
        ),
        collapse = ";"
      ),
      reason = safe_chr(
        item$reason,
        ""
      )
    )
  }
)

numeric_mapping <- dplyr::bind_rows(
  mapping_rows
)

expected_sentence_ids <- numeric_sentences$sentence_id

missing_sentence_ids <- setdiff(
  expected_sentence_ids,
  numeric_mapping$sentence_id
)

extra_sentence_ids <- setdiff(
  numeric_mapping$sentence_id,
  expected_sentence_ids
)

duplicate_sentence_ids <- numeric_mapping$sentence_id[
  duplicated(
    numeric_mapping$sentence_id
  )
]

allowed_numeric_classifications <- c(
  "CURRENT_REVIEW",
  "EXTERNAL_LITERATURE",
  "NON_RESULT"
)

invalid_numeric_classifications <- setdiff(
  unique(
    toupper(
      trimws(
        numeric_mapping$classification
      )
    )
  ),
  allowed_numeric_classifications
)

if (
  length(missing_sentence_ids) > 0L ||
  length(extra_sentence_ids) > 0L ||
  length(duplicate_sentence_ids) > 0L ||
  length(invalid_numeric_classifications) > 0L
) {
  stop(
    paste(
      "Numeric claim mapper returned an invalid mapping contract.",
      if (length(missing_sentence_ids) > 0L) {
        paste0(
          "Missing sentence IDs: ",
          paste(missing_sentence_ids, collapse = ", "),
          "."
        )
      } else {
        ""
      },
      if (length(extra_sentence_ids) > 0L) {
        paste0(
          "Unexpected sentence IDs: ",
          paste(extra_sentence_ids, collapse = ", "),
          "."
        )
      } else {
        ""
      },
      if (length(duplicate_sentence_ids) > 0L) {
        paste0(
          "Duplicate sentence IDs: ",
          paste(unique(duplicate_sentence_ids), collapse = ", "),
          "."
        )
      } else {
        ""
      },
      if (length(invalid_numeric_classifications) > 0L) {
        paste0(
          "Invalid classification(s): ",
          paste(invalid_numeric_classifications, collapse = ", "),
          "."
        )
      } else {
        ""
      }
    ),
    call. = FALSE
  )
}

numeric_mapping <- numeric_mapping |>
  dplyr::left_join(
    numeric_sentences,
    by = "sentence_id"
  ) |>
  dplyr::select(
    sentence_id,
    section,
    sentence,
    classification,
    fact_ids,
    reason
  )

readr::write_csv(
  numeric_mapping,
  numeric_mapping_file,
  na = ""
)


# ============================================================
# 10. Deterministic numeric comparison
# ============================================================

normalize_numeric_text <- function(text) {
  text <- strip_numeric_citations(
    text
  )

  gsub(
    "−",
    "-",
    text,
    fixed = TRUE
  )
}

numeric_token_pattern <- paste0(
  "(?<![A-Za-z0-9.])",
  "[-+]?",
  "(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)",
  "(?:[eE][-+]?[0-9]+)?"
)

extract_numeric_tokens <- function(text) {
  normalized_text <- normalize_numeric_text(
    text
  )

  locations <- stringr::str_locate_all(
    normalized_text,
    numeric_token_pattern
  )[[1]]

  if (
    is.null(locations) ||
      nrow(locations) == 0L
  ) {
    return(
      tibble::tibble(
        token = character(0),
        value = numeric(0),
        digits_after_decimal = integer(0),
        start = integer(0),
        end = integer(0)
      )
    )
  }

  tokens <- stringr::str_sub(
    normalized_text,
    locations[, "start"],
    locations[, "end"]
  )

  tibble::tibble(
    token = tokens,
    value = safe_num(tokens),
    digits_after_decimal = vapply(
      tokens,
      function(token) {
        token_clean <- sub(
          "[eE].*$",
          "",
          token
        )

        if (!grepl("\\.", token_clean)) {
          return(0L)
        }

        nchar(
          sub(
            "^.*\\.",
            "",
            token_clean
          )
        )
      },
      integer(1)
    ),
    start = as.integer(
      locations[, "start"]
    ),
    end = as.integer(
      locations[, "end"]
    )
  )
}

confidence_level_token_flags <- function(
    text,
    numeric_tokens
) {
  if (
    is.null(numeric_tokens) ||
      nrow(numeric_tokens) == 0L
  ) {
    return(
      logical(0)
    )
  }

  normalized_text <- normalize_numeric_text(
    text
  )

  vapply(
    seq_len(
      nrow(numeric_tokens)
    ),
    function(i) {
      token_end <- numeric_tokens$end[[i]]

      if (
        is.na(token_end) ||
          token_end < 0L
      ) {
        return(FALSE)
      }

      after_token <- if (
        token_end < nchar(normalized_text)
      ) {
        substr(
          normalized_text,
          token_end + 1L,
          min(
            nchar(normalized_text),
            token_end + 60L
          )
        )
      } else {
        ""
      }

      # Exempt the confidence LEVEL, not result percentages in general.
      # Examples:
      #   95% CI
      #   95 % CI
      #   95% confidence interval
      #   99% confidence intervals
      grepl(
        "^\\s*%\\s*(?:CI\\b|confidence\\s+intervals?\\b)",
        after_token,
        ignore.case = TRUE,
        perl = TRUE
      )
    },
    logical(1)
  )
}


temporal_descriptor_token_flags <- function(text, numeric_tokens) {
  if (is.null(numeric_tokens) || nrow(numeric_tokens) == 0L) return(logical(0))
  normalized_text <- normalize_numeric_text(text)

  vapply(
    seq_len(nrow(numeric_tokens)),
    function(i) {
      token_end <- numeric_tokens$end[[i]]
      if (is.na(token_end)) return(FALSE)

      after_token <- if (token_end < nchar(normalized_text)) {
        substr(normalized_text, token_end + 1L, min(nchar(normalized_text), token_end + 45L))
      } else {
        ""
      }

      grepl(
        "^\\s*(?:[-–—]\\s*)?(?:seconds?|secs?|minutes?|mins?|hours?|hrs?|days?|weeks?|months?|years?)\\b",
        after_token,
        ignore.case = TRUE,
        perl = TRUE
      )
    },
    logical(1)
  )
}

context_terms <- function(text) {
  x <- tolower(strip_numeric_citations(text))
  x <- gsub("[^a-z]+", " ", x, perl = TRUE)
  terms <- unlist(strsplit(x, "[[:space:]]+", perl = TRUE))
  terms <- terms[nchar(terms) >= 4L]
  terms <- ifelse(nchar(terms) > 4L & grepl("s$", terms), sub("s$", "", terms), terms)

  stop_terms <- c(
    "with","from","that","this","were","have","into","than","then","their",
    "there","which","when","review","analysi","result","section","reported",
    "current","primary"
  )
  unique(setdiff(terms, stop_terms))
}

contextual_fact_matches <- function(sentence, claim_tokens, canonical_facts) {
  if (
    is.null(claim_tokens) || nrow(claim_tokens) == 0L ||
    is.null(canonical_facts) || nrow(canonical_facts) == 0L
  ) {
    return(rep(NA_character_, nrow(claim_tokens)))
  }

  sentence_terms <- context_terms(sentence)

  vapply(
    seq_len(nrow(claim_tokens)),
    function(i) {
      value <- claim_tokens$value[[i]]
      digits <- claim_tokens$digits_after_decimal[[i]]

      candidate_idx <- which(
        vapply(
          canonical_facts$canonical_value,
          function(cv) numeric_equivalent(value, cv, digits),
          logical(1)
        )
      )

      if (length(candidate_idx) == 0L) return(NA_character_)

      scores <- vapply(
        candidate_idx,
        function(idx) {
          length(
            intersect(
              sentence_terms,
              context_terms(
                canonical_facts$source_text[[idx]]
              )
            )
          )
        },
        integer(1)
      )

      count_metric <- grepl(
        "^(?:study_count::|aggregate_count::|aggregate_total_sample|derived_composite_count::)",
        canonical_facts$metric[candidate_idx],
        perl = TRUE
      )

      sentence_is_count_context <- any(
        c(
          "participant",
          "patient",
          "subject",
          "person",
          "sample",
          "cohort",
          "observation"
        ) %in% sentence_terms
      )

      adjusted_scores <- scores +
        ifelse(
          count_metric &
            sentence_is_count_context,
          1L,
          0L
        )

      best <- which.max(adjusted_scores)

      if (length(best) == 0L) return(NA_character_)

      required_score <- if (count_metric[[best]]) {
        1L
      } else {
        2L
      }

      if (adjusted_scores[[best]] < required_score) {
        return(NA_character_)
      }

      canonical_facts$fact_id[candidate_idx[[best]]]
    },
    character(1)
  )
}

numeric_parser_self_test <- extract_numeric_tokens(
  "RR 0.858; 95% CI 0.011-70.038; mean difference -1.25."
)

expected_numeric_self_test <- c(
  0.858,
  95,
  0.011,
  70.038,
  -1.25
)

if (
  nrow(numeric_parser_self_test) !=
    length(expected_numeric_self_test) ||
    any(
      abs(
        numeric_parser_self_test$value -
          expected_numeric_self_test
      ) >
        1e-12
    )
) {
  stop(
    "Internal numerical-token parser self-test failed.",
    call. = FALSE
  )
}

confidence_test_sentence <- paste(
  "There were 95 participants,",
  "and the estimate was 0.858 with a",
  "95% confidence interval of 0.0105-70.0;",
  "a 99 % CI was also described hypothetically."
)

confidence_test_tokens <- extract_numeric_tokens(
  confidence_test_sentence
)

confidence_test_flags <- confidence_level_token_flags(
  confidence_test_sentence,
  confidence_test_tokens
)

expected_confidence_flags <- c(
  FALSE,  # actual value: 95 participants
  FALSE,  # effect estimate: 0.858
  TRUE,   # 95% confidence interval
  FALSE,  # lower CI
  FALSE,  # upper CI
  TRUE    # 99 % CI
)

if (
  length(confidence_test_flags) !=
    length(expected_confidence_flags) ||
    !identical(
      as.logical(confidence_test_flags),
      expected_confidence_flags
    )
) {
  stop(
    "Internal confidence-level exemption self-test failed.",
    call. = FALSE
  )
}



metadata_strip_self_test <- strip_machine_numeric_metadata(
  paste(
    "<!-- ARTIFACT: A_12AB34CD -->",
    "STUDY_000123",
    "[[PMID:12345678]]",
    "Actual sample 613 and 30-day mortality."
  )
)

if (
  grepl("12AB34CD|000123|12345678", metadata_strip_self_test, perl = TRUE) ||
  !grepl("613", metadata_strip_self_test, fixed = TRUE)
) {
  stop("Internal machine-metadata numeric stripping self-test failed.", call. = FALSE)
}

temporal_test_text <- "The 30-day outcome and 5-year survival were assessed; the sample size was 613."
temporal_test_tokens <- extract_numeric_tokens(temporal_test_text)
temporal_test_flags <- temporal_descriptor_token_flags(temporal_test_text, temporal_test_tokens)

if (!identical(as.logical(temporal_test_flags), c(TRUE, TRUE, FALSE))) {
  stop("Internal temporal-descriptor numeric exemption self-test failed.", call. = FALSE)
}

numeric_equivalent <- function(
    claimed,
    canonical,
    claimed_digits
) {
  if (
    is.na(claimed) ||
    is.na(canonical)
  ) {
    return(FALSE)
  }

  if (
    isTRUE(
      all.equal(
        claimed,
        canonical,
        tolerance = numeric_relative_tolerance
      )
    )
  ) {
    return(TRUE)
  }

  if (
    is.na(claimed_digits) ||
    claimed_digits < 0L
  ) {
    claimed_digits <- 6L
  }

  rounded <- round(
    canonical,
    digits = claimed_digits
  )

  abs(
    claimed -
      rounded
  ) <=
    max(
      10^(
        -claimed_digits
      ) / 2,
      .Machine$double.eps
    )
}


contextual_match_test_facts <- tibble::tibble(
  fact_id = c("TEST_F1", "TEST_F2"),
  source = c("test", "test"),
  analysis_group = c("test", "test"),
  metric = c(
    "aggregate_total_sample_across_arms",
    "other"
  ),
  canonical_value = c(613, 613),
  unit = c("", ""),
  source_text = c(
    "Analysis group test total participants/sample across two verified complementary group denominators = 613.",
    "An unrelated appendix entry happened to contain the value 613."
  )
)

contextual_match_test_tokens <- extract_numeric_tokens(
  "Across 613 participants, the mortality synthesis was highly imprecise."
)

contextual_match_test <- contextual_fact_matches(
  "Across 613 participants, the mortality synthesis was highly imprecise.",
  contextual_match_test_tokens,
  contextual_match_test_facts
)

if (length(contextual_match_test) != 1L || contextual_match_test[[1]] != "TEST_F1") {
  stop("Internal contextual canonical-number matching self-test failed.", call. = FALSE)
}


derive_composite_count_facts <- function(
    numeric_sentences,
    canonical_facts
) {
  if (
    is.null(numeric_sentences) ||
      nrow(numeric_sentences) == 0L ||
      is.null(canonical_facts) ||
      nrow(canonical_facts) == 0L
  ) {
    return(
      canonical_facts[
        0,
        ,
        drop = FALSE
      ]
    )
  }

  derived_rows <- list()

  canonical_integer_idx <- which(
    !is.na(canonical_facts$canonical_value) &
      canonical_facts$canonical_value > 0 &
      abs(
        canonical_facts$canonical_value -
          round(
            canonical_facts$canonical_value
          )
      ) <
        1e-9
  )

  if (length(canonical_integer_idx) == 0L) {
    return(
      canonical_facts[
        0,
        ,
        drop = FALSE
      ]
    )
  }

  for (row_index in seq_len(nrow(numeric_sentences))) {
    sentence <- numeric_sentences$sentence[[row_index]]

    sentence_terms <- context_terms(
      sentence
    )

    count_context <- any(
      c(
        "participant",
        "patient",
        "subject",
        "person",
        "sample",
        "cohort",
        "observation"
      ) %in% sentence_terms
    )

    if (!count_context) {
      next
    }

    tokens <- extract_numeric_tokens(
      sentence
    )

    if (nrow(tokens) < 3L) {
      next
    }

    temporal_flags <- temporal_descriptor_token_flags(
      sentence,
      tokens
    )

    confidence_flags <- confidence_level_token_flags(
      sentence,
      tokens
    )

    integer_flags <- (
      !is.na(tokens$value) &
        tokens$value > 0 &
        abs(
          tokens$value -
            round(
              tokens$value
            )
        ) <
          1e-9
    )

    eligible_token_idx <- which(
      integer_flags &
        !temporal_flags &
        !confidence_flags
    )

    if (length(eligible_token_idx) < 3L) {
      next
    }

    component_is_canonical <- vapply(
      eligible_token_idx,
      function(token_idx) {
        any(
          abs(
            canonical_facts$canonical_value[
              canonical_integer_idx
            ] -
              tokens$value[[token_idx]]
          ) <
            1e-9
        )
      },
      logical(1)
    )

    canonical_component_idx <- eligible_token_idx[
      component_is_canonical
    ]

    if (length(canonical_component_idx) < 2L) {
      next
    }

    component_pairs <- utils::combn(
      canonical_component_idx,
      2L,
      simplify = FALSE
    )

    for (target_idx in eligible_token_idx) {
      target_value <- tokens$value[[target_idx]]

      valid_pairs <- Filter(
        function(pair_idx) {
          !(target_idx %in% pair_idx) &&
            abs(
              sum(
                tokens$value[
                  pair_idx
                ]
              ) -
                target_value
            ) <
              1e-9
        },
        component_pairs
      )

      # Ambiguous arithmetic is never auto-accepted.
      if (length(valid_pairs) != 1L) {
        next
      }

      pair_idx <- valid_pairs[[1]]
      component_values <- tokens$value[pair_idx]

      component_groups <- lapply(
        component_values,
        function(component_value) {
          unique(
            canonical_facts$analysis_group[
              abs(
                canonical_facts$canonical_value -
                  component_value
              ) <
                1e-9
            ]
          )
        }
      )

      common_groups <- Reduce(
        intersect,
        component_groups
      )

      common_groups <- common_groups[
        !is.na(common_groups) &
          nzchar(common_groups)
      ]

      if (length(common_groups) == 0L) {
        next
      }

      derived_rows[[
        length(derived_rows) + 1L
      ]] <- tibble::tibble(
        fact_id = "",
        source = "R40_DETERMINISTIC_COMPOSITION",
        analysis_group = common_groups[[1]],
        metric = "derived_composite_count::sum_of_two_verified_components",
        canonical_value = target_value,
        unit = "",
        source_text = paste(
          "Verified participant/sample total",
          target_value,
          "equals two independently canonical component counts",
          paste(
            component_values,
            collapse = " + "
          ),
          "explicitly stated in the same manuscript sentence.",
          "Sentence:",
          sentence
        )
      )
    }
  }

  if (length(derived_rows) == 0L) {
    return(
      canonical_facts[
        0,
        ,
        drop = FALSE
      ]
    )
  }

  out <- dplyr::bind_rows(
    derived_rows
  ) |>
    dplyr::distinct(
      analysis_group,
      metric,
      canonical_value,
      .keep_all = TRUE
    )

  out$fact_id <- paste0(
    "FD",
    sprintf(
      "%04d",
      seq_len(
        nrow(out)
      )
    )
  )

  out
}

derived_composite_facts <- derive_composite_count_facts(
  numeric_sentences,
  canonical_facts
)

if (nrow(derived_composite_facts) > 0L) {
  canonical_facts <- dplyr::bind_rows(
    canonical_facts,
    derived_composite_facts
  )
}

# Regression test: the total is accepted only because both component counts
# are independently canonical and explicitly present in the same sentence.
composite_test_facts <- tibble::tibble(
  fact_id = c(
    "T1",
    "T2"
  ),
  source = c(
    "test",
    "test"
  ),
  analysis_group = c(
    "test_group",
    "test_group"
  ),
  metric = c(
    "aggregate_count::group_alpha",
    "aggregate_count::group_beta"
  ),
  canonical_value = c(
    137,
    83
  ),
  unit = c(
    "",
    ""
  ),
  source_text = c(
    "Verified participant/sample count 137.",
    "Verified participant/sample count 83."
  )
)

composite_test_sentences <- tibble::tibble(
  sentence_id = "TEST_001",
  section = "Results",
  sentence = paste(
    "The analysis included 220 participants,",
    "with 137 in one group and 83 in the other group;",
    "follow-up was 30-day."
  )
)

composite_test_result <- derive_composite_count_facts(
  composite_test_sentences,
  composite_test_facts
)

if (
  nrow(composite_test_result) != 1L ||
    abs(
      composite_test_result$canonical_value[[1]] -
        220
    ) >
      1e-9
) {
  stop(
    "Internal deterministic composite-count derivation self-test failed.",
    call. = FALSE
  )
}

# Persist the augmented canonical registry for a complete audit trail.
readr::write_csv(
  canonical_facts,
  canonical_fact_file,
  na = ""
)

fact_lookup <- split(
  canonical_facts,
  canonical_facts$fact_id
)

numeric_audit_rows <- list()

for (i in seq_len(nrow(numeric_mapping))) {
  row <- numeric_mapping[
    i,
    ,
    drop = FALSE
  ]

  classification <- toupper(
    trimws(
      row$classification[[1]]
    )
  )

  if (
    !identical(
      classification,
      "CURRENT_REVIEW"
    )
  ) {
    numeric_audit_rows[[
      length(numeric_audit_rows) + 1L
    ]] <- tibble::tibble(
      sentence_id = row$sentence_id[[1]],
      section = row$section[[1]],
      sentence = row$sentence[[1]],
      classification = classification,
      mapped_fact_ids = row$fact_ids[[1]],
      status = "NOT_APPLICABLE",
      claimed_values = "",
      canonical_values = "",
      contextual_fact_ids = "",
      issue = ""
    )
    next
  }

  ids <- unlist(
    strsplit(
      safe_chr(
        row$fact_ids,
        ""
      ),
      ";",
      fixed = TRUE
    )
  )

  ids <- trimws(ids)
  ids <- ids[
    nzchar(ids)
  ]

  invalid_ids <- setdiff(
    ids,
    canonical_facts$fact_id
  )

  if (length(invalid_ids) > 0L) {
    numeric_audit_rows[[
      length(numeric_audit_rows) + 1L
    ]] <- tibble::tibble(
      sentence_id = row$sentence_id[[1]],
      section = row$section[[1]],
      sentence = row$sentence[[1]],
      classification = classification,
      mapped_fact_ids = row$fact_ids[[1]],
      status = "FAIL",
      claimed_values = paste(
        extract_numeric_tokens(
          row$sentence[[1]]
        )$token,
        collapse = ";"
      ),
      canonical_values = "",
      contextual_fact_ids = "",
      issue = paste0(
        "Invalid canonical fact ID(s): ",
        paste(
          invalid_ids,
          collapse = ", "
        )
      )
    )
    next
  }

  claim_tokens <- extract_numeric_tokens(
    row$sentence[[1]]
  )

  fact_data <- if (length(ids) == 0L) {
    canonical_facts[
      0,
      ,
      drop = FALSE
    ]
  } else {
    canonical_facts |>
      dplyr::filter(
        fact_id %in% ids
      )
  }

  canonical_values <- fact_data$canonical_value

  matched_claim <- rep(
    FALSE,
    nrow(claim_tokens)
  )

  matched_fact <- rep(
    FALSE,
    length(canonical_values)
  )

  if (
    nrow(claim_tokens) > 0L &&
    length(canonical_values) > 0L
  ) {
    for (cidx in seq_len(nrow(claim_tokens))) {
      for (fidx in seq_along(canonical_values)) {
        if (
          !matched_fact[[fidx]] &&
          numeric_equivalent(
            claim_tokens$value[[cidx]],
            canonical_values[[fidx]],
            claim_tokens$digits_after_decimal[[cidx]]
          )
        ) {
          matched_claim[[cidx]] <- TRUE
          matched_fact[[fidx]] <- TRUE
          break
        }
      }
    }
  }

  # Confidence-level labels and calendar years can appear in
  # current-review sentences without being canonical result values.
  #
  # IMPORTANT:
  # Do NOT globally exempt the numeric value 95. Only exempt a token when
  # its own location is immediately followed by "% CI" or
  # "% confidence interval(s)". This prevents false negatives when an
  # actual result value of 95 appears elsewhere in the same sentence.
  exempt_claim <- rep(
    FALSE,
    nrow(claim_tokens)
  )

  if (nrow(claim_tokens) > 0L) {
    confidence_level_claim <- confidence_level_token_flags(
      row$sentence[[1]],
      claim_tokens
    )

    calendar_year_claim <- (
      claim_tokens$value >= 1900 &
        claim_tokens$value <= 2100
    )

    temporal_descriptor_claim <- temporal_descriptor_token_flags(
      row$sentence[[1]],
      claim_tokens
    )

    contextual_fact_ids <- contextual_fact_matches(
      row$sentence[[1]],
      claim_tokens,
      canonical_facts
    )

    contextual_verified_claim <- !is.na(contextual_fact_ids)

    exempt_claim <- (
      confidence_level_claim |
        calendar_year_claim |
        temporal_descriptor_claim |
        contextual_verified_claim
    )
  } else {
    contextual_fact_ids <- character(0)
  }

  unmatched_nonexempt <- which(
    !matched_claim &
      !exempt_claim
  )

  if (length(unmatched_nonexempt) > 0L) {
    status <- "FAIL"
    issue <- paste0(
      "Unmatched scientific current-review numeric token(s): ",
      paste(
        claim_tokens$token[
          unmatched_nonexempt
        ],
        collapse = ", "
      )
    )
  } else {
    status <- "PASS"
    issue <- ""
  }

  numeric_audit_rows[[
    length(numeric_audit_rows) + 1L
  ]] <- tibble::tibble(
    sentence_id = row$sentence_id[[1]],
    section = row$section[[1]],
    sentence = row$sentence[[1]],
    classification = classification,
    mapped_fact_ids = row$fact_ids[[1]],
    status = status,
    claimed_values = paste(
      claim_tokens$token,
      collapse = ";"
    ),
    canonical_values = paste(
      canonical_values,
      collapse = ";"
    ),
    contextual_fact_ids = paste(
      unique(contextual_fact_ids[!is.na(contextual_fact_ids)]),
      collapse = ";"
    ),
    issue = issue
  )
}

numeric_audit <- dplyr::bind_rows(
  numeric_audit_rows
)

readr::write_csv(
  numeric_audit,
  numeric_audit_file,
  na = ""
)


# ============================================================
# 11. Citation number parsing
# ============================================================

extract_citation_numbers <- function(text) {
  matches <- stringr::str_match_all(
    text,
    "\\[([0-9]+(?:\\s*[-–—,]\\s*[0-9]+)*)\\]"
  )[[1]]

  if (
    is.null(matches) ||
    nrow(matches) == 0L
  ) {
    return(integer(0))
  }

  expanded <- integer(0)

  for (expr in matches[, 2]) {
    parts <- trimws(
      unlist(
        strsplit(
          expr,
          ",",
          fixed = TRUE
        )
      )
    )

    for (part in parts) {
      if (
        grepl(
          "^[0-9]+\\s*[-–—]\\s*[0-9]+$",
          part
        )
      ) {
        ends <- safe_num(
          unlist(
            strsplit(
              part,
              "\\s*[-–—]\\s*",
              perl = TRUE
            )
          )
        )

        if (
          length(ends) == 2L &&
          all(!is.na(ends))
        ) {
          expanded <- c(
            expanded,
            seq.int(
              min(ends),
              max(ends)
            )
          )
        }
      } else {
        value <- suppressWarnings(
          as.integer(part)
        )

        if (!is.na(value)) {
          expanded <- c(
            expanded,
            value
          )
        }
      }
    }
  }

  sort(
    unique(expanded)
  )
}

citation_parser_self_test <- extract_citation_numbers(
  "Example [1,3–4] and [2] and [5—6]."
)

if (
  !identical(
    citation_parser_self_test,
    1:6
  )
) {
  stop(
    "Internal citation-number parser self-test failed.",
    call. = FALSE
  )
}

references_text <- extract_section(
  manuscript_text,
  "References"
)

reference_source <- "manuscript_draft.md"

if (
  !nzchar(references_text) &&
    file_nonempty(r39_references_file)
) {
  references_text <- trimws(
    read_text_file(
      r39_references_file
    )
  )

  # references_vancouver.md generated by R/39 may itself begin with a
  # References heading. Strip it if present.
  references_from_file <- extract_section(
    references_text,
    "References"
  )

  if (nzchar(references_from_file)) {
    references_text <- references_from_file
  }

  reference_source <- "references_vancouver.md"
}

if (!nzchar(references_text)) {
  stop(
    paste(
      "R/40 could not resolve the manuscript reference list.",
      "Checked manuscript_draft.md and references_vancouver.md."
    ),
    call. = FALSE
  )
}

body_without_references <- remove_references_and_after(
  manuscript_text
)

cat(
  "R/40 reference source: ",
  reference_source,
  "\n",
  sep = ""
)

cited_numbers <- extract_citation_numbers(
  body_without_references
)

reference_lines <- strsplit(
  references_text,
  "\n",
  fixed = TRUE
)[[1]]

reference_lines <- reference_lines[
  grepl(
    "^[0-9]+\\.\\s+",
    trimws(reference_lines)
  )
]

reference_numbers <- suppressWarnings(
  as.integer(
    sub(
      "^([0-9]+)\\..*$",
      "\\1",
      trimws(reference_lines)
    )
  )
)

reference_numbers <- reference_numbers[
  !is.na(reference_numbers)
]

map_number_col <- first_existing_col(
  citation_map,
  c(
    "citation_number",
    "reference_number",
    "number"
  )
)

map_pmid_col <- first_existing_col(
  citation_map,
  c(
    "pmid",
    "PMID"
  )
)

if (
  is.na(map_number_col) ||
  is.na(map_pmid_col)
) {
  stop(
    "R/39 citation_number_map.csv lacks citation_number and/or pmid.",
    call. = FALSE
  )
}

map_numbers <- suppressWarnings(
  as.integer(
    citation_map[[map_number_col]]
  )
)

map_pmids <- as.character(
  citation_map[[map_pmid_col]]
)

if (
  any(is.na(map_numbers)) ||
  any(map_numbers < 1L)
) {
  stop(
    "R/39 citation_number_map.csv contains invalid citation numbers.",
    call. = FALSE
  )
}

if (anyDuplicated(map_numbers) > 0L) {
  stop(
    "R/39 citation_number_map.csv contains duplicate citation numbers.",
    call. = FALSE
  )
}

if (
  any(
    is.na(map_pmids) |
      !nzchar(trimws(map_pmids))
  )
) {
  stop(
    "R/39 citation_number_map.csv contains blank PMID values.",
    call. = FALSE
  )
}

if (anyDuplicated(map_pmids) > 0L) {
  stop(
    "R/39 citation_number_map.csv contains duplicate PMID mappings.",
    call. = FALSE
  )
}

expected_map_numbers <- seq_len(
  length(map_numbers)
)

if (
  length(map_numbers) > 0L &&
    !identical(
      sort(map_numbers),
      expected_map_numbers
    )
) {
  stop(
    "R/39 citation numbers are not contiguous from 1 through N.",
    call. = FALSE
  )
}

citation_integrity_rows <- list()

all_numbers <- sort(
  unique(
    c(
      cited_numbers,
      reference_numbers,
      map_numbers
    )
  )
)

for (num in all_numbers) {
  cited <- num %in% cited_numbers
  in_reference_list <- num %in% reference_numbers
  in_map <- num %in% map_numbers

  pmid <- if (in_map) {
    safe_chr(
      map_pmids[
        match(
          num,
          map_numbers
        )
      ],
      ""
    )
  } else {
    ""
  }

  in_selected <- nzchar(pmid) &&
    pmid %in%
      as.character(
        selected_literature$pmid
      )

  reference_line <- ""

  if (in_reference_list) {
    line_hit <- reference_lines[
      reference_numbers == num
    ]

    if (length(line_hit) > 0L) {
      reference_line <- trimws(
        line_hit[[1]]
      )
    }
  }

  reference_pmid_match <- if (
    in_reference_list &&
    nzchar(pmid)
  ) {
    grepl(
      paste0(
        "PMID:",
        pmid,
        "\\b"
      ),
      reference_line,
      perl = TRUE
    )
  } else {
    FALSE
  }

  status <- if (
    cited &&
    in_reference_list &&
    in_map &&
    in_selected &&
    reference_pmid_match
  ) {
    "PASS"
  } else {
    "FAIL"
  }

  issues <- character(0)

  if (cited && !in_reference_list) {
    issues <- c(
      issues,
      "Cited number missing from reference list"
    )
  }

  if (in_reference_list && !in_map) {
    issues <- c(
      issues,
      "Reference number missing from citation map"
    )
  }

  if (in_map && !in_selected) {
    issues <- c(
      issues,
      "Mapped PMID missing from verified selected literature"
    )
  }

  if (
    in_reference_list &&
    in_map &&
    !reference_pmid_match
  ) {
    issues <- c(
      issues,
      "Reference-list PMID does not match citation map"
    )
  }

  if (!cited && in_reference_list) {
    issues <- c(
      issues,
      "Reference is present but not cited"
    )
  }

  citation_integrity_rows[[
    length(citation_integrity_rows) + 1L
  ]] <- tibble::tibble(
    citation_number = num,
    cited_in_text = cited,
    present_in_reference_list = in_reference_list,
    present_in_citation_map = in_map,
    pmid = pmid,
    present_in_verified_library = in_selected,
    reference_pmid_matches_map = reference_pmid_match,
    status = status,
    issue = paste(
      issues,
      collapse = "; "
    )
  )
}

citation_integrity <- dplyr::bind_rows(
  citation_integrity_rows
)

readr::write_csv(
  citation_integrity,
  citation_integrity_file,
  na = ""
)


# ============================================================
# 12. PubMed reference re-verification
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
  )

  req <- do.call(
    httr2::req_url_query,
    c(
      list(req),
      query
    )
  )

  req <- req |>
    httr2::req_user_agent(
      if (nzchar(ncbi_email)) {
        paste0(
          "Universal-SRMA-Toolkit-QC/1.0 (",
          ncbi_email,
          ")"
        )
      } else {
        "Universal-SRMA-Toolkit-QC/1.0"
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
    inherits(
      hit,
      "xml_missing"
    )
  ) {
    return("")
  }

  trimws(
    xml2::xml_text(
      hit
    )
  )
}

parse_pubmed_basic <- function(xml_text) {
  doc <- xml2::read_xml(
    xml_text
  )

  articles <- xml2::xml_find_all(
    doc,
    ".//PubmedArticle"
  )

  if (length(articles) == 0L) {
    return(
      tibble::tibble(
        pmid = character(),
        pubmed_title = character(),
        pubmed_doi = character()
      )
    )
  }

  rows <- lapply(
    articles,
    function(node) {
      pmid <- xml_text_one(
        node,
        ".//MedlineCitation/PMID"
      )

      title <- xml_text_one(
        node,
        ".//Article/ArticleTitle"
      )

      doi_node <- xml2::xml_find_first(
        node,
        ".//PubmedData/ArticleIdList/ArticleId[@IdType='doi']"
      )

      doi <- if (
        inherits(
          doi_node,
          "xml_missing"
        )
      ) {
        ""
      } else {
        trimws(
          xml2::xml_text(
            doi_node
          )
        )
      }

      tibble::tibble(
        pmid = pmid,
        pubmed_title = title,
        pubmed_doi = doi
      )
    }
  )

  dplyr::bind_rows(
    rows
  )
}

cited_pmids <- unique(
  citation_integrity$pmid[
    citation_integrity$cited_in_text &
      nzchar(
        citation_integrity$pmid
      )
  ]
)

pubmed_reverification <- tibble::tibble()

if (
  reverify_pubmed &&
  length(cited_pmids) > 0L
) {
  batch_size <- 100L

  batches <- split(
    cited_pmids,
    ceiling(
      seq_along(cited_pmids) /
        batch_size
    )
  )

  fetched_rows <- list()

  for (batch in batches) {
    xml_text <- pubmed_fetch_xml(
      batch
    )

    fetched_rows[[
      length(fetched_rows) + 1L
    ]] <- parse_pubmed_basic(
      xml_text
    )
  }

  fetched <- dplyr::bind_rows(
    fetched_rows
  )

  selected_subset <- selected_literature |>
    dplyr::filter(
      as.character(pmid) %in%
        cited_pmids
    ) |>
    dplyr::mutate(
      pmid = as.character(pmid)
    )

  missing_selected_pmids <- setdiff(
    cited_pmids,
    selected_subset$pmid
  )

  if (length(missing_selected_pmids) > 0L) {
    stop(
      paste0(
        "Cited PMID(s) are missing from literature_selected.csv: ",
        paste(
          missing_selected_pmids,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }

  pubmed_reverification <- selected_subset |>
    dplyr::left_join(
      fetched,
      by = "pmid"
    ) |>
    dplyr::mutate(
      pubmed_record_found = !is.na(pubmed_title) &
        nzchar(pubmed_title),
      title_match = pubmed_record_found &
        normalize_title(title) ==
        normalize_title(pubmed_title),
      stored_doi_normalized = normalize_doi(
        doi
      ),
      pubmed_doi_normalized = normalize_doi(
        pubmed_doi
      ),
      doi_match = dplyr::case_when(
        !nzchar(stored_doi_normalized) &
          !nzchar(pubmed_doi_normalized) ~ TRUE,
        nzchar(stored_doi_normalized) &
          nzchar(pubmed_doi_normalized) ~
          stored_doi_normalized ==
          pubmed_doi_normalized,
        TRUE ~ FALSE
      ),
      status = ifelse(
        pubmed_record_found &
          title_match &
          doi_match,
        "PASS",
        "FAIL"
      )
    ) |>
    dplyr::select(
      pmid,
      title,
      pubmed_title,
      doi,
      pubmed_doi,
      pubmed_record_found,
      title_match,
      doi_match,
      status
    )
} else {
  pubmed_reverification <- tibble::tibble(
    pmid = cited_pmids,
    title = "",
    pubmed_title = "",
    doi = "",
    pubmed_doi = "",
    pubmed_record_found = NA,
    title_match = NA,
    doi_match = NA,
    status = if (
      reverify_pubmed
    ) {
      "NOT_APPLICABLE"
    } else {
      "SKIPPED_BY_CONFIG"
    }
  )
}

readr::write_csv(
  pubmed_reverification,
  pubmed_reverify_file,
  na = ""
)


# ============================================================
# 13. Cross-section semantic consistency audit
# ============================================================

canonical_summary_text <- paste(
  apply(
    canonical_facts,
    1,
    function(row) {
      paste0(
        row[["fact_id"]],
        " | ",
        row[["analysis_group"]],
        " | ",
        row[["metric"]],
        "=",
        row[["canonical_value"]],
        row[["unit"]]
      )
    }
  ),
  collapse = "\n"
)

consistency_system <- paste(
  "You are a final QC reviewer for a systematic review and meta-analysis manuscript.",
  "Audit ONLY the Abstract, Results, and Discussion supplied.",
  "Use BOTH the canonical numeric facts and the full frozen R/38 Results evidence dossier as sources of truth.",
  "The R/38 dossier contains canonical descriptive/reporting tables as well as quantitative, diagnostic, study-selection, and risk-of-bias evidence.",
  "Identify direct contradictions, direction reversals, mismatched study counts or effect estimates, claims of significance inconsistent with confidence intervals/p-values, unsupported equivalence/no-difference claims, causal overstatement from observational evidence, and Discussion statements that materially distort the Results.",
  "Also compare manuscript prose against descriptive table evidence study by study. Flag claims that give a value when the corresponding canonical table says NR/not reported, claims that say NR/not available when a value is present, N/A-versus-NR misuse that changes the scientific meaning (for example, a nonexistent comparator versus an unreported comparator), incorrect author/year or study labels, and mismatched intervention/comparator or baseline-characteristic descriptions.",
  "A clear table-versus-prose mismatch or NR/N/A contradiction must be classified as CONTRADICTION or UNSUPPORTED_CLAIM, not POTENTIAL_AMBIGUITY.",
  "Do not criticize style.",
  "Do not require identical wording across sections.",
  "POTENTIAL_AMBIGUITY should be reserved for language that could mislead but is not a clear factual contradiction.",
  "Return STRICT JSON only."
)

consistency_user <- paste0(
  "PROJECT TITLE:\n",
  project_title,
  "\n\nR/38 STATUS:\n",
  r38_status,
  "\n\nCANONICAL NUMERIC RESULT FACTS:\n",
  canonical_summary_text,
  "\n\nFULL FROZEN R/38 RESULTS EVIDENCE DOSSIER (includes canonical descriptive tables):\n",
  r38_results_text,
  "\n\nABSTRACT:\n",
  abstract_text,
  "\n\nRESULTS:\n",
  results_text,
  "\n\nDISCUSSION:\n",
  discussion_text,
  "\n\nReturn JSON exactly:\n",
  "{\n",
  '  "issues": [\n',
  '    {"issue_id":"C001","severity":"CONTRADICTION|POTENTIAL_AMBIGUITY|UNSUPPORTED_CLAIM","sections":["Abstract","Results"],"statement_a":"...","statement_b":"...","canonical_fact_ids":["F0001"],"explanation":"..."}\n',
  "  ],\n",
  '  "overall":"PASS|ISSUES_FOUND"\n',
  "}\n",
  "If there are no issues, return an empty issues array and overall PASS."
)

consistency_text <- call_openai(
  consistency_system,
  consistency_user,
  "02_cross_section_consistency",
  max_output_tokens = 12000L
)

consistency_text <- strip_markdown_fence(
  consistency_text
)

consistency_json <- tryCatch(
  jsonlite::fromJSON(
    consistency_text,
    simplifyVector = FALSE
  ),
  error = function(e) NULL
)

if (
  is.null(consistency_json) ||
  is.null(consistency_json$issues) ||
  is.null(consistency_json$overall)
) {
  stop(
    "Cross-section consistency JSON could not be parsed.",
    call. = FALSE
  )
}

consistency_overall <- toupper(
  safe_chr(
    consistency_json$overall,
    ""
  )
)

if (
  !consistency_overall %in%
    c(
      "PASS",
      "ISSUES_FOUND"
    )
) {
  stop(
    paste0(
      "Cross-section semantic audit returned invalid overall status: ",
      consistency_overall
    ),
    call. = FALSE
  )
}

if (length(consistency_json$issues) == 0L) {
  consistency_audit <- tibble::tibble(
    issue_id = character(),
    severity = character(),
    sections = character(),
    statement_a = character(),
    statement_b = character(),
    canonical_fact_ids = character(),
    explanation = character()
  )
} else {
  consistency_rows <- lapply(
    consistency_json$issues,
    function(item) {
      tibble::tibble(
        issue_id = safe_chr(
          item$issue_id,
          ""
        ),
        severity = toupper(
          safe_chr(
            item$severity,
            ""
          )
        ),
        sections = paste(
          as.character(
            unlist(
              item$sections
            )
          ),
          collapse = ";"
        ),
        statement_a = safe_chr(
          item$statement_a,
          ""
        ),
        statement_b = safe_chr(
          item$statement_b,
          ""
        ),
        canonical_fact_ids = paste(
          as.character(
            unlist(
              item$canonical_fact_ids
            )
          ),
          collapse = ";"
        ),
        explanation = safe_chr(
          item$explanation,
          ""
        )
      )
    }
  )

  consistency_audit <- dplyr::bind_rows(
    consistency_rows
  )
}

allowed_semantic_severities <- c(
  "CONTRADICTION",
  "POTENTIAL_AMBIGUITY",
  "UNSUPPORTED_CLAIM"
)

if (nrow(consistency_audit) > 0L) {
  invalid_semantic_severities <- setdiff(
    unique(
      consistency_audit$severity
    ),
    allowed_semantic_severities
  )

  if (length(invalid_semantic_severities) > 0L) {
    stop(
      paste0(
        "Cross-section semantic audit returned invalid severity value(s): ",
        paste(
          invalid_semantic_severities,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
}

if (
  identical(consistency_overall, "PASS") &&
    nrow(consistency_audit) > 0L
) {
  stop(
    "Cross-section semantic audit contract mismatch: overall PASS but issues were returned.",
    call. = FALSE
  )
}

if (
  identical(consistency_overall, "ISSUES_FOUND") &&
    nrow(consistency_audit) == 0L
) {
  stop(
    "Cross-section semantic audit contract mismatch: ISSUES_FOUND but no issue rows were returned.",
    call. = FALSE
  )
}

readr::write_csv(
  consistency_audit,
  consistency_audit_file,
  na = ""
)


# ============================================================
# 14. QC status logic
# ============================================================

numeric_fail_n <- sum(
  numeric_audit$status == "FAIL",
  na.rm = TRUE
)

citation_fail_n <- sum(
  citation_integrity$status == "FAIL",
  na.rm = TRUE
)

artifact_coverage_fail_n <- if (
  nrow(artifact_coverage_audit) > 0L
) {
  sum(
    artifact_coverage_audit$status == "FAIL",
    na.rm = TRUE
  )
} else {
  0L
}

machine_identifier_fail_n <- if (
  nrow(machine_identifier_audit) > 0L
) {
  sum(
    machine_identifier_audit$status == "FAIL",
    na.rm = TRUE
  )
} else {
  0L
}

pubmed_fail_n <- if (
  nrow(pubmed_reverification) > 0L
) {
  sum(
    pubmed_reverification$status == "FAIL",
    na.rm = TRUE
  )
} else {
  0L
}

semantic_contradiction_n <- if (
  nrow(consistency_audit) > 0L
) {
  sum(
    consistency_audit$severity %in%
      c(
        "CONTRADICTION",
        "UNSUPPORTED_CLAIM"
      ),
    na.rm = TRUE
  )
} else {
  0L
}

semantic_potential_n <- if (
  nrow(consistency_audit) > 0L
) {
  sum(
    consistency_audit$severity ==
      "POTENTIAL_AMBIGUITY",
    na.rm = TRUE
  )
} else {
  0L
}

deterministic_pass <- (
  numeric_fail_n == 0L &&
  citation_fail_n == 0L &&
  pubmed_fail_n == 0L &&
  artifact_coverage_fail_n == 0L &&
  machine_identifier_fail_n == 0L
)

semantic_pass <- (
  semantic_contradiction_n == 0L &&
  (
    !semantic_fail_on_potential ||
      semantic_potential_n == 0L
  )
)

final_evidence_ready <- identical(
  r38_status,
  "FINAL_EVIDENCE_READY"
) &&
  identical(
    r39_status,
    "DRAFT_COMPLETE_READY_FOR_R40_VALIDATION"
  )

if (
  deterministic_pass &&
  semantic_pass &&
  final_evidence_ready
) {
  overall_status <- "PASS_READY_FOR_SUBMISSION_FORMATTING"
} else if (
  deterministic_pass &&
  semantic_pass &&
  !final_evidence_ready
) {
  overall_status <- "PROVISIONAL_PENDING_FINAL_ROB"
} else {
  overall_status <- "CHANGES_REQUIRED"
}


# ============================================================
# 15. QC summary
# ============================================================

qc_summary <- tibble::tibble(
  project_id = project_id,
  project_title = project_title,
  r38_status = r38_status,
  r39_status = r39_status,
  numeric_claim_failures = numeric_fail_n,
  citation_integrity_failures = citation_fail_n,
  pubmed_reverification_failures = pubmed_fail_n,
  artifact_coverage_failures = artifact_coverage_fail_n,
  machine_identifier_leak_failures = machine_identifier_fail_n,
  semantic_contradictions_or_unsupported = semantic_contradiction_n,
  semantic_potential_ambiguities = semantic_potential_n,
  deterministic_pass = deterministic_pass,
  semantic_pass = semantic_pass,
  final_evidence_ready = final_evidence_ready,
  overall_status = overall_status,
  qc_model = qc_model,
  manuscript_sha256 = sha256_file(
    manuscript_file
  ),
  checked_at = now_text()
)

readr::write_csv(
  qc_summary,
  qc_summary_file,
  na = ""
)


# ============================================================
# 16. Issues for Author Review
# ============================================================

issue_lines <- c(
  "# Issues for Author Review",
  "",
  paste0(
    "**QC status:** ",
    overall_status
  ),
  ""
)

if (numeric_fail_n > 0L) {
  issue_lines <- c(
    issue_lines,
    "## Numerical issues",
    ""
  )

  fail_rows <- numeric_audit |>
    dplyr::filter(
      status == "FAIL"
    )

  for (i in seq_len(nrow(fail_rows))) {
    issue_lines <- c(
      issue_lines,
      paste0(
        "- **",
        fail_rows$sentence_id[[i]],
        " / ",
        fail_rows$section[[i]],
        "**: ",
        fail_rows$issue[[i]],
        "  \n  > ",
        fail_rows$sentence[[i]]
      )
    )
  }

  issue_lines <- c(
    issue_lines,
    ""
  )
}

if (citation_fail_n > 0L) {
  issue_lines <- c(
    issue_lines,
    "## Citation/reference issues",
    ""
  )

  fail_rows <- citation_integrity |>
    dplyr::filter(
      status == "FAIL"
    )

  for (i in seq_len(nrow(fail_rows))) {
    issue_lines <- c(
      issue_lines,
      paste0(
        "- Reference/citation ",
        fail_rows$citation_number[[i]],
        ": ",
        fail_rows$issue[[i]],
        if (
          nzchar(
            fail_rows$pmid[[i]]
          )
        ) {
          paste0(
            " (PMID ",
            fail_rows$pmid[[i]],
            ")"
          )
        } else {
          ""
        }
      )
    )
  }

  issue_lines <- c(
    issue_lines,
    ""
  )
}

if (pubmed_fail_n > 0L) {
  issue_lines <- c(
    issue_lines,
    "## PubMed re-verification issues",
    ""
  )

  fail_rows <- pubmed_reverification |>
    dplyr::filter(
      status == "FAIL"
    )

  for (i in seq_len(nrow(fail_rows))) {
    issue_lines <- c(
      issue_lines,
      paste0(
        "- PMID ",
        fail_rows$pmid[[i]],
        ": record_found=",
        fail_rows$pubmed_record_found[[i]],
        ", title_match=",
        fail_rows$title_match[[i]],
        ", DOI_match=",
        fail_rows$doi_match[[i]],
        "."
      )
    )
  }

  issue_lines <- c(
    issue_lines,
    ""
  )
}

if (artifact_coverage_fail_n > 0L) {
  issue_lines <- c(
    issue_lines,
    "## Table/Figure artifact coverage issues",
    ""
  )

  fail_rows <- artifact_coverage_audit |>
    dplyr::filter(
      status == "FAIL"
    )

  for (i in seq_len(nrow(fail_rows))) {
    issue_lines <- c(
      issue_lines,
      paste0(
        "- **",
        fail_rows$artifact_id[[i]],
        " — ",
        fail_rows$title[[i]],
        "**: ",
        fail_rows$issue[[i]],
        " (source: `",
        fail_rows$source_relative_path[[i]],
        "`)"
      )
    )
  }

  issue_lines <- c(
    issue_lines,
    ""
  )
}

if (machine_identifier_fail_n > 0L) {
  issue_lines <- c(
    issue_lines,
    "## Internal machine identifier leakage",
    ""
  )

  for (i in seq_len(nrow(machine_identifier_audit))) {
    issue_lines <- c(
      issue_lines,
      paste0(
        "- **",
        machine_identifier_audit$pattern_name[[i]],
        "**: ",
        machine_identifier_audit$issue[[i]]
      )
    )
  }

  issue_lines <- c(
    issue_lines,
    ""
  )
}

if (
  semantic_contradiction_n > 0L ||
  semantic_potential_n > 0L
) {
  issue_lines <- c(
    issue_lines,
    "## Cross-section consistency issues",
    ""
  )

  for (i in seq_len(nrow(consistency_audit))) {
    issue_lines <- c(
      issue_lines,
      paste0(
        "- **",
        consistency_audit$severity[[i]],
        " ",
        consistency_audit$issue_id[[i]],
        "** (",
        consistency_audit$sections[[i]],
        "): ",
        consistency_audit$explanation[[i]],
        if (
          nzchar(
            consistency_audit$statement_a[[i]]
          )
        ) {
          paste0(
            "  \n  > ",
            consistency_audit$statement_a[[i]]
          )
        } else {
          ""
        },
        if (
          nzchar(
            consistency_audit$statement_b[[i]]
          )
        ) {
          paste0(
            "  \n  > ",
            consistency_audit$statement_b[[i]]
          )
        } else {
          ""
        }
      )
    )
  }

  issue_lines <- c(
    issue_lines,
    ""
  )
}

if (
  numeric_fail_n == 0L &&
  citation_fail_n == 0L &&
  pubmed_fail_n == 0L &&
  artifact_coverage_fail_n == 0L &&
  machine_identifier_fail_n == 0L &&
  semantic_contradiction_n == 0L &&
  semantic_potential_n == 0L
) {
  issue_lines <- c(
    issue_lines,
    "No numerical, citation-integrity, PubMed-verification, artifact-coverage, machine-identifier, or cross-section consistency issue was detected.",
    ""
  )
}

if (!final_evidence_ready) {
  issue_lines <- c(
    issue_lines,
    "## Remaining workflow gate",
    "",
    "- R/38 and/or R/39 is still provisional. Final submission readiness requires final RoB completion followed by refreshed R/38 and R/39 outputs.",
    ""
  )
}

write_text_file(
  issue_lines,
  issues_file
)


# ============================================================
# 17. PASS-only submission candidate
# ============================================================

if (
  identical(
    overall_status,
    "PASS_READY_FOR_SUBMISSION_FORMATTING"
  )
) {
  clean_submission_text <- stringr::str_replace_all(
    manuscript_text,
    "<!--\\s*ARTIFACT:\\s*[A-Za-z0-9_-]+\\s*-->",
    ""
  )

  clean_submission_text <- gsub(
    "\\n[[:space:]]*\\n[[:space:]]*\\n+",
    "\\n\\n",
    clean_submission_text,
    perl = TRUE
  )

  write_text_file(
    clean_submission_text,
    submission_candidate_file
  )
} else if (file.exists(submission_candidate_file)) {
  unlink(
    submission_candidate_file
  )
}


# ============================================================
# 18. QC workbook
# ============================================================

wb <- openxlsx::createWorkbook()

sheet_payloads <- list(
  QC_Summary = qc_summary,
  Canonical_Numeric_Facts = canonical_facts,
  Numeric_Sentences = numeric_sentences,
  Numeric_Claim_Mapping = numeric_mapping,
  Numeric_Claim_Audit = numeric_audit,
  Citation_Integrity = citation_integrity,
  PubMed_Reverification = pubmed_reverification,
  Artifact_Coverage = artifact_coverage_audit,
  Machine_ID_Leakage = machine_identifier_audit,
  Cross_Section_Consistency = consistency_audit
)

for (sheet_name in names(sheet_payloads)) {
  openxlsx::addWorksheet(
    wb,
    substr(
      sheet_name,
      1,
      31
    )
  )

  openxlsx::writeData(
    wb,
    substr(
      sheet_name,
      1,
      31
    ),
    sheet_payloads[[sheet_name]]
  )
}

openxlsx::saveWorkbook(
  wb,
  qc_workbook_file,
  overwrite = TRUE
)


# ============================================================
# 19. State
# ============================================================

state <- list(
  manuscript_qc = list(
    version = "1.6.0",
    generated_at = now_text(),
    project_id = project_id,
    project_title = project_title,
    r38_status = r38_status,
    r39_status = r39_status,
    overall_status = overall_status,
    deterministic_pass = deterministic_pass,
    semantic_pass = semantic_pass,
    final_evidence_ready = final_evidence_ready,
    numeric_claim_failures = numeric_fail_n,
    citation_integrity_failures = citation_fail_n,
    pubmed_reverification_failures = pubmed_fail_n,
    artifact_coverage_failures = artifact_coverage_fail_n,
    machine_identifier_leak_failures = machine_identifier_fail_n,
    semantic_contradictions_or_unsupported = semantic_contradiction_n,
    semantic_potential_ambiguities = semantic_potential_n,
    qc_model = qc_model,
    statistical_recalculation = FALSE,
    manuscript_sha256 = sha256_file(
      manuscript_file
    ),
    submission_candidate_created = identical(
      overall_status,
      "PASS_READY_FOR_SUBMISSION_FORMATTING"
    )
  )
)

yaml::write_yaml(
  state,
  state_file
)


# ============================================================
# 20. Report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/40 — Final Manuscript QC",
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
    "QC model: ",
    qc_model
  ),
  "",
  "SOURCE-OF-TRUTH CONTRACT",
  "--------------------------------------------------",
  "Canonical statistics: R/27",
  "Canonical manuscript evidence freeze: R/38",
  "Draft manuscript: R/39",
  "Statistical recalculation in R/40: NO",
  "",
  "NUMERICAL CONSISTENCY",
  "--------------------------------------------------",
  paste0(
    "Numeric claim failures: ",
    numeric_fail_n
  ),
  paste0(
    "Numeric deterministic PASS: ",
    numeric_fail_n == 0L
  ),
  "",
  "CITATION / REFERENCE INTEGRITY",
  "--------------------------------------------------",
  paste0(
    "Citation integrity failures: ",
    citation_fail_n
  ),
  paste0(
    "PubMed reverification enabled: ",
    reverify_pubmed
  ),
  paste0(
    "PubMed reverification failures: ",
    pubmed_fail_n
  ),
  "",
  "TABLE / FIGURE ARTIFACT COVERAGE",
  "--------------------------------------------------",
  paste0(
    "Coverage-required artifacts: ",
    nrow(required_artifacts)
  ),
  paste0(
    "Artifact coverage failures: ",
    artifact_coverage_fail_n
  ),
  "",
  "INTERNAL IDENTIFIER LEAKAGE",
  "--------------------------------------------------",
  paste0(
    "Machine identifier leakage failures: ",
    machine_identifier_fail_n
  ),
  "",
  "ABSTRACT–RESULTS–DISCUSSION / TABLE CONSISTENCY",
  "--------------------------------------------------",
  paste0(
    "Contradiction/unsupported-claim issues: ",
    semantic_contradiction_n
  ),
  paste0(
    "Potential ambiguities: ",
    semantic_potential_n
  ),
  paste0(
    "Semantic PASS: ",
    semantic_pass
  ),
  "",
  "WORKFLOW READINESS",
  "--------------------------------------------------",
  paste0(
    "R/38 status: ",
    r38_status
  ),
  paste0(
    "R/39 status: ",
    r39_status
  ),
  paste0(
    "Final evidence ready: ",
    final_evidence_ready
  ),
  "",
  "FINAL STATUS",
  "--------------------------------------------------",
  overall_status,
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0(
    "QC summary: ",
    normalize_slash(
      qc_summary_file
    )
  ),
  paste0(
    "QC workbook: ",
    normalize_slash(
      qc_workbook_file
    )
  ),
  paste0(
    "Issues: ",
    normalize_slash(
      issues_file
    )
  ),
  paste0(
    "State: ",
    normalize_slash(
      state_file
    )
  ),
  if (
    identical(
      overall_status,
      "PASS_READY_FOR_SUBMISSION_FORMATTING"
    )
  ) {
    paste0(
      "Submission candidate: ",
      normalize_slash(
        submission_candidate_file
      )
    )
  } else {
    "Submission candidate: NOT CREATED"
  }
)

write_text_file(
  report_lines,
  report_file
)


# ============================================================
# 21. Console summary
# ============================================================

cat(
  "\n============================================================\n",
  "R/40 — Final Manuscript QC\n",
  "============================================================\n\n",
  sep = ""
)

cat(
  "Project: ",
  project_title,
  "\n",
  "Numeric failures: ",
  numeric_fail_n,
  "\n",
  "Citation integrity failures: ",
  citation_fail_n,
  "\n",
  "PubMed reverification failures: ",
  pubmed_fail_n,
  "\n",
  "Artifact coverage failures: ",
  artifact_coverage_fail_n,
  "\n",
  "Machine identifier leakage failures: ",
  machine_identifier_fail_n,
  "\n",
  "Cross-section/table contradictions/unsupported claims: ",
  semantic_contradiction_n,
  "\n",
  "Potential ambiguities: ",
  semantic_potential_n,
  "\n",
  "Final evidence ready: ",
  final_evidence_ready,
  "\n",
  "Overall status: ",
  overall_status,
  "\n",
  "Statistical recalculation: NO\n",
  sep = ""
)

cat(
  "\nQC workbook: ",
  normalize_slash(
    qc_workbook_file
  ),
  "\n",
  "Issues: ",
  normalize_slash(
    issues_file
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
    overall_status,
    "PASS_READY_FOR_SUBMISSION_FORMATTING"
  )
) {
  cat(
    "Submission candidate: ",
    normalize_slash(
      submission_candidate_file
    ),
    "\n\nPASS: Manuscript is ready for submission formatting / DOCX export.\n",
    sep = ""
  )
} else if (
  identical(
    overall_status,
    "PROVISIONAL_PENDING_FINAL_ROB"
  )
) {
  cat(
    "\nPASS WITH GATE: Internal QC passed, but final RoB workflow remains pending.\n"
  )
} else {
  cat(
    "\nCHANGES REQUIRED: Review issues_for_author_review.md, correct the manuscript or upstream artifact, then rerun R/40.\n"
  )
}
