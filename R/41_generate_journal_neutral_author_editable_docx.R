# ============================================================
# Universal SR-MA Agent Toolkit
# R/41_generate_journal_neutral_author_editable_docx.R
#
# VERSION: 1.11.0
#
# PURPOSE
# ------------------------------------------------------------
# Final journal-neutral manuscript export stage.
#
# R/41 runs ONLY after R/40 has returned:
#   PASS_READY_FOR_SUBMISSION_FORMATTING
#
# R/41:
#   1) reads the PASS-only R/40 manuscript_submission_candidate.md;
#   2) converts the manuscript into an author-editable Word document;
#   3) appends manuscript-relevant tables as native editable Word tables;
#   4) appends figure legends and copies canonical figure files into a
#      journal-neutral author-review package;
#   5) optionally embeds raster figures when explicitly enabled;
#   6) creates a complete export manifest/state/report for provenance.
#
# R/41 DOES NOT:
#   - rewrite scientific prose;
#   - recalculate statistics;
#   - add unsupported claims;
#   - select a target journal;
#   - enforce journal-specific reference, title-page, table, or figure rules;
#   - call an LLM or any external API.
#
# JOURNAL-NEUTRAL DESIGN
# ------------------------------------------------------------
# The output is intended for AUTHOR FINAL REVIEW, not direct submission.
# Journal-specific formatting is intentionally outside the universal toolkit.
#
# Default behavior:
#   - use the exact R/40 PASS manuscript text;
#   - preserve numbered references as written;
#   - append editable Word tables;
#   - append figure legends;
#   - append the actual numbered figure images at the end of the DOCX;
#   - copy original figure files separately;
#   - preserve the same first-mention numbering in body, legends, and images.
#
# OPTIONAL CONFIG
# ------------------------------------------------------------
# analysis:
#   manuscript:
#     docx:
#       include_tables: true
#       include_figure_legends: true
#       append_figures_at_end: true
#       repeat_figure_legend_with_image: true
#       copy_artifact_files: true
#       table_font_size: 9
#       figure_dpi: 300
#       figure_max_width_in: 6.2
#       figure_max_height_in: 8.0
#       auto_install_figure_render_dependencies: true
#       figure_render_cran_repo: https://cloud.r-project.org
#
# OUTPUT ROOT
# ------------------------------------------------------------
# results/manuscript/production/author_review/
#   manuscript_author_editable.docx
#   docx_export_manifest.csv
#   docx_export_state.yml
#   docx_export_report.txt
#   README_author_review.txt
#   figure_embedding_audit.csv
#   figure_discovery_audit.csv
#   pdf_renderer_inventory.csv
#   figure_render_dependency_audit.csv
#   _r41_runtime_library/
#   tables/
#   figures/
#     _word_render/
#
# GOVERNANCE
# ------------------------------------------------------------
# - R/27 remains the canonical statistical engine.
# - R/38 remains the manuscript evidence freeze.
# - R/39 remains the drafting stage.
# - R/40 remains the final scientific/numerical/citation QC gate.
# - R/41 is formatting/export only.
# - Scientific rewriting: NO.
# - Statistical recalculation: NO.
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
  "digest",
  "officer",
  "flextable",
  "openxlsx"
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
    paste0(
      "Missing required package(s): ",
      paste(
        missing_packages,
        collapse = ", "
      ),
      ". Install them before running R/41."
    ),
    call. = FALSE
  )
}


# ============================================================
# 1. Generic helpers
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
  sizes <- rep(
    NA_real_,
    length(path)
  )

  if (any(exists)) {
    sizes[exists] <- file.info(
      path[exists]
    )$size
  }

  exists &
    !is.na(sizes) &
    sizes > 0
}

safe_chr <- function(
    x,
    default = ""
) {
  if (
    is.null(x) ||
      length(x) == 0L ||
      all(is.na(x))
  ) {
    return(default)
  }

  out <- trimws(
    as.character(
      x[[1]]
    )
  )

  if (
    is.na(out) ||
      !nzchar(out)
  ) {
    return(default)
  }

  out
}

safe_num <- function(
    x,
    default = NA_real_
) {
  if (
    is.null(x) ||
      length(x) == 0L ||
      all(is.na(x))
  ) {
    return(default)
  }

  out <- suppressWarnings(
    as.numeric(
      x[[1]]
    )
  )

  if (
    length(out) == 0L ||
      is.na(out)
  ) {
    return(default)
  }

  out
}

safe_flag <- function(
    x,
    default = FALSE
) {
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

  value <- tolower(
    trimws(
      as.character(
        x[[1]]
      )
    )
  )

  if (value %in% c(
    "true",
    "yes",
    "y",
    "1",
    "on"
  )) {
    return(TRUE)
  }

  if (value %in% c(
    "false",
    "no",
    "n",
    "0",
    "off"
  )) {
    return(FALSE)
  }

  isTRUE(default)
}

# Synthetic regression tests are development-only. Production R/41 must fail
# only on real project invariants, never because an optional synthetic test
# disagrees with a local R/locale implementation detail.
run_internal_self_tests <- safe_flag(
  Sys.getenv(
    "SRMA_R41_INTERNAL_SELF_TESTS",
    unset = "false"
  ),
  FALSE
)

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

write_text_file <- function(
    text,
    path
) {
  dir.create(
    dirname(path),
    recursive = TRUE,
    showWarnings = FALSE
  )

  writeLines(
    enc2utf8(
      as.character(text)
    ),
    con = path,
    useBytes = TRUE
  )

  invisible(path)
}

first_existing_col <- function(
    data,
    candidates
) {
  hits <- candidates[
    candidates %in% names(data)
  ]

  if (length(hits) == 0L) {
    return(NA_character_)
  }

  hits[[1]]
}

get_nested <- function(
    x,
    path,
    default = NULL
) {
  parts <- strsplit(
    path,
    "\\.",
    perl = TRUE
  )[[1]]

  current <- x

  for (part in parts) {
    if (
      is.null(current) ||
        !is.list(current) ||
        is.null(current[[part]])
    ) {
      return(default)
    }

    current <- current[[part]]
  }

  current
}

first_config_value <- function(
    config,
    paths,
    default = NULL
) {
  for (path in paths) {
    value <- get_nested(
      config,
      path,
      NULL
    )

    if (!is.null(value)) {
      return(value)
    }
  }

  default
}

slugify <- function(x) {
  x <- tolower(
    trimws(
      as.character(x)
    )
  )

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

clean_file_component <- function(x) {
  x <- as.character(x)

  x <- gsub(
    "[<>:\"/\\\\|?*]+",
    "_",
    x,
    perl = TRUE
  )

  x <- gsub(
    "[[:space:]]+",
    "_",
    x,
    perl = TRUE
  )

  x <- gsub(
    "_+",
    "_",
    x
  )

  x <- gsub(
    "^_|_$",
    "",
    x
  )

  if (!nzchar(x)) {
    return("artifact")
  }

  x
}

relative_to_root <- function(
    path,
    root
) {
  p <- normalize_slash(
    normalizePath(
      path,
      winslash = "/",
      mustWork = FALSE
    )
  )

  r <- normalize_slash(
    normalizePath(
      root,
      winslash = "/",
      mustWork = FALSE
    )
  )

  prefix <- paste0(
    r,
    "/"
  )

  if (startsWith(p, prefix)) {
    return(
      substring(
        p,
        nchar(prefix) + 1L
      )
    )
  }

  p
}

resolve_registry_path <- function(
    source_path,
    source_relative_path,
    repo_root
) {
  candidates <- unique(
    c(
      safe_chr(
        source_path,
        ""
      ),
      if (
        nzchar(
          safe_chr(
            source_relative_path,
            ""
          )
        )
      ) {
        file.path(
          repo_root,
          safe_chr(
            source_relative_path,
            ""
          )
        )
      } else {
        ""
      }
    )
  )

  candidates <- candidates[
    nzchar(candidates)
  ]

  if (length(candidates) == 0L) {
    return("")
  }

  existing <- candidates[
    file.exists(
      candidates
    )
  ]

  if (length(existing) == 0L) {
    return(
      candidates[[1]]
    )
  }

  existing[[1]]
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
    "config/project_config.yml was not found. Run R/41 from the project root.",
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

project_id <- safe_chr(
  first_config_value(
    config,
    c(
      "project.id",
      "project_id"
    ),
    basename(
      repo_root
    )
  ),
  basename(
    repo_root
  )
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

docx_cfg <- first_config_value(
  config,
  c(
    "analysis.manuscript.docx",
    "manuscript.docx"
  ),
  list()
)

if (!is.list(docx_cfg)) {
  docx_cfg <- list()
}

include_tables <- safe_flag(
  docx_cfg$include_tables,
  TRUE
)

include_figure_legends <- safe_flag(
  docx_cfg$include_figure_legends,
  TRUE
)

append_figures_at_end <- safe_flag(
  docx_cfg$append_figures_at_end,
  TRUE
)

repeat_figure_legend_with_image <- safe_flag(
  docx_cfg$repeat_figure_legend_with_image,
  TRUE
)

# Backward-compatibility only. R/41 v1.6+ does not place figures beside the
# Figure Legends section. Actual figures are appended in a dedicated Figures
# section at the end of the manuscript.
legacy_embed_figures <- safe_flag(
  docx_cfg$embed_figures,
  FALSE
)

if (legacy_embed_figures) {
  warning(
    paste0(
      "Config key 'embed_figures' is deprecated in R/41 v1.6+. ",
      "Use 'append_figures_at_end'. Figures are appended at the end of the ",
      "manuscript, not embedded beside the Figure Legends section."
    )
  )
}

copy_artifact_files <- safe_flag(
  docx_cfg$copy_artifact_files,
  TRUE
)

figure_dpi <- safe_num(
  docx_cfg$figure_dpi,
  300
)

if (
  is.na(figure_dpi) ||
    figure_dpi < 72 ||
    figure_dpi > 600
) {
  figure_dpi <- 300
}

figure_max_width_in <- safe_num(
  docx_cfg$figure_max_width_in,
  6.2
)

if (
  is.na(figure_max_width_in) ||
    figure_max_width_in < 2 ||
    figure_max_width_in > 7.5
) {
  figure_max_width_in <- 6.2
}

figure_max_height_in <- safe_num(
  docx_cfg$figure_max_height_in,
  8.0
)

if (
  is.na(figure_max_height_in) ||
    figure_max_height_in < 2 ||
    figure_max_height_in > 10
) {
  figure_max_height_in <- 8.0
}

auto_install_figure_render_dependencies <- safe_flag(
  docx_cfg$auto_install_figure_render_dependencies,
  TRUE
)

figure_render_cran_repo <- safe_chr(
  docx_cfg$figure_render_cran_repo,
  "https://cloud.r-project.org"
)

if (!nzchar(figure_render_cran_repo)) {
  figure_render_cran_repo <- "https://cloud.r-project.org"
}

table_font_size <- safe_num(
  docx_cfg$table_font_size,
  9
)

if (
  is.na(table_font_size) ||
    table_font_size < 6 ||
    table_font_size > 14
) {
  table_font_size <- 9
}


# ============================================================
# 3. Input/output paths
# ============================================================

production_root <- file.path(
  repo_root,
  "results",
  "manuscript",
  "production"
)

qc_root <- file.path(
  production_root,
  "qc"
)

draft_root <- file.path(
  production_root,
  "drafting"
)

author_root <- file.path(
  production_root,
  "author_review"
)

tables_root <- file.path(
  author_root,
  "tables"
)

figures_root <- file.path(
  author_root,
  "figures"
)

qc_state_file <- file.path(
  qc_root,
  "manuscript_qc_state.yml"
)

qc_summary_file <- file.path(
  qc_root,
  "qc_summary.csv"
)

submission_candidate_file <- file.path(
  qc_root,
  "manuscript_submission_candidate.md"
)

artifact_registry_file <- file.path(
  production_root,
  "manuscript_artifact_registry.csv"
)

results_draft_file <- file.path(
  draft_root,
  "results_draft.md"
)

docx_file <- file.path(
  author_root,
  "manuscript_author_editable.docx"
)

manifest_file <- file.path(
  author_root,
  "docx_export_manifest.csv"
)

crossref_file <- file.path(
  author_root,
  "table_figure_cross_reference_map.csv"
)

figure_embedding_audit_file <- file.path(
  author_root,
  "figure_embedding_audit.csv"
)

figure_discovery_audit_file <- file.path(
  author_root,
  "figure_discovery_audit.csv"
)

pdf_renderer_inventory_file <- file.path(
  author_root,
  "pdf_renderer_inventory.csv"
)

figure_render_dependency_audit_file <- file.path(
  author_root,
  "figure_render_dependency_audit.csv"
)

figure_runtime_library <- file.path(
  author_root,
  "_r41_runtime_library"
)

figure_render_root <- file.path(
  figures_root,
  "_word_render"
)

state_file <- file.path(
  author_root,
  "docx_export_state.yml"
)

report_file <- file.path(
  author_root,
  "docx_export_report.txt"
)

readme_file <- file.path(
  author_root,
  "README_author_review.txt"
)

dir.create(
  author_root,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_runtime_library,
  recursive = TRUE,
  showWarnings = FALSE
)

# Optional renderer dependencies are isolated here. The canonical project
# dependency lockfile is not modified.
.libPaths(
  unique(
    c(
      normalizePath(
        figure_runtime_library,
        winslash = "/",
        mustWork = FALSE
      ),
      .libPaths()
    )
  )
)

dir.create(
  tables_root,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figures_root,
  recursive = TRUE,
  showWarnings = FALSE
)

if (dir.exists(figure_render_root)) {
  unlink(
    figure_render_root,
    recursive = TRUE,
    force = TRUE
  )
}

dir.create(
  figure_render_root,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 4. Mandatory R/40 PASS gate
# ============================================================

if (!file_nonempty(qc_state_file)) {
  stop(
    paste0(
      "R/40 QC state was not found: ",
      normalize_slash(
        qc_state_file
      ),
      ". Run R/40 first."
    ),
    call. = FALSE
  )
}

qc_state <- yaml::read_yaml(
  qc_state_file
)

qc_status <- safe_chr(
  get_nested(
    qc_state,
    "manuscript_qc.overall_status",
    ""
  ),
  ""
)

submission_created <- safe_flag(
  get_nested(
    qc_state,
    "manuscript_qc.submission_candidate_created",
    FALSE
  ),
  FALSE
)

if (
  !identical(
    qc_status,
    "PASS_READY_FOR_SUBMISSION_FORMATTING"
  ) ||
    !submission_created
) {
  stop(
    paste0(
      "R/41 requires R/40 status PASS_READY_FOR_SUBMISSION_FORMATTING. ",
      "Current R/40 status: ",
      ifelse(
        nzchar(qc_status),
        qc_status,
        "[UNKNOWN]"
      ),
      "."
    ),
    call. = FALSE
  )
}

if (!file_nonempty(submission_candidate_file)) {
  stop(
    paste0(
      "R/40 PASS submission candidate was not found or is empty: ",
      normalize_slash(
        submission_candidate_file
      )
    ),
    call. = FALSE
  )
}

submission_text <- read_text_file(
  submission_candidate_file
)

if (
  grepl(
    "<!--\\s*ARTIFACT:",
    submission_text,
    ignore.case = TRUE,
    perl = TRUE
  )
) {
  stop(
    paste0(
      "Internal artifact markers remain in the R/40 submission candidate. ",
      "R/41 will not export a manuscript containing internal coverage tokens."
    ),
    call. = FALSE
  )
}


# ============================================================
# 5. Journal-neutral Markdown parsing
# ============================================================

strip_inline_markdown <- function(x) {
  x <- as.character(x)

  # Preserve both visible label and URL for Markdown links.
  x <- gsub(
    "\\[([^]]+)\\]\\(([^)]+)\\)",
    "\\1 (\\2)",
    x,
    perl = TRUE
  )

  # Preserve visible text while removing common emphasis/code delimiters.
  x <- gsub(
    "\\*\\*([^*]+)\\*\\*",
    "\\1",
    x,
    perl = TRUE
  )

  x <- gsub(
    "__([^_]+)__",
    "\\1",
    x,
    perl = TRUE
  )

  x <- gsub(
    "(^|[^*])\\*([^*]+)\\*",
    "\\1\\2",
    x,
    perl = TRUE
  )

  x <- gsub(
    "(^|[^_])_([^_]+)_",
    "\\1\\2",
    x,
    perl = TRUE
  )

  x <- gsub(
    "`([^`]+)`",
    "\\1",
    x,
    perl = TRUE
  )

  x <- gsub(
    "<br\\s*/?>",
    " ",
    x,
    ignore.case = TRUE,
    perl = TRUE
  )

  trimws(
    x
  )
}

parse_markdown_blocks <- function(text) {
  text <- as.character(text)

  # Restore literal escaped line breaks that can otherwise appear as visible
  # "nn###" fragments in the author-editable Word manuscript.
  text <- gsub(
    "\\r\\n",
    "\n",
    text,
    fixed = TRUE
  )

  text <- gsub(
    "\\n",
    "\n",
    text,
    fixed = TRUE
  )

  text <- gsub(
    "\r\n?",
    "\n",
    text,
    perl = TRUE
  )

  lines <- strsplit(
    text,
    "\n",
    fixed = TRUE
  )[[1]]

  blocks <- list()
  paragraph_buffer <- character(0)

  flush_paragraph <- function() {
    if (length(paragraph_buffer) == 0L) {
      return(NULL)
    }

    value <- trimws(
      paste(
        paragraph_buffer,
        collapse = " "
      )
    )

    if (!nzchar(value)) {
      return(NULL)
    }

    list(
      type = "paragraph",
      level = NA_integer_,
      text = strip_inline_markdown(
        value
      )
    )
  }

  append_block <- function(block) {
    if (!is.null(block)) {
      blocks[[
        length(blocks) + 1L
      ]] <<- block
    }
  }

  for (line in lines) {
    raw <- line
    trimmed <- trimws(raw)

    heading_match <- stringr::str_match(
      trimmed,
      "^(#{1,6})\\s+(.+)$"
    )

    is_heading <- (
      !is.null(heading_match) &&
        nrow(heading_match) > 0L &&
        !is.na(
          heading_match[1, 1]
        )
    )

    is_bullet <- grepl(
      "^[-*+]\\s+",
      trimmed,
      perl = TRUE
    )

    is_numbered <- grepl(
      "^[0-9]+[.)]\\s+",
      trimmed,
      perl = TRUE
    )

    is_quote <- grepl(
      "^>\\s*",
      trimmed,
      perl = TRUE
    )

    is_rule <- grepl(
      "^([-*_])(?:\\s*\\1){2,}$",
      trimmed,
      perl = TRUE
    )

    is_blank <- !nzchar(trimmed)

    if (
      is_blank ||
        is_heading ||
        is_bullet ||
        is_numbered ||
        is_quote ||
        is_rule
    ) {
      append_block(
        flush_paragraph()
      )

      paragraph_buffer <- character(0)
    }

    if (is_blank) {
      next
    }

    if (is_rule) {
      next
    }

    if (is_heading) {
      append_block(
        list(
          type = "heading",
          level = nchar(
            heading_match[1, 2]
          ),
          text = strip_inline_markdown(
            sub(
              "^[0-9]+(?:\\.[0-9]+)*\\.?\\s+",
              "",
              heading_match[1, 3],
              perl = TRUE
            )
          )
        )
      )
      next
    }

    if (is_bullet) {
      append_block(
        list(
          type = "bullet",
          level = NA_integer_,
          text = strip_inline_markdown(
            sub(
              "^[-*+]\\s+",
              "",
              trimmed,
              perl = TRUE
            )
          )
        )
      )
      next
    }

    if (is_numbered) {
      append_block(
        list(
          type = "numbered",
          level = NA_integer_,
          text = strip_inline_markdown(
            trimmed
          )
        )
      )
      next
    }

    if (is_quote) {
      append_block(
        list(
          type = "quote",
          level = NA_integer_,
          text = strip_inline_markdown(
            sub(
              "^>\\s*",
              "",
              trimmed,
              perl = TRUE
            )
          )
        )
      )
      next
    }

    paragraph_buffer <- c(
      paragraph_buffer,
      trimmed
    )
  }

  append_block(
    flush_paragraph()
  )

  blocks
}

collapse_consecutive_duplicate_headings <- function(blocks) {
  if (length(blocks) <= 1L) {
    return(blocks)
  }

  normalize_heading <- function(x) {
    x <- tolower(
      trimws(
        as.character(x)
      )
    )

    gsub(
      "[^a-z0-9]+",
      "",
      x,
      perl = TRUE
    )
  }

  out <- list()

  for (block in blocks) {
    if (
      identical(block$type, "heading") &&
        length(out) > 0L &&
        identical(
          out[[length(out)]]$type,
          "heading"
        ) &&
        identical(
          normalize_heading(
            out[[length(out)]]$text
          ),
          normalize_heading(
            block$text
          )
        )
    ) {
      prior_level <- out[[length(out)]]$level
      current_level <- block$level

      if (
        !is.na(current_level) &&
          (
            is.na(prior_level) ||
              current_level < prior_level
          )
      ) {
        out[[length(out)]] <- block
      }

      next
    }

    out[[length(out) + 1L]] <- block
  }

  out
}

markdown_blocks <- collapse_consecutive_duplicate_headings(
  parse_markdown_blocks(
    submission_text
  )
)

if (length(markdown_blocks) == 0L) {
  stop(
    "R/40 submission candidate could not be parsed into manuscript blocks.",
    call. = FALSE
  )
}

heading_text <- vapply(
  markdown_blocks,
  function(block) {
    if (identical(block$type, "heading")) {
      return(
        tolower(
          trimws(
            block$text
          )
        )
      )
    }

    ""
  },
  character(1)
)

if (!any(heading_text == "abstract")) {
  warning(
    "No explicit Abstract heading was found in the PASS manuscript. R/41 will preserve the source content without inventing one."
  )
}

if (!any(heading_text == "references")) {
  warning(
    "No explicit References heading was found in the PASS manuscript. R/41 will preserve the source content without inventing one."
  )
}


# ============================================================
# 6. Artifact registry and semantic order
# ============================================================

artifact_registry <- if (
  file_nonempty(
    artifact_registry_file
  )
) {
  readr::read_csv(
    artifact_registry_file,
    show_col_types = FALSE,
    progress = FALSE
  )
} else {
  tibble::tibble()
}

required_artifact_cols <- c(
  "artifact_id",
  "artifact_type",
  "title",
  "manuscript_role",
  "manuscript_priority",
  "coverage_required",
  "coverage_token",
  "analysis_group",
  "file_extension",
  "source_path",
  "source_relative_path",
  "source_status"
)

if (nrow(artifact_registry) > 0L) {
  missing_cols <- setdiff(
    required_artifact_cols,
    names(
      artifact_registry
    )
  )

  if (length(missing_cols) > 0L) {
    stop(
      paste0(
        "manuscript_artifact_registry.csv is missing required column(s): ",
        paste(
          missing_cols,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }

  artifact_registry <- artifact_registry |>
    dplyr::mutate(
      coverage_required = vapply(
        coverage_required,
        safe_flag,
        logical(1),
        default = FALSE
      ),
      resolved_source_path = mapply(
        resolve_registry_path,
        source_path,
        source_relative_path,
        MoreArgs = list(
          repo_root = repo_root
        ),
        USE.NAMES = FALSE
      ),
      source_available_now = vapply(
        resolved_source_path,
        file_nonempty,
        logical(1)
      )
    )
}

results_draft_text <- read_text_file(
  results_draft_file
)

artifact_marker_position <- function(
    token,
    text
) {
  if (
    !nzchar(
      safe_chr(
        token,
        ""
      )
    ) ||
      !nzchar(text)
  ) {
    return(
      Inf
    )
  }

  pos <- regexpr(
    token,
    text,
    fixed = TRUE
  )[[1]]

  if (is.na(pos) || pos < 0L) {
    return(Inf)
  }

  as.numeric(pos)
}

if (nrow(artifact_registry) > 0L) {
  artifact_registry <- artifact_registry |>
    dplyr::mutate(
      narrative_order = vapply(
        coverage_token,
        artifact_marker_position,
        numeric(1),
        text = results_draft_text
      ),
      priority_rank = dplyr::case_when(
        manuscript_priority == "primary" ~ 1L,
        manuscript_priority == "supporting" ~ 2L,
        TRUE ~ 3L
      ),
      role_rank = dplyr::case_when(
        manuscript_role == "study_selection" ~ 1L,
        manuscript_role == "study_characteristics" ~ 2L,
        manuscript_role == "quantitative_result" ~ 3L,
        manuscript_role == "diagnostic_result" ~ 4L,
        manuscript_role == "risk_of_bias" ~ 5L,
        TRUE ~ 6L
      )
    ) |>
    dplyr::arrange(
      narrative_order,
      priority_rank,
      role_rank,
      title,
      artifact_id
    )
}

# ------------------------------------------------------------
# Publication artifact discovery contract
# ------------------------------------------------------------
# Tables continue to use the R/38 semantic coverage requirement.
#
# Figures are different: the manuscript artifact registry can contain several
# renderings of the same semantic figure (for example PDF + PNG). R/38 may
# require only one representative for evidence coverage, while the directly
# embeddable PNG/JPEG row can legitimately have coverage_required = FALSE.
# Therefore R/41 must preserve all usable figure renderings until AFTER
# semantic deduplication and rendering preference are applied.

if (nrow(artifact_registry) > 0L) {
  artifact_registry <- artifact_registry |>
    dplyr::mutate(
      normalized_artifact_type = tolower(
        trimws(
          as.character(
            artifact_type
          )
        )
      ),
      normalized_source_status = toupper(
        trimws(
          as.character(
            source_status
          )
        )
      ),
      marker_present_in_results_draft = is.finite(
        narrative_order
      ),
      figure_registry_row = normalized_artifact_type == "figure",
      table_registry_row = normalized_artifact_type == "table",
      usable_source = (
        normalized_source_status == "AVAILABLE" &
          source_available_now
      ),
      selected_table_candidate = (
        table_registry_row &
          usable_source &
          coverage_required
      ),
      selected_figure_candidate = (
        figure_registry_row &
          usable_source
      ),
      selected_publication_candidate = (
        selected_table_candidate |
          selected_figure_candidate
      )
    )
}

figure_discovery_audit <- if (
  nrow(artifact_registry) > 0L
) {
  artifact_registry |>
    dplyr::filter(
      figure_registry_row
    ) |>
    dplyr::transmute(
      artifact_id = artifact_id,
      title = title,
      manuscript_role = manuscript_role,
      manuscript_priority = manuscript_priority,
      coverage_required = coverage_required,
      marker_present_in_results_draft = marker_present_in_results_draft,
      analysis_group = analysis_group,
      coverage_unit_id = if (
        "coverage_unit_id" %in% names(
          artifact_registry
        )
      ) {
        coverage_unit_id
      } else {
        ""
      },
      file_extension = file_extension,
      source_path = source_path,
      source_relative_path = source_relative_path,
      resolved_source_path = resolved_source_path,
      source_status = source_status,
      source_available_now = source_available_now,
      selected_figure_candidate = selected_figure_candidate,
      discovery_reason = dplyr::case_when(
        selected_figure_candidate &
          coverage_required ~
          "AVAILABLE_FIGURE_COVERAGE_REQUIRED",
        selected_figure_candidate &
          marker_present_in_results_draft ~
          "AVAILABLE_FIGURE_RESULTS_MARKER",
        selected_figure_candidate ~
          "AVAILABLE_FIGURE_MANUSCRIPT_REGISTRY",
        !source_available_now ~
          "FIGURE_SOURCE_MISSING_OR_EMPTY",
        normalized_source_status != "AVAILABLE" ~
          paste0(
            "SOURCE_STATUS_",
            normalized_source_status
          ),
        TRUE ~
          "NOT_SELECTED"
      )
    )
} else {
  tibble::tibble(
    artifact_id = character(0),
    title = character(0),
    manuscript_role = character(0),
    manuscript_priority = character(0),
    coverage_required = logical(0),
    marker_present_in_results_draft = logical(0),
    analysis_group = character(0),
    coverage_unit_id = character(0),
    file_extension = character(0),
    source_path = character(0),
    source_relative_path = character(0),
    resolved_source_path = character(0),
    source_status = character(0),
    source_available_now = logical(0),
    selected_figure_candidate = logical(0),
    discovery_reason = character(0)
  )
}

readr::write_csv(
  figure_discovery_audit,
  figure_discovery_audit_file,
  na = ""
)

available_registry_figure_count <- if (
  nrow(figure_discovery_audit) > 0L
) {
  sum(
    figure_discovery_audit$selected_figure_candidate,
    na.rm = TRUE
  )
} else {
  0L
}

publication_artifacts <- if (
  nrow(artifact_registry) > 0L
) {
  artifact_registry |>
    dplyr::filter(
      selected_publication_candidate
    )
} else {
  tibble::tibble()
}


# ============================================================
# 6.1 Synchronized Table/Figure numbering + body cross-references
# ============================================================

strip_existing_display_number <- function(
    title,
    artifact_type
) {
  title <- safe_chr(
    title,
    ""
  )

  if (!nzchar(title)) {
    return("")
  }

  prefix <- if (identical(artifact_type, "table")) {
    "^\\s*Table\\s+[0-9]+(?:\\.[0-9]+)?\\s*[-–—:.]?\\s*"
  } else {
    "^\\s*Figure\\s+[0-9]+(?:\\.[0-9]+)?\\s*[-–—:.]?\\s*"
  }

  trimws(
    sub(
      prefix,
      "",
      title,
      ignore.case = TRUE,
      perl = TRUE
    )
  )
}

normalize_crossref_text <- function(x) {
  x <- strip_inline_markdown(
    safe_chr(
      x,
      ""
    )
  )

  x <- tolower(x)

  x <- gsub(
    "[^a-z0-9]+",
    " ",
    x,
    perl = TRUE
  )

  trimws(
    gsub(
      "[[:space:]]+",
      " ",
      x,
      perl = TRUE
    )
  )
}

crossref_terms <- function(x) {
  x <- normalize_crossref_text(
    x
  )

  terms <- unlist(
    strsplit(
      x,
      "[[:space:]]+",
      perl = TRUE
    )
  )

  terms <- terms[
    nchar(terms) >= 4L
  ]

  stop_terms <- c(
    "table",
    "figure",
    "figures",
    "results",
    "result",
    "analysis",
    "analyses",
    "primary",
    "final",
    "summary",
    "publication"
  )

  unique(
    setdiff(
      terms,
      stop_terms
    )
  )
}

infer_artifact_role <- function(
    manuscript_role,
    title,
    source_relative_path = ""
) {
  role <- tolower(
    safe_chr(
      manuscript_role,
      ""
    )
  )

  if (
    role %in% c(
      "study_selection",
      "study_characteristics",
      "risk_of_bias",
      "quantitative_result",
      "diagnostic_result"
    )
  ) {
    return(role)
  }

  probe <- normalize_crossref_text(
    paste(
      safe_chr(title, ""),
      safe_chr(source_relative_path, "")
    )
  )

  if (
    grepl(
      "\\b(prisma|study selection|selection flow|flow diagram)\\b",
      probe,
      perl = TRUE
    )
  ) {
    return("study_selection")
  }

  if (
    grepl(
      "\\b(risk of bias|robins|rob 2|rob2|quality assessment)\\b",
      probe,
      perl = TRUE
    )
  ) {
    return("risk_of_bias")
  }

  if (
    grepl(
      "\\b(funnel|egger|leave one out|sensitivity|meta regression|diagnostic|baujat|influence)\\b",
      probe,
      perl = TRUE
    )
  ) {
    return("diagnostic_result")
  }

  if (
    grepl(
      "\\b(forest|pooled|meta analysis|quantitative synthesis|effect estimate)\\b",
      probe,
      perl = TRUE
    )
  ) {
    return("quantitative_result")
  }

  if (
    grepl(
      "\\b(characteristics|baseline|participant|patient|intervention|exposure|comparator|study design)\\b",
      probe,
      perl = TRUE
    )
  ) {
    return("study_characteristics")
  }

  role
}

# ------------------------------------------------------------
# 6.1.1 Semantic artifact deduplication
# ------------------------------------------------------------

if (nrow(publication_artifacts) > 0L) {
  publication_artifacts <- publication_artifacts |>
    dplyr::mutate(
      inferred_role = mapply(
        infer_artifact_role,
        manuscript_role,
        title,
        source_relative_path,
        USE.NAMES = FALSE
      ),
      resolved_analysis_group = vapply(
        analysis_group,
        safe_chr,
        character(1),
        default = ""
      )
    )

  quantitative_groups <- unique(
    publication_artifacts$resolved_analysis_group[
      publication_artifacts$artifact_type == "figure" &
        publication_artifacts$inferred_role == "quantitative_result" &
        nzchar(
          publication_artifacts$resolved_analysis_group
        )
    ]
  )

  if (length(quantitative_groups) == 1L) {
    fill_idx <- which(
      publication_artifacts$artifact_type == "figure" &
        publication_artifacts$inferred_role == "quantitative_result" &
        !nzchar(
          publication_artifacts$resolved_analysis_group
        )
    )

    if (length(fill_idx) > 0L) {
      publication_artifacts$resolved_analysis_group[
        fill_idx
      ] <- quantitative_groups[[1]]
    }
  }
}

artifact_semantic_key <- function(
    artifact_type,
    inferred_role,
    resolved_analysis_group,
    artifact_id,
    coverage_unit_id = ""
) {
  type <- safe_chr(
    artifact_type,
    ""
  )

  role <- safe_chr(
    inferred_role,
    ""
  )

  group <- safe_chr(
    resolved_analysis_group,
    ""
  )

  coverage_unit_id <- safe_chr(
    coverage_unit_id,
    ""
  )

  if (
    identical(type, "figure") &&
      identical(role, "study_selection")
  ) {
    return(
      "figure::study_selection"
    )
  }

  if (
    identical(type, "figure") &&
      identical(role, "quantitative_result") &&
      nzchar(group)
  ) {
    return(
      paste(
        "figure",
        role,
        group,
        sep = "::"
      )
    )
  }

  if (
    identical(type, "table") &&
      nzchar(coverage_unit_id)
  ) {
    return(
      paste0(
        "table::",
        coverage_unit_id
      )
    )
  }

  if (nzchar(coverage_unit_id)) {
    return(
      paste(
        type,
        coverage_unit_id,
        sep = "::"
      )
    )
  }

  paste(
    type,
    safe_chr(
      artifact_id,
      ""
    ),
    sep = "::"
  )
}

if (nrow(publication_artifacts) > 0L) {
  coverage_unit_values <- if (
    "coverage_unit_id" %in% names(
      publication_artifacts
    )
  ) {
    publication_artifacts$coverage_unit_id
  } else {
    rep(
      "",
      nrow(
        publication_artifacts
      )
    )
  }

  publication_artifacts <- publication_artifacts |>
    dplyr::mutate(
      semantic_key = mapply(
        artifact_semantic_key,
        artifact_type,
        inferred_role,
        resolved_analysis_group,
        artifact_id,
        coverage_unit_values,
        USE.NAMES = FALSE
      ),
      normalized_file_extension = tolower(
        gsub(
          "^\\.",
          "",
          as.character(
            file_extension
          )
        )
      ),
      word_render_rank = dplyr::case_when(
        artifact_type != "figure" ~ 1L,
        normalized_file_extension %in% c(
          "png",
          "jpg",
          "jpeg"
        ) ~ 1L,
        normalized_file_extension == "pdf" ~ 2L,
        normalized_file_extension == "svg" ~ 3L,
        normalized_file_extension %in% c(
          "tif",
          "tiff",
          "bmp",
          "gif",
          "webp"
        ) ~ 4L,
        TRUE ~ 5L
      )
    )

  # Keep the full rendering inventory BEFORE semantic deduplication.
  # This allows a PDF representative to borrow an equivalent raster rendering
  # from the same semantic figure family without changing scientific content.
  figure_rendering_inventory <- publication_artifacts |>
    dplyr::filter(
      artifact_type == "figure",
      source_available_now
    ) |>
    dplyr::arrange(
      semantic_key,
      word_render_rank,
      narrative_order,
      priority_rank,
      role_rank,
      title,
      artifact_id
    )

  publication_artifacts <- publication_artifacts |>
    dplyr::group_by(
      semantic_key
    ) |>
    dplyr::arrange(
      word_render_rank,
      narrative_order,
      priority_rank,
      role_rank,
      title,
      artifact_id,
      .by_group = TRUE
    ) |>
    dplyr::slice_head(
      n = 1L
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(
      narrative_order,
      priority_rank,
      role_rank,
      title,
      artifact_id
    )
} else {
  figure_rendering_inventory <- tibble::tibble()
}

# ------------------------------------------------------------
# 6.1.2 Artifact -> final manuscript body mapping
# Numbering MUST NOT be assigned before this step.
# ------------------------------------------------------------

artifact_anchor_text <- function(
    coverage_token,
    results_text
) {
  token <- safe_chr(
    coverage_token,
    ""
  )

  if (
    !nzchar(token) ||
      !nzchar(results_text)
  ) {
    return("")
  }

  marker_position <- regexpr(
    token,
    results_text,
    fixed = TRUE
  )[[1]]

  if (
    is.na(marker_position) ||
      marker_position < 0L
  ) {
    return("")
  }

  marker_end <- marker_position +
    nchar(token) -
    1L

  following_text <- substring(
    results_text,
    marker_end + 1L
  )

  next_marker <- regexpr(
    "<!--\\s*ARTIFACT:",
    following_text,
    perl = TRUE
  )[[1]]

  if (
    !is.na(next_marker) &&
      next_marker > 0L
  ) {
    following_text <- substring(
      following_text,
      1L,
      next_marker - 1L
    )
  }

  following_blocks <- collapse_consecutive_duplicate_headings(
    parse_markdown_blocks(
      following_text
    )
  )

  candidate_blocks <- Filter(
    function(block) {
      block$type %in% c(
        "paragraph",
        "numbered",
        "bullet",
        "quote"
      ) &&
        nzchar(
          safe_chr(
            block$text,
            ""
          )
        )
    },
    following_blocks
  )

  if (length(candidate_blocks) == 0L) {
    return("")
  }

  safe_chr(
    candidate_blocks[[1]]$text,
    ""
  )
}

find_best_manuscript_block <- function(
    anchor_text,
    blocks
) {
  anchor_norm <- normalize_crossref_text(
    anchor_text
  )

  if (nchar(anchor_norm) < 20L) {
    return(NA_integer_)
  }

  candidate_idx <- which(
    vapply(
      blocks,
      function(block) {
        block$type %in% c(
          "paragraph",
          "numbered",
          "bullet",
          "quote"
        )
      },
      logical(1)
    )
  )

  if (length(candidate_idx) == 0L) {
    return(NA_integer_)
  }

  candidate_norm <- vapply(
    blocks[
      candidate_idx
    ],
    function(block) {
      normalize_crossref_text(
        block$text
      )
    },
    character(1)
  )

  probe_length <- min(
    140L,
    nchar(anchor_norm)
  )

  probe <- substr(
    anchor_norm,
    1L,
    probe_length
  )

  exact_prefix <- which(
    startsWith(
      candidate_norm,
      probe
    )
  )

  if (length(exact_prefix) == 1L) {
    return(
      candidate_idx[
        exact_prefix[[1]]
      ]
    )
  }

  distances <- vapply(
    candidate_norm,
    function(value) {
      value_probe <- substr(
        value,
        1L,
        probe_length
      )

      as.numeric(
        utils::adist(
          probe,
          value_probe,
          partial = FALSE,
          ignore.case = FALSE
        )
      )
    },
    numeric(1)
  )

  best <- which.min(
    distances
  )

  if (length(best) == 0L) {
    return(NA_integer_)
  }

  max_distance <- max(
    8L,
    floor(
      probe_length * 0.20
    )
  )

  if (distances[[best]] > max_distance) {
    return(NA_integer_)
  }

  candidate_idx[[best]]
}

results_block_bounds <- function(blocks) {
  heading_idx <- which(
    vapply(
      blocks,
      function(block) {
        identical(
          block$type,
          "heading"
        )
      },
      logical(1)
    )
  )

  if (length(heading_idx) == 0L) {
    return(
      c(
        1L,
        length(blocks)
      )
    )
  }

  heading_norm <- vapply(
    blocks[
      heading_idx
    ],
    function(block) {
      normalize_crossref_text(
        block$text
      )
    },
    character(1)
  )

  results_hit <- heading_idx[
    which(
      heading_norm == "results" |
        grepl(
          "^results\\b",
          heading_norm,
          perl = TRUE
        )
    )
  ]

  if (length(results_hit) == 0L) {
    return(
      c(
        1L,
        length(blocks)
      )
    )
  }

  start <- results_hit[[1]]
  following_heading_idx <- heading_idx[
    heading_idx > start
  ]

  end <- length(blocks)

  if (length(following_heading_idx) > 0L) {
    for (idx in following_heading_idx) {
      heading_text <- normalize_crossref_text(
        blocks[[idx]]$text
      )

      if (
        heading_text %in% c(
          "discussion",
          "conclusion",
          "conclusions",
          "references",
          "tables",
          "figure legends"
        )
      ) {
        end <- idx - 1L
        break
      }
    }
  }

  c(
    start,
    end
  )
}

section_content_blocks <- function(
    blocks,
    heading_index,
    region_end
) {
  if (
    is.na(heading_index) ||
      heading_index < 1L
  ) {
    return(integer(0))
  }

  heading_level <- blocks[[heading_index]]$level

  if (is.na(heading_level)) {
    heading_level <- 6L
  }

  out <- integer(0)

  if (heading_index >= region_end) {
    return(out)
  }

  for (idx in seq.int(
    heading_index + 1L,
    min(
      region_end,
      length(blocks)
    )
  )) {
    block <- blocks[[idx]]

    if (identical(block$type, "heading")) {
      level <- block$level

      if (
        !is.na(level) &&
          level <= heading_level
      ) {
        break
      }

      next
    }

    if (
      block$type %in% c(
        "paragraph",
        "numbered",
        "bullet",
        "quote"
      ) &&
        nzchar(
          safe_chr(
            block$text,
            ""
          )
        )
    ) {
      out <- c(
        out,
        idx
      )
    }
  }

  out
}

role_heading_patterns <- function(role) {
  switch(
    role,
    study_selection = c(
      "study selection",
      "selection",
      "prisma"
    ),
    study_characteristics = c(
      "study characteristics",
      "characteristics",
      "baseline characteristics"
    ),
    risk_of_bias = c(
      "risk of bias",
      "quality assessment"
    ),
    quantitative_result = c(
      "quantitative synthesis",
      "meta analysis",
      "pooled",
      "outcome"
    ),
    diagnostic_result = c(
      "statistical diagnostics",
      "diagnostic",
      "sensitivity",
      "publication bias"
    ),
    character(0)
  )
}


role_prose_terms <- function(role) {
  switch(
    role,
    study_selection = c(
      "record",
      "screen",
      "exclude",
      "retrieval",
      "eligibility",
      "included",
      "prisma"
    ),
    study_characteristics = c(
      "characteristic",
      "cohort",
      "trial",
      "sample",
      "participant",
      "patient",
      "country",
      "centre",
      "center",
      "follow",
      "intervention",
      "exposure",
      "comparator",
      "device",
      "baseline"
    ),
    risk_of_bias = c(
      "risk",
      "bias",
      "domain",
      "judgement",
      "judgment",
      "robins",
      "moderate",
      "serious",
      "critical",
      "concern"
    ),
    quantitative_result = c(
      "pooled",
      "meta",
      "ratio",
      "effect",
      "confidence",
      "heterogeneity",
      "tau",
      "forest",
      "random",
      "fixed"
    ),
    diagnostic_result = c(
      "diagnostic",
      "sensitivity",
      "leave",
      "funnel",
      "egger",
      "regression",
      "moderator",
      "influence",
      "heterogeneity",
      "descriptive",
      "estimable"
    ),
    character(0)
  )
}

heading_context_for_block <- function(
    blocks,
    block_index,
    region_start = 1L
) {
  if (
    is.na(block_index) ||
      block_index < 1L ||
      block_index > length(blocks)
  ) {
    return("")
  }

  heading_candidates <- which(
    vapply(
      blocks,
      function(block) {
        identical(
          block$type,
          "heading"
        )
      },
      logical(1)
    )
  )

  heading_candidates <- heading_candidates[
    heading_candidates >= region_start &
      heading_candidates < block_index
  ]

  if (length(heading_candidates) == 0L) {
    return("")
  }

  safe_chr(
    blocks[[
      tail(
        heading_candidates,
        1L
      )
    ]]$text,
    ""
  )
}

score_results_prose_block <- function(
    artifact_row,
    blocks,
    block_index,
    region_start
) {
  paragraph_text <- safe_chr(
    blocks[[block_index]]$text,
    ""
  )

  paragraph_terms <- crossref_terms(
    paragraph_text
  )

  heading_text <- heading_context_for_block(
    blocks,
    block_index,
    region_start
  )

  heading_terms <- crossref_terms(
    heading_text
  )

  role <- safe_chr(
    artifact_row$inferred_role[[1]],
    ""
  )

  title_terms <- crossref_terms(
    artifact_row$title[[1]]
  )

  group_terms <- crossref_terms(
    artifact_row$resolved_analysis_group[[1]]
  )

  role_terms <- role_prose_terms(
    role
  )

  # Role vocabulary is a generic scientific anchor. Title/group overlap then
  # discriminates among several artifacts within the same broad Results area.
  role_overlap <- length(
    intersect(
      role_terms,
      paragraph_terms
    )
  )

  title_overlap <- length(
    intersect(
      title_terms,
      paragraph_terms
    )
  )

  group_overlap <- length(
    intersect(
      group_terms,
      paragraph_terms
    )
  )

  heading_role_overlap <- length(
    intersect(
      role_terms,
      heading_terms
    )
  )

  heading_title_overlap <- length(
    intersect(
      title_terms,
      heading_terms
    )
  )

  score <- (
    role_overlap * 4L +
      title_overlap * 5L +
      group_overlap * 6L +
      heading_role_overlap * 3L +
      heading_title_overlap * 3L
  )

  # Small generic bonuses for strong role-specific phrases. These do not use
  # any disease, intervention, device, author, or project-specific wording.
  normalized_paragraph <- normalize_crossref_text(
    paragraph_text
  )

  if (
    identical(role, "study_selection") &&
      grepl(
        "\\b(records?|screen|eligibility|retriev|included|excluded)\\b",
        normalized_paragraph,
        perl = TRUE
      )
  ) {
    score <- score + 5L
  }

  if (
    identical(role, "risk_of_bias") &&
      grepl(
        "\\b(risk of bias|domains?|judg|robins|low risk|moderate|serious|critical)\\b",
        normalized_paragraph,
        perl = TRUE
      )
  ) {
    score <- score + 5L
  }

  if (
    identical(role, "quantitative_result") &&
      grepl(
        "\\b(pooled|confidence interval|heterogeneity|risk ratio|odds ratio|hazard ratio|meta analysis)\\b",
        normalized_paragraph,
        perl = TRUE
      )
  ) {
    score <- score + 5L
  }

  if (
    identical(role, "diagnostic_result") &&
      grepl(
        "\\b(leave one out|funnel|egger|meta regression|sensitivity|diagnostic|moderator)\\b",
        normalized_paragraph,
        perl = TRUE
      )
  ) {
    score <- score + 5L
  }

  if (
    identical(role, "study_characteristics") &&
      grepl(
        "\\b(characteristics|cohort|trial|sample|participants?|patients?|intervention|exposure|comparator|device|follow up)\\b",
        normalized_paragraph,
        perl = TRUE
      )
  ) {
    score <- score + 4L
  }

  as.integer(score)
}

semantic_target_block <- function(
    artifact_row,
    blocks
) {
  bounds <- results_block_bounds(
    blocks
  )

  region_start <- bounds[[1]]
  region_end <- bounds[[2]]

  candidate_blocks <- which(
    vapply(
      blocks,
      function(block) {
        block$type %in% c(
          "paragraph",
          "numbered",
          "bullet",
          "quote"
        ) &&
          nzchar(
            safe_chr(
              block$text,
              ""
            )
          )
      },
      logical(1)
    )
  )

  candidate_blocks <- candidate_blocks[
    candidate_blocks >= region_start &
      candidate_blocks <= region_end
  ]

  if (length(candidate_blocks) == 0L) {
    return(NA_integer_)
  }

  prose_scores <- vapply(
    candidate_blocks,
    function(idx) {
      score_results_prose_block(
        artifact_row,
        blocks,
        idx,
        region_start
      )
    },
    integer(1)
  )

  best_prose_pos <- which.max(
    prose_scores
  )

  if (
    length(best_prose_pos) > 0L &&
      prose_scores[[best_prose_pos]] > 0L
  ) {
    return(
      candidate_blocks[
        best_prose_pos[[1]]
      ]
    )
  }

  # Fallback: if prose vocabulary is sparse, use the closest semantically
  # matching Results subsection heading and then its first substantive block.
  heading_idx <- which(
    vapply(
      blocks,
      function(block) {
        identical(
          block$type,
          "heading"
        )
      },
      logical(1)
    )
  )

  heading_idx <- heading_idx[
    heading_idx >= region_start &
      heading_idx <= region_end
  ]

  if (length(heading_idx) == 0L) {
    return(NA_integer_)
  }

  role <- safe_chr(
    artifact_row$inferred_role[[1]],
    ""
  )

  title_terms <- crossref_terms(
    artifact_row$title[[1]]
  )

  group_terms <- crossref_terms(
    artifact_row$resolved_analysis_group[[1]]
  )

  patterns <- role_heading_patterns(
    role
  )

  heading_scores <- vapply(
    heading_idx,
    function(idx) {
      h_text <- normalize_crossref_text(
        blocks[[idx]]$text
      )

      h_terms <- crossref_terms(
        h_text
      )

      score <- (
        2L *
          length(
            intersect(
              title_terms,
              h_terms
            )
          ) +
          3L *
            length(
              intersect(
                group_terms,
                h_terms
              )
            )
      )

      if (length(patterns) > 0L) {
        score <- score +
          5L *
            sum(
              vapply(
                patterns,
                function(pattern) {
                  grepl(
                    paste0(
                      "\\b",
                      gsub(
                        " ",
                        "\\\\s+",
                        normalize_crossref_text(
                          pattern
                        ),
                        fixed = TRUE
                      ),
                      "\\b"
                    ),
                    h_text,
                    perl = TRUE
                  )
                },
                logical(1)
              )
            )
      }

      as.integer(score)
    },
    integer(1)
  )

  best_heading_pos <- which.max(
    heading_scores
  )

  if (
    length(best_heading_pos) == 0L ||
      heading_scores[[best_heading_pos]] <= 0L
  ) {
    return(NA_integer_)
  }

  best_heading_idx <- heading_idx[
    best_heading_pos[[1]]
  ]

  section_blocks <- section_content_blocks(
    blocks,
    best_heading_idx,
    region_end
  )

  if (length(section_blocks) == 0L) {
    return(NA_integer_)
  }

  section_blocks[[1]]
}

# ------------------------------------------------------------
# Regression self-tests for heading-independent semantic mapping
# ------------------------------------------------------------

if (run_internal_self_tests) {
  semantic_mapping_test_blocks <- list(
    list(
      type = "heading",
      level = 2L,
      text = "Results"
    ),
    list(
      type = "heading",
      level = 3L,
      text = "Study characteristics"
    ),
    list(
      type = "paragraph",
      level = NA_integer_,
      text = paste(
        "The included cohorts differed in sample size and follow-up.",
        "The intervention device and comparator strategy varied across studies."
      )
    ),
    list(
      type = "paragraph",
      level = NA_integer_,
      text = paste(
        "Formal leave-one-out analysis was not performed.",
        "Funnel assessment was descriptive, while Egger testing and meta-regression were not estimable."
      )
    ),
    list(
      type = "heading",
      level = 2L,
      text = "Discussion"
    ),
    list(
      type = "paragraph",
      level = NA_integer_,
      text = "Discussion text."
    )
  )

  semantic_mapping_test_table <- tibble::tibble(
    inferred_role = "study_characteristics",
    title = "Intervention and comparator characteristics",
    resolved_analysis_group = ""
  )

  semantic_mapping_test_diagnostic <- tibble::tibble(
    inferred_role = "diagnostic_result",
    title = "Exploratory diagnostic results",
    resolved_analysis_group = ""
  )

  semantic_table_target <- semantic_target_block(
    semantic_mapping_test_table,
    semantic_mapping_test_blocks
  )

  semantic_diagnostic_target <- semantic_target_block(
    semantic_mapping_test_diagnostic,
    semantic_mapping_test_blocks
  )

  if (
    !identical(
      semantic_table_target,
      3L
    ) ||
      !identical(
        semantic_diagnostic_target,
        4L
      )
  ) {
    stop(
      paste0(
        "Internal R/41 heading-independent semantic mapping self-test failed. ",
        "Expected table->3 and diagnostic->4; observed table->",
        safe_chr(
          semantic_table_target,
          "NA"
        ),
        ", diagnostic->",
        safe_chr(
          semantic_diagnostic_target,
          "NA"
        ),
        "."
      ),
      call. = FALSE
    )
  }

}

# First map every publication artifact to its first manuscript mention.
mapping_rows <- list()

if (nrow(publication_artifacts) > 0L) {
  for (i in seq_len(nrow(publication_artifacts))) {
    artifact_row <- publication_artifacts[
      i,
      ,
      drop = FALSE
    ]

    anchor_text <- artifact_anchor_text(
      artifact_row$coverage_token[[1]],
      results_draft_text
    )

    target_block <- find_best_manuscript_block(
      anchor_text,
      markdown_blocks
    )

    mapping_method <- if (!is.na(target_block)) {
      "ANCHOR_TEXT"
    } else {
      "UNMAPPED"
    }

    if (is.na(target_block)) {
      target_block <- semantic_target_block(
        artifact_row,
        markdown_blocks
      )

      if (!is.na(target_block)) {
        mapping_method <- "SEMANTIC_ROLE"
      }
    }

    mapping_rows[[
      length(mapping_rows) + 1L
    ]] <- tibble::tibble(
      artifact_id = artifact_row$artifact_id[[1]],
      manuscript_block_index = target_block,
      mapping_method = mapping_method,
      anchor_text = anchor_text,
      mapped_to_body = !is.na(target_block)
    )
  }
}

mapping_table <- if (length(mapping_rows) > 0L) {
  dplyr::bind_rows(
    mapping_rows
  )
} else {
  tibble::tibble(
    artifact_id = character(0),
    manuscript_block_index = integer(0),
    mapping_method = character(0),
    anchor_text = character(0),
    mapped_to_body = logical(0)
  )
}

if (nrow(publication_artifacts) > 0L) {
  publication_artifacts <- publication_artifacts |>
    dplyr::left_join(
      mapping_table,
      by = "artifact_id"
    )
} else {
  publication_artifacts$manuscript_block_index <- integer(0)
  publication_artifacts$mapping_method <- character(0)
  publication_artifacts$anchor_text <- character(0)
  publication_artifacts$mapped_to_body <- logical(0)
}

if (
  nrow(publication_artifacts) > 0L &&
    any(
      !publication_artifacts$mapped_to_body
    )
) {
  stop(
    paste0(
      "R/41 cross-reference synchronization failed after both ",
      "anchor-text and semantic-role mapping. Could not map artifact ID(s): ",
      paste(
        publication_artifacts$artifact_id[
          !publication_artifacts$mapped_to_body
        ],
        collapse = ", "
      ),
      ". DOCX export stopped before numbering."
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 6.1.3 FIRST-MENTION NUMBERING CONTRACT
# ------------------------------------------------------------
# Table numbers are assigned by the order in which tables are first cited in
# the final manuscript body. Figure numbers are independently assigned by the
# order in which figures are first cited. No artifact receives a display
# number before manuscript_block_index has been resolved.

if (nrow(publication_artifacts) > 0L) {
  publication_artifacts <- publication_artifacts |>
    dplyr::group_by(
      artifact_type
    ) |>
    dplyr::arrange(
      manuscript_block_index,
      narrative_order,
      priority_rank,
      role_rank,
      title,
      artifact_id,
      .by_group = TRUE
    ) |>
    dplyr::mutate(
      display_number = dplyr::row_number(),
      display_label = dplyr::case_when(
        artifact_type == "table" ~ paste(
          "Table",
          display_number
        ),
        artifact_type == "figure" ~ paste(
          "Figure",
          display_number
        ),
        TRUE ~ paste(
          artifact_type,
          display_number
        )
      )
    ) |>
    dplyr::ungroup()
}

# Hard invariant: numbering must be monotonic in first-mention order,
# independently for tables and figures.
validate_first_mention_numbering <- function(
    artifacts,
    type
) {
  subset <- artifacts |>
    dplyr::filter(
      artifact_type == type
    ) |>
    dplyr::arrange(
      display_number
    )

  if (nrow(subset) <= 1L) {
    return(TRUE)
  }

  if (
    any(
      diff(
        subset$manuscript_block_index
      ) <
        0L
    )
  ) {
    return(FALSE)
  }

  identical(
    subset$display_number,
    seq_len(
      nrow(
        subset
      )
    )
  )
}

if (
  !validate_first_mention_numbering(
    publication_artifacts,
    "table"
  )
) {
  stop(
    "R/41 first-mention numbering contract failed for tables.",
    call. = FALSE
  )
}

if (
  !validate_first_mention_numbering(
    publication_artifacts,
    "figure"
  )
) {
  stop(
    "R/41 first-mention numbering contract failed for figures.",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 6.1.3A Figure-discovery survival gate
# ------------------------------------------------------------
# If the manuscript artifact registry contained usable figure files, at least
# one final semantic figure must survive discovery/deduplication/mapping.
# Silent omission of all figures is not allowed.

final_semantic_figure_count <- if (
  nrow(publication_artifacts) > 0L
) {
  sum(
    publication_artifacts$artifact_type == "figure",
    na.rm = TRUE
  )
} else {
  0L
}

if (
  available_registry_figure_count > 0L &&
    final_semantic_figure_count == 0L
) {
  stop(
    paste0(
      "R/41 figure discovery contract failed: ",
      available_registry_figure_count,
      " usable figure file(s) were present in manuscript_artifact_registry.csv, ",
      "but no semantic figure survived into the final manuscript figure set. ",
      "DOCX export stopped to prevent silent omission of figures. See: ",
      normalize_slash(
        figure_discovery_audit_file
      )
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 6.1.4 Insert exactly those assigned display labels into mapped prose
# ------------------------------------------------------------

normalize_display_reference_text <- function(x) {
  x <- paste(
    as.character(x),
    collapse = " "
  )

  x <- tolower(x)

  x <- gsub(
    "\u00A0",
    " ",
    x,
    fixed = TRUE
  )

  x <- gsub(
    "[[:space:]]+",
    " ",
    x,
    perl = TRUE
  )

  trimws(x)
}

display_label_present <- function(
    text,
    label
) {
  text <- normalize_display_reference_text(
    text
  )

  label <- normalize_display_reference_text(
    safe_chr(
      label,
      ""
    )
  )

  if (
    !nzchar(text) ||
      !nzchar(label)
  ) {
    return(FALSE)
  }

  positions <- gregexpr(
    label,
    text,
    fixed = TRUE
  )[[1]]

  if (
    length(positions) == 1L &&
      positions[[1]] < 0L
  ) {
    return(FALSE)
  }

  label_length <- nchar(
    label
  )

  text_length <- nchar(
    text
  )

  for (position in positions[
    positions > 0L
  ]) {
    before_position <- position - 1L
    after_position <- position + label_length

    before_character <- if (
      before_position >= 1L
    ) {
      substr(
        text,
        before_position,
        before_position
      )
    } else {
      ""
    }

    after_character <- if (
      after_position <= text_length
    ) {
      substr(
        text,
        after_position,
        after_position
      )
    } else {
      ""
    }

    before_is_boundary <- (
      !nzchar(before_character) ||
        !grepl(
          "[[:alnum:]_]",
          before_character,
          perl = TRUE
        )
    )

    after_is_boundary <- (
      !nzchar(after_character) ||
        !grepl(
          "[[:alnum:]_]",
          after_character,
          perl = TRUE
        )
    )

    if (
      before_is_boundary &&
        after_is_boundary
    ) {
      return(TRUE)
    }
  }

  FALSE
}

if (run_internal_self_tests) {
  display_label_match_self_test <- c(
    display_label_present(
      "The finding is shown in (Figure 1).",
      "Figure 1"
    ),
    display_label_present(
      "See TABLE 2 for study characteristics.",
      "Table 2"
    ),
    display_label_present(
      "See Figure   3 for the pooled estimate.",
      "Figure 3"
    ),
    display_label_present(
      "See Figure\u00A04.",
      "Figure 4"
    ),
    !display_label_present(
      "See Figure 10.",
      "Figure 1"
    ),
    !display_label_present(
      "See Figure 1A.",
      "Figure 1"
    ),
    !display_label_present(
      "See xFigure 1.",
      "Figure 1"
    )
  )

  if (!all(display_label_match_self_test)) {
    stop(
      "Internal R/41 display-label boundary matching self-test failed.",
      call. = FALSE
    )
  }
}

append_display_references <- function(
    paragraph_text,
    labels
) {
  labels <- unique(
    labels[
      !is.na(labels) &
        nzchar(labels)
    ]
  )

  if (length(labels) == 0L) {
    return(paragraph_text)
  }

  missing_labels <- labels[
    !vapply(
      labels,
      function(label) {
        display_label_present(
          paragraph_text,
          label
        )
      },
      logical(1)
    )
  ]

  if (length(missing_labels) == 0L) {
    return(paragraph_text)
  }

  reference_text <- paste(
    missing_labels,
    collapse = "; "
  )

  if (
    grepl(
      "[.!?]$",
      paragraph_text,
      perl = TRUE
    )
  ) {
    return(
      sub(
        "([.!?])$",
        paste0(
          " (",
          reference_text,
          ")\\1"
        ),
        paragraph_text,
        perl = TRUE
      )
    )
  }

  paste0(
    paragraph_text,
    " (",
    reference_text,
    ")"
  )
}

block_crossrefs <- list()

if (nrow(publication_artifacts) > 0L) {
  citation_order_artifacts <- publication_artifacts |>
    dplyr::arrange(
      manuscript_block_index,
      artifact_type,
      display_number
    )

  for (i in seq_len(nrow(citation_order_artifacts))) {
    row <- citation_order_artifacts[
      i,
      ,
      drop = FALSE
    ]

    key <- as.character(
      row$manuscript_block_index[[1]]
    )

    prior_labels <- if (
      !is.null(
        block_crossrefs[[key]]
      )
    ) {
      block_crossrefs[[key]]
    } else {
      character(0)
    }

    block_crossrefs[[key]] <- c(
      prior_labels,
      row$display_label[[1]]
    )
  }
}

if (length(block_crossrefs) > 0L) {
  for (key in names(block_crossrefs)) {
    block_index <- suppressWarnings(
      as.integer(
        key
      )
    )

    if (
      is.na(block_index) ||
        block_index < 1L ||
        block_index > length(markdown_blocks)
    ) {
      next
    }

    markdown_blocks[[block_index]]$text <- append_display_references(
      markdown_blocks[[block_index]]$text,
      block_crossrefs[[key]]
    )
  }
}

# ------------------------------------------------------------
# 6.1.5 Final audit map + body/caption numbering identity gate
# ------------------------------------------------------------

crossref_map <- if (nrow(publication_artifacts) > 0L) {
  publication_artifacts |>
    dplyr::transmute(
      artifact_id = artifact_id,
      artifact_type = artifact_type,
      manuscript_role = manuscript_role,
      inferred_role = inferred_role,
      analysis_group = analysis_group,
      resolved_analysis_group = resolved_analysis_group,
      semantic_key = semantic_key,
      manuscript_block_index = manuscript_block_index,
      mapping_method = mapping_method,
      display_number = display_number,
      display_label = display_label,
      title = title,
      coverage_token = coverage_token,
      anchor_text = anchor_text,
      mapped_to_body = mapped_to_body
    )
} else {
  tibble::tibble(
    artifact_id = character(0),
    artifact_type = character(0),
    manuscript_role = character(0),
    inferred_role = character(0),
    analysis_group = character(0),
    resolved_analysis_group = character(0),
    semantic_key = character(0),
    manuscript_block_index = integer(0),
    mapping_method = character(0),
    display_number = integer(0),
    display_label = character(0),
    title = character(0),
    coverage_token = character(0),
    anchor_text = character(0),
    mapped_to_body = logical(0),
    body_reference_present = logical(0),
    mapped_block_reference_present = logical(0)
  )
}

if (nrow(crossref_map) > 0L) {
  body_text_after_crossrefs <- paste(
    vapply(
      markdown_blocks,
      function(block) {
        safe_chr(
          block$text,
          ""
        )
      },
      character(1)
    ),
    collapse = "\n"
  )

  crossref_map$body_reference_present <- vapply(
    crossref_map$display_label,
    function(label) {
      display_label_present(
        body_text_after_crossrefs,
        label
      )
    },
    logical(1)
  )

  crossref_map$mapped_block_reference_present <- vapply(
    seq_len(
      nrow(
        crossref_map
      )
    ),
    function(i) {
      block_index <- crossref_map$manuscript_block_index[[i]]
      label <- crossref_map$display_label[[i]]

      if (
        is.na(block_index) ||
          block_index < 1L ||
          block_index > length(markdown_blocks)
      ) {
        return(FALSE)
      }

      display_label_present(
        markdown_blocks[[block_index]]$text,
        label
      )
    },
    logical(1)
  )

  missing_global <- !crossref_map$body_reference_present
  missing_local <- !crossref_map$mapped_block_reference_present

  if (any(missing_global | missing_local)) {
    failed_labels <- unique(
      crossref_map$display_label[
        missing_global |
          missing_local
      ]
    )

    stop(
      paste0(
        "R/41 body-reference insertion failed for: ",
        paste(
          failed_labels,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
}

readr::write_csv(
  crossref_map,
  crossref_file,
  na = ""
)


# ============================================================
# 7. Table/figure artifact helpers
# ============================================================

read_table_artifact <- function(path) {
  ext <- tolower(
    tools::file_ext(
      path
    )
  )

  if (identical(ext, "csv")) {
    return(
      readr::read_csv(
        path,
        show_col_types = FALSE,
        progress = FALSE,
        name_repair = "minimal"
      )
    )
  }

  if (identical(ext, "tsv")) {
    return(
      readr::read_tsv(
        path,
        show_col_types = FALSE,
        progress = FALSE,
        name_repair = "minimal"
      )
    )
  }

  if (identical(ext, "xlsx")) {
    sheets <- tryCatch(
      openxlsx::getSheetNames(
        path
      ),
      error = function(e) character(0)
    )

    if (length(sheets) == 0L) {
      return(
        NULL
      )
    }

    for (sheet in sheets) {
      data <- tryCatch(
        openxlsx::read.xlsx(
          path,
          sheet = sheet,
          check.names = FALSE
        ),
        error = function(e) NULL
      )

      if (
        !is.null(data) &&
          (
            nrow(data) > 0L ||
              ncol(data) > 0L
          )
      ) {
        return(
          tibble::as_tibble(
            data,
            .name_repair = "minimal"
          )
        )
      }
    }

    return(
      NULL
    )
  }

  # Legacy .xls files are not silently reinterpreted. They remain in the
  # copied author-review package and are reported as not converted.
  if (identical(ext, "xls")) {
    return(
      NULL
    )
  }

  NULL
}

make_word_table <- function(
    data,
    font_size = 9
) {
  data <- as.data.frame(
    data,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  data[] <- lapply(
    data,
    function(column) {
      if (is.list(column)) {
        return(
          vapply(
            column,
            function(value) {
              paste(
                as.character(value),
                collapse = "; "
              )
            },
            character(1)
          )
        )
      }

      as.character(
        column
      )
    }
  )

  ft <- flextable::flextable(
    data
  )

  ft <- flextable::theme_booktabs(
    ft
  )

  ft <- flextable::bold(
    ft,
    part = "header"
  )

  ft <- flextable::fontsize(
    ft,
    size = font_size,
    part = "all"
  )

  ft <- flextable::font(
    ft,
    fontname = "Arial",
    part = "all"
  )

  ft <- flextable::align(
    ft,
    align = "left",
    part = "all"
  )

  ft <- flextable::valign(
    ft,
    valign = "top",
    part = "all"
  )

  ft <- flextable::set_table_properties(
    ft,
    layout = "autofit",
    width = 1
  )

  ft <- flextable::autofit(
    ft
  )

  ft
}

normalize_figure_extension <- function(path) {
  tolower(
    gsub(
      "^\\.",
      "",
      tools::file_ext(
        safe_chr(
          path,
          ""
        )
      )
    )
  )
}

figure_is_direct_word_image <- function(path) {
  normalize_figure_extension(
    path
  ) %in% c(
    "png",
    "jpg",
    "jpeg"
  )
}

# ------------------------------------------------------------
# Strict figure-render result contract
# ------------------------------------------------------------
# Every renderer and every normalization path must resolve to exactly one
# named list with the same fields. No downstream code is allowed to use `$`
# on an unnormalized external-return object.

figure_render_result <- function(
    ok = FALSE,
    path = "",
    method = "",
    message = "",
    width = NA_real_,
    height = NA_real_,
    raw_return_class = ""
) {
  list(
    ok = isTRUE(ok),
    path = safe_chr(
      path,
      ""
    ),
    method = safe_chr(
      method,
      ""
    ),
    message = safe_chr(
      message,
      ""
    ),
    width = suppressWarnings(
      as.numeric(
        width
      )
    )[1],
    height = suppressWarnings(
      as.numeric(
        height
      )
    )[1],
    raw_return_class = safe_chr(
      raw_return_class,
      ""
    )
  )
}

render_field <- function(
    x,
    name,
    default = NULL
) {
  if (
    is.null(x) ||
      length(x) == 0L
  ) {
    return(default)
  }

  x_names <- names(x)

  if (
    is.null(x_names) ||
      !(name %in% x_names)
  ) {
    return(default)
  }

  index <- match(
    name,
    x_names
  )

  if (
    is.na(index) ||
      index < 1L ||
      index > length(x)
  ) {
    return(default)
  }

  value <- tryCatch(
    {
      if (is.list(x)) {
        x[index][[1]]
      } else {
        x[index]
      }
    },
    error = function(e) {
      default
    }
  )

  if (
    is.null(value) ||
      length(value) == 0L
  ) {
    return(default)
  }

  value
}

render_value <- function(
    x,
    name,
    default = NULL
) {
  value <- render_field(
    x,
    name,
    default
  )

  if (
    is.null(value) ||
      length(value) == 0L
  ) {
    return(default)
  }

  value
}

render_chr <- function(
    x,
    name,
    default = ""
) {
  value <- render_value(
    x,
    name,
    default
  )

  safe_chr(
    value,
    default
  )
}

render_num <- function(
    x,
    name,
    default = NA_real_
) {
  value <- render_value(
    x,
    name,
    default
  )

  safe_num(
    value,
    default
  )
}

render_flag <- function(
    x,
    name,
    default = FALSE
) {
  value <- render_value(
    x,
    name,
    default
  )

  if (
    is.null(value) ||
      length(value) == 0L ||
      is.na(value[1])
  ) {
    return(default)
  }

  isTRUE(
    as.logical(
      value[1]
    )
  )
}

normalize_figure_render_result <- function(
    x,
    fallback_path = "",
    method_hint = ""
) {
  raw_class <- paste(
    class(x),
    collapse = "/"
  )

  # Canonical/list-like result.
  if (is.list(x)) {
    ok <- isTRUE(
      render_field(
        x,
        "ok",
        FALSE
      )
    )

    path <- safe_chr(
      render_field(
        x,
        "path",
        fallback_path
      ),
      fallback_path
    )

    method <- safe_chr(
      render_field(
        x,
        "method",
        method_hint
      ),
      method_hint
    )

    message <- safe_chr(
      render_field(
        x,
        "message",
        ""
      ),
      ""
    )

    width <- suppressWarnings(
      as.numeric(
        render_field(
          x,
          "width",
          NA_real_
        )
      )
    )[1]

    height <- suppressWarnings(
      as.numeric(
        render_field(
          x,
          "height",
          NA_real_
        )
      )
    )[1]

    # File existence is authoritative for a claimed successful render.
    if (
      ok &&
        !file_nonempty(
          path
        )
    ) {
      ok <- FALSE

      message <- paste0(
        if (nzchar(message)) {
          paste0(
            message,
            " "
          )
        } else {
          ""
        },
        "Renderer reported success but produced no non-empty image file."
      )
    }

    return(
      figure_render_result(
        ok = ok,
        path = path,
        method = method,
        message = message,
        width = width,
        height = height,
        raw_return_class = raw_class
      )
    )
  }

  # Some third-party renderers return a filename vector.
  if (
    is.character(x) &&
      length(x) >= 1L
  ) {
    candidates <- unique(
      c(
        fallback_path,
        as.character(
          x
        )
      )
    )

    candidates <- candidates[
      !is.na(candidates) &
        nzchar(candidates)
    ]

    if (length(candidates) > 0L) {
      ready <- candidates[
        vapply(
          candidates,
          file_nonempty,
          logical(1)
        )
      ]

      if (length(ready) > 0L) {
        return(
          figure_render_result(
            ok = TRUE,
            path = ready[[1]],
            method = if (nzchar(method_hint)) {
              method_hint
            } else {
              "ATOMIC_CHARACTER_PATH_RETURN"
            },
            message = "",
            raw_return_class = raw_class
          )
        )
      }
    }

    return(
      figure_render_result(
        ok = FALSE,
        path = "",
        method = method_hint,
        message = paste0(
          "Renderer returned character data but no non-empty image path was produced. ",
          "Raw class: ",
          raw_class,
          "."
        ),
        raw_return_class = raw_class
      )
    )
  }

  # Some renderer APIs return TRUE/FALSE while writing to a requested path.
  if (
    is.logical(x) &&
      length(x) >= 1L
  ) {
    ok <- isTRUE(
      x[[1]]
    ) &&
      file_nonempty(
        fallback_path
      )

    return(
      figure_render_result(
        ok = ok,
        path = if (ok) {
          fallback_path
        } else {
          ""
        },
        method = if (nzchar(method_hint)) {
          method_hint
        } else {
          "ATOMIC_LOGICAL_RETURN"
        },
        message = if (ok) {
          ""
        } else {
          paste0(
            "Renderer returned logical data but no non-empty image was produced. ",
            "Raw class: ",
            raw_class,
            "."
          )
        },
        raw_return_class = raw_class
      )
    )
  }

  figure_render_result(
    ok = FALSE,
    path = "",
    method = method_hint,
    message = paste0(
      "Unexpected renderer return type was normalized safely. Raw class: ",
      raw_class,
      "."
    ),
    raw_return_class = raw_class
  )
}

canonicalize_figure_render_result <- function(
    x,
    fallback_path = "",
    method_hint = ""
) {
  normalized <- normalize_figure_render_result(
    x,
    fallback_path = fallback_path,
    method_hint = method_hint
  )

  figure_render_result(
    ok = render_flag(
      normalized,
      "ok",
      FALSE
    ),
    path = render_chr(
      normalized,
      "path",
      fallback_path
    ),
    method = render_chr(
      normalized,
      "method",
      method_hint
    ),
    message = render_chr(
      normalized,
      "message",
      ""
    ),
    width = render_num(
      normalized,
      "width",
      NA_real_
    ),
    height = render_num(
      normalized,
      "height",
      NA_real_
    ),
    raw_return_class = render_chr(
      normalized,
      "raw_return_class",
      paste(
        class(x),
        collapse = "/"
      )
    )
  )
}


raw_u32_be <- function(bytes) {
  values <- as.integer(
    bytes
  )

  if (length(values) != 4L) {
    return(
      NA_real_
    )
  }

  values[[1]] * 256^3 +
    values[[2]] * 256^2 +
    values[[3]] * 256 +
    values[[4]]
}

png_pixel_dimensions <- function(path) {
  if (!file_nonempty(path)) {
    return(
      c(
        width = NA_real_,
        height = NA_real_
      )
    )
  }

  con <- file(
    path,
    open = "rb"
  )

  on.exit(
    close(con),
    add = TRUE
  )

  bytes <- readBin(
    con,
    what = "raw",
    n = 24L
  )

  if (length(bytes) < 24L) {
    return(
      c(
        width = NA_real_,
        height = NA_real_
      )
    )
  }

  signature <- as.integer(
    bytes[
      1:8
    ]
  )

  expected_signature <- c(
    137L,
    80L,
    78L,
    71L,
    13L,
    10L,
    26L,
    10L
  )

  if (!identical(
    signature,
    expected_signature
  )) {
    return(
      c(
        width = NA_real_,
        height = NA_real_
      )
    )
  }

  c(
    width = raw_u32_be(
      bytes[
        17:20
      ]
    ),
    height = raw_u32_be(
      bytes[
        21:24
      ]
    )
  )
}

jpeg_pixel_dimensions <- function(path) {
  if (!file_nonempty(path)) {
    return(
      c(
        width = NA_real_,
        height = NA_real_
      )
    )
  }

  size <- suppressWarnings(
    as.numeric(
      file.info(
        path
      )$size
    )
  )

  if (
    length(size) == 0L ||
      is.na(size[[1]]) ||
      size[[1]] < 4
  ) {
    return(
      c(
        width = NA_real_,
        height = NA_real_
      )
    )
  }

  con <- file(
    path,
    open = "rb"
  )

  on.exit(
    close(con),
    add = TRUE
  )

  bytes <- readBin(
    con,
    what = "raw",
    n = as.integer(
      size[[1]]
    )
  )

  values <- as.integer(
    bytes
  )

  if (
    length(values) < 4L ||
      values[[1]] != 255L ||
      values[[2]] != 216L
  ) {
    return(
      c(
        width = NA_real_,
        height = NA_real_
      )
    )
  }

  sof_markers <- c(
    192L,
    193L,
    194L,
    195L,
    197L,
    198L,
    199L,
    201L,
    202L,
    203L,
    205L,
    206L,
    207L
  )

  standalone_markers <- c(
    1L,
    216L,
    217L,
    208:215
  )

  pos <- 3L
  n <- length(
    values
  )

  while (pos <= n - 8L) {
    if (values[[pos]] != 255L) {
      pos <- pos + 1L
      next
    }

    while (
      pos <= n &&
        values[[pos]] == 255L
    ) {
      pos <- pos + 1L
    }

    if (pos > n) {
      break
    }

    marker <- values[[pos]]
    pos <- pos + 1L

    if (marker %in% standalone_markers) {
      next
    }

    if (pos + 1L > n) {
      break
    }

    segment_length <- values[[pos]] * 256 +
      values[[pos + 1L]]

    if (
      marker %in% sof_markers &&
        pos + 6L <= n
    ) {
      height <- values[[pos + 3L]] * 256 +
        values[[pos + 4L]]

      width <- values[[pos + 5L]] * 256 +
        values[[pos + 6L]]

      return(
        c(
          width = width,
          height = height
        )
      )
    }

    if (
      is.na(segment_length) ||
        segment_length < 2L
    ) {
      break
    }

    pos <- pos + segment_length
  }

  c(
    width = NA_real_,
    height = NA_real_
  )
}

image_pixel_dimensions <- function(path) {
  ext <- normalize_figure_extension(
    path
  )

  if (identical(ext, "png")) {
    return(
      png_pixel_dimensions(
        path
      )
    )
  }

  if (ext %in% c(
    "jpg",
    "jpeg"
  )) {
    return(
      jpeg_pixel_dimensions(
        path
      )
    )
  }

  c(
    width = NA_real_,
    height = NA_real_
  )
}

fit_word_image_dimensions <- function(
    path,
    max_width = 6.2,
    max_height = 8.0
) {
  pixels <- image_pixel_dimensions(
    path
  )

  width_px <- suppressWarnings(
    as.numeric(
      pixels[["width"]]
    )
  )

  height_px <- suppressWarnings(
    as.numeric(
      pixels[["height"]]
    )
  )

  if (
    length(width_px) == 0L ||
      length(height_px) == 0L ||
      is.na(width_px[[1]]) ||
      is.na(height_px[[1]]) ||
      width_px[[1]] <= 0 ||
      height_px[[1]] <= 0
  ) {
    return(
      c(
        width = max_width,
        height = min(
          max_height,
          max_width * 0.75
        )
      )
    )
  }

  ratio <- width_px[[1]] /
    height_px[[1]]

  width_in <- max_width
  height_in <- width_in /
    ratio

  if (height_in > max_height) {
    height_in <- max_height
    width_in <- height_in *
      ratio
  }

  c(
    width = width_in,
    height = height_in
  )
}

# ------------------------------------------------------------
# Managed optional renderer dependencies
# ------------------------------------------------------------

figure_dependency_events <- list()
figure_dependency_attempted <- new.env(
  parent = emptyenv()
)

write_figure_dependency_audit <- function() {
  audit <- if (
    length(figure_dependency_events) > 0L
  ) {
    dplyr::bind_rows(
      figure_dependency_events
    )
  } else {
    tibble::tibble(
      package = character(0),
      reason = character(0),
      available_before = logical(0),
      install_attempted = logical(0),
      install_success = logical(0),
      available_after = logical(0),
      library_path = character(0),
      repository = character(0),
      install_type = character(0),
      message = character(0)
    )
  }

  readr::write_csv(
    audit,
    figure_render_dependency_audit_file,
    na = ""
  )

  invisible(
    audit
  )
}

record_figure_dependency_event <- function(
    package,
    reason,
    available_before,
    install_attempted,
    install_success,
    available_after,
    library_path,
    repository,
    install_type,
    message
) {
  figure_dependency_events[[
    length(figure_dependency_events) + 1L
  ]] <<- tibble::tibble(
    package = safe_chr(package, ""),
    reason = safe_chr(reason, ""),
    available_before = isTRUE(available_before),
    install_attempted = isTRUE(install_attempted),
    install_success = isTRUE(install_success),
    available_after = isTRUE(available_after),
    library_path = normalize_slash(
      safe_chr(library_path, "")
    ),
    repository = safe_chr(repository, ""),
    install_type = safe_chr(install_type, ""),
    message = safe_chr(message, "")
  )

  write_figure_dependency_audit()
}

ensure_figure_render_package <- function(
    package,
    reason = "FIGURE_RENDERING"
) {
  package <- safe_chr(
    package,
    ""
  )

  if (!nzchar(package)) {
    return(FALSE)
  }

  available_before <- requireNamespace(
    package,
    quietly = TRUE
  )

  if (available_before) {
    if (!exists(
      package,
      envir = figure_dependency_attempted,
      inherits = FALSE
    )) {
      assign(
        package,
        TRUE,
        envir = figure_dependency_attempted
      )

      record_figure_dependency_event(
        package = package,
        reason = reason,
        available_before = TRUE,
        install_attempted = FALSE,
        install_success = FALSE,
        available_after = TRUE,
        library_path = "",
        repository = figure_render_cran_repo,
        install_type = "ALREADY_AVAILABLE",
        message = "Package was already available."
      )
    }

    return(TRUE)
  }

  if (exists(
    package,
    envir = figure_dependency_attempted,
    inherits = FALSE
  )) {
    return(
      requireNamespace(
        package,
        quietly = TRUE
      )
    )
  }

  assign(
    package,
    TRUE,
    envir = figure_dependency_attempted
  )

  if (!auto_install_figure_render_dependencies) {
    record_figure_dependency_event(
      package = package,
      reason = reason,
      available_before = FALSE,
      install_attempted = FALSE,
      install_success = FALSE,
      available_after = FALSE,
      library_path = figure_runtime_library,
      repository = figure_render_cran_repo,
      install_type = "AUTO_INSTALL_DISABLED",
      message = paste0(
        "Automatic optional renderer installation is disabled. ",
        "Enable analysis.manuscript.docx.auto_install_figure_render_dependencies."
      )
    )

    return(FALSE)
  }

  dir.create(
    figure_runtime_library,
    recursive = TRUE,
    showWarnings = FALSE
  )

  runtime_lib <- normalizePath(
    figure_runtime_library,
    winslash = "/",
    mustWork = FALSE
  )

  .libPaths(
    unique(
      c(
        runtime_lib,
        .libPaths()
      )
    )
  )

  old_timeout <- getOption(
    "timeout"
  )

  on.exit(
    options(
      timeout = old_timeout
    ),
    add = TRUE
  )

  timeout_value <- suppressWarnings(
    as.numeric(
      old_timeout
    )
  )

  if (
    length(timeout_value) == 0L ||
      is.na(timeout_value[[1]])
  ) {
    timeout_value <- 60
  } else {
    timeout_value <- timeout_value[[1]]
  }

  options(
    timeout = max(
      600,
      timeout_value
    )
  )

  install_type <- if (
    identical(
      .Platform$OS.type,
      "windows"
    )
  ) {
    "binary"
  } else {
    "default"
  }

  cat(
    paste0(
      "\nR/41: installing optional renderer package '",
      package,
      "' into isolated runtime library:\n  ",
      normalize_slash(
        runtime_lib
      ),
      "\n"
    )
  )

  install_error <- ""
  install_warnings <- character(0)

  install_success <- tryCatch(
    {
      install_args <- list(
        pkgs = package,
        lib = runtime_lib,
        repos = figure_render_cran_repo,
        dependencies = NA,
        quiet = FALSE
      )

      if (
        identical(
          .Platform$OS.type,
          "windows"
        )
      ) {
        install_args$type <- "binary"
      }

      withCallingHandlers(
        {
          do.call(
            utils::install.packages,
            install_args
          )
        },
        warning = function(w) {
          install_warnings <<- c(
            install_warnings,
            conditionMessage(
              w
            )
          )

          invokeRestart(
            "muffleWarning"
          )
        }
      )

      requireNamespace(
        package,
        quietly = TRUE
      )
    },
    error = function(e) {
      install_error <<- conditionMessage(
        e
      )

      FALSE
    }
  )

  available_after <- requireNamespace(
    package,
    quietly = TRUE
  )

  install_success <- isTRUE(
    install_success
  ) &&
    available_after

  event_message <- if (install_success) {
    paste0(
      "Package installed successfully in the isolated R/41 runtime library. ",
      "The canonical project dependency lockfile was not modified.",
      if (length(install_warnings) > 0L) {
        paste0(
          " Installer warning(s): ",
          paste(
            unique(
              install_warnings
            ),
            collapse = " || "
          )
        )
      } else {
        ""
      }
    )
  } else {
    paste0(
      "Automatic installation did not make the package available.",
      if (nzchar(install_error)) {
        paste0(
          " Error: ",
          install_error
        )
      } else {
        ""
      },
      if (length(install_warnings) > 0L) {
        paste0(
          " Installer warning(s): ",
          paste(
            unique(
              install_warnings
            ),
            collapse = " || "
          )
        )
      } else {
        ""
      }
    )
  }

  record_figure_dependency_event(
    package = package,
    reason = reason,
    available_before = FALSE,
    install_attempted = TRUE,
    install_success = install_success,
    available_after = available_after,
    library_path = runtime_lib,
    repository = figure_render_cran_repo,
    install_type = install_type,
    message = event_message
  )

  available_after
}

bootstrap_pdf_renderer <- function() {
  ensure_figure_render_package(
    "pdftools",
    reason = "PDF_TO_WORD_IMAGE"
  )
}


raster_figure_extensions <- c(
  "png",
  "jpg",
  "jpeg"
)

normalize_existing_paths <- function(paths) {
  paths <- as.character(
    paths
  )

  paths <- paths[
    !is.na(paths) &
      nzchar(paths)
  ]

  if (length(paths) == 0L) {
    return(
      character(0)
    )
  }

  paths <- unique(
    normalizePath(
      paths,
      winslash = "/",
      mustWork = FALSE
    )
  )

  paths[
    vapply(
      paths,
      file_nonempty,
      logical(1)
    )
  ]
}

same_file_stem <- function(
    path,
    stem
) {
  identical(
    tolower(
      tools::file_path_sans_ext(
        basename(
          safe_chr(
            path,
            ""
          )
        )
      )
    ),
    tolower(
      safe_chr(
        stem,
        ""
      )
    )
  )
}

find_raster_companion <- function(
    source,
    semantic_candidates = character(0),
    project_root = ""
) {
  source <- safe_chr(
    source,
    ""
  )

  if (!file_nonempty(source)) {
    return(
      figure_render_result(
        ok = FALSE,
        path = "",
        method = "RASTER_COMPANION_SOURCE_MISSING",
        message = "Raster companion search skipped because the canonical source is missing."
      )
    )
  }

  source_stem <- tools::file_path_sans_ext(
    basename(
      source
    )
  )

  # 1. Same semantic figure family from manuscript_artifact_registry.csv.
  semantic_candidates <- normalize_existing_paths(
    semantic_candidates
  )

  semantic_rasters <- semantic_candidates[
    vapply(
      semantic_candidates,
      function(path) {
        normalize_figure_extension(
          path
        ) %in% raster_figure_extensions
      },
      logical(1)
    )
  ]

  if (length(semantic_rasters) > 0L) {
    exact_semantic_stem <- semantic_rasters[
      vapply(
        semantic_rasters,
        same_file_stem,
        logical(1),
        stem = source_stem
      )
    ]

    chosen <- if (length(exact_semantic_stem) > 0L) {
      exact_semantic_stem[[1]]
    } else {
      semantic_rasters[[1]]
    }

    return(
      figure_render_result(
        ok = TRUE,
        path = chosen,
        method = "RASTER_COMPANION_SEMANTIC_REGISTRY",
        message = ""
      )
    )
  }

  # 2. Exact same-stem raster in the canonical source directory.
  source_dir <- dirname(
    source
  )

  sibling_candidates <- file.path(
    source_dir,
    paste0(
      source_stem,
      c(
        ".png",
        ".PNG",
        ".jpg",
        ".JPG",
        ".jpeg",
        ".JPEG"
      )
    )
  )

  sibling_candidates <- normalize_existing_paths(
    sibling_candidates
  )

  if (length(sibling_candidates) > 0L) {
    return(
      figure_render_result(
        ok = TRUE,
        path = sibling_candidates[[1]],
        method = "RASTER_COMPANION_SAME_DIRECTORY",
        message = ""
      )
    )
  }

  # 3. Search the project's results tree for an exact same-stem raster.
  # Only use an unambiguous result; never guess among multiple different files.
  project_root <- safe_chr(
    project_root,
    ""
  )

  if (nzchar(project_root)) {
    results_root <- file.path(
      project_root,
      "results"
    )

    if (dir.exists(results_root)) {
      all_rasters <- tryCatch(
        list.files(
          results_root,
          pattern = "\\.(png|jpe?g)$",
          recursive = TRUE,
          full.names = TRUE,
          ignore.case = TRUE
        ),
        error = function(e) {
          character(0)
        }
      )

      all_rasters <- normalize_existing_paths(
        all_rasters
      )

      # Do not reuse R/41's own generated render cache.
      all_rasters <- all_rasters[
        !grepl(
          "/manuscript/production/author_review/figures/_word_render/",
          normalize_slash(
            all_rasters
          ),
          fixed = TRUE
        )
      ]

      stem_matches <- all_rasters[
        vapply(
          all_rasters,
          same_file_stem,
          logical(1),
          stem = source_stem
        )
      ]

      if (length(stem_matches) == 1L) {
        return(
          figure_render_result(
            ok = TRUE,
            path = stem_matches[[1]],
            method = "RASTER_COMPANION_PROJECT_RESULTS_EXACT_STEM",
            message = ""
          )
        )
      }

      if (length(stem_matches) > 1L) {
        source_dir_norm <- normalize_slash(
          normalizePath(
            source_dir,
            winslash = "/",
            mustWork = FALSE
          )
        )

        same_parent <- stem_matches[
          normalize_slash(
            dirname(
              stem_matches
            )
          ) == source_dir_norm
        ]

        if (length(same_parent) == 1L) {
          return(
            figure_render_result(
              ok = TRUE,
              path = same_parent[[1]],
              method = "RASTER_COMPANION_PROJECT_RESULTS_SAME_PARENT",
              message = ""
            )
          )
        }

        return(
          figure_render_result(
            ok = FALSE,
            path = "",
            method = "RASTER_COMPANION_AMBIGUOUS",
            message = paste0(
              "Multiple same-stem raster files were found in results/; ",
              "R/41 refused to guess which one corresponds to the PDF."
            )
          )
        )
      }
    }
  }

  figure_render_result(
    ok = FALSE,
    path = "",
    method = "RASTER_COMPANION_NOT_FOUND",
    message = "No safe existing PNG/JPEG rendering of this figure was found."
  )
}

find_ghostscript_executable <- function() {
  path_candidates <- unname(
    Sys.which(
      c(
        "gswin64c",
        "gswin32c",
        "gs"
      )
    )
  )

  windows_candidates <- c(
    Sys.glob(
      "C:/Program Files/gs/gs*/bin/gswin64c.exe"
    ),
    Sys.glob(
      "C:/Program Files/gs/gs*/bin/gswin32c.exe"
    ),
    Sys.glob(
      "C:/Program Files (x86)/gs/gs*/bin/gswin64c.exe"
    ),
    Sys.glob(
      "C:/Program Files (x86)/gs/gs*/bin/gswin32c.exe"
    )
  )

  candidates <- unique(
    c(
      path_candidates,
      windows_candidates
    )
  )

  candidates <- candidates[
    !is.na(candidates) &
      nzchar(candidates) &
      file.exists(candidates)
  ]

  if (length(candidates) == 0L) {
    return("")
  }

  # Prefer the last Program Files version when several Ghostscript releases
  # are installed; PATH candidates remain valid fallbacks.
  candidates[[length(candidates)]]
}

system_renderer_inventory <- function() {
  tibble::tibble(
    renderer = c(
      "pdftoppm",
      "pdftocairo",
      "ghostscript",
      "mutool",
      "magick_cli"
    ),
    executable = c(
      safe_chr(
        Sys.which(
          "pdftoppm"
        ),
        ""
      ),
      safe_chr(
        Sys.which(
          "pdftocairo"
        ),
        ""
      ),
      find_ghostscript_executable(),
      safe_chr(
        Sys.which(
          "mutool"
        ),
        ""
      ),
      safe_chr(
        Sys.which(
          "magick"
        ),
        ""
      )
    )
  ) |>
    dplyr::mutate(
      available = nzchar(
        executable
      )
    )
}

run_system_renderer <- function(
    executable,
    args,
    destination,
    method
) {
  executable <- safe_chr(
    executable,
    ""
  )

  if (!nzchar(executable)) {
    return(
      figure_render_result(
        ok = FALSE,
        path = "",
        method = method,
        message = paste0(
          method,
          " executable is not available."
        )
      )
    )
  }

  if (file.exists(destination)) {
    unlink(
      destination,
      force = TRUE
    )
  }

  output <- tryCatch(
    {
      system2(
        executable,
        args = args,
        stdout = TRUE,
        stderr = TRUE
      )
    },
    error = function(e) {
      structure(
        conditionMessage(e),
        status = 1L
      )
    }
  )

  status <- attr(
    output,
    "status"
  )

  if (is.null(status)) {
    status <- 0L
  }

  if (
    identical(
      as.integer(status),
      0L
    ) &&
      file_nonempty(
        destination
      )
  ) {
    return(
      figure_render_result(
        ok = TRUE,
        path = destination,
        method = method,
        message = "",
        raw_return_class = paste(
          class(output),
          collapse = "/"
        )
      )
    )
  }

  figure_render_result(
    ok = FALSE,
    path = "",
    method = method,
    message = paste0(
      method,
      " failed",
      if (length(output) > 0L) {
        paste0(
          ": ",
          paste(
            output,
            collapse = " | "
          )
        )
      } else {
        "."
      }
    ),
    raw_return_class = paste(
      class(output),
      collapse = "/"
    )
  )
}

render_pdf_first_page <- function(
    source,
    destination,
    dpi = 300
) {
  errors <- character(0)

  # If no PDF renderer is installed, bootstrap the CRAN pdftools binary into
  # R/41's isolated library. On Windows this removes the requirement for a
  # separately installed Poppler/Ghostscript executable.
  bootstrap_pdf_renderer()

  remember_failure <- function(result) {
    if (!render_flag(
      result,
      "ok",
      FALSE
    )) {
      msg <- render_chr(
        result,
        "message",
        ""
      )

      if (nzchar(msg)) {
        errors <<- c(
          errors,
          msg
        )
      }
    }

    invisible(
      result
    )
  }

  # Strategy 1: pdftools::pdf_convert
  if (
    requireNamespace(
      "pdftools",
      quietly = TRUE
    ) &&
      "pdf_convert" %in%
        getNamespaceExports(
          "pdftools"
        )
  ) {
    raw_return <- tryCatch(
      {
        suppressMessages(
          pdftools::pdf_convert(
            source,
            format = "png",
            pages = 1,
            filenames = destination,
            dpi = dpi,
            verbose = FALSE
          )
        )
      },
      error = function(e) {
        structure(
          character(0),
          render_error = conditionMessage(e)
        )
      }
    )

    normalized <- canonicalize_figure_render_result(
      raw_return,
      fallback_path = destination,
      method_hint = "pdftools::pdf_convert"
    )

    if (render_flag(
      normalized,
      "ok",
      FALSE
    )) {
      ready <- render_chr(
        normalized,
        "path",
        ""
      )

      if (
        file_nonempty(
          ready
        ) &&
          !identical(
            normalizePath(
              ready,
              winslash = "/",
              mustWork = FALSE
            ),
            normalizePath(
              destination,
              winslash = "/",
              mustWork = FALSE
            )
          )
      ) {
        file.copy(
          ready,
          destination,
          overwrite = TRUE
        )
      }

      if (file_nonempty(destination)) {
        return(
          figure_render_result(
            ok = TRUE,
            path = destination,
            method = "pdftools::pdf_convert",
            message = "",
            raw_return_class = render_chr(
              normalized,
              "raw_return_class",
              ""
            )
          )
        )
      }
    }

    render_error <- attr(
      raw_return,
      "render_error"
    )

    if (!is.null(render_error)) {
      errors <- c(
        errors,
        paste0(
          "pdftools::pdf_convert: ",
          render_error
        )
      )
    } else {
      remember_failure(
        normalized
      )
    }
  }

  # Strategy 2: pdftools::pdf_render_page + png
  if (
    requireNamespace(
      "pdftools",
      quietly = TRUE
    ) &&
      !"pdf_convert" %in%
        getNamespaceExports(
          "pdftools"
        )
  ) {
    ensure_figure_render_package(
      "png",
      reason = "PDF_RENDER_PAGE_TO_PNG"
    )
  }

  if (
    requireNamespace(
      "pdftools",
      quietly = TRUE
    ) &&
      requireNamespace(
        "png",
        quietly = TRUE
      )
  ) {
    raw_return <- tryCatch(
      {
        bitmap <- pdftools::pdf_render_page(
          source,
          page = 1,
          dpi = dpi
        )

        png::writePNG(
          bitmap,
          destination
        )

        file_nonempty(
          destination
        )
      },
      error = function(e) {
        structure(
          FALSE,
          render_error = conditionMessage(e)
        )
      }
    )

    normalized <- canonicalize_figure_render_result(
      raw_return,
      fallback_path = destination,
      method_hint = "pdftools::pdf_render_page"
    )

    if (render_flag(
      normalized,
      "ok",
      FALSE
    )) {
      return(
        figure_render_result(
          ok = TRUE,
          path = destination,
          method = "pdftools::pdf_render_page",
          message = "",
          raw_return_class = render_chr(
            normalized,
            "raw_return_class",
            ""
          )
        )
      )
    }

    render_error <- attr(
      raw_return,
      "render_error"
    )

    if (!is.null(render_error)) {
      errors <- c(
        errors,
        paste0(
          "pdftools::pdf_render_page: ",
          render_error
        )
      )
    } else {
      remember_failure(
        normalized
      )
    }
  }

  source_norm <- normalizePath(
    source,
    winslash = "/",
    mustWork = TRUE
  )

  destination_norm <- normalizePath(
    destination,
    winslash = "/",
    mustWork = FALSE
  )

  destination_prefix <- sub(
    "\\.png$",
    "",
    destination_norm,
    ignore.case = TRUE,
    perl = TRUE
  )

  # Strategy 3: Poppler pdftoppm
  pdftoppm <- safe_chr(
    Sys.which(
      "pdftoppm"
    ),
    ""
  )

  if (nzchar(pdftoppm)) {
    result <- run_system_renderer(
      executable = pdftoppm,
      args = c(
        "-f",
        "1",
        "-singlefile",
        "-png",
        "-r",
        as.character(
          as.integer(dpi)
        ),
        shQuote(
          source_norm
        ),
        shQuote(
          destination_prefix
        )
      ),
      destination = destination,
      method = "pdftoppm"
    )

    if (render_flag(
      result,
      "ok",
      FALSE
    )) {
      return(
        result
      )
    }

    remember_failure(
      result
    )
  }

  # Strategy 4: Poppler pdftocairo
  pdftocairo <- safe_chr(
    Sys.which(
      "pdftocairo"
    ),
    ""
  )

  if (nzchar(pdftocairo)) {
    result <- run_system_renderer(
      executable = pdftocairo,
      args = c(
        "-f",
        "1",
        "-l",
        "1",
        "-singlefile",
        "-png",
        "-r",
        as.character(
          as.integer(dpi)
        ),
        shQuote(
          source_norm
        ),
        shQuote(
          destination_prefix
        )
      ),
      destination = destination,
      method = "pdftocairo"
    )

    if (render_flag(
      result,
      "ok",
      FALSE
    )) {
      return(
        result
      )
    }

    remember_failure(
      result
    )
  }

  # Strategy 5: Ghostscript, including standard Windows Program Files paths.
  ghostscript <- find_ghostscript_executable()

  if (nzchar(ghostscript)) {
    result <- run_system_renderer(
      executable = ghostscript,
      args = c(
        "-dSAFER",
        "-dBATCH",
        "-dNOPAUSE",
        "-dFirstPage=1",
        "-dLastPage=1",
        "-sDEVICE=pngalpha",
        paste0(
          "-r",
          as.integer(dpi)
        ),
        paste0(
          "-sOutputFile=",
          shQuote(
            destination_norm
          )
        ),
        shQuote(
          source_norm
        )
      ),
      destination = destination,
      method = "ghostscript"
    )

    if (render_flag(
      result,
      "ok",
      FALSE
    )) {
      return(
        result
      )
    }

    remember_failure(
      result
    )
  }

  # Strategy 6: MuPDF mutool
  mutool <- safe_chr(
    Sys.which(
      "mutool"
    ),
    ""
  )

  if (nzchar(mutool)) {
    result <- run_system_renderer(
      executable = mutool,
      args = c(
        "draw",
        "-F",
        "png",
        "-r",
        as.character(
          as.integer(dpi)
        ),
        "-o",
        shQuote(
          destination_norm
        ),
        shQuote(
          source_norm
        ),
        "1"
      ),
      destination = destination,
      method = "mutool"
    )

    if (render_flag(
      result,
      "ok",
      FALSE
    )) {
      return(
        result
      )
    }

    remember_failure(
      result
    )
  }

  # Strategy 7: ImageMagick command-line executable
  magick_cli <- safe_chr(
    Sys.which(
      "magick"
    ),
    ""
  )

  if (nzchar(magick_cli)) {
    result <- run_system_renderer(
      executable = magick_cli,
      args = c(
        "-density",
        as.character(
          as.integer(dpi)
        ),
        shQuote(
          paste0(
            source_norm,
            "[0]"
          )
        ),
        shQuote(
          destination_norm
        )
      ),
      destination = destination,
      method = "magick_cli"
    )

    if (render_flag(
      result,
      "ok",
      FALSE
    )) {
      return(
        result
      )
    }

    remember_failure(
      result
    )
  }

  # Strategy 8: R package magick
  if (
    requireNamespace(
      "magick",
      quietly = TRUE
    )
  ) {
    raw_return <- tryCatch(
      {
        images <- magick::image_read_pdf(
          source,
          density = dpi
        )

        if (length(images) < 1L) {
          stop(
            "No PDF page was rendered."
          )
        }

        magick::image_write(
          images[1],
          path = destination,
          format = "png"
        )

        file_nonempty(
          destination
        )
      },
      error = function(e) {
        structure(
          FALSE,
          render_error = conditionMessage(e)
        )
      }
    )

    normalized <- canonicalize_figure_render_result(
      raw_return,
      fallback_path = destination,
      method_hint = "magick::image_read_pdf"
    )

    if (render_flag(
      normalized,
      "ok",
      FALSE
    )) {
      return(
        figure_render_result(
          ok = TRUE,
          path = destination,
          method = "magick::image_read_pdf",
          message = "",
          raw_return_class = render_chr(
            normalized,
            "raw_return_class",
            ""
          )
        )
      )
    }

    render_error <- attr(
      raw_return,
      "render_error"
    )

    if (!is.null(render_error)) {
      errors <- c(
        errors,
        paste0(
          "magick::image_read_pdf: ",
          render_error
        )
      )
    } else {
      remember_failure(
        normalized
      )
    }
  }

  inventory <- system_renderer_inventory()

  availability_text <- paste(
    paste0(
      inventory$renderer,
      "=",
      ifelse(
        inventory$available,
        inventory$executable,
        "NOT_FOUND"
      )
    ),
    collapse = "; "
  )

  pdftools_available_final <- requireNamespace(
    "pdftools",
    quietly = TRUE
  )

  figure_render_result(
    ok = FALSE,
    path = "",
    method = "PDF_RENDER_FAILED",
    message = paste0(
      "PDF figure could not be rasterized after all universal fallbacks. ",
      "pdftools available after managed bootstrap=",
      pdftools_available_final,
      ". Renderer availability: ",
      availability_text,
      ". Dependency audit: ",
      normalize_slash(
        figure_render_dependency_audit_file
      ),
      ". ",
      if (length(errors) > 0L) {
        paste0(
          "Attempts: ",
          paste(
            unique(errors),
            collapse = " || "
          )
        )
      } else {
        "No renderer produced a usable image."
      }
    ),
    raw_return_class = ""
  )
}

render_svg_to_png <- function(
    source,
    destination
) {
  errors <- character(0)

  if (
    requireNamespace(
      "rsvg",
      quietly = TRUE
    )
  ) {
    raw_return <- tryCatch(
      {
        rsvg::rsvg_png(
          source,
          file = destination
        )

        file_nonempty(
          destination
        )
      },
      error = function(e) {
        errors <<- c(
          errors,
          paste0(
            "rsvg::rsvg_png: ",
            conditionMessage(e)
          )
        )

        FALSE
      }
    )

    normalized <- canonicalize_figure_render_result(
      raw_return,
      fallback_path = destination,
      method_hint = "rsvg::rsvg_png"
    )

    if (
      isTRUE(
        render_flag(normalized, "ok", FALSE)
      )
    ) {
      return(
        figure_render_result(
          ok = TRUE,
          path = destination,
          method = "rsvg::rsvg_png",
          message = "",
          raw_return_class = render_chr(normalized, "raw_return_class", "")
        )
      )
    }
  }

  if (
    requireNamespace(
      "magick",
      quietly = TRUE
    )
  ) {
    raw_return <- tryCatch(
      {
        image <- magick::image_read(
          source
        )

        magick::image_write(
          image[1],
          path = destination,
          format = "png"
        )

        file_nonempty(
          destination
        )
      },
      error = function(e) {
        errors <<- c(
          errors,
          paste0(
            "magick::image_read SVG: ",
            conditionMessage(e)
          )
        )

        FALSE
      }
    )

    normalized <- canonicalize_figure_render_result(
      raw_return,
      fallback_path = destination,
      method_hint = "magick::image_read SVG"
    )

    if (
      isTRUE(
        render_flag(normalized, "ok", FALSE)
      )
    ) {
      return(
        figure_render_result(
          ok = TRUE,
          path = destination,
          method = "magick::image_read SVG",
          message = "",
          raw_return_class = render_chr(normalized, "raw_return_class", "")
        )
      )
    }
  }

  figure_render_result(
    ok = FALSE,
    path = "",
    method = "SVG_RENDER_FAILED",
    message = paste0(
      "SVG figure could not be converted to PNG. ",
      "Install R package 'rsvg' or 'magick', or provide PNG/JPEG. ",
      if (length(errors) > 0L) {
        paste0(
          "Renderer details: ",
          paste(
            unique(errors),
            collapse = " || "
          )
        )
      } else {
        ""
      }
    )
  )
}

render_other_image_to_png <- function(
    source,
    destination
) {
  if (
    !requireNamespace(
      "magick",
      quietly = TRUE
    )
  ) {
    return(
      figure_render_result(
        ok = FALSE,
        path = "",
        method = "MAGICK_NOT_AVAILABLE",
        message = paste0(
          "Figure format '",
          normalize_figure_extension(source),
          "' requires conversion before Word insertion. ",
          "Install R package 'magick' or provide PNG/JPEG."
        )
      )
    )
  }

  raw_return <- tryCatch(
    {
      image <- magick::image_read(
        source
      )

      magick::image_write(
        image[1],
        path = destination,
        format = "png"
      )

      file_nonempty(
        destination
      )
    },
    error = function(e) {
      structure(
        FALSE,
        render_error = conditionMessage(e)
      )
    }
  )

  normalized <- canonicalize_figure_render_result(
    raw_return,
    fallback_path = destination,
    method_hint = "magick::image_read/image_write"
  )

  if (
    isTRUE(
      render_flag(normalized, "ok", FALSE)
    )
  ) {
    return(
      figure_render_result(
        ok = TRUE,
        path = destination,
        method = "magick::image_read/image_write",
        message = "",
        raw_return_class = render_chr(normalized, "raw_return_class", "")
      )
    )
  }

  render_error <- attr(
    raw_return,
    "render_error"
  )

  figure_render_result(
    ok = FALSE,
    path = "",
    method = "magick::image_read/image_write",
    message = if (
      !is.null(render_error) &&
        nzchar(
          safe_chr(
            render_error,
            ""
          )
        )
    ) {
      paste0(
        "Figure conversion failed: ",
        render_error
      )
    } else {
      safe_chr(
        render_chr(normalized, "message", ""),
        "Figure conversion failed."
      )
    },
    raw_return_class = render_chr(normalized, "raw_return_class", "")
  )
}

prepare_figure_for_word <- function(
    source,
    render_root,
    artifact_id,
    display_number,
    dpi = 300,
    max_width = 6.2,
    max_height = 8.0,
    semantic_candidates = character(0),
    project_root = ""
) {
  if (!file_nonempty(source)) {
    return(
      figure_render_result(
        ok = FALSE,
        path = "",
        method = "SOURCE_VALIDATION",
        width = NA_real_,
        height = NA_real_,
        message = "Canonical figure source is missing or empty."
      )
    )
  }

  ext <- normalize_figure_extension(
    source
  )

  render_name <- paste0(
    "Figure_",
    sprintf(
      "%03d",
      as.integer(
        display_number
      )
    ),
    "_",
    clean_file_component(
      artifact_id
    ),
    ".png"
  )

  destination <- file.path(
    render_root,
    render_name
  )

  # Prefer an existing safe raster rendering before invoking any PDF/SVG
  # converter. This is lossless with respect to the already-generated
  # publication artifact and avoids unnecessary environment dependencies.
  raster_companion <- find_raster_companion(
    source = source,
    semantic_candidates = semantic_candidates,
    project_root = project_root
  )

  companion_ok <- render_flag(
    raster_companion,
    "ok",
    FALSE
  )

  companion_path <- render_chr(
    raster_companion,
    "path",
    ""
  )

  companion_method <- render_chr(
    raster_companion,
    "method",
    ""
  )

  raw_prepared <- tryCatch(
    {
      if (
        figure_is_direct_word_image(
          source
        )
      ) {
        figure_render_result(
          ok = TRUE,
          path = source,
          method = "DIRECT_RASTER",
          message = "",
          raw_return_class = "internal_list"
        )
      } else if (
        companion_ok &&
          file_nonempty(
            companion_path
          )
      ) {
        figure_render_result(
          ok = TRUE,
          path = companion_path,
          method = companion_method,
          message = "",
          raw_return_class = "internal_raster_companion"
        )
      } else if (identical(ext, "pdf")) {
        render_pdf_first_page(
          source,
          destination,
          dpi = dpi
        )
      } else if (identical(ext, "svg")) {
        render_svg_to_png(
          source,
          destination
        )
      } else if (
        ext %in% c(
          "tif",
          "tiff",
          "bmp",
          "gif",
          "webp"
        )
      ) {
        render_other_image_to_png(
          source,
          destination
        )
      } else {
        figure_render_result(
          ok = FALSE,
          path = "",
          method = "UNSUPPORTED_FORMAT",
          message = paste0(
            "Unsupported figure format for automatic Word insertion: .",
            ext,
            ". Provide PNG/JPEG, PDF, SVG, TIFF, BMP, GIF, or WEBP."
          )
        )
      }
    },
    error = function(e) {
      figure_render_result(
        ok = FALSE,
        path = "",
        method = "RENDER_DISPATCH_ERROR",
        message = paste0(
          "Unexpected figure-render dispatch error: ",
          conditionMessage(e)
        ),
        raw_return_class = paste(
          class(e),
          collapse = "/"
        )
      )
    }
  )

  prepared <- canonicalize_figure_render_result(
    raw_prepared,
    fallback_path = if (
      figure_is_direct_word_image(
        source
      )
    ) {
      source
    } else {
      destination
    },
    method_hint = paste0(
      "PREPARE_",
      toupper(
        ext
      )
    )
  )

  prepared_ok <- render_flag(
    prepared,
    "ok",
    FALSE
  )

  prepared_path <- render_chr(
    prepared,
    "path",
    ""
  )

  prepared_method <- render_chr(
    prepared,
    "method",
    ""
  )

  prepared_message <- render_chr(
    prepared,
    "message",
    ""
  )

  prepared_raw_class <- render_chr(
    prepared,
    "raw_return_class",
    ""
  )

  if (
    !prepared_ok ||
      !file_nonempty(
        prepared_path
      )
  ) {
    return(
      figure_render_result(
        ok = FALSE,
        path = "",
        method = prepared_method,
        width = NA_real_,
        height = NA_real_,
        message = if (nzchar(prepared_message)) {
          prepared_message
        } else {
          "Figure could not be prepared for Word insertion."
        },
        raw_return_class = prepared_raw_class
      )
    )
  }

  dimensions <- fit_word_image_dimensions(
    prepared_path,
    max_width = max_width,
    max_height = max_height
  )

  width_in <- suppressWarnings(
    as.numeric(
      dimensions[["width"]]
    )
  )[1]

  height_in <- suppressWarnings(
    as.numeric(
      dimensions[["height"]]
    )
  )[1]

  if (
    is.na(width_in) ||
      is.na(height_in) ||
      width_in <= 0 ||
      height_in <= 0
  ) {
    return(
      figure_render_result(
        ok = FALSE,
        path = "",
        method = prepared_method,
        width = NA_real_,
        height = NA_real_,
        message = "Prepared image exists but Word image dimensions could not be resolved.",
        raw_return_class = prepared_raw_class
      )
    )
  }

  figure_render_result(
    ok = TRUE,
    path = prepared_path,
    method = prepared_method,
    width = width_in,
    height = height_in,
    message = safe_chr(
      prepared_message,
      ""
    ),
    raw_return_class = prepared_raw_class
  )
}


copy_artifact <- function(
    source,
    destination_dir,
    artifact_id
) {
  if (!file_nonempty(source)) {
    return("")
  }

  dir.create(
    destination_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  base <- clean_file_component(
    basename(
      source
    )
  )

  destination <- file.path(
    destination_dir,
    paste0(
      clean_file_component(
        artifact_id
      ),
      "_",
      base
    )
  )

  ok <- file.copy(
    from = source,
    to = destination,
    overwrite = TRUE,
    copy.mode = TRUE
  )

  if (!isTRUE(ok)) {
    return("")
  }

  destination
}


# ============================================================
# 8. Create journal-neutral Word manuscript
# ============================================================

doc <- officer::read_docx()

# ------------------------------------------------------------
# Universal Word-style discovery
# ------------------------------------------------------------
# DOCX default style sets vary by Microsoft Word version, locale,
# LibreOffice, institutional templates, and user-customized Normal.dotm.
# R/41 must never assume that a style such as "Title" exists.

available_paragraph_styles <- function(doc) {
  info <- tryCatch(
    officer::styles_info(doc),
    error = function(e) NULL
  )

  if (
    is.null(info) ||
      nrow(info) == 0L
  ) {
    return(
      "Normal"
    )
  }

  if (
    "style_type" %in% names(info)
  ) {
    paragraph_rows <- grepl(
      "paragraph",
      tolower(
        as.character(
          info$style_type
        )
      ),
      fixed = TRUE
    )

    if (any(paragraph_rows)) {
      info <- info[
        paragraph_rows,
        ,
        drop = FALSE
      ]
    }
  }

  name_col <- first_existing_col(
    info,
    c(
      "style_name",
      "style_id"
    )
  )

  if (is.na(name_col)) {
    return(
      "Normal"
    )
  }

  styles <- unique(
    trimws(
      as.character(
        info[[name_col]]
      )
    )
  )

  styles <- styles[
    !is.na(styles) &
      nzchar(styles)
  ]

  if (length(styles) == 0L) {
    return(
      "Normal"
    )
  }

  styles
}

pick_paragraph_style <- function(
    available_styles,
    candidates
) {
  available_styles <- as.character(
    available_styles
  )

  if (length(available_styles) == 0L) {
    return(
      NULL
    )
  }

  normalize_style_name <- function(x) {
    x <- tolower(
      trimws(
        as.character(x)
      )
    )

    gsub(
      "[[:space:]_-]+",
      "",
      x,
      perl = TRUE
    )
  }

  available_normalized <- normalize_style_name(
    available_styles
  )

  for (candidate in candidates) {
    candidate_normalized <- normalize_style_name(
      candidate
    )

    hit <- which(
      available_normalized ==
        candidate_normalized
    )

    if (length(hit) > 0L) {
      return(
        available_styles[
          hit[[1]]
        ]
      )
    }
  }

  # Final fallback is the actual Normal style if present.
  normal_hit <- which(
    available_normalized ==
      normalize_style_name(
        "Normal"
      )
  )

  if (length(normal_hit) > 0L) {
    return(
      available_styles[
        normal_hit[[1]]
      ]
    )
  }

  # As a last resort, use the first available paragraph style.
  available_styles[[1]]
}

available_styles <- available_paragraph_styles(
  doc
)

style_map <- list(
  title = pick_paragraph_style(
    available_styles,
    c(
      "Title",
      "graphic title",
      "centered",
      "heading 1",
      "Normal"
    )
  ),
  heading1 = pick_paragraph_style(
    available_styles,
    c(
      "heading 1",
      "Heading 1",
      "Title",
      "graphic title",
      "Normal"
    )
  ),
  heading2 = pick_paragraph_style(
    available_styles,
    c(
      "heading 2",
      "Heading 2",
      "heading 1",
      "Normal"
    )
  ),
  heading3 = pick_paragraph_style(
    available_styles,
    c(
      "heading 3",
      "Heading 3",
      "heading 2",
      "heading 1",
      "Normal"
    )
  ),
  normal = pick_paragraph_style(
    available_styles,
    c(
      "Normal",
      "Body Text",
      "Text Body"
    )
  )
)

required_style_keys <- c(
  "title",
  "heading1",
  "heading2",
  "heading3",
  "normal"
)

missing_style_keys <- required_style_keys[
  !vapply(
    style_map[
      required_style_keys
    ],
    function(x) {
      !is.null(x) &&
        length(x) == 1L &&
        !is.na(x) &&
        nzchar(x)
    },
    logical(1)
  )
]

if (length(missing_style_keys) > 0L) {
  stop(
    paste0(
      "R/41 could not resolve required paragraph style fallback(s): ",
      paste(
        missing_style_keys,
        collapse = ", "
      ),
      ". Available paragraph styles: ",
      paste(
        available_styles,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

style_resolution_self_test <- all(
  unlist(
    style_map[
      required_style_keys
    ],
    use.names = FALSE
  ) %in% available_styles
)

if (!isTRUE(style_resolution_self_test)) {
  stop(
    "Internal R/41 Word-style resolution self-test failed.",
    call. = FALSE
  )
}

add_manuscript_block <- function(
    doc,
    block
) {
  type <- block$type
  text <- safe_chr(
    block$text,
    ""
  )

  if (!nzchar(text)) {
    return(doc)
  }

  if (identical(type, "heading")) {
    level <- block$level

    if (
      !is.na(level) &&
        level <= 1L
    ) {
      doc <- officer::body_add_par(
        doc,
        value = text,
        style = style_map$title
      )
      return(doc)
    }

    if (
      !is.na(level) &&
        level == 2L
    ) {
      doc <- officer::body_add_par(
        doc,
        value = text,
        style = style_map$heading1
      )
      return(doc)
    }

    if (
      !is.na(level) &&
        level == 3L
    ) {
      doc <- officer::body_add_par(
        doc,
        value = text,
        style = style_map$heading2
      )
      return(doc)
    }

    doc <- officer::body_add_par(
      doc,
      value = text,
      style = style_map$heading3
    )

    return(doc)
  }

  if (identical(type, "bullet")) {
    doc <- officer::body_add_par(
      doc,
      value = paste0(
        "\u2022 ",
        text
      ),
      style = style_map$normal
    )
    return(doc)
  }

  if (identical(type, "quote")) {
    doc <- officer::body_add_par(
      doc,
      value = text,
      style = style_map$normal
    )
    return(doc)
  }

  # Preserve explicit numbering exactly as written. This is especially
  # important for the verified reference list.
  doc <- officer::body_add_par(
    doc,
    value = text,
    style = style_map$normal
  )

  doc
}

for (block in markdown_blocks) {
  doc <- add_manuscript_block(
    doc,
    block
  )
}

manifest_rows <- list()

manifest_rows[[
  length(manifest_rows) + 1L
]] <- tibble::tibble(
  item_type = "manuscript_source",
  item_id = "R40_PASS_MANUSCRIPT",
  title = project_title,
  source_path = normalize_slash(
    submission_candidate_file
  ),
  source_sha256 = sha256_file(
    submission_candidate_file
  ),
  included_in_docx = TRUE,
  copied_path = "",
  copied_sha256 = "",
  note = "Exact PASS-only R/40 manuscript source; scientific rewriting by R/41 = NO."
)


# ============================================================
# 9. Append editable tables
# ============================================================

table_artifacts <- if (
  nrow(publication_artifacts) > 0L
) {
  publication_artifacts |>
    dplyr::filter(
      artifact_type == "table"
    )
} else {
  tibble::tibble()
}

if (
  include_tables &&
    nrow(table_artifacts) > 0L
) {
  doc <- officer::body_add_break(
    doc
  )

  doc <- officer::body_add_par(
    doc,
    value = "Tables",
    style = style_map$heading1
  )

  for (i in seq_len(nrow(table_artifacts))) {
    row <- table_artifacts[
      i,
      ,
      drop = FALSE
    ]

    source <- row$resolved_source_path[[1]]

    data <- tryCatch(
      read_table_artifact(
        source
      ),
      error = function(e) NULL
    )

    copied <- if (copy_artifact_files) {
      copy_artifact(
        source = source,
        destination_dir = tables_root,
        artifact_id = row$artifact_id[[1]]
      )
    } else {
      ""
    }

    if (
      is.null(data) ||
        (
          nrow(data) == 0L &&
            ncol(data) == 0L
        )
    ) {
      manifest_rows[[
        length(manifest_rows) + 1L
      ]] <- tibble::tibble(
        item_type = "table",
        item_id = row$artifact_id[[1]],
        title = row$title[[1]],
        source_path = normalize_slash(
          source
        ),
        source_sha256 = sha256_file(
          source
        ),
        included_in_docx = FALSE,
        copied_path = normalize_slash(
          copied
        ),
        copied_sha256 = sha256_file(
          copied
        ),
        note = "Table source was available but could not be converted to an editable Word table."
      )

      next
    }

    table_number <- as.integer(
      row$display_number[[1]]
    )

    if (
      is.na(table_number) ||
        table_number < 1L
    ) {
      stop(
        "R/41 internal table numbering contract failed.",
        call. = FALSE
      )
    }

    clean_title <- strip_existing_display_number(
      row$title[[1]],
      "table"
    )

    caption <- if (nzchar(clean_title)) {
      paste0(
        "Table ",
        table_number,
        ". ",
        clean_title
      )
    } else {
      paste0(
        "Table ",
        table_number
      )
    }

    doc <- officer::body_add_par(
      doc,
      value = caption,
      style = style_map$heading2
    )

    ft <- make_word_table(
      data,
      font_size = table_font_size
    )

    doc <- flextable::body_add_flextable(
      doc,
      value = ft,
      align = "center"
    )

    doc <- officer::body_add_par(
      doc,
      value = "",
      style = style_map$normal
    )

    manifest_rows[[
      length(manifest_rows) + 1L
    ]] <- tibble::tibble(
      item_type = "table",
      item_id = row$artifact_id[[1]],
      title = caption,
      source_path = normalize_slash(
        source
      ),
      source_sha256 = sha256_file(
        source
      ),
      included_in_docx = TRUE,
      copied_path = normalize_slash(
        copied
      ),
      copied_sha256 = sha256_file(
        copied
      ),
      note = paste0(
        "Editable Word table generated from canonical artifact; rows=",
        nrow(data),
        "; columns=",
        ncol(data),
        "."
      )
    )
  }
}


# ============================================================
# 10. Figure preflight, legends, and actual figures at document end
# ============================================================

figure_artifacts <- if (
  nrow(publication_artifacts) > 0L
) {
  publication_artifacts |>
    dplyr::filter(
      artifact_type == "figure"
    ) |>
    dplyr::arrange(
      display_number
    )
} else {
  tibble::tibble()
}


if (
  available_registry_figure_count > 0L &&
    nrow(figure_artifacts) == 0L
) {
  stop(
    paste0(
      "R/41 final figure set is empty despite ",
      available_registry_figure_count,
      " usable registry figure file(s). ",
      "Figure Legends/Figures sections will not be silently omitted. See: ",
      normalize_slash(
        figure_discovery_audit_file
      )
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------
# 10.1 Preflight every final numbered figure BEFORE DOCX is written
# ------------------------------------------------------------

pdf_renderer_inventory <- system_renderer_inventory()

readr::write_csv(
  pdf_renderer_inventory,
  pdf_renderer_inventory_file,
  na = ""
)

write_figure_dependency_audit()

figure_embedding_rows <- list()

if (
  append_figures_at_end &&
    nrow(figure_artifacts) > 0L
) {
  for (i in seq_len(nrow(figure_artifacts))) {
    row <- figure_artifacts[
      i,
      ,
      drop = FALSE
    ]

    source <- row$resolved_source_path[[1]]

    figure_number <- as.integer(
      row$display_number[[1]]
    )

    if (
      is.na(figure_number) ||
        figure_number < 1L
    ) {
      stop(
        "R/41 internal figure numbering contract failed before figure preflight.",
        call. = FALSE
      )
    }

    semantic_key_value <- safe_chr(
      row$semantic_key[[1]],
      ""
    )

    semantic_companion_paths <- if (
      nrow(figure_rendering_inventory) > 0L &&
        nzchar(
          semantic_key_value
        )
    ) {
      figure_rendering_inventory |>
        dplyr::filter(
          semantic_key ==
            semantic_key_value
        ) |>
        dplyr::arrange(
          word_render_rank,
          narrative_order,
          priority_rank,
          role_rank,
          title,
          artifact_id
        ) |>
        dplyr::pull(
          resolved_source_path
        )
    } else {
      character(0)
    }

    raw_prepared <- prepare_figure_for_word(
      source = source,
      render_root = figure_render_root,
      artifact_id = row$artifact_id[[1]],
      display_number = figure_number,
      dpi = figure_dpi,
      max_width = figure_max_width_in,
      max_height = figure_max_height_in,
      semantic_candidates = semantic_companion_paths,
      project_root = repo_root
    )

    prepared <- canonicalize_figure_render_result(
      raw_prepared,
      fallback_path = "",
      method_hint = "PREPARE_FIGURE_FOR_WORD"
    )

    prepared_ok <- render_flag(
      prepared,
      "ok",
      FALSE
    )

    prepared_path <- render_chr(
      prepared,
      "path",
      ""
    )

    prepared_method <- render_chr(
      prepared,
      "method",
      ""
    )

    prepared_raw_class <- render_chr(
      prepared,
      "raw_return_class",
      ""
    )

    prepared_width <- render_num(
      prepared,
      "width",
      NA_real_
    )

    prepared_height <- render_num(
      prepared,
      "height",
      NA_real_
    )

    prepared_message <- render_chr(
      prepared,
      "message",
      ""
    )

    figure_embedding_rows[[
      length(figure_embedding_rows) + 1L
    ]] <- tibble::tibble(
      artifact_id = row$artifact_id[[1]],
      display_number = figure_number,
      display_label = row$display_label[[1]],
      title = row$title[[1]],
      source_path = normalize_slash(
        source
      ),
      source_extension = normalize_figure_extension(
        source
      ),
      source_sha256 = sha256_file(
        source
      ),
      prepared = prepared_ok,
      word_image_path = normalize_slash(
        prepared_path
      ),
      word_image_sha256 = sha256_file(
        prepared_path
      ),
      render_method = safe_chr(
        prepared_method,
        ""
      ),
      raw_return_class = safe_chr(
        prepared_raw_class,
        ""
      ),
      width_in = suppressWarnings(
        prepared_width
      ),
      height_in = suppressWarnings(
        prepared_height
      ),
      message = safe_chr(
        prepared_message,
        ""
      )
    )
  }
}

figure_embedding_audit <- if (
  length(figure_embedding_rows) > 0L
) {
  dplyr::bind_rows(
    figure_embedding_rows
  ) |>
    dplyr::arrange(
      display_number
    )
} else {
  tibble::tibble(
    artifact_id = character(0),
    display_number = integer(0),
    display_label = character(0),
    title = character(0),
    source_path = character(0),
    source_extension = character(0),
    source_sha256 = character(0),
    prepared = logical(0),
    word_image_path = character(0),
    word_image_sha256 = character(0),
    render_method = character(0),
    raw_return_class = character(0),
    width_in = numeric(0),
    height_in = numeric(0),
    message = character(0)
  )
}

readr::write_csv(
  figure_embedding_audit,
  figure_embedding_audit_file,
  na = ""
)

if (
  append_figures_at_end &&
    nrow(figure_artifacts) > 0L
) {
  if (
    nrow(figure_embedding_audit) !=
      nrow(figure_artifacts)
  ) {
    stop(
      "R/41 figure preflight audit is incomplete.",
      call. = FALSE
    )
  }

  failed_preflight <- which(
    !figure_embedding_audit$prepared |
      !nzchar(
        figure_embedding_audit$word_image_path
      ) |
      !vapply(
        figure_embedding_audit$word_image_path,
        file_nonempty,
        logical(1)
      )
  )

  if (length(failed_preflight) > 0L) {
    details <- vapply(
      failed_preflight,
      function(index) {
        paste0(
          figure_embedding_audit$display_label[[index]],
          " [",
          basename(
            figure_embedding_audit$source_path[[index]]
          ),
          "]: ",
          safe_chr(
            figure_embedding_audit$message[[index]],
            "Could not prepare image."
          )
        )
      },
      character(1)
    )

    stop(
      paste0(
        "R/41 cannot append all actual figures to the Word manuscript. ",
        paste(
          details,
          collapse = " || "
        ),
        " See: ",
        normalize_slash(
          figure_embedding_audit_file
        )
      ),
      call. = FALSE
    )
  }

  expected_numbers <- seq_len(
    nrow(
      figure_embedding_audit
    )
  )

  if (
    !identical(
      as.integer(
        figure_embedding_audit$display_number
      ),
      as.integer(
        expected_numbers
      )
    )
  ) {
    stop(
      "R/41 figure embedding order does not match first-mention Figure numbering.",
      call. = FALSE
    )
  }
}

# ------------------------------------------------------------
# 10.2 Figure Legends
# ------------------------------------------------------------

if (
  include_figure_legends &&
    nrow(figure_artifacts) > 0L
) {
  doc <- officer::body_add_break(
    doc
  )

  doc <- officer::body_add_par(
    doc,
    value = "Figure Legends",
    style = style_map$heading1
  )

  for (i in seq_len(nrow(figure_artifacts))) {
    row <- figure_artifacts[
      i,
      ,
      drop = FALSE
    ]

    figure_number <- as.integer(
      row$display_number[[1]]
    )

    clean_title <- strip_existing_display_number(
      row$title[[1]],
      "figure"
    )

    legend <- if (nzchar(clean_title)) {
      paste0(
        "Figure ",
        figure_number,
        ". ",
        clean_title
      )
    } else {
      paste0(
        "Figure ",
        figure_number
      )
    }

    doc <- officer::body_add_par(
      doc,
      value = legend,
      style = style_map$normal
    )
  }
}

# ------------------------------------------------------------
# 10.3 Actual numbered figures at the END of the manuscript
# ------------------------------------------------------------

if (
  append_figures_at_end &&
    nrow(figure_artifacts) > 0L
) {
  doc <- officer::body_add_break(
    doc
  )

  doc <- officer::body_add_par(
    doc,
    value = "Figures",
    style = style_map$heading1
  )

  for (i in seq_len(nrow(figure_artifacts))) {
    row <- figure_artifacts[
      i,
      ,
      drop = FALSE
    ]

    audit_row <- figure_embedding_audit |>
      dplyr::filter(
        artifact_id ==
          row$artifact_id[[1]]
      )

    if (nrow(audit_row) != 1L) {
      stop(
        paste0(
          "R/41 figure embedding audit could not resolve exactly one row for artifact ",
          row$artifact_id[[1]],
          "."
        ),
        call. = FALSE
      )
    }

    figure_number <- as.integer(
      row$display_number[[1]]
    )

    if (
      !identical(
        figure_number,
        as.integer(
          audit_row$display_number[[1]]
        )
      )
    ) {
      stop(
        paste0(
          "R/41 Figure number identity failure for artifact ",
          row$artifact_id[[1]],
          "."
        ),
        call. = FALSE
      )
    }

    clean_title <- strip_existing_display_number(
      row$title[[1]],
      "figure"
    )

    figure_heading <- if (
      repeat_figure_legend_with_image &&
        nzchar(clean_title)
    ) {
      paste0(
        "Figure ",
        figure_number,
        ". ",
        clean_title
      )
    } else {
      paste0(
        "Figure ",
        figure_number
      )
    }

    if (i > 1L) {
      doc <- officer::body_add_break(
        doc
      )
    }

    doc <- officer::body_add_par(
      doc,
      value = figure_heading,
      style = style_map$normal
    )

    image_path <- audit_row$word_image_path[[1]]
    image_width <- as.numeric(
      audit_row$width_in[[1]]
    )
    image_height <- as.numeric(
      audit_row$height_in[[1]]
    )

    if (
      !file_nonempty(image_path) ||
        is.na(image_width) ||
        is.na(image_height) ||
        image_width <= 0 ||
        image_height <= 0
    ) {
      stop(
        paste0(
          "R/41 prepared image validation failed for Figure ",
          figure_number,
          "."
        ),
        call. = FALSE
      )
    }

    doc <- tryCatch(
      {
        officer::body_add_img(
          doc,
          src = image_path,
          width = image_width,
          height = image_height
        )
      },
      error = function(e) {
        stop(
          paste0(
            "R/41 could not insert Figure ",
            figure_number,
            " into the Word document: ",
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )

    doc <- officer::body_add_par(
      doc,
      value = "",
      style = style_map$normal
    )

    source <- row$resolved_source_path[[1]]

    copied <- if (copy_artifact_files) {
      copy_artifact(
        source = source,
        destination_dir = figures_root,
        artifact_id = row$artifact_id[[1]]
      )
    } else {
      ""
    }

    manifest_rows[[
      length(manifest_rows) + 1L
    ]] <- tibble::tibble(
      item_type = "figure",
      item_id = row$artifact_id[[1]],
      title = figure_heading,
      source_path = normalize_slash(
        source
      ),
      source_sha256 = sha256_file(
        source
      ),
      included_in_docx = TRUE,
      copied_path = normalize_slash(
        copied
      ),
      copied_sha256 = sha256_file(
        copied
      ),
      display_number = figure_number,
      display_label = paste(
        "Figure",
        figure_number
      ),
      word_image_path = normalize_slash(
        image_path
      ),
      word_image_sha256 = sha256_file(
        image_path
      ),
      render_method = audit_row$render_method[[1]],
      note = paste0(
        "Actual figure image appended at manuscript end; ",
        "body/legend/image number identity verified."
      )
    )
  }
} else if (
  nrow(figure_artifacts) > 0L
) {
  # Preserve a figure manifest even when the optional actual-image append
  # feature is explicitly disabled.
  for (i in seq_len(nrow(figure_artifacts))) {
    row <- figure_artifacts[
      i,
      ,
      drop = FALSE
    ]

    source <- row$resolved_source_path[[1]]

    copied <- if (copy_artifact_files) {
      copy_artifact(
        source = source,
        destination_dir = figures_root,
        artifact_id = row$artifact_id[[1]]
      )
    } else {
      ""
    }

    manifest_rows[[
      length(manifest_rows) + 1L
    ]] <- tibble::tibble(
      item_type = "figure",
      item_id = row$artifact_id[[1]],
      title = paste(
        "Figure",
        row$display_number[[1]]
      ),
      source_path = normalize_slash(
        source
      ),
      source_sha256 = sha256_file(
        source
      ),
      included_in_docx = FALSE,
      copied_path = normalize_slash(
        copied
      ),
      copied_sha256 = sha256_file(
        copied
      ),
      display_number = as.integer(
        row$display_number[[1]]
      ),
      display_label = row$display_label[[1]],
      word_image_path = "",
      word_image_sha256 = "",
      render_method = "",
      note = "Actual figure append explicitly disabled by configuration."
    )
  }
}


# ============================================================
# 11. Write Word file
# ============================================================

print(
  doc,
  target = docx_file
)

if (!file_nonempty(docx_file)) {
  stop(
    paste0(
      "R/41 failed to create a non-empty DOCX file: ",
      normalize_slash(
        docx_file
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 12. Export manifest
# ============================================================

export_manifest <- dplyr::bind_rows(
  manifest_rows
)

readr::write_csv(
  export_manifest,
  manifest_file,
  na = ""
)


# ============================================================
# 13. Deterministic export checks
# ============================================================

docx_sha256 <- sha256_file(
  docx_file
)

candidate_sha256 <- sha256_file(
  submission_candidate_file
)

manifest_missing_sources <- if (
  nrow(export_manifest) > 0L
) {
  export_manifest |>
    dplyr::filter(
      item_type %in% c(
        "table",
        "figure"
      ),
      !nzchar(
        source_sha256
      )
    )
} else {
  tibble::tibble()
}

figure_manifest_validation <- TRUE

if (
  append_figures_at_end &&
    nrow(figure_artifacts) > 0L
) {
  embedded_figure_manifest <- export_manifest |>
    dplyr::filter(
      item_type == "figure",
      included_in_docx
    ) |>
    dplyr::arrange(
      display_number
    )

  figure_manifest_validation <- (
    nrow(embedded_figure_manifest) ==
      nrow(figure_artifacts) &&
      identical(
        as.integer(
          embedded_figure_manifest$display_number
        ),
        as.integer(
          seq_len(
            nrow(
              figure_artifacts
            )
          )
        )
      ) &&
      all(
        vapply(
          embedded_figure_manifest$word_image_path,
          file_nonempty,
          logical(1)
        )
      )
  )
}

export_pass <- (
  file_nonempty(docx_file) &&
    nzchar(candidate_sha256) &&
    nzchar(docx_sha256) &&
    nrow(manifest_missing_sources) == 0L &&
    isTRUE(
      figure_manifest_validation
    )
)

if (!export_pass) {
  stop(
    paste0(
      "R/41 export validation failed. ",
      "DOCX/source hashes or artifact source validation did not pass."
    ),
    call. = FALSE
  )
}


# ============================================================
# 14. State
# ============================================================

state <- list(
  manuscript_docx_export = list(
    version = "1.11.0",
    generated_at = now_text(),
    project_id = project_id,
    project_title = project_title,
    upstream_r40_status = qc_status,
    journal_neutral = TRUE,
    author_editable = TRUE,
    scientific_rewriting = FALSE,
    statistical_recalculation = FALSE,
    external_api_calls = FALSE,
    resolved_word_styles = style_map,
    include_tables = include_tables,
    include_figure_legends = include_figure_legends,
    append_figures_at_end = append_figures_at_end,
    repeat_figure_legend_with_image = repeat_figure_legend_with_image,
    figure_dpi = figure_dpi,
    figure_max_width_in = figure_max_width_in,
    figure_max_height_in = figure_max_height_in,
    copy_artifact_files = copy_artifact_files,
    source_manuscript = normalize_slash(
      submission_candidate_file
    ),
    source_manuscript_sha256 = candidate_sha256,
    output_docx = normalize_slash(
      docx_file
    ),
    output_docx_sha256 = docx_sha256,
    cross_reference_map = normalize_slash(
      crossref_file
    ),
    cross_reference_count = nrow(
      crossref_map
    ),
    numbering_contract = "FIRST_MENTION_IN_FINAL_MANUSCRIPT_BODY",
    semantic_mapping_contract = "ANCHOR_TEXT_THEN_RESULTS_PROSE_THEN_HEADING_FALLBACK",
    display_label_validation = "CASE_INSENSITIVE_BOUNDARY_AWARE_LABEL_MATCH_IN_MAPPED_BODY_BLOCK",
    internal_self_tests_enabled = run_internal_self_tests,
    table_artifacts_in_registry = nrow(
      table_artifacts
    ),
    available_registry_figure_files = available_registry_figure_count,
    final_semantic_figure_count = nrow(
      figure_artifacts
    ),
    figure_discovery_audit = normalize_slash(
      figure_discovery_audit_file
    ),
    pdf_renderer_inventory = normalize_slash(
      pdf_renderer_inventory_file
    ),
    figure_render_dependency_audit = normalize_slash(
      figure_render_dependency_audit_file
    ),
    figure_runtime_library = normalize_slash(
      figure_runtime_library
    ),
    auto_install_figure_render_dependencies = auto_install_figure_render_dependencies,
    figure_artifacts_in_registry = nrow(
      figure_artifacts
    ),
    actual_figures_embedded = if (
      append_figures_at_end
    ) {
      nrow(
        figure_embedding_audit
      )
    } else {
      0L
    },
    figure_embedding_audit = normalize_slash(
      figure_embedding_audit_file
    ),
    figure_render_contract = "SAFE_ACCESSOR_ONLY_NO_DIRECT_NAMED_SUBSCRIPT_AT_RENDER_BOUNDARIES",
    figure_render_directory = normalize_slash(
      figure_render_root
    ),
    export_status = "PASS_AUTHOR_REVIEW_READY"
  )
)

yaml::write_yaml(
  state,
  state_file
)


# ============================================================
# 15. Journal-neutral author review README
# ============================================================

readme_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "Journal-neutral author-review package",
  "",
  paste0(
    "Project: ",
    project_title
  ),
  "",
  "PRIMARY AUTHOR FILE",
  "--------------------------------------------------",
  paste0(
    "Editable Word manuscript: ",
    normalize_slash(
      docx_file
    )
  ),
  "",
  "WHAT R/41 DID",
  "--------------------------------------------------",
  "- Exported the exact R/40 PASS manuscript into editable Word format.",
  "- Appended eligible canonical tables as native editable Word tables when enabled.",
  "- Added figure legends when enabled.",
  "- Discovered usable figure renderings independently of the R/38 coverage_required representative flag.",
  "- Preferred directly embeddable raster renderings after semantic figure deduplication.",
  "- Appended the actual numbered figure images at the end of the Word manuscript when enabled.",
  "- Synchronized manuscript body references, figure legends, and actual figure-image numbering.",
  "- Assigned Table numbers and Figure numbers independently according to first mention in the final manuscript body.",
  "- Copied canonical publication artifacts into tables/ and figures/ when enabled.",
  "",
  "WHAT R/41 DID NOT DO",
  "--------------------------------------------------",
  "- No scientific rewriting.",
  "- No statistical recalculation.",
  "- No literature search.",
  "- No citation renumbering beyond what already existed in the R/40 PASS manuscript.",
  "- No journal-specific formatting.",
  "- No scientific modification of figure content; PDF/SVG rasterization is rendering-only.",
  "",
  "AUTHOR FINAL REVIEW",
  "--------------------------------------------------",
  "The author should now review wording, interpretation, emphasis, authorship/affiliation information, acknowledgments, funding, disclosures, and any journal-specific requirements.",
  "",
  "Journal-specific formatting should be performed only after a target journal is selected."
)

write_text_file(
  readme_lines,
  readme_file
)


# ============================================================
# 16. Report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/41 — Journal-Neutral Author-Editable DOCX Export",
  "",
  paste0(
    "Version: 1.11.0"
  ),
  paste0(
    "Generated: ",
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
  "",
  "UPSTREAM GATE",
  "--------------------------------------------------",
  paste0(
    "R/40 status: ",
    qc_status
  ),
  paste0(
    "Source manuscript SHA-256: ",
    candidate_sha256
  ),
  "",
  "EXPORT POLICY",
  "--------------------------------------------------",
  "Journal neutral: TRUE",
  "Author editable: TRUE",
  "Scientific rewriting: NO",
  "Statistical recalculation: NO",
  "External API calls: NO",
  "Table/Figure cross-reference strategy: anchor text -> Results prose scoring -> heading fallback; then number by first manuscript mention",
  paste0(
    "Resolved Word styles: title=", style_map$title,
    "; heading1=", style_map$heading1,
    "; heading2=", style_map$heading2,
    "; heading3=", style_map$heading3,
    "; normal=", style_map$normal
  ),
  paste0(
    "Tables appended as editable Word tables: ",
    include_tables
  ),
  paste0(
    "Figure legends appended: ",
    include_figure_legends
  ),
  paste0(
    "Actual figures appended at manuscript end: ",
    append_figures_at_end
  ),
  paste0(
    "Actual figures prepared/embedded: ",
    if (
      append_figures_at_end
    ) {
      nrow(
        figure_embedding_audit
      )
    } else {
      0L
    }
  ),
  paste0(
    "Figure embedding audit: ",
    normalize_slash(
      figure_embedding_audit_file
    )
  ),
  paste0(
    "Figure discovery audit: ",
    normalize_slash(
      figure_discovery_audit_file
    )
  ),
  paste0(
    "PDF renderer inventory: ",
    normalize_slash(
      pdf_renderer_inventory_file
    )
  ),
  paste0(
    "Figure render dependency audit: ",
    normalize_slash(
      figure_render_dependency_audit_file
    )
  ),
  paste0(
    "Automatic renderer dependency installation: ",
    auto_install_figure_render_dependencies
  ),
  paste0(
    "Private renderer runtime library: ",
    normalize_slash(
      figure_runtime_library
    )
  ),
  paste0(
    "Usable figure files discovered in registry: ",
    available_registry_figure_count
  ),
  paste0(
    "Final semantic figures: ",
    nrow(
      figure_artifacts
    )
  ),
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0(
    "DOCX: ",
    normalize_slash(
      docx_file
    )
  ),
  paste0(
    "DOCX SHA-256: ",
    docx_sha256
  ),
  paste0(
    "Manifest: ",
    normalize_slash(
      manifest_file
    )
  ),
  paste0(
    "Table/Figure cross-reference map: ",
    normalize_slash(
      crossref_file
    )
  ),
  paste0(
    "State: ",
    normalize_slash(
      state_file
    )
  ),
  paste0(
    "README: ",
    normalize_slash(
      readme_file
    )
  ),
  "",
  "STATUS",
  "--------------------------------------------------",
  "PASS_AUTHOR_REVIEW_READY"
)

write_text_file(
  report_lines,
  report_file
)


# ============================================================
# 17. Console summary
# ============================================================

cat(
  "\n============================================================\n",
  "R/41 — Journal-Neutral Author-Editable DOCX Export\n",
  "============================================================\n\n",
  sep = ""
)

cat(
  "Project: ",
  project_title,
  "\n",
  "Upstream R/40 status: ",
  qc_status,
  "\n",
  "Journal neutral: TRUE\n",
  "Scientific rewriting: NO\n",
  "Statistical recalculation: NO\n",
  "External API calls: NO\n",
  "Cross-reference strategy: anchor text -> Results prose scoring -> heading fallback; then number by first manuscript mention\n",
  "Resolved title style: ",
  style_map$title,
  "\n",
  "Resolved heading styles: ",
  paste(
    c(
      style_map$heading1,
      style_map$heading2,
      style_map$heading3
    ),
    collapse = " / "
  ),
  "\n",
  "Resolved normal style: ",
  style_map$normal,
  "\n",
  "Editable tables: ",
  include_tables,
  "\n",
  "Usable registry figure files: ",
  available_registry_figure_count,
  "\n",
  "Final semantic figures: ",
  nrow(
    figure_artifacts
  ),
  "\n",
  "Figure legends: ",
  include_figure_legends,
  "\n",
  "Actual figures appended at manuscript end: ",
  append_figures_at_end,
  "\n",
  "Actual figures embedded: ",
  if (
    append_figures_at_end
  ) {
    nrow(
      figure_embedding_audit
    )
  } else {
    0L
  },
  "\n",
  sep = ""
)

cat(
  "\nDOCX: ",
  normalize_slash(
    docx_file
  ),
  "\n",
  "Manifest: ",
  normalize_slash(
    manifest_file
  ),
  "\n",
  "Cross-reference map: ",
  normalize_slash(
    crossref_file
  ),
  "\n",
  "Figure discovery audit: ",
  normalize_slash(
    figure_discovery_audit_file
  ),
  "\n",
  "PDF renderer inventory: ",
  normalize_slash(
    pdf_renderer_inventory_file
  ),
  "\n",
  "Figure renderer dependency audit: ",
  normalize_slash(
    figure_render_dependency_audit_file
  ),
  "\n",
  "Private renderer runtime library: ",
  normalize_slash(
    figure_runtime_library
  ),
  "\n",
  "Figure embedding audit: ",
  normalize_slash(
    figure_embedding_audit_file
  ),
  "\n",
  "Report: ",
  normalize_slash(
    report_file
  ),
  "\n",
  "README: ",
  normalize_slash(
    readme_file
  ),
  "\n\nPASS: Journal-neutral author-editable manuscript package is ready for final author review.\n",
  sep = ""
)
