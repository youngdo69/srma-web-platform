# ============================================================
# Universal SR-MA Agent Toolkit
# R/38_prepare_manuscript_evidence_package.R
#
# VERSION: 2.1.3
#
# PURPOSE
# ------------------------------------------------------------
# First manuscript-production module after the analytical SR-MA
# workflow. This module DOES NOT write interpretive scientific
# prose and DOES NOT recalculate any statistical result.
#
# It:
#   1) validates and inventories canonical evidence artifacts;
#   2) freezes/copies manuscript-relevant evidence into one package;
#   3) records SHA-256 hashes for auditability;
#   4) creates an evidence registry and evidence-map workbook;
#   5) generates deterministic Methods/Results factual blocks;
#   6) generates a manuscript scaffold with explicit AUTHOR/AGENT
#      drafting placeholders for Introduction and Discussion;
#   7) distinguishes DRAFT_READY_PENDING_ROB from
#      FINAL_EVIDENCE_READY.
#
# ARCHITECTURAL RULES
# ------------------------------------------------------------
# - R/27 remains the canonical statistical engine.
# - R/33 remains a publication-output/reporting layer.
# - R/35 remains the canonical PRISMA source.
# - R/37 remains the canonical final RoB source.
# - R/38 performs NO statistical calculation.
# - R/38 must never fabricate unavailable values.
# - Introduction/Discussion claims requiring external literature are
#   NOT invented here.
# - Project-specific content comes from project_config.yml and
#   canonical upstream artifacts only.
#
# OUTPUT ROOT
# ------------------------------------------------------------
# results/manuscript/production/
#   evidence_package/
#   manuscript_evidence_registry.csv
#   manuscript_artifact_registry.csv
#   manuscript_freeze_manifest.csv
#   manuscript_evidence_map.xlsx
#   methods_factual_blocks.md
#   results_factual_blocks.md
#   manuscript_scaffold.md
#   author_tasks.md
#   manuscript_production_state.yml
#   manuscript_production_report.txt
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

safe_num <- function(x) {
  suppressWarnings(as.numeric(x))
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

first_existing_col <- function(data, candidates) {
  hits <- candidates[
    candidates %in% names(data)
  ]

  if (length(hits) == 0L) {
    return(NA_character_)
  }

  hits[[1]]
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

format_num <- function(x, digits = 3) {
  x <- safe_num(x)

  if (
    length(x) == 0L ||
    is.na(x[[1]])
  ) {
    return("")
  }

  value <- x[[1]]

  if (value == 0) {
    return("0")
  }

  if (
    abs(value) < 0.001 ||
    abs(value) >= 10000
  ) {
    return(
      formatC(
        value,
        format = "e",
        digits = max(2L, digits - 1L)
      )
    )
  }

  formatC(
    value,
    format = "fg",
    digits = digits,
    flag = "#"
  )
}

copy_file_preserve <- function(
    source,
    destination
) {
  if (!file_nonempty(source)) {
    return(FALSE)
  }

  dir.create(
    dirname(destination),
    recursive = TRUE,
    showWarnings = FALSE
  )

  isTRUE(
    file.copy(
      source,
      destination,
      overwrite = TRUE
    )
  )
}

relative_to_root <- function(
    path,
    root
) {
  path <- normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )

  root <- normalizePath(
    root,
    winslash = "/",
    mustWork = FALSE
  )

  root_prefix <- paste0(
    root,
    "/"
  )

  if (startsWith(path, root_prefix)) {
    substring(
      path,
      nchar(root_prefix) + 1L
    )
  } else {
    normalize_slash(path)
  }
}

markdown_escape <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(
    x,
    "\\|",
    "\\\\|"
  )
  x <- stringr::str_replace_all(
    x,
    "\n",
    " "
  )
  x
}

table_to_markdown <- function(
    data,
    max_rows = 50L
) {
  if (
    is.null(data) ||
    nrow(data) == 0L ||
    ncol(data) == 0L
  ) {
    return(
      "_No data available._"
    )
  }

  data <- as.data.frame(
    data,
    stringsAsFactors = FALSE
  )

  if (nrow(data) > max_rows) {
    data <- data[
      seq_len(max_rows),
      ,
      drop = FALSE
    ]
  }

  data[] <- lapply(
    data,
    function(x) {
      x <- as.character(x)
      x[
        is.na(x)
      ] <- ""
      markdown_escape(x)
    }
  )

  header <- paste0(
    "| ",
    paste(
      names(data),
      collapse = " | "
    ),
    " |"
  )

  divider <- paste0(
    "| ",
    paste(
      rep(
        "---",
        ncol(data)
      ),
      collapse = " | "
    ),
    " |"
  )

  rows <- apply(
    data,
    1,
    function(row) {
      paste0(
        "| ",
        paste(
          row,
          collapse = " | "
        ),
        " |"
      )
    }
  )

  paste(
    c(
      header,
      divider,
      rows
    ),
    collapse = "\n"
  )
}

pick_config_value <- function(
    x,
    path,
    default = ""
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

  if (
    is.null(cur) ||
    length(cur) == 0L
  ) {
    return(default)
  }

  if (
    is.atomic(cur) &&
    length(cur) == 1L
  ) {
    return(
      safe_chr(
        cur,
        default
      )
    )
  }

  cur
}

first_config_value <- function(
    config,
    paths,
    default = ""
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
      if (
        is.atomic(value) &&
        length(value) == 1L
      ) {
        text <- safe_chr(value)
        if (nzchar(text)) {
          return(text)
        }
      } else {
        return(value)
      }
    }
  }

  default
}

as_display_text <- function(
    x,
    default = ""
) {
  if (
    is.null(x) ||
    length(x) == 0L
  ) {
    return(default)
  }

  if (is.atomic(x)) {
    values <- trimws(
      as.character(x)
    )

    values <- values[
      !is.na(values) &
        nzchar(values)
    ]

    if (length(values) == 0L) {
      return(default)
    }

    return(
      paste(
        values,
        collapse = "; "
      )
    )
  }

  default
}



collect_unique_values <- function(
    data,
    candidates
) {
  if (
    is.null(data) ||
      nrow(data) == 0L
  ) {
    return(character(0))
  }

  cols <- intersect(
    candidates,
    names(data)
  )

  if (length(cols) == 0L) {
    return(character(0))
  }

  values <- unlist(
    lapply(
      cols,
      function(col) {
        as.character(
          data[[col]]
        )
      }
    ),
    use.names = FALSE
  )

  values <- trimws(values)
  values <- values[
    !is.na(values) &
      nzchar(values)
  ]

  unique(values)
}

read_text_if_nonempty <- function(
    path,
    max_lines = 250L
) {
  if (!file_nonempty(path)) {
    return("")
  }

  lines <- readLines(
    path,
    warn = FALSE,
    encoding = "UTF-8"
  )

  if (length(lines) > max_lines) {
    lines <- c(
      lines[
        seq_len(max_lines)
      ],
      paste0(
        "[TRUNCATED AFTER ",
        max_lines,
        " LINES]"
      )
    )
  }

  paste(
    lines,
    collapse = "\n"
  )
}

as_flag <- function(
    x,
    default = FALSE
) {
  if (
    is.null(x) ||
      length(x) == 0L ||
      all(is.na(x))
  ) {
    return(default)
  }

  value <- tolower(
    trimws(
      as.character(
        x[[1]]
      )
    )
  )

  if (
    value %in%
      c(
        "true",
        "t",
        "1",
        "yes",
        "y",
        "on",
        "enabled"
      )
  ) {
    return(TRUE)
  }

  if (
    value %in%
      c(
        "false",
        "f",
        "0",
        "no",
        "n",
        "off",
        "disabled"
      )
  ) {
    return(FALSE)
  }

  default
}

nonempty_or_none <- function(
    x,
    none_text = "None documented in canonical executed outputs."
) {
  x <- trimws(
    as.character(x)
  )

  x <- x[
    !is.na(x) &
      nzchar(x)
  ]

  if (length(x) == 0L) {
    return(none_text)
  }

  paste(
    unique(x),
    collapse = "; "
  )
}

table_evidence_section <- function(
    title,
    data,
    source_label,
    max_rows = 50L
) {
  if (
    is.null(data) ||
      nrow(data) == 0L
  ) {
    return(
      c(
        paste0(
          "### ",
          title
        ),
        "",
        paste0(
          "- Source: ",
          source_label
        ),
        "- Status: not available in the frozen evidence package.",
        ""
      )
    )
  }

  c(
    paste0(
      "### ",
      title
    ),
    "",
    paste0(
      "- Source: ",
      source_label
    ),
    paste0(
      "- Rows: ",
      nrow(data),
      "; columns: ",
      ncol(data),
      "."
    ),
    "- The table below is canonical evidence for manuscript description. Describe patterns faithfully; do not compute or invent inferential statistics from it.",
    "",
    table_to_markdown(
      data,
      max_rows = max_rows
    ),
    ""
  )
}


humanize_artifact_name <- function(x) {
  x <- tools::file_path_sans_ext(
    basename(
      as.character(x)
    )
  )

  x <- gsub(
    "^[Tt]able[_ -]*[0-9]+[_ -]*",
    "",
    x,
    perl = TRUE
  )

  x <- gsub(
    "^[Ff]ig(?:ure)?[_ -]*[0-9]+[_ -]*",
    "",
    x,
    perl = TRUE
  )

  x <- gsub(
    "[_-]+",
    " ",
    x,
    perl = TRUE
  )

  x <- gsub(
    "[[:space:]]+",
    " ",
    trimws(x),
    perl = TRUE
  )

  if (!nzchar(x)) {
    return("Untitled artifact")
  }

  paste0(
    toupper(substr(x, 1L, 1L)),
    substr(x, 2L, nchar(x))
  )
}

artifact_display_label <- function(path) {
  name <- basename(
    as.character(path)
  )

  stem <- tools::file_path_sans_ext(name)

  table_match <- regexec(
    "^[Tt]able[_ -]*([0-9]+)",
    stem,
    perl = TRUE
  )

  table_parts <- regmatches(
    stem,
    table_match
  )[[1]]

  if (length(table_parts) >= 2L) {
    return(
      paste0(
        "Table ",
        table_parts[[2]],
        " — ",
        humanize_artifact_name(stem)
      )
    )
  }

  figure_match <- regexec(
    "^[Ff]ig(?:ure)?[_ -]*([0-9]+)",
    stem,
    perl = TRUE
  )

  figure_parts <- regmatches(
    stem,
    figure_match
  )[[1]]

  if (length(figure_parts) >= 2L) {
    return(
      paste0(
        "Figure ",
        figure_parts[[2]],
        " — ",
        humanize_artifact_name(stem)
      )
    )
  }

  lower_name <- tolower(stem)

  if (grepl("prisma", lower_name, fixed = TRUE)) {
    return("PRISMA flow diagram")
  }

  if (grepl("forest", lower_name, fixed = TRUE)) {
    parent_name <- basename(
      dirname(path)
    )

    return(
      paste0(
        "Forest plot — ",
        humanize_artifact_name(parent_name)
      )
    )
  }

  if (
    grepl("risk", lower_name, fixed = TRUE) ||
      grepl("rob2", lower_name, fixed = TRUE) ||
      grepl("robins", lower_name, fixed = TRUE) ||
      grepl("traffic", lower_name, fixed = TRUE)
  ) {
    return(
      paste0(
        "Risk-of-bias figure — ",
        humanize_artifact_name(stem)
      )
    )
  }

  humanize_artifact_name(stem)
}

artifact_role_from_path <- function(path) {
  rel <- tolower(
    normalize_slash(
      as.character(path)
    )
  )

  if (grepl("/prisma/", rel, fixed = TRUE)) {
    return("study_selection")
  }

  if (grepl("/risk_of_bias/", rel, fixed = TRUE)) {
    return("risk_of_bias")
  }

  if (
    grepl("exploratory", rel, fixed = TRUE) ||
      grepl("diagnostic", rel, fixed = TRUE) ||
      grepl("sensitivity", rel, fixed = TRUE)
  ) {
    return("diagnostic_result")
  }

  if (grepl("/meta_analysis/", rel, fixed = TRUE)) {
    return("quantitative_result")
  }

  if (grepl("/manuscript/tables/", rel, fixed = TRUE)) {
    return("descriptive_result")
  }

  "result_artifact"
}

artifact_type_from_path <- function(path) {
  ext <- tolower(
    tools::file_ext(path)
  )

  rel <- tolower(
    normalize_slash(
      as.character(path)
    )
  )

  if (
    ext %in% c("pdf", "png", "jpg", "jpeg", "svg", "tif", "tiff")
  ) {
    return("figure")
  }

  if (
    ext %in% c("csv", "tsv") &&
      grepl("/manuscript/tables/", rel, fixed = TRUE)
  ) {
    return("table")
  }

  if (
    ext %in% c("xlsx", "xls") &&
      grepl("/manuscript/tables/", rel, fixed = TRUE)
  ) {
    return("table_container")
  }

  "backing_data"
}

artifact_priority_from_path <- function(
    path,
    artifact_type
) {
  rel <- tolower(
    normalize_slash(
      as.character(path)
    )
  )

  name <- tolower(
    basename(path)
  )

  if (
    artifact_type == "table" &&
      grepl("/manuscript/tables/", rel, fixed = TRUE)
  ) {
    return("primary")
  }

  if (
    artifact_type == "figure" &&
      (
        grepl("prisma", name, fixed = TRUE) ||
          grepl("forest", name, fixed = TRUE) ||
          grepl("traffic", name, fixed = TRUE) ||
          grepl("rob2", name, fixed = TRUE) ||
          grepl("robins", name, fixed = TRUE)
      )
  ) {
    return("primary")
  }

  if (
    artifact_type == "figure" &&
      (
        grepl("funnel", name, fixed = TRUE) ||
          grepl("leave", name, fixed = TRUE) ||
          grepl("influence", name, fixed = TRUE) ||
          grepl("baujat", name, fixed = TRUE) ||
          grepl("regression", name, fixed = TRUE) ||
          grepl("sensitivity", name, fixed = TRUE)
      )
  ) {
    return("supporting")
  }

  "supplementary"
}

coverage_required_for_artifact <- function(
    artifact_type,
    manuscript_priority
) {
  artifact_type %in% c("table", "figure") &&
    manuscript_priority %in% c("primary", "supporting")
}

derive_analysis_group <- function(path) {
  rel <- normalize_slash(
    as.character(path)
  )

  marker <- "/results/meta_analysis/"
  lower <- tolower(rel)
  marker_pos <- regexpr(
    marker,
    lower,
    fixed = TRUE
  )

  if (marker_pos[[1]] < 0L) {
    return("")
  }

  tail <- substr(
    rel,
    marker_pos[[1]] + attr(marker_pos, "match.length"),
    nchar(rel)
  )

  parts <- strsplit(
    tail,
    "/",
    fixed = TRUE
  )[[1]]

  if (length(parts) < 2L) {
    return("")
  }

  parts[[1]]
}

classify_effect_family <- function(effect_measure) {
  x <- toupper(
    trimws(
      as.character(effect_measure)
    )
  )

  if (!nzchar(x)) {
    return("unspecified/generic")
  }

  if (
    x %in% c("RR", "OR", "RD", "RISK RATIO", "ODDS RATIO", "RISK DIFFERENCE")
  ) {
    return("binary")
  }

  if (
    x %in% c("HR", "HAZARD RATIO")
  ) {
    return("time-to-event")
  }

  if (
    x %in% c("MD", "SMD", "WMD", "MEAN DIFFERENCE", "STANDARDIZED MEAN DIFFERENCE")
  ) {
    return("continuous")
  }

  if (
    grepl("RATE|IRR|INCIDENCE", x, perl = TRUE)
  ) {
    return("rate")
  }

  if (
    grepl("PROPORTION|PREVALENCE|PREVALENCE RATIO", x, perl = TRUE)
  ) {
    return("proportion/prevalence")
  }

  if (
    grepl("CORRELATION|PEARSON|SPEARMAN|FISHER", x, perl = TRUE)
  ) {
    return("correlation")
  }

  if (
    grepl("SENSITIVITY|SPECIFICITY|DOR|DIAGNOSTIC|LR\\+|LR-", x, perl = TRUE)
  ) {
    return("diagnostic-accuracy")
  }

  "generic effect estimate"
}

publication_artifact_selection_mask <- function(
    relative_paths,
    extensions
) {
  rel <- tolower(
    normalize_slash(
      as.character(relative_paths)
    )
  )

  ext <- tolower(
    as.character(extensions)
  )

  if (length(rel) != length(ext)) {
    stop(
      "Internal artifact-discovery contract error: relative_paths and extensions must have equal length.",
      call. = FALSE
    )
  }

  if (length(rel) == 0L) {
    return(
      logical(0)
    )
  }

  image_extensions <- c(
    "pdf",
    "png",
    "jpg",
    "jpeg",
    "svg",
    "tif",
    "tiff"
  )

  table_extensions <- c(
    "csv",
    "tsv",
    "xlsx",
    "xls"
  )

  in_manuscript_tables <- grepl(
    "/manuscript/tables/",
    paste0("/", rel),
    fixed = TRUE
  ) &
    ext %in% table_extensions

  in_figures_dir <- grepl(
    "/figures/",
    paste0("/", rel),
    fixed = TRUE
  ) &
    ext %in% image_extensions

  # IMPORTANT: grepl() is vectorized. Use element-wise "|" here, not
  # scalar "||", because rel may contain hundreds of discovered files.
  base_name <- basename(rel)

  named_primary_figure <- (
    grepl(
      "forest_plot",
      base_name,
      fixed = TRUE
    ) |
      grepl(
        "prisma",
        base_name,
        fixed = TRUE
      )
  ) &
    ext %in% image_extensions

  excluded <- grepl(
    "/manuscript/production/",
    paste0("/", rel),
    fixed = TRUE
  )

  (
    in_manuscript_tables |
      in_figures_dir |
      named_primary_figure
  ) &
    !excluded
}

artifact_discovery_self_test <- function() {
  test_rel <- c(
    "results/meta_analysis/primary/forest_plot.pdf",
    "results/prisma/prisma_flow.pdf",
    "results/manuscript/tables/study_characteristics.csv",
    "results/manuscript/production/qc/old_figure.pdf",
    "results/notes/readme.txt"
  )

  test_ext <- c(
    "pdf",
    "pdf",
    "csv",
    "pdf",
    "txt"
  )

  expected <- c(
    TRUE,
    TRUE,
    TRUE,
    FALSE,
    FALSE
  )

  observed <- publication_artifact_selection_mask(
    test_rel,
    test_ext
  )

  if (
    length(observed) != length(expected) ||
      !identical(
        as.logical(observed),
        expected
      )
  ) {
    stop(
      "Internal artifact-discovery vectorization self-test failed.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

artifact_discovery_self_test()

discover_publication_artifacts <- function(repo_root) {
  results_root <- file.path(
    repo_root,
    "results"
  )

  if (!dir.exists(results_root)) {
    return(character(0))
  }

  files <- list.files(
    results_root,
    recursive = TRUE,
    full.names = TRUE,
    all.files = FALSE
  )

  if (length(files) == 0L) {
    return(character(0))
  }

  files <- files[
    !dir.exists(files)
  ]

  if (length(files) == 0L) {
    return(character(0))
  }

  rel <- vapply(
    files,
    relative_to_root,
    character(1),
    root = repo_root
  )

  ext <- tools::file_ext(
    files
  )

  keep <- publication_artifact_selection_mask(
    rel,
    ext
  )

  if (length(keep) != length(files)) {
    stop(
      "Internal artifact-discovery contract error: selection mask length does not match discovered files.",
      call. = FALSE
    )
  }

  unique(
    files[
      keep
    ]
  )
}

read_publication_table <- function(path) {
  ext <- tolower(
    tools::file_ext(path)
  )

  if (ext == "csv") {
    data <- tryCatch(
      readr::read_csv(
        path,
        show_col_types = FALSE,
        progress = FALSE
      ),
      error = function(e) NULL
    )

    if (is.null(data)) {
      return(list())
    }

    return(
      list(
        table = data
      )
    )
  }

  if (ext == "tsv") {
    data <- tryCatch(
      readr::read_tsv(
        path,
        show_col_types = FALSE,
        progress = FALSE
      ),
      error = function(e) NULL
    )

    if (is.null(data)) {
      return(list())
    }

    return(
      list(
        table = data
      )
    )
  }

  if (ext %in% c("xlsx", "xls")) {
    sheets <- tryCatch(
      openxlsx::getSheetNames(path),
      error = function(e) character(0)
    )

    if (length(sheets) == 0L) {
      return(list())
    }

    out <- list()

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
          nrow(data) > 0L
      ) {
        out[[sheet]] <- tibble::as_tibble(data)
      }
    }

    return(out)
  }

  list()
}

detect_model_features <- function(
    model_texts
) {
  text <- paste(
    model_texts,
    collapse = "\n"
  )

  features <- character(0)

  if (
    grepl(
      "random[- ]effects|random effects model",
      text,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    features <- c(
      features,
      "Random-effects model explicitly present in canonical model output"
    )
  }

  if (
    grepl(
      "fixed[- ]effects|fixed effect model",
      text,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    features <- c(
      features,
      "Fixed-effect model explicitly present in canonical model output"
    )
  }

  if (
    grepl(
      "\\bREML\\b|restricted maximum likelihood",
      text,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    features <- c(
      features,
      "REML between-study variance estimation explicitly present"
    )
  }

  if (
    grepl(
      "Hartung|Knapp|\\bHK\\b",
      text,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    features <- c(
      features,
      "Hartung-Knapp/Knapp-Hartung inference explicitly present"
    )
  }

  if (
    grepl(
      "DerSimonian|\\bDL\\b",
      text,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    features <- c(
      features,
      "DerSimonian-Laird variance estimation explicitly present"
    )
  }

  if (
    grepl(
      "Paule|Mandel|\\bPM\\b",
      text,
      ignore.case = TRUE,
      perl = TRUE
    )
  ) {
    features <- c(
      features,
      "Paule-Mandel variance estimation explicitly present"
    )
  }

  unique(features)
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
    "config/project_config.yml was not found. Run R/38 from the project root.",
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

project_id <- first_config_value(
  config,
  c(
    "project.id",
    "project_id"
  ),
  default = basename(
    repo_root
  )
)

project_title <- first_config_value(
  config,
  c(
    "project.title",
    "title"
  ),
  default = project_id
)

reporting_guideline <- first_config_value(
  config,
  c(
    "reporting.guideline",
    "analysis.reporting_guideline"
  ),
  default = "PRISMA 2020"
)

registered_protocol <- first_config_value(
  config,
  c(
    "registration.prospero_id",
    "registration.registration_id",
    "prospero_id"
  ),
  default = ""
)


# ============================================================
# 3. Output paths
# ============================================================

manuscript_root <- file.path(
  repo_root,
  "results",
  "manuscript"
)

production_root <- file.path(
  manuscript_root,
  "production"
)

package_root <- file.path(
  production_root,
  "evidence_package"
)

dir.create(
  production_root,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  package_root,
  recursive = TRUE,
  showWarnings = FALSE
)

registry_file <- file.path(
  production_root,
  "manuscript_evidence_registry.csv"
)

artifact_registry_file <- file.path(
  production_root,
  "manuscript_artifact_registry.csv"
)

freeze_manifest_file <- file.path(
  production_root,
  "manuscript_freeze_manifest.csv"
)

evidence_map_file <- file.path(
  production_root,
  "manuscript_evidence_map.xlsx"
)

methods_blocks_file <- file.path(
  production_root,
  "methods_factual_blocks.md"
)

results_blocks_file <- file.path(
  production_root,
  "results_factual_blocks.md"
)

scaffold_file <- file.path(
  production_root,
  "manuscript_scaffold.md"
)

author_tasks_file <- file.path(
  production_root,
  "author_tasks.md"
)

state_file <- file.path(
  production_root,
  "manuscript_production_state.yml"
)

report_file <- file.path(
  production_root,
  "manuscript_production_report.txt"
)


# ============================================================
# 4. Evidence artifact registry definition
# ============================================================

artifact_specs <- tibble::tribble(
  ~logical_name, ~role, ~source_rel, ~required_for_draft, ~required_for_final,
  "project_config",
  "protocol_methods",
  "config/project_config.yml",
  TRUE,
  TRUE,

  "included_studies",
  "study_selection",
  "data/processed/full_text/included_studies.csv",
  TRUE,
  TRUE,

  "prisma_source_registry",
  "study_selection",
  "results/prisma/prisma_source_registry.csv",
  TRUE,
  TRUE,

  "prisma_flow_audit",
  "study_selection",
  "results/prisma/prisma_flow_audit.csv",
  FALSE,
  TRUE,

  "prisma_full_text_exclusions",
  "study_selection",
  "results/prisma/prisma_full_text_exclusion_reasons.csv",
  FALSE,
  TRUE,

  "prisma_pdf",
  "figure",
  "results/prisma/prisma_2020_flow_diagram.pdf",
  FALSE,
  TRUE,




  "manuscript_tables_workbook",
  "manuscript_table",
  "results/manuscript/tables/manuscript_tables_author_editable.xlsx",
  FALSE,
  TRUE,

  "meta_execution_summary",
  "quantitative_result",
  "results/meta_analysis/meta_analysis_execution_summary.csv",
  TRUE,
  TRUE,

  "meta_reporting_summary",
  "quantitative_result",
  "results/meta_analysis/tables/meta_analysis_summary.csv",
  FALSE,
  TRUE,

  "study_level_effects",
  "quantitative_result",
  "results/meta_analysis/tables/study_level_effects.csv",
  FALSE,
  TRUE,

  "meta_outputs_workbook",
  "quantitative_result",
  "results/meta_analysis/tables/meta_analysis_outputs.xlsx",
  FALSE,
  TRUE,

  "meta_diagnostics_summary",
  "diagnostic_result",
  "results/meta_analysis_diagnostics/meta_analysis_diagnostics_summary.csv",
  FALSE,
  TRUE,

  "meta_diagnostics_workbook",
  "diagnostic_result",
  "results/meta_analysis_diagnostics/meta_analysis_diagnostics.xlsx",
  FALSE,
  FALSE,

  "exploratory_diagnostics_summary",
  "exploratory_result",
  "results/meta_analysis_exploratory_diagnostics/exploratory_diagnostics_summary.csv",
  FALSE,
  FALSE,

  "exploratory_diagnostics_workbook",
  "exploratory_result",
  "results/meta_analysis_exploratory_diagnostics/exploratory_diagnostics.xlsx",
  FALSE,
  FALSE,

  "rob_final",
  "risk_of_bias",
  "results/risk_of_bias/risk_of_bias_final.csv",
  FALSE,
  TRUE,

  "rob2_final",
  "risk_of_bias",
  "results/risk_of_bias/rob2_final.csv",
  FALSE,
  FALSE,

  "robins_i_final",
  "risk_of_bias",
  "results/risk_of_bias/robins_i_final.csv",
  FALSE,
  FALSE,

  "rob2_traffic_light",
  "figure",
  "results/risk_of_bias/figures/rob2_traffic_light_and_summary.pdf",
  FALSE,
  FALSE,

  "robins_i_traffic_light",
  "figure",
  "results/risk_of_bias/figures/robins_i_traffic_light_and_summary.pdf",
  FALSE,
  FALSE
)

artifact_specs <- artifact_specs |>
  dplyr::mutate(
    source_path = file.path(
      repo_root,
      source_rel
    ),
    exists = vapply(
      source_path,
      file_nonempty,
      logical(1)
    ),
    status = ifelse(
      exists,
      "AVAILABLE",
      "MISSING"
    )
  )


# ============================================================
# 4.1 Discover publication-oriented tables and figures dynamically
# ============================================================

publication_artifact_paths <- discover_publication_artifacts(
  repo_root
)

if (length(publication_artifact_paths) > 0L) {
  dynamic_rows <- lapply(
    publication_artifact_paths,
    function(path) {
      rel <- normalize_slash(
        relative_to_root(
          path,
          repo_root
        )
      )

      artifact_type <- artifact_type_from_path(
        path
      )

      manuscript_priority <- artifact_priority_from_path(
        path,
        artifact_type
      )

      tibble::tibble(
        logical_name = paste0(
          "artifact_",
          substr(
            digest::digest(
              rel,
              algo = "sha256",
              serialize = FALSE
            ),
            1L,
            12L
          )
        ),
        role = artifact_role_from_path(
          path
        ),
        source_rel = rel,
        required_for_draft = FALSE,
        required_for_final = FALSE,
        source_path = normalizePath(
          path,
          winslash = "/",
          mustWork = FALSE
        ),
        exists = file_nonempty(
          path
        ),
        status = ifelse(
          file_nonempty(path),
          "AVAILABLE",
          "MISSING"
        )
      )
    }
  )

  artifact_specs <- dplyr::bind_rows(
    artifact_specs,
    dplyr::bind_rows(
      dynamic_rows
    )
  ) |>
    dplyr::arrange(
      source_rel,
      dplyr::desc(required_for_final),
      dplyr::desc(required_for_draft)
    ) |>
    dplyr::distinct(
      source_rel,
      .keep_all = TRUE
    )
}


# ============================================================
# 5. Discover canonical R/27 per-group artifacts
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

if (length(r27_summary_files) > 0L) {

  extra_rows <- list()

  for (summary_path in r27_summary_files) {

    group_dir <- dirname(
      summary_path
    )

    group_name <- basename(
      group_dir
    )

    study_file <- file.path(
      group_dir,
      "study_effects_used.csv"
    )

    forest_file <- file.path(
      group_dir,
      "forest_plot.pdf"
    )

    model_file <- file.path(
      group_dir,
      "model_output.txt"
    )

    group_slug <- slugify(
      group_name
    )

    extra_rows[[
      length(extra_rows) + 1L
    ]] <- tibble::tibble(
      logical_name = paste0(
        "r27_",
        group_slug,
        "_summary"
      ),
      role = "quantitative_result",
      source_rel = relative_to_root(
        summary_path,
        repo_root
      ),
      required_for_draft = TRUE,
      required_for_final = TRUE,
      source_path = normalizePath(
        summary_path,
        winslash = "/",
        mustWork = FALSE
      ),
      exists = file_nonempty(
        summary_path
      ),
      status = ifelse(
        file_nonempty(summary_path),
        "AVAILABLE",
        "MISSING"
      )
    )

    extra_rows[[
      length(extra_rows) + 1L
    ]] <- tibble::tibble(
      logical_name = paste0(
        "r27_",
        group_slug,
        "_study_effects"
      ),
      role = "quantitative_result",
      source_rel = relative_to_root(
        study_file,
        repo_root
      ),
      required_for_draft = FALSE,
      required_for_final = TRUE,
      source_path = normalizePath(
        study_file,
        winslash = "/",
        mustWork = FALSE
      ),
      exists = file_nonempty(
        study_file
      ),
      status = ifelse(
        file_nonempty(study_file),
        "AVAILABLE",
        "MISSING"
      )
    )

    extra_rows[[
      length(extra_rows) + 1L
    ]] <- tibble::tibble(
      logical_name = paste0(
        "r27_",
        group_slug,
        "_forest"
      ),
      role = "figure",
      source_rel = relative_to_root(
        forest_file,
        repo_root
      ),
      required_for_draft = FALSE,
      required_for_final = TRUE,
      source_path = normalizePath(
        forest_file,
        winslash = "/",
        mustWork = FALSE
      ),
      exists = file_nonempty(
        forest_file
      ),
      status = ifelse(
        file_nonempty(forest_file),
        "AVAILABLE",
        "MISSING"
      )
    )

    if (file_nonempty(model_file)) {
      extra_rows[[
        length(extra_rows) + 1L
      ]] <- tibble::tibble(
        logical_name = paste0(
          "r27_",
          group_slug,
          "_model_output"
        ),
        role = "provenance",
        source_rel = relative_to_root(
          model_file,
          repo_root
        ),
        required_for_draft = FALSE,
        required_for_final = FALSE,
        source_path = normalizePath(
          model_file,
          winslash = "/",
          mustWork = FALSE
        ),
        exists = TRUE,
        status = "AVAILABLE"
      )
    }
  }

  artifact_specs <- dplyr::bind_rows(
    artifact_specs,
    dplyr::bind_rows(
      extra_rows
    )
  )
}


# Final source-path deduplication after all static, dynamic, and R/27
# artifacts have been registered. Preserve the row carrying the strongest
# workflow requirement when the same physical artifact was discovered twice.
artifact_specs <- artifact_specs |>
  dplyr::arrange(
    source_rel,
    dplyr::desc(required_for_final),
    dplyr::desc(required_for_draft)
  ) |>
  dplyr::distinct(
    source_rel,
    .keep_all = TRUE
  )


# ============================================================
# 6. Determine manuscript readiness
# ============================================================

draft_required <- artifact_specs |>
  dplyr::filter(
    required_for_draft
  )

final_required <- artifact_specs |>
  dplyr::filter(
    required_for_final
  )

draft_missing <- draft_required |>
  dplyr::filter(
    !exists
  )

final_missing <- final_required |>
  dplyr::filter(
    !exists
  )

rob_final_available <- file_nonempty(
  file.path(
    repo_root,
    "results",
    "risk_of_bias",
    "risk_of_bias_final.csv"
  )
)

if (nrow(draft_missing) > 0L) {
  readiness_status <- "NOT_READY"
} else if (
  !rob_final_available ||
  nrow(final_missing) > 0L
) {
  readiness_status <- "DRAFT_READY_PENDING_ROB"
} else {
  readiness_status <- "FINAL_EVIDENCE_READY"
}


# ============================================================
# 7. Freeze/copy evidence package
# ============================================================

registry_rows <- list()

for (
  i in seq_len(
    nrow(
      artifact_specs
    )
  )
) {

  row <- artifact_specs[
    i,
    ,
    drop = FALSE
  ]

  source_path <- row$source_path[[1]]
  source_rel <- row$source_rel[[1]]

  package_rel <- file.path(
    row$role[[1]],
    basename(
      source_path
    )
  )

  # Avoid basename collision by prefixing logical name if required.
  candidate_destination <- file.path(
    package_root,
    package_rel
  )

  if (
    file.exists(
      candidate_destination
    ) &&
    !identical(
      normalizePath(
        candidate_destination,
        winslash = "/",
        mustWork = FALSE
      ),
      normalizePath(
        source_path,
        winslash = "/",
        mustWork = FALSE
      )
    )
  ) {
    package_rel <- file.path(
      row$role[[1]],
      paste0(
        slugify(
          row$logical_name[[1]]
        ),
        "__",
        basename(
          source_path
        )
      )
    )
  }

  destination <- file.path(
    package_root,
    package_rel
  )

  copied <- FALSE

  if (isTRUE(row$exists[[1]])) {
    copied <- copy_file_preserve(
      source_path,
      destination
    )
  }

  source_hash <- if (
    isTRUE(row$exists[[1]])
  ) {
    sha256_file(
      source_path
    )
  } else {
    ""
  }

  package_hash <- if (
    copied
  ) {
    sha256_file(
      destination
    )
  } else {
    ""
  }

  hash_match <- if (
    copied
  ) {
    identical(
      source_hash,
      package_hash
    )
  } else {
    FALSE
  }

  registry_rows[[
    length(registry_rows) + 1L
  ]] <- tibble::tibble(
    logical_name = row$logical_name[[1]],
    role = row$role[[1]],
    required_for_draft = row$required_for_draft[[1]],
    required_for_final = row$required_for_final[[1]],
    source_path = normalize_slash(
      source_path
    ),
    source_relative_path = normalize_slash(
      source_rel
    ),
    source_status = row$status[[1]],
    source_sha256 = source_hash,
    package_path = if (
      copied
    ) {
      normalize_slash(
        destination
      )
    } else {
      ""
    },
    package_relative_path = if (
      copied
    ) {
      normalize_slash(
        relative_to_root(
          destination,
          repo_root
        )
      )
    } else {
      ""
    },
    package_sha256 = package_hash,
    copied = copied,
    hash_match = hash_match,
    frozen_at = now_text()
  )
}

evidence_registry <- dplyr::bind_rows(
  registry_rows
)

readr::write_csv(
  evidence_registry,
  registry_file,
  na = ""
)

artifact_registry <- evidence_registry |>
  dplyr::mutate(
    artifact_type = vapply(
      source_path,
      artifact_type_from_path,
      character(1)
    ),
    title = vapply(
      source_path,
      artifact_display_label,
      character(1)
    ),
    manuscript_role = vapply(
      source_path,
      artifact_role_from_path,
      character(1)
    ),
    analysis_group = vapply(
      source_path,
      derive_analysis_group,
      character(1)
    ),
    file_extension = tolower(
      tools::file_ext(
        source_path
      )
    ),
    manuscript_priority = mapply(
      artifact_priority_from_path,
      source_path,
      artifact_type,
      USE.NAMES = FALSE
    ),
    coverage_required = mapply(
      coverage_required_for_artifact,
      artifact_type,
      manuscript_priority,
      USE.NAMES = FALSE
    ),
    artifact_id = paste0(
      "A_",
      toupper(
        vapply(
          source_relative_path,
          function(path) {
            substr(
              digest::digest(
                path,
                algo = "sha256",
                serialize = FALSE
              ),
              1L,
              12L
            )
          },
          character(1)
        )
      )
    ),
    coverage_token = paste0(
      "<!-- ARTIFACT: ",
      artifact_id,
      " -->"
    ),
    discovery_method = ifelse(
      grepl(
        "^artifact_",
        logical_name
      ),
      "dynamic_publication_artifact_discovery",
      "canonical_core_contract"
    )
  ) |>
  dplyr::select(
    artifact_id,
    artifact_type,
    title,
    manuscript_role,
    manuscript_priority,
    coverage_required,
    coverage_token,
    analysis_group,
    file_extension,
    logical_name,
    source_path,
    source_relative_path,
    source_status,
    source_sha256,
    package_relative_path,
    package_sha256,
    discovery_method
  )

if (
  !any(
    artifact_registry$artifact_type == "table" &
      artifact_registry$source_status == "AVAILABLE"
  ) &&
    any(
      artifact_registry$artifact_type == "table_container" &
        artifact_registry$source_status == "AVAILABLE"
    )
) {
  table_container_rows <- artifact_registry$artifact_type == "table_container" &
    artifact_registry$source_status == "AVAILABLE"

  artifact_registry$manuscript_priority[
    table_container_rows
  ] <- "primary"

  artifact_registry$coverage_required[
    table_container_rows
  ] <- TRUE
}


# ============================================================
# 8.1 Collapse file-level artifacts into semantic coverage units
# ============================================================

coverage_unit_key_for_artifact <- function(
    artifact_type,
    manuscript_role,
    analysis_group,
    source_relative_path
) {
  type <- tolower(
    trimws(
      as.character(artifact_type)
    )
  )

  role <- tolower(
    trimws(
      as.character(manuscript_role)
    )
  )

  group <- tolower(
    trimws(
      as.character(analysis_group)
    )
  )

  rel <- tolower(
    normalize_slash(
      as.character(source_relative_path)
    )
  )

  stem <- tolower(
    tools::file_path_sans_ext(
      basename(rel)
    )
  )

  parent <- dirname(rel)

  if (type == "table") {
    return(
      paste0(
        "table::",
        rel
      )
    )
  }

  if (role == "study_selection") {
    return("study_selection")
  }

  # Risk-of-bias traffic-light, summary, PDF, and image renderings all
  # support one Results concept: the author-approved final RoB assessment.
  if (role == "risk_of_bias") {
    return("risk_of_bias")
  }

  if (role == "quantitative_result") {
    group_key <- if (nzchar(group)) {
      group
    } else {
      parent
    }

    return(
      paste0(
        "quantitative_result::",
        group_key
      )
    )
  }

  if (role == "diagnostic_result") {
    diagnostic_family <- dplyr::case_when(
      grepl("funnel|publication", stem, perl = TRUE) ~ "publication_bias",
      grepl("leave|loo|influence|baujat", stem, perl = TRUE) ~ "influence",
      grepl("regression|moderator|meta_regression", stem, perl = TRUE) ~ "meta_regression",
      grepl("sensitivity", stem, fixed = TRUE) ~ "sensitivity",
      TRUE ~ "other_diagnostic"
    )

    group_key <- if (nzchar(group)) {
      group
    } else {
      parent
    }

    return(
      paste(
        "diagnostic_result",
        group_key,
        diagnostic_family,
        sep = "::"
      )
    )
  }

  # For other figures, collapse alternate renderings with common suffixes.
  normalized_stem <- stem
  normalized_stem <- gsub(
    "(_and_summary|_summary|_traffic_light|_forest_plot|_plot|_figure|_final|_publication)+$",
    "",
    normalized_stem,
    perl = TRUE
  )

  paste(
    type,
    role,
    parent,
    normalized_stem,
    sep = "::"
  )
}

artifact_registry <- artifact_registry |>
  dplyr::mutate(
    coverage_candidate = coverage_required,
    coverage_unit_key = mapply(
      coverage_unit_key_for_artifact,
      artifact_type,
      manuscript_role,
      analysis_group,
      source_relative_path,
      USE.NAMES = FALSE
    ),
    coverage_unit_id = paste0(
      "U_",
      toupper(
        vapply(
          coverage_unit_key,
          function(key) {
            substr(
              digest::digest(
                key,
                algo = "sha256",
                serialize = FALSE
              ),
              1L,
              12L
            )
          },
          character(1)
        )
      )
    ),
    .priority_rank = dplyr::case_when(
      manuscript_priority == "primary" ~ 1L,
      manuscript_priority == "supporting" ~ 2L,
      TRUE ~ 3L
    ),
    .extension_rank = dplyr::case_when(
      file_extension == "pdf" ~ 1L,
      file_extension %in% c("png", "jpg", "jpeg") ~ 2L,
      file_extension %in% c("csv", "tsv") ~ 1L,
      file_extension %in% c("xlsx", "xls") ~ 2L,
      TRUE ~ 3L
    )
  ) |>
  dplyr::arrange(
    coverage_unit_id,
    .priority_rank,
    .extension_rank,
    source_relative_path
  ) |>
  dplyr::group_by(
    coverage_unit_id
  ) |>
  dplyr::mutate(
    coverage_representative = coverage_candidate &
      cumsum(coverage_candidate) == 1L,
    coverage_required = coverage_representative
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    -dplyr::all_of(
      c(
        ".priority_rank",
        ".extension_rank"
      )
    )
  )

# A semantic coverage unit may have many file renderings, but only one
# representative may be required in manuscript prose.
coverage_unit_duplicate_test <- artifact_registry |>
  dplyr::filter(
    coverage_required
  ) |>
  dplyr::count(
    coverage_unit_id,
    name = "n_required"
  ) |>
  dplyr::filter(
    n_required > 1L
  )

if (nrow(coverage_unit_duplicate_test) > 0L) {
  stop(
    "Internal artifact coverage-unit contract failed: more than one required representative exists for a semantic evidence unit.",
    call. = FALSE
  )
}

readr::write_csv(
  artifact_registry,
  artifact_registry_file,
  na = ""
)

freeze_manifest <- evidence_registry |>
  dplyr::filter(
    copied
  ) |>
  dplyr::select(
    logical_name,
    role,
    source_relative_path,
    package_relative_path,
    source_sha256,
    package_sha256,
    hash_match,
    frozen_at
  )

readr::write_csv(
  freeze_manifest,
  freeze_manifest_file,
  na = ""
)

if (
  nrow(freeze_manifest) > 0L &&
  any(!freeze_manifest$hash_match)
) {
  stop(
    "Evidence package freeze failed hash verification for one or more files.",
    call. = FALSE
  )
}


# ============================================================
# 8. Load manuscript-relevant canonical datasets
# ============================================================

load_if_exists <- function(path) {
  if (!file_nonempty(path)) {
    return(NULL)
  }

  tryCatch(
    safe_read_csv(
      path
    ),
    error = function(e) NULL
  )
}

included_studies <- load_if_exists(
  file.path(
    repo_root,
    "data",
    "processed",
    "full_text",
    "included_studies.csv"
  )
)

prisma_registry <- load_if_exists(
  file.path(
    repo_root,
    "results",
    "prisma",
    "prisma_source_registry.csv"
  )
)

prisma_audit <- load_if_exists(
  file.path(
    repo_root,
    "results",
    "prisma",
    "prisma_flow_audit.csv"
  )
)

meta_execution_summary <- load_if_exists(
  file.path(
    repo_root,
    "results",
    "meta_analysis",
    "meta_analysis_execution_summary.csv"
  )
)

meta_reporting_summary <- load_if_exists(
  file.path(
    repo_root,
    "results",
    "meta_analysis",
    "tables",
    "meta_analysis_summary.csv"
  )
)

study_level_effects <- load_if_exists(
  file.path(
    repo_root,
    "results",
    "meta_analysis",
    "tables",
    "study_level_effects.csv"
  )
)

meta_diagnostics_summary <- load_if_exists(
  file.path(
    repo_root,
    "results",
    "meta_analysis_diagnostics",
    "meta_analysis_diagnostics_summary.csv"
  )
)

exploratory_summary <- load_if_exists(
  file.path(
    repo_root,
    "results",
    "meta_analysis_exploratory_diagnostics",
    "exploratory_diagnostics_summary.csv"
  )
)

rob_final <- load_if_exists(
  file.path(
    repo_root,
    "results",
    "risk_of_bias",
    "risk_of_bias_final.csv"
  )
)


# ============================================================
# 9. Load canonical R/27 per-group summaries
# ============================================================

r27_summary_rows <- list()

if (length(r27_summary_files) > 0L) {

  for (summary_path in r27_summary_files) {

    data <- tryCatch(
      safe_read_csv(
        summary_path
      ),
      error = function(e) NULL
    )

    if (
      is.null(data) ||
      nrow(data) == 0L
    ) {
      next
    }

    data <- data |>
      dplyr::mutate(
        source_r27_summary = normalize_slash(
          relative_to_root(
            summary_path,
            repo_root
          )
        )
      )

    r27_summary_rows[[
      length(r27_summary_rows) + 1L
    ]] <- data
  }
}

r27_summary_combined <- if (
  length(r27_summary_rows) == 0L
) {
  tibble::tibble()
} else {
  dplyr::bind_rows(
    r27_summary_rows
  )
}

analysis_family_inventory <- tibble::tibble(
  analysis_group = character(0),
  effect_measure = character(0),
  analysis_family = character(0),
  source = character(0)
)

if (nrow(r27_summary_combined) > 0L) {
  measure_col <- first_existing_col(
    r27_summary_combined,
    c(
      "summary_measure",
      "effect_measure",
      "measure"
    )
  )

  if (!is.null(measure_col)) {
    groups <- vapply(
      r27_summary_combined$source_r27_summary,
      function(path) {
        basename(
          dirname(path)
        )
      },
      character(1)
    )

    measures <- as.character(
      r27_summary_combined[[measure_col]]
    )

    analysis_family_inventory <- tibble::tibble(
      analysis_group = groups,
      effect_measure = measures,
      analysis_family = vapply(
        measures,
        classify_effect_family,
        character(1)
      ),
      source = r27_summary_combined$source_r27_summary
    ) |>
      dplyr::filter(
        !is.na(effect_measure) &
          nzchar(trimws(effect_measure))
      ) |>
      dplyr::distinct()
  }
}

r27_model_outputs <- list()
r27_study_effect_tables <- list()

if (length(r27_summary_files) > 0L) {
  for (summary_path in r27_summary_files) {
    group_dir <- dirname(
      summary_path
    )

    group_name <- basename(
      group_dir
    )

    model_path <- file.path(
      group_dir,
      "model_output.txt"
    )

    study_path <- file.path(
      group_dir,
      "study_effects_used.csv"
    )

    if (file_nonempty(model_path)) {
      r27_model_outputs[[
        group_name
      ]] <- read_text_if_nonempty(
        model_path,
        max_lines = 180L
      )
    }

    if (file_nonempty(study_path)) {
      study_data <- tryCatch(
        safe_read_csv(
          study_path
        ),
        error = function(e) NULL
      )

      if (
        !is.null(study_data) &&
          nrow(study_data) > 0L
      ) {
        r27_study_effect_tables[[
          group_name
        ]] <- study_data
      }
    }
  }
}


# ============================================================
# 10. Deterministic Methods factual blocks
# ============================================================

enabled_databases_node <- pick_config_value(
  config,
  "search.databases",
  default = list()
)

enabled_databases <- character(0)

if (
  is.list(enabled_databases_node) &&
    length(enabled_databases_node) > 0L
) {
  for (db_name in names(enabled_databases_node)) {
    db_cfg <- enabled_databases_node[[
      db_name
    ]]

    enabled <- if (is.list(db_cfg)) {
      as_flag(
        db_cfg$enabled,
        FALSE
      )
    } else {
      as_flag(
        db_cfg,
        FALSE
      )
    }

    if (isTRUE(enabled)) {
      display_name <- if (
        is.list(db_cfg)
      ) {
        safe_chr(
          db_cfg$display_name,
          db_name
        )
      } else {
        db_name
      }

      enabled_databases <- c(
        enabled_databases,
        display_name
      )
    }
  }
}

if (length(enabled_databases) == 0L) {
  enabled_databases <- first_config_value(
    config,
    c(
      "databases.enabled",
      "enabled_databases"
    ),
    default = character(0)
  )
}

enabled_databases_text <- as_display_text(
  enabled_databases,
  default = "No enabled database names were recoverable from canonical configuration."
)


eligibility_dimension_specs <- list(
  population = c(
    "eligibility.population",
    "research_question.population.description",
    "population"
  ),
  intervention = c(
    "eligibility.intervention",
    "research_question.intervention.description",
    "intervention"
  ),
  comparator = c(
    "eligibility.comparator",
    "research_question.comparator.description",
    "comparator"
  ),
  exposure = c(
    "eligibility.exposure",
    "research_question.exposure.description",
    "exposure"
  ),
  index_test = c(
    "eligibility.index_test",
    "research_question.index_test.description",
    "index_test"
  ),
  reference_standard = c(
    "eligibility.reference_standard",
    "research_question.reference_standard.description",
    "reference_standard"
  ),
  prognostic_factor = c(
    "eligibility.prognostic_factor",
    "research_question.prognostic_factor.description",
    "prognostic_factor"
  ),
  outcome = c(
    "eligibility.outcome",
    "eligibility.outcomes",
    "research_question.outcome.description",
    "outcomes"
  ),
  study_design = c(
    "eligibility.study_design",
    "eligibility.study_designs",
    "research_question.study_design.description"
  )
)

eligibility_dimensions <- tibble::tibble(
  dimension = names(
    eligibility_dimension_specs
  ),
  value = vapply(
    eligibility_dimension_specs,
    function(paths) {
      as_display_text(
        first_config_value(
          config,
          paths,
          default = ""
        ),
        default = ""
      )
    },
    character(1)
  )
) |>
  dplyr::filter(
    nzchar(value)
  )

binary_measure <- first_config_value(
  config,
  c(
    "analysis.binary_effect_measure"
  ),
  default = ""
)

continuous_measure <- first_config_value(
  config,
  c(
    "analysis.continuous_effect_measure"
  ),
  default = ""
)

time_to_event_measure <- first_config_value(
  config,
  c(
    "analysis.time_to_event_effect_measure"
  ),
  default = ""
)

primary_model <- first_config_value(
  config,
  c(
    "analysis.model.primary"
  ),
  default = ""
)

tau_method <- first_config_value(
  config,
  c(
    "analysis.heterogeneity.tau_squared_estimator"
  ),
  default = ""
)

hartung_knapp <- first_config_value(
  config,
  c(
    "analysis.heterogeneity.hartung_knapp_adjustment"
  ),
  default = ""
)

rob_config_values <- c(
  first_config_value(
    config,
    c(
      "risk_of_bias.randomized_trials"
    ),
    default = ""
  ),
  first_config_value(
    config,
    c(
      "risk_of_bias.nonrandomized_studies"
    ),
    default = ""
  ),
  first_config_value(
    config,
    c(
      "risk_of_bias.diagnostic_accuracy"
    ),
    default = ""
  ),
  first_config_value(
    config,
    c(
      "risk_of_bias.prognostic_studies"
    ),
    default = ""
  )
)

rob_config_values <- trimws(
  as.character(
    rob_config_values
  )
)

rob_config_values <- unique(
  rob_config_values[
    !is.na(rob_config_values) &
      nzchar(rob_config_values)
  ]
)

rob_executed_values <- collect_unique_values(
  rob_final,
  c(
    "tool",
    "rob_tool",
    "risk_of_bias_tool",
    "assessment_tool",
    "rob_method",
    "risk_of_bias_method"
  )
)

rob_tools_present <- unique(
  c(
    rob_executed_values,
    rob_config_values
  )
)

executed_measures <- unique(
  c(
    collect_unique_values(
      r27_summary_combined,
      c(
        "summary_measure",
        "effect_measure",
        "measure"
      )
    ),
    collect_unique_values(
      meta_execution_summary,
      c(
        "summary_measure",
        "effect_measure",
        "measure"
      )
    )
  )
)

executed_models <- unique(
  c(
    collect_unique_values(
      r27_summary_combined,
      c(
        "model",
        "model_type",
        "meta_model",
        "effect_model",
        "method"
      )
    ),
    collect_unique_values(
      meta_execution_summary,
      c(
        "model",
        "model_type",
        "meta_model",
        "effect_model",
        "method"
      )
    )
  )
)

executed_tau_methods <- unique(
  c(
    collect_unique_values(
      r27_summary_combined,
      c(
        "tau_method",
        "tau2_method",
        "tau_squared_estimator",
        "heterogeneity_estimator"
      )
    ),
    collect_unique_values(
      meta_execution_summary,
      c(
        "tau_method",
        "tau2_method",
        "tau_squared_estimator",
        "heterogeneity_estimator"
      )
    )
  )
)

executed_inference_methods <- unique(
  c(
    collect_unique_values(
      r27_summary_combined,
      c(
        "test",
        "inference_method",
        "ci_method",
        "hartung_knapp",
        "knapp_hartung"
      )
    ),
    collect_unique_values(
      meta_execution_summary,
      c(
        "test",
        "inference_method",
        "ci_method",
        "hartung_knapp",
        "knapp_hartung"
      )
    )
  )
)

model_features <- detect_model_features(
  unname(
    unlist(
      r27_model_outputs,
      use.names = FALSE
    )
  )
)

heterogeneity_metrics_present <- character(0)

if (nrow(r27_summary_combined) > 0L) {
  if (
    any(
      c(
        "heterogeneity_i2",
        "i2",
        "I2"
      ) %in%
        names(r27_summary_combined)
    )
  ) {
    heterogeneity_metrics_present <- c(
      heterogeneity_metrics_present,
      "I²"
    )
  }

  if (
    any(
      c(
        "heterogeneity_tau2",
        "tau2"
      ) %in%
        names(r27_summary_combined)
    )
  ) {
    heterogeneity_metrics_present <- c(
      heterogeneity_metrics_present,
      "τ²"
    )
  }

  if (
    any(
      c(
        "heterogeneity_q",
        "q",
        "Q"
      ) %in%
        names(r27_summary_combined)
    )
  ) {
    heterogeneity_metrics_present <- c(
      heterogeneity_metrics_present,
      "Cochran Q"
    )
  }

  if (
    any(
      c(
        "heterogeneity_p",
        "q_p",
        "QEp"
      ) %in%
        names(r27_summary_combined)
    )
  ) {
    heterogeneity_metrics_present <- c(
      heterogeneity_metrics_present,
      "heterogeneity P value"
    )
  }
}

config_statistical_items <- c(
  if (nzchar(binary_measure)) {
    paste0(
      "Binary effect measure configured: ",
      binary_measure
    )
  },
  if (nzchar(continuous_measure)) {
    paste0(
      "Continuous effect measure configured: ",
      continuous_measure
    )
  },
  if (nzchar(time_to_event_measure)) {
    paste0(
      "Time-to-event effect measure configured: ",
      time_to_event_measure
    )
  },
  if (nzchar(primary_model)) {
    paste0(
      "Primary model configured: ",
      primary_model
    )
  },
  if (nzchar(tau_method)) {
    paste0(
      "Between-study variance estimator configured: ",
      tau_method
    )
  },
  if (nzchar(hartung_knapp)) {
    paste0(
      "Hartung-Knapp setting configured: ",
      hartung_knapp
    )
  }
)

methods_lines <- c(
  "# Methods factual blocks",
  "",
  "> These blocks are deterministic summaries of canonical configuration and executed workflow artifacts. They are source material for manuscript drafting, not a final polished Methods section.",
  "> IMPORTANT: when configuration is silent, manuscript drafting must use executed R/27/R/28/R/28B provenance below rather than emitting [CONFIG NOT SPECIFIED]. Unsupported methods must be omitted, not invented.",
  "",
  "## Protocol and reporting",
  "",
  paste0(
    "- Project title: ",
    project_title
  ),
  paste0(
    "- Project ID: ",
    project_id
  ),
  paste0(
    "- Reporting guideline: ",
    reporting_guideline
  ),
  if (nzchar(registered_protocol)) {
    paste0(
      "- Registration identifier: ",
      registered_protocol
    )
  } else {
    "- Registration identifier: not populated in canonical project configuration; do not invent one."
  },
  "",
  "## Information sources",
  "",
  paste0(
    "- Enabled bibliographic databases: ",
    enabled_databases_text
  ),
  "- Exact search strategies, dates, counts, and database exports remain governed by the canonical search-stage artifacts.",
  "",
  "## Eligibility framework — canonical question dimensions",
  "",
  if (nrow(eligibility_dimensions) > 0L) {
    table_to_markdown(
      eligibility_dimensions,
      max_rows = 50L
    )
  } else {
    "_No structured eligibility dimensions were recoverable from project_config.yml. Use the canonical protocol/question artifacts; do not invent a PICO/PICOS structure._"
  },
  "",
  "## Study selection and extraction",
  "",
  "- Title/abstract screening and full-text eligibility were completed through the toolkit's staged review workflow with preserved human-review gates.",
  "- Quantitative analysis inputs were finalized only after targeted Human Verification.",
  "",
  "## Risk of bias",
  "",
  paste0(
    "- RoB tool(s) documented in canonical configuration and/or final executed RoB outputs: ",
    nonempty_or_none(
      rob_tools_present,
      none_text = "No RoB tool label was recoverable; manuscript drafting must use the final R/37 schema without guessing a tool name."
    )
  ),
  "- Final manuscript claims about risk of bias must use R/37 final author-approved outputs only; manuscript modules must not impose a tool based on review topic.",
  "",
  "## Statistical analysis — executed-method inventory",
  "",
  "- Canonical inferential engine: R/27. Manuscript-production modules do not refit any model.",
  paste0(
    "- Effect measure(s) actually present in canonical quantitative outputs: ",
    nonempty_or_none(
      executed_measures
    )
  ),
  paste0(
    "- Analysis family/families inferred from executed effect measures: ",
    if (nrow(analysis_family_inventory) > 0L) {
      paste(
        unique(analysis_family_inventory$analysis_family),
        collapse = "; "
      )
    } else {
      "No family-specific label could be inferred; retain the generic executed-effect description."
    }
  ),
  paste0(
    "- Model label(s) explicitly present in R/27 execution/summary outputs: ",
    nonempty_or_none(
      executed_models
    )
  ),
  paste0(
    "- Between-study variance estimator(s) explicitly present in executed outputs: ",
    nonempty_or_none(
      executed_tau_methods
    )
  ),
  paste0(
    "- Inference/CI adjustment labels explicitly present in executed outputs: ",
    nonempty_or_none(
      executed_inference_methods
    )
  ),
  paste0(
    "- Model-output feature(s) detected from canonical R/27 provenance: ",
    nonempty_or_none(
      model_features
    )
  ),
  paste0(
    "- Heterogeneity quantities available in canonical R/27 summaries: ",
    nonempty_or_none(
      heterogeneity_metrics_present,
      none_text = "No heterogeneity quantity was exposed in the available R/27 summary schema."
    )
  ),
  "- Pooled estimates are reported with the confidence intervals written by the canonical R/27 output; manuscript drafting must preserve those values and their precision.",
  "- R/28 diagnostic outputs, when present, are part of the executed analysis record and should be described only according to their recorded status/result.",
  "- R/28B exploratory outputs, when present, must retain their eligibility labels (for example STANDARD_ELIGIBLE, EXPLORATORY_UNDERPOWERED, DESCRIPTIVE_ONLY, NOT_ESTIMABLE, or CONTRACT_MISMATCH). Exploratory/underpowered analyses must never be upgraded to standard evidence in prose.",
  if (length(config_statistical_items) > 0L) {
    paste0(
      "- Additional configuration-declared statistical settings: ",
      paste(
        config_statistical_items,
        collapse = "; "
      )
    )
  } else {
    "- No separate analysis.* settings were populated in project_config.yml; executed canonical outputs below are the source of truth for methods actually used."
  },
  "",
  "### Executed analysis-family inventory",
  "",
  if (nrow(analysis_family_inventory) > 0L) {
    table_to_markdown(
      analysis_family_inventory,
      max_rows = 100L
    )
  } else {
    "_No effect-family inventory could be derived from executed R/27 summaries._"
  },
  "",
  "### Canonical R/27 execution summary",
  "",
  if (!is.null(meta_execution_summary)) {
    table_to_markdown(
      meta_execution_summary,
      max_rows = 100L
    )
  } else {
    "_No global R/27 execution-summary table was available._"
  },
  "",
  "### Canonical R/27 pooled-summary schema and values",
  "",
  if (nrow(r27_summary_combined) > 0L) {
    table_to_markdown(
      r27_summary_combined,
      max_rows = 100L
    )
  } else {
    "_No per-group R/27 pooled summary was available._"
  },
  "",
  "### Canonical R/27 model-output provenance",
  ""
)

if (length(r27_model_outputs) > 0L) {
  for (group_name in names(r27_model_outputs)) {
    methods_lines <- c(
      methods_lines,
      paste0(
        "#### ",
        group_name
      ),
      "",
      "```text",
      r27_model_outputs[[
        group_name
      ]],
      "```",
      ""
    )
  }
} else {
  methods_lines <- c(
    methods_lines,
    "_No R/27 model_output.txt provenance files were available._",
    ""
  )
}

methods_lines <- c(
  methods_lines,
  "### R/28 statistical-diagnostic execution record",
  "",
  if (!is.null(meta_diagnostics_summary)) {
    table_to_markdown(
      meta_diagnostics_summary,
      max_rows = 100L
    )
  } else {
    "_R/28 diagnostics were not available._"
  },
  "",
  "### R/28B exploratory-diagnostic execution record",
  "",
  if (!is.null(exploratory_summary)) {
    table_to_markdown(
      exploratory_summary,
      max_rows = 100L
    )
  } else {
    "_R/28B exploratory diagnostics were not run or not available._"
  },
  "",
  "### Statistical-analysis drafting rule",
  "",
  "- The final Statistical analysis subsection should be written at publication-manuscript depth: effect measures and data types; model and variance estimator; confidence-interval/inference method; heterogeneity assessment; sensitivity/diagnostic analyses; publication-bias or meta-regression procedures only if actually executed; thresholds/eligibility conditions when recorded; and software/workflow provenance when supported.",
  "- Do not emit [CONFIG NOT SPECIFIED]. If an optional method is absent from all canonical executed evidence, omit it from final prose rather than guessing.",
  "- Statistical recalculation in R/38: NO.",
  ""
)

writeLines(
  methods_lines,
  methods_blocks_file,
  useBytes = TRUE
)


# ============================================================
# 11. Deterministic Results factual blocks
# ============================================================

included_n <- if (
  !is.null(included_studies)
) {
  nrow(included_studies)
} else {
  NA_integer_
}

quantitative_group_n <- if (
  !is.null(meta_execution_summary)
) {
  nrow(meta_execution_summary)
} else if (
  nrow(r27_summary_combined) > 0L
) {
  nrow(r27_summary_combined)
} else {
  NA_integer_
}

build_meta_result_sentences <- function(
    summary_data
) {
  if (
    is.null(summary_data) ||
      nrow(summary_data) == 0L
  ) {
    return(
      "- No canonical R/27 meta-analysis summary was available."
    )
  }

  lines <- character(0)

  for (
    i in seq_len(
      nrow(summary_data)
    )
  ) {

    row <- summary_data[
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

    measure_col <- first_existing_col(
      row,
      c(
        "summary_measure",
        "effect_measure",
        "measure"
      )
    )

    k_col <- first_existing_col(
      row,
      c(
        "n_studies",
        "k",
        "studies"
      )
    )

    effect_col <- first_existing_col(
      row,
      c(
        "pooled_effect",
        "pooled_estimate",
        "estimate"
      )
    )

    lower_col <- first_existing_col(
      row,
      c(
        "pooled_lower_ci",
        "lower_ci",
        "lower"
      )
    )

    upper_col <- first_existing_col(
      row,
      c(
        "pooled_upper_ci",
        "upper_ci",
        "upper"
      )
    )

    i2_col <- first_existing_col(
      row,
      c(
        "heterogeneity_i2",
        "i2",
        "I2"
      )
    )

    tau2_col <- first_existing_col(
      row,
      c(
        "heterogeneity_tau2",
        "tau2"
      )
    )

    het_p_col <- first_existing_col(
      row,
      c(
        "heterogeneity_p",
        "q_p",
        "QEp"
      )
    )

    p_col <- first_existing_col(
      row,
      c(
        "p_value",
        "overall_p",
        "p"
      )
    )

    group_name <- if (!is.na(group_col)) {
      safe_chr(
        row[[group_col]],
        paste0(
          "Analysis ",
          i
        )
      )
    } else {
      paste0(
        "Analysis ",
        i
      )
    }

    measure <- if (!is.na(measure_col)) {
      safe_chr(
        row[[measure_col]],
        "effect"
      )
    } else {
      "effect"
    }

    k <- if (!is.na(k_col)) {
      suppressWarnings(
        as.integer(
          row[[k_col]][[1]]
        )
      )
    } else {
      NA_integer_
    }

    effect <- if (!is.na(effect_col)) {
      safe_num(
        row[[effect_col]]
      )[[1]]
    } else {
      NA_real_
    }

    lower <- if (!is.na(lower_col)) {
      safe_num(
        row[[lower_col]]
      )[[1]]
    } else {
      NA_real_
    }

    upper <- if (!is.na(upper_col)) {
      safe_num(
        row[[upper_col]]
      )[[1]]
    } else {
      NA_real_
    }

    i2 <- if (!is.na(i2_col)) {
      safe_num(
        row[[i2_col]]
      )[[1]]
    } else {
      NA_real_
    }

    tau2 <- if (!is.na(tau2_col)) {
      safe_num(
        row[[tau2_col]]
      )[[1]]
    } else {
      NA_real_
    }

    het_p <- if (!is.na(het_p_col)) {
      safe_num(
        row[[het_p_col]]
      )[[1]]
    } else {
      NA_real_
    }

    p_value <- if (!is.na(p_col)) {
      safe_num(
        row[[p_col]]
      )[[1]]
    } else {
      NA_real_
    }

    parts <- c(
      paste0(
        "- ",
        group_name,
        ":"
      )
    )

    if (!is.na(k)) {
      parts <- c(
        parts,
        paste0(
          "k=",
          k
        )
      )
    }

    if (!is.na(effect)) {
      effect_text <- paste0(
        measure,
        " ",
        format_num(
          effect
        )
      )

      if (
        !is.na(lower) &&
        !is.na(upper)
      ) {
        effect_text <- paste0(
          effect_text,
          " (95% CI ",
          format_num(
            lower
          ),
          " to ",
          format_num(
            upper
          ),
          ")"
        )
      }

      parts <- c(
        parts,
        effect_text
      )
    }

    if (!is.na(p_value)) {
      parts <- c(
        parts,
        paste0(
          "overall-effect P=",
          format_num(
            p_value
          )
        )
      )
    }

    if (!is.na(i2)) {
      parts <- c(
        parts,
        paste0(
          "I²=",
          format_num(
            i2
          ),
          "%"
        )
      )
    }

    if (!is.na(tau2)) {
      parts <- c(
        parts,
        paste0(
          "τ²=",
          format_num(
            tau2
          )
        )
      )
    }

    if (!is.na(het_p)) {
      parts <- c(
        parts,
        paste0(
          "heterogeneity P=",
          format_num(
            het_p
          )
        )
      )
    }

    lines <- c(
      lines,
      paste(
        parts,
        collapse = "; "
      )
    )
  }

  lines
}

meta_result_lines <- build_meta_result_sentences(
  r27_summary_combined
)

publication_figures <- artifact_registry |>
  dplyr::filter(
    artifact_type == "figure",
    source_status == "AVAILABLE"
  ) |>
  dplyr::arrange(
    factor(
      manuscript_priority,
      levels = c(
        "primary",
        "supporting",
        "supplementary"
      )
    ),
    title
  )

figure_manifest_lines <- c(
  "## Figure-aware evidence map — dynamically discovered",
  "",
  "> Figures are discovered from canonical result directories rather than from topic-specific names. R/39 must cover every semantic evidence unit represented by an artifact with coverage_required=TRUE and must use canonical backing data rather than visual guesswork.",
  ""
)

if (nrow(publication_figures) > 0L) {
  for (i in seq_len(nrow(publication_figures))) {
    row <- publication_figures[
      i,
      ,
      drop = FALSE
    ]

    figure_manifest_lines <- c(
      figure_manifest_lines,
      paste0(
        "- ",
        row$coverage_token[[1]],
        " **",
        row$title[[1]],
        "** — source: `",
        row$source_relative_path[[1]],
        "`; role=",
        row$manuscript_role[[1]],
        "; priority=",
        row$manuscript_priority[[1]],
        "; coverage_required=",
        row$coverage_required[[1]],
        if (nzchar(row$analysis_group[[1]])) {
          paste0(
            "; analysis_group=",
            row$analysis_group[[1]]
          )
        } else {
          ""
        },
        "."
      )
    )
  }
} else {
  figure_manifest_lines <- c(
    figure_manifest_lines,
    "_No publication-oriented figure artifact was discovered._"
  )
}

figure_manifest_lines <- c(
  figure_manifest_lines,
  ""
)

results_lines <- c(
  "# Results factual blocks",
  "",
  "> These are deterministic factual blocks derived from canonical upstream artifacts. They are not a final narrative Results section.",
  "> R/39 must review every table/figure evidence subsection below and convert it into a complete Results narrative. It must not ignore a table or primary figure simply because a short summary sentence is already available.",
  "",
  "## Study selection",
  "",
  if (!is.na(included_n)) {
    paste0(
      "- Final included reports/studies represented in the canonical included-studies table: ",
      included_n,
      "."
    )
  } else {
    "- Final included-study count was not available."
  },
  "- PRISMA counts must be taken from the canonical R/35 artifacts below; they are not manually reconstructed.",
  "",
  "### PRISMA source registry",
  "",
  if (!is.null(prisma_registry)) {
    table_to_markdown(
      prisma_registry,
      max_rows = 100L
    )
  } else {
    "_PRISMA source registry was not available._"
  },
  "",
  "### PRISMA audit",
  "",
  if (!is.null(prisma_audit)) {
    table_to_markdown(
      prisma_audit,
      max_rows = 100L
    )
  } else {
    "_PRISMA audit was not available._"
  },
  "",
  "## Descriptive/reporting tables — dynamically discovered",
  "",
  "_R/39 must describe the substantive patterns in every table artifact marked coverage_required=TRUE, regardless of topic or review type._",
  "",
  "## Quantitative synthesis",
  "",
  if (!is.na(quantitative_group_n)) {
    paste0(
      "- Canonical quantitative analysis groups: ",
      quantitative_group_n,
      "."
    )
  } else {
    "- Canonical quantitative analysis-group count was not available."
  },
  meta_result_lines,
  "",
  "### R/27 pooled summary — full canonical table",
  "",
  if (nrow(r27_summary_combined) > 0L) {
    table_to_markdown(
      r27_summary_combined,
      max_rows = 100L
    )
  } else {
    "_No canonical R/27 pooled summary was available._"
  },
  ""
)

# Prefer atomic publication tables (CSV/TSV) as the canonical textual
# evidence supplied to R/39. Workbook containers are delivery/editing
# artifacts and may contain duplicate table sheets plus audit/instruction
# sheets. Feeding both the atomic tables and workbook containers into the
# Results dossier can duplicate evidence or reintroduce stale/noncanonical
# content. Use a single workbook only as a fallback when no atomic table
# files are available.
available_atomic_tables <- artifact_registry |>
  dplyr::filter(
    artifact_type == "table",
    source_status == "AVAILABLE"
  )

if (nrow(available_atomic_tables) > 0L) {

  publication_tables <- available_atomic_tables
  table_evidence_mode <- "ATOMIC_TABLE_FILES"

} else {

  available_table_containers <- artifact_registry |>
    dplyr::filter(
      artifact_type == "table_container",
      source_status == "AVAILABLE"
    ) |>
    dplyr::mutate(
      .container_rank = dplyr::case_when(
        grepl(
          "manuscript_tables_author_editable\\.xlsx$",
          source_relative_path,
          ignore.case = TRUE
        ) ~ 1L,
        grepl(
          "manuscript_tables_generated\\.xlsx$",
          source_relative_path,
          ignore.case = TRUE
        ) ~ 2L,
        TRUE ~ 3L
      )
    ) |>
    dplyr::arrange(
      .container_rank,
      source_relative_path
    )

  publication_tables <- if (nrow(available_table_containers) > 0L) {
    available_table_containers |>
      dplyr::slice(1L) |>
      dplyr::select(-.container_rank)
  } else {
    artifact_registry[0, , drop = FALSE]
  }

  table_evidence_mode <- if (nrow(publication_tables) > 0L) {
    "SINGLE_WORKBOOK_FALLBACK"
  } else {
    "NO_TABLE_EVIDENCE"
  }
}

publication_tables <- publication_tables |>
  dplyr::arrange(
    factor(
      manuscript_priority,
      levels = c(
        "primary",
        "supporting",
        "supplementary"
      )
    ),
    title
  )

if (nrow(publication_tables) > 0L) {
  table_lines <- c(
    "## Canonical table evidence — dynamic artifact registry",
    "",
    paste0(
      "- Table evidence selection mode: ",
      table_evidence_mode,
      "."
    ),
    ""
  )

  for (i in seq_len(nrow(publication_tables))) {
    row <- publication_tables[
      i,
      ,
      drop = FALSE
    ]

    table_lines <- c(
      table_lines,
      paste0(
        "### ",
        row$title[[1]],
        " [",
        row$artifact_id[[1]],
        "]"
      ),
      "",
      row$coverage_token[[1]],
      paste0(
        "- Source: `",
        row$source_relative_path[[1]],
        "`"
      ),
      paste0(
        "- Role: ",
        row$manuscript_role[[1]],
        "; priority: ",
        row$manuscript_priority[[1]],
        "; coverage_required: ",
        row$coverage_required[[1]],
        "."
      )
    )

    table_payloads <- read_publication_table(
      row$source_path[[1]]
    )

    if (length(table_payloads) == 0L) {
      table_lines <- c(
        table_lines,
        "- Tabular content could not be rendered into the text dossier; use the frozen source artifact directly.",
        ""
      )
      next
    }

    for (sheet_name in names(table_payloads)) {
      data <- table_payloads[[sheet_name]]

      table_lines <- c(
        table_lines,
        if (
          length(table_payloads) > 1L ||
            !identical(sheet_name, "table")
        ) {
          paste0(
            "#### Sheet/data block: ",
            sheet_name
          )
        } else {
          ""
        },
        if (
          length(table_payloads) > 1L ||
            !identical(sheet_name, "table")
        ) {
          ""
        } else {
          character(0)
        },
        paste0(
          "- Rows: ",
          nrow(data),
          "; columns: ",
          ncol(data),
          "."
        ),
        "- Describe informative patterns faithfully. Do not create inferential statistics that are absent from the table.",
        "",
        table_to_markdown(
          data,
          max_rows = 150L
        ),
        ""
      )
    }
  }

  results_lines <- c(
    results_lines,
    table_lines
  )
}

if (!is.null(meta_reporting_summary)) {
  results_lines <- c(
    results_lines,
    "### R/27 publication/reporting summary table",
    "",
    table_to_markdown(
      meta_reporting_summary,
      max_rows = 100L
    ),
    ""
  )
}

if (!is.null(study_level_effects)) {
  results_lines <- c(
    results_lines,
    "### R/27 global study-level effects table",
    "",
    table_to_markdown(
      study_level_effects,
      max_rows = 150L
    ),
    ""
  )
}

if (length(r27_study_effect_tables) > 0L) {
  for (group_name in names(r27_study_effect_tables)) {
    results_lines <- c(
      results_lines,
      paste0(
        "### Forest-plot backing data — ",
        group_name
      ),
      "",
      table_to_markdown(
        r27_study_effect_tables[[
          group_name
        ]],
        max_rows = 150L
      ),
      ""
    )
  }
}

results_lines <- c(
  results_lines,
  "## Statistical diagnostics and sensitivity analyses",
  "",
  "### R/28 diagnostic summary",
  "",
  if (!is.null(meta_diagnostics_summary)) {
    table_to_markdown(
      meta_diagnostics_summary,
      max_rows = 100L
    )
  } else {
    "_R/28 diagnostic summary was not available._"
  },
  "",
  "### R/28B exploratory diagnostics",
  "",
  if (!is.null(exploratory_summary)) {
    table_to_markdown(
      exploratory_summary,
      max_rows = 100L
    )
  } else {
    "_R/28B exploratory diagnostics were not run or not available._"
  },
  "",
  "## Risk of bias",
  "",
  if (!is.null(rob_final)) {
    c(
      paste0(
        "- Final author-approved RoB rows available: ",
        nrow(rob_final),
        "."
      ),
      "",
      table_to_markdown(
        rob_final,
        max_rows = 150L
      )
    )
  } else {
    "_Final author-approved RoB was not available._"
  },
  "",
  figure_manifest_lines,
  "## Results-drafting rules",
  "",
  "- Study selection: report canonical PRISMA counts and refer to the PRISMA figure when available.",
  "- Tables 1–3: describe the study-design mix, sample-size structure, participant characteristics, and intervention/procedural characteristics that are actually visible in the tables. Select the most informative patterns rather than reciting every cell.",
  "- Quantitative outcomes: for each canonical analysis, report k and total N when available, the effect measure and pooled estimate, 95% CI, and heterogeneity quantities available in the canonical output. State whether the interval excludes the null only when that conclusion follows directly from the measure.",
  "- Forest plots: describe the pooled result and the study-level pattern/precision using the backing R/27 data. Do not infer a direction such as 'favored intervention' unless outcome coding and group direction are explicit in the evidence.",
  "- Sensitivity/diagnostic analyses: report only analyses actually executed. Preserve underpowered/descriptive/not-estimable labels exactly.",
  "- Risk of bias: summarize the author-approved distribution/pattern from R/37 and refer to the corresponding figure(s) when available.",
  "- Results may statistically interpret precision, heterogeneity, robustness, and whether a confidence interval crosses the null. Mechanistic, causal, or clinical explanations belong in Discussion.",
  "- Statistical recalculation in R/38: NO.",
  ""
)

writeLines(
  results_lines,
  results_blocks_file,
  useBytes = TRUE
)


# ============================================================
# 12. Author-task list
# ============================================================

author_tasks <- character(0)

if (!nzchar(registered_protocol)) {
  author_tasks <- c(
    author_tasks,
    "Confirm and insert the final PROSPERO/registration identifier if applicable."
  )
}

if (!rob_final_available) {
  author_tasks <- c(
    author_tasks,
    "Complete Author-final risk-of-bias review and run R/37 before final manuscript freeze."
  )
}

if (nrow(final_missing) > 0L) {
  author_tasks <- c(
    author_tasks,
    paste0(
      "Resolve final-stage missing artifact(s): ",
      paste(
        final_missing$logical_name,
        collapse = ", "
      ),
      "."
    )
  )
}

author_tasks <- c(
  author_tasks,
  "Draft Introduction using external literature and cite each background/novelty claim.",
  "Draft Discussion using the frozen results plus external literature; clearly separate current-study findings from interpretation.",
  "Confirm target journal, word limits, reference style, abstract structure, and figure/table limits.",
  "Review every manuscript number against the frozen evidence package before submission.",
  "Complete funding, conflicts of interest, author contributions, data availability, acknowledgments, and AI-use statements as required by the target journal."
)

author_task_lines <- c(
  "# Author tasks",
  "",
  paste0(
    "- Manuscript readiness: **",
    readiness_status,
    "**"
  ),
  "",
  paste0(
    seq_along(author_tasks),
    ". ",
    author_tasks
  ),
  ""
)

writeLines(
  author_task_lines,
  author_tasks_file,
  useBytes = TRUE
)


# ============================================================
# 13. Manuscript scaffold
# ============================================================

scaffold_lines <- c(
  paste0(
    "# ",
    project_title
  ),
  "",
  paste0(
    "**Manuscript evidence state:** ",
    readiness_status
  ),
  "",
  "> This scaffold is generated from frozen SR-MA artifacts. Bracketed instructions identify text that requires author/agent drafting or verification. Canonical numerical results must not be changed manually.",
  "",
  "## Abstract",
  "",
  "### Background",
  "[DRAFT AFTER INTRODUCTION/DISCUSSION ARE STABLE]",
  "",
  "### Methods",
  "[SYNTHESIZE FROM methods_factual_blocks.md]",
  "",
  "### Results",
  "[SYNTHESIZE ONLY FROM results_factual_blocks.md AND FROZEN TABLES]",
  "",
  "### Conclusions",
  "[AUTHOR INTERPRETATION AFTER FINAL RoB REVIEW]",
  "",
  "## Introduction",
  "",
  "[AUTHOR/AGENT DRAFTING STEP — requires external literature search and references.]",
  "",
  "Suggested structure:",
  "1. Clinical/problem context.",
  "2. Current standard and emerging alternative(s).",
  "3. Evidence gap / why this review is needed now.",
  "4. Review objective and prespecified question.",
  "",
  "## Methods",
  "",
  "### Protocol and registration",
  "[DRAFT FROM methods_factual_blocks.md + registration record]",
  "",
  "### Eligibility criteria",
  "[DRAFT FROM canonical protocol/config]",
  "",
  "### Information sources and search strategy",
  "[DRAFT FROM canonical search strategies and search logs]",
  "",
  "### Study selection",
  "[DRAFT FROM screening workflow; preserve human-review gates]",
  "",
  "### Data extraction and verification",
  "[DRAFT FROM extraction workflow and Human Verification contract]",
  "",
  "### Outcomes and effect measures",
  "[DRAFT FROM config + Human-verified analysis datasets]",
  "",
  "### Risk-of-bias assessment",
  "[DRAFT FROM RoB tool config; final judgments from R/37 only]",
  "",
  "### Statistical analysis",
  "[DRAFT FROM R/27 model/config; do not infer settings not present in canonical sources]",
  "",
  "### Additional and exploratory analyses",
  "[REPORT ONLY analyses actually executed. R/28B results must retain their eligibility labels such as DESCRIPTIVE_ONLY or NOT_ESTIMABLE.]",
  "",
  "## Results",
  "",
  "### Study selection",
  "[USE R/35 PRISMA counts + included_studies.csv]",
  "",
  "### Study characteristics",
  "[USE R/34 manuscript tables]",
  "",
  "### Quantitative synthesis",
  "[USE R/27 canonical pooled results; R/33 only for presentation-ready figures/tables]",
  "",
  "### Statistical diagnostics and sensitivity analyses",
  "[USE R/28; optionally R/28B with explicit limitations]",
  "",
  "### Risk of bias",
  if (rob_final_available) {
    "[USE R/37 final author-approved RoB outputs]"
  } else {
    "[PENDING R/37 — DO NOT WRITE FINAL RoB CONCLUSIONS YET]"
  },
  "",
  "## Discussion",
  "",
  "[AUTHOR/AGENT DRAFTING STEP — use the frozen current-study findings plus external literature.]",
  "",
  "Suggested structure:",
  "1. Principal findings.",
  "2. Interpretation and comparison with prior evidence.",
  "3. Clinical implications.",
  "4. Evidence gaps / future research.",
  "5. Strengths.",
  "6. Limitations.",
  "",
  "## Conclusions",
  "",
  "[AUTHOR FINAL INTERPRETATION — must reflect uncertainty and RoB.]",
  "",
  "## Declarations",
  "",
  "### Funding",
  "[AUTHOR REQUIRED]",
  "",
  "### Conflicts of interest",
  "[AUTHOR REQUIRED]",
  "",
  "### Author contributions",
  "[AUTHOR REQUIRED]",
  "",
  "### Data availability",
  "[AUTHOR REQUIRED]",
  "",
  "### Acknowledgments",
  "[IF APPLICABLE]",
  "",
  "### Use of AI-assisted tools",
  "[FOLLOW TARGET JOURNAL POLICY]",
  "",
  "## Tables and Figures",
  "",
  "[USE manuscript_artifact_registry.csv. Do not assume a fixed number, title, or topic-specific type of table/figure.]",
  "",
  "- Every artifact with coverage_required=TRUE must be represented in the Results narrative using its exact coverage token.",
  "- Artifacts marked supporting/supplementary should be described only to the extent supported by their canonical backing data and manuscript role.",
  "- The registry may represent intervention reviews, diagnostic reviews, prevalence/proportion reviews, prognostic/exposure reviews, time-to-event reviews, or other supported quantitative review structures without changing this manuscript module.",
  ""
)

writeLines(
  scaffold_lines,
  scaffold_file,
  useBytes = TRUE
)


# ============================================================
# 14. Evidence-map workbook
# ============================================================

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb,
  "Evidence_Registry"
)

openxlsx::writeData(
  wb,
  "Evidence_Registry",
  evidence_registry
)

openxlsx::addWorksheet(
  wb,
  "Freeze_Manifest"
)

openxlsx::writeData(
  wb,
  "Freeze_Manifest",
  freeze_manifest
)

if (!is.null(included_studies)) {
  openxlsx::addWorksheet(
    wb,
    "Included_Studies"
  )
  openxlsx::writeData(
    wb,
    "Included_Studies",
    included_studies
  )
}

if (!is.null(prisma_registry)) {
  openxlsx::addWorksheet(
    wb,
    "PRISMA_Source_Registry"
  )
  openxlsx::writeData(
    wb,
    "PRISMA_Source_Registry",
    prisma_registry
  )
}

if (!is.null(prisma_audit)) {
  openxlsx::addWorksheet(
    wb,
    "PRISMA_Audit"
  )
  openxlsx::writeData(
    wb,
    "PRISMA_Audit",
    prisma_audit
  )
}

openxlsx::addWorksheet(
  wb,
  "Artifact_Registry"
)
openxlsx::writeData(
  wb,
  "Artifact_Registry",
  artifact_registry
)

publication_table_rows <- artifact_registry |>
  dplyr::filter(
    artifact_type == "table",
    source_status == "AVAILABLE"
  )

if (nrow(publication_table_rows) > 0L) {
  for (i in seq_len(nrow(publication_table_rows))) {
    row <- publication_table_rows[
      i,
      ,
      drop = FALSE
    ]

    payloads <- read_publication_table(
      row$source_path[[1]]
    )

    if (length(payloads) == 0L) {
      next
    }

    data <- payloads[[1]]

    sheet_name <- substr(
      paste0(
        row$artifact_id[[1]],
        "_",
        slugify(
          humanize_artifact_name(
            row$title[[1]]
          )
        )
      ),
      1L,
      31L
    )

    if (sheet_name %in% names(wb)) {
      next
    }

    openxlsx::addWorksheet(
      wb,
      sheet_name
    )
    openxlsx::writeData(
      wb,
      sheet_name,
      data
    )
  }
}

if (nrow(r27_summary_combined) > 0L) {
  openxlsx::addWorksheet(
    wb,
    "R27_Meta_Summary"
  )
  openxlsx::writeData(
    wb,
    "R27_Meta_Summary",
    r27_summary_combined
  )
}

if (!is.null(meta_diagnostics_summary)) {
  openxlsx::addWorksheet(
    wb,
    "R28_Diagnostics"
  )
  openxlsx::writeData(
    wb,
    "R28_Diagnostics",
    meta_diagnostics_summary
  )
}

if (!is.null(exploratory_summary)) {
  openxlsx::addWorksheet(
    wb,
    "R28B_Exploratory"
  )
  openxlsx::writeData(
    wb,
    "R28B_Exploratory",
    exploratory_summary
  )
}

if (!is.null(rob_final)) {
  openxlsx::addWorksheet(
    wb,
    "R37_Final_RoB"
  )
  openxlsx::writeData(
    wb,
    "R37_Final_RoB",
    rob_final
  )
}

task_table <- tibble::tibble(
  task_number = seq_along(
    author_tasks
  ),
  task = author_tasks,
  status = "OPEN"
)

openxlsx::addWorksheet(
  wb,
  "Author_Tasks"
)

openxlsx::writeData(
  wb,
  "Author_Tasks",
  task_table
)

state_table <- tibble::tibble(
  project_id = project_id,
  project_title = project_title,
  readiness_status = readiness_status,
  frozen_at = now_text(),
  draft_required_missing = nrow(
    draft_missing
  ),
  final_required_missing = nrow(
    final_missing
  ),
  final_rob_available = rob_final_available
)

openxlsx::addWorksheet(
  wb,
  "State"
)

openxlsx::writeData(
  wb,
  "State",
  state_table
)

openxlsx::saveWorkbook(
  wb,
  evidence_map_file,
  overwrite = TRUE
)


# ============================================================
# 15. State file
# ============================================================

state <- list(
  manuscript_production = list(
    version = "2.1.2",
    generated_at = now_text(),
    project_id = project_id,
    project_title = project_title,
    status = readiness_status,
    draft_required_missing = as.list(
      draft_missing$logical_name
    ),
    final_required_missing = as.list(
      final_missing$logical_name
    ),
    final_rob_available = rob_final_available,
    canonical_statistical_engine = "R/27",
    canonical_prisma_module = "R/35",
    canonical_final_rob_module = "R/37",
    manuscript_module_recalculates_statistics = FALSE,
    evidence_registry = relative_to_root(
      registry_file,
      repo_root
    ),
    artifact_registry = relative_to_root(
      artifact_registry_file,
      repo_root
    ),
    freeze_manifest = relative_to_root(
      freeze_manifest_file,
      repo_root
    ),
    evidence_map = relative_to_root(
      evidence_map_file,
      repo_root
    ),
    manuscript_scaffold = relative_to_root(
      scaffold_file,
      repo_root
    )
  )
)

yaml::write_yaml(
  state,
  state_file
)


# ============================================================
# 16. Final validation
# ============================================================

required_outputs <- c(
  registry_file,
  freeze_manifest_file,
  evidence_map_file,
  methods_blocks_file,
  results_blocks_file,
  scaffold_file,
  author_tasks_file,
  state_file
)

output_ok <- vapply(
  required_outputs,
  file_nonempty,
  logical(1)
)

if (!all(output_ok)) {
  stop(
    "R/38 failed to create required manuscript-production artifact(s): ",
    paste(
      basename(
        required_outputs[
          !output_ok
        ]
      ),
      collapse = ", "
    ),
    call. = FALSE
  )
}

if (
  nrow(freeze_manifest) > 0L &&
  any(!freeze_manifest$hash_match)
) {
  stop(
    "R/38 evidence freeze manifest contains a hash mismatch.",
    call. = FALSE
  )
}


# ============================================================
# 17. Report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/38 — Manuscript Evidence Package",
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
  "",
  "ARCHITECTURAL CONTRACT",
  "--------------------------------------------------",
  "Canonical statistical engine: R/27",
  "Canonical PRISMA source: R/35",
  "Canonical final RoB source: R/37",
  "Statistical recalculation in R/38: NO",
  "External-literature claims generated in R/38: NO",
  "",
  "READINESS",
  "--------------------------------------------------",
  paste0(
    "Status: ",
    readiness_status
  ),
  paste0(
    "Draft-required missing artifacts: ",
    nrow(draft_missing)
  ),
  if (
    nrow(draft_missing) > 0L
  ) {
    paste0(
      "  - ",
      paste(
        draft_missing$logical_name,
        collapse = ", "
      )
    )
  } else {
    "  - none"
  },
  paste0(
    "Final-required missing artifacts: ",
    nrow(final_missing)
  ),
  if (
    nrow(final_missing) > 0L
  ) {
    paste0(
      "  - ",
      paste(
        final_missing$logical_name,
        collapse = ", "
      )
    )
  } else {
    "  - none"
  },
  paste0(
    "Final R/37 RoB available: ",
    rob_final_available
  ),
  "",
  "FREEZE",
  "--------------------------------------------------",
  paste0(
    "Available artifacts frozen: ",
    sum(
      evidence_registry$copied
    )
  ),
  paste0(
    "SHA-256 verified copies: ",
    sum(
      evidence_registry$hash_match
    )
  ),
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  paste0(
    "Evidence registry: ",
    normalize_slash(
      registry_file
    )
  ),
  paste0(
    "Artifact registry: ",
    normalize_slash(
      artifact_registry_file
    )
  ),
  paste0(
    "Freeze manifest: ",
    normalize_slash(
      freeze_manifest_file
    )
  ),
  paste0(
    "Evidence map workbook: ",
    normalize_slash(
      evidence_map_file
    )
  ),
  paste0(
    "Methods factual blocks: ",
    normalize_slash(
      methods_blocks_file
    )
  ),
  paste0(
    "Results factual blocks: ",
    normalize_slash(
      results_blocks_file
    )
  ),
  paste0(
    "Manuscript scaffold: ",
    normalize_slash(
      scaffold_file
    )
  ),
  paste0(
    "Author tasks: ",
    normalize_slash(
      author_tasks_file
    )
  ),
  paste0(
    "State: ",
    normalize_slash(
      state_file
    )
  ),
  "",
  "NEXT",
  "--------------------------------------------------",
  if (
    identical(
      readiness_status,
      "NOT_READY"
    )
  ) {
    "Resolve missing draft-critical evidence before manuscript drafting."
  } else if (
    identical(
      readiness_status,
      "DRAFT_READY_PENDING_ROB"
    )
  ) {
    "Draft Introduction/Methods/Results in parallel, but do not finalize RoB-dependent Results/Discussion until R/37 is complete; then rerun R/38."
  } else {
    "Proceed to manuscript drafting from the frozen evidence package, followed by numerical/reference consistency validation."
  }
)

writeLines(
  report_lines,
  report_file,
  useBytes = TRUE
)


# ============================================================
# 18. Console summary
# ============================================================

cat(
  "\n============================================================\n",
  "R/38 — Manuscript Evidence Package\n",
  "============================================================\n\n",
  sep = ""
)

cat(
  "Project: ",
  project_title,
  "\n",
  "Status: ",
  readiness_status,
  "\n",
  "Frozen artifacts: ",
  sum(
    evidence_registry$copied
  ),
  "\n",
  "Hash-verified copies: ",
  sum(
    evidence_registry$hash_match
  ),
  "\n",
  "Statistical recalculation: NO\n",
  sep = ""
)

cat(
  "\nEvidence package: ",
  normalize_slash(
    package_root
  ),
  "\n",
  "Evidence map: ",
  normalize_slash(
    evidence_map_file
  ),
  "\n",
  "Manuscript scaffold: ",
  normalize_slash(
    scaffold_file
  ),
  "\n",
  "Author tasks: ",
  normalize_slash(
    author_tasks_file
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
    readiness_status,
    "NOT_READY"
  )
) {
  cat(
    "\nWARNING: Manuscript drafting is NOT READY because draft-critical evidence is missing.\n"
  )
} else if (
  identical(
    readiness_status,
    "DRAFT_READY_PENDING_ROB"
  )
) {
  cat(
    "\nPASS: Draft manuscript work may begin, but final RoB-dependent text remains pending R/37.\n"
  )
} else {
  cat(
    "\nPASS: Final manuscript evidence package is ready for drafting and validation.\n"
  )
}
