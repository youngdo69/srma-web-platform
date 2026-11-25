# ============================================================
# Universal SR-MA Agent Toolkit
# R/30_audit_portability_paths.R
#
# TASK-030 — Portability & User-independent Path Audit
# Revision v4:
#   - Audit SOURCE / CONFIG / TEMPLATE files only
#   - Exclude generated data, results, logs, project outputs
#   - Avoid false positives from provenance/output files that
#     legitimately record the current absolute repository path
#   - Exclude R/30 itself from scanning because it intentionally
#     contains the path-detection patterns
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "purrr"
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
    x,
    "\\\\",
    "/"
  )
}

safe_read_lines <- function(path) {
  tryCatch(
    readLines(
      path,
      warn = FALSE,
      encoding = "UTF-8"
    ),
    error = function(e) {
      character(0)
    }
  )
}

relative_to_root <- function(path, repo_root) {

  normalized_path <- normalize_slash(path)
  normalized_root <- normalize_slash(repo_root)

  prefix <- paste0(
    normalized_root,
    "/"
  )

  if (startsWith(normalized_path, prefix)) {
    return(
      substring(
        normalized_path,
        nchar(prefix) + 1L
      )
    )
  }

  normalized_path
}


# ============================================================
# 2. Repository root
# ============================================================

repo_root <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)

if (!file.exists(file.path(repo_root, "config", "project_config.yml"))) {
  stop(
    paste0(
      "config/project_config.yml was not found.\n",
      "Run R/30 from the repository root.\n",
      "Current directory: ",
      repo_root
    ),
    call. = FALSE
  )
}


# ============================================================
# 3. Output
# ============================================================

result_dir <- file.path(
  repo_root,
  "results",
  "portability"
)

dir.create(
  result_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

findings_csv <- file.path(
  result_dir,
  "path_audit_findings.csv"
)

report_txt <- file.path(
  result_dir,
  "path_audit_report.txt"
)


# ============================================================
# 4. Canonical audit scope
# ============================================================

# Directories that contain executable/configurable toolkit material.
source_roots <- c(
  "R",
  "config",
  "scripts",
  "skills",
  "harnesses",
  "templates",
  "governance",
  "agents",
  "tasks",
  "plans",
  "reviews"
)

# Root-level source/governance/documentation files.
root_files <- c(
  "README.md",
  "AGENTS.md",
  "CLAUDE.md",
  ".env.example",
  ".gitignore"
)

# Text-like extensions worth auditing for hard-coded paths.
allowed_extensions <- c(
  "r",
  "rmd",
  "qmd",
  "md",
  "txt",
  "yml",
  "yaml",
  "json",
  "toml",
  "ini",
  "cfg",
  "conf",
  "ps1",
  "sh",
  "bat",
  "cmd",
  "py",
  "js",
  "ts"
)

# Generated/runtime content must NOT determine portability PASS/FAIL.
# These may legitimately record an absolute path for provenance.
excluded_roots <- c(
  ".git",
  ".Rproj.user",
  "renv",
  "data",
  "results",
  "logs",
  "projects",
  "output",
  "outputs",
  "tmp",
  "temp",
  "cache",
  "node_modules"
)


collect_source_files <- function() {

  collected <- character(0)

  for (root in source_roots) {

    root_path <- file.path(
      repo_root,
      root
    )

    if (!dir.exists(root_path)) {
      next
    }

    files <- list.files(
      root_path,
      recursive = TRUE,
      full.names = TRUE,
      all.files = TRUE,
      include.dirs = FALSE
    )

    collected <- c(
      collected,
      files
    )
  }

  for (f in root_files) {

    p <- file.path(
      repo_root,
      f
    )

    if (file.exists(p)) {
      collected <- c(
        collected,
        p
      )
    }
  }

  collected <- unique(
    collected
  )

  if (length(collected) == 0L) {
    return(character(0))
  }

  relative <- vapply(
    collected,
    relative_to_root,
    repo_root = repo_root,
    FUN.VALUE = character(1)
  )

  first_component <- vapply(
    strsplit(
      normalize_slash(relative),
      "/",
      fixed = TRUE
    ),
    function(x) {
      if (length(x) == 0L) "" else x[[1]]
    },
    FUN.VALUE = character(1)
  )

  keep <- !tolower(first_component) %in% tolower(excluded_roots)

  collected <- collected[keep]

  ext <- tolower(
    tools::file_ext(
      collected
    )
  )

  # Keep root files with no extension only if explicitly listed.
  explicit_root <- basename(collected) %in% basename(root_files)

  collected[
    ext %in% allowed_extensions |
      explicit_root
  ]
}

audit_files <- collect_source_files()

# Exclude this audit script itself. It intentionally contains the exact
# path patterns being searched for (e.g., /Users/<name>, /home/<name>),
# so scanning itself would create deterministic false positives.
audit_files <- audit_files[
  basename(audit_files) != "30_audit_portability_paths.R"
]

if (length(audit_files) == 0L) {
  stop(
    "No source/config files were found for portability audit.",
    call. = FALSE
  )
}


# ============================================================
# 5. Non-portable path patterns
# ============================================================

patterns <- tibble::tribble(
  ~rule_id, ~category, ~pattern, ~description,

  "P001",
  "windows_user_absolute",
  "(?i)[A-Z]:[/\\\\]Users[/\\\\][^/\\\\\\s\"']+",
  "Hard-coded Windows user directory",

  "P002",
  "macos_user_absolute",
  "(?i)/Users/[^/\\s\"']+",
  "Hard-coded macOS user directory",

  "P003",
  "linux_home_absolute",
  "(?i)/home/[^/\\s\"']+",
  "Hard-coded Linux home directory",

  "P004",
  "windows_drive_workspace",
  "(?i)[A-Z]:[/\\\\](Projects|Research|Workspace|Workspaces|Code|Repos|Repositories)[/\\\\]",
  "Hard-coded drive-level workspace/project path",

  "P005",
  "personal_folder_absolute",
  "(?i)[A-Z]:[/\\\\]Users[/\\\\][^/\\\\\\s\"']+[/\\\\](Desktop|Downloads|Documents|OneDrive)",
  "Hard-coded Windows personal folder",

  "P006",
  "known_dev_machine_path",
  "(?i)(C:[/\\\\]Users[/\\\\]Jason|/Users/Jason|/home/Jason)",
  "Known development-machine user path"
)


# ============================================================
# 6. Context exclusions for legitimate examples
# ============================================================

# Generic placeholders/documentation examples should not fail audit.
placeholder_pattern <- paste0(
  "(?i)(",
  "<username>|<user>|",
  "YOUR_USERNAME|YOUR_USER|",
  "\\{username\\}|\\{user\\}|",
  "%USERNAME%|",
  "\\$USER|",
  "example_user|sample_user",
  ")"
)

# Lines explicitly discussing/removing/testing hard-coded paths are
# documentation/test context, not operational hard-coding.
non_operational_context_pattern <- paste0(
  "(?i)(",
  "example|for example|e\\.g\\.|",
  "do not use|must not use|",
  "hard[- ]?coded|hardcode|",
  "portability|audit|detect|scan|pattern|",
  "remove.*absolute path|",
  "user-independent",
  ")"
)


# ============================================================
# 7. Scan source/config files
# ============================================================

scan_one_file <- function(path) {

  lines <- safe_read_lines(path)

  if (length(lines) == 0L) {
    return(NULL)
  }

  relative_path <- relative_to_root(
    path,
    repo_root
  )

  hits <- list()

  for (i in seq_len(nrow(patterns))) {

    idx <- which(
      stringr::str_detect(
        lines,
        stringr::regex(
          patterns$pattern[[i]]
        )
      )
    )

    if (length(idx) == 0L) {
      next
    }

    for (line_no in idx) {

      line_text <- stringr::str_squish(
        lines[[line_no]]
      )

      # Ignore clearly generic/documentation examples.
      if (
        stringr::str_detect(
          line_text,
          stringr::regex(
            placeholder_pattern
          )
        )
      ) {
        next
      }

      if (
        stringr::str_detect(
          line_text,
          stringr::regex(
            non_operational_context_pattern
          )
        )
      ) {
        next
      }

      matched <- stringr::str_extract_all(
        line_text,
        stringr::regex(
          patterns$pattern[[i]]
        )
      )[[1]]

      hits[[length(hits) + 1L]] <- tibble::tibble(
        file = relative_path,
        line = line_no,
        rule_id = patterns$rule_id[[i]],
        category = patterns$category[[i]],
        description = patterns$description[[i]],
        matched_text = paste(
          matched,
          collapse = " | "
        ),
        line_text = line_text
      )
    }
  }

  if (length(hits) == 0L) {
    return(NULL)
  }

  dplyr::bind_rows(
    hits
  )
}


findings <- purrr::map_dfr(
  audit_files,
  scan_one_file
)

if (nrow(findings) > 0L) {

  # Same line can match both a general and a specific rule.
  # Keep unique operational locations while retaining the strongest match.
  findings <- findings |>
    dplyr::arrange(
      file,
      line,
      dplyr::desc(rule_id)
    ) |>
    dplyr::distinct(
      file,
      line,
      matched_text,
      .keep_all = TRUE
    )
}


# ============================================================
# 8. Architecture checks
# ============================================================

config_path <- file.path(
  repo_root,
  "config",
  "project_config.yml"
)

path_utils_path <- file.path(
  repo_root,
  "R",
  "utils",
  "path_utils.R"
)

r29_path <- file.path(
  repo_root,
  "R",
  "29_run_author_directed_additional_analysis.R"
)

config_exists <- file.exists(
  config_path
)

path_utils_exists <- file.exists(
  path_utils_path
)

r29_exists <- file.exists(
  r29_path
)

r29_uses_project_root <- FALSE

if (r29_exists) {

  r29_lines <- safe_read_lines(
    r29_path
  )

  r29_uses_project_root <- any(
    stringr::str_detect(
      r29_lines,
      stringr::fixed(
        "paths$project_root"
      )
    )
  )
}


# ============================================================
# 9. Save findings
# ============================================================

if (nrow(findings) == 0L) {

  findings_out <- tibble::tibble(
    file = character(),
    line = integer(),
    rule_id = character(),
    category = character(),
    description = character(),
    matched_text = character(),
    line_text = character()
  )

} else {

  findings_out <- findings |>
    dplyr::arrange(
      file,
      line
    )
}

readr::write_csv(
  findings_out,
  findings_csv,
  na = ""
)


# ============================================================
# 10. Determine PASS / FAIL
# ============================================================

architecture_failures <- character(0)

if (!config_exists) {
  architecture_failures <- c(
    architecture_failures,
    "config/project_config.yml missing"
  )
}

if (!path_utils_exists) {
  architecture_failures <- c(
    architecture_failures,
    "R/utils/path_utils.R missing"
  )
}

if (r29_exists && !r29_uses_project_root) {
  architecture_failures <- c(
    architecture_failures,
    "R/29 does not appear to derive workspace from paths$project_root"
  )
}

status <- if (
  nrow(findings_out) == 0L &&
    length(architecture_failures) == 0L
) {
  "PASS"
} else {
  "FAIL"
}


# ============================================================
# 11. Report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "TASK-030 — Portability & User-independent Path Audit",
  "",
  paste0(
    "Repository root (runtime only): ",
    repo_root
  ),
  paste0(
    "Source/config files scanned: ",
    length(audit_files)
  ),
  paste0(
    "Operational non-portable path findings: ",
    nrow(findings_out)
  ),
  "",
  "AUDIT SCOPE",
  "--------------------------------------------------",
  "R/30 audit script itself excluded from scanning: YES",
  paste0(
    "Included source roots: ",
    paste(source_roots, collapse = ", ")
  ),
  paste0(
    "Excluded generated/runtime roots: ",
    paste(excluded_roots, collapse = ", ")
  ),
  "",
  "ARCHITECTURE CHECKS",
  "--------------------------------------------------",
  paste0(
    "config/project_config.yml exists: ",
    ifelse(config_exists, "YES", "NO")
  ),
  paste0(
    "R/utils/path_utils.R exists: ",
    ifelse(path_utils_exists, "YES", "NO")
  ),
  paste0(
    "R/29 exists: ",
    ifelse(r29_exists, "YES", "NO")
  ),
  paste0(
    "R/29 uses paths$project_root: ",
    ifelse(r29_uses_project_root, "YES", "NO")
  )
)

if (length(architecture_failures) > 0L) {

  report_lines <- c(
    report_lines,
    "",
    "ARCHITECTURE FAILURES",
    "--------------------------------------------------",
    paste0(
      "- ",
      architecture_failures
    )
  )
}

report_lines <- c(
  report_lines,
  "",
  "SOURCE PATH AUDIT",
  "--------------------------------------------------"
)

if (nrow(findings_out) == 0L) {

  report_lines <- c(
    report_lines,
    "No operational user/machine-specific absolute path was detected."
  )

} else {

  report_lines <- c(
    report_lines,
    paste0(
      nrow(findings_out),
      " operational non-portable path reference(s) detected."
    ),
    paste0(
      "See: ",
      findings_csv
    )
  )
}

report_lines <- c(
  report_lines,
  "",
  "IMPORTANT",
  "--------------------------------------------------",
  "Generated results/data/logs are intentionally excluded from PASS/FAIL.",
  "Such files may record an absolute runtime path for provenance without",
  "making the toolkit non-portable.",
  "",
  "NEXT AFTER PASS",
  "--------------------------------------------------",
  "TASK-031 — Config-driven canonical path refactor",
  "TASK-032 — Runner upload routing to active analysis workspace",
  "",
  "STATUS",
  "--------------------------------------------------",
  paste0(
    "TASK-030: ",
    status
  )
)

writeLines(
  report_lines,
  report_txt
)


# ============================================================
# 12. Console
# ============================================================

cat("\n")
cat("============================================================\n")
cat("TASK-030 — Portability & User-independent Path Audit\n")
cat("============================================================\n\n")

cat(
  "Source/config files scanned: ",
  length(audit_files),
  "\n",
  sep = ""
)

cat(
  "Operational non-portable paths: ",
  nrow(findings_out),
  "\n",
  sep = ""
)

cat(
  "Architecture failures: ",
  length(architecture_failures),
  "\n\n",
  sep = ""
)

if (nrow(findings_out) > 0L) {

  print(
    findings_out |>
      dplyr::select(
        file,
        line,
        category,
        matched_text
      ),
    n = Inf,
    width = Inf
  )

  cat("\n")
}

if (length(architecture_failures) > 0L) {

  cat(
    paste0(
      "- ",
      architecture_failures,
      collapse = "\n"
    ),
    "\n\n"
  )
}

if (status == "FAIL") {

  cat(
    "FAIL: TASK-030 detected operational portability issue(s).\n"
  )

  cat(
    "Findings: ",
    findings_csv,
    "\n",
    sep = ""
  )

  stop(
    "TASK-030 FAIL — review source/config findings above.",
    call. = FALSE
  )
}

cat(
  "PASS: TASK-030 portability audit completed.\n"
)

cat(
  "Report: ",
  report_txt,
  "\n",
  sep = ""
)

cat(
  "\nNext: TASK-031 — Config-driven canonical path refactor.\n"
)

invisible(
  list(
    status = status,
    findings = findings_out,
    architecture_failures = architecture_failures,
    report = report_txt
  )
)
