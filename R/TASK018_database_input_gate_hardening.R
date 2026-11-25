# ============================================================
# Universal SR-MA Agent Toolkit
# TASK-018 — Universal Database Input Gate Hardening
#
# 목적
# ------------------------------------------------------------
# 기존 canonical Runner의 "database_input_gate"를 강화한다.
#
# 이 TASK는 새로운 sequential workflow stage를 추가하지 않는다.
# 기존 구조를 유지한다:
#
#   pubmed_download
#        ↓
#   database_input_gate   <- TASK-018이 강화하는 지점
#        ↓
#   merge_deduplication
#
# 핵심 기능
# ------------------------------------------------------------
# 1. enabled database를 canonical config에서 확인한다.
# 2. 자동 수집 DB와 사용자 검색/export DB를 구분한다.
# 3. manual DB 입력이 없으면 generic missing-file error 대신:
#      - 정확한 canonical 검색식
#      - 검색식 파일 경로
#      - 검색 수행 안내
#      - export 형식
#      - exact canonical 저장 경로/파일명
#      - rerun/resume 방법
#    을 한 번에 출력한다.
# 4. 여러 manual DB가 빠졌으면 모두 한 번에 안내한다.
# 5. export가 저장되면 재실행 시 자동으로 검증한다.
# 6. 완료된 upstream stage는 재실행하지 않는다.
# 7. topic/project-specific hardcoding을 금지한다.
# 8. repository-wide newest-file discovery를 사용하지 않는다.
#
# 직접 실행:
#   source("R/TASK018_database_input_gate_hardening.R")
#
# Runner 통합:
#   existing database_input_gate가 이 파일의
#   task018_run_database_input_gate()를 호출하도록 연결한다.
#
# 반환 상태:
#   PASS
#   WAITING_FOR_DATABASE_EXPORT
#   MISSING_AUTOMATED_OUTPUT
#   INVALID_DATABASE_INPUT
#
# ============================================================


# ------------------------------------------------------------
# 0. Dependencies and canonical toolkit utilities
# ------------------------------------------------------------

required_packages <- c(
  "yaml",
  "readr",
  "tibble",
  "dplyr"
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
    "TASK-018 requires package(s): ",
    paste(missing_packages, collapse = ", "),
    ". 먼저 START_HERE.R 환경 준비를 완료하십시오.",
    call. = FALSE
  )
}

if (!file.exists("R/utils/config_utils.R")) {
  stop(
    "TASK-018은 repository root에서 실행해야 합니다. ",
    "R/utils/config_utils.R 을 찾을 수 없습니다.",
    call. = FALSE
  )
}

if (!file.exists("R/utils/path_utils.R")) {
  stop(
    "TASK-018은 repository root에서 실행해야 합니다. ",
    "R/utils/path_utils.R 을 찾을 수 없습니다.",
    call. = FALSE
  )
}

source("R/utils/config_utils.R")
source("R/utils/path_utils.R")


# ------------------------------------------------------------
# 1. Small helpers
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

task018_now <- function() {
  format(
    Sys.time(),
    "%Y-%m-%d %H:%M:%S %z"
  )
}

task018_norm_db <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)

  aliases <- c(
    "pub_med" = "pubmed",
    "medline_pubmed" = "pubmed",
    "embase_com" = "embase",
    "cochrane_central" = "central",
    "cochrane_library_central" = "central",
    "cochrane_central_register_of_controlled_trials" = "central"
  )

  if (x %in% names(aliases)) {
    aliases[[x]]
  } else {
    x
  }
}

task018_is_absolute_path <- function(path) {
  if (is.null(path) || length(path) == 0L) return(FALSE)

  path <- as.character(path[[1]])

  if (!nzchar(path)) return(FALSE)

  grepl(
    "^([A-Za-z]:[/\\\\]|/|\\\\\\\\)",
    path
  )
}

task018_absolute_path <- function(path, project_root) {
  if (is.null(path) || length(path) == 0L) {
    return(NA_character_)
  }

  path <- trimws(as.character(path[[1]]))

  if (!nzchar(path)) {
    return(NA_character_)
  }

  if (!task018_is_absolute_path(path)) {
    path <- file.path(project_root, path)
  }

  normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )
}

task018_file_nonempty <- function(path) {
  if (
    is.null(path) ||
    length(path) == 0L ||
    is.na(path) ||
    !nzchar(path) ||
    !file.exists(path)
  ) {
    return(FALSE)
  }

  info <- file.info(path)

  !isTRUE(info$isdir) &&
    !is.na(info$size) &&
    info$size > 0
}

task018_md5 <- function(path) {
  if (!task018_file_nonempty(path)) {
    return(NA_character_)
  }

  unname(
    tools::md5sum(path)
  )
}

task018_print_rule <- function(char = "=", width = 68L) {
  cat(
    paste(
      rep(char, width),
      collapse = ""
    ),
    "\n",
    sep = ""
  )
}


# ------------------------------------------------------------
# 2. Canonical active project
# ------------------------------------------------------------

task018_load_context <- function() {
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

  if (
    is.null(paths$project_root) ||
    !nzchar(as.character(paths$project_root))
  ) {
    stop(
      "TASK-018: build_project_paths()가 canonical project_root를 반환하지 않았습니다.",
      call. = FALSE
    )
  }

  list(
    config = config,
    paths = paths,
    project_root = normalizePath(
      paths$project_root,
      winslash = "/",
      mustWork = TRUE
    )
  )
}


# ------------------------------------------------------------
# 3. Enabled database extraction
#
# 기존 helper가 있으면 우선 사용하고,
# 없을 때만 canonical config 구조에서 읽는다.
# ------------------------------------------------------------

task018_enabled_databases <- function(config) {

  if (
    exists(
      "get_enabled_databases",
      mode = "function",
      inherits = TRUE
    )
  ) {
    dbs <- get_enabled_databases(config)

    if (!is.null(dbs) && length(dbs) > 0L) {
      return(
        unique(
          vapply(
            dbs,
            task018_norm_db,
            character(1)
          )
        )
      )
    }
  }

  candidates <- list(
    config$enabled_databases,
    config$search$enabled_databases
  )

  for (candidate in candidates) {
    if (!is.null(candidate)) {
      x <- unlist(
        candidate,
        use.names = FALSE
      )

      x <- x[
        nzchar(
          trimws(
            as.character(x)
          )
        )
      ]

      if (length(x) > 0L) {
        return(
          unique(
            vapply(
              x,
              task018_norm_db,
              character(1)
            )
          )
        )
      }
    }
  }

  inspect_named_database_list <- function(node) {
    if (
      is.null(node) ||
      !is.list(node) ||
      is.null(names(node))
    ) {
      return(character(0))
    }

    db_names <- names(node)

    enabled <- vapply(
      db_names,
      function(db_name) {
        x <- node[[db_name]]

        if (
          is.logical(x) &&
          length(x) == 1L
        ) {
          return(isTRUE(x))
        }

        if (
          is.list(x) &&
          !is.null(x$enabled)
        ) {
          return(isTRUE(x$enabled))
        }

        TRUE
      },
      logical(1)
    )

    db_names[enabled]
  }

  x <- inspect_named_database_list(
    config$databases
  )

  if (length(x) > 0L) {
    return(
      unique(
        vapply(
          x,
          task018_norm_db,
          character(1)
        )
      )
    )
  }

  x <- inspect_named_database_list(
    config$search$databases
  )

  if (length(x) > 0L) {
    return(
      unique(
        vapply(
          x,
          task018_norm_db,
          character(1)
        )
      )
    )
  }

  stop(
    paste0(
      "TASK-018: enabled database 목록을 canonical config에서 찾지 못했습니다.\n",
      "config/project_config.yml의 database 설정을 확인하십시오."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 4. Universal database registry
#
# 원칙:
# - core gate는 DB별 if/else 집합이 되어서는 안 된다.
# - known DB에는 안전한 기본값을 제공한다.
# - project_config.yml에 DB metadata가 있으면 그것이 우선한다.
# - 향후 DB는 config metadata를 추가하여 core gate 수정 없이 확장한다.
# ------------------------------------------------------------

task018_builtin_registry <- function() {
  list(
    pubmed = list(
      database_id = "pubmed",
      display_name = "PubMed",
      retrieval_mode = "automated",
      search_strategy_file = "pubmed_search.txt",
      canonical_search_path_keys = c("pubmed_search"),
      canonical_input_path_keys = c("pubmed_csv", "pubmed_xml"),
      preferred_export_format = "TOOLKIT_AUTOMATED_OUTPUT",
      accepted_extensions = c(
        "csv",
        "xml"
      ),
      default_extension = "csv",
      upstream_task = "TASK-005 — Canonical PubMed Download",
      manual_refinement_mode = "none",
      search_instructions = character(0)
    ),

    embase = list(
      database_id = "embase",
      display_name = "Embase",
      retrieval_mode = "guided_manual",
      search_strategy_file = "embase_search.txt",
      canonical_search_path_keys = c("embase_search"),
      canonical_input_path_keys = c("embase_ris"),
      preferred_export_format = "RIS",
      accepted_extensions = c(
        "ris"
      ),
      default_extension = "ris",
      upstream_task = NA_character_,
      manual_refinement_mode = "descriptor_no_explosion",
      search_instructions = c(
        "Embase를 엽니다.",
        "Advanced Search 또는 기관에서 제공되는 해당 검색 인터페이스를 엽니다.",
        "아래 canonical Embase 검색식을 그대로 복사하여 사용합니다.",
        "검색을 실행합니다.",
        "검색된 전체 record를 export합니다."
      )
    ),

    central = list(
      database_id = "central",
      display_name = "CENTRAL",
      retrieval_mode = "guided_manual",
      search_strategy_file = "central_search.txt",
      canonical_search_path_keys = c("central_search"),
      canonical_input_path_keys = c("central_ris"),
      preferred_export_format = "RIS",
      accepted_extensions = c(
        "ris"
      ),
      default_extension = "ris",
      upstream_task = NA_character_,
      manual_refinement_mode = "central_term_only",
      search_instructions = c(
        "Cochrane Library / CENTRAL을 엽니다.",
        "Advanced Search 또는 Search Manager 화면을 엽니다.",
        "아래 canonical CENTRAL 검색식을 그대로 사용합니다.",
        "검색을 실행합니다.",
        "검색된 전체 record를 export합니다."
      )
    )
  )
}

task018_config_database_metadata <- function(
  config,
  database_id
) {
  candidates <- list()

  if (
    !is.null(config$databases) &&
    is.list(config$databases) &&
    !is.null(config$databases[[database_id]]) &&
    is.list(config$databases[[database_id]])
  ) {
    candidates[[length(candidates) + 1L]] <- config$databases[[database_id]]
  }

  if (
    !is.null(config$database_registry) &&
    is.list(config$database_registry) &&
    !is.null(config$database_registry[[database_id]]) &&
    is.list(config$database_registry[[database_id]])
  ) {
    candidates[[length(candidates) + 1L]] <- config$database_registry[[database_id]]
  }

  if (length(candidates) == 0L) {
    return(list())
  }

  out <- list()

  for (candidate in candidates) {
    for (nm in names(candidate)) {
      out[[nm]] <- candidate[[nm]]
    }
  }

  out
}

task018_registry_entry <- function(
  database_id,
  config
) {
  database_id <- task018_norm_db(
    database_id
  )

  builtin <- task018_builtin_registry()
  base <- builtin[[database_id]] %||% list(
    database_id = database_id,
    display_name = database_id,
    retrieval_mode = NA_character_,
    search_strategy_file = paste0(
      database_id,
      "_search.txt"
    ),
    canonical_search_path_keys = character(0),
    canonical_input_path_keys = character(0),
    preferred_export_format = NA_character_,
    accepted_extensions = character(0),
    default_extension = NA_character_,
    upstream_task = NA_character_,
    manual_refinement_mode = "none",
    search_instructions = character(0)
  )

  override <- task018_config_database_metadata(
    config,
    database_id
  )

  for (nm in names(override)) {
    base[[nm]] <- override[[nm]]
  }

  base$database_id <- database_id

  if (
    is.null(base$display_name) ||
    !nzchar(as.character(base$display_name))
  ) {
    base$display_name <- database_id
  }

  required_fields <- c(
    "retrieval_mode",
    "preferred_export_format"
  )

  missing_required <- required_fields[
    vapply(
      required_fields,
      function(field) {
        value <- base[[field]]

        is.null(value) ||
          length(value) == 0L ||
          is.na(value[[1]]) ||
          !nzchar(
            trimws(
              as.character(value[[1]])
            )
          )
      },
      logical(1)
    )
  ]

  if (length(missing_required) > 0L) {
    stop(
      paste0(
        "TASK-018: database '",
        database_id,
        "'의 registry metadata가 불완전합니다. Missing: ",
        paste(
          missing_required,
          collapse = ", "
        ),
        "\nKnown DB가 아니라면 config/project_config.yml의 ",
        "database_registry에 retrieval_mode/export metadata를 추가하십시오."
      ),
      call. = FALSE
    )
  }

  mode <- tolower(
    trimws(
      as.character(
        base$retrieval_mode[[1]]
      )
    )
  )

  if (!mode %in% c(
    "automated",
    "guided_manual"
  )) {
    stop(
      "TASK-018: unsupported retrieval_mode for ",
      database_id,
      ": ",
      mode,
      call. = FALSE
    )
  }

  base$retrieval_mode <- mode

  if (
    is.null(base$accepted_extensions) ||
    length(base$accepted_extensions) == 0L
  ) {
    base$accepted_extensions <- tolower(
      tools::file_ext(
        paste0(
          "x.",
          base$preferred_export_format
        )
      )
    )
  }

  base$accepted_extensions <- unique(
    tolower(
      gsub(
        "^\\.",
        "",
        as.character(
          base$accepted_extensions
        )
      )
    )
  )

  if (
    is.null(base$default_extension) ||
    length(base$default_extension) == 0L ||
    is.na(base$default_extension[[1]]) ||
    !nzchar(
      as.character(
        base$default_extension[[1]]
      )
    )
  ) {
    base$default_extension <-
      base$accepted_extensions[[1]]
  }

  base
}


# ------------------------------------------------------------
# 5. Canonical search strategy resolution
#
# repository-wide search는 하지 않는다.
# active project의 explicit config/path 또는 canonical
# search_strategies/<db>_search.txt 만 허용한다.
# ------------------------------------------------------------

task018_resolve_search_strategy <- function(
  database_id,
  metadata,
  config,
  paths,
  project_root
) {
  candidates <- character(0)

  add_candidate <- function(x) {
    if (
      !is.null(x) &&
      length(x) > 0L &&
      !is.na(x[[1]]) &&
      nzchar(
        trimws(
          as.character(x[[1]])
        )
      )
    ) {
      candidates <<- c(
        candidates,
        as.character(x[[1]])
      )
    }
  }

  # 1. Canonical keys emitted by build_project_paths()
  canonical_keys <- metadata$canonical_search_path_keys %||% character(0)

  if (!is.null(paths) && is.list(paths) && length(canonical_keys) > 0L) {
    for (key in canonical_keys) {
      add_candidate(paths[[key]])
    }
  }

  # 2. Explicit config overrides
  if (
    !is.null(config$search_strategies) &&
    is.list(config$search_strategies)
  ) {
    add_candidate(
      config$search_strategies[[database_id]]
    )
  }

  if (
    !is.null(config$paths) &&
    is.list(config$paths)
  ) {
    add_candidate(
      config$paths[[paste0(database_id, "_search_strategy")]]
    )

    add_candidate(
      config$paths[[paste0(database_id, "_search")]]
    )
  }

  # 3. Deterministic active-project canonical search_strategies location
  add_candidate(
    file.path(
      project_root,
      "search_strategies",
      metadata$search_strategy_file
    )
  )

  candidates <- unique(candidates)

  for (candidate in candidates) {
    candidate <- task018_absolute_path(
      candidate,
      project_root
    )

    if (task018_file_nonempty(candidate)) {
      return(candidate)
    }
  }

  task018_absolute_path(
    file.path(
      "search_strategies",
      metadata$search_strategy_file
    ),
    project_root
  )
}


# ------------------------------------------------------------
# 6. Canonical database input resolution
#
# 우선순위:
# 1) build_project_paths()가 제공한 explicit DB path
# 2) project_config.yml의 explicit DB path
# 3) universal active-project canonical destination
#
# 다른 프로젝트 tree를 탐색하지 않는다.
# ------------------------------------------------------------

task018_resolve_database_input <- function(
  database_id,
  metadata,
  config,
  paths,
  project_root
) {
  candidates <- list()

  add_candidate <- function(
    value,
    source
  ) {
    if (
      !is.null(value) &&
      length(value) > 0L &&
      !is.na(value[[1]]) &&
      nzchar(
        trimws(
          as.character(value[[1]])
        )
      )
    ) {
      candidates[[length(candidates) + 1L]] <<- list(
        value = as.character(value[[1]]),
        source = source
      )
    }
  }

  # 1. Canonical path keys emitted by build_project_paths()
  canonical_keys <- metadata$canonical_input_path_keys %||% character(0)

  if (!is.null(paths) && is.list(paths) && length(canonical_keys) > 0L) {
    for (key in canonical_keys) {
      add_candidate(
        paths[[key]],
        paste0("paths$", key)
      )
    }
  }

  # 2. Generic existing canonical path shapes
  direct_keys <- c(
    paste0(database_id, "_input"),
    paste0(database_id, "_file"),
    paste0(database_id, "_export"),
    paste0(database_id, "_records")
  )

  if (!is.null(paths) && is.list(paths)) {
    for (key in direct_keys) {
      add_candidate(
        paths[[key]],
        paste0("paths$", key)
      )
    }

    nested_keys <- c(
      "database_inputs",
      "database_exports",
      "raw_database_inputs",
      "raw_inputs"
    )

    for (key in nested_keys) {
      node <- paths[[key]]

      if (is.list(node)) {
        add_candidate(
          node[[database_id]],
          paste0(
            "paths$",
            key,
            "$",
            database_id
          )
        )
      }
    }
  }

  # 3. Explicit project config overrides
  if (
    !is.null(config$paths) &&
    is.list(config$paths)
  ) {
    for (key in direct_keys) {
      add_candidate(
        config$paths[[key]],
        paste0("config$paths$", key)
      )
    }

    nested_keys <- c(
      "database_inputs",
      "database_exports",
      "raw_database_inputs",
      "raw_inputs"
    )

    for (key in nested_keys) {
      node <- config$paths[[key]]

      if (is.list(node)) {
        add_candidate(
          node[[database_id]],
          paste0(
            "config$paths$",
            key,
            "$",
            database_id
          )
        )
      }
    }
  }

  metadata_from_config <-
    task018_config_database_metadata(
      config,
      database_id
    )

  add_candidate(
    metadata_from_config$input_file %||%
      metadata_from_config$export_file %||%
      metadata_from_config$input_path,
    paste0(
      "config database metadata: ",
      database_id
    )
  )

  if (length(candidates) > 0L) {
    # Prefer the first existing canonical candidate.
    for (candidate in candidates) {
      candidate_path <- task018_absolute_path(
        candidate$value,
        project_root
      )

      if (file.exists(candidate_path)) {
        return(
          list(
            path = candidate_path,
            source = candidate$source
          )
        )
      }
    }

    # If none exists yet, return the first canonical expected destination.
    hit <- candidates[[1]]

    return(
      list(
        path = task018_absolute_path(
          hit$value,
          project_root
        ),
        source = hit$source
      )
    )
  }

  # 4. Future supported DBs may use a deterministic active-project fallback,
  #    but known DBs should normally be resolved by build_project_paths().
  default_path <- file.path(
    project_root,
    "data",
    "raw",
    "search",
    database_id,
    paste0(
      database_id,
      "_results.",
      metadata$default_extension
    )
  )

  list(
    path = normalizePath(
      default_path,
      winslash = "/",
      mustWork = FALSE
    ),
    source = "TASK018_UNIVERSAL_ACTIVE_PROJECT_CANONICAL"
  )
}


# ------------------------------------------------------------
# 7. Minimal importer-independent validation
# ------------------------------------------------------------

task018_count_ris_records <- function(path) {
  lines <- readLines(
    path,
    warn = FALSE,
    encoding = "UTF-8"
  )

  sum(
    grepl(
      "^TY  -",
      lines
    )
  )
}

task018_count_csv_records <- function(path) {
  result <- tryCatch(
    {
      x <- readr::read_csv(
        path,
        show_col_types = FALSE,
        progress = FALSE
      )

      nrow(x)
    },
    error = function(e) {
      NA_integer_
    }
  )

  result
}

task018_validate_database_input <- function(
  path,
  metadata
) {
  if (!file.exists(path)) {
    return(
      list(
        passed = FALSE,
        status = if (
          identical(
            metadata$retrieval_mode,
            "guided_manual"
          )
        ) {
          "MISSING_USER_EXPORT"
        } else {
          "MISSING_AUTOMATED_OUTPUT"
        },
        message = "Expected canonical database input file is absent.",
        record_count = NA_integer_
      )
    )
  }

  info <- file.info(path)

  if (
    isTRUE(info$isdir) ||
    is.na(info$size) ||
    info$size <= 0
  ) {
    return(
      list(
        passed = FALSE,
        status = "INVALID_DATABASE_INPUT",
        message = "Database input file is empty or is not a regular file.",
        record_count = NA_integer_
      )
    )
  }

  extension <- tolower(
    tools::file_ext(path)
  )

  if (
    length(metadata$accepted_extensions) > 0L &&
    !extension %in%
      metadata$accepted_extensions
  ) {
    return(
      list(
        passed = FALSE,
        status = "INVALID_DATABASE_INPUT",
        message = paste0(
          "Unsupported extension: .",
          extension,
          "; accepted: ",
          paste(
            paste0(
              ".",
              metadata$accepted_extensions
            ),
            collapse = ", "
          )
        ),
        record_count = NA_integer_
      )
    )
  }

  record_count <- NA_integer_

  if (identical(extension, "ris")) {
    record_count <- tryCatch(
      task018_count_ris_records(
        path
      ),
      error = function(e) {
        NA_integer_
      }
    )

    if (
      is.na(record_count) ||
      record_count < 1L
    ) {
      return(
        list(
          passed = FALSE,
          status = "INVALID_DATABASE_INPUT",
          message = paste0(
            "Valid RIS records were not detected ",
            "(missing 'TY  -' record markers)."
          ),
          record_count = record_count
        )
      )
    }
  }

  if (identical(extension, "csv")) {
    record_count <- task018_count_csv_records(
      path
    )

    if (is.na(record_count)) {
      return(
        list(
          passed = FALSE,
          status = "INVALID_DATABASE_INPUT",
          message = "CSV file could not be parsed.",
          record_count = NA_integer_
        )
      )
    }
  }

  if (identical(extension, "xml")) {
    first_lines <- tryCatch(
      paste(
        readLines(
          path,
          n = 50L,
          warn = FALSE
        ),
        collapse = "\n"
      ),
      error = function(e) {
        ""
      }
    )

    if (
      !nzchar(first_lines) ||
      !grepl(
        "<",
        first_lines,
        fixed = TRUE
      )
    ) {
      return(
        list(
          passed = FALSE,
          status = "INVALID_DATABASE_INPUT",
          message = "File does not appear to contain XML content.",
          record_count = NA_integer_
        )
      )
    }
  }

  list(
    passed = TRUE,
    status = "READY",
    message = "Canonical database input is present and minimally valid.",
    record_count = record_count
  )
}


# ------------------------------------------------------------
# 8A. Manual database search breadth review / refinement
#
# These are TEST CANDIDATES only.
# Canonical search files are not overwritten here.
# ------------------------------------------------------------

task018_refined_candidate_path <- function(
  project_root,
  database_id
) {
  file.path(
    project_root,
    "search_strategies",
    paste0(
      database_id,
      "_search_refined_candidate.txt"
    )
  )
}

task018_refine_embase_descriptor_no_explosion <- function(text) {
  # Conservative DB-specific narrowing:
  # retain every free-text term, but stop exploding Emtree descriptors.
  refined <- gsub(
    "'/exp",
    "'/de",
    text,
    fixed = TRUE
  )

  refined <- gsub(
    "/exp",
    "/de",
    refined,
    fixed = TRUE
  )

  refined
}

task018_refine_central_term_only <- function(text) {
  # Conservative CENTRAL precision candidate:
  # preserve every controlled-vocabulary descriptor and every
  # generated free-text synonym, but turn MeSH explosion OFF.
  #
  # Cochrane Library compact syntax:
  #   [mh "Term"]   -> exploded
  #   [mh ^"Term"]  -> this term only / unexploded
  #
  # This candidate is never auto-promoted to canonical.

  refined <- gsub(
    '[mh "',
    '[mh ^"',
    text,
    fixed = TRUE
  )

  # CENTRAL fail-safe: slash is prohibited by toolkit policy.
  refined <- gsub(
    "/",
    " ",
    refined,
    fixed = TRUE
  )

  refined
}

task018_generate_manual_refinement_candidates <- function(
  database_results,
  config,
  project_root
) {
  candidate_paths <- list()

  for (database_id in names(database_results)) {
    x <- database_results[[database_id]]

    if (!identical(x$status, "MISSING_USER_EXPORT")) {
      next
    }

    metadata <- task018_registry_entry(
      database_id,
      config
    )

    mode <- as.character(
      metadata$manual_refinement_mode %||% "none"
    )

    if (
      identical(mode, "none") ||
      !task018_file_nonempty(x$search_strategy_file)
    ) {
      next
    }

    original <- paste(
      readLines(
        x$search_strategy_file,
        warn = FALSE,
        encoding = "UTF-8"
      ),
      collapse = "\n"
    )

    refined <- switch(
      mode,

      descriptor_no_explosion =
        task018_refine_embase_descriptor_no_explosion(
          original
        ),

      central_term_only =
        task018_refine_central_term_only(
          original
        ),

      stop(
        "Unsupported manual_refinement_mode: ",
        mode,
        call. = FALSE
      )
    )

    if (!nzchar(trimws(refined))) {
      stop(
        "Refined candidate became empty for database: ",
        database_id,
        call. = FALSE
      )
    }

    if (
      identical(database_id, "central") &&
      grepl("/", refined, fixed = TRUE)
    ) {
      stop(
        "CENTRAL refined candidate still contains '/'.",
        call. = FALSE
      )
    }

    candidate_path <- task018_refined_candidate_path(
      project_root,
      database_id
    )

    writeLines(
      strsplit(
        refined,
        "\n",
        fixed = TRUE
      )[[1]],
      candidate_path,
      useBytes = TRUE
    )

    candidate_paths[[database_id]] <- candidate_path
  }

  candidate_paths
}

task018_prompt_manual_search_review <- function(
  database_results,
  config,
  project_root
) {
  result <- list(
    action = "NOT_REVIEWED",
    candidate_paths = list()
  )

  if (!interactive()) {
    return(result)
  }

  task018_print_rule()
  cat("MANUAL DATABASE SEARCH REVIEW\n")
  task018_print_rule()

  cat(
    "\nEmbase/CENTRAL에서 위 검색식을 시험 검색한 뒤,\n",
    "검색 건수와 제목/초록 일부를 확인하십시오.\n\n",
    "현재 systematic review의 범위에 비해 검색 결과가 지나치게 broad합니까?\n\n",
    "[1] 아직 판단하지 않았습니다 — 현재 검색식을 유지하고 일단 pause\n",
    "[2] 아니오 — 현재 검색식이 적절합니다\n",
    "[3] 예 — 더 정제된 검색식 후보를 생성합니다\n\n",
    sep = ""
  )

  choice <- trimws(
    readline("선택 [1-3]: ")
  )

  if (!choice %in% c("1", "2", "3")) {
    choice <- "1"
  }

  if (identical(choice, "1")) {
    result$action <- "NOT_REVIEWED"
    return(result)
  }

  if (identical(choice, "2")) {
    result$action <- "KEEP_CURRENT"
    return(result)
  }

  candidate_paths <- task018_generate_manual_refinement_candidates(
    database_results = database_results,
    config = config,
    project_root = project_root
  )

  result$action <- "REFINED_CANDIDATE"
  result$candidate_paths <- candidate_paths

  cat("\n")
  task018_print_rule()
  cat("REFINED SEARCH CANDIDATES GENERATED\n")
  task018_print_rule()

  cat(
    "\n이 파일들은 검토용 후보입니다.\n",
    "아직 canonical search strategy를 덮어쓰지 않았습니다.\n",
    "실제 Embase/CENTRAL에서 다시 시험 검색한 후 최종 선택합니다.\n\n",
    sep = ""
  )

  for (database_id in names(candidate_paths)) {
    metadata <- task018_registry_entry(
      database_id,
      config
    )

    cat(
      metadata$display_name,
      " refined candidate:\n",
      candidate_paths[[database_id]],
      "\n\n",
      sep = ""
    )
  }

  result
}
# ------------------------------------------------------------
# 8. Manual search/export guidance
# ------------------------------------------------------------

task018_print_manual_guidance <- function(
  metadata,
  search_strategy_file,
  input_file
) {
  task018_print_rule()
  cat(
    metadata$display_name,
    " — 사용자 검색 및 Export 필요\n",
    sep = ""
  )
  task018_print_rule()

  cat("\n[검색 절차]\n")

  instructions <-
    metadata$search_instructions %||%
    character(0)

  if (length(instructions) > 0L) {
    for (i in seq_along(instructions)) {
      cat(
        sprintf(
          "%d. %s\n",
          i,
          instructions[[i]]
        )
      )
    }
  } else {
    cat(
      "1. 해당 데이터베이스 검색 인터페이스를 엽니다.\n",
      "2. 아래 canonical 검색식을 그대로 사용합니다.\n",
      "3. 검색을 실행합니다.\n",
      "4. 검색된 전체 record를 export합니다.\n",
      sep = ""
    )
  }

  cat("\n[Canonical 검색식]\n")
  task018_print_rule(
    "-",
    68L
  )

  if (task018_file_nonempty(search_strategy_file)) {
    cat(
      paste(
        readLines(
          search_strategy_file,
          warn = FALSE,
          encoding = "UTF-8"
        ),
        collapse = "\n"
      ),
      "\n",
      sep = ""
    )
  } else {
    cat(
      "Canonical search strategy file을 찾지 못했습니다.\n",
      "Expected: ",
      search_strategy_file,
      "\n",
      "R/11 search generation 및 R/12 validation 상태를 확인하십시오.\n",
      sep = ""
    )
  }

  task018_print_rule(
    "-",
    68L
  )

  cat(
    "검색식 파일: ",
    search_strategy_file,
    "\n",
    sep = ""
  )

  cat("\n[Export 요구사항]\n")
  cat(
    "범위: 검색된 전체 record (ALL retrieved records)\n",
    sep = ""
  )
  cat(
    "형식: ",
    metadata$preferred_export_format,
    "\n",
    sep = ""
  )
  cat(
    paste0(
      "가능하면 포함: Title, Authors, Journal, Year, DOI, ",
      "PMID/accession identifier, Abstract, Keywords/index terms\n"
    )
  )

  cat("\n[저장 위치]\n")
  cat(
    input_file,
    "\n",
    sep = ""
  )

  cat("\n[파일명]\n")
  cat(
    basename(
      input_file
    ),
    "\n",
    sep = ""
  )

  cat(
    "\n검색과 export가 끝나면 위 canonical path에 저장한 후\n",
    "현재 Toolkit workflow를 다시 실행/resume하십시오.\n",
    "완료된 upstream stage는 다시 실행할 필요가 없습니다.\n",
    sep = ""
  )
}


# ------------------------------------------------------------
# 9. Provenance
# ------------------------------------------------------------

task018_provenance_file <- function(
  config,
  project_root
) {
  explicit <- NULL

  if (
    !is.null(config$paths) &&
    is.list(config$paths)
  ) {
    explicit <-
      config$paths$database_search_log %||%
      config$paths$search_provenance
  }

  if (
    !is.null(explicit) &&
    length(explicit) > 0L &&
    nzchar(
      trimws(
        as.character(
          explicit[[1]]
        )
      )
    )
  ) {
    return(
      task018_absolute_path(
        explicit[[1]],
        project_root
      )
    )
  }

  task018_absolute_path(
    file.path(
      "data",
      "raw",
      "database_exports",
      "database_search_log.csv"
    ),
    project_root
  )
}

task018_write_provenance <- function(
  rows,
  output_file
) {
  if (length(rows) == 0L) {
    return(
      invisible(NULL)
    )
  }

  out <- dplyr::bind_rows(
    rows
  )

  dir.create(
    dirname(output_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(
    out,
    output_file,
    na = ""
  )

  invisible(
    output_file
  )
}


# ------------------------------------------------------------
# 10. Evidence report
# ------------------------------------------------------------

task018_write_evidence <- function(
  evidence,
  result,
  paths
) {
  output_dir <- if (
    !is.null(paths$workflow_results_directory)
  ) {
    paths$workflow_results_directory
  } else {
    file.path(
      paths$project_root,
      "results",
      "workflow"
    )
  }

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  csv_file <- file.path(
    output_dir,
    "task018_database_input_gate.csv"
  )

  txt_file <- file.path(
    output_dir,
    "task018_database_input_gate.txt"
  )

  readr::write_csv(
    evidence,
    csv_file,
    na = ""
  )

  report_lines <- c(
    "============================================================",
    "TASK-018 — Universal Database Input Gate Hardening",
    "============================================================",
    "",
    paste0(
      "Generated at: ",
      task018_now()
    ),
    paste0(
      "Project ID: ",
      result$project_id
    ),
    paste0(
      "Status: ",
      result$status
    ),
    paste0(
      "Runner state: ",
      result$runner_state
    ),
    paste0(
      "All enabled database inputs ready: ",
      if (isTRUE(result$ready)) "YES" else "NO"
    ),
    "",
    paste0(
      "Evidence CSV: ",
      csv_file
    ),
    paste0(
      "Search provenance: ",
      result$provenance_file
    )
  )

  writeLines(
    report_lines,
    txt_file,
    useBytes = TRUE
  )

  list(
    csv = csv_file,
    report = txt_file
  )
}


# ------------------------------------------------------------
# 11. Main operational gate
# ------------------------------------------------------------

task018_run_database_input_gate <- function(
  config = NULL,
  paths = NULL,
  verbose = TRUE,
  write_evidence = TRUE,
  write_provenance = TRUE
) {
  if (
    is.null(config) ||
    is.null(paths)
  ) {
    context <- task018_load_context()

    if (is.null(config)) {
      config <- context$config
    }

    if (is.null(paths)) {
      paths <- context$paths
    }

    project_root <- context$project_root
  } else {
    project_root <- normalizePath(
      paths$project_root,
      winslash = "/",
      mustWork = TRUE
    )
  }

  project_id <- if (
    exists(
      "get_project_id",
      mode = "function",
      inherits = TRUE
    )
  ) {
    get_project_id(
      config
    )
  } else {
    as.character(
      config$project_id %||%
        config$project$project_id %||%
        basename(
          project_root
        )
    )
  }

  enabled_databases <-
    task018_enabled_databases(
      config
    )

  if (verbose) {
    cat("\n")
    task018_print_rule()
    cat(
      "TASK-018 — Universal Database Input Gate Hardening\n"
    )
    task018_print_rule()
    cat(
      "Project ID: ",
      project_id,
      "\n",
      sep = ""
    )
    cat(
      "Project root: ",
      project_root,
      "\n",
      sep = ""
    )
    cat(
      "Enabled databases: ",
      paste(
        enabled_databases,
        collapse = ", "
      ),
      "\n\n",
      sep = ""
    )
  }

  results <- vector(
    "list",
    length(
      enabled_databases
    )
  )

  names(results) <-
    enabled_databases

  provenance_rows <- list()

  for (database_id in enabled_databases) {
    metadata <- task018_registry_entry(
      database_id,
      config
    )

    search_strategy_file <-
      task018_resolve_search_strategy(
        database_id = database_id,
        metadata = metadata,
        config = config,
        paths = paths,
        project_root = project_root
      )

    input_resolution <-
      task018_resolve_database_input(
        database_id = database_id,
        metadata = metadata,
        config = config,
        paths = paths,
        project_root = project_root
      )

    input_file <-
      input_resolution$path

    if (
      identical(
        metadata$retrieval_mode,
        "guided_manual"
      )
    ) {
      dir.create(
        dirname(
          input_file
        ),
        recursive = TRUE,
        showWarnings = FALSE
      )
    }

    validation <-
      task018_validate_database_input(
        input_file,
        metadata
      )

    results[[database_id]] <- list(
      database_id = database_id,
      display_name = metadata$display_name,
      retrieval_mode = metadata$retrieval_mode,
      status = validation$status,
      passed = validation$passed,
      message = validation$message,
      record_count = validation$record_count,
      search_strategy_file = search_strategy_file,
      input_file = input_file,
      input_path_source = input_resolution$source,
      preferred_export_format =
        metadata$preferred_export_format
    )

    if (
      isTRUE(validation$passed) &&
      task018_file_nonempty(
        input_file
      )
    ) {
      info <- file.info(
        input_file
      )

      provenance_rows[[database_id]] <- tibble::tibble(
        database = metadata$display_name,
        database_id = database_id,
        search_strategy_file =
          search_strategy_file,
        search_date = format(
          info$mtime,
          "%Y-%m-%d"
        ),
        reported_search_count =
          NA_integer_,
        parsed_export_count =
          validation$record_count,
        export_file =
          input_file,
        export_format =
          toupper(
            tools::file_ext(
              input_file
            )
          ),
        file_size =
          as.numeric(
            info$size
          ),
        checksum =
          task018_md5(
            input_file
          ),
        verification_status =
          "READY"
      )
    }
  }

  evidence <- dplyr::bind_rows(
    lapply(
      results,
      function(x) {
        tibble::tibble(
          database = x$display_name,
          database_id = x$database_id,
          retrieval_mode = x$retrieval_mode,
          status = x$status,
          validation_message = x$message,
          record_count = x$record_count,
          search_strategy_file =
            x$search_strategy_file,
          input_file = x$input_file,
          input_path_source =
            x$input_path_source,
          preferred_export_format =
            x$preferred_export_format
        )
      }
    )
  )

  if (verbose) {
    cat(
      "[Database input status]\n"
    )

    for (database_id in enabled_databases) {
      x <- results[[database_id]]

      cat(
        sprintf(
          "%-12s : %s\n",
          x$display_name,
          x$status
        )
      )
    }

    cat("\n")
  }

  # 모든 문제를 한 번에 출력한다.
  for (database_id in enabled_databases) {
    x <- results[[database_id]]

    metadata <- task018_registry_entry(
      database_id,
      config
    )

    if (
      identical(
        x$status,
        "MISSING_USER_EXPORT"
      )
    ) {
      task018_print_manual_guidance(
        metadata = metadata,
        search_strategy_file =
          x$search_strategy_file,
        input_file =
          x$input_file
      )

      cat("\n")
    }

    if (
      identical(
        x$status,
        "MISSING_AUTOMATED_OUTPUT"
      )
    ) {
      task018_print_rule()
      cat(
        x$display_name,
        " — AUTOMATED OUTPUT MISSING\n",
        sep = ""
      )
      task018_print_rule()

      cat(
        "Expected canonical file:\n",
        x$input_file,
        "\n\n",
        sep = ""
      )

      cat(
        "이 데이터베이스는 Toolkit이 자동 수집하도록 설정되어 있습니다.\n"
      )

      cat(
        "필요한 upstream stage: ",
        metadata$upstream_task,
        "\n",
        sep = ""
      )

      cat(
        "해당 upstream stage 완료 후 현재 workflow를 다시 실행하십시오.\n\n"
      )
    }

    if (
      identical(
        x$status,
        "INVALID_DATABASE_INPUT"
      )
    ) {
      task018_print_rule()
      cat(
        x$display_name,
        " — INVALID DATABASE INPUT\n",
        sep = ""
      )
      task018_print_rule()

      cat(
        "File: ",
        x$input_file,
        "\n",
        sep = ""
      )

      cat(
        "Problem: ",
        x$message,
        "\n",
        sep = ""
      )

      cat(
        "Expected export format: ",
        x$preferred_export_format,
        "\n",
        sep = ""
      )

      if (
        identical(
          x$retrieval_mode,
          "guided_manual"
        )
      ) {
        cat(
          paste0(
            "해당 데이터베이스에서 전체 검색 결과를 다시 export하여\n",
            "위 canonical file을 덮어쓰십시오.\n"
          )
        )
      }

      cat("\n")
    }
  }

  manual_search_review <- list(
    action = "NOT_REVIEWED",
    candidate_paths = list()
  )

  manual_missing_ids <- names(results)[
    vapply(
      results,
      function(x) identical(x$status, "MISSING_USER_EXPORT"),
      logical(1)
    )
  ]

  if (
    isTRUE(verbose) &&
    length(manual_missing_ids) > 0L
  ) {
    manual_search_review <- task018_prompt_manual_search_review(
      database_results = results[manual_missing_ids],
      config = config,
      project_root = project_root
    )

    if (
      identical(manual_search_review$action, "REFINED_CANDIDATE") &&
      length(manual_search_review$candidate_paths) > 0L
    ) {
      cat("\n")
      for (database_id in names(manual_search_review$candidate_paths)) {
        metadata <- task018_registry_entry(database_id, config)
        candidate_path <- manual_search_review$candidate_paths[[database_id]]

        task018_print_rule()
        cat(
          metadata$display_name,
          " — REFINED CANDIDATE FOR TEST SEARCH\n",
          sep = ""
        )
        task018_print_rule()

        cat(
          paste(
            readLines(
              candidate_path,
              warn = FALSE,
              encoding = "UTF-8"
            ),
            collapse = "\n"
          ),
          "\n\n",
          sep = ""
        )
      }
    }
  }

  statuses <- vapply(
    results,
    `[[`,
    character(1),
    "status"
  )

  ready <- all(
    statuses == "READY"
  )

  has_manual_wait <- any(
    statuses ==
      "MISSING_USER_EXPORT"
  )

  has_auto_missing <- any(
    statuses ==
      "MISSING_AUTOMATED_OUTPUT"
  )

  has_invalid <- any(
    statuses ==
      "INVALID_DATABASE_INPUT"
  )

  if (ready) {
    status <- "PASS"
    runner_state <- "READY_FOR_MERGE_DEDUP"
  } else if (
    has_invalid
  ) {
    status <- "ACTION_REQUIRED"
    runner_state <- "INVALID_DATABASE_INPUT"
  } else if (
    has_auto_missing
  ) {
    status <- "ACTION_REQUIRED"
    runner_state <- "MISSING_AUTOMATED_OUTPUT"
  } else if (
    has_manual_wait
  ) {
    status <- "WAITING"
    runner_state <- if (
      identical(
        manual_search_review$action,
        "REFINED_CANDIDATE"
      )
    ) {
      "WAITING_FOR_REFINED_DATABASE_SEARCH"
    } else {
      "WAITING_FOR_DATABASE_EXPORT"
    }
  } else {
    status <- "ACTION_REQUIRED"
    runner_state <- "DATABASE_INPUT_NOT_READY"
  }

  provenance_file <-
    task018_provenance_file(
      config,
      project_root
    )

  if (
    isTRUE(write_provenance) &&
    length(
      provenance_rows
    ) > 0L
  ) {
    task018_write_provenance(
      provenance_rows,
      provenance_file
    )
  }

  result <- list(
    task = "TASK-018",
    project_id = project_id,
    status = status,
    runner_state = runner_state,
    ready = ready,
    enabled_databases =
      enabled_databases,
    database_results =
      results,
    manual_search_review =
      manual_search_review,
    provenance_file =
      provenance_file
  )

  if (verbose) {
    task018_print_rule()

    if (ready) {
      cat(
        "PASS: 모든 enabled database input이 준비되었습니다.\n"
      )
      cat(
        "기존 다음 단계인 Merge & Deduplication으로 진행할 수 있습니다.\n"
      )
    } else {
      cat(
        "DATABASE INPUT GATE — ACTION REQUIRED\n"
      )

      if (has_manual_wait) {
        waiting_names <- vapply(
          results[
            statuses ==
              "MISSING_USER_EXPORT"
          ],
          `[[`,
          character(1),
          "display_name"
        )

        cat(
          "사용자 검색/Export 필요: ",
          paste(
            waiting_names,
            collapse = ", "
          ),
          "\n",
          sep = ""
        )
      }

      cat(
        paste0(
          "위 안내에 따라 필요한 작업을 완료한 뒤\n",
          "현재 Toolkit workflow를 다시 실행/resume하십시오.\n",
          "완료된 upstream stage는 다시 실행할 필요가 없습니다.\n"
        )
      )
    }

    cat(
      "STATUS: ",
      runner_state,
      "\n",
      sep = ""
    )

    task018_print_rule()
  }

  if (isTRUE(write_evidence)) {
    evidence_files <-
      task018_write_evidence(
        evidence,
        result,
        paths
      )

    result$evidence_csv <-
      evidence_files$csv

    result$evidence_report <-
      evidence_files$report
  }

  class(result) <- c(
    "task018_database_input_gate_result",
    "list"
  )

  invisible(
    result
  )
}


# ------------------------------------------------------------
# 12. Runner integration contract
# ------------------------------------------------------------
#
# 기존 Runner stage graph는 변경하지 않는다.
#
# database_input_gate validator/handler에서:
#
#   source(
#     "R/TASK018_database_input_gate_hardening.R",
#     local = .GlobalEnv
#   )
#
#   gate <- task018_run_database_input_gate(
#     config = context$config,
#     paths = context$paths,
#     verbose = TRUE
#   )
#
#   if (isTRUE(gate$ready)) {
#     # existing database_input_gate PASS / RECOVERED logic
#   } else {
#     # existing runner_pause() contract 사용
#     # gate$runner_state 및 user guidance를 state/run log에 기록
#   }
#
# 새 35번째 stage를 추가하지 않는다.
# database_input_gate의 HUMAN GATE classification을 유지한다.
# ------------------------------------------------------------


# ------------------------------------------------------------
# 13. Direct execution
#
# source() 또는 Rscript로 직접 실행해도 operational gate를
# 확인할 수 있다.
#
# WAITING_FOR_DATABASE_EXPORT는 software failure가 아니므로
# generic stop()으로 종료하지 않는다.
# ------------------------------------------------------------

task018_is_direct_execution <- function() {
  args <- commandArgs(
    trailingOnly = FALSE
  )

  file_arg <- grep(
    "^--file=",
    args,
    value = TRUE
  )

  if (length(file_arg) == 0L) {
    return(FALSE)
  }

  basename(
    sub(
      "^--file=",
      "",
      file_arg[[1]]
    )
  ) %in% c(
    "TASK018_database_input_gate_hardening.R",
    "TASK018_database_input_gate.R"
  )
}

if (task018_is_direct_execution()) {
  task018_run_database_input_gate()
}

# BEGIN TASK-019C USER-DIRECTED REFINEMENT INTEGRATION
#
# Wrapper around the existing TASK-018 gate.
# It preserves the existing database-input/refinement behavior, then
# offers one additional user-directed AI refinement option while the
# manual-database gate is waiting.
#
if (!exists(".task018_run_database_input_gate_pre_task019c", inherits = FALSE)) {
  .task018_run_database_input_gate_pre_task019c <- task018_run_database_input_gate
}

task018_run_database_input_gate <- function(..., task019c_prompt = TRUE) {
  result <- .task018_run_database_input_gate_pre_task019c(...)

  if (!isTRUE(task019c_prompt) || !interactive()) {
    return(result)
  }

  runner_state <- ""
  if (is.list(result) && !is.null(result$runner_state) && length(result$runner_state) > 0L) {
    runner_state <- as.character(result$runner_state[[1]])
  }

  eligible_states <- c(
    "WAITING_FOR_DATABASE_EXPORT",
    "WAITING_FOR_REFINED_DATABASE_SEARCH"
  )

  if (!(runner_state %in% eligible_states)) {
    return(result)
  }

  cat("\n")
  cat("============================================================\n")
  cat("ADDITIONAL USER-DIRECTED SEARCH REFINEMENT\n")
  cat("============================================================\n")
  cat("현재 검색식에 AI가 반영할 추가 수정 요청이 있습니까?\n\n")
  cat("[1] 아니오 — 현재 검색식으로 진행\n")
  cat("[2] 예 — 자연어로 추가 수정 요청 입력\n\n")

  choice <- trimws(readline("선택 [1-2]: "))

  if (!identical(choice, "2")) {
    return(result)
  }

  request <- trimws(readline(
    "AI에게 반영할 검색식 수정 요청을 입력하세요: "
  ))

  if (!nzchar(request)) {
    cat("입력된 요청이 없어 현재 검색식을 유지합니다.\n")
    return(result)
  }

  source(
    "R/TASK019C_user_directed_search_refinement.R",
    local = .GlobalEnv
  )

  refinement <- task019c_apply_user_refinement(
    request = request,
    databases = NULL,
    verbose = TRUE,
    run_r12_validation = TRUE
  )

  result$status <- "WAITING"
  result$runner_state <- "WAITING_FOR_USER_REFINED_DATABASE_SEARCH"
  result$ready <- FALSE
  result$user_directed_refinement <- refinement

  result
}
# END TASK-019C USER-DIRECTED REFINEMENT INTEGRATION
