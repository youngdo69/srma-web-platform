# ============================================================
# Universal SR-MA Agent Toolkit
# R/36_generate_risk_of_bias.R
#
# VERSION: UNIVERSAL-R36-2026-08-16G
#
# PURPOSE
# ------------------------------------------------------------
# Prepare and validate result-level risk-of-bias assessments for
# studies contributing to the final quantitative synthesis.
#
# RCTs:
#   RoB 2 (22 Aug 2019 framework)
#
# Non-randomized intervention studies:
#   ROBINS-I original 2016 framework
#
# CRITICAL TOOLKIT REQUIREMENTS
# ------------------------------------------------------------
# - Uses only exported openxlsx APIs for workbook formatting.
# - Heterogeneous upstream column types are normalized to character
#   before source aggregation to prevent bind_rows() type conflicts.
# 1. Do NOT assess every included paper automatically.
#    The default assessment universe is the unique set of studies/results
#    that actually contribute to a finalized meta-analysis manifest.
#
# 2. RoB is RESULT-SPECIFIC:
#    study + target outcome + target time point/estimand.
#
# 3. For every signalling question the Agent layer must produce:
#       - response
#       - evidence quotation/location or evidence summary
#       - rationale
#       - confidence
#
# 4. For every domain the Agent layer must produce:
#       - provisional judgement
#       - domain rationale
#
# 5. Authors can review/override provisional judgements in an editable
#    workbook. Final author judgements are authoritative.
#
# 6. No study IDs, drug names, disease names, or project-specific
#    clinical values are hard-coded in the universal core.
#
# 7. The script automatically discovers:
#       - final meta-analysis study universe
#       - verified study design
#       - target PDF
#       - RoB tool assignment
#
# 8. This R module writes AGENT TASKS, sends each canonical PDF to a
#    configurable multimodal Agent provider, receives structured provisional
#    assessments, validates them, and writes rob_agent_assessments.csv.
#
# 9. The default implemented provider adapter is the OpenAI Responses API.
#    Provider/model settings are environment-configurable; provider-specific
#    code is isolated from the universal RoB assessment contract.
#
# 10. The repo-root .env is loaded automatically before provider settings are
#     read. Existing OS/R-session environment variables take precedence; .env
#     only fills values that are currently unset or empty.
#
# OUTPUT
# ------------------------------------------------------------
# results/risk_of_bias/
#   rob_study_result_manifest.csv
#   rob_agent_tasks.csv
#   rob_agent_tasks.jsonl
#   rob_agent_assessments.csv               [after Agent step]
#   risk_of_bias_author_editable.xlsx
#   rob2_final.csv
#   robins_i_final.csv
#   rob_validation_report.txt
#
# NOTE ON QUESTION TEXT
# ------------------------------------------------------------
# The script uses compact semantic signalling-question labels rather than
# redistributing the full official tool wording. The Agent must apply the
# official RoB 2 (2019) / ROBINS-I (2016) guidance when interpreting them.
# ============================================================


# ============================================================
# 0. Packages
# ============================================================

required_packages <- c(
  "yaml",
  "readr",
  "dplyr",
  "tibble",
  "stringr",
  "purrr",
  "jsonlite",
  "openxlsx",
  "httr2",
  "base64enc"
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
# 1. Root / paths
# ============================================================

repo_root <- normalizePath(
  ".",
  winslash = "/",
  mustWork = TRUE
)


# ============================================================
# 1A. Repo-root .env loader
# ============================================================
#
# UNIVERSAL TOOLKIT POLICY
# ------------------------------------------------------------
# - Read <repo_root>/.env automatically when present.
# - Existing OS / R-session environment variables ALWAYS take precedence.
# - .env only fills variables that are currently unset or empty.
# - Blank lines and comment lines are ignored.
# - Optional leading "export " is accepted.
# - Single- and double-quoted values are supported.
# - Inline comments are removed only for unquoted values.
# - The API key/value is NEVER printed to the console or report.
#
# Example:
#   OPENAI_API_KEY=...
#   SRMA_ROB_AGENT_PROVIDER=openai
#   SRMA_ROB_AGENT_MODEL=gpt-5.6
#   SRMA_ROB_AGENT_RUN=true
#   SRMA_ROB_FORCE_RERUN=false
# ============================================================

strip_env_quotes <- function(x) {

  x <- trimws(
    as.character(x)
  )

  if (nchar(x) >= 2L) {

    first_char <- substr(
      x,
      1L,
      1L
    )

    last_char <- substr(
      x,
      nchar(x),
      nchar(x)
    )

    if (
      (first_char == '"' && last_char == '"') ||
      (first_char == "'" && last_char == "'")
    ) {
      x <- substr(
        x,
        2L,
        nchar(x) - 1L
      )
    }
  }

  x
}


parse_env_line <- function(line) {

  original <- line
  line <- trimws(
    line
  )

  if (
    line == "" ||
    startsWith(
      line,
      "#"
    )
  ) {
    return(NULL)
  }

  line <- sub(
    "^export[[:space:]]+",
    "",
    line
  )

  eq_pos <- regexpr(
    "=",
    line,
    fixed = TRUE
  )

  if (
    eq_pos[[1]] < 1L
  ) {
    return(NULL)
  }

  key <- trimws(
    substr(
      line,
      1L,
      eq_pos[[1]] - 1L
    )
  )

  value <- trimws(
    substr(
      line,
      eq_pos[[1]] + 1L,
      nchar(line)
    )
  )

  if (
    key == "" ||
    !grepl(
      "^[A-Za-z_][A-Za-z0-9_]*$",
      key
    )
  ) {
    warning(
      paste0(
        "Skipping invalid .env entry: ",
        original
      ),
      call. = FALSE
    )

    return(NULL)
  }

  # Remove inline comments only from unquoted values.
  quoted_value <- (
    nchar(value) >= 2L &&
      (
        (
          substr(value, 1L, 1L) == '"' &&
            substr(value, nchar(value), nchar(value)) == '"'
        ) ||
          (
            substr(value, 1L, 1L) == "'" &&
              substr(value, nchar(value), nchar(value)) == "'"
          )
      )
  )

  if (!quoted_value) {

    value <- sub(
      "[[:space:]]+#.*$",
      "",
      value
    )

    value <- trimws(
      value
    )
  }

  value <- strip_env_quotes(
    value
  )

  list(
    key = key,
    value = value
  )
}


load_repo_env <- function(
  repo_root,
  filename = ".env"
) {

  env_file <- file.path(
    repo_root,
    filename
  )

  status <- list(
    file = normalizePath(
      env_file,
      winslash = "/",
      mustWork = FALSE
    ),
    found = file.exists(
      env_file
    ),
    loaded = character(),
    preserved = character(),
    skipped = character()
  )

  if (!status$found) {
    return(status)
  }

  lines <- readLines(
    env_file,
    warn = FALSE,
    encoding = "UTF-8"
  )

  for (
    line in lines
  ) {

    parsed <- parse_env_line(
      line
    )

    if (is.null(parsed)) {
      next
    }

    key <- parsed$key
    value <- parsed$value

    current_value <- Sys.getenv(
      key,
      unset = ""
    )

    # OS / pre-existing R environment takes precedence.
    if (
      !is.na(current_value) &&
      trimws(current_value) != ""
    ) {
      status$preserved <- c(
        status$preserved,
        key
      )

      next
    }

    if (
      is.na(value) ||
      value == ""
    ) {
      status$skipped <- c(
        status$skipped,
        key
      )

      next
    }

    do.call(
      Sys.setenv,
      stats::setNames(
        list(value),
        key
      )
    )

    status$loaded <- c(
      status$loaded,
      key
    )
  }

  status$loaded <- unique(
    status$loaded
  )

  status$preserved <- unique(
    status$preserved
  )

  status$skipped <- unique(
    status$skipped
  )

  status
}


repo_env_status <- load_repo_env(
  repo_root
)


normalize_slash <- function(x) {
  stringr::str_replace_all(
    as.character(x),
    "\\\\",
    "/"
  )
}


cat(
  ".env: ",
  if (
    isTRUE(
      repo_env_status$found
    )
  ) {
    "FOUND"
  } else {
    "NOT FOUND"
  },
  "\n",
  sep = ""
)

if (
  isTRUE(
    repo_env_status$found
  )
) {

  if (
    length(
      repo_env_status$loaded
    ) > 0L
  ) {
    cat(
      ".env variables loaded: ",
      paste(
        repo_env_status$loaded,
        collapse = ", "
      ),
      "\n",
      sep = ""
    )
  }

  if (
    length(
      repo_env_status$preserved
    ) > 0L
  ) {
    cat(
      "Existing environment variables preserved: ",
      paste(
        repo_env_status$preserved,
        collapse = ", "
      ),
      "\n",
      sep = ""
    )
  }
}


output_dir <- file.path(
  repo_root,
  "results",
  "risk_of_bias"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

task_dir <- file.path(
  output_dir,
  "agent_tasks"
)

dir.create(
  task_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

manifest_file <- file.path(
  output_dir,
  "rob_study_result_manifest.csv"
)

tasks_csv <- file.path(
  output_dir,
  "rob_agent_tasks.csv"
)

tasks_jsonl <- file.path(
  output_dir,
  "rob_agent_tasks.jsonl"
)

agent_output_csv <- file.path(
  output_dir,
  "rob_agent_assessments.csv"
)

generated_workbook <- file.path(
  output_dir,
  "risk_of_bias_generated.xlsx"
)

author_workbook <- file.path(
  output_dir,
  "risk_of_bias_author_editable.xlsx"
)

agent_raw_dir <- file.path(
  output_dir,
  "agent_raw"
)

dir.create(
  agent_raw_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

agent_checkpoint_dir <- file.path(
  output_dir,
  "agent_checkpoints"
)

dir.create(
  agent_checkpoint_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

rob2_final_csv <- file.path(
  output_dir,
  "rob2_final.csv"
)

robins_final_csv <- file.path(
  output_dir,
  "robins_i_final.csv"
)

report_file <- file.path(
  output_dir,
  "rob_validation_report.txt"
)


# ============================================================
# 2. Generic file discovery
# ============================================================


as_character_table <- function(df) {

  if (is.null(df)) {
    return(NULL)
  }

  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ as.character(.x)
      )
    )
}


read_table_any <- function(path) {

  ext <- tolower(
    tools::file_ext(path)
  )

  tryCatch(
    {
      if (ext == "csv") {
        return(
          readr::read_csv(
            path,
            show_col_types = FALSE,
            progress = FALSE,
            guess_max = 10000
          ) |>
            tibble::as_tibble() |>
            as_character_table()
        )
      }

      if (ext %in% c("xlsx", "xlsm")) {
        return(
          suppressWarnings(
            openxlsx::read.xlsx(
              path,
              sheet = 1,
              detectDates = TRUE,
              check.names = FALSE
            )
          ) |>
            tibble::as_tibble() |>
            as_character_table()
        )
      }

      NULL
    },
    warning = function(w) {
      NULL
    },
    error = function(e) {
      NULL
    }
  )
}


first_existing_col <- function(
  df,
  candidates
) {

  hit <- candidates[
    candidates %in% names(df)
  ]

  if (length(hit) == 0L) {
    return(NA_character_)
  }

  hit[[1]]
}


find_files <- function(pattern) {

  list.files(
    repo_root,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  ) |>
    normalize_slash()
}


# ============================================================
# 3. Discover finalized meta-analysis study/result universe
# ============================================================

# Priority:
# A. explicit final/meta-analysis manifests
# B. verified extraction files carrying meta_analysis_role/analysis_group
# C. final meta-analysis-ready datasets in results/data processed
#
# The resulting universe is de-duplicated by screening_id + target_result.

discover_meta_analysis_sources <- function() {

  files <- unique(
    c(
      find_files(
        "meta.*analysis.*manifest.*\\.(csv|xlsx)$"
      ),
      find_files(
        "targeted.*human.*verification.*\\.(csv|xlsx)$"
      ),
      find_files(
        "meta.*analysis.*ready.*\\.csv$"
      )
    )
  )

  # Avoid RoB outputs and obvious archives.
  files <- files[
    !grepl(
      "/risk_of_bias/|/archive/|/backup/|/old/",
      tolower(files),
      perl = TRUE
    )
  ]

  files
}


candidate_files <- discover_meta_analysis_sources()

candidate_tables <- lapply(
  candidate_files,
  function(path_i) {

    df <- read_table_any(
      path_i
    )

    if (
      is.null(df) ||
      nrow(df) == 0L
    ) {
      return(NULL)
    }

    screening_col <- first_existing_col(
      df,
      c(
        "screening_id",
        "screen_id"
      )
    )

    role_col <- first_existing_col(
      df,
      c(
        "meta_analysis_role",
        "analysis_group",
        "evidence_role"
      )
    )

    design_col <- first_existing_col(
      df,
      c(
        "human_verified_study_design",
        "final_study_design",
        "study_design"
      )
    )

    if (
      is.na(screening_col) ||
      is.na(role_col)
    ) {
      return(NULL)
    }

    df |>
      dplyr::mutate(
        .source_file = path_i,
        .screening_id = as.character(
          .data[[screening_col]]
        ),
        .meta_role = as.character(
          .data[[role_col]]
        ),
        .study_design = if (
          is.na(design_col)
        ) {
          NA_character_
        } else {
          as.character(
            .data[[design_col]]
          )
        }
      )
  }
)

candidate_tables <- candidate_tables[
  !vapply(
    candidate_tables,
    is.null,
    logical(1)
  )
]

if (length(candidate_tables) == 0L) {
  stop(
    paste0(
      "No finalized meta-analysis study universe could be discovered. ",
      "Expected a manifest, verified extraction file, or meta-analysis-ready dataset ",
      "containing screening_id and meta-analysis role."
    ),
    call. = FALSE
  )
}

candidate_tables <- lapply(
  candidate_tables,
  as_character_table
)

meta_pool <- dplyr::bind_rows(
  candidate_tables
) |>
  dplyr::filter(
    !is.na(.screening_id),
    trimws(.screening_id) != "",
    !is.na(.meta_role),
    trimws(.meta_role) != ""
  )


# ============================================================
# 4. Keep quantitative-synthesis roles only
# ============================================================

role_text <- tolower(
  trimws(
    meta_pool$.meta_role
  )
)

include_role <- grepl(
  "primary|secondary|sensitivity|meta",
  role_text
) &
  !grepl(
    "narrative|exclude|not used|not_used",
    role_text
  )

meta_pool <- meta_pool[
  include_role,
  ,
  drop = FALSE
]

if (nrow(meta_pool) == 0L) {
  stop(
    "No quantitative-synthesis studies remained after role filtering.",
    call. = FALSE
  )
}


# ============================================================
# 5. Canonical study metadata / PDF resolution
# ============================================================

discover_pdf <- function(screening_id) {

  exact_name <- paste0(
    screening_id,
    ".pdf"
  )

  hits <- find_files(
    paste0(
      "^",
      exact_name,
      "$"
    )
  )

  # Prefer canonical raw full-text folder.
  if (length(hits) > 1L) {

    score <- ifelse(
      grepl(
        "/data/raw/full_text/pdfs/",
        tolower(hits),
        fixed = TRUE
      ),
      100,
      0
    )

    hits <- hits[
      order(
        score,
        decreasing = TRUE
      )
    ]
  }

  if (length(hits) == 0L) {
    return(
      NA_character_
    )
  }

  hits[[1]]
}


infer_design_class <- function(x) {

  z <- tolower(
    paste(
      x,
      collapse = " "
    )
  )

  if (
    grepl(
      "random",
      z
    )
  ) {
    return(
      "RCT"
    )
  }

  if (
    grepl(
      "observ|cohort|retrospective|prospective|propensity|causal inference",
      z
    )
  ) {
    return(
      "NRSI"
    )
  }

  NA_character_
}


study_metadata <- meta_pool |>
  dplyr::group_by(
    .screening_id
  ) |>
  dplyr::summarise(
    meta_analysis_role = paste(
      unique(
        .meta_role[
          !is.na(.meta_role)
        ]
      ),
      collapse = "; "
    ),
    discovered_design = infer_design_class(
      .study_design
    ),
    source_files = paste(
      unique(
        .source_file
      ),
      collapse = "; "
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    pdf_path = vapply(
      .screening_id,
      discover_pdf,
      character(1)
    )
  )


# ============================================================
# 6. Determine target result
# ============================================================

# The project may expose outcome/time-window columns in the meta-analysis
# datasets. We collect them if available; otherwise create an unresolved
# target_result that the Agent/author must resolve before final judgement.

outcome_col <- first_existing_col(
  meta_pool,
  c(
    "outcome_name",
    "outcome",
    "endpoint",
    "analysis_outcome"
  )
)

time_col <- first_existing_col(
  meta_pool,
  c(
    "time_window",
    "follow_up_window",
    "outcome_timepoint",
    "timepoint"
  )
)

target_result_map <- meta_pool |>
  dplyr::mutate(
    .outcome = if (
      is.na(outcome_col)
    ) {
      NA_character_
    } else {
      as.character(
        .data[[outcome_col]]
      )
    },
    .time = if (
      is.na(time_col)
    ) {
      NA_character_
    } else {
      as.character(
        .data[[time_col]]
      )
    }
  ) |>
  dplyr::group_by(
    .screening_id
  ) |>
  dplyr::summarise(
    target_result = paste(
      unique(
        trimws(
          paste(
            .outcome,
            .time
          )
        )
      )[
        !is.na(
          unique(
            trimws(
              paste(
                .outcome,
                .time
              )
            )
          )
        ) &
          unique(
            trimws(
              paste(
                .outcome,
                .time
              )
            )
          ) != ""
      ],
      collapse = "; "
    ),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    target_result = dplyr::if_else(
      target_result == "",
      "USER/AGENT REQUIRED: identify exact synthesized result",
      target_result
    )
  )


manifest <- study_metadata |>
  dplyr::left_join(
    target_result_map,
    by = ".screening_id"
  ) |>
  dplyr::mutate(
    rob_tool = dplyr::case_when(
      discovered_design == "RCT" ~ "RoB 2 (2019)",
      discovered_design == "NRSI" ~ "ROBINS-I (2016)",
      TRUE ~ "USER REQUIRED"
    ),
    pdf_status = dplyr::if_else(
      is.na(pdf_path) |
        pdf_path == "",
      "MISSING",
      "AVAILABLE"
    )
  ) |>
  dplyr::rename(
    screening_id = .screening_id,
    study_design_class = discovered_design
  ) |>
  dplyr::distinct(
    screening_id,
    .keep_all = TRUE
  )


readr::write_csv(
  manifest,
  manifest_file,
  na = ""
)


# ============================================================
# 7. Signalling-question banks
# ============================================================
#
# Compact semantic labels keyed to the official domains.
# The Agent must use the official guidance documents when answering.
#
# Allowed response vocabulary:
#   RoB 2: Y / PY / PN / N / NI / NA
#   ROBINS-I: Y / PY / PN / N / NI / NA
#
# Provisional domain judgements:
#   RoB 2: Low / Some concerns / High
#   ROBINS-I: Low / Moderate / Serious / Critical / No information
# ============================================================

rob2_questions <- tibble::tribble(
  ~domain_id, ~domain, ~question_id, ~signalling_question,
  "D1", "Randomization process", "1.1", "Was the allocation sequence random?",
  "D1", "Randomization process", "1.2", "Was allocation concealed until participants were enrolled and assigned?",
  "D1", "Randomization process", "1.3", "Do baseline imbalances suggest a problem with randomization?",
  "D2", "Deviations from intended interventions", "2.1", "Were participants aware of assigned intervention during the trial?",
  "D2", "Deviations from intended interventions", "2.2", "Were carers or intervention providers aware of assigned intervention?",
  "D2", "Deviations from intended interventions", "2.3", "Were deviations from intended intervention caused by the trial context?",
  "D2", "Deviations from intended interventions", "2.4", "Could those deviations have affected the outcome?",
  "D2", "Deviations from intended interventions", "2.5", "Were such deviations balanced between groups?",
  "D2", "Deviations from intended interventions", "2.6", "Was the analysis appropriate for the effect of assignment to intervention?",
  "D2", "Deviations from intended interventions", "2.7", "Could failure to analyse participants in their randomized groups materially affect the result?",
  "D3", "Missing outcome data", "3.1", "Were outcome data available for all or nearly all randomized participants?",
  "D3", "Missing outcome data", "3.2", "Is there evidence that the result was not biased by missing outcome data?",
  "D3", "Missing outcome data", "3.3", "Could missingness depend on the true outcome value?",
  "D3", "Missing outcome data", "3.4", "Is it likely that missingness depended on the true outcome value?",
  "D4", "Measurement of the outcome", "4.1", "Was the outcome measurement method inappropriate?",
  "D4", "Measurement of the outcome", "4.2", "Could outcome measurement or ascertainment differ between groups?",
  "D4", "Measurement of the outcome", "4.3", "Were outcome assessors aware of intervention received?",
  "D4", "Measurement of the outcome", "4.4", "Could assessor knowledge influence outcome assessment?",
  "D4", "Measurement of the outcome", "4.5", "Is it likely that assessor knowledge influenced outcome assessment?",
  "D5", "Selection of the reported result", "5.1", "Were data analysed according to a prespecified plan finalized before unblinded outcome data were available?",
  "D5", "Selection of the reported result", "5.2", "Is the reported numerical result likely selected from multiple eligible outcome measurements?",
  "D5", "Selection of the reported result", "5.3", "Is the reported numerical result likely selected from multiple eligible analyses?"
)


robins_questions <- tibble::tribble(
  ~domain_id, ~domain, ~question_id, ~signalling_question,
  "D1", "Bias due to confounding", "1.1", "Is there potential for confounding of the intervention effect?",
  "D1", "Bias due to confounding", "1.2", "Was analysis based on participant follow-up split according to intervention received?",
  "D1", "Bias due to confounding", "1.3", "Were important baseline confounding domains measured validly and reliably?",
  "D1", "Bias due to confounding", "1.4", "Were important confounders controlled appropriately in the analysis?",
  "D1", "Bias due to confounding", "1.5", "Did adjustment avoid controlling for post-intervention variables affected by intervention?",
  "D2", "Selection of participants into the study", "2.1", "Was selection into the study related to intervention and outcome?",
  "D2", "Selection of participants into the study", "2.2", "Did selection into the analysis occur after intervention initiation?",
  "D2", "Selection of participants into the study", "2.3", "Could selection processes induce an association between intervention and outcome?",
  "D2", "Selection of participants into the study", "2.4", "Were appropriate methods used to correct selection bias?",
  "D3", "Classification of interventions", "3.1", "Were intervention groups clearly defined?",
  "D3", "Classification of interventions", "3.2", "Was intervention information recorded at intervention start?",
  "D3", "Classification of interventions", "3.3", "Could intervention classification be affected by knowledge of outcome or outcome risk?",
  "D4", "Deviations from intended interventions", "4.1", "Were there deviations from usual care beyond the intervention being compared?",
  "D4", "Deviations from intended interventions", "4.2", "Were important co-interventions balanced or appropriately handled?",
  "D4", "Deviations from intended interventions", "4.3", "Was intervention implementation or adherence adequate for the target effect?",
  "D4", "Deviations from intended interventions", "4.4", "Was an appropriate analysis used for the target intervention effect?",
  "D5", "Missing data", "5.1", "Were outcome data available for all or nearly all participants?",
  "D5", "Missing data", "5.2", "Were participants excluded because of missing intervention status?",
  "D5", "Missing data", "5.3", "Were participants excluded because of missing covariate data needed for analysis?",
  "D5", "Missing data", "5.4", "Could missingness depend on the true outcome value?",
  "D5", "Missing data", "5.5", "Were appropriate methods used to address missing data?",
  "D6", "Measurement of outcomes", "6.1", "Could outcome measurement be influenced by knowledge of intervention received?",
  "D6", "Measurement of outcomes", "6.2", "Were outcome assessors aware of intervention received?",
  "D6", "Measurement of outcomes", "6.3", "Were outcome assessment methods comparable across intervention groups?",
  "D6", "Measurement of outcomes", "6.4", "Were systematic outcome-measurement errors related to intervention status likely?",
  "D7", "Selection of the reported result", "7.1", "Was the reported result selected from multiple outcome measurements?",
  "D7", "Selection of the reported result", "7.2", "Was the reported result selected from multiple analyses of the intervention-outcome relationship?",
  "D7", "Selection of the reported result", "7.3", "Was selection of the reported result likely based on findings?"
)


# ============================================================
# 8. Expand manifest to agent signalling-question tasks
# ============================================================

tasks <- purrr::map_dfr(
  seq_len(
    nrow(manifest)
  ),
  function(i) {

    m <- manifest[
      i,
      ,
      drop = FALSE
    ]

    bank <- if (
      m$study_design_class[[1]] == "RCT"
    ) {
      rob2_questions
    } else if (
      m$study_design_class[[1]] == "NRSI"
    ) {
      robins_questions
    } else {
      tibble::tibble()
    }

    if (nrow(bank) == 0L) {
      return(
        tibble::tibble()
      )
    }

    bank |>
      dplyr::mutate(
        screening_id = m$screening_id[[1]],
        meta_analysis_role = m$meta_analysis_role[[1]],
        target_result = m$target_result[[1]],
        study_design_class = m$study_design_class[[1]],
        rob_tool = m$rob_tool[[1]],
        pdf_path = m$pdf_path[[1]],
        agent_response = NA_character_,
        evidence_location = NA_character_,
        evidence_summary = NA_character_,
        rationale = NA_character_,
        agent_confidence = NA_character_,
        provisional_domain_judgement = NA_character_,
        domain_rationale = NA_character_,
        agent_overall_provisional_judgement = NA_character_,
        agent_overall_rationale = NA_character_
      ) |>
      dplyr::select(
        screening_id,
        meta_analysis_role,
        target_result,
        study_design_class,
        rob_tool,
        pdf_path,
        domain_id,
        domain,
        question_id,
        signalling_question,
        agent_response,
        evidence_location,
        evidence_summary,
        rationale,
        agent_confidence,
        provisional_domain_judgement,
        domain_rationale,
        agent_overall_provisional_judgement,
        agent_overall_rationale
      )
  }
)


tasks <- tasks |>
  dplyr::mutate(
    screening_id = as.character(
      screening_id
    ),
    domain_id = as.character(
      domain_id
    ),
    question_id = as.character(
      question_id
    )
  )

readr::write_csv(
  tasks,
  tasks_csv,
  na = ""
)


# ============================================================
# 9. JSONL task contract for multimodal Agent
# ============================================================

agent_task_objects <- split(
  tasks,
  tasks$screening_id
)

json_lines <- vapply(
  names(agent_task_objects),
  function(study_id) {

    study_tasks <- agent_task_objects[[study_id]]

    obj <- list(
      task_type = "risk_of_bias_result_assessment",
      screening_id = study_id,
      rob_tool = unique(
        study_tasks$rob_tool
      ),
      study_design_class = unique(
        study_tasks$study_design_class
      ),
      meta_analysis_role = unique(
        study_tasks$meta_analysis_role
      ),
      target_result = unique(
        study_tasks$target_result
      ),
      pdf_path = unique(
        study_tasks$pdf_path
      ),
      instructions = paste0(
        "Read the full PDF. Assess the specified synthesized result, not the paper globally. ",
        "For every signalling question return response, evidence location, concise evidence summary, ",
        "rationale, and confidence. Then return a provisional judgement and rationale for each domain. ",
        "Do not infer absent methods as performed; use No information when evidence is insufficient. ",
        "Apply the official guidance for the stated tool."
      ),
      signalling_questions = lapply(
        seq_len(
          nrow(
            study_tasks
          )
        ),
        function(j) {

          list(
            domain_id = study_tasks$domain_id[[j]],
            domain = study_tasks$domain[[j]],
            question_id = study_tasks$question_id[[j]],
            signalling_question = study_tasks$signalling_question[[j]]
          )
        }
      ),
      required_output_fields = c(
        "screening_id",
        "domain_id",
        "domain",
        "question_id",
        "agent_response",
        "evidence_location",
        "evidence_summary",
        "rationale",
        "agent_confidence",
        "provisional_domain_judgement",
        "domain_rationale"
      )
    )

    jsonlite::toJSON(
      obj,
      auto_unbox = TRUE,
      null = "null"
    )
  },
  character(1)
)

writeLines(
  json_lines,
  tasks_jsonl
)


# ============================================================
# 10. Per-study human-readable Agent prompts
# ============================================================

for (
  study_id in names(
    agent_task_objects
  )
) {

  s <- agent_task_objects[[study_id]]

  prompt_file <- file.path(
    task_dir,
    paste0(
      study_id,
      "_rob_agent_task.md"
    )
  )

  question_lines <- paste0(
    "- ",
    s$domain_id,
    " / ",
    s$question_id,
    " — ",
    s$signalling_question
  )

  prompt_lines <- c(
    paste0(
      "# Risk-of-bias Agent Task — ",
      study_id
    ),
    "",
    paste0(
      "**Tool:** ",
      unique(
        s$rob_tool
      )
    ),
    paste0(
      "**Target result:** ",
      unique(
        s$target_result
      )
    ),
    paste0(
      "**PDF:** ",
      unique(
        s$pdf_path
      )
    ),
    "",
    "## Agent requirements",
    "",
    "Assess the specified result, not the study globally.",
    "Read the full PDF and available supplements when present.",
    "For every signalling question provide:",
    "- response",
    "- evidence location",
    "- evidence summary",
    "- rationale",
    "- confidence",
    "",
    "For each domain provide:",
    "- provisional domain judgement",
    "- domain rationale",
    "",
    "Do not invent unreported methods.",
    "If evidence is insufficient, explicitly record No information.",
    "",
    "## Signalling questions",
    "",
    question_lines
  )

  writeLines(
    prompt_lines,
    prompt_file
  )
}


# ============================================================
# 11. Automated multimodal RoB Agent
# ============================================================
#
# This section performs the actual PDF-reading step.
#
# Default provider: OpenAI Responses API.
# The provider/model are configurable through environment variables:
#
#   SRMA_ROB_AGENT_PROVIDER=openai
#   SRMA_ROB_AGENT_MODEL=gpt-5.6
#   SRMA_ROB_AGENT_RUN=true
#   SRMA_ROB_FORCE_RERUN=false
#
# For the OpenAI provider:
#   OPENAI_API_KEY must be available in the environment.
#
# PDF files are sent one study at a time. The model receives the actual PDF
# as a multimodal file input plus all signalling questions for that study.
# The model returns strict structured JSON, which is validated and flattened
# into rob_agent_assessments.csv.
#
# IMPORTANT:
# - Agent outputs are PROVISIONAL.
# - Author-final judgements remain authoritative.
# - Existing successful per-study checkpoints are reused by default.
# ============================================================

rob_agent_provider <- tolower(
  trimws(
    Sys.getenv(
      "SRMA_ROB_AGENT_PROVIDER",
      unset = "openai"
    )
  )
)

rob_agent_model <- trimws(
  Sys.getenv(
    "SRMA_ROB_AGENT_MODEL",
    unset = "gpt-5.6"
  )
)

rob_agent_run <- tolower(
  trimws(
    Sys.getenv(
      "SRMA_ROB_AGENT_RUN",
      unset = "true"
    )
  )
) %in% c(
  "1",
  "true",
  "yes",
  "y"
)

rob_force_rerun <- tolower(
  trimws(
    Sys.getenv(
      "SRMA_ROB_FORCE_RERUN",
      unset = "false"
    )
  )
) %in% c(
  "1",
  "true",
  "yes",
  "y"
)


build_rob_output_schema <- function(
  rob_tool
) {

  domain_judgements <- if (
    grepl(
      "^RoB 2",
      rob_tool
    )
  ) {
    c(
      "Low",
      "Some concerns",
      "High"
    )
  } else {
    c(
      "Low",
      "Moderate",
      "Serious",
      "Critical",
      "No information"
    )
  }

  list(
    type = "object",
    properties = list(
      screening_id = list(
        type = "string"
      ),
      question_assessments = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            domain_id = list(
              type = "string"
            ),
            question_id = list(
              type = "string"
            ),
            agent_response = list(
              type = "string",
              enum = c(
                "Y",
                "PY",
                "PN",
                "N",
                "NI",
                "NA"
              )
            ),
            evidence_location = list(
              type = "string"
            ),
            evidence_summary = list(
              type = "string"
            ),
            rationale = list(
              type = "string"
            ),
            agent_confidence = list(
              type = "string",
              enum = c(
                "High",
                "Moderate",
                "Low"
              )
            )
          ),
          required = c(
            "domain_id",
            "question_id",
            "agent_response",
            "evidence_location",
            "evidence_summary",
            "rationale",
            "agent_confidence"
          ),
          additionalProperties = FALSE
        )
      ),
      domain_assessments = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            domain_id = list(
              type = "string"
            ),
            provisional_domain_judgement = list(
              type = "string",
              enum = domain_judgements
            ),
            domain_rationale = list(
              type = "string"
            )
          ),
          required = c(
            "domain_id",
            "provisional_domain_judgement",
            "domain_rationale"
          ),
          additionalProperties = FALSE
        )
      ),
      agent_overall_provisional_judgement = list(
        type = "string",
        enum = domain_judgements
      ),
      agent_overall_rationale = list(
        type = "string"
      )
    ),
    required = c(
      "screening_id",
      "question_assessments",
      "domain_assessments",
      "agent_overall_provisional_judgement",
      "agent_overall_rationale"
    ),
    additionalProperties = FALSE
  )
}


build_rob_agent_prompt <- function(
  study_tasks
) {

  tool <- unique(
    study_tasks$rob_tool
  )

  target_result <- unique(
    study_tasks$target_result
  )

  role <- unique(
    study_tasks$meta_analysis_role
  )

  question_text <- paste(
    paste0(
      study_tasks$domain_id,
      " / ",
      study_tasks$question_id,
      " / ",
      study_tasks$domain,
      ": ",
      study_tasks$signalling_question
    ),
    collapse = "\n"
  )

  judgement_guidance <- if (
    grepl(
      "^RoB 2",
      tool
    )
  ) {
    paste0(
      "Use RoB 2 (2019) result-level logic. Domain and overall provisional ",
      "judgements must be Low, Some concerns, or High. "
    )
  } else {
    paste0(
      "Use ROBINS-I original 2016 result-level logic, considering bias relative ",
      "to a well-conducted randomized trial addressing the same causal question. ",
      "Domain and overall provisional judgements must be Low, Moderate, Serious, ",
      "Critical, or No information. "
    )
  }

  paste0(
    "You are the Risk-of-Bias Assessor Agent in a systematic-review toolkit.\n\n",
    "ASSESSMENT UNIT\n",
    "Assess the SPECIFIC SYNTHESIZED RESULT, not the paper globally.\n",
    "Screening ID: ", unique(study_tasks$screening_id), "\n",
    "Tool: ", tool, "\n",
    "Meta-analysis role: ", role, "\n",
    "Target result: ", target_result, "\n\n",
    judgement_guidance,
    "\n\nEVIDENCE RULES\n",
    "- Read the full supplied PDF, including methods, results, tables, figures, ",
    "appendices visible in the PDF, trial registration/protocol information reported ",
    "inside the PDF, and relevant footnotes.\n",
    "- Do not assume an unreported safeguard was performed.\n",
    "- Distinguish absence of evidence from evidence of absence.\n",
    "- Use NI when the paper does not provide enough information.\n",
    "- Use NA only when a signalling question truly does not apply.\n",
    "- Keep every answer tied to the target result and its relevant analysis population/time window.\n",
    "- evidence_location should identify page/section/table/figure when visible; ",
    "if page numbering is unavailable, give the most specific section/table locator possible.\n",
    "- evidence_summary must paraphrase the supporting evidence; do not fabricate quotations.\n",
    "- rationale should explain why that evidence leads to the signalling response.\n",
    "- confidence refers to confidence in the evidence-based provisional assessment, ",
    "not statistical confidence.\n\n",
    "SIGNALLING RESPONSE VOCABULARY\n",
    "Y = Yes; PY = Probably yes; PN = Probably no; N = No; ",
    "NI = No information; NA = Not applicable.\n\n",
    "SIGNALLING QUESTIONS\n",
    question_text,
    "\n\n",
    "Return exactly one question assessment for every supplied question_id and exactly ",
    "one domain assessment for every domain_id represented above. Also provide a provisional ",
    "overall judgement and rationale. The overall judgement remains provisional until author review."
  )
}


extract_openai_output_text <- function(
  response_body
) {

  if (
    !is.null(
      response_body$output_text
    ) &&
    length(
      response_body$output_text
    ) > 0L
  ) {
    return(
      paste(
        unlist(
          response_body$output_text
        ),
        collapse = ""
      )
    )
  }

  output_items <- response_body$output

  if (
    is.null(
      output_items
    )
  ) {
    return(
      NA_character_
    )
  }

  text_parts <- character()

  for (
    item in output_items
  ) {

    if (
      is.null(
        item$content
      )
    ) {
      next
    }

    for (
      content_item in item$content
    ) {

      if (
        !is.null(
          content_item$type
        ) &&
        identical(
          content_item$type,
          "output_text"
        ) &&
        !is.null(
          content_item$text
        )
      ) {
        text_parts <- c(
          text_parts,
          as.character(
            content_item$text
          )
        )
      }
    }
  }

  if (length(text_parts) == 0L) {
    return(
      NA_character_
    )
  }

  paste(
    text_parts,
    collapse = ""
  )
}


validate_agent_study_result <- function(
  parsed,
  study_tasks
) {

  study_id <- unique(
    study_tasks$screening_id
  )

  if (
    is.null(
      parsed$screening_id
    ) ||
    as.character(
      parsed$screening_id
    ) != study_id
  ) {
    stop(
      "Agent returned a mismatched screening_id for ",
      study_id,
      ".",
      call. = FALSE
    )
  }

  qa <- parsed$question_assessments

  if (
    is.null(
      qa
    ) ||
    !is.data.frame(
      qa
    )
  ) {
    qa <- tibble::as_tibble(
      qa
    )
  }

  da <- parsed$domain_assessments

  if (
    is.null(
      da
    ) ||
    !is.data.frame(
      da
    )
  ) {
    da <- tibble::as_tibble(
      da
    )
  }

  expected_q <- study_tasks |>
    dplyr::distinct(
      domain_id,
      question_id
    )

  observed_q <- qa |>
    dplyr::transmute(
      domain_id = as.character(
        domain_id
      ),
      question_id = as.character(
        question_id
      )
    )

  missing_q <- expected_q |>
    dplyr::anti_join(
      observed_q,
      by = c(
        "domain_id",
        "question_id"
      )
    )

  unexpected_q <- observed_q |>
    dplyr::anti_join(
      expected_q,
      by = c(
        "domain_id",
        "question_id"
      )
    )

  if (
    nrow(
      missing_q
    ) > 0L ||
    nrow(
      unexpected_q
    ) > 0L ||
    nrow(
      observed_q
    ) != nrow(
      expected_q
    )
  ) {
    stop(
      "Agent signalling-question keys are incomplete or unexpected for ",
      study_id,
      ".",
      call. = FALSE
    )
  }

  expected_domains <- unique(
    study_tasks$domain_id
  )

  observed_domains <- unique(
    as.character(
      da$domain_id
    )
  )

  if (
    !setequal(
      expected_domains,
      observed_domains
    )
  ) {
    stop(
      "Agent domain assessment set is incomplete for ",
      study_id,
      ".",
      call. = FALSE
    )
  }

  TRUE
}


flatten_agent_study_result <- function(
  parsed,
  study_tasks
) {

  qa <- tibble::as_tibble(
    parsed$question_assessments
  ) |>
    dplyr::mutate(
      domain_id = as.character(
        domain_id
      ),
      question_id = as.character(
        question_id
      )
    )

  da <- tibble::as_tibble(
    parsed$domain_assessments
  ) |>
    dplyr::mutate(
      domain_id = as.character(
        domain_id
      )
    )

  study_tasks <- study_tasks |>
    dplyr::mutate(
      screening_id = as.character(
        screening_id
      ),
      domain_id = as.character(
        domain_id
      ),
      question_id = as.character(
        question_id
      )
    )

  study_tasks |>
    dplyr::select(
      screening_id,
      meta_analysis_role,
      target_result,
      study_design_class,
      rob_tool,
      pdf_path,
      domain_id,
      domain,
      question_id,
      signalling_question
    ) |>
    dplyr::left_join(
      qa |>
        dplyr::select(
          domain_id,
          question_id,
          agent_response,
          evidence_location,
          evidence_summary,
          rationale,
          agent_confidence
        ),
      by = c(
        "domain_id",
        "question_id"
      )
    ) |>
    dplyr::left_join(
      da |>
        dplyr::select(
          domain_id,
          provisional_domain_judgement,
          domain_rationale
        ),
      by = "domain_id"
    ) |>
    dplyr::mutate(
      agent_overall_provisional_judgement = as.character(
        parsed$agent_overall_provisional_judgement
      ),
      agent_overall_rationale = as.character(
        parsed$agent_overall_rationale
      )
    )
}


run_openai_rob_agent <- function(
  study_tasks,
  model,
  api_key
) {

  study_id <- unique(
    study_tasks$screening_id
  )

  pdf_path <- unique(
    study_tasks$pdf_path
  )

  if (
    length(
      pdf_path
    ) != 1L ||
    is.na(
      pdf_path
    ) ||
    pdf_path == "" ||
    !file.exists(
      pdf_path
    )
  ) {
    stop(
      "Canonical PDF not available for ",
      study_id,
      ": ",
      paste(
        pdf_path,
        collapse = "; "
      ),
      call. = FALSE
    )
  }

  pdf_size <- file.info(
    pdf_path
  )$size

  if (
    is.na(
      pdf_size
    ) ||
    pdf_size <= 0
  ) {
    stop(
      "PDF is empty or unreadable for ",
      study_id,
      ".",
      call. = FALSE
    )
  }

  # OpenAI file-input constraint: each PDF must remain below 50 MB.
  if (
    pdf_size >= 50 * 1024 * 1024
  ) {
    stop(
      "PDF exceeds the 50 MB per-file input limit for ",
      study_id,
      ".",
      call. = FALSE
    )
  }

  pdf_base64 <- base64enc::base64encode(
    pdf_path
  )

  prompt_text <- build_rob_agent_prompt(
    study_tasks
  )

  schema <- build_rob_output_schema(
    unique(
      study_tasks$rob_tool
    )
  )

  request_body <- list(
    model = model,
    store = FALSE,
    input = list(
      list(
        role = "user",
        content = list(
          list(
            type = "input_file",
            filename = basename(
              pdf_path
            ),
            file_data = paste0(
              "data:application/pdf;base64,",
              pdf_base64
            ),
            detail = "high"
          ),
          list(
            type = "input_text",
            text = prompt_text
          )
        )
      )
    ),
    text = list(
      format = list(
        type = "json_schema",
        name = "risk_of_bias_assessment",
        strict = TRUE,
        schema = schema
      )
    )
  )

  req <- httr2::request(
    "https://api.openai.com/v1/responses"
  ) |>
    httr2::req_headers(
      Authorization = paste(
        "Bearer",
        api_key
      ),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(
      request_body,
      auto_unbox = TRUE,
      null = "null"
    ) |>
    httr2::req_timeout(
      seconds = 900
    )

  last_error <- NULL
  response <- NULL

  for (
    attempt in seq_len(
      3L
    )
  ) {

    response <- tryCatch(
      httr2::req_perform(
        req
      ),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(response)) {
      break
    }

    if (attempt < 3L) {
      Sys.sleep(
        5 * attempt
      )
    }
  }

  if (is.null(response)) {
    stop(
      "OpenAI RoB Agent request failed for ",
      study_id,
      ": ",
      conditionMessage(
        last_error
      ),
      call. = FALSE
    )
  }

  body <- httr2::resp_body_json(
    response,
    simplifyVector = FALSE
  )

  raw_response_file <- file.path(
    agent_raw_dir,
    paste0(
      study_id,
      "_openai_response.json"
    )
  )

  writeLines(
    jsonlite::toJSON(
      body,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null"
    ),
    raw_response_file
  )

  output_text <- extract_openai_output_text(
    body
  )

  if (
    is.na(
      output_text
    ) ||
    trimws(
      output_text
    ) == ""
  ) {
    stop(
      "No structured output_text was returned for ",
      study_id,
      ".",
      call. = FALSE
    )
  }

  parsed <- jsonlite::fromJSON(
    output_text,
    simplifyVector = TRUE
  )

  validate_agent_study_result(
    parsed,
    study_tasks
  )

  flattened <- flatten_agent_study_result(
    parsed,
    study_tasks
  )

  checkpoint_file <- file.path(
    agent_checkpoint_dir,
    paste0(
      study_id,
      "_assessment.csv"
    )
  )

  readr::write_csv(
    flattened,
    checkpoint_file,
    na = ""
  )

  flattened
}


run_multimodal_rob_agent <- function() {

  if (!rob_agent_run) {
    message(
      "SRMA_ROB_AGENT_RUN=false: automated PDF assessment skipped."
    )
    return(
      FALSE
    )
  }

  if (!identical(
    rob_agent_provider,
    "openai"
  )) {
    stop(
      paste0(
        "Unsupported SRMA_ROB_AGENT_PROVIDER='",
        rob_agent_provider,
        "'. This R/36 build implements the OpenAI Responses API adapter. ",
        "Additional providers should be added as provider adapters without ",
        "changing the RoB assessment contract."
      ),
      call. = FALSE
    )
  }

  api_key <- trimws(
    Sys.getenv(
      "OPENAI_API_KEY",
      unset = ""
    )
  )

  if (api_key == "") {
    stop(
      paste0(
        "OPENAI_API_KEY is not set. R/36 can discover PDFs and create tasks, ",
        "but actual multimodal provisional assessment requires an API-capable ",
        "model provider. Set OPENAI_API_KEY in the environment, or set ",
        "SRMA_ROB_AGENT_RUN=false to generate tasks only."
      ),
      call. = FALSE
    )
  }

  assessment_parts <- list()

  study_ids <- names(
    agent_task_objects
  )

  for (
    i in seq_along(
      study_ids
    )
  ) {

    study_id <- study_ids[[i]]
    study_tasks <- agent_task_objects[[study_id]]

    checkpoint_file <- file.path(
      agent_checkpoint_dir,
      paste0(
        study_id,
        "_assessment.csv"
      )
    )

    cat(
      "RoB Agent [",
      i,
      "/",
      length(
        study_ids
      ),
      "] ",
      study_id,
      " ... ",
      sep = ""
    )

    if (
      file.exists(
        checkpoint_file
      ) &&
      !rob_force_rerun
    ) {

      checkpoint <- readr::read_csv(
        checkpoint_file,
        show_col_types = FALSE,
        progress = FALSE,
        col_types = readr::cols(
          .default = readr::col_character()
        )
      ) |>
        dplyr::mutate(
          screening_id = as.character(
            screening_id
          ),
          domain_id = as.character(
            domain_id
          ),
          question_id = as.character(
            question_id
          )
        )

      expected_keys <- study_tasks |>
        dplyr::transmute(
          screening_id = as.character(
            screening_id
          ),
          domain_id = as.character(
            domain_id
          ),
          question_id = as.character(
            question_id
          )
        ) |>
        dplyr::distinct()

      checkpoint_keys <- checkpoint |>
        dplyr::transmute(
          screening_id = as.character(
            screening_id
          ),
          domain_id = as.character(
            domain_id
          ),
          question_id = as.character(
            question_id
          )
        ) |>
        dplyr::distinct()

      if (
        nrow(
          expected_keys |>
            dplyr::anti_join(
              checkpoint_keys,
              by = c(
                "screening_id",
                "domain_id",
                "question_id"
              )
            )
        ) == 0L
      ) {
        assessment_parts[[length(assessment_parts) + 1L]] <- checkpoint
        cat(
          "CHECKPOINT REUSED\n"
        )
        next
      }
    }

    assessment <- run_openai_rob_agent(
      study_tasks = study_tasks,
      model = rob_agent_model,
      api_key = api_key
    )

    assessment_parts[[length(assessment_parts) + 1L]] <- assessment

    cat(
      "DONE\n"
    )
  }

  combined <- dplyr::bind_rows(
    lapply(
      assessment_parts,
      as_character_table
    )
  )

  expected_total <- nrow(
    tasks
  )

  if (
    nrow(
      combined
    ) != expected_total
  ) {
    stop(
      paste0(
        "Combined Agent assessment row count mismatch: expected ",
        expected_total,
        ", got ",
        nrow(
          combined
        ),
        "."
      ),
      call. = FALSE
    )
  }

  readr::write_csv(
    combined,
    agent_output_csv,
    na = ""
  )

  TRUE
}


agent_execution_completed <- FALSE

if (
  rob_agent_run
) {
  agent_execution_completed <- run_multimodal_rob_agent()
}


# ============================================================
# 12. Validate/import Agent assessments when available
# ============================================================

required_agent_columns <- c(
  "screening_id",
  "domain_id",
  "domain",
  "question_id",
  "agent_response",
  "evidence_location",
  "evidence_summary",
  "rationale",
  "agent_confidence",
  "provisional_domain_judgement",
  "domain_rationale",
  "agent_overall_provisional_judgement",
  "agent_overall_rationale"
)

agent_assessment_available <- file.exists(
  agent_output_csv
)

if (agent_assessment_available) {

  agent_output <- readr::read_csv(
    agent_output_csv,
    show_col_types = FALSE,
    progress = FALSE,
    col_types = readr::cols(
      .default = readr::col_character()
    )
  ) |>
    dplyr::mutate(
      screening_id = as.character(
        screening_id
      ),
      domain_id = as.character(
        domain_id
      ),
      question_id = as.character(
        question_id
      )
    )

  missing_agent_columns <- setdiff(
    required_agent_columns,
    names(
      agent_output
    )
  )

  if (length(missing_agent_columns) > 0L) {
    stop(
      "Agent assessment output is missing required columns: ",
      paste(
        missing_agent_columns,
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  expected_keys <- tasks |>
    dplyr::transmute(
      screening_id = as.character(
        screening_id
      ),
      domain_id = as.character(
        domain_id
      ),
      question_id = as.character(
        question_id
      ),
      expected = TRUE
    )

  observed_keys <- agent_output |>
    dplyr::transmute(
      screening_id = as.character(
        screening_id
      ),
      domain_id = as.character(
        domain_id
      ),
      question_id = as.character(
        question_id
      ),
      observed = TRUE
    )

  key_check <- expected_keys |>
    dplyr::left_join(
      observed_keys,
      by = c(
        "screening_id",
        "domain_id",
        "question_id"
      )
    )

  if (
    any(
      is.na(
        key_check$observed
      )
    )
  ) {
    stop(
      "Agent output is incomplete: one or more signalling questions are missing.",
      call. = FALSE
    )
  }

} else {

  agent_output <- tasks
}


# ============================================================
# 13. Author-editable workbook
# ============================================================

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb,
  "Study_Result_Manifest"
)

openxlsx::writeData(
  wb,
  "Study_Result_Manifest",
  manifest
)

openxlsx::addWorksheet(
  wb,
  "Signalling_Questions"
)

openxlsx::writeData(
  wb,
  "Signalling_Questions",
  agent_output
)

# Domain-review sheet
domain_review <- agent_output |>
  dplyr::group_by(
    screening_id,
    rob_tool,
    domain_id,
    domain
  ) |>
  dplyr::summarise(
    provisional_domain_judgement = dplyr::first(
      provisional_domain_judgement[
        !is.na(
          provisional_domain_judgement
        ) &
          provisional_domain_judgement != ""
      ],
      default = NA_character_
    ),
    domain_rationale = dplyr::first(
      domain_rationale[
        !is.na(
          domain_rationale
        ) &
          domain_rationale != ""
      ],
      default = NA_character_
    ),
    author_domain_judgement = NA_character_,
    author_domain_rationale = NA_character_,
    author_override = NA_character_,
    .groups = "drop"
  )

openxlsx::addWorksheet(
  wb,
  "Domain_Review"
)

openxlsx::writeData(
  wb,
  "Domain_Review",
  domain_review
)

# Overall review is explicitly author-facing and carries the Agent's
# provisional overall assessment into the author-review workbook.
agent_overall <- agent_output |>
  dplyr::group_by(
    screening_id
  ) |>
  dplyr::summarise(
    agent_overall_provisional_judgement = dplyr::first(
      agent_overall_provisional_judgement[
        !is.na(agent_overall_provisional_judgement) &
          agent_overall_provisional_judgement != ""
      ],
      default = NA_character_
    ),
    agent_overall_rationale = dplyr::first(
      agent_overall_rationale[
        !is.na(agent_overall_rationale) &
          agent_overall_rationale != ""
      ],
      default = NA_character_
    ),
    .groups = "drop"
  )

overall_review <- manifest |>
  dplyr::select(
    screening_id,
    rob_tool,
    target_result
  ) |>
  dplyr::left_join(
    agent_overall,
    by = "screening_id"
  ) |>
  dplyr::mutate(
    author_final_overall_judgement = NA_character_,
    author_final_overall_rationale = NA_character_,
    author_reviewer = NA_character_,
    author_reviewed_at = NA_character_
  )

openxlsx::addWorksheet(
  wb,
  "Overall_Review"
)

openxlsx::writeData(
  wb,
  "Overall_Review",
  overall_review
)

openxlsx::addWorksheet(
  wb,
  "Instructions"
)

instructions <- tibble::tibble(
  item = c(
    "Assessment level",
    "Agent step",
    "Author step",
    "RoB 2 judgement vocabulary",
    "ROBINS-I judgement vocabulary",
    "Missing evidence",
    "Final authority"
  ),
  guidance = c(
    "Assess a specific synthesized result: study + outcome + time point/estimand.",
    "Agent reads the full PDF and returns signalling-question responses, evidence, rationale, confidence, and provisional domain judgements.",
    "Author reviews all provisional judgements and may override them with rationale.",
    "Low / Some concerns / High",
    "Low / Moderate / Serious / Critical / No information",
    "Do not infer unreported methods. Record No information where appropriate.",
    "Author-final judgements are authoritative for manuscript reporting."
  )
)

openxlsx::writeData(
  wb,
  "Instructions",
  instructions
)

header_style <- openxlsx::createStyle(
  textDecoration = "bold",
  wrapText = TRUE,
  valign = "center",
  border = "bottom"
)

sheet_names <- openxlsx::sheets(
  wb
)

for (
  sheet in sheet_names
) {

  openxlsx::freezePane(
    wb,
    sheet,
    firstActiveRow = 2,
    firstActiveCol = 2
  )

  # Apply conservative column widths without relying on non-exported
  # openxlsx internals such as getSheetDims().
  openxlsx::setColWidths(
    wb,
    sheet,
    cols = 1:30,
    widths = 18
  )

  openxlsx::addStyle(
    wb,
    sheet,
    header_style,
    rows = 1,
    cols = 1:30,
    gridExpand = TRUE,
    stack = TRUE
  )
}

openxlsx::saveWorkbook(
  wb,
  generated_workbook,
  overwrite = TRUE
)


author_workbook_has_manual_edits <- function(
  path
) {

  if (!file.exists(path)) {
    return(FALSE)
  }

  domain_existing <- tryCatch(
    openxlsx::read.xlsx(
      path,
      sheet = "Domain_Review",
      check.names = FALSE
    ),
    error = function(e) {
      NULL
    }
  )

  overall_existing <- tryCatch(
    openxlsx::read.xlsx(
      path,
      sheet = "Overall_Review",
      check.names = FALSE
    ),
    error = function(e) {
      NULL
    }
  )

  if (
    is.null(domain_existing) ||
    is.null(overall_existing)
  ) {
    # Unknown workbook structure: preserve conservatively.
    return(TRUE)
  }

  domain_author_cols <- intersect(
    c(
      "author_domain_judgement",
      "author_domain_rationale",
      "author_override"
    ),
    names(domain_existing)
  )

  overall_author_cols <- intersect(
    c(
      "author_final_overall_judgement",
      "author_final_overall_rationale",
      "author_reviewer",
      "author_reviewed_at"
    ),
    names(overall_existing)
  )

  has_domain_edit <- FALSE
  has_overall_edit <- FALSE

  if (length(domain_author_cols) > 0L) {
    vals <- unlist(
      domain_existing[
        domain_author_cols
      ],
      use.names = FALSE
    )

    has_domain_edit <- any(
      !is.na(vals) &
        trimws(
          as.character(vals)
        ) != ""
    )
  }

  if (length(overall_author_cols) > 0L) {
    vals <- unlist(
      overall_existing[
        overall_author_cols
      ],
      use.names = FALSE
    )

    has_overall_edit <- any(
      !is.na(vals) &
        trimws(
          as.character(vals)
        ) != ""
    )
  }

  has_domain_edit ||
    has_overall_edit
}


author_workbook_updated <- FALSE
author_workbook_preserved <- FALSE

if (!file.exists(author_workbook)) {

  author_workbook_updated <- isTRUE(
    file.copy(
      generated_workbook,
      author_workbook,
      overwrite = FALSE
    )
  )

} else if (
  !author_workbook_has_manual_edits(
    author_workbook
  )
) {

  author_workbook_updated <- isTRUE(
    file.copy(
      generated_workbook,
      author_workbook,
      overwrite = TRUE
    )
  )

} else {

  author_workbook_preserved <- TRUE
}


# ============================================================
# 14. Final export only after author review
# ============================================================

# This initial R/36 version deliberately does not auto-promote provisional
# judgements into final manuscript judgements. Final export requires author
# review fields to be completed.

final_export_status <- "PENDING AUTHOR REVIEW"

if (
  agent_assessment_available
) {

  final_export_status <- paste0(
    "AGENT ASSESSMENT AVAILABLE; ",
    "AUTHOR REVIEW REQUIRED BEFORE FINAL RoB EXPORT"
  )
}


# ============================================================
# 15. Report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "R/36 — Risk of Bias",
  "",
  paste0(
    "Unique quantitative-synthesis studies: ",
    nrow(
      manifest
    )
  ),
  paste0(
    "RoB 2 studies: ",
    sum(
      manifest$study_design_class == "RCT",
      na.rm = TRUE
    )
  ),
  paste0(
    "ROBINS-I studies: ",
    sum(
      manifest$study_design_class == "NRSI",
      na.rm = TRUE
    )
  ),
  paste0(
    "PDF available: ",
    sum(
      manifest$pdf_status == "AVAILABLE"
    ),
    "/",
    nrow(
      manifest
    )
  ),
  paste0(
    "Signalling-question tasks: ",
    nrow(
      tasks
    )
  ),
  paste0(
    "Agent assessment file present: ",
    agent_assessment_available
  ),
  paste0(
    "Final export status: ",
    final_export_status
  ),
  paste0(
    "Repo-root .env found: ",
    repo_env_status$found
  ),
  paste0(
    ".env variables loaded: ",
    if (
      length(repo_env_status$loaded) == 0L
    ) {
      "none"
    } else {
      paste(
        repo_env_status$loaded,
        collapse = ", "
      )
    }
  ),
  paste0(
    "Pre-existing environment variables preserved: ",
    if (
      length(repo_env_status$preserved) == 0L
    ) {
      "none"
    } else {
      paste(
        repo_env_status$preserved,
        collapse = ", "
      )
    }
  ),
  paste0(
    "Multimodal Agent provider: ",
    rob_agent_provider
  ),
  paste0(
    "Multimodal Agent model: ",
    rob_agent_model
  ),
  paste0(
    "Automated Agent execution completed: ",
    agent_execution_completed
  ),
  paste0(
    "Author workbook updated from generated assessment: ",
    author_workbook_updated
  ),
  paste0(
    "Existing author-edited workbook preserved: ",
    author_workbook_preserved
  ),
  "",
  "OUTPUTS",
  "--------------------------------------------------",
  normalize_slash(
    manifest_file
  ),
  normalize_slash(
    tasks_csv
  ),
  normalize_slash(
    tasks_jsonl
  ),
  normalize_slash(
    agent_output_csv
  ),
  normalize_slash(
    generated_workbook
  ),
  normalize_slash(
    author_workbook
  ),
  "",
  if (
    agent_assessment_available
  ) {
    "PASS: Agent assessments imported and validated; author review remains required."
  } else {
    paste0(
      "PASS: RoB study/result manifest and multimodal Agent tasks generated. ",
      "NEXT: run the Risk-of-Bias Agent on rob_agent_tasks.jsonl and save ",
      "rob_agent_assessments.csv."
    )
  }
)

writeLines(
  report_lines,
  report_file
)


cat("============================================================\n")
cat("R/36 — Risk of Bias\n")
cat("============================================================\n\n")

cat(
  "Unique quantitative-synthesis studies: ",
  nrow(
    manifest
  ),
  "\n",
  sep = ""
)

cat(
  "RoB 2 studies: ",
  sum(
    manifest$study_design_class == "RCT",
    na.rm = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "ROBINS-I studies: ",
  sum(
    manifest$study_design_class == "NRSI",
    na.rm = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "PDF available: ",
  sum(
    manifest$pdf_status == "AVAILABLE"
  ),
  "/",
  nrow(
    manifest
  ),
  "\n",
  sep = ""
)

cat(
  "Signalling-question tasks: ",
  nrow(
    tasks
  ),
  "\n\n",
  sep = ""
)

if (agent_assessment_available) {

  cat(
    "Agent assessment rows: ",
    nrow(agent_output),
    "\n",
    sep = ""
  )

  cat(
    "PASS: R/36 read the canonical PDFs and generated provisional RoB assessments; author review is required.\n"
  )

} else {

  cat(
    "PASS: R/36 generated result-level RoB assessment tasks only.\n"
  )
}
