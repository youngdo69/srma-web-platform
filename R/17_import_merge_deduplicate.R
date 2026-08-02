# ============================================================
# SR-MA Agent Toolkit v2.0
# R/17_import_merge_deduplicate.R
#
# Purpose
# ------------------------------------------------------------
# 1. Load the project configuration
# 2. Import PubMed CSV
# 3. Import Embase RIS
# 4. Import CENTRAL RIS
# 5. Convert records to a common structure
# 6. Merge and normalize records
# 7. Deduplicate records
# 8. Preserve source-database provenance
# 9. Save PRISMA-compatible counts and reports
#
# Project-specific paths and filenames are defined in:
# config/project_config.yml
# ============================================================


# ============================================================
# 1. Required packages
# ============================================================

required_packages <- c(
  "dplyr",
  "purrr",
  "readr",
  "stringr",
  "tibble",
  "tidyr"
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
    "The following packages are required but not installed: ",
    paste(
      missing_packages,
      collapse = ", "
    ),
    "\n\nInstall them with:\n",
    "install.packages(c(",
    paste(
      paste0(
        '"',
        missing_packages,
        '"'
      ),
      collapse = ", "
    ),
    "))"
  )
}


# ============================================================
# 2. Load project configuration and paths
# ============================================================

source(
  "R/utils/config_utils.R"
)

source(
  "R/utils/path_utils.R"
)

config <- load_project_config(
  quiet = FALSE
)

paths <- build_project_paths(
  config,
  create_directories = TRUE
)

project_id <- get_project_id(
  config
)

project_title <- get_project_title(
  config
)

enabled_databases <- get_enabled_databases(
  config
)


# ============================================================
# 3. Input and output paths
# ============================================================

pubmed_file <-
  paths$pubmed_csv

embase_file <-
  paths$embase_ris

central_file <-
  paths$central_ris

merged_output_file <-
  paths$merged_records_csv

deduplicated_output_file <-
  paths$deduplicated_records_csv

duplicates_output_file <-
  paths$removed_duplicates_csv

deduplication_map_file <-
  paths$deduplication_map_csv

prisma_counts_file <-
  paths$prisma_counts_csv

report_output_file <-
  paths$import_merge_deduplication_report


# Validate files for enabled databases only

validate_merge_inputs(
  config,
  paths
)


# ============================================================
# 4. General helper functions
# ============================================================

first_nonempty <- function(x) {

  x <- as.character(x)
  x <- trimws(x)

  x <- x[
    !is.na(x) &
      nzchar(x)
  ]

  if (length(x) == 0L) {

    return(
      NA_character_
    )
  }

  x[1]
}


collapse_unique <- function(
    x,
    separator = "; "
) {

  x <- as.character(x)
  x <- trimws(x)

  x <- x[
    !is.na(x) &
      nzchar(x)
  ]

  x <- unique(x)

  if (length(x) == 0L) {

    return(
      NA_character_
    )
  }

  paste(
    x,
    collapse = separator
  )
}


normalize_doi <- function(doi) {

  doi <- as.character(doi)

  doi <- stringr::str_to_lower(
    doi
  )

  doi <- stringr::str_trim(
    doi
  )

  doi <- stringr::str_remove(
    doi,
    "^doi\\s*:\\s*"
  )

  doi <- stringr::str_remove(
    doi,
    "^https?://(dx\\.)?doi\\.org/"
  )

  doi <- stringr::str_remove(
    doi,
    "[[:punct:]]+$"
  )

  doi[
    is.na(doi) |
      doi == ""
  ] <- NA_character_

  doi
}


normalize_pmid <- function(pmid) {

  pmid <- as.character(pmid)

  pmid <- stringr::str_extract(
    pmid,
    "[0-9]+"
  )

  pmid[
    is.na(pmid) |
      pmid == ""
  ] <- NA_character_

  pmid
}


normalize_title <- function(title) {

  title <- as.character(title)

  title <- iconv(
    title,
    from = "",
    to = "ASCII//TRANSLIT"
  )

  title <- stringr::str_to_lower(
    title
  )

  title <- stringr::str_replace_all(
    title,
    "&",
    " and "
  )

  title <- stringr::str_replace_all(
    title,
    "[^a-z0-9]+",
    " "
  )

  title <- stringr::str_squish(
    title
  )

  title[
    is.na(title) |
      title == ""
  ] <- NA_character_

  title
}


extract_year <- function(year_value) {

  year_value <- as.character(
    year_value
  )

  extracted <- stringr::str_extract(
    year_value,
    "(19|20)[0-9]{2}"
  )

  suppressWarnings(
    as.integer(
      extracted
    )
  )
}


standardize_record_columns <- function(data) {

  required_columns <- c(
    "database",
    "source_record_id",
    "pmid",
    "doi",
    "title",
    "authors",
    "year",
    "journal",
    "journal_abbreviation",
    "volume",
    "issue",
    "pages",
    "abstract",
    "keywords",
    "controlled_terms",
    "publication_type",
    "language",
    "url"
  )

  missing_columns <- setdiff(
    required_columns,
    names(data)
  )

  for (column_name in missing_columns) {

    if (column_name == "year") {

      data[[column_name]] <- NA_integer_

    } else {

      data[[column_name]] <- NA_character_
    }
  }

  data |>
    dplyr::select(
      dplyr::all_of(
        required_columns
      )
    )
}


empty_record_dataset <- function() {

  tibble::tibble(
    database = character(0),
    source_record_id = character(0),
    pmid = character(0),
    doi = character(0),
    title = character(0),
    authors = character(0),
    year = integer(0),
    journal = character(0),
    journal_abbreviation = character(0),
    volume = character(0),
    issue = character(0),
    pages = character(0),
    abstract = character(0),
    keywords = character(0),
    controlled_terms = character(0),
    publication_type = character(0),
    language = character(0),
    url = character(0)
  )
}


add_internal_record_ids <- function(
    data,
    prefix
) {

  if (nrow(data) == 0L) {

    return(
      data |>
        dplyr::mutate(
          database_record_number = integer(0),
          internal_record_id = character(0)
        )
    )
  }

  data |>
    dplyr::mutate(
      database_record_number =
        dplyr::row_number(),
      internal_record_id = paste0(
        prefix,
        "_",
        sprintf(
          "%06d",
          database_record_number
        )
      )
    )
}


# ============================================================
# 5. Generic RIS parser
# ============================================================

parse_ris_file <- function(
    file_path,
    database_name
) {

  ris_lines <- readLines(
    file_path,
    warn = FALSE,
    encoding = "UTF-8"
  )

  if (length(ris_lines) == 0L) {

    stop(
      "The RIS file is empty:\n",
      file_path
    )
  }

  end_positions <- which(
    grepl(
      "^ER  -",
      ris_lines
    )
  )

  if (length(end_positions) == 0L) {

    stop(
      "No RIS record terminators (ER  -) were found in:\n",
      file_path
    )
  }

  start_positions <- c(
    1L,
    head(
      end_positions,
      -1L
    ) + 1L
  )

  record_blocks <- Map(
    function(start, end) {

      ris_lines[
        start:end
      ]
    },
    start_positions,
    end_positions
  )


  parse_one_ris_record <- function(
      record_lines,
      record_number
  ) {

    parsed_tags <- list()
    current_tag <- NULL

    for (line in record_lines) {

      tag_match <- stringr::str_match(
        line,
        "^([A-Z0-9]{2})  - ?(.*)$"
      )

      if (!is.na(tag_match[1, 2])) {

        current_tag <- tag_match[1, 2]
        value <- tag_match[1, 3]

        if (is.null(parsed_tags[[current_tag]])) {

          parsed_tags[[current_tag]] <-
            value

        } else {

          parsed_tags[[current_tag]] <- c(
            parsed_tags[[current_tag]],
            value
          )
        }

      } else if (
        !is.null(current_tag) &&
          trimws(line) != ""
      ) {

        last_index <- length(
          parsed_tags[[current_tag]]
        )

        parsed_tags[[current_tag]][last_index] <-
          paste(
            parsed_tags[[current_tag]][last_index],
            trimws(line)
          )
      }
    }


    get_first_tag <- function(tags) {

      for (tag in tags) {

        values <- parsed_tags[[tag]]

        if (
          !is.null(values) &&
            length(values) > 0L
        ) {

          value <- first_nonempty(
            values
          )

          if (!is.na(value)) {

            return(
              value
            )
          }
        }
      }

      NA_character_
    }


    get_all_tags <- function(tags) {

      values <- unlist(
        parsed_tags[
          intersect(
            tags,
            names(parsed_tags)
          )
        ],
        use.names = FALSE
      )

      collapse_unique(
        values
      )
    }


    source_record_id <- get_first_tag(
      c(
        "AN",
        "ID",
        "UT",
        "CN"
      )
    )

    if (is.na(source_record_id)) {

      source_record_id <- paste0(
        database_name,
        "_",
        sprintf(
          "%06d",
          record_number
        )
      )
    }


    note_values <- get_all_tags(
      c(
        "N1",
        "N2"
      )
    )


    pmid_from_notes <- stringr::str_match(
      ifelse(
        is.na(note_values),
        "",
        note_values
      ),
      "(?i)PMID[: ]+([0-9]+)"
    )[, 2]


    pmid_from_id <- stringr::str_match(
      ifelse(
        is.na(source_record_id),
        "",
        source_record_id
      ),
      "(?i)PMID[: ]*([0-9]+)"
    )[, 2]


    pmid <- first_nonempty(
      c(
        get_first_tag("PM"),
        pmid_from_notes,
        pmid_from_id
      )
    )


    ris_url <- get_first_tag(
      "UR"
    )


    doi <- first_nonempty(
      c(
        get_first_tag("DO"),

        stringr::str_extract(
          ifelse(
            is.na(note_values),
            "",
            note_values
          ),
          "10\\.[0-9]{4,9}/[-._;()/:A-Za-z0-9]+"
        ),

        stringr::str_extract(
          ifelse(
            is.na(ris_url),
            "",
            ris_url
          ),
          "10\\.[0-9]{4,9}/[-._;()/:A-Za-z0-9]+"
        )
      )
    )


    start_page <- get_first_tag(
      "SP"
    )

    end_page <- get_first_tag(
      "EP"
    )


    pages <- if (
      !is.na(start_page) &&
        !is.na(end_page)
    ) {

      paste0(
        start_page,
        "-",
        end_page
      )

    } else {

      first_nonempty(
        c(
          start_page,
          get_first_tag("PG")
        )
      )
    }


    tibble::tibble(
      database = database_name,

      source_record_id =
        source_record_id,

      pmid =
        pmid,

      doi =
        doi,

      title = get_first_tag(
        c(
          "TI",
          "T1",
          "CT"
        )
      ),

      authors = get_all_tags(
        c(
          "AU",
          "A1",
          "A2"
        )
      ),

      year = extract_year(
        get_first_tag(
          c(
            "PY",
            "Y1",
            "DA"
          )
        )
      ),

      journal = get_first_tag(
        c(
          "JO",
          "JF",
          "T2",
          "JA"
        )
      ),

      journal_abbreviation = get_first_tag(
        "J2"
      ),

      volume = get_first_tag(
        "VL"
      ),

      issue = get_first_tag(
        "IS"
      ),

      pages =
        pages,

      abstract = get_first_tag(
        c(
          "AB",
          "N2"
        )
      ),

      keywords = get_all_tags(
        "KW"
      ),

      controlled_terms = get_all_tags(
        "MH"
      ),

      publication_type = get_all_tags(
        c(
          "TY",
          "M3"
        )
      ),

      language = get_first_tag(
        "LA"
      ),

      url = get_first_tag(
        c(
          "UR",
          "L1",
          "L2"
        )
      )
    )
  }


  records <- purrr::map2_dfr(
    record_blocks,
    seq_along(
      record_blocks
    ),
    parse_one_ris_record
  )

  records |>
    standardize_record_columns()
}


# ============================================================
# 6. Start report
# ============================================================

cat("\n")
cat("====================================================\n")
cat("Import, Merge, and Deduplicate Search Results\n")
cat("====================================================\n\n")

cat("Project ID:\n")
cat(project_id, "\n\n")

cat("Review title:\n")
cat(project_title, "\n\n")

cat("Enabled databases:\n")
cat(
  paste(
    enabled_databases,
    collapse = ", "
  ),
  "\n\n"
)


# ============================================================
# 7. Import PubMed
# ============================================================

if ("pubmed" %in% enabled_databases) {

  cat("Importing PubMed CSV...\n")

  pubmed_records <- readr::read_csv(
    pubmed_file,
    show_col_types = FALSE,
    col_types = readr::cols(
      .default =
        readr::col_character(),

      year =
        readr::col_integer()
    )
  ) |>
    standardize_record_columns()

} else {

  cat("PubMed is disabled in project_config.yml.\n")

  pubmed_records <-
    empty_record_dataset()
}


# ============================================================
# 8. Import Embase
# ============================================================

if ("embase" %in% enabled_databases) {

  cat("Importing Embase RIS...\n")

  embase_records <- parse_ris_file(
    embase_file,
    "Embase"
  )

} else {

  cat("Embase is disabled in project_config.yml.\n")

  embase_records <-
    empty_record_dataset()
}


# ============================================================
# 9. Import CENTRAL
# ============================================================

if ("central" %in% enabled_databases) {

  cat("Importing CENTRAL RIS...\n")

  central_records <- parse_ris_file(
    central_file,
    "CENTRAL"
  )

} else {

  cat("CENTRAL is disabled in project_config.yml.\n")

  central_records <-
    empty_record_dataset()
}


# ============================================================
# 10. Add internal record IDs
# ============================================================

pubmed_records <- add_internal_record_ids(
  pubmed_records,
  "PUBMED"
)

embase_records <- add_internal_record_ids(
  embase_records,
  "EMBASE"
)

central_records <- add_internal_record_ids(
  central_records,
  "CENTRAL"
)


# ============================================================
# 11. Merge and normalize all records
# ============================================================

all_records <- dplyr::bind_rows(
  pubmed_records,
  embase_records,
  central_records
)

if (nrow(all_records) == 0L) {

  stop(
    "No records were imported from the enabled databases."
  )
}


all_records <- all_records |>
  dplyr::mutate(
    original_row_number =
      dplyr::row_number(),

    doi_normalized = normalize_doi(
      doi
    ),

    pmid_normalized = normalize_pmid(
      pmid
    ),

    title_normalized = normalize_title(
      title
    ),

    year_normalized = suppressWarnings(
      as.integer(
        year
      )
    ),

    title_year_key = dplyr::if_else(
      !is.na(title_normalized),

      paste0(
        title_normalized,
        "__",
        dplyr::if_else(
          is.na(year_normalized),
          "YEAR_UNKNOWN",
          as.character(
            year_normalized
          )
        )
      ),

      NA_character_
    ),

    completeness_score = rowSums(
      !is.na(
        dplyr::pick(
          title,
          authors,
          year,
          journal,
          abstract,
          doi,
          pmid,
          keywords,
          controlled_terms
        )
      )
    ),

    database_priority = dplyr::case_when(
      database == "PubMed" ~ 1L,
      database == "Embase" ~ 2L,
      database == "CENTRAL" ~ 3L,
      TRUE ~ 9L
    )
  )


# ============================================================
# 12. Determine duplicate-match frequencies
# ============================================================

doi_counts <- all_records |>
  dplyr::filter(
    !is.na(
      doi_normalized
    )
  ) |>
  dplyr::count(
    doi_normalized,
    name = "doi_frequency"
  )


pmid_counts <- all_records |>
  dplyr::filter(
    !is.na(
      pmid_normalized
    )
  ) |>
  dplyr::count(
    pmid_normalized,
    name = "pmid_frequency"
  )


title_year_counts <- all_records |>
  dplyr::filter(
    !is.na(
      title_year_key
    )
  ) |>
  dplyr::count(
    title_year_key,
    name = "title_year_frequency"
  )


all_records <- all_records |>
  dplyr::left_join(
    doi_counts,
    by = "doi_normalized"
  ) |>
  dplyr::left_join(
    pmid_counts,
    by = "pmid_normalized"
  ) |>
  dplyr::left_join(
    title_year_counts,
    by = "title_year_key"
  ) |>
  dplyr::mutate(
    dplyr::across(
      c(
        doi_frequency,
        pmid_frequency,
        title_year_frequency
      ),
      ~ tidyr::replace_na(
        .x,
        0L
      )
    )
  )


# ============================================================
# 13. Create deduplication group keys
#
# Priority
# ------------------------------------------------------------
# 1. Exact normalized title and publication year
# 2. Exact normalized DOI
# 3. Exact normalized PMID
# 4. Unique internal record ID
#
# Title-year remains first so that a record without DOI may
# still match a corresponding record from another database.
# ============================================================

all_records <- all_records |>
  dplyr::mutate(
    duplicate_group_key = dplyr::case_when(
      title_year_frequency > 1L ~ paste0(
        "TITLEYEAR:",
        title_year_key
      ),

      doi_frequency > 1L ~ paste0(
        "DOI:",
        doi_normalized
      ),

      pmid_frequency > 1L ~ paste0(
        "PMID:",
        pmid_normalized
      ),

      TRUE ~ paste0(
        "UNIQUE:",
        internal_record_id
      )
    )
  )


# ============================================================
# 14. Save all records before deduplication
# ============================================================

readr::write_csv(
  all_records,
  merged_output_file,
  na = ""
)


# ============================================================
# 15. Create and save deduplication map
# ============================================================

deduplication_map <- all_records |>
  dplyr::group_by(
    duplicate_group_key
  ) |>
  dplyr::arrange(
    database_priority,
    dplyr::desc(
      completeness_score
    ),
    original_row_number,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    retained_internal_record_id =
      dplyr::first(
        internal_record_id
      ),

    record_status = dplyr::if_else(
      dplyr::row_number() == 1L,
      "retained",
      "duplicate_removed"
    ),

    duplicate_group_size =
      dplyr::n()
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    duplicate_group_key,
    duplicate_group_size,
    internal_record_id,
    retained_internal_record_id,
    record_status,
    database,
    source_record_id,
    pmid,
    doi,
    title,
    year
  )


readr::write_csv(
  deduplication_map,
  deduplication_map_file,
  na = ""
)


# ============================================================
# 16. Create deduplicated screening dataset
#
# Metadata from matching records are combined so that:
# - all source databases are preserved
# - missing DOI, PMID, abstract, or other metadata may be
#   supplemented by another source record
# ============================================================

deduplicated_records <- all_records |>
  dplyr::group_by(
    duplicate_group_key
  ) |>
  dplyr::arrange(
    database_priority,
    dplyr::desc(
      completeness_score
    ),
    original_row_number,
    .by_group = TRUE
  ) |>
  dplyr::summarise(
    screening_id = paste0(
      "SCR_",
      sprintf(
        "%06d",
        dplyr::cur_group_id()
      )
    ),

    source_databases = collapse_unique(
      database,
      separator = "; "
    ),

    source_record_ids = collapse_unique(
      paste0(
        database,
        ":",
        source_record_id
      ),
      separator = "; "
    ),

    duplicate_group_size =
      dplyr::n(),

    pmid = first_nonempty(
      pmid_normalized
    ),

    doi = first_nonempty(
      doi_normalized
    ),

    title = first_nonempty(
      title
    ),

    authors = first_nonempty(
      authors
    ),

    year = suppressWarnings(
      as.integer(
        first_nonempty(
          year
        )
      )
    ),

    journal = first_nonempty(
      journal
    ),

    journal_abbreviation = first_nonempty(
      journal_abbreviation
    ),

    volume = first_nonempty(
      volume
    ),

    issue = first_nonempty(
      issue
    ),

    pages = first_nonempty(
      pages
    ),

    abstract = first_nonempty(
      abstract
    ),

    keywords = collapse_unique(
      keywords
    ),

    controlled_terms = collapse_unique(
      controlled_terms
    ),

    publication_type = collapse_unique(
      publication_type
    ),

    language = collapse_unique(
      language
    ),

    url = first_nonempty(
      url
    ),

    title_normalized = first_nonempty(
      title_normalized
    ),

    deduplication_method = dplyr::case_when(
      stringr::str_starts(
        dplyr::first(
          duplicate_group_key
        ),
        "TITLEYEAR:"
      ) ~ "exact_normalized_title_and_year",

      stringr::str_starts(
        dplyr::first(
          duplicate_group_key
        ),
        "DOI:"
      ) ~ "exact_doi",

      stringr::str_starts(
        dplyr::first(
          duplicate_group_key
        ),
        "PMID:"
      ) ~ "exact_pmid",

      TRUE ~ "unique_record"
    ),

    .groups = "drop"
  ) |>
  dplyr::mutate(
    title_abstract_screening_decision =
      NA_character_,

    title_abstract_exclusion_reason =
      NA_character_,

    reviewer_1_decision =
      NA_character_,

    reviewer_2_decision =
      NA_character_,

    conflict_resolution =
      NA_character_,

    notes =
      NA_character_
  )


# ============================================================
# 17. Save removed duplicate records
# ============================================================

removed_duplicate_ids <- deduplication_map |>
  dplyr::filter(
    record_status ==
      "duplicate_removed"
  ) |>
  dplyr::pull(
    internal_record_id
  )


removed_duplicates <- all_records |>
  dplyr::filter(
    internal_record_id %in%
      removed_duplicate_ids
  ) |>
  dplyr::left_join(
    deduplication_map |>
      dplyr::select(
        internal_record_id,
        retained_internal_record_id
      ),
    by = "internal_record_id"
  )


readr::write_csv(
  removed_duplicates,
  duplicates_output_file,
  na = ""
)


# ============================================================
# 18. Save deduplicated screening dataset
# ============================================================

readr::write_csv(
  deduplicated_records,
  deduplicated_output_file,
  na = ""
)


# ============================================================
# 19. Calculate database and PRISMA counts
# ============================================================

pubmed_count <- nrow(
  pubmed_records
)

embase_count <- nrow(
  embase_records
)

central_count <- nrow(
  central_records
)

total_before_deduplication <- nrow(
  all_records
)

total_after_deduplication <- nrow(
  deduplicated_records
)

duplicates_removed <-
  total_before_deduplication -
  total_after_deduplication


database_counts <- tibble::tibble(
  stage = c(
    "Records identified from PubMed",
    "Records identified from Embase",
    "Records identified from CENTRAL",
    "Total records before deduplication",
    "Duplicate records removed",
    "Records after deduplication"
  ),

  count = c(
    pubmed_count,
    embase_count,
    central_count,
    total_before_deduplication,
    duplicates_removed,
    total_after_deduplication
  )
)


readr::write_csv(
  database_counts,
  prisma_counts_file,
  na = ""
)


# ============================================================
# 20. Validation checks
# ============================================================

expected_total_before_deduplication <-
  pubmed_count +
  embase_count +
  central_count


if (
  total_before_deduplication !=
    expected_total_before_deduplication
) {

  stop(
    "Record-count inconsistency occurred during merging.\n",
    "Expected: ",
    expected_total_before_deduplication,
    "\nObserved: ",
    total_before_deduplication
  )
}


if (
  total_after_deduplication +
    duplicates_removed !=
    total_before_deduplication
) {

  stop(
    "Record-count inconsistency occurred during deduplication."
  )
}


if (
  nrow(deduplication_map) !=
    total_before_deduplication
) {

  stop(
    "The deduplication map does not contain one row per ",
    "original database record."
  )
}


if (
  sum(
    deduplication_map$record_status ==
      "duplicate_removed"
  ) !=
    duplicates_removed
) {

  stop(
    "The number of removed records in the deduplication map ",
    "does not match the PRISMA duplicate count."
  )
}


# ============================================================
# 21. Save report
# ============================================================

report_lines <- c(
  "SR-MA Agent Toolkit v2.0",
  "Search Import, Merge, and Deduplication Report",
  "",

  paste0(
    "Project ID: ",
    project_id
  ),

  paste0(
    "Review title: ",
    project_title
  ),

  paste0(
    "Configuration file: ",
    attr(
      config,
      "config_file"
    )
  ),

  paste0(
    "Enabled databases: ",
    paste(
      enabled_databases,
      collapse = ", "
    )
  ),

  paste0(
    "Completed at: ",
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    )
  ),

  "",
  "DATABASE COUNTS",
  "--------------------------------------------------",

  paste0(
    "PubMed: ",
    pubmed_count
  ),

  paste0(
    "Embase: ",
    embase_count
  ),

  paste0(
    "CENTRAL: ",
    central_count
  ),

  paste0(
    "Total before deduplication: ",
    total_before_deduplication
  ),

  paste0(
    "Duplicates removed: ",
    duplicates_removed
  ),

  paste0(
    "Records after deduplication: ",
    total_after_deduplication
  ),

  "",
  "DEDUPLICATION HIERARCHY",
  "--------------------------------------------------",

  "1. Exact normalized title and publication year",
  "2. Exact normalized DOI",
  "3. Exact normalized PMID",
  "4. Unique internal record ID",

  "",
  "VALIDATION",
  "--------------------------------------------------",

  paste0(
    "Merged-record count valid: ",
    total_before_deduplication ==
      expected_total_before_deduplication
  ),

  paste0(
    "Deduplication arithmetic valid: ",
    total_after_deduplication +
      duplicates_removed ==
      total_before_deduplication
  ),

  paste0(
    "Deduplication-map row count valid: ",
    nrow(deduplication_map) ==
      total_before_deduplication
  ),

  "",
  "INPUT FILES",
  "--------------------------------------------------",

  paste0(
    "PubMed: ",
    pubmed_file
  ),

  paste0(
    "Embase: ",
    embase_file
  ),

  paste0(
    "CENTRAL: ",
    central_file
  ),

  "",
  "OUTPUT FILES",
  "--------------------------------------------------",

  merged_output_file,
  deduplicated_output_file,
  duplicates_output_file,
  deduplication_map_file,
  prisma_counts_file
)


writeLines(
  report_lines,
  con = report_output_file,
  useBytes = TRUE
)


# ============================================================
# 22. Console output
# ============================================================

cat("\n")
cat("====================================================\n")
cat("Import, Merge, and Deduplication Complete\n")
cat("====================================================\n\n")

cat("Project ID:\n")
cat(project_id, "\n\n")

print(
  database_counts
)

cat("\nFiles saved:\n")
cat("-", merged_output_file, "\n")
cat("-", deduplicated_output_file, "\n")
cat("-", duplicates_output_file, "\n")
cat("-", deduplication_map_file, "\n")
cat("-", prisma_counts_file, "\n")
cat("-", report_output_file, "\n")

cat(
  "\nNext step:",
  "\n1. Review removed duplicate records.",
  "\n2. Confirm that true duplicates were removed correctly.",
  "\n3. Refactor R/18_prepare_screening_workbook.R.",
  "\n4. Regenerate the title and abstract screening workbook.",
  "\n"
)