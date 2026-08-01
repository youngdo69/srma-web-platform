# ============================================================
# Cardiac Surgery SR-MA Toolkit v2.0
# R/17_import_merge_deduplicate.R
#
# Purpose
# ------------------------------------------------------------
# 1. Import PubMed CSV
# 2. Import Embase RIS
# 3. Import CENTRAL RIS
# 4. Convert all databases to a common structure
# 5. Merge all records
# 6. Normalize DOI, PMID, title, and year
# 7. Deduplicate records
# 8. Preserve provenance from all source databases
# 9. Save PRISMA-compatible counts
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

if (length(missing_packages) > 0) {

  stop(
    "The following packages are required but not installed: ",
    paste(missing_packages, collapse = ", "),
    "\nInstall them with:\n",
    "install.packages(c(",
    paste(
      paste0('"', missing_packages, '"'),
      collapse = ", "
    ),
    "))"
  )
}


# ============================================================
# 2. Input and output paths
# ============================================================

pubmed_file <-
  "data/raw/search/pubmed/pubmed_sglt2i_cardiac_surgery.csv"

embase_file <-
  "data/raw/search/embase/embase_sglt2i_cardiac_surgery.ris"

central_file <-
  "data/raw/search/central/central_sglt2i_cardiac_surgery.ris"

output_directory <-
  "data/processed/search"

merged_output_file <-
  file.path(
    output_directory,
    "all_database_records_before_deduplication.csv"
  )

deduplicated_output_file <-
  file.path(
    output_directory,
    "deduplicated_records_for_screening.csv"
  )

duplicates_output_file <-
  file.path(
    output_directory,
    "removed_duplicate_records.csv"
  )

deduplication_map_file <-
  file.path(
    output_directory,
    "deduplication_map.csv"
  )

prisma_counts_file <-
  file.path(
    output_directory,
    "search_and_deduplication_counts.csv"
  )

report_output_file <-
  "results/logs/import_merge_deduplication_report.txt"

required_files <- c(
  pubmed_file,
  embase_file,
  central_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {

  stop(
    "The following required search-result files are missing:\n",
    paste(
      paste0("- ", missing_files),
      collapse = "\n"
    )
  )
}

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  "results/logs",
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 3. General helper functions
# ============================================================

first_nonempty <- function(x) {

  x <- as.character(x)
  x <- trimws(x)

  x <- x[
    !is.na(x) &
      x != ""
  ]

  if (length(x) == 0) {
    return(NA_character_)
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
      x != ""
  ]

  x <- unique(x)

  if (length(x) == 0) {
    return(NA_character_)
  }

  paste(
    x,
    collapse = separator
  )
}


normalize_doi <- function(doi) {

  doi <- as.character(doi)
  doi <- stringr::str_to_lower(doi)
  doi <- stringr::str_trim(doi)

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
    as.integer(extracted)
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

    data[[column_name]] <- NA_character_
  }

  data |>
    dplyr::select(
      dplyr::all_of(
        required_columns
      )
    )
}


# ============================================================
# 4. Generic RIS parser
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

  if (length(ris_lines) == 0) {

    stop(
      "The RIS file is empty: ",
      file_path
    )
  }

  # Every record normally ends with ER  -
  end_positions <- which(
    grepl(
      "^ER  -",
      ris_lines
    )
  )

  if (length(end_positions) == 0) {

    stop(
      "No RIS record terminators (ER  -) were found in: ",
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

          parsed_tags[[current_tag]] <- value

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
          length(values) > 0
        ) {

          value <- first_nonempty(
            values
          )

          if (!is.na(value)) {
            return(value)
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
            is.na(get_first_tag("UR")),
            "",
            get_first_tag("UR")
          ),
          "10\\.[0-9]{4,9}/[-._;()/:A-Za-z0-9]+"
        )
      )
    )

    start_page <- get_first_tag(
      c(
        "SP"
      )
    )

    end_page <- get_first_tag(
      c(
        "EP"
      )
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
      source_record_id = source_record_id,
      pmid = pmid,
      doi = doi,
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
        c(
          "J2"
        )
      ),
      volume = get_first_tag(
        c(
          "VL"
        )
      ),
      issue = get_first_tag(
        c(
          "IS"
        )
      ),
      pages = pages,
      abstract = get_first_tag(
        c(
          "AB",
          "N2"
        )
      ),
      keywords = get_all_tags(
        c(
          "KW"
        )
      ),
      controlled_terms = get_all_tags(
        c(
          "MH"
        )
      ),
      publication_type = get_all_tags(
        c(
          "TY",
          "M3"
        )
      ),
      language = get_first_tag(
        c(
          "LA"
        )
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
    seq_along(record_blocks),
    parse_one_ris_record
  )

  records |>
    standardize_record_columns()
}


# ============================================================
# 5. Import PubMed
# ============================================================

cat("\n")
cat("====================================================\n")
cat("Import, Merge, and Deduplicate Search Results\n")
cat("====================================================\n\n")

cat("Importing PubMed CSV...\n")

pubmed_records <- readr::read_csv(
  pubmed_file,
  show_col_types = FALSE,
  col_types = readr::cols(
    .default = readr::col_character(),
    year = readr::col_integer()
  )
) |>
  standardize_record_columns()


# ============================================================
# 6. Import Embase RIS
# ============================================================

cat("Importing Embase RIS...\n")

embase_records <- parse_ris_file(
  embase_file,
  "Embase"
)


# ============================================================
# 7. Import CENTRAL RIS
# ============================================================

cat("Importing CENTRAL RIS...\n")

central_records <- parse_ris_file(
  central_file,
  "CENTRAL"
)


# ============================================================
# 8. Add internal record IDs
# ============================================================

pubmed_records <- pubmed_records |>
  dplyr::mutate(
    database_record_number =
      dplyr::row_number(),
    internal_record_id = paste0(
      "PUBMED_",
      sprintf(
        "%06d",
        database_record_number
      )
    )
  )

embase_records <- embase_records |>
  dplyr::mutate(
    database_record_number =
      dplyr::row_number(),
    internal_record_id = paste0(
      "EMBASE_",
      sprintf(
        "%06d",
        database_record_number
      )
    )
  )

central_records <- central_records |>
  dplyr::mutate(
    database_record_number =
      dplyr::row_number(),
    internal_record_id = paste0(
      "CENTRAL_",
      sprintf(
        "%06d",
        database_record_number
      )
    )
  )


# ============================================================
# 9. Merge all database records
# ============================================================

all_records <- dplyr::bind_rows(
  pubmed_records,
  embase_records,
  central_records
) |>
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
      as.integer(year)
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
# 10. Determine duplicate match frequencies
# ============================================================

doi_counts <- all_records |>
  dplyr::filter(
    !is.na(doi_normalized)
  ) |>
  dplyr::count(
    doi_normalized,
    name = "doi_frequency"
  )

pmid_counts <- all_records |>
  dplyr::filter(
    !is.na(pmid_normalized)
  ) |>
  dplyr::count(
    pmid_normalized,
    name = "pmid_frequency"
  )

title_year_counts <- all_records |>
  dplyr::filter(
    !is.na(title_year_key)
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
# 11. Create deduplication group key
#
# Priority:
# 1. Exact normalized title + year when shared
# 2. DOI when duplicated
# 3. PMID when duplicated
# 4. Unique internal ID
#
# Exact title-year is placed first so that a record lacking
# DOI can still match another database record that has a DOI.
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
# 12. Save all records before deduplication
# ============================================================

readr::write_csv(
  all_records,
  merged_output_file,
  na = ""
)


# ============================================================
# 13. Create deduplication map
# ============================================================

deduplication_map <- all_records |>
  dplyr::group_by(
    duplicate_group_key
  ) |>
  dplyr::arrange(
    database_priority,
    dplyr::desc(completeness_score),
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
# 14. Create deduplicated screening dataset
#
# Metadata from duplicate records are combined so that:
# - source databases are preserved
# - missing abstract/DOI/PMID may be filled from another source
# ============================================================

deduplicated_records <- all_records |>
  dplyr::group_by(
    duplicate_group_key
  ) |>
  dplyr::arrange(
    database_priority,
    dplyr::desc(completeness_score),
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
    duplicate_group_size = dplyr::n(),
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
# 15. Save removed duplicate records
# ============================================================

removed_duplicate_ids <- deduplication_map |>
  dplyr::filter(
    record_status == "duplicate_removed"
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
# 16. Save deduplicated screening dataset
# ============================================================

readr::write_csv(
  deduplicated_records,
  deduplicated_output_file,
  na = ""
)


# ============================================================
# 17. Calculate database and PRISMA counts
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
# 18. Validation checks
# ============================================================

if (
  total_before_deduplication !=
    pubmed_count +
    embase_count +
    central_count
) {

  stop(
    "Record-count inconsistency occurred during merging."
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


# ============================================================
# 19. Save report
# ============================================================

report_lines <- c(
  "Cardiac Surgery SR-MA Toolkit v2.0",
  "Search Import, Merge, and Deduplication Report",
  "",
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
  "3. Exact PMID",
  "4. Unique source record",
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
# 20. Console output
# ============================================================

cat("\n")
cat("====================================================\n")
cat("Import, Merge, and Deduplication Complete\n")
cat("====================================================\n\n")

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
  "\n1. Review removed_duplicate_records.csv.",
  "\n2. Confirm that true duplicates were removed correctly.",
  "\n3. Begin title and abstract screening using",
  "\n   deduplicated_records_for_screening.csv.",
  "\n"
)