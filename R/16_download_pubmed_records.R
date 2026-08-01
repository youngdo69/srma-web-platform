# ============================================================
# Cardiac Surgery SR-MA Toolkit v2.0
# R/16_download_pubmed_records.R
#
# Purpose
# ------------------------------------------------------------
# 1. Read the finalized PubMed search strategy
# 2. Search PubMed through NCBI E-utilities
# 3. Download all PubMed records in XML batches
# 4. Parse detailed bibliographic information
# 5. Save:
#      - raw PubMed XML
#      - normalized PubMed CSV
#      - download report
#
# NCBI workflow:
# ESearch -> History Server -> EFetch
# ============================================================


# ============================================================
# 1. Required packages
# ============================================================

required_packages <- c(
  "rentrez",
  "xml2",
  "dplyr",
  "purrr",
  "readr",
  "stringr",
  "tibble"
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
# 2. File paths
# ============================================================

search_file_candidates <- c(
  "search_strategies/pubmed_search_final.txt",
  "search_strategies/pubmed_search.txt"
)

search_file <- search_file_candidates[
  file.exists(search_file_candidates)
][1]

if (
  length(search_file) == 0 ||
  is.na(search_file)
) {

  stop(
    "No PubMed search strategy file was found.\n",
    "Expected one of:\n",
    paste(
      paste0("- ", search_file_candidates),
      collapse = "\n"
    )
  )
}

output_directory <-
  "data/raw/search/pubmed"

xml_output_file <-
  file.path(
    output_directory,
    "pubmed_sglt2i_cardiac_surgery.xml"
  )

csv_output_file <-
  file.path(
    output_directory,
    "pubmed_sglt2i_cardiac_surgery.csv"
  )

report_output_file <-
  "results/logs/pubmed_download_report.txt"

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
# 3. Read PubMed search strategy
# ============================================================

pubmed_search_lines <- readLines(
  search_file,
  warn = FALSE,
  encoding = "UTF-8"
)

pubmed_search <- paste(
  pubmed_search_lines,
  collapse = "\n"
)

pubmed_search <- trimws(pubmed_search)

if (pubmed_search == "") {

  stop(
    "The PubMed search strategy is empty: ",
    search_file
  )
}


# ============================================================
# 4. Optional NCBI identification
#
# These can be stored in .Renviron:
#
# NCBI_EMAIL=your_email@example.com
# NCBI_API_KEY=your_api_key
# ============================================================

ncbi_email <- Sys.getenv(
  "NCBI_EMAIL",
  unset = ""
)

ncbi_api_key <- Sys.getenv(
  "NCBI_API_KEY",
  unset = ""
)

if (ncbi_api_key != "") {

  rentrez::set_entrez_key(
    ncbi_api_key
  )
}


# ============================================================
# 5. Helper functions
# ============================================================

collapse_nonempty <- function(
    values,
    separator = "; "
) {

  values <- as.character(values)
  values <- trimws(values)

  values <- values[
    !is.na(values) &
      values != ""
  ]

  if (length(values) == 0) {
    return(NA_character_)
  }

  paste(
    unique(values),
    collapse = separator
  )
}


xml_text_or_na <- function(node) {

  if (
    length(node) == 0 ||
    inherits(node, "xml_missing")
  ) {
    return(NA_character_)
  }

  value <- xml2::xml_text(
    node,
    trim = TRUE
  )

  value <- stringr::str_squish(
    value
  )

  if (
    is.na(value) ||
    value == ""
  ) {
    return(NA_character_)
  }

  value
}


first_year_from_text <- function(text) {

  if (
    is.na(text) ||
    text == ""
  ) {
    return(NA_integer_)
  }

  year <- stringr::str_extract(
    text,
    "(19|20)[0-9]{2}"
  )

  suppressWarnings(
    as.integer(year)
  )
}


extract_pubmed_year <- function(article) {

  year <- xml_text_or_na(
    xml2::xml_find_first(
      article,
      ".//JournalIssue/PubDate/Year"
    )
  )

  if (!is.na(year)) {

    return(
      suppressWarnings(
        as.integer(year)
      )
    )
  }

  medline_date <- xml_text_or_na(
    xml2::xml_find_first(
      article,
      ".//JournalIssue/PubDate/MedlineDate"
    )
  )

  if (!is.na(medline_date)) {

    return(
      first_year_from_text(
        medline_date
      )
    )
  }

  article_date <- xml_text_or_na(
    xml2::xml_find_first(
      article,
      ".//ArticleDate/Year"
    )
  )

  if (!is.na(article_date)) {

    return(
      suppressWarnings(
        as.integer(article_date)
      )
    )
  }

  completed_year <- xml_text_or_na(
    xml2::xml_find_first(
      article,
      ".//DateCompleted/Year"
    )
  )

  suppressWarnings(
    as.integer(completed_year)
  )
}


extract_authors <- function(article) {

  author_nodes <- xml2::xml_find_all(
    article,
    ".//Article/AuthorList/Author"
  )

  if (length(author_nodes) == 0) {
    return(NA_character_)
  }

  author_names <- purrr::map_chr(
    author_nodes,
    function(author) {

      collective_name <- xml_text_or_na(
        xml2::xml_find_first(
          author,
          "./CollectiveName"
        )
      )

      if (!is.na(collective_name)) {
        return(collective_name)
      }

      last_name <- xml_text_or_na(
        xml2::xml_find_first(
          author,
          "./LastName"
        )
      )

      initials <- xml_text_or_na(
        xml2::xml_find_first(
          author,
          "./Initials"
        )
      )

      fore_name <- xml_text_or_na(
        xml2::xml_find_first(
          author,
          "./ForeName"
        )
      )

      if (
        !is.na(last_name) &&
        !is.na(initials)
      ) {

        return(
          paste(
            last_name,
            initials
          )
        )
      }

      if (
        !is.na(last_name) &&
        !is.na(fore_name)
      ) {

        return(
          paste(
            last_name,
            fore_name
          )
        )
      }

      if (!is.na(last_name)) {
        return(last_name)
      }

      NA_character_
    }
  )

  collapse_nonempty(
    author_names
  )
}


extract_abstract <- function(article) {

  abstract_nodes <- xml2::xml_find_all(
    article,
    ".//Article/Abstract/AbstractText"
  )

  if (length(abstract_nodes) == 0) {
    return(NA_character_)
  }

  abstract_sections <- purrr::map_chr(
    abstract_nodes,
    function(node) {

      section_text <- xml_text_or_na(
        node
      )

      label <- xml2::xml_attr(
        node,
        "Label"
      )

      if (
        !is.na(label) &&
        label != "" &&
        !is.na(section_text)
      ) {

        paste0(
          label,
          ": ",
          section_text
        )

      } else {

        section_text
      }
    }
  )

  collapse_nonempty(
    abstract_sections,
    separator = " "
  )
}


extract_identifier <- function(
    article,
    id_type
) {

  xpath <- paste0(
    ".//PubmedData/ArticleIdList/",
    "ArticleId[@IdType='",
    id_type,
    "']"
  )

  xml_text_or_na(
    xml2::xml_find_first(
      article,
      xpath
    )
  )
}


extract_keywords <- function(article) {

  keywords <- xml2::xml_text(
    xml2::xml_find_all(
      article,
      ".//KeywordList/Keyword"
    ),
    trim = TRUE
  )

  collapse_nonempty(
    keywords
  )
}


extract_mesh_terms <- function(article) {

  mesh_nodes <- xml2::xml_find_all(
    article,
    ".//MeshHeadingList/MeshHeading"
  )

  if (length(mesh_nodes) == 0) {
    return(NA_character_)
  }

  mesh_terms <- purrr::map_chr(
    mesh_nodes,
    function(mesh_node) {

      descriptor <- xml_text_or_na(
        xml2::xml_find_first(
          mesh_node,
          "./DescriptorName"
        )
      )

      qualifiers <- xml2::xml_text(
        xml2::xml_find_all(
          mesh_node,
          "./QualifierName"
        ),
        trim = TRUE
      )

      qualifiers <- qualifiers[
        qualifiers != ""
      ]

      if (
        !is.na(descriptor) &&
        length(qualifiers) > 0
      ) {

        paste0(
          descriptor,
          " / ",
          paste(
            qualifiers,
            collapse = ", "
          )
        )

      } else {

        descriptor
      }
    }
  )

  collapse_nonempty(
    mesh_terms
  )
}


extract_publication_types <- function(article) {

  publication_types <- xml2::xml_text(
    xml2::xml_find_all(
      article,
      ".//Article/PublicationTypeList/PublicationType"
    ),
    trim = TRUE
  )

  collapse_nonempty(
    publication_types
  )
}


parse_pubmed_article <- function(article) {

  pmid <- xml_text_or_na(
    xml2::xml_find_first(
      article,
      "./MedlineCitation/PMID"
    )
  )

  title <- xml_text_or_na(
    xml2::xml_find_first(
      article,
      ".//Article/ArticleTitle"
    )
  )

  journal <- xml_text_or_na(
    xml2::xml_find_first(
      article,
      ".//Article/Journal/Title"
    )
  )

  journal_abbreviation <- xml_text_or_na(
    xml2::xml_find_first(
      article,
      ".//MedlineJournalInfo/MedlineTA"
    )
  )

  volume <- xml_text_or_na(
    xml2::xml_find_first(
      article,
      ".//JournalIssue/Volume"
    )
  )

  issue <- xml_text_or_na(
    xml2::xml_find_first(
      article,
      ".//JournalIssue/Issue"
    )
  )

  pages <- xml_text_or_na(
    xml2::xml_find_first(
      article,
      ".//Article/Pagination/MedlinePgn"
    )
  )

  language <- collapse_nonempty(
    xml2::xml_text(
      xml2::xml_find_all(
        article,
        ".//Article/Language"
      ),
      trim = TRUE
    )
  )

  tibble::tibble(
    database = "PubMed",
    source_record_id = pmid,
    pmid = pmid,
    doi = extract_identifier(
      article,
      "doi"
    ),
    title = title,
    authors = extract_authors(
      article
    ),
    year = extract_pubmed_year(
      article
    ),
    journal = journal,
    journal_abbreviation = journal_abbreviation,
    volume = volume,
    issue = issue,
    pages = pages,
    abstract = extract_abstract(
      article
    ),
    keywords = extract_keywords(
      article
    ),
    controlled_terms = extract_mesh_terms(
      article
    ),
    publication_type = extract_publication_types(
      article
    ),
    language = language,
    url = if (!is.na(pmid)) {
      paste0(
        "https://pubmed.ncbi.nlm.nih.gov/",
        pmid,
        "/"
      )
    } else {
      NA_character_
    }
  )
}


# ============================================================
# 6. Run PubMed search
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PubMed Detailed Record Download\n")
cat("====================================================\n\n")

cat("Search strategy file:\n")
cat(search_file, "\n\n")

cat("Searching PubMed...\n")

search_result <- tryCatch(
  {

    rentrez::entrez_search(
      db = "pubmed",
      term = pubmed_search,
      retmax = 0,
      use_history = TRUE
    )
  },
  error = function(error) {

    stop(
      "PubMed ESearch failed: ",
      conditionMessage(error)
    )
  }
)

record_count <- as.integer(
  search_result$count
)

cat("Records found:", record_count, "\n")

if (
  is.na(record_count) ||
  record_count == 0
) {

  stop(
    "PubMed returned zero records. ",
    "Review the search strategy before continuing."
  )
}

if (is.null(search_result$web_history)) {

  stop(
    "PubMed did not return a History Server object."
  )
}


# ============================================================
# 7. Download PubMed XML in batches
# ============================================================

batch_size <- 200L

batch_starts <- seq.int(
  from = 0L,
  to = record_count - 1L,
  by = batch_size
)

xml_batches <- vector(
  mode = "list",
  length = length(batch_starts)
)

for (i in seq_along(batch_starts)) {

  retstart <- batch_starts[i]

  retmax <- min(
    batch_size,
    record_count - retstart
  )

  cat(
    "Downloading records ",
    retstart + 1L,
    " to ",
    retstart + retmax,
    " of ",
    record_count,
    "...\n",
    sep = ""
  )

  xml_batches[[i]] <- tryCatch(
    {

      rentrez::entrez_fetch(
        db = "pubmed",
        web_history = search_result$web_history,
        rettype = "xml",
        retmode = "xml",
        retstart = retstart,
        retmax = retmax
      )
    },
    error = function(error) {

      stop(
        "PubMed EFetch failed at retstart = ",
        retstart,
        ": ",
        conditionMessage(error)
      )
    }
  )

  if (ncbi_api_key == "") {
    Sys.sleep(0.34)
  } else {
    Sys.sleep(0.11)
  }
}


# ============================================================
# 8. Combine XML batches
# ============================================================

extract_article_xml <- function(xml_text) {

  xml_document <- xml2::read_xml(
    xml_text
  )

  article_nodes <- xml2::xml_find_all(
    xml_document,
    "/PubmedArticleSet/PubmedArticle"
  )

  if (length(article_nodes) == 0) {
    return(character(0))
  }

  vapply(
    article_nodes,
    as.character,
    character(1)
  )
}

article_xml_strings <- unlist(
  lapply(
    xml_batches,
    extract_article_xml
  ),
  use.names = FALSE
)

combined_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>\n',
  "<PubmedArticleSet>\n",
  paste(
    article_xml_strings,
    collapse = "\n"
  ),
  "\n</PubmedArticleSet>\n"
)

writeLines(
  combined_xml,
  con = xml_output_file,
  useBytes = TRUE
)


# ============================================================
# 9. Parse combined PubMed XML
# ============================================================

cat("\nParsing PubMed XML...\n")

pubmed_document <- xml2::read_xml(
  xml_output_file
)

article_nodes <- xml2::xml_find_all(
  pubmed_document,
  "/PubmedArticleSet/PubmedArticle"
)

pubmed_records <- purrr::map_dfr(
  article_nodes,
  parse_pubmed_article
)


# ============================================================
# 10. Normalize PubMed fields
# ============================================================

pubmed_records <- pubmed_records |>
  dplyr::mutate(
    pmid = stringr::str_trim(
      as.character(pmid)
    ),
    doi = stringr::str_to_lower(
      stringr::str_trim(
        as.character(doi)
      )
    ),
    doi = stringr::str_remove(
      doi,
      "^https?://(dx\\.)?doi\\.org/"
    ),
    title = stringr::str_squish(
      title
    ),
    abstract = stringr::str_squish(
      abstract
    ),
    year = suppressWarnings(
      as.integer(year)
    ),
    downloaded_at = format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    )
  ) |>
  dplyr::distinct(
    pmid,
    .keep_all = TRUE
  )


# ============================================================
# 11. Save PubMed CSV
# ============================================================

readr::write_csv(
  pubmed_records,
  csv_output_file,
  na = ""
)


# ============================================================
# 12. Validate output
# ============================================================

parsed_count <- nrow(
  pubmed_records
)

if (parsed_count != record_count) {

  warning(
    "PubMed search count was ",
    record_count,
    ", but ",
    parsed_count,
    " unique PMID records were parsed."
  )
}


# ============================================================
# 13. Save download report
# ============================================================

report_lines <- c(
  "Cardiac Surgery SR-MA Toolkit v2.0",
  "PubMed Detailed Record Download Report",
  "",
  paste0(
    "Downloaded at: ",
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    )
  ),
  paste0(
    "Search file: ",
    search_file
  ),
  paste0(
    "PubMed search count: ",
    record_count
  ),
  paste0(
    "Parsed unique records: ",
    parsed_count
  ),
  paste0(
    "Batch size: ",
    batch_size
  ),
  paste0(
    "NCBI email supplied: ",
    ncbi_email != ""
  ),
  paste0(
    "NCBI API key supplied: ",
    ncbi_api_key != ""
  ),
  "",
  "OUTPUT FILES",
  "--------------------------------------------------",
  xml_output_file,
  csv_output_file
)

writeLines(
  report_lines,
  con = report_output_file,
  useBytes = TRUE
)


# ============================================================
# 14. Console report
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PubMed Download Complete\n")
cat("====================================================\n\n")

cat("PubMed search records:", record_count, "\n")
cat("Parsed unique records:", parsed_count, "\n\n")

cat("Files saved:\n")
cat("-", xml_output_file, "\n")
cat("-", csv_output_file, "\n")
cat("-", report_output_file, "\n")

cat(
  "\nNext step:",
  "\n1. Confirm that the Embase RIS file is present.",
  "\n2. Confirm that the CENTRAL RIS file is present.",
  "\n3. Run R/17_import_merge_deduplicate.R.",
  "\n"
)