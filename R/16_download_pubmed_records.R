# ============================================================
# Universal SR-MA Agent Toolkit
# R/16_download_pubmed_records.R
#
# v0.8.2 purpose
# ------------------------------------------------------------
# 1. Read the finalized PubMed search strategy
# 2. Verify that it is the exact strategy approved by R/11
#    intelligent search QC
# 3. Re-check PubMed count BEFORE EFetch
# 4. Download all records only after the search-quality guard
#    passes
# 5. Parse with vectorized/grouped extraction while preserving
#    full canonical metadata semantics and save PubMed outputs
#
# Important
# ------------------------------------------------------------
# - R/16 never silently edits the search strategy.
# - Search refinement belongs upstream in R/11, before R/12.
# - If the final search file changed after R/11 QC, R/16 stops
#   before downloading and asks for R/11 + R/12 to be rerun.
# - No universal fixed record-count cutoff is used.
# - XML parsing avoids per-record tibble construction and vectorizes
#   scalar XPath extraction for larger retrieval sets.
# ============================================================

required_packages <- c(
  "rentrez",
  "xml2",
  "dplyr",
  "purrr",
  "readr",
  "stringr",
  "tibble",
  "yaml"
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
    "R/16 requires packages recorded in renv.lock but currently unavailable: ",
    paste(missing_packages, collapse = ", "),
    "\nRun source(\"START_HERE.R\") so the complete renv environment is restored.",
    call. = FALSE
  )
}

source("R/utils/config_utils.R")
source("R/utils/path_utils.R")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) y else x
}

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

enabled_databases <- get_enabled_databases(
  config,
  validate = FALSE
)

if (!"pubmed" %in% enabled_databases) {
  stop(
    "PubMed is not enabled in project_config.yml.",
    call. = FALSE
  )
}

project_id <- get_project_id(config)
project_title <- get_project_title(config)

# ============================================================
# Canonical paths
# ============================================================

search_file <- paths$pubmed_search
output_directory <- paths$pubmed_directory
xml_output_file <- paths$pubmed_xml
csv_output_file <- paths$pubmed_csv
report_output_file <- paths$pubmed_download_report

quality_report_file <- file.path(
  paths$search_strategies_directory,
  "search_quality_report.yml"
)

assert_file_exists(
  search_file,
  "Canonical PubMed search-strategy file"
)

cat(
  "\nCanonical PubMed input/output paths\n",
  "------------------------------------\n",
  "Search strategy: ", search_file, "\n",
  "Quality report:  ", quality_report_file, "\n",
  "XML output:      ", xml_output_file, "\n",
  "CSV output:      ", csv_output_file, "\n",
  "Download report: ", report_output_file, "\n",
  sep = ""
)

# ============================================================
# Read final PubMed search
# ============================================================

pubmed_search <- paste(
  readLines(
    search_file,
    warn = FALSE,
    encoding = "UTF-8"
  ),
  collapse = "\n"
)

pubmed_search <- trimws(pubmed_search)

if (!nzchar(pubmed_search)) {
  stop(
    "The PubMed search strategy is empty: ",
    search_file,
    call. = FALSE
  )
}

current_search_md5 <- unname(
  tools::md5sum(search_file)
)[[1]]

# ============================================================
# v0.8.2 search-quality guard
# ============================================================

if (!file.exists(quality_report_file)) {
  stop(
    "Pre-download search-quality report is missing:\n",
    quality_report_file,
    "\nRun R/11_generate_search_strategies.R and R/12 validation before R/16.",
    call. = FALSE
  )
}

quality_document <- yaml::read_yaml(
  quality_report_file
)

quality <- quality_document$search_quality

if (is.null(quality) || !is.list(quality)) {
  stop(
    "Invalid search_quality_report.yml: missing search_quality section.",
    call. = FALSE
  )
}

quality_status <- toupper(
  trimws(
    as.character(
      quality$status %||% ""
    )
  )
)

audited_md5 <- trimws(
  as.character(
    quality$final_pubmed_md5 %||% ""
  )
)

if (!nzchar(audited_md5)) {
  stop(
    "The R/11 search-quality report does not contain a final PubMed hash.",
    call. = FALSE
  )
}

if (!identical(current_search_md5, audited_md5)) {
  stop(
    "The canonical PubMed search strategy changed after R/11 intelligent QC.\n",
    "R/16 will not download an unreviewed strategy.\n",
    "Rerun R/11, then R/12, then resume R/16.",
    call. = FALSE
  )
}

if (!identical(quality_status, "PASS")) {
  stop(
    "R/11 search-quality status is: ",
    quality_status,
    "\nPubMed download is blocked before EFetch.\n",
    "Resolve the search-quality issue upstream in R/11 / canonical search terms, ",
    "then rerun R/12.",
    call. = FALSE
  )
}

cat(
  "\nPre-download search-quality guard: PASS\n",
  "R/11 action: ",
  as.character(quality$action %||% ""),
  "\n",
  "Refinement applied: ",
  isTRUE(quality$refinement_applied),
  "\n",
  sep = ""
)

# ============================================================
# Optional NCBI identification
# ============================================================

ncbi_email <- Sys.getenv(
  "NCBI_EMAIL",
  unset = ""
)

ncbi_api_key <- Sys.getenv(
  "NCBI_API_KEY",
  unset = ""
)

if (nzchar(ncbi_api_key)) {
  rentrez::set_entrez_key(
    ncbi_api_key
  )
}

# ============================================================
# Helpers
# ============================================================

collapse_nonempty <- function(
    values,
    separator = "; "
) {
  values <- as.character(values)
  values <- trimws(values)

  values <- values[
    !is.na(values) &
      nzchar(values)
  ]

  if (length(values) == 0L) {
    return(NA_character_)
  }

  paste(
    unique(values),
    collapse = separator
  )
}

xml_text_or_na <- function(node) {
  if (
    length(node) == 0L ||
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
    !nzchar(value)
  ) {
    return(NA_character_)
  }

  value
}

first_year_from_text <- function(text) {
  if (
    is.na(text) ||
    !nzchar(text)
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

  if (length(author_nodes) == 0L) {
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

  if (length(abstract_nodes) == 0L) {
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
        nzchar(label) &&
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

  if (length(mesh_nodes) == 0L) {
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
        nzchar(qualifiers)
      ]

      if (
        !is.na(descriptor) &&
        length(qualifiers) > 0L
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

vector_xml_text_or_na <- function(nodes) {
  values <- xml2::xml_text(nodes, trim = TRUE)
  values <- as.character(values)
  values[is.na(values) | !nzchar(trimws(values))] <- NA_character_
  values
}

find_first_text_vector <- function(article_nodes, xpath) {
  vector_xml_text_or_na(
    xml2::xml_find_first(
      article_nodes,
      xpath
    )
  )
}

parse_pubmed_articles_fast <- function(article_nodes) {
  n_articles <- length(article_nodes)

  if (n_articles == 0L) {
    return(tibble::tibble())
  }

  cat("  Extracting scalar fields...\n")

  pmid <- find_first_text_vector(
    article_nodes,
    "./MedlineCitation/PMID"
  )

  title <- find_first_text_vector(
    article_nodes,
    ".//Article/ArticleTitle"
  )

  journal <- find_first_text_vector(
    article_nodes,
    ".//Article/Journal/Title"
  )

  journal_abbreviation <- find_first_text_vector(
    article_nodes,
    ".//MedlineJournalInfo/MedlineTA"
  )

  volume <- find_first_text_vector(
    article_nodes,
    ".//JournalIssue/Volume"
  )

  issue <- find_first_text_vector(
    article_nodes,
    ".//JournalIssue/Issue"
  )

  pages <- find_first_text_vector(
    article_nodes,
    ".//Article/Pagination/MedlinePgn"
  )

  doi <- find_first_text_vector(
    article_nodes,
    ".//PubmedData/ArticleIdList/ArticleId[@IdType='doi']"
  )

  # Vectorized year extraction with the same precedence as the
  # original per-article helper: Year -> MedlineDate ->
  # ArticleDate -> DateCompleted.
  journal_year <- find_first_text_vector(
    article_nodes,
    ".//JournalIssue/PubDate/Year"
  )
  medline_date <- find_first_text_vector(
    article_nodes,
    ".//JournalIssue/PubDate/MedlineDate"
  )
  article_year <- find_first_text_vector(
    article_nodes,
    ".//ArticleDate/Year"
  )
  completed_year <- find_first_text_vector(
    article_nodes,
    ".//DateCompleted/Year"
  )

  year <- suppressWarnings(as.integer(journal_year))
  need_year <- is.na(year)

  if (any(need_year)) {
    medline_year <- vapply(
      medline_date[need_year],
      function(x) {
        if (is.na(x) || !nzchar(x)) return(NA_integer_)
        first_year_from_text(x)
      },
      integer(1)
    )
    year[need_year] <- medline_year
  }

  need_year <- is.na(year)
  if (any(need_year)) {
    year[need_year] <- suppressWarnings(
      as.integer(article_year[need_year])
    )
  }

  need_year <- is.na(year)
  if (any(need_year)) {
    year[need_year] <- suppressWarnings(
      as.integer(completed_year[need_year])
    )
  }

  # ----------------------------------------------------------
  # Full-author metadata extraction without per-article root XPath
  # ----------------------------------------------------------
  # Preserve the canonical meaning of `authors`: the complete author
  # list for each PubMed record.  The previous slow implementation
  # called xml_find_all() separately from each PubmedArticle root.
  # Here one vectorized XPath call creates one Author nodeset per
  # article (flatten = FALSE); the inexpensive formatting step then
  # works only inside those already-resolved nodesets.

  cat("  Extracting full author lists (grouped vectorized)...\n")

  author_groups <- xml2::xml_find_all(
    article_nodes,
    ".//Article/AuthorList/Author",
    flatten = FALSE
  )

  format_author_group <- function(author_nodes) {
    if (length(author_nodes) == 0L) return(NA_character_)

    collective <- vector_xml_text_or_na(
      xml2::xml_find_first(author_nodes, "./CollectiveName")
    )
    last_name <- vector_xml_text_or_na(
      xml2::xml_find_first(author_nodes, "./LastName")
    )
    initials <- vector_xml_text_or_na(
      xml2::xml_find_first(author_nodes, "./Initials")
    )
    fore_name <- vector_xml_text_or_na(
      xml2::xml_find_first(author_nodes, "./ForeName")
    )

    author_names <- ifelse(
      !is.na(collective) & nzchar(trimws(collective)),
      collective,
      ifelse(
        !is.na(last_name) & !is.na(initials),
        paste(last_name, initials),
        ifelse(
          !is.na(last_name) & !is.na(fore_name),
          paste(last_name, fore_name),
          last_name
        )
      )
    )

    collapse_nonempty(author_names)
  }

  authors <- vapply(
    author_groups,
    format_author_group,
    character(1),
    USE.NAMES = FALSE
  )

  # Abstract parent extraction is vectorized over all PubmedArticle
  # nodes. xml_text() returns the descendant text content; squish it
  # for screening. This avoids one XPath traversal per record/section.
  cat("  Extracting abstracts (vectorized parent nodes)...\n")

  abstract_nodes <- xml2::xml_find_first(
    article_nodes,
    ".//Article/Abstract"
  )

  abstract <- vector_xml_text_or_na(abstract_nodes)
  abstract <- stringr::str_squish(abstract)

  # Optional bibliographic metadata: keep the canonical columns but
  # extract each parent container vectorially rather than iterating
  # through every child node of every article. These fields are not
  # deduplication keys and are supplementary to title/abstract screening.
  cat("  Extracting optional metadata (vectorized)...\n")

  keyword_nodes <- xml2::xml_find_first(
    article_nodes,
    ".//KeywordList"
  )
  keywords <- vector_xml_text_or_na(keyword_nodes)
  keywords <- stringr::str_squish(keywords)

  mesh_nodes <- xml2::xml_find_first(
    article_nodes,
    ".//MeshHeadingList"
  )
  controlled_terms <- vector_xml_text_or_na(mesh_nodes)
  controlled_terms <- stringr::str_squish(controlled_terms)

  publication_type_nodes <- xml2::xml_find_first(
    article_nodes,
    ".//Article/PublicationTypeList"
  )
  publication_type <- vector_xml_text_or_na(publication_type_nodes)
  publication_type <- stringr::str_squish(publication_type)

  language_nodes <- xml2::xml_find_first(
    article_nodes,
    ".//Article/Language[1]"
  )
  language <- vector_xml_text_or_na(language_nodes)

  tibble::tibble(
    database = rep("PubMed", n_articles),
    source_record_id = pmid,
    pmid = pmid,
    doi = doi,
    title = title,
    authors = authors,
    year = year,
    journal = journal,
    journal_abbreviation = journal_abbreviation,
    volume = volume,
    issue = issue,
    pages = pages,
    abstract = abstract,
    keywords = keywords,
    controlled_terms = controlled_terms,
    publication_type = publication_type,
    language = language,
    url = ifelse(
      is.na(pmid),
      NA_character_,
      paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid, "/")
    )
  )
}

# ============================================================
# Count-only ESearch immediately before download
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PubMed Pre-download Safety Check\n")
cat("====================================================\n\n")

cat("Project ID:\n")
cat(project_id, "\n\n")

cat("Review title:\n")
cat(project_title, "\n\n")

cat("Checking current PubMed record count before download...\n")

search_result <- tryCatch(
  rentrez::entrez_search(
    db = "pubmed",
    term = pubmed_search,
    retmax = 0,
    use_history = TRUE
  ),
  error = function(error) {
    stop(
      "PubMed ESearch failed: ",
      conditionMessage(error),
      call. = FALSE
    )
  }
)

record_count <- suppressWarnings(
  as.integer(
    search_result$count
  )
)

cat(
  "Current PubMed records: ",
  record_count,
  "\n",
  sep = ""
)

audited_count <- suppressWarnings(
  as.integer(
    quality$final_pubmed_count %||% NA_integer_
  )
)

if (!is.na(audited_count)) {
  cat(
    "R/11 audited PubMed records: ",
    audited_count,
    "\n",
    sep = ""
  )
}

if (
  is.na(record_count) ||
  record_count <= 0L
) {
  stop(
    "PubMed returned zero/invalid records. ",
    "Review the search strategy upstream before continuing.",
    call. = FALSE
  )
}

if (is.null(search_result$web_history)) {
  stop(
    "PubMed did not return a History Server object.",
    call. = FALSE
  )
}

cat(
  "\nPre-download volume decision: PASS\n",
  "R/11 already assessed the query in the context of this review question; ",
  "R/16 does not apply a fixed universal record-count threshold.\n",
  sep = ""
)

# ============================================================
# Download XML in batches
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

cat("\nDownloading PubMed XML...\n")

for (i in seq_along(batch_starts)) {
  retstart <- batch_starts[[i]]

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
    rentrez::entrez_fetch(
      db = "pubmed",
      web_history = search_result$web_history,
      rettype = "xml",
      retmode = "xml",
      retstart = retstart,
      retmax = retmax
    ),
    error = function(error) {
      stop(
        "PubMed EFetch failed at retstart = ",
        retstart,
        ": ",
        conditionMessage(error),
        call. = FALSE
      )
    }
  )

  if (nzchar(ncbi_api_key)) {
    Sys.sleep(0.11)
  } else {
    Sys.sleep(0.34)
  }
}

# ============================================================
# Combine XML batches
# ============================================================

extract_article_xml <- function(xml_text) {
  xml_document <- xml2::read_xml(
    xml_text
  )

  article_nodes <- xml2::xml_find_all(
    xml_document,
    "/PubmedArticleSet/PubmedArticle"
  )

  if (length(article_nodes) == 0L) {
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
# Parse PubMed XML
# ============================================================

cat("\nParsing PubMed XML...\n")

pubmed_document <- xml2::read_xml(
  xml_output_file
)

article_nodes <- xml2::xml_find_all(
  pubmed_document,
  "/PubmedArticleSet/PubmedArticle"
)

parse_started_at <- Sys.time()

pubmed_records <- parse_pubmed_articles_fast(
  article_nodes
)

cat(
  "PubMed XML parsing completed for ",
  nrow(pubmed_records),
  " records.\n",
  sep = ""
)

# ============================================================
# Normalize + PubMed-internal exact dedup
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

parsed_count <- nrow(
  pubmed_records
)

# ============================================================
# Save PubMed CSV
# ============================================================

readr::write_csv(
  pubmed_records,
  csv_output_file,
  na = ""
)

if (parsed_count != record_count) {
  warning(
    "PubMed search count was ",
    record_count,
    ", but ",
    parsed_count,
    " unique PMID records were parsed.",
    call. = FALSE
  )
}

# ============================================================
# Download report
# ============================================================

report_lines <- c(
  "Universal SR-MA Agent Toolkit",
  "PubMed Detailed Record Download Report",
  "",
  paste0("Project ID: ", project_id),
  paste0("Review title: ", project_title),
  paste0(
    "Configuration file: ",
    attr(config, "config_file")
  ),
  "",
  paste0(
    "Downloaded at: ",
    format(
      Sys.time(),
      "%Y-%m-%d %H:%M:%S"
    )
  ),
  paste0("Search file: ", search_file),
  paste0("Search MD5: ", current_search_md5),
  paste0("Search quality report: ", quality_report_file),
  paste0("Search quality status: ", quality_status),
  paste0(
    "R/11 search QC action: ",
    as.character(quality$action %||% "")
  ),
  paste0(
    "R/11 refinement applied: ",
    isTRUE(quality$refinement_applied)
  ),
  paste0(
    "R/11 audited PubMed count: ",
    ifelse(is.na(audited_count), "NA", audited_count)
  ),
  paste0("Pre-download PubMed count: ", record_count),
  paste0("Parsed unique PMID records: ", parsed_count),
  paste0("Batch size: ", batch_size),
  paste0("NCBI email supplied: ", nzchar(ncbi_email)),
  paste0("NCBI API key supplied: ", nzchar(ncbi_api_key)),
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
# Console report + standalone validation
# ============================================================

cat("\n")
cat("====================================================\n")
cat("PubMed Download Complete\n")
cat("====================================================\n\n")

cat("PubMed search records: ", record_count, "\n", sep = "")
cat("Parsed unique records: ", parsed_count, "\n", sep = "")

cat("\nFiles saved:\n")
cat("- ", xml_output_file, "\n", sep = "")
cat("- ", csv_output_file, "\n", sep = "")
cat("- ", report_output_file, "\n", sep = "")

required_outputs <- c(
  pubmed_xml = xml_output_file,
  pubmed_csv = csv_output_file,
  download_report = report_output_file
)

output_nonempty <- vapply(
  required_outputs,
  function(file_path) {
    file.exists(file_path) &&
      !is.na(file.info(file_path)$size) &&
      file.info(file_path)$size > 0
  },
  logical(1)
)

if (!all(output_nonempty)) {
  failed_outputs <- names(required_outputs)[
    !output_nonempty
  ]

  stop(
    "TASK-005 standalone validation failed.\n",
    "Missing or empty output(s): ",
    paste(failed_outputs, collapse = ", "),
    call. = FALSE
  )
}

if (parsed_count <= 0L) {
  stop(
    "TASK-005 standalone validation failed: no parsed PubMed records were produced.",
    call. = FALSE
  )
}

cat(
  "\nPASS: TASK-005 Canonical PubMed Download completed successfully.\n"
)

cat(
  "\nNext step:\n",
  "TASK-006 — R/17 Merge & Deduplication\n",
  sep = ""
)
