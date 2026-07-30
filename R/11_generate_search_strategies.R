# Cardiac Surgery SR-MA Toolkit v2.0
# Generate PubMed, Embase, and CENTRAL search strategies

config_file <- "config/search_terms.yml"

if (!file.exists(config_file)) {
  stop(
    "Search-term configuration file does not exist: ",
    config_file,
    "\nRun R/10_collect_search_terms.R first."
  )
}

search_terms <- yaml::read_yaml(config_file)

# ---------------------------------------------------------
# Helper functions
# ---------------------------------------------------------

remove_empty <- function(x) {

  if (is.null(x)) {
    return(character(0))
  }

  x <- as.character(x)
  x <- trimws(x)
  x <- x[!is.na(x)]
  x <- x[x != ""]

  unique(x)
}

escape_quotes <- function(x) {
  gsub('"', '\\"', x, fixed = TRUE)
}

quote_term <- function(x) {
  paste0('"', escape_quotes(x), '"')
}

is_single_word <- function(x) {
  !grepl("\\s", trimws(x))
}

combine_or_block <- function(terms) {

  terms <- remove_empty(terms)

  if (length(terms) == 0) {
    return("")
  }

  paste0(
    "(\n  ",
    paste(terms, collapse = "\n  OR "),
    "\n)"
  )
}

# ---------------------------------------------------------
# PubMed formatting
# ---------------------------------------------------------

format_pubmed_free_text <- function(term) {

  term <- trimws(term)

  if (is_single_word(term)) {
    paste0(term, "[Title/Abstract]")
  } else {
    paste0(quote_term(term), "[Title/Abstract]")
  }
}

format_pubmed_mesh <- function(term) {
  paste0(quote_term(term), "[Mesh]")
}

# ---------------------------------------------------------
# Embase formatting
# Elsevier Embase style
# ---------------------------------------------------------

format_embase_free_text <- function(term) {

  term <- tolower(trimws(term))

  paste0(
    "'",
    gsub("'", "\\\\'", term, fixed = TRUE),
    "':ti,ab,kw"
  )
}

format_embase_emtree <- function(term) {

  term <- tolower(trimws(term))

  paste0(
    "'",
    gsub("'", "\\\\'", term, fixed = TRUE),
    "'/exp"
  )
}

# ---------------------------------------------------------
# CENTRAL formatting
# ---------------------------------------------------------

format_central_free_text <- function(term) {

  term <- trimws(term)

  if (is_single_word(term)) {
    paste0(term, ":ti,ab,kw")
  } else {
    paste0(quote_term(term), ":ti,ab,kw")
  }
}

format_central_mesh <- function(term) {

  term <- trimws(term)

  paste0(
    '[mh "',
    escape_quotes(term),
    '"]'
  )
}

# ---------------------------------------------------------
# Collect terms from configuration
# ---------------------------------------------------------

intervention_terms <- remove_empty(
  c(
    search_terms$intervention$class_terms,
    search_terms$intervention$drug_terms
  )
)

procedure_terms <- remove_empty(
  c(
    search_terms$procedure$general_terms,
    search_terms$procedure$specific_terms
  )
)

perioperative_terms <- remove_empty(
  search_terms$optional$perioperative_terms
)

use_perioperative_terms <- isTRUE(
  search_terms$optional$use_perioperative_terms
)

pubmed_intervention_mesh <- remove_empty(
  search_terms$controlled_vocabulary$pubmed$intervention_mesh
)

pubmed_procedure_mesh <- remove_empty(
  search_terms$controlled_vocabulary$pubmed$procedure_mesh
)

embase_intervention_emtree <- remove_empty(
  search_terms$controlled_vocabulary$embase$intervention_emtree
)

embase_procedure_emtree <- remove_empty(
  search_terms$controlled_vocabulary$embase$procedure_emtree
)

# ---------------------------------------------------------
# Check essential concepts
# ---------------------------------------------------------

if (length(intervention_terms) == 0) {
  stop("No intervention terms were provided.")
}

if (length(procedure_terms) == 0) {
  stop("No procedure terms were provided.")
}

# ---------------------------------------------------------
# Generate PubMed search
# ---------------------------------------------------------

pubmed_intervention_block <- combine_or_block(
  c(
    vapply(
      pubmed_intervention_mesh,
      format_pubmed_mesh,
      character(1)
    ),
    vapply(
      intervention_terms,
      format_pubmed_free_text,
      character(1)
    )
  )
)

pubmed_procedure_block <- combine_or_block(
  c(
    vapply(
      pubmed_procedure_mesh,
      format_pubmed_mesh,
      character(1)
    ),
    vapply(
      procedure_terms,
      format_pubmed_free_text,
      character(1)
    )
  )
)

pubmed_blocks <- c(
  pubmed_intervention_block,
  pubmed_procedure_block
)

if (
  use_perioperative_terms &&
  length(perioperative_terms) > 0
) {

  pubmed_perioperative_block <- combine_or_block(
    vapply(
      perioperative_terms,
      format_pubmed_free_text,
      character(1)
    )
  )

  pubmed_blocks <- c(
    pubmed_blocks,
    pubmed_perioperative_block
  )
}

pubmed_search <- paste(
  pubmed_blocks,
  collapse = "\nAND\n"
)

# ---------------------------------------------------------
# Generate Embase search
# ---------------------------------------------------------

embase_intervention_block <- combine_or_block(
  c(
    vapply(
      embase_intervention_emtree,
      format_embase_emtree,
      character(1)
    ),
    vapply(
      intervention_terms,
      format_embase_free_text,
      character(1)
    )
  )
)

embase_procedure_block <- combine_or_block(
  c(
    vapply(
      embase_procedure_emtree,
      format_embase_emtree,
      character(1)
    ),
    vapply(
      procedure_terms,
      format_embase_free_text,
      character(1)
    )
  )
)

embase_blocks <- c(
  embase_intervention_block,
  embase_procedure_block
)

if (
  use_perioperative_terms &&
  length(perioperative_terms) > 0
) {

  embase_perioperative_block <- combine_or_block(
    vapply(
      perioperative_terms,
      format_embase_free_text,
      character(1)
    )
  )

  embase_blocks <- c(
    embase_blocks,
    embase_perioperative_block
  )
}

embase_search <- paste(
  embase_blocks,
  collapse = "\nAND\n"
)

# ---------------------------------------------------------
# Generate CENTRAL search
# CENTRAL uses PubMed MeSH concepts as starting candidates.
# The final descriptor names must be checked in CENTRAL.
# ---------------------------------------------------------

central_intervention_block <- combine_or_block(
  c(
    vapply(
      pubmed_intervention_mesh,
      format_central_mesh,
      character(1)
    ),
    vapply(
      intervention_terms,
      format_central_free_text,
      character(1)
    )
  )
)

central_procedure_block <- combine_or_block(
  c(
    vapply(
      pubmed_procedure_mesh,
      format_central_mesh,
      character(1)
    ),
    vapply(
      procedure_terms,
      format_central_free_text,
      character(1)
    )
  )
)

central_blocks <- c(
  central_intervention_block,
  central_procedure_block
)

if (
  use_perioperative_terms &&
  length(perioperative_terms) > 0
) {

  central_perioperative_block <- combine_or_block(
    vapply(
      perioperative_terms,
      format_central_free_text,
      character(1)
    )
  )

  central_blocks <- c(
    central_blocks,
    central_perioperative_block
  )
}

central_search <- paste(
  central_blocks,
  collapse = "\nAND\n"
)

# ---------------------------------------------------------
# Save generated strategies
# ---------------------------------------------------------

dir.create(
  "search_strategies",
  recursive = TRUE,
  showWarnings = FALSE
)

writeLines(
  pubmed_search,
  con = "search_strategies/pubmed_search.txt",
  useBytes = TRUE
)

writeLines(
  embase_search,
  con = "search_strategies/embase_search.txt",
  useBytes = TRUE
)

writeLines(
  central_search,
  con = "search_strategies/central_search.txt",
  useBytes = TRUE
)

strategy_summary <- data.frame(
  database = c(
    "PubMed",
    "Embase",
    "CENTRAL"
  ),
  output_file = c(
    "search_strategies/pubmed_search.txt",
    "search_strategies/embase_search.txt",
    "search_strategies/central_search.txt"
  ),
  intervention_term_count = length(intervention_terms),
  procedure_term_count = length(procedure_terms),
  perioperative_concept_used = (
    use_perioperative_terms &&
      length(perioperative_terms) > 0
  ),
  generated_at = as.character(Sys.time()),
  stringsAsFactors = FALSE
)

readr::write_csv(
  strategy_summary,
  "search_strategies/search_strategy_summary.csv"
)

# ---------------------------------------------------------
# Display
# ---------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("Search strategies generated successfully\n")
cat("====================================================\n")

cat("\nPUBMED SEARCH\n")
cat("----------------------------------------------------\n")
cat(pubmed_search, "\n")

cat("\nEMBASE SEARCH\n")
cat("----------------------------------------------------\n")
cat(embase_search, "\n")

cat("\nCENTRAL SEARCH\n")
cat("----------------------------------------------------\n")
cat(central_search, "\n")

cat("\nGenerated files:\n")
cat("- search_strategies/pubmed_search.txt\n")
cat("- search_strategies/embase_search.txt\n")
cat("- search_strategies/central_search.txt\n")
cat("- search_strategies/search_strategy_summary.csv\n")

cat(
  "\nThese strategies are drafts.",
  "\nDatabase-specific syntax and controlled vocabulary",
  "\nmust be validated before the searches are executed.\n"
)