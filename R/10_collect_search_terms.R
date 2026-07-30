# Cardiac Surgery SR-MA Toolkit v2.0
# Interactive collection of systematic-review search terms

# ---------------------------------------------------------
# Helper functions
# ---------------------------------------------------------

split_terms <- function(input_text) {

  if (
    is.null(input_text) ||
    is.na(input_text) ||
    trimws(input_text) == ""
  ) {
    return(character(0))
  }

  terms <- unlist(
    strsplit(
      input_text,
      split = ",",
      fixed = TRUE
    )
  )

  terms <- trimws(terms)
  terms <- terms[terms != ""]
  unique(terms)
}

ask_terms <- function(question, example = NULL) {

  cat("\n")
  cat(question, "\n")

  if (!is.null(example)) {
    cat("Example:", example, "\n")
  }

  answer <- readline(
    prompt = "Enter comma-separated terms: "
  )

  split_terms(answer)
}

ask_yes_no <- function(question, default = FALSE) {

  default_text <- if (default) "Y/n" else "y/N"

  answer <- readline(
    prompt = paste0(question, " [", default_text, "]: ")
  )

  answer <- tolower(trimws(answer))

  if (answer == "") {
    return(default)
  }

  answer %in% c("y", "yes")
}

# ---------------------------------------------------------
# Start
# ---------------------------------------------------------

cat("\n")
cat("====================================================\n")
cat("Cardiac Surgery SR-MA Search-Term Collector\n")
cat("====================================================\n")

cat(
  "\nEnter synonyms separated by commas.",
  "\nDo not enter AND or OR.",
  "\nThe search-strategy generator will add Boolean operators.",
  "\n"
)

review_title <- readline(
  prompt = "\nReview title: "
)

review_short_name <- readline(
  prompt = "Short project name: "
)

# ---------------------------------------------------------
# Concept 1: Intervention or exposure
# ---------------------------------------------------------

intervention_class_terms <- ask_terms(
  question = "1. Enter intervention or drug-class terms.",
  example = paste(
    "SGLT2 inhibitor,",
    "sodium glucose cotransporter 2 inhibitor,",
    "SGLT-2 inhibitor"
  )
)

intervention_drug_terms <- ask_terms(
  question = "2. Enter individual drug names.",
  example = paste(
    "dapagliflozin, empagliflozin,",
    "canagliflozin, ertugliflozin"
  )
)

# ---------------------------------------------------------
# Concept 2: Population or procedure
# ---------------------------------------------------------

procedure_general_terms <- ask_terms(
  question = "3. Enter general cardiac-surgery terms.",
  example = paste(
    "cardiac surgery, heart surgery,",
    "cardiothoracic surgery"
  )
)

procedure_specific_terms <- ask_terms(
  question = "4. Enter specific procedure terms.",
  example = paste(
    "coronary artery bypass grafting, CABG,",
    "valve surgery, aortic surgery,",
    "cardiopulmonary bypass"
  )
)

# ---------------------------------------------------------
# Optional concept 3
# ---------------------------------------------------------

use_perioperative_terms <- ask_yes_no(
  "Do you want an optional perioperative concept?",
  default = FALSE
)

if (use_perioperative_terms) {

  perioperative_terms <- ask_terms(
    question = "5. Enter perioperative terms.",
    example = "perioperative, preoperative, postoperative"
  )

} else {

  perioperative_terms <- character(0)
}

# ---------------------------------------------------------
# Optional outcomes
# ---------------------------------------------------------

collect_outcomes <- ask_yes_no(
  paste(
    "Do you want to record outcome terms",
    "without adding them to the primary search?"
  ),
  default = TRUE
)

if (collect_outcomes) {

  outcome_terms <- ask_terms(
    question = "6. Enter outcome terms for documentation.",
    example = paste(
      "acute kidney injury, mortality,",
      "heart failure, ketoacidosis"
    )
  )

} else {

  outcome_terms <- character(0)
}

# ---------------------------------------------------------
# Known relevant records
# ---------------------------------------------------------

known_relevant_ids <- ask_terms(
  question = paste(
    "7. Enter PMID or DOI values of known relevant studies",
    "(optional)."
  ),
  example = "12345678, 10.1000/example.doi"
)

# ---------------------------------------------------------
# Controlled vocabulary
# ---------------------------------------------------------

pubmed_mesh_intervention <- ask_terms(
  question = "8. Enter PubMed MeSH terms for the intervention.",
  example = "Sodium-Glucose Transporter 2 Inhibitors"
)

pubmed_mesh_procedure <- ask_terms(
  question = "9. Enter PubMed MeSH terms for the procedure.",
  example = "Cardiac Surgical Procedures"
)

embase_emtree_intervention <- ask_terms(
  question = "10. Enter Embase Emtree terms for the intervention.",
  example = "sodium glucose cotransporter 2 inhibitor"
)

embase_emtree_procedure <- ask_terms(
  question = "11. Enter Embase Emtree terms for the procedure.",
  example = "heart surgery, coronary artery bypass grafting"
)

# ---------------------------------------------------------
# Build configuration object
# ---------------------------------------------------------

search_terms <- list(

  project = list(
    title = review_title,
    short_name = review_short_name,
    created_at = as.character(Sys.time())
  ),

  intervention = list(
    class_terms = intervention_class_terms,
    drug_terms = intervention_drug_terms
  ),

  procedure = list(
    general_terms = procedure_general_terms,
    specific_terms = procedure_specific_terms
  ),

  optional = list(
    use_perioperative_terms = use_perioperative_terms,
    perioperative_terms = perioperative_terms,
    outcome_terms = outcome_terms
  ),

  controlled_vocabulary = list(

    pubmed = list(
      intervention_mesh = pubmed_mesh_intervention,
      procedure_mesh = pubmed_mesh_procedure
    ),

    embase = list(
      intervention_emtree = embase_emtree_intervention,
      procedure_emtree = embase_emtree_procedure
    )
  ),

  validation = list(
    known_relevant_ids = known_relevant_ids
  )
)

# ---------------------------------------------------------
# Save
# ---------------------------------------------------------

dir.create(
  "config",
  recursive = TRUE,
  showWarnings = FALSE
)

yaml::write_yaml(
  search_terms,
  file = "config/search_terms.yml"
)

cat("\n")
cat("====================================================\n")
cat("Search terms saved successfully\n")
cat("====================================================\n")

cat("\nSaved file:\n")
cat("config/search_terms.yml\n")

cat("\nNumber of intervention terms:\n")
cat(
  length(intervention_class_terms) +
    length(intervention_drug_terms),
  "\n"
)

cat("\nNumber of procedure terms:\n")
cat(
  length(procedure_general_terms) +
    length(procedure_specific_terms),
  "\n"
)

cat("\nNext command:\n")
cat('source("R/11_generate_search_strategies.R")\n')