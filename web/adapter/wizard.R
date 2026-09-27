# Private configuration writer. Contract comes from the frozen engine schema;
# scientific validation is delegated to config_utils.R by engine_entry.R.
web_write_configuration <- function(engine_root, root, values, previous = NULL) {
  schema_env <- new.env(parent = baseenv())
  sys.source(file.path(engine_root, "R/utils/initialization_prompt_utils.R"), schema_env)
  schema <- schema_env$build_initialization_json_schema()$properties$project_config
  config <- if (is.null(previous)) list() else previous
  allowed <- c("project.title", "project.project_id", "project.principal_investigator",
    "research_question.framework", "research_question.question", "research_question.population.description",
    "research_question.intervention.description", "research_question.comparator.description",
    "research_question.outcomes.primary", "research_question.outcomes.secondary",
    "eligibility.inclusion", "eligibility.exclusion", "eligibility.study_designs.include",
    "eligibility.language_restriction", "search.search_start_date", "search.search_end_date")
  stopifnot(all(names(values) %in% c(allowed, "databases", "exclusion_reason")))
  set_value <- function(object, keys, value) {
    # Canonical legacy PICO components may be scalar text. The schema-backed
    # wizard represents those same editable descriptions as structured objects.
    if (!is.null(object) && !is.list(object)) object <- list(description = object)
    if (length(keys) == 1L) object[[keys]] <- value
    else object[[keys[1]]] <- set_value(object[[keys[1]]], keys[-1], value)
    object
  }
  for (name in intersect(names(values), allowed)) {
    keys <- strsplit(name, ".", fixed = TRUE)[[1]]
    node <- schema
    for (key in keys) {node <- node$properties[[key]]; stopifnot(!is.null(node))}
    config <- set_value(config, keys, values[[name]])
  }
  for (part in c("population", "intervention", "comparator"))
    config$research_question[[part]]$values <- list(config$research_question[[part]]$description)
  id <- config$project$project_id
  config$project$short_name <- id
  config$project$output_prefix <- id
  config$project$version <- "1"
  config$project$status <- "draft_requires_human_review"
  config$search$databases <- setNames(lapply(c("pubmed", "embase", "central"), function(db)
    list(enabled = db %in% unlist(values$databases), display_name = db)), c("pubmed", "embase", "central"))
  config$screening$decisions <- c("Include", "Exclude", "Unclear")
  if (is.null(config$screening$exclusion_reasons))
    config$screening$exclusion_reasons <- list(list(code = "E01", label = values$exclusion_reason))
  else config$screening$exclusion_reasons[[1]]$label <- values$exclusion_reason
  # Infrastructure defaults, never topic-specific search terms or scientific decisions.
  naming <- names(schema$properties$naming$properties)
  if (is.null(config$naming)) config$naming <- setNames(as.list(sub("_basename$", "", naming)), naming)
  if (is.null(config$paths)) config$paths <- list(
    search_terms_file = "config/search_terms.yml", search_strategies_directory = "search_strategies",
    raw_search_directory = "data/raw", pubmed_directory = "data/raw/pubmed", embase_directory = "data/raw/embase",
    central_directory = "data/raw/central", processed_search_directory = "data/processed/search",
    processed_screening_directory = "data/processed/screening", screening_results_directory = "results/screening",
    logs_directory = "results/logs", figures_directory = "results/figures", tables_directory = "results/tables",
    manuscript_directory = "results/manuscript")
  stopifnot(all(names(config$paths) %in% names(schema$properties$paths$properties)))
  config$search$terms_file <- config$paths$search_terms_file
  dir.create(file.path(root, "config"), recursive = TRUE, showWarnings = FALSE)
  yaml::write_yaml(config, file.path(root, "config/project_config.yml"))
  terms <- file.path(root, config$paths$search_terms_file)
  if (!file.exists(terms)) yaml::write_yaml(list(project = list(id = id, title = config$project$title),
    search = list(concepts = list()), validation = list(controlled_vocabulary_requires_verification = TRUE)), terms)
}
