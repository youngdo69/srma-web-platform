# Synthetic structural inputs only; no clinical observations or proposed real search terms.
web_fixture <- function(root, id = web_id()) {
  dir.create(file.path(root, "config"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root, "search_strategies"), recursive = TRUE, showWarnings = FALSE)
  naming <- c("pubmed_basename", "embase_basename", "central_basename", "merged_records_basename", "deduplicated_records_basename", "removed_duplicates_basename", "deduplication_map_basename", "prisma_counts_basename", "screening_workbook_basename", "completed_screening_workbook_basename")
  config <- list(project = list(project_id = id, title = "Synthetic structural validation fixture", short_name = "Fixture", output_prefix = "fixture", version = "1"),
    research_question = list(framework = "PICO", population = "Synthetic concept", intervention = "Synthetic concept", comparator = "Synthetic concept", outcomes = "Synthetic concept"),
    eligibility = list(description = "No real studies in this fixture"), search = list(databases = list(pubmed = TRUE)),
    screening = list(decisions = c("Include", "Exclude", "Unclear"), exclusion_reasons = list(list(code = "E01", label = "Ineligible"))),
    naming = setNames(as.list(sub("_basename$", "", naming)), naming),
    paths = list(search_terms_file = "config/search_terms.yml", search_strategies_directory = "search_strategies", raw_search_directory = "data/raw",
      pubmed_directory = "data/raw/pubmed", embase_directory = "data/raw/embase", central_directory = "data/raw/central",
      processed_search_directory = "data/processed", processed_screening_directory = "data/screening",
      screening_results_directory = "results/screening", logs_directory = "results/logs"))
  yaml::write_yaml(config, file.path(root, "config/project_config.yml"))
  writeLines("synthetic_fixture: true", file.path(root, "config/search_terms.yml"))
  writeLines('(\n"concept"[Mesh]\nOR "concept"[Title/Abstract]\n)', file.path(root, "search_strategies/pubmed_search.txt"))
  normalizePath(root)
}
