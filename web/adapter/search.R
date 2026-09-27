# Prepared semantic mappings. Registry remains fail-closed until runtime, upstream
# human registration and actual search-quality integration are validated.
web_canonical_search <- function(operation, e, paths, root, engine_root) {
  scripts <- c(generate_search = "R/11_generate_search_strategies.R", retrieve_pubmed = "R/16_download_pubmed_records.R")
  web_assert(operation %in% names(scripts), "This operation is not available.")
  dir.create("R/utils", recursive = TRUE, showWarnings = FALSE)
  for (name in c("config_utils.R", "path_utils.R", "llm_provider_utils.R"))
    file.copy(file.path(engine_root, "R/utils", name), file.path("R/utils", name), overwrite = TRUE)
  if (operation == "generate_search") {
    file.copy(file.path(engine_root, "config/initialization_agent.yml"), "config/initialization_agent.yml", overwrite = TRUE)
    # Inherit operator-supplied provider environment only. Never copy .env into jobs.
    sys.source(file.path(engine_root, scripts[[operation]]), envir = e)
    outputs <- list.files(paths$search_strategies_directory, full.names = TRUE)
    quality <- yaml::read_yaml(file.path(paths$search_strategies_directory, "search_quality_report.yml"))$search_quality
    ready <- identical(quality$status, "PASS")
  } else {
    # R/16 itself verifies the unchanged R/11 quality-approved query before EFetch.
    sys.source(file.path(engine_root, scripts[[operation]]), envir = e)
    outputs <- unlist(paths[c("pubmed_xml", "pubmed_csv", "pubmed_download_report")], use.names = FALSE)
    ready <- TRUE
  }
  outputs <- outputs[file.exists(outputs) & !dir.exists(outputs)]
  list(outputs = lapply(outputs, function(p) list(path = web_inside(p, root), logical_name = basename(p),
    type = if (operation == "generate_search") "search_strategy" else "raw_database_records")),
    ready = ready, script_hash = web_sha(file.path(engine_root, scripts[[operation]])))
}
