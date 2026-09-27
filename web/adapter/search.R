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
    outputs <- outputs[!grepl("raw_response",basename(outputs),fixed=TRUE)]
    quality <- yaml::read_yaml(file.path(paths$search_strategies_directory, "search_quality_report.yml"))$search_quality
    web_assert(quality$status %in% c("PASS","REVIEW_REQUIRED"), "Canonical search quality returned an unsupported failure state.", "ENGINE_FAILED")
    ready <- identical(quality$status, "PASS")
  } else {
    # R/16 itself verifies the unchanged R/11 quality-approved query before EFetch.
    sys.source(file.path(engine_root, scripts[[operation]]), envir = e)
    outputs <- unlist(paths[c("pubmed_xml", "pubmed_csv", "pubmed_download_report")], use.names = FALSE)
    quality <- yaml::read_yaml(file.path(paths$search_strategies_directory, "search_quality_report.yml"))$search_quality
    ready <- TRUE
  }
  outputs <- outputs[file.exists(outputs) & !dir.exists(outputs)]
  list(outputs = lapply(outputs, function(p) list(path = web_inside(p, root), logical_name = basename(p),
    type = if (operation == "generate_search") "search_strategy" else "raw_database_records")),
    ready = ready, quality = quality$status, query_sha256 = web_sha(paths$pubmed_search),
    query_md5 = unname(tools::md5sum(paths$pubmed_search)), script_hash = web_sha(file.path(engine_root, scripts[[operation]])))
}

# Presentation of canonical states only; this does not compute scientific quality.
web_search_result <- function(request, produced, manifest) {
  web_assert(produced$quality %in% c("PASS","REVIEW_REQUIRED"),
    "Canonical search quality returned an unsupported failure state.","ENGINE_FAILED")
  passed<-identical(produced$quality,"PASS")
  web_result(request$action,request$project_id,request$job_id,
    if(passed)"COMPLETE" else "USER_ACTION_REQUIRED",
    if(passed)paste0("Canonical search quality: ",produced$quality,". Processing finished; human review remains required.")
      else paste0("Canonical search quality: ",produced$quality,". Review the canonical report; retrieval remains blocked."),
    request$engine_version,code=if(!passed)"USER_ACTION_REQUIRED" else NULL,
    artifacts=produced$outputs,provenance=list(input_hashes=manifest,engine_script_sha256=produced$script_hash,
      search_quality=produced$quality,query_sha256=produced$query_sha256,query_md5=produced$query_md5,R=R.version.string))
}
