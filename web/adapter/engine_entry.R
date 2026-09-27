# Private subprocess boundary. Never sourced by a Shiny session.
args <- commandArgs(trailingOnly = TRUE)
# Library path is inherited explicitly from the parent operational process.
request <- jsonlite::fromJSON(args[[1L]], simplifyVector = FALSE)
if (!is.null(request$engine_library)) .libPaths(c(request$engine_library, .Library), include.site = FALSE)
source(file.path(request$engine_root, "web/R/core.R"))
started <- web_now()
result <- tryCatch({
  missing <- c("yaml", "readr", "dplyr", "digest")[!vapply(c("yaml", "readr", "dplyr", "digest"), requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) web_error("DEPENDENCY_FAILED", "The processing environment needs maintenance.", paste(missing, collapse = ", "))
  root <- normalizePath(request$project_dir, mustWork = TRUE)
  setwd(root)
  e <- new.env(parent = globalenv())
  sys.source(file.path(request$engine_root, "R/utils/config_utils.R"), envir = e)
  sys.source(file.path(request$engine_root, "R/utils/path_utils.R"), envir = e)
  if (identical(request$action, "write_configuration")) {
    source(file.path(request$engine_root, "web/adapter/wizard.R"))
    previous <- if (file.exists("config/project_config.yml")) yaml::read_yaml("config/project_config.yml") else NULL
    web_write_configuration(request$engine_root, root, request$values, previous)
  }
  config <- e$load_project_config("config/project_config.yml", validate = FALSE, quiet = TRUE)
  # Security boundary only: scientific parsing, validation and path mapping stay canonical.
  for (path in unlist(config$paths, use.names = FALSE)) {
    web_assert(is.character(path) && length(path) == 1L && !grepl("(^[/~]|^[A-Za-z]:|(^|[/\\\\])\\.\\.([/\\\\]|$))", path),
               "Use project-relative paths before importing this project.")
    web_inside(file.path(root, path), root, exists = FALSE)
  }
  config <- e$load_project_config("config/project_config.yml", validate = TRUE, quiet = TRUE)
  paths <- e$build_project_paths(config, create_directories = TRUE, validate = TRUE)
  source(file.path(request$engine_root,"web/R/evidence.R"))
  configuration_only <- request$action %in% names(web_b2_operations()) || request$action %in% c("initialize_project", "prepare_manual_concepts", "activate_initialization", "inspect_configuration", "write_configuration", "database_context", "validate_database_inputs", "generate_search", "retrieve_pubmed")
  inputs <- c("config/project_config.yml", paths$search_terms_file)
  if (!configuration_only) inputs <- c(inputs,
    unlist(paths[paste0(e$get_enabled_databases(config, validate = FALSE), "_search")], use.names = FALSE))
  if (configuration_only) {
    # Preserve verified uploaded/generated inputs when editing a saved version.
    extra <- list.files(root, recursive = TRUE, full.names = TRUE)
    extra <- extra[!dir.exists(extra)]
    extra <- extra[!grepl("^(R/|prompts/)|(^|/)(initialization_agent|human_decision_policy)\\.yml$",sub(paste0(root,"/"),"",extra,fixed=TRUE))]
    inputs <- unique(c(inputs, extra))
  }
  for (p in inputs) {
    if (!file.exists(p)) web_error("USER_ACTION_REQUIRED", "Add the configured search inputs before continuing.", p)
    web_inside(p, root)
  }
  inputs <- unique(vapply(inputs, function(p) substring(normalizePath(p, winslash = "/", mustWork = TRUE), nchar(root) + 2L), character(1)))
  manifest <- setNames(lapply(inputs, web_sha), inputs)
  if(request$action %in% names(web_b2_operations())) {
    source(file.path(request$engine_root,"web/adapter/evidence.R"))
    web_b2_canonical(request,config,paths,root,manifest)
  } else if (request$action %in% c("initialize_project", "prepare_manual_concepts", "activate_initialization")) {
    source(file.path(request$engine_root,"web/adapter/initialization.R"))
    produced <- web_initialization_action(request,config,root)
    web_result(request$action,request$project_id,request$job_id,"COMPLETE",
      "Canonical draft processing finished. Explicit human review is required before search generation.",request$engine_version,
      artifacts=produced$outputs,provenance=list(input_hashes=manifest,R=R.version.string))
  } else if (request$action %in% c("generate_search", "retrieve_pubmed")) {
    source(file.path(request$engine_root, "web/adapter/search.R"))
    produced <- web_canonical_search(request$action, e, paths, root, request$engine_root)
    web_search_result(request,produced,manifest)
  } else if (request$action %in% c("database_context", "validate_database_inputs")) {
    source(file.path(request$engine_root, "web/adapter/database.R"))
    databases <- web_database_context(e, config, paths, root, request$engine_root)
    if (request$action == "database_context") list(status = "COMPLETE", databases = databases)
    else {
      checked <- e$task018_run_database_input_gate(config = config, paths = paths,
        verbose = FALSE, write_evidence = TRUE, write_provenance = TRUE, task019c_prompt = FALSE)
      outputs <- c(checked$evidence_csv, checked$evidence_report)
      web_result(request$action, request$project_id, request$job_id,
        if (isTRUE(checked$ready)) "COMPLETE" else "USER_ACTION_REQUIRED",
        if (isTRUE(checked$ready)) "Database inputs passed preflight. Human review remains required; no merge or screening has started."
        else "Database inputs still need attention. Review the database instructions and upload the required exports.",
        request$engine_version, code = if (!isTRUE(checked$ready)) "USER_ACTION_REQUIRED" else NULL,
        artifacts = lapply(outputs, function(p) list(path = web_inside(p, root), logical_name = basename(p), type = "database_preflight")),
        provenance = list(input_hashes = manifest, engine_script_sha256 = web_sha(file.path(request$engine_root, "R/TASK018_database_input_gate_hardening.R")), R = R.version.string))
    }
  } else if (request$action %in% c("inspect", "inspect_configuration", "write_configuration")) {
    list(status = "COMPLETE", project_id = e$get_project_id(config), title = e$get_project_title(config),
         inputs = as.list(inputs), manifest = manifest)
  } else {
    web_assert(identical(request$action, "validate_search"), "This operation is not enabled.")
    # Only this adapter knows engine filenames. Copy source verbatim into the isolated job tree.
    dir.create("R/utils", recursive = TRUE, showWarnings = FALSE)
    for (name in c("config_utils.R", "path_utils.R"))
      file.copy(file.path(request$engine_root, "R/utils", name), file.path("R/utils", name), overwrite = TRUE)
    sys.source(file.path(request$engine_root, "R/12_validate_search_strategies.R"), envir = e)
    outputs <- file.path(paths$logs_directory, c("search_strategy_validation.csv", "search_strategy_validation_report.txt"))
    for (p in outputs) web_inside(p, root)
    web_result(request$action, request$project_id, request$job_id, "COMPLETE",
      "Search checks completed. Test each search in its database next.", request$engine_version,
      artifacts = lapply(outputs, function(p) list(path = normalizePath(p), logical_name = basename(p), type = "validation")),
      started = started, provenance = list(input_hashes = manifest, engine_script_sha256 = web_sha(file.path(request$engine_root, "R/12_validate_search_strategies.R")), R = R.version.string))
  }
}, error = function(e) {
  code <- if (inherits(e, "srma_web_error")) e$code else if (request$action == "inspect") "VALIDATION_FAILED" else "ENGINE_FAILED"
  if(grepl("requires packages recorded in renv.lock but currently unavailable",conditionMessage(e),fixed=TRUE))code<-"DEPENDENCY_FAILED"
  # The known validator intentionally raises on structural validation failures.
  if (grepl("canonical-path validation failed", conditionMessage(e), fixed = TRUE)) code <- "VALIDATION_FAILED"
  web_result(request$action, request$project_id, request$job_id,
    if (code == "USER_ACTION_REQUIRED") "USER_ACTION_REQUIRED" else "FAILED",
    switch(code, USER_ACTION_REQUIRED = if (inherits(e,"srma_web_error")) conditionMessage(e) else "Add the configured search inputs before continuing.",
      VALIDATION_FAILED = "Check the project configuration and search inputs, then submit a corrected version.",
      DEPENDENCY_FAILED = "The processing environment needs maintenance.", "Processing could not finish. The diagnostic record is available to the operator."),
    request$engine_version, code, paste(conditionMessage(e), if (!is.null(e$diagnostic)) e$diagnostic else ""), started = started)
})
result <- web_redact(result, request$engine_root)
jsonlite::write_json(result, args[[2L]], auto_unbox = TRUE, null = "null", pretty = TRUE)
