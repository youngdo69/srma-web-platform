# Private subprocess boundary. Never sourced by a Shiny session.
args <- commandArgs(trailingOnly = TRUE)
# Library path is inherited explicitly from the parent operational process.
request <- jsonlite::fromJSON(args[[1L]], simplifyVector = FALSE)
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
  config <- e$load_project_config("config/project_config.yml", validate = FALSE, quiet = TRUE)
  # Security boundary only: scientific parsing, validation and path mapping stay canonical.
  for (path in unlist(config$paths, use.names = FALSE)) {
    web_assert(is.character(path) && length(path) == 1L && !grepl("(^[/~]|^[A-Za-z]:|(^|[/\\\\])\\.\\.([/\\\\]|$))", path),
               "Use project-relative paths before importing this project.")
    web_inside(file.path(root, path), root, exists = FALSE)
  }
  config <- e$load_project_config("config/project_config.yml", validate = TRUE, quiet = TRUE)
  paths <- e$build_project_paths(config, create_directories = TRUE, validate = TRUE)
  inputs <- c("config/project_config.yml", paths$search_terms_file,
     unlist(paths[paste0(e$get_enabled_databases(config, validate = FALSE), "_search")], use.names = FALSE))
  for (p in inputs) {
    if (!file.exists(p)) web_error("USER_ACTION_REQUIRED", "Add the configured search inputs before continuing.", p)
    web_inside(p, root)
  }
  inputs <- unique(vapply(inputs, function(p) substring(normalizePath(p, winslash = "/", mustWork = TRUE), nchar(root) + 2L), character(1)))
  manifest <- setNames(lapply(inputs, web_sha), inputs)
  if (identical(request$action, "inspect")) {
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
  # The known validator intentionally raises on structural validation failures.
  if (grepl("canonical-path validation failed", conditionMessage(e), fixed = TRUE)) code <- "VALIDATION_FAILED"
  web_result(request$action, request$project_id, request$job_id,
    if (code == "USER_ACTION_REQUIRED") "USER_ACTION_REQUIRED" else "FAILED",
    switch(code, USER_ACTION_REQUIRED = "Add the configured search inputs before continuing.",
      VALIDATION_FAILED = "Check the project configuration and search inputs, then submit a corrected version.",
      DEPENDENCY_FAILED = "The processing environment needs maintenance.", "Processing could not finish. The diagnostic record is available to the operator."),
    request$engine_version, code, paste(conditionMessage(e), if (!is.null(e$diagnostic)) e$diagnostic else ""), started = started)
})
jsonlite::write_json(result, args[[2L]], auto_unbox = TRUE, null = "null", pretty = TRUE)
