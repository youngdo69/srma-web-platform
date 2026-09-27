web_engine_call <- function(settings, request, diagnostic_dir = tempfile("engine-call-")) {
  dir.create(diagnostic_dir, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  request$engine_root <- settings$engine_root
  if (!is.null(settings$engine_library)) request$engine_library <- settings$engine_library
  input <- file.path(diagnostic_dir, "request.json")
  output <- file.path(diagnostic_dir, "result.json")
  web_no_secrets(request, settings$engine_root)
  jsonlite::write_json(request, input, auto_unbox = TRUE, null = "null")
  previous_libs <- Sys.getenv("R_LIBS", unset = NA_character_)
  Sys.setenv(R_LIBS = if (!is.null(settings$engine_library)) settings$engine_library else paste(.libPaths(), collapse = .Platform$path.sep))
  on.exit(if (is.na(previous_libs)) Sys.unsetenv("R_LIBS") else Sys.setenv(R_LIBS = previous_libs), add = TRUE)
  run <- tryCatch(suppressWarnings(system2(settings$rscript,
     shQuote(c("--vanilla", file.path(settings$engine_root, "web/adapter", if (identical(request$action, "runtime_probe")) "runtime_probe.R" else "engine_entry.R"), input, output)),
     stdout = nullfile(), stderr = nullfile(),
     timeout = settings$timeout)), error = identity)
  if (inherits(run, "error") || !file.exists(output)) {
    return(web_result(request$action, request$project_id, request$job_id, "FAILED",
      "Processing could not finish. The operator can safely retry this check.", request$engine_version,
      "SYSTEM_FAILED", if (inherits(run, "error")) conditionMessage(run) else paste("Subprocess exit", run)))
  }
  jsonlite::read_json(output, simplifyVector = FALSE)
}
web_engine_inspect <- function(settings, root, configuration_only = FALSE) {
  result <- web_engine_call(settings, list(action = if (configuration_only) "inspect_configuration" else "inspect", project_dir = root))
  if (result$status != "COMPLETE") web_error(result$error_code, result$user_message, result$technical$diagnostic)
  result
}
# Semantic operation contract; callers cannot select scripts, paths or shell commands.
srma_run <- function(settings, project_id, operation, payload, job_id, project_dir, engine_version, diagnostic_dir) {
  reg <- web_registry()[[operation]]
  if (is.null(reg) || !isTRUE(reg$enabled) || length(payload))
    return(web_result(operation, project_id, job_id, "FAILED", "This operation is not enabled in this release.", engine_version, "VALIDATION_FAILED"))
  if (operation %in% c("validate_database_inputs",web_pipeline_operations())) {
    if(operation %in% web_pipeline_operations() && !web_provider_ready(settings))
      return(web_result(operation,project_id,job_id,"USER_ACTION_REQUIRED",
        "Ask the operator to configure the provider credential. Saved work is retained.",engine_version,"USER_ACTION_REQUIRED"))
    scientific <- tryCatch(web_science_settings(settings), error = identity)
    if (inherits(scientific, "error")) return(web_result(operation, project_id, job_id, "FAILED",
      "The scientific processing environment needs maintenance.", engine_version, "DEPENDENCY_FAILED"))
    settings <- scientific
  }
  web_engine_call(settings, list(action = operation, project_id = project_id, job_id = job_id,
    project_dir = project_dir, engine_version = engine_version), diagnostic_dir)
}

web_science_settings <- function(settings, verify = TRUE) {
  script <- Sys.getenv("SRMA_SCIENTIFIC_RSCRIPT", "")
  library <- Sys.getenv("SRMA_SCIENTIFIC_LIBRARY", "")
  web_assert(nzchar(script) && file.exists(script) && nzchar(library) && dir.exists(library),
    "The scientific processing environment needs maintenance.", "DEPENDENCY_FAILED")
  scientific <- settings; scientific$rscript <- script; scientific$engine_library <- normalizePath(library)
  if (verify) {
    probe <- web_engine_call(scientific, list(action = "runtime_probe"))
    web_assert(probe$status == "COMPLETE", "The scientific processing environment needs maintenance.", "DEPENDENCY_FAILED")
  }
  scientific
}
