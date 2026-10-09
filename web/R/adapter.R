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
srma_run <- function(settings, project_id, operation, payload, job_id, project_dir, engine_version, diagnostic_dir, version_id = NULL) {
  reg <- web_registry()[[operation]]
  if (is.null(reg) || !isTRUE(reg$enabled) || length(payload))
    return(web_result(operation, project_id, job_id, "FAILED", "This operation is not enabled in this release.", engine_version, "VALIDATION_FAILED"))
  if (operation %in% c("validate_database_inputs","prepare_prospero",web_pipeline_operations())) {
    if(operation %in% web_pipeline_operations() && !web_provider_ready(settings))
      return(web_result(operation,project_id,job_id,"USER_ACTION_REQUIRED",
        "Ask the operator to configure the provider credential. Saved work is retained.",engine_version,"USER_ACTION_REQUIRED"))
    scientific <- tryCatch(web_science_settings(settings), error = identity)
    if (inherits(scientific, "error")) return(web_result(operation, project_id, job_id, "FAILED",
      "The scientific processing environment needs maintenance.", engine_version, "DEPENDENCY_FAILED"))
    settings <- scientific
  }
  if(operation %in% names(web_b2_operations())) {
    if(operation %in% names(web_b2_ai_operations()) && !nzchar(Sys.getenv(web_b2_credential_name(settings$engine_root,operation),"")))return(web_result(operation,project_id,job_id,"USER_ACTION_REQUIRED","AI assistance requires operator credentials; use manual Human review.",engine_version,"USER_ACTION_REQUIRED"))
    settings<-web_science_settings(settings)
  }
  if(operation %in% c(names(web_c1_operations()),names(web_c2_operations())))settings<-web_science_settings(settings)
  web_engine_call(settings, list(action = operation, project_id = project_id, job_id = job_id,
    project_dir = project_dir, engine_version = engine_version, version_id=version_id), diagnostic_dir)
}

web_science_settings <- function(settings, verify = TRUE) {
  script <- Sys.getenv("SRMA_SCIENTIFIC_RSCRIPT", "")
  library <- Sys.getenv("SRMA_SCIENTIFIC_LIBRARY", "")
  web_assert(nzchar(script) && file.exists(script) && nzchar(library) && dir.exists(library),
    "The scientific processing environment needs maintenance.", "DEPENDENCY_FAILED")

  scientific <- settings
  scientific$rscript <- script
  scientific$engine_library <- normalizePath(library)

  # Local-development UAT seam only.
  #
  # The release/production path remains strict: runtime_probe must match the
  # canonical lock exactly.  For local development, an operator may explicitly
  # permit the already-installed scientific runtime to execute canonical code
  # even when runtime_probe reports DEPENDENCY_FAILED (for example, an R-version
  # mismatch against renv.lock).  The probe must still launch successfully;
  # SYSTEM_FAILED and other failures remain blocked.
  #
  # This seam does NOT constitute canonical-runtime acceptance.  Child-process
  # provenance continues to record the actual R version used, and release gates
  # remain independent.
  dev_runtime_seam <- identical(tolower(as.character(settings$mode)), "development") &&
    identical(Sys.getenv("SRMA_DEV_ALLOW_UNVERIFIED_SCIENTIFIC_RUNTIME", ""), "1")

  if (isTRUE(verify)) {
    probe <- web_engine_call(scientific, list(action = "runtime_probe"))
    exact_runtime <- identical(probe$status, "COMPLETE")
    allowed_dev_mismatch <- isTRUE(dev_runtime_seam) &&
      identical(probe$status, "FAILED") &&
      identical(probe$error_code, "DEPENDENCY_FAILED")

    if (!isTRUE(exact_runtime) && !isTRUE(allowed_dev_mismatch)) {
      diagnostic <- paste(
        "runtime_probe status:", as.character(probe$status %||% "UNKNOWN"),
        "error_code:", as.character(probe$error_code %||% ""),
        "R:", as.character(probe$R %||% "")
      )
      web_error(
        "DEPENDENCY_FAILED",
        "The scientific processing environment needs maintenance.",
        diagnostic
      )
    }

    scientific$runtime_acceptance <- if (isTRUE(exact_runtime)) {
      "EXACT_CANONICAL_RUNTIME"
    } else {
      "DEVELOPMENT_UNVERIFIED_RUNTIME"
    }
  }

  scientific
}
