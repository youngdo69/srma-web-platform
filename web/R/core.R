web_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")
web_id <- function() paste0(sample(c(letters[1:6], 0:9), 32L, replace = TRUE), collapse = "")
web_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", dataframe = "rows")
web_parse <- function(x) jsonlite::fromJSON(x, simplifyVector = FALSE)
web_error <- function(code, message, diagnostic = "") {
  stop(structure(list(message = message, call = NULL, code = code, diagnostic = diagnostic),
                 class = c("srma_web_error", "error", "condition")))
}
web_assert <- function(ok, message = "The requested action is not available.", code = "VALIDATION_FAILED") {
  if (!isTRUE(ok)) web_error(code, message)
}
web_inside <- function(path, root, exists = TRUE) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  path <- normalizePath(path, winslash = "/", mustWork = exists)
  # Resolve existing ancestors as well, preventing escapes via symlinked parents.
  parent <- path
  while (!file.exists(parent) && !dir.exists(parent)) {
    next_parent <- dirname(parent)
    if (identical(next_parent, parent)) break
    parent <- next_parent
  }
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  web_assert((identical(path, root) || startsWith(path, paste0(root, "/"))) &&
             (identical(parent, root) || startsWith(parent, paste0(root, "/"))),
             "A project path is outside its permitted location.")
  path
}
web_sha <- function(path) digest::digest(file = path, algo = "sha256")
web_settings <- function(engine_root = Sys.getenv("SRMA_ENGINE_ROOT", getwd()),
                         data_root = Sys.getenv("SRMA_WEB_DATA", tools::R_user_dir("srma-web", "data")),
                         backend = Sys.getenv("SRMA_WEB_DB", "sqlite")) {
  dir.create(data_root, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  list(engine_root = normalizePath(engine_root, mustWork = TRUE),
       data_root = normalizePath(data_root, mustWork = TRUE), backend = backend,
       sqlite_file = file.path(normalizePath(data_root), "platform.sqlite"),
       rscript = Sys.getenv("SRMA_RSCRIPT", file.path(R.home("bin"), "Rscript")),
       timeout = 120, max_attempts = 2L)
}
web_registry <- function() {
  names <- c("initialize_project", "generate_search", "retrieve_pubmed", "validate_database_inputs",
    "merge_deduplicate", "prepare_screening", "finalize_screening", "register_fulltext",
    "finalize_included_studies", "extract_data", "assess_risk_of_bias", "run_meta_analysis",
    "generate_figures", "generate_tables", "generate_manuscript", "run_final_qc", "build_release_package",
    "validate_search")
  setNames(lapply(names, function(n) list(enabled = identical(n, "validate_search"),
     replay_safe = identical(n, "validate_search"),
     label = if (n == "validate_search") "Check search strategies" else "Not enabled in this release",
     progress = "Checking search strategies…")), names)
}
web_result <- function(operation, project_id, job_id, status, message, engine_version,
                       code = NULL, diagnostic = NULL, artifacts = list(), started = web_now(), provenance = list()) {
  list(operation = operation, project_id = project_id, job_id = job_id, status = status,
       user_message = message, error_code = code, technical = list(diagnostic = diagnostic),
       engine_version = engine_version, artifacts = artifacts, warnings = list(),
       started_at = started, finished_at = web_now(), provenance = provenance)
}
