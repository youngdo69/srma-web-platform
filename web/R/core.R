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
  if (!grepl("^(/|[A-Za-z]:|\\\\)", path)) path <- file.path(getwd(), path)
  path <- normalizePath(path, winslash = "/", mustWork = exists)
  # Resolve existing ancestors as well, preventing escapes via symlinked parents.
  parent <- path
  while (!file.exists(parent) && !dir.exists(parent)) {
    next_parent <- dirname(parent)
    if (identical(next_parent, parent)) break
    parent <- next_parent
  }
  parent <- normalizePath(parent, winslash = "/", mustWork = TRUE)
  if (!isTRUE((identical(path, root) || startsWith(path, paste0(root, "/"))) &&
             (identical(parent, root) || startsWith(parent, paste0(root, "/")))))
    web_error("VALIDATION_FAILED", "A project path is outside its permitted location.",
      paste("Path:", path, "Root:", root, "Resolved parent:", parent))
  path
}
web_sha <- function(path) digest::digest(file = path, algo = "sha256")
web_settings <- function(engine_root = Sys.getenv("SRMA_ENGINE_ROOT", getwd()),
                         data_root = Sys.getenv("SRMA_WEB_DATA", tools::R_user_dir("srma-web", "data")),
                         backend = Sys.getenv("SRMA_WEB_DB", "sqlite")) {
  dir.create(data_root, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  mode <- tolower(Sys.getenv("SRMA_WEB_MODE", "development"))
  list(engine_root = normalizePath(engine_root, mustWork = TRUE),
       data_root = normalizePath(data_root, mustWork = TRUE), backend = backend, mode = mode,
       sqlite_file = file.path(normalizePath(data_root), "platform.sqlite"),
       rscript = Sys.getenv("SRMA_RSCRIPT", file.path(R.home("bin"), "Rscript")),
       host = Sys.getenv("SRMA_WEB_HOST", "127.0.0.1"), port = as.integer(Sys.getenv("SRMA_WEB_PORT", "3838")),
       public_url = Sys.getenv("SRMA_PUBLIC_URL", ""),
       trusted_auth_proxy = identical(Sys.getenv("SRMA_TRUSTED_AUTH_PROXY"), "1"),
       auth_header = Sys.getenv("SRMA_AUTH_HEADER", "HTTP_X_SRMA_USER"),
       timeout = 120, max_attempts = 2L)
}
web_registry <- function() {
  names <- c("initialize_project", "generate_search", "retrieve_pubmed", "validate_database_inputs",
    "merge_deduplicate", "prepare_screening", "finalize_screening", "register_fulltext",
    "finalize_included_studies", "extract_data", "assess_risk_of_bias", "run_meta_analysis",
    "generate_figures", "generate_tables", "generate_manuscript", "run_final_qc", "build_release_package",
    "validate_search")
  registry <- setNames(lapply(names, function(n) list(enabled = n %in% c("validate_search", "validate_database_inputs"),
     replay_safe = n %in% c("validate_search", "validate_database_inputs"),
     label = if (n == "validate_search") "Check search strategies" else "Not enabled in this release",
     progress = "Checking search strategies…")), names)
  labels <- c(initialize_project = "Prepare review configuration", generate_search = "Generate database searches",
    retrieve_pubmed = "Retrieve PubMed records", validate_database_inputs = "Check database inputs")
  for (name in names(labels)) {
    registry[[name]]$label <- labels[[name]]
    registry[[name]]$blocked_reason <- "This step needs validated scientific runtime and canonical workflow integration before it can be enabled. Your saved inputs are retained."
  }
  configured <- nzchar(Sys.getenv("SRMA_SCIENTIFIC_RSCRIPT")) && file.exists(Sys.getenv("SRMA_SCIENTIFIC_RSCRIPT")) &&
    nzchar(Sys.getenv("SRMA_SCIENTIFIC_LIBRARY")) && dir.exists(Sys.getenv("SRMA_SCIENTIFIC_LIBRARY"))
  for(op in c("initialize_project","generate_search","retrieve_pubmed")) {
    registry[[op]]$enabled <- configured
    registry[[op]]$replay_safe <- FALSE
    registry[[op]]$progress <- paste0(registry[[op]]$label,"…")
    registry[[op]]$blocked_reason <- "Configure the validated scientific runtime and provider credential; saved work is retained."
  }
  registry$validate_database_inputs$progress <- "Checking database inputs…"
  if(exists("web_b2_operations",mode="function"))for(op in names(web_b2_operations())) {
    label<-gsub("_"," ",op,fixed=TRUE)
    registry[[op]]<-list(enabled=TRUE,replay_safe=FALSE,label=label,progress=paste0(label,"…"))
  }
  if(exists("web_c1_operations",mode="function"))for(op in names(web_c1_operations()))registry[[op]]<-list(enabled=TRUE,replay_safe=FALSE,label="Run approved analysis and generate figures/tables",progress="Running the approved canonical analysis…")
  if(exists("web_c2_operations",mode="function"))for(op in names(web_c2_operations()))registry[[op]]<-list(enabled=TRUE,replay_safe=FALSE,label=gsub("_"," ",op),progress="Preparing author-review manuscript output…")
  registry$run_final_qc<-list(enabled=TRUE,replay_safe=TRUE,label="Inspect final scientific consistency",progress="Checking exact manuscript, sources and artifacts…")
  registry
}
web_result <- function(operation, project_id, job_id, status, message, engine_version,
                       code = NULL, diagnostic = NULL, artifacts = list(), started = web_now(), provenance = list()) {
  list(operation = operation, project_id = project_id, job_id = job_id, status = status,
       user_message = message, error_code = code, technical = list(diagnostic = diagnostic),
       engine_version = engine_version, artifacts = artifacts, warnings = list(),
       started_at = started, finished_at = web_now(), provenance = provenance)
}

# Operator credentials never enter requests, snapshots, published results or logs.
web_operator_secrets <- function(engine_root) {
  cfg <- yaml::read_yaml(file.path(engine_root,"config/initialization_agent.yml"))
  screening <- yaml::read_yaml(file.path(engine_root,"config/agent_screening.yml"))
  keys <- unique(c(cfg$initialization_agent$provider$api_key_env,screening$agent_screening$provider$api_key_env,"OPENAI_API_KEY","NCBI_API_KEY"))
  values <- Sys.getenv(keys,unset="")
  unname(values[nzchar(values)])
}
web_redact <- function(value,engine_root) {
  secrets <- web_operator_secrets(engine_root)
  redact <- function(x) {
    if(is.list(x))return(lapply(x,redact))
    if(is.character(x))for(secret in secrets)x<-gsub(secret,"[REDACTED]",x,fixed=TRUE)
    x
  }
  redact(value)
}
web_no_secrets <- function(value,engine_root) {
  text<-as.character(web_json(value))
  web_assert(!any(vapply(web_operator_secrets(engine_root),function(s)grepl(s,text,fixed=TRUE),logical(1))),
    "Remove private credentials from project inputs; credentials belong in the operator environment.")
  invisible(TRUE)
}
web_no_file_secrets <- function(path,engine_root) {
  secrets<-web_operator_secrets(engine_root)
  if(!length(secrets))return(invisible(TRUE))
  con<-file(path,"rb");on.exit(close(con));tail<-raw()
  repeat {
    chunk<-readBin(con,"raw",n=65536L);if(!length(chunk))break
    bytes<-c(tail,chunk)
    for(secret in secrets) {
      needle<-charToRaw(secret); starts<-which(bytes==needle[1])
      found<-any(vapply(starts,function(i)i+length(needle)-1L<=length(bytes)&&identical(bytes[i:(i+length(needle)-1L)],needle),logical(1)))
      web_assert(!found,"An output contains a private credential and cannot be saved.","SYSTEM_FAILED")
    }
    tail<-tail(bytes,max(nchar(secrets,type="bytes")))
  }
  invisible(TRUE)
}
