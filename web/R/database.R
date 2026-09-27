web_database_info <- function(con, settings, actor, project_id, version_id, private = FALSE) {
  version <- web_version_root(con, settings, actor, project_id, version_id)
  # Canonical path construction may create directories; inspect a disposable copy.
  root <- tempfile("database-info-", settings$data_root); dir.create(root, mode = "0700")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (p in names(version$manifest)) {
    dest <- file.path(root, p); dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    web_assert(file.copy(file.path(version$root, p), dest), "Inputs could not be prepared.", "SYSTEM_FAILED")
  }
  for(j in web_pipeline_sources(con,project_id,version_id,"validate_database_inputs"))
    web_overlay_job(con,settings,actor,project_id,version_id,j,root)
  result <- web_engine_call(settings, list(action = "database_context", project_dir = root))
  if (result$status != "COMPLETE") web_error(result$error_code, result$user_message, result$technical$diagnostic)
  if (!private) result$databases <- lapply(result$databases, function(x) {x$target <- NULL; x})
  result$databases
}
web_upload_database <- function(con, settings, actor, project_id, version_id, database,
                                 upload, upload_root, max_bytes = 20 * 1024^2) {
  project <- web_authorize(con, actor, project_id)
  web_assert(is.data.frame(upload) && nrow(upload) == 1L && all(c("name", "datapath", "size") %in% names(upload)), "Choose one database export.")
  name <- upload$name[[1]]
  web_assert(is.character(name) && nchar(name) <= 200L && grepl("^[A-Za-z0-9][A-Za-z0-9_. -]*$", name) && !grepl("..", name, fixed = TRUE), "Use a simple export filename without folders.")
  source <- web_inside(upload$datapath[[1]], upload_root)
  web_assert(!dir.exists(source) && !nzchar(Sys.readlink(upload$datapath[[1]])), "This upload is not available.")
  size <- file.info(source)$size
  web_assert(is.finite(size) && size > 0L && size <= max_bytes && isTRUE(as.numeric(upload$size[[1]]) == size), "The export must be nonempty and no larger than 20 MB.")
  info <- web_database_info(con, settings, actor, project_id, version_id, private = TRUE)
  spec <- Filter(function(x) identical(x$database, database), info)
  web_assert(length(spec) == 1L && spec[[1]]$mode == "guided_manual", "Choose an enabled database that requires manual export.")
  spec <- spec[[1]]
  web_assert(tolower(tools::file_ext(name)) %in% unlist(spec$extensions), paste0("Upload the required ", spec$format, " export."))
  version <- web_version_root(con, settings, actor, project_id, version_id)
  root <- tempfile("upload-version-", settings$data_root); dir.create(root, mode = "0700")
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (p in names(version$manifest)) {
    dest <- file.path(root, p); dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    web_assert(file.copy(file.path(version$root, p), dest), "Inputs could not be prepared.", "SYSTEM_FAILED")
  }
  sources <- web_pipeline_sources(con,project_id,version_id,"validate_database_inputs")
  for(j in sources) web_overlay_job(con,settings,actor,project_id,version_id,j,root)
  if(length(sources)) {
    dir.create(file.path(root,"config/generated"),recursive=TRUE,showWarnings=FALSE)
    yaml::write_yaml(list(source_version=version_id,source_jobs=as.list(sources),
      reason="Explicit export upload into the selected immutable input version; approvals are not inherited."),
      file.path(root,"config/generated/web_import_provenance.yml"))
  }
  hash <- web_sha(source)
  target <- web_inside(file.path(root, spec$target), root, exists = FALSE)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  web_assert(file.copy(source, target, overwrite = TRUE) && identical(web_sha(target), hash), "The export changed during upload. Upload it again.")
  saved <- web_import_project(con, settings, actor, project$workspace_id, root, configuration_only = TRUE, draft_gates = TRUE)
  web_audit(con, project, "DATABASE_EXPORT_UPLOADED", saved$version_id, actor,
    list(database = database, sha256 = hash, size = size, source_version = version_id, validation = "PENDING_CANONICAL_PREFLIGHT"))
  saved
}
# Compatibility boundary pinned to Shiny 1.8.0, with a real upload-context test.
# Place server-created upload files in a session-owned root before accepting any input.
web_bind_upload_root <- function(session, settings) {
  root <- tempfile("session-upload-", settings$data_root); dir.create(root, mode = "0700")
  private <- session$.__enclos_env__$private
  web_assert(is.environment(private) && exists("fileUploadContext", envir = private, inherits = FALSE),
    "Uploads are unavailable in this session.", "SYSTEM_FAILED")
  private$fileUploadContext <- shiny:::FileUploadContext$new(dir = root)
  session$onSessionEnded(function() unlink(root, recursive = TRUE))
  root
}
web_download_strategy <- function(con, settings, actor, project_id, version_id, database, destination) {
  info <- web_database_info(con, settings, actor, project_id, version_id)
  selected <- Filter(function(x) identical(x$database, database), info)
  web_assert(length(selected) == 1L && nzchar(selected[[1]]$strategy), "No canonical search strategy is saved for this database yet.")
  writeLines(selected[[1]]$strategy, destination, useBytes = TRUE)
  invisible(destination)
}
web_database_status <- function(con, settings, actor, project_id, version_id) {
  web_authorize(con, actor, project_id)
  web_assert(nrow(web_query(con,"SELECT id FROM project_versions WHERE id=?v AND project_id=?p",v=version_id,p=project_id))==1L,
    "This project version is not available.")
  job <- web_query(con, "SELECT id,state,message FROM jobs WHERE project_id=?p AND version_id=?v AND operation='validate_database_inputs' ORDER BY created_at DESC LIMIT 1", p = project_id, v = version_id)
  if (!nrow(job)) return(data.frame(Status = "This version has not been checked. Run database preflight."))
  if (!job$state %in% c("COMPLETE", "USER_ACTION_REQUIRED")) return(data.frame(Status = job$message))
  artifacts <- web_query(con, "SELECT * FROM artifacts WHERE project_id=?p AND version_id=?v AND job_id=?j AND artifact_type='database_preflight' AND status='AVAILABLE'", p = project_id, v = version_id, j = job$id)
  csv <- artifacts[grepl("\\.csv$", artifacts$logical_name), , drop = FALSE]
  if (nrow(csv) != 1L) return(data.frame(Status = job$message))
  path <- web_store(settings)$get(csv$storage_locator, csv$checksum)
  report <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
  web_assert(all(c("database", "status", "record_count") %in% names(report)), "The preflight report needs operator review.", "SYSTEM_FAILED")
  meanings <- c(READY = "Input passed canonical checks", MISSING_USER_EXPORT = "Search, export and upload this database",
    MISSING_AUTOMATED_OUTPUT = "Automatic retrieval is still required", INVALID_DATABASE_INPUT = "Correct the export and upload a new version")
  message <- unname(meanings[report$status]); message[is.na(message)] <- "Review the canonical preflight report before continuing"
  data.frame(Database = report$database, Validation = report$status, Records = report$record_count, Next = message)
}
