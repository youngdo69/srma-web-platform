# Object-store port: immutable put/get. No file paths are returned to ordinary UI consumers.
web_store <- function(settings) {
  root <- file.path(settings$data_root, "objects")
  dir.create(root, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  list(put = function(source, key) {
    web_assert(grepl("^[a-f0-9/]+$", key), "Invalid artifact key.")
    target <- web_inside(file.path(root, key), root, exists = FALSE)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE, mode = "0700")
    if (!file.exists(target)) {
      temp <- tempfile("put-", dirname(target))
      web_assert(file.copy(source, temp), "Could not store the output.", "SYSTEM_FAILED")
      web_assert(file.rename(temp, target), "Could not store the output.", "SYSTEM_FAILED")
    }
    web_assert(identical(web_sha(source), web_sha(target)), "Stored output verification failed.", "SYSTEM_FAILED")
    list(locator = key, checksum = web_sha(target))
  }, get = function(key, checksum) {
    web_assert(grepl("^[a-f0-9/]+$", key), "Invalid artifact key.")
    target <- web_inside(file.path(root, key), root)
    web_assert(file.exists(target) && identical(web_sha(target), checksum), "This output is unavailable or failed integrity verification.", "SYSTEM_FAILED")
    target
  })
}
web_download <- function(con, settings, actor, project_id, artifact_id, destination) {
  web_authorize(con, actor, project_id)
  a <- web_query(con, "SELECT * FROM artifacts WHERE id=?id AND project_id=?p AND status='AVAILABLE'", id = artifact_id, p = project_id)
  web_assert(nrow(a) == 1L, "This output is not available.")
  path <- web_store(settings)$get(a$storage_locator, a$checksum)
  web_assert(file.copy(path, destination, overwrite = TRUE), "Could not prepare the download.", "SYSTEM_FAILED")
  project <- web_authorize(con, actor, project_id)
  web_audit(con, project, "ARTIFACT_DOWNLOADED", artifact_id, actor, list(checksum = a$checksum, logical_name = a$logical_name))
  invisible(destination)
}
