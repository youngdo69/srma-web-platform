# Read-only scientific runtime proof. The caller supplies a server-configured library.
a <- commandArgs(trailingOnly = TRUE)
r <- jsonlite::fromJSON(a[[1]], simplifyVector = FALSE)
.libPaths(c(r$engine_library, .Library), include.site = FALSE)
lock <- jsonlite::read_json(file.path(r$engine_root, "renv.lock"))
records <- lapply(names(lock$Packages), function(p) {
  expected <- lock$Packages[[p]]$Version
  actual <- tryCatch(utils::packageDescription(p, fields = "Version"), error = function(e) NA_character_)
  found <- tryCatch(find.package(p), error = function(e) "")
  loaded <- suppressWarnings(requireNamespace(p, quietly = TRUE))
  list(package = p, expected = expected, actual = actual, loaded = loaded, library = dirname(found),
    matches = loaded && !is.na(actual) && utils::packageVersion(p) == package_version(expected))
})
ok <- getRversion() == package_version(lock$R$Version) && all(vapply(records, function(x) x$matches, logical(1)))
result <- list(status = if (ok) "COMPLETE" else "FAILED", error_code = if (!ok) "DEPENDENCY_FAILED" else NULL,
  user_message = if (ok) "Canonical scientific runtime verified." else "The scientific processing environment needs maintenance.",
  R = R.version.string, packages = records, canonical_lock_sha256 = digest::digest(file = file.path(r$engine_root, "renv.lock"), algo = "sha256"),
  docx_dependencies = setNames(lapply(c("officer", "flextable"), function(p) requireNamespace(p, quietly = TRUE)), c("officer", "flextable")))
jsonlite::write_json(result, a[[2]], auto_unbox = TRUE, null = "null", pretty = TRUE)
