# RC28.7 — shared Web runtime contract. Read-only; no installs, DB access or API calls.
# Source from the repository root before loading any application modules.
rc287_locked_r <- function(path) {
  if (!file.exists(path)) stop("RUNTIME_LOCK_MISSING: ", path, call. = FALSE)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  a <- grep('^[[:space:]]*"R"[[:space:]]*:', lines)
  b <- grep('^[[:space:]]*"Packages"[[:space:]]*:', lines)
  if (length(a) != 1L || length(b) != 1L || b <= a + 1L)
    stop("INVALID_RUNTIME_LOCK: top-level R/Packages sections", call. = FALSE)
  versions <- grep('^[[:space:]]*"Version"[[:space:]]*:[[:space:]]*"[0-9]',
                   lines[seq.int(a + 1L, b - 1L)], value = TRUE)
  if (length(versions) != 1L)
    stop("INVALID_RUNTIME_LOCK: R version missing or ambiguous", call. = FALSE)
  sub('^.*"Version"[[:space:]]*:[[:space:]]*"([^"]+)".*$', "\\1", versions[[1L]])
}

rc287_web_library <- function(root) {
  configured <- Sys.getenv("SRMA_WEB_LIBRARY", "")
  if (nzchar(configured)) path.expand(configured) else file.path(root, "web", ".library")
}

rc287_check_web_r <- function(root) {
  pinned <- rc287_locked_r(file.path(root, "web", "renv.lock"))
  actual <- as.character(getRversion())
  if (!identical(pinned, actual))
    stop("WEB_R_VERSION_MISMATCH: required ", pinned, "; current ", actual,
         ". Use the R version from web/renv.lock.", call. = FALSE)
  invisible(pinned)
}

rc287_use_web_library <- function(root) {
  rc287_check_web_r(root)
  lib <- rc287_web_library(root)
  if (!dir.exists(lib))
    stop("WEB_LIBRARY_MISSING: ", lib,
         ". An administrator must run web/bin/setup.R first.", call. = FALSE)
  lib <- normalizePath(lib, winslash = "/", mustWork = TRUE)
  # Do NOT borrow unpinned user/site packages or scientific R 4.6 libraries.
  .libPaths(c(lib, .Library), include.site = FALSE)
  invisible(lib)
}

rc287_dependency_report <- function(root, profile = c("active", "full")) {
  profile <- match.arg(profile)
  rc287_check_web_r(root)
  lib <- rc287_use_web_library(root)
  if (!requireNamespace("jsonlite", quietly = TRUE))
    return(list(ok = FALSE, missing = "jsonlite", unpinned = character(),
                mismatch = character(), unloadable = character(), library = lib))
  lock <- jsonlite::read_json(file.path(root, "web", "renv.lock"), simplifyVector = FALSE)
  deps <- new.env(parent = baseenv())
  sys.source(file.path(root, "web", "config", "dependencies.R"), envir = deps)
  backend <- tolower(Sys.getenv("SRMA_WEB_DB", "sqlite"))
  if (!backend %in% c("sqlite", "postgres"))
    stop("UNSUPPORTED_DB_BACKEND: ", backend, call. = FALSE)
  direct <- unique(as.character(deps$web_packages))
  if (backend == "sqlite") direct <- setdiff(direct, "RPostgreSQL")
  if (backend != "sqlite") direct <- setdiff(direct, "RSQLite")
  required <- if (profile == "full") unique(c(names(lock$Packages), direct)) else direct
  unpinned <- setdiff(required, names(lock$Packages))
  pinned <- intersect(required, names(lock$Packages))
  versions <- vapply(pinned, function(name) {
    value <- tryCatch(utils::packageDescription(name, lib.loc = lib, fields = "Version"),
                      error = function(e) NA_character_)
    if (length(value) != 1L || is.na(value) || !nzchar(value)) "" else as.character(value)
  }, character(1))
  expected <- vapply(pinned, function(name) as.character(lock$Packages[[name]]$Version), character(1))
  missing <- pinned[!nzchar(versions)]
  mismatched <- pinned[nzchar(versions) & versions != expected]
  verified <- setdiff(pinned, c(missing, mismatched))
  unloadable <- verified[!vapply(verified, requireNamespace, logical(1), quietly = TRUE)]
  list(ok = !length(unpinned) && !length(missing) && !length(mismatched) && !length(unloadable),
       missing = missing, unpinned = unpinned,
       mismatch = setNames(paste0(versions[mismatched], " (expected ", expected[mismatched], ")"), mismatched),
       unloadable = unloadable, library = lib,
       checked = length(required), backend = backend)
}

rc287_assert_web_ready <- function(root) {
  check <- rc287_dependency_report(root, "active")
  if (!isTRUE(check$ok)) {
    details <- c(if(length(check$unpinned)) paste("not pinned:", paste(check$unpinned,collapse=", ")),
                 if(length(check$missing)) paste("missing:",paste(check$missing,collapse=", ")),
                 if(length(check$mismatch)) paste("version mismatch:",paste(names(check$mismatch),collapse=", ")),
                 if(length(check$unloadable)) paste("cannot load:",paste(check$unloadable,collapse=", ")))
    stop("WEB_DEPENDENCY_FAILED: ", paste(details, collapse="; "),
         ". Restore the version-pinned Web library with web/bin/setup.R; do not mix with the scientific library.",
         call. = FALSE)
  }
  invisible(check)
}
