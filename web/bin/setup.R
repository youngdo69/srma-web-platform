# RC28.7 — explicit operator setup, not part of normal Web/Worker startup.
# Run at the canonical repository root with the R pinned in web/renv.lock.
source("web/bin/rc287_runtime.R")
root <- normalizePath(".", mustWork = TRUE)
rc287_check_web_r(root)  # Fail BEFORE creating or installing anything.
platform <- file.path(root, "web")
library <- rc287_web_library(root)
dir.create(library, recursive = TRUE, showWarnings = FALSE)
library <- normalizePath(library, mustWork = TRUE)
.libPaths(c(library, .Library), include.site = FALSE)
options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 300)
bootstrap <- Sys.getenv("SRMA_RENV_BOOTSTRAP", "")
if (nzchar(bootstrap)) {
  if (!dir.exists(bootstrap)) stop("SRMA_RENV_BOOTSTRAP directory is missing", call. = FALSE)
  .libPaths(c(library, bootstrap, .Library), include.site = FALSE)
}
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", lib = library)
  if (!requireNamespace("renv", quietly = TRUE))
    stop("renv bootstrap failed; Web packages were not restored", call. = FALSE)
}
# Restore the exact active package set from the Web lock; do not attempt
# RPostgreSQL during local SQLite setup (or RSQLite on PostgreSQL hosts).
deps <- new.env(parent = baseenv())
sys.source(file.path(root, "web/config/dependencies.R"), envir = deps)
backend <- tolower(Sys.getenv("SRMA_WEB_DB", "sqlite"))
if (!backend %in% c("sqlite", "postgres"))
  stop("Unsupported SRMA_WEB_DB backend: ", backend, call. = FALSE)
required <- unique(as.character(deps$web_packages))
if (backend == "sqlite") required <- setdiff(required, "RPostgreSQL")
if (backend != "sqlite") required <- setdiff(required, "RSQLite")
lock_path <- file.path(platform, "renv.lock")
lock <- renv::lockfile_read(lock_path)
not_pinned <- setdiff(required, names(lock$Packages))
if (length(not_pinned)) stop("Not in Web lock: ", paste(not_pinned, collapse=", "), call. = FALSE)
renv::restore(project = platform, lockfile = lock_path, library = library,
              packages = required, prompt = FALSE)
# Restrict all verification to the actual installed Web library.
.libPaths(c(library, .Library), include.site = FALSE)
rc287_assert_web_ready(root)
cat("PASS: RC28.7 Web dependency restore; scientific lock untouched.\n")
