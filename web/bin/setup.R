# Run from repository root: Rscript --vanilla web/bin/setup.R
platform <- normalizePath("web", mustWork = TRUE)
library <- file.path(platform, ".library")
dir.create(library, recursive = TRUE, showWarnings = FALSE)
.libPaths(c(library, .libPaths()))
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv", lib = library, repos = "https://cloud.r-project.org")
renv::restore(project = platform, lockfile = file.path(platform, "renv.lock"), library = library, prompt = FALSE)
cat("Web development dependencies restored. Canonical scientific renv.lock was not changed.\n")
