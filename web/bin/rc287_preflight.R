# RC28.7 operator preflight: read-only. Execute from the repository root.
source("web/bin/rc287_runtime.R")
root <- normalizePath(".", winslash = "/", mustWork = TRUE)
cat("RC28.7 WEB / SCIENTIFIC RUNTIME PREFLIGHT\n")
cat("Web R:", R.version.string, "\n")
cat("Web library:", rc287_web_library(root), "\n")
result <- tryCatch(rc287_dependency_report(root, "active"), error = identity)
if (inherits(result, "error")) {
  cat("WEB BLOCKED:", conditionMessage(result), "\n")
  quit(save = "no", status = 3L)
}
if (!isTRUE(result$ok)) {
  cat("WEB BLOCKED: dependency contract incomplete\n")
  if (length(result$unpinned)) cat("Not pinned:", paste(result$unpinned, collapse=", "), "\n")
  if (length(result$missing)) cat("Missing:", paste(result$missing, collapse=", "), "\n")
  if (length(result$mismatch)) for (p in names(result$mismatch))
    cat("Mismatch:", p, result$mismatch[[p]], "\n")
  if (length(result$unloadable)) cat("Cannot load:", paste(result$unloadable, collapse=", "), "\n")
  cat("ACTION: use the official web/bin/setup.R (administrator-controlled).\n")
  quit(save = "no", status = 2L)
}
cat("PASS: Web packages", result$checked, "verified for", result$backend, "\n")
scientific_script <- Sys.getenv("SRMA_SCIENTIFIC_RSCRIPT", "")
scientific_lib <- Sys.getenv("SRMA_SCIENTIFIC_LIBRARY", "")
if (!nzchar(scientific_script) || !nzchar(scientific_lib)) {
  cat("SCIENTIFIC: NOT CONFIGURED; analysis/AI jobs remain blocked.\n")
} else if (!file.exists(scientific_script) || !dir.exists(scientific_lib)) {
  cat("SCIENTIFIC: INVALID PATH; analysis/AI jobs remain blocked.\n")
} else {
  cat("SCIENTIFIC: PATHS CONFIGURED; exact lock verification still required via web_science_settings().\n")
}
cat("DATABASE: existing deployment preflight checks backend and schema after startup.\n")
quit(save = "no", status = 0L)
