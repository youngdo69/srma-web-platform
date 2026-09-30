# Source from the canonical repository root, with --vanilla to isolate Web dependencies.
web_root <- normalizePath(Sys.getenv("SRMA_ENGINE_ROOT", getwd()), mustWork = TRUE)
web_library <- Sys.getenv("SRMA_WEB_LIBRARY", file.path(web_root, "web/.library"))
if (dir.exists(web_library)) .libPaths(c(web_library, .libPaths()))
for (module in c("core", "adapter", "persistence", "storage", "service", "identity", "wizard", "database", "pipeline", "evidence", "analysis", "manuscript", "manuscript_render", "final_qc", "final_qc_checks", "release", "security_audit", "deployment", "worker")) {
  source(file.path(web_root, "web/R", paste0(module, ".R")), local = TRUE)
}
