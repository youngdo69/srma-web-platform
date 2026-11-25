# Source from the canonical repository root, with --vanilla to isolate Web dependencies.
web_root <- normalizePath(Sys.getenv("SRMA_ENGINE_ROOT", getwd()), mustWork = TRUE)
web_library <- Sys.getenv("SRMA_WEB_LIBRARY", file.path(web_root, "web/.library"))
# RC28.7: resolve the version-pinned Web library before any Web modules load.
# Scientific dependencies are intentionally managed by the separate canonical runtime.
source(file.path(web_root, "web/bin/rc287_runtime.R"), local = TRUE)
rc287_use_web_library(web_root)
rc287_assert_web_ready(web_root)
for (module in c("core", "adapter", "persistence", "storage", "service", "identity", "wizard", "database", "pipeline", "evidence", "analysis", "manuscript", "manuscript_render", "manuscript_revision", "statistical_methods", "final_qc", "final_qc_checks", "qc_adaptive", "qc_r40", "qc_r40_inputs", "qc_r40_ai", "qc_r40_full", "qc_r40_execute", "qc_confirmation", "qc_followup", "release", "security_audit", "deployment", "worker")) {
  source(file.path(web_root, "web/R", paste0(module, ".R")), local = TRUE)
}
