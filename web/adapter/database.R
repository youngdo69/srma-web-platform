# Canonical database metadata, paths and validation. No Web parser/search builder.
web_database_context <- function(e, config, paths, root, engine_root) {
  dir.create("R/utils", recursive = TRUE, showWarnings = FALSE)
  for (name in c("config_utils.R", "path_utils.R"))
    file.copy(file.path(engine_root, "R/utils", name), file.path("R/utils", name), overwrite = TRUE)
  sys.source(file.path(engine_root, "R/TASK018_database_input_gate_hardening.R"), envir = e)
  registry <- e$task018_builtin_registry()
  enabled <- e$get_enabled_databases(config, validate = FALSE)
  lapply(enabled, function(db) {
    spec <- registry[[db]]
    web_assert(!is.null(spec), "This database needs an operator-reviewed integration.")
    search <- paths[[spec$canonical_search_path_keys[[1]]]]
    target <- paths[[spec$canonical_input_path_keys[[1]]]]
    search <- web_inside(search, root, exists = FALSE); target <- web_inside(target, root, exists = FALSE)
    list(database = db, label = spec$display_name, mode = spec$retrieval_mode,
      format = spec$preferred_export_format, extensions = as.list(spec$accepted_extensions),
      target = substring(target, nchar(root) + 2L),
      instructions = as.list(spec$search_instructions),
      strategy = if (file.exists(search)) paste(readLines(search, warn = FALSE), collapse = "\n") else "",
      input_present = file.exists(target) && file.info(target)$size > 0L)
  })
}
