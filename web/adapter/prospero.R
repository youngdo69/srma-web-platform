# Web-only semantic adapter for canonical PROSPERO preparation.
# The scientific implementation remains R/create_and_autofill_prospero.R.
# This adapter loads all canonical functions while deliberately skipping only
# the CLI entrypoint `run_prospero_preparation()` so the background worker can
# drive the non-interactive draft-preparation stage without changing science.

web_load_prospero_module <- function(engine_root) {
  script <- file.path(engine_root, "R", "create_and_autofill_prospero.R")
  web_assert(file.exists(script), "The canonical PROSPERO module is unavailable.", "DEPENDENCY_FAILED")
  web_assert(requireNamespace("openxlsx", quietly = TRUE),
    "The scientific processing environment needs the openxlsx package.", "DEPENDENCY_FAILED")

  lines <- readLines(script, warn = FALSE, encoding = "UTF-8")
  nonblank <- which(nzchar(trimws(lines)))
  web_assert(length(nonblank) > 0L, "The canonical PROSPERO module is empty.", "SYSTEM_FAILED")
  entry <- tail(nonblank, 1L)
  web_assert(identical(trimws(lines[[entry]]), "run_prospero_preparation()"),
    "The canonical PROSPERO module entrypoint changed; the Web adapter requires review.", "SYSTEM_FAILED")

  module <- new.env(parent = globalenv())
  code <- paste(lines[-entry], collapse = "\n")
  eval(parse(text = code, keep.source = FALSE), envir = module)
  module
}

web_prospero_field_spec <- function(module) {
  sections <- module$prospero_sections()
  rows <- lapply(names(sections), function(section) {
    items <- sections[[section]]
    do.call(rbind, lapply(items, function(x) {
      data.frame(
        section = section,
        field_id = as.character(x[[1]]),
        label = as.character(x[[2]]),
        key = as.character(x[[3]]),
        stringsAsFactors = FALSE
      )
    }))
  })
  do.call(rbind, rows)
}

web_prepare_prospero_canonical <- function(request, config, root, manifest) {
  module <- web_load_prospero_module(request$engine_root)
  out_dir <- file.path(root, "results", "registration", "prospero")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Never write generated templates back into the protected scientific engine.
  template_path <- file.path(out_dir, "Universal_PROSPERO_Form_Template.xlsx")
  autofilled_path <- file.path(out_dir, "prospero_autofilled.xlsx")
  provenance_path <- file.path(out_dir, "prospero_drafting_provenance.yml")

  module$create_prospero_template(template_path)

  cfg <- yaml::read_yaml(file.path(root, "config", "project_config.yml"))
  search_terms_path <- file.path(root, "config", "search_terms.yml")
  search_terms <- if (file.exists(search_terms_path)) yaml::read_yaml(search_terms_path) else list()
  project_context <- module$collect_active_project_context(repo_root = root, project_root = root)
  state <- module$build_deterministic_draft(cfg = cfg, project_text = project_context)

  llm_result <- module$run_llm_protocol_drafting(
    repo_root = request$engine_root,
    cfg = cfg,
    search_terms = search_terms,
    project_context = project_context,
    baseline_info = state$info,
    out_dir = out_dir
  )
  if (isTRUE(llm_result$ok)) state <- module$merge_llm_draft(state, llm_result)

  summary <- module$autofill_prospero(
    template_path = template_path,
    output_path = autofilled_path,
    state = state
  )
  project_slug <- if (exists("get_project_id", inherits = TRUE)) get_project_id(config) else request$project_id
  module$write_provenance_file(
    out_dir = out_dir,
    project_slug = project_slug,
    state = state,
    llm_result = llm_result
  )

  spec <- web_prospero_field_spec(module)
  req <- summary$audit[summary$audit$Status == "USER_REQUIRED", , drop = FALSE]
  required <- lapply(seq_len(nrow(req)), function(i) {
    hit <- spec[spec$field_id == req$Field_ID[[i]], , drop = FALSE]
    list(
      field_id = req$Field_ID[[i]],
      section = if (nrow(hit)) hit$section[[1]] else "",
      label = if (nrow(hit)) hit$label[[1]] else req$Auto.fill.key[[i]],
      key = req$Auto.fill.key[[i]],
      current_value = req$Generated_value[[i]]
    )
  })

  artifacts <- list(
    list(path = web_inside(autofilled_path, root), logical_name = "prospero_autofilled.xlsx", type = "prospero_draft"),
    list(path = web_inside(provenance_path, root), logical_name = "prospero_drafting_provenance.yml", type = "prospero_provenance")
  )

  web_result(
    request$action,
    request$project_id,
    request$job_id,
    "COMPLETE",
    "PROSPERO draft prepared. Review the USER REQUIRED items, submit through PROSPERO, then return with the registration ID.",
    request$engine_version,
    artifacts = artifacts,
    provenance = list(
      input_hashes = manifest,
      engine_script_sha256 = web_sha(file.path(request$engine_root, "R", "create_and_autofill_prospero.R")),
      R = R.version.string,
      prospero = list(
        counts = as.list(summary$counts),
        user_required = required,
        llm_status = if (isTRUE(llm_result$ok)) "LLM_ASSISTED" else "DETERMINISTIC_FALLBACK",
        llm_note = if (isTRUE(llm_result$ok)) "" else llm_result$reason
      )
    )
  )
}
