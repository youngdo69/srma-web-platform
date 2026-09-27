web_version_root <- function(con, settings, actor, project_id, version_id) {
  web_authorize(con, actor, project_id)
  version <- web_query(con, "SELECT * FROM project_versions WHERE id=?v AND project_id=?p", v = version_id, p = project_id)
  web_assert(nrow(version) == 1L, "This project version is not available.")
  root <- web_inside(file.path(settings$data_root, version$snapshot_locator), settings$data_root)
  manifest <- web_parse(version$manifest_json)
  for (p in names(manifest)) web_assert(identical(web_sha(web_inside(file.path(root, p), root)), manifest[[p]]),
    "Saved inputs need operator review.", "SYSTEM_FAILED")
  list(root = root, manifest = manifest)
}
web_edit_configuration <- function(con, settings, actor, project_id, version_id) {
  version <- web_version_root(con, settings, actor, project_id, version_id)
  yaml::read_yaml(file.path(version$root, "config/project_config.yml"))
}
web_save_configuration <- function(con, settings, actor, workspace_id, values, project_id = NULL, version_id = NULL) {
  web_assert(nrow(web_query(con, "SELECT user_id FROM workspace_members WHERE workspace_id=?w AND user_id=?u", w = workspace_id, u = actor)) == 1L,
    "This workspace is not accessible.")
  required <- c("project.title", "project.project_id", "research_question.framework", "research_question.question",
    "research_question.population.description", "research_question.intervention.description", "research_question.comparator.description",
    "research_question.outcomes.primary", "eligibility.inclusion", "eligibility.exclusion", "eligibility.study_designs.include", "exclusion_reason")
  web_assert(is.list(values) && all(required %in% names(values)), "Complete the required review fields.")
  for (key in required) web_assert(length(values[[key]]) > 0L && all(nzchar(trimws(unlist(values[[key]])))), "Complete the required review fields.")
  web_assert(all(nchar(unlist(values)) <= 10000L) && sum(nchar(unlist(values))) <= 100000L, "Review metadata is too large.")
  web_assert(values$research_question.framework %in% c("PICO", "PICOS"), "Choose PICO or PICOS.")
  web_assert(length(values$databases) > 0L && all(unlist(values$databases) %in% c("pubmed", "embase", "central")), "Choose at least one supported database.")
  for (key in c("search.search_start_date", "search.search_end_date")) {
    date <- values[[key]]
    web_assert(is.character(date) && length(date) == 1L && (date == "" || (!is.na(suppressWarnings(as.Date(date, "%Y-%m-%d"))) && format(as.Date(date), "%Y-%m-%d") == date)), "Use a valid search date or choose inception.")
  }
  if (nzchar(values$search.search_start_date) && nzchar(values$search.search_end_date))
    web_assert(values$search.search_start_date <= values$search.search_end_date, "The search end date must follow its start date.")
  root <- tempfile("configuration-", tmpdir = settings$data_root)
  dir.create(root, mode = "0700"); on.exit(unlink(root, recursive = TRUE), add = TRUE)
  if (!is.null(project_id)) {
    project <- web_authorize(con, actor, project_id)
    web_assert(project$workspace_id == workspace_id && project$canonical_id == values$project.project_id,
      "Keep the project identifier and workspace when editing a saved review.")
    version <- web_version_root(con, settings, actor, project_id, version_id)
    # A metadata edit starts a fresh scientific draft. Generated queries/raw exports
    # cannot silently survive a change in eligibility or research question.
    web_copy_version(con,settings,actor,project_id,version_id,root,core_only=TRUE)
  } else web_assert(!nrow(web_query(con, "SELECT id FROM projects WHERE workspace_id=?w AND canonical_id=?c", w = workspace_id, c = values$project.project_id)),
    "This project identifier already exists. Open it to create a new version.")
  result <- web_engine_call(settings, list(action = "write_configuration", project_dir = root, values = values))
  if (result$status != "COMPLETE") web_error(result$error_code, result$user_message, result$technical$diagnostic)
  saved <- web_import_project(con, settings, actor, workspace_id, root, configuration_only = TRUE, draft_gates = TRUE)
  saved
}
