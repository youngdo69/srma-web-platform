# ============================================================
# Universal SR-MA Agent Toolkit
# R/02_activate_project_configuration.R
# ============================================================

required_packages <- c("yaml")
if (!requireNamespace("yaml", quietly = TRUE)) stop("Missing package: yaml")

agent_document <- yaml::read_yaml("config/initialization_agent.yml")
agent_config <- agent_document$initialization_agent

if (!isTRUE(agent_config$activation$require_human_approval)) {
  stop("Human approval must remain required for activation.")
}

output_dir <- agent_config$output$generated_directory
project_draft <- file.path(output_dir, agent_config$output$project_config_draft)
search_draft <- file.path(output_dir, agent_config$output$search_terms_draft)
decisions_file <- file.path(output_dir, agent_config$output$decisions_required)

for (file in c(project_draft, search_draft, decisions_file)) {
  if (!file.exists(file)) stop("Missing required draft: ", file)
}

decisions <- yaml::read_yaml(decisions_file)$human_decisions_required
if (length(decisions) > 0L) {
  stop(
    "Activation blocked: unresolved human decisions remain in ",
    decisions_file,
    ". Resolve them and replace the list with human_decisions_required: []"
  )
}

approval <- readline(
  prompt = paste0(
    "Type ACTIVATE to archive current configs and activate the reviewed drafts: "
  )
)

if (!identical(trimws(approval), "ACTIVATE")) {
  stop("Activation cancelled.")
}

archive_dir <- agent_config$activation$archive_directory
dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

active_project <- agent_config$activation$active_project_config
active_search <- agent_config$activation$active_search_terms

if (file.exists(active_project)) {
  file.copy(
    active_project,
    file.path(archive_dir, paste0("project_config_", stamp, ".yml")),
    overwrite = FALSE
  )
}

if (file.exists(active_search)) {
  file.copy(
    active_search,
    file.path(archive_dir, paste0("search_terms_", stamp, ".yml")),
    overwrite = FALSE
  )
}

if (!file.copy(project_draft, active_project, overwrite = TRUE)) {
  stop("Failed to activate project config.")
}
if (!file.copy(search_draft, active_search, overwrite = TRUE)) {
  stop("Failed to activate search terms.")
}

cat("\nConfiguration activation complete.\n")
cat("Active project config:", active_project, "\n")
cat("Active search terms:", active_search, "\n")
cat("Archive directory:", archive_dir, "\n")
