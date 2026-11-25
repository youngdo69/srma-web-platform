# Apply selected HD patches to generated drafts
required <- c("yaml","jsonlite")
missing <- required[!vapply(required, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
if (length(missing)) stop("Missing package(s): ", paste(missing, collapse=", "))

source("R/utils/human_decision_patch_utils.R")
policy <- yaml::read_yaml("config/human_decision_policy.yml")$human_decision_policy
paths <- policy$paths

required_files <- c(
  paths$generated_decisions, paths$response_file,
  paths$project_config_draft, paths$search_terms_draft
)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files)) stop("Missing:\n- ", paste(missing_files, collapse="\n- "))

decisions <- normalize_human_decisions_v3(
  yaml::read_yaml(paths$generated_decisions)$human_decisions_required
)
responses <- normalize_record_array(
  yaml::read_yaml(paths$response_file)$human_decision_responses,
  c("decision_id","question","recommended_option","recommendation_rationale",
    "suggested_options","selected_option","approval_status","reviewer_note"),
  "human_decision_responses"
)

responses <- lapply(responses, function(r) list(
  decision_id=as_character_vector(r$decision_id)[[1]],
  selected_option=as_character_vector(r$selected_option)[[1]],
  approval_status=as_character_vector(r$approval_status)[[1]],
  reviewer_note=paste(as_character_vector(r$reviewer_note), collapse=" ")
))

dmap <- setNames(decisions, vapply(decisions, `[[`, character(1), "decision_id"))
rmap <- setNames(responses, vapply(responses, `[[`, character(1), "decision_id"))

missing_responses <- setdiff(names(dmap), names(rmap))
if (length(missing_responses)) stop("Missing responses: ", paste(missing_responses, collapse=", "))

project_config <- yaml::read_yaml(paths$project_config_draft)
search_terms <- yaml::read_yaml(paths$search_terms_draft)
resolutions <- list()
applied <- list()

for (id in names(dmap)) {
  d <- dmap[[id]]
  r <- rmap[[id]]

  if (!(r$approval_status %in% c("approved_default","approved_selected")))
    stop("Unresolved decision: ", id)

  if (!(r$selected_option %in% d$suggested_options))
    stop("Unknown option for ", id, ": ", r$selected_option)

  idx <- which(vapply(d$option_patches, function(p) identical(p$option, r$selected_option), logical(1)))
  if (length(idx) != 1L) stop("Expected one patch set for ", id, " / ", r$selected_option)

  for (op in d$option_patches[[idx]]$operations) {
    if (op$target == "project_config") {
      project_config <- apply_patch_operation(project_config, op)
    } else {
      search_terms <- apply_patch_operation(search_terms, op)
    }
    applied[[length(applied)+1L]] <- list(
      decision_id=id, selected_option=r$selected_option,
      target=op$target, path=op$path, operation=op$operation,
      rationale=op$rationale
    )
  }

  resolutions[[length(resolutions)+1L]] <- list(
    decision_id=id,
    recommended_option=d$recommended_option,
    selected_option=r$selected_option,
    approval_status=r$approval_status,
    reviewer_note=r$reviewer_note,
    resolved_at=format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )
}

dir.create(paths$archive_directory, recursive=TRUE, showWarnings=FALSE)
stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
for (f in required_files) {
  dest <- file.path(
    paths$archive_directory,
    paste0(tools::file_path_sans_ext(basename(f)), "_", stamp, ".yml")
  )
  if (!file.copy(f, dest, overwrite=FALSE)) stop("Could not archive: ", f)
}

project_config$human_decision_resolutions <- resolutions
search_terms$human_decision_resolutions <- resolutions

yaml::write_yaml(project_config, paths$project_config_draft)
yaml::write_yaml(search_terms, paths$search_terms_draft)
yaml::write_yaml(list(human_decisions_required=list()), paths$generated_decisions)
yaml::write_yaml(
  list(
    metadata=list(
      policy_version=policy$version,
      resolved_at=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      decision_count=length(resolutions),
      operation_count=length(applied)
    ),
    resolutions=resolutions,
    applied_operations=applied
  ),
  paths$resolution_log
)

cat("\nApplied HD patches.\nDecisions:", length(resolutions),
    "\nOperations:", length(applied), "\n")
