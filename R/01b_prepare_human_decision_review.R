# Prepare auditable human-decision response file
required <- c("yaml","jsonlite")
missing <- required[!vapply(required, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
if (length(missing)) stop("Missing package(s): ", paste(missing, collapse=", "))

source("R/utils/human_decision_patch_utils.R")
policy <- yaml::read_yaml("config/human_decision_policy.yml")$human_decision_policy
decisions_file <- policy$paths$generated_decisions
response_file <- policy$paths$response_file

if (!file.exists(decisions_file)) stop("Missing: ", decisions_file)

decisions <- normalize_human_decisions_v3(
  yaml::read_yaml(decisions_file)$human_decisions_required
)

if (!length(decisions)) {
  cat("\nNo unresolved human decisions.\n")
} else {
  responses <- lapply(decisions, function(d) list(
    decision_id=d$decision_id,
    question=d$question,
    recommended_option=d$recommended_option,
    recommendation_rationale=d$recommendation_rationale,
    suggested_options=d$suggested_options,
    selected_option=d$recommended_option,
    approval_status="pending",
    reviewer_note=""
  ))
  yaml::write_yaml(list(human_decision_responses=responses), response_file)
  cat("\nCreated:", response_file, "\nDecisions:", length(decisions), "\n")
}
