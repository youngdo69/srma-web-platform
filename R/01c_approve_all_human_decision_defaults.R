# Approve all recommended HD defaults
if (!requireNamespace("yaml", quietly=TRUE)) stop("Missing package: yaml")
source("R/utils/human_decision_patch_utils.R")

policy <- yaml::read_yaml("config/human_decision_policy.yml")$human_decision_policy
response_file <- policy$paths$response_file
if (!file.exists(response_file)) stop("Run R/01b_prepare_human_decision_review.R first.")

responses <- normalize_record_array(
  yaml::read_yaml(response_file)$human_decision_responses,
  c("decision_id","question","recommended_option","recommendation_rationale",
    "suggested_options","selected_option","approval_status","reviewer_note"),
  "human_decision_responses"
)

cat("\nRecommended defaults:\n")
for (r in responses) {
  cat("- ", as_character_vector(r$decision_id)[[1]], ": ",
      as_character_vector(r$recommended_option)[[1]], "\n", sep="")
}

confirmation <- readline("\nType APPROVE ALL DEFAULTS to continue: ")
if (!identical(trimws(confirmation), "APPROVE ALL DEFAULTS")) stop("Approval cancelled.")

responses <- lapply(responses, function(r) {
  r$selected_option <- as_character_vector(r$recommended_option)[[1]]
  r$approval_status <- "approved_default"
  r
})

yaml::write_yaml(list(human_decision_responses=responses), response_file)
cat("\nAll recommended defaults approved.\n")
