# Verify HD resolution and application
if (!requireNamespace("yaml", quietly=TRUE)) stop("Missing package: yaml")

policy <- yaml::read_yaml("config/human_decision_policy.yml")$human_decision_policy
p <- policy$paths
errors <- character(0)
warnings <- character(0)

required <- c(p$generated_decisions,p$project_config_draft,p$search_terms_draft,p$resolution_log)
missing <- required[!file.exists(required)]
if (length(missing)) errors <- c(errors, paste("Missing:", paste(missing, collapse=", ")))

if (!length(errors)) {
  unresolved <- yaml::read_yaml(p$generated_decisions)$human_decisions_required
  if (!is.null(unresolved) && length(unresolved)) errors <- c(errors, "Unresolved HDs remain.")

  pc <- yaml::read_yaml(p$project_config_draft)
  st <- yaml::read_yaml(p$search_terms_draft)
  log <- yaml::read_yaml(p$resolution_log)

  if (is.null(pc$human_decision_resolutions)) errors <- c(errors, "Project draft lacks resolutions.")
  if (is.null(st$human_decision_resolutions)) errors <- c(errors, "Search draft lacks resolutions.")
  if (is.null(log$resolutions)) errors <- c(errors, "Resolution log lacks resolutions.")
  if (is.null(log$applied_operations)) warnings <- c(warnings, "No operations recorded.")
}

report <- list(
  verified_at=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  passed=!length(errors), errors=errors, warnings=warnings
)
dir.create(dirname(p$verification_report), recursive=TRUE, showWarnings=FALSE)
yaml::write_yaml(report, p$verification_report)

if (length(errors)) stop("HD verification failed:\n- ", paste(errors, collapse="\n- "))
cat("\nPASS: HD application verified.\n")
if (length(warnings)) cat("Warnings:\n- ", paste(warnings, collapse="\n- "), "\n")
