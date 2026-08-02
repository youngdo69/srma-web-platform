# HD schema and automatic patch system

This package replaces the earlier simple HD response workflow.

## Replaced files
- `R/utils/initialization_prompt_utils.R`
- `prompts/project_initialization_prompt.md`

## Added files
- `config/human_decision_policy.yml`
- `R/utils/human_decision_patch_utils.R`
- `R/01b_prepare_human_decision_review.R`
- `R/01c_approve_all_human_decision_defaults.R`
- `R/01d_apply_human_decision_patches.R`
- `R/01e_verify_human_decision_application.R`

## Important
Existing HDs were generated under the old schema and do not contain
`recommended_option`, `recommendation_rationale`, or `option_patches`.
After installing this package, archive the current generated drafts and rerun
the initialization agent.

```r
dir.create(
  "config/generated_schema_v2_archive",
  recursive = TRUE,
  showWarnings = FALSE
)

old_files <- list.files("config/generated", full.names = TRUE)

if (length(old_files) > 0L) {
  file.copy(
    old_files,
    "config/generated_schema_v2_archive",
    recursive = TRUE,
    overwrite = TRUE
  )
}

unlink("config/generated", recursive = TRUE)

dir.create(
  "config/generated",
  recursive = TRUE,
  showWarnings = FALSE
)

source("R/00_run_project_initialization_agent.R")
source("R/01_validate_project_initialization.R")
```

## Approve all recommended defaults

```r
source("R/01b_prepare_human_decision_review.R")
source("R/01c_approve_all_human_decision_defaults.R")
source("R/01d_apply_human_decision_patches.R")
source("R/01e_verify_human_decision_application.R")
source("R/01_validate_project_initialization.R")
```

At the prompt type:

```text
APPROVE ALL DEFAULTS
```

## Approve selected alternatives
Run `R/01b_prepare_human_decision_review.R`, edit
`config/generated/human_decision_responses.yml`, then set:

```yaml
selected_option: "the chosen option"
approval_status: "approved_selected"
```

Apply and verify with `R/01d` and `R/01e`.

## Audit trail
- Resolution log: `config/generated/human_decision_resolution_log.yml`
- Pre-application archives: `config/generated/decision_archive/`
- Verification report: `results/logs/human_decision_application_verification.yml`
