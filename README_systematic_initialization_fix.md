# Systematic initialization schema fix

This patch aligns four layers:

1. Structured-output JSON schema
2. Initialization prompt
3. Draft validator
4. Existing draft migration

## Replace these files

- `R/utils/initialization_prompt_utils.R`
- `prompts/project_initialization_prompt.md`
- `R/01_validate_project_initialization.R`

## Add this one-time migration script

- `R/01a_migrate_existing_initialization_draft.R`

## Recommended sequence for the existing generated draft

```r
source("R/01a_migrate_existing_initialization_draft.R")
source("R/01_validate_project_initialization.R")
```

The migration script changes only:

```yaml
screening:
  decisions:
    - Include
    - Exclude
    - Unclear
  decision_rules:
    include: [...]
    exclude: [...]
    unclear: [...]
```

The existing exclusion reasons, eligibility criteria, search concepts, and human
decisions are preserved.

## Recommended sequence for a clean regeneration

After replacing the schema and prompt files:

```r
unlink("config/generated", recursive = TRUE)
dir.create("config/generated", recursive = TRUE, showWarnings = FALSE)

source("R/00_run_project_initialization_agent.R")
source("R/01_validate_project_initialization.R")
```

A clean regeneration uses an additional API call. The one-time migration route
avoids that call and preserves the current draft.
