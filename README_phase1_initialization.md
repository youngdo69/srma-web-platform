# Phase 1 — Project Initialization Agent

## Install

Copy the package contents into the project root. Do not place them in a nested
package directory.

## Files created

- `config/project_seed.yml`
- `config/initialization_agent.yml`
- `config/migration_manifest.yml`
- `prompts/project_initialization_prompt.md`
- `R/00_run_project_initialization_agent.R`
- `R/01_validate_project_initialization.R`
- `R/02_activate_project_configuration.R`
- `R/utils/initialization_prompt_utils.R`

The existing `R/utils/llm_provider_utils.R` from Agent Reviewer screening is reused.

## Important compatibility note

The existing provider function is named `call_llm_screening_provider()`.
It is reused temporarily even though initialization is not screening.
A later utility refactor should rename the generic function to
`call_llm_structured_provider()` while retaining a backward-compatible wrapper
for `R/21_run_agent_title_abstract_screening.R`.

## Run order

1. Edit `config/project_seed.yml`.
2. Run:

```r
source("R/00_run_project_initialization_agent.R")
source("R/01_validate_project_initialization.R")
```

3. Review all files under `config/generated/`.
4. Resolve every item in `initialization_decisions_required.yml`.
5. Replace the resolved list with:

```yaml
human_decisions_required: []
```

6. Run:

```r
source("R/02_activate_project_configuration.R")
```

7. Type `ACTIVATE` only after reviewing the drafts.

## Safety design

The initialization agent never directly overwrites active configuration.
Activation archives existing active configs before copying reviewed drafts.
