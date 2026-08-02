# Universal SR-MA Agent Screening Build

## Files

Copy each file into the matching path in the project root.

- `.env.example`
- `config/agent_screening.yml`
- `prompts/title_abstract_screening_prompt.md`
- `R/utils/screening_prompt_utils.R`
- `R/utils/llm_provider_utils.R`
- `R/19_run_agent_title_abstract_screening.R`
- `R/20_validate_agent_screening.R`

## Required project_config.yml content

The existing project configuration must provide:

```yaml
project:
  project_id: "..."
  title: "..."

eligibility:
  review_question: "..."
  inclusion:
    - "..."
    - "..."

screening:
  decisions:
    - "Include"
    - "Exclude"
    - "Unclear"
  exclusion_reasons:
    - code: "E01"
      label: "..."
```

The code contains fallback lookups, but explicit review-question and inclusion criteria are strongly recommended.

## API key

Copy `.env.example` to `.env` and enter the real key:

```env
OPENAI_API_KEY=...
```

Do not commit `.env`.

Recommended `.gitignore` entries:

```gitignore
.env
.env.*
!.env.example
```

## Packages

```r
install.packages(c(
  "yaml", "openxlsx", "readr", "dplyr", "tibble",
  "stringr", "jsonlite", "httr2", "dotenv"
))
```

## Pilot run

Set in `config/agent_screening.yml`:

```yaml
processing:
  start_record: 1
  max_records: 10
```

Then run:

```r
source("R/19_run_agent_title_abstract_screening.R")
source("R/20_validate_agent_screening.R")
```

After reviewing the pilot, change `max_records` to `null` and run R/19 again.

## Outputs

- `results/screening/title_abstract_screening_agent.xlsx`
- `results/logs/agent_screening_decisions.csv`
- `results/logs/agent_screening_failures.csv`
- `results/logs/agent_screening_checkpoint.csv`
- `results/logs/agent_screening_report.txt`

Reviewer 2 and adjudication remain human tasks.
