# AGENTS.md

## Project purpose

This repository contains reusable skills and harnesses for systematic
reviews and meta-analyses in cardiovascular surgery.

## General rules

- Never fabricate study data, references, DOI numbers, or extracted outcomes.
- Preserve a full audit trail for search, deduplication, screening,
  extraction, exclusion, and statistical analysis.
- Use PRISMA 2020 terminology.
- Distinguish verified data from inferred or estimated data.
- Do not overwrite source datasets.
- Write generated outputs to the outputs/ directory.
- Run validation tests before accepting a meta-analysis result.

## Statistical rules

- Report effect measure, confidence interval, heterogeneity, model,
  and number of studies.
- Explain all continuity corrections.
- Do not pool clinically incompatible outcomes.
- Flag outcomes with fewer than three studies.
- Do not perform publication-bias testing when the number of studies
  is insufficient unless explicitly requested and clearly qualified.

## Environment

- Python environment: .venv
- R scripts: analysis/r/
- Python scripts: analysis/python/
- Temporary files: tmp/
- Final outputs: outputs/