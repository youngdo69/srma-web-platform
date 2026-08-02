# AGENTS.md



## Mandatory Startup Procedure

Before planning, editing, generating, or running any project code, you must:

1. Read `PROJECT_ROADMAP.md`.
2. Read `PROJECT_STATUS.md`.
3. Identify:
   - the current project phase
   - the current active task
   - the last completed task
   - known blockers
   - deferred work
4. Confirm that the requested work is consistent with the roadmap.
5. Continue from the current status instead of restarting or skipping steps.

Do not modify project code before reading both files.

`PROJECT_ROADMAP.md` is the authoritative source for overall strategy and
task ordering.

`PROJECT_STATUS.md` is the authoritative source for current progress,
completed work, blockers, and the next planned task.

If the user's latest explicit instruction conflicts with either document,
follow the user's instruction and update the affected project document.


## Mandatory End-of-Task Procedure

A project task is not complete until `PROJECT_STATUS.md` has been reviewed
and updated when the task changes project progress.

After completing a task:

1. Verify that the expected output files were created.
2. Record the completed task in `PROJECT_STATUS.md`.
3. Update:
   - current phase
   - last completed task
   - completed outputs
   - current next task
   - blockers
   - deferred work, if changed
   - last updated date
4. Do not mark a task as completed if validation failed.
5. If the task is only partially completed, record it as `In progress`
   and describe the remaining work.
6. Mention in the final response that `PROJECT_STATUS.md` was updated.

Do not change `PROJECT_ROADMAP.md` for ordinary progress updates.
Update the roadmap only when the user changes overall strategy,
architecture, or task ordering.

## Project purpose

This repository contains reusable skills and harnesses for systematic

reviews and meta-analyses in cardiovascular surgery.


## Project Direction

Before performing any coding, refactoring, automation, or workflow-design task, read:

`PROJECT_ROADMAP.md`

`PROJECT_ROADMAP.md` is the authoritative source for:

* development phases
* task ordering
* architecture decisions
* universalization timing
* configuration strategy
* frontend timing
* human-review policy

Do not skip directly to full universalization or frontend development while the current reference systematic review and meta-analysis remains incomplete.

When proposing or implementing the next task, follow the authoritative development sequence in `PROJECT_ROADMAP.md`.

If a request conflicts with the roadmap, follow the user's latest explicit instruction and document the change in `PROJECT_ROADMAP.md`.


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
