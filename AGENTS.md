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

Apply the roadmap's software prerequisites to universalization, frontend
development, Software Acceptance, and Release Candidate approval. Manage the
reference review's scientific decisions independently.

When proposing or implementing the next task, follow the authoritative development sequence in `PROJECT_ROADMAP.md`.

If a request conflicts with the roadmap, follow the user's latest explicit instruction and document the change in `PROJECT_ROADMAP.md`.

## Universal Software and Project Scientific Validation

Apply this governing rule when determining the scope of every Toolkit development task.
Route incomplete Protocol, search evidence, Study Selection, Outcome, RoB,
GRADE, R/40 Scientific QC, and final Manuscript Approval through the affected
project's existing Web workflow and Human Decision processes. Change Toolkit
code only for an objectively reproduced shared software defect or a reusable
feature requirement explicitly approved by the PM.

Each new Toolkit development proposal must specify:

1. The reproducible universal defect or the need for a reusable feature.
2. Why the existing implementation cannot meet the requirement.
3. How the change applies across SR-MA projects.
4. The minimum code changes needed.
5. The targeted test using a project-neutral fixture.
6. The expected AI and API costs and the expected validation cost.

Base Software Acceptance and Release Candidate approval on software and
operational verification. Assess scientific validity and manuscript submission
readiness within each research project through its existing approvals. Preserve
the researcher's authority and record their decisions and provenance so the
Toolkit can generate verifiable research results and manuscripts.

Preserve approved Scientific R calculations, Canonical Data, Versions,
Provenance, and Human Decisions. Scope verification to the affected behavior.
Avoid unnecessary repeat tests, full pipeline reruns, and project-specific
hardcoding. Paid AI calls require explicit approval.

## General rules

- Never fabricate study data, references, DOI numbers, or extracted outcomes.
- Preserve a full audit trail for search, deduplication, screening,
  extraction, exclusion, and statistical analysis.
- Use PRISMA 2020 terminology.
- Distinguish verified data from inferred or estimated data.
- Do not overwrite source datasets.
- Write generated outputs to the outputs/ directory.
- Run validation tests before accepting a meta-analysis result.
- Treat Human Final Study Selection and finalized outcome definitions, labels, and reported timepoints as authoritative. Reconcile by study ID and evidence version, preserve conflicting records, and distinguish review inclusion from synthesis participation.
- Reuse canonical approvals and saved QC only with verified project, version, source, approval provenance, and available integrity metadata. Preserve original findings and approval history. A source link never regrades QC.
- Keep review logic protocol-driven and applicable to narrative and quantitative designs. Complete automatic reconciliation before asking for substantive scientific decisions or obligatory information. Group duplicate questions and keep technical provenance in operator diagnostics.
- Inspect Git status and the verified baseline before editing. Keep authorized work on the feature branch. Protect approved scientific code, policies, dependency locks, schemas, and private runtime data. Keep changes and offline validation scoped to the affected behavior, and report only observed test results. External writes, paid AI calls, and release actions require explicit authorization.

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


## Universal SR-MA Toolkit — File Unity Instruction

Before editing or generating any project code or configuration:

* Inspect the existing target file and all materially related upstream and downstream files.
* Ask the user to attach missing existing files instead of guessing their contents.
* Preserve unity across R scripts, utilities, YAML schemas, paths, filenames, outputs, and workflow stages.
* Prefer modifying the shared source of truth over adding local rescue logic.
* Do not duplicate normalization, path discovery, schema migration, or validation across scripts.
* State the expected impact of each change on related files.
* Confirm that artifact producers and consumers use the same contract.
* Avoid replacing stable existing structures unless a coordinated migration plan exists.
* Treat isolated fallback patches as temporary and unacceptable for the final toolkit architecture.
