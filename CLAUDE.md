# CLAUDE.md

This is a systematic-review and meta-analysis automation project.

## Mandatory Startup Procedure

At the beginning of every coding session or project task:

1. Read `PROJECT_ROADMAP.md`.
2. Read `PROJECT_STATUS.md`.
3. Determine the current development phase and next valid task.
4. Do not begin implementation until both files have been reviewed.
5. Do not skip unfinished prerequisite steps unless explicitly instructed.

Use:

- `PROJECT_ROADMAP.md` for long-term direction and architecture.
- `PROJECT_STATUS.md` for current progress and immediate next actions.

## Before beginning any task:

1. Read README.md.
2. Identify the current SR-MA stage.
3. Inspect the existing audit trail.
4. Do not repeat completed steps without justification.
5. Never invent references or study values.
6. Preserve original datasets.
7. Record all decisions that affect study inclusion or analysis.
8. Use the relevant Skill whenever one is available.

For clinical or methodological decisions, distinguish:

- verified evidence,
- investigator decision,
- statistical assumption,
- unresolved uncertainty.

## Definition of Done

When a task changes project progress, completion requires updating
`PROJECT_STATUS.md`.

The update must accurately record:

- what was completed
- which files were created or modified
- whether validation passed
- the next valid task
- any blocker or unresolved issue
- the date of the update

Do not report completion without updating the status file.
Do not mark failed or unvalidated work as completed.

## Mandatory Project Context

Read `PROJECT_ROADMAP.md` before modifying this repository.

Treat it as the authoritative project plan and architectural policy.

The current strategy is:

1. Add only a minimal configuration layer now.
2. Complete the current SGLT2 inhibitor–cardiac surgery SR-MA through final manuscript.
3. Remove topic-specific hardcoding afterward.
4. Validate the harness using a second topic.
5. Build the Shiny frontend only after the configuration schema is stable.
6. Complete the one-command universal pipeline last.

Do not prematurely redesign the entire project as a universal framework unless explicitly instructed.


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
