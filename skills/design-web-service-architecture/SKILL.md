# Skill: Design Universal SR-MA Web-Service Architecture

## Purpose

Define the target architecture for converting the Universal SR-MA Agent Toolkit into a secure, scalable web service without discarding the existing R analysis engine.



## Mandatory File-Unity and Dependency Rule

Before modifying, replacing, or generating any project file:

1. Identify the file's upstream inputs, downstream consumers, shared utilities, configuration files, schemas, and path contracts.
2. Review the existing related files before proposing a change.
3. Ask the user to attach any required existing file that is not currently available.
4. Do not create an isolated rescue patch merely to suppress the current error.
5. Prefer correcting the common source of truth, shared utility, schema, or path contract over adding local fallback logic to one script.
6. Preserve existing file names, YAML structures, interfaces, and folder conventions unless a coordinated migration is explicitly planned.
7. State which files and workflow stages may be affected by a proposed change.
8. Verify that producers and consumers of each artifact use the same canonical path, filename, field names, and schema.
9. Avoid duplicating normalization, migration, validation, or path-building logic across multiple scripts.
10. When uncertainty remains, request the existing code rather than guessing its contents.

The user will provide requested existing files promptly. Therefore, file inspection should take precedence over assumptions.


## Anti-Rescue-Patch Rule

Do not solve workflow errors by repeatedly adding local search paths, schema guesses, fallback filenames, or duplicated normalization functions to downstream scripts.

Prefer:

```text
Inspect generator + path utility + config + consumers
→ define one canonical contract
→ update the common source
→ make downstream scripts consume that contract
```

## When to Use

Use this skill when planning the SR-MA web application, assigning responsibilities among the front-end, application back-end, R engine, LLM provider, storage, and job queue, or reviewing whether a proposed implementation leaks secrets or mixes UI and analysis logic.

## Target Architecture

```text
Browser
  ↓
Next.js / TypeScript front-end
  ↓ HTTPS / JSON
Application back-end
  ↓
Workflow orchestration and job queue
  ↓
R analysis workers + LLM APIs
  ↓
PostgreSQL and object storage
```

## Responsibilities

### Front-end

Prefer Next.js, React, and TypeScript. Handle project creation, PICO and eligibility input, database selection, Human Decision review, screening interfaces, progress displays, result dashboards, and artifact downloads. Never expose private LLM API keys in browser code.

### Application Back-end

Use FastAPI or Node.js when moving beyond a prototype. Handle authentication, authorization, project state, API validation, LLM calls, retries, rate limits, audit logging, job orchestration, storage access, and sensitive-data controls.

### R Analysis Engine

Preserve R as the scientific engine for search processing, imports, merging, deduplication, screening preparation, meta-analysis, risk of bias, PRISMA outputs, forest plots, and statistical reports. R functions should accept structured inputs and return structured results rather than depend on console input.

### Storage and Jobs

Prefer PostgreSQL for structured project state, S3-compatible object storage for files and reports, a queue such as Redis for long-running jobs, and append-only audit records for consequential workflow changes.

## Prototype Path

A Plumber API may be used initially:

```text
TypeScript front-end
  ↓
Plumber API
  ↓
R functions
```

Do not treat this prototype as the final production architecture once authentication, multi-user concurrency, recovery, and large-file handling become important.

## Design Rules

1. Never expose LLM API keys in browser code.
2. Keep scientific analysis in R unless migration is justified.
3. Keep workflow state outside interactive R sessions.
4. Use explicit JSON or YAML schemas between components.
5. Make consequential transitions auditable.
6. Separate project configuration, decisions, outputs, and active state.
7. Design APIs around projects and jobs, not direct `source()` calls.
8. Make retries idempotent whenever possible.
9. Preserve rollback or transactional behavior for activation changes.
10. Do not couple front-end screens to internal R file paths.

## Example API Shape

```text
POST /projects
POST /projects/{project_id}/initialize
GET  /projects/{project_id}/human-decisions
POST /projects/{project_id}/human-decisions/finalize
POST /projects/{project_id}/search
POST /projects/{project_id}/screening
POST /projects/{project_id}/meta-analysis
GET  /jobs/{job_id}
GET  /projects/{project_id}/artifacts
```

## Completion Criteria

The architecture is acceptable when front-end, orchestration, R analysis, LLM calls, storage, and job processing have clearly separated responsibilities; secrets remain server-side; long-running work can be tracked and recovered; Human Decisions remain explicit and auditable; and existing R scientific logic can be reused through stable interfaces.
