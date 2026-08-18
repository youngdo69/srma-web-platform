# TASK-017 — End-to-End Operational Verification & Release Readiness

**Project:** Universal SR-MA Agent Toolkit  
**Predecessor:** TASK-016 — Final Universal Runner Integration  
**Canonical Runner baseline:** v3.0.6  
**Goal:** Demonstrate that the integrated Universal Runner operates safely end-to-end and prepare the toolkit for release without weakening standalone-module, Human Gate, canonical-path, recovery, or universality contracts.

## Governance

TASK-017 is verification-first. Do not redesign the 34-stage architecture unless an operational test reveals a blocking defect.

No scientific module should be modified merely to make a release-readiness check pass. Any failure must first be classified as:

1. Runner/orchestration defect
2. canonical config/path defect
3. standalone module defect
4. environment/dependency defect
5. external-service dependency
6. expected Human Gate / optional-stage behavior
7. documentation/release-packaging defect

Independent Reviewer review remains required for any code correction that changes Runner/module behavior.

---

## Phase A — Completed-project operational resume verification

Purpose: prove that the actual canonical Runner can traverse the entire 34-stage workflow on the completed active project without executing scientific modules, reopening Human Gates, or entering a pause/failure state.

### Safety preflight

Before the operational run:

- source the canonical Runner with autorun disabled;
- validate all 34 stages using the same completion validators;
- abort before calling the Runner if any enabled stage is not currently recoverable;
- keep `run_additional_analysis = FALSE`.

### Expected actual Runner behavior

With all validators passing:

- 33 enabled/completed stages should be logged as `recovered`;
- 1 disabled optional stage should be logged as `skipped`;
- 0 scientific stages should be logged as newly `completed`;
- 0 stages should be `failed` or `paused`;
- final workflow state should be `completed`;
- final state should contain all 34 stages in `completed_steps`;
- new run log and state must both report Runner version 3.0.6.

### Evidence

Generate:

- `results/workflow/task017_operational_resume_verification.csv`
- `results/workflow/task017_operational_resume_verification.txt`
- normal new Runner run log under `results/workflow/runs/`

**Gate A:** PASS required before Phase B.

---

## Phase B — Clean-room portability / fresh-project verification

Purpose: demonstrate universality beyond the completed SGLT2i project.

Use a temporary or dedicated validation project with no dependency on another project's artifacts.

Verify:

- initialization and canonical config/path creation;
- project isolation;
- Human Gate stop/resume behavior;
- database-input gate behavior;
- standalone execution contracts;
- failure and resume behavior;
- optional-stage disabled/enabled semantics;
- R/29 resumable state and R/32 service-only routing;
- no cross-project artifact recovery;
- no topic-specific assumptions.

External API/database calls should be isolated into clearly identified live-service checks. Deterministic fixture-based checks should be used wherever the purpose is orchestration rather than scientific validity.

**Gate B:** PASS required before release candidate declaration.

---

## Phase C — Release-readiness audit

Audit the repository for:

- canonical config examples/templates;
- dependency/install documentation;
- `.env` / credential instructions without secrets;
- required R packages;
- module inventory and standalone entry points;
- Runner usage and resume/recovery instructions;
- Human Gate operator instructions;
- optional/service-stage documentation;
- PROSPERO canonical artifact contract;
- output-directory contract;
- no absolute machine-specific paths;
- no SGLT2i/cardiac-surgery-specific logic in universal core;
- no stale Runner-version strings;
- no obsolete runner variants in canonical locations;
- changelog/release notes;
- clean git diff and repository status.

**Gate C:** PASS required before tagging a release candidate.

---

## Phase D — Release candidate

Only after Gates A–C pass:

1. freeze code;
2. assign release-candidate version;
3. create release notes;
4. create reproducibility/install checklist;
5. run final smoke + clean-room + release audit;
6. obtain independent Reviewer approval;
7. tag the release.

Do not declare v1.0 solely because TASK-016 passed. Version/tag selection is a PM release decision after TASK-017 evidence is complete.
