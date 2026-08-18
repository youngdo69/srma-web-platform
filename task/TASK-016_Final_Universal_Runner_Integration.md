# TASK-016 — Final Universal Runner Integration

**Repository:** Universal SR-MA Agent Toolkit  
**Canonical Runner:** `R/run_universal_srma_workflow.R`  
**Final Runner version:** **3.0.6**  
**Task status:** **CLOSED — APPROVED**  
**Next task:** TASK-017 — End-to-End Operational Verification & Release Readiness

---

## 1. Goal

Integrate all validated SR-MA modules into one canonical Universal Runner while preserving:

- topic-neutral reusable architecture;
- standalone executability of scientific modules;
- canonical config/path contracts;
- Human Review / Human Verification gates;
- optional-stage semantics;
- service-only modules;
- safe stop, resume, recovery, and audit logging;
- no project-specific hardcoding;
- no duplication of scientific logic inside the Runner.

---

## 2. Final Integration Principles

The Runner is an **orchestrator only**.

It may:

- invoke validated modules;
- evaluate stage-specific completion contracts;
- pause at Human Gates;
- recover completed stages from authoritative artifacts;
- maintain workflow state and run logs;
- skip disabled optional stages;
- invoke service modules only when required.

It must not:

- duplicate analytical logic;
- infer completion from unrelated project artifacts;
- bypass Human Gates;
- silently weaken standalone-module completion criteria;
- use repository-wide newest-file discovery for normal workflow completion;
- hardcode SGLT2i-, cardiac-surgery-, AKI-, or study-specific logic.

---

## 3. Final 34-Stage Runner Graph

1. `initialization_agent`
2. `initialization_validation`
3. `human_decision_review`
4. `human_decision_finalization`
5. `prospero_preparation`
6. `search_generation`
7. `search_validation`
8. `pubmed_download`
9. `database_input_gate`
10. `merge_deduplication`
11. `screening_workbook`
12. `agent_screening`
13. `agent_validation`
14. `human_reviewer_2_gate`
15. `human_adjudication_gate`
16. `screening_finalization`
17. `full_text_registry`
18. `full_text_agent_eligibility`
19. `full_text_author_gate`
20. `final_included_studies`
21. `prisma`
22. `multimodal_extraction`
23. `manuscript_tables`
24. `scope_advisory`
25. `targeted_verification_prep`
26. `extraction_human_gate`
27. `human_verified_finalization`
28. `meta_analysis`
29. `meta_analysis_diagnostics`
30. `additional_analysis`
31. `forest_plots`
32. `risk_of_bias`
33. `rob_author_gate`
34. `rob_finalization`

---

## 4. Module Classification

### RUN
Validated executable workflow stages, including initialization, search, merge/deduplication, screening, full-text processing, extraction, finalization, meta-analysis, diagnostics, plots, PRISMA, and RoB generation/finalization.

### HUMAN GATE
Explicit pauses remain authoritative at:

- Human Decision finalization
- Database input
- Human Reviewer 2
- Adjudication
- Full-text Author Final Review
- Extraction Human Verification
- RoB Author Review

### OPTIONAL
- `R/25A` Scope Advisory
- `R/29` Additional Analysis

### SERVICE
- `R/02` — invoked by initialization flow as required
- `R/32` — service-only for R/29 file-routing/resume support

### EXCLUDE
- `R/13–R/15` — development/manual live-interface checks
- `R/30` — portability/maintenance audit
- `R/31` — one-time config/path refactor

R/30–R/31 are not analytical Runner stages.

---

## 5. Important Final Integration Decisions

### 5.1 PROSPERO

PROSPERO preparation remains a **mandatory pre-search stage**.

Normal completion recovery is restricted to the active project's canonical evidence path:

`results/registration/prospero/`

Legacy PROSPERO artifacts were migrated once into this canonical location. Normal recovery does not use repository-wide `projects/*` discovery.

---

### 5.2 R/23 Full-Text Eligibility

The validated R/23 module was confirmed to implement the intended two-stage architecture:

1. Reviewer Agent eligibility assessment
2. 1st Author Final Review

The 1st Author decision is authoritative.

There is:

- no second independent human reviewer at this stage;
- no conflict/adjudication stage at full text;
- no creation of authoritative Included Studies inside R/23.

Canonical filename:

`R/23_run_multimodal_full_text_eligibility_2stage.R`

---

### 5.3 R/26 Human-Verified Finalization

Recovery validation uses the authoritative analytical contract:

- authoritative manifest exists and is readable;
- referenced datasets exist and are non-empty.

The audit workbook may be recognized as supporting evidence but is not required to force re-execution when the authoritative manifest and datasets are valid.

Human Verification logic itself was not weakened or bypassed.

---

### 5.4 R/27 → R/28 → optional R/29

Final dependency order:

`R/27 Meta-analysis`
→ `R/28 Diagnostics`
→ optional `R/29 Additional Analysis`

R/29 remains optional and resumable.

---

### 5.5 R/32 Service-Only

R/32 is not a sequential top-level Runner stage.

It is invoked only when an active R/29 workflow requires file routing/resume support.

---

## 6. Canonical Path Isolation Corrections

Reviewer-requested targeted corrections were applied without redesigning the 34-stage architecture.

Final recovery validation rules:

- active-project canonical paths only;
- no repository-wide newest-matching-file completion logic;
- no other `projects/*` artifact can complete the active project;
- Human Gate recovery must be supported by authoritative downstream artifacts from the same active project;
- R/17–R/37 validators were audited for path isolation;
- R/19 completion validation is fail-closed;
- missing validation dependencies cannot produce PASS.

Negative regression tests were added to verify that same-named artifacts in another dummy project cannot satisfy the active project's completion contract.

---

## 7. Runner Version Consistency

Final canonical Runner version:

**3.0.6**

Runner state and run log use the same version source.

The run log records:

`runner_version = runner_empty_state()$runner_version`

The final smoke test includes regression verification that:

`runner_empty_state()$runner_version == new_run_log$runner_version`

Result: **PASS**

---

## 8. Final Structural Smoke Test

Final result:

- **PASS checks: 23**
- **FAIL checks: 0**

Validated areas include:

- exact 34-stage graph;
- stage ordering;
- PROSPERO-before-search;
- R/28 between R/27 and optional R/29;
- R/32 service-only;
- R/30–R/31 exclusion;
- executable-stage file resolution;
- validator dispatch coverage;
- topic-neutrality;
- workflow state round-trip;
- Human Gate semantics;
- canonical-path isolation;
- R/19 fail-closed behavior;
- PROSPERO isolation;
- Runner version consistency between state and new run log.

---

## 9. Final Controlled Resume/Recovery Audit

Final result:

| Classification | Count |
|---|---:|
| ALREADY_COMPLETE | 14 |
| RECOVER_FROM_ARTIFACT | 19 |
| PAUSE_AT_HUMAN_GATE | 0 |
| WOULD_EXECUTE | 0 |
| SKIP_OPTION_DISABLED | 1 |
| **Total** | **34** |

Expected profile:

`14 / 19 / 0 / 0 / 1`

Result:

**MATCH = YES**

The audit confirmed:

- no analytical module was executed;
- no Human Gate was advanced;
- workflow state was not modified;
- no enabled stage remained unresolved;
- no completed analytical stage would be unnecessarily re-executed.

---

## 10. Final Git / Regression Verification

Final verification included:

- structural smoke test: PASS;
- controlled resume/recovery audit: PASS;
- `git diff --check`: PASS;
- canonical Runner replacement verified;
- R/23 canonical rename documented;
- no intentional standalone scientific-module redesign;
- no intentional topic-specific hardcoding introduced.

---

## 11. Acceptance Criteria

TASK-016 is accepted when all of the following are true:

- [x] Final integration map completed before code modification
- [x] 34-stage Runner architecture finalized
- [x] Human Gates preserved
- [x] PROSPERO remains mandatory pre-search
- [x] R/27 → R/28 → optional R/29 ordering correct
- [x] R/32 remains service-only
- [x] R/30–R/31 excluded
- [x] Active-project canonical path isolation enforced
- [x] R/19 validation fail-closed
- [x] Cross-project negative tests PASS
- [x] Runner/state/run-log version consistency PASS
- [x] Structural smoke test 23 PASS / 0 FAIL
- [x] Recovery audit 14 / 19 / 0 / 0 / 1
- [x] WOULD_EXECUTE = 0
- [x] unresolved Human Gate = 0
- [x] `git diff --check` PASS
- [x] Independent Reviewer approval obtained

---

## 12. Final Disposition

**TASK-016: APPROVED / CLOSED**

Canonical Runner baseline for downstream release-readiness work:

**`R/run_universal_srma_workflow.R` v3.0.6**

The next task is:

**TASK-017 — End-to-End Operational Verification & Release Readiness**
