# B1-001 — PM execution report

Date: 2026-09-27. Authority: **B1 Completion Mission v1.2**. Scope ends before WEB-009/B2. This is an implementation report, not PM acceptance or production approval.

## A. Recovery/source checkpoint

- Delivery baseline: `148cb68f9175b9c57403fd06e708e4e96e4697da` (verified recovered B1 source).
- Working branch: `web/b1-completion-001`.
- Final code/test checkpoint: `f52f55ef34b3c7e44467c25fe4de4fc7851b6dd3`. Its working tree was clean before this report/status update. The final documentation commit and final clean status are recorded in the delivery manifest.
- The original `repo` checkout and recovery archive were preserved. Its previously reverted files were not reset/reverted. A separate checkout was materialized from committed contents, then checked against the scientific manifest. All valid recovery identity, authorization, wizard, manual imports, gates, worker fencing, artifacts and tests were retained.
- The earlier original Block A commit was not reconstructed or impersonated; prior recovery provenance remains unchanged.

## B. PM dependency update

Dedicated commit `e3ce298` applies only the approved additive proposal. All **79 existing package records remain identical**, **27 are added**, **106 total**, with no removals and unchanged top-level lock metadata. Follow-up `4da521f` preserves original CRLF line endings. Registered engine baseline points at `4da521f`; the lock SHA and source manifest are explicit. Original package record values, not formatting/order, were compared programmatically.

`integrity.json` confirms **97 other scientific files unchanged**, with `renv.lock` the only approved scientific manifest change. Available development officer/flextable created a real editable Word table; this is **not** canonical R/41 or scientific manuscript acceptance. The current R is 4.3.3, not locked 4.6.1. Canonical renv synchronization, START_HERE environment-only, all 106 exact dependencies and canonical DOCX smoke are not executed here. The development R/41 dependency inventory lacks `openxlsx`.

## C. WEB-008 implementation

1. Existing wizard saves a canonical project seed/configuration as an immutable draft.
2. `initialize_project` invokes canonical initialization in an isolated worker; only generated configuration/terms/decisions/report are registered as initiating-version artifacts. Missing operator credential/runtime fails closed and retains work.
3. A manual alternative validates explicitly human-entered canonical concepts; it never derives synonyms, MeSH or Emtree from PICO. It creates an auditable draft version.
4. Human Review displays the proposed config, terms and unresolved canonical decisions. No recommended option is preselected. Every required decision, approval checkbox and rationale must be supplied explicitly.
5. Canonical decision validation, patching, verification and activation create a new immutable approved input version. Protocol gates remain pending until a separate explicit decision.
6. `generate_search` requires the exact activated version, its Human Gates, operator credential and verified scientific runtime. Real canonical generation produces registered strategies and quality provenance.
7. `retrieve_pubmed` pins a passing search job for that same version. Unchanged canonical R/16 still verifies exact query identity and scientific quality before EFetch.
8. Manual uploads explicitly select an input version, bring only that version's selected generated inputs into a new immutable snapshot and retain origin provenance. Each upload has new pending gates. Later metadata edits start a fresh draft and discard prior generated/raw results.
9. Canonical database preflight examines the selected snapshot and pinned same-version outputs. It never starts merge/deduplication/screening.

## D. Canonical integration

The private adapter invokes `R/00_run_project_initialization_agent.R` and `R/01_validate_project_initialization.R`; manual concepts reuse canonical concept normalization plus the existing config validator. Human review uses actual functions in `human_decision_patch_utils.R`, `approve_human_decisions.R`, `apply_human_decision_patches.R`, and `verify_human_decision_application.R`.

Activation evaluates the unchanged `R/02_activate_project_configuration.R` workflow, replacing **only its console confirmation function** with the authenticated explicit Web confirmation. Existing approval-required, unresolved-decision, copy/archive/rollback checks remain. R/11, R/16, R/12 and TASK018 database gate code are invoked unchanged. Scientific source hashes prove those files were not edited. Web code performs authorization, orchestration, state presentation and artifact/version binding.

## E. Human Gates

Explicit activation approves configuration gates only for the newly created version and the actual reviewer. Protocol gates are not fabricated. Parent-version approvals are unchanged; edits and upload versions do not inherit approvals. Canonical choices cannot be omitted or replaced with an unknown option. Decision history and new lineage/source rows are immutable. Automated artifacts remain `UNREVIEWED`. No final inclusion, PROSPERO completion, literature approval or downstream review was inferred from automated completion.

## F. Search-quality behavior

Canonical `PASS` maps to `COMPLETE`; `REVIEW_REQUIRED` maps to `USER_ACTION_REQUIRED`, remains visible verbatim, and prevents retrieval. Unexpected/failed quality produces a structured engine failure. Missing scientific dependencies remain a dependency failure, distinct from scientific failure. The status mapping is directly tested with labeled synthetic contract inputs; these are **not real QC PASS evidence**.

## G. PubMed path

**Successful real retrieval was not executed. Zero real PubMed records were retrieved in this session.** The implementation binds query SHA-256/MD5, approved ProjectVersion, originating search job, worker attempt, retrieval report and raw artifact checksum. Synthetic registry fixtures test binding, checksum and rejection across versions without claiming scientific retrieval. A newer search cannot silently reuse raw records from an older query. Canonical exact-query safeguards remain unchanged.

`web/tests/b1_operator_acceptance.R` provides a staged operator test: canonical initialization; pause for actual explicit review; activation; real generation/QC; bounded retrieval (1–100 matches for the acceptance test only, 180-second subprocess limit); checksum/provenance/one-attempt verification. It was syntax-checked, not executed with credentials.

## H. Manual DB workflow

Actual canonical registry/parser/preflight tests pass on development R. Embase-only inputs produce `USER_ACTION_REQUIRED` for missing CENTRAL. Both required manual inputs produce `COMPLETE`, two registered `UNREVIEWED` artifacts, verified downloads and one worker attempt. Filename/path escape, symlink, size, project and version checks pass. Separate synthetic job-output tests verify search/raw promotion through two upload versions and confirm that the source approved snapshot stays unchanged. No merge, deduplication or screening was invoked.

## I. External acceptance

| Gate | Current result / reason |
|---|---|
| PostgreSQL | `POSTGRESQL_EXTERNAL_ACCEPTANCE_PENDING`: root-only environment, no usable unprivileged server/container runtime; privilege bypass not attempted. |
| Private-provider / real PubMed path | `CREDENTIALED_ACCEPTANCE_PENDING`: credential absent, canonical scientific environment unavailable. |
| Canonical R/11 and R/16 negative-path execution | Not executed successfully: current development environment lacks `rentrez`, `dotenv`, `httr2`. Initial execution stopped on these dependencies; the maintained test exits 77 with an explicit dependency result. |
| Canonical R 4.6.1 + 106-package restore | Pending: current development R 4.3.3 is not the required runtime. No repeated restore attempts or substituted lock. |
| START_HERE environment-only / full R41 dependency acceptance | Not executed in an isolated canonical environment. Development inventory and limited editable DOCX smoke are reported separately. |
| Full manuscript scientific regression | Not executed; not implied by any dependency/API smoke. |

Exact operator commands are in `web/docs/B1_EXTERNAL_ACCEPTANCE.md`. PostgreSQL tests are preserved. B2 remains unauthorized regardless of implementation verdict.

## J. Tests

Evidence directory: `web/evidence/completion-001`; exact argv, exit codes and previous corrected failures are recorded in `tests.json`. Development runner: `/workspace/scratch/f22ae3d8c5eb/runtime/Rscript`, with `SRMA_RSCRIPT` set to the same executable. Operational backend: SQLite.

- PASS: `Rscript --vanilla web/tests/{baseline,foundation,contract,state,run,b1_import,b1_security,b1_ui,b1_request_policy,b1_review_ui}.R` (each filename executed separately). `run.R`: **27 groups, 0 failed**. Baseline: 98 registered hashes and 78 scientific R files parse.
- PASS: `Rscript --vanilla -e 'source("web/tests/b1.R");cat("PASS: B1 script reached end\n")'` — six B1 groups and explicit completion marker.
- PASS: `Rscript --vanilla web/tests/b1_pipeline.R` — actual manual concepts/decision patches/activation, version isolation, missing credential, synthetic secret rejection, immutability, real store checksums, labeled synthetic search/raw/quality handoff contracts.
- PASS: `Rscript --vanilla web/tests/b1_pipeline_inputs.R` — actual canonical parser/preflight through persistent worker/storage using an **explicit test-only development-runtime seam**; not a locked-runtime validation.
- PASS: `Rscript --vanilla web/tests/b1_pipeline_ui.R` — actual Shiny save/review/explicit approval/new-version/fail-closed request dispatch.
- PASS: `python3 web/tests/vertical.py` with the runner environment — real HTTP 200, 8 JS/CSS assets, app/submitter exit before worker execution, 2 competing worker processes, exactly 1 attempt and 2 checksum-verified authorized downloads.
- PASS: `Rscript --vanilla web/tests/b1_development_docx.R` — actual editable table with available officer 0.6.4/flextable 0.9.4, scoped to development API smoke.
- Environment inventory executed: `Rscript --vanilla web/tests/b1_environment_status.R`; **inventory success is not scientific acceptance**.
- NOT EXECUTED / dependency exit 77: `Rscript --vanilla web/tests/b1_pipeline_guard.R`.
- All Web R files, including external acceptance commands, parse. Python integrity comparison verifies 79 unchanged package records + 27 additions and only the approved scientific lock difference.

Failures diagnosed and fixed: empty Shiny selector construction; older test loaders missing the new pipeline module; outdated migration table expectation; YAML list/vector round-trip assertion; missing strategies in an expanded synthetic fixture. These are resolved in the final passing runs. Missing scientific dependencies remain an environment limitation, not a passed test. Logs retain the initial failed scientific attempt separately for transparency.

## K. Security / provenance

Requests still pass authenticated session/CSRF and strict field allowlists. Users cannot supply actor, runtime, script or operator paths. Provider keys come from operator environment only; no credential UI was added. Subprocess stdout/stderr are discarded; structured errors are redacted. Current operator secret values are rejected from request/version files and artifact publication. `.env` is never copied. Synthetic secret sentinels in tests are plainly labeled and are not real credentials.

Composite project/version foreign keys and immutable source rows prevent accidental cross-version handoff. Every overlaid artifact is looked up in its exact job/project/version and checksum-verified. Metadata edits discard stale queries/raw data. Delivery contains no job data, live secrets, `.env`, dependencies or full Git history.

## L. Git checkpoints / changed files

Local commits preserve each unit: additive dependency `e3ce298`, registered baseline `5e8eeb0`, lock line endings `4da521f`, canonical activation `e0b38fb`, pipeline/UI/security `85518d4`, UI/preflight correction `5ec9fe7`, regressions/operator path `0c5b422`, final handoff/integrity/API smoke `f52f55e`. No push, merge, tag, deployment or remote-resource changes.

Relevant non-evidence file changes relative to recovery baseline (`A` new; `M` modified):

| Action | File |
|---|---|
| M | `PROJECT_STATUS.md` |
| M | `renv.lock` |
| M | `web/R/adapter.R` |
| M | `web/R/core.R` |
| M | `web/R/database.R` |
| M | `web/R/identity.R` |
| M | `web/R/persistence.R` |
| A | `web/R/pipeline.R` |
| M | `web/R/service.R` |
| M | `web/R/wizard.R` |
| M | `web/R/worker.R` |
| M | `web/adapter/engine_entry.R` |
| A | `web/adapter/initialization.R` |
| M | `web/adapter/search.R` |
| A | `web/db/004_pipeline.sql` |
| A | `web/db/005_pipeline_immutability.postgres.sql` |
| A | `web/db/005_pipeline_immutability.sqlite.sql` |
| A | `web/docs/B1_COMPLETION_CHECKPOINT.md` |
| A | `web/docs/B1_EXTERNAL_ACCEPTANCE.md` |
| M | `web/docs/engine-baseline.json` |
| M | `web/docs/engine-source-manifest.json` |
| M | `web/load.R` |
| A | `web/tests/b1_development_docx.R` |
| A | `web/tests/b1_environment_status.R` |
| A | `web/tests/b1_operator_acceptance.R` |
| A | `web/tests/b1_pipeline.R` |
| A | `web/tests/b1_pipeline_guard.R` |
| A | `web/tests/b1_pipeline_inputs.R` |
| A | `web/tests/b1_pipeline_ui.R` |
| M | `web/tests/contract.R` |
| M | `web/tests/run.R` |
| M | `web/tests/state.R` |
| M | `web/ui/app.R` |
| A | `web/docs/B1_COMPLETION_REPORT.md` |

New evidence files are all under `web/evidence/completion-001`; the package manifest lists every individual added/modified file and before/after SHA-256. Existing historical evidence is preserved. No source files are deleted. `PROJECT_STATUS.md` is updated with the current completion/external-acceptance distinction. Final Git state and exact final commit are included in the delivery metadata.

## M. Remaining implementation defects

None known after the above local verification. External scientific-runtime, PostgreSQL and credentialed behavior remain unverified and may expose issues during acceptance; they are not reported as PASS. The implementation verdict does not authorize production or B2.

## N. Verdict

**B1 IMPLEMENTATION COMPLETE**

PM acceptance remains pending. Stop before WEB-009/B2.
