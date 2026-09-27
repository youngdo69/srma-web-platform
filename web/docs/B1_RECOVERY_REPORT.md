# B1 Recovery & Resume — 2026-09-27

**B1 IMPLEMENTATION INCOMPLETE — PostgreSQL/runtime acceptance and WEB-008 upstream integration remain incomplete.**

## Recovery

The new session folder contained no repository. The surviving earlier workspace had HEAD `3137017`, no tracked/staged diff, and untracked Block A implementation files. It was preserved unchanged.

Recovered the saved B1 delivery (`faca63e`) and approved Block A source archive separately. Original final Block A object `50f1e3b` was unavailable; therefore the supplied incremental B1 bundle could not establish its prerequisite. Branch `web/block-b1-recovery` retains surviving history through `3137017` and explicitly records source-snapshot import commits. These are not the original missing commits. The delivery includes a self-contained recovery Git bundle and the original incremental B1 bundle for provenance.

Preserved: identity/session implementation, dashboard, canonical wizard, immutable versions/gates, uploads, canonical preflight, worker fencing, artifact registry, prior tests/evidence and scientific dependency proposals. All 98 frozen scientific source hashes match. Root scientific lock and canonical R/config files remain unchanged. No push, merge, tag, deployment or WEB-009+ work.

## Implementation

- Restricted request dispatch to named public arguments for each action. Callers cannot override upload `max_bytes`, request private fields, or inject unnamed/duplicate arguments.
- Connected version-scoped Human Review UI to the existing authenticated gate decision service. Users explicitly approve/reject a pending item with rationale/reference. CSRF, workspace authorization, immutable decisions and audit remain enforced. This records a human decision, not a fabricated registration or canonical search-quality approval. Search/retrieval remain disabled.
- Added request-policy and review-UI regression tests, including new-version approval isolation within the same project.
- Updated PROJECT_STATUS.md and recorded the recovery checkpoint.

Application files modified: `web/R/identity.R`, `web/R/service.R`, `web/ui/app.R`. The view adds gate IDs/operation labels to support selection; scientific contracts are unchanged.

Created: `web/tests/b1_request_policy.R`, `web/tests/b1_review_ui.R`, `web/docs/B1_RECOVERY_CHECKPOINT.md`, this report, and `web/evidence/recovery/`. See RECOVERY_DIFF.txt for the complete inventory.

## Requirement status

| Requirement | Current result |
|---|---|
| B0.1 PostgreSQL | BLOCKED: installed 16.2 rejects root; normal non-root execution unavailable; no Docker/Podman. No server test PASS. |
| B0.2 clean Web restore | NOT VERIFIED this session. Repaired missing R installation files in a separate core copy. Fresh-library restore reached dependency resolution but ended at the bounded 120-second limit (exit 124). Historical PASS is not a current rerun. |
| B0.3 scientific runtime | Historical UPDATE_READY proposal retained, not applied. Current R 4.6.1/runtime unavailable. No new scientific restore PASS. |
| WEB-005 | IMPLEMENTED — VALIDATION PENDING: local session/security checks pass; real PostgreSQL evidence required. |
| WEB-006 | Existing shell and added explicit review UI tested locally. No production authentication claim. |
| WEB-007 | Canonical wizard/edit/resume/version behavior reverified locally. |
| WEB-008 | PARTIAL: manual import/private canonical preflight work in development tests. Search concept preparation, generated-output/version handoff and credentialed retrieval integration remain incomplete. |
| B1 end-to-end | NOT COMPLETE. No new-project → approved generated search → successful PubMed retrieval → completed input preflight acceptance claim. |

## Verification

Tests use surviving development R 4.3.3 packages and SQLite, explicitly not clean-room or PostgreSQL evidence.

- PASS: baseline (98 source hashes, 78 R parses, actual validator), foundation, contract, state, and existing run.R (27 integrated groups).
- PASS: b1, b1_import, b1_security, b1_ui.
- PASS on final application source: b1_request_policy, b1_review_ui, b1, b1_security, b1_ui, b1_import. All six exit codes are 0; see final-test-summary.json.
- PASS: independent-process vertical test: actual Shiny HTTP 200, 8 JS/CSS resources, application exit before work, two workers, exactly one execution attempt, real canonical validator, two authorized checksum-verified downloads. This ran before the review-panel addition; the latter was verified with actual Shiny testServer handlers.
- PASS: final source hash comparison and git diff --check.

Not successfully executed: PostgreSQL suites, current clean restore, scientific R 4.6.1 restore, credentialed search QC/PubMed success path, production OIDC, browser-click automation. None is marked PASS. Configured provider credential presence was checked without exposing values; OPENAI_API_KEY was absent.

Classification: PostgreSQL is an infrastructure/identity limitation. Clean restore initially had missing Makeconf/ldpaths/repositories, then incomplete dependency resolution after separate repair. Scientific tests lack the required runtime and credential. Upstream search/version handoff is an implementation gap, not an environment excuse. The request-policy defect found in this audit is fixed and regression-tested.

## Remaining work and stop conditions

1. Run real PostgreSQL acceptance in an ordinary unprivileged disposable server: migration/checksum, foreign keys, immutability, transitions, active-job uniqueness, two-worker contention, reconnect and isolation.
2. Complete supported clean Web restore and isolated canonical R 4.6.1 restore. Review the preserved additive DOCX lock proposal separately; do not silently replace the frozen baseline.
3. Finish canonical search concepts and generated-output/version handoff. Human-review controls close only the review-recording UI gap.
4. Configure provider credentials securely on the operator side and verify actual canonical QC, PubMed retrieval/raw artifacts and full B1 end-to-end behavior. Keep unverified operations disabled.

The PM scientific-runtime/essential-private-credential stop conditions apply to substantive engine enablement. Remaining upstream integration is explicitly unfinished. B1 is not ready for B2 acceptance.

## Repository state

Local branch: web/block-b1-recovery. Final status is clean after the report checkpoint; RECOVERY_GIT_STATE.txt records exact HEAD/status. RECOVERY_DIFF.txt records changes against recovered B1 snapshot e4539bb. Original earlier workspace remains unchanged. PROJECT_STATUS.md was updated.
