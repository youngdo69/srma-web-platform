# B1 recovery checkpoint — 2026-09-27

Before implementation: current session directory contained no repository. The remaining earlier workspace has HEAD 3137017, no tracked/staged diff and 17 untracked Block A paths (including db directory). It was preserved unchanged. Saved B1 delivery faca63e contains later implementation and evidence. Approved A and B1 source snapshots were recovered into this separate branch based on surviving 3137017 history. Original final A commit 50f1e3b is absent, so the B1 incremental bundle cannot establish its prerequisite; recovered snapshot commits explicitly do not impersonate those original hashes. Original archives/bundle retained.

Scope: B0.1 PostgreSQL, B0.2 clean Web restore, B0.3 scientific runtime assessment, WEB-005 identity, WEB-006 shell, WEB-007 wizard, WEB-008 search/import; stop before WEB-009. PM mission read from Pasted text.txt.

Preserved: identity/session, immutable wizard, shell, safe database import, canonical preflight adapters, migrations 001–003, runtime gates, proposed scientific dependency diff, all existing tests/evidence. No canonical scientific file changes.

Completed according to recovered implementation and historical evidence: WEB-006/007 and manual import/preflight path. WEB-005 partial pending actual PostgreSQL execution. WEB-008 partial: search/retrieval private mappings exist but are disabled; no complete upstream concept/approval/job-output handoff. Full end-to-end not started successfully. Current runtime lacks the previously recorded clean R 4.3.3 library and R 4.6.1 scientific environment; historical PASS is not a current rerun.

First incomplete requirement: B0.1. Rechecked installed PostgreSQL 16.2: initdb refuses root, runuser cannot set groups, setpriv cannot set UID, UID mapping admits only root; no container runtime. Infrastructure blocker, not implementation PASS. Do not bypass identity checks.

Tests present: baseline, foundation, contract, state, run, b1, b1_import, b1_security, b1_ui, b1_preflight, b1_search_guard, clean_runtime, postgres, scientific_docx, independent-process vertical.
Tests to rerun: available local development-runtime structural/security/UI/worker suites; scientific and clean-restore validation separately if dependencies available. Logs saved under evidence/recovery, separate from historical b1 evidence.

Concrete defect found during audit: request dispatch forwards arbitrary non-identity named arguments, allowing max_bytes override on upload_database. Restrict each action to an explicit argument contract before dispatch. Preserve server-side upload policy and test rejection before file access.

Remaining blockers: PostgreSQL privilege environment; missing clean/scientific runtimes; credentialed scientific success path and complete upstream search integration. Do not enable unverified operations or alter scientific gates.
