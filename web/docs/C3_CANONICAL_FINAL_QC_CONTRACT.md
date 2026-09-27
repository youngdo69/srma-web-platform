# WEB-013 canonical Final QC contract

Audited baseline: C2 153b8fb; R40 v1.6.0 and R41 canonical source retained byte-for-byte.

R40 has side-effectful top-level orchestration, frozen R38/R39 file contracts, canonical numeric registry/rounding, section parsing, citation mapping, artifact coverage, internal-ID leakage, PubMed re-verification and provider semantic audit. Its CLI PASS requires deterministic AND semantic audits; C3 does not fabricate its state file or submission candidate.

C3 reuses actual R40 functions `extract_section`, `remove_references_and_after`, `extract_numeric_tokens`, `numeric_equivalent`, `extract_citation_numbers`, `normalize_doi`, `extract_artifact_markers`, `artifact_marker_context`; canonical machine-ID patterns and original citation/coverage audit expression blocks are evaluated in a bounded environment with explicit version-bound inputs. R38/R39/R41 formatting remains C2's source of truth. No pooled values or PRISMA totals are recalculated. C2's protected facts permit exact, stronger source association than R40's ambiguous free-prose numeric matching; R40 rounding still checks presentation.

Inputs: current APPROVED C2 ProjectVersion, READY evidence, approved analyses, source/reference/spec hashes, exact registered Word and selected artifacts. Web binds DB/Job/attempt/EngineRelease and verifies storage bytes; worker compares snapshots, canonical registry/CSV and actual DOCX. QC is append-only, bound to existing ProjectVersion; its revisions are QCVersion records, not new scientific ProjectVersions.

Outputs: registered final_qc.json, immutable QCVersion and Human dispositions, dedicated exact-source final_qc gate, machine-readable release eligibility. No DOCX regeneration, scientific repair, publication candidate or distribution bundle.

Machine checks: exact lineage/checksums, canonical numeric tokens and full approved content versus DOCX, canonical table cells, figure-result source, references, placeholders/leakage, completeness and structural study/report/RoB consistency. Missing canonical artifacts yield USER_ACTION_REQUIRED. All deterministic BLOCKER/MAJOR items require upstream correction and a new QC run; Human cannot waive them.

Human checks: claim/citation support, interpretation and Methods/protocol fidelity, descriptive/RoB statements, and actual figure/document readability. Explicit per-item disposition and final confirmation required. This is the PM-authorized Human semantic route, not a claim of R40 provider-semantic PASS. Provider absence never prevents deterministic QC.

External checks: exact runtime+lock with real statistical/manuscript reproduction and actual unprivileged PostgreSQL acceptance. The operator-only acceptance runner records immutable evidence tied to engine/lock; no browser field can assert acceptance. Historical development analyses/Word or fixture artifacts cannot acquire scientific release PASS merely because the server later changes runtime. Five carry-forward external gates are displayed. Runtime/PostgreSQL are mandatory for release; optional AI routes remain pending until separately accepted and are not inferred from deterministic checks.

R41 boundary: inspect the exact C2-approved DOCX, no regeneration. Any newer manuscript/analysis/evidence/reference or changed artifact/engine invalidates current eligibility. Historical decisions remain auditable.
