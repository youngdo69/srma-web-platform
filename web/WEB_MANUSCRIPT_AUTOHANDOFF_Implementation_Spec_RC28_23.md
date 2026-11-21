# WEB-MANUSCRIPT-AUTOHANDOFF — Implementation specification (RC28.23 proposal)

**Status:** DESIGN REVIEW COMPLETE / IMPLEMENTATION NOT APPLIED / SCIENTIFIC ACCEPTANCE NOT CLAIMED  
**Scope:** SR-MA Universal Toolkit Web Platform; universal across disease, intervention, study design and outcome.  
**Sources reviewed:** user-provided `web/R/analysis.R` excerpt, `web/R/manuscript.R`, `web/ui/manuscript.R`, `web/adapter/manuscript.R`, `web/R/manuscript_render.R`, and narrative review/finalization modules.  
**Constraint:** Frozen scientific R engine, original `renv.lock`, registered PDFs, historical Evidence Versions and Human approvals are immutable. Existing 14-file Web overlay must not be silently expanded or rehashed without an explicit approved change package.

## 1. Confirmed code-level blockers

1. `web/R/manuscript.R::web_c2_catalog` calls `web_c1_evidence` (line 80). `web/R/analysis.R::web_c1_evidence` requires `c$row$stage == 'READY'`, a completed `finalize_evidence` job, and approved `b2_rob` Human Gate. The verified narrative-only route instead stores `narrative_source_finalization$status='FINALIZED'` in the Evidence packet, with scope `NARRATIVE_ANALYSIS_LOCKED; NOT_CANONICAL_R26`, and does not promote its Evidence stage to `READY`.
2. `web/R/manuscript.R::web_c2_catalog` enumerates **approved canonical** `analysis_versions` and their registered analysis artifacts (lines 81–84). The narrative route does not produce equivalent pooled-effect analysis versions or forest plots.
3. `web_c2_plan` (lines 86–118) requires nonempty analysis IDs, PubMed IDs and artifacts; requires a primary analysis and a canonical result table per included analysis; it cannot create a valid narrative-only plan even if the initial stage check were bypassed.
4. `web/R/manuscript_render.R::web_c2_system_blocks` (lines 33–41) unconditionally calls canonical pooled-result sentence generation; `web/adapter/manuscript.R::web_c2_canonical` (lines 27–30) unconditionally constructs pooled summary tables and selects a table with `[[1]]`. Those assumptions fail when no pooled effects / tables exist.
5. `web/R/manuscript_render.R::web_c2_facts` (lines 9–21) imports numeric-looking values from broad evidence and output tables without an explicit check of the **narrative PDF human review decision** for each result. This must NOT be used to automatically assert verified numerical narrative claims.
6. `web/ui/manuscript.R` (lines 3–6, 21–24) requires manual load and empty plan inputs. An automatic handoff and default selections are absent.
7. The approved 14-file Web overlay shown in `web/R/service.R` allows only named Web modules; `web/R/manuscript.R`, `web/R/manuscript_render.R`, `web/ui/manuscript.R`, `web/adapter/manuscript.R` are not in that approved list. A reviewed, additive Web source approval mechanism is needed before deployment. Never modify a frozen scientific source or trust a self-edited hash alone.

## 2. New route-aware source contract

Introduce a **versioned, immutable source descriptor**, with `route = 'NARRATIVE' | 'QUANTITATIVE' | 'MIXED'`, selected only from approved evidence/protocol metadata (not by hardcoded outcome strings). Do not represent narrative completion as canonical `READY` or fake an R/26 approval.

For NARRATIVE handoff, preflight MUST verify:

- Exact requested saved Evidence Version and manifest hashes; no mutable live files.
- `final_study_selection$status == 'HUMAN_CONFIRMED'` and `synthesis_plan$status == 'HUMAN_CONFIRMED'`.
- `synthesis_execution$status == 'AUTO_PREPARED'`; saved `source_digest` equals fresh recomputation.
- `narrative_batch_verification$status == 'HUMAN_VERIFIED_EDITABLE'`, bound to the same source digest.
- `narrative_source_finalization$status == 'FINALIZED'`, bound to the same source digest and exact version; its recorded scope must remain NARRATIVE, not canonical R/26.
- All selected result IDs are unique and exactly match the final synthesis set; each has a matching human review record, not just an AI proposal.
- For each result: `result_id`, `study_id`, `screening_id`, verbatim outcome, human-approved common outcome, original timepoint, analysis timepoint, PDF source location, evidence excerpt, PDF SHA-256, `source_digest`, Human decision, numeric fields. Confirm report registration/SHA against the exact saved PDF. No inferred 30-day timepoints or numeric conversion of absent values.
- `NOT_REPORTED` is a documented status with NO numeric fact token; uncertain or unverified results are not rendered as verified numeric facts.
- `unresolved_rob` is preserved as an explicit blocker for *final publication approval*, not silently changed. Working drafts may be produced with visible pending flags.

Save a new manuscript source package with source digest, frozen input manifest, selected analysis route, study/outcome mapping and a reproducible source fact registry. Historical versions/approvals remain unchanged.

## 3. Minimum-input, route-aware Manuscript Builder

- On entry to the Manuscript Builder, retrieve the verified source for the **selected** Evidence Version and automatically recognize NARRATIVE/QUANTITATIVE/MIXED route; do not require a manual `Load sources` action for the routine case. Show provenance and version identity to the user.
- Auto-fill title, preferred language (editable), selected approved analyses or narrative synthesis sets, available table/figure assets, prior verified PubMed reference metadata, and a draft rationale based on the saved protocol and analysis decisions. Never invent references/PMIDs. If a verified reference registry does not exist, expose only this missing input, not all fields.
- Avoid asking to verify the same unchanged numeric results again. Show Human decisions that remain legitimately outstanding (e.g., absent RoB judgments, contradictory source values, author editorial choices, final review).
- Table/figure selection is optional for a narrative-only route with no real registered artifacts. Do not synthesize forest plots or canonical pooled statistics to satisfy the UI.
- Keep an explicit “verified source facts only” token system for individual narrative studies/results. Generated drafts must use these tokens for numerical assertions and verified PMID tokens for citations.

## 4. Preserve Canonical path; extend rendering safely

- **QUANTITATIVE:** call current `web_c1_evidence` and C1/C2 requirements unchanged; retain canonical method-summary tables/forest plots and current source checks.
- **NARRATIVE:** use a separate `web_c2_narrative_catalog` / source assembler (or equivalent route-aware branch) with its own typed, source-bound fact payload; `web_c2_system_blocks` must generate result-by-study statements, no pooled effect sentences; DOCX writer must omit canonical pooled summary tables when no approved quantitative analyses are present. Reuse stable section drafting, reference verification/citation expansion, R/41 paragraph/table styling, `officer`, `flextable` and docx output where semantics remain valid.
- **MIXED:** preserve canonical quantitative summaries only for approved C1 datasets while adding verified narrative sets separately. No numeric cloning and no silent harmonization.
- `web_c2_check`: route-aware provenance integrity, strict numeric token checks, existing verified reference checks and final author review. Do not weaken citation-policy minimums without explicit protocol/governance approval.
- `web_c2_canonical` worker: carry route-aware source metadata and provenance into result JSON and DOCX artifacts. No AI request during load/preflight; AI generation only on explicitly requested draft action.

## 5. Implementation order (minimize compute and code risk)

1. Freeze and verify the current 14-file Web approval manifest, snapshot selected version ID, and backup only affected Web files.
2. Add a read-only narrative handoff validator/assembler with **unit fixtures** (success, stale digest, mismatched PDF SHA, missing Human decision, NOT_REPORTED, unresolved RoB). Test without OpenAI or subprocess.
3. Add route-aware manuscript catalog and plan generation while keeping C1/C2 canonical path untouched. Introduce necessary API/dispatcher wiring.
4. Add route-aware fact expansion, rendering and DOCX conditional paths. Verify no raw numeric claims from unapproved data; no fake pooled estimates.
5. Add auto-prefill UI, source identity display, and exception-only Human Gate prompts.
6. Perform one targeted offline end-to-end test using a verified *temporary clone* of the specific narrative Evidence Version. No provider calls or writes to historical version. Verify study-by-study results, source footnotes, reference tokens, editable DOCX, stale source protection, and unchanged quantitative path.
7. Authorize each Web-only path in a **reviewed** development overlay upgrade with explicit backup/change log. Do not alter frozen scientific manifest or canonical R.

## 6. Acceptance checks

| ID | Required behavior |
| --- | --- |
| H01 | Selected finalized Narrative Evidence auto-loads in Manuscript without `READY` gate spoofing. |
| H02 | Every displayed narrative numeric token resolves to a current Human-verified result and exact study/PDF SHA/source location. |
| H03 | Six selected outcomes stay mapped to their original study-specific results and timepoints. |
| H04 | `NOT_REPORTED`, missing values, unresolved RoB and conflicting evidence remain flagged; no numeric fabrication. |
| H05 | Manuscript inputs prepopulate from approved sources; only missing reference metadata and essential Human judgments require entry. |
| H06 | Narrative-only DOCX can be generated without a forest plot or pooled-effect table, with visible draft/not-final status. |
| H07 | Strict canonical quantitative READY/ROB/finalization checks remain unchanged. |
| H08 | Source revision invalidates stale downstream plan without modifying historical approvals or changing saved evidence. |
| H09 | Web-only hashes and approved source list PASS; no change to scientific R, renv, archived PDFs, SQLite historical rows. |
| H10 | Unit and one offline integration test pass; no OpenAI calls in verification tests. |

**Current verdict:** design and code-path audit complete; no patches applied, no runtime acceptance. The implementation requires approved Web-only extension scope, and should not be approximated by simply allowing `EXTRACTION` into `web_c1_evidence`.
