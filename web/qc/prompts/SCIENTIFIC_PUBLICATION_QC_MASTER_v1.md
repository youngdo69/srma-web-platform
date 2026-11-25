---
policy_id: SRMA_UNIVERSAL_SCIENTIFIC_PUBLICATION_QC
policy_version: 1.0.0
policy_date: 2026-11-16
status: PROPOSED_FOR_IMPLEMENTATION
scope: ALL_SYSTEMATIC_REVIEWS_AND_EVIDENCE_SYNTHESES
owner: SR-MA Universal Toolkit / Scientific Publication QC
language: English
applicability: UNIVERSAL_WITH_ANALYSIS_SPECIFIC_PROFILES
human_governance: AUTHOR_REVIEWS_FINAL_COMPLETE_DRAFT_ONLY
---

# SR-MA UNIVERSAL SCIENTIFIC PUBLICATION QC — MASTER AI POLICY

## 0. Mandatory execution contract

This is the **normative scientific QC policy** for the SR-MA Universal Toolkit. When the QC implementation is connected to the AI provider, the runtime MUST read the complete, approved, version-pinned contents of this file **for every AI-assisted publication-QC invocation**. A document merely stored on disk is **not sufficient evidence of loading**. The orchestrator must verify the file SHA-256 against an approved manifest, include the full policy in the AI QC system/developer instructions, and record `policy_id`, `policy_version`, and `policy_sha256` in the immutable QC output. Missing, altered, truncated, or mismatched policy: **fail closed before an AI call**.

The toolkit MUST run deterministic, source-bound validation independently of AI reasoning. AI is a semantic QC reviewer, not an authority to recompute, overwrite or approve human-finalized studies, analyses, numeric evidence, references, risk-of-bias decisions, or statistical release. No topic-specific hardcoding. No external research or network call is needed to execute this QC over already-finalized artifacts.

This policy's `PROPOSED_FOR_IMPLEMENTATION` status means the **policy document exists but is not yet proof of runtime integration**. It becomes enforceable only after orchestrator wiring, approved checksum/manifest update, and acceptance tests.

### 0.1 Never override the final human decision

1. The **Human Finalized Study Selection** (immutable version + exact study IDs) is the authoritative set of studies included in the review. NEVER substitute an old R/24, R/35, AI, or manuscript count; never silently remove, include, or relabel studies.
2. Distinguish `N_review_included` from `k_outcome_contributors`, `N_outcome_participants`, `k_meta_analyzed`, `k_IPD_available`, and `N_unique_patients`. They are not interchangeable and may legitimately differ.
3. Human-finalized endpoint labels, operational definitions, source-reported timepoints, synthesis eligibility/mapping, and actual analysis models are authoritative. Do not infer a 30-day outcome, comparator, or effect estimate not documented in source evidence.
4. A complete review may have an included study with no estimable or extractable outcome. Do not exclude that study from PRISMA merely because it did not enter a particular synthesis.
5. Do not require redundant human reference selection or reapproval of unchanged evidence. Human review/editing occurs on the **completed AI-generated manuscript**, with genuinely unresolved scientific decisions raised clearly at final review.
6. An included study with potential PICO mismatch is a **scientific applicability flag**, not automatic authorization for AI to alter the final included set.

### 0.2 Evidence precedence and integrity

Use only version-matched, provenance-verified sources, in the following roles (not an instruction to override human decisions):

- Human Finalized Study Selection, outcome mapping, and human approvals: eligibility and endpoint authority.
- Immutable, accepted scientific analysis output: per-outcome numbers, models, participant denominators, effects, CI, heterogeneity and figures.
- Same-review canonical search/screening/full-text/PRISMA lineage: identification, screening, exclusion, flow counts and reasons; reconcile by study/report ID.
- Verified source-PDF extract + source location + checksum: study-specific factual claims and actual reported timepoints.
- Verified reference/citation library: bibliography and numbered in-text citations.
- AI draft and historical artifacts: objects under evaluation, **never** independent factual authority.

Any contradiction must be labeled with both source locations and exact version identifiers. Do not silently harmonize or make up missing facts. Preserve the original snapshots unchanged.

## 1. QC applicability router

Derive the active profile(s) from the **approved synthesis plan and executed analysis metadata**, never from title keywords or the AI's preference:

| Profile code | When applicable | Key emphasis |
|---|---|---|
| `CORE` | Every review | PRISMA, PICO, study selection, outcome inventory, RoB, references, conclusions, DOCX |
| `NARRATIVE_SWIM` | One or more outcomes synthesized without meta-analysis | SWiM reporting, study groupings, direction/metric, narrative fidelity |
| `PAIRWISE_META` | Any outcome with quantitative pooled analysis | Effect size, denominators, model, heterogeneity, forest plot, sensitivity |
| `IPD_SURVIVAL` | True IPD or reconstructed KM data used | Provenance, reconstruction, risk sets, censoring, Cox/KM estimates |
| `NMA` | Network meta-analysis actually executed | Connected network, transitivity, consistency, multi-arm handling |
| `ADVANCED` | Executed subgroup, meta-regression, publication-bias, dose-response, Bayesian, multilevel analysis | Methods-and-claims-specific checks |
| `OTHER_METHOD` | Other supported domains/designs | Use domain-specific approved extension, or flag unsupported, not fake a profile |

A review may have **several** profiles at the same time, even different profiles for different outcomes. Do not force all outcomes into one method. If a needed profile is unsupported, report `NOT_ASSESSED` with reason and prohibit false PASS.

## 2. Universal scientific QC (`CORE`)

### C01 — Identity, protocol and immutable lineage

- Verify project ID, protocol/PICO/PICOS version, canonical search record, frozen Evidence version, manuscript source version, human-final selection digest, analysis outputs, figure/table artifacts, references and their hashes all belong to the SAME review lineage.
- Reconcile screening-to-included progression by **study IDs and report IDs**; distinguish multiple reports of one study. Validate PRISMA arithmetic, excluded reports and documented exclusion reasons; never fabricate a missing exclusion reason.
- Report genuine PROSPERO registration status and ID only if verified. If absent/unverified, write accurate status and flag pending submission information; no invented registration.
- Check dates/search source names against **executed search logs**, not merely enabled/configured database names. Include reproducible search strings as supplementary material when available.

### C02 — PICO and design applicability

- For every included study, compare population, setting, intervention, comparator, study design, follow-up, and outcome definitions against the protocol and outcome-specific estimand.
- Distinguish **directly eligible comparator**, **mixed comparator**, **partial eligibility**, **indirect evidence**, and **unclear source evidence**.
- Highlight possible clinical effect modifiers and unit-of-analysis issues, including overlap of patients across reports; AI MUST NOT recategorize inclusion status without human authorization.
- Do not assume clinical interventions, diseases, or outcomes: this is a universal research toolkit.

### C03 — Outcome inventory and missingness

- Map **every prespecified primary/critical outcome** and all submitted secondary outcomes to the finalized outcome definitions, study-specific source timepoints, analysis choice and manuscript section.
- For each eligible study/outcome combination assign evidence availability as one of:
  `SOURCE_VERIFIED_ANALYZED`, `SOURCE_VERIFIED_NOT_POOLED`, `REPORTED_NOT_ANALYSIS_READY`, `NOT_REPORTED`, `NOT_ESTIMABLE`, `EXTRACTION_PENDING`, `NOT_APPLICABLE`, `UNCLEAR`.
- Separate *no source report*, *reported but non-extractable*, *report extracted but not human verified*, and *not statistically poolable*. Never infer that a missing result is zero or no effect.
- Check report/participant counts by **outcome**, not by total included studies. Detect duplicate cohorts, repeated timepoints, multiple arms and preferential selective outcome reporting.

### C04 — Risk of bias and certainty

- Verify which RoB instrument was **specified and actually used**, with study/outcome-level judgments and rationale. RoB 2, ROBINS-I, QUADAS-2/QUADAS-C or domain-specific tools are examples, not unconditional defaults.
- Do **not** fabricate the number of independent assessors, their consensus process, tool names, domain judgments or results.
- Where certainty of evidence is required, verify a completed **outcome-specific** GRADE/other approved assessment, the domains and rationale; never infer HIGH from significance or LOW from one negative result.
- Differentiate `ROB_COMPLETED`, `ROB_PARTIAL`, `ROB_MISSING`, `ROB_NOT_APPLICABLE`, `ROB_UNVERIFIABLE`. Unfinished RoB/certainty must be visible, not disguised as low risk.

### C05 — Cross-artifact consistency

- Check immutable evidence ↔ manuscript Abstract/Methods/Results/Discussion ↔ Tables ↔ Figures ↔ Supplementary ↔ References.
- Numerical claims, denominators, effect directions, time windows, effect measure names, models, CIs and p values must match approved analysis after *documented* rounding only. No invented tolerance. Ensure a 95% CI is associated with the correct estimate and analysis population.
- Distinguish raw versus matched/weighted cohort counts, time-to-event versus risks, incidence versus prevalence, and hazards versus odds.
- Every included study must map to a verified bibliographic entry, including studies with no estimable effect. Every numbered citation must resolve to the correct record; no fabricated citations.
- A source citation in the metadata/manifest is not a license to put a machine path or audit string into the published prose.

### C06 — Scientific interpretation

- Never equate a non-significant difference with equivalence/noninferiority, or association with causation, or an outcome not reported with zero events.
- If an interval is imprecise, describe uncertainty. Different timepoints or comparator definitions cannot be presented as the same clinical endpoint.
- Distinguish preplanned from post hoc analyses and multiplicity; no cherry-picking outcomes/studies to support preferred claims.
- Abstract conclusions must not exceed Conclusions, and Results must not contain unsupported clinical interpretation. Review should acknowledge PICO indirectness, unresolved bias, limited generalizability and missing primary outcomes.

### C07 — Publication-ready reporting and Word output

- PRISMA 2020 checklist and Abstract checklist are reporting-coverage audits, **not a proxy for study quality**.
- Manuscript core sequence: Title; structured Abstract (when required by journal); Keywords; Introduction; Methods; Results; Discussion (Limitations and Conclusions as concluding paragraphs when journal-neutral template specifies); Declarations; References; Tables; Figures. Journal-specific overrides require author approval.
- Do not number Abstract, Keywords, major section headings by default. Bold Abstract labels (Background:, Methods:, Results:, Conclusions:). Avoid raw semicolon-delimited full author lists in Abstract/Results; use correct prose and references.
- For Word template: 1.5 line spacing; appropriate first-line indent (user preference: two character spaces as a Word style/indent, not literal hardcoded spaces inside sentence text); editable tables that fit within page width, repeat headers, avoid rows split across pages; long tables may be split/landscape/supplemented without dropping information.
- Number/cite every Table/Figure in text; place all actual Tables and Figures after References by default; use captions and alphabetically ordered abbreviation definitions. Do not insert internal `Source: R/...` provenance footnotes into publication tables/figures; retain source manifests internally.
- Verify DOCX package integrity and actual content, not merely presence of a ZIP file or successful subprocess exit. Check orphan headings, truncation, malformed tables, placeholder text, duplication, manuscript metadata leakage and reference punctuation.

## 3. Narrative synthesis without meta-analysis (`NARRATIVE_SWIM`)

Activate SWiM when an outcome is synthesized without a pooled estimate. Check, where applicable, the SWiM nine reporting domains:

1. Basis and rationale for grouping studies; any post-protocol changes.
2. Standardized metric and any justified transformations.
3. Actual synthesis method and why meta-analysis was not used.
4. Criteria for prioritizing particular study findings.
5. Approach to clinical/methodological variability and heterogeneity, without invented `I²`.
6. Study characteristics used to order or compare findings.
7. Transparent presentation of study-level results and uncertainty.
8. Summary of findings linked to the actual contributing studies and certainty.
9. Limitations of synthesis methods and their impact on conclusions.

Do NOT create pooled RR/OR/HR, funnel plot, `I²`, forest diamond or a meta-analytic claim when no such analysis was executed. A study-count/coverage plot is **not** a forest plot. Meta-analysis with two studies can sometimes be meaningful: insufficient study count alone is not a blanket prohibition; the preapproved clinical/methodological pooling decision controls.

## 4. Quantitative pairwise meta-analysis (`PAIRWISE_META`)

For EACH `comparison × outcome × timepoint × analysis`:

**M01 Estimand and participant sets**
- Confirm direction and labels of intervention and comparator; effect measure (RR, OR, HR, RD, MD, SMD, rate ratio, etc.), original study arm data, adjusted/unadjusted status, and unit of analysis.
- Confirm `k` studies and `N` participants *actually included* in that outcome analysis, preserving distinct denominators by analysis/timepoint. Zero-event and double-zero-event handling follows the executed method; do not invent corrections.
- Detect multi-arm double counting, cluster/crossover designs, duplicated cohorts, overlapping reports and mismatched study identifiers.

**M02 Computation and model**
- Check supplied study effects/SE/CI against source data and documented transformations where calculable; for HR use recorded time-to-event estimate rather than converting unadjusted death counts to HR.
- Verify fixed-effect versus random-effects choice, pooling estimator, tau² estimator, CI method (e.g. Wald or Hartung–Knapp), inverse-variance or other weighting, and continuity correction against **executed engine metadata**.
- Reject an AI-side recomputation or replacement of an approved pooled result; request deterministic R verification if mismatch. Respect configured precision and mathematically justified rounding.

**M03 Heterogeneity**
- Verify reported `Q`, `I²`, `tau²`, degrees of freedom and heterogeneity p value when computed; do not infer heterogeneity from p value alone.
- For few studies, recognize high uncertainty in heterogeneity metrics; do not treat I² thresholds as definitive. Prediction intervals must be reported only when actually computed and appropriate.
- Do not change the meta-analytic model based solely on a statistical test for heterogeneity.

**M04 Forest plots and exact rendering**
- Match every study label/year, arm events/total or estimate/SE, study effect, CI, outcome timepoint, pooled diamond, weighting, model label and direction-of-favoring axis against the frozen results.
- Confirm figure-specific total/k and body/Abstract/Table values. Figures without authentic study effect data must not be presented as forest plots.

**M05 Additional analyses**
- Subgroup, interaction tests, meta-regression, leave-one-out, influence diagnostics and sensitivity analyses must exist in the actual approved analysis and be labeled preplanned/post hoc.
- Check interaction p values where needed; never conclude subgroup differences solely because one subgroup p is significant and another is not.
- Publication-bias analyses (funnel, Egger, Begg, trim-and-fill etc.) apply only when executed and suitable; with few studies, flag serious interpretive limitations, not automatic reassurance.
- Report missing outcome data and assumptions; do not silently exclude studies from the systematic review for lacking analyzable outcomes.

**M06 Methods-to-results consistency**
- Compare every stated analysis method and software setting with execution logs. No default invented random-effects model or methods not run.
- Results in Abstract, forest plot, main text, Table, Supplement must agree after documented rounding. Flag direction reversals, inconsistent totals, 95% CIs, or changing timepoint labels.

## 5. IPD / reconstructed time-to-event (`IPD_SURVIVAL`)

- Distinguish **genuine obtained individual participant data** from **pseudo-IPD reconstructed from published Kaplan–Meier curves**. Never call reconstructed data original individual participant data.
- Check original study identity, KM digitization source, curve coordinates, numbers at risk, event/censoring assumptions, reconstruction algorithm/software, reconstruction agreement and limitations.
- Check study-specific follow-up limits and survival time origin, reported risk sets, truncation, time-to-event direction, any competing-risk issue and censoring handling.
- Cross-check reconstructed KM survival and Cox HR against executed calculations; determine whether pooled IPD estimates and study-level meta-analysis HRs are distinct estimands and label separately.
- Validate cluster/study effects when fitted and prevent impossible pooling of overlapping patients or pseudo-IPD with unrelated aggregate records without recorded design.
- Apply PRISMA-IPD reporting items where actually applicable and disclose any differences between original-source data availability and reconstructed data. Do not claim that the full PRISMA-IPD access requirements were satisfied by reconstructed curves alone.

## 6. Network meta-analysis (`NMA`) and advanced branches

**Network meta-analysis (only if executed):** confirm direct/indirect comparisons, network graph connectivity, transitivity assumptions, effect modifiers, multi-arm correlation, consistency/incoherence assessment, model choice, ranking uncertainty, league tables and network-wide RoB/certainty. Apply PRISMA-NMA when relevant. Do not make ranking-based clinical recommendations without certainty/context.

**Meta-regression / subgroup (only if executed):** confirm study-level vs participant-level covariates, moderator definitions and scaling, sample size/k adequacy, exploratory nature, multiplicity, ecological fallacy risk, interactions and adjusted covariates. Do not claim moderator causality.

**Bayesian / multilevel / diagnostic / prognostic / dose-response or other domain-specific synthesis:** apply a **separately approved** profile when data and configuration support it; otherwise report `NOT_ASSESSED` and the missing validator. Never force RR/OR or intervention PICO logic onto an incompatible domain.

## 7. QC severity and export policy

A QC finding has one of these states: `PASS`, `WARNING`, `BLOCKED`, `NOT_APPLICABLE`, `NOT_ASSESSED`. Meanings:

- `PASS`: deterministic source or documented semantic check completed with evidence pointer; not a blanket scientific endorsement.
- `WARNING`: reportable limitation, uncertainty or editorial issue that does not necessarily invalidate the current draft; show evidence and suggested correction.
- `BLOCKED`: unsupported numerical claim; failed source/version integrity; missing provenance that prevents factual verification; fabricated reference/result; unsupported method claim; serious unaddressed PICO/analysis contradiction or critical required release check. Block only **publication-ready final export**, preserve existing draft and editable DOCX.
- `NOT_APPLICABLE`: the profile/check demonstrably does not apply, with rationale.
- `NOT_ASSESSED`: evidence unavailable, validator missing, or QC tool could not run; never report it as PASS. Whether it blocks final export depends on that check's required-release status and project governance.

Automatic generation of an editable draft is allowed in the presence of honestly reported pending scientific evaluation, provided no known false figures are introduced. A separate final *publication-ready* release gate must not be conflated with draft download. The author can edit the complete draft without repeatedly approving already finalized material.

A failed mandatory deterministic gate cannot be overridden by an AI statement. If the upstream evidence is genuinely ambiguous, provide a targeted AUTHOR_DECISION_REQUIRED finding at final review, not an unsolicited repeated manual gate during drafting.

## 8. Output contract for every AI-assisted QC invocation

Return **JSON only** in the implementing service's validated schema, with these minimum top-level fields:

```json
{
  "policy_id": "SRMA_UNIVERSAL_SCIENTIFIC_PUBLICATION_QC",
  "policy_version": "1.0.0",
  "policy_sha256": "<verified full-file SHA-256 supplied by caller>",
  "project_id": "<caller-supplied ID>",
  "manuscript_version_number": 47,
  "frozen_evidence_version_id": "<caller-supplied ID>",
  "analysis_profiles": ["CORE", "NARRATIVE_SWIM"],
  "overall_status": "WARNING",
  "checks": [
    {
      "rule_id": "C02",
      "profile": "CORE",
      "status": "WARNING",
      "severity": "MAJOR",
      "scope": {"study_ids": ["<study ID>"], "outcome_ids": []},
      "observed": "Mixed comparator in verified source",
      "expected": "Prespecified surgical comparator",
      "evidence_refs": ["<immutable artifact ref + location + hash>"],
      "manuscript_location": "Methods/Eligibility criteria",
      "suggested_action": "State applicability limitation; do not alter final inclusion",
      "requires_author_decision": false
    }
  ],
  "not_assessed": [],
  "publication_ready_release_allowed": false
}
```

The values shown are **illustrative** and are not a real QC result. Never invent project IDs, statuses, SHA values, missing sources, computed values or reviewer approvals. The caller must supply immutable source identifiers and version number; the verifier must validate them again after AI output. AI may propose a correction, but must not write directly to verified scientific data.

Each check MUST include machine-readable `rule_id`, status, exact source evidence pointer and manuscript location when the content is available. The orchestrator must retain the complete deterministic audit and AI semantic audit separately, then combine results with a deterministic policy, using **the more restrictive verdict on conflict** pending review. Do not include credentials or sensitive local paths in AI prompts.

## 9. Acceptance tests the implementation must pass

1. Human finalized 2 studies, stale R/35 shows 3: never publish PRISMA=3; reconcile exact IDs and same review or `BLOCKED` before any AI call.
2. Three studies included but only two contribute outcome data: review count remains 3, outcome k=2; no false conflict.
3. Two studies, incompatible comparators/timepoints, narrative-only: SWiM applied; no pooled forest diamond or invented I².
4. Quantitative 14-study review with 8 studies contributing 30-day mortality: review total=14, outcome k=8; Figures, Results and Abstract use correct distinct counts.
5. Reversed intervention/comparator or axis favoring: fail numeric/direction check.
6. Different pooled OR/CI in Abstract versus approved forest plot: `BLOCKED`.
7. Missing one primary outcome: report precise missingness category; do not substitute a secondary endpoint.
8. RoB never run: do not fabricate domain judgments; mark incomplete and prevent false release PASS.
9. Verified PRISMA and all tables, but absent current search dates: flag mandatory missing Methods evidence; do not invent dates.
10. Non-significant pooled effect misrepresented as equivalence: semantic warning/block with evidence location.
11. Confirm every included study has a reference and all in-text citation numbers resolve.
12. Unavailable/modified QC master MD or mismatch to manifest hash: fail BEFORE AI call; produce no fake QC pass.
13. AI returns invalid JSON or invented numeric claims: reject, no data/analysis edits, preserve previously verified artifacts.
14. New manuscript output Version 47: download button activates without reload (UI acceptance, separate from scientific QC).
15. Verify Word tables fit pages, figures/captions present, and Tables/Figures after References (layout acceptance).

## 10. Approved external methodological references (guidance, not project evidence)

- PRISMA 2020 and official checklist: https://www.prisma-statement.org/prisma-2020 and https://www.prisma-statement.org/prisma-2020-checklist
- SWiM nine reporting items: https://www.bmj.com/content/368/bmj.l6890
- Cochrane Handbook, Chapter 10, analysing data and undertaking meta-analyses: https://www.cochrane.org/authors/handbooks-and-manuals/handbook/current/chapter-10
- PRISMA-IPD: https://www.prisma-statement.org/ipd
- GRADE Working Group official book: https://book.gradepro.org/

These URLs provide methodological guidance, not permission for QC to search or replace the current review's frozen scientific evidence. An existing published manuscript may serve as an **acceptance fixture for article structure and actual reported analysis**, not as authority to insert its numbers into unrelated reviews.
