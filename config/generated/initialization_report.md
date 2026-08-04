# Project Initialization Report

- Generated: 2026-08-04 23:54:54
- Model: gpt-5-2025-08-07
- Prompt version: 1.0.0

## Summary

Initialized a PICOS-framed systematic review and meta-analysis on SGLT2 inhibitor exposure in adult cardiac surgery, with conservative eligibility (open surgical procedures; comparative designs; peer-reviewed full texts), comprehensive search databases, and concept-based, outcome-agnostic search terms. Material scope decisions regarding transcatheter procedures, heart transplantation, VAD implantation, and conference abstracts are flagged for human confirmation with deterministic patches.

## Assumptions

- Adults (≥18 years) only; pediatric studies are excluded.
- Cardiac surgery refers to open surgical procedures (CABG, valve, aortic) by default.
- Preoperative/perioperative exposure window includes pre-admission or chronic outpatient use carried into the preoperative period and exposure during the index hospitalization up to surgery and immediate postoperative period.
- Comparative study designs are required (RCTs and comparative cohorts); non-comparative studies and case series are excluded without separate decision.
- Language has no restrictions; studies in any language are eligible if otherwise includable.
- Peer-reviewed full texts included; preprints excluded by default due to uncertain peer-review status.
- Conference abstracts are excluded by default but surfaced for decision.
- Dual SGLT1/2 inhibitors (e.g., sotagliflozin) are not considered SGLT2 inhibitors and would be excluded unless SGLT2-specific effects are separable.
- Outcome terms are not included in the search strategy to preserve sensitivity; outcomes will be applied at screening and extraction.

## Warnings

- Candidate controlled-vocabulary terms require librarian verification and possible refinement before finalizing database-specific strategies.
- The term mapping for aortic procedures may need adjustment (e.g., specific MeSH/Emtree headings for aortic root or arch surgery).
- Exposure timing (preoperative vs perioperative) is often poorly indexed and may rely on full-text assessment during screening.
- If scope decisions (transcatheter, transplant, VAD, abstracts) change, apply the provided patches before generating search strategies and screening workbooks.

## Human decisions required

### D01_transcatheter_scope

**Question:** Should transcatheter-only cardiac procedures (e.g., TAVR/TAVI, MitraClip, PCI) be included as eligible populations?

**Why it matters:** Including transcatheter procedures broadens the population beyond open cardiac surgery and may change risk profiles and perioperative management, affecting applicability and heterogeneity.

**Recommended option:** exclude_transcatheter_only

**Recommendation rationale:** The review is targeted to open surgical cardiac procedures; transcatheter interventions are procedurally and perioperatively distinct.

**Suggested options:**
- exclude_transcatheter_only
- include_transcatheter_only

**Affected components:**
- project_config.eligibility
- project_config.screening

### D02_heart_transplant_scope

**Question:** Should heart transplantation procedures be included as eligible cardiac surgery?

**Why it matters:** Heart transplantation patients have unique perioperative and immunosuppressive considerations that may affect safety and efficacy outcomes.

**Recommended option:** exclude_heart_transplant

**Recommendation rationale:** Maintains a more homogeneous surgical population aligned with typical cardiac surgery cohorts (CABG/valve/aortic).

**Suggested options:**
- exclude_heart_transplant
- include_heart_transplant

**Affected components:**
- project_config.eligibility

### D03_vad_scope

**Question:** Should ventricular assist device (VAD) implantation procedures be included as eligible cardiac surgery?

**Why it matters:** VAD implantation has distinct perioperative risks and patient characteristics that may alter intervention effects and safety outcomes.

**Recommended option:** exclude_vad

**Recommendation rationale:** Preserves homogeneity with conventional open cardiac surgeries (CABG/valve/aortic).

**Suggested options:**
- exclude_vad
- include_vad

**Affected components:**
- project_config.eligibility

### D04_conference_abstracts

**Question:** Should conference abstracts without full-text articles be included?

**Why it matters:** Including abstracts can increase recall but may introduce incomplete data and risk of bias; exclusion may omit recent evidence.

**Recommended option:** exclude_conference_abstracts

**Recommendation rationale:** Conservative default prioritizes data completeness and quality.

**Suggested options:**
- exclude_conference_abstracts
- include_conference_abstracts

**Affected components:**
- project_config.eligibility.publication_status
- project_config.screening


## Draft files

- `config/generated/project_config_draft.yml`
- `config/generated/search_terms_draft.yml`
- `config/generated/initialization_decisions_required.yml`

> These are drafts. Do not activate them before human review.
