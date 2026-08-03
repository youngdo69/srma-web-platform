# Project Initialization Report

- Generated: 2026-08-03 19:29:30
- Model: gpt-5-2025-08-07
- Prompt version: 1.0.0

## Summary

Initialized a conservative, reviewable protocol for assessing the effects of pre/perioperative SGLT2 inhibitor exposure in adult patients undergoing cardiac surgery. Search concepts target cardiac surgery populations and SGLT2 inhibitors without outcome or design filters to maximize sensitivity. Key unresolved scope issues are surfaced as explicit human decisions with deterministic patches to eligibility, screening rules, exclusion reasons, and search terms where applicable.

## Assumptions

- Adult is defined as age ≥18 years.
- Comparative designs (RCTs and comparative cohorts) are the primary focus.
- Language and date are unrestricted unless decided otherwise later.
- Perioperative exposure includes preoperative and immediate postoperative periods as defined by study authors.

## Warnings

- Do not apply outcome or study design filters during searching to avoid missing relevant studies.
- Controlled vocabulary candidates require librarian verification prior to execution.
- Unresolved decisions (transcatheter procedures, heart transplantation, VAD implantation, conference abstracts, non-comparative designs, sotagliflozin scope) must be finalized before full-text screening and data extraction.

## Human decisions required

### HD1_transcatheter_procedures

**Question:** Should studies of transcatheter cardiac procedures (e.g., TAVR/TAVI, TMVR, MitraClip) be eligible?

**Why it matters:** Including transcatheter procedures may introduce clinical heterogeneity distinct from open cardiac surgery and affect applicability and effect estimates.

**Recommended option:** Exclude transcatheter cardiac procedures.

**Recommendation rationale:** The topic focuses on surgical (open) procedures; transcatheter interventions differ in perioperative course and risks. Excluding them preserves clinical homogeneity.

**Suggested options:**
- Exclude transcatheter cardiac procedures.
- Include transcatheter cardiac procedures.
- Include only when mixed with open cardiac surgery and data for open surgery are separable.

**Affected components:**
- project_config.eligibility
- project_config.screening.decision_rules
- project_config.screening.exclusion_reasons

### HD2_heart_transplantation

**Question:** Should studies of heart transplantation be eligible?

**Why it matters:** Heart transplantation patients have distinct perioperative management and risks that may not generalize to typical cardiac surgical cohorts.

**Recommended option:** Exclude heart transplantation.

**Recommendation rationale:** Transplant surgery differs markedly in patient selection, immunosuppression, and outcomes; excluding improves homogeneity.

**Suggested options:**
- Exclude heart transplantation.
- Include heart transplantation.
- Include only when mixed with other cardiac surgeries and data for non-transplant surgeries are separable.

**Affected components:**
- project_config.eligibility
- project_config.screening.decision_rules
- project_config.screening.exclusion_reasons

### HD3_vad_implantation

**Question:** Should studies of ventricular assist device (VAD) implantation (e.g., LVAD/RVAD) be eligible?

**Why it matters:** VAD implantation involves unique perioperative management and outcomes distinct from routine cardiac surgery, potentially biasing effect estimates.

**Recommended option:** Exclude VAD implantation.

**Recommendation rationale:** To maintain a clinically coherent population focused on conventional cardiac surgery.

**Suggested options:**
- Exclude VAD implantation.
- Include VAD implantation.
- Include only when mixed with other cardiac surgeries and data for non-VAD surgeries are separable.

**Affected components:**
- project_config.eligibility
- project_config.screening.decision_rules
- project_config.screening.exclusion_reasons

### HD4_conference_abstracts

**Question:** Should conference abstracts without full peer-reviewed articles be included?

**Why it matters:** Abstracts may lack methodological detail and full outcome data, affecting risk of bias assessment and synthesis.

**Recommended option:** Exclude conference abstracts without full articles.

**Recommendation rationale:** Excluding abstracts improves data completeness and reduces bias risk in effect estimates.

**Suggested options:**
- Exclude conference abstracts without full articles.
- Include conference abstracts if they report comparative data relevant to the review.
- Retain conference abstracts during screening but require full article for inclusion; exclude if no full text is available.

**Affected components:**
- project_config.eligibility
- project_config.screening.decision_rules
- project_config.screening.exclusion_reasons

### HD5_noncomparative_and_case_series

**Question:** Should non-comparative studies (single-arm, case series) be eligible?

**Why it matters:** Including non-comparative designs may bias effect estimates but could provide safety signals; the choice affects synthesis and risk of bias.

**Recommended option:** Exclude non-comparative designs and case series.

**Recommendation rationale:** The review is focused on comparative effectiveness; excluding non-comparative designs maintains internal validity.

**Suggested options:**
- Exclude non-comparative designs and case series.
- Include non-comparative designs for safety outcomes only (no effect estimates vs comparator).
- Include non-comparative designs if they provide pre-post within-subject comparisons relevant to outcomes.

**Affected components:**
- project_config.eligibility
- project_config.screening.decision_rules
- project_config.screening.exclusion_reasons

### HD6_sotagliflozin_scope

**Question:** Should the dual SGLT1/2 inhibitor sotagliflozin be considered within scope?

**Why it matters:** Sotagliflozin has dual SGLT1/2 activity and may differ mechanistically and in safety profile from pure SGLT2 inhibitors, affecting applicability and heterogeneity.

**Recommended option:** Exclude sotagliflozin (dual SGLT1/2 inhibitor).

**Recommendation rationale:** Maintains a mechanistically coherent exposure group focused on pure SGLT2 inhibitors; sotagliflozin can be considered later if justified.

**Suggested options:**
- Exclude sotagliflozin (dual SGLT1/2 inhibitor).
- Include sotagliflozin under the SGLT2 inhibitor umbrella.
- Include sotagliflozin only if data are reported separately from pure SGLT2 inhibitor agents.

**Affected components:**
- project_config.eligibility
- project_config.screening.decision_rules
- search_terms.search.concepts[C2_SGLT2I]


## Draft files

- `config/generated/project_config_draft.yml`
- `config/generated/search_terms_draft.yml`
- `config/generated/initialization_decisions_required.yml`

> These are drafts. Do not activate them before human review.
