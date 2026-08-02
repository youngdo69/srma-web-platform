# Project Initialization Report

- Generated: 2026-08-02 23:57:37
- Model: gpt-5-2025-08-07
- Prompt version: 1.0.0

## Summary

Initialized a conservative protocol for evaluating pre/perioperative SGLT2 inhibitor exposure in adult cardiac surgery with PICO framework, explicit eligibility, operational screening rules, and structured search concepts. Key unresolved scope issues are surfaced as discrete human decisions with deterministic patches.

## Assumptions

- Adult defined as age ≥18 years.
- Primary focus is open cardiac surgery (CABG, valve, open aortic) unless human decisions broaden scope.
- Comparative designs (RCTs and comparative cohorts) are the core evidence base for effect estimation.
- Outcome terms will not be used in the primary search to preserve sensitivity.

## Warnings

- Controlled vocabulary candidates and search strings require information specialist verification before execution.
- Decisions on transcatheter procedures, transplantation, VAD implantation, and abstracts can materially affect study yield and heterogeneity.
- Including non-comparative or case series designs would necessitate adaptations in data extraction and synthesis methods.

## Human decisions required

### D1_TRANSCATHETER_INCLUDED

**Question:** Should transcatheter cardiac procedures (e.g., TAVR/TAVI, TMVR, MitraClip, other percutaneous structural heart procedures) be considered eligible as “cardiac surgery”?

**Why it matters:** Including transcatheter procedures could introduce heterogeneous, non-surgical perioperative pathways and confound safety/efficacy estimates specific to open cardiac surgery.

**Recommended option:** Exclude

**Recommendation rationale:** Transcatheter procedures are not open surgical operations and typically have different perioperative risks and care pathways; excluding them preserves a homogeneous surgical population.

**Suggested options:**
- Include
- Exclude

**Affected components:**
- project_config.eligibility
- project_config.screening.decision_rules
- project_config.screening.exclusion_reasons

### D2_HEART_TRANSPLANT_INCLUDED

**Question:** Should heart transplantation be included as an eligible cardiac surgery?

**Why it matters:** Heart transplant recipients have unique perioperative and immunologic profiles that could alter safety/efficacy of SGLT2 inhibitors, potentially biasing pooled estimates for standard cardiac surgeries.

**Recommended option:** Exclude

**Recommendation rationale:** Heart transplantation substantially differs from routine cardiac surgery with distinct risks and pathways; excluding improves clinical homogeneity.

**Suggested options:**
- Include
- Exclude

**Affected components:**
- project_config.eligibility
- project_config.screening.decision_rules
- project_config.screening.exclusion_reasons

### D3_VAD_INCLUDED

**Question:** Should ventricular assist device (VAD) implantation (e.g., LVAD/RVAD/BiVAD) be included as an eligible cardiac surgery?

**Why it matters:** VAD implantation carries distinct perioperative management and outcomes compared with routine cardiac surgery, potentially impacting generalizability and effect estimates.

**Recommended option:** Exclude

**Recommendation rationale:** VAD implantation represents a specialized population with different risk profiles; excluding maintains focus on routine cardiac surgeries (CABG/valve/aortic).

**Suggested options:**
- Include
- Exclude

**Affected components:**
- project_config.eligibility
- project_config.screening.decision_rules
- project_config.screening.exclusion_reasons

### D4_CONFERENCE_ABSTRACTS

**Question:** Should conference abstracts without full-text articles be eligible?

**Why it matters:** Including abstracts-only may introduce incomplete data and higher risk of bias; excluding them may omit timely evidence. Decision affects study yield and data completeness.

**Recommended option:** Exclude

**Recommendation rationale:** Abstract-only records often lack sufficient methodological and outcome details for reliable synthesis; excluding prioritizes data quality.

**Suggested options:**
- Include
- Exclude

**Affected components:**
- project_config.eligibility
- project_config.screening.decision_rules
- project_config.screening.exclusion_reasons

### D5_NON_COMPARATIVE_STUDIES

**Question:** Should non-comparative study designs (e.g., single-arm cohorts, before–after without concurrent control) be eligible?

**Why it matters:** Including non-comparative designs can bias effect estimates for intervention efficacy/safety and complicate meta-analysis of comparative effects.

**Recommended option:** Exclude

**Recommendation rationale:** The review targets intervention effects; restricting to RCTs and comparative cohorts maintains internal validity and analytic consistency.

**Suggested options:**
- Include
- Exclude

**Affected components:**
- project_config.eligibility
- project_config.screening.decision_rules
- project_config.screening.exclusion_reasons

### D6_CASE_SERIES

**Question:** Should case series or case reports be eligible?

**Why it matters:** Case series/reports lack comparators and can overestimate effects; inclusion would affect bias, synthesis approach, and study selection volume.

**Recommended option:** Exclude

**Recommendation rationale:** To maintain focus on comparative effects with stronger internal validity, exclude case series/reports.

**Suggested options:**
- Include
- Exclude

**Affected components:**
- project_config.eligibility
- project_config.screening.decision_rules
- project_config.screening.exclusion_reasons


## Draft files

- `config/generated/project_config_draft.yml`
- `config/generated/search_terms_draft.yml`
- `config/generated/initialization_decisions_required.yml`

> These are drafts. Do not activate them before human review.
