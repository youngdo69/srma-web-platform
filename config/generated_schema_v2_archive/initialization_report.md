# Project Initialization Report

- Generated: 2026-08-02 22:28:03
- Model: gpt-5-2025-08-07
- Prompt version: 1.0.0

## Summary

Initialized a conservative PICO-based protocol for evaluating preoperative/perioperative SGLT2 inhibitor exposure in adult cardiac surgery patients. Search concepts target cardiac surgery and SGLT2 inhibitors with optional study-design filters. Ambiguities requiring human decisions were surfaced for transcatheter procedures, heart transplantation, VAD implantation, abstract-only evidence, non-comparative designs, case series/reports, and case-control designs. Recommended exclusions preserve a homogeneous surgical population and comparative evidence base.

## Assumptions

- Adult humans only; no language or date restrictions applied by default.
- Perioperative period includes preoperative and in-hospital perioperative exposure; exact window to be refined during full-text screening if needed.
- Comparator encompasses no SGLT2 inhibitor use, placebo, or standard care.

## Warnings

- Controlled vocabulary candidates require librarian verification.
- Do not apply outcomes as mandatory search terms to avoid missing relevant studies.
- Operational screening rules for transcatheter procedures, transplantation, VAD, abstracts, and study designs depend on pending human decisions.

## Human decisions required

### D1-transcatheter

**Question:** Should transcatheter cardiac procedures (e.g., TAVR/TAVI, MitraClip) be included as eligible procedures?

**Why it matters:** These procedures are not open surgeries and may have different perioperative courses and risks, affecting applicability and heterogeneity.

**Suggested options:**
- Include
- Exclude

### D2-transplant

**Question:** Should heart transplantation be included as an eligible cardiac surgery?

**Why it matters:** Transplant recipients have distinct perioperative and immunologic profiles that may alter SGLT2i risk-benefit and increase heterogeneity.

**Suggested options:**
- Include
- Exclude

### D3-vad

**Question:** Should ventricular assist device (VAD) implantation be included as an eligible procedure?

**Why it matters:** VAD implantation patients differ markedly in indications, physiology, and perioperative management, potentially altering SGLT2i effects.

**Suggested options:**
- Include
- Exclude

### D4-abstracts

**Question:** Should conference abstracts without a corresponding peer-reviewed full text be eligible?

**Why it matters:** Abstract-only reports often lack methodological detail and complete outcomes; inclusion can bias results and limit risk-of-bias assessment.

**Suggested options:**
- Include
- Exclude

### D5-noncomparative

**Question:** Should non-comparative observational studies (e.g., single-arm series) be eligible?

**Why it matters:** The review targets comparative effectiveness/safety; including non-comparative designs may bias effect estimates and hinder pooling.

**Suggested options:**
- Include
- Exclude

### D6-case_series_reports

**Question:** Should case series or case reports be eligible?

**Why it matters:** Case reports/series provide very low-level evidence and can bias safety signals without appropriate denominators or comparators.

**Suggested options:**
- Include
- Exclude

### D7-case_control

**Question:** Should comparative case-control studies be eligible alongside cohorts and RCTs?

**Why it matters:** Case-control designs differ in risk-of-bias (selection/recall) and effect measures, potentially complicating synthesis.

**Suggested options:**
- Include
- Exclude


## Draft files

- `config/generated/project_config_draft.yml`
- `config/generated/search_terms_draft.yml`
- `config/generated/initialization_decisions_required.yml`

> These are drafts. Do not activate them before human review.
