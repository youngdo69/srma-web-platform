
## DEC-007 — Full-Text Eligibility Review Architecture

**Decision:** Full-text eligibility uses a two-stage review architecture:

1. Reviewer Agent
2. 1st Author Final Review

The Reviewer Agent performs the preliminary full-text eligibility assessment using available PDF full text or metadata when full text is unavailable.

The 1st Author reviews the Agent assessment and provides the authoritative final `Include` or `Exclude` decision.

No second independent human reviewer or separate adjudicator is required for the full-text eligibility stage.

**Rationale:** This architecture preserves Agent-assisted evidence review while maintaining direct human authority over the final study-selection decision and avoiding unnecessary reviewer/adjudication complexity at the full-text stage.

**Applies to:** TASK-011 and TASK-012.

**Authoritative final decision:** `author_final_decision` in `Author_Final_Review`.



## DEC-008 — Data Extraction Review Architecture

**Decision:** Data extraction uses a two-stage architecture:

1. Multimodal Agent Data Extraction
2. Human Verification

The extraction Agent reviews the available full-text study materials and produces a structured extraction dataset according to the predefined data-extraction schema.

A human reviewer then verifies the Agent-extracted values against the source study and corrects them when necessary.

The human-verified dataset is authoritative for downstream meta-analysis.

No second independent human extractor or separate third-reviewer adjudication stage is required for the data-extraction workflow.

**Rationale:** This architecture preserves multimodal Agent-assisted extraction while retaining direct human verification of all analysis-critical data. It reduces unnecessary duplicate extraction work while maintaining a clear audit trail between the Agent-extracted values and the final human-verified dataset.

**Applies to:** TASK-013 and TASK-014.

**Authoritative final data:** Human-verified extraction dataset produced after TASK-014.

**Workflow:**

`Included Studies → Multimodal Agent Extraction → Human Verification → Meta-analysis-ready Dataset`
