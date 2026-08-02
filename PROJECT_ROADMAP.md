
# Universal SR-MA Harness Project Roadmap

## 1. Project Objective

The objective of this project is to develop a reusable, reproducible, and user-friendly universal harness for conducting systematic reviews and meta-analyses from protocol development through final manuscript preparation.

The current systematic review on SGLT2 inhibitors in patients undergoing cardiac surgery will serve as the reference implementation.

The project must first complete one real systematic review and meta-analysis from beginning to end before full universalization and frontend development.

---

## 2. Core Development Strategy

This project follows a hybrid development strategy:

1. Complete the current SGLT2 inhibitor–cardiac surgery systematic review and meta-analysis as a working reference implementation.
2. Introduce only the minimum configuration layer needed to prevent further topic-specific hardcoding.
3. Expand the configuration schema gradually as real workflow requirements emerge.
4. Universalize the complete workflow only after the current review reaches final manuscript completion.
5. Validate the universal harness using a second, methodologically different review topic.
6. Develop the user-facing frontend only after the configuration schema and pipeline are stable.

The project must not attempt premature full universalization before the complete SR-MA workflow has been tested.

---

## 3. Authoritative Development Sequence

The required development order is:

### Phase 1 — Minimal Configuration Layer

1. Create `config/project_config.yml`.
2. Create shared configuration and path utilities.
3. Refactor only the necessary topic-specific elements in R/16–R/18.
4. Move file naming, review title, project ID, and exclusion reasons into the configuration file.
5. Do not yet redesign the entire R/11–R/18 workflow.

### Phase 2 — Complete the Current Reference Review

6. Complete title and abstract screening.
7. Complete conflict resolution.
8. Complete full-text screening.
9. Generate final eligibility decisions and exclusion reasons.
10. Generate PRISMA counts and the PRISMA 2020 flow diagram.
11. Prepare the data-extraction workbook.
12. Validate extracted study data.
13. Complete risk-of-bias assessment.
14. Define outcomes and effect measures.
15. Calculate effect sizes.
16. Perform the primary meta-analysis.
17. Perform heterogeneity, sensitivity, subgroup, and meta-regression analyses where appropriate.
18. Perform publication-bias analyses where appropriate.
19. Generate publication-ready forest plots, funnel plots, tables, and supplementary outputs.
20. Generate GRADE or certainty-of-evidence assessments where appropriate.
21. Draft the Results section.
22. Draft and complete the final manuscript.

### Phase 3 — Universalization

23. Search the entire repository for topic-specific hardcoding.
24. Remove terms such as SGLT2, cardiac surgery, dapagliflozin, empagliflozin, and topic-specific filenames from generic R scripts.
25. Move all topic-specific information into configuration files or structured input files.
26. Refactor repeated logic into reusable utility functions.
27. Finalize the universal `project_config.yml` schema.
28. Create configuration validation rules.
29. Ensure each pipeline step can be executed independently and resumed after failure.
30. Ensure all reports and workbooks are generated from configuration and input data rather than hardcoded text.

### Phase 4 — Cross-Topic Validation

31. Select a second SR-MA topic that differs meaningfully from the reference review.
32. Create a new project configuration without changing generic R code.
33. Run the complete pipeline with the second topic.
34. Document failures, assumptions, and topic-specific dependencies.
35. Revise the harness until both topics run successfully.

### Phase 5 — Frontend Development

36. Develop a Shiny-based frontend.
37. Collect topic, review framework, PICO or PECO, eligibility criteria, database selection, exclusion reasons, search concepts, and naming rules through guided questions.
38. Validate user input before project creation.
39. Automatically generate `config/project_config.yml`.
40. Allow Embase and CENTRAL RIS uploads.
41. Display pipeline status, warnings, missing inputs, and errors.
42. Allow users to run individual steps or resume the pipeline.
43. Provide downloadable search strategies, reports, datasets, workbooks, figures, and manuscript outputs.

### Phase 6 — One-Command Universal Pipeline

44. Implement a pipeline runner.
45. Support commands equivalent to:

    * initialize project
    * validate configuration
    * generate searches
    * validate searches
    * import records
    * deduplicate
    * prepare screening
    * prepare extraction
    * run meta-analysis
    * generate reports
    * generate manuscript
46. Add automated tests and example projects.
47. Complete documentation and installation instructions.

---

## 4. Current Project Status

The project is currently in the reference-review implementation phase.

Completed or substantially developed:

* Search-strategy generation
* Search-strategy validation
* PubMed testing and downloading
* Embase live-search documentation
* CENTRAL live-search documentation
* PubMed, Embase, and CENTRAL import
* Record merging and deduplication
* Title and abstract screening workbook generation

Current next task:

* Complete title and abstract screening using two independent human reviewers.
* Import reviewer decisions.
* Identify and resolve disagreements.
* Prepare the full-text screening workflow.

---

## 5. Current Reference Topic

The current reference implementation is:

**Effects of SGLT2 Inhibitors on Outcomes in Patients Undergoing Cardiac Surgery**

This topic is used to discover and validate all workflow requirements.

It must not permanently determine the architecture of the final universal harness.

---

## 6. Configuration Policy

`config/project_config.yml` will gradually become the source of all topic-specific settings.

Generic R scripts should eventually read, rather than hardcode:

* project title
* project ID
* short name
* review framework
* PICO or PECO
* eligibility criteria
* databases
* search concepts
* exclusion reasons
* reviewers
* outcomes
* effect measures
* analysis methods
* output names
* file paths
* manuscript settings

During the reference-review phase, only settings that are already stable should be moved into the configuration file.

Do not overdesign the configuration schema based on hypothetical future requirements.

---

## 7. Development Rules

### 7.1 Do Not Skip the Reference Implementation

Do not pause the current review to build the complete universal frontend or complete universal configuration schema.

### 7.2 Prevent New Hardcoding

New scripts should avoid introducing topic-specific text when a simple configuration value can be used.

### 7.3 Preserve Reproducibility

Every pipeline step must:

* use deterministic inputs where possible
* record input and output paths
* record execution date and time
* preserve intermediate data
* produce an execution report or log
* avoid silently overwriting important reviewed data

### 7.4 Preserve Human Review

Title/abstract screening and full-text screening will initially use:

* Reviewer 1
* Reviewer 2
* consensus discussion
* third reviewer adjudication when unresolved

The multi-agent screening approach is deferred and is not part of the current production workflow.

### 7.5 Avoid Premature Frontend Development

Do not build the final Shiny frontend until:

* the reference manuscript is complete
* the configuration schema is stable
* a second topic has been tested

### 7.6 Maintain Backward Compatibility

When refactoring a working script:

* preserve existing outputs when practical
* document changed filenames or column names
* provide migration instructions
* avoid breaking downstream scripts without updating them

### 7.7 Validate Every Major Output

Each major step must verify:

* required input files exist
* required columns exist
* record counts are internally consistent
* output files were created
* no unexpected records were lost
* errors stop execution with a clear message

---

## 8. Decision Priority

When development choices conflict, use this priority order:

1. Scientific validity
2. Reproducibility
3. Protection against erroneous study exclusion or data loss
4. Completion of the current reference SR-MA
5. Maintainability
6. Reusability
7. Automation
8. User-interface convenience

Automation must not reduce scientific validity or auditability.

---

## 9. Definition of the Universal Harness

The project will be considered a universal SR-MA harness only when:

1. A new review project can be initialized without editing generic R scripts.
2. Topic-specific information is supplied through configuration or structured frontend input.
3. At least two substantially different SR-MA topics complete the workflow successfully.
4. Database-specific search strategies can be generated and validated.
5. Records can be imported, standardized, merged, and deduplicated.
6. Screening and full-text review workbooks can be generated.
7. Data extraction and risk-of-bias workflows can be configured.
8. Meta-analysis methods can be selected according to outcome type.
9. Standard figures, tables, PRISMA outputs, and reports can be generated.
10. A manuscript scaffold or draft can be generated from verified outputs.
11. Each step is logged and auditable.
12. The workflow can resume after interruption or failure.

---

## 10. Instructions for Coding Agents

Before generating, editing, or refactoring project code:

1. Read this file.
2. Identify the current development phase.
3. Do not advance to a later phase unless the required earlier phase is complete or the user explicitly directs otherwise.
4. Preserve the current reference-review workflow.
5. Do not introduce unnecessary abstractions.
6. Do not replace working human-review steps with AI-agent decisions unless explicitly requested.
7. Clearly identify any proposed change that alters filenames, directories, schemas, or downstream compatibility.
8. When suggesting a next task, align it with the authoritative development sequence in this document.

This document is the authoritative project roadmap unless the user explicitly revises it.
