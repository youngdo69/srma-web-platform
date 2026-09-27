# WEB-011 canonical contract / audit

Baseline: approved B2 17edbfd4583d752c9aa0abf97ee71980b8defc49. No scientific source or lock change.

| Responsibility | Actual canonical component / boundary |
|---|---|
| Verified groups / rows | R/26 finalize_human_verified_extraction: explicit Human roles, outcomes, designs, timepoints, meta_input_type; dependent rows rejected |
| Main and selected subset models | R/27 run_universal_meta_analysis, run_one_group / prepare_effects. RR/OR/HR/RD/MD/SMD; reported CI, raw binary or continuous. Random-effects REML, Knapp-Hartung; no fixed-model option. Raw binary correction 0.5 only0, drop00 FALSE. CI95. Human must explicitly accept this fixed engine policy. |
| Sensitivity | Approved canonical R26 groups or explicit Human subset of exact verified rows, linked to approved parent; same R27 function, never Web formulas |
| Diagnostics | R/28 run_meta_analysis_diagnostics; canonical leave-one-out / Baujat, explicitly opted in |
| Exploratory / categorical moderators | R/28B generate_universal_exploratory_diagnostics; optional explicit full bundle, configured moderators only (auto discovery disabled), descriptive funnel / qualified Egger / numeric or categorical regression. No invented subgroup interaction statistic. |
| Figures/tables | R/33 generate_forest_plots_and_editable_tables: reads R27 outputs, copies its forest PDF, never refits main model; CSV and editable XLSX |
| Excluded integration | R29 interactive extra-data/AI intake bypasses B2 verification and writes manuscript text; do not expose it. No WEB-012. |

Web orchestrates validated file contracts only. Each immutable plan pins the exact approved READY EvidenceVersion and hashes, included row/report/study IDs, outcome definition, population, timepoint, design, effect/adjustment policy, exclusions and rationale. User-supplied runtime paths/scripts are forbidden. Fixed engine settings are displayed and explicitly approved; conflicting config requires upstream Human revision, never silent override. Subgroup-labelled subsets require a Human definition and approved parent; no claim of subgroup interaction testing.

PLAN -> canonical worker -> RESULT -> explicit Human result gate -> APPROVED: every transition creates a ProjectVersion/AnalysisVersion, gates are never inherited. New upstream EvidenceVersion invalidates current use of old analysis approvals while preserving immutable historical results. Downloads remain authorized/checksummed historical artifacts, not evidence of current approval.

One bounded canonical-runtime availability probe found R4.3.3, no configured exact R4.6.1. No runtime guard bypass in production. Test-only development seam and synthetic numbers cannot establish scientific release acceptance.
