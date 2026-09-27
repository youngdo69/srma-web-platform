# Meta-analysis workflow

1. Complete Screening / Evidence through READY with Human inclusion, independent extraction verification and Human RoB review. Choose that saved version in Project Overview.
2. Open Meta-analysis and load its evidence groups. Choose a group and explicitly select the extraction rows. Review outcome/timepoint/design; enter the clinical definition, population, adjustment policy, exclusion reasons and protocol rationale.
3. Review the displayed canonical methods. Only supported Random-effects/REML/Knapp-Hartung with the documented transformation/correction policy is available. A conflicting protocol requires an upstream revision; the Web layer cannot silently change methods.
4. Explicitly choose optional diagnostics. Exploratory funnel/Egger/moderator testing requires a separate explicit opt-in, including acceptance of qualified underpowered output. Moderator discovery is never automatic. Save the plan as a new version, load it, review the entire plan and approve its analysis_plan gate under Human Review Required.
5. Queue analysis. The independent worker fits the canonical model and produces canonical forest PDF, CSV and editable XLSX tables. Leave and return safely; repeated active submissions are deduplicated. Non-estimable results remain actionable jobs, not approved pooled results.
6. Save the completed job as a new result version. Load it to inspect canonical estimates, confidence intervals, heterogeneity, warnings, forest plot and authorized downloads. The result is still unapproved.
7. Confirm intended studies, endpoint/timepoint, success, warnings and role; supply a rationale and approve the result. This freezes another immutable version. Canonical statistics cannot be manually edited.
8. Create a linked sensitivity/subgroup/exploratory plan from the same READY evidence, referencing the approved primary/secondary result. Explicitly record changed rows/definition and why. This never replaces the parent. Subgroup subsets use the same canonical estimator; no subgroup-interaction statistic is invented. Categorical moderator diagnostics are exploratory.
9. Revising evidence or configuration makes old results historical for downstream use. Finish the revised evidence and submit a new plan. Historical results/downloads and approval records remain intact; approvals never transfer.

All values are parsed from canonical outputs. Stored summary/statistics, CSV/XLSX tables and original/copy forest hashes are checked for consistency. Missing canonical statistics are not invented. Figure rendering is the actual authorized PDF, not a browser-drawn statistical reconstruction.

Scientific release still requires exact canonical runtime acceptance. Local synthetic-development PASS is implementation evidence only. Provider/AI and PostgreSQL acceptance remain separate. WEB-012 manuscript creation is not included.
