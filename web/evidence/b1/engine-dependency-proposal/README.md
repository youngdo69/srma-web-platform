# ENGINE_DEPENDENCY_UPDATE_READY — PM review only

No canonical source lock was changed. The unchanged prescribed maintenance utility completed in an isolated scientific environment, installing officer 0.7.6 and flextable 0.10.1.

`PROPOSED_NOT_APPLIED_renv.lock` and `proposed-renv.diff` preserve its exact raw snapshot. It adds 27 packages but drops 12 original packages, including meta, lme4 and their dependencies, because the snapshot dependency scan does not retain the entire original lock. **Do not adopt this raw snapshot.** `package-diff.json` lists the exact additions/removals.

`ADDITIVE_PM_REVIEW_renv.lock` is the recommended conservative proposal: all 79 original package records and original R/repository metadata are preserved, with only the 27 new dependency records added (106 total). No existing package is removed or upgraded. `additive-renv.diff` is its exact diff. This proposal has not been applied to the approved repository. Namespace/version checks and a real synthetic DOCX/table smoke verify dependency resolution only; the scientific R/41 workflow is not executed and its upstream human gates remain untouched.
