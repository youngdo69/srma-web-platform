# B1 evidence index

This directory contains actual execution evidence, including failed attempts. Read the PM report for acceptance status. No PostgreSQL server test passed in this environment.

## Final acceptance evidence

- `postgres-attempts.txt`: real installed binary version and environment restrictions; not server execution.
- `web-clean-restore-verified.log`, `web-clean-runtime.json`: supported Web setup completion and 56 exact package versions/paths, no old Block A package library.
- `clean-suite.log`, `clean-test-summary.json`, `clean-*.log`: clean environment tests. Later focused regressions use `clean-b1_ui-final.log`, `clean-b1_search_guard.log`, `clean-baseline-final.log` and `clean-b1_preflight-post-maintenance.log`.
- `vertical-slice.json`, `clean-vertical.log`: actual Shiny HTTP/assets, application exit, two separate workers, one engine attempt and two downloaded outputs.
- `preflight-real-engine.json`: real scientific R 4.6.1 manual-input preflight, synthetic RIS only, pending human approvals preserved.
- `search-negative-path.json`: actual R/11 credential-free review-required result and R/16 rejection before EFetch; zero downloaded records. Not successful search-quality approval or retrieval.
- `scientific-runtime-original-lock.json`: all 79 original locked namespaces/versions verified before adding the DOCX proposal.
- `scientific-docx-maintenance-verified.log`: unchanged prescribed utility successfully installed officer/flextable and generated its raw isolated snapshot.
- `scientific-docx-validation-verified.log`, `scientific-docx-validation.json`: all 79 original and all 106 proposed package versions load; real synthetic DOCX/native table smoke; no R/41 workflow execution.
- `scientific-start-here-verified.log`: unchanged canonical environment-only startup using the conservative proposal in the isolated copy; synchronized restore and required direct packages PASS.
- `engine-dependency-proposal/`: exact raw utility snapshot/diff, explicitly rejected removals, and conservative additive proposal/diff for PM review. The approved root lock is unchanged.
- `runtime-recipes/`: historical workspace-specific command scripts and SDK package metadata, not portable installers.

## Historical attempt logs

`*-development.log` uses the earlier development environment and is not clean restore proof. Earlier restore/maintenance logs retain compiler/dependency/stale-lock failures to document repairs. `scientific-restore-historical-mixed.log` is a historical mixed-output file, not authoritative final proof. Earlier `scientific-docx-validation*.log` without the `-verified` suffix may contain a failed test assertion about Word XML formatting; the corrected verification parses the XML table rather than assuming the element has no attributes. Use the final files above for conclusions.

Fixtures are explicitly synthetic. No real study records, private API credentials, production sessions or research datasets are included.
