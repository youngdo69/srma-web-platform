# Final QC: author workflow

1. Approve the exact manuscript in Manuscript Builder. Final QC never treats manuscript approval as QC approval.
2. Select Final QC → Inspect selected approved manuscript. Wait for the worker, then Register completed worker inspection.
3. Review check/severity/source/action rows. Correct deterministic BLOCKER/MAJOR issues in the indicated earlier stage. A checkbox cannot waive numeric mismatch, invalid references, missing artifacts or checksum failures.
4. Review every Human queue item against source evidence/references, including Methods/protocol, study/report distinctions, RoB and actual Word/figure readability. Record Confirmed or Correction required and a rationale. Every disposition creates a new immutable QC record.
5. After machine checks and Human items pass, explicitly confirm and approve Human Final QC. This creates an exact-source final_qc decision.
6. Read external gates and release eligibility separately. A Human-approved QC can still be ineligible for scientific release. New upstream versions or artifact/ruleset changes invalidate eligibility; historical decisions stay visible.

Publication-facing fixture names, screening IDs, paths and placeholders are blocked. The system does not repair source science or silently regenerate Word. Use protected C2 fact/artifact tokens and real citations; every selected table/figure needs substantive Results coverage. Existing source-package tables can be inspected without treating report totals as analysis-study counts.
