# WEB-017 Final Integration

WEB-017 closes the planned Web-platform sequence (WEB-001–WEB-017).

The final architecture keeps the canonical SR-MA R engine as the scientific source of truth and uses the Web layer for guided workflow, immutable versions, Human Gates, audit/provenance, background jobs, artifact delivery and deployment boundaries.

Production mode is fail-closed. It requires PostgreSQL, persistent storage, a trusted TLS/organization-authentication proxy, current migrations, and imported operator acceptance evidence for the exact Web code hash and dependency lock. Scientific final-package eligibility remains project-specific and requires a current Human-approved Final QC decision.

No production credential, private manuscript, runtime library tree, operational database or user PDF is part of the source release.
