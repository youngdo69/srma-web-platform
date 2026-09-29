# WEB-014 Download Center contract

WEB-014 is a presentation and packaging layer. It never recalculates or changes scientific results.

## User-facing rules

- The Download Center lists only files tied to an immutable, Human-approved Final QC lineage.
- Individual reviewed outputs can remain available for audit even when final-package creation is blocked.
- Historical reviewed versions are labeled as historical and cannot create a new final package.
- The final package can be created only when the authoritative WEB-013 `RELEASE_ELIGIBLE` state is true.
- The UI explains mandatory blocking checks in plain language. Optional provider acceptance remains visible without being misrepresented as a mandatory Human-authored release gate.

## Packaging rules

- File selection comes from registered artifacts and an allowlisted evidence snapshot, never loose filesystem discovery.
- Every selected file is checksum-verified before staging.
- Package paths are sanitized and collision-safe; path traversal and absolute paths are not accepted.
- The release manifest records immutable lineage and each included file's provenance/checksum.
- Release package records are append-only and immutable.
- Final ZIP creation re-checks release eligibility and manifest identity immediately before publication.

## Scope boundary

WEB-014 does not re-run search, screening, extraction, RoB, meta-analysis, manuscript generation, or scientific Final QC. It consumes already-approved outputs only.
