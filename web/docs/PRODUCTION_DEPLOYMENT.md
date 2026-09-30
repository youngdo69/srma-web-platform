# Production deployment

WEB-017 keeps the scientific engine separate from Web orchestration. Production startup is fail-closed for the **exact final Web code and dependency lock**.

## Required production boundary

- PostgreSQL operational database under an unprivileged application role.
- Persistent `SRMA_WEB_DATA` on backed-up storage.
- Shiny bound to loopback only.
- TLS + organization authentication reverse proxy that strips/overwrites the identity header.
- Test identity provider disabled.
- Current database migrations.
- Actual `POSTGRESQL` and `CANONICAL_RUNTIME` acceptance evidence for the exact Web code hash and root lock.

Provider-backed AI features remain separately fail-closed when credentials are absent; they are not permitted to fabricate scientific or release acceptance.

## Acceptance evidence without contaminating production data

Run `web/tests/c3_operator_acceptance.R` in the appropriate disposable/operator acceptance environment with `SRMA_ACCEPTANCE_EXPORT=/secure/path/<kind>.json`. After the test passes, copy the evidence through an operator-controlled channel and import it into the production PostgreSQL database:

```sh
Rscript --vanilla web/bin/import_acceptance.R /secure/path/CANONICAL_RUNTIME.json
Rscript --vanilla web/bin/import_acceptance.R /secure/path/POSTGRESQL.json
```

The importer rejects evidence whose lock hash or executable Web-code hash differs from the deployed repository.

Then run:

```sh
Rscript --vanilla web/bin/production_preflight.R
```

Only after PASS should the app and worker services be started/restarted in `SRMA_WEB_MODE=production`.

## Scientific release remains per-project

A production service can host multiple reviews. Each final package is still gated by its own current EvidenceVersion, AnalysisVersion, ManuscriptVersion and immutable Final-QC decision. WEB-014 will not build a final scientific package unless that project is release-eligible.
