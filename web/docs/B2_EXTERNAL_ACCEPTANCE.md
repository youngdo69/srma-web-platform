# B2 operator acceptance (not executed locally)

Carry forward POSTGRESQL_EXTERNAL_ACCEPTANCE_PENDING, CREDENTIALED_ACCEPTANCE_PENDING, CANONICAL_RUNTIME_ACCEPTANCE_PENDING. Add CREDENTIALED_AI_ACCEPTANCE_PENDING. These are mandatory production gates; local R 4.3.3/SQLite deterministic tests do not satisfy them.

Use the B1 operator instructions first. Configure the canonical R 4.6.1 environment and approved lock with `SRMA_SCIENTIFIC_RSCRIPT` and `SRMA_SCIENTIFIC_LIBRARY`. Keep credentials only in the operator environment: the configured `agent_screening.provider.api_key_env` for screening/fulltext/extraction, OPENAI_API_KEY for canonical RoB. Never insert them in project fields or command arguments.

Prepare a small non-private, Human-approved fixture through the real UI; record project/version IDs at SCREENING, FULLTEXT (registered readable PDFs), EXTRACTION and ROB. Pause other workers so no jobs are queued/running. Point SRMA_WEB_DATA at the disposable acceptance instance (see web_settings for operator paths); use its actual provisioned actor.

Run one bounded provider operation at a time (at most settings timeout, capped at 600 seconds):

```
Rscript --vanilla web/tests/b2_operator_acceptance.R ACTOR PROJECT VERSION recommend_screening
Rscript --vanilla web/tests/b2_operator_acceptance.R ACTOR PROJECT VERSION recommend_eligibility
Rscript --vanilla web/tests/b2_operator_acceptance.R ACTOR PROJECT VERSION recommend_extraction
Rscript --vanilla web/tests/b2_operator_acceptance.R ACTOR PROJECT VERSION recommend_risk_of_bias
```

Use the appropriate original-stage version for each command. Each operation calls the real canonical engine/provider, registers artifacts and saves an immutable proposal version. The script checks proposals remain UNVERIFIED, Human fields are unchanged and new gates remain PENDING. A missing key/runtime is a failure, never a skip reported PASS. Provider calls incur real costs. Review evidence locations, report identity, model metadata and content accuracy, including uncertainty and overrides. Do not assert acceptance from a signature-only synthetic PDF.

For PostgreSQL run targeted migration/state/security tests in an unprivileged disposable database, including upgrade from schema 5, FK isolation and immutable triggers. Do not run migrations/tests against clinical/production data. No privilege workaround is authorized.

No models, figures, manuscript or release-generation operations are enabled by B2.
