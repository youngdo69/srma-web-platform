# WEB-011 operator acceptance

Use a fresh isolated data directory and the approved R4.6.1 + exact root renv.lock library. Set SRMA_SCIENTIFIC_RSCRIPT and SRMA_SCIENTIFIC_LIBRARY only through operator environment; set SRMA_RSCRIPT to the Web runtime. Restore both documented dependency environments without changing either lock. Never activate test-only development seams for scientific acceptance.

Run from the repository root:

```
Rscript --vanilla web/tests/c1_operator_acceptance.R
python3 web/tests/c1_integrity.py
```

The first command performs the actual production runtime probe before the synthetic real-canonical E2E. It retains all guards, traverses the real manual B2 READY flow, plan/result Human gates, canonical primary/sensitivity/diagnostic/exploratory execution, numeric checks, security and UI. The frozen reference is labelled development R4.3.3/metafor4.4.0; it is a comparison target, not pre-certified R4.6.1 output. Investigate any tolerance discrepancy under the approved runtime; never overwrite expected values to make a test pass. Retain runtime/package versions and logs for Reviewer acceptance.

Locally verified development command (requires the separately provisioned development packages):

```
Rscript --vanilla web/tests/c1_fixture.R
```

Only that test uses a process-local replacement of web_science_settings when C1_CANONICAL_ACCEPTANCE is not 1. Production code never checks that variable and never disables the guard.

POSTGRESQL_EXTERNAL_ACCEPTANCE_PENDING: run existing web/tests/postgres.R under an unprivileged correctly configured PostgreSQL identity, including migration 008/009. No root privilege workaround.
CREDENTIALED_ACCEPTANCE_PENDING and CREDENTIALED_AI_ACCEPTANCE_PENDING: follow B1/B2 operator procedures. Meta-analysis itself makes no AI/provider request.
CANONICAL_RUNTIME_ACCEPTANCE_PENDING: exact runtime unavailable locally; no scientific release is approved.

Reviewer: confirm immutable source hashes, explicit methods/exclusions/definition, role-qualified forest filenames, canonical CSV/XLSX/PDF checksums, recorded attempt/engine/lock/adapter provenance, new-version gates and stale-evidence rejection. Historical downloads are research records, not proof of current downstream approval. No manuscripts or releases are generated here.
