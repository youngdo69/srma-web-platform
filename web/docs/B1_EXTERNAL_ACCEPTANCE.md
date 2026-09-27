# B1 external acceptance

Implementation tests and external acceptance are separate. All commands below must run from the repository root with an operator-owned environment. Never put provider credentials in arguments, JSON, YAML, project folders or reports. Do not copy `.env` into project snapshots. Use a dedicated, non-sensitive acceptance project and a dedicated `SRMA_WEB_DATA` directory.

## Scientific runtime

Set `SRMA_RSCRIPT` to the Web R runner, `SRMA_SCIENTIFIC_RSCRIPT` to canonical R 4.6.1 and `SRMA_SCIENTIFIC_LIBRARY` to an isolated library restored from the approved root lock. Restore in an isolated checkout, never change the canonical lock to match a development runtime. Verify `renv::status()` after restore. Run `START_HERE.R` there with `options(srma.start_here.environment_only=TRUE)`; this does not authorize scientific workflow execution. Then:

```sh
"$SRMA_SCIENTIFIC_RSCRIPT" --vanilla web/tests/scientific_docx.R "$PWD" "$SRMA_SCIENTIFIC_LIBRARY"
"$SRMA_RSCRIPT" --vanilla web/tests/b1_preflight.R
"$SRMA_RSCRIPT" --vanilla web/tests/b1_search_guard.R
```

The DOCX test checks all locked versions, actual R/41 dependency names, officer/flextable and an editable Word table. It is a dependency smoke, not scientific manuscript regression. The private negative guard test deliberately unsets the provider credential and must observe actual REVIEW_REQUIRED and R/16 rejection. Neither was successfully executed in the present environment.

## Credentialed acceptance

The provider environment-variable name is read from canonical `config/initialization_agent.yml`; configure it securely in the operator process. No Web credential field exists. `NCBI_API_KEY` is optional and also environment-only.

```sh
"$SRMA_RSCRIPT" --vanilla web/tests/b1_operator_acceptance.R prepare /absolute/non-sensitive-project
```

Inspect the resulting dedicated data directory's `b1_acceptance.json`. It contains the immutable initiating version, registered initialization job, proposed canonical configuration/terms and unresolved decisions. Create an explicit review JSON with these fields:

- `project_id`, `draft_version_id`, `draft_job_id`: exact identifiers from that file.
- `selections`: object mapping every canonical decision ID to the human-selected option text; `{}` if no decisions remain.
- `human_approved`: true only after actual review.
- `rationale`: actual review rationale.
- `protocol_approved`: true only after explicit protocol/test review.
- `protocol_reference`: truthful supporting reference; never invent PROSPERO registration or claim a synthetic test is a registered real review.

```sh
"$SRMA_RSCRIPT" --vanilla web/tests/b1_operator_acceptance.R complete /absolute/explicit-review.json
```

The command activates through the canonical decision/activation path, generates a real canonical search, requires QC PASS and 1–100 matches for this bounded acceptance test, then executes unchanged R/16. Each subprocess has a 180-second deadline. It verifies one attempt, pinned search job, exact query hash, exact approved version, retrieval report and raw artifact checksums. It does not start merge/deduplication/screening. It stops with an actionable error if review, count, external service or environment requirements fail. Existing state is retained for inspection. This resource bound belongs to the acceptance test only; no scientific production count cutoff was added.

Current status: **CREDENTIALED_ACCEPTANCE_PENDING**. The command was syntax-checked, not executed with a private credential.

## PostgreSQL

Use the existing `web/tests/postgres.R` with valid operator-supplied PG environment settings under an ordinary PostgreSQL server/user. Inspect that test's required dedicated database before running it. No PostgreSQL privilege bypass or root-server patch is allowed. Preserve the existing tests, including concurrency/fencing and immutable-record checks. Migrations 004–005 add composite version-bound provenance references and immutable lineage/source rows; exercise them with the pipeline tests against a disposable PostgreSQL database as well.

Current status: **POSTGRESQL_EXTERNAL_ACCEPTANCE_PENDING**. Current container offers only root and no usable unprivileged server/container runtime. No new attempts were made to circumvent this limitation.
