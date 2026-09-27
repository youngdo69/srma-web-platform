# WEB-012 operator acceptance

Use migrations 010/011 through existing startup, with normal backup and isolated verification. Do not apply code backwards to a migrated DB. Production manuscript jobs preserve `web_science_settings`: exact R4.6.1 / approved locked packages required. Runtime/script choices never come from browser input. Word presentation requires officer/flextable and the fixed `pdftoppm` executable for selected PDFs. No lock changes are proposed.

Public PubMed metadata retrieval requires httr2/xml2 and outbound NCBI EFetch. No client-supplied reference record or verified flag is accepted. The library hash records actual server verification; plan approval is the Human relevance review. Refreshing a material reference set means creating a new plan. Credential-free author editing works with a verified frozen library.

For real AI acceptance configure OPENAI_API_KEY and optionally SRMA_MANUSCRIPT_MODEL on the operator server. Select a genuine Human-approved plan, prepare a DRAFT, configure SRMA_ACCEPTANCE_ACTOR / SRMA_ACCEPTANCE_PROJECT / SRMA_ACCEPTANCE_MANUSCRIPT_VERSION and run:

```sh
Rscript --vanilla web/tests/c2_operator_acceptance.R
```

This command produces a real unreviewed Introduction draft in the chosen project. Use a non-private operator test project; it creates immutable versions/jobs. It is not run by default and does not approve content. Continue actual Human section review, pre-check, Word generation, authorized download and manuscript approval. Review real citation relevance and correct numerical semantics. WEB-013 full semantic/publication QC remains unimplemented and unclaimed.

Local fixture `web/tests/c2_fixture.R` uses genuine cached public reference metadata, synthetic study data and the existing explicitly test-process-only development runtime seam. It is not credentialed/canonical acceptance. The fixture's lower reference minima are explicitly in its synthetic project config; production defaults are unchanged.

External gates remain POSTGRESQL_EXTERNAL_ACCEPTANCE_PENDING, CREDENTIALED_ACCEPTANCE_PENDING, CANONICAL_RUNTIME_ACCEPTANCE_PENDING, CREDENTIALED_AI_ACCEPTANCE_PENDING and CREDENTIALED_MANUSCRIPT_ACCEPTANCE_PENDING until separately exercised. PostgreSQL migrations are supplied, but local SQLite success is not PostgreSQL proof.
