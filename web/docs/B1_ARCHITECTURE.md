# Block B1 — implementation boundary

Baseline: PM-approved Block A `50f1e3b`; frozen scientific source `e7b98da4cfd01abbfccc86205f670209eba6ae12`.

The architecture remains Shiny → authenticated application service → persisted jobs → fenced worker → private canonical adapter → immutable artifacts. No canonical R source or scientific lock is modified by the Web implementation.

## Identity and requests

`web_identity_provider(name, verify)` is a server-installed provider contract. A future OIDC verifier must validate authorization-code/PKCE/state, issuer, signature/JWKS, audience, expiry and nonce before returning an issuer-scoped subject. This release does not pretend to implement an OIDC provider or password database.

External identity mappings are explicitly provisioned; email addresses never auto-link accounts and sign-in never auto-enrolls workspaces. Opaque 256-bit OS-random session credentials are hashed in the database, expire within one hour and can be revoked. Every request re-resolves its session. Browser-supplied actor/user fields are rejected. Mutation requests require the current server-bound Shiny connection nonce. There are no ambient authentication cookies or URL tokens.

The local test provider requires `SRMA_WEB_TEST_IDP=1` both at issuance and request validation. The application has no fixed actor fallback. Internal CLI/worker services retain explicit trusted actor arguments; they are not browser routes. Production OIDC selection/TLS/security certification remain deployment decisions.

## Canonical configuration

The private writer reads the canonical initialization JSON schema and writes the existing `project_config.yml` shape. The canonical config/path utilities validate it. Web fields do not define a competing scientific schema or generate scientific search terms. Blank concept lists denote an unfinished draft. No reviewer, extraction, bias or analysis choices are inferred.

Each save/edit creates a checksummed immutable ProjectVersion. Configuration review and protocol registration requirements are created atomically with the version. Existing source versions and decisions are never rewritten. A saved draft is not scientific approval, protocol registration or a complete initialized scientific workflow.

## Database uploads

Database metadata, export formats, instructions and strategy paths come from canonical TASK-018. Strategies are read from saved inputs, never recreated in Web code. Uploaded manual exports are accepted only from the authenticated Shiny session's server-owned upload directory. A compatibility adapter binds the pinned Shiny 1.8.0 upload context to that directory; its actual file-write mechanism has an integration test.

The service rejects traversal/unsupported names, extensions, empty/oversized files, size mismatch, symlinks and out-of-root paths. It copies to the canonical destination in a fresh version, verifies SHA-256, records an audit event and requires fresh review. The client never chooses a destination path. Extension acceptance is not scientific format validation: canonical preflight performs that check separately.

Search-generation and PubMed adapters invoke the original R/11 and R/16 respectively. R/16's exact-query quality guard remains intact. These mappings remain disabled. Actual credential-free R/11 execution produces REVIEW_REQUIRED; actual R/16 rejects that strategy before EFetch. A successful credentialed QC/retrieval path and full upstream workflow integration are not demonstrated. No merge/deduplication, screening or later operation is enabled.

## Persistence

Migration 001 remains byte-for-byte unchanged. Migration 002 adds external identity mappings and sessions. Migration 003 prevents gate deletion and ProjectVersion update/deletion. All migration source checksums are checked on reuse. SQLite is an exercised local development backend; PostgreSQL status must be based on real server evidence, never inferred from these tests.

## Verified scientific preflight

`validate_database_inputs` uses the unchanged canonical TASK-018 on separately configured R 4.6.1. Submission and worker execution both check the original scientific lock and all 79 package versions. The Web R 4.3.3 library is never substituted. Missing manual input returns USER_ACTION_REQUIRED and publishes canonical evidence; complete synthetic manual inputs return COMPLETE with two checksummed reports. Status tables read those reports through authorized immutable storage. Both results remain UNREVIEWED and start no downstream operation.
