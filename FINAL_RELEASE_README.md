# SR-MA Web Platform — Final Source Release

This repository contains the integrated Universal SR-MA scientific toolkit plus the WEB-001–WEB-017 Web platform.

## What the Web layer does

The Web platform guides a researcher from project setup through search/import, Human screening, full text, extraction, risk of bias, meta-analysis, manuscript review, Final QC and authorized downloads. The canonical R engine remains the scientific source of truth; Web code orchestrates versions, Human Gates, provenance, authorization and presentation rather than reimplementing statistical science.

## Operating modes

- `development`: local SQLite/test workflows only.
- `beta`: PostgreSQL + persistent storage + trusted organization-authentication proxy; production acceptance not required.
- `production`: all beta requirements plus current PostgreSQL and canonical-runtime acceptance evidence for the exact Web code and dependency lock.

Production Shiny binds to loopback and is intended to sit behind a TLS/authentication reverse proxy. See `web/docs/PRODUCTION_DEPLOYMENT.md` and `web/docs/PRODUCTION_RELEASE_CHECKLIST.md`.

## Important distinction

Platform health does **not** make a review scientifically releasable. Each review still requires current Human-approved Final QC. `SRMA_FINAL_PACKAGE.zip` generation remains blocked unless that review's immutable release eligibility is true.

## Local source restore

Unpack the final source archive into a new directory. Do not overlay a live repository. Git users can instead clone the accompanying final Git bundle.

## Secrets

No passwords, API keys or `.env` files belong in Git. Supply database/provider secrets through your organization service manager or secret store.
