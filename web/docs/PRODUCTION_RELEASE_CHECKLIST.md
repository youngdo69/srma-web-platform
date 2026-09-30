# Production release checklist

## Platform
- [ ] Exact final source commit deployed.
- [ ] PostgreSQL acceptance evidence imported for the exact Web code/lock.
- [ ] Canonical runtime acceptance evidence imported for the exact Web code/lock.
- [ ] `production_preflight.R` passes.
- [ ] Shiny listens only on loopback.
- [ ] Organization TLS/authentication proxy strips and sets `X-SRMA-User`.
- [ ] `SRMA_WEB_TEST_IDP` is unset.
- [ ] Persistent data directory is backed up and restore procedure is documented.
- [ ] App and worker run as an unprivileged service account.
- [ ] Secrets are supplied outside Git and outside project snapshots.

## User acceptance
- [ ] Sign in through organization identity.
- [ ] Open an authorized workspace/project.
- [ ] Service Status is understandable to a non-technical research user.
- [ ] Security & Audit records sign-in/download activity.
- [ ] Download Center distinguishes current/historical files and explains blocked final packaging.

## Scientific release
- [ ] Do not infer scientific acceptance from platform health.
- [ ] Each review must have current Human-approved Final QC.
- [ ] Final package generation remains blocked when that review is not release-eligible.
