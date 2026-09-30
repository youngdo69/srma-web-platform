# Beta deployment

WEB-016 prepares the Web layer for a private organization beta. It does not change or revalidate the canonical scientific engine.

## Required boundary

Run the Shiny app on loopback behind an organization-controlled TLS/authentication reverse proxy. The proxy must remove any client-supplied `X-SRMA-User` header and set its own authenticated subject header. Set `SRMA_TRUSTED_AUTH_PROXY=1` only when that boundary is enforced.

Beta startup is fail-closed unless PostgreSQL, persistent `SRMA_WEB_DATA`, the trusted proxy boundary, and current database migrations are present. `SRMA_WEB_TEST_IDP` must be unset.

## Operator sequence

1. Restore the Web dependency environment.
2. Create the PostgreSQL database and protected runtime environment.
3. Run `Rscript --vanilla web/bin/beta_preflight.R`.
4. Provision organization subjects with `web/bin/provision_proxy_user.R`.
5. Start the worker and app under a service manager using the systemd examples.
6. Put a TLS/authentication reverse proxy in front of the loopback Shiny endpoint.
7. Use **Service Status** in the UI to explain operational readiness to users.

Production mode remains disabled until WEB-017.
