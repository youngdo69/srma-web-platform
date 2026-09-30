# Operations runbook

## Start / restart
1. Validate the protected environment file and persistent storage.
2. Run `Rscript --vanilla web/bin/production_preflight.R`.
3. Restart worker, then app.
4. Confirm reverse-proxy sign-in and the Service Status page.

## Health
`Rscript --vanilla web/bin/health.R` reports operational state without changing scientific results.

## User provisioning
Use `web/bin/provision_proxy_user.R` from an operator shell. Browser requests cannot create identities or assign themselves to workspaces.

## Backup
Back up PostgreSQL and `SRMA_WEB_DATA` together using your organization backup system. Stop writes or use transactionally consistent database/object-storage procedures. Never restore only one side and assume artifact integrity.

## Incident response
- Use **Sign out all sessions** for an affected account.
- Review project Security & Audit history.
- Rotate external provider/database credentials outside Git.
- Preserve immutable audit/QC/release records.
- If executable Web code or canonical lock changes, prior external acceptance will no longer match and production preflight will fail until reaccepted.

## Upgrade
Deploy to a separate checkout, inspect migrations, back up, run migrations/preflight once, then switch services. Do not overwrite a live checkout with a partial delta.
