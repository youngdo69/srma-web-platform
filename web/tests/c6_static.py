#!/usr/bin/env python3
"""WEB-016 beta-deployment contract checks; no scientific execution."""
from pathlib import Path
root=Path(__file__).resolve().parents[2]
core=(root/'web/R/core.R').read_text(); dep=(root/'web/R/deployment.R').read_text(); app=(root/'web/bin/app.R').read_text(); worker=(root/'web/bin/worker.R').read_text(); ui=(root/'web/ui/deployment.R').read_text()
for needle in ('SRMA_WEB_MODE','SRMA_TRUSTED_AUTH_PROXY','SRMA_AUTH_HEADER','SRMA_PUBLIC_URL'):
    assert needle in core or needle in dep, needle
assert 'settings$backend == "postgres"' in dep
assert 'SRMA_WEB_TEST_IDP' in dep and 'settings$backend == "postgres"' in dep
assert 'web_proxy_sign_in' in dep and 'SRMA_TRUSTED_AUTH_PROXY' in core
# WEB-017 may enable production, but beta requirements must remain intact.
assert 'web_proxy_sign_in' in dep and "provider='trusted-proxy'" in dep
assert 'web_deployment_preflight(con, settings)' in app and 'web_deployment_preflight(con, settings)' in worker
assert 'Service Status' in ui
for f in ('web/bin/beta_preflight.R','web/bin/health.R','web/bin/provision_proxy_user.R','web/docs/BETA_DEPLOYMENT.md','web/deploy/systemd/srma-web-app.service.example','web/deploy/systemd/srma-web-worker.service.example'):
    assert (root/f).exists(), f
print('PASS: WEB-016 static beta-deployment contract')
