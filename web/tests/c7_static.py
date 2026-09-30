#!/usr/bin/env python3
"""WEB-017 production-integration checks; intentionally no canonical scientific execution."""
from pathlib import Path
root=Path(__file__).resolve().parents[2]
dep=(root/'web/R/deployment.R').read_text(); ui=(root/'web/ui/deployment.R').read_text(); op=(root/'web/tests/c3_operator_acceptance.R').read_text()
assert 'web_release_acceptance_status' in dep
assert 'CANONICAL_RUNTIME_ACCEPTED' in dep and 'POSTGRESQL_ACCEPTED' in dep
assert 'grepl("^https://", settings$public_url)' in dep
assert 'Production Shiny must bind to loopback' in dep
assert 'web_import_release_acceptance' in dep
assert 'SRMA_ACCEPTANCE_EXPORT' in op and 'SRMA_ACCEPTANCE_RECORD' in op
assert 'Production startup is blocked' in ui
for f in (
 'web/bin/production_preflight.R','web/bin/import_acceptance.R','web/deploy/production.env.example',
 'web/deploy/nginx/srma-web.conf.example','web/docs/PRODUCTION_DEPLOYMENT.md',
 'web/docs/PRODUCTION_RELEASE_CHECKLIST.md','web/docs/OPERATIONS_RUNBOOK.md','FINAL_RELEASE_README.md'):
    assert (root/f).exists(), f
nginx=(root/'web/deploy/nginx/srma-web.conf.example').read_text()
assert 'auth_request' in nginx and 'proxy_set_header X-SRMA-User $srma_user' in nginx and '127.0.0.1:3838' in nginx
# Scientific R source/locks must not be modified by WEB-015–017; Git check is performed separately.
print('PASS: WEB-017 static production integration contract')
