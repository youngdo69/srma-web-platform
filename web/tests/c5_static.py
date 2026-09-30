#!/usr/bin/env python3
"""WEB-015 Web-only security/audit contract checks; no scientific engine execution."""
from pathlib import Path
import sqlite3, tempfile
root = Path(__file__).resolve().parents[2]
checks = {
    "security module": root / "web/R/security_audit.R",
    "security UI": root / "web/ui/security_audit.R",
    "auth audit schema": root / "web/db/016_security_audit.sql",
    "auth audit trigger": root / "web/db/017_security_audit_immutability.sqlite.sql",
}
for label, path in checks.items():
    assert path.exists(), f"missing {label}: {path}"
identity = (root / "web/R/identity.R").read_text()
storage = (root / "web/R/storage.R").read_text()
release = (root / "web/R/release.R").read_text()
security = (root / "web/R/security_audit.R").read_text()
assert all(x in identity for x in ("audit_list", "audit_export", "security_summary", "logout_all"))
assert "ARTIFACT_DOWNLOADED" in storage
assert "DOWNLOAD_CENTER_ITEM_DOWNLOADED" in release and "RELEASE_PACKAGE_DOWNLOADED" in release
assert "[REDACTED]" in security and "web_c5_logout_all" in security
with tempfile.NamedTemporaryFile(suffix=".sqlite") as tmp:
    con = sqlite3.connect(tmp.name)
    con.executescript("CREATE TABLE app_users(id TEXT PRIMARY KEY, display_name TEXT NOT NULL); INSERT INTO app_users VALUES('u','User');")
    con.executescript((root / "web/db/016_security_audit.sql").read_text())
    for part in (root / "web/db/017_security_audit_immutability.sqlite.sql").read_text().split("-- NEXT"):
        con.executescript(part)
    con.execute("INSERT INTO auth_audit_events VALUES('a','u','trusted-proxy','SIGN_IN','{}','now')")
    try:
        con.execute("UPDATE auth_audit_events SET event_type='X' WHERE id='a'")
        raise AssertionError("auth audit UPDATE was not blocked")
    except sqlite3.DatabaseError:
        pass
    try:
        con.execute("DELETE FROM auth_audit_events WHERE id='a'")
        raise AssertionError("auth audit DELETE was not blocked")
    except sqlite3.DatabaseError:
        pass
print("PASS: WEB-015 static security/audit contract")
