from pathlib import Path
import re, subprocess, sys

ROOT = Path(__file__).resolve().parents[2]
BASE = "255932c68ced6cfd3416e96dea1b9c79e26db8ff"

def need(cond, msg):
    if not cond:
        raise AssertionError(msg)


release = (ROOT / "web/R/release.R").read_text()
ui = (ROOT / "web/ui/download_center.R").read_text()
identity = (ROOT / "web/R/identity.R").read_text()
sql = (ROOT / "web/db/014_release.sql").read_text()
sqlite_guard = (ROOT / "web/db/015_release_immutability.sqlite.sql").read_text()
pg_guard = (ROOT / "web/db/015_release_immutability.postgres.sql").read_text()

need("isTRUE(catalog$release_eligible)" in release, "release package must require authoritative release eligibility")
need("Final QC is historical or stale" in release, "stale QC must block packaging")
need("web_c4_safe_component" in release and "basename(x)" in release, "release filenames must be normalized")
need("web_inside(file.path(stage, e$package_path)" in release, "archive staging must enforce path containment")
need("identical(web_sha(target), e$sha256)" in release, "staged files must match registered checksums")
need("RELEASE_PACKAGE_CREATED" in release, "release package creation must be audited")
need("release packages are immutable".lower() in (sqlite_guard + pg_guard).lower(), "release records must be immutable")
need("release_build=c(\"project_id\",\"qc_id\")" in identity.replace(" ", ""), "browser must not supply release eligibility")
need("release_eligible" in ui and "Historical versions cannot create" in ui, "UI must distinguish release eligibility and history")
need("Individual approved files can still be downloaded" in ui, "blocked-release UX must preserve approved individual downloads")
need("CREATE TABLE release_packages" in sql, "release package persistence is missing")

changed = subprocess.check_output(["git", "diff", "--name-only", BASE], cwd=ROOT, text=True).splitlines()
for p in changed:
    need(not p.startswith("R/"), f"WEB-014 must not modify canonical scientific source: {p}")
need("renv.lock" not in changed and "web/renv.lock" not in changed, "WEB-014 must not change dependency locks")

for forbidden in ["git reset --hard", "RELEASE_ELIGIBLE=TRUE", "Ready for publication"]:
    need(forbidden not in release + ui, f"forbidden WEB-014 behavior/text: {forbidden}")

print("PASS: WEB-014 static web contract")
print(f"Changed paths against C3 baseline: {len(changed)}")
