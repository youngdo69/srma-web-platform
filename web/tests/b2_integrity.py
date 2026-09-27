"""Read-only scientific-source and dependency protection for B2."""
from pathlib import Path
import hashlib,json,subprocess
root=Path(__file__).resolve().parents[2]
manifest=json.loads((root/'web/docs/engine-source-manifest.json').read_text())
for path,expected in manifest.items():
 assert hashlib.sha256((root/path).read_bytes()).hexdigest()==expected,path
baseline='fb303704d7a7412eedf2d8736bd0de7886d1f8ef'
assert not subprocess.check_output(['git','diff',baseline,'--','R','config','renv.lock','web/renv.lock'],cwd=root)
assert not subprocess.check_output(['git','diff','--name-only','--diff-filter=D',baseline],cwd=root)
print('PASS:',len(manifest),'scientific hashes; canonical source, both locks unchanged; no deletions')
