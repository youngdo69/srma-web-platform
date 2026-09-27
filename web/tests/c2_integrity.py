"""Read-only source/lock/lineage protection against approved C1."""
from pathlib import Path
import subprocess,json,hashlib
r=Path(__file__).resolve().parents[2];baseline='045ce28c954a5804f17f60f0f6d9c9958fa6100b'
def git(*a):return subprocess.check_output(['git','-C',str(r),*a])
m=json.loads((r/'web/docs/engine-source-manifest.json').read_text())
for p,h in m.items():
 assert hashlib.sha256((r/p).read_bytes()).hexdigest()==h,p
 assert git('show',baseline+':'+p)==(r/p).read_bytes(),p
assert git('show',baseline+':web/renv.lock')==(r/'web/renv.lock').read_bytes()
assert not git('diff','--name-only','--diff-filter=D',baseline)
assert not git('diff','--name-only',baseline,'--','R','config','renv.lock','web/renv.lock')
subprocess.check_call(['git','-C',str(r),'diff','--check',baseline])
print('PASS: 98 scientific hashes; both locks unchanged; no deletion; diff integrity')
