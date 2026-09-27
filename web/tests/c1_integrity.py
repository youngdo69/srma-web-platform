"""Scientific source/lock protection against the approved B2 commit."""
import pathlib,json,hashlib,subprocess
r=pathlib.Path(__file__).resolve().parents[2];base='17edbfd4583d752c9aa0abf97ee71980b8defc49'
def git(*args):return subprocess.check_output(['git','-C',str(r),*args])
manifest=json.loads((r/'web/docs/engine-source-manifest.json').read_text())
for p,h in manifest.items():
 assert hashlib.sha256((r/p).read_bytes()).hexdigest()==h,p
 assert git('show',base+':'+p)==(r/p).read_bytes(),p
assert git('show',base+':web/renv.lock')==(r/'web/renv.lock').read_bytes()
assert not git('diff','--name-only','--diff-filter=D',base).strip()
assert not git('diff','--name-only',base,'--','R','config','renv.lock','web/renv.lock').strip()
subprocess.check_call(['git','-C',str(r),'diff','--check',base])
print('PASS: 98 canonical source hashes, both locks byte-identical to approved B2, no scientific changes/deletions, diff check')
