from pathlib import Path
import os,subprocess,json,time
base=Path('/workspace/scratch/f22ae3d8c5eb');repo=base/'source/srma-agent-toolkit';r=str(base/'runtime/Rscript-clean433')
env=os.environ.copy();env.update(SRMA_WEB_LIBRARY=str(base/'runtime/web-clean433'),SRMA_RSCRIPT=r)
results=[]
for name in ['clean_runtime','baseline','foundation','contract','state','run','b1','b1_import','b1_security','b1_ui','b1_preflight']:
 current=env.copy()
 if name=='b1_preflight':current.update(SRMA_SCIENTIFIC_RSCRIPT=str(base/'runtime/Rscript461'),SRMA_SCIENTIFIC_LIBRARY=str(base/'runtime/scientific-b1/renv/library/linux-ubuntu-noble/R-4.6/x86_64-pc-linux-gnu'))
 command=[r,'--vanilla','web/tests/'+name+'.R'];start=time.time()
 with (repo/('web/evidence/b1/clean-'+name+'.log')).open('w') as stream:
  p=subprocess.run(command,cwd=repo,env=current,stdout=stream,stderr=subprocess.STDOUT,timeout=240)
 results.append({'test':name,'command':command,'exit_code':p.returncode,'seconds':round(time.time()-start,2)})
 print(name,p.returncode,flush=True)
 (repo/'web/evidence/b1/clean-test-summary.json').write_text(json.dumps(results,indent=2))
 if p.returncode:raise SystemExit(p.returncode)
with (repo/'web/evidence/b1/clean-vertical.log').open('w') as stream:
 p=subprocess.run(['python3','web/tests/vertical.py'],cwd=repo,env=env,stdout=stream,stderr=subprocess.STDOUT,timeout=180)
print('vertical',p.returncode,flush=True)
raise SystemExit(p.returncode)
