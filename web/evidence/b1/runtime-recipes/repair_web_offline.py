from pathlib import Path
import json,tarfile,re,subprocess,os,sys
base=Path('/workspace/scratch/f22ae3d8c5eb');repo=base/'source/srma-agent-toolkit';lib=base/'runtime/scientific-b1/renv/library/linux-ubuntu-noble/R-4.6/x86_64-pc-linux-gnu'
lock=json.loads((repo/'web/renv.lock').read_text());lib=base/'runtime/web-clean433';archives={};deps={}
for n,d in lock['Packages'].items():
 p=Path('/root/.cache/R/renv/source/repository')/n/(n+'_'+d['Version']+'.tar.gz')
 if not p.exists():continue
 with tarfile.open(p) as t:
  text=t.extractfile(n+'/DESCRIPTION').read().decode();fields={};key=None
  for line in text.splitlines():
   if line.startswith((' ','\t')) and key:fields[key]+=' '+line.strip()
   elif ': ' in line:key,val=line.split(': ',1);fields[key]=val
  assert fields['Version']==d['Version'],(n,fields['Version'],d['Version'])
  deps[n]=[re.split(r'\s|\(',x.strip())[0] for x in ','.join(fields.get(k,'') for k in ['Depends','Imports','LinkingTo']).split(',') if x.strip()]
 archives[n]=p
ordered=[];seen=set()
def visit(n):
 if n in seen or n not in archives:return
 seen.add(n)
 for dep in deps[n]:visit(dep)
 ordered.append(n)
for n in archives:visit(n)
env=os.environ.copy();env.update(CPATH=str(base/'runtime/root/usr/include'),TAR_OPTIONS='--no-same-owner',PATH=str(base/'runtime/bin')+':'+str(base/'runtime/root/usr/bin')+':'+env['PATH'],XML_CONFIG=str(base/'runtime/root/usr/bin/xml2-config'),PG_CONFIG=str(base/'runtime/pg_config'))
built=0
for n in ordered:
 code=f'.libPaths(c({json.dumps(str(lib))},.Library),include.site=FALSE); ok <- requireNamespace({json.dumps(n)},quietly=TRUE); if(ok) ok <- packageVersion({json.dumps(n)})==package_version({json.dumps(lock["Packages"][n]["Version"])}); if(!ok) {{install.packages({json.dumps(str(archives[n]))},repos=NULL,type="source",lib={json.dumps(str(lib))});stopifnot(requireNamespace({json.dumps(n)},quietly=TRUE))}};cat("VERIFIED {n}\\n")'
 result=subprocess.run([str(base/'runtime/Rscript-clean433'),'--vanilla','-e',code],env=env,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True)
 print(result.stdout,flush=True)
 if result.returncode:raise SystemExit(result.returncode)
 if '* installing' in result.stdout:built+=1
 if built>=int(sys.argv[1] if len(sys.argv)>1 else 8):print('BATCH COMPLETE',flush=True);break
