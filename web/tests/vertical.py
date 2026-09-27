"""Real process-level test. Run at repo root; no engine mocks or real research data."""
import json, os, pathlib, subprocess, tempfile, time, urllib.request, urllib.parse, socket, re
repo = pathlib.Path.cwd()
rscript = os.environ.get("SRMA_RSCRIPT", "Rscript")
data = pathlib.Path(tempfile.mkdtemp(prefix="srma-vertical-"))
env = os.environ.copy()
env.update(SRMA_WEB_DATA=str(data), SRMA_ENGINE_ROOT=str(repo), SRMA_RSCRIPT=rscript)
with socket.socket() as sock:
    sock.bind(("127.0.0.1", 0)); port = sock.getsockname()[1]
env["SRMA_WEB_PORT"] = str(port)
def run(action):
    subprocess.run([rscript,"--vanilla","web/tests/vertical.R",action],env=env,check=True,timeout=60)
run("prepare")
with (data/"app.log").open("w") as log:
    app = subprocess.Popen([rscript,"--vanilla","web/bin/app.R"],env=env,stdout=log,stderr=log)
    try:
        for _ in range(80):
            if app.poll() is not None: raise RuntimeError("Shiny exited before serving")
            try:
                response = urllib.request.urlopen(f"http://127.0.0.1:{port}",timeout=1)
                html = response.read()
                assert response.status == 200 and b"Research workspace" in html
                break
            except OSError: time.sleep(.1)
        else: raise RuntimeError("Shiny did not start")
        assets = set(re.findall(rb'(?:src|href)="([^"#]+\.(?:js|css)(?:\?[^" ]*)?)"', html))
        for asset in assets:
            url=urllib.parse.urljoin(f"http://127.0.0.1:{port}/",asset.decode())
            assert urllib.request.urlopen(url,timeout=5).status==200
        print(f"PASS: actual Shiny application HTTP 200; {len(assets)} JS/CSS assets available",flush=True)
        run("submit")
    finally:
        app.terminate();app.wait(timeout=10)
# The application and submitter have BOTH exited before workers start.
logs=[];workers=[]
try:
    for i in range(2):
        stream=(data/f"worker-{i}.log").open("w");logs.append(stream)
        workers.append(subprocess.Popen([rscript,"--vanilla","web/bin/worker.R","--once"],env=env,stdout=stream,stderr=stream))
    for worker in workers: assert worker.wait(timeout=60)==0
finally:
    for worker in workers:
        if worker.poll() is None: worker.terminate();worker.wait(timeout=10)
    for stream in logs:stream.close()
run("verify")
evidence=json.loads((data/"vertical-evidence.json").read_text())
evidence.update(application_http_status=200,application_exited_before_execution=True,parallel_workers=2)
(repo/"web/evidence/vertical-slice.json").write_text(json.dumps(evidence,indent=2)+"\n")
# Only synthetic engine outputs, no private execution paths or logs in deliverables.
for path in (data/"downloads").iterdir():
    (repo/"web/evidence"/path.name).write_bytes(path.read_bytes())
print("PASS: process-level vertical slice evidence recorded",flush=True)
