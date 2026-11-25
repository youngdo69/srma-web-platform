# Windows Installation, Research Workflow and Recovery

This package is for offline source review and a private, loopback Windows evaluation. Use a disposable project and database for acceptance tests.

## 1. Verify and extract

Compare `Get-FileHash -Algorithm SHA256 .\srma-6.3.16-rc1-source.zip` with the detached checksum delivered with the archive. Extract to a new directory with `Expand-Archive -LiteralPath .\srma-6.3.16-rc1-source.zip -DestinationPath C:\SRMA\srma-6.3.16-rc1`. Do not extract over a previous installation. Run the launcher scripts from the new directory. The archive preserves source bytes and the exact lockfiles.

To reproduce the archive byte for byte, use Python 3.12.6 with zlib 1.3.1. From the extracted root run `python .\RC\BUILD_RC.py . C:\SRMA\rebuild.zip`. Use `--verify` against an existing ZIP to validate the inventory and member hashes. The script validates the source against `RC/SOURCE_MANIFEST.json` and refuses to overwrite an existing archive.

## 2. Set up the two R environments

Use Web R 4.3.3 for Shiny and Worker, and Scientific R 4.6.1 for the canonical engine and R/41. Use the versions and packages pinned in `web/renv.lock` and `renv.lock`. A new machine may need approved system build tools for locked source packages, network access to the configured package repository and an administrator-managed package restore.

In PowerShell, from the extracted root:

```powershell
$env:SRMA_ENGINE_ROOT = (Get-Location).Path
$env:SRMA_WEB_MODE = 'development'
$env:SRMA_WEB_DB = 'sqlite'
$env:SRMA_WEB_HOST = '127.0.0.1'
$env:SRMA_WEB_PORT = '3838'
$env:SRMA_WEB_DATA = 'C:\SRMA\private\rc1-evaluation' # Use a new private data directory.
$env:SRMA_WEB_LIBRARY = Join-Path $env:SRMA_ENGINE_ROOT 'web\.library'
$env:SRMA_RSCRIPT = 'C:\Program Files\R\R-4.3.3\bin\Rscript.exe'
$env:SRMA_SCIENTIFIC_RSCRIPT = 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe'
$env:SRMA_R40_ALLOW_PAID = '0'
& $env:SRMA_RSCRIPT --version
& $env:SRMA_SCIENTIFIC_RSCRIPT --version
```

On a new installation, the operator can restore the scientific library via the existing dependency-only entry point. This does not initialize a review, run the scientific workflow, or invoke an AI provider. Use `web/bin/setup.R` for the Web library. Skip both restore commands when the two exact libraries already exist.

```powershell
& $env:SRMA_SCIENTIFIC_RSCRIPT --vanilla -e "options(srma.start_here.environment_only=TRUE); source('START_HERE.R'); cat(.libPaths()[1])"
& $env:SRMA_RSCRIPT --vanilla web/bin/setup.R
```

Set `SRMA_SCIENTIFIC_LIBRARY` to the restored Scientific R library path printed by the first command, or the path to your existing matching library. Run both preflights:

```powershell
& $env:SRMA_RSCRIPT --vanilla web/bin/rc287_preflight.R
& $env:SRMA_RSCRIPT --vanilla web/bin/rc287_science_probe.R
```

Resolve R or lock mismatches before running scientific jobs. If the default user cache is read-only, set `R_USER_CACHE_DIR` to a private writable scratch directory in every shell. The public listener is for a separately accepted deployment. For an approved local PDF reader, use the existing `SRMA_PDFTOTEXT` absolute executable setting for the PDF review route. A missing reader or text layer can leave evidence marked pending. Some figure checks discover `pdftotext` on PATH. Do not substitute PDFs or mark such checks PASS.

## 3. Start and stop

Give each PowerShell window the same root, library, database, and runtime settings. Choose another unused port if needed. App startup initializes the new SQLite schema and checks local readiness. Enable the existing test identity provider only for the isolated loopback evaluation. Its issued credential is private and expires. Keep it out of logs and screenshots.

```powershell
# App shell. Use the test identity only in the isolated local evaluation.
$env:SRMA_WEB_TEST_IDP = '1'
& $env:SRMA_RSCRIPT --vanilla web/bin/test_session.R
& $env:SRMA_RSCRIPT --vanilla web/bin/app.R

# Worker shell. Load the same setup variables first.
& $env:SRMA_RSCRIPT --vanilla web/bin/worker.R

# Optional status check in a third initialized shell.
& $env:SRMA_RSCRIPT --vanilla web/bin/health.R
```

Open `http://127.0.0.1:3838`, or the configured loopback port, and use the local credential. Keep the App and Worker in their own foreground consoles. Stop taking new actions, let active work finish, then use Ctrl+C in each console. Collect private console and operator logs by time, job ID and Version. Redact credentials and research identifiers before sharing them.

## 4. Research workflow

1. Create a new review, prepare the Protocol and search strategy, and record applicable approvals or registration evidence. Import and validate search results, then review title/abstract and full-text evidence with documented exclusions.
2. Finalize Study Selection, check extracted outcomes against the sources, and approve harmonization. Confirm the applicable Narrative or Pairwise analysis plan and complete any required RoB and GRADE decisions. Mixed components use the applicable approved project inputs and the current drafting scope.
3. Confirm scientific reporting, PRISMA, figures and citations. Open the generated manuscript and review Statistical Analysis against the approved execution. Edit a supported prose section, compare the versions and save. The new review version and DOCX are exposed from the same manuscript screen after processing.
4. Review applicable Final QC results. A review DOCX remains downloadable while work is pending. Final author approval uses the existing authenticated workflow and the exact document version. Check the final Submission Gate and download the approved DOCX from the same screen once eligible.

The packaged B2, C1, C2 and C3 guides describe the detailed workflow. Complex inline formatting and citation or equation edits are outside the supported editor scope. Actual changes to analysis settings require the Analysis approval route.

## 5. Manual Chrome acceptance

Use an isolated test project only. Record date, Chrome version and candidate code hash. Open Manuscript, check the current approved source and compare a supported Methods edit. Save a new version and verify automatic selection. Review QC status, then download and hash the review DOCX. Exercise final approval against an eligible test version and use the same screen to download the approved file. Compare Version and SHA-256 with the registered artifact. Confirm that a pending or invalid required check preserves review download and blocks submission. Record failing steps and stop if the source or version differs. Native Chrome acceptance remains NOT_VERIFIED until this procedure actually runs.

## 6. Recovery and rollback

- For SQLite, stop App and Worker before taking a backup. Preserve the entire `SRMA_WEB_DATA` tree as a single consistent snapshot, with SQLite and any WAL/SHM files, versions, approvals, artifact storage and audit logs. Also preserve tracked source hashes, locks and private environment configuration separately.
- For deployment, use the existing consistent database and artifact-store backup procedures. Confirm matching data and source versions and restorable credentials with the operator. Operational PostgreSQL acceptance and the recovery rehearsal are separate from this local RC.
- Recover into a new path. Restore matching database and artifact snapshots together, verify source hashes and the authorized runtime, then check health and a permitted artifact/download in isolation. Do not combine a restored database with artifacts from another version. Preserve the current data tree and its approval history during investigation.
- Roll back by switching to a separately extracted known-good source and its compatible pre-migration snapshot after verification. Leave both data trees in place. A migration, release approval, or production change follows the operator runbook and explicit PM authorization.
