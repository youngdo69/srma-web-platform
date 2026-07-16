$ErrorActionPreference = "Stop"

$RepoRoot = Split-Path -Parent $PSScriptRoot

Set-Location $RepoRoot

git status
git pull --ff-only

$HarnessRoot = Join-Path $RepoRoot "harnesses\srma-harness"
$PythonExe = Join-Path $HarnessRoot ".venv\Scripts\python.exe"
$Requirements = Join-Path $HarnessRoot "requirements.txt"

if ((Test-Path $PythonExe) -and (Test-Path $Requirements)) {
    & $PythonExe -m pip install -r $Requirements
}

Write-Host "Toolkit update completed."

#입력: powershell -ExecutionPolicy Bypass -File .\scripts\update-windows.ps1