$ErrorActionPreference = "Stop"

$RepoRoot = Split-Path -Parent $PSScriptRoot
$SourceSkills = Join-Path $RepoRoot "skills"

$ClaudeRoot = Join-Path $env:USERPROFILE ".claude"
$ClaudeSkills = Join-Path $ClaudeRoot "skills"

$AgentsRoot = Join-Path $env:USERPROFILE ".agents"
$CodexSkills = Join-Path $AgentsRoot "skills"

Write-Host "Repository root: $RepoRoot"
Write-Host "Skill source: $SourceSkills"

New-Item -ItemType Directory -Force -Path $ClaudeRoot | Out-Null
New-Item -ItemType Directory -Force -Path $AgentsRoot | Out-Null

function New-SkillJunction {
    param(
        [Parameter(Mandatory)]
        [string]$LinkPath,

        [Parameter(Mandatory)]
        [string]$TargetPath
    )

    if (Test-Path $LinkPath) {
        Write-Host "Already exists: $LinkPath"
        return
    }

    New-Item `
        -ItemType Junction `
        -Path $LinkPath `
        -Target $TargetPath | Out-Null

    Write-Host "Created junction:"
    Write-Host "  $LinkPath"
    Write-Host "  -> $TargetPath"
}

New-SkillJunction `
    -LinkPath $ClaudeSkills `
    -TargetPath $SourceSkills

New-SkillJunction `
    -LinkPath $CodexSkills `
    -TargetPath $SourceSkills

$HarnessRoot = Join-Path $RepoRoot "harnesses\srma-harness"
$VenvPath = Join-Path $HarnessRoot ".venv"

if (Test-Path (Join-Path $HarnessRoot "requirements.txt")) {
    if (-not (Test-Path $VenvPath)) {
        python -m venv $VenvPath
    }

    $PythonExe = Join-Path $VenvPath "Scripts\python.exe"

    & $PythonExe -m pip install --upgrade pip
    & $PythonExe -m pip install -r `
        (Join-Path $HarnessRoot "requirements.txt")
}

Write-Host ""
Write-Host "Installation completed."
Write-Host "Restart Claude Code and Codex if the skills are not visible."

#입력: powershell -ExecutionPolicy Bypass -File .\scripts\install-windows.ps1