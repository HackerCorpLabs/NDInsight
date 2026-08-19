# Install the tracked git hooks for this repo (and optionally any other repo).
#
# Hooks normally live in .git/hooks, which is NOT version controlled - so a hook
# put there is invisible to everyone and lost on a fresh clone. Instead we keep
# them in scripts/git-hooks (tracked) and point git's core.hooksPath at that.
#
# Usage:
#   .\scripts\install-git-hooks.ps1                 # this repo
#   .\scripts\install-git-hooks.ps1 -Repo E:\Dev\Repos\Ronny\RetroCore
#
# To undo:  git config --unset core.hooksPath

param(
    [string]$Repo = ""
)

$ErrorActionPreference = 'Stop'

# Default to the repo this script lives in.
if ([string]::IsNullOrWhiteSpace($Repo)) {
    $Repo = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
}

if (-not (Test-Path (Join-Path $Repo ".git"))) {
    Write-Output "NOT A GIT REPO: $Repo"
    exit 1
}

# The hooks themselves always come from THIS repo, even when installing into
# another one, so there is a single copy to maintain.
$hookSource = Join-Path (Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)) "scripts\git-hooks"
if (-not (Test-Path $hookSource)) {
    Write-Output "hook source folder missing: $hookSource"
    exit 1
}

Push-Location $Repo
try {
    git config core.hooksPath $hookSource
    $now = git config --get core.hooksPath
    Write-Output "repo            : $Repo"
    Write-Output "core.hooksPath  : $now"
    Write-Output ""
    Write-Output "hooks now active:"
    Get-ChildItem $hookSource -File | ForEach-Object { Write-Output ("  " + $_.Name) }
}
finally {
    Pop-Location
}
