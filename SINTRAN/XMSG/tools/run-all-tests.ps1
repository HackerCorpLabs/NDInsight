<#
.SYNOPSIS
    Builds and runs every XMSG test project, and treats a SILENT project as a failure.

.DESCRIPTION
    Why this exists, and why it is not a one-line `dotnet test`:

    On 2026-08-17 an ad-hoc loop reported "0 failing, 177 passed" after a wide edit and looked
    perfectly healthy. It was not. Nine of the eleven projects had failed to BUILD, and the loop
    only counted projects that printed a "Passed!/Failed!" summary - so those nine were skipped in
    silence rather than counted. The 177 was the two projects that still compiled, and a broken tree
    was one commit away from going in on a green light.

    So the rule this script enforces is: a project that produces NO result is a FAILURE. Absence of
    a red line is not evidence of green. It also builds everything first, because a build error is
    the failure most likely to be mistaken for "nothing to run".

.PARAMETER Filter
    Optional xunit filter passed through to `dotnet test --filter`.

.PARAMETER SkipBuild
    Skip the explicit build pass. Only for a quick re-run when nothing has changed.

.EXAMPLE
    .\run-all-tests.ps1
    Build everything, run all eleven projects, print a true total.

.EXAMPLE
    .\run-all-tests.ps1 -Filter "FullyQualifiedName~Tad"
    The same, restricted to the TAD tests.

.NOTES
    Leaves no build server behind: finishes with `dotnet build-server shutdown`, because a leftover
    MSBuild node holds the DLLs and the NEXT run then silently tests stale code.
#>
[CmdletBinding()]
param(
    [string] $Filter,
    [switch] $SkipBuild
)

$ErrorActionPreference = 'Continue'

$src = Join-Path (Split-Path $PSScriptRoot -Parent) 'SRC'
if (-not (Test-Path $src)) {
    Write-Error "SRC not found at $src"
    exit 2
}

$projects = Get-ChildItem $src -Filter '*.Tests.csproj' -Recurse -Depth 2 | Sort-Object Name
if ($projects.Count -eq 0) {
    Write-Error "no *.Tests.csproj found under $src - refusing to report success on an empty run"
    exit 2
}

Write-Host "XMSG test run: $($projects.Count) project(s) under $src"
Write-Host ''

# Build first. A build error is the failure most easily mistaken for "nothing happened".
if (-not $SkipBuild) {
    Write-Host 'Building...'
    $buildFailed = 0
    foreach ($p in $projects) {
        $out = & dotnet build $p.FullName -v q --nologo 2>&1 | Out-String
        if ($out -match '(?m)^\s*(\d+)\s+Error\(s\)' -and [int]$Matches[1] -gt 0) {
            $buildFailed++
            Write-Host ("  BUILD FAILED  " + $p.BaseName) -ForegroundColor Red
            ($out -split "`n" | Where-Object { $_ -match ': error ' } | Select-Object -First 3) |
                ForEach-Object { Write-Host ("      " + $_.Trim()) -ForegroundColor Red }
        }
    }

    if ($buildFailed -gt 0) {
        Write-Host ''
        Write-Host "BUILD FAILED in $buildFailed project(s) - not running tests." -ForegroundColor Red
        & dotnet build-server shutdown | Out-Null
        exit 1
    }

    Write-Host '  all projects built.'
    Write-Host ''
}

$silent = 0
$failed = 0
$passedTotal = 0

foreach ($p in $projects) {
    $args = @('test', $p.FullName, '--nologo', '-v', 'q')
    if ($Filter) { $args += @('--filter', $Filter) }

    $out = & dotnet @args 2>&1 | Out-String
    $summary = ($out -split "`n" | Where-Object { $_ -match 'Passed!|Failed!' } | Select-Object -First 1)

    if (-not $summary) {
        # THE POINT OF THIS SCRIPT. No summary means the project never ran - a build break, a
        # crashed host, a missing runner. Counting it as "not failing" is how a broken tree reads
        # as green.
        $silent++
        Write-Host ("  NO RESULT     " + $p.BaseName + "  (did not run - treat as FAILED)") -ForegroundColor Red
        continue
    }

    if ($summary -match 'Failed!') {
        $failed++
        Write-Host ("  FAILED        " + $p.BaseName) -ForegroundColor Red
    }

    if ($summary -match 'Passed:\s+(\d+)') { $passedTotal += [int]$Matches[1] }
}

Write-Host ''
Write-Host "projects: $($projects.Count)   failed: $failed   no result: $silent   tests passed: $passedTotal"

# Never leave a build host of ours behind: it holds the DLLs, and the next run then tests stale code
# while reporting green.
& dotnet build-server shutdown | Out-Null

if ($failed -gt 0 -or $silent -gt 0) {
    Write-Host 'RUN NOT CLEAN' -ForegroundColor Red
    exit 1
}

Write-Host 'all green' -ForegroundColor Green
exit 0
