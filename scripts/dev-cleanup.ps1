# Shut down .NET build servers and report any host processes still running.
#
# Why: `dotnet build` and `dotnet test` each leave an MSBuild node and a
# VBCSCompiler behind, and those HOLD THE DLLs. A locked DLL makes the next
# build silently keep the old binary, so the tests report GREEN against stale
# code. That has actually happened here - it is recorded in memory as
# `nd500-suite-two-flaky-tests-and-locked-dll`.
#
# IMPORTANT - this script does NOT kill anything by default. Most stray
# dotnet.exe on this box belong to Unity, to other repos, or to Ronny's own
# running apps, and killing those breaks his work. It LISTS them with their
# command lines so a human can decide. Use -KillMine to kill only processes
# whose command line matches a repo you name.
#
# Usage:
#   .\scripts\dev-cleanup.ps1                       # shutdown build servers, then list
#   .\scripts\dev-cleanup.ps1 -KillMine RetroCore   # also kill hosts whose cmdline mentions RetroCore

param(
    [string]$KillMine = ""
)

$ErrorActionPreference = 'Continue'

Write-Output "=== dotnet build-server shutdown ==="
try {
    $out = & dotnet build-server shutdown 2>&1
    Write-Output ($out -join "`n")
}
catch {
    Write-Output "  (dotnet not on PATH, or shutdown failed: $_)"
}

Write-Output ""
Write-Output "=== .NET host processes still running ==="

$names = "dotnet.exe", "testhost.exe", "MSBuild.exe", "VBCSCompiler.exe", "testhost.x86.exe"
$filter = ($names | ForEach-Object { "Name='$_'" }) -join " OR "

$procs = Get-CimInstance Win32_Process -Filter $filter -ErrorAction SilentlyContinue
if ($null -eq $procs -or $procs.Count -eq 0) {
    Write-Output "  none - clean."
    exit 0
}

foreach ($p in $procs) {
    $cmd = $p.CommandLine
    if ([string]::IsNullOrWhiteSpace($cmd)) { $cmd = "(command line unavailable)" }
    # Trim so the output stays readable - the full line is often enormous.
    if ($cmd.Length -gt 200) { $cmd = $cmd.Substring(0, 200) + " ..." }
    Write-Output ""
    Write-Output ("  PID {0}  {1}" -f $p.ProcessId, $p.Name)
    Write-Output ("      {0}" -f $cmd)
}

Write-Output ""
Write-Output "CHECK THE COMMAND LINE before killing any of these."
Write-Output "A stray dotnet.exe on this box is usually Unity's or another repo's."

if ([string]::IsNullOrWhiteSpace($KillMine)) {
    Write-Output ""
    Write-Output "Nothing killed (no -KillMine given)."
    exit 0
}

Write-Output ""
Write-Output ("=== killing hosts whose command line mentions '{0}' ===" -f $KillMine)
$killed = 0
foreach ($p in $procs) {
    if ([string]::IsNullOrWhiteSpace($p.CommandLine)) { continue }
    if ($p.CommandLine -notmatch [regex]::Escape($KillMine)) { continue }
    try {
        Stop-Process -Id $p.ProcessId -Force -ErrorAction Stop
        Write-Output ("  killed PID {0} ({1})" -f $p.ProcessId, $p.Name)
        $killed++
    }
    catch {
        Write-Output ("  could not kill PID {0}: {1}" -f $p.ProcessId, $_)
    }
}
Write-Output ("killed {0} process(es)." -f $killed)
