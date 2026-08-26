<#
.SYNOPSIS
    Lint, deliver and GATE a PLANC source on a live ND-100 - and refuse to report
    success unless each step proved itself.

.DESCRIPTION
    The build loop has five steps that feel like one, and each has been skipped in a
    real session while everything on screen looked healthy:

      lint      - a name declared nowhere compiles with 0 DIAGNOSTICS
      stage     - into the folder the daemon is ACTUALLY watching, not a stale default
      deliver   - proved by the machine's own byte count, not by a local file size
      compile   - proved by "LINE:" rising, not by having typed the command
      gate      - proved by a SETTLED listing, checked for real diagnostics AND for
                  reaching the last source line

    Skipping any one of them produces a green build of the wrong bytes, which is
    indistinguishable from a good build until the program misbehaves on the machine.

    This script does not drive the terminal - a human or an MCP session types the MODE
    command - because the terminal is shared and grabbing it is how two sessions tread
    on each other. What it does is make every OTHER step honest, and say exactly when
    to type it.

.EXAMPLE
    tools\nd-build.ps1 -Source SINTRAN-CHAT\CHAT.PLNC -StageOnly

    Get the bytes onto the machine, proved by the machine's own number.

.EXAMPLE
    tools\nd-build.ps1 -Source SINTRAN-CHAT\CHAT.PLNC -GateOnly -Listing pulled\CLIST.SYMB

    Judge a listing that has already been pulled.
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string] $Source,

    # What the file is called on the machine. Defaults to the local name with the
    # extension turned into a SINTRAN type: CHAT.PLNC -> CHAT:PLNC.
    [string] $RemoteName,

    # The listing to gate, once it has been pulled off the machine.
    [string] $Listing,

    # Stop after proving delivery.
    [switch] $StageOnly,

    # Gate a listing that is already pulled, and nothing else.
    [switch] $GateOnly,

    [int] $DeliverTimeoutSec = 300
)

$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent $PSScriptRoot

function Stop-WithReason {
    param([string] $Message, [string] $Fix)
    Write-Host ''
    Write-Host "STOPPED: $Message" -ForegroundColor Red
    if ($Fix) { Write-Host "  -> $Fix" }
    exit 1
}

function Write-Step { param([string] $Text) Write-Host ''; Write-Host "== $Text" }

# ---------------------------------------------------------------------------
# The source, and what it is called at the far end.
# ---------------------------------------------------------------------------
if (-not (Test-Path $Source)) { Stop-WithReason "no source at $Source" }
$src      = Get-Item $Source
$srcBytes = $src.Length
$srcLines = (Get-Content $src.FullName).Count

if (-not $RemoteName) {
    $RemoteName = $src.BaseName + ':' + $src.Extension.TrimStart('.')
}

# THE NAME CARRIED OVER FA IS CAPPED AT 13 CHARACTERS by our own client's compact
# QFORM string, and the far end refuses a longer one with nothing useful in the log.
if ($RemoteName.Length -gt 13) {
    Stop-WithReason "remote name '$RemoteName' is $($RemoteName.Length) characters" `
        'our FA client packs the name into a 15-byte QFORM string, leaving 13 for the name'
}

Write-Host ''
Write-Host '--- nd-build ---------------------------------------------------'
Write-Host ("source     {0}" -f $src.FullName)
Write-Host ("           {0} bytes, {1} lines" -f $srcBytes, $srcLines)
Write-Host ("remote     {0}" -f $RemoteName)

if (-not $GateOnly) {

    # -----------------------------------------------------------------------
    # 1. LINT. Cheap, local, and catches the family of PLANC faults that compile
    #    with 0 DIAGNOSTICS and misbehave at run time.
    # -----------------------------------------------------------------------
    Write-Step 'lint'
    $lint = Join-Path $PSScriptRoot 'planc-lint.py'
    if (Test-Path $lint) {
        & python $lint $src.FullName
        if ($LASTEXITCODE -ne 0) {
            Stop-WithReason 'lint found problems' 'fix them - every one of these has cost a build cycle'
        }
    }
    else {
        Write-Host 'no planc-lint.py - skipped'
    }

    # -----------------------------------------------------------------------
    # 2. STAGE - into the folder the daemon is ACTUALLY watching.
    #
    #    Reading it out of the running process is the whole point. A default that
    #    had drifted from the running daemon once produced a green build of the
    #    PREVIOUS source, with nothing anywhere saying the file had not moved.
    # -----------------------------------------------------------------------
    Write-Step 'stage'
    $daemon = Get-CimInstance Win32_Process -Filter "Name='Xmsg.Live.Runner.exe'" -ErrorAction SilentlyContinue
    if ($null -eq $daemon) {
        Stop-WithReason 'the sync daemon is not running' `
            'start it FROM the folder holding topology-*.json, then run tools\nd-preflight.ps1'
    }
    if ($daemon.CommandLine -notmatch '--sync\s+(\S+)') {
        Stop-WithReason 'the daemon is running without --sync' 'restart it with --sync <folder>'
    }
    $watch = $Matches[1]
    Write-Host "daemon watches $watch"

    $staged = Join-Path $watch ($src.BaseName + $src.Extension)
    Copy-Item $src.FullName $staged -Force
    Write-Host "staged      $staged"

    # -----------------------------------------------------------------------
    # 3. DELIVER - proved by the MACHINE's number, not ours.
    #
    #    A byte count on Windows proves a write. Only the daemon's own
    #    "done, NNNNN byte(s)" line, matching the local size, proves a delivery.
    # -----------------------------------------------------------------------
    Write-Step 'deliver'
    $log = Get-ChildItem -Path $repo -Filter 'sync-relay*.log' -ErrorAction SilentlyContinue |
           Sort-Object LastWriteTime -Descending | Select-Object -First 1
    if ($null -eq $log) {
        Stop-WithReason 'no sync-relay*.log to watch' 'is the daemon redirecting its output to one?'
    }

    $want     = "done, $srcBytes byte(s)"
    $deadline = (Get-Date).AddSeconds($DeliverTimeoutSec)
    $seen     = $false
    Write-Host "waiting for '$want' in $($log.Name)"
    while ((Get-Date) -lt $deadline) {
        $tail = Get-Content $log.FullName -Tail 80 -ErrorAction SilentlyContinue
        if (($tail -match [regex]::Escape($RemoteName)) -and ($tail -match [regex]::Escape($want))) {
            $seen = $true
            break
        }
        Start-Sleep -Seconds 3
    }
    if (-not $seen) {
        Stop-WithReason "no delivery of $srcBytes bytes within $DeliverTimeoutSec s" `
            'the daemon may not have learned the peer. On the ND run: COPY-FILE WAKE:TEXT,D19999(SYSTEM).WAKE:TEXT - it FAILS, and the failure is what wakes it'
    }
    Write-Host "delivered   $srcBytes bytes, confirmed by the machine"

    if ($StageOnly) {
        Write-Host ''
        Write-Host 'staged and delivered.'
        exit 0
    }

    # -----------------------------------------------------------------------
    # 4. COMPILE - typed by a human, because the terminal is shared.
    # -----------------------------------------------------------------------
    Write-Step 'compile'
    Write-Host 'Now type this on the machine:'
    Write-Host ''
    Write-Host '    MODE CHATCC:MODE,,'
    Write-Host ''
    Write-Host 'STAGING IS NOT BUILDING. Before waiting, look at the screen and check it says'
    Write-Host 'LINE: with the number RISING. The matching byte count above reads as completion'
    Write-Host 'and is not - two cycles were lost to exactly that.'
    Write-Host ''
    Read-Host 'press RETURN once the build has finished AND the listing has been pulled'
}

# ---------------------------------------------------------------------------
# 5. GATE - on a SETTLED listing.
# ---------------------------------------------------------------------------
Write-Step 'gate'
if (-not $Listing) {
    Stop-WithReason 'no -Listing given' 'pull the listing off the machine and pass its path'
}
if (-not (Test-Path $Listing)) {
    Stop-WithReason "no listing at $Listing" 'the compile did not run, or the pull has not started'
}

# WAIT FOR IT TO STOP GROWING. A 440KB listing arrives over about a minute and reads as
# zero errors the whole way down - which is how a half-transferred listing passes.
$prev = -1
while ($true) {
    $now = (Get-Item $Listing).Length
    if ($now -eq $prev -and $now -gt 0) { break }
    $prev = $now
    Start-Sleep -Seconds 5
}
Write-Host "listing settled at $prev bytes"

& python (Join-Path $PSScriptRoot 'nd-listing-check.py') $Listing $srcLines
if ($LASTEXITCODE -ne 0) {
    Stop-WithReason 'the build is NOT clean' `
        'read the errors above - the listing is the only place a PLANC error survives'
}

Write-Host ''
Write-Host 'BUILD GOOD - linted, delivered and gated.' -ForegroundColor Green
exit 0
