<#
.SYNOPSIS
    Start the sync daemon - ONE of it, into ONE log name. Refuses to start a second.

.DESCRIPTION
    MEASURED 2026-08-29, from the 19 sync-relay*.log files this repo accumulated between
    the 25th and the 28th of August:

      - no two daemons ever ran at once. Every run began after the previous one ended.
        ONE IS ENOUGH, and one was all that ever ran - the sprawl was in the NAMES;
      - six runs ended at exactly 3h, 4h, 2h, 3h, 8h and 6h after starting. Those are
        not coincidences, they are the run window the daemon is given as its last
        positional argument. It dies by design, and a dead daemon gets started again -
        which is where 19 runs in three days came from;
      - each restart was typed by hand, and each one picked the next free number so as
        not to overwrite the previous run's evidence. Nothing required that. Both
        tools/nd-build.ps1 and tools/nd-preflight.ps1 just take the NEWEST
        sync-relay*.log, so they never cared what it was called.

    So this script does the two things hand-typing could not:

      1. REFUSES to start a second daemon. Two runners on node 19999 make every push
         time out and look like a dead file server on the machine. If one is already
         up, this prints its PID, its folders and how much window it has left, then
         exits without touching it. It never kills anything.
      2. Always writes sync-relay.log, archiving the previous one to
         sync-relay-<when-it-ran>.log first. Nothing is lost and the live log has a
         fixed, findable name.

    It also starts the daemon from the folder holding topology-*.json, because --config
    resolves against the CURRENT DIRECTORY: started from the wrong folder the runner
    quietly falls back to built-in defaults and dials HDLC at an Ethernet hub. This
    script then READS THE LOG BACK to prove which topology actually loaded, rather than
    reporting success because a process object came back.

.EXAMPLE
    .\start-relay.ps1
    Six-hour window, watching sync-relay, pulling into sync-pull, writing to D100.

.EXAMPLE
    .\start-relay.ps1 -WindowSeconds 28800 -SyncFolder sync-out
    An eight-hour run staging out of a different folder.

.NOTES
    The daemon's output goes to the log, not to a window, because the log is what
    nd-build.ps1 and nd-preflight.ps1 read. Watch it live with:
        Get-Content .\sync-relay.log -Wait -Tail 20
#>
[CmdletBinding()]
param(
    # How many seconds the daemon should run. This is the LAST positional argument the
    # runner takes, and nd-preflight.ps1 parses it back out of the command line to warn
    # when the window is nearly up. It must stay 3 to 6 digits for that check to see it.
    [ValidateRange(100, 999999)]
    [int] $WindowSeconds = 21600,

    # The folder the daemon carries TO the machine. A file dropped here is delivered.
    [string] $SyncFolder = 'sync-relay',

    # The folder a .req is dropped in to fetch a file FROM the machine. Without this the
    # build gate cannot pull a listing, and no listing means no build verification.
    [string] $PullFolder = 'sync-pull',

    # Which topology to load. Resolved against the runner's own folder.
    [string] $Topology = 'topology-d19999.json',

    # The hub, our node number, and the SINTRAN user and node we write as.
    [string] $HubHost = '127.0.0.1',
    [int]    $HubPort = 5010,
    [int]    $OwnNode = 19999,
    [string] $SyncUser = 'SYSTEM',
    [int]    $SyncToNode = 100
)

$ErrorActionPreference = 'Stop'

$xmsg      = Split-Path -Parent $PSScriptRoot                  # ...\SINTRAN\XMSG
$runnerDir = Join-Path $xmsg 'SRC\Xmsg.Live.Runner'
$exe       = Join-Path $runnerDir 'bin\Release\net9.0\Xmsg.Live.Runner.exe'
$log       = Join-Path $xmsg 'sync-relay.log'
$errLog    = Join-Path $xmsg 'sync-relay.err'

function Fail([string] $what, [string] $fix) {
    Write-Host ''
    Write-Host "STOPPED: $what" -ForegroundColor Red
    if ($fix) { Write-Host "         $fix" -ForegroundColor Yellow }
    exit 1
}

# ---------------------------------------------------------------------------
# 1. IS ONE ALREADY RUNNING?  This is the whole point of the script.
#
#    Two runners on the same node number is not a tidiness problem. The machine
#    answers one of them and the other's pushes time out, which reads on the ND
#    as a dead file server - a fault that has cost hours before.
# ---------------------------------------------------------------------------
$running = @(Get-CimInstance Win32_Process -Filter "Name='Xmsg.Live.Runner.exe'" -ErrorAction SilentlyContinue)
if ($running.Count -gt 0) {
    Write-Host ''
    Write-Host 'A SYNC DAEMON IS ALREADY RUNNING - not starting a second one.' -ForegroundColor Yellow
    foreach ($p in $running) {
        $cl    = $p.CommandLine
        $watch = if ($cl -match '--sync\s+(\S+)')      { $Matches[1] } else { 'NOT SET' }
        $pull  = if ($cl -match '--sync-pull\s+(\S+)') { $Matches[1] } else { 'NOT SET - the build gate cannot pull a listing' }
        $win   = if ($cl -match '\s(\d{3,6})\s*$')     { [int]$Matches[1] } else { 0 }
        Write-Host ''
        Write-Host "  PID        $($p.ProcessId)"
        Write-Host "  started    $($p.CreationDate)"
        Write-Host "  watches    $watch"
        Write-Host "  pulls from $pull"
        if ($win -gt 0) {
            $endsAt = $p.CreationDate.AddSeconds($win)
            $left   = [int]($endsAt - (Get-Date)).TotalMinutes
            $colour = if ($left -lt 30) { 'Red' } else { 'Gray' }
            Write-Host "  window     $left min left (ends $($endsAt.ToString('HH:mm')))" -ForegroundColor $colour
        }
    }
    Write-Host ''
    Write-Host 'Use it as it stands. If you want a fresh one, stop that PID yourself first,'
    Write-Host 'then run this again - this script never kills a process it did not start.'
    exit 0
}

# ---------------------------------------------------------------------------
# 2. THE THINGS THAT MUST EXIST BEFORE, not halfway through a six-hour run.
# ---------------------------------------------------------------------------
if (-not (Test-Path $exe)) {
    Fail "no runner at $exe" 'build it: dotnet build -c Release SRC\Xmsg.Live.Runner'
}
$configPath = Join-Path $runnerDir $Topology
if (-not (Test-Path $configPath)) {
    $have = (Get-ChildItem $runnerDir -Filter 'topology-*.json' | ForEach-Object { $_.Name }) -join ', '
    Fail "no topology at $configPath" "pick one of: $have"
}
# Both folders are CREATED if missing. A daemon watching a folder that does not exist
# carries nothing and says nothing about it.
$syncPath = Join-Path $xmsg $SyncFolder
$pullPath = Join-Path $xmsg $PullFolder
foreach ($d in @($syncPath, $pullPath)) {
    if (-not (Test-Path $d)) {
        New-Item -ItemType Directory -Path $d -Force | Out-Null
        Write-Host "created    $d"
    }
}

# ---------------------------------------------------------------------------
# 3. ARCHIVE THE OLD LOG, then always write the same name.
#
#    Named for WHEN THAT RUN HAPPENED, not for now - so the archive reads as a
#    history instead of a pile of restarts.
# ---------------------------------------------------------------------------
if ((Test-Path $log) -and ((Get-Item $log).Length -gt 0)) {
    $when    = (Get-Item $log).LastWriteTime.ToString('yyyyMMdd-HHmm')
    $archive = Join-Path $xmsg "sync-relay-$when.log"
    if (Test-Path $archive) { Remove-Item $archive -Force }
    Move-Item $log $archive -Force
    Write-Host "archived   $(Split-Path -Leaf $archive)"
}

# ---------------------------------------------------------------------------
# 4. START IT - from the runner's OWN folder, or --config finds nothing and the
#    daemon silently falls back to built-in defaults.
# ---------------------------------------------------------------------------
$runnerArgs = @(
    '--config', $configPath
    '--originate-from-seed'
    '--sync', $syncPath
    '--sync-pull', $pullPath
    '--sync-user', $SyncUser
    '--sync-to', "$SyncToNode"
    $HubHost, "$HubPort", "$OwnNode", "$WindowSeconds"
)

Write-Host ''
Write-Host "starting   $exe"
Write-Host "           window $WindowSeconds s ($([int]($WindowSeconds / 60)) min), ends about $((Get-Date).AddSeconds($WindowSeconds).ToString('HH:mm'))"
Write-Host "           log $log"

$proc = Start-Process -FilePath $exe -ArgumentList $runnerArgs -WorkingDirectory $runnerDir -RedirectStandardOutput $log -RedirectStandardError $errLog -WindowStyle Minimized -PassThru

# ---------------------------------------------------------------------------
# 5. PROVE IT - by reading the daemon's own words back, not by the fact that a
#    process object came back. It prints its full command line and which config it
#    loaded, so both can be checked rather than assumed.
# ---------------------------------------------------------------------------
$deadline = (Get-Date).AddSeconds(20)
$head     = @()
while ((Get-Date) -lt $deadline) {
    Start-Sleep -Milliseconds 500
    if ($proc.HasExited) {
        Write-Host ''
        Write-Host "the daemon exited immediately with code $($proc.ExitCode)" -ForegroundColor Red
        if (Test-Path $errLog) { Get-Content $errLog -Tail 20 }
        if (Test-Path $log)    { Get-Content $log    -Tail 20 }
        exit 1
    }
    $head = @(Get-Content $log -TotalCount 12 -ErrorAction SilentlyContinue)
    if ($head -match 'command line:') { break }
}

Write-Host ''
if ($head -match 'using built-in defaults') {
    Write-Host 'BUILT-IN DEFAULTS LOADED - the topology did NOT take.' -ForegroundColor Red
    Write-Host "PID $($proc.Id) is running but will dial the wrong path. Stop it and check $configPath."
    exit 1
}
$loaded = $head | Where-Object { $_ -match 'topology loaded' } | Select-Object -First 1
if (-not $loaded) {
    Write-Host "started PID $($proc.Id), but the log has not yet said which topology it loaded." -ForegroundColor Yellow
    Write-Host "Check it yourself:  Get-Content '$log' -TotalCount 12"
    exit 0
}

Write-Host "running    PID $($proc.Id)" -ForegroundColor Green
Write-Host "           $loaded"
Write-Host "           watching $syncPath -> node $SyncToNode ($SyncUser)"
Write-Host ''
Write-Host "watch it:  Get-Content '$log' -Wait -Tail 20"
Write-Host "check all: .\tools\nd-preflight.ps1"
