<#
    deploy-over-xmsg.ps1 - carry TESTUI to the machine over XMSG/COSMOS.

    THIS IS THE NORMAL ROUTE. Nothing is stopped, no disk image is touched, the
    machines keep running. The sync daemon holds ONE link open and carries whatever
    is dropped into the sync-out folder; you then compile from a terminal.

    WHY A HELD LINK AND NOT A PUSH PER BUILD. A one-shot --push ends by sending
    DISC, and a link teardown is where XMSG dies - push-then-compile killed XMSG
    fourteen times out of fourteen. With the link held open the same compile ran
    through and the machine was still alive afterwards.

    THE 13-CHARACTER CEILING, WHICH THIS SCRIPT ENFORCES AND THE SYNC LAYER DOES
    NOT. The transfer packs the specification, an apostrophe and the access letter
    into a 15-byte QFORM string, so the spec may be at most 13 characters
    INCLUDING its two quotes - 11 characters of NAME:TYPE. That is why the screen
    runtime is carried as INTRF1B:BRF and not INTERF-1B:BRF, which is 15 with
    quotes and is refused before a byte goes out. Nothing in Xmsg.Sync checks
    this, so a too-long name would be attempted and fail out on the wire.

    USAGE

      .\deploy-over-xmsg.ps1                  # stage the files, print the daemon command
      .\deploy-over-xmsg.ps1 -StartDaemon     # stage, then start the daemon in a visible window
      .\deploy-over-xmsg.ps1 -SourceOnly      # only TESTUI:PLNC and TESTUI:MODE (the rebuild loop)
#>

[CmdletBinding()]
param(
    # Stage only the two files that change while iterating. The screen library is
    # carried once and then stays put.
    [switch] $SourceOnly,

    # Start the sync daemon afterwards, in a window you can watch.
    [switch] $StartDaemon,

    # How long the daemon should run, in seconds. Give it a real window.
    [int] $DaemonSeconds = 3600,

    # Which topology to dial. The HDLC route to D100 is the default.
    [string] $Topology = 'topology-d19999-hdlc-server.json',

    [string] $PeerHost = '127.0.0.1',
    [int]    $PeerPort = 10362,
    [int]    $LocalSystem = 19999,
    [string] $SyncUser = 'SYSTEM',
    [int]    $SyncTo = 100
)

$ErrorActionPreference = 'Stop'
$here    = Split-Path -Parent $MyInvocation.MyCommand.Path
$xmsg    = Split-Path -Parent $here                       # ...\SINTRAN\XMSG
$syncOut = Join-Path $xmsg 'sync-out'
$runner  = Join-Path $xmsg 'SRC\Xmsg.Live.Runner'

if (-not (Test-Path $syncOut)) { throw "sync-out folder not found at $syncOut" }

# The four files, and the name each lands under. A local NAME.TYPE becomes
# NAME:TYPE on the machine, which is why the local names are already short.
$all = @(
    @{ Local = 'TESTUI.PLNC';  Nd = 'TESTUI:PLNC'; Always = $true;  What = 'the program source' }
    @{ Local = 'TESTUI.MODE';  Nd = 'TESTUI:MODE'; Always = $true;  What = 'the build job' }
    @{ Local = 'SCREEN.SYMB';  Nd = 'SCREEN:SYMB'; Always = $false; What = 'PLANC-SCREEN-H interface' }
    @{ Local = 'INTRF1B.BRF';  Nd = 'INTRF1B:BRF'; Always = $false; What = 'PLANC-SCREEN-H 1-bank runtime' }
)
$files = if ($SourceOnly) { $all | Where-Object { $_.Always } } else { $all }

# ---------------------------------------------------------------------------
# Check the ceiling BEFORE anything is copied. A refusal on the wire is slow to
# diagnose and looks like a transport fault; this is instant and says why.
# ---------------------------------------------------------------------------
$tooLong = 0
foreach ($f in $files) {
    $withQuotes = $f.Nd.Length + 2
    if ($withQuotes -gt 13) {
        Write-Host ("  {0} is {1} characters, {2} with quotes - OVER THE 13 LIMIT" -f $f.Nd, $f.Nd.Length, $withQuotes) -ForegroundColor Red
        $tooLong++
    }
}
if ($tooLong -gt 0) {
    Write-Host "`nShorten the name(s) above and update the LOAD line in TESTUI.MODE to match." -ForegroundColor Red
    exit 1
}

Write-Host "Staging into $syncOut"
Write-Host ""
foreach ($f in $files) {
    $src = Join-Path $here $f.Local
    if (-not (Test-Path $src)) { throw "missing $src - the repo folder is incomplete" }
    Copy-Item $src (Join-Path $syncOut $f.Local) -Force
    $len = (Get-Item $src).Length
    Write-Host ("  {0,-14} -> {1,-14} {2,7} bytes   {3}" -f $f.Local, $f.Nd, $len, $f.What) -ForegroundColor Green
}

Write-Host ""
Write-Host "Staged. NOTHING has been carried yet - the daemon does that." -ForegroundColor Yellow
Write-Host ""

$cmd = "Xmsg.Live.Runner --config $Topology --originate-from-seed " +
       "--sync sync-out --sync-user $SyncUser --sync-to $SyncTo " +
       "$PeerHost $PeerPort $LocalSystem $DaemonSeconds"

if ($StartDaemon) {
    # Visible window, and a working directory - lab processes are watched, never hidden.
    Write-Host "Starting the sync daemon in its own window..." -ForegroundColor Cyan
    Write-Host "  $cmd"
    Start-Process -FilePath 'dotnet' `
                  -ArgumentList (@('run', '--project', 'Xmsg.Live.Runner.csproj', '--',
                                   '--config', $Topology, '--originate-from-seed',
                                   '--sync', $syncOut, '--sync-user', $SyncUser, '--sync-to', $SyncTo,
                                   $PeerHost, $PeerPort, $LocalSystem, $DaemonSeconds)) `
                  -WorkingDirectory $runner `
                  -WindowStyle Normal
    Write-Host "Started. Watch that window - it says what it carries." -ForegroundColor Cyan
}
else {
    Write-Host "Run the daemon from $runner :"
    Write-Host ""
    Write-Host "    $cmd" -ForegroundColor Cyan
    Write-Host ""
    Write-Host "  or re-run this script with -StartDaemon."
}

Write-Host ""
Write-Host "NOT --announce-restart and NOT --resync-hard. Both are known-harmful and"
Write-Host "poison the conversation; --originate-from-seed is what works."
Write-Host ""
Write-Host "Then on the machine, and CHECK EACH STEP - every one lies in its own way:"
Write-Host "    @FILE-STATISTICS TESTUI:PLNC,,     byte count must match the repo file"
Write-Host "    @MODE TESTUI:MODE,,"
Write-Host "    read what LIST-ENTRIES-UNDEFINED printed - an undefined entry does NOT"
Write-Host "      fail the build, it produces a program that runs and misbehaves"
Write-Host "    check TESTUI:LIST has no *** line AND reaches the last source line"
Write-Host "    @TESTUI"
