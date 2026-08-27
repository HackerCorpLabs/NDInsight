<#
.SYNOPSIS
    Is the lab actually ready? One command, thirty seconds, before any build or test.

.DESCRIPTION
    Every one of these has been discovered MID-TEST, after the time was already spent:

      - the hub was not running, so no machine could reach any other;
      - the sync daemon's window had elapsed, so a staged file sat there for ever;
      - the daemon was running but had loaded BUILT-IN DEFAULTS, because --config
        resolves against the CURRENT DIRECTORY and it was started from the wrong one;
      - a trunk had aged out, so chat lines went nowhere;
      - the terminal had been logged out by SINTRAN's idle timeout, and every command
        typed after that went into a void while the screen looked perfectly normal.

    This reports all of them and says plainly which are wrong. It changes NOTHING - it
    is a report, so it is always safe to run.

.NOTES
    Machine console ports come from each RetroCore instance's own .ini, never from
    memory: RULE #0. See -IniRoot.
#>
[CmdletBinding()]
param(
    # Where the RetroCore instances live. Each subfolder holds a RetroCore.ini whose
    # "device add TERM" lines name that machine's console port.
    [string] $IniRoot = 'F:\RC\RonnyTest',

    # The hub every machine dials.
    [int]    $HubPort = 5010,

    # How little remaining daemon window is worth warning about.
    [int]    $DaemonWarnMinutes = 30,

    # THE MACHINES THIS LAB ACTUALLY USES. $IniRoot holds a dozen RetroCore instances -
    # other experiments, other eras - and reporting every one of them as a fault buries
    # the three that matter in noise nobody reads. Name them explicitly.
    [string[]] $Machines = @('HDLC1', 'HDLC2', 'HDLC3')
)

$ErrorActionPreference = 'Stop'
$problems = New-Object System.Collections.Generic.List[string]

function Write-Check {
    param([string] $Name, [bool] $Ok, [string] $Detail)
    $mark = if ($Ok) { 'ok  ' } else { 'BAD ' }
    Write-Host ("{0} {1,-22} {2}" -f $mark, $Name, $Detail)
}

Write-Host ''
Write-Host '--- lab preflight ---------------------------------------------'

# ---- the hub -------------------------------------------------------------
$hub = Get-CimInstance Win32_Process -Filter "Name='xmsghub.exe'" -ErrorAction SilentlyContinue
$listening = @(Get-NetTCPConnection -LocalPort $HubPort -State Listen -ErrorAction SilentlyContinue).Count
$hubOk = ($null -ne $hub) -and ($listening -gt 0)
Write-Check 'hub' $hubOk $(if ($hubOk) { "pid $($hub.ProcessId), listening on $HubPort" } else { "NOT running or not listening on $HubPort" })
if (-not $hubOk) {
    $problems.Add("start the hub: xmsghub.exe --port $HubPort")
}
elseif ($hub.CommandLine -match '--capture') {
    # Not a fault, but a capture left running grows a file for ever - it reached 3 MB
    # in an hour once and nobody was reading it.
    Write-Check 'hub capture' $true 'CAPTURING - remember to stop it when the hunt is over'
}

# ---- the sync daemon -----------------------------------------------------
$daemon = Get-CimInstance Win32_Process -Filter "Name='Xmsg.Live.Runner.exe'" -ErrorAction SilentlyContinue
if ($null -eq $daemon) {
    Write-Check 'sync daemon' $false 'NOT running - a staged file will never be carried'
    $problems.Add('start the sync daemon FROM THE FOLDER HOLDING topology-*.json')
}
else {
    $cmd = $daemon.CommandLine

    # The folder it is actually watching. Staging anywhere else is silent.
    $watch = if ($cmd -match '--sync\s+(\S+)') { $Matches[1] } else { '(none)' }
    Write-Check 'daemon --sync' $true $watch

    # ---- CAN THE GATE FETCH AT ALL? -------------------------------------
    # The build gate reads a LISTING, and the only way it gets one is by dropping a
    # .req in the daemon's --sync-pull folder. A daemon started WITHOUT --sync-pull
    # takes those requests nowhere: the .req file just sits there. Nothing errors,
    # nothing logs, and the gate reports "no listing - the compile did not run",
    # which points at the compile instead of at the daemon.
    #
    # MEASURED 2026-08-27: a CHATUI.LIST.req sat unclaimed for TWO DAYS beside a
    # perfectly healthy daemon, and the gate could not run at all.
    #
    # WITHOUT THE GATE THERE IS NO BUILD VERIFICATION - a PLANC source with errors
    # links and runs, so this is not a convenience, it is the check itself.
    $pull = if ($cmd -match '--sync-pull\s+(\S+)') { $Matches[1] } else { $null }
    Write-Check 'daemon --sync-pull' ($null -ne $pull) $(
        if ($pull) { $pull } else { 'NOT SET - the build gate cannot pull a listing' })
    if ($null -eq $pull) {
        $problems.Add('daemon has no --sync-pull, so the build gate cannot fetch a listing - restart it with --sync-pull sync-pull')
    }
    else {
        # A request nobody claimed is the same fault wearing a different hat: the
        # option is set, but this daemon is not the one watching that folder.
        $stale = @(Get-ChildItem -Path $pull -Filter '*.req' -ErrorAction SilentlyContinue |
                   Where-Object { $_.LastWriteTime -lt (Get-Date).AddMinutes(-5) })
        if ($stale.Count -gt 0) {
            Write-Check 'pull requests' $false "$($stale.Count) unclaimed .req older than 5 min - $($stale[0].Name)"
            $problems.Add('unclaimed .req files in the pull folder - the daemon is not servicing it')
        }
    }

    # The window. The last positional argument is the number of seconds it will run.
    $started = $daemon.CreationDate
    $window  = if ($cmd -match '\s(\d{3,6})\s*$') { [int]$Matches[1] } else { 0 }
    if ($window -gt 0) {
        $endsAt = $started.AddSeconds($window)
        $left   = [int]($endsAt - (Get-Date)).TotalMinutes
        $ok     = $left -gt $DaemonWarnMinutes
        Write-Check 'daemon window' $ok "$left min left (ends $($endsAt.ToString('HH:mm')))"
        if (-not $ok) {
            $problems.Add("daemon window has $left min left - restart it before a long build")
        }
    }
}

# ---- did the daemon load a real topology, or its built-in defaults? ------
# --config resolves against the CURRENT DIRECTORY. Started from the wrong folder it
# quietly falls back to defaults and dials HDLC at an Ethernet hub.
$logs = Get-ChildItem -Path (Join-Path $PSScriptRoot '..') -Filter 'sync-relay*.log' -ErrorAction SilentlyContinue |
        Sort-Object LastWriteTime -Descending | Select-Object -First 1
if ($null -ne $logs) {
    $head = Get-Content $logs.FullName -TotalCount 3 -ErrorAction SilentlyContinue
    $usedDefaults = $head -match 'using built-in defaults'
    Write-Check 'daemon topology' (-not $usedDefaults) $(
        if ($usedDefaults) { "BUILT-IN DEFAULTS - started from the wrong folder ($($logs.Name))" }
        else { ($head | Where-Object { $_ -match 'topology loaded' } | Select-Object -First 1) }
    )
    if ($usedDefaults) {
        $problems.Add('restart the daemon with -WorkingDirectory set to the folder holding topology-*.json')
    }
}

# ---- the machines --------------------------------------------------------
# Console ports are READ FROM EACH INI, never remembered.
Write-Host ''
foreach ($name in $Machines) {
    $dir = Get-Item -Path (Join-Path $IniRoot $name) -ErrorAction SilentlyContinue
    if ($null -eq $dir) {
        Write-Check $name $false "no folder at $IniRoot\$name"
        $problems.Add("$name has no instance folder")
        continue
    }
    $ini = Join-Path $dir.FullName 'RetroCore.ini'
    if (-not (Test-Path $ini)) {
        Write-Check $name $false 'no RetroCore.ini'
        $problems.Add("$name has no RetroCore.ini")
        continue
    }

    $port = $null
    foreach ($line in (Get-Content $ini)) {
        if ($line -match 'device\s+add\s+TERM\s+\d+\s+--port=(\d+)') { $port = $Matches[1]; break }
    }
    $running = @(Get-CimInstance Win32_Process -Filter "Name='RetroCore.exe'" -ErrorAction SilentlyContinue |
                 Where-Object { $_.CommandLine -like "*$($dir.Name)*" }).Count -gt 0
    $up = if ($port) { @(Get-NetTCPConnection -LocalPort $port -State Listen -ErrorAction SilentlyContinue).Count -gt 0 } else { $false }

    Write-Check $dir.Name ($running -and $up) "console port $port, process $(if ($running) {'up'} else {'DOWN'})"
    if (-not $running) { $problems.Add("$($dir.Name) is not running") }
}

# ---- verdict -------------------------------------------------------------
Write-Host ''
if ($problems.Count -eq 0) {
    Write-Host 'preflight clean - the lab is ready.'
    exit 0
}
Write-Host "$($problems.Count) thing(s) to deal with first:"
foreach ($p in $problems) { Write-Host "  - $p" }
exit 1
