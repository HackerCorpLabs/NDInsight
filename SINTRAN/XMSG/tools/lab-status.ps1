<#
.SYNOPSIS
    Prints the live state of the XMSG lab in one command: which machines are up, how their HDLC
    links are wired right now, whether our relay is running, and which .NET hosts are ours.

.DESCRIPTION
    Written because every working session used to begin by re-deriving this with ad-hoc
    Get-Process / Get-NetTCPConnection calls, and Ronny repeatedly had to say things like
    "d103 is running" or "i had to kill retrocore" to unblock it.

    All facts about the machines come from lab-topology.json beside LAB.md - this script reads
    them, it does not carry its own copy. Change the lab, change the JSON.

    READ-ONLY. It starts nothing, stops nothing and touches no machine state.

    Two conditions it calls out explicitly, because both LOOK healthy and are not:

     - A TCP self-connection: a machine's HDLC dialling a listener that is down can complete a
       connection to ITSELF. On the machine, LIST-LINKS then shows State Run with Sysid equal to
       its OWN system number. Only a restart clears it.
     - An orphan dialler: a machine dialling a port nobody is listening on, which reads as a dead
       link at the SINTRAN end with no clue as to why.

.PARAMETER TopologyPath
    Path to lab-topology.json. Defaults to the copy beside this script's parent folder.

.EXAMPLE
    .\lab-status.ps1
#>
[CmdletBinding()]
param(
    [string] $TopologyPath = (Join-Path (Split-Path $PSScriptRoot -Parent) 'lab-topology.json')
)

$ErrorActionPreference = 'Stop'

if (-not (Test-Path $TopologyPath)) {
    Write-Error "lab-topology.json not found at $TopologyPath - see SINTRAN/XMSG/LAB.md"
    return
}

$topo = Get-Content $TopologyPath -Raw | ConvertFrom-Json

# One snapshot each, so every line below describes the SAME instant rather than drifting as we go.
$procs = @(Get-CimInstance Win32_Process -Filter "Name='RetroCore.exe' OR Name='Xmsg.Live.Runner.exe' OR Name='dotnet.exe' OR Name='testhost.exe' OR Name='MSBuild.exe' OR Name='VBCSCompiler.exe' OR Name='tshark.exe' OR Name='RetroTerm.Desktop.exe'")
$conns = @(Get-NetTCPConnection -ErrorAction SilentlyContinue)

function Get-Proc([int] $processId) {
    foreach ($p in $procs) { if ($p.ProcessId -eq $processId) { return $p } }
    return $null
}

Write-Host ""
Write-Host "XMSG LAB STATUS" -ForegroundColor Cyan
Write-Host ("topology: {0}   (verified {1})" -f $TopologyPath, $topo.verifiedOn) -ForegroundColor DarkGray
Write-Host ""

# ---------------------------------------------------------------- machines
Write-Host "MACHINES" -ForegroundColor Cyan
foreach ($m in $topo.machines) {
    $proc = $null
    foreach ($p in $procs) {
        if ($p.Name -eq 'RetroCore.exe' -and $p.CommandLine -and $p.CommandLine -like "*$($m.folder)*") { $proc = $p; break }
    }

    if ($null -eq $proc) {
        Write-Host ("  {0,-6} DOWN" -f $m.name) -ForegroundColor Red
        Write-Host ("         start: {0}\{1}  (working directory MUST be that folder)" -f $m.folder, $m.exe) -ForegroundColor DarkGray
        continue
    }

    $termUp = $false
    foreach ($c in $conns) { if ($c.State -eq 'Listen' -and $c.LocalPort -eq $m.terminalPort -and $c.OwningProcess -eq $proc.ProcessId) { $termUp = $true; break } }
    $termText = if ($termUp) { "terminal $($m.terminalPort)" } else { "terminal $($m.terminalPort) NOT LISTENING" }
    Write-Host ("  {0,-6} up   pid {1,-6} {2}" -f $m.name, $proc.ProcessId, $termText) -ForegroundColor Green

    foreach ($h in $m.hdlc) {
        $lu = if ($h.sintranLu) { "LU $($h.sintranLu)" } else { "no SINTRAN link" }

        if ($h.mode -eq 'listen') {
            $peers = @()
            foreach ($c in $conns) {
                if ($c.State -eq 'Established' -and $c.LocalPort -eq $h.port -and $c.OwningProcess -eq $proc.ProcessId) { $peers += $c }
            }
            if ($peers.Count -eq 0) {
                Write-Host ("         HDLC{0} listen {1,-6} {2,-16} no peer" -f $h.controller, $h.port, $lu) -ForegroundColor DarkYellow
            }
            foreach ($c in $peers) {
                # Who is on the other end: find the reverse connection (their local port is our
                # remote port) and identify its owning process.
                $whoPid = $null
                foreach ($r in $conns) { if ($r.State -eq 'Established' -and $r.LocalPort -eq $c.RemotePort -and $r.RemotePort -eq $h.port) { $whoPid = $r.OwningProcess; break } }
                $whoProc = if ($whoPid) { Get-Proc $whoPid } else { $null }
                $who = if ($whoProc) { Split-Path $whoProc.CommandLine.Trim('"') -Leaf } else { "pid $whoPid" }
                if ($whoProc -and $whoProc.CommandLine -match 'RetroCore') {
                    foreach ($mm in $topo.machines) { if ($whoProc.CommandLine -like "*$($mm.folder)*") { $who = $mm.name } }
                }
                elseif ($whoProc -and $whoProc.Name -eq 'Xmsg.Live.Runner.exe') { $who = 'OUR RELAY' }

                $selfConnect = ($whoPid -eq $proc.ProcessId)
                $colour = if ($selfConnect) { 'Red' } else { 'Green' }
                $suffix = if ($selfConnect) { "  <-- SELF-CONNECTED, restart this machine (LIST-LINKS will show its own Sysid)" } else { "" }
                Write-Host ("         HDLC{0} listen {1,-6} {2,-16} peer: {3}{4}" -f $h.controller, $h.port, $lu, $who, $suffix) -ForegroundColor $colour
            }
        }
        else {
            $targetPort = [int]($h.target -split ':')[-1]
            $state = 'no connection'
            foreach ($c in $conns) {
                if ($c.OwningProcess -eq $proc.ProcessId -and $c.RemotePort -eq $targetPort) { $state = $c.State; break }
            }
            $listener = $null
            foreach ($c in $conns) { if ($c.State -eq 'Listen' -and $c.LocalPort -eq $targetPort) { $listener = $c; break } }

            if ($state -eq 'Established') {
                Write-Host ("         HDLC{0} dials {1,-14} {2,-16} connected" -f $h.controller, $h.target, $lu) -ForegroundColor Green
            }
            elseif ($null -eq $listener) {
                Write-Host ("         HDLC{0} dials {1,-14} {2,-16} {3} - NOTHING IS LISTENING on {4}" -f $h.controller, $h.target, $lu, $state, $targetPort) -ForegroundColor Red
            }
            else {
                Write-Host ("         HDLC{0} dials {1,-14} {2,-16} {3}" -f $h.controller, $h.target, $lu, $state) -ForegroundColor DarkYellow
            }
        }
    }
}

# ---------------------------------------------------------------- our relay
Write-Host ""
Write-Host "OUR NODE ($($topo.ourNode.name), system $($topo.ourNode.systemNumber))" -ForegroundColor Cyan
$relay = $null
foreach ($p in $procs) { if ($p.Name -eq 'Xmsg.Live.Runner.exe') { $relay = $p; break } }
if ($relay) {
    Write-Host ("  running, pid {0}" -f $relay.ProcessId) -ForegroundColor Green
    Write-Host ("  {0}" -f $relay.CommandLine.Trim()) -ForegroundColor DarkGray
}
else {
    Write-Host "  not running" -ForegroundColor DarkYellow
    Write-Host ("  start: {0}" -f $topo.ourNode.command) -ForegroundColor DarkGray
    Write-Host "  NOTE start the relay's listener BEFORE the machine that dials it." -ForegroundColor DarkGray
}

# ---------------------------------------------------------------- .NET hosts
Write-Host ""
Write-Host ".NET / CAPTURE HOSTS" -ForegroundColor Cyan
$hosts = @()
foreach ($p in $procs) { if ($p.Name -in @('dotnet.exe','testhost.exe','MSBuild.exe','VBCSCompiler.exe','tshark.exe')) { $hosts += $p } }
if ($hosts.Count -eq 0) {
    Write-Host "  none - clean" -ForegroundColor Green
}
else {
    # The command line is the ONLY reliable way to tell our build hosts from Unity's or another
    # repo's. Killing someone else's is the failure this listing exists to prevent.
    #
    # Identical rows are COLLAPSED with a count: a single `dotnet test` elsewhere spawns ~19
    # indistinguishable MSBuild worker nodes, and listing each one buries the one line that
    # matters. The pids are still printed so any of them can be looked up.
    $repo = Split-Path (Split-Path $PSScriptRoot -Parent) -Parent
    $groups = @{}
    foreach ($p in $hosts) {
        $cmd = if ($p.CommandLine) { $p.CommandLine.Trim() } else { '(no command line)' }
        $short = $cmd.Substring(0, [Math]::Min(110, $cmd.Length))
        if (-not $groups.ContainsKey($short)) { $groups[$short] = @() }
        $groups[$short] += $p.ProcessId
    }

    foreach ($key in ($groups.Keys | Sort-Object)) {
        $pids = $groups[$key]
        $mine = $key -like "*$repo*" -or $key -like '*tshark*'
        $tag = if ($mine) { 'XMSG  ' } else { 'FOREIGN' }
        $colour = if ($mine) { 'Yellow' } else { 'DarkGray' }
        $count = if ($pids.Count -gt 1) { " x$($pids.Count)" } else { "" }
        Write-Host ("  [{0}]{1} {2}" -f $tag, $count, $key) -ForegroundColor $colour
        Write-Host ("           pids: {0}" -f ($pids -join ', ')) -ForegroundColor DarkGray
    }
    Write-Host "  FOREIGN hosts belong to another repo or to the Editor - do NOT kill them." -ForegroundColor DarkGray
    Write-Host "  XMSG hosts are ours: finish with 'dotnet build-server shutdown'." -ForegroundColor DarkGray
}

Write-Host ""
Write-Host "Traps and bring-up: SINTRAN/XMSG/LAB.md" -ForegroundColor DarkGray
Write-Host ""
