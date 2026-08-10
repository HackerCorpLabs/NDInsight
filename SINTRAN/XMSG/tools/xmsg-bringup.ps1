<#
.SYNOPSIS
    Brings XMSG up on one machine using the COSMOS Operator Guide section 2.5 sequence, with the
    adjacency rule applied automatically, then verifies the link.

.DESCRIPTION
    The sequence itself is four commands, but getting it WRONG is what cost this project days:

      DEFINE-REMOTE-NAME,,<name>,<number>     for EVERY system, including this machine's own name
      DEFINE-SYSTEM-ROUTE,,<dest>,<nextHop>   for NON-ADJACENT systems ONLY
      START-LINK,<lu>,,,-1,,                  LU is OCTAL, -1 means retry forever

    COSMOS Operator Guide ND-30.025.02 section 2.5.4: a system on the far end of THIS machine's own
    cable is ADJACENT and must NOT be given a route. Only systems reached THROUGH another node get
    one. Every peer reported "Remote system is not accessible" for days because adjacent systems had
    been given routes.

    This script does not hardcode who is adjacent. It DERIVES it from lab-topology.json: it finds
    the HDLC controller this machine actually runs its SINTRAN link on, works out who is on the far
    end of that port, and routes everything else via that peer. Rewire the lab, update the JSON, and
    the plan follows.

    It prints the plan and stops unless -Apply is given, because it reconfigures a live machine.

    STATUS, 2026-08-08. Both paths are now verified against D100.

    The PLAN reproduces, from the topology alone, the configuration that was found by hand and that
    made the relay carry a full round trip (DOC/captures/TRANSIT-PROVEN-2026-08-08).

    -Apply runs end to end and its verification correctly read back
    `Run, Sysid 19999`. An earlier attempt LOOKED hung and was not: step lists ending in EXIT made
    the 'X-C:' waiter sit out two full 120-second timeouts. EXIT is now sent separately and the
    verify uses a short waiter, since LIST-LINKS' own prompts can never match 'X-C:'.

    Still confirm with lab-status.ps1 after a bring-up. This script reports what the machine
    answered; it does not prove the far end is reachable.

    Verification afterwards reads LIST-LINKS and checks the ONE field that catches the failure that
    looks healthy: `Sysid` equal to this machine's OWN system number means its HDLC has TCP-connected
    to itself, which only a restart clears.

.PARAMETER Machine
    Machine name as it appears in lab-topology.json, for example D100.

.PARAMETER Apply
    Actually send the commands. Without it the plan is printed and nothing is sent.

.PARAMETER RestartXmsg
    Run SINTRAN-SERVICE / STOP-XMSG / START-XMSG first. NOTE this clears the ENTIRE name table
    including the machine's own name, which is exactly why the full sequence is re-sent after it.

.EXAMPLE
    .\xmsg-bringup.ps1 -Machine D100
    .\xmsg-bringup.ps1 -Machine D100 -Apply
    .\xmsg-bringup.ps1 -Machine D103 -Apply -RestartXmsg
#>
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)][string] $Machine,
    [switch] $Apply,
    [switch] $RestartXmsg,
    [string] $TopologyPath = (Join-Path (Split-Path $PSScriptRoot -Parent) 'lab-topology.json')
)

$ErrorActionPreference = 'Stop'

$topo = Get-Content $TopologyPath -Raw | ConvertFrom-Json

$me = $null
foreach ($m in $topo.machines) { if ($m.name -ieq $Machine) { $me = $m; break } }
if ($null -eq $me) {
    Write-Error "Unknown machine '$Machine'. Known: $(($topo.machines | ForEach-Object { $_.name }) -join ', ')"
    return
}

# --- which controller actually carries the SINTRAN link -------------------------------------------
# A machine can have several HDLC controllers wired up while SINTRAN runs a link on only one. Using
# the wrong one gives a healthy TCP connection, a peer that SABMs forever, and RXBad 0 - a fault
# that reads as a protocol problem and is not one.
$link = $null
foreach ($h in $me.hdlc) { if ($h.sintranLu) { $link = $h; break } }
if ($null -eq $link) {
    Write-Error "$($me.name) has no HDLC controller with a SINTRAN LU in the topology - nothing to bring up."
    return
}

# --- who is on the far end of that controller = the ADJACENT system -------------------------------
$adjacent = $null
if ($link.mode -eq 'listen') {
    # Someone dials us. Our own relay dials this port, or another machine does.
    if ($topo.ourNode.relayOutboundDial -match ":$($link.port)$") { $adjacent = $topo.ourNode }
    foreach ($other in $topo.machines) {
        foreach ($oh in $other.hdlc) {
            if ($oh.mode -eq 'connect' -and $oh.target -match ":$($link.port)$") { $adjacent = $other }
        }
    }
}
else {
    $targetPort = [int]($link.target -split ':')[-1]
    if ($targetPort -eq $topo.ourNode.relayInboundListenPort) { $adjacent = $topo.ourNode }
    foreach ($other in $topo.machines) {
        foreach ($oh in $other.hdlc) {
            if ($oh.mode -eq 'listen' -and [int]$oh.port -eq $targetPort -and $oh.sintranLu) { $adjacent = $other }
        }
    }
}

if ($null -eq $adjacent) {
    Write-Error "Cannot work out who is on the far end of $($me.name) HDLC$($link.controller) - fix lab-topology.json before running this."
    return
}

# --- everything else is reached THROUGH the adjacent system ---------------------------------------
$everyone = @()
foreach ($m in $topo.machines) { $everyone += [pscustomobject]@{ name = $m.name; number = $m.systemNumber } }
$everyone += [pscustomobject]@{ name = $topo.ourNode.name; number = $topo.ourNode.systemNumber }

$routed = @()
foreach ($s in $everyone) {
    if ($s.name -ieq $me.name) { continue }          # ourselves: never routed
    if ($s.name -ieq $adjacent.name) { continue }    # ADJACENT: section 2.5.4, no route
    # Only route systems that actually sit behind the adjacent peer. A machine wired to a controller
    # SINTRAN does not use is unreachable from here, and inventing a route to it would be a guess.
    $reachable = $false
    foreach ($m in $topo.machines) {
        if ($m.name -ieq $s.name) {
            foreach ($h in $m.hdlc) {
                if (-not $h.sintranLu) { continue }
                if ($h.mode -eq 'connect' -and $h.target -match ":$($topo.ourNode.relayInboundListenPort)$") { $reachable = $true }
                if ($h.mode -eq 'listen' -and $topo.ourNode.relayOutboundDial -match ":$($h.port)$") { $reachable = $true }
            }
        }
    }
    if ($s.name -ieq $topo.ourNode.name) { $reachable = $true }
    if ($reachable) { $routed += $s }
}

# --- build the command list -----------------------------------------------------------------------
# Every step below is answered by the X-C: prompt, so they share one waiter. EXIT is deliberately
# NOT in this list: after it the prompt is '@', so a step list ending in EXIT makes the X-C: waiter
# sit out its full 120-second timeout. Two of those is what made the first -Apply run look hung when
# it was only waiting. EXIT is sent separately below, with a waiter that can actually match.
$steps = @('XMSG-COMMAND')
foreach ($s in $everyone) { $steps += "DEFINE-REMOTE-NAME,,$($s.name),$($s.number)" }
foreach ($s in $routed)   { $steps += "DEFINE-SYSTEM-ROUTE,,$($s.name),$($adjacent.name)" }
$steps += "START-LINK,$($link.sintranLu),,,-1,,"

# --- show the plan ---------------------------------------------------------------------------------
Write-Host ""
Write-Host "XMSG BRING-UP PLAN - $($me.name) (system $($me.systemNumber), terminal $($me.terminalPort))" -ForegroundColor Cyan
Write-Host ("  link:     HDLC$($link.controller) $($link.mode) $(if($link.mode -eq 'listen'){$link.port}else{$link.target})  ->  SINTRAN LU $($link.sintranLu)") -ForegroundColor Gray
Write-Host ("  ADJACENT: $($adjacent.name)  - gets a NAME and NO route (operator guide 2.5.4)") -ForegroundColor Green
if ($routed.Count -gt 0) {
    Write-Host ("  routed:   " + (($routed | ForEach-Object { "$($_.name) via $($adjacent.name)" }) -join ', ')) -ForegroundColor Green
}
else {
    Write-Host "  routed:   nothing - every known system is adjacent or unreachable from here" -ForegroundColor DarkYellow
}
Write-Host ""
if ($RestartXmsg) {
    Write-Host "  SINTRAN-SERVICE / STOP-XMSG / START-XMSG first (this CLEARS every name, including this machine's own)" -ForegroundColor Yellow
}
foreach ($s in $steps) { Write-Host "    $s" -ForegroundColor DarkGray }
Write-Host ""

if (-not $Apply) {
    Write-Host "Dry run. Re-run with -Apply to send these to $($me.name)." -ForegroundColor Yellow
    Write-Host ""
    return
}

# --- apply ------------------------------------------------------------------------------------------
$ndterm = Join-Path $PSScriptRoot 'ndterm.ps1'

if ($RestartXmsg) {
    Write-Host "Restarting XMSG on $($me.name)..." -ForegroundColor Cyan
    & $ndterm -Port $me.terminalPort -User 'SYSTEM' -WaitFor '*' -Steps @('SINTRAN-SERVICE','STOP-XMSG','START-XMSG','EXIT')
}

Write-Host "Applying the bring-up sequence to $($me.name)..." -ForegroundColor Cyan

# ONE connection for the whole interaction - apply AND verify - and then log out.
#
# Two ndterm calls back to back is a trap: the terminal port is a POOL (TERM 8/9/10 all share it),
# so the second connection lands on a DIFFERENT terminal. On 2026-08-08 that second line was in a
# wedged state and answered "TERMINAL LINE IS NOT CONNECTED / NO FILE OPEN WITH THIS NUMBER"
# forever, so a bring-up that had actually succeeded reported a failed verification.
#
# LIST-LINKS is appended here rather than run separately for the same reason. Its two prompts
# (XROUT system? / Record address?) are answered by the blank steps; they cannot match the 'X-C:'
# waiter, so a SHORT -WaitForTimeoutMs keeps that from costing two minutes each.
# KNOWN, harmless: ndterm's trailing LOGOUT is sent while still inside XMSG-COMMAND, so it answers
# "** Command not recognised **" and the line is left at the X-C: prompt. SINTRAN logs it out on
# inactivity. Sending EXIT first would need a second waiter ('@'), which is not worth a second
# connection - and a second connection is the exact trap described above.
$verifySteps = $steps + @('LIST-LINKS', '', '')
$applyOut = & $ndterm -Port $me.terminalPort -User 'SYSTEM' -WaitFor 'X-C:' -WaitForTimeoutMs 8000 `
            -Steps $verifySteps | Out-String
Write-Host $applyOut -ForegroundColor DarkGray

# Classify the replies. A bring-up that half-applied is worse than one that plainly failed, because
# the machine then LOOKS configured - so anything not recognised as benign is reported loudly.
#
# Two replies are benign and both are common on a rebuild:
#
#  - "Another port already has this name" - the XROUT name table survives a machine restart even
#    though the XMSG kernel tables do not, so re-defining an existing name is a no-op, not an error.
#  - "Illegal/Reserved Logical Unit Number (LUN) for link" from START-LINK when the link is ALREADY
#    running. MEASURED 2026-08-08: LU 1360 - which was carrying a live link at that moment - was
#    refused with exactly this text. The message therefore does NOT mean the LU is invalid, and it
#    must not be read as evidence about LU numbering. Check LIST-LINKS before concluding anything
#    from it.
$okCount     = ([regex]::Matches($applyOut, '(?m)^\s*Ok\s*$')).Count
$nameExists  = ([regex]::Matches($applyOut, 'Another port already has this name')).Count
$lunRefused  = ([regex]::Matches($applyOut, 'Illegal/Reserved Logical Unit Number')).Count
$expected    = $steps.Count - 1                      # every step except XMSG-COMMAND itself
$accounted   = $okCount + $nameExists + $lunRefused

Write-Host ("replies: {0} Ok, {1} name-already-defined, {2} link-already-started  (of {3} commands)" -f $okCount, $nameExists, $lunRefused, $expected) -ForegroundColor Gray
if ($accounted -lt $expected) {
    Write-Host "WARNING: $($expected - $accounted) command(s) gave something else - read the transcript above." -ForegroundColor Red
}
else {
    Write-Host "Every command accounted for." -ForegroundColor Green
}

# --- verify -------------------------------------------------------------------------------------------
Write-Host ""
Write-Host "Verifying..." -ForegroundColor Cyan

# Read the link row out of the SAME transcript. LIST-LINKS was appended to the step list above so
# the whole interaction uses ONE connection - opening a second one lands on a different terminal
# from the pool, which is how a successful bring-up came to report a failed verification.
# Short -WaitForTimeoutMs on purpose. LIST-LINKS prompts twice (XROUT system? / Record address?) and
# those prompts are NOT 'X-C:', so the two blank steps that answer them cannot match the waiter and
# will always time out. ndterm carries on correctly afterwards, so the only cost is the wait - at the
# default 120s that is four minutes of nothing. Eight seconds is plenty for a local emulator.
$out = $applyOut

# The link row is:  No  Addr. State Sysid Rcv Xmit  Lun Timeout  Soft-stat-hard TXData/Retry/RXBad
if ($out -match '(?m)^\s*\d+\s+\d+\s+(\S+)\s+(\d+)\s') {
    $state = $Matches[1]
    $sysid = [int]$Matches[2]

    if ($sysid -eq $me.systemNumber) {
        Write-Host "SELF-CONNECTED: Sysid $sysid is this machine's OWN system number." -ForegroundColor Red
        Write-Host "Its HDLC has TCP-connected to itself, which happens when the listener it dials is down." -ForegroundColor Red
        Write-Host "State reads '$state' and everything else looks fine. Only restarting this machine clears it." -ForegroundColor Red
    }
    elseif ($sysid -eq 0) {
        Write-Host "Link state '$state', Sysid 0 - no peer identified yet. Is the far end listening?" -ForegroundColor DarkYellow
    }
    else {
        $peerName = $adjacent.name
        Write-Host "Link state '$state', Sysid $sysid (expected $peerName). Looks healthy." -ForegroundColor Green
    }
}
else {
    Write-Host "Could not read a link row from LIST-LINKS - check the output above by eye." -ForegroundColor DarkYellow
}
Write-Host ""
