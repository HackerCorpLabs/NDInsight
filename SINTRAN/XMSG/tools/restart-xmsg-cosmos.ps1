# Restarts XMSG and COSMOS on a live SINTRAN machine, headless, over its terminal port.
#
# This is Ronny's sequence, dictated 2026-08-04 and verified on D100 the same morning. It is NOT
# reconstructed from the boot mode file: (SYSTEM)LOAD-MODE:MODE is close but does NOT include the
# ENNS0 network server or the DEF-NETWORK-CONN lines, and those are what put the machine on the
# Ethernet segment. Running LOAD-MODE alone leaves XMSG up but the segment unconfigured.
#
#   .\restart-xmsg-cosmos.ps1 -Port 9010                     # D100
#   .\restart-xmsg-cosmos.ps1 -Port 9102 -LocalSystem D102   # D102, same sequence
#
# WHEN TO RUN IT. The tell for a half-crashed XMSG is a contradiction:
#   @X-COMM            says  "XMSG Kernel error: XMSG is either not generated, not loaded or not started"
#   @SINTRAN START-XMSG says "ERROR: XMSG is already running"
# The RT side is up, the user interface cannot reach it. One fault, three symptoms - file access
# reports "not running or crashed" (SINTRAN error 324B), LIST-ROUTE is not recognised, and nothing
# at all leaves the machine.
param(
    [Parameter(Mandatory = $true)][int]$Port,

    # The machine we are talking to. Its own name must be skipped in DEF-NETWORK-CONN: the local
    # system is rejected with "You cannot make this network definition for the local defined system".
    [string]$LocalSystem = "D100",

    [string]$User = "SYSTEM",
    [string]$Password = "",
    [string]$RemoteHost = "127.0.0.1",

    # Systems to define. D19999 is the C# node; it must be redefined after EVERY XMSG restart,
    # because a restart clears the whole remote-name table.
    #
    # D200 added 2026-08-09 on Ronny's instruction: a high-level Ethernet controller emulator
    # another agent is writing. He configured D100 and D102 by hand already; these lines are so a
    # later restart puts it back without anyone having to remember it.
    [string[]]$Systems = @("D100 100", "D101 101", "D102 102", "D103 103", "D200 200", "D19999 19999"),

    # Systems reachable over the Ethernet segment through the ENNS0 network server.
    [string[]]$NetworkSystems = @("D100", "D102", "D200", "D19999"),

    # The HDLC link started at the end of the XMSG configuration.
    [string]$LinkNumber = "1362",

    # Bring up the ENNS0 Ethernet network server and the DEF-NETWORK-CONN entries that need it.
    #
    # OFF by default, deliberately. ENNS0 is only needed to reach a system over the ETHERNET
    # segment (D19999). A system reached over HDLC (D103) does not need it, and starting it costs
    # you an intermittent XMSG crash: on 2026-08-04 one restart in eleven had START-NET-SERVER
    # answer "-45: XMSG is either not generated, not loaded or not started", after which every
    # later command failed the same way - so START-LINK never ran and the machine looked broken in
    # a way that took a long time to attribute correctly.
    [switch]$WithEthernet
)

$ErrorActionPreference = "Stop"
$ndterm = Join-Path $PSScriptRoot "ndterm.ps1"
if (-not (Test-Path $ndterm)) {
    throw "ndterm.ps1 not found next to this script (looked in $PSScriptRoot)"
}

function Invoke-Steps([string[]]$steps, [string]$waitFor, [int]$settleMs, [string]$label) {
    Write-Host ""
    Write-Host "=== $label ===" -ForegroundColor Cyan
    $args = @{
        Port       = $Port
        User       = $User
        Password   = $Password
        RemoteHost = $RemoteHost
        Steps      = $steps
        SettleMs   = $settleMs
    }
    if ($waitFor -ne "") { $args["WaitFor"] = $waitFor }
    & $ndterm @args
}

# --- 1. Stop XMSG -------------------------------------------------------------------------------
# From the @ prompt, SIN enters the SINTRAN service program (its prompt is *), where STOP-X and
# START-X live. STOP-X first even when XMSG looks dead - a half-crashed kernel still reports
# "already running" and START-X alone will refuse.
Invoke-Steps @("SIN", "STOP-X", "EXIT") "" 10000 "1. stop XMSG"

# Let the kernel settle before starting it again. Ronny's instruction, and it costs nothing.
Write-Host "waiting 10 seconds before restart..." -ForegroundColor DarkGray
Start-Sleep -Seconds 10

# --- 2. Start XMSG ------------------------------------------------------------------------------
Invoke-Steps @("SIN", "START-X", "EXIT") "" 12000 "2. start XMSG"

# --- 3. TAD and availability --------------------------------------------------------------------
# TADA is TADADM, which prints the TAD table - it is a report, not an interactive program, so it
# returns straight to @.
Invoke-Steps @("START-TAD", "TADA", "SET-AVAIL") "" 12000 "3. start TAD, set available"

# --- 3b. Abort the Ethernet network server ------------------------------------------------------
# ONLY when -WithEthernet. ABORT is a SINTRAN command at the @ prompt, NOT an X-C one - it belongs
# here, before the X-C session, alongside the COSMOS mode file's own "@ABORT FSART".
#
# WHY: if a previous START-NET-SERVER took XMSG down with it, the ENNS0 RT program is left wedged,
# and starting it again on top of that just repeats the crash. Aborting a program that is not
# running is harmless, so this is safe to do unconditionally.
#
# THEN WAIT 10 SECONDS. The abort does not complete the moment the prompt comes back, and starting
# the server again too early is the same as not aborting at all.
if ($WithEthernet) {
    Invoke-Steps @("ABORT ENNS0") "" 6000 "3b. abort the Ethernet network server"
    Write-Output "waiting 10 seconds for ENNS0 to go down before starting it again..."
    Start-Sleep -Seconds 10
}

# --- 4. XMSG configuration ----------------------------------------------------------------------
# All of this happens inside X-C (the XMSG command program, prompt "X-C:"). WaitFor is used rather
# than a fixed delay because START-NET-SERVER announces "wait 10 sec!" and a fixed delay that is
# too short sends the following command into it and garbles both - which is exactly what happened
# on the first attempt.
$xcSteps = New-Object System.Collections.Generic.List[string]
$xcSteps.Add("X-C")

for ($i = 0; $i -lt $Systems.Count; $i++) {
    $xcSteps.Add("DEF-REMOTE,,$($Systems[$i])")
}

# The Ethernet network server on the 68000 card. Takes about 10 seconds to come up.
#
# ONLY when -WithEthernet is given. On 2026-08-04 this command KILLED XMSG on one restart in
# eleven: it answered
#     *- XMSG error code: -45: XMSG is either not generated, not loaded or not started
# and so did every command after it, so START-LINK never ran, D100 never learned D103, and the
# HDLC test failed with UNKNOWN REMOTE SYSTEM NAME. It is intermittent, not reliable, but it is
# also completely unnecessary for an HDLC-reached system - ENNS0 is the ETHERNET server, and D103
# is reached over HDLC. Do not pay for a crash you do not need.
#
# The ABORT that has to happen before this lives OUTSIDE the X-C session - see above the X-C block.
if ($WithEthernet) {
    $xcSteps.Add("START-NET-SERVER,ENNS0,,,N")
}

for ($i = 0; $i -lt $NetworkSystems.Count; $i++) {
    # Skip our own name - the local system is rejected here, and that rejection is harmless but
    # noisy enough to look like a real failure in the log.
    if ($NetworkSystems[$i] -eq $LocalSystem) { continue }
    $xcSteps.Add("DEF-NETWORK-CONN $($NetworkSystems[$i]) ENNS0,,0,0,0,0")
}

$xcSteps.Add("START-LINK,$LinkNumber,,,-1,,")
$xcSteps.Add("ENABLE-ROUTE-THROUGH")
$xcSteps.Add("EXIT")

Invoke-Steps $xcSteps.ToArray() "X-C:" 8000 "4. XMSG configuration (X-C)"

# --- 5. COSMOS ----------------------------------------------------------------------------------
# COS-START-E04 ends with @SET-UNAVAILABLE by design; LOAD-MODE is what normally follows it with
# @SET-AVAILABLE. Both run in the SAME session here, which matters: SET-UNAVAILABLE blocks NEW
# logins, so a session that is already open can still issue SET-AVAIL. Split them across two
# connections and you lock yourself out - TERM 5/6/7 have no port and terminal 1 is the RetroCore
# window, so the only way back is typing SET-AVAILABLE on the GUI console.
Invoke-Steps @("MODE (PACK-ONE:COSMOS-BASIC)COS-START-E04:MODE,,", "SET-AVAIL") "" 45000 "5. COSMOS, then set available"

Write-Host ""
Write-Host "Done. Expect these and ignore them:" -ForegroundColor Green
Write-Host "  - printer definitions failing for ND-969 / ND-1068 / ND-5005 (those systems do not exist here)"
Write-Host "  - 'File already exists, but it does not belong to COSMOS-SPOOLING'"
Write-Host "Look for: 'Server 1 started.  No of FACs attached: 30'"
