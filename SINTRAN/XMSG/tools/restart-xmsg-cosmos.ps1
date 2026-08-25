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
    [switch]$WithEthernet,

    # Leave the chat server alone at the end. It is this project's own program, so somebody
    # restarting XMSG for an unrelated reason should not have it started underneath them. It is
    # otherwise brought up by step 7, because forgetting it by hand is what most often makes a
    # restart look like it did not work.
    [switch]$SkipChatServer,

    # A FREE segment to RT-LOAD the chat server onto at the end. Zero means "do not", and the
    # script then says what is left to do instead of running a command that will not work - see
    # step 7 for why a bare RT start is not enough after an XMSG restart.
    [int]$ChatSegment = 0
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

    # The machine's answers are kept as well as shown, so a later step can CHECK one instead of
    # leaving it to whoever reads the scrollback. See the file-server check after step 5: the one
    # line that says whether the file server came up sits in the middle of a long COSMOS log, and
    # on 2026-08-20 it said "Server full! Come back later." while this script still exited 0.
    # TEE, not capture-then-print. Capturing into a variable and printing afterwards holds a
    # whole step's output back until the step ENDS - and the X-C and COSMOS steps run for
    # minutes, so the screen sits on a bare "=== 4 ===" heading with no sign of life. Tee-Object
    # passes each line through as it arrives AND fills the variable.
    & $ndterm @args 2>&1 | Tee-Object -Variable out
    $script:LastStepsOutput = ($out | Out-String)
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

# DEF-REMOTE IS ONLY HALF OF IT. It puts the name in the local table - which is why LIST-NAMES
# lists the system afterwards and why a second DEF-REMOTE says the name is taken - but it grants
# no ACCESS, and an XMSG restart clears the friend entries along with everything else. A peer
# without access is refused with XRUNN, "unknown name (of server or system)", and it means the
# SYSTEM half of that phrase, not the server half.
#
# The machine states it plainly if you ask. X-C -> LIST-ROUTING-INFO:
#
#   19999  L: *->19999
#          T: *, but no access to system 19999
#          A: *, but no route to system 19999
#
# DEFINE-FRIEND-SYSTEM removes the "no access" line. Measured on D100 2026-08-17, twice: after a
# restart every transfer went unanswered - no XRUNN, no reply at all - with the link showing
# State Run and thousands of frames received, and this is what was missing. Leaving it out costs
# an evening of blaming the wire, so it is part of the bring-up now.
# ONE LINE, not command-then-answer. The step driver waits for the "X-C:" prompt after
# every line it sends, and DEFINE-FRIEND-SYSTEM on its own leaves "System?" instead - so
# the two-line form works but stalls thirty seconds per system and logs a timeout that
# reads like a failure. The command takes the number inline, same as LIST-SYSTEMS.
# THE FRIEND ENTRIES ARE NOT GRANTED HERE ANY MORE - SEE STEP 6.
#
# They used to be, right after the DEF-REMOTE lines, and every one of them answered "Ok". They were
# still gone by the time the script finished. MEASURED 2026-08-18: after a full clean run of this
# script, X-C -> LIST-ROUTING-INFO said
#
#   19999  T: *, but no access to system 19999
#
# and every transfer was refused in total silence. Granting the SAME entry by hand at that point,
# changing nothing else, made D100 answer the very next connect letter.
#
# What clears them is COSMOS: step 5 runs COS-START-E04, which makes its own system definitions, and
# a DEF-REMOTE wipes the friend flag of the system it names (see DOC/FRIEND-SYSTEMS.md - "THE ORDER
# MATTERS"). So granting access before COSMOS starts is granting it to something that is about to be
# overwritten. The grant has to be the LAST thing that touches the routing tables.

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

# --- 5b. DID THE FILE SERVER ACTUALLY COME UP? -------------------------------------------------
# CHECKED, not left to the reader. COS-START-E04 either says
#
#     Server 1 started.     No of FACs attached: 30
#
# or it says "Server full! Come back later." - and on 2026-08-20 it said the second one while this
# script still exited 0 and every step after it answered Ok. Nothing downstream names the problem
# either: a push then climbs LAPB, sends its connect letter, and node 100 ANSWERS - with a TAD
# REJE carrying the ASCII "D100". That reads as a live machine refusing us for some access reason,
# and an hour can go into the routing tables before anybody looks at this line.
#
# Told here, the cure is three commands and it is printed with the warning.
$fileServerUp = $script:LastStepsOutput -match 'No of FACs attached'
$serverFull   = $script:LastStepsOutput -match 'Server full'
if (-not $fileServerUp) {
    Write-Host ""
    Write-Host "*** THE COSMOS FILE SERVER DID NOT START - STARTING IT ***" -ForegroundColor Yellow
    if ($serverFull) {
        Write-Host "    COS-START-E04 answered 'Server full! Come back later.'" -ForegroundColor Yellow
    } else {
        Write-Host "    No 'No of FACs attached' line came back from COS-START-E04." -ForegroundColor Yellow
    }

    # DO IT, DO NOT JUST SAY IT.
    #
    # This block printed the three commands and left them for a person to type, and on 2026-08-21
    # that cost most of a day: EVERY pull after a restart timed out at 240 seconds with no file
    # server to answer it, the failure was read as an XMSG death, and five controlled experiments
    # were run around a machine that simply had nothing serving files. A tool that knows the cure
    # and only prints it is a tool that will be ignored at exactly the moment it matters.
    #
    # RT FSART FIRST. SELECT-FSA on its own answers "Remote FSA is not running" straight after a
    # COSMOS restart - RT starts the RT program, and the SERVER inside it is a separate thing that
    # has to be told to start.
    Invoke-Steps @(
        "RT FSART",
        "FS-ADMINISTRATOR",
        "SELECT-FSA,,,,",
        "START-SERVER 1,,,,",
        "EXIT"
    ) "" 45000 "5c. start the COSMOS file server"

    # CHECK IT TOOK. "Server N started" is the only line that says so, and N is NOT the number
    # asked for - START-SERVER allocates the next free server, so a second run answers "Server 2".
    # Read it as "a server is up", never as "server 1 was replaced".
    if ($script:LastStepsOutput -match 'No of FACs attached') {
        Write-Host "    file server is up." -ForegroundColor Green
    } else {
        Write-Host ""
        Write-Host "*** THE FILE SERVER IS STILL NOT UP ***" -ForegroundColor Red
        Write-Host "    Every FA PULL will now time out after 240s with the link CONNECTED and the" -ForegroundColor Yellow
        Write-Host "    peer silent - which looks nothing like a missing server. Check by hand:" -ForegroundColor Yellow
        Write-Host "        @RT FSART" -ForegroundColor Yellow
        Write-Host "        @FS-ADMINISTRATOR" -ForegroundColor Yellow
        Write-Host "        FSA: SELECT-FSA,,,,          -> 'Connection established'" -ForegroundColor Yellow
        Write-Host "        FSA(own system): START-SERVER 1,,,,   -> 'No of FACs attached: 30'" -ForegroundColor Yellow
        Write-Host "        FSA(own system): EXIT" -ForegroundColor Yellow
        Write-Host ""
    }
}

# --- 6. Friend systems - LAST, because COSMOS wipes them --------------------------------------
# This is the step that has to come after everything else that touches the routing tables.
#
# MEASURED 2026-08-18. Granted in step 4 (before COSMOS) every DEFINE-FRIEND-SYSTEM answered "Ok"
# and every one of them was GONE by the end of the run:
#
#   X-C -> LIST-ROUTING-INFO
#     19999  L: *->19999
#            T: *, but no access to system 19999
#
# with the link up, the name defined, the FA server registered with 30 free seats - and every
# transfer refused in TOTAL SILENCE, no error frame of any kind. Granting the same entry by hand at
# that point, changing nothing else, made D100 answer the very next connect letter and the FA ladder
# started climbing. That is the whole difference.
#
# DEF-REMOTE clears the friend flag of the system it names (DOC/FRIEND-SYSTEMS.md, "THE ORDER
# MATTERS"), and COS-START-E04 makes its own system definitions. So the grant must be last or it is
# granted to something that is about to be overwritten.
#
# Read the routing line to check it, NOT LIST-FRIEND-SYSTEMS: that command still listed 19999 while
# routing said "no access", so the two tables disagree and only the routing one predicts behaviour.
$friendSteps = New-Object System.Collections.Generic.List[string]
$friendSteps.Add("X-C")
foreach ($s in $Systems) {
    $number = ($s -split '\s+')[-1]
    if ($number -eq $LocalSystem -or $s -like "$LocalSystem *") { continue }
    $friendSteps.Add("DEFINE-FRIEND-SYSTEM $number")
}
$friendSteps.Add("EXIT")

Invoke-Steps $friendSteps.ToArray() "X-C:" 8000 "6. friend systems (AFTER COSMOS - it wipes them)"

# --- 7. The chat server ------------------------------------------------------------------------
# THE ONE THING THAT DIES WITH XMSG AND WAS STILL BEING STARTED BY HAND EVERY SINGLE TIME.
#
# Measured across ten XMSG deaths on 2026-08-20/21: a restart takes down FOUR things, not one, and
# every failure names something ELSE -
#
#   TADADM missing      -> START-SERVER says "Terminal access not running or unknown port name"
#   file server missing -> a push climbs LAPB, D100 ANSWERS, and the answer is a TAD REJE carrying
#                          the ASCII "D100" - which reads as an access problem and is not
#   link missing        -> our SABM goes unanswered for ever; the runner just times out at 240s
#   CHATSER missing     -> clients get nothing at all
#
# Steps 3, 4 and 5 already cover the first three - the link is started at the END of step 4, see
# the START-LINK line in $xcSteps. Only the chat server was left to the operator, and it is the
# easiest of the four to forget because nothing complains until somebody tries to join.
#
# DO NOT ADD START-LINK HERE. It was in this step for one commit and it was redundant: step 4 has
# already started the link, and starting it twice answers
#
#     Error in communicating with XROUT.
#     XMSG Routing/Naming error: Illegal/Reserved Logical Unit Number (LUN) for link
#
# which is the same message the command gives when the link does not exist YET. That message says
# "this LUN is not in a state where this command applies" and NOTHING about which verb you used -
# measured 2026-08-21 both ways round, once too early and once too late.
#
# THE SAME ORDERING TRAP CATCHES CHATSER, which is why it is last: started before XROUT is
# configured it goes straight to PASSIVE, because xmpopcn has nothing to register against.
# AND A BARE "RT CHATSER" IS NOT ENOUGH AFTER A RESTART - it goes straight to PASSIVE.
#
# Measured 2026-08-21, twice within minutes, same source, same machine:
#   segment 2526, linked BEFORE this restart -> PASSIVE, 0 CPU units, no log line written
#   segment 2527, same source, linked AFTER  -> IN TIME QUEUE ... TMOUT, alive and serving
# A foreground run works either way, because it loads from its :PROG each time. That difference
# made the RT path look broken for most of a session.
#
# So the segment must be RT-LOADED again, which needs a FRESH segment number this script cannot
# invent. Give -ChatSegment to have it done here; without it, say plainly what is left to do
# rather than running a command that will quietly fail.
if ($SkipChatServer) {
    Write-Host ""
    Write-Host "=== 7. SKIPPED (-SkipChatServer): chat server not started ===" -ForegroundColor DarkGray
} elseif ($ChatSegment -gt 0) {
    $rtLoad = Join-Path $PSScriptRoot "rt-load.ps1"
    Write-Host ""
    Write-Host "=== 7. the chat server (RT-LOAD onto segment $ChatSegment) ===" -ForegroundColor Cyan
    & $rtLoad -Port $Port -Segment $ChatSegment -User $User -Password $Password `
        -RemoteHost $RemoteHost -AndStart
} else {
    Write-Host ""
    Write-Host "=== 7. the chat server - NOT STARTED, and a bare RT start will not work ===" -ForegroundColor Yellow
    Write-Host "    An RT program that uses XMSG must be RT-LOADED AGAIN after an XMSG restart;" -ForegroundColor Yellow
    Write-Host "    a segment linked before the restart goes PASSIVE with 0 CPU units." -ForegroundColor Yellow
    Write-Host "    Run:  tools\rt-load.ps1 -Port $Port -Segment <a free one> -AndStart" -ForegroundColor Yellow
    Write-Host "    or re-run this script with -ChatSegment <a free one>." -ForegroundColor Yellow
}

Write-Host ""
Write-Host "Done. Expect these and ignore them:" -ForegroundColor Green
Write-Host "  - printer definitions failing for ND-969 / ND-1068 / ND-5005 (those systems do not exist here)"
Write-Host "  - 'File already exists, but it does not belong to COSMOS-SPOOLING'"
if ($fileServerUp) {
    Write-Host "File server: UP - COS-START-E04 reported its FACs attached." -ForegroundColor Green
} else {
    Write-Host "File server: NOT RUNNING - see the red block above. FA transfers will be refused." -ForegroundColor Red
}
