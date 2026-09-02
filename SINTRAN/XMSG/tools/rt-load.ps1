<#
.SYNOPSIS
    RT-load a PLANC program onto a fresh segment and set its description, in one command.

.DESCRIPTION
    Why this exists: the RT-LOADER sequence is short, entirely mechanical, and has THREE traps
    that have each cost real time on D100. Written down in prose they kept being fallen into
    anyway - the ALTERNATIVE PAGE TABLE one twice in a single day, hours after being documented.
    A trap that lives in a document does not stop you. A trap encoded in a script does.

    TRAP 1 - CHANGE-RT-DESCRIPTION TAKES EIGHT PARAMETERS AND SLOT 5 IS START ADDRESS, NOT RING.

        RT-PROGRAM PRIORITY SEGMENT-ONE SEGMENT-TWO START-ADDRESS RING INITIAL-PT ALTERNATIVE-PT

    Typed on one line as "CHATSER,75,2520,,2," the 2 meant as the ring lands in START ADDRESS.
    The loader accepts it silently, and the program then starts INSIDE ITS OWN DATA and dies as

        - STACK OVERFLOW AT 003217B

    which reads exactly like a runaway routine and is nothing of the kind. LIST-RT-DESCRIPTION
    shows "START ADDRESS: 2B" where a real one is five digits octal. A BLANK DOES NOT REPAIR IT
    afterwards - blank means "leave as loaded" and the wrong value has been loaded - so the
    segment has to be loaded again from scratch. This script always answers the prompts one at a
    time and always leaves START ADDRESS blank.

    TRAP 2 - THE PROMPTS RUN PAST THE ONE YOU EXPECT. After RING come INITIAL PAGE TABLE and
    ALTERNATIVE PAGE TABLE. A command typed while one of those is still waiting is EATEN as its
    answer, and the whole thing aborts with ILLEGAL PARAMETER TYPE - which then leaves the RT
    description half set and the next command going to the loader instead of SINTRAN.

    TRAP 3 - NEW-SEGMENT TAKES FIVE PARAMETERS, and given two it swallows the NEXT COMMAND LINE
    as the answer to SEGMENT TYPE. "NEW-SEGMENT 2521,2" eats the following LOAD and answers
    PARAMETER NO. 3 IS ILLEGAL. The trailing commas are not decoration.

    AND THE RULE THAT IS NOT A TRAP BUT A FACT ABOUT XMSG:

    AN RT PROGRAM THAT USES XMSG MUST BE RT-LOADED AGAIN AFTER EVERY XMSG RESTART. Restarting it
    is NOT enough. Measured on D100, 2026-08-21, twice within a few minutes:

        segment 2526, linked BEFORE the XMSG restart -> @RT CHATSER goes PASSIVE, 0 CPU units,
                                                        not one line written to its log
        segment 2527, same source, linked AFTER      -> IN TIME QUEUE ... TMOUT, alive, serving

    A foreground @CHATSV works either way, because it is loaded from its :PROG on every run. That
    difference is what made the RT path look broken for most of a session. So after any
    restart-xmsg-cosmos, do not just start the server - RUN THIS.

.PARAMETER Program
    The BRF to load, without type. Default CHATSV.

.PARAMETER RtName
    The RT program name in the description. This is the PROGRAM unit's name, NOT the file name -
    CHATSV.PLNC declares "PROGRAM : chatServer" and the RT program is CHATSER.

.PARAMETER Segment
    The segment to make and load onto. MUST be free: this script will not reuse one.

.PARAMETER Priority
    RT priority. 75 is what the chat server has always used.

.PARAMETER Ring
    Ring, answered at the RING prompt where it belongs.

.PARAMETER Libraries
    Loaded AFTER the program, in order. The linker wants the program first and then what
    satisfies it, which is the opposite of the compiler's order.

    AND THE OTHER THING AN RT-LOAD DESTROYS: THE MACHINE'S NAME AND ITS TRUNKS.

    A fresh segment is a fresh server, and the server keeps NEITHER of those on disk. It comes up
    with no name - so it calls itself D100 instead of FJELL - and with an EMPTY peer table, so
    every trunk is gone. Both look completely healthy: the server is in the time queue, clients
    join, and a room works perfectly until somebody tries to reach the machine next door.

    That is five lines to be typed on each of three machines after every single build, and
    forgetting one is nearly invisible. So this script types them, from the table below, keyed by
    the terminal port it was given. See -ChatName, -Trunks and -NoChatSetup.

.PARAMETER ChatName
    What this machine calls itself - the SET-NAME argument. Left empty it comes from the table
    keyed by -Port. Only used with -AndStart.

.PARAMETER Trunks
    The system numbers to START-TRUNK to. Left empty they come from the same table.

.PARAMETER NoChatSetup
    Skip the CHAT-MON block entirely. For loading something that is not the chat server, or when
    the trunks are already up and must not be disturbed - see the START-TRUNK warning below.

.PARAMETER ChatSetupOnly
    Run ONLY the CHAT-MON block: no loader, no segment, nothing loaded. This is the retry when the
    load went in cleanly but CHAT-MON lost the race with the server claiming its admin port.

.EXAMPLE
    .\rt-load.ps1 -Port 9010 -Segment 2528
    Load CHATSV onto segment 2528 and point CHATSER at it.

.EXAMPLE
    .\rt-load.ps1 -Port 9102 -ChatSetupOnly
    Just put VIDDA's name and its two trunks back.
#>
param(
    [Parameter(Mandatory = $true)][int]$Port,

    # NOT mandatory any more, because -ChatSetupOnly loads nothing and needs no segment. It is
    # still required for a load, and the check below says so in one line rather than letting the
    # loader be handed a segment 0.
    [int]$Segment = 0,

    [string]$Program   = "CHATSV",
    [string]$RtName    = "CHATSER",
    [int]$Priority     = 75,
    [int]$Ring         = 2,

    # CHATLIB FIRST, and it is not optional for CHATSV any more.
    #
    # The server used to declare the twenty message kinds itself; it now IMPORTs
    # them, so the RT load needs the library exactly as the BRF-LINKER step does.
    # Without it END-LOAD answers NEGLECTING REFERENCES and the load is
    # INCOMPLETE - which is what happened the first time, and the script stopped
    # rather than leaving a half-loaded server running.
    [string[]]$Libraries = @("CHATLIB", "XMP-100-1-B02", "MON-CALL-1B-A00", "PLANC-1BANK-F00"),

    [string]$User       = "SYSTEM",
    [string]$Password   = "",
    [string]$RemoteHost = "127.0.0.1",

    # Start it once the description is set, and report what LIST-RT-DESCRIPTION says.
    [switch]$AndStart,

    # The machine's own name and its peers. Empty means "look them up by -Port" - see the table.
    [string]$ChatName = "",
    [int[]]$Trunks    = @(),

    [switch]$NoChatSetup,
    [switch]$ChatSetupOnly,

    # Print what WOULD be typed, and the prompt each line waits for, then stop. Opens no socket
    # and touches no machine.
    #
    # This exists because the step list is the whole script, and until now the only way to see it
    # was to run it against a live D100. Every trap in the header above is a step list that was
    # wrong, and one of them cost a reboot. A line that can be read on Windows in a second is a
    # line that gets read.
    [switch]$ShowSteps
)

# WHICH MACHINE IS THIS, WHAT IS IT CALLED, AND WHO DOES IT TRUNK TO.
#
# Keyed by TERMINAL PORT, because that is the one thing the caller always passes and the one
# thing that cannot be wrong: the port IS the machine.
#
#   port 9010 -> D100, FJELL      port 9102 -> D102, VIDDA      port 9003 -> D103, SKOGEN
#
# Ports read from lab-topology.json (verified 2026-08-08 against all three RetroCore.ini files);
# names and trunk lists read from boot/README.md, which records what was installed in each
# machine's own boot mode file on 2026-08-26.
#
# EVERY MACHINE TRUNKS TO BOTH OTHERS, not just to its neighbour. A machine learns names only
# from its DIRECT peers, so on a chain the far end stays a number; and dedup - the same line
# arriving twice by two routes and being shown once - cannot be exercised at all without the
# triangle. D100 had no START-TRUNK 103 for weeks because of exactly this.
$chatMachines = @{
    9010 = @{ Name = "FJELL";  System = 100; Trunks = @(102, 103) }
    9102 = @{ Name = "VIDDA";  System = 102; Trunks = @(100, 103) }
    9003 = @{ Name = "SKOGEN"; System = 103; Trunks = @(100, 102) }
}

# The CHAT-MON block runs on -AndStart (a fresh server has no name and no peers) or on its own
# with -ChatSetupOnly. It is skipped for anything that is not the chat server, because CHAT-MON
# talks to CHATSER's admin port and nothing else has one.
$doChatSetup = (-not $NoChatSetup) -and ($AndStart -or $ChatSetupOnly) -and ($RtName -eq "CHATSER")

if ($doChatSetup) {
    if ($ChatName -eq "" -or $Trunks.Count -eq 0) {
        $known = $chatMachines[$Port]
        if ($null -eq $known) {
            # NAME THE PORT AND STOP. Guessing a name here would set the WRONG name on a real
            # machine, and a wrong name is worse than none: a peer that has learned it will keep
            # using it, and direct messages then fail against a name nobody answers to.
            throw ("port $Port is not in this script's machine table, so its chat name and trunks " +
                   "are not known. Pass -ChatName and -Trunks, or -NoChatSetup, or add the machine " +
                   "to the table at the top of rt-load.ps1.")
        }
        if ($ChatName -eq "")     { $ChatName = $known.Name }
        if ($Trunks.Count -eq 0)  { $Trunks   = $known.Trunks }
    }
}

if (-not $ChatSetupOnly -and $Segment -le 0) {
    throw "-Segment is required unless -ChatSetupOnly is given."
}

$ndterm = Join-Path $PSScriptRoot "ndterm.ps1"
if (-not (Test-Path $ndterm)) {
    throw "ndterm.ps1 not found next to this script (looked in $PSScriptRoot)"
}

# EVERY STEP CARRIES THE PROMPT IT EXPECTS BACK, and $waits stays exactly parallel to $steps.
#
# TRAP 4, and it cost a reboot on 2026-08-23. This script used to send its steps on a fixed 1200 ms
# settle. That is not a wait, it is a bet, and it lost: the transcript shows "LOAD LOAD
# PLANC-1BANK-F00" and "NO SUCH FILE NAME", MON-CALL never loaded at all, and END-LOAD then ate
# EXIT-LOADER, RT and LOGOUT as answers to "NEGLECTING REFERENCES?". The loader was left holding a
# terminal, no further session on D100 would prompt, and the machine had to be restarted.
#
# THE PROMPTS BELOW WERE READ OFF D100 ONE AT A TIME on 2026-08-23, not remembered and not guessed.
# CHANGE-RT-DESCRIPTION asks, in this exact order:
#
#     RT-PROGRAM:  PRIORITY:  SEGMENT ONE:  SEGMENT TWO:  START ADDRESS:  RING:
#     INITIAL PAGE TABLE:  ALTERNATIVE PAGE TABLE:
#
# and then returns to the loader's own "*". If any of those strings ever changes, this script stops
# on the step that lost the thread and prints the transcript, which is the whole point - a wrong
# prompt should fail loudly at the step that caused it, not five commands later.
$steps = New-Object System.Collections.Generic.List[string]
$waits = New-Object System.Collections.Generic.List[string]

function Add-Step([string]$line, [string]$expect) {
    $steps.Add($line)
    $waits.Add($expect)
}

# The whole loader block is skipped by -ChatSetupOnly, which loads nothing and only puts the name
# and the trunks back. Its closing brace is marked below.
if (-not $ChatSetupOnly) {

# ---- STOP THE SERVER BEFORE LOADING OVER IT ---------------------------------------------
#
# MEASURED ON D100, 2026-08-29, and it cost two load attempts. If CHATSER is RUNNING, the
# CHANGE-RT-DESCRIPTION below answers
#
#     RT-PROGRAM IS ACTIVE
#
# and then does NOTHING. Every step after it still reports success, EXIT-LOADER and RT CHATSER
# both look normal, and the machine goes on running the OLD segment with the OLD start address.
# The script had no ABORT step at all, so the only thing that revealed it was reading the start
# address afterwards and seeing it unchanged.
#
# A load that silently does nothing is the worst failure this script can have - it is exactly
# the "green build of the wrong thing" that the whole build loop exists to prevent, one stage
# later. So: abort first, and make it explicit rather than hoping the program happens to be
# passive.
#
# ABORT on an already-passive program is harmless - it is how the machine says "stay stopped".
# And an aborted RT server cannot simply be RT'd again; it needs the fresh segment this script
# is about to give it, so there is nothing to put back on the failure path.
Add-Step "ABORT $RtName" "@"
Add-Step "LIST-RT-DESCRIPTION $RtName" "@"

Add-Step "RT-LOADER" "*"

# RELEASE THE SEGMENT BEFORE ALLOCATING IT.
#
# NEW-SEGMENT does NOT reopen an existing segment for rewriting. ND-60.051.8 page 47 is explicit:
# "Allocate a segment to be used in the current load operation. The <segment> must be an available
# FREE segment number." So on every rebuild after the first, the number we want is the one the
# server we are replacing is still sitting on, and NEW-SEGMENT answers PARAMETER NO. 1 IS ILLEGAL.
# Every LOAD after it then says ILLEGAL LOAD SEGMENT, END-LOAD is accepted, CHANGE-RT-DESCRIPTION
# is accepted, the program STARTS - and it starts on the OLD code, with the start address
# unchanged. MEASURED on D100 2026-08-30: a completely healthy-looking load of nothing.
#
# RELEASE-SEGMENT (same manual, Segment Operations) frees the entry. It refuses if an RT program
# is CURRENTLY USING the segment, which is why the ABORT above must come first - and it is safe
# to run when the segment does not exist yet, which is the first-ever load.
Add-Step "RELEASE-SEGMENT $Segment" "*"

# TRAILING COMMAS ARE LOAD-BEARING - see TRAP 3 above.
Add-Step "NEW-SEGMENT $Segment,$Ring,,," "*"

# "CHATSER REPLACING?" is answered Y. The loader asks it whenever the RT description already
# exists, which it does on every rebuild after the first, and a bare LOAD would sit on the
# question for ever. It does NOT ask on a first-ever load, so both answers are accepted here and
# the Y that follows is harmless at a bare "*" - the output checks below catch it if it is not.
Add-Step "LOAD $Program,$Segment," "REPLACING?|*"
Add-Step "Y" "*"

foreach ($lib in $Libraries) { Add-Step "LOAD $lib,$Segment," "*" }

# END-LOAD BEFORE CHANGE-RT-DESCRIPTION, not after. The worked example in the SIBAS manual shows
# the opposite order and here it answers THIS COMMAND IS NOT ALLOWED NOW.
#
# Waiting for "*" here also catches the bad case: unresolved references make END-LOAD ask
# "NEGLECTING REFERENCES?" instead, the wait times out, and the run stops with the transcript -
# rather than typing the rest of the script into that question, which is what happened before.
Add-Step "END-LOAD" "*"

# ONE PROMPT AT A TIME, and START ADDRESS deliberately blank - see TRAP 1 and TRAP 2.
Add-Step "CHANGE-RT-DESCRIPTION" "RT-PROGRAM:"
Add-Step $RtName      "PRIORITY:"
Add-Step "$Priority"  "SEGMENT ONE:"
Add-Step "$Segment"   "SEGMENT TWO:"
Add-Step ""           "START ADDRESS:"          # SEGMENT TWO      - one bank, always
Add-Step ""           "RING:"                   # START ADDRESS    - BLANK. Never anything else.
Add-Step "$Ring"      "INITIAL PAGE TABLE:"
Add-Step ""           "ALTERNATIVE PAGE TABLE:"
Add-Step ""           "*"
Add-Step "EXIT-LOADER" "@"

}   # end of the loader block - skipped by -ChatSetupOnly

if ($AndStart) {
    Add-Step "RT $RtName" "@"
    Add-Step "LIST-RT-DESCRIPTION $RtName" "@"
}

# THE MACHINE'S NAME AND ITS TRUNKS, which the load has just destroyed.
#
# SET-NAME FIRST, then the trunks: the name travels on the trunk Hello, so a trunk started before
# the name is set introduces this machine by its number and the peer remembers that until the next
# hello. Setting the name first costs nothing and removes the window.
#
# The prompt is "C-M: " - CHAT-MON prints it before every line it reads - so every step here waits
# for "C-M:" and only EXIT waits for the SINTRAN "@" back.
#
# THE RACE THIS CANNOT SOLVE, and it is written in the boot file too: "@RT CHATSER" returns when
# the RT program has been STARTED, not when it has claimed its admin port, and CHAT-MON talks to
# that port. How long the server needs has never been measured. If CHAT-MON gets there first this
# block fails, loudly, and the fix is one command - re-run with -ChatSetupOnly.
if ($doChatSetup) {
    Add-Step "CHAT-MON" "C-M:"

    # SET-NAME with an argument answers "this machine is now FJELL"; with none it CLEARS the name.
    # An empty -ChatName never reaches here - the table lookup above throws first - so this cannot
    # silently clear a name it failed to look up.
    Add-Step "SET-NAME $ChatName" "C-M:"

    # START-TRUNK only REGISTERS the peer. It does not have to reach it, and the server dials and
    # re-dials on its own clock, so running this while the other machine is down is fine.
    #
    # BUT: against a trunk that is ALREADY UP it answers "trunk added" and knocks that trunk DOWN
    # for about a minute before it heals itself. Harmless right after a load, where nothing is up
    # - which is the only place this block runs by default. Do NOT reach for -ChatSetupOnly on a
    # healthy machine just to check something.
    foreach ($sys in $Trunks) { Add-Step "START-TRUNK $sys" "C-M:" }

    # Whatever happened, the log says so. A trunk that did not register is then VISIBLE instead of
    # silent, which is the whole reason this line is here and not left to be typed later.
    Add-Step "LIST-TRUNKS" "C-M:"
    Add-Step "EXIT" "@"
}

Write-Host ""
if ($ChatSetupOnly) {
    Write-Host "=== $ChatName on port $Port - name and trunks only, nothing loaded ===" -ForegroundColor Cyan
} else {
    Write-Host "=== RT-load $Program onto segment $Segment as $RtName ===" -ForegroundColor Cyan
}
if ($doChatSetup) {
    Write-Host "    then SET-NAME $ChatName and trunks to $($Trunks -join ', ')" -ForegroundColor Cyan
}

if ($ShowSteps) {
    Write-Host ""
    Write-Host "    line sent                        waits for" -ForegroundColor DarkGray
    Write-Host "    -------------------------------  ---------" -ForegroundColor DarkGray
    for ($i = 0; $i -lt $steps.Count; $i++) {
        # A blank step is a bare CR answering a prompt with its default - shown as (CR) so it is
        # not mistaken for a missing line.
        $shown = $steps[$i]
        if ($shown -eq "") { $shown = "(CR)" }
        Write-Host ("    {0,-31}  {1}" -f $shown, $waits[$i])
    }
    Write-Host ""
    Write-Host "$($steps.Count) steps. Nothing was sent - remove -ShowSteps to run it." -ForegroundColor Yellow
    exit 0
}

$out = & $ndterm -Port $Port -User $User -Password $Password -RemoteHost $RemoteHost `
    -Steps $steps.ToArray() -StepWaits $waits.ToArray() -SettleMs 1200 2>&1 | Tee-Object -Variable teed
$text = ($teed | Out-String)

Write-Host ""

# CHECK, DO NOT HOPE. Each of these has been the actual outcome of a run at some point today.
$bad = @()
if ($text -match 'ILLEGAL PARAMETER TYPE')   { $bad += 'ILLEGAL PARAMETER TYPE - a prompt was answered with a command' }
# PARAMETER NO. n IS ILLEGAL has TWO quite different causes and the message used to name only
# one of them, which sent the 2026-08-29 run looking for a missing comma that was not missing.
#
#  - after NEW-SEGMENT it is almost always THE SEGMENT NUMBER, and there are TWO ways to get it
#    wrong. The number may be OUT OF RANGE - segment numbers here are OCTAL, so a decimal-looking
#    2601 is refused outright. Or, far more often on a rebuild, the number is perfectly legal and
#    simply NOT FREE: NEW-SEGMENT allocates a free segment and never reopens an existing one
#    (ND-60.051.8 p.47), so the segment the old server is sitting on is refused every time. That
#    is what RELEASE-SEGMENT above is for. Ask the machine which case it is - LIST-SEGMENT <n>
#    prints length 0 and no permission flags for a free segment, and real values plus OK for a
#    live one.
#  - anywhere else it is usually a dropped trailing comma, which lets the next line be eaten
#    as an answer to the prompt this one left open.
if ($text -match 'PARAMETER NO\.\s*\d+ IS ILLEGAL') {
    if ($text -match 'NEW-SEGMENT[^\r\n]*\r?\n\s*\r?\n?\s*PARAMETER NO') {
        $bad += ("PARAMETER NO. n IS ILLEGAL right after NEW-SEGMENT - segment $Segment was REFUSED. " +
                 "NEW-SEGMENT allocates a FREE segment; it does not reopen an existing one " +
                 "(ND-60.051.8 p.47). So the usual cause is that segment $Segment is still OCCUPIED - " +
                 "very often by the very server being replaced - and RELEASE-SEGMENT did not clear it. " +
                 "The other cause is a number out of range; segment numbers here are OCTAL. " +
                 "Ask the machine which it is: LIST-SEGMENT $Segment prints length 0 and no permission " +
                 "flags when free, and real values plus OK when occupied.")
    }
    else {
        $bad += 'PARAMETER NO. n IS ILLEGAL - a trailing comma was dropped and the next line was eaten as an answer'
    }
}
if ($text -match 'NOT ALLOWED NOW')          { $bad += 'THIS COMMAND IS NOT ALLOWED NOW - END-LOAD came too late' }
if ($text -match 'NO SUCH COMMAND')          { $bad += 'NO SUCH COMMAND - a SINTRAN command was typed inside the loader' }
if ($text -match 'UNDEFINED')                { $bad += 'UNDEFINED entries - a library is missing from -Libraries' }
if ($text -match 'NO SUCH FILE NAME')        { $bad += 'NO SUCH FILE NAME - a LOAD went in while the loader was still busy, or a library name is wrong' }
if ($text -match 'NEGLECTING REFERENCES')    { $bad += 'NEGLECTING REFERENCES - END-LOAD found unresolved references; the load is INCOMPLETE' }

# THE SILENT ONE. CHANGE-RT-DESCRIPTION refuses to touch a RUNNING program and says so ONCE, then
# every following step reports success and the machine keeps the OLD segment. Measured on D100
# 2026-08-29: the run ended "loaded onto segment 2601" while LIST-RT-DESCRIPTION still read
# segment 132B and start address 34617B - nothing had been loaded at all. The ABORT step near the
# top is the fix; this line is what proves the fix held on every future run.
if ($text -match 'RT-PROGRAM IS ACTIVE')     { $bad += 'RT-PROGRAM IS ACTIVE - the descriptor was NOT changed, the OLD segment is still in use, and NOTHING was loaded. Abort the program first.' }

# A step that never saw its prompt is the most important failure of all, because everything typed
# after it goes somewhere unintended. ndterm stops on it; this names it.
if ($text -match 'never saw its prompt')     { $bad += 'A STEP LOST ITS PROMPT - the run was stopped there. The loader may still be holding the terminal.' }

if ($bad.Count -gt 0) {
    # PRINT THE TRANSCRIPT. It used to say "read the transcript above" and there WAS no
    # transcript above: Tee-Object -Variable fills $teed but the pipeline output was being
    # captured into $out, so every line went nowhere. On 2026-08-22 this reported
    # "PARAMETER NO. n IS ILLEGAL" twice, with not one line of evidence, and the whole
    # sequence had to be retyped by hand on the machine to find out what had happened -
    # where it turned out to work perfectly. A failure report with no evidence is worse
    # than no report: it sends you looking at the machine instead of at the script.
    Write-Host "--- transcript ---" -ForegroundColor DarkGray
    Write-Host $text
    Write-Host "--- end transcript ---" -ForegroundColor DarkGray
    Write-Host ""
    Write-Host "*** THE LOAD DID NOT GO CLEANLY ***" -ForegroundColor Red
    foreach ($b in $bad) { Write-Host "    $b" -ForegroundColor Red }
    Write-Host "    Nothing has been proved about $RtName." -ForegroundColor Yellow
    exit 1
}

# A START ADDRESS of a couple of octal digits is the phantom-stack-overflow bug, caught here
# rather than three commands later when the program dies for no visible reason.
if ($AndStart) {
    $m = [regex]::Match($text, 'START ADDRESS:\s*(\d+)B')
    if ($m.Success -and $m.Groups[1].Value.Length -le 2) {
        Write-Host "*** START ADDRESS IS $($m.Groups[1].Value)B - THAT IS THE RING VALUE, NOT AN ADDRESS ***" -ForegroundColor Red
        Write-Host "    The program will die as a phantom STACK OVERFLOW. Load the segment again." -ForegroundColor Red
        exit 1
    }
    if ($text -match 'IN TIME QUEUE') {
        Write-Host "$RtName is ALIVE - in the time queue, sleeping in its TimeOut." -ForegroundColor Green
    } elseif ($text -match 'PASSIVE') {
        Write-Host "$RtName went PASSIVE - it opened nothing and gave up." -ForegroundColor Red
        Write-Host "    Usual causes, in order:" -ForegroundColor Yellow
        Write-Host "      1. something else already holds its port name (a foreground @$Program?)" -ForegroundColor Yellow
        Write-Host "      2. XROUT is not configured yet - the X-C step of the restart must finish first" -ForegroundColor Yellow
        Write-Host "      3. it was loaded BEFORE the last XMSG restart - that is what this script fixes" -ForegroundColor Yellow
        exit 1
    }
}

# DID THE NAME AND THE TRUNKS ACTUALLY TAKE? CHECK THE SERVER'S OWN WORDS, NOT THAT WE TYPED IT.
#
# Every reply here is built by CHATSV and its exact text is in the source:
#
#   SET-NAME <n>   -> "this machine is now <n>"   (buildNameReply, CHATSV.PLNC)
#                     "this machine has no name - it answers to D<number>" when it was cleared
#   START-TRUNK    -> "trunk added"               (buildAdmText, CHATSV.PLNC)
#   LIST-TRUNKS    -> "<sys> <name> <state>" per peer, or nothing at all when there are none
#
# So a missing "this machine is now" is not a cosmetic gap - it means CHAT-MON never got an answer
# out of the admin port, and NOTHING in this block took, trunks included.
if ($doChatSetup) {
    $chatBad = @()

    if ($text -notmatch [regex]::Escape("this machine is now $ChatName")) {
        $chatBad += "SET-NAME $ChatName was not confirmed - the server never answered 'this machine is now $ChatName'"
    }

    # One "trunk added" per trunk asked for. Fewer means one of them did not register, and which
    # one is then answered by LIST-TRUNKS below rather than guessed at here.
    $added = ([regex]::Matches($text, 'trunk added')).Count
    if ($added -lt $Trunks.Count) {
        $chatBad += "$added of $($Trunks.Count) trunks answered 'trunk added'"
    }

    # The LIST-TRUNKS answer, lifted out between its own echo and the next prompt, so the run's
    # log records the peer table the machine actually has - not the one that was asked for.
    # SKIP THE REST OF THE ECHOED COMMAND LINE FIRST - "[^\r\n]*\r?\n".
    #
    # Without it this matched from the word LIST-TRUNKS inside ndterm's own step
    # echo and captured the single "]" that ends it, so the run printed
    # "LIST-TRUNKS: ]" while the real answer sat on the next line. Measured on
    # D100 2026-08-28, the first time this ran against a live server.
    $lt = [regex]::Match($text, '(?s)LIST-TRUNKS[^\r\n]*\r?\n(.*?)C-M:')
    $trunkLines = ""
    if ($lt.Success) { $trunkLines = $lt.Groups[1].Value.Trim() }

    if ($chatBad.Count -gt 0) {
        Write-Host "--- transcript ---" -ForegroundColor DarkGray
        Write-Host $text
        Write-Host "--- end transcript ---" -ForegroundColor DarkGray
        Write-Host ""
        Write-Host "*** THE NAME AND TRUNKS DID NOT GO IN ***" -ForegroundColor Red
        foreach ($b in $chatBad) { Write-Host "    $b" -ForegroundColor Red }
        Write-Host ""
        Write-Host "    The LOAD itself was clean - $RtName is running on segment $Segment." -ForegroundColor Yellow
        Write-Host "    The usual cause is the race: @RT $RtName returns before the server has" -ForegroundColor Yellow
        Write-Host "    claimed its admin port, and CHAT-MON got there first. Retry with:" -ForegroundColor Yellow
        Write-Host ""
        Write-Host "        .\rt-load.ps1 -Port $Port -ChatSetupOnly" -ForegroundColor Yellow
        Write-Host ""
        Write-Host "    Until that comes back clean this machine has NO name and NO trunks:" -ForegroundColor Yellow
        Write-Host "    it will call itself D<number> and nothing will cross to the other machines." -ForegroundColor Yellow
        exit 1
    }

    Write-Host "$ChatName - name set, $added trunk(s) registered." -ForegroundColor Green
    if ($trunkLines -ne "") {
        Write-Host "    LIST-TRUNKS: $trunkLines" -ForegroundColor Green
    } else {
        # Not a failure on its own - a trunk registers before it is reachable, and LIST-TRUNKS can
        # legitimately be terse. Said out loud rather than hidden, because an empty peer table is
        # exactly what this whole block exists to prevent.
        Write-Host "    LIST-TRUNKS printed nothing that could be read back." -ForegroundColor Yellow
    }
}

if ($ChatSetupOnly) {
    exit 0
}

if ($AndStart) {
    # D.6 - AN RT-LOAD ORPHANS EVERY JOINED CLIENT. A fresh segment is a fresh server with an
    # EMPTY member table; a client that was joined before this load keeps its old serverMagic and
    # LOOKS COMPLETELY NORMAL - the tell is the missing echo of your own line, not an error. Said
    # here because it is easy to forget and invisible until somebody's message goes nowhere.
    Write-Host ""
    Write-Host "*** RESTART EVERY CLIENT THAT WAS JOINED BEFORE THIS LOAD ***" -ForegroundColor Yellow
    Write-Host "    An orphaned client shows a normal screen and a normal prompt. The tell is a" -ForegroundColor Yellow
    Write-Host "    missing echo of your own line - X-C LIST-NAMES will also show free seats" -ForegroundColor Yellow
    Write-Host "    where somebody believes they are seated." -ForegroundColor Yellow
}

Write-Host "loaded onto segment $Segment." -ForegroundColor Green
exit 0
