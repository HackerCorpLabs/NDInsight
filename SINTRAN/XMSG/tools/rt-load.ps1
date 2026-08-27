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

.EXAMPLE
    .\rt-load.ps1 -Port 9010 -Segment 2528
    Load CHATSV onto segment 2528 and point CHATSER at it.
#>
param(
    [Parameter(Mandatory = $true)][int]$Port,
    [Parameter(Mandatory = $true)][int]$Segment,

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
    [switch]$AndStart
)

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

Add-Step "RT-LOADER" "*"

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

if ($AndStart) {
    Add-Step "RT $RtName" "@"
    Add-Step "LIST-RT-DESCRIPTION $RtName" "@"
}

Write-Host ""
Write-Host "=== RT-load $Program onto segment $Segment as $RtName ===" -ForegroundColor Cyan

$out = & $ndterm -Port $Port -User $User -Password $Password -RemoteHost $RemoteHost `
    -Steps $steps.ToArray() -StepWaits $waits.ToArray() -SettleMs 1200 2>&1 | Tee-Object -Variable teed
$text = ($teed | Out-String)

Write-Host ""

# CHECK, DO NOT HOPE. Each of these has been the actual outcome of a run at some point today.
$bad = @()
if ($text -match 'ILLEGAL PARAMETER TYPE')   { $bad += 'ILLEGAL PARAMETER TYPE - a prompt was answered with a command' }
if ($text -match 'PARAMETER NO\.\s*\d+ IS ILLEGAL') { $bad += 'PARAMETER NO. n IS ILLEGAL - NEW-SEGMENT ate the next line' }
if ($text -match 'NOT ALLOWED NOW')          { $bad += 'THIS COMMAND IS NOT ALLOWED NOW - END-LOAD came too late' }
if ($text -match 'NO SUCH COMMAND')          { $bad += 'NO SUCH COMMAND - a SINTRAN command was typed inside the loader' }
if ($text -match 'UNDEFINED')                { $bad += 'UNDEFINED entries - a library is missing from -Libraries' }
if ($text -match 'NO SUCH FILE NAME')        { $bad += 'NO SUCH FILE NAME - a LOAD went in while the loader was still busy, or a library name is wrong' }
if ($text -match 'NEGLECTING REFERENCES')    { $bad += 'NEGLECTING REFERENCES - END-LOAD found unresolved references; the load is INCOMPLETE' }

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

Write-Host "loaded onto segment $Segment." -ForegroundColor Green
exit 0
