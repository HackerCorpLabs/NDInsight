<#
.SYNOPSIS
    Carries a PLANC source to D100, compiles it there, brings the LISTING back, and reads it.

.DESCRIPTION
    Why this exists: the compiler prints its diagnostics as it goes, and on a source of a
    thousand lines they scroll off a 24-line screen long before the summary appears. The
    "0 DIAGNOSTICS" you can see at the bottom belongs to the SECOND pass - the loader - and it
    sits happily underneath a COMPILE that had three errors. That is how a build with

        1055  (395)/DRAINPORT  *** ERROR   - ILLEGAL DATA TYPE "AND"

    in it was read as clean twice in a row. The listing is the only honest record, so this
    script fetches it and greps it, and reports FAILURE on any "*** ERROR".

    Three things it also handles, each learned the hard way on D100:

      CRLF. A source carried over with bare LF makes the compiler answer LINE IS TOO LONG on
      every line - including a one-character line, which is what proves it is not about length.

      THE HALF-OPEN LINK. Hard-killing the runner leaves D100's LAPB believing the connection
      is still up, so a fresh SABM goes unanswered for ever and every transfer stalls in
      "status Starting". The cure is to cycle the link from X-COMM on the machine:
          X-C: STOP-LINK <CR> 1362 <CR> <CR>
          X-C: START-LINK,1362,,,-1,,
      This script cannot do that itself (it drives no terminal); it tells you when to.

      ONE SEAM AT A TIME. The push daemon and a one-shot pull cannot both own the HDLC seam,
      so the daemon is stopped for the pull and left stopped - restart it yourself afterwards.

.PARAMETER Source
    Path to the PLANC source on Windows. Defaults to the chat client.

.PARAMETER RemoteName
    The name to give it on the machine. REMEMBER THE 13-CHARACTER CEILING: our transfer packs
    the specification, an apostrophe and the access letter into a 15-byte QFORM string, and a
    CREATE carries its own quotes inside that budget, so "CHAT-CC:MODE" (14) is refused and
    "CHATCC:MODE" (13) fits.

.PARAMETER ModeFile
    The build MODE file to run on the machine.

.PARAMETER ListingDir
    Where to put the fetched listing.

.EXAMPLE
    .\planc-build.ps1
    Carry CHAT.PLNC, then tell you to run the MODE file and re-run with -PullOnly.

.EXAMPLE
    .\planc-build.ps1 -PullOnly
    Just fetch CHAT:LIST and analyse it.

.NOTES
    Leaves no .NET host behind: every runner it starts is a one-shot that exits by itself.

    That claim was NOT true until 2026-08-18. A one-shot held the seam for the whole --for window -
    an hour by default - after its file had been written, so this script blocked and the only way
    out was killing the process, which leaves D100's LAPB half-open. The runner now ends the run
    when the transfer finishes, and gives up with a non-zero exit if it never does.
#>
[CmdletBinding()]
param(
    [string] $Source     = 'E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SINTRAN-CHAT\CHAT.PLNC',
    [string] $RemoteName = 'CHAT:PLNC',
    [string] $Listing    = 'CHAT:LIST',
    [string] $ListingDir = 'E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\listings',
    [string] $WatchDir   = 'E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\watch',
    [int]    $Node       = 100,

    # How long the fetch may take before it is called a failure. A net, not a diagnosis - see the
    # note on --transfer-timeout in the runner. Raise it for a very large listing.
    [int]    $TransferTimeout = 240,
    [switch] $PullOnly,
    [switch] $PushOnly
)

$ErrorActionPreference = 'Continue'

$runner = 'E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Live.Runner\bin\Debug\net9.0\Xmsg.Live.Runner.exe'
if (-not (Test-Path $runner)) { Write-Error "runner not built: $runner"; exit 2 }
if (-not (Test-Path $ListingDir)) { New-Item -ItemType Directory -Force $ListingDir | Out-Null }

function Stop-Daemon {
    $p = Get-Process -Name 'Xmsg.Live.Runner' -ErrorAction SilentlyContinue
    if ($p) {
        Write-Host 'stopping the sync daemon so the seam is free...'
        $p | Stop-Process -Force -Confirm:$false
        Start-Sleep -Seconds 3
        Write-Host '  NOTE: a hard stop can leave D100 LAPB half-open. If the next transfer' -ForegroundColor Yellow
        Write-Host '  sits in "status Starting", cycle the link on the machine:' -ForegroundColor Yellow
        Write-Host '    @X-COMM  ->  STOP-LINK / 1362 / <CR>  ->  START-LINK,1362,,,-1,,' -ForegroundColor Yellow
    }
}

if (-not $PullOnly) {
    # Stage as CRLF. Anything else and the compiler cannot find an end-of-line at all.
    $staged = Join-Path $WatchDir (Split-Path $Source -Leaf)
    $text = [IO.File]::ReadAllText($Source) -replace "`r`n", "`n" -replace "`n", "`r`n"
    [IO.File]::WriteAllText($staged, $text)
    Write-Host "staged $staged  ($((Get-Item $staged).Length) bytes, CRLF)"
    Write-Host "the running sync daemon will carry it to $RemoteName; watch its log for 'written to'."
}

if ($PushOnly) { exit 0 }

if ($PullOnly) {
    Stop-Daemon

    # Stamped BEFORE the transfer, so "did this file arrive just now" is answerable afterwards.
    $startedAt = Get-Date

    # THE NAME COMES FROM -Listing. This used to be hard-coded to 'CHAT.LIST' whatever was asked
    # for, so "-Listing CHATSV:LIST" fetched the server listing and wrote it OVER the client one -
    # and every check below then reported on a file whose name said something else. A wrong answer
    # under a right-looking name is the worst kind.
    $out = Join-Path $ListingDir ($Listing -replace ':', '.')

    # MOVE ANY EXISTING LISTING ASIDE FIRST, and this is not tidiness.
    #
    # The runner REFUSES to overwrite - "already exists; move it or give --pull-to" - which is a
    # deliberate non-destructive default and the right one. But it means a pull that never ran
    # leaves yesterday's file sitting there, and the old existence test below then called that
    # success. MEASURED 2026-08-18: two listings from the previous evening were read as the output
    # of a compile that had just run, and the conclusion drawn from them - that the build had used
    # the wrong source - was wrong and cost a round of re-checking on the machine.
    #
    # So: keep the old file (it may be the one you want to diff against), just get it out of the
    # way, and let the runner do a clean write.
    if (Test-Path $out) {
        $aside = "$out.previous"
        Move-Item $out $aside -Force
        Write-Host "moved the previous listing to $aside"
    }

    Write-Host "fetching $Listing from node $Node ..."
    # NO --announce-restart. It is what makes the peer refuse the conversation.
    #
    # MEASURED 2026-08-18: with that flag a push/pull against a freshly brought-up D100 is refused,
    # and the peer names it - XDTYP 0x0017 InitializationNak carrying XRDDF, "Another port already
    # has this name". The announce claims a name D100 already holds. Without the flag the identical
    # transfer completes, and so does the next one straight after it.
    #
    # THE OUTPUT GOES TO A FILE, not Out-Null. Discarding it throws away the one line that says
    # WHY a pull did nothing, which is exactly the line you need.
    $pullLog = Join-Path $ListingDir 'last-pull.log'
    & $runner --self 19999 --originate-from-seed --transfer-timeout $TransferTimeout `
              --pull $Listing --pull-from $Node --pull-to $out *> $pullLog
    $pullExit = $LASTEXITCODE

    # A PULL THAT DID NOTHING MUST NOT LOOK LIKE ONE THAT WORKED. Three separate things are checked
    # because each has been seen alone:
    #   the exit code   - the runner already returns 1 when it refuses; nobody was reading it;
    #   the file exists - the transfer may have died before writing anything;
    #   the file is NEW - a stale file is the trap this whole block exists to close.
    $fresh = (Test-Path $out) -and ((Get-Item $out).LastWriteTime -gt $startedAt)

    if ($pullExit -ne 0 -or -not $fresh) {
        Write-Host "FAILED: no listing came back (exit $pullExit)." -ForegroundColor Red
        Write-Host "The runner said:" -ForegroundColor Red
        Get-Content $pullLog -Tail 6 | ForEach-Object { Write-Host "  $_" -ForegroundColor Red }
        Write-Host "If it showed repeated SABM, the link is half-open - cycle it on the machine:" -ForegroundColor Red
        Write-Host "  X-C: STOP-LINK / 1362 / <CR>   then   START-LINK,1362,,,-1,," -ForegroundColor Red
        exit 1
    }

    Write-Host "listing: $out  ($((Get-Item $out).Length) bytes)"
    Write-Host ''

    # SINTRAN text can carry even parity in bit 7, so strip it before reading.
    $bytes = [IO.File]::ReadAllBytes($out)
    for ($i = 0; $i -lt $bytes.Length; $i++) { $bytes[$i] = $bytes[$i] -band 0x7F }
    $clean = [Text.Encoding]::ASCII.GetString($bytes)
    [IO.File]::WriteAllText(($out + '.txt'), $clean)

    $errors = ($clean -split "`r?`n") | Where-Object { $_ -match '\*\*\* ERROR' }
    $warns  = ($clean -split "`r?`n") | Where-Object { $_ -match '\*\*\* WARNING' }
    $summary = ($clean -split "`r?`n") | Where-Object { $_ -match 'LINES COMPILED' }

    $summary | ForEach-Object { Write-Host $_.Trim() }
    Write-Host ''

    if ($warns.Count -gt 0) {
        Write-Host "$($warns.Count) warning(s):" -ForegroundColor Yellow
        $warns | Select-Object -First 20 | ForEach-Object { Write-Host ('  ' + $_.Trim()) -ForegroundColor Yellow }
    }

    if ($errors.Count -gt 0) {
        Write-Host "$($errors.Count) ERROR(S) - THE BUILD IS NOT CLEAN:" -ForegroundColor Red
        $errors | ForEach-Object { Write-Host ('  ' + $_.Trim()) -ForegroundColor Red }
        exit 1
    }

    Write-Host 'no *** ERROR in the listing.' -ForegroundColor Green
    exit 0
}
