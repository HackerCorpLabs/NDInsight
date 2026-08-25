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

    # Where a RUNNING sync daemon takes fetch requests. Matches --sync-pull. When a daemon is up
    # the gate asks IT for the listing, so nothing has to tear the link down.
    [string] $PullFolder = 'E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\sync-pull',
    # THE DAEMON'S OWN --sync FOLDER, not "watch".
    #
    # This defaulted to "watch" while the running daemon was started with "--sync sync-out",
    # so a push staged the file, printed "staged ... 180967 bytes", and NOTHING EVER CARRIED
    # IT. The build then compiled whatever was already on the machine and looked fine. That is
    # the same failure shape as a-half-written-file-compiles-as-its-old-content: a green build
    # of the wrong source. Checked against the live daemon below so it can never drift again.
    [string] $WatchDir   = 'E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\sync-out',
    [int]    $Node       = 100,

    # How long the fetch may take before it is called a failure. A net, not a diagnosis - see the
    # note on --transfer-timeout in the runner. Raise it for a very large listing.
    [int]    $TransferTimeout = 240,
    # Kill a runner that is holding the seam, instead of refusing. OFF by default and it should
    # stay off: a hard kill skips the runner's clean exit, so D100 is left believing a link and a
    # conversation are still up. See Stop-Daemon.
    [switch] $ForceStopDaemon,

    # WHICH TRANSPORT REACHES D100. Without this the runner picks the Ethernet path and every
    # transfer times out with nothing arriving - see the pull below.
    [string] $Config = 'E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Live.Runner\topology-d19999-hdlc-server.json',

    [switch] $PullOnly,
    [switch] $PushOnly
)

$ErrorActionPreference = 'Continue'

$runner = 'E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Live.Runner\bin\Debug\net9.0\Xmsg.Live.Runner.exe'
if (-not (Test-Path $runner)) { Write-Error "runner not built: $runner"; exit 2 }
if (-not (Test-Path $ListingDir)) { New-Item -ItemType Directory -Force $ListingDir | Out-Null }

# DO NOT HARD-KILL THE RUNNER. THIS USED TO, AND IT IS A PRIME SUSPECT FOR THE XMSG DEATHS.
#
# It ran Stop-Process -Force on a process that was very likely mid-conversation with D100, and
# then printed its own warning that this "can leave D100 LAPB half-open" - as though the warning
# made it acceptable. Across 2026-08-20/21 XMSG went down ELEVEN times, and this function is in
# the path of a build gate that was being run repeatedly.
#
# THE RUNNER HAS A CLEAN EXIT and it was being thrown away: a cancelled token, Ctrl-C or the
# --for window elapsing all end the run properly, which includes sending DISC so the peer does
# NOT hold the link. Killing the process skips every bit of that, so the machine is left believing
# a conversation and a link are still up while the other end has simply vanished.
#
# So: refuse by default. A one-shot transfer ends by itself, so there is usually nothing to stop -
# and if a long-running daemon really is holding the seam, stopping it is the operator's call, not
# a side effect of asking for a listing.
function Stop-Daemon {
    $p = Get-Process -Name 'Xmsg.Live.Runner' -ErrorAction SilentlyContinue
    if (-not $p) { return }

    if (-not $ForceStopDaemon) {
        Write-Host ''
        Write-Host 'A runner is holding the seam and this script will NOT kill it.' -ForegroundColor Red
        Write-Host '  Hard-killing a runner mid-transfer leaves D100 believing the link is still' -ForegroundColor Yellow
        Write-Host '  up, and is a prime suspect for the XMSG deaths of 2026-08-20/21.' -ForegroundColor Yellow
        Write-Host '  Stop it cleanly yourself - Ctrl-C in its window, or let its --for window end -' -ForegroundColor Yellow
        Write-Host '  then run this again. To override anyway: -ForceStopDaemon' -ForegroundColor Yellow
        Write-Host ("  (process %d, started %s)" -f $p[0].Id, $p[0].StartTime) -ForegroundColor DarkGray
        exit 3
    }

    Write-Host 'FORCING the runner down - you asked for it.' -ForegroundColor Red
    $p | Stop-Process -Force -Confirm:$false
    Start-Sleep -Seconds 3
    Write-Host '  A hard stop leaves D100 LAPB half-open. Cycle the link before the next transfer:' -ForegroundColor Yellow
    Write-Host '    @X-C  ->  START-LINK,1362,,,-1,,' -ForegroundColor Yellow
}

if (-not $PullOnly) {
    # ---- ASK THE DAEMON WHERE IT IS ACTUALLY LOOKING ------------------------
    # Staging into a folder nobody watches is silent, and the build that follows
    # is a green build of the OLD source. So read the running daemon's own
    # command line and refuse if it does not match.
    $runner = Get-CimInstance Win32_Process -Filter "Name='Xmsg.Live.Runner.exe'" -ErrorAction SilentlyContinue
    if (-not $runner) {
        # NO DAEMON, NO DELIVERY - and this used to be printed as a promise.
        #
        # The message below the staging line says "the running sync daemon will
        # carry it", and nothing checked that one was running. The daemon ends
        # by itself when its --for window elapses (which is correct - a clean
        # exit sends DISC), so the window quietly expiring turns every later
        # push into a file written to a folder and forgotten. That happened
        # twice on 2026-08-22 and cost about ten minutes each time, waiting on
        # a transfer that was never going to start.
        Write-Host ''
        Write-Host 'NO SYNC DAEMON IS RUNNING - nothing would carry this file.' -ForegroundColor Red
        Write-Host '  Its --for window ends by itself; that is by design. Start another:' -ForegroundColor Yellow
        Write-Host '    Start-Process .\SRC\Xmsg.Live.Runner\bin\Debug\net9.0\Xmsg.Live.Runner.exe -ArgumentList @(' -ForegroundColor DarkGray
        Write-Host "      '--config','.\SRC\Xmsg.Live.Runner\topology-d19999-hdlc-server.json','--originate-from-seed'," -ForegroundColor DarkGray
        Write-Host "      '--sync','sync-out','--sync-pull','sync-pull','--sync-user','SYSTEM','--sync-to','100'," -ForegroundColor DarkGray
        Write-Host "      '127.0.0.1','10362','19999','3600') -RedirectStandardOutput sync.log -WindowStyle Hidden" -ForegroundColor DarkGray
        exit 5
    }
    if ($runner) {
        $cmd = $runner[0].CommandLine
        if ($cmd -match '--sync\s+(\S+)') {
            $daemonDir = (Resolve-Path (Join-Path (Split-Path $PSScriptRoot -Parent) $Matches[1]) -ErrorAction SilentlyContinue)
            $wantDir   = (Resolve-Path $WatchDir -ErrorAction SilentlyContinue)
            if ($daemonDir -and $wantDir -and ($daemonDir.Path -ne $wantDir.Path)) {
                Write-Host ''
                Write-Host "The running daemon watches $($daemonDir.Path)" -ForegroundColor Red
                Write-Host "but this would stage into  $($wantDir.Path)" -ForegroundColor Red
                Write-Host '  Nothing would be carried and the build would silently compile the OLD source.' -ForegroundColor Yellow
                Write-Host "  Re-run with -WatchDir '$($daemonDir.Path)'." -ForegroundColor Yellow
                exit 4
            }
        }
    }

    # ---- STAGE UNDER THE REMOTE NAME, NOT THE SOURCE'S -----------------------
    #
    # THE DAEMON DERIVES THE REMOTE NAME FROM THE STAGED FILE'S OWN NAME, so
    # -RemoteName was being IGNORED for every daemon transfer - accepted,
    # documented, and doing nothing. That is the worst kind of parameter.
    #
    # It cost a real failure and one that only showed up in the daemon's log:
    # CHATMON.PLNC staged under its own name became '"CHATMON:PLNC"' - 14
    # characters - and the FA open refused it, because the request packs the
    # specification, an apostrophe and the access letter into a 15-byte QFORM
    # string, leaving 13 for the name. The build then compiled the OLD CHATMN
    # on the machine and looked completely healthy. Same shape as staging into
    # a folder nobody watches: the push says something and nothing arrives.
    #
    # So the staged file is named for where it is GOING: CHATMN:PLNC becomes
    # CHATMN.PLNC. The colon is the SINTRAN type separator and a dot is the
    # Windows one; that swap is the whole translation.
    $remoteLeaf = ($RemoteName -replace ':', '.')
    $staged = Join-Path $WatchDir $remoteLeaf

    # AND CHECK IT FITS BEFORE SENDING IT. 13 characters, counted the way the
    # FA server counts them - the quotes a CREATE adds are inside the budget.
    if ($remoteLeaf.Length -gt 13) {
        Write-Host ''
        Write-Host "REMOTE NAME TOO LONG: '$RemoteName' is $($remoteLeaf.Length) characters." -ForegroundColor Red
        Write-Host '  The FA open packs the specification, an apostrophe and the access letter' -ForegroundColor Yellow
        Write-Host '  into a 15-byte QFORM string, so 13 is the most a name can be.' -ForegroundColor Yellow
        Write-Host '  Shorten it - CHATMON:PLNC (14) had to become CHATMN:PLNC (11).' -ForegroundColor Yellow
        exit 6
    }
    $text = [IO.File]::ReadAllText($Source) -replace "`r`n", "`n" -replace "`n", "`r`n"
    [IO.File]::WriteAllText($staged, $text)
    Write-Host "staged $staged  ($((Get-Item $staged).Length) bytes, CRLF)"
    Write-Host "the running sync daemon will carry it to $RemoteName; watch its log for 'written to'."
}

if ($PushOnly) { exit 0 }

if ($PullOnly) {
    # A RUNNING DAEMON IS NOW THE GOOD CASE, NOT THE OBSTACLE.
    #
    # This used to call Stop-Daemon unconditionally, because the only way to fetch was a one-shot
    # runner and two runners would fight over one link. Since the daemon learned to fetch, a daemon
    # that is up is exactly what we want: it carries the listing on the link it already holds, and
    # nothing gets torn down.
    #
    # Stop-Daemon is still called when there is a pull folder to work with - because then a runner
    # holding the seam really is in the way. It refuses to kill by default and that has not changed.
    $daemonForFetch = (Get-Process -Name 'Xmsg.Live.Runner' -ErrorAction SilentlyContinue) `
                      -and (Test-Path $PullFolder)
    if (-not $daemonForFetch) {
        Stop-Daemon
    }

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
    # THE TOPOLOGY FILE IS NOT OPTIONAL, and leaving it out cost four failed pulls in a row.
    #
    # "--self 19999" alone leaves the runner to pick a transport, and it picks the ETHERNET one -
    # which on this lab answers "NOTHING at all is arriving" for ever, because D100 is reached over
    # HDLC. The pull then climbs LAPB, sends its connect letter, gets no answer, and times out
    # after four minutes looking exactly like a machine-side refusal. Measured 2026-08-21: the same
    # pull with -Config topology-d19999-hdlc-server.json returned the listing first time.
    $pullLog = Join-Path $ListingDir 'last-pull.log'

    # ---- PREFER THE DAEMON'S HELD-OPEN LINK -------------------------------------------------
    #
    # A one-shot runner ends by sending DISC, and a DISC after a transfer is what kills the peer's
    # XMSG - fourteen deaths out of fourteen attempts, measured 2026-08-22. Worse, this gate ran
    # right after a compile, so it was the SECOND transfer-then-teardown of every build.
    #
    # If a sync daemon is up it already holds one link open and can fetch on it: drop a .req file
    # in its pull folder and wait. No connect, no DISC, and it works while the daemon is running -
    # which the one-shot could not do at all, because the two would fight over the same link.
    #
    # The one-shot is kept as the fallback for when no daemon is running. It still works; it just
    # costs an XMSG restart.
    $pullExit = 1
    $daemon = Get-Process -Name 'Xmsg.Live.Runner' -ErrorAction SilentlyContinue

    if ($daemon -and (Test-Path $PullFolder)) {
        $reqName = [System.IO.Path]::GetFileName($out)
        $target  = Join-Path $PullFolder $reqName
        foreach ($stale in @("$target", "$target.taken", "$target.req.failed")) {
            if (Test-Path $stale) { Remove-Item $stale -Force }
        }
        Set-Content -Path "$target.req" -Value $Listing -NoNewline
        Write-Host "asked the running daemon to fetch $Listing (no link teardown)"

        # The daemon scans every 5s and a big listing takes a couple of minutes.
        $deadline = (Get-Date).AddSeconds($TransferTimeout)
        while ((Get-Date) -lt $deadline) {
            if (Test-Path "$target.req.failed") {
                Write-Host "the daemon refused the request: $(Get-Content "$target.req.failed" -Raw)"
                break
            }
            if ((Test-Path $target) -and ((Get-Item $target).Length -gt 0)) {
                # Give the write a moment to finish, then take it.
                $sizeA = (Get-Item $target).Length
                Start-Sleep -Milliseconds 1500
                if ((Get-Item $target).Length -eq $sizeA) {
                    Move-Item $target $out -Force
                    $pullExit = 0
                    "fetched over the daemon's held-open link" | Set-Content $pullLog
                    break
                }
            }
            Start-Sleep -Seconds 2
        }

        if ($pullExit -ne 0) {
            Write-Host "the daemon did not produce the listing in ${TransferTimeout}s."
        }
    }

    if ($pullExit -ne 0) {
        if ($daemon) {
            # A one-shot CANNOT run while the daemon holds the link - it would climb LAPB against a
            # link that is already up and get nowhere. Say so instead of failing obscurely.
            Write-Host "NOT falling back to a one-shot pull: a sync daemon is running and holds the link."
            Write-Host "Either wait for it, or stop it and re-run - see task #106."
        } else {
            & $runner --config $Config --originate-from-seed --transfer-timeout $TransferTimeout `
                      --pull $Listing --pull-from $Node --pull-to $out *> $pullLog
            $pullExit = $LASTEXITCODE
        }
    }

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
        Write-Host ''

        # THE COMMONEST CAUSE AFTER AN XMSG RESTART, AND IT LOOKS LIKE A DEAD MACHINE.
        #
        # "REFUSED us: XRMFL (34)" / "the file server did not answer any of 4 connect letters"
        # usually means ACCESS, not a missing server. DEFINE-FRIEND-SYSTEM answers "Ok" whether or
        # not it did anything: it only applies to a row that EXISTS in the kernel system table, and
        # after a restart that table holds ONE row - D100 itself. The remote's row appears only when
        # the peer actually CONNECTS.
        #
        # Measured 2026-08-22: grant -> push REFUSED -> (the refused attempt created the row) ->
        # grant again -> push finished, 133477 bytes. So the FIRST refusal after a restart is
        # EXPECTED and is not worth diagnosing.
        #
        # Check the ROW COUNT, not LIST-FRIEND-SYSTEMS - that listed all five systems while every
        # transfer was refused, so it is not evidence.
        # AND THE COMMONEST CAUSE OF ALL: XMSG DIED BECAUSE OF THIS VERY BUILD.
        #
        # Established 2026-08-22 by testing all four cells: a TRANSFER FOLLOWED BY A COMPILE kills
        # XMSG in about four minutes, 14 times out of 14. A transfer then left idle survives (56
        # minutes proven); a compile with no transfer survives (13m40s proven). So it is the
        # combination, and push-then-compile is exactly what this script does.
        #
        # The fatal is XXHER (HDLC driver error) 56 words inside ZLKIL, LINK KILL: the compile
        # starves the HDLC driver, XMSG reaps the link and dies in the teardown path.
        #
        # DO NOT "tidy up" with STOP-LINK after a push - that IS link kill and it triggers the
        # crash in 13 SECONDS. It was tried as a mitigation and is the worst option available.
        Write-Host "If XMSG is DEAD, that is EXPECTED after a push+compile - it is not your change:" -ForegroundColor Yellow
        Write-Host "  recover with:  @MODE XMSGRE:MODE,,   (restores XMSG, TAD, COSMOS, file server, dumps)" -ForegroundColor Yellow
        Write-Host "  then the FIRST transfer is refused, grant, and retry - see below." -ForegroundColor Yellow
        Write-Host "  NEVER use STOP-LINK after a transfer: it is a trigger, not a fix." -ForegroundColor Yellow
        Write-Host ''
        Write-Host "If it showed XRMFL (34) or 'the file server did not answer', it is ACCESS:" -ForegroundColor Yellow
        Write-Host "  the FIRST refusal after an XMSG restart is EXPECTED - it creates the system-table row." -ForegroundColor Yellow
        Write-Host "  1. X-C LIST-UTILIZATION,,        -> 'System table ... In use' should have gone 1 -> 2" -ForegroundColor Yellow
        Write-Host "  2. X-C DEFINE-FRIEND-SYSTEM $Node" -ForegroundColor Yellow
        Write-Host "  3. run this again - it works on the second attempt." -ForegroundColor Yellow
        exit 1
    }

    Write-Host "listing: $out  ($((Get-Item $out).Length) bytes)"
    Write-Host ''

    # SINTRAN text can carry even parity in bit 7, so strip it before reading.
    $bytes = [IO.File]::ReadAllBytes($out)
    for ($i = 0; $i -lt $bytes.Length; $i++) { $bytes[$i] = $bytes[$i] -band 0x7F }
    $clean = [Text.Encoding]::ASCII.GetString($bytes)
    [IO.File]::WriteAllText(($out + '.txt'), $clean)

    # DROP EACH LINE'S COMMENT BEFORE LOOKING FOR A DIAGNOSTIC.
    #
    # The listing echoes the SOURCE, comments and all, and a source that documents a past mistake
    # quotes the compiler at itself. CHAT.PLNC carries the line
    #
    #     % 2494  (1834)/HANDLECOMM  *** ERROR   - ILLEGAL SYNTAX "SHOWMYNAME"
    #
    # in the comment explaining why the routine now exists - so a clean build reported two errors
    # and the gate would have blocked it. A gate that fails on a good build gets switched off, and
    # then it is not a gate at all.
    #
    # A real diagnostic is never behind a "%": PLANC emits it as its own line.
    $codeOnly = ($clean -split "`r?`n") | ForEach-Object { ($_ -split '%')[0] }
    $errors = $codeOnly | Where-Object { $_ -match '\*\*\* ERROR' }
    $warns  = $codeOnly | Where-Object { $_ -match '\*\*\* WARNING' }
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
