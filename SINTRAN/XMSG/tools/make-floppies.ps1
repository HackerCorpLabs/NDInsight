<#
.SYNOPSIS
    Build the two NDCHAT floppy images for the archive: INSTALL (binaries) and SOURCE.

.DESCRIPTION
    Writes, into dist\:

        NDCHAT-INSTALL-<date>.img     1.2 MB floppy (ndtool template floppy12, 616 pages):
                                      CHAT:PROG  CHAT-MON:PROG  CHATSV:BRF  CHATLIB:BRF
                                      CHATRT:MODE  CHATBOOT:MODE  README:TEXT   all under SYSTEM
        NDCHAT-SOURCE-<date>.img      every .PLNC / .MODE / .INCL / .SYMB in SINTRAN-CHAT\,
                                      plus README:TEXT
        NDCHAT-<date>.manifest.txt    every file on both images with its byte count, and the
                                      ndtool -t listing of each image read back after writing

    THE BINARIES COME OFF D100, NOT OUT OF THE REPO. The repo holds sources; the only place a
    built CHAT:PROG exists is the machine that compiled it. They are fetched through the RUNNING
    sync daemon's pull folder (a <name>.req file, the same way planc-build.ps1 fetches a
    listing), which holds one link open and tears nothing down. Proved binary-safe on
    2026-09-02: CHAT:PROG came back at 108544 bytes, the byte count FILE-STATISTICS reports on
    D100, with 28446 bytes above 0x7F intact.

    TEXT FILES ARE WRITTEN CRLF WITH EVEN PARITY (ndtool -p), which is how the machines' own
    files are encoded - boot\install-boot-files.py proved that by round-tripping a real boot
    file off D100. The repo keeps LF; the conversion happens in a scratch copy, the repo is
    not touched.

    EVERY PUT IS READ BACK. ndtool -x extracts each image into a scratch folder and every file
    is compared byte for byte (text after stripping parity) with what was put in. A manifest
    written from what was ASKED for would prove nothing.

.PARAMETER Stamp
    The date in the image names. Defaults to today, yyyy-MM-dd.

.PARAMETER SkipPull
    Use the binaries already in sync-pull\ instead of asking the daemon for fresh ones. For
    re-running the packaging after a pull that is known good.

.PARAMETER PullTimeoutSeconds
    How long to wait for each pull. A 100 KB PROG takes about a minute.

.EXAMPLE
    .\tools\make-floppies.ps1
    Pull the four binaries from D100 and write both images and the manifest into dist\.

.NOTES
    Needs a running sync daemon (tools\start-relay.ps1) for the pull, and ndtool.exe.
    Attaching the resulting image to a RetroCore machine: 'attach <device> <file>' at the
    RetroCore console, or a 'mount floppy0 ...' line - see DOC\manuals\CHAT-ADMIN-MANUAL.md
    section 3.1, and read its UNVERIFIED note before relying on it.
#>
[CmdletBinding()]
param(
    [string] $Stamp = (Get-Date).ToString('yyyy-MM-dd'),
    [switch] $SkipPull,
    [int]    $PullTimeoutSeconds = 300,
    [string] $NdTool = 'E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build\ndtool.exe'
)

$ErrorActionPreference = 'Stop'
$xmsg     = Split-Path -Parent $PSScriptRoot
$pullDir  = Join-Path $xmsg 'sync-pull'
$srcDir   = Join-Path $xmsg 'SINTRAN-CHAT'
$distDir  = Join-Path $xmsg 'dist'
$filesDir = Join-Path $distDir 'floppy-files'
$scratch  = Join-Path $env:TEMP ("ndchat-floppy-" + [guid]::NewGuid().ToString('N').Substring(0, 8))

function Fail([string] $what) {
    Write-Host ''
    Write-Host "STOPPED: $what" -ForegroundColor Red
    exit 1
}
function Step([string] $text) { Write-Host ''; Write-Host "== $text" -ForegroundColor Cyan }

if (-not (Test-Path $NdTool)) { Fail "no ndtool at $NdTool" }
New-Item -ItemType Directory -Force $distDir, $scratch | Out-Null

# The four binaries and the SINTRAN name each is fetched by / stored under.
$binaries = @(
    @{ Host = 'CHAT.PROG';     Spec = 'CHAT:PROG';     Nd = 'SYSTEM/CHAT:PROG' }
    @{ Host = 'CHAT-MON.PROG'; Spec = 'CHAT-MON:PROG'; Nd = 'SYSTEM/CHAT-MON:PROG' }
    @{ Host = 'CHATSV.BRF';    Spec = 'CHATSV:BRF';    Nd = 'SYSTEM/CHATSV:BRF' }
    @{ Host = 'CHATLIB.BRF';   Spec = 'CHATLIB:BRF';   Nd = 'SYSTEM/CHATLIB:BRF' }
)

# ---------------------------------------------------------------------------
Step "1. the binaries, off D100 through the sync daemon"
# ---------------------------------------------------------------------------
if (-not $SkipPull) {
    $daemon = @(Get-CimInstance Win32_Process -Filter "Name='Xmsg.Live.Runner.exe'" -ErrorAction SilentlyContinue |
                Where-Object { $_.CommandLine -match '--sync-pull\s+(\S+)' })
    if ($daemon.Count -eq 0) { Fail 'no sync daemon is running - start one with tools\start-relay.ps1, or pass -SkipPull to package what sync-pull already holds' }
    $daemon[0].CommandLine -match '--sync-pull\s+(\S+)' | Out-Null
    $daemonPull = $Matches[1]
    if (-not [IO.Path]::IsPathRooted($daemonPull)) { $daemonPull = Join-Path $xmsg $daemonPull }
    if ((Resolve-Path $daemonPull).Path -ne (Resolve-Path $pullDir).Path) {
        Fail "the daemon pulls into $daemonPull, not $pullDir - a request dropped here would sit unread"
    }

    # Log lines start with a timestamp in this exact shape, so a string compare on the line
    # picks out entries written after the requests went in.
    $requestedAt = (Get-Date).ToString('yyyy-MM-dd HH:mm:ss')
    foreach ($b in $binaries) {
        $target = Join-Path $pullDir $b.Host
        foreach ($stale in @($target, "$target.req", "$target.req.failed", "$target.taken")) {
            if (Test-Path $stale) { Remove-Item $stale -Force }
        }
        Set-Content -Path "$target.req" -Value $b.Spec -NoNewline
        Write-Host "   asked for $($b.Spec)"
    }

    # The daemon scans every five seconds and answers one request at a time.
    $deadline = (Get-Date).AddSeconds($PullTimeoutSeconds * $binaries.Count)
    $pending  = @($binaries | ForEach-Object { $_.Host })
    while ($pending.Count -gt 0 -and (Get-Date) -lt $deadline) {
        Start-Sleep -Seconds 5
        $still = @()
        foreach ($h in $pending) {
            $target = Join-Path $pullDir $h
            if (Test-Path "$target.req.failed") { Fail "the daemon refused $h : $(Get-Content "$target.req.failed" -Raw)" }
            # A refused OpenFile is reported only in the daemon's log, not as a .req.failed - seen
            # 2026-09-02 when CHATSV:BRF was asked for while CHATSV:MODE had just deleted it
            # (SINTRAN error 110). Waiting the full timeout for that would be a bet, not a wait.
            $spec = ($binaries | Where-Object { $_.Host -eq $h }).Spec
            $failed = Select-String -Path (Join-Path $xmsg 'sync-relay.log') -Pattern "FAILED pull $([regex]::Escape($spec)) " -ErrorAction SilentlyContinue |
                      Where-Object { $_.Line -gt $requestedAt } | Select-Object -Last 1
            if ($failed) { Fail "the daemon could not pull $spec :`n   $($failed.Line)`n   (is a build deleting or rewriting it right now?)" }
            # A file that exists but is still being written grows between two looks.
            if ((Test-Path $target) -and (-not (Test-Path "$target.req"))) {
                $a = (Get-Item $target).Length; Start-Sleep -Seconds 3; $b2 = (Get-Item $target).Length
                if ($a -gt 0 -and $a -eq $b2) { Write-Host "   landed $h  $a byte(s)" -ForegroundColor Green; continue }
            }
            $still += $h
        }
        $pending = $still
    }
    if ($pending.Count -gt 0) { Fail "not pulled within the timeout: $($pending -join ', ')" }
}

foreach ($b in $binaries) {
    $p = Join-Path $pullDir $b.Host
    if (-not (Test-Path $p) -or (Get-Item $p).Length -eq 0) { Fail "$($b.Host) is missing or empty in $pullDir" }
}

# ---------------------------------------------------------------------------
Step "2. text files, converted to CRLF in scratch (the repo stays LF)"
# ---------------------------------------------------------------------------
function ToCrlf([string] $from, [string] $to) {
    $text = [IO.File]::ReadAllText($from)
    $text = $text -replace "`r`n", "`n" -replace "`n", "`r`n"
    if (-not $text.EndsWith("`r`n")) { $text += "`r`n" }
    # ASCII only - a byte above 0x7F would collide with the parity bit ndtool -p sets.
    $bad = [regex]::Matches($text, '[^\x00-\x7F]')
    if ($bad.Count -gt 0) { Fail "$from holds $($bad.Count) non-ASCII character(s); the machine cannot read them" }
    [IO.File]::WriteAllText($to, $text, [Text.Encoding]::ASCII)
}

$textScratch = Join-Path $scratch 'text'
New-Item -ItemType Directory -Force $textScratch | Out-Null

# Install floppy text: the RT-load mode file, the boot block, the README.
$installText = @(
    @{ From = (Join-Path $filesDir 'CHATRT.MODE.txt');   Nd = 'SYSTEM/CHATRT:MODE';   Host = 'CHATRT.MODE' }
    @{ From = (Join-Path $filesDir 'CHATBOOT.MODE.txt'); Nd = 'SYSTEM/CHATBOOT:MODE'; Host = 'CHATBOOT.MODE' }
    @{ From = (Join-Path $filesDir 'README-INSTALL.txt'); Nd = 'SYSTEM/README:TEXT';  Host = 'README.TEXT' }
)
foreach ($t in $installText) {
    if (-not (Test-Path $t.From)) { Fail "missing $($t.From)" }
    ToCrlf $t.From (Join-Path $textScratch $t.Host)
}

# Source floppy text: everything a rebuild on a machine with PLANC needs.
$sourceText = @()
foreach ($f in (Get-ChildItem $srcDir -File | Where-Object { $_.Extension -in '.PLNC', '.MODE', '.INCL', '.SYMB' } | Sort-Object Name)) {
    $type = $f.Extension.TrimStart('.')
    $sourceText += @{ From = $f.FullName; Nd = "SYSTEM/$($f.BaseName):$type"; Host = $f.Name }
    ToCrlf $f.FullName (Join-Path $textScratch $f.Name)
}
$srcReadme = Join-Path $filesDir 'README-SOURCE.txt'
if (-not (Test-Path $srcReadme)) { Fail "missing $srcReadme" }
ToCrlf $srcReadme (Join-Path $textScratch 'README-SOURCE.TEXT')
$sourceText += @{ From = $srcReadme; Nd = 'SYSTEM/README:TEXT'; Host = 'README-SOURCE.TEXT' }

# ---------------------------------------------------------------------------
Step "3. write the images"
# ---------------------------------------------------------------------------
$installImg = Join-Path $distDir "NDCHAT-INSTALL-$Stamp.img"
$sourceImg  = Join-Path $distDir "NDCHAT-SOURCE-$Stamp.img"
$manifest   = Join-Path $distDir "NDCHAT-$Stamp.manifest.txt"
foreach ($img in @($installImg, $sourceImg)) { if (Test-Path $img) { Remove-Item $img -Force } }

function NdPut([string] $img, [string] $hostFile, [string] $ndPath, [bool] $text) {
    # --overwrite is not optional: without it ndtool prints "skipped (exists)" and exits 0.
    $args = @()
    if ($text) { $args += '-p' }
    $args += @('--put', $hostFile, $ndPath, '--overwrite', $img)
    $out = & $NdTool @args 2>&1
    if ($LASTEXITCODE -ne 0 -or ($out -match 'skipped|error|Error|no space|full')) {
        Fail "ndtool --put $ndPath failed:`n$($out -join "`n")"
    }
    # PUBLIC read - ndtool 0.0.6 sets it already; an older ndtool left PUBLIC=NONE and SYSTEM
    # then got NOT READ ACCESS, so it is set explicitly and cheaply here.
    & $NdTool --chmod 'PUBLIC+R' $ndPath $img 2>&1 | Out-Null
}

$entries = @()   # what went on which image, for the manifest and the read-back

& $NdTool --create floppy12 --name NDCHAT $installImg 2>&1 | Out-Null
if (-not (Test-Path $installImg)) { Fail "ndtool did not create $installImg" }
foreach ($b in $binaries) {
    $h = Join-Path $pullDir $b.Host
    NdPut $installImg $h $b.Nd $false
    $entries += @{ Img = $installImg; Nd = $b.Nd; Host = $h; Text = $false; Bytes = (Get-Item $h).Length }
    Write-Host "   install  $($b.Nd)  $((Get-Item $h).Length) byte(s)"
}
foreach ($t in $installText) {
    $h = Join-Path $textScratch $t.Host
    NdPut $installImg $h $t.Nd $true
    $entries += @{ Img = $installImg; Nd = $t.Nd; Host = $h; Text = $true; Bytes = (Get-Item $h).Length }
    Write-Host "   install  $($t.Nd)  $((Get-Item $h).Length) byte(s)"
}

& $NdTool --create floppy12 --name NDCHATSRC $sourceImg 2>&1 | Out-Null
if (-not (Test-Path $sourceImg)) { Fail "ndtool did not create $sourceImg" }
foreach ($t in $sourceText) {
    $h = Join-Path $textScratch $t.Host
    NdPut $sourceImg $h $t.Nd $true
    $entries += @{ Img = $sourceImg; Nd = $t.Nd; Host = $h; Text = $true; Bytes = (Get-Item $h).Length }
    Write-Host "   source   $($t.Nd)  $((Get-Item $h).Length) byte(s)"
}

# ---------------------------------------------------------------------------
Step "4. read both images back and compare every file"
# ---------------------------------------------------------------------------
function BytesEqual([byte[]] $a, [byte[]] $b) {
    if ($a.Length -ne $b.Length) { return $false }
    for ($i = 0; $i -lt $a.Length; $i++) { if ($a[$i] -ne $b[$i]) { return $false } }
    return $true
}

$bad = 0
foreach ($img in @($installImg, $sourceImg)) {
    # TWO extractions. -p strips the parity bit on the way out, which is right for the text
    # (it compares against the CRLF scratch copy) and WRONG for a binary - the first run of this
    # script extracted everything with -p and reported all four binaries as damaged, when the
    # only thing damaged was the check.
    $outText = Join-Path $scratch ([IO.Path]::GetFileNameWithoutExtension($img) + '-text')
    $outBin  = Join-Path $scratch ([IO.Path]::GetFileNameWithoutExtension($img) + '-bin')
    New-Item -ItemType Directory -Force $outText, $outBin | Out-Null
    & $NdTool -p -x -o $outText $img 2>&1 | Out-Null
    & $NdTool    -x -o $outBin  $img 2>&1 | Out-Null
    foreach ($e in ($entries | Where-Object { $_.Img -eq $img })) {
        $name = ($e.Nd -split '/')[-1] -replace ':', '.'
        $out  = if ($e.Text) { $outText } else { $outBin }
        $back = Get-ChildItem $out -Recurse -File | Where-Object { $_.Name -eq $name } | Select-Object -First 1
        if (-not $back) { Write-Host "   MISSING after read-back: $($e.Nd)" -ForegroundColor Red; $bad++; continue }
        $same = BytesEqual ([IO.File]::ReadAllBytes($e.Host)) ([IO.File]::ReadAllBytes($back.FullName))
        if ($same) { Write-Host "   ok  $($e.Nd)" }
        else       { Write-Host "   DIFFERS after read-back: $($e.Nd) ($($back.Length) vs $($e.Bytes))" -ForegroundColor Red; $bad++ }
    }
}
if ($bad -gt 0) { Fail "$bad file(s) did not survive the round trip - the images are not to be trusted" }

# ---------------------------------------------------------------------------
Step "5. the manifest"
# ---------------------------------------------------------------------------
$lines = @()
$lines += "NDCHAT floppy images, built $((Get-Date).ToString('yyyy-MM-dd HH:mm'))"
$lines += "git: $(git -C $xmsg rev-parse --short HEAD 2>$null) on $(git -C $xmsg rev-parse --abbrev-ref HEAD 2>$null)"
$lines += ''
foreach ($img in @($installImg, $sourceImg)) {
    $lines += "$(Split-Path -Leaf $img)  $((Get-Item $img).Length) bytes  (ndtool floppy12, volume $(if ($img -eq $installImg) {'NDCHAT'} else {'NDCHATSRC'}))"
    foreach ($e in ($entries | Where-Object { $_.Img -eq $img })) {
        $kind = if ($e.Text) { 'text, CRLF, even parity' } else { 'binary' }
        $lines += ('  {0,-28} {1,8} bytes  {2}' -f $e.Nd, $e.Bytes, $kind)
    }
    $lines += ''
    $lines += "  ndtool -t $(Split-Path -Leaf $img):"
    $lines += (& $NdTool -t $img 2>&1 | ForEach-Object { "    $_" })
    $lines += ''
}
$lines += 'Binaries were pulled off D100 through the sync daemon; every file was extracted again'
$lines += 'from the finished image and compared byte for byte with what was put in.'
Set-Content -Path $manifest -Value ($lines -join "`r`n") -Encoding ASCII

Remove-Item $scratch -Recurse -Force -ErrorAction SilentlyContinue

Write-Host ''
Write-Host "DONE" -ForegroundColor Green
Write-Host "  $installImg"
Write-Host "  $sourceImg"
Write-Host "  $manifest"
exit 0
