<#
    install-screen-h.ps1 - THE FALLBACK. Write PLANC-SCREEN-H and TESTUI straight
    into a SINTRAN disk image.

    USE deploy-over-xmsg.ps1 INSTEAD unless the machine cannot be talked to.
    That route carries the files over XMSG/COSMOS with the machines still
    running and nothing stopped. This one costs a stop and a boot of every
    machine, and is here for two cases only: the transport is down, or the file
    has to exist before the machine can bring its network up (boot/mode files).

    PLANC-SCREEN-H is not part of a stock SINTRAN. It is a 1986/87 floppy with no
    ND article number, no PD sheet and no installer program of its own - so the
    install IS a file copy, which is what this script does.

    WHAT IT PUTS ON

      SCREEN:SYMB       the interface, $INCLUDEd by the source at compile time
      INTRF1B:BRF       the 1-bank runtime, linked at build time. The vendor calls
                        it INTERF-1B, which is 15 characters with quotes and so
                        cannot cross the file transfer - both routes use the short
                        name so the LOAD line in TESTUI:MODE matches either way.
      TESTUI:PLNC       the program source
      TESTUI:MODE       the build job

    Everything else the build needs - MON-CALL-1B-A00:BRF and PLANC-1BANK-F00:BRF
    - is already on D100 and is NOT touched.

    IT WILL NOT RUN WHILE A MACHINE IS UP. An image cannot be written underneath a
    running RetroCore: the machine holds its own view of the disk and will write it
    back over whatever landed here. This script CHECKS and STOPS - it never closes
    anything itself. Close the machine window yourself and run it again.

    USAGE

      .\install-screen-h.ps1
      .\install-screen-h.ps1 -Image 'F:\RC\RonnyTest\HDLC2\BIGDISK0-K-102.IMG'
      .\install-screen-h.ps1 -WhatIf        # say what would be written, write nothing
#>

[CmdletBinding()]
param(
    # The disk image to write into. Defaults to D100's.
    [string] $Image = 'F:\RC\RonnyTest\HDLC1\BIGDISK0-K-100.IMG',

    # The SINTRAN user the files land under.
    [string] $User = 'SYSTEM',

    # ndtool, the NDFS image tool.
    [string] $NdTool = 'E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build\ndtool.exe',

    # List what would happen and write nothing.
    [switch] $WhatIf
)

$ErrorActionPreference = 'Stop'
$here = Split-Path -Parent $MyInvocation.MyCommand.Path

# ---------------------------------------------------------------------------
# 1. Refuse to touch a live image.
#
# Never kill a machine to get a write in. Report and stop - the person running
# this decides whether to close it.
# ---------------------------------------------------------------------------
# -WhatIf writes nothing, so it is safe to preview while the machines are up.
$running = @()
if (-not $WhatIf) {
    $running = @(Get-CimInstance Win32_Process -Filter "Name='RetroCore.exe'" -ErrorAction SilentlyContinue)
}
if ($running.Count -gt 0) {
    Write-Host "RetroCore is running - $($running.Count) process(es):" -ForegroundColor Yellow
    foreach ($p in $running) { Write-Host ("  pid {0}  {1}" -f $p.ProcessId, $p.CommandLine) }
    Write-Host ""
    Write-Host "An image cannot be written underneath a running machine - the machine would" -ForegroundColor Yellow
    Write-Host "write its own copy back over anything put there. Close the machine window(s)" -ForegroundColor Yellow
    Write-Host "yourself, then run this again. Nothing has been written." -ForegroundColor Yellow
    exit 1
}

if (-not (Test-Path $NdTool)) { throw "ndtool not found at $NdTool" }
if (-not (Test-Path $Image))  { throw "disk image not found at $Image" }

# ---------------------------------------------------------------------------
# 2. The files, and what each is for.
# ---------------------------------------------------------------------------
$files = @(
    @{ Local = 'SCREEN.SYMB';   Nd = 'SCREEN:SYMB';   What = 'PLANC-SCREEN-H interface ($INCLUDE screen)' }
    @{ Local = 'INTRF1B.BRF';   Nd = 'INTRF1B:BRF';   What = 'PLANC-SCREEN-H 1-bank runtime' }
    @{ Local = 'TESTUI.PLNC';   Nd = 'TESTUI:PLNC';   What = 'the demo source' }
    @{ Local = 'TESTUI.MODE';   Nd = 'TESTUI:MODE';   What = 'the build job' }
)

foreach ($f in $files) {
    $src = Join-Path $here $f.Local
    if (-not (Test-Path $src)) { throw "missing $src - the repo folder is incomplete" }
}

Write-Host "Image : $Image"
Write-Host "User  : $User"
Write-Host ""

$failed = 0
foreach ($f in $files) {
    $src = Join-Path $here $f.Local
    $dst = "$User/$($f.Nd)"
    $len = (Get-Item $src).Length

    if ($WhatIf) {
        Write-Host ("  would put {0,-16} -> {1,-22} {2} bytes   {3}" -f $f.Local, $dst, $len, $f.What)
        continue
    }

    # --overwrite IS NOT OPTIONAL. Without it ndtool prints "skipped (exists)"
    # and STILL EXITS 0, so a run that wrote nothing looks exactly like one that
    # worked.
    & $NdTool --put $src $dst --overwrite $Image | Out-Null
    if ($LASTEXITCODE -ne 0) { Write-Host "  FAILED to put $dst" -ForegroundColor Red; $failed++; continue }

    # ---------------------------------------------------------------------
    # 3. Prove it by the BYTES, not by the exit code.
    #
    # A stalled or skipped write reports success. Reading the file back and
    # comparing hashes is the only thing that actually says it is there.
    # ---------------------------------------------------------------------
    $tmp = Join-Path $env:TEMP ("screenh-verify-" + [System.Guid]::NewGuid().ToString('N'))
    New-Item -ItemType Directory -Path $tmp | Out-Null
    try {
        & $NdTool -x -F $dst -o $tmp $Image | Out-Null
        $back = Get-ChildItem -Path $tmp -File | Select-Object -First 1
        if ($null -eq $back) {
            Write-Host ("  {0,-16} -> {1,-22} WROTE BUT COULD NOT READ BACK" -f $f.Local, $dst) -ForegroundColor Red
            $failed++
        }
        elseif ((Get-FileHash $back.FullName).Hash -eq (Get-FileHash $src).Hash) {
            Write-Host ("  {0,-16} -> {1,-22} {2,7} bytes  verified   {3}" -f $f.Local, $dst, $len, $f.What) -ForegroundColor Green
        }
        else {
            Write-Host ("  {0,-16} -> {1,-22} HASH MISMATCH - on-image copy differs" -f $f.Local, $dst) -ForegroundColor Red
            $failed++
        }
    }
    finally {
        Remove-Item -Recurse -Force $tmp -ErrorAction SilentlyContinue
    }
}

if ($WhatIf) { Write-Host "`n-WhatIf: nothing was written."; exit 0 }

Write-Host ""
if ($failed -gt 0) {
    Write-Host "$failed file(s) did not verify - do NOT start the machine expecting a good build." -ForegroundColor Red
    exit 1
}

Write-Host "All files verified on the image." -ForegroundColor Green
Write-Host ""
Write-Host "Next, on the machine:"
Write-Host "    @MODE TESTUI:MODE,,"
Write-Host "    then read what LIST-ENTRIES-UNDEFINED printed - an undefined entry does"
Write-Host "    NOT fail the build, it produces a program that runs and misbehaves."
Write-Host "    Fetch TESTUI:LIST back and check it has no *** ERROR line and that it"
Write-Host "    reaches the last source line."
Write-Host "    Then:  @TESTUI"
