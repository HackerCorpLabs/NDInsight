<#
.SYNOPSIS
    Deploy a PLANC source to a machine, build it, check the build, load it and prove it runs.

.DESCRIPTION
    ONE COMMAND FOR THE WHOLE LOOP. It exists because the loop is ten steps, every step has a way
    of failing that LOOKS LIKE SUCCESS, and doing it by hand means remembering all of them:

      - a transfer reports success and leaves a half-written file;
      - `ndtool --put` without --overwrite prints "skipped (exists)" and exits 0;
      - PLANC compiles an undeclared name CLEAN, so a spliced source builds green;
      - a listing that stops two thirds of the way through reports no error for the third it
        never read;
      - `@ABORT` then `@RT` does not pick up a new build;
      - and a machine that "works" may be running a binary older than its source.

    Every one of those has cost a day on this project. This script fails loudly on all of them.

    THE STEPS

      1. LINT     planc-lint.py                                        (instant, catches most)
      2. DEPLOY   the SYNC DAEMON if one is running, else stop/ndtool/start
      3. VERIFY   the bytes on the machine match the repo               (nd-verify.ps1)
      4. BUILD    @MODE <name>:MODE,,
      5. CHECK    no *** in the listing AND it reaches the last source line
      6. LOAD     rt-load.ps1 onto a FRESH segment
      7. PROVE    LI-RT-DES shows it in the time queue

.PARAMETER Machine
    100 or 102.

.PARAMETER Source
    Repo source file. Defaults to the chat server.

.PARAMETER Segment
    A FREE segment to load onto. Never reuse one - see rt-load.ps1 for why.

.PARAMETER SkipLoad
    Deploy, build and check, but do not RT-load. For a program that is not an RT server.

.EXAMPLE
    .\nd-deploy.ps1 -Machine 100 -Segment 2603
#>
param(
    [Parameter(Mandatory = $true)][int]$Machine,
    [string]$Source = "",
    [int]$Segment = 0,
    [string]$NdName = "",
    [string]$ModeFile = "",
    [string]$RtName = "CHATSER",
    [switch]$SkipLoad
)

$ErrorActionPreference = 'Stop'
$XmsgRoot = Split-Path $PSScriptRoot -Parent
$NdTool = 'E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build\ndtool.exe'

$Ports = @{ 100 = 9010; 102 = 9102; 103 = 9003 }
$Images = @{
    100 = 'F:\RC\RonnyTest\HDLC1\BIGDISK0-K-100.IMG'
    102 = 'F:\RC\RonnyTest\HDLC2\BIGDISK0-K-102.IMG'
    103 = 'F:\RC\RonnyTest\HDLC3\BIGDISK0-K-103.IMG'
}

if ($Source -eq "") { $Source = "$XmsgRoot\SINTRAN-CHAT\CHATSV.PLNC" }
if ($NdName -eq "") { $NdName = 'SYSTEM/CHATSV:PLNC' }
if ($ModeFile -eq "") { $ModeFile = 'CHATSV:MODE' }

$port = $Ports[$Machine]
$image = $Images[$Machine]
if (-not $port) { Write-Error "unknown machine D$Machine"; exit 2 }
if (-not (Test-Path $Source)) { Write-Error "no such source: $Source"; exit 2 }

$leaf = Split-Path $Source -Leaf
$listName = ($NdName -split '/')[-1] -replace ':PLNC$', ':LIST'
$sourceLines = (Get-Content $Source).Count

function Step($n, $text) { Write-Host ""; Write-Host "== $n. $text" -ForegroundColor Cyan }
function Fail($text) { Write-Host ""; Write-Host "FAILED: $text" -ForegroundColor Red; exit 1 }

Write-Host "nd-deploy: $leaf -> D$Machine  ($sourceLines source lines)" -ForegroundColor White

# ---------------------------------------------------------------------------
Step 1 "LINT"
# ---------------------------------------------------------------------------
$lint = & python "$XmsgRoot\tools\planc-lint.py" $Source 2>&1
$lint | ForEach-Object { Write-Host "   $_" }
if ($LASTEXITCODE -ne 0) { Fail "the linter refused it. Fix that before spending minutes on the machine." }

# ---------------------------------------------------------------------------
Step 2 "DEPLOY"
#
# THE SYNC DAEMON IS THE WAY. It holds ONE link open and carries files as they appear. A one-shot
# push ends by sending DISC, and that teardown killed XMSG fourteen times out of fourteen.
# ---------------------------------------------------------------------------
$daemon = Get-CimInstance Win32_Process -Filter "Name='dotnet.exe' OR Name='Xmsg.Live.Runner.exe'" -ErrorAction SilentlyContinue |
    Where-Object { $_.CommandLine -like '*--sync*' }

if ($daemon) {
    $syncDir = "$XmsgRoot\SRC\Xmsg.Live.Runner\sync-out"
    New-Item -ItemType Directory -Force $syncDir | Out-Null
    Copy-Item $Source (Join-Path $syncDir $leaf) -Force
    Write-Host "   dropped in sync-out, daemon is running - waiting for it to land" -ForegroundColor DarkGray

    $want = (Get-FileHash $Source).Hash
    $landed = $false
    for ($i = 0; $i -lt 60; $i++) {
        Start-Sleep -Seconds 2
        $v = & "$PSScriptRoot\nd-verify.ps1" -Machine $Machine 2>&1
        if ($LASTEXITCODE -eq 0) { $landed = $true; break }
    }
    if (-not $landed) { Fail "the daemon did not land the file within two minutes. Check its window." }
    Write-Host "   landed" -ForegroundColor Green
}
else {
    Write-Host "   NO SYNC DAEMON RUNNING - falling back to writing the disk image." -ForegroundColor Yellow
    Write-Host "   That costs a stop and a boot of every machine. Start the daemon instead when you can:" -ForegroundColor Yellow
    Write-Host "     Xmsg.Live.Runner --config topology-d19999-hdlc-server.json --originate-from-seed \" -ForegroundColor DarkGray
    Write-Host "                      --sync sync-out --sync-user SYSTEM --sync-to $Machine 127.0.0.1 10362 19999 3600" -ForegroundColor DarkGray

    $running = @(Get-CimInstance Win32_Process -Filter "Name='RetroCore.exe'" -ErrorAction SilentlyContinue)
    $dirs = @()
    foreach ($p in $running) {
        if ($p.CommandLine -match '(F:\\RC\\RonnyTest\\[^\\]+)\\RetroCore\.exe') { $dirs += $Matches[1] }
        Stop-Process -Id $p.ProcessId -Force
    }
    if ($running.Count -gt 0) { Start-Sleep -Seconds 5 }

    # --overwrite IS NOT OPTIONAL: without it ndtool prints "skipped (exists)" and exits 0.
    $out = & $NdTool --put $Source $NdName --overwrite $image 2>&1
    $out | ForEach-Object { Write-Host "   $_" }
    if ($out -match 'skipped') { Fail "ndtool SKIPPED the write - nothing was deployed." }

    foreach ($d in $dirs) { Start-Process -FilePath "$d\RetroCore.exe" -WorkingDirectory $d -WindowStyle Normal }
    if ($dirs.Count -gt 0) {
        Write-Host "   machines restarted - waiting for the boot" -ForegroundColor DarkGray
        Start-Sleep -Seconds 100
    }
}

# ---------------------------------------------------------------------------
Step 3 "VERIFY THE BYTES ON THE MACHINE"
# ---------------------------------------------------------------------------
& "$PSScriptRoot\nd-verify.ps1" -Machine $Machine
if ($LASTEXITCODE -ne 0) { Fail "what is on the machine is not what is in the repo. Do not build this." }

# ---------------------------------------------------------------------------
Step 4 "BUILD"
# ---------------------------------------------------------------------------
Write-Host "   @MODE $ModeFile,,   (a few minutes)" -ForegroundColor DarkGray
& "$PSScriptRoot\ndterm.ps1" -Port $port -Steps @("MODE $ModeFile,,") -WaitFor '@' -WaitForTimeoutMs 600000 | Out-Null

# ---------------------------------------------------------------------------
Step 5 "CHECK THE LISTING - both halves"
# ---------------------------------------------------------------------------
$work = Join-Path $env:TEMP ("nd-deploy-" + [System.Guid]::NewGuid().ToString('N').Substring(0, 8))
New-Item -ItemType Directory -Force $work | Out-Null
try {
    Copy-Item $image "$work\img" -Force
    & $NdTool -x -F "SYSTEM/$listName" -o $work "$work\img" 2>&1 | Out-Null
    $lst = Join-Path $work (($listName -split '/')[-1] -replace ':', '.')
    if (-not (Test-Path $lst)) { Fail "no listing was produced - the compile did not run." }

    $report = & python "$PSScriptRoot\nd-listing-check.py" $lst $sourceLines 2>&1
    $report | ForEach-Object { Write-Host "   $_" }
    if ($LASTEXITCODE -ne 0) { Fail "the listing says the build is not good." }
}
finally { Remove-Item $work -Recurse -Force -ErrorAction SilentlyContinue }

# ---------------------------------------------------------------------------
Step 6 "LOAD"
# ---------------------------------------------------------------------------
if ($SkipLoad) {
    Write-Host "   -SkipLoad given, stopping here." -ForegroundColor DarkGray
    Write-Host ""
    Write-Host "BUILT AND VERIFIED on D$Machine." -ForegroundColor Green
    exit 0
}
if ($Segment -eq 0) { Fail "give -Segment <FREE segment>. Never reuse one: a rebuild does not reach a loaded segment." }

& "$PSScriptRoot\rt-load.ps1" -Port $port -Segment $Segment -RtName $RtName -AndStart

# ---------------------------------------------------------------------------
Step 7 "PROVE IT IS RUNNING"
# ---------------------------------------------------------------------------
$desc = & "$PSScriptRoot\ndterm.ps1" -Port $port -Steps @("LI-RT-DES,$RtName") -WaitFor '@' -WaitForTimeoutMs 60000
$descText = ($desc | Out-String)
if ($descText -match 'IN TIME QUEUE') {
    Write-Host "   $RtName is IN TIME QUEUE - running." -ForegroundColor Green
}
else {
    Write-Host $descText
    Fail "$RtName is not in the time queue. It loaded but it is not running."
}

Write-Host ""
Write-Host "DONE. $leaf is deployed, built, loaded on segment $Segment and running on D$Machine." -ForegroundColor Green
Write-Host "Now TEST it - one command at a time, and ask the SERVER, not the screen." -ForegroundColor White
exit 0
