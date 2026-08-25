<#
.SYNOPSIS
    Compare the sources in this repository against what the machines actually hold.

.DESCRIPTION
    WHY THIS EXISTS, and it is not hypothetical.

    On 2026-08-24 the chat server would not seat anybody. `STATUS` said `SEATS 0/16` and a client's
    `/WHO` returned silence. The cause was found after most of a day: a stalled push had spliced
    TWENTY-EIGHT LINES out of the middle of CHATSV.PLNC on the machine - including
    `1 =: memberUsed(slot)`, the line that claims a seat - and left the remaining 49KB in place.

    Nothing anywhere said the file was damaged:

      - the transfer had reported success;
      - it COMPILED CLEAN, because the splice left a bare `th` behind and an undeclared name is
        legal PLANC;
      - the listing had no `*** ERROR`;
      - and D100 appeared healthy the whole time, because its RT server had been built BEFORE the
        corruption and nobody had rebuilt it since.

    The only thing that would have caught it on day one is the byte count. 230583 on the machine
    against 231840 in the repo. This script asks that question about every source, in one command.

    RUN IT AT THE START OF A SESSION, before believing anything a machine tells you.

.PARAMETER Machine
    Which machine to check: 100, 102 or 103. Omit to check every machine in the map.

.PARAMETER Detailed
    Also report files that match, not only the ones that differ.

.EXAMPLE
    .\nd-verify.ps1
    Check every source on every machine.

.EXAMPLE
    .\nd-verify.ps1 -Machine 100 -Detailed
    Check D100 and list the matches too.
#>
param(
    [int]$Machine = 0,
    [switch]$Detailed,

    # Point one machine at a different image. Used with -Machine, and it exists so this script's
    # FAILURE path can be exercised against a doctored copy - a check that has never been seen to
    # fail is not a check.
    [string]$Image = ""
)

$ErrorActionPreference = 'Stop'

# ---------------------------------------------------------------------------
# WHAT IS DEPLOYED WHERE.
#
# Left is the file in this repository, right is its name on the machine. They differ because our
# transfer has a 13-character filespec ceiling, so the machine gets short names while the repo
# keeps readable ones - CHATMON.PLNC is CHATMN:PLNC over there.
# ---------------------------------------------------------------------------
$RepoRoot = Split-Path (Split-Path $PSScriptRoot -Parent) -Parent   # ...\SINTRAN
$XmsgRoot = Split-Path $PSScriptRoot -Parent                        # ...\SINTRAN\XMSG

$Sources = @(
    @{ Repo = "$XmsgRoot\SINTRAN-CHAT\CHATSV.PLNC";  Nd = 'SYSTEM/CHATSV:PLNC';  Machines = @(100, 102) },
    @{ Repo = "$XmsgRoot\SINTRAN-CHAT\CHAT.PLNC";    Nd = 'SYSTEM/CHAT:PLNC';    Machines = @(100, 102) },
    @{ Repo = "$XmsgRoot\SINTRAN-CHAT\CHATMON.PLNC"; Nd = 'SYSTEM/CHATMN:PLNC';  Machines = @(100, 102) },
    @{ Repo = "$XmsgRoot\SINTRAN-CHAT\CHATSV.MODE";  Nd = 'SYSTEM/CHATSV:MODE';  Machines = @(100, 102) },
    @{ Repo = "$XmsgRoot\SINTRAN-CHAT\CHATCC.MODE";  Nd = 'SYSTEM/CHATCC:MODE';  Machines = @(100, 102) },
    @{ Repo = "$XmsgRoot\SINTRAN-CHAT\CHATMON.MODE"; Nd = 'SYSTEM/CHATMN:MODE';  Machines = @(100, 102) }
)

$Images = @{
    100 = 'F:\RC\RonnyTest\HDLC1\BIGDISK0-K-100.IMG'
    102 = 'F:\RC\RonnyTest\HDLC2\BIGDISK0-K-102.IMG'
    103 = 'F:\RC\RonnyTest\HDLC3\BIGDISK0-K-103.IMG'
}

$NdTool = 'E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build\ndtool.exe'

if (-not (Test-Path $NdTool)) {
    Write-Error "ndtool not found at $NdTool - cannot read the machine images."
    exit 2
}

# ---------------------------------------------------------------------------
# READ FROM A COPY, NEVER THE LIVE IMAGE.
#
# The emulators may be running. A read straight off a file another process is writing can tear and
# report a difference that is not there - which would be exactly the kind of false alarm this
# script exists to avoid producing.
# ---------------------------------------------------------------------------
$Work = Join-Path $env:TEMP ("nd-verify-" + [System.Guid]::NewGuid().ToString('N').Substring(0, 8))
New-Item -ItemType Directory -Force $Work | Out-Null

function Get-MachineFile {
    param([int]$Mach, [string]$NdPath, [string]$ImageCopy)

    $out = Join-Path $Work ("m$Mach")
    New-Item -ItemType Directory -Force $out | Out-Null
    & $NdTool -x -F $NdPath -o $out $ImageCopy 2>&1 | Out-Null

    # ndtool names the extracted file after the ND name with the colon turned into a dot.
    $leaf = ($NdPath -split '/')[-1] -replace ':', '.'
    $path = Join-Path $out $leaf
    if (Test-Path $path) { return $path }
    return $null
}

$targets = if ($Machine -ne 0) { @($Machine) } else { $Images.Keys | Sort-Object }
$problems = 0
$checked = 0

try {
    foreach ($mach in $targets) {
        $image = if ($Image -ne "" -and $Machine -ne 0) { $Image } else { $Images[$mach] }
        if (-not $image -or -not (Test-Path $image)) {
            Write-Host "D$mach : NO IMAGE at $image - skipped" -ForegroundColor Yellow
            continue
        }

        $copy = Join-Path $Work "d$mach.img"
        Copy-Item $image $copy -Force

        Write-Host ""
        Write-Host "=== D$mach ===" -ForegroundColor Cyan

        foreach ($s in $Sources) {
            if ($s.Machines -notcontains $mach) { continue }
            if (-not (Test-Path $s.Repo)) {
                Write-Host ("  {0,-22} REPO FILE MISSING: {1}" -f (Split-Path $s.Repo -Leaf), $s.Repo) -ForegroundColor Yellow
                continue
            }

            $checked++
            $repoLen = (Get-Item $s.Repo).Length
            $ndFile = Get-MachineFile -Mach $mach -NdPath $s.Nd -ImageCopy $copy

            if (-not $ndFile) {
                Write-Host ("  {0,-22} NOT ON THE MACHINE          repo {1} bytes" -f (Split-Path $s.Repo -Leaf), $repoLen) -ForegroundColor Red
                $problems++
                continue
            }

            $ndLen = (Get-Item $ndFile).Length
            $same = ((Get-FileHash $s.Repo).Hash -eq (Get-FileHash $ndFile).Hash)

            if ($same) {
                if ($Detailed) {
                    Write-Host ("  {0,-22} ok                          {1} bytes" -f (Split-Path $s.Repo -Leaf), $repoLen) -ForegroundColor DarkGray
                }
            }
            else {
                $problems++
                $delta = $ndLen - $repoLen
                Write-Host ("  {0,-22} DIFFERS   repo {1}  machine {2}  ({3:+#;-#;0} bytes)" -f `
                        (Split-Path $s.Repo -Leaf), $repoLen, $ndLen, $delta) -ForegroundColor Red

                # A SHORTER file on the machine is the truncation signature. Say so, because that is
                # the case that compiles clean and wastes days.
                if ($ndLen -lt $repoLen) {
                    Write-Host ("      the machine's copy is SHORTER. A stalled transfer leaves a half-written file " +
                        "that still COMPILES - an undeclared name is legal PLANC. Redeploy before trusting any build.") -ForegroundColor Red
                }
            }
        }
    }
}
finally {
    Remove-Item $Work -Recurse -Force -ErrorAction SilentlyContinue
}

Write-Host ""
if ($problems -eq 0) {
    Write-Host "$checked files checked, all match the repository." -ForegroundColor Green
    exit 0
}

Write-Host "$checked files checked, $problems DIFFER OR ARE MISSING." -ForegroundColor Red
Write-Host "Deploy again with nd-deploy.ps1 before believing anything these machines do." -ForegroundColor Red
exit 1
