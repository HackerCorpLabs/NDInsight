param(
    [string]$DumpFile = "C:\Users\ronny\Downloads\nd100_physmem_256k.bin"
)

$data = [System.IO.File]::ReadAllBytes($DumpFile)
Write-Output "File size: $($data.Length) bytes = $($data.Length / 2) words"

function ReadWord {
    param([int]$wordAddr)
    $byteOff = $wordAddr * 2
    if ($byteOff + 1 -ge $data.Length) { return -1 }
    return ($data[$byteOff] -shl 8) -bor $data[$byteOff + 1]
}

function Oct {
    param([int]$v)
    return [Convert]::ToString($v, 8).PadLeft(6, '0')
}

# Read the key global pointers (all on page 2, identity-mapped)
Write-Output ""
Write-Output "=== SEGMENT TABLE AND CORE MAP ROOT POINTERS ==="
# 004320 octal = 2256 decimal
$segtb = ReadWord 2256
Write-Output "SEGTB  (004320) = $(Oct $segtb)  (bank $segtb)"
# 004321 octal = 2257 decimal
$segst = ReadWord 2257
Write-Output "SEGST  (004321) = $(Oct $segst)  (segment table base offset)"
# 004322 octal = 2258 decimal
$cormb = ReadWord 2258
Write-Output "CORMB  (004322) = $(Oct $cormb)  (bank $cormb)"
# 004021 octal = 2065 decimal
$corms = ReadWord 2065
Write-Output "CORMS  (004021) = $(Oct $corms)"

Write-Output ""
Write-Output "=== PHYSICAL ADDRESS CALCULATION ==="
$segPhysBase = ($segtb -shl 16) + $segst
Write-Output "Segment table physical base = (SEGTB << 16) + SEGST = ($segtb << 16) + $segst = $segPhysBase words"
Write-Output "  In octal: $(Oct $segPhysBase)"
$maxWord = $data.Length / 2
Write-Output "Dump covers words 0 to $($maxWord - 1) ($maxWord words)"

$cormPhysBase = $cormb -shl 16
Write-Output "Core map physical base = (CORMB << 16) = $cormPhysBase words"

if ($segPhysBase -lt $maxWord -and $segPhysBase -ge 0) {
    Write-Output ""
    Write-Output "=== SEGMENT TABLE IS IN THE DUMP! ==="
    Write-Output "Reading first 10 segment entries (8 words each):"
    for ($seg = 0; $seg -lt 10; $seg++) {
        $base = $segPhysBase + ($seg * 8)
        if ($base + 7 -ge $maxWord) { break }
        $segli = ReadWord $base
        $prese = ReadWord ($base + 1)
        $logad = ReadWord ($base + 2)
        $segle = ReadWord ($base + 3)
        $madr  = ReadWord ($base + 4)
        $flag  = ReadWord ($base + 5)
        $sgsta = ReadWord ($base + 6)
        $bpagl = ReadWord ($base + 7)
        Write-Output "  Seg $(Oct $seg) @ $(Oct $base): SEGLI=$(Oct $segli) LOGAD=$(Oct $logad) SEGLE=$(Oct $segle) SGSTA=$(Oct $sgsta) BPAGL=$(Oct $bpagl)"
    }

    # Also read specific kernel segments
    Write-Output ""
    Write-Output "=== KERNEL SEGMENT ENTRIES ==="
    $kernelSegs = @(
        @(19, "5DPIT"),
        @(29, "5MPIT"),
        @(39, "5RPIT"),
        @(41, "55PIT"),
        @(52, "5ECOM"),
        @(55, "5IPIT")
    )
    foreach ($ks in $kernelSegs) {
        $segNum = $ks[0]
        $segName = $ks[1]
        $base = $segPhysBase + ($segNum * 8)
        if ($base + 7 -ge $maxWord) {
            Write-Output "  $segName (seg $(Oct $segNum)): OUTSIDE DUMP"
            continue
        }
        $segli = ReadWord $base
        $logad = ReadWord ($base + 2)
        $segle = ReadWord ($base + 3)
        $sgsta = ReadWord ($base + 6)
        $bpagl = ReadWord ($base + 7)
        Write-Output "  $segName (seg $(Oct $segNum)) @ $(Oct $base): LOGAD=$(Oct $logad) SEGLE=$(Oct $segle) SGSTA=$(Oct $sgsta) BPAGL=$(Oct $bpagl)"
    }
} else {
    Write-Output ""
    Write-Output "Segment table at physical word $segPhysBase is OUTSIDE the dump ($maxWord words)"
}

# Now try to read core map entries if in dump
if ($cormPhysBase -lt $maxWord -and $cormPhysBase -ge 0) {
    Write-Output ""
    Write-Output "=== CORE MAP IS IN THE DUMP! ==="
    Write-Output "Reading first 20 core map entries (physical pages 0-19, 4 words each):"
    for ($pg = 0; $pg -lt 20; $pg++) {
        $base = $cormPhysBase + ($pg * 4)
        if ($base + 3 -ge $maxWord) { break }
        $pagli = ReadWord $base
        $unk   = ReadWord ($base + 1)
        $prote = ReadWord ($base + 2)
        $logpa = ReadWord ($base + 3)
        Write-Output "  PhysPage $(Oct $pg) @ $(Oct $base): PAGLI=$(Oct $pagli) w1=$(Oct $unk) PROTE=$(Oct $prote) LOGPA=$(Oct $logpa)"
    }

    # Try to follow BPAGL chain for 5MPIT segment if we found it
    if ($segPhysBase -lt $maxWord) {
        $mpitSegBase = $segPhysBase + (29 * 8)  # 5MPIT = segment 35 octal = 29 decimal
        if ($mpitSegBase + 7 -lt $maxWord) {
            $bpagl = ReadWord ($mpitSegBase + 7)
            Write-Output ""
            Write-Output "=== FOLLOWING 5MPIT SEGMENT PAGE CHAIN (BPAGL=$(Oct $bpagl)) ==="
            $x = $bpagl
            $count = 0
            while ($x -ne 0 -and $count -lt 30) {
                $physBase = $cormPhysBase + $x  # x is offset within core map bank
                if ($physBase + 3 -ge $maxWord) {
                    Write-Output "  Core map entry at offset $(Oct $x) is outside dump"
                    break
                }
                $physPage = [math]::Floor($x / 4)  # physical page = address >> 2
                $pagli = ReadWord $physBase
                $unk   = ReadWord ($physBase + 1)
                $prote = ReadWord ($physBase + 2)
                $logpa = ReadWord ($physBase + 3)
                Write-Output "  PhysPage $(Oct $physPage) -> LogPage $(Oct $logpa) PROTE=$(Oct $prote) PAGLI=$(Oct $pagli)"
                $x = $pagli
                $count++
            }
            if ($x -eq 0) { Write-Output "  (end of chain)" }
        }
    }
} else {
    Write-Output ""
    Write-Output "Core map at physical word $cormPhysBase is OUTSIDE the dump ($maxWord words)"
}
