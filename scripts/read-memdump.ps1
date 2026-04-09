param(
    [string]$DumpFile = "C:\Users\ronny\Downloads\nd100_physmem_256k.bin"
)

$data = [System.IO.File]::ReadAllBytes($DumpFile)
Write-Output "Loaded $($data.Length) bytes ($($data.Length / 2) words)"

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

function Hex {
    param([int]$v)
    return '0x' + $v.ToString('X4')
}

function DumpRange {
    param([int]$startWord, [int]$count, [string]$label)
    Write-Output ""
    Write-Output "=== $label ($(Oct $startWord) - $(Oct ($startWord + $count - 1))) ==="
    for ($i = 0; $i -lt $count; $i++) {
        $addr = $startWord + $i
        $w = ReadWord $addr
        $octAddr = Oct $addr
        $octVal = Oct $w
        $hexVal = Hex $w
        Write-Output ("  {0}  {1}  {2}  ({3})" -f $octAddr, $octVal, $hexVal, $w)
    }
}

Write-Output "=== SYSEVAL TABLE ==="
$base = 0x829  # 004051 octal
$labels = @('SYSNO   ','HWINFO0 ','HWINFO1 ','HWINFO2 ','SINVER0 ','SINVER1 ','REVLEV  ','GENDAT0 ','GENDAT1 ','GENDAT2 ','GENDAT3 ','GENDAT4 ')
for ($i = 0; $i -lt 12; $i++) {
    $addr = $base + $i
    $w = ReadWord $addr
    Write-Output ("  {0} {1} @ {2} = {3}  {4}  ({5})" -f $labels[$i], ("disp " + $i), (Oct $addr), (Oct $w), (Hex $w), $w)
}

Write-Output ""
Write-Output "=== UNAFLAG ==="
$w = ReadWord 0x847
Write-Output ("  UNAFLAG  @ {0} = {1}  {2}" -f (Oct 0x847), (Oct $w), (Hex $w))

Write-Output ""
Write-Output "=== QUEUE HEADS & KEY GLOBALS ==="
$globals = @(
    @(0x807, 'RTREF '),
    @(0x808, 'CURPR '),
    @(0x809, 'MQUEU '),
    @(0x80A, 'BTIMQ '),
    @(0x80B, 'BEXQU '),
    @(0x810, 'RTSTA '),
    @(0x8D1, 'SEGST '),
    @(0x8D3, 'RTEND ')
)
foreach ($g in $globals) {
    $w = ReadWord $g[0]
    Write-Output ("  {0} @ {1} = {2}  {3}" -f $g[1], (Oct $g[0]), (Oct $w), (Hex $w))
}

# Dump RT table area
$rtsta = ReadWord 0x810
$rtend = ReadWord 0x8D3
Write-Output ""
Write-Output "=== RT TABLE (RTSTA=$(Oct $rtsta) to RTEND=$(Oct $rtend)) ==="
Write-Output "  RT-Description size = 26 octal (22 decimal) words"

if ($rtsta -gt 0 -and $rtend -gt $rtsta -and $rtend -lt 0x10000) {
    $rtSize = 0x16  # 26 octal = 22 decimal
    $numRt = [math]::Floor(($rtend - $rtsta) / $rtSize)
    Write-Output "  Number of RT entries: $numRt"
    Write-Output ""

    # Show first 3 RT entries
    $showCount = [math]::Min($numRt, 5)
    for ($rt = 0; $rt -lt $showCount; $rt++) {
        $entryBase = $rtsta + ($rt * $rtSize)
        Write-Output "  --- RT Entry $rt (base=$(Oct $entryBase)) ---"
        $fieldNames = @('TLINK ','STATU ','off02 ','TYPRI ','DTIM1 ','DTIM2 ','off06 ','off07 ',
                        'STADR ','SEGM1 ','SEGM2 ','WLINK ','ACT1S ','ACT2S ','off16 ','ACTPR ',
                        'BRESL ','RSEGM ','BUFWI ','off23 ','N5WIN ','RTDLG ')
        for ($f = 0; $f -lt $rtSize; $f++) {
            $faddr = $entryBase + $f
            $w = ReadWord $faddr
            $fname = if ($f -lt $fieldNames.Count) { $fieldNames[$f] } else { "off$f  " }
            Write-Output ("    +{0} {1} @ {2} = {3}  {4}" -f ([Convert]::ToString($f, 8).PadLeft(2,'0')), $fname, (Oct $faddr), (Oct $w), (Hex $w))
        }
        Write-Output ""
    }
}

# Dump segment table
$segst = ReadWord 0x8D1
if ($segst -gt 0 -and $segst -lt 0x10000) {
    Write-Output "=== SEGMENT TABLE (SEGST=$(Oct $segst)) ==="
    Write-Output "  Segment entry size = 10 octal (8 decimal) words"
    $segSize = 8
    for ($s = 0; $s -lt 3; $s++) {
        $segBase = $segst + ($s * $segSize)
        Write-Output "  --- Segment $s (base=$(Oct $segBase)) ---"
        $segFields = @('SEGLI ','PRESE ','LOGAD ','SEGLE ','off04 ','off05 ','SGSTA ','BPAGL ')
        for ($f = 0; $f -lt $segSize; $f++) {
            $faddr = $segBase + $f
            $w = ReadWord $faddr
            Write-Output ("    +{0} {1} @ {2} = {3}  {4}" -f ([Convert]::ToString($f, 8).PadLeft(2,'0')), $segFields[$f], (Oct $faddr), (Oct $w), (Hex $w))
        }
        Write-Output ""
    }
}

# Also dump GENDAT as raw bytes for the date investigation
Write-Output "=== GENDAT RAW BYTES ==="
for ($i = 0; $i -lt 5; $i++) {
    $addr = 0x830 + $i
    $byteOff = $addr * 2
    $hiByte = $data[$byteOff]
    $loByte = $data[$byteOff + 1]
    $word = ($hiByte -shl 8) -bor $loByte
    Write-Output ("  GENDAT($i) @ $(Oct $addr): hi=0x{0:X2} lo=0x{1:X2}  word={2}  {3}" -f $hiByte, $loByte, (Oct $word), (Hex $word))
}

# Try packed ND date on GENDAT word pairs
Write-Output ""
Write-Output "=== GENDAT AS PACKED ND DATE (parseNdTime format) ==="
for ($pair = 0; $pair -lt 4; $pair++) {
    $w0 = ReadWord (0x830 + $pair)
    $w1 = ReadWord (0x831 + $pair)
    $ndtime = (([long]$w0 -shl 16) -bor [long]$w1) -band 0xFFFFFFFFL
    $year = (($ndtime -shr 26) -band 0x3F) + 1950
    $month = ($ndtime -shr 22) -band 0x0F
    $day = ($ndtime -shr 17) -band 0x1F
    $hour = ($ndtime -shr 12) -band 0x1F
    $minute = ($ndtime -shr 6) -band 0x3F
    $second = $ndtime -band 0x3F
    Write-Output ("  GENDAT($pair):GENDAT($($pair+1)) = 0x{0:X8} -> {1}-{2:D2}-{3:D2} {4:D2}:{5:D2}:{6:D2}" -f $ndtime, $year, $month, $day, $hour, $minute, $second)
}
