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

# PIT RAM base: 174000 octal = 63488 decimal
$pitBase = 63488  # 174000 octal

# Check if PIT RAM range is in the dump
$pitEnd = $pitBase + (16 * 128) - 1  # 16 PITs * 128 words each = 2048 words
Write-Output "PIT RAM range: words $pitBase ($(Oct $pitBase)) to $pitEnd ($(Oct $pitEnd))"
Write-Output ""

# Summary: count non-zero entries per PIT
Write-Output "=== PIT SUMMARY (non-zero entries per PIT) ==="
$pitNames = @{
    0  = "PIT#0  (identity)"
    3  = "FUPIT#3"
    4  = "FPIT#4"
    5  = "5PIT#5"
    6  = "XPIT#6"
    7  = "DPIT#7"
    8  = "RPIT#10"
    9  = "SPIT#11"
    10 = "MPIT#12"
    13 = "IPIT#15"
    15 = "PIT#17 (alt)"
}

for ($pit = 0; $pit -lt 16; $pit++) {
    $pitStart = $pitBase + ($pit * 128)  # 128 words per PIT (64 entries * 2 words)
    $nonZero = 0
    for ($pg = 0; $pg -lt 64; $pg++) {
        $addr = $pitStart + ($pg * 2)
        $w0 = ReadWord $addr
        $w1 = ReadWord ($addr + 1)
        if ($w0 -ne 0 -or $w1 -ne 0) { $nonZero++ }
    }
    $pitOct = [Convert]::ToString($pit, 8)
    $name = if ($pitNames.ContainsKey($pit)) { $pitNames[$pit] } else { "PIT#$pitOct" }
    $marker = if ($nonZero -gt 0) { " <-- HAS DATA" } else { "" }
    Write-Output ("  {0,-15} : {1,2}/64 non-zero entries{2}" -f $name, $nonZero, $marker)
}

# Dump MPIT (#12 octal = 10 decimal) - the main kernel page table
$mpitNum = 10  # decimal, = 12 octal
$mpitStart = $pitBase + ($mpitNum * 128)
Write-Output ""
Write-Output "=== MPIT (#12 octal = #10 decimal) - FULL DUMP ==="
Write-Output "Base address: $(Oct $mpitStart)"
for ($pg = 0; $pg -lt 64; $pg++) {
    $addr = $mpitStart + ($pg * 2)
    $w0 = ReadWord $addr  # protection/status word
    $w1 = ReadWord ($addr + 1)  # physical page number
    if ($w0 -ne 0 -or $w1 -ne 0) {
        $pgOct = [Convert]::ToString($pg, 8).PadLeft(2, '0')
        Write-Output "  LogPage $pgOct -> PhysPage $(Oct $w1) PROTE=$(Oct $w0)"
    }
}

# Also dump PIT#0 for comparison (should be identity-mapped)
$pit0Start = $pitBase
Write-Output ""
Write-Output "=== PIT #0 (should be identity-mapped) - first 20 entries ==="
for ($pg = 0; $pg -lt 20; $pg++) {
    $addr = $pit0Start + ($pg * 2)
    $w0 = ReadWord $addr
    $w1 = ReadWord ($addr + 1)
    $pgOct = [Convert]::ToString($pg, 8).PadLeft(2, '0')
    $marker = ""
    if ($w1 -eq $pg -and $w0 -ne 0) { $marker = " (identity OK)" }
    elseif ($w0 -eq 0 -and $w1 -eq 0) { $marker = " (empty)" }
    else { $marker = " (NOT identity!)" }
    Write-Output "  LogPage $pgOct -> PhysPage $(Oct $w1) PROTE=$(Oct $w0)$marker"
}

# Dump all PITs with data
Write-Output ""
Write-Output "=== ALL NON-EMPTY PITs ==="
for ($pit = 0; $pit -lt 16; $pit++) {
    $pitStart2 = $pitBase + ($pit * 128)
    $hasData = $false
    for ($pg = 0; $pg -lt 64; $pg++) {
        $addr = $pitStart2 + ($pg * 2)
        $w0 = ReadWord $addr
        $w1 = ReadWord ($addr + 1)
        if ($w0 -ne 0 -or $w1 -ne 0) { $hasData = $true; break }
    }
    if (-not $hasData) { continue }

    $pitOct = [Convert]::ToString($pit, 8)
    $name = if ($pitNames.ContainsKey($pit)) { $pitNames[$pit] } else { "PIT#$pitOct" }
    Write-Output ""
    Write-Output "--- $name ---"
    for ($pg = 0; $pg -lt 64; $pg++) {
        $addr = $pitStart2 + ($pg * 2)
        $w0 = ReadWord $addr
        $w1 = ReadWord ($addr + 1)
        if ($w0 -ne 0 -or $w1 -ne 0) {
            $pgOct = [Convert]::ToString($pg, 8).PadLeft(2, '0')
            Write-Output "  LogPage $pgOct -> PhysPage $(Oct $w1) PROTE=$(Oct $w0)"
        }
    }
}
