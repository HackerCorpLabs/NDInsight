# ACCP (ND-324716 / PCB 5616) — 68000 EPROM dump

Two AM27C512 (64 Ki x 8) EPROM dumps from the ACCP "baby card" — the Samson
ACCess Processor, the ND-5000's access processor and octobus controller — merged
into a single 16-bit-wide ROM image.

See `../README.md` for what the card is and where the analysis lives. This file
covers only the mechanical merge and its validation.

## Files

| File | Size | MD5 | Role |
|---|---|---|---|
| `51200J.bin` | 65,536 | `62516100001DAB82117795034E9B25CE` | **Even** bytes — D8..D15 (high byte) |
| `51201J.bin` | 65,536 | `0BDD9FB0FD7982D960AE64DF6E56EE22` | **Odd** bytes — D0..D7 (low byte) |
| `octo.bin` | 131,072 | `39A21C86A74BFEDFC0B996DE7F5ADB63` | Merged 16-bit image |

## Interleave

The 68000 has a 16-bit data bus, so a pair of byte-wide EPROMs is split
high/low. Byte `n` of `51200J.bin` lands at even offset `2n`, byte `n` of
`51201J.bin` at odd offset `2n+1`:

```
octo.bin[2n]     = 51200J.bin[n]   ; D8..D15, even address
octo.bin[2n + 1] = 51201J.bin[n]   ; D0..D7,  odd address
```

Reproduce with PowerShell:

```powershell
$a = [IO.File]::ReadAllBytes('51200J.bin')
$b = [IO.File]::ReadAllBytes('51201J.bin')
$o = New-Object byte[] 131072
for ($i = 0; $i -lt 65536; $i++) { $o[2*$i] = $a[$i]; $o[2*$i+1] = $b[$i] }
[IO.File]::WriteAllBytes('octo.bin', $o)
```

## Why this order and not the other

Swapping the two ROMs yields reset SSP `0x1100FC3F` and reset PC `0x00D6000B`.
An odd PC is illegal on a 68000 (it would take an address-error exception
before executing a single instruction), so that ordering is ruled out.

## Validation

### Reset vector

| Offset | Value | Meaning |
|---|---|---|
| `0x000000` | `0x00113FFC` | Initial supervisor stack pointer (SSP) |
| `0x000004` | `0x00000BD6` | Initial program counter (PC) |

Both are even and plausible. The SSP sits just below `0x114000`.

### Exception vector table

Vectors 2..11 are all distinct, even, ascending, and packed into a small
handler region at `0x400..0x490`. Vectors 12..15 (all reserved/unassigned on
the plain 68000) share one common handler — the classic "catch-all" pattern.

| Vec | Exception | Handler |
|---|---|---|
| 2 | Bus error | `0x000400` |
| 3 | Address error | `0x00040C` |
| 4 | Illegal instruction | `0x000418` |
| 5 | Divide by zero | `0x000436` |
| 6 | CHK | `0x000442` |
| 7 | TRAPV | `0x00044E` |
| 8 | Privilege violation | `0x00045A` |
| 9 | Trace | `0x000466` |
| 10 | Line 1010 emulator (A-line) | `0x000472` |
| 11 | Line 1111 emulator (F-line) | `0x000490` |
| 12..15 | Reserved | `0x0008B8` |

### Entry point disassembly

The code at the reset PC decodes cleanly as valid 68000 and is recognisably a
RAM sizing loop — note that it probes `0x00110000`, which is exactly where the
reset SSP (`0x00113FFC`) points. The two independent facts agree, which is
strong evidence the merge is correct.

```
0BD6  203C 0011 0000   MOVE.L  #$00110000,D0   ; base of RAM under test
0BDC  7400             MOVEQ   #0,D2           ; clear counters
0BDE  7600             MOVEQ   #0,D3
0BE0  7800             MOVEQ   #0,D4
0BE2  7A00             MOVEQ   #0,D5
0BE4  2040             MOVEA.L D0,A0
0BE6  2080             MOVE.L  D0,(A0)         ; write address-as-pattern
0BE8  2210             MOVE.L  (A0),D1         ; read it back
0BEA  B280             CMP.L   D0,D1           ; does it stick?
0BEC  6706             BEQ.S   $0BF4
0BEE  0682 0000 0001   ADDI.L  #1,D2           ; count a failure
```

## Resolved since this file was written (2026-07-27)

Full analysis:
`../../../../SINTRAN/ND5000/ACCP-324716-FIRMWARE-RE-2026-07-27.md`

- **What the firmware is — ANSWERED.** It is the ACCP's own operating software.
  The text is not in the low region, which is why the first pass missed it; it
  lives from `0x011500` upward, and it is unambiguous:
  `****** S A M S O N   A C C E S S   P R O C E S S O R ******` (`0x011729`),
  `ACCP local ram test OK`, `Communication ACCP-ND100 started. Version:`,
  `Only 32-bit Word accesses available from ACCP to MF-bus!`, and the
  `in DOSEND_MULTI_OCTO` / `in DOREC_MULTI_OCTO` octobus routine names. There is
  also a **43-entry console command table at `0x0130FE`** (LOAD-CONTROL-STORE,
  START-MICROPROGRAM, SEND-OCTOBUS, RESET-CPU, ...) and a **3072 x 16-byte block
  of selftest control-store microcode at `0x013C30`**.
  Correction to the sentence this replaces: the printable runs in the LOW region
  really are incidental, but the image as a whole is full of real text.
- **The RAM map — now PROVEN, not inferred.** The reset routine walk-tests
  `0x00110000-0x00113FFF` and `0x00114000-0x00117FFF`, then zeroes both. That is
  the card's 4x 8192x8 SRAM = 32 KB, as two 16 KB halves, and it corroborates the
  reset SSP.
- **A second hardware address is now pinned**: the SCN2681 DUART at `0x00DD0000`,
  registers on odd bytes (register N at `0xDD0000 + 2N + 1`), proven by the
  SRA/THRA/SRB/THRB accesses in the transmit routine at `0x001D4C`.

## Resolved 2026-07-28 - the image is now fully disassembled

The firmware has been completely carved. Every one of its 279 functions is named, all 43
console commands are decoded with their handlers, and the octobus transmit/receive registers
are proven from the console commands themselves. See
`../../../../SINTRAN/ND5000/ACCP-FULL-DISASSEMBLY-PLAN-2026-07-27.md` for the state table and
the other three documents it links.

- **Dump completeness - much stronger evidence now.** The whole image disassembles as valid
  68000 with no unexplained regions: code 0x000000-0x0114FF, text and tables
  0x011500-0x013C2F, selftest microcode 0x013C30-0x01FC2F, and **0x01FC30-0x01FFFF is 976
  bytes of pure zeros**. Every array descriptor resolves to text that reads correctly, the
  43-entry command table fits its name blob exactly, and the microcode blob's
  two-dimensional descriptor `{3072} x {8}` accounts for its extent to the byte. A corrupted
  or short dump would not do that. **This is still not a checksum** - no integrity word was
  found - but it is far more than "it looks plausible".
- **No embedded symbol table.** Settled: no printable run of 6+ characters exists anywhere
  above the microcode blob, and the tail is zeros. Unlike the ENCOS image (241 linker symbols
  at 0x663E0) this ROM carries none.

## Still not verified

- **The ROM base address on the real hardware.** The vectors point at low
  offsets inside the image, which is consistent with the ROM being mapped at
  `0x000000` at reset (the usual 68000 arrangement, often with the ROM later
  shadowed/switched out in favour of RAM). This has *not* been confirmed
  against a schematic or address decoder — it is an inference from the image
  alone. Nothing found since changes that.
- **Whether the dumps are complete/clean.** No checksum or CRC word was
  located in the image, so the dumps have not been verified against any
  internal integrity value. Weak positive evidence only: the firmware
  disassembles cleanly, its strings are intact, and its internal array
  descriptors point at exactly the right places.
