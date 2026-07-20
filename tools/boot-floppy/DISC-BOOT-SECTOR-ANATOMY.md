# Anatomy of a real Norsk Data **hard-disk** boot sector (page 0)

Scope: the page-0 bootstrap that exists on **installed mass-storage packs** --
SMD, ST-506/MFM Winchester, SCSI. Floppy boot loaders (BPUN / FLOMON) are
**out of scope** here and are covered by the rest of this directory; the FLOMON
blob is decoded only far enough to say what it is.

Every claim below is tagged **[VERIFIED]** (read directly out of bytes or a
cited file) or **[INFERRED]** (a reading of the evidence that I could not prove).
Where I could not decode something I say so rather than inventing a story.

Companion artefacts produced with this document:

| file | content |
|---|---|
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\boot-sectors\installed-smd-296ed770.bin` | SMD page-0 region, 2000 bytes |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\boot-sectors\installed-smd-296ed770.md` | provenance + octal dump + full disassembly + patch points |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\boot-sectors\installed-winchester-0ab983b4.bin` | Winchester page-0 region |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\boot-sectors\installed-winchester-0ab983b4.md` | ditto |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\boot-sectors\installed-scsi-d90b55c5.bin` | SCSI page-0 region |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\boot-sectors\installed-scsi-d90b55c5.md` | ditto |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\boot-sectors\installed-floppy-f3fe2d5a.bin` | FLOMON floppy stream (out of scope, kept for completeness) |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\boot-sectors\installed-floppy-f3fe2d5a.md` | ditto |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\nd100_disasm.py` | table-driven ND-100 disassembler |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\extract_disc_boot.py` | blob decoder + read-only page-0 scanner |
| `E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\make_disc_boot_docs.py` | regenerates the `.bin`/`.md` pairs above |

All disk images were opened **read-only** (`open(path, "rb").read(2048)`); no
file under `D:\ND\` was modified.

---

## 1. The four RetroFS blobs, decoded

Source: `E:\Dev\Ronny\RetroFS\src\RetroFS.NDFS\Creation\NdfsBootBlobs.cs`
lines 25-38 (four base64 constants, `BootRegionSize = 2000`, line 22).

| blob | length | SHA-256 | meaningful | record format |
|---|---|---|---|---|
| `Smd` | 2000 B (1000 w) | `296ed770239c4fd6e1fa7626582c6b2f67d02c65faba2961136bd0a48cd8f1a3` | 1856 B (928 w) | raw binary, starts `PIOF` 0150405 |
| `Winchester` | 2000 B (1000 w) | `0ab983b49bef91f1fdb26eed0b56a23590dcb2173fad6ba7df380f999f40b506` | 1888 B (944 w) code, then filler | raw binary, starts `PIOF` 0150405 |
| `Scsi` | 2000 B (1000 w) | `d90b55c504841c4e85b1703e0525a299be8470444f6af6edcbb3c8546bf6bd70` | 1856 B (928 w) + trailing table | raw binary, starts `PIOF` 0150405 |
| `Floppy` | 2000 B (1000 w) | `f3fe2d5a0021303b0f77cb4f34bd66b0ea471b997ce6126326f7cf3a06c1a439` | whole region | **FLOMON**, byte-expanded; `'!'` at byte 13 |

**[VERIFIED]** all four figures computed by `extract_disc_boot.py blobs`.

Note on "meaningful". The number quoted is the *structural* end of the code,
taken from the loader's own relocation constants (§3.2), not a heuristic:

* SMD -- body B ends at page word 927; the blob is then all-zero to the end.
  The heuristic in `extract_disc_boot.py` (strip trailing zeros/filler)
  independently reports 928 words, so the two agree.
* Winchester -- body B ends at page word 943, i.e. 944 words. From page word
  944 the page is the repeating filler `155555 133333 066666`. The heuristic
  reports 1000 words because that filler is a 3-word cycle, not a single
  repeated value; **trust 944**.
* SCSI -- body B ends at page word 927, then a separate 72-word table occupies
  page words 0o1640..0o1747 (§7). The heuristic reports 1000 words for that
  reason.

### 1.1 Provenance -- these are genuine, and I can name the donor packs

**[VERIFIED]** by SHA-256 over bytes 0..1999 of page 0 of real images on this
machine:

| blob | byte-identical real images (read-only) |
|---|---|
| `Smd` | `D:\ND\HDD\BIGDISK0-K.IMG`, `D:\ND\HDD\BIGDISK0-K2.IMG`, `D:\ND\img-backup\BIGDISK0-K.IMG`, `D:\ND\cv\BIGDISK0-K.IMG` |
| `Winchester` | `D:\ND\HDD\1325.img`, `D:\ND\c3\1325.img`, `D:\ND\img-backup\1325.img`, `D:\ND\HDD\c3_2024_1.img` (and `_2`,`_5`,`_6`,`_7`), `D:\ND\HDD\disk-dump-1k.img`, `D:\ND\c3\2024\c3-recovered.img` |
| `Scsi` | `D:\ND\HDD\scsi-1.img`, `D:\ND\HDD\disk.image`, `D:\ND\HDD\MacDisk.img`, `D:\ND\HDD\test.IMG`, `D:\ND\img-backup\scsi-k.img` |

This closes two items previously marked OPEN in
`E:\Dev\Ronny\NDInsight\SINTRAN\Filesystem\boot-creation.md` (lines 493-499:
"We have no real Winchester boot image" / "no real SCSI boot image"). Real
bytes for both now exist and are disassembled here.

### 1.2 Why the blob is 2000 bytes and not 2048

**[VERIFIED]** Page 0 is 2048 bytes but bytes **2000..2047** (page words
1000..1023) are **not** bootstrap -- they are the NDFS volume label. Read
directly from the real images:

```
BIGDISK0-K.IMG  words 1000..1023:
  010121 000000 000000 000000 100000 000000 000000 110121
  050101 041513 026517 047105 023400 000000 000000 000000
  040000 044374 040000 044376 000000 044044 000000 044006
```

Words 1008..1012 = `050101 041513 026517 047105 023400` = ASCII
`P A C K - O N E '` -- the pack name **PACK-ONE**, which is exactly the drive
named in the RetroFS source comment on line 24. The same string appears in
`1325.img` and `scsi-1.img`.

Consequence: **[VERIFIED]** a whole-page (2048-byte) IOX scan produces false
positives from this label. `scsi-1.img` word 1007 is `0167154`, which matches
the `IOX` opcode mask and reads as "IOX 3154" -- it is volume-label data, not an
instruction. `NdfsBootLoader.DetectControllerType` scans the whole 2048 bytes
and could hit it; it happens to be saved by first-match-in-order because the
genuine `IOXT` at word 290 comes first.

---

## 2. The load contract the boot sector has to satisfy

**[VERIFIED]** from `E:\Dev\Ronny\NDInsight\SINTRAN\Filesystem\boot-creation.md`
lines 61-85, citing ND-06.014.2A §4.2.5.2/§4.2.5.3:

* ALD bit 13 = 1 selects **mass-storage load**; ALD bits 0-10 give the
  **controller's lowest device address**.
* The firmware reads **1 KW (1024 words = 2048 bytes) from mass-storage
  address 0 into memory starting at address 0** and **starts the CPU at
  address 0**.
* Therefore page 0 is a raw ND-100 program with **no header and no checksum**;
  word 0 is the first instruction executed. Every image examined here starts
  with `PIOF` (0150405).

**[VERIFIED] There is no ALD value stored anywhere in the boot sector.** ALD is
a thumbwheel/console input; the bootstrap *reads* it (`TRR ALD`, page word
0o11) but does not carry a copy. So "retarget the ALD" is not a boot-sector
edit at all.

---

## 3. Structure: the page is a *relocator*, not the program

This is the single most important structural fact and it is **[VERIFIED]** by
reading the relocation constants and following the copy loops.

Page 0 has three parts:

```
page words 0o0000..0o0035   prologue -- runs in place at address 0
page words 0o0036..         body A   -- copied to just under 0o177000
page words 0o0270..         body B   -- the device driver, copied to mid memory
page words (tail)           optional per-controller table (SCSI, Winchester)
page words 1000..1023       NDFS volume label (not code)
```

### 3.1 The prologue (identical in all three, modulo 5 words)

```
000000  150405  PIOF                      ; interrupts + paging off
000001  150001  TRA    STS
000002  175345  BSKP   ONE 12 DA          ; STS bit 12 = "N100" CPU-family flag
000003  124005  JMP    *5                 ; -> 000010   (not an ND-100)
000004  170400  SAA    0
000005  150111  TRR    ACTL
000006  170477  SAA    63
000007  124002  JMP    *2                 ; -> 000011
000010  044023  LDA    *19                ; A <- mem[000033]
000011  150112  TRR    ALD
000012  150110  TRR    CSR
000013  054017  LDX    *15                ; X <- mem[000032] = -(body-A length)
000014  047014  LDA    ,X I *12           ; A <- mem[ mem[000030] + X ]
000015  007014  STA    ,X I *12           ;      mem[ mem[000031] + X ] <- A
000016  132776  JNC    *-2                ; X++, loop while X < 0
000017  044011  LDA    *9                 ; advance the source pointer by
000020  060014  ADD    *12                ;   mem[000034] = body-B length
000021  004007  STA    *7
000022  054013  LDX    *11                ; X <- mem[000035] = -(body-B length)
000023  047005  LDA    ,X I *5
000024  007036  STA    ,X I *30           ; second copy loop
000025  132776  JNC    *-2
000026  125001  JMP    I *1               ; -> mem[000027] = body-A entry
```

Notes:

* **[VERIFIED]** P-relative effective address is `inst_start + disp`, i.e.
  relative to the instruction's own address. Confirmed from
  `E:\Dev\Ronny\ghidra-nd100\ND-100\data\languages\nd100_memory.sinc` line 22
  (`reloc = inst_start + disp`) and independently from the `JNC *-2` loop
  structure above (the loop must re-enter at the `LDA`, which only works with
  `here + disp`).
* **[VERIFIED]** `BSKP` encoding: 9-bit opcode in bits 15-7, condition in bits
  8-7, bit number in bits 6-3, register in bits 2-0. Confirmed against
  `ghidra-nd100/ND-100/data/languages/nd100.slaspec` lines 336 and 369-400
  (`op9 = 0x1F0..0x1F7`). `175345` = `BSKP ONE 12 DA`, and STS bit 12 is the
  ND-100-family flag per `nd100-definitions/specs/cpu.yaml`. So words 2-3 are a
  **CPU-model fork**, which is why they are one of the few prologue words that
  differ between images.
* **[INFERRED]** the exact purpose of the `TRR ALD` / `TRR CSR` pair at
  0o11/0o12; `TRR` writes the internal register, and I did not confirm what
  writing ALD does on this CPU. Do not rely on this line.

### 3.2 Relocation constants (page words 0o27..0o35) -- [VERIFIED]

| page word | meaning | SMD | Winchester | SCSI |
|---|---|---|---|---|
| `0o27` | body-A runtime entry (= destination start) | `176546` | `176536` | `176546` |
| `0o30` | body-A source end, as a page word index | `000270` | `000300` | `000270` |
| `0o31` | body-A destination end | `177000` | `177000` | `177000` |
| `0o32` | `-(body-A word count)` | `177546` (-154) | `177536` (-162) | `177546` (-154) |
| `0o33` | (constant loaded into A at 0o10) | `037400` | `037400` | `037400` |
| `0o34` | body-B word count | `001350` (744) | `001360` (752) | `001350` (744) |
| `0o35` | `-(body-B word count)` | `176430` (-744) | `176420` (-752) | `176430` (-744) |

Derived layout (all **[VERIFIED]** arithmetic):

| | body A source | body A runtime | body B source | body B runtime |
|---|---|---|---|---|
| SMD | page `0o36`..`0o267` | `176546`..`176777` | page `0o270`..`0o1637` | `062520`..`064067` |
| Winchester | page `0o36`..`0o277` | `176536`..`176777` | page `0o300`..`0o1657` | `101160`..`102547` |
| SCSI | page `0o36`..`0o267` | `176546`..`176777` | page `0o270`..`0o1637` | `064252`..`065621` |

**Body A always ends exactly at 0o177000.** The body-B destination is a plain
16-bit address held in the page: page word `0o65` (SMD/SCSI) or `0o75`
(Winchester) is the body-B *start* address, page word `0o62`/`0o72` is the
address just past its end.

**Any disassembly of these blobs that decodes page 0 linearly from word 0 is
wrong past word 0o35.** The bodies must be disassembled at their runtime bases.
That is what `tools/nd100_disasm.py --start/--base` is for, and what the
companion `.md` files do.

---

## 4. What body A does

Body A is a short dispatcher plus a large parameter block. Runtime layout
(SMD, base `176546`):

```
176546  044061  LDA    *49          ; A <- mem[176627]  = 176726
176547  146153  RADD   CLD SA DB    ; B <- 176726       <-- the parameter block
176550  050027  LDT    *23          ; T <- mem[176577]  = device class
176551  173377  AAT    -1
176552  140006  SKP    DT EQL       ; class 1 ?
176553  124006  JMP    *6
...                                  ; per-class setup
176600  054371  LDX    *-7          ; X <- mem[176571]  = 0o176 (126)
176601  050777  LDT    -1,B         ; T <- mem[B-1]     = unit number (0)
176602  154006  SHT     6           ; T <<= 6
176603  044373  LDA    *-5          ; A <- mem[176576]  = 0
176604  146056  RADD   SA DT        ; T = (unit << 6) | 0
176605  024366  LDD    *-10         ; D:A <- mem[176573]:mem[176574] = 0 : 0o400
176606  135367  JPL    I *-9        ; call mem[176575] = body-B entry
176607  124371  JMP    *-7          ; loop
176610  124002  JMP    *2           ; two-entry dispatch, reached on a skip return
176611  124013  JMP    *11
176612  004014  STA    *12
176613  150206  MCL    PID          ; x4
176617  165544  IOX    1544         ; SMD status read (SAA 0 on the SCSI page)
176620  175025  BSKP   ZRO 2 DA
176621  124372  JMP    *-6
176624  143611  IDENT
176625  125003  JMP    I *3
```

**[VERIFIED]**

* `B` (the parameter-block pointer) is the word at runtime `176627`:
  `176726` for SMD and SCSI, `176741` for Winchester.
* `mem[B-1]` is the **unit number**; it is **0** in all three images.
* `mem[B-3]` is the **controller device base** (see §5).
* The driver call passes `T = unit<<6 | 0`, `D:A = 0:0o400`, `X = 0o176 (126)`.
  The driver's first act is `STF -26,B` / `STX -23,B`, i.e. it spills `T,A,D`
  and `X` into the block, then checks `T & 63 == 0`.

**[INFERRED / not proven]** Which of `D:A = 0:0o400` and `X = 126` is the disk
address, the core address and the word count. `0o400` = 256 and `126` are the
only two transfer-shaped constants in body A, and the driver later divides by
the words-per-sector constant (`LDX 31,B` / `RDIV SX`, runtime `060610`), which
is consistent with `0o400` being a *word* quantity that gets converted to a
sector number -- but I did not trace it to a conclusion. **I cannot state the
load address and word count of the second-stage system image as fact.** What I
can state is: the constants live at page words `0o61`, `0o63`, `0o64`
(SMD/SCSI) / `0o71`, `0o73`, `0o74` (Winchester) and are identical across all
three media types.

**[INFERRED]** Where control finally goes. The transfer to the loaded system is
*not* a single constant in the page: the loop at `176600..176607` calls the
driver through the indirect pointer at `176575`, and body B ends its paths with
`EXIT` and with `JMP I` through an in-body dispatch table (runtime `060634`..
`060645` on the SMD image, all pointing back inside body B). The final handoff
is parameter-driven. This agrees with the earlier note in
`SINTRAN/Filesystem/boot-creation.md` lines 204-206. **I could not identify a
single "jump to the loaded system" instruction and will not invent one.**

---

## 5. What VARIES between the media types

### 5.1 Device number -- one data word, plus every literal `IOX`

**[VERIFIED]** the controller base is held as a **data word at `mem[B-3]`**:

| | page word | value | low 11 bits |
|---|---|---|---|
| SMD | `0o213` | `001540` | `0o1540` -- SMD/ECC window |
| Winchester | `0o236` | `000500` | `0o0500` -- ST-506 window |
| SCSI | `0o213` | `144300` | `0o0300` |

For the Winchester the same value is also used by the embedded `IOXT` driver as
`LDT -3,B; AAT <reg>; IOXT` (runtime `102174`..). For SCSI it is the **only**
device-number source, because SCSI touches the controller exclusively through
`IOXT`.

For SMD and the Winchester's primary driver the device number is **also baked
into every literal `IOX` instruction**, so `mem[B-3]` alone is not sufficient to
retarget them. Full inventory (page-word offsets, from `extract_disc_boot.py`):

| | literal `IOX` in body A | literal `IOX` in body B | `IOXT` in body B |
|---|---|---|---|
| SMD | 1 (`0o107`, `IOX 1544`) | 31, devices 1540-1547 | 0 |
| Winchester | 1 (`0o117`, `IOX 0504`) | 24, devices 0500-0507 (+ `IOX 0004`, `0012`, `0013`) | 18 |
| SCSI | 0 | 2 (`IOX 0012`, `IOX 0013` -- real-time clock, not the disk) | 42 |

So: **SCSI is fully parameterised by one word. SMD needs 32 instruction edits.
Winchester needs 25 instruction edits plus the word.**

### 5.2 There is genuinely different code per controller

**[VERIFIED]** by word-level diff of the three 1000-word regions:

* SMD vs SCSI: identical for page words `0o0`..`0o271` except **12 words**;
  then completely different from page word `0o272` (= body B) onward. The
  shared front is the generic relocator + dispatcher; body B is a *different
  driver program*.
* SMD vs Winchester: 892 of 1000 words differ, with divergence already inside
  body A (page words `0o57`..`0o125`). The Winchester body A is 8 words longer.
* The Winchester page carries **two** drivers: an `IOX 050x` literal one
  (page words `0o350`..`0o1072`) and an `IOXT` one (page words `0o1312`..
  `0o1641`) whose code is byte-for-byte the same instruction sequence as the
  SCSI driver's opening (compare runtime `102160`..`102215` with `064420`..
  `064452` -- identical). The SCSI page does **not** carry the Winchester
  driver.

So the answer to "is it only the device number?" is **no**. It is a different
driver body per controller family, selected at build time, with a
device-class word telling body A which one it has.

### 5.3 The device-class word

**[VERIFIED]** body A's `LDT` at the third instruction reads a class selector:

| | page word | value |
|---|---|---|
| SMD | `0o67` | `1` |
| Winchester | `0o77` | `2` |
| SCSI | `0o67` | `3` |

Confirmed across other real images too (see §6).

### 5.4 **Yes -- the boot sector encodes GEOMETRY**

This is the answer to the question that matters downstream. **[VERIFIED]** a
contiguous 9-word block sits inside body A, anchored by the constant `0o1000`
(512):

| slot | SMD (`BIGDISK0-K`) | Winchester (`1325`) | SCSI (`scsi-1`) |
|---|---|---|---|
| page word of slot +0 | `0o255` | `0o132` | `0o255` |
| +0 | `0o1000` = **512** | `0o1000` = **512** | `0o1000` = **512** |
| +1 | `0o22` = **18** | `0o11` = **9** | 0 |
| +2 | `0o132` = **90** | `0o110` = **72** | 0 |
| +3 | `0o1466` = **822** | `0o1775` = **1021** | 0 |
| +4 | `0o1465` = **821** | `0o1763` = **1011** | 0 |
| +5 | 0 | 0 | 0 |
| +6 | 0 | `0o1776` = **1022** | 0 |
| +7 | 0 | 0 | 0 |
| +8 | `0o23` = **19** | `0o15` = **13** | `0o36` = **30** |

Readings:

* **[VERIFIED]** slot +0 = **words per sector**. The SMD driver does
  `LDX 31,B` (= `mem[B+31]`, the `0o1000` slot) followed by `RDIV SX` at
  runtime `060610`/`060611` -- it is used as a divisor. 512 words = 1024 bytes
  per sector, so an NDFS 2048-byte page is 2 sectors.
* **[INFERRED, but strongly]** slot +1 = **sectors per track** and slot +2 =
  **sectors per cylinder**. 90/18 = **5 heads** for the SMD pack (an
  18-sector, 5-surface, 823-cylinder 80 MB SMD -- CDC 9762 class); 72/9 =
  **8 heads** for the Winchester, and 8 heads is exactly the Micropolis 1325
  named in the RetroFS comment. The values are self-consistent in both cases,
  which is what makes this more than a guess -- but I did not trace the driver
  code that consumes +1 and +2.
* **[INFERRED]** slots +3 and +4 are cylinder counts. SMD 822/821 differ by 1;
  Winchester 1021/1011 differ by 10, and the 1325 has 1024 physical cylinders.
  A plausible reading is "cylinders present" vs "cylinders usable after
  alternates", but **I cannot determine which is which** and the two disks do
  not agree on the relationship.
* **[UNKNOWN]** slot +8 (19 / 13 / 30). It is present and non-zero on all
  three, including SCSI where every geometry slot is zero, so it is *not*
  geometry. I could not determine what it is.
* **[VERIFIED]** for SCSI, slots +1..+7 are **all zero** -- consistent with
  linear (LBA) addressing needing no CHS translation. This is a usable
  discriminator on its own.

Caveat: the block is **not at a fixed B-relative offset**. On SMD/SCSI it sits
at `B+31..B+39`; on the Winchester at `B-71..B-63`. Locate it by scanning body A
for the `0o1000` anchor, which is what `variants` scanning does below, not by a
hard-coded offset.

---

## 6. Cross-check against every other installed pack on this machine

`extract_disc_boot.py scan` was run read-only over every `*.img/*.IMG/*.image`
larger than 2 MB under `D:\ND\`. Distinct hard-disk boot sectors found
(SHA-256 prefix of bytes 0..1999):

| sha8 | class word | controller evidence | geometry (sec/tr, sec/cyl, cyl, cyl2) | example image |
|---|---|---|---|---|
| `296ed770` | 1 | `IOX 154x` | 18, 90, 822, 821 | `D:\ND\HDD\BIGDISK0-K.IMG` **(= RetroFS `Smd`)** |
| `ec962fc2` | 1 | `IOX 154x` | 18, 90, 822, 821 | `D:\ND\HDD\BIGDISK0-L.IMG` |
| `86e648bd` | (different layout) | `IOX 154x` | 18, 90, 822, 821 | `D:\ND\HDD\BIGDISK0-M.IMG` |
| `059ac510` | 1 | `IOX 154x` | 18, 90, 822, 821 | `D:\ND\HDD\BIGDISK0.IMG` |
| `80ba323f` | 1 | `IOX 154x` | 18, 90, 822, 821 | `D:\ND\HDD\c3-k-bd-clean.img` |
| `21382ad9` | - | `IOX 154x` | - | `D:\ND\HDD\a.IMG` |
| `57e23513` | - | `IOX 154x` | - | `D:\ND\moved\BIGDISK0-L.IMG` |
| `1da707c1` | 0 | `IOX 0300,0302,0303,0305,030x` + `IOX 0004` | none found | `D:\ND\HDD\BIGDISK0-H.IMG` |
| `0ab983b4` | 2 | `IOX 050x` + `IOXT` | 9, 72, 1021, 1011 | `D:\ND\HDD\1325.img` **(= RetroFS `Winchester`)** |
| `e2da6491` | (different layout) | `IOX 050x`, no `IOXT` | 9, 72, 1021, 1011 | `D:\ND\HDD\HD0.IMG` |
| `05afdb0f` | 2 | `IOX 050x` + `IOXT` | 9, 72, 1021, 1011 | `D:\ND\HDD\WD0.img` |
| `d9df77b6` | - | `IOX 050x`, no `IOXT` | - | `D:\ND\c3\Tingo-HDD\try2\tingo_micropolis_1325.img` |
| `c6f30aba`, `8329440e` | - | `IOX 050x` + `IOXT` | - | `D:\ND\HDD\disk-dump.img`, `disk-dump-2k.img` |
| `d90b55c5` | 3 | `IOXT` only | all zero | `D:\ND\HDD\scsi-1.img` **(= RetroFS `Scsi`)** |
| `47f5dc0d` | 3 | `IOXT` only | all zero | `D:\ND\HDD\HD00_imaged.img` |
| `70c3d994` | 3 | `IOXT` only | all zero | `D:\ND\HDD\scsi-k.img`, `tor-disk.img` |
| `9411182f` | 3 | `IOXT` only | all zero | `D:\ND\SI1.img` |
| `a47ce5c4` | (different layout) | `IOXT` only | all zero | `D:\ND\HDD\sintran_iii_m05_st31200n.image` |
| `726d55d5`, `fcab334d`, `ddc530b5`, `f50f5e50` | - | `IOX 0303/0305` (console) + `IOX 154x` | - | the `D:\ND\bsd\BSD_*.IMG` family -- BSD, not SINTRAN |

**[VERIFIED]** notes on that table:

* The class word / geometry columns are only filled where the standard body-A
  layout was recognised. Three images (`86e648bd`, `e2da6491`, `a47ce5c4`) have
  `mem[0o27] = 000315` instead of a `17xxxx` relocation target -- a **different
  page-0 generation** with a different prologue. Their geometry block is still
  findable by the `0o1000` anchor and still reads 18/90/822/821 and 9/72/1021/1011
  respectively, so the geometry structure survives across generations.
* `1da707c1` uses device window **`0o0300`..`0o0307`**, which is a fourth
  controller family not represented in the RetroFS blob set. **I did not
  identify which controller that is**, and no `0o1000`-anchored geometry block
  was found in it.
* `D:\ND\HDD\WD-L.IMG` and `D:\ND\img-backup\WD-L.IMG` start with `076110`, not
  `PIOF`/`IOF` -- **not bootable page 0** by the firmware contract in §2.
* The Sun-2 / SunOS images (`sun2-*.img`, `micropolis1355-sun2-*.img`) and
  `ide-disk-2.img` are not ND boot sectors and were excluded.

---

## 7. The trailing tables

**[VERIFIED]** after body B, two of the three pages hold a table.

SCSI, page words `0o1640`..`0o1747`, an 18-entry x 4-word table:

```
000000 000001 000642 125252
000000 000001 000646 125252
000000 000001 000652 125252   ...  third word steps by 4 octal
```

Winchester, page words `0o1640`..`0o1747`: 10 words of what disassemble as
plausible instructions (`150415 IOXT`, `175075`, `124003`, ... `164013`,
`164012`) followed by the repeating filler `155555 133333 066666`.

**[INFERRED]** the SCSI table looks like a per-something descriptor array
(`125252` = `0o125252` is the classic `101010...` fill pattern, and the third
word is a monotonically stepping address). **I could not determine what it
describes** and it is not referenced by any code I decoded. The SMD page has
this area all zero.

---

## 8. Patch points -- can the boot sector be parameterised?

Answer: **partially, and it differs sharply by media type.**

### 8.1 Fully parameterisable (single-word edits) -- [VERIFIED offsets]

| what | SMD page word | Winchester page word | SCSI page word |
|---|---|---|---|
| controller IOX base (`mem[B-3]`) | `0o213` | `0o236` | `0o213` |
| unit number (`mem[B-1]`) | `0o215` | `0o240` | `0o215` |
| device-class selector | `0o67` | `0o77` | `0o67` |
| words per sector (geometry +0) | `0o255` | `0o132` | `0o255` |
| sectors per track (+1) | `0o256` | `0o133` | `0o256` |
| sectors per cylinder (+2) | `0o257` | `0o134` | `0o257` |
| cylinders (+3) | `0o260` | `0o135` | `0o260` |
| cylinders-2 (+4) | `0o261` | `0o136` | `0o261` |
| unknown (+8) | `0o265` | `0o142` | `0o265` |
| body-A entry / destination | `0o27` | `0o27` | `0o27` |
| body-A source end | `0o30` | `0o30` | `0o30` |
| body-A destination end | `0o31` | `0o31` | `0o31` |
| `-(body-A count)` | `0o32` | `0o32` | `0o32` |
| body-B count / `-count` | `0o34` / `0o35` | `0o34` / `0o35` | `0o34` / `0o35` |
| body-B runtime start / end | `0o65` / `0o62` | `0o75` / `0o72` | `0o65` / `0o62` |
| loader-call constants | `0o61`,`0o63`,`0o64`,`0o66` | `0o71`,`0o73`,`0o74`,`0o76` | `0o61`,`0o63`,`0o64`,`0o66` |

The geometry and unit words are true single-word patch points -- they are pure
data, referenced B-relative, and changing them changes nothing else. **This is
what makes generating a bootable image for a chosen geometry feasible.**

### 8.2 Not parameterisable by a single word

* **SMD**: 32 literal `IOX` instructions (page words `0o107`; then `0o315`,
  `0o316`, `0o331`, `0o416`, `0o470`, `0o504`, `0o541`, `0o557`, `0o724`,
  `0o726`, `0o733`, `0o736`, `0o740`, `0o742`, `0o744`, `0o747`, `0o774`,
  `0o1004`, `0o1043`, `0o1233`, `0o1236`, `0o1240`, `0o1242`, `0o1243`,
  `0o1255`, `0o1261`, `0o1317`, `0o1332`, `0o1344`, `0o1351`, `0o1373`).
  To retarget from base `0o1540` to base `X`, each word must become
  `0o164000 | (X + (old & 7))`. The full list with device numbers is in
  `boot-sectors/installed-smd-296ed770.md`.
* **Winchester**: 24 literal `IOX 050x` (page words `0o117`, `0o350`, `0o371`,
  `0o373`, `0o421`, `0o422`, `0o423`, `0o503`, `0o560`, `0o564`, `0o570`,
  `0o573`, `0o576`, `0o642`, `0o674`, `0o675`, `0o726`, `0o1023`, `0o1035`,
  `0o1042`, `0o1046`, `0o1070`, `0o1072`) plus the `IOX 0004`, `IOX 0012`,
  `IOX 0013` at `0o450`, `0o1657`, `0o1656` which are **not** the disk
  controller (0012/0013 is the real-time clock) and must be left alone.
* **SCSI**: nothing -- all 42 controller accesses are `IOXT`, so the single
  word at `0o213` is the whole device parameterisation.

### 8.3 What cannot be retargeted at all

Changing the **media type** (SMD <-> Winchester <-> SCSI) is not a patch; body B
is a different program. Generating a bootable image for a chosen media type
means selecting the right whole body-B blob, then patching §8.1.

---

## 9. What I could not decode -- explicit list

Recorded so nobody mistakes silence for a finished answer.

1. **The load address and word count of the second-stage system image.** The
   candidate constants are `0o400` (as `D:A = 0:0o400`) and `0o176` (in X), at
   page words `0o64` and `0o61`. I could not prove which is address, which is
   count, or in what units.
2. **The final transfer of control** into the loaded system. It goes through
   body B's in-body `JMP I` dispatch table; I did not follow it to the end.
3. **Geometry slots +3 / +4** -- both are cylinder-like, but their exact roles
   differ between the SMD (822/821) and Winchester (1021/1011) packs.
4. **Geometry slot +8** (19 / 13 / 30). Non-zero even on SCSI.
5. **The SCSI trailing table** at page words `0o1640`..`0o1747`.
6. **The `0o0300` controller family** (`1da707c1`, `BIGDISK0-H.IMG` etc.).
7. **`TRR ALD` / `TRR CSR`** at page words `0o11`/`0o12` -- what writing those
   internal registers does at boot time.
8. Body B was disassembled in full but only the first ~120 words of the SMD
   driver were followed semantically. The rest is decoded instruction-by-
   instruction in the companion `.md` files but not narrated.
9. In all three bodies some words decode as instructions but are certainly
   **data** (constants, pointers, dispatch tables). The disassembler cannot
   tell them apart; treat every line inside a long run of `STZ *0` /
   `BORA` / `BLDC` as data.

---

## 10. Tooling

```
# decode the four RetroFS blobs and write the .bin files
python E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\extract_disc_boot.py blobs <outdir>

# summarise page 0 of a real image (read-only)
python ...\tools\extract_disc_boot.py image D:\ND\HDD\1325.img

# one-line scan over many images
python ...\tools\extract_disc_boot.py scan D:\ND\HDD\*.img

# disassemble a body at its runtime address
python ...\tools\nd100_disasm.py boot-sectors\installed-smd-296ed770.bin \
       --start 30 --count 154 --base 0o176546

# regenerate all four .bin/.md pairs
python ...\tools\make_disc_boot_docs.py
```

`nd100_disasm.py` builds its opcode table at run time from
`E:\Dev\Ronny\nd100-definitions\specs\` (`cpu.yaml` +
`instructions/*.yaml` + `operand_types/addressing_modes.yaml`); the only
hand-written correction is the `BSET`/`BSKP` mask, documented in the source with
its `ghidra-nd100` slaspec citation.
