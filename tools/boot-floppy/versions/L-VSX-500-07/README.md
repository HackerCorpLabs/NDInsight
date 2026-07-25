# L-VSX-500-07 — SINTRAN III/VSX Version L, revision 07

The original ND distribution set for the version the carver knows as
[`L-VSX-500`](../../../sintran-segment-carver/versions/L-VSX-500/).

The `-07` comes from the volume label `250305L07`, which the carver folder
name does not record. `make-segment-ref.py` already depends on this
revision — `VER2SYM = {"L-VSX-500": "L07"}` — so the two now agree
explicitly.

## The diskettes

| volume | image | files |
|---|---|---|
| `250305L07-XX-01D` | `D:\ND\S\VSXL1.IMG` | `MACM-1718L:BPUN` (40,039 B), `SINTRAN-L-1:DATA` (1,095,538 B) |
| `250305L07-XX-02D` | `D:\ND\S\VSXL2.IMG` | `SINTRAN-L-2:DATA` (549,938 B) |
| `250305L07-XX-03D` | `D:\ND\S\VSXL3.IMG` | 15 files: assemblers + symbol tables |

All 8″ DS/DD, 1,261,568 bytes (77 × 8 × 1024 × 2), 616 pages, single user
`SYSTEM`. Diskettes 1–2 dated **1988-07-08**, diskette 3 **1988-12-16**.

A duplicate of diskette 3 exists at
`D:\ND\S\Test-microprogram\test\VSXL3.IMG`.

### Diskette 3 in full

```
NEW-SYSTEM:PROG            DMAC-1915G:BPUN          COS-TADADM:BPUN
F32-FMAC-1920C:PROG        F48-FMAC-1408D:PROG
FILSYS-SYMBOLS:SYMB        RTLO-SYMBOLS:SYMB        N500-SYMBOLS:SYMB
SYMBOL-1-LIST:SYMB         SYMBOL-2-LIST:SYMB       LIBRARY-MARKS:SYMB
XMSG-COMMAND:PROG          XMSG-STARTEX:MODE        XMSG-STARTEX:BATC
XMSG-SYMBOL-LIST:SYMB
```

Companion sets for this version: `210373L03-XX-01D` (XMSG L03) and
`211305B02-XX-01D` (ND-500(0) system package for VSX L).

## What is here

| file | contents |
|---|---|
| `inputs/distribution-layout-params.txt` | the verbatim 7,457-byte MACM header from `SINTRAN-L-1:DATA` — patching procedure, macro legend, and the `NAME=page` layout parameters |
| `inputs/distribution-layout-params.json` | the same, parsed: 34 parameters with page/madr values, plus the 21-entry macro→area legend |
| `carve-crosscheck.md` | **the findings** — what this confirms, corrects and leaves open in `segment-facts.json` |

## The two tools for this distribution

Both live in `E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\`. They are
specific to this floppy set — they hard-code L07 paths — and each carries a
full header comment, so the file itself is the reference. Summary:

### `extract_layout_params.py` — generates everything in `inputs/`

**This is the only thing that produces the two `inputs/` files.** They are
committed; if either is ever doubted, re-run this and diff.

`SINTRAN-L-1:DATA` is the MACM generation stream ND shipped. Its first 7,457
bytes are plain text that MACM reads: `NAME=octal` lines giving each SINTRAN
system area's SEGFIL page number, plus a `%%` legend mapping patch-macro
names to area names. Everything after that first control byte is binary and
is ignored.

Why it is worth having: that header is a **third, independent witness** to
the segment page map — neither the live `LIST-SEGMENT` dump nor the OCR'd
release-manual §8.3. It is what confirmed 28 of 32 layout parameters against
carved `madr` values and let 30 segments rated `medium` *only* because of OCR
damage be promoted to `high`. See [`carve-crosscheck.md`](carve-crosscheck.md).

One non-obvious rule it implements: in an expression such as `300-2`, the
leading additive terms are the **page address** and a trailing `-N` is a
length/in-page offset that is **not** part of the address. SEGFIL page
numbers relate to the carver's `madr` as `madr = page - 0o200`.

```
python E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\extract_layout_params.py ^
       D:\ND\extract\VSXL1\SINTRAN-L-1.DATA ^
       E:\Dev\Ronny\NDInsight\tools\boot-floppy\versions\L-VSX-500-07\inputs
```

Verified 2026-07-25: reproduces both committed files byte-for-byte in
content (7,457-byte header, 34 parameters, 21 legend entries); the only
difference is line endings — CRLF in the repository, LF from the script.

### `analyze_vsxl1.py` — how the shipped MACM differs from the analysed one

Read-only; prints, writes nothing. It parses each BPUN tape into **the
program it actually loads** (skips the NUL leader, finds `!`, reads the
big-endian base and word count, then the words and the trailing checksum)
and reports base, length, span and checksum validity for the floppy's
`MACM-1718L:BPUN` against the standalone `D:\ND\BPUN\MACM-1718L.BPUN`. It
word-diffs them only when base and length match.

Why it is worth having: **every Ghidra finding in
[`../../MACM-DIALOGUE.md`](../../MACM-DIALOGUE.md) and
[`../../MSTYP-SWTYP-BRIDGE.md`](../../MSTYP-SWTYP-BRIDGE.md) was made on the
standalone binary**, and ND shipped a different build. `MACM-DIALOGUE.md`
open question 7 records this and says the floppy copy "was **not** examined".
This is the tool for it.

Measured 2026-07-25:

| copy | base | words | span | checksum |
|---|---|---|---|---|
| floppy | `076203` | 19738 | `076203`–`144634` | `121055` OK |
| standalone | `077120` | 19273 | `077120`–`144630` | `117607` OK |

Both checksums verify, so the difference is **real, not tape damage**. They
are different programs: the floppy build is 465 words larger, with its base
**461 words lower** and its top **4 words higher** (461 + 4 = 465) — i.e.
extended almost entirely *downward*, both images ending at nearly the same
address. Content near the top sits at close to the same address in both
builds; content near the bottom is displaced by ~461 words. That is the
constraint on whether an address-specific finding carries over. Which side
the MSTYP tables fall on is **still open** — this script does not resolve it.

Large binaries are **not** copied in. `SINTRAN-L-1:DATA` is 1.05 MB and the
floppy images are 1.2 MB each; they stay at their `D:\ND\S\` paths and are
extracted on demand.

**Do not add `-p` here.** `VSXL1.IMG` holds `MACM-1718L:BPUN` and
`SINTRAN-L-1:DATA`, both 8-bit binaries; `-p` strips bit 7 and destroys them
(no BPUN checksum then verifies). `-p` is for text files only (`:SYMB`,
`:PATC`).

```
ndtool -x -o D:\ND\extract\VSXL1 D:\ND\S\VSXL1.IMG
ndtool -x -o D:\ND\extract\VSXL3 D:\ND\S\VSXL3.IMG
```

## Findings in one line each

- **28 of 32** layout parameters match carved `madr` values exactly.
- Segment **61**'s description is a copy-paste of segment 60's — the floppy
  shows it is XMSG **XROUT**, not the kernel.
- **30** segments marked `medium` confidence only because of OCR damage in
  the manual can be promoted to `high` — this is a third, non-OCR witness.
- One symbol-table corruption in the repo: `ENT0-011302` → `ENT0=011302`.
- One genuine open conflict (`PRD` vs `S3SDPIT`) — flagged, not "fixed".

Details and evidence: [`carve-crosscheck.md`](carve-crosscheck.md).
