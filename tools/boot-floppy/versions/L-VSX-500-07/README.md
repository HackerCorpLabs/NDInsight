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

Both `inputs/` files are generated together by
`E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\extract_layout_params.py`
from the extracted `SINTRAN-L-1:DATA` stream. It takes the source stream as
argument 1 and the output directory as argument 2:

```
python E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\extract_layout_params.py ^
       D:\ND\extract\VSXL1\SINTRAN-L-1.DATA ^
       E:\Dev\Ronny\NDInsight\tools\boot-floppy\versions\L-VSX-500-07\inputs
```

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
