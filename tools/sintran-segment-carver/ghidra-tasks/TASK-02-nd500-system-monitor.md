# TASK-02 — Map the ND-500 System Monitor + extended MON calls

Do the [shared setup](README.md#shared-setup-every-task-does-this-first) first.

## Load
| Program | File | Base | Symbols |
|---|---|---|---|
| S3SM5 | `segments\030-S3SM5.bin` | `0x4000` (oct 40000) | `re\030-S3SM5.ghidra-symbols.txt` |

Segment 30 (octal) = `S3SM5` (live) / `S3SMS5` (manual §8.3), "ND-500 System
Monitor segment", page range 40000:177777. The save copy is `062-S3SSM5.bin`
(identical content, same base) — use it to cross-check.

**Verify:** after applying `re\030-S3SM5.ghidra-symbols.txt`, `N500`-prefixed
labels should land on real routine entries in the `0x4000+` range. Pick any label
from the symbol file and confirm it sits on a `JPL`/instruction boundary, not mid-data.

## Goal
Produce a map of the ND-500 System Monitor: its entry points, the ND-100↔ND-500
communication path, and especially the **extended ND-500 MON call handlers** that
do NOT go through the ND-100 `GOTAB` (call numbers > 255 / ND-500-side):
- `MON 300` EUSEL, `MON 347` NUCL, `MON 350` RWSEG, `MON 440` AttachSegment,
  `MON 515` SMTRANS (per the L release manual §10). Find where each is handled.

## Context (from prior work)
- The ND-100-side MON dispatch (GOTAB, level 14) is TASK-03; the ND-500 extended
  calls route to THIS segment instead. `../SINTRAN\OS\23-MON-CALL-DISPATCH-DEVELOPER-GUIDE.md`
  covers the ND-100 side and flags the ND-500 routing as the open piece.
- Symbol table `N500-SYMBOLS.SYMB.TXT` (L07). For M06 there is also
  `N5000-SYMBOLS.SYMB.TXT` (ND-5000).
- Related repo docs: `..\..\SINTRAN\ND500\` (bus interface, domain setup),
  `..\..\SINTRAN\OS\05-ND500-DMA-KERNEL.md`, `06-MULTIPORT-MEMORY-*`.

## Steps
1. Enumerate the monitor's entry points from the symbol labels; identify the main
   command/dispatch loop.
2. Trace the ND-100↔ND-500 shared-memory / interrupt communication (level-12
   driver on the ND-100 side, `N500DF`, `X500DF`).
3. Locate each extended MON handler (300/347/350/440/515) and summarize what it does.

## Deliverable
Write `versions\L-VSX-500\re\TASK-02-results.md`: entry-point map, the comms path,
and the extended-MON handler table (call number → routine addr hex+octal → summary),
VERIFIED/UNCERTAIN. Note anything that belongs in a future `SINTRAN\OS\` doc.
