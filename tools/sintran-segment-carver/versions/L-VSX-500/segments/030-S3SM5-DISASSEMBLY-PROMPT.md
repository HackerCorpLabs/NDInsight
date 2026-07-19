# Disassembling the ND-500 System Monitor (030-S3SM5.bin) + aligning N500-SYMBOLS

Target: `030-S3SM5.bin` in this folder — SINTRAN segment **30 (octal)** `S3SM5`
("ND-500 System Monitor segment", manual §8.3). 48 pages / 49152 bytes. Save copy:
`062-S3SSM5.bin` (identical, cross-check).

## CRITICAL — this is ND-500 code, NOT ND-100 code
Do **not** load it with the ND-100 Ghidra processor / `nd100-dis`. **Verified:**
- `nd100-dis` finds only **108** recognizable control-flow instructions in the
  whole segment, vs **6373** in the same-size ND-100 segment `006-S3FS.bin` — i.e.
  it is noise as ND-100.
- The ND-500 disassembler produces valid ND-500 mnemonics (`ret`, `clr`, `go`,
  `if > go`, …) from the same bytes.
- Repo confirms the split: `..\..\..\..\SINTRAN\ND500\ND500-L-RELEASE-RE-TASK-HANDOFF.md`
  — `:PROG` = ND-100 code, `:PSEG/:DSEG` = **ND-500 machine code, byte-addressed**.
  `S3SM5` is ND-500 (32-bit, byte-addressed) code mapped by SINTRAN via 5PIT.

So: use an **ND-500 processor**, not ND-100.

## Tool: nd500-dis (WSL)
`/home/ronny/repos/ragge/pcc-nd500/bin/nd500-dis` — ND-500/ND-5000 disassembler.
Reads the raw `.bin` **as-is** (PSEG raw format, no byte-swap — unlike the ND-100
`nd100-dis` path). Key options:
- `-a` show addresses + bytes; `-o` octal (matches the symbol tables); `-x` hex.
- `-b <hexaddr>` map file offsets to a virtual base. ND-500 privileged/kernel TEXT
  (Domain 0) base is `0x08000000`; the ND-500 System Monitor is privileged code, so
  **start with `-b 0x08000000`** and see whether it aligns with N500-SYMBOLS.
- `-s <hexoffset>` start offset, `-n <count>` limit instructions, `-j` JSON output.

Quick start:
```bash
D=/home/ronny/repos/ragge/pcc-nd500/bin/nd500-dis
S=".../versions/L-VSX-500/segments/030-S3SM5.bin"
"$D" -a -o -b 0x08000000 "$S" | less        # octal, addresses, kernel base
"$D" -j "$S" > 030-S3SM5.nd500.json          # machine-readable for tooling
```
If your Ghidra has an **ND-500 SLEIGH/processor module**, load the raw `.bin` with
it at the ND-500 base instead; if not, `nd500-dis` (esp. `-j`) is the path.

## CORRECTION (VERIFIED) — N500-SYMBOLS do NOT label this ND-500 code
A follow-up analysis established three things that change the approach below:
1. **`N500-SYMBOLS` values are 16-bit ND-100 interface addresses** (max `177777₈`),
   **not** 32-bit ND-500 byte offsets. Testing in-range names (e.g. `UNFIX=112463`)
   as byte offsets lands mid-instruction on desynced fragments, not routine entries.
   So these symbols do **not** label the S3SM5 ND-500 routines — do not rely on them.
2. **`nd500-dis -b` is display-only for raw files** — it relabels addresses but does
   not re-decode; offset-0 and `-b 0x08000000` produce identical bytes. There is no
   base that "aligns symbols" by re-decoding.
3. **A linear sweep desyncs**: ND-500 has variable-length instructions, so
   `030-S3SM5.asm` is ~53% `??? opcode 0x0000` and the decoded instructions near data
   are nonsense. Correct disassembly needs **control-flow seeding from real entry
   points**, which are not yet known.

Practical consequence: recovering S3SM5's own MON dispatch from this artifact alone
is not currently feasible. Better path — use the **verified ND-100 side**: the
routing and handlers are proven in `MP-P2-N500.NPL` (see
`../../../../SINTRAN/ND500/MON/ND500-MON-CALL-ROUTING-MAP.md`), which shows the ND-100
services 500–515 and forwards the rest (incl. 410–427). To decode S3SM5 itself, first
find real ND-500 entry points (e.g. via a live ND-500 run in `nd500x`, or the domain
header), then seed the disassembler there.

## Aligning with N500-SYMBOLS (where to find it) — superseded, see correction above
Symbol table: `SINTRAN/NPL-SOURCE/SYMBOLS/L07/N500-SYMBOLS.SYMB.TXT`
(for ND-5000 / M06 there is also `..\M06\N5000-SYMBOLS.SYMB.TXT`). Lines are
`NAME=octaladdr`. A pre-filtered Ghidra import for the S3SM5 window is
`..\re\030-S3SM5.ghidra-symbols.txt` (hex) — but note it was filtered assuming the
ND-100 40000:177777 window, which is the WRONG address space for ND-500 code, so
treat it as approximate.

**Alignment task (this is the real work):** determine how the octal N500-SYMBOLS
values map to the ND-500 virtual addresses `nd500-dis` prints. Candidates to test:
- symbol value = ND-500 byte address relative to `-b 0x08000000`;
- symbol value = word offset (× the ND-500 word size) from the segment base;
- symbol value = ND-100 5PIT-window address (40000-based) that must be translated.
Anchor by content: pick a symbol you can recognize from its expected code
(e.g. a `MON`/trap dispatch, a routine that returns quickly = `ret`) and adjust
`-b` until the labelled address lands on that routine's entry. Then the same `-b`
aligns the rest.

## Goal — the ND-500 MON-call handlers the request targets
These are all > 0377 (not in the ND-100 GOTAB); they are handled ND-500-side:
`MON 410, 411, 416, 417, 425, 426, 427, 500, 501, 505, 510, 511 (DVIO),
512 (XMSG-for-ND-500), 513, 515 (5MTRANS)`. Priorities / showstoppers: **511, 512,
513**. Names from the request: fixseg, unfix, wsegn, mxpisg, sprname, gprnum,
gprname, startpr, stoppr, gerrcod. Find each handler, its entry, and its parameter
convention.

## Build on existing work (don't redo)
- `..\..\..\..\SINTRAN\ND500\ND500-L-RELEASE-RE-TASK-HANDOFF.md` — the ND-500 L
  release RE handoff (artifacts, file formats, priority questions, tooling).
- `..\..\..\..\SINTRAN\ND500\ND500-MONITOR-CALL-PARAMETER-PASSING.md` — MON-call
  parameter passing on the ND-500 side.
- `..\..\ghidra-tasks\TASK-05-undocumented-mon-calls.md` — the full MON-call
  request (ND-100 + ND-500) this segment covers the ND-500 half of.

## Deliverable
Write `..\re\030-S3SM5-results.md`: the confirmed `-b` alignment, a routine map
(N500 symbol → ND-500 address → summary), and for each target MON call the handler
entry + parameter convention, VERIFIED/UNCERTAIN. Feed anything reusable back into
the `SINTRAN\ND500\` docs.
