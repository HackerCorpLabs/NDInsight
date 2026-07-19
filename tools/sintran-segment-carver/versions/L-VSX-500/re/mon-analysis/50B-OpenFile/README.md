# MON 50B (octal) - OpenFile (OPENF)

> **CORRECTED 2026-07-15 (byte-verified).** The worker + dispatch described below are on the
> DEBUNKED model and are WRONG. Byte truth from the carved L07 image:
> `MCTAB[50B] = 005670B = OPFIL=103034B` in segment 006-S3FS, reached by the real dispatch
> `MON 50B -> ENT14(072167B) -> GOTAB[50B]=MFELL(072114B) -> CALLP(032201B) -> MCTAB[50B]=OPFIL`.
> Any "GOTAB from commoncode" / "uncarved CALLPROC bridge" / "F16xx stub" / old worker name below
> is an artefact of the wrong table. Verified: `dd if=044-S3IDPIT.bin bs=1 skip=1904 count=2`
> -> `86 1c`. Cross-ref ../317B-ExecuteCommand/README.md and SINTRAN/CARVING-HANDOFF.md sec 3a.

Opens a file for access. The caller supplies a file name, a default file type,
and an access code (sequential/random, read/write/append, contiguous); on the
normal return the open file number is delivered back to the caller. A file must
be opened with this call before it can be read or written, and later closed with
[MON 43B CloseFile](../43B-CloseFile/README.md).

**Status:** GOTAB dispatch head byte-proven as **fall-through** (`GOTAB[50B] = 000000`,
no per-call stub); the OPENF worker body is real SINTRAN L bytes and calls the
`FOPEN` file-open primitive; the exact `MON 50 -> worker` link crosses an
uncarved kernel bridge (see [Honest caveats](#honest-caveats)). All addresses/values
are **octal**.

- **Full disassembly:** [`50B-OpenFile.ASM`](50B-OpenFile.ASM) - the actual code (the OPENF worker body; there is no entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 50B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[50B] = 000000<br/>(byte-proven: fall-through)"]
    C -.uncarved MFELL / CALLPROC.-> E["OPENF open worker<br/>006-S3FS :123525B"]
    E --> F["FOPEN file-open primitive<br/>006-S3FS :67432B"]
    class A blue
    class B,C blue
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The GOTAB slot is zero, so there is **no per-call entry stub**. The dashed hop
(`C -> E`) is the resident `MFELL`/`CALLPROC` fall-through second-level dispatch -
it is **not present in any carved segment**, so it is the one link that cannot be
followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words x 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[50] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071303B` (1 word) | 58758 | `GOTAB+50` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| OPENF open worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `123525B-123637B` (113w) | 63146 | `OPENF` | real bytes; link **MISATTRIBUTED** |
| FOPEN file-open primitive | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `67432B` (call target) | 32340 | `FOPEN` | called by OPENF (link cell `123633`) - **VERIFIED** |

**Verify by hand:** `grep '^123525 ' ../../segments-ref/006-S3FS/006-S3FS.hex` -> byte offset `63146`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=63146 count=8 | od -An -tx1` -> `22 3b cc 65 cc 59 f0 48`
(= octal `021073 146145 146131 170110` = `STD I 73` / `RADD CLD SL DA` / `RADD CLD SB DD` / `SAB 110`, the OPENF entry).

The GOTAB slot itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58758 count=2 | od -An -tx1` -> `00 00` (= `000000`, fall-through).

---

## Instruction walkthrough

Full listing: [`50B-OpenFile.ASM`](50B-OpenFile.ASM). The functional body is the
OPENF worker (region B); there is no F16xx stub because `GOTAB[50] = 0`. All
calls to shared workers are **indirect** (`JPL I` / `JMP I`) through a table of
pointer words at the tail of the window (`123620-123637`). nd100-dis renders
those pointer words as bogus instructions (`ROP NOOP`, `STZ I ...`, `SUB I ...`) -
they are **data (link cells)**, not code; their contents are the real worker
addresses (resolved below).

**Entry prologue (`123525-123531`)** - `123525 STD I 73` stashes the caller's
double-word parameter; `123530 SAB 110` builds the local frame `B` (110 words -
larger than the scratch workers because Open carries name/type strings);
`123531 JPL I 70` -> `003752` is the shared resident prologue worker (the same
prologue cell used by the scratch and file read/write bodies).

**Name / parameter parse (`123532-123542`)** - `123536` and `123541 JPL I` ->
`044777` (`CLPAR`, parameter/command-line parse) resolve the caller's file-name
and type arguments; a failure jumps to the store-status exit at `123612`.

**Open-file table scan (`123543-123556`)** - `123545 SAT 11` / `123546 SKP IF DT
GRE SX` walk the open-file table (`123550 LDA I ,X 57`, `123553 AAX 1`,
`123554 JMP -7` loop back to `123545`). On no free / no matching slot,
`123555 SAA 104` loads error `104` and exits via `123556 JMP 34 -> 123612`.

**Open + finish (`123557-123617`)** - `123560 JPL I 51` -> `010500` (resident
worker) prepares the slot, then `123565 JPL I 46` -> `067432` (**FOPEN**, the
file-open primitive) performs the actual open. On error the OUTRC/OCTAL
formatting workers are called (`123573 JPL I 42` -> `040730` `OUTRC`,
`123604 JPL I 32` -> `040336` `OCTAL`). The two store-status points
`123612`/`123614 STA ,B 2` write the result word into the caller's status slot
`B+2`; every path funnels into the resident return `123611`/`123617 JMP I` ->
`003776`.

The `JPL I 46` call to **FOPEN** (link cell `123633 = 067432`) is the byte-level
proof that OPENF is the OpenFile worker: it is the entry that drives the file
system's open primitive. (The neighbouring open-family symbols `OPFIL=103034B`,
`DOPEN=103026B`, `FOPEN=67432B` are called *by* OPENF or are siblings; OPENF at
`123525B` is the outermost open entry - it carries the standard file-worker
prologue `STD I 73` / `SAB` / `JPL I 70 -> 003752`, identical in shape to the
scratch and file read/write workers.)

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `123525B` = OPENF worker entry (fall-through, no stub) | VERIFIED (bytes) |
| `D` (double) | in | caller parameter block, saved first (`STD I 73`) | VERIFIED (copy); layout inferred |
| local frame `B` | internal | `SAB 110` = 110-word working frame | VERIFIED (bytes) |
| `X` (manual) | in | address of file-name string (0 = read from terminal) | inferred (manual MAC example) |
| `A` (manual) | in/out | in: address of default file-type string; out: open file number | inferred (manual MAC example) |
| `T` (manual) | in | access code (0..9, sequential/random/read/write/append/contiguous) | inferred (manual) |
| `B+2` | out | returned status word (`STA ,B 2` at `123612`/`123614`) | VERIFIED (bytes) |
| error `104` | out | no-free / not-found error literal (`SAA 104` at `123555`) | VERIFIED (bytes); mapping inferred |

The user-visible `X`/`A`/`T` register convention lives in the caller-side
`MON 50` wrapper and the uncarved `MFELL`/`CALLPROC` frame, so the precise
user-register-to-field assignment is **inferred** from the manual, not
byte-proven here. The error literal `104` is VERIFIED in the code; its mapping to
the SINTRAN error-code table is **UNVERIFIED**.

---

## Pseudo-code (for an emulator)

See **[`50B-OpenFile.pseudo.c`](50B-OpenFile.pseudo.c)** - a pseudo-C model of the
handler for emulator authors. Control flow + the call to the FOPEN primitive are
byte-verified; the parameter-field semantics and error-number meanings are
inferred from the call structure and the manual.

Every instruction in the pseudo-code is translated against the canonical
[ND-100 instruction semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(RADD/COPY register ops, addressing-mode effective addresses, and skip/branch
senses).

---

## Honest caveats

**What is byte-proven:** `GOTAB[50B] = 000000` (level-14 dispatch, a fall-through
with no per-call vector - matches a live read of the running system); the `OPENF`
worker body at `123525B` in `006-S3FS` is real code (entry bytes
`021073 146145 146131 170110` match the disassembly); and it belongs to the
file-open family - it calls `FOPEN` (`067432B`, link cell `123633`), the file-open
primitive.

**What is NOT proven:** the link from the zero GOTAB slot to the `OPENF` worker.
Because the vector is zero there is no stub to disassemble and no pointer to
dereference; dispatch drops into the resident `MFELL`/`CALLPROC` second-level
path, which lives in an **uncarved overlay**. So the `MON 50 -> OPENF`
attribution rests on the `OPENF` symbol name + its call to `FOPEN` + the matching
open behaviour, not a followed pointer - hence **MISATTRIBUTED** in the strict
sense. Confirming the link needs a live trace: issue a real `MON 50`, single-step
the level-14 fall-through into the resident `CALLPROC` dispatch, and confirm P
lands on `OPENF = 123525`.

**Which open worker was chosen and why:** `OPENF = 123525B` was selected over the
sibling symbols `OPFIL = 103034B`, `DOPEN = 103026B`, and `FOPEN = 67432B`.
`OPENF` is the outermost open entry: it carries the canonical file-worker
prologue (`STD I 73` / `SAB` / `JPL I 70 -> 003752`, identical to the scratch and
file read/write worker entries) and it *calls* `FOPEN` (the low-level open
primitive) rather than *being* it. `DOPEN`/`OPFIL` are the DirectOpen and
open-file-family internals reached further down; they are not the top-level
`MON 50` OpenFile body.

Several link-cell contents (`003752`, `010500`, `010506`, `003776`) match no
`FILSYS-SYMBOLS` entry; their low addresses suggest resident-monitor /
save-restore routines outside the file-system segment and are not resolved here.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
