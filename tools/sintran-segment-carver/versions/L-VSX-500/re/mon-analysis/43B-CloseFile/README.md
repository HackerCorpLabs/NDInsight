# MON 43B (octal) - CloseFile (CLOSF)

> **CORRECTED 2026-07-15 (byte-verified).** The worker + dispatch described below are on the
> DEBUNKED model and are WRONG. Byte truth from the carved L07 image:
> `MCTAB[43B] = 005663B = CLOFI=103355B` in segment 006-S3FS, reached by the real dispatch
> `MON 43B -> ENT14(072167B) -> GOTAB[43B]=MFELL(072114B) -> CALLP(032201B) -> MCTAB[43B]=CLOFI`.
> Any "GOTAB from commoncode" / "uncarved CALLPROC bridge" / "F16xx stub" / old worker name below
> is an artefact of the wrong table. Verified: `dd if=044-S3IDPIT.bin bs=1 skip=1894 count=2`
> -> `86 ed`. Cross-ref ../317B-ExecuteCommand/README.md and SINTRAN/CARVING-HANDOFF.md sec 3a.

Closes one or more open files. A file must be opened (with
[MON 50B OpenFile](../50B-OpenFile/README.md)) before it is accessed and closed
afterwards. The single argument is the open file number; `-1` closes all files
not permanently open, `-2` closes all files including scratch and permanently
open files. CloseFile also resets peripheral files.

**Status:** GOTAB dispatch head byte-proven (`GOTAB[43B] = 121050B`, the `F1630`
level-14 stub in `025-S3IRPIT`); the CLOSF worker body is real SINTRAN L bytes
and calls the `FCLOS` file-close primitive; the exact `MON 43 -> worker` link
crosses an uncarved kernel bridge (see [Honest caveats](#honest-caveats)). All
addresses/values are **octal**.

- **Full disassembly:** [`43B-CloseFile.ASM`](43B-CloseFile.ASM) - the actual code, both regions (F1630 entry stub + CLOSF close worker).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 43B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[43B] = 121050B<br/>(byte-proven)"]
    C --> D["F1630 entry stub<br/>025-S3IRPIT :121050B"]
    D -.uncarved CALLPROC.-> E["CLOSF close worker<br/>006-S3FS :123741B"]
    E --> F["FCLOS file-close primitive<br/>006-S3FS :67612B"]
    class A blue
    class B,C,D teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The dashed hop (`D -> E`) is the resident `CALLPROC`/segment-switch - it is **not
present in any carved segment**, so it is the one link that cannot be followed
statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words x 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[43] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071276B` (1 word) | 58748 | `GOTAB+43` = `121050B` | **VERIFIED** |
| F1630 entry stub | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) - [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `121050B-121074B` (21w) | 56400 | `F1630` | **VERIFIED** |
| resident CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| CLOSF close worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `123741B-123777B` (37w) | 63426 | `CLOSF` | real bytes; link **MISATTRIBUTED** |
| FCLOS file-close primitive | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `67612B` (call target) | 34580 | `FCLOS` | called by CLOSF (link cell `123775`) - **VERIFIED** |

**Verify by hand:** `grep '^121050 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex` -> byte offset `56400`;
then `dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=56400 count=8 | od -An -tx1` -> `cd ed f5 ff b3 02 a8 03`
(= octal `146755 172777 131402 124003` = `RADD AD1 CM1 CLD SA DA` / `AAA -1` / `JAF 2` / `JMP 3`, the F1630 stub head).

The GOTAB slot itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58748 count=2 | od -An -tx1` -> `a2 28` (big-endian word = `121050B`).

The CLOSF worker entry:
`dd if=../../../segments/006-S3FS.bin bs=1 skip=63426 count=8 | od -An -tx1` -> `22 16 cc 65 cc 59 f0 06`
(= octal `021026 146145 146131 170006` = `STD I 26` / `RADD CLD SL DA` / `RADD CLD SB DD` / `SAB 6`, the CLOSF entry).

---

## Instruction walkthrough

Full listing: [`43B-CloseFile.ASM`](43B-CloseFile.ASM). The functional body is the
CLOSF worker (region B); the F1630 stub (region A) is the level-14 entry.

**Region A - F1630 stub (`121050-121074`)** is the compact level-14 entry pointed
at by `GOTAB[43]`. It tests/normalises the argument (`121050 RADD AD1 CM1 CLD SA
DA`, `121051 AAA -1`, `121052 JAF 2`) and loops over a small validation
(`121056-121074`). Two direct branches leave the stub - `121054 JMP 106 ->
121162` and `121070 JMP 72 -> 121162` - into shared level-14 tail code that lives
in sibling `F16xx` stubs (that address is inside `F1633`'s range), and
`121056 JPL I 123 -> 121201` calls a shared routine in `F1634`'s range. These are
the family's common level-14 handling, not the CLOSF file worker; the stub does
not itself reach the CLOSF body (that transfer is the uncarved `CALLPROC` hop).

**Region B - CLOSF worker (`123741-123777`)** is the file-close body. All calls to
shared workers are **indirect** (`JPL I` / `JMP I`) through a pointer table at the
tail of the window (`123767-123777`); nd100-dis renders those pointer words as
bogus instructions (`ROP NOOP`, `STZ I ...`, `SUB I ...`) - they are **data (link
cells)**, not code.

- **Entry prologue (`123741-123745`)** - `123741 STD I 26` stashes the caller's
  double-word (the file-number argument); `123744 SAB 6` builds the small local
  frame `B`; `123745 JPL I 23` -> `003752` is the shared resident prologue worker
  (identical to the scratch and file read/write worker entries).
- **Argument parse (`123746-123750`)** - `123747 JPL I 23` -> `044777` (`CLPAR`,
  parameter parse); a failure jumps to the store-status exit at `123763`.
- **Close + finish (`123751-123766`)** - `123752 JPL I 22` -> `010500` (resident
  worker) prepares, then `123754 JPL I 21` -> `067612` (**FCLOS**, the file-close
  primitive) performs the actual close. The two store-status points
  `123763`/`123765 STA ,B 2` write the result word into the caller's status slot
  `B+2`; every path funnels into the resident return `123762 JMP I` -> `003776`.

The `JPL I 21` call to **FCLOS** (link cell `123775 = 067612`) is the byte-level
proof that CLOSF is the CloseFile worker - it drives the file system's close
primitive, mirroring how OPENF drives FOPEN.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point (stub) | in | `121050B` = `F1630`, the `GOTAB[43]` level-14 stub | VERIFIED (bytes) |
| entry point (worker) | in | `123741B` = CLOSF worker entry | VERIFIED (bytes) |
| `D` (double) | in | caller parameter, saved first (`STD I 26`) | VERIFIED (copy); layout inferred |
| local frame `B` | internal | `SAB 6` = 6-word working frame | VERIFIED (bytes) |
| `T` (manual) | in | file number to close (`-1` = all non-permanent; `-2` = all incl. scratch/permanent) | inferred (manual MAC example) |
| `A` (manual) | out | error number on the error return | inferred (manual) |
| `B+2` | out | returned status word (`STA ,B 2` at `123763`/`123765`) | VERIFIED (bytes) |
| error `3` (stub) | out | argument-error literal (`SAA 3` at `121072`) | VERIFIED (bytes); mapping inferred |

The user-visible `T` register convention lives in the caller-side `MON 43`
wrapper and the uncarved `CALLPROC` frame, so the precise
user-register-to-field assignment is **inferred** from the manual, not
byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`43B-CloseFile.pseudo.c`](43B-CloseFile.pseudo.c)** - a pseudo-C model of the
handler for emulator authors. Control flow + the call to the FCLOS primitive are
byte-verified; the argument semantics and error-number meanings are inferred from
the call structure and the manual.

Every instruction in the pseudo-code is translated against the canonical
[ND-100 instruction semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(RADD/COPY register ops, addressing-mode effective addresses, and skip/branch
senses).

---

## Honest caveats

**What is byte-proven:** `GOTAB[43B] = 121050B` (level-14 dispatch); the `F1630`
stub at `121050B` in `025-S3IRPIT` is real code; the `CLOSF` worker body at
`123741B` in `006-S3FS` is real code (entry bytes `021026 146145 146131 170006`
match the disassembly); and it belongs to the file-close family - it calls
`FCLOS` (`067612B`, link cell `123775`), the file-close primitive.

**What is NOT proven:** the link from the `F1630` stub (in `025-S3IRPIT`) to the
`CLOSF` worker (in `006-S3FS`). The value `123741` occurs **zero** times as a
target inside the `F1630` stub; the stub's own direct branches stay in
`025-S3IRPIT` (to shared sibling-stub tail code at `121162`/`121201`), and the
stub->worker transfer is the resident `CALLPROC`/segment switch in an **uncarved
overlay**. So the `MON 43 -> CLOSF` attribution rests on the `CLOSF` symbol name
+ its call to `FCLOS` + the matching close behaviour, not a followed pointer -
hence **MISATTRIBUTED** in the strict sense. Confirming it needs a live trace:
break at `121050B` on a real `MON 43`, single-step the segment switch, and
confirm P lands on `CLOSF = 123741`.

**Region-A bound:** the `F1630` stub is bounded strictly to the next symbol
`F1631 = 121075B` (21 words). Its two `JMP -> 121162` branches and the
`JPL I 123 -> 121201` call land in neighbouring `F16xx` stubs' shared level-14
tail code; those are deliberately **not** included here (they are a different
call's body) - the branches are noted, not swallowed.

Several link-cell contents (`003752`, `010500`, `010506`, `003776`) match no
`FILSYS-SYMBOLS` entry; their low addresses suggest resident-monitor /
save-restore routines outside the file-system segment and are not resolved here.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
