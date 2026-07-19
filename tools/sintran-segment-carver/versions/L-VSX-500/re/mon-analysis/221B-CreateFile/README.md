# MON 221B (octal) - CreateFile (CRFIL)

> **CORRECTED 2026-07-15 (byte-verified).** The worker + dispatch described below are on the
> DEBUNKED model and are WRONG. Byte truth from the carved L07 image:
> `MCTAB[221B] = 006041B = CRALF=105562B` in segment 006-S3FS, reached by the real dispatch
> `MON 221B -> ENT14(072167B) -> GOTAB[221B]=MFELL(072114B) -> CALLP(032201B) -> MCTAB[221B]=CRALF`.
> Any "GOTAB from commoncode" / "uncarved CALLPROC bridge" / "F16xx stub" / old worker name below
> is an artefact of the wrong table. Verified: `dd if=044-S3IDPIT.bin bs=1 skip=2114 count=2`
> -> `8b 72`. Cross-ref ../317B-ExecuteCommand/README.md and SINTRAN/CARVING-HANDOFF.md sec 3a.

Creates a file in a user's directory. The file may be indexed (default, expands
automatically when written to), contiguous, or allocated. CRFIL shares one code
body with three create variants (MCRFI, MCRNW, CRNVE) that enter at adjacent
words and preset an SSK/SSM skip-flag pair to select the variant.

**Status:** GOTAB dispatch head byte-proven as **fall-through** (`GOTAB[221B] = 000000`,
no per-call stub); the CRFIL worker body is real SINTRAN L bytes; the exact
`MON 221 -> worker` link crosses an uncarved kernel bridge (see
[Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`221B-CreateFile.ASM`](221B-CreateFile.ASM) - the actual code (the CRFIL worker body; there is no entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 221B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[221B] = 000000<br/>(byte-proven: fall-through)"]
    C -.uncarved MFELL / CALLPROC.-> E["CRFIL create worker<br/>006-S3FS :115425B"]
    E --> F["directory alloc + write<br/>via JPL I workers"]
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
| GOTAB[221] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071454B` (1 word) | 58968 | `GOTAB+221` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| CRFIL create worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `115425B-116124B` (320w) | 56874 | `CRFIL` | real bytes; link **MISATTRIBUTED** |

**Verify by hand:** `grep '^115425 ' ../../segments-ref/006-S3FS/006-S3FS.hex` -> byte offset `56874`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=56874 count=8 | od -An -tx1` -> `f8 10 f8 38 a8 09 f8 10`
(= octal `174020 174070 124011 174020` = `BSET ZRO SSK` / `BSET ZRO SSM` / `JMP 11` ..., the CRFIL 4-entry preamble).

The GOTAB slot itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58968 count=2 | od -An -tx1` -> `00 00` (= `000000`, fall-through).

---

## Instruction walkthrough

Full listing: [`221B-CreateFile.ASM`](221B-CreateFile.ASM). The functional body
is the CRFIL worker; there is no F16xx stub because `GOTAB[221] = 0`. Calls to
shared file-system workers are **indirect** (`JPL I` / `JMP I`) through small
tables of pointer words (link cells) at the tails of the window
(`115523-115534`, `115657-115676`, `116034-116043`, `116122-116124`). nd100-dis
renders those pointer words as bogus instructions (`FDV`, `ROP NOOP`, `STF`,
`LDT`, ...) - they are **data (link cells)**, not code.

**Four-entry preamble (`115425-115437`)** - four one-family entry points each set
the `SSK`/`SSM` skip-flag pair and jump to the joined body at `115440`:
`115425 CRFIL` (SSK=0,SSM=0), `115430 MCRFI` (SSK=0,SSM=1), `115433 MCRNW`
(SSK=1,SSM=1), `115436 CRNVE` (SSK=1,SSM=0). MON 221 enters at `CRFIL`.

**Prologue + variant code (`115440-115462`)** - `115440 STD I 62` stashes the
caller's D; `115443 SAB 133` builds the local frame `B`; `115444 JPL I 57` ->
`003752` is a prologue worker. `115445-115461` fold `SSM` then `SSK` into a
variant/mode value (0..3) stored at `B+117` (`115462 STA ,B 117`).

**Name parse + directory locate (`115463-115560`)** - the file-name string is
parsed and the owning directory located/opened via indirect workers
(`115473/115476/115510/115516 JPL I`). A directory-object record is built at `X`
(`115544 LDX I 115`) and fields `X+24..X+27` are filled from frame slots.

**File-type decision (`115561-115626`)** - `115561 BSKP ONE 0 DD` tests whether a
StartAddress was supplied: `B+120 = 1` (contiguous/allocated) or `0` (indexed).
`115574-115626` classify the request (`SAT 3` / `SAT 1` type tests) and read the
directory object's page/entry descriptors (`LDD ,X 0`) into `B+110/112/114/115`.

**Version + allocation (`115627-116013`)** - `115627-115656` handle same-name
version collisions, computing the new version's page range into `B+125/B+126`.
The allocation loop (`115677-116013`) walks the directory page structure
allocating the requested pages (`JPL I 116/62/44` -> allocation/write-back
workers), tracking the running page cursor in `B+125/126/127` against limits in
`B+131/132`; `115777-116012` write the start/page words back into the directory
object (`X+60/X+64/X+44`) and flush.

**Status + exit (`116044-116124`)** - error paths load a code with `SAA` (e.g.
`67`) and funnel to `116044 STA ,B 2` (caller status slot); the exit link cell
`116043 = 003776` is the resident return. `ALFIL` (AllocateFile) begins at
`116125`, bounding the body.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `115425B` = CRFIL (create); `115430/115433/115436` = MCRFI/MCRNW/CRNVE variants, shared body, `SSK`/`SSM` split | VERIFIED (bytes) |
| `SSK`,`SSM` | internal | variant selector pair; preset at each entry, folded to a 0..3 mode at `115445-115461`, stored `B+117` | VERIFIED (bytes) |
| `B+117` | internal | variant/mode value | VERIFIED (bytes); meaning inferred |
| `B+120` | internal | contiguous(1)/indexed(0) flag from StartAddress test (`115561`) | VERIFIED (bytes); label inferred |
| `X` (record) | in/out | directory-object record; fields `X+24..X+27`, `X+44`, `X+60`, `X+64` | VERIFIED (accesses); field meaning inferred |
| `B+2` | out | returned status word (`STA ,B 2` at `116044`) | VERIFIED (bytes) |
| user `X` | in | address of file-name string (manual MAC example) | inferred (manual) |
| user `AD` (double) | in | StartAddress; 0 = system-placed contiguous/indexed | inferred (manual) |
| user `T` | in | address of double word = NoOfPages; 0 = indexed | inferred (manual) |
| skip / error | out | normal return skips; error return has error number in A | inferred (manual) |

Error numbers observed as literals (`SAA 67`, ...) are VERIFIED in the code; their
mapping to the SINTRAN error-code table is **UNVERIFIED**. The user-visible A/T/X
register convention lives in the caller-side `MON 221` wrapper and the uncarved
`MFELL`/`CALLPROC` frame, so the precise user-register-to-field assignment is
**inferred** from the manual (file name, StartAddress, NoOfPages), not byte-proven
here.

---

## Pseudo-code (for an emulator)

See **[`221B-CreateFile.pseudo.c`](221B-CreateFile.pseudo.c)** - a pseudo-C model
of the handler for emulator authors. Control flow + the SSK/SSM entry split are
byte-verified; the file-system worker semantics, directory-field meanings, and
error-number meanings are inferred from the call structure. Every instruction is
translated per the canonical [`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md).

---

## Honest caveats

**What is byte-proven:** `GOTAB[221B] = 000000` (level-14 dispatch, a fall-through
with no per-call vector - matches a live read of the running system); the `CRFIL`
worker body at `115425B` in `006-S3FS` is real code (entry bytes match the
disassembly); and it is one of four alternate entry points (`CRFIL`/`MCRFI`/
`MCRNW`/`CRNVE`) sharing one body via the `SSK`/`SSM` split, bounded above by the
next FILSYS routine `ALFIL` at `116125B`.

**What is NOT proven:** the link from the zero GOTAB slot to the `CRFIL` worker.
Because the vector is zero there is no stub to disassemble and no pointer to
dereference; dispatch drops into the resident `MFELL`/`CALLPROC` second-level
path, which lives in an **uncarved overlay**. So the `MON 221 -> CRFIL`
attribution rests on the `CRFIL` symbol name + the matching create behaviour, not
a followed pointer - hence **MISATTRIBUTED** in the strict sense. (NC's
disassembler labels both 221B and 50B as "CRALF"; the authoritative table has
221B = CreateFile with mnemonic CRALF, and 50B = OpenFile, so CRFIL/CreateFile is
the correct attribution for 221B.) Confirming the link needs a live trace: issue a
real `MON 221`, single-step the level-14 fall-through into the resident `CALLPROC`
dispatch, and confirm P lands on `CRFIL = 115425`.

Several link-cell contents in this body (`003752`, and cells pointing at low or
in-body addresses) match no `FILSYS-SYMBOLS` entry; their addresses suggest
resident-monitor / save-restore routines and local error fragments outside the
resolvable symbol set, and are not resolved here.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
