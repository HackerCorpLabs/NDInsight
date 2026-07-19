# MON 217B (octal) - GetAllFileIndexes (GUIOI)

Gets the directory index, the user index and the object index of an open file -
the three indexes that identify the file inside the SINTRAN III File System. The
file must be open (its file number comes from
[MON 50B OpenFile](../50B-OpenFile/README.md)). The indexes may be fetched for a
local file, or for a file on a remote computer connected through a COSMOS network.

**Status:** GOTAB dispatch head byte-proven as **fall-through** (`GOTAB[217B] =
000000`, no per-call stub); the GUIOI worker body is real SINTRAN L bytes and calls
the `FOPTB` (File Open TaBle) primitive; the exact `MON 217 -> worker` link crosses
an uncarved kernel bridge (see [Honest caveats](#honest-caveats)). All
addresses/values are **octal**.

- **Full disassembly:** [`217B-GetAllFileIndexes.ASM`](217B-GetAllFileIndexes.ASM) - the actual code (the GUIOI worker body; there is no entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 217B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[217B] = 000000<br/>(byte-proven: fall-through)"]
    C -.uncarved MFELL / CALLPROC.-> E["GUIOI file-index worker<br/>006-S3FS :105432B"]
    E --> F["FOPTB file open table<br/>006-S3FS :101043B"]
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
| GOTAB[217] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071452B` (1 word) | 58964 | `GOTAB+217` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| GUIOI file-index worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `105432B-105551B` (120w) | 48692 | `GUIOI` | real bytes; link **MISATTRIBUTED** |
| FOPTB file open table | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `101043B` (call target) | - | `FOPTB` | called by GUIOI (link cell `105542`) - **VERIFIED** |

**Verify by hand:** `grep '^105432 ' ../../segments-ref/006-S3FS/006-S3FS.hex` -> byte offset `48692`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=48692 count=8 | od -An -tx1` -> `22 44 cc 65 cc 59 f0 1f`
(= octal `021104 146145 146131 170037` = `STD I 104` / `RADD CLD SL DA` / `RADD CLD SB DD` / `SAB 37`, the GUIOI entry).

The GOTAB slot itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58964 count=2 | od -An -tx1` -> `00 00` (= `000000`, fall-through).

---

## Instruction walkthrough

Full listing: [`217B-GetAllFileIndexes.ASM`](217B-GetAllFileIndexes.ASM). The
functional body is the GUIOI worker (region B); there is no F16xx stub because
`GOTAB[217] = 0`. Calls to shared workers are **indirect** (`JPL I` / `JMP I`)
through a table of pointer words at the tail of the window (`105536-105551`);
nd100-dis renders those pointer words as bogus instructions - they are **data (link
cells)**, not code; their contents are the real worker addresses (resolved below).

**Entry prologue (`105432-105436`)** - `105432 STD I 104` stashes the caller's
double-word (the file number); `105435 SAB 37` builds the 37-word local frame `B`;
`105436 JPL I 101` -> `003752` is the shared resident prologue worker.

**File-number validate + table lookup (`105437-105454`)** - `105441 JPL I 100` ->
`010376` (resident worker) checks the file number; on a bad number `105446 SAA 132`
loads error `132` and exits to `105534`. `105453 JPL I 67` -> `101043` (**FOPTB**,
the file open table) resolves the open-file table entry; a miss takes `105454 JMP 57`
to the error-`133` exit at `105533`.

**Index-triple extraction (`105512-105527`)** - the body reads the table entry's
fields and packs them: `105512-105517` build the directory/user index word and store
it via `105517 STT ,B 1` (`B+1`); `105520-105527` extract the object index and store
it via `105527 STA ,B 0` (`B+0`). The two store points align with the manual's
returned `INDEX` (directory index in the left byte, user index in the right byte) and
`OBJIX` (object index).

**Finish (`105530-105551`)** - the store-status point `105534 STA ,B 2` writes the
result word into the caller's status slot `B+2`; every path funnels into the resident
return `105532 JMP I 17` -> `003776`.

The `JPL I 67` call to **FOPTB** (link cell `105542 = 101043`) is the byte-level
proof that GUIOI is the GetAllFileIndexes worker: it reads the open-file table to
recover the directory/user/object index triple, matching the manual behaviour, and
its short name `GUIOI` is the manual's symbol for `GetAllFileIndexes`.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `105432B` = GUIOI worker entry (fall-through, no stub) | VERIFIED (bytes) |
| `D` (double) | in | caller parameter, saved first (`STD I 104`) | VERIFIED (copy); layout inferred |
| local frame `B` | internal | `SAB 37` = 37-word working frame | VERIFIED (bytes) |
| `A` (manual) | in | file number (returned earlier from OpenFile) | inferred (manual MAC example) |
| `T` (manual) | out | `INDEX`: directory index (left byte) + user index (right byte) | inferred (manual MAC example) |
| `X` (manual) | out | object index | inferred (manual MAC example) |
| `D` (manual) | out | remote system identification (if bit 15 of `T` was set) | inferred (manual) |
| `B+0` | out | object index word (`STA ,B 0` at `105527`) | VERIFIED (bytes) |
| `B+1` | out | directory/user index word (`STT ,B 1` at `105517`) | VERIFIED (bytes) |
| `B+2` | out | returned status word (`STA ,B 2` at `105534`) | VERIFIED (bytes) |
| error `132` | out | bad-file-number literal (`SAA 132` at `105446`) | VERIFIED (bytes); mapping inferred |
| error `133` | out | file-not-open / not-found literal (`SAA 133` at `105533`) | VERIFIED (bytes); mapping inferred |

The user-visible `A`/`T`/`X` register convention lives in the caller-side `MON 217`
wrapper and the uncarved `MFELL`/`CALLPROC` frame, so the precise
user-register-to-field assignment is **inferred** from the manual
([`217B_GetAllFileIndexes.yaml`](../../../../../../../Developer/MON/calls/217B_GetAllFileIndexes.yaml)),
not byte-proven here. The mapping of error literals `132`/`133` to the SINTRAN
error-code table is **UNVERIFIED**.

---

## Pseudo-code (for an emulator)

See **[`217B-GetAllFileIndexes.pseudo.c`](217B-GetAllFileIndexes.pseudo.c)** - a
pseudo-C model of the handler for emulator authors. Control flow + the call to the
FOPTB primitive are byte-verified; the index-field semantics and error-number
meanings are inferred from the call structure and the manual.

Every instruction in the model is translated per the canonical
[`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(bare `LDA disp` = `mem[P+disp]`; `RADD CLD Sx Dy` = `y = x`; `BSKP` bit polarity;
`MIN ,B 4` success bump).

---

## Honest caveats

**What is byte-proven:** `GOTAB[217B] = 000000` (level-14 dispatch, a fall-through
with no per-call vector); the `GUIOI` worker body at `105432B` in `006-S3FS` is real
code (entry bytes `021104 146145 146131 170037` match the disassembly); and it
belongs to the file-index family - it calls `FOPTB` (`101043B`, link cell `105542`),
the file open table, and stores an index triple into `B+0`/`B+1`.

**What is NOT proven:** the link from the zero GOTAB slot to the `GUIOI` worker.
Because the vector is zero there is no stub to disassemble and no pointer to
dereference; dispatch drops into the resident `MFELL`/`CALLPROC` second-level path,
which lives in an **uncarved overlay**. So the `MON 217 -> GUIOI` attribution rests
on the `GUIOI` symbol name (`GetAllFileIndexes`) + its call to `FOPTB` + the matching
index-triple behaviour, not a followed pointer - hence **MISATTRIBUTED** in the
strict sense. Confirming the link needs a live trace: issue a real `MON 217`,
single-step the level-14 fall-through into the resident `CALLPROC` dispatch, and
confirm P lands on `GUIOI = 105432`.

**Region bound:** the GUIOI body is bounded strictly to the next symbol
`SFACC = 105552B` (120 words), and every direct branch lands inside that window.

Several link-cell contents (`003752`, `010376`, `020274`, `003776`) match no
`FILSYS-SYMBOLS` entry; their low addresses suggest resident-monitor / save-restore
routines outside the file-system segment and are not resolved here.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
