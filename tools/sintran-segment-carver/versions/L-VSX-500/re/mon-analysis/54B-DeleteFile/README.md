# MON 54B (octal) - DeleteFile (MDLFI)

Deletes a file and releases its pages. A version number in the file name deletes
that one version; otherwise all versions are deleted. MDLFI is the delete entry of
a shared directory-entry dispatcher: three siblings (STEFI, SPEFI, MRNFI) enter
through 3-word stubs just above it and join the common body, each presetting an
SSK/SSM skip-flag pair that selects the operation performed.

**Status:** GOTAB dispatch head byte-proven as **fall-through** (`GOTAB[54B] = 000000`,
no per-call stub); the MDLFI worker body is real SINTRAN L bytes; the exact
`MON 54 -> worker` link crosses an uncarved kernel bridge (see
[Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`54B-DeleteFile.ASM`](54B-DeleteFile.ASM) - the actual code (the MDLFI worker body; there is no entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 54B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[54B] = 000000<br/>(byte-proven: fall-through)"]
    C -.uncarved MFELL / CALLPROC.-> E["MDLFI delete worker<br/>006-S3FS :106063B"]
    E --> F["find entry + release pages<br/>via MODLF worker"]
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
| GOTAB[54] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071307B` (1 word) | 58766 | `GOTAB+54` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| MDLFI delete worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `106063B-106211B` (87w) | 49254 | `MDLFI` | real bytes; link **MISATTRIBUTED** |

**Verify by hand:** `grep '^106063 ' ../../segments-ref/006-S3FS/006-S3FS.hex` -> byte offset `49254`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=49254 count=8 | od -An -tx1` -> `f8 38 f8 10 22 49 cc 65`
(= octal `174070 174020 021111 146145` = `BSET ZRO SSM` / `BSET ZRO SSK` / `STD I 111` ..., the MDLFI entry).

The GOTAB slot itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58766 count=2 | od -An -tx1` -> `00 00` (= `000000`, fall-through).

---

## Instruction walkthrough

Full listing: [`54B-DeleteFile.ASM`](54B-DeleteFile.ASM). The functional body is
the MDLFI worker; there is no F16xx stub because `GOTAB[54] = 0`. Calls to shared
file-system workers are **indirect** (`JPL I` / `JMP I`) through a table of
pointer words (link cells) at the tail of the window (`106177-106211`). nd100-dis
renders those pointer words as bogus instructions (`STZ`, `MPY`, `FDV`, ...) -
they are **data (link cells)**, not code; their contents are the real worker
addresses (resolved below).

**Entry + mode derivation (`106063-106107`)** - `106063 MDLFI` presets `SSM=0`,
`SSK=0`; the three siblings enter above (`106052 STEFI` SSM=1/SSK=1, `106055
SPEFI` SSM=1/SSK=0, `106060 MRNFI` SSM=0/SSK=1) and join at `106065`. `106065 STD
I 111` stashes the caller's D; `106070 SAB 125` builds the frame `B`; `106071 JPL
I 106` -> `003752` is the prologue worker. `106072-106106` fold `SSM` then `SSK`
into an operation mode value (0..3) stored at `B+123` (`106107 STA ,B 123`); for
DeleteFile the mode is `0`.

**Directory lookup (`106110-106115`)** - `106114 JPL I 66` -> `031075` locates
the file's directory entry (`X = B+0`); a failure returns via `106115 JMP 57` ->
`106174` (store status + return).

**Mode dispatch (`106116-106170`)** - a ladder of `SAT n` / `SKP IF DA EQL ST`
tests on `B+123` selects the worker for the operation; on a match it calls that
worker through a link cell, and each call is followed by `JMP -> 106174` (error)
or `JMP -> 106171` (done):
| Mode | Worker (link cell) | Symbol | Operation (inferred) |
|------|--------------------|--------|----------------------|
| 0 | `106204 = 120067` | `MODLF` | delete / release pages (DeleteFile) |
| 1 | `106205 = 117352` | `MRENF` | rename |
| 2 | `106206 = 120470` | `MSPER` | set permanent |
| 3 | `106207 = 120236` | `MSTMP` | set temporary |
| tail | `106210 = 120233` | `MSTRM` | (variant) |

**Exit (`106171-106211`)** - `106171 MIN ,B 4` bumps the status, `106172 SAA -125`,
`106173 JMP I 16` -> `106211` (= `003776`, the resident return cell). The error
path `106174 STA ,B 2` stores the status into the caller's slot then falls to
`106172`. `106176-106211` are the link cells (`003752`, `031075`, `120067` MODLF,
`117352` MRENF, `120470` MSPER, `120236` MSTMP, `120233` MSTRM, `003776`).

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `106063B` = MDLFI (delete, SSM=0/SSK=0); siblings `106052/106055/106060` = STEFI/SPEFI/MRNFI, shared body, `SSK`/`SSM` split | VERIFIED (bytes) |
| `SSK`,`SSM` | internal | operation selector pair; preset at each entry, folded to a 0..3 mode at `106072-106106` | VERIFIED (bytes) |
| `B+123` | internal | operation mode (0 = delete) | VERIFIED (bytes); labels inferred |
| `B+0` (`X`) | in | directory-entry context passed to the lookup worker (`106113 LDX ,B 0`) | VERIFIED (bytes); meaning inferred |
| `B+2` | out | returned status word (`STA ,B 2` at `106174`) | VERIFIED (bytes) |
| user `X` | in | address of file-name string (manual MAC example) | inferred (manual) |
| skip / error | out | normal return skips; error return has error number in A | inferred (manual) |

The mode-to-worker mapping (`MODLF`/`MRENF`/`MSPER`/`MSTMP`/`MSTRM`) is byte-proven
at the link-cell level (the cell contents equal those FILSYS symbol addresses);
the *meaning* assigned to each (delete/rename/set-permanent/set-temporary) is
**inferred** from the symbol names. The user-visible register convention lives in
the caller-side `MON 54` wrapper and the uncarved `MFELL`/`CALLPROC` frame, so the
`X = file-name address` assignment is **inferred** from the manual, not byte-proven
here.

---

## Pseudo-code (for an emulator)

See **[`54B-DeleteFile.pseudo.c`](54B-DeleteFile.pseudo.c)** - a pseudo-C model of
the handler for emulator authors. Control flow + the SSK/SSM -> mode split are
byte-verified; the worker semantics and error-number meanings are inferred from
the FILSYS symbol table and the call structure. Every instruction is translated
per the canonical [`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md).

---

## Honest caveats

**What is byte-proven:** `GOTAB[54B] = 000000` (level-14 dispatch, a fall-through
with no per-call vector - matches a live read of the running system); the `MDLFI`
worker body at `106063B` in `006-S3FS` is real code (entry bytes match the
disassembly); MDLFI presets `SSM=0`/`SSK=0` and shares its body with the three
sibling entries above it; and the mode-dispatch link cells at `106204-106210`
contain the FILSYS worker addresses `MODLF`/`MRENF`/`MSPER`/`MSTMP`/`MSTRM`.

**What is NOT proven:** the link from the zero GOTAB slot to the `MDLFI` worker.
Because the vector is zero there is no stub to disassemble and no pointer to
dereference; dispatch drops into the resident `MFELL`/`CALLPROC` second-level
path, which lives in an **uncarved overlay**. So the `MON 54 -> MDLFI`
attribution rests on the `MDLFI` symbol name + the matching delete behaviour, not
a followed pointer - hence **MISATTRIBUTED** in the strict sense. Confirming the
link needs a live trace: issue a real `MON 54`, single-step the level-14
fall-through into the resident `CALLPROC` dispatch, and confirm P lands on
`MDLFI = 106063`.

The prologue link cell `003752` and the lookup cell `031075` match no
`FILSYS-SYMBOLS` entry; their addresses suggest resident-monitor / directory-scan
routines outside the resolvable symbol set, and are not resolved here.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
