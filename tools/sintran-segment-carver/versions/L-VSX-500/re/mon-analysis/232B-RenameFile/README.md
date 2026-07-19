# MON 232B (octal) - RenameFile (MRNFI)

Renames a file. The caller passes the old file name and a new file name; only the
file type may be changed (e.g. `:SYMB`) - the directory, user, and version parts
are not used. `MRNFI` is a sibling entry of the shared directory-entry dispatcher
(the same `MDLFI` body used by MON 54B DeleteFile); it presets an SSK/SSM
skip-flag pair selecting the rename operation.

**Status:** GOTAB dispatch head byte-proven (`GOTAB[232B] = 066172B`, the `F1723`
level-14 stub in `025-S3IRPIT`); the `MRNFI` worker body is real SINTRAN L bytes
and dispatches (mode 1) to the `MRENF` rename worker; the exact `MON 232 -> worker`
link crosses an uncarved kernel bridge (see [Honest caveats](#honest-caveats)). All
addresses/values are **octal**.

- **Full disassembly:** [`232B-RenameFile.ASM`](232B-RenameFile.ASM) - the actual code (F1723 entry stub + MRNFI entry + shared MDLFI dispatcher body).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 232B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[232B] = 066172B<br/>(byte-proven)"]
    C --> D["F1723 entry stub<br/>025-S3IRPIT :66172B"]
    D -.uncarved CALLPROC.-> E["MRNFI rename entry<br/>006-S3FS :106060B"]
    E --> F["mode 1 -> MRENF worker<br/>006-S3FS :117352B"]
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

Every row is a real region you can open. The `025-S3IRPIT` carve has an unmapped
hole before the stub, so its byte offset is the authoritative decimal offset from
the segment `.hex`, not the plain `(addr - loadbase) x 2`.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[232] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071465B` (1 word) | 58986 | `GOTAB+232` = `066172B` | **VERIFIED** |
| F1723 entry stub | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) - [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `66172B-66175B` (4w) | 28916 | `F1723` | **VERIFIED** |
| resident CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| MRNFI rename entry + MDLFI body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `106060B-106211B` (code to `106175B`) | 49248 | `MRNFI` | real bytes; link **MISATTRIBUTED** |
| MRENF mode-1 worker | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `117352B` (link cell `106205`) | - | `MRENF` | link-cell **VERIFIED**; meaning inferred |

**Verify by hand:** `grep '^106060 ' ../../segments-ref/006-S3FS/006-S3FS.hex` -> byte offset `49248`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=49248 count=8 | od -An -tx1` -> `f8 38 f8 90 a8 03 f8 38`
(the stored words = octal `174070 174220 124003 174070` = `BSET ZRO SSM` / `BSET ONE SSK` / `JMP 3` / `BSET ZRO SSM`, the MRNFI entry + next sibling).

The GOTAB slot itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58986 count=2 | od -An -tx1` -> `6c 7a` (the stored word = `066172B`).

The F1723 stub: `grep '^66172 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex` -> byte offset `28916`;
then `dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=28916 count=2 | od -An -tx1` -> `08 10` (= `004020B`, the F1723 entry word).

---

## Instruction walkthrough

Full listing: [`232B-RenameFile.ASM`](232B-RenameFile.ASM). Three regions:

**Region A - F1723 entry stub (`66172-66175`, `025-S3IRPIT`)** is the 4-word
level-14 entry vectored from `GOTAB[232]`, bounded strictly to the next symbol
`F1724 = 66176B`. Its words (`STA 20`, `STZ -104`, `STA -130`, `STA -132`) are an
entry point into a shared `025-S3IRPIT` level-14 handler (a device/segment table
min/max scan that ends in `EXIT` at `66261B`); the stub does **not** itself branch
to the `MRNFI` worker - that transfer is the uncarved resident `CALLPROC` hop.

**Region B - MRNFI rename entry (`106060-106064`, `006-S3FS`)** presets the
`SSK`/`SSM` pair (`106060 BSET ZRO SSM`, `106061 BSET ONE SSK` -> SSM=0/SSK=1) and
jumps to the shared body at `106065`. The two words below (`106063/106064`) are the
`MDLFI` sibling entry (SSM=0/SSK=0).

**Region C - shared MDLFI dispatcher body (`106065-106175`; link cells
`106176-106211`)** is the functional body (the same one documented for MON 54B
DeleteFile). It stashes the caller's D (`106065 STD I 111`), builds the frame `B`
(`106070 SAB 125`), runs the prologue worker (`106071 JPL I 106` -> `003752`), then
folds `SSM`/`SSK` into a 0..3 mode: for MRNFI the fold path is
`106072 BSKP ONE SSM` (SSM=0, no skip) -> `106102 BSKP ONE SSK` (SSK=1, skip) ->
`106104 SAA 1`, i.e. **mode 1**, stored at `B+123` (`106107 STA ,B 123`).

- **Directory lookup (`106110-106115`)** - `106114 JPL I 66` -> `031075` locates
  the file's directory entry; failure returns via `106115 JMP 57` -> `106174`.
- **Mode ladder (`106116-106170`)** - `SAT n` / `SKP IF DA EQL ST` tests select the
  worker through a link cell. Mode 1 matches at `106137-106147`:
  `106145 JPL I 40` -> `106205` = `MRENF` (`117352`, the rename worker). The mode-1
  pre-branch (`106116-106127`) first re-locates the entry via
  `106126 JPL I 54` -> `031075`.
- **Exit (`106171-106211`)** - `106171 MIN ,B 4` bumps status; `106173 JMP I 16` ->
  `106211` (= `003776`, resident return). Error path `106174 STA ,B 2` stores the
  status. `106176-106211` are the link cells (`003752`, `031075`, `120067` MODLF,
  `117352` MRENF, `120470` MSPER, `120236` MSTMP, `120233` MSTRM, `003776`).

The mode-1 call to `MRENF` is the byte-level reason `MRNFI` is the RenameFile
worker - it renames the located directory entry exactly as the manual describes.

---

## Parameter / register contract

Contract from [`Developer/MON/calls/232B_RenameFile.yaml`](../../../../../../../Developer/MON/calls/232B_RenameFile.yaml).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point (stub) | in | `66172B` = `F1723`, the `GOTAB[232]` level-14 stub | VERIFIED (bytes) |
| entry point (worker) | in | `106060B` = MRNFI (rename, SSM=0/SSK=1) | VERIFIED (bytes) |
| `SSK`,`SSM` | internal | operation selector pair; preset at `106060/106061`, folded to mode 1 at `106072-106106` | VERIFIED (bytes) |
| `B+123` | internal | operation mode (1 = rename) | VERIFIED (bytes); label inferred |
| `B+0` (`X`) | in | directory-entry context passed to the lookup worker (`106113 LDX ,B 0`) | VERIFIED (bytes); meaning inferred |
| `B+2` | out | returned status word (`STA ,B 2` at `106174`) | VERIFIED (bytes) |
| OldFileName (user `X`) | in | address of old file-name string | inferred (manual/yaml) |
| NewFileName (user `A`) | in | address of new file-name string (file type only may change) | inferred (manual/yaml) |
| skip / error | out | normal return skips; error return has error number in A | inferred (manual/yaml) |

The mode-1-to-`MRENF` mapping is byte-proven at the link-cell level (`106205` =
`117352` = `MRENF`); the *meaning* (rename) is **inferred** from the symbol name and
the manual. The user-visible `X`/`A` convention lives in the caller-side `MON 232`
wrapper and the uncarved `CALLPROC` frame, so it is **inferred** from the manual,
not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`232B-RenameFile.pseudo.c`](232B-RenameFile.pseudo.c)** - a pseudo-C model of
the handler for emulator authors. Control flow + the SSK/SSM -> mode 1 fold + the
`MRENF` call are byte-verified; the worker semantics and error-number meanings are
inferred from the FILSYS symbol table and the call structure. Every instruction is
translated per the canonical [`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(note the `BSET ZRO`/`BSET ONE` flag presets, the `BSKP ONE` skip-flag tests, and
the `RADD CLD` COPY idiom).

---

## Honest caveats

**What is byte-proven:** `GOTAB[232B] = 066172B` (level-14 dispatch); the `F1723`
stub at `66172B` in `025-S3IRPIT` is real code (4 words into a shared handler that
ends in `EXIT` at `66261B`); the `MRNFI` entry at `106060B` in `006-S3FS` is real
code (entry bytes `174070 174220 124003` match the disassembly); MRNFI presets
`SSM=0`/`SSK=1`, shares the `MDLFI` dispatcher body, folds to mode 1, and the mode-1
link cell `106205` contains the FILSYS worker address `MRENF = 117352`.

**What is NOT proven:** the link from the `F1723` stub (in `025-S3IRPIT`) to the
`MRNFI` worker (in `006-S3FS`). The value `106060` occurs nowhere the stub
dereferences; the stub is part of a shared handler that returns via `EXIT`, and the
stub->worker transfer is the resident `CALLPROC`/segment switch in an **uncarved
overlay**. So the `MON 232 -> MRNFI` attribution rests on the `MRNFI` symbol name +
its mode-1 `MRENF` (rename) call + the matching behaviour, not a followed pointer -
hence **MISATTRIBUTED** in the strict sense. Confirming it needs a live trace: break
at `66172B` on a real `MON 232`, single-step the segment switch, and confirm P lands
on `MRNFI = 106060`.

The prologue link cell `003752`, the lookup cell `031075`, and the return cell
`003776` match no `FILSYS-SYMBOLS` entry; their addresses suggest resident-monitor /
directory-scan routines outside the resolvable symbol set, and are not resolved here.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
