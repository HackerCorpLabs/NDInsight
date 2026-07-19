# MON 231B (octal) - ExpandFile (EXPFI)

Expands the size of a file - increases the number of pages of contiguous and
allocated files (indexed files created with 0 pages may also be expanded). EXPFI
shares one code body with three sibling directory-allocation operations (SFACC,
CRALN, CRALF) that enter at adjacent words and preset an SSK/SSM skip-flag pair to
select the operation performed.

**Status:** GOTAB dispatch head byte-proven as **fall-through** (`GOTAB[231B] = 000000`,
no per-call stub); the EXPFI worker body is real SINTRAN L bytes and dispatches
(mode 2) to the `MEXFI` expand-file worker; the exact `MON 231 -> worker` link
crosses an uncarved kernel bridge (see [Honest caveats](#honest-caveats)). All
addresses/values are **octal**.

- **Full disassembly:** [`231B-ExpandFile.ASM`](231B-ExpandFile.ASM) - the actual code (the EXPFI entry + shared create/allocate dispatcher body; there is no entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 231B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[231B] = 000000<br/>(byte-proven: fall-through)"]
    C -.uncarved MFELL / CALLPROC.-> E["EXPFI expand-file entry<br/>006-S3FS :105555B"]
    E --> F["mode 2 -> MEXFI worker<br/>006-S3FS :116623B"]
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
| GOTAB[231] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071464B` (1 word) | 58984 | `GOTAB+231` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| EXPFI entry + dispatcher body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `105555B-106042B` (body to `106036B`) | 48858 | `EXPFI` | real bytes; link **MISATTRIBUTED** |
| MEXFI mode-2 worker | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `116623B` (link cell `106032`) | - | `MEXFI` | link-cell **VERIFIED**; meaning inferred |

**Verify by hand:** `grep '^105555 ' ../../segments-ref/006-S3FS/006-S3FS.hex` -> byte offset `48858`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=48858 count=8 | od -An -tx1` -> `f8 b8 f8 10 a8 05 f8 90`
(the stored words = octal `174270 174020 124005 174220` = `BSET ONE SSM` / `BSET ZRO SSK` / `JMP 5` / `BSET ONE SSK`, the EXPFI entry + next sibling).

The GOTAB slot itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58984 count=2 | od -An -tx1` -> `00 00` (= `000000`, fall-through).

---

## Instruction walkthrough

Full listing: [`231B-ExpandFile.ASM`](231B-ExpandFile.ASM). The functional body is
the EXPFI dispatcher; there is no F16xx/F17xx stub because `GOTAB[231] = 0`. Calls
to shared file-system workers are **indirect** (`JPL I` / `JMP I`) through pointer
tables (link cells) at `105664-105677` and `106021-106042`. nd100-dis renders those
pointer words as bogus instructions (`STZ`, `MPY`, `FDV`, `STF`, ...) - they are
**data (link cells)**, not code; their contents are the real worker addresses
(resolved in the `.ASM`).

**Sibling entries + mode fold (`105552-105574`)** - four entries preset the
`SSK`/`SSM` pair and join at `105564`: `105552 SFACC` (SSM=1/SSK=1, mode 3),
`105555 EXPFI` (SSM=1/SSK=0, mode 2), `105560 CRALN` (SSM=0/SSK=1, mode 1),
`105562 CRALF` (SSM=0/SSK=0, mode 0). The body stashes the caller's D
(`105564 STD I 77`), builds the frame `B` (`105567 SAB 145`), runs the prologue
worker (`105570 JPL I 74` -> `003752`), then folds `SSM`/`SSK` into a 0..3 mode:
`105571 RADD CLD 0 DA` (`A = 0`), `105572 SHA LIN 2` (shift left 2, M-bit fill),
`105573 BSET BAC 0 DA` (`A` bit0 = K), stored at `B+123` (`105574 STA ,B 123`).

**Parameter unpack + user context (`105575-105720`)** - caller words are copied
into the frame (`105575-105621`); the user/context is established via
`USCPS` (`031075`) and `USCPB` (`031067`), and the directory object is read. Each
`JPL I` is followed by `JMP -> 106010` (error, store status).

**Second-phase mode dispatch (`105721-105776`)** - a ladder of `SAT n` /
`SKP IF DA EQL ST` tests on `B+123` selects the worker through a link cell:
| Mode | Worker (link cell) | Symbol | Operation (inferred) |
|------|--------------------|--------|----------------------|
| 3 | `106031 = 120752` | `MSFLA` | set file allocation |
| 2 | `106032 = 116623` | `MEXFI` | **expand file (ExpandFile)** |
| 1 | `106033/106034 = 115430/115433` | `MCRFI` / `MCRNW` | create / create-new |
| 0 | `106035/106036 = 116130/116133` | `MALFI` / `MALNE` | allocate / allocate-new |

For ExpandFile the mode is `2`, so `105743 JPL I 67` -> `106032` = `MEXFI` runs.

**Exit (`105777-106042`)** - `105777 MIN ,B 4` bumps the status; `106000-106005`
write the returned handles back through link cells; `106006 SAA -145`,
`106007 JMP I 33` -> `106042` (= `003776`, resident return). The error path
`106010 STA ,B 2` stores the status into the caller's slot then loops to `106000`.
`106021-106042` are the link cells.

---

## Parameter / register contract

Contract from [`Developer/MON/calls/231B_ExpandFile.yaml`](../../../../../../../Developer/MON/calls/231B_ExpandFile.yaml).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `105555B` = EXPFI (expand, SSM=1/SSK=0); siblings `105552/105560/105562` = SFACC/CRALN/CRALF, shared body, `SSK`/`SSM` split | VERIFIED (bytes) |
| `SSK`,`SSM` | internal | operation selector pair; preset at each entry, folded to a 0..3 mode at `105571-105574` | VERIFIED (bytes) |
| `B+123` | internal | operation mode (2 = expand) | VERIFIED (bytes); labels inferred |
| `B+143/144`, `B+135/136/137` | internal | page/entry descriptors passed to the workers | VERIFIED (accesses); meaning inferred |
| `B+140/141/142` | out | returned handles written back at exit (`STA I 36/35/34`) | VERIFIED (bytes); meaning inferred |
| `B+2` | out | returned status word (`STA ,B 2` at `106010`) | VERIFIED (bytes) |
| FileName (user `X`) | in | address of file-name string (may be abbreviated) | inferred (manual/yaml) |
| NoOfPages (user `T`) | in | address of double word = additional pages | inferred (manual/yaml) |
| skip / error | out | normal return skips; error return has error number in A | inferred (manual/yaml) |

The mode-to-worker mapping (`MSFLA`/`MEXFI`/`MCRFI`/`MCRNW`/`MALFI`/`MALNE`) is
byte-proven at the link-cell level (the cell contents equal those FILSYS symbol
addresses); the *meaning* assigned to each is **inferred** from the symbol names.
The user-visible `X`/`T` convention lives in the caller-side `MON 231` wrapper and
the uncarved `MFELL`/`CALLPROC` frame, so the `X = file-name` / `T = NoOfPages`
assignment is **inferred** from the manual, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`231B-ExpandFile.pseudo.c`](231B-ExpandFile.pseudo.c)** - a pseudo-C model
of the handler for emulator authors. Control flow + the SSK/SSM -> mode fold are
byte-verified; the worker semantics and error-number meanings are inferred from the
FILSYS symbol table and the call structure. Every instruction is translated per the
canonical [`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(note the `RADD CLD` COPY idiom, the `SHA LIN` M-bit fill, and `BSET BAC` K-source
used in the mode fold).

---

## Honest caveats

**What is byte-proven:** `GOTAB[231B] = 000000` (level-14 dispatch, a fall-through
with no per-call vector); the `EXPFI` entry at `105555B` in `006-S3FS` is real code
(entry bytes `174270 174020 124005` match the disassembly); EXPFI presets
`SSM=1`/`SSK=0` and shares its body with the three sibling entries `SFACC`/`CRALN`/
`CRALF`; the mode fold yields mode 2; and the second-phase link cell `106032`
contains the FILSYS worker address `MEXFI = 116623`.

**What is NOT proven:** the link from the zero GOTAB slot to the `EXPFI` worker.
Because the vector is zero there is no stub to disassemble and no pointer to
dereference; dispatch drops into the resident `MFELL`/`CALLPROC` second-level path,
which lives in an **uncarved overlay**. So the `MON 231 -> EXPFI` attribution rests
on the `EXPFI` symbol name + the matching `MEXFI` (expand-file) worker call, not a
followed pointer - hence **MISATTRIBUTED** in the strict sense. Confirming the link
needs a live trace: issue a real `MON 231`, single-step the level-14 fall-through
into the resident `CALLPROC` dispatch, and confirm P lands on `EXPFI = 105555`.

The prologue link cell `003752`, the return cell `003776`, and the cells `010500`/
`010506` (reached via `105713`/`105717`) match no `FILSYS-SYMBOLS` entry; their low
addresses suggest resident-monitor routines outside the resolvable symbol set, and
are not resolved here.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
