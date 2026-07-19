# MON 62B (octal) - GetBytesInFile (RMAX)

Gets the number of data bytes in an open file, returned as a 32-bit
(INTEGER4 / double-word) value. Only the bytes containing data are counted, and
the count is only meaningful for sequentially accessed files.

**Status:** GOTAB dispatch head byte-proven as **fall-through**
(`GOTAB[62B] = 000000`, no per-call stub); the RMAX worker body is real SINTRAN L
bytes and its call to the deeper worker `RMAXB` (`72016B`) is byte-proven via a
resolved link cell; the exact `MON 62 -> worker` link crosses an uncarved kernel
bridge (see [Honest caveats](#honest-caveats)). All addresses/values are
**octal**.

- **Full disassembly:** [`62B-GetBytesInFile.ASM`](62B-GetBytesInFile.ASM) - the actual code (the RMAX worker body; there is no entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 62B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[62B] = 000000<br/>(byte-proven: fall-through)"]
    C -.uncarved MFELL / CALLPROC.-> E["RMAX byte-count worker<br/>006-S3FS :103767B"]
    E --> F["RMAXB deeper worker<br/>006-S3FS :72016B"]
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
followed statically. The `E -> F` hop (`RMAX -> RMAXB`) **is** byte-proven: link
cell `104033` = `072016B` = `RMAXB`.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words x 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[62] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071315B` (1 word) | 58778 | `GOTAB+62` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| RMAX byte-count worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `103767B-104004B` (14w) | 47086 | `RMAX` | real bytes; link **MISATTRIBUTED** |
| RMAXB deeper worker (called) | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `72016B` (entry) | 36892 | `RMAXB` | **VERIFIED** (link cell) |

**Verify by hand:** `grep '^103767 ' ../../segments-ref/006-S3FS/006-S3FS.hex` -> byte offset `47086`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=47086 count=8 | od -An -tx1` -> `22 1c cc 65 cc 59 f0 06`
(= octal `021034 146145 146131 170006` = `STD I 34` / `RADD CLD SL DA` / `RADD CLD SB DD` / `SAB 6`, the RMAX entry).

The GOTAB slot itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58778 count=2 | od -An -tx1` -> `00 00` (= `000000`, fall-through).

---

## Instruction walkthrough

Full listing: [`62B-GetBytesInFile.ASM`](62B-GetBytesInFile.ASM). The body is short
and clean (14 words). There is no F16xx stub because `GOTAB[62] = 0`. The three
indirect calls go through pointer-word link cells (`104024`, `104030`, `104033`)
in the shared pointer block just past RMAX; those words are **data (worker
addresses)**, not code.

- **Prologue (`103767-103772`)** - `103767 STD I 34` stashes the caller's `D`
  (parameter pointer); `103772 SAB 6` builds the local frame `B`.
- **Setup + work (`103773-103775`)** - `103773 JPL I 31` -> `003752` runs a
  resident prologue worker; `103774 STT I 31` stashes `T` (the file number);
  `103775 JPL I 36` -> `RMAXB` (`072016B`) is the deeper worker that computes the
  byte count.
- **Success / error split (`103776-104004`)** - `RMAXB` returns SKIP on success
  and NO-SKIP on error. The **no-skip** (error) path falls to `103776 JMP 5` ->
  `104003 STA ,B 2` (store the error code `A` into the caller's slot). The **skip**
  (success) path lands at `103777 STD ,B 2` (store the double-word byte count `D`
  into the caller's slot) and `104000 MIN ,B 4` (bump the caller's skip-return
  flag). Both paths converge at `104001 SAA -6` / `104002 JMP I 26` -> `003776`,
  the resident return cell.

The skip-return-on-success shape matches the MAC calling pattern in the manual
(`MON 62` / `JMP ERROR` / `STD BYTES` - the double word is stored only on the skip
return), and `STD ,B 2` writing a **double** word matches the INTEGER4 result.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `T` | in | file number (from an earlier OpenFile); stashed at `103774 STT I 31` | VERIFIED (`STT`); role per manual MAC example |
| `D` (double) | in | caller parameter pointer, saved first (`103767 STD I 34`) | VERIFIED (bytes); role inferred |
| `RMAXB` result | internal | byte count computed by `RMAXB` (`72016B`), returned in `D` | inferred |
| `B+2` | out | on success = INTEGER4 byte count (`STD ,B 2`); on error = error code (`STA ,B 2`) | VERIFIED (bytes) |
| `B+4` | out | caller skip-return / OK flag, bumped on success (`MIN ,B 4`) | VERIFIED (bytes) |
| skip return | out | success = SKIP return; error = normal (no-skip) return | VERIFIED (structure) |

`T` as the file-number input is byte-visible (`STT I 31`) and matches the manual's
MAC example (`LDT FILNO` / `MON 62`); the precise user-visible register frame
otherwise lives in the caller-side `MON 62` wrapper and the uncarved
`MFELL`/`CALLPROC` bridge, so it is **inferred** where not byte-shown.

---

## Pseudo-code (for an emulator)

See **[`62B-GetBytesInFile.pseudo.c`](62B-GetBytesInFile.pseudo.c)** - a pseudo-C
model of the handler for emulator authors. Control flow + the success/error
skip-return split are byte-verified; what `RMAXB` computes internally is inferred
from the call structure and the manual.

Every instruction in the pseudo-code is translated against the canonical
[ND-100 instruction semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(RADD/COPY register ops, addressing-mode effective addresses, and skip/branch
senses).

---

## Honest caveats

**What is byte-proven:** `GOTAB[62B] = 000000` (a level-14 fall-through with no
per-call vector - matches a live read of the running system); the `RMAX` worker
body at `103767B` in `006-S3FS` is real code (entry bytes match the disassembly);
and it calls `RMAXB` (`72016B`) - the link cell `104033 = 072016B` resolves to the
`RMAXB` FILSYS symbol, so the `RMAX -> RMAXB` edge is byte-proven.

**What is NOT proven:** the link from the zero GOTAB slot to the `RMAX` worker.
Because the vector is zero there is no stub to disassemble and no pointer to
dereference; dispatch drops into the resident `MFELL`/`CALLPROC` second-level
path, which lives in an **uncarved overlay**. So the `MON 62 -> RMAX` attribution
rests on the `RMAX` symbol name + the matching byte-count behaviour, not a
followed pointer - hence **MISATTRIBUTED** in the strict sense. Confirming the link
needs a live trace: issue a real `MON 62`, single-step the level-14 fall-through
into the resident `CALLPROC` dispatch, and confirm P lands on `RMAX = 103767`.

The link cells `003752` (prologue) and `003776` (resident return) match no
`FILSYS-SYMBOLS` entry; their low addresses suggest resident-monitor routines
outside the file-system segment and are not resolved here. RMAX shares its tail
pointer block with the neighbouring `REABT` routine (`104005B`), which is why the
cells it references sit past its own last instruction.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
