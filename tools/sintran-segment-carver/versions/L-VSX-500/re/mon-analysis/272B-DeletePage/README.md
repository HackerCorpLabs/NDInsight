# MON 272B (octal) — DeletePage (DELPG)

Deletes the pages of an opened file between two page numbers (FirstPage..LastPage inclusive;
LastPage = −1 means to end of file) and returns the number of pages deleted.

**Status:** dispatch head byte-proven as a **fall-through** (`GOTAB[272B] = 000000`); worker body is
real SINTRAN L bytes; the exact `MON 272 → worker` link crosses an uncarved kernel bridge
(`CALLPROC`/`MFELL`) — see [Honest caveats](#honest-caveats). All addresses/values are **octal**.

- **Full disassembly:** [`272B-DeletePage.ASM`](272B-DeletePage.ASM) — the actual `DELPG` worker body (MON 272B has no GOTAB entry stub, see below).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 272B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[272B] = 000000<br/>(fall-through, byte proven)"]
    C -.uncarved CALLPROC/MFELL.-> E["DELPG delete worker<br/>006-S3FS :110472B"]
    E --> F["page-table free<br/>via JPL I workers"]
    class A blue
    class B,C teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

`GOTAB[272B]` is literally `000000`, so there is no entry stub to disassemble; the dashed hop
(`C ⇢ E`) is the resident `CALLPROC`/`MFELL` second-level dispatch, which is **not present in any
carved segment** and cannot be followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr − loadbase)` in octal words × 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[272] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071525B` (1 word) | 59050 | `GOTAB+272` = `000000` | **VERIFIED** (fall-through) |
| resident CALLPROC/MFELL bridge | — (uncarved) | — | — | `CALLPROC` | **UNVERIFIED** |
| DELPG delete worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) · [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `110472B–111012B` | 51828 | `DELPG` | real bytes; link **UNVERIFIED** |

**Verify by hand:** `grep '^110472 ' ../../segments-ref/006-S3FS/006-S3FS.hex` → byte offset `51828`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=51828 count=4 | od -An -tx1` → `22 4e cc 65`
(the stored words `021116 146145` = `STD I 116` / `RADD CLD SL DA`, the DELPG prologue).
Confirm the fall-through: `dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=59050 count=2 | od -An -tx1`
→ `00 00`; `prove-mon.py 272` reads `GOTAB[272] … 000000`.

---

## Instruction walkthrough

Full listing: [`272B-DeletePage.ASM`](272B-DeletePage.ASM). There is no entry stub (fall-through
dispatch); the body is the `DELPG` worker.

**Prologue (110472–110476)** saves the caller `A:D` pair (`110472 STD I 116`) and calls resident
register-save `SPUSH` via pointer `@110611 = 003752B`. **File lookup (110477–110501)** resolves the
open-file descriptor from the caller file number via pointer `@110613 = 010376B`. **Descriptor +
access checks (110502–110537)** test the descriptor for null (`110504 SKP IF DX EQL 0` → error
`110507 SAA 132` / `110511 SAA 125`), scan attribute bits (`110513 LDA ,X 3`, masked `BSKP` →
`SAA 133/125`) and set up the page range (`110524–110537 JPL I 66/60` → `@110616`); all fault paths
take an indirect exit → `@110614`. **Range bound (110540–110606)** computes `LastPage − FirstPage`
as a 32-bit value (`110576 RSUB SX DD` / `110577 RADD ADC` / `110601 RSUB ST DA`); `110602 JAP`
errors (`SAA 174`) on a negative range. **Delete loop (110624–111002)** locates each page in the
file's page-table entry (compares against the `,X 27` / `,X 51` slot lists), frees the mapping
(`110647/110650 STZ ,X 0` / `STZ ,X 1`), marks the table word dirty (`110653/110674 BSET ONE … DT` /
`STT ,X 7`), commits it (`110700 JPL I 110` → `@111010`), and bumps the 32-bit deleted count
(`,B 15`) and current page (`,B 7`), looping while pages remain (`110721 JAN → 110723`).
**Result + return (110723–110760)** stores the 32-bit deleted count into the caller frame
(`110737 STD ,X 23`) and returns via `110760 JMP I 32` (`@111012 = 003776B`, `SPOP`).
**Pointer/data tables (110610–110623, 111005–111012)** hold the `JPL I` targets.

---

## Parameter / register contract

Manual-side names/types are from [`272B_DeletePage.yaml`](../../../../../../../Developer/MON/calls/272B_DeletePage.yaml)
(MAC: `LDT FILNO` / `LDA (FIRST` / `LDX (LAST` / `MON 272` / `STD NODEL`).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `T` = FileNo | in | open-file number, resolved at `110477–110501` | inferred (manual) |
| `A` = FirstPage | in | address of the 32-bit first page to delete | inferred (manual) |
| `X` = LastPage | in | address of the 32-bit last page (−1 = to end of file) | inferred (manual) |
| range check | internal | `LastPage − FirstPage` (32-bit) at `110576–110601`; negative → error 174 | VERIFIED (bytes) |
| `D` = NoOfPages | out | number of pages deleted (double word), stored at `110737` | VERIFIED stored; meaning inferred |
| error return | out | standard error code on the fault paths | inferred (manual) |

This post-CALLPROC body works through `,B` frame fields, so the precise T/A/X mapping is **inferred**
from the manual and the caller-side `MON 272` wrapper, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`272B-DeletePage.pseudo.c`](272B-DeletePage.pseudo.c)** — a pseudo-C model of the handler for
emulator authors. Control flow, the 32-bit range check and the page-free loop shape are
byte-verified; the page-table/free-space worker semantics are inferred from the call structure. Every
instruction is translated per the canonical
**[ND-100 Instruction Semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)**.

---

## Honest caveats

**What is byte-proven:** `GOTAB[272B] = 000000` (a fall-through, matching `prove-mon.py 272`); the
`DELPG` worker at `110472B` is real code (its `SPUSH`/`SPOP` prologue/epilogue, its open-file lookup,
its 32-bit `LastPage − FirstPage` range check, its page-table clearing loop, and its 32-bit
deleted-count result at `110737`).

**What is NOT proven:** the link from the MON number to the `DELPG` worker. Because `GOTAB[272] =
000000` there is **no** static edge from the level-14 dispatch word into `110472B`; the only path
runs through the resident `CALLPROC`/`MFELL` second-level dispatch, in an **uncarved** overlay. So
the `MON 272 → DELPG` attribution rests on the symbol name (`DELPG` = DELete PaGe) + the matching
delete behaviour, not a followed pointer — **UNVERIFIED** in the strict sense. This reconciles into
one story: the body is genuine SINTRAN L bytes (its direct branches all close inside the carve, up to
its pointer tables), but the *attachment* to MON 272 is runtime-populated. Confirming it needs a live
trace: break at the level-14 MON entry with the number = `272`, single-step the resident
fall-through, and confirm P lands on `DELPG = 110472B`.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) §9 · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) §G · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
