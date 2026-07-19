# MON 251B (octal) — CopyPage (COPAG)

Copies file pages sequentially between two opened files (one may be a magnetic tape or floppy disk
with volume). A special call used by the **BACKUP-SYSTEM** — no high-level-language interface; the
X/D register use is tailored for reading magnetic-tape labels. Copying stops at end-of-file, a
non-existent page, or a short magnetic-tape record. Both files must be local files.

**Status:** dispatch head byte-proven as a **fall-through** (`GOTAB[251B] = 000000`); worker body is
real SINTRAN L bytes; the exact `MON 251 → worker` link crosses an uncarved kernel bridge
(`CALLPROC`/`MFELL`) — see [Honest caveats](#honest-caveats). All addresses/values are **octal**.

- **Full disassembly:** [`251B-CopyPage.ASM`](251B-CopyPage.ASM) — the actual `COPAG` worker body (MON 251B has no GOTAB entry stub, see below).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 251B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[251B] = 000000<br/>(fall-through, byte proven)"]
    C -.uncarved CALLPROC/MFELL.-> E["COPAG copy worker<br/>006-S3FS :110050B"]
    E --> F["per-page copy<br/>via JPL I workers"]
    class A blue
    class B,C teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

`GOTAB[251B]` is literally `000000`, so there is no entry stub to disassemble; the dashed hop
(`C ⇢ E`) is the resident `CALLPROC`/`MFELL` second-level dispatch, which is **not present in any
carved segment** and cannot be followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr − loadbase)` in octal words × 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[251] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071504B` (1 word) | 59016 | `GOTAB+251` = `000000` | **VERIFIED** (fall-through) |
| resident CALLPROC/MFELL bridge | — (uncarved) | — | — | `CALLPROC` | **UNVERIFIED** |
| COPAG copy worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) · [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `110050B–110471B` | 51280 | `COPAG` | real bytes; link **UNVERIFIED** |

**Verify by hand:** `grep '^110050 ' ../../segments-ref/006-S3FS/006-S3FS.hex` → byte offset `51280`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=51280 count=4 | od -An -tx1` → `22 5e cc 65`
(the stored words `021136 146145` = `STD I 136` / `RADD CLD SL DA`, the COPAG prologue).
Confirm the fall-through: `dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=59016 count=2 | od -An -tx1`
→ `00 00`; `prove-mon.py 251` reads `GOTAB[251] … 000000`.

---

## Instruction walkthrough

Full listing: [`251B-CopyPage.ASM`](251B-CopyPage.ASM). There is no entry stub (fall-through
dispatch); the body is the `COPAG` worker.

**Prologue (110050–110054)** saves `D` (`110050 STD I 136` — `D` = the buffer address per the MAC
example) and calls resident register-save `SPUSH` via pointer `@110207 = 003752B`.
**Open source + dest (110055–110074)** resolves the two open-file descriptors: `110057 JPL I 131`
(`@110210`) opens/validates one, `110060 JMP I 131` (`@110211`) is the destination-error return; the
sequence repeats at `110065–110070` for the second file (source-error return `110070 JMP I 122`).
**First-page fetch (110075–110102)** loads the caller 32-bit first-page address (`X` points at a
double word), shared by both files. **Main copy loop (110103–110325)** branches on whether each
side is a real file (`110110/110134/110162 JAF`), transfers one page (`110155 JPL I 36` →
`@110215`), detects missing pages / holes (`110126 JAP`, `110145 JAF`) and stores the missing-page
skip-return values (`110133/110161/110204 STD ,B nn`), then bumps the page (`110321 MIN ,B 17`) and
copied counters (`110323 MIN ,B 16`). **Pointer/data tables (110206–110216, 110357–110374,
110463–110471)** hold the `JPL I`/`JMP I` targets (`SPUSH 003752`, `SPOP 003776` among them).

---

## Parameter / register contract

Manual-side names/types are from [`251B_CopyPage.yaml`](../../../../../../../Developer/MON/calls/251B_CopyPage.yaml)
(MAC: `LDT FL1NO` / `LDA (BUFF` / `COPY SA DD` / `LDA FL2NO` / `LDX (PAGE` / `MON 251`).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `T` = SourceFile | in | source file number (or logical device for tape/floppy) | inferred (manual) |
| `A` = DestFile | in | destination file number (or logical device) | inferred (manual) |
| `X` = FirstPage | in | address of a 32-bit word with the first page address | inferred (manual) |
| `D` = DestBuffer | in | address of a buffer for a short magtape record (−1 = none), saved at `110050` | VERIFIED saved; meaning inferred |
| `A`&`D` = FirstPageMiss | out | missing-page number (double-skip return) | inferred (manual) |
| `T`&`X` = LastPageMiss | out | last page number in a hole (double-skip return, directory source) | inferred (manual) |
| error return | out | destination error / source error (EOF = 3) | inferred (manual) |

This post-CALLPROC body works through `,B` frame fields, so the precise T/A/X/D mapping is
**inferred** from the manual and the caller-side `MON 251` wrapper, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`251B-CopyPage.pseudo.c`](251B-CopyPage.pseudo.c)** — a pseudo-C model of the handler for
emulator authors. Control flow and the copy-loop shape are byte-verified; the per-page worker and
tape/hole semantics are inferred from the call structure and the manual. Every instruction is
translated per the canonical
**[ND-100 Instruction Semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)**.

---

## Honest caveats

**What is byte-proven:** `GOTAB[251B] = 000000` (a fall-through, matching `prove-mon.py 251`); the
`COPAG` worker at `110050B` is real code (its `SPUSH`/`SPOP` prologue/epilogue, its two-file open
sequence, its page-copy loop with missing-page skip returns).

**What is NOT proven:** the link from the MON number to the `COPAG` worker. Because `GOTAB[251] =
000000` there is **no** static edge from the level-14 dispatch word into `110050B`; the only path
runs through the resident `CALLPROC`/`MFELL` second-level dispatch, in an **uncarved** overlay. So
the `MON 251 → COPAG` attribution rests on the symbol name (`COPAG` = COpy PAGe) + the matching
copy behaviour, not a followed pointer — **UNVERIFIED** in the strict sense. This reconciles into
one story: the body is genuine SINTRAN L bytes (its direct branches all close inside the carve, up to
its pointer tables), but the *attachment* to MON 251 is runtime-populated. Confirming it needs a live
trace: break at the level-14 MON entry with the number = `251`, single-step the resident
fall-through, and confirm P lands on `COPAG = 110050B`.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) §9 · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) §G · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
