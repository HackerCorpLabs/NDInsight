# MON 5B (octal) — ReadScratchFile (RDISK)

Reads one block from the scratch file (file `100B`) into a caller-supplied buffer. It shares
one code body with MON 6B WriteScratchFile (WDISK); the two enter one word apart and the body
forks on a read/write skip flag (`SSK`).

**Status:** dispatch head byte-proven; worker body is real SINTRAN L bytes; the exact
`MON 5 → worker` link crosses an uncarved kernel bridge (see [Honest caveats](#honest-caveats)).
All addresses/values are **octal**.

- **Full disassembly:** [`005B-ReadScratchFile.ASM`](005B-ReadScratchFile.ASM) — the actual code, both regions (entry stub + read worker).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 5B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[5B] = 120355B<br/>(byte + live proven)"]
    C --> D["F1611 entry stub<br/>025-S3IRPIT :120355B"]
    D -.uncarved CALLPROC.-> E["RDISK read worker<br/>006-S3FS :102021B"]
    E --> F["fs read/write block<br/>via JPL I workers"]
    class A blue
    class B,C,D teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The dashed hop (`D ⇢ E`) is the resident `CALLPROC`/segment-switch — it is **not present in any
carved segment**, so it is the one link that cannot be followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr − loadbase)` in octal words × 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[5] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071240B` (1 word) | 58688 | `GOTAB+5` = `120355B` | **VERIFIED** |
| F1611 entry stub | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `120355B–120425B` | 55770 | `F1611` | **VERIFIED** |
| resident CALLPROC bridge | — (uncarved) | — | — | `CALLPROC` | **UNVERIFIED** |
| RDISK read worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) · [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `102021B–102161B` | 45090 | `RDISK` | real bytes; link **MISATTRIBUTED** |

**Verify by hand:** `grep '^102021 ' ../../segments-ref/006-S3FS/006-S3FS.hex` → byte offset `45090`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=45090 count=8 | od -An -tx1` → `f4 10 a8 02 f8 90 …`
(= octal `174020 124002 174220 …`, the RDISK/WDISK entry split).

---

## Instruction walkthrough

Full listing: [`005B-ReadScratchFile.ASM`](005B-ReadScratchFile.ASM). The functional body is the
RDISK worker (region B); the F1611 stub (region A) is the level-14 entry.

**Read/write entry split (102021–102024)** — RDISK enters at `102021` and clears `SSK` (read);
WDISK (MON 6B) enters one word later at `102023` and sets `SSK` (write). One common body follows.
```
102021  174020  BSET ZRO SSK   ; RDISK: skip flag = 0 (READ)
102022  124002  JMP  102024    ; jump over the write-set
102023  174220  BSET ONE SSK   ; WDISK entry: skip flag = 1 (WRITE)
102024  021073  STD I 73       ; save D (double) -> param slot
```
**Prologue (102024–102031)** stages caller args; `102030 JPL I 70` calls a resident FS worker via
pointer word `@102120 = 003752B`.
**Classification loop (102032–102060)** scans a 2-word/entry table (`AAX 2` / `JMP 102032`), testing
attribute bits with masked `BSKP` skips until a match.
**Position compute + fork (102060–102116)** computes the on-disk position (`SUB 40` / `SHA SHR 1` /
`AAA 100` = address/size scaling), then forks on `SSK`: `102105 JPL I 20` (write worker `@102125 =
100130B`) vs `102110 JPL I 16` (read worker `@102126 = 077542B`); tail `102114 JMP I 13` (`@102127 =
003776B`).
**Pointer table (102117–102127)** — data, the address constants used by the `JPL I`/`JMP I` above:
`@102120=003752 @102124=033740 @102125=100130 @102126=077542 @102127=003776`.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `102021B` = read; `102023B` = write (shared body, `SSK` split) | VERIFIED (bytes) |
| `D` (double) | in | caller parameter saved first (`STD I 73`) — block/descriptor | inferred |
| `SSK` | internal | read (0) / write (1) selector, tested at `102040/102103/102152` | VERIFIED (bytes) |
| `A`/`X`/`B` | in | staged in the prologue into working regs; buffer + position operands | inferred |
| skip return | out | success skip-return set up by `102160 SKP IF DX EQL 0` | inferred |

The exact user-visible register convention lives in the caller-side `MON 5` wrapper and the uncarved
CALLPROC frame, so the precise A/X/T assignment is **inferred**, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`005B-ReadScratchFile.pseudo.c`](005B-ReadScratchFile.pseudo.c)** - a pseudo-C model of the
handler for emulator authors. Control flow + the read/write (`SSK`) fork are byte-verified; the
file-system worker semantics are inferred from the call structure. Every instruction is translated
per the canonical
**[ND-100 Instruction Semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)**.


---

## Honest caveats

**What is byte-proven:** `GOTAB[5B] = 120355B` (level-14 dispatch, matches a live read of the running
system); the `F1611` stub at `120355B` is real code and belongs to the scratch read/write family (its
`SSK` split); the `RDISK` worker entry bytes at `102021B` match the disassembly.

**What is NOT proven:** the link from the `F1611` stub (in `025-S3IRPIT`) to the `RDISK` worker (in
`006-S3FS`). The value `102021` occurs **zero** times in `025-S3IRPIT`, and every pointer the stub
dereferences stays inside `025-S3IRPIT`. The stub→worker transfer is the resident `CALLPROC`/segment
switch, which is in an **uncarved overlay**. So the `MON 5 → RDISK` attribution rests on the symbol
name + the matching read/write behaviour, not a followed pointer — hence **MISATTRIBUTED** in the
strict sense. Confirming it needs a live trace (break at `120355B` on a real `MON 5`, single-step the
segment switch, confirm P lands on `RDISK=102021`).

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) §9 · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) §G · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
