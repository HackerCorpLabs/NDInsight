# MON 270B (octal) — ReadDiskPage (RDPAG)

Reads one or more raw directory pages (2048 bytes each) from a disk into a caller buffer; any page
can be read. The directory must be reserved with ReserveDir. It shares one code body with MON 271B
WriteDiskPage (WDPAG); the two enter one word apart and the body forks on a read/write skip flag
(`SSK`).

**Status:** dispatch head byte-proven (`GOTAB[270B] = 066276B`, a real `F1742` entry stub); worker
body is real SINTRAN L bytes; the exact `MON 270 → worker` link crosses an uncarved kernel bridge
(see [Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`270B-ReadDiskPage.ASM`](270B-ReadDiskPage.ASM) — the actual code, both regions (entry stub + read/write worker).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 270B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[270B] = 066276B<br/>(byte proven)"]
    C --> D["F1742 entry stub<br/>025-S3IRPIT :066276B"]
    D -.uncarved CALLPROC.-> E["RDPAG read worker<br/>006-S3FS :107447B"]
    E --> F["disk page transfer<br/>via JPL I workers"]
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
| GOTAB[270] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071523B` (1 word) | 59046 | `GOTAB+270` = `066276B` | **VERIFIED** |
| F1742 entry stub | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `066276B–066301B` | 29052 | `F1742` | **VERIFIED** |
| resident CALLPROC bridge | — (uncarved) | — | — | `CALLPROC` | **UNVERIFIED** |
| RDPAG/WDPAG read/write worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) · [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `107447B–110047B` | 50766 | `RDPAG` (`WDPAG`=107451) | real bytes; link **MISATTRIBUTED** |

**Verify by hand:** `grep '^107447 ' ../../segments-ref/006-S3FS/006-S3FS.hex` → byte offset `50766`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=50766 count=4 | od -An -tx1` → `f8 10 a8 02`
(the stored words `174020 124002` = `BSET ZRO SSK` / `JMP` — the RDPAG read entry + skip-over).
Confirm the dispatch word: `dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=59046 count=2 | od -An -tx1`
→ `6c be` (the stored word `066276` = `GOTAB[270]`). `prove-mon.py 270` reads the same.

---

## Instruction walkthrough

Full listing: [`270B-ReadDiskPage.ASM`](270B-ReadDiskPage.ASM). Region A is the tiny `F1742`
level-14 stub (4 words: it primes `,B -1` / `,B 17` and clears `,B -2`/`,B -1` — a
parameter-descriptor default, then dispatch continues resident); region B is the shared read/write
worker (`RDPAG` read entry `107447B`, `WDPAG` write entry `107451B`).

**Read/write entry split (107447–107452)** — `RDPAG` enters at `107447` and clears `SSK` (read);
`WDPAG` (MON 271B) enters one word later at `107451` and sets `SSK` (write); one common body follows.
```
107447  174020  BSET ZRO SSK   ; RDPAG: skip flag = 0 (READ)
107450  124002  JMP  107452    ; jump over the write-set
107451  174220  BSET ONE SSK   ; WDPAG entry (MON 271B): skip flag = 1 (WRITE)
107452  021047  STD I 47       ; save D (double) -> param slot
```
**Prologue (107453–107456)** sets the frame base (`107455 SAB 22`) and calls resident register-save
`SPUSH` via pointer `@107522 = 003752B`. **Function-code + directory resolve (107457–107466)** forks
on `SSK` to load a device function code (`107462 SAA 61` write / `107464 SAA 60` read) into `,B 20`,
then `107466 JPL I 35` (`@107523 = 030225B`) resolves/validates the directory index.
**Index + reservation checks (107467–107520)** stage the buffer (`107467 STX ,B 7`), test the
directory descriptor and its reserved flag, and on failure load an error code and take an indirect
exit (`107474 JMP I 31`, `107502 JMP I 23`, `107520 JMP I 5` → `@107525`).
**Page transfer loop (107531–110035)** scales the page count to words (`107536 SHA ZIN 12` = ×1024,
1 page = 2048 bytes = 1024 words), builds and issues the disk request via `JPL I` workers
(`107635/107676`), advances the 32-bit disk page address by 1 per page (`110026–110034`), and loops
while pages remain (`110035 JMP → 107741`). **Pointer/data tables (107703–107724, 110036–110047)**
hold the `JPL I` targets (`SPUSH 003752`, directory helper `030225` among them).

---

## Parameter / register contract

Manual-side names/types are from [`270B_ReadDiskPage.yaml`](../../../../../../../Developer/MON/calls/270B_ReadDiskPage.yaml)
(MAC: `LDT DIRIX` / `LDX (BUFF` / `LDA COUNT` / `COPY SA DD` / `LDA (PAGNO` / `MON 270`).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `107447B` = read; `107451B` = write (shared body, `SSK` split) | VERIFIED (bytes) |
| `SSK` | internal | read (0) / write (1) selector, tested at `107460` | VERIFIED (bytes) |
| `T` = DirIndex | in | directory index (see GetDirUserIndexes), resolved at `107466` | inferred (manual) |
| `X` = Buffer | in | caller buffer (even byte address), staged at `107467` | inferred (manual) |
| `A` = NoOfPages | in | page count → words via `107536 SHA ZIN 12` | VERIFIED scale; meaning inferred |
| `A` = PageAddr | in | address of the 32-bit disk page address | inferred (manual) |
| function code | internal | `061` (write) / `060` (read) into `,B 20` | VERIFIED (bytes) |
| error return | out | standard error code on the fault paths | inferred (manual) |

This post-CALLPROC body works through `,B` frame fields, so the precise T/A/X mapping is **inferred**
from the manual and the caller-side `MON 270` wrapper, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`270B-ReadDiskPage.pseudo.c`](270B-ReadDiskPage.pseudo.c)** — a pseudo-C model of the handler
for emulator authors. Control flow, the read/write (`SSK`) fork and the page→word scale are
byte-verified; the disk-driver worker semantics are inferred from the call structure. Every
instruction is translated per the canonical
**[ND-100 Instruction Semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)**.

---

## Honest caveats

**What is byte-proven:** `GOTAB[270B] = 066276B` (level-14 dispatch; `prove-mon.py 270` reads the
same word); the `F1742` stub at `066276B` is real code; the `RDPAG` worker entry at `107447B` is
real `BSET ZRO SSK` code with a `SSK` read/write split and a page→word scale (`SHA ZIN 12`) matching
the manual's 2048-byte page.

**What is NOT proven:** the link from the `F1742` stub (in `025-S3IRPIT`) to the `RDPAG` worker (in
`006-S3FS`). The `F1742` stub is only 4 words and holds no pointer to `107447`; the stub→worker
transfer is the resident `CALLPROC`/segment switch, in an **uncarved overlay** — hence
**MISATTRIBUTED** in the strict sense. This reconciles into one story: the dispatch head and the
worker are both genuine SINTRAN L bytes (the body's direct branches all close inside the carve, up to
its pointer tables), but the *attachment* of MON 270 to `RDPAG` rests on the symbol name + matching
read behaviour, not a followed pointer. Confirming it needs a live trace (break at `066276B` on a real
`MON 270`, single-step the segment switch, confirm P lands on `RDPAG = 107447`).

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) §9 · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) §G · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
