# MON 7B (octal) — ReadBlock (RPAGE)

> **CORRECTED 2026-07-15 (byte-verified).** The worker + dispatch described below are on the
> DEBUNKED model and are WRONG. Byte truth from the carved L07 image:
> `MCTAB[7B] = 005627B = XRPAG=026572B` in segment 006-S3FS, reached by the real dispatch
> `MON 7B -> ENT14(072167B) -> GOTAB[7B]=MFELL(072114B) -> CALLP(032201B) -> MCTAB[7B]=XRPAG`.
> Any "GOTAB from commoncode" / "uncarved CALLPROC bridge" / "F16xx stub" / old worker name below
> is an artefact of the wrong table. Verified: `dd if=044-S3IDPIT.bin bs=1 skip=1838 count=2`
> -> `2d 7a`. Cross-ref ../317B-ExecuteCommand/README.md and SINTRAN/CARVING-HANDOFF.md sec 3a.

Reads one block randomly from a file opened for random read access (standard block = 512 bytes;
first block is number 0). It shares one code body with MON 10B WriteBlock (WPAGE); the two enter
one word apart and the body forks on a read/write skip flag (`SSK`) — the same shape as the
scratch-file twins MON 5B/6B (`RDISK`/`WDISK`), but here the file is chosen by a caller file
number, not the fixed scratch file.

**Status:** dispatch head byte-proven (`GOTAB[7B] = 120402B`, a real `F1612` entry stub); worker
body is real SINTRAN L bytes; the exact `MON 7 → worker` link crosses an uncarved kernel bridge
(see [Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`7B-ReadBlock.ASM`](7B-ReadBlock.ASM) — the actual code, both regions (entry stub + read/write worker).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 7B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[7B] = 120402B<br/>(byte proven)"]
    C --> D["F1612 entry stub<br/>025-S3IRPIT :120402B"]
    D -.uncarved CALLPROC.-> E["RPAGE read worker<br/>006-S3FS :101707B"]
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
| GOTAB[7] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071242B` (1 word) | 58692 | `GOTAB+7` = `120402B` | **VERIFIED** |
| F1612 entry stub | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `120402B–120425B` | 55812 | `F1612` | **VERIFIED** |
| resident CALLPROC bridge | — (uncarved) | — | — | `CALLPROC` | **UNVERIFIED** |
| RPAGE/WPAGE read/write worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) · [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `101707B–102020B` | 44942 | `RPAGE` (`WPAGE`=101711) | real bytes; link **MISATTRIBUTED** |

**Verify by hand:** `grep '^101707 ' ../../segments-ref/006-S3FS/006-S3FS.hex` → byte offset `44942`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=44942 count=4 | od -An -tx1` → `f8 10 a8 02`
(the stored words `174020 124002` = `BSET ZRO SSK` / `JMP` — the RPAGE read entry + skip-over).
Confirm the dispatch word: `dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58692 count=2 | od -An -tx1`
→ `a1 02` (the stored word `120402` = `GOTAB[7]`). `prove-mon.py 7` reads the same.

---

## Instruction walkthrough

Full listing: [`7B-ReadBlock.ASM`](7B-ReadBlock.ASM). Region A is the `F1612` level-14 stub; region
B is the shared read/write worker (`RPAGE` read entry `101707B`, `WPAGE` write entry `101711B`).

**Read/write entry split (101707–101712)** — `RPAGE` enters at `101707` and clears `SSK` (read);
`WPAGE` (MON 10B) enters one word later at `101711` and sets `SSK` (write); one common body follows.
```
101707  174020  BSET ZRO SSK   ; RPAGE: skip flag = 0 (READ)
101710  124002  JMP  101712    ; jump over the write-set
101711  174220  BSET ONE SSK   ; WPAGE entry (MON 10B): skip flag = 1 (WRITE)
101712  021077  STD I 77       ; save D (double) -> param slot
```
**Prologue (101713–101716)** stages args and calls the resident register-save via pointer word
`@102012 = 003752B` (`SPUSH`). **File lookup (101717–101721)** stages the caller file number (`A=T`)
and calls the open-file resolver via pointer `@102014 = 010376B`. **Descriptor + access checks
(101722–101756)** fork on `SSK`, test the descriptor for null (`101727 SKP IF DX EQL 0`) and scan
access-attribute bits with masked `BSKP`, loading error codes (`SAA 132/126/125/133`) on failure.
**Position compute + fork (101757–102010)** builds the 32-bit on-disk position (`SAD ZIN 1`,
`RADD ADC` negate/adjust), then forks on `SSK` at `101775 BSKP ONE SSK`: write `101777 JPL I 17`
(`@102016 = 100130B`, `FWRT`) vs read `102002 JPL I 15` (`@102017 = 077542B`, `FREA`); the tail
`102006 JMP I 12` (`@102020 = 003776B`, `SPOP`) restores + returns, and `102004 MIN ,B 4` counts a
completed block. **Pointer table (102011–102020)** — data: `003752 (SPUSH)`, `010376`,
`033740`, `100130 (FWRT)`, `077542 (FREA)`, `003776 (SPOP)`.

---

## Parameter / register contract

Manual-side names/types are from [`7B_ReadBlock.yaml`](../../../../../../../Developer/MON/calls/7B_ReadBlock.yaml)
(MAC: `LDT FILNO` / `LDA BLKNO` / `LDX (BUFF` / `MON 7`).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `101707B` = read; `101711B` = write (shared body, `SSK` split) | VERIFIED (bytes) |
| `SSK` | internal | read (0) / write (1) selector, tested at `101722/101775` | VERIFIED (bytes) |
| `T` = FileNumber | in | open-file number, resolved at `101717–101721` | inferred (manual) |
| `A` = BlockNo | in | block number → on-disk position (`101757–102010`) | inferred (manual) |
| `X` = DataDestination | in | caller buffer address | inferred (manual) |
| `A` = ErrCode | out | standard error code; set via `SAA nnn` on the fault paths | inferred (manual) |
| `FWRT` (100130) | call | file-write worker (write path), via ptr `102016` | VERIFIED (ptr table) |
| `FREA` (077542) | call | file-read worker (read path), via ptr `102017` | VERIFIED (ptr table) |

The exact user-visible register convention lives in the caller-side `MON 7` wrapper and the uncarved
CALLPROC frame, so the precise T/A/X assignment inside this body is **inferred**, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`7B-ReadBlock.pseudo.c`](7B-ReadBlock.pseudo.c)** — a pseudo-C model of the handler for
emulator authors. Control flow + the read/write (`SSK`) fork are byte-verified; the file-system
worker semantics are inferred from the call structure. Every instruction is translated per the
canonical **[ND-100 Instruction Semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)**.

---

## Honest caveats

**What is byte-proven:** `GOTAB[7B] = 120402B` (level-14 dispatch; `prove-mon.py 7` reads the same
running-confirmed word); the `F1612` stub at `120402B` is real code; the `RPAGE` worker entry at
`101707B` is real `BSET ZRO SSK` code and belongs to a random-block read/write family (its `SSK`
split, its `FWRT`/`FREA` pointer table).

**What is NOT proven:** the link from the `F1612` stub (in `025-S3IRPIT`) to the `RPAGE` worker (in
`006-S3FS`). The value `101707` occurs nowhere in the stub, and every pointer the stub dereferences
stays inside `025-S3IRPIT`; the stub→worker transfer is the resident `CALLPROC`/segment switch, in
an **uncarved overlay** — hence **MISATTRIBUTED** in the strict sense. This reconciles into one
story: the dispatch head and the worker are both genuine SINTRAN L bytes (the body's direct branches
all close inside the carve, up to its pointer table at `102011–102020`), but the *attachment* of MON 7
to `RPAGE` rests on the symbol name + matching read behaviour, not a followed pointer. Confirming it
needs a live trace (break at `120402B` on a real `MON 7`, single-step the segment switch, confirm P
lands on `RPAGE = 101707`).

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) §9 · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) §G · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
