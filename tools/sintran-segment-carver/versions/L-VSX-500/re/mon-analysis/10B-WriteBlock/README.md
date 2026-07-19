# MON 10B (octal) — WriteBlock (WPAGE)

> **CORRECTED 2026-07-15 (byte-verified).** The worker + dispatch described below are on the
> DEBUNKED model and are WRONG. Byte truth from the carved L07 image:
> `MCTAB[10B] = 005630B = XWPAG=026574B` in segment 006-S3FS, reached by the real dispatch
> `MON 10B -> ENT14(072167B) -> GOTAB[10B]=MFELL(072114B) -> CALLP(032201B) -> MCTAB[10B]=XWPAG`.
> Any "GOTAB from commoncode" / "uncarved CALLPROC bridge" / "F16xx stub" / old worker name below
> is an artefact of the wrong table. Verified: `dd if=044-S3IDPIT.bin bs=1 skip=1840 count=2`
> -> `2d 7c`. Cross-ref ../317B-ExecuteCommand/README.md and SINTRAN/CARVING-HANDOFF.md sec 3a.

Writes one block randomly to a file opened for random write access (standard block = 512 bytes;
first block is number 0). It shares one code body with MON 7B ReadBlock (RPAGE); the two enter one
word apart and the body forks on a read/write skip flag (`SSK`). MON 10B is the **write** entry: it
sets `SSK` at `101711B`.

**Status:** dispatch head byte-proven as a **fall-through** (`GOTAB[10B] = 000000`); worker body is
real SINTRAN L bytes; the exact `MON 10 → worker` link crosses an uncarved kernel bridge
(`CALLPROC`/`MFELL`) — see [Honest caveats](#honest-caveats). All addresses/values are **octal**.

- **Full disassembly:** [`10B-WriteBlock.ASM`](10B-WriteBlock.ASM) — the actual code (the shared read/write worker body; MON 10B has no GOTAB entry stub, see below).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 10B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[10B] = 000000<br/>(fall-through, byte proven)"]
    C -.uncarved CALLPROC/MFELL.-> E["WPAGE write worker<br/>006-S3FS :101711B"]
    E --> F["fs read/write block<br/>via JPL I workers"]
    class A blue
    class B,C teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

Unlike its read twin MON 7B (`GOTAB[7B] = 120402B`, a real `F1612` entry stub), MON 10B's GOTAB word
is **zero**: there is no level-14 stub. The dashed hop (`C ⇢ E`) is the resident `CALLPROC`/`MFELL`
second-level dispatch that binds the MON number to the worker at run time — it is **not present in
any carved segment**, so it is the one link that cannot be followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr − loadbase)` in octal words × 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[10] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071243B` (1 word) | 58694 | `GOTAB+10` = `000000` | **VERIFIED** (fall-through) |
| resident CALLPROC/MFELL bridge | — (uncarved) | — | — | `CALLPROC` | **UNVERIFIED** |
| WPAGE write worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) · [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `101711B–102020B` | 44946 | `WPAGE` (`RPAGE`=101707) | real bytes; link **UNVERIFIED** |

**Verify by hand:** `grep '^101711 ' ../../segments-ref/006-S3FS/006-S3FS.hex` → byte offset `44946`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=44946 count=4 | od -An -tx1` → `f8 90 22 3f`
(the stored words `174220 021077` = `BSET ONE SSK` / `STD I 77`, the WPAGE write-set entry).
Confirm the fall-through: `dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58694 count=2 | od -An -tx1`
→ `00 00`; `prove-mon.py 10` reads `GOTAB[10] … 000000`.

---

## Instruction walkthrough

Full listing: [`10B-WriteBlock.ASM`](10B-WriteBlock.ASM). The functional body is the shared
read/write worker; MON 10B (`WPAGE`) enters at `101711B`, its read twin MON 7B (`RPAGE`) at `101707B`.

**Read/write entry split (101707–101712)** — `RPAGE` enters at `101707` and clears `SSK` (read);
`WPAGE` (MON 10B) enters one word later at `101711` and **sets** `SSK` (write); one common body follows.
```
101707  174020  BSET ZRO SSK   ; RPAGE: skip flag = 0 (READ)
101710  124002  JMP  101712    ; jump over the write-set
101711  174220  BSET ONE SSK   ; WPAGE entry (MON 10B): skip flag = 1 (WRITE)
101712  021077  STD I 77       ; save D (double) -> param slot
```
**Prologue (101713–101716)** calls the resident register-save `SPUSH` via pointer `@102012 =
003752B`. **File lookup (101717–101721)** resolves the open-file descriptor from the caller file
number via pointer `@102014 = 010376B`. **Descriptor + access checks (101722–101756)** fork on `SSK`,
test for a null descriptor (`101727 SKP IF DX EQL 0`) and scan access-attribute bits with masked
`BSKP`, loading error codes on failure. **Position compute + fork (101757–102010)** builds the
32-bit on-disk position (`SAD ZIN 1`, `RADD ADC`), then forks on `SSK` at `101775 BSKP ONE SSK`:
with `SSK = 1` (write) `101777 JPL I 17` (`@102016 = 100130B`, `FWRT`); with `SSK = 0` (read) it
takes `102002 JPL I 15` (`@102017 = 077542B`, `FREA`). Tail `102006 JMP I 12` (`@102020 = 003776B`,
`SPOP`) restores + returns; `102007 STA ,B 2` funnels the status/error. **Pointer table
(102011–102020)** — data: `003752 (SPUSH)`, `010376`, `033740`, `100130 (FWRT)`, `077542 (FREA)`,
`003776 (SPOP)`.

---

## Parameter / register contract

Manual-side names/types are from [`10B_WriteBlock.yaml`](../../../../../../../Developer/MON/calls/10B_WriteBlock.yaml)
(MAC: `LDT FILNO` / `LDA BLKNO` / `LDX (BUFF` / `MON 10`).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `101711B` = write; `101707B` = read (shared body, `SSK` split) | VERIFIED (bytes) |
| `SSK` | internal | write (1) / read (0) selector, set at `101711`, tested at `101722/101775` | VERIFIED (bytes) |
| `T` = FileNumber | in | open-file number, resolved at `101717–101721` | inferred (manual) |
| `A` = BlockNumber | in | block number → on-disk position | inferred (manual) |
| `X` = Buffer | in | caller buffer (data to write) | inferred (manual) |
| `A` = ErrCode | out | standard error code on the fault paths | inferred (manual) |
| `FWRT` (100130) | call | file-write worker (write path), via ptr `102016` + `SSK` select | VERIFIED (ptr table) |
| `FREA` (077542) | call | file-read worker (read path), via ptr `102017` | VERIFIED (ptr table) |

The exact user-visible register convention lives in the caller-side `MON 10` wrapper and the uncarved
`CALLPROC` frame, so the precise T/A/X assignment is **inferred**, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`10B-WriteBlock.pseudo.c`](10B-WriteBlock.pseudo.c)** — a pseudo-C model of the handler for
emulator authors. Control flow + the read/write (`SSK`) fork are byte-verified; the file-system
worker semantics are inferred from the call structure. Every instruction is translated per the
canonical **[ND-100 Instruction Semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)**.

---

## Honest caveats

**What is byte-proven:** `GOTAB[10B] = 000000` (a fall-through, matching `prove-mon.py 10` reading
the running-confirmed resident `commoncode.bin`); the `WPAGE` worker entry at `101711B` is real
`BSET ONE SSK` (write-select) code and belongs to the random-block read/write family (its `SSK`
split); the read/write decision at `101775 BSKP ONE SSK` dispatching to `FWRT` (write) vs `FREA`
(read) via the pointer table.

**What is NOT proven:** the link from the MON number to the `WPAGE` worker (in `006-S3FS`). Because
`GOTAB[10] = 000000` there is **no** static edge from the level-14 dispatch word into `101711B`; the
only path runs through the resident `CALLPROC`/`MFELL` second-level dispatch, which lives in an
**uncarved** overlay (`commoncode.bin` is zero there, and `025-S3IRPIT` holds no MON-10 stub). So the
`MON 10 → WPAGE` attribution rests on the symbol name + the matching write behaviour, not a followed
pointer — **UNVERIFIED** in the strict sense. This reconciles into one story: the body is genuine,
self-consistent SINTRAN L bytes (its direct branches all close inside the carve), but its *attachment*
to MON 10 is runtime-populated and cannot be confirmed statically. Confirming it needs a live trace:
break at the level-14 MON entry with the number = `010`, single-step the resident fall-through, and
confirm P lands on `WPAGE = 101711B`.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) §9 · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) §G · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
