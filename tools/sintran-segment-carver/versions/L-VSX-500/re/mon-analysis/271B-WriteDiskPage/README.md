# MON 271B (octal) — WriteDiskPage (WDPAG)

Writes to one or more raw directory pages (2048 bytes each) from a caller buffer to disk; any page
can be written. The directory must be reserved with ReserveDir. It shares one code body with MON
270B ReadDiskPage (RDPAG); the two enter one word apart and the body forks on a read/write skip flag
(`SSK`). MON 271B is the **write** entry: it sets `SSK` at `107451B`.

**Status:** dispatch head byte-proven as a **fall-through** (`GOTAB[271B] = 000000`); worker body is
real SINTRAN L bytes; the exact `MON 271 → worker` link crosses an uncarved kernel bridge
(`CALLPROC`/`MFELL`) — see [Honest caveats](#honest-caveats). All addresses/values are **octal**.

- **Full disassembly:** [`271B-WriteDiskPage.ASM`](271B-WriteDiskPage.ASM) — the actual code (the shared read/write worker body; MON 271B has no GOTAB entry stub, see below).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 271B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[271B] = 000000<br/>(fall-through, byte proven)"]
    C -.uncarved CALLPROC/MFELL.-> E["WDPAG write worker<br/>006-S3FS :107451B"]
    E --> F["disk page transfer<br/>via JPL I workers"]
    class A blue
    class B,C teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

Unlike its read twin MON 270B (`GOTAB[270B] = 066276B`, a real `F1742` entry stub), MON 271B's GOTAB
word is **zero**: there is no level-14 stub. The dashed hop (`C ⇢ E`) is the resident
`CALLPROC`/`MFELL` second-level dispatch that binds the MON number to the worker at run time — it is
**not present in any carved segment**, so it is the one link that cannot be followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr − loadbase)` in octal words × 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[271] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071524B` (1 word) | 59048 | `GOTAB+271` = `000000` | **VERIFIED** (fall-through) |
| resident CALLPROC/MFELL bridge | — (uncarved) | — | — | `CALLPROC` | **UNVERIFIED** |
| WDPAG write worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) · [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `107451B–110047B` | 50770 | `WDPAG` (`RDPAG`=107447) | real bytes; link **UNVERIFIED** |

**Verify by hand:** `grep '^107451 ' ../../segments-ref/006-S3FS/006-S3FS.hex` → byte offset `50770`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=50770 count=4 | od -An -tx1` → `f8 90 22 27`
(the stored words `174220 021047` = `BSET ONE SSK` / `STD I 47`, the WDPAG write-set entry).
Confirm the fall-through: `dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=59048 count=2 | od -An -tx1`
→ `00 00`; `prove-mon.py 271` reads `GOTAB[271] … 000000`.

---

## Instruction walkthrough

Full listing: [`271B-WriteDiskPage.ASM`](271B-WriteDiskPage.ASM). The functional body is the shared
read/write worker; MON 271B (`WDPAG`) enters at `107451B`, its read twin MON 270B (`RDPAG`) at
`107447B`.

**Read/write entry split (107447–107452)** — `RDPAG` enters at `107447` and clears `SSK` (read);
`WDPAG` (MON 271B) enters one word later at `107451` and **sets** `SSK` (write); one common body
follows.
```
107447  174020  BSET ZRO SSK   ; RDPAG: skip flag = 0 (READ)
107450  124002  JMP  107452    ; jump over the write-set
107451  174220  BSET ONE SSK   ; WDPAG entry (MON 271B): skip flag = 1 (WRITE)
107452  021047  STD I 47       ; save D (double) -> param slot
```
**Prologue (107453–107456)** sets the frame base and calls resident `SPUSH` via pointer `@107522 =
003752B`. **Function-code + directory resolve (107457–107466)** forks on `SSK`: `107462 SAA 61` gives
the **write** device function code into `,B 20`; `107466 JPL I 35` (`@107523 = 030225B`) resolves the
directory index. **Index + reservation checks (107467–107520)** stage the buffer and verify the
directory descriptor + reserved flag, taking indirect error exits on failure. **Page transfer loop
(107531–110035)** scales the page count to words (`107536 SHA ZIN 12` = ×1024, 1 page = 2048 bytes =
1024 words), issues the disk request via `JPL I` workers, advances the 32-bit disk page address per
page (`110026–110034`), and loops while pages remain. **Pointer/data tables (107703–107724,
110036–110047)** hold the `JPL I` targets.

---

## Parameter / register contract

Manual-side names/types are from [`271B_WriteDiskPage.yaml`](../../../../../../../Developer/MON/calls/271B_WriteDiskPage.yaml)
(MAC: `LDT DIRIX` / `LDX (BUFF` / `LDA PAGES` / `COPY SA DD` / `LDA (PAGNO` / `MON 271`).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `107451B` = write; `107447B` = read (shared body, `SSK` split) | VERIFIED (bytes) |
| `SSK` | internal | write (1) / read (0) selector, set at `107451`, tested at `107460` | VERIFIED (bytes) |
| `T` = DirIndex | in | directory index (see GetDirUserIndexes), resolved at `107466` | inferred (manual) |
| `X` = Buffer | in | caller buffer (even byte address, data to write) | inferred (manual) |
| `A` = NoOfPages | in | page count → words via `107536 SHA ZIN 12` | VERIFIED scale; meaning inferred |
| `A` = PageAddr | in | address of the 32-bit destination disk page address | inferred (manual) |
| function code | internal | `061` (write) into `,B 20` | VERIFIED (bytes) |
| error return | out | standard error code on the fault paths | inferred (manual) |

This post-CALLPROC body works through `,B` frame fields, so the precise T/A/X mapping is **inferred**
from the manual and the caller-side `MON 271` wrapper, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`271B-WriteDiskPage.pseudo.c`](271B-WriteDiskPage.pseudo.c)** — a pseudo-C model of the
handler for emulator authors. Control flow, the read/write (`SSK`) fork and the page→word scale are
byte-verified; the disk-driver worker semantics are inferred from the call structure. Every
instruction is translated per the canonical
**[ND-100 Instruction Semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)**.

---

## Honest caveats

**What is byte-proven:** `GOTAB[271B] = 000000` (a fall-through, matching `prove-mon.py 271`); the
`WDPAG` worker entry at `107451B` is real `BSET ONE SSK` (write-select) code and belongs to the
directory-page read/write family (its `SSK` split, its page→word scale `SHA ZIN 12` matching the
manual's 2048-byte page, its `061` write function code).

**What is NOT proven:** the link from the MON number to the `WDPAG` worker. Because `GOTAB[271] =
000000` there is **no** static edge from the level-14 dispatch word into `107451B`; the only path
runs through the resident `CALLPROC`/`MFELL` second-level dispatch, in an **uncarved** overlay
(`commoncode.bin` is zero there, and `025-S3IRPIT` holds no MON-271 stub). So the `MON 271 → WDPAG`
attribution rests on the symbol name + the matching write behaviour, not a followed pointer —
**UNVERIFIED** in the strict sense. This reconciles into one story: the body is genuine, self-
consistent SINTRAN L bytes (its direct branches all close inside the carve), but its *attachment* to
MON 271 is runtime-populated and cannot be confirmed statically. Confirming it needs a live trace:
break at the level-14 MON entry with the number = `271`, single-step the resident fall-through, and
confirm P lands on `WDPAG = 107451B`.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) §9 · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) §G · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
