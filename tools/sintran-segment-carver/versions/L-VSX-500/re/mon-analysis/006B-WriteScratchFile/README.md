# MON 6B (octal) — WriteScratchFile (WDISK)

Writes one block to the scratch file into/from a caller-supplied buffer. It shares one code
body with MON 5B ReadScratchFile (RDISK); the two enter one word apart and the body forks on a
read/write skip flag (`SSK`). MON 6B is the **write** entry: it sets `SSK` at `102023B`.

**Status:** dispatch head byte-proven as a **fall-through** (`GOTAB[6B] = 000000`); worker body
is real SINTRAN L bytes; the exact `MON 6 → worker` link crosses an uncarved kernel bridge
(`CALLPROC`/`MFELL`) — see [Honest caveats](#honest-caveats). All addresses/values are **octal**.

- **Full disassembly:** [`006B-WriteScratchFile.ASM`](006B-WriteScratchFile.ASM) — the actual code (the shared read/write worker body; MON 6B has no GOTAB entry stub, see below).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 6B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[6B] = 000000<br/>(fall-through, byte proven)"]
    C -.uncarved CALLPROC/MFELL.-> E["WDISK write worker<br/>006-S3FS :102023B"]
    E --> F["fs read/write block<br/>via JPL I workers"]
    class A blue
    class B,C teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

Unlike its read twin MON 5B (`GOTAB[5B] = 120355B`, a real `F1611` entry stub), MON 6B's GOTAB
word is **zero**: there is no level-14 stub. The dashed hop (`C ⇢ E`) is the resident
`CALLPROC`/`MFELL` second-level dispatch that binds the MON number to the worker at run time — it
is **not present in any carved segment**, so it is the one link that cannot be followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr − loadbase)` in octal words × 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[6] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071241B` (1 word) | 58690 | `GOTAB+6` = `000000` | **VERIFIED** (fall-through) |
| resident CALLPROC/MFELL bridge | — (uncarved) | — | — | `CALLPROC` | **UNVERIFIED** |
| WDISK write worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) · [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `102023B–102173B` | 45094 | `WDISK` | real bytes; link **UNVERIFIED** |

**Verify by hand:** `grep '^102023' ../../segments-ref/006-S3FS/006-S3FS.hex` → byte offset `45094`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=45094 count=8 | od -An -tx1` → `f8 90 22 3b cc 65 …`
(= octal `174220 021073 …` = `BSET ONE SSK` / `STD I 73`, the WDISK write-set entry).
Confirm the fall-through: `python3 ../../../../../../../scripts/prove-mon.py 006` → `GOTAB[6] … 000000`.

---

## Instruction walkthrough

Full listing: [`006B-WriteScratchFile.ASM`](006B-WriteScratchFile.ASM). The functional body is the
shared read/write worker; MON 6B (`WDISK`) enters at `102023B`, its read twin MON 5B (`RDISK`) at
`102021B`.

**Read/write entry split (102021–102024)** — RDISK enters at `102021` and clears `SSK` (read);
WDISK (MON 6B) enters one word later at `102023` and **sets** `SSK` (write). One common body follows.
```
102021  174020  BSET ZRO SSK   ; RDISK: skip flag = 0 (READ)
102022  124002  JMP  102024    ; jump over the write-set
102023  174220  BSET ONE SSK   ; WDISK entry (MON 6B): skip flag = 1 (WRITE)
102024  021073  STD I 73       ; save D (double) -> param slot
```
**Prologue (102024–102031)** stages caller args; `102030 JPL I 70` calls the resident FS prologue
`SPUSH` via pointer word `@102120 = 003752B` (register save).
**Classification loop (102032–102060)** scans a 2-word/entry table (`AAX 2` / `JMP 102032`), using
`102040 BSKP ONE SSK` to pick the write-side (`,X 1`) vs read-side (`,X 0`) descriptor field, and
testing attribute bits with masked `BSKP` skips until a match.
**Position compute + fork (102060–102116)** computes the on-disk position (`SUB 40` / `SHA SHR 1` /
`AAA 100` = address/size scaling), then forks on `SSK` at `102103 BSKP ONE SSK`: with `SSK = 1`
(write) it executes `102105 JPL I 20` (write worker `@102125 = 100130B` = `FWRT`); with `SSK = 0`
(read) it takes `102104 JMP → 102110 JPL I 16` (read worker `@102126 = 077542B` = `FREA`). Both
exit through the tail `102114 JMP I 13` (`@102127 = 003776B` = `SPOP`, restore + return); the
guard/error exit funnels through `102115 STA ,B 2` (status into frame field 2).
**Pointer table (102117–102127)** — data, the address constants used by the `JPL I`/`JMP I` above:
`@102120=003752 (SPUSH) @102124=033740 (B4INW/ATMUL, ambiguous) @102125=100130 (FWRT)
@102126=077542 (FREA) @102127=003776 (SPOP)`. The four `147xxx` words at `102117/121/122/123` are
unreferenced.
**Adjacent RFILE/WFILE body (102130–102173)** is pulled in by the carve's control-flow closure of
the pointer-table span, not by any direct branch from the WDISK body; its indirect targets
(`102312/102314/102315`) lie past the carved window.

---

## Parameter / register contract

This body runs **after** `CALLPROC`, so it operates on the process/file data-frame via `,B` fields,
not on the raw MON `T/X/A` registers.

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `102023B` = write; `102021B` = read (shared body, `SSK` split) | VERIFIED (bytes) |
| `SSK` | internal | write (1) / read (0) selector, set at `102023`, tested at `102040/102103/102152` | VERIFIED (bytes) |
| `D` (double) | in | caller parameter saved first (`102024 STD I 73`) — block/descriptor | inferred |
| `,B 0`,`,B 1`,`,B 5` | in | data-frame fields read at `102066/102100/102102` (buffer / block descriptor) | VERIFIED read; meaning inferred |
| `,B 2` | out | result/error stored at `102115 STA ,B 2` | VERIFIED written; meaning inferred |
| `,B 4` | in/out | `102112 MIN ,B 4` (counter bump) | VERIFIED bumped; meaning inferred |
| `A := 137` | out | error/count constant on the guard-fail exit (`102035`) | VERIFIED value; meaning inferred |
| `FWRT` (100130) | call | file-write worker (WDISK path), via ptr `102125` + `SSK` select | **VERIFIED** |
| `FREA` (077542) | call | file-read worker (RDISK path), via ptr `102126` | **VERIFIED** |
| `SPUSH`/`SPOP` (003752/003776) | — | register save/restore around the body | VERIFIED (ptr table) |
| `T`=block no., `X`=buffer, `A`=error out | in/out | classic WDISK MON contract | **UNVERIFIED here** — from README/manual; consumed at the MON entry before `CALLPROC` |

The exact user-visible register convention lives in the caller-side `MON 6` wrapper and the
uncarved `CALLPROC` frame, so the precise A/X/T assignment is **inferred**, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`006B-WriteScratchFile.pseudo.c`](006B-WriteScratchFile.pseudo.c)** — a pseudo-C model of the
handler for emulator authors. Control flow + the read/write (`SSK`) fork are byte-verified; the
file-system worker semantics and the `,B` frame-field meanings are inferred from the call structure.
Every instruction is translated per the canonical
**[ND-100 Instruction Semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)**.

---

## Honest caveats

**What is byte-proven:** `GOTAB[6B] = 000000` (a fall-through, matching `prove-mon.py 006` reading
the running-confirmed resident `commoncode.bin`); the `WDISK` worker entry at `102023B` is real
`BSET ONE SSK` (write-select) code and belongs to the scratch read/write family (its `SSK` split);
the read/write decision at `102103 BSKP ONE SSK` dispatching to `FWRT` (write) vs `FREA` (read) via
the pointer table; `SPUSH`/`SPOP` prologue/epilogue.

**What is NOT proven:** the link from the MON number to the `WDISK` worker (in `006-S3FS`). Because
`GOTAB[6] = 000000` there is **no** static edge from the level-14 dispatch word into `102023B`; the
only path runs through the resident `CALLPROC`/`MFELL` second-level dispatch, which lives in an
**uncarved** overlay (`commoncode.bin` is zero in that region, and `025-S3IRPIT` holds no MON-6
stub). So the `MON 6 → WDISK` attribution rests on the symbol name + the matching write behaviour,
not a followed pointer — **UNVERIFIED** in the strict sense. This reconciles the two source notes:
the body is genuine, self-consistent SINTRAN L bytes (its direct branches all close inside the
carve), but its *attachment* to MON 6 is runtime-populated and cannot be confirmed statically.
Confirming it needs a live trace: break at the level-14 MON entry with the number = `006`,
single-step the resident `CALLPROC`/`MFELL` fall-through, and confirm P lands on `WDISK = 102023B`.

Also unverified: `033740` (helper at ptr `102124`) is ambiguous between `B4INW` and `ATMUL`; the
adjacent `RFILE/WFILE` body (`102130–102173`) is in the carve by pointer-span closure, not by a
proven branch from `WDISK`; the read twin `RDISK`'s "sets `SSK=0`" is inferred from the shared body.

---

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
