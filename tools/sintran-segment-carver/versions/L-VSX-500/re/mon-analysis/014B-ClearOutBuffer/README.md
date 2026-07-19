# MON 014B (octal) - ClearOutBuffer (COBUF)

Clears a device's output (ring) buffer. It shares one code body with MON 013B ClearInBuffer
(CIBUF); the two enter five words apart (`CIBUF=044120B`, `COBUF=044125B`) and the body forks
on which datafield pointer cell it clears.

**Status:** dispatch head byte-proven as fall-through (`GOTAB[14B]=000000`); worker body is real
SINTRAN L bytes; the `MON 14 -> COBUF` link crosses an uncarved kernel bridge (see
[Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`014B-ClearOutBuffer.ASM`](014B-ClearOutBuffer.ASM) - the actual code (the shared CIBUF/COBUF worker).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 14B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[14B] = 000000<br/>(byte-proven fall-through)"]
    C --> D["MFELL / monitor level"]
    D -.uncarved CALLPROC.-> E["COBUF clear-buffer worker<br/>commoncode :044125B"]
    E --> F["clear output-buffer pointer<br/>via JPL I @044222"]
    class A blue
    class B,C,D teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The dashed hop (`D ⇢ E`) is the resident `MFELL`/`CALLPROC` fall-through dispatch - it is **not
present in any carved segment**, so it is the one link that cannot be followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr − loadbase)` in octal words × 2.
`GOTAB` base is `071233B`; `commoncode` load base is `0`.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[14] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071247B` (1 word) | 58702 | `GOTAB+14` = `000000` | **VERIFIED** (fall-through) |
| MFELL / resident CALLPROC bridge | — (uncarved) | — | — | `MFELL` / `CALLPROC` | **UNVERIFIED** |
| COBUF clear-buffer worker body | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `044113B–044156B` (entry `044125B`) | 37014 (entry 37034) | `COBUF` | real bytes; link **MISATTRIBUTED** |

**Verify by hand:** `grep '^44125 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
→ `44125  070073  160 073  37034`; then
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=37034 count=2 | od -An -tx1` → `70 3b`
(= octal `070073`, `AND 73`, the `COBUF` entry). The `GOTAB[14]` word at byte 58702 reads `00 00`
= `000000` (the fall-through).

---

## Instruction walkthrough

Full listing: [`014B-ClearOutBuffer.ASM`](014B-ClearOutBuffer.ASM). `CIBUF` (013B) and `COBUF`
(014B) are labels **inside one loop**; the loop top is `044114B`.

**Shared prologue / loop reload (044113–044117)** — `LDX ,B -200` sets the datafield base,
`LDD ,X 0` fetches a two-word buffer descriptor under a `POF`/`PON` critical section, `SAT -1`
loads the sentinel into T.

**CIBUF entry — input path (044120–044124)** — `044120 SKP IF DA UEQ ST` compares the
descriptor to the sentinel: on equality `044121 JMP -> 044152` finalizes; otherwise a masked
`BSKP` routes to the input-buffer block, falling into COBUF.

**COBUF entry — output path (044125–044136)** — `044125 AND 73` / `044126 LDT 73` mask the
buffer control word; if the buffer is non-empty (`044127 MLST ST`) the output-buffer pointer at
datafield `-54` is adjusted (`ADD ,B -54` / `STA ,B -54`) and the shared drain worker is called
via `044135 JPL I 65` (indirect through the pointer cell at `044222`, outside the carved window).

**Input-buffer clear block (044137–044147)** — structurally identical, operating on datafield
`-16` with mask `56`; this is the `CIBUF`/MON 013B body. Both blocks share the loop tail.

**Loop tail (044150–044151)** — `AAX 2` steps X to the next descriptor, `JMP -> 044114` reloads.

**Finalize / exit (044152–044156)** — reached from `044121`; writes back exit state
(`STA ,X 14`) and continues just past the carved window. No direct branch escapes the window;
the only outbound transfers are the two indirect `JPL I` worker calls through `044222`.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `044125B` = output (COBUF); `044120B` = input (CIBUF) — shared body | VERIFIED (bytes) |
| device datafield | in | B-relative base (`LDX ,B -200`); output ptr `-54`, input ptr `-16` | VERIFIED (bytes) |
| buffer descriptor | internal | two words per `LDD ,X 0`; X advanced by 2 each iteration | VERIFIED (bytes) |
| mask / sentinel | internal | output mask `73`, input mask `56`; sentinel `-1` in T (`SAT -1`) | VERIFIED (bytes) |
| shared drain worker | out | `JPL I` through pointer cell at `044222` (outside window) | call VERIFIED; target inferred |
| caller device number reg | in | which register carries the logical device number on entry | UNVERIFIED (CALLPROC glue, not traced) |
| return status / error `153B` | out | manual documents error `153B` (illegal output buffer) | UNVERIFIED (no status store visible here) |
| skip-return | out | no MON-prologue skip-return in this shared body (reached via CALLPROC) | VERIFIED absent here |

The exact user-visible register convention lives in the caller-side `MON 14` wrapper and the
uncarved CALLPROC frame, so the precise A/X/T assignment is **inferred**, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`014B-ClearOutBuffer.pseudo.c`](014B-ClearOutBuffer.pseudo.c)** - a pseudo-C model of the
handler for emulator authors. The scan loop and the CIBUF/COBUF entry fork are byte-verified; the
datafield-cell labels and the drain-worker semantics are inferred from context.

Every instruction in the `.pseudo.c` is translated against the canonical
[`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(bare `AND`/`LDT disp` = P-relative `mem[P+disp]`, not literals - both `AND` operands here resolve
to `mem[044220B]`, out of carve; `SKP`/`BSKP` skip polarity; `RADD CLD SD DA` = `A = D`).

---

## Honest caveats

**What is byte-proven:** `GOTAB[14B] = 000000` (level-14 dispatch is fall-through, read directly
from `commoncode.bin` byte 58702 = `00 00`); the `COBUF` worker entry bytes at `044125B` (`070073`,
`AND 73`) and the whole `044113B–044156B` body match the disassembly; `CIBUF`/`COBUF` are two
labelled entries in one shared clear-buffer routine.

**What is NOT proven:** the link from MON 014B to the `COBUF` body. `GOTAB[14]` is zero, so there
is no stub address to follow; the fall-through hands off to the resident `MFELL`/`CALLPROC`
second-level dispatcher, which lives in an **uncarved overlay**. The `MON 14 → COBUF` attribution
therefore rests on the symbol name (`COBUF` = "Clear Output BUFfer", from `SYMBOL-1-LIST`) plus the
matching ring-buffer-clear behaviour, not a followed pointer — hence **MISATTRIBUTED** in the
strict sense. The earlier note that placed this handler in `116-S3SERWD.bin` was wrong: that carve
is a data/text save segment; the real L code is resident in `commoncode.bin` at load 0. Confirming
the dispatch needs a live trace (break at the level-14 entry with T=014B, single-step the
fall-through, confirm P lands on `044125B`).

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
