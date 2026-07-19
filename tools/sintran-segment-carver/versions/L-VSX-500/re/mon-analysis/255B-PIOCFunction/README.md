# MON 255B (octal) - PIOCFunction (PIOCM)

Parallel-I/O-Controller call: the caller selects a PIOC sub-function (reserve /
release a slot, send / read a message, load / unload a segment, start / stop the
PIOC) by a function number. The carved worker `PIOCM` validates the function
code, then dispatches through a cross-bank jump table to the per-function
handler. The PIOC itself is an MC68000-based front-end used for X.25 / Ethernet
data communication.

**Status:** dispatch word byte-proven (`GOTAB[255B] = 000000`, a level-14
fall-through); the worker body `PIOCM = 114120B` is real SINTRAN L bytes and is
control-flow closed; the `MON 255 -> PIOCM` hop crosses the uncarved resident
`CALLPROC`/`MFELL` bridge (see [Honest caveats](#honest-caveats)). Identity is
strong: the routine returns exactly the manual's status codes `-24B` (illegal
function code) and `-32B` (illegal LDN). All addresses/values are **octal**.

- **Full disassembly:** [`255B-PIOCFunction.ASM`](255B-PIOCFunction.ASM) - the actual code (the `PIOCM` worker body; there is no F17xx entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 255B<br/>FuncNo in T"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[255B] = 000000<br/>(byte-proven, fall-through)"]
    C -.uncarved CALLPROC.-> D["PIOCM worker<br/>025-S3IRPIT :114120B"]
    D --> E["validate FuncNo<br/>range checks + -24B/-32B"]
    E --> F["cross-bank jump table<br/>114170-114174 -> handler"]
    F --> G["PIRET epilogue<br/>Status -> caller, IOF"]
    class A blue
    class B,C teal
    class D,E,F green
    class G green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

A zero GOTAB slot **is** the fall-through marker: there is no per-call `F17xx`
stub. The dashed hop (`C ⇢ D`) is the resident `CALLPROC` second-level dispatch
that maps the fallen-through MON number onto its worker - it is **not present in
any carved segment**, so it is the one link that cannot be followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr − loadbase)` in
octal words × 2 (decimal). Load bases: `SINTRAN-DATA_commoncode` = `0B`,
`025-S3IRPIT` = `32000B`.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[255] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071510B` (1 word) | 59024 | `GOTAB+255` = `000000` | **VERIFIED** |
| resident CALLPROC bridge | — (uncarved) | — | — | `CALLPROC` / `MFELL` | **UNVERIFIED** |
| PIOCM worker body | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `114120B–114241B` | 51360 | `PIOCM` (SYMBOL-2-LIST) | real bytes; link **partial** |

`071510B = 071233B (GOTAB base) + 255B`. The internal labels `OPD65` (114123B),
`OPD66` (114135B), `OPD67` (114147B), `OPD77` (114167B) and `PIRET` (114207B) all
sit inside the single contiguous `114120B..114241B` region; the carve is bounded
below by the next distinct symbol `EXEL = 114242B`.

**Verify by hand (GOTAB word):** `grep '^71510 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
→ byte offset `59024`, value `000000`; then
```
dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=59024 count=2 2>/dev/null | od -An -tx1
# -> 00 00   (the two zero bytes compose octal word 000000 = GOTAB[255] = fall-through)
```
**Verify by hand (PIOCM entry `114120B`):** `grep '^114120 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex`
→ byte offset `51360`, word `015076` (`032 076` per the hex columns); then
```
grep -n 'PIOCM' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.symbols.txt
dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=51360 count=2 2>/dev/null | od -An -tx1
# -> 1a 3e   (the two bytes compose octal word 015076 = STX I 76, the PIOCM entry)
```
**Segment tie-breaker:** the same `PIOCM=114120B` in the sibling overlay is data,
not code — `grep '^114120 ' ../../segments-ref/026-S3IMPIT/026-S3IMPIT.hex` →
word `000000` (the start of a repeating 8-word float/data record), confirming
`025-S3IRPIT` as the code overlay.

---

## Instruction walkthrough

Full listing: [`255B-PIOCFunction.ASM`](255B-PIOCFunction.ASM). All addresses
octal; `X` = caller parameter-block base, `B` = device/data-field base (roles
inferred from the access pattern). Words `114216B–114241B` are a **pointer-word /
save-cell / data table** — `nd100-dis` renders them as bogus instructions because
it cannot tell data from code; every one is a pointer or save cell referenced by
the code above (resolved in the `.ASM` comments), or trailing padding.

**Entry prologue (114120–114122)** — `STX I 76` saves caller `X` through the
resident cell `007253B`; `RADD CLD SB DA` copies `B` into `A`; `STA I 75` saves
it through `007254B`.

**Function/slot pre-checks (114123–114146)** — reads the caller word `,X 11`; a
value of `-1` fast-paths to the sibling routine `EXEL` (`JMP I 71 -> 114242B`).
Otherwise it hands `,X 10` to a resident helper (`JPL I 70 -> 010376B`), and on a
zero/failed result stores status `-32B` (illegal LDN) and exits via `PIRET`. A
second gate requires the device-state word `,B 12` to equal `173B`, else `-32B`.

**Function-code range split (114147–114167)** — re-reads the function code `,X
11`; codes `>= 20B` divert to the alternate path at `114175`. In the low range it
enables the PIOC interrupt bits (`SAA 4 ; MST PIE`), re-bases the block
(`LDX I 35`), and bounds the code to `10B` (`SAT 10 ; SKP IF DT MLST SA`, an
**unsigned** compare); out of range stores `-24B` (illegal function code) and
exits via `PIRET`.

**Cross-bank jump-table dispatch — the core (114170–114174)** — `BSET ZRO SSPTM`
selects the base bank (`*1BANK`), `RADD CLD SA DX` puts the scaled function code
in `X`, `LDA I ,X 34` reads the sub-function handler address from the table based
at `114106B`, `BSET ONE SSPTM` restores the alternate bank (`*2BANK`), and
`RADD CLD SA DP` (`P := A`) performs the computed jump to the handler.
**VERIFIED (bytes).**

**Alternate range path (114175–114206)** — `AAA -20` rebases the high code, a
signed bound (`SAT 2 ; SKP IF DT LST SA`) rejects out-of-range with `-24B`, and a
computed relative jump (`RADD SA DP`, `P := P + A`) selects one of three resident
sub-workers (`115532B / 115527B / 115642B`).

**PIRET common return (114207–114215)** — restores the return block base
(`LDX I 7`), publishes the status word into the caller slot (`STA ,B 11`), turns
interrupts off (`IOF`) to serialise the driver hand-off, then calls two resident
finalisers (`JPL I 17 -> 010610B`, `-> 010341B`).

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `T` | in | Function number `0..7` (Reserve/Release/Send/Read/Load/Unload/Start/Stop) | inferred (manual/YAML; caller-side, marshalled before PIOCM) |
| `,X 11` | in | Function/slot code the worker actually tests and dispatches on | VERIFIED (114123, 114147, 114162) |
| `,X 10` | in | Second parameter word handed to a resident helper | VERIFIED (114130) |
| `,B 12` | in | Device-state word gated against `173B` | VERIFIED (114141–114143) |
| `X` (caller) | in | Base of the marshalled parameter block | VERIFIED (bytes); caller meaning inferred |
| `,B 11` | out | Status word published to the caller (`STA ,B 11`) | VERIFIED (114212) |
| status `-24B` | out | Illegal function code (`SAA -24`) — matches the manual | VERIFIED (114166, 114201) |
| status `-32B` | out | Illegal LDN (`SAA -32`) — matches the manual | VERIFIED (114135, 114145) |
| jump-table (114106B) | work | Base of the cross-bank per-function handler table | VERIFIED (114172) |

The caller-visible convention (FuncNo in `T`, plus `DeviceNo` / `SlotNo` /
`Message` / `SegNo` / `PageNo`) lives in the caller-side `MON 255` wrapper and the
uncarved `CALLPROC` frame; the precise register-to-slot marshalling is inferred
from the code shape, not byte-proven in this window. Full contract:
[`Developer/MON/calls/255B_PIOCCFunction.yaml`](../../../../../../../Developer/MON/calls/255B_PIOCCFunction.yaml).

---

## Pseudo-code (for an emulator)

See **[`255B-PIOCFunction.pseudo.c`](255B-PIOCFunction.pseudo.c)** — a pseudo-C
model of the `PIOCM` worker for emulator authors. Control flow, the function-code
range checks, the cross-bank jump-table dispatch, and the two documented error
codes are byte-verified; the descriptor-field meanings and the caller-register
marshalling are inferred from the call structure. Every instruction in the
`.pseudo.c` is translated against the canonical
[`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(`RADD CLD Ss Dd` = COPY `d := s`; `RADD Ss Dd` = `d := d + s`; bare `LDA`/`LDT`
`disp` = P-relative `mem[P+disp]`, not literals; `SKP`/`BSKP` skip polarity;
`LDA I ,X d` = `mem[X + mem[P+d]]`; `BSET ZRO/ONE SSPTM` = clear/set the bank-select
STS bit; `MST PIE` = `PIE |= A`).

---

## Honest caveats

**What is byte-proven:** `GOTAB[255B] = 000000` (level-14 fall-through, read
straight from `commoncode.bin` — so 255 is NOT a directly-vectored call). The
`PIOCM` worker entry at `114120B` is real code whose first word is `015076`
(`STX I 76`); the routine is control-flow closed inside `114120B..114241B` (every
direct branch lands in-window; all `JMP I`/`JPL I` go through the in-window
link-cell table); it validates a function code, dispatches through a cross-bank
jump table, and returns the manual's `-24B`/`-32B` status codes.

**Segment identity (the tie-breaker):** the symbol `PIOCM = 114120B` from
`SYMBOL-2-LIST` maps into two overlays that both load at `32000B`. In
`025-S3IRPIT` the bytes are coherent ND-100 code (the routine above). In
`026-S3IMPIT` the same address is the start of a repeating 8-word data record
(`000000 000000 177777 ...`), i.e. float/data garbage — so `025-S3IRPIT` is the
code overlay. This is the same coherent-vs-garbage discriminator used for 32B and
263B.

**What is NOT proven (single clear story):** the link from the fall-through
dispatch to the `PIOCM` worker. `GOTAB[255]` is zero, so there is no stored
address to follow; the resident `CALLPROC`/`MFELL` that selects the worker for a
fallen-through MON number lives in an **uncarved overlay** and cannot be read
from any carved segment. So the `MON 255 -> PIOCM` attribution rests on the symbol
name plus the behaviour (a PIOC function dispatcher that returns the documented
PIOC status codes), not a followed pointer — hence the link is **partial**, not
byte-proven. The handler addresses reached through the jump table (`114106B`
base) and the computed relative jump (`115532B`/`115527B`/`115642B`) live outside
the carved window and are not resolvable from these bytes.

Confirming the link needs a live trace: break at the resident `CALLPROC` entry on
a real `MON 255`, single-step the second-level dispatch, and confirm P lands on
`PIOCM = 114120B` (mapped through the `025-S3IRPIT` load base `32000B`).

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
