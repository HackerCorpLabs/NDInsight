# MON 104B (octal) - HOLD (SuspendProgram)

Suspend the calling program / yield the CPU at a chosen priority. Short name `HOLD`.

**Status:** **byte-verified** dispatch + worker body. Worker is coherent code in `026-S3IMPIT`
(the monitor-PIT overlay). All addresses/values **octal**.

> **OVERLAY RESOLVED 2026-07-13.** An intermediate pass flagged this worker as "a data table,
> not code". That was an overlay error: at `040645B` the `003-S3CP` overlay holds a powers-of-ten
> CONSTANT table (`100000000, 10^7 .. 10^0`), used by the neighbouring clock/decimal code. The
> real HOLD worker is in the **monitor-PIT overlay `026-S3IMPIT`** (identical in `017-S3SMPIT`) -
> the view the monitor level actually runs in, since `CALLP` dispatches there. See
> `SINTRAN/CARVING-HANDOFF.md` section 3a and the overlay trap in the `sintran-carving` skill.

- **Full disassembly:** [`104B-SuspendProgram.ASM`](104B-SuspendProgram.ASM).
- **Emulator model:** [`104B-SuspendProgram.pseudo.c`](104B-SuspendProgram.pseudo.c).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 104B"] --> B["ENT14 level-14<br/>026-S3IMPIT :072167B"]
    B --> C["GOTAB[104B] = MFELL<br/>:071543B"]
    C --> D["MFELL level switch<br/>:072114B -> CALLP 032201B"]
    D --> E["MCTAB[104B] = HOLD<br/>044-S3IDPIT :005724B = 040645B"]
    E --> F["HOLD worker<br/>026-S3IMPIT :040645B"]
    class A blue
    class B,C,D,E teal
    class F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

---

## Code location (dispatch path)

Byte offset = `(addr - loadbase)` in octal words x 2 (decimal); reproduced with `dd`.

| Role | Segment | Addr (octal) | Byte offset | Symbol | Verdict |
|------|---------|--------------|-------------|--------|---------|
| MCTAB[104B] slot | [044-S3IDPIT.asm](../../segments-ref/044-S3IDPIT/044-S3IDPIT.asm) | `005724B` = `040645B` | 1960 | -> `HOLD` | **VERIFIED** |
| HOLD worker body | [026-S3IMPIT.asm](../../segments-ref/026-S3IMPIT/026-S3IMPIT.asm) | `040645B-040663B` | 6986 | `HOLD` | **VERIFIED** (code); overlay **inferred** |

**Verify by hand** (from `...\versions\L-VSX-500\segments\`):
```
dd if=044-S3IDPIT.bin bs=1 skip=1960 count=2 | od -An -tx1   ->  41 a5   (= 040645B = HOLD)
dd if=026-S3IMPIT.bin bs=1 skip=6986 count=2 | od -An -tx1   ->  48 11   (= 044021B, HOLD entry LDA 21)
```

---

## Instruction walkthrough

Full listing: [`104B-SuspendProgram.ASM`](104B-SuspendProgram.ASM).

The worker is a **5-way priority-ladder** (`040645B-040655B`): five entry points, each loading a
different priority level into `A` (`21B / 20B / 17B / 16B / 15B`), all jumping to a common tail at
`040656B`. The tail (`040656B-040663B`) sets `B` and `L` from a saved value (`LDT -115 / RADD CLD
ST DB / RADD CLD ST DL`), enables interrupts (`ION`), and `EXIT`s - i.e. it returns control to the
scheduler at the chosen priority. This is the classic shape of a suspend/yield primitive.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | selects the priority level (`21B..15B`) at which to suspend | VERIFIED (bytes) |
| `A` | internal | holds the selected priority, staged for the tail | VERIFIED (bytes) |
| `T` (`,B -115`) | internal | saved base/link value restored into `B` and `L` | VERIFIED (bytes) |
| return | out | via `ION` + `EXIT` (to the scheduler) | VERIFIED (bytes) |

The mapping from the caller's MON-104B arguments to the specific priority entry point is **inferred**
(it is set by the level switch / caller wrapper, not visible in this body).

---

## Pseudo-code (for an emulator)

See **[`104B-SuspendProgram.pseudo.c`](104B-SuspendProgram.pseudo.c)** - the priority-ladder and the
`ION`/`EXIT` tail are byte-verified; the argument-to-priority mapping is inferred.

---

## Honest caveats

**What is byte-proven:** `MCTAB[104B] = 040645B = HOLD`; the worker at `040645B` in `026-S3IMPIT`
is a coherent 5-entry priority ladder ending in `ION`/`EXIT`.

**What is NOT proven:** that `026-S3IMPIT` (as opposed to `017-S3SMPIT`, its byte-identical swap
twin) is the exact overlay the monitor maps for this call - the two are indistinguishable by bytes.
The overlay choice rests on "coherent code + the monitor-PIT architecture (CALLP dispatches on this
level)", not a followed pointer. The `003-S3CP` reading (a powers-of-ten table) is definitively the
WRONG overlay and is retired.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) ·
master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
