# MON 425B (octal) - SetProcessName (SPRNAM)

Defines a new name for the calling ND-500 process (up to 16 chars plus an optional
`(user)` prefix). It is an **ND-500 monitor call** dispatched through the S3SM5 numeric
vector table - **but its vector slot is empty**, so no handler code exists in the carved
`030-S3SM5` segment.

**Status:** the empty vector slot is **byte-proven** (slot `0x028a` = `0x0000`); the actual
servicing point is **NOT LOCATED / UNVERIFIED**. All addresses/values are **octal** unless a
`0x` prefix or `(dec)` marks them hex/decimal.

- **No code body:** this call has no worker in any carved segment - there is **no `.ASM` and
  no `.pseudo.c`** here on purpose. Fabricating one would be wrong; the table below documents
  the absence instead.
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/)
  (segment `030-S3SM5`, load base `40000B`).

---

## Dispatch path

```mermaid
flowchart LR
    A["ND-500 program<br/>CALLG SetProcessName,1,name<br/>(37B9 + 425B)"] --> B["ND-500 monitor entry<br/>MCNO = 425B"]
    B --> C["S3SM5 0x60 vector table<br/>slot 0x028a = 40505B"]
    C -.slot = 0x0000, no handler.-> D["serviced elsewhere<br/>(ND-100 back-end, UNCONFIRMED)"]
    class A blue
    class B,C teal
    class D green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The dashed hop (`C ⇢ D`) is where a real call would jump to its handler word. For MON 425B
that word is **`0x0000`**, so no jump target exists inside `030-S3SM5`. Where the call is
actually serviced is not carved here (see [Honest caveats](#honest-caveats)).

---

## Code location (dispatch path)

`030-S3SM5` holds a numeric MON-dispatch vector table based at byte offset `0x60`. The slot
for MON number `N` is at byte `0x60 + 2*N`. Byte offset -> segment word address = load base
`40000B` + `(offset / 2)` words.

| Role | Segment (full disasm) | Addr / slot (octal) | Byte offset (dec) | Symbol | Verdict |
|------|------------------------|---------------------|-------------------|--------|---------|
| ND-500 monitor entry (numeric dispatch, `MCNO=425B`) | - (uncarved ND-500 resident) | n/a | n/a | ND-500 MON entry | **UNVERIFIED** |
| S3SM5 `0x60` vector slot for 425B | [030-S3SM5.asm](../../segments-ref/030-S3SM5/030-S3SM5.asm) · [.hex](../../segments-ref/030-S3SM5/030-S3SM5.hex) | word `40505B` (slot `0x028a`) | 650 | (empty) | **VERIFIED empty (`0x0000`)** |
| Worker body | none in `030-S3SM5` | n/a | n/a | expected ND-100 back-end (not located) | **UNVERIFIED** |

Slot byte offset = `0x60 + 2*425B` = `96 + 554 = 650 (dec)` = `0x028a`. Canonical binary:
`../../../segments/030-S3SM5.bin`.

**Verify by hand:** `grep '^650 ' ../../segments-ref/030-S3SM5/030-S3SM5.hex` -> byte `650  000`
(and `651  000`); then
`dd if=../../../segments/030-S3SM5.bin bs=1 skip=650 count=2 2>/dev/null | od -An -tx1` -> `00 00`
(the empty slot). For contrast, the populated MON 410B slot at byte `624` reads `ba e1`.

---

## Instruction walkthrough

There are **no instructions to walk** - the vector slot is data (`0x0000`), not a jump target,
so no worker body was carved and there is no `.ASM`.

What the dispatch would do for a *populated* neighbour: the ND-500 monitor takes the MON number,
indexes the `0x60` table, and transfers to the 16-bit handler word found there. Slots `410B-421B`
hold real handler words (e.g. `410B -> ba e1`, `411B -> bb 38`); slots **`422B-427B` are all
`0x0000`**, i.e. no S3SM5 handler for the `sprname / gprnum / gprname` family. MON 425B is one of
those empty slots.

Vector-table dump (byte offsets, big-endian words) for the fix/process family:

```
410B off 624 = ba e1     420B off 640 = be 0f
411B off 626 = bb 38     421B off 642 = bf cf
412B off 628 = 98 dd     422B off 644 = 00 00
413B off 630 = bb 73     423B off 646 = 00 00
414B off 632 = bb 9e     424B off 648 = 00 00
415B off 634 = bc 20     425B off 650 = 00 00   <-- SetProcessName
416B off 636 = bd 70     426B off 652 = 00 00
417B off 638 = bd f6     427B off 654 = 00 00
```

---

## Parameter / register contract

From the manual (ND-860228.2 EN, p.461); **not** byte-proven here because no handler was located.

| Field | Dir | Meaning | Verdict |
|-------|-----|---------|---------|
| call form | in | ND-500 `CALLG SetProcessName, 1, ProcessName` (`37B9 + 425B`) | manual (inferred) |
| `ProcessName` | in | `STRING`, new process name, up to 34 chars (max 16 + optional `(user)` prefix) | manual (inferred) |
| availability | - | ND-500 only; not callable from ND-100 / user / RT / system programs | manual (inferred) |
| handler slot | - | S3SM5 vector `0x028a` = `0x0000` (no ND-500 handler) | **VERIFIED (bytes)** |

Full YAML contract: `Developer/MON/calls/425B_SetProcessName.yaml`.

---

## Honest caveats

**What is byte-proven:** the S3SM5 `0x60` vector slot for MON 425B, at byte offset `650`
(segment word `40505B`, slot `0x028a`), reads **`0x0000`** in the real SINTRAN L bytes of
`030-S3SM5.bin`. The whole `422B-427B` slot run is `0x0000`, while `410B-421B` hold non-zero
handler words - so the empty slot is a genuine "no S3SM5 handler", not a carve artefact.

**What is NOT proven:** where MON 425B is actually serviced. A `0x0000` slot means "not routed
to S3SM5"; the manual marks the call ND-500-only, and the expected back-end is an ND-100-side
companion (documented short name `SPRNAM`), but **that handler has not been located or carved**.
The earlier note calling it `SPRNA` was a truncation; the manual symbol is `SPRNAM`. Confirming
the real dispatch needs a live trace of an ND-500 `SetProcessName` call, or locating the ND-100
back-end that consumes it. Until then this folder correctly holds **no code**.

This reconciles the two prior claims into one story: the *slot* is verified empty (DISPATCH
finding), and the *worker* is unverified/absent (ANALYSIS open item) - consistent, not contradictory.

---

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) (item E4) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
