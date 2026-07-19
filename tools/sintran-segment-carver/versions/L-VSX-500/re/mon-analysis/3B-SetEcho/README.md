# MON 3B (octal) - ECHOM

Short name `ECHOM`. The old folder glossed MON 3B as "SetEcho"; the carved worker is a
page-table-crossing block-copy routine, so the exact function is left **inferred** (see caveats).

**Status:** **byte-verified** dispatch + worker body (code). Worker is coherent code in
`026-S3IMPIT` (the monitor-PIT overlay). All addresses/values **octal**.

> **OVERLAY RESOLVED 2026-07-13.** An intermediate pass could not read a coherent body: at
> `044540B` the `003-S3CP` overlay holds ASCII command-name text, not code. Wrong overlay. The
> real ECHOM worker is in the **monitor-PIT overlay `026-S3IMPIT`** (identical in `017-S3SMPIT`),
> confirmed by its `MOVEW` block copies bracketed by `BSET ONE/ZRO SSPTM` (the cross-page-table
> switch) - monitor-level machinery that belongs in the monitor-PIT view. See
> `SINTRAN/CARVING-HANDOFF.md` section 3a and the overlay trap in the `sintran-carving` skill.

- **Full disassembly:** [`3B-SetEcho.ASM`](3B-SetEcho.ASM).
- **Emulator model:** [`3B-SetEcho.pseudo.c`](3B-SetEcho.pseudo.c).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 3B"] --> B["ENT14 level-14<br/>026-S3IMPIT :072167B"]
    B --> C["GOTAB[3B] = MFELL<br/>:071236B"]
    C --> D["MFELL level switch<br/>:072114B -> CALLP 032201B"]
    D --> E["MCTAB[3B] = ECHOM<br/>044-S3IDPIT :005623B = 044540B"]
    E --> F["ECHOM worker<br/>026-S3IMPIT :044540B"]
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
| MCTAB[3B] slot | [044-S3IDPIT.asm](../../segments-ref/044-S3IDPIT/044-S3IDPIT.asm) | `005623B` = `044540B` | 1830 | -> `ECHOM` | **VERIFIED** |
| ECHOM worker body | [026-S3IMPIT.asm](../../segments-ref/026-S3IMPIT/026-S3IMPIT.asm) | `044540B-044610B` | 10944 | `ECHOM` | **VERIFIED** (code); overlay **inferred** |

**Verify by hand** (from `...\versions\L-VSX-500\segments\`):
```
dd if=044-S3IDPIT.bin bs=1 skip=1830  count=2 | od -An -tx1   ->  49 60   (= 044540B = ECHOM)
dd if=026-S3IMPIT.bin bs=1 skip=10944 count=2 | od -An -tx1   ->  59 2a   (= 054452B, ECHOM entry LDX ,B 52)
```

---

## Instruction walkthrough

Full listing: [`3B-SetEcho.ASM`](3B-SetEcho.ASM).

The worker performs **three `MOVEW` block moves** (`044545B`, `044561B`, `044576B`) between
frame-relative descriptors, computing source/size operands from frame slots `52/53/62/64` and
calling helpers via `JPL I 30` / `JPL I 11`. The second helper call is bracketed by
`BSET ONE SSPTM` ... `BSET ZRO SSPTM` (`044602B` / `044605B`) - it accesses memory through the
alternate page table, i.e. it copies across PIT boundaries. It returns via `JMP I -117` (`044610B`).

What the three blocks are and the exact effect are **not** decoded here (see caveats).

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `,B 52` | in | source descriptor for the block moves | VERIFIED (bytes) |
| `,B 53` / `,B 62` / `,B 64` | in | size / helper operands | VERIFIED (bytes); meaning inferred |
| `MOVEW` x3 | internal | three block copies, one across the alternate page table (`SSPTM`) | VERIFIED (bytes) |
| return | out | via `JMP I -117` (`-> 044471B`) | VERIFIED (bytes) |
| function | - | manual gloss "SetEcho"; carved code is block-copy, not an echo-mode flag | **INFERRED / unresolved** |

---

## Pseudo-code (for an emulator)

See **[`3B-SetEcho.pseudo.c`](3B-SetEcho.pseudo.c)** - the block-move control flow and the
page-table switch are byte-verified; the semantic purpose is inferred.

---

## Honest caveats

**What is byte-proven:** `MCTAB[3B] = 044540B = ECHOM`; the worker at `044540B` in `026-S3IMPIT`
is a coherent routine doing three `MOVEW` block copies, one across the alternate page table, then
returning.

**What is NOT proven:** (1) that `026-S3IMPIT` vs its byte-identical swap twin `017-S3SMPIT` is the
exact mapped overlay - indistinguishable by bytes; the choice rests on coherent code + monitor-PIT
architecture, not a followed pointer. (2) That this routine is "SetEcho" - the carved body is a
block-copy, which does not match a terminal-echo-flag primitive; the name-to-function mapping is
unresolved and may need the manual entry for MON 3B or a live trace. The `003-S3CP` reading (ASCII
text) is definitively the WRONG overlay and is retired.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) ·
master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
