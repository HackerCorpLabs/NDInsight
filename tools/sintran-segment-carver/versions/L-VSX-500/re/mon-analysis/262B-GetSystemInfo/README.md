# MON 262B (octal) - GetSystemInfo (CPUST)

Returns system information into a caller-supplied **12-word (24-byte) buffer**: the system
number, the **CPU type**, the **SINTRAN III version**, the **instruction set**, the **patch
indicator**, and the **system generation time**. The input parameter `Number` should always be
`0`. This is an ND-100 monitor call.

**Status:** **byte-verified** (dispatch + worker body). The worker is real carved code in
`006-S3FS`. All addresses/values are **octal**.

> **CORRECTED 2026-07-13.** The previous version of this folder said the call was
> `misattributed`, claiming `GOTAB[262B] = 066262B` routed to an `F1737` "compiler dispatch
> descriptor" in `025-S3IRPIT`, and that the `F1737 -> CPUST` hop was unproven. **That was an
> artefact of the wrong dispatch model.** MON calls are not dispatched through that `GOTAB`
> (it is `MFELL` for 224 of 256 calls); the real worker address comes from **`MCTAB @ 005620B`**
> in segment `044-S3IDPIT`. `MCTAB[262B] = 063022B = CPUST`, byte-proven, and that worker IS
> carved in `006-S3FS`. The old `066262B`/`F1737` reading came from the disproven "uncarved
> CALLPROC bridge" story. See [`../317B-ExecuteCommand/README.md`](../317B-ExecuteCommand/README.md)
> and `SINTRAN/CARVING-HANDOFF.md` section 3a.

- **Full disassembly:** [`262B-GetSystemInfo.ASM`](262B-GetSystemInfo.ASM).
- **Emulator model:** [`262B-GetSystemInfo.pseudo.c`](262B-GetSystemInfo.pseudo.c).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 262B"] --> B["ENT14 level-14 entry<br/>026-S3IMPIT :072167B"]
    B --> C["GOTAB[262B] = MFELL<br/>MGOTA=071233B :071515B"]
    C --> D["MFELL level switch<br/>026-S3IMPIT :072114B -> CALLP 032201B"]
    D --> E["MCTAB[262B] = CPUST<br/>MCTAB=005620B :006102B = 063022B"]
    E --> F["CPUST worker<br/>006-S3FS :063022B"]
    class A blue
    class B,C,D,E teal
    class F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

There is **no dashed hop**. `GOTAB[262B] = MFELL` (byte-proven, `74 4c`); `MFELL` is a
program-**level** switch (it writes `CALLP = 032201B` into the monitor level's `P`), not a
subroutine call. The monitor level then indexes `MCTAB` and reaches `CPUST`.

---

## Code location (dispatch path)

Byte offset = `(addr - loadbase)` in octal words x 2 (decimal). Every offset below was
reproduced with `dd` (see "Verify by hand").

| Role | Segment | Addr (octal) | Byte offset | Symbol | Verdict |
|------|---------|--------------|-------------|--------|---------|
| GOTAB[262B] slot | [026-S3IMPIT.asm](../../segments-ref/026-S3IMPIT/026-S3IMPIT.asm) | `071515B` = `072114B` | 32410 | -> `MFELL` | **VERIFIED** |
| MCTAB[262B] slot | [044-S3IDPIT.asm](../../segments-ref/044-S3IDPIT/044-S3IDPIT.asm) | `006102B` = `063022B` | 2180 | -> `CPUST` | **VERIFIED** |
| CPUST worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) | `063022B-063301B` | 29732 | `CPUST` | **VERIFIED** |

**Verify by hand** (from `tools/sintran-segment-carver/versions/L-VSX-500/segments/`):
```
dd if=026-S3IMPIT.bin bs=1 skip=32410 count=2 | od -An -tx1   ->  74 4c   (= 072114B = MFELL)
dd if=044-S3IDPIT.bin bs=1 skip=2180  count=2 | od -An -tx1   ->  66 12   (= 063022B = CPUST)
dd if=006-S3FS.bin    bs=1 skip=29732 count=2 | od -An -tx1   ->  22 4e   (= 021116B, CPUST entry: STD I 116)
```

---

## Instruction walkthrough

Full listing: [`262B-GetSystemInfo.ASM`](262B-GetSystemInfo.ASM).

**Prologue (`063022B-063027B`).** `STD I 116` / `RADD CLD SL DA` / `RADD CLD SB DD` / `SAB 43`
save the caller's registers and set up the local frame base; `JPL I 113` (`-> 063141B`) calls
the standard FILSYS entry helper, then `STX ,B 11` stashes the caller's `X`.

**Parameter check (`063030B-063045B`).** Loads `,B 2` (the caller's `Number` parameter), and on
one path branches to the error return (`JMP I 110 -> 063142B`); it then range-checks against the
table bounds `LDT 105` / `LDT 103` and jumps to the error path `063144B` when out of range.
(The manual states `Number` should be `0`; the bytes range-check it.)

**System-info block copy (`063046B-063066B`).** The routine builds source/destination pointers
(`RADD CLD SA DL` / `RADD CLD SA DD`, count in `T` via `SAT 25` / `SAT 37`) and executes two
**`MOVEW`** block-move instructions (`063054B`, `063063B`) that copy the resident system-info
words (system number, CPU type, version, instruction set, patch indicator, generation time)
into the caller's 12-word buffer. The worker closes cleanly at `063301B`.

The overall shape - prologue, parameter validation, then `MOVEW` copies into the caller buffer -
is byte-proven; the exact field layout of the copied block is **inferred** from the manual.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `,B 2` (`Number`) | in | selector; manual says always `0`; range-checked in bytes | VERIFIED (bytes); meaning inferred (manual) |
| caller buffer | out | 12-word (24-byte) system-info block filled by two `MOVEW` copies | VERIFIED (bytes: `MOVEW` at `063054B`/`063063B`); field layout inferred (manual) |
| `X` | in | saved at `063027B` (`STX ,B 11`) | VERIFIED (bytes) |
| return | out | via saved link; error path at `063142B`/`063144B` | VERIFIED (bytes) |

---

## Pseudo-code (for an emulator)

See **[`262B-GetSystemInfo.pseudo.c`](262B-GetSystemInfo.pseudo.c)** - the prologue, the
parameter range-check and the two `MOVEW` block copies are byte-verified; the exact system-info
field layout is inferred from the manual.

---

## Honest caveats

**What is byte-proven:** `GOTAB[262B] = MFELL`; `MCTAB[262B] = 063022B = CPUST`; the `CPUST`
entry bytes at `063022B` in `006-S3FS` match the disassembly (`021116B = STD I 116`); the worker
is a real handler that validates `Number`, then copies a system-info block into the caller's
buffer with two `MOVEW` instructions. The `MCTAB` identification is not a one-slot coincidence:
216 of 256 slots land exactly on named L07 symbols, and known anchors (`MON 005B -> RDISK`,
`MON 200B -> XMSG 007516B`, `MON 144B -> MAGTP`) all match.

**What is NOT proven:** the precise field-by-field layout and byte order of the 12-word block
(system number / CPU type / version / instruction set / patch indicator / generation time) -
that ordering comes from the manual, not from these bytes.

**Correction to earlier work.** The old folder read the disproven `GOTAB` and claimed
`GOTAB[262B] = 066262B -> F1737 -> CPUST` with the last hop unproven. The real dispatch is
`GOTAB[262B] = MFELL -> MCTAB[262B] = CPUST`, fully byte-proven. The **NC oracle depends on this
call**; its worker is now byte-anchored at `063022B`.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) -
master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
