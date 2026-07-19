# MON 122B (octal) - RESRV (ReserveResource)

Reserve a device or file for the calling program **only**. Some devices have a separate **input part**
and **output part**; each `MON 122` reserves one part. Released by `MON 123` (ReleaseResource) or
automatically on normal RT-program termination. ND-100 monitor call, manual section for `RESRV`.

**Status:** **byte-verified** (dispatch + worker body). The worker is real carved code in `003-S3CP`,
next to its sibling `RELES` (MON 123B). All addresses/values are **octal**.

> **CORRECTED 2026-07-13.** The previous version of this folder said the worker was
> `misattributed / not-carved`, "zero-filled" at `037103B`. **That was an artefact of the wrong
> dispatch model.** MON calls are not dispatched through the level-14 `GOTAB` (it is `MFELL` for 224
> of 256 calls); they go through **`MCTAB @ 005620B`** (segment `044-S3IDPIT`). `MCTAB[122B] =
> 037103B = RESRV`, and that worker IS carved - in `003-S3CP`. The old analysis read `037103B` in
> `SINTRAN-DATA_commoncode` (zeros there - wrong overlay) and wrongly concluded "absent". See
> [`../317B-ExecuteCommand/README.md`](../317B-ExecuteCommand/README.md) and
> `SINTRAN/CARVING-HANDOFF.md` section 3a.

> **REFINEMENT 2026-07-14** (see [`../../kernel-carving/PRSRV-124B/README.md`](../../kernel-carving/PRSRV-124B/README.md)).
> The overlay/bytes here are CORRECT: `003-S3CP` @ `037103B` is byte-identical to the
> resident System Monitor `071-S3SM`. Two interpretation notes: (1) the `037110B-037155B`
> command-builder block is really the *shared* executor **`EXECC 037110B`** (the entry stub
> `RESRV` is the 5 words `037103-037107` in front of it); (2) the two `MON 70B` dispatches
> resolve to **`MCTAB[070]=050673B=COMSB`**, the @-command interpreter. Its force-sibling
> **PRSRV (124B) is a DIFFERENT shape** - a 2-word trampoline to `LEAV2 027417B`, not this
> command builder - so do not assume RESRV and PRSRV share a body.

- **Full disassembly:** [`122B-ReserveResource.ASM`](122B-ReserveResource.ASM).
- **Emulator model:** [`122B-ReserveResource.pseudo.c`](122B-ReserveResource.pseudo.c).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 122B"] --> B["ENT14 level-14<br/>026-S3IMPIT :072167B"]
    B --> C["GOTAB[122B] = MFELL<br/>:071355B"]
    C --> D["MFELL level switch<br/>:072114B -> CALLP 032201B"]
    D --> E["MCTAB[122B] = RESRV<br/>044-S3IDPIT :005742B = 037103B"]
    E --> F["RESRV worker<br/>003-S3CP :037103B"]
    class A blue
    class B,C,D,E teal
    class F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

---

## Code location (dispatch path)

Byte offset = `(addr - loadbase)` in octal words x 2 (decimal). Offsets reproduced with `dd`.

| Role | Segment | Addr (octal) | Byte offset | Symbol | Verdict |
|------|---------|--------------|-------------|--------|---------|
| MCTAB[122B] slot | [044-S3IDPIT.asm](../../segments-ref/044-S3IDPIT/044-S3IDPIT.asm) | `005742B` = `037103B` | 1988 | -> `RESRV` | **VERIFIED** |
| RESRV worker body | [003-S3CP.asm](../../segments-ref/003-S3CP/003-S3CP.asm) | `037103B-037155B` | 7302 | `RESRV` | **VERIFIED** |

**Verify by hand** (from `tools/sintran-segment-carver/versions/L-VSX-500/segments/`):
```
dd if=044-S3IDPIT.bin bs=1 skip=1988 count=2 | od -An -tx1   ->  3e 43   (= 037103B = RESRV)
dd if=003-S3CP.bin    bs=1 skip=7302 count=2 | od -An -tx1   ->  3d fe   (= 036776B, RESRV entry)
```

---

## Instruction walkthrough

Full listing: [`122B-ReserveResource.ASM`](122B-ReserveResource.ASM).

**Prologue (`037103B-037117B`).** A short register/context preamble, then `EXECC` (`037110B`) loads a
flag word and takes an early `EXIT` (`037112B`) if it is clear. Otherwise it saves the link (`T := L`),
and stages a command-string pointer into `D`.

**Command dispatch (`037120B-037124B`).** Calls a helper (`JPL I 45`) that builds a SINTRAN command
line, then issues **`MON 70B` (CallCommand)** to run a RESERVE command through the command processor.

**Byte-scan loop (`037125B-037155B`).** Walks the string byte-by-byte (`LBYT`), comparing each byte to
delimiter `47B`; for each delimited field it re-issues `MON 70B`. The loop ends at `037156B`, which is
the `RELES` boundary. The exact per-field semantics are **inferred**; the two `MON 70B` dispatches and
the loop control are byte-proven.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `A` | in | command/parameter-string pointer used to build the RESERVE command | VERIFIED (bytes: `LDA 46` / `MON 70`) |
| `mem[053]` flag | in | gate checked at `EXECC`; clear -> early `EXIT` | VERIFIED (bytes) |
| `L` | in/out | return link saved through `T` across the helper call | VERIFIED (bytes) |
| DeviceNo / IOFlag / WaitFlag / Status | in/out | manual parameter list for the reservation itself | inferred (manual) |
| error return | out | standard error code | inferred (manual) |

---

## Pseudo-code (for an emulator)

See **[`122B-ReserveResource.pseudo.c`](122B-ReserveResource.pseudo.c)** - the `EXECC` gate, the two
`MON 70B` (CallCommand) dispatches, and the byte-scan loop are byte-verified; the resource-table
semantics are inferred from the manual.

---

## Honest caveats

**What is byte-proven:** `MCTAB[122B] = 037103B = RESRV`; the `RESRV` entry bytes at `037103B` in
`003-S3CP` match the disassembly; the routine is a thin wrapper that builds and runs a RESERVE command
line via `MON 70B` inside a byte-scan loop, sharing infrastructure with its neighbour `RELES` (MON 123B).

**What is NOT proven:** the resource-table layout and the manual's `DeviceNo/IOFlag/WaitFlag/Status`
parameter list - the actual locking happens inside the command processor `MON 70B` calls, not in
these bytes. The old "zero-filled / not carved" verdict was simply the wrong overlay (commoncode
instead of `003-S3CP`) and is withdrawn.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) ·
master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
