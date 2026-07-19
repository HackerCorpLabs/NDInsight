# MON 321B (octal) - UEADM (UEAdministrator)

Handle User Environment (administrator) - manual section 2.14, short name `UEADM`. A companion
to MON 320B UELogin (`UELOG`) and MON 317B ExecuteCommand (`UECOM`), all in the same
User-Environment code cluster.

**Status:** **byte-verified** (dispatch + worker body). The worker is real carved code in
`003-S3CP`. All addresses/values are **octal**.

> **CORRECTED 2026-07-13.** The previous version of this folder said the worker was
> `not-carved / name-only`, "zero-filled", with `GOTAB[321B]=112376B` vectoring to a diagnostic
> region `DIA6`. **All of that was an artefact of the wrong dispatch model.** MON calls are not
> dispatched through that `GOTAB` (it is `MFELL` for 224 of 256 calls); they go through
> **`MCTAB @ 005620B`**. `MCTAB[321B] = 065453B = UEADM`, and that worker IS carved - in
> `003-S3CP`, the same segment as its siblings `UECOM`/`UELOG`/`SETOL`. The old analysis read
> `065453B` in `SINTRAN-DATA_commoncode` (zeros there - wrong overlay) and wrongly concluded
> "absent". See [`../317B-ExecuteCommand/README.md`](../317B-ExecuteCommand/README.md) and
> `SINTRAN/CARVING-HANDOFF.md` section 3a.

- **Full disassembly:** [`321B-UEAdministrator.ASM`](321B-UEAdministrator.ASM).
- **Emulator model:** [`321B-UEAdministrator.pseudo.c`](321B-UEAdministrator.pseudo.c).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 321B"] --> B["ENT14 level-14<br/>026-S3IMPIT :072167B"]
    B --> C["GOTAB[321B] = MFELL<br/>:071554B"]
    C --> D["MFELL level switch<br/>:072114B -> CALLP 032201B"]
    D --> E["MCTAB[321B] = UEADM<br/>044-S3IDPIT :006141B = 065453B"]
    E --> F["UEADM worker<br/>003-S3CP :065453B"]
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
| MCTAB[321B] slot | [044-S3IDPIT.asm](../../segments-ref/044-S3IDPIT/044-S3IDPIT.asm) | `006141B` = `065453B` | 2242 | -> `UEADM` | **VERIFIED** |
| UEADM worker body | [003-S3CP.asm](../../segments-ref/003-S3CP/003-S3CP.asm) | `065453B-065642B` | 30294 | `UEADM` | **VERIFIED** |

**Verify by hand** (from `tools/sintran-segment-carver/versions/L-VSX-500/segments/`):
```
dd if=044-S3IDPIT.bin bs=1 skip=2242  count=2 | od -An -tx1   ->  6b 2b   (= 065453B = UEADM)
dd if=003-S3CP.bin    bs=1 skip=30294 count=2 | od -An -tx1   ->  cc 5f   (= 146137B, UEADM entry)
```

---

## Instruction walkthrough

Full listing: [`321B-UEAdministrator.ASM`](321B-UEAdministrator.ASM).

**Prologue (`065453B-065460B`).** Saves the parameter/frame base (`X := B`, stored at `,B -166`)
and the return link (`L`, stored at `,B -167`).

**Sub-function decode (`065461B-065474B`).** Loads a selector from `param[12]` (`LDA ,X 12`) and
range-checks it against `[1..8]` (`SAT 1 / SKP IF DA GRE ST` ... `SAT 10 / SKP IF DT LST SA`).
Out of range -> stores error code `124B` into `param[12]` and takes the error/return path.

**User-table access (`065475B-065534B`).** Stages the in-range operands, calls a helper via
`JPL I 111` (pointer `@065620B`), then computes an index (`MPY 74 / ADD I 74`) and does a
**physical** read of a user-table entry (`LDT I 73 / LDATX`).

**Bit-field extraction (`065515B-065524B`).** Pulls two fields out of `param[17]`
(`SHA ZIN SHR 7 / AND 103` and `SHA ZIN SHR 13 / AND 77`) into frame slots.

**Selector jump table (`065600B-065612B`).** `RADD SA DP` computes `P := P + selector`, i.e. a
computed dispatch into a small vector of `JMP`/`JMP I` per sub-function, using the pointer table
at `065615B-065640B`. The common tail reloads the saved registers at `065641B` and returns.

The exact semantics of each of the 8 sub-functions are **inferred** from structure; only the
control flow and the physical user-table read are byte-proven.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `B` / param base | in | frame/parameter block base (saved `X := B`, `STX ,B -166`) | VERIFIED (bytes) |
| `param[12]` | in/out | sub-function selector, range-checked `[1..8]`; on error set to `124B` | VERIFIED (bytes) |
| `param[17]` | in | packed bit-fields extracted at `065515B-065524B` | VERIFIED (bytes); field meaning inferred |
| `param[10]`,`param[32]`,`param[44]` | in | staged operands for the selected sub-function | VERIFIED (bytes); meaning inferred |
| return | out | via saved link `,B -167`; common tail at `065641B` | VERIFIED (bytes) |
| caller class | in | system programs (manual: administer per-user User Environment) | inferred (manual) |

---

## Pseudo-code (for an emulator)

See **[`321B-UEAdministrator.pseudo.c`](321B-UEAdministrator.pseudo.c)** - the prologue, the
selector range-check, the physical user-table read and the computed jump table are byte-verified;
the per-sub-function semantics are inferred.

---

## Honest caveats

**What is byte-proven:** `MCTAB[321B] = 065453B = UEADM`; the `UEADM` entry bytes at `065453B` in
`003-S3CP` match the disassembly; the routine is a real ~100-word handler that decodes an 8-way
sub-function selector from `param[12]`, reads a per-user table by physical address (`LDATX`), and
dispatches through a computed jump table.

**What is NOT proven:** the meaning of each of the 8 sub-functions, the exact layout of the user
table it reads, and the manual's claim that `321B` is "no longer supported" (section 2.16) - the
carved bytes show a live, complete handler, which does not obviously match a retired call. That
tension is noted, not resolved.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) ·
master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
