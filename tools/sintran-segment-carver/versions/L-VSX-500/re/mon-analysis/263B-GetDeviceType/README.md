# MON 263B (octal) - GetDeviceType (GDEVT)

Gets the device type of a logical device (terminal, TAD, communication channel,
internal block device, floppy, magnetic tape, mass-storage file) and returns a
device-attribute bitmask describing how the device may be handled (byte I/O,
start-on-interrupt, device control, block calls, clear-device, reservation, and
so on). Available to user, RT and system programs on the ND-100 and ND-500.

**Status:** GOTAB dispatch head byte-proven as **fall-through** (`GOTAB[263B] =
000000`, no per-call stub); the `GDEVT` worker body is real SINTRAN L bytes in
segment `025-S3IRPIT` (the SYMBOL-2-LIST code overlay). The exact `MON 263 ->
worker` link crosses an uncarved kernel bridge, and the worker's own tail branches
into the adjacent `X21CL` routine (see [Honest caveats](#honest-caveats)). All
addresses/values are **octal**.

- **Full disassembly:** [`263B-GetDeviceType.ASM`](263B-GetDeviceType.ASM) - the actual code (the GDEVT worker body; there is no entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 263B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[263B] = 000000<br/>(byte-proven: fall-through)"]
    C -.uncarved MFELL / CALLPROC.-> E["GDEVT get-device-type worker<br/>025-S3IRPIT :107104B"]
    E -.shared tail.-> F["X21CL routine<br/>025-S3IRPIT :107131B (adjacent; uncarved link)"]
    class A blue
    class B,C blue
    class E green
    class F teal
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The GOTAB slot is zero, so there is **no per-call entry stub**. The dashed hop
(`C -> E`) is the resident `MFELL`/`CALLPROC` fall-through second-level dispatch.
The worker exits (`E ⇢ F`) into the adjacent `X21CL` routine, which is shared
level-14 tail code, **not part of the GetDeviceType body** and not carved here.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words x 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[263] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071516B` (1 word) | 59036 | `GOTAB+263` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `MFELL`/`CALLPROC` | **UNVERIFIED** |
| GDEVT get-device-type worker body | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) - [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `107104B-107130B` (21 words) | 46216 | `GDEVT` | real bytes; link **MISATTRIBUTED** |
| X21CL shared tail | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) - [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `107131B` (branch target) | 46258 | `X21CL` | adjacent routine; **UNVERIFIED** as GDEVT tail |

The window is bounded strictly to the next symbol `X21CL=107131B` (21 words).

**Verify by hand:** `grep '^107104 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex` -> byte offset `46216`;
then `dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=46216 count=8 | od -An -tx1` -> `ba 71 f1 04 d0 c7 4a 6f`
(= octal `135161 170404 150307 045157` = `JPL I 161` / `SAA 4` / `MST PIE` / `LDA I 157`, the GDEVT entry).

The GOTAB slot itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=59036 count=2 | od -An -tx1` -> `00 00` (= `000000`, fall-through).
`prove-mon.py 263` reads the same GOTAB zero.

---

## Instruction walkthrough

Full listing: [`263B-GetDeviceType.ASM`](263B-GetDeviceType.ASM). The body is the
`GDEVT` worker (there is no F16xx stub because `GOTAB[263] = 0`).

**Entry + setup (`107104-107106`)** - `107104 JPL I 161` (link cell `107265`)
calls a resident worker; `107105 SAA 4` / `107106 MST PIE` masked-set the
interrupt-enable register.

**Descriptor fetch + presence test (`107107-107114`)** - `107107 LDA I 157`
loads the device descriptor word; `107110 JAZ 21 -> 107131` exits to the shared
`X21CL` tail when it is zero; `107111 LDA ,B 11` / `107112 SAT 1` /
`107113 SKP IF DA EQL ST` test the device flag, and `107114 JMP 15 -> 107131`
exits to `X21CL` when it does not match.

**Attribute selection (`107115-107130`)** - `107115 LDA I -13` / `107116 JAF 3`
choose a branch; the `107121-107127` path follows a device-table chain
(`LDX I -16`, `LDA ,B 12`, `LDX ,X 12`, and `LDA ,X 26` / `LDA ,X 23` select the
device-type attribute word); `107130 JMP 2 -> 107132` returns through the shared
`X21CL` tail where `DevType`/`DevAttr` are stored back to the caller.

---

## Parameter / register contract

Manual-side names/types are from [`263B_GetDeviceType.yaml`](../../../../../../../Developer/MON/calls/263B_GetDeviceType.yaml).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `107104B` = GDEVT worker entry (fall-through, no stub) | VERIFIED (bytes) |
| `T` (manual) | in | DeviceNo - logical device number (1 = own terminal, appendix B) (`LDT DEVNO`) | inferred (manual MAC example) |
| `A` (manual) | in | IOFlag - 0 = input part, 1 = output part (`LDA IOF`) | inferred (manual MAC example) |
| `T` (manual) | out | DevType - 0..7 (unspecified / terminal / TAD / comm / block / floppy / mag-tape / mass-storage) | inferred (manual) |
| `A:D` (manual) | out | DevAttr - 32-bit attribute bitmask (byte I/O, interrupt, control, block, clear, reservation, COSMOS, NOTS, MTAD) | inferred (manual) |
| device descriptor | internal | fetched via `LDA I 157` (107107), then presence/attribute tested | VERIFIED (bytes); field labels inferred |
| error return | out | standard error number in `A` (K flag set) | inferred (manual) |

The user-visible `T`/`A` -> DeviceNo/IOFlag convention and the `DevType`/`DevAttr`
returns live in the caller-side `MON 263` wrapper and the shared `X21CL` store
tail (uncarved), so the precise register-to-field assignment is **inferred** from
the manual, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`263B-GetDeviceType.pseudo.c`](263B-GetDeviceType.pseudo.c)** - a pseudo-C
model of the handler for emulator authors. The control flow (descriptor fetch,
the equality test, the two-way attribute selection) is byte-verified; the
`DevType`/`DevAttr` store happens in the shared `X21CL` tail past the window and
the field semantics are inferred from the manual.

Every instruction in the pseudo-code is translated against the canonical
[ND-100 instruction semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(`MST PIE` masked-set, `LDA I` one-level indirect, `SKP`/`JAZ`/`JAF` senses,
X-indexed loads, addressing-mode effective addresses).

---

## Honest caveats

**What is byte-proven:** `GOTAB[263B] = 000000` (level-14 dispatch, a fall-through
with no per-call vector); the `GDEVT` worker body at `107104B` in `025-S3IRPIT` is
real code (entry bytes `135161 170404 150307 045157` match the disassembly); and
it is a device-descriptor lookup that selects a device-type attribute word,
consistent with GetDeviceType.

**Which overlay and why:** `GDEVT=107104B` is a `SYMBOL-2-LIST` symbol, present in
both the `025-S3IRPIT` and `026-S3IMPIT` overlays (both load `32000B`). Both hold
code-shaped bytes at `107104B`, but `025-S3IRPIT` is the `SYMBOL-2-LIST` code
overlay - its companion MON-32 worker (`MSG`) is real code there while
`026-S3IMPIT` holds unrelated float/data at that address, so `025-S3IRPIT` is the
consistent choice for this `SYMBOL-2-LIST` symbol. The `026-S3IMPIT` image at
`107104B` is a different (N500) overlay and is not used here.

**What is NOT proven:** the link from the zero GOTAB slot to the `GDEVT` worker.
Because the vector is zero there is no stub to disassemble and no pointer to
dereference; dispatch drops into the resident `MFELL`/`CALLPROC` second-level
path, which lives in an **uncarved overlay**. So the `MON 263 -> GDEVT`
attribution rests on the `GDEVT` symbol name + the matching device-lookup
behaviour, not a followed pointer - hence **MISATTRIBUTED** in the strict sense.
The worker's tail branches (`107110`/`107114 -> 107131`, `107130 -> 107132`)
leave the window into the adjacent `X21CL` routine (shared level-14 tail code) -
those bytes are real but are **not part of the GetDeviceType body** and are not
carved here. Confirming the dispatch link needs a live trace: issue a real
`MON 263`, single-step the level-14 fall-through into the resident `CALLPROC`,
and confirm P lands on `GDEVT = 107104`.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
