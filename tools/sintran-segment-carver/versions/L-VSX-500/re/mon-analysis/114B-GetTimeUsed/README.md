# MON 114B (octal) - TUSED (GetTimeUsed)

Returns the CPU time the caller has used since login (for a batch job, the time since the job
started). The worker `TUSED` is carved in `003-S3CP`. All addresses/values are **octal**.

**Status:** **byte-verified** (dispatch chain + worker entry). The worker is real carved code
in `003-S3CP`.

> **CORRECTED 2026-07-13.** Earlier versions of this folder used the disproven dispatch model
> (GOTAB as the monitor-call table, an "uncarved CALLPROC bridge"). MON calls dispatch through
> **`MCTAB @ 005620B`** (segment `044-S3IDPIT`), not `GOTAB` (which is `MFELL` for 224 of 256
> calls). `MCTAB[114B] = 041303B = TUSED`, carved in `003-S3CP`. See
> [`../317B-ExecuteCommand/README.md`](../317B-ExecuteCommand/README.md) and
> `SINTRAN/CARVING-HANDOFF.md` section 3a.

- **Full disassembly:** [`114B-GetTimeUsed.ASM`](114B-GetTimeUsed.ASM).
- **Emulator model:** [`114B-GetTimeUsed.pseudo.c`](114B-GetTimeUsed.pseudo.c).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 114B"] --> B["ENT14 level-14 entry<br/>026-S3IMPIT :072167B"]
    B --> C["GOTAB[114B] = MFELL<br/>MGOTA=071233B :071347B"]
    C --> D["MFELL level switch<br/>026-S3IMPIT :072114B -> CALLP 032201B"]
    D --> E["MCTAB[114B] = TUSED<br/>MCTAB=005620B :005734B = 041303B"]
    E --> F["TUSED worker<br/>003-S3CP :041303B"]
    class A blue
    class B,C,D,E teal
    class F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

---

## Code location (dispatch path)

Byte offset = `(addr - loadbase)` in octal words x 2 (decimal). Every offset was reproduced
with `dd`.

| Role | Segment | Addr (octal) | Byte offset | Symbol | Verdict |
|------|---------|--------------|-------------|--------|---------|
| GOTAB[114B] slot | [026-S3IMPIT.asm](../../segments-ref/026-S3IMPIT/026-S3IMPIT.asm) | `071347B` = `072114B` | 32206 | -> `MFELL` | **VERIFIED** |
| MFELL level switch | [026-S3IMPIT.asm](../../segments-ref/026-S3IMPIT/026-S3IMPIT.asm) | `072114B` | 32920 | `MFELL` | **VERIFIED** |
| MCTAB[114B] slot | [044-S3IDPIT.asm](../../segments-ref/044-S3IDPIT/044-S3IDPIT.asm) | `005734B` = `041303B` | 1976 | -> `TUSED` | **VERIFIED** |
| TUSED worker body | [003-S3CP.asm](../../segments-ref/003-S3CP/003-S3CP.asm) | `041303B-041373B` | 9606 | `TUSED` | **VERIFIED** |

**Verify by hand** (from `tools/sintran-segment-carver/versions/L-VSX-500/segments/`):
```
dd if=026-S3IMPIT.bin bs=1 skip=32206 count=2 | od -An -tx1   ->  74 4c   (= 072114B = MFELL)
dd if=044-S3IDPIT.bin bs=1 skip=1976  count=2 | od -An -tx1   ->  42 c3   (= 041303B = TUSED)
dd if=003-S3CP.bin    bs=1 skip=9606  count=2 | od -An -tx1   ->  c4 35   (= 142065B, TUSED entry word)
```

---

## Instruction walkthrough

Full listing: [`114B-GetTimeUsed.ASM`](114B-GetTimeUsed.ASM).

**Entry / arg check (`041303B-041306B`).** `142065 SKP IF DA UEQ ST` compares `A` against `T` and,
on failure, `170547 SAA 147` loads an error code and `125146 JMP I 146` (-> `041454B`) takes the
error/return path.

**Selector decode (`041307B-041366B`).** `146157 RADD CLD SA DX` moves the selector into `X`,
`052012 LDT ,X 12` / `010602 STT ,B -176` stage a field, then `054143 LDX 143` / `135143 JPL I 143`
(-> `041456B`) calls a helper. The body is a multi-way selector (sub-symbols `1NOTO`, `XSBPR`,
`1FU2`, `1FU3`, `STERM`, `1FU4`, `1FU5`) each of which loads a table pointer (`LDX 136`/`133`...)
and calls the helper at `041456B`, returning via `125742 JMP I ,B -36`. A section brackets a
critical read with `150401 IOF` / `150402 ION` (interrupts off/on) while sampling the accounting
field (`046003 LDA ,X 3`).

The routine reads a per-process CPU-time accounting field and returns the time used; the exact
field offsets and the sub-function set are **inferred** from structure, the control flow and the
IOF/ION-guarded read are byte-proven.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `A`,`T` | in | argument checked at entry (`SKP IF DA UEQ ST`); mismatch -> error `147` | VERIFIED (bytes); meaning inferred |
| `,B -176` | frame | staged field (`STT ,B -176`) | VERIFIED (bytes) |
| accounting field | in | per-process CPU-time read under IOF/ION guard (`LDA ,X 3`) | VERIFIED (bytes); field offset inferred |
| result | out | CPU time used since login / job start | VERIFIED (manual); packing inferred |

---

## Pseudo-code (for an emulator)

See **[`114B-GetTimeUsed.pseudo.c`](114B-GetTimeUsed.pseudo.c)**. The arg check, the selector
decode and the interrupt-guarded accounting read are byte-verified; the field offsets and the exact
sub-function semantics are inferred.

---

## Honest caveats

**What is byte-proven:** the full dispatch chain - `GOTAB[114B] = MFELL`, `MFELL` switches program
level to `CALLP`, `MCTAB[114B] = TUSED = 041303B`, and the `TUSED` entry bytes at `041303B` in
`003-S3CP` match the disassembly. The routine range-checks an argument, decodes a multi-way
selector and reads a per-process accounting field under an IOF/ION guard.

**What is NOT proven:** the exact accounting-field layout, the meaning of each sub-function
(`1FU2`..`1FU5` etc.), and the result packing. Those are inferred from structure and the manual.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) ·
master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
