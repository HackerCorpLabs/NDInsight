# MON 155B (octal) - GraphicFunction (GRAPH)

Executes various functions on a graphic peripheral (a NORDCOM terminal, a pen plotter, or a Tektronix
display): `Func 0` = PLOT, `Func 1` = PLOTS (establish reference point / clear a NORDCOM screen),
`Func 2` = NEWP (select pen or screen). The obsolete PLOT monitor call is replaced by this. This is
an ND-100 monitor call.

**Status:** `partial`. `GOTAB[155B] = 122461B` (byte-proven) routes to the label `F1675` **inside**
the graphic-function dispatcher cluster `F1674..F1677` in overlay `025-S3IRPIT` - **real code**. The
cluster validates the `Func` code, then a computed jump (`LDA ,B -2` / `RADD SA DP`) selects one of
the graphic operations, each of which drives a graphic device register via `IRW`/`IRR` after a
`JPL-I` worker call. The graphic device workers reached through the `JPL-I` pointer words are in the
resident/runtime graphic driver, which is **not in any carved segment** (see
[Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`155B-GraphicFunction.ASM`](155B-GraphicFunction.ASM) - the graphic dispatcher cluster + the worker pointer words it dispatches through.
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 155B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[155B] = 122461B<br/>(byte-proven)"]
    C --> D["F1675 graphic dispatcher<br/>025-S3IRPIT :122461B"]
    D --> E["Code computed jump<br/>PLOT / PLOTS / NEWP + IRW/IRR"]
    E -.uncarved JPL-I pointer words.-> F["graphic device worker<br/>(resident/runtime driver)"]
    class A blue
    class B,C,D teal
    class E teal
    class F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

`GOTAB[155]` enters the cluster at `F1675 = 122461B`. The dispatch itself (Func check, Code computed
jump, `IRW`/`IRR` device-register access) is real in-cluster code. The dashed hop (`E ⇢ F`) is the
`JPL-I` dispatch through the graphic worker pointer words into the graphic driver - **not present in
any carved segment**.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr − loadbase)` in octal words × 2
(decimal); commoncode load base is `0`, `025-S3IRPIT` load base is `32000B`.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[155] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071410B` (1 word) | 58896 | `GOTAB+155` = `122461B` | **VERIFIED** |
| graphic dispatcher (GOTAB entry) | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `122461B` (F1675) within `122434B-122557B` | 57954 | `F1675` (in `F1674..F1677` cluster) | **VERIFIED** (real code) |
| graphic worker pointer words | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `122616B-122621B` (data) | 58140 | (unnamed) | real bytes = **DATA** |
| graphic device driver | — (uncarved) | — | — | graphic driver | **UNVERIFIED** |

**Verify by hand (GOTAB word):** `grep '^71410 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
→ `71410  122461  245 061  58896`; then
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58896 count=2 2>/dev/null | od -An -tx1`
→ `a5 31` (= octal `122461`, the F1675 dispatch address).

**Verify by hand (F1675 entry):** `grep '^122461 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex`
→ byte offset `57954`, value `124365`; then
`dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=57954 count=2 2>/dev/null | od -An -tx1`
→ `a8 f5` (= octal `124365`, `JMP -13`, the F1675 entry word). `prove-mon.py 155` reports the same
`GOTAB[155]=122461 -> F1675`.

---

## Instruction walkthrough

Full listing: [`155B-GraphicFunction.ASM`](155B-GraphicFunction.ASM). All addresses octal; `B` =
per-call graphic datafield (role inferred from the access pattern).

**Func validation (122442-122460)** — `122442 LDA ,B 4` fetches the `Func` code; `SAT 1 / SKP IF DA
UEQ ST` routes `Func 1` (PLOTS) through the setup worker `JPL I 152 -> [122620]`. The `GOTAB` entry
`F1675 = 122461` re-joins this setup path with `JMP -13 -> 122446`. **VERIFIED (bytes).**

**Code computed jump (122462-122473)** — `122462 LDA ,B -2` loads the `Code` parameter and
`122463 RADD SA DP` performs `P = P + Code`, a computed jump into the 8-entry `JMP` table at
`122464-122473`. Each entry vectors to one graphic operation arm. **VERIFIED (bytes).**

**Graphic-register operation arms (122474-122557)** — each arm calls a graphic worker
(`JPL I -> [122621]`), stages an operand (`STA ,B -7 / SHA ZIN 10`), and reads or writes a graphic
device register with `IRW 10 D<r>` (write) / `IRR 10 D<r>` (read, then `ORA ,B -7 / IRW`). The
register letter cycles through `A`/`D`/`L`/`X` across the eight `Code` arms. The tail merges at
`122557 STZ ,B -2`. **VERIFIED (bytes); which physical graphic register each maps to is inferred.**

**Worker pointer words (122616-122621)** — the `JPL-I` targets; they disassemble as bogus
instructions because they are pointer cells. Their final callees are in the uncarved graphic driver.

---

## Parameter / register contract

Manual-side names/types are from [`155B_GraphicFunction.yaml`](../../../../../../../Developer/MON/calls/155B_GraphicFunction.yaml).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `Func` / `,B 4` | in | `0`=PLOT, `1`=PLOTS, `2`=NEWP | VERIFIED fetch + `Func 1` branch (122442-122446); mapping inferred (manual) |
| `Code` / `,B -2` | in | integer code selecting the graphic sub-operation | VERIFIED computed jump (122462-122473); meaning inferred |
| `Ycoor` / `Xcoor` | in | Y/X coordinate of new line relative to reference point | inferred (manual) |
| `DeviceNo` | in | logical device number of the graphic peripheral | inferred (manual) |
| graphic device reg | io | written/read via `IRW`/`IRR 10 D<r>` per Code arm | VERIFIED (122500-122556); register mapping inferred |
| `ReturnValue` | out | output parameter for the PLOT function (not used on ND-500) | inferred (manual) |

The graphic device workers reached through `[122621]` need live device context and are outside the
carved window.

---

## Pseudo-code (for an emulator)

See **[`155B-GraphicFunction.pseudo.c`](155B-GraphicFunction.pseudo.c)** — a pseudo-C model for
emulator authors. The `Func` validation, the `Code` computed jump, and the `IRW`/`IRR`
device-register arms are byte-verified; the PLOT/PLOTS/NEWP mapping and the physical register meanings
are inferred from the manual.

Every instruction in the `.pseudo.c` is translated against the canonical
[`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(`RADD SA DP` = `P = P + A` computed jump; `SKP IF DA UEQ ST` = `if (A != T) skip`; `SHA ZIN 10` =
logical `A <<= 8`; `IRW 10 D<r>` = `reg[level 1][r] = A`; `IRR 10 D<r>` = `A = reg[level 1][r]`;
`ORA ,B -7` = `A |= mem[B-7]`; bare `LDA 157` = P-relative `mem[P+disp]`, not a literal; `MIN ,B -2`
= increment-and-skip-on-zero).

---

## Honest caveats

**What is byte-proven:** `GOTAB[155B] = 122461B` routes to `F1675` (real code, entry word `124365` =
`JMP -13`); the `Func` code fetch (`LDA ,B 4`) and the `Func 1` (PLOTS) branch; the `Code`
computed-jump table (`LDA ,B -2` / `RADD SA DP` / eight `JMP` arms); the per-arm `IRW`/`IRR 10 D<r>`
graphic-device-register access; the common tail merge at `STZ ,B -2`; and that `122616B..122621B` are
real data (pointer cells).

**What is NOT proven:** the PLOT/PLOTS/NEWP naming of the Func codes, the meaning of each `Code`
sub-operation, and the graphic device workers behind the `JPL-I` pointer words - those live in the
resident/runtime graphic driver, **not present in any carved segment**. The `GRAPH` symbol appears
only in the ND-500 overlays (`026-S3IMPIT`/`030-S3SM5`), i.e. the ND-500-side graphic companion; the
ND-100 handler is this `F1674..F1677` cluster, attributed by the `GOTAB` entry + the graphic-register
dispatch shape.

This reconciles into one story: the dispatch head (`GOTAB[155] -> F1675`) is solid; the graphic
dispatcher cluster is real, fully-decoded ND-100 code that reaches graphic device registers via
`IRW`/`IRR`; but its leaf workers and the exact Func/Code semantics cross the uncarved pointer/driver
layer. Confirming them needs a live trace (break at `122461B` on a real `MON 155`, single-step the
`RADD SA DP` computed jump, and record the `IRW`/`IRR` device addresses).

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
