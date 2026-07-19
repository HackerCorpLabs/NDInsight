# MON 333B (octal) - DMAFunction (UDMA)

Various DMA (Direct Memory Access) functions on a DMA-channel device: receive/send DMA data
(with/without wait), read/clear DMA status, read last status, and use programmed I/O devices. This
is an ND-100 monitor call (also available on ND-500).

**Status:** `partial`. `GOTAB[333B] = 112561B` (byte-proven) routes to the `DIA11` DMA-device entry
stub in overlay `025-S3IRPIT` - real code that saves the caller register block and switches page
context (`TRR PCR`). The worker `UDMA = 110770B` (same overlay) is **real code**: a function-code
decoder that matches `FuncCode` against the manual's DMA function codes (`0,1,2,3,7,20,21,24,54..57`)
and builds a device control word. The `DIA11 -> UDMA` link crosses the `TRR PCR` context switch /
resident `CALLPROC` bridge (uncarved), so it is attributed by symbol + the matching function codes,
not a followed pointer (see [Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`333B-DMAFunction.ASM`](333B-DMAFunction.ASM) - both regions (DIA11 entry stub + the UDMA function decoder).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User/RT program<br/>MON 333B"] --> B["ENT14 level-14<br/>T = device no"]
    B --> C["GOTAB[333B] = 112561B<br/>(byte-proven)"]
    C --> D["DIA11 entry stub<br/>025-S3IRPIT :112561B"]
    D -.uncarved TRR PCR / CALLPROC.-> E["UDMA worker<br/>025-S3IRPIT :110770B"]
    E --> F["FuncCode decode<br/>build DMA control word"]
    class A blue
    class B,C,D teal
    class E teal
    class F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The dashed hop (`D ⇢ E`) is the page-context switch (`TRR PCR`) plus the resident `CALLPROC`
second-level dispatch - **not present in any carved segment**, so the exact `DIA11 -> UDMA` transfer
cannot be followed statically. Both endpoints are real bytes in `025-S3IRPIT`.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr − loadbase)` in octal words × 2
(decimal); commoncode load base is `0`, `025-S3IRPIT` load base is `32000B`.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[333] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071566B` (1 word) | 59116 | `GOTAB+333` = `112561B` | **VERIFIED** |
| DIA11 entry stub | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `112561B-112615B` (29 words) | 49890 | `DIA11` | **VERIFIED** (real stub) |
| context switch / CALLPROC bridge | — (uncarved) | — | — | `TRR PCR` / `CALLPROC` | **UNVERIFIED** |
| UDMA worker body | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `110770B-111120B` (code) | 48112 | `UDMA` | real bytes; link **inferred** |

**Verify by hand (GOTAB word):** `grep '^71566 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
→ `71566  112561  225 161  59116`; then
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=59116 count=2 2>/dev/null | od -An -tx1`
→ `95 71` (= octal `112561`, the DIA11 dispatch address).

**Verify by hand (DIA11 stub):** `grep '^112561 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex`
→ byte offset `49890`, value `150401`; then
`dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=49890 count=2 2>/dev/null | od -An -tx1`
→ `d1 01` (= octal `150401`, `IOF`, the stub's first word).

**Verify by hand (UDMA worker):** `grep '^110770 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex`
→ byte offset `48112`, value `135131`; then
`dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=48112 count=2 2>/dev/null | od -An -tx1`
→ `ba 59` (= octal `135131`, `JPL I 131`, the worker's first word). `prove-mon.py 333` reports the
same `GOTAB[333]=112561 -> DIA11`.

---

## Instruction walkthrough

Full listing: [`333B-DMAFunction.ASM`](333B-DMAFunction.ASM). All addresses octal; `X` = saved
register block, `B` = per-call DMA datafield (roles inferred from the access pattern).

**DIA11 entry stub (112561-112615)** — the level-14 DMA entry. It goes `IOF` / `TRR PCR` (page
context switch) / stages the caller `A` into a register block (`LDX I 76 -> [112661B]` ; `STA ,X
17`) / `ION`, then stages the parameter word (`,B 20`) and the MON number (`STT ,X 0`) with a second
`TRR PCR`, and `JMP`s into the shared disk/DMA-stub tail. **VERIFIED (bytes).**

**UDMA function decoder (110770-111120)** — after setup (`JPL I 131/130`) and a non-zero check
(`SKP IF DA UEQ 0`), it enables interrupt bits (`SAA 4 ; MST PIE`) and stages a DMA descriptor with
the register-block moves `LRB 4` / `SRB 40`. It then clears the control word (`SAA 0 ; RADD CLD SA
DD` → `D = 0`) and `111020 LDA ,B 20` fetches `FuncCode`. A ladder of `SAT n / SKP IF DA EQL ST`
comparisons matches the manual's codes and sets the corresponding `BSET ONE` bit in `D` (bit number =
the nd100-dis printed field `>>3`: func 0 → bits 0+2; func 1 → bit 1; func 2/3 → bit 3; func 20/24 →
bits 2+4; func 54/56 → bit 5; func 55/57 → bit 6; func 7/62 → bit 2; func 21/64/65/70 → indirect
worker via `JMP I 10 -> [111130]`). **VERIFIED (bytes); the meaning of each control bit is
inferred.**

**Shared status-store tail (111131-111132)** — `RADD CLD SD DA` copies the built control word from
`D` into `A`, and `STA ,B 35` stores it to the caller datafield. Words `111121-111130` are the
`JPL-I` pointer/data table (they disassemble as bogus instructions); their callees are outside this
carve.

---

## Parameter / register contract

Manual-side names/types are from [`333B_DMAFunction.yaml`](../../../../../../../Developer/MON/calls/333B_DMAFunction.yaml).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `DeviceNo` (`T`) | in | logical device number of a DMA channel | inferred (manual `LDT LDN`) |
| `FuncCode` / `,B 20` | in | DMA function code (`0..3,7,20,21,24,54..57`) | VERIFIED literals (111021-111116); meaning inferred |
| `DataAddress` | io | memory address of data (func 0:3) or PIO device no (func 54:57) | inferred (manual) |
| `InPara` / `OutPara` | io | 32-bit function-dependent input/output parameters | inferred (manual) |
| control word (reg `D`) | work | device control word built bit-by-bit per FuncCode, stored to `,B 35` | VERIFIED (111016-111132) |
| `ErrCode` | out | standard error code (appendix A) | inferred (manual) |

The concrete DMA operation each control bit triggers, and the indirect workers reached through
`[111130]`, need live device context and are outside the carved window.

---

## Pseudo-code (for an emulator)

See **[`333B-DMAFunction.pseudo.c`](333B-DMAFunction.pseudo.c)** — a pseudo-C model for emulator
authors. The `DIA11` register-block save + `TRR PCR` context switch and the `UDMA` FuncCode decoder
(the `SAT`/`SKP IF DA EQL ST` comparisons and the `BSET ONE` control-word bits) are byte-verified;
the meaning of each control bit and the caller-side parameter list are inferred from the manual.

Every instruction in the `.pseudo.c` is translated against the canonical
[`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(`RADD CLD SA DD` = `D = A` COPY; `SKP IF DA EQL ST` = `if (A == T) skip`; `SKP IF DA UEQ ST` =
`if (A != T) skip`; `BSET ONE b Dr` = `reg |= (1<<b)` with bit number = printed field `>>3`;
`SAT n` = `T := n`; `MST PIE` = masked-set of the interrupt-enable register; `LRB`/`SRB` =
load/store 8-word register block via the alt page table).

---

## Honest caveats

**What is byte-proven:** `GOTAB[333B] = 112561B` routes to `DIA11` (real code, first word `150401` =
`IOF`); the `DIA11` stub's `IOF`/`TRR PCR`/`ION` critical section and register-block staging; the
`UDMA` worker at `110770B` (first word `135131` = `JPL I 131`); the FuncCode ladder whose literal
codes (`0,1,2,3,7,20,21,24,54,55,56,57,...`) match the manual's DMA function codes; the `BSET ONE`
control-word construction in register `D`; the store to `,B 35`.

**What is NOT proven:** the `DIA11 -> UDMA` link and the physical DMA operation each control bit
drives. `DIA11` switches page context with `TRR PCR` and re-enters through the resident `CALLPROC`
bridge in an **uncarved overlay**; the device-side workers reached through `[111130]` sit past the
carve. Attributing the body to `UDMA` rests on the symbol name (`UDMA` = User DMA) plus the FuncCode
match with the manual, not a followed pointer.

This reconciles into one story: the dispatch head (`GOTAB[333] -> DIA11`) is solid; the `DIA11` stub
and the `UDMA` decoder are both real bytes in `025-S3IRPIT`; the transfer between them and the leaf
DMA workers cross the uncarved context-switch/pointer layer. Confirming the exact link needs a live
trace (break at `112561B` on a real `MON 333`, single-step the `TRR PCR` switch, and record where P
lands).

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
