# MON 515B (octal) - MultipleDataTransfer (5MTRA)

Queues / checks / starts a *multiple* disk data transfer for an ND-500 process, mapping the
process's logical device + read/write function onto the ND-100 disk driver queue.

**Status:** worker body is real SINTRAN L bytes (symbol `5MTRA = 143445B`, byte-verified);
this is an **ND-500 level-12 GOSW call**, NOT an ND-100 GOTAB slot - the GOSW-index-13 to
`5MTRA` table lookup is resident and **not byte-located** (see [Honest caveats](#honest-caveats)).
All addresses/values are **octal**.

- **Full disassembly:** [`515B-MultipleDataTransfer.ASM`](515B-MultipleDataTransfer.ASM) - the actual code, the level-12 worker body.
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["ND-500 process<br/>MON 515B"] --> B["stop MOCALL,<br/>enter level-12"]
    B --> C["MCHANDEL reads<br/>call number 515B"]
    C -.uncarved GOSW table.-> D["5CMNO GOSW index 13"]
    D --> E["5MTRA worker body<br/>026-S3IMPIT :143445B"]
    class A blue
    class B,C blue
    class D teal
    class E green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The dashed hop (`C ⇢ D`) is the resident level-12 GOSW table lookup - it is **not present in any
carved segment**, so the index-13 to `5MTRA` selection cannot be followed statically here. It is
inferred from the ND-500 dispatch structure, not byte-proven.

---

## Code location (dispatch path)

Every worker row is a real region you can open. Byte offset = `(addr − loadbase)` in octal words × 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| ND-100 GOTAB[515] neighbour (WRONG namespace) | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071750B` word = `057605B` | 59344 | `GOTAB+515` → `T109R` | **MISATTRIBUTED** (ND-100 IO routine, not this call) |
| level-12 GOSW table lookup | — (resident, uncarved) | — | — | `5CMNO+13` | **UNVERIFIED** (index inferred) |
| level-12 worker body `5MTRA` | [026-S3IMPIT.asm](../../segments-ref/026-S3IMPIT/026-S3IMPIT.asm) · [.hex](../../segments-ref/026-S3IMPIT/026-S3IMPIT.hex) | `143445B–143643B` (127 words) | 75338 | `5MTRA` | **VERIFIED** (real bytes) |

`prove-mon.py 515` walks the ND-100 GOTAB and lands on `T109R = 057605B` - a *different, unrelated*
ND-100 IO routine in another dispatch namespace that merely shares the number 515. The row above lists
it only to mark it MISATTRIBUTED so no one mistakes it for this handler.

**Verify by hand (the real `5MTRA` entry):** `grep '^143445 ' ../../segments-ref/026-S3IMPIT/026-S3IMPIT.hex`
→ byte offset `75338`; then
`dd if=../../../segments/026-S3IMPIT.bin bs=1 skip=75338 count=8 | od -An -tx1` → `1a 62 02 62 cc 5d 0a 61`
(= octal `015142 001142 146135 005141 …`, the `STX I 142 / STZ I 142 / RADD SB DA / STA I 141` prologue).

**Verify the MISATTRIBUTED GOTAB word:**
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=59344 count=2 | od -An -tx1` → `5f 85`
(= octal `057605B` = `T109R`, not this handler).

---

## Instruction walkthrough

Full listing: [`515B-MultipleDataTransfer.ASM`](515B-MultipleDataTransfer.ASM). One region, the
`5MTRA` worker body. Control flow is byte-verified; the semantic labels are inferred from
instruction shape.

**Entry prologue (`143445–143456`)** - save context, fetch the descriptor. On entry `B` holds the
ND-500 message/control-block base (`143447 RADD SB DA` copies it into A immediately, then
`143450 STA I 141`). A double-word field at `base+100B` is fetched (`AAX 100 / LDDTX`) and its low
word kept in A.

**Validity gate (`143457–143460`).** `BSKP ONE 0 DA` tests a bit of A; on failure
`143460 JMP I 135 -> 143615` branches to the exit fix-up path.

**Range / function classification (`143461–143512`).** Reloads a sub-field (`AAX 110 / LDATX`),
calls a shared worker (`143463 JPL I 133 -> 143616`, indirect), then a cascade of
`LDT <const> / SKP IF DX MGRE|MLST ST / JMP` comparisons against bounds `132 / 130 / 126 / 117`,
building a function selector in A (`SAA 1`, `RADD CLD 0 DA`). This is the classic "is the requested
function/sector inside the legal window?" ladder. Out-of-range takes:
```
143513  SAA 6             ; A := 6  (status/err 6)
143514  JMP I 111         ; -> 143625  (exit fix-up)
```

**Queue-slot handling (`143515–143546`).** With a valid classification it stores the slot index
(`STX I 111 / LDX 111`), loads a per-device slot word (`LDA ,X 13`) and branches on a busy/free flag
(`143520 JAF 27 -> 143547`). The FREE fall-through issues a burst of indirect worker calls
(`JPL I 106/105/104 -> 143630/143631/143632/143633`) interleaved with `RAND 0 0` guards, builds a
transfer descriptor (`LDT I 62 / AAX 147 / LDATX / … / STDTX`), does the sector arithmetic
(`SAD 6 / SUB I 73 / SAD ZIN SHR 5`) and hands off:
```
143545  STD I 70
143546  JMP I 71          ; -> 143637  (enqueue / start-transfer worker)
```

**Already-active branch (`143547–143606`).** Decrements an outstanding-count field
(`LDA ,B 13 / AAA -1 / STA ,B 13`), then decodes a device status word
(`LDX I 30 / LDT I 32 / AAX 111 / LDATX / AND 56`) onto ND-500 return-status codes via a
`SAT n / SKP IF DA EQL|UEQ ST / JMP` decision tree. The `SAT` immediates seen (`60, 1, 7, 61, 6, 66`)
are the candidate status values. Terminates with `143605 SAA 7 / 143606 JMP I 17 -> 143625`.

**Exit / fix-up tail (`143607–143643`).** A mix of pointer words and short store sequences
(`STT I …`, `STF I ,B 41`, `SWAP SP DD`) that restore the ND-500 message-buffer / process fields and
post the status word before returning to `MCHANDEL`. The last executable line is an indirect call
`143637 JPL I 67 -> 143726`, a shared worker OUTSIDE this window (excluded from closure - see
[Honest caveats](#honest-caveats)).

---

## Parameter / register contract

ND-500 level-12 calls pass arguments in the process's message/control block, not in ZAREG/ZXREG/ZTREG.

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `B` = message/control-block base | in | `143447 RADD SB DA`, `143450 STA I 141`; every field access is `,B`/`I 141` relative | **VERIFIED** (bytes) |
| double-word field at `base+100B` | in | `143452 AAX 100 / 143453 LDDTX` | VERIFIED offset; meaning **inferred** |
| function/sector value in `DX` | in | bounds ladder `SKP IF DX MGRE/MLST ST` vs `132/130/126/117` | VERIFIED compared; semantics **inferred** |
| per-device slot word `base+13B` | in/out | `143517 LDA ,X 13`; `143552/143554` decrement | **VERIFIED** (bytes) |
| device status word (via `I 30`/`I 32`) | in | `143557 LDX I 30 / 143560 LDT I 32 / 143562 LDATX / 143564 AND 56` | **VERIFIED** (bytes) |
| return status in `A` | out | `SAA 1/6/7`, `SAT 60/1/7/61/6/66` written then stored | VERIFIED literals exist; exact mapping **inferred** |

**Return-status literals seen (octal):** `1, 6, 7` (via `SAA`) and `60, 61, 66` (via `SAT`). The
older note "status 1/2/4 or errors 6/7" is only *partly* consistent - `2` and `4` are **not** present
as literals in this window (**UNVERIFIED**). The `5MNWA/5MFNC/5MLGN/5MDIS/5MEMA/5DSEC/5MNOS/5MREQ`
message-field names are plausible labels but were **not** resolved to byte offsets from ground truth
here - treat them as UNVERIFIED behaviour hints.

---

## Pseudo-code (for an emulator)

The pseudo-C is grounded in the ND-100 instruction-semantics reference
[`../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
the T/X transfers are 24-bit **physical** accesses `EL = ((T & 0377) << 16) | ((X + disp3) & 0177777)`
(T = the bank, MMU-bypassed), and `RADD CLD Sx Dy` = `y = x`. For this memory-transfer call
those physical transfers are the core of the handler.

See **[`515B-MultipleDataTransfer.pseudo.c`](515B-MultipleDataTransfer.pseudo.c)** - a pseudo-C model
of the handler for emulator authors. The control flow (validity gate, bounds ladder, busy/free fork,
status decode) is byte-verified; the field and status-code semantics are inferred from instruction
shape, not from a manual or a symbol-resolved field map.

---

## Honest caveats

**What is byte-proven:** the worker bytes at `143445B..143643B` are real SINTRAN L bytes for `5MTRA`
(L07 symbol `5MTRA = 143445B`, `SINTRAN/NPL-SOURCE/SYMBOLS/L07/SYMBOL-2-LIST.SYMB.TXT:3950`); they are
non-zero coherent code; every DIRECT branch resolves inside the window; the on-disk bytes match the
disassembly line-for-line (regenerated from the canonical segment in this pass).

**What is NOT proven:**
- **The dispatch link.** This is an ND-500 level-12 GOSW call, dispatched by `MCHANDEL` through the
  resident `5CMNO` GOSW table at index 13. That table is in an **uncarved** resident region, so the
  "index 13 → `5MTRA`" selection is inferred from the ND-500 dispatch structure, not a followed
  pointer. Confirming it needs a live trace (level-12 call 515B, single-step `MCHANDEL`, confirm P
  lands on `5MTRA = 143445B`).
- **The number collision.** `prove-mon.py 515` walks the *ND-100 GOTAB* and reports `T109R = 057605B`,
  a real but unrelated ND-100 IO routine. It cannot see the ND-500 level-12 GOSW table, so for a
  level-12 call number it necessarily lands on the wrong routine. `T109R` is MISATTRIBUTED here; it is
  not this handler.
- **The out-of-window worker.** `143637 JPL I 67 -> 143726` is an INDIRECT call to a shared worker
  outside the carved window; correctly excluded from closure, its body is not in these bytes.
- **Source-copy label.** The window is cited from `026-S3IMPIT.bin` (Image copy); the equivalent Save
  copy `017-S3SMPIT.bin` (both load `32000B`) is expected to be byte-identical over this range but was
  **not** re-verified byte-for-byte across both files in this pass (UNVERIFIED).
- **Field / status semantics.** The `5M*` field names and the exact function-code / status-code
  meanings are inferred from instruction shape, not resolved from ground truth (see the contract).

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
