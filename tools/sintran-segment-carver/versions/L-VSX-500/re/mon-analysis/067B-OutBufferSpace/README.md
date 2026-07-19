# MON 67B (octal) - OutBufferSpace (OSIZE)

Returns the number of bytes that can still be written to a device's output buffer before the
calling program must wait - i.e. **bytes FREE, not bytes used**. Not available for internal
devices. This is an ND-100 monitor call.

**Status:** dispatch head byte-proven (`GOTAB[67B] = 121372B = F1642`); the candidate worker body
(`OSIZE = 044231B`) is real SINTRAN L bytes but is **not** statically reachable from the F1642 stub -
the `MON 67 -> OSIZE` link crosses an uncarved kernel bridge (see [Honest caveats](#honest-caveats)).
All addresses/values are **octal**.

- **Full disassembly:** [`067B-OutBufferSpace.ASM`](067B-OutBufferSpace.ASM) - the actual code, both regions (entry stub + OSIZE worker).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 67B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[67B] = 121372B<br/>(byte-proven)"]
    C --> D["F1642 entry stub<br/>025-S3IRPIT :121372B"]
    D -.uncarved CALLPROC.-> E["OSIZE buffer-scan worker<br/>commoncode :044231B"]
    E --> F["per-device free-space<br/>via IOXT + POF/PON reads"]
    class A blue
    class B,C,D teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The dashed hop (`D ⇢ E`) is the resident `CALLPROC`/segment-switch - it is **not present in any
carved segment**, so it is the one link that cannot be followed statically. F1642 sets up registers,
`MST PIE` / `EXIT`, and re-enters via a runtime-populated pointer; its three direct jumps all stay
inside overlay 025 and its one readable indirect pointer (`116036B`) is **not** `044231`.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words x 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[67] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071322B` (1 word) | 58788 | `GOTAB+67` = `121372B` | **VERIFIED** |
| F1642 entry stub | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `121372B-121417B` (21 words) | 56820 | `F1642` | **VERIFIED** |
| resident CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| OSIZE worker body (candidate) | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `044231B-044427B` (127 words) | 37170 | `OSIZE` | real bytes; link **MISATTRIBUTED** |

There is no `MFELL -> CALLPROC` fall-through row: `GOTAB[67]` is non-zero (`121372B`), so the
level-14 handler is the F1642 stub, not a resident fall-through path.

**Verify by hand:** `grep '071322' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
-> `71322  121372  242 372  58788` (the GOTAB word = `121372B`); then
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58788 count=2 2>/dev/null | od -An -tx1`
-> `a2 fa` (= octal `121372`). For the stub first word:
`dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=56820 count=2 2>/dev/null | od -An -tx1`
-> `cc 6f` (= octal `146157`, F1642's first word).

---

## Instruction walkthrough

Full listing: [`067B-OutBufferSpace.ASM`](067B-OutBufferSpace.ASM). The functional body is the OSIZE
worker (region B); the F1642 stub (region A) is the level-14 entry.

**F1642 entry stub (121372-121417)** - a 21-word compiler wrapper (label `F1642`, next symbol
`F1643=121417B`). It stages saved registers (`RADD CLD SA DX/DB/DL`), enables interrupts
(`121400 MST PIE`) and returns (`121401 EXIT`). Three direct jumps (`JMP 4 -> 121410`,
`JMP -44 -> 121343`, `JMP -17 -> 121371`) all land inside overlay 025; the one indirect call
(`121405 JPL I 10`) dereferences pointer word `@121415 = 116036B`, a runtime-populated value that
does **not** resolve to the OSIZE body. Words `121411-121416` decode as instructions but are
constant/pointer data, and `121417..` is `000000` padding. So beyond those resolved hops the stub's
control flow is not byte-closable from a static decode.

**OSIZE worker prologue (044231-044240)** - frame/pointer setup relative to `B`, clears working
locals (`STZ`), moves the I/O interrupt-enable words into CPU regs (`LDA 107 / TRR IIE / TRA IIC`),
and sets `X` as the loop index at the loop head `044240`.

**Per-device scan loop (044240-044342)** - loads a double-word device-table entry
(`044241 LDD I ,X 104`, stride 2 words), tests an end-of-table sentinel
(`044243 SKP IF DA UEQ ST` -> `JMP 77 -> 044343`), and skips empty slots (`044245 JAZ -> 044337`).
For a live device it does a device I/O status read (`044250-044252 LDT / AAT 2 / IOXT`) and branches
on device class (`044255 JAZ -> 044304`).

**Buffer-pointer arithmetic, two class paths**
- `044256-044303`: masks fields (`AND 67`, `SHA ZIN SHR 6`), reads the buffer descriptor with
  interrupts **off** (`044267 POF ... 044270 LDD ,X 0 ... 044271 PON`), compares head vs tail, and
  conditionally zeroes the pair (buffer empty) before `044303 JMP 34 -> 044337`.
- `044304-044330`: alternate path selected by `BSKP ONE 60/100 DD` bit tests; at `044320`
  `JPL I 27 -> 044347` is an indirect call through the pointer word at `044347`. Computes a
  masked/shifted value and stores it via `STA ,X 27`.

**Result store (044331-044336)** - `RADD SD DA / SWAP SD DA` assemble the free-byte count into `A`,
then `STD I ,X 15` stores the result (paging-mode bit `SSPTM` toggled around the store).

**Loop advance (044337-044342)** - index `+= 2` (`AAA 2`), `JMP -102 -> 044240` back to the loop head.

**Data / tail (044343-044427)** - `044343 JMP 10 -> 044353` jumps over a data/pointer block
(`044344-044352`, e.g. pointer word `000215B` at `044347`). The tail `044353-044427` is a second
scan with more indirect worker calls and `IOT`-style transfers; its exact role (a sibling buffer
routine sharing the segment) is **UNVERIFIED**. There is no explicit `EXIT`/skip-return inside
`044231-044336`, consistent with a called worker rather than a top-level MON entry.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `A` | in | logical device number (1 = own terminal) | inferred (manual behaviour; not isolable from this carve) |
| `A` | out | number of bytes free in the output buffer | inferred; the count is assembled by `RADD SD DA / SWAP SD DA` and stored via `STD I ,X 15` (the compute is VERIFIED from bytes) |
| device table | in | scanned via `LDD I ,X 104`, stride 2 words | VERIFIED (bytes) |
| `IOXT` | - | per-device status read inside the loop | VERIFIED (bytes) |
| skip return / error | out | no in-body skip-return; internal-device / illegal-LDN errors would be raised by the F1642 entry wrapper | inferred |

The exact ZAREG/ZXREG/ZTREG save-area mapping and the user-visible A/X/T convention live in the
caller-side `MON 67` wrapper and the uncarved CALLPROC frame, so the precise register contract is
**inferred**, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`067B-OutBufferSpace.pseudo.c`](067B-OutBufferSpace.pseudo.c)** - a pseudo-C model of the
handler for emulator authors. The control flow of both regions is byte-verified; the semantic labels
(which field is head/tail, the A-in/A-out register roles) are inferred from the call structure and
the manual, and the F1642 -> OSIZE bridge is modelled but not proven. Every instruction is translated
per the canonical
**[ND-100 Instruction Semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)**.

---

## Honest caveats

**What is byte-proven:** `GOTAB[67B] = 121372B` (level-14 dispatch; `prove-mon.py 067` reads
commoncode file byte `0xe5a4 = a2 fa = 121372`). The `F1642` stub at `121372B` is real code (its
first word `146157B` matches the disassembly, and `F1643=121417B` bounds it at 21 words). The `OSIZE`
worker entry bytes at `044231B` (`060730B ...`) match the disassembly and describe a per-device
buffer scan using `IOXT`, interrupt-guarded (`POF/PON`) double-word reads, and pointer arithmetic that
assembles a count - consistent with computing output-buffer free space.

**What is NOT proven:** the link from the `F1642` stub (in `025-S3IRPIT`) to the `OSIZE` worker (in
resident `commoncode`). F1642's three direct jumps stay inside overlay 025 (`121410/121343/121371`),
and its one readable indirect pointer is `116036B` - **not** `044231`. F1642 returns via
`MST PIE / EXIT`; the transfer to the resident worker goes through the resident `CALLPROC`/second-level
dispatch, which lives in an **uncarved overlay**. So the `MON 67 -> OSIZE` attribution rests on the
symbol name + the matching "free output-buffer space" behaviour, not a followed pointer - hence
**MISATTRIBUTED** in the strict sense.

This reconciles the two earlier notes into one story: the dispatch head (`GOTAB -> F1642`) is solid;
the OSIZE body is real bytes and a plausible worker; only the middle hop is open. Note also a numeric
coincidence - `044231` falls inside overlay 025's word span, but `OSIZE=044231` is defined in the
resident image (base 0), a different physical location; do not conflate them. A second `OSIZE=111254`
exists in `N500-SYMBOLS` (ND-500 side); this folder documents the ND-100 resident `044231` body only.
Confirming the bridge needs a live trace (break at `121372B` on a real `MON 67`, single-step the
segment switch, confirm P lands on `OSIZE=044231`).

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
