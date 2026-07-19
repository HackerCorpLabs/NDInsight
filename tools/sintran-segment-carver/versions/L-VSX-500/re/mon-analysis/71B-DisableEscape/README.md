# MON 71B (octal) - DisableEscape (DESCF)

> **CORRECTED 2026-07-15 (byte-verified).** The worker + dispatch described below are on the
> DEBUNKED model and are WRONG (the old folder named an ND-500-side symbol; that is not the
> ND-100 worker). Byte truth from the carved L07 image:
> `MCTAB[71B] = 005711B = MCDES=047020B` in segment 003-S3CP, reached by the real dispatch
> `MON 71B -> ENT14(072167B) -> GOTAB[71B]=MFELL(072114B) -> CALLP(032201B) -> MCTAB[71B]=MCDES`.
> Verified: `dd if=044-S3IDPIT.bin bs=1 skip=1938 count=2` -> `4e 10`. Cross-ref
> ../317B-ExecuteCommand/README.md and SINTRAN/CARVING-HANDOFF.md sec 3a.

Disables the terminal **ESCAPE** (user-break) function. Normally the ESCAPE key terminates a running
program (a user break); after this call the escape character is treated as any other character. The
function is re-enabled by MON 72 (EnableEscape) or automatically when you log out. A logical device
number selects the terminal for RT programs; background programs always use their own terminal. This is
an ND-100 monitor call (also available on ND-500).

**Status:** `documented`. `GOTAB[71B] = 121417B` (byte-proven) points at the `F1643` entry stub in
overlay `025-S3IRPIT` - but in this real SINTRAN L image the 3-word `F1643` stub is **all zero**
(runtime-populated pointer cells, bounded by `NOWTS=121422B`), so the onward transfer to the actual
disable-escape routine is **not** byte-followable from a static decode. The named ND-100 worker is not
isolated in any carved segment (the `DESCF` symbol resolves only to the ND-500 side), so the worker body
is attached by **name** only and its code is **not recoverable** from these bytes (see
[Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`71B-DisableEscape.ASM`](71B-DisableEscape.ASM) - both regions (GOTAB word + the zero `F1643` stub).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 71B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[71B] = 121417B<br/>(byte-proven)"]
    C --> D["F1643 entry stub<br/>025-S3IRPIT :121417B (zero / runtime cells)"]
    D -.uncarved CALLPROC / runtime cells.-> E["disable-escape worker<br/>(uncarved; not isolated)"]
    E --> F["clear terminal user-break flag"]
    class A blue
    class B,C teal
    class D teal
    class E green
    class F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The dashed hop (`D ⇢ E`) is the resident `CALLPROC`/segment-switch reached through `F1643`'s
runtime-populated pointer cells - **not present in any carved segment**. `GOTAB[71B]` is a real non-zero
pointer (`121417B`), but the `F1643` stub it targets is 3 zero words in this static image, so a static
decode cannot follow it onward to the disable-escape worker. `DESCF` (E) is the documented short name for
this call, but it resolves only to the ND-500 side; no ND-100 worker body is present in these bytes.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words x 2; the
commoncode load base is `0` (so byte offset = `octal-addr x 2` decimal), and `025-S3IRPIT` load base is
`32000B`, so its byte offset is `(addr - 32000B) x 2`.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[71] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071324B` (1 word) | 58792 | `GOTAB+71` = `121417B` | **VERIFIED** |
| F1643 entry stub | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `121417B-121421B` (3 words) | 56862 | `F1643` | **NOT CARVED** (zero / runtime cells) |
| resident CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| disable-escape worker body | - (not isolated) | - | - | `DESCF` (ND-500 only) | body **MISATTRIBUTED** (name only) |

**Verify by hand:** the GOTAB word:
`grep '^71324 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
-> `71324  121417  243 017  58792` (the GOTAB word = `121417B`); then
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58792 count=2 2>/dev/null | od -An -tx1`
-> `a3 0f` (the raw stored bytes read as octal `121417`). For the `F1643` stub:
`grep '^121417 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex`
-> `121417  000000  000 000  56862`; then
`dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=56862 count=6 2>/dev/null | od -An -tx1`
-> `00 00 00 00 00 00` (all three `F1643` words are zero in this L image).
`prove-mon.py 71` reports the same `GOTAB[71]=121417 -> F1643` with a zero stub region.

---

## Instruction walkthrough

Full listing: [`71B-DisableEscape.ASM`](71B-DisableEscape.ASM).

**F1643 entry stub (121417-121421)** - a 3-word compiler stub (label `F1643`, next symbol
`NOWTS=121422B`). All three words are `000000` in this real SINTRAN L image; they are runtime-populated
pointer cells (zero at rest), so there is no static instruction body and no branch to follow. This is why
the transfer onward to the real disable-escape routine cannot be resolved by a static decode: it goes
through these cells and the resident `CALLPROC`, which lives in an uncarved overlay. There is no
executable worker region to walk through in these bytes.

---

## Parameter / register contract

Manual-side names/types are from [`71B_DISABLEESCAPE.yaml`](../../../../../../../Developer/MON/calls/71B_DISABLEESCAPE.yaml).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `T` (DeviceNumber) | in | terminal's logical device number; ignored for background programs (own terminal always selected) (`MAC` example `LDT DEVNO`) | inferred (manual) |
| error return | out | standard error code in `A` (appendix A) | inferred (manual) |

None of this is byte-proven here: the `F1643` stub is zero and no worker body is present, so the
device/flag contract comes entirely from the manual, not from these bytes.

---

## Pseudo-code (for an emulator)

See **[`71B-DisableEscape.pseudo.c`](71B-DisableEscape.pseudo.c)** - a pseudo-C model for emulator
authors. Because the `F1643` stub is zero and the worker is not isolated, the model is of the
**documented** behaviour only (clear the terminal user-break flag), explicitly flagged as not
byte-derived. The `GOTAB[71B] = 121417B` dispatch word is the only byte-verified fact. Any ND-100
instruction that a live image would run here would be translated per the canonical
[`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md), but no
worker instructions are present in these bytes to translate.

---

## Honest caveats

**What is byte-proven:** `GOTAB[71B] = 121417B` (level-14 dispatch; `prove-mon.py 71` reads commoncode
file byte `0xe5a8 = a3 0f = 121417`), a real non-zero pointer to `F1643`. The `F1643` region at
`121417B` is exactly 3 words (bounded by `NOWTS=121422B`) and every word is `000000` in this L image
(confirmed by `dd` = `00 00 00 00 00 00`).

**What is NOT proven:** everything past the GOTAB word. `F1643`'s three words are zero (runtime pointer
cells), so there is no static branch to follow; the onward hop goes through those cells and the resident
`CALLPROC` in an **uncarved** overlay. The documented ND-100 worker is not isolated in any carved
segment - the `DESCF` short name resolves only to the ND-500 side (`026-S3IMPIT` / `030-S3SM5`, address
`112111B`, N500-SYMBOLS), which is not the ND-100 body. So the disable-escape worker is attached by
**name** only and its ND-100 code is **not recoverable** from these carved bytes.

This reconciles into one story: the dispatch head (`GOTAB[71] -> F1643`) is solid; `F1643` is a real but
**zero-filled** runtime stub in this image; and the ND-100 disable-escape worker is not present in the
carved set. Confirming the real worker needs a live trace (break at `121417B` on a real `MON 71`,
single-step through the stub and CALLPROC, and record where P lands). MON 71 and MON 72 (EnableEscape)
are a disable/enable pair that likely share one terminal-flag body forked on a selector - inferred from
the pairing, not proven here.

**How this was carved:** the `F1643` stub was bounded by its own `SYMBOL-2-LIST` entry
(`F1643=121417B`) and the next symbol (`NOWTS=121422B`), then read from the canonical `025-S3IRPIT`
segment binary and found to be three zero words. Method:
[../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
