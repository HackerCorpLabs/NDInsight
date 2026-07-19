# MON 72B (octal) - EnableEscape (EESCF)

> **CORRECTED 2026-07-15 (byte-verified).** The worker + dispatch described below are on the
> DEBUNKED model and are WRONG (the old folder named the ND-500-side symbol EESCF; that is not
> the ND-100 worker). Byte truth from the carved L07 image:
> `MCTAB[72B] = 005712B = MCEES=047022B` in segment 003-S3CP, reached by the real dispatch
> `MON 72B -> ENT14(072167B) -> GOTAB[72B]=MFELL(072114B) -> CALLP(032201B) -> MCTAB[72B]=MCEES`.
> Verified: `dd if=044-S3IDPIT.bin bs=1 skip=1940 count=2` -> `4e 12`. Cross-ref
> ../317B-ExecuteCommand/README.md and SINTRAN/CARVING-HANDOFF.md sec 3a.

Re-enables the terminal **ESCAPE** (user-break) function after MON 71 (DisableEscape). The ESCAPE key
normally terminates a running program (a user break); this call restores that behaviour. The function is
also enabled automatically when you log out. A logical device number selects the terminal for RT
programs; background programs always use their own terminal. This is an ND-100 monitor call.

**Status:** `documented`. `GOTAB[72B] = 000000` (byte-proven) is a **fall-through**: there is no direct
GOTAB handler word, so the level-14 handler is reached through the resident MFELL/CALLPROC path
(uncarved). The named ND-100 worker is **not isolated** in any carved segment - the `EESCF` short name
resolves only to the ND-500 side (`112123B`, N500-SYMBOLS), which is not the ND-100 body. So the
enable-escape worker is attached by **name** only and its ND-100 code is **not recoverable** from these
bytes (see [Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`72B-EnableEscape.ASM`](72B-EnableEscape.ASM) - the GOTAB dispatch word (fall-through); no worker body is present in the carved set.
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 72B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[72B] = 000000<br/>(fall-through, byte-proven)"]
    C -.uncarved MFELL / CALLPROC.-> D["enable-escape worker<br/>(uncarved; not isolated)"]
    D --> E["set terminal user-break flag"]
    class A blue
    class B,C blue
    class D teal
    class E green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The dashed hop (`C ⇢ D`) is the resident `MFELL`/`CALLPROC` fall-through - it is **not present in any
carved segment**, so it is the one link that cannot be followed statically. `GOTAB[72B]` is literally
`000000`, so there is no entry stub to disassemble; dispatch enters the resident handler, which then
sets the terminal flag. `EESCF` (D) is the documented short name for this call, but it resolves only to
the ND-500 side; no ND-100 worker body is present in the carved set.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words x 2; the
commoncode load base is `0`, so the byte offset is simply `octal-addr x 2` (decimal).

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[72] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071325B` (1 word) | 58794 | `GOTAB+72` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `MFELL`/`CALLPROC` | **UNVERIFIED** |
| enable-escape worker body | - (not isolated) | - | - | `EESCF` (ND-500 only) | body **MISATTRIBUTED** (name only) |

There is no entry-stub row: `GOTAB[72]` is `000000`, so the level-14 handler is a resident fall-through,
not a `025-S3IRPIT` stub. (Its pair, MON 71 DisableEscape, does have a non-zero `GOTAB[71]=121417B`
pointing at the zero `F1643` stub - see [71B-DisableEscape/](../71B-DisableEscape/).)

**Verify by hand:** the GOTAB word is a zero (fall-through):
`grep '^71325 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
-> `71325  000000  000 000  58794`; then
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58794 count=2 2>/dev/null | od -An -tx1`
-> `00 00` (GOTAB[72] = 000000, a fall-through).
`prove-mon.py 72` reports the same `GOTAB[72]=000000` fall-through.

---

## Instruction walkthrough

Full listing: [`72B-EnableEscape.ASM`](72B-EnableEscape.ASM). There is no worker code to walk through:
`GOTAB[72]` is `000000` (a resident fall-through, no entry stub), and no ND-100 enable-escape worker is
isolated in the carved set (the `EESCF` symbol is ND-500 only). The `.ASM` therefore lists only the
byte-proven `GOTAB` dispatch word plus notes on why no body is present.

---

## Parameter / register contract

Manual-side names/types are from [`72B_EnableEscape.yaml`](../../../../../../../Developer/MON/calls/72B_EnableEscape.yaml).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `T` (DeviceNumber) | in | terminal's logical device number; ignored for background programs (own terminal always selected) (`MAC` example `LDT DEVNO`) | inferred (manual) |
| error return | out | standard error code in `A` (appendix A) | inferred (manual) |

None of this is byte-proven here: the dispatch is a fall-through and no worker body is present, so the
device/flag contract comes entirely from the manual, not from these bytes.

---

## Pseudo-code (for an emulator)

See **[`72B-EnableEscape.pseudo.c`](72B-EnableEscape.pseudo.c)** - a pseudo-C model for emulator authors.
Because the dispatch is a fall-through and the worker is not isolated, the model is of the **documented**
behaviour only (set the terminal user-break flag), explicitly flagged as not byte-derived. The
`GOTAB[72B] = 000000` fall-through is the only byte-verified fact. Any ND-100 instruction a live image
would run here would be translated per the canonical
[`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md), but no
worker instructions are present in these bytes to translate.

---

## Honest caveats

**What is byte-proven:** `GOTAB[72B] = 000000` (level-14 dispatch; `prove-mon.py 72` reads commoncode
file byte `0xe5aa = 00 00 = 000000`), a fall-through. There is no entry stub and no worker region to
disassemble.

**What is NOT proven:** the enable-escape worker body. The `GOTAB[72]` fall-through enters the resident
`MFELL`/`CALLPROC`, which lives in an **uncarved** overlay; a static decode cannot follow it. The
documented ND-100 worker is not isolated in any carved segment - the `EESCF` short name resolves only to
the ND-500 side (`026-S3IMPIT` / `030-S3SM5`, address `112123B`, N500-SYMBOLS), which is not the ND-100
body. So the enable-escape worker is attached by **name** only and its ND-100 code is **not recoverable**
from these carved bytes.

This reconciles into one story: the dispatch head (`GOTAB[72] = 000000`, fall-through) is solid; the
resident second-level dispatch is uncarved; and the ND-100 enable-escape worker is not present in the
carved set. MON 72 (enable) and MON 71 (disable) are a pair that likely share one terminal-flag body
forked on a selector - inferred from the pairing, not proven here. Confirming the real worker needs a
live trace (break on a real `MON 72`, single-step the fall-through and CALLPROC, and record where P lands).

**How this was carved:** the GOTAB word was located at `GOTAB base 071233B + 072 = 071325B` in the
canonical resident commoncode binary and read directly (`000000`, fall-through); the carved segment set
was then searched for an ND-100 enable-escape worker and none was found (the `EESCF` symbol is ND-500
only). Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) ·
dispatch reality: [../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map:
[../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
