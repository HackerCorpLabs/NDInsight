# MON 313B (octal) - InBufferState (IBRSIZ)

Returns terminal input-buffer state: the number of characters currently held in the input
ring, and the number of characters before the next break (BRKMAX-limited). Terminals walk the
input ring; TAD and internal-buffer devices take other paths. Files are not valid here.

**Status:** GOTAB[313] level-14 dispatch is byte-proven and live-confirmed; the IBRSIZ worker
body is real SINTRAN L bytes; the exact `MON 313 -> IBRSIZ` link crosses an uncarved runtime
CALLPROC/second-level dispatch (see [Honest caveats](#honest-caveats)). All addresses/values
are **octal**.

- **Full disassembly:** [`313B-InBufferState.ASM`](313B-InBufferState.ASM) - the actual code, both regions (GOTAB entry stub + IBRSIZ worker body).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 313B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[313B] = 112164B<br/>(byte + live proven)"]
    C --> D["112164B entry stub<br/>025-S3IRPIT<br/>JMP I 10 -> 112247B"]
    D -.runtime CALLPROC.-> E["IBRSIZ worker body<br/>025-S3IRPIT :110543B"]
    E --> F["walk input ring<br/>held / before-break via LBYT"]
    class A blue
    class B,C,D teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The dashed hop (`D ⇢ E`) is the resident second-level / `CALLPROC`-style computed jump - the
stub at `112164B` byte-forwards to `112247B` (a monitor/interrupt-entry routine), and the
final worker is register-loaded at runtime. That `MON# -> worker` link is **not present as a
static pointer in any carved segment**, so it cannot be followed from bytes.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words × 2.
Load bases: `SINTRAN-DATA_commoncode` = `0`, `025-S3IRPIT` = `32000B`.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[313] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071546B` (= `071233B`+313, 1 word) | 59084 | `GOTAB+313` = `112164B` | **VERIFIED** |
| Entry stub (`JMP I 10` -> ptr `112174B` = `112247B`) | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `112164B..112174B` | 49384 | SYMBOL-2-LIST mislabels as `DIA3` | **VERIFIED** |
| Reached routine (monitor/interrupt entry) | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `112247B..112264B`+ | 49486 | unnamed (list misaligned here) | VERIFIED bytes |
| runtime CALLPROC / 2nd-level dispatch | — (register-loaded target) | — | — | `CALLPROC` | **UNVERIFIED** |
| IBRSIZ worker body (carved window, 149 words) | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `110543B..110770B` | 47814 | `IBRSIZ` / `T2P06` (L07) | real bytes; link **MISATTRIBUTED** |

**Verify by hand (GOTAB entry):** `grep -E '^112164 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex`
-> byte offset `49384`; then
`dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=49384 count=8 2>/dev/null | od -An -tx1`
-> `aa 08 cc 4d ba 07 ab 17` (= octal `125010 146115 135007 ...` = the `JMP I 10` stub).
The GOTAB word itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=59084 count=2 2>/dev/null | od -An -tx1`
-> `94 74` (big-endian `0x9474` = `112164B`).

**Verify by hand (worker body):** `grep -E '^110543 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex`
-> byte offset `47814`; then
`dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=47814 count=8 2>/dev/null | od -An -tx1`
-> `ba 52 ba 52 aa 52 c4 05` (= octal `135122 135122 125122 ...` = the `IBRSIZ: CALL GET0 / CALL GZTREG` prologue).

---

## Instruction walkthrough

Full listing: [`313B-InBufferState.ASM`](313B-InBufferState.ASM). Region A is the level-14
entry; Region B is the functional IBRSIZ body.

**GOTAB entry (`112164B`)** - the level-14 dispatch word `GOTAB[313] = 112164B` (live-confirmed
against the running system). The word at `112164B` is `125010` = `JMP I 10`, an indirect jump
whose pointer word is at `112164B + 10B = 112174B`. That pointer reads `112247B`, so the stub
forwards to `112247B` (a different routine - a monitor/interrupt-entry sequence, `SAA 2` /
`ORA 12` / critical-section setup), which itself continues via `112254B JMP I 10` -> pointer
`112264B` = `112632B` and onward through register-indexed computed jumps.

**IBRSIZ prologue (`110543B..110547B`)** - `110543 JPL I 122` dereferences pointer word
`110665B = 000374B` (**GET0**, the NPL `IBRSIZ: CALL GET0` prologue), immediately followed by
`110544 JPL I 122` -> `110666B = 044276B` (**GZTREG**, fetch the logical device number). A
`SKP IF DA UEQ 0` guard (`110546`) bails for non-eligible devices.

**Ring scan (`110555B..110662B`)** - an `IOF` critical section (`110555`) walks the input ring
uninterrupted. The inner loop (`110606..110630`) uses `LBYT` (`110614`) to fetch each input byte
and `AAX 1` to advance, counting characters held and characters-before-break, honoring BRKMAX
via the compare/skip chain around `110573..110576` and `110645..110662`.

**Result (`110663B` onward)** - `110663 LDA ,B 16` loads the held-count; the before-break count
is staged through `110723..110762`, and `110762 JMP I ,B 34` returns to the caller.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `ZTREG` (`T`) | in | logical device number, fetched via `GZTREG` (`110544`) | VERIFIED (bytes: `CALL GZTREG`) |
| `ZAREG` (`A`) | out | BHOLD = characters held in input buffer (`110663 LDA ,B 16`) | inferred (NPL IBRSIZ) |
| `ZXREG` (`X`) | out | BCOUNT = characters before break (BRKMAX-limited) | inferred (NPL IBRSIZ) |
| error path | out | `ZAREG = 240B` on error | inferred (NPL, not proven in these bytes) |
| `IOF`/`ION` | internal | ring walked in a critical section (`110555`) | VERIFIED (bytes) |

The exact user-visible A/X/T assignment lives in the caller-side `MON 313` wrapper and the
uncarved second-level dispatch frame, so the precise register convention is **inferred**, not
byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`313B-InBufferState.pseudo.c`](313B-InBufferState.pseudo.c)** - a pseudo-C model of the
handler for emulator authors. The worker-body control flow (GET0/GZTREG prologue, the `IOF`
ring scan, held / before-break accounting) is byte-verified; the semantic labels (which register
holds which count, the exact break arithmetic) are inferred from the NPL IBRSIZ structure.

Every instruction in the `.pseudo.c` is translated against the canonical
[`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(`IOF` opens the ring-scan critical section but NO matching `ION` is in the carved window - the
re-enable is out of carve; `LBYT` fetches one input byte; `SKP`/`BSKP` skip polarity, do not invert).

---

## Honest caveats

**What is byte-proven:** `GOTAB[313] = 112164B` (level-14 dispatch, matches a live read of the
running system); the `112164B` stub is real code (`JMP I 10`) whose pointer word `112174B`
reads `112247B`; the routine at `112247B` is real bytes (a monitor/interrupt-entry sequence);
and the IBRSIZ worker body at `110543B..110770B` is real SINTRAN L bytes, structurally IBRSIZ
(`CALL GET0` / `CALL GZTREG` prologue, `IOF` ring scan, `LBYT` byte walk).

**What is NOT proven:** the link from the GOTAB dispatch to the `110543B` IBRSIZ body. On the
static evidence they do not connect:
- `GOTAB[313]` resolves (via the `112164B` stub) to `112247B`, **not** `110543B`.
- Scanning all GOTAB entries `0..377B`: none point to `110543B`, and no single `JMP I` stub
  forwards there.
- The value `110543` appears as a pointer word **nowhere** in `025-S3IRPIT.bin`, so no
  `JMP I`/`JPL I` in the segment can dereference to it.
- `110543B` is ~`1400B` words below `112247B`, far outside the ±127-word reach of a direct
  jump.

So the `MON 313 -> IBRSIZ` attribution rests on the symbol name + the matching InBufferState
behaviour (GET0/GZTREG/ring-scan), reachable only via a runtime computed jump (`CALLPROC`
second-level dispatch) - hence **MISATTRIBUTED** in the strict sense. The earlier `ANALYSIS`
claim of a direct `GOTAB[313] -> 110543B` path and the `DISPATCH` byte-trace disagree; the
byte-trace WINS - the true static target is `112247B`, and `110543B` is the structurally-correct
worker whose runtime reachability is unproven. Confirming it needs a live trace: break at
`112164B` on a real `MON 313`, single-step `112164 -> 112247` (via `112174`) and through the
`112247/112632` computed jumps, and capture whether the final indexed `JMP/JPL` lands on
`110543B` or on a different worker.

> Address note: the NPL source is a different revision (NPL `IBRSIZ=110642B` vs L07 `110543B`).
> Match behaviour/structure, not raw bytes.

---

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
