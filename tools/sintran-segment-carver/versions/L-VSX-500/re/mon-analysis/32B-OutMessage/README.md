# MON 32B (octal) - OutMessage (MSG)

Writes a message string to the calling program's terminal (maximum 512
characters). This is convenient for error messages from background programs. On
the ND-100 and ND-500 it is available to all users; the message text is passed
by address and written character-by-character to the user's terminal.

**Status:** GOTAB dispatch head byte-proven as **fall-through** (`GOTAB[32B] =
000000`, no per-call stub); the `MSG` worker body is real SINTRAN L bytes in
segment `025-S3IRPIT` (the SYMBOL-2-LIST code overlay - the parallel `026-S3IMPIT`
overlay holds unrelated float/data at the same address). The exact `MON 32 ->
worker` link crosses an uncarved kernel bridge (see [Honest caveats](#honest-caveats)).
All addresses/values are **octal**.

- **Full disassembly:** [`32B-OutMessage.ASM`](32B-OutMessage.ASM) - the actual code (the MSG worker body; there is no entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 32B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[32B] = 000000<br/>(byte-proven: fall-through)"]
    C -.uncarved MFELL / CALLPROC.-> E["MSG out-message worker<br/>025-S3IRPIT :102453B"]
    E --> F["character loop:<br/>LBYT fetch + emit"]
    class A blue
    class B,C blue
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The GOTAB slot is zero, so there is **no per-call entry stub**. The dashed hop
(`C -> E`) is the resident `MFELL`/`CALLPROC` fall-through second-level dispatch -
it is **not present in any carved segment**, so it is the one link that cannot be
followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words x 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[32] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071265B` (1 word) | 58730 | `GOTAB+32` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `MFELL`/`CALLPROC` | **UNVERIFIED** |
| MSG out-message worker body | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) - [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `102453B-102514B` (code) + `102515B-102524B` (link cells) | 41558 | `MSG` | real bytes; link **MISATTRIBUTED** |

The window is bounded strictly to the next symbol `GERDV=102525B` (42 words).
Words `102453B-102514B` are code; `102515B-102524B` are a pointer table (link
cells) the `JPL I` / `JMP I` indirections dereference - `nd100-dis` renders them
as bogus instructions but they are **data**.

**Verify by hand:** `grep '^102453 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex` -> byte offset `41558`;
then `dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=41558 count=8 | od -An -tx1` -> `ba 23 51 08 49 0f cc 69`
(= octal `135043 050410 044417 146151` = `JPL I 43` / `LDT ,B 10` / `LDA ,B 17` / `RADD CLD SA DD`, the MSG entry).

The GOTAB slot itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58730 count=2 | od -An -tx1` -> `00 00` (= `000000`, fall-through).
`prove-mon.py 32` reads the same GOTAB zero.

---

## Instruction walkthrough

Full listing: [`32B-OutMessage.ASM`](32B-OutMessage.ASM). The body is the `MSG`
worker (there is no F16xx stub because `GOTAB[32] = 0`).

**Entry + setup (`102453-102462`)** - `102453 JPL I 43` (link cell `102516`)
calls the shared resident prologue worker; `102454-102461` build the terminal
output descriptor from the local `B`-frame; `102462 MST PIE` masked-sets the
interrupt-enable register from `A`.

**Character loop (`102463-102504`)** - `102463` clears `X` (the index);
`102464 LDT 34` loads the remaining character count; `102465 SKP IF DT GRE SX`
continues while `T > X`. Each iteration `102470 LBYT` fetches one character;
`102471 SAT 47` / `102474 SAT 44` compare it against the control bytes `47B`/`44B`
and `102500`/`102502 JPL -31/-33 -> 102447` call the preceding in-segment emit
routine; `102503 AAX 1` advances and `102504 JMP -20 -> 102464` loops.

**Finish (`102505-102514`)** - `102505 JPL I 14` (link `102521`) is a resident
worker; `102506 RADD CLD SX DA` / `102507 SHA ZIN SHR 1` halves the emitted count
(bytes -> words); `102511 ADD ,B 10` / `102512 STA ,B 10` fold it back into the
frame; `102513 JPL I 7` (link `102522`) and the tail `102514 JMP I 7`
(link `102523`) return through the resident exit.

---

## Parameter / register contract

Manual-side names/types are from [`32B_OutMessage.yaml`](../../../../../../../Developer/MON/calls/32B_OutMessage.yaml).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `102453B` = MSG worker entry (fall-through, no stub) | VERIFIED (bytes) |
| `X` (manual) | in | address of the string to write to the terminal (`LDX (TEXT`) | inferred (manual MAC example) |
| Message | in | STRING, max 512 characters | inferred (manual) |
| `T` | internal | character count driving the scan loop (`LDT 34`, `SKP IF DT GRE SX`) | VERIFIED (bytes); count source inferred |
| control bytes `47B`/`44B` | internal | per-character class test (`SAT 47`/`SAT 44`) | VERIFIED (bytes); meaning inferred |
| `B+10` | internal/out | running emitted-word count (`ADD ,B 10` / `STA ,B 10`) | VERIFIED (bytes) |

The user-visible `X` = string-address convention lives in the caller-side
`MON 32` wrapper and the uncarved `MFELL`/`CALLPROC` frame, so the precise
register-to-field assignment is **inferred** from the manual, not byte-proven
here.

---

## Pseudo-code (for an emulator)

See **[`32B-OutMessage.pseudo.c`](32B-OutMessage.pseudo.c)** - a pseudo-C model of
the handler for emulator authors. The control flow (character loop, `LBYT` fetch,
the two byte comparisons, the emit calls, the halving shift, and the indirect
resident tail) is byte-verified; the register/field semantics and the exact
`47B`/`44B` character-class meaning are inferred from the manual and the code
shape.

Every instruction in the pseudo-code is translated against the canonical
[ND-100 instruction semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(`RADD CLD` copy idiom, `MST PIE` masked-set, `LBYT` byte load, `SKP` compare
senses, `SHA ZIN SHR` logical shift, addressing-mode effective addresses).

---

## Honest caveats

**What is byte-proven:** `GOTAB[32B] = 000000` (level-14 dispatch, a fall-through
with no per-call vector); the `MSG` worker body at `102453B` in `025-S3IRPIT` is
real code (entry bytes `135043 050410 044417 146151` match the disassembly); and
it is a terminal output loop (`LBYT` fetch + per-character emit + word-count
fold-back), consistent with OutMessage.

**Which overlay and why:** `MSG=102453B` is a `SYMBOL-2-LIST` symbol.
`025-S3IRPIT` is the `SYMBOL-2-LIST` code overlay - its companion MON-32-family
code is real here, whereas the parallel `026-S3IMPIT` overlay holds unrelated
float/data words at `102453B` (`FAD ,X ,B ...`). So `025-S3IRPIT` is the segment
that carries the real MSG worker; `026-S3IMPIT` is a different overlay image at
the same load base.

**What is NOT proven:** the link from the zero GOTAB slot to the `MSG` worker.
Because the vector is zero there is no stub to disassemble and no pointer to
dereference; dispatch drops into the resident `MFELL`/`CALLPROC` second-level
path, which lives in an **uncarved overlay**. So the `MON 32 -> MSG` attribution
rests on the `MSG` symbol name + the matching output behaviour, not a followed
pointer - hence **MISATTRIBUTED** in the strict sense. The `JPL -31/-33 -> 102447`
calls target the preceding in-segment routine (a shared emit worker); the link
cells `102516/102517/102521/102522/102523` resolve to low resident addresses
(`000374`, `004177`, `004116`, `001135`, `010341`) that are outside `025-S3IRPIT`
and are **not resolved** here. Confirming the dispatch link needs a live trace:
issue a real `MON 32`, single-step the level-14 fall-through into the resident
`CALLPROC`, and confirm P lands on `MSG = 102453`.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
