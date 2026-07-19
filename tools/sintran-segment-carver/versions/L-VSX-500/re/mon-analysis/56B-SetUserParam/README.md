# MON 56B (octal) - SetUserParam (PASET)

Sets the 5 user parameters of a background program (termination-handling information).
GetUserParam (MON 57B) reads them back. Background programs only.

**Status:** GOTAB dispatch head byte-proven as **fall-through** (`GOTAB[56B] = 000000`,
no per-call stub); the `MPASE` worker body is real SINTRAN L bytes in the second-level
monitor segment `025-S3IRPIT` (a `SYMBOL-2-LIST` symbol). It is a **shared set/get
body**: `MPASE=102363B` (this call) clears the `SSK` skip flag for the SET direction,
the twin `MPAGE=102365B` (GetUserParam, MON 57B) sets it for GET; both copy 5 words
between the user array and the `USPAR` system array via `MOVUS`. The code closes at
`102406B` (indirect return), bounded by the next symbol `RERRP=102413B`. The exact
`MON 56 -> worker` link crosses an uncarved kernel bridge (see
[Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`56B-SetUserParam.ASM`](56B-SetUserParam.ASM) - the actual code (the shared MPASE/MPAGE worker; there is no entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 56B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[56B] = 000000<br/>(byte-proven: fall-through)"]
    C -.uncarved MFELL / CALLPROC.-> E["MPASE set-param worker<br/>025-S3IRPIT :102363B (SSK=0)"]
    E --> F["get array; MST PIE;<br/>MOVUS 5 words user->system"]
    class A blue
    class B,C blue
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The GOTAB slot is zero, so there is **no per-call entry stub**. The dashed hop
(`C -> E`) is the resident `MFELL`/`CALLPROC` fall-through second-level dispatch - it
is **not present in any carved segment**, so it is the one link that cannot be followed
statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal
words x 2; for `025-S3IRPIT` (load base `32000B`) it is `(addr - 32000B) x 2`.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[56] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071311B` (1 word) | 58770 | `GOTAB+56` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `MFELL`/`CALLPROC` | **UNVERIFIED** |
| MPASE/MPAGE worker body | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) - [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `102363B-102406B` (code) + `102407B-102412B` (link cells) | 41446 | `MPASE` (SSK=0) / `MPAGE`=102365B (SSK=1) | real bytes = **CODE**; body link **MISATTRIBUTED** |

The window is bounded strictly by the next symbol `RERRP=102413B` (the MON 207B
read-error-parameters handler). Words `102363B-102406B` are code and
`102407B-102412B` are a pointer table (link cells) - one of them, `102410B`, is the
`USPAR=145445B` constant loaded by the copy setup; they are **data**.

**Verify by hand:** `grep '^102363 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex`
-> byte offset `41446`; then
`dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=41446 count=2 2>/dev/null | od -An -tx1`
-> `f8 10` (the stored word = octal `174020`, a genuine `BSET ZRO SSK` instruction,
the MPASE set entry). The GOTAB slot itself:
`grep '^71311 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
-> `71311  000000  000 000  58770`; then
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58770 count=2 2>/dev/null | od -An -tx1`
-> `00 00` (= `000000`, fall-through). `prove-mon.py 56` reads the same GOTAB zero.

---

## Instruction walkthrough

Full listing: [`56B-SetUserParam.ASM`](56B-SetUserParam.ASM). The body is the shared
`MPASE`/`MPAGE` worker (there is no F16xx stub because `GOTAB[56] = 0`).

**Entry split (102363-102366)** - `102363 BSET ZRO SSK` is the SET entry (MPASE, this
call), clearing the skip flag; `102365 BSET ONE SSK` is the GET entry (MPAGE, MON 57B),
setting it; both fall through to `102366 JPL I 21 -> [102407]`, the resident prologue
that fetches the user-array address.

**Monitor entry + direction fork (102367-102401)** - `102367 SAA 4` / `102370 MST PIE`
enter the monitor level; `102371 BSKP ONE SSK` forks on the direction. The GET side
(`102373-102375`) puts the `USPAR` constant in `D` and the user array in `T`; the SET
side (`102377-102401`, this call) puts the user array in `D` and `USPAR` in `T`.

**5-word copy + return (102402-102406)** - `102402 SAX 5` sets the count, `102403 LDA
,B 17` loads the page selector, `102404 JPL I 5 -> [102411]` calls `MOVUS` to copy the
5 words between the user segment and the system segment; `102405 STZ ,B 12` clears the
returned status (`A = 0`) and `102406 JMP I 4 -> [102412]` returns indirectly.

---

## Parameter / register contract

Manual-side names/types are from [`56B_SetUserParam.yaml`](../../../../../../../Developer/MON/calls/56B_SetUserParam.yaml).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| user array (5 words) | in | the 5 user parameters, 16-bit on ND-100 (`PASET(APAR)`) | inferred (manual) |
| entry point | in | `102363B` = set (this call); `102365B` = get (shared body, `SSK` split) | VERIFIED (bytes) |
| `SSK` | internal | set (0) / get (1) direction selector (`BSKP ONE SSK` at `102371`) | VERIFIED (bytes) |
| `B+20` (D0) | internal | user-array pointer used as one side of the `MOVUS` copy | VERIFIED (bytes); meaning inferred |
| `USPAR`=145445B | internal | system-side parameter array (the other `MOVUS` side) | VERIFIED (bytes) |
| `A` | out | status word cleared to 0 on success (`102405 STZ ,B 12`) | VERIFIED (bytes); meaning inferred |

---

## Pseudo-code (for an emulator)

See **[`56B-SetUserParam.pseudo.c`](56B-SetUserParam.pseudo.c)** - a pseudo-C model for
emulator authors. The control flow (the `SSK` set/get split, the monitor entry, the
direction of the 5-word `MOVUS` copy and the zeroed return status) is byte-verified;
the user-array layout is inferred from the manual and the monitor-call source shape.

Every instruction in the pseudo-code is translated against the canonical
[ND-100 instruction semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(`BSET ZRO/ONE SSK` clear/set skip flag, `JPL I` indirect call, `SAA` set A, `MST PIE`
mask-set, `BSKP ONE SSK` skip-if-set, `RADD CLD SA DD` copy idiom, `LDA`/`LDT ,B`
frame loads, `SAX` set X, `JMP I` indirect return).

---

## Honest caveats

**What is byte-proven:** `GOTAB[56B] = 000000` (level-14 fall-through; `prove-mon.py 56`
reads commoncode file byte `0xe592 = 00 00`); the `MPASE` worker body at `102363B` in
`025-S3IRPIT` is real code (first word `174020B = BSET ZRO SSK` matches the
disassembly); and it is a set/get user-parameter routine (monitor entry, `SSK`-selected
copy direction, `MOVUS` 5-word transfer keyed on the `USPAR` constant), consistent with
SetUserParam and its `MPAGE` GET twin.

**Which segment and why:** `MPASE=102363B` is a `SYMBOL-2-LIST` symbol, so it lives in
the second-level monitor overlay `025-S3IRPIT`, with its GET twin `MPAGE=102365B` two
words later. The window `102363B-102412B` is bounded strictly by the next symbol
`RERRP=102413B` (a different handler): `102363-102406` are code and `102407-102412` are
the `JPL I`/`JMP I` link-cell table (one cell is the `USPAR=145445B` constant).

**What is NOT proven:** the link from the zero GOTAB slot to the `MPASE` worker.
Because the vector is zero there is no stub to disassemble and no pointer to
dereference; dispatch drops into the resident `MFELL`/`CALLPROC` second-level path,
which lives in an **uncarved overlay** - hence **MISATTRIBUTED** in the strict sense:
the attribution rests on the `MPASE` symbol name + matching behaviour + the `MPAGE`
GET twin, not a followed pointer. The `JPL I`/`JMP I` link cells (`102407..102412`) are
a pointer table whose runtime targets are not resolved here. Confirming the dispatch
link needs a live trace of a real `MON 56`.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
