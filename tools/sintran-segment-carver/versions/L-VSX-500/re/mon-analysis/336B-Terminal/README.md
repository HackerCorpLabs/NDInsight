# MON 336B (octal) - Terminal (IOMTY)

The **I/O multifunction** monitor call: changes the attributes of terminal and
terminal-access-device (TAD) input/output, and configures NET/One interfaces and
SCSI disks. It needs a varying number of input/output parameters depending on the
function code, so all parameters are passed in an array (a FunctionCode, an
ArrayLength and a ParameterArray). Available to all users; background programs.

**Status:** GOTAB dispatch head byte-proven as **fall-through** (`GOTAB[336B] =
000000`, no per-call stub); the `IOMTY` worker body is real SINTRAN L bytes in the
resident `SINTRAN-DATA_commoncode` image (a `SYMBOL-1-LIST` symbol). The worker is
real executable code - an alternating sequence of resident-worker calls (`JPL I`)
and parameter-descriptor loads (`LDD ,B ...`), the dispatch entry into the
resident I/O multifunction machinery (it closes at `51762B`, bounded by the next
symbol `MBFDI=51763B`). The exact `MON 336 -> worker` link crosses an uncarved
kernel bridge (see [Honest caveats](#honest-caveats)). All addresses/values are
**octal**.

- **Full disassembly:** [`336B-Terminal.ASM`](336B-Terminal.ASM) - the actual code (the IOMTY worker body; there is no entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 336B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[336B] = 000000<br/>(byte-proven: fall-through)"]
    C -.uncarved MFELL / CALLPROC.-> E["IOMTY I/O-multifunction worker<br/>commoncode :51745B"]
    E --> F["resident-worker calls +<br/>parameter-array descriptor loads"]
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

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in
octal words x 2; for commoncode (load base `0`) the byte offset is the octal
address x 2 (decimal).

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[336] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071571B` (1 word) | 59122 | `GOTAB+336` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `MFELL`/`CALLPROC` | **UNVERIFIED** |
| IOMTY worker body | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `51745B-51762B` (14 words) | 42954 | `IOMTY` | real bytes = **CODE**; body link **MISATTRIBUTED** |

The window is bounded strictly to the next symbol `MBFDI=51763B` (14 words). All
14 words are code (seven `JPL I` resident-worker calls interleaved with seven
`LDD ,B` parameter-descriptor loads). The `JPL I` link cells (`052021/052023`) lie
past this window in the following region.

**Verify by hand:** `grep '^51745 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
-> byte offset `42954`; then
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=42954 count=2 2>/dev/null | od -An -tx1`
-> `ba 2c` (the stored word = octal `135054`, a genuine `JPL I 54` instruction,
confirming the region is code). The GOTAB slot itself:
`grep '^71571 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
-> `71571  000000  000 000  59122`; then
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=59122 count=2 2>/dev/null | od -An -tx1`
-> `00 00` (= `000000`, fall-through). `prove-mon.py 336` reads the same GOTAB zero.

---

## Instruction walkthrough

Full listing: [`336B-Terminal.ASM`](336B-Terminal.ASM). The body is the `IOMTY`
worker (there is no F16xx stub because `GOTAB[336] = 0`).

**Dispatch body (51745-51762)** - the worker is a tight, regular sequence: each
odd word calls a resident I/O worker through a link cell
(`51745 JPL I 54 -> [052021]`, `51747 JPL I 52 -> [052021]`,
`51751 JPL I 52 -> [052023]`, ...), and each even word loads a double-word
parameter descriptor from the caller's `B`-frame parameter array
(`51746 LDD ,B -115`, `51750 LDD ,B -125`, `51752 LDD ,B -133`, ...). Seven
resident-worker calls interleave with seven descriptor loads across the 14 words -
the classic shape of an I/O multifunction dispatcher that walks a parameter array
and hands each descriptor to a resident sub-worker. The concrete sub-function
chosen by the function code lives past the uncarved `CALLPROC`.

---

## Parameter / register contract

Manual-side names/types are from [`336B_TERMINAL.yaml`](../../../../../../../Developer/MON/calls/336B_TERMINAL.yaml).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `FunctionCode` | in | function code selecting the I/O sub-function (`MAC` `COPY SA DL`, function 0) | inferred (manual) |
| `ArrayLength` | in | length of the parameter array (>= parameters for the function) | inferred (manual) |
| `ParameterArray` | in/out | the function parameter array (the `LDD ,B` descriptors read it) | inferred (manual); descriptor loads VERIFIED |
| `T` (MAC example) | in | logical device number of the terminal (`LDT NTERM`) | inferred (manual) |
| `A` | in/out | translation flag on entry; error number on return (`MON 336` / `STA ERROR`) | inferred (manual) |

The worker's `LDD ,B` parameter-array reads and `JPL I` resident-worker calls are
VERIFIED from bytes, but the mapping onto the user-visible
FunctionCode/ArrayLength/ParameterArray contract lives in the caller-side `MON 336`
wrapper and the uncarved CALLPROC frame, so the contract is **inferred**, not
byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`336B-Terminal.pseudo.c`](336B-Terminal.pseudo.c)** - a pseudo-C model of
the handler for emulator authors. The control flow (the seven resident-worker
calls interleaved with the seven parameter-descriptor loads) is byte-verified; the
register/field semantics and which I/O sub-function the function code selects are
inferred from the manual and the code shape.

Every instruction in the pseudo-code is translated against the canonical
[ND-100 instruction semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(`LDD ,B` load-double with A in the low word, `JPL I` indirect call via a link
cell, addressing-mode effective addresses).

---

## Honest caveats

**What is byte-proven:** `GOTAB[336B] = 000000` (level-14 dispatch, a fall-through
with no per-call vector; `prove-mon.py 336` reads commoncode file byte
`0xe6f2 = 00 00`); the `IOMTY` worker body at `51745B` in `SINTRAN-DATA_commoncode`
is real code (first word `135054B = JPL I 54` matches the disassembly); and it is
an I/O multifunction dispatcher (seven resident-worker calls interleaved with
seven parameter-array descriptor loads), consistent with the Terminal I/O
multifunction call.

**Which segment and why:** `IOMTY=51745B` is a `SYMBOL-1-LIST` symbol, so it lives
in the resident `SINTRAN-DATA_commoncode` image (the always-resident kernel code),
appropriate for a terminal / TAD / SCSI I/O primitive. The window `51745B-51762B`
is bounded strictly by the next symbol `MBFDI=51763B` (14 words), all code.

**What is NOT proven:** the link from the zero GOTAB slot to the `IOMTY` worker.
Because the vector is zero there is no stub to disassemble and no pointer to
dereference; dispatch drops into the resident `MFELL`/`CALLPROC` second-level
path, which lives in an **uncarved overlay**. So the `MON 336 -> IOMTY`
attribution rests on the `IOMTY` symbol name (terminal I/O multifunction) + the
matching dispatcher behaviour, not a followed pointer - hence **MISATTRIBUTED** in
the strict sense. The worker's `JPL I` indirections target the link cells
`052021/052023`, which lie past the carved window and are not resolved here; this
window is the named dispatch entry into a larger resident I/O module, not a fully
self-contained subroutine. Confirming the dispatch link needs a live trace: issue
a real `MON 336`, single-step the level-14 fall-through into the resident
`CALLPROC`, and confirm P lands on `IOMTY = 51745`.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
