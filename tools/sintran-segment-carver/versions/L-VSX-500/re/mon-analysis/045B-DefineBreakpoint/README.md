# MON 045B (octal) - DefineBreakpoint (F1631)

> **CORRECTED 2026-07-15 (byte-verified).** The worker + dispatch described below are on the
> DEBUNKED model and are WRONG. Byte truth from the carved L07 image:
> `MCTAB[45B] = 005665B = BDBRK=002235B`, reached by the real dispatch
> `MON 45B -> ENT14(072167B) -> GOTAB[45B]=MFELL(072114B) -> CALLP(032201B) -> MCTAB[45B]=BDBRK`.
> Verified: `dd if=044-S3IDPIT.bin bs=1 skip=1898 count=2` -> `04 9d`. NOTE: BDBRK=002235B is
> below the 003-S3CP load base (030000B), so the worker BODY segment is OPEN (not 003-S3CP);
> only the MCTAB slot is byte-proven here. Cross-ref ../317B-ExecuteCommand/README.md and
> SINTRAN/CARVING-HANDOFF.md sec 3a.

Defines / re-arms a symbolic-debugger breakpoint. The byte-verified `GOTAB[45]`
target is a 21-word entry stub (`F1631` at `121075B`) that forks on the caller
arg, edits a breakpoint-table slot, and hands off to a message/queue-send worker
family.

**Status:** level-14 dispatch is byte-proven (`GOTAB[45] = 121075B = F1631`,
re-checkable via `prove-mon.py 045`); the stub body is real SINTRAN L bytes;
worker *behaviour* is UNVERIFIED and the `stub -> worker` deeper hops cross an
uncarved/low-resident boundary (see [Honest caveats](#honest-caveats)). All
addresses/values are **octal**.

- **Full disassembly:** [`045B-DefineBreakpoint.ASM`](045B-DefineBreakpoint.ASM) - the actual code, both regions (F1631 entry stub + general-path worker).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 45B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[45B] = 121075B<br/>(byte + prove-mon proven)"]
    C --> D["F1631 entry stub<br/>025-S3IRPIT :121075B"]
    D -.uncarved / low-resident.-> E["worker family<br/>SNDRE 121640B / PUTPO 115720B"]
    E --> F["slot edit + queue send<br/>via JPL I workers"]
    class A blue
    class B,C,D teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

Unlike a two-word MFELL fall-through, `GOTAB[45]` is a **direct** dispatch word
(`121075B`, not `000000`). The dashed hop is the one link that cannot be followed
statically: worker `121211B -> 010341B` lands in **low resident memory below the
carved overlay** (load `32000B`), runtime-populated.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in
octal words x 2 (decimal).

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[45] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071300B` (1 word) | 58752 | `GOTAB+45` = `121075B` | **VERIFIED** |
| F1631 entry stub | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) - [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `121075B-121121B` (21 words) | 56442 | `F1631` | **VERIFIED** |
| general-path worker (A!=3, `JPL I 65`->`121206B`) | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) - [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `115720B-115737B` | 53152 | `PUTPO` | real bytes; behaviour **UNVERIFIED** |
| A==3 worker (`JPL I 101`->`121202B`) | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) - [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `121640B` | 57152 | `SNDRE` | target VERIFIED; behaviour **UNVERIFIED** |
| family ptr `121211B` -> low resident | - (below load `32000B`, uncarved) | `010341B` | - | (none) | **UNVERIFIED** |

**Verify by hand:** `grep '^71300 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
-> byte offset `58752`; then
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58752 count=2 | od -An -tx1`
-> `a2 3d` (= big-endian `0xa23d` = octal `121075` = the F1631 entry). Then the
stub body: `grep '^121075 ' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex` ->
offset `56442`;
`dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=56442 count=4 | od -An -tx1`
-> `b1 40 f2 03` (= octal `130500 171003` = `JAN 100 / SAT 3`).

---

## Instruction walkthrough

Full listing: [`045B-DefineBreakpoint.ASM`](045B-DefineBreakpoint.ASM). Region A
is the byte-verified `GOTAB[45]` target (`F1631`); region B is the general-path
worker (`PUTPO`).

**F1631 arg fork (121075-121104).** The stub reads the caller arg in `A`.
`121075 JAN 100` takes an alternate exit (`-> 121175`) when `A` is negative.
`121076 SAT 3` / `121077 SKP IF DA EQL ST` compare `A` to `3`: when `A == 3` it
calls the worker at `121640B` (`SNDRE`) via `121101 JPL I 101` (pointer word
`@121202 = 121640`); otherwise it falls into the general slot path at `121105B`.

**Breakpoint-slot handling (121105-121120).** `121107 LDX I 76` loads a table
base (pointer word `@121205 = 061157`); `121110 LDA ,X 14` reads slot field `14`.
`121111 JAZ` skips to the next stub (`F1632 = 121122B`) when the slot is empty.
Otherwise `121112-121115` clear four slot fields (`STZ ,X 14/46/4/2`) - consistent
with releasing / re-arming a breakpoint record before it is (re)written - and
`121117 LDD ,X 15` loads the slot descriptor.

**Worker hand-off (121121).** `121121 JPL I 65` calls the general-path worker at
`115720B` (`PUTPO`, pointer word `@121206 = 115720`).

**Worker body PUTPO (115720-115737).** `115720 IOF` opens an I/O-off critical
section; `115721 JXZ` skips the edit when `X == 0`; `115726 STDTX` / `115731 STATX`
store a double and a single word into an `X`-indexed table; `115736 ION` /
`115737 EXIT` close the section. Structurally a queue/table update - the exact
DefineBreakpoint meaning of each field is UNVERIFIED.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `A` (ZAREG) | in | selector: `<0` -> alt exit; `==3` -> `SNDRE` worker; else slot path | VERIFIED (branch structure) |
| `X` base | in | breakpoint-table base (from ptr `@121205 = 061157`) | VERIFIED (bytes); encoding inferred |
| slot fields `2,4,14,46` | out | cleared (`STZ ,X`) during slot handling | VERIFIED (from code) |
| slot field `15` (double) | in | slot descriptor loaded (`LDD ,X 15`) before worker call | VERIFIED (from code) |
| workers `121640B`/`115720B` | - | shared subroutines invoked via `JPL I` | VERIFIED targets; behaviour UNVERIFIED |
| `010341B` (via `121211B`) | - | low-resident, runtime-populated hop | UNVERIFIED |
| skip / error return | out | not resolvable from the 21-word stub alone | UNVERIFIED |

The user-visible register convention lives in the caller-side `MON 45` wrapper and
the deeper (uncarved / low-resident) worker frames, so the precise A/X/T meaning
is **inferred**, not byte-proven here. Error codes cited by earlier drafts
(`174/274/371/372/400`) are NOT derivable from these bytes -> UNVERIFIED.

---

## Pseudo-code (for an emulator)

See **[`045B-DefineBreakpoint.pseudo.c`](045B-DefineBreakpoint.pseudo.c)** - a
pseudo-C model of the handler for emulator authors. The F1631 stub control flow
(the `A==3` fork, the four-field slot clear, the two `JPL I` worker hand-offs) is
byte-verified; the worker semantics (breakpoint slot vs queue send) are inferred
from the call structure.

Every instruction in the `.pseudo.c` is translated against the canonical
[`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(bare `LDx`/`LDT`/`AND disp` = P-relative `mem[P+disp]`, not literals; `SKP`/`BSKP` skip
polarity; `RADD CLD SD DA` = `A = D`; T/X transfers = physical `EL`).

---

## Honest caveats

**What is byte-proven:** `GOTAB[45] = 121075B = F1631` (level-14 direct dispatch,
matches a re-runnable `prove-mon.py 045`); the `F1631` stub at `121075B` is real,
non-zero code (`130500 171003 140065 ...`); its worker pointer words
(`@121202 = 121640`, `@121206 = 115720`) are real bytes and land inside the carved
`025-S3IRPIT` overlay.

**What is NOT proven:** the *behaviour* of MON 45 end to end. The F1631 workers
resolve to a message/queue-send family (`SNDRE / PUTPO / SNDWT / SNDBU / CREME /
REDOM` per SYMBOL-2-LIST) whose exact role in DefineBreakpoint is not derivable
from these bytes; and `121211B -> 010341B` leaves the carved image entirely into
low resident memory (below load `32000B`), runtime-populated. Symbol NAMES come
from SYMBOL-2-LIST (SYMBOL-1-LIST, which meta names for segment 025, has NO entry
for these addresses - a revision mismatch), so treat octal addresses as truth and
names as hints.

**Reconciling the old carve (one clear story):** earlier drafts of this folder
carved the 16-way symbolic-`DEBUGGER` dispatcher (`105075B`) and the `BRPNT`
breakpoint-trap head (`104554B`). Those are genuine SINTRAN L bytes but are
**MISATTRIBUTED** to MON 45: a full scan of `GOTAB[0..377]` finds NO entry equal
to `105075B` or `104554B`, the value `105075`/`104554` appears **zero** times as a
pointer word anywhere in `025-S3IRPIT`, and the F16xx stub family (`~121xxx`) is
`~14000` words away - far outside the 8-bit P-relative reach of any stub. `BRPNT`
is entered by a breakpoint *trap* (a planted breakpoint firing), and `DEBUGGER` by
a resident second-level / `CALLPROC` dispatch not visible in GOTAB - **not** by
MON 45. The byte-truth handler for MON 45 is `F1631`, carved here. Confirming the
full path needs a live trace (break at `121075B` on a real `MON 45`, single-step
through the worker and capture the runtime `010341B` dereference).

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
