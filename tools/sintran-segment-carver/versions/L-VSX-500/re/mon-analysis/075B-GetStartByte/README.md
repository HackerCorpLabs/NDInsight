# MON 075B (octal) — GetStartByte (REABT)

Returns, as an INTEGER4, the number of the next byte that would be accessed in an opened
sequential mass-storage file ("read byte pointer"). The worker body reads the current byte
position out of the open-file descriptor and stages it as two 16-bit words.

**Status:** dispatch head byte-proven (`GOTAB[075B] = 121471B`, non-zero → dispatched, not a
fall-through); worker body is real SINTRAN L bytes; the exact `MON 075 → REABT worker` link crosses
an uncarved kernel bridge (see [Honest caveats](#honest-caveats)). All addresses/values are
**octal**.

- **Full disassembly:** [`075B-GetStartByte.ASM`](075B-GetStartByte.ASM) — the actual code, both regions (F1645 entry stub + REABT worker).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 75B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[75B] = 121471B<br/>(byte-proven)"]
    C --> D["F1645 stub -> shared RPIT<br/>025-S3IRPIT :121471B / :121434B"]
    D -.uncarved CALLPROC.-> E["REABT byte-ptr worker<br/>006-S3FS :104005B"]
    E --> F["read byte position<br/>via JPL I workers"]
    class A blue
    class B,C,D teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The dashed hop (`D ⇢ E`) is the resident `CALLPROC` proc-table dispatch (selector `45` selected by
the F1645 stub) — it is **not present in any carved segment**, so it is the one link that cannot be
followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr − loadbase)` in octal words × 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[75] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071330B` (1 word) | 58800 | `GOTAB+75` = `121471B` | **VERIFIED** |
| F1645 entry stub (2 words → shared dispatch) | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `121471B–121472B` | 56946 | `F1645` | **VERIFIED** |
| shared RPIT-image dispatch (selector 45) | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `121434B–121470B` | 56888 | (RPIT image, no named entry) | bytes **VERIFIED**; semantic inferred |
| resident CALLPROC bridge | — (uncarved) | — | — | `CALLPROC` | **UNVERIFIED** |
| REABT byte-pointer worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) · [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `104005B–104203B` | 47114 | `REABT` | real bytes; link **MISATTRIBUTED** |

**Verify by hand:** `grep '^71330 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
→ word `121471`, byte offset `58800`; then read the F1645 entry and the REABT entry word:
```
dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=56946 count=2 | od -An -tx1   # 48 d7  = octal 044327  (F1645 LDA -51)
dd if=../../../segments/006-S3FS.bin   bs=1 skip=47114 count=2 | od -An -tx1   # 22 0e  = octal 021016  (REABT STD I 16)
```

---

## Instruction walkthrough

Full listing: [`075B-GetStartByte.ASM`](075B-GetStartByte.ASM). Region A is the level-14 entry
(F1645 stub + shared RPIT dispatch); the functional body is the REABT worker (region B).

**F1645 stub (121471–121472)** — the byte-proven GOTAB target. `121471 LDA -51` loads the word at
`121420B`, which on disk is `000000`, so `A := 0`; `121472 JMP -36` is a **direct** jump back to the
shared dispatch at `121434B`.

**Shared RPIT dispatch (121434–121470)** — `121434 JAF 3` sees `A = 0` (false) and branches to
`121437 SAA 45`, selecting selector **45** (byte-derived; "45 == the REABT proc number" is INFERRED).
The fan-out then goes through indirect pointer words (`JPL I` / `JMP I` via `@121502=116374`,
`@121507=117107`, `@121504=116036`, data `@121501=061160`) — every resolvable target stays **inside
segment 025's own span**, so the static chain dead-ends here; the cross to `006-S3FS` is the runtime
`CALLPROC` hop.

**REABT prologue (104005–104013)** — `104005 STD I 16` stashes the caller double; `104011 JPL I 13`
and `104013 JPL I 21` call resident FS workers (pointer words `@104024 = 003752`, `@104034 = 072014`)
to locate the open-file descriptor.

**Validity flag + fork (104044–104054)** — `104044 JPL I 53` (`@104117 = 003752`, worker reused),
then `104045 BSKP ONE SSK` computes a boolean into local `B+127` (`SAA 1`/`STA` vs `STZ`).
`104053 LDA ,B 127` / `104054 JAF 63` forks: flag `0` → byte-pointer path at `104137`; flag non-zero
→ error/return path via the indirect returns at `104132/104133`.

**Byte-pointer read-out (104137–104201)** — `104141 JPL I 73` (`@104234`) looks up the record; two
masked tests (`104151 BSKP ONE 160 DA`, `104154 BSKP ONE 100 DA`) select between two sub-paths that
each load fields off an indexed open-file record (`LDX I 56`, `LDA ,X 3/7`, `STA ,X 24/25`) — the
actual read-out of the current byte position. `104200 STT ,B 123` / `104201 STD ,B 125` stage the
INTEGER4 result as two 16-bit words. Precise field offsets are compiled detail; **INFERRED**.

**Common exit (104202)** — `104202 JPL I 42` (`@104244`) returns into the shared monitor-return path
(the skip-return to the caller is taken by that shared code, not by an instruction in this window).

**Pointer/data islands (104023–104037, 104116–104131)** — these words are **address constants**, not
executable instructions; the disassembler renders them as `ROP NOOP / AND ,X / STZ I` noise. Do not
read them as logic. They hold the targets of the indirect calls above (`003752`, `003776`, `072014`,
`031075`, `104362`, `104351`).

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `D` (double, `104005 STD I 16`) | in | caller parameter (file number / descriptor pointer) | inferred |
| local `B+127` | internal | validity/open flag, computed `104045–104052` (`SSK`-gated) | VERIFIED (bytes) |
| local `B+123` / `B+125` | out | INTEGER4 result staged as two 16-bit words (hi / lo) | VERIFIED staging (bytes); "= byte number" inferred |
| flag fork `104054` | internal | `0` → byte-pointer path `104137`; ≠0 → error/return `104132` | VERIFIED (bytes) |
| skip / error return | out | shared monitor-return via `104202 JPL I 42`; error via `104132/104133` | inferred (handled past window) |

Manual-level contract (from the manual, NOT re-derived from these bytes): input is the file number
(INTEGER) of an opened sequential mass-storage file; output is the next byte number (INTEGER4). The
code confirms an INTEGER4-sized result and two outcomes (success vs error), consistent with the
manual, but does not let me name the exact input register or the error code from the window alone.

---

## Pseudo-code (for an emulator)

See **[`075B-GetStartByte.pseudo.c`](075B-GetStartByte.pseudo.c)** — a pseudo-C model of the handler
for emulator authors. Control flow (the `B+127` flag fork and the two result-staging paths) is
byte-verified; the file-system worker semantics (which field is the byte position) are inferred from
the call structure. Every instruction is translated per the canonical
**[ND-100 Instruction Semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)**.

---

## Honest caveats

**What is byte-proven:** `GOTAB[075B] = 121471B` (level-14 dispatch word on disk); the `F1645` stub
at `121471B` is real code — it loads `A = 0` and direct-jumps to the shared RPIT dispatch at
`121434B`, which selects selector `45` (`SAA 45`); the `REABT` worker entry bytes at `104005B` match
the disassembly and the `FILSYS-SYMBOLS` address `REABT = 104005`.

**What is NOT proven:** the link from the `F1645` stub (in `025-S3IRPIT`) to the `REABT` worker (in
`006-S3FS`). Every pointer the stub and shared dispatch dereference (`061160`, `116374`, `117107`,
`116036`) stays **inside segment 025's span (32000..200000)** and lands on more RPIT-image dispatch —
none crosses into the `104xxx` file-system range. The stub→worker transfer is the resident
`CALLPROC` proc-table indexed by selector `45`, which is **runtime-populated and uncarved**. So the
`MON 075 → REABT` attribution rests on the symbol name ("read byte pointer" = GetStartByte) + the
manual, not a followed pointer — hence **MISATTRIBUTED** in the strict sense.

This reconciles the two older notes: the folder's earlier README spoke of an `MFELL → CALLPROC`
narrative, but the byte-true GOTAB target symbol is `F1645` (a compiler-generated local label), not
`MFELL`; the dispatch is real and non-zero, and `F1645` is the ground-truth intermediate. Confirming
the REABT attribution needs a live trace: break at `121471B` on a real `MON 075`, single-step through
the `CALLPROC` dispatch with selector `45` loaded, and confirm P lands on `REABT = 104005`.

Whether file 0 (the command buffer) is special-cased is **UNCERTAIN** — no static file-0 test is
visible in this window, consistent with the manual restricting the call to sequential mass-storage
files.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
