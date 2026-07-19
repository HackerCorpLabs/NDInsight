# MON 327B (octal) - FileSystemFunction (GTYPR)

> **CORRECTED 2026-07-15 (byte-verified).** The worker + dispatch described below are on the
> DEBUNKED model and are WRONG. Byte truth from the carved L07 image:
> `MCTAB[327B] = 006147B = MFFSC=111563B` in segment 006-S3FS, reached by the real dispatch
> `MON 327B -> ENT14(072167B) -> GOTAB[327B]=MFELL(072114B) -> CALLP(032201B) -> MCTAB[327B]=MFFSC`.
> Any "GOTAB from commoncode" / "uncarved CALLPROC bridge" / "F16xx stub" / old worker name below
> is an artefact of the wrong table. Verified: `dd if=044-S3IDPIT.bin bs=1 skip=2254 count=2`
> -> `93 73`. Cross-ref ../317B-ExecuteCommand/README.md and SINTRAN/CARVING-HANDOFF.md sec 3a.

File-system information/maintenance "monster" call: a function code selects a
sub-operation that classifies a file/object descriptor against type and size
tables and returns packed status/flag/value words. The carved worker body is
`GTYPR` ("Get TYP-RING") in segment `006-S3FS`.

**Status:** dispatch word byte-proven (`GOTAB[327B] = 112503B`, non-zero, not a
fall-through); the `112503B -> worker` hop is UNVERIFIED (banked-overlay
ambiguity); the carved `GTYPR` body is real SINTRAN L bytes but is
MISATTRIBUTED - not statically reachable from `112503B`. See
[Honest caveats](#honest-caveats). All addresses/values are **octal**.

- **Full disassembly:** [`327B-FileSystemFunction.ASM`](327B-FileSystemFunction.ASM) - the actual code, both regions (dispatch target + GTYPR worker).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 327B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[327B] = 112503B<br/>(byte-proven, non-zero)"]
    C --> D["dispatch target<br/>025-S3IRPIT :112503B<br/>(overlay UNVERIFIED)"]
    D -.uncarved CALLPROC.-> E["GTYPR worker body<br/>006-S3FS :113312B"]
    E --> F["type/size classify<br/>-> B-frame result words"]
    class A blue
    class B,C,D teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The dashed hop (`D ⇢ E`) is the resident `CALLPROC`/segment switch - it is **not
present in any carved segment**, so it is the one link that cannot be followed
statically. `112503B` sits in an alternately-banked overlay window (both
`025-S3IRPIT` and `006-S3FS` cover it with different bytes), so which overlay is
mapped at MON-327 time is a runtime paging decision, not in the static bytes.

---

## Code location (dispatch path)

Byte offset = `(addr − loadbase)` in octal words × 2 (decimal). Verdicts follow
the byte-level DISPATCH analysis, which wins over any older prose.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[327] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071562B` (1 word) | 59108 | `GOTAB+327` = `112503B` | **VERIFIED** |
| Level-14 dispatch target | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `112503B–112532B` | 49798 | `DIA9` (SYMBOL-2-LIST) | **UNVERIFIED** (banked overlay) |
| resident CALLPROC bridge | — (uncarved) | — | — | `CALLPROC` | **UNVERIFIED** |
| GTYPR worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) · [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `113312B–114035B` | 54676 | `GTYPR` (FILSYS-SYMBOLS) | real bytes; link **MISATTRIBUTED** |

`071562B = 071233B (GOTAB base) + 327B`.

**Verify by hand (dispatch word):** `grep '^59108' ` on the raw byte, or read it
straight from commoncode:
```
dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=59108 count=2 2>/dev/null | od -An -tx1
# -> 95 43  (big-endian word 0x9543 = 112503 octal = GOTAB[327])
```
**Verify by hand (dispatch target `112503B` = `DIA9`):**
```
grep -n 'DIA9' ../../segments-ref/025-S3IRPIT/025-S3IRPIT.symbols.txt
dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=49798 count=8 2>/dev/null | od -An -tx1
# -> 48 11 d1 01 d0 43 5a 0d  (big-endian words 044021 150401 150103 055015 = DIA9)
```
**Verify by hand (GTYPR worker `113312B`):**
```
grep -n 'GTYPR' ../../segments-ref/006-S3FS/006-S3FS.symbols.txt
dd if=../../../segments/006-S3FS.bin bs=1 skip=54676 count=8 2>/dev/null | od -An -tx1
# -> 22 7f cc 65 cc 59 f0 37  (big-endian word 021177 = STD I 177, the GTYPR entry)
```

---

## Instruction walkthrough

Full listing: [`327B-FileSystemFunction.ASM`](327B-FileSystemFunction.ASM). The
functional body is the `GTYPR` worker (region B); region A is the byte-verified
dispatch target `112503B` in `025-S3IRPIT` (an overlay-ambiguous window - see
caveats). Pointer WORDS embedded in region B (`113511-113524`, `113705-113716`,
`114020-114034`) are data that the disassembler linearly decoded as
pseudo-instructions, not executable code.

**Entry prologue (113312-113316)** - `STD I 177` saves caller D through a pointer
word; `SAB 67` establishes the local `B`-frame; `JPL I 174` (ptr `@113512 =
003752B`) calls a resident file-system worker.

**Function-code range check (113317-113331)** - `LDT 172` loads a limit; A is
compared (`SKP IF DA GRE ST`), adjusted (`AAT 100`), and normalised (`SUB 165`)
into `B+63`; `JPL I 164` (ptr `@113515 = 010376B`) calls a resident worker.

**Descriptor scan + type/size classification (113347-113525)** - reads a
multi-word descriptor via `X` (`LDA ,X 0/1/3/6/7/11/22/23/44/45`), tests flags
with `BSKP ZRO/ONE n DA`, and walks a stride-2 table (`LDX ,B 60 / AAX 2 / MIN
,B 57 / JMP 113360`, counter in `B+57`). It builds a value into `B+61`, a
function selector (1/2/3) into `B+62`, and a result into `B+63`. Error/short
paths jump indirect to the error-return entry `114016B`.

**Type-value membership test (113576-113640)** - a straight-line `SAT n / SKP IF
DA UEQ ST` chain over the whitelist `{3,20,21,25,32,33,34,40,41}` (octal) against
`B+61`. A match sets `SAX 1` and jumps to result assembly (`113765`); no match
falls to the size ladder.

**Size/threshold classification (113717-113764)** - a descending ladder
(`LDT 100/76/74/72/70/66/64/62/60/56/54/52; SKP IF DA GRE ST`) maps `B+61` (a
size/count) into a ring/category bucket - consistent with the `GTYPR = Get
TYP-RING` name.

**Result assembly and exit (113765-114017, 114035)** - sets a status bit
(`LDA ,B 65 / BSET ONE 10 DA / STA ,B 65`), then publishes `B+64 -> B+3` (flags),
`B+65 -> B+0` (status), `B+63 -> B+2` (value). The **normal return** executes
`MIN ,B 4` at `114013` (bumping the caller's skip/return-index field) and leaves
via `JMP I 20` (ptr `@114035 = 003776B`). The **error return** is entered at
`114016` (`STA ,B 2`; `JMP -3 -> 114014`), joining the exit but SKIPPING the
`MIN ,B 4` - so an error does NOT bump the skip field.

**Resident pointer targets (all octal, outside the carved window, unresolved to
names):** `003752, 010376, 020274, 055566, 056307, 071413, 003776`.

---

## Parameter / register contract

| Item | Dir | Verdict | Notes |
|------|-----|---------|-------|
| `A` (function code) | in | VERIFIED (bytes) | Range-checked at entry (`LDT 172`, `AAT 100`, `SUB 165`) and used as an index/selector. |
| `X` | in | VERIFIED (bytes) | Points to a multi-word file/object descriptor; routine reads `,X 0/1/3/6/7/11/22/23/44/45`. |
| `B+0` | out | VERIFIED (bytes) | Status word (`B+65` copied here at `114010`). |
| `B+2` | out | VERIFIED (bytes) | Value word (`B+63` at `114012`; error path stores A here at `114016`). |
| `B+3` | out | VERIFIED (bytes) | Flags word (`B+64` copied here at `114006`). |
| `B+4` | out | VERIFIED (bytes) | Skip/return-index field bumped (`MIN ,B 4`) on the normal path only. |
| normal vs error return | out | VERIFIED (bytes) | Normal = `MIN ,B 4` executed; error = entered at `114016`, `MIN ,B 4` skipped. |
| `T` = per-function selector | in | inferred | NPL claims T selects func 1/2/3/4; here the selector lands in A, not obviously T. |
| `A` = open-file/device no. | in | inferred | NPL/README claim; A is used but its caller meaning is not provable from these bytes. |
| `D` = TYPRING bits out | out | inferred | NPL claim; results here land in the `B`-frame slots, not visibly in D. |

The caller-visible register convention lives in the caller-side `MON 327` wrapper
and the uncarved `CALLPROC` frame, so the precise T/A/X assignment is inferred,
not byte-proven in this window.

---

## Pseudo-code (for an emulator)

See **[`327B-FileSystemFunction.pseudo.c`](327B-FileSystemFunction.pseudo.c)** - a
pseudo-C model of the `GTYPR` worker for emulator authors. Control flow, the
type/size classification, and the normal-vs-error return split are byte-verified;
the descriptor-field and caller-register semantics are inferred from the call
structure. Every instruction is translated per the canonical
**[ND-100 Instruction Semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)**.

---

## Honest caveats

**What is byte-proven:** `GOTAB[327B] = 112503B` (level-14 dispatch word, read
straight from `commoncode.bin`, non-zero - so 327 is NOT a fall-through). The
`GTYPR` worker bytes at `113312B..114035B` are real SINTRAN L bytes and are
control-flow closed: every direct branch lands inside the window, and the routine
is a type/size classifier that reads an `X`-descriptor and returns packed
status/flag/value words in a `B`-frame with a normal-vs-error return split.

**What is NOT proven (single clear story):** the link from the dispatch target
`112503B` to the `GTYPR` worker `113312B`.
- `112503B ≠ 113312B`. The dispatch word points at `112503B`, not at `GTYPR`.
- `112503B` lies in a banked-overlay window covered by BOTH `025-S3IRPIT` (where
  the symbol table calls it `DIA9`, a device-driver-style name - almost certainly
  an **overlay-mismatch name**, since a file-system call would run under the
  file-system PIT, not the device PIT) and `006-S3FS` (different bytes). Which
  overlay is mapped at MON-327 runtime is a paging-context decision, not in the
  static bytes - so the name `DIA9` is not trustworthy and the hop is UNVERIFIED.
- A scan of `006-S3FS` for any pointer word equal to `113312B` in `112000..113312`
  returns ZERO hits; the `025-S3IRPIT` code at `112503B` makes one direct
  in-segment jump (`JMP -44 -> 112447`) and never references `113312B`. So `GTYPR`
  is NOT statically reachable from `112503B` - hence **MISATTRIBUTED**.
- The symbols `FSMTY` and `MFELL` used in earlier folklore do **not** exist in any
  L07 symbol table; do not treat them as L facts. (Note also a symbol-table
  conflict: `FILSYS-SYMBOLS` says `GTYPR = 113312`, while `N500-SYMBOLS` says
  `GTYPR = 107550` - even the worker's own address is revision-dependent.)

Confirming the real path needs a live trace: break at the ENT14 dispatch for MON
327, confirm P lands at `112503B`, read the PIT/paging registers to identify the
mapped overlay, then single-step the first `JMP I`/`JPL I` that leaves the overlay
and confirm whether control reaches `GTYPR`.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
