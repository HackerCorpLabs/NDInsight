# GOLDEN PATH — how every MON-call analysis folder must be built

This is the canonical template + handoff for the SINTRAN MON-call deliverable. **Every new
or revised call folder MUST follow this exactly.** Reference exemplar:
[`005B-ReadScratchFile/`](005B-ReadScratchFile/).

Rule zero: **bytes are ground truth, not the NPL source** (a different revision). Mark
anything not proven from bytes as `inferred` or `UNVERIFIED`. Never fabricate. Never write
the bare phrase "real L bytes" — write **"real SINTRAN L bytes"**. Never put disassembler
plumbing (byte-swap / little-endian / "byte-swapped copy") in a `.ASM` or a reader-facing doc.

---

## 1. The folder = exactly these files (nothing else)

```
NNNB-Name/
  README.md            one consolidated document (section list in §3)
  NNNB-Name.ASM        the REAL disassembly of the call's code region(s)
  NNNB-Name.pseudo.c   pseudo-C model for an emulator author, in ITS OWN FILE
  NNNB-Name.bin        ONLY for ND-500 calls: a generated single-region slice (labelled)
```
- **Never** ship `ANALYSIS.md` + `DISPATCH.md` as separate files — they were merged into the
  one README. Three overlapping files is the anti-pattern this replaced.
- **Fall-through / absent calls** (`GOTAB[N]=0` with no attributed body, or an empty ND-500
  slot — e.g. 042B, 425B, 426B, 427B) are **README-only**: no `.ASM`, no `.pseudo.c`. The
  README's Code-location table proves the absence.
- **ND-100 calls do NOT carry a per-call `.bin`** — those bytes are 100% duplicated in
  `../../segments-ref/`. Reference, don't copy. **ND-500 calls keep a generated `.bin`**
  (single contiguous region) with a header note that it is a generated slice.

## 2. The `.ASM`
The actual disassembly of the call's code region(s), from the canonical segment bytes.
- ND-100 call: usually **two labelled regions** — the `GOTAB[N]` entry stub (in
  `025-S3IRPIT` or the target segment) AND the worker body (in `006-S3FS` / `commoncode`).
- ND-500 level-12 call (500–515): the handler region in `026-S3IMPIT`.
- ND-500 monitor call (410–417): the handler in `030-S3SM5` via `nd500-dis`.
- ASCII header naming each region + its segment + load base; addresses/values octal; no
  byte-swap wording.

Disassembly recipe (ND-100 region `[lo..hi]` in segment `SEG`, octal load `L`, decimal `Ld`):
```
python3 -c "d=bytearray(open('../../../segments/SEG.bin','rb').read());d[0::2],d[1::2]=d[1::2],d[0::2];open('/tmp/x.le','wb').write(d)"
nd100-dis -a -o -b Ld /tmp/x.le | awk '$1>="lo" && $1<="hi"'
```
(commoncode bin is `../../../resident/SINTRAN-DATA_commoncode.bin`; the `-b` value is the
**decimal** of the octal load base — e.g. `26000B` → `11264`. ND-500 `030-S3SM5`: use
`nd500-dis` on the big-endian bin, no swap.)

## 3. `README.md` — the exact section order (see 005B)
1. `# MON NB (octal) — Name (symbol)` + one line: what it does.
2. **Status:** line (dispatch proven? worker link? per the verdicts).
3. Two bullets: link to **`NNNB-Name.ASM`** and to the canonical layer `../../segments-ref/`.
4. `## Dispatch path` — a WCAG mermaid flow (palette below); show the uncarved `CALLPROC`
   as a **dashed** hop for ND-100 fall-through / misattributed calls.
5. `## Code location (dispatch path)` — the table, **each segment cell a working markdown
   link** to `../../segments-ref/<seg>/<seg>.asm` and `.hex`, plus a "Verify by hand"
   `grep`+`dd` line that reproduces the entry bytes.
6. `## Instruction walkthrough` — reference `NNNB-Name.ASM`.
7. `## Parameter / register contract` — table, each row `VERIFIED` vs `inferred`.
8. `## Pseudo-code (for an emulator)` — a short paragraph that **links** `NNNB-Name.pseudo.c`
   (do NOT embed the C in the README). Omit for fall-through/absent calls.
9. `## Honest caveats` — what is byte-proven vs not; reconcile any contradiction into ONE story.
10. Footer linking method + maps (exact paths in §5).

## 4. Code-location table columns + verdict vocabulary
`| Role | Segment (links) | Addr range (octal) | Byte offset | Symbol | Verdict |`
- Byte offset = `(addr − loadbase)` in **octal** words × 2 (decimal). ND-500 `030-S3SM5`:
  32-bit units — use the `.hex` offset, not the ×2 rule.
- Verdicts: **VERIFIED** (byte-proven), **inferred** (structure implies it), **UNVERIFIED**
  (runtime/uncarved), **MISATTRIBUTED** (real code but not reachable from `GOTAB[N]`).

## 5. Link-path rules (source repo) — must resolve with `test -e`
- same-folder file: just the filename.
- `../../segments-ref/<seg>/<seg>.asm` and `.hex`
- canonical bin in `dd`: `../../../segments/<seg>.bin` or `../../../resident/SINTRAN-DATA_commoncode.bin`
- `../../TASK-05-mismatches.md`, `../../MON-CALL-INDEX.md`
- `../../../../../EXTRACTING-RESIDENT-CODE.md` (5 levels up in the repo)
- prove-mon: `scripts/prove-mon.py`
(The `/mnt/d/ND/t` delivery is a mirror where `EXTRACTING*` sits at `re/`, so the export step
rewrites the 5-level EXTRACTING link to `../../`. Everything else is layout-identical.)

## 6. Segment facts
| Segment | Load (oct) | Canonical bin | Symbols |
|---------|-----------|---------------|---------|
| SINTRAN-DATA_commoncode | 0 | resident/SINTRAN-DATA_commoncode.bin | SYMBOL-1 + SYMBOL-2 |
| 025-S3IRPIT | 32000B | segments/025-S3IRPIT.bin | SYMBOL-2-LIST |
| 006-S3FS | 26000B | segments/006-S3FS.bin | FILSYS-SYMBOLS |
| 026-S3IMPIT | 32000B | segments/026-S3IMPIT.bin | N500 + SYMBOL-2 |
| 030-S3SM5 | 40000B | segments/030-S3SM5.bin | N500-SYMBOLS |

## 7. Mermaid palette (WCAG 2.1 AA)
```
classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

## 8. The 3-layer dispatch reality (why ND-100 links stay UNVERIFIED)
`MON N → GOTAB[N]` (byte + live proven) `→ resident CALLPROC / segment-switch` (**uncarved**)
`→ worker body` (real bytes). The middle hop is in no carved segment, so the exact
`MON N → worker` link is not statically provable for ND-100 calls. ND-500 calls dispatch
through their own tables and ARE fully proven. Full: `../../../../../EXTRACTING-RESIDENT-CODE.md` §9,
`../../TASK-05-mismatches.md` §G.

## 8b. Pseudo-C MUST be grounded in real opcode semantics (no assumptions)
The `<call>.pseudo.c` must reflect the ACTUAL ND-100/ND-500 instruction behaviour, not a
guess from the mnemonic. Ground every translation in the instruction-semantics reference:
`../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md` (ND-100, synthesised from the
nd100x emulator + Ghidra SLEIGH: agreement = VERIFIED, emulator wins on conflict) and
`../instruction-semantics/ND500-INSTRUCTION-SEMANTICS.md` (ND-500, from nd500x).

Verified gotchas that guessing gets WRONG (do not repeat the old mistakes):
- **ROP `RADD CLD SD DA` = `A = D`** (CLD zeroes the dest operand, source=reg[D]). `COPY`=`RADD CLD`;
  `RSUB`=`RADD AD1 CM1` (dest−source). Register letters: STS0 D1 P2 B3 L4 A5 T6 X7.
- **T/X transfers are 24-bit PHYSICAL, bypassing the MMU.** `EL = ((T & 0xFF) << 16) | ((X + disp) & 0xFFFF)`,
  `disp=(operand>>3)&7`. `STATX`→`phys[EL]=A`; `LDATX`→`A=phys[EL]`; `LDDTX`→`A=phys[EL];D=phys[EL+1]`;
  `STDTX`→`phys[EL]=A;phys[EL+1]=D`; `STZTX`→`phys[EL]=0`; `LDBTX`→`B=0177000|(2*phys[EL])`. T is the
  bank/high byte — NOT a T-relative or plain X-indexed load. (This is how level-12 handlers reach the
  ND-500 message buffer: T=`5MBBANK`.)
- **SWAP** truly exchanges dst↔src; **LIN** right-shift fills from the M/link bit; **BSET BAC** K-source
  — per the emulator (SLEIGH under-models these). **STBTX** is illegal (no opcode) — never emit it.
- **ND-500**: flag `C=1` means **no-borrow** (inverted). For mid-block/misaligned carves (e.g. 416B/417B,
  partly 412B) the RAW BYTES are ground truth but decoded mnemonics at wrong alignment are unreliable —
  say so in the README and do not model unreliable lines as if verified.

Where a line's opcode is UNRESOLVED in the reference, model it as `/* UNVERIFIED: <mnemonic> */`,
never as confident behaviour. An all-zero worker region proves nothing (not code, not data) — model the
documented behaviour only and flag the bytes as not-recoverable (see the 322B / 122B folders).

## 9. Validate before done
- `python3 scripts/validate-mon-carves.py` — per-call `.ASM`
  are multi-region excerpts (they are exempt from the whole-segment closure check; the
  authoritative closure is on `segments-ref/`).
- **Pseudo-C grounding:** every non-trivial line in `<call>.pseudo.c` must trace to a VERIFIED (or
  emulator-authoritative) entry in the instruction-semantics reference; anything else marked UNVERIFIED.
- `python3 scripts/mon-status-report.py` — golden-path status:
  a call is **complete** = README + `.ASM` + `.pseudo.c` (or a documented README-only stub).
- `test -e` every markdown link in the README.
