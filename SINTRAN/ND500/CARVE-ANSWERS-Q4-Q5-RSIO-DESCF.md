# Carved-L07 Answers: Q4 (MON 143B RSIO) and Q5 (MON 71B DESCF)

Source of truth for this document is the CARVED L07 binary disassembly (not NPL source).
All paths below are repo-root-relative to the repository root
`E:\Dev\Ronny\NDInsight` (Linux mount `/mnt/e/Dev/Ronny/NDInsight`).
All numbers are octal, written `nnnB`. Two's complement B-relative displacements
are shown both as the assembler wrote them (e.g. `,B -103`) and as their unsigned
16-bit word offset (e.g. `177675B`).

Carve base files used:
- `tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/003-S3CP/003-S3CP.asm`
  (+ `003-S3CP.symbols.txt`) - worker segment, load base `30000B`.
- `tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/044-S3IDPIT/044-S3IDPIT.asm`
  - MCTAB dispatch table, load base `4000B`.
- `tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/011-S3ERRL/011-S3ERRL.symbols.txt`
  - ESCON/ESCOF symbol definitions.
- Prior NPL-only answer being upgraded:
  `SINTRAN/ND500/NPL-ANSWERS-DEVICE0-CMDBUF-RSIO-DESCF.md`.

================================================================================
## Q4 - MON 143B RSIO field layout (BYTE-PROVEN from the carved worker body)
================================================================================

**Short answer: the prior NPL offset mapping is CONFIRMED and now upgraded from
"symbol-equation + analogous GDEVTY code" to "read directly out of the RSIO worker
body." The RSIO body itself is carved.** RSIO returns three byte-traced values -
execution mode, command-input device, command-output device - and does NOT touch any
directory/user-index field.

### Dispatch slot (VERIFIED)
File: `tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/044-S3IDPIT/044-S3IDPIT.asm`
```
005763  051430  	LDT I ,B 30		; MCTAB[143B] = 051430B = RSIO   (line 1433)
```
`MCTAB` base = `005620B`; `005620B + 143B = 005763B`; stored word = `051430B` = `RSIO`.
Verified independently for a sibling: `MCTAB[71B] @005711B` = `047020B` (below), same
base+index arithmetic - so the indexing is proven, not assumed.

### RSIO worker body (VERIFIED, byte-for-byte)
File: `tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/003-S3CP/003-S3CP.asm`
(worker symbol `RSIO=051430B`, `003-S3CP.symbols.txt` line 848). Entry bytes verified
in the carve; the listing here is the actual disassembly lines 9845-9862:
```
051430  146131  	RADD CLD SB DD		; D := B   (save param/frame base)
051431  044021  	LDA 21
051432  146153  	RADD CLD SA DB		; B := A   (switch to per-process work base)
051433  044675  	LDA ,B -103		; A := [B -103] = XBCHF (BCHFLAG, mode flag)
051434  131404  	JAF 4			; if A != 0 (batch/mode) -> 051440
051435  044631  	LDA ,B -147		; A := [B -147] = XTTNO  (terminal number)   [interactive]
051436  146156  	RADD CLD SA DT		; T := A   (T = command-input device)
051437  124005  	JMP 5			; -> 051444
051440  054632  	LDX ,B -146		; X := [B -146] = XTTIF (TTIFIELD ptr)        [batch/mode]
051441  052026  	LDT ,X 26		; T := [TTIFIELD + 26B] = RIFIL  (cmd-input file no.)
051442  056012  	LDX ,X 12		; X := [TTIFIELD + 12B] = DFOPP  (opposite/output datafield)
051443  046023  	LDA ,X 23		; A := [outputDF   + 23B] = ROFIL (cmd-output file no.)
051444  054675  	LDX ,B -103		; X := XBCHF (mode) again
051445  146113  	RADD CLD SD DB		; B := D   (restore param base)
051446  146151  	RADD CLD SA DD		; D := A   (D = command-output device)
051447  146175  	RADD CLD SX DA		; A := X   (A = mode / BCHFLAG)
051450  055357  	LDX I -21		; X := fixed P-relative-indirect pointer (return linkage)
051451  146142  	EXIT			; return: T = cmd-input dev, D = cmd-output dev, A = mode
```

### Offset -> symbol equations (VERIFIED from the carve's own symbol tables)
`003-S3CP.symbols.txt`:
```
177631B  WDCNT / XTTNO   (line 1776)   -> ,B -147 = XTTNO   = TTNO (terminal number)
177632B  FCNT1 / XTTIF   (line 1777)   -> ,B -146 = XTTIF   = TTIFIELD (input datafield ptr)
177675B  DED05 / XBCHF   (line 1805)   -> ,B -103 = XBCHF   = BCHFLAG (batch/mode flag)
```
(The alias names WDCNT/FCNT1/DED05 are overlaid record fields at the same negative
displacement; the background-field reading is XBCHF/XTTNO/XTTIF, and RSIO uses exactly
those as a background/command datafield.)

Chased datafield displacements (values confirmed by name across the carve symbol
tables; RIFIL/DFOPP/ROFIL/5IESC are not local to 003-S3CP's filtered list but are
defined in the L07 symbol sets under `segments-ref/*/ *.symbols.txt`):
```
RIFIL = 26B    (matches LDT ,X 26 at 051441 -> command INPUT file number)
DFOPP = 12B    (matches LDX ,X 12 at 051442 -> pointer to opposite/output datafield)
ROFIL = 23B    (matches LDA ,X 23 at 051443 -> command OUTPUT file number)
```

### Register-level result contract (VERIFIED by register trace of the body)
Tracing the `RADD CLD` moves (each `S<src> D<dst>` with `CLD` is a register MOVE):

| Case | 051434 JAF | T on EXIT | D on EXIT | A on EXIT |
|------|-----------|-----------|-----------|-----------|
| BCHFLAG = 0 (interactive) | not taken | XTTNO (TTNO) | XTTNO (TTNO) | 0 (mode) |
| BCHFLAG != 0 (batch/mode) | taken     | TTIFIELD.RIFIL | TTIFIELD.DFOPP.ROFIL | BCHFLAG (mode) |

So:
- **mode** is `XBCHF`/BCHFLAG (`,B -103` = `177675B`); `A` carries it on return. VERIFIED.
- **command-input device** is `T`: `XTTNO` when interactive, else `TTIFIELD.RIFIL`. VERIFIED.
- **command-output device** is `D`: `XTTNO` when interactive, else `TTIFIELD.DFOPP.ROFIL`. VERIFIED.

### "What does InputDev literally contain for an interactive program?"
**VERIFIED: the terminal's logical device number (TTNO), literally.** Line
`051435 044675 LDA ,B -147` loads `XTTNO` (= `TTNO`, `177631B`) and moves it into the
command-input register at `051436`. For interactive programs both input and output
devices resolve to `TTNO`. This is now proven from the RSIO body itself, not inferred
from GDEVTY.

### Directory + user index item (prior UNKNOWN - now settled for THIS worker)
**VERIFIED absent from the RSIO byte-traced field chain.** The worker reads only
`XBCHF` (`-103`), `XTTNO` (`-147`), `XTTIF` (`-146`) and the datafield chain
`RIFIL/DFOPP/ROFIL`. There is no access to any directory-index or user-index field.
The only remaining load is `051450 LDX I -21`, a fixed P-relative-indirect fetch used
as return linkage, not a per-process directory/user field. Conclusion: RSIO=051430B
produces exactly three documented values (mode, cmd-input dev, cmd-output dev); the
"owner's directory + user index" the manual associates with MON 143B is NOT computed
in this worker (obtain it via a dedicated call such as MON 213B GetDirUserIndexes).
Verdict: **the dir+user item is not in this worker (VERIFIED absent); its "UNKNOWN"
in the prior answer is resolved to "not produced here."**

================================================================================
## Q5 - MON 71B DESCF (DisableEscape) - what it MUTATES / OBSERVES
================================================================================

**Short answer: the carve UPGRADES the DISPATCH and the TARGET (which word/bit) to
byte-proven, but does NOT recover the mutation instruction itself. The prior NPL claim
(set `DFLAG` bit `5IESC`, `5IESC` set = escape disabled) is CONSISTENT with the carve
and is the target the carve confirms is real, but the actual "set the bit" opcode is
still INFERRED, not read from these bytes.**

### Dispatch slot (VERIFIED - and it corrects both earlier 71B write-ups)
File: `tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/044-S3IDPIT/044-S3IDPIT.asm`
```
005711  047020  	LDA I ,X 20		; MCTAB[71B] = 047020B = MCDES   (line 1389)
005712  047022  	LDA I ,X 22		; MCTAB[72B] = 047022B = MCEES   (line 1390)
```
`005620B + 71B = 005711B` -> `047020B`; `005620B + 72B = 005712B` -> `047022B`. The
carve's symbol table names these:
`tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/003-S3CP/003-S3CP.symbols.txt`
```
47020B  MCDES   (line 760)      ; Monitor-Call DisableEscape  = MON 71B worker
47022B  MCEES   (line 761)      ; Monitor-Call EnableEscape   = MON 72B worker
```
This is a clean disable/enable pair, two words apart, at the two adjacent MCTAB slots
71B/72B. **This supersedes the `71B-DisableEscape.ASM` "F1643 zero-stub / ND-500-only
DESCF" model** (that folder's body was never updated after the 2026-07-15 correction
banner; the real ND-100 worker is `MCDES` in `003-S3CP`, not `F1643` in `025-S3IRPIT`,
and not the ND-500 `DESCF=112111B`).

### Target word and bit (VERIFIED as real L07 symbols)
From the carve symbol tables:
```
DFLAG = 177766B  (= -12B)   003-S3CP.symbols.txt line 1862 (union incl. DFLAG)
5IESC = 15B                 defined in segment symbol sets (e.g. 011-S3ERRL, 010-S3RTFIL)
```
So the word the NPL answer named (`DFLAG` at datafield offset `-12B`) and the bit
(`5IESC = 15B`) are both real in this L07 image. `5IESC` set = escape disabled is
the meaning used by the escape consumer path (per NPL `MP-P2-TAD.NPL`); the carve
confirms the names/values exist, not the set-instruction.

### What DESCF MUTATES - NOT byte-recovered (INFERRED, unchanged from NPL)
The `MCDES=047020B` worker body does NOT disassemble as recognizable escape/`DFLAG`
code in the carved linear listing. `047020B` falls inside/adjacent to the byte-string
routine `GVSTR=046766B`, and the region `047017B-047076B` is a dense symbol cluster
(`SNUCL 047017B`, `MCDES 047020B`, `MCEES 047022B`, `T1P07 047056B`, `MSDAE 047070B`,
`MGDAE/SERVE 047072B`, `BPOPE 047074B`, `PROPE 047076B`) - spacing consistent with a
table / very short stubs rather than a clean routine. The words at `047020B` decode as
`050047 = LDT 47` / `142600 = SBYT`, i.e. byte-store material, with no
`BSET/BONE 5IESC` into `[df -12B]` visible. Corroborating negatives:
- No access to `DFLAG` offset `-12B` (`,X -12` / `177766B`) anywhere in the escape
  region of `003-S3CP.asm`.
- `ESCON=004273B` and `ESCOF=004414B` (the internal escape on/off routines) are named
  in `011-S3ERRL.symbols.txt` (lines 516, 547) but their code regions are ZERO-filled
  in `011-S3ERRL.asm` (`004273B` and `004414B` both `000000`), so the mutation is not
  in that segment either.

Therefore the concrete "set `DFLAG` bit `5IESC` on the caller's terminal input
datafield" remains **INFERRED** from NPL (`RP-P2-SEGADM.NPL`:
`X.TTIFIELD.DFLAG =: SVDFLAG BONE 5IESC =: X.DFLAG`), CONSISTENT with the carve's
confirmed target word/bit, but the mutation opcode is NOT present/decodable in these
carved bytes.

### What a caller can OBSERVE afterwards / is it pollable?
**No byte evidence of any return value or pollable state** - the carve does not expose
the `MCDES` body, so there is nothing in these bytes that returns or reads escape
state. This is CONSISTENT with the prior "write-only, nothing pollable; success only"
conclusion but is NOT upgraded to byte-proven. The `5IESC` bit lives in the terminal
input datafield, which is not returned by this call in any carved evidence. Verdict:
**pollable = no (INFERRED, consistent with carve); observable return = success only
(INFERRED).**

### The ~2100-calls "bracketing" explanation
**Not addressable from these bytes.** With the `MCDES` body unrecovered, the carve
neither confirms nor refutes the prior INFERRED "per-operation disable/enable
bracketing" story. It stays INFERRED. What the carve does add: MON 71B/72B are a
genuine adjacent worker pair (`MCDES`/`MCEES`), which is consistent with tight
disable/enable bracketing, but that is corroboration, not proof of the loop count.

================================================================================
## Does the carve UPGRADE the prior NPL-only claims?
================================================================================

**Q4 (RSIO): YES - upgraded to source-proven.** The prior answer derived the offset
mapping from L07 symbol equations plus the analogous `GDEVTY` NPL code because the RSIO
body was missing from the NPL tree. The carve contains the RSIO body itself
(`003-S3CP` `051430B-051451B`); reading it confirms every claim:
`,B -103 = XBCHF/BCHFLAG` (mode), `,B -147 = XTTNO/TTNO` (interactive device),
`,B -146 = XTTIF/TTIFIELD` chased `26B=RIFIL / 12B=DFOPP / 23B=ROFIL` (batch in/out),
and interactive InputDev = the terminal's logical device number (TTNO). No
contradiction with the prior answer or the manual. It additionally SETTLES the prior
UNKNOWN: the directory+user index is NOT produced by this worker.

**Q5 (DESCF): PARTIAL upgrade.** The carve upgrades two things to byte-proven that the
NPL answer could not: (1) the real ND-100 dispatch - MON 71B -> `MCTAB[71B]=047020B`
= `MCDES` (with `MCEES=047022B` = MON 72B), correcting the stale `F1643`/ND-500 model
in the `71B-DisableEscape` folder; and (2) that the target `DFLAG=177766B (-12B)` and
`5IESC=15B` are real L07 symbols. It does NOT upgrade the mutation itself: the `MCDES`
worker body is not cleanly decodable in the carve (region reads as string/table
material; `ESCON/ESCOF` code is zero-filled), so "sets `DFLAG` bit `5IESC`", "nothing
pollable", and the "bracketing explains ~2100 calls" claims remain INFERRED - now
CONSISTENT with, but not proven by, the carved bytes.

**Contradictions found:** none with the manual or the prior NPL answer on substance.
One internal carve contradiction resolved: the `mon-analysis/71B-DisableEscape/`
`.ASM`/README body (F1643 zero-stub, ND-500-only `DESCF=112111B`) is DEBUNKED by the
`MCTAB[71B]=MCDES=047020B` bytes; only that folder's 2026-07-15 correction banner is
right, its listing body is stale.
