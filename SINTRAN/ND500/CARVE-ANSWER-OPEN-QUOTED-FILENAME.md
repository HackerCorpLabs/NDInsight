# CARVE ANSWER - MON 50B OPEN quoted-vs-unquoted file-name semantics

Answers [CARVE-REQUEST-OPEN-QUOTED-FILENAME.md](CARVE-REQUEST-OPEN-QUOTED-FILENAME.md).
Source: SINTRAN III L07, L-VSX-500 carve. All addresses are virtual addresses in the
named segment's own space (006-S3FS loads at 26000B; 012-S3SFS is the byte-identical
save copy at the same VAs; 044-S3IDPIT loads at 4000B). Tags: [V] = byte-verified
(word read from the .bin), [I] = inferred with stated basis.

## TL;DR

- The quote is handled by ONE routine: **GCFIL @064670B** (006-S3FS). First character
  '"' (042B) selects the CREATE dispatch (**CROBJ @063726B**); no quote selects the
  LOOKUP-ONLY dispatch (**GFILI @057173B**). The quotes are stripped by the copy loop;
  there is no "create flag" - quoted and unquoted names take different routines. [V]
- Quoted + file already exists = **error 076B "File already exists"**. Not opened,
  not truncated, not versioned. Create happens ONLY if the lookup returns 056B
  no-such / 057B ambiguous (or the found-but-empty sub-case below). [V]
- Unquoted + missing = **error 056B "No such file name"** (46 decimal), for every
  access code. GFILI contains no create call at all - byte-listed pointer pool.
  SINTRAN III has NO write-open-creates. Your auto-create is non-standard, confirmed. [V]
- Version-in-quotes is the same lexical layer: `NAME;"n"` routes to **CRNEW @064410B**
  inside the same GCFIL scan; GVERS also accepts a quoted version. [V]

## 0. Dispatch chain (how MON 50B reaches this code)

[V] `MCTAB[50B] = 103034B` (044-S3IDPIT, MCTAB@005620B; table validated by
MCTAB[5B]=102021B RDISK and MCTAB[144B]=026354B MAGTP; note: this build's slot 200B
is 0). 103034B = **OPFIL** (FILSYS-SYMBOLS exact hit). OPFIL is a 4-entry cluster
(DOPEN=103026B, SCROP=103031B, OPFIL=103034B, +103037B) encoding an open-mode
variant via SSM/SSK flags (103046B-103063B).

[V] The name/create machinery: **FCON @067002B** (file connect) calls
**FFILE @065144B** (pointer cell 067340B = 065144B, call at 067240B); FFILE calls
**GCFIL @064670B** (pointer cell 065273B = 064670B). [I] The OPFIL -> FCON hop is
inferred from role and family placement; everything from GCFIL down is byte-walked.

## 1. Q1: where the '"' is detected and what happens to it

**GCFIL @064670B** ("get/create file"), 006-S3FS:

```
064676  JPL I -> GETCH(030062B)     ; fetch char[0] of the name
064677  171042  SAT 42              ; '"' ?
064700  140065  SKP IF DA EQL ST
064701  124101  JMP -> 065002       ; NOT quoted -> unquoted scan
064702  000675/STZ ,B 14            ; QUOTED: output index := 0
```

Quoted path 064703B-064762B [V]: fetches char[i+1] (one ahead of the opening quote),
validates it (alphanumerics 060-072B / 0101-0132B and the specials ( ) , - " ;
anything else -> error 021B "Illegal character in parameter" at 064746B), and copies
it down one position via PUTCH (064757B-064761B) - i.e. THE QUOTES ARE STRIPPED BY
THE COPY. The closing '"' (064754B test) ends the copy; char after it must be the
' terminator (047B) or error 021B (064766B-064771B). Then:

```
064775  044402  LDA ,B 2
064776  135134  JPL I 134           ; -> ptr 065132B = 063726B = CROBJ  (CREATE)
```

Unquoted path 065002B-065015B [V]: scans to ';' or ' terminator, then:

```
065062  044402  LDA ,B 2
065063  050401  LDT ,B 1
065064  135050  JPL I 50            ; -> ptr 065134B = 057173B = GFILI  (LOOKUP ONLY)
```

So the "create if quoted" convention is implemented as a ROUTINE DISPATCH, not a
flag. Related quote sites [V]:

- **SEPUS @043100B** (user-prefix parser) skips exactly ONE leading '"' before '('
  (043120B-043125B) - this is why a quote can wrap the whole spec including the
  (USER) prefix. A quote anywhere else inside the parens = error 021B (043144B).
- **SEPFS @042622B** (spec splitter, runs on the already-stripped name) treats '"'
  in any part as error 021B (042642B-042644B etc.) - quotes never survive to the
  part level.

## 2. Q2: quoted-name create semantics

**CROBJ @063726B** [V]:

1. Directory-access pre-checks: missing directory-access bits -> error **070B**
   "Not directory access" (063745B-063750B, 063753B-063756B).
2. GVERS parses the version; result defaulted to 1 if none (064007B-064011B).
3. **GOBJI lookup** (call 064034B via ptr 064136B = 056326B). Branches:

```
064035  JMP -> 064042               ; lookup ERROR return:
064042  SAT 56  ... ==056B -> create    ; no such file name
064045  SAT 57  ... ==057B -> create    ; ambiguous file name
        else -> abort with that error
064036  SKP IF DT UEQ 0             ; lookup SUCCESS return:
064037  JMP -> 064051 (create)      ;   T == 0 -> proceed to create
064040  170476  SAA 76              ;   T != 0 -> ERROR 076B "File already exists"
```

4. Create = **COBJE @061502B** (create object entry, call 064055B via ptr 064137B)
   then **CNEWV @063313B** in a loop (064057B-064071B) for quoted version counts > 1.

ANSWER: a quoted name creates ONLY if absent. If the quoted file already exists,
OPEN fails with **076B "File already exists"** (manual error table, ND-60.050.06
line 10378) - it is NOT truncated, NOT overwritten, NOT re-versioned, NOT opened
as-is. [V]

The linker manual's "an existing (unquoted) domain is overwritten" is therefore the
LINKER's doing: it opens the existing file (unquoted, access W/RW) and writes over
its contents. OPEN itself never truncates. [I - linker side not carved; the OPEN
side is V]

Sub-case not fully pinned [I]: lookup success with T == 0 also proceeds to create
(064037B). Most plausibly "object entry exists but holds no version"; the exact
meaning of T from GOBJI's success return was not chased.

## 3. Q3: unquoted name, file does not exist

**GFILI @057173B** [V] is lookup-only. Its complete pointer pool (057353B-057371B):
SPUSH, SEPOB 056645B, GETCH 030062B, SEPFS 042622B, GOBJI 056326B, GVERS 057627B,
GNEXV 057567B, plus three local labels. **No COBJE, no CNEWV, no CROBJ - there is
no create path.** The no-match error from GOBJI is **056B** (= 46 decimal, "No such
file name"; GOBJI's 056/057 codes byte-verified at 056576B-056607B in the earlier
FLPAR carve; manual table line 10351). Your assumption 056B/46 is CONFIRMED.

Access codes never enter this decision: name resolution (GCFIL/GFILI/CROBJ) runs
before and independent of access-code processing in OPFIL. There is no access code
0..9 for which an unquoted missing file is created. [V structurally: the only
create sites reachable from OPEN's resolution are CROBJ/CNEWV, both behind the
quote dispatch]

## 4. Q4: access-code interaction

Uniform. [V] The quote decision is purely lexical, taken in GCFIL on the name
string alone; A/T at the dispatch carry buffer/directory arguments, not the access
code. Access 0 (sequential write) of a missing unquoted file returns 056B exactly
like access 3. Write-access does NOT imply create. (The separate SCROP entry at
103031B is the scratch-open variant and does not create named files either;
scratch files are pre-existing. [I on the scratch detail])

## 5. Q5: the NC compiler case

Honest limit: NC's binary is not in this repo, so its exact OPEN sequence is not
carved. But the SINTRAN side is now byte-certain and it settles the design:

- Real SINTRAN III NEVER creates on an unquoted open, for any access code. [V]
- Therefore, on real hardware NC's output files (A:NRF, A:LIST) can only have come
  from: (a) quoted names passed through to MON 50B, (b) a pre-create via MON 221B
  (CRALF @105562B = MCTAB[221B] [V]), or (c) files that already existed. There is
  no fourth mechanism.
- So enforcing unquoted-missing -> 056B in nd500x is CORRECT and cannot "break NC
  vs real hardware": if NC then fails in the emulator, it was depending on the
  emulator's non-standard auto-create, and the same invocation would fail on a
  real machine. Fix the invocation (quote the output names, or pre-create), not
  the semantics. Recommendation: trace NC's actual MON 50B/221B arguments once
  under the corrected semantics; that will show which of (a)/(b) it uses. [I on
  NC's choice; V on the constraint set]

## 6. Q6: version-in-quotes

Same lexical layer, two sites [V]:

- Inside GCFIL: `NAME;"n"` - after the ';' a '"' routes to the quoted-version
  parser (065016B-065056B) which dispatches **CRNEW @064410B** ("create new
  version") at 065055B-065056B via ptr 065133B.
- GVERS @057627B (the general version parser used by both GFILI and CROBJ) itself
  accepts a quoted version number (SAT 42 at 057644B, quoted-digits branch
  057647B+).

For a plain quoted-name create this does not matter: CROBJ defaults the version to
1 (064007B-064011B) and creates it via COBJE/CNEWV.

## Error codes referenced (ND-60.050.06 error table)

| Code | Meaning | Where returned |
|---|---|---|
| 021B | Illegal character in parameter | GCFIL bad char / bad quote nesting (064746B, 064711B, 064771B); SEPFS/SEPUS quote misuse |
| 056B | No such file name | GOBJI no-match -> GFILI -> unquoted OPEN of missing file |
| 057B | Ambiguous file name | GOBJI multi-match (CROBJ treats it like absent and creates) |
| 070B | Not directory access | CROBJ pre-check (create requires directory access) |
| 076B | File already exists | CROBJ when the quoted file exists (064040B) |

## Evidence register (words re-read from the .bin files)

006-S3FS.bin (base 26000B): 064677B=171042B (SAT 42, first-char test);
064701B=124101B (unquoted branch); 064775B/064776B=044402B/135134B with ptr
065132B=063726B (CROBJ); 065062B-065064B with ptr 065134B=057173B (GFILI);
065055B-065056B with ptr 065133B=064410B (CRNEW); 064035B-064050B (056B/057B ->
create, T!=0 -> SAA 76); 064040B=170476B; 063747B/063755B=170470B (SAA 70);
057173B pointer pool 057353B-057371B (no create targets); 043120B-043125B (SEPUS
leading-quote skip); 042642B-042644B (SEPFS quote -> 021B); 057644B=171042B
(GVERS quoted version); ptr 067340B=065144B (FFILE), call at 067240B; ptr
065273B=064670B (GCFIL from FFILE).

044-S3IDPIT.bin (base 4000B): MCTAB[50B]=103034B OPFIL, MCTAB[43B]=103355B CLOFI,
MCTAB[221B]=105562B CRALF, MCTAB[54B]=106063B MDLFI; validation slots
MCTAB[5B]=102021B, MCTAB[144B]=026354B.

Symbols: FILSYS-SYMBOLS L07 exact hits: OPFIL=103034, GCFIL=064670, CROBJ=063726,
CRNEW=064410, GFILI=057173, COBJE=061502, CNEWV=063313, GOBJI=056326, GVERS=057627,
GNEXV=057567, SEPFS=042622, SEPUS=043100, SEPOB=056645, GETCH=030062, PUTCH=030100,
FFILE=065144, FCON=067002, CRALF=105562.
