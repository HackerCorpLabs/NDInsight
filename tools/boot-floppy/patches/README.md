# Norsk Data SINTRAN III PATCH format

Full path: `E:\Dev\Ronny\NDInsight\tools\boot-floppy\patches\README.md`

Groundwork study: what an ND "SINTRAN III Patch" floppy actually contains, how a
patch identifies *where* and *what* it changes, and how far that gets us towards
(a) identifying the patches on an installed system and (b) replaying a patch set
onto another system.

Everything below is split into **VERIFIED** (read directly out of the media in
this repo / this machine, quoted verbatim) and **INFERRED** (my reading of the
evidence, not proven). Nothing here is taken from memory or from secondary
sources unless the manual is named.

Media analysed: see `inventory.md`.
Scripts: `tools/parse_patch.py`, `tools/diff_system.py`, `tools/read_revle.py`.
Sample outputs actually produced by those scripts: `samples/`.

---

## 1. Headline answer

**VERIFIED.** A SINTRAN III patch file is **not** binary, and it is **not** a
table of address/old/new triples. It is a **plain 7-bit ASCII command script**
that is fed to SINTRAN's own MAC-family assembler/patcher (`FMAC` or `DMAC`) as
if an engineer had typed it at the console. It contains:

* human-readable patch headers with an ND **report number** (the patch ID),
* a **revision letter** per patch,
* MACM directives that select **which coreload / segment save file** to patch,
* MACM **conditional-generation guards** (`"8N500` etc.) so a patch is only
  applied if the target system was generated with that option,
* **open-location deposits** (`SYMBOL+offset/ value`), very often carrying the
  previous contents in a comment `% OLD: nnnnnn`,
* free-form ND-100 assembly source assembled *in place* into spare space.

All nine `.PATC` files measured are 100.0 % printable ASCII after `ndtool -p`
(parity strip). Verified by byte census in `tools/parse_patch.py`'s loader and
by an independent printable-character count during analysis.

The installed system records the patch level it was brought to in a single word
called **`REVLE`**, and that word is readable from a carved system today
(section 6). That is the single most useful result of this study.

---

## 2. What is on a patch floppy

**VERIFIED** (from `ndtool -t` listings, reproduced in `inventory.md`).

| File | Role |
|---|---|
| `PATCHES:PATC` (J, K) / `PATCH-FILE:PATC` (H) | the patch script itself — the payload |
| `PATCH-FILE:MODE` | SINTRAN MODE (batch) file: sets up MACM/DMAC, loads symbol tables, defines segment start/length/first-address symbols, then runs the `.PATC` |
| `START-PATCH-FILE:MODE` | the operator entry point: messages, workmode check, `DEF-SEG-FILE`, load DMAC, run `PATCH-FILE:MODE`, verify, write `REVLE` |
| `SYMBOLS:FADM` | **one line**: `33CPV=<octal patch level>` — the identity of the patch set |
| `REFERENCE:FADM` | list of SINTRAN symbol names the patch script references (`)9ASSM`'d so the assembler resolves them) |
| `PATCH-FILE:OUT` (H floppies) | the *log* of an actual patching run — proves the mechanism |
| `NEW-SYSTEM:PROG` (K) / `MULTI-FUNCTION:PROG` (J) / `FILE-TEST*:PROG`, `CPU-TYPE:PROG` (H) | helper utilities: `WRITE-MESSAGE`, `CHECK-WORKMODE`, `FILE-TEST` (scans the produced `:OUT` files for errors) |

`SYMBOLS:FADM`, verbatim, from `K011411`:

```
33CPV=11411
@
```

and from `H223`:

```
33CPV=000223
@
```

`PATCH-FILE:OUT` on the H floppies states the meaning in plain English
(verbatim, parity stripped):

```
% ===============================================
%  PATCH-FILE FOR SINTRAN-III VS/VSE H-VERSION.
%  SYMBOL '33CPV' BELOW DEFINES REVISION LEVEL!

   33CPV:000223
% ===============================================
```

So **`33CPV` is the patch-set revision level**, and the floppy label numbers
("Patch 223", "Patch 011411", "Patch 11110") are exactly that octal value.

---

## 3. Record structure of a `.PATC` file

### 3.1 The patch record header

**VERIFIED.** Every patch is introduced by a comment block. Two dialects:

H and J series (rich):

```
% ======================================================================
% ==> REPORT: SIN-H         1    PROGRAM: ALL              REASON: E
% ==>
% ==> SUBJECT:
% ==>   LOGIN OVER ND-NET.
% ==>
% ==> SYMPTOM:
% ==>   <ESC> DO NOT WORK OVER ND-NET
% ==>
% ==> ERROR DESCRIPTION::
% ==>   WHEN TESTING FOR BACKGROUND OR COPY-PROGRAM THE ADDRESS OF
% ==>   RTREF IS FETCHED IN PLACE OF THE CONTENT OF RTREF.
```

K series (terse — `PROGRAM:`/`REASON:` dropped, and many K records carry no
descriptive text at all):

```
% ==> REPORT: SIN-K       451
```

Fields:

| Field | Meaning | Evidence |
|---|---|---|
| `SIN-H` / `SIN-J` / `SIN-K` | SINTRAN version family the report belongs to | VERIFIED — matches floppy labels and `33CPV` prefixes |
| number | the ND error-report number = the **patch identifier** | VERIFIED — monotone sets, stable across floppies of the same family |
| optional letter after the number | revision of *that individual patch* | VERIFIED — see the worked example in §5 |
| `PROGRAM:` | which product variant it applies to (`ALL`, `VSE`, `VSX/500`, `RT-LOADER`, …) | VERIFIED |
| `REASON:` | always `E` in every file measured (presumably "Error") | VERIFIED that it is always `E`; the expansion is INFERRED |

Counts (produced by `tools/parse_patch.py --summary`):

| Floppy | records | report-number range |
|---|---|---|
| H-17 | 94 | 1–144 |
| H-223 | 124 | 1–184 |
| H (nddisk5, `33CPV=002204`) | 126 | 1–189 |
| J-10300 | 236 | 1–309 |
| J-11100 | 258 | 1–337 |
| J-11110 | 258 | 1–337 |
| K-10200 | 284 | 7–417 |
| K-011411 | 297 | 7–451 |

**Note the numbering is not dense** — H-223 has 124 records for numbers up to
184. Report numbers are *ND's global error-report numbers*, not a patch index;
some reports simply do not produce a SINTRAN patch. Also **H/J files are in
ascending report order, K files in descending order**. VERIFIED by inspection.

### 3.2 Where the patch is applied — the target context

**VERIFIED.** Three nested pieces of context are in force when a deposit
happens. `tools/parse_patch.py` tracks all three and stamps them onto every
deposit record.

1. **Which program / which image.** A SINTRAN command line inside the `.PATC`
   selects the tool, and the *next* line answers its `IMAGE-FILE :` prompt:

   ```
   @CONTINUE
   SINTRAN:DATA
   ```

   H/J patch files target the **generated system image files**
   `SINTRAN:DATA` and `MACM-AREA:DATA` via `FMAC`
   (`@FMAC` / `@CONTINUE`, verified against the `IMAGE-FILE :` prompt visible
   in `H17/PATCH-FILE:OUT`).
   K patch files run entirely under **`DMAC`** — one `@DMAC` at the top of the
   file and nothing else.

2. **Which coreload / segment.** `)CLOAD <n>` (MACM manual ND-60.009.02 §3.6:
   *"Defines the current coreload number, N1, to be used by MACM henceforth"*).

   * H/J use **octal segment numbers**: `)CLOAD 2`, `)CLOAD 17`, `)CLOAD 26
     % ND-NET SAVE SEGMENT`, `)CLOAD 42`. These map through the SINTRAN
     segment table in ND-820023 (e.g. `6 = S3FS`, `7 = S3DMAC`,
     `17 = S3SMPIT`).
   * K uses **segment save-file names directly**: `)CLOAD S3SSM5`,
     `)CLOAD S3SMPIT`, `)CLOAD S3SRPIT`, `)CLOAD S3SAVE`, `)CLOAD S3FSSV`,
     `)CLOAD S3OPCSV`, `)CLOAD S3SDPIT`, `)CLOAD S3SRTC`, `)CLOAD S3S5PIT` …

   Target distribution measured by `parse_patch.py --summary` (deposit words):

   ```
   K-011411:  S3SMPIT 1878 | S3SRPIT 1337 | S3SSM5 931 | S3OPCSV 601
              S3SAVE 522 | S3FSSV 490 | S3SRTC 141 | S3S5PIT 120 | S3SDPIT 103 ...
   J-11110:   SINTRAN:DATA 2699 | MACM-AREA:DATA 942 | seg 17: 857 | seg 2: 330 ...
   H-17:      seg 2: 1188 | seg 22: 65 | seg 36: 63 | seg 26: 11 | seg 13: 6
   ```

3. **Conditional-generation guard.** A line beginning `"` opens a block that is
   only assembled if the named MACM generation flags are set; a bare `"` closes
   it. Example:

   ```
   "8N500 WM500
   ...
   "
   ```

   `8N500` is the ND-500 option flag; `8MT1+8MT2+8MT3+8MT4` guards a magtape
   patch; `8CLI1+…+8CLI9` guards the CLI drivers. VERIFIED that these appear and
   bracket patch bodies; that they are MACM conditional-generation flags is
   INFERRED from the `8xxxx` naming shared with the generation stream's
   `)MCDEF` flags.

   Guard census (top): K-011411 `8N500`×132, `8STRD`×17, `8MBN0`×15,
   `8LAMU`×11. J-11110 `8N500`×64, `8F5UD`×18, `8PIOC`×14.

### 3.3 What the patch writes — deposits

**VERIFIED.** The workhorse line is MACM's open-location form:

```
<address-expression>/ <value>            % OLD: <previous octal contents>
```

Real examples:

```
IBM2+6/ JMP *1                    % OLD:   763
OPSYD+27/ JMP I *+1                % OLD: 142065
       */ SG62F                    % OLD: 124004
27622/ 21712           % OLD: 027333
RTREC+1276/ ^+I           % OLD: 044076
```

* the address is a **symbol + octal offset**, or `*` (current location), or
  `*+n`, or a bare octal address;
* the value may be an ND-100 **assembly mnemonic**, a symbol, an octal
  constant, or an expression (`^` = current contents, so `^+I` = "add I to
  what's there");
* `% OLD: nnnnnn` records the pre-patch contents. This is the crucial field for
  verification and for reverse-application.

Coverage of `% OLD:` (from `--summary`):

| File | open-location deposits | with `% OLD:` |
|---|---|---|
| H-17 | 433 | 305 (70 %) |
| J-11110 | 1446 | 858 (59 %) |
| K-011411 | 1994 | 1276 (64 %) |

### 3.4 Sequential deposition

**INFERRED (strongly evidenced).** After an open-location line, subsequent
source lines deposit into consecutive words until a `)` directive intervenes:

```
7ENDC/ PIOF
LDA   (SPECI
IRW   30 DP      % Execute routine SPECI
SAA   10
...
SPEC2, JMP I (ESCQE
)FILL
```

`)FILL` terminates. `LABEL,` at line start defines a symbol at the current
location. This is standard MAC/MACM console behaviour and the layout is
consistent across all nine files, but I have **not** found the sentence in a
manual that states it, so it is marked inferred. `parse_patch.py` emits these as
`"kind": "sequential"` records with `base_expr` + `word_offset`, never as
addresses it claims to know.

### 3.5 Supporting directives seen

**VERIFIED** (they appear; the semantics marked ? are not proven):

| Directive | Count in K-011411 | Meaning |
|---|---|---|
| `)CLOAD x` | 477 | select coreload / segment (manual ND-60.009.02 §3.6) |
| `)KILL sym …` | 833 | remove symbol(s) from the symbol table (?) |
| `)FILL` | 348 | terminate the open-location fill (?) |
| `)RESSM` | 144 | restore/reset symbol table (?) |
| `)9ASSM file` | (H/J) | assemble/read a symbol or reference file |
| `)CLEAR`, `)LIST`, `)ULIST`, `)SYSDF`, `)9TABL`, `)9EXIT` | | MACM housekeeping |

`)9TABL XXX YYY ZZZ` at the head of `PATCH-FILE:MODE` sizes the symbol tables.

---

## 4. `PATCH-FILE:MODE` — how a patch run finds the system

**VERIFIED**, verbatim from `K011411/PATCH-FILE:MODE`:

```
@DMAC
)CLEAR
XXX=4000; YYY=200; ZZZ=4000
)9TABL XXX YYY ZZZ
)CLOAD S3FSSV
SG12S=26000
26001/  SG12L=^
26002/  SG12F=^
SG12O=SG12F
)CLOAD S3OPCSV
SG13S=30000
SG13S+1/SG13L=^
SG13S+2/SG13F=^
SG13O=SG13F
...
)CLOAD 52
25/ SG17S=^
26/ SG17L=^
27/ SG17F=^
SG17O=SG17F
...
)CLOAD S3PATCH
174000/REVLE
)9ASSM (ND-PATCH-SIN-:SYSTEM)REFERENCE:FADM
)9ASSM (ND-PATCH-SIN-:SYSTEM)SYMBOLS:FADM
)SYSDF
)9ASSM (SYSTEM)SYMBOL-1-LIST
)SYSDF
)9ASSM (SYSTEM)SYMBOL-2-LIST
```

Reading: `X/ SYM=^` **reads** the current contents of location X into symbol
`SYM` (`^` = contents of the open location). So the MODE file *interrogates the
target system's segment table* to learn each segment's **S**tart, **L**ength and
**F**irst address (`SG12S/L/F`, `SG13S/L/F`, `SG17S/L/F`, `SGCCS/L/F`, …) before
the patch script runs. That is how a patch written once can be applied to a
site-specific generated system. **VERIFIED** in structure; the exact expansion
of the S/L/F suffixes is INFERRED from usage (`SG46L=37777` is forced, which
only makes sense as a length).

Then it `)9ASSM`s the site's own `SYMBOL-1-LIST` / `SYMBOL-2-LIST`, so every
`OPSYD+27/` in the patch script resolves against **that machine's** symbol
values.

**This is the key portability property**: the patch is symbolic, not absolute.
Applying it needs the target system's symbol lists, not a matching binary.

---

## 5. Worked example: SIN-J 315 revision C → D

**VERIFIED.** The J-11100 and J-11110 floppies carry byte-identical
`MULTI-FUNCTION:PROG`, `PATCH-FILE:MODE`, `START-PATCH-FILE:MODE` and
`REFERENCE:FADM`, and `PATCHES:PATC` files of *identical length* (232 179
bytes) but different MD5. The entire difference is:

```
427c427
< % ==> REPORT: SIN-J       315 C  PROGRAM: ALL              REASON: E
> % ==> REPORT: SIN-J       315 D  PROGRAM: ALL              REASON: E
454c454
< STA ,X INDX1
> STD ,X INDX1
466c466
< STA ,X INDX2
> STD ,X INDX2
```

and `SYMBOLS:FADM` changing `33CPV=011100` → `33CPV=011110`.

So: **a whole patch-set revision (11100 → 11110) can consist of a single patch
being re-issued at the next revision letter, changing two instructions.**
This confirms the revision-letter semantics and confirms `33CPV` versions the
*set*, while the letter versions the *individual patch*.

Parsed form: `samples/J-11110-report-315.json`.

Second worked example, fully parsed: `samples/K-011411-report-449.json`
(SIN-K 449, guard `8N500 WM500`, target `S3SSM5`, three open-location deposits
two of which carry `% OLD:`, plus twelve sequential words of new code).

---

## 6. `REVLE` — reading the patch level off an installed system

This is the most operationally valuable finding.

**VERIFIED chain:**

1. `SYMBOLS:FADM` on the floppy defines `33CPV = <patch level>`.
2. `START-PATCH-FILE:MODE` (K-011411), verbatim, at the very end of a
   successful run:

   ```
   @DMAC
   )CLEAR
   )CLOAD S3PATCH
   176000/ SYSNO; 33CPU; 33CPN; HWINF; FCPUN; REVLE; 33CPV
   )SYSDF
   )9ASSM SYMB-2-LIST
   )SYSDF
   )9ASSM SYMB-1-LIST
   )SYSDF
   )9ASSM SYMBOLS:FADM
   )CLOAD S3SDPIT
   SYSNO/ 33CPN
   FCPUN/ 33CPN
   HWINF+2/ 33CPU
   REVLE/   33CPV; 33CPV:
   ```

   i.e. **`REVLE/ 33CPV` writes the patch level into the word `REVLE` of
   segment `S3SDPIT`.**
3. `REVLE = 004057` (octal) in **all three** SINTRAN symbol lists in this repo:
   `SINTRAN/NPL-SOURCE/SYMBOLS/K03/SYMBOL-1-LIST.SYMB.TXT`,
   `.../L07/...`, `.../M06/...`. The value is identical in K, L and M.
4. The carver's `S3SDPIT` / `S3IDPIT` segments load at octal `4000`
   (`053-S3SDPIT.meta.json` → `load_address.oct = "4000"`), so `REVLE` is word
   octal 57 of that segment.

**Measured result** (`tools/read_revle.py`, output saved as
`samples/carved-revle.json`):

```
system                   segment   REVLE    SYSNO
K-VSX-500                S3IDPIT   010200   000144
L-VSX-500                S3IDPIT   000000   000146
L-VSX-500                S3SDPIT   000000   000146
M-VSX-500                S3IDPIT   003200   000144
M-VSX-500                S3SDPIT   003200   000144
```

**K-VSX-500 reports `REVLE = 010200`, and we physically hold the matching patch
floppy** — `D:\ND\S\N-250306K05-patch.img`, whose `SYMBOLS-10200:FADM` reads
`33CPV=10200` and whose payload file is literally named `PATCHES-10200:PATC`.
That is an independent cross-check of the whole chain.

M-VSX-500 reports `REVLE = 003200`; we do **not** hold an M-3200 patch floppy.

L-VSX-500 reports `REVLE = 000000`. **INFERRED**: that L system was never
patched with an ND patch floppy (or was patched by a mechanism that does not
write `REVLE`). It is *not* proof of an unmodified system — see §8.

### 6.1 The `S3PATCH` segment

**VERIFIED.** `S3PATCH` is segment octal 43, 2 pages, described by the carver
metadata as *"Used for patching purposes"*, loading at octal 174000 (K, L) —
M's residue sits at 176000.

Contents measured:

| System | `S3PATCH` |
|---|---|
| K-VSX-500 | non-zero; **word at 174000 = `004057`** |
| L-VSX-500 | **all zero** (4096 bytes, byte sum 0) |
| M-VSX-500 | non-zero; **word at 176000 = `004057`** |

`004057` is exactly the octal address of `REVLE`. And `PATCH-FILE:MODE` on the
K floppy contains, verbatim, `)CLOAD S3PATCH` / `174000/REVLE`.

So **`S3PATCH` is a scratch/communication coreload used by the patch MODE files
to hand symbol *addresses* to DMAC, and the residue of the last patch run is
still sitting in it on both the K and M carved systems.** The rest of its
content is DMAC working data and I have **not** decoded it — it does *not*
appear to be a patch log or a list of applied report numbers. L's `S3PATCH`
being all zero is consistent with L's `REVLE = 0`: that system shows no trace
of ever having been patched.

**There is no patch inventory anywhere on the system.** A patched SINTRAN
records exactly one number — `REVLE` — and nothing about *which* individual
reports were applied. That is a hard limitation, not a gap in this analysis.

---

## 7. Cross-version comparison

**VERIFIED.**

| | H (17 / 223 / 2204) | J (10300 / 11100 / 11110) | K (10200 / 011411) |
|---|---|---|---|
| Volume label | `PATCH-SINTRAN` | `ND-PATCH-SIN-J` | `ND-PATCH-SIN-K` |
| Payload file | `PATCH-FILE:PATC` | `PATCHES:PATC` | `PATCHES:PATC` / `PATCHES-10200:PATC` |
| Order | ascending report no. | descending | descending |
| Header richness | full (SUBJECT/SYMPTOM/DESCRIPTION) | full | minimal, mostly bare `REPORT:` |
| Tool | `FMAC` + `DMAC`, alternating | `FMAC` + `DMAC`, alternating | `DMAC` only |
| Target | image files `SINTRAN:DATA`, `MACM-AREA:DATA` + numeric coreloads | same | **named segment save files** (`S3SSM5`, `S3SMPIT`, …) |
| `)RESSM` used | no | 2× | 144× |
| Helper program | `FILE-TEST:PROG`, `CPU-TYPE:PROG` | `MULTI-FUNCTION:PROG` | `NEW-SYSTEM:PROG` |
| `START-PATCH-FILE:MODE` | absent (H-17/H-223 use `PATCH-FILE-F/G:MODE`) | present | present |

The format is **one family, evolving**: same header comment convention, same
`% OLD:` convention, same `)CLOAD`/open-location deposit mechanics, same
`33CPV`/`SYMBOLS:FADM` identity file throughout 1983 → 1988.

The visible architectural shift is H/J → K: H and J patch the *generated system
image files*; K patches the *individual segment save files* by name. INFERRED
consequence: a K-era patch run needs the live system's segment files present,
which matches its `START-PATCH-FILE:MODE` doing `DEF-SEG-FILE ... SEGFIL0`
first.

Patch sets are **mostly but not strictly cumulative**. Measured by comparing the
parsed record sets (report number → revision letter):

| Step | added | **removed** | revision-letter changes |
|---|---|---|---|
| H-17 → H-223 | 33 (145…184) | **3** (19, 94, 111) | 10, incl. 3 C→D, 105 C→E, 142 B→G |
| H-223 → H-2204 | 3 (185, 188, 189) | **1** (14) | 0 |
| J-10300 → J-11100 | 22 (311…337) | 0 | 6, incl. 208 B→D |
| J-11100 → J-11110 | 0 | 0 | **1** (315 C→D) — the whole release |
| K-10200 → K-011411 | 13 (428…451) | 0 | 8, incl. 160 D→G, 162 I→J, 164 A→C |

The H-series *withdrew* four reports across two releases. So "the system is at
`REVLE` = X" does **not** imply "every report numbered ≤ X is applied", and a
later set is not always a superset of an earlier one. VERIFIED.

---

## 8. What this does NOT give us

Stated plainly, because it bounds everything in `WORKFLOW.md`:

1. **No patch inventory on the system.** Only `REVLE` (one word). If a site
   engineer hand-patched something, or applied a partial set, nothing records
   it.
2. **`REVLE = 0` does not mean "unmodified".** It means "no ND patch floppy
   wrote a level here". Site patches, and any patch applied before this
   convention existed, are invisible.
3. **The patch scripts are symbolic, not absolute.** You cannot compute the
   target address of `OPSYD+27` without the *target system's* `SYMBOL-1-LIST` /
   `SYMBOL-2-LIST`, and those are generation-specific. We have K03, L07 and M06
   symbol lists in this repo; that is a partial set.
4. **The values are ND-100 assembly, not numbers.** `JMP I *+1` must be
   assembled before it can be compared against a binary. `parse_patch.py`
   deliberately does *not* assemble — it emits the source expression. Turning
   the JSON into words needs an ND-100 assembler (the repo has `nd100-as`).
5. **Conditional guards are unresolved.** Whether `8N500` was set at generation
   time on a given machine is not recorded in the parsed output; it has to come
   from the machine's generation stream.
6. **No binary as-shipped baseline for the carved systems.** The distribution
   material we hold for L (`D:\ND\extract\VSXL1\SINTRAN-L-1.DATA`) is a MACM
   *source/generation stream*, not a segment image. So `diff_system.py` cannot
   yet be pointed at a true "as-shipped vs installed" pair for L — we would have
   to run the generation to produce one.

---

## 9. Scripts

All run under WSL Ubuntu, `python3` (3.10 tested), no third-party modules.

### `tools/parse_patch.py`

```
python3 parse_patch.py PATCHES.PATC --summary
python3 parse_patch.py PATCHES.PATC --report 449          # one record, JSON
python3 parse_patch.py PATCHES.PATC --deposits-only       # flat deposit list
```

Emits per patch record: system, report, revision letter, PROGRAM, REASON,
description text, the target context (program / image file / coreload / guard),
the directive trace, symbol assignments, every deposit (`open` and
`sequential`), and — importantly — `unparsed_lines` for anything it did not
classify, so nothing is silently dropped.

Real output on all seven distinct patch files:

```
file            H17/PATCH-FILE.PATC      94 records, 433 open + 900 seq deposits,   9 unparsed
file            H223/PATCH-FILE.PATC    124 records, 519 open + 1549 seq deposits,  27 unparsed
file            nddisk5/PATCH-FILE.PATC 126 records, 522 open + 1610 seq deposits,  27 unparsed
file            J10300/PATCHES.PATC     236 records, 1311 open + 3475 seq deposits, 426 unparsed
file            J11110/PATCHES.PATC     258 records, 1446 open + 3795 seq deposits, 438 unparsed
file            K011411/PATCHES.PATC    297 records, 1994 open + 4411 seq deposits, 173 unparsed
file            K05patch/PATCHES-10200  284 records, 1875 open + 3948 seq deposits, 171 unparsed
```

The "unparsed" lines are chiefly multi-line `)KILL` operand lists and stray
text; they are preserved verbatim in the JSON. **The parser does not claim to
resolve addresses or assemble values** — that is out of scope and would be a
lie if faked.

### `tools/diff_system.py`

Word-diffs two ND-100 big-endian images and emits candidate patch records in a
comparable JSON shape, optionally annotating each address with the nearest
preceding symbol from a `SYMBOL-n-LIST` file.

Demonstrated run (`samples/L-VSX-500-IDPIT-vs-SDPIT.json`) — the *initial* vs
*current* copies of L-VSX-500's data PIT, 13 runs / 15 words:

```
LOADI     000001 -> 000000
FIXCL     164000 -> 000000
XTMRT     000000 -> 177777
XRTAC+6   ...
```

That is exactly the shape a real identified patch would take. Directory mode
also works (`--dirs K-VSX-500/segments L-VSX-500/segments`, 29 comparable
segments, 12 100 runs / 491 328 words — correctly showing that two *different*
SINTRAN versions are not a usable baseline).

### `tools/read_revle.py`

Reads `REVLE` (and `SYSNO`) out of a carved system's `S3SDPIT`/`S3IDPIT`.
Output above in §6.

### `tools/check_applied.py` — PILOT

Tests a `.PATC` file against a carved system using the `% OLD:` values as
fingerprints — **no baseline image needed**. Real run, K-10200 patch set vs the
carved K-VSX-500 system (which reports `REVLE = 010200`, i.e. *should* have this
exact set applied), saved as `samples/K-10200-vs-K-VSX-500-pilot.txt`:

```
  total                5823
  open                 1875
  has-OLD              1251
  unknown-segment       271
  complex-address       522
  unknown-symbol        244

  APPLIED                 1
  NOT-APPLIED             8
  UNRESOLVED-NEW        200
  MISMATCH                5
```

**This result is inconclusive and the script says so.** 200 of the 214
resolvable deposits cannot be decided because their new value is ND-100 assembly
source, and this script refuses to guess. The 5 outright mismatches most likely
mean the generic `SYMBOLS\K03` list is not this machine's generation-specific
symbol list. See `WORKFLOW.md` §"A3 pilot" for the full reading.

---

## 10. Next investigative steps

1. **Get an as-shipped binary baseline.** Either run the L/M generation stream
   through an emulated SINTRAN to produce virgin segments, or carve a
   never-patched disk of the same version. Without it, `diff_system.py` has
   nothing honest to diff against.
2. **Assemble the patch deposits.** Wire `parse_patch.py`'s output through
   `nd100-as` + the matching `SYMBOL-1/2-LIST` to turn `JMP I *+1` into words
   and `OPSYD+27` into an absolute address. Then a patch record becomes directly
   comparable to a `diff_system.py` record, and *matching by `% OLD:` value*
   becomes possible — that is the real "which patches are present" test, and it
   works even without a baseline image, because 60–70 % of deposits carry their
   pre-patch word.
3. **Confirm `)KILL` / `)FILL` / `)RESSM` semantics** from the MAC/DMAC manual
   (`ND-60.096.01 MAC Interactive Assembly and Debugging System User's Guide`
   is in `Reference-Manuals/`) before any attempt to *replay* a patch.
4. **Decode the rest of `S3PATCH`** on K-VSX-500 and M-VSX-500 to confirm it is
   only DMAC scratch and holds no applied-patch list.
5. **Look for an M-series patch floppy** to match M-VSX-500's `REVLE = 003200`.
