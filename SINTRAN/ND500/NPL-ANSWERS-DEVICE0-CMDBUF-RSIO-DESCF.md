# NPL Source Answers: Device-0 / Command Buffer / MON 1B / MON 143B / MON 71B

**Scope**: Answers to five questions about SINTRAN III VSX/500 (L07) internals, from the
NPL source tree at `../NPL-SOURCE/NPL/` (Windows path
`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\`), cross-checked against the L07 symbol
lists at `../NPL-SOURCE/SYMBOLS/L07/`.

**Date**: 2026-07-16

**Before searching the NPL tree for anything else, read the coverage/gap map**
(what the tree can and cannot answer, with redirects to the L07 segment carve for
every gap): `../NPL-SOURCE/NPL-COVERAGE-AND-GAPS.md`

**Critical caveat about this source tree (VERIFIED)**:
The NPL tree is the **s3vs-4 build job, Pass-2 (P2) files only**. Per
`../NPL-SOURCE/CLAUDE.md` ("Source Code Limitations"), the following are **absent**:
the file system (S3FS - only FILSYS-SYMBOLS), the command processor (S3CP), the
resident byte-I/O workers, XMSG, and batch processing. Additionally, the listing
addresses in the NPL files do NOT match L07 exactly (example: label `M1` sits at
listing address 071455 in `MP-P2-2.NPL` line 231, while the L07 carve has M1=071633B;
`GOTAB` sits at listing 071055 vs L07 MGOTA=071233B). Structure and code are the same
family; absolute addresses are from a different generation. The `SYMBOLS/L07/` files,
by contrast, ARE the L07 addresses.

Consequence up front: the workers for MON 1B (YFGET, S3FS), MON 12B (SETOL, S3CP),
MON 143B (RSIO, S3CP) and MON 71B (S3CP/resident) are **referenced but not defined**
in this tree. Their entry addresses exist in the L07 symbol lists
(`SYMBOLS/L07/SYMBOL-1-LIST.SYMB.TXT` line 6559 `SETOL=050666`, line 6504
`RSIO=051430`; `SYMBOLS/L07/FILSYS-SYMBOLS.SYMB.TXT` line 3332 `YFGET=026576` -
all matching the carve). What follows is everything the tree does settle.

---

## Q1 - Device-0 exhaustion semantics

**Verdict: NOT FOUND in this NPL tree.** The code path that would decide between
options (a)-(e) is in the missing sources. I will not pick one.

What is VERIFIED about the path and the structures involved:

1. **MON 1B never dispatches on device number at level 14.** The GOTAB slot-1 handler
   `M1` is two words: it loads the address of external routine `INBT` and activates
   level 4 (BLEV) with it:

   `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\MP-P2-2.NPL` lines 228-245:
   ```
   % ACTIVATE LEVEL 4 FOR MONITOR CALLS THAT WILL BE HANDLED
   % ON LEVEL 4.
   %
   M1:    "INBT";   GO IOB14
   M2:    "OUTBT";  GO IOB14
   ...
   IOB14: *IRW BLEVB DP               % SET MONCALL ROUTINE ADDR ON BLEVL
          A:=1; *IRW BLEVB            % SET BIT #0 IN STATUS REG. ON BLEVL.
          BLEV; *MST PID
          GO RET14
   ```

2. **`INBT` (the level-4 worker that does the device dispatch) is not in the tree.**
   It is referenced only as a quoted external symbol (the line above). Its L07 address
   is `INBT=032471` (`SYMBOLS/L07/SYMBOL-1-LIST.SYMB.TXT` line 5982; neighbours
   `FINBT=032206` line 5798, `5INB=032340` line 5764). No NPL file in the tree
   contains its body. `YFGET` (the S3FS worker in MCTAB[1B]) is likewise
   symbol-only (`FILSYS-SYMBOLS.SYMB.TXT` line 3332).

3. **The command buffer is scanned against a terminator character, and that character
   is apostrophe 47B (0x27).** VERIFIED from two independent resident routines that
   handle command-processor strings:

   `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-MONCALLS.NPL` lines 2246-2252
   (PLREE, writing a command string):
   ```
   FOR X := 0 TO 20 DO            % COPY NAMESTRING
   ...
      T:=PCSTRI; *SBYT          % CSTRING ON SYSTEM SEGMENT(DPIT)
      WHILE A >< ##'              %
   OD
   ```
   and lines 2704-2711 (error-message path, scanning a buffer):
   ```
   ##'=:D; T:="CBUFF"                              % COUNT NUMBER OF BYTES IN ERROR MESSAGE
   X:=0; 176=:L; *1BANK
   DO
       *LBYT
   ```
   Per the NPL language definition (character constants, section 2.4.2 of
   `E:\Dev\Ronny\NDInsight\Developer\Languages\System\NPL-DEVELOPER-GUIDE.md`,
   lines 224-229: `A:=##A % Character 'A' (ASCII code)`), `##'` is the ASCII code
   of `'` = 47B = 0x27. Command-processor strings in these routines are
   terminated/scanned by 0x27, and I found **no byte-count field** for the command
   string - only the byte pointer CPNT (see Q2). Copy loops are bounded by the
   terminator (and in PLREE additionally by the FOR limit 20B).

   INFERRED (my reading, clearly labeled): a reader that consumes the command buffer
   through CPNT would naturally hit the 0x27 terminator as the last meaningful byte.
   This is *consistent with* your option (d) and with your observation of 0x27 on the
   wire, but what YFGET/INBT actually returns on the read AFTER the terminator is in
   the missing S3FS source. UNKNOWN. Do not implement past-terminator behaviour from
   this document.

**Files searched for Q1** (all of `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\`,
plus the 3.9MB combined listing `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\s3vs-4.symb`)
for: `INBT`, `YFGET`, `M8INB`, `B8INB`, `5INB`, `FINBT`, `MINBT`, "COMMAND BUFFER",
"DEVICE 0", byte-input comment phrases. The only INBT-family bodies present are the
stand-alone boot console routine `7INBT` (`PH-P2-MEMTOF.NPL` lines 368-375, raw
IOX 302/303 loop - not the OS path) and the terminal ring-buffer driver
(`MP-P2-TERM-DRIV.NPL`, interrupt side only: FYLLE/HENTE pointers, echo, break).
**Where to hunt next**: S3FS (file 006) and the resident I/O module containing
INBT=032471B - neither is in this repo; this is the same gap the repo's own
CLAUDE.md documents.

---

## Q2 - The command-buffer structure

**Part 1 - the carve's CBUF label is wrong. VERIFIED.**
`CBUF` at 170207B is NOT a command-buffer data area. It is a single INTEGER holding
the address of the current I/O buffer during system startup:

`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\PH-P2-START-BASE.NPL` lines 24-25
(listing address column = 170207/170210):
```
INTEGER CBUF                 % ADDRESS OF CURRENT I/O BUFFER
INTEGER ASCBUF               % LOGICAL START ADDR OF I/O BUFFERS IN
```
It is written all over `PH-P2-OPPSTART.NPL` while carving up the I/O buffer area
(e.g. lines 509-545: `A:=ASCBUF+1777 SHZ -12 SH 12=:ASCBUF=:CBUF`, `CBUF=:X.BUFST`).
"Zero-filled in the L image" is consistent with a startup-only variable, not with a
32-character text buffer.

**Part 2 - the real command-buffer machinery. VERIFIED symbols, structure partly UNKNOWN.**
The command-processor state lives in the per-background-process area on the system
segment (base `BGSYS=144000` in L07). From
`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\SYMBOL-1-LIST.SYMB.TXT`
(all octal, one word each, consecutive):

| Addr (L07) | Symbol | Meaning (source of meaning) |
|---|---|---|
| 144031B | TTNO   | terminal number (GDEVTY code, Q4) |
| 144032B | TTIFI  | TTIFIELD - addr of terminal input datafield (GDEVTY code) |
| 144033B | CPNT   | command-buffer byte pointer ("VALUE OF BYTE POINTER IN COMMAND BUFFER", `5P-P2-MON60.NPL` line 262: `SYMBOL S3CPNT= 142`) |
| 144034B | OPNT   | (by symmetry with CPNT - INFERRED, output pointer) |
| 144035B | CSTRI  | CSTRIN - command string ("CSTRING ON SYSTEM SEGMENT(DPIT)", `RP-P2-MONCALLS.NPL` line 2250) |
| 144036B | OSTRI  | (by symmetry - INFERRED, output string) |

VERIFIED semantics from `RP-P2-MONCALLS.NPL`:
- Line 2238: `INTEGER POINTER PCSTRI:=CSTRIN,PCPNT:=CPNT`
- Lines 2255-2257: `0=: PCPNT ... 0=:PCPNT  % RESTORE COMMAND PROCESSOR POINTER`
  - so **CPNT=0 means "pointer at start of buffer"**; the kernel resets it to 0
  before re-parsing (here, before the command-table lookup `CALL CABLOOK`).
- Lines 2246-2252: the program NAME is written into the CSTRIN string, terminated
  by `##'` (0x27), bounded by FOR X := 0 TO 20 (21B bytes copied max in this path).

UNKNOWN / NOT FOUND: whether CSTRIN at 144035B is the first word of an inline buffer
or a one-word pointer to the text (CPNT/CSTRIN are one word apart, which suggests
pointer variables, but the declaration is in the missing S3CP source); total buffer
size (the manual's 32 characters is not confirmable here); whether name and
parameters share one buffer; any separate byte-count field (none found - exhaustion
appears to be positional: CPNT vs the 0x27 terminator; INFERRED).

Note for the ND-500 side: the resident exports the ADDRESS of CPNT to the ND-500
command processor in the N500 datafield routine table -
`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\s3vs-4.symb` line 5765 (listing address
054402): `RSGMO;1INSE;CPNT;TS3CO;CLADB;5DFRE;5DLRE;CT500` (table begins at 054335
with `5GNSE;RESNA;...` and also contains `ESCON;ESCOF` at 054352). So ND-500-side
code reads CPNT directly rather than via MON60 dispatch.

---

## Q3 - MON 1B dispatch (the M1 fast handler)

**VERIFIED (with the build-generation caveat from the header):**

1. `*MGOTA=*` marks GOTAB (`MP-P2-2.NPL` lines 180-215): a 256-entry level-14 table,
   default `MFELL`, with slot 1B = `M1`, 2B = `M2`, 21B/22B/23B/24B = 8-bit variants,
   63B, 310B, 373B, 376B (`"5INB"`), 377B (`"5OUTB"`), plus `MONERR` and `XMSGY`
   entries. This confirms the carve's MGOTA/GOTAB model.

2. **M1 does no device-number branching at all.** Full body quoted in Q1. Level 14
   only parks the worker address (`"INBT"`) into the level-4 P register and fires
   level 4. So in THIS generation, "fast handler" means "fast activation stub",
   nothing more. If the L07 M1 at 071633B is also 2 words (carve can check the two
   words at 071633B against `LDA (INBT / JMP IOB14` semantics), the model is the same.
   Whether L07's M1 contains extra logic: UNKNOWN from here - but the s3vs-4 M1
   provably does not.

3. **Device-number classes** (the dispatch itself is in the missing INBT, but two
   present routines show the classification the kernel uses):

   - `MP-P2-N500.NPL` lines 3670-3692 (`5GTDF`, resolving a logical device number
     from an ND-500 message):
     ```
     T:=5MBBANK; *AAX DOUTD; LDDTX             % AD=LOGICAL DEVICE NUMBER
     IF A><0 GO NERET                          % ILL. LOG.DEV (MORE THAN 16 BITS)
     IF 1=D THEN ... T:="XDFOPP"; CALL XGTDFADDR         % A=INPUT DATAFIELD
     ...
     FI; A:=D                                  % A=LOGICAL DEVICE NUMBER
     IF A>=100 AND A<=177 GO NERET             % ASSURE IT'S NOT A FILE WHEN CALLING LOGPH ON LEVEL 12
     CALL LOGPH; IF D=0 GO NERET               % NOT OUTPUTDEVICE
     IF D.TYPRING NBIT 5SPLITDF GO NERET       % ERROR IF NOT TERMINAL
     ```
     VERIFIED classes: **device 1 = own terminal (special-cased); 100B-177B = open
     file numbers (never given to LOGPH); everything else resolved through LOGPH
     into an input/output datafield pair.** Device 0 is not special-cased HERE
     (this routine rejects it via the LOGPH path), so device-0 handling is done
     elsewhere (the missing INBT/YFGET). NOT FOUND whether 0 is a table entry or
     an explicit compare in those workers.

   - `RP-P2-MONCALLS.NPL` lines 2603-2629 (`GDEVTY`, MON 263B) shows the same
     pattern for background programs, including the batch/interactive fork - quoted
     in Q4 below.

**Answer to "special-cased or table entry": UNKNOWN for device 0 specifically** -
the deciding code (INBT=032471B / YFGET=026576B) is not in the tree. What is
verified is that device 1 IS special-cased in kernel code paths that do this kind
of resolution, and files are a numeric range (100B-177B), not datafield entries.

---

## Q4 - MON 143B RSIO field layout

**The carve's unproven offsets are now resolved. VERIFIED via two independent
sources: the L07 symbol equations, plus resident NPL code that walks exactly this
chain.** (The RSIO body itself is still missing - the *application* of the mapping
inside RSIO=051430B is INFERRED from the identical mechanism in GDEVTY.)

The three B-relative offsets are the standard background-field displacement symbols
(`SYMBOLS/L07/SYMBOL-1-LIST.SYMB.TXT`; 16-bit two's complement):

| Carved access | Octal value | L07 symbol (line) | Meaning |
|---|---|---|---|
| `,B -103` | 177675B | `XBCHF=177675` (line 4534) | BCHFLAG - batch/mode-job flag; 0 = interactive |
| `,B -147` | 177631B | `XTTNO=177631` (line 4743) | TTNO - terminal number |
| `,B -146` | 177632B | `XTTIF=177632` (line 4535) | TTIFIELD - addr of terminal input datafield |

The chased offsets 26B/12B/23B are terminal-datafield displacements
(`SYMBOL-1-LIST.SYMB.TXT`):

| Offset | Symbol (line) | Meaning |
|---|---|---|
| 26B | `RIFIL=000026` (line 2769) | batch/mode command INPUT file number |
| 12B | `DFOPP=000012` (line 592)  | pointer to opposite (output) datafield |
| 23B | `ROFIL=000023` (line 2770) | command OUTPUT file number |

VERIFIED code walking this exact chain -
`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-MONCALLS.NPL` lines 2606-2620
(GDEVTY):
```
INTEGER POINTER PTTNO:=TTNO,PBCHFLAG:=BCHFLAG, PTTIF:=TTIFIELD
...
GDEVTY:
       CALL GET0; MLEV; *MST PIE
       IF BACKGROUND><0 AND ZTREG=1 THEN
          IF PBCHFLAG=0 THEN
             PTTNO
          ELSE
             X:=PTTIF
             IF ZAREG=0 THEN X.RIFIL ELSE X.DFOPP.ROFIL FI
          FI
       ELSE
          ZTREG
       FI
       CALL LOGPH
```
Reading: for a background program, **if BCHFLAG=0 (interactive) the device is TTNO -
literally the terminal's logical device number; if BCHFLAG is nonzero (batch/mode)
the input device is TTIFIELD.RIFIL (a file number) and the output device is
TTIFIELD.DFOPP.ROFIL.** That is exactly the manual's RSIO statement ("your terminal
number for interactive programs; batch/mode jobs return the file number"), now with
the concrete fields.

So for the carved RSIO frame: mode comes from BCHFLAG (`,B -103`), and
InputDev/OutputDev come from TTNO (`,B -147`) when interactive, else from
TTIFIELD (`,B -146`) -> RIFIL (26B) / DFOPP (12B) -> ROFIL (23B).
Where the values live: the per-process background field (B-relative negative
displacements), with the current process's copies also visible at absolute
144031B/144032B on the system segment (Q2 table). The directory+user index item in
the carve note is NOT resolved here - no candidate offset found: UNKNOWN.

**InputDev for an interactive program = the content of TTNO = the terminal's
logical device number.** VERIFIED for GDEVTY; INFERRED (same fields, missing body)
for RSIO.

---

## Q5 - MON 71B DESCF and the escape/break state

**The DESCF worker body is NOT in the tree** (MON 71B is `MFELL` in GOTAB, so it
goes through MCTAB to a worker in the missing source; the `DESCF` symbol appears
only in the N500 segment lists, e.g. `SYMBOLS/L07/N500-SYMBOLS.SYMB.TXT` line 542
`DESCF=112111` - an ND-500-monitor-segment address, not the ND-100 resident worker).

**What the escape-disable state IS - VERIFIED:**
it is bit `5IESC` (=15B, i.e. bit 13 decimal) in the `DFLAG` word of the terminal
INPUT datafield. `DFLAG=177766` (displacement -12B from the datafield pointer;
`SYMBOLS/L07/SYMBOL-1-LIST.SYMB.TXT` lines 454 and 725). Evidence:

- Disable (kernel brackets a segment-load; note it saves and sets the bit) -
  `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-SEGADM.NPL` line 876:
  ```
  X.TTIFIELD.DFLAG=:SVDFLAG BONE 5IESC =:X.DFLAG     % DISABLE ESCAPE AND REMEMBER ESCAPE STATE.
  ```
  and the matching conditional re-enable, lines 863-870 (`ESPLRE`):
  ```
  ESPLRE: IF SVDFLAG NBIT 5IESC THEN
              ... T:="DFLAG"; CALL XGTDFADDR
              A BZERO 5IESC
              T:="DFLAG"; CALL XSTDFADDR ...
  ```
- The consumer - escape processing tests it,
  `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\MP-P2-TAD.NPL` lines 602-620:
  ```
  ESCDIS: IF X=BDESC OR X=RLOCA THEN                  % ESCAPE OR REMOTE-LOCAL MESSAGE
            IF DFLAG NBIT 5IESC THEN                  % ESCAPE ENABLED
               ...
            ELSE                                   % ESCAPE DISABLED
  ```
  and line 700 (forced logout path): `DFLAG BZERO 5IESC=:DFLAG  % ENABLE ESCAPE`.
- Logout clears it - `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-1.NPL`
  line 120:
  ```
  T:="DFLAG"; CALL XGTDFADDR; A BZERO 5IESC; T:="DFLAG"; CALL XSTDFADDR
  ```

So: **5IESC set = escape DISABLED; clear = enabled.** DESCF's job (per the manual)
is to disable escape; the only state anything in the system consults for that is
this bit. INFERRED (strong, but the worker body is absent): MON 71B sets
DFLAG BONE 5IESC on the caller's terminal input datafield and returns; MON 72B
(EESCF) clears it.

**What a caller can observe afterwards: nothing.** No return value beyond success
is evidenced anywhere; the bit is not readable by any monitor call found in this
tree. Setting an already-set bit is idempotent.

**Why a program might call it repeatedly - UNKNOWN, but one VERIFIED kernel idiom
is suggestive:** SINTRAN's own code brackets individual critical operations with
disable/enable pairs rather than disabling once -
`5P-P2-MON60.NPL` lines 554-596 (`CHM1`/`CHM2`: `CALL ESCOFF ... CALL ESCON`) and
the SPLRE/ESPLRE pair above (once per segment-load). If the ND linker's runtime
does DESCF/EESCF (or DESCF alone) around each of ~2100 small operations, you would
see exactly your trace. That is an INFERRED explanation of the count, not a fact;
the calls themselves are NOT polling anything - there is nothing to poll.

Related but distinct (do not conflate): `FLAGB` (offset 42B, line 1243) carries the
"escape/break has been TYPED" event bits (`5IBRK`, tested in
`CC-P2-N500.NPL` line 51: `IF A BIT 5IBRK THEN ... % HAS ESCAPE/BREAK BEEN TYPED?`),
and the resident->TAD notification of enable/disable is `RCCESC`
(`CC-P2-COMMON.NPL` lines 336-351, "ROUTINE CALLED FROM CDESCFU/CEESCFU (SPIT) TO
SEND ENABLE/DISABLE ESCAPE" - CDESCFU/CEESCFU are the SPIT-side DESCF/EESCF
continuations, bodies missing).

---

## Contradictions with the given manual/carve statements

1. **CBUF=170207B is not the command buffer.** NPL declares it as
   `INTEGER CBUF % ADDRESS OF CURRENT I/O BUFFER` (`PH-P2-START-BASE.NPL` line 24,
   listing address 170207). The command-processor string machinery is
   CPNT=144033B / CSTRIN=144035B on the system segment. The carve label
   "command-buffer DATA area" should be retired or re-verified.
2. **"M1 resident level-14 fast handler" overstates it** (in the s3vs-4 generation):
   M1 is a two-word activation stub; ALL MON 1B work, including any device-0
   special case, happens at level 4 (INBT) or below. Nothing "fast" is handled at
   level 14 for MON 1B.
3. **The manual's "Max 32 chars" for MON 12B could not be confirmed** - no bound
   is visible in the present sources (the only visible copy bound is 20B in the
   PLREE name-copy path, which is a different operation).
4. Minor: the manual says command-buffer reads deliver "normal SINTRAN III command
   editing"; the present sources show command strings are stored ALREADY terminated
   with 0x27 and re-scanned positionally via CPNT - the editing necessarily happened
   at type-in time (terminal driver), not at device-0 read time. (INFERRED from
   structure; the read-side code is missing.)
