# NPL Source Tree: Coverage and Gaps

**Purpose**: answer one question before you search this tree: *can it answer your
question at all, and if not, where do you go instead?* A bare "not found in NPL" has
already caused one costly detour (it was read as "not available anywhere" when the
disassembled bytes were sitting in the segment carve). Every gap below therefore
carries a redirect.

**Companion document** (the detailed Q and A this map was distilled from, with full
quotes): `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\NPL-ANSWERS-DEVICE0-CMDBUF-RSIO-DESCF.md`

**Date**: 2026-07-16. Labels: VERIFIED (quoted from source/symbols), INFERRED
(reading, clearly marked), UNKNOWN, CARVER-REPORTED (byte results relayed by the
carver agent, not independently re-checked here).

---

## 1. What this tree actually is (read this first)

**VERIFIED**: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\` is the **s3vs-4 build
job, Pass-2 (P2) compilation units only**. It is **NOT the L07 build**:

- Listing addresses in the NPL files belong to the s3vs-4 generation and do NOT match
  L07. Example: label `M1` is at listing address 071455 in
  `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\MP-P2-2.NPL` line 231, while L07 has
  M1=071633B. `GOTAB` is at listing 071055 (same file, line 184) vs L07 MGOTA=071233B.
  Code structure is the same family; absolute addresses are not.
- The **symbol lists ARE per-version**: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\`
  (and K03, M06) hold real L07 addresses. Use the NPL for logic, the symbol lists for
  L07 addresses, never the NPL listing column for L07 addresses.
- Addresses in `s3vs-4.symb` and the NPL listing columns **overlap across compilation
  units** (each unit restarts its address space). Grepping an octal address across the
  tree WILL produce false hits in unrelated units. Match address + surrounding code,
  never address alone.
- Symbol names are truncated to 5 characters in the symbol lists (CSTRIN -> CSTRI,
  TTIFIELD -> TTIFI, BCHFLAG -> BCHFL/XBCHF).

Consequence: every "not found" verdict below means "not in the s3vs-4 P2 units" -
it says nothing about L07 bytes, which mostly DO exist in the segment carve (section 3).

---

## 2. What IS present, by area

All paths under `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\`. (File-to-subsystem
mapping also in `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\CLAUDE.md` and `README.md`.)

| Area | File | Notable contents (with line refs) |
|---|---|---|
| Level-14 internal interrupts, GOTAB, moncall fast stubs | `MP-P2-2.NPL` | `*MGOTA=*` + 256-entry `GOTAB` (lines 180-215); `M1`..`M377` two-word stubs + `IOB14` level-4 activation (lines 231-245); page-fault handler `IPAGFAULT` (line 283+) |
| Resident monitor calls (upper) | `RP-P2-MONCALLS.NPL` | `GDEVTY` MON 263B with the background device-resolution chain BCHFLAG/TTNO/TTIFIELD.RIFIL/DFOPP.ROFIL (lines 2603-2651); `PLREE` with `PCSTRI`/`PCPNT` command-string handling (lines 2236-2257); error-message `CBUFF(100)` scan with `##'` terminator (lines 2701-2721) |
| Startup base variables | `PH-P2-START-BASE.NPL` | `INTEGER CBUF % ADDRESS OF CURRENT I/O BUFFER` (line 24) - see section 4 for the mislabel warning |
| System startup / I/O buffer carving | `PH-P2-OPPSTART.NPL` | I/O buffer area setup writing CBUF/ASCBUF (lines 506-548, 1068-1115) |
| Terminal driver (interrupt side) | `MP-P2-TERM-DRIV.NPL` | ring buffer FYLLE/HENTE, echo, break, XON/XOFF, DFLAG bit handling throughout (e.g. lines 36-90) |
| ND-500 monitor-level interface | `MP-P2-N500.NPL` | `5GTDF` logical-device classing: dev 1 = own terminal, 100B-177B = files, else LOGPH (lines 3670-3692); break/echo strategy transfer (lines 1847-1917) |
| ND-500 MON 60B dispatch | `5P-P2-MON60.NPL` | subfunction SYMBOL catalog incl. `S3CPNT=142 % VALUE OF BYTE POINTER IN COMMAND BUFFER` (line 262); ESCOFF/ESCON bracketing idiom `CHM1`/`CHM2` (lines 554-596) |
| ND-500 command-processor glue | `CC-P2-N500.NPL`, and the N500 datafield routine table (see `s3vs-4.symb` listing 054335-054427, incl. `ESCON;ESCOF` and `CPNT` exported by address) | |
| Resident common (SPIT interface stubs) | `CC-P2-COMMON.NPL` | `RCCESC` "CALLED FROM CDESCFU/CEESCFU (SPIT) TO SEND ENABLE/DISABLE ESCAPE" (lines 336-351) |
| Segment administration | `RP-P2-SEGADM.NPL` | `SPLRE`/`ESPLRE` escape-disable bracketing with DFLAG bit 5IESC (lines 863-876) |
| TAD, SCSI, HDLC, PIOC, X21, PERF, DIMIR | `MP-P2-TAD.NPL`, `RP-P2-TAD.NPL`, `IP-P2-SCSI-*.NPL`, `MP-P2-HDLC-DRIV.NPL`, ... | driver bodies present |
| Boot/standalone console I/O | `PH-P2-MEMTOF.NPL` | `7INBT`/`7OUTB` raw IOX 302/303 console loop (lines 368-375) - NOT the OS byte-I/O path |

---

## 3. What is MISSING - and where to go instead

Missing means: referenced as an external symbol, body in no NPL file and not in
`s3vs-4.symb` either. The repo's own `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\CLAUDE.md`
("Source Code Limitations") lists most of these.

Redirect root (L07 disassembled segment carve):
`E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\`

| Missing here | L07 entry (symbol list) | Go instead to |
|---|---|---|
| File system S3FS - incl. **YFGET** (MON 1B worker, MCTAB[1B]) - the device-0 exhaustion answer lives here | `YFGET=026576` (`SYMBOLS\L07\FILSYS-SYMBOLS.SYMB.TXT` line 3332) | `...\re\segments-ref\006-S3FS\006-S3FS.asm` |
| Command processor S3CP - incl. **SETOL** (MON 12B) and **RSIO** (MON 143B) bodies, CPNT/CSTRIN declarations | `SETOL=050666` (line 6559), `RSIO=051430` (line 6504) in `SYMBOLS\L07\SYMBOL-1-LIST.SYMB.TXT` | `...\re\segments-ref\003-S3CP\003-S3CP.asm` |
| **M1** L07 body (level-14 stub) | M1=071633B (carve) | `...\re\segments-ref\026-S3IMPIT\026-S3IMPIT.asm` |
| Command-buffer data area / 025 segment (CPNT/CSTRIN live copies) | CPNT=144033B, CSTRI=144035B (`SYMBOL-1-LIST` lines 2362, 3339) | `...\re\segments-ref\025-S3IRPIT\025-S3IRPIT.asm` |
| **MON 71B DESCF / 72B EESCF workers** | CARVER-REPORTED dispatch: MON 71B -> MCDES=047020B, MON 72B -> MCEES | `...\re\mon-analysis\71B-DisableEscape\` (note: carver reports this folder contained a stale ND-500 body, being fixed) |
| Resident level-4 byte-I/O worker **INBT** (referenced by `M1: "INBT"; GO IOB14`, never defined) | `INBT=032471` (`SYMBOL-1-LIST` line 5982; neighbours `FINBT=032206`, `5INB=032340`) | Segment membership UNRESOLVED as of this writing - 032471B collides with 006-S3FS coordinate space, and segment addresses overlap (section 1). Carver is resolving code-vs-data and the owning segment. If it proves resident-only/non-carveable, the fallback is a live nd100x single-step trace of INBT (nd500-debug workflow). Check `...\re\mon-analysis\` for the landed result before re-searching anything. |
| RT-monitor reservation core (BRESERVE/BRELEASE, RESRV/RELES) | entries per `NPL-SOURCE\CLAUDE.md` | `E:\Dev\Ronny\NDInsight\SINTRAN\OS\21-SEMAPHORES-RECOVERED-CODE.md` (recovered L07 machine code) |
| XMSG message system | `SYMBOLS\L07\XMSG-SYMBOL-LIST.SYMB.TXT` only | `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\` (protocol docs + C# library) |
| Batch processing, system utilities/commands, full terminal handlers, non-HDLC networking | - | segment carve per subsystem under `...\re\segments-ref\`; otherwise reference manuals |

**Questions this tree therefore CANNOT answer** (do not re-search it for these):
device-0 (command buffer) read/exhaustion semantics of MON 1B; the RSIO/SETOL body
logic; MCTAB contents; the DESCF worker's mutation; file-system byte I/O; anything
about the command buffer's declared size.

---

## 4. Verified structures worth having in one place

All L07 addresses from `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\SYMBOL-1-LIST.SYMB.TXT`
unless noted. All octal. Full evidence quotes in the companion report.

**Command-processor per-background-process area** (system segment, base BGSYS=144000B):

| Addr | Symbol | Meaning | Label |
|---|---|---|---|
| 144031B | TTNO | terminal number (interactive InputDev = this content) | VERIFIED (GDEVTY code) |
| 144032B | TTIFI(ELD) | address of terminal input datafield | VERIFIED |
| 144033B | CPNT | command-buffer byte pointer; **0 = start of buffer** | VERIFIED (`RP-P2-MONCALLS.NPL` lines 2255-2257; MON60 fn 142) |
| 144034B | OPNT | output pointer (by symmetry) | INFERRED |
| 144035B | CSTRI(N) | command string; inline buffer vs pointer-to-text UNKNOWN | VERIFIED symbol, structure UNKNOWN |
| 144036B | OSTRI(N) | output string (by symmetry) | INFERRED |

**Command-string convention**: terminated by the character constant `##'` =
apostrophe = 47B (0x27); **no byte-count field found anywhere** - consumption is
positional (CPNT vs terminator). VERIFIED from two independent scan loops
(`RP-P2-MONCALLS.NPL` lines 2246-2252 and 2704-2711).

**Background-field B-relative displacements** (the carved RSIO frame offsets):

| Access | Value | Symbol (line) | Meaning |
|---|---|---|---|
| `,B -103` | 177675B | XBCHF (4534) | BCHFLAG; 0 = interactive, nonzero = batch/mode |
| `,B -147` | 177631B | XTTNO (4743) | TTNO |
| `,B -146` | 177632B | XTTIF (4535) | TTIFIELD |

**Terminal-datafield offsets**: RIFIL=26B (line 2769, batch command input file),
DFOPP=12B (line 592, opposite/output datafield pointer), ROFIL=23B (line 2770,
output file), BSTAT=22B (line 3139), FLAGB=42B (line 1243, escape/break event bits
incl. 5IBRK=10B), **DFLAG=177766B (-12B)** (line 454) with **bit 5IESC=15B**
(line 725): set = escape DISABLED. All VERIFIED; the DESCF worker applying it is
CARVER-REPORTED (dispatch proven, mutation still inferred, not pollable).

**CBUF=170207B - carve mislabel warning (VERIFIED)**: `CBUF` is
`INTEGER CBUF % ADDRESS OF CURRENT I/O BUFFER`
(`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\PH-P2-START-BASE.NPL` line 24) - a
**boot-time scratch pointer used while carving up the I/O buffer area**, NOT a
command-buffer data area. The segment-carve label saying otherwise is wrong and is
being corrected by the carver agent (do not fix it independently - single owner).

**MON 1B shape (VERIFIED for s3vs-4)**: `M1` is a two-word stub
(`MP-P2-2.NPL` line 231: `M1: "INBT"; GO IOB14`) - zero device dispatch at level 14;
all real work is in the level-4 INBT worker (missing here, see section 3).
