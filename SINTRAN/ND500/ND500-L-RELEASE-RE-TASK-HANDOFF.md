# RE TASK HANDOFF: ND-500 support software of the SINTRAN III L release

**Date:** 2026-07-08
**Audience:** an LLM (or human) reverse-engineering session with access to this repo,
the F:\ND archive, RetroCore/RetroCommander and Ghidra.
**Mission:** reverse engineer the binaries that the SINTRAN III L installation copies
in for ND-500 support, in order to (a) recover the protocol constants that are still
UNVERIFIED in the evidence dossier, (b) obtain the first real ND-500 machine code for
analysis, and (c) validate the bus-interface spec against shipped software.

---

## 0. Ground rules (non-negotiable)

1. NEVER present speculation as fact. Every claim gets a citation: file + byte
   offset / disassembly address / doc line. If you cannot verify it, tag it
   UNVERIFIED or ASSUMPTION.
2. Read these BEFORE touching a binary:
   - [ND500-BUS-INTERFACE-REFERENCE.md](ND500-BUS-INTERFACE-REFERENCE.md) - the
     verified spec for the 3022/5015 interface and the message protocol.
   - [ND500-EVIDENCE-AND-CONTRADICTIONS.md](ND500-EVIDENCE-AND-CONTRADICTIONS.md) -
     what is verified, what is open (section 3 lists the open items this task
     should close).
   - [../File-Formats/PROG-FILE-FORMAT.md](../File-Formats/PROG-FILE-FORMAT.md) and
     [../File-Formats/BRF-FILE-FORMAT.md](../File-Formats/BRF-FILE-FORMAT.md).
3. ASCII only in produced documents. Octal numbers unless stated otherwise.
4. Findings that contradict the spec/dossier are VALUABLE - report them explicitly
   with both sides quoted, do not silently reconcile.

---

## 1. What the L release installs for ND-500 (verified from the release doc)

Source: `../Release-Documentation/ND-860230-6-EN Sintran III - Release Information -
L-Version.md` (line numbers cited).

### 1.1 The ND-500/5000 System Package, product ND-211305 (ver. B)

One product carries all ND-500 support software (doc lines 2725-2731): the
ND-500/5000 Background Monitor (version J04 or later), the ND-500/5000 Swapper
(version K) and the ND-500 Place Library.

Installation copy commands (doc lines 726-741), from the ND-211305 diskette:

```
@DELETE-FILE ND-500-MON:PROG
@COPY-FILE "ND-500-MON-J:PROG" (211305:FL)ND-500-MON-J:PROG

@DELETE-FILE SWAPPER:PSEG
@DELETE-FILE SWAPPER:DSEG
@COPY-FILE "SWAPPER-K:PSEG" (211305:F-U)SWAPPER-K:PSEG
@COPY-FILE "SWAPPER-K:DSEG" (211305:F-U)SWAPPER-K:DSEG
```

### 1.2 ND-5000 microcode (ND-5000 systems only)

Doc lines 750-775: per-model diskettes; copy `MIC-5xxx-2-500:DATA` (xxx = 200, 400,
500, 700, 800) to `CONTROL-STORE:DATA`; the ND-5900 uses `CONTROL-1-STORE:DATA`,
`CONTROL-2-STORE:DATA`, etc. Required microprogram versions (doc lines 357-366):

| ND prod.no. | System type | Microprogram version |
|---|---|---|
| 210786 D | ND-550/560/570 | 15211 |
| 210787 D | ND-530 | 15311 |
| 210701 F | ND-580 | 15111 |
| 211272 C | ND-5200 | 11529 |
| 211273 C | ND-5400 | 11629 |
| 211274 C | ND-5500 | 11729 |
| 211275 C | ND-5700 | 11829 |
| 211276 C | ND-5800 | 11929 |

### 1.3 The SINTRAN-resident side (already inside SINTRAN, not copied by the user)

- "Just as in the K-version, the ND-500/5000 System Monitor is installed as part of
  SINTRAN" (doc line 485).
- Segment table entries (doc lines 2515-2549): segment 20 S3SDT5 = ND-500 standard
  domains, segment 21 S3NM5S = ND-500 name tables, segment 30 S3SMS5 = ND-500
  System Monitor (40000:177777), segment 62 = save copy of it.
- "ND-500 Monitor 1057B 60B 40000B" in the fixed-segment table (doc line 2372).
- RT program 5SWAP: "Performs ABSTR in ND-100 for the ND-500/5000 Swapper" (doc
  line 2615) - matches the verified 5SWRT analysis (dossier 2.7).
- Hardware requirement note (doc line 340): "ND-500 model II w/ND-100 Octobus Line
  Driver. (ND-324133, level D)" for NUCLEUS - i.e. PCB 3096 with OBCON revision
  36600D (dossier 4.11 catalogue table).

### 1.4 Distribution note

Doc lines 563-570: the L install media list includes "ND-500/5000 System Package
for version L" and "ND-5000 microcode (ND-5000 systems, only)"; the K-version
equivalent already shipped "ND-500/5000 System Package ver.B".

---

## 2. Artifact inventory (checked on F:\ND, 2026-07-08)

| Artifact | Location | Status |
|---|---|---|
| **ND-211305 package floppy, rev B02** | `F:\ND\SINTRAN-K05-XMSG-2026\FLOPPY\211305B02-XX-01D.img` (1310720 bytes) | PRESENT - PRIMARY TARGET. Contents not yet listed. |
| SINTRAN L distribution floppies | `F:\ND\SINTRAN-L - 2026\FLOPPY\VSXL1/2/3.IMG`, `211024E02-XX-01D.IMG` | Present. VSXL3 file list (FILE-INFO) shows NO ND-500 monitor files - only N500-SYMBOLS:SYMB etc. |
| Installed L system disk | `F:\ND\SINTRAN-L - 2026\HDD\BIGDISK0-L.IMG` | Present, but the ND-500 package was NEVER installed on it (FILE-INFO lists only N500-SYMBOLS:SYMB). |
| L07 symbol tables | `F:\ND\SINTRAN-L - 2026\L07\` and `../NPL-SOURCE/SYMBOLS/L07/` | Present - use for constant cross-referencing. |
| ND-5000 microcode diskettes (MIC-5xxx) | NOT FOUND on F:\ND | Missing artifact - note in findings if located elsewhere. |
| BPUN format examples | `F:\ND\SINTRAN-K05-XMSG-2026\FLOPPY\xmsg\xmsg-kern-cx-l03.bpun` etc.; `(SYSTEM)DMAC-1915G:BPUN` on VSXL3 | Same-era format references. |

First mechanical step of the RE task: mount/parse `211305B02-XX-01D.img`, produce
the full file list (name, type, size, dates) the same way the FILE-INFO TXT dumps
were made, and extract `ND-500-MON-J:PROG`, `SWAPPER-K:PSEG`, `SWAPPER-K:DSEG` and
the Place Library files.

---

## 3. File formats you will meet

| Extension | What it is | Format reference |
|---|---|---|
| :PROG | SINTRAN background-program executable (ND-100 code) | `../File-Formats/PROG-FILE-FORMAT.md` |
| :BPUN | Bootable punch image (ND-100) | nd100-asm knowledge; examples above |
| :PSEG / :DSEG | ND-500 domain program/data segment images - **ND-500 machine code, byte-addressed** | ND-05.009.4 EN ND-500 Reference Manual (instruction set); `../../Reference-Manuals/500/ND500-adressing-modes.md`; ND-60.136.04A (domain/segment placement) |
| :DATA (CONTROL-STORE) | ND-500 control store image: 144-bit words stored as 9 x 16-bit parts (18 bytes per word), full store 20000B words | ND-60.136.04A section 8.10.6.3; spec section 8.2 |

WARNING for the ND-100 binaries: SINTRAN :PROG files are two-bank images; use the
existing PROG format doc, do not guess the layout.

WARNING for :PSEG/:DSEG: there is NO ND-500 disassembler in the toolchain yet. The
ND-500 instruction set is documented in ND-05.009.4. Building even a minimal
decoder for the common opcodes is a legitimate sub-task; do not eyeball hex and
call it disassembly.

---

## 4. Priority questions (each closes a known gap)

**Q1 - Package contents.** Full file list of 211305B02-XX-01D.img. Deliverable:
a FILE-INFO-style listing + extracted files.

**Q2 - The UNVERIFIED protocol constants (highest value).** The dossier (section 3
items 4 and 6) lists constants whose SEMANTICS are verified but whose VALUES are
absent from the NPL sources: message status codes MSGN500, WAITING, ANSWER,
5ERANSWER (manual hint: 0=free, 1=to-ND500, 2=in-process, 3=answer, 4=error);
MICFU codes 3MONCO, 3START, 3TRACO, 3WMONCO, 3FITRNSF, 3RPREG, 3RMICV, 3SWMESS;
stop reasons MOCALL, TRAPCODE, 5FMOCALL (docs claim 1/2/3, unconfirmed); DUMMESS;
swapper states PSWWAIT, PSW1WAIT, SWPWAIT, SWPPING. ND-500-MON-J:PROG must contain
comparisons/stores of these values when it builds and reads messages. Find them,
cite disassembly addresses, cross-check against the L07 symbol tables
(`N500-SYMBOLS.SYMB.TXT`).

**Q3 - Monitor-to-driver interface.** How does the background monitor (user-side
:PROG) reach the resident ND-500 driver - which MON calls, with which parameter
blocks? (The resident dispatch MCHANDLE/DECOMESS side is documented from NPL; the
user side is not.)

**Q4 - The swapper segments.** SWAPPER-K:PSEG/DSEG is the first real ND-500-side
code available. Identify: entry point convention, how it reads its message
(validates the message field offsets, dossier 2.6.2), what MICFU/status values it
writes (feeds Q2), and any direct interface interaction.

**Q5 - Segment capabilities (dossier C9).** The Place Library implements
PLACE-DOMAIN. Recover the segment-capability word layout (bit positions of W/P/S,
width of the physical segment number) - this resolves the open contradiction C9
(11-bit vs 12-bit segment field, W/P positions).

**Q6 - IOX usage.** Does ND-500-MON:PROG ever execute IOX against the 3022
(HDEV+offset patterns), or is ALL register access confined to resident SINTRAN?
The spec (section 3.3) predicts the latter; confirm or refute.

**Q7 - Microcode image (if a MIC-5xxx file is ever located).** Validate the
9-parts-per-word structure and the version word (compare against the microprogram
version table in section 1.2).

---

## 5. Tooling

- RetroCore/RetroCommander (`F:\ND\SINTRAN-L - 2026\RetroCore.exe`) mounts the
  floppy/disk images; the FILE-INFO TXT dumps show the expected listing format.
- Ghidra with the ND-100 loader (see the nd100-ghidra workflow and
  `../File-Formats/BRF-GHIDRA-LOADER-HANDOFF.md` for the loader-project pattern).
- Symbol tables: `../NPL-SOURCE/SYMBOLS/L07/` (match the L-release exactly).
- The verified NPL driver sources in `../NPL-SOURCE/NPL/` for the resident side.

## 6. Additional guidance (hard-won, read carefully)

### 6.1 Poisoned priors - sources you must NOT trust

- `../Emulator/ND500-QUICK-REFERENCE.md`, `../Emulator/DETAILED-TAG-MECHANISM-EXPLANATION.md`
  and the C# emulator `NDBusND500IF.cs` (RetroCore repo) all implement a FABRICATED
  "high-level TAG code" protocol (codes 8/9/16 = MonitorCall/PageFault/
  OperationComplete). It does not exist in hardware or in SINTRAN (spec section
  10.3, dossier C6). If your model has seen these files, actively distrust that
  prior. The retired docs in `old/` are wrong in documented ways (`old/README.md`).
- The OCR'd manuals corrupt register mnemonics (RTAG -> "RFAG/RMAG", LCON ->
  "LOCN/ICON", DIEN -> "DlEN"). Trust POSITIONS and octal values over names;
  ND-30.013.02 is the cleaner copy of the section-3 text that NEC-01 duplicates.

### 6.2 Dynamic-analysis shortcut for Q2 (recommended - do this FIRST)

You do not need working ND-500 emulation to recover the message constants.
SINTRAN's presence test (CH5CPUPRESENT, spec section 8.1) only requires that an
IOX read of RSTA5 does not bus-fault - the EXISTING RetroCore 3022 device stub
satisfies that. Therefore:

1. Boot the SINTRAN L system in RetroCore with the 3022 device configured
   (thumbwheel 0, IOX base 60).
2. SINTRAN flags the CPU OLD500 + 5ALIVE and runs XMSINIT, which WRITES the real
   constants into the mailbox area: the histogram message HIMESS gets MICFU :=
   3RPREG, the watchdog message WATCHDOG gets SENDE := -1 and MICFU := 3RMICV
   (RP-P2-N500.NPL:801-822), and watchdog restarts write N5STA := MSGN500
   (MP-P2-N500.NPL:308).
3. Locate the mailbox bank (5MBBANK, derived from 5FPMAILBOX - or find the
   WATCHDOG message by its unique SENDE = -1 anchor at offset 3) and dump the
   area with the DAP/MCP debugger.
4. Read the numeric values of 3RPREG, 3RMICV, MSGN500 (and DUMMESS from the queue)
   directly from memory. Field offsets to decode a message: dossier 2.6.2
   (LINK=0, LINK2=1, N5STA=2, SENDE=3, X5CPU=4, X5ACT=5, MICFU=6, ...).
5. Cross-validate every value found this way against the static disassembly of
   ND-500-MON-J:PROG (Q2) - two independent sources per constant is the bar.

CAUTION: consider timing - XMSINIT runs during startup; take the memory dump after
the system reaches idle. If detection does NOT flag the CPU present, check the
stub answers IOX base+2 (RSTA5) without error; the trap test is armed via
IIE bit 200.

### 6.3 Static-analysis tips for ND-500-MON-J:PROG

- Anchor the disassembly on COMMAND STRINGS first: the Background Monitor contains
  its command names (e.g. "PLACE-DOMAIN", "RESTART-PROCESS" - reintroduced in L,
  release doc line 2766, and the commands listed in ND-60.136.04A). String tables
  lead to dispatch tables lead to handlers.
- Map MON instructions to the SINTRAN monitor-call numbers using
  `../../Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md`; the
  ND-500-specific calls are in the 500B-523B range (see
  [ND500-MONITOR-CALL-MECHANISM.md](ND500-MONITOR-CALL-MECHANISM.md)).
- The message status word carries FLAGS in the high bits: the driver preserves
  them with `A/\160000\/MSGN500` (MP:992). Expect the status CODES to be small
  integers in the low bits, and remember "status > 100" has its own meaning
  (restart) in CHN5STATUS.
- Byte order: ND-100 files are 16-bit word streams; the ND-500 is big-endian and
  byte-addressed. When carving :PSEG bytes out of a container, verify pairing
  against a known instruction sequence before trusting any decode.

### 6.4 Contradiction protocol

If you find ANYTHING that contradicts
[ND500-BUS-INTERFACE-REFERENCE.md](ND500-BUS-INTERFACE-REFERENCE.md) - for
example TAG-register IOX access from the background monitor, or status values
that do not match the manual's 0-4 scheme - do not reconcile silently and do not
assume you misread. Report it as a dedicated finding with both sides quoted
(your disassembly evidence vs the spec section). The spec was validated against
NPL and manuals, but shipped binaries outrank derived documents.

## 7. Expected deliverables

1. `ND500-L-PACKAGE-CONTENTS.md` - the 211305 floppy inventory (Q1).
2. `ND500-MON-RE-FINDINGS.md` - constants and monitor-call interface (Q2, Q3, Q6),
   every value cited to a disassembly address, cross-referenced to symbol tables.
3. `ND500-SWAPPER-SEGMENT-RE-FINDINGS.md` - Q4 (+ Q5 if the Place Library yields).
4. A patch list for [ND500-EVIDENCE-AND-CONTRADICTIONS.md](ND500-EVIDENCE-AND-CONTRADICTIONS.md):
   which open items (section 3) and verdicts (C9, C12 values) can be upgraded from
   UNVERIFIED, with the new evidence lines.
5. Explicit list of what could NOT be determined and why.

Do NOT edit the spec or dossier directly; deliver findings + the patch list for
review first.
