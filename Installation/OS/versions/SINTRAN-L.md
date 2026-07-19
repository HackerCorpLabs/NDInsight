# SINTRAN III Version L — Install Notes

> Status: SCAFFOLD. Verified facts only. Source: SINTRAN-L distribution archive.

## Verified facts
- Golden disk dump: `FILE-INFO\BIGDISK0-L.TXT` (PACK-ONE, 38400 pages).
- Three VSX distribution floppies: `VSXL1.TXT`, `VSXL2.TXT`, `VSXL3.TXT`.
- Has a dedicated startup script: `Admin\START-SINTRAN-MULTIUSER.TXT`.
- L07 symbol set includes `XMSG-SYMBOL-LIST.SYMB.TXT`.

## Source files
- `Admin\System initialisation.txt`, `Admin\START-SINTRAN-MULTIUSER.TXT`, `Admin\SINTRAN-COMMANDS-L.TXT`
- `FILE-INFO\BIGDISK0-L.TXT`, `VSXL1/2/3.TXT`

## Installing the ND-500/5000 Monitor and ND-5000 Microcode

> Applies only to ND-500/ND-5000 systems. Skip entirely for ND-100/ND-110-only installs.

### 1. Install the ND-500/5000 System Package (product ND-211305)

Source: `../../../SINTRAN/Release-Documentation/ND-860230-6-EN Sintran III - Release Information - L-Version.md`
(lines 710-746). Done after `@ENTER-DIRECTORY` on the main directory, logged in as user `SYSTEM`.

1. Insert the diskette for the ND-500/5000 System Package for version L (ND-211305) into
   FLOPPY-DISC-1 unit 0.
2. `@ENTER-DIRECTORY,,FLOPPY-DISC-1,0`
3. Replace the ND-500 Background Monitor:
   ```
   @DELETE-FILE ND-500-MON:PROG
   @COPY-FILE "ND-500-MON-J:PROG" (211305:FL)ND-500-MON-J:PROG
   ```
4. Replace the ND-500 Swapper:
   ```
   @DELETE-FILE SWAPPER:PSEG
   @DELETE-FILE SWAPPER:DSEG
   @COPY-FILE "SWAPPER-K:PSEG" (211305:F-U)SWAPPER-K:PSEG
   @COPY-FILE "SWAPPER-K:DSEG" (211305:F-U)SWAPPER-K:DSEG
   ```
5. `@RELEASE-DIRECTORY 211305`, then remove the floppy.

Version constraints (same doc, lines 3547-3551): only **ND-500/5000 Background Monitor version J
or later** (ND-210333) and **ND-500/5000 Swapper version K** (ND-211034) may run under SINTRAN III
version L. As of L, both ship combined as one product: the ND-500/5000 System Package (ND-211305),
which also includes the ND-500 Place-Library.

### 2. ND-5000 systems only: install the microcode

Same source doc, lines 748-779.

1. Insert the ND-5000 microprogram diskette matching your model (ND-5200/5400/5500/5700/5800)
   into FLOPPY-DISC-1 unit 0.
2. `@ENTER-DIRECTORY,,FLOPPY-DISC-1,0`
3. Copy the microcode to disk:
   - ND-5200/5400/5500/5700/5800:
     `@COPY-FILE CONTROL-STORE:DATA (211:)MIC-5xxx-2-500:DATA` (substitute `xxx` = 200/400/500/700/800)
   - ND-5900: `@COPY-FILE CONTROL-1-STORE:DATA (211:)MIC-5800-2-500:DATA`, repeated to
     `CONTROL-2-STORE:DATA` etc. depending on the ND-5900 model.
4. `@RELEASE-DIRECTORY 211`, then remove the floppy.

### 3. Run the cold-start mode file

Whether or not step 2 applied:
```
@MODE HENT-MODE:MODE.,
```

### 4. Starting the Monitor — RESOLVED by live test (2026-07-09)

> **CORRECTION (superseded by verified fact below):** an earlier version of this doc
> guessed the Monitor was loaded via `RT-LOADER READ-BINARY ND500-MONITOR 62`. That
> filename came from `../research/HDD-IMAGE-FINDINGS.md` section 1's disk-image table
> (`WD0-M.IMG`, `HD0.IMG`), both explicitly marked **M-version** images — not L. It was
> wrongly presented here as applying to L. Confirmed wrong by live test on an L system:
> `NO SUCH FILE NAME`.

**Verified fact: `ND-500-MON:PROG` needs NO RT-LOADER/HENT-MODE load step at all.** It
is an ordinary, self-contained SINTRAN background program. It is started by simply
typing its name at the terminal, exactly like a compiler subsystem — confirmed both by
the manual and by a live run:

- Manual: `../../../Reference-Manuals/ND-60.136.04A ND-500 Loader Monitor.md` lines
  729-822 — `@ND-500` or the full name `@ND-500-MONITOR` starts the Monitor; most
  installations abbreviate it to `ND-500`.
- Live test (2026-07-09): copied `ND-500-MON-J04:PROG` from floppy `211305B02-XX-01D`
  to disk and ran `@ND-500`. Output:
  ```
  ND-500/5000 MONITOR  Version J04 88. 6.16 /     6. 6
  ND-500(0) error:      No ND-500(0) CPU found
  ```
  This confirms the Monitor loads and runs correctly as a plain `:PROG` — no RT-LOADER
  step, no HENT-MODE entry, no segment load needed for the `:PROG` itself.

**"No ND-500(0) CPU found" is a separate, later-stage issue — not a load/install
problem.** It is SINTRAN's boot-time hardware-presence flag never getting set. Per
`../../../SINTRAN/ND500/ND500-BUS-INTERFACE-REFERENCE.md` section 8.1: at SINTRAN
startup, the routine `CH5CPUPRESENT` (`PH-P2-OPPSTART.NPL:3895-3945`) probes each
generated ND-500 CPU datafield — an IOX read of RSTA5 via the 3022/5015 interface (or,
on fault, an Octobus/Samson probe) — and only if that succeeds does it flag
`CPUAVAILABLE`/set `5ALIVE`. The Background Monitor's own runtime check reads that flag
and refuses to proceed if it was never set. Fix path (UNVERIFIED which applies to your
setup): either the physical 3022/5015 interface card isn't answering, or — if running
under an emulator — the ND-500 CPU device isn't configured/attached (the RE task
handoff, `../../../SINTRAN/ND500/ND500-L-RELEASE-RE-TASK-HANDOFF.md` section 6.2, notes
RetroCore needs "the 3022 device configured (thumbwheel 0, IOX base 60)" for this
presence test to pass).

**RESOLVED (2026-07-09): confirmed by live test with a bus-interface emulation that
answers the `CH5CPUPRESENT` probe.** With enough of the 3022/5015 bus interface emulated
to make SINTRAN's presence probe succeed, `@ND-500` now reaches the interactive Monitor
prompt cleanly:

```
@ND-500
ND-500/5000 MONITOR  Version J04 88. 6.16 / 88. 8.17
N500: help
Command:
[... full command list, see below ...]
N500:
```

Note the two version-date fields changed from the earlier failed run
(`88. 6.16 /     6. 6`, CPU not found) to (`88. 6.16 / 88. 8.17`, CPU found) — the first
date is the Monitor build date (fixed), the second is read from the ND-500 CPU/microcode
side and is only populated once a CPU is actually detected. UNVERIFIED: exact field
semantics (e.g. whether the second date is a microcode build date) — not confirmed from
any manual read so far, only inferred from this before/after comparison.

#### Live-verified `N500:` command reference (Version J04, HELP output, 2026-07-09)

The full interactive command set the Monitor advertises via `HELP` — this is a direct
transcript, useful as a live cross-check against
`../../../Reference-Manuals/ND-60.136.04A ND-500 Loader Monitor.md`'s alphabetic command
list (manual lines 8177-8210+). Spot-check: `RESTART-PROCESS` is present, consistent
with the release doc's claim (line 2764-2766) that it was reintroduced in L after being
removed in the J-version Monitor — the version string here is J04, so this confirms the
release-doc claim against a real J04 binary's own advertised command set.

```
GO  <Address: >
CONTINUE
RUN
HELP  <Command: >
OUTPUT-FILE  <File name: >
CC
EXIT
RECOVER-DOMAIN  <Domain name: >
LOOK-AT-PROGRAM  <Address: >,[<Domain name>]
LOOK-AT-DATA  <Address: >,[<Domain name>]
LOOK-AT-STACK
LOOK-AT-RELATIVE  <Relative to: >
LOOK-AT-REGISTER  [<Register name>]
LOOK-AT-FILE  <Address: >,<File name: >
INSPECT-DUMP  <File name: >
RESET-INSPECT-DUMP
MAIN-FORMAT  <Format: >
EXTRA-FORMAT  <Format: >
TRACE  <Address: >,[<Datatype: >]
RESET-TRACE
GUARD  <Address: >,<Datatype: >,[<Lower limit>],[<Upper limit>]
RESET-GUARD
BRANCH-TRACE  <Start address: >,<Min trace: >,<Max trace: >
RESET-BRANCH-TRACE
CALL-TRACE  <Start address: >,<Min trace: >,<Max trace: >
RESET-CALL-TRACE
BREAK  <Address: >,[<Count>],[<Command>]
TEMPORARY-BREAK  <Address: >,[<Count>],[<Command>]
RESET-LAST-BREAK
EXHIBIT-ADDRESS  <Address in program: >,<Address of variable: >,<Datatype: >
RESET-BREAKS  [<Break number>]
DEBUG-STATUS
DEBUGGER  [<Domain name>]
SPECIAL-DEBUGGER  <File name: >,<Segment number: >,[<Domain name>]
STEP  [<Step start address, [Execution start address], [Count]>]
PLACE-DOMAIN  <Domain name: >
DEBUG-PLACE  <Domain name: >
RESIDENT-PLACE  <Domain name: >
LOCAL-TRAP-ENABLE  <Label: >,<Trap condition: >
SYSTEM-TRAP-ENABLE  <Trap condition: >
LOCAL-TRAP-DISABLE  <Trap condition: >
SYSTEM-TRAP-DISABLE  <Trap condition: >
RESET-DEBUG
STATUS
ENABLED-TRAPS
RESTART-PROCESS  <Process name: >
OPEN-FILE  <File name: >,<Connect file number: >,<Access: >
CLOSE-FILE  <Connect number: >
SET-BLOCK-SIZE  <Connect number: >,<Size(in bytes): >
LIST-OPEN-FILES
SET-HISTOGRAM  <Start address: >,<Max address: >,<Number of intervals: >
PRINT-HISTOGRAM
STOP-HISTOGRAM
START-HISTOGRAM
RELEASE-HISTOGRAM
TIME-USED
WHO-IS-ON
DEFINE-MACRO  <Macro name: >
EXECUTE-MACRO  <Macro name: >,[<Parameters: >]
ERASE-MACRO  <Macro names: >
LIST-MACRO  <Macro names: >
DUMP-MACRO  <Macro name: >
RESUME-MACRO
ABORT-BATCH-ON-ERROR  <On/Off: >
AUTOMATIC-ERROR-MESSAGE
RESET-AUTOMATIC-ERROR-MESSAGE
SET-MEMORY-CONTENTS  <From address: >,<Up to address: >,<Value: >,<Datatype: >
SET-FLAG  <Process number: >,<Value: >
GET-FLAG  <Process number: >
DEFINE-MEMORY-CONFIGURATION  <ND-100 page number for ND-500 physical address 0: >
MEMORY-CONFIGURATION
VERSION  [<>],[<>]
MASTER-CLEAR
RUN-SELFTEST
RESET-CPU
INIT-TRACER  <Cycle: >,<Mode: >,<Trigger: >,<CSA: >,<Clear trace memory(yes/no): >
ARM-TRACER
DISARM-TRACER
CLEAR-TRACE-ADDRESS
CLEAR-TRACE-MEMORY
DUMP-TRACE-MEMORY
EXAMINE-TRACE
READ-TRACE-FILE  <File name: >
WRITE-TRACE-FILE  <File name: >
LOAD-CONTROL-STORE  <File name: >,<Start address: >,<Number of words: >
COMPARE-CONTROL-STORE  <File name: >,<Start address: >,<Number of words: >,<Max number of faults: >
LOOK-AT-CONTROL-STORE  <Address: >
LOOK-AT-RESIDENT-MEMORY  <Address: >
LOOK-AT-HARDWARE  <Register name: >
MICRO-START  <Start address: >
MICRO-STOP
SET-PRIORITY  <ND-500 process number: >,<ND-500 priority: >
START-PROCESS-LOG-ALL
START-PROCESS-LOG-ONE  <Process number: >
PROCESS-LOG-ALL  <First process: >,<Interval: >
PROCESS-LOG-ONE  <Process number: >,<Interval: >
PRINT-PROCESS-LOG  <First process: >
SWAPPING-LOG  <Interval: >
RELEASE-LOG-BUFFER
ATTACH-PROCESS  <Process number: >
LOOK-AT-PHYSICAL-SEGMENT  <Address: >,<Segment number: >
LOOK-AT-SRF  <SRF-address: >
SET-SEGMENT-LIMITS  <SEGMENT NUMBER: >,<Type(P or D): >,<Min number of pages: >,<Max number of pages:>,[<Process number.>]
FIX-SEGMENT-SCATTERED  <Segment number: >,<Type(P or D): >,<Low address: >,<High address: >
FIX-SEGMENT-CONTIGUOUS  <Segment number: >,<Type(P or D): >,<Low address: >,<High address: >
FIX-SEGMENT-ABSOLUTE  <Segment number: >,<Type(P or D): >,<Low address: >,<High address: >,<Physical page: >
UNFIX-SEGMENT  <Segment number: >,<Type (P or D): >
LIST-SYSTEM-PARAMETERS
SET-SYSTEM-PARAMETERS
VALUE-ENTRIES  <Entries: >
START-MONCALL-LOG  <Own/All: >
PRINT-MONCALL-LOG
STOP-MONCALL-LOG
DEFINE-STANDARD-DOMAIN  <Standard domain name: >,<Domain name: >
DELETE-STANDARD-DOMAIN  <Standard domain name: >
LIST-STANDARD-DOMAINS
LIST-EXECUTION-QUEUE  < Interval: >
LIST-TIME-QUEUE  < Interval: >
DEFINE-SWAP-FILE  <File name: >
DELETE-SWAP-FILE  <File name: >
SET-ND-500-AVAILABLE
SET-ND-500-UNAVAILABLE  [<>]
STOP-ND-500
STOP-ND-500
LOGOUT-PROCESS  <Process number: >
ABORT-PROCESS  <Process number: >
LIST-ACTIVE-PROCESSES
LIST-DOMAIN  <Domain name: >
DOMAIN-STATUS  <Domain name: >
LIST-STATUS  <Domain name: >
SET-PROCESS-NAME  <Process name: >
LIST-PROCESS-TABLE-ENTRY  <Process number: >
LIST-ACTIVE-SEGMENTS  <Process number: >
PROCESS-STATUS
LIST-SEGMENT-TABLE-ENTRY  <Physical segment number: >
LIST-SWAP-FILE-INFO  <Swap file number: >
DEBUG-SWAPPER  <On/Off: >
DUMP-SWAPPER  <File name: >
DUMP-PHYSICAL-SEGMENT  <File name: >,<Ph segno: >
LIST-TABLE  <Table name: >,[<Index: >]
LOAD-SWAPPER  <File name: >
START-SWAPPER
GIVE-N500-PAGES  <Number of pages: >
TAKE-N500-PAGES  <Number of pages: >
SET-CPU-STATUS  <CPU Number: >,<Image: >,<Save: >,<Status: >
CHANGE-CPU  <CPU Number: >
SET-PHYSICAL-SEGMENT-ADDRESS  <Segment number: >,<Physical page number: >
CACHE-MODE  <Program cache mode: >,<Data cache mode: >
REMOVE-FROM-TIME-SLICE  <Process number: >
INSERT-IN-TIME-SLICE  <Process number: >,<Time slice class: >
```

### 4b. Normal boot sequence — verified: mostly AUTOMATIC, not manual

> Corrects the earlier "likely next steps" guess in this section (LOAD-CONTROL-STORE ->
> LOAD-SWAPPER -> START-SWAPPER -> SET-ND-500-AVAILABLE as a manual sequence). That
> guess is WRONG per the manual text below — do not follow it.

**Verified from `../../../Reference-Manuals/ND-60.136.04A ND-500 Loader Monitor.md`:**

- **`STOP-ND-500` (section 8.10.3, manual line 5359-5361):** *"When a user attempts to
  start an ND-500 process after this command has been executed, the microcode will
  automatically be reloaded, the swapper process placed in memory and started ('warm
  start' of ND-500)."* — i.e. control-store load + swapper load/start are **automatic**,
  triggered by the first process start, not operator-issued commands.
- **`LOAD-SWAPPER` (section 8.10.10.4, manual line 5759):** *"Normally, this is done
  automatically when the first ND-500 process is initiated by the monitor, but this
  command may be useful to load a new copy if there are reasons to believe that the
  existing one is corrupted, or to load a non-standard version of the swapper process."*
  — confirms `LOAD-SWAPPER`/`START-SWAPPER` are **manual overrides for the abnormal
  case**, not normal-boot commands. Default source file if invoked manually:
  `(SYSTEM)SWAPPER` (`:PSEG`/`:DSEG` — file type may not be specified).
- **`LOAD-CONTROL-STORE` (section 8.10.6.3, manual line 5471-5477):** default file
  `CONTROL-STORE:DATA` (the file you copy the `MIC-5xxx-2-500:DATA` microcode to per
  section 2 above), default start address 0, default word count = entire control
  store (20000B words). Same pattern: this is the command SINTRAN issues automatically;
  you'd only run it by hand to reload/replace microcode.
- **`DEFINE-MEMORY-CONFIGURATION` (section 8.10.4/8.10.4.1, manual line 5373-5385) — the
  one piece that genuinely IS a required manual step:** *"The ND-500 system has itself
  limited capability to investigate its own memory configuration. Therefore the memory
  configuration must be defined by the command DEFINE-MEMORY-CONFIGURATION."* Parameter:
  `<ND-100 page number for ND-500 physical address 0>` — i.e. the offset between ND-500
  and ND-100 physical addressing for the same shared-memory cell. Persists across a warm
  start; "local ND-500 memory is not legal in the ND-500 multiuser Monitor" (line 5375).
  **This is the command most likely relevant to your emulator setup** — if your bus
  emulation doesn't already answer the presence probe with the right memory offsets, the
  Monitor will not know where shared/multiport memory lives without this.
- **`MASTER-CLEAR` (section 8.10.13, manual line 5828-5832):** hardware reset only —
  *"Brings the ND-500 out of any hang-up state... equivalent to pressing the MCL button
  on the ND-500 front panel... used before a complete restart... contents of registers
  are unpredictable."* Not a routine boot step.
- **`RUN-SELFTEST`:** present in your live J04 `HELP` output but **not documented
  anywhere in `ND-60.136.04A`** — no manual entry found despite a full-text search. Note
  it as UNVERIFIED/undocumented; I don't know its parameters or effect.
- **`SET-ND-500-AVAILABLE`/`SET-ND-500-UNAVAILABLE`** (section 8.10.1-8.10.2): control
  whether *other users* may log in — access control, not initialization.

**Practical implication for your emulator:** since you already got past `CH5CPUPRESENT`
(section 4 above) and reached the interactive `N500:` prompt, the "normal boot stuff"
for a real system — microcode load and swapper start — should already trigger
automatically the moment you start (or place/recover) a domain. If that fails, the
manual points at `DEFINE-MEMORY-CONFIGURATION` as the thing your emulator setup most
likely still needs to supply correctly, since the ND-500 side cannot self-discover it.

### 4d. `DEFINE-MEMORY-CONFIGURATION` — what it does, and what value to use

**What it does (verified, manual section 8.10.4/8.10.4.1, lines 5377-5409, and kernel
source):**

1. You give one parameter: `<ND-100 page number for ND-500 physical address 0>` — the
   offset between ND-500 physical addressing and ND-100 page addressing for the same
   shared-memory cell.
2. The Monitor then asks a set of **subcommands per memory part** (line 5395-5407):
   size in pages, whether ND-100 has access, whether ND-500 has access as Program,
   whether ND-500 has access as Data, and whether this is the last part. Default
   answer is "access for both CPUs, both P and D for ND-500."
3. Kernel-side, this is **`N500M` function `40B` (`DEFM`)** — confirmed in the manual's
   N500M function table (`ND-60.136.04A`, chapter 10, line 6470: *"Define memory
   config. <start page> <no. of memory parts> <part array>"*). Per
   `../../../SINTRAN/OS/20-MPM-VS-LOCAL-MEMORY-DETECTION.md` section 8.2.3 (NPL-source
   traced, `5P-P2-MON60.NPL:587`), the value you give is written straight into the
   kernel variable **`ADRZERO`** — the ND-100 page number of the first page of MPM as
   seen from the ND-100 side. This **overrides** whatever boot-time auto-detection found.
4. Persistence: *"saved and will survive a normal restart (warm start)"* but *"lost"* on
   a `MACM )HENT / 22!` cold start (line 5385, 5409) — the manual recommends saving it as
   a permanent macro so it can be reissued after a cold start.

**Confirms your assumption: yes, this is specifically about MPM (multiport) memory.**
`ADRZERO` is exactly the ND-100 page number where the shared multiport-memory window
begins. Per the OS/20 doc's NPL trace (`PH-P2-OPPSTART.NPL:2498`, section 8.2.4):
SINTRAN also has an **automatic fallback** — at boot, if ND-500 is configured but
`ADRZERO` is still `-1` ("not configured"), it scans physical memory for the first page
that answers as an MPM page type and sets `ADRZERO` to that. `DEFINE-MEMORY-CONFIGURATION`
is the explicit, authoritative override of that guess — priority order (section 8.2.4):
(1) boot-time auto-detection, then (2) `MEMDEF`/`DEFINE-MEMORY-CONFIGURATION` overrides it
if given.

**What value the manual suggests: none — there is no manual-stated default or example
number.** `ND-60.136.04A` only describes the semantics (an ND-100 page number, site- and
hardware-dependent); it never states a recommended figure, because on real hardware this
depends entirely on where your MPM5/multiport card is physically strapped into the
ND-100 address space.

**UNVERIFIED / this repo's own derived guess, NOT an official ND value:** a separate
repo analysis doc, `../../../SINTRAN/OS/20-MPM-VS-LOCAL-MEMORY-DETECTION.md` section
10.5, self-labels a "recommended" `ADRZERO` of **2000B octal (1024 pages, i.e. ND-100
word address 20000000B)** as "typical," with 1000B as a safe minimum (above kernel
space) — but that section is explicitly flagged in the source doc as based on generic
SINTRAN layout patterns, not a cited manual value, and section 9.2 of the same doc
carries its own warning: *"The following offsets are ASSUMPTIONS based on symbol
analysis. They have NOT been verified against authoritative documentation."* Treat this
number as a plausible starting guess only, not a fact.

**What you actually need for your emulator:** since `ADRZERO` must equal wherever your
bus/MPM emulation exposes the shared-memory window in ND-100 page-number terms, the
correct value is a property of *your* bus-interface emulation, not a constant from the
manual — there is no single right answer without knowing what page range your emulated
3022/5015-or-MPM device already claims/responds to. If your emulation already answers
`CH5CPUPRESENT` at a specific IOX base/page, that's the reference point to derive this
from, not the 2000B guess above.

**CAUTION — fabrication found in a companion doc:** while researching this,
`../../../SINTRAN/ND500/ND500-INITIALIZATION-AND-EXECUTION-GUIDE.md` section 9 was
found to use commands `LIST-DOMAINS`, `EXAMINE-DOMAIN`, and `LINK-LIBRARY` in its
example transcripts. None of these three appear anywhere in `ND-60.136.04A` (full-text
searched), and none appear in the live J04 `HELP` output captured above. The real
equivalents per the verified command list are `LIST-DOMAIN <domain name>` (singular),
`DOMAIN-STATUS <domain name>`, and `LIST-STATUS <domain name>`; there is no
`LINK-LIBRARY` NLL command — real segment-linking commands are `LINK-SEGMENT`,
`OPEN-SEGMENT`/`CLOSE-SEGMENT`, `LOAD-SEGMENT` (all confirmed present in the manual).
That guide's section 9 example transcripts should not be trusted as-is; this has not yet
been reported/fixed in that file.

**Note on the resident kernel-side monitor** (distinct from the `:PROG` you ran): the
release doc states "the ND-500/5000 System Monitor is installed as part of SINTRAN"
(line 485) — this piece needs no separate install step at all. Verified segment
location: segment **30**, name **S3SMS5**, page range `40000:177777`, description
"ND-500 System Monitor segment" (release doc line 2523); segment **62** holds a save
copy of it (line 2549); fixed-segment table: `ND-500 Monitor | 1057B | 60B | 40000B`
(line 2372).

### 4c. `RUN-SELFTEST` -> FATAL "Illegal function code in MON 60" (2026-07-09)

**Live test result:**
```
N500: run-selftest

 *** FATAL SYSTEM ERROR ***
ND-500(0) error:      Illegal function code in MON 60
```

**Verified error identity.** This is the named kernel error `EILFUNC`, value **2011B**
(octal), defined at `../../../SINTRAN/NPL-SOURCE/NPL/5P-P2-MON60.NPL:49`:
`SYMBOL EILFUNC= 2011 % ILLEGAL FUNCTION CODE IN MON 60`. The manual documents the same
error verbatim: `../../../Reference-Manuals/ND-60.136.04A ND-500 Loader Monitor.md`
lines 7383-7385 — *"Illegal Function Code in MON 60 — Check the list of valid function
codes in chapter 10 against the location pointed to by the ND-100 A register. If this
monitor call was not used by the application program, the error message indicates an
error in the Monitor that should be reported to Norsk Data."*

**Root cause traced to the resident kernel's MON-60 dispatch table.** The `:PROG`
Monitor calls `MON 60B` with a function code in the A register (mechanism confirmed
independently in `../../../SINTRAN/ND500/ND500-MON-RE-FINDINGS.md` section 2). The
resident side dispatches through the `5IFUNC` array in `5P-P2-MON60.NPL:1319-1335`
("TABLE OF ENTRY POINTS TO GOTO BEFORE CALLING THE SYSTEM MONITOR / THE FUNCTION CODE
IS INDEX IN THE TABLE"). Reading that array by octal index, **this SINTRAN build
(s3vs-4 source, matching the L07/M06 symbol-table generation in this repo) explicitly
hard-wires these function codes to `ILLFUNC`** (i.e. deliberately rejects them,
regardless of what the calling `:PROG` sends): **146B, 147B, 151B, 152B, 153B, 174B,
175B, 176B**.

**Not yet determined (needs disassembly, not guessed):** which of these codes — or
another one — the Monitor's `RUN-SELFTEST` command actually sends. `ND500-MON-RE-
FINDINGS.md` section 2.3-2.4 already decoded the `:PROG`'s MON-60 stub-dispatch array
(`ram:ccc8-ce6d` in `ND-500-MON-J04:PROG`) but only named codes up to roughly 106B/143B
— `RUN-SELFTEST`'s specific code was not among those named. Finding it requires tracing
the `RUN-SELFTEST` command-string handler in the same binary to its `SAA <code>` stub,
the same technique already used for the other commands in that findings doc.

**A plausible but UNVERIFIED lead:** the driver source has a block of function-code
constants documented only in comments (i.e. **not compiled as active `SYMBOL`s** in
this NPL source revision — each line prefixed `%SYMBOL`, `5P-P2-MON60.NPL:270-279`),
including `STSELFTST=155` *"START SAMSON SELFTEST"*. That code name strongly suggests a
selftest function exists but is **SAMSON/ND-5000-specific** — and per
`CH5CPUPRESENT`(`../../../SINTRAN/ND500/ND500-BUS-INTERFACE-REFERENCE.md` section 8.1),
your emulated CPU is flagged as type **OLD500** or **SAMSON** depending on which probe
path answered (IOX RSTA5 = OLD500; Octobus response = SAMSON). If your bus emulation
answers as OLD500, a Samson-only selftest function could plausibly be rejected — but
this is not confirmed: the dispatch-table index for 155B is `5NOPAR`, a shared
pre-dispatch entry routine (`5P-P2-MON60.NPL:1889`, "COMMON POINT TO GO THROUGH BEFORE
CALLING THE SYSTEM MONITOR"), not `ILLFUNC` — so 155B is not itself in the confirmed
illegal set, weakening this lead. Do not treat the STSELFTST connection as established;
it is a hypothesis to test via disassembly, not a diagnosis.

**Practical takeaway:** `RUN-SELFTEST` is not part of the normal boot/bring-up path (see
section 4b) — it is an optional hardware diagnostic. Given it hits a kernel-level
`ILLFUNC` rejection rather than a bus/detection failure, this looks like a genuine
function-not-implemented-in-this-kernel-build (or CPU-type-mismatch) condition rather
than an emulator bus-interface gap. Recommend avoiding `RUN-SELFTEST` for now and
proceeding with normal domain bring-up instead; treat this as a separate open item, not
a blocker for the CPU-detection/monitor-startup work in sections 4-4b.

- UNVERIFIED: no extracted RT-LOADER/HENT-MODE command for loading the **ND-500 Swapper**
  (`SWAPPER-K:PSEG`/`:DSEG`) was found in any source read. It is a `:PSEG`/`:DSEG` pair, not a
  `:BPUN`, so it likely does not load via the same `READ-BINARY` idiom — do not assume the syntax.

### 4a. The ND-500 Place Library — six `:BRF` variants, install step UNVERIFIED

Live floppy listing of `211305B02-XX-01D` (2026-07-09) shows the package contains more
than the Monitor and Swapper — full inventory and analysis now recorded in
`../../../SINTRAN/ND500/ND500-L-PACKAGE-CONTENTS.md`. Six Place Library files, all type
`:BRF`, all under user `FLOPPY-USER`:
`PLACE-1BANK-C01`, `PLACE-2BANK-C01`, `PLACE-BIG-1B-C01`, `PLACE-BIG-2B-C01`,
`PLACE-SML-1B-C01`, `PLACE-SML-2B-C01`.

- Verified: `PLACE-BIG-2B-C01.BRF` is **ND-100 object code** (confirmed by Ghidra
  disassembly, language ND-100:BE:16 — see
  `../../../SINTRAN/ND500/ND500-PLACE-LIBRARY-C9-FINDINGS.md`), implementing the
  Monitor's `PLACE-DOMAIN`/`DEBUG-PLACE` functionality and MMS/hardware-fault decoding
  (strings: "Domain information table", "Contents of Physical segment Table", "SAMSON
  HARDWARE FAULT", "500 HARDWARE FAULT").
- UNVERIFIED / no install command found: the release doc's copy commands (section 1
  above) only cover the Monitor and Swapper — it gives no `@COPY-FILE` or NRL/link
  command for the Place Library files. Working hypothesis (NOT confirmed by any source
  read, do not act on it as fact): these `:BRF` files may already be linked into
  `ND-500-MON-J04:PROG` at Norsk Data's build time (analogous to how `PASCAL-LIB`/
  `PASCAL-2LIB` get NRL-linked into `PASCAL:PROG` per
  `../../Software/INSTALL-METHODOLOGY.md` section 5), which would explain why the
  release doc never mentions copying them separately. This is plausible but unverified.

### 5. HENT-MODE / LOAD-MODE changes required for version L (general, not ND-500-specific)

Source: same release doc, section 1.6 (lines 499-519).

- HENT-MODE (cold start): remove any commands/mode files that load XMSG; replace Cosmos Basic
  Module loading with the version-F loader; remove ERS/SINTRAN III Watchdog load commands (if
  previously installed); remove `@INITIALIZE-ERROR-LOG` (K and earlier only).
- LOAD-MODE (warm start): remove ERS/Watchdog start commands; if using DOMINO devices, add
  `@START-SERVERS` to start all system-included servers **before** starting the ND-500/5000 part
  of the system; ensure the XMSG-START file references the correct (M-version) XMSG-Command
  program.

## TODO (validate via doc / command / manual exercise)
- Transcribe `START-SINTRAN-MULTIUSER.TXT` (not yet read).
- L bootstrap transcript and floppy file-system IDs.
- Confirm the ND-500 Swapper's actual load command (PSEG/DSEG pair) — not found in sources read.
- RESOLVED (2026-07-09): the Monitor `:PROG` needs no HENT-MODE/RT-LOADER load step —
  it is started directly with `@ND-500`. See section 4.
- Diagnose "No ND-500(0) CPU found" — 3022/5015 interface or Octobus/Samson probe not
  answering `CH5CPUPRESENT` at boot (hardware/emulator config issue, not install issue).
  See section 4.
- Find the actual install/link command for the six Place Library `:BRF` files — not in
  the release doc's copy-command list. See section 4a.
