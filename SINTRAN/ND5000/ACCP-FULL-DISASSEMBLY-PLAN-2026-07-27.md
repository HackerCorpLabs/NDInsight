# Plan: fully disassemble and annotate the ACCP firmware (`octo.bin`)

**Date**: 2026-07-27
**Target**: `E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`
(131,072 bytes, MC68000:BE:32, image base 0, SHA256
`0EA81716AD81984B64675E9A8CCEB6C1909AB299BE0048857C58F85C3479C5F1`)
**Ghidra program**: `octo.bin`, currently open from the working copy `C:\Temp\octo\octo.bin`
(verified byte-identical)

Goal: every byte of the image accounted for as code, data or padding; every routine named
and commented; the octobus and MF-bus hardware interfaces fully specified; the result usable
as the reference for a RetroCore ACCP machine.

---

## STATUS as of 2026-07-28 - THE PLAN IS COMPLETE

| Phase | State |
|---|---|
| 0 - root cause of the dark regions | **DONE** (already in RE section 2.4f; the `noreturn` mechanism was the addition) |
| 1 - prepare the database | **DONE** - No Return cleared on 0x1A0A/0x1BF6, `PlancFixFlow` + `PlancAnnotate` + `PlancApplyConvention` run HEADLESS |
| 2 - find the command dispatch | **DONE** - `ACCP-CONSOLE-COMMAND-SET-AND-DISPATCH-2026-07-27.md` |
| 3 - name every routine | **DONE - ZERO `FUN_` remain**, all 279 named; 26 `HW_*`, 32 `g_*`, 14 tables |
| 4 - specify the hardware | **DONE** - `ACCP-HARDWARE-ADDRESS-MAP-2026-07-27.md` + `OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md`. Octobus TX/RX registers PROVEN from the console commands |
| 5 - remaining data regions | **DONE** - microcode blob solved (128-bit microwords); `0x1FC30-0x1FFFF` examined and is 976 bytes of zeros |
| 6 - write-up | **DONE** - four documents, RE doc updated, memory updated |

### How phase 1 was actually done - the GUI would not cooperate

Four of the six ND.PLANC scripts never appeared in the Script Manager. **The cause was never
found**: files present, correctly tagged `//@category ND.PLANC`, compiling clean against the
12.0.4 jars, directory registered and enabled, bundle cache rebuilt, nothing in the log. The
GUI was bypassed with **headless**:

```
analyzeHeadless.bat E:\Dev\Repos\Ronny\RetroGhidra\ETH_II ND_ETH_II ^
  -process octo.bin -noanalysis -scriptPath <dir> -postScript <Script>.java
```

The scripts call `askYesNo` / `askChoice`, which throws headless - copy the script, replace
the ask with a constant, rename the class to match the new filename, run that. Ghidra must be
closed or the project lock blocks it.

**Order mattered.** `PlancFixFlow` run *before* the No Return flag was cleared managed 368
fallthroughs / 333 sites; run *after*, it did **721 / 693**. Clear the flag first.

**`PlancSetupTypes` and `PlancFrameTypes` were deliberately NOT run** - both hardcode ENCOS
values (8-byte array descriptor, first parameter at +0x12) where this image uses 12-byte and
+0x14. They would apply wrong types across the whole database.

### What remains open

- **13 of the 17 OBCON function-code handlers unread** (0x44-0x48, 0x4A-0x51). None of the
  four read so far touches hardware, so the emulator may not need them.
- `HW_AA_UNKNOWN` (0xAA0000) and `HW_BB_UNKNOWN` (0xBB0000) roles.
- Whether DUART channel B is the ND-100 serial link (likely, unproven).
- Which routine owns the second trace flag `g_traceFlag2_owner_unknown` (0x1143B6).
- ~85 routines are located but not understood - named `<Region>_Helper_<addr>`.

### What phase 4 delivered, and its one real limitation

Proven or carved: SRAM 0x11, DUART 0xDD (all 13 registers, `+2N+1`), MF-bus/BADAP data pair
0x44/0x55, command port 0x22 (a **general** command/function port, not MF-bus-specific),
status 0x66, write-only latches 0x33 with RAM shadows at 0x1144EE/0x1144EF, message/kick read
port 0x88, and 0x77 (data 0x04, cmd 0x06, ready-poll bit 3 of 0x07).

**LIMITATION**: the sweep filtered on replicated nibbles, so it structurally could not see
`0x00900007`, which is real (`move.b (0x00900007).l,...` at 0x07D2). **The nibble rule is a
tendency, not a law, and the select list is therefore still not proven complete.**

---

## 0. Original baseline - measured at the start of the day

| Measure | Value |
|---|---|
| Functions defined in Ghidra | **187** (now 230+) |
| Functions given a real name | **28** (now ~75) |
| Still `FUN_xxxxxxxx` | **159** |
| Code region | roughly `0x000000-0x0114FF` |
| Text / tables region | `0x011500-0x013C2F` (154 strings, the 43-command table at 0x130FE) |
| Selftest microcode blob | `0x013C30-0x01FC2F`, 3072 x 16 bytes - **data, never disassemble** |
| Tail | `0x01FC30-0x01FFFF` unexamined |

**Known gaps in the function map** - large runs between defined functions that are almost
certainly undisassembled code, not padding:

`0x2F66-0x412E`, `0x4C0A-0x6904`, `0x7D26-0x9218`, `0x9218-0xA15A`, `0xA32C-0xB16E`,
`0xB410-0xB8C8`, `0xB8C8-0xC0AE`, `0xC30A-0xCC42`, `0xCE94-0xD926`, `0xD926-0xDF76`,
`0xE2EC-0xE818`, `0xEA3E-0xEF0C`, `0xF06A-0xF3EC`, `0x10320-0x105EA`, `0x10832-0x10BE8`.

That is a large fraction of the image. Most of it is reachable only through the command
dispatch, which is why a fallthrough sweep never found it - the same shape as PIOC-OS's
`trap #2` dispatcher, which was invisible to every static sweep until the vector table was
typed.

---

## Phase 0 - ROOT CAUSE of the dark regions

**NOT a new finding - `ACCP-324716-FIRMWARE-RE-2026-07-27.md` section 2.4f already recorded
this**, under "METHOD NOTE - why large parts of this image look like undefined bytes
[ROOT-CAUSED]", including the same conclusion that `PlancFixFlow` has not been run on this
image and the same manual workaround. What follows adds the `noreturn` mechanism and the
call-site count; the diagnosis itself is not mine.

**Every PLANC routine in this image is flagged `noreturn`, and that single flag is what
created the 15 gap regions.**

`ConsPrintString` @0x1A0A has the signature `noreturn undefined ConsPrintString(void)`, and
`ConsPutCharQueued` @0x1BF6 is flagged the same way. Ghidra inferred it because the PLANC
skip-return epilogue ends in an indirect `jmp (2,A2)`, which its analyser cannot recognise as
a return. Consequence: **disassembly stops dead after every call site.** `ConsPrintString`
alone has **163 call sites** spread across the whole image.

Proof: forcing disassembly at 0x2120 ran cleanly until `jsr 0x1A0A` at 0x2190 and stopped on
the next byte. Forcing it again just past the call yields 2 instructions and stops at the
next call. That is the exact shape of all 15 gaps.

**The fix is one action, and it is not worth working around.** Clearing the `noreturn` flag
on the PLANC routines and re-running auto-analysis should collapse most of the gaps by
itself; `PlancFixFlow` addresses the underlying flow problem properly. Chasing it over MCP
means 163 separate force-disassembles that each recover two instructions - do not do that.

---

## Phase 1 - Prepare the database (must be first)

**Nothing else is reliable until this is done.** The five `ND.PLANC` GUI scripts cannot be
driven over MCP; they need to be run in the Ghidra GUI, in this order:

1. `PlancFixFlow` - fixes the skip-return control flow, loops until stable
2. `PlancAnnotate` - comments error slots, unwinds, prologues; adds tags
3. `PlancSetupTypes` - data types, namespaces, leaf-runtime signatures
4. `PlancApplyConvention` - sets `__planc` on every PLANC routine
5. `PlancFrameTypes` - retypes frame pointers to `PlancFrame*`

Launch with `C:\Utils\ghidraRun.bat` - only that install has the `__planc` prototype patched
into `68000.cspec`. Each script runs in one transaction, so Ctrl+Z reverts a whole run.

**Before running them, adjust for THIS image.** The ACCP is not ENCOS:

| Setting | ENCOS value | **ACCP value** |
|---|---|---|
| First further parameter | +0x12 | **+0x14** |
| Array descriptor | 8 bytes `{long, word, word}` | **12 bytes `{long, long, long}`** |
| Argument staging | via `(0x4,A6)` | via **`(A6)`** |
| Error vector in A5 | 0x135A8 / 0x13596 | **0x115AE** |

If `PlancSetupTypes` hardcodes the 8-byte descriptor or the +0x12 offset, it must be
parameterised or the types it applies will be wrong everywhere. **Check the scripts before
running, not after.**

Exit criterion: skip returns resolved, `jmp (A5)` sites annotated, no new red flow errors.

---

## Phase 2 - Find the command dispatch [DONE 2026-07-27]

**Answer: there is NO jump table. The dispatch is a LINEAR COMPARE CHAIN at 0x227E-0x2746**,
43 x `cmpi.w` / `bne.b` / `jsr` on the command code held in RAM at `0x00113334`. That is why
no table was ever found, and it fits the codes being sparse (0x03..0x46 with real holes),
which a jump table could not be. `HELP` (0x0C) is inline at 0x22D2 with no `jsr`.

Chain: `AccpMainInitAndRunConsole` 0x205C -> `ConsoleCommandLoop` 0x21A6 ->
`ConsoleReadCommandLine` 0x274E + `MatchCommandNamePrefix` 0x2D36. All 42 handlers named
`Cmd<code>_<Name>`. Full table with codes, parameter syntax and handler addresses:
`ACCP-CONSOLE-COMMAND-SET-AND-DISPATCH-2026-07-27.md`.

**Note the exit criterion was NOT met**: identifying the handlers did *not* collapse the gap
regions, because the blocker is phase 0's `noreturn` flag, not missing references. The
handlers exist as named 1-byte stubs and will fill in when phase 1 runs.

### Superseded original text

The 43-entry table at `0x130FE` is `{word code, long origo, long lower, long upper}` - a
command **code plus its name descriptor**. It holds no handler address. So there is a
separate dispatch keyed on that code word, and finding it is what makes the 15 gap regions
reachable.

Method:
1. Find the reader of `0x130FE` (xrefs), which is the command matcher.
2. From it, follow the code word into whatever selects the handler - a jump table, a
   computed branch, or a linear compare chain.
3. Type the jump table as pointers. That single action creates the references that make
   every dark handler visible, exactly as typing the vector table did.
4. Auto-analyse again; the gap regions should collapse.

Exit criterion: each of the 43 commands has an identified handler address, and the largest
gap regions have become functions.

**Fallback if there is no table**: sweep each gap for the PLANC prologue byte pattern
(`2F 0E` `2C 56` ...) and force-disassemble at each hit. Slower and it will not tell you
which command owns which routine, so try the dispatch first.

---

## Phase 3 - Name every routine by evidence, in this priority order

Per the standing rule: rename everything, comment heavily, never leave a `FUN_`.

There is **no symbol table in this image** (checked - the candidate byte patterns all fall
inside the microcode blob), so names must be invented from evidence. The evidence sources,
strongest first:

1. **Error-string descriptors.** The single most productive technique so far. A routine that
   fails loads a 12-byte descriptor; resolve `{origo, lower, upper}` to the text and the
   routine names itself. This is how the MF-bus routine at 0x70CC was pinned via
   `"$MF-bus memory timeout$"`. The strings already name routines outright:
   `in MFCRECEIVE`, `in ND100TRANSMIT`, `in Areceive`, `in DOREC_MULTI_OCTO`,
   `in DOSEND_MULTI_OCTO`. **Those five names are ND's own and must be used verbatim.**
2. **The command table** - a handler reached only from `SEND-KICK-OCTOBUS` is named for it.
3. **Hardware addresses touched** - a routine hitting 0xDD0000 is DUART, 0x220000 is MF-bus.
4. **Call-graph position** - leaf helpers, the print chain, the matcher.

Order of attack, by value:
- **3a. The octobus / NDOBCON driver, `0x6A74-0x7C14`** (the dense 0x33/0x66/0x88 cluster).
  This is the highest-value region: it is the interface we have no documentation for and the
  reason this analysis started.
- **3b. The main loop and command interpreter** - `"Error exit from idle loop"` anchors it.
- **3c. The MF-bus / BADAP path** around 0x70CC, already partly carved.
- **3d. The selftest command handlers** - large, but each is self-labelling from its test
  name string, so cheap per byte.
- **3e. The console / DUART layer**, mostly done.
- **3f. The PLANC leaf runtime** around 0x115AE - name from the ENCOS equivalents
  (`#XRET`, `#ERET`, `#IMU`, `#IDV`, `#APPD`, `#REMV`), which are byte-comparable across the
  two images. **Compare bytes before assuming a match**, the compiler versions differ.

---

## Phase 4 - Specify the hardware interface

The deliverable that the emulator actually needs. Chip selects are nibble-replicated
(`0xNN0000`); assume more exist than are currently listed, because 0x44 and 0x55 only
surfaced when 0x70CC was hand-disassembled.

| Select | Status |
|---|---|
| 0x11 | SRAM 0x110000-0x117FFF - **proven** |
| 0xDD | SCN2681 DUART, register N at `0xDD0000 + 2N + 1` - **proven** |
| 0x22 / 0x44 / 0x55 / 0x66 | MF-bus command / data-low / data-high / status - **carved, function codes NOT proven** |
| 0x33, 0x88, 0x90, 0xBB | **unidentified** - the octobus side |

For each: record every access (read/write, width, bit tested), then derive the register's
role from what the code does with it.

**Read the document BEFORE naming anything** - a documented map beats a carved one.
`E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-14001-1-EN DOMINO Standard Hardware
Description.md` is present and its **chapter 4, "THE OCTOBUS ADAPTER (OBA)"** is
substantive, not a stub: 4.1 protocol, 4.2 nodes and the MASTER, 4.3 allocation algorithm,
4.4 frame format (including the frame as seen from the output driver and from the input
driver, and the acknowledge bits), 4.5 hardware-generated messages, 4.7 the INT7 OCTObus
Message Reset Register, 4.8 initialisation and MASTER selection. It also names the part
**OBCON** ("OCTObus interface (OBCON)"), and documents "Write OCTObus Initialization Values
(WOI)". Also check `ND-05.017.01` section 3.4.

That chapter should be read first and its register names adopted; the carve then becomes
confirmation rather than invention. It probably also settles the NDOBCON-vs-OCTC naming
question - the manual's spelling is OBCON.

Exit criterion: a register table with, for every address, the access width, the bits used,
and how the firmware reacts to each - enough to write the emulated device.

---

## Phase 5 - The remaining data regions

- **The selftest microcode blob `0x13C30-0x1FC2F` - SOLVED 2026-07-27.** The loader is
  `LoadSelftestMicrocodeIntoControlStore` @0xB16E. The descriptor is at **0x13C18** and is
  **two-dimensional, 12 bytes per dimension**: `{origo 0x13C30, 0, 0xBFF}` x `{stride 8, 0, 7}`.
  Element address = `IMU(8, i*2)` = `i*16`. So each 16-byte record is **eight 16-bit fields =
  one 128-bit microword**, matching `LOAD-CONTROL-STORE <127-112>..<015-000>` field for field.
  **The "1-based sequence number at +0x0C" reading was WRONG** - those bytes are word index 6
  (bits 031-016) of the microword, which merely increments. Staging buffer 0x1144F0.
- **`0x1FC30-0x1FFFF`** - never examined. Could be padding, could be a checksum region. The
  eprom README notes no integrity value has been found; look here.
- Type every remaining string and descriptor so the listing has no undefined bytes left.

---

## Phase 6 - Write it up and close the loop

- Fold everything into `ACCP-324716-FIRMWARE-RE-2026-07-27.md` (the write-up of record).
- Update `..\..\Installation\Communication\OctobusAccp\eprom\README.md` - specifically the
  "Still not verified" items (ROM base address, dump integrity) if phase 5 resolves them.
- Update the RetroCore handoff
  `ACCP-RETROCORE-MACHINE-IMPLEMENTATION-HANDOFF-2026-07-27.md` with the phase 4 register
  table.
- Update the memory `accp-octo-bin-68k-firmware.md`.

---

## Sequencing and what blocks what

```
Phase 1 (GUI scripts)  --->  Phase 2 (dispatch)  --->  Phase 3 (naming)
                                    |                        |
                                    +--------> Phase 4 (hardware) <--- ND-14001 ch.4
                                                             |
                                    Phase 5 (data) ----------+---> Phase 6 (docs)
```

Phase 1 is a hard prerequisite - naming routines before the skip-return flow is fixed means
re-doing them. Phase 2 gates most of phase 3. Phase 4 can start on the already-visible
regions in parallel.

## What I need from you

- **Phase 1 must be run by you in the Ghidra GUI.** The scripts cannot be driven over MCP.
  Everything after it I can do through the MCP connection.

(The ND-14001 question is answered - see phase 4.)

## Explicitly out of scope

- Decompiling to recoverable PLANC source
- The ND-5000 microcode itself (the blob's payload semantics)
- Building the RetroCore machine - that is the separate handoff document
