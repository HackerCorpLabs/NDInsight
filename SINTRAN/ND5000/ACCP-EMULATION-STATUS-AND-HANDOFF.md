# ACCP emulation - plan, status, handoff, defects and captures

The working state of the ACCP work: the disassembly plan and its status of record, the
RetroCore machine implementation handoff, the defect report, and the raw captures taken
off the emulated card.

**This file replaces six separate documents** (listed below), merged 2026-07-31. The part
bodies are the original text; nothing was summarised, condensed or dropped, and the merge
was verified line by line. Part 1 additionally carries a 2026-07-31 edit made before the
merge, so it is newer than its last commit.

Companion file: `ACCP-COMPLETE-REFERENCE.md` (firmware, hardware, protocol, CPU model).

---

## HEADLINE 2026-08-04: the ACCP now LOADS CODE into an ND-5000 control store

Typed at the card's own prompt, parsed by the real `octo.bin`, shifted over the real register
protocol, landing in a real `ControlStore`:

```
LOAD-CONTROL-STORE 100 1122 3344 5566 7788 99AA BBCC DDEE F001
  -> card prints "0100H: 1122H ... F001H" then "- OK -"
  -> COMMIT cmd=0x0018 addr=0x0100 gate=0x04  112233445566778899AABBCCDDEEF001
```

The card's OWN `BUS test` and `MIR test` selftests now pass - the firmware writes a pattern, reads it
back and judges itself, which is the strongest oracle available here.

Code (full paths):
`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\src\Devices\Nd5000ControlStoreLink.cs`,
`...\src\Devices\Nd5000LinkWindow.cs`, `...\src\Devices\IAccpStatusBitSource.cs`, attached via
`AccpMachineConfig.ControlStoreSink`. The `IControlStoreSink` seam keeps the ACCP package free of any
dependency on the CPU package.

**Five corrections to the earlier CS-load carve** - all found by TRACING the running firmware:

1. **The gate is bit 1 OR bit 2.** Boot uses bit 1 (`0x764E`) exclusively; bit 2 (`0x741E`) is the
   console command only. Recognising just bit 2 gave 1.6M clock pairs and ZERO microwords.
2. **The address is the NINTH gated word**, not a pre-gate write. Boot values once logged as junk
   (`000B, 0016, 0021, 002C, 0037`) were addresses stepping by 11.
3. **The completed word must be LATCHED** - the card closes and reopens the gate between the shift
   and the perform. The carve's own "BUFFERED CI-bit groups" was literal.
4. **Multiple performs occur inside one gate window** - reset the staging count after each.
5. **`0x440000` must be HELD and echoed**, not ignored.

> **RETRACTED, and worth keeping as a warning:** "281 microwords went into the control store during
> boot." A histogram by command word showed all of them were `0x2018` at address 0, never `0x0018`.
> **Booting the card does not exercise the addressed control-store path at all.** Break an operation
> count down BY COMMAND before claiming what it proves.

**STILL BLOCKED: "run it".** `STARTMIC` -> `ALIVE` needs the ND-5000 executing microcode. The AFLAG
half of that is now answered - see `ANSWERS-ACCP-CPU-SEAM-CONTRACT-2026-08-04.md`: `AflagAtrapBit = 5`
`[V]`, FATAL is a trap-WORD payload classified by `TRAP_ACCP` and keeps `BitNotModelled`,
`AobReadClearsWide = false`.

---

## READ THIS FIRST - this is a STATUS file and parts of it are stale

Six documents written across five days. Later parts overturn earlier ones, and in several
places the **stale claim sits later in reading order**, so it looks current. It is not.

| Claim you will meet | Where | Reality |
|---|---|---|
| The matrix builder has **three** phases, plus a full `read[w] bit s` derivation and a closing "one-line instruction for an implementer" | **part 2, section 4z** | **WRONG - four phases.** Implementing 4z's derivation produces the wrong matrix and a refused model. See part 1's follow-on section, and part 5 of `ACCP-COMPLETE-REFERENCE.md` |
| "Only model digits 8 and 9 can ever be accepted today" | part 2, section 4z | Superseded. **Any class is now selectable**, live-verified: class 2, ND-5500 accepted |
| The three model-digit tests are `[Ignore]d` because of defect 1 | part 2 | Defect 1 is marked **SOLVED** in the same part. The stated reason no longer exists |
| `0x900001` "is being missed" / "the machine almost certainly returns 0" | **part 3, defect D6a** | Stale. It IS in the handoff table (part 2) and the machine returns **1**. Whether 1 is the right value is still open - see part 6 question 4 |
| "`PlancAnnotate` has been run but `PlancFixFlow` has not - run the scripts in the Ghidra GUI" | part 2 | Done, and the GUI route is a **dead end**. Part 1 records that headless was required and that the GUI would not list the scripts |
| "Find the command that sets the trace flags `0x1143B4` / `0x1143B6`" | part 2 | Half done: `TRACE-COMMUNICATION-DATA` (command `0x3C`) sets `0x1143B4`. The owner of `0x1143B6` is genuinely still open |
| `0x1FC30-0x1FFFF` "never examined" | part 1 baseline section | Examined - 976 bytes of zeros. Stated correctly elsewhere in part 1 |
| Part 1's phase-4 table: "`0x33`, `0x88`, `0x90`, `0xBB` unidentified" | part 1 | All since identified. See part 2's device table and part 2 of the companion file |
| Phase 3 / Phase 5 / Phase 6 "remaining work" - reach the prompt, carve OBCON, build it | part 2 | Largely done: `ACCP:` prompt reached, `HELP` answered, 28/28 selftests, `NDOBCON` built with 15/15 tests. **Do not rebuild what exists** |
| Baseline counts "187 functions, 28 named, 159 still `FUN_`" | part 1 | A historical baseline. Current state is **279 named, zero `FUN_`** |
| The six questions in part 6 | part 6 | **All answered.** See `ANSWERS-TO-ND5000-TEAM-2026-07-30.md` (one section per question) and `ANSWER-CPU-MODEL-ENCODINGS-2026-07-30.md` for question 2. Question 1 is also marked `[SUPERSEDED]` inside part 2 |

**Part 5 is a deliberately BROKEN-STATE capture.** It was produced by putting a defect back
on purpose, and it predates the corrected model decode, so it still shows `model=0x38
class=3 digit=8`. **It is not current behaviour.** Part 4 is the clean run. The 244B
TERMINATE appears in BOTH captures - only the stuck flag differs.

**"Phase N" is ambiguous across this file.** Four unrelated numbering schemes share it:
part 1's *disassembly plan* phases 0-6; part 2's *build* phases; the *signature matrix
builder's* firmware phases 1-4; and this file's own Part 1-6. "Phase 4" alone means three
different things. Always check which scheme a mention belongs to.

**Cross-reference warning.** Each part kept its own numbering, so a bare "see section 5"
means section 5 **of the part you are reading** - and "section 5" is easily misread as
"Part 5". The `2.4x` lettered references into the companion file are unique and safe.

---

## Contents

1. **Part 1** - the disassembly plan and its status of record, including the headless-Ghidra
   recipe that got past a GUI which would not list the ND.PLANC scripts, and the 2026-07-31
   follow-on recording the signature matrix as solved.
   Originally `ACCP-FULL-DISASSEMBLY-PLAN-2026-07-27.md`.
2. **Part 2** - the RetroCore machine implementation handoff: decisions, the device table,
   the phased build, and what an MFbus controller model must do. **Its section 4z is
   superseded - see the table above.**
   Originally `ACCP-RETROCORE-MACHINE-IMPLEMENTATION-HANDOFF-2026-07-27.md`.
3. **Part 3** - the defect report (D1-D6b). **ALL CLOSED as of 2026-08-01** - audited one by one
   against the code and the tests:
   D1 fixed and guarded by a pair of tests; D2 pinned as literals; D3 banner spacing asserted;
   D4 cannot occur and its stated mechanism was wrong; D5+D6 already resolved; D6a implemented;
   D6b implemented with a live peer. **No defect in this list is open.** The entries are kept for
   their analysis, which is still the best explanation of WHY each mattered.
   Originally `ACCP-MACHINE-DEFECT-REPORT-2026-07-28.md`.
4. **Part 4** - clean-boot bidirectional ACCP command log. The good capture.
   Originally `ACCP-COMMAND-LOG-CLEAN-BOOT-CAPTURE-2026-07-30.md`.
5. **Part 5** - the 244B TERMINATE capture. **PRE-FIX, broken on purpose, not current
   behaviour.** Originally `ACCP-244B-TERMINATE-PREFIX-CAPTURE-2026-07-30.md`.
6. **Part 6** - questions put to the ACCP team. **All since answered** - see the table above
   for where. Originally `QUESTIONS-TO-ACCP-TEAM-2026-07-30.md`.


---

# Part 1 - originally `ACCP-FULL-DISASSEMBLY-PLAN-2026-07-27.md`

> Merged verbatim 2026-07-31. Content is byte-identical to the original;
> only heading levels were demoted by one so the parts nest under this file.

## Plan: fully disassemble and annotate the ACCP firmware (`octo.bin`)

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

### STATUS as of 2026-07-28 - THE PLAN IS COMPLETE

| Phase | State |
|---|---|
| 0 - root cause of the dark regions | **DONE** (already in RE section 2.4f; the `noreturn` mechanism was the addition) |
| 1 - prepare the database | **DONE** - No Return cleared on 0x1A0A/0x1BF6, `PlancFixFlow` + `PlancAnnotate` + `PlancApplyConvention` run HEADLESS |
| 2 - find the command dispatch | **DONE** - part 3 of `ACCP-COMPLETE-REFERENCE.md` |
| 3 - name every routine | **DONE - ZERO `FUN_` remain**, all 279 named; 26 `HW_*`, 32 `g_*`, 14 tables |
| 4 - specify the hardware | **DONE** - part 2 of `ACCP-COMPLETE-REFERENCE.md` + `OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md`. Octobus TX/RX registers PROVEN from the console commands |
| 5 - remaining data regions | **DONE** - microcode blob solved (128-bit microwords); `0x1FC30-0x1FFFF` examined and is 976 bytes of zeros |
| 6 - write-up | **DONE** - four documents, RE doc updated, memory updated |

#### How phase 1 was actually done - the GUI would not cooperate

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

#### FOLLOW-ON CARVE 2026-07-31 - the signature matrix / CPU class is now SOLVED

Not part of the original six phases; it came out of driving the emulated ACCP machine. Full
write-up: **part 5 of `ACCP-COMPLETE-REFERENCE.md`**.

| Item | State |
|---|---|
| Class chain `0x110A` - which matrix words, which sentinel | **DONE** (word 3/6/2 at byte offsets +6/+0x0C/+4 vs `0x7F55`) |
| Matrix builder `0x7D26` **phase 4** rewrite `0x7DD0`..`0x7EA4` | **DONE** - carved and inverted |
| Helper `0x7CA2` | **DONE** - a 7-bit Gray-to-binary decoder |
| `0x220000` read-port behaviour | **DONE** - armed/disarmed, arm = a write of `0x0007` |
| Selecting an arbitrary CPU model class | **DONE** - live-verified class 2 / ND-5500 accepted |

Two corrections this produced, both to statements made confidently earlier:

- **The matrix is NOT just the transpose of the sixteen reads.** The builder has **four**
  phases; phase 4 rewrites every word in place before the class chain reads it. Any formula
  mapping `read[w]` straight onto `matrix[s]` describes phase 3 only.
- **`0x220000` has exactly TWO readers**, not "many sites". An earlier claim of 51 reads per
  boot from multiple sites was an artefact of a broken alignment rule, and the conclusion drawn
  from it ("the design needs replacing") was wrong.

This also closes part of the phase-4 gap above: **`0x22` is confirmed as a general command port
whose READ side is the CPU-model signature source**, and its arm code `0x0007` is now known.

#### What remains open

- **13 of the 17 OBCON function-code handlers unread** (0x44-0x48, 0x4A-0x51). None of the
  four read so far touches hardware, so the emulator may not need them.
- **Why the matrix builder runs three times per boot**, and whether any caller expects a
  different sequence.
- `0x220000` write codes `0x300F` / `0x400A` / `0x400C` / `0x000F` remain undecoded.
- What real hardware presents at `0x220000` - we model the sequence the firmware consumes, not
  the mechanism that produces it.
- `HW_AA_UNKNOWN` (0xAA0000) and `HW_BB_UNKNOWN` (0xBB0000) roles.
- Whether DUART channel B is the ND-100 serial link (likely, unproven).
- Which routine owns the second trace flag `g_traceFlag2_owner_unknown` (0x1143B6).
- ~85 routines are located but not understood - named `<Region>_Helper_<addr>`.

#### What phase 4 delivered, and its one real limitation

Proven or carved: SRAM 0x11, DUART 0xDD (all 13 registers, `+2N+1`), MF-bus/BADAP data pair
0x44/0x55, command port 0x22 (a **general** command/function port, not MF-bus-specific),
status 0x66, write-only latches 0x33 with RAM shadows at 0x1144EE/0x1144EF, message/kick read
port 0x88, and 0x77 (data 0x04, cmd 0x06, ready-poll bit 3 of 0x07).

**LIMITATION**: the sweep filtered on replicated nibbles, so it structurally could not see
`0x00900007`, which is real (`move.b (0x00900007).l,...` at 0x07D2). **The nibble rule is a
tendency, not a law, and the select list is therefore still not proven complete.**

---

### 0. Original baseline - measured at the start of the day

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

### Phase 0 - ROOT CAUSE of the dark regions

**NOT a new finding - part 1 of `ACCP-COMPLETE-REFERENCE.md` section 2.4f already recorded
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

### Phase 1 - Prepare the database (must be first)

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

### Phase 2 - Find the command dispatch [DONE 2026-07-27]

**Answer: there is NO jump table. The dispatch is a LINEAR COMPARE CHAIN at 0x227E-0x2746**,
43 x `cmpi.w` / `bne.b` / `jsr` on the command code held in RAM at `0x00113334`. That is why
no table was ever found, and it fits the codes being sparse (0x03..0x46 with real holes),
which a jump table could not be. `HELP` (0x0C) is inline at 0x22D2 with no `jsr`.

Chain: `AccpMainInitAndRunConsole` 0x205C -> `ConsoleCommandLoop` 0x21A6 ->
`ConsoleReadCommandLine` 0x274E + `MatchCommandNamePrefix` 0x2D36. All 42 handlers named
`Cmd<code>_<Name>`. Full table with codes, parameter syntax and handler addresses:
part 3 of `ACCP-COMPLETE-REFERENCE.md`.

**Note the exit criterion was NOT met**: identifying the handlers did *not* collapse the gap
regions, because the blocker is phase 0's `noreturn` flag, not missing references. The
handlers exist as named 1-byte stubs and will fill in when phase 1 runs.

#### Superseded original text

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

### Phase 3 - Name every routine by evidence, in this priority order

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

### Phase 4 - Specify the hardware interface

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

### Phase 5 - The remaining data regions

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

### Phase 6 - Write it up and close the loop

- Fold everything into part 1 of `ACCP-COMPLETE-REFERENCE.md` (the write-up of record).
- Update `..\..\Installation\Communication\OctobusAccp\eprom\README.md` - specifically the
  "Still not verified" items (ROM base address, dump integrity) if phase 5 resolves them.
- Update the RetroCore handoff
  part 2 of this file with the phase 4 register
  table.
- Update the memory `accp-octo-bin-68k-firmware.md`.

---

### Sequencing and what blocks what

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

### What I need from you

- **Phase 1 must be run by you in the Ghidra GUI.** The scripts cannot be driven over MCP.
  Everything after it I can do through the MCP connection.

(The ND-14001 question is answered - see phase 4.)

### Explicitly out of scope

- Decompiling to recoverable PLANC source
- The ND-5000 microcode itself (the blob's payload semantics)
- Building the RetroCore machine - that is the separate handoff document

---

# Part 2 - originally `ACCP-RETROCORE-MACHINE-IMPLEMENTATION-HANDOFF-2026-07-27.md`

> Merged verbatim 2026-07-31. Content is byte-identical to the original;
> only heading levels were demoted by one so the parts nest under this file.

## HANDOFF - implement the ACCP machine in RetroCore

**Goal**: an emulated Samson ACCP card (ND-324716 / PCB 5616) in RetroCore that boots the real
firmware, prints its banner on an emulated console, and becomes a unit-test harness that uses the
firmware's own selftest output as the oracle.

**Companion document - READ IT FIRST**:
part 1 of `ACCP-COMPLETE-REFERENCE.md`
Every hardware fact below is derived there, with the evidence. This file is only the build plan.

**Firmware image (of record)**:
`E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`
131072 bytes. SHA256 `0EA81716AD81984B64675E9A8CCEB6C1909AB299BE0048857C58F85C3479C5F1`.
Load it from this path; do not copy it into the RetroCore tree.

**Repo**: `E:\Dev\Repos\Ronny\RetroCore`

---

### 0a. UPDATE 2026-07-28 - the firmware is now FULLY reverse engineered

This handoff was written while large parts of the image were still undisassembled. That is no
longer true: **every one of the 279 functions is named, all 43 console commands are decoded
with their handler addresses, and the octobus registers are proven.** Build against the table
below rather than anything softer earlier in this file.

#### The device set you must model - and it is smaller than expected

| Address | Dir | Width | Role | Confidence |
|---|---|---|---|---|
| `0x110000-0x117FFF` | rw | - | **SRAM 32 KB**. Stack 0x110000-0x111FFF (SP = 0x112000, grows down); globals from 0x112000. A6 = 0x110000 is the PLANC global base | PROVEN |
| `0xDD0000 + 2N + 1` | rw | byte | **SCN2681 DUART**, register N on ODD bytes. Channel A = console | PROVEN |
| `0x770004` | w | word | **OCTObus transmit data** | PROVEN |
| `0x770007` | r | byte | bit 3 = **transmit ready** | PROVEN |
| `0x880000` | r | word | **OCTObus receive data (FIFO)** | PROVEN |
| `0x660001` | r | byte | bit 1 = AOB busy, **bit 2 = octobus RX available**, bit 4 = MF-bus complete | PROVEN |
| `0x660000` | r | byte | bit 0 = control-store op OK; bits 3, 5 also tested | carved |
| `0x220000` | w | word | **general command/function port** - the code selects the target of the data pair: `0x300F`/`0x400A`/`0x400C`/`0x000F` MF-bus, `0x0005` AOB, `0x0018` control store | carved |
| `0x440000` / `0x550000` | rw | word | 32-bit data pair, low / high | carved |
| `0x330000` / `0x330001` | w | byte | **write-only latches**. 0x330000 bit 6 = write strobe, bit 2 = control-store gate. **Never read back** - the firmware keeps RAM shadows at `0x1144EE`/`0x1144EF`, so your read value for these two is irrelevant | carved |
| **`0x900001`** | r | byte | **THE ACCP'S OWN STATION NUMBER**, low 5 bits (`and.b #0x1F`). Read at 0x122E before the MFbus scan. **Must not be 0** - station 0 is illegal on the OCTObus | **PROVEN 2026-07-28** |
| `0xAA0000`, `0xBB0000`, `0x900007` | - | - | real but role unknown | unknown |

**`0x900007` breaks the replicated-nibble rule.** Do not build an address decoder that assumes
`0xNN0000` with NN a repeated nibble - that is a strong tendency, not a law.

#### Two behaviours you MUST reproduce exactly

1. **Neither octobus ready-poll has a timeout.** `OctobusTransmitWord` @0x7890 and
   `OctobusReceiveWord` @0x786C are unbounded `beq.b -10` spins. If your model never raises
   `0x770007` bit 3 or `0x660001` bit 2, **the ACCP hangs** - exactly as the real card would.
   The `"K I C K   T I M E O U T"` message comes from a caller, not from these.
2. **The OBCON driver is software, not registers.** `ObconRequestDispatch` @0xF686 has 17
   function codes, and none of the four handlers examined touches hardware at all. Model the
   two raw primitives plus the IRQ3 (0x0510) and IRQ7 (0x0826) paths; the dispatcher then runs
   as ordinary code on top. You do not need to emulate a 17-function driver.

#### Free test oracles the firmware hands you

- **The RAM walk-test at reset** (0x0BD6) validates CPU + ROM + RAM with no chip present.
  Assert `g_ramTestErrors_firstHalf` (0x11312A, **32-bit**), `_secondHalf` (0x11312E, 32-bit)
  and `g_ramTestDone` (0x113132, **16-bit**) - note the widths.
- **`TRACE-COMMUNICATION-DATA Y`** (command 0x3C) sets `g_traceOctobusKicks` (0x1143B4), after
  which the IRQ3 handler prints every kick with `" from SAMSON"` / `" to SAMSON"`. **The
  firmware narrates its own octobus traffic** - the cheapest possible cross-check.
- **The selftest suite** prints per-test pass/fail text; each test is a named function
  (`Selftest_*`) so you can aim at one at a time.
- **`READ-ACCP-STATUS`, `CHECK-ALIVE`, `SHOW-REGISTERS`** are cheap liveness commands.

#### Where the details live

- Command set and dispatch: part 3 of `ACCP-COMPLETE-REFERENCE.md`
- Full hardware sweep: part 2 of `ACCP-COMPLETE-REFERENCE.md`
- OCTObus protocol + driver API: `OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md`
- Everything else: part 1 of `ACCP-COMPLETE-REFERENCE.md`

---

### 0. Decisions already made - do not relitigate

| Decision | Choice | Why |
|---|---|---|
| DUART chip home | `Nuget\HackerCorpLabs.Emulation.Chips.Motorola\src\DUART\SCN2681\` | MAME shares one core (`duart_base_device`) between `scn2681_device` and `mc68681_device`; putting it anywhere else splits or duplicates that core. Package id stays `HackerCorpLabs.Emulation.Chips.Motorola`. |
| Namespace | `HackerCorpLabs.Emulation.Chips.Motorola.DUART.SCN2681` | Namespaces mirror folders in this repo - see `src\ACIA\MC6850\MC6850ACIA.cs` -> `...Chips.Motorola.ACIA.MC6850`. |
| Serial fidelity | **Byte-level**, like the existing `MC6850ACIA` | Do NOT port MAME's `device_serial_interface` bit shifting. Characters move as bytes with a per-character delay. Sufficient for the console and for every test below. |
| Build order | Machine + memory map FIRST, DUART second | The RAM walk-test at reset validates CPU + ROM + RAM with no chip present. |

Record a note in `Chips.Motorola\TODO.md`: the SCN2681 is a Signetics/Philips part parked in the
Motorola package so it can share the DUART core with the MC68681; the split is reversible.

---

### 1. What already exists in RetroCore (verified 2026-07-27)

| Need | Where |
|---|---|
| MC68000 CPU | `Nuget\HackerCorpLabs.Emulation.CPU.MC68K`, class `MC68KCpu(bus, CpuLevel.M68000)` |
| Smallest machine template | `Nuget\HackerCorpLabs.Emulation.Machines.Generic68K\src\Generic68KMachine.cs` - copy its shape |
| Full machine example (ROM + many devices) | `Nuget\HackerCorpLabs.Emulation.Machines.MacIIci\src\MacIIciMachine.cs` |
| Package scaffold | `Nuget\_template\` (src, tests, benchmarks, Directory.Build.props, global.json, nuget.config, .sln, .github\workflows\docs.yml) |
| Serial-chip pattern | `Chips.Motorola\src\ACIA\MC6850\MC6850ACIA.cs` - `class MC6850ACIA : ChipBase, IMemoryDevice` |
| Bus builder | `Nuget\HackerCorpLabs.Emulation.SystemBus\src\MemoryBuilder.cs` |
| MAME source | `E:\Dev\Emulators\mame\src\devices\machine\mc68681.cpp` (47 KB) + `.h`; `scn2681_device` at `mc68681.h:199` |

~~`MemoryBuilder` public surface is exactly: `Ram(start,end,name,useFastPath)`,
`Ram(start,end,byte[],name)`, `AlwaysVisibleIo(device,start,end)`, `Device(IMemoryDevice,name)`,
`Mirror(mirrorMask)`.~~ **WRONG - corrected 2026-07-27.** That list came from a case-sensitive
search that missed the all-caps methods. The real surface also includes `ROM(start,end,name)`,
`ROM(name)`, `Banking`, `BankedRegion`, `RelocatableRegion`, `GatedIoRegion`, `Space`, `Chip<T>`,
`IO()` and `Build()`. See §1.2.

#### 1.1 The odd-byte DUART decode is already solved

`GappedAddressDecoder(inner, start, end, shift, mask)` does exactly what the ACCP needs. The
SCN2681 sits at register N = `0xDD0000 + 2N + 1`, so `shift: 1, mask: 0xF`. **Write no custom
wrapper.**

**Arithmetic verified 2026-07-27** against `Machines.MacIIci\src\GappedAddressDecoder.cs:66`, which
computes `reg = ((address - _start) >> _shift) & _mask`. For the ACCP: `address - 0xDD0000 = 2N+1`,
`>> 1 = N`, `& 0xF = N`. Correct for all 16 registers, with `end = 0xDD001F` (register 15 lives at
`0xDD0000 + 31`).

Two caveats, neither a blocker:

- The decoder is **byte-only** (it implements just `ReadByte`/`WriteByte`). That matches the
  firmware, which touches the DUART exclusively with `move.b`. If the bus splits a word access into
  two byte calls, an even+odd pair would hit register N twice rather than N and N+1 - harmless here
  because no word access to `0xDD....` exists in the firmware, but worth a comment in the machine.
- Even addresses (`0xDD0000 + 2N`) alias onto the same register N rather than reading as
  unmapped/bus-error, because the shift discards bit 0. Real hardware wires only the odd byte lane.
  Again harmless for this firmware; do not "fix" it without evidence.

**BLOCKER-ISH / ask Ronny before coding**: `GappedAddressDecoder` currently exists TWICE -
`Machines.MacIIci\src\GappedAddressDecoder.cs` and an inline copy in
`Machines.MacClassic\src\MacClassicMachine.cs:1251`, both `internal sealed`. Per the project's
no-code-duplication rule, **promote one copy into `HackerCorpLabs.Emulation.SystemBus` and repoint
MacIIci + MacClassic at it**, rather than creating a third copy.

**RESOLVED 2026-07-28 - DONE.** Ronny ruled: move it into `HackerCorpLabs.Emulation.SystemBus`.
Implemented:

- New `public sealed class GappedAddressDecoder` at
  `Nuget\HackerCorpLabs.Emulation.SystemBus\src\GappedAddressDecoder.cs`
  (namespace `HackerCorpLabs.Emulation.SystemBus`).
- Deleted `Nuget\HackerCorpLabs.Emulation.Machines.MacIIci\src\GappedAddressDecoder.cs`.
- Removed the inline copy from `MacClassicMachine.cs` (a plain comment records the move).
- Both machines already had `using HackerCorpLabs.Emulation.SystemBus;`, so no call site changed.

So the ACCP just uses it: `new GappedAddressDecoder(duart, 0xDD0000, 0xDD001F, shift: 1, mask: 0xF)`.

**PERFORMANCE TRAP found while validating this - read before touching that class.**
The original copies re-read `_inner.StartAddress` - an **interface property call** - on *every*
`ReadByte`/`WriteByte`. That is a per-bus-access interface dispatch on a path the Mac ROM hits
hundreds of millions of times during boot.

Consolidating without addressing it made the MacIIci ADB boot test ~18% slower
(2m02s -> 2m24s standalone). That test carries `[Timeout(300_000)]`, and **NUnit's timeout kills the
entire test host**, so under full-suite load the run did not fail one test - it aborted the whole
suite after 39 tests. Baseline was clean 2/2; the regression reproduced 3/3.

Fix: cache the inner base address in a `readonly uint _innerBase` field in the constructor. That
restored parity (2m06s standalone) and the full suite is green again. The cache is valid only
because every device wrapped by this type has a construction-fixed `StartAddress`; a movable-base
device must not be wrapped by it. `[MethodImpl(AggressiveInlining)]` on the two accessors helped
slightly but was **not** sufficient on its own - the interface property call was the real cost.

Validation (all after the fix):

| Suite | Result |
|---|---|
| `HackerCorpLabs.Emulation.SystemBus.Tests` | 61/61 pass |
| `HackerCorpLabs.Emulation.Machines.MacClassic.Tests` | 39/39 pass |
| `HackerCorpLabs.Emulation.Machines.MacIIci.Tests` | 56/56 pass, twice |

Lesson worth carrying: a long-running emulation test with an NUnit `[Timeout]` turns a modest
performance regression into a whole-suite abort with no useful error message. When a test host
"crashes" after a refactor, measure the hot path before assuming a correctness bug.

#### 1.2 RESOLVED 2026-07-27 - there is no ROM problem

**This section previously claimed "`MemoryBuilder` has no `Rom(...)` method" and sent the
implementer chasing `RomManager` and the MacIIci post-Build overlay. That premise was wrong.**

Root cause of the error: the method is spelled **`ROM`**, all caps, not `Rom`. A case-sensitive
search for `Rom(` misses it, and the "public surface is exactly Ram / AlwaysVisibleIo / Device /
Mirror" list above was built from that bad search. That list is incomplete - `MemoryBuilder` also
exposes `ROM`, `Banking`, `BankedRegion`, `RelocatableRegion`, `GatedIoRegion`, `Space`, `Chip`,
`IO`, `Build`.

`MemoryBuilder.ROM(start, end, name)` (`SystemBus\src\MemoryBuilder.cs:53`) returns a `ROMBuilder`
(`SystemBus\src\ROMBuilder.cs`), which you finish with `.LoadFile(path)` or `.LoadArray(bytes)`.
Both funnel into `TrackFastMemory(..., isReadOnly: true, ...)` - candidate 1 in the old list, just
reached through the fluent API instead of by hand.

So the ACCP ROM declaration is simply:

```csharp
// 128 KB EPROM at 0x000000. LoadFile logs a warning and 0xFF-fills if the
// path is missing, so a wrong path shows up as a bus of 0xFF rather than a
// silent zero-fill - which the 68000 would take as SSP/PC = 0.
builder.Memory
    .ROM(0x000000, 0x01FFFF, "ACCP EPROM")
    .LoadFile(@"E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin")
    .Ram(0x110000, 0x117FFF, "ACCP SRAM");
```

**No overlay is needed.** `TrackFastMemory` regions are installed into the `SystemBus` during
`MachineBuilder.Build()`, i.e. before the machine's first `Reset()`, so the 68000's SSP/PC fetch
from `0x000000`/`0x000004` sees real ROM. The MacIIci overlay dance exists because that machine
must *toggle* ROM-at-zero off later; the ACCP never does.

Two gotchas worth knowing:

- `LoadFile` **does not throw** on a missing file - it logs a warning and installs a 0xFF-filled
  region. `0xFFFFFFFF` as SSP and PC will fault in a confusing way, so the Phase 2 test should
  assert the reset vectors read back correctly before it asserts anything else.
- If the file size does not match the region, `LoadFile` resizes with 0xFF padding and warns. For
  `octo.bin` the sizes match exactly (131072 = 0x20000), so neither path should trigger.

---

### 2. The ACCP memory map - implement exactly this

All values proven from the firmware; see the companion doc for the evidence.

**The chip-select decode is nibble-replicated**: every peripheral sits at `0xNN0000` with NN a
repeated nibble (0x11 SRAM, 0x22/0x33/0x44/0x55/0x66/0x88/0xBB peripherals, 0xDD DUART). 0x44 and
0x55 were only found once 0x70CC was hand-disassembled - **assume further selects exist until
proven otherwise**, and make the stub installer cover the whole `0xNN0000` family rather than a
hand-listed set, so an unmodelled select shows up in the log instead of as a bus error.

| Range | Contents | Status |
|---|---|---|
| `0x000000-0x01FFFF` | ROM, 128 KB, `octo.bin` | Base address is an INFERENCE (standard 68000 arrangement, vectors point at low offsets). No schematic seen. |
| `0x110000-0x117FFF` | SRAM, 32 KB (4x 8192x8), two 16 KB halves | **PROVEN** - the reset routine walk-tests both halves and zeroes them |
| `0x00DD0000` + odd bytes | SCN2681 DUART, register N at `+2N+1` | **PROVEN** from SRA/THRA/SRB/THRB |
| `0x220000` | word - MF-bus COMMAND / PARAMETER port. High nibble = function, low byte = value. Seen `0x300F` open, `0x400A`/`0x400C` sub-function, `0x000F` strobe. | **CARVED** at 0x70CC (MF-bus, not octobus - its timeout branch loads `"$MF-bus memory timeout$"`). Function-code *meanings* are INFERENCE; read ND-14001 ch. 4 before naming them. |
| `0x440000` | word - MF-bus DATA, LOW half of a 32-bit value | **CARVED** |
| `0x550000` | word - MF-bus DATA, HIGH half (`swap D0` between the two writes) | **CARVED** |
| `0x660001` | byte - MF-bus STATUS, **bit 4 = transaction complete**, polled with a software countdown | **CARVED** |
| `0x330000` | byte - command port. Seen `0xF0` (master clear) and `0xD8` (send). | **CARVED** in the IRQ3 handler |
| `0x330001` | byte - **write-only control latch**, RAM shadow at `0x001144EF`; bit 1 pulsed | **CARVED** - never read back by the firmware, so a stub returning anything is fine, but the shadow must be modelled if you ever want to know the latch state |
| `0x880000` | word - **message / kick read port**. IRQ3 reads one word as the interrupt cause; IRQ7 drains it in a loop while `0x660001` bit 2 is set. | **CARVED** - it is both a cause register and FIFO-like, depending on the path |
| `0x770004` / `0x770007` | word data-in (from `0x440000`) + status byte, bits 3/4 handshake, retry count 10 | **NEWLY FOUND 2026-07-27** - was in no previous list |
| `0x660000` | byte status. **bit 0 = control-store operation OK** (0x744E). bit 3 tested (0x06A4), bit 5 tested (0x082E); whole byte snapshotted to `0x1143BA` | **bit 0 CARVED**, rest open |
| `0x900007` | byte, snapshotted to `0x1143B8` at interrupt time | **CONFIRMED REAL** - `0x90` is not a repeated nibble, so the nibble rule is a tendency, not a law |
| `0xBB0000` | word, written `0` in the IRQ7 path right before the restart | UNIDENTIFIED |

`0x660001` is now known to carry **three** unrelated bits: bit 1 = AOB busy, bit 2 = message
available at `0x880000`, bit 4 = MF-bus transaction complete. A stub must be able to drive them
independently.

**`0x220000` is a general command port, not "the MF-bus port".** The IRQ3 AOB path writes
`move.w #0x0005,(0x00220000)` and uses the same `0x440000` data port. The function code selects the
target: `0x300F/0x400A/0x400C/0x000F` = MF-bus memory, `0x0005` = AOB. Likewise `0x660001` is a
shared status byte - **bit 1 = AOB busy, bit 4 = MF-bus complete**. Model these as one command/status
block with per-function meaning, not as two separate devices.

**`0x220000` is BOTH a command port AND a shift clock - this is the key fact for Phase 6.**
A "bit-banged serial port" reading was floated, retracted, and then resolved properly (companion
§2.4h -> §2.4j -> §2.4n). Final position, all verified:

- **As a command port**: `0x71F8` writes four discrete command words and reads 32-bit results back
  from `0x440000`/`0x550000`. Definitely a parallel register interface.
- **As a clock**: three routines (`0x76E6`, `0x7776`, `0x77B6`) emit the pair `0x0010` / `0x000F`
  in tight loops to shift data. **Write vs read is distinguished purely by the phase order** of that
  pair - `0x0010` then `0x000F` shifts out, `0x000F` then `0x0010` shifts in.

**An emulator must tell "command word" from "clock edge" by context.** That makes this the value
table to work from: `0x0001`, `0x0005`, `0x0007`, `0x000F`*, `0x0010`*, `0x0015`, `0x0017`,
`0x2010`, `0x2011`, `0x2018`, `0x300F`, `0x3010`, `0x400A`, `0x400C`, `0x4016`, `0x8013`
(* = clock constants, not commands).

**`0x001144F0` is a 16-byte (128-bit) microword buffer** - exactly the ND-5000 microword width.
`0x7776` shifts it out to the control store; `0x77B6` shifts it back in. **`0x775A` - the shared
exit path of both control-store paths - is a read-back VERIFY**: it issues `0x2010` and shifts the
128 bits back into that buffer after every access. With stubs returning zeros the verify always
mismatches, which is the correct "no ND-5000 present" behaviour, and `0x001144F0` is a good place
for a test to inspect.

**`0x440000` / `0x550000` are a bidirectional 32-bit data pair** (low / high, `swap D0` between).
`0x71F8` writes both, issues four commands, then reads both back - so they are real readable
registers. A stub must return what was written, or the firmware's read-back path yields garbage.

**Two transaction gates and one strobe** - these are the bits an emulator has to honour:

| Bit | Behaviour |
|---|---|
| `0x330001` bit 6 | cleared for the duration of a 32-bit transaction, restored after |
| `0x330000` bit 0 | set for the duration of a 32-bit transaction, cleared after |
| `0x330000` bit 6 | **write strobe** for the AOB single-word path: set, whole byte written, then cleared *in the shadow only* - there is no explicit falling-edge write. Treat the write-with-bit-6-set as the commit edge for the word in `0x440000`. |

The AOB write path (`0x72A0`) spins on `0x660001` bit 1 until the AOB is free (unless `0x113138` is
non-zero), then runs the strobe at `SR = 0x2700` - masked against IRQ3/7. A stub that never clears
`0x660001` bit 1 will hang the firmware here rather than time out.

**Neither `0x33` byte is ever read back.** Both are write-only with RAM shadows -
**`0x001144EE` shadows `0x330000`**, `0x001144EF` shadows `0x330001`. So a stub's *read* value for
those two addresses is irrelevant, and a test that wants the latch state should read the shadows in
RAM, not the device.

**The card can be reset remotely over the octobus.** A kick matching mask `0xC0FF` = `0xC0FF` with
bits 13-8 equal to the guard word at `0x001143A0` makes the firmware pulse `0x330001`, write `0xF0`
to `0x330000`, spin 10000 iterations, and `jmp 0x00000C72` - re-entering init *after* the RAM test.
A stub that returns arbitrary values on `0x880000` can trip this by accident and silently restart
the machine mid-test. **Have the Phase 2 stub return 0 for `0x880000`, and assert the machine never
re-enters `0x0C72`.**

There are in fact **three** paths that end in `jmp 0x00000C72` (restart-without-RAM-test): the IRQ3
remote master clear, the IRQ7/NMI path (drain `0x880000`, clear `0xBB0000`, restart), and the
`0x660000` bit-5 branch at `0x082E`. A stub that returns `0xFF` for status bytes will hit these
immediately. **Default every stub read to `0`, not `0xFF`** - and make the "never re-enters
`0x0C72`" assertion a shared precondition of all Phase 2 tests, not just one.

Related: `0x00113146` and `0x00113136` are set to `1` on error paths (0x0780, 0x07DC) - candidate
extra oracles once their meaning is known.

**Correction to an earlier version of this document**: `0x220000` was listed as "written `1` on
entry to BOTH the IRQ6 and IRQ7 handlers", implying an interrupt-acknowledge register. That write
exists, but the select is the MF-bus command port - do not model it as an interrupt register.

A 32-bit MF-bus datum must move as a low/high word pair, which is exactly what the banner line
`Only 32-bit Word accesses available from ACCP to MF-bus!` is telling the operator. The canonical
sequence (from 0x70D0) is: three command writes (`0x300F`, `0x400A`, `0x000F`), data low to
`0x440000`, `swap D0`, data high to `0x550000`, three more command writes (`0x300F`, `0x400C`,
`0x000F`), then `btst #4,(0x00660001)`. A stub that never sets bit 4 of `0x660001` will make the
firmware print the timeout - which is a perfectly good Phase 2 assertion, and later the hook for a
real MF-bus model.

The remaining UNIDENTIFIED ranges are the **NDOBCON / OCTC octobus controller LSI** and the BADAP.
Do NOT guess their semantics. Install a **logging stub device** for each: record
(address, size, direction, value, CPU PC) and return a configurable fixed value. The stubs are the
instrument that identifies the chip - see section 5.

Reset values that fall out of ROM automatically: SSP `0x00113FFC`, PC `0x00000BD6`.

---

### 3. Package layout to create

**RULING FROM RONNY (2026-07-27) - not negotiable**: the whole ACCP machine is a **NuGet machine
package**. Put **nothing** in the legacy `Emulated.*` namespaces or projects, and wire the machine
up with **`MachineBuilder`** (the `Machines.Generic68K` / `Machines.MacIIci` shape), not by hand.

```
Nuget\HackerCorpLabs.Emulation.Machines.Accp\
  Directory.Build.props   LICENSE   README.md   TODO.md
  .github\   docfx\
  src\  HackerCorpLabs.Emulation.Machines.Accp.csproj
        AccpMachine.cs
        AccpMachineConfig.cs
        Devices\AccpLoggingStub.cs        <- the UNIDENTIFIED-range recorder
  tests\ HackerCorpLabs.Emulation.Machines.Accp.Tests.csproj
         AccpBootTests.cs
```

Copy the `.csproj` shape from
`Machines.Generic68K\src\HackerCorpLabs.Emulation.Machines.Generic68K.csproj` - it already lists the
right project references (Abstractions, Common, SystemBus, CPU.Base, CPU.MC68K, Machines.Base,
Debugger.Abstractions). Add `Chips.Motorola` once the DUART lands.

Machine attribute:

```csharp
[Machine(
    FolderName  = "Accp",
    WindowTitle = "Norsk Data Samson ACCess Processor (ND-324716)",
    Description = "ND-5000 access processor and octobus controller. MC68000, 128 KB EPROM, 32 KB SRAM, SCN2681 DUART console.",
    Id          = "accp",
    DisplayName = "ND ACCP (Samson)",
    Family      = "ND-5000",
    Vendor      = "Norsk Data",
    Tags        = new[] { "nd", "norsk-data", "nd5000", "samson", "accp", "octobus", "mc68000" })]
```

Also add the machine to `Nuget\Tools\Sdl2CliDemo\Sdl2CliDemo.csproj` so `machine.start accp` works
(see skill `cli-attach-machine`).

---

### 4. Phased build - each phase ends with a green build AND green tests

Project rules that apply throughout: **no LINQ, no `foreach`, no FluentAssertions**, prefer `Span` /
`ArrayPool`, keep and add as many comments as possible, run `dotnet format` if whitespace complains,
and **never report success without actually running the tests**.

#### 2026-07-28 - THE CARD IS INTERACTIVE ✅ `ACCP:` PROMPT REACHED

The firmware boots, runs its full selftest suite, prints `ACCP:` and **answers typed commands**:

```
ACCP: HELP
Command:
```

**28/28 ACCP tests, 15/15 NDOBCON, 179/179 Chips.Motorola.**

Three things had to be right, and two of them were wrong first:

1. **`NDOBCON`** (`Nuget\HackerCorpLabs.Emulation.Chips.NorskData\src\OBCON\NDOBCON.cs`) — the
   OCTObus adapter gate array. Transmit is FAKED (frames counted, recorded, dropped; transmitter
   always ready) because the firmware's TX poll has no timeout. Requests complete after a
   configurable delay via `Tick()` + `AutoReplyDelayTicks`; `ReplyBuilder` is the seam for a real
   peer. Without the auto-reply the boot stalls in a software poll at 0x6C42 waiting on two OBCON
   request-block status words and never reaches the prompt.

2. **THE DUART INTERRUPT IS ON IRQ5, AND WIRING IT IS NOT OPTIONAL.** Proven: vector 29 at ROM
   `0x074` -> `0x796` -> `0x1E0C`, which writes 0 to `0xDD000B` (IMR), reads it back (register 5 =
   ISR) and tests bits 1/5/0/4 = RxRDY A/B, TxRDY A/B. **The firmware NEVER polls the receiver** —
   it reads characters only from that handler. With the interrupt unconnected the banner and prompt
   print perfectly and every keystroke is silently discarded. That failure mode looks exactly like a
   working machine, so `Prompt_EchoesAndAnswersTypedCommand` exists as its gate.

3. **A live status port**, not a static stub, for `0x660000`/`0x660001` — the receive-available bit
   has to track the real FIFO or an auto-reply is never collected.

**Method warning worth carrying**: "run until the console stops growing, then type" is a BAD way to
detect the prompt. The selftest suite has natural gaps, so it fires mid-suite, pokes a busy machine
and reports it dead. That produced two confidently wrong "the console does not respond" findings
before it was caught. Wait for the `ACCP:` marker instead.

#### Phases 1 and 3 - COMPLETE 2026-07-28 ✅ THE CARD BOOTS

**Phase 1 — SCN2681 DUART.** `Nuget\HackerCorpLabs.Emulation.Chips.Motorola\src\DUART\SCN2681\`
(`SCN2681Duart.cs`, `SCN2681Registers.cs`), byte-level serial, semantics cross-checked against
MAME `mc68681.cpp`. **29 new tests; 179/179 pass in the package.** The centrepiece replays the
firmware's real `DuartInit` @0x162E register for register and asserts 9600 7E2 on channel A,
9600 8N1 on channel B, IMR 0x22, counter preload 0x9000 and the counter stopped.

**Phase 3 — console.** DUART attached at `0xDD0000` through
`GappedAddressDecoder(shift: 1, mask: 0xF)`. **20/20 ACCP tests pass** and the real firmware
prints:

```
******   S A M S O N   A C C E S S   P R O C E S S O R   ******
ACCP local ram test OK
BUS test  failed          Result: 00000000H   Expected: 1C587698H
MIR test a failed         Expected: 7698H B027H 0AAAH 2C91H 0D8CH F58BH AFBEH 6195H
Control Store  sample test ab failed
Start/stop microprogram test abc failed at CSA: 00FFH
A,MARG D,AIB test  failed Result: 00000000H   Expected: FFFFFFFFH
Loading control store with selftests...
ALU verify test  failed   Result: 00000000H   Expected: 87654322H
```

Every selftest failing is the CORRECT result — they target the ND-5000, which is not modelled.
What matters is that the firmware runs the whole suite and reports it.

**THE KEY IMPLEMENTATION FACT — there are FOUR unbounded ready-polls, not two.** Section 0a
lists two; running it found two more. Each stops the boot at a different point, and the symptom
is always "the console just stops", never an error. See the octobus doc §5a for the table and
the measured progression (513 -> 608 -> 1769 chars of console output as each is released).

Bits that must be left LOW are as important as the ones raised: `0x660001` bit 1 (AOB busy),
`0x660000` bit 0 (**control-store OK — raising it fabricates a selftest pass**) and `0x660000`
bit 5 (restart path). And `0x660001` bit 2 held high gets the boot *less* far, not further.

Implemented as `AccpMachineConfig.Hold*` flags, all defaulting true except the receive one, with
`AccpMachine.ApplyHandshakeBits` composing the shared `0x660001` byte rather than letting the
last writer win.

**Remaining Phase 3 items need a command prompt** (`HELP` -> 43 commands,
`LOOK-AT-LOCAL-MEMORY`, `MAIN-FORMAT`). The firmware is still inside its selftest suite at the
end of the run; reaching the prompt needs either a longer budget or enough of an ND-5000 model
to let the tests pass. That is Phase 5/6 work.

#### Phase 2 - COMPLETE 2026-07-28 ✅

Built and validated. **10/10 tests pass** in
`Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpBootTests.cs`.

Files created:

```
Nuget\HackerCorpLabs.Emulation.Machines.Accp\
  Directory.Build.props
  src\HackerCorpLabs.Emulation.Machines.Accp.csproj
  src\AccpMachine.cs
  src\AccpMachineConfig.cs
  src\Devices\AccpLoggingStub.cs
  tests\HackerCorpLabs.Emulation.Machines.Accp.Tests.csproj
  tests\AccpBootTests.cs
```

Verified at instruction level on the real `octo.bin`:

- Reset vectors read back SSP `0x00113FFC` / PC `0x00000BD6`, and the CPU actually takes them.
- The RAM walk-test completes with **both error counts zero** and the flag set - i.e. the MC68K
  core, the ROM mapping at `0x000000`, the reset-vector fetch and the 32 KB SRAM at `0x110000` are
  all correct.
- No 68000 exception fires (`0x00113112` stays 0).
- The card does not take any of the three restart paths.
- SRAM round-trips through the bus; ROM reads the image and correctly ignores writes.

**The boot runs far past init unaided** - with only stubs present the firmware reaches the
control-store shift loop at `0x77A6`. First census, to ~8M instructions:

| Select | Accesses |
|---|---|
| CMD-22 | 5,781,124 |
| DATA-HI-55 | 313,442 |
| LATCH-33 | 34,844 |
| XFER-77 | 6 |
| DATA-LO-44 | 4 |
| MISC-90 | 2 |
| STATUS-66 | 1 |
| MSG-88, CTL-BB | 0 |

The `CMD-22` / `DATA-HI-55` traffic is the microword shift engine (§2.4n) spinning - expected with
no ND-5000 present. Note `MISC-90` is non-zero, which independently confirms `0x900007` is real
(§2.5).

#### Phase 2 original plan (kept for reference) - machine + memory map, no DUART

Build `AccpMachine` with ROM, SRAM, and a logging stub on every non-SRAM, non-DUART `0xNN0000`
select (0x22, 0x33, 0x44, 0x55, 0x66, **0x77**, 0x88, 0xBB, plus whatever `0x900007` really is -
and be generous, see section 2: the list has grown twice already, first 0x44/0x55, then 0x77).
No chip.

**Test 1 - the RAM walk-test is the oracle.** Boot and run until the reset routine finishes, then
assert:

| Address | Width | Expected | Meaning |
|---|---|---|---|
| `0x0011312A` | 32-bit (`move.l`) | `0` | first-half RAM error count (D2) |
| `0x0011312E` | 32-bit (`move.l`) | `0` | second-half RAM error count (D3) |
| `0x00113132` | 16-bit (`move.w`) | `1` | RAM test completed flag |

All three re-verified by disassembly on 2026-07-27 (`0x0C5A` / `0x0C60` / `0x0C66`) - the widths
were previously unstated and are not uniform.

> **CRITICAL CORRECTION 2026-07-28, found by running it: `0x00113132` IS TRANSIENT.**
> The firmware sets it to 1 at `0x0C66` and something later in init clears it again. Measured on
> the real image: set at single-step **139,274**, and back to `0x0000` well before the boot settles
> (by which time the CPU is deep in the control-store shift loop at `0x77A6`).
>
> So the obvious implementation - "run the machine for a while, then assert the flag is 1" - reports
> **"the firmware never completed its RAM test" on a perfectly healthy boot**. That is exactly what
> the first version of the test did, and it cost a debugging cycle.
>
> **Read the verdict at the moment it is published**: single-step until `PC == 0x00000C72` (the
> instruction right after the three stores), then assert. That address is reached after ~139,276
> steps and is deterministic. The two error counts happen to survive, but read them at the same
> stop point anyway.
>
> This also gives the restart test its teeth for free: arriving at `0x0C72` *with the flag still
> clear* means the firmware got there via one of the three restart paths rather than by finishing
> the walk test.

Prerequisite assertion for this test: check the reset vectors read back as SSP = `0x00113FFC` and
PC = `0x00000BD6` *before* running. Both are confirmed against `octo.bin` bytes 0-7
(`00 11 3F FC 00 00 0B D6`), and `LoadFile` fails soft on a bad path, so this catches a mis-pathed
ROM immediately instead of as a baffling fault.

Note the reset SSP is **never used** - the walk test makes no subroutine call, and the zero-fill at
`0x0C4A` wipes the whole 32 KB including `0x113FFC`. The real stack is set to `0x112000` at
`0x0C7A`, after the test. Do not "fix" a test failure by preserving the boot stack.

**Better still, breakpoint `0x00000ECE`.** That is the firmware's own `tst.l (0x0011312A)` /
`bne 0x0EFC` - the branch that decides between the `ACCP local ram test OK` message and the failure
report. Asserting on which way that branch goes tests the firmware's verdict directly, without
depending on console plumbing that does not exist until Phase 3. It also independently confirms the
32-bit width of the error count, since a different routine reads it with `tst.l`.

That single test validates the MC68K core, the ROM mapping, the reset-vector fetch and the SRAM
mapping at once, with no peripheral in the way.

**Test 2 - no unexpected exception.** Assert the fault code word at `0x00113112` is untouched. Any
68000 fault writes a code there (`0x20` unused TRAP, `0x2A` TRAP #10, `0x4D` reserved vector) plus
SR/SP/PC/A6 at `0x113118`/`0x11311E`/`0x113122`/`0x113126`. If a test fails, **read that block
first** - it says exactly which exception fired and where.

**Test 3 - stub access census.** Assert the recorded stub hits match a golden list. This is how you
notice the firmware reaching for hardware you have not modelled.

#### Phase 1 (second) - the SCN2681

Port the SCN2681 variant only from `mc68681.cpp`. Skip `sc28c94_device`, `mc68340_duart_device`,
`xr68c681_device`, `mcf5206e_uart_device` - most of that 47 KB is other parts.

Registers 0-0x0F: MR1/MR2 A+B (with the MR pointer auto-advance), SRA/SRB, CSRA/CSRB, CRA/CRB,
RHR/THR A+B, ACR, ISR/IMR, CTUR/CTLR, IPCR, OPCR, SET-OPR / CLR-OPR, plus the 16-bit counter/timer
and the interrupt output. Receiver FIFO is 3 deep.

Decorate per skill `retrocore-chip-cli-decoration`: `[Chip]`, `[ChipRegisterMap]` + register enum
with `[Description]`, `[ChipRegisterBits]` + `[Flags]` bit enums (use `1<<n` style, on-wire hex in
the XML summary), correct `RegisterAccess` semantics, and a `PeekPort` override so debug reads do
not consume RHR or clear status.

Tests: register read/write, RxRDY / TxRDY / TxEMT transitions, FIFO depth 3 and overrun, break
detect, counter/timer modes, ISR/IMR masking, and the reset commands in CRA/CRB.

**The firmware's own power-on programming is the best conformance test you will get.** `DuartInit`
@ `0x162E` was fully carved on 2026-07-27 (companion doc §2.2b) and touches 11 distinct registers
in a fixed order. Write one test that runs the machine to the end of `DuartInit` and asserts the
resulting chip state:

| Property | Expected after `DuartInit` |
|---|---|
| Channel A line settings | **9600 baud, 7 data bits, even parity, 2 stop bits** (MR1A=`0x02`, MR2A=`0x0F`, CSRA=`0xBB`, ACR=`0xE0` -> baud set 2, index 0xB) |
| Channel B line settings | **9600 baud, 8 data bits, no parity, 1 stop bit** (MR1B=`0x13`, MR2B=`0x07`, CSRB=`0xBB`) |
| Both channels | RX enabled and TX enabled (`CRA`/`CRB` = `0x05`) |
| IMR | `0x22` - RxRDY-A + RxRDY-B **only** |
| CTUR/CTLR | `0x90` / `0x00` (preload `0x9000`) |
| Counter | stopped (init *reads* `0xDD001F` = stop-counter command) |

Baud indices verified against MAME `mc68681.cpp:84` (`baud_rate_ACR_1[0x0B] == 9600`); register
indices against the write-path `case` labels at `mc68681.cpp:953-1025`.

**Two behaviours the model must get right or the console will silently misbehave:**

1. **MR pointer auto-advance is load-bearing.** Init writes MR1A then MR2A through the *same*
   address `0xDD0001`, and MR1B/MR2B through `0xDD0011`, relying on the pointer advancing after the
   first write and on `CRA/CRB` command 1 resetting it. Get this wrong and you get the wrong
   character length and parity - which produces plausible-looking garbage on the console instead of
   an obvious failure. Test it explicitly.
2. **TX must not raise interrupts.** IMR = `0x22` masks `INT_TXRDYA` (0x01) and `INT_TXRDYB`
   (0x10); the firmware polls SR bit 2 instead (`DuartTxServiceBothChannels` @ `0x1D4C`). A model
   that asserts IRQ on TxRDY will interrupt firmware that is not expecting it.

**Channel B is initialised AND enabled at power-on**, at 8N1 - a data setting, not a terminal
setting. That is now strong (still not conclusive) support for the ND-100-serial-link reading in
Phase 3; it also means leaving channel B unconnected must not fault.

#### Phase 3 - console

Attach the DUART via `GappedAddressDecoder(duart, 0xDD0000, 0xDD001F, shift: 1, mask: 0xF)`.
Channel A to the CLI console (`IInputMachine`, skill `retrocore-keyboard-input`).
Leave channel B unconnected - it is LIKELY the ND-100 serial link (command
`SET-SERIAL-LINE <Enable ND100-communication via serial line ? (y/n)>` and the string
`Illegal kick ... received over serial line`) but that is NOT proven.

**Tests, using the firmware's own output:**

1. Banner: console receives `****** S A M S O N   A C C E S S   P R O C E S S O R ******`.
2. `ACCP local ram test OK` appears.
3. Type `HELP` and assert all **43** commands come back (list is in the companion doc, section 5).
   This one test exercises RX, the command scanner, the string engine and TX together.
4. `LOOK-AT-LOCAL-MEMORY <addr>` returns bytes that match `octo.bin` at that offset.
5. `MAIN-FORMAT` accepts `HEXADECIMAL` / `DECIMAL` / `OCTAL` and the output radix changes.

Note for the console driver: `$` (0x24) is the ND newline marker inside firmware strings; the
firmware turns it into CR LF itself (`ConsPutCrLf` at `0x1D32`), so the host sees ordinary CR LF.

#### Phase 5 - carve OBCON, then Phase 6 - implement it

Covered in the companion doc, section 8. In short: read `ND-14001-1-EN DOMINO Standard Hardware
Description` chapter 4 (OBCON / OCTObus Adapter) and `ND-05.017.01` section 3.4 FIRST - a documented
register map beats a carved one - then drive `SEND-OCTOBUS`, `RECEIVE-OCTOBUS`,
`SEND-KICK-OCTOBUS`, `READ-AIB16/32`, `LOAD-AOB16/32`, `READ-ACCP-STATUS` and `TEST-BUSLOOP` against
the logging stubs and read the address map out of the trace.

**Head start, 2026-07-27**: the IRQ3 handler (`0x0510`) is already carved - see companion doc §2.4b.
It is the KICK / AOB path, named by the firmware's own strings (`K I C K   T I M E O U T :`,
`AOB not read by microprogram within timeout.`, `AOB full, previous message not read.`). "microprogram"
= the ND-5000 microcode, so **AOB = ACCP -> SAMSON**, **AIB = SAMSON -> ACCP**, which lines up with
the `LOAD-AOB` / `READ-AIB` command names.

**Do this before anything else in Phase 5**: `0x001143B4` and `0x001143B6` are trace-enable flags.
While non-zero, the firmware prints each kick value followed by ` from SAMSON` / ` to SAMSON` on the
console. Find the command that sets them - then the firmware narrates its own octobus traffic, and
you get a console-visible oracle for every stub interaction essentially for free. That is a far
better instrument than reading the stub log.

**Both flags are explicitly cleared at boot** (`clr.w` at 0x0E9A and 0x0EA8), so tracing is off by
default and there *is* a command that turns it on. **Do not go looking for it in the current Ghidra
database** - see the next paragraph.

**Prerequisite for any further firmware carving - run `PlancFixFlow` first.** Large parts of this
image (including `0x5D00-0x6882`, where the trace-flag writes live) show as undefined bytes. The
bytes are fine; Ghidra stops disassembling at every PLANC error slot (`4E D5` = `jmp (A5)`) because
it reads as a flow terminator, and PLANC puts one after *every* call. `PlancAnnotate` has been run
on `octo.bin` but `PlancFixFlow` has not. Run the ND.PLANC script set in the patched Ghidra install
(`C:\Utils\ghidraRun.bat`) before spending time here - it will likely surface more code, and
possibly more chip selects, than any amount of manual work. See companion doc §2.4f.

Also useful: the AOB busy-wait timeout counter is loaded from `0x001131DC`, so the timeout is a
tunable RAM word rather than a hard-coded constant - handy for keeping tests fast.

**`0x330001` latch model (carved 2026-07-27, companion §2.4e).** The latch is written in a
**two-phase** pattern: first an intermediate value with bits 1 and 3 forced low, then the real
value. A stub or model must accept that intermediate write without treating it as a state change.
`0x795A` clears latch bits 2+3 (disable), `0x79E4` sets them (enable) - they are a matched pair, and
`0x795A` is *not* the big "controller re-init" routine it looked like from the call sites.

**`0x001131F8` is a variant/identity word that changes behaviour**: `0x795A` clears latch bit 0 only
when it holds `0x5400` or `0x5500`, and the firmware prints it to the console at 0x10D4. If the
emulator ever needs to pick a value here, that choice is observable both on the console and in the
latch - so it must be a deliberate, documented config value, not a default of zero. What `0x5400` /
`0x5500` actually mean is NOT established.

**Naming caution**: Ronny calls the part NDOBCON. The existing repo analysis
(`SINTRAN\ND5000\OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md`, line 179)
says the ND-5000-side equivalent of OBCON is the **OCTC** gate array on the ACCP, "the same OBCON
family chip". Settle which name and which datasheet applies to THIS card before assuming ND-14001
chapter 4 describes it verbatim.

---

### 4z. WHAT AN MFbus CONTROLLER MODEL MUST DO [for the octobus machine work, 2026-07-30]

A working MFbus-controller peer now exists as a test double and the exchange is fully
decoded. Anyone building the octobus side needs the following, because the ACCP will not
come up clean without it.

#### The CPU model is a CONFIGURATION value held by the MFbus controller

Not a memory type, and not something the CPU reports about itself. The MFbus controller
stores "which ND-5000 model is this system", and the ACCP **cross-checks it against its own
hardware probe**. The firmware's complaint says so directly: `MFbus controller has incorrect
CPU model setting.` A second string, `Not allowed for this CPU model: ND-`, shows the model
gates other behaviour too.

Reported model, computed at 0x12F4 from reply content byte 1:

```
model = 0x5000 | (contentByte1 << 8)
```

so 2 -> ND-5200, 4 -> ND-5400, 5 -> ND-5500, 7 -> ND-5700, 8 -> ND-5800, 9 -> ND-5900.

The ACCP then derives a **class** by probing `0x00114550` for `0x7F55` at `+6`, `+0x0C`, `+4`
and requires the reported model to be in that class's set:

| Class | `0x1131F6` | Accepted models |
|---|---|---|
| 1 | 1 | `0x5200` |
| 2 | 2 | `0x5400`, `0x5500`, `0x5700` |
| 3 | 3 | `0x5800`, `0x5900` |

`0x1131FA` is the accept bit. If it stays zero, `0x120C` sets bit 15 of `0x1131E2`, and the
error is printed. **A peer cannot claim an arbitrary model.**

#### What fills `0x00114550` - CARVED, implementable [2026-07-30]

`0x00114550` is **not** shared MPM memory, **not** a thumbwheel, and must **not** be written
directly by an emulator. It is a 16-word buffer in the ACCP's own local SRAM
(`0x110000`-`0x117FFF`), and the firmware **clears and rebuilds it** from the ND-5000 datapath
every time. Seeding it would be overwritten.

> **[SUPERSEDED 2026-07-31 - EVERYTHING FROM HERE TO THE END OF SECTION 4z IS BUILT ON A
> WRONG PREMISE. DO NOT IMPLEMENT IT.]**
>
> The builder has **FOUR** phases, not three. After the transpose ends at `0x7DCE`, a fourth
> phase at `0x7DD0`..`0x7EA4` rewrites every matrix word in place - `bit11 := bit10`, field
> moves, and a 7-bit **Gray decode** by the helper at `0x7CA2` - and the class chain reads
> THAT. So:
>
>  - the `read[w] bit s` derivation below, and the `0x7F55` bit pattern it produces, describe phase 3 only and yield `0x7A59` in the real matrix. The model is then REFUSED. This was measured, not theorised.
>  - the closing instruction "model what `0x00220000` returns on sixteen successive word reads" is right in shape but wrong in content if you use the numbers below.
>  - the `[OPEN]` note near the end - "phase 3 is followed at `0x7DD0` by field extraction ... only partly decoded and is not needed to satisfy the model check" - is wrong on both counts. It IS decoded, and it IS needed.
>
> Correct procedure: invert phase 4 per word, THEN invert the transpose. See **part 5 of
> `ACCP-COMPLETE-REFERENCE.md`**. That path is live-verified - class 2 established, ND-5500
> accepted, `0x1131FA = 1`.
>
> Kept verbatim because the phase 1-3 carve, the addresses and the class-chain reading are all
> still correct and were the foundation the phase-4 work was built on.

The builder is the routine entered at **`0x7D26`** (`link.w A6,-0x4c`). Three phases:

**Phase 1 - clear (0x7D2E..0x7D4C)**

```
for (off = 0; off <= 0x1E; off += 2)
    word16[0x114550 + off] = 0
```

**Phase 2 - sixteen sequential reads of the command port (0x7D4E..0x7D74)**

```
for (w = 0; w < 16; w++)
    read[w] = read_word16(0x00220000)     // sixteen separate reads, same address
```

**Phase 3 - 16x16 bit transpose (0x7D76..0x7DCE)**

```
for (bit = 0; bit < 16; bit++)
    for (w = 0; w < 16; w++)
        matrix[bit] bit w  =  read[w] bit bit
```

Verified from `0x7D7E` (`D2 = read[w]`, `btst bit,D2`) and `0x7DA2`
(`A0 = 0x114550 + bit*2`, `bclr/bset w`). So **`matrix[b]` is the b-th bit of every read,
gathered into one word**, and matrix word index `s` is byte offset `s*2`.

#### Consequence: exactly what the reads must contain

Both consumers index the SAME matrix, so their requirements are computable rather than
guessable.

**CPU-model class** (`0x110A`) tests `matrix[3]`, `matrix[6]`, `matrix[2]` - byte offsets
`+6`, `+0x0C`, `+4` - against `0x7F55`:

```
if (matrix[3] != 0x7F55)                       -> class 3, accepts ND-5800 / ND-5900
else if (matrix[6] == 0x7F55)                  -> neither class established, always rejects
else if (matrix[2] == 0x7F55)                  -> neither class established, always rejects
else                                           -> class 2, accepts ND-5400 / ND-5500 / ND-5700
   (the full-match path at 0x1150 gives class 1, accepts ND-5200 only)
```

Because `matrix[s] bit w = read[w] bit s`, requiring `matrix[s] == 0x7F55` means:

```
read[w] bit s must be 1 for w in {0,2,4,6,8,9,10,11,12,13,14}
read[w] bit s must be 0 for w in {1,3,5,7,15}
```

(`0x7F55` = `0111 1111 0101 0101`.)

**So for the emulator's current all-zero reads, `matrix[3] != 0x7F55` holds and class 3 is
chosen - which is why only model digits 8 and 9 can ever be accepted today.** That is correct
behaviour, not a bug. To reach class 2 or class 1, bit 3 (and then bits 6 and 2) of the
sixteen reads must follow the pattern above.

**ECO levels** (`0x9F78`, reached from `Cmd1F_ReadEcoLevels` @0x9F12) read the same matrix:

```
eco(s) = (matrix[s] >> 11) & 0x0F          // 0x9FBA: asr #11, and #0x0F
```

and `0x9FC6` treats **`0x0F` as "absent"** - it prints `00` instead of a level. Expanded
through the transpose:

```
eco(s) = (read[11]>>s & 1)
       | (read[12]>>s & 1) << 1
       | (read[13]>>s & 1) << 2
       | (read[14]>>s & 1) << 3
```

`Cmd1F_ReadEcoLevels` walks ten selectors in this order: **0, 1, 2, 4, 5, 8, 6, 0x0C, 0x0D, 3**,
labelling each from a 12-byte descriptor table at `0x00012D5C`.

**[INFERENCE - consistent, not proven]** `0x7F55` looks like an **"absent / invalid" sentinel**:
its bits 11-14 are all ones, which is exactly the `eco == 0x0F` "absent" case, and
`Selftest_ProbeCacheAndAap_B` (`0xF28E`, `0xF2DE`) uses the same `0x7F55` test to decide
whether to print a message instead of running a test. Reading it as "slot empty" makes the
model-class chain read naturally: word 3 not empty means class 3, and so on.

**[OPEN]** Phase 3 is followed at `0x7DD0` by field extraction that copies matrix bit 10 into
bit 11, then splits each word into `& 0x7800`, `(w << 3) & 0x700`, `(w >> 3) & 0x60` and
`w & 0x1F` before calling `0x7CA2`. That repacking is only partly decoded and is not needed to
satisfy the model check.

#### The one-line instruction for an implementer

Model what **`0x00220000` returns on sixteen successive word reads** during the routine at
`0x7D26`. Do not touch `0x114550`.

#### Straps and thumbwheels - the complete list [SWEPT 2026-07-30]

**The 5616 has no thumbwheel switches.** The board documentation records `Switches: None` and
`LEDs: None`, with only five ECO-level straps. ND-14001 section 4.8.1 explains why: thumbwheels
are for **global** OCTObus nodes, while **local** nodes - which the ACCP is, sitting on the
MFbus backwiring - are initialised by the MFbus controller writing their on-board WOI register.

A full sweep of the `0x90xxxx` select finds only **two** addresses touched by code:

| Address | Read at | Use |
|---|---|---|
| `0x00900001` | `0x0B4A` in `BootInitAndErrorRouting`, `0x1230` in `MfBusControllerConfigCheck` | **Station / configuration register.** At `0x1230` masked with `0x1F` (5 bits) to give the ACCP's own OCTObus station number for the discovery payload. At `0x0B4A` the whole byte is shifted left 8, OR'd with the byte at `0x001143B8`, and passed to `0x72A0`. |
| `0x00900007` | `0x07D4` in `Vec30_AutoIrq6`, `0x7C04` in `CmdPortWithLatchGate` | **Not configuration.** Interrupt and latch gating. |

**`0x900001` is almost certainly the WOI/STANO register the MFbus controller writes, not a
strap.** The 5-bit mask matches WOI's STANO field width exactly, and the board has no switches
to read. **[INFERENCE, and it matters]** if that is right, a correct emulator has the MFbus
controller write this register during crate configuration, *before* the ACCP boots - and the
discovery scan would then never need to run at all. Today the emulator returns **1**, which is
the ND-120 CPU slot and not a legal local-node number (local nodes are 20-77 octal).

**ECO levels are not straps read by the firmware.** `Cmd1F_ReadEcoLevels` @`0x9F12` reads them
out of the `0x114550` matrix, not from any port - see the eco(s) formula above. So there is no
strap address to model for them either.

**False positives to ignore**: a byte search for `009000??` also hits `0x1704` in `DuartInit`
(which is `move.b #0x90,(0x00DD000D)` - a DUART write, not a `0x90` select) and a dozen offsets
above `0x14000`, which are in the string and descriptor region.

#### Reuse what exists - do not reimplement the protocol

`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Chips.NorskData\src\OBCON\ObconMessage.cs`
already has `ObconFrame` (compose/decode), `ObconMultibyte` (`Encode` / `TryDecode`),
`ObconInformationBits`, `ObconMessageType` and `ObconReceiveBuffer` (buffer offsets,
registration stride, the CMD constants). On the ND-100 side there is also
`OctobusFabric` / `IOctobusStation` / `OctobusStationBase` / `OctobusMultibyteCollector`, with
`OctobusStationType.MFbus = 2` already defined and two worked examples
(`OctobusND5000Station`, `OctobusScsiDiocStation`).

#### Checklist for the peer

- Answer on **CMD 5**, and be **registered** for it - an unconnected CMD is never delivered.
- Reply content byte **0 = 0x00** (status good). `0xFF` instead makes byte 1 an error code:
  1 = keep scanning, 2/3/4 = print a specific complaint.
- Reply content byte **1 = the model digit**, consistent with the signature table.
- Sit in the station range **2-7**; the ACCP scans exactly those.
- **The direction rewrite is the classic trap.** On transmit bits 13-8 are a DESTINATION; on
  receive they are a SOURCE. A reply must be composed with the PEER's station in that field.
  Composing it with the ACCP's station looks correct in a log and then silently fails
  MFCRECEIVE's source test.
- Disable the OBCON loopback (`AutoReplyEnabled = false`). The default echo ends the scan
  while carrying no real information, and it masks a real peer.

#### Two defects this surfaced

1. ~~**[BLOCKER] Only the first content byte of a reply reaches the driver's receive buffer.**~~
   **SOLVED 2026-07-30, RetroCore commit `dbdc291e5` - and the diagnosis below was wrong.** The
   real cause was **interrupt presentation**: `Run()` sampled `UpdatePendingInterrupt()` once per
   1024 instructions, and that function picks one highest source with `else-if`, so octobus IRQ3
   was never presented while the DUART asserted IRQ5. The receive ISR takes **one frame per
   interrupt**, so a ten-frame reply needs ten interrupts, and MFCRECEIVE abandons after 10000
   polls. The buffer was never truncated - the write pointer was still at its initial 5, so ZERO
   content bytes had been appended, and byte 5 read `0x00` because the buffer was untouched.
   **Also: never sample that buffer at end of run** - the firmware clears the reassembly record on
   close and re-initialises all 64 per-station records and 16 registration entries at `0xF4E6`.
   Superseded original text follows.

   **[SUPERSEDED] Only the first content byte of a reply reaches the driver's receive buffer.**
   Dumped live from the buffer at registration entry 5 (`0x00112D40`, data area `0x00112D54`)
   after a six-byte reply: `byte1=0x02` source, `byte3=0x05` own CMD, `byte4=0x06` length all
   correct, `byte5=0x00` content[0] - and then **zeros**. Content bytes 1 upward never arrive,
   so the model digit cannot get through. This is a receive-path bug, and it is why the model
   cross-check cannot yet be satisfied.
2. **`0x900001` returns station 1.** Better than the old zero, but station 1 is the ND-120 CPU
   slot. The ACCP sits on the local octobus and should be 20-77 octal. Worth confirming
   whether that value is deliberate.

#### Harness to build against

`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpMfBusControllerPeer.cs`
and `...\tests\AccpMfBusDiscoveryTests.cs`. Six tests pass; the three model-digit cases are
`[Ignore]`d with defect 1 as the stated reason. `Diag_CpuModelCrossCheckState` dumps the
signature table, `0x1131F6`/`F8`/`FA`, the registration entry and the buffer - use it rather
than reasoning from console output, which cannot distinguish "wrong digit" from "no class
established".

Full carve: section 1c of
`E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-OBCON-PROTOCOL-AND-ACCP-DRIVER-2026-07-27.md`.

---

### 5. Explicitly out of scope for now

- Connecting the ACCP machine to the ND-100 side or to `NDBusOctobus`. Get the card alive standing
  alone first, then bridge.
- The control-store / microprogram commands. `LOAD-CONTROL-STORE` and friends talk to an ND-5000
  CPU that is not present; expect them to time out, and that is correct behaviour for now.

  **Update 2026-07-27 - the register-level path is now identified, and it fails cleanly.** The
  region `0x71F8-0x7C14` is the control-store loader (companion doc §2.4l); `0x741E` prints
  `CONTROL STORE ERROR in buffered CI-bits 35 or 40.` and returns -1 when
  **`0x660000` bit 0** (operation OK) is clear. So with the Phase 2 stubs returning 0, every
  control-store operation reports a clean error rather than hanging - exactly the "expect them to
  time out" behaviour assumed above, now confirmed at instruction level rather than hoped for.

  Two levers worth knowing: **`0x001131E2`** is the sticky error latch (set to -1), and bits 10..8
  of **`0x00114560`** are a message-level threshold - the error text is only printed when that
  field is >= 1, so a test can suppress or force the diagnostic.

  **There are TWO control-store paths, not one** (companion §2.4m). `0x741E` and `0x764E` are
  near-identical - same `0x0018` command, same `0x660000` bit-0 success test, same error latch and
  string - and differ only in which `0x330000` gate bit they set (**bit 2** vs **bit 1**) and which
  status word they consult (**`0x00114560`** vs **`0x0011455C`**, 4 bytes apart). That lines up with
  the error text naming two positions, "buffered CI-bits 35 **or** 40": two buffered
  control-instruction bit groups, selected by the gate bit.

  Gotcha for tests: the two paths do **not** report identically. `0x741E` complains when the level
  field (bits 10..8) is >= 1; `0x764E` additionally requires `(status and 0x1F) > 3` when the level
  is exactly 1. Same stub state can therefore produce a diagnostic from one path and silence from
  the other - do not assert they behave the same.
- Bit-level serial. Byte-level was chosen deliberately.

### 6. Related

- `SINTRAN\ND5000\part 1 of `ACCP-COMPLETE-REFERENCE.md` - all firmware facts
- `Installation\Communication\OctobusAccp\` - the image, the EPROM dumps, the interleave README
- `SINTRAN\ND5000\OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md`
- `SINTRAN\ND5000\OCTOBUS-TEST-PROTOCOL-RE.md` - the OMD-0 protocol this card answers
- Skills: `retrocore-machine-integration`, `retrocore-chip-cli-decoration`, `retrocore-csharp`,
  `retrocore-cpu-test`, `cli-attach-machine`, `retrocore-keyboard-input`, `octobus-nd5000`

---

# Part 3 - originally `ACCP-MACHINE-DEFECT-REPORT-2026-07-28.md`

> Merged verbatim 2026-07-31. Content is byte-identical to the original;
> only heading levels were demoted by one so the parts nest under this file.

## ACCP machine - defect report from the first full boot log

**Date**: 2026-07-28
**Subject**: the RetroCore ACCP machine, which now boots the real firmware to the `ACCP:` prompt
**Evidence**: the firmware image `octo.bin` and the Ghidra database; every claim below is read
from instruction bytes or computed, and says which.

Reaching the prompt is a real milestone. This lists what the log shows is still wrong, ranked
by how much damage it does if left.

---

### D1 ~~[CRITICAL]~~ **FIXED AND GUARDED** - the four "completed OK" tests were FALSE PASSES

> **STATUS 2026-08-01: FIXED. Do not re-open.** The fix is exactly option 1/2 below:
> `AccpMachineConfig.AbsentDeviceDataValue` defaults to **`0xFF`**, so an absent ND-5000 reads as
> all-ones instead of a clean zero, and the four vacuous passes are gone.
>
> **Guarded by a PAIR of tests, deliberately:**
>  - `Selftest_ReportsNoFalsePasses` - asserts `completed OK` never appears with no ND-5000.
>  - `Selftest_ZeroDataPairCausesFalsePasses` - sets the value back to `0x00` and asserts the false
>    passes DO return. Without this second test the first could pass for the wrong reason (e.g. the
>    firmware stopped reaching the selftests at all); with it, the causal link is pinned.
>
> The analysis below is kept because it is correct and explains WHY - in particular the crux that
> the same read means "data" to one group of tests and "error code" to another.

> `Register test abcd completed OK`
> `TSB test completed OK`
> `Instruction Cache test completed OK`
> `Data Cache test completed OK`

With no ND-5000 attached none of these can legitimately pass. Here is exactly why they do.

`MfBusCmdDataPairStatus` @0x7374 - the routine every one of them calls:

```
737C  tst.w  g_skipBusyWaitFlag (0x113138)
7382  bne    skip                        ; flag set -> do not wait
7384  btst.b #0,(0x00660001) ; beq -10   ; else spin for "command done"
738E  move.w (0x00550000),D0 ; swap D0   ; high half
7396  move.w (0x00440000),D0             ; low half
73A0  move.w #0x5,(0x00220000)           ; function code 0x0005
73A8  return D0                          ; the 32-bit data pair
```

`Selftest_Tsb` @0xE818 then does:

```
E846  jsr    MfBusCmdDataPairStatus
E850  tst.l  D0
E852  bne.b  fail                        ; NON-ZERO = failure
      ; falls through on ZERO ->
E854  print "completed OK"
```

**Zero means PASS.** The emulator returns 0 for reads of `0x440000` / `0x550000`, so D0 is 0,
so the test reports success without touching any ND-5000.

#### The crux - the same read has two opposite meanings

The identical data pair is interpreted **as data** by one group of tests and **as an error
code** by another:

| Group | Interprets the read as | Reading 0 gives |
|---|---|---|
| BUS / MIR / Control Store / Control Cache | test **data**, compared against the LCG pattern | correct FAIL (`Result: 00000000H`) |
| Register / TSB / Instruction Cache / Data Cache | an **error code**, 0 = no error | **false PASS** |

That is precisely the pattern in the log, and it means the failures and the passes have the
same cause. **Fixing the failures is not the job - fixing the passes is.**

#### What to do

Reads of `0x440000` / `0x550000` with no responder attached must not yield a clean zero.
Options, in preference order:

1. Model a **no-responder / timeout** result and return a distinct non-zero error code. This
   is closest to hardware and makes both groups behave correctly.
2. Failing that, return a fixed sentinel (e.g. `0xFFFFFFFF`) so the second group fails loudly
   instead of passing silently.

**Do not** leave it at 0. A test that fails is the model behaving correctly; a test that
passes without the hardware is the model lying, and it will silently validate broken behaviour
in every later phase.

Also note `g_skipBusyWaitFlag` (0x113138) at 0x737C: when non-zero the routine **skips the
0x660001 bit 0 wait entirely**. Worth knowing before tuning status bits.

---

### D2 ~~[HIGH]~~ **CLOSED - the eight values are pinned as literals in a test**

> **STATUS 2026-08-01.** The recommendation at the end of this entry - "those eight words belong in
> a unit test as literals" - **has been done.** `Selftest_PrintsExpectedLcgPatterns` asserts the
> BUS-test value `1C587698H` and the exact sequence
> `7698H B027H 0AAAH 2C91H 0D8CH F58BH AFBEH 6195H`, so a regression to `F538H` now fails the build
> rather than being spotted by eye in a log.

Log 1 printed `... 0D8CH F58BH AFBEH ...`; this log prints `... 0D8CH F538H AFBEH ...`.

The expected values are **not stored constants** - a byte search for them in the ROM finds
nothing. They are generated. `SelftestPatternLcgSeed` @0xB3DC sets:

```
g_lcgMultiplier (0x00114584) = 0x00010DCD = 69069
g_lcgIncrement  (0x00114588) = 0x0000006F = 111
g_lcgSeed       (0x0011458C) = 69069        (passed in D0 by the caller)
```

so the pattern is a linear congruential generator:

```
next = (seed * 69069 + 111) mod 2^32
```

**Fully deterministic** - no hardware input, no timer, no uninitialised memory. The firmware
therefore cannot vary here, on real or emulated hardware.

Computed 2026-07-28:

```
v1                  = 0x1C587698     <- matches the BUS test line exactly
low words of v1..v8 = 7698 B027 0AAA 2C91 0D8C F58B AFBE 6195
```

**`F58BH` is correct; `F538H` is wrong.** Either this build regressed or the log was mistyped -
but the ground truth is now fixed and those eight words belong in a unit test as literals.
(69069 is the Marsaglia/VAX multiplier; ND used the same family for XMSG's ZRAND.)

---

### D3 ~~[MEDIUM]~~ **CLOSED - the spaced banner is asserted**

> **STATUS 2026-08-01.** A test now asserts the console reproduces
> `S A M S O N   A C C E S S   P R O C E S S O R` with its runs of spaces intact, so the
> space-collapsing this entry warned about would fail the build. That matters beyond the banner:
> the concern here was that collapsing runs of spaces would also corrupt every column-aligned
> diagnostic the firmware prints, and several selftest reports are column-aligned.

The ROM string at 0x11729 is the **spaced** form:

```
******   S A M S O N   A C C E S S   P R O C E S S O R   ******
```

Log 1 reproduced it. This log shows `******  SAMSON ACCESS PROCESSOR  ******`. If the output
path is collapsing runs of spaces, it will also corrupt every column-aligned diagnostic the
firmware prints - and several of the selftest reports are column-aligned.

---

### D4 ~~[CONFIRMED]~~ **CANNOT OCCUR IN THE CURRENT MODEL, and its mechanism was WRONG**

> **STATUS 2026-08-01.** The observation was real - the pair did return `0x00000024` - but the
> stated mechanism, "a read-back-what-was-written model hands it straight back", **does not match
> the code**. `AccpLoggingStub.WriteByte` **records the write and discards the value**; `ReadByte`
> returns the configured read value with no memory of writes. Nothing written can come back out.
>
> With `AbsentDeviceDataValue = 0xFF` (the D1 fix) the pair reads all-ones, so the result is
> determined by the model rather than by whatever the firmware last printed.
>
> **Guarded by `Selftest_DataPairDoesNotReturnStaleWrittenValues`**, which fails if `0x24` ever
> appears as a selftest result again, and also asserts the absent-device value is not `0x00` - so it
> cannot pass merely because the machine stopped reaching the test.
>
> **Kept, not deleted**, because the `0x24` = `$` observation is a good catch and the reasoning is
> the right shape - it was simply aimed at a mechanism this emulator does not have. If a stale value
> ever reappears, look for a stub that DOES retain writes, not this one.

Carved 2026-07-28 at 0xE186, in the test that follows the "Loading control store with
selftests..." announcement:

```
E186  move.l #0x55555555,(0x18,A6)          ; the expected value
E18E  jsr    ControlStore_Helper_79BC        ; prepare
E194  jsr    MfBusCmdDataPairStatus @0x7374  ; D0 = (0x550000)<<16 | (0x440000)
E1A6  cmp.l  (0x18,A6),D0
E1AA  bne    fail                            ; -> "failed / Result / Expected"
```

So `Result: 00000024H` **is the value your machine returned from the data pair**: high word
`0x0000` from `0x550000`, low word `0x0024` from `0x440000`.

**This is the same register as D1, and it proves the model is inconsistent.** In the D1 path
the pair reads back `0x00000000`; here it reads back `0x00000024`. It cannot be both unless the
pair is returning **stale state rather than a modelled value**.

#### The likely mechanism, and it is specific

`0x24` is exactly `$`, the ND string terminator that ends every message this firmware prints.
`AobSingleWordWrite` @0x72A0 writes words to `0x440000`, and the firmware has an ND-100 output
path (`Nd100OutputQueue` @0x1C8A, plus the `"ND100 output ... buffer overflow"` strings). If
console or ND-100 output passes through the AOB, the **last word written to `0x440000` before
this test is the `$` that terminated the previous string** - and a read-back-what-was-written
model hands it straight back.

**Marked as the leading hypothesis for the mechanism; the value 0x24 arriving from 0x440000 is
proven.**

#### What this means for the D1 fix

D1 and D4 are one bug: **`0x440000` / `0x550000` are implemented as read-back-what-was-written
(or as uninitialised storage), and the firmware treats those reads as real responses.** That
single behaviour produces both symptoms - a clean 0 becomes a false "no error" pass, and a
stale 0x24 becomes a bogus data mismatch.

Fix them together: reads of the data pair with no responder attached must return a **modelled
no-responder result**, never the last value written and never a bare zero.

---

### D4-original [superseded] `Result: 00000024H` - 0x24 is the newline marker

> `Loading control store with selftests... failed`
> `Result   : 00000024H  Expected : 55555555H`

**0x24 is exactly `$`**, the ND newline/terminator byte embedded in every string in this
firmware. A control-store read-back handing back 0x24 suggests a data register or the staging
buffer `g_microwordStagingBuffer` (0x001144F0) is returning a byte that passed through the
console/string path. Check whether `0x440000` / `0x550000` are shared with, or not cleared
between, the string writer and the control-store path.

**Marked as a hypothesis, not proven** - 0x24 could be coincidence. But it is a specific enough
coincidence to check first.

---

### D6a ~~[HIGH - NEW REGISTER]~~ **CLOSED - the register is modelled and configurable**

> **STATUS 2026-08-01.** `0x00900001` is implemented in `AccpMachine` as
> `OwnStationNumberAddress`, and the card's own station number is served from
> `AccpMachineConfig.StationNumber` masked to the low 5 bits, exactly as the firmware reads it at
> 0x122E before the MFbus scan. It is no longer "being missed".

> **[STALE 2026-07-31 - the premise no longer holds, but the requirement below still does.]**
> `0x900001` **is** in the handoff table (part 2, marked PROVEN 2026-07-28), and the machine
> returns **1**, not 0. The defect as titled is fixed. What survives is the requirement in the
> body: the value must be a legal, unique station number. Station **1 is the ND-120 CPU slot**
> and is a poor choice for the ACCP; the firmware masks the byte with `0x1F` at `0x1260`, so
> only **20B-37B** are expressible for a local node. `AccpMachineConfig.StationNumber` now
> defaults to `0x10` (20B), the lowest legal local-node number. Whether the card should instead
> learn its number from the MFbus controller writing its WOI register is **still open** - it is
> re-asked as question 4 in part 6.

Carving D6 turned up **a hardware register that was not in the handoff table**, and the machine
almost certainly returns 0 for it.

`MfBusControllerConfigCheck` @0x121C, first thing it does:

```
122E  move.b (0x00900001).l,(0x19,A6)     ; the board's own config byte
1260  moveq  #0x1F,D0
1262  and.b  D0b,D1b                      ; mask to 5 BITS
1268  move.b D1b,(0x17,A6)                ; -> the ACCP's OWN STATION NUMBER
```

Five bits = 0..31, matching the octal station ranges in ND-14001 section 4.8.1 (global 0-17B by
thumbwheel, local 20B-77B by register). On real hardware this is the thumbwheel/DIP setting.

**An emulator must supply a sane, unique value here. Zero is an illegal OCTObus station
number**, and returning 0 gives the card an invalid identity before it says a word on the bus.

Note this is `0x900001`, distinct from the already-known `0x900007`. Select 0x90 has at least
two registers, and `0x90` is not a replicated nibble - do not let the address decoder assume
the nibble rule.

---

### D6b ~~[HIGH]~~ **CLOSED - discovery is implemented and exercised by a peer**

> **STATUS 2026-08-01.** The MFbus discovery exchange this entry specifies is implemented and has
> its own test file, `AccpMfBusDiscoveryTests.cs`, driven by `AccpMfBusControllerPeer.cs` - a real
> peer that answers the scan rather than a stubbed reply. `Discovery_ReplyByte1SelectsCpuModel`
> proves the model digit is taken from the reply, so the rule is demonstrated rather than assumed.
>
> The protocol description below stays as the reference for what the peer must answer.

The "not found at Octobus stations 2-7" message is not a vague failure. It is a precise scan
you can satisfy:

```
1236  station = 1
123C  station = station + 1                 ; -> 2 first pass
1290  cmp.b (0x1A,A6),D2   with D2 = 7
1294  bcs   exit                            ; loop while station <= 7  -> STATIONS 2..7
12A4  lea   (0x1C,A6),A0                    ; the OBCON request block
12A8  jsr   ObconRequestDispatch @0xF686
12AE  cmpi.w #-0x7D00,(0x1E,A6)             ; status == 0x8300 ?
12B4  bne   next station
12BC  jsr   MFCRECEIVE @0x14B4              ; on success, read the reply
```

Request block built at (0x1C,A6) - **and it independently confirms the layout carved from
`Cmd3B_SendKickOctobus`**:

| Offset | Value | Field |
|---|---|---|
| +0x00 | `0x0041` | function code - **multibyte message** |
| +0x02 | - | **status; must return `0x8300` for "found"** |
| +0x06 | `0x05` | process / subprocess |
| +0x0C | station | destination, 2..7 |
| +0x0E | `0x05` | |
| +0x10 | 12 bytes | array descriptor over the message buffer |

**To get past this**: answer OBCON function 0x41 addressed to a station in 2..7 with status
`0x8300` *and* a reply `MFCRECEIVE` @0x14B4 can parse. Anything less and the firmware is right
to report "not found" - which means the current message is the model behaving correctly, not a
bug. Fix D6a first; the station identity comes before the scan.

---

### D5 + D6 [RESOLVED - both are artifacts, one root cause] `ND-5800` and the contradictory MFbus line

**Both lines come from the same thing: nothing answered a signature probe.** Neither is a
defect, and neither is a real report. Carved from `DetectCpuModelBySignature` @0x110A.

The firmware probes memory through A0 for the 16-bit signature **`0x7F55`** at byte offsets 0,
4 and 0x0C, in a three-class chain:

| Class | Condition | Sets |
|---|---|---|
| 1 (0x1150) | signature at +0 and +4 | `g_cpuModelClass`=1, `g_cpuModelCode`=`0x5200` |
| 2 (0x1184) | signature NOT at +0x0C or +4 | `g_cpuModelClass`=2, `g_cpuModelCode`=`0x5400` |
| **3 (0x11D2)** | signature NOT at +0x0C or +4 | `g_cpuModelClass`=3, **`g_cpuModelCode`=`0x5800`** |
| - | signature FOUND at a probe point | `clr.b g_cpuModelClass` (= unknown) |

Each class cross-checks D0 (the expected model) against its permitted set - class 1: `0x5200`;
class 2: `0x5400`/`0x5500`/`0x5700`; class 3: `0x5800`/`0x5900`. On a match it sets
`g_cpuModelConfirmed` (0x1131FA) = 1. Then:

```
1204  tst.w  g_cpuModelConfirmed
120A  bne    done
120C  ori.w  #0x8000,(g_controlStoreErrorLatch 0x001131E2)
```

**Class 3 / ND-5800 is the LAST branch in the chain.** With every probe read returning 0, no
signature is ever found, the chain falls all the way through to class 3, D0 fails to match, so
`g_cpuModelConfirmed` stays 0 and the error bit is set. That produces **exactly** the two lines
in the log, in that order.

So: `CPU model: ND-5800` is the fall-through default, not a detection, and
`MFbus controller has incorrect CPU model setting.` is its direct consequence. **Do not treat
the ND-5800 line as evidence of anything** until something answers the probe - which matters,
because ND-5800 versus classic ND-500 is the 128-bit versus 144-bit microword distinction.

Bonus finding: `g_cpuModelCode` is **hex-coded decimal**. `0x5800` prints as "5800" because the
console's default base is 16 (`g_numberBase` = 0x10, set at 0x213A).

**To make this line meaningful**, answer the `0x7F55` signature probe at the right offset for
the model you want to present. That is a Phase 5/6 task, not a bug fix.

---

### NOT A DEFECT - do not "fix" these

#### The `a` / `ab` / `abc` / `abcd` letters are genuine firmware output

`Selftest_MirTest` @0xB8C8 at 0xB908:

```
B908  move.b #0x61,(0x14,A0)      ; 0x61 = 'a'
B90E  jsr    ConsPutCharQueued
```

One progress letter per completed sub-phase. `MIR test a failed` = got through phase a;
`Register test abcd completed OK` = all four phases ran. The accumulation is correct.

#### The BUS / MIR / Control Store / Control Cache failures are the correct outcome

Those tests target an ND-5000 that is not modelled. Their `Expected:` values are the ready-made
oracle for the day one is attached.

---

### Free assertions for the test suite

| Assertion | Value |
|---|---|
| BUS test expected | `0x1C587698` |
| MIR / Control Store 8 words | `7698 B027 0AAA 2C91 0D8C F58B AFBE 6195` |
| Selftest status word | `0x043F` (a failure bitmask - one cheap assertion instead of diffing console text) |
| RAM walk-test results | `g_ramTestErrors_firstHalf` 0x11312A (**32-bit**), `_secondHalf` 0x11312E (32-bit), `g_ramTestDone` 0x113132 (**16-bit**) |

---

### Provenance

D1's mechanism was read from the disassembly of 0x7374 and 0xE818. D2's LCG parameters were
read from 0xB3DC and the sequence computed independently; v1 matches the firmware's own printed
value exactly, which is what validates the computation. D3 is a string comparison against
0x11729. D4, D5 and D6 are labelled as hypotheses. The "not a defect" items were read from
0xB908.

---

# Part 4 - originally `ACCP-COMMAND-LOG-CLEAN-BOOT-CAPTURE-2026-07-30.md`

> Merged verbatim 2026-07-31. Content is byte-identical to the original;
> only heading levels were demoted by one so the parts nest under this file.

## ACCP command log - one clean SINTRAN boot over the octobus

**Date**: 2026-07-30
**From**: the SINTRAN-over-octobus side (ND-100 card, `OctobusND5000Station`).
**Answers**: section 6 of `ANSWERS-TO-ND5000-TEAM-2026-07-30.md` ("the most useful single
artefact is the command log from one clean boot, with the AIB command numbers and the answers").

The capture is BIDIRECTIONAL: every OMD-3/OMD-4 multibyte command the ND-100 sent, and every
reply our station sent back, with the full payload bytes. `IN` = ND-100 to station.
`OUT` = station to ND-100.

Produced by `Nd100SintranNd5000OctobusBootHarnessTests.FullFlow_Octobus_Login_Nd500_Status_StartSwapper_Capture`
(`DumpAccpExchange("full-run")`). Machine-readable original:
`C:\Users\ronny\AppData\Local\Temp\retrocore-nd5000-octobus\sintran-octobus-accp-exchange-full-run.txt`

---

### Totals for this run

| | Count |
|---|---|
| Commands in (ND-100 to station) | **149** |
| Replies out | **150** |
| Commands with NO reply | **0** |
| `LCS0` (023B, control-store load) commands | **128** |
| `244B TERMINATE ACCP` emergencies | **1** (see the correction below - we first reported 0) |

The extra outbound message is the one UNSOLICITED reply in the whole run: the `TRAP_OCBM 202B`
model/version report we emit at `ENKICK`. It answers no command, which is why out exceeds in
by exactly one.

### Read this before comparing against your command-3 answer

Two things in the trace look wrong at a glance and are not:

1. **`LSSYSPAR` arrives on OMD 4 and is answered on OMD 3.** That is deliberate. The reply goes
   to the S5 reply-to OMD carried in the message body (`message[4]` = 5OMDNO), not to the OMD the
   command arrived on. SINTRAN's `5OMBREAD` only sets `CPUAVAILABLE.5ALIVE` for an ACK that
   arrives there, so answering on the arrival OMD would leave the monitor printing
   "No ND-500(0) CPU found".

2. **`ALIVE(037B)` is answered `Messnak err=7`.** Correct at that point in the ladder: 037B
   means "is the microprogram running", and at that moment it is not (this is before
   `STAMIC0`). Error 7 = not alive. SINTRAN expects the refusal and continues.

**Where the model digit actually crosses**: line 377 of the trace,
`OUT omd=3 [82 01 38 38 2E 9A]`. Byte 3 and byte 4 are both `0x38` (model, ND-5800) - current
model as seen on the ACCP/backplane side, then my-model read from loaded control-store word 7.
They are equal by construction on our side. Version `0x2E9A` is control-store word 1 (LARG) for
the 5800-B30 image. This is the byte your command 3 has to agree with.

### About the 244B evidence you asked for - CORRECTED 2026-07-30

**We first wrote that a clean run contains no 244B. That was wrong, and we are correcting it
before you write it down.** A clean run DOES send one, in the same place, after the same three
answered commands. Its own footer:

```
# commands=147 unanswered=0 accpIdle=False
# 244B TERMINATE snapshot: 244B TERMINATE after 3 ACCP commands, 0 unanswered.
  Last 3: cmd=16B len=9 answered | cmd=60B len=3 answered | cmd=16B len=9 answered
```

The mistake was ours and worth naming: our first clean capture predated the footer field that
records this snapshot, so the line was simply not being written. We read a missing FIELD as a
missing EVENT.

The honest statement for your documentation:

- 244B is **not** evidence of a timeout. It arrives with a 100%-answered command history behind
  it, in every run we have, fixed or not.
- The G10 fix does **not** stop SINTRAN sending 244B. It stops the resulting `_accpIdle` from
  sticking: the flag ends `False` instead of `True`, so later kicks are no longer swallowed.
- Treat 244B as an unconditional bring-up step. Do not treat receiving one as a fault signal.

part 5 of this file shows the CONSEQUENCE of the stuck flag (the
pre-fix run where every subsequent kick dies). The 244B itself is in either capture.

### The trace

```
IN  omd=3 [03 07 0E 01 03 00 00 00 00] cmd=LSSYSPAR(016B/LoadSysPar)
OUT omd=3 [00 00] Messack(status 0)
IN  omd=4 [04 01 30] cmd=READSELFT(060B/ReadSelftestStatus)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 07 0E 01 03 00 00 00 00] cmd=LSSYSPAR(016B/LoadSysPar)
OUT omd=3 [00 00] Messack(status 0)
IN  omd=4 [04 01 1F] cmd=ALIVE(037B)
OUT omd=4 [FF 07 00] Messnak err=7
IN  omd=4 [04 01 32] cmd=DISKICK(062B)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 1C] cmd=STOPMIC(034B)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 39] cmd=CPURES(071B/ResetSamsonCpu)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 32] cmd=DISKICK(062B)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 1C] cmd=STOPMIC(034B)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 05 11 00 00 08 00] cmd=LPARPNT(021B/LoadParamPtr)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 12] cmd=VERPAP(022B/VerifyParamPtr)
OUT omd=4 [00 65 96 9B 49] VPARP echo
IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
CMWWC N=128 csWord=0x0000 pb+4=[0000 0000 0001 8000 0000 0000 194F 2E9A 4000 0001 DE01 6010]
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
CMWWC N=128 csWord=0x0080 pb+4=[4000 0000 0201 5000 0000 0000 1558 0000 4000 0000 0801 4000]
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
CMWWC N=128 csWord=0x0100 pb+4=[4000 800E 7E01 0000 0000 A24F 0101 0005 4000 000E 7E01 A000]
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
CMWWC N=128 csWord=0x0180 pb+4=[4000 0006 7E01 2000 0000 024F 0181 0005 4000 0004 3C01 0000]
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
CMWWC N=128 csWord=0x0200 pb+4=[4000 000E 7E01 2000 0000 020F 0201 0004 F000 0000 6521 2000]
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
CMWWC N=128 csWord=0x0280 pb+4=[4000 0002 7E01 8000 0000 A049 0281 0004 4000 0002 7E01 8000]
OUT omd=4 [00 00] Messack(status 0)

    ... the LCS0 / Messack pair repeats to 128 loads in total. Only the first six
    carry a CMWWC content line here because our multiport-window dump is armed for
    the first six; the remaining 122 are identical in shape:
        IN  omd=4 [04 01 13] cmd=LCS0(023B/LoadCSviaMPM)
        OUT omd=4 [00 00] Messack(status 0)

IN  omd=4 [04 01 15] cmd=DUC0(025B/DumpCSviaMPM)
DUCS-ARM #1 N=1 csWord=0x0000 SigmaR25=0xC7EA cs[0..7]=0000,0000,0001,8000,0000,0000,194F,2E9A
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 15] cmd=DUC0(025B/DumpCSviaMPM)
DUCS-ARM #2 N=1 csWord=0x1000 SigmaR25=0xC410 cs[0..7]=4000,000C,5401,2000,0000,0000,1003,0000
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 15] cmd=DUC0(025B/DumpCSviaMPM)
DUCS-ARM #3 N=1 csWord=0x2000 SigmaR25=0x8604 cs[0..7]=5000,0001,9602,9000,0000,F000,2001,0000
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 15] cmd=DUC0(025B/DumpCSviaMPM)
DUCS-ARM #4 N=1 csWord=0x3000 SigmaR25=0x8A8C cs[0..7]=D001,8000,56B0,B000,0000,0000,33DB,0000
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 32] cmd=DISKICK(062B)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 1C] cmd=STOPMIC(034B)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 03 36 00 00] cmd=STAMIC0(066B/StartMicDirect)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 31] cmd=ENKICK(061B)
OUT omd=4 [00 00] Messack(status 0)
OUT omd=3 [82 01 38 38 2E 9A] TRAP_OCBM 202B model/version report (model=0x38 version=0x2E9A)
IN  omd=4 [04 01 10] cmd=REOD(020B/ReadEodLevels)
OUT omd=4 [00 00] Messack(status 0)
IN  omd=4 [04 01 3D] cmd=PRGMVERS(075B/ReadAccpPrgmVersion)
OUT omd=4 [00 00] Messack(status 0)
```

Each `DUC0` also produces 18 `DUCS-OVR` byte-level lines showing how the microprogram writes the
dumped control-store word back into the multiport window one byte at a time. They are in the
machine-readable file but stripped here as noise; ask if the byte-splitting order matters to you.

### The ladder in words

1. `LSSYSPAR` twice, `READSELFT` - system parameters and self-test status. The first `LSSYSPAR`
   is what earns `5ALIVE`.
2. `ALIVE` refused (microprogram not started yet), then `DISKICK` + `STOPMIC` + `CPURES` +
   `DISKICK` + `STOPMIC` - put the CPU and the ACCP into a known stopped state.
3. `LPARPNT` then `VERPAP` - the parameter-area pointer handshake. `VERPAP` is the ONE command a
   canned answer cannot pass, because it echoes a 32-bit word SINTRAN wrote into shared multiport
   memory.
4. **128x `LCS0`** - the control store, loaded through the multiport window.
5. **4x `DUC0`** - dump-back checksum verification at control-store words 0, 0x1000, 0x2000,
   0x3000.
6. `DISKICK` + `STOPMIC` + `STAMIC0` + `ENKICK` - start the microprogram, then hand the octobus
   over to it. Our `TRAP_OCBM` model report goes out here.
7. `REOD` + `PRGMVERS` - read EOD levels and the ACCP program version.

In the whole run exactly **one** kick is ever sent, and not in this ladder: `CLRKICK` (kick 3) at
`stop-system`. Activation is the `X5ACT := 0` write, never a kick.

### Related documents

- part 6 of this file - the questions this answers section 6 of
- `ANSWERS-TO-ND5000-TEAM-2026-07-30.md` - their replies
- part 4 of `ACCP-COMPLETE-REFERENCE.md` - the interface spec
- `OCTOBUS-KICK-AND-MAILBOX-GAP-REGISTER-2026-07-30.md` - our gap register (G1-G10)
- `STOP-SYSTEM-ANALYSIS-AND-CLRKICK-GAP-2026-07-30.md` - the `stop-system` / `CLRKICK` analysis

---

# Part 5 - originally `ACCP-244B-TERMINATE-PREFIX-CAPTURE-2026-07-30.md`

> Merged verbatim 2026-07-31. Content is byte-identical to the original;
> only heading levels were demoted by one so the parts nest under this file.

## The 244B TERMINATE capture - produced by putting the defect back on purpose

**Date**: 2026-07-30
**From**: the SINTRAN-over-octobus side (ND-100 card, `OctobusND5000Station`).
**Answers**: the second half of section 6 of `ANSWERS-TO-ND5000-TEAM-2026-07-30.md` - you asked for
the 244B snapshot with our evidence attached rather than as hearsay.

We offered this in part 4 of this file and said there the clean
run contains no 244B at all. **That statement was wrong and both documents are now corrected** -
see the correction immediately below. This document remains the pre-fix run, which is what shows
the consequences of the stuck flag.

---

### CORRECTION 2026-07-30: the fixed build sends 244B too

**We got this wrong first time and are correcting it before you act on it.** We originally wrote
that a fixed run never sends 244B. It does. A clean run's own footer reads:

```
# commands=147 unanswered=0 accpIdle=False
# 244B TERMINATE snapshot: 244B TERMINATE after 3 ACCP commands, 0 unanswered.
  Last 3: cmd=16B len=9 answered | cmd=60B len=3 answered | cmd=16B len=9 answered
```

Same position in the ladder, same three answered commands. **The fix does not remove the 244B; it
removes its aftermath** - `accpIdle` ends `False` instead of stuck `True`.

Why we believed otherwise: our earlier clean capture predated the footer field that records the
244B snapshot, so the line was simply absent from the file. We read absence of the FIELD as
absence of the EVENT. Flagging our own mistake because it is the same failure shape we warned you
about in point 2 below - a counter that looks innocent because nothing is writing to it.

**This strengthens rather than weakens the finding**: 244B is unconditionally a normal bring-up
step, not a fault path at all.

### Why this run was still manufactured

The defect run below was produced deliberately via
`OctobusND5000Station.Diag_SuppressAccpIdleClearOnMicroStart`, which makes starting the
microprogram stop clearing `_accpIdle`. That is exactly the G10 defect as it existed before
2026-07-30 - nothing else differs, and both runs execute the same ladder code. It is what shows
the CONSEQUENCE of the stuck flag; the 244B itself is visible in either run.

**Read this capture as "what the pre-fix system did", not as current behaviour.**

Capture file:
`C:\Users\ronny\AppData\Local\Temp\retrocore-nd5000-octobus\sintran-octobus-accp-exchange-prefix-g10-defect-full-run.txt`

### The snapshot

```
244B TERMINATE after 3 ACCP commands, 0 unanswered.
Last 3: cmd=16B len=9 answered | cmd=60B len=3 answered | cmd=16B len=9 answered
```

Decoded, those three commands are:

| # | Command | Answered |
|---|---|---|
| 1 | `LSSYSPAR` (016B, LoadSysPar) | yes, Messack |
| 2 | `READSELFT` (060B, ReadSelftestStatus) | yes, Messack |
| 3 | `LSSYSPAR` (016B, LoadSysPar) | yes, Messack |

**This is the whole argument.** SINTRAN sends emergency 244B TERMINATE ACCP after exactly three
commands, and every one of them was answered. A timeout requires something to have gone
unanswered. Nothing did. **244B is a normal bring-up step.**

### End-of-run state, same run

```
ACCP (before-stop-system) commands=149 unanswered=0 accpIdle=True
KICKS (before-stop-system) NONE RECEIVED | droppedDisabled=0 kicksEnabled=True
OUTCOME: ENTER=OK login=OK nd-500=OK status=STALL start-swapper=OK list=OK stop-system=OK
```

Three things worth your attention:

1. **149 commands, 0 unanswered, for the entire run.** The command channel was never the problem.
   The 244B at command 3 and the perfect answer record at command 149 are the same run.

2. **`kicksEnabled=True` but `droppedDisabled=0` and no kick ever arrived.** The kicks were not
   dropped by the kicks-disabled guard - they were swallowed earlier, by the `_accpIdle` guard
   that 244B set and that nothing subsequently cleared. If you carry a similar "idle after
   terminate" flag, this is the failure shape: the disabled-kick counter stays at zero and looks
   innocent while every kick is discarded somewhere else.

3. **`stop-system` still reports OK.** It is not a hang. `ST0PSYS` (MP-P2-N500.NPL:3759, 147433B)
   polls `X5CLR` a bounded 1000 times and then falls through to `ERRFATAL`, which still reaches
   the power-fail path and halts the ND-100. So the defect is a correctness gap - the ND-500 is
   never actually cleared - and never presents as a hang. Worth knowing before you use "it
   stopped" as a health signal.

`status=STALL` in this run is a harness wall-clock flag, not a machine failure; the command
completes late. Do not read it as a difference between the two runs.

### Compared with a clean run

| | Pre-fix (this capture) | Fixed |
|---|---|---|
| ACCP commands | 149 | 149 |
| Unanswered | 0 | 0 |
| 244B TERMINATE | **1**, after 3 answered commands | **1**, same place, same 3 commands |
| `accpIdle` at stop-system | **True** (stuck) | False |
| Kicks received | **none** (all swallowed) | `CLRKICK` (kick 3) received |

The command traffic is identical, and so is the 244B. The only difference the defect makes is the
stuck flag and the kicks that die behind it.

### New in both captures: where the CPU model comes from

Since you cross-check the model digit, the capture now shows its DERIVATION and not only the byte
we put on the wire. Immediately before the model report you will now see:

```
CPUMODEL-DERIV csWord7=0x0038 packedModel=0x38 cpuType=3(bits4-5) modelDigit=8(bits0-3) (ND-5800)
  accepts=[5800, 5900] bareDigitForCmd5=0x08 NOT-ASCII csWord1=0x2E9A version=0x2E9A
OUT omd=3 [82 01 38 38 2E 9A] TRAP_OCBM 202B model/version report (model=0x38 version=0x2E9A)
```

**NOTE on the capture file itself**: this run predates the corrected decode, so the line inside
`sintran-octobus-accp-exchange-prefix-g10-defect-full-run.txt` still reads
`model=0x38 class=3 digit=8`. Same byte, same meaning, worse naming - see
`ANSWER-CPU-MODEL-ENCODINGS-2026-07-30.md`. We did not re-run to refresh the wording, because you
asked us not to re-run on your account and this capture is filed rather than load-bearing.

Reading that line:

- `csWord7=0x0038` - the last halfword of control-store word 7 in the image we modelled as
  loaded. This is the source of the model byte; we do not choose it.
- `packedModel=0x38` is TWO FIELDS: `cpuType=3` (bits 4-5) and `modelDigit=8` (bits 0-3), per
  ND-60230-5-EN Function 156a WRSYSINFO. It is **not** ASCII `'8'` despite the identical byte.
- `bareDigitForCmd5=0x08` is what your CMD-5 encoding carries - printed so nobody feeds the packed
  `0x38` into `model = 0x5000 | (byte << 8)` and gets `0x7800`.
- `accepts=[5800, 5900]` is your type table from `ANSWERS-TO-ND5000-TEAM-2026-07-30.md` section 2,
  printed alongside so a type/digit mismatch is visible in the log instead of only as a
  downstream "Wrong microprogram" (EWRON).
- `csWord1=0x2E9A` - control-store word 1 (LARG), the microprogram version.

The same derivation is also traced to the device log (DEBUGTRACE 2), including an explicit warning
if the model byte comes out `0x00`, which means the control store is not loaded rather than that
the model is wrong.

**This should make our two sides directly comparable**: if your command 3 ever answers a digit we
did not derive from the loaded image, the disagreement is now visible on both ends with its
provenance attached.

### Related documents

- part 4 of this file - the clean-boot command log (which DOES
  contain a 244B, same as this one - only the stuck flag differs)
- `ANSWERS-TO-ND5000-TEAM-2026-07-30.md` - the answers this responds to
- part 6 of this file - the original questions
- `OCTOBUS-KICK-AND-MAILBOX-GAP-REGISTER-2026-07-30.md` - the gap register; G10 is this defect
- `STOP-SYSTEM-ANALYSIS-AND-CLRKICK-GAP-2026-07-30.md` - the `stop-system` / `CLRKICK` analysis

---

# Part 6 - originally `QUESTIONS-TO-ACCP-TEAM-2026-07-30.md`

> Merged verbatim 2026-07-31. Content is byte-identical to the original;
> only heading levels were demoted by one so the parts nest under this file.

## Questions to the ACCP / ND-5000-octobus team

**From**: the SINTRAN-over-octobus side (ND-100 card, fabric, `OctobusND5000Station`, the C#
servicer that stands in for the microcode).
**Re**: part 4 of `ACCP-COMPLETE-REFERENCE.md`.

Context on our side, so you know what is already true and do not re-derive it:

- The ND-500 monitor over octobus completes its whole administrative ladder, the swapper runs and
  allocates 7110B pages, and `stop-system` halts the ND-100 cleanly.
- Kicks now work end to end. Our kick table matches yours exactly (0 NOTREC; 1,2 ACTIVATE;
  3 OCB_KICK03; 4,5 OCB_KICK05; 6 OCB_KICK06; 7-63 NOTREC 204). Kick 3 and kick 6 were
  implemented against the executed B30 microcode, not the carve summary.
- Activation is the `X5ACT := 0` write (ACT51), never a kick. In a whole SINTRAN boot exactly ONE
  kick is sent: `CLRKICK` at `stop-system`.
- We measured 0 of 149 ACCP commands unanswered in a full run.

---

### 1. Your open item 6 - which side is the multibyte truncation on?

You report: *"only the FIRST content byte of an octobus multibyte reply reaches the ACCP driver's
receive buffer, so the model digit cannot currently get through"*, with the dump at `0x00112D54`
showing `byte5=0x00` content[0] then zeros.

**Our SENDER emits every payload byte.** `NDBusOctobus.SendMultibyteMessage`:

```
SOMB (C=1,M=1,S=1, destOmd)
data frame: sourceOmd
data frame: payload.Length
data frame: payload[i]      for EVERY i
EOMB (C=1,M=1,S=0, destOmd)
```

So the loss is on the receive path, not in what we put on the bus.

**Question:** did you observe the truncation (a) at the ACCP's octobus receive FIFO / driver, or
(b) at the fabric delivery into your station's `HandleFrame`? If (b), tell us the destination
station and OMD and we will chase it on the fabric side - our `OctobusFabric.SendFrame` delivers
one frame per call and does not batch, so a drop there would be ours.

Useful detail if you have it: how many DATA frames does your side actually see between SOMB and
EOMB? That single number separates "we sent one" from "you kept one".

### 2. Command 3 / the CPU model - what should an emulated ACCP return, and where from?

You say an emulated ACCP **must** answer AIB command 3 or the CPU never announces itself, and that
digit **8** (ND-5800) is the only value that works while the signature matrix reads zero.

Our station already has a CPU type/model concept for micro command 3 (chapter 5.3.7).

**Questions:**
- Is `0x5800` what we should return today, or is that only correct until the sixteen `0x220000`
  reads are modelled?
- We have a standing rule here: never hardcode a value the real hardware learns at runtime. Since
  the model is *configured in the MFbus controller*, is there any path by which our side can
  derive it, or is a constant genuinely the honest answer until the MFbus controller exists?

### 3. MFbus controllers at stations 2-7 - blocking or not?

Our standalone ACCP prints `MFbus controller not found at Octobus stations 2-7`. We have no
documentation for these and expect to have to carve them.

**Questions:**
- For the ND-5000 bring-up path we exercise (SINTRAN -> ACCP -> microcode -> swapper), is a
  responding MFbus controller required for anything **other** than supplying the CPU model?
- Does the monitor tolerate silence from 2-7, or does the probe have a timeout that costs real
  time or triggers a fallback?
- Do you have ANY document, part number or trace for the MFbus controller? We could not locate one
  and would rather carve from a hint than from nothing.

### 4. Your open item 7 - `0x900001` returning station 1

You suspect it is the WOI/STANO value the MFbus controller writes during crate configuration, and
that a correct model writes it before the ACCP boots.

**Question:** if that is right, the discovery scan never runs at all in a correct system. Does that
change what you want from us - i.e. should our fabric present a station number in the 20-77 octal
range for the ACCP's local node, and if so which one?

### 5. AFLAG bits 7 and 8

You flag these as never re-verified after the off-by-one correction.

**Question:** do you need them for anything current? From our side they only matter if we run the
real microcode through data/instruction faults, which we do not do yet. If nobody needs them, we
would rather leave them marked OPEN than have someone "tidy" them into a guess.

### 6. One thing we can give you back

If you want the reverse direction verified, our harness can now record every ACCP command with
whether it was answered (`AccpCommandLog`, `AccpUnansweredCount`), plus a snapshot at the moment
emergency 244B TERMINATE arrives. That is how we established 244B is a NORMAL bring-up step and not
a timeout. Say the word and we will hand over the trace format or run a capture for you.
