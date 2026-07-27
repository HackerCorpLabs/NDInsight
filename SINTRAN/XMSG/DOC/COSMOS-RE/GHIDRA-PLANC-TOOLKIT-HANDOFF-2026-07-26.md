# Ghidra PLANC/68000 Toolkit - Handoff

**Date**: 2026-07-26
**Target**: ENCOS Ethernet controller 68000 firmware (PLANC-MC compiled)
**Skill**: `ghidra-planc` (`C:\Users\ronny\.claude\skills\ghidra-planc\SKILL.md`)

---

## 1. Where the scripts live

Source of record (checked into NDInsight):

```
E:\Dev\Ronny\NDInsight\tools\ghidra-planc\
    PlancFixFlow.java            flow repair - skip returns, undisassembled slots
    PlancAnnotate.java           convention comments
    PlancSetupTypes.java         PLANC data types
    PlancApplyConvention.java    applies the __planc calling convention
    PlancFrameTypes.java         retypes A6 frame pointers to PlancFrame*
    PlancUndoFixFlow.java        undo for PlancFixFlow
    M68kVectorTable.java         68000 exception vector table
    README.md
    planc-68000.cspec-snippet.xml
```

Ghidra loads them from `C:\Users\ronny\ghidra_scripts\`. **The scripts are COPIED there** -
editing the NDInsight copy alone changes nothing. After every edit:

```powershell
Copy-Item 'E:\Dev\Ronny\NDInsight\tools\ghidra-planc\*.java' 'C:\Users\ronny\ghidra_scripts\' -Force
```

The `__planc` calling convention is patched into
`C:\Utils\Ghidra\ghidra_12.0.4_PUBLIC\Ghidra\Processors\68000\data\languages\68000.cspec`
(backup: `.before-planc.bak`). Launcher: `C:\Utils\ghidraRun.bat`.

---

## 2. What changed in this session (M68kVectorTable.java)

Three defects fixed, in order of how much damage they did.

### 2.1 Duplicate labels stacking up on every run

`createLabel()` **adds** a label; it does not replace one. Ghidra keeps every label ever placed
on an address and merely flags one as primary. So each run left the previous run's names sitting
underneath the new one - three labels on one address, no way to tell which was current.

**Fix**: every label now goes through `setSoleLabel(Address, String)`, which deletes this
script's own earlier output at that address first, then creates. Only two `createLabel` call
sites existed; both were routed through it.

**Trap for any future Ghidra script in this repo**: this is not specific to the vector table.
ANY script that names addresses and might run twice has this bug unless it deletes first.

### 2.2 The script defended its own garbage

The "does this address already have a real name?" test only recognised Ghidra's default prefixes
(`LAB_`, `FUN_`, `SUB_`, `DAT_`, `UNK_`). A name written by an EARLIER RUN of the same script -
`VecStub_004` - matched none of them, so the script classified it as a name earned by analysis
and refused to overwrite it. The bad names were therefore permanent.

**Fix**: `isReplaceable(name)` now also matches everything this script has ever emitted:
`VEC_*`, `Vec###_*`, `VecStub_*`, `UserVec*`, and `_Handler` / `_Stub` names.

The `_Handler`/`_Stub` rule is deliberately narrow - a bare suffix match would destroy a
hand-written `Timer_Handler`. The base must be one of the 256 strings `baseHandlerName()` can
actually generate (`ourBaseNames()`, built once and cached).

Names a human or another analysis pass wrote are still never touched; where one is present the
script adds its own as a **secondary** label instead of fighting for primary.

### 2.3 Useless names

| Rejected | Why | Now |
|---|---|---|
| `VecStub_004` | says nothing about which exception | `IllegalInstruction_Stub` |
| `UserVec100_Handler` | "user vector" names the table region, not the source | `DeviceIrq100_Handler` |
| `Reset_Handler` | this IS the image entry point, not just a pin | `ResetFirmwareEntry_Handler` |
| `VEC_004_IllegalInstruction` | stutters, and says nothing about the target | `Vec004_IllegalInstruction` |

Rule that settled it: the name always states **which exception reaches the address**. Whether the
target happens to be a shared default stub is secondary and only changes the SUFFIX
(`_Stub` vs `_Handler`), never the name.

### 2.4 Slot labels now carry the destination

```
Vec034_TRAP2_to_PiocOsTrap2Dispatch
|      |         |
|      |         +-- destination, but ONLY when it already has a name earned by analysis
|      +------------ what raises it, same vocabulary as the handler name
+------------------- vector NUMBER: what a device puts on the bus during interrupt acknowledge
```

A `LAB_00007c16` destination is omitted rather than pasted in - it would make the label longer
without making it say anything. The number is zero-padded and first so sorting by name sorts by
vector number.

### 2.5 Revert now cleans up properly

Revert reads the pointers **before** clearing the table, so it can also strip the handler labels
this script scattered across the image. Previously those were orphaned once the table was gone.

---

## 3. How to run it

1. **Revert** - clears 0x000-0x3FF and purges all labels from earlier runs (prints each one
   removed as `removed stale label 'X' at ADDR`).
2. **Apply** - types all 256 vectors as pointers, labels and comments them, names and
   disassembles the handlers.

Preview reports the fully decoded table, including the slot label next to each value, and writes
nothing. Ctrl+Z undoes a whole run.

Because the vectors become POINTERS, Ghidra creates references to every handler - which is what
makes previously unreferenced handlers visible in the symbol tree and in xrefs. That is how the
PIOC-OS kernel at 0x3498 (reachable only via TRAP #2) and the 13KB never-disassembled bank at
0x68BDE-0x6C077 were found.

---

## 4. ENCOS-specific vectors identified so far

| Vector | Offset | What it is |
|---|---|---|
| 34 (TRAP #2) | 0x088 | PIOC-OS kernel entry - function code in D0, argument block in A0; dispatcher bounds-checks D0 to 0..0x1A via `tbl_piocOsTrap2FunctionDispatch` |
| 32 (TRAP #0) | 0x080 | same handler as TRAP #2 |
| 69 | 0x114 | RTC / MFP timer tick - drives preemptive scheduling, bumps tick counters at 0x0FC2 / 0x0FCA |
| 78 | 0x138 | ND-100 host doorbell - statically points at the default handler, but POMNPROCES saves that to 0x199E8 at runtime and installs its own in front, chaining back with `move.l (0x199E8).l,-(SP) ; rts` |

The vector table can also be patched at run time, so a live card may differ from the image.

---

## 5. PIOC-OS trap #2 kernel - all 27 services decoded (2026-07-26)

`tbl_piocOsTrap2FunctionDispatch` @ **0x0C6A**, 27 longwords indexed by D0 (0..0x1A). Read from
the image bytes; the four entries already known (0x0F, 0x11, 0x12, 0x13, 0x1A) matched exactly,
which validates the read. The full decode is a plate comment on 0x0C6A in the Ghidra database and
in the `ghidra-planc` skill; the summary:

| Code | Handler | Service |
|---|---|---|
| 0x00 | 0x2E72 | resolve + validate current process |
| 0x01 | 0x2DEC | composite: claim slot then slot-op |
| 0x02 | 0x2D94 | wrapper around 0x14 |
| 0x03 | 0x2E98 | create object/process |
| 0x04 | 0x3046 | start process |
| 0x05 | 0x30F2 | **delete object** - clears the object-table slot at 0x0A8A[handle] |
| 0x06 | 0x3176 | handle existence check |
| 0x07 | 0x31D4 | **lookup object by name -> handle** |
| 0x08 | 0x3218 | **get object name by handle** (inverse of 0x07) |
| 0x09 | 0x3476 | post/signal; fast-pathed at 0x349C, slow path runs the FREE idle entry 0x1A96 |
| 0x0A | 0x3286 | store arg+4 into currentProc+0x7A, then 0x2BD8 |
| 0x0B | 0x32CA | identical to 0x0A but calls 0x2C34 |
| 0x0C | 0x330E | **atomic read-and-clear of pending events** at currentProc+0x76 |
| 0x0D | 0x3EA6 | timer arm (caveat below) |
| 0x0E | 0x3FBE | **timer cancel**, wildcard when arg+2 == 0, -6 if no match |
| 0x0F | 0x3396 | install a 68000 exception vector |
| 0x10 | 0x33EA | store arg longword into global 0x050E |
| 0x11 | 0x31A8 | get own process handle |
| 0x12 | 0x2340 | claim slot ownership |
| 0x13 | 0x23CA | release slot ownership |
| 0x14 | 0x2424 | slot table lookup via the word array at 0x0B96 |
| 0x15 | 0x24AA | slot operation |
| 0x16 | 0x40A2 | **unimplemented**, returns -0x0B |
| 0x17 | 0x1F08 | **unimplemented**, returns -0x0B |
| 0x18 | 0x403A | timer family; arg[0] must be **2**, not 1 |
| 0x19 | 0x3348 | **suspend/block self** - sets reschedule flag 0x0660, sets bit 1 of currentProc flags |
| 0x1A | 0x3150 | terminate process |

Object lifecycle is now complete and symmetric: **create** 0x03, **start** 0x04, **look up by
name** 0x07, **name from handle** 0x08, **delete** 0x05 (frees the table slot), **terminate self**
0x1A. That 0x05/0x1A pair was previously conflated.

### Two corrections

1. **0x19 is not unimplemented.** It had been recorded as "raises `#PRERR 0x16`". That was a
   misread of an inline frame descriptor - handlers open `jsr 0x4492 ; dc.l N` where N is the
   PLANC **frame size** (skip distance 4), and Ghidra renders those bytes as `ori.b #N,D0b`. The
   `0x16` was a frame size. 0x19 suspends the caller. The genuinely unimplemented services are
   **0x16 and 0x17**, both returning -0x0B.
2. **0x0D "wait / await event" is unverified.** Its body is timer work and it pairs with 0x0E
   (cancel), so "arm timer" fits better - but a wait-with-timeout would look the same from
   outside. Recorded as unresolved rather than relabelled.

### Incidental findings

- **Argument blocks are versioned.** Most begin with a word that must equal 1; **code 0x18 requires
  2**. A mismatch returns -1 and does nothing else. Useful as an emulation conformance check.
- **All processor faults funnel into `trap #1`.** The 2-byte stubs at 0x1F24+ that vectors 4-11
  point at are each `4E 41` (`trap #1`). So vector 33 is the fault reporter and the stub ADDRESS is
  what identifies the fault.
- **Several kernel handlers are still mis-disassembled** - 0x2E72, 0x3336 and 0x3396 show as
  undefined bytes or misaligned instructions despite being live code. Feeds directly into the dark
  code sweep.

---

## 6. Dark code sweep (2026-07-26)

### 6.1 The three mis-disassembled kernel sites - FIXED

Cleared and re-disassembled. All three were live code showing as undefined or misaligned bytes:

- **0x2E72** (trap 0x00) - the `jsr 0x4492` + 4-byte inline frame descriptor was raw bytes, so the
  body disassembled misaligned. Split: instruction at 0x2E72, descriptor left as data, body from
  0x2E7C.
- **0x3336** (inside trap 0x0C) - now reads `move.l (0x76,A2),(A1)` / `clr.l (0x76,A2)` /
  `move (SP)+,SR`. This **verifies** the atomic read-and-clear of pending events that section 5
  had only inferred from raw bytes.
- **0x3396** (`PosTrap0FInstallExceptionVector`) - was entirely undefined despite carrying a name.
  Now readable and it confirms the recorded behaviour exactly: argument block
  `{word vectorNum, long handler}`, vector validated 8..255, written to `vectorTable[vec*4]`.

### 6.2 The 13KB "dark bank" was already resolved - and is now IDENTIFIED

The skill's open item was **stale**. 0x68BDE-0x6C077 is fully disassembled *and* carved into
functions - the program holds 451 functions including bank 3's own PLANC leaf-runtime copy
(`PlancFrameAllocator_copy3` @0x6BE20, `PlancXRET_copy3` @0x6BE92, `PlancIMul_copy3` @0x6BCF0).

**What bank 3 IS**, newly established: **Ethernet diagnostic / reporting code.** Its data segment
carries the LANCE error message table at 0x65F00-0x663DF - fixed-width entries separated by the
PLANC format marker `I14$`:

```
"bad address received"  "restarts"  "missing transceiver heart beat"
"jabber detected"       "memory error"  ...
```

The routines match that role: ASCII digit conversion at 0x69180, field formatting, counter
readers. ~56 routines remain unnamed - that is the next tranche of work.

### 6.3 A hypothesis tested and rejected

Bank-3 code at 0x68C00 calls `0x0000BCF0` - a low address - while its own runtime copy sits at
0x6BCF0, exactly 0x60000 higher. That looked like strong evidence for a **banked overlay** whose
absolute references were un-relocated, which would have meant every bank-3 xref in the database is
wrong.

**It is not true.** `FUN_00069D94` reads `(0x00062F82).l` - its OWN data segment, a full bank-3
address. Bank 3 is linked at its true address. The apparent offset is a coincidence of the low 16
bits: 0xBCF0 is the body of `COPY` (0xBCDE) past its prologue, so the call is a legitimate
cross-bank call into shared low-bank code.

**Do not "fix" bank-3 xrefs by adding 0x60000.** Recorded as a plate comment on 0x68BDE so the
next reader does not re-derive and act on the wrong version.

---

## 6.4 The statistics report label table - VENDOR wording for LNMASTATIS

Chasing bank 3 turned up something better than routine names: the **printed report labels**, at
0x65B76 onward, referenced by `LNPRINTMAS` / `maybe_print_statistics` / `LNOutTextAndNumber` in the
LOW bank. Entries are fixed-width, separated by PLANC format markers (`$`, `I14$`, `I5$`), and the
first fourteen line up **exactly** with the `LNMASTATIS` counter order established from code:

| # | Counter | Vendor label |
|---|---|---|
| 1 | `STAT_txFramesOk` 0x1888E | "frames transmitted successfully" |
| 2 | `STAT_txOneRetry` 0x18892 | "including after one collision" |
| 3 | `STAT_txMoreThanOneRetry` 0x18896 | "and after multiple collisions" |
| 4 | `STAT_txRetryErrors` 0x1889A | "frames aborted(excess collisions)" |
| 5 | `STAT_rxFramesDelivered` 0x1889C | "frames received and given to user" |
| 6 | `STAT_rxDroppedNoPoolNode` 0x188A0 | "received and dropped" |
| 7 | `STAT_rxMissedPackets` 0x188A4 | "missed" |
| 8 | `STAT_rxCrcErrors` 0x188A8 | "CRC errors" |
| 9 | `STAT_rxFramingErrors` 0x188AA | **"alignment errors"** |
| 10 | `STAT_rxOverflowErrors` 0x188AC | "FIFO overflows" |
| 11 | `STAT_rxBufferErrors` 0x188AE | "buffer overflows" |
| 12 | **`STAT_rx8023LengthMismatch` 0x188B0** | **"bad MA length field"** |
| 13 | `STAT_maRestartsAfterHwError` 0x188B2 | "restarts" |
| 14 | `STAT_txLostCarrier` 0x188B4 | "loss of carrier" |

Two payoffs:

- **Independent vendor confirmation of the Ethernet II finding.** The counter that ticks for every
  DIX/Ethernet II frame dropped while `g_mode8023LengthField` is nonzero is called, by ND
  themselves, **"bad MA length field"**. That is precisely a length-field semantic check, not a
  generic error - exactly what the mode-word analysis predicted.
- Counter 9 should be read as ND read it: **"alignment errors"**, not "framing errors".

**An unresolved discrepancy, recorded rather than papered over.** After "loss of carrier" the
labels continue "jabber detected", "bad length", "unexpected interrupts", "buffer request when
active" - which do NOT line up with the code-derived names for 0x188B6-0x188C4
(`rxFrameLengthOutOfRange`, `rxAddressFilterRejects`, `hwErrorsCodeMinus1`, ...). Either the print
order stops tracking memory order past entry 14, or one of the two analyses is wrong for the tail.
**Do not assume the 1:1 mapping continues past counter 14** until the print routine's own order is
read out.

Note also a SEPARATE table at 0x65F00 - hardware error text ("bad address received", "missing
transceiver heart beat", "jabber detected", "memory error"). Overlapping vocabulary with the
statistics labels; do not conflate the two.

---

## 7. Open / next

- Run Revert then Apply on the ENCOS program to clear the accumulated duplicate labels.
- Name the ~56 remaining bank-3 routines. **Note the entry point is still unknown:** a byte search
  found NO `jsr`/`jmp` absolute to any 0x6xxxx address anywhere in the image, and `bsr` cannot
  reach that far. Bank-3 CODE is therefore not called from the low banks by any direct control
  transfer, while bank-3 DATA is referenced constantly. Resolve how (or whether) bank-3 code is
  entered before investing in naming it - it may be linked-in but unreachable in this build.
- Read the statistics print routine's own emission order to settle the counter 15+ discrepancy in
  section 6.4.
- Resolve the callees that would settle the remaining trap semantics: **0x2BD8 / 0x2C34** (the
  0x0A/0x0B pair), **0x21D6** (what 0x19 blocks on), **0x3D9E / 0x3E00** (timer engine), and
  **global 0x050E** (what 0x10 installs - it sits 4 bytes below `tbl_piocOsSchedulerActionDispatch`
  @0x0512, which may or may not be meaningful).
- Vectors 64-255 other than 69 and 78 are still `DeviceIrq###` - the devices behind them are not
  yet identified.
- ~~Check the other six scripts for the same duplicate-label defect.~~ **DONE 2026-07-26 - see
  section 8. No other script is affected.**
- ~~Dark code sweep.~~ **DONE 2026-07-26 - see section 6.**

---

## 8. Re-run safety audit of the other six scripts (2026-07-26)

All six were checked for the section 2.1 defect and for the general question "what happens on the
second run?". **None is affected.** `M68kVectorTable` was the only script that created labels.

| Script | Writes | Second run |
|---|---|---|
| `PlancSetupTypes` | data types, namespaces, function comments | SAFE - `addType()` returns early if the type already exists; the namespace move short-circuits when the symbol is already in the target namespace; the runtime-library comment is only written when the existing one is empty |
| `PlancFrameTypes` | `PlancFrame` struct, variable retypes | ~~SAFE~~ **WRONG - see 8.1** |
| `PlancAnnotate` | EOL / PRE / function comments, function tags | SAFE - `setCmt()` leaves any non-empty comment alone (`APPEND_TO_EXISTING` is `false`, and even when enabled it skips text already containing "PLANC"); `Function.addTag` is set-valued |
| `PlancFixFlow` | disassembly, one EOL comment per inline descriptor | SAFE - `setComment` REPLACES rather than appends, and the disassembly work is guarded by the `disasmAttempted` / `callsDone` / `disasmFailed` sets |
| `PlancApplyConvention` | calling convention on functions | SAFE - setting a convention that is already set is a no-op |
| `PlancUndoFixFlow` | clears code units | SAFE by construction - it only removes |

The distinction that matters: **Ghidra's `setComment` replaces, but `createLabel` adds.** That
asymmetry is the whole reason only the label-writing script had the bug. Any future script here
that creates labels must go through a delete-then-create helper.

### 8.1 The audit was WRONG about PlancFrameTypes - corrected 2026-07-26

The table above originally said `PlancFrameTypes` was re-run safe because "retyping a variable to
the type it already has is a no-op". That reasoning only covered the TYPE. The script also
**renames** the variable to `frame`, and a real second run failed on **11 functions**:

```
failed on POCSSpCommAppend @ 0000bbbe :
    A Local Var symbol with name frame already exists in namespace POCSSpCommAppend
```

Cause: run 1 named one variable `frame`. On run 2 the decompiler had settled on a *different*
variable as the frame pointer, and renaming that one collided with the survivor from run 1.

Two lessons, both now encoded in the script:

1. **Idempotence must be checked per side effect, not per script.** The type write was idempotent;
   the name write was not. Auditing "does re-running break it?" one operation at a time would have
   caught this - auditing it script-by-script did not.
2. `chooseName()` now keeps the existing name when another local already owns `frame`. A correct
   type with an ugly name beats an exception that types nothing.

### 8.2 Stale type definitions were also being defended

Both type scripts reported "already exists - left alone" and therefore kept the OLD, wrong
`PlancFrame` (+0x04 as `frameLimit`, 32-bit errcode) and `PlancArrayDescriptor` (12-byte)
definitions after those were corrected. Same defend-my-own-stale-output shape as the label bug.

Fixed: `PlancSetupTypes` gained a **Replace** choice and `PlancFrameTypes` a **Rebuild** choice,
both using `dtm.replaceDataType(...)` so every variable already typed with the struct follows the
new layout automatically. No hand-deleting in the Data Type Manager is needed.

One weak spot worth knowing about, not currently live: `PlancAnnotate`'s append guard tests
`existing.contains("PLANC")` to decide whether a comment is its own. That is a substring heuristic
and would misfire on a hand-written comment that happens to mention PLANC - which, in this
codebase, is most of them. It is harmless while `APPEND_TO_EXISTING = false`. Do not flip that
flag without replacing the heuristic with a real marker.

---

## Related documents

- `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\COSMOS-RE\ETHERNET-II-FEASIBILITY-AND-MODE-WORD-RE-2026-07-25.md`
- `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\stripped\docs\ENCOS-FIRMWARE-SYMBOL-TABLE-2026-07-26.md`
- `E:\Dev\Ronny\NDInsight\tools\ghidra-planc\README.md`
