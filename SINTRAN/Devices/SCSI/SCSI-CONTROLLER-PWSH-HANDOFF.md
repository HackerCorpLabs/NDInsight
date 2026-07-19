# SCSI Controller Fix - Handoff for a Windows/PowerShell Claude session

**Full path:** `SINTRAN/Devices/SCSI/SCSI-CONTROLLER-PWSH-HANDOFF.md`
(WSL: `SINTRAN/Devices/SCSI/SCSI-CONTROLLER-PWSH-HANDOFF.md`)

**Why this handoff exists:** the investigation needs to run natively on Windows/PowerShell - to
launch the RetroCore **console** executable, reach the **Ghidra** MCP (the ND-3201 ROM project is
open there), and hit the RetroCore **DAP** server on `127.0.0.1`. A WSL session cannot do those
cleanly. This document is self-contained: everything verified so far, the exact open problem, the
two work-tracks, all file paths (Windows form), and the constraints.

All ND-100 addresses are **octal** unless prefixed `0x`. C# values are hex/decimal.

---

## 0. THE MISSION (one line)
Make `@ENTER-DIRECTORY,,DISC-SCSI-1,0` mount a SCSI disk in the RetroCore ND-100 emulator. It boots
fine, but the mount never completes: the disk directory is never entered. SMD disks mount; SCSI does
not. Fails on SINTRAN K, L, and M.

---

## 0.5 RESOLUTION (2026-07-14) - READ THIS FIRST; it overrides parts of Sections 2-4

The "final `WCONT=5` does nothing" thread is **CLOSED as correct emulator behavior**. Proven
statically from the driver NPL (`SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DRIV.NPL`):

- Line ~187 `5\/SCCCW; T:=HDEV+WCONT; *IOXT` then `GO SCWTI` is the **unconditional common TAIL of
  the interrupt handler** - every path that services an NCR interrupt falls into it on the way out.
  It is not a request for a transfer; it means "done servicing this interrupt, re-arm so a future NCR
  event can interrupt me," then park at `SCWTI`.
- `SCCCW` is zeroed on handler entry (line ~139 `0=:SCCCW`) and only set (to the DMA/direction bit)
  when a data transfer needs it (line ~746). So `5\/SCCCW` with `SCCCW=0` **is** the `0x0005` in the
  trace; the `0x0065` on real transfers is the identical line with the DMA bit OR'd in. The final GO
  is therefore a **content-free re-arm** by construction, not a stalled transfer.
- The interrupt it had just serviced was the **Disconnect** (target hung up because READ_6 finished).
  No pending work -> no bus event -> no interrupt should fire. The emulator is right.
- **Making GO fire an IRQ would be a BUG:** a spurious IRQ re-enters `SCINT`, which reads RSTAU,
  finds the NCR-interrupt bit (11) clear, does nothing, hits line 187, re-arms, parks - an interrupt
  storm that services nothing. `ExecuteGo` completing only when `InterruptFromNCR5386` is set matches
  the hardware contract: a GO completes when the NCR has something to report.

**Consequences:**
- **A/B ambiguity (Section 2.2) is settled as B, statically. No CPU trace is needed to prove it.**
  The blocker is **CPU-side**, in SINTRAN's connect/verify layer.
- **Track 2 (Sections 3 + 4-Track-2: reverse-engineer `45900E.bin`) is DEAD - but NOT for the reason
  first given.** An earlier draft of this section argued the ROM was "a different board's firmware"
  because "the ND-100 would only see a mailbox". **That reasoning was WRONG and is retracted.**
  `45900E.bin` *is* the ND-3201's ROM (PN 350001, PCB 3201), and RetroCore explicitly emulates the
  3201 (see the header comment in `NDBusDiscControllerSCSI.cs`).

  **The correct reason (VERIFIED by an exhaustive byte-level I/O sweep of the ROM):** the ND-3201 is
  **two independent controllers on one PCB**. The Z80 + AM9517 + FD1797 are the **floppy** half. The
  NCR 5386 is hardware-decoded straight onto the ND-100 IOX bus and is driven by **SINTRAN**, which
  is the SCSI protocol engine register-by-register - it writes `WNCOM` (NCR command register), reads
  `RITRG` (NCR interrupt register), sets ATN, and programs the transfer counter and MAR itself.

  **The Z80 never touches the NCR 5386 anywhere in the 8KB ROM.** Evidence:
  - The supposed "NCR window" at Z80 ports `0x20-0x3D` is a complete **AM9517/8237 DMA** register
    file at base 0x20 (0x28 Command/Status, 0x29 Request, 0x2A Single Mask, 0x2B Mode, 0x2C Clear
    Byte Pointer, 0x2D Master Clear, channel addr/count at 0x20+2n / 0x21+2n via the byte-pointer
    flip-flop). `ram:0717` is the giveaway: `OUT (0x2b),A; AND 0x3` - extracting the DMA *channel*
    from a mode byte.
  - No port anywhere receives an NCR command code (0x00/0x01/0x03/0x04/0x08/0x09/0x0B/0x24/0x54/
    0x94/0xA4); no port is read in the "write command -> read interrupt register" NCR pattern.
  - The only command-code stream goes to port `0x70` and they are FD179x opcodes (0x02 Restore,
    0x12/0x18/0x1C Seek, 0x88/0x8C Read Sector, 0xC4 Read Address, 0xD0/0xD4 Force Interrupt,
    0xF0/0xF2 Write Track), with 0x71/0x72/0x73 = Track/Sector/Data and a `BIT 0,A` Busy poll.
  - The DMA is programmed immediately before every FD1797 data-moving command - it exists to serve
    the floppy.
  - Corroboration: the ND-3106/3112 manual (ND-11.021.1) confirms the Z80-side ports 0x50-0x57, the
    command-block format and `FDVSEL` (port 0x74 = **Floppy** Drive Select) as identical to those
    **floppy controllers**. The Z80 half of the 3201 is essentially an ND-3112.

  So the ROM is not in the SCSI path. Do not RE it for this bug. (It *is* the reference if floppy
  support is ever wanted - see revised Section 3.)

  Full corrected board reference: `SINTRAN/Devices/SCSI/nd-scsi-3201.md`.
- **The real target (was every prior static pass's conclusion):** after the last-block control-record read (function-42 connect)
  (READ_6 of LBA 129311 -> `blockSize=1024, lastLBA=129311`), the connect/verify layer decides **not
  to queue the block-0 read**. On the Disconnect the driver calls `DCTHR` (disconnect logical thread)
  and goes `BUSFP -> SELEC` (line ~147), which checks the **arbitration queue** for the next
  operation and finds nothing. **The question to chase: why was nothing queued - what does the
  connect layer do with `blockSize=1024 / lastLBA=129311`, and which comparison sends it down the
  give-up path?** That is where the fix is (likely a geometry/capacity-consistency decision, possibly
  the `ECAPD` check), and it is CPU-side, not in the SCSI controller. See revised Section 4.

---

## 1. WHAT IS VERIFIED (ground truth - do not re-litigate)

### 1.1 Running system
- Running SINTRAN = **L-VSX-500** (byte-verified: resident commoncode word `011300`&#8323; matches
  `tools/sintran-segment-carver/versions/L-VSX-500/resident/SINTRAN-DATA_commoncode.bin`;
  hex `D1 0A D0 0D CC 77 B5 00 ...`). The DAP `program` field says `BIGDISK0-M.IMG` - MISLEADING, it
  is L. Correct symbol set = **L07**.
- Disk under test: `DISC-SCSI-1`, directory **PACK-ONE**, 129312 blocks (LBA 0..129311), 1024-byte
  sectors. Block 0 = valid PACK-ONE master. Block 129311 (last) = a SINTRAN disk area/layout table
  (checksum XOR = 0, valid). Disk = memory byte-identical (no corruption).

### 1.2 What the SCSI mount actually does on the wire (from the device trace)
The full IOX/CDB/DMA/IRQ transaction is documented in
`SINTRAN/Devices/SCSI/SCSI-IOX-TRANSACTION-LOG.md` (686 lines: register map,
WCONT/RSTAU bit maps, full chronological ledger, interrupt analysis, Mermaid diagrams). Source
trace: `C:\Users\ronny\AppData\Local\trace\file-trace.txt` (device-side only; NO CPU `Opcodes;`
lines in the current capture).

Key verified facts:
- CDBs issued, in order: **INQUIRY (0x12)**, **READ CAPACITY (0x25**, mislabeled `SC_GET_WINDOW` in
  the C# enum but handled correctly; returns blockSize=1024, lastLBA=129311**)**, **READ_6 (0x08,
  lba=129311)**. That is the LAST block, read as the control-record (function-42 connect). **Block 0 is never read. No WRITE
  CDB is ever issued.** Only ONE data CDB total transfers real data.
- The `129311` in the READ_6 is **copied verbatim from the READ CAPACITY reply** (`00 01 F9 1F 00 00
  04 00`), proven earlier: `0xF91F` reaches memory only via the capacity DMA, never in a CPU
  register/store before the CDB is built.
- The READ_6 completes cleanly: `readBlock 129311` returns data, DMA to ND memory succeeds, RSTAU
  reads `0x0208/0x3208/0x5208` (**bit 4 = error-summary is CLEAR** - the old "STATUS 100020" bit-4
  theory is a STALE build and no longer applies), interrupts are raised and acked on RITRG. 24 NCR
  interrupts = 24 controller completions = 24 RITRG acks, perfectly balanced.
- **The mount is NOT a hang.** Console reconstruction: `@ENTER-DIRECTORY,,DISC-SCSI-1,0` ->
  `APPROACHING END OF ACCOUNTING FILE` -> returns to `@`. (The later `stop-system`/`WAIT with IONI
  off` halt was the operator typing `@stop-system`, not a driver deadlock.)
- `APPROACHING END OF ACCOUNTING FILE` (error 243) is a **non-fatal warning** from the accounting
  record write, and the SMD-vs-SCSI comparison is CONFOUNDED because the SMD disk (`DISC-75-1`) is
  the already-entered BOOT directory (fast path, no fresh entry). 243 is coincident, not the cause.

### 1.3 The SINTRAN L SCSI driver expectation (from NPL source, verified)
Source: `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DRIV.NPL`. Note the NPL is a
DIFFERENT revision than the running L binary, so treat addresses as indicative, logic as reliable.
- The driver services the NCR **once per SCSI phase**. Interrupt entry `SCINT` (`066651`): reads
  RSTAU, decodes the phase/status, issues the next NCR command, then re-arms and waits.
- Re-arm pattern (line ~187): `5\/SCCCW; T:=HDEV+WCONT; *IOXT   % ACTIVATE+ENABLE INTERRUPT` then
  `GO SCWTI`. `5` = WCONT bits 0+2 = **Enable-Interrupt + Active**. `SCWTI` = the driver's wait
  state. The driver **does not poll** - after writing Active it relies on the controller raising a
  completion interrupt to re-enter at `SCINT`.
- Symbols: `WCONT=05`, `RSTAU=04` (IOX offsets). `SG110=1100`&#8323; is a system/reference device
  constant (NOT a directory-format gate - an earlier `126305` "gate" theory was retracted; that PC
  is a legit fast-path vs general-connect router).

---

## 2. THE OPEN PROBLEM - the final `WCONT=5` that does nothing

This is the crux the owner wants nailed. Read the tail of the transaction (trace lines ~1795-1906),
reproduced as the essential sequence:

```
        (repeat, once per SCSI phase - the WORKING pattern)
  IOX R  RSTAU        (read status)
  IOX W  WCONT=0x0000 (disarm)
  RAUXS  (phase)
  IOX R  RITRG        (read NCR interrupt register -> CLEARS intFromNCR)
  IOX W  WNCOM=0x0054 (NCR command TransferInfo)   <-- an NCR COMMAND is loaded
  RAUXS / RNDAT
  IOX W  WCONT=0x0005 [Enable-Interrupt][Active]    -> ExecuteGo -> completion IRQ fires
        ...
  IOX W  WNCOM=0x0004 (NCR command MessageAccepted) <-- an NCR COMMAND is loaded
  IOX W  WCONT=0x0005 -> ExecuteGo -> completion IRQ fires (RSTAU 0x0208)
  IOX W  WCONT=0x0000 (disarm)
  RAUXS=0x0000
  IOX R  RITRG=0x0004 (Disconnected) -> CLEARS intFromNCR to False
  IOX W  WCONT=0x0005 [Enable-Interrupt][Active]    <-- THE FINAL GO
  ExecuteGo MAR=$0004C800
        (nothing. no completion. no IRQ. console prints the accounting msg, returns to @)
```

**The difference the owner identified is the key:** every *working* GO is preceded by a `WNCOM`
write (an NCR command: TransferInfo `0x54` or MessageAccepted `0x04`). That NCR command is what
causes the NCR chip to raise a fresh interrupt, which the controller converts into the ND-100
completion IRQ. **The final GO has NO preceding `WNCOM`** - no NCR command is loaded - and its WCONT
value is `0x0005` (Active + Enable-Interrupt only; **no DMA-enable bit 5, no Write-ND-memory bit 6**,
unlike a real data transfer which used `0x0065`). So the controller is armed with nothing to do.

### 2.1 Why the emulator produces no IRQ for it (VERIFIED in the C#)
File: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusDiscControllerSCSI.cs`
- WCONT write handler (~line 1095-1176): setting Active (bit 2) calls `ExecuteGo()`.
- `ExecuteGo()` (~line 1372): **is a stub** - it logs the banner and `return;`s at ~line 1394; all
  the real command logic below is dead code (`#if _GO_`, unreachable).
- The ONLY path that raises the ND-100 completion IRQ is `StepGoState()` (~line 1242), driven each
  `Clock()` while `regs.active`, and it fires `SetInterruptBit(true)` **only if
  `regs.InterruptFromNCR5386 == true`** (~line 1247-1255).
- `Ncr5386_OnInterrupt` (~line 763) sets `regs.InterruptFromNCR5386 = true` when the NCR raises an
  interrupt. `RITRG` read (~line 998) clears it.
- Therefore: final GO -> `RITRG` just cleared `InterruptFromNCR5386` -> no NCR command loaded to set
  it again -> `StepGoState` never completes -> `regs.active` latches true forever, no IRQ. **Dead.**

### 2.2 SETTLED (see Section 0.5): reading B - the controller is correct here
This was formerly an open A/B ambiguity. It is now resolved **statically** from the driver NPL: the
final `WCONT=5` is the interrupt handler's unconditional re-arm tail (`5\/SCCCW`, `SCCCW=0`), issued
after servicing the Disconnect that ended the completed READ_6. No pending work -> no NCR event -> no
interrupt should fire, and forcing one would build an interrupt storm (Section 0.5). The emulator is
correct; **the blocker is CPU-side.** Section 2.1's mechanism description is accurate but its
"defect" framing is void - that behavior is the hardware contract.

---

## 3. THE ND-3201 CONTROLLER - background only (NOT the bug path; see Section 0.5)

> **DEAD END for this bug - and note the reason has been CORRECTED.** An earlier banner here said the
> ROM was "a different board's firmware" whose Z80 "owns its NCR privately". That was wrong on both
> counts. `45900E.bin` **is** the ND-3201's ROM, and RetroCore **does** emulate the 3201.
> The real reason it is a dead end: the Z80 half never touches the NCR. See below.

### 3.1 The verified architecture: two independent halves

The ND-3201 board (Norsk Data "PCB 3201 - N-100 SCSI/Floppy Ctrl", PN 350001) chipset, read directly
from `E:\Dev\Repos\Ronny\RetroGhidra\ND3201SCSI\CHIP doc\Controller-info.pdf`:
- **Z80 CPU** + **2x Z80 CTC** + **NCR5386 (SCSI protocol controller)** + **AM9517 DMA controller** +
  **FD1797 (floppy)** + **8 KB SRAM (TMM2063)**. LEDs include "Z80 DMA". Connectors: (A) floppy,
  (B) SCSI bus, (C) ND-100 system bus.

The board is **two independent controllers sharing one PCB and one host connector** (VERIFIED by an
exhaustive byte-level I/O sweep of the ROM - see Section 0.5 for the full evidence):

| Half | Hardware | Driven by |
|------|----------|-----------|
| **SCSI** | NCR 5386 | The **ND-100**. NCR register file is hardware-decoded onto the IOX bus; SINTRAN is the SCSI protocol engine |
| **Floppy** | Z80 + AM9517 + FD1797 | The **Z80 firmware** (`45900E.bin`), ND-3112-style command block over ports 0x50-0x57 |

**The Z80 never touches the NCR 5386 anywhere in the 8KB ROM.** The chipset list above is accurate;
the *conclusion* previously drawn from it ("so the Z80 must drive the NCR") was an inference, and the
bytes refute it.

### 3.2 What this means for `ExecuteGo()`

**RetroCore's model is faithful, not a shortcut.** Exposing the NCR registers directly on the ND-100
IOX map and not emulating the Z80/AM9517 is *exactly what the hardware does* for the SCSI half. The
Z80 and AM9517 belong to the floppy half, which the emulator simply does not implement (a real gap -
but a floppy gap, not a SCSI one).

So the empty `ExecuteGo()` is **not** a missing-Z80 stub, and there is **no missing completion
behavior for the bare final GO**. `StepGoState()` raising the IRQ only when
`regs.InterruptFromNCR5386` is set matches the hardware contract: a GO completes when the NCR has
something to report. Per Section 0.5, the correct response to the final bare `WCONT=5` is silence.

### 3.3 When the Z80 ROM *does* matter

Only if floppy support is ever wanted. Then `45900E.bin` is the authoritative spec for the floppy
half, and the corrected map (verified port map, RST/IM2 vectors, FD1797 command values, POST/event
error codes, ND-3112 correspondence) is in:
`SINTRAN/Devices/SCSI/nd-scsi-3201.md`

---

## 4. TWO WORK-TRACKS

### Track 1 - Unblock live debugging (the A/B ambiguity it was meant to settle is now CLOSED)

> **Scope change per Section 0.5.** Track 1's *purpose* was to get a CPU trace to discriminate A vs B.
> **That is settled statically as B - no trace is needed for it.** What remains useful here is only
> **1a** (the DAP thread-safety fix, so live debugging works at all), because you will still want a
> working debugger to chase the CPU-side connect/verify layer. **1b's specific mission (park-at-SCWTI
> vs not) is obsolete** - `GO SCWTI` is the driver's normal exit on *every* path, so "parked at
> SCWTI" was never going to discriminate anything. Keep 1b's DAP quirks as reference for whatever
> breakpoints you do set.

**1a. DAP thread-safety fix - ALREADY APPLIED to RetroCore source (build-clean, NOT runtime-tested).**
Root cause (verified, documented in
`SINTRAN/Emulator/RETROCORE-DAP-THREADSAFETY.md`): the crash
`NullReferenceException at Instructions.RADD() -> doROP()` is NOT an RADD bug. It is a race: two CPU
execution pumps run concurrently under DISJOINT locks. One thread executes
`regs.fetchedInstructionRef = null;` (`CpuND100.cs:298`) while the other is mid-instruction reading
`regs.fetched.roRAD` (`Instructions.RegisterOperations.cs:304`). The single serialization point is
`machine.CpuExecutionGate`; `machine.DapOwnsCpu` signals DAP ownership.

**The racing pump was the console `BOOT` command**, `BootDevice` at
`E:\Dev\Repos\Ronny\RetroCore\Emulated.Debugger\DebugCommands.Devices.cs:78`. It ran
`machine.Run((ushort)startAddress, -1)` (run-until-stop) holding **no lock** and never checking
`DapOwnsCpu`. The console hosts the DAP TCP server
(`Emulated.Debugger\DebugCommands.DebugStepper.cs:115`); a DAP `continue` spawns the gated
`DapRunLoop` (`MachineBaseDAP.cs:270-272`) which returns immediately and leaves the `ND-100[...]>`
prompt live, so a `BOOT` at that prompt started a second, un-gated `Run()` -> two pumps -> the NRE.
(`doRun` already refused in this state; `BootDevice` was the hole.)

**Applied edits (verify with `git diff` in the RetroCore repo; build-clean, 0 errors; owner has NOT
committed - do not commit without asking):**
1. `E:\Dev\Repos\Ronny\RetroCore\Emulated.Debugger\DebugCommands.Devices.cs` (~78): `BootDevice` now
   refuses when `machine.DapOwnsCpu || !Monitor.TryEnter(machine.CpuExecutionGate)` (same message as
   `doRun`), else runs inside `try/finally { Monitor.Exit(gate) }`. This is the fix that matters for
   the console host.
2. `E:\Dev\Repos\Ronny\RetroCore\Emulated.UI.Avalon\Services\EmulationService.cs` (~1567): frame pump
   now skips when `DapOwnsCpu`/gate held, else runs under the gate (was `_dapLock` only).
3. `E:\Dev\Repos\Ronny\RetroCore\Emulated.UI.SDL2\RetroEmulation.cs` (~483): frame pump now takes the
   gate / respects `DapOwnsCpu` (was lockless).
All three use non-blocking `Monitor.TryEnter` and SKIP/refuse when the gate is held, so hosts yield
to the DAP session without blocking (no deadlock; matches lock order `_dapLock -> _cpuExecutionGate`).

**Residual (a reviewer / this session must check):**
- **Not runtime-verified** - confirm a DAP `continue` on a booted L no longer crashes.
- Two console dev-tool steppers remain un-gated and were DEFERRED (they need an external remote/SIMH
  and run step-by-step doing register compares): gRPC `StepCpuND100`/`SyncStepCpuND100`
  (`DebugCommands.DebugStepperND100.cs:79,136`) and the SIMH cross-compare loop
  (`DebugCommands.SimhSync.cs:236,240`). Not in the SCSI-mount path; ignore unless you use them.
- Avalon skips its `Run()` each frame while DAP owns the CPU - confirm existing frame pacing prevents
  a busy-spin during long DAP sessions (pacing note, not a deadlock; irrelevant to the console host).
- The transient `regs.fetchedInstructionRef = null;` at `CpuND100.cs:298` was left in place (minimal
  change); no null-guard was added inside `doROP` (that would mask, not fix, the race).

**1b. Get the decisive CPU trace.** With the emulator surviving a DAP session:
- Restart the console, boot L with the SCSI disk to `@`.
- Set a breakpoint at `SCINT` (the SCSI interrupt entry; L07 resident address - find it: NPL name
  `SCINT`, the routine that does `T:=HDEV+RSTAU; *IOXT` on entry) and observe whether the driver
  **re-enters `SCINT`** after the final GO (=> it got an IRQ, reading B) or **never does / is parked
  at `SCWTI`** (=> reading A, controller is the blocker).
- Equivalent: enable a CPU+device trace (WIDE_CPU_TRACE) and read the PC stream after the final GO.
- DAP quirks (learned the hard way) are in
  `SINTRAN/Devices/SCSI/SCSI-MOUNT-DEBUG-HANDOFF.md` sections 3-4: breakpoints
  ACCUMULATE (only a restart clears them), `continue` re-triggers the current PC (budget 2 continues
  per bp), no single-step, use HIGH addresses only, console is owned by DAP while paused. From
  Windows, DAP is `127.0.0.1:4712` (no WSL IP juggling needed).

**This one trace decides whether Track 2 (controller rewrite) is on the critical path or a detour.**

### Track 2 - CANCELLED. The real target: SINTRAN's connect/verify layer (CPU-side)

Per Section 0.5 the controller behaves correctly and the ND-3201 Z80 RE is a dead end. The fix is
CPU-side: after the last-block control-record read (function-42 connect) returns `blockSize=1024 / lastLBA=129311`, the connect/
verify layer decides not to queue the block-0 (directory-master) read, and on the Disconnect goes
`DCTHR -> BUSFP -> SELEC` (line ~147), finds the arbitration queue empty, and gives up.

**Chase this (needs the DAP fix from Track 1a to debug live):**
1. Find where the mass-storage connect/verify layer consumes the probe result. The probe is issued by
   the resident mass-storage connect/verify overlay (uncarved in the L carve). READ CAPACITY reports
   raw `129311`; block-0's PACK-ONE master claims capacity 61036 pages = 122072 usable blocks. The
   suspect is a **capacity/geometry-consistency comparison** (candidate: the `ECAPD` check - reporting
   `122071` was already rejected by it, and raw `129311` also does not mount, so the value SINTRAN
   wants is neither; pin what it actually compares).
2. Live method (PowerShell): after the DAP fix, boot L, breakpoint the connect/verify path and the
   ENDIR/COLDE/GDIRE workers (L07 addresses in `SCSI-MOUNT-DEBUG-HANDOFF.md` Section 4), run the
   mount, and watch which comparison branches to the give-up path instead of queueing the block-0
   read. Read the device datafield (`dspace:`) built from the probe and diff SCSI vs the working SMD
   datafield.
3. The fix is then either an emulator-side geometry/capacity value the controller should present so
   the consistency check passes, OR (if it is a genuine SINTRAN limitation) a documented
   incompatibility - decide with the owner once the exact comparison is pinned. Do NOT blind-guess
   capacity numbers (Section 5 note).

The former Ghidra RE instructions are retained below only if someone wants to annotate the ND-3201
firmware for its own sake - it will NOT fix this bug.

#### (archived - ND-3201 Ghidra annotation, not on the bug path)

Do the real Ghidra work (the owner explicitly does NOT want the stale PDF export
`Z80ASM_SCSI.pdf` - work the live program). Ghidra is open with the ROM.

**Ghidra project:** `E:\Dev\Repos\Ronny\RetroGhidra\ND3201SCSI\ND3201SCSIController.gpr`
**ROM binary:** `E:\Dev\Repos\Ronny\RetroGhidra\ND3201SCSI\45900E.bin` (8 KB Z80, `ram:0000-1fff`).
**Ghidra MCP tools available in the Windows session** (from that project's
`.claude\settings.local.json`): `mcp__ghidra__get_program_info`, `list_functions`,
`get_disassembly`, `get_code`, `get_function_info`, `xrefs`, `get_call_graph`, `list_strings`,
`get_hexdump`, `search_bytes`, `disassemble`, `rename_symbol_batch`, `set_comment`, `list_programs`,
`get_task_status`. (These are NOT reachable from WSL - that is the whole reason for this handoff.)

**Do this in Ghidra:**
1. Map the Z80 I/O ports and memory-mapped windows. From the ROM's `MAIN` (`ram:003b`): it does
   `OUT (0x70),A; OUT (0x54),A; OUT (0x55),A; OUT (0x56),A` at init, and the main loop reads a
   memory-mapped region at `0x2000`. Identify which ports are the NCR5386, which are the AM9517 DMA
   controller, which are the Z80 CTC, and which are the ND-100 interface (the WCONT/RSTAU side / the
   shared SRAM mailbox). Use the chip docs: `E:\Dev\Repos\Ronny\RetroGhidra\ND3201SCSI\CHIP
   doc\NCR5386_protocol_controller.pdf`, `...\Z80-CTC.pdf`, and the AM9517 datasheet; the SCSI
   protocol references are in `...\ND3201SCSI\Books\`.
2. **Rename `FUN_ram_*`, `LAB_ram_*`, and I/O accesses** to describe their role (e.g.
   `ncr_write_cmd`, `dma_setup`, `nd100_irq_raise`, `wait_ncr_int`, `handle_reselect`), and
   **`set_comment`** the key logic. Use `rename_symbol_batch` and `set_comment`. Follow `xrefs` and
   `get_call_graph` from the RST/NMI vectors and `MAIN`.
3. **Answer the controlling question:** when the ND-100 sets the Active/GO bit (and the NCR is idle /
   just disconnected, with no fresh command loaded), what does the firmware do, and **when/how does
   it raise the completion interrupt to the ND-100?** Specifically: does a GO after a target
   Disconnect arm a reselection wait, auto-complete, or genuinely do nothing until new work? That is
   the direct answer to the Section 2 ambiguity from the hardware's own spec.
4. Write the findings (renamed map + the WCONT/GO/interrupt state machine) to a doc, e.g.
   `SINTRAN/Devices/SCSI/ND3201-FIRMWARE-ANALYSIS.md`.

**Then map into the C#** (`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusDiscControllerSCSI.cs`
and the NCR model in `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\NCR\SCSI\NCR5386\`):
- If the firmware shows a GO must produce a completion/interrupt in the observed state, implement
  that in `ExecuteGo()`/`StepGoState()` (currently `ExecuteGo` is an empty stub - Section 2.1).
- Decide with the owner whether it is a tiny targeted fix (make the specific bare-GO-after-disconnect
  path complete) or a larger refactor toward modeling the Z80/AM9517 handshake. Do NOT rewrite blind
  - Track 1's trace tells you if the controller is even the blocker.

---

## 5. C# CONTROLLER CODE MAP (RetroCore, Windows paths)
- Controller: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusDiscControllerSCSI.cs`
  - IOX read switch incl. `RSTAU` bit assembler (~874-946), `RITRG` ack (~996-1000).
  - IOX write switch incl. `WCONT` handler (~1064-1176), `WNCOM`/`WTC*`/`WHMAR`/`WDESI` (~1189-1219).
  - `Ncr5386_OnInterrupt` (~763), `Clock` -> `StepGoState` (~828-837), `StepGoState` (~1242),
    `ExecuteGo` stub (~1372-1394, dead code below).
- NCR 5386 chip: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\NCR\SCSI\NCR5386\NCR5386SCSI.cs`
  (+ `.CommandHandling.cs`, `.StateHandling.cs`, `Enums.cs`). Interrupt-register bits (`Enums.cs`
  ~847-873): `FunctionComplete=0x01`, `BusService=0x02`, `Disconnected=0x04`.
- Disk target: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\Common\SCSI\SCSIHDD.cs`,
  `...\SCSIHDDMicropolis.cs` (READ CAPACITY reply, READ_6). READ CAPACITY reports raw `129311`; do
  NOT blind-guess capacity numbers - `122071` was already rejected by SINTRAN's `ECAPD` check and
  raw `129311` also does not mount. The gate is CPU-side / firmware, not a magic capacity value.
- CPU (for the DAP race): `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND100\CpuND100.cs`
  (`TickCpuUnsafe` 276, `fetchedInstructionRef` null at 298, `ExecuteDecodedInstruction` guard 315),
  `Instructions.RegisterOperations.cs` (`doROP` 302, deref at 304), `Registers.cs:252` (`fetched`
  property). DAP host: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\Common\Machine\MachineBaseDAP.cs`
  (`_cpuExecutionGate` 191, `DapRunLoop` 262). Console host: `RetroCore\Program.cs`,
  `RetroCore\FrontendCommands.cs`, `RetroCore\Menu.cs`, `Emulated.Debugger\DebugCommands.Machines.cs`.

---

## 6. REFERENCE DOCS (NDInsight, Windows paths)
- `SINTRAN/Devices/SCSI/SCSI-MOUNT-DEBUG-HANDOFF.md` - live-debug state, DAP
  quirks, L07 addresses, ruled-out theories (read sections 6b, 7, 7b).
- `SINTRAN/Devices/SCSI/SCSI-IOX-TRANSACTION-LOG.md` - full IOX ledger +
  interrupt analysis + diagrams (the device-side reference for Section 2).
- `SINTRAN/Emulator/RETROCORE-DAP-THREADSAFETY.md` - the DAP race analysis.
- `SINTRAN/Devices/SCSI/scsi-transfer-status.md`,
  `...\scsi-open-last-block-read.md`, `...\mount-gate-diff.md`,
  `SINTRAN/Filesystem/code-logic/scsi-mount-geometry.md` - prior RE (note the
  retractions inside them; the CORRECTED/6b sections supersede earlier claims).
- SINTRAN SCSI driver NPL: `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DRIV.NPL`
  (+ `IP-P2-SCSI-DISK.NPL`, `IP-P2-SCSI-OPDI.NPL`).
- Trace: `C:\Users\ronny\AppData\Local\trace\file-trace.txt` (regenerate per
  `SINTRAN/Devices/SCSI/SCSI-TRACE-HOWTO.md`; `SCSIDevice.ScsiTrace` default
  true; `Logger.EnableLogger(Device, File)`).

---

## 7. CONSTRAINTS (always)
- **RetroCore is a separate repo.** Editing source to implement a fix is fine, but **do NOT commit**
  there without the owner's explicit OK, and never run destructive git commands (`reset`,
  `checkout`, `restore`, `clean`, `stash`, branch switch, `--no-verify`).
- Never mention any AI assistant/tool in code, comments, commits, or documents.
- Full absolute paths whenever a file is named (this is a handoff read cold by another session).
- No Unicode in `.cs`/`.ASM`/`.NPL` or anything fed to period compilers/assemblers.
- Bytes/traces/ROM are ground truth; NPL is a different revision; verify, do not claim. Label
  VERIFIED vs INFERRED.
- Mermaid diagrams follow `MERMAID_COLOR_STANDARDS.md` (WCAG 2.1 AA).

---

## 8. RECOMMENDED ORDER (revised per Section 0.5)
1. Verify the DAP thread-safety fix (Track 1a, already applied to source): confirm the console
   `dotnet build` is clean and a DAP `continue` on a booted L no longer crashes. This is the enabler
   for live CPU debugging.
2. Chase the CPU-side blocker (Track 2, revised): with live debugging working, find the connect/
   verify comparison that consumes `blockSize=1024 / lastLBA=129311` and branches to the give-up path
   (`DCTHR -> BUSFP -> SELEC`, empty queue) instead of queueing the block-0 read. Pin the exact
   comparison (capacity/geometry consistency; candidate `ECAPD`).
3. Implement the fix where the pinned comparison lives - likely an emulator-side geometry/capacity
   value the SCSI target must present, or a documented incompatibility. Do NOT touch the SCSI
   controller's GO/interrupt logic and do NOT reverse-engineer `45900E.bin` - both are confirmed off
   the bug path (Section 0.5).
