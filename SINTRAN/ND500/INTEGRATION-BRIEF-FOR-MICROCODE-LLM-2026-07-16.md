# Integration brief: ND-500 (3022) + ND-5000 (Octobus) CPU wiring - question for the microcode LLM

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\INTEGRATION-BRIEF-FOR-MICROCODE-LLM-2026-07-16.md`
**From:** the bus-interface session (RetroCore NDBusND500IF work). **To:** the ND-5000 microcode
analysis session. **Date:** 2026-07-16.

## THE QUESTION

You have reverse-engineered the B30 microcode's complete mailbox/MON-call/trap servicing
(MAILBOX-MICROCODE-PSEUDOCODE.md). We have a working 3022 bus-interface emulation whose
activate/answer engine currently PLAYS the microcode's role in C#, plus a macro-instruction
CpuND500 emulator that is NOT yet connected to the interface.

**Propose the concrete integration architecture** for BOTH generations:
1. **Classic ND-500**: CpuND500 behind the 3022/5015 (level-12 doorbell, TAG plumbing, MPM window).
2. **ND-5000**: CpuND500 behind the Octobus/ACCP (OCB 100501B activate / 100401B answer strobe,
   OctobusND5000Station).

Specifically:
- Where should the microcode-role logic live so BOTH doorbells share it (one "MicrocodeServicer"
  consumed by NDBusND500IF and OctobusND5000Station? inside CpuND500?) - propose class boundaries.
- How the MON-call exit should be implemented in CpuND500 given your section 3.8 findings
  (CALLG into segment-31 trampoline -> fetch trap 6 -> arg (addr,value) pairs into the message
  data part -> STOPR/NUMPA/MCNO -> answer-in-place -> doorbell), mapped onto the EXISTING trap
  infrastructure (CpuND500.Trap.cs) and domain/PCB model (CpuND500.Domain.cs / MMU).
- How the restart (3MONCO: FUNCV -> X1, KFLIP -> K flag, resume at saved P:=L) plugs into
  CpuND500's register model.
- The trap-stop path (your section 3.9): local-handler check (context block + DIT) vs stop-with-
  TRAPCODE vs (5000-only) OCB out-of-band - what CpuND500 needs (DIT model? which fields?).
- Which MICFU handlers stay pure-engine (3RMICV incl. the TWO-halfword answer, 13B/14B resident
  read/write, 34B/46B on 5800) vs which route into CpuND500 (3START/3MONCO/3TRACO/3WMONCO).
- The generation switch: classic vs 5800 MICFU semantics (3SWMESS/3FITRNSF ILLEG on 5800),
  doorbell plumbing differences, and where that policy should sit.
- A phased implementation plan with per-phase tests (the existing command-shaped test style).

Flag every assumption with the repo's evidence grades. Do not propose behavior that contradicts
the live-trace findings below.

## STATE OF THE EMULATOR (what already works, live-validated 2026-07-16)

3022 side (`NDBusND500IF.cs`): register map incl. four-mode decode; strobes fire on IOX READ
(SLOC5/UNLC5/MCLR5); RETG5 bit1 stop / bit1-clear restart (5CLOST); REAL control store
(csStore 16384x9 parts, load + read-back verify - the SINTRAN loader VERIFIES words 0-7);
MAR = ND-100 WORD address; activate/answer engine: N5STA lifecycle with power-fail-bit
preservation, MICFU dispatch (1,5,23B,24B,25B,26B,27B,34B,44B + 13B resident-read implemented
from live trace), decoded MAILBOX RECV/ANSW trace logging, level 12 gated on CONTROL bit 0.
Full test suite green (1747 tests; ~100 interface/command-shaped tests).
Live status: CS load+verify+restart+watchdog+13B all pass; awaiting next live `status` verdict.

## DOCUMENTS (all findings of both sessions)

- `E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md` - YOUR pseudo-C (idle/activate,
  64-entry MICFU dispatch, MSG_END/GIVEINT, MON-call exit 3.8, traps 3.9, emulator mapping)
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-MAILBOX-MESSAGE-CATALOG.md` - the message spec
  (fields/overlays, MICFU/STOPR, ISR dispatch, MONICO write-back, swapper family, sec 7/7b cross-checks)
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-WHO-ANSWERS-THE-MAILBOX.md` - the servicer model + diagrams
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-CS-LOAD-TRACE-FINDINGS-2026-07-16.md` - live-trace record
  (load/verify protocol, strobes-on-read, MAR word addressing, STATUS timelines)
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-BUS-INTERFACE-REFERENCE.md` - the carve reference (3022)
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-STATUS-AND-INDEX.md` - status of record + full doc index
  (GOSW table, symbol values, CONTROL STORE provenance)
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-TEST-PROTOCOL-RE.md` - Octobus protocol RE (sec 4b aligned)
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\nd-500-mon\` - ND-500-MON command RE (101 mon60-callers,
  SUBFUNCTION-TABLE.md) + `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\ND500-MON-COMMAND-TEST-PLAN.md`
- `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\ND500-BUS-INTERFACE-DESIGN.md` - the emulation
  design + phased plan + section 9a status handoff
- Skill (condensed all-of-the-above): `C:\Users\ronny\.claude\skills\nd-500-bus-interface\SKILL.md`

## SOURCE CODE (RetroCore repo: E:\Dev\Repos\Ronny\RetroCore)

**ND-500 CPU (macro-instruction emulator):** `Emulated.HW\ND\CPU\ND500\`
- `CpuND500.cs` (core; NOTE: constructor IGNORES the passed Registers - live regs = cpu.CpuRegisters)
- `CpuND500.Trap.cs` (trap infrastructure - target for the CALLG/fetch-trap MON exit)
- `CpuND500.Domain.cs` (domain switch/return, DIT/PCB model), `CpuND500.MMU.cs`, `MMUConfiguration.cs`
- `CpuND500.Loader.cs` (LoadDOMFile, domain alloc), `CpuND500.Memory.cs`, `CpuND500.ND100Bridge.cs`
- `Registers.cs` (X1 etc., ST.K flag), `IND500Cpu.cs` (interface incl. SetMpmMemory/TagWritten)
- `SimulatedND500.cs` (test harness CPU; MPM Port B via bit 31)
- `Sintran\MON_*.cs` (201 ND-500-side monitor-call emulations - the existing MON semantics library)

**ND-100 CPU:** `Emulated.HW\ND\CPU\ND100\`
- `CpuND100.cs` + `CpuND100.Interrupt.cs` (level-12 delivery), `CpuND100.MMS.cs`, `OpCom.cs`

**Classic bus interface (3022 + engine):** `Emulated.HW\ND\CPU\NDBUS\`
- `NDBusND500IF.cs` - registers, CS engine, mailbox engine (ProcessMailboxMessage, N5MessageOffsets,
  MicroFunction, MessageProcessed hook), MPM window (SharedMemoryStart, ReconfigureSharedMemory)
- 5015 side: DOES NOT EXIST YET (design doc Phase 3: ND500ControlII + ND500InterfaceLink planned;
  TAG-IN strobes currently latch + feed the CS engine inside NDBusND500IF)
- `NDBusDeviceBase.cs` (IOX plumbing, InterruptBits, DMARead/DMAWrite)

**ND-5000 / Octobus:** `Emulated.HW\ND\CPU\NDBUS\`
- `NDBusOctobus.cs` (the Octobus controller; recently refactored)
- `OctobusFabric.cs` (bus fabric), `OctobusND5000Station.cs` (the ND-5000 station/ACCP responder)

**Tests (style to follow):** `Emulated.Tests.ND500\nd500if\` - Nd500CommandShapedTests*.cs,
Nd500MonBringupTests.cs, ControlStoreGateTests.cs, Nd3022RegisterTests.cs, Mpm5MemoryTests.cs;
DOM/domain: `Emulated.Tests.ND500\TestND500_DomainManagement.cs`, `TestND500_DOM_*.cs`.

## URGENT PREQUEL QUESTION (live blocker, trace-grounded 2026-07-16 13:32)

After a successful CS load + RETG5 restart, SINTRAN sends MICFU=13B messages (fields:
N500A=0B, dest ND-100 word 0x212400 via offsets 11B/12B = 41B/22000B, count 4000B = 2048 bytes,
TRAPN slot = 10746B) - **four bit-identical retries 8 ms apart, then one with N500A=177B after a
1.03 s status poll, then "Error when loading Control Store." + "Error in memory configuration"
(both from the SAME rejection, PIL-10 P=$5CBC).** Our emulator answers with an echo + 2048 bytes
copied from ND-500 address 0 - which is ZEROS, because no microcode INIT ever ran. SINTRAN
content-validates the buffer and rejects it.

**QUESTION: what does the microcode's startup (INIT_SAMSON, microword 0 boot jump) write into
ND-500 resident/low memory - specifically byte range 0-2048 and whatever N500A=177B addresses?**
(Version words? config/self-test table? START_MESS init? memory-sizing markers?) Give the exact
layout + values (or the derivation from CPU parameters) so the emulator can materialize that
memory image after MICRO-START, letting the 13B read return what SINTRAN validates. Also: after
the 13B burst SINTRAN polls RSTA5 for ~1 s waiting for a status CHANGE that never comes
(stuck at $21 = locked+bit0) - does the microcode change interface status / write anything
additional after servicing these reads? And confirm whether 13B on this image is RESIRD
(read) or a memory-configuration function - the ND-100 error text says "memory configuration".

## CONSTRAINTS

- C# rules: no LINQ, no foreach in product code, NUnit + Assert (no FluentAssertions), performance-
  minded (Span/ArrayPool where hot). Never fabricate protocol behavior - evidence grades on every claim.
- The live machine (SINTRAN L + nd-500-mon J04 + 5800 microcode image via classic 3022) is the
  acceptance oracle; decoded MAILBOX trace lines are the observability channel.
- Deliverable format: an architecture proposal document + phased plan; implementation happens in
  the bus-interface session afterwards.
