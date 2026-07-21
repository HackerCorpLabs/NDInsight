# TRACK B - Shared ND-500 CPU Interface Design (octobus attach path)

Date: 2026-07-21
Author: TRACK B design session (analysis only, no code edited)
Goal: let the REAL microword `CpuND5000` wire into the octobus attach path exactly
where the functional `CpuND500` does today, so the boot harness can boot the genuine
128-bit control-store swapper on the microcode CPU.

Grades: [V] verified from source read this session; [I] inferred from the code but not
proven; [OPEN] cannot verify from code, needs a decision or a live experiment.

All paths are absolute. This is DESIGN ONLY - nothing here has been implemented.

---

## 0. Executive summary

- The station's stored CPU field is already an interface (`IND500Cpu`), but the three
  wiring entry points the task cares about - `AttachNd5000Cpu`, `AttachRealCpu`, and the
  `Nd500CpuProcessBridge` constructor - are all typed to the CONCRETE `CpuND500` class,
  not to an interface. [V]
- `CpuND5000` lives in the leaf NuGet `HackerCorpLabs.Emulation.CPU.ND5000`, which
  references only the HackerCorpLabs.Emulation.* packages and does NOT (and by dependency
  direction cannot) reference `Emulated.HW` where `IND500Cpu` and `CpuND500` live. [V]
  Therefore `CpuND5000` CANNOT implement `IND500Cpu` or any RetroCore interface without a
  dependency inversion. That rules out "make CpuND5000 implement the interface" and makes
  an ADAPTER (living in `Emulated.HW`, wrapping `CpuND5000`) the only non-invasive option.
- Recommended: EXTRACT a new interface on the RetroCore side (over the concrete-`CpuND500`
  surface the attach path uses) + write a `CpuND5000` ADAPTER in `Emulated.HW`. `CpuND500`
  already has every member, so it implements the extracted interface trivially. The
  microcode session's `CpuND5000.cs` needs ZERO changes.
- The hard part is NOT the interface plumbing - it is that the functional path installs a
  C# servicer + bridge that REPLACE the microcode's mailbox/MON/context handling, whereas
  `CpuND5000` does that work itself in real microcode. Wiring `CpuND5000` through the SAME
  bridge/servicer is semantically wrong. The microcode CPU needs a THINNER attach variant.
  This is the biggest design decision and the biggest [OPEN] (boot-from-CS model).

---

## 1. Task 1 - COMPLETE surface invoked on the CPU object today

Two callers touch the CPU: the station (through the `IND500Cpu` field `_cpu`, plus the
concrete-typed `AttachRealCpu`), and the process bridge (through the concrete `CpuND500`).
Plus the machine attach helper and its attachment handle.

### 1a. Station - `OctobusND5000Station.cs`
File: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\OctobusND5000Station.cs`

Members reached through the `IND500Cpu _cpu` field (interface-typed - already abstract):

| Member | Kind | Site |
| --- | --- | --- |
| `TagWritten` (subscribe/unsubscribe) | event `Action<ushort>` | line 483, 490 |
| `SetMpmMemory(RAM, uint)` | method | line 497, 665 |
| `Reset()` | method | line 1304 |
| `CpuRegisters` | property `RegistersBase` (then `.stopMode`) | line 1308-1310 |

Members reached through the CONCRETE `CpuND500 cpu` parameter of `AttachRealCpu` (lines
529-552) - these are NOT on `IND500Cpu`:

| Member | Kind | Site |
| --- | --- | --- |
| `WakeRunThread()` | method (also used as `Action` for `MailboxDoorbell +=`) | line 539 |
| `OnRunThreadPark` | settable `Action` field | line 540 |
| `regs.stopMode` | field-of-field (ND500 `Registers`) | line 546, 547 |
| `StartRunThread(string)` | method | line 548 |

### 1b. Process bridge - `Nd500CpuProcessBridge.cs`
File: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND500\Servicer\Nd500CpuProcessBridge.cs`
Constructor param is concrete `CpuND500 cpu` (line 40, 54). Every call is on the concrete class:

| Member | Kind | Site |
| --- | --- | --- |
| `MonitorCallSink` (set/clear) | property `IMonitorCallSink` | line 59, 87 |
| `TrapSink` (set/clear) | property `ITrapSink` | line 60, 89 |
| `SetND100PrivateOffset(uint)` | method | line 75 |
| `regs.stopMode` | field (read/clear WAIT bit) | line 118, 123, 327, 364, 372, 438, 454 |
| `RunThreadActive` | property `bool` | line 122, 156, 338, 373, 455 |
| `WakeRunThread()` | method | line 123, 157, 331, 339, 374, 456 |
| `StartProcessFromRegisterImage(ushort, ushort[])` | method | line 131 |
| `ReadMpmByte(uint)` | method (on `IND500Cpu` too) | line 202, 208 |
| `regs.PC` | field (read + write) | line 269, 276, 300 |
| `regs.CAD` | field | line 276 |
| `MapExistingPhysicalRegion(uint,uint,bool,uint,uint,bool)` | method | line 277, 279 |
| `EnableProgramMMU()` | method | line 297 |
| `EnableDataMMU()` | method | line 298 |
| `StartProcessFromContextBlock(uint)` | method | line 335 |
| `regs.I1` | field (write) | line 369, 443 |
| `regs.ST.K` | field-of-field (write) | line 370, 444 |
| `WriteVirtualMemory32(uint,uint)` | method | line 420 |
| `WriteVirtualMemory(uint,byte)` | method | line 450 |
| `ReadVirtualMemory32(uint)` | method | line 480 |

Bridge also uses the servicer (not the CPU): `servicer.ProcessZeroRegisterImage`,
`ProcessZeroFirstRegister`, `ResiwrLog`, `ActiveProcessMessageAddress`, `Answer*`. Those
are servicer-side and out of the CPU contract.

### 1c. Machine attach + handle - `ND100Machine.ND5000.cs`
File: `E:\Dev\Repos\Ronny\RetroCore\Emulated.Machines\ND\ND100\ND100Machine.ND5000.cs`

| Member | Kind | Site |
| --- | --- | --- |
| `new CpuND500(Registers, SystemBusAdapter)` | ctor | line 134 |
| passes `CpuND500? cpu` param into `AttachCpu` / `AttachRealCpu` | - | line 147, 164 |
| `Nd5000CpuAttachment.Cpu` typed `CpuND500` | property | line 237 |
| `Cpu.OnRunThreadPark = null` | field | line 268 |
| `Cpu.StopRunThread()` | method | line 269 |

`regs` type note: on `CpuND500`, `regs` is the ND-500 `Registers` class with `.stopMode`,
`.PC`, `.CAD`, `.I1`, `.ST` (with `.K`). [V] The bridge reaches into these directly.

---

## 2. Task 2 - Capability matrix (CpuND500 vs CpuND5000)

`CpuND5000` public surface (from
`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND5000\src\CpuND5000.cs`
and siblings): fields `Cs` (`ControlStore`), `Cache`, `Regs` (ND5000 `Registers`: `Wrf[24]`,
`Srf[4096]`, `P`, `Q`, `Data`, micro-status...), `State` (`CpuState`: `Mpc`, `Stack1..4`,
`Stopped`), `Memory` (`IMicroMemory?`); methods `Tick()`, `Run(int)`,
`StepOneMacroInstruction(int)`, `RaiseTrap(int)`. No events. No Reset. Transport is the
`AccessModule` (AIB/AOB, `OnAibWritten`/`OnAobRead`) driven by `AccpController`. [V]

exists = a semantically equivalent member the adapter can forward to directly.
adapter = the adapter can synthesize/translate it from CpuND5000 primitives.
gap = no equivalent; needs a real decision or new microcode-side capability.

| Contract member (from Task 1) | CpuND500 | CpuND5000 | Verdict |
| --- | --- | --- | --- |
| `TagWritten` event / `WriteTag` | native event on WriteTag [V] | no event; microcode writes AIB via `AccessModule.WriteAib` -> `OnAibWritten` [V] | adapter: map `AccessModule.OnAibWritten` -> raise `TagWritten` (station already treats TagWritten payload as "the AIB word" [V line 55-59, 474-477]) |
| `SetMpmMemory(RAM,uint)` | Port B backing [V] | `Memory` is `IMicroMemory`, byte-addressed big-endian [V] | adapter: wrap the shared `RAM` in an `IMicroMemory` and assign `cpu.Memory` |
| `ReadMpmByte`/`WriteMpmByte`/`ReadMpmWord32`/`WriteMpmWord32` | native [V] | via `Memory.Read/Write(addr,width)` [V] | adapter: forward to `Memory` (bit-31 offset handling in adapter) |
| `Reset()` | clears regs, stopMode=NONE [V] | NO Reset method [V] | adapter: reset `State`/`Regs`, set micro-PC to the init vector; master-clear semantics [OPEN] |
| `CpuRegisters` (`RegistersBase`) | native [V] | ND5000 `Registers` is NOT a `RegistersBase` and is a different class [V] | gap/adapter: adapter exposes a `RegistersBase` facade or the interface drops this member (see section 3) |
| `WakeRunThread()` / `StartRunThread()` / `StopRunThread()` / `RunThreadActive` / `OnRunThreadPark` | native run-thread model [V] | NONE - CpuND5000 has no thread; caller calls `Tick()`/`Run()` [V] | adapter: adapter owns a run thread that calls `Tick()` in a loop; wake/park translate to the loop's gate |
| `regs.stopMode` (WAIT park/un-park) | native `StopMode` [V] | `State.Stopped` (STOP word) only; no WAIT idle concept [V] | adapter: the microcode's own IDLE loop IS the park; WAIT has no direct equivalent [OPEN] |
| `MonitorCallSink` / `TrapSink` (set) | native sinks; microcode replaced by C# [V] | NONE - the microcode raises traps itself (`RaiseTrap`) and builds MON stop records in microcode [V] | gap: for the microcode CPU these sinks should NOT be installed (microcode does the work); see section 5 |
| `StartProcessFromRegisterImage(ushort,ushort[])` | applies full 21B block to live regs [V] | NONE - microcode loads context via NEWCNTXT/CNTXTLOAD from the mailbox itself [V] | gap: the microcode does NOT take a 21B register image the C# way; it reads the context block from MPM. Adapter would have to write the block into MPM/SRF and let microcode load it [OPEN] |
| `StartProcessFromContextBlock(uint)` | native [V] | NONE (same as above) [V] | gap/[OPEN] |
| MMU: `MapExistingPhysicalRegion`, `EnableProgramMMU`, `EnableDataMMU` | native functional MMU [V] | NONE - MMS not built yet (memory ops are identity/POF-approximated) [V CpuND5000.cs:168-176, MmsUnit stub] | gap: the microcode does its own MMS translation once built; the C# swapper-mapping shim does not apply to it |
| `WriteVirtualMemory32` / `WriteVirtualMemory` / `ReadVirtualMemory32` | native virtual MMU-translated access [V] | via `Memory.Read/Write` (currently identity) [V] | adapter: forward to `Memory` while MMS is identity; becomes real once MmsUnit lands [OPEN] |
| `regs.PC`/`regs.CAD`/`regs.I1`/`regs.ST.K` (bridge write-backs) | native ND500 regs [V] | `Regs.P` exists; CAD/I1/ST.K map onto WRF/SRF/status differently [V/I] | gap: only relevant if the bridge drives the microcode CPU, which it should not |
| `SetND100PrivateOffset(uint)` | native [V] | NONE [V] | adapter no-op (only the bridge uses it; bridge not used for microcode CPU) |

Summary: the TRANSPORT + LIFECYCLE members (TagWritten, SetMpmMemory, MPM accessors,
Reset, run-thread) are adaptable. The PROCESS/MON/TRAP/MMU members
(`StartProcessFrom*`, `MonitorCallSink`, `TrapSink`, `MapExistingPhysicalRegion`,
virtual-memory, register write-backs) are GAPS because they belong to the C# servicer model
that the microcode replaces. They should not be driven against `CpuND5000` at all.

---

## 3. Task 3 - Recommended interface design

### Why not "make both implement one interface" (option a in its pure form)
`CpuND5000` cannot reference `IND500Cpu` (dependency direction, section 0). So a single
interface implemented by BOTH classes is impossible unless the interface is moved down into
a shared NuGet AND `CpuND5000.cs` is edited to implement it - which touches the microcode
session's file. Rejected.

### Why not "pure adapter satisfying the existing contract unchanged" (option b in its pure form)
The attach path (`AttachNd5000Cpu`, `AttachRealCpu`, `Nd500CpuProcessBridge` ctor,
`Nd5000CpuAttachment.Cpu`) is typed to the CONCRETE `CpuND500`. An adapter wrapping
`CpuND5000` is not a `CpuND500`, so it cannot be passed to those methods without changing
their signatures. A pure adapter with NO RetroCore-side changes is therefore not possible.

### Recommended: HYBRID - interface extraction on the RetroCore side + a CpuND5000 adapter
1. EXTRACT a new interface in `Emulated.HW` (NOT in the microcode NuGet, NOT in any file the
   microcode session owns) covering the concrete-`CpuND500` surface the attach path uses.
   Suggested name `INd500AttachableCpu` (or extend `IND500Cpu` with a second interface
   `INd500ProcessCpu`). `CpuND500` implements it trivially - it already has every member, so
   this is a pure `: IND500Cpu, INd500ProcessCpu` addition with no body changes.
2. Retype the attach path to the extracted interface:
   - `Nd500CpuProcessBridge(INd500ProcessCpu cpu, Nd500MicrocodeServicer)`
   - `OctobusND5000Station.AttachRealCpu(INd500ProcessCpu cpu, bool)`
   - `ND100Machine.AttachNd5000Cpu(..., INd500ProcessCpu? cpu = null)`
   - `Nd5000CpuAttachment.Cpu` typed `INd500ProcessCpu`
3. Write `CpuND5000Adapter : IND500Cpu, INd500ProcessCpu` in `Emulated.HW` (it can reference
   both the NuGet and `Emulated.HW`), wrapping a `CpuND5000` + its own run thread + an
   `IMicroMemory`-over-`RAM` shim + an `AccpController`/`AccessModule` bound to the station.
   `CpuND5000.cs` is untouched.

Rationale: the interface extraction is confined to files the RetroCore/functional-CPU
session owns; the adapter is new and self-contained; the microcode session signs off on
NOTHING because none of their files change.

### Sketched interface members (extracted; grouped)
Transport (already on `IND500Cpu`, keep there):
- `event Action<ushort> TagWritten;  void WriteTag(ushort);`
- `void SetMpmMemory(RAM, uint);  byte ReadMpmByte(uint);  void WriteMpmByte(uint,byte);`
- `uint ReadMpmWord32(uint);  void WriteMpmWord32(uint,uint);`
- `void Reset();`

Lifecycle / run-thread (NEW `INd500ProcessCpu`, the part `AttachRealCpu` needs):
- `void StartRunThread(string name);  void StopRunThread(int timeoutMs = 5000);`
- `void WakeRunThread();  bool RunThreadActive { get; }  Action OnRunThreadPark { get; set; }`
- a park/idle primitive that both models can honor. `CpuND500` uses `regs.stopMode` WAIT; the
  adapter has no WAIT. Recommend replacing the raw `cpu.regs.stopMode = WAIT` pokes in
  `AttachRealCpu`/`ResetCpuToIdle` with an interface method `ParkOnIdle()` / `bool IsParked`
  so the adapter can implement "park" as "hold the tick loop". [I - needs the two poke sites
  at station lines 546-547 and 1308-1310 refactored to go through the interface.]

Process / MON / trap / MMU (the servicer-model members): DO NOT put these on the shared
interface. They stay concrete on `CpuND500` and are only reachable when the functional
bridge is used. The microcode adapter never exposes them because the microcode does that
work itself (section 5). This keeps `INd500ProcessCpu` small and honest.

### Where the attach path changes (signatures)
File `E:\Dev\Repos\Ronny\RetroCore\Emulated.Machines\ND\ND100\ND100Machine.ND5000.cs`:
- `AttachNd5000Cpu(..., CpuND500? cpu = null)` -> `INd500ProcessCpu? cpu = null` (line 108)
- default build `new CpuND500(...)` stays (line 134); a new overload/param lets the caller
  pass a `CpuND5000Adapter` instead.
- `Nd5000CpuAttachment.Cpu` -> `INd500ProcessCpu` (line 237); `.Stop()` unchanged.

File `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\OctobusND5000Station.cs`:
- `AttachRealCpu(CpuND500 cpu, bool)` -> `AttachRealCpu(INd500ProcessCpu cpu, bool)`
  (line 529). Body already only uses `WakeRunThread`, `OnRunThreadPark`, `StartRunThread`,
  and the `regs.stopMode` poke (route through `ParkOnIdle()`).
- BUT: `AttachRealCpu` also `new Nd500CpuProcessBridge(cpu, _servicer)` (line 535). For the
  microcode adapter this bridge must be OMITTED or replaced (section 5). Recommend a separate
  `AttachMicrocodeCpu(...)` entry that skips the bridge/servicer-as-answerer wiring.

File `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND500\Servicer\Nd500CpuProcessBridge.cs`:
- ctor `CpuND500 cpu` -> `INd500ProcessCpu` ONLY IF the microcode path reuses the bridge.
  Recommendation is that it does NOT, so the bridge can stay concrete `CpuND500`.

---

## 4. Task 4 - Minimal changes required to `CpuND5000.cs`

NONE. [V-by-construction of the adapter approach]

With the hybrid design, all translation lives in the new `CpuND5000Adapter` (in
`Emulated.HW`) and in `Emulated.HW`-side interface extraction. `CpuND5000.cs`,
`MacroOracleState.cs`, `MacroStepTests.cs`, `InstructionFamilyTests.cs`, and the ND5000
`docs/*PLAN*` / `WHERE-WE-ARE` docs are NOT touched.

The adapter reaches CpuND5000 entirely through members that already exist and are public:
`Cs`, `Cache`, `Regs`, `State`, `Memory`, `Tick()`, `Run()`, `StepOneMacroInstruction()`,
`RaiseTrap()`, and the `AccessModule`/`AccpController` transport. [V]

Two things the adapter needs that the microcode session may CHOOSE to provide later (not
required for a first wire-up, listed so they know what would help - see the coordination
request):
- A public master-clear/reset helper on `CpuND5000` (there is none today [V]). The adapter
  can synthesize reset by reassigning `State` and clearing `Regs`, but a
  microcode-authentic reset entry (jump micro-PC to the INIT_SAMSON vector) would be more
  correct. [OPEN]
- Confirmation of where the ACCP seam (`AccessModule`/`AccpController`) is meant to be
  owned - inside `CpuND5000` or supplied by the host. Today `AccpController` is constructed
  around an `AccessModule` and is transport-agnostic [V AccpController.cs:26-30], which is
  exactly what an adapter wants, so likely no change needed.

---

## 5. Task 5 - Boot concern: what "start" means for each CPU

### CpuND500 (functional) - starts from a 21B register image
The functional model: SINTRAN's PLACE/LOAD-SWAPPER chain hands the C# servicer a 21B (3WREG)
register block; a 23B 3START makes the bridge call
`StartProcessFromRegisterImage(firstReg, image)`, which writes the full register block into
the live register file, sets `regs.stopMode = NONE`, and wakes the run thread; the CPU then
interprets ND-500 MACRO code from P. The C# servicer + bridge REPLACE the microcode: they
answer mailbox activations, MON stops, and trap stops in C#. [V bridge lines 111-160,
ProcessControl.cs:84-121]

### CpuND5000 (microcode) - boots from the loaded 128-bit control store
The microcode model is fundamentally different [V, from CpuND5000.cs + AccessModule +
AccpController + the octobus-nd5000 skill facts]:
- There is NO 21B-register-image start entry. The genuine boot is: SINTRAN loads the 128-bit
  control store through the ACCP (the LCS0/CMWWC path the station currently only MODELS into
  its private `_controlStore[]` array), the microcode master-clears to its INIT_SAMSON
  vector, runs its init, and enters the IDLE poll loop that reads X5ACT from 5MPM and walks
  the X5BEX chain ITSELF. Context load is the microcode's NEWCNTXT/CNTXTLOAD reading the
  context block from shared memory - not a C# `StartProcess*` call. [V/I]
- "start" for CpuND5000 = (1) the control store is actually loaded into `CpuND5000.Cs`
  (today the CS bytes are captured into the station's own `_controlStore`, NOT into any CPU),
  and (2) a host run thread calls `Tick()` continuously so the microcode's IDLE loop runs.
  A kick/doorbell just needs to make sure the tick loop is running; the microcode reads the
  mailbox flags from MPM on its own.

### The consequences for wiring (this is the crux)
- The C# servicer must NOT answer mailbox messages for the microcode CPU - the microcode
  does. So `AttachRealCpu`'s `new Nd500CpuProcessBridge(...)` and the
  `ServiceMailboxOnClock` / `DrainDoorbells`-as-IDLE-poll machinery are for the FUNCTIONAL
  CPU only. A microcode attach variant must skip installing the bridge/servicer as the
  answerer and instead: assign `Memory` = IMicroMemory-over-5MPM, load `Cs` from the CS-load
  path, wire the `AccessModule`/ACCP to the fabric for kicks/GIVEINT, and run `Tick()`.
- The CS-load path is the pivot: today `OctobusND5000Station` intercepts LCS0/CMWWC/DUCS and
  fills its private `_controlStore[]` to make SINTRAN's read-back checksum self-consistent
  (lines 214-352, 703-741). For the microcode CPU those same writes must instead land in
  `CpuND5000.Cs` so the microengine actually executes them. [OPEN - how/whether to redirect
  the CS-load writes into `CpuND5000.Cs` while keeping the checksum read-back correct.]

### Biggest [OPEN]s
1. [OPEN] CS-load target: redirect the ACCP CS-load writes into `CpuND5000.Cs` (so the
   microengine runs real microcode) while preserving the read-back checksum the station
   currently serves from `_controlStore`. Needs a live experiment in the boot harness.
2. [OPEN] Mailbox ownership: with the microcode running its own IDLE poll, the station's
   servicer/bridge must be disabled for that CPU. Confirm the microcode's POF reads actually
   reach 5MPM through the adapter `IMicroMemory` (the `RD,POF`/`WR,POF` approximation,
   CpuND5000.cs:168-193, is an ND-100-seam approximation, not proven end-to-end). [V that it
   is an approximation; OPEN whether it reaches 5MPM correctly.]
3. [OPEN] Reset/master-clear model for CpuND5000 (no Reset method today; INIT_SAMSON entry
   vector unconfirmed in the code I read).
4. [OPEN] Park/idle: `CpuND500` uses `regs.stopMode=WAIT` as the parked-IDLE state; CpuND5000
   has only `State.Stopped` (STOP word) with no WAIT concept. The adapter's "park" = hold the
   tick loop; needs the two station poke sites (lines 546-547, 1308-1310) routed through an
   interface `ParkOnIdle()` method.

---

## 6. Consolidated [OPEN] list

- [OPEN] Redirect ACCP control-store load into `CpuND5000.Cs` vs the station's private
  `_controlStore` while keeping the DUCS/CMRWC read-back checksum correct.
- [OPEN] Disable the C# servicer/bridge-as-answerer for the microcode CPU (separate
  `AttachMicrocodeCpu` path) so the microcode owns the mailbox.
- [OPEN] Prove the microcode's POF reads/writes actually reach the 5MPM window through an
  `IMicroMemory`-over-`RAM` adapter (big-endian byte order, bit-31 offset).
- [OPEN] Microcode-authentic Reset/master-clear (INIT_SAMSON vector) - no Reset on CpuND5000.
- [OPEN] Park/idle equivalence: map `stopMode=WAIT` (functional) to a tick-loop hold (adapter);
  refactor the two raw `regs.stopMode` pokes in the station to an interface method.
- [OPEN] Whether `CpuND500.regs.CAD/I1/ST.K/PC` write-back semantics (the bridge's MON restart)
  have any meaning for the microcode CPU (they should not, since the bridge is not used).

---

## 7. Single next step to make task 11 wireable

Extract `INd500ProcessCpu` in `Emulated.HW` over the run-thread lifecycle members
(`StartRunThread`/`StopRunThread`/`WakeRunThread`/`RunThreadActive`/`OnRunThreadPark` + a
`ParkOnIdle()` primitive), have `CpuND500` implement it (no body changes), and retype
`AttachRealCpu` / `AttachNd5000Cpu` / `Nd5000CpuAttachment.Cpu` to it. That alone makes the
attach path accept a non-`CpuND500` object. THEN write the `CpuND5000Adapter` behind a new
`AttachMicrocodeCpu` entry that skips the functional bridge/servicer. No `CpuND5000.cs` edit.
