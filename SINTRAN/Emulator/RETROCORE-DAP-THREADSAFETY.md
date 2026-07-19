# RetroCore ND-100 RADD NullReferenceException — DAP Thread-Safety Analysis

**Target repo (READ-ONLY, separate from NDInsight):** `/mnt/e/Dev/Repos/Ronny/RetroCore/`
**Crash:** `System.NullReferenceException` in `Emulated.HW.ND.CPU.ND100.Instructions.RADD()` → `doROP()`
**Method:** static source reading only. Each claim tagged **VERIFIED** (read from the cited file:line) or **INFERRED**.

---

## 1. Hypothesis under test

> The RADD crash is NOT a bug in the RADD instruction. It is a symptom of the DAP implementation being thread-unsafe: DAP request handling runs concurrently with the CPU execution thread and mutates/reads shared CPU state (in particular the fetched-instruction reference) without proper locking, so a running instruction observes that state transiently null and crashes.

**Verdict (short): PARTIALLY TRUE — TRUE in its headline, INACCURATE in its named mechanism.**
It is correct that this is NOT an RADD bug and IS a thread-safety defect centred on the fetched-instruction reference. It is wrong about *who* races: no DAP request handler ever writes the fetched-instruction reference. The reference is nulled by the CPU tick itself, and the crash requires a **second, un-gated CPU execution pump** (the GUI emulation loop) running concurrently with the DAP free-run thread. The two pumps guard CPU execution with **two different locks**, so they do not exclude each other. Full reasoning and confidence in §5.

---

## 2. The crash mechanism at the instruction level (single-thread = safe)

**VERIFIED** — `/mnt/e/Dev/Repos/Ronny/RetroCore/Emulated.HW/ND/CPU/ND100/Instructions.RegisterOperations.cs:302-304`
```csharp
private void doROP()
{
    if (regs.fetched.roRAD == 0)   // <-- NRE here when regs.fetched == null
```
`RADD()`, `COPY()` (:295), `SWAP()` (:283), `RSUB()` etc. all call `doROP()`. `doAddSourceToDestination()` (:318) and `doBinaryRegisterOperations()` re-read `regs.fetched.*` repeatedly.

**VERIFIED** — `regs.fetched` is a *property over shared backing state*, `Registers.cs:252`:
```csharp
public FetchedInstruction fetched { get { return base.fetchedInstructionRef as FetchedInstruction; } }
```
If `fetchedInstructionRef` is null (or momentarily not a `FetchedInstruction`), the getter returns null and any subsequent `.roRAD` dereference throws NRE.

**VERIFIED** — the CPU tick transiently nulls then refetches, `CpuND100.cs:298-299`:
```csharp
regs.fetchedInstructionRef = null;                       // clear old decoded
regs.fetchedInstructionRef = this.FetchInstruction(regs.PC);
ExecuteDecodedInstruction((FetchedInstruction)regs.fetched);   // :301
```

**VERIFIED** — `ExecuteDecodedInstruction` has a *one-time* entry guard, `CpuND100.cs:315`:
```csharp
if (regs.fetched == null) return;
```
but the instruction body (`doROP` and friends) re-dereferences `regs.fetched` many times **after** that guard, on the live shared field — not on the `fi` parameter that is already threaded in at `:311`.

**Single-thread conclusion (VERIFIED by control-flow reading):** in one thread the sequence null → refetch (non-null) → guard → execute is race-free; `regs.fetched` is non-null throughout `doROP`. The NRE is impossible without concurrency. **This is the primary evidence that RADD/doROP is not itself buggy.**

---

## 3. Thread model — every thread that can touch CPU state

| Thread | How started | Pumps CPU? | Guard held while in `Run()`/tick |
|---|---|---|---|
| **DAP free-run thread** `"DAP-Run"` | `new System.Threading.Thread(DapRunLoop)` — `MachineBaseDAP.cs:251`, started on every DAP `continue`/`stepOver-call`/`stepOut` via `StartDapRun()` (:229) | **Yes** — `Run(...)` at `MachineBaseDAP.cs:272` | `lock (_cpuExecutionGate)` at `:270`. **Deliberately does NOT hold `_dapLock`** (comment `:225-227`) so `Pause()` can set the flag without deadlock. **VERIFIED** |
| **DAP TCP server / request thread** | `Task.Run(ServerLoop)` — `DapTcpServer.cs:109`; requests read in `ProcessClientMessages` (`:311`, `async`), dispatched by `HandleDapRequest` (`:405`) which calls the handlers **synchronously on this task's thread** | No (except step/continue, which delegate) | Handlers take **their own** locks — see §4. Runs **concurrently** with `DAP-Run` because `Continue` returns immediately after spawning `DapRunLoop`. **VERIFIED** |
| **Avalon GUI emulation thread** | `Task.Run(() => RunMachineThread(...))` — `Emulated.UI.Avalon/Services/EmulationService.cs:817` | **Yes** — `Run(...)` at `EmulationService.cs:1572`, once per video frame | `lock (CpuLock)` at `:1556`, where `CpuLock => SyncLock` (`:82`) and `SyncLock => _dapLock` (`MachineBaseDAP.cs:35`). i.e. it holds **`_dapLock`, NOT `_cpuExecutionGate`.** **VERIFIED** |
| **SDL GUI frame loop** | main loop in `Emulated.UI.SDL2/RetroEmulation.cs:476` | **Yes** — `machine.Run(PC, ...)` at `RetroEmulation.cs:483` | **No lock at all**, and no `DapOwnsCpu` check. **VERIFIED** |
| **Console debugger run loop** | `Emulated.Debugger/DebugCommands.Machines.cs:1015 doRun` (this project also hosts the DAP server — `DebugStepper.cs:115`) | Yes — `machine.Run` at `:1052` | `Monitor.TryEnter(machine.CpuExecutionGate)` **and** refuses if `machine.DapOwnsCpu` (`:1021`). **Correctly gated.** **VERIFIED** |
| **DAP single-step** | `SingleStepCpuLocked` — `MachineBaseDAP.cs:813` (from StepIn/StepOver-non-call) | Yes — `Step(1)` at `:827` | `lock (_cpuExecutionGate)` at `:824` (inside `_dapLock`). **Correctly gated.** **VERIFIED** |
| Clock/RTC, devices | ticked *inside* `Run()`/tick on whichever pump thread is executing | via pump | inherit the pump's guard |

**Key structural fact (VERIFIED):** there are **two disjoint locks** that are each meant to serialise CPU execution:
- `_cpuExecutionGate` (`MachineBaseDAP.cs:191`) — taken in exactly two places: `DapRunLoop` (`:270`) and `SingleStepCpuLocked` (`:824`); and externally by the console `doRun` (`DebugCommands.Machines.cs:1021`).
- `_dapLock` / `SyncLock` / `CpuLock` (`MachineBaseDAP.cs:29`, `:35`) — taken by the DAP request handlers and by the **Avalon** emulation loop (`EmulationService.cs:1556`).

The DAP free-run thread and the Avalon emulation loop therefore guard the *same* critical section (`Run()` → `TickCpuUnsafe`) with *different* mutexes and do **not** exclude one another.

---

## 4. Shared mutable CPU state accessed cross-thread, and its locking

Writers of `fetchedInstructionRef` — **exhaustive** (`grep fetchedInstructionRef`, ND-100 scope): **VERIFIED**
- `CpuND100.cs:298-299` — the tick: `= null` then `= FetchInstruction(PC)`. (CPU pump thread.)
- `Instructions.RegisterOperations.cs:664-669` — `EXR` handler: `save = fetched; ref = fi; Execute; ref = save;`. (CPU pump thread.)
- `OpCom.cs:2410-2413` — `ExecuteInstruction`: same save/set/restore. (CPU pump thread.)

**No DAP request handler is in that list.** `CpuND100.FetchInstruction(address)` (`CpuND100.Fetch.cs:21`) returns a **fresh** `FetchedInstruction` and does **not** assign `regs.fetchedInstructionRef` — **VERIFIED** (`:26` returns `CreateInstruction(...)`, no field write). So decode-for-display cannot null the shared reference.

DAP inspection handlers and the locks they hold:

| Handler (`MachineBaseDAP.cs`) | CPU state touched | `_dapLock`? | `_cpuExecutionGate`? |
|---|---|---|---|
| `Continue` (:494) | clears PAUSED, spawns `DapRunLoop` | **yes** (:498) | no (the spawned thread takes it) |
| `Pause` (:641) | sets PAUSED flag | **no** (deliberate, :645) | no |
| `StepInto`/`StepOver` (:596/:522) | `SingleStepCpuLocked` | yes | yes (via :824) |
| `GetStackTrace` (:686) | reads `cpu.CpuRegisters.PC` (:703) | **no** | **no** |
| `ReadMemory` (:1500) | `DapReadMemorySpace` → `Dbg_ReadVirtualWord`/`ReadPhysicalWordSafe` (ND100Machine.cs:956-1011) — reads memory + MMU page tables | **no** | **no** |
| `WriteMemory` (:1571) | `DapWriteMemorySpace` → `Dbg_WriteVirtualWord`/`Dbg_WritePhysicalWord` — **mutates memory** | **no** | **no** |
| `Disassemble` (:1638) | `DapDisassembleSpace` → `DapReadWord` + `NDcpu.DisassembleWord(addr, word)` (ND100Machine.cs:1067-1087) — reads memory/MMU; `DisassembleWord` takes a pre-read opcode, does not touch `fetchedInstructionRef` | **no** | **no** |
| `Evaluate` (:1709) | `ReadMemory(...)` and register-layout read | **no** | **no** |
| `GetVariables` (:1254) | register-layout read (`cpu.CpuRegisters.GetRegisterLayout()`) | **no** | **no** |
| `SetVariable` (:1327) | parses a register write (placeholder body) | **no** | **no** |
| `SetBreakpoints`/data/instr (:1193…) | `cpu.BreakpointManager` collections | **no** | **no** |
| `TrackDapOperation` wrapper (:2006) | none itself; wraps the above | **no** | **no** |

**VERIFIED conclusion of the table:** the read/inspection handlers run on the DAP server task thread with **neither** lock, concurrently with whatever pump thread is executing. This is a real data race on memory, MMU page-table state, registers and PC (and, for `WriteMemory`/`SetVariable`, a write race). It is a genuine DAP thread-safety defect **in its own right** — but it is **not** the writer that nulls `fetchedInstructionRef`, so it is not the direct cause of the RADD NRE.

---

## 5. The concrete race window that produces the RADD NRE

Two CPU-execution pumps run at once because they hold different locks. Interleaving (both file:line VERIFIED):

```
Thread A  (Avalon "RunMachineThread",  EmulationService.cs:1572, holding _dapLock)
    ... machine.Run() -> TickCpuUnsafe -> ExecuteDecodedInstruction
    passes null-guard  CpuND100.cs:315   (regs.fetched != null)
    enters RADD -> doROP
    about to read      Instructions.RegisterOperations.cs:304   regs.fetched.roRAD
                                          |
Thread B  (DAP "DAP-Run",  MachineBaseDAP.cs:272, holding _cpuExecutionGate)
    ... machine.Run() -> TickCpuUnsafe
    executes           CpuND100.cs:298    regs.fetchedInstructionRef = null;   <-- writes null
                                          |
Thread A  reads regs.fetched -> getter Registers.cs:252 returns null
          -> NullReferenceException at RADD/doROP  (CpuND100.cs:304)
```

The symmetric case (DAP-Run is the crashing thread, Avalon is the nuller) is identical. Either pump can be “Thread A”. **Racing writer = the CPU tick `= null` at `/mnt/e/Dev/Repos/Ronny/RetroCore/Emulated.HW/ND/CPU/ND100/CpuND100.cs:298`, executed by whichever pump the hypothesis did not name.** The racing reader = `/mnt/e/Dev/Repos/Ronny/RetroCore/Emulated.HW/ND/CPU/ND100/Instructions.RegisterOperations.cs:304`.

**Single most important race window:**
`EmulationService.cs:1572` (`machine.Run`, under `_dapLock`) executing `doROP` **vs** `CpuND100.cs:298` (`fetchedInstructionRef = null`, under `_cpuExecutionGate` on `DAP-Run`). The two locks are disjoint (`MachineBaseDAP.cs:191` vs `:29`/`:35`), so nothing serialises them.

**Why the hypothesis's exact wording is off (VERIFIED):** it blames “DAP request handling … mutates … the fetched-instruction reference.” No DAP request handler writes `fetchedInstructionRef` (§4 exhaustive list). The null comes from the CPU tick; the defect is that a second CPU **pump** is allowed to run concurrently with the DAP run thread. So the *root cause is still in the DAP/threading design* (the `_cpuExecutionGate` is not the single gate all pumps honour), which is why the hypothesis is TRUE at the headline level.

---

## 6. Verdict and confidence

- **Is it an RADD-specific bug?** **No.** — HIGH confidence. `doROP`/`RADD` are correct single-threaded (§2, control-flow VERIFIED). The only NRE path is a null `regs.fetched`, and RADD does not create it.
- **Is it a DAP / threading thread-safety defect?** **Yes.** — HIGH confidence on the *code facts* (two disjoint locks over the same critical section: `_cpuExecutionGate` on the DAP run thread `MachineBaseDAP.cs:270` vs `_dapLock` on the Avalon pump `EmulationService.cs:1556`, both VERIFIED; plus lock-free inspection handlers, §4 VERIFIED).
- **Is the hypothesis TRUE, FALSE, or BOTH?** **PARTIALLY TRUE / BOTH.** TRUE: not RADD, and yes a DAP-side thread-safety defect around the fetched-instruction reference. INACCURATE detail: the racing writer is a second CPU pump (`CpuND100.cs:298`), not a DAP request handler; DAP handlers never touch `fetchedInstructionRef`.
- **Is `_cpuExecutionGate` sound-but-unhonoured, or fundamentally insufficient?** **Sound design, not universally honoured.** — HIGH confidence. Where every pump uses it (console `doRun` `DebugCommands.Machines.cs:1021`; `SingleStepCpuLocked` `:824`; `DapRunLoop` `:270`) execution is correctly serialised. It fails only because the GUI pumps bypass it: Avalon uses `_dapLock` instead (`EmulationService.cs:1556`) and SDL uses no lock (`RetroEmulation.cs:483`).
- **What I did NOT do (INFERRED / limits):** I did not run the emulator or reproduce the stack trace; the "this exact interleaving produced the observed trace" step is INFERRED from the fact that a null `regs.fetched` is the *only* route to an NRE in `doROP` and the two-pump race is the *only* code path that nulls it cross-thread. Which specific host was attached when the user saw the crash (Avalon vs SDL vs a console-hosted DAP with an extra pump) is not proven from source; the Avalon host is the concrete, VERIFIED example of a pump that races `DAP-Run`.

---

## 7. Fix surface (analysis only — NOT implemented), ranked by impact

1. **Unify the CPU-execution gate across every pump (root cause).** Make the GUI emulation loops acquire `machine.CpuExecutionGate` around `Run()` (or skip when `machine.DapOwnsCpu`, exactly as the console `doRun` already does at `DebugCommands.Machines.cs:1021`):
   - `Emulated.UI.Avalon/Services/EmulationService.cs:1572` — currently guarded by `_dapLock` (`CpuLock`, :1556), which the DAP run thread does not hold. Must take `_cpuExecutionGate`.
   - `Emulated.UI.SDL2/RetroEmulation.cs:483` — currently un-gated.
   This alone closes the two-pump race and thus the RADD NRE. **Highest impact.**

2. **Make instruction handlers use the `fi` parameter, not the live `regs.fetched`.** `ExecuteDecodedInstruction(FetchedInstruction fi, …)` already threads `fi` in (`CpuND100.cs:311`); `doROP`/`doAddSourceToDestination`/`EXR`/`doBinaryRegisterOperations` re-read `regs.fetched` (e.g. `Instructions.RegisterOperations.cs:304,321,323,324,644,646`). Reading the local captured once makes a concurrent null of the shared field unable to NRE mid-instruction, and removes the guard-once/deref-many inconsistency at `CpuND100.cs:315`. **Defense-in-depth; high value even after fix 1.**

3. **Give the DAP inspection/mutation handlers a lock.** `ReadMemory` (:1500), `WriteMemory` (:1571), `Disassemble` (:1638), `Evaluate` (:1709), `GetVariables` (:1254), `SetVariable` (:1327), `GetStackTrace` (:686) currently take no lock. They should acquire the unified CPU gate (read side) so they do not read/mutate memory, MMU page tables, registers or PC while a pump executes. `WriteMemory`/`SetVariable` (write races) are the most dangerous. This fixes a separate real defect not covered by fix 1.

4. **Remove the transient `regs.fetchedInstructionRef = null;` at `CpuND100.cs:298`.** Assign the fetch result directly (`fetchedInstructionRef = FetchInstruction(PC);`). This deletes the specific null window. It narrows — but does not by itself eliminate — the race (a second pump still swaps the whole object cross-thread), so it is a mitigation, not the fix. **Do only alongside fix 1/2.**

5. **Keep decode-for-display off shared state (already true for ND-100; assert it).** `FetchInstruction` returns a fresh object and `DisassembleWord`/`Dbg_*` are read-only (§4 VERIFIED), so no change is needed on ND-100; worth an assertion/comment so a future refactor does not reintroduce a shared-ref write on the display path. **Lowest.**

---

*All RetroCore paths above are absolute and point outside the NDInsight repository. No RetroCore file was modified.*
