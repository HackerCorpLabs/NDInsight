# ND-500 / ND-5000 Microcode-Servicer Integration Architecture

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-MICROCODE-INTEGRATION-ARCHITECTURE-2026-07-16.md`
**Date:** 2026-07-16. **From:** the ND-5000 microcode-analysis session (answering the integration
brief `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\INTEGRATION-BRIEF-FOR-MICROCODE-LLM-2026-07-16.md`).
**Deliverable:** architecture proposal + phased plan. Implementation happens in the bus-interface
session afterwards.

## Evidence legend

| Tag | Meaning |
|---|---|
| **[V]** | Verified in the B30 microcode listing (`E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md`) |
| **[X]** | Matches the independently byte-verified SINTRAN-side carve (catalog / bus-reference docs) |
| **[CODE]** | Verified in the RetroCore source 2026-07-16 (line numbers current as of that date; gathered via read-only exploration this session) |
| **[LIVE]** | Live-trace-proven on the real machine (CS-load findings doc) |
| **[D]** | Derived — one documented inference step from verified facts |
| **[?] / ASSUMPTION** | Not established. Every design element resting on one is flagged inline. Do not implement without resolving. |

---

## 0. Executive summary

One new class, **`Nd500MicrocodeServicer`**, extracted from the servicing logic currently inside
`NDBusND500IF.ProcessMailboxMessage`, plays the microcode's mailbox role for BOTH generations.
It is owned by neither doorbell and not by the CPU: the 3022 (`NDBusND500IF`) and the Octobus
station (`OctobusND5000Station`) each hold one and ring it from their own doorbell decode; the
CPU (`CpuND500`) is an optional attachment the servicer drives for the process-routing MICFUs
and that calls BACK into the servicer at its two natural stop points (segment-31 CALLG
interception, unhandled trap). Doorbell-specific register/lock/interrupt semantics stay in the
owning interface; message lifecycle, MICFU dispatch, stop-record building, and restart delivery
are shared.

Two corrections to the brief, found in the source [CODE]:

1. **There is no X1 register in `CpuND500`** — the index/result registers are `I1..I4`
   (`Registers.cs`), and the existing MON emulation layer already delivers error codes into
   `regs.I1` and success/error into `regs.ST.K`. The microcode listing's `D,X1` destination
   (MSG_CONMC @015721 [V]) maps to **emulator `I1`** — **RESOLVED [V] 2026-07-16**:
   `E:\Dev\Ronny\ND5000UC\manual\MICROCODE-FIELDS.md` (ND-05.022.1 SAMSON MICROCODE
   DEFINITION) line 1304 lists the macro register file "Index registers | X1-X4 | WRF", and
   line 1236 glosses `AB,X1ORS` as "DESC(X)(**I1**), I1 SCALED ACCORDING TO INSTRUCTION" —
   the manual itself equates microcode name X1 with programmer name I1. So FUNCV→X1 [V]
   means `regs.I1` in the emulator, consistent with the existing MON layer's I1+K convention.
2. **Monitor entry is NOT a trap in `CpuND500`** — there is no trap code 6/7; a CALLG whose
   target segment == 31 is intercepted inline in
   `CpuND500.IndirectSegments.cs::HandleIndirectSegmentCall` and handed to `SintranEmulation`
   [CODE]. The real microcode reaches the same place via instruction-fetch trap code 6
   (TRAP_MONC → CALL_MON [V]), but the emulator already has a cleaner seam: we generalize the
   existing segment-31 branch into a pluggable sink instead of building a fake trap.

And one gap: **the Octobus side has no doorbell at all today** — OCB 100501B/100401B appear
nowhere in `OctobusND5000Station`/`NDBusOctobus`; the station has no shared-memory/mailbox
access and no microcode-load (LOCSD/LOCSM/STARTMIC) path [CODE]. Phase O below specifies that
layer from the microcode evidence; nothing existing is contradicted because nothing exists.

---

## 1. What exists today (source-verified 2026-07-16, all [CODE])

### 1.1 The 3022 engine (`Emulated.HW\ND\CPU\NDBUS\NDBusND500IF.cs`, 2235 lines)

- `ProcessMailboxMessage()` (2082–2180): MAR(word)→byte via `<<1`; reads N5STA, preserves
  power-fail high bits (`sta & 0xE000`); requires `ToNd500(1)`; writes `Waiting(2)`; reads
  MICFU; switch — **only ResidentRead(13B) has a real body** (DMA copy), the other implemented
  MICFUs are accept-only; fires `MessageProcessed(msgByteBase, micfu)` (event, line 2036);
  writes `Answer(3)`/`ErrorAnswer(4)` with high bits restored.
- Level 12: `CheckTriggerInterrupt()` (1237) = `InterruptEnabled AND ND500Finished` →
  `SetInterruptBit` (base `NDBusDeviceBase`, level 12). `InterruptEnabled` mirrors CONTROL bit 0.
- Memory: `ReadNd100Word/WriteNd100Word` (1431–1462) — dual path: inside the MPM window
  (`SharedMemoryStart=0x00420000`, 8 MB) → `_deviceRam` big-endian; outside → base
  `DMARead/DMAWrite`. This IS the microcode's TAG-OUT-6/7 equivalence already.
- The extraction seam is clean: `ProcessMailboxMessage` touches **nothing** from the TAG or
  csStore engines. Its only dependencies: `mar`, the two word accessors, `statusRegister`
  (busy/finished), the base interrupt sink, `Log()`, and the three public diagnostics members
  (`MessageProcessed`, `LastProcessedMicroFunction`, `LastProcessedMessageAddress`) that tests
  bind to and must be forwarded.
- Message offsets/status/MicroFunction enums exist (`N5MessageOffsets` 801, `N5MessageStatus`
  742, `MicroFunction` 761); `DecodeMailboxMessage`/`MicfuName` (2183–2232) produce the decoded
  octal trace lines.

### 1.2 The Octobus side (`NDBusOctobus.cs`, `OctobusFabric.cs`, `OctobusND5000Station.cs`)

- Fabric delivery is **synchronous, in-thread** (`OctobusFabric.SendFrame` 177–261; dest bits
  rewritten to source before delivery; unicast = direct `HandleFrame` call). Reentrancy is real.
- `OctobusND5000Station` (station 70B): decodes emergencies (241B master-clear → `_cpu.Reset()`,
  242B continue, 244B terminate), kicks, SOMB/EOMB multibyte (OMD 0/3 = ACCP-consumed, OMD>3 →
  AOB toward the microprogram), ACCP micro-commands (GetSystemParameters/GetStatus/
  GetCpuTypeAndModel). Microprogram-side surface: `MicroReadAob()`, `MicroWriteAib(ushort)`
  (→ `_fabric.SendFrame` when kicks enabled — the ONLY talk-back primitive), `MicroReadAFlags()`
  (bit9 AOBF / bit10 AIBF, inferred), `MicroSetAccpTrap(bool)`.
- ND-100 receives frames as station 1 (`ND100StationAdapter` → receive FIFO → **level 13**
  input interrupt, ident 40B). The 3022 uses level 12 with the finished/enable gate — the two
  interrupt models differ and stay per-interface.
- **Absent**: OCB 100501B/100401B decode, any activate/answer engine, any mailbox/shared-memory
  access from the station, LOCSD/LOCSM/STARTMIC. (`NDBusOctobus` holds an `NDSharedMemory` but
  it is not wired to the ND-5000 station.)

### 1.3 The CPU (`Emulated.HW\ND\CPU\ND500\CpuND500.*.cs`)

- Constructor ignores the passed `Registers`; live regs = `cpu.Registers` == field `cpu.regs`
  (confirmed, CpuND500.cs:422–436).
- Registers: `P` aliases `PC`; `L`; `I1..I4`; `ST.K` (`StatusRegister.K`, Registers.cs:55).
  No X1 (see §0 correction 1).
- Traps: `TrapCondition : ulong` status-bit flags (PGF, PV, IIC, …); `RaiseTrap` (Trap.cs:184)
  → local handler via `regs.THA` + PCB (`InvokeTrapHandler`, ENTT-opcode-validated) with full
  OTE/CTE/MTE enable-mask and mother-domain propagation — **this IS the DIT/local-handler model
  of microcode TRAP_ENT/TRAP_FIND, already implemented** — else `throw CPUException` →
  `TickCpu` catch → `StopCpuWithReason(CRASHED)`. The "stop with TRAPCODE to the ND-100" third
  path does not exist yet; it replaces the throw for attached-servicer operation.
- DIT exists: `regs.DITBASE`, 16-byte entries {TOS, LL, HL, THA} (Domain.cs:39–48); the richer
  trap-enable masks + `TrapHandlerAddress` + `InsideTrapHandler` live on `ProcessControlBlock`
  (MMU.cs:170–230).
- MON path: CALLG → `CheckAndHandleIndirectCall` → `HandleIndirectSegmentCall`
  (IndirectSegments.cs:137/274) special-cases target segment 31 → `#if SINTRAN_EMULATION`
  `SintranEmulation.ExecuteMonitorCall(monNumber, argCount, argAddresses)` — 201 `MON_*.cs`
  handlers, dictionary dispatch, args by effective address (already resolved into
  `PendingCallArgAddresses`), results = write-back to arg addresses + `regs.I1` + `ST.K`,
  resume at `PendingCallReturnAddress`. Yield/resume template exists
  (`MonitorCallResult.CreatePending()` + `StopMode.WAIT` + `ResumeIO`).
- External memory access for a servicer: `ReadVirtualMemory{,16,32}` / `WriteVirtualMemory{,16,32}`
  (domain-gated by CAD/CED — save/restore idiom exists in `ReadStartAddressVector`), MPM Port B
  via bit 31 (`ReadMpmByte/Word32` etc.), `MapND100ToPhysical`/`ReadND100Word` bridge.
- No instruction-fetch hook, no halt event; halting = `stopMode` flags. `TagWritten` is the
  only CPU→outside event.

---

## 2. Facts that drive the architecture (microcode + SINTRAN side)

1. Lifecycle [V+X+LIVE]: activate → N5STA must be 1 → write WAITING(2) unconditionally →
   MICFU dispatch → answer N5STA:=3/4 (power-fail bits preserved) → doorbell → follow chain
   link, sentinel −1.
2. **Answer-in-place** [V]: MON-call and trap stops answer the process's OWN activation message
   (`srf[ADR_MESS]`); MICFU is left untouched — SINTRAN's DECOMESS dispatches on STOPR.
3. MON exit [V+X]: (addr,value) word pairs into the message data part; 504B/511B/512B also
   inline-copy the user buffer; saved P (offset unknown [?]); STOPR:=MOCALL(1)@11, NUMPA@12,
   MCNO@13; **P:=L before stopping** so 3MONCO resumes after the CALLG; N5STA:=3 + doorbell.
   NDIX fast path answers WITHOUT stopping [V].
4. Restart [V+X]: MONICO writes FUNCV@13, KFLIP@11, NUMPA:=0, MICFU:=3MONCO; microcode delivers
   FUNCV→X1(=I1†), KFLIP→K flag, resumes at saved P.
5. Traps [V]: local macro handler via context block + DIT enables first (SINTRAN never hears);
   else stop with STOPR:=TRAPCODE(2) + TRAPN@16 (page fault 46B), N5STA:=3 — **or 4 if no
   process was running**; page fault additionally builds a swapper message at START_MESS
   (20000B); 5800-only out-of-band OCB trap frames (201B system trap, 203B CPU-unavailable).
6. 3RMICV answers TWO halfwords [V]: version (serve from cached csStore word 1, last part
   [LIVE]) + CPU-parameter halfword.
7. Generation deltas [V]: on the 5800 image, MICFU 05 (3SWMESS) and 27B (3FITRNSF) dispatch to
   MSG_ILLEG; 34B = instruction-memory read, 46B = cache dump-dirty. 3START(23B)/3TRACO(25B)
   share a handler; 3MONCO(24B) delivers the restart; 3WMONCO(26B) block-copies answer data
   into process memory first.
8. Doorbells: classic = 3022 CONTROL activate / level 12 finished-gate [X+LIVE]; 5800 = OCB
   100501B activate in, GIVEINT→ACCP 100401B answer out (observed constants; individual bit
   semantics [?]) [V].
9. **The live oracle runs a 5800 microcode image behind a classic 3022** [LIVE] — so MICFU
   semantics follow the LOADED MICROCODE, not the bus generation. The generation policy must
   therefore be a servicer property, defaulted per doorbell but overridable (and eventually
   derivable from the loaded CS image).

---

## 3. Proposed component model

```
                 ND-100 side                        shared                      ND-500 side
  ┌────────────────────────────────────┐   ┌──────────────────────┐   ┌──────────────────────────┐
  │ NDBusND500IF (3022, level 12)      │   │ Nd500MicrocodeServicer│   │ CpuND500 : IND500Cpu     │
  │  registers/lock/csStore/TAG  ──────┼──▶│  message lifecycle    │◀──┼─ IMonitorCallSink hook   │
  │  LCON5 bit2 ⇒ servicer.Activate()  │   │  MICFU dispatch       │   │  (segment-31 CALLG)      │
  │  IServicerHost impl:               │◀──┼─ AnswerWritten ⇒ host │   │  ITrapSink hook          │
  │   memory=MPM window/DMA,           │   │  stop-record builder  │──▶│  (unhandled traps)       │
  │   doorbell=Finished+CheckTrigger   │   │  restart delivery     │   │  run/stop via stopMode   │
  ├────────────────────────────────────┤   │  generation policy    │   └──────────────────────────┘
  │ OctobusND5000Station (ACCP, st 70B)│   └──────────────────────┘
  │  OCB decode: 100501B ⇒ Activate()  │        one instance per doorbell,
  │  IServicerHost impl:               │        same class, same tests
  │   memory=?? (see §7 decision),     │
  │   doorbell=AIB frame→station 1     │
  └────────────────────────────────────┘
```

### 3.1 Class boundaries (the direct answer to the brief's first question)

**`Nd500MicrocodeServicer` — NEW, one file, `Emulated.HW\ND\CPU\NDBUS\Nd500MicrocodeServicer.cs`.**
Owns: N5STA lifecycle, power-fail-bit preservation, MICFU range check + bit-15 strip [V],
64-entry dispatch, chain walking (link word, −1 sentinel), answer write, stop-record building
(MON + trap), restart delivery, decoded RECV/ANSW trace text, generation policy. It is
**stateless between activations except** the per-process "own message address" registry
(the emulator's `srf[ADR_MESS]` equivalent, §5).

Why not inside `CpuND500`: the servicer must run with `SimulatedND500` and with NO CPU at all
(all admin MICFUs — 3RMICV, RESIRD/WR, cache ops — are pure-engine; today's 1747-green test
suite exercises exactly that). Why not inside `NDBusND500IF`: the Octobus doorbell must consume
the identical engine, and `NDBusND500IF` is IOX-device-shaped (base class, register fields).

**`IServicerHost` — NEW interface, implemented by `NDBusND500IF` and `OctobusND5000Station`.**
What the shared engine needs from a doorbell owner, nothing more:

```csharp
public interface IServicerHost
{
    // ND-100-address-space word access. byteAddress = ND-100 word addr << 1 (MAR convention).
    // 3022 impl: existing ReadNd100Word/WriteNd100Word (MPM window else DMA).
    // Octobus impl: §7 decision.
    ushort ReadNd100Word(uint byteAddress);
    void WriteNd100Word(uint byteAddress, ushort value);

    // The generation-specific doorbell: 3022 = clear busy, set Finished, CheckTriggerInterrupt
    // (level 12 gated on CONTROL bit 0); Octobus = send answer frame toward station 1 (§7).
    void AnswerWritten(uint messageByteAddress);

    // Decoded-trace sink (3022: base Log(); station: its logger). Keeps the MAILBOX RECV/ANSW
    // observability channel identical on both paths.
    void ServicerLog(string message);
}
```

**CPU attachment — extend `IND500Cpu` minimally, implement in `CpuND500` (and stub in
`SimulatedND500`).** The servicer must not depend on `CpuND500` concretely (the 3022 tests run
against `SimulatedND500`). Needed additions (names indicative):

```csharp
// On IND500Cpu (stubs in SimulatedND500 return false / do nothing):
bool StartProcess(uint startInfo);        // 3START/3TRACO: NEWCNTXT + EXECUTE equivalent
void DeliverMonResult(uint funcv, bool kflip);  // 3MONCO: I1† := FUNCV, ST.K := KFLIP
void ResumeExecution();                   // clear WAIT, run at current P
void StopExecution();                     // set WAIT (NOT CRASHED) — process parked
uint ReadProcessWord32(uint logicalAddr); // domain-gated ReadVirtualMemory32 wrapper
bool WriteProcessWord32(uint logicalAddr, uint value); // for 3WMONCO block copy
```

**`IMonitorCallSink` — NEW, the MON-exit seam inside `CpuND500`.** Generalizes the existing
segment-31 branch in `HandleIndirectSegmentCall`:

```csharp
public interface IMonitorCallSink
{
    // Return true = call consumed (CPU parks / resumes per sink's instruction);
    // false = fall through to the next sink / SintranEmulation / trap.
    bool OnMonitorCall(ushort monNumber, uint argCount, uint[] argAddresses,
                       uint returnAddress /* = L */);
}
```

`CpuND500` gets `public IMonitorCallSink MonitorCallSink { get; set; }`, checked FIRST in the
segment-31 branch; when null (or returning false) the existing `SintranEmulation` path runs
unchanged. **Mode policy (user decision 2026-07-17): the REAL MON interface is the target.**
When a servicer + ND-100 are attached, MON calls MUST take the mailbox round-trip
(IMonitorCallSink → STOPR=1/MOCALL stop → real SINTRAN's MCHANDLE services the call →
MONICO write-back → 3MONCO restart) — the partially-implemented 201-handler
`SintranEmulation` direct-emulation layer is NOT used in that configuration. It is retained
strictly as the standalone fallback (no ND-100/SINTRAN attached) and for unit tests that
don't want the round-trip; it must never silently shadow the real path — attach of an
IMonitorCallSink disables it outright rather than per-call.

**`ITrapSink` — NEW, the trap-stop seam.** In `RaiseTrap`, between "local handler found"
(existing `InvokeTrapHandler` — keep first, it IS the microcode's TRAP_ENT [V]) and the current
`throw new CPUException(...)`:

```csharp
public interface ITrapSink
{
    // Return true = trap consumed as a stop-to-ND-100 (CPU parks); false = legacy throw.
    bool OnUnhandledTrap(TrapCondition trap, uint trappingPc, uint trapAddress);
}
```

**No new "ND500ControlII inside this plan"** — the design doc's Phase 3 (5015 + link object)
is orthogonal and untouched: the servicer sits where the 5015's microcode-facing role was
always going to sit, and if/when `ND500ControlII` is built, ownership of the 3022-side servicer
instance moves there without changing the servicer.

### 3.2 Threading & multi-CPU model (REQUIREMENT added 2026-07-17)

**Requirements (user-set):** (a) each ND-500/5000 CPU optionally runs on its OWN host
thread at **full host speed** (no cycle pacing); (b) **multi-CPU**: ND-5000 systems had up
to 4 CPUs (stations 070–073, CPUNO 1–4 [NPL-V]); classic ND-500 systems could have up to
4 controller interfaces, each with its own CPU; (c) all ND-100↔CPU communication must be
thread-safe queues/signals. Per project rules: no LINQ, no allocations on the hot path —
fixed-size ring buffers.

**The protocol IS the synchronization.** The real hardware has no other mechanism than
what we've decoded: a shared-memory semaphore (X5SEM test-and-set), flag cells (X5ACT),
status fields (N5STA), and doorbell signals (kick frames / MAR+CONTROL / GIVEINT
interrupt). The emulator synchronizes the same way:

- **One servicer + one CPU per instance; servicer runs ON the CPU thread.** The
  `Nd500MicrocodeServicer` is single-threaded by construction (like the microcode it
  replaces); it never runs on the ND-100 thread. Up to 4 (servicer, CpuND500) pairs per
  system; the ND-5000 shared structures already support this natively (per-CPU MAILINK
  extension blocks at stride 200B, X5NAC chain, 5CPUN message targeting@−6 [V]); classic
  = one servicer per 3022 interface instance.
- **Doorbell ND-100 → CPU thread:** IOX writes (classic MAR/CONTROL) and shared-memory
  hook hits (X5ACT:=0) and kick frames execute on the ND-100 thread. They must NOT invoke
  the servicer inline; they enqueue a doorbell token into a fixed-size SPSC ring
  (single producer = ND-100 thread, single consumer = that CPU's thread) and signal the
  CPU thread's wake event (`SemaphoreSlim`/`AutoResetEvent`). This mirrors the hardware
  exactly: flag write + kick, servicer polls/wakes. When the CPU is parked (IDLE / MON
  stop / trap stop, StopMode.WAIT), the thread blocks on the same event — zero host CPU
  while idle.
- **Shared memory = one backing array, cross-thread by design** (the MICFU-13B lesson:
  same array as `IND500Cpu.SetMpmMemory`). Rules: X5SEM test-and-set implemented with
  `Interlocked.CompareExchange` on the backing store — this makes the EMULATED semaphore
  a real host-level lock, valid for both emulated contenders (SINTRAN's SLOCK on the
  ND-100 thread) and host threads; all multi-cell mailbox mutations (ring insert + X5FYL
  advance, chain link, N5STA transitions) happen under X5SEM exactly as SINTRAN and
  GIVEINT do [V both sides]; single-cell flags read/written cross-thread (X5ACT, N5STA
  polls) use `Volatile.Read/Write` — aligned 16/32-bit accesses are atomic in .NET, the
  volatile fences give the ordering the real MPM gave via bus serialization.
- **Answer CPU thread → ND-100:** N5STA write + ring insert go through shared memory
  (above). The interrupt/ident (level 12 classic, level-13 octobus ident 40B/41B) must be
  raised via a thread-safe enqueue into the ND-100 interrupt system — **S0 must audit the
  existing ident/interrupt queue for cross-thread safety** and, if it is thread-affine,
  wrap it in an SPSC ring drained by the ND-100 thread (the 3022's existing behavior is
  the template; do NOT call ND-100 devices directly from the CPU thread).
- **Full host speed:** no instruction pacing, SLOW1-3 microcode timings ignored. This is
  safe because the decoded protocol is entirely data/flag-driven, not delay-driven — a
  faster ND-500 only means answers arrive sooner, which the N5STA lifecycle explicitly
  supports (SINTRAN polls status under semaphore). Honest caveat [?]: if SINTRAN has
  timeout-based error paths (e.g. watchdog on a CPU that answers "too fast to be seen in
  state 2"), nothing found so far suggests it — the WAITING(2) write is unconditional and
  observable [V], not required-to-be-observed. Flag for live validation.
- **Deterministic test mode:** the servicer also supports inline (same-thread) operation —
  tests drive doorbell→service→answer synchronously without threads; the threaded mode
  gets dedicated stress tests (concurrent ITOFIFOQ/ITO500XQ producers vs servicer consumer
  under X5SEM, 4-CPU fan-out). `IServicerHost` implementations must document which thread
  each callback arrives on; the contract: all `IServicerHost` calls originate from the
  servicer's (CPU's) thread.

Phase impact: S0's frozen contract includes the thread-ownership rules above + the ident
queue audit; O1's doorbell implementation is the ring+event mechanism from day one (the
inline mode is a degenerate case, not a separate code path).

**TSET atomicity audit (octobus review F-oct-2, owner: architect, due in O1/R1):**
`IServicerHost.TryTakeSemaphore` being Interlocked on the backing array only closes the
race if the ND-100 CPU CORE's TSET/TSETP emulation (opcodes 140123/140516) is ALSO atomic
when the operand lands in MPM memory — a plain read-modify-write in the ND-100 instruction
loop just moves the race. Inline (single-threaded) mode is unaffected; before R1's CPU
thread goes live, audit the ND-100 TSET implementation and route MPM-targeted test-and-set
through the same Interlocked primitive the station host uses.

---

## 4. MON-call exit, mapped onto the existing infrastructure

The microcode flow (pseudocode §3.8 [V]) mapped step-for-step:

| Microcode [V] | Emulator implementation |
|---|---|
| TRAP_MONC code 6 recognizes CALLG into segment 31 | Already intercepted at `HandleIndirectSegmentCall` segment-31 branch [CODE] — hook `IMonitorCallSink` there. **No fake trap 6/7 is built**; the CPU-internal route differs from real hardware but the observable protocol is identical. Grade: emulation shortcut, protocol-equivalent [X]. |
| MON number = low halfword of CALLG target (EQU 37B9+n) | `monNumber` already extracted by the existing branch [CODE]. |
| argc from LC; per-arg G_OPS → (address, value) word pairs into message data part | `argCount` + `PendingCallArgAddresses` already resolved [CODE]; servicer writes pairs: `addr` then `cpu.ReadProcessWord32(addr)` per arg, into the data part of the process's own message. Data-part start offset: after the fixed header per catalog; exact first word [?] — pin from the SINTRAN reader (MCHANDLE) before coding, do NOT guess. |
| mcno ∈ {504B,511B,512B}: inline-copy user buffer | Phase M2; needs per-call buffer descriptor knowledge from the catalog [X]. Defer, flag unimplemented via ServicerLog. |
| saved P written into message (offset [?]) | Same [?] — resolve from SINTRAN reader side first. |
| STOPR:=1 @11, NUMPA @12, MCNO @13 | Direct halfword writes via host memory [X offsets, SYMBOL grade]. |
| **P := L** | `regs.P = returnAddress` before parking — 3MONCO restart then just resumes. |
| CALL_END screening (515B/501B/502B/600B/DUDC family/NDIX) | Phase M3 refinements. NDIX async-answer-without-stopping [V] modeled as: answer + doorbell + do NOT park. Initial implementation: every MON stops (matches the 99% path). |
| N5STA:=3 (answer-in-place, MICFU untouched), GIVEINT | Servicer answer write + `host.AnswerWritten()`. |
| Microcode idles / services chain | `cpu.StopExecution()` (StopMode.WAIT — **not** CRASHED; the process is parked, not dead [CODE stopMode semantics]), then servicer walks the chain link. |

**Where `srf[ADR_MESS]` lives in the emulator:** the servicer records, at activation time, the
message address it activated each process with (single-process first: one `uint _currentMessageByteAddr`;
multi-process later: keyed by whatever process identity 3START carries — resolve from the
catalog's activation-message fields before multi-process work [?]). MON/trap stops write into
THAT address. This is the answer-in-place model [V] and is why the servicer, not the CPU, owns it.

## 5. Restart (3MONCO) into the register model

`MSG_CONMC` [V] mapped:

1. Servicer receives activate; message has MICFU=3MONCO(24B), FUNCV@13, KFLIP@11 [X].
2. NEWCNTXT equivalent: ensure the CPU's current domain/context is the target process
   (single-process: no-op; multi-process: domain switch via existing `PerformDomainSwitch`
   [CODE] — later phase).
3. `cpu.DeliverMonResult(funcv, kflip)` → `regs.I1 := FUNCV`, `regs.ST.K := KFLIP`.
   (X1↔I1 verified via MICROCODE-FIELDS.md, §0 correction 1.)
4. Resume at saved P: P already holds L from the stop (§4); `cpu.ResumeExecution()`.
5. Servicer answers the activation (N5STA:=3) — note [?]: whether the microcode answers the
   3MONCO activation message immediately at EXECUTE or only at the NEXT stop is not pinned in
   the listing (MSG_CONMC ends in EXECUTE, not MSG_END [V]) — **model: no answer until the next
   stop**, matching the listing; verify against SINTRAN's 5RRTWT/XACTRDY expectations [X] in
   the phase test.
6. MSG_CON10's halfword-23B write-back (bookkeeping [?]) — omit, log if SINTRAN is ever
   observed reading it.

3WMONCO(26B) = same, plus first a block copy of answer data from the message into process
memory (`WriteProcessWord32` loop; source/dest/count fields from the catalog [X], exact
offsets to pin before coding).

## 6. Trap-stop path

Triage order, exactly the microcode's [V], mapped onto what exists:

1. **Local macro handler** — ALREADY IMPLEMENTED [CODE]: `RaiseTrap` → OTE/enable masks →
   `InvokeTrapHandler` via `regs.THA` + PCB (`InsideTrapHandler`, ENTT validation), with
   CTE/MTE mother-domain propagation. This corresponds to TRAP_ENT + the DIT walk [V]. **No new
   DIT model is needed for phase T1** — the emulator's PCB/THA model already covers
   "SINTRAN never hears about it". (The microcode's per-trap enable BYTES in the DIT and
   specials 44B/46B/51B [V] are finer-grained than the emulator's masks; treat as refinement
   [?], only needed if a real DOM program exercises per-trap bytes.)
2. **Stop with TRAPCODE** — NEW: `ITrapSink.OnUnhandledTrap` replaces the `throw CPUException`
   when a servicer is attached. Servicer builds into the process's own message:
   `STOPR:=2` @11, `TRAPN` @16 [X SYMBOL], saved P, trap-record words (TRAP_GEN2/3 status words
   — exact word map [?], write what is pinned, log the rest), then
   `N5STA := processWasRunning ? 3 : 4` [V — the test-worthy nuance], doorbell, park CPU.
   **Needs a `TrapCondition` → ND-500 trap-number (TRAPN) mapping table** — only 46B (page
   fault → `TrapCondition.PGF`) is pinned [V]; the full table is [?] and must come from the
   ND-500 reference manual / further carving. Implement as a partial table; unmapped traps
   answer 5ERANSWER + log rather than fabricate numbers.
3. **Page fault extra** [V]: TRAP_GEN4 stop PLUS a swapper message in START_MESS (20000B).
   Phase T2 — requires the swapper-message layout (catalog swapper family [X]) and matters only
   once demand paging is emulated. Not needed for MON-call bring-up.
4. **OCB out-of-band frames** (201B/203B/…) — ND-5000/Octobus only [V], phase O3. On classic,
   there is no out-of-band channel; CPU_UNAVA-class refusals must be modeled differently
   (or not at all) on the 3022 — [?] how the classic microcode signals CPU-unavailable; do not
   invent.

What `CpuND500` needs added: only the `ITrapSink` property + the call site in `RaiseTrap`
before the throw. No DIT changes for phase 1 (see item 1).

## 7. The two doorbells

### 7.1 Classic 3022 (exists, refactor only)

`Write`/`LoadControlRegister` (activate on CONTROL bit 2) keeps ALL current semantics —
lock/busy/finished/CheckTriggerInterrupt stay in `NDBusND500IF` [CODE]. `ExecuteND500Operation`
body becomes `servicer.Activate(mar)`; `IServicerHost.AnswerWritten` = the current
`SetOperationComplete` + trace. `MessageProcessed`/`LastProcessed*` are forwarded from servicer
events so the existing tests stay binding to `nd500IF.*` (zero public-surface change).

### 7.2 ND-5000 / Octobus (new — nothing exists [CODE]; model VERIFIED 2026-07-16)

**Governing principle [V]** (ND-05.020.01 ch. 5.3, via
`E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-ND100-ND5000-REFERENCE.md`): *the octobus
normally carries no data* — it is a control/wake-up path ("look in the mailbox"). ALL data,
including the mailbox itself, lives in **MFbus / MPM-5 shared memory**. PCB 3109 is literally
the "ND-100 Octobus & **MPM** Line Driver" — one card carries both the octobus controller and
the multiport-memory port [V]. So the servicer's memory model is IDENTICAL on both
generations; only the doorbell differs.

- **Activation contract — CLOSED 2026-07-17 (carve answer
  `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md` [NPL-V]
  + microcode ORCON cross-check, pseudocode §3.1a pass 3 [V]).** On ND-5000 there is NO MAR
  write (XACT500 = `GO XACTRDY`, NNJ14). SINTRAN's activation is THREE writes into 5MBBANK
  shared memory + optionally a kick:
  1. **ITO500XQ @022547** links the message into the per-CPU **ex-queue LINK chain** whose
     head the microcode reads at extension-block word 0 (**X5BEX**, init -1 = empty [NPL-V];
     microcode ACTIVATE1→MSG_NEXTL reads base+0 and walks with -1 sentinel [V, D on
     X5BEX naming]). This chain IS the microcode's work queue.
  2. **ITOFIFOQ @030366** writes the message's converted pointer into the **X5FIF ring**
     (X5FIF@6-7 of the GLOBAL header = 32-bit ring BASE POINTER; slot = one 32-bit converted
     message pointer; X5FYL fill / X5HEN head / X5MXF size). The microcode does NOT read this
     ring in the IDLE/ACTIVATE/MSG_NEXTL path [V] — it is the ND-100's in-flight/retire
     tracking (XN500 drain).
  3. **XACTRDY/ACT51 stores 0 into X5ACT** = word 5 of the per-CPU MAILINK extension block
     (stride 200B words) [NPL-V @145500]. The microcode IDLE poll reads exactly this cell:
     srf[0o2017] = block base, halfword read at ORCON=0x0A = byte 10 = word 5; spins while
     nonzero, exits on 0; re-arms it to 1 (IDLE_2) before consuming [V — three independent
     ORCON↔symbol matches: X5ACT@0x0A, X5PRO@0x0C (PRNOWR), X5CCL@0x12 (MSG_CCINCR)].
     Protocol: -1 idle (XMSINIT) / 0 work pending (ACT51) / 1 re-armed (microcode).
- **The kick is the PREEMPT path only [NPL-V ACT52].** If the target CPU is idle or running
  lower-priority work, ACT51 just sets X5ACT=0 and the IDLE poll picks it up — NO kick.
  Only a higher-priority incoming process triggers N100KICK via XKICK500 (kick type 1;
  microcode OCB 100501B = 0x8141 = C | src-station 1 | K | kick#1 [V both sides]). The
  station already fires `KickReceived(kickNumber)` [CODE 303–333], but O1 wiring by kick
  alone is INSUFFICIENT: the servicer must also notice the X5ACT:=0 shared-memory write —
  either a write-hook on the X5ACT cell in `NDSharedMemory` or an idle-poll of the cell
  (mirroring the real microcode). Kick 6 (IDLEKICK) [X] maps to OCB_KICK06 →
  stop/cleanup/IDLE [V].
- **How the servicer finds the block — RESOLVED on the microcode side 2026-07-17
  (pseudocode §3.1b [V]):** NOT via the SYS_READ parameter words. `INIT_ADRP` computes
  srf[0o2017] = START_MESS (patch-panel constant, 20000B area) + SAMSON_CPU (patch-panel
  CPU number) × 256 bytes — 256 bytes = the carve's 5EXTD=200B-word stride exactly. With
  1-based CPU numbering (CPUNO 1..4, 5STATION=CPUNO+FN5DEST-1) this puts the GLOBAL header
  in stride slot 0 and CPU n's block at slot n — matching the carve's "header at the base;
  per-CPU blocks follow" [D fit]. SYS_DATAF reads the ring base (X5FIF, header word 6) and
  ring size (X5MXF, word 5) from that header. The 3 SYS_READ words RESOLVED 2026-07-17
  [NPL-V, CARVE-ANSWER-SYSPAR-LSYSPAR-DISAMBIGUATION.md]: they are the ACCP **LSYSPAR**
  (manual ch. 5.3.13) = CON5IDENT's CMSYSPAR payload built inline — word 1 = **5OMDNO<<8**
  (the ND-100 receive OMD, runtime-allocated by CONOMD; live = 10B — DYNAMIC, never
  hardcode), words 2-3 = 0 always. NOT the N500DF+111B 16-word block (that is the ND-500
  Monitor's SET-SYSTEM-PARAMETERS tunables, MON60 fn 103/104 only — name collision).
  Carve arithmetic confirms GIVEINT's `((w1 & 037400) >> 3) | 100001` = 100401B for
  5OMDNO=10B, naming the interrupt word: it targets SINTRAN's own receive-OMD entry.
  Emulator: one `MailboxBase` (= the header/START_MESS-mapped address in shared memory)
  + CPU number + the CMSYSPAR-delivered 5OMDNO word is sufficient; the station should
  capture word 1 from the CMSYSPAR multibyte it already receives, not from config.
- **Memory access: MPM-5 shared memory — verified, not assumed.** Wire `NDBusOctobus`'s
  existing `NDSharedMemory` [CODE, constructed but unwired to the station] into the station's
  `IServicerHost`. This matches the hardware (3109 = octobus + MPM on one card [V]) and the
  driver design comment [CODE NDBusOctobus 2276–2283].
- **Answer out — RE-CORRECTED 2026-07-17 (GIVEINT decoded, pseudocode §3.1b [V]; this
  supersedes the 2026-07-16 "no ring insert" version, which overcorrected):** FIFOB = the
  X5FIF ring — GIVEINT's descriptor srf[0o2002] is loaded by SYS_DATAF from global header
  word 6 (X5FIF ring base) [V ORCON 0x0C]. The ring is a SHARED notification FIFO: one
  producer index X5FYL under the X5SEM semaphore, TWO producers — ND-100 at activation
  (ITOFIFOQ [X]) and the microcode at answer (GIVEINT [V]: slot[X5FYL] := answered
  message's address, 4-byte slots, X5FYL := +1 mod X5MXF, ring-full check vs X5HEN,
  all under LOCK_QUE = test-and-set on header word 0). So the answer path is:
  (1) servicer takes X5SEM, writes N5STA:=3/4 into the message, **inserts the message
  address into the X5FIF ring at X5FYL and advances X5FYL**, releases X5SEM;
  (2) interrupt word `((SYSPAR-word-1 & 037400) >> 3) | 100001` via ACCP → the observed
  100401B frame [V composition, D shift detail] → ND-100 level-13 input ident [CODE/LIVE
  idents 40B/41B] → **XN500** walks X5HEN→X5FYL checking each entry's N5STA via
  CHN5STATUS [X]. Duplicate entries (activation insert + answer insert of the same
  message) are harmless — retirement is status-driven [D model].
- **Microcode load (LOCSD/LOCSM/STARTMIC)** — absent today; needed for the 5000-generation CS
  gate equivalent (no bit-9 gate on Octobus). Phase O4, spec from OCTOBUS-TEST-PROTOCOL-RE +
  SINTRAN carve; out of scope for the servicer itself.

### 7.3 Generation policy (`Nd500Generation { Classic, Samson5800 }`)

Lives ON THE SERVICER as a property, because the live oracle proves the semantics follow the
loaded microcode, not the bus (§2 fact 9). Defaults: 3022-owned instance → Classic;
station-owned → Samson5800; both overridable. It selects the MICFU dispatch table variant:

- Samson5800 [V]: 05 (3SWMESS) → ILLEG; 27B (3FITRNSF) → ILLEG; 34B = instruction-memory read;
  46B = cache dump-dirty; full table per pseudocode §2.3.
- Classic: table per SINTRAN catalog [X]; where the two disagree and the classic microcode is
  unavailable (no classic CS image found yet — hunt targets in ND500-STATUS-AND-INDEX), the
  SINTRAN sender side is the authority. **MICFU 05 must NOT be hard-coded illegal until the
  carve confirms what SINTRAN sends** [V discrepancy note].
- ~~Future: derive Samson5800 automatically from the loaded csStore version~~ **WITHDRAWN
  2026-07-17 (S0 review F1)**: the live oracle runs a 5800 IMAGE behind a classic 3022 and
  its SINTRAN REQUIRES 21B (3WREG), which is MSG_ILLEG on a true 5800 table — auto-deriving
  Generation from CS content would regress LOAD-SWAPPER on the live machine. The effective
  accept/reject authority is the SINTRAN SENDER: the 3022 host stays Classic regardless of
  the loaded image; Generation changes only by explicit configuration. Regression-guarded
  by Nd500ServicerS1Tests.GenerationDefault_On3022Host_IsClassic_NeverAutoDerived.
- Classic additions pinned 2026-07-17 (catalog 7c/7d + both 5800 listings): 20B (3RREG) and
  21B (3WREG) are CLASSIC-only register-block transfers (MSG_ILLEG on 5800: B30 @015244/45,
  A30 @014260/61); 21B = the LOAD-SWAPPER blocker — servicer stashes the raw register image
  (width 2 words/reg INFERRED) for R1's process-0 context; the swapper-alive acceptance
  chain (21B → 3START → MON 377B N5SWAP → PSWWAIT(7)) is the M1/M2 gate test.

## 8. MICFU handler split (pure-engine vs CPU-routed)

| MICFU | Handler | Engine or CPU | Notes |
|---|---|---|---|
| 1 (3RMICV) | version + CPU-param | **Engine** | TWO halfwords [V]: version from csStore word 1 last part [LIVE]; CPU-param halfword — source [?], make it a servicer property (default 0) until pinned. Which message offsets receive them: [D offsets a,b] — pin from SINTRAN reader (RMVER consumes CPU-DF cache, watchdog path reads it live) before asserting. |
| 13B/14B (RESIRD/RESIWR) | resident copy | **Engine** | 13B exists [CODE, live-proven]; 14B mirrors it. |
| 30B/31B (PHYSRD/WR) | physical copy + PHS select | **Engine** | Same copy core; PHS select param [V]. |
| 12B/45B/46B (CACHE/CLEAR/DUDC) | cache ops | **Engine** (no-op-accept) | Emulator has no caches; answer 3. 46B is dump-dirty on 5800 [V]. |
| 77B (LOOKSRF) | SRF debug read | **Engine** (optional) | Serve zeros or a small emulated SRF map; low priority. |
| 34B/35B (5800: IMEMRD/WR) | instruction memory | **Engine + CPU memory** | Needs CPU/domain memory when a CPU is attached; without CPU → 5ERANSWER. Classic 34B = 3MONO (different!) — generation table decides [V]. |
| 10B/11B (DMEMRD/WR) | process-space copy | **CPU** | NEWCNTXT + domain resolve [V] → `ReadProcessWord32`/`WriteProcessWord32`. |
| 23B/25B (3START/3TRACO) | start/continue | **CPU** | One shared handler [V] → `cpu.StartProcess`; no CPU attached → CPU_UNAVA behavior: classic [?], 5800 = OCB 203B [V] — until pinned, answer 5ERANSWER + log. |
| 24B (3MONCO) | restart | **CPU** | §5. |
| 26B (3WMONCO) | restart + copy | **CPU** | §5. |
| 22B (STARTP0) | start swapper | **CPU** (later) | Swapper = process 0 [X]; multi-process phase. |
| 05/27B | 3SWMESS/3FITRNSF | generation table | ILLEG on 5800 [V]; classic unresolved [?]. |
| 47B (MSG_IDLE) | drop to idle | **Engine** | Park CPU if attached, mark no-process (the N5STA=4-on-no-process state feeds §6.2). |
| trace family 70B–75B, 42B, 44B, 50B–52B | trace/UNIX-500/probe | **Engine stubs** | Accept/log; implement on demand. |

## 9. Phased implementation plan (each phase = green build + tests before the next)

Test style: command-shaped NUnit per the existing idiom [CODE §1.1] — fixture builds the
message in shared memory (`BuildMessage`/`WriteMsgWord`), drives the doorbell (`Act50` or, new,
an Octobus frame helper), asserts N5STA/interrupt/diagnostics. No LINQ, no foreach in product
code, positive + negative cases per fixture.

**PHASE STATUS (2026-07-17, RetroCore branch `ethernet-ii-controller-fixes`):**
- S0 DONE `82e83a148` (suite 1751/0) · S1 DONE `683753505` + LINK fix `8e013d107` (1763/0)
- O1 DONE `fcf150d51` + review fixes `e0ca303ef` (octobus 70/0)
- R1a DONE `b28aa46e4` (MPM one-backing-store + Reset + 21B context load + RunUntilStop; 1786/0)
- R1b DONE `cc419c777` (run thread park/wake full speed + octobus doorbell-to-thread; full 1790/0)
- M1 servicer side DONE `14e4c6ab4` (INd500ProcessHost answer-at-stop + AnswerMonitorCallStop
  MOCALL record + classic async stop completion; filter 110/0)
- M1 CPU side + M2 + T1 IMPLEMENTED, tests written, awaiting suite gate (this session, working
  tree): `Nd500CpuProcessBridge` (both seams + 3MONCO restart FUNCV→I1/KFLIP→K/mask write-back
  + trap stop STOPR=2/TRAPN); seg-31 hook in `HandleIndirectSegmentCall` (emulated MON layer
  bypassed outright when sink attached); TrapSink hook in `RaiseTrap` before the legacy throw.
  NOTE deviations from the plan text below, both source-driven: M1's "saved-P offset carve
  task" was resolved by the microcode §3.10 table (saved P = HW 7-8 N500A; addresses@0o40+2k
  / values@0o100+2k as TWO STRIDED ARRAYS, not pairs); T1's "N5STA=4 no-process variant" is
  N/A at this seam — with no active process message the sink declines and the CPU keeps the
  legacy halt (there is no message to answer).
- M1 CPU side + M2 + T1 COMMITTED `c8cd1db92` (full suite 1819/0). VALIDATED 2026-07-17 by
  both peer sessions (bus: M1-M2-T1-VALIDATION-FROM-BUS-SESSION-2026-07-17.md; octobus:
  verdicts in shared memory). Two confirmed findings FIXED same day (commit pending final
  gate): 3a seg-31 unreachable for real-load-path processes (synthetic seg-31 capability
  when sink attached) + 1a completion-flag thread identity (lost-wakeup race). CONFIRMED
  GAP: Samson5800 can never start a process (21B=MSG_ILLEG there; 5000 context = context
  block via NEWCNTXT) → R2/M3 scope. **Implementation reference (the doc-pass deliverable):
  `ND500-SERVICER-IMPLEMENTATION-REFERENCE-2026-07-17.md`** (same folder) — component map,
  lifecycle, records, threading invariants, generation table, consolidated gaps 1-10.

**Phase S0 — Extract, zero behavior change.**
`Nd500MicrocodeServicer` + `IServicerHost`; `NDBusND500IF` implements the host and forwards
`MessageProcessed`/`LastProcessed*`. Gate: the full existing suite (1747, incl. ~100
interface/command-shaped) stays green with NO test edits. This is the whole phase — resist
adding features.

**Phase S1 — Servicer fidelity upgrades (engine-only).**
(a) chain walking + −1 sentinel [V]; (b) 3RMICV two-halfword answer served from csStore word 1
[V+LIVE]; (c) 14B ResidentWrite; (d) generation table + policy property (05/27B behavior per
§7.3). Tests: chain of 2 messages answered in order then idle; 3RMICV message shows version
halfword == loaded csStore value; Classic-vs-Samson5800 table difference asserted; MICFU-05
NOT asserted illegal on Classic (explicit skipped-assert with the open-question comment).
Live check: next `status` run — decoded trace must still match.

**Phase O1 — Octobus doorbell skeleton. UNBLOCKED 2026-07-17 (activation contract closed, §7.2).**
Station implements `IServicerHost` (memory via NDBusOctobus's shared memory — verified MPM-5
model §7.2; `MailboxExtensionBase` config property until SYS_READ's 3 parameter words are
pinned). TWO activation triggers, both required: (a) **X5ACT doorbell** — the servicer
notices the ND-100's `X5ACT := 0` store at extension-block word 5 (write-hook on the cell in
`NDSharedMemory`, or an idle-poll mirroring the real microcode) and re-arms it to 1 before
consuming, exactly as IDLE_2 does; (b) kick N100KICK(1) via the existing `KickReceived`
event = the preempt path. Work discovery = walk the ex-queue chain from extension-block
word 0 (X5BEX head, -1 sentinel) — the SAME chain walk the classic servicer uses, only the
head cell differs. Answer ⇒ under X5SEM: N5STA write + **X5FIF ring insert of the answered
message's address at X5FYL (advance mod X5MXF, GIVEINT semantics — §7.2 re-corrected
2026-07-17)** + frame to station 1. Tests replay the carved ND-100 producer in order:
ITO500XQ chain link (head := message address), ITOFIFOQ ring write (element[X5FYL] :=
converted pointer, advance X5FYL mod X5MXF), X5ACT := 0 — assert message answered WITHOUT
any kick (idle path); then the kick variant (preempt path); assert X5ACT re-armed to 1
(nonzero) by the servicer; assert the answer appended to the ring (X5FYL advanced by
one more, slot = message address) and X5HEN untouched (consumer-owned); assert X5SEM
released; assert X5PRO updated (PRNOWR semantics) if implemented, else skipped-assert
naming it.

**R1 ground truth (CpuND500 survey agent, 2026-07-17 — all [CODE] with file:line evidence):**
1. **The two-backing-store gap is the top correctness risk**: the CPU's fetch/data path goes
   MMU→SystemBus only (CpuND500.Memory.cs / MMU.cs never consult the MPM helpers); the MPM
   Port-B API (`SetMpmMemory`, `ReadMpmByte…`, bit-31 addressing) is a SIDE api not wired
   into TranslateVirtualAddress. Mailbox 13B/14B copies land in the MPM RAM — the CPU would
   execute from a DIFFERENT store. R1 must unify (same RAM behind SystemBus, or route
   bit-31/shared-window translations through the MPM RAM). This is the 13B-zero-readback
   lesson at CPU level.
2. **No run loop, no park/wake**: only per-instruction `ExecuteOneClockCycle`; StopMode.WAIT
   is set in one place (MON I/O yield, IndirectSegments.cs:178) and consumed by nobody.
   R1 adds the thread loop + wait-handle park (§3.2) checking `regs.stopMode`.
3. **CpuND500.Reset() is a no-op** (no override; SetIrqFlag empty) — the 3022's
   master-clear/programmed-clear calls do nothing to a real CPU. R1 adds a real Reset.
4. **No full context load**: only partial domain/trap save-load exists; 3START needs a new
   CNTXTLOAD-analog (registers+P+ST+CAD/CED/PS/DITBASE+OTE/CTE/MTE+MMU enable).
5. **MON sink hook point** = IndirectSegments.cs:146 (segment-31 branch), BEFORE the
   `#if SINTRAN_EMULATION` block; arg addresses already computed (PendingCallArgAddresses).
   Caveat [?]: segment-31 must be seeded PC_INDIRECT in the PCB program capabilities or the
   CALLG is treated as a direct call — verify/force at attach. **Trap sink hook point** =
   RaiseTrap non-ignorable/fatal branch (Trap.cs:209-237) before the CPUException throw.
6. **Multi-instance is structurally OK** (all architectural state per-instance) EXCEPT:
   a fixed shared temp-file debug write per step when stepping (CpuND500.cs:507-522) and
   unconditional Console.WriteLine in the stop path — must be gated for 4-thread full speed;
   SintranEmulation statics (log level, DeterministicClock, SintranPath.CurrentUser) are
   process-wide — irrelevant once the real MON path is the mode, but never rely on them
   per-instance. Full speed: set TurboMode/ClockSpeed=0 (throttle lives in CpuBase).
7. Registers: `cpu.regs` ≡ `cpu.Registers` (same object) — no discrepancy. The bus already
   has AttachCpu(IND500Cpu) + SetMpmMemory wiring; it never executes the CPU today, and
   3START/22B are answer-only stubs.

**R1 inputs from the bus session (2026-07-17):**
- **The 3START context MUST land in `cpu.CpuRegisters`** — CpuND500's constructor IGNORES
  the Registers instance passed to it (gotcha #4, re-confirmed): loading the 21B stash into
  any other Registers object writes a dead object.
- **Swapper canary (free acceptance instrumentation)**: the swapper's first act after
  context start is the "REV.-K01" build-tag self-check; on mismatch it exits via MON 0B
  LEAVE. So: **MON 0B immediately after 3START = context/mapping wrong; MON 377B subfn 1
  = context right** — the two failure/success shapes are one decoded trace line apart.

**Phase R1 — real CPU attached to BOTH doorbells: download and RUN code (added 2026-07-17).**
Wire the existing `CpuND500` (one implementation — the 500 and 5000 use the same
macro-instruction CPU; only microcode revision/work-mode differs, §7.3) behind the servicer
on both generations, per the §3.2 threading model (own thread, full host speed).
Implements the `IND500Cpu` extension seam: `StartProcess` (3START/3TRACO → MSG_START
semantics: load context, set P, run), `StopExecution`/`ResumeExecution` (StopMode.WAIT
park on the wake event), `Read/WriteProcessWord` for the memory-access MICFUs. Code
download = the mailbox memory functions the servicer already dispatches: 13B/14B
RESIRD/RESIWR (word@7-8 ND-500 addr, word@11B-12B physical, HW@13B count [V]),
DMEMRD/WR, IMEMRD/WR, PHYSRD/WR — all against the SAME backing array the CPU executes
from (the 13B zero-readback lesson). Gate tests, on classic AND octobus hosts: write a
small hand-assembled ND-500 program into memory via 14B messages → 3START → CPU thread
executes it at full speed → program's terminating stop answers the activation message →
ND-100 side observes the answer. This is the "SINTRAN can CSLOAD and RUN" milestone;
M1/M2/T1 then only add the stop-reason richness (MON calls, restarts, traps).

**Phase M1 — MON-call exit via the REAL MON interface (classic path first — the live oracle).**
Real SINTRAN services the MON calls through the mailbox; the emulated 201-handler MON layer
is disabled whenever a sink is attached (§3.1 mode policy) and survives only for standalone
runs. `IMonitorCallSink` in `HandleIndirectSegmentCall`;
`StopExecution/ResumeExecution` (StopMode.WAIT); servicer stop-record builder (STOPR/NUMPA/MCNO
+ (addr,value) pairs; saved-P offset resolved from the SINTRAN reader BEFORE coding — carve
task, blocking); P:=L. Tests: CpuND500 executes a CALLG-37B9+n DOM stub → message fields
asserted word-by-word → level 12 → CPU parked.
**Prereq to resolve first: saved-P/data-part offsets [?] (X1↔I1 already resolved, §0).**

**Phase M2 — 3MONCO/3WMONCO restart.**
`DeliverMonResult` (I1†, ST.K), resume-at-P, 3WMONCO block copy. Tests: full round-trip —
CALLG stop, emulate MONICO write-back (FUNCV/KFLIP/NUMPA:=0/MICFU:=3MONCO), activate, assert
I1/K/PC and that the DOM stub's post-CALLG code runs; K-set error variant; no-answer-until-
next-stop semantics (§5 item 5).

**Phase T1 — Trap stops.**
`ITrapSink` in `RaiseTrap` (after local-handler path, replacing throw when attached); partial
TrapCondition→TRAPN table (PGF→46B pinned; unmapped → 5ERANSWER+log); N5STA 3-vs-4 on
process-running. Tests: trap with local THA handler → SINTRAN never hears (no message write);
trap without handler → STOPR=2/TRAPN=46B message; no-process variant → N5STA=4.

**Phase M3/T2/O2+ — refinements, each gated on carve/live evidence:** 504B/511B/512B inline
buffer copy; NDIX answer-without-stop; CALL_END screening (501B/502B/600B, DUDC family);
page-fault swapper message; multi-process ADR_MESS registry + STARTP0 + domain switching;
Octobus out-of-band trap frames (O3) and LOCSD/LOCSM/STARTMIC load path (O4).

## 10. Open questions blocking specific phases (do not implement past them)

1. ~~X1↔I1 register-naming equivalence~~ **RESOLVED 2026-07-16 [V]**: MICROCODE-FIELDS.md
   (ND-05.022.1) — index registers are X1–X4 in the WRF register map (line 1304) and
   `AB,X1ORS` is glossed "DESC(X)(I1)" (line 1236). Microcode X1 = emulator I1. M2 unblocked.
2. **Saved-P word offset + data-part first-word offset in the message** — ~~blocks M1
   asserts~~ **CLOSED 2026-07-17 [V]**: superseded by the lossless-regeneration offset
   table (pseudocode §3.10): saved P = HW 7-8 (N500A), param ADDRESSES @0o40+2k, param
   VALUES 32-bit @0o100+2k as TWO STRIDED ARRAYS (not pairs). M1 implemented against it.
3. **3RMICV answer halfword offsets + CPU-parameter source** — blocks S1(b) full assert.
4. **Classic MICFU 05/27B semantics** — blocks Classic table completion.
5. **Octobus mailbox plumbing — CLOSED 2026-07-17** (§7.2; carve answer
   CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md [NPL-V] + ORCON cross-check [V], pseudocode
   §3.1a pass 3): (a) ~~the head cell~~ **RESOLVED** — work flag = X5ACT (per-CPU
   extension-block word 5, srf[0o2017]+ORCON 0x0A; -1 idle / 0 work / 1 re-armed), work
   queue = ex-queue chain head at block word 0 (X5BEX), linked by ITO500XQ; kick = preempt
   path only. (b) ~~SYSPAR constants~~ **RESOLVED [NPL-V]**: CMSYSPAR=016B, N100IDENT=1
   (MCOMMAND=007001B), FN5DEST=070B, LN5DEST=073B (stations 070–073), SYSPAR=111B offset in
   N500DF, 16-word block (MON60 fn 103/104). (c) ~~FIFOB identity~~ **RESOLVED 2026-07-17
   [V microcode]**: FIFOB = the X5FIF ring (SYS_DATAF loads srf[0o2002] from header word 6);
   GIVEINT = answer-side producer into it under X5SEM (§7.2). (d) ~~SYSPAR words~~
   **RESOLVED 2026-07-17 [NPL-V]** (CARVE-ANSWER-SYSPAR-LSYSPAR-DISAMBIGUATION.md): the
   3 srf words = ACCP LSYSPAR = CON5IDENT payload (word 1 = 5OMDNO<<8 dynamic, words
   2-3 = 0 always), UNRELATED to the N500DF+111B 16-word block (= ND-500 Monitor
   SET-SYSTEM-PARAMETERS tunables; [I] layout in the carve answer if ever needed).
   Still open, ALL non-blocking for O1: tunables words 10-15 + byte-proof of word order;
   X500D=177745 symbol resolution; ACCP firmware behavior for LSYSPAR words 2-3 (outside
   all carves); the EXQUE (srf 0o2020) SARG-2460B displacement decode; byte-verification
   of the NPL claims against the M06 carve.
6. **TrapCondition→TRAPN full table** — T1 ships partial by design. **PARTIALLY CLOSED
   2026-07-17 [CODE+V]**: the emulator's TrapCondition ST **bit number IS the TRAPN
   vocabulary** (PGF = bit 38 = 46B page fault matches; GetTrapNumber() returns it
   directly) — no mapping table needed unless a specific trap's bit is proven to diverge.
7. **Classic CPU-unavailable signaling** (no OCB channel) — blocks the no-CPU 3START answer
   choice being anything but 5ERANSWER+log.
8. **3WMONCO (26B MSG_CONWR) block-copy field layout** — **CLOSED 2026-07-17 [V]** by the
   bus session's full walk of 015752-016004 (R2-VALIDATION-FROM-BUS-SESSION-2026-07-17.md
   item 5): source = ND-100 WORD address @ HW 0o140 (ABUFA, reached via two chained +0x60
   MARG hops because +0xC0 is unencodable), dest = 26ADD 32-bit @ HW 0o15-0o16, count =
   26NRB bytes @ HW 0o17; the >=0x2000 oversize guard SKIPS the copy but still resumes
   with K=1 and X1=0o174 (not an error answer). Flow: NEWCNTXT → shared 24B fetch → copy →
   MICFU:=23B → EXECUTE. IMPLEMENTED same day (servicer 26B case + OnWaitMonitorCallRestart
   + bridge byte copy; matches both SINTRAN builders slot-for-slot per the bus session).

## 11. Sources

- `E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md` (all [V] claims)
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-MAILBOX-MESSAGE-CATALOG.md` (offsets, MONICO, DECOMESS [X])
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-WHO-ANSWERS-THE-MAILBOX.md`, `ND500-BUS-INTERFACE-REFERENCE.md` (§6.5 X500DF FIFO/semaphore, §7.5 XN500 drain), `ND500-CS-LOAD-TRACE-FINDINGS-2026-07-16.md` ([X]/[LIVE])
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-ND100-ND5000-REFERENCE.md` (octobus-carries-no-data principle, kick/XKICK500/XN500 mapping table [V/X])
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\CARVE-ANSWER-ND5000-ACTIVATION-WORKFLAG.md` (2026-07-17 [NPL-V]: X5ACT work flag, ex-queue/ITO500XQ, kick=preempt-only, X500DF header + per-CPU extension-block layouts, CMSYSPAR/N100IDENT/FN5DEST/SYSPAR constants)
- `E:\Dev\Ronny\ND5000UC\manual\MICROCODE-FIELDS.md` (ND-05.022.1 — X1–X4 = index register file, X1≡I1 [V])
- `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\ND500-BUS-INTERFACE-DESIGN.md` (locked decisions, phase numbering context)
- RetroCore source exploration 2026-07-16 ([CODE]): `NDBusND500IF.cs`, `NDBusDeviceBase.cs`,
  `NDBusOctobus.cs`, `OctobusFabric.cs`, `OctobusND5000Station.cs`, `CpuND500.cs`,
  `CpuND500.Trap.cs`, `CpuND500.Domain.cs`, `CpuND500.MMU.cs`, `CpuND500.Memory.cs`,
  `CpuND500.IndirectSegments.cs`, `CpuND500.ND100Bridge.cs`, `Registers.cs`, `IND500Cpu.cs`,
  `SimulatedND500.cs`, `Sintran\SintranEmulation*.cs`, `Instructions\CALL\Callg.cs`
