# ND-500/ND-5000 Mailbox Servicer & CPU Integration — Implementation Reference

**Date:** 2026-07-17 · **Status:** phases contract→S0→S1→O1→R1a→R1b→M1→M2→T1 implemented,
validated by both peer sessions, all gates green.
**Code:** RetroCore branch `ethernet-ii-controller-fixes` (not pushed — shared branch).
**Design authority:** `ND500-MICROCODE-INTEGRATION-ARCHITECTURE-2026-07-16.md` (same folder).
**Microcode ground truth:** `E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md`
(§3.1a/§3.1b activation+GIVEINT, §3.8 MON exit, §3.9 traps, §3.10 verified offset table).

Evidence legend (same as the architecture doc): **[V]** microcode/byte-verified ·
**[NPL-V]** NPL-source-verified · **[CODE]** verified in emulator source · **[D]** deliberate
model choice, marked in code · **[?]** open.

---

## 1. What this system is

The C# reimplementation of the ND-500/5800 **microcode's mailbox servicer** — the thing that
answers 5MPM messages — plus the seams that let the **real `CpuND500`** run downloaded code
and talk back to real SINTRAN through the mailbox (MON calls, restarts, traps), replacing the
emulated 201-handler MON layer.

One servicer engine serves both generations:

| | Classic ND-500 (3022/5015) | ND-5000/SAMSON (octobus) |
|---|---|---|
| Doorbell in | ACT50: LMAR5 ×2 (MS first) + LCON5 bit 2 | X5ACT:=0 idle poll + kick 1 preempt |
| Work discovery | MAR = message WORD address | X5BEX ex-queue chain @ ext-block word 0 |
| Answer out | N5STA:=3 + finished + level 12 | N5STA:=3 under X5SEM + X5FIF ring insert + GIVEINT frame |
| Host class | `NDBusND500IF` | `OctobusND5000Station` |

## 2. Commit ladder (all gates = full ND-500 suite unless noted)

| Commit | Phase | Content | Gate |
|---|---|---|---|
| `82e83a148` | S0 | Extract `Nd500MicrocodeServicer` + `IServicerHost`, zero behavior change | 1751/0 |
| `683753505` | S1 | Chain walk, 3RMICV two-halfword, generation table, 21B stash, 13B/14B round-up | 1763/0 |
| `8e013d107` | S1 fix | LINK captured BEFORE answering (F-oct-1) | — |
| `fcf150d51` | O1 | Octobus doorbell: station as IServicerHost, X5ACT poll, GIVEINT tail | octobus 70/0 |
| `e0ca303ef` | O1 fix | MPM bounds guard, kick R1-conversion marker | — |
| `b28aa46e4` | R1a | ONE backing store (MPM in CPU path), real Reset, 21B context load, RunUntilStop | 1786/0 |
| `cc419c777` | R1b | CPU run thread (park/wake, full speed), kick→doorbell-flag→CPU-thread drain | 1790/0 |
| `14e4c6ab4` | M1a | Servicer answer-at-stop + MOCALL record + classic async completion | 110/0 filter |
| `c8cd1db92` | M1b/M2/T1 | Seg-31 sink hook, `Nd500CpuProcessBridge`, 3MONCO restart, trap stops | 1819/0 |
| `5408d5984` | fixes | Validation 3a (seg-31 reachability) + 1a (completion thread-identity) | 1822/0 |
| `c62f16b81` | R2 | AttachRealCpu wiring (both hosts), servicer engine lock, SAMSON context-block start | 1830/0 |
| `373bdc28b` | R2 fixes | R2-1 status lock, R2-2 park-before-sink, R2-4 CED/CAD byte mask, R2-6 resume-in-place, 26B 3WMONCO | 1837/0 |
| `1bbe4239d` | Phase 3 | Octobus-session SAMSON swapper-alive canary end-to-end (their authorship) | octobus 78/0 |
| `3d5d3fb10` | tests | Bus-session R2 test package: classic canary, R2-3 emulator half, adversarial 26B, R2-6 real path (their authorship) | ND500IF 181/0 |
| `c2a319fe7` | R2-7 | 26B write-back/copy order fixed to microcode order (copy wins on overlap) | ND500IF 181/0 |

## 3. Component map

All paths under `E:\Dev\Repos\Ronny\RetroCore\`.

**The engine** — `Emulated.HW\ND\CPU\ND500\Servicer\`
- `Nd500MicrocodeServicer.cs` — ProcessChain (MSG_NEXTL walk, LINK@0, −1 sentinel [V],
  LINK captured before answering [D — real microcode reads it after MSG_END], 0-link +
  64-cap guards), ProcessMessage (N5STA 1→WAITING(2) unconditional [V]→3/4, 64-entry MICFU
  dispatch), `AnswerMonitorCallStop`, `AnswerTrapStop`, `AnswerActiveProcessMessage`
  (shared stop tail), `AnswerRingInsert` (GIVEINT [V]).
- `IServicerHost.cs` — the host seam: `ReadNd100Word`/`WriteNd100Word` (BYTE addresses,
  big-endian words), `Nd500AddressBase`, `MailboxHeaderBase`/`CpuExtensionBlockBase`
  (0 = classic N/A), `TryTakeSemaphore`/`ReleaseSemaphore` (host-atomic), `AnswerWritten`,
  `ServicerLog`. All calls on the servicer's thread.
- `INd500ProcessHost.cs` — servicer→CPU: `OnStartProcess` (true = taken, message stays
  WAITING, answer at stop), `OnMonitorCallRestart` (default-false DIM so start-only hosts
  keep compiling).
- `IMonitorCallSink.cs` / `ITrapSink.cs` — CPU→servicer stop seams.
- `Nd500CpuProcessBridge.cs` — ties ONE `CpuND500` to ONE servicer; implements all three
  CPU-facing interfaces. Attach wires `servicer.ProcessHost` + `cpu.MonitorCallSink` +
  `cpu.TrapSink`; `Detach()` unwires.
- `N5MailboxProtocol.cs` — `N5MessageStatus`, `N5MicroFunction`, `N5MessageOffsets`.
- `Nd500Generation.cs` — `Classic` / `Samson5800`.

**The hosts**
- `Emulated.HW\ND\CPU\NDBUS\NDBusND500IF.cs` — classic 3022. MAR<<1 → ProcessChain;
  completion has TWO shapes (§6).
- `Emulated.HW\ND\CPU\NDBUS\OctobusND5000Station.cs` — Samson. `AttachSharedMemory`
  (same array as the ND-100 window — the one-backing-store rule), `ConfigureMailbox`
  (ext block = header + CPUNO×256 bytes), `ServiceMailbox()` (one IDLE-poll iteration),
  `DrainDoorbells()` (CPU-thread consumer), kick-1 → Interlocked pending flag + wake.

**The CPU** — `Emulated.HW\ND\CPU\ND500\`
- `CpuND500.Memory.cs` — post-MMU routing: bit-31 or pa<window → MPM RAM (bounds-guarded,
  never mask-wraps to SystemBus), else SystemBus. No-MPM behavior byte-identical.
- `CpuND500.ProcessControl.cs` — `Reset()` (real master-clear), `StartProcessFromRegisterImage`
  (21B image; index 0 = P [D], rest stashed verbatim until the register map is pinned [?]),
  `RunUntilStop`, run thread (`StartRunThread`/`StopRunThread`/`WakeRunThread`,
  `OnRunThreadPark` hook, TurboMode), `MonitorCallSink`/`TrapSink` properties.
- `CpuND500.IndirectSegments.cs` — the seg-31 MON gate (§5).
- `CpuND500.Trap.cs` — the trap sink hook (§7).

**Tests** — `Emulated.Tests.ND500\nd500if\`: `Nd500ServicerS1Tests` (12),
`Nd500CpuR1Tests` (27), `Nd500ServicerM1Tests` (10), `Nd500CpuM1BridgeTests` (9),
`Nd500CpuM2RestartTests` (6), `Nd500CpuT1TrapTests` (5), `Nd500ValidationFixTests` (3);
`Emulated.Tests.ND100\ControllerOctobus\OctobusMailboxO1Tests` (11).

## 4. The activation / answer-at-stop lifecycle [V]

```
SINTRAN builds message, N5STA:=1 ─→ doorbell ─→ servicer fetch: N5STA:=2 WAITING (unconditional)
   │
   ├─ engine-only MICFU (3RMICV, 13B/14B, 20B/21B, …) → answer NOW: N5STA:=3/4, host signalled
   │
   └─ start-class MICFU (23B 3START / 25B 3TRACO — shared handler [V]; 24B 3MONCO restart)
        → offered to INd500ProcessHost
        ├─ declined / no host → immediate answer (pre-CPU placeholder, byte-identical)
        └─ TAKEN → message STAYS WAITING; it is the process's answer-in-place message.
             The process's NEXT STOP answers it:
             · MON call  → MOCALL record, N5STA:=3   (STOPR=1)
             · trap      → trap record,  N5STA:=3    (STOPR=2)
             then the shared tail: power-fail bits preserved (&0xE000), Samson X5SEM +
             ring insert, ActiveProcessMessageAddress:=0, host.AnswerWritten.
```

MICFU is never rewritten at a stop [V] (except the 3MONCO 23B bookkeeping, §6.2) — which is
why DECOMESS accepts {3START,3TRACO,3MONCO,3WMONCO} and dispatches on STOPR
(validated against the carve by the bus session; octobus session found nothing contradicting).

## 5. MON-call exit (M1)

**Entry:** `CheckAndHandleIndirectCall` → segment 31. With a `MonitorCallSink` attached:
- Segment 31 is routed to the MON path **unconditionally** via a synthetic seg-31
  capability (validation fix 3a): the real SINTRAN load path (14B copies + 21B image)
  never seeds the C#-side PCBTable, so gating on PC_INDIRECT made the sink unreachable.
  [?] open: does the real CALL microcode read the indirect bit from the memory-resident
  PCB or hardwire segment 37B — asked of the microcode LLM.
- The **emulated SintranEmulation MON layer is bypassed OUTRIGHT** (mode policy, user
  decision 2026-07-17) — it survives only for standalone runs with no sink.
- Taken: CPU parks (`StopMode.WAIT`) at the CALLG return address (the microcode's P := L
  [V §3.8]). Not taken (no active process message): honest HALT, never a silent fallback.

**The MOCALL record** (`AnswerMonitorCallStop`) — byte-verified [V §3.10], confirmed
slot-for-slot by the bus session against symbols 5PPA1/5PPA2/5DPn:

| Field | Where | Value |
|---|---|---|
| saved P | HW 7-8 (N500A slot, 32-bit hi-first) | the CALLG return address |
| STOPR | HW 0o11 | 1 (MOCALL) |
| NUMPA | HW 0o12 | argc (clamped ≤16 — the microcode slot limit [V]) |
| MCNO | HW 0o13 | MON number (= CALLG target low halfword) |
| param ADDRESSES | HW 0o40+2k, 32-bit | operand effective addresses |
| param VALUES | HW 0o100+2k, 32-bit hi-first | read from CPU virtual memory by the bridge |

TWO STRIDED ARRAYS — not (addr,value) pairs (the §3.10 correction of carver R1).

## 6. Completion & restart

### 6.1 Classic completion — two shapes [CODE]
- **Synchronous** (the activate itself answered everything): driven by ProcessChain's
  return value; `AnswerWritten` is suppressed. A taken start returns "nothing answered" —
  no FINISHED, the interface stays busy/locked (matches the real driver: DECOMESS
  fires on the stop, TERM5 later releases the lock; confirmed by the bus session).
- **Asynchronous** (a stop answers outside any activate): `AnswerWritten` →
  `SetOperationComplete()` (finished + level 12).
- Discrimination is by **thread identity** (`_syncMailboxThreadId`, validation fix 1a):
  a plain bool lost cross-thread stop completions (lost level-12 wakeup — fatal).
- **[?] LIVE-TRACE GAP:** every live trace so far is synchronous; the async shape's oracle
  is the first wired-up start-swapper run (bus session flag 1b).

### 6.2 3MONCO restart (M2) [V]
Servicer 24B case reads: KFLIP @0o11, FUNCV 32-bit @0o13-0o14, NUMPA as **write-back
mask** — bit k ⇒ 32-bit value @0o100+2k written to the 32-bit address @0o40+2k in
PROCESS memory (MSG_CONMC_33/4/5 015734-751 [V]; symbol-level confirmation:
5ACTSWAPPER stages NUMPA:=6 with SWPST@0o103/HSWPI@0o104 = value slots k=1,2).
Bridge applies: write-back pairs via CPU virtual writes, FUNCV→**I1** (microcode X1 ≡
emulator I1 [V]), KFLIP≠0→K flag set, un-park + wake. The restart message becomes the
NEW answer-in-place target; MICFU rewritten 23B (MSG_CON10 bookkeeping [V 015715]);
nothing answered until the next stop. Declined (CPU not WAIT-parked / no host):
pre-CPU immediate answer.

**3WMONCO (26B) IMPLEMENTED** (2026-07-17, after the bus session's full decode of
015752-016004 closed open question 8): the 24B restart plus a bounded block copy BEFORE the
resume — source = ND-100 WORD address @ HW 0o140 (ABUFA) <<1 to bytes, dest = 26ADD 32-bit
@ HW 0o15-0o16, count = 26NRB bytes @ HW 0o17 [all V]. The >=0x2000 oversize guard skips
the copy and still resumes with K=1, X1=0o174 (in-band error, NOT an error answer) [V].
Servicer reads the record + source data, `OnWaitMonitorCallRestart` carries it, the bridge
writes byte-wise into process memory then delegates to the shared restart tail.

## 7. Trap stops (T1)

Hook in `RaiseTrap`: local THA/PCB handler dispatch stays FIRST (it IS the microcode's
TRAP_ENT local-DIT path [V]); then, sink attached → trap becomes a stop-to-ND-100; else
legacy `CPUException` throw (byte-identical pre-T1 behavior).

**KEY FINDING [CODE+V]:** the emulator's `TrapCondition` ST **bit number IS the TRAPN
vocabulary** — PGF = bit 38 = 0o46 = SINTRAN's page-fault number; the whole enum
(bits 11–41 = 13B–51B) sits inside TRAPDECODER's legal 0..53B window (bus session
verified). No mapping table.

Record: STOPR=2 (TRAPCODE), TRAPN @0o16, saved P in the 0o12/0o14 status-word slots
(TRAP_GEN4B shape [V]; 32-bit write width [D]), fault address @0o17-0o20 [D — TRAPDECODER
never reads it; the true consumer is the swapper, pin from the swapper carve or TRAP_GEN3].
No active process message → sink declines, legacy halt (the microcode's "N5STA:=4 if no
process" case has no message to answer at this seam — deviation documented).

## 8. Threading model & invariants [CODE, architecture §3.2]

1. **One servicer + one CPU per instance; the servicer runs on the CPU's thread.**
   All `IServicerHost` and sink calls are same-thread by contract.
2. **Doorbells never call the servicer inline from a foreign thread.** Octobus kick-1:
   `Interlocked` pending flag + `MailboxDoorbell` wake; drained by `DrainDoorbells()`
   on the CPU thread. (Inline fallback only when no doorbell subscriber — the
   single-threaded/test mode.)
3. **The park loop**: run thread parks on any stop bit via `AutoResetEvent` (zero host
   CPU); `OnRunThreadPark` is the drain point; TurboMode = full host speed (safe: the
   protocol is flag-driven, not delay-driven [V]).
4. **No X5SEM is ever held across a call into the CPU** — it is taken only in the answer
   tail, after MICFU dispatch (octobus session verified: no reentrancy, no lock-ordering
   hazard).
5. **ENFORCED-BY-CONVENTION ONLY (octobus flag 6):** `DrainDoorbells` currently has NO
   production wiring — only tests wire `cpu.OnRunThreadPark = station.DrainDoorbells`.
   When the machine wiring lands (R2), "drain runs on the CPU thread at park time" is a
   HARD invariant: draining from the ND-100 device thread would be a data race on the
   single-threaded servicer.
6. **[?] TSET audit outstanding (F-oct-2):** the ND-100 CPU core's TSET/TSETP emulation
   must be atomic against `TryTakeSemaphore`'s Interlocked on the same backing array
   before real ND-100-thread contention exists.

## 9. Generation differences

| Aspect | Classic | Samson5800 |
|---|---|---|
| 21B 3WREG / 20B 3RREG | live (the LOAD-SWAPPER context path) | MSG_ILLEG → 5ERANSWER [V both listings] |
| 05 / 27B (3SWMESS/3FITRNSF) | live | MSG_ILLEG [V] |
| Answer tail | plain N5STA write, finished+level 12 | X5SEM + X5FIF ring insert + GIVEINT frame |
| Interrupt word | (level-12 ident) | ((LSYSPAR-w1 & 037400)>>3) \| 100001 — DYNAMIC from CMSYSPAR 5OMDNO; needs 5OMDNO≥8 to reach station 1 |
| Generation source | **sender is authority — NEVER auto-derive from CS content** (review F1) | same |

**~~CONFIRMED GAP~~ CLOSED by R2b (2026-07-17, this session):** the Samson start path is
implemented from a fresh microcode decode (`CNTXT-BLOCK-DECODE-2026-07-17.md`, same folder —
GET_CNTXT/NEWCNTXT/CNTXTSAVE/CNTXTLOAD walked word-by-word with the §3.10 ADACT/MARG model):
- ctx = 0o4000 + 0o400×(X5CPU+1) ND-500 physical bytes (base [V], 256-byte stride [D forced
  by the CNTXTSAVE/LOAD address arithmetic]); X5CPU = message HW 4.
- Field map (load/save symmetric, [V]): P@0x00, L@0x04, B@0x08, R@0x0C, X1-X4(=I1-I4)@0x10,
  A1-A4@0x20, E1-E4@0x30, CED@0x5C (→MM,DOM), CAD@0x60 (→MM,ADOM).
- Servicer: Samson 3START/3TRACO computes ctx and calls `OnStartProcessSamson`; bridge loads
  via `CpuND500.StartProcessFromContextBlock`. Uninitialized block (P=0) declines [D guard].
- DELIBERATELY not loaded yet: status composite @0x40 (WRITEST1 redistribution), MM,PS/PHS
  @0x48, SC1/SC2, trap park area @0x94-0xBA, DIT-based trap enables (@0x80+CED×256 [V
  offsets, layout partial]). The CPU_AVAIL srf gate maps to "host attached and takes the
  start" [D]; CPU_UNAVA's 203B OCB answer is not modeled.
21B stays MSG_ILLEG on Samson (regression-pinned) — the context path replaces it.

## 10. Known gaps & open questions (consolidated)

| # | Item | Blocks | Owner/next step |
|---|---|---|---|
| 1 | ~~Samson start path~~ CLOSED (R2b, §9) — residue: status@0x40 composite, MM,PS/PHS@0x48, DIT trap enables, CPU_UNAVA 203B | full Samson fidelity | later phase, decode doc unknowns |
| 2 | 3WMONCO copy source + MSG_CONWR_W/_B operand decode | M2 tail | microcode LLM (arch doc Q8) |
| 3 | Swapper-alive canary run (21B→3START→MON 377B vs MON 0B) | M1 live acceptance | machine wiring + live run |
| 4 | Async answer-at-stop live-trace coverage | classic completion confidence | same live run (flag 1b) |
| 5 | DrainDoorbells production wiring + thread invariant | octobus threaded operation | R2 machine wiring |
| 6 | TSET/TSETP atomicity vs TryTakeSemaphore (F-oct-2) | real contention | audit before multi-thread machine |
| 7 | 21B register map beyond index 0 = P [D] | full context fidelity | swapper canary decides |
| 8 | Seg-31 indirect bit: memory PCB or hardwired 37B? [?] | fix-3a fidelity | microcode LLM |
| 9 | Fault-address slot @0o17 [D] | trap record fidelity | swapper carve / TRAP_GEN3 |
| 10 | PC_INDIRECT seeding for real SINTRAN domains | superseded by fix 3a | closed unless Q8 contradicts |

## 11. Validation record

Both peer sessions reviewed every commit adversarially under code freeze (findings as
flags, no edits):
- **Bus session:** `M1-M2-T1-VALIDATION-FROM-BUS-SESSION-2026-07-17.md` (same folder) —
  6 verdicts: items 1/2/4/5 CONFIRMED at symbol/byte level, 3a DISCREPANCY (fixed, §5),
  6 half-closed (sender fields pinned). Secondary flags 1a (fixed, §6.1) and 1b (live-trace
  gap, open item 4).
- **Octobus session:** verdicts in the shared memory file — items 1/2/3 PASS, 4
  station-side harmless, 5 GAP CONFIRMED (§9), 6 NEW FLAG (§8 invariant 5). Octobus
  suite 74/0 at `c8cd1db92`.
