# ND-5000 Multi-CPU + Octobus I/O Emulation — Comprehensive Plan

**Date:** 2026-07-18
**Author:** octobus session
**Status:** PLAN / analysis. Scope decided 2026-07-18 (see §0.1). No code written yet.
**Repo:** `E:\Dev\Repos\Ronny\RetroCore`

## 0.1 Scope decision (Ronny, 2026-07-18)
- **Document everything** (through Phase 4/DOMINO) — this doc.
- **Implement Phases 0+1+2** here (Phase 0 = one-CPU wiring is the unavoidable foundation for 1+2).
- **NUCLEUS is IN scope** — add the 4 microcoded 5000-local primitives (`nkMove`/`nkSend`/`nkReceive`/`nkGetInfo`) as C# servicer analogs (folded into Phase 2/3).
- **DOMINO (Phase 4) handed off to a separate agent** — needs its own DIOC↔5000 protocol carve first. A handoff brief is a deliverable of this plan.

Evidence tags: **[V]** verified in code or byte-cited doc · **[D]** inferred/deduced · **[?]** open.

---

## 0. Executive summary — what is already done vs what this plan adds

The single most important framing: **the "macrocode variant of the microcode functionality" already exists and is committed.** The B30 SAMSON mailbox microcode has been reverse-engineered into C# and reimplemented as a *servicer* that runs beside a real `CpuND500` executing genuine ND-500 **macro** instructions. The S0→S1→O1→R1→M1→M2→T1→R2 phase ladder is complete and green (full ND-500 suite 1848/0/6 at last gate).

So this plan is **not** "build the macrocode equivalent." It is:

1. **Wire what exists into a real machine** (today it only runs in tests).
2. **Add a per-CPU configuration surface** (id / model / station / speed).
3. **Run multiple ND-5000 CPUs at once** (stations 70B–73B).
4. **Emulate the octobus I/O controllers** (DOMINO DIOCs / MF-controller) — the genuinely new, largest piece.
5. Incrementally **fill the remaining microcode→macrocode gaps** the current corpus still marks `[?]`.

---

## 1. Verified current state (RetroCore)

### 1.1 What is REAL and committed [V]
- **Shared CPU:** `Emulated.HW\ND\CPU\ND500\CpuND500.cs` (+ `.Execute/.Fetch/.Memory/.Trap/.ProcessControl/.IndirectSegments`). Full ND-500/5000 macro instruction set under `Instructions\`. Assembler/disassembler in `Emulated.Assembler.ND500`. There is **no separate `CpuND5000`** — the 5000 rides on `CpuND500` distinguished by the `Nd500Generation.Samson5800` enum.
- **Octobus 5000 side:** `Emulated.HW\ND\CPU\NDBUS\OctobusND5000Station.cs` (1131 lines) — ACCP/AOB/AIB, kicks, emergencies (241B/242B/244B), ACCP micro-cmds 1–3, OMD 0/3 multibyte (CMCPURES/CMSYSPAR), `IServicerHost`, `ServiceMailbox`/`WalkQueue`/`DrainDoorbells`, GIVEINT via `AnswerWritten`. Default station 70B.
- **Octobus 100 side:** `NDBusOctobus.cs` (2934 lines) — PCB 3096/3109 card, IOX map, OMD-0 test-protocol responder, `AttachCpu` builds + registers the 5000 station on the fabric.
- **Fabric:** `OctobusFabric.cs` — station registry + dest→source rewrite.
- **Servicer (the reversed microcode):** `Emulated.HW\ND\CPU\ND500\Servicer\Nd500MicrocodeServicer.cs`, `N5MailboxProtocol.cs`, `IServicerHost.cs`, `IMonitorCallSink.cs`, `ITrapSink.cs`, `Nd500CpuProcessBridge.cs`. MICFU dispatch, 3START context-block load, 3MONCO/3WMONCO restart, 3RMICV, 13B/14B RESIRD/RESIWR, MON exit, trap stops.
- **3022 level-12 pending latch** (R2-8/R2-9) committed `a3e34600a`.
- **Device key:** `CreateAndRegisterDevice("ND5000")` → `new NDBusOctobus(...)` in `ND100Machine.cs`.

### 1.2 Verified GAPS (by absence) [V]
- **G1 — No production machine wiring.** `NDBusOctobus.AttachCpu` has **no non-test caller**. No `ND5000` debugger machine-init command (only `ND500`). A real machine that attaches a running ND-5000 CPU to the octobus does not exist outside tests.
- **G2 — Config not plumbed.** `CpuTypeAndModel`, `SystemParameters`, ACCP identity are public setters with defaults, wired to **no** INI/CLI. **No serial-number field exists** — and no guest software reads a serial (only station number and CPU model), so a serial would be an emulator-invented, cosmetic field.
- **G3 — Multi-CPU not exercised.** Threading is architected (one servicer + one CPU per thread, §3.2 of the servicer reference), but nothing brings up stations 70B–73B concurrently.
- **G4 — No octobus I/O controllers.** No DOMINO DIOC and no MF-controller are emulated. SINTRAN `MFPREPARE` frames to stations 2B–6B go unanswered. The `MicroReadAob`/`MicroReadAFlags` station→CPU direction is modeled but has no executing-CPU consumer.
- **G5 — Microcode corpus gaps `[?]`:** several MICFU entries and SRF cells (0o2004/0o2005/0o2011) and per-MON worker bodies beyond 13B are still inferred; the 4 microcoded NUCLEUS calls are undecoded.

---

## 2. What the new reference docs establish (2026-07-18)

Sources: `ND-14001-1-EN DOMINO Standard Hardware Description.md` [DOMINO-HW], `ND-820026.1 EN DOMINO and NUCLEUS Software Guide.md` [SW-Guide], `ND-814009-1-EN DOMINO SCSI Operator Guide.md` [SCSI-OG], `ND-830102.1B EN ND-5000 ES Model C Hardware Maint. Manual-Sintran.md` [Sintran-MM] — all under `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\`.

### 2.1 Station numbering — a real tension to resolve [V]
- Hardware rule: **global** octobus nodes = 0–17B (thumbwheel-set); **local** nodes (DIOCs, ND-5000s in an MFbus crate) = **77B↓ to 20B, assigned dynamically by the MFB controller** via the "Identify yourself to 77B" probe loop [DOMINO-HW 3671–3721].
- BUT SINTRAN's ND-5000 driver uses **fixed constants**: `FN5DEST=070B`/`LN5DEST=073B`, `5STATION := ASTAT(070B)+cpu-index`, `CH5CPUPRESENT` scan [V, carve]. And `LIST-CONFIGURATION` examples show DIOCs at 10B/12B/13B (operator-configured) [SW-Guide 1582–1590; SCSI-OG 443].
- **Resolution for emulation:** we target SINTRAN, so we **pre-assign** station numbers directly (ND-5000 CPUs → 70B–73B; DIOCs → operator-config values like 10B/13B). We do **not** need to emulate the MFB-controller dynamic-assignment configurator. This is a major simplification and it matches what the guest OS expects.

### 2.2 Per-CPU config: what's real [V]
- Configured into each node at init: **station number, broadcast type, power-fail handler station, octobus speed** (WOI + WMT/BADAP registers) [DOMINO-HW 3058–3077, 3008–3014].
- **CPU model** (2/4/5/7/8, all report type 3) = MFbus non-volatile memory, set by `SET-CPU-MODEL`, read by `READ-CPU-MODEL`; also baked in control-store word 7 of the loaded microcode image [V, prior carve]. Module type/model otherwise hardwired on the PCB (RMT register).
- **CPU serial number: not present in any doc.** Not read by guest software.

### 2.3 DOMINO / NUCLEUS — the I/O story [V], and it changes the assumption
- **DOMINO** = MC68020-based I/O controllers ("DIOCs") in the MFbus crate, "able to support the IO-needs for the ND-5000 CPUs" [SW-Guide 685]. Only on ND-5000 with MF-Bus memory.
- **Each DIOC is its own octobus station** — live `LIST-CONFIGURATION`: SCSI controller @ station 13B (module 21B), Ethernet III @ 12B (module 22B); DOMINO SCSI default station 10B [SW-Guide 1582–1590; SCSI-OG 443].
- **I/O data path bypasses the ND-100:** DIOCs do **their own DMA into shared MFbus/MPM memory** and coordinate with the ND-5000 via **octobus short messages + an MPM mailbox** [SW-Guide 691–695, 825–827]. The ND-100 only runs **PROMAN/BOPCOM** (boot + config + event gateway) [SW-Guide 915–929]. Classic SINTRAN device paths (e.g. `MAGTP`) are explicitly **not** used toward DOMINO SCSI [SCSI-OG 339].
- **DIOCs run their own local monitor** (68020 `TRAP #2`, D0=function, A0=param record) [SW-Guide 5957–5959] — a DOMINOS kernel, separate from SINTRAN MON.
- **NUCLEUS** = fast intra-computer message-passing library over shared memory + octobus [SW-Guide 837–851]. On **ND-5000**, four hot-path calls (`nkMove`, `nkSend`, `nkReceive`, `nkGetInfo`) are **microcoded in the 5000**; all other NUCLEUS calls run in the ND-100 [SW-Guide 6190–6194].

### 2.5 Full octobus controller landscape — what the 5000 actually needs
Station map from `ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE.md` [DOC]; device identities from the DOMINO docs [V].

| Station | Device | Emulated? | Required for CPU+code to RUN? |
|---|---|---|---|
| 1 | ND-100 (octobus MASTER/clock) | Yes | Yes (present) |
| 2B–6B | MF-controller (MFbus crate config + memory) | No — MFPREPARE unanswered | Maybe [?] — infrastructure, not a peripheral |
| 10B–13B | SCSI (DOMINO DIOC, module 21B) | No | No — disk/tape I/O only |
| 12B | Ethernet III (DOMINO DIOC, module 22B) | No | No — network only |
| 14B–15B | Matra VME | No | No |
| 16B–17B | Multifunction comm | No | No |
| 20B | Hyperchannel | No | No |
| 21B–23B | FDDI | No | No |
| 24B–27B | FPS-5000 array processor | No | No — compute peripheral |
| 30B–33B | Graphic | No | No |
| 70B–76B | ND-5000 CPUs (70–73 used) | Station yes; **production wiring no (G1)** | — |

**Key facts:**
- **The CPU + its macrocode run with only ND-100 + 5000 station + shared MPM.** MON I/O flows to the ND-100/SINTRAN. **No I/O controller is required just to make the CPU and code work** — this already works in tests.
- **Almost every peripheral is the same substrate:** a DOMINO DIOC (MC68020 + octobus station + MPM mailbox + DMA) differing only by a "module type" personality byte (21B=SCSI, 22B=ETH3, …) [V, SW-Guide `LIST-CONFIGURATION` 1582–1590]. Build the DIOC substrate **once**; SCSI/Ethernet/FDDI/etc. are personalities on it. → **all fold into the single DOMINO handoff.**
- **Two architectural exceptions:** MF-controller (infrastructure) and FPS-5000 (array processor, out of scope).
- **Open [?]:** whether SINTRAN's full bring-up (`OCSTART`/`CH5CPUPRESENT`) stalls without an MF-controller answering on 2B–6B. Tests bring a 5000 up without one, so the mailbox/MON path does not need it; a complete real SINTRAN boot might. Carve before the Phase-2 multi-CPU boot harness.

### 2.4 MON servicing — baseline holds [V]
ND-5000 MON calls are serviced by the ND-100/SINTRAN (`MON (100)` in the NUCLEUS stack figure) [SW-Guide 6142–6156] — exactly the M1 architecture already implemented. Only the 4 NUCLEUS primitives above are 5000-local.

---

## 3. The plan — phased

Each phase is independently valuable and independently shippable. Phases 0–2 are the "multiple configurable CPUs on the octobus" goal; Phase 4 is the "controllers doing I/O" goal; Phase 3/5 are depth.

### Phase 0 — Production wiring of ONE ND-5000 CPU (foundation, unblocks all)
**Why first:** everything else is untestable end-to-end until a real machine can bring up a 5000 CPU. Today only `AttachRealCpu` in tests does this.
- Add machine-level construction: an `ND100Machine` (or the ND5000-TEST config) instantiates `NDBusOctobus`, a real `CpuND500(Samson5800)`, calls `AttachCpu(cpu, station=70B)`, wires `MailboxDoorbell→WakeRunThread` + `OnRunThreadPark→DrainDoorbells`, starts the run thread parked.
- Add an `ND5000` machine-init/debugger command mirroring `InitializeND500`.
- **Deterministic boot harness** (reuse the committed TPE/RTC deterministic pattern): boot SINTRAN on the ND-100, bring up one 5000 CPU, load a hand-assembled program via 14B, `3START`, run to a MON call, assert the GIVEINT answer round-trips. This is the acceptance gate.
- **Risk:** `ND100Machine.cs` ownership/coordination with other sessions (previously flagged as the blocker). Mitigate with a new partial/config path, minimal edits to the shared file.

### Phase 1 — Per-CPU configuration surface
- Define an `Nd5000CpuConfig` value type: `{ CpuNumber(1–4), OctobusStation(default 70B+CpuNumber−1), CpuModel(2/4/5/7/8), OctobusSpeed, PowerFailStation, Serial(optional/cosmetic) }`.
- Wire it into `CpuTypeAndModel` / `SystemParameters` / the 3RMICV reply / `ConfigureMailbox(cpuNumber)`.
- **Model source decision:** default `CpuModel` from the loaded microcode image (control-store word 7) but allow explicit config override. Recommend explicit config wins, image is the fallback.
- Plumb through `CreateAndRegisterDevice("ND5000", param)` + INI. Serial is accepted and echoed in `LIST-CONFIGURATION`-style output only (no guest reads it) — documented as cosmetic.

### Phase 2 — Multiple ND-5000 CPUs concurrently (70B–73B)
- Register up to 4 stations, each its own `CpuND500` + servicer on its own host thread (the §3.2 architecture, now exercised).
- Verify: fabric routing per station, GIVEINT targets the correct station, no cross-talk, **X5SEM contention** across CPUs sharing one MPM backing array (Interlocked CAS path already designed), ring wrap/full under real concurrency.
- Drive SINTRAN's `CH5CPUPRESENT`/`OCSTART` bring-up ladder for N CPUs; multi-CPU boot harness as the gate.

### Phase 3 — Depth: remaining microcode→macrocode gaps
Incremental, workload-driven. Close `[?]` items: per-MON worker coverage beyond 13B, SRF cells 0o2004/0o2005/0o2011, and — **if NUCLEUS is in scope** — the 4 microcoded 5000-local calls (`nkMove`/`nkSend`/`nkReceive`/`nkGetInfo`) as C# servicer analogs. Only decode what a real target workload exercises.

### Phase 4 — Octobus I/O controllers (the large, new subsystem)
This is a fork. Two sub-targets and two strategies.

**Sub-targets:**
- **4a. MF-controller (stations 2B–6B):** answer `MFPREPARE`/error-record envelopes. Smaller; unblocks the SINTRAN startup ladder's MF path.
- **4b. DOMINO DIOCs (SCSI @10B/13B, Ethernet @12B, …):** the real device I/O.

**Strategy choice for 4b (the key architectural decision):**
- **(i) Behavioral model [RECOMMENDED]:** a C# object that speaks the octobus-message + MPM-mailbox + DMA-into-shared-memory protocol and backs onto a host file/disk image — **no 68020 execution.** This is exactly how the project already models the SAMSON microcode (servicer) rather than executing it. Much less effort; no firmware dump needed; testable against the mailbox/DMA contract.
- **(ii) Full DIOC emulation:** a real MC68020 core + DOMINOS kernel + firmware EPROM image. Faithful but very large, and **requires DIOC firmware dumps we do not have** [?]. Not recommended unless a dump appears and byte-fidelity is required.

Phase 4 is the biggest and should be scoped/greenlit on its own once Phases 0–2 land, since it needs its own protocol reverse-engineering pass (the DIOC↔5000 mailbox/DMA contract is only partially covered by the new docs).

### Phase 5 — Live validation & hardening
Reuse the deterministic RTC + boot-harness pattern for reproducible multi-CPU boots; adversarial concurrency review (X5SEM, ring, doorbell ordering across 4 CPUs); regression gate on the full ND-500 + octobus suites.

---

## 4. Recommended sequencing & the one decision that matters now

Phases 0→1→2 are a clean, low-risk spine that delivers "multiple configurable ND-5000 CPUs answering on the octobus" — and almost all the hard reverse-engineering for it is **already done**. Phase 4 (DOMINO I/O) is a separate, much larger effort that deserves its own reverse-engineering pass and its own greenlight.

**Decision needed:** how far to scope this first pass (see the interview question accompanying this plan).

## 5. Honesty notes
- The 70B–73B CPU-id→station binding is **SINTRAN's convention** (`FN5DEST`/`ASTAT`+index) [V], not the hardware's dynamic-assignment rule [V] — we pre-assign to match the guest, and skip the MFB configurator. Marked as a deliberate modeling choice.
- DOMINO DIOC↔5000 I/O protocol details (exact mailbox layout, DMA handshake, command set) are **only partially** in the new docs; Phase 4 needs a dedicated carve before implementation. Do **not** assume the ND-100↔ND-500 mailbox layout transfers unchanged.
- No serial number exists in guest-visible state; any serial field is cosmetic.
- All gap claims (G1–G5) are verified by absence in the current tree, not assumed.
