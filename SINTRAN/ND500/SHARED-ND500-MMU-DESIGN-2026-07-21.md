# Shared ND-500 MMU design - generic `IAddressTranslator` for both CPUs (2026-07-21)

**Goal (Ronny's direction):** one **generic MMU** in the **Nuget namespace**, plugged into BOTH the
functional macrocode CPU (`CpuND500`, migrating to Nuget) and the microcode CPU (`CpuND5000`,
already in Nuget), following the existing Nuget machine-bus MMU-mapping pattern. First of several
shared classes; the octobus interfaces / structures / bus follow later.

Evidence grades: **[V]** verified in code this session, **[D]** derived from the pattern, **[OPEN]** decision needed.

---

## 0. Why now - the diagnostic that motivates it [V]

Running the REAL swapper (`SWAPPER-K01.PSEG/DSEG`) through the REAL B30 microcode via a MICFU-23
3START (`SwapperStartDiagnosticTests`, green): the microcode dispatched the start, loaded the PCB,
read the DSEG build-tag correctly (`0x08012818 = 'REV.'`), executed the swapper's FIRST instruction
(`init`), and then stopped:

```
Stop: InvalidOperationException: Data type 7 memory access not implemented yet
```

**CORRECTION (2026-07-22, Ronny): data type 7 was NOT the MMU.** It was a missing memory-op
data-type case in the microword `CpuND5000` itself (microcode session's lane), root-caused and
FIXED there. The "= virtual write = the only gap is the MMU" reading here was a wrong inference from
the error string - do not build on it. The shared MMU is still the eventual path for real virtual
translation, but it did not cause the data-type-7 stop. Re-run the diagnostic for the real next stop.
This does NOT change the design below (a generic shared MMU is Ronny's directive regardless); it only
corrects the motivation.

---

## 1. The pattern to follow - `IAddressTranslator` [V]

`HackerCorpLabs.Emulation.Abstractions.Cpu.IAddressTranslator` is the canonical Nuget MMU seam
(skill `retrocore-cpu-mmu`). Contract highlights:

- `bool Translate(uint va, AccessKind kind, int sideChannel, out uint pa, out int space, out TranslationFault fault)`
  - **hot path, NEVER throws**, sealed impl, `AggressiveInlining`. Fault via `out`, CPU raises the trap.
- `bool TranslateAtContext(..., int contextOverride, ...)` - cold/DAP path with explicit context.
- `bool TestAddress(...)` - PTEST/dry-run, no ref/mod mutation.
- `void Flush(FlushScope, int context, uint va)`, `void Reset()`.
- `AccessKind` = Read / Write / **InstructionFetch**; `TranslationFault` = None / PageFault /
  ProtectionError / WriteToReadOnly / SupervisorViolation / BusError.

Plug-in model: the CPU holds `CpuState.Translator` (`IAddressTranslator?`, null = identity) and
`CpuState.PhysicalBus` (`SystemBus?`). **Page-table walks read via `PhysicalBus.ReadPhysical*`, never
the virtual bus** (else infinite recursion).

**ND-100 is the precedent** for a custom-paging ND CPU: split instruction/data tables, per-context
(PIL) via `TranslateAtContext`. ND-500 is the same shape with **context = domain (CED)** and
**I-space/D-space** distinguished by `AccessKind.InstructionFetch` (program capability) vs data
(data capability).

---

## 2. `Nd500Mmu : IAddressTranslator` - the generic unit [D]

The walk already exists twice and agrees (both from the ND-500 3-level model):
- **`CpuND5000.MmsUnit`** [V present] - pure `Translate(IMicroMemory, va, domain, isWrite, isInstruction) -> MmsResult`;
  capability -> PST -> PTE; PFN==0 = not-present; PSTP/PUWP latches. `Enabled=false`, NOT wired into fetch,
  validated vs spec + nd500x only.
- **`CpuND500` MMU** (`CpuND500.MMU.cs`) [V present] - `TranslateVirtualAddress(va, isWrite, isInstruction)`
  + `TryMapVirtualToPhysical`; the PROVEN one (runs the swapper on the 3022 track); its own code already
  cross-references `MmsUnit.Translate`.

`Nd500Mmu` = ONE sealed `IAddressTranslator` that supersedes both, carrying the validated walk:

| IAddressTranslator | ND-500 meaning |
|---|---|
| `AccessKind.InstructionFetch` | use program-capability array (`pcb_pc[32]`) |
| `AccessKind.Read/Write` | use data-capability array (`pcb_dc[32]`); Write checks `DC_WRP` |
| `sideChannel` | unused (0) - ND-500 has no FC/ASI |
| `contextOverride` / current context | **domain (CED)** - selects the PCB (`PcbBase + domain*256`) |
| `out space` | I-space vs D-space (and/or MPM-vs-local routing, bit 31) |
| `TranslationFault.PageFault` | PGF (octal 46) - the diagnostic's data-type-7 target |
| `TranslationFault.WriteToReadOnly/ProtectionError` | PV (octal 44) |

Geometry (carve ground truth, already in `MmsUnit`): page = 2 KB; VA = Seg(5) L1(7) L2(9) Off(11);
PSTE 4 B (bits1-0 mode AZI/ASI/ADI, 31-2 PFN); PTE bit0 = protection, no present bit (PFN==0 =
not-present); PCB = 256 B/domain, `pcb_pc` @0, `pcb_dc` @64; roots `MM,PSTP=2` / `MM,PUWP=3` [OPEN
byte-base mapping].

---

## 3. THE one real decision - the memory seam [OPEN]

The pattern says walk via `PhysicalBus.ReadPhysical*` (a concrete `SystemBus`). But the two CPUs
reach memory differently TODAY:
- `CpuND5000` uses `IMicroMemory` (flat, big-endian) - no `SystemBus`.
- `CpuND500` uses `ISystemBus` + `RouteToMpm` (MPM vs local).

Two ways to give `Nd500Mmu` its page-walk reads:

**Option A - abstract the walk seam.** `Nd500Mmu` walks via a tiny `IPhysicalMemory` (Read/Write
big-endian by physical byte addr). Provide two thin adapters: one over `SystemBus.ReadPhysical*`
(CpuND500), one over `IMicroMemory` (CpuND5000). Smallest change to the microcode CPU; slight
divergence from the skill's "walk via PhysicalBus" letter.

**Option B - both CPUs adopt `SystemBus`/`PhysicalBus`.** Fully matches the pattern and the
`CpuState.Translator`/`PhysicalBus` plug-in. Bigger lift: the microcode CPU would move from
`IMicroMemory` to a `SystemBus` seam. Natural to do WHEN `CpuND5000` + `CpuND500` consolidate in
Nuget, but not a small step.

Recommendation: **A first** (unblocks both CPUs behind the shared `IAddressTranslator` now, proven by
a differential test), **converge to B** during the Nuget consolidation. The public seam is
`IAddressTranslator` either way, so the Option-A adapter is an implementation detail, not a contract.

---

## 3b. Reconciliation findings from the PROVEN walk - decisions needed [V/OPEN]

Reading `CpuND500.MMU.cs` (the proven walk) vs `MmsUnit` surfaced real ND-500-specific points the
generic 68K/SPARC-shaped `IAddressTranslator` does not cover. These are build-time decisions, not
guesses to bury:

1. **Walk source diverges [V].** `CpuND500` keeps **PCB and PST in HOST C# arrays**
   (`PCBTable[domain]`, `PST[psn]`) and reads only PTEs from memory. `MmsUnit` reads **all three
   from guest memory** (microcode-accurate). The pattern (walk via `PhysicalBus.ReadPhysical*`) and
   the Option-A `IPhysicalMemory` seam both imply **guest-memory** for all three. Decision: `Nd500Mmu`
   reads PCB/PST/PTE from `IPhysicalMemory` (guest); the differential test builds equivalent tables in
   BOTH forms to prove algorithm parity vs `CpuND500`. Wiring `CpuND500` to actually source PCB/PST
   from guest memory is the convergence step (SINTRAN's SPLAC/ENDPL build them there on real HW).
2. **Indirect segments have no `TranslationFault` value [OPEN].** A PROGRAM capability with `PC_TYP`
   (bit 15) set = inter-domain CALL segment. `MmsUnit` added its own `MmsFaultKind.IndirectSegment`;
   the shared enum (None/PageFault/ProtectionError/WriteToReadOnly/SupervisorViolation/BusError) can't
   express it. Decision needed: **extend the shared enum** (touches `Abstractions`, affects all CPUs)
   vs **surface indirect via a separate translator output**. `CpuND500` today falls back to identity
   for indirect (a stub).
3. **Address-Zero (AZ) is an *ignorable* trap [V].** A null data read (VA 0, no capability) is AZ
   (bit 24), which real ND software relies on (continues, reads 0). `CpuND500` handles it by raising
   an ignorable trap and returning physical 0. In the translator model this is "success, pa=0", not a
   fault. Decision: `Nd500Mmu` returns success/pa=0 for the AZ case and lets the CPU's trap policy
   decide (keeps the translator pure).
4. **Privileged / user access (`DC_PAC`) [V].** `CpuND500` checks `DC_PAC` + PCB privilege ->
   `SupervisorViolation`. `MmsUnit` omits this. Keep it (maps cleanly to `SupervisorViolation`).

None of these block building+validating `Nd500Mmu` standalone (differential test hand-builds tables);
they DO shape its output contract, so they are settled here before coding.

## 4. Where it lives + lane split [D]

- Interface: already in `HackerCorpLabs.Emulation.Abstractions.Cpu` - no new interface.
- `Nd500Mmu` (sealed): a shared ND-500 Nuget package (e.g. `HackerCorpLabs.Emulation.CPU.ND500.Common`,
  or the ND-500 CPU Nuget once `CpuND500` migrates). Shared by both CPUs.
- `CpuND500` (my lane, Emulated.HW): adopt `CpuState.Translator` + `PhysicalBus`; route its memory
  ops through `Nd500Mmu`; delete/retire the bespoke `TranslateVirtualAddress` body once parity holds.
- `CpuND5000` + `MmsUnit` (microcode session's lane): wire `Nd500Mmu` into the fetch/data path and
  implement the virtual MEMORY ops (data-type 7/15) that currently throw. **I must not edit their
  files** - coordinate; supply the shared unit + the differential oracle.

Validation (mandatory per skill): the standard translator fixtures (identity, page hit/miss,
protection, ref/mod, boundary, flush, TestAddress-no-mutate, context-switch, per-space) + a
**differential test** asserting `Nd500Mmu` == the proven `CpuND500` MMU == `MmsUnit` on a table
corpus + a translator perf gate (<100 ns/translate).

---

## 5. Migration sequence [D]

1. Build `Nd500Mmu : IAddressTranslator` (Option-A seam) from the validated `MmsUnit`/`CpuND500` walk.
2. Differential test: `Nd500Mmu` vs `CpuND500` MMU (proven) vs `MmsUnit` - lock parity.
3. `CpuND500` adopts `Translator`+`PhysicalBus`; route memory ops through it (my lane).
4. Microcode session wires `Nd500Mmu` into `CpuND5000` fetch + virtual MEMORY ops (their lane).
5. Re-run `SwapperStartDiagnosticTests` - the swapper should now pass its first virtual write and run on.
6. Later: consolidate both CPUs + octobus (interfaces/structures/bus) into shared Nuget (Option-B seam).

---

## Status of record
- **BUILT (increment 1, green):** shared Nuget package
  `RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND500.Common\` (added to `RetroCore.sln`):
  `src\IPhysicalMemory.cs` (Option-A walk seam), `src\Nd500Mmu.cs` (`Nd500Mmu : IAddressTranslator`,
  the AZI/ASI/ADI walk), `tests\Nd500MmuTests.cs` (12/12 mandatory fixtures pass). Decisions from
  sec 3b baked in; indirect + DC_PAC deferred.
- **NEXT increments:** (2) differential test `Nd500Mmu` vs the proven `CpuND500` MMU vs `MmsUnit`;
  (3) `CpuND500` adapter (`IPhysicalMemory` over `SystemBus`) + route memory ops through the shared MMU
  (my lane); (4) microcode session wires `Nd500Mmu` into `CpuND5000` fetch + the virtual MEMORY ops
  (data-type 7/15) that currently throw; (5) re-run `SwapperStartDiagnosticTests` past the first write.
- Diagnostic test: `RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND5000\tests\SwapperStartDiagnosticTests.cs` (green, observational).
- Pattern: skill `retrocore-cpu-mmu`; interface `Abstractions\src\Cpu\IAddressTranslator.cs`.
- Existing walks: `CpuND5000\src\MmsUnit.cs`; `Emulated.HW\ND\CPU\ND500\CpuND500.MMU.cs`.
- Cross-link: swapper start = message-driven MICFU-23 (this session's agent traces); see
  `SWAPPER-START-MECHANISM-CARVE-2026-07-19.md` (retraction: swapper is macrocode), and the octobus
  bring-up (LCS0/STAMIC0 = real B30 microcode, version 0x2E9A).
