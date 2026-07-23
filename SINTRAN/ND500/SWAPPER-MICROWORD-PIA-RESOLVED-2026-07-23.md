# Swapper microword — PIA blocker RESOLVED (octobus/context lane → microcode track)

**Date:** 2026-07-23
**Author:** octobus/context lane (mailbox transport) LLM session
**Answers:** `SWAPPER-MICROWORD-PROGRESS-HANDOFF-2026-07-22.md` ("Action for octobus/context lane")
**Status of record:** memory `nd5000-init-datatype-swapper-blocker.md`

---

## TL;DR

The 34-instruction stop (PIA clear) is fixed. The swapper now advances **34 → 46 macro
instructions** (P `0x08000162` → `0x080001A0`). Full ND-5000 suite **191/191 green**.

**BUT the handoff's proposed fix was wrong, and this matters for the real octobus/SINTRAN
path:** PIA is **NOT** taken from the PCB macrostatus field. Seeding the PCB macrostatus has
**zero effect**. The real source is a **domain-information-table byte** that CNTXTLOAD
re-reads and re-derives PIA from, overriding whatever the macrostatus word carried.

The new 46-instruction stop is a genuine **microword-CPU-lane** gap — handed back below.

---

## What the empirical probe found (not assumed — measured)

Added `Probe_PcbMacrostatusOffset_ForPia` to `SwapperStartDiagnosticTests.cs` (self-contained,
observational). It single-steps the real B30 microcode and measures, rather than reasons about,
the PIA path:

1. **PCB macrostatus offset = PCB+0x40.** All-ones sweep: setting PCB+0x40 = `0xFFFFFFFF` makes
   `MIC,STS` = `0xFFFFFFFD` after load. So the macrostatus word IS loaded (WRITEST1 @ `0o15033`,
   full 32 bits including bit 1).

2. **…then bit 1 is immediately overwritten.** Single-step `MIC,STS` trace:
   - `Mpc=0o15033` : `0x00000000 → 0xFFFFFFFF` (WRITEST1 loads the PCB macrostatus)
   - `Mpc=0o15102` : `0xFFFFFFFF → 0xFFFFFFFD` (bit 1 cleared)

3. **The override source is a domain-table byte.** Microwords `0o15074–0o15102` do `RD,PHYS`
   reads (AA=2 = DPA) and at `0o15100–0o15101` conditionally `ANDCB` (clear) / `OR` (set)
   `MIC,STS` bit 1 (`A,BM01`) from a read byte's bit 0. The read that decides it lands on
   **physical `0xC8`** (= DPA `0x80` + `0x48` in this harness). Harness has no domain table →
   the byte is 0 → PIA forced clear.

4. **The fix, behaviourally confirmed:** seed byte `0xC8 |= 0x01`. `MIC,STS` becomes `0x02`
   (PIA) at the first macro instruction and the swapper advances **34 → 46** macro instrs.
   Seeding the PCB macrostatus at PCB+0x40 (any bit, or all-ones) never advances it.

This corrects the handoff: *"CNTXTLOAD reloads the whole macrostatus from the PCB, so the fix
belongs in the PCB"* — true for the macrostatus register load, but PIA specifically is
re-derived from the domain information table one block later (`0o15074–0o15102`), so the PCB
macrostatus is the wrong place.

## The fix that landed (working tree)

`Nuget/HackerCorpLabs.Emulation.CPU.ND5000/tests/SwapperStartDiagnosticTests.cs`:
- `RealSwapper_Start23B_Diagnostic` now seeds the domain-info PIA byte (physical `0xC8`, bit 0)
  right after the PCB setup, with a full comment explaining the mechanism and citing the probe.
- New `Probe_PcbMacrostatusOffset_ForPia` test carries the evidence (PCB read log, macrostatus
  offset sweep, `MIC,STS` change trace, PIA-decision read trace, domain-byte seed confirmation).

Proper long-term fix: a real domain information table (deferred; belongs with the `RD,ADOM`
domain-select work — the NUCLEUS cross-process path).

## Note for the REAL octobus/SINTRAN boot path

Since PIA comes from the domain information table (per ND-05.020.01 §4.3), the real octobus run
will only clear this hang if SINTRAN's domain information table for the swapper's domain has the
PIA bit set AND the microcode's `RD,PHYS` at `0o15076` resolves to that table entry. That is a
context/domain-setup question, not a PCB-macrostatus question — worth verifying against a real
SINTRAN boot when the domain map is wired.

## Handoff back to the microword-CPU (CpuND5000) lane

New stop at 46 instrs is a real microword gap (NOT octobus):

- **`A,SPEC,DACR not implemented yet`**
- P (Npc) = `0x08000199`, opcode `0xB8` (`0o270` = ENTS)
- `Regs.InstrDt = 0`, `OcaKind = 3`, `PrevOcaKind = 2`
- throwing microword `Mpc = 0o4257`

Over to the CpuND5000 session.
