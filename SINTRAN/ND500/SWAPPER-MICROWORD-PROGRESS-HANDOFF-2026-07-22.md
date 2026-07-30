# Swapper microword bring-up — progress + handoff (CpuND5000 LLM → octobus/microcode team)

**Date:** 2026-07-22
**Author:** CpuND5000 (microword ND-5000 CPU) LLM session
**Companion doc:** `SWAPPER-MICROWORD-DIAGNOSTIC-HANDOFF.md` (the self-service loop mechanics)
**Status of record for the microword track:** memory `nd5000-init-datatype-swapper-blocker.md`

---

## TL;DR

Self-drove the `SwapperStartDiagnosticTests` loop (run → read throw dump → add the missing
microword case → re-run). **Swapper advanced 17 → 34 macro instructions** (P 0x08000004 →
0x08000162). Fixed **6 microword gaps**, all in `CpuND5000.cs` / `OperandRouter.cs` / the
dispatch map, full suite **189/189 green** after each.

The 34-instruction stop is **NOT a CpuND5000 bug** — it is a privileged-instruction trap the
microcode raises **correctly** because the harness's fake PCB doesn't set **PIA**. See "Current
blocker" below. This one is the octobus/context lane.

---

## Fixes landed this session (microword lane — all committed to the working tree on disk)

Files: `Nuget/HackerCorpLabs.Emulation.CPU.ND5000/src/{CpuND5000.cs, OperandRouter.cs, Registers.cs,
DispatchEntry.cs, Generated/DispatchMapB30.g.cs}`, `docs/dispatch-map-b30.json`,
`tools/ND5000FieldGen/Program.cs`, and the two test files.

| # | Macro stop | Root cause | Fix |
|---|---|---|---|
| 1 | `A,SPEC,LA not implemented` (RET_SU 0o4420) | LA-latch (A_OP 111) unmodeled | Added `Registers.La`; `LOADLA` primes it from P; `A,SPEC,LA` reads, `D,SPEC,LA` (dest 44) writes |
| 2 | `Operand specifier 0xF8/desync` → phantom "MON 0 LEAVE" | **CALL (0xC3) has a bare 4-byte direct operand[0]** (call target), same class as INIT/ENT; was consuming 3 bytes not 6 → phantom execution | Allow-listed CALL (`directMask=1, sizes=[4]`) |
| 3 | `Operand specifier 0xF5 not implemented` (CALL/other) | Pre-indexed addressing mode (0xF4–0xFF) unimplemented | Implemented pre-indexed in `EnsureOcaDecoded`: `ea = I[n] + disp`; **ND-500 I1–I4 = microword X1–X4 = Wrf[0-3]**; bits1-0 = reg, bits3-2 = disp width; no scaling (mirrors functional CpuND500) |
| 4 | `Opcode 376 (0xFE) has no dispatch entry` | `FetchAndDispatch` only treated `0xFF` as a 2-byte prefix | Widened to `b0 >= 0xFC` — **all of 0xFC/FD/FE/FF are 2-byte prefixes** (CLRK = 0xFE03; 170/216/229/225 extended opcodes). 0xF4–0xFB in that range are only ever operand specifiers, never opcodes |
| 5 | `D,MIC,RESTU not implemented` (RETD 0o4416) | dest 136 = "CLEAR STACK UNDERFLOW" | No-op (no STU status modeled; documented) |
| 6 | `Data type 7 memory access` (ENTS 0o270) | ENTS is a type-less stack op → `InstrDt=-1` → TYP,DR resolves to 7 | `dataType:0` override in dispatch map (same as INIT) |

Also added **spin-detection + a SPIN DUMP** to `SwapperStartDiagnosticTests` (breaks on the first
self-loop repeat so the ring shows the ENTRY path into a hang, not 16× the loop word).

---

## Current blocker (34 instrs) — PIA, and it is NOT the microword CPU's fault

**Symptom:** at P=0x08000162 the swapper executes `dctsb` (0xFF1D, a **privileged** data-cache
instruction). Its microcode `DCTSB_1 @ 0o12342` does `AND MIC,STS, BM01, COND,MZRO → ILLEG`. The
check fails, so it goes ILLEG (`0o200`) → `ILLEG_01` (`0o12530+`) → **DUMMY (`0o103`, a self-loop
"hang" state)**. The diagnostic now prints this as a SPIN DUMP.

**Researched root cause (authoritative):**
- `BM01` = `1 << 1` = **bit 1** (not bit 0 — earlier misread). `MIC,STS` = macrostatus register
  (dest `84H`).
- **ND-05.020.01 (ND-5000 Hardware Description) Table 30**: macrostatus **bit 1 = PIA "Privileged
  Instruction Allowed"**.
- **ND-05.020.01 §4.3, p.73**: *"PIA … is a copy of the PIA-bit in the **domain information table**
  of the currently executing domain … checked by microcode every time a privileged instruction is
  executed."*

**Conclusion:** `dctsb` is behaving **correctly** — a privileged instruction traps to ILLEG when
PIA is clear. The hang is a **diagnostic-harness artifact**: the harness posts a minimal fake PCB
(only P set) so the loaded macrostatus is 0 → PIA clear. SINTRAN's **real** swapper PCB runs in a
privileged domain with PIA set, so a real octobus boot very likely won't hit this.

**Empirically confirmed the load path:** seeding `cpu.Regs.MicSts |= 0x02` directly still hung —
because **`CNTXTLOAD` (`0o14742+`) reloads the whole macrostatus from the PCB** (`WRITEST1`/
`WRITEST2` @ `0o15001-2`), overwriting the register seed. So the fix belongs in the **PCB**, not a
register.

---

## Action for octobus/context lane

1. **Seed PIA (macrostatus bit 1) in the swapper's PCB macrostatus field** in
   `SwapperStartDiagnosticTests` (mimicking SINTRAN's privileged PCB), OR verify that the real
   octobus/SINTRAN boot path already sets it (in which case the diagnostic just needs to match).
2. The exact PCB macrostatus **offset** can be pinned from `CNTXTLOAD`'s ORCON-stepped `RD,POF`
   reads (`0o14751`+, AA=7 = EA3 = PCB base) feeding `WRITEST1`/`WRITEST2`. The CpuND5000 LLM can
   pin it on request.
3. After PIA is set, re-run the diagnostic — `dctsb` should retire and the swapper continue past
   0x08000162.

---

## Notes / open items carried forward

- **INIT/ENT do not set macro register B** (silent, no throw): there are ZERO dest-form `D,DAC,B`
  writes in the whole B30 image; B is a DAC side-effect (or comes from context). Not blocking. See
  memory.
- The `0o326` ORCON listing bug (`MICRO-5800-B30.md` renders `21`, raw is `0o41`) still stands —
  trust `new Microword(Cs.Hi[a],Cs.Lo[a])`.
- Data type 6 (TYP,DD 128-bit) still absent from `AccessWidth` (off critical path so far).
- The domain/permit memory ops (`RD,ADOM`/`WR,ADOM`) are collapsed to flat read/write — the NUCLEUS
  cross-process path is where a real domain select will eventually be needed.
