# NDBusEthernetII.cs — Interrupt & Timer Wiring Validation vs. Schematics

**Cross-check of `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs`
(state of 2026-07-23, 4027 lines) against the print-G schematics
(`../EthIIImages/`, netlist in `EthII-interrupt-clock-netlist.md`) and the technical
manual ND-12.055.1 EN (original PDF).**

Verdicts: **CONFIRMED** = code matches hardware. **MISMATCH** = code differs from
hardware (impact noted). **GAP** = hardware behavior with no code path.
**UNVERIFIED** = could not be settled from schematic+manual (PAL equations etc.).
Line numbers refer to the file as read on 2026-07-23. No code was changed.

---

## A. CONFIRMED — correctly wired

| # | Item | Code | Evidence |
|---|---|---|---|
| A1 | 68k IPL map: 2=LANCE, 3=MFP vectored, 4=console, 5=MERR, 6=OPCOM, 7=power NMI | `Cpu_OnInterruptAck` dispatch (669-699) | LS148@19F wiring = manual p.12 |
| A2 | MFP on level 3, vector from MFP (`HandleMFPInterruptAck`, 706), spurious when none pending | 706-730 | IAK901 from PAL 453-00; MK68901 protocol |
| A3 | MFP vector set 117/116/114/113/112/111/107/105₈ with VR base 0x40; 107=GPIP5 LANERROR, 116=GPIP6 NCINT, 117=GPIP7 WRIV | comments + `TriggerMFPVector` (839-871) | GPIP wiring S3 = manual p.27-28, standard 68901 channel map |
| A4 | Level-6 IACK clears the OPCOM interrupt (823) | `HandleOPCOMInterruptAck` | = hardware `CLROPCOM0` from PAL 453-00 |
| A5 | Level-5 IACK clears the level-5 interrupt (799) | `HandleParityErrorInterruptAck` | = hardware `CLRMERR0` (for the MERR part; see M1 for the STC part) |
| A6 | MFP INTR modeled as level (assert+deassert) → level 3 | `Mfp_OnIRQ` (2443-2457) | 901INTR is a level |
| A7 | ND-100 status word: bank<<8, bit5 halt, bit4 reset, bit2 = asserted INT12, bit0 = enable | `Read` 1160-1234 | manual p.30; **bit2 = RFT·RIE per schematic — and the code deliberately not exposing the pending latch in bit 2 (1176-1181) is hardware-CORRECT** |
| A8 | SCIP fired while enable off stays pending and asserts when enable is set | `MemoryMap_OnNDInterrupt` (877-903) + delivery on enable (1318-1332) | RFT FF clocks regardless of RIE; BINT12 = RFT·RIE |
| A9 | IDENT clears BOTH the INT12 latch and the interrupt enable | `IDENT` override (1439-1460) | **Schematic settles the open C-1 question: RFT and RIE flip-flops BOTH have R̄ = CLINT0 = (DCL OR PIDENT). The current behavior is hardware-correct; fix-plan C-1 ("clear only RFT") would be WRONG.** After IDENT, status bit 0 reads 0 until the next control write — also matches code |
| A10 | Control bits re-captured on every write (not edge-detected) for enable/halt/reset/power-low levels | 1293-1306 | RIE FF + LS175@86 are re-clocked on every OCW |
| A11 | Reset rising edge resets card state incl. pending SCIP; vectors fetched on falling edge | 1342-1367 | manual p.18/29 (Master Clear sets RESET+HALT; restart fetches SSP/PC from DRAM[0..7]) |
| A12 | ND-interrupt (bit 2) delivered to MFP GPIP I6 through the MFP's AER/IER edge logic, not a forced level-3 | 1369-1384 (A4 fix) | NCINT0 → I6; vector 116₈ |
| A13 | MFP timer clock 3.125 MHz | 2153 (`SetTimerClock(3_125_000)`, C-4 applied) | 3CLK = 12.5/4, sheet-1 note |
| A14 | Timer C = RTC → MFP level 3 vector 105₈, no SCIP side effect | `Mfp_OnTimerInterrupt` (2483-2488) | Timer C has no external pin; RTC ≈ 128.07 Hz |
| A15 | ETHSTAT (EF00B8-BF): bit0 = live LANCE INTR state, bit2 = transceiver power, active-low | 3729-3752 | S244@9E sheet 5; manual p.22 |
| A16 | LANRESET (EF00B0-B7) actually resets the LANCE (C-5 applied) | 3754+ | LS139@5D Y2, OR-ed with system reset |
| A17 | Power-low NMI wired to level 7 (C-5): control bit 6 → level 7 | 1389-1397 | LS148 in7 — but see M4 for the gating semantics |
| A18 | ND-100 side: 4 thumbwheel positions → 140360/64/70/74₈, ident 2240-2243₈, level 12 | ctor 487-530 | IDENT PROMs 089-00/01 + comparators; libnd.js |
| A19 | 68000 vectors fetched from DRAM start (no EPROM) | LoadRom/reset flow | manual p.20: EPROM sockets empty; p.18: SSP/PC from first 8 bytes of DRAM |

## B. MISMATCH — code differs from the hardware

### M1. The "STC / Timer Controller" does not exist  **[HIGH, known but not yet removed]**
Code: `stcArmed`/`STC_PERIOD_TICKS=2000`/`timerConfigReg` (3155-3177), STC tick in
`Clock` (3408-3437), config-write arming (3524-3534), `OnTimerInterrupt` → **level 5**
(2256-2266), timer branches in the level-5 and level-6 IACK handlers (788-792,
812-816), and the header comment "STC shares level 5 with MERR" (778-780).

Hardware: the F138@106 has **two enables (IOSPACEL/IOSPACEH), so EF01xx is a pure
mirror of EF00xx**. EF0140=MERRSTAT, EF0160=EAREN, EF0120=EPROMMODE, EF01A0=LANCE RDP.
A level-5 ISR reading EF0140/EF0160 is the **memory-parity-error handler** reading
error status + captured error address. There is no timer chip; level 5 = MERR only.
This confirms fix-plan items B-REV-1/2/3 (2026-07-08, still unchecked) with gate-level
evidence. TPE test 12 failing is the real-hardware outcome.
Also remove/rename: the "AM9519 EOI" interpretation — a 68k write to EF0100-011F lands
in "not used"/PROFF on real hardware and does **not** ring SCIP; only EF0080-9F /
EF0180-9F accesses clock the RFT FF.

### M2. NCINT (control bit 2) is a strobe-qualified pulse, not a stored level  **[MEDIUM]**
Code (1378): `MFP.GPIO_6 = !ND_interrupt` — I6 follows the bit between control writes;
a second write with bit2=1 (without an intervening bit2=0 write) produces no new edge.
Hardware: `NCINT0 = NAND(BND02, OCW-strobe)` — I6 pulses low **for every control write
with bit 2 = 1**, then returns high. Each such write is a fresh falling edge to the
MFP. Fix shape: on every control write, if bit2=1 pulse I6 low→high (or model the
strobe), instead of holding the level. Impact: back-to-back doorbells from the ND-100
driver are lost; also firmware polling GPDR between doorbells reads a stuck-active pin.

### M3. OPCOM re-trigger on repeated writes  **[MEDIUM]**
Code (1399-1405): level 6 asserted only on the 0→1 edge of bit 3 across writes
(`startOpcom && !previousStartOpcom`).
Hardware: the OPCOM FF is **clocked by every control write with bit3=1**; after a
level-6 IACK cleared it (CLROPCOM), the very next write with bit3=1 sets it again even
though the bit never went to 0 in between. Fix shape: trigger on every write with
bit3=1 (the FF + IACK-clear already handles dedup). Impact: a second OPCOM kick with
the bit held at 1 is silently dropped.

### M4. Control bit 6 is a power-low ENABLE, not a trigger  **[LOW-MEDIUM]**
Code (1393-1397): writing bit 6 = 1 immediately fires the level-7 NMI.
Hardware: bit 6 sets `PLOWE` (a level in the LS175); the NMI fires on
**`PLOWE AND Master-Clear`** (gate 5F → PAL 455-02). The manual's power-fail sequence
(p.18) is: ND-100 detects power fail → asserts MCL → card (with PLOWE set) takes the
NMI, saves registers, self-halts; SCIP/RFT+RIE are cleared 200 µs later by Delayed
Clear. Impact: a driver setting bit 6 as an enable during init would spuriously NMI
the 68000 in the emulator. If SINTRAN/TPE never set bit 6 outside an actual power-fail
path, current behavior is benign — worth a trace check before changing.

### M5. Level 7 is never deasserted  **[LOW]**
`HandlePowerFailureInterruptAck` (829-834) acks but never calls
`InterruptControllerSetInterrupt(7, false)` — after one NMI the level-7 request stays
asserted in the interrupt controller (depending on Cpu68K core semantics this can
suppress or re-deliver NMIs). Hardware: LEV7INT is a registered PAL output that
follows its terms (deasserts when PLOW drops). Fix: deassert on ack or on control
writes with bit6=0.

### M6. LANCE INTR modeled as edge + IACK-clear instead of level  **[MEDIUM]**
Code: `Lance_OnIRQ` (2459-2464) only asserts; `HandleLANCEInterruptAck` (755-765)
clears level 2 on IACK.
Hardware: LANINTR is a **level** — it stays asserted while any unmasked CSR0 cause is
set, and IACK does *not* clear it; only a firmware CSR0 write (clearing the cause or
INEA) or LANRESET releases it. Consequences in the emulator: (a) if a second LANCE
event is pending at IACK time, hardware re-interrupts immediately, the emulator stays
silent until the LANCE core happens to raise OnIRQ again; (b) ETHSTAT bit 0 (live pin
via `GetLANCEInterruptState`) can read "interrupt active" while the CPU-level request
was already cleared — the two views of the same pin diverge. Fix shape: drive level 2
from the LANCE core's INTR-state (assert+deassert callback), drop the IACK-clear.

### M7. Pending SCIP flushed by a control write with enable=0  **[MEDIUM — known, deliberate]**
Code (1333-1338): a control write with bit0=0 discards `scipPending`.
Hardware: RFT is cleared **only** by IDENT-answer or Delayed Clear; writing the control
word (any value of bit 0) does not touch it. A disable-then-enable sequence would, on
real hardware, re-assert INT12 from the still-latched RFT. The code comment documents
that the hardware-true latch regressed TPE tests 12/24/25/26 in the 2026-07-07 attempt
— but that attempt likely interacted with the STC block (M1) and the C-1 experiment
(now shown wrong by A9). Recommendation: after removing the STC (M1), retest the
hardware-true model as one holistic change: RFT persists across control writes; BINT12
and status bit 2 = RFT·RIE; IDENT clears both FFs (current A9 behavior).

## C. GAP — hardware paths with no emulator wiring

### G1. LANERROR (MFP I5, vector 107₈) never driven by DMA failures
Hardware: a LANCE memory cycle stopped by protect violation, bus error, or
address-out-of-range pulls `LANERROR0` → MFP I5 (manual p.27). In the code GPIO_5 is
initialized inactive (2160-2164) and only the manual test helper `TriggerMFPVector(107)`
ever drives it; `Lance_OnReadDMA/OnWriteDMA` out-of-bounds paths (2409-2439) return
`false` without raising it. Firmware error recovery for DMA faults can therefore never
be exercised.

### G2. WRIV (MFP I7, vector 117₈) never driven by protect violations
Hardware: a 68000 user-mode write into a protected 512-byte segment yields bus error +
`WRIV0` → MFP I7 (manual p.19). Only `TriggerMFPVector(117)` drives GPIO_7. If the
protect table is enforced in `NDEthernetMemory`, the violation path should also assert
I7.

### G3. Console interrupt (level 4) never asserted
`HandleConsoleInterruptAck` exists (770-776) but no code path ever raises level 4.
Hardware: RREDY/TREDY (USART ready pins) → level 4 autovector when the CONSPRES strap
is fitted. Impact: none for SINTRAN operation (PCT is a service console); needed only
if the test console is emulated interactively.

### G4. MFP not reset by ND-100 reset bit / Master Clear
Hardware: MK68901 RESET pin ← `PMCL0`, which is asserted by power-on MCL **and** by
control-word bit 4 (PRES chain) — an ND-100-commanded reset also hardware-resets the
MFP (and pulses LANCE reset). In the code, the reset rising edge calls
`memoryMap.Reset()` (1345); whether that resets the MFP register file was not verified
— the nearby comment (595-599) warns MFP wipes break firmware reinit for the RESET
*instruction* (which indeed must NOT reset peripherals on this card: the 68000's RESET
output pin is not wired to the MFP — only PMCL is). Verify `memoryMap.Reset()` resets
the MFP; the RESET-instruction no-op stays correct.

## D. UNVERIFIED / notes

- PAL 453-00 `CLRREDY/PRI/IAKE` and PAL 455-02 exact equations: inferred (no dumps).
- BERR bus-timeout (~1.3 µs shifter) is not modeled; the emulator raises bus errors
  immediately on unmapped access — acceptable simplification.
- 68000 clock: print-G schematic runs the MC68HC000-12 at **12.5 MHz** (manual p.11
  says 10 MHz — earlier print). `CPU68K_CYCLES_PER_ND100_TICK=1` (921) is a tuned,
  known-good ratio; if ever recalibrated, target 12.5 MHz, not 10.
- Manual-internal contradiction: p.27 "PCT interrupts on level 5 via the MFP" is wrong
  (p.12 table + LS148: console = level 4 direct; MERR = level 5).
- The OCR'd manual `Reference-Manuals\Devices\ND-12.055.1 EN Ethernet II Controller.md`
  contains at least one hallucinated section (an "optical properties of materials"
  block around lines 85-118). Use the original PDF for load-bearing facts.

## E. Suggested order of work (report only — nothing changed)

1. **M1** — delete the STC block (= fix-plan B-REV-1/2/3, now gate-level-proven);
   accept TPE test 12 failing as real-hardware behavior.
2. **M6** — LANCE INTR as level (assert/deassert from CSR0 state, no IACK-clear).
3. **M2 + M3** — strobe semantics for NCINT and OPCOM (re-trigger on every write with
   the bit set).
4. **M7** — retest hardware-true RFT latch semantics (persist across control writes)
   *after* 1-3, as one change, against TPE 1-12 and the SINTRAN ENNS0 boot.
5. **G1/G2** — wire LANERROR and WRIV into the DMA-fault and protect-violation paths.
6. **M4/M5** — power-low enable gating + level-7 deassert (verify with a SINTRAN trace
   whether bit 6 is ever set outside power-fail first).
