# ND Ethernet II emulator - RE session handoff (2026-07-08)

**Target file (all C# edits):**
`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs`

**Goal:** make the emulated ND Ethernet II controller (ND-110063, PCB 3094) work so
SINTRAN `START-NETWORK-SERVER ENNS0` completes and the COSMOS Ethernet server registers
with XROUT. Current live failure: `Error in communicating with XROUT ... Unknown name
(of server or system)`.

**User's standing directive (ABSOLUTE):** the bug is in the C# Ethernet controller, NOT in
XROUT/XMSG. Do NOT conclude "it's XROUT / the card is done". Keep hunting in the controller.

---

## Oracles (use BOTH on every change)
1. **Regression oracle:** TPE ETHERNET-TWO diagnostic (`TPE> run`, tests 1-33).
   Committed baseline: **tests 1-11 PASS, 12 Timeout/`no interrupt from MC68000`, 22 varies,
   24/25/26 FAIL, 33 DMA**. Do NOT regress 1-11 (esp. test 11 = OPCOM/ND-calling-int).
2. **Goal oracle:** `RT ENNS0` then `START-NETWORK-SERVER ENNS0` with debugger `DebugTrace 2 4`.
   **Delete `%LOCALAPPDATA%\trace\file-trace.txt` before each capture** (append-mode).

Full validated plan: `...\stripped\docs\ND_EthernetII_Fix_Plan.md`.
Reviews: `...\stripped\docs\ND_EthernetII_Emulator_Correctness_Analysis.md` (code/spec/firmware)
and `D:\OCR\ai\ND-324534-G1-EN\NDBusEthernetII-Schematic-Review.md` (schematic).

---

## What is CONFIRMED CORRECT this session (do not touch)
- Register map: status read at base+0/+2, control write base+1/+3; NO data register (0xFF stub removed).
- SCIP doorbell: 0xEF0080-9F write -> ND-100 INT12.
- MC68000 STOP wakes on IRQ (`&= ~WAIT` bug fixed) - see [[mc68k-stop-wait-bug]].
- LANCE `SwapByteLanes` + big-endian `DmaIn`/`DmaOut` - LANCE now fully inits (IDON, MAC
  08:00:26:64:00:00 = valid ND OUI, machine 100 - NOT a placeholder).
- **C-4 (committed):** MFP timer clock 3.6864 -> **3.125 MHz** (3CLK = 12.5MHz/4).
- **C-5 (committed):** power-low (control bit 6) -> 68K level 7 rising-edge; LANRESET
  (0xEF00B0-B7) -> `lance.Reset()` via new `OnLANReset` action.
- **DMA touches (committed):** DmaIn/DmaOut mask `addr &= 0xFFFFFF` + bounds-check ->
  `lance.SimulateMemoryError()` on overrun.

## REVERTED this session (were my mistakes)
- **STC block (TPE test 12):** the F138 decoder ignores addr bit 8, so EF01xx==EF00xx
  (EF0140=MERRSTAT, EF0160=EAREN, EF01A0=LANCE RDP). Test 12 FAILS on real ND-110 hardware.
  I had tuned the emulator to pass a test the real card fails - reverted. STC kept only as a
  harmless off-by-default diagnostic shim.
- **C-1 (IDENT keeps interruptEnabled):** broadly regressed TPE 5/6/7/8/11 ("Unexpected error
  in KICK FROM PIOC"). Reverted; IDENT still does `interruptEnabled = false;` with a NOTE.
  The reviews say A1/C-1 must be done TOGETHER with C-2/C-3 holistically, not alone.

---

## CURRENT LEAD - A4 (uncommitted, built clean, AWAITING TPE VALIDATION)
**Change:** control-word bit-2 handler (~line 1236 in NDBusEthernetII.cs). Replaced the broken
rising-edge `memoryMap.SetMFPInterrupt()` with a proper LEVEL drive of MFP GPIP I6:

```csharp
// A4: control bit 2 (ND interrupt) is a LEVEL on MFP GPIP I6 (active-low), NOT a one-shot.
// Old rising-edge SetMFPInterrupt() drove I6 LOW and never raised it -> pin stuck.
memoryMap.MFP.GPIO_6 = !ND_interrupt;   // bit2=1 -> I6 LOW (active); MFP gpio_input does AER+IER edges
if (ND_interrupt && !previousND_interrupt)
    Logger.Log("ND-100 -> 68000: GPIP I6 asserted (ND interrupt, vector 0x4E)", Logger.LogLevel.Device);
```

**Rationale:** ND-100's post-boot commands to the firmware go through GPIP I6 into
`nd_host_interrupt_handler` (vector 0x4E, 68K level 3). The old code left I6 stuck LOW forever,
so the firmware's GPDR polling never saw it deassert -> after the first OPCOM wake, subsequent
ND-100 commands produced no fresh edge. This matches the observed stall symptom:
**"ND-100 responds/IDENTs once, firmware then floods unanswered SCIP."**

**RISK:** the review calls this "test-11 territory." If TPE test 11 regresses, the AER edge
polarity is wrong - adjust polarity or revert like C-1.

**DO NOT TOUCH YET:** the separate ND-interrupt path at ~line 848 (case 116 vector handler,
does `GPIO_6=false; MFP.TriggerInterrupt(6)`). Leave until A4 is validated in isolation.

---

## NEXT STEPS (for whoever picks this up)
1. Ask Ronny to rebuild `Emulated.HW` + relaunch HOST, run `TPE> run` (all tests).
   - If tests 1-11 hold (esp. 11): commit A4.
   - If 11 regresses: flip AER polarity or revert A4.
2. Then `START-NETWORK-SERVER ENNS0` with fresh trace. Key question:
   **does the firmware now process MORE THAN ONE ND-100 command, and does ND-100 respond more
   than once?** If yes, the handshake advanced past the single-OPCOM wall -> keep going.
3. If A4 doesn't advance it: continue in the controller. Candidates not yet examined:
   - the case-116 line-848 duplicate ND-interrupt path (reconcile with A4).
   - 68K<->ND-100 monitor postbox handshake (postbox ~0x40A: counters 0x40A/0x410,
     monitor_code 2=wait/3=ready/4=warmboot; 0x406=CMD, 0x408=subfunc, 0x4C0=started).
     `maybe_monitor_wait_ack` loops resending monitor_code=2 until D0==1 - check what
     supplies D0=1 on the emulated side.

## Constraints (from user, non-negotiable)
- NEVER mention Claude in commits. One change at a time. Both oracles each step.
- Never kill/stop/terminate any process without explicit permission - ASK Ronny to
  relaunch the host for a fresh build.
- No LINQ/foreach/FluentAssertions; keep all comments; ASCII only; full file paths.

## Related memory
[[ethii-emulator-fixes]], [[mc68k-stop-wait-bug]], [[encos-enns0-analysis]],
[[enns0-monitor-and-xrout-protocol]], [[subtype-07-network-error]].
