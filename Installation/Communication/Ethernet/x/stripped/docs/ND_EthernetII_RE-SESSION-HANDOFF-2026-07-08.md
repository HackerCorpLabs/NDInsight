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

## CURRENT LEAD - A4 (VALIDATED, READY TO COMMIT)
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

**TPE VALIDATION (2026-07-22):** Tests 1-11 ALL PASS ✓ (NO REGRESSION on test 11).
Tests 12/24/25/26 fail as expected (baseline failures, not A4-related).

**STATUS:** Commit A4 to NDBusEthernetII.cs immediately.

**DO NOT TOUCH YET:** the separate ND-interrupt path at ~line 848 (case 116 vector handler,
does `GPIO_6=false; MFP.TriggerInterrupt(6)`). Leave until A4 is committed and ENNS0 trace is analyzed.

---

## NEXT STEPS (Session 2026-07-22)
1. **AFTER WINDOWS REBOOT:** Commit A4 to NDBusEthernetII.cs (no changes, tests validated).
   ```
   // A4: control bit 2 (ND interrupt) is a LEVEL on MFP GPIP I6 (active-low)
   memoryMap.MFP.GPIO_6 = !ND_interrupt;
   if (ND_interrupt && !previousND_interrupt)
       Logger.Log("ND-100 -> 68000: GPIP I6 asserted (ND interrupt, vector 0x4E)", Logger.LogLevel.Device);
   ```
2. Rebuild `Emulated.HW`, relaunch HOST.
3. Boot menu option 8 "SINTRAN Ronny Ethernet II Test" added to ND100Script.ini.
   - Uses ND100-ETH config (Ethernet II + DMA Floppy + SMD).
   - Boot label: `ND-BOOT-ETHII-SINTRAN`.
   - Boot disk: **`D:\BIGDISK0-L.IMG`** (replaced with `F:\RC\RonnyTest\HDLC1\BIGDISK0-L2-100.IMG`, original backed up as `.BACKUP`).
4. Run `RT ENNS0` + `START-NETWORK-SERVER ENNS0` with `DebugTrace 2 4`. Delete trace file first.
   Key question: **does firmware process MORE than ONE ND-100 command?** If yes -> handshake advanced.
5. If A4 doesn't advance ENNS0: continue in controller. Candidates:
   - case-116 line-848 duplicate ND-interrupt path (reconcile with A4).
   - 68K<->ND-100 monitor postbox handshake (postbox ~0x40A: D0 ready signal).

## Constraints (from user, non-negotiable)
- NEVER mention Claude in commits. One change at a time. Both oracles each step.
- Never kill/stop/terminate any process without explicit permission - ASK Ronny to
  relaunch the host for a fresh build.
- No LINQ/foreach/FluentAssertions; keep all comments; ASCII only; full file paths.

## Related memory
[[ethii-emulator-fixes]], [[mc68k-stop-wait-bug]], [[encos-enns0-analysis]],
[[enns0-monitor-and-xrout-protocol]], [[subtype-07-network-error]].
