# ND Ethernet II emulator - validated fix plan & TODO

**Date:** 2026-07-08. **Target:** `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs`
(+ MC68901MFP.cs, Am2990Lance.cs, Cpu68K.Interrupts.cs).
**Goal:** ENNS0 `START-NETWORK-SERVER` completes and registers with XROUT; TPE ETHERNET-TWO
stays a faithful mirror of the real card (which means test 12 is EXPECTED to fail).

Built from two independent reviews + my own trace/Ghidra work, each item tagged with a
confidence and a cross-check so we don't fix things blindly:
- Schematic review: `D:\OCR\ai\ND-324534-G1-EN\NDBusEthernetII-Schematic-Review.md`
- Code/spec/firmware review: `...\stripped\docs\ND_EthernetII_Emulator_Correctness_Analysis.md`
Confidence: **[VERIFIED]** = cross-checked against manual/schematic/trace; **[LIKELY]** =
one strong source, matches understanding; **[NEEDS-TRACE]** = must confirm dynamically.

---

## 0. Ground rules (how to work this)
1. **One change at a time.** Rebuild `Emulated.HW`, then rebuild+relaunch the HOST app.
2. **Two oracles every step:**
   - **Regression:** TPE ETHERNET-TWO all-tests. Baseline BEFORE any change (tests 1-11
     pass, 12 fails/`Timer exceed`, 24/25/26 fail). Do not regress 1-11.
   - **Goal:** `RT ENNS0` -> `START-NETWORK-SERVER ENNS0` with `DebugTrace 2 4`; then
     `ENCOS-MON STAT server 0 source O`. Watch the trace after each step.
3. **Delete `%LOCALAPPDATA%\trace\file-trace.txt` before each capture** (append-mode).
4. Commit already made on branch `ethernet-ii-controller-fixes` (baseline before these).

---

## A. Already done this session and CONFIRMED CORRECT - do not touch
| Fix | Confidence | Cross-check |
|-----|-----------|-------------|
| Status reg: read offset 0/2 = status word (drop 0xFF data stub) | [VERIFIED] | Review D; manual pp.44-45 |
| SCIP doorbell: 0xEF0080-9F write -> INT12 (BUG A) | [VERIFIED] | Both reviews; trace shows doorbell now reaches ND-100 |
| PROFF (0xEF0010-1F) + MODCR (0xEF0020-3F) no longer -> SCIP/INT12 | [VERIFIED] | Manual I/O map; review A6/A7; trace flood 1947->0 |
| MC68000 STOP wakes on IRQ (`&= ~WAIT`) | [VERIFIED] | Firmware `reset_entry` STOPs; review NOTE E |
| LANCE `SwapByteLanes` + `DmaIn`/`DmaOut` big-endian | [VERIFIED] | Review 2 section 1; LANCE now inits (IDON) |

---

## B. My session mistakes to REVERT (I tuned to a test the real card fails)
These emulate hardware that DOES NOT EXIST on this card. The F138 decoder ignores A8, so
EF01xx == EF00xx: **EF0140=MERRSTAT, EF0160=EAREN, EF01A0=LANCE RDP** (confirmed vs manual
I/O map lines 780-781 + review A6). And the real ND-110 log shows TPE test 12 FAILS on
physical hardware (freq ~12.5 MHz = CPU clock). So:

- [ ] **B-REV-1 [VERIFIED]** Remove the STC register block (0xEF0140/0160/01A0 handlers),
      `stcArmed`/`stcCountdown`/`STC_PERIOD_TICKS`/`timerTick32`/`timerConfigReg`, the STC
      arming in the config-write path, and the STC periodic tick in `Clock`. Let those
      addresses fall through to the real MERRSTAT/EAREN/LANCE-RDP decode.
- [ ] **B-REV-2 [VERIFIED]** Revert the timer interrupt from **level 5** back to the MFP
      Timer-C -> level-3 path (review BUG B: production RTC is MFP Timer C, level 3, vector
      0x45, already wired). The level-5 STC path was for the non-existent chip.
- [ ] **B-REV-3 [LIKELY]** Accept TPE test 12 FAILING (as real HW does). Do NOT re-add STC
      logic to pass it. If a "diagnostic shim" is ever wanted, put it behind an explicit
      named flag, off by default.

Note: keep the AM9519/SCIP-channel `HandleSCIPChannelAccess` shim for now (TPE tests 5/6
lean on it) but flag it as non-hardware; revisit under C-6.

---

## C. New fixes, in suggested order (each: change -> rebuild -> both oracles)

- [ ] **C-1 [VERIFIED] IDENT must not clear `interruptEnabled`** (line ~1284).
      Hardware: IDENT/CLINT clears only RFT (the "interrupt set" latch), never RIE (enable).
      Change: in `IDENT()` remove `interruptEnabled = false;` keep `scipPending = false;`.
      Expected: after SINTRAN enables interrupts once, every later doorbell asserts INT12
      without needing a control rewrite. Likely removes the need for most `scipPending`
      juggling. **Validate:** ENNS0 - does ND-100 now get INT12 on every 68K doorbell (not
      just the first)? TPE 1-11 unaffected.

- [ ] **C-2 [VERIFIED] RFT is a latch until IDENT/master-clear; status bit 2 reads the latch.**
      Change: `scipPending` set on any SCIP write, cleared ONLY by IDENT and controller
      reset (not by the "next control-word write"). Status bit 2 = `scipPending` (already
      partly there). Remove the "consumed by next control write" flush.
      **Validate AFTER C-1:** re-test the indefinite-latch behavior that previously regressed
      TPE 12/24/25/26 - the reviews argue that regression was caused by C-1 still being
      present, so it should be clean now. If TPE 24/25/26 regress, stop and re-examine.

- [ ] **C-3 [VERIFIED] Control bit 2 (ND interrupt) = level on MFP GPIP I6.**
      Change: on EVERY control-word write, `mfp.GPIO_6 = ((value & 4) == 0);` (level follows
      the bit). Delete the rising-edge `SetMFPInterrupt()` + forced `mfp.TriggerInterrupt(6)`
      and the never-called `ClearMFPInterrupt()`. The MFP core `gpio_input()` already does
      AER+IER edges. **Validate:** TPE test 11 (OPCOM/ND-calling-int); ENNS0 GPIP6 path
      (`nd_host_interrupt_handler`). Watch for missed/duplicated ND->68K interrupts.

- [ ] **C-4 [VERIFIED] MFP timer clock 3.6864 MHz -> 3.125 MHz** (line ~1728,
      `SetTimerClock`). Schematic: 3CLK = 12.5 MHz / 4 = 3.125 MHz. Fixes RTC (~130-236 Hz
      seen in trace is ~18% fast) and any TPE frequency window. **Validate:** RTC tick rate
      in trace drops ~18%; TPE 1-11 unaffected.

- [ ] **C-5 [LIKELY] Small self-contained:**
      - Power-low (control bit 6) -> raise 68K level 7 (autovector). (review A5)
      - LANRESET (0xEF00B0-B7 access) -> `lance.Reset()` instead of just logging. (review A9)
      **Validate:** no regression; only exercised by specific firmware/diagnostic paths.

- [ ] **C-6 [NEEDS-TRACE] EF0080 GPDR alias + mirrored/direct split.**
      Review says EF0080 is SCIP-only; GPDR alias has no hardware basis (MFP CS is EF00C0-FF).
      We currently keep the GPDR write on 0xEF0080 for TPE test 5. Decide: keep as a flagged
      diagnostic shim, or remove. **Do LAST** - needs a TPE test-5 trace to know what it
      relies on. Do not remove blind.

---

## D. Verification-only tasks (confirm before deciding to change)
- [ ] **D-1 [NEEDS-TRACE] `Am7990Lance.ClearInterrupt()` on level-2 IACK** (review B1). Hardware
      IACK doesn't touch the LANCE; INTR follows CSR0. Read what `ClearInterrupt()` does; if it
      clears flags/pin, model level 2 as continuous `IsInterruptActive` instead.
- [ ] **D-2 [NEEDS-TRACE] MERRSTAT/EAREN latch event** (review B3). Should latch on MERR
      (parity), not on unmapped-address bus errors. Relevant to TPE 24/25 (currently failing
      anyway). Low priority.
- [ ] **D-3 [NEEDS-TRACE] Protect table / WRIV(I7) / LANERROR(I5)** unwired (review B4). TPE 26.
- [ ] **D-4 [LIKELY] Confirm production firmware never writes 0xEF0140 (STC)** so B-REV-1/2 are
      safe (it should now hit MERRSTAT read). Check a trace.

---

## E. Cosmetic / low-risk (batch whenever)
- Octal-vs-decimal vector logging (`LogMFPVectorType`/`MapMFPVectorToSystem`) - dead code,
  add/keep warning comment (review C1 / TRAP C).
- Bank number hardcoded 16 for all thumbwheels - take from thumbwheel (review C2).
- CPU clock comment 10 MHz -> 12.5 MHz; revisit `CPU68K_CYCLES_PER_ND100_TICK` (1:3.2 vs 1:4).

---

## F. The critical validation question (do this FIRST, before C-1)
Establish the TPE ETHERNET-TWO **baseline on the current committed build** and write down the
exact pass/fail of tests 1-11-12-20..28. Everything after is measured against it. If any of
C-1..C-4 regresses tests 1-11, revert that single change and re-examine - do not stack changes.

**Order of execution:** F (baseline) -> B-REV-1/2/3 (revert STC) -> C-1 -> C-2 -> C-3 -> C-4
-> C-5 -> (D verifications) -> C-6. One at a time, both oracles each step.
