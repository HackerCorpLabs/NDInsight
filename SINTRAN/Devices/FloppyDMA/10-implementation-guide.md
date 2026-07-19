# 10. Implementation Guide + C / C# Code Review

This file (a) tells you what to implement, and (b) reviews the two existing software models
against the documentation, with special attention to the **boot/autoload path** (which was
explicitly requested). Reviewed:

- **RetroCore C#** — `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusFloppyDMA.cs`
  (read in full for this review).
- **nd100x C** — `/home/ronny/repos/nd100x/src/devices/floppy/deviceFloppyDMA.c` + `.h`
  (read in full for this review).
- Cross-reference: the handoff `E:\Dev\Repos\Ronny\nd-120\Verilog\docs\HANDOFF-nd100x-floppy-dma-manual-fixes.md`.

Verdicts use: **manual** = ND-11.021.1, **firmware** = the `34300G.bin` ROM (authoritative for
actual behaviour).

---

## 10.1 The boot / autoload path — what actually happens today

> **STATUS UPDATE (RetroCore C#): implemented.** `NDBusFloppyDMA.ExecuteAutoload()` now performs
> a real autoload — see §10.1a below. The stub description that follows is retained for the
> nd100x C model (still a stub) and as the historical baseline.

**Original short answer: nothing real. In both emulators `ExecuteAutoload` was a stub.** Neither
read the floppy, located the BPUN bootstrap, or DMA'd an image into ND-100 memory. Compare
against the real firmware flow in [`04-boot-and-autoload.md`](04-boot-and-autoload.md) §4.3.

**nd100x C** (`ExecuteAutoload`, line 227):
```c
/* TODO: Implement */
/* TODO: DMA TRANSFER PROM bootcode to ND-100 Memory */
Device_QueueIODelay(self, IODELAY_FLOPPY, (IODelayedCallback)AutoLoadEnd, 0, self->interruptLevel);
```
It transfers **no data**, but it *does* queue `AutoLoadEnd`, which sets RFT and raises the
completion interrupt. So the guest doesn't hang — it just gets a "done" with an empty result.

**RetroCore C#** (`ExecuteAutoload`, line 506):
```csharp
Log($"Executing Autoload");
/* TODO: DMA TRANSFER PROM bootcode to ND-100 Memory */
//active = true;
//QueueIODelay(10, AutoLoadEnd, 0, InterruptLevel);
```
The completion call is **commented out**. So `AutoLoadEnd` never runs, **RFT is never
re-set, and no interrupt fires** — a guest that actually issued control-word autoload and
waited for RFT would **hang**. This is strictly worse than nd100x. `AutoLoadEnd` (line 1028)
is therefore dead code.

**Why booting still works today:** the machine setup loads the BPUN bootstrap into ND-100
memory by a *different* route and calls **`InitializeForBoot(unit)`** (C#, line 402), which
just sets `selectedDrive` and `rsr1 = ReadyForTransfer`. That is an out-of-band shortcut, not
the controller's real autoload. Real "press LOAD → boot from floppy" through the controller
is **not** emulated in either model.

### 10.1a How the RetroCore C# autoload is now implemented

`NDBusFloppyDMA.ExecuteAutoload()` (control-word bit 2) now:
1. Picks the boot unit (the selected drive, or 0 — the firmware selects/restores drive 0).
2. Marks the controller busy (clears RFT).
3. Calls a host delegate **`OnAutoloadRequest(unit)`**. The controller can't reference the
   BPUN parser directly (it lives in `Emulated.Utilities`, which references `Emulated.HW` —
   referencing it back is circular), so the **machine** services the request:
   `ND100Machine.LoadFloppyBpunIntoMemory(controllerId, unit)` reads the first track, parses
   the BPUN image (reusing the existing `BPUN` class), and DMAs the code into ND-100 memory at
   the BPUN load address. This is the same helper the ND-100 LOAD path
   (`LoadBootStrapperFromFloppy`) now uses, so the two boot routes share one implementation.
4. On success: `errorCode = TRANSFER_OK`, select the unit, complete with RFT (+ interrupt) via
   the queued `AutoLoadEnd`. The ND-100 LOAD microcode transfers control — the controller does
   not jump, matching the firmware.
5. On failure (no media / bad BPUN / no handler): `errorCode = NO_BOOTSTRAP_FOUND_ON_DISKETTE`
   (oct 50), and `AutoLoadEnd` reflects it in the hardware status word (bit 4 OR-of-errors +
   bit 7 hard error).

Covered by tests in `Emulated.Tests.ND100\ControllerFloppyDMA\TestNDBusFloppyDMA.cs`
(`Autoload_Success_*`, `Autoload_NoHandler_ReportsHardError`,
`Autoload_HandlerReturnsFailure_ReportsHardError`). Divergence #9 is resolved for C#; nd100x C
remains a stub.

**What a correct autoload needs** (from firmware, §4.3/§4.6):
1. On control-word bit 2 (State I) — and, given the §4.2 ambiguity, also accept the bit-8
   fetch-and-execute path the ND-100 microcode may use.
2. RESTORE to track 0, read the first ~2 KB into a buffer.
3. Locate the BPUN signature `'!'` (0x21); require a CR (0x0D) within the preceding 128 bytes
   and an octal digit `'0'..'7'` just before it → else **error 50₈**.
4. (Optional) reject an out-of-date monitor → **error 51₈**.
5. Parse the BPUN header → ND-100 load address + word count; DMA the image there.
6. Complete with an interrupt. Do **not** start execution — the ND-100 LOAD microcode does.

> Because the exact BPUN header arithmetic is not fully resolved (§4.6 [COULD NOT DETERMINE]),
> a functional emulator should parse the BPUN format from its own spec, and can validate
> against the working image `…\N100-FLOPPY-3112\ND Code\DEPOSIT 0 77400.txt`.

---

## 10.2 Divergence table (both models vs. the documentation)

Winner = what the **firmware/manual** say is correct.

| # | Topic | nd100x C | RetroCore C# | Firmware/manual says | Who's right |
|---|-------|----------|--------------|----------------------|-------------|
| 1 | Error-code bit position | `errorCode:7` at **bit 8** (`.h` ~L318) | `errorCode << 9` (bits 9–14) | `(code&0x3F)<<1` in high byte → **bits 9–14** [FW @06b4] | **C#** |
| 2 | IOX `+4` read | returns `status2.raw` (never populated) | returns same as `+2` | `+4 == +2` hardware status word (§3.1 Note 1) | **C#** |
| 3 | Two-status-word split | conflated (dualDensity into CB+6) | conflated (dualDensity bit 15 into CB+6) | HW word (IOX) has b15 dual-density, no code; Status Word 1 (CB+6) has code b9–14, b15 clear | **neither** |
| 4 | Write-protect on WRITE | **enforced** → `WRITE_PROTECTED`, abort | `floppyIsWriteProtected` read but **unused** → write succeeds | oct 16 real, abort before write [FW @0e84/@1ea2] | **nd100x** |
| 5 | Bit 4 "OR of errors" | computed = `hardError\|deleted\|retry` | **never set** (`InclusiveOrReg2` unused) | set whenever **code≠0** (or DMA error) [FW @06bc] | **nd100x**, but see 5b |
| 5b | Bit 4 formula detail | misses plain `errorCode≠0` case (e.g. CRC alone) | — | firmware sets b4 on **any** non-zero code | **neither fully** |
| 6 | Error-code enum completeness | has WRITE_PROTECTED, RAM_ERROR, DRIVE_NOT_READY | **missing** WRITE_PROTECTED(14), FORMAT_NOT_FOUND(8=oct10), RAM_ERROR(oct71) | full octal table (§3.9) | **nd100x** |
| 7 | Backend read/write fail code | `DRIVE_NOT_READY` (oct 20) | `CRC_ERROR` (5) | drive/backend failure ≈ not-ready; CRC = bad media (§3.10) | **nd100x** |
| 8 | IDENTIFY / unknown cmd completion | `IDENTIFY` + `default` both queue `ReadEnd` → complete | `IdentifyFloppy` has **no case** → `default: break` → **no `ReadEnd`** → RFT never set → **hang** | every command must complete + interrupt [FW @03e3] | **nd100x** |
| 9 | Autoload | stub, but completes (no data) | stub, completion **commented out** → hang | full bootstrap load (§4.3) | **neither** |
| 10 | Dual-density bit 15 location | on CB+6 (wrong) + IOX | on CB+6 (wrong) + IOX | IOX hardware word **only**; clear in CB+6 (§3.4/§3.7) | **neither** |

---

## 10.3 RetroCore C# — specific findings

> **STATUS: divergences 1-10 now FIXED.**
> - **C#:** write-protect enforced (`WRITE_PROTECTED=14`); bit-4 OR set when `errorCode!=0`;
>   IDENTIFY/default complete; enum gained `WRITE_PROTECTED`/`FORMAT_NOT_FOUND`/`RAM_ERROR`;
>   autoload + error-image done; **two-status-word split done** — IOX +2/+4 →
>   `CalculateHardwareStatusWord` (bit-15 dual-density, no code), CB+6 → `CalculateStatusWord1`
>   (code bits 9-14, bit-15 clear).
> - **nd100x C:** error code moved to bits 9-14 (`.h` bitfield, FIX 1); IOX +4 → hardware word
>   (FIX 2); split into `CalculateHardwareStatusWord` / `CalculateStatusWord1` (FIX 3); bit-4
>   formula includes `errorCode!=0` (5b). Builds clean.
> - Tests: `TestNDBusFloppyDMA.cs` (`WriteData_OnWriteProtectedFloppy_*` asserts the split,
>   `IdentifyFloppy_Completes_*`). C# 251/0.
>
> Remaining follow-ups (not blockers): Status Word 2 §3.5.2.2 bit remap, C real BPUN autoload,
> the 7 PIO tasks.

Ordered by severity.

1. **Write-protect not enforced (divergence 4).** `NDBusFloppyDMA.cs:736` computes
   `floppyIsWriteProtected = IsDeviceReadOnly(unit)` and never reads it. `WriteData`
   (`:818`) writes unconditionally. **Fix:** before the write, if `floppyIsWriteProtected`,
   set `errorCode = WRITE_PROTECTED` (add `= 14` to the enum), set RFT, write the status
   block, interrupt, and return — do **not** write. (Match the nd100x WRITE block at C `:445`.)

2. **IDENTIFY / unknown command hangs (divergence 8).** `IdentifyFloppy = 0x38` has no
   `case` in the `switch` (`:780`), so it hits `default: break` (`:957`). The post-switch
   block writes Status 1/2 once but **never queues `ReadEnd`**, so `ReadyForTransfer` (cleared
   at `:769`) is never restored and no interrupt fires. **Fix:** give `default` (and
   Identify) a `QueueIODelay(IODELAY_FLOPPY, ReadEnd, unit, InterruptLevel)` like every other
   case.

3. **Bit 4 "OR of errors" never set (divergence 5).** `InclusiveOrReg2` is declared
   (`:278`) but never assigned. **Fix:** in the status-word builder, set bit 4 when
   `errorCode != TRANSFER_OK` (the firmware truth), not the nd100x `hardError|deleted|retry`
   formula.

4. **Two status words conflated (divergence 3/10).** `CalculateStatusWithDualDensity()`
   (`:437`) ORs in `DualDensityController` (bit 15) and is used for **both** the IOX read
   (`:425`) *and* the CB+6 memory writeback (`:727,796,844,968,1019`). Per §3.4, bit 15 in
   **Status Word 1 (CB+6) must be clear**. **Fix:** split into two builders — a hardware-
   status-word builder (bit 15 set, no error code) for IOX +2/+4, and a Status-Word-1 builder
   (error code at bits 9–14, bit 15 clear) for the CB+6 writeback.

5. **Autoload is a no-op that hangs (divergence 9).** See §10.1. **Fix:** implement the §4.3
   flow, or at minimum re-enable a completion so the guest doesn't hang.

6. **Error-code enum incomplete (divergence 6).** Add `WRITE_PROTECTED = 14`,
   `FORMAT_NOT_FOUND = 8`, `RAM_ERROR = 0x39` (oct 71 = 57 dec), etc. Note **values are
   decimal placed at bits 9–14, so the SINTRAN driver reads them back as the octal figure**
   (e.g. value 14 → bits 9–14 = 14 dec = **016 octal** = "Write protected", matching §3.9).

7. **Minor correctness bugs:**
   - `:744` `case 2: bytes_pr_sector = 123;` — **typo, must be 128** (128 B/sector). Also at
     C# `:882` the ReadFormat comment repeats "123".
   - `:638` `sectors_pr_track = 18;` with the comment noting DD is 8 sectors/track — the
     geometry (`:748-749`) mixes `sectors_pr_track` and `sectors_pr_track-1` and is not the
     manual geometry (8 sectors × 77 tracks). This "works" for the current boot path but is
     not format-correct; derive geometry from the selected format (§5).
   - Backend read/write failure uses `CRC_ERROR` (`:792,840`); prefer `DRIVE_NOT_READY`
     (divergence 7).

**Correct in C# (keep):** error code `<< 9` (divergence 1 ✓), IOX +4 == +2 (divergence 2 ✓),
`Reset()` leaving RFT clear (matches firmware; prevents a spurious level-11 interrupt).

## 10.4 nd100x C — specific findings

1. **Error code at bit 8, must be bit 9 (divergence 1).** `deviceFloppyDMA.h` `errorCode : 7`
   sits at bits 8–14. **Fix (handoff FIX 1):**
   ```c
   uint16_t hardError : 1;   // Bit 7
   uint16_t notUsed8  : 1;   // Bit 8
   uint16_t errorCode : 6;   // Bits 9-14
   uint16_t notUsed15 : 1;   // Bit 15
   ```

2. **IOX +4 returns the wrong register (divergence 2).** `FloppyDMA_Read` `:127` returns
   `data->status2.raw` (which is never populated). **Fix (handoff FIX 2):** return the
   hardware status word, same as `+2`.

3. **Two status words conflated (divergence 3/10).** `CalculateStatusRegister1` sets
   `dualDensity = 1` and is used for both IOX +2 (`:124`) and the CB+6 writeback (`:632`).
   **Fix (handoff FIX 3):** a `CalculateHardwareStatusWord()` (b15 set, no code) for IOX, and
   a `CalculateStatusWord1()` (code b9–14, b15 clear) for CB+6.

4. **Bit 4 formula incomplete (divergence 5b).** `inclusiveOrBits = hardError | deletedRecord
   | retryOnController` (`:104`) misses the common case where only `errorCode` is set (e.g.
   CRC, write-protect) with none of those three bits. Firmware sets bit 4 on **any** non-zero
   code. **Fix:** `|| (errorCode != 0)`.

**Correct in nd100x (keep):** write-protect enforcement (`:449`), command completion for
IDENTIFY/`default` (`:583/:588`), the full error-code enum, `DRIVE_NOT_READY` for backend
failures.

## 10.5 Recommended work order

1. **C# write-protect + error enum** (divergence 4, 6) — small, high value, unblocks correct
   error reporting. Add `WRITE_PROTECTED = 14`.
2. **C# IDENTIFY/default completion** (divergence 8) — prevents a real hang.
3. **C# bit-4 OR-of-errors** (divergence 5) — trivial once the status builder is touched.
4. **Both: split the two status words** (divergence 3/10) — the structural fix; do C# and C
   together so they stay bit-identical, and validate against the corrected Verilog testbench
   (`nd-120/Verilog/…/nd_floppy_dma_tb.v`, `TB_RESULT: PASS`).
5. **nd100x error-code bit position + IOX +4** (divergence 1, 2) — per handoff FIX 1/2.
6. **Autoload** (divergence 9) — the largest piece; implement the §4.3 firmware flow if real
   controller-driven boot is wanted. Until then, at least re-enable the C# completion so it
   cannot hang.

## 10.6 Validation

- There is no standalone unit test for the nd100x floppy DMA; validate by booting SINTRAN /
  running the `BFDIS` driver and confirming it reads the error code from CB+6 bits 9–14 and
  detects the DMA controller via IOX +2 bit 15. [handoff §How to validate]
- RetroCore has floppy DMA tests under
  `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests.ND100\ControllerFloppyDMA\TestNDBusFloppyDMA.cs`
  — extend these for write-protect (expect error 016₈ at CB+6 bits 9–14), IDENTIFY completion
  (expect RFT + interrupt), and the bit-4 OR flag.
- The corrected Verilog core + its passing testbench are the bit-exact oracle for the
  two-status-word split.
