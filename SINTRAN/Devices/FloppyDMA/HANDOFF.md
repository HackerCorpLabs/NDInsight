# HANDOFF — ND 3112 floppy-DMA controller (C# + C)

**Date:** 2026-07-13. **Status:** all 10 floppy-DMA divergences fixed in both models; PIO fixes pending.

## What was done

**Docs (this folder `NDInsight\SINTRAN\Devices\FloppyDMA\`):** full 11-file spec set for the
3106/3112 written from the ND-11.021.1 manual + reverse-engineered Z80 firmware `34300G.bin`
(overview, programming interface, commands, boot/autoload, formats, error codes, firmware
internals, streamer, testing, implementation guide, README).

**Ghidra project `RetroGhidra\N100-FLOPPY-3112\ND-FLOPPY-3112.gpr`:** fully annotated — all
`FUN_ram_*` renamed, all `DAT_ram_*` + I/O ports labelled, plate/EOL comments. Key anchors:
`HostCmd_ISR @030c`, `Autoload_BootstrapLoad @1ae8`, status builder `@06b3`, `RST 08h` error
table `@1e8d`, error-image construction `@1f2f-1f6f`, images `@1a92` (LOAD-ERROR) / `@1acc`
(WRONG-BOOTSTRAP).

**RetroCore C# `Emulated.HW\ND\CPU\NDBUS\NDBusFloppyDMA.cs`** (+ `Emulated.Machines\...\ND100Machine.cs`):
- Autoload (`ExecuteAutoload`) implemented — reads floppy, parses BPUN via machine delegate
  `OnAutoloadRequest` → `LoadFloppyBpunIntoMemory`, DMAs to memory, completes.
- Boot error image: `DmaAutoloadErrorImage` DMAs the byte-exact ROM LOAD-ERROR / WRONG-BOOTSTRAP
  image to the first page so the console prints `** LOAD-ERROR: nn **` on boot failure.
- Write-protect enforced (`WRITE_PROTECTED=14`, error 16 oct, no write).
- IDENTIFY + unknown commands now complete (queue `ReadEnd`) — no hang.
- Bit-4 OR-of-errors set when `errorCode != 0`.
- Enum gained `WRITE_PROTECTED`/`FORMAT_NOT_FOUND`/`RAM_ERROR`.
- Two-status-word split: IOX +2/+4 → `CalculateHardwareStatusWord` (bit-15 dual-density, no
  code); CB+6 → `CalculateStatusWord1` (code bits 9-14, bit-15 clear).
- Tests `Emulated.Tests.ND100\ControllerFloppyDMA\TestNDBusFloppyDMA.cs` (26; full ND100 251/0).

**nd100x C `/home/ronny/repos/nd100x/src/devices/floppy/deviceFloppyDMA.{c,h}`:**
- Error-image DMA on autoload failure (`DmaAutoloadErrorImage`, byte-exact) — but C still lacks
  a real BPUN autoload, so its autoload always shows LOAD-ERROR 50 (TODO in code).
- FIX 1: error code `.h` bitfield moved to bits 9-14. FIX 2: IOX +4 → hardware word.
  FIX 3: split `CalculateHardwareStatusWord` / `CalculateStatusWord1`. Bit-4 includes
  `errorCode!=0`. Builds clean (`cmake --build build_linux --target devices`).

## Not verified
End-to-end "console actually prints LOAD-ERROR" (no ND-100+console integration test). Image
bytes verified vs ROM hexdump; DMA verified by unit test. Entry point word 0 inferred.

## Pending — PIO (3027) controller, another day
7 tracked tasks from `HANDOFF-floppy-pio-c-and-csharp-fixes.md` (this is a DIFFERENT device):
C-PIO-1 (C read-only image/dead WP guard), C-PIO-2 (FORMAT_TRACK off-by-one), CS-PIO-1 (C#
`Debug.Assert(false)` landmines), SH-PIO-1/2/3 (control-reset RFT, device-clear+cmd ordering,
sector auto-increment boundary — both models), cosmetics + SH-PIO-4 (autoload bit-2
unverifiable). Validate with the `FLOPPY-FU-1986F` test program. Spec: §B.4 in
`NDInsight\Reference-Manuals\ND-06.015.02 ND-100 Functional Description.md` (~9747-9990).

## Pending — analyse floppy driver vs FLOPPY-STREAM C03 (task #8, another day)
PRE-EXISTING (confirmed: reproduces on a pristine nd100x build, NOT caused by the fixes above).
Running the TPE test program **FLOPPY-STREAM Version C03 - 1988-11-08** (`--boot=floppy
--image=FLOPPY.IMG`, then `fl-s`, `sel-de` unit 0) fails at SELECT-DEVICE with:
`***** ERROR : Interrupt expected but not detected after transfer`. The C floppy-DMA model
(and likely C#) doesn't deliver the completion interrupt the test expects after a transfer.
For another day: dig in with `DEBUG_FLOPPY_DMA`/`DEBUG_DETAIL` logging + DAP tracing of the
ND-100 side — which command it issues, whether `ExecuteFloppyGo`→`ReadEnd` runs, why
`interruptEnabled && readyForTransfer` doesn't fire when expected. (Test self-reports the card
as "3027 Floppy DMA controller, Micro program B".)

To restore my C work if a session starts fresh: it's uncommitted in the WSL nd100x repo
(`git stash` list / working tree `src/devices/floppy/deviceFloppyDMA.{c,h}`); rebuild with
`cmake --build build_linux --target nd100x`.

## Follow-ups (not blockers)
Status Word 2 §3.5.2.2 bit remap; C real BPUN autoload; error-51 trigger (BPUN parser can't
distinguish 50 vs 51, so boot failures report 50).
