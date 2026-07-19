# 4. Boot Mode and Autoload — How Booting From Floppy Works

This is the part that was least understood, so it is documented from **both** the manual and
a full reverse-engineering of the on-card bootstrap loader. Where the two agree it is stated
plainly; where only the firmware knows, it is marked **[FIRMWARE @addr]**; open questions are
marked **[COULD NOT DETERMINE]** rather than guessed.

## 4.1 The two kinds of "load" the manual describes  [MANUAL §1]

The manual describes two distinct load mechanisms:

1. **DMA-load ("first page" load).** *"When DMA-load is performed, 'the first page' on floppy
   is dumped to 'first page' on the ND-100."* This is the mass-storage / binary-format load
   used by the ND-100 CPU microcode. Because of the ND-100 microprogram, it can only be
   invoked **from a terminal** (the `2156Q$` command), **not** by setting the ALD (Automatic
   Load Descriptor) selector on the CPU module. [MANUAL §1]

2. **BPUN LOAD ("press LOAD").** *"It is also possible to 'load' BPUN-files (Binary PUNch) of
   maximum 64 Kwords directly from the floppy by pressing LOAD."* This is the operator boot:
   it loads a bootstrap/monitor image (a BPUN-format file) off the floppy into ND-100 memory
   and starts it. For this to work the floppy must carry a current **FLO-MON** (Floppy
   Monitor 2010F or newer); an out-of-date FLO-MON gives **error 51₈**. [MANUAL §1]

Both are ultimately driven the same way at the controller: the ND-100 loads the **Hardware
Control Word with bit 2 ("Activate Autoload")** — this is control-word **State I** in
§3.6 Table 1, *"Load floppy monitor"*.

> **Reconciliation with the firmware.** The "LOAD" button lives on the **ND-100 CPU**, not on
> the controller card. Pressing it runs ND-100 microcode, which issues the control word to
> the 3112. The firmware confirms there is **no autoload path triggered by controller
> hardware** — see §4.5. So "press LOAD" = "ND-100 microcode asserts the autoload control
> word to the 3112".

## 4.2 Firmware entry: how the control word reaches autoload  [FIRMWARE]

When the ND-100 loads the control word, CTC channel 0 interrupts the Z80 and vectors to
**`HostCmd_ISR` (`ram:030c`)**. The ISR stores the 16-bit control word at `2098h` and then
decides what to do:

- **[FIRMWARE @0325]** If the **"fetch command & execute" bit (bit 8)** is set, dispatch falls
  through to the **default handler `ram:1ae8` (`Autoload_BootstrapLoad`)** — the real
  bootstrap loader with the signature scan and the oct 50 / oct 51 errors.
- **[FIRMWARE @0332-0336]** There is *also* an explicit gate `IN (0x52); CP 0x04` testing
  **control-word bit 2 alone**, which dispatches a *different* handler `ram:1c05` — that one
  reads the floppy and shows a 2-digit code on the front-panel 7-segment LEDs, but does **not**
  perform the `'!'`-signature bootstrap scan.

> **[COULD NOT DETERMINE]** Exactly which control-word bit(s) the ND-100 microcode asserts for
> an operator LOAD — bit 2, bit 8, or both. The firmware has two loaders; the one that
> actually parses and loads a BPUN bootstrap is unambiguously `ram:1ae8`. The rest of this
> document traces `ram:1ae8`, because that is the loader that implements "load the monitor
> from floppy". An emulator that only needs to *boot* should implement the `ram:1ae8`
> behaviour and accept the autoload control word (see §4.6).

## 4.3 The bootstrap loader state machine (`ram:1ae8`)  [FIRMWARE — verified]

Step by step, exactly as the ROM does it:

1. **Recalibrate (RESTORE to track 0).** `Floppy_ReadToBuffer2200` (`ram:09f8`) issues a
   WD179x/FD1797 **RESTORE** (opcode `0x00`) via `OUT(0x74),0; OUT(0x75),0;` then `OUT(0x70),
   0x00`, and polls `IN(0x70)` bit 0 (BUSY) until clear. [FIRMWARE @0a0d-0a20]

2. **Read the first data into the buffer.** It fills the Z80 buffer at **`0x2200`** with
   **`0x800` = 2048 bytes** (`LD BC,0x800` @1aef) from the floppy. [FIRMWARE @1aef]
   > **[COULD NOT DETERMINE]** the exact track/side/sector/sector-count of that first read;
   > `09f8` is parameterised through the `IY = 0x2100` floppy state block. Confirmed only:
   > RESTORE to track 0, then a 2048-byte fill.

3. **Scan for the bootstrap signature `'!'`.** [FIRMWARE @1af2-1af6]
   ```
   LD A,0x21   ; '!'
   CPIR        ; search HL=0x2200 .. for 0x800 bytes
   ; not found  -> StatusErr_NoBootstrap_oct50
   ```
   The signature byte is **ASCII `'!'` (`0x21`)**.

4. **Validate the header behind `'!'`.** [FIRMWARE @1af9-1b16] Scan **backwards up to 128
   bytes** for a carriage return (`0x0D`, after masking `AND 0x7F`). If no CR → **oct 50**.
   The character immediately before the CR is masked (`RES 7`) and range-checked to be an
   **octal digit `'0'..'7'`** (`SUB 0x30` borrow → oct 50; `SUB 0x08` no-borrow → oct 50).

5. **Parse the header / classify format.** [FIRMWARE @1b17-1b62] It ORs 128 even-buffer bytes
   to classify one of two layouts, reads 16-bit fields out of the ASCII header (offsets
   relative to `'!'`: `-3`, `+5`, `+7`, `+10`, stride 2), and computes:
   - a **24-bit ND-100 host load address**, kept in `(0x2082)`;
   - a **word/byte count** for the transfer.

6. **Detect a wrong/out-of-date bootstrap.** [FIRMWARE @1b2a-1b41] If
   `header[-3] == '2'` **and** `header[+10] == '@'` **and** a 10-byte stride-2 XOR checksum
   equals **`0x52` = `'R'`**, raise **oct 51 (WRONG_BOOTSTRAP — "out of date FLO-MON")**.

7. **DMA the image to ND-100 memory.** [FIRMWARE @1b85-1bd0] Loop in **`0x800`-byte chunks**:
   wait for the host-DMA handshake (`HaltWaitLoop` @1bdf: `HALT` then `OUT(0x57)` NFINI),
   transfer the chunk to the host via `RST 0x28`, advance the running host target pointer
   `(0x2082) += (0x20cd)` (byte-swapped, big-endian), and re-fill the buffer with the next
   floppy chunk (`CALL 09f8`). Repeat until the count reaches zero.

8. **Complete and hand control to the ND-100.** [FIRMWARE @1bd2-1bde] Re-arm the host-command
   ISR (`(0x2070) = 0x030c`), `CALL 0750`, `RST 0x18`, `EI`, `RET`. The bootstrap image is now
   in host memory; the controller signals completion by interrupt. **The Z80 does *not* jump
   into the bootstrap** — the ND-100 CPU (its LOAD microcode) transfers control to the loaded
   image.

```
 press LOAD (ND-100 CPU)                3112 Z80 firmware
        │                        ┌──────────────────────────────────────────┐
        │  control word (bit2/8) │ HostCmd_ISR @030c                         │
        └───────────────────────►│   dispatch → Autoload_BootstrapLoad @1ae8 │
                                  │   1. RESTORE track 0 (FD1797 op 0x00)     │
                                  │   2. read 2048 B → buffer 0x2200          │
                                  │   3. CPIR for '!' (0x21)  ──no──► oct 50  │
                                  │   4. CR + octal digit?    ──no──► oct 50  │
                                  │   5. parse header → host addr + count     │
                                  │   6. wrong-format ('2'…'@'…='R')► oct 51  │
                                  │   7. DMA image to host in 0x800 chunks    │
                                  │   8. re-arm ISR, raise completion int     │
        ┌───────────────────────◄┤      (Z80 does NOT jump into bootstrap)   │
   ND-100 LOAD microcode          └──────────────────────────────────────────┘
   jumps into loaded image
```

## 4.4 The autoload error codes  [MANUAL §3.9/§3.10 + FIRMWARE]

| Octal | Meaning | Firmware trigger |
|-------|---------|------------------|
| 50 | **No bootstrap found on diskette** | `'!'` not in the first 2048 bytes; or no CR within 128 bytes before `'!'`; or the char before CR is not an octal digit `'0'..'7'`. Code byte `0xA8` @`ram:1ec8`, hard-error class. [FIRMWARE] |
| 51 | **Wrong bootstrap (out-of-date FLO-MON version)** | `'!'` valid but header matches the `'2'…'@'…checksum='R'` pattern. Code byte `0xA9` @`ram:1eca`, hard-error class. [FIRMWARE] |
| 53 | **Error during Autoload** | Listed in the manual §3.9; a generic autoload failure. |

### How the firmware builds the error image (verified @1f2f-1f6f)

On any autoload failure the firmware constructs the ND-100 error image byte-for-byte and DMAs
it to the host first page, where the ND-100 runs it from word 0:

1. **Always** copy the **0x3A-byte LOAD-ERROR image** from ROM `@1a92` to the buffer. It is a
   self-contained native ND-100 print routine (13 words of code) followed by the text
   `FF SI CR LF "  ** LOAD-ERROR:    00 **" CR LF '` (the `'` = `0x27` is the string terminator).
2. Read the error code (`status_hi >> 1`) and branch:
   - **Wrong-bootstrap (51 oct / `0x29`):** overlay the **0x1C-byte** text
     `"** WRONG BOOTSTTRAP ! **"` (ROM `@1acc`; note the firmware's `BOOTSTTRAP` typo) at
     **byte offset 0x39** — overwriting the LOAD-ERROR terminator, so the printed output is
     *both* lines. Total image = 0x56 bytes. The digits are left as literal `00`.
   - **Any other code:** patch the two octal error digits at **byte offset 0x32/0x33**
     (`AND 7; ADD '0'`). Total image = 0x3A bytes.
3. DMA the byte buffer to the host as big-endian 16-bit words (the card's byte→word PAL),
   starting at word 0; the ND-100 LOAD microcode runs it.

> **Implementation status.** Both emulators now reproduce this **byte-for-byte** on boot
> failure (RetroCore C# `NDBusFloppyDMA.DmaAutoloadErrorImage`; nd100x C
> `DmaAutoloadErrorImage`). The image bytes are verified against the ROM hexdump and the DMA is
> unit-tested (the C# test decodes the DMA'd words back to `"...LOAD-ERROR:...50..."`). The
> entry point word 0 is inferred from the image layout (code at the start); an end-to-end
> "boots, message appears on the console" integration test is **not** part of this change.

The captured ND-100 memory images for these two failure paths are preserved as front-panel
`DEPOSIT` scripts in the Ghidra project folder — they are the ground truth for the operator-
visible messages:
- `E:\Dev\Repos\Ronny\RetroGhidra\N100-FLOPPY-3112\ND Code\Load_error.txt` — the
  "`** LOAD-ERROR: nn **`" image (oct 50 path).
- `E:\Dev\Repos\Ronny\RetroGhidra\N100-FLOPPY-3112\ND Code\wrong_bootstrap.txt` — the
  "`** WRONG BOOTSTRAP ! **`" image (oct 51 path).
- `E:\Dev\Repos\Ronny\RetroGhidra\N100-FLOPPY-3112\ND Code\DEPOSIT 0 77400.txt` — a working
  bootstrap memory image (octal words at octal addresses 0–310).
- `E:\Dev\Repos\Ronny\RetroGhidra\N100-FLOPPY-3112\ND Code\convert.cs` — the helper that
  turns captured Z80 fragments into ND-100 `DEPOSIT` lines (its embedded strings contain both
  the LOAD-ERROR and WRONG-BOOTSTRAP text).

## 4.5 No hardware LOAD-button path on the card  [FIRMWARE — verified]

The reset vector reads `IN A,(0x61)` at `ram:0000`, but the value is **immediately discarded**
(`LD A,0x1` at `ram:0002` overwrites A before it is used, then `OUT(0x74),A`). Reset then
falls through to FD1797/interface init. **There is no power-on or card-side LOAD-button
autoload.** Autoload happens **only** when the ND-100 asserts the autoload control word to the
card. [FIRMWARE @0000-0006]

This matters for an emulator: you do **not** need to model a card-local boot trigger. Model
the control word, and let the ND-100 side (microcode / the emulator's LOAD handling) assert
it.

## 4.6 What an emulator needs for boot  [INFERENCE, grounded in the above]

Minimum to make "boot from floppy" work at the IOX/functional level (i.e. without running the
real Z80 firmware):

1. Accept the **autoload control word** (bit 2, State I — and, given the ambiguity in §4.2,
   also accept bit 8 "fetch & execute" as the trigger the ND-100 microcode may use).
2. Read the diskette's first sectors into a buffer and locate the BPUN bootstrap: find the
   `'!'` (`0x21`) signature; require a CR (`0x0D`) within the preceding 128 bytes and an octal
   digit just before it. On failure, complete with **error 50₈**.
3. Reject an out-of-date monitor with **error 51₈** if you choose to model the version check
   (the `'2'…'@'…='R'` pattern); otherwise skip it.
4. Parse the BPUN header to get the **ND-100 load address** and **word count**, DMA the image
   into ND-100 memory at that address, then **complete with an interrupt** — do **not** start
   execution yourself; the ND-100 LOAD microcode does that.

> **[COULD NOT DETERMINE]** The precise BPUN header field arithmetic (`ram:1b52-1b82`) that
> yields the exact numeric load address and count. If a functional emulator needs bit-exact
> addresses, either (a) run the real firmware, or (b) parse the BPUN format from its own
> specification. The working `DEPOSIT 0 77400.txt` image is a concrete example to validate
> against (its word 0 = `77400₈`, addresses 0–310₈).
