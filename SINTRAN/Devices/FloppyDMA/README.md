# Floppy and Streamer Controller 3106 / 3112 — Controller Software Documentation

**Device:** Norsk Data Floppy and Streamer Controller, card no. **3106** (8" only, ND-630)
and card no. **3112** (8" + 5¼", ND-317).
**Bus:** ND-100 I/O bus (IOX + DMA), device numbers **1560₈** (unit 0) / **1570₈** (unit 1).
**On-card processor:** Zilog **Z80A** + **AM9517A** DMA controller + **FD1797** floppy
controller + **Z80-CTC** timer/interrupt controller. QIC-02 streaming-tape interface.

This documentation set describes the controller **from the software/programmer point of
view** and **from the reverse-engineered on-card Z80 firmware** (`34300G.bin`), so that the
floppy half of the controller can be (re-)implemented in an emulator. The **streamer/tape**
half is documented in its own file so it can be implemented later as a separate work item —
today only the floppy part is emulated.

## Authoritative sources

1. **The manual** — `ND-11.021.1 EN — Floppy and Streamer Controller 3106/3112`
   (August 1984). Located at
   `SINTRAN/Devices/SCSI/ND-11.021.1 EN-Floppy and Streamer Controller 3106 3112.md`.
   This is the primary written authority; every register/status/command table here is
   quoted from it with section references (§3.x, §4.x, §5, §6, §7).
2. **The on-card firmware** — `34300G.bin`, the Z80 EPROM, reverse-engineered in the Ghidra
   project `E:\Dev\Repos\Ronny\RetroGhidra\N100-FLOPPY-3112\ND-FLOPPY-3112.gpr`. Where the
   firmware's *actual behaviour* differs from, or is more specific than, the manual, that is
   called out explicitly. **When the firmware and a secondary source disagree, the firmware
   wins** (it is what the real hardware does).
3. **Cross-checks** — the RetroCore C# model
   (`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusFloppyDMA.cs`), the nd100x C
   model (`~/repos/nd100x/src/devices/floppy/deviceFloppyDMA.c`), and the Verilog core
   (`nd-120/Verilog/ND-BUS-DEVICES/FLOPPY-DMA/`). These are implementations, not authorities;
   they are cited only to flag where an existing emulator is right or wrong.

## Reading order

| File | Contents |
|------|----------|
| [`01-overview-and-architecture.md`](01-overview-and-architecture.md) | What the card is, block diagram, the mailbox model, on-card chips and memory map. |
| [`02-programming-interface.md`](02-programming-interface.md) | IOX registers, the Command Block, Status Word 1 / Status Word 2, the Hardware Status Word, the Hardware Control Word, the Command Word. **The core register reference.** |
| [`03-floppy-commands.md`](03-floppy-commands.md) | Every floppy command (Read/Write/Format/…), its command-word code, and its semantics. |
| [`04-boot-and-autoload.md`](04-boot-and-autoload.md) | **How boot mode works**: DMA-load vs LOAD-button BPUN load, control-word bit 2, the on-card bootstrap loader, and errors 50/51/53. |
| [`05-floppy-formats.md`](05-floppy-formats.md) | Every supported diskette format (3106 and 3112), sector sizes, sides, density, capacities. |
| [`06-error-codes.md`](06-error-codes.md) | The complete octal error-code table (§3.9/§3.10) with per-code explanations and where the firmware raises each. |
| [`07-firmware-internals.md`](07-firmware-internals.md) | The Z80 firmware itself: reset/self-test, the command loop, the FDC ISR, the `RST 08h`+code error-report mechanism, port map — the reverse-engineering results. |
| [`08-streamer-tape.md`](08-streamer-tape.md) | **The QIC-02 streaming-tape half, in its own section** — commands, continuous transfer, extended status. For future implementation. |
| [`09-testing-and-test-macros.md`](09-testing-and-test-macros.md) | Self-test phases (§6), the 25 test macros (§7), and the up/down-load address field. |
| [`10-implementation-guide.md`](10-implementation-guide.md) | Concrete guidance + a divergence table for anyone implementing/fixing the floppy model (RetroCore C#, nd100x C, Verilog). |

## The one thing to get right first (two status words)

The single most common implementation bug — present in **both** the current nd100x C model
and (partly) the RetroCore C# model — is conflating **two different status words**:

- **Hardware Status Word** — returned by `IOX +2` *and* `IOX +4` (they are duplicates,
  §3.1 Note 1). Carries the live handshake/error-summary flags and **bit 15 = "dual density
  controller" (always 1 — this is how SINTRAN detects a 3112/3106 DMA card)**. It carries
  **no numeric error code.** (§3.7)
- **Status Word 1** — written back by DMA into the host's Command Block at **CB+6**. Carries
  the flag bits *and* the **numeric error code in bits 9–14**, with bit 15 clear. (§3.4)

The firmware confirms the numeric code is placed as `(code & 0x3F) << 1` in the high status
byte (LSB at bit 9). See [`02-programming-interface.md`](02-programming-interface.md) and
[`10-implementation-guide.md`](10-implementation-guide.md).

> **Provenance note.** Sections marked **[MANUAL §x]** are quoted from the ND-11.021.1 manual.
> Sections marked **[FIRMWARE @addr]** are verified in the Z80 ROM at the given Ghidra address.
> Sections marked **[INFERENCE]** are reasoned conclusions not directly stated by either;
> they are flagged so they can be challenged. Nothing here is presented as fact unless it is
> sourced.
