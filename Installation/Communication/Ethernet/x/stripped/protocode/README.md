# protocode - ND Ethernet II (PCB 3094) 68000 firmware behavioral model

This folder is a **high-level behavioral translation** of the ENCOS 68000 server
firmware (`encos-ser-all-banks-68k.bin`) into readable C#. It is **not** a
cycle-accurate MC68000 emulator and it does not execute the firmware binary. Its
purpose is to document, in executable form, the protocol-level logic that was
reverse-engineered from the disassembly:

- 68000 reset / startup sequence
- hardware + LANCE initialization
- ND-100 <-> 68000 shared-memory postbox/mailbox protocol
- 68000 -> ND-100 result posting and the SCIP (INT12) doorbell
- LANCE RX/TX descriptor ownership and packet flow
- interrupt routing (ND-100, MFP, LANCE, 68000)
- error / warm-boot / bus-error paths

Full analysis: `../docs/ND_EthernetII_68000_Firmware_ReverseEngineering.md`
Quick reference: `../docs/ND_EthernetII_68000_Firmware_QuickMap.md`

## Namespace and build

- Namespace: `RetroCore.ND.EthernetII.ProtoCode`
- Target: modern C# (nullable enabled), no external NuGet packages, no LINQ.
- Drop the `.cs` files into any C# project (or `dotnet new classlib`) to compile.

## What is CONFIRMED vs provisional

The code is annotated inline. Confidence markers:

- **CONFIRMED** - proven in this image's disassembly or in the authoritative
  RetroCore C# host emulator (`NDBusEthernetII.cs`). Examples: reset entry 0x1CFE,
  the SCIP doorbell (0xEF0080 / 0xEF0180 -> INT12), the LANCE CSR bring-up
  (RAP/RDP, CSR3=BSWP, init block 0x18810, CSR0=INIT), the TX kick CSR0=0x0048,
  the monitor postbox at 0x40A, the register-dump frame at 0x454, the warm-boot
  magic 0x55555555 at 0x4BA. Added after the function-renaming pass:
  MFP init at 0x396A with **VR=0x40** (writes MC68901 base 0xEF00C0), the RTC/timer
  interrupt handler at 0x3A68, the Ethernet **CRC-32** routine at 0x4660
  (polynomial 0xEDB88320), **1520-byte RX buffers** (0x5BCA), and the 8-entry
  **XROUT dispatch table** at 0x1D170 (handlers 0x99E2..0x9D8E).
- **HYPOTHESIS** - strongly suggested by the code but not fully traced. Examples:
  the XMSG postbox ring producer at 0xEACC (8-slot ring, per-slot owner word,
  SCIP mirror doorbell); the exact semantics of XROUT handlers 2..7.
- **UNCONFIRMED / TODO_REVERSED_DETAIL** - taken from the task brief or external
  docs, or a place where a structure/field is not yet dumped. Examples: the
  diagnostic command mailbox map (0x400/0x440/0x880), the final runtime MODE value
  and ring lengths in the LANCE init block, and who writes the MAC address into
  0x1885E (the block is zero in the static image - built at runtime, most likely
  by the ND-100/SINTRAN host).

### Packet paths (fully reversed and verified)

Both directions are now modelled from the actual 68K code, not abstracted:

- **Receive** (RCVRINGAPPEND 0x5B60 + RCVCOMPLETE 0x5C42): the LANCE address-filters
  the frame (promiscuous / broadcast / our PADR / multicast), DMAs it into the next
  chip-owned 8-byte RX descriptor's buffer at 0x18008, writes MCNT (= frame + 4-byte
  FCS), clears OWN, sets STP/ENP, and raises RINT. Ring = 128 descriptors, wrap
  mod 128, ending exactly at the TX ring 0x18408.
- **Transmit** (XMTRINGAPP 0x6054): `BuildTxFrame` writes the Ethernet header into
  the TX buffer with the SOURCE MAC taken from our PADR (0x1885E), pads short frames
  to 60 bytes, builds the TMD at 0x18410 with OWN, and the CSR0 TDMD poke (0x0048)
  makes the chip DMA the buffer to the wire, clear OWN, and raise TINT.

The firmware consumer side is modelled too: the level-2 LANCE interrupt (raw ISR
0x211C is a thin PLANC trampoline) drives `ProcessRxComplete` (RCVCOMPLETE 0x5C42)
and `ProcessTxComplete` (XMTCOMPLETE 0x61D2). On RINT the firmware reads each
released RX descriptor, strips the 4-byte FCS, pulls the destination MAC, then does
a SECOND (software) address check exactly like RCVCOMPLETE at 0x5D5E - it compares
the destination MAC byte-for-byte against our address at 0x1885E (group/multicast
addresses handled separately). Only frames addressed to us are delivered: they
raise `NDEthernetIIController.OnFrameReceived` AND ring the ND-100 doorbell (SCIP ->
INT12) via `PostReceivedFrameToHost`. On TINT it reclaims transmitted descriptors.

So there are TWO MAC filters, matching the hardware: the LANCE address filter
(hardware, before DMA) and the firmware's software recheck (RCVCOMPLETE). In
promiscuous mode the hardware passes everything and the software recheck still gates
delivery. The full vertical slice runs against real shared memory:
wire -> LANCE hw filter -> chip DMA -> ring -> RINT -> firmware RCVCOMPLETE (sw MAC
recheck) -> host event + SCIP doorbell to the ND-100 (INT12); and the reverse
host -> firmware XMTRINGAPPEND -> ring -> TDMD -> chip DMA -> wire -> TINT reclaim.

The XMSG message that carries a received frame up to the ND-100 is now modelled
from XMRECEIVER (0xBED8): `PostReceivedFrameToHost` builds an `XmsgMessage` with the
CONFIRMED header fields - flags 0x4000, id from `xmsg_node_id` (0x1E21A), subtype 4,
payload = the frame - hands it to the XMSG layer (XMPFRRE 0x10C4C), and rings SCIP
(INT12). The message is exposed as `NDEthernetIIFirmware.LastXmsgMessage`. The
CONFIRMED part is the message header the firmware assembles; the exact on-wire XMSG
framing the ND-100 decodes off the postbox is documented in the repo's XMSG protocol
notes and is not re-derived here.

Multicast is filtered with the standard Am7990 LADRF hash: CRC-32 of the 6
destination bytes, top 6 bits index the 64-bit LADRF in the init block, accept if
that bit is set. This is documented chip behaviour; this firmware's own
multicast-add routine (which sets the LADRF bits with the same hash) is not wired
to a caller in the static image, so the hash orientation is standard-chip rather
than firmware-proven - flagged in code with `Trace.Unconfirmed`.

Every provisional spot calls `FirmwareTrace.Unconfirmed(...)` or is tagged with a
`TODO_REVERSED_DETAIL` comment, so you can audit the model's honesty by grepping
for `Unconfirmed` and `TODO_REVERSED_DETAIL`.

## Important correction baked into the model

The command-mailbox map in the task brief (semaphore-per-field at 0x400/0x440/0x880
with a numeric test-dispatch table at 0x948) belongs to the **bank-0 diagnostic
firmware**, not this production server image. In this image:

- 0x40A is the monitor/console postbox (see `Mailboxes.cs` -> `MonitorPostbox`).
- 0x454 is the CPU register-dump frame.
- The message path uses an 8-slot postbox ring with a per-slot owner word.

The diagnostic `MailboxAddress` enum is retained for completeness but flagged as
unconfirmed for this image.

## File map

| File | Responsibility |
|------|----------------|
| `FirmwareTrace.cs` | tracing helpers (Info/IoRead/IoWrite/Mailbox*/Interrupt/Unconfirmed) |
| `FirmwareConstants.cs` | confirmed addresses, I/O regs, LANCE CSRs, MFP vectors |
| `SharedMemory.cs` | 512 KB DRAM + mirror, big-endian read/write helpers |
| `Mailboxes.cs` | diagnostic mailbox enum (provisional) + monitor postbox (confirmed) |
| `BufferDescriptors.cs` | XMSG postbox slot/ring (confirmed shape), LANCE descriptors |
| `MfpControllerModel.cs` | MC68901 GPIP/USART/RTC vector model |
| `LanceControllerModel.cs` | Am7990 RAP/RDP/CSR + RX/TX byte-array hand-off |
| `InterruptController.cs` | ND-100/MFP/LANCE/68000 routing + SCIP doorbell |
| `FirmwareCommandDispatcher.cs` | dispatch table built from discovered PLANC routines |
| `FirmwareCommands.cs` | one handler per discovered routine + HostCommand/HostResult |
| `NDEthernetIIFirmware.cs` | reset flow, init, main-loop step, interrupt handlers |
| `NDEthernetIIController.cs` | top-level model + host-facing API |

## Minimal usage sketch

```csharp
var ctrl = new NDEthernetIIController(Console.WriteLine);
ctrl.OnInterruptToNd100 += () => { /* ND-100 sees INT12 */ };

// ND-100 loads firmware image into shared DRAM, then releases the 68000.
ctrl.Memory.LoadImage(imageBytes);
ctrl.ReleaseFromReset();          // runs reset_entry (0x1CFE) + INITLANCE

ctrl.HostWriteCommand(new HostCommand(0x0000, 0, 0)); // INITLANCE
ctrl.Tick();
HostResult? r = ctrl.HostReadResult();

ctrl.ReceiveEthernetFrame(frameBytes); // LANCE RX -> level-2 IRQ
ctrl.Tick();
```
