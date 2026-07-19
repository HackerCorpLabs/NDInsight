# SCSI-TRACE How-To — Capturing an ENTER-DIRECTORY (mount) transaction

**Purpose**: Toggleable `[SCSI-TRACE]` logging in the RetroCore ND-100 SCSI emulator so
the full mount transaction (SINTRAN `ENTER-DIRECTORY`) can be captured to disk and the
"SCSI disk won't mount" failure diagnosed.

**Provenance**: [SCSI-MOUNT-FIX-PLAN.md](SCSI-MOUNT-FIX-PLAN.md)

---

## 1. What was instrumented

Trace lines are all prefixed `[SCSI-TRACE]` and are guarded by a single `if (ScsiTrace)`
flag, so they cost nothing when disabled. They live in two C# files (outside NDInsight):

| Area | File | Method |
|------|------|--------|
| IOX register R/W boundary | `Emulated.HW/ND/CPU/NDBUS/NDBusDiscControllerSCSI.cs` | `Read(int)`, `Write(int,ushort)` |
| NCR completion interrupt | `NDBusDiscControllerSCSI.cs` | `Ncr5386_OnInterrupt`, `StepGoState` |
| RITRG interrupt ack | `NDBusDiscControllerSCSI.cs` | `Read` (case `RITRG`) |
| DMA -> ND memory summary | `NDBusDiscControllerSCSI.cs` | `StepGoState` (DMA drain loop) |
| SCSI CDB dispatch | `Emulated.HW/Common/SCSI/SCSIHDD.cs` | `scsi_command` |
| READ CAPACITY response | `SCSIHDD.cs` | `CommandReadCapacity` |
| MODE SELECT 6 | `SCSIHDD.cs` | `scsi_command` (`SC_MODE_SELECT_6`) |
| Block data + hex dump | `SCSIHDD.cs` | `readBlock` / `TraceBlock` |

The flag itself and the hex-dump helper live on the SCSI base class
`Emulated.HW/Common/SCSI/SCSIDevice.cs` (`public static bool ScsiTrace`, `HexDump(...)`),
so a single toggle drives both the controller and the disk traces.

---

## 2. How to enable

### 2a. Turn the trace flag on

Set this once at startup (or from the debugger immediate window):

```csharp
NDBusDiscControllerSCSI.ScsiTrace = true;
```

That property forwards to `SCSIDevice.ScsiTrace`, so both the controller and disk traces
switch on together. (Equivalently you may set `SCSIDevice.ScsiTrace = true` directly.)

### 2b. Make the log actually appear

`[SCSI-TRACE]` lines are emitted at log level **Device**, and the logger is **Disabled**
by default. You must lower the level to `Device` (or below) AND pick a destination:

```csharp
// Easiest: file output
Logger.EnableLogger(Logger.LogLevel.Device, Logger.LogDestination.File);

// Or console
Logger.EnableLogger(Logger.LogLevel.Device, Logger.LogDestination.Console);
```

### 2c. Where the log goes

- **File** destination writes to: `%LOCALAPPDATA%\trace\file-trace.txt`
  (appended; delete it first for a clean capture). Each line is prefixed
  `Device; SCSI Bus N: [SCSI-TRACE] ...`.
- **Console** destination prints to stdout.
- **Trace** destination is viewable with SysInternals DebugView.

To capture one mount: delete the old `file-trace.txt`, enable tracing, run
`ENTER-DIRECTORY,,DISC-SCSI-1,0` in SINTRAN, then inspect the file.

---

## 3. Reading the trace — GOOD mount vs BAD mount

The mount reads the master block (LBA 0). The decisive datum is **block 0**: at byte
2016 SINTRAN expects a printable volume/master-block name, with the block-0 checksum at
byte 2000. (Note: a single ND sector is **1024 bytes**, so the `[2000..2047]` dump only
appears if the returned buffer is >= 2048 bytes; otherwise a "block0 len < 2048" note is
logged instead — that in itself tells you the read granularity.)

### Key lines to grep

```
grep "SCSI-TRACE" file-trace.txt
```

Then focus on these four:

| Grep for | GOOD mount | BAD mount |
|----------|-----------|-----------|
| `READ CAPACITY ->` | `blockSize=1024` (ND requires 1024-byte sectors) | `blockSize=512` or other -> wrong geometry, mount rejected |
| `readBlock lba=0 bytes[0..31]=` | plausible structured header, not all `00`/`FF` | all zeros or all `FF` = empty/garbage medium |
| `block0 bytes[2000..2047]=` (or the `len<2048` note) | printable ASCII around offset 2016 (the master-block name) | random bytes / zeros = page 0 garbage -> the root cause of a failed mount |
| `completion:` + `RITRG ack:` | completion fires (`-> SetInterruptBit`), then `RITRG ack: cleared intFromNCR` | completion never logged, or `no IRQ (int disabled)` -> lost IRQ, RSTAU-poll loop, mount timeout |

### The RSTAU / RITRG interrupt pattern

A healthy transaction shows, in order:

1. `IOX W reg=WCONT ...` with the active bit -> command starts.
2. Repeated `IOX R reg=RSTAU ...` polls (SINTRAN waiting for the transfer).
3. `NCR interrupt raised ...` when the NCR5386 finishes.
4. `completion: active->false rft->true intEnabled=True -> SetInterruptBit`.
5. `IOX R reg=RITRG ...` followed by `RITRG ack: cleared intFromNCR`.

If step 3/4/5 never appear while RSTAU polling continues forever, the completion IRQ is
being lost — that is the classic mount-timeout / BUS-RESET loop this trace was built to
catch. (The interrupt is acknowledged on RITRG, never on RSTAU — see fix #1 in the plan.)

### The DMA payload

`DMA->ND xfer bytes=... first16=...` shows how many bytes reached ND memory and the first
16. For the master block read this should be a full sector; `bytes=0` or a truncated count
means the data phase stalled.

---

## 4. Turning it off

Set `NDBusDiscControllerSCSI.ScsiTrace = false;` (the default). No rebuild toggle needed —
it is a runtime flag.
