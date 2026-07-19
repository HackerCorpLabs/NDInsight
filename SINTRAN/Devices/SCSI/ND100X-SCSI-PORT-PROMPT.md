# Porting Prompt: RetroCore C# SCSI Controller -> nd100x C

You are an expert C systems programmer working inside the **nd100x** emulator repository
(WSL path `~/repos/nd100x`). Your task is to **port the RetroCore C# SCSI disk controller
and its SCSI bus/device/disk stack into C**, in the exact style of the existing nd100x
device drivers, so that the ported controller reads and writes disk blocks the same way the
existing **SMD** controller does. The port must be **byte-accurate** so it can be validated
against RetroCore's known-good behavior.

You are doing the port. This document is self-contained: it embeds the concrete inventory of
the C# source, the concrete analysis of the nd100x C target style, a file-by-file port plan,
a byte-level accuracy checklist, a validation plan, and the project constraints. You should
still open and confirm each referenced file before you rely on it, but you should not need any
other context to begin.

---

## 1. Goal

Add a new block device `deviceSCSI` to nd100x that emulates the ND-3201/3204 SCSI disk
controller (NCR-5386 SCSI protocol chip + DMA onto the ND-100 IO bus), driving a
Micropolis-1375-class ND SCSI hard disk. When wired in, SINTRAN III must be able to:

```
@ENTER-DIRECTORY,,DISC-SCSI-1,0     <- mounts the SCSI disk
@DIR                                <- lists the directory
```

This already works in RetroCore (C#) after a recent RDIV fix. Your C port must reproduce the
same on-wire SCSI behavior and the same block I/O so the SINTRAN mount + directory listing
succeeds identically.

**Prime directive:** FIRST study the existing nd100x SMD driver, adopt its structure, naming,
bus-registration, DMA, interrupt, and block read/write patterns, and mirror how the SMD
controller reads/writes disk blocks. Only THEN write the SCSI code. The SCSI controller must
work "similar to the SMD controller" from the machine's point of view (same block-callback
path, same DMA helpers, same interrupt mechanism), while decoding SCSI CDBs internally.

---

## 2. Source inventory (RetroCore C#)

Repo root: `E:\Dev\Repos\Ronny\RetroCore`. Study path: `Emulated.HW\Common\SCSI\` plus the ND
adapter under `Emulated.HW\ND\CPU\NDBUS\`. (Ignore any `.claude\worktrees` copies.)

Architecture: the ND adapter is a **dumb NCR-5386 register + DMA bridge**. It does NOT decode
SCSI CDBs. The Command Descriptor Block is assembled by SINTRAN's driver and interpreted by
the **target device class** (`SCSIHDD` / `SCSIHDDMicropolis`). Your C port collapses this into
one `deviceSCSI` that (a) presents the ND IOX register/DMA interface like the adapter and
(b) internally decodes the CDB like the target device. You do NOT need a faithful NCR-5386
register-level emulation for the acceptance test as long as the CDB decode + DMA + block I/O
match; but you MUST reproduce the register map and status/interrupt semantics SINTRAN's driver
depends on.

Active code path (the one that must be byte-accurate):
`NDBusDiscControllerSCSI` -> `SCSIBus` -> `NCR5386SCSI` -> `SCSIHDDMicropolis` : `SCSIHDD` :
`SCSIFullDevice` : `SCSIDevice`.

Note: `SCSIConfigurableHDD` / `SCSIDeviceConfig` / `ISCSIConfigurable` are a newer data-driven
rewrite whose config dependency is ABSENT from the repo (does not compile-link into the ND
path). Use it only as a cross-reference; the byte-source of truth is `SCSIHDD` +
`SCSIHDDMicropolis`.

### 2.1 ND adapter - `NDBusDiscControllerSCSI`
File: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusDiscControllerSCSI.cs`
Class: `NDBusDiscControllerSCSI : NDBusDeviceBase`. Emulates the ND-3201/3204 card. Creates the
`SCSIBus`, the `NCR5386SCSI` chip, and one `SCSIHDDMicropolis` per unit. Own SCSI ID = 7.
Interrupt level = **11** (disk output-channel). `NDBusAddressLength = 63`.

Thumbwheel TW2 selects IOX base / IDENT / logical device (octal):

| TW2 | IOX base (octal) | IDENT (octal) | SCSI bus | logical dev |
|-----|------------------|---------------|----------|-------------|
| 0/4/8/C | 144300 | 140440 | 1 | 2202 |
| 1/5/9/D | 144400 | 140441 | 2 | 2203 |
| 2/6/A/E | 144500 | 140442 | 3 | 2204 |
| 3/7/B/F | 144600 | 140443 | 4 | 2205 |

Register map (`Register` enum; offset from IOX base; even=read, odd=write):

| Off | Name | Meaning |
|-----|------|---------|
| 00 | RLMAR | read Memory Address Register bits 0-15 |
| 01 | WLMAR | write MAR bits 0-15 |
| 02 | REDAT | read data buffer (IOX PIO mode) |
| 03 | WRDAT | write data buffer (IOX PIO mode) |
| 04 | RSTAU | read status word |
| 05 | WCONT | write control word |
| 06 | RHMAR | read MAR bits 16-23 |
| 07 | WHMAR | write MAR bits 16-23 (`& 0xFF`) |
| 0x08/0x0A | RXWC_HI / RXWC | external word count (3204 only) |
| 0x20/0x21 | RNDAT/WNDAT | NCR data reg |
| 0x22/0x23 | RNCOM/WNCOM | NCR command reg |
| 0x24/0x25 | RNCNT/WNCNT | NCR control reg |
| 0x26/0x27 | RDESI/WDESI | destination ID |
| 0x28/0x29 | RAUXS/WAUXS | aux status |
| 0x2A/0x2B | ROIDN/WOIDN | own ID |
| 0x2C | RITRG | interrupt register (read clears NCR interrupt) |
| 0x2E | RSOUI | source ID |
| 0x32 | RDIST | diagnostic status |
| 0x38/0x39 | RTCM/WTCM | transfer counter |
| 0x3A/0x3B | RTC2/WTC2 | transfer counter 2 |
| 0x3C/0x3D | RTCL/WTCL | transfer counter low |

Status word (`RSTAU`, read): bit0 interrupt-enabled; bit2 active/busy; bit3 ready-for-transfer;
bit4 OR-of-errors (never set in emu); bit5 reset-on-SCSI-bus; bit6 NCR disabled; bit7
single-ended; bit8 data-request-from-NCR; bit9 interrupt-from-NCR; bit10 data-ack; bit11
BERROR (never); bit12 SCSI BSY; bit13 SCSI REQ; bit14 SCSI ACK; bit15 differential.
**Reading RSTAU does NOT clear the NCR interrupt** - the interrupt is acknowledged only when
`RITRG` (0x2C) is read (`InterruptFromNCR5386 = false`).

Control word (`WCONT`, write): bit0 enable interrupt; bit2 activate (GO); bit3 test mode; bit4
clear-device (zeros MAR + buffer pointers, resets NCR, sets ready-for-transfer); bit5 ND-100
DMA enable; bit6 write-ND-100-memory (**DMA direction: 1 = SCSI->ND memory, 0 = ND memory
->SCSI**); bit10 reset SCSI bus. Writing bit2=1 starts the transfer and clears ready-for-transfer.

DMA / byte packing (this is the load-bearing detail): the transfer drains all bytes of the
current SCSI phase. Bytes pack into 16-bit ND words **big-endian**: even byte index -> HIGH
byte of the word, odd byte index -> LOW byte. The MAR increments only after the low (odd)
byte. `WriteNextByteDMA` does read-modify-write of the target word to preserve the untouched
byte. The MAR is a **WORD address**; physical byte address = `MAR << 1`
(`DMABus.WriteMemory16(MAR<<1, data)` / `ReadMemory16(MAR<<1)`).

Completion is event-driven: when the NCR signals interrupt, the controller sets active=false,
ready-for-transfer=true, and raises interrupt level 11 if enabled.

### 2.2 `SCSIBus`
File: `...\Common\SCSI\SCSIBus.cs`. Port of MAME `nscsi_bus`; up to 16 devices; OR-ties data
and control lines; notifies devices on control-line changes. For the C port you may reduce this
to a minimal single-initiator/single-target model (the ND adapter is initiator ID 7, one disk
target), OR keep an explicit phase state machine. The acceptance test does not require the full
OR-tied multi-device bus; it requires correct phase sequencing SELECT -> COMMAND -> DATA -> STATUS
-> MESSAGE IN -> BUS FREE for one target.

### 2.3 `SCSIDevice` / `SCSIFullDevice`
Files: `...\Common\SCSI\SCSIDevice.cs`, `...\Common\SCSI\SCSIFullDevice.cs`.
`SCSIFullDevice` is the target-side **phase state machine** (`step()`), driving a `buf_control`
FIFO with actions `BC_MSG_OR_COMMAND`, `BC_STATUS`, `BC_DATA_IN`, `BC_DATA_OUT`, `BC_MESSAGE_1`,
`BC_BUS_FREE`. Key facts to port:
- `scsi_cmdbuf` = 4096-byte CDB + data staging buffer. `scsi_sense_buffer` = 18 bytes.
- CDB length by command group (top 3 bits of opcode): group0=6, group1=10, group2=10, group3=6,
  group4=16, group5=12, group6=6, group7=6.
- `scsi_data_in(buf, size)` queues a DATA IN phase; `scsi_data_out(buf, size)` queues DATA OUT;
  `scsi_status_complete(status)` queues STATUS + COMMAND-COMPLETE message + BUS FREE.
- Fixed-format sense (`set_sense_data`): byte0 = 0x70 or 0x71 (valid bit 0x80); byte2 = sense
  key + flags; byte7 = 10 (additional length); bytes12-13 = ASC/ASCQ big-endian.
- SBUF ids (`SCSIEnums.cs`): `SBUF_MAIN=0`, `SBUF_SENSE=1`, `SBUF_DATA=2`.
- Block storage is via callbacks: `BlockReadCallback(ControllerId, DeviceUnit, byteOffset, size)`
  returns `byte[]`; `BlockWriteCallback(ControllerId, DeviceUnit, byteOffset, byte[])`;
  `IsMountedCallback`.

### 2.4 `SCSIHDD` - the disk command handler (ACTIVE, byte-accurate)
File: `...\Common\SCSI\SCSIHDD.cs`. `SCSIHDD : SCSIFullDevice`. This is where you get the exact
CDB decode. `m_sectorData` = 1024-byte buffer. Geometry in `hdinfo`.

Command decode (`scsi_command()`) - EXACT parsing:

| Opcode | Name | LBA | Length | Action |
|--------|------|-----|--------|--------|
| 0x00 | TEST UNIT READY | - | - | GOOD if media else CHECK COND |
| 0x03 | REQUEST SENSE | - | cmd[4] | 18-byte sense (see below) |
| 0x04 | FORMAT UNIT | - | - | GOOD |
| 0x08 | READ(6) | `get_u24be(1) & 0x1FFFFF` (21-bit) | `cmd[4]`, 0->256 | read blocks, DATA IN `blocks*1024` |
| 0x0A | WRITE(6) | `get_u24be(1) & 0x1FFFFF` | `cmd[4]`, 0->256 | DATA OUT `blocks*1024`, write |
| 0x12 | INQUIRY | - | `cmd[4]` | see INQUIRY bytes below |
| 0x15 | MODE SELECT(6) | - | - | accept params, IGNORE block descriptor (sector size fixed 1024) |
| 0x1A | MODE SENSE(6) | - | - | mode header + block descriptor + pages |
| 0x1B | START STOP UNIT | - | - | GOOD |
| 0x25 | READ CAPACITY | - | - | see READ CAPACITY bytes below |
| 0x28 | READ(10) | `get_u32be(2)` | `get_u16be(7)` | read, DATA IN `blocks*1024` |
| 0x2A | WRITE(10) | `get_u32be(2)` | `get_u16be(7)` | DATA OUT `blocks*1024`, write |
| 0x2F | VERIFY(10) | - | - | GOOD |

Illegal / out-of-range fields -> CHECK CONDITION with sense key ILLEGAL_REQUEST + ASC 0x24
(INVALID FIELD IN CDB).

**READ CAPACITY (0x25) exact bytes** (`CommandReadCapacity`), 8-byte DATA IN:
```
bytes 0-3 = last LBA (big-endian u32) = DiskSizeInBlocks = cylinders*heads*sectors - 1
bytes 4-7 = block size (big-endian u32) = 1024
```
For the Micropolis-1375-ND: last LBA = 898*8*18 - 1 = **129311 (0x0001F91F)**, block size = **1024
(0x00000400)**. NOTE: the value must be the geometry-derived last LBA; reporting a
directory-derived value causes SINTRAN's completion handler (ECAPD) to raise
DISC-TRANSFER-ERROR / STATUS 100020B.

**INQUIRY (0x12) exact bytes** (`CommandInquiry`, page 0): buffer zero-filled then bytes 8..35
space-padded (0x20), then:
```
byte0 = 0x00   device type = direct-access disk
byte1 = 0x00   not removable
byte2 = 0x05   SPC-3
byte3 = 0x01   response data format = CCS
byte4 = 52     additional length
bytes 8-15  = vendor id   "NDMICROP"   (8 bytes, from SetInquireBuf for the ND disk)
bytes 16-31 = product id  "1375" (space-padded to 16)
bytes 32-35 = revision    "B0C" + pad
```
Returned length = `min(cmd[4], 56)`. (The generic default vendor/product used when no ND
metadata is set is SEAGATE ST225N - NOT used for the ND disk; use the NDMICROP/1375/B0C
identity.) A verified real-hardware INQUIRY exchange: CDB `12 00 00 00 24 00` -> 36 bytes.

**REQUEST SENSE (0x03)**: 18-byte sense, big-endian ASC/ASCQ. After DeviceReset the disk posts
UNIT ATTENTION (sense key 0x06, ASC 0x29 = POWER ON / RESET). Verified real trace:
`70 00 06 00 00 00 00 0A 00 00 00 00 29 00 00 00 00 00`.

**MODE SENSE(6) (0x1A)**: mode header + 8-byte block descriptor (block count = `put_u24be`
DiskSizeInBlocks, block length = `put_u24be` 1024) + pages 0x00/0x01/0x02/0x03/0x04/0x08/0x30.
Only needed if SINTRAN issues it during mount; reproduce faithfully if the trace shows it,
otherwise implement the header + block descriptor + rigid-geometry page 0x04 at minimum.

**The function-42 / control-record connect**: SINTRAN's ENTER-DIRECTORY mount reads the disk's
**last block** as the "control record" connect (fn-42). This is simply a READ of the last LBA
via the normal READ path - there is nothing special in the SCSI decode beyond correctly
answering READ CAPACITY (so SINTRAN computes the right last-LBA) and then serving the READ of
that block from the image. See the NDInsight note
`SINTRAN/Devices/SCSI/scsi-mount-last-block-is-control-record.md`: the last-block read IS the
fn-42 control-record connect (correct behavior). Do NOT describe it as a "geometry probe". The
historical mount bug was block 0 never being read; ensure block 0 and the last block both read
correctly from the image.

Block I/O physical path (`readBlock`/`writeBlock`):
```
location (byte offset) = (long)lba * sectorbytes          // sectorbytes = 1024
readBlock:  BlockReadCallback(ControllerId, DeviceUnit, location, sectorbytes) -> byte[1024]
writeBlock: BlockWriteCallback(ControllerId, DeviceUnit, location, byte[1024])
```
Multi-block transfers stream sector-by-sector: `scsi_get_data` recomputes
`clba = lba + pos/1024` and re-reads the block when the LBA changes, returning
`m_sectorData[pos % 1024]`. `scsi_put_data` mirrors it and flushes via `writeBlock` when
`offset == 1023`.

`hdinfo`: `ushort cylinders, byte heads, ushort sectors, ushort sectorbytes`. Defaults for the
ND disk: cyl=898, heads=8, sectors=18, sectorbytes=1024.
`DiskSizeInBlocks = cylinders*heads*sectors - 1` (this is actually the LAST LBA, note the -1).

### 2.5 `SCSIHDDMicropolis` - the concrete ND disk
File: `...\Common\SCSI\SCSIHDDMicropolis.cs`. `SCSIHDDMicropolis : SCSIHDD`. `MICORPOL_1375_ND`:
heads=8, cyl=898, sectors=18, sectorbytes=1024 -> 129312 sectors * 1024 = 132,415,488 bytes,
last LBA 129311. Identity NDMICROP / 1375 / B0C. Overrides: INQUIRY (rebuilds buffer then
delegates to base), RESERVE(6) 0x16 -> GOOD, SEEK(6) 0x0B (`lba = get_u24be(1) & 0x1FFFFF`,
range-checked), vendor 0x0C = INIT DRIVE PARAMS (`scsi_data_out` 8 bytes). `DeviceReset` posts
UNIT ATTENTION (ASC 0x29).

### 2.6 Supporting files
- `...\Common\SCSI\SCSISupport.cs`: `Buffer` wrapper with big-endian accessors `put_u16be`,
  `put_u24be`, `put_u32be`, `get_u16be`, `get_u24be`, `get_u32be`, plus `fill`, `setText`.
  Port these as small C helpers (or reuse nd100x's existing big-endian word helpers).
- `...\Common\SCSI\SCSIEnums.cs`: `SCSICommands` opcodes (as tabulated above), `SCSIControl`
  bus-signal bits, `SCSIStatus`, sense keys, ASC codes, `SBUF`.
- `...\Common\SCSI\SCSIHDDImage.cs`: internal raw-image reader (seeks `cur_lba*sectorbytes`).
  NOT used by the ND callback path - block I/O flows through the block callbacks. In nd100x the
  equivalent is the machine block-callback layer (see section 3), so you do NOT port this file.

---

## 3. Target-style analysis (nd100x C) - DO THIS FIRST

Repo root (WSL): `~/repos/nd100x` = `/home/ronny/repos/nd100x`. Read with
`wsl.exe bash -lc '...'` (cat/sed/grep). **Before writing any SCSI code, read and internalize
the SMD driver and the device framework.** Confirm every path and signature below against the
actual files - they were captured from the current tree but verify before relying on them.

### 3.1 The Device model (framework)
`~/repos/nd100x/src/devices/devices_types.h` (approx lines 129-172) defines a single `Device`
struct with function pointers (no C++ classes). Private state lives in `void *deviceData`.
```c
typedef struct Device {
    uint32_t startAddress;      // IOX base (octal)
    uint32_t endAddress;        // startAddress + 7 for an 8-register device
    uint16_t interruptBits;     // pending IRQ bitmask (levels 10-13)
    uint16_t interruptLevel;    // default IRQ level (disk = 11)
    uint16_t identCode;         // IDENT response (octal)
    uint16_t logicalDevice;
    DeviceType type;
    char memoryName[MAX_DEVICE_NAME];
    DelayedIoInfo *ioDelays; int ioDelayCount; int ioDelayCapacity;
    void     (*Reset)(struct Device *self);
    uint16_t (*Tick)(struct Device *self);                    // returns interruptBits
    int      (*Boot)(struct Device *self, uint16_t device_id);
    uint16_t (*Read)(struct Device *self, uint32_t address);   // IOX read handler
    void     (*Write)(struct Device *self, uint32_t address, uint16_t value); // IOX write
    uint16_t (*Ident)(struct Device *self, uint16_t level);
    void     (*Destroy)(struct Device *self);
    DeviceClass deviceClass;    // use DEVICE_CLASS_BLOCK
    size_t blockSizeBytes;      // 1024 for the ND SCSI disk
    CharacterDeviceCallbacks charCallbacks;
    BlockDeviceCallbacks blockCallbacks;   // read/write/diskInfo -> machine layer
    void *deviceData;
} Device;
```
Block callback signatures (same header): `size` = number of BLOCKS, `blockAddress` = LBA in
blocks, block size from `device->blockSizeBytes`:
```c
typedef int (*BlockDeviceReadFunc )(struct Device*, uint8_t *buffer, size_t size, uint32_t blockAddress, int unit);
typedef int (*BlockDeviceWriteFunc)(struct Device*, const uint8_t *buffer, size_t size, uint32_t blockAddress, int unit);
typedef int (*BlockDeviceDiskInfoFunc)(struct Device*, size_t *image_size, bool *is_write_protected, int unit);
```

### 3.2 Key framework functions (use these; do NOT reinvent)
| Purpose | Function | File |
|---|---|---|
| IOX read/write dispatch | `io_op` -> `IO_Read/IO_Write` -> `DeviceManager_Read/Write` -> `Device_Read/Write` -> `dev->Read/Write` | `src/machine/io.c`, `src/devices/devicemanager.c`, `src/devices/device.c` |
| DMA write word to ND mem | `void Device_DMAWrite(uint32_t coreAddress, uint16_t data)` | `src/devices/device.c` (~916) |
| DMA read word from ND mem | `int32_t Device_DMARead(uint32_t coreAddress)` | `src/devices/device.c` (~922) |
| Raise interrupt | `Device_GenerateInterrupt(Device*, uint16_t level)` / `Device_SetInterruptStatus(Device*, bool, level)` | `src/devices/device.c` (~800/814) |
| Clear interrupt | `Device_ClearInterrupt(Device*, level)` | `src/devices/device.c` (~785) |
| Deferred completion IRQ | `Device_QueueIODelay(Device*, ticks, IODelayedCallback, param, irqlevel)` | `src/devices/device.c` (~734) |
| Block read (-> image) | `Device_ReadBlock` -> `blockCallbacks.readFunc` = `machine_block_read` | `src/devices/device.c` (~1004), `src/machine/machine.c` (~780) |
| Block write (-> image) | `Device_WriteBlock` -> `machine_block_write` | `src/machine/machine.c` (~857) |
| Disk info (size/WP) | `machine_block_disk_info` | `src/machine/machine.c` (~899) |
| Big-endian word pack/unpack in a byte buffer | `Device_IO_BufferReadWord` / `Device_IO_BufferWriteWord` | `src/devices/device.c` (~836-908) |
| Register offset from IOX addr | `Device_RegisterAddress` | `src/devices/device.c` |
| Device init | `Device_Init(dev, thumbwheel, DEVICE_CLASS_BLOCK, sectorBytes)` | `src/devices/device.c` |

`Device_DMAWrite`/`Device_DMARead` set `gDMAAccess=true` around
`WritePhysicalMemory`/`ReadPhysicalMemory(addr & 0xFFFFFF, false)` so the transfer bypasses
MMU shadow/page checks. **Interrupt levels are restricted to 10-13** (the bit is
`1 << level` in `interruptBits`). Disk uses level **11** (matches RetroCore).

### 3.3 The SMD controller - your structural template
`~/repos/nd100x/src/devices/smd/deviceSMD.c` and `deviceSMD.h`; geometry helper
`diskSMD.c/.h` (only `DiskSMD_SetDiskType` lives there - block transfer logic is in
`deviceSMD.c`, NOT diskSMD.c). Also read `src/devices/floppy/deviceFloppyDMA.c/.h` as a second
example.

SMD IOX register map (base 01540 for thumbwheel 0; register = `addr - startAddress`; even=read,
odd=write; several registers multiplexed by control-word bit 15 `registerMultiplexBit`):
```
0 read Core Address / write Load Memory Address low
1 write Load Core Address / Count Mem+WC (test)
2 read Seek Condition / ECC Count
3 write Load Block Address I / II
4 read Status Register / ECC Pattern
5 write Load Control Word
6 read Block Address I / II
7 write Load Word Counter / ECC Control
```
Block address encoding: `blockAddressI` bits 0-7 = sector, bits 8-15 = head; `blockAddressII` =
cylinder. SMD status/control words are `union { uint16_t raw; struct { ... :1; } bits; }`.
**Important header gotcha (deviceSMD.h ~line 143): every bitfield member must be typed
`uint16_t` (not a mixed/enum type), otherwise `-mms-bitfields` on Windows/MinGW splits the
storage unit and breaks the `raw` overlay. Follow the same rule for all SCSI register unions.**

SMD block transfer (`ExecuteGO` in `deviceSMD.c`, triggered when control-word bit 2 `active` is
written). Copy this pattern exactly:
```c
sector   = blockAddressI & 0xFF;
head     = (blockAddressI >> 8) & 0xFF;
cylinder = blockAddressII;
long lba = ConvertCHStoLBA(regs, cylinder, head, sector);   // (cyl*heads + head)*sectors + sector
long position   = lba * bytesPrSector;                      // bytesPrSector = 1024
wordCounter     = (wordCounterHI << 16) | wordCounter;
coreAddress     = (coreAddressHiBits << 16) | coreAddress;  // WORD address
blockCounter    = (wordCounter * 2) / blockSizeBytes;       // #blocks to move

// READ:
buffer = malloc(blockCounter * blockSizeBytes);
self->blockCallbacks.readFunc(self, buffer, blockCounter, lba, unit);  // = machine_block_read
while (wordCounter > 0) {
    uint32_t w = Device_IO_BufferReadWord(self, buffer, buffer_ptr++); // big-endian (hi<<8)|lo
    Device_DMAWrite(coreAddress, (uint16_t)w);
    coreAddress = IncrementCoreAddress(regs);
    wordCounter = DecrementWordCounter(regs);
}
free(buffer);
Device_QueueIODelay(self, IODELAY_HDD_SMD, (IODelayedCallback)SMDReadEnd, unit, self->interruptLevel);

// WRITE: Device_DMARead(coreAddress) each word -> Device_IO_BufferWriteWord into buffer,
//        then writeFunc(self, buffer, blockCounter, lba, unit) = machine_block_write,
//        then the same Device_QueueIODelay(... SMDReadEnd ...).
// SMDReadEnd clears active, sets readyForTransfer + seek-complete, returns true if IRQ enabled
// (Device_TickIODelay then fires Device_GenerateInterrupt).
```
Endianness: the disk image is stored **big-endian / ND word order** (MSB first). ND word address
N maps to byte offset N*2. `Device_IO_BufferReadWord` = `(buf[N*2]<<8) | buf[N*2+1]`;
`Device_IO_BufferWriteWord` writes `hi = data>>8` to the even byte. No swap beyond hi/lo packing.
This is IDENTICAL to how the RetroCore DMA packs SCSI bytes into ND words (even byte -> high),
so the two implementations agree byte-for-byte.

Machine-layer fseek math (`machine_block_read`, `src/machine/machine.c` ~806-848):
```c
size_t bytes  = size * device->blockSizeBytes;               // size = #blocks
size_t offset = (size_t)blockAddress * device->blockSizeBytes;
fseek(entry->data.local_file, (long)offset, SEEK_SET);
fread(buffer, 1, bytes, entry->data.local_file);             // short read -> zero-filled
```
Write is symmetric (`fwrite` + `fflush`). The image FILE* is opened once at mount time:
`fopen(image_path, "rb+")` (falls back to `"rb"` read-only), `machine.c` ~500.

SMD boot (`SMD_Boot`, deviceSMD.c ~773): reads 4 blocks via `readFunc`, checks non-zero, then
`Device_DMAWrite(i, word)` for 2048 words into memory at address 0, returns 0. Model SCSI boot
on this if a bootstrap is needed (not required for the ENTER-DIRECTORY acceptance test, but
implement it for parity).

### 3.4 Factory + registration pattern (copy `CreateSMDDevice`, deviceSMD.c ~1287)
```c
Device *CreateSCSIDevice(uint8_t thumbwheel) {
    Device   *dev  = malloc(sizeof(Device));   memset(dev, 0, sizeof(Device));
    SCSIData *data = malloc(sizeof(SCSIData));  memset(data, 0, sizeof(SCSIData));
    Device_Init(dev, thumbwheel, DEVICE_CLASS_BLOCK, 1024);
    dev->deviceData = data;
    dev->Read = SCSI_Read; dev->Write = SCSI_Write; dev->Tick = SCSI_Tick;
    dev->Reset = SCSI_Reset; dev->Ident = SCSI_Ident; dev->Boot = SCSI_Boot;
    dev->Destroy = SCSI_Destroy;
    switch (thumbwheel) { /* set startAddress / identCode / logicalDevice / memoryName */ }
    dev->interruptLevel = 11;
    dev->endAddress = dev->startAddress + /* register span */;
    SCSI_Reset(dev);
    return dev;
}
```
Wire it in (three edits, mirror how SMD is registered):
1. `src/devices/devices_types.h`: add `DEVICE_TYPE_DISC_SCSI` to `DeviceType`, and
   `#include "./scsi/deviceSCSI.h"` at the bottom (where all device headers are included).
2. `src/devices/devicemanager.c`: add `case DEVICE_TYPE_DISC_SCSI: dev = CreateSCSIDevice(thumbwheel);`
   in `CreateDevice()`, and a `DeviceManager_AddDevice(DEVICE_TYPE_DISC_SCSI, 0);` in
   `DeviceManager_AddAllDevices()`. Because `deviceClass == DEVICE_CLASS_BLOCK`,
   `DeviceManager_AddDevice` auto-wires `machine_block_read/write/disk_info` - you do NOT open
   the image file yourself.
3. `src/machine/machine.c`: `machine_block_read/write/disk_info` currently pick the drive array
   by `device->type` (`DEVICE_TYPE_DISC_SMD ? DRIVE_SMD : DRIVE_FLOPPY`, ~machine.c:783). Add a
   `DEVICE_TYPE_DISC_SCSI -> DRIVE_SCSI` branch and a `scsi_drives[]` mounted-image array +
   `mount_scsi(...)` helper alongside `smd_drives`/`mount_floppy` (~machine.c:241-262), or the
   SCSI disk will be treated as a floppy. Follow the existing mount helper exactly.
4. Build system: CMake `src/devices/CMakeLists.txt` uses `file(GLOB ...)` per subdir - add a
   `file(GLOB SCSI_SOURCES "scsi/*.c")`, append to `SOURCES`, add the
   `mkptypes ... deviceSCSI.c >> devices_protos.h` line to the custom command, and add `scsi` to
   both `target_include_directories` lists. Makefile `src/devices/Makefile` lists sources
   explicitly - add `scsi/deviceSCSI.c` to `SRCS` and `-I$(SRC_DIR)/scsi` to `INCLUDES`.
   `devices_protos.h` is AUTO-GENERATED by `mkptypes` - do NOT hand-edit it; put your
   `CreateSCSIDevice` prototype in `deviceSCSI.h`.

Naming conventions: folder `src/devices/scsi/`; files `deviceSCSI.c`/`deviceSCSI.h` (+ optional
`diskSCSI.c/.h` for geometry/identity tables, mirroring `diskSMD`); static internals prefixed
`SCSI_*` (`SCSI_Read`, `SCSI_Write`, `SCSI_Tick`, `SCSI_Reset`, `SCSI_Ident`, `SCSI_Boot`,
`SCSI_Destroy`, plus internal `SCSI_ExecuteCommand`, `SCSI_ReadCapacity`, `SCSI_Inquiry`, ...);
shared framework functions stay `Device_*`; register unions `union {uint16_t raw; struct{...} bits;}`;
register enum `SCSIRegisters` with octal IOX addresses in comments; device-private state in a
`SCSIData` typedef stored in `deviceData`.

---

## 4. File-by-file port plan (C# class -> C file/functions)

Create everything under `~/repos/nd100x/src/devices/scsi/`. You may fold the SCSI stack into
fewer files than the C# side - C uses one `Device` struct + `deviceData`, not a class hierarchy.
Recommended layout:

| C# source (RetroCore) | Responsibility | C target |
|---|---|---|
| `NDBusDiscControllerSCSI.cs` | ND IOX register map, control/status words, DMA byte<->word packing, GO/completion, interrupt level 11 | `deviceSCSI.c` : `SCSI_Read`, `SCSI_Write`, `SCSI_Tick`, `SCSI_Reset`, `SCSI_Ident`, `SCSI_Boot`, `SCSI_Destroy`, `SCSI_ExecuteGo`, DMA pump helpers; `deviceSCSI.h` register/status/control unions + `SCSIRegisters` enum + `CreateSCSIDevice` proto |
| `SCSIBus.cs` + `SCSIFullDevice.cs` (phase state machine) | SELECT/COMMAND/DATA/STATUS/MSG/BUS-FREE sequencing, CDB length by group, DATA IN/OUT/STATUS queuing, sense buffer | `deviceSCSI.c` internal phase state in `SCSIData` (`phase`, `scsi_cmdbuf[4096]`, `sense[18]`, FIFO), functions `SCSI_StepPhase`, `SCSI_QueueDataIn`, `SCSI_QueueDataOut`, `SCSI_StatusComplete`, `SCSI_SetSense` |
| `SCSIHDD.cs` | CDB decode (all opcodes in the table), READ CAPACITY, INQUIRY, MODE SENSE, REQUEST SENSE, block read/write via callbacks | `deviceSCSI.c` : `SCSI_ExecuteCommand` (opcode switch), `SCSI_ReadCapacity`, `SCSI_Inquiry`, `SCSI_ModeSense`, `SCSI_RequestSense`, `SCSI_ReadBlocks`, `SCSI_WriteBlocks` |
| `SCSIHDDMicropolis.cs` | Micropolis-1375-ND geometry + identity (NDMICROP/1375/B0C), UNIT ATTENTION on reset, SEEK(6), vendor 0x0C | `diskSCSI.c/.h` : `DiskSCSI_SetDiskType` (cyl=898, heads=8, sectors=18, sectorbytes=1024), identity strings, `SCSI_DeviceReset` posting ASC 0x29 |
| `SCSISupport.cs` (`Buffer` big-endian accessors) | u16/u24/u32 big-endian put/get | small static helpers in `deviceSCSI.c` OR reuse `Device_IO_BufferReadWord`-style helpers; add `scsi_put_u32be/get_u32be/get_u24be/get_u16be` |
| `SCSIEnums.cs` | opcodes, sense keys, ASC codes, status | `#define`/`enum` in `deviceSCSI.h` |
| `SCSIHDDImage.cs` | raw image reader | NOT ported - use machine block callbacks |

Preserve exact behavior: the register/IO map, the full command set (INQUIRY, READ CAPACITY,
READ/WRITE 6/10), the fn-42 control-record last-block read, block size 1024 = 512 words, 21-bit
vs 32-bit LBA handling, big-endian multi-byte fields, DMA byte<->word packing, and the level-11
completion interrupt with RITRG-only acknowledge.

---

## 5. Byte-level accuracy checklist (validation-critical)

Every item here MUST match RetroCore exactly, or SINTRAN's mount / directory listing will fail:

- [ ] **Sector size = 1024 bytes = 512 ND words**, fixed. MODE SELECT block descriptor is
      accepted but IGNORED (sector size never changes).
- [ ] **READ CAPACITY** returns 8 bytes: `[0..3]` = last LBA big-endian u32, `[4..7]` = 1024
      big-endian u32. For Micropolis-1375-ND last LBA = **129311 (0x0001F91F)**. Reporting any
      other last-LBA -> DISC-TRANSFER-ERROR / STATUS 100020B.
- [ ] **INQUIRY** bytes: byte0=0x00, byte1=0x00, byte2=0x05, byte3=0x01, byte4=52; vendor
      "NDMICROP" @8, product "1375" @16 (space-padded), rev "B0C" @32. Return `min(cmd[4],56)`.
- [ ] **READ(6)/WRITE(6)**: LBA = 21-bit `get_u24be(cmd+1) & 0x1FFFFF`; length = `cmd[4]`,
      0 means 256. **READ(10)/WRITE(10)**: LBA = `get_u32be(cmd+2)`; length = `get_u16be(cmd+7)`.
- [ ] **All SCSI multi-byte fields are BIG-ENDIAN** (MSB first).
- [ ] **DMA byte<->word packing is big-endian**: first SCSI byte of a word -> HIGH byte; MAR
      increments only after the low byte; MAR is a WORD address (physical byte addr = `MAR<<1`).
      This equals the SMD `Device_IO_BufferReadWord/WriteWord` convention - keep them identical.
- [ ] **ND-100 is 16-bit WORD addressed** - never treat the ND side as byte-addressed. The `*2`
      (word->byte) conversion is an emulator implementation detail on the image-buffer side only.
- [ ] **LBA -> file offset = LBA * 1024 bytes**; block read/write go through the machine block
      callbacks (`machine_block_read/write`), same as SMD.
- [ ] **Control-record / fn-42 = a READ of the last LBA** (and block 0 must read too). No special
      SCSI opcode; it is a normal READ served from the image.
- [ ] **REQUEST SENSE** after reset = UNIT ATTENTION (sense key 0x06, ASC 0x29).
- [ ] **Completion interrupt = level 11**, acknowledged ONLY on `RITRG` (0x2C) read, never on
      `RSTAU` (0x04) read.
- [ ] Status word bit3 = ready-for-transfer, bit2 = active/busy, bit9 = interrupt-from-NCR;
      control word bit2 = GO, bit5 = DMA enable, bit6 = direction (1 = SCSI->ND memory).

---

## 6. Validation plan

1. **Build**: build nd100x (CMake and/or Makefile in `src/devices/`) after wiring the device in.
   Fix any prototype/GLOB/include issues. `devices_protos.h` regenerates via `mkptypes`.
2. **Mount a SCSI disk image**: configure the machine to attach `deviceSCSI` (thumbwheel 0,
   logical device 2202) with the SINTRAN SCSI disk image (the same image RetroCore uses).
3. **Boot SINTRAN** from the working boot device, then at the SINTRAN prompt run:
   ```
   @ENTER-DIRECTORY,,DISC-SCSI-1,0
   @DIR
   ```
   ACCEPTANCE: ENTER-DIRECTORY mounts without error and `@DIR` reports the directory contents -
   matching RetroCore's behavior exactly (this already works in RetroCore after the RDIV fix).
4. **Diff against the known-good trace**: enable SCSI tracing and compare the CDB sequence and
   the byte-level INQUIRY / READ CAPACITY / READ responses against RetroCore's trace and the
   NDInsight capture docs under `SINTRAN/Devices/SCSI/` (e.g.
   `CAPTURE-WORKING-MOUNT-TRACE.md`, `scsi-open-last-block-read.md`, `scsi-transfer-status.md`,
   `scsi-mount-last-block-is-control-record.md`). Any divergence in opcode, LBA, length, or
   response bytes is a bug. In particular confirm: READ CAPACITY returns 129311/1024; the mount
   reads block 0 AND the last block; the completion interrupt fires on level 11.
5. If SINTRAN reports STATUS 100020B (DISC TRANSFER ERROR), re-check READ CAPACITY last-LBA and
   the completion/interrupt handshake first - that is the known failure signature.

---

## 7. Constraints

- Match nd100x's existing C style exactly. **C, not C++** (no classes; `Device` struct +
  function pointers + `void *deviceData`).
- Follow the existing driver file/proto conventions: `src/devices/scsi/deviceSCSI.c/.h`
  (+ optional `diskSCSI.c/.h`), `SCSI_*` statics, `Device_*` framework calls, register unions
  with all-`uint16_t` bitfields (the `-mms-bitfields` gotcha), octal IOX comments.
- Do NOT hand-edit the auto-generated `devices_protos.h`; declare public prototypes in your `.h`
  and let `mkptypes` regenerate.
- Use the machine block-callback path for all image I/O (do not open the file inside the driver).
- Reuse `Device_DMAWrite`/`Device_DMARead`/`Device_QueueIODelay`/`Device_GenerateInterrupt`/
  `Device_IO_BufferReadWord`/`Device_IO_BufferWriteWord` - do not reinvent DMA, interrupts, or
  word packing.
- Keep as many explanatory comments as the SMD driver has; cite the RetroCore source and the
  verified real-hardware traces (INQUIRY `12 00 00 00 24 00`; REQUEST SENSE
  `70 00 06 00 00 00 00 0A 00 00 00 00 29 00 00 00 00 00`) in comments.
- The final arbiter of correctness is behavioral parity with the RetroCore SCSI controller and
  the successful SINTRAN `@ENTER-DIRECTORY,,DISC-SCSI-1,0` + `@DIR`.

---

### Quick reference - the two key files to read first
- Target style: `~/repos/nd100x/src/devices/smd/deviceSMD.c` (+ `deviceSMD.h`,
  `~/repos/nd100x/src/devices/device.c`, `~/repos/nd100x/src/machine/machine.c`).
- Byte source of truth: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\Common\SCSI\SCSIHDD.cs`
  (+ `SCSIHDDMicropolis.cs`, and the ND adapter
  `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusDiscControllerSCSI.cs`).
