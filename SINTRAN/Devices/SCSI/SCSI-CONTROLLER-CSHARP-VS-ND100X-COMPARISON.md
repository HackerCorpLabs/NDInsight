# ND-100 SCSI Controller: C# RetroCore vs C nd100x - Functional Equivalence Report

Date: 2026-07-17
Method: line-by-line reading of BOTH implementations. Every claim below cites
file + line (or function) on both sides. Anything not byte-verified is marked
UNVERIFIED.

## Sources compared

C# (RetroCore, authoritative, recently debugged against live SINTRAN):

- `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusDiscControllerSCSI.cs` (2654 lines)
- `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\NCR\SCSI\NCR5386\NCR5386SCSI.cs` (+ `.CommandHandling.cs`, `.StateHandling.cs`, `Registers.cs`, `Enums.cs`)
- `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\Common\SCSI\SCSIHDD.cs`, `SCSIHDDMicropolis.cs`, `SCSIFullDevice.cs`, `SCSIBus.cs`, `SCSIEnums.cs`
- `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusDeviceBase.cs` (DMA + IDENT base)

C (nd100x, WSL `~/repos/nd100x`):

- `src/devices/scsi/deviceSCSI.c` / `.h` (controller card)
- `src/devices/scsi/ncr5386.c` / `.h` (chip)
- `src/devices/scsi/scsiDevice.c` / `.h` (target phase machine), `scsiBus.c` / `.h`
- `src/devices/scsi/scsiHDD.c` / `.h`, `diskSCSI.c` / `.h` (disk target + geometry)
- `src/devices/device.c` (Device_DMARead/Write), `src/cpu/cpu_instr.c` + `src/cpu/cpu_mms.c` (bonus)

The C code is an explicit, commented port of the C# (deviceSCSI.h:15-21 names
every C# source file), so the baseline is high similarity; this report focuses
on where they DIVERGE.

---

## (a) Per-checklist verdict table

| # | Item | Verdict | C# citation | nd100x citation |
|---|------|---------|-------------|-----------------|
| 1a | IOX register offsets (RLMAR 0, WLMAR 1, REDAT 2, WRDAT 3, RSTAU 4, WCONT 5, RHMAR 6, WHMAR 7, RXWC_HI 010, RXWC 012, RNDAT 040 ... WTCL 075) | SAME | NDBusDiscControllerSCSI.cs:416-623 (enum Register) | deviceSCSI.h:66-100 (SCSIRegisters) |
| 1b | Address decode: 64 IOX addresses, base 144300/144400/144500/144600 by TW2, unused offsets return undefined data (no IOX error) | SAME | ctor:652-700, NDBusAddressLength=63:722, default cases:1032-1033/1232-1233 | CreateSCSIDevice:723-756 (`thumbwheel & 0x03`, endAddress = start+63), SCSI_Read default:430-432 |
| 1c | REDAT/WRDAT (IOX PIO data) backing store | DIFFERS | REDAT reads 1KW ring `DataBuffer[readbufferPointer]`:861-871; WRDAT writes ring:1064-1073 | REDAT returns single `readWriteData` word:372-374; WRDAT stores it:462-464 |
| 1d | Test-mode DMA data source (WCONT bit 6 write path) | DIFFERS (consequence of 1c) | test-mode DMA writes `regs.ReadWriteData`:1124-1137, which WRDAT never sets (WRDAT goes to the ring) | test-mode DMA writes `data->readWriteData`:479-486, which WRDAT DOES set - matches the ND doc wording |
| 2a | RSTAU bit map (0 int-en, 2 busy/active, 3 ready-for-transfer, 5 reset-on-bus, 6 NCR disabled, 8 DRQ-from-NCR, 9 NCRIT int-from-NCR, 10 DACK, 12 BSY, 13 REQ, 14 ACK; 4/7/11/15 never set) | SAME | Read RSTAU:874-916 | SCSI_Read RSTAU:376-396, bit defines deviceSCSI.h:105-119 |
| 2b | Reading RSTAU does NOT ack the NCR interrupt latch | SAME | comment+code:919-925 (no clear) | comment:381-383, code clears nothing:376-396 |
| 2c | Reading RITRG is the ONLY ack of the controller-level NCR-int latch (and clears the chip int_reg) | SAME | RITRG case:1003-1010 (`regs.InterruptFromNCR5386 = false`), chip clear in ReadInterruptRegister NCR5386SCSI.cs:721-754 | SCSI_Read RITRG:417-422 (`data->interruptFromNCR = false`), chip clear NCR_ReadInterruptRegister ncr5386.c:939-952 |
| 3a | WLMAR/WHMAR composition (WHMAR masked to 8 bits, MAR = MSB<<16 or LSB, 24-bit word address) | SAME | Registers.MAR:239-257, WHMAR:1189-1191 | SCSI_GetMAR/IncrementMAR:90-100, WHMAR:458-460 |
| 3b | MAR auto-increment during DMA (read: on even byte after word fetch; write: on odd byte after RMW) | SAME | ReadNextByteDMA:1353-1370, WriteNextByteDMA:1373-1392 | SCSI_ReadNextByteDMA:214-231, SCSI_WriteNextByteDMA:234-255 (comment 197-213 documents the asymmetry) |
| 3c | Test-mode MAR increment on RLMAR read | DIFFERS (C#-only) | RLMAR case:853-859 (`if (regs.testMode) regs.IncrementMarRegister()`) | SCSI_Read RLMAR:368-370 - NO test-mode increment |
| 3d | ExecuteGo semantics on WCONT activate | DIFFERS (minor) | ExecuteGo:1397-1419 zeroes `dma_bytes_written/read` on every GO (1400); rest is dead code behind `return` | SCSI_Write WCONT:507-508 only clears readyForTransfer; dma byte counters cleared only on Clear Device (494-495) and Reset (340-341) |
| 3e | WCONT with int-enable but WITHOUT activate raises an immediate interrupt if readyForTransfer | DIFFERS (C#-only) | Write WCONT else-branch:1179-1186 (`SetInterruptBit(true)`) | SCSI_Write WCONT:452-509 has no such branch |
| 3f | Clear Device (WCONT bit 4): zero MAR + pointers, NCR DeviceReset, set readyForTransfer, bus RST unaffected | SAME | 1141-1160 | 490-499 |
| 4a | DMA data path: byte-wise chip FIFO packed big-endian into 16-bit ND words (even byte = high) | SAME | WriteNextByteDMA:1373-1392, ReadNextByteDMA:1353-1370 | 214-255 + comment 197-206 |
| 4b | DMA drain model: on every clock, if NCR int -> complete (active=false, rft=true, IRQ 11 if enabled); then drain ALL pending DRQ bytes in a while loop | SAME | Clock:828-837, StepGoState:1253-1341 | SCSI_Tick:536-553, SCSI_StepGoState:269-315 |
| 4c | Transfer counter WTCM/WTC2/WTCL forwarded to chip 24-bit counter; TC-zero aux bit updated on load and on decrement | SAME | Write cases:1220-1230, Registers.cs:226-267, StateHandling.cs:267-272/341-346 | deviceSCSI.c:518-520, ncr5386.c:994-1005 (NCR_UpdateTCAux:94-100), 613-615/680-682 |
| 4d | Memory access: physical, NO page table / NO APT | SAME | NDBusDeviceBase.DMAWrite/DMARead:515-541 (`DMABus.WriteMemory16(coreAddress << 1, ...)` - byte-addressed physical SystemBus, word addr shifted) | device.c Device_DMAWrite/DMARead:372-384 (`WritePhysicalMemory(coreAddress & 0xFFFFFF, data, false)` with `gDMAAccess = true` bypassing shadow/page tables; word-addressed, NOT shifted - documented in deviceSCSI.c:208-213) |
| 5a | NCR5386 register map (Data 0, Command 1, Control 2, DestID 3, AuxStatus 4, OwnID 5, Interrupt 6, SourceID 7, DataII 8, DiagStatus 9, TC 12/13/14) | SAME | Enums.cs SCSIRegisters (via Read/Write dispatch NCR5386SCSI.cs:242-448) | ncr5386.h:27-42 |
| 5b | Command set: immediate 0-7 (ChipReset, Disconnect, Pause, SetATN, MsgAccepted, ChipDisable), interrupting 8-21 (SelectW/WoATN, Reselect, Diagnostic, target-role stubs, TransferInfo, TransferPad); immediate + Diagnostic execute on write, others deferred 2 ticks | SAME | CommandHandling.cs ExecuteLoadedCommand:27-396, WriteCommandRegister NCR5386SCSI.cs:596-634 | NCR_ExecuteLoadedCommand ncr5386.c:197-409, NCR_WriteCommandRegister:911-936 |
| 5c | Interrupt register values: 0x01 FunctionComplete, 0x02 BusService, 0x04 Disconnected, 0x08 Selected, 0x10 Reselected, 0x40 InvalidCommand; valid mask | SAME | Enums.cs:849-923 | ncr5386.h:108-118 |
| 5d | Aux status bits: 0x02 TC-zero, 0x04 Paused, 0x08 IO, 0x10 CD, 0x20 MSG, 0x40 ParityError, 0x80 DataRegFull; phase re-latched on EVERY RAUXS read (deliberate MAME divergence) | SAME | Enums.cs:731-812, Read AuxilaryStatus NCR5386SCSI.cs:360-407 ("Ronny 22.03.2024 MUST be updated always") | ncr5386.h:98-105, NCR5386_Read AUX:1056-1074 (comment cites the C# note) |
| 5e | Phase sequencing SELECT -> (FunctionComplete) -> REQ -> BusService -> CDB via Transfer Info DMA -> DATA -> STATUS -> MSGIN (FunctionComplete on last MSG-IN byte, ACK held); ATN from cmd bit 0; !m_int_state guard before BusService | SAME | StateHandling.cs StepState:31-383 (guard:193) | NCR_StepState ncr5386.c:416-715 (guard:550) |
| 5f | Arbitration bus-free precondition test | DIFFERS | StateHandling.cs:69 `!(ctrl.HasFlag(S_SEL \| S_BSY \| S_RST))` - .NET HasFlag(combined) is true only if ALL three are set, so C# proceeds unless ALL of SEL+BSY+RST are simultaneously asserted | ncr5386.c:459 `!(ctrl & (S_SEL\|S_BSY\|S_RST))` - proceeds only if NONE is asserted (matches MAME) |
| 5g | Arbitration own-ID source: chip uses sourceID (not id_register) for `oid = 1 << sourceID` | SAME code, DIFFERENT effective value | StepState:36 uses `regs.sourceID`; the ND controller writes SourceID=7 (TW1) at ctor:806 / Reset:818 -> oid = 0x80 | NCR_StepState:434 uses `ncr->sourceID`; the ND controller writes ID **register** instead (deviceSCSI.c:352, 497 "SourceID is read-only") -> sourceID stays 0, oid = 0x01. Comment ncr5386.c:426-433 documents this and claims RetroCore leaves sourceID 0 - that claim is STALE (the A/B-TEST revert re-instated the SourceID write). Also affects RSOUI readback: C# returns 7, C returns 0; and ROIDN: C returns 7, C# returns whatever SINTRAN wrote via WOIDN (id_register), else 0. |
| 5h | Chip reset state (aux = TC-zero, diag = SelfDiagnosticComplete, state Disconnected/IDLE, sourceID deliberately NOT cleared) | SAME (C clears a few extra latches) | CommandChipReset CommandHandling.cs:410-428 (does NOT touch CommandCodeLoaded/PauseRequested/m_dat/timer) | NCR_CommandChipReset ncr5386.c:130-154 (additionally zeroes commandCodeLoaded, pauseRequested, m_dat, timer) - only reachable difference is a pending-but-unexecuted command surviving Chip Reset in C#; dormant in the SINTRAN flow |
| 6a | Disk geometry: Micropolis 1375-ND, 898 cyl x 8 heads x 18 sectors x 1024 B, last LBA 129311, vendor NDMICROP / 1375 / B0C, drive params {0,153,4,0,128,0,64,11} | SAME | SCSIHDDMicropolis.cs:136-141, 202-205, 169-179; hdinfo.DiskSizeInBlocks = cyl*heads*sectors - 1 SCSIHDD.cs:861-867 | diskSCSI.c:72-87, DiskSCSI_LastLBA:37-45 |
| 6b | CDB opcodes on the ND path: TEST UNIT READY, REQUEST SENSE (4-byte reply), INQUIRY (page 0, clamp 56), READ CAPACITY (phys last LBA + 1024), READ/WRITE(6) (21-bit LBA, len 0 => 256), READ/WRITE(10), SEEK(6) (CONDITION_MET quirk), RESERVE(6), MODE SELECT(6) (accepted+ignored), START/STOP, VERIFY(10), RECEIVE/SEND DIAGNOSTIC, INIT DRIVE PARAMS 0x0C, READ BUFFER rejected | SAME | SCSIHDDMicropolis.scsi_command:252-352 + SCSIHDD.scsi_command:235-454; opcode values SCSIEnums.cs:59-129 | SCSIHDD_Command scsiHDD.c:298-519; opcode values scsiHDD.h:27-50 |
| 6c | MODE SENSE(6) 0x1A | DIFFERS (nd100x-only gap) | Full CommandModeSense (pages 0,1,2,3,4,8,0x30,0x3F) SCSIHDD.cs:309-311 + 629-831 | scsiHDD.c:320-519 has NO SC_MODE_SENSE_6 case -> falls to default -> SCSITarget_ReportBadCmd (CHECK CONDITION, ILLEGAL REQUEST):515-518 |
| 6d | FORMAT UNIT 0x04 | DIFFERS (deliberate C fix) | Executes a write loop `writeBlock(cyl * head * sector, block)` SCSIHDD.cs:394-419 - the CHS product is NOT an LBA, so it zero-fills wrong blocks including block 0 repeatedly (image corruption) | scsiHDD.c:464-480 returns GOOD without touching the image, with a comment explaining why the C# loop was not ported |
| 6e | REQUEST SENSE returns only 4 bytes (real drive returns 18 - both keep the 4-byte quirk); reset posts UNIT ATTENTION ASC 0x29 | SAME | SCSIHDDMicropolis.cs:320-326 (4 bytes), DeviceReset:241-250 (sense 0x06/0x29) | scsiHDD.c:326-334 (4 bytes + do-not-fix note), SCSIHDD_DeviceReset:523-536 |
| 6f | Vendor cmds 0x0E/0x0F/0x10 put/get-data handlers exist but 0x0E/0x0F/0x10 are not dispatched (unreachable) - except C dispatches none of them either; 0x0C fully wired both sides | SAME | SCSIHDDMicropolis.cs:338-346 (note), 330-336 (0x0C) | scsiHDD.c:164-175 (handlers), 436-439 (0x0C); 0x10 handled in GetData:123-129 (equally unreachable) |
| 6g | Target phase machine (selection, MSG OUT/IDENTIFY, CDB length by group, DATA IN/OUT, STATUS, MSG IN COMMAND COMPLETE, BUS FREE), buf_control FIFO, sense format, LUN via IDENTIFY | SAME | SCSIFullDevice.cs (step:~370-460, scsi_command_done:543-570, scsi_message:937-949, get_lun:951-957, timer:130-138) | scsiDevice.c (Step:370-582, CommandDone:332-351, Message:354-364, GetLun scsiHDD.c:57-62, Clock:588-601 with matching post-decrement) |
| 6h | Sense-buffer clearing | SAME NET EFFECT (different code) | set_sense_data clear loop writes `scsi_sense_buffer[0] = 0` 18 times (index typo; harmless because C# arrays start zeroed) - SCSIFullDevice.cs (set_sense_data; see C-side comment) | scsiDevice.c:229-263 memsets all 18 bytes, comment 232-241 explains why literal porting of the typo would DIVERGE |
| 7a | Ident codes 140440-140443 octal by TW2, logical devices 2202-2205 | SAME | ctor:662-694 | CreateSCSIDevice:725-748 |
| 7b | Interrupt level 11, raised at completion (NCR int while active) only when interruptEnabled | SAME | InterruptLevel=11:723, StepGoState:1258-1267 | interruptLevel=11:751, SCSI_StepGoState:276-287 |
| 7c | IDENT clears interruptEnabled and the level latch | SAME (mechanically different) | IDENT override:822-826 + base IDENT NDBusDeviceBase.cs:223-234 (clears bit, returns identCode unconditionally) | SCSI_Ident deviceSCSI.c:556-572 (returns identCode only if its level bit is set, else 0 - required by nd100x's IDENT polling chain; same system-level outcome) |
| 7d | NCR interrupt is EVENT-latched at the controller (callback with state=0 never clears the latch; only RITRG read does) | SAME | Ncr5386_OnInterrupt:763-782 (`if (intr != 0)` only sets) | SCSI_OnNCRInterrupt deviceSCSI.c:169-182 (same guard + comment) |
| 8a | Parity: pass-parity/diagnostic parity turnaround (odd-parity helper), ParityError aux bit set/cleared by Diagnostic data write, cleared on RITRG read | SAME | Parity handling in WriteDataRegister NCR5386SCSI.cs:502-549, ReadInterruptRegister:743 | NCR_WriteDataRegister ncr5386.c:859-908, NCR_ReadInterruptRegister:944; SCSI_IsOddParity:39-45 |
| 8b | Illegal/reserved NCR register or command -> InvalidCommand interrupt | SAME | Write Reserved case NCR5386SCSI.cs:303-307, HandleError CommandHandling.cs:398-404 | ncr5386.c:1006-1010, NCR_HandleError:122-127 |
| 8c | Selection timeout: no target BSY -> Disconnected interrupt, state IDLE | SAME | StateHandling.cs:147-158 | ncr5386.c:522-529 |
| 8d | ND-100 bus DMA error (BERROR, RSTAU bits 4/11) | SAME (never generated by either) | comment:902/912 | comment:378-380 |
| - | Boot path (bootstrap block load) | nd100x-only (in this file pair) | not present in NDBusDiscControllerSCSI.cs (RetroCore boots via another mechanism - UNVERIFIED which) | SCSI_Boot deviceSCSI.c:588-668 (4 x 1024-byte blocks -> 2048 words at addr 0, bypassing the register path, mirroring SMD_Boot) |

---

## (b) Divergences that matter

Ordered by potential functional impact on SINTRAN boot / mount / read / write.

### D1. WCONT "interrupt when ready" without activate - C#-only
C# (NDBusDiscControllerSCSI.cs:1179-1186): writing WCONT with bit 0
(enable interrupt) set, bit 2 (activate) clear, while readyForTransfer is true,
immediately raises the level-11 interrupt. nd100x (deviceSCSI.c:452-509) never
does this; it only interrupts from SCSI_StepGoState on an NCR completion.
The ND doc bit-0 text ("gives interrupt ... as soon as the controller is
ready") supports the C# behaviour. In the carved SINTRAN driver every
enable-interrupt WCONT write observed ("5", "5\/SCCCW") also sets activate, so
the branch is normally dormant - but any driver path that enables interrupts
while idle (e.g. after Clear Device, which sets readyForTransfer on both
sides) would hang on nd100x waiting for an interrupt that never comes.

### D2. NCR arbitration ID / SourceID vs ID-register write - opposite registers
C# writes TW1 (7) into the chip's SourceID register (NDBusDiscControllerSCSI.cs:806,
818); nd100x writes 7 into the ID register instead, on the stated ground that
SourceID is read-only (deviceSCSI.c:352, 497), and its comment in
ncr5386.c:426-433 asserts "RetroCore ... nothing ever writes sourceID" -
which no longer matches the C# code (the A/B-TEST revert restored the
SourceID write). Effects:
- Arbitration asserts data bit 0x80 in C# vs 0x01 in nd100x (StepState oid
  calculation: StateHandling.cs:36 / ncr5386.c:434). Uncontested with a single
  initiator, so no visible difference today.
- RSOUI (o56) reads back 7 in C#, 0 in nd100x. The SINTRAN driver reads RSOUI
  only on the RECONNECT path (NPL MSGI, `T:= HDEV + RSOUI` - see the driver
  listing embedded at NDBusDiscControllerSCSI.cs:1919-1925) to find which
  target reconnected; neither emulator implements target-initiated reselect,
  so this is dormant, but if reselection is ever added the two will disagree.
- ROIDN (o52) reads back 7 in nd100x but 0 (or whatever SINTRAN wrote via
  WOIDN) in C#.

### D3. Arbitration bus-free gate - C# HasFlag bug
C# StateHandling.cs:69 uses `ctrl.HasFlag(S_SEL | S_BSY | S_RST)`, which in
.NET is "ALL of these bits set", so C# leaves ARBITRATE_BUS_FREE virtually
always, even while the bus is busy. nd100x ncr5386.c:459 uses the correct MAME
semantics (proceed only when NONE are set). nd100x is the STRICTER (and
hardware-correct) one; because SINTRAN only selects when the bus is free and
the target releases BSY before the next select, both pass in practice. A
retry/reset race (select issued while RST or a previous BSY is still up) would
behave differently: C# would barge ahead, nd100x would wait.

### D4. MODE SENSE(6) missing in nd100x
C# answers MODE SENSE(6) with full mode pages (SCSIHDD.cs:629-831); nd100x
returns CHECK CONDITION / ILLEGAL REQUEST (scsiHDD.c default:515-518, no 0x1A
case). Not used by the SINTRAN mount/boot flow as observed (the mount
sequence exercised READ CAPACITY / INQUIRY / READ / WRITE / REQUEST SENSE),
but DIR-related tools or SCSI-TV diagnostics that issue 0x1A will fail on
nd100x and succeed on RetroCore. UNVERIFIED whether any SINTRAN component
issues MODE SENSE.

### D5. FORMAT UNIT - C# corrupts, nd100x no-ops
C# SCSIHDD.cs:394-419 "formats" by writing a zero block to LBA
`cyl * head * sector` for every CHS triple - a wrong product, not an LBA, so
it repeatedly zeroes block 0 (whenever head or sector is 0) and a scatter of
other blocks: running DISK-MM format against RetroCore damages the image.
nd100x scsiHDD.c:464-480 deliberately accepts FORMAT UNIT and does nothing.
Irrelevant to boot/read/write of an existing image; decisive for anyone who
runs a format.

### D6. Test-mode register plumbing (REDAT/WRDAT/RLMAR)
Three related differences, all confined to the "count address registers" test
operation and the odd-byte force path:
- C# increments MAR on RLMAR read in test mode (NDBusDiscControllerSCSI.cs:853-859);
  nd100x does not (deviceSCSI.c:368-370). The SINTRAN NEWPH odd-byte
  recovery (driver listing lines 070565-070601 in the embedded NPL: WCONT=150
  octal test-mode, RLMAR read "FORCE LAST BYTE TO MEMORY", then the MAR
  verification expects the incremented value) relies on the C# behaviour; on
  nd100x an odd-byte data-in stop would fail the MAR check and trigger MARER
  -> SCSI bus reset. Dormant with 1024-byte sectors (all transfers even), but
  it is a REAL latent divergence in nd100x, and the C# side matches hardware.
- WRDAT: C# writes into a 1023-entry word ring (`DataBuffer`, 1064-1073) that
  test-mode DMA never reads (test mode uses `regs.ReadWriteData`, 1124-1137,
  which nothing writes); nd100x WRDAT feeds `readWriteData` which test-mode
  DMA writes to memory (deviceSCSI.c:462-486) - nd100x matches the ND doc
  ("data is taken from the Data Register"), C# does not.
- REDAT: C# returns ring contents (861-871); nd100x returns the test-mode DMA
  read latch (372-374).
  Side note: the C# ring is also off by one - `DataBuffer = new ushort[BUFFER_MAX]`
  with BUFFER_MAX = 1023 while the pointer mask allows index 1023
  (NDBusDiscControllerSCSI.cs:631-637): index 1023 would throw. Never hit in
  the SINTRAN flow.

### D7. DMA byte-parity counters not reset per GO in nd100x
C# ExecuteGo (1397-1400) zeroes `dma_bytes_written/dma_bytes_read` on every
WCONT activate, so byte->word packing parity always starts "even" at each
operation. nd100x clears them only on Clear Device / Reset
(deviceSCSI.c:340-341, 494-495): if any operation ever moved an ODD number of
bytes, the next operation on nd100x would start on the odd half-word and pack
every subsequent byte one position off. All SINTRAN phases observed move even
counts (CDB 12/14 bytes via DMA per COMPH, data 1024*n; single-byte STATUS and
MSG bytes go through RNDAT, not the DMA pump), so this is dormant - but it is
a state-hygiene divergence with a nasty failure mode if an odd DMA transfer
ever occurs.

### D8. Chip Reset residue - C# keeps pending command latches
NCR_CommandChipReset (ncr5386.c:130-154) additionally clears
commandCodeLoaded, pauseRequested, m_dat, and the step timer; the C#
CommandChipReset (CommandHandling.cs:410-428) does not. In C#, a Chip Reset
issued in the 2-tick window between loading an interrupting command and its
deferred execution would still execute the stale command afterwards
(StepState:43-53 checks CommandCodeLoaded first). SINTRAN never resets inside
that window in the traced flows. Dormant.

### Equivalences worth stating (the load-bearing ones)
- The RITRG-acks / RSTAU-does-not-ack interrupt handshake - the fix that
  unblocked the SINTRAN multi-phase WRITE loop - is byte-identical in intent
  and code on both sides (C# 919-925 + 1003-1010; C 376-396 + 417-422). The C
  comments show the behaviour was ported FROM nd100x into C#; both now agree.
- DMA memory access is physical on both sides: C# `DMABus.ReadMemory16(addr << 1)`
  against the byte-addressed physical SystemBus (NDBusDeviceBase.cs:535-541),
  nd100x `ReadPhysicalMemory(addr & 0xFFFFFF, false)` with the gDMAAccess
  shadow-bypass flag (device.c:366-384). Neither goes near a page table or
  APT; the false argument to Read/WritePhysicalMemory is a "punt on
  parity/limits" flag, not an APT selector (cpu.c:412/423 usage pattern).
- READ CAPACITY returns the geometry-derived physical last LBA (129311) plus
  block size 1024 on both sides (SCSIHDD.cs:489-497; scsiHDD.c:216-232) - the
  value SINTRAN's ECAPD validates.
- READ(6)/WRITE(6) 21-bit LBA masking, 0-means-256 length, ILLEGAL REQUEST
  0x24 on out-of-range reads: identical (SCSIHDD.cs:254-298;
  scsiHDD.c:344-383).

---

## (c) Bonus: stack instructions (INIT/ENTR/LEAVE/ELEAV) and APT in nd100x

Verdict: nd100x does NOT have the bug RetroCore just fixed. It already
implements the ND-110 microcode rule and documents it.

- `~/repos/nd100x/src/cpu/cpu_instr.c:1515-1553` carries a comment block
  verified against ND-110-RASK.uc: frame words (LINK/PREVB/STP/SMAX/ERRCODE)
  are DATA -> APT; inline parameters in the instruction stream -> PT.
- `ndfunc_init` (cpu_instr.c:1556-1590): parameter reads `MemoryRead(gPC + n, 0)`
  (UseAPT=0 -> PT), frame writes `MemoryWrite(..., start + n, 1, 2)` (UseAPT=1
  -> APT).
- `ndfunc_entr` (1592-1613): demand read at gPC with UseAPT=0; SMAX/STP reads
  `MemoryRead(gB - 125, 1)` / `MemoryRead(gB - 126, 1)` and all frame writes
  with UseAPT=1.
- `ndfunc_leave` (1614-1620) and `ndfunc_eleav` (1622-1631): all frame
  accesses with UseAPT=1.
- What UseAPT means: `mapVirtualToPhysical` (cpu_mms.c:398, gate at 431)
  applies the alternative page table only `if ((STS_PTM) && (UseAPT))` - i.e.
  UseAPT=1 means "APT when PTM is set, normal PT otherwise", exactly the
  hardware qualifier. The PTM resolution is internal to the MMU function; the
  instruction code correctly passes the static APT qualifier and never tests
  PTM itself.

So the RetroCore fix (frame accesses must be APT-qualified) brings C# into
line with what nd100x already did.

---

## (d) Could not verify / limitations

1. How RetroCore performs the SCSI BOOT load (nd100x has SCSI_Boot,
   deviceSCSI.c:588-668; no equivalent exists in
   NDBusDiscControllerSCSI.cs). RetroCore presumably boots through machine- or
   microcode-level logic elsewhere in the repo - not located in this pass.
   UNVERIFIED.
2. Whether any SINTRAN component actually issues MODE SENSE(6) against the
   SCSI disk (bears on the impact of D4). The carved driver and the observed
   mount traces do not show it, but the full command surface of DIR-MM /
   SCSI-TV against a disk target was not enumerated. UNVERIFIED.
3. Exact equality of the C# `SCSIFullDevice.cs` step machine against
   `scsiDevice.c` line-by-line: verified for the load-bearing pieces
   (command-length groups, IDENTIFY/LUN, timer post-decrement, buf_control
   flow, sense layout, bus wire-OR and the regen_ctrl self-notify exclusion -
   SCSIFullDevice.cs:134/432/543-570/937-957, SCSIBus.cs:82-129/208-250 vs
   scsiDevice.c:332-364/370-601, scsiBus.c:88-150), but the C# file is 1207
   lines and every branch was NOT diffed. Residual risk: low (the C file is a
   stated port with divergences called out in comments). PARTIALLY VERIFIED.
4. The nd100x IDENT dispatch chain (does the machine layer call SCSI_Ident on
   every device per level and rely on the 0 return?). Inferred from
   SCSI_Ident's `interruptBits` check (deviceSCSI.c:564-571); the calling code
   in the machine layer was not read. UNVERIFIED.
5. NO_SCSI_DELAY: the comparison assumes RetroCore is built with
   `#define NO_SCSI_DELAY` active (it is, NCR5386SCSI.cs:21, uncommented), and
   nd100x hard-codes that configuration (ncr5386.c:14-22). If RetroCore were
   rebuilt with MAME timing, the two would diverge in timing (not in logic).
6. `Numeric.ParseIntValue("144300")` octal parsing in C# was taken on the
   authority of the nd100x port comment (deviceSCSI.c:711-717); Numeric.cs
   itself was not read. UNVERIFIED (the resulting addresses are confirmed
   equal by the TPE listing embedded at NDBusDiscControllerSCSI.cs:2588-2594:
   hardware dev 144300, ident 140440).

---

## Alignment applied (2026-07-17)

The following divergences were aligned. Line numbers below refer to the files
AFTER the edits (both trees uncommitted). VERIFIED = byte-read from the edited
code and compile/test-checked; INFERRED marked where applicable.

### D1 / 3e - "interrupt when ready" -> ADDED to nd100x
- Direction: C# -> nd100x. Evidence: ND doc bit-0 text ("gives interrupt ...
  as soon as the controller is ready") and the C# branch is live-verified
  against SINTRAN.
- nd100x `src/devices/scsi/deviceSCSI.c` SCSI_Write WCONT: the activate tail
  now has an else-branch - WCONT with bit 0 (enable int) set, bit 2 (activate)
  clear, while readyForTransfer is true, calls
  `Device_GenerateInterrupt(self, self->interruptLevel)` immediately.
  Mirrors NDBusDiscControllerSCSI.cs Write WCONT else-branch (now ~1206-1213).
- C# side: unchanged (was already correct). VERIFIED.

### D2 / 5g - chip own-ID plumbing -> BOTH sides set ID Register AND SourceID
- Evidence weighed: the embedded NPL driver listing shows the driver only
  READS the IDs (ROIDN=52 "READ OWN ID NUMBER", RSOUI=56 "READ SOURCE ID" -
  used on the reconnect path, listing line 067420); there is no WOIDN write in
  the listing, so on real hardware the own ID (7, TW1) is STRAPPED. Datasheet
  facts present in both repos: own ID lives in the ID Register (reg 5,
  "strapped ID" mode - WOIDN doc comment in the C# Register enum); the Source
  ID register (reg 7) is READ-ONLY (ncr5386.h:35). Neither emulator models ID
  straps, and BOTH chip cores use `sourceID` as the arbitration own-ID, so the
  documented behaviour (arbitrate as ID 7, ROIDN reads back 7) is obtained by
  setting BOTH latches. This keeps the live-verified C# arbitration value
  (bit 0x80) and the documented ROIDN readback.
- C# `NDBusDiscControllerSCSI.cs`: SetSCSIIdNumber (~789-825), Reset
  (~831-838) and the WCONT Clear Device path now write TW1 to BOTH
  `SCSIRegisters.IDRegister` and `SCSIRegisters.SourceID`, with the rationale
  in comments (the stale "A/B-TEST" comments were replaced). Clear Device also
  re-asserts both after `ncr5386.DeviceReset()` (a strap cannot be reset),
  matching nd100x.
- nd100x `deviceSCSI.c` SCSI_Reset and Clear Device: added
  `NCR5386_Write(&data->ncr, NCR_REG_SOURCE_ID, SCSI_CONTROLLER_ID)` next to
  the existing NCR_REG_ID write; the "(SourceID is read-only)" justification
  comment was expanded with the full rationale.
- nd100x `ncr5386.c` NCR_StepState arbitration comment: the STALE claim
  "RetroCore ... nothing ever writes sourceID" was corrected.
- Net effect: both emulators now arbitrate as ID 7 (0x80), ROIDN returns 7 on
  both, RSOUI returns 7 on both (real HW would latch the reselecting target's
  ID there - noted in comments; reselection is unimplemented on both sides).
  VERIFIED (code); arbitration equivalence INFERRED from the shared StepState
  logic, not re-traced live.

### D6a / 3c - test-mode MAR increment on RLMAR read -> ADDED to nd100x
- Direction: C# -> nd100x. Evidence: SINTRAN NEWPH odd-byte recovery (driver
  listing 070565-070601: WCONT=150 octal, RLMAR read "FORCE LAST BYTE TO
  MEMORY", MAR check expects the incremented value).
- nd100x `deviceSCSI.c` SCSI_Read RLMAR: `if (data->testMode)
  SCSI_IncrementMAR(data);` after latching rval, with the NEWPH rationale in a
  comment. C# side unchanged. VERIFIED.

### D6b/c / 1c-1d - REDAT/WRDAT backing store -> C# FIXED to the single Data Register
- Direction: nd100x -> C#. Evidence: ND doc wording ("data is taken from the
  Data Register") and the fact that the C# 1KW ring was never read by the
  test-mode DMA path (WRDAT fed `DataBuffer[]`, test-mode DMA used
  `regs.ReadWriteData`, which nothing wrote).
- C# `NDBusDiscControllerSCSI.cs`: REDAT now returns `regs.ReadWriteData`
  (~884-897), WRDAT now sets it (~1087-1099); the ring
  (DataBuffer/bufferPointer/readbufferPointer, incl. the off-by-one
  `ushort[BUFFER_MAX]` allocation) was removed with an explanatory comment at
  the old declaration site (~631-636) and the Clear Device pointer resets were
  dropped with it. nd100x side unchanged. VERIFIED.

### D4 / 6c - MODE SENSE(6) -> PORTED to nd100x
- Direction: C# -> nd100x. `src/devices/scsi/scsiHDD.c`: new
  `SCSIHDD_CommandModeSense()` (inserted just before SCSIHDD_Command) ports
  SCSIHDD.CommandModeSense byte-for-byte: 4-byte header + 8-byte block
  descriptor + pages 0x00/0x01/0x02/0x03/0x04/0x08/0x30, page 0x3F iterated
  DOWNWARDS 0x3e..0x00 like the C# loop, unknown single page -> CHECK
  CONDITION + ILLEGAL REQUEST 0x24. The block-descriptor "number of blocks"
  deliberately carries `DiskSCSI_LastLBA()` because the C# fills it from
  hdinfo.DiskSizeInBlocks which actually holds the LAST LBA (count-1) -
  copied verbatim and commented on the C side. Dispatch: new
  `case SC_MODE_SENSE_6:` after MODE SELECT(6). LUN rejection is covered by
  SCSIHDD_Command's existing lun!=0 preamble (noted in the function comment).
  VERIFIED (compiles clean; byte layout diffed against SCSIHDD.cs:629-831).

### D5 / 6d - FORMAT UNIT -> C# destructive loop REMOVED (no-op like nd100x)
- Direction: nd100x -> C#. Evidence: the C# loop wrote a zero block to LBA
  `cyl * head * sector` (a product, not an LBA), repeatedly zeroing block 0
  and a scatter of wrong blocks - image corruption on a real format. nd100x
  deliberately no-ops.
- C# `SCSIHDD.cs` SC_FORMAT_UNIT (~394-415): the CHS write loop is gone; the
  command is still accepted and completes with GOOD status, log + comment
  explain the wrong-LBA math and why the image is left untouched. VERIFIED.

### Not changed (deliberately)
- D3 (C# HasFlag arbitration gate), D7 (per-GO DMA byte-counter reset), D8
  (Chip Reset residue): outside the five commissioned alignment items; still
  open as documented above.

### Build / test evidence (2026-07-17)
- C#: `dotnet build Emulated.HW/Emulated.HW.csproj` -> 0 errors (warnings
  pre-existing). ND SCSI regression tests
  (`dotnet test --filter FullyQualifiedName~NCR5386`, includes the write-loop
  oracle NCR5386_WriteLoop_test) -> 3/3 passed.
- nd100x: full `make` -> 100% built, deviceSCSI.c / ncr5386.c / scsiHDD.c
  recompiled with zero warnings.
- NOTE: the broader `--filter FullyQualifiedName~SCSI` run shows 15 failing
  tests (NCR5380/Sun2/Sun3 trace-replay suites). VERIFIED PRE-EXISTING: the
  same tests fail with this alignment's two C# files stashed back to their
  pre-alignment state, and none of the failing tests reference FORMAT UNIT,
  NDBusDiscControllerSCSI, or the NCR5386 files touched here (the RetroCore
  working tree carries many unrelated uncommitted changes from other work).
