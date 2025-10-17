# SINTRAN Device Documentation

**Hardware Device Drivers and Controllers**

---

## Overview

This folder contains detailed documentation for hardware devices supported by SINTRAN III, including communication controllers, disk controllers, and I/O interfaces.

---

## Folders

| Folder | Device Type | Files | Description |
|--------|-------------|-------|-------------|
| [HDLC/](HDLC/) | Communication | 30+ | HDLC/SDLC serial communication controller (COM5025) |
| [SCSI/](SCSI/) | Storage | 10+ | SCSI disk and optical drive controllers |

---

## HDLC Communication Controller

**Location:** [HDLC/](HDLC/)

**Purpose:** Documentation for HDLC (High-Level Data Link Control) / SDLC communication using the SMC COM5025 controller chip.

### Key Documents

| Document | Purpose |
|----------|---------|
| [01-HDLC-Hardware-Reference.md](HDLC/01-HDLC-Hardware-Reference.md) | Hardware interface and registers |
| [Quick-Reference-Card.md](HDLC/Quick-Reference-Card.md) | Quick reference for development |
| [README.md](HDLC/README.md) | Complete HDLC documentation index |

### Topics Covered
- COM5025 hardware interface
- Register definitions and usage
- Interrupt handling (HIINT, HOINT)
- DMA operations
- Protocol implementation (LAPB, X.25)
- Packet structure and analysis
- Emulator implementation

---

## SCSI Disk Controllers

**Location:** [SCSI/](SCSI/)

**Purpose:** Documentation for SCSI (Small Computer System Interface) disk and optical drive controllers.

### Key Documents

| Document | Purpose |
|----------|---------|
| [SCSI-Master-Index.md](SCSI/SCSI-Master-Index.md) | Complete SCSI documentation index |
| [SCSI-C#-Implementation-Guide.md](SCSI/SCSI-C%23-Implementation-Guide.md) | C# emulator implementation |
| [SCSI-Commands-Analysis.md](SCSI/SCSI-Commands-Analysis.md) | SCSI command set |
| [SCSI-INQUIRY-Analysis.md](SCSI/SCSI-INQUIRY-Analysis.md) | INQUIRY command details |

### Topics Covered
- SCSI command set
- INQUIRY command and device identification
- Disk I/O operations
- Optical drive support (SCSI-2)
- Driver implementation (IP-P2-SCSI-DRIV.NPL analysis)
- C# emulator classes

---

## General Device Documentation

**Also see:**
- [DEVICE_DRIVER_SETUP_DOCUMENTATION.md](DEVICE_DRIVER_SETUP_DOCUMENTATION.md) - General device driver setup

**Related OS Documentation:**
- [../OS/18-DEVICE-DRIVER-FRAMEWORK.md](../OS/18-DEVICE-DRIVER-FRAMEWORK.md) - SINTRAN device driver architecture
- [../OS/15-DISK-IO-SUBSYSTEM.md](../OS/15-DISK-IO-SUBSYSTEM.md) - Disk I/O subsystem

---

## Quick Start

### Understanding HDLC Communication

1. Read [HDLC/README.md](HDLC/README.md) for overview
2. Read [HDLC/01-HDLC-Hardware-Reference.md](HDLC/01-HDLC-Hardware-Reference.md) for hardware details
3. Review [HDLC/Quick-Reference-Card.md](HDLC/Quick-Reference-Card.md) for quick lookup

### Understanding SCSI Controllers

1. Read [SCSI/SCSI-Master-Index.md](SCSI/SCSI-Master-Index.md) for overview
2. Read [SCSI/SCSI-Commands-Analysis.md](SCSI/SCSI-Commands-Analysis.md) for command set
3. Use [SCSI/SCSI-C#-Implementation-Guide.md](SCSI/SCSI-C%23-Implementation-Guide.md) for emulation

### For Emulator Developers

**HDLC Emulation:**
- [HDLC/implementation/Emulator-Implementation-Guide.md](HDLC/implementation/Emulator-Implementation-Guide.md)
- Implements COM5025 chip behavior
- Handles interrupts and DMA
- Packet assembly/disassembly

**SCSI Emulation:**
- [SCSI/SCSI-C#-Implementation-Guide.md](SCSI/SCSI-C%23-Implementation-Guide.md)
- SCSI command processing
- Virtual disk implementation
- Optical drive simulation

---

## Device Architecture in SINTRAN

### Interrupt Levels

| Level | Device Type | Examples |
|-------|-------------|----------|
| 10 | Output | Line printers, plotters |
| 11 | Mass Storage | SCSI disks, SMD disks, floppy |
| 12 | Input/Communication | Terminals, HDLC controllers |
| 13 | Real-Time Clock | System timer |

### Device Datafield (DCB)

All devices have a **datafield** (Device Control Block) containing:
- Reservation/waiting queue links
- Device status
- Hardware device number
- Monitor function pointers
- Device-specific data

**See:** [../OS/18-DEVICE-DRIVER-FRAMEWORK.md](../OS/18-DEVICE-DRIVER-FRAMEWORK.md)

---

## Documentation Statistics

| Category | Files | Size |
|----------|-------|------|
| HDLC | 30+ | ~350KB |
| SCSI | 10+ | ~125KB |
| **Total** | **40+** | **~475KB** |

---

## Related Documentation

### OS Layer
- [../OS/18-DEVICE-DRIVER-FRAMEWORK.md](../OS/18-DEVICE-DRIVER-FRAMEWORK.md) - Driver framework
- [../OS/15-DISK-IO-SUBSYSTEM.md](../OS/15-DISK-IO-SUBSYSTEM.md) - Disk I/O
- [../OS/13-INT14-HANDLER-DETAILED.md](../OS/13-INT14-HANDLER-DETAILED.md) - Interrupt handling

### Protocols
- [../TAD/](../TAD/) - TAD protocol (uses HDLC)
- [../TAD/TAD-HDLC-Encapsulation.md](../TAD/TAD-HDLC-Encapsulation.md) - HDLC encapsulation

### Emulator
- [../Emulator/](../Emulator/) - Emulator implementation guides

---

## Version History

| Date | Version | Changes |
|------|---------|---------|
| 2025-10-17 | 1.0 | Initial device documentation structure |

---

**Parent:** [../README.md](../README.md) - SINTRAN Documentation  
**Sibling:** [../OS/](../OS/) - Operating System Documentation

