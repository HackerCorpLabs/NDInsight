# SCSI Device Documentation

**SCSI Disk and Optical Drive Controllers for SINTRAN III**

---

## Overview

Documentation for SCSI (Small Computer System Interface) disk and optical drive controllers used in ND-100 systems running SINTRAN III.

---

## Master Index

**Start here:** [SCSI-Master-Index.md](SCSI-Master-Index.md) - Complete index of all SCSI documentation

---

## Quick Reference

| Document | Purpose | Lines |
|----------|---------|-------|
| [SCSI-Master-Index.md](SCSI-Master-Index.md) | Complete documentation index | - |
| [SCSI-C#-Implementation-Guide.md](SCSI-C%23-Implementation-Guide.md) | C# emulator implementation | ~450 |
| [SCSI-Commands-Analysis.md](SCSI-Commands-Analysis.md) | SCSI command set analysis | ~380 |
| [SCSI-INQUIRY-Analysis.md](SCSI-INQUIRY-Analysis.md) | INQUIRY command details | ~290 |
| [scsi_inquiry_explained.md](scsi_inquiry_explained.md) | INQUIRY explanation | ~120 |
| [SCSI-Optical-Commands-Addendum.md](SCSI-Optical-Commands-Addendum.md) | Optical drive commands | ~95 |
| [SCSI-controller.md](SCSI-controller.md) | Controller documentation | ~85 |

---

## NPL Source Analysis

| Document | NPL Source | Purpose |
|----------|------------|---------|
| [IP-P2-SCSI-DISK.md](IP-P2-SCSI-DISK.md) | IP-P2-SCSI-DISK.NPL | SCSI disk driver analysis |
| [IP-P2-SCSI-DRIV.md](IP-P2-SCSI-DRIV.md) | IP-P2-SCSI-DRIV.NPL | SCSI driver implementation |
| [IP-P2-SCSI-DRIV-ANALYSIS.md](IP-P2-SCSI-DRIV-ANALYSIS.md) | IP-P2-SCSI-DRIV.NPL | Detailed driver analysis |
| [IP-P2-SCSI-OPDI.md](IP-P2-SCSI-OPDI.md) | IP-P2-SCSI-OPDI.NPL | Optical disk implementation |
| [IP-P2-SCSI-OPDI-Analysis.md](IP-P2-SCSI-OPDI-Analysis.md) | IP-P2-SCSI-OPDI.NPL | Optical disk analysis |

---

## Topics Covered

### SCSI Protocol
- Command set (SCSI-1, SCSI-2)
- Command Descriptor Blocks (CDB)
- Status and sense data
- Message system
- Phases (arbitration, selection, data, status)

### Commands Documented
- **INQUIRY** - Device identification
- **READ(6), READ(10)** - Read sectors
- **WRITE(6), WRITE(10)** - Write sectors
- **TEST UNIT READY** - Device status
- **REQUEST SENSE** - Error information
- **MODE SENSE/SELECT** - Device configuration
- **READ CAPACITY** - Disk geometry
- Optical-specific commands (SCSI-2)

### Driver Implementation
- Interrupt handling (Level 11)
- DMA operations
- Error recovery
- Device datafield structure
- Queue management

### Emulation
- C# classes for SCSI emulation
- Virtual disk implementation
- Command processing
- Status handling

---

## For Emulator Developers

**Start with:** [SCSI-C#-Implementation-Guide.md](SCSI-C%23-Implementation-Guide.md)

**Key Classes:**
```csharp
SCSIController          // Main controller
SCSIDisk                // Virtual disk
SCSICommand             // Command processing
SCSIDevice              // Base device class
```

**Implementation Steps:**
1. Read [SCSI-Commands-Analysis.md](SCSI-Commands-Analysis.md) - understand command set
2. Read [SCSI-C#-Implementation-Guide.md](SCSI-C%23-Implementation-Guide.md) - implement classes
3. Reference [IP-P2-SCSI-DRIV.md](IP-P2-SCSI-DRIV.md) - match SINTRAN driver behavior

---

## Related Documentation

### SINTRAN OS
- [../../OS/15-DISK-IO-SUBSYSTEM.md](../../OS/15-DISK-IO-SUBSYSTEM.md) - Disk I/O architecture
- [../../OS/18-DEVICE-DRIVER-FRAMEWORK.md](../../OS/18-DEVICE-DRIVER-FRAMEWORK.md) - Driver framework

### Other Devices
- [../HDLC/](../HDLC/) - HDLC communication controller

---

## Version History

| Date | Version | Changes |
|------|---------|---------|
| 2025-10-17 | 1.0 | Initial SCSI documentation structure |

---

**Parent:** [../README.md](../README.md) - Device Documentation

