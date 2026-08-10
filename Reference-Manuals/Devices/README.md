# Device Controllers

**Reference and analysis material for Norsk Data device controllers**

[← Reference Manuals index](../README.md)

---

## 📖 Overview

This sub-collection contains original Norsk Data **device-controller** manuals and the analysis
material derived from them - the primary source documents for emulating or reverse-engineering ND
peripheral controllers.

Currently covered:

- **Ethernet II Controller** (ND-12.055.1) plus a unit-test specification derived from it - the ND
  Ethernet II / LANCE-based network controller and its COSMOS integration.
- **ECC Disk Controller** (ND-11.013.01A) - the SMD disk controller, including its full IOX
  programming specification, error-correcting-code scheme, track/sector format and a SINTRAN III
  SMD disk driver listing.
- **SMD Disk Controller** (ND-11.020.01) - the later 15 MHz SMD controller.
- **Winchester Disk Controller** (ND-11.015.01) - the 5 1/4 inch (ST506) & 8 inch Winchester disk
  controller.
- **Floppy Disk System** (ND-11.012.01) - the floppy subsystem, drive and controller.
- **NORD-10/HAWK Disk Controller** (ND-11.010.01) - the earlier HAWK cartridge-disk controller.

Two further NORD-10-era storage manuals - the **HAWK Disk System** (ND-11.009.01) and the
**Cartridge Disc System for NORD-10** (ND-11.008.01 / 01A) - are filed with the rest of the NORD-10
documentation in [../10/](../10/README.md). The **Disk Mirroring Operator Guide** (ND-30.070.1) is
SINTRAN operator software rather than controller hardware and lives one level up at
[../ND-30.070.1 EN Disk Mirroring Operator Guide.md](../ND-30.070.1%20EN%20Disk%20Mirroring%20Operator%20Guide.md).

The companion software-side manual, **ND-60.197.01 Ethernet Basic Software Programmer Guide** (the
host-to-controller programming contract - service points, datagram API, DIX vs IEEE framing, and the
four-physical-address multi-protocol scheme), lives one level up at
[../ND-60.197.01 EN Ethernet Basic Software Programmer Guide.md](../ND-60.197.01%20EN%20Ethernet%20Basic%20Software%20Programmer%20Guide.md).

### Total Collection

| Category | Documents | Total Lines |
|----------|-----------|-------------|
| Manuals | 6 | 29,717 |
| Notes / Test specs | 1 | 2,472 |
| **Total** | **7** | **~32,000** |

---

## 🗂️ Index

### Ethernet II Controller

| Document | Document # | Lines | Description |
|----------|-----------|-------|-------------|
| **Ethernet II Controller** | ND-12.055.1 EN | 2,423 | Ethernet II Controller reference manual - control/status registers, I/O address space, LANCE integration, interrupts, transceiver control, COSMOS statistics. Appendix B compares the 802.3 and Ethernet frame formats; Appendix C tabulates the COSMOS stack (`LLC1 / MAC`) against the ARPA stack (`LLC1 / DIX`) |
| **Ethernet II Controller - Unit Test Specification** | - | 2,472 | *Note.* Comprehensive unit-test / analysis specification for the ND-12.055.1 controller (initialization, register, I/O, LANCE, interrupt, memory, loopback, transceiver, error-handling and COSMOS-statistics tests) - a test/analysis document, not an original manual |

**Key Topics:** Ethernet II, LANCE controller, control/status registers, I/O address space,
interrupts, transceiver control, loopback, COSMOS network statistics, DIX 2.0 vs IEEE 802.3 framing

### Disk Controllers

| Document | Document # | Lines | Description |
|----------|-----------|-------|-------------|
| **Error Correction Control (ECC) Disk Controller** | ND-11.013.01A EN | 6,550 | SMD disk controller reference manual (original printing 10/78, revision A 06/79) - general description, addressing concept, control/status words, DMA transfer, interrupt generation and handling, debugging guide and per-logic-board descriptions. Appendix G gives the track/sector format, H the backwiring, I the connector lists, J a theoretical introduction to error-correcting codes, K the test programs, L a **SINTRAN III SMD disk driver routine** listing, M the disk specifications and N the full **programming specifications** |
| **SMD Disk Controller** | ND-11.020.01 | 5,244 | The **15 MHz SMD disk controller** (the manual's own section title is "15MHZ SMD DISK CONTROLLER"; the sintran.com mirror files it under a mis-transcribed "15MHZ SMB CONTROLLER"). 136 pages, with preface, table of contents, list of illustrations and an appendix. The later companion to the ECC controller above |
| **Winchester Disk Controller** | ND-11.015.01 | 1,209 | The **5 1/4 inch (ST506) & 8 inch Winchester disk controller** (© 1983). Sections: description (disk drive, controller, bad-track handling), **programming specifications** (IOX address decoding, memory/block address registers, the control word with transfer modes M0-M7, read status register), detailed hardware description (IOX & IDENT, DMA, Shugart drive connection on the 3038 8" controller, serial data), test procedure (DISC-TEMA, SUPER-RANDOM), block/logic diagrams for both the 8" and ST506 variants, Fairchild 9401 description, arrangement drawings and cable signal definitions |
| **Floppy Disk System** | ND-11.012.01 | 10,007 | Floppy disk subsystem manual, 220 pages - the largest of the storage manuals. Organised into numbered sections (System/General, diskette loading and handling, write protection, ...) plus appendixes |
| **NORD-10/HAWK Disk Controller** | ND-11.010.01 | 4,284 | The HAWK cartridge-disk controller for NORD-10, 105 pages. Section 1 is the **programming specifications**, opening with the disk device register addresses. Appendix A diagrams, B signal definition list, C backwiring print, D controller activity indicators, E N-10/HAWK physical layout |

**Key Topics:** SMD disk interface, 15 MHz SMD controller, ECC error-correcting codes, Winchester
ST506 interface, IOX programming specification, disk device register addresses, track/sector format,
DMA transfer, interrupt handling, SINTRAN III SMD disk driver, floppy subsystem, HAWK cartridge
disk, backwiring and connector lists, signal definition lists, test programs

> The reverse-engineered, emulator-facing counterpart to these manuals is
> [../../SINTRAN/Devices/SMD/SMD-CONTROLLER-PROGRAMMING-GUIDE.md](../../SINTRAN/Devices/SMD/SMD-CONTROLLER-PROGRAMMING-GUIDE.md),
> which derives the SMD register map and I/O sequences from the nd100x emulator and the SINTRAN NPL
> sources rather than from these documents - so the two are independent descriptions of the same
> hardware and can be cross-checked against each other.

---

*Part of the [NDInsight](../../README.md) Norsk Data / SINTRAN III documentation and preservation
project. All manuals are Norsk Data A.S publications; copyrights belong to their original holders
and are reproduced here for historical and technical reference.*
