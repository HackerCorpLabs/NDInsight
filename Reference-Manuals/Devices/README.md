# Device Controllers

**Reference and analysis material for Norsk Data device controllers**

[← Reference Manuals index](../README.md)

---

## 📖 Overview

This sub-collection contains the **Ethernet II Controller** reference manual and an accompanying
unit-test specification derived from it. It is the primary source material for emulating or
reverse-engineering the ND Ethernet II / LANCE-based network controller and its COSMOS integration.

The companion software-side manual, **ND-60.197.01 Ethernet Basic Software Programmer Guide** (the
host-to-controller programming contract - service points, datagram API, DIX vs IEEE framing, and the
four-physical-address multi-protocol scheme), lives one level up at
[../ND-60.197.01 EN Ethernet Basic Software Programmer Guide.md](../ND-60.197.01%20EN%20Ethernet%20Basic%20Software%20Programmer%20Guide.md).

### Total Collection

| Category | Documents | Total Lines |
|----------|-----------|-------------|
| Manuals | 1 | 2,423 |
| Notes / Test specs | 1 | 2,472 |
| **Total** | **2** | **~4,900** |

---

## 🗂️ Index

### Ethernet II Controller

| Document | Document # | Lines | Description |
|----------|-----------|-------|-------------|
| **Ethernet II Controller** | ND-12.055.1 EN | 2,423 | Ethernet II Controller reference manual - control/status registers, I/O address space, LANCE integration, interrupts, transceiver control, COSMOS statistics. Appendix B compares the 802.3 and Ethernet frame formats; Appendix C tabulates the COSMOS stack (`LLC1 / MAC`) against the ARPA stack (`LLC1 / DIX`) |
| **Ethernet II Controller - Unit Test Specification** | - | 2,472 | *Note.* Comprehensive unit-test / analysis specification for the ND-12.055.1 controller (initialization, register, I/O, LANCE, interrupt, memory, loopback, transceiver, error-handling and COSMOS-statistics tests) - a test/analysis document, not an original manual |

**Key Topics:** Ethernet II, LANCE controller, control/status registers, I/O address space,
interrupts, transceiver control, loopback, COSMOS network statistics, DIX 2.0 vs IEEE 802.3 framing

---

*Part of the [NDInsight](../../README.md) Norsk Data / SINTRAN III documentation and preservation
project. All manuals are Norsk Data A.S publications; copyrights belong to their original holders
and are reproduced here for historical and technical reference.*
