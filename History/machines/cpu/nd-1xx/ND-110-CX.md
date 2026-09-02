# ND-110/CX

*The fast ND-110, known inside ND as RASK. Also the I/O processor for ND-500 systems.*

|  |  |
|---|---|
| **Introduced** | 1986 |
| **Type** | Configuration of [ND-110](ND-110.md) |
| **Word length** | 16-bit |
| **Needs a host?** | No, runs on its own |
| **Replaced** | ND-100/CX |
| **Replaced by** | ND-120/CX (1987) |
| **Survivors** | Not separately recorded |

## The short version

A faster ND-110 - between 1.5 and 3.5 times the base machine - introduced in
1986 as part of a general refresh of the ND-100 range that also brought the ND-110
Compact and Satellite. Existing systems could be upgraded. Beyond being a machine
in its own right, it became the **I/O processor inside ND-500 systems**, doing the
operating system and input/output for the 32-bit CPU.

## Specification

| Item | Value |
|---|---|
| CPU | ND-110 with the CX microcode. Three new integrated circuits cut the device count from **365 to 228** |
| Speed | 1.5 to 3.5 times an ND-110 |
| Cache | Increased over the ND-110 |
| Power | 40 percent less than the machine it replaced |
| Instruction set | The CX additions - decimal, MOVEW, TSET, RDUS, segment changes |
| Second role | Used as the I/O processor in ND-500 systems, and in the ND-5400 and ND-5500 |

## What was new

- Device count almost halved, onto a single module.
- Became the standard front-end for the 32-bit machines, not just a machine of its own.

## Sources

- **Primary**: `Reference-Manuals/ND-06.026-1-EN ND-110 Functional Description.md`; `Reference-Manuals/500/ND-05.020.01 EN ND-5000 Hardware Description.md` for the I/O processor role
- **Secondary**: [English Wikipedia](../../../sources/en-wikipedia-nord-100.md)

---

*Full context: [MACHINE-TIMELINE.md](../../../MACHINE-TIMELINE.md).*
