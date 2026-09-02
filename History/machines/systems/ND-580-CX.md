# ND-580/CX

*Several ND-570 processors in one machine, with an ND-110 running the system.*

|  |  |
|---|---|
| **Introduced** | 1986 |
| **Type** | Multiprocessor configuration of [ND-500](../cpu/nd-5xx/ND-500.md) |
| **Word length** | 32-bit |
| **Needs a host?** | **Yes - contains its own ND-110/CX** |
| **Replaced** | - |
| **Replaced by** | ND-5900 |
| **Survivors** | Not recorded |

## The short version

ND's multiprocessor answer on the ND-500 line: two to four ND-570 CPUs plus one
ND-110/CX as the I/O processor, sold as models 20, 30 and 40. ND used the naming
**ND-580/n** and **ND-590n**, where n is the number of processors.

## Specification

| Item | Value |
|---|---|
| Models | 20, 30 and 40 - two, three and four ND-570 CPUs |
| I/O processor | One ND-110/CX |
| Memory | Shared between the CPUs |
| Naming | ND-580/n and ND-590n, n = 2, 3 or 4 |
| Architecture | As [ND-500](../cpu/nd-5xx/ND-500.md) |

## What was new

- Multiple 32-bit CPUs on one shared memory, still fed by a single 16-bit machine.

## Sources

- **Secondary**: [ndwiki ND-500](../../sources/ndwiki-nd-500.md)
- **Secondary**: [ndwiki history](../../sources/ndwiki-history-of-norsk-data.md), 1986 entry

---

*Full context: [MACHINE-TIMELINE.md](../../MACHINE-TIMELINE.md).*
