# Large cabinet (5xx)

*The full-size coprocessor cabinet. Holds a 1xx machine and a 32-bit CPU together.*

|  |  |
|---|---|
| **Used from** | 1981 |
| **Type** | Cabinet |
| **Family** | [Coprocessor systems](../systems/) |
| **Takes which CPU** | One [1xx CPU](../cpu/nd-1xx/) as I/O processor, **plus** one or more [5xx CPUs](../cpu/nd-5xx/) |
| **Coprocessor?** | **Yes - that is the point** |

## The short version

The cabinet for an ND-500 or ND-5000 system. It is a different design from the
[1xx large cabinet](Large-1xx.md), because it has to hold two machines: a complete
ND-100-line computer that runs SINTRAN III and does all the input and output, and
the 32-bit CPU or CPUs that do the computing.

A system in this cabinet can run **everything a standalone 1xx machine runs**, plus
programs written for the 32-bit CPU.

## Specification

| Item | Value |
|---|---|
| Format | Full-size floor-standing, distinct design from the 1xx large cabinet |
| Backplanes | **Two or more** - the ND-100 bus, and the 5xx CPU and support boards |
| I/O processor | One 1xx CPU board plus its memory and I/O cards |
| Compute CPUs | One to four 5xx CPUs, sharing memory |
| Multiprocessor | ND-580/n and ND-590n on the ND-500 line; ND-5900 models 2/3/4 on the ND-5000 line |
| Host link | PCB 3022 DMA registers (ND-500) or octobus (ND-5000) |

## What goes in it

| Slot / backplane | Holds |
|---|---|
| ND-100 bus backplane | 1xx CPU, memory, I/O controllers - a complete standalone machine |
| 5xx backplane | ND-500 or ND-5000 CPU boards and their support boards |
| Shared | Multiport memory reachable by both sides |

## Notes

- This is the cabinet that makes group B what it is: a 1xx machine and a 32-bit machine in one box.
- ND-5000 Satellite and **Technostation** cabinets exist too (drawings B2C20) - not yet documented here.

## Sources

- **Primary**: `Reference-Manuals/500/ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE.md` Table 2 (large cabinet configurations)
- **Repo**: `SINTRAN/ND500/ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md`

---

*Index: [../README.md](../../README.md). Full context: [../../MACHINE-TIMELINE.md](../../MACHINE-TIMELINE.md).*
