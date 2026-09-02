# Source: ndwiki article "NORD-12"

- **Live page**: https://www.ndwiki.org/wiki/NORD-12
- **Copy used here**: Wayback Machine snapshot of 5 March 2024
  http://web.archive.org/web/20240305063425/http://www.ndwiki.org/wiki/NORD-12
- **Why the archive**: the live ndwiki is behind an Anubis proof-of-work gate.
- **Fetched**: 2026-08-27, by Ronny's request.

**Status: SECONDARY, but unusually close to primary.** The article labels itself a
stub, yet its three substantial sections are cited to the *NORD-12 Reference
Manual* - the General Characteristics section to the manual's Introduction, and
the NORD-10/NORD-12 differences list to its Appendix C. It reads as a
transcription rather than a summary, the same pattern as the NORD-1 articles.
**We do not hold the NORD-12 Reference Manual**; `Reference-Manuals/10/` has the
NORD-10/S manual but no NORD-12 one. So this page is currently our only route to
that document, and its claims should be marked as reached through a wiki.

**Why this page matters beyond the NORD-12** - it carries the sentence:

> "Memory parity is an option in which case the word-length is 18 bits with one
> parity bit for each 8-bit byte."

That is ND documenting a 16-data-plus-2-parity = 18-bit memory word. It bears
directly on the open "18-bit core" question in
[NORD-1.md](../NORD-1.md), though it is about a later machine.

---

ROW| 

 | 
  This article is a stub. You can improve NDWiki by expanding it. | 

### Contents

- 1 General Characteristics

- 2 Peripheral Equipment

- 3 Software

- 4 Differences between NORD-10 and NORD-12

- 5 Remaining machines

- 6 Sources

### General Characteristics

The NORD-12 computer system is a compact mini-computer system with an unusually large instruction set.

The NORD-12 belongs to the NORD-10 family of 16-bit computers where program compatibility with the NORD-10 is assured by the fact that both the instruction set and the Read Only Memory which controls the instruction execution is common to both the NORD-10 and the NORD-12. For readers familiar with the NORD-10, we recommend you read Appendix C which lists all differences between NORD-12 and NORD-10.

Front view of the NORD-12 in the Telemuseum collection.

A basic instruction set is common to all NORD-12 machines, and this set is highly optimized to produce effective code; hardware floating point arithmetic is standard as are the instructions to manipulate individual bits at high speed.

The register structure and addressing scheme facilitate the processing of structured data with high efficiency.

The NORD-12 is micro-programmed, and all NORD-12 instructions are executed by means of a micro-program located in a very fast (65 ns) read only memory. Micro-programming gives the NORD-12 computer flexibility and a very large growth potential. New instructions may be optimized for a particular use.

NORD-12 provides up to 1024 customer-specified instructions. These instructions are micro-programmed in a programmable read-only memory, which is added onto the standard read-only memory.

Micro-programming in NORD-12 is also used to control the operator's panel and to perform operator communication between the operator and the console Teletype or display.

Bootstrap loaders both for character-oriented devices and mass storage devices are also controlled by a micro-program.

The NORD-12 uses MOS type memories with memory size from 4K to 64K words; memory increment size is 4K. Memory parity is an option in which case the word-length is 18 bits with one parity bit for each 8-bit byte.

Another option is a power fail/auto restart system which also provides 30 minutes of memory stand-by power.

The NORD-12 standard processor executes at a speed of 490 ns for each micro-instruction. This manual gives complete timing figures for all instructions.

The input/output and interrupt systems of NORD-12 are designed for ease of use and very high speed. NORD-12 has 16 program levels each with its own set of registers, making possible a complete context switching from one program level to another in only 2.0 microseconds. In addition 2048 vectored priority input/output interrupts are standard.
[1]

### Peripheral Equipment

A complete range of peripheral equipment is available for the NORD-12. The I/O system is common for both the NORD-12 and the NORD-10, and all interfaces for the NORD-10 are immediately available also for the NORD-12. When upgrading from a NORD-12 to a NORD-10 all peripherals and interfaces may be moved from the NORD-12 to the NORD-10.

Most peripherals to the NORD-12 are offered with a range of different performances. The range of peripherals include several types of console typewriters, teletypes or display terminals, paper tape equipment, line printers, card equipment, high speed electrostatic printer/plotters, magnetic cassette tape, 9-track magnetic tape also including high performance 90 ips 1600 bpi tape, fixed head drums, moving head cartridge disc, A/D - D/A equipment, transmission line interfaces and a CAMAC interface.
[1]

### Software

Based on 7 years of experience with NORD-1 and NORD-10 a wide range of system software is available including a SINTRAN III/12 operating system.

For further information, please contact A/S Norsk Data-Elektronikk's Sales Office
[1]

### Differences between NORD-10 and NORD-12

This appendix is provided for those comparing NORD-10 with NORD-12.[2]

- Maximum memory size of the NORD-12 is 64K words. The Memory Management System option is not available for the NORD-12. With this option the maximum memory size for NORD-10 is 256K words.

- The speed of the NORD-10 is 300 ns per micro-instruction for NORD-12 it is 500 ns per micro-instruction. This gives a 5/3 ratio for floating point, for all other instructions consult the NORD-10 and NORD-12 Reference Manuals.

- Internal hardware error interrupts are not connected to level 14 on the NORD-12, and there is no IIE (Internal Interrupt Enable) register and no IIC (Internal Interrupt Code). However, on the NORD-12 it is possible to hardwire interrupts to level 14.

- NORD-12 is only available with dynamic MOS memories (4096 bit/chip), while NORD-10 is offered with a range of different memories.

- The rounding algorithm for floating point differs between NORD-10 and NORD-12. On NORD-12 there is no TG (Rounding) flip-flop in the Status Register (bit 1 in Status on NORD-1), and for NORD-12 all floating point results are truncated. On the NORD-10 the least significant bit in the result is forced to one if the result could not be exactly represented. For both the NORD-10 and NORD-12 all integers up to 232-1 will be exactly represented in floating point format, and all results to this limit will also be exact.

- Only NORD-10 may have a NORD-1 Input/Output Channel as option.

- The Memory Parity systems differs between NORD-10 and NORD-12. On NORD-10 TRA instructions are used for reading memory parity error information, on NORD-12 the parity mechanism is an Input/Output device which plugs into an I/O Slot. For both NORD-10 and NORD-12 Memory Parity is an option.

- On NORD-10 the Bus Transceiver or Bus Extender is standard, this is an option on NORD-12.

### Remaining machines

This is sadly a short list. The only known survivor is the one sitting in storage in the Telemuseums storage in Fetsund. Also machines that is known to be lost will be listed to make it easier to locate remaining ones. We don't have any production numbers yet.

- NORD-12 serial 85, unknown condition but looks nice from the outside. In the collection of Telemuseum.[3]

### Sources

- ↑ 1.0 1.1 1.2 NORD-12 Reference Manual, Introduction

- ↑ NORD-12 Reference Manual, Appendix C

- ↑ NORD-12 in the collection of Telemuseum, marked 8004

sintran.com: NORD-12 Operator panel

