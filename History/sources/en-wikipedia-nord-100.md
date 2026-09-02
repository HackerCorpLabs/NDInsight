# Source: English Wikipedia, "Nord-100"

- **Page**: <https://en.wikipedia.org/wiki/Nord-100>
- **Fetched**: 2026-08-27 via the MediaWiki API, by Ronny's request.

**Status: SECONDARY**, and it shares an ancestor with the ndwiki ND-100 article -
that page says it started as a copy of this one in 2008. So on the ND-100, ND-110
and ND-120 the two are near-identical, and agreement between them proves nothing.

**Read it for one section: the ND-110/CX and the Butterfly.** That material is
absent from ndwiki and from every other source here, and reads as though written
from contemporary trade press. Its external links point at two ND manuals we hold,
ND.06.014.02 and ND.06.016.01.

## What only this page has

- **The 1986 ND-100 renewal.** The **ND-110CX** arrived in 1986 to replace the
  ND-100CX, alongside **ND-110 Compact** and **ND-110 Satellite** models, existing
  systems being upgradable. The CPU used three new integrated circuits to cut the
  device count from **365 to 228**, fitting on a single module, with more cache and
  **40 percent** less power. **ND-500 systems were also upgraded to the ND-110CX,
  using the ND-100 architecture as an I/O processor** - directly relevant to the
  ND-100/ND-500 interface work already in this repo.
- **The Butterfly-110 workstation** in full: an IBM PC/AT compatible **made by
  Ericsson** - Intel 80286, 512 KB RAM, EGA, floppy and hard disk - fitted with two
  expansion cards carrying the **ND-110PCX**. That card set had 1 MB of RAM, of
  which **128 KB was "donated" to the PC** to give DOS the desirable 640 KB. MS-DOS
  3.1 ran first and booted **SINTRAN III/VSX** on the expansion, and the machine
  could run DOS software and SINTRAN applications at once, the latter exported to
  terminal users.
- **The Butterfly Teamstation** - that workstation with terminals on its serial
  ports.
- **Butterfly models 10, 11 and 12** - low-end versions with no ND-110 at all,
  running Microsoft Windows and **Norsk Data Desk Top Manager**, a Windows version
  of NOTIS-WP that could read and write documents held on Norsk Data systems or
  locally.
- The ND-100 described as combining as many as **16 boards** from earlier
  generations into what was called **"the world's first high-performance
  single-board minicomputer"**.

## Where it disagrees with ndwiki

**Which option carried MOVEW, TSET and RDUS.** This page gives the block move,
test-and-set and read-without-cache instructions to the **CE**, says "the decimal
instruction set was later renamed CX", and makes the ND-110/CX simply "an ND-110
with decimal instructions". ndwiki splits them instead: CE gets decimal arithmetic
and stack handling, **CX** gets MOVEW, TSET, RDUS and segment change. **SETTLED 2026-08-27 against this page.** Our primary
`Reference-Manuals/ND-06.014.2A EN ND-100 Reference Manual.md` says: "The CX-option
consists of improved CE-instructions (Commercial Extended) plus the following
instructions (**CX only**): MOVEW: move block of words; TSET: test and set; RDUS:
read don't use cache; SINTRAN III: segment-change instructions." **ndwiki has it
right and this page has it wrong.** Opcodes: MOVEW 1431xx (xx 00 through 08),
TSET 140123, RDUS 140127.

**ND-125/CX** does not appear here at all; only ndwiki has it.

---

## Verbatim extract

The Nord-100 was a 16-bit minicomputer series made by Norsk Data, introduced in 1979. It shipped with the Sintran III operating system, and the architecture was based on, and backward compatible with, the Nord-10 line.
The Nord-100 was originally named the Nord-10/M (M for Micro) as a bit sliced OEM processor. The board was laid out, finished, and tested when they realized that the central processing unit (CPU) was far faster than the Nord-10/S. The result was that all the marketing material for the new NORD-10/M was discarded, the board was rechristened the Nord-100, and extensively advertised as the successor of the Nord-10 line. Later, in an effort to internationalize their line, the machine was renamed ND-100.


== Performance ==


== CPU ==
The ND-100 line used a custom processor, and like the PDP-11 line, the CPU decided the name of the computer.

Nord-100/CE, Commercial Extended, with decimal arithmetic instructions (The decimal instruction set was later renamed CX)
ND-110, incrementally improved ND-100
ND-110/CX, an ND-110 with decimal instructions
ND-120/CX, full redesign
The ND-100 line was machine-instruction compatible with the Nord-10 line, except for some extended instructions, all in supervisor mode, mostly used by the operating system. Like most processors of its time, the native bit grouping was octal, despite the 16-bit word length.
The ND-100 series had a microcoded CPU, with downloadable microcode, and was considered a complex instruction set computer (CISC) processor.


=== ND-100 ===
The ND-100 was implemented using medium-scale integration (MSI) logic and bit-slice processors, combining as many as the 16 boards employed in previous generations of Norsk Data computers into what was described as "the world's first high-performance single-board minicomputer".
The ND-100 was frequently sold together with a memory management unit card, the MMS. The combined power use of these boards was 90 watts. The boards would usually occupy slots 2 and 3, for the CPU and MMS, respectively. Slot 1 was reserved for the Tracer, a hardware debugger system.


=== ND-100/CE ===
The CE stood for Commercial Extended. The processor was upgraded by replacing the microcode PROM.
It added instruction for decimal arithmetic and conversion, stack instructions, segment-change instructions used by the OS, a block move, test-and-set, and a read-without-cache instruction.


=== ND-110 ===
The ND-110 was an incremental improvement over the ND-100.
The ND-110 combined the memory management system and CPU, formerly separate cards, on one board. The single CPU/MMS board was plugged into the memory management board slot, usually numbered 3. Power consumption was reduced from 90 watts to 60. 
The ND-110 made extensive use of Programmable Array Logic (PALs) and gate arrays, with semi-custom Very Large Scale Integration (VLSI) chips. 
The ND-110 had three gate arrays:

The Micro Instruction Controller, the MIC, also known as RMIC, for Rask MIC (Speedy MIC). It replaced three 74S482 sequencers and about 30 other ICs.
The Arithmetical and Logical Unit gate array (ALU, also known as the BUFALU). Replaced four Am2901 bit-slice processors, and some added registers like the data bus register the general purpose register, and the internal register block.
The Micro Address Controller (The MAC, also called RMAC, for Rask MAC (Speedy MAC). It implemented hardware address arithmetic, which in the ND-100 had been done in microcode.
Along with the macro-instruction cache memory also in the ND-100, the ND-110 had a unique implementation of cache memory on the micro-instruction level. The step termed mapping in the ND-100 was then avoided because the first micro-instruction word of a macro-instruction was written into the control store cache.
Unlike the ND-100 CPU, it handled synchronous interrupts as traps, similar to how it was handled by the ND-500.
The control store consisted of 4K x 4 bit 40ns static random-access memory (SRAM) chips. This meant that the control store was writable. It was loaded at power up and Master Clear from two 32Kx8 bit erasable programmable read-only memory (EPROM) units.
The CPU clock and the bus arbitration network were implemented using 15ns PALs.
The main oscillator was a 39.3216 MHz crystal oscillator.


=== ND-110/CX ===
This was the ND-110 with the CX microcode programmable read-only memory (PROM). The added instructions were the same as the /CE.
The ND-110CX was introduced in 1986 to replace the earlier ND-100CX model as part of a broader renewal of the company's ND-100 products, this also introducing the ND-110 Compact and ND-110 Satellite models, with existing systems being upgradable to the new configurations. The ND-110CX CPU was enhanced from the earlier ND-100 processor, employing three new integrated circuits to reduce the device count from 365 to 228 and to permit the provision of the CPU on a single module. Cache memory was increased and power consumption reduced by 40 percent. ND-500 systems were also upgraded to use the ND-110CX, these employing the ND-100 architecture in an input/output processing role.
A variant of the ND-110CX known as the ND-110PCX was incorporated into the Butterfly-110 workstation. This workstation was based on an IBM PC/AT-compatible model made by Ericsson, employing an Intel 80286 and featuring 512 KB of RAM, EGA display capabilities, floppy and hard drives, augmented with two expansion cards providing the ND-110PCX system. The ND-110PCX was equipped with 1 MB of RAM, of which 128 KB was "donated" to the PC to provide the more desirable 640 KB configuration for DOS applications of the era. 
The ND-110PCX functionality was included to support applications such as the NOTIS range of software, and where the workstation was deployed with terminals accessing the system via its serial ports, the complete product was known as the Butterfly Teamstation. The PC system itself ran MS-DOS 3.1 which, along with other programs, booted the SINTRAN III/VSX operating system on the ND-110PCX expansion.
 The workstation itself could run DOS software concurrently with SINTRAN applications exported to terminal users.
Low-end versions of the Butterfly workstation were also marketed - models 10, 11 and 12 - these omitting the ND-110 functionality but providing Microsoft Windows and the Norsk Data Desk Top Manager software: a Windows-based version of the NOTIS-WP software able to read and write documents stored on Norsk Data systems or on the local disk.


=== ND-120/CX ===
The ND-120 CPU was a complete reimplementation on an LSI chip (The so-called Delilah chip), and was originally intended to be sold as the ND-1000, to reflect the technology change, which paralleled the change from the ND-500 series to the ND-5000 (codenamed Samson).
The Samson/Delilah naming scheme may reflect that around the time of the development of the ND-120, it was increasingly clear that the mixed 16/32-bit architecture was a bottleneck for the ND-500(0) architecture; Internal technical documentation used at Norsk Data for the Delilah chip has a drawing of a grinning woman with hair in her clenched fist.


== References ==


== External links ==
ND-100 Reference Manual (PDF). Norsk Data. 1984. ND.06.014.02 Revision A.
NORD-100 Input/Output System (PDF). Norsk Data. 1980. ND.06.016.01.
