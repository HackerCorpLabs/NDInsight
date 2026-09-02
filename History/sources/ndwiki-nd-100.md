# Source: ndwiki article "ND-100"

- **Live page**: <https://www.ndwiki.org/wiki/ND-100>
- **Copy used here**: Wayback Machine snapshot of 17 December 2024
  http://web.archive.org/web/20241217220809/https://www.ndwiki.org/wiki/ND-100
- **Fetched**: 2026-08-27, by Ronny's request.

**Status: SECONDARY - but the one page whose claims we can actually test**, since
the ND-100 is the machine this repo knows best. It cites **ND-06.014.02 ND-100
REFERENCE MANUAL**, held here as
`Reference-Manuals/ND-06.014.2A EN ND-100 Reference Manual.md`, and says it began
as a copy of the English Wikipedia NORD-100 article on 4 August 2008.

## Two claims checked against primary manuals here - both hold

- **The CX instructions.** The page says CX added **MOVEW, TSET, RDUS** and
  segment-change instructions. All three mnemonics appear in our primary ND-100
  Reference Manual. Confirmed present, though which option owns them is disputed
  (see the English Wikipedia source file).
- **The 39.3216 MHz oscillator.** Our primary
  `Reference-Manuals/ND-06.026-1-EN ND-110 Functional Description.md` says: "The
  main oscillator is now a 39.3216 Mhz crystal oscillator." Confirmed exactly.

That is a better track record than any other secondary source here, so its
unconfirmed engineering detail deserves to be taken seriously.

## What it adds

- **The ND-100 began as the NORD-10/M** - M for Micro - a bit-sliced OEM
  processor. The board was laid out, built and tested before they realised it was
  far faster than the NORD-10/S; the NORD-10/M marketing was thrown away and the
  board sold as the NORD-100, successor to the NORD-10 line. Renamed **ND-100 in
  1978** to internationalise the range.
- **The CPU names the machine**, as with the PDP-11.
- **ND-125/CX** - an ND-120 CPU with faster on-board memory and **8, 12 or 16 MB**
  on board against the ND-120/CX maximum of 6 MB, using 4 MB SIMMs instead of 1 MB
  SIPs, memory cycle down to 150 ns on 70 ns modules. Cited to sintran.com,
  **ECO 100-786, dated 1994-09-09**. **ND-125 appears nowhere else in this repo.**
- **Performance table**: ND-100 and ND-100/CE need at least 3 microinstructions per
  instruction at 150 ns minimum cycle; ND-110 and ND-110/CX need 1 at 100 ns.
- **The ND-110 three gate arrays**, with their in-house Norwegian nicknames:
  **RMIC** ("Rask MIC", speedy MIC), replacing three 74S482 sequencers and about 30
  other ICs; the **BUFALU**, replacing four Am2901 bit-slice processors plus the
  data bus, general purpose and internal register block; and **RMAC** ("Rask MAC"),
  doing in hardware the address arithmetic the ND-100 did in microcode.
- **Micro-instruction level cache** on the ND-110, above the macro-instruction
  cache the ND-100 already had - the ND-100 "mapping" step is avoided because the
  first micro-instruction word of a macro-instruction goes into the control store
  cache.
- The ND-110 handled **synchronous interrupts as traps**, like the ND-500 and
  unlike the ND-100.
- **Writable control store**: 4K x 4 bit 40 ns SRAM, loaded at power-up and Master
  Clear from two 32K x 8 EPROMs. CPU clock and bus arbitration on 15 ns PALs.
- **Slot layout**: CPU in slot 2, MMS memory management in slot 3, **slot 1
  reserved for the Tracer, a hardware debugger**. CPU plus MMS drew 90 watts; the
  ND-110 put both on one board and dropped it to 60.
- **The ND-120 was going to be the ND-1000**, renamed to parallel the ND-500 to
  ND-5000 change. The codenames are a joke: **Samson** for the ND-5000, **Delilah**
  for the ND-120 LSI chip - ND internal documentation for Delilah carries a drawing
  of a grinning woman with hair in her clenched fist.
- Surviving system named: **ND-100 serial 383**, Telemuseet, accession TELE.Hf-226.

---

## Verbatim extract

The ND-100 was a 16-bit minicomputer series made by Norsk Data, introduced in 1979. It shipped with the SINTRAN III operating system, and the architecture was based on, and backwards compatible with, the NORD-10 line.

The NORD-100 was originally named the NORD-10/M (M for Micro) as a bitsliced OEM processor. The board was laid out and finished and tested when they realized that the CPU was far faster than the NORD-10/S. The result was that all the marketing material for the new NORD-10/M was discarded, the board was rechristened the NORD-100, and extensively advertised as the successor of the NORD-10 line. Later (the year was 1978), in an effort to internationalize their line, the machine was renamed ND-100.

### Contents

- 1 Performance

- 2 CPU

- 2.1 ND-100/CE

- 2.2 ND-100/CX

- 2.3 ND-110

- 2.4 ND-110/CX

- 2.5 ND-110PCX

- 2.6 ND-120/CX

- 2.7 ND-125/CX

- 3 Surviving systems

- 4 Gallery

- 5 See also

- 6 References

### Performance

Relative CPU performance

ROW| 
 | 
ND-100 | 
ND-100/CE | 
ND-110 | 
ND-110/CX | 
ND-120/CX | 
ND-125/CX
 | 

ROW| 
Minimum number of microinstructions per instruction | 
3 | 
3 | 
1 | 
1 | 
 | 

 | 

ROW| 
Minimum microinstruction cycle time | 
150ns | 
150ns | 
100ns | 
100ns | 
 | 

 | 

### CPU

The ND-100 line used a custom processor, and like the PDP-11 line, the CPU decided the name of the computer.

- NORD-100/CE, Commercial Extended, with decimal arithmetic instructions

- ND-100/CX, improved the CE instructions and added some new instructions

- ND-110, incrementally improved ND-100. Same performance and instruction set as the ND-100/CX.

- ND-110/CX, a faster version of the ND-110 (1.5-3.5 times faster).

- ND-120/CX, completely redesigned using one big VLSI gate array (The so-called Delilah chip). Performance is approximate 1.9 times faster than the ND-110/CX. Minor changes in the microcode, no changes in the macrocode/opcodes.

- ND-125/CX, a 120 CPU board with faster access to onboard memory and increased onboard memory size.

The ND-100 line was machine-instruction compatible with the Nord-10 line, except for some "extended instructions", all in supervisor mode, mostly used by the operating system. Like most processors of its time, the native bit grouping was octal, despite the 16-bit word length.

The ND-100 series had a microcoded central processing unit, with downloadable microcode, and was considered a CISC processor.

The ND-100 was implemented using medium-scale integration (MSI) logic and bit-slice processors.

The ND-100 was frequently sold together with a memory management card, the MMS. The combined power use of these boards was 90 watts. These boards would usually occupy slots 2 and 3, for the CPU and MMS, respectively. Slot 1 was reserved for the Tracer, a hardware debugger system.

### ND-100/CE

The CE stood for Commercial Extended. The processor was upgraded by replacing the microcode PROM.

It added instruction for decimal arithmetic and conversion (decimal instructions) and stack handling instructions.

### ND-100/CX

The CX option improved the instructions introduced with the CE option, and added some new instructions: MOVEW, TSET, RDUS and segment change instructions.

### ND-110

Main articles: ND-110 CPU and ND-110 Satellite Series

The ND-110 was an incremental improvement over the ND-100.

The ND-110 combined the Memory Management System and CPU, previously separate cards, on one board. The single CPU/MMS board was plugged into the memory management board slot, usually numbered 3. The power consumption was reduced from 90 watts to 60. 

The ND-110 made extensive use of PALs and gate arrays - with "semi-custom" VLSI chips. 

The ND-110 had three gate arrays:

- The Micro Instruction Controller, the MIC - also known as RMIC, for "Rask MIC" ("Speedy MIC"). It replaced three 74S482 sequencers and about 30 other ICs.

- The Arithmetical and Logical Unit gate array (ALU, also known as the "BUFALU"). Replaced four Am2901 bit-slice processors, and some additional registers like the data bus register the general purpose register, and the internal register block.

- The Micro Address Controller (The MAC, also called RMAC, for "Rask MAC" ("Speedy MAC"). It implemented hardware address arithmetic, which in the ND-100 had been done in microcode.

In addition to the macro-instruction cache memory also found in the ND-100, the ND-110 had a unique implementation of cache memory on the micro-instruction level. The step known as mapping in the ND-100 was then avoided because the first micro-instruction word of a macro-instruction was written into the control store cache.

Unlike the ND-100 CPU, it handled synchronous interrupts as traps, similar to how it was handled by the ND-500.

The control store consisted of 4K x 4 bit 40ns SRAM chips. This meant that the control store was writable. It was loaded at power up and Master Clear from two 32Kx8 bit EPROMs.

The CPU clock and the bus arbitration network were implemented using 15ns PALs.

The main oscillator was a 39.3216?MHz crystal oscillator.

### ND-110/CX

This is the fast version of the ND-110 CPU, also known as RASK.

### ND-110PCX

This is the CPU used in the BUTTERFLY 110 PC-based workstation. The CPU is implemented on two full length ISA cards and based on the same design as ND-110/CX. 

### ND-120/CX

The ND-120 CPU was a complete reimplementation on an LSI chip (The so-called Delilah chip), and was originally intended to be sold as the ND-1000, to reflect the technology change, which paralleled the change from the ND-500 series to the ND-5000 (Codenamed Samson).

The Samson/Delilah naming scheme may reflect that around the time of the development of the ND-120, it was increasingly clear that the mixed 16/32-bit architecture was a bottleneck for the ND-500(0) architecture; Internal technical documentation used at Norsk Data for the Delilah chip has a drawing of a grinning woman with hair in her clenched fist.

### ND-125/CX

The ND-125 CPU appears to be based on an ND-120 CPU, but with improved performance by speeding up memory access to the on-board memory, and by increasing the on-board memory size to 8, 12, or 16 megabytes (the ND-120/CX could have a maximum of 6MB on-board). The larger memory size is achieved by using 4-Mbyte SIMMs instead of 1-Mbyte SIPs. Memory cycle time is reduced to 150 ns by using 70 ns memory modules instead of 100 ns modules.[1]

### Surviving systems

There are quite a lot of surviving ND-100 systems remaining. This list is far from complete.

- ND-100 serial 383?: In the collections of Telemuseet[2]

### Gallery

- 

ND-100 rack with a lot of serial ports but missing CPU

- 

3002 ND-100 CPU board.

- 

ND-100 front panel

- 

ND-100 Satellite

### See also

- Wikipedia:NORD-100

### References

- ^ sintran.com, ECO 100-786, date: 1994.09.09

- ^ Collections of Telemuseet, TELE.Hf-226

- This article was originally a copy of the English Wikipedia article NORD-100 in 4th August 2008.

- Norsk Data Document ND-06.014.02 ND-100 REFERENCE MANUAL
