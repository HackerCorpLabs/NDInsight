# Source: ndwiki article "NORD-10/S"

- **Live page**: <https://www.ndwiki.org/wiki/NORD-10/S>
- **Copy used here**: Wayback Machine snapshot, fetched 2026-08-27 by Ronny's
  request. The live ndwiki is behind an Anubis proof-of-work gate.

**Status: SECONDARY, short, but every claim is footnoted to one source** - which
the page identifies only as reference [1], repeated throughout.

Introduced **1975**, a follow-up to the NORD-10 adding cache, paging and other
improvements.

- **Memory** from 1K to 256K 16-bit words, read-only or read/write.
- Maximum **virtual** address space 128 Kbytes; maximum **physical** address space
  512 Kbytes. (Note the units - Kbytes, not Kwords. 512 Kbytes is 256 Kwords,
  which matches the NORD-10 page's 256K words.)
- **16 priority interrupt levels, each with 8 registers** - unchanged from the
  NORD-10.
- **Bootstrap loaders** for both mass storage and character-oriented devices, with
  three standard load formats: octal, binary and mass storage.

We hold the primary manual for this machine -
`Reference-Manuals/10/ND-06.008.01 NORD-10-S Reference Manual.md` - so this page
should be treated as an index to it, not as a source in its own right.

---

## Verbatim extract

The NORD-10/S was a 16-bit medium scale general purpose computer system[1] made by Norsk Data, introduced in 1975. It was a follow up to NORD-10 and introduced cache, paging, and other miscellaneous improvements. 

### Contents

- 1 Memory

- 2 Interrupt System

- 3 Bootstrap loaders

- 4 Custom Instructions

- 5 Performance

- 6 References and sources

### Memory

Memory size may vary from 1K to 256K 16-bit words. Both read-only and read/write memories may be used.

Maximum virtual memory address space is 128 Kbytes.

Maximum physical memory address space is 512 Kbytes.[1]

### Interrupt System

16 priority interrupt levels each with 8 registers.[1]

### Bootstrap loaders

The NORD-10/S has bootstrap loaders for both mass storage and character oriented devices[1]. Three different load formats are standard:

- octal format load

- binary format load

- mass storage load

### Custom Instructions

The NORD-10/S provides up to 1024 customer-specified instructions. These instructions are micro-programmed in a programmable read-only memory, which is added onto the standard read-only memory.[1]

### Performance

Whetstone results [2]

ROW| 
MWIPS | 
MWIPS double precision | 
Language | 
Date
 | 

ROW| 
0.153 | 
0.0019 | 
Fortran | 
1974
 | 

### References and sources

- ^ 1.0 1.1 1.2 1.3 1.4 30.001.01A NORD-10/NORD-50, Operator's Guide

- ^ Whetstone Benchmark History and Results
