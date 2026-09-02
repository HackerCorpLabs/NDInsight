# Source: ndwiki article "ND-500"

- **Live page**: <https://www.ndwiki.org/wiki/ND-500>
- **Copy used here**: Wayback Machine snapshot of 27 August 2025
  http://web.archive.org/web/20250827232944/http://www.ndwiki.org/wiki/ND-500
- **Fetched**: 2026-08-27, by Ronny's request.

**Status: SECONDARY, and self-labelled a stub** - but short, specific, and it
cites documents we hold. It names **ND-05.009 NORD-500 REFERENCE MANUAL
(ND-05.009.4)**, which is
`Reference-Manuals/ND-05.009.4 EN ND-500 Reference Manual.md` here, and for the
ND-505 it cites a sintran.com PDF, **ND-505CX-A1-EN**, which we also hold as
`Installation/Product-Info/ND-505CX-A1-EN.md`.

## The most important line on the page

> "The ND-500 ... **relied on a ND-100 to do housekeeping tasks and run the
> operating system, SINTRAN III**."

That is the same host-and-slave arrangement as the NORD-1 with the NORD-5 and the
NORD-10 with the NORD-50. Four generations, one pattern - a 16-bit machine running
the operating system, with a fast wide compute engine hung off it. It is arguably
the single most important structural fact about ND hardware, and it should shape
how the machines are grouped when this history is finally written.

## What it adds

- **Model numbers are not implementations.** The ND-500 architecture had four
  distinct hardware implementations, each sold under several model numbers:
  - **ND-500/1** sold as ND-520, ND-540, ND-560
  - **ND-500/2** sold as ND-510, ND-530, ND-550, ND-560, ND-570, plus /CX variants
    as newer ND-1x0 front ends became available
  - **ND-5000** as the latest implementation
  (Note ND-560 appears in both lists - one of them is likely wrong.)
- **Multiprocessor naming**: ND-580/n and ND-590n, where n is the CPU count, 2, 3
  or 4. Up to four ND-500 CPUs in a shared-memory configuration.
- **The ND-505 and the CoCom embargo.** The page calls it a **31-bit** version of
  the ND-500 with **pin 27 snipped on the backplane**, removing its status as a
  superminicomputer so it could legally pass the CoCom embargo on exports to the
  Eastern bloc. It uses a 500/2 CPU, and says 29 bits of addressing are available
  to the user, adding "exactly how this relates to the physical 31 bits is not
  entirely clear".
- **Price**: the smallest ND-500 system in 1981 cost 400,000 German marks,
  according to a 1981 Computerwoche article the page notes has since vanished from
  the internet.

## The 28 / 29 / 31 bit question, now settled for the user-visible figure

Our three secondary sources disagreed: Norwegian Wikipedia said the ND-505 was a
"28-bits maskin", English Wikipedia said 29 bit addresses, this page says a 31-bit
machine with 29 bits for the user.

**The document we hold decides it.** `Installation/Product-Info/ND-505CX-A1-EN.md`
says plainly: *"The ND-505/CX system is based upon the ND-500/2 Central Processor.
It has 29 bits addressing space available to the user."* It then gives each user
up to 1/4 gigabyte for programs and 1/4 gigabyte for data.

So **29 bits of user addressing is right**, and English Wikipedia had it correct.
The "28" is very likely a misreading of the 1/4 GB per space figure - a quarter of
a gigabyte is 2 to the 28th bytes, and program plus data gives the 29th bit. The
**31-bit** claim and the snipped pin 27 are about the physical machine and remain
**unverified**; nothing we hold mentions either.

---

## Verbatim extract

The ND-500 is a 32-bit superminicomputer delivered in 1981 by Norsk Data. It relied on a ND-100 to do housekeeping tasks and run the operating system, SINTRAN III.

A configuration could feature up to four ND-500 CPUs, in a shared-memory configuration. 

### Contents

- 1 Hardware implementations

- 1.1 ND-500/1

- 1.2 ND-500/2

- 1.2.1 ND-505

- 1.3 ND-5000

- 2 Cost

- 3 Sources

### Hardware implementations

The ND-500 architecture lived through four distinct implementations. Each implementation was sold under a variety of different model numbers.

ND also sold multiprocessor configurations, naming them ND-580/n and an ND-590n, where n represented the number of CPUs in a given configuration, 2, 3, or 4.

### ND-500/1

Sold as the ND-520, ND-540, and ND-560.

### ND-500/2

Sold as the ND-510, ND-530, ND-550, ND-560, ND-570 (and various /CX variants as newer ND-1x0 variants became available)

### ND-505

A 31-bit version of the ND-500 machine, but . Pin 27 was snipped on the backplane, removing its status as a superminicomputer, allowing it to legally pass through the CoCom embargo. Cocom (Coordinating Committee for Multilateral Export Controls) was an embargo on Western exports to East Bloc countries during the Cold War [1], [2]
The ND-505(/CX) uses a 500/2 CPU. 29 bits of addressing space is available for the user [3] (exactly how this relates to the physical 31 bits is not entirely clear).

### ND-5000

The ND-5000 series was the latest physical implementation of the ND-500 architecture.

### Cost

The price of the smallest ND-500 system in 1981 was 400.000 German mark, according to a 1981 article in Computerwoche (the source has since disappeared from the internet).

### Sources

- ^ https://en.wikipedia.org/wiki/Coordinating_Committee_for_Multilateral_Export_Controls

- ^ https://en.wikipedia.org/wiki/History_of_computer_hardware_in_Eastern_Bloc_countries

- ^ http://sintran.com/norsk-data/library/libpdpi/ND-505CX-A1-EN.pdf

- Norsk Data Document ND-05.009 NORD-500 REFERENCE MANUAL (ND-05.009.4)

ROW| 

 | 
 ?This article is a stub. You can improve NDWiki by expanding it. |
