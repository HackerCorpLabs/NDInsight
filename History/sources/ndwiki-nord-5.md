# Source: ndwiki article "NORD-5"

- **Live page**: https://www.ndwiki.org/wiki/NORD-5
- **Copy used here**: Wayback Machine snapshot of 2 October 2022
  http://web.archive.org/web/20221002162518/https://www.ndwiki.org/wiki/NORD-5
- **Why the archive**: the live ndwiki is behind an Anubis proof-of-work gate.
- **Fetched**: 2026-08-27, by Ronny's request.

**Status: SECONDARY SOURCE, and the page labels itself a stub.** Its own citations
are worth chasing, and two of them are reachable from this repo:

- *ND-NYTT No 5, September 1972*, article "NORDIC - The Multicomputer Installation
  at the Norwegian Meteorological Institute" - the source of the detailed
  performance figures. The same issue of ND-NYTT is cited by the serial-47 page.
- *Norsk Data library, Software Nord-10 Design Goals (TSS-02)* - we hold
  `Reference-Manuals/10/NORD-10-Design-Goals.md`.
- *Eventyret Norsk Data - En bit av fremtiden*, Per Oeyvind Heradstveit,
  J.M. Stenersens Forlag A/S, 1985, ISBN 82-7201-040-2, page 75. A book, not held.

The key architectural claim - that the NORD-5 is a compute module attached to a
NORD-1 host, with the operating system living in the NORD-1 - matches what Ronny
stated independently.

---

NORD-5 was one of the world's earliest 32-bit minicomputers- beating the VAX by 6 years. The development started in 1970 and a machine was delivered to the Norwegian Meteorological Institute in March 1972.[1]

At least one system is preserved in the storage of the Norwegian Telecommunication Museum.

NORD-5 is a special-pupose high speed compute module designed to be attached to a general purpose NORD computer system, or to computers from other manufacturers, in order to handle heavy compute-bound tasks. The idea behind the NORD-5 is that tasks which require large amount of computations are sent out from the main system for processing while the main computer continues with other tasks. The operating system is contained in the NORD-1 computer.[2]

NORD-5 compute module

The NORD-5 was first delivered in 1972 and is a general purpose 32 bit high speed compute module designed to be attached to a general NORD computer system or to computers from other manufacturers. The NORD-5 can perform a 64 bit floating point multiplication, for example, in 900 nanoseconds and is thus very suitable for such compute bound tasks as are found in meteorology and nuclear research.[3]

NORD-5 has its own core and executes one program at a time. Assembly and compilation are done by the main system which produces object code for NORD-5. An installation with many heavy compute-bound tasks can be equipped with several NORD-5's that may share a common core pool, where all processors can address all core. A standard NORD-5 is equipped with a shift matrix and floating point unit with a speed of 950 ns for floating point add and subtract, and also 950 ns for all kinds of shifts and bit manipulations (regardless of shift count). Floating point multiply and divide require 4 microseconds for 64 bit numbers. Optionally, a high speed multiply unit may be installed, giving a speed of 950 ns for floating point multiply (64 bits). Other special-purpose arithmetic units may be included in the NORD-5. The arithmetic units in the NORD-5 are asynchronously connected to the NORD-5 CPU, allowing a range of performances. To achieve the high computational speed, the floating and shift module and the high speed multiply module are built as a logical array and in the multiply and divide modules Schottky TTL circuits are used.[2]

### Sources

- ↑ [Eventyret Norsk Data - En bit av fremtiden. p.75. (Per Øyvind Heradstveit) J.M. Stenersens Forlag A/S, 1985 (ISBN 82-7201-040-2)]

- ↑ 2.0 2.1 ND-NYTT No 5, September 1972, article "NORDIC - The Multicomputer Installation at the Norwegian Meteorological Institute".

- ↑ Norsk Data library, Software Nord-10 Design Goals (TSS-02)

- http://toresbe.at.ifi.uio.no/nd/history.html

- http://www.nodaf.no/index.php/The_History_of_Norsk_Data

- http://folk.uio.no/toresbe/nd/telemuseet/

- [Eventyret Norsk Data - En bit av fremtiden. p.75. (Per Øyvind Heradstveit) J.M. Stenersens Forlag A/S, 1985 (ISBN 82-7201-040-2)]

ROW| 

 | 
  This article is a stub. You can improve NDWiki by expanding it. |

