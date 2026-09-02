# History of the Norsk Data machines

**The machines.** What each one was, how it worked, what changed between
generations, and how they relate to each other.

Ronny's scope, 2026-08-27: *"i am mostly interrested in the history of the
machines, not the business of people"*. So this is hardware history. The company
story - founders, listings, acquisitions, the collapse, the afterlife - is
gathered in `sources/` where it turned up, and stays there. It gets used only
where it explains a machine: why a model was renamed, why a line was cut, why the
ND-505 was built narrow to clear an embargo.

**This is a research collection, not a finished document.** We are still
gathering. Files are named after machines, not numbered, because the shape and
order of the final document is not knowable until the material is in - how many
machines there turn out to be, which belong together, and which are variants
rather than generations.

Gathering has already changed what any structure would have to be: the NORD-5 is
a compute module for a NORD-1 host rather than a separate generation; the NORD-50
is an array processor, not the supermini one timeline calls it; a **NORD-2U**
turned up in a primary ND document, in none of the timelines being used; and an
image shows a machine badged **ND-5700 Compact**, another model in none of them.
Any ordering fixed before that work would already be wrong.

This folder is our own writing. Official scanned manuals stay in
[Reference-Manuals](../Reference-Manuals/README.md); operating-system detail
stays under [SINTRAN](../SINTRAN/README.md). Every claim needs a source citation,
and anything unproven is marked `ASSUMPTION:` or `UNVERIFIED:`.

## Start here

- **[machines/](machines/README.md)** - **one page per machine**, 46 of them, all
  to the same shape: header table, plain-English paragraph, specification, what was
  new. Start here if you want to look something up rather than read a story.
- **[MACHINE-TIMELINE.md](MACHINE-TIMELINE.md)** - **the main document**: every
  machine with its year, how important it was, and the six threads along which the
  hardware evolved - logic technology, control store, speed, caching, memory and
  protection, and interrupts. Plus the one architectural idea that shaped the whole
  range and then ended it.
- **[WHAT-WE-STILL-NEED.md](WHAT-WE-STILL-NEED.md)** - the state of the research:
  what is covered machine by machine, what is settled, and every conflict still
  open across the sources.
- **[OCR-WANTED.md](OCR-WANTED.md)** - machine documents sitting as PDFs in the
  local sintran.com mirror that are not yet in this repo. Nothing needs finding;
  it needs OCR and importing.

## Written so far

| File | Covers | State |
|------|--------|-------|
| [machines/](machines/README.md) | 46 spec sheets, one per machine and configuration | One page each, same template throughout. Images to be added later via Claude Design. |
| [MACHINE-TIMELINE.md](MACHINE-TIMELINE.md) | Every machine, 1962-1994 | Chronology, importance and hardware evolution. Every claim marked primary, secondary or disputed. The ND-500/5000 half is thinner than the 16-bit half. |
| [NORD-1.md](NORD-1.md) | NORD-1, and the NORD-5 compute module | Checked against **every** primary document in `Reference-Manuals/1/` - the Feb 1970 Reference Manual, ND-01.004.01 Hardware Manual vols I and II, ND-01.005.01 Connectors. Eight open questions recorded, none faked shut. |

## The machines to gather, and the order to do it in

The order below follows two things at once, which happen to agree: **the machines
run roughly chronologically**, and **the primary material in this repo gets
denser as you go down the list**, until the last two groups where it thins out
again. Each group is a batch because its machines share documents - you cannot
sensibly do a host without its slave, or a CPU without its variants.

**The list roughly doubled on 2026-08-27** when the two Wikipedia articles were
added. Between them they name models that appear in nothing else we hold:
Nord-2B, Nord-9, Nord-20, ND-100/CE, ND-Satellite, ND-505, ND-510/CX, ND-520,
ND-530, ND-550, ND-5400, ND-5900-2, ND-5900-3, ND-5904, **ND-88000**, and the
**Uniline 33** and **Uniline 88** Unix ranges. Both pages are uncited, so most of
those are currently **names with no documentation behind them** - working out
which are real machines, which are variants and which are errors is itself part
of the gathering.

Two whole subjects also arrived with the English article and have no home in the
list below yet: **the pre-company machines at FFI Kjeller** (SAM, and SAM 2 also
called FLINK), and **the Unix years** - DIAB, SCO System V, NDIX, the Motorola
68030 and 88000 ranges, and Dolphin Server Technology.

### 1. The NORD-10 family - NORD-2U, NORD-2B, NORD-9, NORD-10, NORD-10/S, NORD-12, NORD-20, NORD-42, NORD-50

Do this next. It is the immediate successor to the work already done, and
`Reference-Manuals/10/` is a rich folder: the ND-06.008.01 NORD-10/S Reference
Manual, ND-06.010.01 microprogram, the Design Goals document, the operator's
guide and panel description, drawings, and verification programs.

The NORD-50 belongs in this batch rather than on its own: ND-06.005.01 **NORD-10
- NORD-50 Communication System** and ND-60.116.01 **NORD-10 - NORD-50 Operator's
Guide** describe a host-and-slave pairing, the same arrangement the NORD-1 and
NORD-5 had.

**NORD-20 is no longer a bare name**: the ndwiki history has development starting
in 1971 and the machine introduced in 1972. It also adds a **NORD-42**, completed
1974 with MOS memory on 4Kb chips, which appears in no other source at all.

Gaps going in: we hold **no NORD-12 Reference Manual** (a wiki transcription is
the only route to it); **NORD-2U** rests on one line in the Design Goals document;
and **NORD-2B** and **NORD-9** are still bare names. Whether NORD-2, NORD-2B and
NORD-2U are one machine, two or three is unknown.

Two systems rather than machines belong with this group: **NORDIC**, developed
1971 and completed 1972 - almost certainly the multicomputer installation at the
Meteorological Institute that ND-NYTT wrote about in September 1972 - and
**NORDCOM**, with a NORDCOM-74 revision and a graphics variant.

### 2. The ND-100 line - ND-100, ND-100/CX, ND-100/CE, ND-100 Compact, ND-110, ND-120/CX, ND-Satellite

The best-documented machine in the repo, and the one the rest of the project
already knows most about - `SINTRAN/`, the emulator work and several skills all
describe ND-100 behaviour, so claims can be cross-checked against working
knowledge rather than only against paper. The ND-120CX "Delilah" arrives 1985 per
the timeline; ND-100 Compacts were still being delivered in 2001. `/CE` and the
`/CE` comes from Norwegian Wikipedia and needs confirming. **ND-Satellite is
confirmed**: the ND-COSMOS article describes it as a small computer ND had
recently released, and says the COSMOS name was an internal pun on it -
and `Hardware/3D-Models/` already holds a measured ND100-Satellite model.

### 3. The ND-500 line - ND-500, ND-505, ND-510/CX, ND-520, ND-530, ND-540, ND-550/560, ND-570/CX, ND-570/CXA, ND-580/CX

`Reference-Manuals/500/`, plus `Reference-Manuals/ND-05.009.4 EN ND-500 Reference
Manual.md` and the existing `SINTRAN/ND500/` analysis. This is where the
"architecture that is not von Neumann based" claim and the 1983 world's-fastest
claim need testing, and where the ND-100/ND-500 interface work already in the
repo becomes directly useful.

The ndwiki history says the ND-500 was **split into ND-520, ND-560 and ND-570 in
1982** - and marks that entry with its own "(?)". Note **ND-560** against
Norwegian Wikipedia's **ND-550**; they may be the same machine misremembered. It
also adds the **ND-570/CXA** in 1983 and, in 1986, the **ND-580/CX models 20, 30
and 40** - "systems based on two to four ND-570 CPU:s and one ND-110/CX CPU",
a multiprocessor line no other source mentions.

One entry here is worth the trip on its own: **ND-505, a cut-down ND-500 built to
clear the CoCom embargo on the Eastern bloc** - a deliberately narrowed export
model. If true that is a real story. It is uncited in both places it appears, and
the two disagree on the number that is the entire point of the machine:
Norwegian Wikipedia says **28-bit**, English Wikipedia says **29 bit addresses**.

### 4. The ND-5000 family - ND-5000/Samson, ND-5400, ND-5700, ND-5800, ND-5850/Rallar, ND-5900-2, ND-5900-3, ND-5904

`Reference-Manuals/500/` also holds the ND-5000 Hardware Description, Hardware
Maintenance, Microprogram Guide, Memory Management System and SAMSON Expected
Behaviour. Images for three of these are already in `images/nd-5000/`. The
**ND-5700** appears on a machine badge in one of those images and in both
Wikipedia articles, but in no timeline. English Wikipedia groups 5400, 5700 and
5800 under the ND-5000 name, and calls **ND-5900-2, ND-5900-3 and ND-5904** dual-,
triple- and quad-CPU machines of the same series - which, if true, means several
of these "models" are configurations rather than designs. The sources also
disagree on when the ND-5850 arrived: 1987 in both Wikipedias, **1990** in the
norsk-data.com timeline.

### 5. Server 88, the Unix years, and ND-88000

Server 88 gave the **TpServer** and **ES** platforms in 1988. Around it sits the
Unix push: the DIAB collaboration from 1987, the 1988 Santa Cruz Operation
agreement for System V on ND's Intel PCs, **NDIX** (ND's own Unix for its own
architecture), the **Uniline 33** on Motorola 68030 in 1989, **ND-88000** and the
**Uniline 88** built by Dolphin, and the Data General Aviion reselling deal.

This is where the architecture story ends - ND leaving its own CPUs. Thin
everywhere except one uncited Wikipedia article, so it goes after the groups with
manuals.

### 6. Before the company - SAM and SAM 2 / FLINK

The founders came from Forsvarets forskningsinstitutt at Kjeller, where they built
the **SAM** and the **SAM 2, also called FLINK**, said to be the basis for the
NORD-1. Not Norsk Data machines, but the machines Norsk Data came out of. FFI, not
ND, would hold the documents.

### Not a group: the company story

Parked by Ronny's scope decision. The material is gathered - fifteen annual
reports and 103 company newspapers sit in the mirror, and
[sources/norsk-data-com-nd-names.md](sources/norsk-data-com-nd-names.md) has an
eyewitness account of every name change to 2004 - but it is not what this document
is for. Pull from it only when it explains a machine.

**Two things sit outside this order** and can be picked up whenever they become
interesting: terminals and peripherals (the ND-246 terminal, the **Technostation**,
NOTIS), and the software lines (SINTRAN II and III, Nord TSS, SIBAS, MAC, NPL,
PLANC, QED, PED, LED, FOCUS, NOTIS, NORTEXT, Technovision, BIBDIA, and the
networking pair **NORDNET then ND-COSMOS**), which mostly belong under `SINTRAN/`
rather than here.

### Two books nobody here has read

- Heradstveit, Per Oeyvind (1985), *Eventyret Norsk Data*, Stenersen,
  ISBN 8272010402
- Steine, Tor Olav (1992), *Fenomenet Norsk Data*, Universitetsforlaget,
  ISBN 8200215016

## How sources are handled here

- **Primary** = an ND document (manual, annual report, drawing, ND-NYTT). Those
  live in `Reference-Manuals/` and are cited by their ND number.
- **Secondary** = someone else's retelling. Copied verbatim into
  [sources/](sources/), each with a header saying where it came from, when it was
  fetched, and what is known to be wrong in it.
- The notes say which of the two each claim rests on. A claim two secondary
  sources agree on is still secondary.
- Contradictions get recorded where they are found, both figures and both sources
  named. They do not get quietly resolved.

### Sources collected so far

| File | What it is |
|------|------------|
| [sources/norsk-data-com-timeline.md](sources/norsk-data-com-timeline.md) | The year-by-year company timeline from norsk-data.com (Sintran Data). Norwegian, verbatim. Its own sources are ND annual reports and the 1987 anniversary book. Carries a list of its known errors. |
| [sources/ndwiki-nord-1.md](sources/ndwiki-nord-1.md) | ndwiki's NORD-1 article - specs, panel, surviving machines. |
| [sources/ndwiki-nord-1-cpu-detail.md](sources/ndwiki-nord-1-cpu-detail.md) | ndwiki's card-level CPU description - which register sits on which board. |
| [sources/ndwiki-nord-1-boards.md](sources/ndwiki-nord-1-boards.md) | ndwiki's board list, board number to ND drawing number to name. |
| [sources/ndwiki-nord-1-serial-47.md](sources/ndwiki-nord-1-serial-47.md) | One surviving machine documented card by card, with a restoration log and a period price list. |
| [sources/ndwiki-nord-5.md](sources/ndwiki-nord-5.md) | ndwiki's NORD-5 article - the compute-module architecture and its timings. |
| [sources/ndwiki-nord-12.md](sources/ndwiki-nord-12.md) | ndwiki's NORD-12 article - largely transcribed from the NORD-12 Reference Manual, which we do not hold. Carries ND's 16-data-plus-2-parity = 18-bit memory word. |
| [sources/no-wikipedia-norsk-data.md](sources/no-wikipedia-norsk-data.md) | Norwegian Wikipedia's Norsk Data article. Uncited. The FFI/SAM pre-history, the full list of managing directors, and a wide machine list. Conflicts listed in its header. |
| [sources/en-wikipedia-norsk-data.md](sources/en-wikipedia-norsk-data.md) | English Wikipedia's Norsk Data article - nearly three times the Norwegian one and not a translation of it. Uncited, but the only source here covering the Unix years, Dolphin, the Telenor purchase, the UK afterlife, and Lithuania. Conflicts listed in its header. |
| [sources/en-wikipedia-nd-cosmos.md](sources/en-wikipedia-nd-cosmos.md) | English Wikipedia on ND-COSMOS. Short and uncited, but partly corroborated from inside this repo: **NORDNET** is confirmed by a primary ND manual, and the Ethernet and HDLC link layers by our own measured XMSG work. |
| [sources/ndwiki-history-of-norsk-data.md](sources/ndwiki-history-of-norsk-data.md) | ndwiki's year-by-year history, 1967-1989. **The best-sourced secondary we have** - twenty numbered references, several to real published work. Adds NORD-20, NORD-42, NORDIC, NORDCOM, ND-560, ND-570/CXA, ND-580/CX, "Samsonite", SINTRAN-IV, and named installations including SCANNET and JET. Its installation counts are ambiguous - see its header. |
| [sources/ndwiki-nd-100.md](sources/ndwiki-nd-100.md) | ndwiki on the ND-100 line. **Two of its claims verified against our primary manuals** - the CX instruction mnemonics and the 39.3216 MHz ND-110 oscillator. Adds the NORD-10/M origin, the ND-125/CX, the RMIC/BUFALU/RMAC gate arrays and the Samson/Delilah joke. |
| [sources/en-wikipedia-nord-100.md](sources/en-wikipedia-nord-100.md) | English Wikipedia on the Nord-100. Shares an ancestor with the ndwiki page, so mostly duplicate - except the ND-110CX 1986 renewal and the whole **Butterfly-110 / Teamstation** workstation story, which is here and nowhere else. |
| [sources/ndwiki-nd-500.md](sources/ndwiki-nd-500.md) | ndwiki on the ND-500. A stub, but it states the host-and-slave arrangement plainly and maps model numbers onto four implementations. Its ND-505 question is **settled** from a product sheet we hold. |
| [sources/ndwiki-nd-5000-family.md](sources/ndwiki-nd-5000-family.md) | ndwiki on the ND-5000 family. Largely redundant with `SINTRAN/ND5000/ND5000-FAMILY-MODELS-REFERENCE.md`, which is primary-based and better - but a compact statement that the models are mostly one design differentiated by CPU type, a clock jumper and enabled caches. |
| [sources/ndwiki-sintran-iii.md](sources/ndwiki-sintran-iii.md) | ndwiki on SINTRAN III. Mostly redundant with `SINTRAN/Release-Documentation/SINTRAN-III-Release-History.md`, which is better and primary-based - but adds versions A to I, the SINTRAN I/II and NORD-TSS predecessors, and what the system felt like to use. |

| [sources/ndwiki-nord-50.md](sources/ndwiki-nord-50.md) | ndwiki on the NORD-50. **Settles what it was** - a special-purpose array processor and "total slave to the NORD-10/S", agreeing with the primary ND Design Goals against three secondary sources. Also names the ND 1100/S - ND 1400/S systems. |
| [sources/ndwiki-nord-10.md](sources/ndwiki-nord-10.md) | ndwiki on the NORD-10. 24 boards, 160 registers, 1K ROM microprogram, 16-to-18-bit paging, and the four protection rings. Warns it is drawn from an ND introduction document we hold. |
| [sources/ndwiki-nord-10-s.md](sources/ndwiki-nord-10-s.md) | ndwiki on the NORD-10/S. Short; an index to the primary manual we hold. |
| [sources/ndwiki-nord-20.md](sources/ndwiki-nord-20.md) | ndwiki on the NORD-20. Near-identical to the NORD-2B, six CPU boards not ten, released *before* the NORD-10. Price and cycle-time comparison against the NORD-10. |
| [sources/ndwiki-nord-2b.md](sources/ndwiki-nord-2b.md) | ndwiki on the NORD-2B. A simplified, cheaper NORD-1; many at CERN; one processing live EEG for epilepsy research. |
| [sources/ndwiki-nord-42.md](sources/ndwiki-nord-42.md) | ndwiki on the NORD-42. **An OEM NORD-12** built for Noratom-Norcontrol, not a separate design. |
| [sources/ndwiki-sintran-ii.md](sources/ndwiki-sintran-ii.md) | ndwiki on SINTRAN II. **SINTRAN is not an ND name** - NTH and SINTEF, 1968, and the name is SINTEF plus FORTRAN. |
| [sources/ndwiki-nord-tss.md](sources/ndwiki-nord-tss.md) | ndwiki on NORD-TSS, the timesharing half of the pre-SINTRAN-III world. |
| [sources/ndwiki-nord-pl.md](sources/ndwiki-nord-pl.md) | ndwiki on NORD PL, the language SINTRAN III was written in. |
| [sources/ndwiki-nd-notis.md](sources/ndwiki-nd-notis.md) | ndwiki on ND-NOTIS. Matters here because COSMOS and the Butterfly existed partly to run it. |
| [sources/norsk-data-com-nd-names.md](sources/norsk-data-com-nd-names.md) | norsk-data.com's company-name table, 1967-2004, with an eyewitness commentary. **Company history, parked** - but it settles the founding dates and is where most of our photographs come from. |

**Note on fetching ndwiki**: as of 2026-08-27 the live site sits behind an Anubis
proof-of-work gate, and both the article URLs and `api.php` refuse any script.
The copies here were taken from the Wayback Machine, and each file names its
snapshot date.

## Images

Pictures live in [images/](images/), one sub-folder per machine. Every one is
listed in [images/CREDITS.md](images/CREDITS.md) with its author and licence.

## Corrections made to the rest of the repo

- `Reference-Manuals/1/README.md` listed "Binder NORD-1 ITT-1600" as a NORD-1
  peripheral binder. It is not - it is Norwegian study notes on the **ITT-1600**,
  a different computer, bound with TI datasheets. Fixed, and the file marked "do
  not cite for any NORD-1 fact". It is the only file in that folder mentioning
  parity, so a grep makes it look like the answer to the NORD-1's 18-bit core
  question. It is not.

## Where the manuals are

Most ND manuals sit at the top level of
[Reference-Manuals/](../Reference-Manuals/README.md). Three machine folders break
that out:

- [Reference-Manuals/1/](../Reference-Manuals/1/) - NORD-1
- [Reference-Manuals/10/](../Reference-Manuals/10/) - NORD-10, and the two
  NORD-10/NORD-50 communication manuals
- [Reference-Manuals/500/](../Reference-Manuals/500/) - ND-500 and ND-5000
- `Reference-Manuals/Assembler_for_NORD-5_April_1972.md` - the NORD-5 assembler,
  at the top level rather than in a machine folder

## Related material elsewhere in the repo

- [Hardware/ND-PHYSICAL-MODELS.md](../Hardware/ND-PHYSICAL-MODELS.md) - measured cabinets and terminals
- [SINTRAN/Release-Documentation/SINTRAN-III-Release-History.md](../SINTRAN/Release-Documentation/SINTRAN-III-Release-History.md) - the OS version timeline
