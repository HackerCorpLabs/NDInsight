# Machine documents to OCR

Compiled 2026-08-27 from the local mirror at `E:\Dev\Ronny\mirror-sintran-com`
(5,323 files, **4,014 PDFs**, with a parsed index of 5,376 documents in
`docs/library/library.md`), checked against what is already in NDInsight.

**Scope: the machines.** Ronny's steer - "i am mostly interrested in the history
of the machines, not the business of people". Company material is listed at the
bottom, demoted, not deleted.

Everything here is **already a PDF on this disk**. Nothing needs finding. It needs
running through the OCR pipeline (see the `ocr-pipeline` skill) and importing.

Related audits: `mirror-sintran-com/to-ocr-sintran.md` (2026-07-18) and
`F:\NDDOC\to-ocr.md`.

Note: the mirror's own index credits **"(rh) = Ronny Hansen has provided some
docs"** among its contributors.

---

## Priority 1 - the NORD-50, because it settles the architecture

The NORD-50 is the clearest case of the pattern that runs through every ND
generation: a compute engine that is a **total slave** to a 16-bit host, with no
I/O and no interrupt system of its own. Three secondary sources wrongly call it a
standalone 1975 supermini. These manuals end that, and document the host-and-slave
interface itself.

| ND number | Title | Date | Pages |
|-----------|-------|------|-------|
| 05.003.01 | NORD-50 Reference Manual | 1976-02 | 48 |
| 05.004.01 | NORD-50 CPU, Hardware Manual I | 1976-03 | 73 |
| 05.004.01B | NORD-50 CPU, Hardware Manual II | 1976-03 | 109 |
| 05.007.01 | NORD-50 Functional Description | 1977-11 | 175 |
| 05.008.01 | NORD-50 General Description and Module Description | 1979-01 | 53 |
| 30.010.02 | NORD-50 Hardware Maintenance Manual | - | - |
| 60.075.01 / .01A | NORD-50 Assembler | - | - |

In `mirror/library/libhw/` and
`mirror/external/www.home.neab.net/ND-library/05-NORD-5 ND-500 ND-5000 CPU/`
(the latter already Gandalf-OCR'd, so may only need importing).
**None of these ND numbers appear anywhere in NDInsight today.**

---

## Priority 2 - machines we have nothing on

### The Butterfly - currently one uncited Wikipedia paragraph

| ND number | Title | Date | Pages |
|-----------|-------|------|-------|
| 06.025.3 | **Butterfly-110, Technical Reference Manual** | 1987-04 | **548** |
| 06.028.1 | **Butterfly, PC Technical Reference Manual** | 1987-05 | 374 |
| B2C18 / B2C18-1987 | Book 2: ND-BUTTERFLY, Assembly and Cable Information | 1988-03 | 48 |

Nine hundred pages on a machine we currently describe from a single unsourced
paragraph. The Butterfly is the oddest thing ND built - an Ericsson PC/AT with an
ND-110PCX on two ISA cards, MS-DOS booting SINTRAN III/VSX on the expansion.

### The NORD-12 and NORD-2B

| Document | Where |
|----------|-------|
| **NORD-12 Reference Manual** (OCR'd) | `mirror/external/heim.bitraf.no/tingo/files/nd/NORD-12_Reference_Manual_ocr.pdf` |
| NORD-2B Hardware Manual Vol 1, Logic Diagrams, Dec 1970 | linked from ndwiki, Tingo collection - **not confirmed in the mirror** |
| NORD-2B I/O System, March 1971 | same |

The NORD-12 is the last big gap in an otherwise well-covered range. The NORD-2B is
the simplified NORD-1 that went to CERN in quantity, and we have no manual at all.

---

## Priority 3 - the CPU microcode and the later CPUs

This is where the ND-100 line's actual engineering lives, and it connects directly
to the emulator work already in this repo.

| ND number | Title | Date | Pages |
|-----------|-------|------|-------|
| 06.018.01 | NORD-100 Microprogramming Description | 1980-01 | 86 |
| 06.018.01B | **NORD-100 Microprogramming Description, with listing** | 1980-01 | 311 |
| 06.031.1 | **ND-110 and ND-120 Microprogrammer's Guide** | 1987-11 | 94 |
| 350002N | **ND120 CPU, MM & M** | 1991-01 | 50 |

`ND-350002-N1-EN.pdf` corrects something in our own gap analysis: we recorded "no
dedicated ND-120 manual found here". There is one.

---

## Priority 4 - the NORD-10/S, the machine we are best placed to write next

We hold the NORD-10/S Reference Manual and microprogram. These three are missing:

| ND number | Title | Date | Pages |
|-----------|-------|------|-------|
| 06.009.01A | NORD-10/S Functional Description | 1978-10 | 300 |
| 06.012.01 | NORD-10/S Input/Output System | 1978-05 | 162 |
| 06.013.01 | NORD-10/S General Description and Module Description | 1978-10 | 50 |

---

## Priority 5 - cabinets, and a codename nobody had

The "Book 2" assembly and cable drawings are how the physical machines were built,
and they name variants that appear in no text source:

| Document | Title | Date |
|----------|-------|------|
| B2C4 | ND-100 Satellite, Assembly Drawings | 1988-10 |
| B2C5 | ND-100 Satellite, Cable Info, Block and Wiring Diagrams | 1988-11 |
| B2C6 | ND-100 COMPACT, Assembly Drawings | 1988-11 |
| B2C7 | ND-100 COMPACT, Cable Info, Block and Wiring Diagrams | 1988-11 |
| B2C8 / B2C8-1987 | **ND-100/5000 COMPACT (COMSON)**, Assembly Drawings | 1987-12, 1988-03 |
| B2C19 | **ND-110 PCT**, Assembly Drawings | 1988-11 |
| B2C20 | **ND-5000 Satellite / Technostation**, Assembly Drawings | 1988-11 |

**COMSON** - presumably Compact + Samson - is a codename in no source we have
gathered, alongside Samson, Delilah, Rallar, KUSK, GAMP and Samsonite. **ND-110
PCT** is likewise new; it may or may not be the ND-110PCX of the Butterfly.

These also matter for `Hardware/3D-Models/`, which already has measured Compact
and Satellite models - assembly drawings would check them.

---

## Priority 6 - earlier and later editions

Lower value because we hold one edition of each, but editions differ:

| ND number | Title | Note |
|-----------|-------|------|
| 05.009.01 | ND-500 Reference Manual, 1980-10, 291pp | we hold 05.009.4 (1988, 446pp) |
| 05.009.3 | ND-500 Reference Manual, 1987-06, 446pp | ditto |
| 06.015.01 | ND-100 Functional Description, 1980-08, 351pp | we hold 06.015.02 (1985) |
| 06.014.02 | ND-100 Reference Manual, 1982-01 | we hold 06.014.2A (1983) |
| 05.017.01B | ND-5000 Hardware Maintenance, 1988-06, 294pp | we hold 05.017.01 |
| 06.019.01 | ND-10 device programming instructions | **no edition in NDInsight** |

---

## Known to exist but marked unavailable in the mirror

Worth watching for elsewhere - the index lists them with no file:

- **05.011.01 ND-500 Hardware Description**
- **06.017.01 ND-100 Backplane/Bus Description** (1979-04)
- **06.030.1 ND-110 and ND-120 Functional Description**
- 06.008.01 NORD-10/S Reference Manual (1977-04) and 06.009.01 (1977-08) - earlier
  editions of ones we have

---

## Machines still with no document found anywhere

- **NORD-4** - the external library has a folder named `01-NORD-1 NORD-4`, and
  nothing else. No document, no article, no mention in any source.
- **NORD-9** - a bare name in Norwegian Wikipedia. No ndwiki page exists.
- **ND-380 MF** - norsk-data.com has a hardware page for it, `hw-nd-380.html`,
  which is **empty**. The category exists; the content never got filled in.
- **ND-125/CX** - one paragraph on ndwiki, cited to sintran.com ECO 100-786 dated
  1994-09-09. The mirror has a `libeco` folder with only 3 files; worth checking
  whether that ECO is among them.

---

## Company material - lower priority, kept for completeness

Ronny's interest is the machines, so this is parked rather than dropped. It is
still the only primary route to launch dates and model histories, so it is not
worthless to the machine story.

- **Fifteen ND annual reports** in `mirror/library/libnews/`: NDAR 1977, 1978,
  1979, 1980, 1983, 1984, 1985, 1986 (EN), 1987, 1988, 1989, 1990 (EN), 1991,
  1992, 1992C. Missing 1967-1976, 1981, 1982.
- **103 company newspapers** in the same folder - 51 issues of FRND from 1988-08,
  9 of the English ND News 1981-1989, 3 of ND-NYTT (1981-82), and others.
  **ND-NYTT No 5, September 1972 is not among them**, and two ndwiki articles cite
  it for machine facts - the NORD-5 performance figures and the NTNU NORD-1s.
- Two Dolphin documents in `mirror/library/libdolphin/`.

---

## What the mirror is

A polite local mirror of norsk-data.com, Jonny Oddene's archive, plus four
external archives it links to: **datamuseum.dk**, **heim.bitraf.no** (the Tingo
collection), **storage.datormuseum.se**, **thomasbutikken.no** and
**www.home.neab.net**, whose `ND-library` is organised by ND subject number.

Library sections by file count: `libpdpi` 366, `libmisc` 320, **`libhw` 297**,
`libswpdpi` 286, `libsw` 226, `libother` 105, `libnews` 103, `libsales` 94,
`libsis` 66, `libquick` 63, `libsingen` 16, `libdolphin` 4, `libdevice` 4,
`libeco` 3.

**`libhw` is the one that matters here** - it holds the CPU and cabinet
documentation. `docs/library/library.md` is the parsed index of everything, with
ND numbers, titles, dates, page counts and download state.
