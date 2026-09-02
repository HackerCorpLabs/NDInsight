# What we have, and what is still missing

Written 2026-08-27, after gathering sixteen sources and checking them against the
manuals in this repo. This is the honest state of the research, not a plan.

**Updated 2026-08-27 (later the same day)**: ten more ndwiki pages fetched, the
sintran.com mirror searched, and several conflicts closed. See
[OCR-WANTED.md](OCR-WANTED.md) for what to run through OCR next.

## The one structural finding - now confirmed by Ronny and by a wiki quoting ND

Four generations, one pattern. A 16-bit machine runs the operating system, with a
faster, wider compute engine attached to it. **Ronny confirmed this directly:
"nd5 needs nd1, nd50 needs nd10 and nd500 or nd5000 needs nd100/110 or 120", and
"the 5xxx machines are co-processors for the 1xxx machines".**

| Host | Attached engine | Evidence |
|------|-----------------|----------|
| NORD-1 | NORD-5 | "The operating system is contained in the NORD-1 computer" - ndwiki NORD-5, from ND-NYTT Sept 1972 |
| NORD-10 | NORD-50 | ND-06.005.01 **NORD-10 - NORD-50 Communication System** and ND-60.116.01, both held here |
| ND-100 | ND-500 | "relied on a ND-100 to do housekeeping tasks and run the operating system" - ndwiki ND-500 |
| ND-100/110/120 | ND-5000 | "uses a ND-100/ND-110/ND-120 CPU as I/O processor" - ndwiki ND-5000 family |

And it is also how the company died. The ND-5000 page states the failure directly:
as the 5000 line got faster, **the dual architecture became bottlenecked because
all I/O had to pass through the ND-100**. English Wikipedia says the same thing
from the other end - the ND-120 was nearly called ND-1000, and the mixed 16/32-bit
architecture was increasingly seen inside ND as the bottleneck.

The ndwiki NORD-50 page puts it in ND's own terms - the NORD-50 is "a **total
slave** to the NORD-10/S", with no I/O system and no interrupt system of its own,
all I/O done through the NORD-10/S, which runs SINTRAN III/VS and the NORD-50
monitor and "submits jobs to NORD-50 for execution in batches". To the NORD-10 the
NORD-50 "looks like any else device and is controlled via standard IOX
instructions".

This is the spine of the whole document: one architectural idea, carried for
twenty years, that eventually became the trap. Five NORD-50 manuals sit in the
mirror waiting to prove it from primary sources - see [OCR-WANTED.md](OCR-WANTED.md).

## Closed on 2026-08-27

- **What the NORD-50 was.** Settled: a special-purpose array processor and total
  slave to the NORD-10/S, agreeing with the primary ND Design Goals document. The
  three secondary sources calling it a standalone 1975 supermini are repeating an
  error. Dead unless a primary document revives it.
- **NORD-42 is not a separate machine.** It is an **OEM version of the NORD-12**,
  built for Noratom-Norcontrol, used as the processor in their DATABRIDGE system
  and in maritime simulators in Norway, China and Mexico.
- **NORD-20 is barely a separate machine either.** "Almost identical with the
  NORD-2B ... electrically the same but the CPU is contained on six boards instead
  of ten." Released *before* the NORD-10; first installed January 1972; 43 systems
  by August 1974. It also carries the same description our NORD-2U entry came
  from, so NORD-2U and NORD-20 may be one machine.
- **NORD-2B has substance now**: a simplified, cheaper NORD-1 with 4-16 kW core,
  a substantial number at CERN, and one at Norway's National Centre for Epilepsy
  processing live EEG to detect epileptic seizures. Hardware Manual dated December
  1970. Serial 05 survives at Telemuseum.
- **The founding date.** `mirror/history/ND-names.html` gives both: **7 July 1967**
  as a personal company, **19 September 1967** as an AS. English Wikipedia's
  "August 8, 1967" matches neither.
- **Where SINTRAN came from.** Not ND. The original was released in **1968** by the
  Department of Engineering Cybernetics at NTH with SINTEF, and the name is a
  portmanteau of **SINTEF and FORTRAN**.
- **The image provenance.** The three ND-5000 photographs are byte-for-byte the
  size of `history/nd-5000.jpg`, `nd5800cpu.jpg` and `nd5850cpu.jpg` in the mirror.
  Source confirmed; permission still not asked.
- **A whole afterlife nobody had.** Nine more company names between 1992 and 2004,
  from an eyewitness. See
  [sources/norsk-data-com-nd-names.md](sources/norsk-data-com-nd-names.md).

## Closed on 2026-08-27, second pass

- **The host-and-slave architecture is now PRIMARY, not a wiki claim.**
  `Reference-Manuals/10/ND-06.005.01 NORD-10 - NORD-50 Communication System.md`
  (August 1975): the NORD-10 "may regard the NORD-50 as an I/O device", and "the
  NORD-10 has complete control, and the NORD-50 is regarded as a slave to the
  NORD-10". Communication is by **IOX instruction**.
- **The bottleneck is visible in 1975.** The same manual: all transfers are 16 bits
  wide, so "the NORD-10 must use **two IOX instructions** to transfer a NORD-50
  word". The NORD-50's registers are all 32-bit. The constraint that the ND-5000
  documentation complains about in 1987 is present in the first pairing.
- **MOVEW, TSET and RDUS are CX, not CE.** The primary ND-100 Reference Manual:
  "The CX-option consists of improved CE-instructions (Commercial Extended) plus
  the following instructions (**CX only**): MOVEW ... TSET ... RDUS ... SINTRAN III:
  segment-change instructions." **ndwiki was right and English Wikipedia was
  wrong.** Opcodes: MOVEW 1431xx (xx 00-08), TSET 140123, RDUS 140127.
- **ND-550 and ND-560 are both real and distinct**, from ND's own ADP/WMIPS ladder
  (510/CX 1.0, 530/CX 1.6, 550/CX 2.1, 560/CX 3.8, 570/CX 5.9).

## Closed on 2026-08-27, third pass

- **The ND-5000 is not a new architecture.** `ND-05.009.4 EN ND-500 Reference
  Manual`: "This manual is valid for both the ND-500 and the ND-5000 computer
  systems." One architecture, 1981-1990, in four implementations - ND-500/1,
  ND-500/2, Samson, Rallar. Even the '87 extensions run on the older CPUs.
- **The ND-500 and ND-5000 host relationship is now primary too.** The same manual
  defines the term: "Whenever the I/O processor is mentioned, this means the
  ND-100 or the ND-110 processor." Three of the four host/slave pairings are now
  proved from ND documents; only NORD-1 with NORD-5 is still secondary.
- **What kind of machine the ND-500 was**: dedicated local-variable-base and
  record-base registers, top-of-stack with low and high limit traps, string
  instruction and subroutine-entry chapters, and own/child/mother trap enables. A
  design built around compiled high-level languages.

## Closed on 2026-08-27, fourth pass

All from `Reference-Manuals/10/ND-06.008.01 NORD-10-S Reference Manual.md`, which
had not been opened before this pass.

- **The 18-bit memory word is an ordinary ND convention, from a primary manual**:
  "Memory modules with 18 bits word length provide one parity bit per byte, while
  21 bit modules are used for memory error correction." 16 data + 2 byte-parity =
  18. This was previously reachable only through a wiki quoting the NORD-12 manual.
  It still does not prove the NORD-1 case, but it makes the explanation ordinary.
- **The NORD-10 register count explained**: "8 general registers for each program
  level and two scratch registers for each level to be used by the
  micro-processor" - 16 x 10 = 160 total, 16 x 8 = 128 program-visible. The wiki
  numbers were right; now we know the arithmetic.
- **Context switching improved between models**: 1.5 us on the NORD-10 (Design
  Goals) and **1 us** on the NORD-10/S (Reference Manual). Two ND documents, two
  machines - not a contradiction.
- **The ring system, in full**, with ND's own recommended layout: ring 0 user
  programs, ring 1 compilers and assembler, ring 2 operating systems, ring 3
  kernel. Both protection systems must pass, not either.
- **Paging detail**: four page index tables of 64 words each, in high-speed 16-bit
  registers - which is where the wiki's "256 word block" comes from, 4 x 64.

## Closed on 2026-08-27, fifth pass

From `Reference-Manuals/10/ND-06.010.01-NORD-10-S-MICROPROGRAM.md`, previously
unread.

- **The NORD-10 control store, exactly**: "1k x 32 bits Read Only Memory". Four
  microinstruction types - ARITHMETIC, INTERBLOCK, JUMP, LOOP - chosen by ROM bits
  31 and 30. Hardware-generated entry points, a readable microprogram counter
  giving simple subroutines, entry points two apart.
- **What else lived in the ROM**: the instruction repertoire, the operator panel
  driver, MOPC (operator communication in stop mode), the bootstrap loader and a
  memory check. Confirms the secondary claim and names the parts.
- **ND's own reason for microprogramming**, contrasting with "a large and
  complicated Time Counter/Cycle Counter" in a non-microprogrammed machine - which
  is precisely the NORD-1, whose card list has card 123 Cycle counter and card 151
  Time counter.
- **The 32-bit vs 48-bit floating point option** was a microcode choice on the same
  hardware - the flexibility was a product, not just a design argument.
- **NORD-42 is primary-confirmed as NORD-10 family**: the ROM covers "NORD's 10/S,
  10, 42 instruction repertoire".

## Closed on 2026-08-28, sixth pass

From `Reference-Manuals/ND-06.015.02 ND-100 Functional Description.md`, previously
unread. The ND-100 rows were the last major ones still resting on wikis.

- **"ND-100 is a 16-bit general purpose single board computer"** - the single-board
  claim is now primary, not a Wikipedia phrase.
- **Bit-slice, quantified**: "a 4-bit subsection of the 16-bit wide ALU and register
  section" - so four slices, which is what the ND-110's BUFALU later replaced.
- **The control store widened**: 2K x **64 bits** on the ND-100 against the
  NORD-10's 1K x **32 bits**.
- **The CX-option is a bigger PROM**: "By expanding the microprogram PROM to 4 K by
  64 bits, a number of instructions are introduced. These instructions comprise
  what is known as the CX-option." CE and CX were microcode, not silicon.
- **A writable control store existed on the ND-100 as an option** - 256 words by 64
  bits - years before the ND-110 made one standard.
- **Address space**: 64 KW without the memory management system, **16 MW (32 MB)
  with it**.
- **Address arithmetic was microcode on the ND-100** and hardware on the ND-110
  (RMAC). Both ends of that trade are now documented.
- **Context switching is register-file selection**, done by microprogram - the
  ND-100 manual states the architecture the NORD-10 introduced.

## Closed on 2026-08-28, seventh pass

From `Reference-Manuals/ND-06.026-1-EN ND-110 Functional Description.md`. This was
the last major machine whose evolution claims rested on a wiki.

- **All three gate arrays confirmed and defined by ND**: RMAC the address-arithmetic
  array, BUFALU the 16-bit ALU array which "also contains the current register set",
  RMIC the microinstruction sequencer. The secondary sources' claim that BUFALU
  absorbed the register blocks is corroborated by ND's own wording.
- **What is on the one card**: CPU, MMS, cache, control store, interrupt and trap
  handlers, timing and real-time clock, operator panel interface, **terminal 1
  serial interface**, register file, cycle controller and bus interface.
- **The control store is 8K deep by 64 bits**, in four 16-bit groups - so the
  progression is 1K x 32 (NORD-10), 2K x 64 (ND-100, 4K for CX), 8K x 64 (ND-110).
- **Independent confirmation of last pass's finding**: "the microprogram control
  store is writeable **(optional on ND-100)**" - stated in the ND-110 manual,
  matching what the ND-100 manual said from the other side.
- **A new instruction worth noting**: TRR CILP, opcode 150113, inhibits individual
  pages in cache. Also VERSN, opcode 140133, reads print and microprogram version
  numbers.

## Closed on 2026-08-28, ninth pass

From `SINTRAN/ND500/ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md`, the repo's own
carve work from NPL source and ND manuals. This closes the gap named in the eighth
pass - the ND-100-to-ND-500 mechanism.

- **The host link changed exactly twice in fourteen years**: IOX instructions
  (NORD-10 to NORD-50, 1975), DMA over **PCB 3022** with a bank of **16 IOX
  registers** (ND-100 to ND-500, 1981), then **octobus message passing** (ND-5000,
  1987).
- **The ND-500 interface is the NORD-50 idea unchanged in kind** - LMAR5/RMAR5 for
  the memory address register, LSTA5/RSTA5 status, LCON5/RCON5 control, master
  clear, terminate, TAG in and out, limits, lock and unlock. A 16-bit host driving
  a 32-bit machine one 16-bit register at a time, six years later.
- **The dependency is literal in the hardware**: the ND-500's status register
  carries a five-bit STOPREASON, and reason 1 is **MOCALL, a monitor call**. The
  32-bit processor stops and asks the 16-bit machine to do the work.

## Closed on 2026-08-28, tenth pass

From `Reference-Manuals/500/ND-05.020.01 EN ND-5000 Hardware Description.md`.

- **Each ND-5000 model ships with a named I/O processor**: ND-5200 with an ND-110,
  ND-5400 and ND-5500 with an ND-110/CX, ND-5700 and ND-5800 with an ND-120/CX. The
  16-bit line's generations are stepped inside the 32-bit range.
- **CPU type to model mapping confirmed from primary**: type 1 = model 2 (ND-5200),
  type 2 = models 4, 5, 7 (ND-5400/5500/5700), type 3 = model 8 (ND-5800, ND-5900).
- **Type 1 is type 2 with the cache and AAP baby modules removed**, and on the
  ND-5200 "floating-point operations are performed by the microprogram" - confirming
  what had only been a wiki claim.
- **Type 3 is not type 2 plus a layer**: unique mother board and cache/IDA baby
  module, plus an IDAC "booster" module used only in the ND-5800 and ND-5900.
- **Clock speeds confirmed**, including the counter-intuitive part: the ND-5400 and
  ND-5500 run slow (156 ns) while the cheaper ND-5200 runs at 70 ns.
- Caveat: our OCR of the model/cache table has merged columns, so which caches each
  model enables is not readable line by line from this copy.

## Opened on 2026-08-27

- **ND 1100/S, ND 1200/S, ND 1300/S, ND 1400/S** - system-level product names for
  NORD-10/S plus NORD-50 combinations, per the ndwiki NORD-50 page. In no other
  source.
- **NORD-4** - the external ND-library in the mirror has a folder named
  `01-NORD-1 NORD-4`. No source we have gathered mentions a NORD-4.

## A correction from Ronny, 2026-08-28

**The host-and-slave dependency runs one way only.** The NORD-10 and the
ND-100/110/120 were also delivered **without** the coprocessor - they were
complete, standalone products, and the volume business. The NORD-5, NORD-50,
ND-500 and ND-5000 are the ones that cannot run alone.

The document had been stating the pattern in a way that could be read backwards,
as if the 16-bit machines existed to host the 32-bit ones. Fixed in
[MACHINE-TIMELINE.md](MACHINE-TIMELINE.md) section 3, with the installed-base
ratio as evidence: 62 NORD-10 against 3 NORD-50 in 1975, 83 against 3 in 1976,
114 against 7 in 1977.

This sharpens rather than weakens the argument: the expensive half was optional
and the cheap half stood alone, which is why the architecture survived twenty
years - and why the eventual bottleneck hit the high-margin product while the
volume machines carried on unaffected.

## Coverage, machine by machine

Legend: **P** = primary ND document held in this repo. **S** = secondary source
gathered. **W** = written up in `History/`.

| Machine | P | S | W | Note |
|---------|---|---|---|------|
| SAM, SAM 2 / FLINK | - | thin | - | Pre-company, built at FFI Kjeller. One sentence in each Wikipedia. FFI would hold the documents, not ND. |
| NORD-1 | **yes, 4 docs** | yes, 4 | **yes** | Done and verified. Eight open questions recorded. |
| NORD-2B | - | **yes** | - | Simplified, cheaper NORD-1, 4-16 kW core. Hardware Manual Dec 1970. Two manuals exist in the Tingo collection, neither held. |
| NORD-2U / NORD-20 | - | **yes** | - | Very likely one machine. Near-identical to the NORD-2B, CPU on six boards not ten, released *before* the NORD-10. 43 installed by Aug 1974. |
| NORD-4 | - | **name only** | - | **New 2026-08-27.** A mirror folder is named `01-NORD-1 NORD-4`. Nothing else anywhere. |
| NORD-5 | assembler only | yes | in NORD-1 | Compute module; needs a NORD-1 host. |
| NORD-9 | - | name only | - | A bare name in Norwegian Wikipedia. **No ndwiki page exists.** May not be real. |
| NORD-10, NORD-10/S | **yes, rich** | **yes, 2** | in timeline | ND-06.008.01 now read: rings, paging, register block and context-switch times all verified from primary. Ready to write as its own notes. |
| NORD-12 | **no manual** | yes | - | **The manual exists in the mirror** - `heim.bitraf.no/tingo/.../NORD-12_Reference_Manual_ocr.pdf`. Needs importing. |
| NORD-42 | - | **yes** | - | Not a separate design: an **OEM NORD-12** for Noratom-Norcontrol. |
| NORD-50 | **yes, 2 comms docs** | **yes, settled** | - | **Array processor, total slave to the NORD-10/S.** Five more manuals sit in the mirror unimported. |
| ND 1100/S - ND 1400/S | - | one line | - | **New 2026-08-27.** System names for NORD-10/S plus NORD-50 combinations. |
| ND-100, /CE, /CX | **yes, rich** | yes, 2 | in timeline | ND-06.015.02 now read: single board, bit-slice width, 2K/4K x 64 control store, WCS option and address space all primary. |
| ND-110, /CX, PCX | **yes, 2 docs** | yes | in timeline | ND-06.026-1 now read: all three gate arrays, the one-card contents, the 8K x 64 writable control store and CILP verified from primary. |
| ND-120 / Delilah | test programs only | yes | - | No dedicated ND-120 manual found here. |
| ND-125 | - | one line | - | **Appears nowhere else in this repo.** Cited to sintran.com ECO 100-786, 1994. |
| Butterfly 110 / Teamstation | - | en.wp only | - | Ericsson PC/AT host, ND-110PCX on two ISA cards. One source. |
| ND Satellite range | 3D model | yes | - | Real: the COSMOS name was a pun on it, and there is a measured model in `Hardware/3D-Models/`. |
| ND-500 family | **yes, rich** | stub | - | Model numbers map to four implementations, not four designs. |
| ND-505 | **yes, product sheet** | yes | - | **29-bit user addressing settled.** The 31-bit / snipped-pin-27 story is unverified. |
| ND-5000 family | **yes, rich** | yes | **partly, elsewhere** | `SINTRAN/ND5000/ND5000-FAMILY-MODELS-REFERENCE.md` covers this from primary manuals. |
| ND-5830 / 5850 Rallar | some | yes | - | KUSK and GAMP gate arrays. Date disputed, 1987 or 1990. |
| Server 88 / TpServer / ES | thin | one line | - | 1988. Almost nothing. |
| ND-88000 / Uniline / NDIX | - | en.wp only | - | The Unix years. One uncited source for the whole subject. |
| The company 1967-2004 | **15 annual reports in the mirror** | **yes, 4** | - | Reports unimported. Plus an eyewitness account of every name change to 2004. |

## What we should get next, in order of value

### 1. Import from the mirror - it is already on this disk

Everything below is a PDF sitting in `E:\Dev\Ronny\mirror-sintran-com`, most of
it already OCR'd once. Nothing needs finding. Full list and file paths in
**[OCR-WANTED.md](OCR-WANTED.md)**. In order:

- **Five NORD-50 manuals** plus its maintenance manual and assembler - they prove
  the host-and-slave architecture from primary sources
- **Fifteen ND annual reports** (1977-1992) - the primary basis for the whole
  company story, which currently rests on wikis
- **103 company newspapers** - 51 issues of FRND through the crisis years, plus
  ND-NYTT and the English-language ND News
- **The NORD-12 Reference Manual** - the last big machine-manual gap
- ND-06.019.01 ND-10 device programming instructions, and earlier editions of the
  NORD-500 Reference Manual and NORD-100 Functional Description

### 2. Documents named by our sources that we do not hold

- **"Norsk Data - hva gikk galt?"** ("Norsk Data - what went wrong?"). The ndwiki
  history page cites this **eight times**, for the PLANC date, the SIBAS numbers,
  the ND-100/CE delivery, the ND-110 and ND-5000 introductions, SINTRAN-IV and
  Uniline. It is the single most-cited source behind our best secondary, and we
  have never seen it.
- **ND-NYTT No 5, September 1972** specifically. The mirror has three ND-NYTT
  issues, all 1981-82. The September 1972 issue is cited by two separate ndwiki
  articles - for the NORD-5 performance figures and for the NTNU machines that
  include NORD-1 serial 47 - and **it is not in the mirror**.
- **Annual reports for 1967-1976, 1981 and 1982** - the mirror has the other
  fifteen.
- **The two NORD-2B manuals** - Hardware Manual Volume 1 (December 1970) and I/O
  System (March 1971), linked from ndwiki as being in the Tingo collection, not
  confirmed present in the mirror.
- **Cambridge Memories EXPANDACORE 18 documentation** - the last route to the
  NORD-1 18-bit core question.
- The two books: Heradstveit, *Eventyret Norsk Data* (1985), ISBN 8272010402; and
  Steine, *Fenomenet Norsk Data* (1992), ISBN 8200215016.

### 3. Questions answerable from manuals already on this disk

Nobody needs to find anything for these. They are sitting here unread.


- **When was the NORD-10 dropped from SINTRAN III?** ndwiki says version H was the
  last for the NORD-10; our own
  `SINTRAN/Release-Documentation/SINTRAN-III-Release-History.md` says NORD-10 was
  dropped at J. If H was last, the drop was at I.
- **ND-60.116.01 NORD-10 - NORD-50 Operator's Guide** is still unopened.
  ND-06.005.01 has now been read - see "Closed" above.
- **What separates the ND-500/1 from the ND-500/2**, and what each of the four
  ND-500 implementations actually changed. `Reference-Manuals/500/` is unread.

### 4. Image licences

Three ND-5000-family images in `images/nd-5000/` have no established licence and
are presumed to come from norsk-data.com, which grants none. They are fine for
research here and must not be published without asking. See
[images/CREDITS.md](images/CREDITS.md).

## Conflicts still open across all sources

Kept in one place so they are not quietly resolved. Details and citations are in
each source file header, and the NORD-1 ones in [NORD-1.md](NORD-1.md).

| Question | Positions |
|----------|-----------|
| Founding date | 7 July 1967 (timeline, Broennoeysund; Norwegian Wikipedia) vs 8 August 1967 (English Wikipedia) |
| How many founders | Three (both Wikipedias) vs four, adding Terje Mikalsen (timeline) |
| First NORD-1 delivery | 1967 to Norcontrol (Norwegian Wikipedia) vs nine sold in 1968 (timeline) vs three installed 1968 (ndwiki history) |
| When Skaar became MD | 1977 (Norwegian Wikipedia) vs 1978 (timeline). Kolbjoern Johansen 1972-77 appears in one source only |
| What the NORD-50 was | 1973 array processor (**primary** ND Design Goals) vs 1975 second-generation supermini (three secondaries) |
| NORD-1 built | "at least 142" (1974 magazine survey) vs ndwiki history annual counts that pass 160 by 1974 |
| ND-100 data channel rate | 16 Mbit/s (Reference Manual 2.2) vs ~20 Mbit/s (same manual, 1.2) |
| NORD-1 interrupt levels | 15, then 256, then a flat 16 - all in the same primary manual |
| ND-5850 launch | 1987 (both Wikipedias) vs 1990 (timeline) |
| ND-5000 introduction | 1985 series (ndwiki history) vs 1987 family (everywhere else) |
| Stock listings | London 1981 + New York/Nasdaq 1982 (both Wikipedias) vs London 1981 + Stockholm 1983 (timeline) |
| ~~MOVEW/TSET/RDUS~~ | **SETTLED: CX only**, from the primary ND-100 Reference Manual. English Wikipedia is wrong |
| ND-505 width | 28-bit (Norwegian Wikipedia) vs 29-bit user addressing (**settled - our own ND-505CX product sheet**) vs 31-bit physical (ndwiki, unverified) |
| Tandberg | bought 1979 and kept (timeline) vs bought 1979 and independent again autumn 1980 (ndwiki history) |
| First CERN contract | 1972 (timeline) vs 1974, Lab II, NORD-10 (ndwiki history) |

## One thing to be careful about

Several of our secondary sources are **not independent of each other**. The ndwiki
ND-100 article says it began as a copy of the English Wikipedia article; the ndwiki
history page says it began as a copy of a NODAF wiki article; the ndwiki NORD-1
article says it began as a copy of English Wikipedia. When two of them agree, that
may be one source counted twice. Agreement between wikis is not corroboration.

The primary manuals in `Reference-Manuals/` do not have that problem, and this repo
holds a lot of them. **Most of the questions above are answerable from documents
already on this disk.**
