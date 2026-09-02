# The machines, the years, and how the hardware evolved

Scope set by Ronny, 2026-08-27: the machines and years, how important each was,
and how the hardware evolved and improved.

**How to read the confidence marks.** **[P]** = a primary ND document held in this
repo. **[S]** = a secondary source in [sources/](sources/). **[?]** = one source
only, or the sources disagree. Where a year is disputed, both are shown. Nothing
here is written from memory.

---

## 1. The chronology

| Year | Machine | Bits | What it was | Importance |
|------|---------|------|-------------|------------|
| 1962-67 | SAM, SAM 2 / FLINK | - | Built at FFI Kjeller, before the company. Said to be the basis of the NORD-1 **[S]** | The seed. No documents held |
| **1968** | **NORD-1** | 16 | The first machine. Core memory, hardware floating point, 8 general registers **[P]** | **Founding machine.** Everything descends from it |
| 1970 **[?]** | NORD-2B | 16 | A simplified, cheaper NORD-1, 4-16 kW core **[S]** | Volume seller. Many went to CERN |
| 1971-72 | NORD-20 (= NORD-2U?) | 16 | Near-identical to the NORD-2B, CPU on six boards not ten. Released *before* the NORD-10 **[S]** | Low end. 43 installed by Aug 1974 |
| **1972** | **NORD-5** | 32 | A compute module attached to a NORD-1 host. Own core, one program at a time **[S]** | **First 32-bit.** Beat the VAX by six years - but only as an attached processor |
| **1973** | **NORD-10** | 16 | Microprogrammed, paged, 16 interrupt levels with their own register sets **[P]** | **The breakthrough machine.** CERN, and the architecture the rest of the 16-bit line inherits |
| **1973** | **NORD-50** | 32 | Array processor. A "total slave to the NORD-10/S" - no I/O, no interrupts of its own **[P][S]** | An **option** on a NORD-10, not part of it: 3 installed against 62 NORD-10s in 1975. Seismic and scientific work |
| 1974 | NORD-42 | 16 | An **OEM NORD-12** for Noratom-Norcontrol, in their DATABRIDGE and in ship simulators **[S]**. ND's microprogram manual covers "NORD's 10/S, 10, 42 instruction repertoire" **[P]** | Not a design - a rebadge, and primary-confirmed as NORD-10 family |
| 1974-75 **[?]** | NORD-12 | 16 | Compact NORD-10-family machine, same instruction set and same microcode ROM. MOS memory 4K-64K words **[S]** | The cheap end of the NORD-10 family. Peripherals and interfaces interchangeable with the NORD-10 |
| **1975** | **NORD-10/S** | 16 | NORD-10 plus cache and improved paging **[P]** | The mature 16-bit machine. Host to the NORD-50 |
| **1979** | **ND-100** | 16 | Bit-sliced **single-board** CPU, 2K x 64 microcode PROM. 64 KW address space, **16 MW (32 MB) with the memory management system**. Software compatible with the NORD-10/S, same SINTRAN III **[P]**. Began life as the NORD-10/M **[S]** | **The workhorse**, and a complete machine on its own - most shipped with no coprocessor at all. Compacts still shipping in 2001 |
| **1981** | **ND-500** | 32 | 32-bit supermini that "relied on a ND-100 to run the operating system" **[S]** | **The performance line**, sold as ND-510 to ND-570 - an upgrade to an ND-100 installation, never a machine on its own |
| 1981 | ND-100/CE | 16 | Commercial Extended - decimal arithmetic and stack instructions, by swapping the microcode PROM **[S]** | Aimed at commercial work, not science |
| 1982 | ND-100/CX | 16 | Improved CE plus **MOVEW, TSET, RDUS and the segment-change instructions - "CX only"** **[P]** | The instruction set the ND-110 inherits |
| ~1982 | ND-505 | 32 | ND-500/2 with 29-bit user addressing, narrowed to clear the **CoCom** embargo **[P]** | Politics as a design constraint |
| 1983 | ND-570/CX | 32 | Top of the ND-500 line. 3.2 WMIPS, 5.9 on ND's ADP scale, 71-83 users **[P]** | Peak of the ND-500 line - and later retired by the ND-5700 |
| **1985** | **ND-110** | 16 | CPU and MMS on one board. Three gate arrays - RMIC, BUFALU, RMAC. 90 W to 60 W **[P][S]** | **First semi-custom VLSI.** Micro-instruction cache; traps like the ND-500 |
| 1986 | ND-110/CX ("RASK") | 16 | 365 devices down to 228, single module, 40% less power. ND-110 Compact and Satellite alongside **[S]** | Also became the I/O processor for ND-500 systems |
| 1986 | ND-580/CX 20/30/40 | 32 | Two to four ND-570 CPUs plus one ND-110/CX **[S][?]** | Multiprocessing on the 500 line |
| **1987** | **ND-5000 (Samson)** | 32 | One CPU replacing 24 ND-500 cards. Runs SINTRAN III and **NDIX** **[S]** | **Not a new architecture** - a new implementation of the ND-500 one **[P]**. Announced **27 Jan 1987**, delivery Q2 1987 **[P]**. Still needs an ND-100/110/120 for I/O |
| 1987 | Butterfly-110 | 16 | Ericsson PC/AT with an ND-110PCX on two ISA cards; MS-DOS boots SINTRAN III/VSX **[S]** | The strangest machine ND built. 900 pages of manuals exist |
| 1987 **[?]** | ND-120/CX ("Delilah") | 16 | One LSI gate array. ~1.9x the ND-110/CX. Nearly sold as the **ND-1000** **[S]** | End of the 16-bit line's evolution |
| 1988 | Server 88 / TpServer / ES | - | Standard platform configurations **[S]** | Repositioning, not new silicon |
| 1989-90 | ND-5830 / ND-5850 ("Rallar") | 32 | **"N-5000 Basic CPU IV"** - the fourth CPU type *inside* the ND-5000 line, not a separate line **[P]**. Chips dated 1989 | Last ND CPU. Built on **Dolphin** silicon |
| ~1990 **[?]** | ND-5950 | 32 | Multi-CPU Rallar **[P]** | In ND manuals; in no wiki |
| 1989-91 | Uniline 33 / 88, ND-88000 | - | Motorola 68030 then 88000, Unix. Built by Dolphin **[S]** | **ND leaves its own CPUs.** The end of the architecture |
| ~1994 | ND-125/CX | 16 | ND-120 CPU with faster and larger on-board memory, 8/12/16 MB **[S][?]** | The last ND-100-line CPU. In one source only |

---

## 2. How the hardware actually evolved

Six threads run from 1968 to 1990. Each is a straight line of improvement, and
each is traceable in the documents.

### Thread 1: logic technology - the CPU shrinks from two racks to one chip

| Machine | Implementation | Size |
|---------|---------------|------|
| NORD-1 | 7400-series MSI TTL, **over 1,300 ICs** **[S]** | Two racks, 64 cards |
| NORD-10 | MSI TTL | **24 printed circuit boards** **[S]** |
| ND-100 | MSI plus **bit-slice** - "an integrated circuit containing a **4-bit subsection of the 16-bit wide ALU** and register section", so four of them **[P]** | **"ND-100 is a 16-bit general purpose single board computer"** **[P]** |
| ND-110 | **Three semi-custom VLSI gate arrays** - RMAC, BUFALU, RMIC **[P]** | One card carrying CPU, MMS, cache, control store, interrupt and trap handlers, timing and real-time clock, panel interface, **terminal 1 serial interface**, register file and bus controller **[P]** |
| ND-110/CX | Three new ICs | Device count **365 to 228** **[S]** |
| ND-120 | **One LSI gate array** (Delilah) **[S]** | A chip |

The ND-110's three gate arrays are the clearest single step. **RMIC** ("Rask MIC",
speedy MIC) replaced three 74S482 sequencers and about 30 other ICs. The
**BUFALU** replaced four Am2901 bit-slice processors plus the data bus, general
purpose and internal register blocks. **RMAC** did in hardware the address
arithmetic the ND-100 had done in microcode. **[S]**

Power tells the same story: CPU plus MMS was 90 W on the ND-100, 60 W on the
ND-110, then 40 percent less again on the ND-110/CX. **[S]**

### Thread 2: control - hardwired, then microcoded, then writable

- **NORD-1**: no microcode. ND's Volume II describes the method - decide registers
  and instruction format, draw flow diagrams of every instruction, turn those into
  logical equations, then draw logic diagrams and distribute circuits across
  boards. The boards are the *last* step. **[P]**
- **NORD-10 / NORD-10/S**: microprogrammed. The primary microprogram manual gives
  the shape exactly: **"The micro-instructions are stored in a 1k x 32 bits Read
  Only Memory - ROM."** Four microinstruction types, selected by ROM bits 31 and
  30 - **ARITHMETIC, INTERBLOCK, JUMP, LOOP**. The entry point is generated from
  the machine instruction's opcode by hardware; a microprogram counter walks the
  ROM, JUMP branches, and because the counter can be read there is a simple
  subroutine capability. Entry points sit two locations apart because most
  instructions take two ROM words. **[P]**

  The ROM is not only instructions. Its five documented sections are: the
  instruction repertoire, **the operator panel driver**, **MOPC** (operator
  communication in stop mode), **the bootstrap loader**, and **a memory check**.
  **[P]** - which confirms the secondary claim that microcode drove the panel and
  the bootstraps, and names the parts.

  ND's own word for the control unit was "microprocessor" - short for *microcode
  processor*, not the modern meaning. **[S]**

**Why they made the change, in ND's own words.** The microprogram manual sets
microprogramming against exactly the design the NORD-1 used:

> "In a **non-microprogrammed machine**, these signals are derived directly from
> the instruction register and a **large and complicated Time Counter/Cycle
> Counter**. This type of control logic is not easily structured and is difficult
> to describe and understand." **[P]**

The NORD-1's card list contains card **123 "Cycle counter"** and card **151 "Time
counter"** - so the machine ND is describing as hard to structure and understand
is its own previous one. Its stated gains were orderly design, documentation and
testing, and flexibility: *"new instructions may be added without changing
hardware design or test methods."* **[P]**

That flexibility was sold, not just claimed. **A customer could order one of two
floating-point formats, 32-bit or 48-bit**, as alternate microcode on the same
hardware. **[P]**
- **NORD-12**: same instruction set **and the same ROM** as the NORD-10 - that is
  what made them one family. Up to 1024 customer-specified instructions in an
  added PROM. Microcode also drove the operator's panel and the bootstrap
  loaders. **[S]**
- **ND-100**: the control store doubles in width and grows. **"All instructions are
  executed by firmware residing in a 2 K by 64 bits programmable read only memory
  (PROM) called the microprogram control store."** **[P]** - 64 bits wide against
  the NORD-10's 32.

  **And the CX-option is literally a bigger PROM**: *"By expanding the microprogram
  PROM to 4 K by 64 bits, a number of instructions are introduced. These
  instructions comprise what is known as the CX-option."* **[P]** So CE and CX were
  not new silicon but new microcode in a larger part - which is exactly why the
  ND-100/CE was described as an upgrade by replacing the microcode PROM.

  A **writable control store was already available as an option**, before the
  ND-110 made it standard: *"To allow dynamic microprogramming, a **256 word by 64
  bits writable control store** is available as an option. This makes it possible
  for software to extend the ND-100 instruction set for special applications."*
  **[P]**

  The manual also explains why bit-slices and microcode go together: the slices
  "must be programmed to execute the system functions ... driven by a set of
  control lines (bits) from a microinstruction. This gives the possibility of
  changing or modifying the microprogram while keeping the existing hardware."
  **[P]**
- **ND-110**: **writable control store as standard**, and the manual says so while
  pointing back at its predecessor: *"To allow dynamic microprogramming, the
  microprogram control store is writeable **(optional on ND-100)**."* **[P]** -
  independent confirmation that the ND-100's writable store was the option that
  became the ND-110's standard.

  The store itself is **8K deep by 64 bits wide**, addressed in four 16-bit
  groups **[P]**. Built, per the secondary sources, from 4K x 4 bit 40 ns SRAMs and
  loaded at power-up and Master Clear from two 32K x 8 EPROMs. **[S]**

**The control store across three generations, all primary:**

| Machine | Control store | Writable? |
|---------|---------------|-----------|
| NORD-1 | none - hardwired | - |
| NORD-10 / 10/S | **1K x 32 bits** ROM | no |
| ND-100 | **2K x 64 bits** PROM, 4K for the CX-option | 256 x 64 as an **option** |
| ND-110 | **8K x 64 bits** | **standard** |

Eight times the depth and twice the width of the NORD-10, in fourteen years - and
the shift from read-only to writable is the ND-100-to-ND-110 step.

**One thing moved the other way, out of microcode and into hardware.** The ND-100
manual says *"The address arithmetic is implemented as microprogram routines. This
implies that the addressing structure of ND-100 can be changed by rewriting the
microprogram."* **[P]** The ND-110's third gate array, **RMAC**, is described as
implementing "hardware address arithmetic, which in the ND-100 had been done in
microcode" **[S]**. Both ends of that change are now documented - flexibility
traded for speed, once the addressing structure had stopped moving.

### Thread 3: speed

| Machine | Microinstruction cycle | Minimum microinstructions per instruction |
|---------|------------------------|-------------------------------------------|
| NORD-10 | 300 ns **[S]** | - |
| NORD-12 | 490-500 ns **[S]** | - |
| ND-100, ND-100/CE | 150 ns **[S]** | 3 |
| ND-110, ND-110/CX | 100 ns **[S]** | **1** |

The ND-110 is the real jump: same job in one microinstruction instead of three,
and each one half again as fast. On top of that, the ND-110/CX was 1.5 to 3.5
times an ND-110, and the ND-120/CX about 1.9 times an ND-110/CX. **[S]**

Memory speed follows separately: the NORD-1's fastest usable cycle was 1
microsecond **[P]**; NORD-10/S memory modules are listed at **8K x 18 bits, 300 ns**
and **32K x 18 bits, 300-350 ns** **[P]**; the ND-125 got to a 150 ns memory cycle
using 70 ns modules **[S][?]**.

### Thread 3b: memory width - where the "18 bits" comes from

The NORD-10/S Reference Manual states the convention plainly: **"Memory modules
with 18 bits word length provide one parity bit per byte, while 21 bit modules are
used for memory error correction."** **[P]**

So an ND memory word is 16 data bits **plus one parity bit per 8-bit byte** = 18,
and ND also built 21-bit modules for full error correction. That is a primary ND
document confirming the scheme that had only been reachable through a wiki
quotation of the NORD-12 manual - and it is the most likely explanation of the
"16K x 18 bit" core on the NORD-1 serial 47 price list. See
[NORD-1.md](NORD-1.md), which still records it as unproved **for the NORD-1
specifically**, since this manual describes a later machine.

### Thread 4: caching - added late, then deepened

- NORD-1, NORD-10: none.
- **NORD-10/S (1975)**: cache introduced. **[S]**
- **ND-100**: macro-instruction cache. **[S]**
- **ND-110**: adds a **micro-instruction level cache** on top - the ND-100's
  "mapping" step disappears because the first micro-instruction word of a
  macro-instruction is written into the control store cache. **[S]**
- **ND-110**: cache control becomes fine-grained. A new instruction, **TRR CILP**
  (opcode 150113), *"allows the programmer to inhibit individual pages in cache"* -
  one bit per physical page. **[P]** By this generation the cache is something
  software has to be able to switch off selectively, page by page.
- **ND-5000**: caching becomes the product line itself. Instruction cache, data
  cache, address cache, Smart IfGo and WICO are switched on or off, with a clock
  jumpered 156 ns or 70 ns, to make an ND-5200, 5400, 5500, 5700 or 5800 out of
  what is largely the same hardware. **[S]**

### Thread 5: memory and protection - the part that matured fastest

- **NORD-1 (1968)**: 4K-64K words of core, 16-bit words, 1 microsecond. **[P]**
- **NORD-1 paging, documented February 1970**: programs written for 64K virtual
  core with only part resident, **256-word pages**, page table in core, hardware
  translation, a miss interrupting on the highest priority level. Protection was a
  16-bit **MPR** register with one flag per block, minimum block 1,024 words, and
  eight privileged instructions executable only from protected memory. Violations
  interrupted on level 14. **[P]**
- **NORD-10 / NORD-10/S (1973-75)**: paging in hardware, **1024-word pages**, a
  16-bit virtual address mapped to an **18-bit physical** address, extending memory
  from 64K to **256K words**. There are **four page index tables of 64 words each**,
  held in high-speed 16-bit registers - 64 entries x 1K words is exactly one 64K
  address space per table. **[P]**

  Two independent protection systems, and the manual is emphatic that **both must
  be satisfied**: the per-page mode bits (RPM, WPM, FPM - read, write, fetch) *and*
  the **Ring Protection System**. Four rings, 0 lowest to 3 highest; rings 2 and 3
  may use the whole instruction repertoire, rings 0 and 1 a restricted set; a
  program may reach its own ring and below, and nothing above. ND's recommended
  layout, verbatim: **ring 0 user programs, ring 1 compilers and assembler, ring 2
  operating systems, ring 3 kernel of operating systems**. In ring 3 (or with
  paging off) the virtual addresses 177400B-177777B are read directly as page index
  table addresses with mapping switched off; from rings 0, 1 and 2 the page index
  table is not reachable at all. **[P]**
- **NORD-12**: no memory management option, 64K words maximum - which is what
  separated it from the NORD-10. **[S]**
- **ND-500 / ND-505**: 29-bit user addressing, a quarter gigabyte each for program
  and data. **[P]**

The step that matters is the NORD-1 to NORD-10 one: pages grow from 256 to 1,024
words, physical memory outgrows the address register for the first time, and a
ring model appears on top of per-page flags.

### Thread 6: interrupts and context switching

- **NORD-1**: 16 levels, 0-15, 15 highest, run by two 16-bit registers, **PID**
  (detect) and **PIE** (enable). On a level change the seven central registers and
  the status flip-flops are saved and reloaded automatically - maximum **38 memory
  cycles, 45 microseconds**. **[P]**
- **ND-100**: the same idea, stated as architecture: *"To each level is assigned a
  complete set of working registers, and these registers are located in a
  **high-speed register file on the CPU module**. With this architecture, context
  switching consists of **selecting another set of working registers**. This is
  done by the microprogram."* **[P]**
- **NORD-10**: **160 registers, 128 of them program-visible**. The NORD-10/S
  Reference Manual gives the arithmetic exactly: "The register block contains
  **8 general registers for each program level and two scratch registers for each
  level** to be used by the micro-processor" - so 16 levels x 10 = 160, of which
  16 x 8 = 128 are the program's. A level change copies nothing at all. **[P]**
- **Context switch time improved between the two models**: ND's Design Goals gives
  the NORD-10 **1.5 microseconds**; the NORD-10/S Reference Manual gives "complete
  context switching from one program level to another in only **1 us**". Both are
  ND documents, and they describe different machines. **[P]**
- **NORD-10/S I/O scale**: "**2048 priority vectored interrupts** are standard, as
  well as **10 priority internal hardware status interrupts**". **[P]**
- **NORD-12**: 2.0 microseconds, and **2048 vectored priority I/O interrupts**
  standard. **[S]**
- **ND-110**: handles synchronous interrupts **as traps**, the way the ND-500
  does - a break with the ND-100. **[S]**

Forty-five microseconds to one is a factor of forty-five, and almost none of it
came from faster memory. It came from a change of method: the NORD-1 *copied*
seven registers and the flip-flops to and from core on every level change, while
from the NORD-10 on, each level simply owns its registers and a switch is a
change of selection. The ND-100 manual states the end point of that idea
plainly - context switching "consists of selecting another set of working
registers".

---

## 2a. The ND-500 architecture - one design, four implementations

**The ND-5000 is not a new architecture.** `ND-05.009.4 EN ND-500 Reference
Manual` states it on its own contents page: *"This manual is valid for both the
ND-500 and the ND-5000 computer systems. When the manual uses the name ND-5000
this is also valid for the ND-500 and vice versa."* **[P]**

So the 32-bit story is **one architecture, 1981 to 1990, in four hardware
implementations** - ND-500/1, ND-500/2, Samson and Rallar - sold under a long list
of model numbers. Even the "'87 extensions" that arrived with the ND-5000 "also
run on computer systems with the ND-500/1 and the ND-500/2 CPUs". **[P]**

That is a different shape from the 16-bit line, where the NORD-1, NORD-10 and
ND-100 are genuinely different machines that happen to stay compatible.

### The host relationship, stated as a definition

The same manual settles the pairing for the ND-500 and ND-5000 the way
ND-06.005.01 settled it for the NORD-50 - not as description, but as terminology:

> "The term 'CPU' is used for the ND-500 or the ND-5000 processor throughout this
> manual. Whenever the **I/O processor** is mentioned, this means the **ND-100 or
> the ND-110** processor." **[P]**

ND does not treat the 16-bit machine as a peer. It is *the I/O processor*, by
definition, in the reference manual for the 32-bit one.

The manual's own vocabulary carries the same asymmetry: *"The term 'word' always
refers to 32-bit words. **16-bit data items (ND-100 words) are referred to as
halfwords**."* **[P]**

### What kind of machine it was

The register block is the tell. Where the NORD-1 had eight flat registers, the
ND-500 has a set built for compiled high-level languages: **[P]**

| Group | Registers | Purpose |
|-------|-----------|---------|
| Addressing | **P, L, B, R** | program counter, subroutine link, **local variable base**, **record base** |
| Integer | I1-I4, 32-bit | accumulators or index registers; work on word, halfword, byte, bit and bit field |
| Floating | A1-A4, 32-bit | each extendable by E1-E4 into **four 64-bit double-precision accumulators** |
| Traps | ST, **OTE, CTE, MTE**, TEMM, 64-bit | status, and **own / child / mother** trap enables |
| Limits | TOS, LL, HL, THA, 32-bit | top of stack, low and high limit traps, trap handler address |
| Memory management | CED, CAD, PS, PSTP | current executing domain, current alternative domain, process segment, physical segment table pointer - microprogram only |

Dedicated **local variable base** and **record base** registers, a top-of-stack
register with low and high limit traps, whole chapters of **string instructions**
and **subroutine entry point** forms, and a trap system with mother/child
inheritance. This is a machine designed around procedure calls, records and
strings - which matches the ND-505 product sheet describing instructions
"tailored for high-level program execution efficiency, such as FORTRAN DO-loops
and COBOL string-handling". **[P]**

It also explains the "not von Neumann based" line in the norsk-data.com timeline,
which no source has ever justified: whatever was meant, the architecture is
unusually far from a plain accumulator machine. **The claim itself remains
unverified** - nothing in the reference manual uses that phrase.

## 2b. The 32-bit line, measured

The 16-bit threads above are well documented here. The 32-bit line was thinner
until this pass; the numbers below come from ND's own sales and hardware documents
via `SINTRAN/ND5000/ND5000-FAMILY-MODELS-REFERENCE.md`, which verified them on the
page.

### The performance ladder, on ND's own basis

**ND's unqualified "MIPS" means Whetstone MIPS** - a floating-point measure, not an
instruction rate. Every figure below is on that one basis, which is what makes them
comparable. **[P]**

| System | ADP/OA scale | WMIPS | Users |
|--------|--------------|-------|-------|
| ND-100/CX | 1.0 | - | - |
| ND-510/CX | 1.0 | 0.4 | 12-14 |
| ND-530/CX | 1.6 | 0.6 | 19-22 |
| ND-550/CX | 2.1 | 1.2 | 28-32 |
| ND-560/CX | 3.8 | 2.1 | 46-53 |
| ND-570/CX | 5.9 | 3.2 | 71-83 |
| ND-5700 | 5.9 | 3-3.5 | 71-83 |
| ND-5800 | 7.4 | 6-7 | 89-104 |
| ND-5900 Mod.2 | - | 12-14 | - |
| ND-5900 Mod.3 | - | 18-21 | - |
| ND-5900 Mod.4 | - | 24-28 | - |

Two things fall out of that table. **ND-550 and ND-560 are both real and
distinct** - which closes a question this repo had open, where one wiki listed
ND-550 and another ND-560 as if one were a mistake for the other. And the ND-5700
was built to *replace* the ND-570/CX rather than beat it: ND's own text says the
5700 "has approximately the same CPU performance as the ND-570/CX", the ND-5800
about twice, and the ND-5900 Models 2/3/4 two, three and four times the ND-5800.

### How important the ND-5000 actually was

ND compared itself with everyone else on the same Whetstone basis in December 1986.
On that table a single **ND-5800 (6-7)** sits level with a **VAX-8550, 8650 or 8700
(6.8)** and above a **VAX-8600 (4.4)** - but does *not* reach a **VAX-8800 (11)**.
Only the four-CPU **ND-5900 Model 4 (24-28)** beats DEC's top machine, by about
2.3x. Others on the page: HP-3000/950 6.7, IBM-4381-14 6.0, PRIME 9955 4.0,
WANG VS 200 3.3. **[P]**

So the honest reading is that ND's single-CPU machines were competitive with the
middle and upper-middle of DEC's range, and it took four processors to top it.

### Each ND-5000 model shipped with a named I/O processor

`Reference-Manuals/500/ND-05.020.01 EN ND-5000 Hardware Description.md` gives the
pairing model by model - so the host is not "an ND-100 or so", it is a specific
part of the product: **[P]**

| System | CPU type | CPU model | I/O processor | Master clock |
|--------|----------|-----------|---------------|--------------|
| ND-5200 | 1 | 2 | **ND-110** | Normal (70 ns) |
| ND-5400 | 2 | 4 | **ND-110/CX** | Slow (156 ns) |
| ND-5500 | 2 | 5 | **ND-110/CX** | Slow (156 ns) |
| ND-5700 | 2 | 7 | **ND-120/CX** | Normal (70 ns) |
| ND-5800 | 3 | 8 | **ND-120/CX** | Normal (70 ns) |

Two things worth noticing. The 16-bit line's own generations - ND-110, ND-110/CX,
ND-120/CX - are stepped *inside* the ND-5000 range, so buying a faster 32-bit
machine meant buying a faster 16-bit one to feed it. And the clock is not a simple
ladder: the ND-5400 and ND-5500 run **slow** at 156 ns while the cheaper ND-5200
runs at 70 ns, because their performance comes from caches instead.

The manual also confirms the CPU-type structure that section 2b describes: **"CPU
types 1 and 2 use the same mother board and baby modules. On CPU type 1, the cache
and AAP baby modules are removed. On the ND-5200 CPU, floating-point operations
are performed by the microprogram."** Type 3 is not simply type 2 plus a layer -
its mother board and cache/IDA baby module are unique to it, and it adds an
**IDAC "booster" module** used only in the ND-5800 and ND-5900. **[P]**

*(The model/cache table in our OCR of this manual has its columns merged, so which
caches each model enables is confirmed in structure but not readable line by line
from our copy. The CPU types, models, I/O processors and clock speeds above are
clear.)*

### The last generation was built on Dolphin silicon

**Rallar is not a separate product line.** The PCB index files it as **"N-5000
Basic CPU IV"**, the fourth CPU type inside the ND-5000 line, alongside Samson
types I, II and III. It came in two clock speeds, **25 MHz and 45 MHz**, with
**8, 16, 32 or 64 MB of local memory on the CPU assembly itself** - a change from
Samson, which was specified by master-clock *period* (70 ns normal, 156 ns slow)
rather than by frequency. **[P]**

Its three gate arrays are described in the archive maintainer's own catalogue of
chips he holds:

| Chip | Size | Dated | Role, verbatim |
|------|------|-------|----------------|
| **KUSK** | 47 x 47 mm | 1989 | "the controlling *kusk*" - Norwegian for driver or coachman |
| **GAMP** | 47 x 47 mm | 1989 | "the work *horse* chip" - *gamp* is a workhorse |
| **DSB** | 42 x 42 mm | 1995 | "division, square root and BCD arithmetic", on the RAAP module |

**RAAP** is the "Rallar Additional Arithmetic Processor", the successor to Samson's
AAP. And all three are described as **"Dolphin chips"** - Dolphin Server
Technology, the company ND spun its hardware R&D into in 1989.

That is the quiet ending of the hardware story. ND's last CPU generation was
designed by the spin-off, on dates (1989, 1995) that run past the parent company's
own end in 1992.

For symmetry, Samson's gate arrays were **ND-IMU, ND-1364 and ND-1365**, and one
ND-1365B variant is stocked "marked **Weitek**" - so the floating-point side was
bought in from a specialist.

**Caveat, stated plainly**: the KUSK/GAMP/DSB entries come from a collector's
catalogue written by an ex-ND employee who holds the parts, not from an ND
specification, and those three names appear in **zero** PDF text across the 7.3 GB
archive. Strong, single-sourced.

---

## 3. The one idea that shaped everything - and then killed it

Four generations, one pattern, confirmed by Ronny and stated in ND's own
documents: **a 16-bit machine runs the operating system, with a wider, faster
compute engine attached to it.**

### The dependency runs one way only

This matters, and it is easy to state the pattern in a way that gets it backwards.
**The coprocessors cannot run without a host. The hosts were complete computers,
sold and delivered on their own.** Ronny put it plainly: the NORD-10 and the
ND-100/110/120 were also delivered without the coprocessor.

The manuals describe the 16-bit machines as whole products, with no mention of
needing anything attached. The ND-100 Functional Description opens: *"ND-100 is a
16-bit general purpose single purpose computer ... It is completely software
compatible with NORD-10/S and runs the same operating system, SINTRAN III."*
**[P]** Nothing in it requires an ND-500. The secondary sources describe the
NORD-50 the other way round - "designed to be **attached to** a general purpose
NORD computer system" - which is the language of an option, not of a half-machine.

**The installed numbers show how much of an option it was.** From the ndwiki
year-by-year history **[S]**:

| Year | NORD-10 installed | NORD-50 installed |
|------|-------------------|-------------------|
| 1975 | 62 | 3 |
| 1976 | 83 | 3 |
| 1977 | 114 (NORD-10 / 10/S) | 7 |

(Those counts carry the caveat recorded in
[sources/ndwiki-history-of-norsk-data.md](sources/ndwiki-history-of-norsk-data.md)
- the page never says whether they are annual or cumulative. Either way the
**ratio** is the point.)

So the 16-bit line was the business. The 32-bit machines were a high-end option
bolted onto it for customers who needed heavy computation - meteorology, seismic
work, nuclear physics, flight simulation. That is why ND could sell a 32-bit
machine in 1972 without writing a 32-bit operating system, and it is why the
architecture survived twenty years: **the expensive half was optional, and the
cheap half stood alone.**

It is also why the ending hurt where it did. When the bottleneck bit, it bit the
*high-end* product - the one carrying the margin and the competitive claims -
while the volume machines went on working exactly as before.

| Host | Attached engine | Years | Evidence |
|------|-----------------|-------|----------|
| NORD-1 | NORD-5 | 1972 | **[S]** |
| NORD-10/S | NORD-50 | 1973-75 | **[P]** - ND-06.005.01 calls it "a slave to the NORD-10" |
| ND-100 / ND-110 | ND-500 | 1981 | **[P]** - ND-05.009.4 defines them as "the I/O processor" |
| ND-100 / ND-110 / ND-120 | ND-5000 | 1987 | **[P]** - same manual covers both |

**Three of the four rows are now primary.** Only the NORD-1 and NORD-5 pairing
still rests on a secondary source.

**This is now proved from a primary ND manual, not a wiki.**
`Reference-Manuals/10/ND-06.005.01 NORD-10 - NORD-50 Communication System.md`,
August 1975, states it outright:

> "The communication between the NORD-10 and the NORD-50 is based on the use of the
> **IOX instruction** in the NORD-10. Accordingly, the hardware part of the
> communication unit is made in such a way that the NORD-10 when software is
> concerned, **may regard the NORD-50 as an I/O device**. In the communication
> procedure **the NORD-10 has complete control, and the NORD-50 is regarded as a
> slave to the NORD-10**." **[P]**

And in the very next sentence, the whole problem in miniature:

> "All the data transfers between the two computers are done in **16 bits parallel
> mode**. This means that the **NORD-10 must use two IOX instructions to transfer a
> NORD-50 word** to or from the NORD-10." **[P]**

The 32-bit machine's registers are all 32 bits - BP, BQ, SA, SD, MD, MA - and every
one of them has to be moved through a 16-bit host two halves at a time, one I/O
instruction each. That is the bottleneck the ND-5000 documentation complains about
fourteen years later, present in the first pairing and never designed out.

The manual also documents a feature worth knowing about for anyone emulating these
machines: **simulated memory**. A program on the NORD-10 acts as the NORD-50's
memory controller, so the NORD-50 CPU can be debugged **with no memory connected to
it at all** - instructions and data are fed to it as if an ordinary memory were
there. **[P]**

It was a good idea for a long time. It let a small company sell a 32-bit machine
in 1972, six years before the VAX, without writing a 32-bit operating system; sell
the same operating system across the whole range for twenty years; and offer the
fast machine as an **upgrade to an existing installation** rather than a
replacement for it.

### How the link was actually built, across three generations

The repo's own carve work on the interfaces -
`SINTRAN/ND500/ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md`, built from NPL
source and ND manuals - shows the mechanism changing exactly twice in fourteen
years:

| Pairing | Year | Mechanism |
|---------|------|-----------|
| NORD-10/S -> NORD-50 | 1975 | **IOX instructions**; two per 32-bit word **[P]** |
| ND-100 -> ND-500/1, /2 | 1981 | **DMA, PCB 3022** - a bank of **16 IOX registers**, direct register I/O |
| ND-100/110/120 -> ND-5000 | 1987 | **Octobus + MFB** - message passing over a serial bus |

The ND-500's register map is the NORD-50 idea grown up but unchanged in kind:
`LMAR5` / `RMAR5` load and read a memory address register, `LSTA5` / `RSTA5` the
status, `LCON5` / `RCON5` the control, plus master clear, terminate, TAG in and
out, limits, lock and unlock. A 16-bit host poking a 32-bit machine one 16-bit
register at a time - the same shape as 1975, six years and one generation later.

**And the status register makes the dependency literal.** When the ND-500 stops,
it writes a five-bit **STOPREASON** into `RSTA5`, and reason 1 is **MOCALL - a
monitor call**. The 32-bit processor stops itself and asks the 16-bit machine to
go and do the work. That is the architecture's whole bargain in one register
field: the fast machine computes, and every time it needs the world it halts and
hands over.

The **octobus** of 1987 is the attempt to escape that - message passing and shared
memory instead of register-at-a-time control. It came too late to save the line.

**And then it became the trap.** The ND-5000 documentation says it directly: as
the 5000 line got faster, the dual architecture **bottlenecked because all I/O had
to pass through the ND-100**. English Wikipedia reports the same view from inside
ND around the ND-120's development - the mixed 16/32-bit architecture was
increasingly seen as the constraint on the ND-500/5000 line, and the ND-120 was
nearly renamed ND-1000 to mark the break. **[S]**

The company's answer, from 1989, was to stop building its own CPUs: Motorola 68030
and 88000, Unix, and the hardware group spun out as Dolphin. **[S]**

---

## 3b. The operating system, which is what made it one range

Ronny asked where TSS started and how it became SINTRAN. The sources answer both,
and the first answer is "both machines" - which is why it is easy to be unsure.

**Scope note**: operating-system detail belongs under `SINTRAN/` per `CLAUDE.md`,
and `SINTRAN/Release-Documentation/SINTRAN-III-Release-History.md` covers versions
J to N properly. This section is here only because the OS is the thing that made
a NORD-10 and an ND-5000 the same product line.

### Two lines that merged

SINTRAN did not start at Norsk Data at all. The original was released in **1968**
by the Department of Engineering Cybernetics at NTH with SINTEF, and the name is a
portmanteau of **SINTEF and FORTRAN**, Fortran being the implementation
language. **[S]**

From there, two separate systems ran side by side:

| System | Purpose | Machines |
|--------|---------|----------|
| **SINTRAN II** | real-time only | NORD-1 and early NORD-10 |
| **NORD-TSS** | timesharing only, from 1971 | NORD-1 and early NORD-10 |

**TSS ran on both machines, which is the answer to the question.** Norwegian
Wikipedia lists it as "Nord tidsdelingssystem fra 1971, **for Nord-1 og tidlige
Nord-10**", and puts it against the NORD-1 entry as well - "Nord-1 ... kjoerte Nord
TSS fra 1971". ndwiki says the same from the other side: NORD-TSS was "the
timesharing alternative for NORD-1 (and, presumably, early NORD-10 systems)". And
the ndwiki NORD-10 article states that **"The NORD-10 was delivered with a
time-shared system, NORD-TSS, and a real-time multitasking operating system,
SINTRAN III"** - so the NORD-10 shipped with both, during the changeover. **[S]**

The "multilingual" in Multilingual Time Sharing System was literal: under NORD-TSS
"all users could simultaneously run any of the systems **FORTRAN IV, BASIC, MAC
Assembler, NODAL, NORD PL, or QED**". **[S]**

### The merge, and why it mattered

**SINTRAN III absorbed both.** It "included support for time-slicing as well,
essentially incorporating the features of both SINTRAN II and NORD-TSS and
obsoleting both" **[S]** - so before SINTRAN III you had to choose between
real-time and timesharing, and after it you did not. It was also the first written
entirely by Norsk Data, in **NORD PL**, from **1974**. **[S]**

That merge is what let one operating system cover a NORD-10 of 1973 and an
ND-5000 of 1987, and it is the other half of why the host-and-slave architecture
survived so long: ND never had to write a second operating system, for a second
machine, in a second language.

### Where it ended

**SINTRAN III release N is the last.** Our own release history has it as
**ND-860230.8, February 1993** - the "final documented release", bringing
performance work including a multi-threaded ND-5000 swapper. **[P]**

One detail in that document is worth keeping. The N-version manual is
**copyright Comma Data Service AS** - not Norsk Data. Norsk Data as a company had
gone in 1992; the last release of its operating system was published a year later
under the name of the successor company. See
[sources/norsk-data-com-nd-names.md](sources/norsk-data-com-nd-names.md) for that
chain of names.

Ronny keeps a working NORD TSS effort at `E:\Dev\Ronny\TSS`, with a `tss-boot`
skill for booting it on the nd100x emulator - so the pre-SINTRAN half of this
story is not only documented here, it runs.

## 4. What is still unproven here

- **The NORD-5's status as "world's first 32-bit minicomputer".** English Wikipedia
  is careful about this and our other sources are not: it was "claimed to be, and
  reported as" a 32-bit minicomputer, being a 16-bit machine with a 32-bit attached
  processor. The Interdata 7/32 followed shortly. **[S]**
- **"First minicomputer with virtual memory" and "with floating point as
  standard".** The NORD-1 features are proved from the primary manual. The word
  *first* is not, and no ND document can prove it.
- **Where the "ND-5000 series introduced 1985" claim comes from.** The
  introduction date itself is settled - 27 January 1987, from a primary sales
  document - so the ndwiki history's 1985 entry is now unexplained rather than
  merely disputed. It may be describing an earlier internal milestone.
- **The ND-125** rests on a single paragraph.
- **`Reference-Manuals/500/` is still unopened for this document.** The interface
  mechanism is now covered in section 3 from the repo's own carve work, and the
  architecture from the ND-500 Reference Manual - but the ND-500 and ND-5000
  hardware manuals in that folder have not been read here, and would be the primary
  check on both.
- **What separates the ND-500/1 from the ND-500/2 in hardware.** Section 2a
  establishes that there are four implementations of one architecture, but not what
  changed between the first two. The ND-500 Hardware Description (ND-05.011.01) is
  listed in the mirror index as **not available**.

For what would close these, see [OCR-WANTED.md](OCR-WANTED.md). Section 3 no
longer depends on the five NORD-50 manuals listed there - ND-06.005.01 settled the
host-and-slave relationship on its own - but those manuals would still be the best
route into what the NORD-50 actually computed, which nothing here covers.

## 5. How well sourced this document is

Written 2026-08-27, then verified across seven passes against the primary manuals
already in this repo. The evolution sections now carry more primary citations than
secondary. What changed under verification is worth recording, because it is the
argument for doing it:

| Claim | Before | After |
|-------|--------|-------|
| ND's "MIPS" | flagged as a wrong unit | **my error** - ND meant Whetstone MIPS |
| ND-5000 | "last new architecture" | a fourth *implementation* of the ND-500 architecture |
| Host and slave | a wiki claim | **primary in three of four pairings** |
| The 18-bit memory word | an unexplained oddity | an ordinary ND convention, 16 data + 1 parity per byte |
| MOVEW / TSET / RDUS | two wikis disagreed | **CX only**, from the ND-100 Reference Manual |
| ND-550 vs ND-560 | possibly one machine misremembered | both real, from ND's own performance ladder |
| The NORD-10 control store | "1K ROM" | 1K x 32 bits, four microinstruction types, five ROM sections |
| Why microprogramming | not addressed | ND's own criticism of the NORD-1's Time/Cycle Counter design |

Two corrections went against sources everyone else repeats, and one went against
something I had written myself. Both kinds are recorded in place rather than
quietly edited out.
