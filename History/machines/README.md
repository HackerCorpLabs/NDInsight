# The machines

Norsk Data sold **CPU boards**, **cabinets**, and **systems** made from the two.
This folder keeps them apart, because that is how the machines actually worked: a
Compact cabinet from 1985 and one from 1987 are the same box with a different card
in it.

If you want the story rather than the specifications, read
**[MACHINE-TIMELINE.md](../MACHINE-TIMELINE.md)** instead.

---

## The four groups

**A - [Standalone 1xx machines](cpu/nd-1xx/)**
One 16-bit CPU board in one cabinet. Runs NORD-TSS in the early days, then
SINTRAN III. Runs 1xx programs, and nothing else. This was the volume business -
most ND machines ever sold are these.

**B - [Coprocessor systems](systems/)**
A 1xx machine **plus** a 32-bit compute processor in the same cabinet. The 1xx CPU
is the I/O and control processor and runs SINTRAN III; the 32-bit CPU does the
computing. These run **everything a standalone 1xx runs, and 5xx programs as
well**. The ND-500 and ND-5000 families.

**C - [Butterfly](Butterfly.md)**
Not an ND machine at all: a board set that plugs into an IBM PC/AT with a 286.

**D - [The early machines](early/)**
NORD-1 and NORD-10 and their relatives. Kept together because the family is small
and, on these, the CPU and the machine are the same thing - there was no swapping
boards between cabinets.

---

## Why CPU and cabinet are separate documents

ND's own manual puts it plainly: **"like the PDP-11 line, the CPU decided the name
of the computer."** You did not buy an "ND-110 Compact" as a distinct product - you
bought a Compact and chose which CPU board went in it, and in 1986 ND explicitly
offered upgrades of existing systems to the newer boards.

So: **[cpu/](cpu/)** documents describe boards, **[cabinets/](cabinets/)** describe
boxes, and **[systems/](systems/)** describe the group-B combinations that ND sold
under their own model numbers.

---

## CPU boards

### 1xx - the 16-bit line ([cpu/nd-1xx/](cpu/nd-1xx/))

Any of these goes in any 1xx cabinet, and each is also the I/O processor for some
group-B system.

| Year | Board | What changed |
|------|-------|--------------|
| 1979 | **[ND-100](cpu/nd-1xx/ND-100.md)** | The whole CPU on one board, bit-sliced |
| 1981 | [ND-100/CE](cpu/nd-1xx/ND-100-CE.md) | Decimal arithmetic, by a bigger microcode chip |
| 1982 | [ND-100/CX](cpu/nd-1xx/ND-100-CX.md) | MOVEW, TSET, RDUS, segment changes |
| 1985 | **[ND-110](cpu/nd-1xx/ND-110.md)** | Three custom gate arrays; CPU and memory management on one board |
| 1986 | [ND-110/CX](cpu/nd-1xx/ND-110-CX.md) | 365 chips down to 228; front end for ND-500 systems |
| 1987 | [ND-110/PCX](cpu/nd-1xx/ND-110-PCX.md) | The same design on two PC cards - see [Butterfly](Butterfly.md) |
| 1985-87 | [ND-120/CX](cpu/nd-1xx/ND-120-CX.md) | One LSI chip, "Delilah" |
| ~1994 **[?]** | [ND-125/CX](cpu/nd-1xx/ND-125-CX.md) | More and faster on-board memory. The last one |

### 5xx - the 32-bit compute processors ([cpu/nd-5xx/](cpu/nd-5xx/))

None of these runs on its own. Each needs a 1xx CPU beside it.

| Year | Board | What it is |
|------|-------|------------|
| 1981 | **[ND-500](cpu/nd-5xx/ND-500.md)** | The architecture itself - one design, four implementations |
| 1981 | [ND-500/1](cpu/nd-5xx/ND-500-1.md) | First implementation |
| ~1982 | [ND-500/2](cpu/nd-5xx/ND-500-2.md) | Second, with a prefetch processor |
| 1987 | **[ND-5000](cpu/nd-5xx/ND-5000.md)** | Samson and Rallar - third and fourth implementations |

## Cabinets ([cabinets/](cabinets/))

| Cabinet | Group | Backplanes | Notes |
|---------|-------|------------|-------|
| [Large (1xx)](cabinets/Large-1xx.md) | A | One | The full 19-inch rack |
| [Compact](cabinets/Compact.md) | A | One | Last ND machine delivered, 2001 |
| [Satellite](cabinets/Satellite.md) | A | One | COSMOS was named after it |
| [Large (5xx)](cabinets/Large-5xx.md) | B | **Two or more** | Different design from the 1xx large cabinet |
| [5000 Compact - COMSON](cabinets/Compact-5000-COMSON.md) | B | **Two** | Looks like a Compact; holds a whole 1xx machine *and* an ND-5000 |

## Systems ([systems/](systems/))

Group-B machines, sold under their own model numbers. Each names its cabinet, its
32-bit CPU and its 1xx I/O processor.

**ND-500 line** — [ND-505](systems/ND-505.md) ·
[ND-510/CX](systems/ND-510-CX.md) · [ND-520](systems/ND-520.md) ·
[ND-530/CX](systems/ND-530-CX.md) · [ND-540](systems/ND-540.md) ·
[ND-550/CX](systems/ND-550-CX.md) · [ND-560/CX](systems/ND-560-CX.md) ·
[ND-570/CX](systems/ND-570-CX.md) · [ND-580/CX](systems/ND-580-CX.md)

**ND-5000 line** — [ND-5200](systems/ND-5200.md) ·
[ND-5400](systems/ND-5400.md) · [ND-5500](systems/ND-5500.md) ·
[ND-5700](systems/ND-5700.md) · [ND-5800](systems/ND-5800.md) ·
[ND-5900](systems/ND-5900.md)

**Rallar** — [ND-5830](systems/ND-5830.md) · [ND-5850](systems/ND-5850.md) ·
[ND-5950](systems/ND-5950.md)

## The early machines ([early/](early/))

CPU and machine are one and the same here.

**NORD-1 family** — [NORD-1](early/NORD-1.md) (1968) ·
[NORD-2B](early/NORD-2B.md) · [NORD-20](early/NORD-20.md) ·
[NORD-5](early/NORD-5.md) *(32-bit, needs a NORD-1)*

**NORD-10 family** — [NORD-10](early/NORD-10.md) (1973) ·
[NORD-10/S](early/NORD-10-S.md) · [NORD-12](early/NORD-12.md) ·
[NORD-42](early/NORD-42.md) ·
[NORD-50](early/NORD-50.md) *(32-bit, needs a NORD-10/S)*

Note the NORD-50's host is a **NORD-10/S**, not a 1xx - which is why it sits here
rather than with the group-B systems.

---

Also:
- **[Catalog of type and model numbers](TYPES.md)** - **read this if the numbers
  confuse you.** ND used five different numbering schemes at once; this decodes
  them and lists every system product number with what it means.
- **[The list of surviving machines](MACHINE-LIST.md)** - 266 individual machines by serial number,
  with names, owners and status, from ND's own service register. Two are confirmed
  saved; 41 confirmed scrapped; the rest unaccounted for.
- **[Machines we cannot confirm](UNCONFIRMED.md)** - names appearing in one source
  and nowhere else.

**Confidence marks**: **[P]** primary ND document held here, **[S]** secondary
source, **[?]** disputed or single-source.

*Images will be added later via Claude Design - see
[../images/CREDITS.md](../images/CREDITS.md) for what is licensed.*
