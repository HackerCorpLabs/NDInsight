# ND-100 Bus Signals

Documentation of the physical bus signals for the ND-100/ND-110/ND-120 shared backplane.

## Contents

- [ND-100-BUS-C-CONNECTOR.md](ND-100-BUS-C-CONNECTOR.md) - Complete C connector (ND-BUS) signal reference
  - DIN 41612 connector pinout (3 rows x 32 pins, 96 pins total)
  - All 60 logic signals with pin assignments, direction, and descriptions
  - ND-120 3202D CPU card IC traces (driver and receiver ICs for every signal)
  - Bus cycle protocols (IOX/IOXT, IDENT PLxx, DMA read/write)
  - Daisy-chain mechanisms (GRANT, IDENT, CONTROL)
  - 5V to 3.3V level shifting analysis and timing budgets
  - Controller card design guidelines

- [CONTROLLER-DESIGN.md](CONTROLLER-DESIGN.md) - Multi-device controller card design (Olimex RP2350-PICO2-BB48R)
  - Architecture, pin allocation, PIO state machines, power, backplane design
  - All design reasoning and trade-offs
  - Device emulation roadmap (floppy, SMD, terminal, HDLC)

- [SCHEMATIC-CAPTURE.md](SCHEMATIC-CAPTURE.md) - Single-page schematic capture quick reference
  - Reference designator allocation (U1-U11, J1-J6, etc.)
  - Net naming convention (`_BUS` / `_3V3` / `_IN` / `_OUT`)
  - Per-IC pin connection tables for every chip
  - Olimex BB48R EXT1/EXT2 header pin map
  - DIN 41612 96-pin connector net assignments
  - 2-layer PCB stackup and JLCPCB order spec
  - Verified LCSC part numbers
  - KiCad library setup commands
  - Suggested schematic capture order

- [Olimex-rp2350/](Olimex-rp2350/) - Olimex RP2350-PICO2-BB48R datasheets and pinout reference

- [Backplane-Kicad/](Backplane-Kicad/README.md) - Third-party backplane KiCad reference design
  (RetroBrew Computers "ECB Backplane-4" project, zip + extracted; same DIN 41612 connector
  family as the ND-100 bus)

- [ND-Compact-Backwiring/](ND-Compact-Backwiring/README.md) - Photos of an ND Compact backplane
  board (ND print 324409), connector side and solder/backwiring side (PL1..PL12)

## Source Data

Signal data extracted from: `ND-120 Commands.xlsx` (sheets: C-BUS, C-PLUG (ND-BUS), Signal Names)

## Scope

This documentation covers the **C connector** which carries the standard shared bus between CPU, memory, and I/O controllers. The A and B connectors are not documented here as they carry machine-specific signals.
