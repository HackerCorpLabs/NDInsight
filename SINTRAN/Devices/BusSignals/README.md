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

## Source Data

Signal data extracted from: `ND-120 Commands.xlsx` (sheets: C-BUS, C-PLUG (ND-BUS), Signal Names)

## Scope

This documentation covers the **C connector** which carries the standard shared bus between CPU, memory, and I/O controllers. The A and B connectors are not documented here as they carry machine-specific signals.
