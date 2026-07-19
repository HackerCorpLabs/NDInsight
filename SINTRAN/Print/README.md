# SINTRAN III Printing and Spooling

This folder documents how printing and output spooling work on Norsk Data
NORD / ND-100 systems running SINTRAN III: what a "printer" actually is to the
operating system, how print output is queued (spooled) to disk and later
emptied onto the physical device, how you configure which printers exist, what
the hardware devices are, and how printing works over the network to other
computers.

The material is split into focused documents by category rather than one large
file. Start with the Overview, then read the document that matches your
question.

---

## The 30-second answer

- SINTRAN talks to a printer through a **peripheral file** - an ordinary
  file-system name (for example `LINE-PRINTER`) that is tied to a **logical
  device number**, which in turn maps to a **hardware device number** on the
  NORD-100 I/O bus.
- The **hardware** doing the actual printing is one of:
  1. a **line printer** (`LINE-PRINTER`, device 430 octal) - whose interface is
     a build-time choice of **DMA** (Fujitsu), **parallel PIO** (CDC 9380), or
     **serial** line;
  2. a **matrix / character printer** (`PRINTER`, e.g. EPSON RX-80/LX-80) - which
     is **not special hardware**: it hangs off an **ordinary async terminal
     line** (its own line, or a real terminal's auxiliary printer port);
  3. a **Versatec printer/plotter** controller; or
  4. a **remote printer** on another computer, reached over the network
     (**COSMOS Spooling** over XMSG for NORD hosts, or an **RJE** emulator for
     IBM / CDC / UNIVAC / Honeywell / Siemens mainframes).
- There is **no laser-printer** driver in classic SINTRAN spooling; "laser" in
  the manuals means an optical *disk* drive or the separate NORTEXT
  phototypesetting product (doc 02 section 7).
- **Spooling** means the print data is first copied to a disk file (a
  **spooling file**), queued, and printed later by a background **spooling
  program**, so the user program does not have to wait for the slow printer.

Full detail, with citations, is in the documents below.

---

## Documents in this folder

| # | Document | What it answers |
|---|----------|-----------------|
| - | [README.md](README.md) | This index. |
| 01 | [01-OVERVIEW-AND-CONCEPTS.md](01-OVERVIEW-AND-CONCEPTS.md) | What spooling is, the peripheral-file / spooling-file / queue model, and the life of a print job. Read this first. |
| 02 | [02-HARDWARE-DEVICES.md](02-HARDWARE-DEVICES.md) | The physical devices: line-printer (parallel) controllers, matrix/serial printers on terminal ports, Versatec, device numbers, and the IOX register interface. |
| 03 | [03-CONFIGURATION.md](03-CONFIGURATION.md) | How you tell SINTRAN which printers exist: peripheral files, versions, `SET-PERIPHERAL-FILE`, `SET-SPOOLING-DEVICE-NUMBER`, and system generation. |
| 04 | [04-OPERATOR-COMMANDS.md](04-OPERATOR-COMMANDS.md) | Running the spooler (user SYSTEM): start/stop spooling, spooling conditions, forms, headers, page pool, and live print control. |
| 05 | [05-USER-COMMANDS.md](05-USER-COMMANDS.md) | Everyday user commands: sending a file to a printer, appending to the queue, listing/editing/removing queue entries. |
| 06 | [06-REMOTE-AND-NETWORK-PRINTING.md](06-REMOTE-AND-NETWORK-PRINTING.md) | Printing to other machines: NORDNET / COSMOS remote peripheral files and Remote Job Entry (RJE) to non-NORD hosts. |
| 07 | [07-INTERNALS-AND-MON-CALLS.md](07-INTERNALS-AND-MON-CALLS.md) | The programmer/kernel view: the monitor calls behind spooling and the SINTRAN segments that implement it. |
| 07a | [07a-CARVED-INTERNALS-FINDINGS.md](07a-CARVED-INTERNALS-FINDINGS.md) | Byte-verified carving results for the spooling internals (segments, MON 240/40, queue structure). Written by the carving pass. |
| 08 | [08-CDC-9380-LINE-PRINTER-INTERFACE.md](08-CDC-9380-LINE-PRINTER-INTERFACE.md) | Register-level reference for the parallel line printer (device 430, CDC 9380): control/status/data bits, control codes, driver loop. |
| 09 | [09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md](09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md) | Printing on a terminal line: `PRINTER` setup, EPSON-on-Tandberg, TDV 2200 terminal types, and how to enable it in a RetroTerm-style telnet emulator. |
| 10 | [10-EMULATED-PDF-PRINTER-DESIGN.md](10-EMULATED-PDF-PRINTER-DESIGN.md) | User guide (set up printers, supported models, edit + print a NOTIS-WP document) PLUS the design + ranked printer research for an emulated printer controller that drains jobs instantly and renders each completed job to a high-quality A4 PDF, invisibly to SINTRAN/NOTIS. Recommends the Diablo 1650 (ND-232). |
| 11 | [11-DIABLO-SETUP-SINTRAN-AND-NOTIS.md](11-DIABLO-SETUP-SINTRAN-AND-NOTIS.md) | Step-by-step operator/user setup: configure the Diablo serial printer in SINTRAN (peripheral + spooling files, serial line, start spooling), add it to NOTIS `WP-PRINTERS`, and generate + print a document (WP direct, or via NOTIS-TF) so it lands as a PDF. |

---

## Primary sources

All documents cite the manuals in `../../Reference-Manuals/`. The main ones are:

- `ND-60.050.06 SINTRAN III Users Guide.md` - section 3.8 "The Spooling
  System" (the authoritative conceptual description).
- `ND-60.128.5 EN SINTRAN III Reference Manual.md` - per-command reference.
- `Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md` - consolidated command
  reference, standard peripheral-file names (Appendix D), MON-call mappings.
- `ND-06.016.01_NORD-100_Input_Output_System.md` - the hardware I/O system,
  device numbers, and the line-printer interface (Appendix B, CDC 9380).
- `ND-60.134.2 EN SINTRAN III Communication Guide.md` - chapter 3, Remote Job
  Entry (RJE) and NORDNET remote batch.
- `../../Operations/SINTRAN/ND-30.003.007 EN SINTRAN III System Supervisor.md` -
  operator setup of printers/spooling, printers on terminal lines, the EPSON on a
  Tandberg TDV 2200, COSMOS Spooling (C-S-S), and the terminal-type table.
- `../Devices/LINE-PRINTER-CONFIG-INSPECTION.md` - byte-verified line-printer
  boot/config path, printer types (DMA/parallel/serial), and the error-33 fix.

---

## Conventions used in these documents

- Numbers written like `430` with the word "octal" (or the SINTRAN habit of a
  trailing context) are **octal** unless stated as decimal; this matches ND
  documentation. Where ambiguity is possible the base is stated explicitly.
- Memory sizes follow the project convention: **words** as the primary unit
  with bytes in parentheses.
- Anything not directly verifiable in the manuals or SINTRAN source is marked
  **ASSUMPTION:** or **UNVERIFIED:**. Facts are cited to a specific manual.

**Last updated:** 2026-07-18
