# 02 - Hardware Devices: What Actually Does the Printing

This document answers the direct hardware question: is a NORD printer driven by
a serial port (a terminal controller), by specialised parallel hardware, or
over a network? The answer is "all three exist," and this document describes
each.

Sources:
`../../Reference-Manuals/ND-06.016.01_NORD-100_Input_Output_System.md`
(device numbers, the line-printer interface, Appendix A device-number table,
Appendix B line-printer programming specification);
`../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md` Appendix D "Standard
Peripheral File Names."

---

## 1. How any device attaches to the NORD-100 bus

Every I/O controller on the NORD-100 bus is addressed by a **hardware device
number (dev. no.)** - the start address of that controller's block of
registers. For Norsk Data controllers the device number is **standardised** and
listed in a table (I/O System manual, Appendix A). The device number on a
module is set with a **thumbwheel**, which selects a location in a device-number
PROM, so identical modules can be given different device numbers to cover a
whole class of peripherals (I/O System 1.3.4.3).

A program (or the SINTRAN driver) reaches a specific register on a controller
with the **IOX / IOXT** instruction, whose operand is
`device number + register number`. There is always a unique correspondence
between a peripheral, the controller, and the device number (I/O System 1.3.4).

So regardless of printer type, SINTRAN's driver ultimately does IOX/IOXT to a
device number. What differs is the **kind** of controller behind that device
number.

---

## 2. Line printer (`LINE-PRINTER`) - THREE possible interface types

The classic high-speed line printer sits at device number **430 octal** (line
printer 1; 434 octal for line printer 2), logical device number **5** (15 octal
for the second). But "line printer" is not one hardware type: the SINTRAN
`LINE-PRINTER` driver supports **three different physical interfaces**, chosen at
system generation by the "Define printer type" parameter (System Supervisor
manual, and byte-verified in
`../Devices/LINE-PRINTER-CONFIG-INSPECTION.md`):

| Type (LPSELECTION) | Name | Interface | NPL driver | Typical printer |
|--------------------|------|-----------|------------|-----------------|
| 0 | Do not use | (disabled) | - | - |
| 1 | DMA | DMA controller | DMPR | Fujitsu |
| 2 | Parallel | Parallel PIO controller | DMLP | CDC / DP (e.g. CDC 9380) |
| 3 | Serial | Serial (async) line | DLPR | Serial line printers |

So the answer to "is a line printer parallel or serial?" is: **it can be
either** (or DMA). The most common classic unit, the CDC 9380, is **type 2
(parallel PIO)**; but SINTRAN can equally drive a **serial** line printer as
type 3 over an async line.

### 2.1 The parallel (CDC) interface

For the parallel type, the I/O System manual gives the register model:

- The line-printer interface "has only one channel - the output channel"
  (Example 1). It is **output-only**.
- The **data register is register number 1** (so `IOX 431` writes a character to
  line printer 1); the **status register is 2** and the **control register is
  3**. You activate the print by writing a control word (I/O System Example 1).
- The full CDC 9380 register/bit specification (control, status, data, the legal
  control codes and paper/error/band-detect status bits) is documented in
  [08-CDC-9380-LINE-PRINTER-INTERFACE.md](08-CDC-9380-LINE-PRINTER-INTERFACE.md).

### 2.2 The boot-time hardware test

SINTRAN does not auto-discover the printer type; it is fixed in the config table
(LPTA) at generation. At boot the driver only **verifies** a device responds at
the expected IOX address (type 2 uses `EXR ST`, types 1/3 use `IOXT`). If
`LPSELECTION = 0` the printer is skipped and `@COPY-FILE LINE-PRINTER,...` fails
with error 33 "NO SUCH LOGICAL UNIT". This whole path is byte-verified in
`../Devices/LINE-PRINTER-CONFIG-INSPECTION.md`.

**In short:** `LINE-PRINTER` is specialised printer hardware at device number
430 octal, but the *interface* behind it is a build-time choice of DMA, parallel
PIO, or serial line.

---

## 3. Matrix / character printer (`PRINTER`) - a printer on a terminal line

Smaller printers - matrix / character printers such as the EPSON RX-80 and
LX-80 - are **not** special printer hardware at all. They are attached to an
**ordinary asynchronous serial terminal line** (the same 4/8 async serial
interface, current-loop or V.24/RS-232, used for terminals) and given the
peripheral file name **`PRINTER`**. This is the direct answer to "are these
special devices or reusing normal terminal devices?": **they reuse a normal
terminal line.**

Verified facts:

- The standard peripheral file name for a matrix printer is **`PRINTER`**
  (Commands Reference, Appendix D), and Appendix D explicitly notes that
  **terminals "can also be PRINTER."**
- The System Supervisor manual (ND-30.003.007) documents exactly this: "If the
  printer is physically connected to a terminal line, the background program for
  this terminal must be disabled from being started" - done with the Service
  Program command `*REMOVE-FROM-BACKGROUND-TABLE <logical device number>`. The
  same manual gives a worked example of "connecting a printer to a terminal
  line" (terminal line 36) using `*SET-SPOOLING-DEVICE-NUMBER 1,36 ...` and
  `*CHANGE-DATAFIELD 36 ...` to set the line speed.
- The "Define printer type" table (types 1 DMA / 2 parallel / 3 serial) is only
  for line printers with a **special** interface. Printers "using a terminal
  line" do not need a printer-type entry - the note in the manual is explicit
  that special-interface printers take the lowest spooling indexes and
  "printers using a terminal line can follow afterwards."

So a `PRINTER` (matrix printer) is a serial device on a terminal port that has
had its login/background process turned off so the line can be used purely for
output. The full setup, plus how this relates to a terminal emulator such as
RetroTerm, is in
[09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md](09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md).

> **Note on the device-number table.** The SINTRAN "standard peripheral file
> names" table lists `PRINTER` at 414/415/417, but the I/O System hardware
> device-number table (Appendix A) lists address block **414-417 octal as
> "Paper Tape Punch 2"** (logical device 13 octal, interrupt level 10). These
> are the same address block reused for a different output device class - which
> is exactly what the thumbwheel-selectable device-number PROM allows (I/O
> System 1.3.4.3). Because a matrix printer is normally put on a *terminal* line
> in practice (as the System Supervisor examples show), treat the 414/415/417
> numbers as the nominal "character output device" slot rather than a fixed
> requirement.

---

## 4. Versatec printer/plotter - its own controller

The Versatec electrostatic printer/plotter has its **own controller** at device
numbers **603, 604, 605, 606 octal** (Commands Reference, Appendix D). Its
peripheral file naming depends on what else is installed:

- If it is the only line-printer-class device, it uses `LINE-PRINTER-1`,
  `LINE-PRINTER-2`, ...
- If another line printer already exists, it uses `VERSATEC-1`,
  `VERSATEC-2`, ...

It is a distinct hardware class from both the parallel line printer and the
serial matrix printer.

---

## 5. Network / remote printers - no local hardware at all

A printer can also live on **another computer**. In that case there is no local
printer controller; SINTRAN sends the print data over a communications link:

- To another **NORD** computer, via **NORDNET / COSMOS** remote peripheral
  files (you address a printer on a remote host by naming the remote system).
- To a **non-NORD mainframe** (IBM, CDC, UNIVAC, Honeywell, Siemens), via a
  **Remote Job Entry (RJE)** emulator.

This is covered fully in
[06-REMOTE-AND-NETWORK-PRINTING.md](06-REMOTE-AND-NETWORK-PRINTING.md). The
important point for this hardware document: the transport is the communications
controller (HDLC / synchronous line / X.25), **not** a printer controller.

---

## 6. Summary table - the printing paths

| Path | Peripheral file name(s) | Hardware | Device number (octal) | Notes |
|------|------------------------|----------|-----------------------|-------|
| Line printer (DMA) | `LINE-PRINTER` | DMA controller (type 1, DMPR) | 430, 434 | Fujitsu. |
| Line printer (parallel) | `LINE-PRINTER` | Parallel PIO output-only controller (type 2, DMLP) | 430, 434 | e.g. CDC 9380. Data reg = 1. See doc 08. |
| Line printer (serial) | `LINE-PRINTER` | Serial async line (type 3, DLPR) | 430, 434 | Serial line printers. |
| Matrix / character printer | `PRINTER` | Ordinary async terminal line (login disabled) | on a terminal line (300-block, etc.) | EPSON RX-80/LX-80. Reuses a terminal port. See doc 09. |
| Versatec printer/plotter | `LINE-PRINTER-n` or `VERSATEC-n` | Versatec controller | 600-607 | Naming depends on other installed printers. |
| Remote / network printer | local logical-printer name | Comms controller (HDLC / X.25) via XMSG | n/a (network) | COSMOS Spooling (C-S-S) for NORD hosts; RJE for mainframes. See doc 06. |

## 7. Is there laser-printer support?

**Not in the general SINTRAN spooling system, as far as the manuals in this
repository show.** Searching the reference manuals, "laser" appears only in two
unrelated contexts:

- The **LD 1200 "Laser Drive"** - a WORM optical *disk* drive (mass storage),
  not a printer (DOMINO SCSI Operator Guide).
- The **NORTEXT-100 / ND-COMTEC phototypesetting** product, whose supported
  output devices include a "Lasercomp" phototypesetter among others (APS, MCS
  8400, CG8600, Linotron 202, Agfa P400, Digiset 720, Metroset) - this is a
  specialised professional typesetting subsystem, **separate** from the
  ordinary spooling/printer path described here (Documentation Catalogue).

So classic SINTRAN print spooling targets line printers, matrix/character
printers, and plotters. Laser output existed only via the dedicated NORTEXT
phototypesetting product line, not as a spooled `LINE-PRINTER`/`PRINTER` device.

**UNVERIFIED:** whether later ND products added a spooled laser printer driver
is not established by the manuals reviewed here. If you have a specific later
release in mind, that would need its own release documentation to confirm.

---

## 8. How SINTRAN reaches the hardware

For a local device, the SINTRAN driver ultimately issues **IOX/IOXT** to the
device number bound to the peripheral file. The binding from file name to
logical device number, and from logical device number to hardware, is set up by
the configuration commands in
[03-CONFIGURATION.md](03-CONFIGURATION.md).

The register-level driver behaviour (the exact IOX sequences the line-printer
driver performs) is a SINTRAN-source / carving topic and is out of scope for
this hardware overview; see
[07-INTERNALS-AND-MON-CALLS.md](07-INTERNALS-AND-MON-CALLS.md) for pointers.
