# 09 - Printing via a Terminal (and RetroTerm)

This document answers three related questions:

1. When a printer has file name `PRINTER` and sits "on a serial port," is that a
   special device or a reused **terminal** line? (Answer: a reused terminal
   line.)
2. How does printing "via a terminal" physically work?
3. How would you enable it in a terminal emulator such as **RetroTerm** (which
   speaks telnet to an ND-100 and emulates a Tandberg **TDV 2200**)?

Sources: `../../Operations/SINTRAN/ND-30.003.007 EN SINTRAN III System
Supervisor.md` (sections 2.4.5-2.4.7, 5.6.2, 6.2.1, appendix K.6, and the
terminal-type table); `../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md`.

Where an exact byte-level detail is needed but not present in these manuals, it
is marked **UNVERIFIED** with a note on where to get it. This document does not
invent escape sequences.

---

## 1. Two distinct ways a printer relates to a terminal line

There are two physically different arrangements, and they need different
handling in an emulator. Keep them separate:

### Arrangement A - printer on its OWN terminal line

The printer is wired to a **dedicated** async serial line. No terminal shares
it. SINTRAN treats that line's logical device as the `PRINTER` peripheral, and
the line's normal login/background process is turned off so nothing competes for
it. Everything SINTRAN sends down that line is the raw print stream.

### Arrangement B - printer hangs off a real terminal (auxiliary printer port)

The printer (e.g. an EPSON LX-80/RX-80) is plugged into the **auxiliary printer
port on the back of a Tandberg terminal**. The terminal and the printer share
one serial line to the host. The host sends data to the terminal; the terminal,
when told to, **passes the characters through to its printer port** instead of
(or as well as) showing them on screen. This is the classic "screen dump / local
print" arrangement.

System Supervisor appendix K.6 (Figure 63 "EPSON LX-80 printer connected to
Tandberg terminal") is exactly Arrangement B. Section 2.4.7 (EPSON RX/80) notes
the printer "may be connected to its own terminal line **or** to a Tandberg
terminal" - i.e. either arrangement.

---

## 2. Host-side (SINTRAN) setup - common to both arrangements

Whichever arrangement, the ND-100 side is configured the same way: bind a
spooling index to the line's logical device number, disable the login process on
that line, and create the peripheral/spooling files.

From the System Supervisor manual, the worked example "connecting a printer to a
terminal line" (terminal line 36):

```
@SINTRAN-SERVICE-PROGRAM

*SET-SPOOLING-DEVICE-NUMBER 1,36 Y Y Y      ; spooling index 1 -> logical device 36
*CHANGE-DATAFIELD 36 I Y Y Y                ; set the line's parameters
   TSPEED/ 000210 000210 000210 273d        ; set speed (this example = 1200 BPS)
```

Notes (verified):

- `*SET-SPOOLING-DEVICE-NUMBER <spooling index>,<logical device number> Y Y Y`
  binds the spooler to the line and (per the manual) means you do **not** also
  have to remove the background program - the three `Y` answers write the change
  to memory, image, and save-area so it survives cold start.
- If instead you set the printer up without `SET-SPOOLING-DEVICE-NUMBER`, you
  must disable the login process yourself:
  `*REMOVE-FROM-BACKGROUND-TABLE <logical device number> Y Y Y`. To give the
  line back to an ordinary terminal later, use `*INSERT-IN-BACKGROUND-TABLE`.
- Printers with a **special** interface (the DMA/parallel/serial line-printer
  types) must take the **lowest** spooling indexes, sorted ascending; printers
  on a terminal line follow after, in any order (manual note).
- Then create the peripheral file and spooling files on user SYSTEM (user RT
  must be friend of SYSTEM with access RWA), exactly as in
  [03-CONFIGURATION.md](03-CONFIGURATION.md).

After this, `PRINTER` (or whatever name you bind) is a normal spooled device:
`@START-SPOOLING PRINTER`, users `@COPY-FILE PRINTER,file`, etc. (docs 04/05).

---

## 3. Arrangement B extras - telling SINTRAN "EPSON shares a Tandberg line"

When the printer shares the line with a Tandberg terminal, SINTRAN must be told,
so it uses the right flow control. From appendix K.6:

```
*CHANGE-DATAFIELD <logical device number> I Y
   TINFO/  xxxxxx  20!        ; "Epson & terminal" - include this in LOAD-MODE
   DFLAG/  xxxxxx  1000J      ; XON/XOFF on input
```

And the **printer section of the Tandberg terminal's own communication menu**
must be set to match:

| Tandberg printer menu item | Value |
|----------------------------|-------|
| Printer handshake | XON/XOFF (use **DTR** if it is an RX-80) |
| Printer code format | 7even (7-bit, even parity) |
| Printer speed | 9600 |

(For a printer on its *own* line, appendix K also shows the line's own menu with
"Printer Handshake: Off - To be set if local printer is used", "Printer Code
Format: 7even", "Printer Speed: 9600".)

Two useful terminal-local features the manual documents for Arrangement B:

- **`CTRL + PRINT`** dumps the current screen picture to the printer.
- Setting **"Printer Mode" = "Log"** in the terminal's Function Switches menu
  makes the terminal log **all** input-from and output-to the terminal onto the
  printer (a hardcopy session log).

---

## 4. Which Tandberg terminal type to declare to SINTRAN

RetroTerm emulates a TDV 2200. On the ND side, tell SINTRAN what the terminal is
with `@SET-TERMINAL-TYPE <type>`. Relevant Tandberg codes from the System
Supervisor terminal-type table:

| Type code (decimal) | Terminal |
|---------------------|----------|
| 54 | Tandberg TDV2200-Standard |
| 53 | Tandberg TDV2200/9-ND-NOTIS |
| 55 | Tandberg TDV2200/9-ND-TET |
| 80 | Tandberg TDV2200/9-ND-NET |
| 83 | Tandberg TDV2200/9-V2-ND-NOTIS |
| 90 | Tandberg TDV2200/9S-ND-NET |
| 93 | Tandberg TDV2200/9S-ND-NOTIS |

Pick the plain **54 (TDV2200-Standard)** unless RetroTerm implements the
NOTIS/TET/NET extensions, in which case match the closest variant. The terminal
type governs how SINTRAN treats screen control, VDU flag, etc. (a per-terminal
`DFLAG` word carries options such as XON/XOFF-on-input, bit values documented in
the same manual).

---

## 5. Enabling terminal printing in RetroTerm - two implementation paths

RetroTerm is the **terminal** in this picture. How you support printing depends
on which arrangement you emulate.

### Path A (recommended, fully specified) - dedicated printer line

Emulate **Arrangement A**: dedicate one telnet connection / line as the
`PRINTER`.

- On the ND-100, configure that line's logical device as `PRINTER` with the
  section-2 commands and disable its background/login process.
- In RetroTerm (or a tiny companion socket client), open that line and treat
  **everything the host sends as the raw print stream** - do not interpret it as
  screen output. Write it straight to a file / spool it to the host OS printer /
  render to PDF. Honour the flow control you configured (XON/XOFF, or just read
  as fast as you can).
- The host drives it as an ordinary spooled printer: `@START-SPOOLING PRINTER`
  then `@COPY-FILE PRINTER,file`.

This path needs **no TDV 2200 escape-sequence knowledge** - it is just "capture
a byte stream from a serial line." It is the cleanest way to get real printing
out of an emulator, and every mechanism it relies on is verified above.

### Path B (faithful, but needs data you do not yet have) - aux printer port

Emulate **Arrangement B**: RetroTerm keeps acting as the TDV 2200 screen
terminal, and additionally implements the terminal's **auxiliary printer port**:

- Recognise the TDV 2200 control/escape sequences that turn the auxiliary
  ("transparent" / pass-through) printer mode on and off, and while it is on,
  route received characters to a printer sink instead of the screen.
- Implement `CTRL + PRINT` as a local "dump current screen buffer to the printer
  sink" action.
- Implement "Printer Mode = Log" as "copy all I/O to the printer sink."

**UNVERIFIED / MISSING DATA:** the exact TDV 2200 control sequences for
auxiliary-print on/off are **not** in the manuals in this repository. The System
Supervisor manual documents the *setup and behaviour* (handshake, code format,
CTRL+PRINT, Printer Mode = Log) but not the byte-level escape codes RetroTerm
would need to detect. To implement Path B faithfully you need one of:

- a **Tandberg TDV 2200 terminal / programmer's reference manual** (the
  auxiliary-print / transparent-print escape sequence), or
- a **capture** of a real ND-100 -> TDV 2200 session that uses CTRL+PRINT or an
  application "print" so you can see the sequence on the wire.

Until you have that, Path A is the reliable choice. (I can research the TDV 2200
control set if you want to pursue Path B - just say so.)

---

## 6. Summary

| Question | Answer |
|----------|--------|
| Is `PRINTER` a special device? | No - it is a printer on an **ordinary async terminal line** (Arrangement A) or on a **terminal's auxiliary printer port** (Arrangement B). It reuses normal terminal hardware. |
| How is the line prepared? | `*SET-SPOOLING-DEVICE-NUMBER`, disable background/login (`*REMOVE-FROM-BACKGROUND-TABLE`), set line speed with `*CHANGE-DATAFIELD`, create peripheral + spooling files. |
| Shared with a Tandberg? | Tell SINTRAN via `*CHANGE-DATAFIELD ... TINFO 20! / DFLAG 1000` and set the Tandberg printer menu (XON/XOFF or DTR, 7even, 9600). CTRL+PRINT dumps the screen; Printer Mode = Log logs the session. |
| Terminal type to declare | `@SET-TERMINAL-TYPE 54` (TDV2200-Standard) or the matching /9 variant. |
| RetroTerm - easy path | Dedicate a line as `PRINTER`, capture the raw stream to a file/PDF (Path A). No escape-sequence knowledge needed. |
| RetroTerm - faithful path | Implement the TDV 2200 auxiliary printer port + CTRL+PRINT + Printer-Mode-Log (Path B). Needs the TDV 2200 escape sequences, which are **not** in these manuals. |

---

## 7. Related

- [02-HARDWARE-DEVICES.md](02-HARDWARE-DEVICES.md) - the printer hardware
  landscape.
- [03-CONFIGURATION.md](03-CONFIGURATION.md) - peripheral/spooling file setup.
- [06-REMOTE-AND-NETWORK-PRINTING.md](06-REMOTE-AND-NETWORK-PRINTING.md) - if the
  printer is on another computer instead of a local line.
