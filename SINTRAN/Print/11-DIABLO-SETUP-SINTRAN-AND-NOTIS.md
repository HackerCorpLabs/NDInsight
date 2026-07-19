# 11 - Setting Up the Diablo Printer in SINTRAN and NOTIS

How to configure SINTRAN III and NOTIS-WP/TF so a user can **generate a document
and print it** to the (emulated) **ND-232 Diablo 1650** letter-quality printer,
which the RetroCore emulator captures and turns into a PDF - transparently to the
guest.

This is the operator/user companion to the emulator design and build plan:
- Design + printer choice: [10-EMULATED-PDF-PRINTER-DESIGN.md](10-EMULATED-PDF-PRINTER-DESIGN.md)
- Emulator build plan: RetroCore `Emulated.HW\ND\CPU\NDBUS\DIABLO-1650-CONTROLLER-IMPLEMENTATION-PLAN.md`
- General printer setup + serial/terminal-line printing:
  [03-CONFIGURATION.md](03-CONFIGURATION.md),
  [09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md](09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md)

Marking: **[VERIFIED]** = from an ND manual (cited); **[MATCH]** = a value that
must be the same on both the emulator and SINTRAN; **[VERIFY]** = confirm against
your SINGEN config / a trace before relying on it.

The Diablo 1650 (ND-232) is an **RS-232 serial** printer ("word processor
quality", proportional spacing, plotting, backward printing;
`mirror-sintran-com/mirror/library/libpdpi/ND-232-B1-EN.pdf`) [VERIFIED]. So it is
set up as a **printer on a serial (async) line**, spooled - the arrangement the
System Supervisor manual calls "connecting a printer to a terminal line."

---

## 0. The one value that must match: the device number

The emulator attaches `NDBusSerialPrinter` at an async serial device-number block
(a spare terminal / async-modem slot). SINTRAN must be told the printer is on the
**same** logical device number. Call it `<LDN>` below (octal). Pick the block in
the emulator (the device ctor thumbwheel) and use the matching SINTRAN logical
device number here. [MATCH] [VERIFY the free block against your SINGEN config]

Everything else follows from `<LDN>`.

---

## 1. SINTRAN setup (as user SYSTEM)

Two layers: bind the printer to a peripheral/spooling file and start the spooler.
Commands with `*` are entered inside `@SINTRAN-SERVICE-PROGRAM`; `@` commands are
normal SINTRAN mode.

### 1.1 Tell SINTRAN the line carries a printer, not a terminal

Because the Diablo sits on an async line, disable the login/background process on
that line and bind the spooler to it (System Supervisor manual, "connecting a
printer to a terminal line") [VERIFIED]:

```
@SINTRAN-SERVICE-PROGRAM

*SET-SPOOLING-DEVICE-NUMBER 1,<LDN> Y Y Y     ; spooling index 1 -> logical device <LDN>
                                              ; (this also spares the *REMOVE-FROM-BACKGROUND-TABLE step)

*CHANGE-DATAFIELD <LDN> I Y Y Y               ; set the line parameters
   TSPEED/ ...                                ; set the line speed (see 1.2)
*EXIT
```

- `<LDN>` is the octal logical device number from section 0.
- The three `Y` answers write the change to **memory / image / save-area** so it
  survives a cold start.
- If you do not use `SET-SPOOLING-DEVICE-NUMBER`, disable the line's login
  yourself with `*REMOVE-FROM-BACKGROUND-TABLE <LDN> Y Y Y` (undo later with
  `*INSERT-IN-BACKGROUND-TABLE`) [VERIFIED].

### 1.2 Line speed

Set `TSPEED` to a value the Diablo/line agree on. The ND-232 Diablo 1650 supports
110/300/1200 baud, optional to 9600 [VERIFIED]; **1200 baud** is the manual's
worked-example value for a serial printer on a terminal line [VERIFIED]. The
emulated printer is always-ready (no real baud limit), so the value only has to
be internally consistent - use 1200 (or 9600) and match it if the emulator models
a baud. [MATCH if the emulator enforces baud; otherwise cosmetic]

### 1.3 Create the peripheral file and spooling files

Create the peripheral file bound to `<LDN>` and a set of spooling files so the
printer is spooled (multiple users, background printing). As user SYSTEM
([03-CONFIGURATION.md](03-CONFIGURATION.md)) [VERIFIED]:

```
@SET-PERIPHERAL-FILE "DIABLO",<LDN>       ; the SINTRAN peripheral file "DIABLO" -> device <LDN>
@CREATE-FILE DIABLO;10,0                  ; 1 peripheral + 9 spooling files (versions)
```

Remember the whole new-file spec is quoted, including any `(SYSTEM)` prefix, e.g.
`"(SYSTEM)DIABLO"` [VERIFIED - project rule]. User RT must be friend of SYSTEM
with access RWA to the spooling files [VERIFIED, System Supervisor].

### 1.4 Start spooling

```
@START-SPOOLING DIABLO
```

Now SINTRAN reserves the printer and prints everything queued to `DIABLO` until
`@STOP-SPOOLING DIABLO` [VERIFIED]. Users get multiple copies because it is
spooled.

> Alternative (simplest, no serial setup): use the **existing parallel line
> printer** (`LINE-PRINTER`, device 0430) that the emulator already provides -
> `@SET-PERIPHERAL-FILE "LINE-PRINTER",5` then `@START-SPOOLING LINE-PRINTER`.
> That is emulator Phase 0 and yields a PDF immediately, but plain (draft) text -
> no Diablo letter-quality codes. Use the Diablo path (above) for quality.

---

## 2. NOTIS setup (so NOTIS emits Diablo codes)

This is the crucial step for **quality**: NOTIS must print through its **Diablo**
printer definition (which emits the Diablo control codes the emulator decodes
into a letter-quality PDF), not the plain `LINE-PRINTER` definition (plain text +
overprint).

### 2.1 Add the printer to `WP-PRINTERS:TEXT`

The NOTIS printer-definition file `WP-PRINTERS:TEXT` (edited during `INSTALL-WP`)
maps a **logical printer name** to the SINTRAN file name, feed type, default
start, and pitch/nationality [VERIFIED, `10079K_NOTIS-WP.md`]. Add a Diablo entry
whose **SINTRAN file name matches the peripheral file from section 1.3** ("DIABLO"):

```
; Logical name | SINTRAN file | Feed | Default start | Font/Nationality
  DIABLO        | DIABLO       | S    | 12            | 8.0-0.2
```

- **Logical name** = what the NOTIS user types at `DEVICE NAME:` and picks as the
  default printer. It must map to a real Diablo definition so NOTIS emits Diablo
  codes. NOTIS-WP lists **Diablo 1610/1650** as ND-supported and (K release) "The
  Diablo 630 printer will now be handled properly" [VERIFIED] - so the Diablo
  definition ships with NOTIS.
- **Feed** = S (single sheet) or T (tractor) to taste; the ND-232 supports both
  (with the ND-233 cut-sheet feeder) [VERIFIED].
- **Font/Nationality** encodes pitch and national set (0 US, 2 France, 3 UK, 4
  Denmark/Norway, 5 Sweden/Finland, 6 Italy) [VERIFIED]. Pick the pitch your
  Diablo wheel uses (10/12/15 cpi); `8.0-0.2` follows the sample daisy-wheel
  entries.
- Remove sample `WP-PRINTERS` entries that do not apply to your system
  [VERIFIED].

> [VERIFY] The internal per-printer control-code detail inside `WP-PRINTERS` (the
> exact Diablo escape bytes NOTIS emits) is not reproduced in the repo manuals -
> it lives in the shipped Diablo definition. Selecting the Diablo logical name is
> what makes NOTIS use it. A capture of the real NOTIS-to-Diablo stream is the
> final authority (and is what tunes the emulator's decoder - see the build plan
> Phase 5).

### 2.2 Make it the default printer (optional)

In NOTIS-WP, **Menu no. 4** (page-layout / default printer) sets the default
printer so the user can just press Enter at `DEVICE NAME:` [VERIFIED].

---

## 3. Generate and print a document

### 3.1 Ordinary document (NOTIS-WP prints it directly)

```
@NOTIS-WP-ENG                 ; start the editor (terminal type 53)

; type the document in the WORK AREA, e.g.
^H1=My Report;                ; a running header
^JM;C;                        ; centre the title line(s)
Quarterly Report
^JM;S;                        ; stretch-justify body (smooth right margin)
This document prints on the Diablo 1650 in letter quality.
^BT=+;bold words^BT=-;        ; bold via the Diablo shadow/double-strike
normal text again.

; store it (HOME command W)
W "MY-REPORT"

; print it (PRINT key, or HOME command O)
DEVICE NAME: DIABLO           ; or Enter for the default printer (Menu 4)
AREA:                         ; Enter = whole document
NO. OF COPIES:1               ; more than 1 works because DIABLO is spooled
PAGE LIST:                    ; Enter = all pages
FORMAT? Y                     ; Y = apply the ^ directives + menus (REQUIRED for quality)
```

`FORMAT? Y` is essential: it applies the print directives and the menus, so
NOTIS emits the formatted Diablo stream (pitch, bold, underline, justification)
rather than raw characters [VERIFIED].

### 3.2 High-quality paginated document (via NOTIS-TF)

For chapters / table of contents / index / running headers, format with NOTIS-TF
first, proof it, then print [VERIFIED]:

```
; in NOTIS-WP, press SHIFT+PRINT (or HOME command J):
ACTIVATE NOTIS-TF-ENG-x <MY-REPORT>:TEXT     ; TF formats :TEXT -> :OUT (A4 geometry)
; TF drops you into INSPECT mode to proof the :OUT on screen
; then print the :OUT with the C command (or PRINT key):
DEVICE NAME: DIABLO
SHEET LIST:                    ; Enter = all sheets
NO. OF COPIES:1
```

NOTIS-TF's default page geometry is tuned for **A4** at 12 cpi (width 100,
borders 18/12) [VERIFIED] - which is exactly what the emulator renders to an A4
PDF.

### 3.3 What happens next (emulator side, transparent)

SINTRAN spools the file and drives the Diablo over the serial line; the emulated
`NDBusSerialPrinter` accepts the bytes, the Diablo decoder reconstructs the
positioned page (pitch, bold, underline, super/subscript from the Diablo codes),
and on job completion writes an **A4 PDF**. The guest sees a normal, fast printer;
the PDF appears on the host (PrinterWindow "Save as PDF", the CLI print command,
or auto-per-job once build Phase 4 is in). None of this is visible to SINTRAN or
NOTIS.

---

## 4. Verify it works

1. `@LIST-SPOOLING-QUEUE DIABLO` - your file should appear (then clear as it
   prints) [VERIFIED].
2. On the host, the PrinterWindow / CLI shows a `DIABLO` printer surface with the
   printed lines; "Save as PDF" (or the auto-saved file) opens as a clean A4 PDF.
3. If nothing prints:
   - Wrong/blocked device: confirm `<LDN>` matches the emulator [MATCH]; check the
     line's background process is off (section 1.1).
   - Plain text instead of letter quality: NOTIS is using `LINE-PRINTER`, not the
     Diablo definition - check the `DEVICE NAME:` / Menu 4 default (section 2) and
     that `FORMAT? Y` was answered.
   - Error 245B "No more unused spooling files" - create more `DIABLO` versions
     (section 1.3) [VERIFIED].

---

## 5. Quick reference

| Step | Command |
|------|---------|
| Bind spooler to the line | `*SET-SPOOLING-DEVICE-NUMBER 1,<LDN> Y Y Y` |
| Set line speed | `*CHANGE-DATAFIELD <LDN> I Y Y Y` -> `TSPEED/ ...` (1200/9600) |
| Create peripheral + spooling files | `@SET-PERIPHERAL-FILE "DIABLO",<LDN>` ; `@CREATE-FILE DIABLO;10,0` |
| Start / stop spooling | `@START-SPOOLING DIABLO` / `@STOP-SPOOLING DIABLO` |
| NOTIS printer definition | add `DIABLO` line to `WP-PRINTERS:TEXT` (SINTRAN file = `DIABLO`) |
| Print from WP | PRINT key -> `DEVICE NAME: DIABLO`, `FORMAT? Y` |
| Format+print via TF | SHIFT+PRINT -> proof in INSPECT -> `C` -> `DEVICE NAME: DIABLO` |
| Check queue | `@LIST-SPOOLING-QUEUE DIABLO` |

**Primary sources:** System Supervisor `../../Operations/SINTRAN/ND-30.003.007 EN
SINTRAN III System Supervisor.md`; NOTIS-WP Editor `../../Reference-Manuals/Notis/
ND-63.002.02 NOTIS-WP Reference Manual - Editor.md`; `10079K_NOTIS-WP.md`;
NOTIS-TF `../../Reference-Manuals/Notis/ND-63.007.01 NOTIS-TF Text Formatter
Reference Manual.md`; ND-232 sheet `mirror-sintran-com/mirror/library/libpdpi/
ND-232-B1-EN.pdf`.
