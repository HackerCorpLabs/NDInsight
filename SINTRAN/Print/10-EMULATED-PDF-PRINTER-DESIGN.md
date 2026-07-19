# 10 - Printer Setup, NOTIS Printing, and an Emulated Print-to-PDF Controller

This document has two halves:

- **Part 1 (user guide, verified):** how to set up the different printers, what
  printers are supported, and how to edit a small NOTIS-WP document and print it.
- **Part 2 (engineering design):** how to build an **emulated printer** in the
  RetroCore ND-100 emulator that looks like real hardware to SINTRAN and NOTIS,
  drains print jobs at full speed, and turns each completed job into a
  high-quality **PDF** - with the SINTRAN machine none the wiser.

Convention: **[VERIFIED]** = taken from an ND manual or a byte-verified carve in
this repo (cited). **[DESIGN]** = engineering proposal. **[VERIFY]** = a fact
from general knowledge that must be confirmed against a datasheet before you rely
on it. ASCII only; octal is written like `430` (octal) or `14o`, hex like `0x0C`.

Related docs: line-printer register spec
[08-CDC-9380-LINE-PRINTER-INTERFACE.md](08-CDC-9380-LINE-PRINTER-INTERFACE.md);
carved spooler internals
[07a-CARVED-INTERNALS-FINDINGS.md](07a-CARVED-INTERNALS-FINDINGS.md); terminal
aux-print [09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md](09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md);
NOTIS printing `../../Reference-Manuals/Notis/NOTIS-PRINTING-AND-A4-PAGES.md`.

---

# PART 1 - USER GUIDE

## 1. What printers are supported [VERIFIED]

Two layers support printers: **SINTRAN** (the OS driver + spooler) and **NOTIS**
(its own printer-definition table).

SINTRAN line-printer interface types (byte-verified in
`../Devices/LINE-PRINTER-CONFIG-INSPECTION.md`): **1 = DMA (Fujitsu)**,
**2 = Parallel (CDC/DP, e.g. CDC 9380 at device 430)**, **3 = Serial**. Plus
matrix/character printers on a terminal line (`PRINTER`) and the Versatec
plotter. See [02-HARDWARE-DEVICES.md](02-HARDWARE-DEVICES.md).

NOTIS-WP supported printer models (from `10079K_NOTIS-WP.md`; technology
classification corrected against web research - see the note below the table):

| Class | Models | Quality |
|-------|--------|---------|
| Daisy-wheel | Data Products DP-55 (ND-235), Diablo 1610 / 1650 / 630, TEC-F10 | letter quality (best) |
| Matrix | Philips GP 300 L (ND-408/410/447), Epson MX-80 (ND-423), RX-80 (ND-224), HX-20 | NLQ / draft |
| Line printer | `LINE-PRINTER` / CDC 9380 (only ones that allow over-printing) | draft, fast |
| Plotter (NOTIS-BG) | HP 7475A (ND-218, HP-GL), Philips ND-447/448, Versatec | vector / raster graphics |

**Classification correction (web-verified):** the **Philips GP 300 / GP 300 L**
(ND-408/410/447) is a **9-wire dot-matrix** printer with an NLQ mode, **not** a
daisy-wheel - confirmed by NDWiki and by the fact that NOTIS-BG drove the Philips
for *graphics* (a daisy-wheel cannot print bit-image graphics). Earlier drafts of
this repo grouped it as daisy-wheel; the true letter-quality daisy-wheels in the
NOTIS list are the **Data Products DP-55** (called a "Daisy wheel printer" in
`10079K_NOTIS-WP.md` line 935), the **Diablo 1610/1650/630**, and the **TEC-F10**.

No laser printer is supported by NOTIS-WP/TF (laser existed only via the separate
NORTEXT phototypesetting product, which is not practical to emulate - see
section 5.4). The highest-quality NOTIS text target is the **Diablo-family
daisy-wheel** (letter quality; NOTIS-TF emits shadow-bold + font/pitch codes to
it).

### 1.1 The real ND printer catalogue (from the ND library mirror) [VERIFIED]

Product numbers and models from `E:\Dev\Ronny\mirror-sintran-com\mirror\library`
(ND product-description sheets), grounding the models above in ND part numbers:

| ND no. | Model | Class |
|--------|-------|-------|
| **ND-232** (+233 feeder, 234 cover) | **Diablo 1650 TEXT PRINTER, RS-232** - "word processor quality", proportional spacing, plotting, backward print, two-colour | daisy-wheel (the letter-quality target) |
| ND-424 (also ND-423) | Epson MX-80 | matrix |
| ND-408/410/447 | Philips GP 300 L | matrix (NLQ, multi-font) |
| ND-104701 / ND-103380 | Epson LX-80 | matrix |
| ND-110090 / ND-741259 | Epson LX-86 | matrix |
| ND-475 | GENICOM 3024 | matrix |
| ND-110278 | ND Matrix Printer ADP 515 | matrix |
| ND-616 CN (ND-12.043/044; fonts ND-211419) | ND 616 CN Printer (downloadable fonts) | matrix/colour |
| ND-460 TTP (ND-12.064) | ND Personal Printer 460 TTP | thermal-transfer personal |
| ND-433X | Line Printer, 1000 lpm | line |
| ND-452 | Line Printer | line |
| ND-110383 | ND Line Printer 815 BPA | line |
| ND-425X/426X/427X | TERMINET 340 (current-loop / parallel / RS-232) | line |
| ND-418X/419X | Tally 1612, 160 cps | matrix |
| ND-652 / ND-106520 | Versatec Controller (DMA) | electrostatic raster |
| ND-218 | HP 7475 plotter | vector plotter |
| ND-10644 ... ND-10885 | NORTEXT-100 Typesetter Output modules (Lasercomp, LN202, CG8400/8600, APS, Digiset, Metroset, AGFA P400, Harris 7450, Unisetter, Philips GP-300) | phototypesetter (NORTEXT only) |

Note the ND library has **no** "DIALOG"/"TELFIPS"/"ASTRID" printer product - those
`WP-PRINTERS` names are site logical names, not hardware (see section 5.0).

## 2. Setting up a printer [VERIFIED]

### 2.1 Central spooled printer (the normal case)

SINTRAN side (as user SYSTEM), see [03-CONFIGURATION.md](03-CONFIGURATION.md):

1. Bind a peripheral file to the printer's device number:
   `@SET-PERIPHERAL-FILE "LINE-PRINTER",5` (device 5 = line printer 1).
2. Create extra versions so there are spooling files (buffers):
   `@CREATE-FILE LINE-PRINTER;10,0` (1 real + 9 spooling files).
3. If it is a special-interface line printer, set its type in SINGEN
   ("Define printer type": 1 DMA / 2 Parallel / 3 Serial) or bind the spooling
   index in the service program:
   `*SET-SPOOLING-DEVICE-NUMBER 1,5 Y Y Y`.
4. Start the spooler: `@START-SPOOLING LINE-PRINTER`.

NOTIS side: add the printer to the printer-definition text file
**`WP-PRINTERS:TEXT`** (columns: Printer Type | SINTRAN file name | Logical name
| Feed T/S/D | Default start | Font/Nationality), edited during **`INSTALL-WP`**.
Pick the default in NOTIS-WP **Menu no. 4**. The logical NOTIS name must map to
the SINTRAN peripheral/spooling file name.

### 2.2 Printer on the user's own terminal (local) [VERIFIED]

For a printer on the ND-NOTIS terminal auxiliary port:

1. SINTRAN: `@SET-TERMINAL-FILE "EPSON"` (routes to the terminal's printer port),
   and disable that line's login if it is a dedicated printer line
   (`*REMOVE-FROM-BACKGROUND-TABLE <ldn>`).
2. NOTIS: add the name (e.g. `EPSON`, or `TERMINAL`) to `WP-PRINTERS:TEXT`.

Warning (from the manual): local terminal printing is unbuffered - "your terminal
will hang unless you have a local printer". See
[09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md](09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md).

## 3. Edit a small document in NOTIS-WP and print it [VERIFIED]

```
@NOTIS-WP-ENG                 ; start the editor (terminal type must be 53)
                              ; WP: appears at the Home position

; --- create and type a document ---
; press the down-arrow / HOME to reach the WORK AREA, then type:

^H1=My First Letter;          ; a page header (print directive)
^JM;C;                        ; centre-justify the next lines
Dear Reader,
^JM;S;                        ; back to stretch (smooth right margin)
This is a small document written in NOTIS-WP on an ND-100.
It will be printed on the line printer.

; --- store it (Home command W) ---
; go to HOME position, type:
W "MY-LETTER"                 ; stores MY-LETTER:TEXT

; --- print it (PRINT key, or Home command O) ---
; the dialog:
DEVICE NAME:                  ; Enter = default printer, or type LINE-PRINTER
AREA:                         ; Enter = whole document
NO. OF COPIES:1               ; only honoured if the printer has a spooling file
PAGE LIST:                    ; Enter = all pages
FORMAT? Y                     ; Y = apply the ^ directives and menus
```

For a plain paginated document this is all you need - NOTIS-WP's own PRINT
formats it. For sophisticated layout (chapters, TOC, index) press **SHIFT+PRINT**
to run **NOTIS-TF** first, proof the `:OUT` file in Inspect mode, then print with
**C**. Either way the bytes end up at a SINTRAN spooling/peripheral file and the
SINTRAN spooler drives the printer.

---

# PART 2 - EMULATED PRINT-TO-PDF CONTROLLER (DESIGN)

## 4. Goal and the transparency principle [DESIGN]

Build a device in the RetroCore ND-100 emulator that:

- presents itself on the ND bus **exactly like a real ND printer controller**, so
  SINTRAN's stock driver and NOTIS drive it with no modification and no knowledge
  that it is emulated;
- **drains the print stream at full emulator speed** (no mechanical delay), so a
  job that took minutes on real iron finishes in milliseconds;
- reconstructs each print job and renders it to a **high-quality PDF**, created
  only once the job is known complete, then saved atomically.

The ND machine sees a normal, always-ready printer. The PDF machinery lives
entirely on the host side of the emulator and is invisible to SINTRAN.

## 5. Which printer to emulate, and why [DESIGN, backed by web research]

This section supersedes an earlier draft that led with the CDC 9380. After
researching every printer NOTIS/SINTRAN documents (web sources cited in 5.5), the
conclusion for **highest quality** is:

> **Emulate the Diablo 630 (630 ECS/API superset) as the primary target for
> letter-quality NOTIS-WP/TF text.** Build one daisy-wheel decoder around the
> byte-verified Diablo 630 command set; expose the other daisy-wheels
> (Diablo 1610/1650, Qume, TEC-F10, DP-55) as profiles/flags on it. Keep the
> **CDC 9380 line printer** as the simple/fast fallback and the **HP 7475A
> (HP-GL)** as the NOTIS-BG graphics target. The phototypesetter tier is out of
> scope (5.4).

### 5.0 ND primary-source confirmation (from the ND library mirror)

The Norsk Data library mirror (`E:\Dev\Ronny\mirror-sintran-com`) settles the key
questions from ND's own product sheets:

- **The ND letter-quality printer is the Diablo 1650, shipped as ND-232**
  ("TEXT PRINTER, Diablo 1650 for RS 232 Interface", `mirror\library\libpdpi\
  ND-232-B1-EN.pdf`). Verbatim: it is built on "Diablo's field proven Model
  1355/WP printer mechanism", produces **"word processor quality" output that
  "can include two-colour printing and graphics"**, with selectable options for
  **"true proportional spacing and right margin justification, plotting, backward
  printing, and remote diagnostics"**. Interface: **RS-232-C**, switchable
  110/300/1200 baud, optional to 9600. (Accessories: ND-233 cut-sheet feeder,
  ND-234 acoustic cover.) This is direct ND confirmation of the section-5
  recommendation: the target is the **Diablo 1650 (ND-232) driven over serial
  with the Diablo command set** (proportional spacing, plotting/HyPlot,
  backward-print, red/black - all in the section 7.2 table). Emulate that.
- **The ND-branded "DIALOG-15xx/16xx/17xx", "TELFIPS", "ASTRID" names are NOT ND
  printer hardware.** The mirror's comprehensive printer catalogue (see the list
  in Part 1 section 1) contains **no** such product. As the NOTIS manual states,
  the `WP-PRINTERS` logical names are site-chosen and "independent of the real
  Sintran-III file name", so these are **example logical names in the sample
  printer-definition file, not distinct devices**. ("DIALOG-1650" is most likely
  a site alias for the ND-232 Diablo 1650.) Do not try to emulate them as
  hardware - map the logical name to whichever real printer it points at.

### 5.1 Why Diablo 630 wins for text [research-backed]

- **Highest text quality in the NOTIS set.** It is letter-quality (era benchmark,
  "equivalent to an IBM Selectric"), whereas the line printer and the Epson/
  Philips matrix printers are draft/NLQ.
- **Richest positioning + formatting to reconstruct from.** Full microspacing
  (HMI = 1/120 inch, VMI = 1/48 inch), absolute horizontal/vertical tabs,
  proportional spacing, half-line feeds for super/subscript, and WP attributes
  (bold `ESC O`, shadow `ESC W`, auto-underline `ESC E`/`ESC R`). This is exactly
  the geometry a faithful high-quality PDF needs - see the full table in 7.2.
- **The de-facto industry standard.** Diablo 630 is *the* letter-quality command
  set; the whole daisy-wheel field either is it, descends from it, or offers a
  "Diablo 630 mode" - including later HP/DEC/Apple/Kyocera laser printers. The
  entire NOTIS daisy-wheel list (1610/1650, and the Diablo-ribbon/wheel-compatible
  DP-55 and TEC-F10) shares the same `ESC US`/`ESC RS` core, so **one decoder
  covers all of them**.
- **Best documented, with a head start.** Full Diablo 630 API/SPI/HPRO5 manuals
  are on bitsavers/archive.org (byte tables verified), and an open-source
  **Diab2PS** translator already converts Diablo-630 streams to PostScript
  (auto-justify, center, underline, bold, shadow, H/V tabs, margins, microspacing,
  colour, backward printing). That is a near-perfect reference for our
  Diablo-630-to-PDF path.
- **NOTIS formats natively for it.** NOTIS-TF's `^BT` shadow-bold and the WP
  attribute codes target the Diablo daisy-wheel; NOTIS-WP lists Diablo 1610/1650
  as ND-supported and the K release note (line 970) says "The Diablo 630 printer
  will now be handled properly." So a real NOTIS stream to a Diablo is rich.

One caveat to honour: the base Diablo **SPI** set does **not** include the WP
attributes (bold/shadow/underline) - those are the **630 ECS/API (Word-Processing)
superset**. A NOTIS-WP stream almost certainly uses the WP superset, so **decode
the ECS/API superset**, not the bare SPI set.

### 5.2 The one-decoder-plus-profiles strategy [research-backed]

The daisy-wheels form one command family (signature: `ESC US n` HMI 1/120",
`ESC RS n` VMI 1/48"). Implement one Diablo-630-ECS decoder, then:

| Profile | Status | Notes |
|---------|--------|-------|
| **Diablo 630 ECS/API** | codes byte-verified from primary manuals | the reference profile |
| Diablo 1610 / 1650 | same core, WP-attribute bytes less accessible | subsumed by the 630 superset |
| Qume Sprint | co-standard, feature-equal, **programming manual not online** | Diablo-630 profile + "Qume" flag; resolve divergences from a captured stream |
| TEC-F10 / C.Itoh | "Diablo-630-like" but treated as a separate entry in period software - diverges somewhere | Diablo-630 profile + "F10" flag; confirm from capture |
| Data Products DP-55 | ND calls it a daisy-wheel; **no command manual found** | Diablo-630 profile; confirm from capture |

**Rule:** wherever the online record is thin (Qume, F10, DP-55), a **captured
real NOTIS-WP byte-stream is the final authority** - do not hard-code guessed
codes. Mark such bytes UNVERIFIED until a capture or primary PDF confirms them.

### 5.3 The other device classes

- **CDC 9380 line printer (device 430) - keep as the fast/simple fallback.**
  Fully documented in this repo (doc 08), already emulated in RetroCore,
  SINTRAN-native, highest throughput, but text-only draft (fixed font; bold/
  underline only via over-printing). Because we re-render, its output can still
  be made into a clean PDF - it just carries the least formatting information.
  Good first milestone; not the quality winner.
- **HP 7475A / HP-GL (ND-218) - the graphics target for NOTIS-BG.** The only
  inherently **vector** device: its stream reconstructs to crisp,
  resolution-independent vector PDF (lines, arcs, scalable stroked text, 6 pens).
  Fully documented standard. Use it when the job is a NOTIS-BG chart/plot, not WP
  text.
- **Epson RX-80 / ESC/P - best-documented, graphics-capable, but draft.** ESC/P
  is byte-for-byte public with bit-image graphics (60-240 dpi), but 9-pin draft
  quality. A reasonable secondary if you want one profile that handles both text
  and raster graphics; below Diablo for pure text quality.
- **Versatec (603-606)** - 200 dpi electrostatic raster, a "dumb bitmap sink";
  easy to reconstruct mechanically but almost no command language.
- **DIALOG-15xx/16xx/17xx, TELFIPS-300/700, ASTRID - resolved: not hardware.**
  The ND library mirror's full printer catalogue contains no such product; these
  are **site-chosen logical names** in the `WP-PRINTERS` sample file (see 5.0), not
  devices. Map each to the real printer it points at (e.g. DIALOG-1650 -> the
  ND-232 Diablo 1650) and emulate that.

### 5.4 The typesetting tier is out of scope [research-backed]

"If it's typesetting, even better" - unfortunately not feasible. The true
typesetting tier (NORTEXT's phototypesetters: Monotype Lasercomp, Linotron 202,
Compugraphic/Agfa) is **not practical to emulate**:

- Their host interfaces were **proprietary and essentially undocumented** - the
  canonical proof is the Linotron 202, which Bell Labs had to **disassemble and
  reverse-engineer** to drive (there was no published command language).
- There was **no standard** - each typesetter needed its own reverse-engineered
  binary driver (font-memory loads, CRT/laser deflection, film transport).
- Output is **1-bit film/photo-paper for platemaking**, not text-with-metrics, so
  "to PDF" would mean building a RIP against an unknown input format.

So the **daisy-wheel (Diablo 630/1650) tier is the realistic quality ceiling**
for a PDF-targeting emulator. (Also note: NOTIS-WP/TF never drove a
phototypesetter anyway - NORTEXT was a separate ND-COMTEC product.)

**ND primary-source confirmation:** the ND library mirror's brochure "TYPESETTER
OUTPUT - NORTEXT-100 Typesetter output modules" (`mirror\library\libpdpi\
ND-10644-A1-EN.pdf`, ND COMTEC) confirms this exactly. It lists a **separate
per-typesetter "output module" program** for each machine - Monotype Lasercomp
(ND-10644), CG8400/CG8600 (ND-10820/21), Linotron LN202 German/Scandinavian
(ND-10822/10874), APS + APS Micro5 (ND-10823), Digiset T20/T40 (ND-10824/10880),
Metroset (ND-10825), AGFA P400 (ND-10875), Unisetter (ND-10876), Harris 7450
(ND-10882), and Philips GP-300 (ND-10885). Each "is adjusted to fit the
specialities of one certain typesetter" and converts NORTEXT-100 typographic
codes into that typesetter's format via example font/typesetter/conversion
tables. That is precisely the bespoke, per-device, undocumented-to-outsiders
model that makes the typesetter tier impractical to emulate - and it required the
NORTEXT-100 Editor (ND-10800), not NOTIS-WP. (Aside: the Philips GP-300 appears
here as a low-end NORTEXT proof/output device too - consistent with it being a
multi-font dot-matrix, not a daisy-wheel.)

### 5.5 Sources

Diablo 630 API/SPI/HPRO5 manuals (bitsavers/archive.org) + Diab2PS
(github.com/wwarthen/Diab2PS); Diablo/Qume/daisy-wheel standard context
(en.wikipedia.org/wiki/Diablo_630, /Qume); NEC Spinwriter + ESC/P + HP-GL
references; Linotron 202 reverse-engineering (cs.princeton.edu/~bwk/202/);
Compugraphic/Agfa (en.wikipedia.org/wiki/Compugraphic); ND rebadge/hardware list
(ndwiki.org/wiki/Hardware_list). Full URL list is in the research notes; every
escape byte in 7.2 is tagged with its verification level.

## 6. The controller interface and the "instant-ready" fast path [VERIFIED interface + DESIGN timing]

Emulate the CDC 9380 register block at device 430 (line printer 1; 434 for
printer 2). Registers [VERIFIED, doc 08]:

| IOX | Reg | Dir | Function |
|-----|-----|-----|----------|
| 430 | 0 | read | Read Data (loopback, test only) |
| 431 | 1 | write | **Write Data** (one character into the buffer) |
| 432 | 2 | read | **Read Status** |
| 433 | 3 | write | **Write Control** |

Control word (write, IOX 433) bits: b0 int-on-ready, b1 int-on-error, b2
**activate** (print buffered char), b3 test, b4 device/interface clear. Status
word (read, IOX 432) bits: b3 **ready for transfer**, b4 error, b5 not-ready, b6
out-of-paper, b11-12 band.

**The performance rule [DESIGN]:** a real line printer asserts "ready for
transfer" only after the mechanism has accepted the character (milliseconds per
char). The emulated controller must **assert ready immediately and always** -
never out-of-paper, never not-ready - and raise the level-10 "ready" interrupt on
the very next emulator tick after each `activate`. Consequences:

- SINTRAN's driver loop (`write data -> activate -> wait ready-interrupt ->
  next char`, byte-verified in `../Devices/LINE-PRINTER-CONFIG-INSPECTION.md`)
  never stalls; the whole file is consumed as fast as the CPU can execute IOX.
- Do **not** model real print speed. The only "device" work per character is
  appending one byte to a buffer (section 10), which is O(1).

If you ever need even less per-character overhead, emulate the **DMA (Type 1)**
line-printer instead so SINTRAN hands whole buffers - but with instant-ready PIO
the character path is already effectively free. Keep the status register truthful
only for what SINTRAN checks (ready set, error clear, paper OK).

**Transport depends on the target [DESIGN].** The register block above is the CDC
9380 parallel line printer (the simple/fast fallback, section 5.3). The
recommended **Diablo 630** primary target is a **serial** device - SINTRAN drives
it as a **Type-3 serial line printer** (or as a serial printer on a terminal
line via `WP-PRINTERS`), so its controller is the async serial interface (data +
control/status registers of the 4/8 async controller, see
[02-HARDWARE-DEVICES.md](02-HARDWARE-DEVICES.md)), not the 430 block. The
**instant-ready principle is identical**: assert transmit-ready every tick and
never assert busy, so the byte stream drains at full speed. Only the register
addresses/handshake differ; the capture + PDF pipeline (sections 8-11) is
transport-independent - it consumes the byte stream whichever controller
delivered it.

## 7. Control / escape codes the controller must understand

### 7.1 CDC 9380 line printer [VERIFIED, doc 08]

The controller does not itself need to interpret these for *printing* (it just
buffers bytes), but the **PDF reconstructor** (section 9) must:

| Code (octal) | Meaning | PDF effect |
|--------------|---------|------------|
| 40-176 (printable ASCII) | character | place glyph at current column, advance |
| 11 (HT) | horizontal tab / space in CDC controller | advance to next column stop |
| 12 (LF) | line feed | next line, same column origin |
| 14 (FF) | form feed | **end of page** -> flush page, new page |
| 15 (CR) | carriage return | column := 0 (enables **overprint** of the same line) |
| 20-33 | VFU channels (LP9, form control) | vertical skip to a form channel; treat 20 as FF-like |
| 0-37 others | illegal, ignored by the interface | ignore |

Emphasis on this hardware is done by **over-printing**: print a line, `CR`, print
it again over the same positions (bold = double strike) or print underscores over
it (underline). The reconstructor detects "a glyph printed where one already
exists" and "underscore over a glyph" to synthesise **bold** and **underline**.

### 7.2 Diablo 630 ECS/API - the primary (letter-quality) decoder

This is the full command table for the recommended primary target (section 5),
sourced from the Diablo 630 API (90440-00A), SPI (90449-01A) and HPRO5 (90441-01A)
manuals on bitsavers/archive.org, cross-checked against the code-verified Diab2PS
decoder. `ESC` = 0x1B. Verification: **[V]** = read from a primary Diablo manual
or the code-verified decoder; **[V-ECS]** = Word-Processing/ECS superset (verify
the exact byte in the API PDF before shipping); **[?]** = lower confidence, confirm
before use. `(n)` = one following count/position byte (not an ASCII digit).

Motion and positioning - the core the PDF geometry depends on:

| Function | Bytes | Meaning | Ver |
|----------|-------|---------|-----|
| Set HMI (horizontal motion index) | `ESC US (n)` = 1B 1F n | HMI = (n-1) x **1/120 inch** | [V] |
| Set VMI (vertical motion index) | `ESC RS (n)` = 1B 1E n | VMI = (n-1) x **1/48 inch** | [V] |
| HMI from pitch switch | `ESC S` = 1B 53 | cancel programmed HMI | [V] |
| Absolute horizontal tab | `ESC HT (n)` = 1B 09 n | move to print column n | [V] |
| Absolute vertical tab | `ESC VT (n)` = 1B 0B n | move to line n | [V] |
| Set / clear / clear-all H-tab | `ESC 1` / `ESC 8` / `ESC 2` | tab stop management | [V] |
| Left / right margin | `ESC 9` / `ESC 0` | set at current column | [?] |
| Form length (lines/page) | `ESC FF (n)` = 1B 0C n | 1-126 | [V] |
| Proportional spacing on / off | `ESC P` / `ESC Q` = 1B 50 / 1B 51 | PS carriage | [V] |
| Forward / backward print | `ESC 5` / `ESC 6` | R-to-L printing | [V] |

Base ASCII controls and vertical specials:

| Function | Bytes | PDF effect | Ver |
|----------|-------|------------|-----|
| CR / LF / FF / BS | 0D / 0A / 0C / 08 | col:=0 / next line (one VMI) / page break / back one HMI | [V] |
| HT / VT | 09 / 0B | to preset horizontal / vertical tab stop | [V] |
| Microspace backspace 1/120" | `ESC BS` = 1B 08 | fine back step (PS) | [V] |
| Reverse (negative) line feed | `ESC LF` = 1B 0A | paper up one line | [V] |
| Half-line feed up (superscript) | `ESC U` = 1B 55 | raise 1/2 line | [V] |
| Half-line feed down (subscript) | `ESC D` = 1B 44 | lower 1/2 line | [V] |

Word-processing attributes (630 ECS/API superset - a NOTIS-WP stream will use
these; the base SPI set does not have them):

| Function | Bytes | PDF effect | Ver |
|----------|-------|------------|-----|
| Auto-underline on / off | `ESC E` / `ESC R` = 1B 45 / 1B 52 | underline run | [V-ECS] |
| Bold (double-strike) on | `ESC O` = 1B 4F | bold (letter O, not zero) | [V-ECS] |
| Shadow print on | `ESC W` = 1B 57 | heavier bold | [V-ECS] |
| Bold / shadow off | `ESC &` = 1B 26 | end emphasis | [V-ECS] |
| Ribbon secondary (red) / primary (black) | `ESC A` / `ESC B` | colour run (NOTIS-TF ^BT can use 2nd pass) | [V] |
| ECS glyph-row shift | `SI` / `SO` = 0F / 0E | second wheel row (symbols/Greek/fractions) | [V] |

Reset / graphics:

| Function | Bytes | Note | Ver |
|----------|-------|------|-----|
| Remote reset / initialise | `ESC CR P` = 1B 0D 50 | ~1 s to complete | [V] |
| HyPlot vector graphics on / off | `ESC 3` / `ESC 4` = 1B 33 / 1B 34 | 120x48 dot grid; BS/LF become fine steps. Diab2PS does NOT implement this - confirm in the API PDF | [?] |

The reconstructor maintains a cursor in HMI/VMI units, applies half-line feeds as
baseline shifts (true super/subscript), tracks the attribute flags for bold/
shadow/underline, and uses proportional-spacing + microspacing to place glyphs at
exact x positions - all of which map cleanly to positioned text runs in the PDF
(section 9). This is why the Diablo path yields the highest-quality output.

**Head start:** the open-source **Diab2PS** (Diablo-630 -> PostScript) already
decodes auto-justify, centre, underline, bold, shadow, H/V tabs, margins,
microspacing, alternate colour and backward printing - use it as the reference
implementation for the decoder + geometry, retargeted from PostScript to the PDF
page model here.

### 7.3 What the printer does NOT see

NOTIS `^...;` directives and NOTIS-TF macros are resolved **before** printing -
the device receives only plain characters + the device control codes above. So
the reconstructor never has to parse NOTIS directives.

## 8. Job lifecycle: new print, page breaks, completion [DESIGN]

This is the crux: at the IOX level there is no explicit "job start/end". Use a
layered detector, most-authoritative signal first.

### 8.1 State machine

```
        (no data, idle)
             |
   first IOX-431 write  ---------->  [JOB ACTIVE]
             |                          |  each data byte -> page model (sec 9)
             |                          |  FF / VFU-form  -> close page, start new page
             |                          |
             |            +-------------+-------------------------------+
             |            | completion trigger (whichever fires first): |
             |            |   A. interface clear (control b4)           |
             |            |   B. spooler close snoop (optional, sec 8.3) |
             |            |   C. idle timeout T after last byte          |
             |            +-------------+-------------------------------+
             |                          v
             |                   [JOB COMPLETE] -> flush last page,
             |                   render + save PDF (sec 9/11), reset
             v                          |
        (back to idle) <----------------+
```

### 8.2 The triggers [DESIGN]

- **JOB START:** transition idle -> active on the first `Write Data` (IOX 431)
  after an idle period, or on the first `activate` control word. Allocate a fresh
  page-model (do not create the PDF file yet).
- **PAGE BREAK:** `FF` (14o), or a VFU top-of-form channel (20o). Close the
  current page and begin a new one; a run of trailing blank pages is trimmed at
  finalize.
- **JOB COMPLETE** - finalize on whichever fires first:
  - **A. Interface clear** - SINTRAN writes control-word bit 4 (device/interface
    clear). The spooler may clear the interface between files; treat as an
    immediate, HW-authentic end-of-job. **[VERIFY]** whether the SINTRAN spooler
    actually issues clear at file boundaries (test by tracing IOX 433 between two
    queued files).
  - **C. Idle timeout** - the robust, printer-independent fallback: if no byte
    arrives for `T` after the last byte (default **T = 2000 ms [DESIGN]**,
    configurable) and the job has >= 1 page of content, finalize. This is exactly
    how host-side print-to-PDF spoolers decide a job ended.
  - Finalize on **min(A, C)** (and B if enabled). A trailing `FF` then idle is
    the normal end-of-file eject and is covered by C.

Idle-timeout note: because the emulated printer drains instantly, a whole file
arrives in a burst; the gap to the next queued file (SINTRAN dequeues the next
`LSPOQ` entry, opens it, starts the driver) is far longer than intra-file gaps,
so a modest `T` cleanly separates jobs. Tune `T` from a trace if needed.

### 8.3 Optional: authoritative boundaries by snooping the spooler [DESIGN, still transparent]

Because RetroCore sees all of SINTRAN's memory, it can *observe* (never signal)
the byte-verified spooler to get exact, deterministic job boundaries and even the
job's identity - completely invisibly to SINTRAN. From
[07a-CARVED-INTERNALS-FINDINGS.md](07a-CARVED-INTERNALS-FINDINGS.md):

- `GetSpoolingEntry` (**MON 55**, worker 106212B) dequeues the next queue entry -
  a clean **job START** signal, and the entry carries the **file name / user /
  job name** (fields `SPFNA`/`SPUME`/`SPJNx` off `LSPOQ=162122B`) - perfect for
  **naming the PDF**.
- `SPCLO` (**MON 40**, worker 067572B) closes a spooling file - a clean **job
  END**.

Hooking a breakpoint/watch on these (emulator-side observation) gives 100%
reliable start/end plus metadata, while the device still behaves like plain HW.
Use this as the primary trigger when available, with the idle-timeout (8.2 C) as
the fallback for direct (non-spooled) or terminal-file printing where these MON
calls are not involved.

## 9. Page model and high-quality PDF rendering [DESIGN]

### 9.1 Page model

Represent each page as positioned glyph runs, not a bitmap:

- A page is a list of cells or runs: `(row, col, char, bold, underline, [xMicro,
  yMicro for daisy-wheel])`.
- Maintain a cursor `(col, row)`; printable char writes a cell and advances;
  `CR` -> col 0; `LF` -> row+1; `HT` -> next tab stop; `FF` -> close page.
- **Overprint detection** (line printer): writing a printable char to an occupied
  cell -> mark that cell **bold** (double strike); writing `_` over a
  non-underscore glyph (or a glyph over `_`) -> mark **underline**.
- For the daisy-wheel profile, use the HMI/VMI/half-line data to set sub-character
  `xMicro/yMicro` and a `superscript/subscript` flag.

### 9.2 Geometry -> A4

- Page size: **A4** by default (595 x 842 pt). Columns/lines per page come from
  the stream: page length from the `FF` cadence (and/or SINTRAN page length), and
  the character pitch from the printer/paper config. The CDC line printer is
  fixed pitch (typically 10 chars/inch, 6 lines/inch, up to 132 columns) - the
  exact cpi/lpi should be taken from the `WP-PRINTERS` "Font" column
  (e.g. `8.0`, `11.7`) or made configurable **[VERIFY exact default cpi/lpi]**.
- Map `(row, col)` to points using the chosen pitch and top/left margins so the
  page matches what NOTIS-TF laid out for A4 (its default geometry is tuned for
  A4 at 12 cpi - see the NOTIS doc).

### 9.3 Rendering for highest quality

- Emit **vector text** (real glyphs, not a rasterised bitmap) so output is crisp
  at any zoom and the PDF stays small.
- Embed a good **monospaced** font for faithful column alignment (a clean
  typewriter / Letter Gothic / a high-quality mono). Offer two style modes:
  a **faithful** look (mono, mimics the original) and a **clean** look; keep it
  configurable, default faithful.
- **Bold** -> bold font weight (or synthetic double-strike offset to mimic the
  daisy-wheel shadow). **Underline** -> drawn rule under the cell.
  **Super/subscript** (daisy-wheel) -> raised/lowered baseline at reduced size.
- Preserve exact horizontal positions from column index (and micro-spacing for
  daisy-wheel) so tables and justified text line up perfectly.
- Render at vector resolution (DPI-independent); if you also want a raster
  preview, 300+ DPI.

Use a PDF library that supports embedded fonts + precise text positioning; keep
the renderer pure (page model in, PDF bytes out) so it is unit-testable.

## 10. Performance architecture [DESIGN]

Keep the emulation hot path O(1) and push all heavy work off-thread:

- **IOX-431 write handler:** append one byte to a lock-free/ring buffer and
  return; assert ready immediately (section 6). No parsing, no allocation on the
  hot path. Use a pooled buffer (`ArrayPool<byte>`), `Span<byte>` slices - no
  LINQ, no `foreach` on the hot path (matches the RetroCore coding rules).
- **Consumer:** a separate worker drains the ring buffer, runs the control-code
  state machine, and builds the page model. This decouples the ND CPU timing from
  parsing.
- **PDF render:** triggered only on JOB COMPLETE, run on a background task, off
  the emulation thread, so finalizing one job never stalls the next.
- Because the device is always-ready, a full document is accepted in a single
  fast burst; parsing + rendering happen afterwards without the ND machine
  waiting.

## 11. Finalize and save the PDF [DESIGN]

On JOB COMPLETE:

1. Flush the current (last) page; trim trailing blank pages.
2. Render all pages to PDF bytes (section 9).
3. **Name it** deterministically: prefer the spooler-snoop metadata
   (`SPFNA` file name + `SPUME` user + timestamp) if available (section 8.3);
   else a timestamp + sequence number.
4. **Atomic save:** write to a temp file, `fsync`/flush, then rename into place,
   so a reader never sees a half-written PDF.
5. Reset the state machine to idle, release pooled buffers, ready for the next
   job.

## 12. Build order, verification, open items [DESIGN]

Recommended build order (milestone 1 is the fast fallback; the quality target is
the Diablo 630):

1. **Fallback controller + always-ready fast path** at device 430 (extend the
   existing RetroCore CDC 9380). Prove SINTRAN `@COPY-FILE LINE-PRINTER,file` and
   `@START-SPOOLING` drain instantly. This validates the capture/lifecycle/PDF
   pipeline against a fully-verified device before adding the Diablo.
2. **Capture + control-code state machine + page model.** Unit-test with a known
   byte stream (chars, CR-overprint bold/underline, FF pages).
3. **PDF renderer** (page model -> A4 vector PDF). Unit-test golden PDFs.
4. **Job lifecycle:** idle-timeout completion first (portable), then add the
   interface-clear trigger, then the optional spooler snoop for authoritative
   boundaries + naming.
5. **Diablo 630 ECS decoder (the quality target)** on a serial controller
   (section 6 transport note): implement the section 7.2 table (HMI/VMI,
   half-line super/subscript, bold/shadow/underline, proportional spacing), using
   Diab2PS as the reference. Add Qume/F10/DP-55/1610-1650 as profile flags.
6. **NOTIS end-to-end:** edit a NOTIS-WP doc (Part 1 section 3), print to the
   emulated Diablo, confirm a faithful letter-quality A4 PDF (with real
   super/subscript and pitch). Capture the actual NOTIS-to-printer byte stream and
   reconcile any UNVERIFIED bytes against it - the capture is the final authority.
7. (Optional) **HP 7475A / HP-GL** profile for NOTIS-BG graphics -> vector PDF.

Open items to verify before/while implementing:

- **[VERIFY]** exact default cpi/lpi and column count the SINTRAN/NOTIS print
  path assumes (from the `WP-PRINTERS` "Font" column, e.g. `8.0`/`11.7`, and/or a
  trace).
- **[VERIFY]** whether the SINTRAN spooler issues interface-clear (control b4)
  between queued files (trace IOX 433) - affects the completion trigger.
- **[VERIFY]** the exact Diablo 630 bytes tagged **[V-ECS]**/**[?]** in section
  7.2 (WP attributes, margins `ESC 9`/`ESC 0`, HyPlot `ESC 3`/`ESC 4`) against the
  API PDF and a captured NOTIS-to-Diablo stream, before hard-coding.
- **[VERIFY]** what the ND-catalogue `DIALOG`/`TELFIPS`/`ASTRID` printers actually
  are (section 5.3) before ever targeting one.
- The generic line-printer driver code body and exact spooler close ordering are
  still open in [07a-CARVED-INTERNALS-FINDINGS.md](07a-CARVED-INTERNALS-FINDINGS.md)
  (relevant if you rely on the snoop path).

---

## Summary

- **Setup / supported printers / edit-and-print:** Part 1, all verified from the
  SINTRAN and NOTIS manuals (with the Philips-GP-300-is-matrix correction).
- **For highest quality, emulate the Diablo 630 (630 ECS/API superset)** - the
  letter-quality de-facto standard, richest text positioning (HMI 1/120", VMI
  1/48", proportional spacing, half-line super/subscript, bold/shadow/underline),
  best-documented (primary manuals + the Diab2PS reference decoder), and the
  device NOTIS-TF formats natively for. One daisy-wheel decoder covers the whole
  family (Qume/F10/DP-55/1610-1650 as profiles). Use the **CDC 9380 line printer**
  as the simple/fast fallback and **HP-GL (HP 7475A)** for NOTIS-BG graphics.
- **The phototypesetter tier is out of scope** - proprietary, undocumented,
  film-output; the daisy-wheel is the realistic quality ceiling.
- Make the controller **always-ready** so jobs drain instantly; **detect a new
  job** on first data after idle (or the spooler-snoop MON 55 dequeue); **detect
  completion** via interface-clear / idle-timeout / spooler-snoop MON 40 close;
  then **render the page model to a crisp A4 vector PDF** and save it atomically -
  all invisible to SINTRAN/NOTIS.
- **Wherever the online record is thin (Qume, F10, DP-55, the ND-branded units), a
  captured NOTIS byte-stream is the final authority** - do not hard-code guessed
  codes.
