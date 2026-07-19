# NOTIS: Editing, Printing, and How the Beautiful A4 Pages Are Made

This document answers three questions about Norsk Data's NOTIS office suite on
SINTRAN III / ND-100:

1. How does a **user edit and print** a document with NOTIS?
2. How does NOTIS produce **professionally typeset, paginated A4 pages**?
3. How does NOTIS **actually print** - the print path, printer definitions, and
   which hardware it drives?

It is built from the NOTIS manuals in this folder; every non-obvious claim is
cited to a specific manual and section. Where the manuals are silent, that is
stated rather than guessed. Start point was [README.md](README.md).

Related NDInsight docs: the SINTRAN-level spooling and printer mechanics that
NOTIS sits on top of are in
[../../SINTRAN/Print/README.md](../../SINTRAN/Print/README.md) (especially
[09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md](../../SINTRAN/Print/09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md)
for the terminal-attached printer path).

---

## 0. The two programs, and the one-line answer

NOTIS splits the job in two:

- **NOTIS-WP** - the interactive full-screen **editor**. You type, edit, and lay
  out the document here, and for ordinary documents you print directly from it.
- **NOTIS-TF** - the batch **Text Formatter**. When you need sophisticated,
  book-quality pagination (chapters, TOC, index, footnotes, running headers),
  NOTIS-TF reads the same file and produces the polished output.

Neither program drives a printer directly for a shared/central printer: both
format the document into a disk file and hand it to **SINTRAN spooling** (or, in
the later N release, the **SPRINT** spooling system), which empties it onto the
physical printer. A printer attached to the user's own terminal is the one
direct-drive exception.

The "beautiful A4 page" is not produced by a "select A4" button - there is **no
paper-size directive**. A4 is the *default page geometry* (width, borders, pitch)
that NOTIS-TF ships with (section 2 below).

---

## 1. How a user edits and prints (NOTIS-WP)

Sources: `ND-63.001.02 Introduction to NOTIS-WP.md` [INTRO],
`ND-63.002.02 NOTIS-WP Reference Manual - Editor.md` [EDITOR],
`ND-99.006.3 EN NOTIS-WP REFERENCE CARD for ND-NOTIS terminals.md` [CARD].

### 1.1 Starting and opening a document

- After the SINTRAN `@` prompt, start the editor by program name:
  `@NOTIS-WP-ENG` (or `-NOR` / `-FR`), abbreviable to `@N-W-E`
  ([INTRO] section 3.1). NOTIS-WP shows `WP:` at the Home position
  ([EDITOR] section 1.4).
- The terminal type must be **53** for an ND-NOTIS terminal (TDV-2200/9); set it
  with `WP:!` then "New terminal type: 53" ([INTRO] section 3.1). (FACIT-4420 =
  type 57.)
- **New document:** just start typing, then store with the Home command **W**
  ("write") and a quoted name, e.g. `"PRACTICE-1"` ([INTRO] section 3.3). Note the
  manual's own distinction: *WRITE stores the document; PRINT produces paper*
  ([INTRO] section 3.5) - they are separate acts.
- **Existing document:** Home command **R** ("retrieve") + name; the `:TEXT`
  file type is the default ([INTRO] section 4.1).

### 1.2 The editing model

A full-screen editor driven by the ND-NOTIS keyboard's dedicated keys, with the
cursor in one of two places: the **HOME / COMMAND position** (top-left, after
`WP:`, where single-letter commands are typed) and the **WORK AREA** (the text).
The HOME key and up-arrow move between them ([INTRO] section 3.2). An
inverse-video **status line** shows the active modes: character size 7/16,
justification (`<<` left, `>>` right, `<>` stretch, `><` center), word-wrap (WW),
word-justify (WJ), Expand, Append, underline ([INTRO] sections 3.1-3.2).

Editing keys (from [CARD] and [INTRO] section 4.3):

- Arrows move by char/line; SHIFT+arrows jump to line ends / scroll; the GO-TO
  key plus a target jumps to first/last line, marked text, or a line/page.
- `DELETE` removes a character; **F1** deletes the current line; **CANCEL**
  restores the last F1-deleted line.
- **INS/EXP** toggles EXPAND (insert) mode; **SHIFT+INS/EXP** toggles APPEND.
- **F2** inserts a blank line, **F5** splits a line, **SHIFT+F5** joins.
- Find: **SHIFT+F7** (`WP:Get string:`), repeat **F7**. Substitute: Home **S**.

### 1.3 Laying out the page while editing

- **Borders (fixed page-edge blanks):** Home **B** -> `Wp:Borders: <left>,<right>`
  (default 1,70). **Margins (changeable on-screen):** set with the margin keys
  ([INTRO] section 5.1). The manual distinguishes *border* (whole document, on
  paper) from *margin* (on-screen, changeable mid-text).
- **Tab stops:** Home **T** -> `WP:Tabulators: <n,n,...>` (defaults
  9,27,36,45,54,63,72), or the `TAB` key sets one at the cursor; `SHIFT+TAB` sets
  a decimal tab ([INTRO] section 5.2).
- **Justification:** the orange justification keys set left/right/stretch/center;
  the `JUST` key re-justifies the current paragraph ([INTRO] section 5.3).
- The third screen line is the **ruler / format line**: `(` left border, `)`
  right border, `T` tab stops ([INTRO] section 3.1). A dedicated **Edit Ruler**
  mode edits and stores rulers ([CARD]).

### 1.4 The PRINT command and its dialog

Press the orange **PRINT** key (= `FUNC !`, or Home command **O**). The dialog
([EDITOR] sections 6.8 and 4.23):

```
DEVICE NAME:            <- printer name; Enter alone = whole document on the DEFAULT printer
AREA:<from line>:<to line>
NO. OF COPIES:<number>  <- accepted ONLY if the printer has a spooling file (queue)
PAGE LIST:<from>:<to>   <- ':' = range;  ',' = individual pages (e.g. 2,5,9,11)
FORMAT? Y/N             <- Y = apply menus + print directives;  N = raw text
```

Key points:

- **DEVICE NAME** picks the printer. Blank = the default printer set in Menu
  no. 4; a name (e.g. `LINE-PRINTER`, `PHILIPS-2`) picks another
  ([EDITOR] section 6.8).
- **NO. OF COPIES** is honoured *only* for a printer that has a spooling file:
  "The system will only take this indication if your printer has a queue system
  (a 'spooling file')" ([EDITOR] section 6.8). A directly-connected printer
  prints a single copy. This is the visible seam between **spooled** and
  **direct** printing.
- **FORMAT? Y** applies the menu values and the standard `^...;` print directives
  (section 2.5 below); **N** prints the raw characters (underline shows as
  literal characters).
- In **Inspect mode** (used to proof a formatted `:OUT` file) the print command is
  **C** (or PRINT) and the range prompt is `SHEET LIST:` instead of AREA/PAGE
  LIST ([EDITOR] section 13.3.1).

During a direct print the top-right corner shows a sweeping arrow and
`Wp:Printing on: <printer-name>` while the text is sent ([INTRO] section 3.4).

### 1.5 WP prints ordinary documents itself; TF is for the hard cases

The PRINT function formats using the four **menus** plus the **standard print
directives** it finds embedded in the text ([EDITOR] section 12). You do **not**
need NOTIS-TF for ordinary paginated output. The boundary is explicit:

> "the PRINT function only carries out NOTIS-WP Editor functions and not
> NOTIS-TF Text Formatter functions" - so NOTIS-TF *special* directives are
> printed as literal text even if you answer FORMAT? Y. ([EDITOR] section 6.8.)

Use NOTIS-TF when you need the sophisticated formatting / macro library
([EDITOR] section 1; [INTRO]). You invoke it from the editor with **SHIFT+PRINT**
(= `FUNC "`, or Home **J**); it prompts
`ACTIVATE NOTIS-TF-xxx-J <document-name>:TEXT`, formats, and drops you into
Inspect mode to proof the result ([EDITOR] section 6.9).

### 1.6 The WP page-layout directives (executed by WP-PRINT)

Stored as `^...;` control codes in the text and executed at print time
([CARD] "Print Directives"; [EDITOR] section 12):

| Concern | Directive(s) |
|---------|--------------|
| Page geometry | `^PL=n;` page length, `^PW=n;` page width, `^LB`/`^OB`/`^TB`/`^BB` borders |
| Pitch / spacing | `^HP=n;` chars/inch, `^VP=n;` lines/inch (`6`=single, `4`=1.5, `3`=double) |
| Headers / footer | `^H1=text;`, `^H2=text;`, `^TL_text;` trailer, `^PN=n;` page number, `^PH=n;` header on/off |
| Page control | `^PG;` new page, `^CP=n;` conditional new page, `^BL[=n];` blank line(s) |
| Emphasis | `^BT=+;`/`^BT=-;` bold (also on-screen **F3** underline), `^B=text;` |
| Structure | `^CHAP`, `^SECD/^SECU`, `^CO;` table of contents, `^CN=n;` chapter number |
| Devices | `^FONT=n;`, `^NAT` national set, `^BIN-IN`/`^BIN-OUT` paper bins, `^DX=n;` duplex |

Menus 3 and 4 are shared by both WP-PRINT and NOTIS-TF; Menu 3's values are "for
use with the NOTIS-TF formatter" ([INTRO] Menu notes).

---

## 2. How the beautiful A4 pages are made (NOTIS-TF)

Sources: `ND-63.007.01 NOTIS-TF Text Formatter Reference Manual.md` [REF],
`ND-63.009.01 NOTIS-TF Macro Guide.md` [MAC],
`ND-63.041.1_NO_NOTIS-TF-M_Nye_funksjoner_October_1985.md` [M-NO].

### 2.1 What NOTIS-TF is

A batch formatter that is both "a mode in the NOTIS-WP system" and "a subsystem
under SINTRAN III" ([REF] section 1). It takes one or more unformatted `:TEXT`
files (which contain the text **and** the embedded `^...;` directives) and
produces a formatted `:OUT` file. It "reads a file as a sequence of words ...
copies the input file word by word ... until a line is full. It then justifies
the line to obtain a smooth right-hand margin" ([REF] section 1).

Crucially, **TF never writes to a device** - "NOTIS-TF never writes the output
file directly on an output device, but creates a disk file", then enters INSPECT
mode in NOTIS-WP for on-screen proofing; you print the `:OUT` from there with the
`C`/PRINT command ([REF] section 1.1). This format-to-file then proof then print
model is the whole pipeline.

### 2.2 Page geometry - and where A4 comes from

TF separates **BORDER** (fixed page-edge delimiters, set before text) from
**MARGIN** (variable within the text area) ([REF] section 1.6.1). Defaults:

| Property | Default | Directive |
|----------|---------|-----------|
| Page length | 66 lines (incl. 4 header + 2 trailer -> 60 body) | `^PL=lines;` |
| Page width | 100 character positions | `^PW=num;` |
| Left border | 18 blanks | `^LB=num;` |
| Right ("other") border | 12 blanks -> 70 columns of text | `^OB=num;` |
| Top border | 4 header lines | `^TB=n;` |
| Bottom border | 2 trailer lines | `^BB,n;` |
| Horizontal pitch | 12 chars/inch (auto-adjusts L/R borders) | `^HP=n;` |
| Vertical pitch | 6 lines/inch = single spacing | `^VP=n;` |
| Duplex | off | `^DX=0/1;` |

**A4 is explicitly the design target of these defaults:**

> "The default values are satisfactory for a pitch of 12 characters/inch on **A4
> paper**, with borders of 1.5 and 1 inch respectively, if the zero point is on
> the edge of the paper." ([REF] section 1.6.1.)

Change to 10 chars/inch and the manual re-derives Page Width 83, Left Border 15,
Other Border 10 - "again setting borders of 1.5 and 1 inch on A4 paper". So:
**there is no `PAPER=A4` directive**; A4 is achieved by the line/column/border
geometry chosen for A4 at a given pitch. Line spacing is `^VP` (e.g. `^VP=4;` =
1.5 spacing); variable in-body margins are `^LM,n;`/`^RM,n;`/`^BM,n;` (absolute or
`+n` relative). Pitch must be set before margins, and borders/page-width cannot
change once text has begun ([REF] section 2.3-2.4).

### 2.3 Typography (how "beautiful" is achieved)

| Feature | Directive | Notes |
|---------|-----------|-------|
| Justification | `^JM;mode;` | STRETCH (default, inserts inter-word space for smooth right margin), LEFT, RIGHT, CENTER |
| Filling | `^FM,mode;` | FILLING (default), NOFILL, CONDITIONAL, TRUNCATE (for listings) |
| Centre one line | `^CE,string;` | breaks line, centres, new line |
| Bold | `^BT,level;` (`+`/`-`), `^BS,n;` | on DIABLO daisy-wheel = double/shadow print (2nd pass slightly displaced); `^BS` double-prints headings |
| Underline | `^UM,mode;` FULL/PARTIAL, `^UC,char;` | usually applied on-screen in WP |
| Super/subscript | `^SC,t,d,cp,ar;` | UP/DOWN/NONE + carriage restart; "the result is printer dependant" |
| Fonts | `^FONT=n;` | selects hardware fonts 1/2/4/5 on the **Philips GP 300** (default text font 2) |
| Accents | `^\a` overprint | produces accented letters by overprinting diacritics |

Honest limits (verified absent - grep count 0 in [REF]/[MAC]): **no automatic
hyphenation** (the user marks optional break points with a discrete-hyphen key;
[REF] section 1.5), **no proportional spacing, no italics, and no multi-column /
newspaper layout**. The only gestures toward true typesetting are the `^BT`
"level" parameter noted as "intended for photo-set output" and the
printer-dependent `^SC`. So NOTIS-TF quality comes from justification, centring,
borders/margins, bold (double/shadow), underlining, GP-300 font selection, and
pitch - not from proportional/typeset composition.

### 2.4 Page furniture (what makes it look like a book)

- **Headers / footer:** `^H1=text;` (line 1, beside the page number), `^H2=text;`
  (line 2), `^TL=text;` (centred trailer/footer), `^PN=n;` (initial page number),
  `^PH=0/1;` (headers+numbers off/on), `^NS,-/+;` (numbering off/on)
  ([REF] section 2.4).
- **New page:** `^PG;` forced, `^CP=n;` conditional (only if < n lines left).
- **Chapters / sections:** auto-numbered `^CH,`/`^SE,`/`^SD,`/`^SU,`/`^AP,`
  (chapter/section/down/up/appendix), or the recommended macros
  (CHAP, SEPG, ...). Numbers are readable via `^$CN;` / `^$SN;`.
- **Table of contents:** `^CO;` builds it from all chapter/section titles; `^IC;`
  places it; `^CO-PUT,...;` inserts manual entries ([REF] section 2.5).
- **Index:** `^INDEXON;`, `^XA,term;` (auto every occurrence), `^X,term;`
  (explicit), `^XR,` (reverse two-word term); two-word terms become main+sub
  entries ([REF] section 2.7).
- **Footnotes:** via macros - `^FOOTNOTES,start,presentation,end;` to initialise,
  then `^FOOT/text;` per note (numeric or roman) ([REF] section 5.12; [MAC]).
- **Figures:** `^FI,n;` reserve n lines, `^FN,n;` figure number ([REF] section 2.5).

### 2.5 Macros = reusable "standard layouts"

A macro is "a combination of commands/directives ... defined to simplify trivial
and time-consuming routines", user-definable (unlike built-in directives), so
users "build up a well adapted macro library" - explicitly including "describing
frequently-used document formats / standard layouts" ([MAC] section 1). NOTIS-TF
ships a standard library `NOTIS-TF-ENG-xxx:LIB`.

Six types: USER (`MD/name/body;`, call `name/params;`), INTEGER (`IM/`), SYSTEM
(read-only), REFERENCE (`RD/`), TRIGGER (`TM/`, auto-expands on a condition), and
TRIGGER STRINGS ([MAC] section 1.5). Examples:

```
md/chl/chloramphenicol;              -> "chl;" expands to chloramphenicol
MD/TITLE/'<'Manual for '1';'>;       -> "TITLE/NOTIS-TF;" -> "Manual for NOTIS-TF"
MD/TITLE/'<Manual for '1,NOTIS-WP;'>;  -> default param "NOTIS-WP" if omitted
```

The shipped document macros (CHAP, SEPG, APPX, FOOTNOTES, FIG, BOLD, ...) are
built exactly this way - so "a standard report or letter layout" is realised by
defining/using such macros ([REF] chapter 5; [MAC] preface).

---

## 3. How NOTIS actually prints (the print path and hardware)

Sources: `10079K_NOTIS-WP.md` [K], `210079N_NOTIS-WP_for_ND-100.md` [N],
`ND-10079G NOTIS-1.md` [G], `ND-63.002.02 ... Editor.md` [EDITOR].

### 3.1 The print modules

Printing is separate code from the editor:

- **K release (1983):** `WP-PRINT-K:BPUN` is the "Printer swap segment code",
  loaded onto its own reentrant segment (251) next to the editor (250); the
  editor is `WP-EDITOR-K:BPUN` ([K] program table + RT-loader log).
- **N release (1987):** a finer split - separate `WP-EDITOR-N05`,
  `WP-CMD-N05` (command), `WP-RES-N05` (resident), **`WP-SPOL-N05` (spooler)**,
  `WP-PRINT-N05` (print), `WP-IO-N05`, plus `TF-MAIN-N05`/`TF-UTIL-N05` for the
  formatter ([N] file manifest). The N release notes treat PRINT as its own
  subsystem alongside EDITOR / INSPECT / TEXT FORMATTER.
- **NOTIS-1 (1982):** earliest; print logic lived inside the editor `TED` ("the
  print part of TED"), which "print[s] these on an output device directly"
  ([G]).

### 3.2 The printer-definition file

NOTIS selects and describes printers through an editable per-installation text
table:

- Named **`WP-PRINTERS-K:SYMB`** ("THE PRINTER DESCRIPTION FILE", [K]) or
  **`WP-PRINTERS:TEXT`** ([EDITOR] section on LOCAL PRINTER). It "is an ordinary
  text file ... This file MUST be in 7-bits format".
- Columns: **Printer Type | SINTRAN file name | Logical printer name | Feed/Form
  type | Default start value | Font/Nationality** ([K]). Feed codes: T (tractor),
  S (single sheet), D/M. Font values encode pitch (`8.0`, `11.7`, ...) and
  national character set (0 US, 2 France, 3 UK, 4 Denmark/Norway, 5
  Sweden/Finland, 6 Italy).
- Logical vs physical: "the printer names must be the exact match of the
  installation's peripheral or spooling file names ... The Logical printer name
  is the name under which you wish to use the printer, and is independent of the
  real Sintran-III file name" ([K]). One printer can appear under several logical
  names with different defaults.
- Installed/edited via **`INSTALL-WP`**; the printer file "must be edited to suit
  your installation" ([K]). In the N release the installer is `WP-INST-100-N`.

**Important gap (honest):** the *internal* per-printer control-code syntax inside
`WP-PRINTERS` (the actual escape sequences) is called "self-explanatory" in the
manual but the byte-level codes are **not reproduced** in any manual read. So the
concrete escape sequences are not documented here.

### 3.3 It rides SINTRAN spooling

NOTIS does not carry its own device driver for shared printers; it writes to a
named SINTRAN **peripheral or spooling file** and SINTRAN's spooler empties it:

- The printer-file names *are* SINTRAN peripheral/spooling file names ([K]).
- Multiple copies work "only ... if your printer has a queue system (a 'spooling
  file')" ([EDITOR] section 6.8) - i.e. copies require a spooled target.
- Print errors are raw SINTRAN errors, e.g. "No more unused spooling files
  available (SINTRAN error no. 245B)" ([EDITOR] Appendix A) - proving the path
  goes through SINTRAN spooling services.
- The **N release** targets the dedicated **SPRINT Spooling System** (reg.
  211056): Services/Printer-queue lists the SPRINT queue, Services/List-printers
  lists SPRINT printers, and there is the dedicated `WP-SPOL-N05` spooler ([N]).

This is the SINTRAN spooling machinery documented in
[../../SINTRAN/Print/01-OVERVIEW-AND-CONCEPTS.md](../../SINTRAN/Print/01-OVERVIEW-AND-CONCEPTS.md)
and the carved internals in
[../../SINTRAN/Print/07a-CARVED-INTERNALS-FINDINGS.md](../../SINTRAN/Print/07a-CARVED-INTERNALS-FINDINGS.md).
NOTIS is a *client* of it: it produces the file and appends it to the queue; the
SINTRAN spooler (APSPF/SPCLO etc.) and the physical line-printer driver do the
rest. (The exact SINTRAN spooler call NOTIS uses is not spelled out in the WP
manuals - only the behaviour and error 245B are.)

### 3.4 The terminal-attached (local) printer exception

NOTIS can also print to a printer on the **user's own terminal** (the ND-NOTIS
terminal auxiliary printer port), driven directly rather than spooled:

- "If a terminal has a local printer attached, you may use this as an output
  device in NOTIS-WP." Define it as a **TERMINAL-FILE** with the SINTRAN command
  `@SET-TERMINAL-FILE "EPSON"`, then add it to `WP-PRINTERS:TEXT`
  ([EDITOR] "LOCAL PRINTER" section).
- It is unbuffered direct I/O, with a sharp warning: "If you use TERMINAL as
  output device, your terminal will hang unless you have a local printer"
  ([EDITOR]). NOTIS-BG corroborates: "Epson ... connected to a NOTIS terminal as
  a local printer ... Output ... is sent via the terminal, which hangs during
  printing."

This is exactly the terminal auxiliary-print path documented at the SINTRAN and
terminal-emulator level in
[../../SINTRAN/Print/09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md](../../SINTRAN/Print/09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md).
`@SET-TERMINAL-FILE` is the SINTRAN half; the terminal's aux printer port is the
hardware half.

### 3.5 Which printers - and the laser question

Verified supported models (NOTIS-WP K, [K]):

- **Daisy-wheel (letter quality):** Data Products DP-55 (called a "Daisy wheel
  printer" in [K] line 935), Diablo 1610 / 1650 / 630, TEC-F10.
- **Matrix (NLQ / draft):** Philips GP 300 L (ND-408/410/447), Epson MX-80,
  Epson RX-80, Epson HX-20.
- **Line printer:** `LINE-PRINTER` (only line printers that allow over-printing).

> **Correction (web-verified):** the **Philips GP 300 / GP 300 L is a 9-wire
> dot-matrix printer with an NLQ mode, NOT a daisy-wheel** (NDWiki; and NOTIS-BG
> drove the Philips for *graphics*, which a daisy-wheel cannot do). NOTIS-TF's
> `^FONT=n;` selecting "fonts on the Philips GP 300" is font selection on a
> matrix printer. The true letter-quality daisy-wheels here are the DP-55,
> Diablo, and TEC-F10.

The **highest-quality** text device NOTIS-TF explicitly formats for is the
**Diablo-family daisy-wheel** (DIABLO shadow-bold `^BT` and the WP attribute
codes). See `../../SINTRAN/Print/10-EMULATED-PDF-PRINTER-DESIGN.md` section 5 for
the full ranked printer comparison and why Diablo 630 is the best emulation
target.

**Laser printers: not supported / not mentioned anywhere in the NOTIS-WP or
NOTIS-TF manuals** (grep count 0 for "laser" in [REF], [M-NO], and the WP files).
No Qume, no Epson FX, no HP. NORTEXT phototypesetting exists as a *separate* ND
product and is referenced only by NOTIS-IR (as NTX-IR), not by NOTIS-WP/TF - so
NOTIS word-processing output did not go to a laser printer or phototypesetter.
This matches the SINTRAN-level finding in
[../../SINTRAN/Print/02-HARDWARE-DEVICES.md](../../SINTRAN/Print/02-HARDWARE-DEVICES.md)
section 7 (no spooled laser driver in classic SINTRAN; laser only via NORTEXT).

---

## 4. The full workflow, end to end

```
   Edit in NOTIS-WP  (@NOTIS-WP-ENG, type text, set borders/tabs/justify,
        |             embed ^...; directives, store with W)
        |
        +--- ordinary document -------------------------------+
        |                                                      |
        |  PRINT key (Home O): FORMAT? Y  -> WP-PRINT applies   |
        |  menus + standard ^ directives                       |
        v                                                      v
   SHIFT+PRINT (Home J) -> NOTIS-TF                     DEVICE NAME: <printer>
        |  formats :TEXT -> :OUT (chapters, TOC,               |
        |  index, footnotes, A4 geometry)                      |
        v                                                      |
   INSPECT mode: proof :OUT on screen                          |
        |  C / PRINT -> DEVICE NAME + SHEET LIST + copies      |
        v                                                      v
   +---------------------------------------------------------------+
   |  Named SINTRAN peripheral / spooling file (or SPRINT queue)   |
   |  -- OR -- a TERMINAL-FILE for a printer on the user's terminal|
   +---------------------------------------------------------------+
        |                                            |
        v                                            v
   SINTRAN spooler empties the queue          Direct out the terminal
   to the physical printer                    aux port (terminal hangs)
```

---

## 5. How ND produced its own manuals (self-evidence)

Were the ND manuals themselves made with NOTIS? For the NOTIS manuals the answer
is **yes, verifiably** - they are self-referential proof. The manuals were
written in NOTIS-WP, formatted with NOTIS-TF, and prepared as **camera-ready
copy** for reproduction.

Direct evidence, all inside `ND-63.007.01 NOTIS-TF Text Formatter Reference
Manual.md`:

- Its own reader-comment sheet is titled **"NOTIS-TF Reference Manual Camera
  Ready Copy"** (line 4409). "Camera-ready copy" = the finished formatted pages
  that are photographed to make offset printing plates.
- The TF **`MANUAL` macro** entry states: **"This macro has been used for the
  NOTIS-1 Reference Manual you are now studying"** (line 3167), and "the present
  manual is an example in itself" (line 3193). So the manual you read was
  paginated by the NOTIS-TF `MANUAL` macro (title page, preface, chapters, TOC).
- The manual demonstrates its own formatting: "See headers in this manual, where
  we have requested duplex copying" (line 960); footnotes "as in this document"
  (line 2844); "The text above was produced by this macro call" (line 2616).
- Manuals are **loose-leaf** "for ease of updating ... old pages may be removed
  and new pages easily inserted" (Documentation Catalogue ND-40.004.7, line 36),
  which fits a NOTIS-TF page-at-a-time reprint workflow.

**Laser vs typeset - the important distinction:**

- **"Typeset" in the sense of computer-formatted/paginated:** yes - NOTIS-TF is
  exactly that (justification, borders/A4 geometry, auto chapters/TOC/index).
- **"Typeset" in the professional photo-typesetting sense:** that was a
  *separate* ND-COMTEC product line, **NORTEXT-100**, whose "Typesetter Modules"
  drove real phototypesetters - APS, MCS 8400, **Lasercomp**, CG8600, **Linotron
  202**, Agfa P400, Digiset 720, Metroset (Documentation Catalogue lines
  3850-3852). NORTEXT was aimed at newspapers/advertising/print shops, not
  ordinary manual production, and the NOTIS-WP/TF manuals give **no** evidence of
  being run through NORTEXT.
- **"Laser":** in this era "laser" at ND means the **Monotype Lasercomp**, a
  laser *imagesetter* (laser-exposed film) inside NORTEXT - **not** a plain-paper
  office laser printer. There is **no evidence** the NOTIS manuals were laser-
  printed; office laser printers were only arriving (HP LaserJet 1984, Apple
  LaserWriter 1985) and are not mentioned in any NOTIS/TF manual here.

**Most-likely production chain (verified where cited, inference where marked):**

```
   Author writes in NOTIS-WP  (verified: manuals made with NOTIS)
        -> NOTIS-TF MANUAL macro formats to :OUT  (verified, line 3167)
        -> printed as CAMERA-READY COPY            (verified, line 4409)
           on a letter-quality device              (INFERENCE: NOTIS-TF's top
           = daisy-wheel Philips GP 300, or a          documented quality device
           phototypesetter via the ^BT photo-set       is the daisy-wheel; the
           hook)                                        exact device is not stated)
        -> offset-printed / reproduced as a loose-leaf manual
        -> distributed via the ND Publications Office (Catalogue line 170)
```

**Honest scope limit:** the self-evidence above is for the **NOTIS** manuals
specifically. It is reasonable to infer ND used its own NOTIS office software for
its other manuals too, but the hardware/SINTRAN manuals in this repository do
**not** carry an explicit "produced with NOTIS" statement, and the exact
camera-ready output device (daisy-wheel vs phototypesetter, and for later manuals
possibly a laser printer) is **not stated** in the sources reviewed. So: "NOTIS
was used" = verified for the NOTIS manuals; "laser or typeset" = it was
computer-formatted (NOTIS-TF) camera-ready copy, most plausibly daisy-wheel/photo
-typeset, with no evidence of laser printing and no evidence of the separate
NORTEXT phototypesetting path.

---

## 6. Honest gaps (not in the manuals reviewed)

- The **byte-level escape sequences** inside `WP-PRINTERS:SYMB`/`:TEXT` for each
  printer model are not reproduced in any manual here.
- The **exact SINTRAN spooler API call** NOTIS-WP uses to hand a file to the
  queue is not documented in the WP manuals (only the behaviour + error 245B).
  The SINTRAN side of that is carved in
  [../../SINTRAN/Print/07a-CARVED-INTERNALS-FINDINGS.md](../../SINTRAN/Print/07a-CARVED-INTERNALS-FINDINGS.md).
- No **proportional-spacing, italic, multi-column, automatic-hyphenation, or
  laser/phototypeset** capability is documented for NOTIS-WP/TF; the closest is
  the `^BT` "photo-set level" note and the printer-dependent `^SC`.

**Primary sources** (all in this folder unless noted): `README.md`,
`ND-63.001.02 Introduction to NOTIS-WP.md`,
`ND-63.002.02 NOTIS-WP Reference Manual - Editor.md`,
`ND-99.006.3 EN NOTIS-WP REFERENCE CARD for ND-NOTIS terminals.md`,
`ND-63.007.01 NOTIS-TF Text Formatter Reference Manual.md`,
`ND-63.009.01 NOTIS-TF Macro Guide.md`,
`ND-63.041.1_NO_NOTIS-TF-M_Nye_funksjoner_October_1985.md`,
`10079K_NOTIS-WP.md`, `210079N_NOTIS-WP_for_ND-100.md`, `ND-10079G NOTIS-1.md`.
