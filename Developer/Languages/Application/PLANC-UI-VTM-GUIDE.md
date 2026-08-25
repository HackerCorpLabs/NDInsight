# PLANC UI Guide — Screen Handling on VTM

**How to draw boxes, bars, and editable fields from PLANC, on top of SINTRAN's VTM
terminal-independence layer.**

**Status: PROVEN ON A REAL MACHINE, 2026-08-24.** A PLANC program written against this interface
compiles (245 lines, 0 diagnostics), links with no undefined entries and RUNS on D100, drawing a
framed screen with centred text and two windows that toggle. So the parameter orders below are no
longer a reading of a vendor caller - they are what a working program does.

Every call, source line and file shown here is byte-verified from the real decoded floppy
(`PLANC-SCREEN-H`). Nothing is invented. Where a gap remains it says so.

**Two things this page used to leave open, both now answered:**

- **A VTM library MUST be linked.** Without it the link leaves nine `VT*` entries undefined.
  `DDBTABLES:VTM` is terminal DATA and defines none of them. The libraries ship with FOCUS
  (`ND-10188`); the working load list is `VTMR`, `VTMDATA`, `VTMARR`. Full detail, plus terminal
  types and the raw VTM surface: [PLANC-VTM-PROGRAMMING-GUIDE.md](PLANC-VTM-PROGRAMMING-GUIDE.md).
- **`frame`'s parameter order**, previously "a working hypothesis", is confirmed by a program that
  draws correctly with it.

Working program, build file and deploy scripts: `SINTRAN/XMSG/TESTUI/`.

---

## 1. The three layers, in one picture

```
  Your PLANC program
        |
        v
  PLANC-SCREEN-H  <- this guide: frame/fullbar/sparsebar, field display+edit, .PICT files
        |
        v
  VTM              <- terminal-independence: DDBTABLES:VTM, terminal types, escape sequences
        |
        v
  Physical terminal (Tandberg TDV, DEC VT100/VT200, Facit, ...)
```

VTM itself has **no published call-level manual** — it is listed "Internal" in the ND
library/resource list (`ND-20034-1-EN`). You do not call VTM directly. **PLANC-SCREEN-H is the
real, documented way a PLANC program draws a screen** — it is a small library built on top of VTM
that a real demo program confirms must be loaded alongside it:

> `Vtm, mon-call-lib, planc-lib must be loaded together with this program`
> — verbatim header comment, `DEMO-SCREEN:SYMB`

For how VTM itself is configured (terminal types, `DDBTABLES:VTM`, the `VTM-COMPOUND` tool), see
[VTM-TERMINAL-INTERFACES.md](../../Workflow/VTM-TERMINAL-INTERFACES.md) — you generally do not
need to touch that layer to write a PLANC screen program; the terminal type is set once per
terminal and PLANC-SCREEN-H/VTM handle the rest.

### One thing VTM does NOT handle for you: WHEN you read the terminal type

A screen program that wants to know what it is drawing on reads `CTYTP` with `MON16`, and there is
exactly one rule:

**NEVER READ `CTYTP` BEFORE YOUR FIRST VTM CALL.**

MEASURED on D100 2026-08-24, from both directions. Before that first call the word may be

- **zero** - nobody has set a type; or
- **a bare model number** - `@SET-TERMINAL-TYPE ,6` stores just `6`.

**In both cases every attribute bit is clear**, so a `VDU`/`CPOS` test reads a perfectly good
VT100 as a hardcopy terminal and refuses to draw. After the first VTM call the word is complete
either way: VTM prompts the user if it must, derives `ND/VDU/BS/FF/CPOS/ESC` from the model
regardless, and writes the finished word back - set `6`, run the program, and
`@GET-TERMINAL-TYPE` afterwards reports `-5114`.

```planc
blankscreen                  % first VTM call: prompts if needed, DERIVES the bits
MON16(1) =: ctytp            % only now is it worth reading
ctytp AND 255 =: model
```

`blankscreen` is the cheapest VTM call and a screen program wants a cleared screen anyway. The
symptom when you get this wrong is memorable: setting the terminal type to 0 and answering VTM's
prompt works, while setting it directly to 6 does not - and neither has anything to do with the
command.

Deciding whether to draw at all belongs to the bits, never to a type number:

```planc
IF (ctytp AND 16384) = 0 THEN ... ENDIF    % VDU  clear -> paper, no screen
IF (ctytp AND 2048)  = 0 THEN ... ENDIF    % CPOS clear -> cannot position anything
```

Use `INTEGER4` for those masks - bit 15 is 32768 and does not fit a 16-bit `INTEGER`.

**VTM's own API underneath** - 37 routines including `VTDSCR`, which really is a viewport - is
[VTM-API-REFERENCE.md](VTM-API-REFERENCE.md). PLANC-SCREEN-H uses only nine of them.

**Administering terminals** - making a type stick across a boot, the Service Program, `CTYTP` in
full - is skill `sintran-management` and
[PLANC-VTM-PROGRAMMING-GUIDE.md](PLANC-VTM-PROGRAMMING-GUIDE.md) §4.

## 2. Source: the `PLANC-SCREEN-H` product

**Status:** IN-PROGRESS. No ND article number found on the floppy or in any catalogue searched;
"PLANC-SCREEN-H" is the floppy's own volume label. Full provenance and open items:
[Installation/Software/ND-PLANC-SCREEN-H/README.md](../../../Installation/Software/ND-PLANC-SCREEN-H/README.md).

Files on the floppy (`ND-PLANC-SCREEN-H`):

| File | Contents |
|---|---|
| `SCREEN:SYMB` | PLANC `IMPORT` declarations — the callable interface, decoded in full below |
| `INTERF:NRF` | compiled interface library (ND-500 loader format) |
| `INTERF-1B:BRF` | 1-bank runtime — see [TWO-BANK-PROGRAMS.md](../../Workflow/TWO-BANK-PROGRAMS.md) |
| `INTERF-2B:BRF` | 2-bank runtime |
| `DEMO-SCREEN:SYMB` | real PLANC demo source using the interface |
| `SUM:PICT` | a real `.PICT` screen-picture definition, decoded in full below |
| `SUM:SYMB` | PLANC source for the SUM demo — **not opened yet** |
| `PLANC-GEN-A00:PROG` | compiled program, likely a picture-file generator — **not decoded** |

## 3. The callable interface — `SCREEN:SYMB`, verbatim

```planc
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,BYTES,BYTES):bytdis)
IMPORT (ROUTINE VOID,BYTES(INTEGER,INTEGER,INTEGER,BYTES READ WRITE,BYTES):bytacc)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER4,BYTES):intdis)
IMPORT (ROUTINE VOID,BYTES(INTEGER,INTEGER,INTEGER,INTEGER4 READ WRITE,BYTES):intacc)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,BYTE,INTEGER,INTEGER,REAL8,BYTES):realdis)
IMPORT (ROUTINE VOID,BYTES(INTEGER,INTEGER,INTEGER,REAL8 READ WRITE,BYTES):realacc)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER,BYTES):frame)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER):fullbar)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER):sparsebar)
IMPORT (ROUTINE VOID,VOID:blankscreen)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER):blankarea)
IMPORT (ROUTINE VOID,VOID:resetscreen)
```

Source: byte-for-byte decode of the real file (`byte & 0x7F`), 926 bytes, in full.

### 3.1 Drawing boxes: `frame`

```planc
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER,BYTES):frame)
```

**`frame(row, column, height, width, attributes)` — CONFIRMED.** Height counts rows INCLUSIVE
from the start row, so `frame(1,1,24,78,'')` is a full-screen box. Established twice over: the
vendor's own `DEMO-SCREEN:SYMB` calls it that way throughout, and a program using it draws the
frame it asks for on a real terminal.

**`SPACE-FILL` in the attributes blanks the frame's interior as it draws** — that is how a window
sits on top of whatever was underneath it, and it is the vendor's own idiom for overlapping
frames.

**WATCH THE ODD ONE OUT: `blankarea` takes CORNERS, not row/col/height/width.**
`blankarea(row1, row2, col1, col2)` — the demo's `blankarea(22,22,1,80)` clears line 22 right
across the screen, which is only possible as two rows then two columns. Four integers on both
calls, different meanings, and they sit next to each other in any window-drawing routine.

### 3.2 Bars: `fullbar` / `sparsebar`

```planc
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER):fullbar)
IMPORT (ROUTINE VOID,VOID(INTEGER,INTEGER,INTEGER,INTEGER):sparsebar)
```

Same four-`INTEGER` shape as `frame` minus the trailing `BYTES` — consistent with a
`(row, column, height, width)` box with no title, filled solid (`fullbar`) or with a sparse/
dashed fill (`sparsebar`). Likely used for progress-bar-style or highlight-bar UI elements, not
confirmed further.

### 3.3 Field display and edit: the `dis`/`acc` pairs

Three data types, each with a display-only routine and a display-and-edit routine:

| Type | Display only | Display + edit |
|---|---|---|
| Byte string | `bytdis` | `bytacc` |
| Integer (`INTEGER4`) | `intdis` | `intacc` |
| Real (`REAL8`) | `realdis` | `realacc` |

`dis` routines take the value by plain (read) parameter and just paint it on screen. `acc`
routines take the value `READ WRITE` — they paint it, then let the user type over it, and return
whatever the user entered. This is the field-level building block the `.PICT` `%ATTRIBUTES`
section (§5.3) wires up via `control add` and similar flags.

### 3.4 Screen clearing: `blankscreen` / `blankarea` / `resetscreen`

- `blankscreen` — no parameters, clears the whole screen.
- **`blankarea(row1, row2, col1, col2)` — CORNERS, not row/col/height/width.** See the warning in
  §3.1; this is the one call in the library that does not take the same shape as `frame`.
- **`resetscreen` — no parameters, and MEASURED 2026-08-24 it does NOT clear the screen.** On exit
  the SINTRAN prompt came back on top of the program's drawing, the `@` merely overwriting the
  top-left corner of the frame. It restores terminal STATE, not content. **Call `blankscreen`
  before it** if the user should be returned to a clean screen.

## 3.4b What the attribute keywords MEAN - ND documented them, in the COBOL manual

The attribute vocabulary decoded out of `INTERF-1B:BRF` had no explanation attached. It turns out
**ND documented the same vocabulary in the COBOL Reference Manual** - COBOL-85's runtime is linked
against VTM and its `ACCEPT`/`DISPLAY` screen options are the identical set of keywords. So the
meanings below are ND's own words, from
`Reference-Manuals/ND-60.144.3 EN COBOL Reference Manual.md` sections 6.6.1-6.6.1.6, not
inference.

**Field painting and entry control** - these belong on `bytacc` / `intacc`:

| Keyword | ND's description |
|---|---|
| `PROMPT` | "sets the data input field on the screen to indicate that all positions contain the period character ('.') before input is accepted" - that is the `.` you see in TESTUI's CHOICE field |
| `UPDATE` | "will initialize the data input field with the initial contents of the receiving field". Can be combined with `PROMPT` |
| `SPACE-FILL` | pads the field; on a `frame` it erases the interior |
| `MUST` | the field must be filled in |
| `AUTO-SKIP` | completes the field when it is full - **this is what gives single-keystroke menus** |
| `LENGTH-CHECK` | length validation |
| `INVISIBLE` | "will prevent the data entered into the input field from being displayed... such as when typing passwords" |
| `UPPER-CASE` | automatic conversion to capitals |
| `JUSTIFIED-RIGHT` | right justification within the field |
| `BLANK-WHEN-ZERO` | a zero value displays as blank |
| `BEEP` | "will sound the terminal's audio alarm when the system is ready to ACCEPT the field" |

**Display attributes** - on any `dis` or `acc` call:

`INVERSE-VIDEO`, `BLINK`, `UNDERLINE`, `LOW-INTENSITY`, and `NORMAL` which "resets the effect of a
previous INVERSE-VIDEO, LOW-INTENSITY, BLINK or UNDERLINE".

**Frame options** - and COBOL's `DISPLAY ... FRAME` is plainly the same primitive as
PLANC-SCREEN-H's `frame`, described in ND's own words:

| Keyword | ND's description |
|---|---|
| `SPACE-FILL` | "erases the interior of the frame" |
| `HEADING` | "makes COBOL draw a line segment across the third line inside the frame, thus making room for a headline at the second line of the frame" |
| `REMARKS` | "leaves room for a remark at the second line from the bottom line of the frame, with a line across the frame above the remark" |
| `AUTO-ERASE` | "erases the frame (with contents) automatically upon the following ACCEPT" |

**`AUTO-ERASE` is a genuine pop-up mechanism** - up to 16 fields or frames marked with it
"will disappear automatically" when the next input begins. That is a deferred-erase list, and it
is the nearest thing in the whole ND screen stack to automatic window teardown. **It has NOT been
tested from PLANC**, but the keyword is present in `INTERF-1B:BRF`'s vocabulary.

**And COBOL's frame geometry confirms `frame`'s parameter order** independently: "The specified
position is taken to be the upper left corner of a frame of the size given after the FRAME
phrase. The first number after FRAME gives the number of lines down... The second number gives
the number of columns" - position first, then height, then width.

## 3.5 Windows - use VTM's VIEWPORT, and let it do the arithmetic

**This section was rewritten on 2026-08-25 after the whole thing was measured on D100.** The
earlier advice - draw at absolute screen coordinates and hide the corners conversion in a helper -
worked, but it was the hard way round. VTM has a viewport. Use it.

**MEASURED, and the full run is in
[VTM-VIEWPORT-HOW-TO-USE-IT.md](VTM-VIEWPORT-HOW-TO-USE-IT.md).**

### What the viewport does

`VTDSCR(line1, pos1, line2, pos2)` moves VTM's ORIGIN to that rectangle. After it, every
coordinate - `VTPCUR`, `VTCREC`, and through PLANC-SCREEN-H also `frame`, `bytdis`, `blankarea`
and `blankscreen` - is measured from the rectangle's top-left corner instead of the screen's.

**And it CLIPS.** Thirty characters written at position 1,1 into a box ten columns wide put
**exactly ten** on the screen and nothing wrapped to the next row. A window is a box you cannot
write outside of, however long the string. That is worth more than the convenience.

**It does NOT nest.** A second `VTDSCR` REPLACES the first and reads its rectangle against the
PHYSICAL screen. So a window always enters with absolute coordinates and can never open a
sub-window relative to itself.

### The three helpers - copy these

```planc
    IMPORT ( ROUTINE VOID, INTEGER (INTEGER, INTEGER, INTEGER, INTEGER) : VTDSCR )
    IMPORT ( ROUTINE VOID, INTEGER (INTEGER, INTEGER, INTEGER, INTEGER) : VTCREC )

    % Move INTO a window. row/col/HEIGHT/WIDTH, the same convention frame uses,
    % so the CORNERS conversion is written here and nowhere else.
    ROUTINE VOID, VOID (INTEGER, INTEGER, INTEGER, INTEGER) : winEnter(row, col, high, wide)
        INTEGER : st
        VTDSCR(row, col, row + high - 1, col + wide - 1) =: st
    ENDROUTINE

    % Move back OUT to the whole screen. -1 means "to the end", the same
    % convention IVTCSCR(1,1,-1,-1) uses in three vendor programs. MEASURED:
    % issued from inside a 2x5 box, a write at 24,1 afterwards landed on
    % physical row 24 column 1. VTINIT is NOT the only way back - which is what
    % makes windows usable, because a program can leave one and still draw its
    % own status line.
    ROUTINE VOID, VOID : winLeave
        INTEGER : st
        VTDSCR(1, 1, -1, -1) =: st
    ENDROUTINE

    % Open a window and STAY INSIDE IT. SPACE-FILL blanks the interior as the
    % frame is drawn, so opening needs NO repaint of anything else.
    ROUTINE VOID, VOID (INTEGER, INTEGER, INTEGER, INTEGER, BYTES) : openWindow(row, col, high, wide, title)
        winEnter(row, col, high, wide)
        frame(1, 1, high, wide, title)
    ENDROUTINE
```

Closing one loses its corners entirely, because from inside the window the rectangle to clear is
simply the whole of it:

```planc
    ROUTINE VOID, VOID (INTEGER, INTEGER, INTEGER, INTEGER) : clearWindow(row, col, high, wide)
        INTEGER : st
        winEnter(row, col, high, wide)
        VTCREC(1, 1, high, wide) =: st
        winLeave
    ENDROUTINE
```

**`VTCREC` is viewport-relative too - MEASURED:** `CCCCC` written at box position 2,1 followed by
`VTCREC(2,2,2,3)` left `C`s on physical columns 40, 43 and 44, so it cleared box columns 2 and 3
exactly. Physical interpretation would have cleared something at the top of the screen.

### What this buys you, in real code

`SINTRAN/XMSG/TESTUI/TESTUI.PLNC` drew a right-aligned window like this:

```planc
        bytdis( 5, 39, 0, 'Lorem ipsum dolor sit amet',  '')   % 26
        bytdis( 6, 38, 0, 'consectetur adipiscing elit', '')   % 27
        ...                                                    % ...eighteen of them
```

Eighteen start columns, each one `65 - length`, each with the length in a comment because nothing
could check it - and every one of those numbers encoded WHERE THE WINDOW WAS. It now reads:

```planc
        openWindow(bigRow, bigCol, bigHigh, bigWide, 'SPACE-FILL')
        bigLine( 2, 'Lorem ipsum dolor sit amet')
        bigLine( 3, 'consectetur adipiscing elit')
        ...
        winLeave
```

with no column and no length anywhere, because `bigLine` asks the text how long it is:

```planc
    % The window's right border is local column bigWide and every line ends five
    % characters in from it, so a line of L characters starts at bigWide-4-L.
    % L is ASKED FOR, not typed: MAXINDEX returns an array's declared upper
    % bound, and a PLANC string literal subscripts from ZERO.
    ROUTINE VOID, VOID (INTEGER, BYTES) : bigLine(row, text)
        bytdis(row, bigWide - 5 - MAXINDEX(text, 1), 0, text, '')
    ENDROUTINE
```

**MEASURED on the screen: all eighteen lines end on physical column 64**, five in from the border
at 69 - the original specification, now derived instead of counted. Had `MAXINDEX` been wrong they
would have come out ragged, which is a check anyone can make by looking.

`MAXINDEX` is a PLANC **standard routine** and needs no `IMPORT` (ND-60.117.5 section 3.17, page
153). One restriction, page 249: it is not available on array parameters inside a `STANDARD`
routine - the FORTRAN/COBOL calling sequence. An ordinary PLANC routine is fine.

### What VTM still will NOT do for you

**Opening a window needs no repaint** - `SPACE-FILL` covers what was underneath. **Closing one
still does**, because VTM saves nothing under a window and has no z-order. The viewport changes
how you address the repaint, not whether you need it.

A full repaint is the lazy answer and it costs real time on real hardware - a 24x80 screen is about
2000 characters, roughly two seconds at 9600 baud, against about 100 for a window. On an emulator
you do not feel it; on the machine these programs were written for, you do.

If the layout is small and fixed, work the overlaps out once. **MEASURED and working** in TESTUI: a
small window at rows 4-13 columns 5-14 and a big one at rows 4-23 columns 30-69 sit over a block of
text in columns 4-62, drawn in TWO column groups so each close repaints only its own:

| Closing | Clears | Restores |
|---|---|---|
| small window | its rectangle | columns 4-18 only - the big window starts at 30 |
| big window | its rectangle | columns 20+ and the centred text - columns 4-18 were never covered |

Get that split wrong and closing one window paints over the other. **Splitting the drawing by
column group is what makes the restore safe**, and it is worth doing before the layout grows.

When the overlaps stop being knowable by hand you need a z-order and dirty rectangles - a small
window manager. VTM gives you the viewport and the rectangle clear; it does not give you one, and
neither does PLANC-SCREEN-H.

### Two rules for the moment there are THREE windows

Measured on 2026-08-25, when a scrolling window was added to TESTUI and both of these turned out
to be wrong first time:

1. **CLIP WHAT YOU PAINT - DO NOT SKIP IT.** A covered window must not paint through the window
   on top, but "a covered window paints nothing" is the wrong cure: a window covering two rows of
   yours freezes all ten. Ask each cell whether it is visible and emit the runs that survive. A
   test per character costs nothing; what it saves is the wire.
2. **EVERY CLOSE REPAINTS THE STACK BOTTOM UP.** Not "put back the window I overlapped" - that
   works with two windows and breaks with three, because closing the top-left window while the
   other two are open repaints the bottom one straight over the middle one.
3. **EVERY OPEN PUTS BACK THE WINDOWS ABOVE IT.** Opening paints over whatever was there, higher
   windows included. Opening the TOP window costs nothing, so this is cheap.

Full account, with the overlap table: [VTM-VIEWPORT-HOW-TO-USE-IT.md](VTM-VIEWPORT-HOW-TO-USE-IT.md)
section 6b.

### Anything that draws WITHOUT a keystroke needs `VTWBUF`

**MEASURED 2026-08-25, and it is the nastiest trap on this page** because it does not look like a
display problem. **VTM buffers its output and a terminal READ is what flushes it.** Every
PLANC-SCREEN-H program in this guide so far redraws and then calls `intacc`, so the read flushes
the repaint and nobody ever sees the buffer.

Put a timer in - or anything that draws because a message arrived rather than because a key was
pressed - and the drawing goes into the buffer and stays there. **The symptom is that your timer
appears to have stopped.** In TESTUI one keypress made three lines appear at once, two of them
timestamped seconds earlier: drawn on time, shown late.

```planc
IMPORT ( ROUTINE VOID, INTEGER : VTWBUF )     % NO arguments
...
VTWBUF =: st
```

### `bytdis` with a SUBARRAY needs an explicit width

Everywhere in this guide `bytdis` is called with a width of 0, "use the string's own length". That
is right for a LITERAL and **wrong for a subarray of a larger buffer**: MEASURED 2026-08-25, a
70-character subarray of a 700-byte buffer painted with width 0 ran clean off the right-hand side
and wiped two window borders it could not otherwise have reached. Pass the field width:

```planc
bytdis(row, col, scrTextWide, scrBuf(base : base + scrTextWide - 1), '')
```

Note also what that says about the viewport: **the overflow was not stopped at the viewport edge**,
although `VTWRIT` demonstrably is. The clipping measured for `VTWRIT` must not be assumed for
PLANC-SCREEN-H.

## 4. Building and linking a screen program

From the demo header comment, three things must load together with your program:

```
Vtm, mon-call-lib, planc-lib must be loaded together with this program
```

**"Vtm" there means a real LIBRARY, not the `DDBTABLES:VTM` data file** — without it the link
leaves nine `VT*` entries undefined. MEASURED, and this exact list links clean and runs:

```
@BRF-LINKER-C01
PROGRAM-FILE "MYPROG"
LOAD MYPROG                 your program first
LIBRARY-MODE ON
LOAD INTRF1B                PLANC-SCREEN-H 1-bank runtime
LOAD VTMR                   VTM routines - supplies eight of the nine entries
LOAD VTMDATA
LOAD VTMARR                 reports "Redefinition ... VTI4DDB", harmless
LOAD MON-CALL-1B-A00        ND-210913
LOAD PLANC-1BANK-F00        the PLANC runtime, ALWAYS LAST
LIST-ENTRIES-UNDEFINED      prints NOTHING when it is right
EXIT
```

`VTMCPOS` and `VTMCPAR` are NOT needed — `VTM-R-D` already defines `VTCPOS` and `VTCPAR`. The VTM
libraries come off the FOCUS floppy `ND-10188`; see
[PLANC-VTM-PROGRAMMING-GUIDE.md](PLANC-VTM-PROGRAMMING-GUIDE.md) §3.

**`LIST-ENTRIES-UNDEFINED` is not optional** — an undefined entry does not fail the build, so
BRF-LINKER writes a runnable `:PROG` that then misbehaves like a bug in your program.

Older, superseded note (adapted from the demo and from
[LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md) — the exact `NRL`/`NRL500` command line for
this specific library has not been independently re-run in this repo, so treat this section as
the documented shape, not a verified transcript):

```planc
MODULE mydialog
$INCLUDE screen

    PROGRAM : main
        frame(1, 10, 8, 40, 'My Dialog')
        bytdis(2, 12, 20, 'Enter name:')
        ...
    ENDROUTINE
ENDMODULE
```

At link time, include:
- `INTERF-1B:BRF` (1-bank) or `INTERF-2B:BRF` (2-bank) — the PLANC-SCREEN-H runtime, pick per
  your program's bank model (see
  [TWO-BANK-PROGRAMS.md](../../Workflow/TWO-BANK-PROGRAMS.md)).
- the VTM terminal-table array file for your target terminal type(s), generated by
  `VTM-COMPOUND-E09` option 4 (see
  [VTM-TERMINAL-INTERFACES.md §3](../../Workflow/VTM-TERMINAL-INTERFACES.md#3-building-and-extending-terminal-tables--the-real-vtm-compound-procedure)).
- `mon-call-lib` and `planc-lib` per the demo header.

**Where these two actually come from** (traced through the source floppy archive, not just this
repo's manuals):

- **`mon-call-lib` is a real, separately-installed product**:
  [`ND-210913` "SINTRAN III Monitor Call Package"](../../../Installation/Software/ND-210913/README.md).
  Its floppy ships exactly the three format variants this catalog's convention would predict —
  `MON-CALL-1B-A00:BRF` (ND-100 1-bank), `MON-CALL-2B-A00:BRF` (ND-100 2-bank),
  `MON-CALL-LIB-A00:NRF` (ND-500) — plus `MON-CALL-NAMES-A:DATA`, the symbolic routine-name table.
  **Install procedure now verified from NDWiki** (source PD-sheet PDF pending): copy the four
  files off the floppy directory, `MON-CALL-NAMES-A:DATA` to user `SYSTEM` specifically, the other
  three to any user with public read access — full transcript in the product's own README.
- **`planc-lib` is NOT a separately installed product anywhere in the source archive.** A
  repo-wide search of the full 1066-floppy catalog turns up exactly one copy of
  `PLANC-LIB-B:NRF`, bundled inside the SIBAS-II for ND-500 floppy set
  (`ND-10340`) — not on any dedicated `planc-lib` floppy of its own. This matches the reference
  manual's own syntax, `LOAD-SEGMENT (libraries)planc-lib`, where `(libraries)` names a fixed
  system directory rather than a product a customer installs. **Best-supported reading**:
  `planc-lib` is a standard resident library expected to already exist under `(LIBRARIES)` on any
  SINTRAN system that has PLANC installed — not something with its own install procedure. This is
  inference from the evidence available, not a fact confirmed by a manual stating it outright.

## 5. The `.PICT` screen-picture format — `SUM:PICT`, verbatim

Instead of calling `frame`/`bytdis`/etc. one at a time by hand, a screen layout can be described
declaratively in a `.PICT` file and loaded as a unit. Full decoded source:

```
%HEADING
@picture sum-demo,i
%CONTROL
@position 1,35
@size 8,13
@in-frame heading remarks
@field-defaults prompt
%DEFINITIONS
@start

 *** SUM ***

 A   :  $$$$ @1
 B   :  $$$$ @2

 SUM : $$$$$ @3

@end
%ATTRIBUTES
@1 a underline
@2 b underline control add
@+    DISPLAY_FIELDS
@3 sum o low-intensity not-prompt
```

### 5.1 `%HEADING`
Names the picture: `@picture <name>,<version-letter>`.

### 5.2 `%CONTROL`
- `@position row,col` — where the picture is placed on the physical screen.
- `@size height,width` — its bounding box (this is very likely what `frame`'s height/width
  parameters ultimately come from at runtime).
- `@in-frame heading remarks` — draws a frame around the picture; `heading`/`remarks` control
  what's shown in the frame border. **Exact keyword meaning not confirmed further** — inferred
  from context, not from a manual.
- `@field-defaults prompt` — fields default to "prompt" mode (editable) unless overridden per
  field in `%ATTRIBUTES`.

### 5.3 `%DEFINITIONS`
The literal screen layout between `@start`/`@end`. Static text is typed as-is; each editable/
displayed field is a run of `$` placeholders sized to the field width, tagged `@N` to link it to
an `%ATTRIBUTES` entry.

### 5.4 `%ATTRIBUTES`
Maps each `@N` field to:
- a program variable name (`a`, `b`, `sum`),
- display attributes (`underline`, `low-intensity`, ...),
- edit mode (`not-prompt` = display-only; bare/`control add` = editable, with `control add`
  apparently marking a field that participates in a computed group),
- an optional continuation group (`@+ DISPLAY_FIELDS` groups field `@2` with a named refresh
  group — likely fields that get repainted together when one of them changes, e.g. a running sum;
  not confirmed against a manual).

This `.PICT` format is a **fourth, independently-documented UI-definition syntax** in this
catalog, alongside VTM's raw terminal-table API, NSHS's "picture" concept, and UNIQUE's
`start-form`/`start-fields` convention — see
[VTM-TERMINAL-INTERFACES.md §5](../../Workflow/VTM-TERMINAL-INTERFACES.md#5-related-products) for
how these relate (and where the relationship is still unconfirmed).

## 6. How a `.PICT` file actually gets used — `SUM:SYMB` and `PLANC-GEN-A00:PROG`, verbatim

This was the open question in the first draft of this guide. It is now settled by decoding the
real floppy image (`8_nd_f17b_planc-screen-h.img.gz`, mounted read-only, extracted with
`ndtool -x -p`). **`.PICT` is not loaded at runtime — it is compiled at build time.**

`PLANC-GEN-A00:PROG` is a real, identified tool — strings extracted from the binary:

```
ND-100/500 PLANC GENerator  Version A00
Input file :  List file  :  Result file:
First word in input file must be @PICTURE or @REPORT
R PICT   W LIST   W PGEN
```

It reads a `:PICT` file and a `:LIST` file, and **writes a `:PGEN` file — generated PLANC
source**. The strings also show it literally emits a `FRAME(...)` call plus a
`RE_DISPLAY_PICTURE` routine and `BLANK_DISPLAY`, confirming §3.1's parameter-order hypothesis for
`frame` fairly directly (the generator is the thing constructing that call from
`@position`/`@size`/`@in-frame`).

The consuming program, `SUM:SYMB`, verbatim (full decode):

```planc
MODULE xxx
$INCLUDE screen:symb
IMPORT (ROUTINE VOID,VOID (INTEGER) : mon72)
INTEGER ARRAY: stack(0:1000)
PROGRAM: exsum
   INISTACK stack
   INTEGER4: a,b,sum
   ROUTINE VOID,VOID: add
      a + b =: sum
   ENDROUTINE
$INCLUDE sum:pgen
   blankscreen
   sum_demo
   mon72(1)
ENDROUTINE
ENDMODULE
$EOF
```

**Reading it**: the picture's own name, `sum-demo` (from `@picture sum-demo,i` in `SUM:PICT`),
becomes a callable routine `sum_demo` in the generated `SUM:PGEN` file (hyphen becomes
underscore). The program just `$INCLUDE`s the generated source and calls that routine directly —
no separate loader call, no monitor call, nothing dynamic. The real build pipeline for a
`.PICT`-based screen is:

```
SUM:PICT  --[ @PLANC-GEN-A00 ]-->  SUM:PGEN   (generated PLANC source, a callable <picture-name> routine)
SUM:SYMB  $INCLUDE sum:pgen  ...  sum_demo    (call it like any other routine)
```

`SUM:PGEN` itself was not observed directly — running `PLANC-GEN-A00:PROG` to regenerate it is a
live-ND-100 action, out of scope for a static archive read. The exact generated call arguments to
`FRAME(...)` are inferred from strings in the generator binary, not from a captured `SUM:PGEN`.

## 7. What is still NOT known — do not assume past this line

- **The exact literal content of a generated `:PGEN` file** (e.g. `SUM:PGEN`) has not been
  observed — only inferred from strings inside `PLANC-GEN-A00:PROG`. Running the generator on a
  real ND-100 would settle this.
- **`frame`'s trailing `BYTES` argument** is still only inferred from the `.PICT`/generator
  strings. `SPACE-FILL` demonstrably blanks the interior, but the full keyword vocabulary is not
  verified against a disassembly of `INTERF:NRF`.
  **Its PARAMETER ORDER is no longer in doubt:** `frame(row, col, height, width, attributes)` has
  been run repeatedly, and on 2026-08-25 it was run at LOCAL coordinates inside a `VTDSCR`
  viewport - `frame(1, 1, high, wide, 'SPACE-FILL')` drew a box of exactly the right size in
  exactly the right place, which it could not do with the arguments in any other order.
- **Whether PLANC-SCREEN-H is the same product as NSHS** (`ND-10013`) remains open — see
  [PLANC-VTM-UI-CATALOG.md](../../Workflow/PLANC-VTM-UI-CATALOG.md) for the fuller comparison.
  NSHS's caller-language list (FORTRAN/BASIC/COBOL/RPG II) does not mention PLANC, which weakens
  but does not disprove the "same product" theory.
- **No ND article number or manual exists for PLANC-SCREEN-H or PLANC-GEN anywhere searched** —
  confirmed against both the repo and the source software archive's own product metadata
  (`products/PLANC-SCREEN.yaml` has no article number and empty `docs` fields). This is a real,
  permanent documentation gap, not an oversight to chase further.

---

## See Also

- [PLANC-INTERACTIVE-SCREEN-PATTERNS.md](PLANC-INTERACTIVE-SCREEN-PATTERNS.md) - **read this
  before writing a screen program that does anything without a keystroke behind it**

- [VTM-VIEWPORT-HOW-TO-USE-IT.md](VTM-VIEWPORT-HOW-TO-USE-IT.md) - `VTDSCR`, the window primitive
  PLANC-SCREEN-H has no verb for, and everything measured about it
- [VTM-API-REFERENCE.md](VTM-API-REFERENCE.md) - all 37 VTM routines underneath

- **[Installation/Software/ND-PLANC-SCREEN-H/README.md](../../../Installation/Software/ND-PLANC-SCREEN-H/README.md)** — full floppy decode and provenance.
- **[VTM-TERMINAL-INTERFACES.md](../../Workflow/VTM-TERMINAL-INTERFACES.md)** — the VTM layer underneath: terminal types, `DDBTABLES:VTM`, `VTM-COMPOUND`.
- **[PLANC-DEVELOPER-GUIDE.md](PLANC-DEVELOPER-GUIDE.md)** — general PLANC language reference.
- **[TWO-BANK-PROGRAMS.md](../../Workflow/TWO-BANK-PROGRAMS.md)** — 1-bank vs 2-bank runtime choice (`INTERF-1B:BRF` vs `INTERF-2B:BRF`).
- **[LINKING-GUIDE.md](../../Workflow/LINKING-GUIDE.md)** — general NRL/linker mechanics.
