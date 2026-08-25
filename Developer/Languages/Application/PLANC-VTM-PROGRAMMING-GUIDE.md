# Programming VTM from PLANC

**How screen programs work on SINTRAN III: what VTM is, what you need, how the terminal type
is found and what to do when it is unset or a printing terminal, the callable surface, and a
worked program that has been compiled and run on a real machine.**

**Status of this page.** Everything here is one of three things, and each is labelled:

| Mark | Meaning |
|---|---|
| **MEASURED** | Watched happening on D100 (SINTRAN III VSX/500 K), 2026-08-24 |
| **DECODED** | Read byte-for-byte out of a real file - a library, a link map, a vendor source |
| **UNVERIFIED** | Stated in a manual or inferred, and NOT confirmed on a machine |

**There is no VTM programming manual.** ND lists VTM with manual number "Internal" in the
library list of `ND-20034-1-EN`, and none has been found in this repo or in the 1102-floppy
software archive. This page was built from the machine, the libraries and a vendor caller
instead. Where the call-level detail simply is not knowable from those, it says so rather than
inventing a signature.

---

## 1. What VTM is, and why it exists

**VTM is SINTRAN's terminal-independence layer.** Every terminal model of the era spoke its own
escape-sequence dialect - a Tandberg TDV, a DEC VT100, a Facit, a Nokia display - and they agree
on almost nothing. VTM is the layer that knows all of them, so your program does not have to.

```
   your program
        |                        asks for "cursor to row 5, column 12"
        v
   VTM                           looks up THIS terminal's dialect
        |                        and emits its actual escape bytes
        v
   DDBTABLES:VTM                 the table of every terminal type's codes
        |
        v
   the physical terminal
```

**Why you want it.** A program that writes its own escape sequences works on the one terminal it
was tested against and quietly draws rubbish on the next. VTM removes that: the terminal type is
set once, per terminal, by whoever administers the machine, and every VTM program on the system
is correct on every terminal at once.

> **"The file DDBTABLES:VTM contains terminal dependent codes and is used by screen-oriented
> applications."**
> - *ND-30.003.7 EN SINTRAN III System Supervisor*, section 6.4.1

**Everything screen-oriented on SINTRAN sits on it** - NOTIS, FOCUS, UNIQUE, COBOL's screen
handling and PLANC-SCREEN-H. They are all layers over this same one file.
See [PLANC-VTM-UI-CATALOG.md](../../Workflow/PLANC-VTM-UI-CATALOG.md).

## 2. The honest shape of "using VTM from PLANC"

**No published call interface exists for VTM - that has been confirmed four ways and is permanent
(`ND-20034-1`:1248 gives its manual number as literally "Internal"). But "undocumented" is not
"unusable", and this page's advice changed on 2026-08-25 when the direct calls were measured:**

| Route | What you call | Documented? | Proven here? |
|---|---|---|---|
| **PLANC-SCREEN-H** over VTM | `frame`, `bytdis`, `intacc`, ... | interface file is real and complete | **YES - compiled and RUN on D100** |
| **VTM directly, for what SCREEN-H has no verb for** | `VTDSCR`, `VTCREC`, `VTPCUR`, `VTWRIT` | no manual anywhere | **YES - compiled and RUN on D100** |

**Use BOTH, and use each for what it is good at.** PLANC-SCREEN-H is the steering wheel: it draws
boxes and edits fields, and it is the only one of the two with a declaration file you can
`$INCLUDE`. **But it has no window verb**, and VTM does - `VTDSCR`, the viewport. Reach past
SCREEN-H for that one thing and everything SCREEN-H draws afterwards lands inside your window
without knowing a window exists.

That is the shape TESTUI now uses and it is worth copying: SCREEN-H for the drawing, four raw VTM
calls for the geometry. Declare the raw ones yourself - `IMPORT ( ROUTINE VOID, INTEGER (...) )` -
and call the `VT*` names, not the `IVT*` wrappers, which are the FORTRAN entry points.

## 3. Prerequisites - what must be on the machine

### 3.1 The VTM library itself

**MEASURED.** A PLANC-SCREEN-H program that links only its own BRF, the screen runtime,
`MON-CALL-1B-A00` and `PLANC-1BANK-F00` compiles clean and then leaves **nine undefined
entries**:

```
Brl: LIST-ENTRIES-UNDEFINED
VTEXIT....3722 U  VTCREC...11276 U  VTWBUF...20346 U  VTPCUR...20564 U
VTWRIT...20710 U  VTBREAD..11271 U  VTINIT...21020 U  VTINFO...21021 U
VTDBUF...21026 U
```

**`DDBTABLES:VTM` does NOT satisfy those.** It is terminal DATA. The code is a separate library
and it must be linked.

**Where it comes from: there is no standalone VTM product.** The whole archive catalogue was
searched. The libraries ship with **FOCUS, `ND-10188`**:

```
volume ND-10188E-PART3   md5 028462b0f121

  VTM-R-D:BRF            28052   the routines - this is the one that matters
  VTM-DATA-D:BRF          3416   data
  VTM-1B-ARRAY-D-C:BRF    9585   1-bank terminal-type arrays
  VTM-CPOS-D:BRF           694   defines VTCPOS
  VTM-CPAR-D:BRF          1325   defines VTCPAR
  DDBTABLES-D-C:VTM       9458   a terminal table
```

**MEASURED - you need three of them, not five.** Loading `VTMCPOS` and `VTMCPAR` after `VTMR`
answers `Redefinition. First applies VTCPOS` - `VTM-R-D` already contains them. The working load
list is `VTMR`, `VTMDATA`, `VTMARR`.

**The vendor names do not fit our file transfer** (13 characters including quotes), so they are
carried as `VTMR:BRF`, `VTMDATA:BRF`, `VTMARR:BRF`.

### 3.2 The load order

**MEASURED, this exact order links with nothing undefined:**

```
@BRF-LINKER-C01
PROGRAM-FILE "MYPROG"
LOAD MYPROG                 your program first
LIBRARY-MODE ON             from here on, take only what is referenced
LOAD INTRF1B                PLANC-SCREEN-H runtime   (if you use it)
LOAD VTMR                   VTM routines
LOAD VTMDATA                VTM data
LOAD VTMARR                 VTM terminal arrays
LOAD MON-CALL-1B-A00        monitor call package, ND-210913
LOAD PLANC-1BANK-F00        the PLANC runtime, ALWAYS LAST
LIST-ENTRIES-UNDEFINED      <- READ WHAT THIS PRINTS
EXIT
```

**`LIST-ENTRIES-UNDEFINED` is not optional.** An undefined entry does **not** fail the build:
BRF-LINKER still writes a runnable `:PROG` which then misbehaves in ways that look like a bug in
your program. This is the command that told us VTM was needed at all.

### 3.3 The terminal table

`DDBTABLES:VTM` (or `DDBTABLES-n:VTM` for a version letter) must exist and be readable. On D100
there are four: `DDBTABLES-C11`, `-D11`, `-E11`, `-G06`. A missing or unreadable one is
punishing - the installation description for UNIQUE records that the application **"gets
suspended without giving any error message"** when it cannot find the file
(`ND-210731-2-EN`). No error, just a hung program.

---

## 4. The terminal type - finding it, and the two awkward cases

> **Administering terminals, rather than programming them, is skill `sintran-management`** -
> the same ground plus the SINTRAN Service Program, the three copies of SINTRAN, and where
> terminal configuration belongs in a boot chain.

### 4.1 CTYTP - what a "terminal type" really is

**It is not just a model number.** The whole thing lives in one 16-bit datafield variable,
`CTTYP` (the section heading spells it `CTYTP`; both appear in the manuals). Bits 0-7 are the
model; the bits above say what the terminal can DO.

**Bit assignment - verbatim from *ND-30.003.7*, "CTYTP - Terminal type":**

| Bit | Name | Set when |
|---|---|---|
| 15 | `ND` | bits 0-7 contain an official ND terminal type |
| 14 | `VDU` | the terminal is a VDU - a screen, not hardcopy |
| 13 | `BS` | (ASCII) Back Space is handled correctly |
| 12 | `FF` | (ASCII) Form Feed gives a new page or clears the screen |
| 11 | `CPOS` | the terminal has cursor positioning |
| 10 | `ESC` | the terminal can send (ASCII) ESC sequences on input |
| 9-8 | - | zero |
| 7-0 | type | the model number |

**MEASURED on a VT100 line, 2026-08-24:** `CTYTP` = **-5114**, which is `0xEC06` - type **6**
(DEC VT100) with `ND`, `VDU`, `BS`, `CPOS`, `ESC` set and `FF` clear. The attribute part, 60416,
is exactly `1024*(1+2+8+16+32)` - the same arithmetic the manual's own FACIT TWIST example does.
`FF` clear is right for a VT100: Form Feed there is a line feed, it does not clear the screen.

**It reads NEGATIVE because bit 15 is an attribute bit**, so any terminal with an official ND
type has bit 15 set and the word goes negative as a signed INTEGER. That is why
`@GET-TERMINAL-TYPE` prints something like `-5114` and not the small number you were told to type.

### 4.2 Reading it from a program - MEASURED

**`MON16` (MGTIY, GetTerminalType) is in the `MONn` set**, so it costs no extra library. The PLANC
manual's own appendix gives the shape:

```planc
IMPORT ( ROUTINE VOID, INTEGER (INTEGER) : MON16 )
...
MON16(1) =: ctytp                 % device 1 = your own terminal
ctytp AND 255 =: terminalType     % the model
```

Use `INTEGER4` for the masks: bit 15 is 32768 and does not fit a 16-bit `INTEGER` at all.

### 4.3 Setting it - and WHEN the attribute bits appear

```
@SET-TERMINAL-TYPE <terminal number>,<value>      also @QSET-TERMINAL-TYPE
@GET-TERMINAL-TYPE                                also @QGET-TERMINAL-TYPE
```

**A BARE MODEL NUMBER IS ENOUGH - VTM COMPLETES IT ON FIRST USE.** MEASURED 2026-08-24, and this
CORRECTS an earlier version of this page which said the opposite:

```
@SET-TERMINAL-TYPE ,6      ->  CTYTP is now 6      (bare model, attribute bits ZERO)
@TESTUI                    ->  draws correctly, and reports CTYTP -5114
@GET-TERMINAL-TYPE         ->  - 5114              (VTM wrote the bits back)
```

The manual is right: *"It is not necessary to set anything but the terminal type before running an
application using VTM. The remaining attributes will then be modified automatically."* The
derivation happens **on the first VTM call** - VTM looks the model up in `DDBTABLES:VTM`, fills in
`ND/VDU/BS/FF/CPOS/ESC` and stores the completed word.

**THE CONSEQUENCE IS IN YOUR PROGRAM, NOT THE COMMAND.** Between the `SET` and the first VTM call,
`CTYTP` is a bare `6` with every attribute bit clear - so a program that reads it in that window
sees `VDU` and `CPOS` false and concludes "hardcopy". That is a bug in the program, and it
produced a genuinely confusing symptom: setting the type to 0 and answering VTM's prompt worked,
while setting it to 6 directly did not. Same cause both times - see §4.5.

**A packed word also works** and skips the derivation: `@SET-TERMINAL-TYPE ,-5114`. A real
production boot file in this repo does that,
`Installation/Communication/TCP/x/D02-gateway-and-clients/SYSTEM/LOAD-MODE-C3.MODE` setting eight
terminals to `-5067` (= `166065B`, model 53 with attributes). Either form is fine.

**`GET-TERMINAL-TYPE` reporting a small number is not a fault** - it means no VTM program has run
on that line since the type was set.

**These commands write the MEMORY copy only**, so they must be re-run at every warm start. For
something permanent see §4.7.

### 4.4 The list VTM itself knows - MEASURED

Not from a manual. This is what a VTM program printed on D100 when it found no type:

```
Available terminal types are:

  2: Teletype ASR 33                     3: Tandberg TDV 2115
  6: DEC VT100 (80 columns)             53: Tandberg TDV 2200/9 ND-NOTIS
 80: Tandberg TDV 2200/9 ND-NET         83: Tandberg TDV 2200/9 V2 ND-NOTIS
 90: Tandberg TDV 2200/9S ND-NET        93: Tandberg TDV 2200/9S ND-NOTIS
100: Tandberg TDV 2200/9S 25 ND-NET    103: Tandberg TDV 2200/9S 25 ND-NOTIS
106: Nokia ND-Display Terminal 301     110: Tandberg TDV 1200/1 ND-NET
113: Tandberg TDV 1200/1 ND-NOTIS      121: Wordplex 80/90
131: DEC VT220 (Multinational mode)    132: DEC VT220 (National mode)
134: DEC VT100 (132 columns)           135: ANSI Standard Terminal

What is your terminal type?
```

**Answer 6 for anything VT100-ish**, a modern emulator included.

### 4.5 TYPE 0 - not selected. MEASURED, and VTM handles it FOR you

Type `0` is the dummy "nobody has said" value. **VTM asks the user itself** on the first screen
call, prints the list above, and carries on normally once answered - and answering sets the model
AND the attribute bits, which is more than `SET-TERMINAL-TYPE` with a bare number does.

**THE ONE RULE THAT COVERS BOTH AWKWARD CASES: READ `CTYTP` ONLY AFTER YOUR FIRST VTM CALL.**

Before that call the word may be zero (nobody has said) or a bare model number
(`SET-TERMINAL-TYPE`, not yet completed). **In both cases every attribute bit is clear**, so a
`VDU`/`CPOS` test reads a perfectly good screen as a printing terminal and refuses. MEASURED from
both directions. After the first VTM call the word is complete either way - VTM has prompted if it
needed to, and derived the bits regardless:

```planc
blankscreen                  % the first VTM call: prompts if needed, derives the bits
MON16(1) =: ctytp            % NOW it is worth reading
ctytp AND 255 =: model
```

`blankscreen` is the cheapest VTM call there is, and a screen program wants a cleared screen
anyway.

### 4.6 TYPE 2 - the Teletype, and every other printing terminal. MEASURED

**Do not test for type 2.** Test the bits. `VDU` clear means hardcopy; `CPOS` clear means nothing
can be positioned. Either one missing and a framed layout is meaningless - and that rule covers
the Decwriter (type 11) and anything else without naming a list that goes stale:

```planc
TRUE =: screenOk
IF (ctytp AND bitVdu)  = 0 THEN FALSE =: screenOk ENDIF
IF (ctytp AND bitCpos) = 0 THEN FALSE =: screenOk ENDIF
```

**MEASURED 2026-08-24** - the line was set to type 2 and TESTUI run:

```
@TESTUI
TESTUI needs a screen terminal.
CTYTP says this line is hardcopy, or has no cursor
positioning - so nothing has been drawn.
```

Plain readable lines, no escape sequences sprayed at a paper terminal, no hang. **That is the
whole answer to "how do I handle type 2": refuse early, in text, and draw nothing.** What VTM
would do if you ploughed on regardless is still UNVERIFIED and there is now no reason to find out.

### 4.7 Making it stick across a warm start

`SET-TERMINAL-TYPE` writes the memory copy only. SINTRAN keeps three copies - memory, image and
save-area - and **a warm start copies the IMAGE over memory**, so a memory-only change is gone.
The Service Program writes all three:

```
@SINTRAN-SERVICE-PROGRAM
*CHANGE-DATAFIELD <terminal number> INPUT YES YES YES
CTTYP/166065
.
EXIT
```

The three `YES` answers are memory, image and save-area. The value is OCTAL here and decimal at
`SET-TERMINAL-TYPE`; `166065B` = `-5067` decimal, the same word. A `.` closes each datafield.
**Input datafield only** - every worked example in the manuals sets `CTTYP` on the input side.

The System Supervisor manual ships a ready-made mode file for this, Appendix H.7,
`(UTILITY)TERMINAL-SETUP:MODE`, with one `CHANGE-DATAFIELD` stanza per terminal. Because it
writes image and save-area, it does not need re-running at every boot.

**UNVERIFIED:** none of §4.7 has been run on D100. It is quoted from the manuals and from a real
production `LOAD-MODE` in this repo, not measured here.

---

## 5. The callable surface - DECODED from the real library

`LIST-BRF-ENTRIES VTMR,,` on D100. Two families:

**`IVT*` - FORTRAN-callable integer functions.** These are the wrappers a program calls.

```
IVTINFO   IVTAINF   IVTFUNC   IVTHOLD   IVTINBT   IVTOUTB   IVTAIFC
IVTEXIT   IVTDBUF   IVTWBUF   IVTLAMP   IVTLINF   IVTATTR
```

**`VT*` - the routines underneath**, and what PLANC-SCREEN-H links against:

```
VTINIT    VTEXIT    VTWRIT    VTREAD    VTBREAD   VTINBT    VTOUTB
VTPCUR    VTCPOS    VTCPAR    VTCREC    VTCLIN    VTCEOL    VTCBOL
VTCLARE   VTCLLIN   VTCSCR    VTDSCR    VTSSYM    VTCSYM
VTATTR    VTAINF    VTINFO    VTLINF    VTLAMPS   VTFUNC    VTHOLD
VTECHM    VTBRKM    VTCAPS    VTSETD    VTDBUF    VTWBUF    VTSPBK
VTBYTES   VTMSTAC   VTINTER   VTOPSTR   VTEXRAN   VTIMPLF   VTAIFC
```

plus the echo/break mode variants, which come as a block at fixed offsets:

```
VTIECHN VTBECHN VTBECH0 VTIECH0 VTBECH1 VTIECH1 VTBECH5 VTIECH5
VT4BRKN VTBBRKN VTIBRKN VTBBRK0 VTIBRK0 VTBBRK1 VTIBRK1 VTBBRK5 VTIBRK5
```

**Reading the names** (inference from naming, not from a manual): `C` prefixes are clears -
`VTCLIN` clear line, `VTCEOL` clear to end of line, `VTCBOL` clear to beginning of line, `VTCSCR`
clear screen, `VTCLARE` clear area. `VTPCUR` positions the cursor. `VTATTR` sets attributes.
`VTECHM`/`VTBRKM` are echo and break mode. `VTLAMPS` drives the terminal's indicator lamps.

**A larger inventory - 234 symbols - is DECODED from a real link map** in this repo,
`SINTRAN/File-Formats/samples/ND-disk-00216/NOTIS-RG-SW-D10.LINK`. It exposes the internal model:
`VTVXPOS`/`VTVYPOS` virtual cursor position, `VTVXMAX`/`VTVYMAX` limits, `VTVXORG`/`VTVYORG`
origin, `VTPHYCO`/`VTPHYLI` physical columns and lines, `VTF1F01`..`VTF1F25` function keys,
`VTMON22`/`VTMN162`/`VTMN336` the monitor calls it uses underneath. Useful for understanding what
VTM models; **not** a call specification.

## 6. What is known about the calls - and where the full account lives

**This section used to be called "the three signatures that ARE known". That is out of date, and
the correction is worth reading before you write a call.**

**The full account is [VTM-API-REFERENCE.md](VTM-API-REFERENCE.md)** - 37 routines with argument
counts, which slots are strings, which are written back, and the coordinate order settled by
measurement. What follows is only the part you need to get started.

### The three facts that hold everywhere

1. **Every call returns a status as its out-value.** Take it; a discarded status is how a geometry
   mistake stays invisible.
2. **Coordinates are (LINE, POSITION) - ROW FIRST.** MEASURED on D100 2026-08-25: `VTPCUR(5,40)`
   put text on row 5 column 40. This guide previously said column-first, following VALLEY's and
   STAR-TREK's parameter NAMES; the machine disagrees with them and agrees with UNIQUE-II.
   Rectangles are `(line1, pos1, line2, pos2)` - the same order as PLANC-SCREEN-H's `frame`.
3. **`-1` means "to the end"** in a rectangle. Three vendor programs call `IVTCSCR(1,1,-1,-1)`, and
   MEASURED 2026-08-25 `VTDSCR(1,1,-1,-1)` restores the full screen the same way.

### The calls worth knowing first

| Call | Arguments | What it does |
|---|---|---|
| `VTINIT` | 1 | start VTM. **Derives the CTYTP attribute bits** and resets the screen area to full |
| `VTEXIT` | **0** | finish. Restores terminal state; does NOT clear the screen |
| `VTPCUR` | 2 | `(line, position)` |
| `VTWRIT` | 5 | `(device, mode, text, length READ WRITE, flag)`. Device 1 is your own terminal |
| `VTDSCR` | 4 | **the viewport** - see below |
| `VTCREC` | 4 | clear a rectangle, `(startLine, startPos, endLine, endPos)` |
| `VTCSCR` | 4 | clear screen region |
| `VTCLIN` `VTCEOL` `VTCBOL` | **0** | clear line / to end / to beginning. The dummy some callers pass is never touched |

**`VTWRIT`'s length argument comes back as the ACCEPTED count** - MEASURED: pass 30 into a
ten-column viewport and 10 comes back, alongside status `4113`. So a program can tell its text did
not fit without measuring anything itself.

### `VTDSCR` - the viewport, and the most useful call VTM has

```planc
IMPORT ( ROUTINE VOID, INTEGER (INTEGER, INTEGER, INTEGER, INTEGER) : VTDSCR )
```

It moves VTM's ORIGIN to a rectangle. Afterwards every coordinate - including everything
PLANC-SCREEN-H sends - is measured from that rectangle's top-left corner. **MEASURED 2026-08-25:**

- **It CLIPS.** Thirty characters into a ten-column box put exactly ten on the screen, nothing
  wrapped. A window is a box you cannot write outside of.
- **It does NOT nest.** A second `VTDSCR` replaces the first and reads its rectangle against the
  PHYSICAL screen. Always enter a window with absolute coordinates.
- **`VTDSCR(1,1,-1,-1)` gets you back out.** `VTINIT` is not the only way.

The window helpers this argues for, and the run behind every claim, are in
[VTM-VIEWPORT-HOW-TO-USE-IT.md](VTM-VIEWPORT-HOW-TO-USE-IT.md).

### Four status codes with meanings

| Status | Reads as |
|---|---|
| `0` | OK |
| `4112` | the write was refused |
| `4113` | **output truncated** - the length argument came back as what fitted |
| `4121` | **coordinate outside the area** |

**`4121` is worth remembering** because it once cost a wrong conclusion: the first `VTDSCR` probe
returned it and was written up as "VTM has no viewports". It had passed its rectangle TRANSPOSED,
and line 40 does not exist on a 24-line screen. VTM was right; the reading of it was wrong.

---

## 7. Hello world - and it has been RUN

The way to build a VTM screen program in PLANC is to link VTM and call PLANC-SCREEN-H.
Full detail: [PLANC-UI-VTM-GUIDE.md](PLANC-UI-VTM-GUIDE.md).

### 7.1 Smallest thing that works

```planc
MODULE vtmhello
$INCLUDE screen                    % PLANC-SCREEN-H interface, INSIDE the module

    IMPORT ( ROUTINE VOID, VOID (INTEGER) : MON72 )   % EESCF, enable escape

    INTEGER ARRAY : stack(0:1000)

    PROGRAM : helloVtm
        INISTACK stack
        blankscreen
        frame(1, 1, 24, 78, '')
        bytdis(12, 34, 0, 'HELLO WORLD', '')
        MON72(1)
        resetscreen
    ENDROUTINE
ENDMODULE
```

**UNVERIFIED as written** - this exact source has not been compiled. It is the working program of
7.2 with the middle removed, so every construct in it has been.

### 7.2 The real one - MEASURED, compiled and run

`SINTRAN/XMSG/TESTUI/TESTUI.PLNC` in this repo. 245 lines, 0 diagnostics, links with nothing
undefined, and on D100 it draws:

```
 1-Small window 2-Big window 3-Exit    CHOICE: .
┌────────────────────────────────────────────────────────────────────────────┐
│   ┌────────┐               ┌──────────────────────────────────────┐        │
│   │   Lorem│               │        Lorem ipsum dolor sit amet    │        │
│   │   ipsum│               │       consectetur adipiscing elit    │        │
│   │   dolor│               │             sed do eiusmod tempor    │        │
│   └────────┘               └──────────────────────────────────────┘        │
│                                HELLO WORLD                                 │
└────────────────────────────────────────────────────────────────────────────┘
```

A box round the screen, `HELLO WORLD` centred, a status line **above** the main window, and two
windows that toggle on keys 1 and 2 - the small one left aligned 4 characters in from its border,
the big one right aligned ending 5 in from its right border. Key 3 exits.

### 7.3 The four traps this program paid for - all MEASURED

**1. A PLANC LOCAL MAY NOT CARRY AN INITIAL VALUE.**

```planc
PROGRAM : mainUi
    BOOLEAN : smallOpen := FALSE     % *** ERROR - INITIAL VALUE ILLEGAL HERE
```

Declare it bare inside a routine and assign after `INISTACK`. **The build still links and runs**
with the name never set, so the symptom is a key that quietly does nothing.
`tools/planc-lint.py` now catches this.

**2. The "0 DIAGNOSTICS" you can see is the SECOND pass.** It sits directly under a COMPILE that
had errors. Read the line immediately under `*COMPILE`, or fetch the `:LIST` file. This cost a
wrong diagnosis on top of the wrong build.

**3. `intacc` wants an `INTEGER4`.** A plain 16-bit `INTEGER` compiles and then a double word is
written into a single one.

**4. `resetscreen` does NOT blank the screen.** MEASURED on exit: the SINTRAN prompt came back
over a screen still full of the program's drawing. It restores terminal state; it is not
`blankscreen`.

**The tidy exit takes three calls, and it is what SINTRAN's own screen programs do** - `S3-CONFIG`
clears the screen and leaves one line behind:

```planc
        blankscreen                                    % clears - resetscreen does NOT
        MON72(1)                                       % EESCF, escape handling back on
        resetscreen                                    % terminal state back
        OUTPUT(1, 'AL', '- EXIT from MYPROG -$')        % plain text, AFTER the reset
```

MEASURED - the result is a cleared screen, the line, then the `@` prompt.

**5. A one-character menu field needs `AUTO-SKIP`, or the user must press RETURN too.** `intacc`
is a FIELD EDITOR: RETURN is what ends a field. MEASURED - with `'MUST,PROMPT'` a menu key took
two keystrokes; with `'MUST,PROMPT,AUTO-SKIP'` the field completes as soon as it is full and one
key is enough:

```planc
intacc(1, 48, 1, choice, 'MUST,PROMPT,AUTO-SKIP')
```

### 7.4 Building it

```
@MODE TESTUI:MODE,,
```

`SINTRAN/XMSG/TESTUI/TESTUI.MODE` carries the whole recipe including the VTM load list. Then:

1. **Read `LIST-ENTRIES-UNDEFINED`.** Anything printed is a library missing or in the wrong order.
2. **Check the listing** has no `***` line **and reaches the last source line** - a compile that
   stopped two thirds of the way through reports no errors for the third it never read.
3. `@TESTUI`

---

## 8. What is still not known

- **Argument MEANINGS.** Argument COUNTS are now derived for all 37 routines from
  `COBOL-85-LIB.NRF` (see [VTM-API-REFERENCE.md](VTM-API-REFERENCE.md) section 13b), along with
  which slots are strings and which are written back. What each argument SIGNIFIES is still open
  for most of them - `VTWRIT`'s mode values 1, 2, 3 and 4 all appear in real callers and none is
  explained.
- **Terminal type 2 behaviour has never been exercised** (section 4.5). Everything said about
  printing terminals is from the manual, not the machine.
- **`VTINFO`'s parameters** - 2 arguments, the first being the array - which is the call you would
  want for "what kind of terminal is this". `MON16` answers the same question and its shape IS
  known, which is why this program uses it.
- **Whether `VTDSCR` validates a rectangle** that runs off the screen or has its corners the wrong
  way round. Every rectangle in the 2026-08-25 probe was legal.
- **Whether `VTM-1B-ARRAY` is actually required** - it loads and reports a redefinition of
  `VTI4DDB`, so it is at least partly redundant with `VTMR`. It was kept because the link is
  clean with it.
- **No VTM programming manual exists.** Confirmed against this repo and the full software
  archive. This is a permanent gap, not something to go looking for again.

---

## See also

- [PLANC-INTERACTIVE-SCREEN-PATTERNS.md](PLANC-INTERACTIVE-SCREEN-PATTERNS.md) - the patterns
  for an interactive program, once the prerequisites on this page are in place

- [VTM-API-REFERENCE.md](VTM-API-REFERENCE.md) - all 37 routines, argument counts, status codes
- [VTM-VIEWPORT-HOW-TO-USE-IT.md](VTM-VIEWPORT-HOW-TO-USE-IT.md) - what the viewport lets you BUILD
- [PLANC-UI-VTM-GUIDE.md](PLANC-UI-VTM-GUIDE.md) - the PLANC-SCREEN-H call interface in full
- [../../Workflow/VTM-TERMINAL-INTERFACES.md](../../Workflow/VTM-TERMINAL-INTERFACES.md) - terminal types, `VTM-COMPOUND`
- [../../Workflow/PLANC-VTM-UI-CATALOG.md](../../Workflow/PLANC-VTM-UI-CATALOG.md) - every screen system on ND hardware
- `SINTRAN/XMSG/TESTUI/` - the working program, its build file and its deploy scripts
- skill `planc` section 10 - the short form
