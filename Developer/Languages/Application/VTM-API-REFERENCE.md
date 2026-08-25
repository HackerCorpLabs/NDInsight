# VTM API Reference

**The callable interface of SINTRAN III's Virtual Terminal Manager, assembled from the machine
and from surviving vendor source, because ND never published a manual for it.**

---

## Provenance, and why this page exists

**ND never shipped documentation for these routines.** That is not an assumption - it was
established by searching every manual in this repo and all 1102 floppy images in the
`norskdata-software-archive`, at both 7-bit and even parity. The library catalogue in
`Reference-Manuals/ND-20034-1-EN ND-Specific Programming & Advanced PLANC.md:1248` says it
outright:

```
| VTM     | Internal   | Virtual Terminal Manager. |
```

Every neighbouring library carries an ND-60.xxx manual number. VTM's says **Internal**. The
`SINTRAN III Reference Manual` forward-references `ND-60.151` for VTM, and `ND-60.151.3:107`
dead-ends by pointing at PI sheets for the terminal-table product. The six VTM products in the
archive are **all terminal tables**; none is a programming product. A deleted directory entry
named `VTM-CALLS:IMPT` - a PLANC import declaration, 3641 bytes - survives in free space on five
disk images, but its contents are gone.

**So every signature below comes from one of four kinds of evidence:**

| Mark | Meaning |
|---|---|
| **MEASURED** | Called on a live D100 and the result observed, 2026-08-24/25 |
| **VENDOR-DECL** | A typed declaration in surviving vendor source |
| **VENDOR-CALL** | A real call site in vendor source - argument count and order visible |
| **INFERRED** | Reasoned from the name, size or symbol neighbourhood. NOT established |

### The sources

| Source | What it gives | Where |
|---|---|---|
| `T2-SCREEN:SYMB` | **typed PLANC declarations** for 8 routines | COBOL-500 compiler source, archive image `8_nd_f17b_source_cobol-500-h03_fe.img.gz` |
| `UNIQ-SCR-VTM-A02:NRF` | FORTRAN **call sites** with argument lists | UNIQUE-II, product ND-10730, archive |
| `STAR-MAIN:SYMB`, `STAR-SUBR:SYMB` | FORTRAN call sites | STAR-TREK game, archive |
| `VALLEY-VTM:SYMB` | FORTRAN call sites | VALLEY game, archive image `NMN-VALLEY` |
| `VTM-ENTRIES.txt` | the BRF entry list **with sizes**, off the machine | `SINTRAN/XMSG/TESTUI/VTM-ENTRIES.txt` |
| `COBOL-85-LIB.NRF` | the whole VTM library with per-routine addresses | `SINTRAN/ND500-APPS/_shared/files/` |
| six `.LINK` maps | 240 VT* symbols, routine/data split | `SINTRAN/File-Formats/samples/ND-disk-002*/` |

---

## 1. THE COORDINATE RULE - get this wrong and nothing works

**Every coordinate pair is (LINE, POSITION). Row first. Rectangles are
(line1, pos1, line2, pos2).**

**MEASURED 2026-08-25.** `VTPCUR(5, 40)` followed by a write put the text on **row 5, column
40**. Column-first would have meant row 40, which does not exist on a 24-line screen.

**This contradicts two vendor programs and they are the ones that are wrong** - or rather, their
variable NAMES are. `VALLEY-VTM:SYMB` has `SUBROUTINE CURSPOS(COL,ROW)` calling
`IVTPCUR(COL1,ROW)`, and STAR-TREK has `IVTPCUR((POS+1),(LINE))`. Both read as column-first.
`UNIQ-SCR-VTM-A02` has `IVTPCUR(ILIN,IPOS)` and `IVTCREC(ISLIN,ISPOS,IELIN,IEPOS)` - line-first,
and **the machine agrees with UNIQUE**. Do not trust a caller's parameter names over a
measurement.

**This also caught out this repo's own earlier notes**, which stated `IVTPCUR(column,row)` and
`IVTCSCR(x1,y1,x2,y2)`. Both were wrong and are corrected here.

**PLANC-SCREEN-H is the opposite of a counter-example** - its `frame(row, col, height, width)` is
also row-first. It is `blankarea(row1, row2, col1, col2)` that is the odd one out, pairing the
rows together rather than interleaving.

## 2. The two layers, and which to call

| Layer | Naming | Convention | Who calls it |
|---|---|---|---|
| **`IVT*`** | `IVTPCUR` | FORTRAN/COBOL integer function | FORTRAN, COBOL-85's runtime |
| **`VT*`** | `VTPCUR` | plain PLANC routine, out-value INTEGER | PLANC, and every real ND application |

**From PLANC call the `VT*` names, not `IVT*`.** Six link maps of real ND products
(NOTIS-RG, RG-SERVICE, RG-START, OEM-STATU, HYPHEN-TEST, LED-FORTRAN) contain **231-233 `VT*`
symbols and not one `IVT*`**. The `IVT*` layer exists for languages that need the FORTRAN
sequence.

**Every routine returns an INTEGER status.** Callers take it and, in the games, mostly ignore it.
**The status vocabulary is NOT known** - one probe returned `4121` and nothing decodes it.

## 3. Session

### `VTINIT` - initialise
**VENDOR-DECL** `ROUTINE VOID,INTEGER(INTEGER) : VTINIT`
**VENDOR-CALL** `VTINIT(0)` (COBOL-500), `IVT=IVTINIT(0)` (STAR-TREK)

Call once before any other VTM routine. **MEASURED side effect:** if the terminal type is 0, the
first VTM call is where VTM prints its own type list and asks the user; and whatever the type,
VTM derives the `CTYTP` attribute bits and writes them back. It also **resets the screen area to
full** - a viewport left behind by a previous program does not survive into the next one.

### `VTEXIT` - finish
**VENDOR-DECL** `ROUTINE VOID,INTEGER : VTEXIT` - no arguments
**VENDOR-CALL** `VTEXIT; FALSE=:CB99` (COBOL-500)

**MEASURED:** does NOT clear the screen. It restores terminal state. Call `blankscreen` (or
`VTCSCR`) first if the user should be returned to a clean screen.

## 4. Positioning and writing

### `VTPCUR` - position cursor
**VENDOR-DECL** `ROUTINE VOID,INTEGER(INTEGER,INTEGER) : VTPCUR`
**MEASURED** `VTPCUR(line, position)`

### `VTWRIT` - write
**VENDOR-DECL**
`ROUTINE VOID,INTEGER(INTEGER,INTEGER,BYTES,INTEGER READ WRITE,INTEGER) : VTWRIT`
**VENDOR-CALL** `VTWRIT(P.TNO,1,TEXT,I,3)` (COBOL-500),
`IVTWRIT(IDEV,4,CSTRING,INOCH,1)` and `IVTWRIT(IDEV,2,CSTRING,ILENG,1)` (UNIQUE-II)
**MEASURED** `VTWRIT(1, 4, 'XY', len, 1)` wrote `XY` at the current cursor position.

Arguments read as: device, mode, text, length (`READ WRITE` - VTM writes back), and a trailing
flag. **Device 1 is your own terminal. The MODE values 1, 2, 3 and 4 all appear in real callers
and their meanings are NOT known.**

**THE LENGTH WRITE-BACK IS THE ACCEPTED COUNT - MEASURED 2026-08-25.** Passing 30 into a
ten-column viewport returned **10**, alongside status `4113`. So a caller can tell that its text
did not fit without measuring anything itself. That is the first VTM output argument whose
MEANING is known, not just its position.

### `VTWREP` - write repeated
**DERIVED: 6 arguments**, arg 3 a string, args 4 and 6 written back - the shape of `VTWRIT` plus
two. No caller found, so the extra two are unexplained; the name suggests a repeat count.

## 5. Reading

### `VTBREAD` - block read
**VENDOR-DECL**
```planc
ROUTINE VOID,INTEGER(INTEGER,INTEGER,BYTES,INTEGER,INTEGER,
                     INTEGER WRITE,INTEGER,INTEGER2 ARRAY,
                     INTEGER,INTEGER2 ARRAY) : VTBREAD
```
**VENDOR-CALL** `VTBREAD(P.TNO,2,BCHAR,1,3,N,0,VTBREAK,-1,VTECHO)` (COBOL-500),
`IVTBREA(IDEV,2,CCH,1,IOVFLW,I,0,I,-1,I)` (UNIQUE-II)

Ten arguments. The two `INTEGER2 ARRAY` parameters are the **break** and **echo** tables - the
COBOL source names its variables `VTBREAK` and `VTECHO`, which is what identifies them.

### `VTREAD`, `VTINBT` - read, input byte
**VENDOR-CALL** `IVTINBT(INP,2,GETCTRL,3)` (STAR-TREK) - 4 arguments.
`VTREAD` has no caller found; 220 bytes on ND-500, the largest of the read family.

**MEASURED 2026-08-25 on D100 with `SINTRAN/XMSG/SINTRAN-CHAT/KEYPROB.PLNC`.** This is the call
that reads a key AND DECODES IT, and it is how a screen program should read the keyboard - a
program must never look at escape bytes itself.

```planc
IMPORT ( ROUTINE VOID, INTEGER (INTEGER, INTEGER, INTEGER WRITE, INTEGER) : VTINBT )
...
    1 =: dev
    2 =: mode
    3 =: arg4
    0 =: ch
    VTINBT(dev, mode, ch, arg4) =: st     % ch comes back holding the key
```

**ARGUMENT 3 IS THE KEY, AND IT IS THE ONLY OUT PARAMETER.** Arguments 1, 2 and 4 go in
unchanged - 1 is the terminal, and the 2 and the 3 are STAR-TREK's, still not understood. Status
came back 0 on every one of about forty calls.

**TYPE IT AS THREE IN, ONE OUT - GETTING THAT WRONG HIDES THE DECODING.** Declared with all four
`WRITE`, PAGE UP came back as FOUR calls returning 27, 91, 53, 126 and it looked exactly as
though VTM decoded nothing. A PLANC `WRITE` parameter is passed by ADDRESS, so the by-value
arguments were handed pointers. Typed properly the same key is ONE call returning 201.

**AND PASS VARIABLES, NOT LITERALS.** `VTINBT(1, 2, ch, 3)` compiled and ran and decoded
NOTHING - PAGE UP did not register at all. The identical call with the values in variables works
every time. Not explained; just do it.

**IT DOES NOT BLOCK IF YOU ASK FIRST.** `MON66` says how many bytes are waiting; call `VTINBT`
only when there is at least one. A function key is several bytes and `VTINBT` consumes all of
them in one call, so "at least one" is enough - the caller never has to know how many.

#### The key codes, measured

**DEC VT100 (terminal type 6):**

| Key | code | confidence |
|---|---|---|
| PAGE UP | **201** | seen 3 times across 2 runs |
| PAGE DOWN | **197** | seen twice |
| cursor UP | 28 | seen twice |
| cursor DOWN | 11 | once |

**Tandberg TDV 2200/9 ND-NOTIS (terminal type 53):**

| Key | sequence the terminal sent | code | confidence |
|---|---|---|---|
| HJELP | `ESC[46_` | 191 | ONCE |
| ANGRE | `ESC[30_` | 216 | ONCE |
| F1 | `ESC[50_` | 132 | ONCE |
| F2 | `ESC[52_` | 140 | ONCE |
| F3 | `ESC[55_` | 149 | ONCE |
| F4 | `ESC[58_` | 171 | ONCE |
| F5 | `ESC[60_` | 217 | ONCE |

**THE TDV NUMBERS ARE ONE OBSERVATION EACH AND SHOULD NOT BE TRUSTED YET.** They do not form any
progression - F1..F5 step 8, 9, 22, 46 - which is either real or a sign that something else
varies. Repeat them before writing them into a program. The VT100 PAGE UP figure is solid: the
same key gave 201 twice in one run and once in another.

#### VTM DECODES ONLY THE TERMINAL IT IS CONFIGURED FOR

The decisive experiment, 2026-08-25: the VT100 PAGE UP sequence `ESC [ 5 ~` was sent to a line
set to terminal type 53. It came back as **four raw bytes, 27 91 53 126** - undecoded. The same
sequence on a line set to type 6 comes back as the single code 201.

So VTM recognises the key sequences of the terminal it has been told it is talking to, and
anything else falls through byte by byte. That is the terminal-independence contract working
exactly as intended, and it has two consequences for a program:

 - **NEVER treat a stray 27 as a key.** It may be the first byte of a sequence VTM did not
   recognise, and the rest is about to arrive as separate calls.
 - **A BARE ESC IS NOT A DEPENDABLE QUIT KEY.** On a VT100 line, pressing ESC alone did not
   produce 27 at all - CHATUI bound its exit to it and became impossible to leave, and the
   terminal had to be freed with `STOP-TERMINAL` from another session. **Use a typed command
   like `/exit`**: it needs no key code and works on every terminal.

#### WHERE THE MAPPING LIVES - NOT FOUND, and here is where it is NOT

Searched 2026-08-25 so nobody repeats it: the sequence `ESC [ 5 ~` appears in **none** of
`DDBTABLES-C11:VTM`, `DDBTABLES-D11:VTM`, `DDBTABLES-E11:VTM`, `DDBTABLES-G06:VTM`, `VTMR:BRF`
or `VTMARR:BRF`. No stored sequence in any of those tables even ends in `~`.

What the DDBTABLES DO contain is **length-prefixed OUTPUT sequences**: a count byte followed by
that many bytes starting with `1B`. At offset 5878 of G06, `05` is followed by exactly
`1B 5B 38 30 6C` - `ESC [ 8 0 l`. They also carry the VT100 line-drawing map (`j` to a corner
character), which is what `frame` uses. Whatever decodes INPUT is somewhere else.

#### The startup noise, explained

A program that takes the keyboard right after `blankscreen` reads about eight bytes of rubbish -
`63 63 128 103 0 29 63 63` - and they land in whatever the program thinks is its input. They are
**VTM's terminal-type negotiation**: on a line whose type was already set with
`SET-TERMINAL-TYPE`, the same program sees NONE of them. Drain the input before taking the
keyboard, or set the terminal type in advance.

## 6. Clearing - and the viewport

### `VTCSCR` - clear screen (region)
**VENDOR-CALL** `IVTCSCR(1,1,-1,-1)` (VALLEY, UNIQUE-II, STAR-TREK)
Four arguments, `(line1, pos1, line2, pos2)`, and **`-1` means "to the end"**.

### `VTCREC` - clear rectangle
**VENDOR-DECL** `ROUTINE VOID,INTEGER(INTEGER,INTEGER,INTEGER,INTEGER) : VTCREC`
**VENDOR-CALL** `VTCREC(P.ERLINE(N),P.ERFROM(N),P.ERLINETO(N),P.ERTO(N))` (COBOL-500),
`IVTCREC(ISLIN,ISPOS,IELIN,IEPOS)` (UNIQUE-II)

`(startLine, startPos, endLine, endPos)`. The COBOL variable names - `ERLINE`, `ERFROM`,
`ERLINETO`, `ERTO` - settle the order independently of UNIQUE.

**`VTCSCR` and `VTCREC` are both 31-word stubs on ND-100 that call the 670-word `VTCLARE`
("clear area").** They are two entry points onto one worker.

### `VTCLIN`, `VTCEOL`, `VTCBOL` - clear line / to end / to beginning
**VENDOR-CALL** `IVTCLIN()` (VALLEY), `IVTCLIN(IDUM)` (UNIQUE-II)

**RESOLVED - they take NO arguments, and both callers are right.** DERIVED from the binary: all
three are 110-byte records that fetch nothing at all. UNIQUE's `IDUM` is a dummy the wrapper never
touches, and VALLEY's argument-less `IVTCLIN()` is equally valid. See section 13b.

### `VTDSCR` - DEFINE SCREEN AREA. This is the viewport.
**MEASURED 2026-08-25, and it is the most consequential finding on this page.**

```planc
IMPORT ( ROUTINE VOID, INTEGER (INTEGER, INTEGER, INTEGER, INTEGER) : VTDSCR )
...
VTDSCR(14, 40, 19, 69)      % define rows 14-19, columns 40-69
VTPCUR(1, 1)                % ask for position 1,1
VTWRIT(1, 4, 'XY', ln, 1)   % -> lands on PHYSICAL row 14, column 40
```

**After `VTDSCR`, coordinates are RELATIVE to the rectangle.** Position 1,1 is the box's top-left
corner. This is a genuine viewport, and it constrains more than positioning: a subsequent
`blankscreen` cleared only within the area, and every later write in the program stayed inside
it. **It persists until changed**, and `VTINIT` resets it to the full screen - so it does not
leak into the next program.

**How this was missed twice.** The first probe passed `(40,14,69,19)` - the rectangle transposed,
because the coordinate order had not yet been established. It returned status `4121` and did
nothing visible, and was written up as "VTM has no viewports". That conclusion was wrong. The
lesson is the one at the top of this page: settle the coordinate order first.

**DERIVED: 4 arguments**, and its wrapper is byte-identical in structure to `VTCSCR` and
`VTCREC` - so the first failed probe had the right argument COUNT and only the wrong ORDER.

**BOTH ANSWERED, MEASURED 2026-08-25** - see
[VTM-VIEWPORT-HOW-TO-USE-IT.md](VTM-VIEWPORT-HOW-TO-USE-IT.md) for the run:

- **IT CLIPS.** Thirty characters written at 1,1 in a ten-column box put exactly TEN on the screen,
  in the box's own columns, with nothing wrapping to the next row. A viewport is a containment
  boundary, not just an offset.
- **`VTDSCR(1, 1, -1, -1)` restores the full screen**, the same "-1 means to the end" convention
  three vendor programs use with `IVTCSCR(1,1,-1,-1)`. An explicit `VTDSCR(1,1,24,80)` works too.
  `VTINIT` is not the only way back.
- **Viewports do NOT nest.** A second `VTDSCR` REPLACES the first and its rectangle is read against
  the PHYSICAL screen: from inside a rows 10-12 / columns 40-49 box, `VTDSCR(1,1,2,5)` then a write
  at 1,1 landed on physical row 1, column 1.
- **`VTCREC` is viewport-relative too**, so PLANC-SCREEN-H's `frame`, `bytdis`, `blankarea` and
  `blankscreen` all ride along without knowing a window exists.

**Still unknown:** whether `VTDSCR` validates a rectangle that runs off the screen or has its
corners reversed. Every rectangle in the probe was legal.

### `VTCLARE`, `VTCLLINE` - clear area / clear line, the workers
**INFERRED.** 670 and 346 words on ND-100. No `IVT*` wrapper, so not part of the callable API -
`VTCSCR`/`VTCREC` and `VTCLIN` reach them.

### `VTWBUF` - WRITE BUFFER. This is the FLUSH, and you will need it.

**MEASURED 2026-08-25, and nothing in any ND document says this.**

**VTM does not send each write to the terminal as it happens.** It fills a buffer and flushes when
something forces the issue - **and reading a key does.** A program driven entirely by keystrokes
never notices, because every repaint is followed by a read. **Anything on a clock does notice**,
because a timer has no read.

The symptom is brutal, because it does not look like a display fault at all: **it looks like your
timer stopped.** In TESTUI a five-second timer appeared dead, and pressing SPACE once made THREE
lines appear at the same instant - two carrying timestamps from seconds earlier, plus the one the
keypress caused. **Lines that arrive together but are stamped five seconds apart were drawn on
time and shown late.**

```planc
IMPORT ( ROUTINE VOID, INTEGER : VTWBUF )        % no arguments
...
VTWBUF =: st                                      % flush what you just drew
```

After adding it: four lines appeared unattended, 5 seconds apart, keyboard untouched.

**Its no-argument shape was DERIVED from the binary** (section 13b) and this is the first use that
put the derivation to work. That derivation also settled the vendor disagreement - UNIQUE-II passes
a dummy, the COBOL source declares none, and both are right because the dummy is never touched.

**When you need it:** after any drawing that is NOT immediately followed by a terminal read. Timer
work, anything driven by a message arriving, anything polled. If your program only ever redraws in
response to a keystroke you will never see this - which is exactly why it ambushes the first
program that does something on its own.

## 7. Appearance

### `VTATTR` - attributes
**VENDOR-CALL** `IVTATTR(1,imode,CWORK,ILENG,IOFLOW,LIN,IPOS,DIATTR)` (UNIQUE-II)

**Eight arguments** - the largest routine in the library at 315 bytes on ND-500 and 1603 words on
ND-100. Its header comment in UNIQUE, verbatim:
`C     Function: Write field to screen (VTM only) with attributes.`
So it is a combined position-write-with-attributes, not a mode switch: device, mode, text,
length, overflow, line, position, attributes.

### `VTSSYM`, `VTCSYM` - set / clear symbol set
**INFERRED** from the names. No caller found.

### `VTLAMPS` - terminal indicator lamps
**VENDOR-CALL** `IVTLAMP(2,1)` and `IVTLAMP(0,I)` (UNIQUE-II) - 2 arguments.

## 8. Input behaviour

### `VTECHM`, `VTBRKM` - echo mode, break mode
**MEASURED STRUCTURE**, from the ND-100 BRF dump: one **2722-word unit** exports 18 entries at
octal offsets 0, 20, 40, 60, 100, 120, 140, 160 and 710:

```
VTIECHN..0   VTBECHN..0   VTBECH0..20  VTIECH0..20
VTBECH1..40  VTIECH1..40  VTBECH5..60  VTIECH5..60
VT4BRKN..100 VTBBRKN..100 VTIBRKN..100 VTBBRK0..120
VTIBRK0..120 VTBBRK1..140 VTIBRK1..140 VTBBRK5..160
VTIBRK5..160 VTINIT...710
```

**These are not separate routines.** They are a 16-word-strided table of echo/break mode records,
and every `VTB*`/`VTI*` pair shares an offset - two names for one record, almost certainly the
BYTE and INTEGER caller views. `COBOL-85-LIB.NRF` confirms it independently: all sixteen share
one ND-500 address. **`VTINIT` lives in the same unit at offset 710B.**

### `VTCAPS` - caps
**VENDOR-CALL** `IVT=IVTCAPS(.TRUE.)` (STAR-TREK) - one BOOLEAN.

### `VTFUNC` - function keys
**INFERRED.** The data module carries `VTF1F01 VTF1F02 VTF1F03 VTF1F11 VTF1F25 VTF1N11 VTF1N25
VTF1R11 VTF1S11 VTFUNC1 VTFNCTNO VTFNSTRAIL VTBITFUNC`. No caller found.

### `VTHOLD` - hold
**INFERRED.** No caller found.

## 9. Query

### `VTINFO` - terminal information
**VENDOR-CALL** (STAR-TREK, verbatim):
```fortran
      INTEGER IVTARRAY(20),IVTLENGHT
      DATA IVTLENGHT /20/
C+    IVT=IVTINFO(IVTARRAY,IVTLENGHT)
C+    IF(IVTARRAY(3).LT.25) THEN
C+      MAXSIZ = 9
C+    ENDIF
```

**Two arguments: an INTEGER array and its length.** VTM fills the array with terminal properties.
**Element 3 is the number of LINES** - STAR-TREK tests `IVTARRAY(3).LT.25` to shrink its map on a
24-line terminal. The other 19 elements are not identified.

**This is the call to use for "what kind of terminal is this"**, and its shape is now known - it
was previously listed as unknowable.

### `VTAINF`, `VTLINF`, `VTAIFC` - attribute / line information
**INFERRED.** No callers found.

## 10. Buffers

### `VTDBUF` - define output buffer
**VENDOR-DECL** `ROUTINE VOID,INTEGER(BYTES,BOOLEAN) : VTDBUF`
**VENDOR-CALL** `VTDBUF(CBVTMBUF,TRUE)` with `BYTES: CBVTMBUF(0:199)` (COBOL-500)
Comment in the source: `% Define VTM output buffer`

**A 200-byte buffer is what ND's own COBOL runtime uses.**

### `VTWBUF` - write buffer (flush)
**VENDOR-DECL** `ROUTINE VOID,INTEGER : VTWBUF` - no arguments
**VENDOR-CALL** `ISTAT=IVTWBUF(IDUM)` (UNIQUE-II passes a dummy)

Buffering is why a VTM program can build a screen and emit it in one go.

### `VTSPBK`, `VTSETD`, `VTPREL` - unknown
**INFERRED** from size only. No callers, no declarations.

## 11. Routines with no caller and no declaration anywhere

`VTSTART` `VTGCTM` `VTDUMP` `VTRSET` - found only in `COBOL-85-LIB.NRF`'s symbol table as
`IVTSTAR`, `IVTGCTM`, `IVTDUMP`, `IVTRSET`. **`VTSTART` and `VTDUMP` share one address.**
Purpose unknown.

---

## 11b. STATUS CODES - the first four with meanings

**MEASURED 2026-08-25.** Every VTM routine returns a status as its out-value and nothing documents
the vocabulary. Four are now pinned, from a probe that deliberately asked for illegal geometry:

| Status | Returned by | Reads as |
|---|---|---|
| `0` | everything that worked, including all four `VTDSCR` calls | OK |
| `4112` | `VTWRIT` issued after a refused position | the write was refused |
| `4113` | `VTWRIT` whose text ran off the viewport's right edge | **output truncated** - and the length argument came back as what fitted |
| `4121` | `VTPCUR` asked for a row outside the viewport | **coordinate outside the area** |

**`4121` closes the oldest open question on this page.** The failed 2026-08-24 `VTDSCR` probe - the
one written up as "VTM has no viewports" - returned `4121`. It had passed its rectangle transposed,
`(40,14,69,19)`, and line 40 does not exist on a 24-line screen. VTM was saying "that coordinate is
outside the area" the whole time. The status was right; the reading of it was wrong.

## 12. What VTM does NOT have

Searched across all 95 BRF entries, 240 link-map symbols and the COBOL-85 NRF symbol table:

- **No window open/close, no z-order, no save-and-restore-under.** The nearest thing is `VTDSCR`
  (viewport) plus `VTCREC` (clear rectangle) - real primitives, but you build the window
  yourself.
- **No scrolling region, margin or page verb.** No `SCROLL`, `ROLL`, `MARGIN`, `REGION` or `PAGE`
  symbol exists anywhere. A VT100 has `DECSTBM` scrolling regions and VTM has no way to reach
  one. A scrolling pane means keeping your own buffer and repainting the lines.

## 13. The internal model, from the data symbols

The link maps split cleanly into a routine module and a data module. The data module shows a
**two-level coordinate system**, which is what makes `VTDSCR` possible:

```
VTVLIN VTVCOL              virtual screen size
VTVXORG VTVYORG            virtual ORIGIN        <- what VTDSCR sets
VTVXMAX VTVYMAX            virtual limits
VTVXPOS VTVYPOS            current position
VTVXNOW VTVYNOW            VTVXPREV VTVYPREV
VTPHYLIN VTPHYCOL          physical screen size
VTPHYXORG VTPHYYORG        physical origin
```

`VTMON22L`, `VTMN162`, `VTMN336` name the SINTRAN monitor calls VTM sits on: **MON 22, MON 162
and MON 336**.

---

## 13b. ARGUMENT COUNTS FOR ALL 37 - derived from the binary

**DERIVED 2026-08-25** from `SINTRAN/ND500-APPS/_shared/files/COBOL-85-LIB.NRF`, ND COBOL-85's
runtime library, which has the whole VTM library linked in with a symbol table.

**Method, and why it is trustworthy.** In the ND-500 wrappers an argument lives at a fixed
parameter slot, `0x10 + 4N`, and locals are numbered continuously after the parameters - so the
first temporary's number minus `0x46` is the argument count. Those are **two independent
estimators**, and they agree on 36 of 37. Both were calibrated against the twelve routines whose
argument lists are known from real callers, and **eleven of twelve agree exactly**.

| Routine | Args | Notes |
|---|---|---|
| `VTINIT` | 1 | agrees with caller |
| `VTEXIT` | **0** | callers pass a dummy that is never touched |
| `VTPCUR` | 2 | agrees - (line, position) |
| `VTWRIT` | 5 | agrees - **arg 4 is written back** |
| `VTWREP` | 6 | args 4 and 6 written back |
| `VTREAD` | 5 | arg 4 written back |
| `VTBREAD` | 10 | agrees - arg 6 written back |
| `VTINBT` | 4 | agrees - **arg 3 written back**, which is `GETCTRL` in STAR-TREK |
| `VTOUTB` | 4 | |
| `VTCSCR` | 4 | agrees |
| `VTCREC` | 4 | agrees |
| **`VTDSCR`** | **4** | **byte-identical in structure to VTCSCR and VTCREC** |
| `VTCLIN` | **0** | see the dummy note below |
| `VTCEOL` | **0** | |
| `VTCBOL` | **0** | |
| `VTCSYM` | **0** | |
| `VTWBUF` | **0** | agrees with the no-argument declaration |
| `VTSSYM` | 1 | a STRING |
| `VTAINF` | 1 | a STRING |
| `VTDBUF` | 2 | arg 1 is a STRING - agrees with `VTDBUF(BYTES, BOOLEAN)` |
| `VTATTR` | 8 | agrees - arg 3 STRING, **arg 4 written back** |
| `VTINFO` | 2 | agrees - arg 1 is the array |
| `VTLINF` | 1 | |
| `VTAIFC` | 2 | |
| `VTCAPS` | 1 | agrees |
| `VTECHM` | 2 | |
| `VTBRKM` | 2 | |
| `VTFUNC` | 3 | **arg 2 written back** |
| `VTHOLD` | 2 | |
| `VTLAMPS` | 2 | agrees |
| `VTSETD` | 2 | |
| `VTPREL` | 2 | |
| `VTSPBK` | 1 | |
| `VTGCTM` | 3 | arg 3 written back |
| `VTDUMP` | 1 | |
| `VTRSET` | 1 | |
| `VTSTART` | **19** | derived but UNCONFIRMED - see below |

### The dummy-argument class - and it settles a real disagreement

**`VTCLIN`, `VTCEOL`, `VTCBOL`, `VTCSYM`, `VTEXIT` and `VTWBUF` take NO real arguments.** All six
are 110-byte records, **byte-identical apart from the name**, and they fetch nothing at all -
straight to the call.

That resolves a contradiction noted earlier on this page: VALLEY calls `IVTCLIN()` and UNIQUE-II
calls `IVTCLIN(IDUM)`. **Both are correct.** The dummy is genuinely never touched. The same holds
for UNIQUE's `IVTWBUF(IDUM)` against the COBOL source's no-argument `VTWBUF` declaration.

### Output arguments - which slots VTM writes back

Derived from the epilogue store-back idiom, where the operands appear reversed against the load:

`VTWRIT` arg 4 - `VTREAD` arg 4 - `VTWREP` args 4 and 6 - `VTBREAD` arg 6 - `VTATTR` arg 4 -
`VTFUNC` arg 2 - `VTGCTM` arg 3 - `VTINBT` arg 3

Two independent confirmations that this read is right: `VTWRIT`'s fourth parameter is
`INTEGER READ WRITE` in the typed vendor declaration, and `VTINBT`'s arg 3 is `GETCTRL` in
STAR-TREK - a variable that receives a value.

### String arguments

Routed through `VTBYTESPAR`: `VTSSYM` a1, `VTAINF` a1, `VTDBUF` a1, `VTWRIT` a3, `VTWREP` a3,
`VTREAD` a3, `VTBREAD` a3, `VTATTR` a3, and two in `VTSTART`.

### CORRECTION - the sizes quoted earlier on this page were attached to the wrong routines

The symbol table is address-then-name, not name-then-address: **the address precedes the name it
belongs to.** Reading it the other way shifts every address one name early. Corrected values:
`IVTINIT` 117 - `IVTSETD` 122 - **`IVTDSCR` 132** - `IVTATTR` 196 - `IVTSTAR` 315 - `IVTRSET` 128.

**This invalidates one piece of reasoning used earlier.** The guess that `VTDSCR` might take two
arguments "because it is 122 bytes like `VTPCUR`" came from the mis-read table. It is 132 bytes
and takes four - so the live probe that called it with four had the **right** count all along, and
the first failure was purely the transposed coordinate order.

### What is NOT derived

- **Argument MEANINGS.** This gives counts, which slots are strings, and which are written back.
  Nothing about what any argument signifies.
- **`VTSTART` = 19 is the weakest number**, from a routine with register-form operands that were
  not decoded. Treat it as derived-but-unconfirmed.
- **No real ND-500 disassembly was done** - the opcodes are named by their role in this one idiom,
  calibrated against known callers. It is pattern-matching that reproduces twelve known answers,
  not an instruction-set decode.

## 14. The ND documents that DO exist, and what each is good for

No manual documents the calls. These are what the library actually holds, and several are
genuinely useful.

### Configuration - fully documented

| Document | Path | What it gives |
|---|---|---|
| **SINTRAN III System Supervisor** ND-30.003.7 | `Operations/SINTRAN/ND-30.003.007 EN ...md` | **Table 18** at :10672 - 61 terminal types, complete. **CTYTP** at :11042 with all six bits and a checked worked example. Type-0 behaviour at :10712. **:10733 explains the NEGATIVE value** `-5029` and why it is not an error |
| **VTM terminal tables PD sheet** ND-211464 | `Installation/Installation-Description/ND-211464-1-EN.md` | The authoritative **VTM-COMPOUND** reference: file naming across VTM versions A/B/C-onward, all menu procedures with option numbers, and which types are built into the tool |
| NOTIS-BG install ND-210793 | `Installation/Installation-Description/ND-210793-3-EN.md` | :892 a narrated VTM-COMPOUND walkthrough; :278 the warning that a new DDBTABLES loses your custom types |
| VTM tables floppy decode ND-210455 | `Installation/Software/ND-210455/ND-210455G04/README.md` | Real disk contents: `VTM-1B-ARRAY-<ver>:BRF`, `VTM-2B-ARRAY`, `VTM-ARRAYS:NRF`, ~60 per-type `DDBnnn:VTM` |
| SINTRAN III Utilities ND-60.151.3 | `Reference-Manuals/ND-60.151.3 EN ...md` | :107 - **the dead end.** It records that VTM-COMPOUND is "no longer documented in this manual" |

**ND-60.151.02 - the edition that DID carry the VTM-COMPOUND chapter - is not in this repo.**

### Behaviour, from applications that use VTM

| Document | Path | What it gives |
|---|---|---|
| **COBOL Reference Manual** ND-60.144.3 | `Reference-Manuals/ND-60.144.3 EN COBOL Reference Manual.md` | **The capability envelope.** COBOL-85's runtime is linked against VTM, so its screen verbs at :4673-5135 are a documented view of what VTM drives - and **its option names are the SAME vocabulary PLANC-SCREEN-H uses** |
| **NOTIS-WP Editor** ND-63.002.02 | `Reference-Manuals/Notis/ND-63.002.02 ...md` | :655 the type-0 prompt described from the user's side. **:7168 "Terminal not defined with cursor control"** - CTYTP bit 11 (CPOS) clear, surfaced as a user error. :7223 which terminals it runs on, and that ND substitutes characters for missing graphics |
| **LED User Guide** ND-60.266.2 | `Reference-Manuals/ND-60.266.2-EN LED User Guide.md` | :834 the only line tying an application to a **VTM function-key table code**. :392-547 a full overlapping-window model with Z-order and clipping - **built ON TOP of VTM, using calls it does not document** |
| FOCUS-G PI sheet ND-210188 | `Installation/Product-Info/ND-210188-A1-EN.md` | :55 "Many forms may be in use simultaneously, and forms may overlap" - again above VTM |
| NSHS PI sheet ND-10013 | `Installation/Product-Info/ND-10013-A2-EN.md` | :61 its runtime inventory - clear fields, protected areas, a message line. No windows, no scrolling |
| ND-Specific Programming ND-20034-1 | `Reference-Manuals/ND-20034-1-EN ...md` | :1248 - VTM's manual number is **"Internal"**. This is the primary evidence that no manual was ever published |

**Also absent from the repo:** ND-60.088 (NSHS manual), ND-60.137 (FOCUS manual), ND-60.172
(COB-GEN).

### What the surrounding documents independently confirm

Three measurements on this page were made before these documents were found, and each is
corroborated by one:

1. **The negative CTYTP value is normal.** ND-30.003.7:10733 works the identical example:
   *"@GET-TERMINAL-TYPE, TERMINAL TYPE: -5029 ... Has something gone wrong? No, in fact this is
   the same as terminal type 91!"*
2. **A bare model number is enough.** ND-30.003.7's CTYTP example states it: *"It is not
   necessary to set anything but the terminal type before running an application using VTM. The
   remaining attributes will then by modified automatically."* MEASURED true - VTM derives them
   on its first call.
3. **Testing `CPOS` is the right way to refuse a printing terminal.** NOTIS-WP:7168 does exactly
   that and surfaces it as *"Terminal not defined with cursor control ... on a VDU defined
   without cursor positioning capabilities."*

### And the thing no ND document contains

**No scrolling region, at any layer, in any product.** A search for "scroll region", "scrolling
region" and "scrolling area" across `Reference-Manuals`, `Installation`, `Operations` and
`Developer` returns zero hits. Every "scroll" in the library is a user action over a whole screen
or editor region. NSHS, FOCUS, UNIQUE and COBOL all repaint, erase rectangles, or defer-erase -
consistent with VTM having no scroll primitive.

**Windows exist only ABOVE VTM.** LED implements Z-order and clipping; FOCUS-G overlaps forms.
Neither documents the calls used. VTM itself offers `VTDSCR` (viewport) and `VTCREC` (clear
rectangle), and you build the rest.

---

## See also

- [PLANC-INTERACTIVE-SCREEN-PATTERNS.md](PLANC-INTERACTIVE-SCREEN-PATTERNS.md) - how these calls
  go together into a program that polls keys, runs a timer and manages windows

- [VTM-VIEWPORT-HOW-TO-USE-IT.md](VTM-VIEWPORT-HOW-TO-USE-IT.md) - what the viewport lets us
  BUILD: clipping, nesting, getting back out, and the helper shape it argues for
- [PLANC-VTM-PROGRAMMING-GUIDE.md](PLANC-VTM-PROGRAMMING-GUIDE.md) - what VTM is, prerequisites,
  terminal types, and a program that has been run
- [PLANC-UI-VTM-GUIDE.md](PLANC-UI-VTM-GUIDE.md) - PLANC-SCREEN-H, the higher-level library
- `SINTRAN/XMSG/TESTUI/VTM-ENTRIES.txt` - the raw BRF entry list with sizes
- skill `planc` section 10, skill `sintran-management` for terminal administration
