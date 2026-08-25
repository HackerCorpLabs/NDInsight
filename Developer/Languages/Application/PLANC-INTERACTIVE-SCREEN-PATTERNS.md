# Interactive screen programs in PLANC - the patterns that work

**What this page is.** The other VTM pages tell you what the calls do. This one tells you how to
put them together into a program that reacts to keys, redraws windows, and does things on a clock -
and it exists because every one of these patterns was got WRONG first, on a real ND-100, before it
was got right.

**Everything here was measured on D100 between 2026-08-24 and 2026-08-25.** The worked example is
`SINTRAN/XMSG/TESTUI/TESTUI.PLNC` - 1379 lines, compiled and running. Where a claim is not
measured, it says so.

**Read first:** [VTM-API-REFERENCE.md](VTM-API-REFERENCE.md) for the calls,
[VTM-VIEWPORT-HOW-TO-USE-IT.md](VTM-VIEWPORT-HOW-TO-USE-IT.md) for the viewport,
[PLANC-UI-VTM-GUIDE.md](PLANC-UI-VTM-GUIDE.md) for PLANC-SCREEN-H.

---

## 0. The shape of the program

```planc
    INISTACK stack
    blankscreen                     % VTM's first call - the CTYTP bits appear here
    <read the terminal type, refuse a printing terminal>
    drawMain
    MON3(1, -1, echoTab)            % take the keyboard: no echo
    MON71(1)                        % and no ESC abort

    DO
        pollKey =: key
        IF key = 0 THEN
            <anything on a clock goes here>
            MN104(pollHold, 1)      % sleep a tenth of a second
        ELSIF key = ... THEN
            <handle it>
        ENDIF
    ENDDO

exit:
    blankscreen
    MON3(1, 0, echoTab)             % give the keyboard back - BOTH halves
    MON72(1)
    resetscreen
```

**Six things in that skeleton are the patterns below.** None of them is obvious and none of them is
in any ND manual, because VTM's manual number is literally "Internal".

---

## 1. Own the keyboard - and give it back

### Poll, do not block

`intacc` (PLANC-SCREEN-H) **blocks** and can only give back a **number**, so it cannot serve a menu
with letter keys and it cannot coexist with a timer. `MON1` blocks too. **`TerminalNoWait` does not
help - it was tried on this machine and `MON1` went on blocking anyway.**

**`MON66` (ISIZE, InBufferSpace) answers how many bytes are WAITING.** Ask first, read only when
there is one, and blocking stops mattering:

```planc
    ROUTINE VOID, INTEGER : pollKey
        INTEGER : waiting
        INTEGER : ch
        MON66(1) =: waiting
        IF waiting <= 0 THEN
            0 RETURN
        ENDIF
        MON1(1) =: ch
        % TERMINAL INPUT CARRIES AN EVEN-PARITY BIT IN BIT 7 - S arrives as
        % 211, not 83. Mask it or nothing ever matches and the program looks
        % hung rather than wrong.
        ch AND 127 RETURN
    ENDROUTINE
```

**Sleep when the buffer is empty** or this is a spin, not a poll: `MN104(5, 1)`, type 1 = basic
units of a fiftieth of a second. The manual says no parameter may be zero.

**Note the names:** `MON66` but `MN104` and `MN113` - the runtime drops the O at three digits.

### Take the echo, and take ESC to make that safe

A raw key loop has no say in where the cursor is, so SINTRAN echoes each keypress wherever the last
drawing finished - **inside your windows**. Measured: a stray `2` in one window and a `1` in another.

```planc
    BOOLEAN ARRAY PACKED : echoTab(0:127)     % never read except for strategy 7
    ...
    MON3(1, -1, echoTab)      % below zero = no echo (the password-field case)
    MON71(1)                  % take ESC away from SINTRAN
```

**Disabling ESC is what makes disabling the echo safe, not bravado.** An ESC abort skips your exit
path, and a terminal handed back with no echo at all is far worse than a stray character.

**AND ESC STILL QUITS.** Because SINTRAN is no longer intercepting it, ESC arrives at `pollKey` as
ordinary byte 27 - handle it exactly like your quit key. The escape hatch is not lost, it is routed
through your own tidy-up instead of around it. **MEASURED:** ESC left the program and `@TIME`
echoed normally at the prompt afterwards.

**Restore BOTH halves on every exit path.** `MON3(1, 0, echoTab)` and `MON72(1)`.

---

## 2. FLUSH, or your timer will look broken

**VTM buffers its output. A terminal READ is what flushes it.**

Every PLANC-SCREEN-H program redraws and then calls `intacc`, so the read flushes the repaint and
nobody ever sees the buffer. **The moment your program draws something without a keystroke behind
it - a timer, a message arriving, a poll - the drawing goes into the buffer and stays there.**

```planc
    IMPORT ( ROUTINE VOID, INTEGER : VTWBUF )     % NO arguments
    ...
    VTWBUF =: st                                   % after drawing, before sleeping
```

**The symptom points at the wrong component.** It does not look like a stale screen, it looks like
your timer stopped. What proved it: with the timer apparently dead, one keypress made **three**
lines appear at once, two of them stamped seconds earlier. **Lines that arrive together but are
stamped five seconds apart were drawn on time and shown late.**

---

## 3. Windows: use the viewport

`VTDSCR` moves VTM's ORIGIN, so everything afterwards - `VTPCUR`, `VTCREC`, and through
PLANC-SCREEN-H also `frame`, `bytdis`, `blankarea`, `blankscreen` - is measured from the
rectangle's corner. **It CLIPS** (for `VTWRIT`), and **it does NOT nest**: a second `VTDSCR`
replaces the first and reads its rectangle against the PHYSICAL screen.

```planc
    ROUTINE VOID, VOID (INTEGER, INTEGER, INTEGER, INTEGER) : winEnter(row, col, high, wide)
        INTEGER : st
        VTDSCR(row, col, row + high - 1, col + wide - 1) =: st   % ABSOLUTE always
    ENDROUTINE

    ROUTINE VOID, VOID : winLeave
        INTEGER : st
        VTDSCR(1, 1, -1, -1) =: st        % -1 = "to the end". VTINIT is not the only way back
    ENDROUTINE
```

**What this buys:** window code stops carrying the window's position. `TESTUI`'s big window used to
have eighteen start columns, each `65 - length`, every one wrong the moment the window moved. Now
they are all local and the position appears once.

---

## 4. You are the window manager - three rules

VTM has **no z-order and no save-under**, and neither does PLANC-SCREEN-H. So a stack order is
something you write down, and **three separate places have to read it**.

### Rule 1: CLIP what you paint - never SKIP it

A covered window must not paint through the one on top. **The tempting cure - "a covered window
paints nothing" - is safe and WRONG.** It shipped for half a day and froze nine visible lines
because a window covered two of them.

```planc
    % Is one cell of this window visible? Window's own coordinates in,
    % physical conversion in ONE place.
    ROUTINE VOID, BOOLEAN (INTEGER, INTEGER) : visibleAt(lrow, lcol)
        INTEGER : prow
        INTEGER : pcol
        scrRow + lrow - 1 =: prow
        scrCol + lcol - 1 =: pcol
        IF smallOpen THEN
            IF prow >= smallRow AND prow <= smallRow + smallHigh - 1 THEN
                IF pcol >= smallCol AND pcol <= smallCol + smallWide - 1 THEN
                    FALSE RETURN
                ENDIF
            ENDIF
        ENDIF
        ...                                  % one block per window above you
        TRUE RETURN
    ENDROUTINE
```

Then walk the line once and paint each visible **run** as it ends. **A test per character costs
nothing on the CPU; what it saves is the wire**, which is the slow half.

### Rule 2: every CLOSE repaints the stack BOTTOM UP

Not "put back the window I overlapped" - that works with two windows and **breaks with three**.

### Rule 3: every OPEN puts back the windows ABOVE it

Opening paints over whatever was there, higher windows included. `repaintAbove(level)` costs
nothing for the window that is already on top.

**One stack order, three consumers.** In TESTUI: `scroll -> small -> big`.

---

## 5. Scrolling is yours to do

**No scrolling region exists at any layer in any ND product** - searched across every BRF entry,
every link-map symbol and the whole documentation library. A VT100 has `DECSTBM` and VTM has no way
to reach it.

So keep the lines yourself in a **ring buffer** - scrolling is then one index change and no text is
copied at all:

```planc
    BYTES : scrBuf(0:699)          % 10 slots x 70 characters
    INTEGER : scrCount             % lines held, 0..10
    INTEGER : scrFirst             % slot holding the OLDEST line
```

**Two things make it cheap:**

- **Store every line PADDED to full width.** A short line painted over a long one would leave the
  old tail on screen; padding removes the need to erase first.
- **Only a FULL window costs a repaint.** While there is still room the new line goes on the next
  free row and nothing else moves - one line, not ten.

---

## 6. Do not count characters by hand

**`MAXINDEX(text, 1)` asks an array its declared upper bound** and needs no `IMPORT` - it is a
standard routine (ND-60.117.5 section 3.17, page 153). A PLANC string literal subscripts from ZERO,
so the length is `MAXINDEX + 1`:

```planc
    ROUTINE VOID, VOID (INTEGER, BYTES) : bigLine(row, text)
        bytdis(row, bigWide - 5 - MAXINDEX(text, 1), 0, text, '')
    ENDROUTINE
```

**MEASURED: all eighteen lines land on one column.** A hand-counted length builds clean and shows
only as a line in the wrong place - the same family as the `'ALn'` width trap.

**Restriction, page 249:** not available on array parameters inside a `STANDARD` routine (the
FORTRAN/COBOL calling sequence). An ordinary PLANC routine is fine.

---

## 7. The traps, all measured

| Trap | What you see | The fix |
|---|---|---|
| **`bytdis` width 0 with a SUBARRAY** | The line runs off the right and wipes borders it could not reach | Pass the FIELD WIDTH. Width 0 is right for a literal only |
| **No flush** | "The timer stopped" | `VTWBUF` after any drawing not followed by a read |
| **`CLOCK`'s year is 1998, not 98** | Date prints `24-08-     8` | Clamp inside the helper: `v MOD 100`. `digits(199)` is a read 190 bytes out of bounds and **PLANC checks nothing** |
| **A covered window frozen** | Buffer updates, screen does not | Clip per cell, do not skip |
| **Opening under a window** | The new window's frame across the old one | `repaintAbove` |
| **Key echo** | Stray characters inside windows | `MON3(1, -1, ...)` plus `MON71(1)`, restored on exit |
| **Parity bit** | Nothing ever matches your key constants | `ch AND 127` |
| **A PLANC LOCAL may not be initialised** | `*** ERROR - INITIAL VALUE ILLEGAL HERE` | Declare bare, assign after `INISTACK`. Module level MAY be initialised |
| **`VTDSCR` does not nest** | A sub-window lands at the physical corner | Always pass absolute coordinates |

---

## 8. What it costs on the wire

The reason any of this care is worth taking: on a real 9600-baud line a full 24x80 repaint is about
2000 characters, roughly **two seconds**. The numbers TESTUI works to:

| Action | Characters |
|---|---|
| Whole screen | ~2000 |
| One window | ~100-800 |
| One scroll line, window not full | 70 |
| One scroll line, window FULL | ~700 (ten lines move) |
| One clipped line under a window | only the visible runs |

On an emulator you feel none of it. On the machine these programs were written for, you feel all
of it.

---

## See also

- [VTM-API-REFERENCE.md](VTM-API-REFERENCE.md) - all 37 VTM routines, argument counts, status codes
- [VTM-VIEWPORT-HOW-TO-USE-IT.md](VTM-VIEWPORT-HOW-TO-USE-IT.md) - `VTDSCR` measured in full
- [PLANC-UI-VTM-GUIDE.md](PLANC-UI-VTM-GUIDE.md) - PLANC-SCREEN-H, the drawing layer
- [PLANC-VTM-PROGRAMMING-GUIDE.md](PLANC-VTM-PROGRAMMING-GUIDE.md) - prerequisites, terminal types,
  what to do about a printing terminal
- `SINTRAN/XMSG/TESTUI/TESTUI.PLNC` - the worked example, every pattern here in place
- skill `planc` section 10 - the short form
