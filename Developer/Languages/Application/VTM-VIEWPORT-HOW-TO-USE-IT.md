# VTDSCR is a real viewport - what that lets us build

**What this page is.** `VTDSCR` was proved to be a genuine viewport on 2026-08-25: after
`VTDSCR(14,40,19,69)` a write at position `1,1` landed on **physical row 14, column 40**. That is
the finding. This page is the next question - *what can we actually do with it* - and it is
deliberately separate from
[VTM-API-REFERENCE.md](VTM-API-REFERENCE.md), which stays a reference to the calls.

**Read the reference first** for the call itself, its argument count and the coordinate order.
Everything here assumes those.

---

## 1. The one-sentence version

`VTDSCR` moves VTM's ORIGIN. Every coordinate a program hands VTM afterwards - `VTPCUR`,
`VTCREC`, and through PLANC-SCREEN-H also `frame`, `bytdis`, `blankarea` and `blankscreen` - is
measured from the rectangle's top-left corner instead of the screen's.

That is not a small convenience. It is the difference between a window being a **place you have
to compute** and a window being a **place you move into**.

## 2. What it replaces, in code we have already written

`TESTUI.PLNC` draws a right-aligned window today. Here is the real code, unedited:

```planc
    ROUTINE VOID, VOID : drawBig
        openWindow(bigRow, bigCol, bigHigh, bigWide, 'SPACE-FILL')
        bytdis( 5, 39, 0, 'Lorem ipsum dolor sit amet',  '')   % 26
        bytdis( 6, 38, 0, 'consectetur adipiscing elit', '')   % 27
        bytdis( 7, 44, 0, 'sed do eiusmod tempor',       '')   % 21
        ...
        bytdis(22, 39, 0, 'mollit anim id est laborum',  '')   % 26
    ENDROUTINE
```

**Eighteen hand-computed start columns**, each one `65 - length`, each with the length written in
a comment beside it so the next person can check the arithmetic. That comment block exists because
nothing in the program can check it: get one wrong and the line is simply in the wrong place, and
the compiler is perfectly happy.

Worse, those numbers encode WHERE THE WINDOW IS. Move the big window three columns left and all
eighteen change.

With a viewport the window's position appears **once**:

```planc
    ROUTINE VOID, VOID : drawBig
        VTDSCR(bigRow, bigCol, bigRow + bigHigh - 1, bigCol + bigWide - 1)
        frame(1, 1, bigHigh, bigWide, 'SPACE-FILL')
        bytdis( 2, 40 - 26, 0, 'Lorem ipsum dolor sit amet',  '')
        ...
```

and the right-alignment arithmetic becomes `windowWidth - textLength`, which is a property of the
TEXT, not of where the window happens to sit.

**This is the real win, and it is worth saying plainly: a viewport makes window code
position-independent.** Everything else below is secondary.

## 3. What VTM gives you and what it still does not

| Want | VTM call | Status |
|---|---|---|
| Move the origin to a rectangle | `VTDSCR(l1,p1,l2,p2)` | **works** |
| Clear inside the rectangle | `VTCREC`, or `blankscreen` via PLANC-SCREEN-H | works - `blankscreen` was MEASURED to clear only within the area |
| Draw a border | `frame` (PLANC-SCREEN-H) | works, at local coordinates |
| Save what is UNDER the window | - | **does not exist** |
| Z-order / overlap management | - | **does not exist** |
| Scroll a region | - | **does not exist at any layer in any ND product** |

So VTM gives you the two primitives - a moved origin and a rectangle clear - and you build the
window. **Closing a window still means repainting what it covered**, which is exactly what
`closeSmall` and `closeBig` do in TESTUI today. A viewport does not change that; it changes how
you address the repaint.

## 4. The helper shape this argues for

Two routines, and the window's position is written once:

```planc
    % Move into a window. Everything drawn after this is at coordinates
    % 1,1 .. high,wide inside the box, wherever the box happens to be.
    ROUTINE VOID, VOID (INTEGER, INTEGER, INTEGER, INTEGER) : winEnter(row, col, high, wide)
        VTDSCR(row, col, row + high - 1, col + wide - 1)
    ENDROUTINE

    % Move back out to the whole screen.
    ROUTINE VOID, VOID : winLeave
        ...            % see section 5 - which call goes here was the open question
    ENDROUTINE
```

`winEnter` takes row/col/HEIGHT/WIDTH, the same convention `frame`, `openWindow` and `clearWindow`
already use in TESTUI, so the CORNERS conversion stays in exactly one place - the same rule that
page already follows for `blankarea`.

## 5. The questions that decide how far this goes

Four things were NOT known after the first measurement, and each one changes the design:

1. **Does the rectangle CLIP, or only offset the origin?** If it clips, a window is a containment
   boundary and a too-long line cannot damage anything outside it. If it only offsets, the
   viewport is a convenience and every write still needs its own length discipline.
2. **What happens at a position outside the box?**
3. **Does a second `VTDSCR` NEST inside the first, or REPLACE it?** Nesting means window code
   composes - a window can open a window without knowing where it is. Replacing means every
   `winEnter` needs the absolute screen position, and the helper above is wrong.
4. **How do you get back to the full screen without `VTINIT`?** Without an answer, `winLeave`
   cannot be written at all, and a program that has entered a window can never draw its own status
   line again.

**These were measured on 2026-08-25 - the results are in section 6.**

## 6. MEASURED - D100, 2026-08-25, all four answered

**The probe** is `probeView` in `SINTRAN/XMSG/sync-testui/TESTUI.PLNC`, key 4. It runs six steps,
each leaving a LETTER on the screen so the geometry is read off the physical screen rather than
believed from a status, and it collects every call's status for printing after `resetscreen` -
because while a viewport is in force there may be nowhere on screen to print anything.

Compiled 625 lines, 0 diagnostics; linked with no undefined entries.

### The screen, verbatim

```
DD-Small 2-Big 4-PCUR order 3-Exit     CHOICE: .      TERM:   6
lqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqk
...
x  ESC   ENABLED   it can send ESC sequAAAAAAAAAAnput                        x
x                                      C  CC                                 x
...
EqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqFqqqqqqqj
```

### 1. IT CLIPS. This is the big one.

The box was rows 10-12, columns 40-49 - **ten columns wide**. Thirty `A` characters were written
at position 1,1.

**Exactly ten landed, in columns 40 to 49, and NOTHING wrapped to the next row.** Counted from the
captured line, not judged by eye: the longest run of `A` starts at column 40 and ends at column 49,
replacing the ten characters `ences on i` in the text underneath - and row 11 holds the `C`s from
the next step, not the overflow.

**So a viewport is a containment boundary, not just an offset** - for `VTWRIT`.

**AND THAT LAST QUALIFICATION IS NOT PEDANTRY - it was measured the hard way on 2026-08-25.** A
line painted with PLANC-SCREEN-H's `bytdis`, given a width of 0 and a SUBARRAY of a larger buffer,
ran clean off the right-hand side and wiped BOTH the window's own border and the border of the
window outside it. A write of seventy characters starting at column 5 cannot reach either, so
`bytdis` used some other length - and it was **not stopped at the viewport edge**, although
`VTWRIT` demonstrably is.

**Do not carry the clipping result over to PLANC-SCREEN-H.** It is measured for `VTWRIT` and for
nothing else. When you hand `bytdis` a subarray, pass the FIELD WIDTH explicitly rather than 0.

### 2. VTM TELLS YOU HOW MUCH IT TOOK - and that is what the length argument is for

`VTWRIT`'s fourth argument is `INTEGER READ WRITE`. We knew VTM wrote something back; we did not
know what. We passed 30 and **got back 10** - the number of characters actually accepted.

That settles the meaning of the write-back for `VTWRIT`, and it makes clipping *detectable*: a
program can compare what it asked for against what came back and know its text did not fit,
without measuring anything itself.

### 3. A POSITION OUTSIDE THE BOX IS REFUSED, AND THE WRITE IS REFUSED AFTER IT

`VTPCUR(5,1)` on a three-row box - row 5 does not exist - returned **4121** and moved nothing. The
`VTWRIT` that followed returned **4112** and printed nothing anywhere on the screen. No `B` appears
in the capture.

**This closes the oldest open question on the reference page.** The very first `VTDSCR` probe -
the one written up as "VTM has no viewports" - returned status `4121`. It had passed the rectangle
transposed, `(40,14,69,19)`, and line 40 does not exist on a 24-line screen. **4121 was VTM saying
"that coordinate is outside the area" all along.** The status was correct and the reading of it
was wrong.

A small status vocabulary now has meaning, from three observations rather than a manual:

| Status | Seen on | Reads as |
|---|---|---|
| `0` | every call that worked, including all four `VTDSCR`s | OK |
| `4112` | `VTWRIT` after a refused position | the write was refused |
| `4113` | `VTWRIT` whose text ran off the right edge | **output truncated** - and the length came back as what fitted |
| `4121` | `VTPCUR` outside the area, and the 2026-08-24 transposed `VTDSCR` | **coordinate outside the area** |

### 4. `VTCREC` IS VIEWPORT-RELATIVE TOO

`CCCCC` was written at box position 2,1 and then `VTCREC(2,2,2,3)` was asked to clear box columns
2-3 of box row 2.

The surviving `C`s are at physical columns **40, 43, 44** - so columns 41 and 42 were cleared, which
is box columns 2 and 3 of box row 2 exactly. Physical interpretation would have cleared something
at the top of the screen and left all five `C`s standing.

**The whole coordinate system moves, not just the cursor.** Together with the earlier measurement
that `blankscreen` cleared only within the area, that means PLANC-SCREEN-H rides along: `frame`,
`bytdis`, `blankarea` and `blankscreen` all land inside the window without knowing a window exists.

### 5. VIEWPORTS DO NOT NEST - a second VTDSCR REPLACES the first, in PHYSICAL coordinates

With the rows 10-12 / columns 40-49 box in force, `VTDSCR(1,1,2,5)` was issued and `DD` written at
position 1,1.

**`DD` landed at physical row 1, column 1** - it is the `DD` at the very start of the status line
in the capture above. Nesting would have put it at physical row 10, column 40, where an `A` still
sits.

**So the rectangle handed to `VTDSCR` is always read against the PHYSICAL screen.** This has a
direct consequence for the helper in section 4: `winEnter` is correct as written - it takes
absolute screen coordinates and always will - but a window CANNOT open a sub-window relative to
itself. A nested window has to know its own absolute position and do the addition.

### 6. THERE ARE TWO WAYS BACK TO THE FULL SCREEN, AND BOTH WORK

Issued from inside the 2x5 box:

- `VTDSCR(1, 1, -1, -1)` returned 0, and `E` then landed at **physical row 24, column 1**.
- `VTDSCR(1, 1, 24, 80)` returned 0, and `F` then landed at **physical row 24, column 70**.

So the `-1` "to the end" convention that three vendor programs use with `IVTCSCR(1,1,-1,-1)` holds
for `VTDSCR` as well - and it is the better of the two, because it needs no screen size.

**`winLeave` can now be written, and it is one line:**

```planc
    % Back to the whole screen. -1 means "to the end", the same convention
    % IVTCSCR(1,1,-1,-1) uses in VALLEY, UNIQUE-II and STAR-TREK. MEASURED
    % 2026-08-25: issued from inside a 2x5 box, a write at 24,1 afterwards
    % landed on physical row 24 column 1.
    ROUTINE VOID, VOID : winLeave
        VTDSCR(1, 1, -1, -1)
    ENDROUTINE
```

`VTINIT` is no longer the only way out, so a program can enter and leave windows freely and still
draw its own status line.

---

## 6b. OVERLAPPING WINDOWS - two rules, both measured

A viewport tells you where to draw. It tells you nothing about who is on top, and VTM has no
z-order and no save-under. **The application is the window manager**, and these two rules are what
that job reduces to. Both came out of a real fault Ronny found in TESTUI on 2026-08-25.

### Rule 1: CLIP what you paint - do not skip it

A window whose contents change while another window covers it must not paint straight over the top
one. **The tempting rule is "a covered window paints nothing". It is safe and it is wrong**, and
this page said it for half a day before Ronny found what it does:

> *"while window 1 is open timer or space does not update the S window ... the buffer is updated it
> seem, but not ui"*

The small window in TESTUI covers the **top two rows** of the scrolling window and nothing else.
Freezing all ten lines to protect two is not a trade-off, it is a bug.

**Clip per cell instead.** Ask each character position whether anything is over it, and emit the
runs that survive:

```planc
    % Is one cell of this window visible? Takes the window's OWN coordinates
    % and converts to physical, so the geometry appears in one place.
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

then walk the line once and paint each visible run as it ends.

**A test per character sounds expensive and is not.** Seventy tests per line is nothing on the CPU,
and what it SAVES is the half that costs: only the visible runs go on the wire.

**MEASURED 2026-08-25**, all four cases: with the small window open new lines paint on the rows it
does not cover and the covered line shows only its run to the right of it; with BOTH windows open
lines still paint, clipped to the slice left of the big one; closing both restores the window
completely, seven lines at exact five-second intervals; and neither covering window is ever
damaged.

**The buffer is still the truth** - that part was right. It is what a full repaint is rebuilt from
when a covering window closes. What changed is that being covered is no longer a reason to skip
the paint.

### Rule 2: every close repaints the stack BOTTOM UP### Rule 2: every close repaints the stack BOTTOM UP

```planc
    ROUTINE VOID, VOID : repaintStack
        IF scrollOpen THEN drawScroll ENDIF      % bottom
        IF smallOpen  THEN drawSmall  ENDIF
        IF bigOpen    THEN drawBig    ENDIF      % top
    ENDROUTINE
```

**The obvious fix is wrong, which is why this is a rule and not a patch.** "When I close, put back
the window I overlapped" works with two windows and breaks with three: closing the small window
while BOTH the big and the scroll windows are open repaints the scroll window straight over the
big one.

Write the overlap table down where the close routines can be read against it. In TESTUI:

| | small r4-13 c5-14 | big r4-23 c30-69 |
|---|---|---|
| **small** | - | **never** - 14 < 30 |
| **big** | never | - |
| **scroll** r12-23 c3-76 | rows 12-13 | rows 12-23 |

**The gap that produced this section:** `closeScroll` put the other two windows back, and
`closeSmall` and `closeBig` did not put the scroll window back. The table has to be consulted in
BOTH directions.

### Rule 3: OPENING a window must put back the windows ABOVE it

Opening paints over whatever was there - including windows that are meant to be higher in the
stack. Opening the scroll window while the big one was up laid its frame straight across it.

```planc
    % 1 = scroll, 2 = small, 3 = big.  Only what is ABOVE the one just opened.
    ROUTINE VOID, VOID (INTEGER) : repaintAbove(level)
        IF level < 2 THEN
            IF smallOpen THEN drawSmall ENDIF
        ENDIF
        IF level < 3 THEN
            IF bigOpen THEN drawBig ENDIF
        ENDIF
    ENDROUTINE
```

Cheaper than repainting the whole stack: opening the TOP window costs nothing at all. **MEASURED:**
the scroll window opened underneath an open big window, and the big window came back intact on top.

**One stack order, three consumers** - `repaintAbove` on open, `repaintStack` on close, and
`visibleAt` for the clipping. Write it down once.

---

## 6c. IF ANYTHING DRAWS WITHOUT A KEYPRESS, YOU NEED `VTWBUF`

**MEASURED 2026-08-25.** VTM buffers its output and flushes when a key is read. A screen program
that only redraws in response to keystrokes never notices. **A timer does**, and the symptom is
that the timer looks broken rather than the screen.

```planc
IMPORT ( ROUTINE VOID, INTEGER : VTWBUF )     % takes NO arguments
...
VTWBUF =: st                                   % after drawing, before going back to sleep
```

The evidence that it is a FLUSH and not a lost update: with the timer apparently dead, one keypress
made three lines appear at once, two of them stamped seconds earlier. They had been drawn on time
and held.

---

## 7. What this adds up to

**VTM gives you a real window primitive, and it is better than expected on the one axis that
matters for safety.** Position-independent drawing was the hoped-for win; clipping is the one worth
more. A window is a box you cannot write outside of, whose overflow is reported back to you as a
number.

Design rules that follow, all of them measured rather than reasoned:

1. **Enter a window with absolute coordinates, always.** `VTDSCR` never nests.
2. **Draw at local coordinates 1,1..high,wide.** Alignment arithmetic becomes a property of the
   text, not of the window's position on screen.
3. **Leave with `VTDSCR(1,1,-1,-1)`** before touching anything outside - a status line, a prompt,
   another window.
4. **Trust the clip, but read the length back** when it matters whether the text fitted.
5. **Closing a window still means repainting what it covered.** VTM saves nothing underneath, and
   this measurement does not change that.
6. **Call `VTWBUF` after any drawing that is not followed by a keystroke** - see section 6c. VTM
   buffers, and a read is what flushes it.

## 8. Still not known

- **Whether `VTDSCR` validates the rectangle itself.** All four rectangles here were legal. A
  rectangle that runs off the screen, or one with the corners the wrong way round, was not tried -
  `4121` on the transposed 2026-08-24 attempt suggests it does check, but that came back from a
  call made with the arguments in the wrong ORDER, so it is one observation and not a test.
- **`VTWREP`'s two extra arguments.** Six arguments derived, `VTWRIT`'s five plus one; no caller
  found.
- **Whether the clip is per-write or per-line.** A single 30-character write was clipped at the box
  edge. A write that STARTS outside and ends inside was not tried.

---

## See also

- [PLANC-INTERACTIVE-SCREEN-PATTERNS.md](PLANC-INTERACTIVE-SCREEN-PATTERNS.md) - the whole program
  shape, with the viewport as one of six patterns
- [VTM-API-REFERENCE.md](VTM-API-REFERENCE.md) - all 37 routines
- [PLANC-UI-VTM-GUIDE.md](PLANC-UI-VTM-GUIDE.md) - PLANC-SCREEN-H, the drawing layer
