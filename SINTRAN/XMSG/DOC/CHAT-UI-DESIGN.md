# NDCHAT full-screen UI - the main window

**Status: PLAN, nothing built.** Geometry measured from Ronny's mockup 2026-08-25. Every
technique named here is proved in `SINTRAN/XMSG/TESTUI/TESTUI.PLNC`; nothing below needs a
technique we have not already run on D100.

**Read first:** `Developer/Languages/Application/PLANC-INTERACTIVE-SCREEN-PATTERNS.md` - the
polling loop, the flush, the clipping, the traps. This page is the chat client's layout and the
decisions specific to it.

---

## 1. THE RULE THAT SHAPES EVERYTHING: the line mode stays

**A terminal that cannot position the cursor gets exactly what it gets today**, unchanged. This
is not a courtesy - it is how the program stays usable on a printing terminal, and it is decided
by asking the terminal, never by a setting:

```planc
blankscreen                                  % VTM's first call - CTYTP is only valid AFTER this
MON16(1) =: termType
TRUE =: screenOk
IF (termType AND bitVdu)  = 0 THEN FALSE =: screenOk ENDIF
IF (termType AND bitCpos) = 0 THEN FALSE =: screenOk ENDIF
```

So the client has **two renderers over one protocol**:

```
        XMSG receive loop, command parsing, room state
                      |
        +-------------+-------------+
        |                           |
   line renderer               screen renderer
   OUTPUT(1,'AL',...)          VTM + PLANC-SCREEN-H
   what exists today           this document
```

**Nothing in the wire protocol changes.** The kinds, the seats, the trunk relay - all untouched.
The screen is a VIEW. That is what makes this safe to build incrementally: the line renderer
remains the fallback and the reference for what the screen ought to be showing.

**Design rule that follows:** every place the client prints today becomes a call to a small
`show*` routine. The renderer is chosen once, at start-up, and the rest of the program never asks
again.

---

## 2. THE THREE SECTIONS, AND THE GEOMETRY, MEASURED

The mockup is **80 columns by 24 rows exactly** - a standard screen with nothing spare.

```
 row  1   TOP     frame top edge, with the title in it
 row  2   TOP     status line
 row  3           separator
 rows 4-20 MIDDLE the scrolling chat - 17 visible lines
 row 21           separator
 row 22   BOTTOM  input line
 row 23   BOTTOM  window bar
 row 24           frame bottom edge
```

### The message row, column by column

| Field | Columns | Width | Note |
|---|---|---|---|
| border | 1 | 1 | |
| gap | 2 | 1 | |
| **time** | 3-7 | **5** | `HH:MM` |
| gap | 8-9 | 2 | |
| **speaker** | 10-22 | **13** | `NICK@SYS`, or `*` for an event |
| **text** | 23-79 | **57** | |
| border | 80 | 1 | |

**57 characters of text.** A line longer than that has to wrap or be cut, and that is a decision
in section 6.

### THE SPEAKER FIELD IS THE ONE PROBLEM IN THE LAYOUT

**13 columns, and the server allows a 16-character nickname.** From `CHATSV.PLNC`:

```planc
INTEGER     : maxNameLen := 16
BYTES ARRAY : memberName(1:16, 1:16)
```

So the worst case is `SIXTEENCHARSNAME@D103` - **21 characters into a 13-column field**. The
mockup's own examples fit because they are short: `KARI@NORD` is 9, `TERJE@FJELL` and
`SIGRID@VEST` are 11. `SYSTEM@D103`, which is what our machines actually produce today, is 11.

This needs deciding before the field is written, not after - see section 7.

---

## 3. TOP SECTION - what it shows and where each part comes from

```
row 1:  frame title    NDCHAT 2.1                    ARNE@NORD
row 2:  #sintran-dev@trunk   14 here   Use /help ...   Fri 21 Aug 09:14
```

| Element | Source | Have it? |
|---|---|---|
| our nick and system | the client already knows both | **yes** |
| current room | client state | **yes** |
| `@trunk` marker | room has remote members | derivable from the member list |
| `14 here` | member count | **yes** - `/who` already gets it |
| hint text | constant | trivial |
| date and time | `MN113(cal)` - 7 elements | **yes**, and the trap is known |

**The clock trap, already paid for once:** `MN113`'s year element is the FULL year, 1998, not 98.
`digits(1998 / 10)` is a read 190 bytes out of bounds and PLANC checks nothing - it printed
`24-08-     8`. Clamp with `MOD 100` inside the helper, never at the call sites.

**Row 2 is cheap to repaint and worth repainting on a clock** - it is one line, about 78
characters. The whole screen is about 2000.

---

## 4. MIDDLE SECTION - the scrolling chat

**This is TESTUI's scrolling window with a wider text field and a much bigger buffer behind it.**
The painting is the part already built and measured; the cache and paging below are new. The
window shows 17 lines out of a cache of many:

```planc
INTEGER : scrLines    := 17          % visible rows, 4..20
INTEGER : scrTextWide := 77          % cols 3..79 - the whole inside width
BYTES   : scrBuf(0:1308)             % 17 x 77
INTEGER : scrCount                   % lines held
INTEGER : scrFirst                   % slot holding the OLDEST
```

**Store each line already FORMATTED and PADDED to full width.** Time, speaker and text are
composed once when the line arrives and never again. Two reasons, both measured:

- a short line painted over a long one leaves the old tail on screen unless you erase first;
  padding removes the erase.
- scrolling becomes one index change and **no text is copied at all**.

**Only a FULL window costs a repaint.** While there is still room the new line goes on the next
free row - 77 characters. Once full, every line moves up and it costs about 1300. That is the
single most expensive thing this UI does routinely and it is worth knowing the number.

**FLUSH AFTER EVERY ARRIVAL.** A message arriving over XMSG has no keystroke behind it, so
without `VTWBUF` the line goes into VTM's buffer and stays there. The symptom is not a stale
screen - it looks like the chat has stopped receiving, and then several lines appear at once when
the user presses something.

### SCROLLBACK - the cache, and paging through it

**Ronny, 2026-08-25: the chat must be scrollable and cached; PAGE UP and PAGE DOWN navigate the
buffer.** That makes the ring buffer a CACHE that is much larger than the window, plus a view
position - not the 17-slot ring above.

```planc
INTEGER : cacheLines  := 200          % lines KEPT  - see the size arithmetic below
INTEGER : scrLines    := 17           % lines SHOWN
INTEGER : lineWide    := 77
BYTES   : cache(0:15399)              % 200 x 77
INTEGER : cacheCount                  % lines held, 0..cacheLines
INTEGER : cacheFirst                  % slot holding the OLDEST
INTEGER : viewFrom                    % how many lines UP from the newest we are looking
```

**`viewFrom = 0` means live.** Any other value means the user has paged back. That single
variable decides everything below, and keeping it as "distance from the newest" rather than an
absolute slot means new arrivals do not have to adjust it.

**THE RULE WHEN A MESSAGE ARRIVES WHILE PAGED BACK: DO NOT MOVE THE VIEW.** Yanking the screen
to the bottom while somebody is reading is the single most annoying thing a chat client can do.
The line goes into the cache, `viewFrom` is untouched, and the fact that there is something new
below is shown - the status line already has room for it.

**Coming back to live:** paging down to the bottom, or sending a line. Sending should always
return to live, because the user is about to want to see their own message.

**The size arithmetic, because this is a one-bank program.** 77 bytes a line:

| Lines kept | Bytes | Words |
|---|---|---|
| 100 | 7700 | 3850 |
| 200 | 15400 | 7700 |
| 500 | 38500 | 19250 |

**Check it against the linker, do not guess.** `BRF-LINKER`'s `FREE: P nnnnnn-177777` line after
loading the client and its libraries is the real answer, and the client is already a large
program. 200 lines is the suggested starting point; it is one constant.

**Paging is a repaint of the middle section and nothing else** - about 1300 characters. The top
and bottom sections do not move, so a page is cheaper than it looks.

### DECODING PAGE UP AND PAGE DOWN - not settled, and it is the hard part

**There is no easy answer here and it must not be guessed.** Two routes:

**Route A - `VTFUNC`, the ND-native one.** VTM has a function-key call, 3 arguments with the
second written back. It is marked **INFERRED in `VTM-API-REFERENCE.md`: derived from the binary,
and NO CALLER HAS EVER BEEN FOUND.** If it works it is the right answer, because VTM knows what
the terminal's keys actually send and we would not have to care. **Nothing about it is proved.**

**Route B - decode the bytes ourselves.** We already own ESC (`MON71` took it from SINTRAN), so
byte 27 arrives at `pollKey` like any other. On a VT100, PAGE UP is `ESC [ 5 ~` and PAGE DOWN is
`ESC [ 6 ~`. A small state machine in `pollKey` collects them.

**Route B has a real ambiguity and it is worth naming before it bites:** ESC is currently the
QUIT key, and now ESC is also the first byte of a key sequence. Telling "the user pressed ESC"
from "the terminal is sending ESC [ 5 ~" means either a timeout - wait a few hundredths for a
following `[` - or giving up ESC as the quit key. **A timeout on a poll loop is easy** (we
already read the clock with `MON11`) but it is a real behaviour change and should be decided,
not stumbled into.

**A third option worth considering:** bind paging to plain letters as well, so the feature works
on every terminal regardless of what its keypad sends. Whatever PAGE UP does, `/up` or a control
key should do too - the line-mode renderer has no function keys at all and still deserves
scrollback.

### Event lines

The mockup shows `*` in the speaker field for events - joins, window notices, idle notices. These
are a **different line kind, not a different renderer**: build the same 77-character line with `*`
in the speaker column. Worth a marker byte per slot if events should ever be filtered or coloured.

---

## 5. BOTTOM SECTION - input line and window bar

```
row 22:  > /view users_
row 23:    1 #sintran-dev   2 #ops 2   3 #nd-500   4 =KARI 1*   5 =TERJE
```

**The input line is the part with no precedent in anything we have built.** TESTUI reads single
keys; this needs a line editor:

| Needs | Note |
|---|---|
| echo the character at the right place | we own the echo already - `MON3(1,-1,...)` |
| backspace | redraw from the cursor, or just repaint the field |
| the cursor visible at the end | `VTPCUR` - **(line, position), row first** |
| RETURN sends | existing command parser takes it unchanged |
| a full field | 76 columns; decide whether to scroll or refuse |

**The parking trick changes meaning here.** In TESTUI the cursor is parked somewhere harmless
because stray echoes landed inside windows. Here there IS a right place for the cursor - the end
of the input text - so the input line owns the cursor and every repaint must put it back.

**The window bar is display-only for now**, and it is what tells us multi-window is coming:
`#` for a room, `=` for a direct message, a count for unread, `*` for a highlight. Section 8.

---

## 6. WHAT IS NOT SETTLED, AND MUST NOT BE GUESSED

**The box-drawing characters in the mockup cannot go on the wire.** `╔ ═ ║ ╠ ╣ ╚` are Unicode;
an ND terminal has no such glyphs and our sources are 7-bit ASCII with CRLF. **The border comes
from PLANC-SCREEN-H's `frame`**, which draws it with whatever the terminal type provides through
VTM. What `frame` actually emits for a given terminal has **not been measured** - TESTUI draws
frames and they look right, but nobody has recorded the bytes.

So: the mockup is the LAYOUT, not the glyphs. Treat the drawn border as "whatever `frame` gives
us" and do not try to reproduce the double-line look.

**Three separators at rows 3 and 21.** `frame` draws a box, not a box with cross-pieces. Either
draw three separate frames (top, middle, bottom) stacked so their edges touch, or draw one frame
and paint horizontal rules inside it. **Which of those looks right has not been tried.** The
three-frame version has the advantage that each section is already a viewport.

**Line wrapping.** 57 columns of text is narrow. A long line can be cut, wrapped onto a
continuation row with a blank speaker field, or scrolled sideways. Not decided.

---

## 7. THE DECISIONS THAT NEED RONNY

1. **The speaker field is 13 wide and a nickname can be 16.** Truncate the nick, shorten the
   system, drop `@SYS` for local speakers, or widen the field at the cost of text.
2. **Where the input cursor lives when a message arrives** - the arrival repaints the middle
   section, and the cursor has to go back to the input line afterwards, every time.
3. **How PAGE UP and PAGE DOWN are read** - try the unproven `VTFUNC`, decode escape
   sequences ourselves and solve the ESC-versus-sequence ambiguity, or bind paging to ordinary
   keys that work on every terminal including the line-mode one.
4. **How many lines the cache keeps** - 200 is a starting point; the linker's `FREE` line decides
   what is affordable.
5. **Scope of the first build** - the main window alone, with the window bar drawn but inert?

---

## 8. WHERE MULTI-WINDOW PLUGS IN

The bottom bar implies several buffers: `1 #sintran-dev  2 #ops  3 #nd-500  4 =KARI  5 =TERJE`.
Nothing in this design blocks it, and the shape is already right:

- **one ring buffer per window** instead of one - the paint code does not change, only which
  buffer it reads;
- the window bar becomes live, showing unread counts per buffer;
- switching windows repaints the middle section only - 1300 characters, not 2000.

**What it will need that we do not have:** a per-window unread count, a notion of the active
window, and a key or command to switch. The `*` highlight marker in the mockup suggests
"mentions my nick", which the client can test as each line arrives.

**Do not build the buffers as a general window manager.** TESTUI already showed that overlapping
windows need clipping, a stack order and three consumers of it. These windows do not overlap -
they are alternative contents for one rectangle, which is far simpler and should stay that way.

---

## See also

- `Developer/Languages/Application/PLANC-INTERACTIVE-SCREEN-PATTERNS.md` - the patterns
- `Developer/Languages/Application/VTM-VIEWPORT-HOW-TO-USE-IT.md` - `VTDSCR`
- `SINTRAN/XMSG/TESTUI/TESTUI.PLNC` - the worked example
- `DOC/CHAT-FEDERATION-DESIGN.md` - the protocol this is a view of
