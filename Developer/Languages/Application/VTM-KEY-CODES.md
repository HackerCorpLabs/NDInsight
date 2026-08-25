# VTM key codes - the lookup table

**What this is.** What `VTINBT` hands back for each key, measured on D100 on 2026-08-25 with
`SINTRAN/XMSG/SINTRAN-CHAT/KEYPROB.PLNC`. **No ND document describes any of this** - VTM's manual
number is literally "Internal" (`ND-20034-1-EN`:1248) - so this page is the only record there is.

**Read `VTM-API-REFERENCE.md` first** for how to call `VTINBT`. Two traps there will make it look
as though VTM decodes nothing: type it as three values IN and one OUT, and pass VARIABLES rather
than literals.

---

## 1. How this was measured, so it can be repeated

```
1. Set the line's terminal type      @SET-TERMINAL-TYPE ,6      (or ,53 for a TDV)
2. Open RetroTerm in the matching emulation - VT100 or TDV2200
3. @KEYPROB
4. Send each key FOLLOWED BY a full stop as a marker
5. Q to stop; the codes print on the way out, in order
```

**THE MARKER IS WHAT MAKES IT TRUSTWORTHY.** A full stop is code 46, so a correct run reads
`key 46 key 46 ...`. If the count is wrong, the reading is wrong - and it caught a real fault
here, see `ESC[42_` below. **Send at most five keys per run and check the count**; a twenty-key
sweep drifted silently and produced a table that was wrong from the middle onwards.

**Anchor every sweep on a key you already know.** `ESC[46_` is HJELP and must give 191; `ESC[30_`
is ANGRE and must give 216. If an anchor lands in the wrong place, discard the run.

---

## 2. DEC VT100 - terminal type 6

### Cursor and editing keys

| Key | what the terminal sends | VTM code | seen |
|---|---|---|---|
| cursor UP | `ESC [ A` (ANSI normal mode) | **28** | 3 runs |
| cursor DOWN | `ESC [ B` | **11** | 2 runs |
| cursor RIGHT | `ESC [ C` | **24** | 1 |
| cursor LEFT | `ESC [ D` | **8** | 1 |
| FIND | `ESC [ 1 ~` | **130** | 1 |
| INSERT HERE | `ESC [ 2 ~` | **133** | 1 |
| REMOVE | `ESC [ 3 ~` | **129** | 1 |
| SELECT | `ESC [ 4 ~` | **160** | 1 |
| **PREV SCREEN** (page up) | `ESC [ 5 ~` | **201** | **4 runs** |
| **NEXT SCREEN** (page down) | `ESC [ 6 ~` | **197** | 3 runs |

**THE KEY NAMES ABOVE ARE DEC'S, CHECKED AGAINST THE VT220 MANUAL** (vt100.net, VT220
Programmer Reference, chapter 3, table 3-1). An earlier version of this page called them
HOME, DELETE and END. **Those are xterm's names, not DEC's**, and the distinction is not
pedantry - a VT220 keyboard has keys physically labelled FIND, SELECT and REMOVE, and
anyone looking for HOME on one will not find it.

**AND THESE ARE VT220 KEYS, NOT VT100 KEYS.** The same manual says plainly: *"In VT100 or
VT52 modes the editing keys do not generate codes."* A real DEC VT100 has no editing keypad
at all and can never send any of these six sequences.

That makes the measurement above interesting rather than wrong: **SINTRAN terminal type 6 is
"DEC VT100 (80 columns)", and VTM decoded all six anyway.** So VTM's type-6 table accepts
sequences a genuine VT100 could not produce. Useful in practice - a modern emulator sends
them and they work - but it means the type-6 entry is not a strict VT100. SINTRAN's own list
has separate entries for the VT220 at types 131 and 132; whether those decode differently has
NOT been tested.

### Ordinary and control keys

| Key | byte sent | VTM code | note |
|---|---|---|---|
| any printable | its ASCII | the same | parity already stripped - `A` is 65, not 193 |
| TAB | 9 | 9 | passes through |
| RETURN | 13 | 13 | passes through |
| Ctrl-A | 1 | 1 | passes through |
| Ctrl-C | 3 | **3** | passes through - usable as a quit key |
| Ctrl-Z | 26 | 26 | passes through |
| **Ctrl-H / Backspace** | 8 | **127** | **TRANSLATED**, see below |

**Ctrl-H DOES NOT COLLIDE WITH LEFT ARROW, and that is worth knowing because it looks as though it
must.** Left arrow reports 8. Backspace also sends byte 8 - but VTM turns it into **127**. So the
two are distinguishable, and a program that treats 8 as "left arrow" is correct.

### ALT IS NOT USABLE

`ESC` + `a` came back as **two codes, 27 then 97** - VTM does not recognise it and the bytes fall
straight through. **Do not design around Alt.** Whatever it is on the user's keyboard, VTM will
not tell you about it.

### The `ESC O x` family - what it actually is, and why it is NOT settled

`ESC O` is **SS3**, and in ANSI *application* mode it prefixes the auxiliary keypad keys
(VT220 manual, table 3-3):

| sequence | key |
|---|---|
| `ESC O P` | **PF1** |
| `ESC O Q` | **PF2** |
| `ESC O R` | **PF3** |
| `ESC O S` | **PF4** |

**These are the numeric keypad's PF keys, NOT the function keys F1 to F4** - another label this
page had wrong. On a VT220 the keys marked F1 to F5 are *local* (Hold Screen, Print Screen,
Set-Up, Data/Talk, Break) and **send nothing at all**.

`ESC O` also prefixes the CURSOR keys when the terminal is in application mode - `SS3 A/B/C/D`
rather than `CSI A/B/C/D`. Since VTM sends `ESC =` (keypad application mode) during start-up,
which of the two forms a given terminal actually sends is a live question and not one this page
can answer yet.

**Measured, and inconsistent:** `ESC O P` gave 29, but `ESC O Q` returned TWO codes in the same
run. One clean per-key run is needed before any of it is written down. Left here as a warning,
not as data.

---

## 3. Tandberg TDV 2200/9 ND-NOTIS - terminal type 53

**Every special key on this terminal sends `ESC [ <n> _`**, so the whole keyboard can be swept by
varying `n` rather than by finding a name for each key.

| `n` | VTM code | key, where known |
|---|---|---|
| 30 | **216** | ANGRE (undo) |
| 31 | 209 | |
| 32 | 139 | |
| 33 | 152 | |
| 34 | 194 | |
| 35 | 146 | |
| 36 | 206 | |
| 37 | 134 | |
| 38 | 25 | |
| 39 | 214 | |
| 40 | 9 | |
| 41 | 203 | |
| 42 | **174** | **EATS THE NEXT BYTE - see below** |
| 43 | 159 | |
| 44 | 161 | |
| 45 | 162 | |
| 46 | **191** | HJELP (help), also reachable as grid key G53 |
| 47 | 165 | |
| 48 | 163 | grid key G54 - legend not yet identified |
| 49 | 164 | |
| 50 | **132** | F1 |
| 51 | 193 | SHIFT-F1 |
| 52 | 140 | F2 |
| 55 | 149 | F3 |
| 58 | 171 | F4 |
| 60 | 217 | F5 |
| 62 | 204 | F6 |
| 64 | 220 | F7 |
| 66 | 221 | F8 |

### A KEY HAS TWO NAMES: ITS LEGEND AND ITS GRID POSITION

**This confused me for half an hour and it is not a defect.** `HJELP` and `G53` send the same
`ESC[46_` and give the same 191 because **they are the same key**: `HJELP` is the legend printed
on it, `G53` is where it sits on the keyboard grid. Same for `F1` and `F51`, `F2` and `F52`,
`F3` and `F53`, `F4` and `F54` - one key, two names, identical bytes.

**And that is why the F-names stop at F54.** The F5..F8 legends sit on grid row **E**, not row F,
so they are `E51`..`E54`. There is no `F55`. The table is not half filled in; the F row simply has
four keys on it.

Confirmed by the RetroTerm session on 2026-08-25 from its own key registry - `Reg("F51", "F1")`
and `Reg("E51", "F5")`. **Do not report the duplication as a bug; it was already reported and it
is correct behaviour.** What WAS wrong was RetroTerm's parameter text saying "F1..F52", which
mixes the two naming systems into a range that matches neither. That has been corrected.

### SHIFT IS IN THE SEQUENCE NUMBER, NOT A MODIFIER

F1 is `ESC[50_` and SHIFT-F1 is `ESC[51_` - a different key as far as the wire is concerned, and a
different code (132 against 193). So **the sweep above already covers the shifted keys**; they are
simply other values of `n`. There is no modifier bit to combine with anything.

The F-key numbers are not evenly spaced - 50, 52, 55, 58, 60, 62, 64, 66 - and the codes they
produce are not ordered at all (132, 140, 149, 171, 217, 204, 220, 221). It is a lookup, not a
formula. Do not try to compute one.

### `ESC[42_` SWALLOWS THE FOLLOWING BYTE

MEASURED twice, on purpose. Sent alone with one marker after it, the marker disappeared entirely.
Sent with THREE markers, only TWO came back:

```
ESC[42_ . . .   ->   174, 46, 46
```

So it consumes exactly one extra byte. This is what silently corrupted a twenty-key sweep, and it
is why the marker technique and the count check exist. Whatever key that is, a program reading it
will lose the keystroke after it.

---

## 4. THE CODES ARE NOT THE SAME ACROSS TERMINALS - do not assume they are

It is tempting to think VTM normalises every terminal onto one logical key set. **That is NOT
established, and the evidence so far is against it.** None of the 29 TDV codes measured is 201,
the VT100's PAGE UP. The sweeps do not overlap in an obvious way at all.

Two honest qualifications:

 - the TDV sweep covered `n` = 30..52 and a handful of F-keys, NOT the whole range, so a match
   could be sitting at an `n` nobody has tried;
 - a TDV has keys a VT100 does not have and the reverse, so a complete mapping cannot exist
   anyway.

**Until somebody sweeps both terminals exhaustively, treat the codes as PER TERMINAL TYPE.** A
program that must work on both should read its key bindings from a table chosen by terminal type,
not hard-code one number per logical key.

**And VTM only decodes the terminal it has been told it is talking to.** The VT100 `ESC [ 5 ~`
sent to a line set to type 53 came back as four raw bytes - 27, 91, 53, 126 - undecoded. Two
consequences:

 - **never treat a stray 27 as a key**; it may be the head of a sequence VTM did not recognise and
   the rest is arriving as separate calls;
 - **a bare ESC is not a dependable quit key**. On a VT100 line, pressing ESC alone did not produce
   27 at all. `CHATUI` bound its exit to it, became impossible to leave, and its terminal had to be
   freed with `STOP-TERMINAL` from another session. **Use a typed command such as `/exit`.**

---

## 5. Two practical notes

**The rubbish at start-up is VTM's terminal-type negotiation.** A program that takes the keyboard
straight after `blankscreen` reads about eight junk bytes - `63 63 128 103 0 29 63 63` - and they
land in whatever it thinks is its input. On a line whose type was already set with
`SET-TERMINAL-TYPE`, **none of them appear**. Either set the type in advance or drain the input
before taking the keyboard.

**RetroTerm keeps the two emulations properly separate**, so a VT100 session cannot accidentally
send TDV keys - `terminal_sendkey` refuses outright:

```
SENDKEY needs a TDV emulator (current: VT100Emulator) - use SENDRAW for other terminals
```

That was checked deliberately, because if the emulator had been sending TDV sequences on a VT100
line every VT100 figure above would have been meaningless.

---

## See also

- [VTM-API-REFERENCE.md](VTM-API-REFERENCE.md) - `VTINBT` and the other 36 routines
- [PLANC-INTERACTIVE-SCREEN-PATTERNS.md](PLANC-INTERACTIVE-SCREEN-PATTERNS.md) - the polling loop
- `SINTRAN/XMSG/SINTRAN-CHAT/KEYPROB.PLNC` - the probe, and the way to add to this table
