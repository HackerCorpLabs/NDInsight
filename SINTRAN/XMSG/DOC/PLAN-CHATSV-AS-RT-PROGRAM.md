# CHATSV as an RT program - DONE, and the as-built procedure

**Built and proved on D100, 2026-08-18.** The chat server now runs as RT program `CHATSER` on
segment `2512`, holds no terminal, and serves real clients over the wire.

## The proof

```
@RT CHATSER
@X-COMM
X-C: LIST-NAMES
   100     8      16     CHAT-LOBBY.        <- registered by the RT program
```

and then, from C# over HDLC, three clients saying two lines each:

```
[chatload] users asked to join : 3
[chatload] welcomed            : 3
[chatload] refused by the room : 0
[chatload] never answered      : 0
[chatload] frames from the room: 27
[chatload] Said messages heard : 18        <- 3 users x 2 lines x 3 recipients, exact
[chatload] leaves sent         : 3
```

`CHAT-LOBBY` afterwards: **16 free**, the baseline. Identical behaviour to the terminal version.

**And the terminal was in use for X-COMM throughout.** That is the whole point: the line is free.

## The as-built sequence

```
@RT-LOADER
  LIST-FREE-RT-DESCRIPTIONS      -> first free was 63417
  LIST-FREE-SEGMENTS             -> 2512 upwards free
  NEW-SEGMENT   2512 / RING 2 / type, protection, WP-NP all defaulted
  LOAD          CHATSV          onto 2512, no linking segment
  LOAD          XMP-100-1-B02   onto 2512
  LOAD          PLANC-1BANK-F00 onto 2512
  WRITE-REFERENCES               -> EMPTY, so nothing unresolved
  END-LOAD                       <- MUST come before the next command
  CHANGE-RT-DESCRIPTION  CHATSER / PRIORITY 75 / SEGMENT ONE 2512 / RING 2
                                    (start address and page tables left as loaded)
  EXIT-LOADER
@RT CHATSER
```

## THREE THINGS THE PLAN GOT WRONG, and they are the useful part

**1. `END-LOAD` comes BEFORE `CHANGE-RT-DESCRIPTION`, not after.** The worked example in
`Reference-Manuals/210166F SIBAS II for ND-100.md` shows the opposite order:

```
DECLARE-PROG SIB2A,.........
CHANGE-RT-DES SIB2A,56,SIBPF00,SIB2A;11,1,2,1
END-LOAD
```

Done that way here it answers **`THIS COMMAND IS NOT ALLOWED NOW`**. Issue `END-LOAD` first and the
identical command is accepted without complaint.

**2. `DECLARE-PROGRAM` is not needed - `LOAD` already made the program.** Loading the BRF prints

```
NO PRIORITY IN:  CHATSER
```

which is not an error: it means the RT description exists and has no priority yet. A following
`DECLARE-PROGRAM CHATSER` answers **`RT-PROGRAM ALREADY DEFINED`**. All that is left is to set the
priority and segment.

**3. The RT program is called `CHATSER`, not `CHATSV`.** The name comes from the PLANC
`PROGRAM : chatServer` unit, not from the file. `@RT CHATSV` would find nothing.

**And one thing the plan feared that did not happen: `OUTPUT(1, ...)` did NOT stop it.** The plan
said the terminal writes had to be moved to a log file BEFORE this could work. They did not - the
program loaded, started and served clients with every `OUTPUT` still in place. Moving them to a file
is still worth doing, because those lines are how the seat leak and the burst behaviour were found
and they now go nowhere anybody can read, but it is an improvement rather than a prerequisite.

## Still to do

 - **the diagnostics have nowhere to go.** `SV: reaped dead slot` and `SV: get failed st= 16916`
   each solved a real defect; as an RT program they are written to a device that is not there. Move
   them to a log file with the `MON50`/`MON2`/`MON43` sequence already used for `CHAT:CNFG`.
 - **boot-time start is NOT wired yet.** The two halves are below and untested.

## Starting it at boot

Two files, two different moments - from the `sintran-install` skill, which has this researched:

| File | When | What belongs in it |
|---|---|---|
| `(SYSTEM)HENT-MODE:MODE` | every **COLD** start | rebuild the segment file: the RT-LOADER sequence above |
| `(SYSTEM)LOAD-MODE:BATC` | every **WARM** start | start processes: `@RT CHATSV` |

The chain is wired with

```
NEXT-INITIAL-COMMAND APPEND-BATCH 1 (SYSTEM)LOAD-MODE:BATC SYSTEM-OUTPUT-1
```

**Test it by hand before wiring it.** Run `APPEND-BATCH 1 (SYSTEM)LOAD-MODE:BATC SYSTEM-OUTPUT-1`
yourself first: *a broken initial command runs on every restart with no terminal to stop it.*

**And put `@SET-AVAILABLE` at the end of LOAD-MODE.** `COS-FA-SERV-E04:MODE` finishes with
`SET-UNAVAILABLE`, which locks every login out - measured today, and it cost most of a session.

---

## The one code change RT forces

**`OUTPUT(1, ...)` has no terminal.** Every diagnostic in `CHATSV.PLNC` writes to device 1, and an RT
program has no caller to write to.

Three routes, cheapest first:

 1. **a log FILE**, opened once at start-up with `MON50` and closed on exit. The file I/O is already
    written for `CHAT:CNFG` and documented in
    `Developer/Languages/Application/PLANC-XMSG-PROGRAMMING-GUIDE.md`. A server nobody is watching
    wants a log, not a screen - and the reap and refusal lines have each already earned their keep.
 2. **a fixed terminal number**, if a spare line is acceptable - but that gives back the very thing
    this task is trying to reclaim.
 3. **drop them.** Cheapest to write, worst to live with: `SV: reaped dead slot` and
   `SV: get failed st= 16916` are the two lines that explained the seat leak and the burst
   behaviour, and neither would have been found without them.

**Recommended: 1.**

---

## Proving it

The same measurement that proved the seat fix, because it exercises join, talk, leave and the seat
accounting in one go:

```
Xmsg.Live.Runner.exe --self 19999 --originate-from-seed \
    --chat-load 3 --chat-room CHAT-LOBBY --chat-to 100 --chat-lines 2 127.0.0.1 10362 x 120
```

Expect `Said messages heard: 18` (3 users x 2 lines x 3 recipients) and `CHAT-LOBBY` back at its
starting seat count. Then the RT-specific half:

 - `@LIST-RT-DESCRIPTION` shows it;
 - it is still there after every terminal has been logged out;
 - it survives a warm start, because `LOAD-MODE` started it.

---

## What is NOT established

 - the segment number, the RT description and the priority - all machine state, all to be read with
   `LIST-FREE-SEGMENTS` and `LIST-FREE-RT-DESCRIPTIONS` on the day;
 - whether the two-step compile still suits an RT load, or whether the RT loader wants the pieces
   fed to it directly rather than through `BRF-LINKER-C01`;
 - what device number, if any, an RT program may usefully write to on this machine.

None of these can be settled from here, and guessing at them is what the rest of this project has
repeatedly paid for.
