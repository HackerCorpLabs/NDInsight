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

## NEVER TYPE `CHANGE-RT-DESCRIPTION` ON ONE LINE. Answer the prompts.

This is the trap that cost the most time on 2026-08-20, because what it produces looks like a
program bug and nothing like a typing mistake.

`CHANGE-RT-DESCRIPTION` takes its parameters in this order, and there are **eight** of them:

```
RT-PROGRAM  PRIORITY  SEGMENT-ONE  SEGMENT-TWO  START-ADDRESS  RING  INITIAL-PAGE-TABLE  ALTERNATIVE-PAGE-TABLE
```

**Slot 5 is START ADDRESS, not RING.** Typed as a single line,

```
CHANGE-RT-DESCRIPTION CHATSER,75,2520,,2,
```

the `2` that was meant as the ring lands in START ADDRESS. The loader accepts it without a word.
`@RT CHATSER` then starts the program at address 2B - inside its own data - and the machine
answers

```
- STACK OVERFLOW AT 003217B
```

which reads exactly like a runaway routine in the program. It is not. `LIST-RT-DESCRIPTION`
tells the truth in one line:

```
START ADDRESS:  2B          <- wrong; a real one here is 10525B
```

**A blank does not repair a description that is already wrong.** Re-running the command with the
slot left empty keeps whatever is there; empty means "leave as loaded", and 2B has been loaded.
The segment has to be **LOADed again** - `NEW-SEGMENT`, `LOAD`, the libraries, `WRITE-REFERENCES`,
`END-LOAD` - and only then is the description set from a fresh start address.

So: type the verb alone and answer each prompt, leaving **START ADDRESS blank** and giving the
ring at the `RING:` prompt. Two of the eight prompts (`INITIAL PAGE TABLE`, `ALTERNATIVE PAGE
TABLE`) come after RING and are easy to walk past - a command typed while one of them is waiting
is eaten as a parameter and answers `ILLEGAL PARAMETER TYPE`, which aborts the whole thing.

**The same shape bites `NEW-SEGMENT`, which takes only FIVE parameters:**

```
NEW-SEGMENT <seg>,<ring>,<type>,<protection>,<WP/NP>
```

`NEW-SEGMENT 2521,2` leaves three prompts unanswered, and the NEXT COMMAND LINE is swallowed as
the answer to `SEGMENT TYPE:` - so `LOAD CHATSV,2521,` disappears and the loader answers
`PARAMETER NO. 3 IS ILLEGAL`. Write `NEW-SEGMENT 2521,2,,,` with the trailing commas.

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

## The diagnostics now go to a file - and the file needs EVEN PARITY

Done on 2026-08-20. Every diagnostic goes to `CHATLG2:SYMB` through `logLine`, opened with
`MON50`, written with `MON2` and closed with `MON43`, **one line at a time**: a crash leaves an
open file unflushed, and a crash is exactly when the log has to survive. Access **5** is
sequential-write append; access 0 truncates at CLOSE, so each line would erase the one before it.
Nothing in the logger can kill the server - a failure sets `logBroken` and it stops trying.

**Two things about the file that were measured, not assumed:**

**An RT program's default user is RT, not SYSTEM.** An unqualified name lands in
`(PACK-ONE:RT)`. SYSTEM cannot delete it or change its access - both answer
`NOT DIRECTORY ACCESS` - so it can only be read by copying it out:
`@COPY-FILE "SVLOG:SYMB",(RT)CHATLG2:SYMB`. The short local name matters too: **FA refuses a
name over 13 characters**, so `(RT)CHATLG2:SYMB` cannot be pulled directly.

**LIST-FILE AND QED ARE NOT A TEST OF A FILE - this was got wrong and cost two hours.** The log
filled up correctly, and then

```
@LIST-FILE SVLOG:SYMB,,     printed the header and NOTHING else
@QED  R SVLOG:SYMB          PARITY ERROR AT LINE 1, thirteen times
```

which was read as "the bytes are wrong" and answered with an even-parity routine. **The control
run kills it:** the same two commands against `CHATSV:MODE` - the build file `@MODE` compiles this
whole server from - answer *exactly the same way*. Both tools do this to every file our FA push
puts on the machine. The parity code has been taken out; it did not even do what it claimed,
setting bit 7 on **every** byte.

**To read the log, pull it off the machine and look at the bytes:**

```
@COPY-FILE "SVLOG2:SYMB",(RT)CHATLG2:SYMB
Xmsg.Live.Runner --config topology-d19999-hdlc-server.json \
    --pull 'SVLOG2:SYMB' --pull-from 100 --pull-to svlog2.raw --originate-from-seed
```

That worked first time, and it is how the broadcast question was finally answered:

```
SV bcast sent to  0     OLAV joins, nobody else there
SV bcast sent to  1     ANNA joins, OLAV told
SV bcast sent to  2     OLAV speaks - sent to BOTH
```

The log name is `CHATLG2` rather than `CHATLOG` only because the older file already existed and
SYSTEM cannot delete a file owned by RT.

## SET-ERROR-DEVICE - what it can and cannot do

Asked on 2026-08-20: can SINTRAN be told to send errors to a file, or to a terminal we can watch?
From `Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md` and
`Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md`:

```
@SET-ERROR-DEVICE <device name>      permitted only for user SYSTEM
@GET-ERROR-DEVICE                    reports the current one
```

**It names a DEVICE, not a file.** So it cannot be pointed at `CHATLG2:SYMB`, and it is not a
replacement for the file log. What it CAN do is put SINTRAN's own error messages - the
`- STACK OVERFLOW AT 003217B` class, which is how an RT program's death is announced - on a
terminal we are watching, instead of wherever they go now. For a fault that kills the program
before it can write its own line, that is the only report there is.

Three monitor calls go with it, all confirmed in the call list:

| Call | Number | What it does |
|---|---|---|
| `GetErrorDevice` (`GERDV`) | 254B | Returns the **LDN** of the error device, and the RT description address of whoever reserved it |
| `ToErrorDevice` (`ERMON`) | 142B | Writes an error there - but it takes an **error number and a suberror number**, two integers. NO TEXT. |
| `GetErrorInfo` (`RERRP`) | 207B | 12 bytes describing the last error termination |

**`ERMON` cannot print a sentence** - it is numbers only, so it cannot carry `SV bcast sent to 2`.
The way to get free text onto the error device is `GERDV` for the LDN and then ordinary byte
output to that LDN, which is exactly what `logPut` already does to a file device. UNVERIFIED so
far: neither call has been made from this program yet, and `MONITOR_CALL`'s argument shape for a
call with two OUTPUT parameters has not been checked.

## Still to do
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
