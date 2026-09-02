# Splitting CHAT and CHATSV into separately compiled modules

**Why:** one 6586-line source and one 6875-line source each take about twenty minutes to
compile. That is the tax on every experiment, and it is what makes adding one print statement
feel more expensive than one more round of guessing. Separate compilation is **proved on this
machine** - 8 seconds against twenty minutes - and it is already how everything else here is
built: the linker already loads `CHAT`, `CHATLIB`, `XMP-100-1-B02`, `MON-CALL-1B-A00` and
`PLANC-1BANK-F00` as separate BRFs.

This was recommendation #1 of the 2026-08-26 friction review. It was not done, and both files
have grown since.

---

## What was measured, before any seam was chosen

Counted from the sources, not remembered:

| | CHAT.PLNC | CHATSV.PLNC |
|---|---|---|
| lines | 6586 | 6875 |
| lines **inside** routines | 4200 | 3611 |
| routines | 70 | 59 |
| module-level variables | 82 | 164 |
| variables touched by exactly ONE routine | 43 | 71 |
| top-level names colliding at 7 characters | 6 pairs | 12 pairs |

**The important number is the third row.** Roughly a third of each file is declarations and
comment blocks, not code - so the compile is not dominated by one giant routine, and splitting
by subject will actually divide the work.

**The second important number is the fifth row.** Half the module variables are private to a
single routine. The genuinely shared state is small:

- **CHAT**: `myName`, `lenMyName`, `typed`, `typedLen`, `inBuf`, `outBuf`, `joined`,
  `roomName`, `lenRoomName`, `spaceByte`, `returnStatus`, `oneChar`, `myPort`, `myLdn`,
  `maxNameLen`, `sintranUser`, `running`, `readLength` - about 18 names.
- **CHATSV**: `outBuf` (21 routines), `inBuf` (16), `memberUsed` (12), `maxSeats` (11),
  `peerSystem` (10), `mbrRoom` / `mbrRoomLen` (9), `maxPeers` (9), `spaceByte` (8),
  `peerState` (7), `mySystem` (7), `histSize` (7), `peerUp` (6), `histNext` (6),
  `topicRoom` / `topicRoomLen` (5), `sysName` (5), `pNameLen` (5) - about 25 names.

## The constraint that decides the shape

**A name is unique in TEN characters to the compiler but only SEVEN across a BRF
EXPORT/IMPORT.** Two exports agreeing in seven characters are ONE name to the linker, and it
does **not** report a duplicate - it resolves every import to whichever entry it met first.

Today that barely matters, because the only exports in the link come from `CHATLIB` (53
exports, 0 collisions - it was built for this). **Splitting multiplies the exported surface**,
and the two files already contain 18 colliding pairs internally.

The good news, measured: **with the seams below, five of CHAT's six pairs and most of CHATSV's
stay inside one module**, where they are harmless. The ones that genuinely cross are:

| Pair | Both read | Why it crosses |
|---|---|---|
| `mbrRoom` / `mbrRoomLen` | `MBRROOM` | shared state, 9 routines |
| `topicRoom` / `topicRoomLen` | `TOPICRO` | shared state, 5 routines |
| `nameBuf` / `nameBufSize` | `NAMEBUF` | in both files |

**DONE.** All three renamed, 40 occurrences across the two files:

| was | now | why not the obvious name |
|---|---|---|
| `mbrRoomLen` | `lenMbrRoom` | - |
| `topicRoomLen` | `topicRmLen` | `lenTopicRm` was tried first and **clashes with the existing `lenTopicFull`** at `LENTOPI`. The check caught it before the edit |
| `nameBufSize` | `sizeNameBuf` | `maxNameBuf` clashes with `maxNameLen` at `MAXNAME` |

Every proposed name was checked against all **1432** identifiers in the six sources at both seven
and ten characters before anything was edited. Each rename was then proved complete by count -
zero occurrences of the old name left, and the new name appearing exactly as often as the old one
did. That count is the whole verification, and it has to be, because **the linter's
undeclared-name check only sees names on the receiving side of `=:`** - a missed rename in a read
position would compile perfectly and read whatever happened to be next to it.

**The nine pairs that remain are safe, and that was checked rather than assumed.** Each is used by
one to four routines, and where both members are used it is by the *same* routines - so they land
in one module together and never both cross a seam:
`histHdrNew`/`histHdrOld`, `kAdmTrunk`/`kAdmTrunks`, `machineNum`/`machineByName`,
`maxHistRooms`/`maxHistSize`, `maxTopics`/`maxTopicLen`, `rmtName`/`rmtNameLen`,
`rmtRoom`/`rmtRoomLen`, `sameRoom`/`sameRoomAs`, `topicText`/`topicTextLen`. If a seam ever does
separate one, the cross-file check below is what will say so.

**A safety net went in first.** `tools/planc-lint.py` already refused two exports colliding
*within* one file. It now also refuses them **across a set of files**, which is the case that
actually bites here - neither file is wrong on its own, and neither compile says anything:

```
python tools/planc-lint.py SINTRAN-CHAT/CHAT*.PLNC
```

Proved to fail on a crafted pair while both files report clean individually, and to pass on all
six real sources.

## The proposed seams

Both files are **already organised in this order**, so the cut points are where the subject
changes, not new structure imposed on them.

### CHAT.PLNC - the client

| Module | Contains | Routine lines |
|---|---|---|
| `CHATCOR` | shared state + identity, config file, `readCnfg`/`writeCnfg`/`nameFromSintran`/`setupName`, the join and rename builders | ~570 |
| `CHATDRW` | everything that draws: windows, panel, bars, the say cache, `pollKey` | ~1100 |
| `CHATARR` | `uiArrivedInner`, `uiArrived`, `showArrived` - one subject, and the biggest single routine in the file | ~920 |
| `CHATMN2` | `chatClient`, `handleCommand`, `drainPort`, the terminal calls | ~1450 |

### CHATSV.PLNC - the server

| Module | Contains | Routine lines |
|---|---|---|
| `CHATSCO` | shared state, the log, the small helpers, the message builders | ~700 |
| `CHATROO` | rooms, topics, history, `sendTo`, `broadcast`, `histReplay` | ~900 |
| `CHATPEE` | peers and trunks: `addPeer`, `markPeerUp`, `peerTick`, `machineByName`, `refuseNick` | ~400 |
| `CHATSV` | `handleMessage` (915) and `chatServer` (705) - the core loop keeps the name so the RT program and every build file are unchanged | ~1620 |

**Judgement calls, said plainly:** the four-way split is a choice, not a measurement. The data
says where the subjects change and what is shared; it does not say whether four modules is
better than three. `showArrived` at 637 lines is the one routine large enough to be worth its
own module on its own.

## Order of work

1. ~~Rename the three crossing pairs.~~ **Done** - see above.
2. **Move the shared state into the core module** and give it `EXPORT`. Watch R119 - `IMPORT`
   *and* `EXPORT` must both sit before any ordinary declaration - and R120: a name may be
   `IMPORT`ed only once, and a second is an error, not a warning.
3. **Cut one module off first** - `CHATARR` from the client, because it is the largest single
   subject with the fewest shared names - and compile it alone. One seam proved is worth more
   than four seams guessed.

   **This seam has now been measured, and it is clean.** `uiArrivedInner` + `uiArrived` +
   `showArrived` is 918 routine lines, and exactly **45 names cross it**:

   - **20 module variables to IMPORT**: `askedTopic`, `askedWho`, `cacheCount`, `curWin`,
     `firstWelcome`, `hereCount`, `inBuf`, `joined`, `lenMyMach`, `lenMyName`, `maxNameLen`,
     `myMachine`, `myName`, `oneChar`, `readLength`, `screenOn`, `senderMagic`, `serverMagic`,
     `userChoseNick`, `winUnread`
   - **24 routines to IMPORT**: `buildOwn`, `cacheClear`, `countNames`, `drawAll`, `drawPanel`,
     `flush`, `panelAdd`, `panelAnswer`, `panelClear`, `putInWin`, `putSentInWin`,
     `sendToServer`, `showMyName`, `showRoom`, `uiClear`, `uiFrom`, `uiNotice`, `uiSaid`,
     `uiWho2`, `uiWord`, `winFor`, `winSave`, `winUse`, `writeCnfg`
   - **1 routine to EXPORT back**: `showArrived`, which is all the rest of the client calls in
   - plus the **19 CHATLIB names** the three routines use - `cmKind`, `cmNmLen`, `cmTxAt`,
     `cmTxLen` and fifteen message kinds. Those are not new crossings; the new module simply
     repeats the same `IMPORT` lines, because CHATLIB already exports them.

   **No two of those 45 collide at seven characters.** So the first cut needs no further
   renaming - it is a move plus an `IMPORT` block.

   **CORRECTED 2026-08-28, and the way it was wrong is worth keeping.** This section first said
   33 names, 10 variables and 22 routines, and gave the file as 70 routines and 82 module
   variables. All of those were wrong, from three separate patterns that quietly matched
   nothing:

   - **PLANC continues a line with `&`**, and a routine header often uses it -
     `ROUTINE VOID, VOID (INTEGER, ...) : &` then the name on the next line. The header regex
     was anchored per line, so **every continued routine was invisible**: `putInWin` and
     `uiSaid` are called by the moving code and were missing from the import list entirely.
   - **`INTEGER4` was not in the type alternation**, so `senderMagic`, `serverMagic` and
     `hereCount` were not seen as variables at all.
   - **Module variables were only looked for BEFORE the first routine.** This file declares them
     between routines too, which hid about fifty of them - 136, not 82.

   Each one produced a smaller, tidier number than the truth, and a tidy number is exactly what
   does not get questioned. The rule this breaks is the one already written down: a pattern that
   returns nothing is evidence about the pattern.

   The forms are already proved on this machine by `SPLITA`/`SPLITB` (2026-08-26), which
   deliberately tested the three cases that could each have sunk the split: a scalar written in
   one module and read in the other, a **BYTES array** crossing the boundary - the case the
   manual's own example does not cover - and a routine called across. `EXPORT name` in the owner
   with the declaration following it; `IMPORT (INTEGER : x)`, `IMPORT (BYTES : x)` and
   `IMPORT (ROUTINE VOID, VOID : x)` in the user.
4. **Then the rest**, one at a time, gating each.
5. Update the `:MODE` build files and `rt-load.ps1`'s `-Libraries` list.

## What this needs that is not here yet

**The lab.** Our node (D19999) is not running, so nothing can be pushed, compiled or gated - and
a split that cannot be compiled is a product left broken with no way to see it.

**Step 1 was safe without it and is done. Step 2 is NOT** - an earlier version of this page said
it was, and that was too generous. A rename is verifiable by counting, because "the old name
appears nowhere and the new one appears exactly as often" is a complete statement about the
change. Moving twenty-five declarations into another module and adding `EXPORT`/`IMPORT` blocks
is not: R119 ordering, a name imported twice, or a declaration left behind all compile or fail in
ways only the compiler will say. **Step 2 onward waits for the lab.**
