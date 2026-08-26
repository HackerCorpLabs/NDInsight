# Boot files for D100, D102 and D103

What each machine runs at power-on, kept here so it is version controlled instead of living
only inside a disk image.

## How a machine starts

RetroCore's initial commands, identical on all three:

```
ENTER-DIRECTORY PACK-ONE DISC-75MB-1 0
SET-ERROR-DEVICE 1
BATCH
APPEND-BATCH 1 (SYSTEM)LOAD-MODE:BATC SYSTEM-OUTPUT-1
```

So everything else hangs off `(SYSTEM)LOAD-MODE:BATC`, which is now the SAME on every machine
and calls `(PACK-ONE:UTILITY)XMSG-STARTEX-L03:MODE`. That second file is the one that DIFFERS
per machine - same name everywhere, different contents.

| Source | Goes to | Machine role |
| --- | --- | --- |
| `LOAD-MODE.BATC.txt` | `SYSTEM/LOAD-MODE:BATC` on all three | identical everywhere |
| `XMSG-STARTEX-L03.D100.txt` | `UTILITY/XMSG-STARTEX-L03:MODE` on D100 | Ethernet, and the HDLC gateway for D103 |
| `XMSG-STARTEX-L03.D102.txt` | same path on D102 | Ethernet only, D103 routed via D100 |
| `XMSG-STARTEX-L03.D103.txt` | same path on D103 | HDLC only, everything routed via D100 |
| `TRUNKTST.MODE.txt` | anywhere, as a one-off | **A TEST, not part of the boot.** Proves a MODE file can drive `CHAT-MON` without hanging |

### `TRUNKTST.MODE.txt` - run this BEFORE trusting a new boot block

The trunk commands at the end of each `XMSG-STARTEX-L03` are typed into `CHAT-MON`, which is an
ordinary program that reads its input from the terminal. **If MODE-file input does not reach it, it
sits waiting on MON1 and THE BOOT HANGS** - far worse than the trunk simply not starting, and you
find out at a reboot, on a machine you now cannot log into.

This file is that block on its own. Push it and run `@MODE TRUNKTST:MODE,,`. If
`TRUNKTST REACHED THE END` prints, the file's input reached `CHAT-MON` and the terminal came back,
which is the whole test. It costs one file and one command; finding out the other way costs the
machine.

## The wiring these files assume

```
D100  <-- Ethernet hub on TCP 5010 -->  D102        both run ENNS0
D100  <-- HDLC, D100 listens 10362  -->  D103        D103 dials it, link 1360/1362
D19999 and D200 are on the Ethernet, so D103 reaches them through D100.
```

`F:\RC\RonnyTest\HDLC3\RetroCore.ini` used to dial `localhost:10366`, where nothing listens.
It now dials `10362`, which is D100's `HDLC 2`.

## Installing them

**Stop all three emulators first.** An emulator holds its image open and writing underneath it
corrupts the filesystem.

```powershell
python install-boot-files.py
```

It encodes each source the way the machines' own files are encoded - **even parity in bit 7 of
every byte, lines ending CR LF** - and writes them with `ndtool --put --overwrite`. The encoder
is checked by round-tripping a real boot file off D100: decode it, re-encode it, and the bytes
come back identical.

**`--overwrite` is not optional.** Without it ndtool prints `skipped (exists)` and still exits 0,
so a run that wrote nothing looks exactly like a run that worked. The script treats a skip as a
failure for that reason.

## The chat server comes up too

D100's and D102's files end with `@RT CHATSER`. **A plain `@RT` is enough on a cold boot** - the RT
description and its segment survive a reboot, and XMSG has already been started earlier in the same
file, so the server comes up against a live kernel. The "RT-LOAD it again" rule is about XMSG being
restarted *underneath* a server that is already running, which cannot happen here.

COSMOS starts after this file and does not touch XMSG - it restarts XFTRAD, spooling and the file
server - so the chat server is safe where it is.

D103's file has no such line. Whether D103 even has a CHATSER is **not established**.

Checking it: **`@LI-RT-DES,CHATSER`** - a comma, and nothing after the name. `NAME,,` can answer
`ILLEGAL PARAMETER`, which is a complaint about the command and reads exactly like the program not
existing. A running server shows `IN TIME QUEUE`, `RTWT`, and an `ACTUAL` row matching `INITIAL`.

## Proved by a cold boot, 2026-08-23 - nothing typed at any machine

```
D100  LI-RT-DES,CHATSER     IN TIME QUEUE / RTWT / ACTUAL 2575B     the server started itself
D100  LIST-NETWORK-SERVERS  ENNS0 9800 1 LAN
D100  LIST-LINK             1 Run 9800 (Ethernet)   2 Run 103 lun 1362 (HDLC)
D100  LIST-NAMES            *CHAT 16 free, CHAT-ADMIN, CHAT-TRUNK
D100  LIST-NAMES 102        D102's table, over Ethernet - *CHAT 16 free there too
D100  LIST-NAMES 103        D103's table, over HDLC
D102  LIST-NAMES 103        D103's table, THROUGH D100
D102  LIST-ROUT             103  A: *->LAN->100->103
```

## Known, and not caused by this

D103's image reports `4 blocks referenced by multiple files (cross-linked)` under `ndtool --fsck`.
That is present in the untouched backup taken before any of this was written, so it pre-dates
these files.

## Installed, and the boot block is proved safe (2026-08-24)

`XMSG-STARTEX-L03:MODE` now ends with a `START-TRUNK` block on D100 and D102, and both machines
hold a copy **hash-identical to the files here** - D100 4278 bytes, D102 4162. They were pushed
over the live XMSG link with the machines running; nothing was stopped.

**The dangerous question was answered before the change goes anywhere near a reboot.** CHAT-MON
reads its command line with MON1 on its own terminal, and nothing had ever driven it from a
file. If MODE input did not reach that call the program would wait for ever and the boot would
hang. Tested with exactly that block in a throwaway mode file on D100:

```
@MODE TRUNKTST:MODE,,
@CHAT-MON
C-M: START-TRUNK 102
trunk added
C-M: LIST-TRUNKS
102 down
C-M: EXIT
@CC TRUNKTST REACHED THE END
@
```

It works. The lines reached CHAT-MON, EXIT gave the terminal back, and the mode file ran to its
end.

**Two things that came out of the test:**

 - `START-TRUNK` against a trunk that is ALREADY UP answers `trunk added` and knocks it DOWN.
   It healed itself in under a minute. Harmless at boot, where nothing is up yet - but do not
   run it on a live machine to "check" something.
 - **The `@RT CHATSER` race is still NOT measured.** That command returns when the RT program is
   started, not when it has claimed its admin port, and CHAT-MON talks to that port. The test
   above ran against a server that had been up for hours. `LIST-TRUNKS` follows the
   `START-TRUNK` in the boot file so the boot log records what actually happened either way.

**Still unproved: that the trunk comes up at boot.** That needs a reboot.

## INSTALLED ON THE LIVE MACHINES, 2026-08-26

D100 5697, D102 5581, D103 4780 bytes - each matching its staged file exactly.

**NOT with `install-boot-files.py`.** That script writes into the RetroCore disk IMAGES and says in
its own header that the machines must be stopped first, because an emulator holds the image open
and a write underneath it corrupts the filesystem. These went over the LIVE link instead, which
needs nothing stopped: the staged files are already byte-exact SINTRAN text (even parity, CRLF), so
FA carries them unchanged.

### TWO THINGS THAT COST TIME, both worth knowing before doing this again

**THE BOOT FILE IS OWNED BY UTILITY, NOT SYSTEM.** `LOAD-MODE:BATC` calls

```
@MODE (PACK-ONE:UTILITY)XMSG-STARTEX-L03:MODE,,
```

so `LIST-FILES` as SYSTEM does not show it and `COPY-FILE` as SYSTEM answers **NOT WRITE ACCESS** -
the file is `PUBLIC ACCESS : READ`, `FRIEND ACCESS : READ`, and only its OWNER may write it. Log in
as **UTILITY** (blank password, same as SYSTEM) and copy from there.

There IS a `XSTART:MODE` under SYSTEM, 4278 bytes, the same size as the old boot file. **It is not
the file the boot reads.** Editing it would have looked like a successful install and changed
nothing.

**A ROLLBACK COPY FIRST.** `STRTBAK:MODE` beside each one holds the previous content, so putting a
machine back is one `COPY-FILE XMSG-STARTEX-L03:MODE,STRTBAK:MODE` as UTILITY.

### What they now do that they did not

```
@CHAT-MON
SET-NAME FJELL          (VIDDA on D102, SKOGEN on D103)
START-TRUNK 102
START-TRUNK 103
LIST-TRUNKS
EXIT
```

SET-NAME before the trunks, because the name travels on the trunk Hello. Three trunks rather than
two, because a machine only learns names from its DIRECT peers and because dedup cannot be
exercised on a chain. D100 had also never had `START-TRUNK 103` at all.

**NOT PROVED FROM A COLD BOOT.** The files are in place and their contents are verified byte for
byte, but no machine has been restarted since. That is the one test that matters and it has not
been run.
