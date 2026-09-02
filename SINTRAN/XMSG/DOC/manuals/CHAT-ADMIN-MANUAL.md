# NDCHAT - Administrator's Manual

For the SINTRAN operator who installs the chat, starts it, connects machines together and keeps
it running. You do not need to know XMSG internals or how the program is built; the
[build and deploy manual](CHAT-BUILD-AND-DEPLOY-MANUAL.md) covers that.

**Checked against the running lab on 2026-09-02.** Screens marked *captured* were read off a
real terminal on D100 (FJELL) that day. Steps marked **UNVERIFIED** have not yet been run
exactly as written; they say so, and say what is known.

Related: [the user manual](CHAT-USER-MANUAL.md), [the specification](../CHAT-APP-SPECIFICATION.md),
[LAB.md](../../LAB.md) for the three lab machines.

---

## 1. What the pieces are

```
   a user at a terminal            a user on another machine
          |                                   |
      @CHAT (client)                      @CHAT
          |  XMSG, local                      |
      CHATSER (RT server) ==== trunk ==== CHATSER
          |
      @CHAT-MON  (your operator program)
```

| Piece | File on the machine | What it is |
|---|---|---|
| **CHATSER** | `CHATSV:BRF` + `CHATLIB:BRF`, RT-loaded onto a segment | the server. An RT program with no terminal. Owns the rooms, the seats and the trunks. One per machine |
| **CHAT** | `(SYSTEM)CHAT:PROG` | what a user runs. Talks only to its own machine's server |
| **CHAT-MON** | `(SYSTEM)CHAT-MON:PROG` | your program. Same server, a separate command port |
| **history** | `(RT)CHATH<n>:DATA`, one per room | the last lines of each room, kept on disk, shown to newcomers |
| **trunk** | not a file | a connection between two servers so a room spans machines |

Everything rides on **XMSG**, the SINTRAN message system, and between machines on **COSMOS**.
Both must be up before the chat is, which is why the chat is started at the end of the boot
mode file (section 5).

The lab:

| Machine | System number | Chat name | Trunks to |
|---|---|---|---|
| D100 | 100 | FJELL | 102, 103 |
| D102 | 102 | VIDDA | 100, 103 |
| D103 | 103 | SKOGEN | 100, 102 |

---

## 2. Your operator program: CHAT-MON

```
@CHAT-MON
CHAT-MON - type ? for commands
C-M: ?
CHAT-MON commands:
  STATUS                how it stands
  STOP                  stop the server
  ? or HELP             this
  EXIT                  leave
  START                 use @RT CHATSER
  RESTART               stops it only
  START-TRUNK <sys>     trunk to a peer
  STOP-TRUNK <sys>      forget that peer
  RESTART-TRUNK <sys>   stop then start it
  LIST-TRUNKS           peers and states
  INITIALIZE <room> <n> how much that room keeps
  SET-NAME <name>      what this machine is called
  LOCK-TRUNKS          learn no NEW machines
  DROP-MEMBER <name>   free a seat, tell NOBODY (test)
  LIST-MEMBER          who is seated, and which room
  UNLOCK-TRUNKS        learn any machine again

Shorten each part: LI-T is LIST-TRUNKS, LO-T is LOCK-TRUNKS.
Upper or lower case, either is fine.
A trunk is a BELIEF a peer is reachable, not a link.
C-M:
```
*(captured)*

Commands are matched the SINTRAN way - each dash-separated part may be shortened. `EXIT`
returns you to `@`.

**CHAT-MON needs the server running.** It sends its commands to the server by name over XMSG.
With no server up every command answers with a status number instead of a result.

### STATUS - the one line that tells you the truth

```
C-M: STATUS
SEATS 0/16  build S31-0854  default 200  disk 100KB  empty o45 bounce 4 relay 0 fwd 6 fwdbad 0 dupe 0 refuse 0 LOBBY 200/3
C-M:
```
*(captured; on an 80-column terminal the line wraps)*

| Field | What it tells you |
|---|---|
| `SEATS taken/offered` | how many people hold a seat on this server. **Read this, not a screen** - a user whose client died can still hold a seat for a minute until the server sweeps it |
| `build S31-0854` | the stamp the maintainer set in the source when it was built (letter, day, hour-minute). Two servers with the same stamp are not necessarily the same build - the stamp is bumped by hand, and was not for a while. After a reload, the counters resetting to small numbers is the sure sign |
| `default 200` | how many lines a new room keeps as history |
| `disk 100KB` | the size of the history files on disk |
| `empty` | routed letters that arrived with no payload. Climbing while nobody is chatting means a client is misbehaving |
| `bounce` | messages sent to a seat that had gone - dead clients |
| `relay` | lines this server passed on to a further machine. Climbs on a machine in the middle, stays 0 on an end machine |
| `fwd` / `fwdbad` | private messages forwarded to another machine / refused because that machine was not reachable |
| `dupe` | the same line arriving twice by two routes, recognised and dropped. Only ever non-zero when three machines form a triangle |
| `refuse` | joins turned away (16 seats full, or a name already taken) |
| `LOBBY 200/3` | one entry per room that has history: `NAME size/held` - keeps up to 200 lines, holds 3 now |

**`empty o45` is a count of 6345, not a code.** The server printed three digits at most, so a
counter past 999 wrote its "hundreds" (63) as a letter. An hour later the same line read
`empty r36` = 6636. Fixed in the source on 2026-09-02 (five digits); a server built before that
still prints the letter. Read a letter there as "thousands: letter minus `0`".

### LIST-TRUNKS

```
C-M: LIST-TRUNKS
102   VIDDA            up
103   SKOGEN           up
C-M:
```
*(captured on D100)*

Number first, then the name the peer calls itself, then `up` or `down`. You type the number in
every trunk command. A peer that has never said its name shows as `D102`.

### LIST-MEMBER

Who holds a seat on **this** machine and in which room, with the build stamp of the client they
are running:

```
C-M: LIST-MEMBER
OLAV             LOBBY            B31-0731
C-M:
```
*(captured on D100 while OLAV was seated locally and KARI on D102 - only local seats are
listed; use `/who` from a client for the cross-machine view)*

Use it when `SEATS` says somebody is there and no user admits to it.

### DROP-MEMBER <name>

Frees that seat **without telling anyone** - the room is not told the person left. It exists for
one situation: a name stuck in the seat table that the server's own sweep does not clear (a
client that died in a particular way). Every other stranded seat clears itself within a minute;
try `LIST-MEMBER` again before reaching for this.

---

## 3. Installing

### 3.1 From the install floppy

The install floppy `NDCHAT-INSTALL-<date>.img` (built by `tools\make-floppies.ps1`, see the
build manual) holds:

| File | Goes to | Purpose |
|---|---|---|
| `CHAT:PROG` | `(SYSTEM)` | the client, fully linked - VTM and the screen library travel inside it |
| `CHAT-MON:PROG` | `(SYSTEM)` | the operator program |
| `CHATSV:BRF` | `(SYSTEM)` | the server, to be RT-loaded |
| `CHATLIB:BRF` | `(SYSTEM)` | the message library the server is RT-loaded with |
| `CHATRT:MODE` | `(SYSTEM)` | the RT-load sequence, so you do not type it |
| `XMSG-STARTEX-L03:MODE` example | `(UTILITY)` | the boot block that starts the server, names the machine and starts the trunks |
| `README:TEXT` | anywhere | this chapter, short form |

The machine must already have, as ND products: `XMP-100-1-B02:BRF` (the COSMOS XMSG library,
product ND-10609), `MON-CALL-1B-A00:BRF` and `MON-CALL-NAMES-A:DATA`, and `PLANC-1BANK-F00:BRF`.
The server is RT-loaded against those three libraries. **They are not on the floppy** - they
are licensed ND software already on a machine that runs COSMOS and PLANC programs.

**UNVERIFIED - the floppy has not yet been mounted on a lab machine.** What is known: each lab
machine has a floppy controller (`device add FX 0` in its `RetroCore.ini`); RetroCore attaches an
image with `attach <device> <file>` or `mount floppy0 <n> <file>`; a note from July 2026 says a
floppy image attached **at boot** stopped SINTRAN from booting, so attach it after the machine is
up. The SINTRAN side, to be confirmed when the floppy is first used:

```
@ENTER-DIRECTORY NDCHAT,FLOPPY-DISC-1,,        the volume name ndtool gave the image
@COPY-FILE "CHAT:PROG",(NDCHAT:FLOPPY-USER)CHAT:PROG
@COPY-FILE "CHAT-MON:PROG",(NDCHAT:FLOPPY-USER)CHAT-MON:PROG
@COPY-FILE "CHATSV:BRF",(NDCHAT:FLOPPY-USER)CHATSV:BRF
@COPY-FILE "CHATLIB:BRF",(NDCHAT:FLOPPY-USER)CHATLIB:BRF
@COPY-FILE "CHATRT:MODE",(NDCHAT:FLOPPY-USER)CHATRT:MODE
@RELEASE-DIRECTORY NDCHAT
```

Remember: **`COPY-FILE` takes the DESTINATION first**, and quoting a name means "create it".
Quoting one that exists answers `FILE ALREADY EXISTS` - drop the quotes to overwrite.

### 3.2 From another machine that already has it

This is how the lab machines were installed and it is proved. With COSMOS up, a machine can copy
straight from a peer:

```
@COPY-FILE "CHAT:PROG",D100(SYSTEM).CHAT:PROG
@COPY-FILE "CHAT-MON:PROG",D100(SYSTEM).CHAT-MON:PROG
@COPY-FILE "CHATSV:BRF",D100(SYSTEM).CHATSV:BRF
@COPY-FILE "CHATLIB:BRF",D100(SYSTEM).CHATLIB:BRF
```

Then check each one landed whole - **compare the byte count** with the source machine:

```
@FILE-STATISTICS CHAT:PROG;1,,
...
           NN PAGES , NNNNN BYTES IN FILE
```

A transfer that stalls leaves a shorter file behind and reports nothing. The byte count is the
only check.

### 3.3 Loading the server

The server is an RT program and lives on its own segment. `tools\rt-load.ps1` types the whole
sequence from Windows; by hand it is:

```
@ABORT CHATSER                   if it is running - see the third trap below
@RT-LOADER
*RELEASE-SEGMENT 2528            frees the number if an old server sat on it
*NEW-SEGMENT 2528,2,,,          a FREE segment number, ring 2 - five parameters, keep the commas
*LOAD CHATSV,2528,               answer Y to "CHATSER REPLACING?" on a rebuild
*LOAD CHATLIB,2528,
*LOAD XMP-100-1-B02,2528,
*LOAD MON-CALL-1B-A00,2528,
*LOAD PLANC-1BANK-F00,2528,
*END-LOAD
*CHANGE-RT-DESCRIPTION           it prompts: RT-PROGRAM CHATSER, PRIORITY 75, SEGMENT 2528,
                                  leave START ADDRESS blank, RING 2, then two page-table prompts
*EXIT-LOADER
@RT CHATSER
```

Three traps, all measured:

- **`NEW-SEGMENT` allocates a FREE segment.** Reusing the number the running server sits on is
  refused (`PARAMETER NO. 1 IS ILLEGAL`), every LOAD after it fails, and `@RT CHATSER` then
  starts the OLD code. `RELEASE-SEGMENT` first, or a new number. `CHATRT:MODE` on the install
  floppy does the release.
- **`CHANGE-RT-DESCRIPTION` prompt 5 is START ADDRESS, not RING.** A ring number there starts
  the program inside its data and it dies with `STACK OVERFLOW`.
- **Stop the server before loading over it** (`CHAT-MON` `STOP`). A running program refuses the
  description change once, quietly, and everything after still says OK.

Check it is running:

```
@LI-RT-DES,CHATSER
```

A running server shows `IN TIME QUEUE`, `RTWT`, and an `ACTUAL` row matching `INITIAL`. Note the
single comma and nothing after the name; `CHATSER,,` answers `ILLEGAL PARAMETER`, which reads like
"no such program" and is not.

### 3.4 After loading: name and trunks

A freshly loaded server has no name and no peers. In `CHAT-MON`:

```
C-M: SET-NAME FJELL
C-M: START-TRUNK 102
C-M: START-TRUNK 103
```

`tools\rt-load.ps1 -Port <p> -ChatSetupOnly` does exactly this for a lab machine.

### 3.5 The client as a reentrant subsystem (optional)

On D102 the client is also dumped as a reentrant subsystem called `NDCHAT`. **A reentrant
subsystem shadows the :PROG file** - after copying a new `CHAT:PROG` there, the old dumped one
still runs until it is dumped again. `DUMP-PROGRAM-REENTRANT` prints nothing on success.

---

## 4. Running it day to day

### Starting and stopping

| | |
|---|---|
| start | `@RT CHATSER` at the `@` prompt (CHAT-MON's `START` only reminds you of this) |
| stop | `CHAT-MON` -> `STOP`. `RESTART` also only stops; start it again with `@RT CHATSER` |
| after `@ABORT CHATSER` | `@RT CHATSER` does **not** bring it back cleanly - RT-load it again (3.3) |

**An RT-load orphans every joined client.** A fresh segment is a fresh server with an empty seat
table. Every client that was joined shows a perfectly normal screen and receives nothing; the
only tell is that the user's own line never echoes. **Tell users to `/exit` and start `CHAT` again
after every load.**

**After every XMSG restart, RT-load the server again.** Starting it is not enough; a server that
outlives an XMSG restart sends every message one byte offset.

### Trunks

A trunk is the server's belief that a peer is reachable. It is started from either end; one end
is enough. If the peer goes away the trunk goes `down` and the server retries by itself, on a
doubling wait from 15 seconds up to 5 minutes, and a keepalive on a quiet trunk stops it flapping.

```
C-M: START-TRUNK 103
trunk added
```

**Never `START-TRUNK` a trunk that is already `up`.** It knocks the trunk down for about a minute.
Read `LIST-TRUNKS` first.

`LOCK-TRUNKS` stops the server accepting a trunk from a machine it has not been told about;
`UNLOCK-TRUNKS` allows it again.

### Machine names

`SET-NAME FJELL` is what this machine calls itself; the peers learn it on the trunk and show it
after users' names (`KARI@FJELL`). An empty `SET-NAME` clears it and the machine goes back to
`D100`. The name is **not saved to disk** - put it in the boot mode file (section 5). The
system NUMBER still decides where a message came from; the name is only a label.

### History

Each room keeps its last lines in `(RT)CHATH<n>:DATA` under user RT (the server's own user), and
replays them to a joiner. `INITIALIZE <room> <n>` sets how many lines that room keeps; `default
200` in `STATUS` is what a new room gets.

`STATUS` ends with `NAME size/held` per room. **`held` staying at 0 while people are talking
means the server cannot open the history file** - almost always because an earlier, aborted
server still holds it open. `@RTCLOSE-FILE` on the file frees it; so does a cold boot. Clients
then show an empty room on entry until it is fixed, and nothing else looks wrong.

Do not read a history file with `LIST-FILE` or QED - it is a binary ring, and a CONTINUOUS
file's unwritten blocks hold whatever was on the disk before.

---

## 5. Starting at boot

The boot mode file `(PACK-ONE:UTILITY)XMSG-STARTEX-L03:MODE` (one per machine, versioned in
`boot/` in the repository) brings up XMSG, defines the remote systems, starts the network server
and the links, and ends with the chat block:

```
@RT CHATSER
@CHAT-MON
SET-NAME FJELL
START-TRUNK 102
START-TRUNK 103
EXIT
```

A plain `@RT` is enough on a cold boot: the RT description and its segment survive a reboot and
XMSG is already up by then. The names and trunks are re-typed because the server does not
remember them.

**Before trusting a new boot block, prove that MODE input reaches CHAT-MON.** CHAT-MON reads its
commands from the terminal; if the file's input did not reach it, it would wait for ever and
**the boot would hang** on a machine you can then not log in to. `boot/TRUNKTST.MODE.txt` is
that block on its own - push it and run `@MODE TRUNKTST:MODE,,`; if `TRUNKTST REACHED THE END`
prints, the block is safe. Proved on all three lab machines by a cold boot on 2026-08-23.

---

## 6. When users complain

| Complaint | Look at | Usually |
|---|---|---|
| "my lines do not appear" | `STATUS` -> `SEATS`; `LIST-MEMBER` | the server was reloaded and their client is orphaned. They `/exit` and restart |
| "I cannot see people on the other machine" | `LIST-TRUNKS` on both ends | a trunk is `down`. Wait for the retry, or `START-TRUNK <n>` **only if it says down** |
| "the room is empty when I join, but there was a conversation" | `STATUS` -> `NAME size/held` | `held 0`: a history file is locked open by a dead server. `RTCLOSE-FILE`, or reboot |
| "AMBIGUOUS FILE NAME when I type CHAT" | `@LIST-FILES CHAT:PROG,,` | `CHAT:PROG` is missing and only `CHATSV:PROG` etc. match. Copy the client in (3.2) |
| "it says the room is full" | `STATUS` -> `SEATS 16/16`; `LIST-MEMBER` | 16 real people, or stranded seats. Stranded ones clear in a minute; `DROP-MEMBER` for one that does not |
| "everyone shows as `@D102`, not `@VIDDA`" | `LIST-TRUNKS` | that machine has no `SET-NAME` in its boot file |
| CHAT-MON answers with a status number, never a result | `@LI-RT-DES,CHATSER` | the server is not running. `@RT CHATSER`, or RT-load if that does nothing |
| the whole chat dies on every machine at once | `@X-C` -> `LIST-LINK` | XMSG or COSMOS went down. Bring it back, then **RT-load the server again** on each machine |

**Two things that look like chat faults and are not:** SINTRAN's idle timeout ends a user's
program and the screen still looks normal (`--EXIT--` or `ABORTED BY SYSTEM` at the bottom); and
a terminal that reconnected mid-program hangs that terminal line, not the chat.

---

## 7. Limits

- 16 seats per server. A burst of twenty simultaneous joins has taken XMSG down; there is no
  queue.
- One room at a time per user; private conversations open in windows beside it.
- A private message crosses at most one trunk. To someone two machines away it is refused in
  words.
- Nothing is stored for absent users. History is per room and bounded (`INITIALIZE`).
