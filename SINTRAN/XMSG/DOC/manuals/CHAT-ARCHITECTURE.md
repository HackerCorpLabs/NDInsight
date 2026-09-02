# NDCHAT - Architecture

How the chat is built: the server, the clients, the trunks between machines, the limits that
are designed in, and what it costs in time and space. Every number here is read from the source
(`SINTRAN-CHAT/*.PLNC`, the constants are named) or was measured on the lab machines, and says
which. Nothing is from memory.

**As of 2026-09-02**, server build `S02-1340`, client `B02-1340`. The companion pages:
[the specification](../CHAT-APP-SPECIFICATION.md) (what the server sends and to whom),
[the wire registry](../protocols/chat-wire.md) (every message kind, generated from
`chat-wire.json`), [CHAT-FEDERATION-DESIGN.md](../CHAT-FEDERATION-DESIGN.md) (why the trunks are
shaped as they are), [CHAT-HISTORY-DESIGN.md](../CHAT-HISTORY-DESIGN.md), and the three manuals
beside this one.

---

## 1. The shape

```
 machine A (FJELL, 100)                       machine B (VIDDA, 102)
 +---------------------------------+          +---------------------------------+
 |  @CHAT   @CHAT   @CHAT          |          |  @CHAT   @CHAT                  |
 |    |       |       |  XMSG port |          |    |       |                    |
 |    +-------+-------+  to port   |          |    +-------+                    |
 |            |                    |          |            |                    |
 |  CHATSER (RT program, 1 bank)   |  trunk   |  CHATSER                        |
 |    *CHAT       connection port  |<========>|    *CHAT                        |
 |    CHAT-TRUNK  named port       | CHAT-    |    CHAT-TRUNK                   |
 |    CHAT-ADMIN  named port       | TRUNK    |    CHAT-ADMIN                   |
 |            |                    |          |            |                    |
 |  @CHAT-MON (operator)           |          |  @CHAT-MON                      |
 |  (RT)CHATH<n>:DATA history      |          |  (RT)CHATH<n>:DATA              |
 +---------------------------------+          +---------------------------------+
            |                                             |
            +---------------- trunk to SKOGEN (103) ------+
```

Three programs, one per role, all PLANC, all talking XMSG:

| Program | Runs as | Talks to | Source |
|---|---|---|---|
| **CHATSER** | an RT program on its own segment, no terminal, one bank (64 KW for code, data and stack) | its clients, its operator, its peer servers | `CHATSV.PLNC` (7,800 lines) + `CHATLIB.PLNC` |
| **CHAT** | an ordinary program a user runs; also dumped as reentrant `NDCHAT` on D102 | its own machine's server only | `CHAT.PLNC` (6,400 lines) + `CHATARR.PLNC` (1,070) + `CHATLIB.PLNC` |
| **CHAT-MON** | an ordinary program the operator runs | its own machine's server, on the admin port | `CHATMON.PLNC` |

`CHATLIB.PLNC` is the message codec both sides IMPORT: the kind numbers, `cmEnc`/`cmDec` and
the field readers. It is compiled once and linked into all three, which is what keeps the two
ends of the wire agreeing - there is no second copy of the format to drift.

**A client never talks to another client, and never to another machine's server.** Everything
between machines goes server to server over a trunk. That is the whole reason the server exists
as a separate program: it is the one place that knows who is where.

---

## 2. The server

### 2.1 Three ports, and why three

XROUT, the XMSG name service, counts free connections against a **connection port**. Every seat
spent there is one a human cannot have, and one leaked never comes back while the server lives.
So the server opens three names and only one of them costs seats:

| Name | Kind | Reached by | Costs a seat |
|---|---|---|---|
| `*CHAT` | connection port | local clients - the Join is the only message addressed by name | **yes**, one per client |
| `CHAT-TRUNK` | ordinary named port | peer servers | no |
| `CHAT-ADMIN` | ordinary named port | CHAT-MON | no |

`*CHAT` offers **16 seats** (`maxSeats := 16`, measured: 16 offered, 16 taken, the 17th join
refused with `XMXEMFL`). After the Join the client and server talk **port to port**, by the
address read off the arriving letter; XROUT is out of the picture and nothing else costs a seat.

### 2.2 The tables

The server has no heap and no allocation. Everything is a fixed array sized by a named constant:

| Table | Size | Constant | What it is |
|---|---|---|---|
| members | 16 | `maxSeats` | name (16 chars, `maxNameLen`), room, the client's magic number, its build stamp (8 chars) |
| topic / room table | 8 | `maxTopics` | one row per room that has a topic or a history; a room with neither has no row. Topic 64 chars (`maxTopicLen`) |
| history rings | 8 | `maxHistRooms` | shares the topic table's index - the same row IS the room |
| peers | 8 | `maxPeers` | one row per machine we trunk to: system number, name, state, timers |
| remote members | 32 | `maxRemote` | people on other machines, as last reported by their server |
| seen relay ids | 64 | `maxSeen` | `(origin, line id)` pairs, for dropping a line that arrives twice |

**Rooms are derived, not stored.** A room is the set of seats whose room field says so; it
exists while somebody is in it and gets a table row only when it acquires a topic or a history.
`/list` walks the seats.

### 2.3 The loop

The server is one loop with no threads. Each turn it drains the lobby port, then the admin
port, then the trunk port (`maxDrain := 32` letters a turn, so one busy port cannot starve the
others), runs the periodic work (section 2.5), and sleeps on an XMSG wake-up armed on all three
ports. The wake-up is the "doorbell" - the sleep ends when a letter lands, not on a timer - with
a timer as the fallback so the periodic work still runs on a quiet machine. A run of twenty
consecutive empty wakes disarms the doorbell (`maxBadRuns := 20`), the guard against a stuck
flag spinning the server.

### 2.4 What it does with a letter

The specification's section 3 is the authority; the short form:

| Arrives | Server does |
|---|---|
| Join (by name, via XROUT) | takes a seat if the name is free in that room; answers Welcome (carrying the machine's name) or Reject; tells the room Joined; replays the room's history to the joiner; pushes the new member list to every peer that is up (`tellMbrs`) |
| Say | stamps the time, writes it to the room's history ring, broadcasts Said to **everyone in the room including the speaker**, and forwards it to every peer as `TrunkSaid` |
| Rename / Topic / Leave / Who / Rooms / Map / Direct | as the spec says; answers go to the asker, changes go to the room |
| a letter on the trunk port | section 3 |
| a letter on the admin port | CHAT-MON's command; answered with readable text |

**The echo rule** decides how a client is written: your own line comes back from the server with
the server's time on it, and the client prints that, never its own copy. That is what gives
everybody the same order.

### 2.5 Periodic work

Every turn, cheaply: the **sweep** looks at each seat's port through `XFSEC` and frees a seat
whose client has died, announcing it as Left; this is the only way a dead client is noticed
(`bounce` in STATUS counts messages that hit a dead seat first). The **peer tick** counts each
peer's silence: at 20 ticks (`peerPingAt`) it sends a keepalive hello, at 40 another, at 60
(`peerDeadAfter`) it declares the trunk down, announces that machine's members as Left, and
starts retrying on a doubling wait from 15 s to 300 s (`peerFirstWait`, `peerMaxWait`).

### 2.6 History

One file per room, `(RT)CHATH<n>:DATA` under the server's own user, **one 512-byte block per
message** (`histWords := 256`), written with a single `MN120` - no read-modify-write. The ring is
`block = id MOD histSize`; a room keeps up to **200** lines (`histDefault`, `maxHistSize`), set per
room with `INITIALIZE`. A block carries a stamp word (`histStamp := 27127`) so a block written by
an older build without a time is still read correctly.

Replay to a joiner is one `History` letter per remembered line, oldest first, after the
Welcome. **The ring's write pointer lives in memory only** - after an RT-load the room starts
again from block 0, and old blocks are overwritten as it fills. Reading them back to recover the
pointer was tried and reverted: a CONTINUOUS file's unwritten blocks hold whatever was on the
disk before, and a scan counted garbage as messages.

### 2.7 The operator port

CHAT-MON is a thin program: it sends one admin kind per command to `CHAT-ADMIN` by name and
prints the text that comes back. `STATUS` is one line built by the server (`buildStatus`) from
its counters; nothing is computed on the operator's side. Section 2 of the admin manual reads
that line field by field.

---

## 3. The trunks

### 3.1 What a trunk is

A row in the peer table, and a belief. `START-TRUNK 102` adds the row and sends a `TrunkHello`
to `CHAT-TRUNK` on system 102 by name; the peer adds its own row for us (unless it has
`LOCK-TRUNKS` set) and answers. From then on the two servers exchange:

| Kind | Carries | When |
|---|---|---|
| `TrunkHello` (48) | a direction byte and, since 2026-08-25, the machine's name | on start, on every retry, and as the keepalive |
| `TrunkWho` (49) / `TrunkMembers` (50) | nothing / `NAME/ROOM NAME/ROOM ...` | `TrunkWho` is asked on every hello; `TrunkMembers` is the answer, and is ALSO pushed unasked to every peer that is up whenever a local member joins, leaves or renames (`tellMbrs`), because a list that only moved on hellos left a peer advertising a stale name for as long as it was watched. The receiver replaces that peer's rows wholesale, so a push is idempotent and self-healing |
| `TrunkSaid` (51) | a line one of my members said | on every Say |
| `TrunkRelayId` (53) | a line being passed on: origin system, hops left, and the line id the origin stamped | when a line arrives from a peer and I have other peers |
| `TrunkDirect` (54) / `TrunkDirectBad` (55) | a private message across one hop, and its refusal | on `/tell D102!NAME` |

**One side is enough.** A trunk started at either end brings both rows up. Both lab boot files
start both anyway.

### 3.2 Flooding, relay, and exactly-once

A room line is **flooded**: every server forwards it to every peer except the one it came from,
with `hopsLeft` starting at `maxHops - 1` (`maxHops := 3` in CHATLIB) and decremented per hop.
On the D102 - D100 - D103 chain that reaches everybody; in the triangle (all three trunks up)
the same line arrives twice by two routes.

Two things keep that from looping or doubling:

- **the hop count** stops a line travelling for ever;
- **the seen table** stops it being shown twice: 64 `(origin system, origin line id)` pairs, and
  a repeat is dropped and counted as `dupe`. Proved on 2026-08-26 the only way it can be - on a
  triangle, one line typed, `dupe 0 -> 1` on the machine that got it both ways, and one copy on
  the screen.

**The speaker is qualified by the origin, not by the neighbour** - `SYSTEM@SKOGEN` on VIDDA
even though the letter came from FJELL - because the origin system number rides in the relay
header. A name is a label looked up from that number (learned on the hello); the number comes
from the sender's magic and cannot be forged.

### 3.3 What crosses a trunk and what does not

| Crosses | Does not cross |
|---|---|
| room lines (flooded) | a room's **history** - each machine keeps its own ring for a room of the same name |
| joins and leaves, as a member-table diff announced locally as Joined/Left | a **topic** (observed 2026-09-02; whether it should is undecided) |
| `/who` (from the remote member tables) | `/list` counts - local seats only |
| a **direct message, one hop only** - to a machine two hops away it is refused in words, on purpose: flooding a private message hands it to machines with no business seeing it | |

### 3.4 Presence across machines

A peer's member table used to be replaced wholesale and in silence. Since 2026-09-01 the server
snapshots the old table, takes the new one, and announces the difference as ordinary Joined /
Left notices, so screens on this machine show remote arrivals and departures and `n here`
follows them. A rename on the other side therefore shows as new-name-joined, old-name-left. When
a peer goes down its members are announced as Left the same way.

---

## 4. The clients

### 4.1 The full-screen client

`CHAT.PLNC` is two halves: the **send** half (typing, commands, the main loop) and the
**arrival** half (`CHATARR.PLNC`: what to do with each kind that comes in), split so editing
one does not recompile the other.

It draws through VTM (`PLANC-SCREEN-H`, `VTMR`, `VTMDATA`, `VTMARR`, all linked into the
:PROG so it travels as one file). The rules it follows, all measured on the lab and written up
in `Developer/Languages/Application/PLANC-INTERACTIVE-SCREEN-PATTERNS.md`:

- **poll the keyboard with `MON66` (ISIZE)** - the terminal cannot be put in no-wait mode
  (MON1 still blocks, unexplained), so the loop asks whether a byte is waiting and sleeps
  otherwise: 1 unit while lines are arriving, 10 units after 25 idle turns (`sleepShort`,
  `sleepLong`, `idleRunsMax`);
- **a poller flushes VTM itself** - `VTWBUF` is the flush, and a write that is not flushed
  looks exactly like a lost message;
- **paint the rows that differ**, never the whole screen: every line is stored padded to
  full width, each row remembers which line number it shows, and a repaint compares line
  numbers (never ring slots, which are reused);
- **a panel keeps its rectangle at module level** so the room can be clipped behind it rather
  than frozen. `/help`, `/who`, `/list`, `/map` and an asked-for `/topic` open one; any key
  closes it and does nothing else.

### 4.2 Windows

Window 1 is the room; windows 2 to 4 (`maxWins=4`) are one-to-one conversations, one per
person, opened by the first Direct in either direction. They are **alternative contents for one
rectangle**, not overlapping windows - only one is painted, so there is no stack and no
clipping between them. All four share one ring: `cache(0:23099)`, 300 lines (`cacheLines`) of
77 bytes, 75 per window (`winLines`). A room line always lands in window 1 whatever is being
looked at; the bar on the bottom row shows each window's unread count.

### 4.3 The line client

The same program on a printing terminal (type 2, or no VTM) is line by line: the same commands,
answers printed instead of panelled, private messages marked `*(NAME)`. No windows.

### 4.4 State the client keeps

Almost none. Its nickname (`CHAT:CNFG` in the user's own area, written on every successful
`/nick`), the server's address (from the Welcome), the machine name (from the Welcome's text),
the room, and the ring. It does not know the system number - the server tells it its machine
name because a client cannot work that out for itself.

---

## 5. Limits

Designed in, and where each one comes from:

| Limit | Value | Where | Why |
|---|---|---|---|
| people per machine | 16 | `maxSeats` / `*CHAT` seats | XROUT connection ports; a burst of 20 joins has taken XMSG down |
| nickname, room name | 16 chars | `maxNameLen` | fits the 13-column speaker field on screen only up to 13 |
| topic | 64 chars | `maxTopicLen` | |
| typed line | 80 | `maxTyped` | one terminal line; wraps in the room display |
| rooms with topic or history per machine | 8 | `maxTopics` / `maxHistRooms` | one table row each; more rooms than that exist but keep nothing |
| history per room | 200 lines, 512 bytes each = 100 KB on disk | `maxHistSize` | `STATUS` shows `disk nKB` |
| peers (trunks) per machine | 8 | `maxPeers` | |
| remote members remembered | 32 | `maxRemote` | across all peers |
| relay hops | 3 | `maxHops` | enough for any three-machine shape |
| duplicate memory | 64 lines | `maxSeen` | a repeat arrives within a hop or two |
| windows per client | 4 (room + 3 conversations) | `maxWins` | 23 KB ring in a 64 KW bank |
| lines kept per window | 75 | `winLines` | |
| direct message reach | one trunk hop | by design | privacy: no flooding of private text |
| a server's whole memory | one bank, 64 KW | RT loader | code, data and stack together; `CHATSV:BRF` is 35 KB |
| a client's program file | 108 KB, 53 pages | `CHAT:PROG` | VTM inside |

And the ones that are consequences rather than choices:

- **an RT-load orphans every client** - a fresh segment is a fresh server with an empty seat
  table, and a client with no seat shows a normal screen and receives nothing;
- **an XMSG restart needs an RT-load** - a server that outlives it sends one byte offset;
- **history does not survive a reload** in position (2.6), and nothing is stored for a user who
  is away - there is deliberately no mailbox; a direct message to nobody is refused, not queued;
- **no line editing** beyond backspace, and no scrolling into the past on screen;
- **names are per machine** - two RONNYs on two machines are legal, and a direct message to a
  bare `RONNY` that matches both is refused with both full names rather than guessed at.

---

## 6. Performance

All measured on the RetroCore lab machines (ND-100 emulation), which is the only place this
runs; wall-clock numbers on real hardware would differ.

### 6.1 Latency of a said line

| Measurement | Result | When |
|---|---|---|
| a bare client command answered locally (`/list`), floor | 112 ms | 2026-08 |
| a said line back on the speaker's own screen, floor | 514 ms before idle-sleep tuning, **483 ms** after | 2026-08 |
| same, mean over a run | 683 ms before, **592 ms** after | 2026-08 |
| what the remaining time is | the XMSG round trip itself, roughly 400 ms of the 480 | by subtraction |

The target was 500 ms and it is missed by about 10%. Sleep tuning has done what it can; the
lever left is to put fewer letters on the wire per message (`CHAT-PLAN.md`, latency section).

### 6.2 Across a trunk

Not measured as a number. What is measured: a line typed on D103 appears on D102 - two hops,
through D100 - within the same one-second read of the screen used for the local case, and
history replay of three lines to a joiner is not visibly slower than the join itself.

### 6.3 Throughput and load

- 16 seats, 4 users at once measured, load test of 2026-08-19: every seat accounted for by
  `SEATS` and none leaked afterwards.
- **20 simultaneous joins killed XMSG** (the kernel, not the server); there is no queue and
  the fix is not to do that.
- A pull of the 467 KB server listing through the same XMSG kernel parked the send window
  (`*** PARKED *** send window is full; 14 datagram(s) now waiting`) - the kernel's window is
  small, and a chat that keeps its letters short lives comfortably inside it.

### 6.4 Sizes and times of the build

| | |
|---|---|
| compile `CHAT.PLNC` + `CHATARR.PLNC` on D100 | about 5 minutes (7,082 + 1,069 listing lines) |
| compile `CHATSV.PLNC` | about 5 minutes (8,439 listing lines) |
| link, either | seconds |
| push a 300 KB source through the sync daemon | about 1 minute |
| pull a 450 KB listing | 1 to 2 minutes |
| RT-load with name and trunks restored | about 1 minute (`rt-load.ps1`) |
| offline tests | `CHATCTST` 23 checks, `CHATKT` 49, `CHATVT` 48 - seconds each |

### 6.5 Memory

The server fits its one bank with room: `CHATSV:BRF` 35,303 bytes of object plus `CHATLIB`
2,220 and the three ND libraries, all under 64 KW. The client's biggest single object is the
23,100-byte line ring; the whole `CHAT:PROG` is 53 pages. Neither allocates at run time.

---

## 7. What is proved, and what is not

Proved on the machines: everything in sections 2 to 4 that carries a constant name or a date.
Not proved, and said so where it matters: topics across a trunk (3.3), a direct message across
a trunk in both directions (one direction on 2026-09-02, FJELL to VIDDA and back), the
install floppy mounting on a machine (admin manual 3.1), and the doorbell's disarm flag
clearing after a long idle (`CHAT-PLAN.md`). The open questions in the wire registry (C1 to
C4) are answered by this page's section 3: a trunk carries all rooms; a qualified name is
origin number plus a learned label; origin and hop count live in the relay header; a vanished
machine's members are freed by the peer tick at 60 ticks of silence.
