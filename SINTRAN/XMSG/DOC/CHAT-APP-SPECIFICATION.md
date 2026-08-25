# NDCHAT - what the product actually does

**Status: current as of 2026-08-25**, derived from `SINTRAN-CHAT/CHATSV.PLNC` and
`SINTRAN-CHAT/CHAT.PLNC` and checked against the three live machines. Where something is designed
but not built it says so; where something has never been proved it says that too.

**This describes the SINTRAN product** - the PLANC server `CHATSER` and the PLANC client `CHAT`
running on real ND-100s. It is NOT about the C# chat library, which is a separate thing with its
own terminal door; see `CHAT-USER-GUIDE.md` for that and mind its warnings.

**Related:** [CHAT-FEDERATION-DESIGN.md](CHAT-FEDERATION-DESIGN.md) for the trunk protocol and how
it was built, [CHAT-UI-DESIGN.md](CHAT-UI-DESIGN.md) for the full-screen client,
`protocols/chat-wire.json` for the bytes on the wire.

---

## 1. The shape of it

```
   a user at a terminal            a user on another machine
          |                                   |
      @CHAT (client)                      @CHAT
          |  XMSG, local                      |
      CHATSER (RT server) ==== trunk ==== CHATSER
          |
      @CHAT-MON  (operator front end, XMSG on a separate command port)
```

- **`CHATSER`** is an RT program. It owns the rooms, the seats and the trunks. It has no terminal.
- **`CHAT`** is what a user runs. It never talks to another user - only to its own local server.
- **`CHAT-MON`** is the operator's program. Same server, different port.
- **Trunks** join servers so a room spans machines. Today: D100-D102 and D100-D103, with D100
  relaying between them.

**Sixteen seats per server** (`memberName(1:16, 1:16)`), **sixteen characters per nickname**,
sixteen per room name.

---

## 2. User commands

Typed at the client. Matched on a prefix by `cmdIs`, so `/j` works for `/join`.

| Command | Effect |
|---|---|
| `/help` | list the commands |
| `/join <room>` | enter a room, leaving the current one |
| `/nick <name>` | change your name; everyone in the room is told |
| `/who` | who is in this room, including people on other machines |
| `/list` | what rooms exist |
| `/map` | how the machines are wired |
| `/topic <text>` | set the room's topic; everyone is told |
| `/tell <name> <text>` | a message to ONE person - `RONNY`, or `D102!RONNY` for another machine. Section 6. |
| `/leave` | leave the room |
| `/quit` | leave and exit the client |

Anything not starting with `/` is said to the room.

---

## 3. WHAT THE SERVER SENDS, AND TO WHOM

**This is the section a client author needs and it is the one that was never written down.**

`broadcast(length, roomOfSlot, skipSlot)` sends to everyone in a room, where **`skipSlot` is the
one seat left out and `0` leaves nobody out.**

| Event | Kind | Who receives it |
|---|---|---|
| somebody speaks | `kSaid` (5) | **everyone in the room INCLUDING THE SPEAKER** |
| somebody joins | `kJoined` (7) | everyone **except** the joiner |
| the joiner themselves | `kWelcome` (2) | just them |
| a join is refused | `kReject` (3) | just them |
| somebody leaves | `kLeft` (8) | everyone **except** the leaver |
| somebody renames | `kRenamed` (10) | **everyone, including the asker** |
| the topic changes | `kTopic` (14) | **everyone, including the asker** |
| `/who`, `/list`, `/map` | `kWho`/`kRooms`/`kMap` | just the asker |

### THE ECHO RULE, AND IT DECIDES HOW A CLIENT IS WRITTEN

```planc
buildFromSlot(kSaid, slot, textAt, textLength) =: length
broadcast(length, slot, 0)            % their room, speaker too
```

**When you speak, the server sends the line back to you.** A client must therefore **NOT** print
your own line when you press RETURN - it must wait for the `kSaid` to arrive and print that.

Doing it locally as well shows **every message you send twice**. It also loses the server's
ordering and its timestamp, so your own lines would sit in the wrong place relative to everyone
else's.

**The full-screen prototype `CHATUI.PLNC` currently gets this wrong** - it prints locally on
RETURN, because it has no server behind it yet. That must change when it is wired up, and it is
the single most important thing to remember from this page.

### A DEAD CLIENT IS ANNOUNCED TOO

`kLeft` has two sources: the explicit `/leave`, and the server noticing a **dead port** during its
own sweep and announcing the departure itself. So a user whose client died, or whose terminal was
stopped, is reported gone rather than left sitting in the room for ever.

---

## 4. Across machines

A line typed on one machine reaches the others as `kTrkSaid` (51) on a direct trunk, or
`kTrkRelId` (53) when it has been passed on by a machine in the middle.

- **The speaker is qualified by the ORIGIN**, not by the machine the letter arrived from -
  `SYSTEM@D103` even when it came via D100.
- **`/who` crosses trunks**, so the member list includes people on other machines.
- **One side is enough** - a trunk started at either end works.
- **A trunk that ages out re-joins by itself**, on a doubling wait from 15 to 300 seconds.

**MEASURED 2026-08-25:** a line typed on D103 appeared on D102, which has no trunk to D103 at all.

---

## 5. Operator commands - `@CHAT-MON`

| Command | Effect |
|---|---|
| `STATUS` | seats, counters, and the trunk state |
| `LIST-TRUNKS` | each peer and whether it is up |
| `START-TRUNK <n>` / `STOP-TRUNK <n>` / `RESTART-TRUNK <n>` | trunk control |
| `INITIALIZE` | reset server state |
| `STOP` / `RESTART` | stop or restart the server |
| `SET-NAME <name>` | what this machine calls itself - section 5a. Empty clears it |
| `HELP` | list the commands |

`STATUS` reads, for example:

```
SEATS 0/16  default 200  disk 0KB  empty 13 bounce 1 relay 0 dupe 0 refuse 0
```

| Field | What it tells you |
|---|---|
| `SEATS taken/offered` | the truth about seat leaks - a screen cannot tell you this |
| `empty` | routed letters with no payload; climbing while quiet is the seat feedback loop |
| `bounce` | messages home with nobody there - dead clients |
| `relay` | lines passed ON to a further peer. **Climbing on a middle machine, flat zero on an end one** |
| `dupe` | repeats recognised and dropped. **0 on a CHAIN whatever the code does** - it needs two paths to say anything |
| `refuse` | joins turned away |

**`START-TRUNK` on a trunk that is already UP knocks it down for about a minute.** Only use it on
one that is down.

---

## 5a. MACHINE NAMES

A machine is a number to the kernel and can be a NAME to a person. `SET-NAME` in `@CHAT-MON`
sets what this machine calls itself.

```
C-M: SET-NAME FJELL
this machine is now FJELL

[TESTER/LOBBY] /map
CHAT: map this is FJELL - D102 up  D103 up
```

**Proved on D100 2026-08-25**, server segment 2606.

| Rule | Why |
|---|---|
| an EMPTY name CLEARS it | the machine goes back to `D<number>` everywhere, which is exactly how it behaved before names existed - so there is a way back from a typo without a restart |
| **the NUMBER still decides which machine a message came from** | it comes from the sender's magic, which the kernel supplies and no sender can choose. A name is only a LABEL on that number, so a machine may call itself anything and still cannot claim to BE another machine |
| a machine with no name shows as `D<number>` | and that is what an un-upgraded peer looks like |
| `LIST-TRUNKS` shows the NUMBER first, then the name | the operator types `START-TRUNK 102`, so the number has to be what they read first |
| it is NOT written to disk | put `SET-NAME` in the machine's boot mode file beside `START-TRUNK`, which already drives CHAT-MON |

### How a name reaches the other machines

On the **trunk Hello**, after the direction byte that was always there.

**THIS IS BACKWARD COMPATIBLE AND IT WAS MEASURED, not argued.** The old Hello handler reads the
first byte of the text and nothing else, so a server that has not been upgraded takes the
direction byte and ignores the name. Measured on 2026-08-25: D100 was upgraded while D102 and D103
kept the older build, and `LIST-TRUNKS` on D100 answered `102 up 103 up` throughout.

Same property `kTrkRelay` was given, and it is what lets three running machines be upgraded one at
a time.

### PROVED BOTH WAYS BETWEEN TWO MACHINES, 2026-08-25

D100 named FJELL, D102 named VIDDA, both running the same server built once on D100 and carried
over as `CHATSV:BRF` - byte identical at 29541 on both, which is the proof the copy carried the
new build and not a stale one.

```
on D100                          on D102
-------                          -------
C-M: LIST-TRUNKS                 C-M: LIST-TRUNKS
102 VIDDA up 103 down            100 FJELL up

SYSTEM@VIDDA: hello from         SYSTEM: hello from the other machine
             the other machine   TESTER@FJELL: and hello back from this one
```

**Each machine's own speaker stays bare** - `SYSTEM:` on D102, not `SYSTEM@VIDDA:`. The `@name`
means "somewhere else", which is the only reason it is worth printing.

**THE CLIENT ON D102 IS THE OLD BUILD.** Only the server and CHAT-MON were carried across, and the
chat is flagged correctly anyway - because the SERVER decides what a speaker is called and the
client prints what it is given. That is the whole reason the qualification was never left to the
client.

### The client is TOLD which machine it is on

The **welcome's text** carries it. That field was always empty and the client ignored it, so
filling it in is compatible in both directions.

**A CLIENT CANNOT WORK THIS OUT FOR ITSELF** - it has no system number, and nothing in the join
tells it. `/map` says so, but in a sentence written for a person to read, and parsing prose for a
fact is how a client ends up confidently wrong. The server asked the kernel, so the server says so.

The line client shows it as `you are TESTER on FJELL`. The full-screen client puts `TESTER@FJELL`
in its top-right corner - or `TESTER` alone when the server never said, which is what an older
server does.

---

## 6. BUILT versus DESIGNED - do not confuse them

### Built and proved on the machines

Rooms, seats, join/leave/say/nick/topic/who/list/map, the sixteen-seat limit, trunking between
machines, `/who` across trunks, relay through a middle machine, automatic trunk re-join, dead-port
detection, the operator front end, and start-up from a cold boot.

**HISTORY IS BUILT - and an earlier version of this page said it was not.**
The correction is worth keeping visible because the claim was made from memory rather than from
the source, which is the exact failure this document exists to prevent.

```planc
histSave(histSlot, slot, textAt, textLength)     % on every line said
...
histReplay(histSlot, senderMagic)                % on every join
```

Lines are written to a **disk file** (`makeHistFile`, and `STATUS` reports its size as `disk nKB`)
and replayed to a joiner as `kHist` (16). So **a joiner DOES see what was said before they
arrived**. `CHAT-HISTORY-DESIGN.md` describes the design.

**NOT verified live by this page's author** - read from the source, not watched on a machine. The
one-line check is: join a room, say something, `/quit`, rejoin, and see whether the line comes
back.

### DEDUP IS PROVED - it took a third trunk

Kind 53 carries the line number the origin stamped and each server remembers the last 64
`(origin, id)` pairs. This sat as "deployed and not proved" for weeks, because on a CHAIN there is
exactly one path between any two machines, so `dupe` could only ever read 0 - **and a completely
broken implementation would print the same 0.**

`START-TRUNK 103` on D102 made the triangle and settled it. MEASURED on D102, 2026-08-26, either
side of one line typed on D103:

```
before   SEATS 0/16 ... empty 422 bounce 1 relay 0 dupe 0 refuse 0
after    SEATS 0/16 ... empty 426 bounce 2 relay 1 dupe 1 refuse 0
```

and the client showed the line **exactly once**:

```
SYSTEM@SKOGEN: now there is a triangle and this should arrive once
```

**`dupe 1` IS THE EVIDENCE. The single line on screen is NOT.** One copy on screen is also exactly
what a machine that only ever received one copy would show. The counter is the only thing that
says a second copy arrived by the other path, was recognised, and was dropped.

`relay 1` on the same reading says D102 also passed a copy onward - which D100 then dropped as a
repeat of the one it had already relayed. The loop terminates, which is what the hop count and the
id are both for.

### BUILT AND PROVED ON THE MACHINE

**Private messages.** `/tell NAME text`, or `/tell D102!NAME text` for somebody on another
machine. Proved on D100 2026-08-25 against server segment 2605B, START ADDRESS 32255B:

```
[TESTER/LOBBY] /tell NOBODYHERE are you there
not sent to NOBODYHERE: nobody of that name is here now, and nothing is kept for later

[TESTER/LOBBY] /tell TESTER talking to myself
*(D100!TESTER) talking to myself
sent to: D100!TESTER

[TESTER/LOBBY] /tell D102!GHOST hei
not sent to D102!GHOST: nobody of that name is here now, and nothing is kept for later
```

All four kinds exercised: 17 sent, 18 delivered, 19 receipt, 20 refusal.

| What | Rule |
|---|---|
| a bare alias | searched on THIS machine and on every machine we hold a member list for |
| exactly one match | delivered |
| two or more | **REFUSED with both names**, never guessed at |
| nobody | refused - and **nothing is kept**, see below |
| `D102!NAME` | searched on that machine alone |

**The sender arrives QUALIFIED** - `D100!TESTER`, not a bare `TESTER` - because what is displayed
has to be what can be typed back. **The client marks it** (`*(...)`), and that is not decoration:
a reply typed as an ordinary line would go to the whole room, which would be a privacy failure
caused entirely by the display.

**A DELIVERY RECEIPT IS SENT EVERY TIME**, not only on ambiguous names. Refusing an ambiguous alias
protects the sender when the collision is visible; the receipt protects them when it is not - a
second RONNY who logged in a moment ago, a machine that just went down. One line makes a wrong
delivery visible at once instead of days later.

**THERE IS DELIBERATELY NO STORE AND FORWARD.** "nobody of that name is here now" is a refusal, not
a queue. A message that waits for somebody is a mailbox - a different product with different rules,
and SINTRAN already has mail for that.

### BUILT BUT NOT PROVED - a direct message CROSSING a trunk

Kind 54 carries a direct message to another machine, with the same six-byte header as a relayed
room line so it is de-duplicated by machinery that already exists. **The code is on D100 and no
direct message has ever crossed a trunk** - proving it needs a client joined on D102 at the same
time, which has not been done.

**ONE HOP ONLY, AND THAT IS DELIBERATE.** A room line is flooded to every peer and sorted out at
the far end. A private message must not be: flooding one hands it to machines that have no business
seeing it, which is a privacy failure dressed up as routing. So it goes only to the machine holding
the person, and anything further is **refused in words**.

On the D102 - D100 - D103 chain that means D100 reaches both ends and each end reaches only D100.
**That limit is real and it is visible** - the sender is told, rather than the message vanishing.
Routing onward by the target's machine is the next step and needs a way to carry a refusal back,
which no kind does yet.

### Designed, NOT built

**Multiple windows.** One room at a time. The bar in the full-screen mockup is drawn but inert, and
its `=KARI` entries now have a protocol behind them even though no window does.

### Known limits

- **sixteen seats per server**, and a burst of twenty joins has killed XMSG;
- **an RT-load orphans every joined client** - the tell is a missing echo, and every client must
  quit and restart;
- a **nickname may be 16 characters**, which does not fit the full-screen client's 13-column
  speaker field.

---

## 7. Keeping this page honest

It was written because four ordinary questions - does my own message come back, are joins shown,
are leaves shown, can I send a private message - could only be answered by reading PLANC source.
The wire format has a registry that is enforced by tests; the PRODUCT had nothing.

**When the server's behaviour changes, change this page in the same commit**, the same rule the
protocol registry already follows. In particular: any change to who `broadcast` leaves out is a
change to section 3, and section 3 is what client authors build against.
