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
| `/leave` | leave the room |
| `/quit` | leave and exit the client |

Anything not starting with `/` is said to the room.

**There is no `/tell` and no private message.** See section 6.

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
| `dupe` | repeats recognised and dropped. **Always 0 on a chain - see below** |
| `refuse` | joins turned away |

**`START-TRUNK` on a trunk that is already UP knocks it down for about a minute.** Only use it on
one that is down.

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

### Built but NOT proved

**Dedup.** Kind 53 carries the line number the origin stamped and the server remembers the last 64
`(origin, id)` pairs. It is deployed on all three machines and `dupe` reads 0 - **which is the only
reading a CHAIN can produce**, because there is exactly one path between any two machines. A
completely broken implementation would print the same thing. Proving it needs a third trunk.

### Designed, NOT built

**Private messages.** There is no person-to-person kind anywhere in the server - the kinds are
1-16 (room), 32-38 (admin) and 48-53 (trunk), and none of them is a direct message. There is a
design note in `CHAT-DIRECT-MESSAGES-AND-CLIENT-CONFIG.md`.

**This matters for the full-screen design:** the window bar in the mockup shows `4 =KARI 1*` and
`5 =TERJE`, where `=` marks a direct message. **Those windows have no protocol behind them.**

**Multiple windows.** One room at a time. The bar in the mockup is drawn but inert.

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
