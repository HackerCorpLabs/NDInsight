# Federated CHAT-LOBBY: trunking rooms across machines

**Status: BUILT AND RUNNING on D100, D102 and D103, phases A to E.** Last measured 2026-08-25.
Decisions taken 2026-08-18; the constraints below are measured or quoted, and are marked as such.
Anything not marked is a choice, and can be changed.

Two people on two different ND-100s share a room, and a third machine with no trunk to either of
them joins in through the one in the middle. What is NOT proved is called out in Phase E - read
that before trusting dedup.

## What we are building

A `CHAT-LOBBY` on every machine. Clients only ever talk to their **local** lobby. The servers link to
each other like telephone trunks, so a room spans machines: you see users on other machines, and a
message reaches everyone in the room wherever they happen to be logged in.

## Decisions

**Trunks are set up by OPERATOR COMMAND, not a config file.** A front-end program tells the server
`TRUNK D102` at runtime. Persistence comes for free from `LOAD-MODE:MODE` re-issuing the commands at
boot, which is where the machine's other services are already configured.

**That front end is `CHAT-MON`, and it is the server's configuration program in general.** Trunks are
the first thing it sets, not the only thing - every server parameter that needs changing while the
server is running belongs here rather than in a rebuild. It is an ordinary program, not reentrant and
not RT, and it talks to the server over XMSG on a command port separate from `CHAT-LOBBY`.

**The server becomes an RT program with a front end.** This is the ND-native shape and there is a
working example of it on the machine already: **`FSART` is the RT program and `FS-ADMINISTRATOR` is
its front end**, and the front end reaches it over XMSG like any other client. We have watched that
pair start, refuse, and recover this week - it is the model to copy, not invent.

That split also settles two problems already on the books:

 - CHATSV today blocks for ever in `XMPFRCV` with `XFWTF` and cannot be told anything. A command port
   gives it a way to be spoken to - which is the same missing piece as a clean shutdown.
 - An operator can then close the port properly on shutdown, so the `CHAT-LOBBY` name is released
   instead of lingering in XROUT's name table (measured: the name outlived the program when it was
   stopped with `STOP-TERMINAL`).

**Topology is whatever XMSG can already reach - mesh AND chain.** Some machines are direct neighbours
over HDLC or Ethernet; others are only reachable through a third machine. The chat trunks must mirror
that rather than assume every peer is one hop away. This codebase already has a route-through model
for XMSG, so the concept is not new here.

## Constraints that are not up for choice

These come from the manual or from measurement, not from taste.

**1. Trunks need their own NORMAL named port.**
`CHAT-LOBBY` is a connection port (`XMPOPCN`) with a free-connection counter. XROUT decrements that
counter for every letter it forwards there, so a trunk connecting to it would spend one of the room's
seats - and while the seat leak is unfixed, spend it permanently. Open a second port, `CHAT-TRUNK`,
with `XMPOPNM`: a normal named port has no counter at all. Seats then limit *local users*, which is
what they are for.

**2. There is no service discovery. At all.**
`XMPFLMP` returns information about "its own open ports and its own messages" - the task's own, not
the machine's and not the network's. `LIST-NAMES` is an X-COMM **operator** command, not an API. So a
program cannot ask XMSG who else is out there, and the peer list must be supplied by a human or a
mode file. This is why the operator-command option is not merely a preference; the alternatives all
smuggle the same list in somewhere.

**3. Loop prevention is required from the first trunked message.**
With mesh or chain, a message can come back to a server that has already seen it. Every trunked
message carries the **origin system** and a **hop count**; XMSG's own header does exactly this and our
relay already decrements it, so mirror the mechanism rather than inventing one. Without it a
three-machine loop reflects for ever.

**A HOP COUNT ALONE IS NOT ENOUGH, and the difference matters.** It stops a message travelling for
ever. It does NOT stop the same message ARRIVING TWICE by two different routes - and in a mesh that
is the common case, not the rare one. Telling one line arriving twice from two different lines needs
a NAME for the line, and that name has to be stamped by the machine it was typed on: a relay cannot
assign it, because a machine with two neighbours would stamp a different number for each copy and a
node further out would see two names for one line. That is what kind 53 carries, and it is why the
origin stamps and every relay passes the number on untouched.

**4. Names must be system-qualified.**
`RONNY` on D100 and `RONNY` on D102 are different people. The room's "is this name taken" test and
everything shown on screen need the qualified form. The 16-byte name field will not hold
`name@system` for long names - decide whether the system travels as a separate field (preferred) or
inside the name.

**5. New message kinds change a pinned wire format.**
`ChatMessageGoldenBytesTests` pins the layout byte for byte, and the C# and PLANC ends share no code -
those bytes are the only agreement. Every new kind moves both ends **in the same commit**, with a
golden for **each direction**. The `/nick` defect was precisely an unpinned direction: the client put
the new name in one field, the server read another, and both sides silently did nothing.

**6. Each trunked pair needs DEF-REMOTE and DEFINE-FRIEND-SYSTEM, both ways.**
Manual, per machine, whatever the discovery mechanism. A missing friend grant reports as `XRUNN` or
`XRNRO`, which reads as "unknown server" and has sent this project chasing the wrong thing twice.

## Trunk lifecycle: who is the client, and what happens when a machine is away

**Neither side is the client. There is no connection to be one of.**

This is the part that dissolves the question rather than answering it. XMSG named ports are
MESSAGE PASSING, not streams. There is no session to open, nothing to hold, and nothing that "drops"
when a machine goes away. Both servers open `CHAT-TRUNK` with `XMPOPNM`; each one is a server when a
letter arrives and a client when it sends one. Symmetric, and the medium is what makes it so.

So a "trunk" is not a connection. **A trunk is a BELIEF that a peer is reachable**, held as soft
state and refreshed. That reframing decides the rest of the design:

```
peer table, per server:
    system        D102
    state         Up | Down | Unknown
    last heard    when a letter from it last arrived
    retry at      when to send the next HELLO while it is not Up
```

**Coming up, and coming back.** `TRUNK D102` adds a row and sends `HELLO`. Anything arriving from a
peer marks it Up and stamps last-heard - including its own unsolicited `HELLO`, so whichever machine
boots second brings the trunk up without the first having to notice. A machine that has been off for
a day needs no operator attention when it returns: its own mode file re-issues its `TRUNK` commands,
it says `HELLO`, and both sides mark each other Up.

**Detecting an absent peer, cheaply.** XMSG tells us two ways, and the first is free:

 - the letter comes straight back undeliverable - `XMTRE`, or XROUT answering `XRUNN` for a system it
   cannot reach. Immediate, no waiting.
 - nothing arrives for longer than the keepalive period.

**Backoff is required, not optional.** A machine that is switched off must not be probed every few
seconds for a week. Retry quickly at first, then stretch to a cap - something like 15s, 30s, 60s,
then every 5 minutes. Cheap to get right and unpleasant to get wrong on a shared network.

**Re-sync on EVERY transition to Up, not just the first.** A peer that comes back has almost
certainly restarted, so everything we remember about its users is stale. Treat Up as "start again":
drop its members, ask for its room and user lists afresh. This is the strongest argument for soft
state over explicit leave - a machine that dies never sends a goodbye, and a half-open link can lie
for a long time.

**Going Down must clean up the view.** When a peer is declared Down, its members are removed locally
so nobody is shown chatting from a machine that is not there, and a direct message to one of them
fails honestly instead of vanishing.

**A tie-break exists if ever needed.** If some future step genuinely needs one side to lead - a state
merge, say - use the lower system number as the leader. Deterministic, needs no negotiation, and both
sides compute the same answer. Not needed for anything in the phases below.

**Costs nothing in seats.** `CHAT-TRUNK` is a normal named port with no free-connection counter, so
retries and keepalives spend nothing on the peer, however long it is away. That is the second reason
for constraint 1, and it is why the trunk must never point at `CHAT-LOBBY`.

## Phases

Each phase ends with something proved on a real machine, not argued.

**Phase A - the RT server and CHAT-MON, its front end.** No trunking yet.

**Half of this is DONE and measured.** `CHATSV` already runs as the RT program `CHATSER` and holds
no terminal - `LIST-RT-DESCRIPTION CHATSER` on D100, 2026-08-19, shows `ACTIVE / I/O-WAIT`, segment
**2513B**, priority `75B`. What is left is the front end.

**`CHAT-MON` is the configuration program**, following `FS-ADMINISTRATOR`: an ordinary program that
reaches the server over XMSG like any other client, on a command port separate from `CHAT-LOBBY`.
It is where every server parameter is set from now on, not just trunks - trunks are simply the first
thing it configures.

Commands for this phase: `STATUS`, `STOP`. Trunk commands arrive in Phase B.

*Proof:* start the server from LOAD-MODE, drive it from `CHAT-MON`, stop it, and see the server gone.

> **Corrected 2026-08-19.** This phase used to claim it "gives the clean shutdown that releases the
> `CHAT-LOBBY` name". **That premise is refuted** - SINTRAN clears the name by itself when the task
> terminates, measured 2026-08-18. `CHAT-MON` is worth building for configuration and for a stop that
> does not need `STOP-TERMINAL`, not to fix a leak that does not exist.

#### A server with two ports CANNOT block on both - the loop has to change

**QUOTED from ND-60.134.2, section 4.3.2.10.** This is a constraint, not a preference, and it was
found before writing any PLANC because building the obvious thing would have produced a silent
fault rather than a failure.

`XFRCV` receives on **one specified port**:

```
T:=XFRCV (BONE XFWTF/ XFWAK); A:=PORTNO; *MON 2XMSG
```

There is **no receive-on-any-port form**. Today `CHATSV` blocks on `CHAT-LOBBY` with `XFWTF`, and a
server that keeps doing that **would never see an admin message at all**. It would not fail, it
would not log anything - it would sit in the lobby wait while `CHAT-MON` waited for an answer that
could not come. That is exactly the kind of gate that cannot tell you it closed.

**The mechanism the manual gives instead is `XFWAK`:**

> If no message is waiting, then if `XFWTF` is set, the task is suspended until the next message
> arrives, otherwise a zero status is returned and, if `XFWAK` is set, the next transmission to that
> port will lead to a 'wake up' (RT - MON 100) of the receiver task. **This 'wake up' option can be
> enabled on more than one port at a time.**

So the loop becomes:

 1. non-blocking `XFRCV` on the lobby port with `XFWAK` - takes a message if one is there, arms the
    wake-up if not;
 2. the same on the command port;
 3. when both came back empty, wait as an RT task;
 4. a transmission to EITHER port wakes it; go round.

**And the trap, from the same paragraph:**

> When the 'wake up' is done, **the message is not received**, and so the receive must be repeated.

The wake-up is a doorbell, not a delivery. A loop that treats being woken as having received
something drops every message it is woken for.

**`XFWAK` is bit 14, `0x4000`** - already confirmed against the oracle.

#### SETTLED: the wake-up is MON 100, so the server TERMINATES rather than waits

`MON 100B` is **`StartRTProgram`** (`Developer/MON/calls/100B_StartRTProgram.yaml`). So the manual's
"'wake up' (RT - MON 100) of the receiver task" means the task is **RT-started**, exactly as
`@RT CHATSER` starts it.

That decides step 3, and it removes the wait entirely:

```
drain the lobby port   (non-blocking, XFWAK armed)
drain the command port (non-blocking, XFWAK armed)
if both were empty  ->  TERMINATE
```

A transmission to either port then RT-starts the program again, and it drains both. **There is no
wait primitive to choose**, which is why the question is closed rather than answered.

**Two consequences that must be built in from the start:**

 1. **`XFWTF` must go.** Not "should" - a task already blocked in `XFRCV` is ACTIVE, and RT-starting
    an active program does not queue a second run, so a wake-up arriving during the block is lost.
    Keeping the blocking receive on the lobby and merely adding `XFWAK` to the command port would
    starve the command port exactly as before.
 2. **The state has to survive termination.** Module-level PLANC data lives in the program's data
    segment, and an RT program's segment stays loaded between runs - the member table, the port
    numbers and the magic survive. **The stack does not**: `INISTACK` runs every start.
    So the opening work - `XMPOPCN`, `XMPOPNM`, `XMPFP2M` - must be guarded by a flag held in
    module data, or the second start tries to re-open a name it already holds and fails.

**This is a restructure of a working server, not an addition to it.** It is worth doing carefully
and proving on D100 before any trunk work is layered on top.

**Phase A2 - install on D102 and D103.**
Everything so far exists only on D100. Federation cannot be tested on one machine, so the client, the
server and `CHAT-MON` all have to be built and installed on **D102 and D103** as well - the same
`CHATSV.MODE` / `CHATCC.MODE` builds, the RT-LOADER sequence for `CHATSER`, and the reentrant install
for the client under a name that does not shadow its `:PROG` (see
`Developer/Languages/Application/PLANC-RT-AND-REENTRANT-PROGRAMS.md`).

Each machine also needs the monitor call package if `CHAT:CNFG` is to work there - `MON-CALL-1B-A00`
linked between the program and the PLANC runtime.

*Proof:* `@NDCHAT` on D102 and on D103, each remembering its own user's nickname, both talking to
their own local server.

**Phase B - one trunk, two machines, direct.**
Open `CHAT-TRUNK` with `XMPOPNM`. Add `TRUNK <system>` and `UNTRUNK <system>` to `CHAT-MON`, and a
hello/keepalive between servers. Nothing is federated yet; the trunk just comes up and stays up.

Each trunked pair needs `DEF-REMOTE` and `DEFINE-FRIEND-SYSTEM` **both ways, on both machines** -
constraint 6, and it is manual work per pair.

*Proof:* D100 and D102 trunked, both showing the peer up, and `LIST-NAMES` showing the room's seats
untouched by the trunk - that is the whole point of constraint 1.

**Phase B2 - `/map`: let ORDINARY USERS see how the network is wired.**

Ronny, 2026-08-20. The client gets a command that lists the machines and the trunks between them,
so an end user can see the shape of the network rather than having to ask an operator.

**It is a ROOM kind, `Map` (12), not an admin one, and that line matters.** It arrives on
`CHAT-LOBBY` from an ordinary member and costs a seat like any other room message - the asker
already holds one. It shows the configuration and changes nothing. `TRUNK` and `UNTRUNK` stay in
`CHAT-MON` on the command port, because **seeing how the network is wired is not the same right as
rewiring it**.

One kind both directions, the same shape as `/who`: the client sends it with empty text, the server
answers with readable text. That is why it costs no wire change.

**What it can honestly show grows with the phases**, and the command should not pretend otherwise:

 - after **B** - this machine's own trunks and whether each is up;
 - after **C** - the other machines' names and their users;
 - after **D** - the full graph, including peers reachable only through a relay, and the hop count.

*Proof:* a user on D100 types the command and sees D102 and D103 with the trunks between them, and
what it prints matches what `CHAT-MON`'s `STATUS` says on each machine.

**Phase C - federate the state.**
Exchange room and user lists over the trunk. `/who` finally has something to answer, across machines.
Qualified names land here.
*Proof:* a user on D100 sees a user on D102 in `/who`.

> **STATUS 2026-08-24 - the chain is BUILT and the gap is MEASURED.** D103 runs the chat server
> (RT-loaded onto segment 2521 from a BRF copied off D100 - no compiler on that machine, only
> the three libraries) and `LIST-TRUNKS` reads `102 up 103 up` on D100, `100 up` on D103. There
> is deliberately NO D102-to-D103 trunk, so D100 is the middle exactly as this phase requires.
>
> With a client on each machine:
>
> | path | result |
> |---|---|
> | D103 to D100 | delivered, as `SYSTEM@D103` |
> | D100 to D103 | delivered, as `PROBE@D100` |
> | D100 to D102 | delivered - the CONTROL |
> | **D103 to D102** | **nothing** |
>
> The control matters: the same D102 client that saw nothing from D103 received the D100 message
> seconds later, so the silence is the missing relay and not a dead client. Phase D is the only
> thing standing between this and a working three-machine room.

**Phase D - federate the messages. DONE, measured 2026-08-25.**
Forward `say` over trunks with origin and hop count. Relay for peers that are not direct neighbours,
which is the chain case.

**This is what D103 is for.** The chain was built the other way round from the sketch above: the
trunks are **D100-D102 and D100-D103, with no trunk between D102 and D103**, so **D100** is the
machine in the middle and does the relaying. Same shape, same test, and it needed no change to the
design - which of the three sits in the middle is a wiring choice.

*Measured, with the control run both ways:*

| | |
|---|---|
| D102's trunks | `100 up` - **no trunk to D103 at all** |
| Typed on D103 | `HOP THREE FROM D103` |
| Seen on D102 | `SYSTEM@D103: HOP THREE FROM D103` |
| D100, the middle | `relay 2` |
| D102, an end | `relay 0` |

The speaker shows as **`SYSTEM@D103`, not `@D100`** - that is the carried origin doing its job. A
receiver's usual trick is to name the speaker after the system the letter arrived from, which on a
relayed line is the RELAY, and the user would have been shown on the wrong machine.

The `relay 0` on an end machine is what makes the counter mean anything: an end excludes the peer it
heard from, so it has nobody left to send to. A non-zero count on an end machine means the topology
is not what somebody thinks it is.

**It rolled out one machine at a time, and that was designed for.** Kind 52 is a SEPARATE kind, not
extra fields on kind 51, so a server that had not been upgraded ignored it and its direct trunks
carried on working. D103 ran the old build throughout the D100/D102 upgrade and `103 up` never
dropped.

**Phase E - resilience.**
The peer table, HELLO keepalives and backoff. Trunk loss detection and automatic re-join. Re-sync on
every transition to Up. A vanished remote member's seat freed on its home machine. Dedup if a mesh
delivers twice.
*Proof:* stop the peer machine's chat server mid-conversation, watch the other side mark it Down and
drop its users; start it again and watch the trunk come back with no operator action, with the user
list correct rather than doubled.

**Trunk loss and automatic re-join: DONE and measured.** The idle sleep is the clock, the wait
doubles 15 -> 300 seconds, and the cap bounds the WAIT and not the number of tries. A trunk that
ages out comes back on its own.

**Dedup: BUILT AND DEPLOYED 2026-08-25, and NOT YET PROVED. Read this before trusting it.**

Kind 53 carries the line number the ORIGIN stamped, the server remembers the last 64 `(origin, id)`
pairs, and a repeat is dropped without being shown OR passed on. `STATUS` reports a `dupe` count.
All three machines run it and both relay directions were re-tested afterwards.

**But `dupe 0` is the only honest reading today, and it will stay 0.** D100/D102/D103 are a CHAIN.
There is exactly one path between any two of them, so no line can arrive twice and there is nothing
for dedup to catch. What has been shown is that the mechanism is in place and harmless - NOT that it
works. **Proving it needs a third trunk** (D102-D103) to create a real duplicate; the expected result
is that every user still sees each line once and the `dupe` count on the machines with two paths
starts climbing.

Two limits worth knowing before that test:

 - **A line older than the 64-entry window is treated as new again.** Deliberate, and it has its own
   unit test so it is not read as a defect later. A repeat arrives within a hop or two of the
   original, so it is always well inside the window.
 - **Lines arriving on the older kinds 51 and 52 carry no id and are always treated as new.** That is
   exactly the pre-dedup behaviour, and it is the honest answer - inventing a number on arrival would
   be worse than useless, because a second relay would invent a different one for the same line.

## Keep the documentation moving with it

Standing instruction from 2026-08-18: **update the docs and the skill as we go, not at the end.**
The protocol registry under `DOC\protocols\` is the source of truth for wire changes and must be
updated in the SAME commit as the code - the `.json` is the source, the `.md` is generated by
`generate.py`. New message kinds belong there before they are built, marked INFERRED until a capture
proves them.

## Open questions worth settling early

 - **Does a trunk carry one room or all rooms?** One `CHAT-TRUNK` per machine pair carrying every
   room is simpler and matches "telephone trunk". Not yet decided.
 - **Who owns a federated room's seat limit?** Each machine limits its own local users. A remote
   member costs nothing locally - probably right, but it means a room's total size is the sum of the
   machines, not 16.
 - **What happens when a machine holding half a room goes away?** Phase E, but the answer shapes
   whether membership is soft state (re-advertised periodically) or hard state (explicit leave). Soft
   state is more forgiving on a network where a link can sit half-open.
