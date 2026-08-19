# Federated CHAT-LOBBY: trunking rooms across machines

**Status: design, nothing built.** Decisions taken 2026-08-18; the constraints below are measured or
quoted, and are marked as such. Anything not marked is a choice, and can be changed.

## What we are building

A `CHAT-LOBBY` on every machine. Clients only ever talk to their **local** lobby. The servers link to
each other like telephone trunks, so a room spans machines: you see users on other machines, and a
message reaches everyone in the room wherever they happen to be logged in.

## Decisions

**Trunks are set up by OPERATOR COMMAND, not a config file.** A front-end program tells the server
`TRUNK D102` at runtime. Persistence comes for free from `LOAD-MODE:MODE` re-issuing the commands at
boot, which is where the machine's other services are already configured.

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

**Phase A - the RT server and its front end.** No trunking yet.
Make CHATSV an RT program started from the mode file. Add a command port and a front-end program
(`CHATADM`, following `FS-ADMINISTRATOR`). Commands: `STATUS`, `STOP`. Proves the shape, and gives the
clean shutdown that releases the `CHAT-LOBBY` name.
*Proof:* start it from LOAD-MODE, drive it from the front end, stop it, and see the name gone from
`LIST-NAMES`.

**Phase B - one trunk, two machines, direct.**
Open `CHAT-TRUNK` with `XMPOPNM`. Add `TRUNK <system>` and `UNTRUNK <system>` to the front end, and a
hello/keepalive between servers. Nothing is federated yet; the trunk just comes up and stays up.
*Proof:* D100 and D102 trunked, both showing the peer up, and `LIST-NAMES` showing the room's seats
untouched by the trunk - that is the whole point of constraint 1.

**Phase C - federate the state.**
Exchange room and user lists over the trunk. `/who` finally has something to answer, across machines.
Qualified names land here.
*Proof:* a user on D100 sees a user on D102 in `/who`.

**Phase D - federate the messages.**
Forward `say` over trunks with origin and hop count. Relay for peers that are not direct neighbours,
which is the chain case.
*Proof:* three machines where one pair is only reachable through the third, and a message from the
first arrives once - exactly once - at the third.

**Phase E - resilience.**
The peer table, HELLO keepalives and backoff. Trunk loss detection and automatic re-join. Re-sync on
every transition to Up. A vanished remote member's seat freed on its home machine. Dedup if a mesh
delivers twice.
*Proof:* stop the peer machine's chat server mid-conversation, watch the other side mark it Down and
drop its users; start it again and watch the trunk come back with no operator action, with the user
list correct rather than doubled.

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
