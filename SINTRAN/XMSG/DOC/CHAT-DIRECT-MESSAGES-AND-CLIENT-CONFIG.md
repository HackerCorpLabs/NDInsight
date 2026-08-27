# Direct messages, remembered aliases, and friends

**Status: design, nothing built.** Companion to `CHAT-FEDERATION-DESIGN.md`, which covers the trunks
this rides on.

## What we are building

Send a message to one person, on this machine or another, **without being in the same room** - or in
any room. Remember your alias between sessions. Keep a friends list.

## The command shapes

Two prefixes, and the parallel is the point:

```
/command ...        does something to your session      (already true today)
@person message     says something to one person        (new)
anything else       is said to the room                 (already true today)
```

So `typed(0) = 47` means command, `typed(0) = 64` means direct message, and everything else is
speech. The client already tests the first character for `/`; this is one more branch beside it.

**Long and short forms, exactly as proposed:**

```
/direct RONNY hello there          the explicit form, reads well in /help
/direct D100!RONNY hello there     ...qualified by machine
@RONNY hello there                 the short form people will actually type
@D100!RONNY hello there            ...qualified by machine
```

**Why `!` for the machine and not `@`.** `@` is taken by the prefix. `@RONNY@D100 hello` is ugly and
needs a parser that counts `@` signs. `machine!alias` reads left to right as route-then-person, is
one character, and has an honourable precedent in UUCP addressing from exactly this era. It also does
not collide with the `:` and `(` `)` that SINTRAN filespecs already use.

**Display uses the same form**, so what you read is what you can type back:

```
RONNY: hello everyone                 someone on your own machine
D102!KARI: hello from over here       someone on another machine
*D102!KARI: are you free?             a direct message TO you, marked so it cannot be missed
```

## Duplicate aliases: the hard part, and the rule

Aliases are unique **per machine** - `findByName` already enforces that - and cannot be made unique
across machines without a central registry. Worse, as you say, two machines can each legitimately
have a `RONNY` long before they ever start talking to each other. There is no moment at which a
global rule could have been applied.

**So do not try. Allow duplicates, and never guess who was meant.**

```
@RONNY hello        one RONNY in the federation   -> delivered
                    two or more                   -> REFUSED, with the list
```

```
CHAT: which RONNY? D100!RONNY, D102!RONNY
```

The refusal is the feature. A direct message going silently to the wrong person is a privacy
failure, not an inconvenience, and it is exactly the kind of quiet wrong answer this project has been
bitten by elsewhere. Refusing costs one retype and cannot mislead.

**AND EVERY DELIVERY SAYS WHERE IT WENT**, ambiguous or not:

```
@KARI are you free?
CHAT: sent to D102!KARI
```

This is the other half of the same idea and it matters more than it looks. Refusing an ambiguous
name protects you when the client can SEE the collision; the delivery report protects you when it
cannot - a machine that has just gone Down, a second `RONNY` who logged in a moment ago, a trunk that
came back with a different user list. You are told the qualified name that actually received it,
every time, so a wrong delivery is visible immediately instead of days later. It costs one line.

**Rejected: local-first.** Delivering to the local `RONNY` when a remote one also exists is
convenient right up to the day you meant the remote one, and then your message went to a stranger
with no sign that anything was wrong.

**Never renaming anyone retroactively.** Forcing the second `RONNY` to rename when trunks connect
punishes whoever's machine happened to boot later, and the timing is arbitrary.

## Delivering to someone in another room

Today the server's `memberUsed` / `memberMagic` / `memberName` table is flat, because there is one
room. Direct messages need a user who is **not in your room**, so the table becomes
**server-scope with a room attribute**, not per-room. That is a small change now and an awkward one
later, so it belongs in the first room-aware phase.

Delivery paths:

 - **same machine** - look the alias up in the local table, send to that magic number.
 - **another machine** - hand it to the trunk addressed `machine!alias`; the receiving server looks
   the alias up locally and delivers. If that machine is not a direct neighbour it relays, which is
   the chain case already in the federation plan.
 - **not logged in** - refused, and said plainly. **No store and forward.** A message that waits for
   someone is a mailbox, which is a different product with different rules; SINTRAN already has mail
   for that. Say "not logged in" rather than half-implementing it.

## The client config file

`CHAT:CNFG` in the user's own SINTRAN area - which is per-user by construction, so there is nothing
to configure about where it lives.

**It is reachable from PLANC today.** The runtime provides the monitor calls needed as ordinary
`MONn` routines: **50** open, **54** close, **1** inbt, **2** outbt. Nothing exotic, no interface
routine, no assembler.

Line-oriented, keyword then value, because it has to be editable by hand when there is no other
tooling:

```
ALIAS RONNY
ROOM CHAT-LOBBY
FRIEND D100!OLAV
FRIEND D102!KARI
```

Rules that keep it pleasant:

 - **unknown keywords are ignored, not errors.** A newer client's file must not break an older one.
 - **missing file is not an error** - it means a first run, and the client asks for an alias or falls
   back to the SINTRAN user name.
 - **written back when something changes**, so `/nick OLAV` is remembered next time. That is the
   whole point: today the nickname is hard-coded to `RONNY` and the "what is not finished" note in
   `CHAT.PLNC` already lists reading it from somewhere as an open item.

## Seeing who is out there

Refusing an ambiguous name is only reasonable if you can find out what the choices are. Two
commands, in the existing one-word style:

```
/machines            the trunks, and whether they are up
/who                 everyone, qualified, grouped by machine
/who D102            just that machine
```

```
/machines
CHAT: D100  this machine   3 users
CHAT: D102  up             2 users
CHAT: D103  DOWN           last heard 11.42
```

```
/who
CHAT: D100  RONNY, OLAV, KARI
CHAT: D102  KARI, SVEIN
```

Two people called `KARI` are visible at a glance, which is exactly when you need to know it - before
you type `@KARI` and get refused, not after.

**`/who` finally has an answer.** It exists in the client today and deliberately says
"NOT ON THE WIRE YET", because there was no query kind in the format. This is that kind, and the
same reply shape serves both commands: a list of machines, each with its users.

**`/machines` shows the peer table from the federation design directly** - system, state, last heard.
A machine that is Down still appears, because "D103 is off" is a useful answer and an empty list is
not.

## Friends

```
/friends              list them, showing who is on right now
/friend D102!KARI     add
/unfriend D102!KARI   remove
```

Aligned with the existing commands: one word, at most one argument, same as `/join` and `/nick`.

**Friends are stored qualified**, always - `D102!KARI`, never bare `KARI` - because an unqualified
friend is ambiguous the moment two machines have that name, and a friends list that silently points
at the wrong person is worse than no list.

**Showing who is online needs presence, which needs federation.** So the list can be stored and
edited from day one, but "who is on" is gated behind the phase that exchanges user lists.

## New message kinds

Three, and each needs a golden in **both** directions before it is built:

```
kDirect     client -> server   target machine, target alias, text
kDirGot   server -> client   from machine!alias, text
kDirectSent client -> ...      server -> client, the qualified name it was delivered to
kDirectBad  server -> client   why it failed, and the candidate list when ambiguous
kWhoAsk     client -> server   optional machine filter
kWhoList    server -> client   machines, each with state and its users
```

`kDirectBad` carrying candidates is what makes the ambiguity rule usable rather than annoying.

Reminder from the `/nick` defect: the C# and PLANC ends share no code, so those bytes are the only
agreement, and an unpinned direction is where the two silently drift apart.

## Order of work

Follows the federation phases rather than fighting them:

1. **Config file and alias memory** - needs no server change at all, and can be built and proved on
   one machine today. `/nick` starts persisting.
2. **Local direct messages** - the server-scope member table, `kDirect` / `kDirGot` /
   `kDirectSent` / `kDirectBad`, `@` and `/direct`. Still one machine; ambiguity cannot arise yet,
   but the qualified syntax and the delivery report are there from the start, so nothing has to
   change later and the habit of reading "sent to X" forms before it matters.
   `/who` and `/machines` land here too - trivial with one machine, and the reply shape is then
   already right when there are several.
3. **Friends list, stored** - qualified names, no presence yet.
4. **Remote direct messages** - after federation phase C, when a server knows who is on other
   machines. Ambiguity refusal becomes real here.
5. **Friend presence** - who is on, across machines.

Step 1 is worth doing first regardless of everything else: it is small, self-contained, needs no
trunk, and removes the hard-coded `RONNY`.
