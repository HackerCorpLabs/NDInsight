# What we do not know

**Kept because the unknowns kept arriving one at a time, mid-hunt.** This is the standing list, so a
question can be looked up rather than rediscovered.

**Last swept:** 2026-08-25. Method: every `UNVERIFIED`, `GUESS`, `not established`, `UNPROVEN`,
`ASSUMPTION` and `not decoded` marker in `SRC/`, plus the open questions in `DOC/`.

Not everything unknown is a problem. The list is ordered by **what it costs us**, not by how
interesting it is.

---

## OPEN 2026-08-25: does chat dedup actually work? It is deployed and UNPROVED

Kind 53 carries the line number the origin stamped; the server remembers the last 64 `(origin, id)`
pairs and drops a repeat without showing it or passing it on. Built, compiled clean, RT-loaded on
D100, D102 and D103, and both relay directions re-tested afterwards. `STATUS` reports a `dupe` count.

**Every machine reports `dupe 0`, and that is the only honest reading - it will stay 0.**
D100/D102/D103 are a **CHAIN**: exactly one path between any two of them, so no line can arrive
twice and there is nothing to catch. What has been shown is that the mechanism is in place and does
no harm. **It has NOT been shown to work.**

**The measurement that would settle it:** add a third trunk, D102-D103, making a triangle. Then a
line typed on any machine reaches the other two by two routes. Expected: every user still sees each
line exactly ONCE, and the `dupe` count climbs on the machines that now have two paths. If lines
double, dedup is broken; if `dupe` stays 0 while lines double, the ids are not travelling.

**Do not read `relay N dupe 0` as evidence of anything but reachability.** It is exactly what a
completely broken dedup would also print on a chain.

Two limits that are deliberate, both with unit tests so they are not mistaken for defects later:

 - a line older than the 64-entry window is treated as NEW again;
 - lines arriving on the older kinds 51 and 52 carry no id and are always treated as new, which is
   the pre-dedup behaviour and is the honest answer.

---

## SETTLED and STILL OPEN 2026-08-18: the version J TAD driver source

`DOC/XMSG-NPL-SOURCE-AUDIT.md` was checked claim by claim against the source it cites and then
applied. What it settled is out of this list: the four wrong registry meanings (`Bmmx`, `Sycn`,
`Reco`, `Cesc`), the "20-byte" echo/break table (it is 16 - an octal literal read as decimal), the
"intrinsic 0x00 prefix" before ECKM/BMMX (it is CRHEOD's deliberate odd start so the word-sized
payload aligns), the TMOD bit decode, and twelve opcodes that now have names.

**Four things it could NOT settle, and they stay here:**

 - **Does a Release L machine still send REJE (0xFE), EDRS (0x29) or TREP (0x2A)?** All three
   constants are in the L07 and M06 symbol tables and the J driver plainly uses all three, but we
   have never captured one. Only a capture settles it. We now SEND a REJE for an opcode we cannot
   name; if a real peer ever answers one of ours, that is the measurement.
 - **What the SYCN payload values 0x0002 / 0x0003 / 0x0006 / 0x000A encode.** The J driver shows the
   channel and the one-word payload and nothing else - whatever writes those particular codes sits
   above the driver and is not in that listing. The observed stepping stays MEASURED as data.
 - **The FBSI (0x15) payload `01 08`.** The driver's buffers are `NOBUFF` buffers of `FBSIZ` bytes,
   so the pair could be that, or `FBSIZ = 0x0108 = 264`, or neither. TADADM builds the trailer and
   TADADM is on segment SG36, which is not in this source. Speculation only.
 - **The names of the raw setup opcodes 0x06 / 0x07, and of the 0xFD notify.** `7CORQ=000006`,
   `7CORS=000007` and `7POLL=000375` sit at exactly those values in the J symbol table - but the
   `7`-prefix symbol space carries several unrelated families with overlapping small values, so this
   is a VALUE MATCH ONLY. They stay raw in the code until captured semantics confirm them.

**One thing the audit itself got wrong, corrected here:** it says `TadOpcodes.cs` "already lists all
of these names and values". EDRS (0x29) was missing from that table, and so was ESRS (0x20) - a
captured escape response decoded as a bare `0x20`. Both added.

**Two questions this source explicitly cannot answer** and which are unchanged: the accept parameter
trailer (B3) and the session-port rule (T1). Both are TADADM's, on segment SG36, absent from the
listing. They still need a machine experiment or the TADADM binary.

---

## SOLVED 2026-08-18: the "random" FA push stall is a leaked connection seat

Measured, and it is arithmetic rather than anything subtle. `*FA-SERVER` free seats, from
`X-C` -> `LIST-NAMES`:

```
after FS-ADMINISTRATOR / START-SERVER          30
one refused create + one successful overwrite  28
one more successful push                       27
```

**One seat per transfer, succeeded or refused, and none comes back.** Thirty transfers after a
restart and the server answers new connect letters with silence.

That retires the standing puzzle. "Three of six pushes stalled at blocks 10, 19 and 21" - the block
number was a red herring; the discriminator is the transfer COUNT since the file server last
started. "Size is not the discriminator" was right and is now explained. And "restarting the FA
server cleared it twice then stopped working" is just 30 fresh seats being spent again.

**Why:** a connection port has a fixed number of seats and XROUT decrements the counter to deliver a
connect letter. The seat has to be given back explicitly - our own PLANC room server does it with
`XMPINFC(0, myPort, 1, 0)` and says in its comment that without it "the room fills up permanently".

**That reasoning was right about CHATSV and WRONG about FA - see the carve below.** CHATSV owns its
connection port and so must hand seats back itself. In an FA transfer we are the CLIENT of the
machine's `*FA-SERVER`; the seat is on the SERVER's port, and neither we nor the ND file server ever
calls `XMPINFC` for it. The fix was the teardown frame, not a missing `XMPINFC`.

**FIXED 2026-08-18**, commits `a6e8f047` (the transfer path) and `ec7f0772` (the refusal path). Our
client sent the SERVER's `Close` (`07C0`) with the two conversation numbers swapped and the wrong
frame flags, where a client sends `Release` (`0782`), sender's conversation first, flags `0x82`/role
`0x84`. All three had to be right together. Measured after: 3 pushes + 2 pulls, `*FA-SERVER` steady
at 29 free.

**"Whose job is the return?" - CARVED AND SETTLED 2026-08-18. Not the FA server's.**
`cos-fa-serv-e04.prog` calls `XMPINFC`/`XSNSP` in exactly ONE place, and it is on the
initialisation path (`fa_init_global_registries` 0x3fee -> 0x7d99 -> 0x9245 -> 0x8d74 -> 0x9f74),
where it publishes the pool one seat at a time. `SAA 0x51` (bytes `f1 51`, XSNSP = 000121 octal)
occurs exactly once in the whole 231KB binary. **No request handler, `Release` handler or
session-teardown routine can reach it**, so there was never a message we could have sent to trigger
one - which is why all three substitution experiments on the teardown frame failed to move the
counter. Full working, with the call chain and the search counts:
`DOC\COSMOS-RE\CARVE-ANSWER-FA-SEAT-RETURN-2026-08-18.md`.

**SETTLED 2026-08-18 - it is ONE BIT, and the guide is silent because it is not an API.**
`XMSG-POFTABS-L03.SYMB` defines the port status word `XPSTA` and its bit 0 in ND's own words:
`SYMBOL 5PKOC=0  % KICK XROUT ON CLOSE (SET BY XROUT)`. XROUT marks the port when it spends a seat
to deliver a connect letter. The kernel's port-close routine `YCLOS` (131306) honours the mark - at
`ram:b30e` it does `BSKP ONE bit 0` on `XPSTA` and, when set, calls `131460 = YKROU` (kick XROUT);
otherwise it skips. So the seat comes back because the SERVER CLOSES THE PORT and the kernel kicks
XROUT - no `XMPINFC`, nothing on the wire.

**Which is why the Release fix worked and the frame experiments could not:** sending the server's own
`Close` meant the server never concluded the session, never closed the port, `5PKOC` never fired.
The leak was three hops upstream of the counter. Full chain and the BSKP decode (cross-checked on
three defined bits): `DOC\COSMOS-RE\CARVE-ANSWER-FA-SEAT-RETURN-2026-08-18.md` section 7.

Full working on the leak itself: `DOC\CARVE-FA-SEAT-LEAK-2026-08-18.md`.

**Practical rule that still holds: read `LIST-NAMES` before blaming a transfer.** A push into a
server with no seats looks exactly like a network fault.

---

## OPEN 2026-08-18: the sync daemon creates what it should overwrite, and now loops on it

Measured while proving the ledger fix. Two facts, both from the machine:

 - the daemon issues **CREATE** when its ledger has never carried a file, and **OVERWRITE** when it
   has - the ledger is its only evidence about what exists remotely, which the planner says out
   loud in its own comment;
 - **a create of a file that is already there is refused** with SINTRAN error 62 = 076 octal,
   "File already exists".

So a remote file the ledger does not know about can never be written. Before today that refusal was
recorded as SUCCESS and the file was silently dropped for ever. Now it is correctly reported and
**retried on every pass, for ever**. Neither is right.

**The controls:** the refused file was both newly created AND read-only, so access was the first
suspect. Re-pushing a DIFFERENT file that already existed with NORMAL access succeeded - as an
OVERWRITE, because the ledger knew that one. That clears access and leaves the create/overwrite
choice as the cause.

**How it happens in practice:** the ledger is deleted or lost, someone else creates the file, or a
file is restored on the machine. None of those are exotic.

**The fix is not a patch on the retry.** Error 62 IS the evidence the ledger was missing, so it
should be fed back: on a create refused with 62, record that the file exists remotely and retry as
an overwrite. That changes how the daemon LEARNS what is on the far machine, which is the one piece
of state the whole planner rests on - worth doing deliberately rather than bolting onto the failure
path.

---

## CARVED 2026-08-18: the FA read driver ignores a refusal D100 already sends correctly

A `--pull` of a missing file hung. Not because the machine is silent and not because the refusal is
undecodable - **D100 refuses correctly on the very first step**, with a number we have had
documented and verified since 2026-08-04:

```
OpenFile reply   07F0 0002 81 00 9169 92 0005 92 0002  F2 0001  A2 002E  F2 00FF 00
                                                        ^refused ^46 = NO SUCH FILE NAME
```

`FaReadDriver` never reads selector 1, so it treats that as an ordinary reply, climbs the rest of
the ladder against a file that was never opened, collects three more refusals and then waits for a
data block that never comes.

A successful transfer carries **no selector 1 anywhere** - zero occurrences across a whole 53-block
read - so testing for it cannot produce a false failure.

**Still not explained:** the follow-on code `A2 4104` on the three operations after the refused
open. It is not 46, matches no ND error number in the Reference-Manuals, and is byte-identical for
two different missing filenames, so it is a fixed code rather than anything name-derived. It may
mean "the file is not open"; that is a guess and is recorded as one.

Full working, with the controls: `DOC\CARVE-FA-READ-REFUSAL-2026-08-18.md`.

---

## NARROWED 2026-08-18: `XRMFL` is NOT the message table being full, and it is TRANSIENT

Two measurements, taken together on D100 minutes apart, and they kill the obvious reading of this
error and downgrade it from a blocker to a nuisance.

**It is not what its name says.** A push was refused with

```
[push] <- node 100 REFUSED us: XRMFL (34) - XDTYP=0x0007 XDSCR=0xFFDE
```

and `XRMFL` reads *"Remote system message table space full"*. So the message table was measured on
the spot, from the kernel's own accounting - `X-C` -> `LIST-UTILIZATION`:

```
                         Limit  Max used  In use
Message table..........:   256       34        4
```

**Four in use out of 256.** The table it names is 98% free at the moment it refuses. Whatever the
number 34 means here, it is not that. `*FA-SERVER` was registered with 30 free connection seats at
the same time, so the seat count is eliminated too.

**It clears by itself.** The identical push, same flags, same file, same daemon options, was run
again a few minutes later with nothing changed on either side and **completed**: 60355 bytes
written. So `XRMFL` right after a bring-up is a transient state, not a wall - the earlier nights
spent restarting XMSG, cycling links and re-granting friends were most likely spent waiting.

**Also eliminated, this round:** the sequence. D100's `LIST-SYSTEMS` showed `Sequence no.
Send-receive 0 0` for system 19999 after its XMSG restart, and our own store had already reached
`100=0000` - the two sides were ALIGNED at zero and the push was still refused. A sequence
mismatch cannot explain it.

**What is still not known:** what the number actually counts, and what consumes and releases it.
The honest current advice is: **retry before diagnosing.**

**And a gap in our own tooling that this exposed:** there is no runner option for "the peer
restarted and zeroed its counters". The only thing in that area is `--resync-hard`, which is
documented as meaningful only with `--announce-restart` - and that flag is now known to poison the
conversation. Nothing in between exists.

---

## DEFECT FOUND AND FIXED 2026-08-18: the two PLANC programs disagreed about the rename

Not an unknown any more, but recorded here because of HOW it hid.

`/nick OLAV` was accepted by the client, sent, and changed nothing - the room went on using the old
name, and **neither side printed a word**. The two ends disagreed about which field carries the new
nickname:

 - the PLANC **client** put the new name in the message TEXT and the current name in the NAME field;
 - the PLANC **server** reads the new name from the NAME field.

So the server compared the arriving name against the seat's own name, found them equal, and took its
deliberate *"already your name: not news"* return - a path that prints nothing by design.

**Two self-consistent sides agreeing to do nothing.** That is why it survived a build that was
otherwise working, and why no diagnostic would have caught it: nothing failed.

**The C# pair is the arbiter and was right all along** - `ChatClient.Rename` sends
`new ChatMessage(ChatMessageKind.Rename, nickname, string.Empty)` and `ChatServer.HandleRename`
reads `message.Nickname`. The PLANC server already matched it; only the client did not.

**Why it was not caught:** the golden-bytes tests pinned the server-to-client `Renamed` and NOT the
client-to-server `Rename` - the exact direction that was wrong. That golden now exists
(`ARenameMessageCarriesTheNewNameAndAnEmptyText`). The general rule: **pin both directions, or the
unpinned one is where the two implementations drift.**

---

## SOLVED 2026-08-18: the transport blocker was `--announce-restart`, and the chat now runs

On a freshly brought-up D100, a push carrying `--announce-restart` was refused - and with the
refusal decoded the machine named it:

```
[push] <- node 100 REFUSED us: XRDDF (3) - XDTYP=0x0017 XDSCR=0xFFFD
```

`XDTYP 0x0017` is an **InitializationNak** and `XRDDF` is **"Another port already has this name"**.
Our announce claims a name D100 already holds, D100 NAKs the initialise, and the conversation is
dead. **Drop the flag and the identical push completes** - and a SECOND consecutive push completed
after it, with no bring-up and no link cycle in between.

**That retires "one conversation per link bring-up" a second time, now with a cause.** It was an
artefact of always passing `--announce-restart`. The link cycle appeared to cure it only because
the run that followed happened not to announce.

**The chat then ran on real hardware:**

```
@CHAT
CHAT: port =    10 magic=     6554946 st=     0
type=   1 rel=     0
RONNY:
RONNY: HELLO FROM A REAL ND
```

and the server saw both message paths correctly - `t= 2` routed letter with `hdr ends= 16`, `t= 1`
ordinary message with no header skip, `slot now= 1` for the seat.

**`XRMFL` is still unexplained** and is now a lower-priority curiosity rather than a blocker: it was
what a poisoned conversation reported, not the root cause. Everything eliminated for it above still
stands.


---

## DECODED 2026-08-18: the magic number is (node << 16) | (port * 128 + generation)

Measured with `XMPFP2M` on both sides at once, then checked against `X-C -> LIST-PORTS`:

| | magic | = hex | node | low 16 | port | gen |
| --- | --- | --- | --- | --- | --- | --- |
| the server's OWN | 6554960 | `0x640550` | 100 | 1360 | **10** | 80 |
| the joining client | 6555305 | `0x6406A9` | 100 | 1705 | **13** | 41 |

The server had printed `port= 10` for itself, and `LIST-PORTS` showed the client's job owning
port **13**. Both decode correctly, so the format is confirmed on two independent samples.

It also agrees with something already in our own logs: the FA trace annotates port 1092 as
`log 8/low 68`, and 8 * 128 + 68 = 1092. Same decomposition, arrived at from the other end.

**Node 100 is 0x64 in the top half**, which is why a magic number for a local port still looks
"big" - it carries the system number.

---

## THE CHAT'S REMAINING FAULT IS IN THE CLIENT'S POLL, NOT IN THE ADDRESS

**A hypothesis was written here on 2026-08-18 and is now REFUTED.** It said the welcome vanished
because `senderMagic` from `XMPFMST` on a routed letter was not an address a server can reply to,
and that `XMPSEND` with `remoteMagicNum = -1` was the documented fix. The numbers above kill it:
the server replied to node 100 port 13, and port 13 is exactly the client. **The address was
right the whole time.** Left standing here deliberately - it was a plausible reading of the guide
and it cost a cycle, so the refutation is worth more than quietly deleting it.

What the machine actually shows, with the server instrumented and both programs running:

 - The welcome is **delivered and QUEUED on the client's port**: `LIST-PORTS` showed port 13 with
   `Qhead 161641, Qlen 1`. The earlier reading of "Qlen 0 everywhere" was taken at the wrong
   moment and is what made this look like a lost message.
 - **The client does not service its port until a key is pressed.** With nothing typed it sat with
   `Qlen 1` indefinitely, and its port showed `IOW 0` - it was NOT waiting on XMSG, so it was
   blocked in the TERMINAL read. The no-wait terminal the client's design depends on is not in
   effect, so `MON1` blocks instead of answering ERROR 3.

   This is a design-level defect, not a detail: the client prints "joining" and then cannot see
   the welcome until the user types something, and it refuses to send anything before the welcome
   arrives. Nothing the user types can help, because typing is what it is waiting for.
 - After one keystroke the queue drained to 0 - **and the client printed nothing at all**, not even
   the unconditional `type= / rel=` line that `drainPort` prints for every message it takes. No
   return message reached the server either, so it was not bounced.

**That last point is the open contradiction:** the message left the queue and produced no output on
a path that has an unconditional print in it. Resolving it needs the CLIENT instrumented (its own
port and magic, printed at open), and that is exactly the file that could not be carried over - see
the FA stall entry below. The instrumented client is committed; only the transport is missing.

---

## SUPERSEDED - kept for the record: "what address does a server reply to?"

**A reply sent with status 0 never arrives.** Measured on D100 2026-08-18, with both programs
built from commit `b4533ca9` and every step printed:

```
SV: raw =  123 65 0 12 255 10 'CHAT-LOBBY' 1 5 82 79 78 78 89 0 0
SV: hdr ends=    16
SV: parse k=   1 nlen=   5 rlen=     9      <- kJoin, "RONNY", parsed correctly
SV: slot now=   0
SV: get st=     0 len=     9
SV: wri st=     0 wrt=     9
SV: snd st=     0                           <- XMSG ACCEPTED the welcome
```

and on the client, nothing. Ever. Its `drainPort` runs every pass round the loop and its own
`type= / rel=` diagnostic never fires once, so its port is empty - not holding a message it
mishandled. `X-C -> LIST-PORTS` at that moment showed **`Qlen 0` on every port in the table**, so
the welcome is not queued anywhere either. It was accepted and it went nowhere we can find.

**What is not in doubt any more:** the header skip is right (`hdr ends= 16`, derived from the
length word), the payload parses right, the nickname is right (`82 79 78 78 89` = RONNY, the R
restored), and the send itself reports OK.

**The remaining suspect is the ADDRESS.** `senderMagic` comes from `XMPFMST` on a letter that
arrived via XROUT, and whether that is a magic number a server can send BACK to has never been
checked - only assumed. The guide documents a different mechanism for exactly this case, under
`XMPSEND`:

> "A remoteMagicNum parameter of -1 will direct the message back to the port from which it was
> last sent."

**The next step is a MEASUREMENT, not a change.** Print `senderMagic` on the server next to its own
magic, and print the client's own magic from `XMPFP2M`, then compare the numbers. If the server's
idea of the client is zero, or is its own, or is anything but the client's port magic, that is the
answer and `-1` is the documented fix. The instrumentation to do this is written and committed;
the server half is already on D100 as `CHSVB:PLNC`.

**A caution for whoever applies the fix:** the member slot stores `senderMagic` for later
broadcasts, so if the join reply has to go out with `-1`, the seat still needs a real magic from
somewhere. The natural source is the first ORDINARY (`XMTNO`) message the client sends after the
welcome, where `XMPFMST` is on solid ground.

**THE MEASUREMENT WAS RUN AND THIS WHOLE SECTION IS WRONG.** `senderMagic` decoded to node 100
port 13, which is precisely the client. See the two sections above. Two things misled it: a
`LIST-PORTS` taken at the wrong moment (reporting `Qlen 0` when the message was in fact queued a
moment later), and the assumption that "sent, status 0, not seen" can only mean a bad address. The
lesson is the cheaper one - **a queue snapshot is a single instant, so take it more than once
before concluding anything from it.**

---

## OPEN: after a fresh COSMOS restart D100 refuses the connect letter with EVERY sequence tried

2026-08-18, after a clean `restart-xmsg-cosmos.ps1` run, pushes stopped getting off the ground:
four attempts at the connect letter, no answer, `RXBad` climbing 11 -> 14 in step with them.

**Everything environmental was checked on the machine and is right:**

| checked | answer |
| --- | --- |
| `LIST-LINKS` | `State Run`, sysid 19999, Lun 1362 |
| `LIST-SYSTEMS` access | `*----F` - friend, granted after COSMOS |
| `LIST-NAMES` | **`*FA-SERVER` on port 11 with 30 free connections** |
| `LIST-ROUTING-INFO` | `T: *` clean; "no route" on an adjacent system is correct |

So this is NOT the missing friend entry of 2026-08-17, and NOT an unregistered server name.

**Both ends of the sequence question were tried and both were refused:**

 - `100=0000` (matching `LIST-SYSTEMS`, which read `Send-receive 0 0` after the restart) - refused.
 - `100=0415` (carrying on from the link table, which still read `Rcv 414I`) - refused.

**And that pair is itself the finding worth keeping:** after an XMSG restart the two tables
DISAGREE about where we are. `LIST-SYSTEMS` says the system-level counters are back to `0 0`, while
`LIST-LINKS` still shows `Rcv 414I` - the link remembers the old high sequence. Zeroing our store to
match the system table re-created the documented BEHIND-SEQUENCE fault; carrying on from the link
table did not help either. Which of the two governs acceptance is not established.

**Also measured:** `--originate-from-seed` is required or the push never starts at all - without it
the runner sits at "SABM sent" and never sends a connect letter, which is the readiness gate (#50)
doing its job, not a fault.

**THE FRAME COMPARISON WAS DONE AND CLEARS THE FRAME.** A connect letter D100 ACCEPTED and one it
refused, byte for byte:

```
accepted  0902 2113 000E 0064 4E1F 0010 0022 9029 2100 86E4 ... *FA-SERVER ...
refused   0900 2113 000E 0064 4E1F 0415 0022 8C24 2100 86E4 ... *FA-SERVER ...
                                    ^^^^      ^^^^
                                    Flags1    word 6
```

Identical apart from the LAPB send number, Flags1, and word 6 - and **word 6 is correct in both**:
`0x0010 + 0x9029` and `0x0415 + 0x8C24` both equal `0x9039`, so the checksum tracks Flags1 exactly
as it should. We are not fabricating it.

**And Flags1 is not the discriminator either.** Four values were tried against the same machine:

| Flags1 | outcome |
| --- | --- |
| `0x0010` | ACCEPTED (early, when access was granted) |
| `0x0000` | refused |
| `0x0002` | refused |
| `0x0415` | refused |

`LIST-ROUTING-INFO`, read as a WHOLE line, says `T: 100, but no access to system 19999` even
straight after a bring-up that granted it - so the friend entry IS broken (see `FRIEND-SYSTEMS.md`,
including the `LIST-SYSTEMS` correction: it reported `Access *----F` throughout and was wrong).
**But granting it on a freshly restarted machine and pushing seconds later was ALSO refused**, so
the friend entry is not the whole story either.

### The elimination list, all measured on D100 the same night

| suspect | how it was eliminated |
| --- | --- |
| frame contents | byte-identical to a letter D100 ACCEPTED, bar Flags1 and word 6 |
| word 6 / checksum | `X-C -> DISABLE-CHECKSUM` for system 19999, then push - **still refused** |
| the Flags1 value | four values tried: `0x0010` accepted, `0x0000`/`0x0002`/`0x0415` refused |
| the friend entry | granted by hand on a fresh machine, pushed seconds later - still refused |
| the server name | `LIST-NAMES`: `*FA-SERVER` port 11, **30 free connections** |
| the link | `LIST-LINKS`: `State Run` throughout |
| our readiness gate | `--originate-from-seed` present; without it the push never even starts |

**`RXBad` counts the refusals exactly 1:1** - it went 48 -> 52 across a push that sent four connect
letters, and 11 -> 14 across an earlier one. So D100's XMSG RECEIVES each letter and DISCARDS it
before XROUT, silently, with everything above checking out.

### The advanced diagnostics were switched on, and they moved the question

`X-C -> SET-ADVANCED-MODE` (prompt becomes `X-C(Adv):`) then `DEBUG-MODE-ON` unlocks a second and
third tier of commands: `Debugprint-On/Off`, `Systemmode-On/Off`, `Monitorcall-On/Off`, `XMSG-Call`,
`Get-Link-State`, `Define-Alternative-Link`, `Lock-POF`, `Dump-XMSG`. **Undo with `DEBUGPRINT-OFF`
and `CLEAR-ADVANCED-MODE`.**

`DEBUGPRINT-ON` prints every XMSG call THIS program makes, as
`*- XMSG Function: n. Regs (A,D,X): ... Return status= s`. Useful for API-level work; it does NOT
show kernel handling of ARRIVING frames, so it did not explain the discard.

**`LIST-UTILIZATION` is the instrument that paid off** - it prints every kernel table with limit,
high-water mark and in-use count:

```
Friend table...........:   128        5        5      <- the grants ARE there
Receive Frame table....:    20        5        5      <- 5 stuck on an IDLE machine
Transmit Frame table...:    25        3        0
Task/Port/Message/Name/System tables all sane
Free space in Kernel...:  4273 words
```

Two things came out of it:

 - **The friend entry is genuinely present** - five entries for the five systems the bring-up
   grants, and `LIST-FRIEND-SYSTEMS` names 19999. So "no access" on the routing line does NOT mean
   the entry is missing, and the earlier conclusion that "only `LIST-ROUTING-INFO` is honest" was
   too strong. It correlates with the refusals; its meaning is not established. Corrected in
   `FRIEND-SYSTEMS.md`.
 - ~~5 receive frames in use on an idle machine looks like a leak~~ - **REFUTED BY A CONTROL.**
   D102, a separate and idle machine, reads `Receive Frame table: 20, max 5, in use 5` as well. It
   is the normal preallocated pool, not a leak. Two minutes of control saved a hunt.

### The discriminator is NOT IN THE FRAME - which is the useful negative result

A proper byte-for-byte diff of a connect letter D100 **accepted** against one it **refused** (both
64 bytes):

```
differing byte offsets: 2
  off 11: accepted 10  refused 06     <- Flags1, low byte
  off 15: accepted 29  refused 33     <- word 6, low byte (10+29 = 06+33, checksum tracks)
```

**Two bytes in sixty-four, and they are the same field twice.** Destination name, system numbers,
ports, markers, protocol, length - all identical. So Flags1 is the ONLY thing that varies, and the
checksum correctly follows it.

And Flags1 has now been swept across the whole meaningful range, all refused:

| Flags1 | relation to the peer | result |
| --- | --- | --- |
| `0x0000` | zero | refused |
| `0x0001` | **exactly what `LIST-SYSTEMS` publishes** (`Send-receive 1 0`) | refused |
| `0x0002`, `0x0006` | just above | refused |
| `0x0415` | carry on from the link table | refused |

...while earlier the SAME night a contiguous run `0x0010, 0x0011, ... 0x0413` was accepted across
four transfers (102, 118, 106 and 26+ frames each), three of which completed.

**So no value of the only variable works now, and every value worked before.** Whatever changed at
the 04:50 COSMOS restart is peer state that is not expressible in the frame and that none of
`LIST-SYSTEMS`, `LIST-NAMES`, `LIST-LINKS`, `LIST-FRIEND-SYSTEMS`, `LIST-ROUTING-INFO` or
`LIST-UTILIZATION` exposes. That is where the next session should aim - and it should stop
re-testing the frame, the checksum, the sequence and the friend entry, which are now all
eliminated by measurement.

One state change WAS observed and is unexplained: `LIST-SYSTEMS` for 19999 moved from
`State 0, Access *----F` to `State 4, Access *<==>F` (`<==>` = an established association) during
the failing attempts, without any transfer succeeding.

### It is NOT the FA server - a second, unrelated service is equally dead

A letter to **`*XFTRA`** (the remote-batch server, a different name, a different server, a proven
path - see #30) was sent down the same link and got **zero reply frames**, exactly like every
letter to `*FA-SERVER`:

```
[arb] APPEND-REMOTE-BATCH to D100(SYSTEM) ... letter body: 1B4100BEFF062A5846545241FE04443130 ...
[arb] sent. Anything the machine says back is printed below.
   ... nothing. RX data frames after the send: 0
```

**So no named server on D100 is reachable from us**, and the FA server's own health (30 free
connections, freshly restarted, `Server 1 started`) was never the question. The gate is
system-level, which is what `T: 100, but no access to system 19999` has been saying all along -
even though the friend TABLE demonstrably holds the entry.

That is the sharpest statement available tonight: **the friend table entry exists and the
system-level access check still fails**, so either the check consults something other than that
table, or the entry is not bound to the system entry it is supposed to authorise. Finding what
that check reads is the whole remaining problem.

Worth noting for the next attempt: `LIST-NAMES` showed `*FA-SERVER` with **30 of 30** free
connections after all of this. XROUT decrements that counter when it FORWARDS a letter, so an
untouched counter is independent confirmation that not one letter was ever forwarded.

### SOLVED: D100 WAS ANSWERING ALL ALONG - "remote system message table space full"

**The letters were never unanswered.** D100 replied with CONTROL datagrams carrying the BAD STATUS
bit, and the reason sat in header word 5 as a negated `XroutError`. Nothing in our stack read them,
so the driver printed "no answer to the connect letter" while the machine was naming the fault.
**39 BAD STATUS datagrams across the session**, against three "no answer" lines in a single push log.

| what D100 sent | XDTYP | word 5 | decodes to |
| --- | --- | --- | --- |
| tonight's refusals | `0x0007` = CO+UT+BA | `0xFFDE` = -34 | **`XRMFL` - "Remote system message table space full"** |
| the older chat session | `0x0017` = CO+UT+BA+IN | `0xFFFD` = -3 | `XRDDF` - "Another port already has this name" |

**The `-3` case is what proves the decoding method**, because the same machine ALSO printed
"Another port already has this name" as plain text at a terminal - the numeric decode agrees with
something read independently of the wire.

**So the refusal is NOT "no access".** It is a named RESOURCE error, and that fits the decay
measured all night: transfers worked early, then stalled progressively earlier (block 21, 19, 10,
8) and finally stopped. Message-table space tracked per remote system, filling up as aborted
conversations accumulate, is exactly that shape. `LIST-UTILIZATION` shows D100's OWN message table
(`256 / 37 / 4`) - not what it tracks for us, which is why no `List-` command ever exposed it.

**Still open:** what exactly consumes that space, and what releases it. An XMSG restart did not
visibly clear it, which is the part that does not yet fit. The initialise exchange carries
`XDSCR = 0x0001` in both directions and `XDSCR` is "SCRATCH (SIZE, DISPL, STATUS)", so a size or
credit being advertised there is the first thing to look at.

**CONFIRMED LIVE 2026-08-18.** Wired into the push driver, D100 said it on the wire immediately:

```
[push] <- node 100 REFUSED us: XRMFL (34) - XDTYP=0x0007 XDSCR=0xFFDE
```

**Why it stayed invisible, which is the lesson worth keeping.** `FaPushRun` already carried a
comment saying the peer "has been ANSWERING our connect letter all along" and that a driver which
discards replies cannot be debugged from its own log - and that display is gated on
`frame.SubHeader != null` AND on our own port. A refusal is a CONTROL datagram: seven header words,
no sub-header, no port. **The fix written to end exactly this kind of hunt skipped exactly this
kind of frame.** The new check runs before any port test and needs only a header.

We had also NAMED these frames for months without reading them: `0x07 NetworkError`,
`0x17 InitializationNak`, `0x13 ReachabilityReply`, `0x19 ReachabilityRequest` in
`SintranPacketSubtype` - a reading of the XDTYP bits that lines up exactly - with every code
already enumerated in `XroutError`. The two were never joined.

**What the answer MEANS is still open, and this is the live question:** `XRMFL` is "Remote system
message table space full", and from D100's side the remote system is US. What advertises our
capacity is unproven. The initialise exchange carries `XDSCR = 0x0001` in BOTH directions, and
`XmsgFrameBuilder.ReachabilityRequest` hard-codes that `0x0001` - a value adopted by mirroring a
capture and never understood. `XDSCR` is documented as SCRATCH (SIZE, DISPL, STATUS), so a size is
plausible and NOT established.

**Two candidate explanations tested against the readings and REJECTED, so they are not chased again:**

 - *"D100's own message table is full."* No - `LIST-UTILIZATION` shows `Message table: 256 / 37 / 4`
   against a generated maximum of `X4MES=256`. Four in use is not full. So `XRMFL` is about the
   space D100 tracks for the REMOTE system, not its own table.
 - *"The per-system `XSSCN` (Send control and counter: 0-dead, <0 init, >0 run) has gone dead."*
   No. `LIST-SYSTEMS`' `State` column read **4** during refusals at 05:00 and again at 06:05, and
   only read 0 immediately after a restart. A non-zero, running state coexisted with the refusals,
   so whatever that column is, it is not the gate.

**INFERRED and useful anyway:** the `LIST-SYSTEMS` columns line up with the XS-block fields -
`Address` is the XS-block address, `Sysno` is `XSSID`, `Link` is `XSLNK`, `Subaddr` is `XSSAD`,
`Send-receive` is `XSSSQ`/`XSRSQ`, `Access` renders the `XSSTA` bits (`F` = `5SFRI`), and
`Hops WAN/LAN` is `XSHOP`. The `State` column is most likely `XSSCN`. Marked INFERRED: this is a
reading of the values against the table, not a carve of the printing code.

**A first pass at the XROUT disassembly, and what it did and did not show.** The carved segment
`tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/077-S3XROU/` has 1.1 MB of
disassembly. Searching it for the `XRMFL` constant (42 octal) found ONE hit, and it is a false one:

```
023000  LDA -42
023001  AAA 42        <- a table STRIDE of 34 words, not an error code
023002  STA -44
```

**The emitter of `XRMFL` has NOT been located.** Grepping for the value as an immediate (`SAA 42`)
or as the negated wire form (`177736`) finds nothing usable, so it is built some other way. That
needs a proper Ghidra pass rather than grep.

**Two things the pass DID establish:**

 - The system-table walk just above corroborates the XS-block layout from EXECUTABLE code, not just
   from the table definition: it reads `,X 1` (`XSSID`) and `,X 2` (`XSLNK`) and SKIPS the entry
   when `XSLNK` is zero - which is exactly "Link to use (or 0 if not accessible)".
 - **A trap worth recording: `<segment>.symbols.txt` is a VALUE-to-NAMES index, not a list of the
   symbols a segment references.** Its `42B` line names every symbol in `SYMBOL-1-LIST` whose value
   is 42 octal, `XRMFL` among them. It says nothing about whether this segment uses any of them.
   Reading it the other way would have "proved" that XROUT issues the refusal, which the
   disassembly does not show.

**A SECOND-MACHINE CONTROL, started and not completed - and what it did prove.** The sharpest
remaining question is whether `XRMFL` is D100-specific state or something our stack provokes. The
control for that is the same push against a different ND. D102 has a free HDLC listener on 10370
(`device add HDLC 1 --listen=10370`), so:

 - D102 had NO HDLC link at all - its only link was `Lun 7, Sysid 9800`, a VIRTUAL system (the POF
   tables give `XSVSL=9800, XSVSH=9999` as the virtual range). `START-LINK,1360,,,-1,,` on D102
   brought one up on that controller.
 - **Our stack then established a clean LAPB link to a machine that has never seen us**, and both
   sides exchanged supervisory frames normally. So the link layer and our framing are healthy
   against a fresh, independent peer - that is real evidence the lower layers are not the fault.
 - **The push never sent a connect letter**, because originating to a peer we have not heard from
   first needs a remembered LINK SEED and `xmsg-link-seed.state` has no entry for D102. Completing
   the control also needs D19999 defined on D102 and granted access there.

D102 was left exactly as found: `STOP-LINK 1360`, back to its single `Lun 7` link. Only the
`Max used` high-water mark records the visit.

**The control was then run with D102 fully configured, and it stopped on OUR side.** D102 already
knew `D19999` (`DEF-REMOTE` answered "Another port already has this name" - which is `XRDDF`, the
very code decoded off the wire earlier, so the numeric decode is confirmed a second time and from a
second machine). `START-LINK,1360` and `DEFINE-FRIEND-SYSTEM 19999` both answered `Ok`. Then:

```
[link] hdlc:127.0.0.1:10370 status Starting -> Active (LAPB connected)
[link] announcing our XMSG restart to node 102 (ReachabilityRequest...)
[RX] 102->19999 sub=ReachabilityReply proto=144 f1=0xFFFF info=211300134E1F0066FFFF00019053
```

**D102 answered our ReachabilityRequest with a ReachabilityReply - and the push still never sent a
connect letter.** That is not a protocol fact about D102; it is our own readiness path. Against
D100 the push DOES originate, because `xmsg-link-seed.state` holds a seed for node 100 (`0x5B`).
There is no seed for 102, so `--originate-from-seed` has nothing to originate from.

**So "COLD START WORKS" is narrower than its name.** It works for a peer we already hold a seed
for. A genuinely new peer that answers our reachability probe still cannot be pushed to. That is a
real limitation in our stack, found only because the control was tried against a second machine.

**To finish this control:** either learn/derive a seed for the new peer from its ReachabilityReply
(it is right there in the frame), or let the peer address us first so the seed is learned. Then a
DIFFERENT error from D102 proves our frames are fine and `XRMFL` is D100 state; the SAME error
proves we provoke it.

D102 restored both times: `REMOVE-FRIEND-SYSTEM 19999`, `STOP-LINK 1360`, back to its single
`Lun 7` virtual link.

### ONE CONVERSATION PER LINK BRING-UP IS REAL - and a link cycle is the cure (on a healthy peer)

**Reproduced on a FRESH machine, so it is not D100 damage.** Immediately after the 56062-byte push
to D102 completed, the very next push to D102 was refused - "no answer to the connect letter" x4 -
and `LIST-LINKS` on D102 read:

```
 2 152262  Run 19999 402I 43RR 1360  10/Off  0  0  5/4/11
```

`TXData/Retry/RXBad = 5/4/11` - **the identical signature D100 showed at 03:48**, on a machine that
had been fresh twenty minutes earlier and had had exactly ONE conversation.

**`STOP-LINK 1360` + `START-LINK,1360,,,-1,,` on D102 cured it instantly**: the same push then ran
to `CloseFile` and the file landed byte-exact (28407 bytes, a BINARY, odd length - so odd files
transfer fine and the word-align rule is about frame bodies, not the file).

**This revives the retired "ONE CLIENT CONVERSATION PER BRING-UP"** and sharpens it: the unit is the
LINK, and the cure is a link cycle - not an XMSG restart, not a COSMOS restart, not friend grants,
all of which we spent the night on.

**AND THE SAME CURE DOES NOT WORK ON D100.** After `STOP-LINK 1362` + `START-LINK` on D100, the push
was refused again with the decoded `XRMFL`. So:

 - the PHENOMENON (one conversation, then refusal) is universal - measured on a fresh peer;
 - the PERSISTENCE across a link cycle is D100-specific - something on D100 holds the state that
   D102 releases.

That is the remaining question, and it is much narrower than "why does D100 refuse us".

**The XS-block was the obvious candidate and it is ELIMINATED.** A link cycle rebuilds the XL-block
(per link) but leaves the XS-block (per system), and D102's XS-block for us was minutes old while
D100's had been there all night. So on D100, with the link stopped:

```
X-C: STOP-LINK / 1362        Ok
X-C: REMOVE-SYSTEM / 100 / 19999   Ok          <- the XS-block for 19999 removed
X-C: START-LINK,1362,,,-1,,        Ok
X-C: DEFINE-FRIEND-SYSTEM 19999    Ok
```

and the next push was refused with the decoded `XRMFL` again. **Removing D100's system entry does
not clear it.**

**Two commands learned in the process:**

 - **`REMOVE-SYSTEM` only works with the LINK STOPPED.** With link 1362 up it answers
   `Illegal system number parameter`; with the link stopped the identical command answers `Ok`.
   That contradicts the note that recorded it simply answering Ok, and explains that earlier failure.
 - **The NAME survives `REMOVE-SYSTEM`**, exactly as the standing rule says: the following
   `DEF-REMOTE` answers `Another port already has this name`. So the system entry and the name entry
   are removed by different means, and only the system entry can be removed at all.

**Eliminated on D100 so far, by measurement:** the frame, the checksum, the Flags1 sequence, the
friend entry, the server name, the link (cycled), the XS-block (removed), an XMSG restart and a full
COSMOS restart. What holds `XRMFL` across all of that is still unknown - but the list of places left
to look is now short.

 **Correcting
the earlier entry above:** "XRMFL is D100-specific state" was half right. The refusal is universal;
only its stickiness is D100's.

### THE LINK "SEED" IS DERIVABLE, NOT SECRET - so the seed store caches something computable

Chasing why we cannot originate to a new peer led to the seed itself. The learn rule is:

```csharp
byte seed = XmsgEnvelope.LearnSeed(Flags1, Header.Counter, Flags2);
//  LearnSeed = (byte)(counter + flags1 + (flags2 & 0xFF))
```

and `Header.Counter` is the OBSOLETE property - offset 13, which we proved is the **checksum's low
byte**. So the "seed" is `checksumLow + flags1Low + flags2Low`, and because word 6 is a checksum
over the whole header, that sum is exactly the contribution of the header words that do NOT vary.

**Verified against two real D100 frames, one accepted and one refused:**

| frame | Flags1 | Flags2 | checksum | seed |
| --- | --- | --- | --- | --- |
| accepted connect letter | `0x0010` | `0x0022` | `0x9029` | **0x5B** |
| refused connect letter | `0x0415` | `0x0022` | `0x8C24` | **0x5B** |

`0x5B` is exactly what `xmsg-link-seed.state` holds for node 100 - reproduced from the wire, twice,
at different Flags1. It is invariant across Flags1 precisely because the checksum moves to cancel it.

**Consequence:** the seed is a function of the node pair and the frame shape, so it can be COMPUTED
for a peer we have never met. For D102 the only differing header word is the destination
(`0x0064` -> `0x0066`), which moves the ones-complement checksum by -2, giving a predicted seed of
`0x59`. `--originate-from-seed` depending on a REMEMBERED value is therefore unnecessary in
principle, and is why a genuinely new peer cannot be pushed to. This is a live fossil of the
superseded channel-and-counter model, which the word 6 note already warned had left a "per-link
seed" behind.

**PROVED END TO END, 2026-08-18.** The derived seed `0x59` - computed, never observed on any wire -
was written into the store and the same push run against D102:

```
[seq] node 102: link opened from the REMEMBERED seed 0x59, starting Flags1 0x0000
[push] SendConnectLetter / ReserveFileEntry / OpenFile / SetBlockSize
[push] 28 x WriteFile + SendData ... CloseFile
```

and on D102: `FILE 73 : (PACK-ONE:SYSTEM)CTRL5:PLNC;1 ... 28 PAGES, 56062 BYTES IN FILE` -
**byte-exact with the source.** Zero refusals, 125 replies from D102.

**Two things this settles.**

 - **The seed really is derivable.** A value computed from the checksum formula, for a peer we had
   never exchanged a data frame with, opened the link on the first attempt. The seed store caches a
   computable quantity; it does not hold a secret.
 - **OUR FRAMES ARE CORRECT.** A fresh ND accepted the identical
   connect letter and carried a 56 KB file to completion. Nothing in our stack provokes the refusal.
   Every remaining question about `XRMFL` belongs to D100's own tables.

D102 restored: file deleted, `REMOVE-FRIEND-SYSTEM 19999`, `STOP-LINK 1360`.

**Superseded note, kept for the record.** The first attempt at this control did not run: the ESC that
opens a fresh terminal broke into an ALREADY LOGGED IN session and logged it out, so the
`START-LINK` / `DEFINE-FRIEND-SYSTEM` lines never reached an `X-C:` prompt and the link was never
up. The SABM timeout that followed was my own setup error, not a fact about D102. **Check that a
command landed before believing the run that depends on it.** D102 verified restored afterwards:
single `Lun 7` link, `* No friend exists *`.

**Deliberately not tried:** changing that constant to see what happens. Fabricating a protocol
value is how the word-6 channel-and-counter model survived for months. Find what the field means
first - the XMSG initialise handling in the NPL source is where to look.

### CARVED FROM THE L03 KERNEL SOURCE: what the system entry actually holds

`SINTRAN/XMSG/XMSG-POFTABS-L03.SYMB` - the POF tables of the **L03 kernel we drive**, not a J-era
guess - defines one **XS-block per known remote system**:

```
DISP XS5LN=0
  INTEGER XSCHN    % Chain word thru active or free blocks
  INTEGER XSSID    % System id (0 if free)
  INTEGER XSLNK    % Link to use (or 0 if NOT ACCESSIBLE)
  INTEGER XSSAD    % Secondary address (if via gateway)
  INTEGER XSSSQ    % Send sequence number
  INTEGER XSSCN    % Send control and counter (0-dead, <0 init, >0 run)
  INTEGER XSRSQ    % Receive sequence number
  INTEGER XSSTA    % System status word
  INTEGER XSHOP    % No of hops to this guy

% Bits in XSSTA
% Bits 0-7 are reserved for 'Option-Version-Protocol' (ie. remote X5VRS).
SYMBOL 5SFRI=10  % Indicates that this system is defined as friend
SYMBOL 5SLEC=11  % Checksum on datagrams enabled in local system
SYMBOL 5SREC=12  % Checksum on datagrams enabled in remote system
                 % If both 5SLEC and 5SREC are set, a checksum is provided
                 % (and checked) on the transport layer header and data.
SYMBOL 5SDCE=13  % Set if checksum error detected on datagram
```

Four facts fall straight out of this, and they reshape the hunt:

1. **The friend flag is a BIT (`5SFRI`, bit 10) inside the per-system `XSSTA` word** - it lives in
   the SYSTEM entry, not in a friend table consulted at check time. So "the friend table has 5
   entries" and "this system is flagged friend" are two different things that can disagree, which
   is exactly the contradiction we measured. Which of `LIST-FRIEND-SYSTEMS` / `LIST-SYSTEMS` reads
   the table and which reads the bit is NOT yet established - that is the next carve.
2. **`XSLNK` is documented as "Link to use (or 0 if not accessible)"** - the source's own phrase
   "not accessible" is the wording the tools print as "no access". So that message may be about
   the LINK BINDING in the system entry, not about privilege at all.
3. **`5SLEC` and `5SREC` must BOTH be set for a checksum to be applied or checked.** That is why
   `DISABLE-CHECKSUM` at one end produced no change and cannot on its own prove the checksum
   innocent - though the byte diff already did.
4. **Bits 0-7 of `XSSTA` hold the REMOTE's Option-Version-Protocol (`X5VRS`)** - D100 records OUR
   protocol version inside its system entry for us. **This is the best remaining candidate for the
   invisible state**: it is peer state, it is learned from our frames, it survives in the XS-block,
   and NO `List-` command prints it. If D100 latched a wrong version for 19999, it would refuse
   our datagrams while every table we can read still looks correct.

**Do not "fix" this with `DEFINE-SYSTEM-ROUTE`.** Its prompts are `XROUT system?` / `System?` /
`Via system?`, and giving an ADJACENT system a route is documented as harmful - it once stopped
D103 reaching us. It was backed out of unused.

---

## OPEN: the FA push stalls part-way, and an FA server restart only sometimes clears it

Measured repeatedly 2026-08-18. A push climbs the ladder normally - `SendConnectLetter`,
`ReserveFileEntry`, `OpenFile`, `SetBlockSize`, then `WriteFile`/`SendData` cycles - and then stops
mid-file. D100 keeps short-acking (`07A2...`) every data frame it got; our side simply stops
sending the next block. No error, either end.

Nine pushes in one evening. Three completed (22, 23 and 26 blocks); the rest stalled at roughly
blocks 21, 19, 10 and 8. **Size is not the discriminator** - 26 blocks completed once and stalled
twice, and the SAME 53165-byte file stalled at 21, then 19, then 10 on three consecutive tries.

**The stall point falls with each attempt, which is the real shape of it:** the FA server degrades
as aborted sessions accumulate, so each restart buys less than the one before. After enough of them
the machine stopped answering the connect letter at all - see the entry above.

`ABORT FSART` + `RT FSART` + `FS-ADMINISTRATOR` -> `SELECT-FSA` -> `START-SERVER 1` (which answers
`No of FACs attached: 30`) cleared it the first two times and then stopped helping: the run right
after a clean restart stalled earlier than the one before it. So "restart the FA server" is a
workaround with a decay, not a cure, and the real fault is not identified.

Note also that a stalled push leaves the target file OPEN, and SINTRAN truncates an `OPEN W` file
at CLOSE - so the half-pushed name still holds its OLD contents and will COMPILE as the old code.
Check `FILE-STATISTICS` byte count against the source before believing any build.

---

## DECODED 2026-08-18 FROM THE WIRE: the XSLET letter header sits in front of the payload

The server dumped every byte of the arriving connect. Twenty-five bytes, decimal:

```
123  65   0  12  255  10 | 67 72 65 84 45 76 79 66 66 89 | 1  5  79 78 78 89  0  0  0
```

Split:

| Offset | Bytes | Meaning |
| --- | --- | --- |
| 0 | `123` | the SERIAL NUMBER the sender passed to `XMPBLET` - ours is literally 123 |
| 1 | `65` | service code (`XSLET`, send letter) |
| 2-3 | `0 12` | 12 - length of the rest of the letter |
| 4 | `255` | parameter type: string |
| 5 | `10` | parameter length: 10 characters |
| 6-15 | `67 72 65 84 45 76 79 66 66 89` | `CHAT-LOBBY`, the destination port name |
| 16.. | `1 5 79 78 78 89 ...` | OUR payload: kind 1 = kJoin, name length 5, then the name |

**So the payload starts at 6 + (length of the port name), not at 0.** CHATSV has been reading byte
0 - the serial number - as its message kind, which is why the join never matched anything. The
offset is NOT a constant: it moves with the port name, so it has to be computed from the length byte
at offset 5, or the parse has to walk the parameter list properly.

This also retires the earlier reading that "XROUT withholds the letter body on a connection port".
It does not. The body is there; we were looking at the header.

**A SECOND, SEPARATE DEFECT IS VISIBLE IN THE SAME DUMP.** The name length says 5 and the bytes are
`79 78 78 89` = `ONNY`. The nickname is `RONNY`. **The leading `R` is missing** - the name is one
byte short and shifted, so the client's payload builder writes the name one byte earlier than its
own length field claims. Nothing on either side could have shown this except the raw bytes.

---

## SETTLED 2026-08-18: XMPFRCV answers XMOK, and a connection port is told about a CONNECT first

Two things measured on D100 with the server printing the status of every receive, outside any test.

### 1. A successful XMPFRCV returns XMOK (0). The "message type range" reading was WRONG.

    SV: rcv st=     0 t=   2 len=     4

`returnStatus` is **0**. CHATSV had been testing `returnStatus >= XMTNO AND returnStatus <= XMTPS`
(1..6), defended by a comment claiming 0 "is NOT a message type, so comparing to it here discards
every message". It is the other way round: that test discarded EVERY message, and because every
diagnostic in the program sat inside it, the server looked as if it had never woken.

Appendix G's SERVER in `ND-60.164.3 EN COSMOS Programmer Guide` had it right all along - it tests
`IF RETURNSTATUS >< XMOK` and only then looks at the **msgType out-parameter** to check for XMROU.
**The message type belongs in msgType, not in the status.**

### 2. What arrives on a CONNECTION port first is a 4-byte connect, not the sender's payload

    SV: rcv st=     0 t=   2 len=     4
    SV: kind=   0 read=     4

`msgType` = 2, length = **4 bytes**, and byte 0 is **0** - not our `kJoin` = 1. The client had built
an XSLET header with its join payload appended and sent it with `xmprout`, so if the letter itself
had been delivered the body would be far longer than 4 bytes and would start with 1.

This lines up with the working FORTRAN sample `SERVER.SYMB` in
`E:\Dev\Emulators\ND-Emulators\ND110-02-07-2021\(XMSG)\`, where "a client connected to my server
port" is its OWN event carrying no user data, separate from any message. The Programmer Guide's
connection-port text says the server then "sends a (positive) reply to the requester, and thereby
gives away its own magic number", after which the client uses `XMPFMST` to pick that magic up and
talks to the server directly.

**STILL OPEN - and this is the next thing to measure, not to reason about:**
 - What are those 4 bytes? The server prints only `inBuf(0)`. Dump all four.
 - Whether XROUT genuinely withholds the letter body on a connection port, or whether our
   `xmpfwri` append is not landing. The 4-byte length points at the former, but that is inference,
   not measurement.
 - An unanswered connect is re-presented forever: the server printed the same 4-byte arrival in a
   tight loop until it was stopped. So the connect MUST be answered.

---

## THE LIVE PATH WORKS. Read this before anything below it.

Everything under "the live blocker" further down is HISTORY, and most of it is refuted. The file
grew a section per hypothesis on 2026-08-17 and they are kept only so nobody re-runs them.

### LIST-PORTS names the OWNER TASK of every port - use it to find who holds a name

`X-C` -> `LIST-PORTS` (RETURN at "Record address?") prints the whole port table with the owning
task, and it is the cheapest way to answer "which program is holding this name":

```
 No  Address  Owner-task     Qhead  Qlen  Chain WAK IOW CURMES
  1  160014 152554 XROUT        0    0      0    0   1       0
  6  160057 153026 FSART        0    0      0    0   0       0
  8  160075 152616 BAK03        0    0      0    0   1  161636
  9  160104 153026 FSART        0    0      6    0   0       0
 11  160122 153174 BAK12        0    0      0    0   0       0
 12  160131 153026 FSART        0    0      9    0   0       0
```

**`BAK03` is background job 3** - that is `CHATSV` holding `CHAT-LOBBY` on port 8. `BAK12` is
the `CHAT` client. So a background program's job number is recoverable even when `WHO-IS-ON`
lists five terminals all owned by SYSTEM and cannot say which is which.

**Two columns worth reading**, though the readings below are NOT settled:

 - `IOW 1` appears on port 8 and on XROUT's port 1. The obvious reading is "the owner is in an
   IO wait on this port", which is what a server blocked in `xmpfrcv` with `2**XFWTF` should
   look like - i.e. normal, not stuck. `WAK 1` appears instead on TADAD, COSPO, XFTRAD and
   Driver, which are the wake-on-arrival tasks.
 - `CURMES 161636` on port 8 says the port holds a CURRENT MESSAGE. Whether that is a leftover
   pointer or a message taken and not finished with is NOT established, and it matters: the
   server releases what it receives before building any reply.
 - `Chain` is non-zero on the FSART ports (6 -> 9 -> 12), which is what a connection port with
   live connections looks like. **Port 8's chain is 0** - so no connection appears to have been
   chained onto the room, even though three letters were forwarded to it and three seats spent.

That last line is the sharpest clue yet about the join, and it is exactly what the instrumented
server was built to settle. Do not theorise past it.


### OPEN: the join reaches the room's port and nothing comes back

Both halves of the SINTRAN-native chat compile clean and run on D100. What does not yet work is
the join, and the evidence narrows it to one gap.

**What is proved:**

 - `CHATSV` runs and opens its named connection port. `LIST-NAMES 100` shows
   `100  8  8  CHAT-LOBBY.` - port 8, eight free seats.
 - `CHAT` runs, and `xmprout` returns OK for the join letter - the client prints nothing, and
   every failure path calls `showStatus`.
 - **THE LETTERS ARRIVE.** After three join attempts the same listing shows the free seat count
   at **5**. XROUT spends a seat to forward a letter, so 8 -> 5 is three letters delivered to the
   room's port. This is the measurement that matters: the name resolves, the route works, the
   letter is forwarded.

**What does not happen:** nothing comes back. The client's drain loop prints a `type=`/`rel=`
line for every message it receives and printed none, so no reply ever reached it. The server's
terminal stays blank, but that proves little - it only prints on failure.

**So the gap is between the letter arriving at the port and a reply reaching the client**, and
the cheapest next step is to make the SERVER print on every arrival. That single line separates
the two remaining possibilities, which no amount of reading will:

 - the server never wakes from `xmpfrcv` - in which case the wait flag or the port is wrong;
 - or it wakes, handles, replies, and the reply is lost - in which case `xmpfsnd` and the magic
   number from `xmpfmst` are where to look.

Guessing between them is what this register exists to prevent.


### REMOVE-SYSTEM does NOT remove the NAME, and there is no operator command that does

Measured 2026-08-17 while trying to rebuild the route to 19999 by hand.

    X-C: REMOVE-SYSTEM
    XROUT system? 100
    System? 19999
    Ok

answers Ok, and a second attempt then answers "Illegal system number parameter" - so the SYSTEM
entry really is gone. But `LIST-NAMES 100` still shows

     19999     0             D19999.

and `DEF-REMOTE` therefore refuses to recreate it: "Another port already has this name". The
name entry outlives the system entry.

**And `DEF-REMOTE` IS `Define-Remote-Name`** - the abbreviation the operator guide's examples
use. `HELP` lists no separate clear-name command, so from X-COMM there appears to be no way to
release a remote NAME once it exists. The API has one - `XMPCLNM`, "clear name and close" - but
that is a call, not a command.

**The state this leaves is worse than the one it was meant to fix:** name present, system
absent, and nothing that can reconcile them from the operator interface. The way out is the full
restart (`tools/restart-xmsg-cosmos.ps1`), which rebuilds both tables from nothing.

**So do not try to repair the routing tables by hand.** Two rounds of it produced no route and
one degraded machine. If `LIST-ROUTING-INFO` says "no route", restart rather than reach for
REMOVE-SYSTEM.


### SETTLED: X-C HELP lists every command, and the first parameter is the XROUT to ASK

Two things that between them explain a run of "Remote system is not accessible" answers and a
round of guessing at command names.

**`HELP` lists the whole command set.** It prompts `Commands?` - press RETURN for all of them -
and pages with `More?`. The `?` command is NOT this: it lists only what is NEW in this release,
which is why `Remove-System` never appeared and three invented names were tried instead
(`CLEAR-REMOTE`, `CLEAR-NAME`, `REMOVE-REMOTE`, all "Command not recognised"). The full list
includes `Remove-System`, `Remove-Friend-System`, `Define-Remote-Name`, `Define-Local-System`,
`List-Friend-Systems`, `List-Servers`, `Get-System-Name-or-Number`, `Dump-XMSG` and the restart
and checksum commands.

**THE FIRST PARAMETER OF THESE COMMANDS IS THE XROUT TO TALK TO, NOT THE SUBJECT.** This is the
trap. Typed inline, the number is eaten by the FIRST prompt:

    X-C: REMOVE-SYSTEM 19999
    System? 19999
    XMSG Kernel error: Remote system is not accessible

- that asked XROUT ON 19999 to remove 19999, and 19999 is exactly what cannot be reached. Answer
the prompts instead and name the LOCAL system first, and it works:

    X-C: REMOVE-SYSTEM
    XROUT system? 100
    System? 19999
    Ok

The same applies to `DEF-REMOTE`, which is why `DEF-REMOTE,,D19999 19999` fails on an unreachable
peer while the prompted form with `XROUT system? 100` answers Ok.

**THE COMMANDS ARE NOT UNIFORM ABOUT THIS.** `DEFINE-FRIEND-SYSTEM 19999` inline answers `Ok`,
so ITS first parameter is the system itself. Read the prompts before trusting an inline
argument; the shape is per command.

**Still not solved: the route.** With the link Active, `REMOVE-SYSTEM` then
`DEFINE-FRIEND-SYSTEM` still leaves `T: *, but no route to system 19999`. `DEF-REMOTE` cannot be
re-run in between because the name is then held by the live link's own port - "Another port
already has this name" - which is a further clue about who owns `D19999` while a link is up.


### RETRACTED: the missing T: route is EXPECTED - 19999 is an ADJACENT system

Two of the three lines `LIST-ROUTING-INFO` prints are now understood and one is not.

**Settled:** `DEF-REMOTE` gives the system a NAME in the local table, `DEFINE-FRIEND-SYSTEM`
gives it ACCESS, an XMSG restart clears the friend entries, and `DEF-REMOTE` CLEARS a friend
entry that already exists - so the order is name first, access second. The bring-up script does
both passes now and the "no access to system 19999" line is gone for good.

**AND THE `T:` LINE IS NOT A FAULT AT ALL.** `ND-30.025.02 COSMOS Operator Guide` section
2.5.4, verbatim:

> "When defining the route to remote systems, **it is not necessary to define the route to the
> adjacent system(s)**. By adjacent system we mean the system to which the local system has a
> direct link... physically connected to the local system with an HDLC or Megalink cable."

and again at page 129: "It is not necessary to define the route to an adjacent system."

19999 IS adjacent - it is on the far end of D100's own HDLC cable. So "no route to system 19999"
on the `T:` line is the CORRECT reading for this topology, not a defect, and
`DEFINE-SYSTEM-ROUTE` refusing it with "Illegal system number parameter" is the machine agreeing.
`[[xmsg-adjacent-system-needs-no-route]]` recorded this on 2026-08-08 - giving an adjacent
system a route is not merely redundant, it is WRONG, and doing so is what once kept D103 from
reaching us.

**The healthy indicator is the `A:` line**, and we have it: `A: *->19999`. The same note records
D103 working with exactly this shape while still sending INNAK.

**Two rounds were spent chasing this before the note was re-read.** The repo's own rule -
[[grep-reference-manuals-first]] - would have cost one grep.

**What IS still unexplained** is therefore narrower than it looked: with name, access, link and
`A:` path all correct, D100 still discards every connect letter (RXBad +1 each). The remaining
suspects are the datagram sequence and the InitializationNak that precedes it - not the routing
tables. Superseded text follows, kept because the measurements in it are real:

    19999  L: *->19999
           T: *, but no route to system 19999
           A: *->19999, but no route to system 19999

and every transfer is refused with no reply.

**It HAS been seen to clear.** Earlier the same day, after a by-hand `DEF-REMOTE` that actually
executed - naming the local XROUT explicitly at the "XROUT system?" prompt - followed by
`DEFINE-FRIEND-SYSTEM`, `T:` became a bare `*` and 42368 bytes went across immediately. Trying
to repeat that afterwards fails at the first step: `DEF-REMOTE` answers "Another port already
has this name", because the restart script has already defined it.

**AND A CLEAN RESTART REPRODUCES THE FAULT, which removes a suspect.** After a full
`restart-xmsg-cosmos.ps1` - names defined, friend entries granted, FA server up, nothing touched
by hand afterwards - `LIST-ROUTING-INFO` reads exactly the same:

    19999  L: *->19999
           T: *, but no route to system 19999
           A: *->19999, but no route to system 19999

So the missing route is NOT damage from hand-editing the tables. It is the state the documented
bring-up leaves the machine in, which is what the 2026-08-07 note meant by "the full bring-up
ran and did NOT fix it". Whatever made it work once was something else, and it is still not
identified.

**So the cheapest experiment is:** clear the remote name, then `DEF-REMOTE` it again WHILE THE
LINK IS UP, then `DEFINE-FRIEND-SYSTEM`. The missing piece is the clear command.
`CLEAR-REMOTE`, `CLEAR-NAME` and `REMOVE-REMOTE` are all "Command not recognised" - those were
guesses and they are recorded as guesses. The X-C command list is worth reading properly, and
`XMPCLNM` (clear name and close) is the API-level call the operator command presumably wraps.

**What is NOT the cause, measured:** the link is healthy throughout - `LIST-LINKS` shows
`State Run`, `Sysid 19999`, thousands of frames received - and `DEFINE-SYSTEM-ROUTE` refuses a
direct route to the system itself with "Illegal system number parameter".


### OPEN: D100 counts every connect letter of ours as RXBad - 1:1, measured

**Measured on D100 2026-08-17.** `X-C` -> `LIST-LINKS` prints a per-link
`TXData/Retry/RXBad` triple, and RXBad moves by EXACTLY the number of connect letters we send:

    before a push attempt   22/8/21
    one attempt, 4 letters
    after                   27/8/25          RXBad +4

The link itself is healthy in the same listing - `State Run`, `Sysid 19999`, thousands of
frames received - so the frames arrive and are then classified as bad. That is why the push
answers "node 100 answered none of 4 connect letters" with no XRUNN and no reply of any kind:
whatever rejects them does so before XROUT is asked, so there is no error to send back.

**This is a usable oracle and that is the point of writing it down.** Until now a refused
transfer looked exactly like a lost one. Now it is countable: send N letters, read RXBad, and
the machine tells you whether it saw them and threw them away.

**WHAT IT IS NOT YET.** Two readings fit and NEITHER is established:

 - the frame is malformed as far as D100's link layer is concerned. Against this: header word 6
   has been a real carved checksum since 2026-08-05, verified 3595/3595 frames across every
   subtype and both directions, and the same builder is used here.
 - RXBad counts "received and could not be delivered", in which case it is a SYMPTOM of the
   missing route (`LIST-ROUTING-INFO` still says `T: *, but no route to system 19999`) rather
   than a cause.

Distinguishing them is one experiment: get the route back and see whether RXBad stops moving.
`DISABLE-CHECKSUM` cannot be used to test the first reading from here - it needs to reach 19999
and answers "Remote system is not accessible".

**What IS settled from the same session:** `DEF-REMOTE` does not grant access, an XMSG restart
clears the friend entries, and `DEFINE-FRIEND-SYSTEM` removes the "no access to system 19999"
line from `LIST-ROUTING-INFO`. That was a real fault and it is fixed in
`tools/restart-xmsg-cosmos.ps1`. It did not, on its own, make transfers work.


### SOLVED 2026-08-17: the second process was BEHIND-SEQUENCE, because `--resync-hard` zeroed a counter D100 keeps

**The machine publishes the answer.** `X-C` -> `LIST-SYSTEMS` -> XROUT system `100` -> system `19999`
prints the datagram sequence pair D100 holds for us:

```
                                       Sequence no.   Timeout             Hops
Address  Sysno  State    Link Subaddr  Send-receive  Send-recv  Access  WAN/LAN
1211535  19999      4  152164       0    64      65   200  150  *----P    0/1
```

The columns are DECIMAL. `receive 65` = `0x0041` is **the next Flags1 D100 expects from us**, and it
equalled `100=0041` in our own `xmsg-sequence.state` to the digit. Verified a second time after a
transfer: D100 read `send 122 receive 124` while our file held `100=007C` = 124. **So this command is
an ORACLE for the counter - stop guessing it, read it.**

**What was actually wrong.** `--resync-hard` throws that stored value away and originates at
`0x0000`. `IResponderSequenceStore` already spells out the consequence: reset to 0 while the peer's
XSRSQ has advanced and our frames are behind-sequence and *silently dropped*. That silence is the
whole illusion - no error, no refusal, just a peer that appears not to answer.

**Measured, not reasoned:**

 - With `--request-link --announce-restart --resync-hard`, D100's columns **stayed at 64/65** and it
   ignored every `0x0000` frame. So D100 does NOT reset on our announce, and does NOT reset on a
   reachability request either. The earlier claim that it did is REFUTED.
 - Its one reply was an `InitializationNak`, which is normal and is not the refusal.
 - Running **without** `--resync-hard`, carrying on from the stored `0x0041`, pulled `BIGPSH3:TXT`
   first time: 20400 bytes, SHA256 `d523bddc...8d5aebda`, identical to every earlier verified pull.
   That was the FOURTH process against one un-restarted D100 - so "one conversation per bring-up" is
   dead as a rule.

**The correct recipe is therefore: do NOT reset. Carry on from `xmsg-sequence.state`** - which is
exactly why that file must never be deleted. `--resync-hard` does not recover a drifted sequence
against D100; it manufactures one.

**Re-proved on the corrected recipe, 2026-08-17**, three transfers in one sitting against one
un-restarted D100, counter simply carried on from the persisted value (`0x007C` -> `0x00B7`):

 - pull `BIGPSH3:TXT` - 20400 bytes, SHA256 `d523bddc...8d5aebda`, identical to the pushed original;
 - push under a quoted new name - `FILE 76 : (PACK-ONE:SYSTEM)SEQFIX:TXT;1` on the machine;
 - the folder-watch daemon - `FILE 99 : (PACK-ONE:SYSTEM)SEQOK:TXT;1` on the machine.

### APPEND-REMOTE-BATCH works on the HDLC path too - and D100 has NO BATCH PROCESSOR

`--append-batch` used to be refused here as "wired only to the Ethernet seam". That was missing
plumbing, not a protocol limit: an XROUT letter to `*XFTRA` does not care which transport carries
it. Wired into the seam path and run live, from cold via the remembered seed:

```
[arb] APPEND-REMOTE-BATCH to D100(SYSTEM): input SEQOK:TXT, output ARBOUT:SYMB
[arb] letter body: 1B410042FF062A5846545241FE0444313030F40653595354454D0D020000
                   F8095345514F4B3A54585400F70453594D420A0204000B020003
                   F00B4152424F55543A53594D4200
[arb] answer-ish frame from node 100: subtype=Data Flags1=0x012C Flags2=0x0046
```

D100's reply echoes our serial `1B`, then `SEQOK` ... `TXT`, then `0063` = **99** - which is exactly
`FILE 99 : (PACK-ONE:SYSTEM)SEQOK:TXT`. So the letter arrived, the file name was parsed, and the
machine answered with that file's real number.

**And it RAN, end to end.** `@BATCH` starts a processor (`BATCH NUMBER = 2`), `@ABORT-BATCH <n>`
stops it; both are SYSTEM commands. The full chain works and every leg is ours:

 1. the sync daemon carried the job file over **from cold** (`BJOB:SYMB`, 61 bytes);
 2. our `APPEND-REMOTE-BATCH` letter over HDLC queued it;
 3. D100's `BCH02` executed it;
 4. our pull client fetched the output back (`BJOUT:SYMB`, 2497 bytes).

Decoded output, evidence in `DOC/captures/BATCH-RAN-2026-08-17/`:

```
 --- SINTRAN III BATCH PROCESSOR ---
 USER SYSTEM ENTERED AT 10.45.01     17 AUGUST   1998
MAXIMUM TIME IS     10 MINUTES
@LIST-FILES SEQOK,,
FILE 99 : (PACK-ONE:SYSTEM)SEQOK:TXT;1
@LIST-FILES PROOF,,
FILE 102 : (PACK-ONE:SYSTEM)PROOF:TXT;1
 BATCH USER LOGGED OUT AT 10.45.02     17 AUGUST   1998
```

**Three things the manual told us and the machine then confirmed:**

 - a batch job's first line is `@ENTER <user>,<password>,<project>,<max time>`, with **two commas**
   when there is no password, and the job is **terminated by two ESC bytes** (octal 33);
 - `APPEND-BATCH` does **NOT create the output file**. The first attempt failed on the machine's own
   console with `ERROR 07 IN BCH02; BATCH OUTPUT ERROR / FILE ERROR NO.: 56 / NO SUCH FILE NAME` -
   which is also the proof that the processor had picked the job up and run it. Create the output
   file first (we carried a 2048-byte one over with the daemon);
 - **batch output is 8-bit, with the high bit set on every character.** Mask with `& 0x7F` to read
   it. Raw and decoded copies are both kept in the capture folder so nobody re-derives that.

**RETRACTED, and the retraction is the more useful fact: "this D100 has no batch processor" was
WRONG.** `LIST-BATCH-QUEUE 0` answering `NO SUCH BATCH` was read as absence. It is not - **batch
processors are numbered from 1**, so index 0 never exists on any machine. `LIST-BATCH-PROCESS` is
the command that actually answers the question, and it shows:

```
      1 IDLE,    NO USER LOGGED IN
      2 PASSIVE
      3 PASSIVE
```

Processor 1 was running the whole time, which is precisely why `@BATCH` handed out number **2**. The
first append therefore failed for ONE reason only - the missing output file (`FILE ERROR NO.: 56`) -
not for want of a processor. Same shape as every count-based mistake in this file: **a query is only
as good as the index you gave it**, and "no such X" answers the question you asked, not the one you
meant.

`LIST-BATCH-QUEUE <n>` reads `EMPTY` after a successful append - the processor takes the job at once
rather than parking it, so an empty queue is not evidence that nothing was submitted. `PASSIVE` in
`LIST-BATCH-PROCESS` means stopped; `IDLE` means running with nobody logged in.

**That is FOUR flags found wired to the Ethernet seam alone** - `--announce-restart` with
`--resync-hard`, the link-seed store, `--sync`, and this. When a flag is refused "because of the
transport", check whether the transport actually cares.

### SOLVED: the daemon DOES run from cold - the XRUNN that said otherwise was the counter bug

The gate was ours, not the machine's. With `--originate-from-seed` the daemon addresses a peer it
has met before straight from `xmsg-link-seed.state`, and D100 accepts it. Its own line, now that the
log is plugged in on this path:

```
[seq] node 100: link opened from the REMEMBERED seed 0x5B, starting Flags1 0x011E
      from the store (nothing was received first)
```

Zero inbound XMSG frames before that line, and the transfer went straight through. Confirmed on the
machine: `FILE 102 : (PACK-ONE:SYSTEM)PROOF:TXT;1`, created with nobody at the console.

**The old `XRUNN` refusal is refuted.** It was measured while `--resync-hard` was zeroing the
outgoing counter on every run, so the letter was behind-sequence before its name was ever read. That
is the same root cause as the "second process is refused" story above - one bug wearing two masks.

**Two traps this experiment walked into, both worth keeping:**

 - **The first "success" was confounded.** That run showed a cold start, but D100 had re-sent stale
   I-frames from a previous conversation just beforehand, and those could have taught the seed on
   their own. A silent control (flag off, same state) *waited* - but it had also seen no stale
   frames, so two things differed and it proved nothing either. Only the third run settled it: flag
   on, and the inbound frames arrive 164 ms AFTER the transfer starts, so they are answers to our
   origination rather than its cause.
 - **The answer was already in the program and being thrown away.** `ServerHost.Log` was wired on
   the relay and Ethernet paths and NOT on the seam, so the one line that names which path opened
   the link could never print. Plugging it in turned a three-run inference into a single line of
   evidence. Same lesson as 2026-08-11: a diagnostic that is not plugged in is not a diagnostic.

**Left OFF by default.** It is proved on D100 only; D103 over HDLC behaves differently on the
related reset question, so the flag stays explicit until a second machine agrees.

**The port-reuse hypothesis is REFUTED.** `0212` vs `0211` was a SYMPTOM, not a cause: the working
runs had `0212` only because an inbound console-triggered conversation had already taken `0211` in
that process - and that same inbound conversation is what opened the readiness gate. Both the port
and the `Flags1=0001` were downstream of "the peer spoke first".

**Why this took a night: the runner never logged its own options.** Six runs could not be told apart
after the fact because the one field that mattered - whether `--resync-hard` was passed - was in
nobody's log. It now prints its raw argv and the exact stored counter it is discarding, with a
warning when that counter is non-zero.

### NEGATIVE: `LIST-CONNECTIONS` cannot see FA conversations at all

Asked immediately after a transfer that HAD just succeeded, with the runner still attached, for both
systems:

```
X-C:LIST-CONNECTIONS   XROUT system? 100   Connection system? 100     -> empty
X-C:LIST-CONNECTIONS   XROUT system? 100   Connection system? 19999   -> empty
```

Empty in both, straight after 20400 bytes had crossed. **So this command does not record FA
conversations**, and comparing it across a success and a refusal was never going to distinguish
them. It is eliminated as a DIAGNOSTIC rather than as a cause - a distinction worth keeping, because
"the table was empty both times" would otherwise read as evidence that nothing is held.

**It was also simply the wrong command to reach for.** `LIST-CONNECTIONS` lists *ongoing* XROUT
connections, of which an FA transfer has none. The X-C commands that DO answer questions about our
traffic are below - all read-only, all safe to run against a live machine:

 - `LIST-SYSTEMS` -> XROUT system -> system: the **datagram sequence pair** (`Send-receive`, decimal)
   plus State, Link, Access and hops. This is the Flags1 oracle described above.
 - `LIST-NAMES <system>`: registered names with their port and **`Free SPs`** - a counted, per-name
   resource. `*FA-SERVER` sat at 29 free while `*FA-USER` sat at 0.
 - `LIST-PORTS` (prompts `Record address?`, bare CR for all): the kernel port table with owner task
   and queue lengths - `128 entries, 13 in use, Max 13 used`. The `No` column matches the `Port`
   column of `LIST-NAMES`. Small integers, so a wire port like `0x0211` is NOT an index into this.
 - `LIST-MESSAGES` (same prompt): live messages with **From-Port / To-port**, owner task, length and
   buffer - `256 entries, 5 in use, Max 34 used`.
 - `HELP <prefix>` expands a command name; `?` lists only what is new in the product version.

The high-water marks (`Max n used`) are the cheap way to see whether something of ours leaks a port
or a message record across runs.

**That is the third read-only elimination**, and it leaves the short list at one item plus the
unknown:

 - **our session port** - `FaServer._replyPort`, cached per process. Deliberately untouched: its
   remarks record that caching it fixed a live failure of eleven ignored confirmations, so it wants
   a designed experiment (pin the port across two processes, the way the connection block was
   pinned) rather than a poke.
 - whatever D100 keys a conversation on that is neither its port table, nor its connection table,
   nor our connection number.

**Where this investigation actually stands**, stated plainly because the file is long: the live path
WORKS, and everything the priority list asks for is verified on the current build. What remains is
one narrow behaviour - a second runner PROCESS is refused where a second conversation inside one
process is not - and it has a documented architecture that avoids it entirely, which is the
long-lived daemon.

### NEGATIVE: D100's PORT TABLE is identical after a success and after a refusal

Read-only comparison on one bring-up, chosen over changing our session port because that caching is
a documented fix for a live failure and should not be disturbed on a guess.

```
after the pull SUCCEEDED   Port table status: 128 entries. 11 in use. Max 11 used.
after the push was REFUSED Port table status: 128 entries. 11 in use. Max 11 used.
```

Same eleven rows, same owners, same high-water mark - XROUT, COSPO, FSART twice, TADAD, XMFIDO
twice, XFTRAD, BAK03, Driver, BAK02. **Nothing is allocated, leaked or consumed on D100's port table
by either outcome.**

**So the second suspect list is shrinking from the outside in.** Eliminated so far, each by a
single-variable test: the FA connection-number block (pinned and reused, still refused) and now
D100's port table (unchanged across both outcomes).

**What is left, and it is a short list:**
 - our session port, which `FaServer._replyPort` caches per process. Untested BECAUSE its remarks
   record that caching it fixed a live failure - eleven ignored confirmations - so it deserves a
   deliberate experiment rather than a poke.
 - `LIST-CONNECTIONS` after each outcome, which has not been compared the same way.
 - whatever D100 keys a conversation on that is neither its port table nor our connection number.

**Method note, since it is what has made the last three answers clean:** one bring-up, two runs,
exactly one thing different, and a READ rather than a change wherever a read will do. The port-table
comparison cost two commands and removed a whole suspect without touching any code.

### NEGATIVE: the FA connection-number block is NOT what a second process trips over

Single-variable test, run properly this time.

```
bring-up, then:
  fa-connection.state = 130
  run 1  pull   ->  20400 bytes, WORKED      (state advances to 194)
  restore fa-connection.state = 130           <- the ONLY thing changed
  run 2  push   ->  refused, 4 connect letters
```

Process 2 reused the EXACT connection-number block that process 1 had just used successfully, and was
still refused. **So the block is not the cause**, and suspect (1) of the two is eliminated.

**What remains of the per-process state:** the session port, which `FaServer._replyPort` caches per
process. Its remarks record that caching it was a deliberate fix for a live failure - a moved port
made D100 ignore eleven confirmations - so read them before changing anything there. That is the last
untested suspect, and if it is also negative then the state lives on D100's side rather than ours.

**Worth noting for whoever runs that:** this test cost one bring-up and two runs, and it answered
cleanly because exactly one thing varied. Every ambiguous result in this file came from varying more
than one.

### CORRECTION (same day): CanReach is NECESSARY but NOT SUFFICIENT - a second PROCESS is refused

Ran the whole priority list against ONE bring-up to confirm the fix end to end, and it exposed the
limit of it:

```
bring-up, then:
  run 1  pull   ->  20400 bytes, worked
  run 2  push   ->  GIVING UP: none of 4 connect letters answered
```

Both runs had the `CanReach` gate. **So the gate is necessary and it is not the whole answer.**

**What the daemon actually proved.** It carried two files because they were two conversations in ONE
process. The distinction that matters is therefore:

 - **many conversations inside one process: WORK** (the daemon, measured)
 - **a second runner PROCESS after the first: REFUSED** (measured just now, and every failing pair
   earlier in this file was two processes)

**That re-reads the whole night correctly.** Every "second conversation refused" observation was a
second PROCESS, not a second conversation. The one-per-bring-up rule was wrong in its unit, not in
its shape.

**Which makes the architecture the answer, not a workaround:** one long-lived process that keeps the
conversation going is what the sync daemon already is. A tool that starts, transfers and exits works
exactly once per bring-up.

**And it sharpens what to look for**, since it is now per-PROCESS state rather than per-conversation:
our FA connection-number block (`fa-connection.state` hands each process a new range - 0x0042..0x0081,
then 0x0082..0x00C1) and our session port are the two things that change between processes and
persist across them. Neither has been tested as the cause.

**Do not repeat this mistake:** "solved" was written after one successful daemon run without checking
whether a second PROCESS still worked. The daemon test and the two-process test look alike and are
not the same experiment.

### The rule that was actually wrong

**A transfer driver must not originate until the PEER HAS ADDRESSED US.**
`ServerHost.CanReach(node)` is that test - true only once an inbound datagram has taught the layer
the peer's envelope seed. A gate wired to our own link state (`link.State == Connected`) is true
milliseconds after startup, so the connect letter goes out before the peer can answer it. D100
replies `XRUNN` and our drivers used to DISCARD that reply, so it looked like silence.

Pinned by `SyncReadinessWiringTests`, proved to fail on the weak gate. One definition, in
`Program.BuildSyncReadiness`.

### The bring-up recipe, confirmed repeatedly

```
1. restart RetroCore (F:\RC\RonnyTest\HDLC1)
2. WAIT ~3 minutes - the BOOT starts COSMOS. Verify: X-C -> LIST-NAMES, answer 100 EXPLICITLY,
   and *FA-SERVER must be listed with free session points.
3. X-C:  DEF-REMOTE,,D19999 19999        (note the SPACE before the number)
         START-LINK,1362,,,-1,,
4. run the runner; a console command on D100 makes it address us, and transfers then work.
```

**The X-C prompts are STICKY** - a bare CR reuses the last value, so answer `XROUT system?`
explicitly or you will read about a different system than you think.

### Verified live on the current build

 - **#38** pull: 20400 bytes, SHA256 identical to the original
 - **#23** push: verified ON D100 as `FILE 63 : (PACK-ONE:SYSTEM)SHOWME:TXT;1`
 - **#33** sync daemon: two files carried unattended, `FILE 62` and `FILE 97` on D100
 - **#34** chat: two real ND terminals talking through our server, both directions

### DEAD ENDS - measured and refuted. Do not re-run any of these.

Each was a real difference between a working run and a failing one. None was the cause.

 1. the session port - real servers reuse one per run, our caching was already right
 2. `Subaddr` overflow for node 19999 - that column describes the LINK, not the number
 3. RX-BAD counts - our own restart noise, not rejection
 4. the first-conversation pattern - refuted, revived, refuted again
 5. single-threaded peer - it serves us while refusing us, proved in the same second
 6. the one-shot connect letter racing readiness - a retry does not help
 7. link state `Call` vs `Run` - measured with the runner dead, which is not the failing state
 8. our frame bytes - byte-identical between a run that works and one refused
 9. the name in the letter - same
10. the remembered seed - it makes things WORSE without a retry
11. `*FA-SERVER` missing - it is started by the boot; that reading was taken mid-boot
12. no access / no route in `LIST-ROUTING-INFO` - `DEFINE-FRIEND-SYSTEM` clears it, the route arrives
13. the full COSMOS bring-up - ran it, changed nothing
14. one client conversation per bring-up - refuted by the daemon carrying two files
15. XROUT name service degraded - a consequence of my own commands, not a cause

**The method lesson, which cost more than any of them:** comparing two runs that differ in many ways
finds a difference every time and none of them is evidence. What worked, every time, was asking the
MACHINE a direct question - `LIST-LINKS`, `LIST-NAMES`, `LIST-ROUTING-INFO`, the XROUT error enum -
or reading our own captures. Do that first.

---

## 0. THE BLOCKER - and it is NOT the send window

> **CORRECTED 2026-08-17, same night.** This section first said the window never opens. It does. A
> traced run shows our first frame going out at seq 0, D100 answering `ack seq=29` because it had
> carried state over, our layer repositioning to 29 and then sending **29, 30, 31 back to back** -
> with D100 acknowledging 30, 31, 32 in turn. The parking IS real but TRANSIENT: it lasts until the
> peer's first acknowledgement places us, and then the window opens and traffic flows both ways.
> Everything below the correction line was written before that run.

### What is actually wrong

The FA ladder now reaches its first reply and stalls exactly there:

```
[fa] connect letter ... answering with an FA ConnectionConfirm 07D2 0010 00C2 6400
[fa] request ReserveFileEntry seq 1 from system 100: accepted
[fa] short acknowledgement 1 to system 100 at Flags1 0x0008
[fa] reply to system 100: 18 byte(s), FA body 07F0 0010 8000 9081 92 0002 92 0001 F2 00...
[fa] system 100 re-sent the request at datagram Flags 1 0x0008; replaying the same reply
```

D100 re-sends the request instead of accepting the reply, and gives up. Meanwhile the LINK layer
carries and acknowledges our frames throughout - seq 29 through 35, each answered.

**So our frames arrive and the layer above rejects the reply.** That is a much narrower target than
anything chased tonight: the fault is in what the REPLY says, not in whether it is sent.

### The comparison is DONE, and the reply is indistinguishable from a real one

Field by field against a real server's ReserveFileEntry reply in
`claude-delete-file-102-to-100-2026-07-29.pcapng`:

| field | real | ours | |
|---|---|---|---|
| magic, subtype | `2113 000E` | `2113 000E` | same |
| destination, source | `0066 0064` | `0064 4E1F` | correct nodes for each session |
| Flags1 | `01CE` | `0001` | the sender's own datagram sequence |
| Flags2 | `0012` | `0012` | same |
| checksum | `DC34` | `9048` | **both verify** - recomputed by hand |
| role | `2100 8284` | `2100 8284` | same |
| client node, port | `0066 0666` | `0064 04EF` | correctly echoed |
| server node, port | `0064 05AD` | `4E1F 0211` | **the one structural difference** |
| length | `0012` | `0012` | same, and both pad to even |
| FA body | `07F0 0002 8000 9081 920002 920001 F200FF` | `07F0 0010 8000 9081 920002 920001 F200FF` | same but the conversation |

**So the reply is not malformed.** Every field a single frame can be judged on is right, including the
checksum, the role bytes and the whole FA body.

**The one difference left is the server's session port**: a real D100 answers from `0x05AD`, we
answer from `0x0211`, a constant. The same shape appeared in the TAD accept, where a real server uses
`0x0215` and we use `0x0156`. In both protocols we announce the port in the confirm and then reply
from it, so we are self-consistent - but we have never established what the number MEANS, and two
protocols disagreeing with real traffic the same way is not a coincidence worth ignoring.

### SOLVED: it was never a budget. The transfer must WAIT UNTIL THE PEER HAS ADDRESSED US.

The sync daemon now carries files over HDLC, unattended, and D100 holds them:

```
[sync] create D100(SYSTEM)."SYNCA:TXT" ... done, 31 byte(s)
[sync] create D100(SYSTEM)."SYNCB:TXT" ... done, 44 byte(s)

@LIST-FILES SYNCA:TXT,,     FILE 62 : (PACK-ONE:SYSTEM)SYNCA:TXT;1
@LIST-FILES SYNCB:TXT,,     FILE 97 : (PACK-ONE:SYSTEM)SYNCB:TXT;1
```

**TWO client conversations, back to back, in ONE process. So "one client conversation per bring-up"
is REFUTED** - by the same daemon that was supposed to be blocked by it.

**The one-line cause.** I gated the daemon on `link.State == Connected`, so it fired about five
seconds after startup, before D100 had sent us anything. The push and pull drivers gate on
`CanReach`, which is true only once an INBOUND datagram has taught the layer the peer's link seed -
the point at which a frame we originate can actually be addressed. Changed the daemon to the same
gate and both files went across on the first attempt.

**And that explains every "refused" run in this register.** Each was a NEW runner process firing its
connect letter before D100 had addressed that process. The successes were the runs where a console
command happened to make D100 speak first. Nothing was ever consumed, no budget was ever spent, and
the peer was never refusing us on protocol grounds - **we were talking before we could be heard.**

**What this retires, and it is most of the file above:** the session-port census, the Subaddr
overflow, the first-conversation pattern, the single-threaded peer, the one-shot race, the link
state, the missing `*FA-SERVER`, and the "one conversation per bring-up" rule that replaced them all.
Each was a real difference between two runs; none was the cause.

**The rule that survives, and it is the cheap one:** a driver must not originate until the peer has
addressed it. `CanReach` is that test. Anything gated on our own link state instead will fire early,
be ignored, and look exactly like a protocol refusal.

**Still true and still worth the fix:** D100 ANSWERS the early letter with `XRUNN`, and until this
session our drivers discarded that answer. The new "<- from node N on our port" log line is what
made all of this readable, and it should never be removed.

### THE SYNC DAEMON NOW RUNS ON HDLC - and it hits the one-conversation wall, as predicted

Wired `SyncDaemon` into `RunSeamAsync`: constructed beside the node host, fed inbound frames from
the same `MessageReceived` hook the push and pull use, and pumped on the loop tick. Readiness is the
LAPB link being `Connected`, the HDLC equivalent of `HasLearnedPeer` on the Ethernet seam. The
`--sync` refusal added an hour ago is gone; `--append-batch` keeps its refusal, since it is still
Ethernet-only.

**It starts and it works as a daemon:**

```
[sync] ledger: ...xmsg-sync-ledger.state (1 file(s) already carried)
[sync] watching ...\syncbox -> D100(SYSTEM), scanning every 5s, a file must be still for 3s
[sync] 2 transfer(s) queued, 2 waiting
[sync] create D100(SYSTEM)."SYNCA:TXT" on D100 <- ...\SYNCA.TXT started
[push] sending SYNCA.TXT (31 bytes) to "SYNCA:TXT" in 1 block(s)
```

Folder scanned, both files queued, the ledger consulted, the remote name built with the quoting rule
that was carved earlier tonight (`D100(SYSTEM)."SYNCA:TXT"` - name quoted, system not), and the push
driven. **Everything above the transfer is correct.**

**And then the transfer is refused**, four connect letters, exactly as a first-conversation transfer
is refused whenever the one-conversation budget has already gone. So #33 is now blocked by #50 and
by nothing else - which is progress worth having, because before this the daemon could not run on
this path at all and the blocker was invisible.

**What this proves about the ordering rule, and it is a caution.** This run WAS the first client
conversation after a fresh boot and bring-up, and it still failed. So "one conversation per bring-up"
is not the whole rule either - something about the daemon's timing differs from the manual push that
succeeded at 08:35, most likely that it fires within five seconds of startup while the console
trigger is still in flight. **Recorded as a limit on the rule rather than a new theory** - the next
run should give the daemon a longer initial delay and see whether that alone changes it.

### THE THIRD SILENTLY-IGNORED FLAG: `--sync` has NEVER run over HDLC

Tried to test "one process, two transfers" with the folder-watch daemon - two files in a watched
folder, one runner. The daemon never started, and there was no error, because **`--sync` is parsed
and then handed only to the ETHERNET seam.** Over the HDLC bridge it does nothing at all.

That is the THIRD capability found wired to one seam path and silently absent from the other:

```
--announce-restart / --resync-hard   parsed, Ethernet only   - so "cured by --resync-hard" over
                                                               HDLC cured nothing
the link-seed store                  persisted, never read   - a transfer could not start against
                                                               a quiet peer
--sync                               parsed, Ethernet only   - the daemon has NEVER run over HDLC
```

**So task #33's "the sync daemon carried a file to D100 unattended" was an ETHERNET result**, and the
daemon has no HDLC history at all. That is not a regression, it is a gap that was never visible.

**Fixed the class, not just the instance.** The runner now REFUSES `--sync` and `--append-batch` on
the HDLC path with a message naming the reason, instead of starting and quietly doing nothing:

```
[runner] --sync is NOT supported on the HDLC path - it is wired only to the Ethernet seam.
         Refusing rather than starting a runner that silently never syncs.
```

Verified: the refusal fires and the process exits non-zero.

**Why refuse rather than warn.** A flag that is accepted and ignored gets written into a commit
message as working - that is exactly how `--resync-hard` entered this project's history as a cure.
An exit code cannot be misread.

**A trap that cost two builds here, worth stating:** the runner holds its own exe, so a rebuild while
one is running SILENTLY keeps the old binary - the guard was in the source and absent from the
running program, and the version banner (`built 2026-08-17 07:12:32`) was the only tell. **Check the
build stamp in the banner before concluding a change did not work.**

### CONFIRMED, BOTH HALVES: ONE client conversation per bring-up, and direction is irrelevant

The single run that was supposed to decide it, run - and then its complement, on the same machine.

**Preparation, now a reliable recipe:** restart `RetroCore`, WAIT about three minutes for the boot
(`LIST-NAMES 100` shows `*FA-SERVER` with 30 free SPs, started by the boot itself), then
`DEF-REMOTE,,D19999 19999` and `START-LINK,1362,,,-1,,`.

**First client conversation - a PUSH, the direction that has failed all session:**

```
[push] finished: 20400 bytes written to "DECIDE:TXT"
@LIST-FILES DECIDE:TXT,,
FILE 78 : (PACK-ONE:SYSTEM)DECIDE:TXT;1        <- verified ON THE MACHINE
```

**Second client conversation, same machine, ninety seconds later:**

```
[push] no answer to the connect letter - sending it again (attempt 2 of 4)
... 3 of 4 ... 4 of 4
[push] GIVING UP: node 100 answered none of 4 connect letters.
```

**So the rule is: ONE client conversation per bring-up. Push or pull makes no difference - the
direction question is dead, and #23 is re-verified along the way.** Every run of this session now
fits without exception, including the ones that caused this pattern to be proposed, retracted and
revived:

```
06:00  booted          1st client conv (pull)   WORKED
07:13  NOT booted      1st (push)               refused - the machine, not the pattern
08:26  booted          1st client conv (pull)   WORKED
08:28  same machine    2nd (push)               refused
08:35  booted          1st client conv (PUSH)   WORKED - verified on D100
08:36  same machine    2nd (push)               refused
```

**What is NOT yet known is what the first conversation consumes.** `*FA-SERVER` still reports 30 free
session points afterwards, so it is not a session-point leak in the obvious place. The candidates are
D100's side of OUR port, its XROUT connection entry for us, or something our runner fails to release
when it exits - each of which is now a small, checkable question rather than a protocol mystery.

**This is what blocks #33.** A folder-watch sync daemon opens a conversation per file; it will move
exactly one and then stop. Fixing that is the whole remaining task, and it is now well posed.

### THE BOOT STARTS COSMOS - and the first-client-conversation pattern is back, with all data consistent

**Two findings, and together they reconcile every live run of this session.**

**1. `*FA-SERVER` is started by D100's BOOT, not by any bring-up command.** Restarted the machine and
waited three minutes touching NOTHING:

```
   100    11      30     *FA-SERVER.
   100     0             D100.       102  D102.   103  D103.
```

Registered, unaided. Only `D19999` is missing, which is what `DEF-REMOTE` supplies. **So the earlier
"the two-command bring-up never starts `*FA-SERVER`" entry is WRONG** - that reading was taken about
a minute after a restart, against a machine that had not finished booting. Measuring a half-booted
machine and blaming the recipe is the same error as measuring the link with the runner dead.

**2. #38 RE-VERIFIED on the current build**, in that state: 20400 bytes, `D523BDDC...8D5AEBDA` both
sides.

**3. And then, on the SAME machine two minutes later, the push was refused all four letters.** The
frames are byte-identical - not just the letter, the WHOLE frame including header, sub-header and
ports:

```
pull (worked, 08:26):  09022113000E00644E1F000000229039210086E4006400004E1F021100221B410012FF...
push (refused, 08:28): 09022113000E00644E1F000000229039210086E4006400004E1F021100221B410012FF...
```

**So the difference is not in what we send. It is which conversation it is.**

```
06:00   fully booted        1st client conversation (pull)   WORKED
07:13   restarted 1 min ago - NOT booted   1st (push)        refused
08:26   fully booted        1st client conversation (pull)   WORKED
08:28   same machine        2nd client conversation (push)   refused
```

**Every run fits: the FIRST client conversation after a fully-booted bring-up succeeds, later ones
are refused.** The 07:13 push, which looked like a counter-example when the pattern was first
proposed and caused it to be retracted, was against a machine that had not finished booting - so it
never tested the pattern at all.

**This was retracted earlier for a good reason and is being revived for a better one.** The
refutation used two LISTINGS, which exercise our SERVER half; the server half genuinely does allow
many conversations. The claim now is only about the CLIENT half, and every client run is consistent
with it.

**THE TEST THAT SETTLES IT, and it must be run before this is called a finding:** restart D100, wait
for the boot, `DEF-REMOTE` + `START-LINK`, then run a PUSH as the very FIRST client conversation. If
it succeeds, the pattern is real and the direction is irrelevant. If it fails, the pattern is about
push-versus-pull after all. One run decides it, and nothing else should be built until it does.

### THE SEED IS ONLY SAFE WHERE A RETRY BACKS IT - reverted a second time, with the reason measured

I re-enabled opening the link from the remembered seed to make the "first client conversation"
experiment possible. It made the higher-priority path WORSE, and that is worth stating plainly.

With the seed, a transfer originates the moment LAPB is up, against a peer that has said nothing.
**The PUSH survives that because it retries its connect letter four times. The PULL does not - it has
no retry** - so it fires one letter early, the peer ignores it, and the transfer is dead. Measured:
with the seed enabled the pull got a single `InitializationNak` and nothing else, four runs running.

**So I improved the path I was investigating and quietly degraded the one at the top of the priority
list.** The seed call is reverted again, and the comment in the code now says it may only be
re-enabled once the PULL has the same bounded retry the push has.

**Current lab state, recorded because it is worse than when this session started.** After a
`RetroCore` restart plus the three-step bring-up (`DEF-REMOTE`, `START-LINK`,
`MODE (COSMOS-BASIC)COS-FA-SERV-E:MODE`), with `*FA-SERVER` confirmed registered:

 - our announce draws an `InitializationNak` and NOTHING else
 - a `LIST-FILES` typed on D100's console produces NO connect letter to our server at all

**Both halves are now silent**, where earlier tonight the server half worked reliably. So D100 has
moved into a third state, and neither the two-command nor the three-step bring-up reproduces the
configuration that was working at 06:00 - which came from a boot I did not perform and have never
been able to recreate.

**That is the honest summary of the live work:** the machine that worked at 06:00 was in a state
this session has not managed to rebuild, and every subsequent measurement has been taken against
machines in other states. The findings that survive are the ones that came from reading D100's own
tables and our own captures, not from run-to-run comparison.

### PROVEN GAP: the two-command bring-up does NOT start `*FA-SERVER`

Measured directly with `LIST-NAMES`, answering the sticky prompt explicitly this time.

**After `RetroCore` restart + `DEF-REMOTE` + `START-LINK` (the shortcut used all night):**

```
   100     0   D100.      100  2   *COSPO.     100  3   *FA-FSA.
   100     4   *TADADM.   100  6   *XM-FIDO.   100  7   *XFTRA.
   102     0   D102.      103  0   D103.     19999  0   D19999.
```

**`*FA-SERVER` IS ABSENT.** Then `@MODE (COSMOS-BASIC)COS-FA-SERV-E:MODE,,,` alone, which ends in
`Server 1 started.  No of FACs attached: 30`:

```
   100    12      30     *FA-SERVER.
```

**So in every run after my first restart, our connect letter asked D100 for a name that was genuinely
not registered.** `XRUNN` - "unknown name (of server or system)" - was the literal truth, on the
SERVER half of its own phrase. The 06:00 pull worked because that D100 had COSMOS running from an
earlier boot, before any restart of mine.

**HONEST LIMIT, and it matters.** This does NOT explain every failure. At 07:05 `LIST-NAMES` showed
`*FA-SERVER` present on port 11 and the push was still answered `XRUNN`. So the missing name is a
real, proven gap in the bring-up and a real cause of SOME failures, and it is not established as the
whole story. Two different states have produced the same error.

**The bring-up recipe is now known-incomplete in a specific, checkable way.** `DEF-REMOTE` +
`START-LINK` gives a link and names but NO file server. The minimum for a transfer appears to be
those two plus `MODE (COSMOS-BASIC)COS-FA-SERV-E:MODE,,,` - which is far less than the whole restart
script, and does not carry its lock-out trap.

**Verify with `LIST-NAMES` and an EXPLICIT system number before every transfer attempt.** If
`*FA-SERVER` is not in the list, nothing else is worth trying, and hours were spent tonight not
knowing that.

### AFTER THE BRING-UP, D100's XROUT NAME SERVICE IS DEGRADED - and the prompts are STICKY

Two findings, one of them a caution about my own earlier readings.

**1. `LIST-NAMES` no longer works on D100, even for its OWN system.**

```
X-C:LIST-NAMES
XROUT system? 100
System   Port  Free SPs   Name
Error in communicating with XROUT.
XMSG Kernel error: Remote system is not accessible
```

Before the bring-up this command listed `*FA-SERVER`, `D100`, `D19999` and the rest. It now refuses
for the LOCAL system. So the documented restart sequence left XROUT's name service in a worse state
than it found it - and since every name lookup goes through XROUT, that is a plausible source of
`XRUNN` in this state, though NOT of the `XRUNN` seen before the bring-up, when `LIST-NAMES` was
healthy.

**The lab is therefore left degraded, and that is recorded here rather than hidden.** D100 answers
its console, serves files, and carries the link; its XROUT name service does not answer. Another
`RetroCore` restart plus the two-command bring-up restores the name service - that combination was
demonstrably healthy earlier tonight.

**2. THE X-C PROMPTS ARE STICKY, which undermines some earlier readings.** `XROUT system?` answered
with a bare CR does NOT mean "local" - it reuses whatever was last entered. `LIST-NAMES` failed with
a blank prompt purely because `19999` was left over from a `DEFINE-SYSTEM-ROUTE` several commands
earlier.

**Which means any X-C output in this register taken with a blank `XROUT system?` may describe a
DIFFERENT system than intended.** The `LIST-ROUTING-INFO` and `LIST-SYSTEMS` readings were taken that
way. They are probably right - they named both systems in their output, which a wrong-system query
would not - but "probably" is not the standard this register is meant to hold, and any future reading
must answer that prompt EXPLICITLY.

**That is a thirteenth way to be wrong, and it is a new one:** not a bad inference from good data, but
good inference from data about something else. Answer every prompt explicitly.

### THE FULL BRING-UP WAS RUN. It did not fix it, so "never fully configured" is RETRACTED too.

`tools
estart-xmsg-cosmos.ps1 -Port 9010` ran end to end: XMSG stopped and started, TAD started,
six `DEF-REMOTE` lines, `START-LINK`, `ENABLE-ROUTE-THROUGH`, and the COSMOS mode file through to the
line the script says to look for:

```
Server 1 started.     No of FACs attached: 30
```

**The push is still refused, all four attempts.**

**And every table now looks RIGHT.** The system row for 19999, beside a known-good peer:

```
Address  Sysno  State    Link Subaddr  Send-receive        Access
1211546    102      4  152164     102  1203    1203        *----P     <- the working peer, earlier
1211535  19999      4  152164       0     3       3        *----P     <- us, now
```

State 4, link bound, sequence advancing, and the Access column now reads `*----P` - identical to the
peer that works, with the `<=` gone. `LIST-LINKS` shows the link `Run` and bound to 19999 whenever
our runner is attached.

**Yet `LIST-ROUTING-INFO` still says:**

```
  19999  L: *->19999   T: *, but no route to system 19999   A: *->19999, but no route to system 19999
```

**So the machine reports a healthy system, a healthy link, and no route, at the same time.** That is
the contradiction to chase next, and it is a much smaller one than anything earlier in this register:
two of D100's own diagnostics disagree with each other about the same system.

**Retracted:** "the machine was never fully configured" (commit 4b0dc1f9). It has now been configured
by its own documented script and behaves the same. The two-command shortcut being incomplete was
true and was NOT the cause.

**What is genuinely established after all of this**, and is worth more than the unsolved half:
 - D100 ANSWERS every connect letter with `XRUNN`; it never ignored us, and our driver discards the
   answer - that one decode invalidated nine hypotheses at a stroke
 - `XRUNN` here means the SYSTEM half of "unknown name (of server or system)", not `*FA-SERVER`
 - `DEFINE-FRIEND-SYSTEM` clears the "no access" complaint, and the full bring-up leaves the Access
   column matching a working peer
 - the frame, the seed, the link state, ordering, timing and our node number are all eliminated by
   measurement

**The next question is now inside XROUT's route table specifically:** why `LIST-SYSTEMS` shows a
bound link while `LIST-ROUTING-INFO` reports no route for the same system. `List-Frames`,
`List-Ports` and `List-Utilization` are unread, and the kernel's route walk is in the symbols.

### THE ROUTE CANNOT BE DEFINED BY HAND: "Remote system is not accessible", circularly

`DEFINE-SYSTEM-ROUTE` was tried with every sensible via, with the runner CONNECTED this time (the
state mistake is not repeated):

```
Via system? 19999   ->  XMSG Routing/Naming error: Illegal system number parameter
Via system? <blank> ->  XMSG Kernel error: Remote system is not accessible
```

A system cannot be its own via, and a blank via - the direct-neighbour form - is refused because the
system is not accessible. **But accessibility is the very thing the route would provide.** The two
errors close a loop, so the route is not meant to be defined by hand for a direct neighbour: it is
meant to ARRIVE, from the reachability exchange during a proper bring-up.

**Which points at the part that has been skipped all night.** The bring-up used throughout was two
commands:

```
DEF-REMOTE,,D19999 19999
START-LINK,1362,,,-1,,
```

The recorded working sequence is not that. From the restart notes, it is five `DEF-REMOTE` lines,
then `START-NET-SERVER,ENNS0,,,N`, `DEF-NETWORK-CONN <sys> ENNS0,,0,0,0,0`, `START-LINK`,
`ENABLE-ROUTE-THROUGH`, and finally the `COS-START-E04:MODE` file. **`ENABLE-ROUTE-THROUGH` and
`DEF-NETWORK-CONN` are exactly the steps that would populate a route**, and neither has been run
since the machine was restarted.

**So the honest conclusion of this whole line: the machine was never fully configured.** Twelve
explanations were sought in the protocol, our encoder, sequence numbers, ports, timing and link
state, for a peer that was missing its network configuration. `DEFINE-FRIEND-SYSTEM` fixed the access
half by hand and made that visible; the route half says plainly that it wants the real sequence.

**Next, and it is the documented path rather than another experiment:**
`SINTRAN\XMSG	ools
estart-xmsg-cosmos.ps1 -Port 9010` runs the whole sequence. **Read its four
traps first** - trap 3 is a genuine console lock-out if `COS-START-E04:MODE` and `LOAD-MODE` are
split across two connections. Then check `LIST-ROUTING-INFO` shows a route to 19999 BEFORE running
any transfer.

### CONFIRMED CURE, HALF APPLIED: `DEFINE-FRIEND-SYSTEM` removes the "no access"

`HELP` -> `DEF` lists what was needed all along:

```
Define-System-Route          Define-Friend-System        Define-Remote-Name
Define-Local-System          Define-Network-Connection   Define-Alternative-Link  ...
```

**`DEFINE-FRIEND-SYSTEM` / `System? 19999` -> `Ok`**, and the routing line changed exactly where it
should, checked BEFORE going anywhere near the push:

```
before:   19999  L: *->19999   T: *, but no ACCESS to system 19999   A: *, but no route
after:    19999  L: *->19999   T: *, but no route to system 19999    A: *->19999, but no route
```

**The access complaint is gone, and the ACTUAL path now resolves to `*->19999`.** That is the
`5SFRI` friend bit from the X-C binary, set at last - the fourth reading of the same fact, and the
first one that changed the machine's answer.

**What remains is the ROUTE**, and `DEFINE-SYSTEM-ROUTE` is the command for it. Its prompts are:

```
X-C:DEFINE-SYSTEM-ROUTE
XROUT system?              <- blank accepted
System? 19999
Via system? 19999          -> XMSG Routing/Naming error: Illegal system number parameter
```

**A system cannot be its own via.** For a DIRECT neighbour the via is something else - blank, `0`,
the link number, or the local system - and which one is not yet known. That is the only thing
standing between here and a working client half.

**Note the shape of this whole cure:** the bring-up used all night was `DEF-REMOTE` + `START-LINK`,
two commands. The recorded working sequence in the restart notes has five `DEF-REMOTE` lines plus
`START-NET-SERVER`, `DEF-NETWORK-CONN` and `ENABLE-ROUTE-THROUGH`. The shortcut was never equivalent,
and every "the protocol is refusing us" conclusion was really an incomplete bring-up. Worth
remembering the next time a machine seems to reject something for no reason: check that it was fully
configured before blaming the wire.

### THE MACHINE ANSWERS: "no access to system 19999" - and it took one command

Asked D100 directly, which is what should have happened ten answers ago. `X-C` ->
`LIST-ROUTING-INFO`:

```
   To     Route
    100  L: *
         T: *
         A: *
  19999  L: *->19999
         T: *, but no access to system 19999
         A: *, but no route to system 19999

L: - path according to local tables
T: - path according to tables
A: - actual path
```

**D100 has a LOCAL table entry for 19999 and NO ACCESS and NO ACTUAL ROUTE.** Compare node 100,
which has all three. The machine states the fault in plain English, and it has been able to say so
the whole time.

**That is exactly why XROUT answers `XRUNN`.** The name resolves in the local table - which is why
`LIST-NAMES` lists it and why `DEF-REMOTE` says the name is taken - but there is no ACCESS to the
system, so a lookup on behalf of a conversation fails. `XRUNN` reads "unknown name (of server or
system)" and it means the SYSTEM half of that phrase, not the server half. Every reading that
assumed `*FA-SERVER` was the unresolvable part was looking at the wrong half of the error's own name.

**It also joins two findings that looked unrelated.** The Access column decoded from the X-C binary
(`5SFRI` -> `P` for not-a-friend) and `LIST-FRIEND-SYSTEMS` answering "No friend exists" are the same
fact seen from two other angles: 19999 has no access rights on D100. Three independent readings now
agree, which is the check worth having after eleven that did not.

**And it explains the asymmetry that made no sense.** D100 SERVES us happily - it opens conversations
with our file server and completes them - because answering an inbound conversation needs no route
of its own. Originating one towards 19999 needs access, and that is precisely the half that fails.
Our server half working was never evidence that our client half should.

**What is NOT yet known:** how access is granted. `HELP FRIEND` and `HELP REMOTE` match nothing -
X-C's `HELP` takes command prefixes, so the right query is `HELP` then `DEF`, or the full
`DEFINE-REMOTE...` spelling. `DEF-REMOTE,,D19999 19999` has been sent with TWO skipped parameters
between the verb and the name, and one of those is the likely place access is set. The
`restart-xmsg-cosmos` notes list five such definitions in the working sequence, so a full bring-up
may set it and my two-command shortcut may be what has been missing all along.

**Next, concretely:** find `DEF-REMOTE`'s full parameter list, set the access parameter for 19999,
then re-run the push. If the route line changes from "no access" to a path, the answer is settled.

### ORDERING IS DEAD IN EVERY FORM: a COMPLETED prior conversation does not help either

The last variant of the ordering idea, tested and refused.

The 07:26 run had D100's conversation arrive AFTER our first letter, which left the loophole that a
COMPLETED inbound conversation must precede it - the shape of the 06:00 run that worked. D100 had
exactly that state at 07:31 (a listing had completed at 07:26), so a push started then tests it
directly:

```
07:31:34  SendConnectLetter: 1 frame(s) sent
07:31:39  ... attempt 2 of 4
07:31:44  ... attempt 3 of 4
07:31:49  ... attempt 4 of 4
07:31:54  GIVING UP: node 100 answered none of 4 connect letters.
```

**Refused.** So ordering does not matter in any form: not before, not during, not completed-first.

**The eliminated list is now long enough to be the finding.** By measurement, none of these is the
cause: the frame bytes, the name in them, the link state, the seed, conversation count, conversation
ordering, a completed prior conversation, single-threading, the one-shot race, the peer being busy,
the peer being idle, and our node number. The peer answers `XRUNN` for a name its own `LIST-NAMES`
lists as registered, and it does so while conversing happily with our server on the same link.

**What has NOT been tried, and is the honest next move:** nothing on our side is left to vary, so
stop varying it. The remaining difference between the run that worked and every run since is inside
D100, and the only instrument not yet used on it is D100's own diagnostics - `LIST-ROUTING-INFO`,
the XROUT lookup path, and if those do not answer, the kernel's name-resolution code in
`XMSG-SYMBOL-L03.SYMB`. Task #45 holds that, with the `DEF-NETWORK-CONN` local-system prior.

**A note on method, since this register is now a record of eleven wrong answers.** Every one of them
was a difference between a working run and a failing one that turned out not to be causal. The
pattern is stable enough to name: with two runs that differ in many ways, ANY difference can be found
and none of them is evidence. What broke the deadlock each time was asking the MACHINE a direct
question rather than comparing our own runs - `LIST-LINKS`, `LIST-NAMES`, the XROUT error enum. That
should have been the first move, not the eleventh.

### REFUTED: "the peer must address us first" - it did, mid-retry, and our letters were still refused

The last untested variable for the CLIENT half, isolated using the retry as the instrument.

Started a push, which fires at once and is refused, then made D100 address us DURING the retry
window. Same runner, same link, same seconds:

```
07:26:50  [push] SendConnectLetter: 1 frame(s) sent
07:26:54  [RX]   100->19999 Data                      <- D100 opens a conversation with US
07:26:54  [fa]   connect letter from node 100 ... answering with an FA ConnectionConfirm
07:26:55  [RX]   ... a full stream of Data frames, our server serving normally
07:26:55  [push] no answer to the connect letter - sending it again (attempt 2 of 4)
07:27:00  [push] ... attempt 3 of 4
07:27:05  [push] ... attempt 4 of 4
07:27:10  [push] GIVING UP: node 100 answered none of 4 connect letters.
```

**D100 was actively talking to us while ignoring us.** Our server answered its connect letter and
served it a listing, in the same second that our own connect letter went unanswered for the second
time.

**So the two halves are independent, and that is the sharpest statement available:** on one link, in
one instant, D100 will open and complete a conversation with our server while refusing to open one
with us. It is not the link, not the frame, not ordering, not who spoke first, not the peer being
busy, and not the peer being idle - all of those are now eliminated by measurement.

**Every environmental explanation is spent.** What is left is inside D100's XROUT: our XSLET asks it
to resolve a name, and it answers XRUNN even though `LIST-NAMES` lists that name. The question is no
longer about our side at all.

**The next step is therefore on the machine, not in our code:** drive the same lookup from D100's own
console - `LIST-ROUTING-INFO`, and the XROUT path that `DEF-NETWORK-CONN` exercises - and see what it
says about resolving `*FA-SERVER` for a REMOTE asker as opposed to a local one. The relevant prior is
already recorded: `DEF-NETWORK-CONN` refuses the LOCAL system by name, which is the same family of
refusal.

### PROVED: the SAME letter bytes are accepted at one moment and answered XRUNN at another

The diff the previous entry asked for, done - and it settles the content question for good.

Our connect letter, from the pull that WORKED and from the push that was REFUSED, both taken from
`[TX]` lines:

```
working pull:  1B410012FF0A2A46412D534552564552FE044431303007E2000000026400A200FF00
refused push:  1B410012FF0A2A46412D534552564552FE044431303007E2000000026400A200FF00
```

**Identical. Every byte.** Same serial, same XSLET service, same `*FA-SERVER`, same `D100`, same
length, same trailer. One was accepted and drove a 20400-byte transfer; the other is answered
`XRUNN`, unknown name.

**So the letter is not the variable, and neither is the name it carries.** That closes the question
the previous entry opened, and it closes it against the obvious answer.

**A near miss worth recording.** The first diff appeared to show the accepted letter naming `D19999`
rather than `D100` - a clean, satisfying explanation. It was an `[RX]` line: D100's letter TO US,
asking OUR `*FA-SERVER`. Comparing a received frame with a sent one and calling the difference a
defect would have been the tenth wrong answer, and it was caught only because the code computes that
field identically for push and pull, which a genuine wire difference would have contradicted.
**When the wire and the code disagree, check that you are reading the same direction.**

**Where this leaves it, precisely:** identical bytes, different answers, so the difference is in
D100's state at the moment of asking - specifically whether XROUT can resolve `D100` for us right
then. `LIST-NAMES` shows the name registered, so "not registered" is not it either.

**The one measurement that would settle it:** ask D100 to resolve the name at both moments, not just
look at the list. `LIST-ROUTING-INFO` and the XROUT lookup path are where that lives, and the answer
is a comparison of two lookups rather than of two frames.

### DECODED FOR CERTAIN: D100 answers our connect letter with XRUNN - "Unknown name"

The reply byte is now identified from OUR OWN enum rather than a table that merely contained the
value.

`XroutService` starts at **64** (`XSNUL = 64`, `XSLET = 65`), so a small number in that byte of a
REPLY cannot be a service - it is an `XroutError`. And `XroutError` says:

```
XRUNN = 2    "Unknown name (of server or system)"
```

So the full exchange is, unambiguously:

```
us   -> D100   serial 0x1B  service 0x41 (XSLET)   "*FA-SERVER"  on  "D100"
D100 -> us     serial 0x1B  error   0x02 (XRUNN)   unknown name
```

**Every "D100 ignores our connect letter" conclusion in this register is wrong.** D100 answered every
single one, immediately, with a named error. Our driver dropped the answer because it does not decode
an error in that position, so the ladder stalled in silence and nine hypotheses were built on top of
a message we were throwing away.

**THE TENSION, and it is the next question.** `LIST-NAMES` on D100 shows both names present:

```
   100     0             D100.
   100    11      30     *FA-SERVER.
```

So a name D100 lists as registered is a name D100 says it cannot resolve. One of these is not what it
looks like. Candidates, none tested:
 - the trailing `.` in the listing may be part of the stored name rather than display
 - the letter names the SYSTEM as `D100`, and XROUT may refuse to resolve the LOCAL system by name
   the way `DEF-NETWORK-CONN` refuses it ("You cannot make this network definition for the local
   defined system")
 - the name we encode may differ in case, padding or length from the stored one

**How to settle it without guessing:** the successful pull at 06:00 sent a letter that WAS accepted.
Diff its target-name bytes against the ones being refused now. That is a comparison of two captures
we already own, not another live experiment.

### DECODED: `1B41` is serial + service, NOT a role byte - and D100 replies with service 02

Two corrections and one real step forward.

**`1B41` is not a role.** It is the XROUT letter header: **serial `0x1B`, service `0x41`**, where
`0x41` is `XSLET`, the connect letter (`FaConnectLetter.XsletService`, and the same reading in
`XmsgFrame` and `XmsgDataFields`). Calling it a role byte in the previous entry was wrong.

**So D100's reply is not a raw bounce.** `1B02` is our serial `0x1B` ECHOED, with service `0x02` -
a structured answer to our letter, in the same shape as the append-remote-batch reply that echoed
our serial. The endpoints being rewritten to us is what an answer addressed back to the asker looks
like, not evidence of a loop.

**A tempting wrong answer, checked and killed before it was published.** The registry's XROUT error
table has `XRUNN = 2`, "nobody here has registered that name - either the service is not running or
the name is spelled differently", MEASURED. It fits perfectly. It is also almost certainly the wrong
table, because `LIST-NAMES` on D100 shows the name present:

```
   100    11      30     *FA-SERVER.
```

Registered, port 11, thirty free session points. **The service is running.** An error table that
happens to contain the value 2 is not evidence that this 2 came from it - the same class of mistake
as reading the Subaddr column, and it is the ninth in this investigation.

**What is now solid:** our letter is `serial 0x1B / service 0x41 (XSLET)`; D100 answers `serial 0x1B
/ service 0x02`; the name it asks for is registered on D100; and our transfer driver silently drops
that answer.

**The exact next lookup:** what service `0x02` means in an XSLET ANSWER. Not the XROUT error table
unless something independent says so. `FaConnectLetter` and the FA status codes are the place to
start, then `XMSG-SYMBOL-L03.SYMB`.

**And the change worth making regardless:** the driver must LOG a frame addressed to its own port
that it cannot match, instead of dropping it. Every wrong turn in this investigation traces back to
an answer that was never shown.

### THE BEST LEAD YET: D100 BOUNCES our connect letter back, both endpoints rewritten to us

Pre-registered experiment, run because seven readings had failed: with the connect-letter retry in
place the remembered seed is safe to use, so a PUSH can be the FIRST conversation after a clean
bring-up, with no console command speaking first. That variable had never been isolated - the earlier
"first conversation" test used two LISTINGS, which exercise only our SERVER half.

**D100 answers.**

```
[RX] ReachabilityReply f1=0xFFFF     our announce, answered
[RX] Ack               f1=0x0000     our connect letter, ACKED
[RX] Data              f1=0x0000     and a reply
```

**And the reply is our own letter, sent back:**

```
header      dst 4E1F (us)   src 0064 (D100)
sub-header  src 4E1F:0211   dst 4E1F:0211        BOTH endpoints are US
body        *FA-SERVER  D100                     our connect letter, verbatim
role        1B02                                 we sent 1B41
```

**D100 returns the letter with both system/port pairs rewritten to ours.** Our driver does not
recognise it, so the ladder stops with no error - which is exactly why every failure looked like
"no answer" when the peer was in fact replying. **We have been calling a bounce a silence.**

**What this changes.** The whole client-half hunt assumed nothing came back. It did. The question is
no longer "why is D100 ignoring us" but "what does this bounce MEAN, and why does our driver drop
it". Those are answerable: the role byte differs (`1B02` against our `1B41`), and a returned letter
with both ends set to the sender is the shape of an undeliverable-to-that-server bounce.

**Next, in order:**
 1. Find `0x1B02` against `0x1B41` in the sub-header role/flags - the enums are in the registry and
    the kernel symbols, so this is a lookup rather than a guess.
 2. Make the driver LOG unmatched frames on its own conversation instead of dropping them silently.
    That single change would have shown this hours ago, and it is worth having permanently.
 3. Only then ask what the bounce is telling us about `*FA-SERVER` on D100.

**Also corrected:** my refutation of the first-conversation pattern was too broad. Two listings back
to back do work, so conversation ordering does not matter for our SERVER half - but that says nothing
about the CLIENT half, and the client letter WAS accepted here as the first conversation after a
bring-up. Whether ordering matters for the client is still open, and this run is one data point.

### RETRACTED: the link was NOT in Call during the failures. I measured it with the runner dead.

The section below claims the client-half refusal was D100's link stuck in `Call`. **It is wrong, and
the mistake is in the method, not the reading.** I ran `LIST-LINKS` AFTER killing the runner - so of
course the link showed `Call`: there was nothing on the other end to complete SABM.

Checked properly, with a runner connected:

```
 1 152164  Run 19999 40RR 40RR 1362  10/Off   0  0   66/8/66
```

**`Run`, bound to 19999, the moment our runner attaches.** The link recovers by itself; `Call` is
simply the idle state between runs. `START-LINK` on that LUN is refused as already in use, which is
consistent - there was never anything to repair.

So the link was almost certainly `Run` during every failing push, and the link state does NOT explain
the refusal. What the section got right is only the narrow part: our "Active" refers to the TCP
bridge and is not evidence about the peer. That remains worth knowing and is not the same as the
conclusion drawn from it.

**The method error, stated so it is not repeated:** the failure was observed with the runner RUNNING,
and the diagnosis was taken with the runner STOPPED. A measurement of a different state than the one
being explained is not evidence about it. That is the seventh wrong answer tonight and the second
caused by measuring the wrong moment rather than the wrong thing.

**Status: the client half is unexplained again**, with the link state now eliminated alongside the
frame content, the seed, conversation ordering, single-threading and the one-shot race. The one
solid, repeatable fact remains: D100 accepted a connect letter at 06:00 and has refused every one
since, byte-identical, four in a row, on a link that is `Run`.

### SUPERSEDED - retracted above

### SOLVED: the "refusal" was D100's LINK sitting in Call. Our log says Active and means nothing.

Asked the machine instead of the code, and D100's own tables answer the whole client-half mystery.

```
System table status: 512 entries. 2 in use.
Address  Sysno  State    Link Subaddr  Send-receive
1211535  19999      4       0       0    27      19        <- link address 0: NO LINK

Link table status: 4 entries. 1 in use.
No  Addr. State Sysid Rcv Xmit  Lun Timeout  Soft-stat-hard TXData/Retry/RXBad
 1 152164 Call     0    0 SABM 1362  10/  7       0       0         61/4/56
```

**D100's only link is in `Call`, still sending SABM, with Sysid 0.** It is DOWN at the LAPB level, so
D100 cannot answer anything - and the system table shows node 19999 with link address 0, no route
home. Nothing above that layer was ever going to work.

**Our runner reported the link Active the whole time.** That status means the TCP BRIDGE accepted our
connection; it says nothing about whether the ND on the far side has completed SABM. Two different
things with one name, which is why every measurement above it looked so contradictory: a link that is
up for us and down for the peer explains a peer that receives our frames and never replies.

**It also explains the sequence of the whole night.** It worked at 06:00 when the link was `Run`. It
fell into `Call` after a runner was killed mid-session - the TCP drop leaves D100 re-SABMing - and
every attempt after that was made over a half-open link. The retry could not help, the seed could not
help, and the frame was byte-identical because the frame was never the problem.

**THE RULE, and it is cheap:** before diagnosing anything above the link, read `LIST-LINKS` on the
peer and check the state is `Run`, not `Call`. `Call` means it is still trying to establish. Our own
"Active" is not evidence about the peer. The cure is `START-LINK,1362,,,-1,,` in `X-C`, the same
command the post-restart bring-up needs.

**And it names the real defect on our side:** killing the runner drops the TCP connection and leaves
the peer's link half-open, which no amount of restarting OUR process repairs. That is worth fixing -
a clean shutdown that lets LAPB close - and it is why "restart the runner and try again" kept making
things worse rather than better tonight.

### THE RETRY WORKS - and it proves D100 REFUSES our connect letter, four times over

Built the bounded connect-letter retry, instrumented the pump when it appeared not to fire, and got
a far better answer than a working push would have given:

```
06:58:37  SendConnectLetter: 1 frame(s) sent
06:58:42  no answer to the connect letter - sending it again (attempt 2 of 4)
06:58:47  no answer to the connect letter - sending it again (attempt 3 of 4)
06:58:52  no answer to the connect letter - sending it again (attempt 4 of 4)
06:58:57  GIVING UP: node 100 answered none of 4 connect letters.
```

**Four identical letters, five seconds apart, on a link that is up and carrying our server's traffic
in the same window. Every one ignored.**

**That kills the race.** Last turn's reading - a one-shot letter arriving before the peer was ready -
predicted that retrying would eventually land one. It does not. The refusal is persistent, not a
timing accident, so it was never about when the letter was sent.

**What the retry is still worth keeping for:** it turns an invisible stall into a statement. Before
it, a failed push looked like a healthy runner that had simply gone quiet; now it says how many times
it asked and stops rather than flooding. That is the difference between a diagnosable failure and a
mystery, and it cost one small bounded loop.

**Two mistakes made building it, both the same shape as the bug itself:** the "peer answered" flag
was first set from ANY frame on the link - including the peer's own requests to our file server - and
then from any frame after the letter, which those same requests also satisfy. It has to mean
"answered US", which needs the destination port checked against our own. A flag named for one thing
and set by another is exactly how the original defect hid.

**Where this leaves the client half, honestly:** D100 accepted our connect letter at 06:00 and has
refused every one since, including four in a row on a healthy link, with the frame proved
byte-identical to the accepted one. That is not a race, not the content, not the seed, not
conversation ordering, and not single-threading. It is something about D100's state that changed
between 06:00 and 06:04 and has not changed back - and NOTHING currently in this register explains
it.

### THE REAL GAP: the connect letter is sent ONCE and never retried

Found by making things worse and looking at why.

Opening the link from the remembered seed let a transfer originate against a silent peer. It also
BROKE the working path: the driver fired its connect letter at a D100 that was not ready, D100
ignored it, and because the letter is sent **once with no retry**, the transfer was then dead. The
early-origination call is reverted for that reason; the store wiring stays, because persisting and
loading the seed is correct on its own.

**But the revert does not fix the timing, and that is the important part.** `CanReach` is simply
"is there a link", and a link is created by ANY inbound frame - including the `InitializationNak`
D100 sends in answer to our announce. So the driver can originate early with or without the seed
call. The pull that worked at 06:00 was not gated by anything: the console trigger simply happened
to land first.

```
06:41  runner up, INNAK arrives, link exists -> [pull] SendConnectLetter: 1 frame(s) sent
       ... ignored, and nothing is ever sent again
```

**So the client half is not blocked by a protocol mystery. It is blocked by a one-shot connect
letter racing the peer's readiness.** Everything else observed tonight follows from it: the letter
is byte-identical to a working one, it is ignored whether the peer is busy or idle, and success came
only when the timing happened to be right.

**The fix is a retry, and it is specific:** re-send the connect letter on a timer until the
conversation is confirmed or a bounded number of attempts is spent, the way the accept-resync already
gives up rather than storming. Then originating early becomes safe, the remembered seed becomes
useful, and the daemon can address a quiet peer - which is the whole point of task #33.

**Not claimed:** that a retry makes every push succeed. It removes the race that explains every
failure seen tonight; whether anything else is behind it is unknown until it is run.

### FIXED: the link-seed store was never wired on the HDLC path - and the hypothesis it unblocked is DEAD

**The defect.** `SeedStore` was assigned in ONE place, inside the Ethernet seam. Over the HDLC bridge
it was never attached, so `OpenLinkFromRememberedSeed` always returned false and
`xmsg-link-seed.state` - which has held `100=5B` since 11 August - sat on disk unread. Exactly the
same shape as `--announce-restart` being dropped on this path: a capability that exists, is
persisted, and is silently never consulted.

Wired now, and it works: a push starts against a peer that has said nothing.

```
[push] sending bigpush.txt (20400 bytes) to "QUIET2:TXT" in 10 block(s)
[push] SendConnectLetter: 1 frame(s) sent          with NO console activity at all
```

That is a real capability gain on its own - a daemon could not previously originate to a quiet peer
over HDLC, which is most of the point of a daemon.

**And it kills the hypothesis it was built to test.** Against a completely idle D100, with no
listing conversation open and nothing typed at any console, our connect letter is STILL ignored.
D100's file access is therefore NOT single-threaded per peer, and the overlap with a listing was
never the cause.

**Where that leaves it, stated without a story:**
 - our client connect letter is byte-identical to the pull's that worked
 - it is ignored whether D100 is busy or idle
 - the same code pulled a file successfully at 06:00 and has not originated successfully since
 - the one thing still different about that success: it went out immediately after D100 had spoken
   to us. Whether that matters is NOT known, and it is not being written up as though it were.

**Four hypotheses have now been killed by measurement in this register tonight** - the session port,
the Subaddr overflow, the first-conversation pattern, and the single-threaded peer. Each fitted
every observation at the time. The client half is still open, and it is worth more to say that
plainly than to offer a fifth.

### MEASURED: our push connect letter is BYTE-IDENTICAL to the pull's that worked

An offline diff of two logs, no lab needed. Our FA client connect letter to `*FA-SERVER` on `D100`,
from the pull that succeeded and the push that was ignored:

```
pull (worked):  2100 86E4 0064 0000 4E1F 0212 0022 1B41 0012 FF0A *FA-SERVER FE04 D100 07E2 ...
push (ignored): 2100 86E4 0064 0000 4E1F 0212 0022 1B41 0012 FF0A *FA-SERVER FE04 D100 07E2 ...
```

Identical - same source port `0x0212`, same sub-header, same body, same target name. The ONLY
differences in the whole frame are Flags1 (`0001` against `0004`) and the checksum that follows from
it.

**So the push's letter is not malformed, and the content is not the variable.** That closes off the
entire "find the wrong byte in our client frame" line, which is where this would otherwise go next.

**UNTESTED hypothesis, recorded as such.** The one thing that differed in the runs themselves: in the
working pull the console trigger had COMPLETED (`FILE 0 : ...` printed) before the pull's letter went
out. In every failing push, the letter went out while D100's listing conversation with our server was
still open. That would mean D100's file access is single-threaded per peer and will not open a
conversation as client while serving one. It fits every observation, and it is exactly the kind of
tidy story that has been wrong three times tonight, so it stays a hypothesis until it is run.

**Why it could not be tested here:** isolating it needs the push to start WITHOUT a console trigger,
and the push driver waits to be spoken to before it will originate - the remembered link seed
(`xmsg-link-seed.state`, and `OpenLinkFromRememberedSeed` on the host) is not consulted on that path.
Wiring the push driver to use the remembered seed is a small change and it makes the experiment
possible: start a push against a quiet D100, with no console activity at all, and see whether the
letter is answered.

**Status unchanged and stated plainly:** our server half is proved working; our client half worked
once and the reason it has not since is still open.

### RETRACTED, by the experiment it asked for: later conversations work fine

The section below claims the first FA conversation after a bring-up works and later ones do not. It
proposed its own test - clean D100, one runner, two conversations back to back, no restart between -
and that test REFUTES it:

```
@LIST-FILES D19999(SYSTEM).SERVED:TXT,,
FILE 0 : D19999.(PACK-ONE:SYSTEM)SERVED:TXT;1
@LIST-FILES D19999(SYSTEM).SERVED:TXT,,
FILE 0 : D19999.(PACK-ONE:SYSTEM)SERVED:TXT;1
```

Both succeeded. Our log shows ONE connect letter and nine replies, so D100 reused the conversation
for the second listing rather than opening another. Conversation count is not the variable.

**What the failing runs actually had in common, and it is the untested thing:** the runner was
running a **PUSH** - us as CLIENT to D100's file server - while also serving. The listings that work
exercise only our SERVER half. Every failure this session was our client half failing to get its
connect letter answered, and D100's repeated connect letters were its retries of a conversation that
had gone wrong, not evidence about ordering.

So the honest split is by ROLE, not by ordering:
 - **our server half: WORKING** - proved twice back to back just now, and by the byte-identical
   round trip earlier.
 - **our client half: worked once (the pull at 06:00), then not again.** That is the open question,
   and it is narrower than anything the retracted section said.

**Why the wrong pattern looked so convincing:** the successes and failures really did line up with
first-versus-later, because the pushes all came after a listing. Correlation across five runs, and I
published it as the pattern behind years of trouble. The rule I keep having to relearn: a pattern
found by looking ACROSS runs is a hypothesis, and it needs its own controlled test before it goes in
as a finding - which is exactly what the next-measurement note existed to do, and what should have
been done BEFORE writing it up rather than after.

### SUPERSEDED - refuted above

### THE PATTERN, at last: the FIRST FA conversation after a bring-up works, later ones do not

Every success tonight was a FIRST conversation after a fresh bring-up. Every failure was a LATER
one. That was invisible while each run was treated on its own.

**The evidence, from one runner instance, no restarts in between:**

```
06:00  pull BIGPSH3:TXT           WORKED - 20400 bytes, SHA256 identical
06:04  push, same build           connect letter sent, never answered
06:13  our FA server answers D100's connect letter with a ConnectionConfirm ... 3 times
       [fa] connect letter from node 100 (port 0x04B4) ... ConnectionConfirm 07D2000200826400
       [fa] connect letter from node 100 (port 0x045E) ... ConnectionConfirm 07D2000800836400
       [fa] connect letter from node 100 (port 0x04B4) ... ConnectionConfirm 07D2000400826400
```

D100 re-sends its request three times, about a minute apart, while we answer every one. Meanwhile
our own push connect letter to D100 is never answered. **Both directions stop after the first
conversation.**

**Ruled out this session, each by a controlled run:**
 - `--resync-hard`. Removed it so our counter continued from the persisted `0x0003` instead of
   dropping to zero: identical failure. Not the cause.
 - `InitializationNak`. Present on the runs that WORKED too.
 - The trigger method. The same `LIST-FILES` trigger produced the working pull.

**What this reframes.** The long-running "our reply is field-perfect and D100 ignores it" hunt was
never about the frame - the same frame is accepted as conversation one and ignored as conversation
two. That is STATE, on one side or both, and it is the first time the boundary has been this sharp.

**The next measurement, and it is cheap:** bring D100 up clean, then run TWO FA conversations back to
back from one runner with nothing else touching the link, and capture both. The difference between
the accepted first and the ignored second is the whole answer. Do NOT restart anything between them
- that is what has been hiding the boundary all along, because a restart makes the next conversation
a "first" again and it works.

**Honest status:** #23 and #33 are NOT re-verified on the current build. They are also NOT shown
broken - #38 re-ran byte-perfect on this build minutes earlier, as conversation one.

### NOT RE-VERIFIED THIS SESSION: #23 and #33 on the current build - and the reason is my own method

#38 was re-run and passed (SHA256 identical). #23 (push under a new name) and #33 (the sync daemon)
were attempted on the same build and did NOT complete. **The cause is test choreography, not the
code, and saying so plainly is the point of this entry.**

**What happened.** The push needs D100 to address us first, so a `LIST-FILES` at the console is used
as the trigger. Killing our runner between attempts - which I did, to restart it with new arguments -
leaves D100's console STRANDED inside an open file-access conversation whose server has just
vanished. Everything after that looks broken:

 - the console stops accepting commands, so the next trigger is never typed at all
 - the push sits with only its connect letter sent, because no seed ever arrives
 - `InitializationNak` (subtype `0x17`) appears and looks like a smoking gun

**`InitializationNak` is a red herring here.** It is already recorded that both peers NAK our INIT
while reachability still works, and that it is not caused by frame contents or node number. It was
present on the run that SUCCEEDED too.

**The rule this gives:** do not kill the runner while the peer has a conversation open with it. Let
the exchange finish, or wait out D100's file-access timeout, before restarting. The stranded console
recovers on its own - `NO ANSWER FROM REMOTE SYSTEM; FILE-ACCESS CONNECTION ABORTED` - in about half
a minute, and no restart of the machine is needed.

**Status, stated honestly:** #23 and #33 remain verified from their original runs and are NOT known
to be broken - the pull re-verification minutes earlier exercised the same client code path on the
same build and was byte-perfect. What is missing is a clean re-run of those two, which needs one
trigger per attempt and no runner restart in between.

### LAB BRING-UP AFTER A D100 EMULATOR RESTART - two steps, or nothing works

Learned by restarting D100 and watching everything fail in a way that looks exactly like a protocol
bug. After the emulator comes back:

```
X-C:LIST-LINKS
Link table status: 4 entries. 0 in use. Max 0 used.      <- NO LINKS AT ALL
```

Our side reports a healthy LAPB connection to the TCP bridge, so the log looks fine, and D100
answers file access with `NO ANSWER FROM REMOTE SYSTEM`. Two commands fix it:

```
X-C:DEF-REMOTE,,D19999 19999      Ok      the restart CLEARS every remote name
X-C:START-LINK,1362,,,-1,,        Ok      the restart leaves the link table EMPTY
```

Then restart our runner (its LAPB predates the reboot) and the pull runs first time.

**Note what this confirms and what it does not.** `DEF-REMOTE` now answers `Ok` where before the
restart it answered "Another port already has this name" - so names really are cleared by a restart,
exactly as the restart notes say. That does NOT resurrect the stale-name theory: before the restart
the name was present and correct, and file access failed for the quoting reason, which is settled.

**#38 RE-VERIFIED on the current build** - after every change made tonight (the announce reaching the
HDLC path, the setup frames originating, the 16-bit system number, the derived accept trailer):

```
original bigpush.txt : D523BDDC33C913F5E7C1CCDAA92B78DC2A329987BEA61EC4058B34758D5AEBDA
pulled off D100      : D523BDDC33C913F5E7C1CCDAA92B78DC2A329987BEA61EC4058B34758D5AEBDA
```

20400 bytes, identical. The client path has NOT regressed - worth checking, because every change
tonight was on the server side of shared code and none of them had been exercised as a client since.

### TASK #34 DONE: two real ND terminals talking to each other through our chat server

```
Terminal 1 (TAD:1, ronny)                    Terminal 2 (TAD:2, system)
# chat join                                  # chat join
you are in the room as ronny                 you are in the room as system
system joined
# chat say hello from terminal one on a real ND
<ronny> hello from terminal one ...          <ronny> hello from terminal one ...
<system> and terminal two hears you          # chat say and terminal two hears you
```

Both directions, plus the join notification. Two TAD sessions (`tty1`, `tty2`) from node 100,
carried over XMSG to a chat room running on Windows.

**The rig, which was the only thing actually missing:** `RetroCore.ini` declares
`device add TERM 8/9/10 --port=9010` - three terminals behind ONE port - so a second and third
telnet connection to `localhost:9010` land on free terminals. The note that #34 was "blocked on the
seam serving ONE ethernet peer" was wrong; nothing needed to change in the seam.

**A trap worth writing down:** a terminal sitting in an established `CONNECT-TO` cannot be recovered
by ESC - that goes to the REMOTE shell, not the local one. The banner names the way back
(`LOCAL CHARACTER IS : 0`), and when the remote runner has been killed the session simply strands
until its idle timeout. Open a fresh terminal rather than fighting a stranded one.

### CONFIRMED LIVE: a real ND user is logged into our TAD server

The origination fix is verified on the machine. `CONNECT-TO D19999` from D100:

```
=== CONNECTION ESTABLISHED ===
    TAD LOGICAL UNIT NO: 770
    LOCAL CHARACTER IS : 0 (ascii value)

 05.39.15     17 AUGUST   2026
 Emulated TAD server version v0.0.1
--- HOST ID:19999 TAD:1 ---

ENTER ronny
OK
# help
----- COMMANDS -----
  1-4     time/date/echo/disc
  stat    session info
  who     list users
  tell N  msg a user
  wall    broadcast txt
  list    servers|service|route
  chat    join|say|who|nick|part
  help    this list
```

Our MOTD, our host id, our login, our shell - on a real ND's console. `TAD LOGICAL UNIT NO: 770`
is `768 + 2`, our LUN index byte, exactly as the code's comment predicted.

**The frames that did it:**

```
before:  0000, 0001, 0000     the echo made the DUMM repeat the accept
after:   0000, 0001, 0002     our own counter, like the real server's 012f/0130/0131
```

**The "D100 stopped answering" scare was my own terminal.** The console was still sitting inside a
hung CONNECT-TO with no `@` prompt, so everything typed went into that program instead of the
monitor, and both FA and TAD looked dead. ESC broke out and the very next attempt succeeded. A dead
peer and a busy console look identical from the log side - **check for the prompt before concluding
the machine has stopped talking.**

**Task #34 is unblocked:** the chat commands are reachable from a real ND terminal. What remains is
the two-user run itself, which needs a second terminal into the same server.

### RESOLVED: the TAD setup frames ORIGINATE - they do not echo. Task #32's capture already exists.

The code at the port assignment says the 2026-08-09 measurement "does NOT separate echo from own
counter that happens to line up" and asks for a capture where the two sides' counters have diverged.
**That capture is in the archive.** `conn-to-d102-from-100.pcapng`, a real ND answering this rung:

```
client 100:   connect 00f8    session-setup 00f9    reply 00fa
server 102:   accept  012f    port-assign   0130    DUMM  0131
```

The accept answers a connect letter numbered `00f8` and goes out as `012f`. The port assignment
answers `00f9` and goes out as `0130`. The counters are 0x37 apart, so nothing "lines up" - **all
three server frames are its own consecutive numbers.** Decisive, and it had been sitting in the
capture folder the whole time the question was open.

**What the echo cost us, measured:** our three frames went out `0000, 0001, 0000`. The echo pulled
the port assignment to the asker's `0001` while our own counter sat at zero, so the priming DUMM
REPEATED the accept's `0000`. A repeated number is dropped in silence - which is exactly the
observed failure: D100 acknowledged all three frames at the link layer and its CONNECT-TO program
then did nothing, because the DUMM that should start its TMOD/TTYP negotiation never landed as a new
datagram.

Both sites now originate. 80 tests pass. Fixing the port assignment alone moved the frames to
`0000, 0000, 0001` - still a duplicate, because the accept was echoing too - which is why both had
to change.

**HONESTLY NOT CONFIRMED LIVE.** After the second change D100 stopped sending the connect letter at
all: zero frames, where before there were four. That letter is D100's FIRST frame, sent before we
transmit anything, so our change cannot be its cause - the likely reason is D100 state after five
CONNECT-TO attempts whose sessions were never released. But "cannot be the cause" is reasoning, not
a measurement, and the fix therefore stands on the capture evidence alone.

**To confirm it, next session:** clear D100's held TAD sessions (or restart XMSG there, notes and
traps in the restart script), then one clean CONNECT-TO. Expect our frames to run `0000, 0001, 0002`
and D100 to answer the DUMM with its TMOD/TTYP chain, as `conn-to-d102` frame 26 shows a real client
doing.

### FIXED: our port assignment truncated the system number to eight bits

Carved by putting our frame beside a REAL server's at the same rung. `conn-to-d102-from-100.pcapng`
has D100 connecting to D102 - the same shape as our case, with a real ND as the TAD server - so its
answer to the session-setup is the oracle:

```
real (D102):   07 05 | 00 | 00 66 | 04 c2      system 0x0066 = 102
ours (19999):  07 05 | 00 | 00 1F | 02 11      system 0x001F = 31     WRONG
```

The parameter is tag, length five, a zero, the system as TWO bytes, then the port. We wrote it with
`byte sysByte = (byte)transport.NodeNumber`, which is right for every node in every capture - 102 is
0x0066 and fits in a byte - and silently wrong for ours, because 19999 is 0x4E1F and truncates to
0x1F. **We were telling D100 the session lived on system 31.**

Fixed, and verified on the wire: `07 05 00 4E 1F 02 11`. 80 tests pass.

**A defect that only exists above node 255**, which is why years of captures never showed it and why
it survived every test - there is no test node big enough to catch it. Worth remembering as a shape:
a field carved from captures of small numbers will encode the small case correctly and hide the
truncation until a real system exceeds it.

**It did NOT unblock CONNECT-TO.** Same stall, same four frames. So it was a real bug on the path
and not the one that stops the exchange - both facts are worth having.

**The next difference, from the same diff, untested:**

```
real:  ... 1f 03 4c 00 00 00 0b 02 03 04
ours:  ... 1F 03 4C 00 00 00 0B 02 03 02 15 02 01 08 FF 00
                                      ^^ LUN index: real 04, ours 02
```

The LUN index byte differs, and our trailer carries an extra `15 02 01 08` block the real one does
not have at that point. Both are worth checking against the full real frame - my dump truncated it -
before changing anything. Diff the WHOLE frame, not the first 72 characters, which is how the system
number was found in the first place.

### SUPERSEDED - the accept-trailer lead, tested and negative below

### NEGATIVE RESULT: deriving the accept trailer does NOT fix CONNECT-TO

The obvious next move was made and it failed, which is worth as much as a success and is cheaper to
record than to repeat.

We emitted `01 02 0000  02 02 000A` (0 in use, 10 free) unconditionally. That is untrue the moment we
hold a session, and tonight it was untrue exactly when the exchange stopped - the log shows the
session opened one millisecond BEFORE the accept was built. So the count is now derived from the
live session table, and the wire shows what it should:

```
before:  0102 0000  0202 000A     0 in use, 10 free   (a lie while tty1 was open)
after:   0102 0001  0202 0009     1 in use,  9 free   (true, and a pair D100 itself sends)
```

**CONNECT-TO stalls in exactly the same place.** Same version line, same silence, same four frames.

**So the trailer is not the cause.** The note that suggested it also said, correctly, that it was
never shown D100 reads the field at all - and it does not, or not in a way that changes this.

**The change is kept**, because emitting a true count is right regardless of whether the peer reads
it, and because the next person to look at that frame should not have to re-derive that it is live
state. 80 tests still pass. But it is recorded here as a FIX THAT DID NOT FIX ANYTHING, so nobody
tries it twice.

**What is still true and still the frontier:** the session opens, D100 acknowledges every frame we
send, and then its CONNECT-TO program does nothing. Two fields remain different from a real accept
and are still unexplained - Flags1 (D100 sends the asker's `0021`, we send `0000`) and the session
port (D100 `0215`, ours `0156`). The Flags1 one is explicitly flagged as a trap in symmetric-history
sessions, so it needs a capture where the two sides' counters have diverged before it can be tested
at all - which is task #32, not a guess to make now.

### SUPERSEDED - the trailer lead, tested and negative below

### ADVANCED 2026-08-17: CONNECT-TO now opens a TAD session; it stalls at the accept trailer

With the same two cures that fixed file access (a landed announce, correct syntax), `CONNECT-TO
D19999` from D100 gets further than it ever has:

```
COSMOS CONNECT-TO PROGRAM VERSION - E02 , September 25, 1987     on D100
[tad] session opened: tty1 from node 100                          on ours
```

Both Data frames from D100 arrive, both our replies are ACKed, and then it stops. D100's screen
sits after the version line and no banner appears.

**The stall point is exactly where the known best lead sits.** Our accept ends:

```
... 0002 0202 000A
```

which is the hardcoded `0/10` accept trailer. It is already recorded that this trailer is NOT
constant - D100 has been captured sending both `0/10` and `1/9`, so it is live state we are
guessing. Tonight pins that guess to a specific stall: everything before it is accepted, and
nothing after it happens.

**Next, and it is now a narrow job:** derive the trailer instead of hardcoding it. The two captured
values and the frames around them are in the ARCHIVE captures; the pair looks like a count and its
complement to 10, so read what D100 sends in the frame we are answering rather than inventing it.

**Not claimed:** that the trailer IS the cause. It is where the exchange stops, which is a much
better place to stand than "somewhere in the connect ladder".

### SOLVED 2026-08-17: D100 reads and writes files on OUR server. The blocker was QUOTES.

**Proved end to end, both directions, byte-identical:**

```
@LIST-FILES D19999(SYSTEM).SERVED:TXT,,
FILE 0 : D19999.(PACK-ONE:SYSTEM)SERVED:TXT;1        <- D100 lists OUR file

@COPY-FILE "FROMND:TXT" D19999(SYSTEM).SERVED:TXT    <- D100 READS from us
@LIST-FILES FROMND:TXT,,
FILE 93 : (PACK-ONE:SYSTEM)FROMND:TXT;1

@COPY-FILE D19999(SYSTEM)."ROUNDTRIP:TXT" FROMND:TXT <- D100 WRITES back to us
```

```
8423924070271d18...  SERVED.TXT
8423924070271d18...  ROUNDTRIP.TXT      byte-identical
```

Windows -> D100 -> Windows, 92 bytes, SHA256 equal. The FA ladder ran clean through
ReserveFileEntry, the content transfer and ReleaseFileEntry, with D100 acknowledging every reply.

**THE CAUSE WAS THE QUOTING, AND IT WAS IN OUR NOTES ALL ALONG.**

```
@LIST-FILES "D19999(SYSTEM).SERVED:TXT",,   ->  UNKNOWN REMOTE SYSTEM NAME
@LIST-FILES  D19999(SYSTEM).SERVED:TXT,,    ->  works
@COPY-FILE "D19999(SYSTEM).ROUNDTRIP:TXT" . ->  "ROUNDTRIP:TXT"" UNKNOWN REMOTE SYSTEM NAME
@COPY-FILE  D19999(SYSTEM)."ROUNDTRIP:TXT" ->  works, file created
```

**Quoting the WHOLE remote filespec destroys the system part.** The error echo `"ROUNDTRIP:TXT""`
shows SINTRAN keeping only the tail. The quotes belong around the FILE NAME ONLY, inside the
system/user prefix, and only when creating:

```
    D19999(SYSTEM)."NEWFILE:TXT"        correct
   "D19999(SYSTEM).NEWFILE:TXT"         breaks name resolution
```

This CORRECTS the existing rule "quote the whole filespec", which is right for LOCAL files and
wrong for remote ones. Every `UNKNOWN REMOTE SYSTEM NAME` in this register came from that.

**The second half of the cure was ours:** `--resync-hard` had to actually reach the HDLC path. It
was parsed, handed to the Ethernet seam only and silently dropped, so the announce that resets
D100's expected-from-us never went out. With the announce landing, our first frame at Flags1 0x0000
is in step; without it D100 answered the ConnectionConfirm with XENSE (`FFDE`, sequence error) - the
one time the quoting was right and the sequence was not, which is exactly the frame that named the
remaining fault.

**Working recipe, both halves needed:**
```
Xmsg.Live.Runner.exe --self 19999 --announce-restart --resync-hard --trace-frames
@COPY-FILE  <local> D19999(SYSTEM).<remote>          read from us
@COPY-FILE  D19999(SYSTEM)."<new>" <local>           write to us
```

**What this retires:** the entire "our reply is field-perfect and D100 ignores it" line of enquiry.
It was never the encoder, the checksum, the ports, the sequence law, the node number, RX-BAD, the
name table or the access flags. It was a pair of quotation marks in the command I was typing, plus
one flag that never reached the code path it named.

### DECODED from the X-C binary: the "Access" column is direction + friend, NOT permissions

Carved out of `DOC/COSMOS-RE/ENNS0-Startup-RE-2026-07-23/xmsg-L-binaries/XMSG-COMMAND-L03.PROG`
(the X-C program, 210373L) rather than guessed. The formatter, at file offset `0x29AA`:

```
"  *", 5SREC,$B0,"--","<=" ;  5SLEC,$B0,"--","=>" ;
      (XSSID=T3,"O",<>), (XSSID><T3,FXSEX4,<>)
      5SFRI,$B0,"P","F" ;
```

and the literal table of the four-character middle field, at `0x8ADA`:

```
'----'   '--=>'   '<=--'   '<==>'   '????'
```

**So `*<=--P` decodes as:** `5SREC` SET (drawn `<=`), `5SLEC` clear (drawn `--`), not our own system
(so not `O`), and `5SFRI` clear (drawn `P`, for not-a-friend). The bits are positions in the
system-table flag word - `5SFRI` = 8, `5SLEC` = 9, `5SREC` = 10 (octal 10/11/12 in
`XMSG-SYMBOL-L03.SYMB`).

**The column is not an access-permission field at all.** The `P` everywhere is simply "not a friend",
which agrees with `LIST-FRIEND-SYSTEMS` answering "No friend exists" - two independent readings that
now agree, which is the check worth having.

**What the arrows mean, stated at the confidence it deserves:** the pair reads as a
remote-established / local-established connection flag, `<=` inbound and `=>` outbound. That is
inferred from the mnemonics (`REC` / `LEC`) and the arrow directions the program draws, NOT from a
manual - the symbol file gives bit numbers and no prose.

**Which de-escalates the anomaly rather than solving anything.** `<=` being set on every system we
connect as is consistent with us being the side that announces - a remote-initiated contact is what
we are. It is a symptom of our ROLE, not an access denial, and it is very likely not the blocker.
D102 shows `----` because its connection was established some other way.

**Net: one unknown converted into a known, and one suspect cleared.** The blocker is still not
found, and the honest position is that nothing currently on the table explains it.

### SUPERSEDED - the stale-name reading, refuted below

### REFUTED: the name is NOT stale. `LIST-NAMES` shows it healthy and identical to a working peer

Last turn's stale-port explanation is dead, killed by the first read-only command that could test
it. `X-C` -> `HELP` -> `LIST` reveals the full command set, including **`LIST-NAMES`**:

```
System   Port  Free SPs   Name
   100     0             D100.
   102     0             D102.        <- resolves fine, 2974 frames
   200     0             D200.
 19999     0             D19999.      <- identical shape
```

`D19999` is registered on system 19999, port 0 - **the same form as `D102`, which works.** Nothing
is stale, nothing is orphaned, and the refusal of `DEF-REMOTE` is simply the correct answer to
defining a name that already exists. Two answers I called contradictory were both just true.

**So the restart procedure is NOT the next step** - which is the useful part, because it is the risky
one and it was about to be run on the strength of that reading.

**Also read, all empty or irrelevant:** `LIST-NETWORK-SERVERS` (only ENNS0 on 9800),
`LIST-CONNECTIONS` (none), `LIST-FRIEND-SYSTEMS` ("No friend exists" - so friend-system access
control is not what separates us from D102 either).

**THE ONE MEASURED DIFFERENCE LEFT, and it is precise.** In `LIST-SYSTEMS`, the Access column:

```
   102   ...  *----P     <- works
  9800   ...  *----P     <- works
   200   ...  *<=--P     <- us
 19999   ...  *<=--P     <- us
```

Every system we have connected as carries `<=` in positions 2-3 where a working peer has `--`. It
follows US, not the number: 19999 and 200 both show it, and it stays as they swap links.

**What `<=` means is UNKNOWN.** It is not in the Communication Guide, there is no X-C command with
"access" in its name, and it is not the friend list. **This is now the single most specific open
question in the project**, and it is a good one - a two-character difference between a peer that
resolves and one that does not, reproducible on demand.

**Where to look next, in order:** the XMSG kernel source for the system-table access byte and the
routine that formats this column (`XMSG-SYMBOL-L03.SYMB` has the symbols; the L03 decode is in
`DOC/COSMOS-RE/xmsg-kernel-l03-symbols-decoded.txt`), and `LIST-GENERATION-VARIABLES` for whatever
sets it at generation time. Carve it - do not guess it, and do not restart the machine hoping it
clears.

### SUPERSEDED - the stale-port reading, refuted above

Measured 2026-08-17, with everything else known good - link `Run`, bound to the right sysid, our
file server up and serving one file:

```
X-C:DEF-REMOTE,,D19999 19999
      XMSG Routing/Naming error: Another port already has this name
X-C:DEF-REMOTE,,D200 200
      XMSG Routing/Naming error: Another port already has this name
@LIST-FILES "D19999(SYSTEM).SERVED:TXT",,
      UNKNOWN REMOTE SYSTEM NAME
```

**Those two answers together are the evidence.** XROUT says the name is taken; file access says the
name is unknown. Both cannot be true of a healthy registration. A name held by a port that no longer
exists - left behind by an earlier XMSG session - satisfies both at once: the slot is occupied, so a
fresh `DEF-REMOTE` is refused, and it points nowhere, so resolution fails.

This is the leading explanation and it is consistent with everything measured. It is NOT proven -
what would prove it is clearing the name and defining it fresh.

**Things now ruled OUT as the cause, each by measurement rather than argument:**
 - The name being absent. It is registered; `DEF-REMOTE` refuses to add it.
 - The link binding. `LIST-LINKS` shows link 152262 `Run` with Sysid 19999 - our announce rebinds it
   correctly, and it self-heals after being pointed elsewhere.
 - Our node number. 19999 and 200 behave identically here.
 - RX-BAD. Our own restart noise (see above).
 - Our frame encoding. Field-identical to a real machine's, checksum included.

**The next step, and it is documented, not exploratory:**
`SINTRAN\XMSG	ools
estart-xmsg-cosmos.ps1 -Port 9010` restarts XMSG on D100, which CLEARS every
remote name (trap 2 in the restart notes says so in as many words). Then redefine with
`DEF-REMOTE,,D19999 19999` - which should now be ACCEPTED rather than refused, and that acceptance
is itself the confirmation of the diagnosis - and reconnect.

**Read the restart notes before running it.** Trap 3 is a genuine lock-out: `COS-START-E04:MODE`
ends with `SET-UNAVAILABLE`, and splitting the sequence across two connections locks the console.
That is why this is being handed over rather than half-run.

### CORRECTED 2026-08-17 (same night): the Subaddr explanation below is WRONG

The section that follows claims D100's `Subaddr` field cannot hold 19999. **It can.** A later
`LIST-SYSTEMS` on the same machine, after running as node 200:

```
1211557    200      4  152262       0       <- 200 now has Subaddr 0
1211570  19999      4  152164   19999       <- 19999 HAS subaddr 19999
```

`Subaddr` is 0 for whichever system sits on link **152262** - our point-to-point HDLC link, which
has no subaddressing - and carries the system number on link **152164**. It is a property of the
LINK, not of the size of the number. The one table I built a mechanism on was showing me our link
type, and I read it as an overflow.

**What survives, because it was measured rather than explained:**
 - D100 counts RX-BAD frames on our link (0/0/21 climbing) while the real ND on the other link has
   0 bad in 2970. Our frames reach it and are rejected. That is solid.
 - With `--self 200` D100 answered `RR nr=1` instead of `nr=0` and `TXData` went 0 -> 1. Observed
   ONCE, with a confound: the announce wiring changed in the same session.

**What does NOT survive:** the reason. Why the frames are counted bad is UNKNOWN again. The sysid
may still be involved - the one-shot result is real - but the mechanism published for it was
invented from a misread field, and a plausible mechanism attached to a real result is exactly how
the last six theories survived as long as they did.

**Not to be repeated:** two overclaims in one night, both from a single reading of a table I had not
established the meaning of. The rule that catches this is already written down - a count is only as
good as its filter, and a FIELD is only as good as its definition. Establish what a column means
before building on it, ideally by finding a case where it varies for a known reason.

**A candidate for what RX-BAD means, NOT verified by me.** An analysis already in this repo
(`SINTRAN/Devices/HDLC/HDLC-ALL.md`, around the HDLC driver counters) states that RX-BAD is not an
HDLC-driver counter at all: no such counter exists in the driver source. It is incremented at the
LAPB layer and counts DUPLICATE frames - "3 duplicate frames in log = 3 RXBad count".

If that is right, our 21 are probably self-inflicted. Our LAPB starts at `ns=0` on every process
restart while D100's link stays `Run` across them, so each reconnect re-sends a sequence number the
peer already has and is counted as a duplicate. The counter climbed +4 and +7 across restart cycles,
which fits. **That would make RX-BAD a red herring and not the blocker at all.**

Flagged rather than believed: that document sits beside an `archive/to-delete` copy, I have not read
the driver source myself, and I have just spent a night learning what happens when a plausible
reading of one artefact is promoted to a cause.

**The measurement that settles it, and it is cheap:** hold the runner up and send data WITHOUT
restarting. If RX-BAD does not climb, it is restart noise. If it climbs per data frame, it is real.
One run, no theory needed.

**MEASURED, and it points at restart noise.** One more restart, sending exactly ONE data frame
(the announce - the push stalled waiting for D100 to address us first):

```
RX-BAD  32 -> 37     five more bad, one data frame sent
```

Earlier cycles: +4 and +7, also one data frame each. **The counter tracks RESTARTS, not data.** Five
bad frames cannot come from one data frame; they can easily come from a SABM/UA/RR handshake plus a
re-sent `ns=0` that the peer already holds, which is what the duplicate reading predicts.

**So RX-BAD is almost certainly our own restarts, and the "solid" half of the finding above dissolves
too.** It is not evidence that D100 rejects our frames. What is left of the whole night is ONE
observation: with `--self 200`, D100 answered `RR nr=1` instead of `nr=0` and TXData went 0 -> 1.
One data point, with the announce wiring changed in the same session.

Stated plainly: **the blocker is NOT solved and nothing here should be built on.** The register is
back to where it was, minus six dead theories and plus a working announce on the HDLC path.

**Still worth having from tonight, because it is mechanical rather than interpretive:**
 - `--announce-restart` / `--resync-hard` now actually work over HDLC; they were silently ignored.
 - D100's `LIST-SYSTEMS` / `LIST-LINKS` are readable and are the right place to look first.
 - `Subaddr` describes the LINK, not the number.
 - A valid sysid is not a name: `UNKNOWN REMOTE SYSTEM NAME` is a separate table.

**Next, and only this:** find out what D100 calls bad. `LIST-GENERATION-VARIABLES` and the RX-BAD
counter's meaning in the kernel source, not another theory.

### SUPERSEDED (mechanism refuted above, one-shot result kept): our NODE NUMBER was too big

**The symptom, for years of this register:** our replies were structurally perfect - every field,
every flag, the header checksum, the session port policy - and D100 ignored them and re-sent its
request. Six theories died against that wall, the last of them the session port.

**What actually settles it is D100's own tables, which nobody had read.** `X-C` -> `LIST-LINKS` and
`LIST-SYSTEMS` on the machine itself:

```
No  Addr. State Sysid Rcv Xmit  Lun Timeout  Soft-stat-hard TXData/Retry/RXBad
 1 152164  Run  9800    0    0    7  50/Off       0       0       2970/0/0
 2 152262  Run 19999 2065 41RR 1362  10/Off       0       0          0/0/21
                                                                    ^^^^^^^^
```

Link 2 is ours. **Zero data frames ever transmitted to us, and 21 received frames counted BAD** -
while the real ND on link 1 has zero bad in 2970. Our frames were not being silently dropped by a
sequence rule at all. They were arriving and being rejected as malformed at the link layer, which no
amount of staring at our own encoder could have shown.

**The system table names the cause:**

```
Address  Sysno  State    Link Subaddr  ...
1211546    102      4  152164     102
1211535   9800      0  152164    9800
1211557    200      0  152164     200
1211570  19999      4  152262       0     <-- subaddr 0, not 19999
```

Every entry stores its own system number in `Subaddr` - except ours. **The field cannot hold 19999,
so it holds nothing.** Every real sysid ever seen on this network is small: 100, 102, 103, 200, 9800.

**Proved by changing one number.** Same code, same link, same everything, `--self 200`:

```
before (19999):  RR nr=0   TXData 0     our I-frame never acknowledged
after  (200):    RR nr=1   TXData 1     acknowledged, and D100 SENT US DATA
```

`TXData 0 -> 1` is D100 transmitting to us on that link for the first time in the project's history.

**The exact ceiling is NOT known** and is not worth guessing: 9800 fits and 19999 does not, so it
lies somewhere between. Whether it is 14 bits (16383), a 4-digit decimal convention (9999), or a
generation variable, is one `LIST-GENERATION-VARIABLES` away and is now a small, bounded question
instead of an open-ended hunt.

**What this retires:** every "our reply is field-perfect and still ignored" entry in this register.
They were all true and all irrelevant - the frame never got as far as the code that would have cared.

**What is still open:** the remote NAME. With sysid 200 accepted at the link layer, `LIST-FILES
"D200(SYSTEM).SERVED:TXT"` still answers `UNKNOWN REMOTE SYSTEM NAME`, because the name-to-number
table is a separate thing from the routing the announce builds. That is the next step, and it is a
naming question, not a protocol one.

**The lesson, again:** ASK THE MACHINE. The answer was sitting in two console commands on a machine
that has been running all along, while the search went through captures, checksums, ports and
sequence counters. `LIST-LINKS` would have said "21 bad frames" on day one.

### RETRACTED: "a real server uses a fresh session port each time" is WRONG

> The section below was published on 2026-08-17 and is false. It is kept, struck through, because it
> was committed and pushed and somebody will find it.

**What a correct count shows.** Filtering to FA connection confirms only, and to the server that
sent each one, D100 answers from exactly ONE port per capture:

```
claude-delete-file-102-to-100      node 100 -> 0x05AD
claude-create-file-102-to-100      node 100 -> 0x05AD
claude-open-close-file-102-to-100  node 100 -> 0x05AD      (nine captures at 0x05AD)
ALLTEST-fa-connectto-102-100-103   node 100 -> 0x05AE
filenum-8-vs-307-102-to-100        node 100 -> 0x05AE      (two later ones at 0x05AE)
```

**One port per server RUN**, stable across every conversation in it, changing only between runs. That
is exactly what `FaServer._replyPort` already does - allocate once, reuse - and its remarks record
the same thing from three other captures, plus the live failure that caused it to be written that
way: moving the port made D100 ignore eleven confirmations in a row.

**So our port policy is right, and the session port is NOT the difference.** The value differs from
any real server's, but the BEHAVIOUR matches, which is what the peer can see.

**Why the census was wrong, because the method matters more than the answer.** It counted every
port every node ever used, with no filter: client ports, TAD ports, XROUT ports and FA ports, across
captures spanning months and several server restarts. Seventeen "D100 ports" was seventeen different
things. A count is only as good as its filter - a rule already written down here - and it was broken
in the act of claiming a measurement.

**Where that leaves the blocker:** back to unknown. Our reply matches a real one field by field, the
port policy included, and D100 still re-sends the request instead of accepting it. Nothing in a
single frame explains it, so the next thing to look at is STATE - what differs between the
conversation D100 accepts and the one it does not - not the bytes of any one reply.

### ~~MEASURED: a real server uses a FRESH session port each time. We use a constant.~~ (RETRACTED)

A census of every session port in the archive:

| | distinct session ports |
|---|---|
| D100 | **17** |
| D102 | **33** |
| D103 | **11** |
| us | **one per protocol**, hard-coded |

No derivation from the node number, the conversation or anything else in the frame - the values
scatter (`0x0156`, `0x0215`, `0x0288`, `0x02AB`, `0x05AD`, `0x0605`, `0x0763` for D100 alone). They
behave like an ephemeral port allocated per conversation, which is also what a port IS.

Two details sharpen it. `0x0156`, the constant our TAD responder uses, IS used by real machines - so
the VALUE is plausible and that is not the fault. `0x0211`, the constant our FA server uses, appears
**nowhere** in any real traffic.

**The hypothesis this suggests** - and it is a hypothesis, the counts above are the finding: if the
peer keys session state on our port, then reusing one constant means every new conversation collides
with its memory of the last one. That would fit the pattern nobody has explained: the first connect
after a restart working, later ones failing, and "connect twice" helping sometimes and not others.

**The change it implies** is to allocate a fresh port per conversation, as real servers do, and to
listen on the one we advertise. That is a real piece of work rather than a constant swap - we must
own the port we name - and it has NOT been done. It is the first thing to build next, and the
hypothesis above is what it would test.

---

## 0b. Superseded readings of this blocker - all mine, all from tonight

| Read as | Killed by |
|---|---|
| the peer is ignoring our answers | it never received them - they were parked |
| the acknowledgements are stale | the stale-ack detector reported ZERO |
| D100 is fouled and needs a restart | `LIST-LINKS` shows the link in Run, 2913 frames, no retries |
| the send window never opens | a traced run shows it opening and three frames going out back to back |

Four theories, four measurements, each cheap once taken. The pattern is worth more than any of them:
**every one died to a measurement that took minutes, after reasoning that took hours.**

---

## 0c. The old section, kept for its evidence

## 0. THE ONE BLOCKER - three symptoms, one cause

**Our ND-link send window does not open, so nothing leaves this node.** Measured 2026-08-17: the
`PARKED` diagnostic fires 62 times in a single delete attempt, the queue climbing 1, 2, 3, 4, 5, 6
while D100's own link counters show it sending 2913 frames with no retries and no bad receives.

Everything that looked like a separate problem is this:

| Looked like | Actually |
|---|---|
| **#34** - D100 refuses our TAD connect accept | the accept is built, parked, and never sent |
| **B5** - a live delete answers NO ANSWER FROM REMOTE SYSTEM | the delete reply is built, parked, and never sent |
| **F3** - a listing puts D100 into a reconnect loop | the listing replies are built, parked, and never sent |

The peer is not ignoring our answers. **It never receives them.** That also explains why every
"first-connect stall" workaround is unreliable - connecting twice sometimes wins the race to get one
frame out through the unpositioned window of one, and sometimes does not.

### MEASURED 2026-08-17: the window is ONE, and the peer has not placed us

One log line answers it:

```
[TX] *** PARKED *** eth:d100 took a 14B frame but the send window is full; 2 datagram(s) now
     waiting ... window 1, outstanding 1, peer placed us: False
```

`UnpositionedWindow` is **1** and stays 1, because `_havePeerPosition` is false. So exactly one
frame can be in flight, and the SECOND frame of any reply parks. The FA connect and the TAD accept
both need more than one frame promptly, so both stall - not because the peer refuses them, but
because we cannot send them yet.

The acknowledgements do arrive - ten in that run - but AFTER the exchange has already given up. The
one-frame window is deliberate, and its reason is good: a peer carrying state from a previous
session throws away whatever arrives at the wrong sequence, so only one frame should be risked
before it tells us where we are. That safety measure and a multi-frame reply are in direct tension,
and the tension is now measured rather than suspected.

**Two of the three candidates are now settled, from real traffic.** A working ND-to-ND exchange
(`ethernet-conn-to-D100-from-102-WORKING-2026-08-01.pcapng`) is strictly LOCK-STEP:

```
D102 DATA seq=0043      each frame appears twice in the capture - both copies are held
D100 ACK  seq=0044
D100 DATA seq=0043
D102 ACK  seq=0044
D100 DATA seq=0044
D102 ACK  seq=0045
```

One data frame outstanding at a time, acknowledged before the next goes out. Across 48 ND frames the
longest run before the other side answers is one unique frame.

- **"Open the window sooner" is WRONG.** A real ND does not push frames back to back, so a window of
  one is not the fault - it matches what the wire does. It also means `SendWindow = 6` does not
  describe this peer, and where that six came from should be re-examined.
- **"Send one frame at a time" is what already happens**, and is correct.
- **So the fault is that our FIRST frame is not acknowledged.** Parking the second is right; it never
  drains because the first never gets its answer.

**Still UNKNOWN:** why D100 does not acknowledge our first frame, when it acknowledges D102's
immediately in the same shape. The acknowledgements we DO receive - ten in one run - have not been
matched against what we sent, and that is the next measurement.

**Two theories killed on the way, both mine:** the peer is not ignoring our answers (it never
receives them), and the acknowledgements are not stale - the stale-ack detector added the same day
reported ZERO, so an ack that arrives is accepted. It simply arrives too late to matter.

**Superseded, same day.** The acknowledgements are not missing: `[segment]` reports 22 of them
in the failing delete run and 22 in the run where FA exchanges completed. So they arrive and
`TakeAcknowledgement` runs. What is not known is what they SAY - and nothing logged that, which is
why the window looked healthy while it was shut.

`[RX] *** STALE ACK ***` now reports every acknowledgement refused for being behind our edge, with
the number it carried, where we already had the peer, and how many datagrams are parked. Refusing a
stale one is correct; a RUN of them beside a climbing park queue is the deadlock, because the frames
the peer needs in order to move forward are the ones stuck behind the window.

**The next live run reads its own answer** - the two lines together are the signature. What is still
UNKNOWN is whether that is what is happening: it is a shape that fits, not a measurement.

**Do NOT chase** the FA connection number. That theory is DISPROVED and the disproof is recorded on
`FaServer.LastConnectionNumber`: a restart answering with a fresh `0x0082` stalled anyway.

---

## 1. Blocking a task right now

| # | Question | Why it blocks | What would settle it |
|---|---|---|---|
| **B1** | Why does D100 refuse our TAD connect accept? | #34 - the two-user chat run cannot start | **B3 and B4 are what is left.** Eliminated so far, each with evidence: the link layer (7 acks, 7 data frames out), the send window, the datagram sequence, the Flags1 policy (a repeat keeps its number - S3), and the header checksum, which is arithmetically CORRECT on our accept: `~(0x2113+0x000E+0x0064+0x4E1F+0x0000+0x0400) = 0x8C5B`, the value we sent |
| **B2** | ~~Does our accept's Flags1 need to advance per connect?~~ | **Answered 2026-08-17, and the answer is no.** Real responders advance on a NEW connect (`li-syst-tad-103`: D100 `000b`→`000c`), but D100's re-send was a RETRANSMISSION of identical bytes, and a repeat keeps its Flags1. Ours does the same. See S3 | - |
| **B3** | What are the two accept trailer parameters? | We emit `01 02 0000 / 02 02 000A` always; real machines emit that AND `0001/0009` | Still open. A third distinct value would help. The sessions-in-use reading is REFUTED - see S2. It is stable within a capture and differs between captures, so it tracks something slower-moving than a connect |
| **B6** | Does the TAD responder's SUPERSEDED envelope arithmetic hold for node 19999? | Not blocking - it produces the right word 6 today, proven by a new test - but `ComputeCounter` and `DeriveChannel` are documented as reproducing "the traffic they were fitted to and NOT in general", and the fitted traffic was nodes under 256 | `EveryResponderFrameCarriesACorrectChecksumForOurNodeNumber` now guards it. The clean fix is to build word 6 with `ComputeHeaderChecksum` and delete the fitted path |
| **B4** | The ethernet seam serves ONE peer | A two-user chat across D100 and D102 is impossible; D102's connection requests go unanswered | Nothing to measure - it is work: one `EthernetLink` per peer. Try two terminals on ONE machine first |
| **F3** | Why does a LIST-FILES against our FA server put D100 into a reconnect loop? | Blocks driving anything through a listing, which is how B5 was attempted | Six conversations in thirty seconds on 2026-08-17: we issue a new connection number per confirm (0x0042..0x0047) while D100 keeps naming 0x0042. Whether the CLIENT owns that number is not established - a real ND-to-ND listing capture would settle it |
| **B5** | Does FA `DeleteFile` work at all? | #23 marked UNVERIFIED in `SyncActionKind`, `SyncFolderMapping` - the sync daemon cannot mirror a deletion | **CLIENT side DONE** 2026-08-17 - `FaDeleteFileCodec.BuildRequest` reproduces a real ND-to-ND delete byte for byte, 8 tests. The SERVER side was attempted live the same day and D100 answered `NO ANSWER FROM REMOTE SYSTEM` after our connection confirm - see M4. Needs a freshly restarted D100 before another attempt means anything |

## 1b. Superseded readings - do not re-derive these

Traps that look like findings. Each cost real time, twice in one day.

| # | The old reading | The truth |
|---|---|---|
| **S1** | The two bytes after Flags2 are a **channel** and a **Counter**, with `Channel = 0xDE - (XMCSM>>24) - epoch` and `Counter = (seed - Flags1) & 0xFF` | They are the **header CHECKSUM**, high byte then low. `SintranHeader.ProtocolId` and `.Counter` are `[Obsolete]` compatibility views saying exactly this, and the build prints the warning every time. The seed model is not wrong so much as a special case: the checksum depends on Flags1, so it reads as "a constant minus Flags1", and the per-link "seed" IS that constant |
| **S2** | The accept trailer pair is sessions-in-use / free, summing to ten | REFUTED. Two connects to the same responder in one capture keep the same pair |
| **S3** | The responder's accept Flags1 must advance on every connect | Only on a NEW connect. D100's re-send was a RETRANSMISSION of the identical bytes, and a repeat keeps the same Flags1 - which is what we do |

**How S1 bites:** our accept carries `04008C5B` where a real one carries `0400D9F3`, and it is easy
to read that as a wrong channel byte. It is not. Different headers give different checksums, and
ours matching D100's incoming connect is expected rather than suspicious - swapping source and
destination leaves a sum unchanged.

## 2. Wire semantics we guess at

> **These rows mirror `status` fields in `protocols/sintran-wire.json`.** That file is the
> machine-readable original and the conformance test holds the C# to it; this table is the readable
> view. Change the JSON first, then this. A row here with no matching registry entry is a row that
> will rot.

These do not stop anything today, but a wrong guess here produces a fault that looks like something
else entirely.

| # | Question | Where | Status |
|---|---|---|---|
| W1 | The ND link **connection-confirm** byte `0x1F` | `NdLinkLayer.ConnectionConfirmKindUnverified` | **Largely settled 2026-08-17** - D100 answered our CC by moving on to DT, which is the test the comment itself names. The low nibble is still inferred from "every control frame ends in F" |
| W2 | The **low nibble** of the disconnect-request family (`0x6F`) | `NdLinkHeader`, `EthernetLink` | Unverified. Only the by-network-service variant has ever been seen |
| W3 | Every field of a connection confirm except the sender link id | `NdLinkLayer` | Unverified - no real CC has ever been captured, only ours |
| W4 | Is echoing the session token **required**, or only observed? | `IXmsgServerTransport` | Unverified through `0x01FF` |
| W5 | Subtype `0x17` as an init reject | `SintranPacketSubtype` | UNPROVEN - see `DOC/SUBTYPE-17-INIT-REJECT-2026-08-07.md` steps 7-9 |
| W6 | What the FA read-ladder **bit** means | `FaReadLadder` | Only *when* it is set is known, not what it means |
| W7 | The XROUT random generator's seed cell | `XmsgRandomGenerator` | The cell's contents are unverified and cannot be reproduced from the image we have |
| W8 | `XmsgFrameFlags.Letter`, `Marker01`, `SystemMode` per-bit rules | `XmsgFrameFlags` | Deliberately unused - frames mirror whole captured combos. Fine as long as nobody composes bit by bit |
| F1 | What the FA read-ladder bit MEANS | `FaReadLadder` | Only *when* it is set is known. Mirrored in `protocols/fa-qform.json` |
| F2 | QFORM classes 4, 5 and 6 | `QformClass` | Seen in captures, never decoded. Nothing we build uses them, which is why it has never cost anything |
| W9 | Does a real letter arrive as **XMROU**? | `XmsgKernel.ChooseType` | **SETTLED 2026-08-17** from ND's two sample servers. Left here so it is not re-asked |
| W10 | **What in the FA path makes D100's XMSG gateway die with code 27?** | the FA write/read path, `Fa*` | **OPEN, and it is the biggest one.** Six deaths timed from both sides 2026-08-21/22 - see below |

### W10 - the XMSG gateway death, stated properly so it is not re-guessed

**What is MEASURED** (console error device on a logged terminal, both ends carrying wall-clock
stamps - see `captures/XMSG-CRASH-2026-08-22/`):

 - Every death prints `XMSG ERROR CODE: 27` (`XXNER`, *network gateway error*) at
   `PHYSICAL ADDRESS 141204`. Identical every time back to 2026-08-04, so the fault is
   **deterministic**, not a timer on uptime and not "the machine being unstable".
 - **6 for 6, every death follows an FA file transfer** - push OR pull.
 - **The gap has no period**: during-transfer, ~30 s, 4 min 38 s, 4 min 48 s, ~3-6 min, 8 min 39 s.
 - It is **not** a leaked connection seat: `X-C LIST-CONNECTIONS` for 19999 showed BOTH tables
   EMPTY minutes after a completed transfer.
 - It is **not** resource exhaustion: kernel free space read 4273 words in every sample, including
   minutes before a death. It STOPS, it does not degrade.
 - It is **not** the chat server or client - neither was running for any of the six.
 - It is **not** a link restart - four deliberate STOP-LINK/START-LINK cycles produced no fatal.

**THREE THEORIES WERE PUBLISHED AND ALL THREE RETRACTED** in one night. Recorded here because the
failure mode is the useful part:

 1. *"dies ~3 minutes after a link restart"* - killed by four deliberate link cycles.
 2. *"our SABM kills it"* - two deaths followed one by 147 ms and 201 ms, which looked conclusive;
    a third had NO runner and NO SABM (ours came 27 s AFTER it) and a fourth had sent ZERO
    I-frames.
 3. *"a completed push kills it at ~4 m 45 s"* - 4:38 and 4:48 looked like a period until a pull
    gave 8:39.

**The pattern in the reasoning, not the machine: two matching measurements are not a period.**
And a control window must be LONGER than the longest gap already seen - a 7-minute window was run
against an 8-minute effect and reported "survived", while read-only queries were issued during the
wait. Both mistakes are cheap to avoid and were not.

**WHAT TO DO NEXT, and it is not another stopwatch:** `XMSGDUMP-A` (segments 33/34), `-T` (tables)
and `-B` (buffers) are now written by XROUT on every crash. `XMSGDUMP-T` is already off the machine
- 38912 bytes, 5.1% non-zero, 658 distinct words, D100's node number `0064` 84 times, a repeating
record shape. **Decode it against the table layouts in the NPL source, starting at the gateway
tables**, and compare a push-triggered death with a pull-triggered one. Every timing theory has
died; the dump is actual evidence.

**Arming this is now permanent and takes two minutes** - see `xmsg-safe` RULE 8b. Without it the
fatal goes to terminal 1, the physical console, where no agent session can see it: that is what
made ten earlier controls measure the wrong thing.

## 3. The machines and the lab

| # | Question | Cost |
|---|---|---|
| M1 | Why does D100 sometimes not reset its expected sequence on our announce? | `--request-link` does it, the announce alone does not (measured 2026-08-17, correcting an earlier note). The DR it draws is a cost, not a failure |
| **M5** | **D100 is NOT fouled - measured, and it retires M2 and half of M4** | I was about to restart the emulator, which I have standing permission to do. Asking the machine first showed why that would have been wrong: `LIST-LINKS` reports link 152164 - the one D19999 uses - in state **Run** with **2913 data frames sent, 0 retries, 0 bad receives**, and the system entry's state 4 matches `Run` in the link-state enum where 1 is `Init`. So the 1 to 4 change I read as a fault was Init to Run: progress. The machine is healthy and the fault is in what WE send |
| **M4** | **Both services now fail the SAME way, and that contradicts an earlier reading** | Earlier on 2026-08-17 I recorded "FA works while TAD does not", from a run where FA completed real exchanges - short acks, a 98-byte reply. A later run on the same day got only as far as our ConnectionConfirm, and D100 answered `NO ANSWER FROM REMOTE SYSTEM; FILE-ACCESS CONNECTION ABORTED` for both LIST-FILES and DELETE-FILE. So the difference is between RUNS, not between services, which points at machine state rather than at either service's code - and is consistent with M2 |
| M2 | ~~Is D100 currently fouled?~~ **Answered 2026-08-17: NO** - see M5 | By the fourth connect attempt it answered `PERF_CONNCT : 17`. Repeated failures foul COSMOS there, so **back-to-back attempts are not independent samples**. Needs a restart before its answers are evidence - and that is Ronny's call |
| M3 | Where the "virtual system number" boundary lies | 9999 was refused, 19999 accepted. Nothing in the manuals here explains the rule |

## 4. PLANC / the SINTRAN-native chat (#40)

| # | Question | Status |
|---|---|---|
| P1 | **Does any of it compile?** | Nothing in `SINTRAN-CHAT` has ever been through the compiler. This is the gate for #40 and everything below is downstream of it |
| P2 | Does an XMSG arrival wake a BACKGROUND program passivated by `WaitForRestart`? | Not established. The `XFWAK` experiment in `SINTRAN-CHAT/README.md` is written out and unrun |
| P3 | Does `InByte` in no-wait really return error code 3? | From the `NoWaitSwitch` entry, which says so for no-wait calls generally rather than for `InByte` specifically |

---

## How to use this

- **Before starting a hunt**, look here. If the question is listed, its status and the cheapest
  experiment are already written down.
- **When a new unknown turns up**, add a row rather than only burying it in a code comment. The
  comment is still right and should stay; this is the index.
- **When one is settled**, leave the row and mark it settled with the date, like W1 and W9. Deleting
  it invites the question to be asked again.
- **If it is a field or a bit, it belongs in `protocols/sintran-wire.json` too**, with a `status` and
  an `evidence` pointer. The registry is what the code is tested against; this file is the index.

The markers in the code are the source of truth for detail - this file is the map, not the
territory. To re-sweep:

```
grep -rn "UNVERIFIED\|GUESS\|not established\|UNPROVEN\|ASSUMPTION\|not decoded" --include=*.cs SRC
```

### HAZARD: hard-killing our server with live TAD sessions CRASHES the peer's XMSG

Measured the hard way on 2026-08-17. Three TAD sessions were open (tty1, tty2, tty3 - two test
logins and one real user mid-chat) when the runner was killed with `Stop-Process -Force`. D100's
console:

```
ERROR 46 IN DUMMY AT 34360; XMSG FATAL ERROR - INTERNAL ERROR OR INCONSISTENCY
XMSG ERROR CODE:      27; PHYSICAL ADDRESS:  141204
ERROR 53 IN XROUT AT  21661; USER ERROR
SUBER
```

Afterwards every network command answers *"XMSG is either not generated, not loaded or not
started"* while LOCAL commands still work - `LIST-FILES` returned `FILE 99` quite happily. **That
split is the fastest way to tell a dead XMSG from a dead link.**

The sessions are live state on BOTH sides. Our process vanishing leaves D100's XMSG holding session
records whose peer will never answer, and it takes a fatal inconsistency rather than timing them
out.

**Rules:**

 - Get the users out before stopping the server. `list servers` shows the count
   (`*TADADM 2 3 7` = three sessions in use). `chat part` leaves the ROOM, not the TAD session.
 - Never `Stop-Process -Force` a server the ND is still talking to.
 - **No code change would have prevented this one** - a force kill bypasses any shutdown hook. A
   graceful teardown on cancellation would help the ordinary Ctrl-C path, but claiming it as the fix
   would be overclaiming.

**Recovery, verified:** restart the emulator (NOT `START-XMSG`). The boot brings COSMOS back by
itself - `*FA-SERVER` reappears with its seat count. Then `DEF-REMOTE,,D19999 19999` and
`START-LINK,1362,,,-1,,`, both answering `Ok`.

**And realign the counter.** After the restart D100's system table has NO row for 19999 at all
(`1 in use`, no line) - the row is created lazily on first contact, so the peer holds no sequence
state for us and our stored value is far ahead. Set the `100=` entry to `0000`; do NOT delete
`xmsg-sequence.state`, and leave the other nodes alone. Confirmed working immediately afterwards:
`BIGPSH3:TXT` pulled again, 20400 bytes, SHA256 `d523bddc...8d5aebda`.

#### The clean-shutdown fix, and why it is NOT a bolt-on

Attempted and reverted the same day, deliberately. The obvious fix - "on the way out, tear the
sessions down" - does not work where it looks like it should:

```
await adapter.RunAsync(token, ...);   // returns only once the loop has STOPPED
// anything queued here never reaches the wire
```

By the time `RunAsync` returns, the LAPB loop that would transmit the teardown is finished. Frames
handed to the codec afterwards sit in a queue nobody pumps. So the teardown has to be sent **while
the loop is still running** - cancellation has to be handled INSIDE the run loop, not after it.

The pieces that already exist and should be reused rather than rewritten:

 - `TadServer.AppendTeardownLadder` + `BuildDconIndication` + `CloseSession` - the exact ladder a
   user's own `logout` produces, so a clean shutdown adds no new wire behaviour;
 - `XmsgNodeHost.Pump()` - drains `ServerHost.DrainPending()` and sends through the codec; the
   natural place for queued teardown output to leave from;
 - `TadServer.DrainPending` already streams unsolicited per-session output, which is how a chat
   message reaches somebody else's terminal - the same road the goodbye should travel.

There is deliberately no `SendFrame` on `XmsgServerHost`: a server only ever answers an incoming
frame or queues pending output. Adding an out-of-band send to make shutdown work would widen that
contract, which is why this was reverted rather than forced.

**It would not have prevented the crash that prompted it** - a forced kill runs none of our code -
so the operating rule above remains the actual protection.

### CONFIRMED: the TAD logical unit is `768 + LunIndexByte`, and the last two "wire differences" were not differences

Closing out the port-assign frame. Two bytes were left flagged as unexplained after the TAD hunt;
both are now settled, and neither is a defect.

**`0203 0415` (real D102) against our `0203 0215`.** Anchored to the builder in `TadServer` rather
than eyeballed:

```csharp
0x0B, 0x02, 0x03, LunIndexByte,
```

`LunIndexByte` is the 7LUN value, and the code comment already recorded that real captures carry
BOTH `0x04` (conn-to-d102) and `0x02` (new-conn). We emit `0x02` deliberately, because `0x00` was
never observed. **The allocation rule remains unknown - that part is a genuine open question - but
emitting a real observed value is correct and the difference is not a fault.**

**CONFIRMED LIVE 2026-08-17:** the formula in that comment, `LU = 768 + value`, was an inference.
With `LunIndexByte = 0x02` a real ND printed:

```
    TAD LOGICAL UNIT NO: 770
```

768 + 2 = 770. The inference is now a measurement.

**The "trailing `1b` the real server sends and we do not" was a misread.** In the raw payload the
bytes run `... 0201 08ff 00 | 1bb8 | 7e` - `1bb8` is the two-byte **frame check sequence** and `7e`
the closing flag. It was never an element. That mistake came from splitting a concatenated payload
by eye; the same payload also held three HDLC frames where only one was parsed. **Split on the `7e`
flags and strip the FCS before reading a body.**

### THE TAD OPCODES WERE IN THIS REPO ALL ALONG - grep the symbol tables before carving

Prompted by reading an earlier independent implementation (`E:\Dev\Ronny\X25Emulator`). Its TAD
opcode table disagreed with ours on almost every value, so the question had to be settled - and the
authority turned out to be sitting in this repository:

```
SINTRAN/NPL-SOURCE/SYMBOLS/K03/*.SYMB.TXT      one NAME=oooooo line per symbol, OCTAL
```

`7DUMM=000030` is 0x18. `7LUN=000013` is 0x0B. Every TAD opcode is there.

**All twelve values in `TadOp` that had been carved from captures match it exactly.** The carving
was right; it was also days of work that a grep would have answered. **Check the symbol tables
first from now on** - the same file set names XMSG, file-system and RT symbols too.

Three opcodes gained from it, now registered in `tad-wire.json` and named in `TadOp`:

 - `7ESRS = 0x20` - the host's answer to ESCA, sent with RESE. We had observed the byte live and
   guessed the name; the table confirms it.
 - `7LUN = 0x0B` and `7FBSI = 0x15` - previously emitted in the port-assign trailer as bare magic
   numbers. `LU = 768 + value` for the LUN, measured live.

**Names still unmodelled, worth knowing they exist:** `7RFI` ready-for-input, `7REJE`, `7RLOC`,
`7NWRE`, `7ISRQ`/`7ISRS`, `7ERRS`, `7TREP`, `7BMMX`/`7ECKM` (20-byte break/echo strategy tables),
`7CUTY`, `7SATY`, `7SYSI`, `7USID`/`7PASS`, `7STRQ`/`7STRS`, `7KEYI`, `7BADT`, `7NOWT`/`7TNOW`,
`7EDRS`, `7CPCO`, `7POLL`.

**Careful: the value space is reused across namespaces.** `0x08` is both `7ESCA` and `7CUTY`;
`0x0C` is both `7TMOD` and `7SATY`. A number alone does not identify a message - the context does.

### The pad byte is word alignment, and the NPL says so

The same reading turned up the rule behind a finding we had only ever observed the hard way
(odd-length bodies vanishing). The TAD element stream is:

```
[optional pad 0x00] [type] [count] [data...]
```

and the pad is present when the write position is ODD - from `WRMHEAD` in the NPL, `IF TDBTPT BIT 0
THEN`. That is why our port-assign trailer carries `0x00` twice, and why odd bodies are dropped: it
is word alignment, not decoration. **Track the position; do not sniff for the pad.** An
implementation that guesses ("is the next byte a plausible type?") will eventually guess wrong on
data that happens to start with a valid type byte.

#### Release check: the opcode VALUES are stable, the opcode SET is not

The opcode validation above was done against `SYMBOLS/K03` while the live D100 reports XMSG
**Release L**, so the three symbol sets were diffed rather than assumed equivalent:

 - **K03 -> L07 only ADDS symbols.** No existing value changes.
 - **L07 -> M06** changes exactly two, `7LOGG` and `7MCTY`, and both are large address-like values
   (octal `031336` / `032741`), not opcodes.

So every opcode value validated earlier holds for our Release L machine. **The set does not:**

```
78MOD=000054 (0x2C)   absent in K03, present in L07 and M06
7UMOD=000053 (0x2B)   likewise
```

`78MOD` is 8-bit mode negotiation for the terminal line. Both are now in `TadOp` and the registry as
NOT handled, so nobody re-carves them. **A Release K table would have hidden both** - when a symbol
is missing, check the other releases before concluding it does not exist.

Also worth noting: the registry binding for `operations` is **bidirectional** - every enum member
needs a registry entry AND every registry entry needs an enum member. Adding a documented-only
opcode is therefore not possible without also adding the member, which is the right trade.

### The XROUT enums check out - and the symbol tables have a trap the TAD check got lucky on

Same treatment applied to the XMSG side. Every member of `XroutService` and `XroutError` was diffed
against the symbol tables mechanically:

```
XroutService: 36 members -> 34 confirmed, 1 apparent mismatch, 1 not in tables
XroutError:   57 members -> 56 confirmed, 0 mismatches,        1 not in tables
```

`XSLET=0x41`, `XSNUL=0x40`, `XRUNN=0x02`, `XSGMG=0x47`, `XSDRN=0x49`, `XSDSY=0x4A` - all confirmed,
including every value the live path actually depends on.

**Use `SYMBOLS/*/XMSG-SYMBOL-LIST.SYMB.TXT` for XMSG symbols**, not the whole directory. It exists in
L07 and M06 and is the right authority; a sweep over every file mixes FILSYS, N500 and RTLO symbols
that reuse the same 5-character names.

**THE TRAP, and it nearly produced a wrong "correction".** The one apparent mismatch was `XSCMG`:
our code says 72 (octal 110), the symbol list says 2. The code is right and the table hit is a false
match:

 - our registry records the evidence - **ND Communication Guide section 4.4.2.11, octal 110** - a
   protocol document describing what a caller passes;
 - **five** `XS*` names share value 2 in that file (`XSCMG`, `XSDRF`, `XSINF`, `XSLNK`, `XSRFR`), so
   that block cannot be the service-number namespace, which has to be unique;
 - `0x48` has no `XS*` symbol at all in the XMSG list, so nothing else claims our value.

So a symbol file is a set of BLOCKS, not one flat namespace, and a name can appear in several with
different meanings. **Matching by name alone will hand you a confident wrong answer.** Before
"correcting" code to a table value, check that the value is unique within its block and that the
code does not already cite a stronger source.

The TAD opcode check passed cleanly only because those twelve values also agreed with captures and
with live behaviour. Agreement from two independent directions is what made it safe - not the grep
on its own.

#### The closing flush could never write - found while re-attempting clean shutdown

Going back at the shutdown problem found the reason the teardown was impossible, one layer below
where it was being attempted. `LapbLayerAdapter.PumpAsync` ends both of its loops on cancellation
and then flushed with the SAME token:

```csharp
}                                          // loop ended: cancellation requested
await FlushPendingAsync(cancellationToken);   // token is already cancelled
```

`TcpBridgeTransport.WriteAsync` hands straight to `NetworkStream.WriteAsync`, which returns a
CANCELLED task without writing when the token is already cancelled. So **every frame still queued at
shutdown was discarded**, and the resulting throw looked like a normal stop because the runner
catches `OperationCanceledException` and prints "time window elapsed". A goodbye could not have been
sent no matter where it was queued.

Fixed: the closing flush now uses a fresh token with a bounded grace, so queued frames go out on a
live link and a dead peer cannot hang the exit. It changes nothing while the link is running.

**NO TEST, deliberately, and this is the honest part.** Three attempts each proved something about
the test rather than the code:

 - `InMemoryDuplex` ignores its cancellation token and always writes, so it passes with or without
   the fix - it cannot see this bug at all;
 - cancelling before the pump starts dies on the pump's FIRST flush (line 285, same token) and never
   reaches the closing one;
 - cancelling mid-run needs a frame queued in the window between enqueue and the next flush, and the
   pump flushes after every read, so that window is a race rather than a state a test can arrange.

The realistic case - a teardown queued BY a shutdown hook - cannot be tested because that hook does
not exist yet. **Testing it now would mean testing a path nothing uses.** The fix is kept because
flushing with a token you already know is cancelled is wrong on its face, but it is recorded here as
unpinned rather than dressed up with a test that passes for the wrong reason.

**The restructure is now smaller than it was.** The blocker was never the loop shape - it was that
the closing flush could not write. With that gone, a `Stopping` callback raised inside the loop when
cancellation is first seen, before the final flush, is enough for handlers to queue their goodbyes.
