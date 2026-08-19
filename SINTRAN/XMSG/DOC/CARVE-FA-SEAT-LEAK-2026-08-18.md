# Every FA transfer leaks a connection seat, and that is why pushes "randomly" stall

> **SOLVED 2026-08-18.** Our client sent the SERVER's message (`Close`, `07C0`) instead of the
> client's (`Release`, `0782`), with the two conversation numbers SWAPPED and the wrong frame flags.
> Fixed in both drivers. **Measured after the fix: three pushes and two pulls, `*FA-SERVER` steady
> at 29 free - not one seat spent.** Before it, five transfers cost five seats.
>
> The three failed attempts below are kept: each eliminated one wrong explanation, and the last one
> is why the fix was found by reading instead of guessing.


**Measured on D100, 2026-08-18.** This explains a problem that has cost this project days, and the
explanation is arithmetic rather than anything subtle.

## The measurement

`X-C` -> `LIST-NAMES` prints a free-connection count for every named server. Watching
`*FA-SERVER` across known transfers:

| event | Free SPs |
|---|---|
| straight after `FS-ADMINISTRATOR` / `START-SERVER` | **30** |
| one refused create, then one successful overwrite | **28** |
| one more successful push | **27** |

**One seat per transfer, whether it succeeded or was refused, and none ever comes back.** Thirty
transfers after a restart, the server has nothing left to give and answers new connect letters with
silence.

## What it explains

The recurring "the FA push stalls at unpredictable points" problem, recorded over several days:

 - **"three of six pushes stalled, at roughly blocks 10, 19 and 21"** - the block number is a red
   herring. What matters is how many transfers had already run.
 - **"size is not the discriminator - 26 blocks completed once and stalled twice"** - correct, and
   now explained. The discriminator is the transfer COUNT since the last file-server start.
 - **"restarting the FA server cleared it twice and then stopped working"** - each restart hands out
   30 fresh seats, so it works until they are gone again. It looked like a cure that decays because
   the thing being consumed was never identified.
 - **`START-SERVER` allocating a NEW instance each time** (server 1, then 2) leaves the exhausted
   one registered alongside the fresh one, which is why `LIST-NAMES` can show two `*FA-SERVER`
   entries, one with 0 free and one with 30.

## Why it happens

A connection port hands out a fixed number of seats and XROUT decrements the counter to deliver a
connect letter. The seat is returned by the SERVER program, not by the transport - and it has to be
given back explicitly.

Our own PLANC room server does exactly that, and says so in its own comment:

```planc
% Give the seat back to XROUT, or the room fills up permanently and
% XROUT stops forwarding joins long after anybody left.
xmpinfc(0, myPort, 1, 0) =: returnStatus
```

`XMPINFC` is the call. **The C# FA client never does the equivalent**, so every conversation it
opens costs the far server a seat for good.

## What is NOT yet established

 - **Whose job the return is on this path.** CHATSV returns a seat on ITS OWN connection port, as
   the server. For an FA transfer we are the CLIENT talking to the machine's `*FA-SERVER`, so the
   return may belong to the server's own close handling, to something our close is failing to
   trigger, or to a message we never send. That is the next thing to carve.
 - **Whether the seat comes back on a timeout.** Readings minutes apart did not move, but nothing
   has been left overnight.

## How to check it on a live machine

```
@X-COMM
X-C: LIST-NAMES        -> read the Free SPs column against *FA-SERVER
```

and to get the seats back:

```
@FS-ADMINISTRATOR
FSA: SELECT-FSA,,,,
FSA(own system): START-SERVER 1,,,,     -> "No of FACs attached: 30"
FSA(own system): EXIT
```

**Read `LIST-NAMES` before blaming a transfer.** A push into a server with no seats left looks
exactly like a network fault: the connect letter goes out, nothing answers, and the ladder never
starts.


---

# REFUTED: sending a client Release instead of a Close does NOT fix it

Tried and measured the same afternoon. **It made things worse and is reverted.** Written down so
nobody spends the evening on it twice.

## The hypothesis, which looked strong

Our client's last frame is `07C0`, and `07C0` is the SERVER's message. Our own type table says so:
`FaMessageType.SessionFinished` (`0x0782`) is documented as "the client has finished with the
conversation and expects a Close back", while `Close` (`0x07C0`) is what comes the other way.

The real ND client agrees. From `DOC\captures\ND-TO-ND-WRITE-2026-08-10\`, D102 writing to D100
with no C# in the path:

```
D102  Release  0782 0043 0004 8000 0000     the client says it is finished
D100  Close    07C0 0004 0043 0000          the server answers, sides swapped
```

So: we send the server's message, the server never hears "I am finished", never tears the session
down, never returns the seat. Everything fits.

## What actually happened

`BuildCloseBody` was changed to emit `0782 <ourConv> <serverConv> 8000 0000`, copying the capture's
shape. One push later:

 - `*FA-SERVER` free seats went from **26 to 0** - the whole pool, in a single transfer, instead of
   the usual one;
 - **`*FA-FSA` disappeared from `LIST-NAMES` entirely.** The file server administrator had died, so
   `FS-ADMINISTRATOR` then answered "Remote FSA is not running" and could not restart anything.

Recovery took `ABORT FSART` + `RT FSART` to get the administrator back, and only then
`FS-ADMINISTRATOR` / `SELECT-FSA` / `START-SERVER`. That also cleared the stale exhausted
`*FA-SERVER` entries, leaving one clean registration.

With the change reverted, a push behaves exactly as before: 30 free, one transfer, 29 free.

## What this tells us

 - **The opcode is not the whole story.** A frame that merely carries `0782` is not a Release; some
   other part of the layout is wrong, and D100 mis-parsed it badly enough to take out the FSA.
 - The conversation numbers are the obvious suspects. `_target.LetterEchoWord` is what we put in
   the Close and it worked there; whether it is the right value in this position is unverified, and
   `0x8000` was copied without knowing what it means.
 - **Do not try this again from the shape alone.** The next attempt needs the actual bytes of a
   client Release pulled out of the pcapng and compared field by field against what we build - not
   the summary table above, which prints only four words and drops whatever else is in the frame.

The leak itself is unchanged and still real: one seat per transfer, thirty and the server is silent.


---

# The field-by-field comparison, and a second refuted fix

Done properly this time: the FULL frames pulled out of `nd-to-nd-write.pcapng`, sub-header included,
rather than `fa_view.py`'s four-word summary.

## What the summary was hiding

```
native Release   sub 2100 8284 0064 05b9 0066 03f9 000a   body 0782 0043 0004 8000 0000
native Close     sub 2100 8284 0066 03f9 0064 05b9 0008   body 07c0 0004 0043 0000
our Close        sub 2100 9600 0064 07a2 4e1f 0211 0008   body 07c0 0002 0041 0000
```

Two things the four-word view never showed:

 - **the last sub-header word is the FA body length** (`000a` for the ten-byte Release, `0008` for
   the eight-byte Close). Our builder computes it from the body, so it was never the problem - but
   it is worth knowing before anyone hand-builds a frame.
 - **every real FA message uses `frameFlags 0x82` and `role 0x84`.** Counted across the whole
   capture, both machines, every type:

```
FA kind   frameFlags  role   count
0x0782      0x82      0x84     2      Release
0x07a2      0x82      0x84    30      ShortAck
0x07c0      0x82      0x84     2      Close
0x07d2      0x82      0x84     1      ConnectConfirm
0x07f0      0x82      0x84    30      Request/Reply
```

**Every frame we send carries `0x96` / `0x00` instead.** Our transfers work anyway, which is why
this survived so long.

## The second experiment, and its refutation

Sending the close with `frameFlags 0x82` / `role 0x84` - matching a real ND, and changed on the
CLOSE ONLY so a mistake could not break every transfer:

 - the push completed normally, 20 bytes written;
 - `*FA-SERVER` went **29 -> 28**. The seat was still not returned;
 - nothing was damaged - `*FA-FSA` stayed up, unlike the first attempt.

**So the close's flags and role are not what returns the seat either.** Reverted: an unverified wire
change with no measured benefit does not ship, even one that makes us look more like a real ND.

## What is left

Two candidates survive, and one new observation to build on:

 - **A connection-level teardown we never perform.** Everything tried so far has been at the
   CONVERSATION level. XROUT's counter is a property of the connection port, and the PLANC room
   server returns its seat with `XMPINFC` - a call about the PORT, not about any conversation.
 - **The seat may be the server's to return, and it does not.** In that case there is nothing for a
   client to fix, and the answer is operational: watch `LIST-NAMES` and restart the file server.

**The new observation:** a real client registers its own connection port, `*FA-USER`, which appears
in `LIST-NAMES` while it is transferring and is gone afterwards. We never register anything. If the
seat accounting is tied to a named client port, that difference is where to look next - and it is
checkable without sending a single new frame, by watching whether `*FA-USER` appears for a native
transfer that returns its seat.


---

# A native client on our own wire, a third refutation, and where this stops

## The best measurement of the day: a real ND client talking to OUR server

Our runner has an FA server. Pointed at the HDLC seam as D19999 with a folder to serve, D100's own
`COPY-FILE` was aimed at us:

```
@COPY-FILE "FROMUS:TXT",D19999(SYSTEM)."NATREAD:TXT"
```

The whole native CLIENT session then landed in our log, on the same link our pushes use. Its
teardown:

```
[fa] system 100 has finished with conversation 0x0082 (0x0782); answering with a close
[fa] reply to system 100: 8 byte(s), FA body 07C0000E00820000
```

and the frame it sent, in full:

```
XMSG sub-header: frameFlags=0x82  role=0x84
endpoints:       src 100:1703 -> dst 19999:529
FA body:         0782 0082 000E 8000 0000
                      ^client conv  ^server conv
```

**Our SERVER already speaks this correctly.** It recognises the Release and answers with a Close.
Only our CLIENT is wrong.

## Native transfers do not leak

Five native `COPY-FILE`s in a row: `*FA-SERVER` stayed at 28, unmoved. Every one of our pushes costs
exactly one. So the leak is ours, not a property of the server.

(Caveat kept honest: those five were LOCAL copies, D100 to D100. A local path might not touch
`*FA-SERVER` at all. What is not in doubt is the contrast with our own transfers.)

## Third refutation, and it was destructive again

Attempt 1 sent the Release BODY with our usual flags - it killed the FSA.
Attempt 2 sent our usual body with the Release FLAGS - harmless, no effect.
Neither had tested both together, so attempt 3 did: body `0782 ...` AND `frameFlags 0x82 / role 0x84`,
copied from the frame above.

**It destroyed the file server.** `*FA-SERVER` and `*FA-FSA` both vanished from `LIST-NAMES`
entirely - worse than attempt 1, which only exhausted the seats. Recovery again needed
`ABORT FSART` + `RT FSART`, then `FS-ADMINISTRATOR` / `SELECT-FSA` / `START-SERVER`.

Reverted. A push works normally again and still costs one seat.

## What that leaves, and why the experiments stop here

The opcode is right, the flags are right, and it still destroys the server. **The remaining
difference is the conversation NUMBERS**, and there is a specific reason to suspect them: the native
client's own conversation is `0x0082`, a per-session value, while ours is the constant our code
already describes as "only the USUAL value of that word". A Release naming a conversation the server
does not have is a plausible way to make it free the wrong thing and fall over.

**That is a hypothesis, and it is not being tested by guessing again.** Three attempts, two of them
destructive, is enough evidence that this frame is not safe to explore by substitution. The next
step is to READ, not to send: work out where our conversation word comes from, what the server
expects in that position, and only then build a Release - or decide the client cannot safely send
one at all and the seat must come back another way.

Until then the leak stands, with the operational workaround: watch `LIST-NAMES`, and restart the
file server when the seats run low.


---

# READ, not sent: the conversation numbers in our close are SWAPPED

Found by reading, as promised after the third refutation. No frames were sent to find this.

## The two fields are not the same thing

`FaFileEndpoint` carries both, and its own comments distinguish them:

```
Conversation   = 0x0044   "the conversation number we stamp on every request. The asker picks it.
                           Across the captures it ran 003F, 0044, 0046, 0048, 0052 and never
                           repeated, so it behaves as a counter"
LetterEchoWord = 0x0002   "the word the connect letter carries for the server to echo ...
                           comes back in the confirmation and is then stamped on everything the
                           SERVER sends"
```

So `LetterEchoWord` is the number the **peer** puts on its messages. It is not ours.

## Who stamps what, measured

From the native client's session against OUR server:

```
native client -> us    conversation 0x0082      (what the client stamps)
us -> native client    conversation 0x000E      (what the server stamps)
native Release body    0782 0082 000E           = [sender's conv][peer's conv]
```

And on our own transfers against D100: our requests carry `_serverConversation`, while D100's
replies carry `0x0002` - our `LetterEchoWord`, echoed back exactly as documented.

**So for our client: OUR conversation is `_serverConversation`, and the PEER's is
`LetterEchoWord`.** The names are misleading, which is most of why this survived.

## The swap

```csharp
// our SERVER - and a real ND client accepts this, so it is right
NdEndian.PutBe16(body, 2, _responderConversation);   // ours
NdEndian.PutBe16(body, 4, _clientConversation);      // theirs

// our CLIENT
NdEndian.PutBe16(body, 2, _target.LetterEchoWord);   // THEIRS
NdEndian.PutBe16(body, 4, _serverConversation);      // OURS
```

Sender-then-peer against peer-then-sender. Two independent implementations agree the first is
right: the frame a real ND client sends, and the frame our own server sends that a real ND client
accepts.

## Why this explains the destruction

As a `Close` (`07C0`) the swap is survivable - our transfers work, so the server evidently does not
act on that message's operands. As a `Release` (`0782`) the server DOES act on them: it frees the
session named in the first word. Named a conversation it does not have, twice it took the whole file
server down with it.

**Both destructive attempts sent the right opcode with the operands backwards.** That is a much
better explanation than "the opcode is dangerous".

## The next step, and it is a SAFE one

Do not try attempt four against D100. Our own server already speaks this protocol correctly - it
handled a real ND client's whole session including the Release. So the test bed is **our client
against our own server**, where a wrong frame costs nothing:

 1. fix the ordering in `BuildCloseBody` and send `0782` with `frameFlags 0x82 / role 0x84`;
 2. point our push at our own file server rather than at D100;
 3. our server either answers with a Close - the protocol completed - or logs what it could not
    make sense of.

Only once that round trip works locally is it worth spending a D100 file server on it.


---

# ANSWERED: what returns a seat, and why it is not in any capture

Not carved from the binary - the COSMOS Programmer Guide (ND-60.164.3) says it outright, and the
capture then confirms the consequence.

## The rule, quoted

On taking a seat:

> XROUT will look at the free connection counter for portName and if this is greater than zero,
> **XROUT will decrement the counter** and forward the whole message to portName.

On giving it back:

> After opening a connection port using XMPOPCN, **a task can later increment** (when the
> connections become available) or decrement ... the free connection counter associated with that
> port.

That task is the one that OWNS the port - the server. The call is `XMPINFC`, and at the service
level it is `XSNSP` = **0o121 = 81 decimal** (guide section 3.3).

**CORRECTION 2026-08-18.** This section first cited "service 291". **291 is a PAGE number** from the
guide's index, not a service number. The value comes from the XMSG symbol list,
`SINTRAN/NPL-SOURCE/SYMBOLS/L07/XMSG-SYMBOL-LIST.SYMB.TXT`:

```
XSNUL=000100   XSLET=000101   XSNAM=000102   XSCRS=000120   XSNSP=000121
```

`XSLET=000101` is 65 decimal, which is the value CHATSV already uses for a letter - so the file holds
service numbers in OCTAL and **`XSNSP` is 0o121 = 81 decimal**. The capture search that reported
"zero frames carrying service 291" was hunting the wrong byte and proved nothing. Redone for 0x51:
still zero, and still exactly ONE XROUT-addressed frame in the whole transfer, so the conclusion is
unchanged and now actually has evidence behind it.


## Why no capture will ever show it

**The server and XROUT live on the same machine.** `XMPINFC` is a local call from `*FA-SERVER` to
D100's own XROUT, so it never crosses the link to the client. Confirmed rather than assumed: in the
whole `nd-to-nd-write.pcapng`, **not one frame carries `XSNSP`**, and only a single frame in the
entire transfer is addressed to XROUT at all - the client's opening connect letter.

```
D102  0bc6  0x1b41  100:0 -> 102:1017   1b410012ff0a2a46412d534552564552...   <== XROUT
```

So a client CANNOT return the seat by sending anything. There is no message for it. All a client can
do is tell the server it has finished, and let the server run its own teardown.

## What the real machines do at the end

```
client -> server   Release  0782 <clientConv> <serverConv> 8000 0000
                            ...server tears the session down and calls XMPINFC locally,
                               which is invisible on the wire and is what frees the seat...
server -> client   Close    07C0 <serverConv> <clientConv> 0000
```

## What we do differently, in one line

**We send `Close 07C0` - the SERVER's message - instead of `Release 0782`.** D100's file server never
learns the client has finished, so it never runs the teardown, so it never calls `XMPINFC`, so the
seat is gone for good.

Our own C# server proves the trigger, from the other direction. Driving a real ND client at our
`FaServer` over the HDLC link produced:

```
[fa] system 100 has finished with conversation 0x0082 (0x0782); answering with a close
[fa] reply to system 100: 8 byte(s), FA body 07C0000E00820000
```

So **our SERVER already implements this correctly**. Only our CLIENT is wrong.

## Which narrows the three failed attempts to one suspect

The direction was right all along; the contents were not.

 - attempt 1: correct body `0782`, wrong flags - killed `*FA-FSA`
 - attempt 2: correct flags `0x82`/`0x84`, wrong body - harmless, seat still lost
 - attempt 3: both together - killed `*FA-SERVER` and `*FA-FSA`

Attempt 3 had the right opcode and the right flags, so what is left is the **two conversation
words**. A real client sends its own per-session number first (`0x0082` in the run above) and the
server's second. We send `_target.LetterEchoWord`, which our own code comments describe as "only the
USUAL value of that word", and `_serverConversation`. If the first word does not name a session D100
is actually holding, a teardown that frees the wrong thing is exactly what a dying server looks like.

**The next experiment is therefore narrow and safe to state:** capture what conversation number D100
stamps on its replies to US during a push, and send THAT as the first word of the Release. Change
that one field, measure `LIST-NAMES`, revert if the count does not move.


---

# The other half: a REFUSED transfer leaked a seat too

Found by arithmetic, not by suspicion. After the fix above, `*FA-SERVER` had gone from 30 free to
27 across a set of daemon runs - and those runs contained exactly **three refused creates**, while
every successful transfer in them returned its seat.

## Why

A refusal ends the TRANSFER. It does not end the CONVERSATION, and the seat belongs to the
conversation. But `FaClientWriteSession.NextAction` tests the failure first:

```csharp
if (_failure.Length != 0) { return FaClientAction.Failed; }
```

so once the server refuses us the ladder is over, `SendClose` is never reached, and the Release
never goes out. Exactly the same stranded seat as before, on a path the first fix did not touch.

## The fix

On a refusal the driver now OWES a Release, and sends it from the top of `BuildNext`, outside the
ladder - because the ladder will not run again. The run reports itself unfinished while that
goodbye is owed, or the one-shot would stop and cut the conversation off before the frame reached
the wire, which is the whole thing this is for.

## Measured

Six conversations in a row against a server sitting at 27 free:

| what | count | seats after |
|---|---|---|
| refused create (`PRST:TXT` already exists) | 3 | 27 |
| refused pull (`NOSUCH:LIST`) | 2 | 27 |
| successful pull, 108561 bytes | 1 | 27 |

**Not one seat spent**, and the good path still works. Before this, the three refused creates alone
cost three seats.

## The general shape, worth remembering

Both halves of this bug were the same mistake in different clothes: **treating "the transfer is
over" as "the conversation is over".** The first was a successful transfer sending the server's own
message instead of a client Release; the second was a failed transfer sending nothing at all. The
seat is the conversation's, and the conversation has to be ended deliberately either way.
