# A room that shrinks: the chat seat leak, and the XMROU that was never set

**Date:** 2026-08-11
**Found:** offline, by cross-checking the two chat implementations against each other
**Status:** fixed in both, proved by tests. The XMROU reading was SETTLED on 2026-08-17 from ND's own sample servers - see "XMROU is settled".

---

## How it was found

Not by looking for it. The chat rules exist twice - once in C# (`Xmsg.Chat/ChatRoom.cs`,
`ChatServer.cs`) and once by hand in PLANC (`SINTRAN-CHAT/CHATSV.PLNC`) - and the two share no
code at all. Reading them side by side for disagreements turned up a small one, and the small one
led to a real defect underneath.

**The small one:** somebody who never joined sends a `Rename`.

| | what happens |
|---|---|
| `ChatServer` | `ChatRoom.TryRename` reports `"you are not in the room"` like any other refusal, so the server answers a **Reject** |
| `CHATSV.PLNC` | tests membership once, before it looks at the message kind at all, so a stranger gets **silence** |

`HandleSay` already had the rule written down - replying "would confirm the port is a chat server
to something that never joined" - so silence is right and the C# was the one out of step. The C#
comment directly above the code even claimed it *was* silent; it was wrong about its own code.

Fixing that meant asking how a stranger's message reaches the server at all, and that is where the
real fault was.

---

## The real fault: a leaked seat

A chat room's size is not enforced by the room. It is enforced by XROUT, which holds a
free-connection count for the port, forwards a letter only while that count is above zero, and
decrements it on each forward. That is the mechanism the real file access server uses for its 30
seats, and the chat server copies it.

So the seat is spent **by XROUT, on forwarding, before the room has seen one byte of the body.**

The old code handed the seat back inside `HandleJoin`, on the refusal path. That is correct for a
refused join and wrong for everything else, because letters carrying something else exist:

- a letter carrying `Say`, `Rename` or `Leave` from somebody who never joined
- a letter whose body does not decode at all - which `Poll` drops on purpose, so that one confused
  client cannot stop the room

Neither reaches a handler that returns a seat. The seat is simply gone.

**Nothing goes wrong at the time.** No error, no log line, nobody refused. The room is just one
seat smaller, permanently. It surfaces much later as XROUT turning joins away from a room that
visibly has space - a fault nobody would connect to a message sent hours earlier.

`CHATSV.PLNC` had it worse: its `refuse` never returned a seat either, so it leaked on **every
refused join** as well.

### Proved before it was claimed

`SRC/Xmsg.Chat.Tests/ChatSeatAccountingTests.cs` was written first and failed 3 of 4:

```
ALetterThatIsNotAJoinGivesItsSeatBack   [FAIL]
AnUndecodableLetterGivesItsSeatBack     [FAIL]
ManyStrayLettersDoNotCloseTheRoom       [FAIL]
ARefusedJoinGivesItsSeatBackAndAn...    pass
```

The one that passed is the join path, which was already right. The stray-letter loop is the one
that matters: a single stray letter costing a seat is easy to overlook, but the same letter in a
retry loop closes the room to everybody.

---

## Why the server could not tell a letter from an ordinary message

It could not, and that is the second half of the defect.

`XmsgMessageType` has had `XMROU = 2`, *"Routed message (Via XROUT)"*, all along, and
`XmsgQueueSnapshot` has code that reads "a router message is first in this port's queue". But
**nothing in the kernel ever set it.** Both `Deliver` overloads chose only between `XMTHI` and
`XMTNO`, so the snapshot's router branch could never fire from a delivered message, and a receiver
had no way to know a seat had been spent on it.

`XroutDirectory.SendLetter` delivers with `XmsgSendFlags.Forward` (XFFWD) - forward without
updating the sender, which is exactly what XROUT does passing a letter on. So XFFWD now produces
`XMROU`, via a single shared `ChooseType` so the two delivery paths cannot drift.

Order matters in that helper, and it is commented: **XFHIP and XFRRO are the same bit**, so a
forwarded letter with remote-route set would otherwise read as high priority.

---

## The fix, in one sentence

The seat belongs to the **arrival**, not to any kind of message inside it - so it is settled once,
in the receive loop, in both implementations:

```csharp
bool spentASeat = arrived.MessageType == XmsgMessageType.XMROU;
long id = Handle(sender);
bool wasMember = _room.Contains(id);
...
if (spentASeat && !(!wasMember && _room.Contains(id)))
{
    ReleaseSeat();
}
```

The condition is "became a member", not "was accepted", and that distinction is load-bearing: a
member who sends a second letter spends a second seat, and only one of the two is theirs to keep.
`HandleJoin` no longer touches seats at all.

`CHATSV.PLNC` does the same thing with `findByMagic` before and after `handleMessage`, releasing
with `xmpinfc(0, myPort, 1, 0)` - the call its `Leave` path already used.

---

## What is not verified

**ND's worked example now reproduces in full, which is corroboration and not proof.** The XMROU
fix made the middle arm of `X-MESSAGE 210373L` section 6.3 stageable for the first time, so all
three - ordinary, router, returned - can be put on one kernel and the answer read out in ND's own
arithmetic: **A=7, D=2, X=1**, exact. That is worth having because the three words come from ONE
walk of the port list, so a bit-order mistake that looks right with a single flagged port shows up
here as the wrong word. It confirms the SNAPSHOT reading; it does not confirm that a letter off the
wire carries XMROU.

### XMROU is settled - 2026-08-17

**A forwarded letter DOES arrive as XMROU**, and the source is better than the capture that was
planned. XMROU is not a field on the wire at all - it is what XFRCV hands the receiving program in
the T-register - so a packet capture could never have shown it. The right oracle is code that
receives one, and ND published two.

Both sample servers in the COSMOS Programmer Guide (ND-60.164.3), appendix of sample programs, wait
for the forwarded letter and then refuse anything else:

```
PLANC     IF MSGTYPE >< XMROU THEN
              OUTPUT(1, 'A', 'WRONG MESSAGE TYPE ')

FORTRAN   IF (MSGTYPE .NE. XMROU) THEN
              WRITE(1,*)'WRONG MESSAGE TYPE'
```

Two independent bindings making the same check rules out a quirk of one library, and it survives the
OCR of these scans: identifiers can be mangled, but the same test appearing twice in two languages
is not an artefact.

**The other half falls out of the same sample.** Each server then reads the CLIENT's magic number
from the arrived message, with `XMFMFST` / `XMPFMST`, to answer it directly. So the sender field
carries the original client rather than XROUT - which is exactly what XFFWD means - and the TYPE is
the only thing saying XROUT carried it. That is precisely the pair our `ChooseType` implements.

**`XMROU` is available to a PLANC program by name.** The PLANC sample uses it bare, under the same
two includes `CHATSV.PLNC` already has (`XMP:IMPT` and `XMP:DEFS`), so that file now uses the symbol
instead of the `xmRoutedMsg := 2` literal it was carrying.

What remains genuinely unmeasured is only the PLANC itself: **nothing in `SINTRAN-CHAT` has been
compiled**. That is a different question from this one and is still owed.

---

## What this says about the method

The bug was not found by testing the chat server. It was found by making two independent
implementations of the same rules and reading them against each other. The C# had a
one-line inconsistency; chasing why it existed led to a silent, cumulative, permanent fault that
no existing test could see - because every existing test drove the server through `ChatClient`,
which only ever sends a join as a letter.

A second implementation is not duplicated work. It is the only thing that was looking.

---

## Files

- `SRC/Xmsg.Api/Kernel/XmsgKernel.cs` - `ChooseType`, XFFWD produces XMROU
- `SRC/Xmsg.Api.Tests/XmsgKernelTests.cs` - the routed/normal pair, with a control
- `SRC/Xmsg.Chat/ChatServer.cs` - seat accounting moved into `Poll`; stranger rename now silent
- `SRC/Xmsg.Chat/ChatRoom.cs` - `Contains`
- `SRC/Xmsg.Chat.Tests/ChatSeatAccountingTests.cs` - 4 tests, 3 of which failed first
- `SINTRAN-CHAT/CHATSV.PLNC` - the same accounting, and the same reasoning in its comments

1011 tests green.

---

## Same method again: the CLIENTS, and three more defects

The pass above compared the two servers. Comparing the two CLIENTS - `ChatClient.cs` against
`CHAT.PLNC` - found three more, none of which the C# can have because C# strings carry their own
length.

**1. `CHAT.PLNC` wrote past the end of the typed line.** `typed(0:79)` with no test on `typedLen`:
the 81st printable character went past the array, into `roomName`, `typedLen`, `serverMagic` and
`joined` - whatever the compiler laid out next. Holding a key down would corrupt the program's own
state. PLANC checks no array bound. Fixed with a named `maxTyped` and a test; a full line now stops
accepting characters.

**2. `joined` was set and never read.** `ChatClient.Say` opens with `if (!_joined) return false` -
nothing is sent before the welcome, because the welcome is what reveals the server's address. The
PLANC set `joined := TRUE` on the welcome and then never looked at it, so a line typed before the
welcome arrived was posted to an uninitialised `serverMagic`. Fixed: the send is guarded, and
`serverMagic` is now explicitly `0` so the guard protects a known value rather than whatever the
loader left.

**3. `CHATSV.PLNC` trusted a length byte off the wire - the worst of the three.** A nickname length
is one byte, so a peer can say 255. `copyNameIn` copied that many bytes into `memberName(1:8, 1:16)`
- sixteen bytes per seat - straight through the other seven members' names and out of the array. One
malformed message, from anyone who can post a letter, and no bound check anywhere in the language.

The C# equivalent was already right: `ChatMessage.TryDecode` validates both declared lengths against
the buffer and returns false. So the guard now sits at the top of `handleMessage` and checks the
same two things: that the name FITS, and that the fields lie inside the message actually read.
Dropped in silence rather than refused, because a refusal echoes the name and the name is the part
that cannot be trusted. The seat is settled by the receive loop either way - which is precisely why
that accounting does not live in the handler.

### One divergence that fixing #3 created

Capping the name at sixteen made the PLANC drop something the C# still admitted. That is a new
disagreement, so the cap became a rule of the room rather than a detail of one implementation:
`ChatRoom.MaxNicknameLength = 16`, enforced on the join door AND the rename door, since CHATSV
copies into the same fixed row on a rename.

The replies still differ - C# refuses with a reason, the PLANC drops in silence - but the rule is
the same and the room state is identical: the name is never admitted. Three tests pin it, including
the inclusive boundary, because the PLANC guard is `nameLength > maxNameLen` and an off-by-one on
either side would show only at exactly sixteen characters.

1014 tests green.

---

## Was XMROU the only one? A sweep, and a negative result

XMROU was an instance of a class worth naming: **an enum value the code can READ but never
PRODUCES**. Something switches on it, so it looks handled, and nothing ever makes one.

The obvious search - members never referenced at all - is the wrong one, and finding that out is
the useful part. XMROU *was* referenced: `XmsgQueueSnapshot` had a `case` for it. A never-referenced
scan would have walked straight past the actual defect. The search that finds this shape is
**consumed more than zero times, produced zero times**.

Run over every enum in `Xmsg.Protocol/Enums`, the survivors were:

| Candidate | Verdict |
|---|---|
| all of `XmsgError` | **peer-produced by nature.** They are read once each by `XmsgErrorDispositions`, a table classifying codes the other machine sends. We never make one, and should not. |
| `XmsgLinkState` (Dead, Init, Conn, Kill) | decode-side vocabulary for a state a real machine reports in a link-status reply |
| `XmsgFrameFlags` (Letter, Marker01, SystemMode) | deliberate. The enum's own remarks say the per-bit rule is UNKNOWN, so frames are built from whole captured combos and never composed bit by bit. `Letter` here is a bit named from a capture - nothing to do with an XROUT letter. |
| `XMKIK` | `SendLetterAndKick` (XSLEK) is a request we BUILD for a real XROUT. Our in-memory stand-in does not implement the receiving side, so nothing here should produce one. |

**So: no second XMROU.** The reason it stood out is that a message TYPE is something this kernel
hands out itself, where an error code is something a peer hands us. That distinction is the whole
answer, and it is why the sweep was worth running once and is not worth running again.

No test was added for this. `NdsWorkedExampleReproducedInFull` already stages XMTNO, XMROU and
XMTRE together, so the one case that was genuinely broken is now the one case that is pinned.
