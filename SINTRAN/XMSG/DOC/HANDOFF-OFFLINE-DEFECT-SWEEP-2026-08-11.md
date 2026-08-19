# Handoff: the offline defect sweep, 2026-08-11

Follows `DOC/HANDOFF-2026-08-11.md`, which covers the hardware results. Nothing here touched
D100, D102 or D103 - the lab was yours all session. This is what came out of reading the two chat
implementations against each other, and from a sweep of the wire-facing decoders.

---

## The decoder sweep: now run, and it holds

`SRC/Xmsg.Protocol.Tests/FaDecoderRobustnessTests.cs` was written but unrun when this handoff was
first written. It has since been compiled and run: **5 tests, all passing**, and it is committed.

It takes a real captured CreateFile request and damages it every way a link can - every truncation,
every single-byte change to a length trap, and both together, 145 bodies in all - then pushes each
one through every wire-facing FA decoder and fails if any of them **throws** instead of refusing.

The result is the one that was expected: the decoders were already correct. Every FA codec goes
through `QformReader.TryRead`, which turns `QformFormatException` into a plain false, and
`QformReader.Read` does check each declared length against the body. So this is a regression guard
for a property that already held, not a bug that was found.

**The fifth test is the one that makes the other four mean anything.** Three sweeps that pass
because no decoder threw would pass just as quietly if every damaged body had been rejected at its
first byte, having tested nothing. So `TheSweepActuallyReachesBodiesThatWouldThrow` counts the
damaged bodies that make the THROWING door actually throw, and requires at least ten. Each of those
is a body that walked far enough in to hit a length running past the end - the exact state the
2026-08-06 padding defect died in.

---

## What IS proved and committed

Two commits, both pushed to `origin/5000x`, full suite green at each.

### `54e3559` - a room that silently shrinks, and an XMROU nobody was setting

Found by reading `ChatServer` against `CHATSV.PLNC`. The visible disagreement was small: a stranger
who never joined sends a `Rename`, and one side answered with a Reject while the other stayed
silent. `HandleSay` already had the rule - replying confirms the port is a chat server to something
that never joined - so the C# was out of step, and its own comment claimed it was silent when it
was not.

Underneath was the real fault. **XROUT spends one of a connection port's free seats to FORWARD a
letter**, before the room has seen a byte of the body. The seat was handed back inside `HandleJoin`,
which is right for a refused join and wrong for everything else - a letter carrying `Say`, `Rename`
or `Leave` from a stranger, or a body too damaged to decode, never reaches a handler at all. Nothing
fails at the time. The room is one seat smaller for good, and it surfaces much later as XROUT
refusing joins for a room that visibly has space.

The server also could not tell a letter from an ordinary message: `XmsgMessageType.XMROU` has always
existed and `XmsgQueueSnapshot` reads it, but **nothing in the kernel ever set it**. XFFWD now
produces XMROU through a shared `ChooseType` that tests Forward FIRST, because XFHIP and XFRRO are
the same bit.

Seats are now settled once, against the arrival, in both implementations. `ChatSeatAccountingTests`
was written first and failed 3 of 4.

### `38fd397` - three PLANC buffer faults, and a name limit that became a rule

Then the same method on the CLIENTS, `ChatClient.cs` against `CHAT.PLNC`:

1. `typed(0:79)` had no bound test, so the 81st typed character went into `roomName`, `typedLen`,
   `serverMagic` and `joined` - whatever the compiler laid out next.
2. `joined` was set on the welcome and **never read**, so a line typed before the welcome went to an
   uninitialised `serverMagic`. `ChatClient.Say` refuses in exactly that state.
3. Worst: `CHATSV` trusted a length byte off the wire. A nickname length is one byte, so a peer can
   say 255, and `copyNameIn` copied that many into `memberName(1:8, 1:16)` - sixteen bytes per seat
   - through the other seven members' names and out of the array. One malformed message from anyone
   who can post a letter.

`ChatMessage.TryDecode` already did this right, so `handleMessage` now checks the same two things:
the name FITS, and the fields lie inside the message actually read.

Capping the name created a NEW divergence - the PLANC dropped what the C# still admitted - so the
cap became `ChatRoom.MaxNicknameLength`, enforced on the rename door too.

---

## What is reasoned but NOT measured

Flagged because it is the kind of thing that reads as settled six months from now.

| Claim | Basis | What would settle it |
|---|---|---|
| a real letter arrives as **XMROU** | **SETTLED 2026-08-17** - both sample servers in ND-60.164.3 refuse anything that is not XMROU, and each then reads the CLIENT's magic to reply | done, no capture needed |
| `XMP:IMPT` exports the name `XMROU` | **SETTLED 2026-08-17** - the PLANC sample uses `XMROU` bare under the same two includes CHATSV has | done; the literal is gone |
| the PLANC fixes are correct | read against the PLANC reference manual | **nothing in `SINTRAN-CHAT` has ever been compiled** |

The accounting rule itself does not depend on the first line: it follows from XROUT decrementing on
forward, which IS observed.

---

## Still owed, and all of it needs the lab

- **#34** - the live two-user chat run. Everything else about chat is built and tested.
- **#40** - the SINTRAN-native chat, which you asked to build together. Both PLANC files are
  drafted. The first compile is the gate, and the only real unknown is what `XMP:IMPT` declares.
- The **`XFWAK` + `WaitForRestart`** experiment, so `CHAT.PLNC` can sleep instead of spinning. The
  shape to try and the exact quotes are in `SINTRAN-CHAT/README.md`.

---

## Files

- `DOC/CHAT-SEAT-LEAK-AND-XMROU-2026-08-11.md` - the full write-up of both passes
- `SRC/Xmsg.Api/Kernel/XmsgKernel.cs` - `ChooseType`
- `SRC/Xmsg.Chat/ChatServer.cs`, `ChatRoom.cs` - seat accounting, `Contains`, `MaxNicknameLength`
- `SRC/Xmsg.Chat.Tests/ChatSeatAccountingTests.cs`, `ChatRoomRulesTests.cs`
- `SRC/Xmsg.Api.Tests/XmsgKernelTests.cs` - routed and normal, with a control
- `SINTRAN-CHAT/CHATSV.PLNC`, `CHAT.PLNC`
- `SRC/Xmsg.Protocol.Tests/FaDecoderRobustnessTests.cs` - 5 tests, run and committed

1019 tests green.
