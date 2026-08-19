# SOLVED: the chat Say was always delivered - the load run stopped listening first

**The protocol was never wrong.** This document previously argued that the frames were correct and
the delivery was broken, and named framing, addressing and `answeredFlags1` as suspects. All three
were innocent. It is kept, corrected, because the way it was wrong is the useful part.

## The answer

**A `Said` is a BROADCAST the room makes AFTER it receives a line - it is not an answer to the
sender.** So it cannot possibly have arrived by the time the line that caused it has been sent.

The load run went straight from its last line to leaving, and from leaving to printing the summary.
Nothing was listening when the broadcasts came back, so every run reported

```
[chatload] Said messages heard : 0
```

and that zero was read as "never delivered". It meant "never waited for".

**Fixed** by adding two settle phases - keep pumping for twenty ticks after the talking, and again
after the leaving. Measured immediately afterwards, three users saying two lines each:

```
[chatload] users asked to join : 3
[chatload] welcomed            : 3
[chatload] frames from the room: 27
[chatload] Said messages heard : 18
[chatload] leaves sent         : 3
```

**The arithmetic is exact.** 3 users x 2 lines = 6 lines said, each broadcast to all 3 members =
**18**. And 27 frames = 3 welcomes + 3 `Joined` + 18 `Said` + 3 `Left`. Seats afterwards:
`CHAT-LOBBY 16 free`, exactly the baseline.

**So the live chat works end to end over the wire**: C# clients join a real PLANC `CHATSV` on an
ND-100, talk, hear each other, leave, and every connection seat comes back.

## What misled me, and the lesson

The evidence that should have settled it was already in hand and pointed the other way:

 - **the leaves worked.** A leave is an ordinary port-to-port message, sent by the same code, with
   the same flags, to the same port as a Say - and the seats came back, which only happens if
   `CHATSV` received and acted on it. If leaves arrive, Says arrive.
 - **the server printed nothing.** That was read as "it never saw them", but a successful broadcast
   is SILENT. Absence of output was not absence of the message.

**A count of zero from your own instrument is a claim about the instrument first.** The framing
change made on the strength of that zero was a real change - our ordinary messages now carry the
`0x82`/`0x84` the machine itself uses - but it fixed nothing, and it was kept only because matching
the machine is right on its own terms.

Related: `DOC/BRINGUP-ORDER-AND-TRAPS-2026-08-18.md` for reading `LIST-UTILIZATION`, where the
20-user burst shows `Data transmit blocks 10*` - the one resource that genuinely does run out.
