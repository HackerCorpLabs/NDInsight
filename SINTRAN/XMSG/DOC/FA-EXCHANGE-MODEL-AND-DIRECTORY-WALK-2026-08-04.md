# COSMOS FA: the exchange model, the directory walk, and the teardown (2026-08-04)

How a `*FA-SERVER` conversation is actually sequenced. This is the layer ABOVE
[FA-READ-WRITE-WIRE-PROTOCOL-2026-08-04.md](FA-READ-WRITE-WIRE-PROTOCOL-2026-08-04.md), which
covers what a single request and reply CONTAIN. This document covers when they are sent, what
number they carry, and how the conversation ends.

Everything here was found by writing a C# FA server, pointing a real SINTRAN III machine (D100) at
it, and comparing what it did against the real server-to-server captures in
`DOC\captures\FA-READ-WRITE-2026-08-04\`. The reference conversation throughout is
`capture-list-files.txt` - D100 listing a directory on D102, more than a hundred files.

---

## 1. VERIFIED - a request is answered by a ShortAck, not by its reply

This is the rule the whole thing turns on, and getting it wrong is what produced weeks of
unexplained rejections.

A request is answered by an eight-byte **ShortAck** (`0x07A2`) carrying the request's Flags 1. The
actual reply is a **new exchange one Flags 1 higher**, which the client then acknowledges with a
ShortAck of its own:

```
f1=018D  100->102  07F0 SiiiSpecial request  sess=8100D761
f1=018D  102->100  07A2 ShortAck             sess=0200922A   <- THE answer
f1=018E  102->100  07F0 the actual reply     sess=810090BB   <- a NEW exchange
f1=018E  100->102  07A2 ShortAck             sess=02008485
f1=018F  100->102  the next request
```

That pattern repeats without a break through the whole listing. The ShortAck counters run 1, 2, 3
and up on each side independently - the server's carry the constant `0x922A`, the client's
`0x8485`.

### What goes wrong if you send the reply alone

Send the reply stamped with the request's Flags 1 and skip the ShortAck, and your exchange count
runs one behind the peer's from the first request onward. The peer answers with a subtype `0x07`
NetworkError whose Flags 2 is `0xFFDE` - **XENSE, -34, a SEQUENCING reject**.

Live on 2026-08-04, D100 accepted the first such reply and rejected the second, which made the
fault look content-related rather than sequencing-related. It was not. The rule is simply that the
number must be right.

### Why this hid for so long

On the CONNECT handshake, "echo the request's Flags 1" and "keep your own running number" produce
**identical bytes**, because every request there gets exactly one data answer. The two models are
indistinguishable. Our `ChooseFlags1` was carved from exactly that handshake, and its own comment
said the echo rule was UNVERIFIED - correctly, as it turned out.

Only a directory listing separates the two models, because a listing contains exchanges (the
ShortAcks) that carry no reply. **If a sequencing rule was carved from the connect alone, treat it
as unproven.**

---

## 2. VERIFIED - three fields belong to the frame you are SENDING

The single root cause behind several separate bugs was building a reply by copying header fields
off the request. Only one of these three is genuinely an echo.

| Field | Rule | How it was proved |
| --- | --- | --- |
| **Flags 1** | The ShortAck echoes the request's. The reply ORIGINATES `f1 + 1`. | Section 1 |
| **XMCSM / Flags 2** | This frame's OWN body length, in bytes from offset 28. | Every Data frame in all four captures |
| **session-header byte** | ECHOED from the request. | `8100D761` request pairs with `810090BB` reply, `8200D761` with `820090BB`, unbroken |

On XMCSM: an 8-byte ShortAck carries `0x0008`, a 98-byte directory reply `0x0062`, a 34-byte
connect letter `0x0022`. Echoing the request's value made a 97-byte reply declare 100 and a
17-byte reply declare 112.

On the session-header byte: an own-counter (`0x80 + repliesBuilt`) agrees with the echo only while
every request yields exactly one reply, and drifts the moment one does not - the same class of
mistake as deriving Flags 1.

The responder session TOKEN (the word after it) is a different matter: it reads `0x9081`, `0x909E`
and `0x90BB` across the recordings we hold. It is the server's own per-conversation value and
varies. **Do not treat a mismatch there as a defect.**

---

## 3. VERIFIED - the SERVER holds the directory-walk position

The cursor in a listing request - the `A2 xxxx` field inside the request's `8C` block - is **not an
index**.

Across a walk of more than a hundred files, `capture-list-files.txt` carries:

```
A2 FFFF     exactly ONCE   (the first request)
A2 0000     102 times      (every later request)
```

and the real server hands back a **different file every time**. So the cursor only chooses between
two behaviours:

```
A2 FFFF  = start the walk again
A2 0000  = give me the NEXT one
```

The position belongs to the server. Read the cursor as an index and every request after the first
asks for index 0 and gets the same file back forever - which is exactly what happened live: D100
received `HELLO:TXT` twice and abandoned the listing.

**INFERRED, not proven:** that the snapshot should be taken once when the walk starts rather than
re-listed per request. We do it that way so a file created mid-walk cannot shift later entries, but
the capture does not show a folder changing mid-walk.

---

## 4. VERIFIED - how a listing ends, and how the conversation is torn down

The tail of `capture-list-files.txt`, in wire order:

```
f1=01F4  102->100  07F0 SiiiSpecial reply, 24 bytes   <- short: no more entries
f1=01F5  100->102  07F0 ReleaseFileEntry
f1=01F5  102->100  07A2 ShortAck
f1=01F6  102->100  07F0 ReleaseFileEntry reply
f1=01F6  100->102  07A2 ShortAck
f1=01F7  100->102  0782 conv=0008  0002 8000 ....     <- "I am finished", 10 bytes
f1=01F7  102->100  07C0 Close
```

Two things to note.

**The end of the directory is a SHORT reply** - 24 bytes against about 126 for a real entry. The
client keeps asking until it gets one.

**`0x0782` means "finished" and its answer is a Close** (`0x07C0`), not a refusal. It carries no
operation/sequence pair, so a server that only knows how to parse requests will treat it as
malformed. Answering it with an error makes the client abandon the session and reconnect in a loop,
forever.

The very last frame in the capture is a subtype `0x07` NetworkError carrying `0xFFED` (XEIMA, -19,
invalid magic) from D100. That appears to be normal teardown noise after the Close, not a fault.

### The one thing here that is NOT verified

D100 sends **`0x0781`**, not `0x0782`. Same ten bytes, same layout, same point in the conversation.
`0x0781` appears in **none** of the four captures.

Treating the two alike is **INFERRED from that structural match, not proven.** What the low bit
distinguishes is **UNKNOWN**. A plausible reading is "ended early" against the captured "ran to the
end of the directory", since our served folder holds two files and the captured listing held more
than a hundred - but that is a guess and is recorded here as one. A capture showing both forms in
one session would settle it.

---

## 5. What this changed in the code

Unlike the READ/WRITE document, this one is written after the changes, and all of it is applied.

| File | Change |
| --- | --- |
| `Xmsg.Servers\Fa\FaServer.cs` | `AckThenReply` sends the ShortAck then the reply; XMCSM is the reply's own length; `0x078x` answered with a Close |
| `Xmsg.Servers\Fa\FaServerSession.cs` | `WalkPosition` (server-held) and `NextShortAckCounter` |
| `Xmsg.Protocol\Fa\FaServerConversation.cs` | `BuildReply` takes the echoed session-header byte instead of counting its own |
| `Xmsg.Protocol\Fa\FaMessageType.cs` | `SessionFinished` (`0x0782`, verified) and `SessionFinishedAlternate` (`0x0781`, observed only) |
| `Xmsg.Node\Services\XmsgServerHost.cs` | `ChooseFlags1` is the one place Flags 1 is decided |

Tests worth knowing about:

- `FaServerWiringTests.AssertAckThenReply` - asserts the two-frame shape, the Flags 1 step, and
  that both frames declare their own body length. Falsifiable against the capture.
- `FaServerWiringTests.DirectoryWalk_AdvancesOnEachRequest` - two successive "next" requests must
  return different files. Fails by construction under the old index reading.
- `FaServerConversationTests` - replays real captured replies byte for byte, feeding back the
  capture's own session-header byte, so a wrong echo fails the comparison.

`XmsgNode.ResyncAcceptDown` ("step the accept down one per XENSE") is this same bug seen from the
other end and patched locally. It should be revisited now the model is right.

---

## 6. Still unknown

1. What the low bit of `0x0781` / `0x0782` distinguishes (section 4).
2. Whether the walk snapshot should be taken once or refreshed per request (section 3).
3. What the responder session token (`0x9081` / `0x909E` / `0x90BB`) actually is.
4. Whether the short end-of-directory reply has a distinguishing field, or whether clients simply
   detect it by length.
5. Whether the ShortAck counters wrap, and at what.
