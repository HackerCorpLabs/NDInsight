# A real ND client writing a file to a real ND file server (2026-08-10)

`nd-to-nd-write.pcapng` - captured on `\Device\NPF_Loopback` filtered to `tcp port 5010`,
the XMSG Ethernet hub. **No C# anywhere in the path**: D102 is the client, D100 is the
server, and both are RetroCore machines.

Taken to answer one question: **what does a real client's short acknowledgement look
like**, because D100 refuses ours and re-sends its reply ten times instead
(`fa-push-connect-letter-rejected-by-d100`).

## What produced it

On D102, logged in as SYSTEM:

```
COPY-FILE D100(SYSTEM)."NDWR811:TXT",ARBJOB:SYMB
COPY-FILE D100(SYSTEM)."NDWR812:LIST",ENCOS-LOAD-0-B01:LIST
```

Both returned to `@` with no error, and `LIST-FILES D100(SYSTEM).NDWR810,,` confirmed
`FILE 64 : D100.(PACK-ONE:SYSTEM)NDWR810:TXT;1` from an identical earlier run. So this is
a **successful** write ladder, start to finish, not a failure being studied.

The capture holds the tail of the `NDWR811` session (conversation `0043`, already running
when the capture started) and **the whole of the `NDWR812` session** (conversation `0044`):
connect letter, confirm, the ladder, Release and Close.

## Decoding it

```
python decode_hub.py nd-to-nd-write.pcapng      # one line per SINTRAN datagram
python fa_view.py    nd-to-nd-write.pcapng      # the FA layer, named fields
python check_rules.py nd-to-nd-write.pcapng     # the two rules below, checked
```

`fa-ladder.txt` is the `fa_view.py` output, kept so the ladder can be read without a
Wireshark install.

**The hub is a broadcast hub.** A frame sent once is forwarded to every other member, so
it appears on several TCP streams and any naive count is multiplied by the member count -
that is what spoiled the block-size numbers in `ETH-WRITE-2026-08-09`. `decode_hub.py`
keeps **only the machine-to-hub direction** (`tcp.dstport == 5010`), which is the frame's
originator, so every frame is counted exactly once.

Framing is per `DOC/COSMOS-ETHERNET-TRANSPORT-FRAMING-2026-08-01.md`: a 2-byte big-endian
length prefix, then the 802.3 frame, LLC `A8 A8 03`, the 11-byte ND link header, then the
SINTRAN header. Only link kind `0x20` (DT) carries a payload.

The decoder was validated against `ETH-BATCH-SUCCESS-2026-08-09` first: it reproduces that
capture's known `*XFTRA` letter and its closing `XEIMA` (`f2=ffed`) reject.

---

## 1. The short acknowledgement [VERIFIED]

Eight bytes of FA body, in both directions:

```
07A2 <conversation> <counter> <trailer>
```

| field | client (D102) | server (D100) |
|---|---|---|
| conversation | the number the SERVER assigned (`0043`, `0044`) | the word it ECHOED from the letter (`0004`, `0006`) |
| counter | `0100`, `0200`, `0300` ... | `0100`, `0200`, `0300` ... |
| trailer | **`8485`** | **`922A`** |

The conversation rule is the one already recorded in
`fa-server-conversation-word-is-echoed`, now confirmed a third time on a fresh pair of
numbers.

**The trailer differs by role.** `8485` is what a client sends and `922A` is what a server
sends; neither side ever sends the other's. Our push sends `8485` and is the client, which
is correct.

### The counter is per SESSION, starts at 0x0100, and steps by 0x0100

Measured runs, per sender and source port, across the session boundary:

```
D100 port 05b9 : 0600 0700 0800 0900 0a00 0b00 0c00 0d00 | 0100 0200 0300 0400 0500 ...
D102 port 03f9 : 0600 0700 0800 0900 0a00 0b00           | 0100 0200 0300 0400 0500 ...
                                                         ^ new conversation - RESET
```

Both sides reset to `0100` when the new conversation opens, on the **same** port. So the
counter belongs to the conversation, not to the port and not to the process.

The `0043` session starts at `0600` only because it was already running when the capture
began.

**This clears our own value.** Our push sends `0100` as the first short acknowledgement of
a session, and a real client sends `0100` too. The counter is NOT the reason D100 refuses
it.

### The server acknowledges more messages than the client does

Across the capture D100 sent 17 short acknowledgements and D102 sent 13. The extra ones
sit next to the content messages - D100 answers those too, and the client does not answer
back. So the counters run at different rates on the two sides by design, and neither side
can be checked against the other's.

---

## 2. Flags 1: each sender counts for itself [CORRECTED 2026-08-10]

> **An earlier revision of this section claimed Flags 1 is ONE sequence shared by the pair,
> and flagged a contradiction with `xmsg-flags1-is-per-sender-not-shared`. That was WRONG,
> and the wrong reading is kept here because it is an easy one to make twice.**

In this capture the two machines' numbers do interleave into what looks like a single
`+1` series:

```
D102 0bb7   D100 0bb8   D102 0bb9   D100 0bba   D102 0bbb ...
```

But that is **not** evidence of a shared pool. The write ladder is a strict ping-pong -
each side sends exactly one message per turn - so two INDEPENDENT counters that both step
by one, and that happen to start level, stay level forever. The shared-pool reading and
the per-sender reading predict identical bytes here, so this capture cannot tell them
apart. It was over-read.

**A capture of our own node settles it**, because there the two counters are NOT level -
D19999 was at `0x001B` while D100 was at `0x0023`, eight apart - and both sides carried on
happily at the datagram layer:

```
100   -> 19999  f1=0023  ConnectLetter
19999 -> 100    f1=0023  DATAGRAM-ACK      <- echoes the ORIGINATOR's number
19999 -> 100    f1=001b  ConnectConfirm    <- our own counter, NOT 0024
100   -> 19999  f1=001b  DATAGRAM-ACK      <- D100 echoes ours
```

So: **each sender keeps its own counter, and an answer echoes the number it answers.**
That is exactly what `xmsg-flags1-is-per-sender-not-shared` already said. There is no
contradiction, and nothing about Flags 1 needs changing.

---

## 2b. What the per-sender rule cost us - a real defect, found by this capture

Chasing section 2 into our own code found the bug that had been killing the live push.
`XmsgServerHost.ChooseFlags1` ECHOED correctly, but while echoing it also dragged our OWN
counter forward:

```csharp
ushort following = (ushort)(echoed + 1);
if (following > link.NextFlags1) { link.NextFlags1 = following; }   // WRONG - removed
```

That is the shared-pool model expressed as code, and it is a no-op while both sides are
level - which is why every offline test passed and why it survived so long. On the hub the
sides are NOT level. Measured 2026-08-10, D100 running eight ahead of us:

```
our originations : 001b 001c 001d 001e 001f 0020 0021 -> 0029
                                                          ^ 0x0028 echoed, +1, ratcheted
no number reused, seven skipped (0022..0028)
D100's answer    : XENSE (subtype 0x07, Flags 2 0xFFDE) on our 0029, then on everything
                   after it - 127 rejects - then the link torn down with ND kind 0x6F
```

`D100`'s own `LIST-SYSTEM` agreed: its receive counter sat at 34 = `0x0022`, the number it
was waiting for, and our persisted store held `0x0022` too. **Only the in-memory link
counter was wrong**; nothing on disk was corrupt, which is why a restart appeared to help
and then failed again.

Regression test: `ResponderSequenceStoreTests.AnsweringAPeerThatIsAheadDoesNotDragOurOwnCountForward`,
which forces the two counters apart and fails against the old code.

**The lesson worth keeping:** the peer's expectation of US is a value it never transmits.
No capture states it. It can only be inferred from what the peer REJECTS, or asked for
directly with `X-C` `LIST-SYSTEM` - which is the fastest check available and was not used
for weeks.

## 3. A complete reference ladder

Session `0044`, every message in order, is in `fa-ladder.txt`. Its shape:

```
client  connect letter (FA 1b41, XROUT serial 1b service 41)
server  ConnectConfirm 07d2, assigns conversation 0044
  then, repeated per step:
client  request  07f0 0044 <counter> ...
server  ShortAck 07a2 0006 <counter> 922a
server  reply    07f0 0006 <counter> 9081 ...
client  ShortAck 07a2 0044 <counter> 8485
  and finally:
client  Release  0782 0044 0006 8000 0000
server  Close    07c0 0006 0044 0000
```

Every data frame is separately acknowledged at the datagram layer with a subtype `0x03`
message carrying Flags 2 `0001`. That layer and the FA short acknowledgement are two
independent things and must not be conflated.

---

## What this capture does NOT settle

It does not say why D100 refuses OUR short acknowledgement. Every field of ours that this
capture can check - conversation number, counter, trailer, destination port, the Flags 1
echo - matches what a real client sends. The next step is to capture **our own** push with
`decode_hub.py` and diff it against `fa-ladder.txt` message by message, which is the first
time the two will be readable in the same format.
