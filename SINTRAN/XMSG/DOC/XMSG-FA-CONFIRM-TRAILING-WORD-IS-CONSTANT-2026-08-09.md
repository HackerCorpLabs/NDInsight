# The FA connection confirmation's last word is a CONSTANT, not a system number

**Date:** 2026-08-09
**Status:** MEASURED from five real connection confirmations across seven captures
**Code:** `SRC/Xmsg.Protocol/Fa/FaExchangeCodec.cs` (`ConfirmTrailingWord`),
`SRC/Xmsg.Protocol/Fa/FaServerConversation.cs` (`BuildConnectionConfirm`),
`SRC/Xmsg.Servers/Fa/FaServer.cs` (`ConfirmSystemByte`, deleted)

---

## What we used to send

The `*FA-SERVER` connection confirmation is an eight-byte body:

```
07D2 <echoed letter word> <our connection number> <word 3>
```

Word 3 was modelled as "the system number byte, `0x64` = 100 in every capture", and
`FaServer.ConfirmSystemByte` built it from the **client's** node number. Its own comment
admitted the model was shaky:

> **INFERRED - the two captures disagree on what this byte names.** ... The client reading
> is the one consistent with both, so it is used here - but it is a reading, not a carved
> fact.

The reading was wrong.

## What every real machine actually sends

Every connection confirmation in every capture, with the node numbers of both ends:

| Capture | Server | Client | Confirmation |
|---|---|---|---|
| `claude-file-stat-102-to-100-2026-07-29` | D100 `0x64` | D102 `0x66` | `07D2 0002 0046 6400` |
| `claude-OPENONLY-102-to-100-2026-07-30` | D100 `0x64` | D102 `0x66` | `07D2 0002 0055 6400` |
| `claude-create-file-NAMELEN-102-to-100-2026-07-30` | D100 `0x64` | D102 `0x66` | `07D2 0002 004D 6400` |
| `ND-TO-ND-2026-08-08/nd-to-nd-scenarios` | D102 `0x66` | D103 `0x67` | `07D2 0008 003F 6400` |

**The last row settles it.** The server is `0x66` and the client is `0x67`, and the word is
still `0x6400`. It is neither side's node number.

The first three rows agreed with "the server's own number" only because that server *was*
node 100, and they agreed with nothing about the client at all - so the client model was
never supported by them either.

### The connect letters say the same thing

Every connect letter carries the same value in its trailing extras, `07E2 0000 <echo> 6400`.
That includes a letter sent from **node 103 to node 19999** in
`ND-TO-ND-2026-08-08/fa-edge-cases.pcapng`, where neither endpoint is node 100:

```
07 e2 00 00 00 06 64 00
```

Counts across the captures, all identical in the trailing `64 00`:

```
nd-to-nd.pcapng                 6x ...0002 6400   6x ...0004 6400   3x ...0006 6400
nd-to-nd-scenarios.pcapng       3x ...0008 6400   3x ...000C 6400
verify-conversation-word.pcapng 3x ...0002 6400   3x ...0004 6400
nd-to-nd-fa-ops.pcapng          3x ...0002 6400   3x ...0004 6400
fa-edge-cases.pcapng            3x ...0006 6400
```

## A second, independent reason it was never a system number

**An ND system number is 16 bits.** A single byte cannot hold one whatever its value, so
"the system-number byte" was structurally impossible before any capture was consulted. The
field is a 16-bit word at offset 6, and it now reads and writes as one.

## What was sent on the wire

Against D103 (`0x67`) our server emitted:

```
07D2 0006 0B02 6700      <- ours, the client's node number
07D2 0008 003F 6400      <- what a real ND emits in the same position
```

No real machine has ever put anything but `0x6400` there.

## The correction

`FaExchangeCodec.ConfirmTrailingWord = 0x6400`, documented as **meaning UNKNOWN** and
reproduced because it is what every real machine emits. `ConfirmSystemByte` is deleted.
`BuildConnectionConfirm` takes a `ushort trailingWord` and writes a word, not a byte and a
pad.

## Why this hid

The same shape as the conversation-word defect fixed the day before: **a value that never
varies across the captures you have cannot be told apart from a value you derived**, and
three of the four confirmations came from a server that happened to be node 100. It took a
capture where neither endpoint was node 100 to break the tie.

Two rules this reinforces:

 1. When a field is modelled as "derived from X", find a capture where X differs from the
    observed value. If no such capture exists, the model is untested, not confirmed.
 2. Check the field WIDTH against what the thing is. A 16-bit identifier in an 8-bit field
    is a modelling error visible without any capture at all.

## Still unknown

What `0x6400` means. It is not a node number, and it is not a length. It is reproduced as a
constant and left unexplained rather than guessed at.

## Related

 - `SINTRAN/XMSG/DOC/captures/ND-TO-ND-2026-08-08/RIG.md` - the D102/D103 capture rig
 - `FaServerConversation.ResponderConversation` - the same failure mode, one day earlier
