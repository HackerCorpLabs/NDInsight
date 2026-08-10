# The FA READ framing, carved from a known file (2026-08-10)

How a COSMOS `*FA-SERVER` sends file content back to a reader, derived byte for byte and
then proved by rebuilding the file.

**Specimen**: `captures/ND-TO-ND-WRITE-2026-08-10/readback-10-blocks.pcapng` - D102 reading
`BIGPSH3:TXT` from D100. Both are real ND machines; no C# in the path.

**Why this specimen settles it**: the file's 20400 bytes were known exactly in advance (300
lines of a fixed pattern), so every field could be found by ALIGNING wire bytes against
known plaintext rather than by guessing at a layout. Nothing here is inferred.

---

## The rule

A content message is **TWO fragments sharing ONE Flags 1**, and the two have *different*
header shapes:

```
fragment    subtype   total     header                                        content
--------    -------   -------   -------------------------------------------   -------
first       0x0A      594 B     SINTRAN 14 + XMSG sub-header 14 + FA env 8     586 B
continuation 0x0C     452 B     SINTRAN 14 only - NO XMSG SUB-HEADER            438 B
                                                          per message  =      1024 B
```

 - **The continuation carries no XMSG sub-header.** That is the trap: a decoder that
   assumes the body always starts 28 bytes in silently discards 14 bytes of FILE CONTENT
   on every continuation fragment.
 - The FA envelope on the first fragment is the usual 8 bytes:
   `07F0 <conversation> <counter> <token>`.
 - **1024 content bytes per message, 2 messages per block = 2048**, which is exactly the
   value `SetBlockSize` asked for. The read side and the write side use the same framing.
 - **The last block is PADDED, never shortened.** 20400 bytes came back as 20 messages =
   20480 bytes, the final 80 being pad.

## Proof

Rebuilding the stream with that rule and nothing else:

```
rebuilt 20480 bytes from the capture ; source is 20400 bytes
first 20400 bytes: 20400 identical (100.0000%)
*** EXACT MATCH: the rebuilt stream IS the source file, byte for byte ***
```

## How the 14 bytes were found

A naive decode (body always at +28) reassembled 259 of 300 lines. The 41 that failed were
not random - they sat at offsets `508, 504, 500, 496, 492` and `32, 28, 24, 20, 16, 12`
within 512-byte blocks, on a regular seven-and-a-half-line period. **Periodic damage is a
framing bug; random damage is corruption.** Aligning each fragment against the source then
gave the offsets directly:

```
sub=0a len=594  header 8   -> source offset 0
sub=0c len=424  header 0   -> source offset 600     <- 586 + 14 = 600, the missing sub-header
sub=0a len=594  header 8   -> source offset 1024
```

## The read ladder

Same shape as the write ladder, with the server sending the content:

**CORRECTED 2026-08-10.** The first version of this section listed the requests by their
message numbers and called two of them unexplained. They are not: every operation in the read
ladder is one this library already had a name for, and reading the QFORM out rather than the
message numbers settles both. What follows is the whole ladder with its operation codes.

```
client  connect letter (FA 1B41)
server  ConnectionConfirm 07D2
client  92 0002 seq 0001   ReserveFileEntry  (112 B) the asker and user, same as a write
client  92 0005 seq 0002   OpenFile          F2 0002 BD "BIGPSH3:TXT'."
server    reply                              F2 0002 A2 0040       the file number
                                             F2 0003 A4 00004FB0   THE BYTE LENGTH, 20400
client  92 0007 seq 0003   SetBlockSize      A2 0800 = 2048
client  92 000C seq 0004   SiiiSpecial       F2 0001 92 0021 = FileInformation
server    reply (88 B)                       the 64-byte directory entry
client  92 0008 seq 0005   ReadFile          A4 00000000   block 0
   ... ten in all, positions 0 to 9, sequences 0005 8006 0007 8008 ... 800E ...
client  92 0006 seq 000F   CloseFile
client  92 0003 seq 0010   ReleaseFileEntry
```

Each exchange is acknowledged the same way as on the write side: the server short-acks the
request, replies as a new exchange, and the client short-acks that. **A block step costs the
client THREE short acks** - one for the reply and one for each of the two content messages.

## The two things this used to call unknown

 - **`920021` is not a mystery field.** `92 000C` is the OPERATION - `SiiiSpecial` - and
   `0x0021` under selector 1 is its SUB-FUNCTION, `FaSpecialFunction.FileInformation`, which
   our own enum has documented since 2026-08-06 along with the note that refusing it made a
   real `COPY-FILE` give up without ever issuing a read. The 88-byte reply is that
   sub-function's answer: the 64-byte directory entry, the same record a listing hands out.
   Both were already written up; the carve had simply read the message numbers off the
   summary instead of decoding the bodies. **Grep your own library before declaring
   something unknown.**
 - **The `0x8000` sequence bit is EXPLAINED.** `FaWriteLadder` recorded the alternation
   `0004 8005 0006 8007 ...` and said its meaning was unknown and must not be invented. With
   a read capture beside it the rule falls out, because the two ladders start their blocks at
   different parities:

   ```
   write blocks   0004 8005 0006 8007 0008 8009 000A 800B 000C
   read blocks    0005 8006 0007 8008 0009 800A 000B 800C 000D 800E
   ```

   It is not the parity of the number - `0005` is odd and clear, `8005` is odd and set. It
   ALTERNATES across block requests, starting CLEAR on the first. Epilogue steps never carry
   it, including the write's `000D`, which falls where the alternation would have set it.
   Two captures, nineteen block requests, no exception.

   **What it MEANS is still not established** - only when it is set. Our own push omits it
   across ten blocks and completes, verified by a second real ND, so a server does not
   require it. It stays documented and unsent; `FaReadLadder.CapturedSequenceBitSetForBlock`
   holds the rule and a test checks it against both captures.

## The reader's stopping rule

`0x00004FB0` in the open reply is 20400, exactly the file that had been pushed, and
`ceil(20400 / 2048)` is the ten `ReadFile` requests the capture sends. That is the whole
stopping rule: **the last block is padded, never shortened, and there is no end marker
anywhere in the transfer**, so a reader that ignores the open reply's length gets the file
plus up to 2047 bytes of padding with nothing to say which is which.

## Still not established

The remaining fields of the 64-byte directory entry beyond the block count and byte length
that `FaSpecialFunction.FileInformation` already documents. Not needed to decode the content,
and not guessed at here.

## The client that implements this

Built 2026-08-10, in `SRC/`:

```
Xmsg.Protocol/Fa/FaReadLadder.cs        the ladder above, and the block arithmetic
Xmsg.Protocol/Fa/FaReadRequests.cs      the three requests that differ from a write
Xmsg.Protocol/Fa/FaClientReadSession.cs the sequencing, including the three acks per block
Xmsg.Servers/Fa/FaReadDriver.cs         ladder to datagrams, and content back to a file
Xmsg.Live.Runner/FaPullRun.cs           --pull on the runner
```

The transport needed NO change. `SintranFragmentReassembler` and
`SintranMessageFragment.ContinuationBodyOffset` already had the continuation rule right - the
14-byte trap this document describes was in the Python decoder used to carve it, not in the
C#. So this carve CONFIRMED the library rather than correcting it.

Proved offline before any live run, the same way the push was: `FaReadDriverTests` drives the
real driver against a simulated server and reassembles a 20400-byte file byte for byte,
padding trimmed. The request bytes are pinned against this capture in `FaReadLadderTests`.

## Related

 - `captures/ND-TO-ND-WRITE-2026-08-10/README.md` - the write ladder, the same way
 - `XMSG-PROTOCOL.md` section 5.x - bodies must be an even number of bytes
