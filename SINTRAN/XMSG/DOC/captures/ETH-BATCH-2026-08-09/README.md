# APPEND-REMOTE-BATCH over Ethernet, and the first *XFTRA reply ever captured

`eth-append-remote-batch.pcapng` - captured on `\Device\NPF_Loopback`, `tcp port 5010`
(the XMSG Ethernet hub), 2026-08-09.

## What produced it

On D100, logged in as SYSTEM:

```
@TRANSFER-FILE                          (COSMOS File Transfer E02)
F-T: APPEND-REMOTE-BATCH
  Batch system and user name? D102(SYSTEM)
  Input file?                ARBTEST:SYMB
  Output file?               ARBOUT:SYMB
```

The input file deliberately does NOT exist. A batch file that DID exist would have been
executed on D102, and the plausible candidates there are things like `LOAD-MODE:BATC` -
not something to run on a machine in someone else's test network. The letter and the reply
are what this test is for, and they happen either way.

## 1. Our builder is right, on a second transport

The letter node 100 put on the wire:

```
FF06 *XFTRA   FE04 D102   F406 SYSTEM   0D02 0000
F80C ARBTEST:SYMB   F704 SYMB   0A02 0400   0B02 0003
F00B ARBOUT:SYMB 00
```

Byte for byte what `XftraRequests.AppendRemoteBatch` produces, apart from `D102` where the
2026-07-31 reference capture carried `D100` - the target machine. That includes the operation
selector `0B 02 0003` and the trailing `00` pad after the odd-length `ARBOUT:SYMB`.

**The XROUT header reads `01 41 0044`** - a declared length of 0x0044 = 68. That is the value
the `PadFinalParameter` fix produced earlier the same day; before it we emitted 0x43. So the
pad rule is now confirmed by a SECOND capture, on a DIFFERENT transport, from a different
target machine.

The reference capture was HDLC. This one is Ethernet. The XROUT letter is identical, which is
what "the transport does not change the letter" looks like when it is measured rather than
assumed.

## 2. The reply - not observed before

`XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md` records that its capture **failed at
routing**, so what a real `*XFTRA` does after receiving a letter had never been seen. Here is
the whole exchange:

```
100->102  sub=0E  F1=09F3  len 72  the letter
102->100  sub=03  F1=09F3  len  0  short ack
100->102  sub=03  F1=09F5  len  0  our ack
102->100  sub=0E  F1=09F5  len  8  01 16 0004 01 02 002E     <- THE REPLY
```

Decoded:

| Byte | Value | Meaning |
|---|---|---|
| 0 | `01` | serial, ECHOED from our letter |
| 1 | `16` | the service byte, overwritten by XROUT with a return status |
| 2-3 | `0044`->`0004` | remainder length, 4 bytes |
| 4-7 | `01 02 002E` | integer parameter 1 = **46** |

**46 is SINTRAN error "NO SUCH FILE NAME"** - confirmed in ND's own error tables
(`ND-60.074.01` and `ND-60.145.7A`, which agree), and it is exactly what the terminal printed:

```
*** Error in accessing: D102(SYSTEM).ARBTEST:SYMB
Sintran file system error:
NO SUCH FILE NAME
```

So a failed `*XFTRA` answers with an eight-byte XROUT reply carrying the SINTRAN error number
as integer parameter 1. The serial is echoed, which is how a client matches the reply to its
request.

## What is still NOT known

 - **What status byte `0x16` means.** It is recorded, not explained. One observation, one
   error case - nothing separates "this particular failure" from "any failure" from "a
   constant".
 - **What a SUCCESSFUL reply looks like.** This run failed on purpose. A successful batch
   submission has still never been captured, and its reply may carry different parameters.
   That needs a real batch file on the remote, which needs a way to author one.

## Related

 - `DOC/XMSG-APPEND-REMOTE-BATCH-CAPTURED-2026-07-31.md` - the HDLC reference capture.
 - `SRC/Xmsg.Api/Xrout/XftraRequests.cs` - the builder this validates.
