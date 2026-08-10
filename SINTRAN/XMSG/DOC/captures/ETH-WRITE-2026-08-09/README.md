# A large file written D100 -> D102 over the Ethernet hub (2026-08-09)

`eth-write-blocksize.pcapng` - 12985 frames, 1.5 MB, captured on `\Device\NPF_Loopback`
filtered to `tcp port 5010` (the XMSG Ethernet hub).

## What produced it

On D100, logged in as SYSTEM:

```
CREATE-FILE D102(SYSTEM).BLKTST99:DATA,0
COPY-FILE                                  (interactive)
  DESTINATION FILE: D102.(SYSTEM)BLKTST99:DATA
  SOURCE FILE:      BRF-LINKER-C01:PROG
```

Took 6.9 s and returned to `@` with no error.

## COPY-FILE semantics, learned the hard way

 - The destination must **already exist**. `COPY-FILE` to a name that does not exist gives
   `NO SUCH FILE NAME`, so `CREATE-FILE` it first.
 - `CREATE-FILE D102(SYSTEM).NAME:TYPE,0` works - note the dot AFTER the parenthesis.
 - `LIST-FILES` and `COPY-FILE` write the remote prefix the other way round:
   `D102.(SYSTEM)NAME:TYPE`.
 - The positional form `COPY-FILE "dest","source"` kept answering `FILE ALREADY EXISTS` and
   naming the SOURCE. The interactive form worked. What the positional form actually parses is
   NOT established - do not copy that syntax from here.

## Measured, and trustworthy

```
SetBlockSize asked for   2048          (the same value the 2026-08-04 HDLC capture used)
SetEndOfFile carried     155647        so the file is 155648 bytes
```

## Measured, and NOT trustworthy - the decode is incomplete

Counting through the hub framing gave 44 WriteFile requests and 88 content messages per
session, each message a fragment pair of 594 + 424 = 1018 body bytes.

**Those numbers cannot be right.** 88 x 1018 = 89584 bytes, and the file is 155648. The
fragments counted cannot physically carry the file, so roughly 42% of them are being missed -
the hub-stream reassembly used to read this file drops frames on a transfer this large.

So **no block-size conclusion is drawn from this capture**. The open question from
`FaWriteLadder` - how the declared 2048 relates to what is actually carried - is still open,
and this capture will answer it once the reassembly is fixed.

Note the hub is a broadcast hub with three members, so every frame appears on three TCP
streams: the sender's, and one per forwarded member. Any counting must divide by the member
count or filter to one direction, which is a trap for the next reader as much as it was here.

## Still to take from this capture

 - Fix the reassembly, then recount. 44 writes for 155648 bytes would put a block at about
   3538 bytes, which does NOT match the 2048 asked for - but that figure rests on the same
   suspect count, so it is a lead and not a finding.
 - It also holds a full connect, reserve, open, 44 writes, SetEndOfFile, close and release on
   the ETHERNET transport, where every earlier FA capture was HDLC.
