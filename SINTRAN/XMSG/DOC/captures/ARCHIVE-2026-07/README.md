# Capture archive - 2026-07-29 to 2026-08-01

The 35 recordings the XMSG and COSMOS work was built from, before captures started being
stored in this repository. Everything the FA file-server implementation was derived from is
here: FILE-STATISTICS, CREATE-FILE, DELETE-FILE, LIST-FILES, OPEN/CLOSE, the file transfers,
APPEND-REMOTE-BATCH, CONNECT-TO, list-routing and the Ethernet route-through runs.

**These are COSMOS/XMSG captures.** There is no X.25 in them. They previously sat in another
project's working directory purely because that is where the capture tool wrote them at the
time; 22 of the 35 were not version-controlled anywhere. They are copied here so the evidence
lives with the code that depends on it. The originals were not modified.

## Two transports, and they decode differently

Read the TCP port before choosing a decoder - guessing wrong produces convincing nonsense.

**HDLC/LAPB captures** (ports 1036x) decode with `hdlc_tcp.lua`, which is installed globally.
Just open them.

**Ethernet captures** (port 5010, the XMSG Ethernet hub) do NOT. Wireshark claims 5010 as
IPSICTL, so the dissector never runs.

Do **not** force it with `-d tcp.port==5010,hdlc_lapb`. It looks like it works - thousands of
frames turn into LAPB - but the decode is misaligned and the output reads like data while
being noise: node numbers come out as `53306 -> 1` instead of 100/102/103, and only a handful
of SINTRAN blocks appear in a whole file.

The real layout of the TCP payload on 5010:

```
0-1     2-byte big-endian length prefix       the hub's own framing
+14     IEEE 802.3: dst MAC, src MAC, length  the MAC carries the node number:
                                              08:00:26:64:00:00 = 100, ..66.. = 102
+14     ND link header, starts a8 a8 03 0b    see NdLinkHeader
+14     the ordinary SINTRAN header, 21 13    e.g. 21 13 00 0E 00 64 00 66 00 04 00 22 DD EE
                                              = subtype 0E, dest 100, src 102, Flags1 0x0004
        then the XMSG sub-header and body, exactly as on HDLC
```

So the SINTRAN header sits at payload offset **30**. An 802.3 length of 14 is a link-level ack
carrying no SINTRAN header - skip those. Above the ND link header everything is identical to
the HDLC captures, so all the usual decoding applies once the three wrappers are stripped.

Port 5010 is a LIVE hub port when the Ethernet work is running. Read these FILES; do not
connect to it.

## What these settled

 - The FA exchange model: a ShortAck answers a request, the reply is a NEW exchange one
   Flags 1 higher. Unbroken through a 100-file listing.
 - The FA listing record IS the SINTRAN on-disk object entry, plus fields the on-disk writer
   never fills.
 - The connect letter's trailing extras `07E2 0000 <word> 6400`, and that the `6400` is a
   constant rather than a system number - it appears here on the Ethernet transport too, node
   102 to node 100, where the HDLC captures show the same value between different node pairs.
 - `*XFTRA` parameter 11 as the operation selector: 2 = TRANSFER-FILE, 3 = APPEND-REMOTE-BATCH.
 - The word-alignment rule for XROUT parameters, including that the trailing pad after a final
   odd-length parameter is the writer's choice.

## Related

 - Newer captures: the dated folders beside this one.
 - `SINTRAN/Devices/HDLC/WireShark/hdlc_tcp.lua` - the dissector.
 - Skill `xmsg-decode` - the working decode reference.
