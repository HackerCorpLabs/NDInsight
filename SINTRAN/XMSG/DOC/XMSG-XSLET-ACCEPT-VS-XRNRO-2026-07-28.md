# What a successful answer to an XSLET letter looks like (2026-07-28)

Every file-server letter captured this week came back as `XRNRO` - no access to remote system -
because no peer existed to answer. The obvious question was what the *correct* answer is. It turns
out we have had it on the wire since April: **`connect-to` sends the same letter shape to
`*TADADM` and gets answered**, and that exchange is in the pcap corpus.

This documents the success form, the failure form, and the difference - which is smaller and
stranger than expected.

---

## 1. The two forms, side by side [VERIFIED]

The client's request is the same shape in both cases: an `XSLET` (`0x41`) naming a server and a
system, with two string parameters.

**Success** - `connect-to` reaching `*TADADM` on node 102:

```
src 102:342 -> dst 100:683        the SERVER's own port, not XROUT
XMCSM = 0x04000041                service byte STILL 0x41 = XSLET
XMLEN = 8
  01 02 0000                      integer parameter 1 = 0
  02 02 000A                      integer parameter 2 = 10
```

Full frame:
`2113000E00640066012F0400D8E521008640006402AB00660156040000410008010200000202000A`

**Failure** - `*XFTRA` / `*FA-SERVER` with no route to 102:

```
01 0C 00 3A  ...the entire original body, unchanged...
                                  service byte OVERWRITTEN with 0x0C = 12 = XRNRO
```

## 2. The three differences that matter

**The service byte is the status field, and success leaves it alone.** On failure XROUT writes the
error code over the `0x41`. On success `0x41` survives. So a responder must NOT invent a
success code - it echoes the service it was sent.

**Success replaces the body; failure preserves it.** The accept carries two *integer* parameters
where the request carried two *strings*, and the message shrinks from 58 bytes to 8. The refusal
returns every original byte untouched, which is how a sender matches a returned letter to what it
sent. Two opposite conventions on the same message, selected by outcome.

**The answer comes from the SERVER, not from XROUT.** The accept's source is `102:342` -
`*TADADM`'s own port - while the request went to `102:0`, the XROUT sink. XROUT forwarded the
letter and then stepped out of the exchange entirely. This is the wire-level proof of the rule
stated in
[XMSG-SERVER-NAMES-AND-LETTERS.md](XMSG-SERVER-NAMES-AND-LETTERS.md): XROUT never hands the client
another task's magic number; the server discloses its own by answering.

That last point is the one that changes how a responder must be built. **The refusal we have been
capturing all week never crossed a wire** - our own local XROUT generated it and handed it straight
back through MON 200. A pcap of a healthy link would not contain one.

## 3. The accept body is a constant [VERIFIED]

`01 02 0000 02 02 000A` appears **35 times across the whole pcap corpus** - multiple capture files,
multiple sessions, multiple client nodes (100 and 103). Not once did either value differ.

Parameter 1 = 0 reads naturally as a status ("accepted"). Parameter 2 = 10 is UNKNOWN. The manual
documents `XSLET`'s *inputs* only (param 1 name, param 2 system, optional param 4 LAN-only flag)
and says nothing about what comes back, so there is nothing to check it against. It is constant in
every observation, so no capture we have can distinguish a fixed protocol constant from a value
that simply never varied under the conditions captured.

## 4. What this gives the responder

Enough to answer the file-server letters without guessing the envelope:

- reply from the responder's **own port**, not from port 0
- keep the service byte as sent
- body = two integer parameters, `0` then `10`
- the client then addresses the responder directly at the disclosed port

What it does NOT give: what `*XFTRA` and `*FA-SERVER` expect *after* the accept. `connect-to`'s
next move is a TAD chain, which is specific to terminal access. The file servers' equivalent has to
come from the Ghidra carve of `cos-file-tra-e02` and `cos-fa-serv-e04`, or from a live client
answering our responder.

---

## Provenance

`E:\Dev\Ronny\X25Emulator\pcap\conn-to-d102-from-100.pcapng` and the rest of the corpus, decoded
into `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\pcap-decode-report.txt` by `PcapDecodeTests`.

Caution for anyone re-deriving this: the per-capture markdown summaries in the pcap folder truncate
frame payloads to the first 16 bytes, and the `analyze_lapb.py` trace embedded in them is truncated
too. The accept letter is not visible in either. Use the decode report, which carries every frame
in both directions with full raw hex.
