# What a successful answer to an XSLET letter looks like (2026-07-28)

> **CORRECTION 2026-08-04 - the accept form is NOT generic. Read this first.**
>
> This note concluded that the accept shape below is generic rather than per-server, and
> `FaServer` acted on that by answering an `*FA-SERVER` connect letter with the same
> TAD-shaped accept (`0041 0008 01020000 0202000A`). **A live capture disproves that for
> file access.**
>
> In `DOC/captures/FA-READ-WRITE-2026-08-04/capture-read.txt` D100 sends the `*FA-SERVER`
> letter and D102 answers with a secure ACK (subtype `0x03`) and then a DATA frame:
>
> ```
> 2113 000E 0064 0066 01F9 0008 DC13   SINTRAN header (dest 100, src 102, Flags2 0x0008)
> 2100 82 84 0064 0812 0066 06B6 0008  XMSG sub-header, XMCSM 0x0008
> 07D2 0002 0042 6400                  the BODY, at absolute offset 28
> ```
>
> `0x07D2` is `FaMessageType.ConnectionConfirm` and `0x0002` is
> `FaExchangeCodec.ResponderConversation` - both VERIFIED. XMCSM `0x0008` is the body's byte
> length. The trailing `0042 6400` is INFERRED from one sample: the connect letter carries no
> conversation number for `0x0042` to be derived from, and `0x64` = 100 is the CLIENT's system
> here (the answering node is 102), which contradicts the earlier reading of that byte as the
> answering system's number. `FaServer` now answers a file-access connect letter with an FA
> ConnectionConfirm.
>
> What still stands from this note: the service byte IS the status field, `XRNRO` is XROUT
> overwriting it with `0x0C`, and the TAD accept shape below is correct for `*TADADM`.
>
> One more correction: the frame layout in section 1 predates the 2026-08-04 header/sub-header
> split. The SINTRAN header is 14 bytes and the XMSG sub-header 14, so the body starts at
> absolute 28; `XMCSM = 0x04000041` is really XMCSM `0x0400` (one word, at 26-27) plus the
> body's first word `0x0041`, which is the XROUT serial byte 0 and service byte `0x41`. What
> this note calls `XMLEN = 8` is the XROUT declared length at wire 30-31, not a sub-header
> field.

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

> ## The fresh-accept rule is a HARD REQUIREMENT, not a convention [2026-07-31]
>
> Independent corroboration from the RetroCore side, working the ND Ethernet II HLE card
> (`XMSG-RETROCORE-ENNS0-MBOXH-CONTRIBUTION-2026-07-31.md`):
>
> **Answering with `XFRTN` (returning the received message) CRASHES the ND-100 XMSG kernel** -
> `XMSG error code 23B`, `XMFIDO ABORTS`, internal inconsistency. The cause is that a receive can
> hand back an XMSG-**internal pool block**, and returning it corrupts the pool.
>
> So a server must `XFGET` / `XFWRI` / `XFSND` (or `XFSMC`) a **fresh** letter addressed to the
> requester's magic. Never `XFRTN` the one it received. What this document described as the
> observed convention now has a concrete failure mode behind it: getting it wrong does not
> produce a malformed reply, it takes the peer's XMSG down.
>
> The same source confirms the accept body `01 02 0000  02 02 000A` on a **different server** -
> `*XM-ENNS0`, the COSMOS Ethernet network server, during `DEFINE-NETWORK-CONNECTION`. So the
> accept shape is the **generic XSLET connection accept**, not something specific to `*TADADM`
> or the file servers, which is a wider claim than this document could make from its own captures.

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
