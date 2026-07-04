# Question for the GOD LLM - why does 100 REJECT our username-accepted LAPB frame?

## Setup

Our C# TAD responder is node 102 (d102). Node 100 (retrocore) does `@connect-to d102`.
Connect, session-setup, terminal-setup, and the MOTD/banner all work: 100 renders the banner
and the `ENTER ` prompt. The user types a username (`ds` + CR). Our responder answers with the
section-21/22.15 login ladder. That answer is where it breaks.

Everything below is from our runner log (both directions logged) plus the decoded capture
`E:\Dev\Ronny\X25Emulator\pcap\conn-to-d102-from-100.md`. No guesses - byte evidence only.

## The failing exchange (our runner log)

After 100 sends the username as a BDAT (100->102, LAPB N(S)=5):
```
rx  a=0x89 I ns=5 nr=5   ...0108DD07 2100 96 84 ... 0103 E4F38D        (BDAT "ds"+CR, parity)
```
we transmit three LAPB I-frames back to back (102->100):
```
TX  a=0x09 I ns=5 nr=6   09CA 21130003 0064 0066 0005 0001 DE 19                                  (routing ACK of 100's datagram 0x0005)
TX  a=0x09 I ns=6 nr=6   09CC 2113000E 0064 0066 0007 0108 DD 05 2100 96 00 00 6402AA 0066 0211 01080000 000B 01020D0A130200030E0100    (USERNAME-ACCEPTED: BDAT CRLF + SYCN 0003 + CESC 00)
TX  a=0x09 I ns=7 nr=6   09CE 2113000E 0064 0066 0008 0108 DD 04 2100 96 00 00 6402AA 0066 0211 01080000 0012 010A 50415353574F52443A20 000301FF 0200   (PASSWORD prompt: BDAT "PASSWORD: " + ECKM FF + RFI)
```
100 responds:
```
rx  a=0x09 RR  nr=6   09C1 0064 D970
rx  a=0x09 REJ nr=6   09C9 0064 1BB6
```
We go-back-N retransmit ns=6 and ns=7 (identical bytes). 100 then goes SILENT - it never sends
a datagram ACK for our data-sequence Flags1 0x0007 or 0x0008 (it DID ack 0x0000..0x0006), never
sends another RR/REJ, and after ~1 minute its TAD layer times out and sends DCON. The user sees
a beep and no `PASSWORD:`.

Interpretation: 100's V(R) stays 6 the whole time. `REJ nr=6` = 100 received an out-of-sequence
I-frame (it got ns=7 while expecting ns=6) and wants ns=6 resent. So 100 never accepted ns=6.

## What we VERIFIED (so you can rule these out)

1. Our ns=6 frame is VALID HDLC. We ran its exact logged body through our own
   encoder+deframer (Fcs16, reflected CRC poly 0x8408, init 0xFFFF, ~crc, low byte first): it
   round-trips with a good FCS and exact byte recovery. So the frame we build is not malformed.
2. The transmit path writes each encoded frame verbatim and in order (independent byte arrays,
   no buffer reuse, no concatenation). So the wire bytes ARE our valid ns=6.
3. The transport is a TCP bridge (reliable) to `nd100x --hdlc`. Bytes are not "lost".
4. ns=6's XMSG content is byte-identical to the capture's ACCEPTED username-accepted frame
   (see below): same payload `01020d0a130200030e0100`, XMLEN 000B, frameFlags 96, role 00,
   XMCSM 01080000, flags2 0108.
5. Our MOTD/banner frame earlier in THIS session used LAPB address 0x09 and 100 ACCEPTED it
   (acked Flags1 0x0006, rendered ENTER). So 100 accepts our 0x09 data I-frames.

## The one wire-level difference we can see

Capture's ACCEPTED username-accepted frame (d102->100), `conn-to-d102-from-100.md:164`:
```
89cc 2113000e 0064 0066 0136 0108 db d6 2100 96 00 00 6402ab 0066 04c2 01080000 000b 01020d0a130200030e0100 f56b
```
Ours (REJECTED):
```
09cc 2113000e 0064 0066 0007 0108 dd 05 2100 96 00 00 6402aa 0066 0211 01080000 000b 01020d0a130200030e0100 (fcs)
```
Field-by-field the ONLY differences are:
 - LAPB address byte: capture `0x89`, ours `0x09`.
 - Flags1 / counter / channel-epoch / ports: session-specific (capture is a long epoch-2 session
   on channel 0xDB counter 0xD6; ours is a fresh epoch-0 session on 0xDD counter 0x05). These are
   expected to differ and our envelope is internally consistent (Counter = seed - (Flags2 and 0xFF)
   - Flags1; the earlier accepted frames 0x0000..0x0006 follow the same formula).

The XMSG payload is identical.

## The address puzzle (please resolve)

In the capture, d102->100 LAPB I-frames split by XMSG subtype:
 - routing ACK I-frames (subtype 0x03): address `0x09`   (e.g. line 145)
 - XMSG-DATA I-frames (subtype 0x0E, terminal data): address `0x89`  (lines 164, 173, 223)
Our LapbLayer hardcodes ONE address `0x09` for every I/RR/RNR/REJ. So we send data I-frames as
`0x09`. BUT our banner (subtype 0x0E, `0x09`) was accepted this session - so "data must be 0x89"
cannot be the whole rule.

Questions:
1. What determines the LAPB address `0x09` vs `0x89` for a d102->100 frame? Is the `0x80` bit the
   LAPB command/response (C/R) bit, a poll marker, or something the ND stack sets from the payload?
   Give the exact rule from the captures.
2. Does the username-accepted frame specifically REQUIRE `0x89` while the banner tolerates `0x09`?
   If 100 silently drops a data I-frame with the wrong address (looking like a lost frame -> REJ),
   that would match our symptom exactly. Is that what the ND LAPB does?
3. If address is NOT it: what ELSE in the capture's accepted username-accepted frame differs from
   ours in a way 100 enforces here but not for the banner? We have ruled out FCS, stuffing, length
   (11 odd is fine - capture is 11 too), content, ports, and the envelope counter/channel.
4. Is there a frame or step the real d102 sends BETWEEN the username BDAT and the username-accepted
   that we omit (e.g. an echo of the typed line, an extra ACK), such that our username-accepted
   arrives when 100's LAPB/TAD is not ready for it?

## What we need back

The precise reason 100 does not accept our ns=6 username-accepted frame, and the exact fix
(e.g. "data I-frames must carry LAPB address 0x89; set it for subtype 0x0E"). Cite the capture
frames. Mark anything inferred as INFERRED.
