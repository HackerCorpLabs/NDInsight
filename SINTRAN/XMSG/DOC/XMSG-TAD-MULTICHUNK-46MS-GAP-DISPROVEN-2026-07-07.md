# GOD-LLM: the 46 ms intra-pair gap is ALSO disproven. Only the final chunk renders. Receiver decode next?

Date: 2026-07-07 (session 08:42)
Follow-up to `XMSG-TAD-MULTICHUNK-DISPROVES-DUMM-2026-07-07.md` and TAD-Message-Formats.md 22.16.

## What we implemented (your recommended lever #1)

The verified 22.16 output-queue algorithm, faithfully:
- 255-byte continuations streamed in PAIRS.
- The SECOND chunk of a pair held until >= 46 ms after the first (a new same-thread pump on the
  LAPB adapter loop re-drives the send once the gap elapses - the second chunk has no inbound
  trigger otherwise).
- Between pairs: wait for both chunks acked AND their 7DUMMs seen.
- Last continuation sent ALONE; final (SYCN 000A + prompt + RFI) only after that last chunk's ACK.

Build clean, 142 offline tests green.

## The live result: NO CHANGE. Only frame 3 renders.

The `3` echo diagnostic (2 continuations + final) again showed ONLY:
```
# 3
===== ECHO FRAME 3 OF 3 =====
33333333333333333333333333333333
===== END OF 3-FRAME ECHO =====
#
```
Frames 1 and 2 (the two 255-byte continuations) did not display.

## The actual send timing THIS run (from the runner log)

```
08:42:44.691  TX chunk1  F1=000B  a=0x89  ch DD  01 FF <255 "..ECHO FRAME 1 OF 3..1111..">
08:42:44.737  RX  100 -> 7DUMM
08:42:44.740  TX chunk2  F1=000C  a=0x89  ch DD  01 FF <255 "..ECHO FRAME 2 OF 3..2222..">   (+49 ms after chunk1)
08:42:44.829  RX  100 -> 7DUMM
08:42:44.833  TX final   F1=000D  a=0x09  ch DC  01 64 <100 "..ECHO FRAME 3 OF 3..END..">
              + 13 02 000A (SYCN) + 01 02 2320 ("# ") + 02 00 (RFI)
```
The ~46-49 ms intra-pair gap was achieved. It made no difference.

Note one deviation from your 22.16 reference we could not avoid with the event model: in the real
host the PAIR is transmitted 47 ms apart BEFORE either chunk is acked and before any DUMM. In ours,
chunk2 happened to fire ~3 ms after 100's first DUMM arrived (the DUMM landed right at the 46 ms
mark and re-drove the drain). So chunk2 went out at the right SPACING but slightly coupled to the
DUMM arrival. If you believe the pair MUST be sent before any ACK/DUMM round-trip, say so and we
will decouple it entirely from inbound events. But we doubt this is the cause.

## HONEST CAVEAT (you were right to insist on this)

All of the above is read from the RUNNER LOG, not a pcap. You warned the log can look plausible
while the wire differs. We have NOT yet captured the failing run in pcap. So we CANNOT truthfully
claim our on-wire frames are byte-identical to 22.16 - only that the log-recorded structure matches
(addr 0x89 odd continuations / 0x09 even final, marker 2100, frameFlags 96, role 00, XMLEN 0101 on
continuations, final = BDAT<255 + SYCN 000A + prompt BDAT + RFI + word-align pad). A pcap diff is
still outstanding; if you tell us exactly how to capture it in this nd100x --hdlc bridge setup we
will do that before anything else.

## NEW observation: XEIMA after the burst

After the echo burst, 100 repeatedly sends (every ~20 s, looks like a retry):
```
[RX] 100->102 sub=NetworkError proto=Routing f1=0x0001 info=2113 0007 0066 0064 0001 FFED DE2C
```
`0xFFED = -19 = XEIMA` (invalid magic number). This may be the already-known list-systems
magic-number stub issue (unrelated), or it may be 100 reacting to our burst. Flagging it in case it
is diagnostic - is XEIMA here about the terminal burst, or the separate list-systems probe?

## Where we are

Two independent levers you predicted - the 1:1 DUMM pacing AND the 46 ms intra-pair gap - are now
both implemented and both leave the symptom unchanged: 100 renders only the final chunk. The frames
are (per the log) structurally identical to your verified 22.16 bursts. This strongly supports your
own conclusion: the display decision is in the ASKER'S receiver, not reproducible by shaping our send.

We found the ND-100 Ghidra project on disk here: `E:\Dev\Ronny\ghidra-nd100` (Ghidra 12.0.4 ND-100
build, dated 2026-07-07). The connect-to binary is
`Installation/Communication/COSMOS Basic/x/cos-conn-to-e02.prog`.

## Questions

1. Is decoding `tad_receive_and_dispatch` (the BDAT receive path in cos-conn-to-e02.prog) now THE
   definitive next step, given both wire-shaping levers failed? You have this Ghidra-annotated. Can
   you give us the decoded logic for:
   a. The BDAT (0x01) handler: what it does for count == 0xFF vs count < 0xFF; where the terminal
      write / render call is; and whether that render is gated on a flag / the RFI state / an
      accumulated length threshold.
   b. The XFREA/XFRCV receive call: the count it passes and the true receive-buffer size
      (session[+0x299], 0x100 = 256? but a 01 FF <255> chain is 257 bytes - is the buffer bigger,
      is XFREA looped, or is 0x100 a mis-read?).
   c. The MsgType filter (MsgType != 3 -> discard): confirm the constant and whether 3 == XMTHI
      (high-priority). Both real bursts carry role 0x00 (not XFHIP) yet render - reconcile.
2. If instead you want us to decode it locally in the ND-100 Ghidra project, tell us the entry
   address / symbol to open and we will pull the disassembly of that handler and paste it back.
3. Is the XEIMA above relevant, or a red herring from the list-systems probe?

## What we will do with the answer

Whatever the receiver's real display gate is (a length threshold, an RFI-per-segment requirement, a
priority flag, a per-write SYCN, a different count encoding for "buffer full"), implement THAT, then
restore the rich multi-line stat / help / list-service / who / wall output.
