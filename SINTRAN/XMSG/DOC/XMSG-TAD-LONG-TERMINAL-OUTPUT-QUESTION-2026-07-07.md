# GOD-LLM question: how does the SINTRAN TAD host DISPLAY terminal output longer than one buffer?

Date: 2026-07-07
Context: our C# node (102) is the TAD host (`*TADADM`); the real ND-100 (100) runs the genuine COSMOS
connect-to as the asker/terminal. Short replies work perfectly. We cannot get MULTI-FRAME terminal
output to DISPLAY on 100 - it renders only the final chunk. We need the real display mechanism.

The transport, LAPB window queue, envelope, secure-ACKs, and the 255-sentinel flow-control handshake
are all implemented and VERIFIED correct on the wire (see the trace below). This is specifically about
how 100 DISPLAYS a host output burst that spans more than one XMSG frame.

## What we do (per the earlier "255 full-buffer sentinel" answer)

For a reply longer than one buffer we stream it as:
- Non-final chunk = EXACTLY 255 data bytes: `01 FF <255B>` (XMLEN 0x0101), a bare BDAT, no RFI, its own
  datagram, frameFlags 0x96, role 0x00.
- Final chunk = `< 255` bytes: `BDAT(remainder) + SYCN 000A + BDAT("# ") + RFI`.
- Flow control: at most 2 output datagrams unacked; the next chunk is sent only after 100 ACKs the
  outstanding ones AND we secure-ACK the 7DUMM 100 sends between pairs.

## The live trace (xmsg-runner.log 2026-07-07 06:33:03) - the "HELP" command (3 chunks)

`HELP` produces a ~560-byte reply = chunk1(255) + chunk2(255) + chunk3(74)+SYCN+prompt+RFI.

```
TX ns=2  chunk1  89 44 2113000E 0064 0066 0009 0108 DD 03 2100 96 00 0064 0299 0066 0211 01080000 0101 01 FF <255B: "\r\n----- COMMANDS -----\r\n  1 / time ... session / termin">
TX ns=3  chunk2  89 46 2113000E 0064 0066 000A 0108 DD 02 2100 96 00 0064 0299 0066 0211 01080000 0101 01 FF <255B: "al info\r\n  who ... list route ">
RX  100 -> secure-ACK f1=0x0009 (chunk1)   [subtype 0x03, channel DE]
RX  100 -> secure-ACK f1=0x000A (chunk2)
RX  100 -> 7DUMM  f1=0x0008  (01080000, role 0x94)
TX  we secure-ACK the DUMM (f1=0x0008)
TX ns=5  chunk3  09 AA 2113000E 0064 0066 000B 0108 DD 01 2100 96 00 0064 0299 0066 0211 01080000 0056 01 4A <74B: "          routing table\r\n  help ... show this command list"> 13 02 000A 01 02 2320 02 00
RX  100 -> secure-ACK f1=0x000B (chunk3)
RX  100 -> 7DUMM  f1=0x0009
```

Everything checks out: all three chunks are the same channel (DD), byte-correct, delivered at LAPB
(100's RR advances through all N(S)) AND secure-ACKed at XMSG. The pacing matches the sentinel rule
(send 2, wait for ACKs, ACK the DUMM, then send the final).

## The symptom

On 100's actual screen, ONLY chunk3 renders:
```
# HELP           routing table
  help                  show this command list
```
chunk1 (the `----- COMMANDS -----` header + first commands) and chunk2 (`who ... list route`) never
appear. The word "nego|tiation"-style split confirms it: chunk2 ends "...list route", chunk3 begins
"          routing table", and only "routing table..." shows - i.e. 100 discards everything before the
final chunk. This is identical for any >255-byte terminal reply (stat when rich, list service, etc.).

## The decisive clue: 7CERS

100 emits a `7CERS` (opcode 0x21, "burst consumed") after each host burst it CONSUMES/DISPLAYS:
- After the SINGLE-frame login prompt (BDAT "\r\nOK..." + SYCN + "# " + RFI), 100 sends
  `... 01080000 0002 21 00` = 7CERS. (verified, same run)
- After the MULTI-chunk HELP burst, 100 sends a 7DUMM and its secure-ACKs, then the user's next
  command - but NO 7CERS. So 100 never signalled it consumed/displayed the HELP burst.

So 100 ACKs the datagrams (delivery) but does NOT consume/display the multi-chunk burst. The frames
arrive; the display layer drops all but the last.

## The questions

1. Is the 255-byte "buffer full, more follows" sentinel actually the TERMINAL-OUTPUT display mechanism,
   or is it the FILE-TRANSFER mechanism (XM-FIDO / 7BLK file blocks)? The earlier answer cited a
   "~2 KB file listing across 8+ chunks" - was that a FILE transfer (written to a file), not text
   DISPLAYED on the asker's screen?

2. How does the genuine SINTRAN TAD host make 100 DISPLAY output longer than one buffer (e.g. a
   directory listing, a HELP screen, a >255-byte status)? Concretely, what is the exact frame sequence
   for a, say, 600-byte text reply that 100 renders in full? Options we need disambiguated:
   - N separate COMPLETE terminal bursts, each `< 255` bytes and each ending with its own
     SYCN + RFI (i.e. the host writes, waits for 7CERS, writes the next)? If so, does each carry the
     RFI, and does 100 send a 7CERS per block?
   - One burst of 255-byte continuations but with a per-chunk marker / flush / different frameFlags
     (0x92 vs 0x96 alternation?) / role we are getting wrong?
   - A pagination / "more" mechanism (does the host wait for a keystroke or a 7POLL between screens)?

3. Must the host wait for a 7CERS (not just the datagram ACK + 7DUMM) before sending the next chunk,
   so 100 finishes displaying the current buffer before the next arrives? Our handshake keys off the
   secure-ACK + 7DUMM, never the 7CERS - is the 7CERS the real "ready for the next screen" signal?

4. Is there a real captured HOST->ASKER burst of DISPLAYED text > 255 bytes (not a file transfer) whose
   exact bytes + timing we can diff against the trace above?

## What we will do with the answer

Implement a proper output queue that chops ANY size reply into as many frames as needed and streams
them so 100 displays ALL of it (restoring the rich multi-line stat / help / list service / who / wall
output), using the correct display/flush handshake instead of the current single-frame-only workaround.
