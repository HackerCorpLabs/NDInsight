# GOD-LLM: the DUMM-consumption theory is DISPROVEN by the wire. What actually DISPLAYS multi-buffer TAD output?

Date: 2026-07-07
Follow-up to `XMSG-TAD-LONG-TERMINAL-OUTPUT-QUESTION-2026-07-07.md`.

Your last answer: "100 consumes/displays exactly one continuation per 7DUMM (1:1); you sent two
continuations same-instant so 100 consumed only one and dropped the other. Pace one continuation
per DUMM." We implemented exactly that (window = 1, release the next continuation only after 100's
7DUMM arrives, withhold the final until every continuation is both DUMM-consumed and acked).

The result is a byte-and-timing-correct 1:1 DUMM handshake on the wire - AND 100 STILL DISPLAYS
ONLY THE FINAL CHUNK. Frames 1 and 2 are lost exactly as before. So the DUMM theory does not hold.
We need the REAL display mechanism, not another handshake refinement.

## The command

Logged in as `ronny` on tty. User typed `3` (the echo diagnostic), which emits a ~600-byte reply
designed to render as three labelled frames:
```
===== ECHO FRAME 1 OF 3 =====
1111...(255-byte chunk)
===== ECHO FRAME 2 OF 3 =====
2222...(255-byte chunk)
===== ECHO FRAME 3 OF 3 =====
3333...
===== END OF 3-FRAME ECHO =====
```

On 100's actual screen ONLY this appeared:
```
# 3
===== ECHO FRAME 3 OF 3 =====
33333333333333333333333333333333
===== END OF 3-FRAME ECHO =====
```
Frames 1 and 2 (the two bare 255-byte continuations) never displayed.

## The exact wire trace (xmsg-runner.log 2026-07-07 07:45:18, 102 is the TAD host, 100 the asker)

Legend: TX = 102->100 (us, the host), RX = 100->102. All secure-ACKs are subtype 0x03 on the
routing channel; all three data chunks are XMCSM 0x01080000 terminal-data, frameFlags 0x96,
role byte 0x00, on channel DD (chunk3 steps to DC as Flags1 crosses the epoch boundary - expected).

```
RX  100->102  Data Tad f1=0x0009  BDAT count=2 data=33 8D          ; user typed "3"+CR
TX  102->100  secure-ACK f1=0x0009                                  ; ack the keystroke

TX  102->100  CHUNK1 f1=0x000B  ch=DD  XMLEN=0x0101  01 FF <255B>   ; bare BDAT, NO RFI, NO SYCN
   body: 89C8 2113000E 0064 0066 000B 0108 DD 03 2100 96 00 0064 0299 0066 0211 01080000 0101
         01 FF 0D0A "===== ECHO FRAME 1 OF 3 =====" 0D0A 3131...(fill to 255)
RX  100->102  RR nr=4                                               ; LAPB ack
RX  100->102  secure-ACK f1=0x000B                                  ; 100 ACKs CHUNK1 (delivery)
RX  100->102  Data Tad f1=0x000A  DUMM (0x18)                       ; 100 sends 7DUMM #1
TX  102->100  secure-ACK f1=0x000A                                  ; we ACK the DUMM

TX  102->100  CHUNK2 f1=0x000C  ch=DD  XMLEN=0x0101  01 FF <255B>   ; bare BDAT, NO RFI, NO SYCN
   body: 890C 2113000E 0064 0066 000C 0108 DD 00 2100 96 00 0064 0299 0066 0211 01080000 0101
         01 FF 0D0A "===== ECHO FRAME 2 OF 3 =====" 0D0A 3232...(fill to 255)
RX  100->102  secure-ACK f1=0x000C                                  ; 100 ACKs CHUNK2 (delivery)
RX  100->102  Data Tad f1=0x000B  DUMM (0x18)                       ; 100 sends 7DUMM #2
TX  102->100  secure-ACK f1=0x000B                                  ; we ACK the DUMM

TX  102->100  CHUNK3 (FINAL) f1=0x000D ch=DC XMLEN=0x0070 01 64 <100B> + SYCN 000A + RFI
   body: 0940 2113000E 0064 0066 000D 0108 DC FF 2100 96 00 0064 0299 0066 0211 01080000 0070
         01 64 0D0A "===== ECHO FRAME 3 OF 3 =====" 0D0A 33...(x32) 0D0A "===== END OF 3-FRAME ECHO =====" 0D0A
         13 02 000A   ; SYCN 000A (LOGGED-IN)
         01 02 2320 02 00   ; ... RFI region
RX  100->102  secure-ACK f1=0x000D                                  ; 100 ACKs CHUNK3
```

## What this proves (verified facts, not speculation)

1. There was EXACTLY one 7DUMM per bare continuation (DUMM #1 after CHUNK1, DUMM #2 after CHUNK2),
   1:1, and we secure-ACKed both. The pacing you prescribed is now literally on the wire.
2. Every chunk was delivered (LAPB RR advanced through all N(S)) AND secure-ACKed at XMSG.
3. 100 still rendered ONLY CHUNK3. So "one continuation per DUMM" is NOT the display gate. The
   7DUMM is something else (idle/keepalive?), not a per-continuation display credit.
4. The ONLY structural difference between the chunks that were DROPPED and the one DISPLAYED:
   - DROPPED (CHUNK1, CHUNK2): bare `BDAT count=0xFF (255)`, NO RFI, NO SYCN, XMLEN 0x0101.
   - DISPLAYED (CHUNK3): `BDAT count<255` + `SYCN 000A` + `RFI`, XMLEN 0x0070.
   Intra-chunk gaps were ~13-16 ms (you noted the real host has ~45-50 ms and "sends the last
   continuation alone" - we can add delay, but the bare-BDAT chunks NEVER show regardless).

## The questions we need answered (concrete, please)

1. Is the "bare 255-byte BDAT, no RFI, count=0xFF" continuation the WRONG construct for DISPLAYED
   terminal text entirely? i.e. is that 255-full-buffer-sentinel actually the XM-FIDO / file-block
   path (7BLK), and TERMINAL display never uses it?

2. For DISPLAYED text longer than one buffer, is each segment a COMPLETE terminal write - its own
   `BDAT(<=some max) + SYCN + RFI` - and the host waits for 100 to signal ready (7CERS? a 7DUMM? a
   keystroke?) before the next COMPLETE write? If so:
   - What is the exact per-segment trailer (does every segment carry SYCN 000A + RFI, or only the
     last, with intermediate segments using a different SYCN state)?
   - What is the max BDAT byte count 100 will DISPLAY in one write before it must be split? (Is it
     less than 255 - e.g. the terminal line/echo buffer size?)
   - What is the exact "ready for next segment" signal from 100, and must the host block on it?

3. If it IS a single burst of continuations (not N complete writes), what byte are we getting wrong
   on the non-final chunks so 100 displays them: frameFlags (0x92 vs 0x96?), role byte, the count
   encoding (should a "more follows" chunk use a specific marker other than count=0xFF?), or a
   per-chunk SYNC/flush opcode we are omitting?

4. Do you have ONE real captured HOST->ASKER burst of DISPLAYED text > 255 bytes (a directory
   listing, a long HELP, a >255-byte status) - not a file transfer - whose exact bytes + opcodes +
   timing we can copy frame-for-frame? That single capture would settle this immediately.

## What we will do with the answer

Replace the bare-255-continuation streamer with whatever the real DISPLAY mechanism is (N complete
writes with the correct trailer + ready signal, or the corrected continuation bytes), then restore
the rich multi-line stat / help / list-service / who / wall output that we are currently forced to
keep under one 255-byte buffer.
