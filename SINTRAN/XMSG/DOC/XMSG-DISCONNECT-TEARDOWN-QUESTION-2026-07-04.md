# Question for the GOD LLM - our 0xFD does not tear the session down (100 answers DUMM, not DCON)

## Context

The connect-to login now works end to end (MOTD -> SYSTEM/SYSTEM -> menu -> Time/Date/Echo), on a
FRESH 100 (Reachability at Flags1 0x0000). The remaining break is menu choice 4 (Disconnect): we
print "--- DISCONNECTING ---" and send the 0xFD session-notification, 100 ACKs both, but then 100
sends a DUMM keepalive and the session stays up - it never sends DCON and never closes. On the
terminal the user sees "--- DISCONNECTING ---" and then nothing (still connected).

All bytes below are from the runner log (102 = us, 100 = the connect-to host).

## What we send for choice 4

Frame 1 (terminal data, XMCSM 0x01080000): BDAT "\r\n--- DISCONNECTING ---\r\n"
```
tx 102->100  a=0x89  2113000E 0064 0066 0013 0108 DC F9  210096 00 ... 01080000 001B  01 19 0D0A 2D2D2D20444953434F4E4E454354494E47202D2D2D 0D0A
             (Flags1=0x0013 counter=0xF9 proto=DC frameFlags=0x96 role=0x00 ; BDAT only)
```
Frame 2 (the 0xFD notification, class 0x0006, XMCSM 0x00060000, from TADADM port 342):
```
tx 102->100  a=0x09  2113000E 0064 0066 0014 0006 DD FA  210082 54  0066 0156 00060000 0002  FD 00
             (Flags1=0x0014 counter=0xFA proto=DD frameFlags=0x82 role=0x54 ; TAD 0xFD, empty)
```
100's response:
```
rx 100->102  Ack Flags1=0x0013     (acks the DISCONNECTING text)
rx 100->102  Ack Flags1=0x0014     (acks the 0xFD)
rx 100->102  Data proto=Dc Flags1=0x0012 XMCSM=0x01080000 role=0x94 -> TAD DUMM (0x18)   <-- a keepalive, NOT a DCON
```
Then the session just continues (DUMM/CERS traffic). No DCON, no teardown.

## Our hypothesis (please confirm or correct)

The documented teardown ladder (TAD-Message-Formats.md ~line 1194-1196 / 1363) is longer than what
we send. For a normal host-initiated logout it is:
```
H->>C: 7BDAT(final text) + 7CESC 00
H->>C: 7BMMX 000000 + 7ECKM 00 + 7CESC 00
H->>C: 7BDAT("\r\n--EXIT--\r\n") + 7SYCN 000B     <-- SYCN 000B = LoggedOut
H->>C: 7CESC 01
H->>C: 7FD  (0x0006 class)
```
We send ONLY `BDAT("--- DISCONNECTING ---")` then `0xFD` - we skip the `SYCN 000B` (LoggedOut)
signal, the `CESC 00`/`CESC 01` transitions, and the `BMMX 000000 + ECKM 00` reset. Our INFERENCE
is that 100 will not initiate DCON until it receives the `SYCN 000B` logout (mirroring how it needed
`SYCN 000A` to consider us logged IN), so our bare `0xFD` is ignored as a teardown trigger and 100
just keepalives.

## Questions

1. What is the EXACT minimal frame sequence 100 needs from the host to make it send DCON and close
   the connect-to session? Is `SYCN 000B` (LoggedOut) the required trigger, or is it the `0xFD`, or
   the combination, and in what order?
2. Do the `CESC 00` -> `CESC 01` transitions and the `BMMX 000000 + ECKM 00` reset matter for the
   teardown, or are they cosmetic (echo/break-mode restore) and safe to omit?
3. Our `0xFD` frame: Flags1 0x0014 lands at epoch 1 for class 0x0006 (baseLow = seed 0x14 - 0x06 =
   0x0E; 0x14 -> epoch 1), so it rode channel `DD` with counter 0xFA, frameFlags 0x82, role 0x54.
   Is that the correct channel/counter/role for the `0xFD`, or does the teardown notification need a
   specific channel (e.g. DE at epoch 0)?
4. After the host's teardown, does 100 send a single DCON we must ACK, and should the responder then
   stop (close its session state), or keep the LAPB link up for the next connect?

## What we need back

The exact host-side teardown ladder (frames, opcodes, SYCN value, channels, order) that makes 100
send DCON and close the session, and confirmation of whether our bare `BDAT + 0xFD` is simply
missing the `SYCN 000B` logout step. Mark inferred vs capture-verified, cite capture frames.
