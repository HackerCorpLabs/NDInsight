# Capture spec: climbed reconnect (no 100 restart) - the missing pcap

## Why

Our C# responder 24B-crashes / stalls when its outgoing datagram sequence lands on the
Counter-0xFF boundary of a class-0x0400 connect/accept LETTER (F1 = seed+1; for 100<->102 that is
F1 0x0015). A "connect -> login -> 1 command -> disconnect" run is about 0x13 frames, so the store
ends near 0x0014 and the NEXT connect's port-assign lands on 0x0015. No existing capture shows a
reconnect onto a peer that did NOT restart XMSG, so we cannot see what a correct d102 does at the
boundary (burn past it? use a different F1? something else?). This capture fills that hole.

## MUST be a real d102

Capture the SAME real/retrocore both-ends setup that produced the existing corpus in
E:\Dev\Ronny\X25Emulator\pcap - NOT our C# runner (the runner is the thing that is wrong). We need
the correct responder's behaviour.

## Wireshark

- Interface: loopback ("Adapter for loopback traffic capture" on Windows / lo on Linux) - the
  HDLC/XMSG traffic runs over the TCP bridge on loopback.
- Capture filter: tcp port 10362  (or whatever port the real-both bridge uses).
- Start capturing BEFORE the first connect.

## Scenario (keep each session identical)

1. @connect-to d102
2. log in SYSTEM / SYSTEM
3. one command: 1 (Time)
4. 4 (Disconnect); wait for return to D100
5. WITHOUT restarting XMSG on 100: @connect-to d102 again
6. repeat connect / login / 1 / disconnect 3-4 more times, never restarting 100

## Save + decode

- Save As reconnect-climbed-no-restart.pcapng in E:\Dev\Ronny\X25Emulator\pcap
- Run gen_md_per_pcap.py to produce the .md
- Hand the .md (or .pcapng) back.

## What to extract (the answer we need)

For the 2nd and later connects, where 100's connect Flags1 is climbed (e.g. 0x0014):
- the responder ACCEPT Flags1 / channel / counter
- the PORT-ASSIGN Flags1 - does it land on 0x0015 (Counter 0xFF, D9), or is it skipped/stepped past?
- any EXTRA datagram (XSGSY / DUMM / other) inserted before the port-assign, with class + Flags1 -
  this is the "burn" if it exists
- whether a correct d102 EVER emits a Counter-0xFF class-0x0400 letter

If the real d102 sails through 0x0015 on class 0x0400 with no extra frame and no crash, then the
crash is something ELSE our node sends and the capture will show that too.
