# DEF-NETWORK-CONN: oracle card SUCCEEDS, HLE card FAILS - the exact missing exchange

**Date:** 2026-08-10
**Method:** compared two live device-log captures of the SAME command ladder, one on the
real-68K oracle card, one on the native-C# HLE card. No new decode of firmware needed - the
XROUT record/tag format is already pinned in `XROUT-DIRECTORY-RECORD-TAGS-DECODE-2026-08-10.md`.

## Captures compared

| | Card | Ladder | DEFINE-NETWORK-CONNECTION result |
|---|---|---|---|
| ORACLE | real 68K firmware | D2XX=17848 + A10x, `,,,N` | **Ok** - routes built (`17848 L: *->WAN?->17848`) |
| HLE | native C# `NDBusEthernetIIHle` | real names D100..D19999, `,,,N` | **"Unknown name (of server or system)"** |

- Oracle console: `C:\Users\ronny\AppData\Local\Temp\retrocore-ethii\oracle-dram-console.txt` L74-102
- Oracle device log: same dir `oracle-startnet-device.txt` (2026-08-09), the exchange at 12:00:40
- HLE console: `...\retrocore-hle-dram\run-130792\hle-dram-console.txt`
- HLE device log: same dir `hle-startnet-device.txt`, the exchange ~18:42:50

Both cards start ENNS0 fine (sysid 9800). `,,,N` vs `,,,Y` makes NO difference to the failure.
`DEF-NETWORK-CONN D100` is correctly refused on both ("local defined system"). The real card
accepts the connection define with a totally FAKE remote and NO peer on the wire, so this is
NOT a two-node / liveness limitation - it is a missing card-side response in the HLE.

## The winning card exchange (oracle, 12:00:40) - what our HLE must reproduce

1. **ND-100 -> 68K mailbox kick, SUBFUNCTION = 0x0005** (GPIP I6 strobe, vector 0x4E). This is
   the "do directory work" doorbell. (oracle log L19262-19263)
2. Card: `XFRCV port 4`, then `XFREA @0x1D30` = the incoming directory query
   `[0x0400][0x0024][0x1102][0x2648][0x2702][0x0000][0x0302][0x0000][0x0402][0x0000]...`
   - `0x0400` = serial 4, status OK; `0x1102`(param 17) = 0x2648 = 9800 = the card's own sysid.
3. Card: `XFMST` handle A=0xFFFF -> sender MAGNO (the kernel, sys 0x0064).
4. Card: `XFWRI NBYTES=12 @0x1E30 = [0x054A][0x0008][0x0102][0x45B8][0x0202][0x2648]`
   - `0x054A` = serial 5, service **0x4A = XSDSY (Define System Routing)**.
   - param1 `0x0102` = 0x45B8 = 17848 = the REMOTE system.
   - param2 `0x0202` = 0x2648 = 9800 = VIA the card's own sysid.
   - i.e. "define system 17848 reachable via me (9800)". One XSDSY per accumulated remote.
5. Card: `XFSND` Receiving port 0x00640000 (system 100, port 0 = XROUT), Sending port 4.
6. Kernel XROUT ingests the 0x054A (buffer 0x00D6), XFMST for magic (A=0x0064 D=0x0271), then
   `XFWRI [0x0500][0x0000]` (XSDSY reply OK) broadcast. (L19295-19315)
7. X-C reads `[0x0500]` at 0x1D30 -> **Ok**, route defined. (L19322)

## The HLE gap (proven by counting)

In the HLE device log for the same stage:
`[0x054A]`=0, `[0x0500]`=0, `[0x0400]`(reply)=0, `[0x1102]`=0 occurrences. The card never
runs the exchange. Its XSGSY replies keep returning the LOCAL system (100/9800) or all-zeros;
it never emits the XSDSY that defines "remote via me", so XROUT has no route -> "Unknown name".

Our compose code is CORRECT: `NDBusEthernetIIHle.BuildStartNetDirectoryReply(0x054A, remote,
OwnSysid)` builds exactly `[0x054A][len][0x0102:remote][0x0202:own]`, and `DriveConnAcceptServer`
has the SnRead/SnMst/SnSend states to do XFREA/XFMST/XFWRI/XFSND. What is missing is the TRIGGER
and/or the DATA:
- (a) the card is not woken by / does not act on the **SUBFUNCTION=0x0005 directory kick**, and/or
- (b) it has **no accumulated remotes** (`SnRemoteCount==0`), so SnMst takes the
  "no remotes known yet - draining" branch and re-parks without replying.

## Runtime diff (added 2026-08-10) - the card is starved, not just mis-coding a reply

Comparing the two device logs over the whole run:

| Metric | Oracle (works) | HLE (fails) |
|---|---|---|
| level-12 IDENT interrupts | 38 | **3281** (storm) |
| XFRRE receives | 6 | **1524** (every one EMPTY, marker X=0xE97C) |
| XFSND sends | 88 | 52 |
| XFWRI writes | 86 | 48 |
| real message received on the card's port 4 | yes (0x0400 query) | **never** |
| one-time port setup XFOPN/XFGET/XFP2M/XFPRV | present | present (counts match) |

So the initial port OPEN is fine on both. The gap is at RUNTIME: the oracle card is handed the
0x0441/0x0400 directory query on port 4 and does the reply work; the HLE card spins in an
empty-receive interrupt storm and is never handed the query. The request is instead answered
OFF-CARD with a malformed 0x0102 record.

Two coupled defects:
1. **Delivery / interrupt storm.** XROUT's directory query never lands in the HLE card's port-4
   receive; the card re-fires level 12 ~86x more than the oracle while receiving nothing.
2. **Classification.** Even delivered, `DriveConnAcceptServer` (~line 1298) only matches record
   headers 0x0100 / 0x0400 / 0x0154. The actual REQUEST header is **0x0441** (serial 4, service
   0x41); 0x0400 is the REPLY header. So the request would be misclassified and drained.

## Fix direction

Make the HLE card, on the directory kick, reply with one XSDSY (`[0x054A]{0x0102:remote}
{0x0202:own}`) per remote it should route - the remotes being the systems named by DEF-REMOTE /
carried in the earlier 0x0100/0x0400 records. Then XROUT issues 0x0500 OK and
DEFINE-NETWORK-CONNECTION succeeds, exactly as on the oracle. Verify by re-running
`Nd100EthernetIIHleDramDumpTests.Boot_Login_EnnS0_DumpHleDram` and checking the console for
`Ok` + a populated `List-Routing-Info`, and the device log for `[0x054A]`/`[0x0500]`.
