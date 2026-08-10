# Subtype 0x17 — both machines REJECT our reachability announce

**Date:** 2026-08-07
**Captured:** `DOC/captures/XROUT-SERVICES-2026-08-07/d103-asks-us.pcapng` and `d103-asks-us.log`
**How:** relay node 19999 between D100 and D103, announcing on both links as each reached Active.

---

## The bytes

Both machines answer our announce with the same 14-byte frame — header only, no body:

```
from D100:  21 FE 00 17 4E 1F 00 64 FF FF FF FD 8F 69
from D103:  21 FE 00 17 4E 1F 00 67 FF FF FF FD 8F 66
```

Field by field, against the 7-word header:

| Word | Bytes | Meaning |
|---|---|---|
| 0 | `21 FE` | Marker 1 = `0x21` as always; **Marker 2 = `0xFE`**, which is neither `0x13` normal nor `0x12` relayed |
| 1 | `00 17` | packet type 0, **subtype `0x17` = 23** |
| 2 | `4E 1F` | destination 19999 — us |
| 3 | `00 64` / `00 67` | source 100 / 103 |
| 4 | `FF FF` | Flags 1 — the broadcast/out-of-sequence marker, same as a reachability request |
| 5 | `FF FD` | Flags 2 = **`0xFFFD` = −3** |
| 6 | `8F 69` / `8F 66` | header checksum |

## What it means

**−3 is `XENIR`**, which our enum documents from the official ND include as *"Network layer
initialization reject (purely internal)"*.

So this is exactly what it says: the peer is REJECTING our network-layer initialisation. Both
machines do it, independently, in reply to the reachability announce this node sends when a link
comes up.

That explains the symptom that led here. `LIST-ROUTING-INFO` on D103 reports:

```
19999  L: *->19999
       T: *, but no access to system 19999
       A: *, but no route to system 19999
```

The local table has the route we asked for, but there is no access — because initialisation was
refused. Access is not missing through misconfiguration; it is being actively declined.

## What is NEW here

- **Subtype `0x17` is not in `SintranPacketSubtype`.** Known members are `0x03` Ack, `0x07`
  NetworkError, `0x0A`/`0x0C` fragments, `0x0E` Data, `0x13` ReachabilityReply, `0x19`
  ReachabilityRequest. 23 has never been seen in this project.
- **Marker 2 = `0xFE`** on this frame type. Every frame in the capture corpus carries `0x13` or
  `0x12`. What `0xFE` signifies is UNKNOWN — do not assume it is a third "kind of hop" without
  evidence.
- **An error code travels in Flags 2.** `SintranPacketSubtype.NetworkError` (`0x07`) already carries
  a negative `XE*` code in Flags 2, so the shape is consistent with that; `0x17` is a second,
  distinct subtype doing the same thing for initialisation specifically.

## What we do about it today: NOTHING, and that is the bug

Our relay logs `*** NO REPLY BUILT ***` for these frames. That label is misleading here — a reject
is a RESPONSE, not a request, so there is nothing to answer. The real fault is that we do not
RECOGNISE it: the announce is refused, we carry on as though it succeeded, and the operator sees
"no access" with nothing in our logs explaining why.

**Why the reject happens is NOT established.** Candidates, none verified:
 - our announce is malformed in a way these machines refuse,
 - initialisation requires something we do not send first,
 - or the peer refuses because it already holds state for 19999 from an earlier run.

The third is worth trying first, since it is cheap: restart XMSG on a peer and announce into a
clean state. Guessing at the other two would mean inventing protocol, which is what this project
has been burned by.

---

# Root-cause work, 2026-08-07

## Step 1 - our announce is byte-for-byte the right shape. RULED OUT.

Pulled our own frame out of the capture and set it beside the real exchange documented in
`XMSG-PROTOCOL.md`:

```
real   2113 0019 dst=0066 src=0064 f1=FFFF f2=0001 ck=DE08   -> answered with 0x13 reply
ours   2113 0019 dst=0064 src=4E1F f1=FFFF f2=0001 ck=904F   -> answered with 0x17 reject
```

Marker, subtype, Flags 1 and Flags 2 all identical. The only difference is the node number, and the
checksum difference follows from it (the `0x4E00` high-byte shift that the word-6 correction
already documents). **Our frame is not malformed.**

## Step 2 - the node number is NOT the cause. RULED OUT.

19999 is outside even the 9800-9999 virtual range that XMSG-COMMAND refuses outright, so it was the
strongest candidate. Re-ran the whole relay announcing as **101**, an ordinary small number D103
already knows. Both machines rejected it identically:

```
103->101   21FE 0017 0065 0067 FFFF FFFD DD20
100->101   21FE 0017 0065 0064 FFFF FFFD DD23
```

A clean negative result: identity is irrelevant. It also confirms the checksum high byte was purely
a node-number artifact - as 101 we produce `DD2x`, adjacent to real traffic's `DE0x`.

**What this leaves:** the frame is right and the identity is irrelevant, so the cause is STATE or
SEQUENCE - the conditions under which a peer will accept an initialisation at all.

## Step 3 - the carve. Groundwork done, condition not yet named.

 - `XENIR=177775` octal confirmed in `XMSG-SYMBOL-L03.SYMB` (= `0xFFFD` = -3).
 - Kernel flattened to `DOC/COSMOS-RE/carve/XMSG-KERNEL-L03_flat.bin` with
   `bpun2raw.py XMSG-KERNEL-L03.BPUN <out> --at 131055`. 23552 words, span `120000..175777` octal,
   which matches what the loader places (NOT the 23833 nd100-dis reports on the raw BPUN).
 - **Base `0120000` is verified**: disassembling at file word 9347 prints address `142203`, exactly
   where the symbol table puts `XHINI`.
 - Scanning for the literal `0x21FE` or `0x0017` finds NOTHING. The kernel assembles that header
   from parts rather than a template, which is why constant scans are useless here - navigate by
   SYMBOL.
 - The symbol file has **412 symbols inside the code range**, including **30 `XH*` receive
   handlers**. The relevant cluster:

```
XHLNK 140566   XHRES 142136   XHSUP 142142   XHSCL 142160   XHCLR 142162
XHINI 142203   XHCHK 142241   XHRRS 142337   XHTRS 142401
```

`XHINI` is an initialisation routine - it clears a block of fields - and it carries a GUARD:

```
142213  LDA -64
142214  JAZ 3           -> 142217
142215  LDA -74
142216  SKP IF DA EQL 0
142217  JMP 2           -> 142221    continue initialising
142220  JMP I -57       -> 142141    bail out
```

Two conditions are tested and failure jumps to `142141`, just before `XHSUP`. **That is the
candidate reject path.** What the two tested words at `-64` and `-74` (B-relative) actually hold is
NOT established - naming them is the next step and needs the surrounding routine read properly,
not guessed.

## Step 3b - the carve does NOT corroborate "this is XENIR". Downgrade the claim.

Searched BOTH halves of the system for the values this frame would need if a program built it:

| Looking for | XMSG kernel | XROUT |
|---|---|---|
| `SAA -3` (set A to -3, the XENIR value) | 0 | 0 |
| literal word `177775` (-3) | 0 | 0 |
| `0x21FE` (the frame's marker pair) | 0 | 0 |
| `0x0017` (the subtype word) | 0 | 0 |

`XMSG-XROUT-L03.BPUN` flattens to 39943 words spanning `0..116006` octal. Neither it nor the
23552-word kernel contains any of the four.

**So the identification "Flags 2 = 0xFFFD = -3 = XENIR" is INFERENCE that the carve fails to
support.** It rested on name-matching: a negative value in Flags 2, and `XENIR = -3` in the include
with a description ("network layer initialization reject") that fits the symptom. That is suggestive,
not evidence, and this is exactly the trap the project already recorded as
"value scans are noise" - a number matching a constant is not proof the constant is what produced it.

Three readings remain open, and NOTHING here distinguishes them:

1. It IS XENIR, but assembled arithmetically rather than from a literal - plausible, since the
   markers and subtype are not literals in either binary either.
2. Flags 2 on subtype `0x17` is not an error code at all. On a reachability frame Flags 2 is
   `0x0001`; `0xFFFD` might be a count, a hop value or a sequence delta rather than a status.
3. The frame is not XMSG's at all. Marker 2 `0xFE` appears nowhere in the corpus, and the SINTRAN
   HDLC driver (`MP-P2-HDLC-DRIV.NPL`) is a third party on that link that neither binary covers.

**Do not build code on the XENIR reading.** Adding `0x17` to the subtype enum is still right - those
bytes were captured. Labelling it "initialization reject" in the code is not yet earned.

## Step 4 - THE MACHINE NAMES IT. Subtype 0x17 is INNAK.

`LIST-FRAMES` on D100, run while the relay was live, prints a **`Nettype`** column giving SINTRAN's
own name for every frame in its tables:

```
 Addr.  Link   HAC Status   To  From  Refno Nettype Cnt1   Buff1 Cnt2   Buff2
163214      0 410I     0 19999   100  65535  INNAK     0 1201061    0 1247221
164600 152164 400I     0   100 19999  65535   INIT     0 1231654    0 1231654
164512 152164 400I     0   100   101  65535   INIT     4 1230472    0 1230472
163247      0 476I     0   103   100     21    ACK     0 1201040    0 1246001
163335      0 477I     0   103   100     23  * <->    14 1201061    8 1246001
```

Read by direction:

 - `To 100, From 19999` and `To 100, From 101` are OUR announces, from this run and the earlier
   node-101 run. SINTRAN calls them **`INIT`**.
 - `To 19999, From 100` is the `0x17` frame coming back. SINTRAN calls it **`INNAK`**.

So our reachability announce IS an initialisation in SINTRAN's vocabulary, and subtype `0x17` is an
initialisation NEGATIVE ACKNOWLEDGEMENT. `Refno 65535` is `0xFFFF`, matching Flags 1 on both.

**This restores the substance of the reading that step 3b downgraded** - but on far better evidence.
Step 3b was right to reject name-matching against a constant absent from both binaries; the machine
labelling its own traffic is a different and much stronger kind of witness. Both steps stand: the
inference was not earned when it was made, and it is earned now.

Also captured, and previously unknown: the `Nettype` vocabulary itself. Seen so far - `INIT`,
`INNAK`, `ACK`, and `* <->` (an established two-way conversation). That is SINTRAN's own
classification of link traffic and nothing in this project modelled it.

**Still open: WHY the NAK.** Knowing it is a refused initialisation does not say what the peer
objects to. But the question is now sharply framed - "what makes a SINTRAN INIT acceptable?" - and
`LIST-FRAMES` gives a way to watch every attempt.

## Step 5 - real ACCEPTED INITs found in the corpus, and what they have that ours does not

`device-online-100-102-103.pcapng` contains the only INIT exchanges in the whole capture corpus,
and both are ACCEPTED:

```
frame 2997   19 (INIT)   100 -> 102   F1=FFFF F2=0001     LAPB  I  N(S)=0 N(R)=0   "ROUTING"
frame 2999   13 (reply)  102 -> 100   F1=FFFF F2=0001     LAPB  I  N(S)=0 N(R)=1
frame 3030   19 (INIT)   100 -> 103   F1=FFFF F2=0001
frame 3037   13 (reply)  103 -> 100   F1=FFFF F2=0001
```

No `0x17` anywhere in that capture. Flags 1 and Flags 2 are identical to ours, which closes off the
frame contents for a second time - see step 1.

Two things the accepted INITs show that we did not have:

**1. The INIT is the FIRST I-frame on the link.** `N(S)=0, N(R)=0`. Ours is also sent immediately on
link Active, so this may match - but it is worth confirming our N(S) really is 0 rather than assumed.

**2. Every INIT in the corpus is sent BY D100, to 102 and to 103 - both machines D100 already has
configured as link peers.** There is no example anywhere of a machine accepting an INIT from a
system it did not already know as a LINK peer.

That sharpens the hypothesis to something testable:

> A peer accepts an INIT from a system it has defined as a LINK peer. A ROUTE is not enough.

`DEFINE-SYSTEM-ROUTE` gave D100 and D103 a route to 19999 - and the route is visibly present in
`LIST-ROUTING-INFO` - but a route says "to reach X, go this way", not "X is the machine on the other
end of this line". The frames immediately before the accepted INIT are on a different TCP stream
(an operator typing at a console), so that INIT was triggered by a COMMAND, most likely a link
start - which is exactly the step our setup never performed on the peer's side.

**Not proven.** It fits every observation so far and contradicts none, but the corpus contains no
counter-example either way, and the test - defining or starting a link toward 19999 on D100 - changes
a real machine's configuration and has not been run.

## Step 6 - the LAPB layer matches too. The wire is fully exonerated.

Step 5 flagged `N(S)=0` as ASSUMED for our own INIT. Checked it, from the capture already on disk,
by reading the LAPB address and control bytes in front of each datagram:

```
ours    7E 09 00   2113 0019 ...    control 00 -> I-frame, N(S)=0, N(R)=0
theirs  7E 09 20   21FE 0017 ...    control 20 -> I-frame, N(S)=0, N(R)=1
```

Against the known-good accepted exchange in `device-online-100-102-103.pcapng`:

| | real (accepted) | ours (NAKed) |
|---|---|---|
| initiator's frame | `I N(S)=0 N(R)=0` | `I N(S)=0 N(R)=0` |
| responder's frame | `I N(S)=0 N(R)=1` | `I N(S)=0 N(R)=1` |
| responder's subtype | `0x13` reply | `0x17` INNAK |

**Identical at every layer that can be observed.** The assumption held.

## Where that leaves the investigation

Everything checkable on the wire is now eliminated:

 - the datagram's markers, subtype, Flags 1 and Flags 2 - identical to an accepted INIT (step 1).
 - the node number - re-ran as 101, NAKed the same (step 2).
 - the LAPB address, control byte and sequence numbers - identical (step 6).
 - the frame is not built from constants in either binary, so there is nothing more to carve for it
   (step 3b).

The peers answer at exactly the right moment, in the right LAPB position, to a frame that matches a
known-good one byte for byte - and refuse it. **The remaining variable is not on the wire at all: it
is what the peer knows about us before the INIT arrives.**

That is the link-peer hypothesis from step 5, now the only survivor rather than merely the best fit.
Testing it means defining or starting a link toward our system on D100, which changes a real
machine's configuration - deliberately left for Ronny rather than done unattended.

## Step 7 (2026-08-08) - the manual says our routes are BACKWARDS, and there is no LINK command

Read AFTER the reboot, from a manual that has been in the repo the whole time:
`Operations/Cosmos/ND-30.025.02 COSMOS Operator Guide.md`, section 2.5.4 "Defining the Route to the
Remote Systems":

> "When defining the route to remote systems, it is **not necessary to define the route to the
> adjacent system(s)**. By adjacent system we mean the system to which the local system has a direct
> link. In other words, the system that is physically connected to the local system with an HDLC or
> Megalink cable."

Section 2.5 gives the whole start-up sequence, and it has FOUR steps, not one:

```
SET-PRIVILEGED
DEFINE-LOCAL-SYSTEM,999                 <- optional; XMSG does it at init in this version
DEFINE-REMOTE-NAME,,MAIN,999            <- a NAME for every system, adjacent or not
DEFINE-REMOTE-NAME,,FORMAT,998
DEFINE-REMOTE-NAME,,BATCH,997
DEFINE-REMOTE-NAME,,TEST,996
DEFINE-SYSTEM-ROUTE,,BATCH,FORMAT       <- ONLY for the NON-adjacent ones
DEFINE-SYSTEM-ROUTE,,TEST,FORMAT
START-LINK,1360,,,-1,,
```

In that example FORMAT is the adjacent machine and gets **no route at all**. Only BATCH and TEST,
which sit behind FORMAT, get one.

**Two corrections to this document's own step 5.**

1. **There is no `DEFINE-LINK` / `DEFINE-SYSTEM-LINK` command.** A grep for `DEFINE-[A-Z-]*LINK`
   across `Operations/Cosmos` and the 210373L manual returns nothing. The step-5 phrase "defined as a
   LINK peer" named a thing that does not exist. What does exist is `DEFINE-REMOTE-NAME` (identity),
   `DEFINE-SYSTEM-ROUTE` (path, non-adjacent only) and `START-LINK` (the physical line).

2. **Our routes are pointed the wrong way.** The topology is `D100 <-> 19999 <-> D103`, so 19999 is
   ADJACENT to both peers. What was configured:

   | Machine | Configured | What 2.5.4 requires |
   |---|---|---|
   | D100 | `DEFINE-SYSTEM-ROUTE,,D19999 D103` | none for 19999 (adjacent); `DEFINE-SYSTEM-ROUTE,,D103 D19999` for the far one |
   | D103 | `DEFINE-SYSTEM-ROUTE,,D19999 D100` | none for 19999 (adjacent); `DEFINE-SYSTEM-ROUTE,,D100 D19999` for the far one |

   D100 was told "to reach 19999, go via D103" while D103 is only reachable *through* 19999. That is
   a loop, and it contradicts the cable D100 can see for itself. A machine holding that route has
   been told, in its own terms, that 19999 is NOT on the other end of this line - which is precisely
   the state step 5 concluded produces the NAK.

**Also unmodelled here:** `DEFINE-REMOTE-NAME,,<name>,<number>` must exist for every system. Nothing
in the session notes shows 19999 was ever NAMED on D100 or D103, only routed. And section 2.5.5 says
`LIST-ROUTING` answered with all CRs **initialises the network** - that is the deliberate operator
trigger for the INIT exchange, which explains step 5's observation that the accepted INIT in the
corpus followed console typing on a separate TCP stream.

**Still not proven** - it has not been run. But it replaces "define a link (command unknown)" with a
concrete, manual-backed sequence, and it explains the NAK without needing anything new on the wire.

The lesson is [[grep-reference-manuals-first]] again: this was in an operator guide already in the
repo, while the investigation went to the binaries.

## Step 8 (2026-08-08) - RAN IT. D103 now reaches us. INNAK is NOT fatal.

A Windows reboot had wiped both machines' XMSG kernel tables (system table down to the local entry,
zero links), so the step 7 sequence was built from scratch rather than unpicked. **The XROUT NAME
table SURVIVED while the XMSG kernel tables did not** - `D100`, `D102`, `D103` were still named on
both machines; only `D19999` was missing. Two separate stores, worth knowing before a rebuild.

Applied on D100: `DEFINE-REMOTE-NAME,,D19999,19999`, `DEFINE-SYSTEM-ROUTE,,D103,D19999`, no route
to 19999, `START-LINK` LU `1360` with max attempts `-1`. Mirrored on D103 with
`DEFINE-SYSTEM-ROUTE,,D100,D19999`.

**Result - D103's `LIST-ROUTING-INFO`, which yesterday said "not accessible":**

```
  19999  L: *->19999
         T: *->19999
         A: *->19999                                  <- ACTUAL path. It works.
    100  L: *->19999->100
         T: *->19999, but no access to system 100
         A: *->19999->, but no access to system 100
```

D103 reaches us. It then sent a real `ReachabilityRequest` (answered), and real `Data` datagrams
addressed to node 100 - so it is actively trying to route THROUGH us. **The INNAK still arrives and
is still unanswered by our node, and it did not stop any of that.** So `INNAK` is not a rejection of
our identity: reachability, naming and routing all completed alongside it. What the frame does mean
is still unproven; what is now settled is that it is not the blocker it was assumed to be.

The remaining failure is one leg only:

```
[link] hdlc-out:127.0.0.1:10362 SendData refused: link not Active (status Starting)
[relay] DROPPED from hdlc-in:10366 for node 100: outgoing link refused the datagram
```

### Why the D100 leg cannot come up - a wiring fact, not a protocol fault

D100's `RetroCore.ini` has TWO controllers: `HDLC 1 --listen=10362` and `HDLC 2 --listen=10364`.
Its SINTRAN knows exactly ONE link LU: `START-LINK,1364` is refused with *"Illegal/Reserved Logical
Unit Number (LUN) for link"*, so LU `1360` is the only one. Socket state shows who owns it:

```
127.0.0.1:10362  <- 127.0.0.1:6899   pid 33188  = D102 (F:\RC\RonnyTest\HDLC2)
127.0.0.1:10362  <- 127.0.0.1:6140   pid 36900  = our relay
```

**D102 already holds D100's HDLC 1 line.** LU `1360` is the D100<->D102 wire; our relay is a second
client on the same listening socket and is ignored. That is why D100 sits in state `Call`
transmitting `SABM` with `TXData/Retry/RXBad = 0/0/0` - it is not receiving from us at all, and our
LAPB is not at fault (it answers a SABM with UA in any state, `LapbLayer.cs:486`).

Yesterday's relay reached D100 on 10364; today nothing is listening for a link there because
SINTRAN has no LU for HDLC 2. Whether that changed at the reboot or was always luck is UNVERIFIED.

**So the D100 leg needs a wiring decision, not more protocol work:** free LU 1360 for us, or give
D100's SINTRAN a second HDLC link. Left for Ronny - it stops or rewires a running machine.

## Step 9 (2026-08-08) - the D100 line WORKS now. Both peers reach us. One bug left.

D100's `RetroCore.ini` was edited to SWAP the two HDLC listen ports, so controller 1 - the only one
LU `1360` can use - sits on 10364 where only our relay dials, instead of 10362 where D102 dials.
Original lines are kept commented directly above; restore them to give D102 its line back. Backup:
`F:\RC\RonnyTest\HDLC1\RetroCore.ini.bak-2026-08-08`. D100 was restarted and reconfigured.

**The outbound link came up on the first attempt, for the first time ever:**

```
[relay] outbound link Starting -> Active (LAPB connected)
[relay] announcing to outbound peer 100
```

and D100's own `LIST-LINKS` agreed, having learned who we are:

```
No  Addr. State Sysid Rcv Xmit  Lun Timeout  Soft-stat-hard TXData/Retry/RXBad
 1 152164  Run 19999 41RR 410I 1360  10/Off       0       0          6/4/10
```

`State Run`, `Sysid 19999`. D100's `LIST-ROUTING-INFO` then answered `19999  A: *->19999`. **Both
machines now reach our node.** Note `SET-PRIVILEGED` is *"Command not recognised"* in this build -
the privileged warning is emitted automatically on the first privileged command instead.

### The bug this exposed: we advertised a route that loops

D100's first path query for 103 came back:

```
    103  L: *->19999->103
         T: *->19999->100->19999->100->19999->100->... *Loop suspected*
         A: *->19999->100->19999->100->19999->100->... *Loop suspected*
```

D100's own local table (`L:`) is right. The looping paths are built from what WE advertised, and we
advertised `via [100]` for node 103 because `topology-d19999.json` still describes the OLD Ethernet
arrangement where D103 sat behind D100. Forwarding was never wrong - the runner logs
`inbound link on 10366 reaches [103], outbound to 127.0.0.1:10364 reaches [100]`. Only the
advertisement was.

Fix committed: **`SRC/Xmsg.Live.Runner/topology-d19999-relay.json`**, where both peers are
`"reach": "neighbour"` with no `via`, because both are ADJACENT to us - the same section 2.5.4 rule
that fixed the ND side. `topology-d19999.json` must NOT be used for a relay run.

### Where it stopped

With the corrected topology both links went Active immediately and both peers were announced to.
Then D100's XMSG degraded: `LIST-ROUTING-INFO` began repeating the same system block over and over,
reported `no route to system 19999` for a route that had just worked, and ended with
`Buffer handling error - internal to Command prog.` A `DEFINE-SYSTEM-ROUTE` re-applied cleanly
(`Ok`) without restoring it.

**UNVERIFIED which of these caused it** - the repeated querying, the unanswered `INNAK`s, or the
`RXBad 10` on the link. Nothing more was poked; the relay was stopped and D100 left alone. D100
most likely needs `STOP-XMSG` / `START-XMSG` and the section 2.5 sequence re-run before the next
attempt.

**The one code change this points at:** our node answers an `INNAK` with
`*** NO REPLY BUILT *** (this hangs the caller)`. Both peers send one on every announce and we have
never replied to any of them. That is the most likely thing to fix before the next live run.

## Step 10 (2026-08-08) - D100 recovered cleanly; D103's HDLC is stuck TALKING TO ITSELF

D100's degraded XMSG from step 9 recovered exactly as the manual says it should:
`SINTRAN-SERVICE` / `STOP-XMSG` / `START-XMSG`, both answering `OK`. The restart **clears the whole
name table** - `LIST-NAMES` afterwards showed only `*XM-FIDO`, which had re-registered itself, with
even the machine's OWN name gone. So a restart means re-running the entire section 2.5 sequence,
including `DEFINE-REMOTE-NAME,,D100,100` for the local name. Both machines were then rebuilt
symmetrically and both accepted every command.

**The new blocker is a TCP artefact, not XMSG.** D103's `RetroCore.ini` says
`device add HDLC 1 --connect=localhost:10366`. When the relay holding that listener is killed, D103
re-dials - and with the listener gone it can complete a TCP **simultaneous open against itself**:

```
LocalPort RemotePort       State  OwningProcess
    10366      10366 Established          27996     <- D103 connected to D103
    10366          0       Listen          41916     <- our relay, listening, never dialled
```

D103's own `LIST-LINKS` proves what that means:

```
No  Addr. State Sysid Rcv Xmit  Lun Timeout  Soft-stat-hard TXData/Retry/RXBad
 1 152164  Run   103 40RR 40RR 1360  10/Off       0       0          0/0/0
```

**`Sysid 103` on D103's own link** - it brought LAPB up against itself and learned its own system
number as the peer. That is impossible in a real topology, so the field is a reliable detector for
this state. `State Run` looks healthy, which is exactly why it is worth writing down: everything
reads normal except the one field that gives it away.

Clearing it needs D103's RetroCore process restarted, and D103 is NOT covered by the
D100/D102 restart permission, so it was left alone.

**Operational rule this earns:** start the relay's listener BEFORE the peer that dials it, and when
stopping a relay expect the dialling machine to need a restart afterwards. Check
`LIST-LINKS`'s `Sysid` against the peer's real number before trusting a `Run` state.

## Next steps, in order

0. **(2026-08-08) DONE** - the section 2.5 sequence was run on both peers; D103 reaches 19999. See
   step 8. What remains is the D100 wiring decision above, plus deciding what our node should
   ANSWER an INNAK with (today: `*** NO REPLY BUILT ***`).

1. Add `0x17` to `SintranPacketSubtype` with these captured bytes as the evidence — but name it for
   what was OBSERVED (a header-only frame both peers send in reply to a reachability request), NOT
   "initialization reject", which step 3b shows is unearned.
2. Recognise it on receive and report the raw fact — "peer answered our announce with subtype 0x17,
   Flags 2 = 0xFFFD" — rather than the current silence. That is useful without claiming a meaning.
3. To settle what it IS, the next artifact is the SINTRAN HDLC driver, not XMSG: `MP-P2-HDLC-DRIV.NPL`
   is the third party on that link and neither binary searched here covers it. Marker 2 `0xFE`
   appearing nowhere in the XMSG corpus points the same way.
4. Independently, ask a real machine: `LIST-FRAMES` on D100 with tracing enabled shows how SINTRAN
   itself labels these frames. That is cheaper than any carve and the machine is authoritative about
   its own traffic.
