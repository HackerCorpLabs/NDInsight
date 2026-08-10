# FA operations capture, 2026-08-08

**Code under test:** commit `65fc1d0` plus the relay-host wiring fix made during this sitting.
**Machines:** D100 (terminal 9010, HDLC listen 10364), D103 (terminal 9003, dials 10366),
D19999 = our C# runner as a relay between the two.
**Files:** `fa-ops.pcapng` (the D100 attempts), `fa-ops-d103.pcapng` (the D103 session).
Both on `\Device\NPF_Loopback`, filter `tcp port 10364 or tcp port 10366`.

---

## 1. A relay served on only ONE of its two links - found, fixed, verified

`Program.RunRelay` built the inbound host with the server list and the outbound host with none:

```
XmsgNodeHost inboundHost  = new XmsgNodeHost(inbound,  ..., BuildServerList(...));
XmsgNodeHost outboundHost = new XmsgNodeHost(outbound, ...);          // no servers
```

Forwarding never touches a server, so the relay looked healthy the whole time and its counters kept
moving. Anything a peer on the outbound link addressed **to us** reached `XmsgServerHost.Route`,
found no server to hand it to, and produced nothing but the datagram acknowledgement.

**What it looked like.** `LI-FI D19999(SYSTEM).,,` typed on D100 hung the terminal for about two
minutes and then printed

```
NO ANSWER FROM REMOTE SYSTEM; FILE-ACCESS CONNECTION ABORTED
```

**What the capture shows** (`fa-ops.pcapng`, frames 79-93). The reachability request is answered
normally, then D100's connect letter arrives and gets a bare acknowledgement:

```
frame 85  D100 -> us   2113 000E 4E1F 0064 0000 0024 9037    header, subtype 000E = Data
                       2100 86E4 4E1F 0000 0064 021F 0024    sub-header, dest port 0000 = XROUT
                       1B41 0014 FF0A 2A46412D534552564552   XSLET letter, "*FA-SERVER"
                       FE06 443139393939 07E2 ...            "D19999" and the extras
frame 87  us -> D100   2113 0003 0064 4E1F 0000 0001 9065    subtype 0003 = Ack. And nothing else.
```

Note `FF0A` / `FE06`: the tag byte carries the length, so the letter is the same shape as the older
`FE04 "D102"` recordings with a longer system name. Nothing in the parse depends on 4 characters.

**The fix.** Both hosts get the same server instances, through
`Program.BuildRelayHosts`, which exists so the two constructions cannot drift apart again. One set
of instances, not one per link: a session is keyed by its wire port and records the node and system
it belongs to, and every reply is built through whichever host routed the request, so answers still
leave by the link they arrived on.

Pinned by `SRC/Xmsg.Live.Runner.Tests/RelayHostWiringTests.cs`, which asks each host in turn
whether it knows about any server. Asking the pair as a whole is what the old code effectively did,
and it passed.

---

## 2. What now works, measured

`LI-FI D19999(SYSTEM).,,` typed on **D103**, through the relay:

```
FILE 0 : D19999.(PACK-ONE:SYSTEM)BIG:TXT;1
FILE 1 : D19999.(PACK-ONE:SYSTEM)HELLO:TXT;1
FILE 2 : D19999.(PACK-ONE:SYSTEM)README:TXT;1
FILE 3 : D19999.(PACK-ONE:SYSTEM)THIRD:TXT;1
```

All four files, the directory and user entries, and the walk ending on `EndOfDirectory`.

---

## 3. `CreateFile` 0x0A driven by a real client for the first time (task #23)

```
CREATE-FILE D19999(SYSTEM)."NEWF1:TXT",10
```

reached the server and was accepted:

```
[fa] CreateFile '"NEWF1:TXT"' for system 103: file number 5, 10 page(s) asked for
[fa] short acknowledgement 9 to system 103 at Flags1 0x0011
[fa] reply to system 103: 18 byte(s), FA body 07F000028800908192000A920009F200...
```

**The quotes are part of the name on the wire.** The server received `"NEWF1:TXT"` with both quote
characters, exactly as `COPY-FILE` was already known to send them. The operation word is `92 000A`,
which is `CreateFile`, and the reply is the same 18-byte shape the other short replies use.

**Only ONE sample.** A second `CREATE-FILE` with a longer name was driven but its conversation hit
the stall in section 5, so the name-length comparison the plan asks for is NOT done.

---

## 4. A complete FA session including the close (task #18 background)

The D103 session ran to a clean end:

```
[fa] request ReleaseFileEntry seq 10 from system 103: accepted
[fa] system 103 has finished with conversation 0x0882 (0x0782); answering with a close
[fa] reply to system 103: 8 byte(s), FA body 07C0000208820000...
[in RX] 103->19999 sub=Ack f1=0x0015
```

**No `XEIMA`.** D103 acknowledged our close and said nothing more. Task #18 is about D100 answering
our close with `XEIMA` (-19); on this evidence that is not what a healthy client does, but the
sitting did not get a D100 close to compare - see section 5.

---

## 5. NEW, UNEXPLAINED - some conversations stop dead after the ReserveFileEntry reply

Three FA conversations were opened over the fixed code. One completed, two stopped at the same
point: the connect confirmation goes out, the client's first `ReserveFileEntry` is accepted, our
18-byte reply goes out, the client acknowledges both frames and then sends nothing further until it
times out.

| time | client | conversation | outcome |
|---|---|---|---|
| 15:57 | D100 | 0x0802 | stopped after the ReserveFileEntry reply |
| 16:01 | D103 | 0x0882 | **completed** - full listing, CreateFile, close |
| 16:03 | D103 | 0x0883 | stopped after the ReserveFileEntry reply |

**The two D103 requests are byte-identical** apart from the conversation number, the datagram
Flags 1 and the checksum:

```
16:01  ... 07F0 0882 8000 0001 920002 920001 F20001 A207D0 F20002 8C06 ...   walked on
16:03  ... 07F0 0883 8000 0001 920002 920001 F20001 A207D0 F20002 8C06 ...   stopped
```

and our replies are identical too (`07F0 0002 8000 9081 920002 920001 F200...`, the 18-byte form the
2026-08-05 write-up records as matching the real server apart from the responder session token).

**So the difference is not in the FA message bytes.** What differs between the two is the datagram
layer: the working request arrived at Flags 1 `0x0001`, the stalled one at `0x0017`. That is a
lead, not a conclusion - it has not been tested.

**What is NOT yet known:**

 - whether the rule is "the first conversation after our process starts works and later ones do
   not" - it fits all three rows, but D100's row only fits if its aborted pre-fix attempt counts as
   a conversation on its side, which has not been checked.
 - whether the client is rejecting something in our reply or simply never being woken.
 - whether this is the same underlying fault as task #18's `XEIMA`, which also only ever appeared
   on a later exchange.

**The cheap next measurement:** restart the runner, then drive two `LI-FI` commands back to back
from D103 and compare. If the first walks and the second stops, the rule is confirmed and the
search narrows to per-conversation state on our side.

---

## 6. That measurement, run - and what it actually found (`fa-stall-confirm.pcapng`)

**The "first conversation works" rule is WRONG.** On a fresh runner at 16:30 the FIRST conversation
failed, and earlier than any before it: before a single request, right after the connect
confirmation.

**The confirmation goes out at a stale Flags 1.** Side by side:

```
letter in     2113 000E 4E1F 0067 001B 0024 9019     F1 = 001B
our ack       2113 0003 0067 4E1F 001B 0001 9047     F1 = 001B   echoes, correct
our confirm   2113 000E 0067 4E1F 0000 0008 9050     F1 = 0000   stale
D103          7E 0961 0067 954D                      a LAPB RR, no datagram acknowledgement
```

D103 retries the letter twice more and the terminal gives up. The one conversation that ever
completed had its letter at `0x0000`, where every model agrees - it worked by luck.

**Where the zero comes from.** `XmsgServerHost.EnsureLink` builds a link as

```csharp
_links[node] = new XmsgLink(node, seed, _store.LoadNextFlags1(node));
```

so `NextFlags1` comes from the persisted `xmsg-sequence.state` and never from the Flags 1 of the
frame that just arrived. That file reads `100=0000` / `103=0000`, so a fresh process starts at zero
while the peer has climbed to `0x001B`. This is NOT stale persisted state - the store is at zero,
which is exactly the problem.

**What the real captures say.** A real ND `*FA-SERVER`'s confirmation carries the SAME Flags 1 as
the letter. In both exchanges we hold, the letter, both acknowledgements and the confirmation are
all on one number:

```
FA-READ-WRITE-2026-08-04/capture-list-files.txt  02:20:05  all four at 0x018A
FA-READ-WRITE-2026-08-04/capture-read.txt        02:29:10  all four at 0x01F9
```

So Flags 1 advances per EXCHANGE, not per frame, and two frames sharing one number is normal.

**And yet echoing does not work live.** Changing the confirmation to echo was tried today: D103
stopped ignoring it and answered `NetworkError` with Flags 2 `0xFFDE` - XENSE, "network sequencing
error". The change was reverted.

**This experiment had already been run and recorded.**
`FaServerWiringTests.ConnectLetter_IsRoutedByName_AndAnswered` says so in a comment dated
2026-08-04: "Live, D100 opened at 0x000E while our count stood at 0x000B, and echoing earned a
XENSE reject." Two attempts have now been spent re-running the same rejected experiment. **Read
that test before touching the confirmation's Flags 1 again.**

**The contradiction is the task.** The real captures show the echo; echoing live earns XENSE. A
real server's own count equals the letter's because it never restarts out from under a
conversation - ours restarts at zero while the peer keeps climbing. So the rule is probably neither
"echo" nor "read the store" but learning the peer's position, and whatever it is, it has to explain
the XENSE. Task #28 carries this.

---

## 7. WITHDRAWN - "Flags 1 is one shared per-link counter" is WRONG

> **This whole section is wrong and is kept only so the mistake is legible.** It reads a
> coincidence as a rule. On 2026-08-08 a real ND-to-ND capture
> (`DOC/captures/ND-TO-ND-2026-08-08/nd-to-nd-outofstep.pcapng`) showed the two directions running
> SEPARATE counter streams at the same time - `000F 0010 0011 ...` one way, `001B 001C 001D ...`
> the other. The four-frames-per-value pattern below happened only because the two sides' counters
> were equal in those recordings.
>
> The real rule: an acknowledgement echoes the sender's number; an ORIGINATED frame carries the
> sender's OWN counter. See RIG.md in the ND-TO-ND folder and task #28.

### The original (wrong) reading follows

Read off the real ND-to-ND captures rather than argued. Every datagram in
`capture-list-files.txt`, printed as `time sub dest src F1`:

```
02:19:00.642  sub=000E dest=0066 src=0064 F1=0189      100 asks
02:19:00.648  sub=0003 dest=0064 src=0066 F1=0189      102 acknowledges
02:19:00.652  sub=000E dest=0064 src=0066 F1=0189      102 answers
02:19:00.666  sub=0003 dest=0066 src=0064 F1=0189      100 acknowledges
02:20:05.522  sub=000E dest=0066 src=0064 F1=018A      100 asks   (the *FA-SERVER letter)
02:20:05.528  sub=0003 dest=0064 src=0066 F1=018A      102 acknowledges
02:20:05.546  sub=000E dest=0064 src=0066 F1=018A      102 answers (the confirmation)
02:20:05.557  sub=0003 dest=0066 src=0064 F1=018A      100 acknowledges
02:20:05.575  ... F1=018B ... 018C ... 018D ... 018E ... 018F ... 0190
```

**Four frames per value, both directions, one increment per EXCHANGE.** And the initiator
alternates - the `018C` group is opened by 102, the `018D` group by 100 - so neither side owns the
number. Counting frames per value across `capture-read.txt` gives the same shape (4 or 5 frames per
value, both directions in each group).

**The code's model is per-side.** `XmsgServerHost.ChooseFlags1` allocates from `link.NextFlags1`
when originating, and the test comment states it outright: "Flags 1 is OUR OWN outgoing count, not
the letter's number." On an in-step link the two models produce identical bytes, which is why this
survived.

**This reframes the whole item.** If the counter is shared and per-link, then a node joining a link
that is already running must ADOPT the number from the first frame it sees - and ours does not:
`EnsureLink` takes it from the persisted file. That makes the confirmation only one symptom; the
reachability announce we send on link-up is originated the same way and goes out at `0x0000` too,
before any FA traffic. Fixing the confirmation alone would leave that.

**Which is also the shape of an explanation for the XENSE**: echoing gets the confirmation's number
right, but by then we have already sent an announce at a wrong number on that link. Untested.

**What the captures CANNOT tell us:** there is no XENSE anywhere in any of them -
`grep -c FFDE` is 0 in all four files. Every real conversation we hold is in step from the start,
so no recording shows a real node recovering from the out-of-step case. That route is closed with
what we have; the next step is a live experiment, not more reading.
