# The ND-to-ND capture rig, 2026-08-08

**Why it exists.** Every real capture we hold was made on a D100-to-D102 pair that no longer
exists: our C# relay took D100's single usable HDLC controller, so there was no real ND-to-ND link
left to sniff. Nothing new could be recorded, and three tasks were blocked on recordings
(#18 the close, #23 the FA operations, #28 the out-of-step counter). This rig rebuilds a real pair
WITHOUT disturbing D100.

**The pairing.** D102 listens, D103 dials it. D100 and the C# node are untouched.

```
D102/RetroCore.ini    device add HDLC 1 --listen=10370            (was --connect=localhost:10362)
D103/RetroCore.ini    device add HDLC 1 --connect=localhost:10370 (was --connect=localhost:10366)
```

Backups: `RetroCore.ini.bak-2026-08-08-pairing` in each folder. Restoring either line and
restarting that emulator puts the old arrangement back. **While this rig is up, D103 no longer
dials our relay on 10366, so the C# node has no inbound peer and sits idle. That is expected.**

**The capture.**

```powershell
& "C:\Program Files\Wireshark\tshark.exe" -i "\Device\NPF_Loopback" -f "tcp port 10370" -w nd-to-nd.pcapng
```

## Bring-up, in order, and the two things that bit

Restarting the two emulators is not enough - a reboot clears the XMSG kernel tables (the XROUT
NAMES survive, the link table does not), and D102 came up with XMSG not started at all.

 - **D102 booted into `SET-UNAVAILABLE`**, which blocks new logins on the pooled TCP terminals, so
   it could not be reached at all. Only the machine's own console window can clear that; Ronny
   typed `@SET-AVAILABLE` there. Nothing in the terminal MCP can reach console terminal 1.
 - **D102's XMSG kernel was not running** - `X-COMM` answered "XMSG is either not generated, not
   loaded or not started". Fixed with `@SINTRAN` then `START-XMSG` ("OK: XMSG started.").

Then, on **D102**:

```
X-C:  DEF-REMOTE,,D103 103          note the SPACE before the number
X-C:  START-LINK,1360,,,-1,,        LU is OCTAL, -1 = retry forever
@     MODE (PACK-ONE:COSMOS-BASIC)COS-START-E04:MODE,,
@     SET-AVAILABLE                 IN THE SAME SESSION - the mode file ends with SET-UNAVAILABLE
```

and on **D103**:

```
X-C:  DEF-REMOTE,,D102 102          answers "Another port already has this name" - BENIGN,
                                    the XROUT name survived the reboot
X-C:  START-LINK,1360,,,-1,,
```

`COS-START-E04:MODE` is what starts the file server. Without it a remote listing answers
`REMOTE FILE SERVER IS NOT AVAILABLE` even though the link is perfectly up. Success line:
`Server 1 started.     No of FACs attached: 30`.

**Link up, confirmed on both sides:**

```
No  Addr. State Sysid Rcv Xmit  Lun Timeout  Soft-stat-hard TXData/Retry/RXBad
 1 152164  Run   103 40RR 40RR 1360  10/Off       0       0          0/0/0
```

## What has been driven so far

**Baseline listing** - `LI-FI D102(SYSTEM).,,` typed on D103, answered by a real ND file server:

```
FILE 0 : D102.(PACK-ONE:SYSTEM)SINTRAN:DATA;1
FILE 1 : D102.(PACK-ONE:SYSTEM)MACM-AREA:DATA;1
...
FILE 9 : D102.(PACK-ONE:SYSTEM)PAPERTAPE-READER:;1
```

On the wire, the connect exchange starts at Flags 1 `0000`:

```
103 -> 102  2113 000E 0066 0067 0000 0022 DDEF   2100 86E4 0066 0000 0067 044C 0022
            1B41 0012 FF0A "*FA-SERVER" FE04 "D102" 07E2 0000 0002 6400 A200 FF00
102 -> 103  2113 0003 0067 0066 0000 0001 DE1B   the acknowledgement
102 -> 103  2113 000E 0067 0066 0000 0022 DDEF   ... 1B02 0012 FF0A "*FA-SERVER" FE04 "D102" ...
103 -> 102  ... F1 0001 ...                       the next exchange
```

Note `1B02` in the answer against `1B41` in the request - the XROUT service byte differs between
the letter and its reply, which our own notes have never recorded.

## Driven into `nd-to-nd-scenarios.pcapng`

 - **A full read session including the close** (task #18):
   `COPY-FILE "NDTEST1:OUT",D102(SYSTEM).WRTEST1:OUT` - completed clean, so the capture holds a
   real client's close and a real server's answer to it.
 - **Two remote file creations at different name lengths** (task #23), which is what separates a
   real field from padding:
   `COPY-FILE D102(SYSTEM)."NDNEW1:TXT",NDTEST1:OUT` and
   `COPY-FILE D102(SYSTEM)."NDLONGERNAME12:DATA",NDTEST1:OUT`.
 - **Two TAD endings**, both server-driven: a remote `LOGOUT`, and the idle timeout.

### `CREATE-FILE` DOES take a remote spec - it just must not be quoted

**CORRECTED.** An earlier version of this file said `CREATE-FILE` accepts no remote spec at all.
That was wrong: it was concluded from the two QUOTED forms without trying the bare one. Ronny
pointed at the bare form and it works.

```
CREATE-FILE D102(SYSTEM).NDNEW9:TXT,10       Ok - accepted, no error       <- USE THIS
CREATE-FILE D102(SYSTEM)."NDNEW1:TXT",10     ILLEGAL CHARACTER IN PARAMETER
CREATE-FILE "D102(SYSTEM).NDNEW1:TXT",10     UNKNOWN REMOTE SYSTEM NAME
```

So the quoting rule is narrower than "quote the file being created": `COPY-FILE` wants the quotes
(prefix OUTSIDE them, as the 2026-08-04 write test recorded), and `CREATE-FILE` wants none at all.
`RENAME-FILE` with quotes is refused like the quoted `CREATE-FILE`; the bare form there is untried.

`COPY-FILE D102(SYSTEM)."NAME",<local>` also works and creates the file, so both routes exist. A
later listing shows `FILE 46 NDNEW1:TXT` and `FILE 47 NDLONGERNAME12:DATA`, confirming the two
`COPY-FILE` creations landed on the real server.

### The CONNECT-TO local character - what the program itself says

`CONNECT-TO` with no system name gives a `C-T:` prompt. Its own `HELP`:

```
The ASCII character : 0 octal, will terminate your connection if
typed twice within 3 seconds.After you log out, the remote connection
can be terminated by typing this character once.

          HELP  <COMMAND: >
          LIST-SYSTEMS  <SYSTEM-NAME: >
          CONNECT-TO  <SYSTEM NAME: >
          LIST-LOCAL-CHARACTER
          LIST-TIMEOUT-VALUES
          EXIT
          SERVICE-PROGRAM
```

`SERVICE-PROGRAM` gives `CT-SERV:` and a further set, including the one that matters:

```
          CHANGE-LOCAL-CHARACTER  <ASCII VALUE: >
(SYSTEM): RECONNECT-TAD  <TAD LOGICAL UNIT NO: >  <SYSTEM-NAME: >
(SYSTEM): SET-TIMEOUT-VALUES  <NOT LOGGED IN: >  <NOT ACTIVE: >
(RT):     CHANGE-CONNECT-TYPE  <DESIRED TAD TYPE: >
(RT):     TIMEOUT-OFF / TIMEOUT-ON
          INITIALIZE-SCRIPT  <SCRIPT-FILE: >
          DUMP-PROGRAM  <PROG-FILE: >
          RESTART-PROGRAM
```

**The value is OCTAL.** `CHANGE-LOCAL-CHARACTER 30` then `LIST-LOCAL-CHARACTER` answers
`OCTAL VALUE OF LOCAL CHARACTER: 30`, and the next connection banners
`LOCAL CHARACTER IS : 30 (ascii value)` - so 30 octal = 0x18. The banner's "(ascii value)" is
misleading; it is the same octal number.

### NOT ACHIEVED - the client-controlled ending could not be triggered

Tried, all while logged in on the remote and all with no effect:

 - `0x00` once, and `0x00 0x00` in one write, with the default local character 0.
 - `0x18` twice in one write, and `0x18` as two separate writes, after setting the character to 30
   octal.

`0x00` never reaching is expected - a bare NUL is the telnet NVT's CR padding and gets dropped in
transit - but `0x18` is an ordinary control byte, so something else is eating it. **UNVERIFIED
guess, recorded as a guess:** the local character is handled by the CONNECT-TO program reading its
own terminal, and a pooled telnet terminal may not deliver it the same way a real console would.

What this means for the task: the two endings we DID capture are both driven by the far side (a
remote logout, and the timeout). **The client-driven ending is still missing from the capture.**

## THE OUT-OF-STEP MEASUREMENT (task #28) - and it corrects two of my own claims

`nd-to-nd-outofstep.pcapng`. Setup, all recorded:

 1. Counter read off the wire before touching anything: both sides at **`0x0134`**.
 2. On D102 only: `@SINTRAN` / `STOP-XMSG` ("OK: XMSG terminated.") / `START-XMSG`, then
    `DEF-REMOTE`, `START-LINK`, `COS-START-E04:MODE`, `SET-AVAILABLE`. **D102's counter is now
    zero; D103's XMSG was never touched.**
 3. `LI-FI D102(SYSTEM).,,` from D103.

**RESULT: it works.** The real server served the full listing. So a real ND handles the case that
kills our node, and the difference is now readable byte for byte.

### CORRECTION 1 - Flags 1 is NOT one shared per-link counter

An earlier note (SESSION-NOTES section 7, and task #28) said it was, read off
`capture-list-files.txt` where a request, both acknowledgements and the reply all carried `0x018A`.
**That was a coincidence of two counters that happened to be equal.** Here the two directions run
plainly separate streams at the same time:

```
D103 -> D102   F1 = 000F  0010  0011  0012  0013 ...
D102 -> D103   F1 = 001B  001C  001D  001E  001F ...
```

### The rule, from the connect exchange in this capture

```
103 -> 102  2113 000E 0066 0067 0015 0022 DDDA   the *FA-SERVER letter      F1 = 0015
102 -> 103  2113 0003 0067 0066 0015 0001 DE06   the acknowledgement        F1 = 0015  ECHOES
102 -> 103  2113 000E 0067 0066 0020 0008 DDE9   the CONNECTION CONFIRM     F1 = 0020  ITS OWN
            ... 07D2 0002 0004 6400
103 -> 102  2113 0003 0066 0067 0020 0001 DDFB   the acknowledgement        F1 = 0020  ECHOES
```

**An acknowledgement echoes the sender's number. An originated frame carries the sender's OWN
counter.** The confirmation is originated, not an echo - `0x0020` against the letter's `0x0015`.

### CORRECTION 2 - the originate model in our code is RIGHT

`FaServer.OnConnect` passing `XmsgAnsweredFlags1.None` matches a real server. The 2026-08-04
decision was correct, and the echo change tried on 2026-08-04 and again on 2026-08-08 was wrong
both times - which is exactly why both attempts earned a XENSE reject. That question is now closed
by measurement rather than by argument.

### So what IS our defect

Not the shape - the VALUE. Our per-link outgoing counter restarts at zero when our process
restarts, while the peer's XMSG keeps running and keeps expecting numbers above what it has already
seen from us. Our acknowledgements echo (so they look fine) and never advance our counter, so the
first thing we originate after a restart - the announce, then the confirmation - carries `0x0000`
and the peer drops it as old. In THIS capture both counters restarted together, because stopping
XMSG tore the link down at both ends; our restart does not do that.

**Fix direction, still to be proved:** never originate a number the peer has already seen on this
link - carry the counter forward across our own restarts rather than starting at zero. Not
implemented, not verified.

## THE REAL CLOSE (task #18) - our bytes were already right

Decoded from the `COPY-FILE` read session in `nd-to-nd-scenarios.pcapng`:

```
103 -> 102  2113 000E 0066 0067 0103 000A DD04   2100 8284 0066 05CD 0067 044C 000A
            0782 0040 000C 8000 0000                      the finished message
102 -> 103  2113 000E 0067 0066 0103 0008 DD06   2100 8284 0067 044C 0066 05CD 0008
            07C0 000C 0040 0000                           the close, 8-byte body
```

Ours, from the live D103 session the same day
(`DOC/captures/FA-OPERATIONS-2026-08-08/runner-d103-session.log`, lines 156-158):

```
in    0782 0882 0002 8000 0000
out   07C0 0002 0882 0000
```

**The rule is `07C0 <w2> <w1> 0000`** - an 8-byte body carrying the finished message's two words
SWAPPED. Both match it exactly, so **our close is already correct** and D103 accepted it silently.

That kills this task's original premise ("what does a real client's close look like that ours does
not"). The `XEIMA` D100 answers with is most likely a symptom of the Flags 1 defect in task #28 -
the peer never accepted the frames that would have kept the conversation alive, so by the time our
close arrives it really is gone. UNVERIFIED; the cheap check is to fix #28 and see if it stops.

Noted in passing: in a full `COPY-FILE` BOTH sides send a `07C0` - the answering side on the
finished message, and the client later for its own conversation. We only ever send one; whether
that matters to us is untested.

## THE CLIENT-CONTROLLED TAD ENDING - captured, after the emulator NUL fix

`nd-to-nd-tad-ending.pcapng`. This is the ending that could NOT be triggered earlier in the day.

### Why it failed before: the emulator ate every NUL

`Emulated.HW/Common/Network/NetworkServer.cs` did `received.Replace("\0", "")` on every chunk from
the terminal socket, so a bare NUL never reached SINTRAN. That is the shared socket for every
emulated machine. Ronny rebuilt and redeployed RetroCore to D100, D102 and D103; the fix keeps
standalone NUL and strips only the telnet NVT CR-NUL padding (RetroCore commit `600b3fddc`).

### The rule the program states about itself

From `CONNECT-TO` with no system name, then `HELP`:

> The ASCII character : 0 octal, will terminate your connection if typed twice within 3 seconds.
> After you log out, the remote connection can be terminated by typing this character once.

### SEND THE TWO NULS AS TWO SEPARATE WRITES

This is the part that cost three failed attempts and is worth knowing for anything automating a
terminal:

```
sendraw 0000     -> nothing happens          both NULs in ONE write
sendraw 00
sendraw 00       -> DISCONNECTS              two SEPARATE writes
```

The program is counting keystrokes as they arrive, so two NULs coalesced into a single read do not
register as two. The same is presumably true of any doubled local character.

### What the user sees - and how it differs from the far-side endings

```
-- DISCONNECTED FROM: D102 --
Returning to: D103 , as user: SYSTEM
```

**No `-- CONNECTION TIMEOUT --` banner and no `--EXIT--`**, and the remote session was still logged
in (`R@`) at the time. Contrast the two far-side endings captured earlier, which both print the
timeout banner first and require the remote to be logged out.

### On the wire

Every TAD data frame in the session carries XMCSM `9694` with Flags 2 `0108`. The final frame from
the client switches both:

```
data     2113 000E 0066 0067 000A 0108 DCFF   2100 9694 0066 05C7 0067 044C 0108   0000 0002 1800
ending   2113 000E 0066 0067 000B 0008 DDFE   2100 8294 0066 05C7 0067 044C 0008   0000 0002 0900
```

**NOT YET DECODED:** what `8294` against `9694` means field by field, and what the body's trailing
`0900` against `1800` carries. The bytes are recorded verbatim here so the decode can be done
against the TAD model rather than by eye - the two frames differ in more than one place and
guessing which difference is the signal is exactly the mistake made twice today.

## RENAME IS OPERATION 0x000C SUB-FUNCTION 0x009A, not operation 0x04 (task #23)

`nd-to-nd-fa-ops.pcapng`. `RENAME-FILE D102(SYSTEM).NDNEW1:TXT,NDREN1:TXT` typed on D103 was
accepted, and a follow-up listing confirms it:

```
FILE 46 : D102.(PACK-ONE:SYSTEM)NDREN1:TXT;1        same file number, new name
```

**Note the bare, unquoted form again** - the quoted `RENAME-FILE` refused earlier, exactly like
`CREATE-FILE`.

**Every operation code in the whole capture:**

```
6 x 0x000C     4 x 0x0002     2 x 0x0003
```

**There is no `0x04` anywhere.** The rename travelled as `0x000C`:

```
07F0 0004 81 00 D761   92 000C   92 0002   F2 0001   92 009A   F2 0002
     8C 80 59  B0 3C  "NDNEW1:TXT" 27 00.. 00        the existing entry, 0x3C = 60 bytes
     86CF ...  B0 19  "NDREN1:TXT" 27 27 00 27 ...   the new name, 0x19 = 25 bytes
```

So **`0x000C` is a file-ENTRY operation family, not "listing"**, and the FOURTH tagged word selects
the action. That extends the three sub-functions already recorded on 2026-08-05:

| sub-function | action |
|---|---|
| `0x0078` | next FILE entry (the walk) - and a NAMED lookup reuses it, with the name in the body |
| `0x00A4` | the DIRECTORY entry |
| `0x008C` | the USER entry |
| **`0x009A`** | **change a file entry - what RENAME-FILE sends** |

**MEASURED:** the operation word, the sub-function word, and that both the old and the new name
travel in one request. **INFERRED, not proved:** the field-by-field reading of the two `B0` records
(60 bytes then 25) - the shapes are clear but which sub-fields mean what is not established here.

**What this means for the task.** `FA-COMMAND-NAMES-READ-FROM-BINARY-2026-08-06.md` lists
`0x04 Reserve-file-entry`-style NAMES read out of the binary, and `0x04 Change-file-entry-id` is one
of them. That table is a command-NAME table; it does NOT follow that a client ever sends `0x04` on
the wire for a rename. On this evidence the ordinary operator route does not.

## Still to drive

 - **Out-of-step (#28).** Restart XMSG on ONE side mid-link so its counter resets while the other
   keeps climbing, then repeat the listing. No recording we hold contains this - `grep -c FFDE` is
   0 in all four of the 2026-08-04 files.
 - **FA operations (#23).** `0x01` file-entry-disconnect, `0x04` change-file-entry-id, `0x0D`
   device-function, plus a second `CreateFile` at a different name length.
 - **The close (#18).** A full session from connect to close, to diff our close frame against a
   real one.
 - **A client-controlled `CONNECT-TO` ending.** Open a TAD session, end it with ASCII `0x00`, then
   exit the client session, and record what is exchanged on that ending.
