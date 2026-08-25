# Handoff - chat product, D100 recovery, and the one fault left

**Written:** 2026-08-23, session ended early because Windows was crashing.
**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\HANDOFF-CHAT-AND-D100-RECOVERY-2026-08-23.md`

Everything below marked MEASURED was read off a machine. Everything marked NOT ESTABLISHED is
open. Nothing here is a guess presented as a fact.

---

## 0. LATEST - the fault is BISECTED. Read this before section 1.

A second stretch of work on 2026-08-23 narrowed it a long way. **The problem is entirely on
D100's outbound side. It is not the hub, not D102, not the sequence drift.**

## 0-STILL-OPEN. IT WORKS FROM A COLD BOOT AND THEN DIES UNDER TRAFFIC.

Section 0-SOLVED below is right about what it says and **too strong about what it implies.** What
is fixed: the machines could never reach each other at all from a fresh boot, and now they do,
automatically. What is NOT fixed, found later the same evening:

```
1. cold boot, boot files do everything      -> LIST-NAMES 102 and 103 answer. WORKS.
2. START-TRUNK 102 on D100                  -> "102 up" here, "100 up" on D102. WORKS.
3. COPY-FILE of CHATSV:PLNC to D102         -> 230583 bytes in 19 s, byte-verified. WORKS.
4. two more small COPY-FILEs, D102 -> D100  -> WORK.
5. COPY-FILE CHAT:PLNC, either direction    -> NO ANSWER FROM REMOTE SYSTEM
6. X-C LIST-NAMES, either direction         -> Remote system is not accessible, INSTANTLY
```

**Everything at the wire level stays healthy while it refuses:**

```
D100  LIST-LINK   1 Run 9800  TXData/Retry/RXBad  1998/0/0     it HAS been transmitting
                  2 Run  103  lun 1362                5/0/0     HDLC to D103 still fine
hub               three Established connections, process alive
```

**The only asymmetry in the tables:**

```
D100's row for 102:   State 0   Seq 821/816   Access *----P   hops 0/1
D102's row for 100:   State 4   Seq 823/818   Access *----P   hops 0/1
```

D102 still believes the link is good; D100 has fallen back to State 0 - and yet BOTH refuse.

**Did not fix it:** `DEFINE-FRIEND-SYSTEM 102` on D100 (answered `Ok`, no change), and
`LIST-NAMES <n>` as a wake-up. Note every `Access` reads `P` and not `F` on both machines - **the
boot files grant no friends at all**, which is a genuine gap to close, but it is not this.

**NOT ESTABLISHED:** whether the trigger is the FA transfers, the volume, the sequence drift or the
known seat leak. **The next step is to decode frames, not to reason about it** - the hub is our own
code and every frame goes through it. This is the same shape as the older HDLC-era note
"one COPY-FILE works, then access to the peer dies", now reproduced on **Ethernet**, which retires
the idea that it is an HDLC driver problem.

---

## 0-SOLVED. THE FAULT IS CLOSED, AND SECTION 0.0 BELOW IS WRONG. READ THIS FIRST.

**2026-08-23 evening. D100, D102 and D103 all reach each other, and they do it straight from a
cold boot with nothing typed at any of them.**

```
on D100:  X-C LIST-NAMES 102  -> D102's table: *XM-FIDO *COSPO *XM-ENNS0 *FA-FSA *TADADM *FA-SERVER
on D100:  X-C LIST-NAMES 103  -> D103's table, over HDLC
on D102:  X-C LIST-NAMES 103  -> D103's table, through D100
on D102:  X-C LIST-ROUT       -> 103  A: *->LAN->100->103      two hops, as designed
```

### What it actually was. Two ordinary configuration mistakes, not a transport defect.

 1. **A blank Remote DTE address.** `DEFINE-NETWORK-CONNECTION D102,ENNS0,,` is REFUSED; the four
    trailing zeros are mandatory. **The refusal says `Illegal system number parameter`** - it
    blames the system number when the fault is the DTE field. Typing the command bare and
    answering prompt by prompt does not help: every field is accepted individually and XROUT
    rejects the lot at the end.
 2. **D102's XMSG was not running at all.** `@XMSG` answered *"XMSG is either not generated, not
    loaded or not started"*. `@SIN` -> `START-X` -> `EXIT` fixes it.

### Section 0.0 was wrong, and how

0.0 concluded "the break is between the two Ethernet cards". It was built on a real pair of
numbers - D102's TXData moved 11196 to 11197 while D100's Rcv stayed at 0 - measured at a moment
when **nobody had checked whether the hub was running**, during the same stretch of Windows
trouble that later turned out to have killed it. A true measurement, read under an unchecked
condition, published as a cause.

**The rule worth keeping from all of this:** when a command's error names a field, and that field
is plainly correct, suspect a DIFFERENT field before you suspect the machine. An error message is
a claim made by the program, not a measurement.

### And it is all automatic now

`SYSTEM/LOAD-MODE:BATC` is identical on all three machines and calls
`(PACK-ONE:UTILITY)XMSG-STARTEX-L03:MODE`, which has the same name on every machine and different
contents - D100 Ethernet plus HDLC gateway, D102 Ethernet, D103 HDLC only. Sources and installer:
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\boot`.

One more thing that could never have worked: **D103's `RetroCore.ini` dialled `localhost:10366`,
where nothing listens.** It now dials `10362`, D100's HDLC 2.

---

### 0.0 LATER STILL - the break is BETWEEN THE TWO ETHERNET CARDS, not in ND configuration

This supersedes the "entirely on D100's outbound side" line above. The bisect below was right
about D100 refusing locally; it was wrong to stop there, because nobody had a baseline for
D102's transmit counter.

**The experiment that settled it.** `LIST-LINKS` on BOTH machines as a baseline, then
`LIST-NAMES 100` typed on D102, then `LIST-LINKS` on both again:

```
BEFORE    D102 link 1  TXData/Retry/RXBad  11196/0/0     D100 link 1   9/0/0   Rcv 0
AFTER     D102 link 1  TXData/Retry/RXBad  11197/0/0     D100 link 1   9/0/0   Rcv 0
```

**D102 DID put exactly one frame on the wire. D100 received nothing.** The frame left one
emulated Ethernet card and never arrived at the other.

So the fault sits in code we own and can debug off the machines - `SRC\Xmsg.Hub` (the hub) or
the ENNS0 / ETH emulation inside RetroCore - and NOT in any SINTRAN command, table or grant.
Every remaining "try this XMSG command" idea in section 0.5 is now low value.

### 0.0b AND THE HUB WAS NOT RUNNING AT ALL after the machines were rebooted

Checked on 2026-08-23 evening, with D100/D102/D103 freshly booted:

```powershell
Get-NetTCPConnection -LocalPort 5010     # nothing listening
Get-NetTCPConnection -RemotePort 5010    # no machine connected
Get-CimInstance Win32_Process -Filter "Name='xmsghub.exe'"   # no such process
```

`xmsghub.exe` is a plain Windows program. **A reboot of Windows kills it and nothing starts it
again.** Starting it by hand brought all three machines back in under three seconds - they
retry the dial on their own, so the machines need no restart:

```
[hub] member joined: 127.0.0.1:30858 (machine), 1 total
[hub] member joined: 127.0.0.1:30860 (machine), 2 total
[hub] member joined: 127.0.0.1:30861 (machine), 3 total
```

**NOT ESTABLISHED, and it matters:** whether the hub was also down during the measurements in
0.1 to 0.4d. Those were taken earlier the same day, when all three machines were verifiably
attached to 5010, so it probably was up - but "probably" is not a measurement. **Before
re-running any of this, check the hub is up first.** It is the cheapest check on the list and
it invalidates everything else if it fails.


### 0.1 THE DECISIVE MEASUREMENT - D100 never puts a frame on the wire

`X-C LIST-UTILIZATION` taken as a baseline, then `LIST-NAMES 102` run, then taken again.
**Not one counter moved:**

```
                         Limit  Max used  In use
Transmit Frame table...:    25       1        0      <-- unchanged
Data transmit blocks...:    10       1        0      <-- unchanged
Control transmit blocks:    15       0        0      <-- unchanged
```

So `Remote system is not accessible` is decided **locally, inside D100's kernel, before any
frame is built**. Nothing is sent, so nothing can be lost, refused or mis-delivered. Every
theory that involves the wire is dead.

**Take the baseline FIRST and re-read after - that is what made this conclusive.**

### 0.2 THE BISECT - the two directions behave completely differently

| direction | what happens |
|---|---|
| D100 -> 102 (`LIST-NAMES 102`) | **instant refusal**, `Remote system is not accessible` |
| D102 -> 100 (`LIST-NAMES 100`) | **HANGS** - prints the header and waits |

D102 genuinely tries and waits for an answer. D100 does not try at all. That matches the hop
counts exactly: D102's row for 100 has **0/1** (one LAN hop), D100's row for 102 has **0/0**
(no hop in either fabric).

**So the question to answer next is narrow: what gives D100's system-table row for 102 a LAN
hop count, and why is it zero?**

### 0.3 What was checked and is HEALTHY - do not re-check these

```
X-C LIST-NETWORK-SERVERS
Name    Sysid  Link  Network  Xnser-port-Xgate  Rcv-buff-Xmit
ENNS0    9800    1     LAN      14         16     5        2
```

ENNS0 is registered and it is a **LAN** server, which is right.

```
X-C LIST-LINKS
No  Addr. State Sysid Rcv Xmit  Lun Timeout  Soft-stat-hard TXData/Retry/RXBad
 1 152164  Run  9800    0    0   20  50/Off       0       0          9/0/0
```

The link is in state **Run**, on the ENNS0 gateway, with **9 data frames sent, 0 retries, 0 bad
receives**. The link is fine.

The kernel is healthy: 4273 free words, no table anywhere near its limit.

### 0.4 A parameter-count correction, and it did NOT fix it

`DEFINE-NETWORK-CONNECTION` takes **FOUR** parameters, read off the machine one prompt at a time:

```
Remote system?        102
Server name?          ENNS0
Server system name?   (blank)
Remote DTE address?   (blank)
```

The form used everywhere in our notes - `DEFINE-NETWORK-CONNECTION D102,ENNS0,,0,0,0,0` - has
**seven**. The surplus is apparently ignored, but note it passes **`0` as the Remote DTE
address** where the interactive form leaves it blank.

**Re-defining it with a blank DTE address answered `Ok` and changed nothing** - `LIST-NAMES 102`
still refuses instantly. So the DTE address was not the cause. Recorded so nobody tries it twice.

### 0.4b `Enable-Route-Through` TRIED - takes no parameters, answered Ok, CHANGED NOTHING

`LIST-NAMES 102` still refuses instantly afterwards. That is not surprising in hindsight -
route-through is about forwarding on behalf of OTHER systems, not about reaching a peer - but it
was the obvious candidate and it is now eliminated. **Do not try it again.**

### 0.4c THE SHARPEST NUMBER YET - the two Ethernet links have carried wildly different traffic

`X-C LIST-LINKS` on each machine, same moment:

```
D100
No  Addr. State Sysid Rcv Xmit  Lun Timeout  Soft-stat-hard TXData/Retry/RXBad
 1 152164  Run  9800    0    0   20  50/Off       0       0          9/0/0

D102
 1 152164  Run  9800    0    0    7  50/Off       0       0      11196/0/0
 2 152262 Call     0    0 SABM 1360  10/ 10       0       0       4672/8/72
```

**D100's Ethernet link has moved 9 frames. D102's has moved 11196.** Both are in state `Run` with
zero retries and zero bad receives - so neither link is broken. D100 simply is not using its one.

That agrees exactly with the LIST-UTILIZATION result: D100 is not transmitting, and the reason is
upstream of the link.

Two more things visible here:

 - **D100 received nothing from D102's attempt.** After D102's `LIST-NAMES 100` hung, D100's link
   row was still `9/0/0` with `Rcv 0`. So D102's request never arrived. Whether D102 actually put
   it on the wire is NOT established - I had no TXData baseline on D102 before the attempt.
   **Take one next time.**
 - **D102 has a SECOND link, number 2, stuck in state `Call` transmitting `SABM` for ever**, on
   lun 1360, with `4672/8/72` - 8 retries and 72 bad receives. That is the HDLC link, and it is
   the classic never-answered-SABM picture. It is almost certainly unrelated to the Ethernet
   fault, but it is a real defect sitting on that machine and nobody has written it down before.

### 0.4d A table difference nobody has explained

D100's system table has **3** entries: 100, 102, 9800. Before the reboot it had 4 - the extra was
19999, with **State 4** and hops **0/1**, i.e. a system that had actually communicated.

D102's has **6**: 100, 102, 103, 9800, 200, 19999.

Rows and their hop counts appear to be **learned from traffic, not configured** - which means the
`0/0` hop count on D100's row for 102 is a SYMPTOM of never having talked, not the cause of it.
**That retires the "give the row a hop count" framing from section 1.** The question is what stops
the first transmission, not what fills in the hops afterwards.

### 0.4e THE COMMAND SHAPES, read off the machine - and one is a trap

Typed bare, these prompt one field at a time. Do this before typing any comma-form from a note.

```
START-NETWORK-SERVER
    Server name?              ENNS0
    Server system name?       (blank)
    Window size?              (blank)
    Wide Area Network (Y/N)?  N        <-- N means NOT a WAN, i.e. Ethernet. Our form was RIGHT.

DEFINE-NETWORK-CONNECTION
    Remote system?            102
    Server name?              ENNS0
    Server system name?       (blank)  <-- TRAP
    Remote DTE address?       (blank)
```

**THE TRAP: `Server system name` DOES A LOOKUP.** Answering it `D102` made the configuration
command itself fail with `Remote system is not accessible` - the very error under investigation,
now coming from the command rather than from traffic. **Leave it blank.**

Re-running `START-NETWORK-SERVER` for a server that is already up fails with
`Illegal/Reserved Logical Unit Number (LUN) for link`. That means "already running", not
"misconfigured" - ENNS0 holds lun 20 on D100.

`ENABLE-ROUTE-THROUGH` takes no parameters. `LIST-CONNECTIONS` on D100 is **empty**, which is
consistent with nothing having talked.

`LIST-GENERATION-VARIABLES` shows nothing that would block this: 512 systems accessible, 4 links,
**20 hops allowed**, 2500-byte messages, 312-word frames.

### 0.5 Commands available that have NOT been tried yet

`X-C ?` lists these privileged commands, and several look relevant to a hop count of zero:

```
Enable-Route-Through      Disable-Route-Through
Define-Alternative-Link   Remove-Alternative-Link
Enable-Checksum           Disable-Checksum
List-Connections          List-Utilization    List-Generation-Variables
```

**`Enable-Route-Through` and `Define-Alternative-Link` are the obvious next things to look at**,
and `List-Generation-Variables` may show how this kernel was generated. None has been tried.
Ronny's 2026-08-22 note for the D103 work also said "D100 needs routing enabled" - that may be
the same switch, and it may never have survived the reboot.

### 0.6 Loose ends I left on the machines

 - **D102 terminal 8 has a `LIST-NAMES 100` still hanging inside XMSG.** It should time out by
   itself; if not, that terminal needs clearing.
 - My ESC on D102 produced `USER BREAK AT 124211B` - I interrupted whatever was running on that
   terminal. Nothing appeared to be harmed, but it is worth knowing it happened.
 - D100 now has the 102 network connection defined twice over (the second with a blank DTE
   address). Both said `Ok`.

---

## 1. Where to pick up - the single next step

**D100 cannot reach D102 through XROUT.** Measured, repeatably:

```
X-C: LIST-NAMES 102
System   Port  Free SPs   Name
Error in communicating with XROUT.
XMSG Kernel error: Remote system is not accessible
```

Because of that the chat trunk never comes up - `LIST-TRUNKS` on D100 stays `102 unknown`
even though `START-TRUNK 102` answered `trunk added`.

**The last thing I was doing when the session ended**, and where to resume: looking at the
Ethernet hub's connections. That produced a NEW and unexpected measurement, see section 5.

**The concrete asymmetry to pull on** (both from `X-C LIST-SYSTEMS`):

| where | row for | Link | Sequence send/recv | Access | Hops WAN/LAN |
|---|---|---|---|---|---|
| on D100 | 102 | 152164 | 0 / 0 | `*----F` | **0/0** |
| on D102 | 100 | 152164 | **1306 / 1305** | `*----F` | **0/1** |

D102 believes it reaches D100 in one LAN hop. D100 believes there is no hop to D102 in either
fabric. D102 also still holds sequence 1306/1305 for a machine that has just rebooted back to
0/0.

**NOT ESTABLISHED:** whether that sequence drift is the cause, a symptom, or a separate second
problem. Do not assume it. See `[[flags1-law-per-sender-per-peer]]` and
`[[nd-publishes-the-flags1-counter-in-list-systems]]` before touching it, and
`[[wake-a-dead-system-entry-with-list-names.md]]` - which was tried and did NOT help here.

---

## 2. What is running right now

| process | what it is |
|---|---|
| `F:\RC\RonnyTest\HDLC1\RetroCore.exe` | **D100** - terminal port 9010 |
| `F:\RC\RonnyTest\HDLC2\RetroCore.exe` | **D102** - terminal port 9102 |
| `F:\RC\RonnyTest\HDLC3\RetroCore.exe` | **D103** - terminal port 9003 |
| `F:\RC\RonnyTest\TCP-M\RetroCore.exe` | TCP-M - terminal port 9210 |
| `xmsghub.exe --port 5010` | the Ethernet hub, from `SRC\Xmsg.Hub` |
| `RetroTerm.Desktop.exe` | the terminal backend the retroterm MCP drives |

D100 was **rebooted** this session with Ronny's explicit permission ("if needed fucking reboot
d100"), because a desynchronised RT-LOADER was holding a SINTRAN terminal and no fresh session
would prompt any more. See section 6 - that was my fault, and the tool is now fixed.

---

## 3. D100 after the reboot - what was brought back, and how it was verified

Each line is the command and the answer the machine actually gave.

```
@RT ENNS0                                          (no error)
@XMSG
X-C: START-NETWORK-SERVER ENNS0,,,N                Ok
X-C: DEFINE-NETWORK-CONNECTION D102,ENNS0,,0,0,0,0 Ok
X-C: DEFINE-FRIEND-SYSTEM 102                      Ok
X-C: EXIT
@RT FSART
@FS-ADMINISTRATOR
FSA: SELECT-FSA,,,,                                Connection established
FSA: START-SERVER 1,,,,                            Server 2 started.  No of FACs attached: 30
FSA: EXIT
tools\rt-load.ps1 -Port 9010 -Segment 2575 -AndStart
                                                   CHATSER is ALIVE - in the time queue
@CHAT-MON
C-M: STATUS                                        SEATS 0/16  default 50  disk 0KB
C-M: START-TRUNK 102                               trunk added
C-M: LIST-TRUNKS                                   102 unknown        <-- THE FAULT
```

`SEATS 0/16  default 50  disk 0KB` matters: the `default 50` and `disk` fields only exist in the
CURRENT build, so D100 is running the newest CHATSV with the history work in it.

**A REBOOT RESTORES THE `.ini` AND NOTHING ELSE.** The Ethernet card comes back because it is in
`RetroCore.ini` line 49 (`device add ETH 0 --net=tcp:127.0.0.1:5010`) and the ini ends with
`boot bd0`. Everything in the block above was typed at SINTRAN and none of it survived.

---

## 4. A real find on D102, independent of the reboot

**D102 had system 100 as access `*----P`, never `*----F`.** It had NEVER been granted friend.
Fixed this session with `X-C: DEFINE-FRIEND-SYSTEM 100` on D102, which answered `Ok`.

**D102 IS RUNNING AN OLDER BUILD.** Two tells, both measured:

 - its CHAT-MON answers `LIST-TRUNKS` with `known, not built yet` - it predates the trunk verbs;
 - its `STATUS` prints only `SEATS 0/16`, without the `default` and `disk` fields.

Its chat server does run and answer. But once the transport carries again, **D102 needs a
rebuild** - and the cheap way is on D100 itself, no Windows process in the path:

```
@COPY-FILE D102(SYSTEM).CHATSV:PLNC,CHATSV:PLNC
```

(no quotes - the file already exists there; quotes mean create. The wildcard is `*`, not `%`.)

This means the "D102 runs the whole product" note in memory is now out of date and has been
corrected.

---

## 5. NEW, and not yet followed up - THREE machines are on the Ethernet hub

The very last measurement of the session, and it was a surprise:

```
local port  process
63871       F:\RC\RonnyTest\HDLC1\RetroCore.exe    D100
31278       F:\RC\RonnyTest\HDLC2\RetroCore.exe    D102
31277       F:\RC\RonnyTest\HDLC3\RetroCore.exe    D103
```

**D103 holds a live connection to the Ethernet hub.** Task #110 has been proceeding on the basis
that D103 is an HDLC-only machine reached via D100 (which is what Ronny said on 2026-08-22, and
that is still what its SINTRAN is configured for). But at the emulator level its Ethernet card is
attached to the same hub as the other two.

**NOT ESTABLISHED:** whether D103's SINTRAN has ENNS0 running or any network connection defined -
I never got to look. If it does, or can, then D103 may be reachable over Ethernet directly and
#110's HDLC-via-D100 plan may not be needed at all. **Check `F:\RC\RonnyTest\HDLC3\RetroCore.ini`
for its `device add ETH 0` line first, then ask D103 itself.**

This also matters for the fault in section 1: three machines share one hub, so a stale or
mis-delivered frame has more places to go than the two-machine picture assumed.

---

## 6. What I broke, and the fix that is already committed

`rt-load.ps1` drove the RT-LOADER on a fixed 1200 ms settle. It ran ahead of the loader:

```
*LOAD LOAD PLANC-1BANK-F00,2575,
NO SUCH FILE NAME
PARAMETER NO.  1 IS ILLEGAL
*END-LOAD
NEGLECTING REFERENCES? CHANCH75
NEGLECTING REFERENCES? 2575
```

MON-CALL never loaded, and `END-LOAD` ate `EXIT-LOADER`, `RT CHATSER` and `LOGOUT` as answers to
its own question. The loader kept SINTRAN terminal 10, and after that no session on D100 would
prompt at all - hence the reboot.

**Fixed in `01cf14ec`.** `ndterm.ps1` gained `-StepWaits`: one expected prompt per step, parallel
to `-Steps`, `|`-separated for alternatives. A step that never sees its prompt STOPS the run and
names itself. `rt-load.ps1` now supplies the loader's real prompts, **read off D100 one at a
time, not remembered**:

```
RT-PROGRAM:  PRIORITY:  SEGMENT ONE:  SEGMENT TWO:  START ADDRESS:  RING:
INITIAL PAGE TABLE:  ALTERNATIVE PAGE TABLE:
```

then back to the loader's own `*`. Waiting for `*` after `END-LOAD` also turns unresolved
references into a clean stop instead of a mess. Proved: CHATSER loaded first try afterwards.

**I also left five stale retroterm sessions open earlier**, which used up D100's terminal pool and
made the situation look worse than it was. Close sessions when done.

---

## 7. Committed this session

| commit | what |
|---|---|
| `aa3c0768` | Two C# chat servers federate: trunk, forward, age out. **95 tests pass.** |
| `9bff7d05` | Protocol registry: mark the eight kinds the machines have actually proved |
| `01cf14ec` | Wait for the loader prompt instead of betting on a delay |

### `aa3c0768` in detail - the C# side of #84

`ChatServer` now handles the trunk kinds:

 - **`HandleTrunkHello`** - the first text byte is the direction, `0` asks and `1` answers. An
   answer is never answered; without that byte two servers greet each other for ever.
 - **`HandleTrunkSaid`** - the speaker is qualified with `@Dnnn` built from the address the letter
   ARRIVED from, never from anything in the letter, so a peer cannot put words in a third
   machine's mouth. Split at the FIRST slash. A room nobody here is in is dropped, not conjured.
 - **`ForwardOverTrunks`** - only what a LOCAL member said, only to peers believed Up. A line that
   arrived on a trunk is delivered and STOPS. Two machines is complete; three needs a hop count
   and an origin this protocol does not have.
 - Public surface: `StartTrunk`, `TickTrunks`, `Trunks`, `Magic`.

`SRC\Xmsg.Chat.Tests\ChatFederationTests.cs` runs **two real `XmsgKernel`s** wired to each other
through `IXmsgDatagramSink` and `XmsgKernel.Deliver`. Nothing about the chat server or the message
format is faked - only the wire between the machines is a method call.

**Still open on #84:** the C# server can only trunk to another C# server. Trunking to CHATSV.PLNC
on a real ND needs the node/transport layer under the kernel - envelope, link sequencing,
Ethernet or HDLC - which is a separate piece of work.

---

## 8. The task list, as it stands

| # | state |
|---|---|
| 76 | LOW PRI - retest LAPB against a long-running D100. Do not block chat work for it. |
| 84 | C# side DONE off-machine, 95 tests. Real-ND trunking needs a transport layer. |
| 104 | ROOT-CAUSE the XXHER crash - HDLC driver error raised inside link teardown. |
| 108 | No-wait accepted on LDN 38 with status 0 and **MON1 still blocks**. Unexplained. |
| 110 | D103 - **re-scope it against section 5 before doing anything.** |
| 111 | D100 recovered; XROUT cannot reach 102. **This is the live one.** |
| 112 | History follow-ups: INITIALIZE per room from CHAT-MON, latest-id for unread counts. |
| 113 | ROOT-CAUSE the "one transfer must be refused before the grant takes" rule and fix it. |

---

## 8b. UNCOMMITTED WORK SITTING IN THE TREE - not mine, not lost, not committed

Five LAPB files are modified and uncommitted, last written **2026-08-22 00:16-00:17**:

```
SRC/Xmsg.Live/LapbLayer.cs                 +56
SRC/Xmsg.Live/LapbOptions.cs               +37
SRC/Xmsg.Live/Seam/LapbLayerAdapter.cs     +25
SRC/Xmsg.Live.Tests/LapbTimerTests.cs      +54
SRC/Xmsg.Live.Tests/LapbConformanceVectorsTests.cs  +9
```

174 insertions in all. That is the previous session's work, and it looks like task #75 (LAPB
gated on both halves of the handshake), which is marked completed but was never committed.

**I deliberately did NOT commit it.** I did not write it, have not built it, and committing
someone else's work unverified would put an unbuilt state in the history under a message I could
not honestly write. **Ronny: build it and commit it, or tell me to.** It is safe on disk.

## 9. Reminders that cost time this session

 - **A fixed delay is not a wait, it is a bet.** If a script drives an interactive program, it
   waits on that program's own output.
 - **Close your terminal sessions.** Five stale ones exhausted D100's terminal pool.
 - **Read the RT description before believing a symptom.** CHAT-MON's `unexpected kind` looked
   like the known one-byte-offset bug; `LIST-RT-DESCRIPTION CHATSER` said **PASSIVE**, and the
   server was not running at all. The RT description corrected the diagnosis.
 - **The hub is a hub.** Both machines saying `--net=tcp:127.0.0.1:5010` is CORRECT. Do not
   "fix" one to `listen:5010` on the strength of the comment in the ini.
 - Leave no .NET host of mine running: `dotnet build-server shutdown`, then check
   `Win32_Process.CommandLine` for anything under `XMSG`.
