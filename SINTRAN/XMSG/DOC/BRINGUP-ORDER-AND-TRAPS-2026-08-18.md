# Bringing D100 back up: the order that matters, and two traps that cost a line

Measured 2026-08-18 while clearing a stuck XMSG name. Written down because the restart script does
not survive every situation and the manual sequence is then the fallback.

## The file server needs TAD, and says so obscurely

`FS-ADMINISTRATOR` -> `SELECT-FSA` -> `START-SERVER 1,,,,` answers:

```
Terminal access not running or unknown port name
```

That is not about a port name. **TAD is not running.** The bring-up order is fixed:

1. stop XMSG        `SIN` -> `STOP-X` -> `EXIT`
2. start XMSG       `SIN` -> `START-X` -> `EXIT`
3. **TAD**          `START-TAD`, `TADA`, `SET-AVAIL`
4. XMSG config      `X-COMM` -> `DEF-REMOTE,,<name> <number>` per system, `START-LINK,1362,,,-1,,`,
                    `ENABLE-ROUTE-THROUGH`, `EXIT`
5. COSMOS           `MODE (PACK-ONE:COSMOS-BASIC)COS-START-E04:MODE,,` then `SET-AVAILABLE`
6. friends          `X-COMM` -> `DEFINE-FRIEND-SYSTEM <number>` per system, `EXIT`
7. file server      `ABORT FSART`, `RT FSART`, then `FS-ADMINISTRATOR` -> `SELECT-FSA,,,,` ->
                    `START-SERVER 1,,,,` -> expect `No of FACs attached: 30`

**Skip 3 and step 7 cannot work**, however many times it is retried. COSMOS's own start MODE runs
an FSA start internally and fails the same way for the same reason, which makes it look as though
COSMOS is broken when only TAD is missing.

**Friends go after COSMOS** - it wipes them - and `DEF-REMOTE` clears the friend flag of the system
it names, so 6 must follow both 4 and 5.

## Trap 1: do not run the restart script while holding a terminal line

`tools/restart-xmsg-cosmos.ps1` opens its own session. There are only three lines on port 9010 -
`TERM 8`, `9` and `10` in `RetroCore.ini` - so a session left open by a person or an earlier run
starves it. Twice the script logged in, printed `===> [SIN]`, and exited without reaching step 3;
the machine was then half configured, XMSG up and COSMOS down, which reads exactly like a crash.

**Close every terminal session before running it**, and check it reached `=== 6.` in its log rather
than assuming it finished because the process exited.

## Trap 1b: the COSMOS start MODE ends with SET-UNAVAILABLE - it locks every login out

`(PACK-ONE:COSMOS-BASIC)COS-START-E04:MODE` finishes with `@SET-UNAVAILABLE`. After it runs, **every
terminal answers `SYSTEM UNAVAILABLE` instead of `ENTER`** - including SYSTEM. Transfers, XMSG and
the file server all keep working perfectly, so nothing looks wrong until somebody tries to log in.

**Run `@SET-AVAILABLE` immediately after the mode file, from the line you already hold.** Measured
2026-08-18: the mode was run from the only logged-in line, that line was then used to start CHATSV,
and there was no way back in - three lines, all refusing logins, one holding a blocked program.

**A remote batch job does NOT get you back in.** `APPEND-REMOTE-BATCH` submits fine and D100 answers
the letter, but the job's `@ENTER` is a login like any other and the output file came back blank.

## Trap 1c: ESC does NOT reach a program suspended in XMPFRCV

A task waiting on `xmpfrcv` with `XFWTF` is suspended inside the monitor call. **ESC sets the break
and nothing happens** - not on the first press, not on ten. The break lands only when a message
arrives and the call returns. Measured twice today, both directions:

 - to STOP a chat server you must first make it receive something (join a client, or let a member's
   port die so a bounce arrives), and only then does the armed ESC take effect;
 - so a server on a terminal line can pin that line indefinitely if nothing can reach it.

**This is the strongest argument for running CHATSV as an RT program** (task 62): an RT program owns
no terminal, so it cannot take one hostage.

## THE BEST WAKE: LIST-NAMES pointed at the REMOTE system

**Try this before the COPY-FILE cure.** Two commands, no file transfer, and it works when the
transfer cure cannot:

```
@X-COMM
X-C: LIST-NAMES
XROUT system? 19999          <- the REMOTE number, not blank
```

D100 must ask node 19999 for its name list, so it ORIGINATES - which is the one thing a dead entry
needs. MEASURED 2026-08-18 at our seam server:

```
[RX] 100->19999 sub=ReachabilityRequest proto=144 f1=0xFFFF
[RX] 100->19999 sub=Data proto=143 ... 040F 0100 0145 0004 01020000   <- the XROUT name query
```

The next push worked first time, 64767 bytes. Our seam server has to be running so there is
something at the far end, but **it does not matter that we never answer the query properly - the
entry is woken by the exchange, not by its success.**

**Why it beats COPY-FILE:** the transfer cure needs a working FA CLIENT on D100, and that client can
be stuck (next section). `LIST-NAMES` sidesteps the FA layer entirely.

## Trap 1d: the wake COPY-FILE can be blocked by a stuck FA CLIENT

The cure for a dead system entry is one `COPY-FILE` from D100 to us (see the section below). That
cure has its own failure mode, met on 2026-08-18 after the second XMSG restart of the day:

```
@COPY-FILE "WOKE:TXT",D19999(SYSTEM)."WAKE:TXT"
""WAKE:TXT""
FILE-ACCESS PROTOCOL ERROR; CONNECTION ABORTED          <- first attempt
FILE-ACCESS INTERNAL ERROR; CALL NOT VALID IN CURRENT STATE   <- every attempt after
```

Our seam file server was up and saw **no FA frames at all** - only LAPB SABM/UA/RR - so D100 never
got as far as sending one. `LIST-SYSTEMS` showed `19999 State 0`, sequence `0 0`, access `*----F`:
the entry is dead and the friend flag is fine, so this is not access and not sequence.

**The COSMOS start mode's `ABORT FSART` / `RT FSART` restarts the file SERVER, and this is the
CLIENT.** They are not the same thing, and a fresh `COS-START-E04:MODE` did not clear it.

**NOT ESTABLISHED:** what does clear it. Candidates for next time, cheapest first - a different
native client (`TRANSFER-FILE` rather than `COPY-FILE`), or whatever RT program owns `*FA-USER`.
Recorded rather than guessed at, because two wrong guesses at the FA server earlier in the week
each destroyed it.

## Trap 2: a failed START-SERVER can take the line with it

`START-SERVER` on a machine without TAD prints `--- please wait ---`, then the error, and then the
line stops accepting input entirely - ESC included. The session is gone and the line with it until
the emulator releases it. With three lines and no permission to restart the emulator, that is a
third of the available access spent on one bad command.

So: **get TAD up before touching FS-ADMINISTRATOR at all.**

## SINTRAN LOGS AN IDLE TERMINAL OUT, and it takes your programs with it

**MEASURED 2026-08-18** on the line that had been running the chat server:

```
IF NO ACTIVITY ON THE TERMINAL, YOU WILL BE LOGGED OUT IN 1 MINUTE
 *** ABORTED BY SYSTEM ***
 05.38.15     18 AUGUST   1998
--EXIT--
```

Both terminals went this way within a few minutes of each other while nothing was being typed.

**This is not the same thing as `STOP-TERMINAL`** and it needs no operator action - it happens on
its own. A background program on that line goes with the session, which is a fourth reason to run
anything long-lived as an RT program (see `PLAN-CHATSV-AS-RT-PROGRAM.md`).

**It is NOT proven to be what kills XMSG.** On the screen above, `CHATSV` reported
`receive refused st= 16933` - XMSG already gone - BEFORE the logout notice appeared, so here the
logout is a consequence of the server exiting and leaving the line idle, not the cause. The timing
is suggestive and the test is cheap: restart XMSG, keep the starting terminal busy, and see whether
it outlives the usual half hour.

## XMSG DIES ABOUT HALF AN HOUR AFTER IT IS STARTED, whatever you do

**Five deaths on 2026-08-18.** Two of them are timed against a known start:

| XMSG started | Dead by | Lived | What was happening |
|---|---|---|---|
| ~16:52 | ~17:15 | ~23 min | a push and a long PLANC compile |
| 15:37:29 | 16:07:20 | **29 min 51 s** | COSMOS, a chat run, then mostly idle |

The other three fit the same span. The workloads have nothing in common - one was a 20-client chat
burst, one was a compile, and in the last one `LIST-NAMES` answered normally a minute or two before
`STOP-LINK` answered `-45`, with nothing running at all in between.

**So it is a LIFETIME, not a load.** Every explanation tried against the workload has failed:

 - not the chat burst - death 3 had no chat at all, and death 5 came while idle;
 - not our bad device write - that code has been gone since death 1;
 - not kernel free space - it sits at 4273 words and does not move, even through a whole COSMOS
   start;
 - not the tables - the only one that ever fills is `Data transmit blocks`, and that empties again.

**PLAN THE SESSION AROUND IT.** Roughly twenty-five minutes of working XMSG per restart, and a
restart costs the stop/start, TAD, COSMOS, the DEF-REMOTEs, the link and a wake. Do the thing that
needs the machine FIRST, and leave reading, writing and committing for after it dies.

**NOT ESTABLISHED: what the timer is.** The X-C banner lists `Watchdog` among the build options,
which is the obvious suspect and has not been looked at. The errors print on the machine's CONSOLE,
which on this emulator is a window we cannot read from a terminal session - so the next real step is
watching that window across a death, or finding whether the watchdog can be queried or disabled.

## LIST-UTILIZATION: the baseline, and the resource that actually runs out

**Take a baseline before any experiment**, because every number below is only meaningful against
one. A freshly started XMSG, nothing else running:

```
Free space in Kernel...:  4273 words.
Task table:  3 in use    Port table:  4    Message table: 1    Name table: 1    System table: 1
everything else: 0
```

**`Free space in Kernel` is NOT where a leak would show.** It stayed at 4273 unchanged through an
entire COSMOS start - TAD, XROUT, the file server, the lot.

**The scarcest resource is DATA TRANSMIT BLOCKS, and there are ten of them.** After a 20-user chat
burst:

```
                         Limit  Max used  In use
Message table..........:   256        34        4
Receive Frame table....:    20         5        5
Transmit Frame table...:    25        16        0
Data transmit blocks...:    10       10*        0     <- the asterisk means it hit the LIMIT
Control transmit blocks:    15         6        0
```

**The asterisk is the machine telling you that table reached its ceiling.** Nothing else is close.
Ten blocks is not many when twenty joins arrive at once, and this is very likely the same event a
server reports as `XMXEMFL`, "message space full", from `XMPFGET`.

So when something fails under load, read this table and look for the asterisk before theorising.

## How to tell what state the machine is in, in one command

```
@X-COMM
X-C: LIST-NAMES
```

 - only `*XM-FIDO`               XMSG is up, COSMOS has not run
 - `D100.` and the `*XROUT` set  COSMOS has run
 - `*FA-SERVER` with free seats  the file server is up and transfers will work
 - `*FA-SERVER` missing          step 7 has not run, or TAD was missing when it did

A push into a machine without `*FA-SERVER` fails as `answered none of 4 connect letters` - the LAPB
link comes up perfectly, which makes it look like a protocol fault rather than a missing server.


---

# After the bring-up, the transport still will not start - and why

Everything above can be green and a push still fails with *"answered none of 4 connect letters"*.
Both halves of that were measured on 2026-08-18 after a manual bring-up.

## Symptom, and how to read it

```
X-C: LIST-LINKS     1 152164  Run 19999 ... 0/0/15      <- link is UP, but RXBad is climbing
X-C: LIST-SYSTEMS   19999   State 0 ... Send-receive 0 0 <- the SYSTEM is dead
```

**RXBad counts frames D100 threw away.** The link being `Run` says only that LAPB is up. The system
entry is the XMSG-level relationship, and `State 0` means there is not one.

## Cause 1: our sequence store is ahead of a restarted peer

An XMSG restart zeroes the peer's counters. Our store still holds where we left off, so we open at
`Flags1 0x09A8`, D100 expects to start again, and every frame is out of sequence and discarded.

The log says it plainly if you look:

```
[seq] node 100: link opened from the REMEMBERED seed 0x5B, starting Flags1 0x09A8 from the store
```

**Re-base that ONE node** - `100=0000` in `xmsg-sequence.state`, leaving every other node alone.
Never delete the file; the other peers have not restarted and their counters are still correct.

**There is still no runner option for this.** `--resync-hard` is tied to `--announce-restart`, and
that flag is refused - see below - so the store is edited by hand. This is the second time it has
blocked a session.

## Cause 2: the system entry only wakes when the PEER addresses us

Re-basing the sequence is necessary and not sufficient. `State 0` stays `0` however many times we
originate, because D100 will not create the relationship on our say-so.

**`--announce-restart` does not fix it, even on a freshly restarted peer.** Tested, and refused
exactly as it is on a long-running one:

```
[push] <- node 100 REFUSED us: XRDDF (3) - XDTYP=0x0017 XDSCR=0xFFFD
```

That is worth knowing: the flag was suspected of being harmful only when the peer already held our
name. It is refused after a full XMSG restart too, with the name table empty. **The rule stands
without qualification: do not pass it.**

**What works is letting D100 address US first.** Run our node with a file server on the HDLC seam
and read a file from it on the machine:

```
@COPY-FILE "WOKE:TXT",D19999(SYSTEM)."WAKE:TXT"
```

One command, and the entry comes up:

```
19999   State 4 ... Send-receive 18 18 ... Hops 0/1
```

After that our own pushes and pulls work normally, and the store learns the peer's number by itself
(`100=0012`). The whole cure is one `COPY-FILE`.
