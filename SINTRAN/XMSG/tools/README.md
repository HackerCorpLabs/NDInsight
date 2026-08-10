# XMSG live-machine tools

Scripts for driving a running SINTRAN machine headless over its RetroCore terminal port.

| Script | What it does |
| --- | --- |
| `lab-status.ps1` | **Start here.** Read-only snapshot of the whole lab: which machines are up, how each HDLC line is wired right now, whether our relay is running, and which .NET hosts are ours versus another repo's. |
| `ndterm.ps1` | Drives one SINTRAN terminal session: ESC, login, commands one prompt at a time, logout. |
| `restart-xmsg-cosmos.ps1` | Restarts XMSG and COSMOS and puts the machine back on the Ethernet segment. |

> **Ports and LUs live in [`../lab-topology.json`](../lab-topology.json), explained in
> [`../LAB.md`](../LAB.md).** `lab-status.ps1` reads that file rather than carrying its own copy.
> The sequences written out further down this page were captured on 2026-08-04 against a DIFFERENT
> HDLC wiring (note `START-LINK,1362`) - check LAB.md for the current controller-to-port-to-LU
> mapping before copying a command from here.

## Reading the lab

```powershell
.\lab-status.ps1
```

It calls out two states that LOOK healthy and are not:

 - **a self-connected HDLC** - a machine whose dial target was down can complete a TCP connection to
   ITSELF; on the machine `LIST-LINKS` then shows `State Run` with `Sysid` equal to its OWN system
   number, and only a restart clears it.
 - **an orphan dialler** - a machine dialling a port nobody listens on, which reads at the SINTRAN
   end as a dead link with no explanation.

It also groups identical .NET hosts and tags them `XMSG` or `FOREIGN`, because a `dotnet test` in
another repo spawns ~19 indistinguishable MSBuild workers and killing one of those is the mistake
the listing exists to prevent.

---

## Timing traps, learned the hard way on 2026-08-04

Both of these cost hours and both made the ND look broken when the fault was on this side.

**Never guess a delay - wait for the prompt.** `ndterm` used fixed delays for the login. D100 was
busy just after an XMSG restart, its banner arrived late, and the user name went out before the
`ENTER` prompt existed. Every step after that was one out of phase - the password answered
`ENTER`, the first command answered `PASSWORD:` - so the session never left the login loop and all
13 X-C commands burned their full 120s timeout. **28 minutes, no configuration applied.** It now
waits for `ENTER` (or `@`, if the line is already logged in), then for `PASSWORD:`.

**Use `-WaitFor` for anything slow.** `START-NET-SERVER` prints `wait 10 sec!`. A short fixed
delay sends the next command INTO the busy one and garbles both.

**Redirect the runner's stdout.** `Xmsg.Live.Runner` writes heavily to the console. Started hidden
with no reader, the buffer fills and every write blocks about a second - so a connect confirm that
should take 200 ms took 23 seconds and D100 gave up. Always:

```powershell
Start-Process .\Xmsg.Live.Runner.exe -ArgumentList "--config","topology-d103-hdlc.json" `
    -WindowStyle Hidden -RedirectStandardOutput runner-stdout.txt -RedirectStandardError runner-stderr.txt
```

**Stop the runner before building.** It locks the DLLs, and a build that fails to copy them leaves
you testing the OLD code while the test suite proves the new one. `dotnet test` does NOT rebuild
the runner - no test project references it.

**STOP THE RUNNER BEFORE RESTARTING D100 - the order matters.** Flags 1 is each side's own count
of the Data frames it has sent, and the two sides must agree on where they are. If the runner is
left running while `restart-xmsg-cosmos.ps1` executes, D100 exchanges a dozen or so datagrams with
it while COSMOS comes up, and both sides end up part-way through a sequence instead of at zero.

Worse, `xmsg-sequence.state` is the RECORD of what D100 has acknowledged from us - **do not delete
it to "start clean"** unless D100 is genuinely restarting with no traffic from us. Deleting it
after a restart that our runner took part in sets us to 0 while D100 sits at 0x000F, and the
conversation then dies silently after the connect confirmation.

The order that works, every time:

```
1. stop the runner
2. .\restart-xmsg-cosmos.ps1 -Port 9010      (no traffic from us while it runs)
3. delete xmsg-sequence.state                 (only now is starting at 0 correct)
4. start the runner
5. run the test
```

A run whose first inbound frame carries a Flags 1 other than a small number is a run where step 1
was skipped. Check it before believing anything the run tells you.

**A failed attempt fouls D100.** After a file-access failure the machine answers
`CALL NOT VALID IN CURRENT STATE` or stops answering at all, whatever you send. Back-to-back runs
are therefore NOT independent samples. Restart, test, restart, test - and only believe a failure
that reproduces on a freshly restarted machine.

---

## Restarting XMSG and COSMOS

```powershell
.\restart-xmsg-cosmos.ps1 -Port 9010                     # D100
.\restart-xmsg-cosmos.ps1 -Port 9102 -LocalSystem D102   # D102, same sequence
```

### When you need it

The tell for a half-crashed XMSG kernel is a **contradiction between two commands**:

```
@X-COMM              -> XMSG Kernel error: XMSG is either not generated, not loaded or not started
@SINTRAN  START-XMSG -> ERROR: XMSG is already running
```

The RT side is up and the user interface cannot reach it. That single fault produces three
symptoms that look unrelated:

- `FILE-ACCESS NOT RUNNING OR CRASHED; ALL CONNECTIONS ABORTED` (SINTRAN error 324 octal)
- `LIST-ROUTE` answered with `** Command not recognised **` inside X-C
- nothing at all leaving the machine on the network

### CHECK THIS FIRST when the HDLC link will not come up

A link that never leaves `Starting` - we send SABM, D100 never answers UA - usually means **XMSG
itself is down**, not that the link needs cycling. Run `X-C` and look:

```
XMSG Kernel error: XMSG is either not generated, not loaded or not started
*- XMSG error code: -45          <- on EVERY command
```

That is the half-crashed state. `STOP-X` will still say `OK: XMSG terminated`, because the RT side
is up and only the interface is gone. Fix it with `STOP-X` / wait / `START-X`, then redefine the
remote names (they are cleared by the restart) and `START-LINK,1362,,,-1,,`. The link comes up in
tens of milliseconds once XMSG is really running.

Cost of not checking this on 2026-08-04: an hour spent on the link and the emulator while the
kernel underneath was simply not running.

Two command details worth knowing:

 - **There is no `STOP-LINK`.** X-C answers `** System name STOP-LINK is not known **`.
 - **`LIST-LINK` takes no link number** - it prompts `XROUT system?`. A command typed after it gets
   swallowed answering that prompt, which makes the NEXT command look rejected.

### The sequence

Ronny's, dictated 2026-08-04 and verified on D100 the same morning.

```
@SIN                                  ; SINTRAN service program, prompt is *
   STOP-X                             ; stop first even if XMSG looks dead
   EXIT
                                      ; wait 10 seconds
@SIN
   START-X
   EXIT
@START-TAD
@TADA                                 ; TADADM - prints the TAD table, returns to @
@SET-AVAIL
@X-C                                  ; XMSG command program, prompt is X-C:
   DEF-REMOTE,,D100 100
   DEF-REMOTE,,D101 101
   DEF-REMOTE,,D102 102
   DEF-REMOTE,,D103 103
   DEF-REMOTE,,D200 200                 ; Ethernet controller emulator, added 2026-08-09
   DEF-REMOTE,,D19999 19999
   START-NET-SERVER,ENNS0,,,N         ; takes ~10 seconds, says so
   DEF-NETWORK-CONN D102 ENNS0,,0,0,0,0
   DEF-NETWORK-CONN D200 ENNS0,,0,0,0,0
   DEF-NETWORK-CONN D19999 ENNS0,,0,0,0,0
   START-LINK,1362,,,-1,,
   ENABLE-ROUTE-THROUGH
   EXIT
@MODE (PACK-ONE:COSMOS-BASIC)COS-START-E04:MODE,,
@SET-AVAIL
```

### Four things that will bite you

**1. `LOAD-MODE:MODE` is NOT a substitute.** The machine's own boot file gets XMSG up but has no
`START-NET-SERVER` and no `DEF-NETWORK-CONN` lines, so the machine comes back without its Ethernet
segment configured.

**2. An XMSG restart CLEARS every remote name, D19999 included.** Redefine them or nothing
resolves - a remote file access will simply report that the system is unknown.

**3. `COS-START-E04:MODE` ends with `@SET-UNAVAILABLE`.** `LOAD-MODE` is what normally follows it
with `@SET-AVAILABLE`. Run the two in the **same terminal session**: `SET-UNAVAILABLE` blocks NEW
logins, so a session already open can still issue `SET-AVAIL`. Split them across two connections
and you lock yourself out - TERM 5/6/7 have no port, TERM 8/9/10 are the pool behind the TCP port,
and terminal 1 is the RetroCore window. The only way back is typing `@SET-AVAILABLE` on the GUI
console.

**4. Do not use fixed delays for the X-C block.** `START-NET-SERVER` prints
`Server not yet started - will try to start him now (wait 10 sec!)`. A delay that is too short
sends the next command into the busy one and garbles both - on the first attempt this produced a
truncated `DEF-NETWORK-CONN` and an `*- Illegal parameter(s) -*` that looked like a syntax error
but was pure timing. `ndterm.ps1 -WaitFor 'X-C:'` waits for the prompt instead of guessing.

### Output you should expect and ignore

- Printer definitions failing for `ND-969`, `ND-1068`, `ND-5005` - those printer systems do not
  exist on this network.
- `File already exists, but it does not belong to COSMOS-SPOOLING`.
- `** You cannot make this network definition for the local defined system **` if you leave the
  machine's own name in `-NetworkSystems`.

The line that says it worked:

```
Server 1 started.     No of FACs attached: 30
```

---

## Driving a terminal directly

```powershell
.\ndterm.ps1 -Port 9010 -User SYSTEM -Steps "LIST-FILES","D19999(sys).",""
.\ndterm.ps1 -Port 9010 -User SYSTEM -Steps "X-C","LIST-ROUTE","EXIT" -WaitFor 'X-C:'
```

Rules the hard way:

- **ESC first.** A fresh connection shows only the RetroCore banner; ESC produces the SINTRAN
  banner and the `ENTER` prompt. ESC also recovers a wedged line.
- **One connection per interaction.** Reconnecting while a program is running wedges the terminal.
- **Commands prompt field by field.** Send the command, then answer each prompt - do not pass
  comma-separated arguments at the `@` prompt. A blank step sends a bare CR, which accepts a
  default (`OUTPUT FILE:` -> terminal).
- **Quoting a filename means CREATE it.** Quoting one that exists gives `FILE ALREADY EXISTS`.
- **A remote filespec in `COPY-FILE` needs a dot:** `d102(system).patch-file:out`.
