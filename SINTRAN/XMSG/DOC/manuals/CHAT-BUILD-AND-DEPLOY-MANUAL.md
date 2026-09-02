# NDCHAT - Build and Deploy Manual

For a maintainer with this repository, the Windows tool chain in `tools\`, and the three lab
machines described in [LAB.md](../../LAB.md). It is the loop that turns an edited `.PLNC` into a
running server and client on all three machines, with a proof at every step - because every step
here has been skipped while the screen looked fine, and each time it produced a green build of
the wrong thing.

**Checked on 2026-09-02** by running it for the client (`DBGUI=0` rebuild). Steps marked
**UNVERIFIED** have not been run as written.

Related: [admin manual](CHAT-ADMIN-MANUAL.md) (what the pieces are, RT-load traps, boot files),
[user manual](CHAT-USER-MANUAL.md), [CHAT-PLAN.md](../CHAT-PLAN.md) (outstanding work only).

---

## 1. The shape of it

```
   edit .PLNC on Windows
        |  planc-lint.py                    seconds, catches most compile errors here
        v
   stage into sync-relay\                   the sync daemon carries it to D100 over XMSG
        |  FILE-STATISTICS byte count       proof the whole file landed
        v
   @MODE CHATCC:MODE,,  on D100 ONLY        the compile; ~4 minutes for the client
        |  planc-build.ps1 -PullOnly        THE GATE: pulls the listing, exits 1 on *** ERROR
        v
   COPY-FILE the binaries to D102, D103     D102/D103 have NO PLANC - never compile there
        |
        v
   rt-load.ps1 (server)  /  re-dump NDCHAT (client on D102)
        |
        v
   every joined client restarts             an RT-load orphans them all
```

**The only machine with a compiler is D100.** `PLANC-100-F00` and `BRF-LINKER-C01` are
installed there and nowhere else. D102 and D103 receive binaries.

### The sources and what they build

| Source | Built by | Produces | What it is |
|---|---|---|---|
| `SINTRAN-CHAT\CHAT.PLNC` + `CHATARR.PLNC` | `CHATCC:MODE` | `CHAT:PROG`, `CHAT:LIST`, `CHATARR:LIST` | the client. Fully linked: VTM (`VTMR`, `VTMDATA`, `VTMARR`, `INTRF1B`), `CHATLIB`, `XMP-100-1-B02`, `MON-CALL-1B-A00`, `PLANC-1BANK-F00` all inside |
| `SINTRAN-CHAT\CHATSV.PLNC` | `CHATSV:MODE` | `CHATSV:BRF`, `CHATSV:LIST` (and a `CHATSV:PROG` nobody runs) | the server. RT-loaded from the BRF with `CHATLIB` and the three ND libraries |
| `SINTRAN-CHAT\CHATLIB.PLNC` | its own MODE | `CHATLIB:BRF` | the message codec both sides IMPORT. Changing it means rebuilding **both** |
| `SINTRAN-CHAT\CHATMON.PLNC` | `CHATMON:MODE` | `CHAT-MON:PROG` | the operator program. **Carried as `CHATMN:PLNC`** - the transfer's file-name budget is 13 characters and `CHATMON:PLNC` with its create-quotes is 14 |
| `CHATCTST.PLNC`, `CHATKT.PLNC`, `CHATVT.PLNC` | their own MODE files | test programs | offline checks - section 7 |

The names on the machine are short for a real reason: our file transfer opens a file with a
15-byte QFORM string holding the name, an apostrophe and an access letter, and a CREATE adds its
own quotes. 13 characters of name is the limit. `CHAT-CC:MODE` is refused; `CHATCC:MODE` fits.

---

## 2. Once per session: the sync daemon

The daemon (`Xmsg.Live.Runner` as node D19999) holds **one** XMSG link to D100 open and carries
any file dropped in its sync folder. It is the only way to move a file to the machine that does
not tear the link down afterwards; a one-shot push ends with a `DISC`, and that teardown killed
XMSG fourteen times out of fourteen.

```powershell
cd E:\Dev\Ronny\NDInsight\SINTRAN\XMSG
.\tools\lab-status.ps1                       # hub up? machines up? is a daemon already running?
.\tools\start-relay.ps1 -WindowSeconds 14400 # four hours; refuses to start a second one
```

What it printed on 2026-09-02:

```
running    PID 60052
           [runner] topology loaded: self=19999, 4 node(s), file=...\topology-d19999.json
           watching E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\sync-relay -> node 100 (SYSTEM)
```

**The folder it watches is the folder in its own command line** - `sync-relay` as started here.
Staging into any other folder is silent: the file sits there, nothing carries it, the machine
compiles its OLD copy, and the build is green. `nd-deploy.ps1` still drops into `sync-out`, which
is one of those folders - do not use it until that is fixed (section 9).

**"the link has not learned the peer yet"** - a daemon started while D100 has nothing to say to
node 19999 can wait for ever:

```
[sync] 1 transfer(s) waiting and none started: the link has not learned the peer yet
```

Make the ND address us. On D100:

```
@COPY-FILE D19999(SYSTEM).WAKE:TXT,CHAT:CNFG
```

That copy FAILS after some minutes with `NO ANSWER FROM REMOTE SYSTEM` and it still does the job:
the daemon learns the peer within seconds and the queued transfer starts. Expect one round of
sequence catch-up afterwards (XENSE steps in the log); it converges by itself.

The daemon ends itself when its window runs out. Never hard-kill it while it is talking to a
machine - it skips the `DISC` and leaves D100 holding a half-open link.

---

## 3. Stamp, lint, stage, verify

**Bump the build stamp first.** `serverVersion` in `CHATSV.PLNC` and `clientVersion` in
`CHAT.PLNC` are hand-set eight-character constants (`S02-1340`: letter, day, hour-minute).
`STATUS` prints the server's, `LIST-MEMBER` prints each client's. They read `S31-0854` /
`B31-0731` across four rebuilds before anyone noticed, which made "check the stamp changed
after a load" worthless. A build that ships gets a new stamp.

```powershell
python tools\planc-lint.py SINTRAN-CHAT\CHAT.PLNC SINTRAN-CHAT\CHATARR.PLNC SINTRAN-CHAT\CHATLIB.PLNC
python tools\planc-lint.py SINTRAN-CHAT\CHATSV.PLNC SINTRAN-CHAT\CHATLIB.PLNC
```

One call per **link set**. The client and the server both have an `inBuf`; fed to the linter
together they draw a seven-character export collision that is real only for modules that link
together, which these two never do.

`python`, not `python3` - `python3` does not exist on this machine. The linter self-tests on every
run and refuses to run if a check is broken. It catches, in a second, the things the compiler
lets through or reports as a bracket problem: undeclared names, `'ALn'` widths that do not match
the string, ten-character name collisions (seven across a BRF), hand-counted lengths beside a
literal, a routine EXPORTed but defined nowhere, an IMPORT whose signature drifted from its
EXPORT. Every build failure becomes a linter check - that is the standing rule.

Give it all the modules of a link set together and it checks the EXPORT/IMPORT interfaces across
them, which neither the compiler nor the linker does.

Stage:

```powershell
Copy-Item SINTRAN-CHAT\CHAT.PLNC sync-relay\CHAT.PLNC -Force
Get-Content sync-relay.log -Tail 5
```

wait for

```
[sync] overwrite D100(SYSTEM).CHAT:PLNC on D100 <- ...\sync-relay\CHAT.PLNC done, 294009 byte(s)
```

and then prove it on the machine. `FILE-STATISTICS CHAT:PLNC` matches **every** file whose name
starts with `CHAT` (eleven of them on D100), and `;1` does not narrow it - the one you want is
the first block and scrolls off a 24-line screen. Read it from the scrollback (`terminal_readnew`
in retroterm, or `ndterm.ps1`, which captures the whole output):

```
@FILE-STATISTICS CHAT:PLNC,,
FILE 112 : (PACK-ONE:SYSTEM)CHAT:PLNC;1
           (INDEXED FILE)
           ...
           OPENED FOR WRITE 07.36.09  SEPTEMBER 2, 1998
           148 PAGES , 294009 BYTES IN FILE
FILE 113 : (PACK-ONE:SYSTEM)CHATSV:PLNC;1
           ...
```
*(captured on D100, 2026-09-02, straight after the push above)*

Same number as the file on Windows, or do not build. A stalled transfer leaves the old content
with a new date, and that old content compiles clean.

`tools\nd-verify.ps1` does this comparison for every source at once.

---

## 4. Compile on D100

```
@MODE CHATCC:MODE,,
```

The MODE file deletes the old outputs, compiles `CHAT:PLNC` and `CHATARR:PLNC` to BRF, and links
`CHAT:PROG` with `BRF-LINKER-C01`. The client takes about four minutes; the compiler prints
`LINE: nnnn` as it goes. The server is `@MODE CHATSV:MODE,,`, CHAT-MON is `@MODE CHATMON:MODE,,`.

**The screen is not the evidence.** The compiler's diagnostics scroll off a 24-line screen, and
the `0 DIAGNOSTICS` still visible at the end belongs to the LINKER, which sits happily under a
compile that failed.

Three rules the MODE files enforce and a hand-typed build usually does not: the source must be
CRLF (bare LF gives `LINE IS TOO LONG` on every line); every included file must end with `$EOF`
(without it the compiler stops at the include and reports 0 diagnostics for the part it never
read); and the XMSG library is linked in a separate step, because nothing pulls it in on its own.

---

## 5. The gate

```powershell
.\tools\planc-build.ps1 -PullOnly -Listing CHAT:LIST
.\tools\planc-build.ps1 -PullOnly -Listing CHATARR:LIST
```

It asks the running daemon to pull the listing into `listings\`, then reads it: exits 1 on any
real `*** ERROR`, and checks the compile reached the last line of the source. **Gate every
module of the link set**, not just the one you edited - a test suite has passed 139/139 on top of
ten `*** ERROR` lines in a module nobody looked at.

`tools\nd-listing-check.py <listing> <source line count>` is the judge on its own.

---

## 6. Distribute and load

### 6.1 Binaries to D102 and D103

From a terminal on the receiving machine, with COSMOS up:

```
@COPY-FILE CHAT:PROG,D100(SYSTEM).CHAT:PROG
@COPY-FILE CHATSV:BRF,D100(SYSTEM).CHATSV:BRF
@COPY-FILE CHATLIB:BRF,D100(SYSTEM).CHATLIB:BRF
@COPY-FILE CHAT-MON:PROG,D100(SYSTEM).CHAT-MON:PROG
```

Destination first. No quotes when the file already exists. Then `FILE-STATISTICS <name>;1,,` on
both machines and compare the byte counts - a stalled copy is silent.

One caution measured more than once: after one `COPY-FILE` to a peer works, the next access to
that peer can die (`ONE COPY-FILE WORKS, THEN ACCESS DIES`). It reproduces with SINTRAN's own
`COPY-FILE`; if the second copy hangs, let it fail, wait, and try again.

### 6.2 The server: RT-load on a FREE segment

```powershell
.\tools\rt-load.ps1 -Port 9010 -Segment 2529 -AndStart -ShowSteps   # print what it would type
.\tools\rt-load.ps1 -Port 9010 -Segment 2529 -AndStart               # do it
```

Ports: 9010 = D100, 9102 = D102, 9003 = D103. `-AndStart` starts `CHATSER` and then puts the
machine's name and its trunks back through CHAT-MON. The script checks the loader's answers and
refuses to go on past `PARAMETER NO. 1 IS ILLEGAL`, `NEGLECTING REFERENCES`, `NO SUCH FILE NAME`
or a description change refused because the server was still running.

It needs a **free terminal line** on the machine: check `terminal_list` (or which RetroTerm
sessions you hold) first. D102 and D103 keep a chat client on terminal slot 8; hold that slot
with one connection and the next gets slot 9.

**Pick a new segment number every time.** The rule and the reason are in the admin manual,
section 3.3.

Then check the build stamp changed:

```
C-M: STATUS
SEATS 0/16  build S31-0854 ...
```

### 6.3 The client on D102: dump the reentrant subsystem again

D102 runs the client as reentrant `NDCHAT`, which shadows `CHAT:PROG`. After copying a new
`CHAT:PROG` there, dump it again or the old one keeps running. `DUMP-PROGRAM-REENTRANT` prints
nothing on success.

### 6.4 Restart every joined client

An RT-load gives the server an empty seat table. Every client that was joined is orphaned and
shows a normal screen. `/exit` and start `CHAT` again on every terminal.

---

## 7. Offline tests - no build cycle needed

Three PLANC test programs run on D100 in seconds and print a PASS/FAIL line per check:

| Program | Checks | What it proves |
|---|---|---|
| `CHATCTST` | 23 | the client's room cache, the paint arithmetic and the wire-arrival parse |
| `CHATKT` | 49 | the XMSG kernel send/receive path by loopback on a local port - do not re-suspect it |
| `CHATVT` | 48 | the server's own `buildFromSlot`/`broadcast` against synthetic members, decoded with the library's own readers |

Build and run each with its MODE file (`CHATKT:MODE` etc.). They exist because reading the
source to work out what a program does costs a build's worth of time every two rounds and is
wrong more often; a number on the screen ends the argument.

The C# side: `tools\run-all-tests.ps1` builds and runs every test project and counts a project
that produced no result as a failure. Always `-nodeReuse:false` on any `dotnet build`/`test`,
and finish with `dotnet build-server shutdown`.

**The 256-file table.** SINTRAN's file table for user SYSTEM fills up with scratch listings and
BRFs from these test builds and answers `ATTEMPT TO CREATE TOO MANY FILES` - which stops a build
mid-way. Delete old `CHATL*`, `CHATA*`, `TMP*` listings and the test BRFs when it happens.

---

## 8. The install and source floppies

**UNVERIFIED - `tools\make-floppies.ps1` is the next thing to write; this section is its
contract.** It will:

1. pull the four binaries off D100 through the daemon's pull folder (`sync-pull`), or fail;
2. `ndtool --create floppy12 --name NDCHAT dist\NDCHAT-INSTALL-<date>.img`, then `--put` each
   binary, `CHATRT:MODE`, the boot-block example and `README:TEXT`, and `--chmod PUBLIC+R` on
   every file - `--put` leaves a file PUBLIC=NONE and SYSTEM then gets `NOT READ ACCESS`;
3. `ndtool --create floppy12 --name NDCHATSRC dist\NDCHAT-SOURCE-<date>.img` with every
   `.PLNC`, `.MODE`, `.INCL` and `SCREEN.SYMB` from `SINTRAN-CHAT\`, converted to CRLF;
4. write `dist\NDCHAT-<date>.manifest.txt` listing every file and its byte count, and
   `ndtool -t` both images back to prove the manifest.

`ndtool.exe` is at `E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build\ndtool.exe` (v0.0.6; the older
`build_win` path no longer exists). A 1.2 MB floppy is 512 pages; the sources are about 900 KB
of text today, so the source floppy is close to full and the script must fail loudly when a file
does not fit rather than silently leaving it out.

---

## 9. Known drift in the tools (do not trust these until fixed)

| Tool | Problem |
|---|---|
| `tools\nd-deploy.ps1` | stages into `SRC\Xmsg.Live.Runner\sync-out`, but `start-relay.ps1` starts the daemon on `sync-relay`. Its fallback path **stops every running RetroCore** to write the disk image - never let it reach that branch |
| `tools\planc-build.ps1` (push mode) | `-WatchDir` defaults to `sync-out` for the same reason; it checks the live daemon's command line and refuses when they differ. `-PullOnly` is unaffected |
| `tools\README.md` | its X-C sequence is from an older HDLC wiring (`START-LINK,1362`) - read LAB.md for the current one |

---

## 10. The traps, in one place

- A green screen is not a build - read the listing (section 5).
- A file that arrived short compiles as its old content - compare byte counts (section 3).
- Staging into a folder the daemon does not watch is silent (section 2).
- Compile on D100 only; D102/D103 have no PLANC.
- `NEW-SEGMENT` needs a FREE segment; the old number starts the OLD code.
- An RT-load orphans every joined client; an XMSG restart needs an RT-load again.
- `START-TRUNK` on an `up` trunk knocks it down for a minute.
- SINTRAN's idle timeout ends the program and logs the terminal out while the rendered screen
  still looks normal - check for `--EXIT--` after any gap.
- `FILE-STATISTICS NAME` matches by prefix and `;1` does not narrow it; read the block you
  want out of the scrollback.
- Do not ask the daemon to pull a file a MODE file is about to delete and rewrite - the pull
  is refused with SINTRAN error 110 and looks like a transport fault.
- PLANC checks no array bounds, accepts an undeclared name, and links a local initialiser error
  into a running program with the flag unset. Lint first.
