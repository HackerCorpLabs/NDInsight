# Plan: capture the COSMOS file-server messages (2026-07-28)

> **PARTLY DONE the same day.** Step 2 (local capture) succeeded for both servers: see
> [XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md](XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md),
> [XMSG-XFTRA-CONTROLLED-VARIATION-2026-07-28.md](XMSG-XFTRA-CONTROLLED-VARIATION-2026-07-28.md)
> and [XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md](XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md).
> Step 1's conformance scans are still PENDING and cannot run yet - nothing inter-node has
> been captured, because no peer ever answered. Current state:
> [XMSG-HANDOFF-2026-07-28.md](XMSG-HANDOFF-2026-07-28.md).
>
> One assumption below was also disproved: `*XFTRA`'s purpose is no longer inferred, and the
> two servers turned out NOT to share a convention.

Next session's target: get real bytes out of `*XFTRA` (file transfer) and the `*FA-*` family (file
access), so their protocols stop being "inferred from the name" and start being decoded.

This is the last big unknown in XMSG. The transport underneath them is finished and verified, so
whatever we capture should differ from TAD **only** in the application layer.

---

## 1. What we know going in

| Name | Seats | What we actually know |
|---|---|---|
| `*XFTRA` | 1 | File transfer. Registers with `XSCRS` + one `XSNSP` (captured). Purpose INFERRED from the name. |
| `*FA-FSA` | 2 | File access, the administrator side. Registration captured. |
| `*FA-FSA-I` | - | Registers alongside `*FA-FSA`; role unknown. |
| `*FA-SERVER` | 30 | File access server, 30 service points. Registration captured. |
| `*XM-FIDO` | - | Described in our notes as the transport these ride on. UNVERIFIED. |

**Not one byte of their working traffic has ever been captured.** Everything above is either
registration (which we have) or inference (which we should stop repeating).

Two useful things carved from the image today:

- `XFTRAD` is an **RT program** loaded on segment 2, declared with priority 30, and it reads a
  *"Remote batch logical device"* from address `61465` (`1241` = batch 1) plus a timeout at
  `61641`. So file transfer is plumbed to a **batch** device, not to an interactive terminal.
  Source: `(UTILITY)COS-XFTRA-E02:MODE`.
- The file-access side starts with `FSART`, then `FS-ADMINISTRATOR` -> `SELECT-FSA` ->
  `START-SERVER 1`. That is already driven by the existing harness test.
  Source: `(UTILITY)COS-FA-SERV-E04:MODE`.

---

## 2. The two capture methods, and which question each answers

This distinction has already cost time once. Get it right first.

| Method | Shows | Cannot show |
|---|---|---|
| **MON 200 trace** (RetroCore, Device level) | Every buffer a task hands the LOCAL kernel or XROUT: letters, requests, replies, magic numbers | Anything that only exists between two machines |
| **pcap over HDLC** | Real inter-node traffic, both directions | Anything local - registration, name lookup, a task talking to a server on its own machine |

For file transfer both matter: the **request** is local (a program asks `*XFTRA` to do something), the
**transfer** is between systems.

---

## 3. Order of work

### Step 1: the conformance scans, before anything else

Run the two scans from `LEARNING-A-NEW-PROTOCOL.md` on whatever we capture:

1. envelope scan - Counter and Channel against the formulas, expect **0 mismatches**
2. ACK scan - `S_ack = seed + 0x0B`

If both pass, only the application layer is new and every transport rule carries over. If either
fails, stop and find out why before decoding a single trailer byte.

### Step 2: local capture first - it is cheap and needs one machine

Extend the existing harness. Everything up to and including the products starting is already
written and works:

```
Boot_Login_StartXmsg_StartCosmos_ListServers   (RT XFTRAD, COSPO, FS-ADMINISTRATOR)
```

Then drive a file operation and read the `XFWRI` buffers out of the Device log. What to look for,
in order:

- an `XSLET` letter naming `*XFTRA` or `*FA-SERVER` - the shape we already know how to parse
- the **XMCSM class words** used after the letter: each new `Flags2 = XMCSM >> 16` is a new lane
- the trailer vocabulary: is it a TAD-style `[opcode][count][data]` chain, or something else?

### The command is documented - and the program can dump its own buffers

`ND-30.025.02 COSMOS Operator Guide` section 6.2 documents it. `TRANSFER-FILE` is an ordinary
program any user can run:

```
@TRANSFER-FILE
COSMOS File-Transfer (version B) of 1983.11.11 11:00
Hello SYSTEM
F-T:
```

Its full command set, from `LIST-ALL-COMMANDS` in the manual:

| Command | Arguments |
|---|---|
| `SET-DEFAULT-REMOTE-SYSTEM` | system name, user name, password |
| `TRANSFER-FILE` / `TRANSFER` | To, From |
| `CHECKOUT` | remote system and user name, number of page transfers |
| `APPEND-REMOTE-BATCH` | batch system and user name, input file, output file |
| `DEFINE-TRANSFER-CONDITIONS` | number of buffers, size in bytes, secure messages |
| `LIST-NAMES` | system name or number |
| **`DEBUGPRINT-ON` / `DEBUGPRINT-OFF`** | - |
| **`DECODE-BUFFER`** | input buffer (y/n) |
| `GET-ERROR-MESSAGE` | error value |
| `LIST-VARIABLES`, `GET-DEFAULT-REMOTE-SYSTEM`, `MODE`, `HELP`, `EXIT` | - |

### TRIED IT ALREADY - here is where it stands [2026-07-27, late]

Two probe runs went in before the tree stopped compiling. Findings, so nobody repeats them:

**1. The image has version E02, not the manual's version B.**

```
@TRANSFER-FILE
COSMOS File Transfer - Version E02 - 1987.10.07 09:38
Hello SYSTEM
F-T:
```

> **WRONG - corrected 2026-07-29.** These commands DO exist on E02. They are gated behind
> `SET-ADVANCED-MODE`, which changes the prompt to `F-T(Adv.):`. All of them were run successfully
> against `(COSMOS-BASIC)COS-FILE-TRA-E02:PROG` on node 100, including `LIST-VARIABLES`, which
> confirmed the transfer buffer size and count. See
> `XMSG-LIST-FILES-ON-THE-WIRE-2026-07-29.md` section 6b. The paragraph below stands only as a record
> of the mistake - do not act on it.

The command set SHRANK between versions. On E02 these all answer `** Illegal command **`:
`DEBUGPRINT-ON`, `DEBUGPRINT-OFF`, `DECODE-BUFFER`, `CHECKOUT`, `LIST-ALL-COMMANDS`,
`LIST-VARIABLES`, `DEFINE-TRANSFER-CONDITIONS`. **So there is no built-in buffer dump to lean on** -
scratch the plan above that hoped for one. The MON 200 trace is the instrument.

What DOES work, with its prompts:

| Command | Prompts |
|---|---|
| `SET-DEFAULT-REMOTE-SYSTEM` | "Remote system name?", "Remote user name?", "Password?" |
| `TRANSFER-FILE` | "To?", "From?" |

**2. The file-name syntax, which is where both attempts died.** From the program's own `?` help,
quoted in ND-60.163.4 COSMOS User Guide:

```
SYSTEM(REMOTE-USER(PASSWORD)).(DIRECTORY:USER)FILENAME:TYPE
```

The separator between access information and file name is a **dot**, not a colon, and a file that
does not exist yet must be **quoted** so SINTRAN creates it - `MINOR(JONES)."MONTHLY-MEMO"` is the
manual's own example. Get either wrong and it fails LOCALLY, before anything reaches the network:

| What was typed | What happened |
|---|---|
| `D102:(SYSTEM)XMSG-COPY:BATC` | `ILLEGAL CHARACTER IN PARAMETER` - the colon |
| `XMSG-COPY:BATC` | `NO SUCH FILE NAME` - unquoted new file |
| `D102(SYSTEM)."XMSG-COPY:BATC"` | **queued, not yet run** - the build broke first |

The harness test is written and waiting: `Boot_Login_StartCosmos_ProbeFileTransfer` in
`Nd100SintranEthernetIIBootHarnessTests.cs`. **Start here in the morning** - one run should either
produce the outgoing request or a new error that says why.

**3. Remote file ACCESS needs something we have never loaded.** The ~30 SINTRAN commands that work
on remote files require the File User on the LOCAL machine: `COS-FA-USER-1:BPUN` on segment 22 and
`COS-FA-USER-2:BPUN` on segment 26, loaded by `(UTILITY)COS-FA-USER-LOAD:MODE` (Operator Guide
page 78). Our harness has never run that. Note also the recorded version gate: the File User needs
revision F or later, and this image may only carry E - so `TRANSFER-FILE` is the more promising
route of the two.

### Step 3: two systems, for the transfer itself

A transfer needs a remote system. Two ways, in order of cost:

1. **Our C# node as the passive peer.** Bring up the seam runner as node 102, let the ND-100 open
   the conversation, and log every frame even if we cannot answer yet. The opening letters and the
   first exchanges are the valuable part, and this needs no second emulator.
2. **Two RetroCore machines over HDLC**, wired through `topology.json`. More faithful, more setup,
   and both ends must have COSMOS started.

Start with option 1. Even a conversation that stalls after three frames tells us the request shape.

### Step 4: write it up the way the others were

Per `LEARNING-A-NEW-PROTOCOL.md`: name census, class-word map, trailer vocabulary, session shape
(accept -> port assign -> session, like TAD; or letter-only; or request/response). Tag every claim
VERIFIED / INFERRED / UNKNOWN with capture references. Then extend the dissector and re-verify.

---

## 3a. Blocker to clear first thing

`E:\Dev\Repos\Ronny\RetroCore` did not compile when this was written - `NDBusEthernetIIHle.cs`
references `OP_POST_RX_BUFFER`, `OP_ENABLE_RX_POOL`, `CMPL_POOL_OK`, `OP_SET_STATION_ADDR`,
`OP_SET_MODE` and `OP_READ_STATS`, none of which exist yet. That is in-flight Ethernet HLE work, not
XMSG, and it was left alone. The probe test cannot run until it builds.

## 4. Traps, all of which have already bitten

- **The harness crashes intermittently** - host process gone, no exception, no dump, different point
  each time. Retry before concluding anything about the command you just typed.
- **Count the prompts.** SINTRAN programs ask one question at a time. Answer too few and the next
  command is swallowed as an argument, which looks exactly like the command failing. This silently
  invalidated two experiments in the last session.
- **`pcap-decode-report.txt` frame numbers are per-direction, not chronological.** Never read it as
  a timeline; re-extract with `frame.time_relative` and merge both directions.
- **Do not reach for a pcap to answer a local question** - and do not reach for a MON 200 trace to
  answer an inter-node one.
- **Registry port numbers move between boots.** Identify servers by name in the capture, never by a
  port number remembered from a previous run.

---

## 5. What "done" looks like

- A new doc `XMSG-FILE-SERVER-*-CAPTURED-<date>.md` with the request shape, the class words, and the
  trailer vocabulary, tagged by evidence.
- `XMSG-PROTOCOL.md` section 7 updated so `*XFTRA` and `*FA-*` are no longer described as inferred.
- The Lua dissector extended, verified with `tshark` at zero new expert warnings on known-good
  captures.
- If the shape turns out to be request/response, a typed client on `Xmsg.Api` the way `Xmsg.Chat`
  sits on it today.

## 6. Why this matters beyond curiosity

The remote-execution console needs file transfer. Today the honest recommendation is to write our
own agent, because the existing servers are undecoded. If this capture succeeds, using the machine's
own file transfer becomes an option instead - which is a far better answer for real 1980s
interoperability.

See [XMSG-HANDOFF-2026-07-27.md](XMSG-HANDOFF-2026-07-27.md) for the state of everything else.
