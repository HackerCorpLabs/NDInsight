# Plan: capture the COSMOS file-server messages (2026-07-28)

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

**Open question to resolve first:** what command actually starts a transfer? The image holds
`COS-FILE-TRA-E02:PROG` and `COS-FILE-TRANS:PROG`, but the user-facing command name is not
established. Find it by listing `(SYSTEM)` programs on the running machine, or by reading the
remaining `COS-*:MODE` files - they are plain text once bit 7 is stripped (they are stored with
parity set; that is why they look like garbage in a hex dump).

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
