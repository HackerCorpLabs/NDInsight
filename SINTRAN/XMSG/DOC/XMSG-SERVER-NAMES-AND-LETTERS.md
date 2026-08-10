# How named servers work in XMSG (`*TADADM` and friends)

Why a name like `*TADADM` exists at all, what happens on the wire when a client uses
one, and what a `*SERVER` component in our library has to implement.

Primary source: **ND-60.164.3 EN COSMOS Programmer Guide** - section 1.2.3 (port),
section 1.3 (XROUT), appendix B sections 2 and 3.1-3.5 (the naming and letter
services), chapter 4 (the request-response model). Wire-level confirmation comes
from the HDLC captures. Every claim below is tagged VERIFIED (manual text or
capture bytes) or INFERRED.

---

## 1. The problem names solve

A task addresses another task's port by its 32-bit **magic number** (MAGNO). But a
magic number is *allocated by the kernel when the port is opened* and contains a
random part (see `XMSG-MAGIC-NUMBER-LAYOUT-CARVED-2026-07-26.md`), so it is
different every run. A client that wants to reach a server therefore cannot know
the server's address in advance, and there is no directory it can query for it:

> "XROUT will never give you somebody else's magic number." - section 1.3 (VERIFIED)

That single sentence is the whole design. XROUT is deliberately **not** a name
resolver for addresses. It is a **letterbox**.

---

## 2. The letter mechanism

The manual's own analogy is directory enquiries that refuses to give out the
number, but will deliver your letter to the subscriber - who may then choose to
ring you back (section 1.3, VERIFIED).

Concretely:

1. The server opens a port and **gives it a name** (`XSNAM`), or creates a
   **connection port** (`XSCRS`).
2. The client sends a **letter** (`XSLET`) to XROUT, addressed by *name*. The letter
   carries the client's own data - typically an identification.
3. XROUT looks the name up and **forwards the whole message** to the matching magic
   number, using the forward option so the sender information is preserved.
4. The server receives the letter. Because the message arrived, the server can ask
   the kernel for the **sender's** magic number with `XFMST` (appendix A section
   3.2.17, VERIFIED) - that is how the server learns where to reply.
5. The server replies. If it replies **directly**, the client learns the server's
   magic number and a direct dialogue begins. If the server replies with the
   **forward option** instead, it answers without disclosing its own address
   (appendix B section 3.4, VERIFIED).

So the asymmetry is deliberate and runs one way: **the letter tells the server who
you are; nothing tells you who the server is until the server decides to say so.**
That is also why a server can demand credentials in the letter before answering -
the manual explicitly suggests user name and password in the letter body, so the
server can vet the caller "before replying and thereby giving the caller his/her
magic number" (appendix B section 3.4, VERIFIED).

### The one exception

`XSGMG` "get magic number from name" *does* return another task's magic number - and
is **privileged** (appendix B section 3.10, VERIFIED). Ordinary tasks cannot use it.
Unprivileged code that only needs to know *where* a name lives, not how to address
it, uses `XSGIN` instead, which returns the system number and (for a port name) the
port number, but never the magic (appendix B section 3.11, VERIFIED).

---

## 3. Two kinds of named port

| | `XSNAM` - named port | `XSCRS` - connection port |
|---|---|---|
| Name uniqueness | must be unique | several ports may share one name, unless the uniqueness parameter is non-zero |
| Admission control | none - every letter is forwarded | a **free-connection counter**; a letter is forwarded only while the count is above zero, and forwarding decrements it |
| When the count runs out | n/a | XROUT tries **another port with the same name**; if none can take it, the letter is returned to the sender with an error |
| Releasing capacity | n/a | `XSNSP` adjusts the counter: positive to give connections back, negative to withdraw them |

(All VERIFIED, appendix B sections 3.1-3.3.)

That table is the whole reason `XSCRS` exists: it is how one logical service name is
spread across a **pool of server ports**, with XROUT doing the load distribution.

**How the count is actually built [VERIFIED 2026-07-27]** - not the way this section
originally described it. A server does *not* register with the count set to N. Every
captured server registers with **zero** (`*XFTRA` omits the parameter entirely) and then
issues **one `XSNSP` of +1 per service point**: 1 for `*XFTRA`, 2 for `*FA-FSA`, 30 for
`*FA-SERVER`, matching what the operator then sees. The counter is a running total the
server maintains, spent by XROUT one letter at a time and topped up as sessions end. Bytes
and method: [XMSG-XSCRS-CONNECTION-PORTS-CAPTURED-2026-07-27.md](XMSG-XSCRS-CONNECTION-PORTS-CAPTURED-2026-07-27.md).

### Name lifetime

A name is removed when the port closes - the kernel clears it as part of closing
(appendix A section 3.1.2, VERIFIED), and XROUT also clears names by itself when it
notices a port has closed (appendix B section 3.9, VERIFIED). `XSCNM` exists to
retire a name early. This is why a server name reappears with a **different magic
number** after a restart, and why the wire port word for `*TADADM` differs between
two boots while its port number stays 2 (VERIFIED in the captures - port words 342
and 358, both `>> 7 == 2`).

---

## 4. The naming convention

> "By convention, all names of ND standard products and of ND standard systems will
> start by `**xx`, where xx are specific to the product." - appendix A section
> 3.2.11 (VERIFIED)

Hence the leading asterisk on every name we observe. The maximum name length is set
when XMSG is generated (default 32 bytes) and **XROUT silently truncates anything
longer** (appendix B section 3.1, VERIFIED) - a quiet failure mode worth guarding
against in our own code.

The same six names, observed on two independent systems:

| Name | COSMOS machine | BIGDISK0-L | Free SPs | What it is |
|---|---:|---:|---:|---|
| `*TADADM` | 2 | **4** | - | terminal access (TAD) - the one we have fully decoded |
| `*XM-FIDO` | 4 | **3** | - | file transfer |
| `*COSPO` | 5 | **6** | - | COSMOS spooling |
| `*FA-FSA` | 7 | 7 | 2 | remote file access, FSA control side |
| `*XFTRA` | 8 | **5** | 1 | file transfer |
| `*FA-SERVER` | 11 | 11 | 30 | remote file access, bulk file server |
| `*XM-ENNS0` | - | 4 | - | Ethernet network server (only when ENNS0 is started) |

### These port numbers are NOT well-known [VERIFIED 2026-07-26]

Booting the BIGDISK0-L image in the emulator, starting XMSG, `START-TADADM` and the
COSMOS products, then running `list-serv` reproduces the whole registry:

```
System   Port  Free SPs   Name
   100     3             *XM-FIDO.
   100     4             *TADADM.
   100     5       1     *XFTRA.
   100     6             *COSPO.
   100     7       2     *FA-FSA.
   100    11      30     *FA-SERVER.
```

Four of the six ports differ from the COSMOS machine while every name matches. Port 4
is even a different server between two runs of the SAME image - `*XM-ENNS0` when the
Ethernet server is brought up, `*TADADM` when the COSMOS module is. So the number
cannot be a property of the name; it is the kernel port-table index of whatever port
the server opened, and it moves with what is loaded and in what order.

`*FA-FSA` (7) and `*FA-SERVER` (11) DO land on the same numbers on both systems. That
is what a fixed allocation order inside one module looks like - the file-access module
opens its ports as a batch - and is not evidence that those two are well-known either.

**Capacity, unlike the port, is stable.** Every connection port reports the same free
count on both systems: `*XFTRA` 1, `*FA-FSA` 2, `*FA-SERVER` 30. That count is the
`XSCRS` maximum the server registered with, so it belongs to the server rather than to
the boot.

**Consequence:** never map a name to a port number. Address a server by sending a
letter to its NAME - which is the entire reason the naming mechanism exists. Only
`*TADADM`'s port 2 has been confirmed against captured wire traffic, and even that is
only true for the boot that produced those captures.

### Reproducing this

The bring-up is automated in RetroCore as
`Emulated.Tests/ND100/Nd100SintranEthernetIIBootHarnessTests.cs`, test
`Boot_Login_StartXmsg_StartCosmos_ListServers`. Two things it has to work around, both
worth knowing for a real machine:

- **Define a local machine name first.** Without one, `COSPO` aborts with "Cannot get
  local machine name". The harness issues `DEF-REMOTE,,D100 100` before starting it.
- **`COS-START-E04:MODE` cannot run to completion on this disk.** It calls
  `COS-DEF-PRIN-E:MODE` and `COS-FA-SERV-E:MODE` by UNVERSIONED name while the image
  carries only the `E02`/`E04` files, so the batch job dies at "NO SUCH FILE NAME"
  before remote file access is ever started. The harness starts the products
  individually instead.

---

## 5. What this looks like on the wire

The transport carries all of it as ordinary XMSG datagrams; nothing about naming is
special at the frame level.

- **Everything arrives at port 0.** Port 0 is the XROUT well-known sink - it is the
  only address a client knows a priori. The receiver forks on the XMCSM low byte:
  `0x41` = `XSLET` (a letter, so parse a name out of it), `0x4B` = `XSGSY` (a direct
  routing service). (VERIFIED, `XMSG-PROTOCOL.md` sections 7.1 and 9.1.)
- **The letter body carries the name** as `FF <len> 2A <name>` - the `2A` being the
  asterisk. Decoded properly, the captured connect letter is a textbook `XSLET`:

  ```
  trailer: FF 07 2A 54 41 44 41 44 4D 00 FE 04 44 31 30 32
           .. .. *  T  A  D  A  D  M  .  .. .. D  1  0  2

  param #1 (string) = "*TADADM"   XSLET In:1  port / connection name
  param #2 (string) = "D102"      XSLET In:2  system name
  XMDPT = 0 (the XROUT sink)      XMCSM low byte 0x41 = XSLET
  ```

  So the client addresses the server **by name only** - the port number 2 appears
  nowhere in the letter. There are 18 such letters in the corpus, all `*TADADM`.
  (VERIFIED.)
- **There is NO four-byte XROUT header on the wire.** The manual's
  serial/service/length trio (appendix B section 2) describes the message BUFFER; in
  an XMSG data frame the parameter blocks start at the first trailer byte and the
  service is in XMCSM instead. Reading a header here mistakes the leading `FF 07`
  (string parameter 1, length 7) for serial 255 plus service 7 and loses every
  parameter - which is exactly what our decoder used to do. The XSGSY reply confirms
  it independently: header-free it yields the manual's four OUT parameters
  (system 100, connection type 4 = Local, extra 100, network 0), while assuming a
  header swallows parameter 1. (VERIFIED; see `XroutMessageFraming` in the library.)
- **The reply carries an address.** For TAD, the server's port-assign message
  (`7CORS`, opcode 0x07) ships `00 00 <system16> <portword16>` - which is exactly a
  32-bit MAGNO in its A/D layout, and those two halves then appear verbatim as the
  XMSSY/XMSPT fields of every following frame. (VERIFIED - see
  `XMSG-WIRE-PORT-IS-MAGIC-LOW-WORD-2026-07-26.md`.)

So the abstract "the server tells the client its magic number" step is, on the wire,
literally a magic number in the payload.

---

## 6. What a `*SERVER` component must implement

Minimum viable named server, in the order the manual puts it:

1. **Open a port** (`XFOPN`). Unnamed at first; it is the naming call that publishes it.
2. **Register the name** - `XSNAM` for a single-client service, `XSCRS` with a
   connection count for a multi-client one. Both are sent **from the port being
   named**, which is how XROUT knows which magic number the name maps to. This is
   the step whose absence is the known root cause of the `*XM-ENNS0` "Unknown name"
   failure: the card never issues `XSNAM`/`XSCRS`, so the name never exists.
3. **Receive on the port** (`XFRCV` / `XFRRE`), with the wait or wake-up option.
   Letters arrive as ordinary messages; a message from XROUT itself is distinguished
   by message type `XMROU`.
4. **Identify the caller** - `XFMST` on the received message yields the sender's
   magic number. Store it; that is the session's return address.
5. **Vet the caller** using whatever the letter body carries, *before* answering.
6. **Answer** - `XFSND` to the caller's magic number for a normal reply, or with the
   forward option to answer without disclosing your own address.
7. **Track capacity** if a connection port: `XSNSP +1` when a session ends.
8. **Clean up** - close the port on shutdown, which clears the name. `XSCNM` if the
   name must go earlier than the port.

Two rules that are easy to get wrong:

- **The naming call must be sent from the port it names.** There is no "name this
  port" parameter; XROUT uses the sending port. (VERIFIED, appendix B sections 3.1
  and 3.9.)
- **The message buffer must be big enough for the REPLY, not just the request** -
  XROUT reuses the caller's buffer and returns an error if the reply will not fit
  (appendix B section 2, VERIFIED).

---

## 7. Where this lives in our code

- `Xmsg.Api/Xrout/XroutRequests.cs` - typed builders for `XSNAM`, `XSCRS`, `XSNSP`,
  `XSLET`, `XSLEK`, `XSGMG`, `XSGIN`, `XSCNM` and the rest, with the appendix B
  parameter numbers.
- `Xmsg.Api/Xrout/XroutReply.cs` - the reply view, including the "XROUT overwrote the
  service byte with a status" rule.
- `Xmsg.Api/Rr/IRrServer.cs` - the request-response server contract (chapter 4),
  which is the shape a `*SERVER` component should present to application code.
- `Xmsg.Api/Model/XmsgKnownServers.cs` - the observed name/logical-port registry.
- `Xmsg.Servers/Tad/TadServer.cs` - the one fully implemented named server
  (`*TADADM`), useful as the worked example.

## 8. Open

- Only `*TADADM` has its logical port confirmed from the wire. Port numbers are
  load-order dependent, not well known, so a wire capture only ever confirms the boot it
  came from - the registry walk in
  [XMSG-XSCRS-CONNECTION-PORTS-CAPTURED-2026-07-27.md](XMSG-XSCRS-CONNECTION-PORTS-CAPTURED-2026-07-27.md)
  is the authoritative view for a given startup order.
- ~~No `XSNAM`/`XSCRS` registration exchange has been captured~~ - **CLOSED 2026-07-27.**
  Both are captured from guest memory, with the buffers, the follow-up `XFSND` to XROUT
  and the `XSNSP` calls that build the free-SP count. `XSNAM`:
  [XMSG-XROUT-BUFFER-FORM-CAPTURED-2026-07-26.md](XMSG-XROUT-BUFFER-FORM-CAPTURED-2026-07-26.md).
  `XSCRS`:
  [XMSG-XSCRS-CONNECTION-PORTS-CAPTURED-2026-07-27.md](XMSG-XSCRS-CONNECTION-PORTS-CAPTURED-2026-07-27.md).
- `XSGIN` is **CLOSED 2026-07-27** - captured for a system name, a port name and an unknown
  name, confirming that the port number comes back as parameter 1 only for a port name:
  [XMSG-XSGIN-NAME-LOOKUP-CAPTURED-2026-07-27.md](XMSG-XSGIN-NAME-LOOKUP-CAPTURED-2026-07-27.md).
- `XSGMG` remains open, and the XMSG-COMMAND route is now a DEAD END rather than an untried
  one. The raw builder exists behind `SET-ADVANCED-MODE` plus `SET-PRIVILEGED`, and it
  assembles a byte-correct service-71 request - but `MESSAGE-STATUS` shows the message stays
  at length 0, because nothing copies the output buffer into it. Both send paths were tried
  and both fail for that reason. Next: carve XMSG-COMMAND to find which command issues
  `XFWRI`, or wait for the ENNS0 network-server path. Full detail:
  [XMSG-XSGIN-NAME-LOOKUP-CAPTURED-2026-07-27.md](XMSG-XSGIN-NAME-LOOKUP-CAPTURED-2026-07-27.md)
  sections 7 and 8.
