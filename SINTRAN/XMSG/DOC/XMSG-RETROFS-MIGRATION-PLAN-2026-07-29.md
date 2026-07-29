# Migrating the XMSG/COSMOS work into RetroFS

**Date**: 2026-07-29
**Question asked**: can all of this move into the RetroFS world, and do we migrate now or later?
**Answer**: take the dependency now, migrate after the protocol research settles. The first step is
already done.

---

## 1. The decision, and why

Migrate **later**, depend **now**.

The XMSG tree is not small:

| Project | Files | Lines |
| --- | ---: | ---: |
| Xmsg.Protocol | 60 | 7172 |
| Xmsg.Api | 31 | 6447 |
| Xmsg.Node | 31 | 5886 |
| Xmsg.Live | 14 | 2372 |
| Xmsg.Servers | 3 | 2369 |
| Xmsg.Hdlc | 7 | 1215 |
| Xmsg.Live.Runner | 2 | 1273 |
| Xmsg.Chat + Diagnostics + Api.Node | 9 | 1965 |
| **tests** (5 projects) | 54 | 9631 |
| **total** | **~210** | **~38300** |

Moving that is a week of mechanical work including an xUnit-to-NUnit port. Doing it now would mean
porting a moving target: the directory-entry layout changed twice on 2026-07-29 alone, and
FILE-STAT, CREATE-FILE and the bulk transfer stream are still uncaptured.

What was urgent was the **duplication**, not the migration. That is fixed - see section 2.

---

## 2. Done already: the NDFS dependency

The 64-byte record the COSMOS file server returns for each directory entry **is the SINTRAN on-disk
object entry, shipped verbatim**. Verified field by field against
`E:\Dev\Ronny\RetroFS\src\RetroFS.NDFS\Elements\ObjectEntry.cs`: every offset agrees, including the
ones this project had not decoded.

The clinching evidence is the dates. The three packed words at +40/+44/+48 were run through
`RetroFS.NDFS.Elements.NdDateTime` - written for the on-disk structure, with no knowledge of this
protocol - and came out as coherent history on a machine whose clock read 29 JULY 1998:

| Entry | Created | Last read | Last written |
| --- | --- | --- | --- |
| SINTRAN:DATA | 1998-07-06 16:55:35 | 1998-07-06 16:59:48 | 1998-07-06 16:59:48 |
| SEGFIL0:DATA | 1998-07-06 16:55:36 | 1998-07-28 07:41:03 | 1998-07-28 07:41:03 |
| RTFIL:DATA | 1998-07-06 16:56:00 | 1998-07-28 07:41:06 | 1998-07-28 07:41:06 |
| SYSTEM-OUTPUT-1:SYMB | 1998-07-06 16:56:06 | never | 1998-07-29 13:30:35 |

The system files were all created within seconds of each other when the pack was built, the two
system files were last opened at the previous boot, and the spooler output file was written on the
day of the capture and never read. None of that was decodable before.

### Structure adopted

```
Xmsg.Protocol      no RetroFS dependency; treats the 64-byte record as OPAQUE
   |
Xmsg.Ndfs          NEW - owns the single ProjectReference to RetroFS.NDFS
   |                     maps record <-> ObjectEntry, nothing else
Xmsg.Ndfs.Tests    NEW - 21 tests, green
```

`FaDirectoryEntry`, which had begun to re-implement the layout and the date maths, was **deleted**.
`Xmsg.Protocol` deliberately does not reference RetroFS: that keeps the wire library from acquiring
`RetroFS.Core` and `RetroFS.Local` transitively. `Xmsg.Ndfs` is the seam that disappears at
migration time, when both halves live in the same tree.

---

## 3. DEFECT FOUND IN RetroFS

`ObjectEntry.ToBytes` writes the header byte as a literal:

```csharp
// Header byte (0x80 = in use)
buffer[offset] = 0x80;
```

It should write `Header`. The captured `SINTRAN:DATA` entry arrives with `0x90`, so a round trip
silently clears **bit 12, "file modified"** - the meaning given in the field documentation on
`ObjectEntry.Header` itself. Bits 15 to 8 are all exposed: used, opened-for-write, reserved,
modified, and the user-vs-object entry flag.

This is a **data-losing bug on the RetroFS write path**, independent of this protocol: any on-disk
entry rewritten through `ToBytes` loses those flags. It matters here because a COSMOS server
answering out of RetroFS would hand clients entries with the modified flag stripped.

`Xmsg.Ndfs.Tests.RetroFsDropsTheHighHeaderByteOnWrite` asserts the current wrong behaviour on
purpose, so fixing RetroFS makes it fail and forces this note to be revisited.

**Not fixed here** - it is in your other repo and is a one-line change, so it is your call.

---

## 4. Where each piece lands

RetroFS already has the exact shape this needs. `RetroFS.Protocols.TNFS` is the template, and the
mapping is close to one-to-one:

| TNFS | XMSG equivalent today | Notes |
| --- | --- | --- |
| `Protocol/TnfsConstants,Enums,Header,Structures` | `Xmsg.Protocol` (enums, envelope, QFORM, FA codecs) | direct |
| `Server/Transport/ITransport` | `Xmsg.Live` `IByteDuplex` | same idea, byte stream vs message |
| `Server/Transport/TcpTransport` | `Xmsg.Live` `TcpBridgeTransport` | direct |
| `Server/Transport/InMemoryTransport` | `Xmsg.Live` `InMemoryDuplex` | direct - both exist for tests |
| `Server/SessionManager`, `TnfsSession` | `Xmsg.Node` `XmsgNode`, session types | direct |
| `Server/Handlers/DirectoryCommandHandler` | the LIST-FILES server (to build) | this is the current work |
| `Server/Handlers/FileCommandHandler` | the transfer server (to build) | needs a capture first |
| `Server/Handlers/SessionCommandHandler` | XROUT connect / letterbox | exists in `Xmsg.Node` |
| `Client/TnfsClient` | the LIST-FILES client (to build) | codec done |
| - | `Xmsg.Hdlc`, `Xmsg.Live` LAPB | **no TNFS equivalent**; a layer below `ITransport` |

Proposed target layout:

```
RetroFS.Protocols.Cosmos          the XMSG/COSMOS protocol + FA servers
RetroFS.Protocols.Cosmos.Hdlc     LAPB/HDLC framing - has no TNFS counterpart
```

The COSMOS server would serve `RetroFS.Server.Core.FileSystem.IFileSystemBackend`, the same seam TNFS
uses, so it inherits the whole provider stack in `RetroFS.Providers` for free.

One thing does **not** map: `ITransport` is message-oriented and assumes the transport frames
messages. XMSG sits on LAPB, which is a reliable byte stream with its own sequencing, timers and
retransmit. That is a layer TNFS simply does not have, and it must stay below the `ITransport` seam
rather than being folded into it.

---

## 5. Convention differences to settle before the port

Most conventions already agree - both repos ban LINQ and `foreach`, both use
`TreatWarningsAsErrors`, both forbid standalone test programs. The real differences:

| | NDInsight XMSG | RetroFS | Cost |
| --- | --- | --- | --- |
| Test framework | xUnit | NUnit | ~9600 lines of tests to port |
| XML docs | `GenerateDocumentationFile=true` | `false` | none - docs simply stop being enforced |
| Namespaces | block-scoped | file-scoped | mechanical |
| Headers | none | SPDX + copyright | mechanical |

The test-framework port is the only real work. `Assert.Equal(expected, actual)` becomes
`Assert.That(actual, Is.EqualTo(expected))` with the argument order reversed, which is exactly the
kind of change that silently inverts an assertion if done carelessly. It should be done in one pass
with the suite green before and after, not incrementally.

---

## 6. Sequence

1. **Done** - `Xmsg.Ndfs` adapter, duplication removed, 21 tests green.
2. Report the `ToBytes` defect (section 3); decide whether to fix it now.
3. Finish the protocol research in NDInsight while it is still volatile:
   FILE-STAT, CREATE-FILE, DELETE-FILE, read/write, and the bulk transfer stream.
4. Build the LIST-FILES client and server, prove them offline, then live as node 103.
5. Only then port. Suggested order, each step green before the next:
   `Xmsg.Protocol` -> `Xmsg.Hdlc` + `Xmsg.Live` -> `Xmsg.Node` -> `Xmsg.Servers` -> runner.
6. Delete `Xmsg.Ndfs`; at that point both halves are in the same tree and the adapter is redundant.

---

## 7. What is NOT claimed

- That the migration is mechanical throughout. `Xmsg.Api` models the SINTRAN MON 200 call surface,
  which has no RetroFS counterpart and may not want one.
- That `IFileSystemBackend` is sufficient for COSMOS. It exposes .NET `FileInfo` and `FileStream`;
  COSMOS needs SINTRAN object entries, version chains and access bits. It probably needs widening,
  or an NDFS-specific backend interface alongside it. **Not investigated.**
- That LAPB fits under `ITransport` cleanly. Section 4 flags it as the one place the TNFS shape does
  not carry over.

---

## 8. The target: what we are actually building

Stated goal: a **client library**, a **server library**, a **standard way to spin up hosts**, and the
SINTRAN file system plus connect-to services wired into `RetroServeUI`, accepting **as many
concurrent incoming HDLC clients as possible**, every one of them able to do file transfer and
connect-to against whatever the RetroServer is configured with.

### 8.1 Proposed package split

```
RetroFS.Protocols.Cosmos.Core      wire types shared by both sides
                                     envelope, QFORM, XROUT, FA codecs, enums
                                     no sockets, no file system, no hosting

RetroFS.Protocols.Cosmos.Hdlc      LAPB/HDLC framing and timers
                                     the layer TNFS has no counterpart for

RetroFS.Protocols.Cosmos.Client    client library
                                     list files, file-stat, create, delete, read, write,
                                     transfer, connect-to

RetroFS.Protocols.Cosmos.Server    server library
                                     *FA-SERVER, *XFTRA, TAD responder
                                     serves an IFileSystemBackend

RetroFS.Protocols.Cosmos.Hosting   DI + BackgroundService wiring, config binding
                                     the "standard way to spin up a host"
```

Client and server both depend on Core and Hdlc, and on nothing of each other. That split is what
makes a client usable in a test harness without dragging in a file system, and it is roughly where
`Xmsg.Api` / `Xmsg.Node` / `Xmsg.Servers` already sit - the current tree just does not enforce it.

### 8.2 Hosting

Follow `TnfsServerService`: a `BackgroundService` taking `IEnumerable<ITransport>` plus per-command
handlers, resolved through DI. `CosmosServerService` would take the same shape, so
`RetroServerHost` starts it exactly as it starts TNFS today, and `RetroServeUI` gains a
`CosmosMonitorViewModel` + view beside `TnfsMonitorViewModel` and `EconetMonitorViewModel`.
Statistics and firewall integration come along the same path TNFS already uses.

Note for the UI work: an Avalonia `DataGrid` needs its `StyleInclude` set, or it renders blank.

### 8.3 The hard part - many concurrent HDLC clients

This is the part that is **not** a straight copy of TNFS, and it is the main reason to hold an
architecture session first.

TNFS is UDP/TCP: a client is an address, and sessions are cheap and independent. COSMOS is not. What
arrives on an HDLC link is not a client, it is a **peer ND machine with a node number**, and above
LAPB sits XMSG routing where messages are addressed to `system:port`, not to a connection. Concretely:

- **One LAPB state machine per link.** Sequencing, T1 retransmit and window are per-link and
  stateful. Today `Xmsg.Live.LapbLayer` is exactly that, and it must stay one-per-link.
- **A node number per peer, not per socket.** Node 100 in the test rig listens on two TCP ports
  because it has two links, and each peer has its own station number. Accepting N incoming clients
  means either N configured links, or a scheme for handing out node numbers dynamically - and
  routing tables on the *other* machines have to agree. **Unresolved.**
- **Multiplexing is by XMSG port, not by connection.** File access and connect-to already share one
  link; that is how the captures look. So "every client can do file transfer and connect-to" is
  naturally satisfied *if* the dispatch is on the XMSG port, and impossible if it is on the
  transport.
- **Concurrency limit is a routing question, not a socket question.** How many peers XMSG routing
  can carry, and whether node numbers can be allocated at connect time, both need checking against
  the routing tables. **Not investigated.**

### 8.4 The file system seam

`IFileSystemBackend` exposes .NET `FileInfo` and `FileStream`. COSMOS needs SINTRAN object entries -
version chains, access bits, page allocation, the packed dates, user indices. Section 7 flags this;
it is the second reason for an architecture session.

Two candidate shapes:

- **Widen `IFileSystemBackend`** with an optional richer interface that NDFS implements and other
  backends do not. Keeps one seam; risks bolting SINTRAN concepts onto a generic contract.
- **A separate `INdfsBackend`** that COSMOS requires outright. Honest about the fact that COSMOS
  cannot meaningfully serve an FTP or Azure volume, but loses the free provider stack.

The second is probably right - a remote `LIST-FILES` returns an object entry, and there is nothing
sensible to put in one for a blob container - but that is a judgement to make deliberately, not in
passing.

### 8.5 Architecture session - proposed agenda

Hold this **before** the port, not during. Agenda, in dependency order:

1. **Node identity and concurrency** (8.3). How does an incoming HDLC connection acquire a node
   number, and what is the real ceiling on concurrent peers? Everything else depends on the answer.
2. **The file system contract** (8.4). Widen `IFileSystemBackend`, or a separate NDFS-only seam?
3. **Client/server package boundary** (8.1). Confirm the five-package split, and decide where the
   MON 200 surface in `Xmsg.Api` belongs - or whether it stays behind in NDInsight as emulator-facing.
4. **Where LAPB sits** relative to `ITransport` (section 4).
5. **Test strategy across the move**: the xUnit-to-NUnit port (section 5) and how the pcap-driven
   conformance tests survive it. These tests are the only thing keeping the wire model honest, so
   they must not be weakened in transit.

Prerequisite for the session, and the current work: the protocol research in section 6 step 3.
Designing the server contract while FILE-STAT, CREATE-FILE and the bulk transfer stream are still
uncaptured would mean designing around three unknowns.
