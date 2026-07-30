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

> **CORRECTED 2026-07-30 - see section 11.1.** The paragraph above assumes `ITransport` is a shared
> contract that XMSG would have to fit under. It is not: TNFS and Econet each define their own
> `ITransport` typed to their own message, and `RetroFS.Server.Core` has no transport abstraction at
> all. COSMOS defines a third one, and LAPB lives inside its implementation. There is no seam to
> conflict with, so this is not a mismatch. The line in the table above marking `Xmsg.Hdlc` /
> `Xmsg.Live` as "a layer below `ITransport`" is right about the layering and wrong to call it a
> problem.
>
> Section 4 also says the server would serve `IFileSystemBackend` - **superseded by section 10**: it
> should serve `INdfsFileSystem`.

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

- ~~That the migration is mechanical throughout. `Xmsg.Api` models the SINTRAN MON 200 call surface,
  which has no RetroFS counterpart and may not want one.~~ **CORRECTED - section 11.3.** `Xmsg.Api` is
  not a MON 200 surface; it is the COSMOS Programmer Guide's XMSG programming interface, owns no wire
  bytes, and is the closest thing the tree has to the client library. It should move.
- ~~That `IFileSystemBackend` is sufficient for COSMOS.~~ **SETTLED - section 10.** It is not, and it
  should not be widened either; serve `INdfsFileSystem` instead. `IFileSystemBackend` cannot even
  report a file size faithfully on a non-OS backend (10.2).
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

> **INVESTIGATED 2026-07-30 - see section 9.** The two items below marked *Unresolved* and
> *Not investigated* have now been answered from the code. The short version: the per-peer split
> already exists and is correct, the blocker is a duplicated copy of the same state plus a missing
> accept loop, and node numbers cannot be allocated at connect time.

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

> **INVESTIGATED 2026-07-30 - see section 10.** Neither of the two candidate shapes below is the
> answer. The NDFS-native seam already exists as `INdfsFileSystem`, and it already returns
> `ObjectEntry`. The two options are kept here as a record of what was considered.

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

1. ~~**Node identity and concurrency** (8.3).~~ **ANSWERED - section 9.** No decision meeting needed;
   it needs the refactor in 9.2 (collapse the duplicated per-link seed state onto `XmsgLink`) plus an
   accept loop. Node numbers cannot be allocated at connect time. One narrow decision is left: one
   node number per host process with N links (recommended), or several.
2. ~~**The file system contract** (8.4).~~ **ANSWERED - section 10.** Neither candidate: serve
   `INdfsFileSystem`, which already exists and already returns `ObjectEntry`. One small addition to
   RetroFS is needed for CREATE-FILE's page count (10.4).
3. ~~**Client/server package boundary** (8.1).~~ **ANSWERED - section 11.2 to 11.4.** The dependency
   graph already matches the split; the one break is `Xmsg.Live -> Xmsg.Node`, fixed by the SAME 9.2
   refactor. `Xmsg.Api` is NOT a MON 200 surface (11.3) - it is the manual-derived client API and it
   should move, with `Rr\` going to Core because it is shared.
4. ~~**Where LAPB sits** relative to `ITransport` (section 4).~~ **DISSOLVED - section 11.1.** There is
   no shared `ITransport`; each protocol defines its own. Nothing to fit under.
5. ~~**Test strategy across the move**~~ **ANSWERED - section 12.** 1049 assertions reverse their
   argument order; byte-array equality survives (verified against RetroFS's own usage); 30
   `ITestOutputHelper` classes need hand work. The real risk is not NUnit - it is the 33 sites that
   pass silently when the pcap corpus is absent (12.4), and that default should be inverted before
   the move.

**All five agenda items are now answered.** What remains needing a decision from you is listed in
section 13.

Prerequisite for the session, and the current work: the protocol research in section 6 step 3.
Designing the server contract while FILE-STAT, CREATE-FILE and the bulk transfer stream are still
uncaptured would mean designing around three unknowns.

---

## 9. Agenda item 1 answered: node identity and concurrency (2026-07-30)

Section 8.3 listed two items as *Unresolved* and *Not investigated*. Both are answerable from the
current code, with no live machine needed. This section replaces the guesses with what the code says.

### 9.1 The per-peer split ALREADY EXISTS, and it is correct

`Xmsg.Node\Services\XmsgLink.cs` is exactly the per-remote-node envelope state the multi-client
design needs:

```csharp
public sealed class XmsgLink
{
    public ushort RemoteNode { get; }
    public byte   Seed      { get; set; }   // per-link envelope seed
    public ushort NextFlags1{ get; set; }   // one continuous outgoing sequence per link
}
```

`XmsgServerHost` holds them as `Dictionary<ushort, XmsgLink> _links`, keyed by remote node, and every
peer-facing operation already takes the peer as a parameter - `ConfirmDelivered(ushort remoteNode,
...)`, `ResetSequence(ushort remoteNode)`, `ResyncAcceptDown(ushort remoteNode)`, and a
`_pendingAccepts` dictionary keyed the same way.

It also gets the identity split right: **one** `_nodeNumber` for the whole host, with
`AllocateSessionPort()` and `AllocateSessionNumber()` as node-wide allocators. That is precisely the
"multiplex by XMSG port, not by connection" model section 8.3 asked for - so that bullet is
**satisfied today**, not something to build.

### 9.2 The actual blocker: the same state exists twice

`XmsgNode` carries a **second, scalar copy** of what `XmsgLink` models per peer:

```csharp
private bool  _linkSeedLearned;
private byte  _linkSeed;          // "learned ONCE from the first valid data frame"
public  ushort OutgoingDatagramSequence { get; }
public  byte   AckCounter { get; }
```

Those scalars are only meaningful for **one** link. `LiveNode` takes exactly one `XmsgNode`, and
`Program.cs` builds exactly one of each:

```
Program.cs:893   XmsgNode xnode = new XmsgNode(node, 0x00);
Program.cs:923   LiveNode live  = new LiveNode(transport, link, xnode);
```

So accepting N incoming HDLC links today would either share one `XmsgNode` across N peers - which
mixes N seeds and N sequences into one pair of scalars - or create N of them, which would fork the
node identity, the routing table and the server host N ways. Neither is correct.

This is **duplication, and duplication is the defect**: `XmsgLink` and the `XmsgNode` scalars are two
representations of one fact. The fix is to collapse the scalars onto `XmsgLink` and have `XmsgNode`
resolve the link by remote node, exactly as `XmsgServerHost` already does. Then one `XmsgNode` serves
N links correctly and nothing needs forking.

**This is not a theoretical risk.** The comments in `XmsgNode` record a real live failure from getting
this state wrong: a per-connect ACK re-seed "reset the channel to DE where the real 102 rode DD past
the ACK baseLow, crashing 100 at PERF_CONNCT on the third connect", and a measured 2026-07-07 bug
where "a burst chunk went out with the Counter for seed 0x16 instead of 0x14". Sharing seed scalars
across peers is the same bug class, and it crashes the peer rather than failing locally.

### 9.3 There is no accept loop

`Xmsg.Live.Runner\Program.cs` contains **zero** references to `TcpListener`, `AcceptTcpClient` or
`AcceptSocket`. The runner only dials out. `topology.json` says so plainly:

```
// Reserved for the planned HUB mode: the port neighbours dial to reach US (we LISTEN, not connect out).
// Not yet active - the accept loop / multi-link forwarding is a follow-up.
"listen": { "host": "0.0.0.0", "port": 10402 },
```

and `Topology.cs` confirms it: "this field only records the intended listen endpoint."

The good news is that the transport needs no change. `TcpBridgeTransport` already has a
`TcpBridgeTransport(TcpClient client)` constructor alongside `ConnectAsync`, so an accepted socket
wraps directly. The missing piece is only the accept loop and the per-link fan-out from 9.2.

### 9.4 Node numbers CANNOT be allocated at connect time

Section 8.3 asked whether node numbers could be handed out dynamically. The answer is no, and the
reason is not in our code - it is in the peers.

`topology.json` is written "from THIS runner's own (self) perspective", and every node is statically
declared with its id, how it is reached, and its hop count. The same is true on the real machines:
their XROUT routing tables are configuration, and `IRoutingTable.TryLookup(ushort querySystem, ...)`
answers from that configuration. A peer that dials in must therefore present a node number that the
machines it wants to talk to **already have in their tables**. Nothing in the protocol assigns one.

So the concurrency ceiling is not a socket count and not a limit in our code. It is: **how many node
numbers the operator has configured across the network.** "As many concurrent incoming HDLC clients
as possible" resolves to "as many as there are pre-agreed node numbers", unless a client is content
to be reachable by nobody.

### 9.5 What this means for the architecture session

Agenda item 1 no longer needs a decision meeting - it needs the refactor in 9.2. The decision that
IS left is narrower and worth stating on its own:

- **Does a listening host serve one node number or several?** One node number and N links to distinct
  peers is a straight consequence of 9.1 plus the 9.2 fix. Several node numbers on one process would
  mean N `XmsgServerHost` instances, and nothing in the captures says a real ND machine ever did that.
  Recommendation: one node number per host process, N links.

The remaining agenda items (8.4 file system contract, 8.1 package boundary, LAPB versus `ITransport`,
test strategy) are untouched by this and still need the session.

### 9.6 What is NOT claimed here

- That the 9.2 refactor is small. It touches the envelope state that the pcap conformance tests
  guard, and those tests are the only thing keeping the wire model honest.
- That N concurrent links have ever been exercised. They have not - not live, not in a test. Every
  capture in the corpus is one or two links driven by real machines, and node 100 listens on two TCP
  ports because it has two *configured* links, not because it accepts arbitrary clients.
- Anything about LAPB's behaviour under N links. One `LapbLayer` per link is the stated design and
  `LiveNode` already composes it that way, but no test runs two at once.

---

## 10. Agenda item 2 answered: the file system contract (2026-07-30)

Section 8.4 offered two candidate shapes - widen `IFileSystemBackend`, or invent a separate
`INdfsBackend` - and section 7 flagged the question as **not investigated**. It has now been
investigated, and **neither candidate is right**: the NDFS-native seam already exists.

### 10.1 The answer: `INdfsFileSystem`, which already returns `ObjectEntry`

`RetroFS.NDFS\FileSystems\INdfsFileSystem.cs` (which extends `IBlockFileSystem`) already exposes
almost exactly the COSMOS file-server surface:

| COSMOS operation | Existing API | Notes |
| --- | --- | --- |
| LIST-FILES walk | `IReadOnlyList<ObjectEntry> GetObjectEntries()` | returns the record the wire carries verbatim |
| FILE-STATISTICS | `ObjectEntry? GetObjectEntry(string objectName, string userName)` | takes a USER NAME - which is what the wire spec block holds |
| DELETE-FILE | `void DeleteFile(string path)` | from `IBlockFileSystem` |
| read an open file | `byte[] ReadFile(string path)`, `ReadFileWithProperties(...)` | |
| write an open file | `void WriteFile(string path, byte[] data)`, `WriteFileWithProperties(...)` | |
| free space | `GetPagesAvailable()`, `GetUnreservedPages()`, `GetBlocksFree()` | |
| the user directory | `GetUsers()`, `GetUser(int userIndex)` | |
| access bits | `RetroFS.NDFS\Security\AccessControl`, `AccessPermissions` | |

And `ObjectEntry` carries every field the protocol needs, including the ones section 8.4 worried
about: `ObjectEntryNextVersion` / `ObjectEntryPrevVersion` (the version chain), `PagesInFile`,
`BytesInFile`, `AccessBits`, `UserName` / `UserIndex`, `CurrentOpenCount` / `TotalOpenCount`, and the
three packed dates.

**The seam is already half-connected.** `Xmsg.Ndfs` references `RetroFS.NDFS.Elements.ObjectEntry`
today, and section 2 verified the wire record against it field by field. So the COSMOS server should
depend on `INdfsFileSystem` and hand its `ObjectEntry` straight to the codec - no translation layer,
no new interface, no widening.

### 10.2 Why NOT to widen `IFileSystemBackend`: it is already strained

`IFileSystemBackend` is ten path-string methods returning **concrete** `FileInfo` and `FileStream`:

```csharp
FileInfo?  GetFileInfo(string path);
FileStream OpenFile(string path, FileMode mode, FileAccess access);
```

Those concrete return types cannot represent a backend that is not an OS file, and the existing
non-OS backend proves it. `RetroFS.Azure\AzureBlobFileSystemBackend.GetFileInfo` creates a temp file,
writes an empty string to it, constructs a `FileInfo` over it, and deletes the temp file - with the
comment "FileInfo is immutable for most properties, so we return a temp file". The returned object
therefore does **not** carry the blob's real size or timestamp. `OpenFile` downloads the entire blob
to a temp file before handing back a stream.

An interface that cannot faithfully report a file's size is not one to hang SINTRAN version chains,
page allocation and access bits on.

### 10.3 The precedent already set in this repo: sidecars, not widening

Econet hit the same problem - Acorn files need load address, exec address, length and access bits,
none of which `FileInfo` has. It did **not** widen the interface. `RetroFS.Protocols.Econet\FileServer\
Metadata\AcornInfStore.cs` stores them in sidecar `.inf` files read through the plain backend
(`TryReadAttributes` / `WriteAttributes`).

So RetroFS's established answer to "my retro protocol needs metadata the generic backend lacks" is a
protocol-owned sidecar, not a wider contract. COSMOS is in a stronger position than Econet was,
because it does not need a sidecar at all - the metadata is native to NDFS and `INdfsFileSystem`
already returns it.

### 10.4 The one real gap: CREATE-FILE with an explicit page count

`CREATE-FILE` prompts `NUMBER OF PAGES: 1`. There is no public API for that. `WriteFile(path, data)`
allocates from the data length, and the page-count allocator
`NdfsFileSystem.AllocateFileBlocks(uint filePages, BlockPointer.PointerType)` is **private**.

So a COSMOS `CREATE-FILE` server needs one addition to `INdfsFileSystem`: create an entry with a
reserved page count and no contents. That is a small, well-scoped change to RetroFS - not the
interface redesign section 8.4 anticipated.

### 10.5 Consequence for the package split

Section 8.1 proposed that `RetroFS.Protocols.Cosmos.Server` serve `IFileSystemBackend`, inheriting
the `RetroFS.Providers` stack "for free". That should change: it should serve **`INdfsFileSystem`**.

The cost is real and worth stating: COSMOS then serves NDFS volumes only, and does not inherit the
provider stack - no serving a remote `LIST-FILES` out of an Azure container. Section 8.4 already
guessed this was the right trade ("there is nothing sensible to put in [an object entry] for a blob
container"), and the investigation supports it. A remote directory listing returns a SINTRAN object
entry; a backend that cannot produce one cannot answer the request at all, so the generality would be
fictional.

### 10.6 What is NOT claimed

- That `INdfsFileSystem` is sufficient as it stands. 10.4 is one known gap; nothing here exercised
  read/write or delete through it against a COSMOS request, because those requests are still
  uncaptured (section 6 step 3).
- That the version chain works end to end. `ObjectEntry` HAS the next/previous version fields and the
  wire carries a `;1` generation, but no capture has exercised more than one version of a file, so
  how COSMOS asks for a specific generation is UNKNOWN.
- Anything about concurrency or locking on `INdfsFileSystem`. `CurrentOpenCount` exists in the record,
  but whether the implementation is safe for several sessions at once was not examined - and section 9
  means several sessions is the target.

---

## 11. Agenda items 3 and 4 answered: package boundary and where LAPB sits (2026-07-30)

### 11.1 Item 4 dissolves: there is no shared `ITransport` to sit below

Section 4 and section 8.1 both worried that `ITransport` is message-oriented while XMSG needs a
reliable byte stream underneath, and treated that as the one place the TNFS shape "does not carry
over". That worry rests on a false premise.

**`ITransport` is not a shared abstraction.** There are two of them, one per protocol, each in its own
package and each typed to its own message:

```
RetroFS.Protocols.TNFS  \Server\Transport\ITransport.cs   event Func<TnfsMessage,   ValueTask>? OnMessageReceived
RetroFS.Protocols.Econet\Server\Transport\ITransport.cs   event EconetMessageReceivedHandler?    OnMessageReceived
```

Both also have `RunAsync(CancellationToken)` and `SendAsync(<its own message>)`. There is **no**
transport abstraction in `RetroFS.Server.Core` at all - searched, nothing. So each protocol owning its
own transport interface is the established pattern, and COSMOS would simply define a third one typed
to an XMSG message. Nothing has to fit under a shared contract, because there is no shared contract.

That makes the layering unremarkable, and **it already exists in our tree**:

```
IByteDuplex                        bytes          Xmsg.Live\TcpBridgeTransport, InMemoryDuplex
  HdlcEncoder + LapbLayer          framing        Xmsg.Live
    ICosmosTransport               messages       <- to add: the per-protocol ITransport equivalent
      XmsgNode / XmsgServerHost    dispatch       Xmsg.Node
```

`LiveNode` is already exactly that adapter - it composes an `IByteDuplex`, a `LapbLayer` and an
`XmsgNode`. Item 4 therefore needs no architectural decision: give `LiveNode` a
`ICosmosTransport`-shaped interface matching the TNFS/Econet pair, and LAPB stays inside its
implementation where it belongs.

### 11.2 Item 3: the dependency graph already matches the proposed split - with one break

Measured from the csproj files:

| Project | Depends on |
| --- | --- |
| `Xmsg.Protocol` | (nothing) |
| `Xmsg.Hdlc` | (nothing) |
| `Xmsg.Api` | `Xmsg.Protocol` |
| `Xmsg.Node` | `Xmsg.Protocol` |
| `Xmsg.Servers` | `Xmsg.Node` |
| `Xmsg.Live` | `Xmsg.Node`, `Xmsg.Protocol`, `Xmsg.Hdlc` |
| `Xmsg.Api.Node` | `Xmsg.Api`, `Xmsg.Node` |
| `Xmsg.Chat` | `Xmsg.Api` |
| `Xmsg.Ndfs` | `Xmsg.Protocol`, `RetroFS.NDFS` |
| `Xmsg.Diagnostics` | `Xmsg.Protocol` |
| `Xmsg.Live.Runner` | `Xmsg.Protocol`, `Xmsg.Hdlc`, `Xmsg.Live`, `Xmsg.Servers`, `Xmsg.Diagnostics` |

The graph is acyclic, and `Xmsg.Protocol` and `Xmsg.Hdlc` are both **leaves with zero dependencies** -
so `Cosmos.Core` and `Cosmos.Hdlc` are already cleanly separable exactly as 8.1 proposed.

**The one break: `Xmsg.Live` depends on `Xmsg.Node`.** Section 8.1 requires that Client and Server
"both depend on Core and Hdlc, and on nothing of each other". Today the LAPB composition layer depends
on the server layer, because `LiveNode`'s constructor is
`LiveNode(IByteDuplex transport, LapbLayer link, XmsgNode node)`. A client that wants LAPB drags in
`XmsgNode`, and with it the server host.

**This is the same defect as section 9.2.** `XmsgNode` conflates two things: per-link transport state
(the seed, the outgoing Flags 1, the ACK counter) and node-level server dispatch. The transport layer
legitimately needs the former and has no business with the latter. Splitting them - which section 9.2
already requires for multi-link support - also removes this dependency, letting `Cosmos.Hdlc` depend
on the link state alone. **One refactor fixes agenda items 1 and 3 together.**

> **WRONG - CORRECTED 2026-07-30, see section 14.** The paragraph above is mistaken. An audit of all 14
> `Xmsg.Live` files found only two that reference `Xmsg.Node`, and neither depends on the link state:
> `LiveNode` calls `HandleFrames` (the dispatch entry point, inherent to being a pump), and
> `LapbLayerAdapter` merely implements `ILink`, which is declared in the wrong package. The 9.2 refactor
> does **not** fix agenda item 3; moving `ILink` down and moving `LiveNode` into Hosting does, more
> cheaply and independently.

### 11.3 CORRECTION: `Xmsg.Api` is not what section 7 says it is

Section 7 states that `Xmsg.Api` "models the SINTRAN MON 200 call surface, which has no RetroFS
counterpart and may not want one". That is wrong, and it matters for the package split.

`Xmsg.Api\README.md` is explicit: it models the XMSG **programming interface** as the COSMOS
Programmer Guide (ND-60.164.3) describes it - the XF* functions, XROUT services and the RR-LIB
request/response model - so applications are written against XMSG concepts rather than wire bytes.
Its stated hard rule is "this project owns no wire bytes", and its contents are manual-derived, not
capture-derived: `Kernel\` (`IXmsgKernel`, buffers, send flags), `Model\` (magic numbers, port
numbers, status, `SintranPassword`), `Xrout\`, and `Rr\`.

So `Xmsg.Api` is not an emulator-facing oddity to leave behind - it is the closest thing the tree has
to the **client library** 8.1 wants, and it is the one project whose contents come from the official
manual rather than from reverse engineering. It should move.

One genuine boundary question does fall out of it: `Rr\` contains **both** `IRrClient` and `IRrServer`.
The RR-LIB model is shared by both sides, so it belongs in `Cosmos.Core` (or a shared API package),
not in `Cosmos.Client`. Splitting `Xmsg.Api` down the client/server line would cut the RR-LIB model in
half for no benefit.

### 11.4 Revised package mapping

| Target package | From |
| --- | --- |
| `RetroFS.Protocols.Cosmos.Core` | `Xmsg.Protocol` + the shared parts of `Xmsg.Api` (`Model`, `Rr`, `Xrout`) |
| `RetroFS.Protocols.Cosmos.Hdlc` | `Xmsg.Hdlc` + `Xmsg.Live` (after the 9.2 split) |
| `RetroFS.Protocols.Cosmos.Client` | `Xmsg.Api\Kernel` client side, the FA client conversation |
| `RetroFS.Protocols.Cosmos.Server` | `Xmsg.Node` + `Xmsg.Servers`, serving `INdfsFileSystem` (section 10) |
| `RetroFS.Protocols.Cosmos.Hosting` | `Xmsg.Live.Runner` + `Xmsg.Api.Node` |
| stays in NDInsight | `Xmsg.Diagnostics`, `Xmsg.Chat` (worked example), the pcap corpus tests |
| disappears | `Xmsg.Ndfs` - the adapter is redundant once both halves share a tree |

### 11.5 What is NOT claimed

- That `Xmsg.Api` splits cleanly at `Kernel`. 11.3 identifies `Rr\` as shared; whether `Kernel\` is
  purely client-side was not checked file by file.
- ~~That the 9.2 split is sufficient to break `Xmsg.Live`'s dependency on `Xmsg.Node`. It is necessary;
  whether `LiveNode` touches anything else on `XmsgNode` beyond the link state was not audited.~~
  **AUDITED - section 14, and the answer overturned 11.2.** The 9.2 split is neither sufficient nor
  necessary for this: only 2 of 14 `Xmsg.Live` files reference `Xmsg.Node`, and neither touches the
  link state.
- That an `ICosmosTransport` shaped like the TNFS one is adequate. TNFS messages are self-contained
  datagrams; XMSG frames carry per-link envelope state that the layer below assigns, so the send path
  may need to hand back the assigned Flags 1. **Not designed.**

---

## 12. Agenda item 5 answered: test strategy across the move (2026-07-30)

Section 5 called the xUnit-to-NUnit port "the only real work" and warned that reversing
`Assert.Equal` arguments "silently inverts an assertion if done carelessly". Here is the measured
surface, and the one risk that turned out NOT to be a risk.

### 12.1 The port surface is narrow - no exotic xUnit features

Counted across all test projects:

| Construct | Count | NUnit equivalent |
| --- | ---: | --- |
| `[Fact]` | 361 | `[Test]` |
| `[Theory]` | 17 | `[TestCase]` |
| `[InlineData(...)]` | 78 | `[TestCase(...)]` |
| `Assert.Equal(` | **991** | `Assert.That(actual, Is.EqualTo(expected))` - **args reverse** |
| `Assert.True(` | 208 | `Assert.That(cond, Is.True)` |
| `Assert.False(` | 80 | `Assert.That(cond, Is.False)` |
| `Assert.Contains(` | **58** | `Assert.That(coll, Does.Contain(x))` - **args reverse** |
| `Assert.NotNull(` | 26 | `Assert.That(x, Is.Not.Null)` |
| `Assert.Throws` | 14 | same name, lambda form differs |
| `Assert.Empty(` / `NotEmpty(` | 18 / 5 | `Is.Empty` / `Is.Not.Empty` |
| `Assert.NotEqual(` | 13 | `Is.Not.EqualTo` |
| `ITestOutputHelper` | 30 | see 12.3 - structural, not a rewrite |
| `[MemberData]` | **0** | - |
| `IClassFixture` | **0** | - |
| `Skip =` | **0** | - |

The three zeros matter: no data-provider methods, no shared fixtures, no skipped tests. Nothing needs
redesign - it is a mechanical rewrite plus the one structural change in 12.3.

**1049 of the assertions reverse their argument order** (991 `Assert.Equal` + 58 `Assert.Contains`).
That is the whole of section 5's warning, quantified. Note `Assert.Contains` is the nastier of the two:
xUnit has both `Assert.Contains(expected, collection)` and `Assert.Contains(substring, string)`, and
they take the same order, so a rewrite must know which overload it is looking at.

### 12.2 RESOLVED: byte-array equality survives the port

This was the real threat and it needed checking rather than assuming. Almost the entire value of this
suite is **byte-for-byte replay** - "rebuild the captured frame and compare". Every one of those is an
`Assert.Equal(byte[], byte[])`, which xUnit compares element-wise. If NUnit's `Is.EqualTo` compared
byte arrays by reference, all of them would silently pass on any two arrays and the suite would become
worthless while still reporting green.

It does not. NUnit's `Is.EqualTo` performs element-wise comparison on `IEnumerable`, and **RetroFS
already depends on this**:

```
RetroFS\tests\RetroCommander.Core.Tests\NativeFileSystemPanelTests.cs:128
    Assert.That(readBack, Is.EqualTo(content), "OpenReadAsync returns the exact bytes written");
```

So the replay tests port without semantic change. Verified against the target repo's own usage, not
inferred from documentation.

### 12.3 `ITestOutputHelper` is a structural change, not a rewrite

30 sites. xUnit injects `ITestOutputHelper` through the **test class constructor**; NUnit has no
constructor injection and uses the static `TestContext.Out` instead. So each affected class needs its
constructor deleted, its `_output` field removed, and every `_output.WriteLine(...)` retargeted.

That cannot be done by find-and-replace on the call sites alone - the constructor has to go too, or the
class will not construct. It is the one part of the port that must be done per-file by hand.

This matters more than the count suggests: the diagnostic tests added on 2026-07-30
(`ChannelOffsetDiagnosticTests`, `FaSpecBlockCrossUserTests`, `FaOperationDumpTests`) carry their
evidence in that output. Losing it turns them into bare pass/fail.

### 12.4 The corpus is the real risk, and it is not about NUnit at all

**17 test files depend on the pcap corpus**, and 14 of them contain 33 sites that log "skipping" and
**pass** when the corpus is absent.

The corpus lives at `E:\Dev\Ronny\X25Emulator\pcap` - **outside both repositories**, checked into
neither. Discovery walks up from `AppContext.BaseDirectory` looking for `X25Emulator\pcap`, with an
`XMSG_PCAP_DIR` override.

Good news on the mechanics: **the discovery still works after the move.** `NDInsight`, `RetroFS` and
`X25Emulator` are all siblings under `E:\Dev\Ronny`, so the walk-up reaches the same parent from either
tree, and the env-var override covers anything else. No path changes needed.

The problem is the silent pass. These tests are, in this document's own words, "the only thing keeping
the wire model honest" - and today on any machine without the corpus they all no-op green. Moving them
into a repository that other people and CI build makes that far more likely to happen unnoticed.

Recommendation, and it should be settled before the port rather than after:

 - Make an absent corpus **FAIL** by default, with an explicit opt-out (for example
   `XMSG_PCAP_OPTIONAL=1`) for a developer who genuinely does not have it. Inverting the default is a
   one-line change per site and turns 33 silent passes into a loud, correct error.
 - Or vendor a minimal corpus - a handful of the small captures - into the repo so the conformance
   tests always have something real to run against.

Either is fine; leaving it as-is is not, because the failure mode is a green suite that proves nothing.

### 12.5 Two smaller conventions

 - **Layout.** RetroFS keeps tests in a top-level `RetroFS\tests\` folder, not beside `src`. The XMSG
   tree keeps them adjacent. Mechanical, but decide it once rather than per project.
 - **XML doc enforcement.** Section 5 recorded this as costing nothing ("docs simply stop being
   enforced"). Reconsider: the XMSG projects build with `GenerateDocumentationFile=true` and a
   documentation sweep on 2026-07-30 expanded 252 collapsed comments and removed the last banned
   `<list>` blocks to reach 0 warnings. Dropping enforcement lets that decay silently. Keep
   `GenerateDocumentationFile=true` on the Cosmos packages even though the rest of RetroFS does not.

### 12.6 Suggested port procedure

1. Port one project at a time, in the section 6 step 5 order, suite green before and after.
2. Do the 1049 order-reversing assertions with a rewrite that reverses arguments, then **prove the
   rewrite**: pick a sample of replay tests, deliberately corrupt one expected byte, and confirm each
   one fails. A reversal bug shows up as a test that cannot fail.
3. Do the 30 `ITestOutputHelper` classes by hand (12.3).
4. Settle the corpus default (12.4) BEFORE the move, while the suite is still known-green here.

### 12.7 What is NOT claimed

- That the assertion rewrite is safe to automate unsupervised. 12.6 step 2 exists because a reversed
  `Is.EqualTo` still compiles and still passes on symmetric data.
- That 991 and 58 are exact per-assertion counts. They are line-match counts; a line carrying two
  assertions counts once.
- That the corpus is complete. It is what has been captured, and section 6 step 3 lists what is still
  missing.

---

## 13. Where this stands, and what needs you (2026-07-30)

All five architecture-session agenda items in 8.5 are answered from the code (sections 9 to 12). The
session no longer needs to resolve open questions - it needs to ratify decisions and authorise work.

### 13.1 The one refactor that unblocks the most

**Split the per-link state off `XmsgNode` onto `XmsgLink`.** It is the single highest-value change
identified, because one refactor fixes **two** separate problems:

 - agenda item 1: multiple concurrent HDLC links become possible at all (9.2).
 - the duplication itself: `XmsgLink` and the `XmsgNode` scalars are two representations of one fact.

~~agenda item 3: the `Xmsg.Live -> Xmsg.Node` dependency disappears (11.2).~~ **Struck 2026-07-30 -
see section 14.** That dependency is a file-placement problem, not this one, and this refactor does not
fix it. It is fixed separately and far more cheaply by moving `ILink` down out of `Xmsg.Node.Seam` and
moving `LiveNode` into Hosting.

It is **not** started, deliberately. It changes the envelope state that crashed node 100 once already
(9.2), so it wants doing with the conformance suite green either side and preferably a live re-test -
not slipped in unannounced.

### 13.2 Decisions that are yours

| # | Decision | Recommendation |
| --- | --- | --- |
| 1 | One node number per host process, or several? (9.5) | one, with N links |
| 2 | Fix the RetroFS `ObjectEntry.ToBytes` header bug? (section 3) | yes - one line, data-losing |
| 3 | Add a page-count create to `INdfsFileSystem`? (10.4) | yes - CREATE-FILE needs it |
| 4 | Invert the pcap-corpus default so absence fails? (12.4) | yes, before the port |
| 5 | Keep `GenerateDocumentationFile=true` on the Cosmos packages? (12.5) | yes |
| 6 | Authorise the 13.1 refactor | after a live re-test is possible |

### 13.3 Still blocked on the live machine

The nodes were up on 2026-07-30 (9010, 9102, 10362, 10364 listening, the 102->100 link established),
but terminal I/O from the tooling is blocked: a TCP connect to the console ports succeeds and then
read/write on the stream is refused. So these need you to drive the terminal:

 - a transfer of a differently sized file, to settle whether the 594-byte fragment split is
   protocol-fixed or an artefact of one file size - the last remaining single-case constant.
 - MON 200 functions 3, 4, 5 and 11, and the option-bit split in `1014` / `23014` / `40015`.
 - `APPEND-REMOTE-BATCH`, and a read and a write against an open file.
 - `CREATE-FILE`, which section 10.4 needs in order to know what the server must answer.

### 13.4 Cleanup owed on the live machine

Files created during the captures, still present: `DUMMY:DATA` on nodes 100 and 102, `DUMMY:SYMB` on
100, `FTPULL:SYMB` on 102.

---

## 14. CORRECTION to 11.2: the `Xmsg.Live -> Xmsg.Node` dependency is a file-placement problem, not the duplication (2026-07-30)

Section 11.5 recorded this as unverified: "That the 9.2 split is sufficient to break `Xmsg.Live`'s
dependency on `Xmsg.Node`. It is necessary; whether `LiveNode` touches anything else on `XmsgNode`
beyond the link state was not audited." It has now been audited, and **11.2 was wrong**.

### 14.1 What the audit found

`Xmsg.Live` has 14 source files. Only **two** reference `Xmsg.Node` at all:

| File | Why it depends on `Xmsg.Node` |
| --- | --- |
| `LiveNode.cs` | one functional call: `_node.HandleFrames(decoded)` at line 268 |
| `Seam\LapbLayerAdapter.cs` | implements `ILink`, which is declared in `Xmsg.Node.Seam` |

The other twelve - `HdlcEncoder`, `IByteDuplex`, `InMemoryDuplex`, `LapbLayer`, `LapbLayerState`,
`LapbOptions`, `RawFrameReceivedHandler`, `TcpBridgeTransport` and the four `Logging` files - are
**already completely free of `Xmsg.Node`**. That is the whole of the genuine HDLC/LAPB/transport layer.

### 14.2 Why 11.2 was wrong

11.2 claimed the 9.2 refactor (splitting per-link state off `XmsgNode` onto `XmsgLink`) would remove
this dependency. It will not, because the dependency was never on the link state:

- `LiveNode`'s single use of `XmsgNode` is `HandleFrames` - the **dispatch entry point**. It is a pump:
  read bytes, run LAPB, decode, hand the frame to the dispatcher, encode the replies, send. Depending
  on the dispatcher is inherent to being a pump; no amount of splitting state off `XmsgNode` changes
  that.
- `LapbLayerAdapter`'s dependency is not on behaviour at all. It implements `ILink`, and `ILink`
  happens to be **declared in the wrong package**.

### 14.3 The actual fix is cheaper than the refactor

Two moves, neither of which needs the 9.2 work:

1. **Move `ILink` (and the `Xmsg.Node.Seam` interfaces the transport implements) into Core or Hdlc.**
   An interface implemented by the transport layer and consumed by the node layer belongs below both.
   `LapbLayerAdapter` then has no `Xmsg.Node` reference.
2. **`LiveNode` is not HDLC - it is a host.** It composes an `IByteDuplex`, a `LapbLayer` and an
   `XmsgNode` into a run loop. That is `Cosmos.Hosting`'s job, and `Cosmos.Hosting` may legitimately
   depend on both the transport and the server.

Result: `Cosmos.Hdlc` = 13 of the 14 files with **zero** dependency on the server half, satisfying
8.1's "nothing of each other" cleanly.

### 14.4 What this changes about the 9.2 refactor

The 9.2 refactor is still worth doing - it is what makes multiple concurrent links possible (agenda
item 1) and it removes a genuine duplication between `XmsgLink` and the `XmsgNode` scalars. But its
value is **narrower than section 13.1 claims**: it fixes agenda item 1 and the duplication, and it does
**not** fix agenda item 3. Item 3 is fixed by the two moves above, independently and much more cheaply.

So the "one refactor fixes three problems" framing in 13.1 is overstated. Corrected count: one
refactor fixes two problems, and a separate interface move fixes the third.

### 14.5 Why this is recorded rather than quietly edited

Sections 11.2 and 13.1 were committed and pushed before this audit ran. The claim is left in place
above with this correction pointing at it, because a plan that silently rewrites its own conclusions is
harder to trust than one that shows where it was wrong. This is the fourth published claim in this
decode to be overturned by actually measuring it - see the methodological note in section 6i of
`XMSG-LIST-FILES-ON-THE-WIRE-2026-07-29.md`.
