# Plan: a C# COSMOS file server, its client test harness, and Windows folder sync (2026-08-01)

Follows [PLAN-CSHARP-ETHERNET-AND-ROUTE-THROUGH-2026-08-01.md](PLAN-CSHARP-ETHERNET-AND-ROUTE-THROUGH-2026-08-01.md),
which covers the transport underneath this. That plan's stage 1 (`Xmsg.Ethernet`) is **built and
green** - 27 tests, framing and link layer, verified against captured frames.

Three things are planned here:

1. A **client-side test harness** that drives our own file server the way SINTRAN would, so all
   traffic is exercised without a live machine.
2. The **file server** itself, and what it must serve from.
3. A **folder-sync application**: edit in VS Code on Windows, save, and the file appears under a
   SINTRAN user ready to compile.

Claims are marked **VERIFIED**, **INFERRED**, or **UNKNOWN**. Section 7 lists what must be settled
by experiment before the corresponding code is written - those parts are deliberately NOT designed
here, because guessing them is how this project has lost time before.

---

## 1. The constraint that decides everything: what a directory listing returns

**VERIFIED** ([XMSG-RETROFS-MIGRATION-PLAN-2026-07-29.md](XMSG-RETROFS-MIGRATION-PLAN-2026-07-29.md)
section 2): the 64-byte record the COSMOS file server returns for each directory entry **is the
SINTRAN on-disk object entry, shipped verbatim** - checked field by field against
`E:\Dev\Ronny\RetroFS\src\RetroFS.NDFS\Elements\ObjectEntry.cs`, every offset agreeing.

That single fact answers the "do we need a metadata file per folder?" question, and it answers it
harder than expected: **a plain Windows folder does not contain enough information to answer a
`LIST-FILES`.** An object entry carries the SINTRAN name and type, the creation / last-access /
last-modified dates in ND format, the owner, the page allocation, and the header flag bits. A
Windows directory entry has none of the ND-specific ones.

So there are exactly two honest designs:

| | What it serves | Object entries | File numbers |
|---|---|---|---|
| **A. Serve an NDFS volume** | a real `.IMG` pack or NDFS container | genuine, read from disk | genuine and stable, for free |
| **B. Serve a Windows folder** | ordinary files | **synthesised** | must be invented and kept stable |

The 2026-07-29 investigation already reached this conclusion for the RetroFS integration and chose
A, in its own words: *"a remote directory listing returns a SINTRAN object entry; a backend that
cannot produce one cannot answer the request at all, so the generality would be fictional."*

**Recommendation: A, and the sync application in section 6 is what makes A convenient.** Rather
than teach the server to fake object entries for a Windows folder, we put the files INTO an NDFS
volume and let everything downstream be real. The user still edits in VS Code; the sync app does
the translation once, in one place, instead of every server response having to fake it.

If B is wanted anyway - for example to serve a scratch directory with no volume behind it - then a
sidecar metadata file IS required, and section 3 says what has to be in it.

---

## 2. File numbers: stable, and not stored in the file

**INFERRED, with good supporting evidence.** The `FILE n` in a listing is the **index of the entry
in the user's object-entry table**, not an attribute of the file:

- `ObjectEntry` has `Header`, `ObjectName`, type, dates, owner, allocation - and **no file-number
  field**. If the number were a property of the file it would have to be stored somewhere; it is
  not.
- Real listings have **gaps**. Node 100's `(SYSTEM)` listing captured 2026-08-01 runs
  `... FILE 30, FILE 31, FILE 33 ...` and `... FILE 53, FILE 56, FILE 58`. Gaps are exactly what an
  index-into-a-table produces when entries are deleted (`HeaderEntryUsed` clear) and nothing is
  compacted.

Consequences for the server, which is what the question was really about:

- The number is **stable for the life of the file** and must not be recomputed per query. A client
  may legitimately list a directory, remember `FILE 7`, and later open by number.
- The number is **per user directory**, not global.
- A deleted file leaves a **hole**, and a later create may reuse it. Renumbering on every listing
  would break any client holding a number.
- Therefore: **never derive the number from a hash of the name, and never use the position in a
  sorted listing.** Both are unstable under create/delete.

Design A gets all of this free, because the index is genuinely the table slot.
Design B must persist a name-to-slot map and honour holes - that is the sidecar's main job.

**UNKNOWN, and it matters:** whether requests actually arrive by number, by name, or both, and
whether an open is by number after a lookup. Section 7.1 says how to settle it. **Do not implement
open-by-number until that capture exists** - the server can answer everything we have actually seen
without it.

### 2.1 There is therefore a hard limit on files per user [VERIFIED]

`ND-30.003.007 EN SINTRAN III System Supervisor` section 4.3.2 (p.165):

> *"In previous versions of SINTRAN, no more than 256 files were allowed in each user area. In
> SINTRAN version K, it is possible to have as many as 4096. This is achieved by dividing the 4096
> files into 16 object blocks of 256 objects (files) each. New object blocks can be allocated for
> the user area when needed."*
>
> *"In order to speed up file searching, the number of files each user is allowed to create is
> initially restricted to 256."*

| | |
|---|---|
| Default per user area | **256** |
| Maximum on SINTRAN K | **4096** (16 object blocks x 256) |
| Grow it | `@GIVE-OBJECT-BLOCKS (<directory name:> user name)` - user SYSTEM only |
| Read the current limit | `@USER-STATISTICS`, last line `MAXIMUM NUMBER OF FILES : 512` |

This is independent confirmation of the index model in section 2: the numbering space is allocated
in blocks of 256 objects, which is exactly why numbers are stable and why holes persist rather than
being compacted away.

**Consequences:**

- A server must reject a create past the user's limit with the SINTRAN error, not by growing
  anything itself - `GIVE-OBJECT-BLOCKS` is a privileged operator action.
- **The folder-sync application (section 6) must check the limit before it starts** and refuse with
  a clear message naming `GIVE-OBJECT-BLOCKS`. A Windows source folder can easily exceed 256 files,
  and discovering that on the file that happens to cross the boundary is a needless mystery.
- Design B's sidecar (section 3) must model the same block structure, or it will hand out numbers
  the real system could never produce.

Note that `@USER-STATISTICS` is a **local** command - it takes no file, so remote file access does
not apply to it (section 1 of `ND-30.003.007` 5.4.1: RFA substitutes a remote file spec wherever a
file spec is accepted, and nothing else). Reading a remote user's limit needs `CONNECT-TO`, or
`MON 70B` from a program.

---

## 3. If a Windows folder is served anyway: the sidecar

Only needed for design B. One file per served directory, e.g. `.sintran-index.json`, holding for
each file:

- the SINTRAN object name and type (`RONNY:TXT` is name `RONNY`, type `TXT` - the Windows extension
  is not the ND type and must be mapped, not assumed),
- the assigned slot number, and the set of free/holed slots,
- ND-format creation / modification dates,
- the owner and the header flag bits.

The sidecar is authoritative for numbering; the folder is authoritative for content. A file added
in Windows with no sidecar entry gets the lowest free slot on the next scan.

Note the known RetroFS bug this interacts with: `ObjectEntry.ToBytes` does not round-trip the
`Header` flags - the 2026-07-29 investigation found a captured entry arriving as `0x90` and coming
back with **bit 12 ("file modified") silently cleared**. That is a data-losing bug on the RetroFS
write path independent of us, and any code here that round-trips entries must not rely on `ToBytes`
until it is fixed.

---

## 4. The client test harness - unit tests that drive the server

This is the piece that lets all the traffic be tested with no live machine, and it should be built
**before** the server, so the server is written against it.

### 4.1 Shape

```
[ test ] -> XmsgFileClient -> XmsgLayer -> EthernetLink -> InProcessEthernetSegment
                                                                   |
[ assert ] <- CosmosFileServer <- XmsgLayer <- EthernetLink <-------+
```

`InProcessEthernetSegment` already exists and is green (`LoopbackEthernetBackends.cs`): a dumb hub
that repeats each frame to every other port and never echoes to the sender, matching both a real hub
and RetroCore's `TcpEthernetRelay`. Delivery is synchronous on the sending thread, so a test sends a
request and asserts on the reply with no waiting, no timers and no flakiness.

### 4.2 Three layers of test, each catching something different

**(a) Vector tests - byte equality against captured frames.** The strongest kind: the expected value
is a real ND machine's output. We already have the captures:

| capture | exercises |
|---|---|
| `claude-list-files-d100-system-2026-07-29.pcapng` | directory listing |
| `claude-open-close-file-102-to-100-2026-07-30.pcapng` | open / close |
| `claude-open-W-close-102-to-100-2026-07-30.pcapng` | open for write |
| `claude-create-file-102-to-100-2026-07-30.pcapng` | create |
| `claude-delete-file-102-to-100-2026-07-29.pcapng` | delete |
| `claude-file-stat-102-to-100-2026-07-29.pcapng` | stat |
| `claude-transfer-*-2026-07-2x.pcapng` | transfer, several sizes incl. sparse |
| `ALLTEST-fa-connectto-102-100-103-2026-08-01.pcapng` | a full working FA session, 560 headers |
| `fa-access-secret-102-to-100-2026-07-29.pcapng` | password on the wire |

For each: replay the captured request into our server and assert the reply matches the captured
reply byte for byte. Where it cannot match exactly (a port number or sequence that legitimately
varies) the test asserts field by field and says in a comment why that field is excluded - never a
blanket "compare some of it".

**(b) Round-trip tests - our client against our server.** Every operation driven end to end over the
in-process segment: list, stat, open read, open write, read, write, close, create, delete, rename,
transfer. These catch state bugs the vector tests cannot, because a vector test replays one frame
and never exercises "open then read then close" ordering, file-number stability across operations,
or two clients at once.

**(c) Adversarial tests.** Wrong password (we have the `secret` / `orange` vectors and the carved
fold, so the expected word is known independently), unknown file, unknown user, open twice, close
without open, read past end, a request for a file number that is a hole, and a request arriving on a
relayed frame. Each should produce the documented error rather than an exception.

### 4.3 The rule that keeps this honest

**A test whose expected value we invented proves only that the code does what we wrote.** Vector
tests come first for every operation we have a capture of. For operations we do not, the round-trip
test is written but marked in its XML doc as *asserting our own convention, pending a capture* - so
nobody later mistakes it for evidence about SINTRAN.

---

## 5. The file server

### 5.1 What is already decoded

- `*FA-SERVER` and `*FA-FSA` opening letters, field by field
  ([XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md](XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md)).
- The XSLET accept form, and that it is generic rather than per-server
  ([XMSG-XSLET-ACCEPT-VS-XRNRO-2026-07-28.md](XMSG-XSLET-ACCEPT-VS-XRNRO-2026-07-28.md)).
- **A server must never answer with `XFRTN`** - returning the received message hands back an
  XMSG-internal pool block and **crashes the peer's kernel** (`XMSG error 23B`, `XMFIDO ABORTS`).
  Always `XFGET`/`XFWRI`/`XFSND` a fresh letter. This is the single most dangerous known mistake.
- The password travels as the 16-bit fold, never plaintext, and the fold is carved.
- The directory entry is the object entry, verbatim (section 1).
- Word alignment: strings are padded to a word and **the declared length counts the pad**.

### 5.2 What is NOT decoded - the honest gap

The **post-accept exchange is only partly carved**, and the two COSMOS file servers do not agree
with each other: `*XFTRA` puts everything in tagged XROUT parameters, while `*FA-SERVER` declares a
length covering only the documented fields and then appends **raw opaque bytes**, built in two
`XFWRI` writes with the second at displacement -1 to append. A reader assuming one write per message
truncates it.

So the server is built **capture-first, one operation at a time**: for each operation, capture it
against node 100, decode it, write the vector test, then implement. Not doc-first, and not by
analogy from the operation next to it - the two servers already disagree, which is exactly the
situation where analogy produces confident wrongness.

### 5.3 Order

1. Register a name with XROUT and answer `XSLET` with the accept form. (Fully specified.)
2. Directory listing - the object entry is known, and we have the capture.
3. Stat, open, close. (Captures exist.)
4. Read, write. (Captures exist for transfer; the FA read/write path needs section 7.2.)
5. Create, delete, rename.

---

## 6. The folder-sync application

The goal in the user's words: *"edit code in Visual Studio Code, save to local folder, and it arrives
at the SINTRAN user ready to be compiled."*

### 6.1 Direction: push, not serve

This is a **client**, not a use of the file server. A `FileSystemWatcher` on a Windows folder, and on
change the file is written to the SINTRAN user over COSMOS - the same path a real `TRANSFER-FILE`
uses. That is the right choice because:

- the file lands on the machine's **own pack**, so `NC`, `PLANC` and the linker see an ordinary
  local file with no server in the path;
- it works against an unmodified SINTRAN;
- it does not require the machine to mount anything of ours.

Writing into the emulator's `.IMG` directly is the obvious alternative and should be **avoided**:
the emulator has the pack open, and writing underneath a running system risks corrupting it.

### 6.2 Pieces

- `SintranFolderSync` - watcher, debounce (editors write several times per save), a content hash per
  file so an unchanged rewrite is not transferred, and a queue so a burst of saves is serialised.
- **Name mapping.** `Program.c` on Windows has to become a legal SINTRAN name and type. SINTRAN
  names are limited in length and character set, and the type is a separate field, not an extension.
  The mapping must be explicit and configurable, not guessed per file.
- **Create-then-write.** A SINTRAN file must exist before it is written, and for contiguous files
  the size is fixed at creation. We have create captures; the sizing rule for the target file type
  needs confirming.
- **Verification.** After transfer, stat the remote file and compare length. Silent truncation is
  the failure mode that would waste the most time.

### 6.3 Parity - SETTLED 2026-08-09

**This section is answered.** See `SINTRAN-FILE-PARITY-BIT-MEASURED-2026-08-09.md`, measured
from 587 bytes of a real file read off a live machine.

 - Bit 7 IS used in stored files, and it IS even parity - set when the low seven bits hold an
   odd number of ones. Content-determined, not positional: the same string appears three times
   in one file with an identical bit-7 pattern.
 - **But the same file also carries plain, unparitied text** - 120 of the 587 bytes are
   characters parity would have marked, left alone, and every exception is a CLEARED bit rather
   than a spurious set one. So a per-file-type rule cannot be right; the mixture is inside one
   file.
 - Therefore: **strip on the way in** (always safe, and what SINTRAN's own code does with
   `BZERO 7`), and do **not** write parity back by default. Whether any tool REQUIRES parity on
   its input is still untested and is the one open question left.

Implemented as `Xmsg.Sync.SintranParity`, with the captured bytes as the test expectation.

The original text of this section follows, kept because its reasoning about why this had to be
settled first was correct.

---

The user asked *"maybe parity is set during transfer?"*. **I do not know**, and this must not be
guessed: getting it wrong gives a file that lists correctly and fails to compile, which is the worst
kind of bug to chase.

What is known is only adjacent: on the TAD terminal path, text carries **even parity in bit 7** and
a receiver strips it. Whether SINTRAN `:SYMB` source files are stored on disk with the parity bit
set is a different question and is **not established** by that.

Settle it before writing the transfer path - section 7.3.

---

## 7. Experiments that must run before the matching code is written

**7.1 File numbers - by number or by name, and HOW WIDE?**

Two questions, and the second is the one that will bite.

*(a) By number or by name.* Capture a `LIST-FILES` followed by an operation on a listed file, and
look at whether the second request carries the number, the name, or both. Then delete a file, create
another, and re-list to confirm the hole is reused and numbering is not compacted. Until this
exists, do not implement open-by-number.

*(b) Is the number 8 or 16 bits?* **A user may hold up to 4096 files (section 2.1), which needs 12
bits. A one-byte field cannot address them.** So any field we have recorded as a byte in this area
is suspect.

The concrete suspect is the `*FA-SERVER` trailer in
[XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md](XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md),
recorded as 8 bytes of which *"only the sixth byte moves, stepping 06 -> 08 -> 0A"*. Read as 16-bit
words instead:

```
07 E2   00 00   00 06   64 00
 |       |       |       |
 |       |       |       +-- 0x0064 = 100 BYTE-REVERSED - the MAC convention, not the header's
 |       |       +---------- the field that steps: 0006 -> 0008 -> 000A
 |       +------------------ zero
 +-------------------------- 0x07E2, fits the FA message-type family (0x07F0, 0x07A2, 0x07C0, 0x07D2)
```

so the "byte that steps" is plausibly the low half of a **16-bit** field, recorded as a byte only
because no observed value exceeded 255.

**This is the same failure mode as the MAC system number, hit twice in one day**: a field reads as
8-bit until a value crosses 255. Nodes 100 and 102 concealed it there; a user with fewer than 256
files conceals it here. Treat every byte-width claim about file numbers, handles and counters in the
FA traffic as unverified until a value above 255 has been observed.

**The experiment**, on the live machines:

```
@USER-STATISTICS                       -> read MAXIMUM NUMBER OF FILES
@GIVE-OBJECT-BLOCKS (SYSTEM)           -> raise the limit past 256   (user SYSTEM only)
   create files until one is allocated a number above 255
   then access THAT file remotely from 102, with capture running
```

Outcomes:
 - a second byte moves -> the field is 16-bit, and the trailer decode, the server, and any sidecar
   design all change.
 - nothing moves -> it really is 8 bits, and the 4096 limit is reached some other way (a
   block-select field elsewhere?), which is itself important and currently unexplained.

Either result is worth having before a line of server code is written.

**7.2 The FA read/write path.**
Capture a remote read of a known file and a remote write, both against node 100, and decode the
opaque trailer that `*FA-SERVER` appends. This is the largest undecoded piece and it gates section
5.3 steps 4 and 5.

**7.3 Parity in stored source files.**
Two ways, do both because they cross-check:
 - extract a known `:SYMB` file from a pack with `ndtool` and look at bit 7 of the text bytes;
 - transfer a file with known content to node 100 and read the resulting bytes back out of the pack.
If the stored bytes have bit 7 set, the sync app must set it on transfer and strip it on the way
back. If not, it must do neither.

**7.4 Legal SINTRAN names.**
Confirm the name length limit and character set from the SINTRAN reference manual rather than from
examples, so the name mapping in 6.2 is bounded by documentation and not by the handful of names we
happen to have seen.

---

## 7A. Access rights - AFTER the RetroFS move, not before

**Sequencing decision (user, 2026-08-01): the file server must enforce the full SINTRAN access
model, and that enforcement is to be integrated with RetroFS's access model - but only AFTER the
RetroFS move.** Until then the server does the minimum that keeps it honest, and no access code is
written that would have to be unpicked when the move happens.

### What SINTRAN's model actually is [VERIFIED, observed live 2026-08-01]

`@USER-STATISTICS` on a freshly created user reports three access sets and five rights:

```
USER 8 : PACK-ONE:BIGMAN
              DEFAULT PUBLIC ACCESS : NONE
              DEFAULT FRIEND ACCESS : READ, WRITE, APPEND, COMMON, DIRECTORY
              DEFAULT OWN ACCESS    : READ, WRITE, APPEND, COMMON, DIRECTORY
```

 - Three scopes: **public** (anyone), **friend** (a named relationship, see `UserFriend.cs` in
   `RetroFS.NDFS\Elements`), and **own** (the owning user).
 - Five rights: **READ, WRITE, APPEND, COMMON, DIRECTORY**.
 - These are the *defaults applied to newly created files*; individual files carry their own access.

Enforcement is real and was hit during this session: creating a file under `(BIGMAN)` while logged
in as `SYSTEM` failed with **`NOT DIRECTORY ACCESS`**, because SYSTEM is neither BIGMAN nor a friend
and public access is NONE. That is a useful negative test vector - a correct server must produce the
same refusal.

Note the consequence for remote access: a remote client authenticates **as a user**
(`d100(bigman(password)).`), and the manual is explicit that *"you now have the same file access
rights as the remote user specified"*. So the server's access decision is made against the
authenticated remote user, not against the connection.

### Before the move - the minimum

- Authenticate the remote user (the password fold is already carved) and record WHICH user a request
  is acting as.
- Refuse anything the authenticated user would not be allowed, using the SINTRAN error the real
  system returns - `NOT DIRECTORY ACCESS` and friends - rather than a generic failure.
- Do **not** build a parallel permission engine. Keep the check behind one interface with a single
  implementation, so the move swaps the implementation and touches nothing else.

### After the move - integration with RetroFS

`RetroFS.Core` already has `ISecurityContext`, and there is a `RetroFS.Security` project plus
`RetroFS.Security.Implementation` and `RetroFS.Security.Firewall`. The integration work, deliberately
deferred:

1. Map the three SINTRAN scopes and five rights onto the RetroFS security model, and write down
   what does NOT map. A right with no counterpart is a design decision, not an oversight to paper
   over - `COMMON` in particular has no obvious modern equivalent.
2. Make the COSMOS server's access decision go through `ISecurityContext` rather than its own check,
   so one model governs every protocol RetroFS serves instead of each one having its own.
3. Carry the SINTRAN friend relationship (`UserFriend`) into whatever RetroFS uses, or state
   explicitly that it is not represented and what is lost.
4. Re-run the negative vectors from the "before the move" phase unchanged. If integration changes an
   answer that SINTRAN gives, the integration is wrong - the vectors are the contract.

**Why this order.** Writing access enforcement now and integrating later means writing it twice and
risking two models that disagree, which is the worst outcome: a server that permits something the
filesystem would refuse, or the reverse. Deferring costs nothing as long as the check sits behind one
interface from the start, which is what the "before the move" bullet requires.

---

## 8. Staging

| Stage | Deliverable | Gated on |
|---|---|---|
| 1 | `EthernetLink : ILink`, D9999 joins a segment | done: framing + link layer are green |
| 2 | In-process client/server harness (section 4.1) | stage 1 |
| 3 | XROUT registration + XSLET accept, with vector tests | stage 2 |
| 4 | Directory listing over a real NDFS volume | stage 3; needs 7.1 |
| 5 | Stat / open / close | stage 4 |
| 6 | Read / write | **7.2** |
| 7 | Folder sync application | **7.3**, 7.4, and stage 6 |

Stages 1-5 are specified by evidence we already hold. Stages 6 and 7 are gated on experiments, and
that gating is the point - they are the two places where guessing would produce something that looks
right and is wrong.

---

## 9. Related

- [PLAN-CSHARP-ETHERNET-AND-ROUTE-THROUGH-2026-08-01.md](PLAN-CSHARP-ETHERNET-AND-ROUTE-THROUGH-2026-08-01.md) - the transport
- [XMSG-RETROFS-MIGRATION-PLAN-2026-07-29.md](XMSG-RETROFS-MIGRATION-PLAN-2026-07-29.md) - object entries, the RetroFS backend question, the `ToBytes` bug
- [XMSG-SERVER-NAMES-AND-LETTERS.md](XMSG-SERVER-NAMES-AND-LETTERS.md) - how to register a named server
- [XMSG-XSLET-ACCEPT-VS-XRNRO-2026-07-28.md](XMSG-XSLET-ACCEPT-VS-XRNRO-2026-07-28.md) - the accept form, and the XFRTN crash
- [COSMOS-ETHERNET-TRANSPORT-FRAMING-2026-08-01.md](COSMOS-ETHERNET-TRANSPORT-FRAMING-2026-08-01.md) - the transport it all rides on
