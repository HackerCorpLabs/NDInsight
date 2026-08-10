# Plan: D19999 as a real COSMOS file and terminal service

**Created:** 2026-08-04
**Goal:** D100 and D19999 exchange files in BOTH directions, for TWO users, plus routing info and a
TAD menu.

This is a FORWARD plan. It lists what comes next, not what is done. For what already works see
`COSMOS-ETHERNET-LINK-CONTROL-FRAMES-2026-08-03.md`.

---

## The target, in the user's words

1. D100 can `list-files` from D19999 for user **SYSTEM** and for user **RONNY**, each showing a few
   files.
2. User SYSTEM on D100 can COPY those files down to a local folder on D100.
3. D100 can copy a few files UP to SYSTEM and to RONNY on D19999.
4. `X-C list-rou` via D19999 returns its routing information.
5. `conn-to D19999` reaches a TAD server showing a menu of activities.
6. A client running ON D19999 pushes files from the Windows folder for user RONNY UP to a new user
   RONNY on D100.

---

## What blocks all of it today

> **STATUS 2026-08-04 (end of day).** The premise below was WRONG and phase 0 is DONE. A test of
> the full runner composition showed the reachability path was already correct; what was actually
> broken were two other things (a dropped reply to the peer's first datagram, and an
> `InitialSequence` of 1 where D100 expects 0). Both fixed.
>
> Since then, over **HDLC** (see the transport A/B test below), the FA file server has been driven
> from a real D100 all the way through connect, reserve, and a two-entry directory walk. Three
> protocol defects were found and fixed on the way - they are written up in
> [FA-EXCHANGE-MODEL-AND-DIRECTORY-WALK-2026-08-04.md](FA-EXCHANGE-MODEL-AND-DIRECTORY-WALK-2026-08-04.md):
>
> 1. a request is answered by a ShortAck, and the reply is the NEXT exchange (an XENSE reject)
> 2. the directory cursor is not an index; the SERVER holds the walk position
> 3. `0x078x` means "finished" and wants a Close, not a refusal
>
> **The transport A/B test also proved the Ethernet path has a bug of its own.** With the identical
> upper stack, HDLC reaches the directory walk while Ethernet is torn down (`kind 0x60`) right
> after the connect confirm, never reaching a single file-access request. So D19999-over-Ethernet
> is blocked on `NdLinkLayer` / `EthernetLink` or the emulated card, NOT on anything in XMSG or FA.
> That is the next thing to chase.
>
> Two harness bugs also cost real time and are fixed: `ndterm`'s login raced on a busy machine
> (fixed delays, now waits for `ENTER` / `PASSWORD:`), and the runner blocked ~1s per log line
> writing to a hidden console with no reader (start it with `-RedirectStandardOutput`).

**Phase 0 must land first.** D100 sends `sub=ReachabilityRequest` over the Ethernet link and we
never answer, so it aborts with `NO ANSWER FROM REMOTE SYSTEM` before any service is reached.
Nothing in phases 1-6 can be tested until that reply goes out.

---

## PHASE 0 - Answer reachability (blocking, small)

- [ ] Find why the Ethernet path does not answer `ReachabilityRequest`. A handler EXISTS at
      `SRC/Xmsg.Node/XmsgNode.cs` around line 520 (`BuildReachabilityReply`), and
      `SRC/Xmsg.Live/Seam/EthernetLink.cs` line 376 raises `PayloadReceived`. **SUSPECTED, NOT
      CONFIRMED:** the runner subscribes the HDLC seam path into `XmsgNode` but not the Ethernet
      path, so Ethernet datagrams are logged and dropped. Confirm before changing anything.
- [ ] Wire it, and log every inbound datagram with its subtype AND whether a reply was produced, so
      "received but unanswered" is never silent again.
- [ ] Test: `li-fi D19999(sys).,,` from D100 gets past reachability and reaches the FA server.

**Watch out:** a node that accepts a link and does not answer HANGS the calling SINTRAN terminal -
ESC will not abort it. Every request must produce a reply, even a refusal.

---

## PHASE 1 - Two users, two folders (target 1)

The file server today serves ONE folder with no notion of a user.

- [ ] Give `FolderFileStore` a per-user root: `served-files/SYSTEM`, `served-files/RONNY`.
- [ ] Extend the `fileServer` block in `SRC/Xmsg.Live.Runner/topology-d19999.json` from a single
      `root` to a user-to-folder map. Keep the existing behaviour when only `root` is given.
- [ ] Decide what a request WITHOUT a user means (probably the caller's own name).
- [ ] Make the synthesised directory entry's owner field follow the user actually being served -
      today `userIndex` is one fixed number for everything.
- [ ] Test: `li-fi D19999(sys).,,` and `li-fi D19999(ronny).,,` from D100 return different lists.

**Open question, must be answered from a capture, not guessed:** how the FA request carries the
target USER. `FaFileName` handles the `(user)name:type` shape, but whether the wire request names
the user in the filename field or in a separate field is NOT established here.

---

## PHASE 2 - Read a file: D100 pulls from D19999 (target 2)

**This is the largest undecoded piece in the whole plan.**

- [ ] Capture a real file READ between D100 and D102. `FaOperation.ReadFile` (0x0008) says in its
      own docs: *"Never recorded. Request and reply layouts are UNKNOWN."* The same holds for open,
      close, create and delete beyond their opening frames.
      Method that already works: the ENCOS monitor on D100 (`trac` / `0` / `E` / `N`, provoke,
      `trac` / `0` / `R` / `N`) plus our own frame sniffing on the segment.
- [ ] Decode the request and reply layouts and write them up with the same evidence discipline as
      the link-control document.
- [ ] Implement read in `FaServerConversation` + `FolderFileStore`, which already has a `Read`.
- [ ] Decide and document how a Windows file's bytes map to ND records - block size, padding,
      whether a text file needs line-ending translation.
- [ ] Test: `@COPY-FILE` on D100 from `D19999(sys)file` to a local file, then compare bytes.

**Do NOT implement read against a guessed layout.** A wrong read silently corrupts data and looks
like it works.

---

## PHASE 3 - Write a file: D100 pushes to D19999 (target 3)

- [ ] Same capture work for WRITE (`FaOperation.WriteFile`, 0x0009), CREATE and DELETE.
- [ ] Implement write, create and delete on top of `FolderFileStore`.
- [ ] Decide the safety rules and make them explicit config, not assumptions: may a remote node
      overwrite? create? delete? outside the served folder (path traversal)? Default should be the
      cautious one.
- [ ] Test: copy a file from D100 to `D19999(sys)` and to `D19999(ronny)`, verify it appears in the
      right Windows folder with the right bytes.

---

## PHASE 4 - Routing information (target 4)

- [ ] Make `X-C list-rou` against D19999 return our routing table. Some of this exists - the node
      answers a list-route (XSGSY) request from the routing table, and an earlier session recorded
      that the reply echoes the request counter and flags and comes back as TWO datagrams.
- [ ] Populate the routing answer from `topology-d19999.json` rather than a fixed reply, so D103
      via D100 is described correctly.
- [ ] Test: `X-C:li-rou` from D100 naming D19999 shows a sensible path, and no longer says
      "no access to system 19999".

---

## PHASE 5 - TAD menu (target 5)

- [ ] `conn-to D19999` must reach the TAD server. A TAD server and client already exist in
      `SRC/Xmsg.Servers/Tad/` and `SRC/Xmsg.Node/Tad/`, and `topology-d19999.json` already carries
      `tadUsers` for SYSTEM and RONNY.
- [ ] Build the activity menu itself: list files, show status, echo test, disconnect.
- [ ] Respect the TAD rules already learned the hard way: output under 255 bytes per write, and the
      five-frame disconnect ladder.
- [ ] Test: `conn-to D19999` from a D100 terminal shows the menu and each entry works.

---

## PHASE 6 - D19999 pushes to D100 (target 6)

This is the reverse direction and needs a CLIENT on our side, not a server.

- [ ] Create user RONNY on D100 (`@CREATE-USER`), with a directory and space.
- [ ] Use the existing `FaClientConversation` to drive a file transfer from D19999 to D100.
- [ ] Add a small command to the runner: "push folder X to user Y on node Z".
- [ ] Test: files in the Windows folder for RONNY appear under RONNY on D100.

**Depends on phase 3's capture work** - the client needs the same write layout the server does.

---

## What the user did not list but the plan needs

- **User authentication.** D100 will present a user and possibly a password when accessing files.
  The password crosses the wire as a 16-bit fold, never plaintext. Right now the file server serves
  anyone who asks. Decide: accept all, or check against a configured list.
- **Access rights.** SINTRAN files carry owner, friend and public rights. A Windows folder has
  none. Decide what we present and whether we enforce anything on write.
- **One link at a time.** The runner serves the FIRST Ethernet neighbour only, so D102 cannot use
  the file server even though it is on the same segment. Serving both needs a host per link, each
  with its own learned reference and datagram sequence.
- **`SintranHeader.Size` is 13 in the code but 14 per the 2026-08-02 handoff.** If the handoff is
  right there is an off-by-one in every FA body offset. Settle this BEFORE decoding read and write
  layouts, or the capture work will be built on a wrong base.
- **The refusal format is ours, not ND's.** No capture of a COSMOS file server refusing anything
  exists. Every status code we return is invented and marked UNVERIFIED. A real client may not
  accept them.
- **Directory entries are part-synthesised** - owner, page count, header flags and the three ND
  dates are made up. A real SINTRAN client has never been watched reading one, so a listing that
  displays correctly is not proof the entry is right.
- **File name and type mapping.** `HELLO.TXT` on Windows against `HELLO:SYMB` on ND. The rule needs
  to be written down and be reversible, or a round trip will not return the same name.
- **Big files and fragmentation.** A transfer larger than one datagram needs the fragmentation path
  exercised; there is a `TransferFragmentationTests` but not against a real machine.
- **Concurrency.** Two clients at once, or one client while a push runs. Not designed for yet.
- **The idle failure is still unexplained.** Ethernet between D100 and D102 died after idle once and
  has never been reproduced with instrumentation on. It could reappear in the middle of any of this.

---

## Suggested order

Phase 0, then 1, then **2 and 3 together** because they share the capture work and that capture is
the single biggest unknown in the plan. Then 4 and 5, which are independent of the file layouts and
could be done by someone else in parallel. Phase 6 last, since it depends on phase 3.

The honest risk: phases 2, 3 and 6 are gated on decoding a protocol nobody here has captured yet. If
that capture proves hard, phases 0, 1, 4 and 5 still deliver a listing service, routing and a TAD
menu.
