# NDCHAT - the plan from here

**Next:** mount `dist\NDCHAT-INSTALL-2026-09-02.img` on a lab machine once (a line in the
RetroCore console window: `mount floppy0 0 <path>` or `attach fd0 <path>`), do the SINTRAN side
from the admin manual section 3.1, and remove its UNVERIFIED mark. Everything else from the
2026-09-02 manuals round is built, deployed on all three machines and committed (`744b2b4c`);
see `HANDOFF-2026-09-02-MANUALS.md`. No sync daemon is running - start one with
`tools\start-relay.ps1` and expect to need the wake trick.

The manuals live in `DOC/manuals/` - user, admin, build-and-deploy - and the rule for them is
the spec's: change them in the same commit as the behaviour.

## Found on 2026-09-02 while photographing the screens - small, all cosmetic, all real

1. **The client does not repaint its own name after `/nick`.** The top-right corner keeps the
   login name (`TESTER@FJELL` after `/nick OLAV`), and the "sent" half of a private window is
   labelled with it too (`*(SYSTEM)` for a user who is KARI). The other end sees the right name;
   it is the client's own copy used for display that is stale. One variable, one repaint.
2. **The header shows `LOBBY@trunk` for a plain `LOBBY`** once a topic is set - the topic's
   name field arrives qualified and the header prints it as the room. Where the `@trunk` comes
   from has not been read out of the source yet.
3. **A topic set on one machine did not reach the other.** OLAV's `/topic` on FJELL showed on
   FJELL only; KARI's header on VIDDA never changed and no `topic:` notice arrived there.
   Whether topics are meant to cross a trunk is not written down anywhere - decide, then either
   carry it or say so in the manual.
4. **`/leave` on a screen terminal shows nothing.** The `left the room` line goes to the line
   renderer; the header keeps the room name and the count. The others do see the leave.
5. **`/list` counts only local seats.** `LOBBY(1)` with two people in LOBBY across two machines.
   Documented as-is in the user manual; a cross-machine count would need the peer tables.
6. **`STATUS` printed `empty o45`** - `putNumber` stopped at three digits and a count of 6345 wrote
   its hundreds as a letter. FIXED in the source (five digits), not yet built or deployed.
7. **The build stamps were never bumped** - `S31-0854` / `B31-0731` across four builds. Bumped
   by hand 2026-09-02 (`S02-1340` / `B02-1340`) and written into the build manual as a rule. A
   check that cannot be forgotten would be better: the linter could refuse a stamp older than
   the source's last change, or the stage step could write it.
8. **`planc-lint.py` treats every file on its command line as one link set**, so CHAT + CHATSV
   together draw a false `inBuf` collision. Either lint per link set (the manual says so now)
   or teach it a `--link-set` boundary.
9. **`tools\nd-deploy.ps1` stages into `sync-out`** while `start-relay.ps1` watches `sync-relay`;
   its no-daemon fallback stops every running RetroCore. Do not use it until both are fixed.

## Small items left over from before

10. Side bug seen once: the server stored "ello123" for a typed "hello123" (first character lost,
    one occurrence, not reproduced).
11. D100's SYSTEM file table sits near its 256-entry cap; expect ATTEMPT TO CREATE TOO MANY FILES
    on any build that creates new names. Delete scratch listings when it happens.
12. The server should CLOSE its history files on the way down, since @ABORT does not, so a
    repeated edit-load cycle stops stranding CHATH<n> locks (`held 0` in STATUS). Not required
    for correct operation; it keeps the dev loop from needing a boot.

---

## Outstanding, 1 to n

### CHATXMS - the second library (Ronny: one sitting, do not begin unattended)

Two libraries, not one - decided. CHATLIB stays pure so `CHATTST` keeps its 17-second,
XMSG-free, machine-free test loop; CHATXMS holds the kernel calls only. `CHAT` and `CHATSV` link
both. The blocker (`xrAddrOf` / `ADDR` of a `BYTES` parameter) is settled - measured
`ADDR param 1001 / ADDR here 1001`. Size: `CHAT.PLNC` 14 call sites, `CHATSV.PLNC` 21, two
addressing modes (by name, by magic). Interface already designed -
[CHATXMS-INTERFACE-DESIGN.md](CHATXMS-INTERFACE-DESIGN.md) - seven routines, nothing compiled yet.

1. Create `CHATXMS.PLNC`. `EXPORT` block FIRST, before any declaration - R119; skipping this once
   cost ten errors that still linked and still passed 139/139.
2. `xsSendM` - shape A, send by magic.
3. `xsSendN` - shape B, send by name through XROUT.
4. `xsRecv` - shape C. **Do this one FIRST** - it carries the clamp and the release, and a mistake
   here corrupts memory rather than failing loudly.
5. Rewire `CHAT.PLNC` - 14 sites.
6. Rewire `CHATSV.PLNC` - 21 sites.
7. Client to `XMPSEND`, dropping `XMPFSND` - approved. One site in `CHAT.PLNC`; the server already
   uses `XMPSEND`.
8. Check every new export name is unique **at 7 characters** - the linker resolves a collision to
   whichever it met first rather than calling it a duplicate. `planc-lint.py` refuses them.
9. Gate the LISTING of **every** module touched, not only the one edited.
10. Deploy to all three machines and prove it by the **START ADDRESS** in `LIST-RT-DESCRIPTION`,
    not by the fact it ran.

**Why one sitting.** 35 sites across two sources of about 300KB each, roughly twenty-minute
compiles, three machines running the product. A half-refactored `CHATSV` is worse than an
unstarted one.

### Latency - the 500ms target is still missed

11. **Send fewer letters per message.** L.1f (client idle-sleep tuning) is built, deployed and
    measured: mean came down from 683ms to 592ms, floor from 514ms to 483ms. It does not reach
    500ms and cannot - the remaining ~400ms is the XMSG round trip itself (measured: a bare local
    client command floors at 112ms against a say floor of 514ms). No amount of sleep tuning
    reaches that; the lever is a wire/design change that sends less. Not designed yet.
12. **XMSG send window PARKS under a big transfer.** Pulling the 467KB `CHATSV:LIST` failed once:
    `*** PARKED *** send window is full; 14 datagram(s) now waiting`, peer re-sending because it
    had not seen our acknowledgement. Our receive/ack latency under load, not an ordering race and
    not a timeout - same family as the blocked-queue bug fixed 2026-08-27, different trigger. The
    identical retry worked; no permanent fix yet. `-TransferTimeout` also defaults to 240s, which
    is short for a listing this size and made the first failure look like something it wasn't.
13. **Verify the doorbell's restart flag actually clears.** The code is deployed on all three
    machines right now (`sleepWhy`, `waitFlags`, the 20-consecutive-empty-wake disarm). Not yet
    observed live: if the flag is sticky, every later sleep returns instantly and the server
    spins. Read the server's own log for `SV doorbell off` during an idle stretch - present means
    it disarmed correctly, absent after enough idle time means sticky and this needs a real fix.

### Backlog - XMSG plumbing, Ronny's order: LOW priority, chat is the product

14. **Ethernet send sequence is not persisted.** HDLC keeps `xmsg-link-seed.state`; Ethernet keeps
    nothing, so a daemon restart begins at 0 while the peer is mid-run and every frame is silently
    discarded. The `NdLinkLayer` fix (2026-08-27) stops the consequence - one stuck frame blocking
    the queue forever - and does not close this.
15. **Re-install the D103 boot file into its disk image.** The repo copy
    `boot/XMSG-STARTEX-L03.D103.txt` was corrected 2026-08-28; `boot/install-boot-files.py` has not
    been run since, so the copy on the machine still carries the old, wrong comment. Comment-only,
    changes no behaviour.
16. **Optional: persist name/trunks server-side instead of via `rt-load.ps1`.** Reverses a
    deliberate design decision and needs a file format, a compile and a deploy. `rt-load.ps1`
    (R.1) already does this from the Windows side and is proven on all three machines - only worth
    doing if that ever proves not enough. No evidence of that yet.

---

## WHERE THE MACHINES ARE, 2026-08-31

All three run the SAME object: segment **201B**, `CHATSV:BRF`, names FJELL / VIDDA / SKOGEN, both
trunks up on each. Built on D100, carried machine-to-machine; D102 and D103 also hold the current
`CHAT-MON:PROG`. Build files: **`CHATSV:MODE`, `CHATMN:MODE`, `CHATCC:MODE`** - `CHATUI:MODE`
builds the screen TEST program, not the client.

---

## Deliberately NOT doing

- **No comment-reformatting sweep of the C#.** Known, mid-sentence tags and collapsed doc comments
  included - `retrocore-csharp` section 32 forbids standalone reformatting passes through working
  code. Fixed when the file is next edited for a real reason, not before.
- **No general window manager.** The chat windows do not overlap - they are alternative contents
  for one rectangle. `TESTUI` already showed what overlapping costs: clipping, a stack order, and
  three places that must agree on it.
- **7.2, the separator style - won't-fix.** `fullbar` (hyphens) and `frame` (graphic line-drawing)
  are both library calls; `fullbar` only ever runs after `frame` has already proven the terminal
  draws graphics correctly on that same screen, so its portability is never actually exercised
  here. Matching them exactly needs a hand-written escape - the one hardcoded, non-VTM-translated
  line in an otherwise fully portable client - for a cosmetic gain. Hyphens stay.

---

## The rules this plan is written under

- **A green test suite can sit on a failed compile.** Ten `*** ERROR` lines once still linked, ran
  and passed 139/139. Gate every module's listing.
- **Every build failure becomes a linter check**, in the same turn, proved to fail on the bad case
  and pass on every real source.
- **A duplicate PLANC routine name cascades hundreds of false diagnostics** through the rest of the
  compile, not just one clean error at the header. `planc-lint.py` now catches it.
- **An RT-load orphans every joined client** and wipes the machine name and the trunks.
  `rt-load.ps1 -AndStart` now prints the reminder itself. Quit and restart the clients.
- **Read the machine, not the screen** - `LIST-NAMES` for free seats, `LIST-RT-DESCRIPTION` for
  the start address, `FILE-STATISTICS` for what actually arrived.
