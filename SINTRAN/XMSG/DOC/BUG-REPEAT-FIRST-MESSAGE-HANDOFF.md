# The repeat-first-message bug - handoff, 2026-09-01

**Status: FIXED AND VERIFIED LIVE, 2026-09-01 22:10.** The root cause was the
CLIENT's painter - paintChat handing paintRow a computed-base subarray of the
23KB cache, which painted line 0's bytes on every row while the wire, the
server, the history ring and the cache itself were all correct the whole
time. See "ROOT CAUSE FOUND" below for the evidence chain and the fix, and
the addendum for the test programs built on the way (CHATKT, CHATVT). After
the fix: six distinct lines rendered distinctly, the full history replay
rendered every stored line with its own text and time, and a two-client
session showed ONE join notice and correct text both ways. The temporary
five-number instrument is still on the title row pending a decision to
remove it, and the fix is deployed on D100 only - D102/D103 still run the
broken client.

## Addendum, 2026-09-01 evening - the KERNEL PATH IS PROVEN CLEAN

`SINTRAN-CHAT/CHATKT.PLNC` (new, built and run on D100): a loopback test
that opens one port, sends to its own magic number with the SERVER's exact
send sequence (`xmpfget` / `xmpfwri(0,0,...)` / `xmpsend` with `2**XFSEC`)
and receives with the CLIENT's exact receive sequence (`xmpfrcv` / clamp /
`xmpfrea(0,0,...)` / `xmpfrel`). Every message a different letter AND a
different length. **49/49 PASS**, including the phase that mirrors a server
mid-handling: a received message HELD unreleased while two replies are
built and sent - both replies arrived intact. So:

- `xmpfget`'s fresh buffer IS what `xmpfwri(0,0,...)` writes into, even
  with an arrival held current (the guide never says this; now measured);
- the client-shape receive returns each message's own bytes, not a stale
  current message.

The bug is NOT in the kernel semantics of the call shapes the product
uses. Build/run it again: `@MODE CHATKT:MODE,,` then `@CHATKT` - one
minute, needs only XMSG up on that machine.

Also that evening: D100 was found HUNG (terminals, Ethernet, OPCOM all
silent, emulator process executing) and was cold-booted with Ronny's
permission. The boot files brought the ladder up. SYSTEM's file table hit
its 256-slot cap mid-build (`ATTEMPT TO CREATE TOO MANY FILES` from
`PROGRAM-FILE`) - stale scratch listings CHATL2/3/5/6/8, CHATA3:LIST/BRF
and TMPX:TXT were deleted to free slots. Watch for this on the next build.

## ROOT CAUSE FOUND, 2026-09-01 late evening - THE PAINTER, not the data

The elimination ran: kernel loopback clean (CHATKT 49/49), server send side
clean (CHATVT 48/48), server history ring on disk holds the RIGHT texts with
the RIGHT live timestamps (one/two/three/aaaaaaaa, decoded byte by byte from
a pulled copy of CHATH1:DATA), bug reproduced on a VIRGIN server after a
cold boot AND on a fresh RT-load AND on a second fresh client - so client
side, render half. A five-number instrument on the client's title row
(kind, readLength, the ARRIVAL's first text byte, cacheCount, the newest
CACHE row's first text byte) then said, on a live repeat:

    005 015 097 003 097        (with the screen showing "first")

kind kSaid, the arrival's text byte 'a' (the "alpha" just typed), the cache's
newest row ALSO 'a' - **the wire is right and the cache is right; the PAINT
is wrong.** paintChat handed paintRow

    cache(base : base + lineWide - 1)

a subarray of the 23KB cache with a COMPUTED base - and every row painted
line 0's bytes. The same project rule that killed putField (a computed-bounds
subarray compiles clean and reads/writes the wrong memory) in different
clothes. This paint shape arrived with the no-blanket-clear repaint rework
(commit e402a572, 2026-08-31) - exactly when the bug first appeared. Fix:
the row is copied into a fixed 77-byte module paintBuf with an explicit
indexed loop (an ordinary computed INDEX reads the cache correctly - the
instrument itself proved that) and paintRow reads paintBuf; its inner run
subarrays are now of that small fixed array, the shape TESTUI has proven.

Note for a future linter rule: computed-bounds subarrays are NOT all broken -
whoBuf(0:whoLen-1), roomName(0:len-1) and TESTUI's paintSlot all work. The
broken ones so far both had a large multiplied/offset base into a big array
(putField's w*16, paintChat's slot*77 into 23100 bytes). Do not encode a
lint rule until the discriminating condition is measured.

`SINTRAN-CHAT/CHATVT.PLNC` (new) is the CHATSV-side test this handoff
asked for: EXPORTs were added to CHATSV.PLNC (block right after MODULE,
before the includes), and CHATVT plants three fake members whose magic is
its own test port, then checks buildFromSlot's outBuf per call (part A),
broadcast's audience by counting arrivals (part B), and three
build+broadcast rounds whose received content must follow the round
(part C). Build: `@MODE CHATSV:MODE,,` first (the exports), then
`@MODE CHATVT:MODE,,`, run `@CHATVT`.

## The symptom, precisely

A client joins, sends a message, sees it correctly. Every message after that -
typed by that client OR by anyone else in the room - renders as a REPEAT of
that first message's text, not the new one. Confirmed on two independent
setups:

- Solo client: typed three different lines one after another; all three showed
  as the FIRST line's text.
- Two clients (SYSTEM/TESTER and ronny): SYSTEM sent "yo" (showed correctly),
  then sent "ghei" - the room showed "yo" again, not "ghei". The OTHER client
  (ronny) saw **three lines of "nick has joined"** - a JOIN notice repeating,
  not chat text. So the repeat is not specific to kSaid; it can repeat
  whatever the FIRST thing shown was, including a system notice.

That last fact matters: it rules out any theory tied to one specific message
KIND. Whatever is wrong repeats "the first thing this client ever rendered",
regardless of what kind of message that was.

## What is PROVEN correct (do not re-suspect these)

Three real, automated, offline tests were written and pass clean on the
live machine - `SINTRAN-CHAT/CHATCTST.PLNC`, 23/23 checks:

1. **The cache itself** - `addSaid`/`cacheAdd`/`slotAt`. Three distinct
   `addSaid` calls with distinct text land in three distinct slots, each
   holding its own text. Not the cache.
2. **`paintChat`'s row-to-line arithmetic** - pulled out into a new, tested,
   exported routine `wantForRow(row)` in CHAT.PLNC. Given three real cache
   entries, row 0/1/2 correctly want lines 0/1/2, further rows correctly want
   nothing. Not the paint math.
3. **The wire-arrival parse path** - built three DIFFERENT synthetic kSaid
   messages with CHATLIB's own proven `cmEnc`, wrote them straight into
   `inBuf`, and called `showArrived` (the REAL entry point `drainPort` calls)
   three times in a row. Three distinct cache entries resulted, correctly.
   Not `uiArrivedInner`/`uiSaid`/`showArrived`.

Run it again with: compile+link `CHATCTST.PLNC` against `CHAT`, `CHATARR`,
`CHATLIB`, `XMP-100-1-B02`, `INTRF1B`, `VTMR`, `VTMDATA`, `VTMARR`,
`MON-CALL-1B-A00`, `PLANC-1BANK-F00` (same libraries as `CHATCC:MODE`, in that
order) - `@CHATCTST:PROG`. No XMSG, no server, no second machine, ~1 minute
total.

## Three real fixes were made and deployed. None of them was THE bug.

All three are correct, deployed, and should stay:

1. **`announceWelcome`** (CHAT.PLNC, exported, called by CHATARR.PLNC) -
   `firstWelcome` used to be an EXPORTed BOOLEAN read back through an IMPORT
   in CHATARR.PLNC, and that branch measurably fired TWICE for one real join
   with both `firstWelcome` and `cacheCount` reading their INITIAL values the
   second time - which a stray message would not do. Moved the whole check
   and the flag it reads into ONE module, called as a ROUTINE (proven
   reliable elsewhere in this file) instead of shared as DATA across the
   module boundary. This did not fix the live symptom, but it is real and it
   removed a genuinely suspicious cross-module data-sharing pattern.
2. **`readStatus`** (CHAT.PLNC `drainPort`, CHATSV.PLNC `chatServer`) -
   `xmpfrea`'s own return status was being overwritten by the very next
   statement (`xmpfrel(...) =: returnStatus`) before anything checked it, so
   a failed read's stale `inBuf` would be parsed as if it were new. Both
   files now capture the read's own status and skip the parse if it is not
   `XMOK`. Correct defensive coding. Did not fix the live symptom.
3. **`XMPSEND` not `XMPFSND`** (CHAT.PLNC `sendToServer`) - this was
   ALREADY a known, documented, unfixed plan item (5.7) from before this
   session: `XMPFSND` sends whatever message happens to be "port current" or
   "task current" on the XMSG kernel side, not necessarily the one just
   built. This looked like a very strong match for "always resends the
   first thing" and was fixed to `xmpsend(0, ident, myPort, serverMagic)`,
   matching the signature CHATSV.PLNC already uses correctly. **Deployed and
   tested live - the symptom is unchanged.** This was the most promising
   lead of the session and it was not it, or not all of it.

## What is still unproven / not yet checked

- **CHATSV.PLNC's own send/broadcast path was NOT independently offline
  tested the way the client's cache/paint/parse were.** `buildFromSlot` and
  `broadcast` were read carefully and look correct, and `sendTo` was already
  fixed to use `XMPSEND` in an earlier session - but nothing exercised them
  with synthetic distinct inputs the way CHATCTST.PLNC exercises the client.
  A `CHATCTST`-style test that calls `buildFromSlot`+`broadcast` (or a
  narrower slice of them) with three distinct fake members/messages and
  inspects the resulting `outBuf` per call would close this gap the same way
  the client side was closed.
- **The fact that a JOIN notice repeated on the OTHER client (ronny saw three
  "nick has joined")** was reported in the interrupted final message and NOT
  yet investigated. It is the single most useful new fact: it means whatever
  is wrong is not specific to `kSay`/`kSaid` at all, and it happened on the
  RECEIVING side of a DIFFERENT client than the one sending. That points
  either at `broadcast()` sending the same built message to the wrong seats
  repeatedly, or at something in `CHATSV.PLNC`'s SEND path (not receive) that
  was not part of this session's `readStatus`/`XMPSEND` fixes.
- **`CHATSV:PROG` was rebuilt with the `readStatus` fix and RT-loaded onto
  segment 204B** (`rt-load.ps1 -Port 9010 -Segment 204 -AndStart`) earlier in
  this session, confirmed alive with `FJELL` name and both trunks. It has
  NOT been rebuilt again since - it does not yet carry any fix beyond
  `readStatus`. Client-side `CHAT:PROG` DOES carry the `XMPSEND` fix
  (confirmed via `FILE-STATISTICS`, linked 22:35, read 22:37, matching the
  failed test).
- **Never confirmed**: does the bug reproduce with only ONE client ever
  having existed in the room's whole history (a truly virgin server, seat
  0 taken exactly once, ever)? Every live test this session ran against a
  server that had already seen several joins/rebuilds/orphaned clients in
  its lifetime. A clean RT-load immediately followed by exactly one join and
  one send, with nothing else touching the server first, has not been tried.

## Tooling / environment notes for whoever continues this

- **The sync daemon's window is 6 hours (21600s) and it does not restart
  itself.** It died silently partway through this session (`start-relay.ps1`
  again) - check `sync-relay.log` for `still waiting for 100` before
  assuming a push failed for a code reason.
- **A stuck link needs the wake trick**: `CREATE-FILE WAKE:TEXT,1` then
  `COPY-FILE WAKE:TEXT,D19999(SYSTEM).WAKE:TEXT` on the machine - the copy
  FAILS (`NO SUCH FILE NAME`) and that is fine, the point is the frame it
  sends.
- **`CHAT:LIST` and `CHATSV:LIST` repeatedly hit `FILE ALREADY OPENED FOR
  WRITE BY ANOTHER USER` and `AMBIGUOUS FILE NAME`** across this session, for
  reasons never identified (not this repo's own daemon or admin session, as
  far as could be told). The workaround used throughout: compile to a
  never-used listing name each time (`CHATL3:LIST`, `CHATL4:LIST`,
  `CHATL5:LIST`... `CHATL8:LIST` were used, next is `CHATL9:LIST` or start
  numbering fresh) - the OBJECT name (third `COMPILE` argument) stays `CHAT`/
  `CHATSV` regardless, so this is always safe and never affects what gets
  linked.
- **CHATARR.PLNC must be compiled in its OWN, separate `PLANC-100-F00`
  session from CHAT.PLNC** (`EXIT` and re-enter the compiler between them) -
  compiling both in one session has previously produced a SPARSE
  `CHATARR:LIST` that looks fine locally but is corrupt on the machine. This
  bit this session once; the isolated-session compile is the fix and was
  used throughout after that.
- **`SINTRAN-CHAT/CHATCTST.PLNC` is a permanent, valuable test file** and
  should stay in the repo (it currently sits alongside `CHATTST.PLNC`, which
  tests the codec; `CHATCTST.PLNC` tests the cache/paint/arrival-parse). It
  is not committed to git yet as of this handoff.
- **`SINTRAN-CHAT/CHATXMS.PLNC`** (from a session before this one) still
  exists, still compiles clean, and is still not linked into anything. Not
  touched this session. Unrelated to this bug as far as is known.

## Recommended next step

Write the CHATSV-side equivalent of `CHATCTST.PLNC` - a small offline test
that builds two or three distinct synthetic "members" with distinct names/
rooms in the member table, calls `buildFromSlot`/`broadcast` for each, and
inspects `outBuf` after each call for distinct content, the way the client
side was proven. If THAT fails, the bug is server-side send, not client-side
receive, and has never actually been tested at all this session. If it
passes too, the next thing worth trying is the "never confirmed" scenario
above - a genuinely virgin server, exactly one join, exactly one send,
nothing else touching it first - since every live test this session ran
against a server with prior history.
