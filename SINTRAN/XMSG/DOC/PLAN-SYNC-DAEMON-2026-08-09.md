# Plan - the folder-watch sync daemon (task #33)

Full path: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\PLAN-SYNC-DAEMON-2026-08-09.md`

**Goal.** Edit ASM / PLANC / NPL in VS Code on Windows. The file lands on a SINTRAN machine,
is compiled there, and the listing appears back in the local folder as fast as possible.

Forward-only. Everything below is work not yet done.

---

> **This extends section 6 of `PLAN-CSHARP-FILE-SERVER-AND-FOLDER-SYNC-2026-08-01.md`**, which
> already set out the folder-sync application. Read that first. This document exists to record
> what has been SETTLED since, and to correct one thing in it. Do not treat them as rival plans.
>
> **Settled since:** section 6.3 of that plan says parity is "UNKNOWN and must be settled
> first". It is now measured - `SINTRAN-FILE-PARITY-BIT-MEASURED-2026-08-09.md`. Bit 7 is even
> parity, and a per-extension rule cannot work because one file carries both conventions.

---

## The decision that gates everything: how does a file get TO the machine?

**CORRECTED 2026-08-09, later the same day.** The first version of this section ranked
"batch job that pulls from us" as the recommended route, on the grounds that the only proven
transfer was SINTRAN reading from our server. That was wrong, and it was wrong because I had
not looked at `DOC/captures/FA-READ-WRITE-2026-08-04/capture-write.txt`.

That capture is node 102 writing to node 100 - **a real client writing to a real server**, 422
frames of it. So the push direction is recorded, from the client's side, in the exact shape we
would have to send. The 2026-08-01 plan's section 6.1 already said "push, not serve", and it
was right.

The four candidates, ranked by what the evidence actually supports:

### A. We act as an FA CLIENT and write to the machine - RECORDED, and the right answer

`capture-write.txt` holds a real client's write, request by request, against a real server. We
already implement the SERVER side of open / write / close, so building the client is mirroring
code we have against bytes we have.

**What is missing is only the driver.** There is an `FaClientConversation` that builds requests
at the protocol level, and no higher-level client that runs a whole session. That is the single
biggest piece of work in this task, and it is ordinary work - not a research question.

The file lands on the machine's own pack, so `NC`, `PLANC` and the linker see an ordinary local
file with no server in the path. Nothing has to be typed on the SINTRAN side.

### B. We serve, SINTRAN reads - PROVEN but needs a human

Our `*FA-SERVER` is live and a `COPY-FILE` typed on SINTRAN reads a file out of us. Useful as a
fallback and for anything the client cannot yet express, but something must issue the command on
the machine, which defeats an automatic loop.

### C. XFTRA letter - NOT PROVEN, stays shelved

`XftraRequests` builds a `TRANSFER-FILE` letter and it is byte-checked against the capture.
But `DOC/XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md` records that the capture
**failed at routing**, so *what a real `*XFTRA` does after receiving the letter was never
observed*. We have the request and no reply.

**Do not build on this until a live exchange has been captured.** It is one letter, so it
looks cheap, and that is exactly the trap - we would be guessing at the follow-on.

### D. Batch job that pulls from us - useful for the BUILD, not the transfer

`APPEND-REMOTE-BATCH` tells SINTRAN to run a batch job. That is how the compile gets triggered
once the file is there, and it is task **#30**. It is not needed to move the file, now that A is
the route.

**Order: A for the transfer, D for the build trigger, B as the manual fallback, C shelved.**

---

## Phase 1 - the transport-agnostic core (needs no machine)

Buildable now, and correct whichever mechanism wins.

 1. `Xmsg.Sync` library with a `IFileTransport` seam - push, pull, list. The engine never
    names FA, XFTRA or batch.
 2. **Settle detection.** `FileSystemWatcher` fires while the editor is still writing, misses
    events under load, and drops everything after a buffer overflow. So: treat every event as a
    hint, record size and last-write time, and only act once they have stopped changing for a
    configured quiet period. Plus a periodic full rescan to catch what the events lost.
 3. **Content hashing** so a save with no edit costs nothing, and so the return direction can
    tell "the machine changed this" from "this is the file we just pushed".
 4. **Echo suppression.** A file we push comes back as a change. Track direction and ignore it,
    or the daemon fights itself.
 5. Config: watched folder, machine, SINTRAN user, remote directory, include/exclude globs,
    several independent pairs.

All of this is pure logic and testable with no timers and no I/O - the settle rule takes
(size, last-write, now) and answers "settled or not". Build it that way.

## Phase 2 - transfer, once #30 lands

 6. Wire `IFileTransport` to mechanism C.
 7. Parity bit per extension (SYMB, LIST, ...). **VERIFY what SINTRAN wants per type first** -
    grep `Reference-Manuals/` before writing a table. This is exactly the kind of fact a manual
    already states.
 8. Fire the batch after a push, configurable per watch.

## Phase 3 - the return leg

 9. Bring back listings, object files and errors. Separate filter from the outbound direction.
10. Change detection on the SINTRAN side: work out from a REAL listing whether `LIST-FILES`
    gives a timestamp or size usable for comparison (`fa-listing-record-fields`). If nothing
    usable is there, say so and fall back to hashing. Do not invent a field.
11. Surface batch output into the local folder so a compile error appears in VS Code.

---

## Traps already known

 - **`FileSystemWatcher` alone is not a design.** Buffer overflow silently drops every pending
   event; the rescan is not optional.
 - **A file is reported before it is finished.** Without settle detection we will push half a
   file and compile garbage.
 - **The echo loop.** Push -> remote changes -> pull -> local changes -> push.
 - **SINTRAN quoting differs per command.** `COPY-FILE` wants quotes, `CREATE-FILE` and
   `RENAME-FILE` do not. Getting this wrong looks like a transfer failure.
 - **Packed ND dates only span 1950-2013**, so a present-day timestamp encodes as 0. If the
   return-leg change detection leans on a date, it will compare zeros.

## Related

 - Task #30 - drive APPEND-REMOTE-BATCH live. Gates phase 2.
 - `DOC/captures/FA-READ-WRITE-2026-08-04/` - the proven read and write path.
 - `DOC/captures/ARCHIVE-2026-07/` - the transfer and batch recordings.
