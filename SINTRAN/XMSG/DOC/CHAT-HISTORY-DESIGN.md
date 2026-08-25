# Chat history: a circular buffer on disk, shared by the server and the client

**Asked for by Ronny, 2026-08-20.** A server that keeps the last N messages so somebody joining sees
what was just said, a client that keeps its own copy so it can redraw after a restart, and a way to
ask for "everything after the message I last saw".

This is the analysis before the code. Two decisions in here are load-bearing and cheap now.

---

## 1. The anchor: machine!id, and what it does and does not promise

**Ronny, 2026-08-20, correcting an earlier reading here:** the request is not "give me everything
after sequence 500". It is *"give me everything after THIS ID"*, where the id is always a
`machine!counter` pair. The id is an **anchor** - it names one specific message - and the answer is
everything that comes after it. And he named the risk himself: a message posted on one machine can
land in another machine's backlog at a different position, because distribution is delayed.

That is the right shape, and it is worth being precise about what it buys.

### The log is APPEND-ONLY, and that is what saves it

A ring is written in arrival order and never inserts. So a message that arrives late is appended at
the **end**, not spliced into the middle. Which means:

> **Anchored sync misses nothing.** Anything a machine has not yet given you is, by construction, at
> a position after your anchor - including a message that was *said* an hour ago and only *arrived*
> a minute ago.

That is the property that matters for "my client died, catch me up", and it holds without any global
ordering at all.

### What it does NOT promise: chronological order

The order you get back is **arrival order at the machine you asked**, not the order things were
said. Two machines can hold the same messages in different positions, and a late arrival appears
late even though it was spoken early.

So:

 - **completeness** - yes, guaranteed, as long as the anchor has not been overwritten;
 - **same answer from any machine** - no;
 - **chronological order** - no, not from the log's own order.

### If speaking order matters, sort on what the record already carries

The record carries the origin `machine` and that machine's own `counter`. A client that wants
speaking order can sort on those rather than on arrival position - the data is already there. It
still cannot totally order two messages from *different* machines without a shared clock, but within
one origin it is exact, and interleaving by arrival is a reasonable display for the rest.

**This is the honest trade and it should be stated in the client, not hidden**: you will never miss a
message, and occasionally one will appear later than it was said.

### The anchor must be pinned to the machine that issued it

"Everything after `D100!4711`" is only meaningful to a machine that HAS `D100!4711` in its log. So:

 - the client stores the anchor **per room, per machine it syncs with** - which is more state than
   one counter per room, and is the real cost of the anchor design;
 - a machine asked about an anchor it does not hold must say so - **"I do not have that anchor"** -
   rather than returning everything or nothing. Silently returning the whole ring on an unknown
   anchor is how a client gets a thousand duplicate lines.

### Still open

Whether a room should ALSO have a home machine assigning a room-local sequence. That would give a
total order and let the client keep one counter per room - but it is a separate mechanism from the
anchor, not a replacement for it, and it costs a home per room. **Not needed for catch-up; only
needed if a stable global order is wanted for its own sake.** Decide it when federation forces the
question, not before.

## 2. Fixed-size records, and what that costs

A circular buffer only stays simple if **slot = sequence MOD n**. That needs a **fixed record
size**, which means a **maximum message length** that gets truncated.

A variable-length ring needs an index, compaction, and a way to find record boundaries after a
crash. On this machine, with PLANC and no array bounds checking, that is a great deal of new
failure surface for the sake of long messages.

**RECOMMENDED: fixed record, and say the limit out loud in the client.** A message longer than the
record is truncated at the point it is stored, and the client tells the user rather than silently
losing the tail.

### Record, first cut

| field | width | why |
|---|---|---|
| room sequence | 4 bytes | the order; `INTEGER4`, because `INTEGER` is 16 bits here and would wrap |
| origin machine | 2 bytes | provenance, and it makes the id globally unique |
| origin counter | 4 bytes | that machine's own counter - kept for debugging and dedup |
| room name | fixed | the room this belongs to |
| nickname | fixed | who said it |
| text length | 2 bytes | how much of the text field is real |
| text | fixed | truncated to fit |

**`INTEGER` IS 16 BITS ON THE ND-100.** A counter declared `INTEGER` wraps at 32767 and the history
silently starts overwriting itself in the wrong order. Counters are `INTEGER4`, which `CHAT.PLNC`
already uses for magic numbers.

---

## 3. The file, and the header

`n` records of fixed size, preallocated, plus a header holding:

 - `n` itself, so a file written with one size is not read with another;
 - the next sequence to write;
 - a format version, because this file outlives the program that wrote it.

**A file whose header disagrees with the code must be refused, not guessed at.** Reading a 1000-record
file as a 2000-record one silently returns other rooms' messages.

`INITIALIZE <num>` creates or resizes it. Resizing is destructive unless the old content is copied
forward, so it must say so before doing it.

Defaults: **1000 client, 2000 server**, as asked.

---

## 4. Sharing the code between server and client - the part with no obvious answer

The requirement is that the storage code is used by both. In PLANC there are two ways:

 - **`$INCLUDE` the same source into both.** Simple, and it is how the constants are already shared.
   The cost is that the code is compiled twice and can drift if one side is rebuilt and the other is
   not.
 - **Compile it once to a `:BRF` and link that into both.** This is what `XMP-100-1-B02` and
   `MON-CALL-1B-A00` already are, and it is the honest form of "shared code": one object, linked
   twice, and the RT loader already knows how to put a library on a segment.

**RECOMMENDED: a compiled `CHATHI:BRF`**, linked into `CHATSV` and into `CHAT` the same way the XMP
library is. It also gives the C# side something to mirror one-for-one.

---

## 5. The disk I/O - what is NOT yet known

`MON 50` open and `MON 43` close are already used and proved in `CHAT.PLNC` for `CHAT:CNFG`. A ring
needs **random access**: seek to a record and read or write it.

**The call for random access has NOT been established, and must not be guessed.** The catalogue is
at `Developer/MON/calls/`; `SetBlockPointer`, `ReadBlock`/`WriteBlock` style calls are the ones to
look for, along with whether a file must be opened in a particular access mode to allow it.

**Until that is confirmed against the manual, no record layout is final** - if only whole blocks can
be addressed, the record size wants to divide the block size, and that changes the table in section 2.

---

## 6. What the client asks for

`give me everything after <sequence> in <room>`. That is a new message kind - a room kind, since an
ordinary member asks it - and its answer is a run of stored messages.

Open, and worth deciding deliberately:

 - **Is the answer one message per reply, or a batch?** One per reply is simple and reuses `Said`,
   but a client returning after a weekend could ask for hundreds, and the machine has a limit of ten
   data transmit blocks that a twenty-user burst has already hit once.
 - **Can the client tell history from live traffic?** If the answer reuses `Said`, no. That matters
   if the client ever timestamps, beeps, or marks unread.
 - **What if the sequence asked for has been overwritten?** The server must say "that far back is
   gone, here is the oldest I have" rather than silently returning less than asked.

---

## 7. Private messages

Raised in the request, not yet designed. A private message has no room, so it needs either a
pseudo-room per pair of users or a separate store. **Not decided.** It affects the record layout, so
it should be decided before the layout is fixed rather than bolted on.

---

## Order of work

1. Settle the room-home ordering question (section 1) - it decides everything downstream.
2. Confirm the random-access monitor call (section 5) - it decides the record size.
3. Then the record layout, the header, and `CHATHI` as a linkable object.
4. C# mirror and tests off the machine.
5. `INITIALIZE <num>` in CHAT-MON, and the per-room size from the room table.
6. The "after this id" request kind, registry first.
