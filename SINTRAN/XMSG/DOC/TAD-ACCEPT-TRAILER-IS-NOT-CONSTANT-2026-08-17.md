# The TAD accept trailer is not a constant

**Date:** 2026-08-17
**Method:** a census of nine archived connect captures. No machine touched.
**Status:** the "constant" is disproved. The replacement reading is an INFERENCE from two samples.

---

## Where this came from

`CONNECT-TO D19999` from D100 fails, and the traced run narrowed it to our reply: D100 re-sends the
identical connect, byte for byte, and eventually gives up. A peer repeats because it has not seen a
satisfactory answer, so the question became what a REAL accept looks like.

`DOC/captures/ARCHIVE-2026-07/ethernet-conn-to-D100-from-102-WORKING-2026-08-01.pcapng` has one,
with D100 as the responder. Ours next to it:

```
D100 (real):  2113 000E 0066 0064 0021 0400D9F3 2100 8640 0066 075E 0064 0215 04000041 0008 01020001 0202 0009
ours:         2113 000E 0064 4E1F 0000 04008C5B 2100 8640 0064 06B1 4E1F 0156 04000041 0008 01020000 0202 000A
```

Same shape throughout - magic, subtype, the node pair, the `2100 8640` role bytes, `04000041 0008`.
The trailing parameter pair differs, and that pair is recorded in `TadTerminalResponder` as a
constant copied verbatim from one capture, with an honest note that its meaning was unknown.

## The census

Scanning every archived connect capture for the accept and its trailer:

| capture | responder | trailer |
|---|---|---|
| ethernet-conn-to-D100-from-102-**WORKING** | 0064 | `0102 0001 0202 0009` |
| ALLTEST-fa-connectto-102-100-103 | 0064 | `0102 0001 0202 0009` |
| ethernet-hdlc-ROUTE-THROUGH-**WORKING** | 0067 | `0102 0001 0202 0009` |
| test1 | 0064 | `0102 0000 0202 000A` |
| li-syst-tad-103 | 0064 | `0102 0000 0202 000A` |
| li-rout-102-tree | 0064 | `0102 0000 0202 000A` |
| conn-to-d102-from-100 | 0066 | `0102 0000 0202 000A` |
| new-conn-to-102-from-100 | 0066 | `0102 0000 0202 000A` |
| conn-to-102-from103-via100 | 0066 | `0102 0000 0202 000A` |

**So it is not a constant.** Two values occur.

## A hypothesis, raised and killed in one step

The first four rows suggested it identifies the responding machine - D100 and D103 answering one
way, D102 another. Widening the sample killed it immediately: **D100 sends BOTH**. It answers 1/9 in
the two working captures and 0/10 in `test1`, `li-syst-tad-103` and `li-rout-102-tree`.

Worth recording because the wrong version was three rows away from looking solid. Three captures
would have "proved" it.

## What survives

The two parameter blocks are `01 02 <p1>` and `02 02 <p2>`, and:

```
p1=0, p2=10        p1=1, p2=9
```

**Both observed pairs sum to ten**, which fits a resource count - sessions in use against sessions
free, out of ten - and would explain why one machine sends both: it depends on what was already open
when the connect arrived.

**This is an inference from exactly two distinct samples, not a decode.** A third value (2/8, or
anything not summing to ten) would settle it either way, and ten may be a coincidence of these
captures. It is written down as a lead, not a fact.

## What it means for us

`TadTerminalResponder.BuildConnectAccept` emits `0/10` unconditionally. If the sum-to-ten reading is
right, that becomes untrue the moment we hold a session - and holding a session is exactly the state
we were in when D100 re-sent its connect and then refused us: our first accept opened `tty1`, and the
retry was answered while that session existed, still claiming nothing was open.

**That is a lead, not a diagnosis.** It has not been shown that D100 reads this field at all, let
alone rejects on it. What makes it worth testing first is that it is cheap: emit the live count and
see whether the retry is accepted.

## Also different, and not chased

Two more fields differ from the real accept and neither has been explained:

- **Flags1** - D100's accept carries `0021`, matching the asker's connect; ours carries `0000`.
  `tad-connect-mirror-model` warns explicitly that this equality is a TRAP in symmetric-history
  sessions and that the responder runs its own sequence from zero. Our value follows that note. The
  capture is consistent with either reading, so it settles nothing.
- **Session port** - D100 uses `0215` in both of its accepts; we use `0156`.

Both are worth a look, and neither should be changed on the strength of one capture.
