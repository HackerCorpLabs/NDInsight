# The channel formula's `baseLow` must not be masked — better fit, NOT a validated mechanism

**Date:** 2026-07-31
**Status:** an expression that matches the whole corpus and regresses nothing.
**NOT validated against the kernel carving, and the carving disagrees with the model it
lives in.** Read section 0 before trusting any of the vocabulary below.

Started from one odd frame in `XMSG-APPEND-REMOTE-BATCH-CAPTURED-2026-07-31.md`.

---

## 0. Honest status — read this first

An earlier revision of this document was titled "SOLVED" and said "root cause found". That
overstated what was done. What was actually done is **curve-fitting**: an expression was
found that reproduces the Protocol-ID byte on 1449 of 1449 captured data frames. That is
good evidence the expression *predicts* the wire. It is **not** evidence that it is how
SINTRAN *computes* the byte, and the two were conflated.

Three specific reasons to hold the mechanism claim loosely:

1. **The kernel symbol file contradicts the field layout the formula is built on.**
   `POFTABS` makes the transported header 7 words / 14 bytes ending after `XMCSM`, and
   `XMCSM` a **single word**, commented *"datagram checksum; if not checksum, then message
   size"*. Our layout reads a **32-bit** `XMCSM` and the channel formula uses
   `XMCSM >> 24` — a shift that cannot exist on a one-word field. This conflict was already
   documented as OPEN in `XMSG-SUBHEADER-NAMED-FROM-SYMBOLS-2026-07-29.md` section 4, and
   it was not checked before the "root cause" claim was made.

2. **`Flags2` is a LENGTH on exactly the traffic where the bug shows up.** Measured across
   the corpus, `Flags2 - infoLen` is `-28` on 492 frames — the file-server ones, matching
   the "length - 28" rule already noted in `ChannelOffsetDiagnosticTests.cs`. But there are
   **54 distinct offsets** overall, so it is not a length everywhere. That is precisely the
   overloaded field the kernel comment describes: checksum **or** size, depending on the
   message.

   This reframes the whole divergence. On TAD/routing traffic `Flags2` is a small class
   word and the "class anchors the channel" story works. On file-server traffic `Flags2` is
   a **message length**, so `baseLow = seed - Flags2low` is subtracting a byte count — which
   is meaningless as an "epoch base". The formula did not fail because of an isolated
   masking slip; it failed because a length is being fed into a term that means "class".

3. **The likely real shape is a checksum with a borrow, not an "epoch".** The seed identity
   `(Counter + Flags1 + Flags2low) & 0xFF == seed` is a **checksum** relation. Removing the
   mask restores a **borrow** across the byte boundary, which is what an ordinary subtract
   with carry does. That is a plausible reason the corrected expression fits perfectly — but
   "epoch" and "class" remain model vocabulary the kernel does not support.

**What is safe to rely on:** the corrected expression reproduces every frame in the corpus
and breaks nothing that previously passed. Use it to validate and to build frames.
**What is not settled:** why. Do not cite this document as having explained the mechanism,
and do not build new inference on the words "epoch" or "class word" without resolving
section 4 of the sub-header doc first.

---

## 1. The change

(Subject to section 0: this is the better-fitting expression, not a derived mechanism.)

```
                 baseLow = (seed - F2low) & 0xFF        <- WRONG
                 baseLow =  seed - F2low                <- correct (signed)

                 epoch   = (Flags1 - baseLow + 0xFF) >> 8
                 Channel = 0xDE - (XMCSM >> 24) - epoch
```

That is the entire change: **drop the `& 0xFF`.**

When `F2low > seed` the true `baseLow` is **negative**. Masking it to 0..255 turns it into a
large positive, which moves the 256-boundary inside the epoch shift and loses a borrow. The
epoch then comes out one too low and the predicted channel one too **high** — which is
exactly the shape of every mismatch observed (`0xDA` predicted where the wire said `0xD9`,
`0xDD` where the wire said `0xDC`).

### Validation over the whole corpus

Every `.pcapng` in `E:\Dev\Ronny\X25Emulator\pcap\`, every link, seed learned per frame from
the frame itself (so the 100-103 and 102-103 links and the relayed +1 lanes are all included
rather than silently dropped):

| Model | Data frames correct |
|---|---|
| masked `baseLow` (old) | 1267 / 1449 |
| signed `baseLow` (fix) | **1449 / 1449** |

Zero seed violations. Critically, the fix **breaks nothing**: every capture that previously
passed still passes at 100%.

---

## 2. Why it hid for so long

`Flags2` variety per capture:

| Family | distinct `Flags2` | old-model result |
|---|---:|---|
| TAD / routing / connect | 5 | 0 mismatches |
| File-server / FA | 11 - 15 | mismatches everywhere |

TAD and routing traffic uses a handful of class words, and they are all **below the seed**,
so `seed - F2low` never goes negative and the mask never bites. File-server traffic swings
`Flags2` across a wide range on consecutive frames, many values above the seed, and the
error fires immediately.

So the old **"VERIFIED 753/753" was scope-limited, not false.** It was measured only on the
family that cannot exercise the bug. This is worth remembering as a general trap: a
conformance figure is only as good as the variety in the corpus it was measured on.

The clearest single view, from `claude-file-stat-102-to-100-2026-07-29` — `Flags1` marching
+1 per frame, actual channel pinned at `0xDC`, prediction flipping purely on `Flags2`:

```
 Flags1  Flags2  base(masked)  ep  pred  act
 0x017F  0x0008     0x0C        2  0xDC  0xDC   ok      (F2low < seed)
 0x0180  0x0062     0xB2        1  0xDD  0xDC   WRONG   (F2low > seed -> mask bites)
 0x0181  0x0008     0x0C        2  0xDC  0xDC   ok
 0x0182  0x0046     0xCE        1  0xDD  0xDC   WRONG
```

With `baseLow` signed, rows 2 and 4 become `base = -0x4E` / `-0x32`, epoch 2, channel `0xDC`.

---

## 3. A wrong turn worth recording

An earlier version of this document claimed the `Flags2` dependence itself was **spurious**
and that the channel depended on `Flags1` alone. **That was wrong**, and the reasoning that
produced it was invalid.

The "decisive test" was: does any `(direction, Flags1)` pair map to two different epochs? It
returned zero conflicts everywhere, which looked like strong evidence. It was **vacuous** —
`Flags1` increments by exactly 1 per data frame per direction, so a `(direction, Flags1)`
pair identifies exactly **one frame**. Single-valuedness was guaranteed by construction and
carried no information at all.

What actually broke it open was dumping frames in **arrival order** for one direction and
doing the arithmetic by hand, which immediately showed the mismatches all had the same
signature (predicted exactly one too high) and that the signature tracked `F2low > seed`.

Lesson: a test that cannot fail is not evidence. Check that a proposed invariant has a way
of being violated before believing a clean result.

---

## 4. What changed in the repo

`SINTRAN\Devices\HDLC\WireShark\hdlc_tcp.lua` — the mask removed at the envelope-validation
site, with the reasoning recorded inline, and the file-header summary corrected from
753/753 to 1449/1449.

**The installed copy has NOT been updated.** The dissector Wireshark actually loads lives at
`C:\Program Files\Wireshark\plugins\hdlc_tcp.lua`, which needs administrator rights to
replace. Copy the repo file over it and then Analyze -> Reload Lua Plugins (Ctrl+Shift+L).

I verified the edited file **compiles** (tshark loads it and reaches the `Proto` call) and I
verified the **arithmetic** independently at 1449/1449 with a standalone scanner. I have
**not** run the patched dissector end to end, because that requires replacing the
admin-owned installed copy.

---

## 5. Knock-on

**Fixed:**

- `Xmsg.Protocol\Packet\XmsgEnvelope.cs` — `BaseLow` returned `byte`, so the truncation hit
  both consumers. Added `BaseLowSigned` and pointed `ComputeEpoch` at it; `BaseLow` now
  delegates to it and keeps the byte truncation, which is correct for its only other
  consumer (`ComputeCounter` is a byte and is exact modulo 256 either way). Whole XMSG
  solution builds clean and **all 459 tests pass**, including the envelope tests that encode
  captured frames — so the fix does not disturb any previously-verified case.
- `hdlc_tcp.lua` — mask removed, reasoning recorded inline (repo copy only; see section 4).
- `xmsg-decode` skill — envelope block corrected to the signed form and 1449/1449.

- `XMSG-PROTOCOL.md` section 18.5 — formula corrected, with the reasoning and the corpus
  figures recorded inline. The ACK section header updated from 904/904 to 1671/1671.
- The channel anomaly in `XMSG-APPEND-REMOTE-BATCH-CAPTURED-2026-07-31.md` section 6 is
  explained by this and is no longer open.

### The ACK closed form was re-scanned and needs NO change

It uses the same epoch machinery and the same masked `baseLow`, and its "904/904" came from
the same TAD-only corpus, so it deserved the same scrutiny. Re-scanned over every capture:

| Check | masked `baseLow` | signed `baseLow` |
|---|---|---|
| trailing byte | 1671/1671 | 1671/1671 |
| channel | 1671/1671 | 1671/1671 |

Identical, because ACK `Flags2` low is only ever **1, 2 or 3** — always far below
`S_ack` (~`0x1F`) — so `baseLow` never goes negative and the mask never bites. Frames where
`F2low > S_ack`: **zero**. Prediction confirmed rather than assumed.

Minor new observation: a `Flags2` low byte of **`0x0003`** occurs once in the corpus. The
spec documents only `0x0001` and `0x0002` for ACKs, so receivers should tolerate `0x0003`
too.

### Outstanding — the real question

The empirical work is done; the mechanism is not. The next step is **carving, not more
captures** — no amount of curve-fitting will settle what the kernel actually computes:

1. Resolve `XMSG-SUBHEADER-NAMED-FROM-SYMBOLS-2026-07-29.md` section 4 — is `XMCSM` one
   word or two? Everything else depends on it, including whether `XMCSM >> 24` is even a
   real quantity.
2. Find the code that writes SINTRAN header offset 12 (the Protocol-ID byte). If it is a
   checksum/borrow rather than a channel selector, the whole "channel lane" vocabulary in
   `XMSG-PROTOCOL.md` needs rewriting, not patching.
3. Pin down when `XMCSM` carries a checksum and when it carries a size — the kernel comment
   says both, and the 54 distinct `Flags2 - infoLen` offsets say the corpus contains both.

Until then the expression is a validated *predictor* and should be described as one.
