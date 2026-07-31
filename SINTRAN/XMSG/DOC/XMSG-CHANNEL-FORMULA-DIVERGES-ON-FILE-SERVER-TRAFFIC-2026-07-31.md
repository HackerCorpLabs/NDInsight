# The channel formula's `baseLow` must not be masked — SOLVED

**Date:** 2026-07-31
**Status:** root cause found, fix derived, validated on the whole corpus, dissector patched.

Started from one odd frame in `XMSG-APPEND-REMOTE-BATCH-CAPTURED-2026-07-31.md`.

---

## 1. The fix

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

**Still to do:**

- `XMSG-PROTOCOL.md` section 18.5 carries the same formula and has NOT been updated.
- The ACK closed form (`S_ack = seed + 0x0B`, `channel = 0xDE - epoch(echoed Flags1)`) uses
  the same epoch machinery. In C# it now picks up the fix automatically, but I have not
  re-validated the ACK scan against the corpus — the original claim there was 904/904 and it
  was measured on the same TAD-only corpus, so it deserves the same scrutiny.
- The channel anomaly in `XMSG-APPEND-REMOTE-BATCH-CAPTURED-2026-07-31.md` section 6 is
  explained by this and is no longer open.
