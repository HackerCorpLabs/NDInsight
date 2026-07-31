# The envelope channel formula has a spurious Flags2 dependence

**Date:** 2026-07-31
**Status:** root cause identified and evidenced. A corrected formula is **not** yet derived.

Started from one odd frame in `XMSG-APPEND-REMOTE-BATCH-CAPTURED-2026-07-31.md`. It is not
one odd frame, and it is not specific to that capture.

---

## 1. The corpus scan: a clean split between two traffic families

Ran the project dissector (`SINTRAN\Devices\HDLC\WireShark\hdlc_tcp.lua`, the copy already
installed in the global Wireshark plugin folder) over **every** `.pcapng` in
`E:\Dev\Ronny\X25Emulator\pcap\`, counting its own
`Channel mismatch: Protocol ID 0xNN but seed model expects 0xMM` warning.

| Family | Captures | Result |
|---|---|---|
| File-server / FA (2026-07-29 onward) | 17 | **all mismatch**, rates 2/1192 to 72/340 |
| TAD / routing / connect (older) | 13 | **exactly zero**, ~1400 frames |

Same two machines, same link, same seed. The split is by traffic family, not by date, size,
direction or node pair.

Full per-file table is in the git history of this file (commit that introduced it); the
counts above are the summary that matters.

---

## 2. Root cause: `baseLow` makes the channel depend on `Flags2`, and it must not

The model under test:

```
baseLow   = (seed - (Flags2 & 0xFF)) & 0xFF
epoch     = (Flags1 - baseLow + 0xFF) >> 8
Channel   = 0xDE - (XMCSM >> 24) - epoch
```

Because `baseLow` is recomputed per frame from `Flags2`, the predicted `epoch` moves when
the class word moves. **The real channel byte does not.**

The clearest single view, from `claude-file-stat-102-to-100-2026-07-29` — `Flags1` marching
1 per frame, the actual channel pinned at `0xDC` throughout, and the prediction flipping
purely on the size of `Flags2`:

```
 Flags1  Flags2  base   ep  pred  act
 0x017F  0x0008  0x0C    2  0xDC  0xDC   ok
 0x0180  0x0062  0xB2    1  0xDD  0xDC   *** MISMATCH
 0x0181  0x0008  0x0C    2  0xDC  0xDC   ok
 0x0182  0x0046  0xCE    1  0xDD  0xDC   *** MISMATCH
 0x0183  0x0008  0x0C    2  0xDC  0xDC   ok
 0x0184  0x0028  0xEC    1  0xDD  0xDC   *** MISMATCH
```

Small `Flags2` leaves `baseLow` small and the epoch lands on 2 (correct). Large `Flags2`
pushes `baseLow` up, the subtraction crosses a 256 boundary, and the epoch drops to 1.
Nothing about the wire changed.

### The decisive test

Read the TRUE epoch straight off the wire for every Data frame —
`epoch_true = 0xDE - (XMCSM>>24) - actual_channel` — and ask whether any
`(direction, Flags1)` pair ever shows two different epochs. `Flags1` is one sequence per
direction per link, so direction has to be part of the key.

| Capture | frames | (dir,Flags1) with >1 epoch | max distinct Flags2 inside ONE epoch group |
|---|---:|---:|---:|
| `claude-file-stat-...-07-29` | 28 | **0** | 11 |
| `append-remote-batch-...-07-31` | 4 | **0** | 2 |
| `claude-list-files-SESSION2-...-07-30` | 146 | **0** | 4 |
| `claude-transfer-SPARSE-...-07-30` | 182 | **0** | 3 |
| `conn-to-d102-from-100` (clean family) | 49 | **0** | 4 |

**Zero conflicts anywhere**, while up to eleven different class words sit inside a single
epoch group. The channel byte is determined by direction and `Flags1`; `Flags2` has no
influence on it. The `Flags2` term in `baseLow` is spurious.

---

## 3. Why the old corpus never caught this

`Flags2` variety, measured across whole captures:

| Capture | distinct `Flags2` | mismatches |
|---|---:|---:|
| `conn-to-d102-from-100` | 5 | 0 |
| `new-conn-to-102-from-100` | 5 | 0 |
| `test1` | 5 | 0 |
| `claude-list-files-SESSION2-...` | 11 | 72 |
| `claude-file-stat-...` | 15 | 10 |

TAD and routing traffic barely varies the class word, so `baseLow` stays effectively
constant and the spurious term never bites. File-server traffic swings `Flags2` across a
wide range on consecutive frames, and the error surfaces immediately.

So the skill's "VERIFIED 753/753" is not false — it is **scope-limited**. It was measured on
the family that cannot exercise the bug.

---

## 4. What a fix must do — and why I am not proposing one yet

Removing the `Flags2` dependence and using the link seed as the fixed reference
(`epoch_alt = (Flags1 - seed + 0xFF) >> 8`) does well but is not right:

| Capture | `epoch_alt` matches |
|---|---|
| `claude-file-stat-...` | 28/28 |
| `claude-transfer-SPARSE-...` | 182/182 |
| `conn-to-d102-from-100` | 46/49 |
| `claude-list-files-SESSION2-...` | 101/146 |
| `append-remote-batch-...` | 0/4 |

So the seed is not the correct fixed reference in general. `append-remote-batch` sits
entirely at `epoch_true = 5` where the seed reference says 4 — a whole-capture offset,
consistent with `epoch` being a **cumulative count over the life of the link** that a
capture starting mid-stream cannot reconstruct from `Flags1` alone.

**One anomaly I cannot explain and did not chase:** in
`claude-list-files-SESSION2`, within a single direction, the `Flags1` ranges of the two
epoch groups OVERLAP (dir 45164: epoch 3 spans `0x02F0..0x030C`, epoch 4 spans
`0x02F1..0x0338`) even though no individual `Flags1` maps to both. A plain cumulative
counter cannot do that. Either `Flags1` is not monotonic there, or
`Channel = 0xDE - (XMCSM>>24) - epoch` is missing a term, in which case `epoch_true` as
computed above is absorbing it. **Resolve this before writing any replacement formula.**

---

## 5. Practical guidance right now

- The mismatching frames are **well formed**. The seed identity
  `(Counter + Flags1low + Flags2low) & 0xFF == seed` holds on every one of them
  (0 violations across all frames scanned). Every decode we have done on file-server
  captures stands; only the predicted channel byte was wrong.
- Do **not** change `hdlc_tcp.lua` yet. Its warning is correctly telling us the model is
  wrong, and it is currently our only detector.
- Do **not** fit a formula to one capture. Any candidate must keep the ~1400-frame TAD
  corpus at zero AND explain the overlap in section 4.

## 6. Tooling

Two throwaway scanners were used and are worth rebuilding if this is picked up again
(they live in the session scratchpad, not in the repo):

- `chanscan.py` — per-frame dump of `(Flags1, Flags2, Counter, XMCSM>>24, baseLow, epoch,
  predicted, actual)` with a verdict column. Does its own TCP reassembly per direction,
  HDLC de-framing, un-stuffing and FCS check rather than trusting per-packet splits.
- `epochtest.py` — reads `epoch_true` off the wire and tests whether `(direction, Flags1)`
  determines it, plus the seed-referenced variant.
