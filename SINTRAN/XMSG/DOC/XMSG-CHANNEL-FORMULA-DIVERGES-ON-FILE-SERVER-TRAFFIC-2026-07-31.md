# The envelope channel formula diverges on file-server traffic — corpus-wide scan

**Date:** 2026-07-31
**Method:** ran the project dissector (`SINTRAN\Devices\HDLC\WireShark\hdlc_tcp.lua`, the
copy installed in the global Wireshark plugin folder) over **every** `.pcapng` in
`E:\Dev\Ronny\X25Emulator\pcap\` and counted its own
`Channel mismatch: Protocol ID 0xNN but seed model expects 0xMM` expert warning.

This started from a single odd frame in
`XMSG-APPEND-REMOTE-BATCH-CAPTURED-2026-07-31.md`. It is **not** a single odd frame.

---

## 1. The result: a clean split between two capture families

| Capture | Frames | Mismatches |
|---|---:|---:|
| `append-remote-batch-102-to-100-2026-07-31` | 12 | **4** |
| `claude-CLOSEONLY-102-to-100-2026-07-30` | 6 | **2** |
| `claude-create-file-102-to-100-2026-07-30` | 35 | **3** |
| `claude-create-file-DIRSPEC-102-to-100-2026-07-30` | 34 | **3** |
| `claude-create-file-NAMELEN-102-to-100-2026-07-30` | 92 | **9** |
| `claude-delete-file-102-to-100-2026-07-29` | 31 | **3** |
| `claude-file-stat-102-to-100-2026-07-29` | 59 | **10** |
| `claude-list-files-d100-system-2026-07-29` | 109 | **27** |
| `claude-list-files-SESSION2-102-to-100-2026-07-30` | 340 | **72** |
| `claude-open-close-file-102-to-100-2026-07-30` | 32 | **4** |
| `claude-open-W-close-102-to-100-2026-07-30` | 47 | **5** |
| `claude-OPENONLY-102-to-100-2026-07-30` | 20 | **4** |
| `claude-transfer-file-COMPLETE-102-to-100-2026-07-29` | 45 | **2** |
| `claude-transfer-PULL-content-100-to-102-2026-07-29` | 326 | **2** |
| `claude-transfer-SMALL-167bytes-102-to-100-2026-07-30` | 31 | **4** |
| `claude-transfer-SPARSE-s3config-102-to-100-2026-07-30` | 1192 | **2** |
| `fa-access-secret-102-to-100-2026-07-29` | 128 | **26** |
| --- | --- | --- |
| `conn-to-102-from103-via100` | 179 | 0 |
| `conn-to-d102-from-100` | 87 | 0 |
| `device-online-100-102-103` | 126 | 0 |
| `li-rout-102-tree` | 28 | 0 |
| `li-rout-103-tree` | 36 | 0 |
| `li-route-d103-tree-x` | 142 | 0 |
| `li-route-d103-tree` | 66 | 0 |
| `li-routing-100-proxy-102` | 50 | 0 |
| `li-syst-tad-103` | 24 | 0 |
| `list-routing-info-100-102-then-102-100` | 12 | 0 |
| `multiple-connect-100-to102-...-connect-again` | 265 | 0 |
| `new-conn-to-102-from-100` | 108 | 0 |
| `start-li-li-1err` | 31 | 0 |
| `test1` | 250 | 0 |

**Every** file-server / FA capture (2026-07-29 onward) mismatches.
**Every** TAD / routing / connect capture is at exactly zero, across ~1400 frames.

The split is by traffic family, not by date, size, direction or node pair — the same two
machines and the same link produce zero mismatches on TAD traffic and hundreds on
file-server traffic.

---

## 2. What this says about "VERIFIED 753/753"

The `xmsg-decode` skill states the envelope formulas are VERIFIED on 753/753 data frames.
That figure was measured on the **older corpus** — precisely the set that still shows zero
here. So the claim is not wrong, but its scope is narrower than it reads: it was never
exercised against file-server traffic, and it does not hold there.

The formula in question:

```
baseLow = (seed - (Flags2 & 0xFF)) & 0xFF
epoch   = (Flags1 - baseLow + 0xFF) >> 8
Channel = 0xDE - (XMCSM >> 24) - epoch
```

The **seed** identity is a different matter and still holds everywhere I checked,
including on the mismatching frames:
`(Counter + Flags1low + Flags2low) & 0xFF == seed`. On the APPEND-REMOTE-BATCH request it
comes out at exactly `0x14`, the known 100-102 seed. So these frames are **well formed**
and the parse is sound — it is only the predicted channel byte that is wrong.

---

## 3. What I ruled OUT

My first hypothesis was that the `Flags2` term is what breaks, because in
`claude-list-files-SESSION2` the actual channel byte is a **single value** (`0xDB`, 133 data
frames) while `Flags2` takes **eleven** distinct values (`0x0001, 0x0002, 0x0008, 0x0012,
0x0020, 0x0022, 0x0028, 0x0046, 0x0062, 0x0064, 0x0070`). In that session the real channel
plainly does not track `Flags2`, while the formula makes it do so through `baseLow`.

**That hypothesis does not survive the next capture.** In
`claude-transfer-SPARSE-s3config` the actual channel *does* move (`0xD8` 28, `0xD9` 150,
`0xDB` 364) and `Flags2` is also spread (`0x0406` 356, `0x0001` 348, `0x0252` 178, plus
others) — yet that capture has only **2 mismatches in 1192 frames**.

So it is NOT simply "channel is constant" and NOT simply "large or varying `Flags2` breaks
it". The mismatch rate ranges from 2/1192 to 72/340 with no explanation I can support.

---

## 4. Status

**VERIFIED**
- Mismatches are confined to the file-server / FA capture family; the TAD / routing family
  is at zero across ~1400 frames.
- The seed identity holds on mismatching frames, so they are well formed.
- In `claude-list-files-SESSION2`, one actual channel value against eleven class words.
- In `claude-transfer-SPARSE-s3config`, three actual channel values and a low mismatch rate.

**UNKNOWN**
- The cause. I have not identified it and I am not proposing a replacement formula.

**Do not** "fix" the formula against any single frame. Whatever the correction is, it has to
keep the ~1400-frame TAD/routing corpus at zero while explaining rates that vary by a factor
of thirty across file-server captures.

---

## 5. Suggested next step

The cheapest discriminator is probably to dump, for one mismatching capture,
`(Flags1, Flags2, Counter, XMCSM>>24, actual channel, predicted channel)` per frame and look
at where predicted and actual part company in sequence — specifically whether the divergence
appears at a `Flags1` wrap boundary (which would make it an off-by-one in the `epoch`
expression) or at a change of service/class (which would make it a missing term). I did not
do this; it needs a small decoder rather than the dissector's summary output.
