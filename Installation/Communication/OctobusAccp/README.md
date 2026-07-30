# OctobusAccp — ACCP (ND-324716 / PCB 5616) octobus controller firmware

The **Samson ACCess Processor** — the ND-5000's access processor *and* its octobus
controller. A 68000 "baby card" with 128 KB of EPROM and 4x 8192x8 SRAM (32 KB).
It supersedes ND-324702. Hardware reference:
http://sintran.com/hardware/nd-5000/nd-324716.html

This folder holds the **physical EPROM dump** of that card's firmware — the octobus
controller's operating software, previously recorded in the repo as missing (see
`SINTRAN\ND5000\HANDOFF-OCTOBUS-EMULATION.md` line 123: *"ACCP EPROM dump (68000
firmware) — would unlock full ACCP library emulation"*, and
`SINTRAN\ND5000\OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md`
section 4.3).

## Layout

```
OctobusAccp/
├── README.md          ← you are here
├── eprom/             ← the raw dumps + merge/validation notes
│   ├── 51200J.bin     ← AM27C512, even bytes (D8..D15, high byte)
│   ├── 51201J.bin     ← AM27C512, odd bytes  (D0..D7, low byte)
│   ├── octo.bin       ← the two interleaved into one 128 KB 68000 image
│   └── README.md      ← interleave order, reset vectors, validation evidence
└── docs/              ← reverse-engineering writeups
```

The `eprom/` layout follows the precedent set by the Ethernet II/III firmware at
`Installation\Communication\Ethernet\x\stripped\`, which is the repo's other
68000-controller firmware dump.

## The image

| Property | Value |
|---|---|
| Merged image | `eprom/octo.bin`, 131,072 bytes (0x20000) |
| MD5 | `39A21C86A74BFEDFC0B996DE7F5ADB63` |
| CPU | MC68000, big-endian |
| Ghidra language | `68000:BE:32:default`, image base `0x00000000` |
| Reset SSP | `0x00113FFC` |
| Reset PC | `0x00000BD6` |
| On-card SRAM | `0x00110000`–`0x00117FFF` (32 KB) |

The image was validated as genuine 68000 code before use: plausible even reset
vectors, a well-formed exception vector table (vectors 2..11 distinct and ascending
in `0x400..0x490`, vectors 12..15 sharing one handler at `0x8B8`), and a cleanly
disassembling entry point. See `eprom/README.md` for the full evidence, including
why the opposite interleave order is ruled out.

Independent corroboration of the interleave: the reset code at `0x0BD6` RAM-tests
`0x00110000`, which is exactly the region the reset SSP `0x00113FFC` points into.
Two facts derived separately that agree.

## Analysis

The reverse-engineering pass currently lives at:

**`E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-324716-FIRMWARE-RE-2026-07-27.md`**

It covers the identifying console strings (`S A M S O N   A C C E S S   P R O C E S S O R`,
`Communication ACCP-ND100 started. Version:`, `6 8 0 0 0   T R A P :`), the memory map,
the SCN2681 DUART debug console, and the exception vector table. That document was
written against the image while it still sat at `C:\Temp\octo\octo.bin`; its path
references are now stale and should point here instead.

`docs/` is reserved for further writeups so they sit next to the binary, matching the
Ethernet folder's structure.

## Not verified

- **The ROM base address on the real card.** The vectors point at low offsets inside
  the image, consistent with the EPROM being mapped at `0x000000` at reset. This is an
  inference from the image, not confirmed against a schematic or address decoder — no
  ACCP schematic is present in this repo.
- **Whether the dumps are complete and error-free.** No internal checksum or CRC word
  has been located, so the dumps have not been checked against any integrity value.
- **The firmware version.** The image contains a `Communication ACCP-ND100 started.
  Version:` string but the version itself is emitted at run time; it has not been
  traced to a constant in the image.
- **What `51200J` / `51201J` are.** These are the labels on the two physical devices.
  They are not ND part numbers in the usual `3xxxxx` / `ND-xxxxxx` form and have not
  been matched to any ND parts list.
