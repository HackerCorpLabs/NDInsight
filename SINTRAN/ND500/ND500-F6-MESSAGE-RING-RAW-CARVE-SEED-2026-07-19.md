# F6 LAST-N500-MSG Ring - Raw Capture (Carve Seed, UNGRADED)

**For**: the 3022 carve bundle (architect, 2026-07-19). **From**: ND-500 bus-interface harness.
**Guard (architect's instruction)**: raw bytes + offsets only. **No field meanings are inferred
here** - this is a seed so the carver matches real bytes against the LIST-TABLE LAST-N500-MSG
structure (ND-60.136 section 8.10.9.1) instead of reconstructing cold. Do not treat any grouping
below as a field boundary.

**Source**: RetroCore harness `Nd500_F6_MessageRing_RawCapture` (committed test file). Boots SINTRAN
III L, logs in SYSTEM, `@ND-500`, `VERSION` (to provoke background polls), then dumps the raw 5MPM
window bytes at the two mailbox sites we captured messages at (`ReadMicroVersion`/`ResidentRead`),
before and after `LIST-TABLE LAST-N500-MSG`. Window base = ND-100 byte `0x420000`. Full 3022 trace:
`scratchpad\sintran-3022-trace-f6ring.txt`; console: `scratchpad\sintran-boot-capture-f6ring.txt`.

**Console note (follow-up, not carve)**: `LIST-TABLE LAST-N500-MSG` printed only `> Loading Swapper`
then returned to `N500:` - NO visible ring table rendered to the terminal. Either it needs a
different argument/output target or the ring renders elsewhere. The raw MPM below is the useful part.

---

## Mailbox site A - window offset 0x0E30 (ND-100 byte 0x420E30)

BEFORE `LIST-TABLE`:
```
0x00420E00  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
0x00420E10  00 00 00 00 00 00 FF FF 00 00 00 00 00 00 00 00
0x00420E20  00 00 00 00 00 01 00 2D 00 00 00 00 57 92 40 00
0x00420E30  FF FF FF FF 00 03 00 01 00 01 00 00 00 01 2E 9A
0x00420E40  00 00 00 21 24 00 08 00 00 00 00 00 11 E6 00 02
0x00420E50  48 00 80 00 00 01 80 27 91 20 01 C8 12 07 E2 08
0x00420E60  00 00 20 00 02 04 11 F4 00 00 50 00 00 00 00 01
0x00420E70  80 00 00 00 01 08 11 F5 E0 00 40 00 00 00 6C 01
0x00420E80  80 00 00 00 01 0F 11 F6 C2 1A 40 00 00 00 70 01
0x00420E90  60 00 00 00 02 04 11 F7 00 00 50 00 00 00 00 01
0x00420EA0  80 00 00 00 01 08 11 F8 E2 04 40 00 00 00 6C 01
0x00420EB0  80 12 00 00 00 07 00 00 00 00 00 03 50 00 00 01
0x00420EC0  80 00 00 00 01 08 11 FA C2 1E 40 00 00 21 04 86
0x00420ED0  E0 00 00 00 02 04 11 FB 00 00 50 00 00 00 00 02
0x00420EE0  50 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
0x00420EF0  00 21 28 00 00 00 00 00 9B 6A 00 00 00 00 04 80
```

AFTER `LIST-TABLE`:
```
0x00420E00  00 00 C3 05 C3 06 C3 08 C3 0A 00 30 00 24 C4 00
0x00420E10  00 00 8E DE 00 02 FF FF 00 00 00 00 00 00 00 00
0x00420E20  00 00 00 00 00 01 00 2D 00 00 00 00 57 92 40 00
0x00420E30  FF FF FF FF 00 04 00 01 00 00 00 00 00 08 08 01
0x00420E40  28 00 00 21 28 00 00 68 00 00 00 00 11 E6 00 02
0x00420E50  48 00 80 00 00 01 80 27 91 20 01 C8 12 07 E2 08
0x00420E60  00 00 20 00 02 04 11 F4 00 00 50 00 00 00 00 01
0x00420E70  80 00 00 00 01 08 11 F5 E0 00 40 00 00 00 6C 01
0x00420E80  80 00 00 00 01 0F 11 F6 C2 1A 40 00 00 00 70 01
0x00420E90  60 00 00 00 02 04 11 F7 00 00 50 00 00 00 00 01
0x00420EA0  80 00 00 00 01 08 11 F8 E2 04 40 00 00 00 6C 01
0x00420EB0  80 12 00 00 00 07 00 00 00 00 00 03 50 00 00 01
0x00420EC0  80 00 00 00 01 08 11 FA C2 1E 40 00 00 21 04 86
0x00420ED0  E0 00 00 00 02 04 11 FB 00 00 50 00 00 00 00 02
0x00420EE0  50 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
0x00420EF0  00 21 28 00 00 00 00 00 9B 6A 00 00 00 00 04 80
```

## Mailbox site B - window offset 0x4130 (ND-100 byte 0x424130, the PROCMSG base)

BEFORE `LIST-TABLE`:
```
0x00424120  00 00 00 00 00 01 7F FE 00 00 00 00 00 00 18 00
0x00424130  00 00 00 00 00 00 FF FF 00 00 00 00 00 01 00 00
0x00424140  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
   (0x424140..0x4241FF all zero)
```

AFTER `LIST-TABLE`:
```
0x00424120  00 00 00 00 00 01 7F FE 00 00 00 00 00 00 18 00
0x00424130  FF FF FF FF 00 03 FF FF 00 00 00 00 00 01 2E 9A
0x00424140  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
   (0x424140..0x4241FF all zero except last bytes 04 80 at 0x4241FE)
```

---

## Raw observations (UNGRADED - byte-alignment only, NOT field claims)

- Both mailbox sites start with `FF FF FF FF` (matches the trace's `MSGHDR link=0xFFFFFFFF`).
- Site A byte at 0x420E35 changed `03 -> 04` across `LIST-TABLE`; the 4 bytes at 0x420E00 also went
  from zero to `00 00 C3 05 C3 06 C3 08 C3 0A ...` (some new data appeared) - a counter/pointer of
  some kind advanced. NOT graded.
- Site A has a repeating ~16-byte entry array from ~0x420E40 to ~0x420EE0 with an incrementing
  2-byte value (`11 E6`, `11 F4`, `11 F5`, `11 F6`, `11 F7`, `11 F8`, `11 FA`, `11 FB`) - looks
  ring-like but the record size/fields are for the carve to determine.
- Site B (0x424130) was ZERO before and became `FF FF FF FF 00 03 FF FF ... 00 01 2E 9A` after -
  the tail `00 01 2E 9A` also appears at site A's pre-`LIST-TABLE` 0x420E3C. Same value at two sites.
- The trace side (`sintran-3022-trace-f6ring.txt`) shows the corresponding `MSGHDR base=0x424130
  N5STA=0x0001 MICFU=0x0001` / `PROCMSG lastMICFU=ReadMicroVersion` and `ResidentRead` events.

**Carve question this seeds**: is the LAST-N500-MSG ring the site-A entry array (0x420E40+), and do
its records line up with the site-B PROCMSG writes? The carver has real bytes on both ends now.
