# Question for the GOD LLM - name & decode three XMSG sub-header wire fields

## Why I'm asking

I'm promoting the last hardcoded hex values in the C# XMSG stack to documented enums. Three
fields are still bare hex at every call site because I don't know their **bit-level meaning** -
naming them would assert semantics I can't verify. You have the TAD/XMSG traffic and the header
layout, so please decode the bitfields and give canonical names.

The three fields (all in the SINTRAN header / XMSG sub-header):

1. **`role`** byte (XMSG sub-header role octet)
2. **`frameFlags`** byte (XMSG sub-header frame-flags octet, offset 3)
3. **`frameClass`** word = SINTRAN **Flags2** (16-bit)

Below is every value I emit today, in context. These reproduce real captures byte-for-byte
(101/102/103 vs retrocore 100), so treat them as observed ground truth, not guesses.

## Evidence - ASKER frames (connect-to client, node 102/103 -> 100)

| Frame (phase)              | frameClass (Flags2) | frameFlags | role | XMCSM        |
|----------------------------|---------------------|------------|------|--------------|
| Connect letter (XSLET)     | 0x0400              | 0x86       | 0xE4 | 0x04000041   |
| Session-setup (06/1B/1C/FF)| 0x0400              | 0x86       | 0x84 | 0x04000000   |
| Terminal-setup             | 0x0108              | 0x86       | 0x84 | 0x01080000   |
| Input line (keystrokes)    | 0x0108              | 0x96       | 0x84 | 0x01080000   |
| ESCA (escape)              | 0x0008              | 0x96       | 0x94 | 0x00080000   |
| RECO (reset-confirm)       | 0x0108              | 0x96       | 0x94 | 0x01080000   |
| CERS (escape-response)     | 0x0108              | 0x96       | 0x94 | 0x01080000   |
| DUMM (keepalive)           | 0x0108              | 0x96       | 0x94 | 0x01080000   |
| DCON (disconnect)          | 0x0008              | 0x96       | 0x94 | 0x00080000   |

## Evidence - RESPONDER frames (TAD terminal server, node 102 -> 100)

| Frame (phase)              | frameClass (Flags2) | frameFlags | role | XMCSM        |
|----------------------------|---------------------|------------|------|--------------|
| Session setup              | 0x0400              | 0x86       | 0x40 | 0x04000041   |
| Terminal data (MOTD burst) | 0x0108              | 0x92       | 0x00 | 0x01080000   |
| Bare-TAD control (esc-ack) | 0x0008              | 0x86       | 0x00 | 0x00080000   |
| Terminal data (no-RFI)     | 0x0108              | 0x92       | 0x00 | 0x01080000   |
| Terminal data (with RFI)   | 0x0108              | 0x96       | 0x00 | 0x01080000   |
| 0xFD session notification  | 0x0006              | 0x82       | 0x54 | 0x00060000   |

## Evidence - LIST-ROUTING (XSGSY) frames

| Frame     | frameClass (Flags2) | frameFlags | role | XMCSM        |
|-----------|---------------------|------------|------|--------------|
| Request   | 0x0100              | 0x86       | 0x84 | 0x0100014B   |
| Reply     | 0x0100              | 0x86       | 0x60 | 0x01000100   |

## My structural observations (please confirm or correct)

**A. frameClass (Flags2) == XMCSM >> 16.** In every row above, Flags2 equals the top 16 bits
of the XMCSM word (0x04000041->0x0400, 0x01080000->0x0108, 0x00060000->0x0006, 0x0100014B->0x0100).
If that holds universally, `frameClass` is not an independent field at all - it's the high half
of XMCSM. **Should I model it as `XMCSM >> 16` rather than a separate enum? Does the ND source
treat Flags2 and the XMCSM high word as the same value, or is the match a coincidence of these
captures?**

**B. role low nibble.** Asker roles are 0xE4/0x84/0x94 (low nibble 4); most responder roles are
0x40/0x00/0x60 (low nibble 0). We currently test `(role & 0x0F) == 0x04` to mean "asker". **BUT
the responder's 0xFD notification uses role 0x54 (low nibble 4)** - which breaks that rule. So:
what do the **high nibble** and **low nibble** of `role` actually encode? (asker/responder?
phase? letter-vs-data? a bitfield?) And why is the 0xFD notify 0x54?

**C. frameFlags bit decomposition.** The four values are 0x82/0x86/0x92/0x96:
- 0x02 (bit 1) is set in all four.
- 0x04 (bit 2) is set in 0x86 and 0x96, clear in 0x82 and 0x92.
- 0x10 (bit 4) is set in 0x92 and 0x96, clear in 0x82 and 0x86.
Empirically 0x86 rides setup/control + first frames; 0x96 rides most terminal data + the input
line; 0x92 rides some terminal-data bursts; 0x82 is only on the 0xFD notify. **What does each
bit mean (0x02, 0x04, 0x10, and the top nibble 0x80)?**

## What I need back

For each of the three fields:

1. **Bit-level meaning** - decode the bitfield (which bits mean what), citing the ND/XMSG source
   or header layout if you can. Mark anything inferred as INFERRED.
2. **Canonical enum member names** - one per distinct value above, each with a one-line meaning.
   Prefer names that describe the *bit semantics* (e.g. a `[Flags]` enum) over opaque
   per-value names, if the field is really a bitfield. If a value is a combination, give me the
   flag composition instead of a standalone name.
3. **Verified vs inferred** - tag each name/meaning as VERIFIED (grounded in source/spec) or
   INFERRED (pattern-matched from captures).
4. **Corrections** - flag any value I've mis-grouped, any missing value you've seen on the wire,
   and resolve the two anomalies: (B) the 0x54 role, and (A) whether frameClass is just
   XMCSM>>16.

Target: C# enums. `frameFlags`/`role` are `byte`; `frameClass` is `ushort` (or dropped if it's
XMCSM>>16). If `frameFlags` or `role` is a bitfield, tell me so I make it `[Flags]`.
