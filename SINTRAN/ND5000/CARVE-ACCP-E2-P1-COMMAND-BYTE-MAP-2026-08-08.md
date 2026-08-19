# Carve — ACCP E2-P1: all 46 dispatcher arms mapped to command bytes (2026-08-08)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\CARVE-ACCP-E2-P1-COMMAND-BYTE-MAP-2026-08-08.md`
**WSL path:** `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND5000/CARVE-ACCP-E2-P1-COMMAND-BYTE-MAP-2026-08-08.md`
**Reads with:** `CARVE-ANSWER-OCTOBUS-ACCP-COMMAND-DISPATCH-AND-RTEST-2026-08-02.md` (the 46 arm
addresses and the three-way confirmation), `HANDOFF-ACCP-E2-PRIORITY-PLAN-2026-08-04.md` (why this
was P1).

## Method — verified, not assumed

Direct byte read of `E:\Dev\Ronny\NDInsight\Installation\Communication\OctobusAccp\eprom\octo.bin`
(131072 bytes) at each of the 46 arm addresses from the 2026-08-02 carve. Every single site reads
`0C 00 00 <imm>` = `cmpi.b #imm,D0` — zero misses, so file offset == ROM address holds for this
range (independently confirmed: the 2026-08-02 doc's own RTEST disassembly at `0x6616` shows
`cmpi.b #0x30,D0`, and this read returns `0x30` at `0x6616`).

## The map — all 46 arms, in dispatcher chain order

| Arm | Cmd | Name (source) | | Arm | Cmd | Name (source) |
|---|---|---|---|---|---|---|
| 4D50 | 0x13 | LOCSM (023B) | | 5DC0 | 0x35 | RAIB32M [D] (no CM*) |
| 4EDC | 0x14 | CMDWW (024B) | | 5E64 | 0x26 | — |
| 4FC0 | 0x15 | — | | 5ECE | 0x27 | — |
| 519C | 0x16 | CMDRW (026B) | | 5F38 | 0x34 | LAOB32M [D] (no CM*) |
| 52C6 | 0x3C | — | | 5FD6 | 0x28 | CMRAS (050B) |
| 547E | 0x3B | — | | 6016 | 0x29 | CMLDM (051B) |
| 558A | 0x36 | STARTMIC (066B) | | 608C | 0x2A | LCON (052B) |
| 562E | 0x1C | STOPMIC (034B) | | 60F6 | 0x2B | CMWMP (053B) |
| 568C | 0x1D | CONTMIC (035B) | | 6178 | 0x2C | CMRMP (054B) |
| 56BC | 0x17 | — | | 61F4 | 0x3A | — |
| 56EA | 0x1F | ALIVE (037B) | | 6326 | 0x2D | CMSET (055B) |
| 5736 | 0x0F | — | | 6390 | 0x37 | — |
| 57E8 | 0x0E | CMSYSPAR (016B) | | 63B8 | 0x38 | — |
| 583A | 0x0D | — | | 6408 | 0x39 | CPURES (071B) |
| 58A4 | 0x18 | — | | 6438 | 0x1E | RESTMIC (036B) |
| 5980 | 0x11 | LPARP (021B) | | 6504 | 0x31 | ENKICK (061B) |
| 59B6 | 0x12 | VPARP (022B) | | 6534 | 0x32 | DISKICK (062B) |
| 5A46 | 0x20 | — | | 6562 | 0x10 | — |
| 5AB0 | 0x21 | CMLMI (041B) | | 65B6 | 0x1B | RUNTST (033B) |
| 5B38 | 0x22 | CMRMI (042B) | | 6616 | 0x30 | RTEST (060B) |
| 5BC8 | 0x23 | CMBUS (043B) | | 6644 | 0x3D | CMRPR (075B) |
| 5C44 | 0x33 | CMBUF (063B) | | 66B6 | 0x3E | — |
| 5CC0 | 0x24 | — | | | | |
| 5D56 | 0x25 | — | | | | |

**⚠️ CORRECTION 2026-08-09:** the four names above were fixed against the AUTHORITATIVE source —
`AccpCommandChannelTests.CarvedCommandNumbersAgreeWithTheDispatcherChain` (a passing test) and
`CM-SYMBOLS-ARE-THE-OCTOBUS-ARM-CODES-2026-08-03.md`, which supersede the 2026-08-02 table this map
originally copied. Two names were WRONG and two "unnamed" arms were actually already identified:
`0x1B` = **RUNTST** (033B, reads no parameters — cannot be STARTMIC), `0x2A` = **LCON** (052B,
SINTRAN alias CMLDC "load decoder"), `0x13` = **LOCSM** (023B), `0x36` = **STARTMIC** (066B, the
real one). So there are **15 named**, not 13, and the worklist below is **31**, not 33.

Names are the 15 confirmed against SINTRAN `CM*` symbols + ND-05.020.01 §5.3; "—" = no name yet.

## What the map shows

- **Three contiguous runs, no gaps: `0x0D–0x18` (12), `0x1B–0x2D` (19), `0x30–0x3E` (15) = 46.**
  This confirms the 2026-08-02 doc's run observation exactly. Absent: `0x19`, `0x1A`, `0x2E`,
  `0x2F` — no arm tests these.
- All 13 externally named commands land on the expected bytes. **Zero drift** between the ND-100
  carve and the firmware immediates.
- The dispatcher chain is NOT in numeric order (e.g. `0x3C` at the fifth arm) — SO the chain order
  itself may encode something (most-frequent-first?) worth one look when carving.

## The E2-P2 worklist — remaining command bytes (updated 2026-08-09)

**23 NAMED+locked this campaign:** 0x14 CMDWW, 0x16 CMDRW, 0x0F CMTEC, 0x10 CMREA, 0x20 CMLMA,
**0x21 CMLMI, 0x22 CMRMI, 0x23 CMBUS,** 0x24 CMR16, 0x25 CMR32, 0x26 CML16, 0x27 CML32, 0x28 CMRAS,
0x29 CMLDM, 0x2B CMWMP, 0x2C CMRMP, 0x2D CMSET, **0x33 CMBUF,** 0x37 CMLOO, 0x38 CMSPE, 0x3A CMTES,
0x3B CMCCD, 0x3D CMRPR. **8 behavior-carved, NO `CM*` symbol:** 0x0D, 0x15, 0x17, 0x18, 0x34, 0x35,
0x3C, 0x3E — of these **0x34/0x35 are direction-resolved** to LAOB32M/RAIB32M `[D]` (see below);
the other **6 are truly nameless** (0x0D, 0x15, 0x17, 0x18, 0x3C, 0x3E).

**2026-08-09 §5.3 promotion:** 0x21/0x22/0x23/0x33 were held collisions; the ND-05.020.01 §5.3.27–.42
section-order run (verified against the real manual) + behavior corroboration promoted them to
CMLMI(LMIR), CMRMI(RMIR), CMBUS(TBUS), CMBUF(TBUF).

**2026-08-09 via-memory pair resolved:** 0x34/0x35 have no `CM*` symbol, but the carved data
direction fixes the manual mnemonic — **0x34 = LAOB32M** (memory→register load, §5.3.37) and
**0x35 = RAIB32M** (register→memory read, §5.3.34), the via-memory twins of 0x27 CML32=LAOB32D and
0x25 CMR32=RAIB32D. §5.3.34/.37 verified against the real manual. These are `[D]` derived, not
CM*-symbol locks, so they stay out of the guard test. See the E2-P2 log's §5.3 note. The remaining 6
nameless arms need a hardware-code source or a real ND-100 traffic capture, not the symbol table.

**✅ ALL 46 DISPATCHER ARMS NOW CARVED (2026-08-09).** The last fresh block (0x28–0x2D, which the
chain-order carve had skipped when it jumped from 0x27 into the 0x33+ block) is done:
```
0x28 @ 0x5FD6 CMRAS (050, read 1 word from HW)     0x2B @ 0x60F6 CMWMP (053, write 2 longs -> MFbus)
0x29 @ 0x6016 CMLDM (051, word -> 2 byte ports)    0x2C @ 0x6178 CMRMP (054, read long -> reply long)
                                                    0x2D @ 0x6326 CMSET (055, 3-word block -> 0x7ACE)
```
Each is the SOLE CM* symbol at its octal value and its carved behavior does not contradict the name,
so all five lock (CMWMP/CMRMP are a clean write/read long-word pair). The remaining worklist is just
the 12 held items awaiting driver-side name disambiguation. Behavior rows live in
`CARVE-ACCP-E2-P2-HANDLER-LOG-2026-08-09.md`.

Note: the real ND-100 boot trace (`AccpCommandChannelTests`) exercises only the NAMED control
commands (identify/params/microcode-control/selftest). The 31 unnamed arms do NOT appear in a basic
boot trace — that is *why* they are unnamed — so ordering them "by real traffic" yields no signal;
they are a static 68000 carve (octo.bin in Ghidra), best taken in dispatcher-chain order. First
truly-unnamed arm in chain order is **0x14 @ 0x4EDC**.

Per the E2 priority plan: carve these ordered by what SINTRAN actually sends (capture real boot
traffic first), lock each name in `AccpCommandChannelTests` as you go. No estimate for that work
exists, on purpose.

## Index pointer

Requested per the product-routing rule: one line for this file in the folder index, actioned by
the reference owner (this doc does not touch `ACCP-COMPLETE-REFERENCE.md`).
