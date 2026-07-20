# Handoff - 3022 / swapper track, session ending 2026-07-20

**Full path:** `SINTRAN/ND500/HANDOFF-3022-SWAPPER-TRACK-2026-07-20.md`

Read with: [`OPEN-QUESTIONS-REGISTER-2026-07-20.md`](OPEN-QUESTIONS-REGISTER-2026-07-20.md) (what we
do not know) and [`ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md`](ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md)
sections 12d-12l (the detail, with citations).

---

## 1. Where D4 stands

**The real ND-500 swapper executes on the functional `CpuND500` under live SINTRAN III L.** Nothing on
that path is faked any more - the swapper injection, the synthetic "swapper alive" announce and the
3MONCO "parked but alive" intercept were all deleted.

Swapper PC progression across the session, each stop matched against the carved disassembly:

| PC | What it is | Fixed by |
|---|---|---|
| `0x04` | `init` - builds its frame at `0x08024254` | mapping the located PSEG |
| `0x11` | first data write | data capability into segment 1 |
| `0x52`/`0x58` | `call $0x080081A5` | program capability into segment 1 |
| `0x82EE` | `h riom` - the DMA intake | RIOM operand fixes |
| **`0x913B`** | **current stop** - null deref of a pointer from its message | open (A5) |

SINTRAN recognises it throughout: its trap decoder names `Shadow process 5SWAP`, and it answers the
swapper's `MON 377B` with real data (`@0x240B0 := 5`, `@0x240B4 := 0x00210718`).

## 2. What is now established [V]

- **Placement.** SINTRAN puts the swapper executable at MPM physical `0x06F800` (19 pages,
  byte-identical to `SWAPPER-K01.PSEG`) and its DSEG at `0x24800` (content signatures at `+0x2408C`
  and `+0x26198` match the file). **Both are present and correct.**
- **Layout source.** SINTRAN announces it in the two page tables it DMAs: `0x6E800` = PROGRAM table
  (`00DF 00E0 ...` -> `0x06F800`), `0x6E000` = DATA table (`0049 004A ...` -> `0x24800`, 107 pages).
  Derive from these, never from RESIWR extents.
- **Link segment.** The swapper is linked at **segment 1**; code and data share it, separated by the
  I/D split. Its own operands prove it (`init $1000441124` = `0x08024254` = the documented stack
  bottom; `call $1000100645` = `0x080081A5`), and both matched live traps exactly.
- **The mailbox answer path works.** MON 377B round trip, write-back into ND-500 memory, and
  SINTRAN's reply are all correct.

## 3. Emulator bugs fixed this session (RetroCore, committed)

1. **RIOM source truncated** - read as halfword, mangling `0x00210718` to `0x0021`. Now `DataType.W`.
2. **RIOM destination dereferenced** - used the operand's contents (0) instead of its effective
   address, sending the DMA to ND-500 address 0. Now `Operands[1].EffectiveAddress`.
3. **RIOM 16-bit ceiling and `0xFFFF` wrap** - the ND-100 bridge was `ushort`-wide end to end.
   Widened to 24 bits with additive `uint` overloads.
4. **RIOM wrote the Z flag** - the manual says "Data status bits: Unaffected".
5. **`instructions.json` (both copies)** described RIOM operand 2 as `Source/Read` and allowed
   CONSTANT. This was the root cause: it let the old test encode a CONSTANT buffer, where value and
   address coincide, so the bug passed its own test. Now `Destination/Write`, CONSTANT removed.
6. **PTE protection bit** - `MapExistingPhysicalRegion` wrote `pte |= 0x1` on every entry, treating
   bit 0 as a present bit when it is the protection bit; every mapped page came out read-only.
7. **Mailbox 17B (3DEPR)** was unhandled and answered 5ERANSWER, so SINTRAN re-sent the bring-up
   cycle forever.

All three RIOM operand bugs were independently confirmed by the B30 microcode, where RIOM is a
**microcoded copy loop, not a DMA engine** (`RD,POF` physical read + `WRITE`, count in the
microcode's own `LC`).

## 4. Retracted claims - do not re-adopt

1. **"The swapper is control-store microcode."** Wrong. Microcode is only what
   `> Loading Control Store` puts in the CPU's control storage; ND-500 code is ordinary executable
   code in an executable segment. This one cost a whole session's design work.
2. **"The swapper's DSEG is never loaded."** Wrong - it is at `0x24800`. Density scans cannot prove
   absence (a mostly-zero 218 KB segment never reaches a 25%-non-zero bar).
3. **"Control-store path INSTEAD OF the classic mailbox."** Wrong framing - they are steps 0 and 3 of
   one `500IN` state machine, not two arms of a branch.
4. **A RIOM "fix" using the effective address** was applied, then reverted when the existing tests
   contradicted it, then re-applied only after the microcode and the manual's `/w/` notation settled
   it. The intermediate revert was correct practice, not churn.
5. **"WIOM"** does not exist in any manual, index or opcode table.

## 5. Next actions, in order

**Refactored into a phased plan:** [`D4-PLAN-PHASES-AND-TASKS-2026-07-20.md`](D4-PLAN-PHASES-AND-TASKS-2026-07-20.md)
is the plan of record (phases 0-6, per-task route + open-question IDs + acceptance). The list below is
the same content in prose; the plan doc supersedes it for tracking.

1. **LIVE probe for A5:** capture `0x420E30` **at the moment of the RIOM**, not after RUN. That
   decides whether the swapper is being handed an empty message or whether we lose it in between.
2. **Answers from the microcode track** for A1 (who builds PCB/PST), A2 (segment number at 3START),
   A3 (PSTP). These unblock the MMU model; until then the emulator hand-builds capabilities, which
   cannot converge.
3. **B1 - the transport** that delivers PSEG/DSEG. Also the octobus track's Q4, so it pays twice.
4. **C2 - RIOM REGISTER mode.** Location already pinned: mode dispatch around CS `001600-001716`;
   follow the LADDR path from RIOM's operand stub at CS `000745`.

## 6. Working notes for whoever picks this up

- **The harness is flaky.** `Nd500_D4_RunDomain_RealCpu_Capture` both passes and dies with "Test host
  process crashed" and an empty progress log for identical code. **Take two samples before believing
  any single run** - one session wrongly blamed a code change for pure harness noise. A crash with no
  exception text is usually an uncatchable `StackOverflowException` on the CPU thread, and the CPU
  trace file is useless after a crash because it flushes only on close.
- **Diagnostics available** in the harness/CPU: `LastProtectionViolation` and `LastPageFault` (each
  naming the exact MMU check that rejected the access), `LastRiomDecode` (operand modes + effective
  addresses), `LastSwapperMapReport`, and `MonCallLog` (the whole MON 377B exchange including
  SINTRAN's write-back). These turned three guesses into three verified diagnoses.
- **Both repos carry other people's uncommitted work.** Stage by explicit path; never `git add -A`.
- **Failure attribution is not possible from that tree** - it holds ~50 modified files from other
  sessions. The 4 ND-500 suite failures (SHR) are *plausibly* pre-existing, not provably.
