# Handoff — ACCP: the priority plan for the one remaining open item (E2)

**Date:** 2026-08-04
**From:** the ACCP emulation effort (RetroCore `Machines.Accp` + the ND5000 carves in this folder)
**To:** whoever picks up ACCP firmware RE next
**Reads with:** `HANDOFF-ACCP-COMMAND-DISPATCH-2026-08-02.md` (what is carved) and
`CARVE-ANSWER-OCTOBUS-ACCP-COMMAND-DISPATCH-AND-RTEST-2026-08-02.md` (the evidence)

---

## 1. Why this handoff exists

The 2026-08-02 handoff says what landed and what is open. It does not say what to do first.
This one does, and nothing else. If you have read the other two documents you can start from
section 3 here.

The forward plan
`E:\Dev\Repos\Ronny\RetroCore\Nuget\_shared\docs\ACCP-FORWARD-PLAN-2026-08-01.md`
ran phases A through F. **A, B, C, D, E1, E3 and F are all closed.** One item is open: **E2**.

## 2. Phase outcomes, in one table

| Phase | Outcome |
|---|---|
| A — trust the suite | Closed. The intermittent host crash never reproduced across four clean single-invocation runs (82, 96, 98, 101 tests). The old "memory pressure" hypothesis is **unsupported**; concurrent runs of the same test project fighting over `bin\Debug` is better evidenced but still **UNVERIFIED** |
| B — observability | Closed. B1 (firmware trace) and B2 (Sdl2CliDemo wiring) were already done before the plan was written. B3 was real: chips were invisible to `chip.list` because `MemoryBuilder.Device()` does not register for discovery, unlike `Chip<T>()`. Landed with `PeekByte` overrides |
| C — CMD-3 traffic | Closed. Real SINTRAN commands driven against the running card; the ack/nak convention settled against ND-05.020.01 |
| D — the `0x03` byte | **Closed as still open.** Two of three readings eliminated by experiment; the third narrowed but NOT proven, and it cannot be proven from this side |
| E1 — name the commands | Closed. 16 of 46 named from the ND-100 carve, four of those confirmed against the running card |
| **E2 — carve the rest** | **OPEN. The only open item** |
| E3 — RTEST discrepancy | Closed. There was no discrepancy; the measurement was contaminated |
| F — housekeeping | Closed. Both items decided |

## 3. The priority plan for E2

### P1 — map the 46 arms to command bytes

Read the `cmpi.b #imm,D0` immediate at each of the 46 enumerated arm addresses (they are listed
in the carve doc). Mechanical, no judgement calls.

Do this **first** because it is cheap and it sizes everything below it: it turns "~30 unknown
handlers" into a named list of exactly which command numbers have no name. It also re-checks the
16 already named from the ND-100 side, so a drift between the two carves surfaces immediately
instead of silently.

### P2 — carve the unnamed handlers

For each: what it reads, what it touches, what it replies. This is the real work and it deserves
a focused session, not a squeezed-in half hour.

**Order within P2 by what SINTRAN actually sends, not by command number.** The commands that
matter to a working card should be carved first; a numeric sweep spends the same effort on
commands nothing ever issues.

### P3 — lock each carved name in a test

`AccpCommandChannelTests`. Do this **per handler as you go**, not batched at the end. A name with
no test is a claim, not a fact — and `CarvedCommandNumbersAgreeWithTheDispatcherChain` already
sets the precedent of asserting cross-source agreement rather than remarking on it.

### P4 — write the results into this folder

Per the product-routing rule. Do **not** edit `ACCP-COMPLETE-REFERENCE.md` — it is another
agent's live file. Write your own dated carve doc and request the index pointer in a handoff,
the way section 3 of the 2026-08-02 handoff does.

## 4. What is deliberately NOT on the priority list

**The `0x03` content byte.** It is narrowed to one reading and **cannot be resolved from this
side**. It needs the Octobus Driver Programming Guide (DVT, 15 Oct 1986) or a second card's ROM;
neither is in this repository, and `OctobusAccp\eprom\51200J.bin` / `51201J.bin` are the two
halves of `octo.bin` itself, not a second card. Picking it up again without one of those
documents would re-derive the same ceiling. **If either document turns up, this becomes P1.**

**The index pointer request** from the 2026-08-02 handoff is the reference owner's to action,
not work for the next RE session.

## 5. Estimate — and its honest limit

P1's cost I am confident about: 46 reads, one immediate each.

**P2 I cannot size.** It is roughly 30 handlers of unknown individual difficulty, and there is no
basis for estimating it until P1 is done and the command list is visible. Do not carry a P2
number forward from anywhere — there isn't one.
