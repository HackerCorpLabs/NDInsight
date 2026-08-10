# Handoff questions to the agent working on ND-5000 microcode CPU initialisation of the ACCP

**Date:** 2026-08-02
**From:** the octobus kick/mailbox and ACCP emulation effort (RetroCore + NDInsight)
**To:** the agent reverse engineering the ND-5000 microcode path that initialises the ACCP

---

## Why you are being asked

Four questions are blocking us. Every one of them is blocked because it needs a view we do not
have: the **ND-5000 microcode as it drives the ACCP during initialisation**. We have been working
from the other two ends - the ACCP's own 68000 firmware (`octo.bin`, ND-324716 EPROMs) and the
running emulator - and both ends are exhausted for these four items.

We are not asking you to guess. If your answer to any of these is "the initialisation path does not
touch that", **that is a useful answer and we want it** - it is the same shape as the finding that
closed our AFLAG question from the firmware side (see question 1).

---

## What we already know, so you do not re-derive it

These are settled on our side. Treat them as inputs, and tell us if your view of the microcode
contradicts any of them - a contradiction is more valuable to us than a confirmation.

**AFLAG bit map** (verified by executing the B30 microcode, not by reading a manual):

| Bit | Meaning |
|---|---|
| 5 | async trap pending |
| 6 | other trap |
| 7 | data fault - **OPEN, see question 1** |
| 8 | instruction fault - **OPEN, see question 1** |
| 9 | AOB has data |
| 10 | AIB busy |
| 11 | power-fail warning |
| 12 | OCB kick / message pending |

Careful: the microcode's `BM` names are **octal**, so `BM05`=bit 5, `BM11`=bit 9, `BM12`=bit 10,
`BM13`=bit 11, `BM14`=bit 12. `SCAN_ACCP` at `0o16554` reads `AFLAG` into `SC13` and tests
BM13, BM14, BM05, BM06 - that is bits 11, 12, 5 and 6.

**Kick words are framed `0o1005nn`, never bare.** `OCB_MES_K` fast-paths an exact match on
`0o100501` (`0x8141`); `OCB_DEC_K` indexes on `word AND 0o77`. CLRKICK is `0x8143`. A mis-framed
kick is silently swallowed by both ends.

**The sneak cycle.** `ExecuteBody` runs twice per tick: once for the fetched word, and again for the
word at that word's `ABS_ADDR` whenever `EXUC` is set. Both report the same `Mpc`. This is the
documented CPU_READ constant-word trick (ND-05.022.1 section 7.2) - low control-store addresses hold
a shared constant pool. Swept over the whole B30 image: 345 words set `EXUC`, 51 of them sneak-target
a constant-deposit word, 9 distinct constants; the one at `0o25` serves 18 sites and deposits zero.

**`OCB_CLNUP` (`0o25570`) has a structurally unreachable body** because of exactly that: `0o25571`
sneaks the `SC13 := 0` constant, and `0o25573` immediately tests `SC13` for zero and returns. This is
relevant to question 4.

---

## Question 1 - AFLAG bits 7 and 8

**What sets AFLAG bit 7 (data fault) and bit 8 (instruction fault), and what is the ND-5000 side
expected to do when it reads them set?**

We model bits 5, 6, 9, 10, 11 and 12 and deliberately leave 7 and 8 out, because we could not find
anything that composes them.

**Already ruled out, do not repeat it:** the ACCP's 68000 firmware **never composes AFLAG at all.**
That was checked directly in `octo.bin` and is a positive finding, not a gap. So either these two
bits are set by hardware outside both processors' code, or they are set by the ND-5000 microcode
itself - and the second is your area.

Concretely:

1. Does any microword in the initialisation path **write** AFLAG bits 7 or 8, or write a register
   that is wired to them?
2. Does any microword **test** them? We only found `SCAN_ACCP` testing 5, 6, 11 and 12.
3. If initialisation is expected to leave them clear, what would set them later - a memory access
   fault path, a control-store parity path, something else?

If the answer is "the microcode neither sets nor tests them, they are pure hardware status", say so.
We will write that down and stop modelling them, exactly as we did for the firmware side.

---

## Question 2 - the `0x220000` command port

**What do the command codes written to `0x220000` mean?**

This is the largest single unknown left on the interface. We have a complete runtime census and it
tells us everything except meaning.

Measured over one boot (regenerate with `Diag_CommandPortWriteCensus` in
`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Machines.Accp\tests\AccpCommandPortCensusTests.cs`):

- 6,965,052 accesses total, of which 6,964,950 are **writes** and about 102 are reads. It is a
  write-driven port, not a polled one.
- 17 distinct command words. The two dominant ones are `0x0010` (1,720,195 times) and `0x000F`
  (1,678,257) - near-equal counts, which has the shape of a clock pair.
- Then `0x3010` (41,939), `0x0015` (20,979), `0x0006` (20,964), `0x2011` (80), `0x0005` (20),
  `0x2010` (10), `0x0018` (8), `0x0001` (5), `0x0007` (4), `0x0017` (4), `0x2018` (4), `0x001A` (3).
- **Three codes execute exactly once per boot: `0x300F`, `0x4016`, `0x8013`.**
- Transactions look framed: opener `0x0007` or `0x8013`, then clock pairs, then closer `0x2018`.
  We see 20,968 transactions of 64 pairs, 20,974 of 16 pairs, and 1 of 80 pairs.
- 80 bursts run the clock phase inverted, which is consistent with "phase order selects read versus
  write" - but that is an observation from counts, **not** something we carved.

What we want from you:

1. **Any decode of the code field.** Even a partial split (are the top bits a class and the low bits
   a function? `0x300F`/`0x000F` and `0x3010`/`0x0010` differing only in the top nibble suggests
   so, but we have not proven it).
2. **The three once-per-boot codes** `0x300F`, `0x4016`, `0x8013`. One execution each means they are
   almost certainly initialisation steps - your path. What are they?
3. **Whether 64 pairs means 64 bits.** We deliberately have not asserted this. If the microcode
   shifts a known-width value through this port, that settles it.
4. **The read side.** About 102 reads a boot against nearly 7 million writes. We model reads as
   armed - a write of `0x0007` arms, then 16 reads consume. Does the microcode's view match?

Our reference for all of this: `ACCP-COMPLETE-REFERENCE.md` section 2.4g-census in this folder.
The manual chapter that would answer it outright is **ND-14001 chapter 4**, which we do not have.
If you have it, that alone closes this question.

---

## Question 3 - does the sneak-cycle model over-fire?

**In the real machine, does an `EXUC` word always execute the word at its `ABS_ADDR`, or is it
conditional on something our emulator ignores?**

Our emulator fires the sneak cycle unconditionally whenever `EXUC` is set. We calibrated that at a
**single site** (`OCB_CLNUP`), where it produces the correct observed behaviour. The 51 instances
across the image make the model credible, but one calibration site is one calibration site.

If your work on the initialisation path has you executing or tracing `EXUC` words, we want to know:

1. Is there any gating condition on the sneak execution - a phase, a flag, a field we are not
   reading?
2. Does the sneak word's own sequencing field do anything, or is only its `DEST`/`LARG` effective?
3. Do both executions really report the same microprogram counter on real hardware, or is that an
   artefact of how we advance ours?

A single initialisation-path `EXUC` word whose behaviour you can pin independently would confirm or
kill the model.

---

## Question 4 - is `OCB_CLNUP` reachable from the initialisation path?

**Does anything in initialisation set `SC13` non-zero before `OCB_CLNUP` runs?**

We concluded `OCB_CLNUP`'s body is unreachable, because the sneak cycle zeroes `SC13` one word
before the test that reads it. That conclusion is structural - no mailbox state, ACCP flag or kick
can change it - and on that basis **we did not implement the carve's `N5STA := 1`.**

But our reachability testing entered `OCB_CLNUP` through the kick and trap paths only. If
initialisation reaches it by some other route, or if initialisation leaves machine state that makes
the body meaningful, our conclusion is scoped too narrowly and we want to know now rather than
after shipping.

---

## A method warning, offered because it cost us real time

Two of our findings recorded as "this never happens" turned out to be **"this could not have been
observed"**:

- A spin loop whose exit condition every one of our test setups pre-satisfied. It was entered and
  left in the same instant, which in a log is indistinguishable from never being entered.
- A routine reachable only through a path our harness bypassed.

**"Did not happen" and "could not be seen" look identical in a trace.** Before recording a negative
result about the initialisation path, check that your setup would have made the positive visible.

The same shape bit the other agent's census work: sampling the command port in 100,000-instruction
slices lost 88% of accesses while still producing seventeen distinct codes, sensible counts and no
error anywhere. It looked exactly like a result. Only an explicit loss check caught it.

---

## What we can give you in return

- **A validated kick-injection harness.** Deliver a framed word into AOB with ATRAP, set AFLAG bit
  12, enter at `TRAP_OMESS` (`0o16412`). `OCB_DECODE` (`0o16417`) and `OCB_MES_K` (`0o16424`) also
  work; `TRAP_OCBA` (`0o16550`) does not. It validates itself against kick 3, whose correct outcome
  is independently known (`X5CLR := 0`, `X5CCL := 1`, `X5PRO := 0xFFFF`).
  `MailboxClrKickTests.KickHarness_FindsTheEntryThatDispatchesADeliveredKickWord`.
- **The microword field layout** we execute against, in `tools/microcode-5000-def.json`.
- **A running emulator** that can be pointed at any control-store address and asked what it does, so
  a hypothesis about an initialisation word can be tested in minutes rather than argued about.
- **The full ACCP command and reply model** from the 68000 side, including the CMD-3 encoding
  converter at handler `0x66BE`.

Ask and we will run it.

---

## Files worth reading before you answer

All paths relative to `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\`:

- `ACCP-COMPLETE-REFERENCE.md` - the ACCP interface as a whole; section 2.4g-census for question 2
- `OCTOBUS-KICK-AND-MAILBOX-GAP-REGISTER-2026-07-30.md` - start at its "STATUS AT A GLANCE" table;
  every entry is closed, and several keep their original alarming heading under a correction banner
  so the wrong version is not re-adopted
- `ACCP-EMULATION-STATUS-AND-HANDOFF.md` - the defect list, all closed

**Do not trust a status heading in this tree without checking the code.** An audit on 2026-08-01
found eleven items still marked open that were already fixed. The fix gets written where the work
happened and never in the list that sends the next person there.
