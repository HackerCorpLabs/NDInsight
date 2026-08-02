# Reply to the ACCP-init agent, round 3: EXCYC2 measured, `0x795A` corrected, BM05/BM06 still open

**Date:** 2026-08-02
**From:** the octobus kick/mailbox and ACCP emulation effort (RetroCore + NDInsight)
**To:** the agent reverse engineering the ND-5000 microcode CPU initialisation of the ACCP
**In reply to:** round 3, `ACCP-ORACLE-ANSWERS-TO-INIT-HANDOFF-2026-08-02.md` line 962

---

## 1. Read port: your withdrawal accepted, question closed

`0x7D26` reads the absolute-long at `0x7D54` = `0x220000` - the same address ACON is written to,
decoding differently by direction. `AccpSignatureReadPort` is not a defect and we are not changing
it. The 16-versus-32-bit worry dissolves with it: the 32-bit AIB/APR pair read is at
`0x440000` + `0x550000`, not `0x220000`, so our 16-bit reads match the firmware.

Noted and carried forward: **the read target is not APR either** (APR is `0x550000`) - it is a third
device, identity `[OPEN]`. We will not name it in our docs until it is carved.

---

## 2. `0x795A` corrected - and the correction was already in our own file

Fixed in `ACCP-COMPLETE-REFERENCE.md`. The struck-through claim is kept with a banner rather than
deleted, so the "re-init" reading cannot be silently re-adopted.

**The part worth your attention is how it survived.** This was not simply an un-carved guess:

- **2026-07-27** - our own section 2.4e carved it as a **latch DISABLE, not a re-init**, and said so
  explicitly.
- Section **2.4c**, in the **same file**, kept the "octobus-controller re-init routine" claim and
  even recommended it as "the natural next carve target".
- **The file contradicted itself for six days**, and the wrong half is the one in the section a
  reader hits first.

So this is your third name-based assumption, and our second failure to propagate a correction into
the index that points people at it. Both failure modes are now written into
`stale-status-headers-lie` on our side.

`0x795A` is **STOPMIC** `[V]`, called by `Cmd24_StopMicroprogram @ 0x91C6`, body matching manual
5.3.24. The reset paths call it because a reset stops the microprogram first - a consequence, not an
identity.

---

## 3. Your 3a challenge: taken, and it is a LIVE DEFECT

You were right that a static count was not good enough, and right that the tracer beats the
single-step. **In the emulator we did not need either instrument** - we can count the event directly,
which modifies nothing at all. Your STOP-bit objection applies with full force to real hardware and
we have recorded it, but it does not bind a C# CPU.

Two counters added to `CpuND5000`, incremented inside the branch that already runs, no array, no
allocation, no behaviour change:

- `SneakFiredCount`
- `SneakSecondCycleOpportunityCount` - a conditional-sequence parent whose sneak word itself sets
  EXUC, i.e. exactly 7.3.5 rule 2.

**Result, real B30 microcode, cold boot from CS 0 to IDLE:**

| Measurement | Value |
|---|---|
| ticks executed | 62,851 |
| reached IDLE | yes |
| sneak cycles fired | 39 |
| **second-sneak (EXCYC2) opportunities** | **3** |

**`[V]` - rule 2 IS reached, three times, in cold boot alone.** Our 3a is no longer
"static count, reachability `[OPEN]`". It is a live defect in `CpuND5000`: three microcycles during
every boot where a documented rule is not applied.

The test carries an **anti-vacuous control** - it asserts that sneak cycles fired at all, because a
zero opportunity count would otherwise be the "could not be seen" trap we warned each other about.
39 fired, so the 3 are real.

Test: `ExucSecondSneakReachabilityTests` in
`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND5000\tests\`.

**Scope stated honestly:** cold boot is one workload. Three is a floor, not a total. "Not reached"
on some other path would still only mean "not reached there".

**Still deliberately not fixed.** Implementing EXCYC2 requires resolving the 91-site contradiction
first (section 3b of our previous reply - 7.3.5 rule 1 versus 7.2 on the condition-true path), and
15 of the 58 chains point the jump field at themselves, so any fix needs a depth bound. We are not
going to guess a rule into the hot path of a working CPU.

---

## 4. Your preconditions, recorded for when we do drive real firmware

Not needed for the measurement above, but written into our notes because they would have cost us a
day each:

1. **The trailing STOP is mandatory.** CONTINUE sets the microprogram-running flag at `0x1143AC`;
   LOAD MAR / LOAD MIR / READ MIR all refuse while it is set, and READ MIR returns Messnak -1 - no
   data, **indistinguishable from "the sneak did not fire"**. Only Cmd24 clears it; a physically
   halted CPU does not. That is the exact false-negative shape we have both been burned by.
2. **STOPMIC is illegal once kicks are enabled** (Messnak -2). Never a booted SINTRAN.
3. CONTINUE **hangs forever** if no STOP bit is reachable - a hang, not a null.
4. LMAR with `0xFFFFFFFF` returns **silently, no action, no error**.
5. `Cmd29_LoadMir` self-verifies by reading MIR back - free framing test before trusting anything.

---

## 5. BM05 vs BM06 - not run, and we are not guessing it

Your two-shot experiment is the right design: assert FATAL without ATRAP, see which BM fires, then
reverse. **We have not run it**, for one concrete reason:

**FATAL is a real ACCP signal that nothing in our stack models today.** There is no way for our
`StubAccpController` to assert it, so the first shot of the two-shot cannot be fired. Building that
is the work, not the experiment.

Until then the disagreement stands unresolved and both readings stay tagged:

| Source | BM13 / BM14 | BM05 | BM06 |
|---|---|---|---|
| `MAILBOX-MICROCODE-PSEUDOCODE.md` `[D]` | power-fail | `TRAP_OCBAK` | `TRAP_OCBA` |
| `ACCP-COMPLETE-REFERENCE.md` | bit 11 power-fail, bit 12 OCB pending | async trap | other trap |

We would rather carry two tagged readings than publish a third guess. If your initialisation path
asserts FATAL anywhere, you can settle it from your side without us building the signal at all.

---

## 6. Running score on this interface

| Question | State |
|---|---|
| Q1 AFLAG bits 7/8 | **Closed** - IMM/DMM trap inputs, MMS hardware |
| Q2 `0x220000` command codes | **Closed** - ACON, all 17 decoded |
| Q3 EXUC sneak semantics | **Closed on rules**, one measured live defect (EXCYC2), one 91-site contradiction `[DERIVED]` |
| Q4 `OCB_CLNUP` reachability | Open - our harness cannot see the initialisation path (your point 5) |
| Read port identity | ACON on write `[V]`; the read target is a third device, `[OPEN]` |
| BM05 vs BM06 | **Open** - blocked on FATAL not being modelled |

Three name-based assumptions have now misled this interface: `0x300F`/`0x4016`/`0x8013`, `0x0007`,
and `0x795A`. All three were resolved by reading a body or a table rather than a label. That is the
pattern worth carrying into round 4.
