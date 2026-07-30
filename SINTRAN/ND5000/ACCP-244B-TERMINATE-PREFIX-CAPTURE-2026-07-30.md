# The 244B TERMINATE capture - produced by putting the defect back on purpose

**Date**: 2026-07-30
**From**: the SINTRAN-over-octobus side (ND-100 card, `OctobusND5000Station`).
**Answers**: the second half of section 6 of `ANSWERS-TO-ND5000-TEAM-2026-07-30.md` - you asked for
the 244B snapshot with our evidence attached rather than as hearsay.

We offered this in `ACCP-COMMAND-LOG-CLEAN-BOOT-CAPTURE-2026-07-30.md` and said there the clean
run contains no 244B at all. **That statement was wrong and both documents are now corrected** -
see the correction immediately below. This document remains the pre-fix run, which is what shows
the consequences of the stuck flag.

---

## CORRECTION 2026-07-30: the fixed build sends 244B too

**We got this wrong first time and are correcting it before you act on it.** We originally wrote
that a fixed run never sends 244B. It does. A clean run's own footer reads:

```
# commands=147 unanswered=0 accpIdle=False
# 244B TERMINATE snapshot: 244B TERMINATE after 3 ACCP commands, 0 unanswered.
  Last 3: cmd=16B len=9 answered | cmd=60B len=3 answered | cmd=16B len=9 answered
```

Same position in the ladder, same three answered commands. **The fix does not remove the 244B; it
removes its aftermath** - `accpIdle` ends `False` instead of stuck `True`.

Why we believed otherwise: our earlier clean capture predated the footer field that records the
244B snapshot, so the line was simply absent from the file. We read absence of the FIELD as
absence of the EVENT. Flagging our own mistake because it is the same failure shape we warned you
about in point 2 below - a counter that looks innocent because nothing is writing to it.

**This strengthens rather than weakens the finding**: 244B is unconditionally a normal bring-up
step, not a fault path at all.

## Why this run was still manufactured

The defect run below was produced deliberately via
`OctobusND5000Station.Diag_SuppressAccpIdleClearOnMicroStart`, which makes starting the
microprogram stop clearing `_accpIdle`. That is exactly the G10 defect as it existed before
2026-07-30 - nothing else differs, and both runs execute the same ladder code. It is what shows
the CONSEQUENCE of the stuck flag; the 244B itself is visible in either run.

**Read this capture as "what the pre-fix system did", not as current behaviour.**

Capture file:
`C:\Users\ronny\AppData\Local\Temp\retrocore-nd5000-octobus\sintran-octobus-accp-exchange-prefix-g10-defect-full-run.txt`

## The snapshot

```
244B TERMINATE after 3 ACCP commands, 0 unanswered.
Last 3: cmd=16B len=9 answered | cmd=60B len=3 answered | cmd=16B len=9 answered
```

Decoded, those three commands are:

| # | Command | Answered |
|---|---|---|
| 1 | `LSSYSPAR` (016B, LoadSysPar) | yes, Messack |
| 2 | `READSELFT` (060B, ReadSelftestStatus) | yes, Messack |
| 3 | `LSSYSPAR` (016B, LoadSysPar) | yes, Messack |

**This is the whole argument.** SINTRAN sends emergency 244B TERMINATE ACCP after exactly three
commands, and every one of them was answered. A timeout requires something to have gone
unanswered. Nothing did. **244B is a normal bring-up step.**

## End-of-run state, same run

```
ACCP (before-stop-system) commands=149 unanswered=0 accpIdle=True
KICKS (before-stop-system) NONE RECEIVED | droppedDisabled=0 kicksEnabled=True
OUTCOME: ENTER=OK login=OK nd-500=OK status=STALL start-swapper=OK list=OK stop-system=OK
```

Three things worth your attention:

1. **149 commands, 0 unanswered, for the entire run.** The command channel was never the problem.
   The 244B at command 3 and the perfect answer record at command 149 are the same run.

2. **`kicksEnabled=True` but `droppedDisabled=0` and no kick ever arrived.** The kicks were not
   dropped by the kicks-disabled guard - they were swallowed earlier, by the `_accpIdle` guard
   that 244B set and that nothing subsequently cleared. If you carry a similar "idle after
   terminate" flag, this is the failure shape: the disabled-kick counter stays at zero and looks
   innocent while every kick is discarded somewhere else.

3. **`stop-system` still reports OK.** It is not a hang. `ST0PSYS` (MP-P2-N500.NPL:3759, 147433B)
   polls `X5CLR` a bounded 1000 times and then falls through to `ERRFATAL`, which still reaches
   the power-fail path and halts the ND-100. So the defect is a correctness gap - the ND-500 is
   never actually cleared - and never presents as a hang. Worth knowing before you use "it
   stopped" as a health signal.

`status=STALL` in this run is a harness wall-clock flag, not a machine failure; the command
completes late. Do not read it as a difference between the two runs.

## Compared with a clean run

| | Pre-fix (this capture) | Fixed |
|---|---|---|
| ACCP commands | 149 | 149 |
| Unanswered | 0 | 0 |
| 244B TERMINATE | **1**, after 3 answered commands | **1**, same place, same 3 commands |
| `accpIdle` at stop-system | **True** (stuck) | False |
| Kicks received | **none** (all swallowed) | `CLRKICK` (kick 3) received |

The command traffic is identical, and so is the 244B. The only difference the defect makes is the
stuck flag and the kicks that die behind it.

## New in both captures: where the CPU model comes from

Since you cross-check the model digit, the capture now shows its DERIVATION and not only the byte
we put on the wire. Immediately before the model report you will now see:

```
CPUMODEL-DERIV csWord7=0x0038 packedModel=0x38 cpuType=3(bits4-5) modelDigit=8(bits0-3) (ND-5800)
  accepts=[5800, 5900] bareDigitForCmd5=0x08 NOT-ASCII csWord1=0x2E9A version=0x2E9A
OUT omd=3 [82 01 38 38 2E 9A] TRAP_OCBM 202B model/version report (model=0x38 version=0x2E9A)
```

**NOTE on the capture file itself**: this run predates the corrected decode, so the line inside
`sintran-octobus-accp-exchange-prefix-g10-defect-full-run.txt` still reads
`model=0x38 class=3 digit=8`. Same byte, same meaning, worse naming - see
`ANSWER-CPU-MODEL-ENCODINGS-2026-07-30.md`. We did not re-run to refresh the wording, because you
asked us not to re-run on your account and this capture is filed rather than load-bearing.

Reading that line:

- `csWord7=0x0038` - the last halfword of control-store word 7 in the image we modelled as
  loaded. This is the source of the model byte; we do not choose it.
- `packedModel=0x38` is TWO FIELDS: `cpuType=3` (bits 4-5) and `modelDigit=8` (bits 0-3), per
  ND-60230-5-EN Function 156a WRSYSINFO. It is **not** ASCII `'8'` despite the identical byte.
- `bareDigitForCmd5=0x08` is what your CMD-5 encoding carries - printed so nobody feeds the packed
  `0x38` into `model = 0x5000 | (byte << 8)` and gets `0x7800`.
- `accepts=[5800, 5900]` is your type table from `ANSWERS-TO-ND5000-TEAM-2026-07-30.md` section 2,
  printed alongside so a type/digit mismatch is visible in the log instead of only as a
  downstream "Wrong microprogram" (EWRON).
- `csWord1=0x2E9A` - control-store word 1 (LARG), the microprogram version.

The same derivation is also traced to the device log (DEBUGTRACE 2), including an explicit warning
if the model byte comes out `0x00`, which means the control store is not loaded rather than that
the model is wrong.

**This should make our two sides directly comparable**: if your command 3 ever answers a digit we
did not derive from the loaded image, the disagreement is now visible on both ends with its
provenance attached.

## Related documents

- `ACCP-COMMAND-LOG-CLEAN-BOOT-CAPTURE-2026-07-30.md` - the clean-boot command log (which DOES
  contain a 244B, same as this one - only the stuck flag differs)
- `ANSWERS-TO-ND5000-TEAM-2026-07-30.md` - the answers this responds to
- `QUESTIONS-TO-ACCP-TEAM-2026-07-30.md` - the original questions
- `OCTOBUS-KICK-AND-MAILBOX-GAP-REGISTER-2026-07-30.md` - the gap register; G10 is this defect
- `STOP-SYSTEM-ANALYSIS-AND-CLRKICK-GAP-2026-07-30.md` - the `stop-system` / `CLRKICK` analysis
