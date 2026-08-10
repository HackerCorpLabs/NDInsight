# Handoff: EXUC/EXCYC2, the AFLAG model, and the ACCP interface after the five-round exchange

**Date:** 2026-08-02
**Repos:** `E:\Dev\Ronny\NDInsight` (docs), `E:\Dev\Repos\Ronny\RetroCore` (code)
**Commits:** NDInsight `692ebcf`; RetroCore `be795d123` (WIP checkpoint) then `b1f7cb3e2`

Claim tags: **[V]** measured or byte-verified - **[DERIVED]** reasoned, not proven - **[OPEN]** unknown.

---

## 1. Read this first, or you will redo work

**Do not trust a status heading in this tree.** An audit on 2026-08-01 found eleven items marked
OPEN that were already fixed, including two sections of the `octobus-nd5000` skill and one file that
contradicted itself six days running. **Grep the code and the tests before investigating anything.**

**Five name-based or label-based assumptions have now misled this interface.** Every one was
resolved by executing or decoding a body rather than reading a name:

| Assumed | Actually |
|---|---|
| `0x300F`/`0x4016`/`0x8013` = initialisation | boot self-test bus loopback |
| `0x0007` = a read-arm | MASKAIBF |
| `0x795A` = octobus re-init | **STOPMIC** |
| `0x220000` = polled serial port | **ACON**, a write-driven control decoder |
| BM05 -> `TRAP_OCBAK` (adjacent label) | **`TRAP_OCBA`** |

---

## 2. What is CLOSED - do not re-open

| Item | Result |
|---|---|
| AFLAG bits 7/8 | IMM/DMM memory-management trap inputs, set by MMS hardware. Correctly unmodelled for octobus work `[V]` |
| `0x220000` command codes | **ACON**, the ACCP Control Decoder. All 17 census codes decoded against ND-05.020.01 table 9 `[V]` |
| EXUC sneak target | The **jump field** (`ABS_ADDR`), whatever the sequence type - ND-05.022.1 7.4 `[V]`. An earlier worry that NEXT/RETURN needed a different target is **refuted** |
| BM05/BM06 **destination** | bit 5 -> `TRAP_OCBA` @ 0o16550; bit 6 -> falls through to 0o16565 `[V]` |
| MREG-upper literals | Five literals, whole image, enumerated not pattern-matched `[V]` |
| ACCP reply convention | ack = `0x00`; nak = `FF <Messnak code> 10 11` `[V]`, pinned by ALIVE nak 7 |
| `0x001131E2` | THE selftest status word, read by console summary `0xF1A4` and `RTEST` `0x6632` `[V]` |

---

## 3. The one thing left half-done: EXCYC2

**The gap is real and reachable `[V]`** - 3 opportunities in a single cold boot, measured by
`ExucSecondSneakReachabilityTests`. Static sweep finds 47 candidate sites in each of B30 and A30.

**Two implementations were tried and both made things worse**, against a 25-failure baseline:

| EXCYC2 target | Result |
|---|---|
| word at EXCYC1's jump field | **38 failures (+13)** |
| word at EXCYC1 address + 1 | **37 failures (+12)** |

Damage was broad and central both times - `DIV`, `LOOP`, `ENT`/`RET`, the boot CPUPAR frame - not
anything ACCP-shaped, which rules out collateral from nearby work. Reverted; back to exactly 25.

**Why it cannot simply be bolted on.** Our EXCYC1 gate is **already a deliberate deviation from the
literal text**. Rule 1 says EXCYC1 executes whenever the conditional word sets EXUC, saying nothing
about which path is taken; we fire only on the false path, because firing on every pass
double-decrements the loop counter and hangs `SHIFT_ROT@017070`. So rule 1 as written is not
implementable here either. **Adding literal rule 2 on top of a deliberately non-literal rule 1 is
incoherent**, and the failure count is the proof.

**What unblocks it:** settling the **91-site** conflict between 7.3.5 rule 1 and 7.2 from hardware or
a third source. Not more tuning. **Do not adjust the gate until the suite goes green - that is
fitting the model to the tests**, which is the exact failure this project keeps catching.

A known measured gap of 3 per boot beats a change that breaks 13 core instruction families.

---

## 4. The AFLAG model, and the trap inside it

`AccessModule` now holds ATRAP and FATAL separately. **Neither has an AFLAG bit position, on
purpose.** `AflagAtrapBit`/`AflagFatalBit` default to a `BitNotModelled` sentinel and `ReadAflag`
composes them only when assigned; a test enforces the default.

**Do not assign a plausible bit to make a poll succeed.** The microcode reads AFLAG, the firmware
writes MREG, and **nobody has carved the wire between them**. That gap is the finding, not an
omission.

MREG-upper `[V]`: bit 7 OBACT, 6 AOBF, **5 FATAL**, **4 ATRAP**, 3 OMESS. Use
`WriteModeRegisterUpper(byte)` - one decoder for the firmware literals and the console back door.

### The trap: the AOB auto-clear conflict fakes the FATAL experiment

ND-05.020.01 contradicts itself - table 8's note says bits 8-15 reset on an AOB read; the prose at
lines 3484 and 3683 names only AOBF and ATRAP. **We implement the narrow reading**
(`AobReadClearsWide` defaults false).

**Under narrow, every `0xF0` delivery followed by an AOB read yields FATAL-set / ATRAP-clear - which
IS the BM05/BM06 cause stimulus.** Run that experiment on it and you measure your own auto-clear and
publish it as hardware behaviour. `AflagComposerTests` asserts the artefact **exists** so it cannot
be forgotten. **Always run the cause experiment under BOTH readings and report the pair.**

To raise FATAL alone: `Cmd31_LoadModeRegister @ 0x945E`, the one ACCP console command with no
`0x1143AC` running-flag guard (its five siblings all guard). **`0x20` is 40 OCTAL** - typing 20 at an
octal prompt asserts ATRAP and fakes a clean "FATAL behaves exactly like ATRAP".

---

## 5. Still open

| Item | Owner |
|---|---|
| BM05/BM06 **cause** (what sets each bit) | Needs FATAL-without-ATRAP **and** a settled auto-clear reading |
| AFLAG positions of ATRAP and FATAL | Needs the MREG-to-AFLAG wire carved - neither side has it |
| The 91-site 7.3.5 rule 1 vs 7.2 conflict | Needs hardware or a third source |
| What raises IRQ3 | The carve agent |
| `OCB_CLNUP` reachability from the init path | Our harness always sets AFLAG bit 12; the init path deliberately does not |
| ~30 of the 46 octobus dispatcher arms unnamed | Read the `cmpi.b` immediate at each site |
| **Save the Ghidra database** | **Only the user can do this** |

---

## 6. Tests worth knowing about

All in `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND5000\tests\`:

- `ExucSneakRuleSweepTests` - static census of every EXUC word in both images against the three
  documented rules.
- `ExucSecondSneakReachabilityTests` - converts the static count into a reachability measurement.
  Carries an **anti-vacuous control**: asserts sneaks fired at all, because a zero opportunity count
  would otherwise be meaningless.
- `ScanAccpBitDispatchTests` - the probe that refuted the adjacency error. **Fails if two different
  inputs reach the same destination**, because that means the routine never discriminated.
- `AflagComposerTests` - unassigned positions contribute nothing; all image literals decode; the
  narrow-reading artefact is asserted to exist.

**Baseline: 25 pre-existing failures in this suite.** Verified as pre-existing by stashing and
re-running, not assumed. Any work here should re-measure that number before and after.

> **"25 failures" is NOT 25 defects - at least a third are deliberate `[V 2026-08-02]`.**
>
> - **5 intentional markers**: `Entd`/`Entm`/`Entsn`/`Entt`/`Rett` `..._CannotBeDriven_HardFail` are
>   bare `Assert.Fail(...)` calls. The ENT*/RET* frame-establish instructions consume pending-call
>   state the single-instruction oracle cannot seed, and the comment says **"HARD-FAIL per audit -
>   not skipped"** - someone deliberately chose a visible failure over a silent skip. That is good
>   practice and should be left alone.
> - **3 named `_OPEN`**: two transcendental cases and `DoubleDivide_ReturnsDividend_OPEN`.
>
> Roughly **17 are real**, clustered in DIV / LOOP / double-precision, matching the known
> float-divide and transcendental gaps. **Read a failing test's name and body before "fixing" it -
> one ending `_HardFail` or `_OPEN` is doing its job by failing.**

---

## 7. The method lessons, which are the durable part

Six shapes have now cost real time, all of which **return a confident result rather than a hole**:

1. A spin whose exit condition every test pre-satisfied - entered and left in one instant, identical
   in a log to never entered.
2. A routine reachable only through a path the harness bypassed.
3. A missing capture **field** read as a missing **event**.
4. **A search whose method cannot match the encoding returns a confident EMPTY set.** Hunting FATAL
   via `bset #5` found nothing; both real sites are literal whole-byte writes bypassing the shadow.
5. **A measurement whose premise is wrong looks exactly like a subject behaving wrongly.** `RTEST`
   "contradicted" the console selftest; the probe had sent commands that clear the status word.
6. **A correct observation whose meaning arrives later and is never applied backwards.** "Write
   `0xF0` to `0x330000`" was recorded correctly, the decoder key arrived in the same round, and
   nobody re-read the constants already on the page.

**Rule from 6, now standing:** when you acquire a register map or decoder key, **re-scan the existing
carve for every literal written to that register**.

**Rule from the adjacency error:** assert one input, enter the routine the microcode itself uses,
record where it actually goes, and **fail the run if two different inputs reach the same
destination**.
