# Handoff: the ACCP <-> ND-5000 seam contract

**Date:** 2026-08-03
**To:** the agent reverse engineering the ND-5000 microcode initialisation of the ACCP
**From:** the ACCP machine / octobus emulation effort (RetroCore + NDInsight)
**Subject:** We want the two halves actually talking. Four questions only you can answer.

---

## Why this document exists

Our job has been restated: **bring up a working octobus ACCP machine that really talks to the
ND-5000 CPU microcode.** Not more carving - a running link.

Both halves exist and both run real ND code:

- `HackerCorpLabs.Emulation.Machines.Accp` runs the real `octo.bin` (ND-324716, 131072 bytes,
  SHA256 `0EA81716AD81984B...`). Its suite is **106/106 green**.
- `CpuND5000` runs the real MICRO-5800-B30 microcode, one 128-bit microword per tick.

**They are not connected.** The piece between them is `AccessModule` in
`RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND5000\src\AccessModule.cs`, and it has a hole in it
that we cannot fill from the firmware side.

---

## The hole, stated exactly

The ACCP firmware signals the CPU by **writing MREG**. The microcode learns about it by **reading
AFLAG**. Those are two different registers, and **nobody has carved the wire between them.**

What we have verified from the firmware side - the MREG upper-byte bit positions:

| MREG upper bit | Signal |
|---|---|
| 7 | OBACT |
| 6 | AOBF |
| **5** | **FATAL** |
| **4** | **ATRAP** |
| 3 | OMESS |

What we have from the manual for AFLAG (octal `BM` names; `BM13` = bit 11, `BM14` = bit 12):

| AFLAG bit | Meaning |
|---|---|
| 5 | async trap |
| 6 | other trap |
| 7 | data fault (IMM/DMM, MMS hardware) |
| 8 | instruction fault |
| 9 | AOBF |
| 10 | AIBF |
| 11 | power-fail |
| 12 | OCB pending |

**ATRAP and FATAL appear in MREG and are absent from that AFLAG list.** So in our code they carry
the sentinel `BitNotModelled`:

```csharp
public const int BitNotModelled = -1;
public int AflagAtrapBit = BitNotModelled;
public int AflagFatalBit = BitNotModelled;
```

`ReadAflag` composes a bit **only** when a position is assigned, so today the microcode cannot see
either signal. That is deliberate - we refused to guess a bit number - but it is exactly the wire a
working link needs.

---

## The four questions

### Q1. Which AFLAG bits does the microcode actually TEST?

Not which bits the manual lists - which bit numbers appear in real AFLAG test microwords in the B30
image. **You can sweep the microcode; we cannot.** If a bit outside 5-12 is tested, that is the
answer to Q2 for free.

### Q2. What bit does the microcode read when the ACCP raises ATRAP, and what for FATAL?

These are the two the ACCP uses to get the CPU's attention, and they are the two we cannot place.
A bit number with the microword that reads it settles it.

### Q3. What does the microcode expect at cold start?

Concretely: does it poll AFLAG **before** the control store is loaded, and what does it do on each
dispatch bit it finds? We need this to know whether our ACCP is allowed to raise anything during
its own selftest, or must stay quiet until STARTMIC.

Related and already half-known on our side: **`SCAN_ACCP` bit 5 goes to `TRAP_OCBA` @ `0o16550`
and bit 6 falls through to `0o16565`** - we have a test that fails if both bits ever reach the same
destination. If you have the full dispatch fan-out of that routine, it likely answers Q1 outright.

### Q4. Does an AOB read clear NARROW or WIDE?

Our `ReadAob` clears `AobFull` and `Atrap`, and clears `Fatal` **only** if `AobReadClearsWide` is
set. We currently have it **false** (narrow), from a plain reading of the manual prose - which is
a reading, not evidence.

**This decides whether FATAL survives an AOB read**, so it decides whether a fatal condition can be
lost. If the microcode reads AOB on a trap path and then re-tests FATAL, that is the proof.

---

## What we do the moment we have answers

Nothing here is blocked on anything except Q1-Q4:

1. Assign `AflagAtrapBit` / `AflagFatalBit`, drop the sentinel.
2. Connect the ACCP's MREG-upper writes (`0x330000`) to `CpuND5000.Regs.Access` -
   `WriteModeRegisterUpper` already decodes the byte into AOBF/ATRAP/FATAL.
3. Wire AIB/AOB both ways: ACCP `0x440000` / `0x550000` against the CPU's `Aib` / `Aob`.
4. Route the control-store load: `LOCSD` (`0x14`) / `LOCSM` (`0x13`) issue **WCS** (ACON `0x06`)
   into `ControlStore.WriteWord`.

Then the bring-up milestones, each of which becomes a test:

| # | Milestone | Proves |
|---|---|---|
| 1 | ACCP boots, CPU idle, `ALIVE` (`0x1F`) answers **nak 7** | baseline - already true today |
| 2 | `LOCSD` loads one CS word, `DCSD` (`0x16`) reads it back identical | the CS path is real |
| 3 | `STARTMIC` (`0x36`) starts the microprogram - MAR via ARMA, MRUN set | the start path is real |
| 4 | **`ALIVE` now answers ack** | **the two halves are actually talking** |
| 5 | `ENKICK` (`0x31`) then a kick delivered end to end | the interrupt path is real |

Milestone 4 is the one that matters. It is the first thing that cannot be faked by either half
alone.

---

## Things you may want from our side

- **All 46 octobus dispatcher arms are carved** with per-row evidence:
  `SINTRAN\ND5000\ACCP-OCTOBUS-COMMAND-TABLE-2026-08-02.md` - 34 verified, 10 inferred,
  2 undocumented. `STARTMIC` is `0x36`, **not** `0x1B`; `0x2A` is LCON, **not** LOCSM.
- **ACON `0x08` is undocumented** and the card issues it on every ENABLE KICK - see round 7.
- **The guard globals** that classify an arm before its body is read - `0x001143AC` running,
  `0x001143B2` parameter pointer, `0x001143B6` kicks enabled, `0x0011455C` CS/cache health
  (good value `0x7F55`), `0x0011314A` CS initialised (**only STARTMIC uses it**).
- Ask for anything else in `octo.bin`; the database is clean over `0x4D50`-`0x66B6`.

---

## One standing caution, since it has bitten both of us

**A name or a bit position that merely fits is not evidence.** On our side only two kinds of
evidence ever survived: a **hardware code** (an MREG literal, an ACON command number) or a **worker
with one or two callers**. Section order, image position, a caller's name, a worker's own name, and
elimination against the manual each misled us at least once.

So for Q2 in particular: **a microword that reads the bit beats any argument from the manual's
table.** If the answer is "the microcode never tests it", that is a real and useful answer - it
would mean the ACCP's ATRAP/FATAL reach the CPU by some path other than AFLAG, and we should be
looking somewhere else entirely.
