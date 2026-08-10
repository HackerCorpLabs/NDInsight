# The two alias-ambiguous rows are settled: `0x22` = RMIR, `0x23` = TBUS

**Date:** 2026-08-03
**From:** the ND-500 carving thread (owner of the 12-row handoff)
**Follows:** `REPLY-OCTOBUS-COMMAND-TABLE-12-ROWS-2026-08-03.md` section 3, which flagged these
two as "the two most likely to be wrong"

Both flagged rows are **confirmed as they stand**. The argument is positional rather than
name-matching, so it does not repeat the mistake that produced the inherited wrong `LOCSM`.

---

## 1. Why the previous method could not settle them

Section 3 of the earlier reply found two `CM*` symbols on each code:

```
0o042 (0x22)  CMACO / CMRMI     row claims RMIR
0o043 (0x23)  CMAST / CMBUS     row claims TBUS
```

A name match cannot choose between them - that is exactly what `CMTMA`/`CMLDC` at `0o052`
showed. **These are small values**, and small values alias heavily: a SINTRAN symbol table is
global, so any constant that happens to equal `0o042` lands on the same line as the command
code. Alias density is a function of how small and common the number is, not of how meaningful
the symbol is.

---

## 2. The ordering argument `[V]`

Dump every `CM*` symbol sorted by value and the command family appears as a contiguous run.
Line it up against the manual's own section order (`ND-05.020.01` section 5.3):

| Code | `CM*` | Manual section | Command |
|---|---|---|---|
| `0o040` (0x20) | `CMLMA` | 5.3.27 | Load MAR (LMAR) |
| `0o041` (0x21) | `CMLMI` | 5.3.28 | Load MIR (LMIR) |
| **`0o042` (0x22)** | **`CMRMI`** | **5.3.29** | **Read MIR (RMIR)** |
| **`0o043` (0x23)** | **`CMBUS`** | **5.3.31** | **Test Bus (TBUS)** |
| `0o044` (0x24) | `CMR16` | 5.3.32 | Read AIB16 (RAIB16) |
| `0o045` (0x25) | `CMR32` | 5.3.33 | Read AIB32 Directly (RAIB32D) |
| `0o046` (0x26) | `CML16` | 5.3.35 | Load AOB16 (LAOB16) |
| `0o047` (0x27) | `CML32` | 5.3.36 | Load AOB32 Directly (LAOB32D) |
| `0o050` (0x28) | `CMRAS` | 5.3.38 | Read ASTS (RASTS) |
| `0o051` (0x29) | `CMLDM` | 5.3.39 | Load MODE (LMODE) |
| `0o052` (0x2A) | `CMLDC` | 5.3.40 | Load CON (LCON) |
| `0o053` (0x2B) | `CMWMP` | 5.3.41 | Write Multiport (WMPM) |
| `0o054` (0x2C) | `CMRMP` | 5.3.42 | Read Multiport (RMPM) |

**Thirteen consecutive codes rise in step with the manual's section order.** The only sections
skipped are `RAIB32M` (5.3.34) and `LAOB32M` (5.3.37) - the two via-memory commands, which is
precisely the Group A pair already known to sit apart at `0x34`/`0x35` and to have no `CM*`
symbol at all. **The exception confirms the rule instead of breaking it.**

`CMRMI` at `0o042` sits in that run. `CMACO` would have to be a command that the manual places
between LMIR and TBUS, and there is none.

Two further supports for `0o042` specifically:

- **The family names itself L/R in pairs**: `CMLMI` = Load MIR at `0o041`, `CMRMI` = Read MIR at
  `0o042`. The same shape recurs at `CMR16`/`CML16`, `CMR32`/`CML32`, `CMWMP`/`CMRMP`. `CMACO`
  fits no pair.
- Every name in the run is a **direct abbreviation of the manual's mnemonic** - `LMA`/LMAR,
  `LMI`/LMIR, `RMI`/RMIR, `RAS`/RASTS, `LDM`/LMODE, `LDC`/LCON, `WMP`/WMPM, `RMP`/RMPM. `ACO`
  and `AST` abbreviate nothing in the ACON command set.

---

## 3. `0o043` - TBUF and TBUS are separated, and both are accounted for `[V]`

The one soft spot in the ordering argument is that **two** manual sections sit between RMIR and
RAIB16:

```
5.3.30  Test Buffer (TBUF)
5.3.31  Test Bus    (TBUS)
```

and only one code (`0o043`) is free. So the run alone cannot say which of the two it is - and
`CMBUS`/`CMAST` cannot be told apart by adjacency either.

It is settled by the other one. **`CMBUF` exists, at `0o063` (0x33)** - already one of the six
rows the earlier pass called unambiguous. So TBUF is placed elsewhere in the numbering and is
not competing for `0o043`, leaving TBUS. And the naming is direct in both cases:
`BUF` -> Test **Buf**fer, `BUS` -> Test **Bus**.

---

## 4. What `CMACO` and `CMAST` probably are `[I]`

Not needed for the result, but worth recording so the next person does not re-flag them:

- `CMACO` reads as **ACON** - the name of the command family itself. A base or marker constant,
  not a member of the set.
- `CMAST` reads as **ASTS**, the A-status register that `RASTS` (`CMRAS`, `0o050`) reads. A
  field or mask constant.

Both are marked inference. The point that IS proven is negative and sufficient: **neither
abbreviates any command in the ACON set**, while every genuine member of the run does.

---

## 5. Scoreboard

| Group | Rows | Change |
|---|---|---|
| A (`0x34`, `0x35`) | 2 | unchanged - still needs the direction read |
| B | 8 | **2 promoted** (`0x22` RMIR, `0x23` TBUS); 6 unchanged |
| C (`0x10`, `0x17`) | 2 | unchanged |

The two rows flagged as most likely to be wrong were right. They now rest on a positional
argument across a thirteen-code run plus a naming convention, not on a single `CM*` name.

---

## 6. Method note - the alias problem has a general answer

The earlier reply warned that de-duplicating symbols destroys the informative one. True, but
keeping both leaves a coin flip. **What breaks the tie is not the symbol table at all - it is
that the manual's section order and the code order rise together.** Once several consecutive
codes line up, each one constrains its neighbours, and an alias that fits no command in the
region loses regardless of how plausible its name looks in isolation.

Worth applying to the remaining six Group B rows before hunting for hardware codes one at a
time: **check where each falls in the run first.** A row whose neighbours are both pinned has
far less freedom than the handoff's per-row treatment assumes.

---

## 7. CORRECTION to section 6, same day - the run does NOT extend past `0o054`

Section 6 was written after checking `0o040`-`0o054` and before checking the rest. Checking the
rest shows **the ordering breaks immediately after the run ends**, so the method must not be
applied blindly to the remaining rows. Reading the full section list (5.3.12 through 5.3.49,
38 commands) against the codes:

| Code | `CM*` | Command | Manual section |
|---|---|---|---|
| `0o055` (0x2D) | `CMSET` | SETTRAC | 5.3.**44** |
| `0o061` (0x31) | `CMENK` | ENKICK | 5.3.**47** |
| `0o062` (0x32) | `CMDIS` | DISKICK | 5.3.**48** |
| `0o063` (0x33) | `CMBUF` | TBUF | 5.3.**30** |
| `0o067` (0x37) | `CMLOO` | LOOP | 5.3.**45** |
| `0o071` (0x39) | `CMCPU` | CPURES | 5.3.**49** |
| `0o072` (0x3A) | `CMTES` | TESTMPM | 5.3.**43** |
| `0o073` (0x3B) | `CMCCD` | DCCD | 5.3.**21** |

Section numbers jump around freely. `TESTMPM` (5.3.43) should have taken `0o055` if the run
continued; `CMSET` is there instead and `TESTMPM` turns up at `0o072`.

**So the ordered run is `0o040`-`0o054` and nothing more.** It settled `0x22` and `0x23` because
both sit INSIDE it - that argument stands unchanged. It settles nothing outside it, and four of
the six remaining Group B rows are outside.

What the remaining rows still have is a direct, unambiguous abbreviation (one `CM*` symbol each,
each an initial-letter contraction of a manual mnemonic): `ENK`/ENKICK, `DIS`/DISKICK,
`LOO`/LOOP, `CPU`/CPURES, `TES`/TESTMPM, `SET`/SETTRAC, `BUF`/TBUF, `CCD`/DCCD. That is the same
class of evidence the handoff already rates below a hardware code - **unchanged, not promoted.**

Of the six, only `0x20` (`CMLMA` = LMAR) is promoted here, because it falls inside the run.

### Two rows match NO command in the set - worth flagging

```
0o070 (0x38)  CMSPE
0o075 (0x3D)  CMRPR
```

Neither `SPE` nor `RPR` begins any of the 38 commands in 5.3.12-5.3.49, while every other name
in the family is a clean initial-letter contraction. Either these are not ACON commands, or
their names do not follow the family's own convention. Both possibilities are informative and
neither should be written up as a confident row.

