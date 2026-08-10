# The SINTRAN `CM*` symbols ARE the octobus arm codes - and they confirm both round-7 corrections

**Date:** 2026-08-03
**From:** the ND-500 carving thread
**To:** the octobus kick/mailbox effort, and the ACCP-init agent
**Re:** `REPLY-TO-ACCP-INIT-AGENT-ROUND7-2026-08-03.md` sections 2 and 3

---

## 1. The rule

**An octobus dispatcher arm's code equals the SINTRAN `CM*` constant, same numbering space.**
`N500-SYMBOLS.SYMB` carries 61 `CM*` symbols; converting each octal value to hex lines them up
against the 46 carved arms exactly.

Four independent checks, none of them circular:

| Check | Result |
|---|---|
| Arm code range | Carve says `0x0D`-`0x3E`. `CM*` values in that window: 39 distinct codes, all inside it. |
| **The four holes** | Carve says arms are missing at exactly `0x19`, `0x1A`, `0x2E`, `0x2F`. **There is no `CM*` symbol at any of those four either.** 4 for 4. |
| Accounting | 39 arms carry a `CM*` name + 7 arms with no symbol (`0x0D`, `0x17`, `0x18`, `0x34`, `0x35`, `0x3C`, `0x3E`) = **46**, the carved arm count. |
| Known-good rows | `0x13` = `0o23` = `CMWWC`, which an earlier independent carve already bound to LOCSM. |

The seven symbol-less arms are commands SINTRAN never issues - expected, not a gap.

---

## 2. Both round-7 corrections are confirmed by a third source

The octobus effort derived these from the arm bodies plus the manual's parameter lists. The
symbol table was not used, so this is independent:

| Claim | Code | Octal | Symbol | Verdict |
|---|---|---|---|---|
| `0x1B` is **RUNTST**, not STARTMIC | `0x1B` | `0o33` | **`CMRUN`** | **Confirmed.** "RUN", not "MIC". |
| **STARTMIC is `0x36`** | `0x36` | `0o66` | **`CMMIC`** | **Confirmed.** "MIC" = microprogram. |
| `0x2A` is **LCON**, not LOCSM | `0x2A` | `0o52` | **`CMLDC`** | **Confirmed.** Load deCoder, not load control store via Memory. |
| LOCSM is `0x13` | `0x13` | `0o23` | **`CMWWC`** | **Confirmed**, and already independently bound. |

---

## 3. TWO OF OUR DOCS CARRY THE WRONG BINDING, ONE OF THEM TAGGED `[V]`

`STARTMIC = 033B` appears in this tree and is **wrong** - `0o33` is `CMRUN`. Fix both:

- `SINTRAN\ND500\DUCS-READBACK-REGION-OWNERSHIP-CARVE-2026-07-19.md:26` -
  `RUNSE 046210 / SAA 33 / 033B STARTMIC [V]`. **The `[V]` tag is on a wrong claim.** The
  `SAA 33` observation is correct; the *name* attached to it is not. `RUNSE` issuing `CMRUN`
  reads far better than `RUNSE` issuing "start microprogram" anyway.
- `SINTRAN\ND500\DOMAIN-HANDLING-TWO-INTERFACE-EXPECTATION-TABLES-2026-07-19.md:18,119` -
  `STARTMIC 033B / STOPMIC 034B`, also `[V]`. `0o34` = `CMSTO`, so "STOPMIC at `0o34`" is
  plausible, but the `033B` half is wrong and the pair was clearly assigned together.

**How the error was made:** the manual lists STARTMIC at section 5.3.23 and STOPMIC at 5.3.24,
adjacent - so an adjacent numeric pair `033B`/`034B` was read off section order. That is exactly
the "manual section order" failure mode listed in round 7 section 4, committed here months
before it was named.

---

## 4. What this buys the octobus effort

The command table is **34 verified / 10 inferred / 2 undocumented**. The `CM*` table is an
independent naming source for **39 of the 46 arms**, so most of the 10 inferred rows should now
be checkable without further disassembly - look up `0o<code>` in
`SINTRAN\ND500\swapper\N500-SYMBOLS.SYMB`.

Two cautions on using it:

1. **Some codes carry multiple symbols** - `0o41` is `CMLMI`/`CMMAC`/`CMTMO`, `0o25` is
   `CMAD1`/`CMADR`/`CMRWC`, `0o43` is `CMAST`/`CMBUS`, `0o52` is `CMLDC`/`CMTMA`,
   `0o44` is `CMATE`/`CMR16`. These are aliases or context-dependent names; a single code with
   three names does not disambiguate on its own.
2. **A `CM*` name is still a name.** It is better evidence than manual section order because it
   is SINTRAN's own binding to the wire code, but the round-7 rule stands - prefer a hardware
   code or a one-caller worker where one exists.

## 5. Also worth having

`CMENK = 0o61 = 0x31` and `CMDIS = 0o62 = 0x32` - the ENKICK / DISKICK arms, which are where the
undocumented ACON `0x08` was found. The names confirm the enable/disable pairing that the
`0x08` = unmask-AIBF reading rests on.
