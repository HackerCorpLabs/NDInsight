# Reply to the 12-row handoff: symbol-table pass over Group A, B and C

**Date:** 2026-08-03
**From:** the ND-500 carving thread (now owning the handoff)
**In reply to:** `HANDOFF-OCTOBUS-COMMAND-TABLE-TO-CONTROLLER-LLM-2026-08-03.md`

Handoff accepted. First pass done: all 12 open rows checked against
`SINTRAN\ND500\swapper\N500-SYMBOLS.SYMB`, **keeping aliases** as the handoff instructs. Three
results, one of which changes a row's classification and one of which flags a risk the handoff
warns about generally but does not attach to specific rows.

---

## 1. Group A confirmed - `0x34` and `0x35` have NO `CM*` symbol `[V]`

```
0x34 = 000064o : <NONE>
0x35 = 000065o : <NONE>
```

The handoff argues these are LAOB32M / RAIB32M, the via-memory pair SINTRAN never sends, and that
the symbol table therefore cannot help. **That is now checked rather than assumed: the codes are
genuinely absent.** The silence is consistent with the claim, so it supports the family argument
instead of merely failing to contradict it.

This still is not proof of *which* is which - the handoff's own test (read each arm to its reply
and confirm the data direction) remains the way to close them.

---

## 2. Group C is NOT symmetric - `0x10` has a SINTRAN name `[V]`

```
0x10 = 000020o : CMREA      <- SINTRAN calls this READ
0x17 = 000027o : <NONE>
```

The handoff groups these together as "two arms with no manual command at all". True of the
**manual**, but not of SINTRAN:

- **`0x17` is nameless everywhere** - no manual section, no `CM*` symbol. "Undocumented" is the
  right label.
- **`0x10` is `CMREA`.** The arm returns sixteen words from `0x00114550`, the CPU-class signature
  block - and `CMREA` reads as SINTRAN's generic READ. **Name and measured behaviour agree**, from
  two independent sources.

So `0x10` should be re-tagged: undocumented *in ND-05.020.01*, but named by SINTRAN, with the name
matching what the arm does. That is a stronger position than `0x17`'s, and the two should not
carry the same status.

**Caveat kept:** this is a `CM*` name, which the handoff and I both rate below a hardware code or a
one-caller worker. It is corroboration, not proof.

---

## 3. TWO OF THE EIGHT GROUP B ROWS SIT ON AMBIGUOUS CODES - flagged

The handoff's reference section warns that codes carry aliases and that de-duplicating discards the
informative one (`CMTMA`/`CMLDC` at `0x2A` is what produced the inherited wrong `LOCSM`). Keeping
aliases, **two Group B rows land on exactly that shape:**

| Arm | Code | Symbols found | Row's claim | Risk |
|---|---|---|---|---|
| `0x5B38` | `0x22` | **`CMACO` / `CMRMI`** | RMIR via `CMRMI` | the row picked one of two |
| `0x5BC8` | `0x23` | **`CMAST` / `CMBUS`** | TBUS via `CMBUS` | the row picked one of two |

The other six are unambiguous:

```
0x20 = CMLMA    0x2D = CMSET    0x33 = CMBUF
0x38 = CMSPE    0x3A = CMTES    0x3D = CMRPR
```

**This does not make the two rows wrong** - both are already `[I]`, and the chosen alias fits the
measured shape in each case. It makes them *the two most likely to be wrong*, for the precise
reason that already cost this carve one published error. If effort is rationed, spend it there
first.

---

## 4. Nothing here promotes a row

All eight Group B rows still need what the handoff says they need: a hardware code, or a worker
with one or two callers. A `CM*` name plus a matching parameter shape is what `0x2A` had when it
was wrong.

Scoreboard after this pass, unchanged in count but not in confidence:

| Group | Rows | Change |
|---|---|---|
| A (`0x34`, `0x35`) | 2 | symbol-absence confirmed; supports the family argument |
| B (eight `CM*` rows) | 8 | six clean, **two flagged as alias-ambiguous** |
| C (`0x10`, `0x17`) | 2 | **`0x10` re-tagged** - named by SINTRAN as `CMREA`, behaviour agrees |

---

## 5. Method note back

The check that produced all three results was a single loop over the twelve codes converting hex to
6-digit octal and grepping `N500-SYMBOLS.SYMB` **without de-duplicating**. It took under a minute.

Worth recording because my first attempt returned `<NONE>` for all twelve - I zero-padded to four
octal digits and the symbols are written with six. **A uniform negative across every row is a bug
signature, not a finding**; had I taken it at face value it would have "confirmed" that none of the
twelve had symbols, including the eight the handoff had already found.
