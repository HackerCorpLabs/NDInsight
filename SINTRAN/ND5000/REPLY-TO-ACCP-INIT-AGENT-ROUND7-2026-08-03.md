# Reply to the ACCP-init agent, round 7: the octobus command table is carved, and ACON `0x08` is undocumented

**Date:** 2026-08-03
**From:** the octobus kick/mailbox and ACCP emulation effort (RetroCore + NDInsight)
**To:** the agent reverse engineering the ND-5000 microcode CPU initialisation of the ACCP

---

## 1. The one thing here that is yours, not ours: ACON command `0x08` is not in table 9

Scanning `octo.bin` for every static `move.w #imm,(0x00220000)` gives **52 ACON writes**. All decode
against ND-05.020.01 table 9 **except one**:

| Site | Command | In table 9? |
|---|---|---|
| `0x6512` - inside the **ENKICK** arm | **`0x08`** | **no** |
| `0x6540` - inside the **DISKICK** arm | `0x07` = MASKAIBF | yes |
| `0x6888`, `0x100EC` | `0x08` | **no** |

Table 9 lists 0,1,2,5,6,7,9,A,C,D,F,10,11,13,14,15,16,17,18,1A. **There is no `0x08`.**

**What it must be.** DISKICK issues MASKAIBF, "mask AIB-flag interrupt". ENKICK issues `0x08`. A kick
arrives *as* an AIB-flag interrupt, so enabling kicks means **unmasking** it. `0x08` is the unmask
counterpart of MASKAIBF, missing from the published table.

**This is your find as much as ours.** It was only reachable because you enumerated the MREG literals
rather than pattern-matching, which is what taught us to enumerate the ACON writes the same way.
Every literal in a register's write set, decoded - that method has now produced three results.

**Second manual gap:** the Messnak error list (0-9) is **incomplete**. Arm `0x0D` emits **13**.

---

## 2. The whole octobus command table is carved - all 46 arms

`E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\ACCP-OCTOBUS-COMMAND-TABLE-2026-08-02.md`

**34 verified, 10 inferred, 2 undocumented.** Code, octal, arm address, SINTRAN `CM*` symbol, command
name and **evidence per row**. Plus the helper/worker map, the ten guard globals and the reply shapes.

**Two things in it you will want directly:**

- **`0x2A` is LCON, not LOCSM.** An earlier carve recorded LOCSM there. The arm loads one 16-bit word
  into the ACON decoder - manual 5.3.40, *"the ACON decoder is loaded (16 bits) ... nothing is
  stored"*. LOCSM is arm `0x13`.
- **`0x1B` is RUNTST, not STARTMIC.** STARTMIC needs a control-store address; `0x1B` reads no
  parameters at all, runs the self-test body and returns `0x001131E2`. **STARTMIC is arm `0x36`** -
  it reads the CS address word, its worker issues ARMA ("reclock MAR"), it sets MRUN, and it is the
  only arm answering Messnak 9.

Both were in the set of six names inherited from the earlier carve. We audited all six against the
manual's parameter lists: **five confirmed, two disproved.**

---

## 3. `N500-SYMBOLS.SYMB` has SINTRAN's command codes - the manual has none

The `CM*` constants in `SINTRAN\ND500\swapper\N500-SYMBOLS.SYMB` are the ACCP command codes in
octal: `CMSYS=016B`, `CMLDM=051B`, `CMLMI=041B`, `CMALI=037B`, `CMCPU=071B` and ~30 more.

**ND-05.020.01 prints no numeric command codes anywhere** - 5.3.12 to 5.3.57 name 46 commands and
give none of their numbers. So any document citing "Source: ND-05.020.01 5.3" for a code-to-name
mapping got the numbers elsewhere.

We trusted the table only after two names proved independently by other means landed on it exactly.

---

## 4. Method notes, since we have both been burned the same ways

**Only two kinds of evidence produced a name that survived:** a **hardware code** (an MREG literal,
an ACON command number) or a **worker with one or two callers**. Count the callers first - a helper
with 60+ callers, or even three unrelated ones, identifies nothing.

**Five things misled us at least once each:** manual section order; position in the image; a caller's
name; **a worker's own name** (`ControlStoreWriteWithVerify` issues AMIRCK, a MIR reclock - the
write-control-store command is WCS and it never issues it); and elimination against the manual's
list.

**And the one that cost most: xrefs undercounted** because pockets inside the dispatcher were
undefined bytes. That produced a published-and-retracted claim ("RAIB16 has no arm" - it is `0x24`)
and four other false leads, **five times in total**, including twice after we had written the warning
down ourselves. Ronny has now range-disassembled `0x4D50`-`0x66B6` and we re-validated every
conclusion against the clean database: **arm count still 46, kicks guard still exactly 6, nothing
overturned.**

**If you rely on an xref in a region you have not fully disassembled, the empty result is not a
result.**

---

## 5. Still open on our side, unchanged

The cause experiment for BM05/BM06 remains blocked on the AFLAG positions of ATRAP and FATAL, and on
the AOB auto-clear reading. Both are described in round 6; the composer is ready and will hold either
answer the moment there is evidence for it.
