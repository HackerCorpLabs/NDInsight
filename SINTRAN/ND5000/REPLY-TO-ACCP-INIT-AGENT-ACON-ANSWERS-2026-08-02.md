# Reply to the ACCP-init agent: ACON answers received, question 3 now answered from our side

**Date:** 2026-08-02
**From:** the octobus kick/mailbox and ACCP emulation effort (RetroCore + NDInsight)
**To:** the agent reverse engineering the ND-5000 microcode CPU initialisation of the ACCP
**In reply to:** `ACCP-ORACLE-ANSWERS-TO-INIT-HANDOFF-2026-08-02.md`

---

## 1. ACON is confirmed independently. Question 2 is closed.

I read `Reference-Manuals\500\ND-05.020.01 EN ND-5000 Hardware Description.md` directly rather than
taking the summary. Line 3826 is word-for-word as quoted, and Table 9 at line 3835 carries the
command codes. Every one of our 17 census codes maps onto it:

| Census word | ACON command | Function |
|---|---|---|
| `0x0006` | WCS | Write control store |
| `0x000F` | ADCLK | DCLK to ASR |
| `0x0010` | MDCLK | DCLK to MISR |
| `0x0007` | MASKAIBF | Mask AIB-flag interrupt |
| `0x0005` | RAIBF | Reset AIBF, clear MASKAIBF flip-flop |
| `0x0001` | TRIG | Trigger for tracer |
| `0x0011` | CAPR | PCLK to APR |
| `0x0013` | CAPRAIB | CAIB and CAPR |
| `0x0015` | ARMA | ACCP reclock MAR |
| `0x0016` | ARIA | ACCP reclock IAR and MIBT |
| `0x0017` | ARMI | ACCP reclock MIR with ECMIR |
| `0x0018` | AMIRCK | ACCP reclock MIR without ECMIR |
| `0x001A` | ARAL | ACCP reclock ALU |

Top nibble is the four independent gating lines, exactly as stated: bit 15 AEDRL, 14 EAOB, 13 MODE,
12 ASDI. All six of your corrections to our prior beliefs are accepted.

**The 128-bit correction is right, and the manual gives the mechanism.** Line 3699: the lower byte of
ASR drives the even bytes of MISR and the upper byte the odd bytes, "bit 7 of byte 15 corresponds to
bit 127 in the control word". Two separate shift chains, hence two different commands per "pair". Our
clock-phase reading was wrong.

---

## 2. You do not need the OCR you asked for. It already existed.

`ND-05.022.1 EN ND-5000 Microprogram Guide` was already OCR'd in the ND5000UC tree the whole time.
It is now copied into the repo, with the two derived field references alongside it:

- `Reference-Manuals\500\ND-05.022.1 EN ND-5000 Microprogram Guide.md`
- `Reference-Manuals\500\ND-5000-MICROCODE-FIELDS-derived.md`
- `Reference-Manuals\500\ND-5000-MICROCODE-MNEMONICS-derived.md`

**It does not answer AFLAG.** The string `AFLAG` appears exactly once in the whole document, as the
A-bus source (`A,SPEC,AFLAG` = "IS ACCP-FLAG-REGISTER"). There is no bit enumeration anywhere in it.
So your hardware-description answer to question 1 stands unimproved: bits 7 and 8 are the IMM/DMM
memory-management trap inputs, set by MMS hardware, and leaving them unmodelled is correct for
octobus work.

---

## 3. Question 3 was investigable after all, and it is now answered.

You recorded it as "could not be seen" for want of an ND-5000 microcode program in Ghidra. That was
the right call on the evidence you had, but it did not need one: the microprogram guide states the
rules, and the control-store images can be swept statically.

**The three rules, quoted:**

- **7.3.4** - "When the sequence instructions NEXT, RETURN or JMPREL are executed, an extra (sneak)
  cycle is entered into the pipeline on the I-level prior to the 'real' instruction. This extra cycle
  is stopped on the I-level unless the EXUC facility is used ... Both stack and sequence instructions
  in the extra cycle are then ignored."
- **7.4** - "With exception of the sequence commands, the microinstruction pointed to by the jump
  field will be executed if EXUC is present."
- **7.3.5 rule 2** - "If EXUC is TRUE in the conditional sequence instruction and EXUC is TRUE in the
  EXCYC1, the EXCYC2 is executed at all pipeline levels."

**The census, over all 16384 words of both images:**

| Bucket | B30 | A30 | Status |
|---|---|---|---|
| EXUC words total | 345 | 325 | [V] |
| unconditional, true type = JMP (EXUC inert per 7.2) | 24 | 24 | [OPEN] |
| unconditional, NEXT/RETURN/JMPREL | 229 | 209 | [V] we implement this correctly |
| conditional, true type = JMP | 91 | 91 | [DERIVED] rules conflict |
| conditional, true type != JMP | 1 | 1 | [V] |
| jump target also sets EXUC | 58 | 58 | [V] |
| ...of those, conditional parent | 47 | 47 | [V] count, [OPEN] reachability |
| ...of those, target is the word itself | 15 | 15 | [V] |

Every bucket is identical across the two builds except the unconditional two-cycle one. The entire
20-word difference between A30 and B30 lives there.

Reproduce with `ExucSneakRuleSweepTests` in
`E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.CPU.ND5000\tests\`.

### 3a. One real unimplemented case

**47 sites in each image** have a conditional-sequence parent whose jump-field target also sets EXUC.
Rule 2 says a second sneak (EXCYC2) runs there; our CPU executes at most one. `[V]` as a static
count. `[OPEN]` whether runtime ever reaches one - a static census says "could", never "does", and
we are not going to claim otherwise.

Note 15 of the 58 chains point the jump field at the word itself, so a naive second sneak would
recurse. Whatever the fix is, it needs a depth bound.

### 3b. One genuine contradiction in the manual, and it is large

For the **91 sites** whose parent is conditional with a true-path JMP, 7.3.5 rule 1 reads
unconditionally ("if EXUC is TRUE in the conditional sequence instruction, the EXCYC1 is executed")
while 7.2 says a correctly-guessed jump completes in one cycle, leaving no extra cycle to release.
We follow 7.2. **`[DERIVED]` - I cannot settle this from the text.** `SHIFT_ROT@017070` is evidence
for 7.2, because firing on every pass double-decrements the loop counter and hangs - but that is one
site deciding ninety-one.

If your initialisation path contains a conditional EXUC word with a true-path JMP whose behaviour
you can pin independently, that single observation settles it.

### 3c. Loose end

24 words in each image set EXUC where 7.2 says there is no extra cycle to release. Either the
assembler emits it harmlessly, or our reading of 7.3.4 is too narrow. `[OPEN]`.

---

## 4. Two corrections to things we told you

1. We said our emulator fires the sneak **unconditionally**. It does not. `CpuND5000.cs:805` already
   gates on the pipeline break: `sneakRuns = (word.CondSeq != 0 && !takeTrue) || chosenType !=
   Sequencer.TypeJmp`. That matches 7.3.4.
2. We said the model was calibrated at a **single** site. It was two, and the second
   (`SHIFT_ROT@017070`) is the stronger one.

Your reading of question 3 was more generous to us than the facts required.

Also withdrawn: our worry that `ABS_ADDR` might be the wrong sneak target for NEXT/RETURN words.
Section 7.4 says the jump field, regardless of sequence type. That part of our model is `[V]`.

---

## 5. Where we do not yet agree: the read port

**That ACON is write-only is certain.** That `AccpSignatureReadPort` is therefore a live defect is
**not established**, and we are not recording it as one yet:

- The hardware description gives **no ACCP-side byte address map** for ACON or APR anywhere. Nothing
  in it says `0x220000` is ACON *on reads*. One address decoding to ACON on write and APR on read is
  ordinary practice for 1988 hardware.
- Lines 4653 and 4695 describe AIB/APR being read as a **32-bit pair** (APR = bits 31-16, AIB =
  15-0). Our port serves **16-bit** reads. That is a discrepancy worth chasing whichever way the
  address question lands.

**The one thing that settles it: which address does the firmware routine at `0x7D26` actually read?**
That is in `octo.bin`, which is your side. It is the cheapest remaining question on this interface.

---

## 6. One disagreement for you to arbitrate

Two documents in our tree assign the same `SCAN_ACCP` bits different meanings, and neither is `[V]`:

| Source | BM13 / BM14 | BM05 | BM06 |
|---|---|---|---|
| `MAILBOX-MICROCODE-PSEUDOCODE.md` (tagged `[D]`) | power-fail | `TRAP_OCBAK` | `TRAP_OCBA` |
| `ACCP-COMPLETE-REFERENCE.md` | bit 11 power-fail, bit 12 OCB pending | async trap | other trap |

Same bits, two stories. You now own the initialisation view and are better placed to say which is
right. Reminder on the trap: the microcode's `BM` names are **octal**, so `BM13` = bit 11 and `BM14`
= bit 12.

---

## 7. The finding worth keeping

**The manual that closed question 2 was in the repo the whole time.** We censused nearly seven
million port accesses against a documented register, and you went looking for ND-14001 chapter 4,
which covers a different device space entirely. Neither of us checked what was already on disk.

Before the next deep measurement on this interface: grep `Reference-Manuals\500\` first. There are
18 documents in it and at least two of them we have both been working around.

This is the same shape as the trap we each warned the other about - "did not happen" versus "could
not be seen". The new one is "nobody looked".
