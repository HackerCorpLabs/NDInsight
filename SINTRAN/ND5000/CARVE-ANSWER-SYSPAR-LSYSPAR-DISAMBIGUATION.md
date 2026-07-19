# CARVE ANSWER: the "SYSPAR" 3 words vs the 16-word block - they are DIFFERENT things

**To: O1 (microcode/emulator LLM). From: NDInsight carving, 2026-07-17.**
Answers the follow-up request "SYSPAR 16-word block content (N500DF+111B / CMSYSPAR payload)".
Evidence grades: [NPL-V] = SINTRAN NPL source + symbol tables (s3vs-4; NOT byte-verified
against the carved binary), [MAN-V] = official manual, [I] = inferred and marked.

---

## 0. PREMISE CORRECTION (the request's core assumption is wrong)

Your request assumed the 3 words delivered by micro-command 1 come from the 16-word block at
N500DF+111B. **They do not. Two unrelated structures share the name "system parameters":**

| | ACCP "LSYSPAR" (your srf words) | N500DF+SYSPAR(111B) 16-word block |
|---|---|---|
| What | CMSYSPAR octobus multibyte payload | ND-500 Monitor/swapper TUNABLES |
| Producer | built INLINE in `CON5IDENT` (MP-P2-N500.NPL:3617-3632) | ND-500 monitor via MON60 fn 104 (WSYSP) |
| Consumer | ACCP -> micro-command 1 -> your srf | MON60 fn 103 (IRSYSP) -> ND-500 monitor LIST-SYSTEM-PARAMETERS; swapper |
| Manual | ND-05.020.01 ch. 5.3.13 (LSYSPAR) | ND-60.136 sections 8.10.11/8.10.12 (SET/LIST-SYSTEM-PARAMETERS) |

There is NO routine that fills N500DF+111B before CON5IDENT/MFPREPARE - because CON5IDENT
does not read it. Its payload cells are assigned inline. [NPL-V]

## 1. The three srf words - exact producer values [NPL-V]

`CON5IDENT` (MP-P2-N500.NPL:3617-3632, addr 147134-147177) builds the multibyte to
(5STATION, OMDACCP=3), MMSGLENGTH=7:

```
MCOMMAND = CMSYSPAR(016B) << 8 | N100IDENT(1)      (147155)
S5       = 5OMDNO SHZ 10        (= ND-100 receive OMD << 8)   (147163)
S6       = 0                                                   (147166)
S7       = 0                                                   (147166)
```

So, mapping to your srf cells:

- **word 1 (your "SYSPAR", srf consumer GIVEINT) = 5OMDNO << 8.**
  `5OMDNO` is the ND-100's receive OMD, allocated AT RUNTIME by `CONOMD` (octobus driver;
  body not in the NPL tree) via `CON5OMD` (MP-P2-N500.NPL:3567-3572) - it is NOT a fixed
  constant (offset 0 of the driver datafield LMDF).
- **word 2 (your "MODINIT") = 0 as sent by SINTRAN.**
- **word 3 (srf 2010, unnamed) = 0 as sent by SINTRAN.**

**CROSS-CHECK - your formula reproduces the live value exactly:**
GIVEINT: `((word1 & 037400) >> 3) | 100001`. With the live-observed result `100401B`:
`100401 & ~100001 = 000400B`; `000400 << 3 = 004000B` = `word1 & 037400`, so
`5OMDNO = 004000B >> 8 = 010B` on the live machine. Forward: `010B<<8 = 004000B`,
`& 037400 = 004000B`, `>> 3 = 000400B`, `| 100001 = 100401B`. **Match.**

**Meaning of GIVEINT's word (now nameable):** the answer interrupt the microcode sends to
the ND-100 is addressed to the OMD that SINTRAN connected for itself at init (CON5OMD/CONOMD)
- i.e. word 1 tells the SAMSON "when you interrupt the host, target THIS OMD entry".
Consistent with `MFPREPARE` giving the same `5OMDNO<<8` to the MF-controller (MDP1, 147113)
with the comment "OMD number on which N100 can receive messages".

**[OPEN] If your microcode ever observes a NONZERO word 2/word 3:** SINTRAN sends 0,0 (this
NPL revision), so a nonzero value would have to be synthesized by the ACCP firmware itself -
that firmware is outside everything carved here. Do not expect SINTRAN to vary them.

## 2. The 16-word block at N500DF+SYSPAR(111B) - what it actually is

- SYSPA = **111B** in SYMBOL-1/N500-SYMBOLS/N5000-SYMBOLS (all versions checked). [NPL-V]
- Only NPL touchers: MON60 fn 103 `IRSYSP` (copy 16 words N500DF+111B -> user) and fn 104
  `IWSYSP` (user -> N500DF+111B), 5P-P2-MON60.NPL:1570-1579. No other producer or consumer
  exists in the available NPL - the block is a mailbox between the ND-500 monitor program,
  SINTRAN, and (via fn 76B/121B) the swapper. [NPL-V]
- J04 caller side (BYTES, from the earlier carve): the monitor's LIST-SYSTEM-PARAMETERS
  handler (073115) issues 103B RSYSP into a local array, ALSO reads a swapper block (121B
  RDSWP), and its SET path writes back via 104B WSYSP / sends 76B TOSWP -
  `SINTRAN/ND500/nd-500-mon/mon60-callers/LIST-SYSTEM-PARAMETERS/`.
- **Content = the SET-SYSTEM-PARAMETERS tunables** (ND-60.136.04A sections 8.10.11-8.10.12),
  10 named parameters: [MAN-V names; word ORDER inferred from the command signature - [I]]

| Word [I] | Parameter (manual name) |
|---|---|
| 0 | no. of physical segments (max 2000B; needs restart) |
| 1 | clean segment at no. of page faults |
| 2 | swapout segment at no. of page faults |
| 3 | default ND-100 priority (moncalls run on behalf of the 500 process) |
| 4 | default ND-500 priority |
| 5 | max ND-100 CPU percentage (per 2-second window) |
| 6 | disk cache buffer size (2KB pages per transport) |
| 7 | no. of disk cache buffers |
| 8 | low priority factor |
| 9 | max no. of pages fixed (system-wide, incl. Monitor direct-transfer fixes) |
| 10-15 | [OPEN] spare / not derivable from NPL or manual |

The word ORDER above is the command's parameter order, NOT byte-proven; the per-word mapping
would need the J04 formatter (073154-073263, helpers 054045/000067/054430/030060 - not yet
decoded) or the swapper's copy. **None of these words go anywhere near the ACCP or your srf.**

## 3. Bottom line for O1

1. srf "SYSPAR" = `5OMDNO << 8` (host receive OMD; live = 10B; dynamic per boot - do NOT
   hardcode; it is whatever CONOMD allocated when SINTRAN connected its OMD).
2. srf "MODINIT" and srf 2010 = 0 from SINTRAN, always (this revision). A microcode consumer
   of srf 2010 that expects nonzero would be dead code against this SINTRAN.
3. The 16-word block is monitor/swapper tuning state, unrelated to the handshake; its
   [I] layout is above if you ever need it.

## Evidence index

| Claim | Where |
|---|---|
| CON5IDENT payload MCOMMAND/S5/S6/S7 | MP-P2-N500.NPL:3617-3632 (147134-147177) |
| MFPREPARE MDP1 = 5OMDNO<<8 | MP-P2-N500.NPL:3586-3598 (147100-147125) |
| 5OMDNO runtime-allocated (CON5OMD -> CONOMD) | MP-P2-N500.NPL:3567-3572; RP-P2-N500.NPL:951-958 (XX5CONOMD kickoff) |
| IRSYSP/IWSYSP = only N500DF+111B touchers | 5P-P2-MON60.NPL:1570-1579 (032702-032724) |
| SYSPA=111B | SYMBOLS/{L07,M06}/SYMBOL-1 + N500/N5000-SYMBOLS |
| LIST/SET-SYSTEM-PARAMETERS = the 10 tunables | ND-60.136.04A sections 8.10.11-8.10.12 |
| J04 LIST-SYSTEM-PARAMETERS uses 103/121/104/76 | nd-500-mon/mon60-callers/LIST-SYSTEM-PARAMETERS (BYTES) |
| Formula check 5OMDNO=10B -> 100401B | arithmetic in section 1 vs O1's live observation |

**OPEN:** words 10-15 of the block; byte-proof of the word order (J04 formatter or swapper
copy); ACCP firmware behavior for words 2-3 (outside all carves).
