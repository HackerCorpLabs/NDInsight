# Answers: the ACCP <-> ND-5000 seam contract (Q1-Q4)

**Date:** 2026-08-04
**Answers to:** `HANDOFF-ACCP-CPU-SEAM-CONTRACT-2026-08-03.md`
**From:** the CpuND5000 microword side (swept the real MICRO-5800-B30 image)
**Method:** decoded the actual AFLAG-test microwords in the B30 control store. Source of truth is the
raw microword, not the manual. Evidence grades: **[V]** = read straight off a real microword,
**[D]** = derived from the microword structure, **[OPEN]** = the microcode does not settle it.

**CRITICAL decoding rule used throughout:** the B30 has a **one-word condition delay** - a word's
`COND,MZRO` tests the ALU flags left by the PREVIOUS word, not its own AND. Reading the scan naively
(each word tests its own bit) gives the WRONG dispatch. The delayed reading is proven correct because
it reproduces your own known-good fact ("SCAN_ACCP bit 5 -> TRAP_OCBA @016550, bit 6 falls through").

Bit numbering: `BMnn` is an OCTAL bit index. `BM05`=bit 5, `BM06`=bit 6, `BM07`=bit 7,
`BM10`=bit 8, `BM11`=bit 9, `BM12`=bit 10, `BM13`=bit 11, `BM14`=bit 12.

---

## Q1. Which AFLAG bits does the microcode actually TEST? [V]

AFLAG is read with `A,SPEC,AFLAG` (into SC13) and then bit-tested with `ALU,AND A,BMnn B,SC13`.
Every AFLAG bit the B30 tests, with the microword that tests it:

| AFLAG bit | BM (octal) | Meaning | Where tested | Dispatches to |
|---|---|---|---|---|
| 5  | BM05 | async trap | `SCAN_ACCP1` @016560, `ATRAP_CHK1` @016601 | SCAN: TRAP_OCBA @016550; ATRAP: **TRAP_ATRP1** @016614 |
| 6  | BM06 | other trap | `SCAN_ACCP2` @016562, `ATRAP_CHK2` @016604 | SCAN_ACCP3 / ATRAP_CHK3 dispatch |
| 7  | BM07 | data fault (IMM/DMM, MMS) | @012563 (after `TRAP_NDF`-area AFLAG read @012562) | data-fault path |
| 8  | BM10 | instruction fault | @012570 (`TRAP_NDF` @012567) | instruction-fault path |
| 9  | BM11 | AOBF (data from ACCP ready) | `ACCP_READ`/`ACCP_WAITI` spin @016371/016375 | spins until set, then reads AOB |
| 10 | BM12 | AIBF (data to ACCP not yet taken) | `ACCP_WRITE`/`ACCP_WAITO` spin @016402/016406 | spins until clear, then writes AIB |
| 11 | BM13 | power-fail | `SCAN_ACCP` @016555, `ATRAP_CHK` @016575 | TRAP_PWF |
| 12 | BM14 | OCB pending | `SCAN_ACCP` @016556, `ATRAP_CHK` @016576 | TRAP_OCBAK @016552 |

**So the microcode tests the full manual AFLAG list (bits 5-12).** No bit outside 5-12 is ANDed
against the AFLAG word. This is the answer to the "if a bit outside 5-12 is tested" clause: there
isn't one.

### The two dispatchers, decoded (with the 1-word delay applied)

`SCAN_ACCP` @016554 - the IDLE-loop AFLAG scanner:
```
016554 SCAN_ACCP : A,SPEC,AFLAG -> SC13
016555           : AND BM13(11)                # result -> next word's COND
016556           : AND BM14(12)  C,SEQ MZRO?   # tests bit 11 -> set: 016557 -> TRAP_PWF
016560 SCAN_ACCP1: AND BM05(5)   C,SEQ MZRO?   # tests bit 12 -> set: 016561 -> TRAP_OCBAK
016562 SCAN_ACCP2: AND BM06(6)   C,SEQ MZRO?   # tests bit 5  -> set: 016563 -> TRAP_OCBA
016564 SCAN_ACCP3: XOR           C,SEQ MZRO?   # tests bit 6  -> set: dispatch; else DUMMY_2
```

`ATRAP_CHK` @016572 - the trap-check scanner (same AFLAG-in-SC13, different targets):
```
016575 : AND BM13(11)
016576 : AND BM14(12) C,SEQ MZRO?   # tests bit 11 -> TRAP_PWF (via 016577/016600)
016601 ATRAP_CHK1: AND BM05(5)  C,SEQ MZRO?   # tests bit 12 -> TRAP_OMESS1 (via 016602/016603)
016604 ATRAP_CHK2: AND BM06(6)  C,SEQ MZRO?   # tests bit 5  -> **TRAP_ATRP1** (via 016605/016606)
016607 ATRAP_CHK3: XOR          C,SEQ MZRO?   # tests bit 6  -> dispatch; else DUMMY_2
```

---

## Q2. What bit does the microcode read for ATRAP, and for FATAL?

### ATRAP = AFLAG bit 5 (BM05). [V]

`ATRAP_CHK1` @016601 tests AFLAG bit 5 and, when set, dispatches to **`TRAP_ATRP1` @016614** - the
async-trap-processing handler (it reads the async trap word, `TRAP_ATRP2`/`TRAP_ATRPV` @016622/016623
do a `JMPREL` classify into `TRAP_NOTREC` etc.). That is the ACCP async-trap entry. Bit 5 is exactly
the bit your `AccessModule` already labels "async trap word pending", so:

> **Assign `AflagAtrapBit = 5`.** Evidence: the microword at 016601 (`AND A,BM05 B,SC13`) whose
> set-branch reaches `TRAP_ATRP1`.

(Note bit 5 is also tested in `SCAN_ACCP`, where its set-branch goes to `TRAP_OCBA` instead - same
bit, different meaning per call site. In the trap-scan context ATRAP_CHK, bit 5 is the async trap.)

### FATAL = NOT an AFLAG bit. It arrives as an ACCP trap-word code. [V for "no AFLAG bit"; D for the path]

There is **no `AND A,BMxx B,SC13` microword whose branch reaches `TRAP_FATAL`.** `TRAP_FATAL` @012575
is reachable from exactly one place, @013217, which is one arm of the fixed dispatch fan
013215/013217/013224/013235 - and the other three arms all go to `TRAP_ACCP` @013313, the ACCP
trap-word CLASSIFIER. So FATAL is decoded from the **contents of the ACCP async-trap word** (delivered
over AOB and classified in TRAP_ACCP), not from an AFLAG bit position.

> This is exactly the "the microcode never tests it - it reaches the CPU by another path" outcome
> your handoff anticipated. **Keep `AflagFatalBit = BitNotModelled`** - composing FATAL into AFLAG
> would be wrong. FATAL should be delivered as an async-trap WORD value that `TRAP_ACCP` classifies,
> reached because ATRAP (AFLAG bit 5) fired and the microcode then read the trap word over AOB.

Concretely for the seam: the ACCP raises **ATRAP (AFLAG bit 5)** to get the CPU's attention for BOTH
an ordinary async trap and a fatal one; the CPU enters TRAP_ATRP1, reads the trap word, and
`TRAP_ACCP` decides normal-vs-fatal from that word. FATAL is a payload, not a flag.

---

## Q3. What does the microcode expect at cold start? [D]

`SCAN_ACCP` is the **idle-loop** AFLAG poller: it is entered from the IDLE loop and from post-instruction
"send" points (`IDLE_1` @024702, `SEND_112` @005212, @012627, @015440, @017462, @024702, @025502,
@025546). It reads AFLAG and, in priority order, dispatches **power-fail (bit 11) -> OCB (bit 12) ->
async-trap (bit 5) -> other (bit 6)**, else `DUMMY_2` (return to idle). So once the microprogram is
running, any AFLAG bit you raise WILL be seen and dispatched on the next idle scan.

What the microcode does NOT do: there is no AFLAG poll on the pre-STARTMIC path - the scan lives in
the running microprogram's idle loop, which only exists after `STARTMIC` starts it. **So the ACCP is
free to run its own selftest and need not stay quiet before STARTMIC; nothing on the CPU side reads
AFLAG until the microprogram idle loop is running.** [D - from the call-sites of SCAN_ACCP, all inside
running microcode; a cold pre-CS poll would have to be in the reset microword path and is not.]

Priority confirms your `TRAP_OCBA`/fall-through test: SCAN dispatches bit 5 -> TRAP_OCBA and bit 6 to
the SCAN_ACCP3 branch - never the same destination, matching your regression test.

---

## Q4. Does an AOB read clear NARROW or WIDE? [OPEN - but the microcode leans NARROW]

The microcode's normal AOB read is `ACCP_READ` @016371: spin on AOBF (bit 9), then
`016374: A,SPEC,AOB -> SC13 ; T,RETURN` - it reads AOB and returns **without re-testing anything.**
It does not re-test FATAL after the read (and could not - FATAL is not an AFLAG bit; see Q2).

Because FATAL is a trap-WORD code delivered via AOB (Q2), the meaningful question becomes "does
reading the AOB word consume the ATRAP signal that carried it". The microcode reads the trap word once
(in the TRAP_ATRP1 path) and classifies it; there is no microword that reads AOB and then re-tests an
AFLAG trap bit to see whether it survived. That is consistent with a **NARROW** clear (the read
consumes AOBF + ATRAP; a distinct FATAL condition, being a word value, is not something an AOB read
could "lose" as a flag).

> **Recommendation: keep `AobReadClearsWide = false` (narrow).** The microcode gives no evidence for a
> wide clear, and since FATAL is a trap-word payload rather than a flag, "FATAL survives an AOB read"
> is not decided by the AOB read at all. Mark this **[OPEN]** pending the hardware schematic - it is
> the one question the microcode alone cannot fully settle, but nothing in the microcode contradicts
> narrow.

---

## Net, for your "the moment we have answers" list

1. `AflagAtrapBit = 5` (evidence: 016601 -> TRAP_ATRP1). Drop the sentinel for ATRAP.
2. `AflagFatalBit` stays `BitNotModelled` - FATAL is an async-trap WORD code classified by TRAP_ACCP,
   not an AFLAG bit. Deliver FATAL as a trap-word value reached via the ATRAP (bit 5) path.
3. `AobReadClearsWide = false` (narrow) - no microcode evidence for wide; [OPEN] on the schematic.
4. The idle AFLAG scan (SCAN_ACCP) sees bits 5/6/11/12; the fault scan (TRAP_NDF) sees 7/8; the
   ACCP spin loops poll 9 (AOBF) / 10 (AIBF). Raising any of these post-STARTMIC will be dispatched.

## Microword citations (all from MICRO-5800-B30, octal CS addresses)

- AFLAG read: `A,SPEC,AFLAG` @016554 (SCAN_ACCP), @012562/@012567 (TRAP_NDF), @016372/@016376/@016403/
  @016407 (ACCP spin), @016400.
- Bit tests: 016555 BM13, 016556 BM14, 016560 BM05, 016562 BM06 (SCAN_ACCP); 016575 BM13, 016576 BM14,
  016601 BM05, 016604 BM06 (ATRAP_CHK); 012563 BM07, 012570 BM10 (fault scan).
- Trap targets: TRAP_PWF, TRAP_OCBAK @016552, TRAP_OCBA @016550, TRAP_ATRP1 @016614, TRAP_OMESS1,
  TRAP_FATAL @012575 (only from 013217 via TRAP_ACCP @013313 classifier).
- AOB read: `A,SPEC,AOB` @016374 (ACCP_READ, RETURN with no FATAL re-test).
