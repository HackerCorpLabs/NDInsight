# CARVE ANSWER — trap-record fault-parameter offsets (link.17..) per trap class, and the MMS status word bit numbers

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVE-ANSWER-TRAP-RECORD-OFFSETS-AND-MMS-BITS-2026-08-11.md`
**Date:** 2026-08-11.
**Question (D4 plan task 3.2 residue, blocking RetroCore `AnswerTrapStop`):** inside the trap
record the microcode writes into the process's activation message (`STOPR@11B := 2`,
`TRAPN@16B`, P at `12B`/`14B`), WHICH words of the trap-dependent area (`link.17..`) hold the
fault LA and the MMS status word, for WHICH trap numbers — and what are the WR/class bit
numbers of the MMS status word in HARDWARE bit order.
**Supersedes / refines:** the two OPEN items of
`MICROCODE-ANSWER-TRAP-REPORT-FIELDS-2026-07-20.md` (its points 1 and 5 are refined below;
its link.NN header layout is confirmed unchanged).

**Sources and grades:**
- **[V-MC]** `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md` — the ND-5800 B30 microword
  listing (octobus generation). Every offset below was read from the microwords this session.
- **[MANUAL]** `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-05.017.01 EN ND-5000 HARDWARE
  MAINTENANCE.md` — appendix A.12 (the MMS status register, bit by bit) and the chapter-6
  worked examples (a decoded message dump for trap 51B, and the page-fault report).
- **[NPL]** `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\MP-P2-N500.NPL` — address-stamped
  compiler listing (its `DECOMESS` address `135161` matches the address the L07 carve cites,
  but NPL remains a different revision: logic evidence, not bytes).
- **[V]** `tools\sintran-segment-carver\versions\L-VSX-500\segments\030-S3SM5.bin`
  (base `40000B`, big-endian) — the consumer/repackaging side, byte-verified (see the
  companion doc `CARVE-ANSWER-PRSTART-STOPINFO-SOURCE-2026-08-11.md`).

---

## 0. TL;DR — the answer tables

All offsets are ND-100 HALFWORD offsets from the message start (the same `link.NN` numbering
as vendor manual ND-05.012.01 §13.16 and the SINTRAN symbols `STOPR=11`, `TRAPN=16`).

### 0.1 Common header (all stop-on-trap classes) [V-MC, four independent anchors]

| link.NN | width | content | microword |
|---|---|---|---|
| 11 | hw | `STOPR := 2` (TRAPCODE) | `013513` (GEN1) / `013571` (GEN4B) |
| 12-13 | 32-bit | trapping P | `013572` (GEN4: from `IAC,P`) / `013515` (GEN1: from saved context) |
| 14-15 | 32-bit | restart P | `013573` (GEN4: same P) / `013517` (GEN1: from saved context) |
| 16 | hw | `TRAPN` | `013574` (GEN4C: `SARG=000046`) |

### 0.2 PAGE FAULT — TRAPN = 46B — `TRAP_GEN4` (`013560-013605`) [V-MC]

| link.NN | width | content | source register | microword |
|---|---|---|---|---|
| **17-20** | 32-bit | **fault LA** (full 32-bit VA; seg = top 5 bits) | SC13 = SRF rec word 2 = `DMM,LA` | `013602` |
| **21** | hw | **physical segment number** (`DMM,CAP & 017777`) | SC11 = SRF rec word 4 | `013603` |
| **22-23** | 32-bit | **MMS status word** (`DMM,STS`, hardware bit order) | SC14 = SRF rec word 1 | `013604` |

`DMM,PHYS` is READ back from the record (`013600` → SC12) but **never written to the
message**; the WR register is not even read on this path. So a page-fault record carries
exactly three fault parameters: LA, phys-seg, MMS.

### 0.3 General trap stop — `TRAP_GEN3` (+`3B`/`3C`) — used by the PV / trace / handler-missing / hardware-fault stop sequences, incl. TRAPN = 51B [V-MC offsets; identities pinned by record order + the manual's decoded dump]

| link.NN | width | content | microword |
|---|---|---|---|
| **17-20** | 32-bit | **fault LA** | `013536` (write; read `013535` src+0x28) |
| **21-22** | 32-bit | **MMS status word** | `013540` (read `013537` src+0x24) |
| 23-24 | 32-bit | physical address | `013542` (read `013541` src+0x2C) |
| 25 | hw | physical segment number (CAP & 017777) | `013544` (read `013543` src+0x30) |
| 26 | hw | WR register | `013546` (read `013545` src+0x34) |
| 27 | hw | ACCP status (ASTS) — only when `TRAP_GEN3B/3C` runs | `013551` / `013555` |
| 30 | hw | BADAP status — only when `TRAP_GEN3B/3C` runs | `013552` / `013557` |

**The load-bearing difference between the two classes** (this is exactly the vendor's
"varies depending on trap number", and the direct cause of the emulator's
`Logical address 0 0B` symptom if the wrong class layout is used):

- LA is at **17-20 in BOTH classes**.
- MMS status is at **22-23 for page fault (46B)** but **21-22 for the GEN3 class**.
- link.21 for page fault is the **physical segment halfword**, not part of the status.

### 0.4 MMS status word — hardware bit order [MANUAL, ND-05.017.01 appendix A.12 (page 276)]

| bits | name | meaning |
|---|---|---|
| 31-29 | STATE7-5 | **the access class**: `000` POFF read, `001` POFF write, `010` PXING, `011` read with write permit, `100` read, `101` write, `110` PHS read, `111` PHS write |
| 28-24 | STATE4-0 | MM state register low bits ("the state number leading to trap") |
| 23 | LOCK | lock request |
| 22 | DIRTY | waiting a dirty request |
| 21 | ALTF | current ALTF bit |
| 20 | WTIP | capability Written-In-Page updated |
| 19 | WRIT | write permitted |
| 18 | PARA | parameter access permitted |
| 17 | SHAR | shared segment |
| 16 | USED | the USED entry bit |
| 15 | PHSUSED | physical segment use of WR |
| 14 | PHSWPTP | physical segment written in page table |
| 13 | PHSWMSS | physical segment TSB miss |
| 12 | INHCGW | inhibit cache write |
| 11 | MISS | **TSB miss** |
| 10 | DMAT | (A)DOM-reg match DDOM |
| 9 | PSMAT | PS-reg match DPS |
| 8 | LAMAT | LA-reg match DLA |
| 7 | spare | |
| **6** | — | **0 = DATA, 1 = PROGRAM** ("set by the microprogram") |
| 5 | ZERO | zero in the WR- or CAP-register |
| 4 | TRAPS | indicates a trap to the CPU |
| 3-0 | TRAP3-0 | trap code: `0000` addr out of range/need one more index level, `0001` alt-protect viol, `0010` write-protect viol, `0011` index error, `0100` memory error, `0101` memory timeout, `0110` indirect cap to another machine, `0111` indirect cap within machine, `1000`-`1010` zero in capability (variants), `1011` zero in PS-table entry for PS / zero cap (variants), `1100` zero in last-level index for PS, `1101` **zero in physical segment table entry**, `1110` zero in 2nd-level index entry, `1111` zero in last-level index entry |

**Consequences that retire earlier imprecision:**
- **Read-vs-write is NOT a single "WR bit".** It is the STATE7-5 field, bits 31-29
  (read=100, write=101, POFF read=000, POFF write=001, PHS read/write=110/111). The "WR" that
  appears in trap reports is the **WR REGISTER**, a separate halfword at link.26 (GEN3 class
  only) — it holds the physical page address the MMS lookup used, not a direction flag.
- **Program-vs-data = bit 6** (plus which fault class fired, IMM vs DMM). The report line
  "`DATA POFF read request`" = bit6=0 + STATE7-5=000.
- The old `MMS_SIX0` note ("top 2 bits = fault class") is refined: `0xC0000000` = STATE7-6,
  the top of the access-state field; `0x1F000000` (PF_NORM mask) = STATE4-0. Same silicon
  facts, now with the manual's names and full enumeration.

**Cross-checks against the manual's own examples (computed, not eyeballed):**
- HWF example MMS `= 5B`: STATE7-5 = `000` → "POFF read request" (printed), bit6=0 → "DATA"
  (printed), TRAP3-0 = `0101` = memory timeout — and the same dump prints
  "BADAP: 140B => Memory timeout". Consistent three ways.
- Page-fault example MMS `= 22701016000B` = `0x97041C00`: STATE7-5 = `100` = read request,
  bit 11 (MISS/TSB miss) set. The shape of a page fault.

---

## 1. Method — the ADACT/ORCON address pipeline, and why the offsets are trustworthy

The B30 trap generators address the message through `AD_ARTI=1 ADACT ... ORCON=0xNN` fields.
Decoded rule (established this session):

> **The `ADACT`/`ORCON` on microword N computes the memory address used by the `WR,POF` /
> `RD,POF` access on microword N+1. `ORCON` is the BYTE offset from the message base
> (`DAC,DPA := srf[ADR_MESS]`, loaded at `013562`/`013505`). Halfword offset = ORCON/2.**

Proof — four independent anchors where the result must equal an already-known offset
(SINTRAN symbols `STOPR=11`, `TRAPN=16` from `N500-SYMBOLS.SYMB`; trapping/restart P from
vendor §13.16):

| setup word (ADACT) | ORCON | access word | writes | = link.NN | known value |
|---|---|---|---|---|---|
| `013567` | 0x12 | `013571` | `BM01`=2, halfword | 0o11 | STOPR ✓ |
| `013571` | 0x14 | `013572` | SC13 (=`IAC,P`), word | 0o12 | trapping P ✓ |
| `013572` | 0x18 | `013573` | SC13, word | 0o14 | restart P ✓ |
| `013573` | 0x1C | `013574` | SC14 (=`SARG 000046`), halfword | 0o16 | TRAPN ✓ |

Four for four. The fault-parameter offsets in §0.2/§0.3 are the SAME pattern continued
(`013601` ORCON=0x1E → hw 0o17; `013602` ORCON=0x22 → hw 0o21; `013603` ORCON=0x24 → hw 0o22;
GEN3: 0x1E/0x22/0x26/0x2A/0x2C → hw 17/21/23/25/26; GEN3B/3C: 0x2E/0x30 → hw 27/30).
Word-vs-halfword width is read off the access microword itself: `TYP,HW` present = halfword,
absent = 32-bit word.

## 2. Where the parameter VALUES come from — the SRF trap record

`TRAP_DFC` (`012702-012723`) collects the DMM fault state, then `TRAP_TO_SRF`
(`012675-012701`) stores a 5-word record DESCENDING from `RFA1 := BM05 = 0o40`:

| SRF cell | value | collected at |
|---|---|---|
| 0o40 | SC14 = **`DMM,STS`** (MMS status; HWF paths OR in `MARG 005`) | `012565` / `012735` |
| 0o37 | SC13 = **`DMM,LA`** (fault LA; HWF-POFF variant uses `DAC,EAO`) | `012715` / `012712` |
| 0o36 | SC5\|SC7 = **`DMM,PHYS`** (top bit masked) | `012716`, `012717` |
| 0o35 | SC6 = **`DMM,CAP & 017777`** (physical segment number, 13 bits) | `012721-012722` |
| 0o34 | SC4 = **`DMM,WR`** | `012723` |

(The IMM/instruction-fetch twin `TRAP_IFC`/`TRAP_IHWF` fills the same record from the IMM
unit — `012572`, `012732-012734`.)

`TRAP_GEN4` reads this record back (`013575`: `RFA1 := 0o40`; `013576-013601`: four `RF1D`
reads → SC14=STS, SC13=LA, SC12=PHYS, SC11=CAP) and writes LA/CAP/STS to the message —
which is how the §0.2 identities are known, not guessed.

`TRAP_GEN3` copies from a MEMORY copy of the same record (source `AA=6` base + byte
0x24..0x34; the source ORCON order 0x24=STS, 0x28=LA, 0x2C=PHYS, 0x30=CAP, 0x34=WR is
exactly the record order at 4-byte stride). The identity of the `AA=6` base register
(context-block save area written by `TRAP_SAVE @013161`) is **[INFERRED]** — the
DESTINATION offsets do not depend on it. The §0.3 identities are additionally pinned by the
manual's decoded message dump for trap 51B (ND-05.017.01 pages 119-120), which prints, in
this order: Trapping P, Restart P, Trap number, **Logical address, MMS Status, Physical
address, Phys.segment, Physical page-WR, ACCP status (ASTS), BADAP** — one-to-one with
links 12, 14, 16, 17-20, 21-22, 23-24, 25, 26, 27, 30.

## 3. Which trap numbers use which layout

- **GEN4 layout (§0.2): TRAPN = 46B (page fault) only.** `TRAP_PGF` (`013446`) is the only
  vector that runs `TRAP_GEN4`; the 46B constant is hard-coded at `013563`. The page-fault
  stop may ALSO wake the swapper (`TRAP_SWAP @013453`).
- **GEN3 layout (§0.3): every other stop-on-trap sequence.** The vector sequences
  (`013400-013423`): generic `TRAP` = GEN1+GEN2+GEN3C; `TRAP_TRAC` = GEN1+GEN2+…+GEN3C;
  `TRAP_PV` = GEN1+GEN2+GEN3+GEN3B; `TRAP_THM` = GEN1+GEN3+GEN3C. ASTS/BADAP (links 27/30)
  appear only when GEN3B/3C is in the sequence.
- Known TRAPN values on the GEN3 side: **51B = hardware fault** [MANUAL ch.6, printed
  "Trap number 051B => 41D"], **45B = THM (NOT protect violation)** [CORRECTED
  2026-08-11 by microcode execution: the CONT-STORE-10611 sorter routes protect
  violations to TRAPN 44B (subtype 10B WRPV, shared arm with zero-capability 22B);
  SINTRAN's {44B,46B,51B} parameter grouping and the 5SWAP live measurement agree.
  The earlier "45B = protect violation" was a reference-manual inference, now refuted].
- Independent corroboration of the grouping, from the L07 consumer bytes [V]: the ND-500
  System Monitor's trap-stop builder (`030-S3SM5` @ `064304-064352`) special-cases TRAPN
  groups `{25B,26B,27B}`, `{44B,46B,51B}` and `{45B}` before repackaging — i.e. SINTRAN's
  own code expects exactly these trap numbers to carry fault parameters.
- SINTRAN validity limits [NPL, address-stamped]: `TRAPDECODER @135324` rejects TRAPN > 53B
  (`ILTRAP`); `DECOERRMESS @135245` accepts an error-status (`N5STA=4`) trap only when
  TRAPN=46B and MICFU is one of the legal page-fault functions.

## 4. Who consumes what (so the emulator knows what must be right)

1. **SINTRAN level-12 driver reads ONLY `TRAPN@16`** (and STOPR/MICFU). `TRAPDECODER
   @135320`, `DECOERRMESS @135240` [NPL listing, addresses match the L07 carve]. Page fault
   → tag the message (`MSWPFAULT SHZ 10 + 46B` back into TRAPN, `135361-135367`) → activate
   the swapper. Any other trap → error log (`9FLER`, 4 params: trapcode/process/rtref/CPU)
   and restart the ND-100 process. **The fault LA / MMS words are never decoded at this
   level.**
2. **The detailed decode happens in the ND-500 Monitor (`MON-DEBUG:PROG`)**, from the 200B
   "stop (trap) info" block that MON 60B function 012 (RUNN) hands back — which is a
   verbatim copy of message halfwords **12B..41B** (see the companion carve
   `CARVE-ANSWER-PRSTART-STOPINFO-SOURCE-2026-08-11.md`). So the offsets above, minus 12B,
   are the offsets inside the user-visible stop block.
3. **The swapper** receives the ADDRESS of the faulted message (`5ACTSWAPPER @144762`
   [NPL]) and pulls/parses it on the ND-500 side; the manual confirms it takes the logical
   page number from the fault LA ("bits 26-11 from the logical address",
   ND-05.017.01:4803).

## 5. What RetroCore `AnswerTrapStop` must write (the deliverable)

For a stop-on-trap answer into the activation message (then `N5STA := 3`, or `4` when the
run-flag says error — `TRAP_END @013606-013612`):

```
hw 11        STOPR   = 2
hw 12-13     trapping P (32-bit)
hw 14-15     restart P (32-bit)
hw 16        TRAPN
-- TRAPN = 46B (page fault):
hw 17-20     fault LA (full 32-bit VA)
hw 21        physical segment number (CAP & 017777)
hw 22-23     MMS status word (hardware bit order, §0.4)
-- other stop traps (PV 45B, HWF 51B, trace, handler-missing, ...):
hw 17-20     fault LA
hw 21-22     MMS status word
hw 23-24     physical address
hw 25        physical segment number
hw 26        WR register
hw 27        ASTS   (hardware-fault flavor only)
hw 30        BADAP  (hardware-fault flavor only)
```

MMS status construction: put the access class in bits 31-29, DATA/PROGRAM in bit 6, TSB-miss
in bit 11, trap code in bits 3-0 — never a C#-struct-ordered word.

## 6. Open / not established

- **Classic ND-500 (CONT-STORE-10611) byte verification.** The offsets above are from the
  ND-5800 B30 image. The classic image's trap writer lives in the `TRAPINF` decode region
  (`E:\Dev\Ronny\ND500UC\docs\MC\CONT-STORE-10611.md` `011014-011232`) and was NOT decoded
  this session. The message contract is generation-shared (ND-05.012.01 — the CLASSIC micro
  program guide — documents the same link table; SINTRAN L07 is a single consumer for both),
  so the layout is expected identical, but that expectation is **[INFERRED]** until the
  10611 trap path is read. [OPEN]
- The `AA=6` ADACT base register identity (GEN1/GEN2/GEN3 source base). [INFERRED: the
  context-block trap save area]
- The semantic identity of the two words GEN2 reads-and-zeroes at src+0x18/0x1C
  (`013520-013524`). [OPEN, does not affect the message layout]
- TRAPN values for the trace / handler-missing / {25B,26B,27B} group names. [OPEN]

## 7. Evidence appendix (exact citations)

- `MICRO-5800-B30.md`: TRAP vectors `013400-013476`; `TRAP_GEN1` `013501-013517`;
  `TRAP_GEN2` `013520-013533`; `TRAP_GEN3` `013534-013546`; `TRAP_GEN3B` `013547-013552`;
  `TRAP_GEN3C` `013553-013557`; `TRAP_GEN4` `013560-013605`; `TRAP_END` `013606-013612`;
  collection `012545-012741` (`TRAP_TO_SRF` `012675-012701`, `TRAP_DFC` `012702-012723`,
  `TRAP_IHWF/DHWF` `012732-012737`).
- `ND-05.017.01`: A.12 full bit table (page 276, file lines 11508-11594); ch.6 message dump
  (pages 119-120, lines ~4700-4770); HWF report + error-device line (lines 4485-4530);
  page-fault report (lines 4789-4815); "not latched on final access" caveat (page 115).
- `MP-P2-N500.NPL`: `DECOMESS` 135161, `DECOERRMESS` 135240, `TRAPDECODER` 135314-135425,
  `5ACTSWAPPER` 144755-145147 (listing-address-stamped NPL).
- `030-S3SM5.bin` byte checks: see the appendix of
  `CARVE-ANSWER-PRSTART-STOPINFO-SOURCE-2026-08-11.md`.
