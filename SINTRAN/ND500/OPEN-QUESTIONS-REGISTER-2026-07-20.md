# Open questions register - ND-500 / 3022 / swapper track (2026-07-20)

**Full path:** `SINTRAN/ND500/OPEN-QUESTIONS-REGISTER-2026-07-20.md`

Everything this track does NOT know, with how each item can actually be answered. Written because
several of these were previously "answered" by inference and had to be retracted; anything not
listed as [V] here is not settled.

**Answer routes:** `CARVE` = bytes we already have (SINTRAN segments, swapper PSEG/DSEG, NPL);
`MICROCODE` = the ND-5000 microcode-CPU track (B30 image + decoder); `LIVE` = an experiment in the
boot harness; `MANUAL` = a document we may not possess.

**Standing rule for all of these: "UNKNOWN" beats a plausible wrong answer.** Four claims in this
track were retracted after being stated too strongly (swapper-is-microcode, DSEG-never-loaded,
control-store-vs-mailbox-as-branches, and a RIOM operand fix that contradicted its own tests).

---

## A. Blocking D4 right now

### A1. Who builds the PCB/PST, and when? [MICROCODE, highest value]

The emulated MMU keeps `PST`/`PCBTable` as C# arrays; real hardware walks memory-resident tables.
A scan of all 8 MB of the MPM window for a PSTE naming either page table SINTRAN DMA'd
(`(0xDD << 2)|mode`, `(0xDC << 2)|mode`) found **0 candidates**, so SINTRAN does not build them
during PLACE-DOMAIN, and MPM is the only memory SINTRAN can write in our model.
Candidates, none verified: the swapper itself once running; the microcode at process start (from the
process context block / `LCNTXT`); CPU-internal state that never appears in memory.
**Consequence while unknown:** every segment the swapper touches needs a hand-built capability, which
cannot converge (observed: program 0 and 1; data 0, 1, and 13).

### A2. Where does the program SEGMENT number come from at 3START? [MICROCODE]

SINTRAN's 21B image sends `P = 0x00000004` - offset only, **no segment bits** - yet the swapper is
linked at segment 1 and its own operands are segment-1 addresses [V, carve]. Neither halfword order
can produce `0x08000004`. Something supplies the segment and we do not model it. The emulator
currently takes segment 1 from the carve.

### A3. Where does PSTP come from? [MICROCODE]

The whole 0-based 21B block is `0=P 1=L 2=B 3=R 4..7=I1..I4 8..11=A1..A4 12..15=E1..E4 16=ST1
17=ST2 18=PS 19=TOS 20=LL 21=HL 22=THA 23=CED 24=CAD 25..28=mic1..4 29..36=OTE/CTE/MTE/TEMM`.
There is **no PST-base entry**. In the emulator `PSTP` is only ever written by `MMUConfiguration`.
Real hardware must load it from somewhere: control store, a fixed physical convention, the context
block, or once at CPU init.

### A4. Is the MMU enabled at swapper start, or does the swapper enable it? [MICROCODE or CARVE]

We enable both program and data MMU as part of starting the swapper. If the real swapper begins
untranslated and switches translation on itself after building its tables, our start state is wrong
in an easily-fixed way. Carve route: disassemble further from `SWAPPER-K01.PSEG` entry.

### A5. Why is SWPINFO empty when the swapper asks for work? [LIVE, then CARVE]

Current stop: after a correct RIOM the swapper null-derefs a pointer from its message
(`w4 laddr r2.(10)` with `r2 = 0`) at `PC=0x0800913B`. A post-run dump shows both the delivered
buffer and the ND-100 source `0x420E30` all zero - but that dump is taken AFTER RUN, so it does not
establish the state at transfer time. **LIVE probe needed: capture `0x420E30` at the moment of the
RIOM.** Then: is "no work" the expected answer, and is the swapper supposed to take an earlier branch
on a field we are not supplying?

---

## B. The transport gap (overlaps the octobus track's Q4)

### B1. What actually delivers the swapper's PSEG and DSEG into memory? [CARVE or MICROCODE]

`SWAPPER-K01.PSEG` appears at physical `0x06F800` byte-identical (38,161 bytes) and the DSEG content
is present at `0x24800` (signature-verified) - but **no 14B RESIWR names either address**; the 44
transfers stop at `0x6F7FF` and are 40/44 zero pages. They carry only page tables plus zeros.
So both segments arrive by a path this track never traced.
Carve route: `500IN` init state machine step 3 / `LDSWA` (`143551`-`143621`).
Microcode route: is a MICFU or ACCP block-move involved, or is it pure ND-100-side window writes?

### B2. What is the descriptor at `0x6F000`? [CARVE]

The third structure SINTRAN DMAs (`02 C0` at +3) is undecoded. It may be the segment/PST descriptor
that answers A1 or A2.

---

## C. Protocol questions shared with the other tracks

### C1. Does the B30 image ever accept MICFU 21B (3WREG)? [MICROCODE - their Q3]

**Correcting how this track's evidence is being cited:** we verified that **SINTRAN SENDS** 21B then
23B. We did NOT verify that any real CPU accepts it - the thing accepting it is our own servicer,
which we wrote, and which we extended this session with a 17B handler precisely because SINTRAN kept
re-sending the cycle. So this track cannot corroborate "and it works there" as hardware behaviour,
and does not contradict a `20,21 -> MSG_ILLEG` dispatch table.

### C2. RIOM operand 2 - is REGISTER a legal addressing mode? [ANSWERED 2026-07-20 - MICROCODE + MANUAL]

**No. REGISTER and CONSTANT are BOTH illegal for the buffer operand.** The microcode fetches operand
2 with a **LADDR (load-address) request** (`RIOM_1` at CS `012261` = `LADDR EA2SAVE ADACT`; LADDR is
value 1 of the microword MEMORY field, the slot that otherwise carries READ/WRITE/RD,POF - so it is
a load-ADDRESS request, not a data access). Manual ND-05.009.4 **section 15.4** states the rule for
that request verbatim: "The address of the operand is loaded into the specified register. Registers
and constants have no address in memory and are illegal as operands." Verified against the manual
directly. This independently re-confirms the earlier CONSTANT removal.

The correct trap is **IOS (Illegal Operand Specifier, trap 34, non-ignorable)**, whose definition
(`:2215`) covers instructions that "do not allow register or constant operands" - NOT IOV (trap 16,
`:2132`), which is a value-range check. IOS is listed per-instruction inconsistently (section 15.4's
own trap line omits it), so its absence from section 16.23 is not evidence.

**IMPLEMENTED** (RetroCore commit 84ca6098d): `Riom.cs` guards operand 1 against REGISTER, CONSTANT
and CONSTANT_SHORT and raises IOS via `TrapIllegalOperand`; metadata already forbade both modes for
this operand; tests `Test_RIOM_RegisterBuffer_RaisesIos` / `Test_RIOM_ConstantBuffer_RaisesIos` added
(9/9 IO suite). The C-side port (nd500x) is the other track's pass.
NOTE: the "templates carry per-operand mode info" idea was WRONG - their addressing-mode bits are
identical across all three RIOM operands; only the size field differs. That is now moot.

### C3. RIOM's ND-100 address width [MICROCODE, low value]

No manual states one. The microcode applies no masking in the loop. Both emulators now use a 24-bit
ceiling as an **emulator convention** (the ND-100's 24-bit bus / 16 MW is the nearest thing to a
source; the earlier 22-bit figure traced to an emulator doc). Nothing live depends on the choice.

### C4. Is the swapper's message buffer PRIVATE or SHARED ND-100 memory? [CARVE]

`SetND100PrivateOffset(0)` is an unproven convention. The `0x210718 * 2 = 0x420E30` agreement spans
two address spaces - ADRZERO is the ND-100's VIEW of the window - and the only document asserting
"private" derives it from the RIOM manual text, circularly. Carve route: SINTRAN's `SWMSG`
allocation (`MSINIT`).

### C5. What does the microcode put in a trap report? [MICROCODE]

Our trap fields are wrong: for a fault the CPU records as an INSTRUCTION fetch at `0x080081A5`,
SINTRAN printed `DATA segment READ access / Logical address 1 100645B`; a segment-1 data write
printed `Logical address 0 0B`. Which word holds the access type, which the failing logical address,
and how is the segment encoded?

### C6. Is 200B the swapper's error-code namespace? [CARVE]

SINTRAN prints `ERROR CODE: 200B` from the swapper. Repo carve notes use `200B = hwfault` in the
ND-5000/ACCP code space; that the SWAPPER shares that namespace is **not** verified. Is there a list
of swapper error codes?

### C7. The classic-500 (Control II / 5015) physical path for RIOM [PARTIALLY ANSWERED 2026-07-20, MANUAL]

All microcode evidence is ND-5000 generation. The `RIOM_0..3` labels also exist in MICRO-5200, so the
loop structure probably carries over.

**Partial answer (MANUAL route, 2026-07-20):** the physical path IS described by manuals we hold —
see `NDIX-KERNEL-INTERFACE-EVIDENCE-2026-07-20.md` §6. The 5015's DATA-IN/DATA-OUT/TAG-OUT registers
are **microcode unit-select codes on the internal CDB/XD bus** (NEC-01 course, unit tables at
lines 1098-1103 + page 57; ND-30.013.02 §3.13 "when micro-programmed"). The microcode drives
**TAG-OUT codes 6/7** ("read/write DATA register **and ND-100 memory**") with the 3022's 24-bit MAR,
bounded by the 3022 DMA limit registers, over the 5015->3022 DBU cable — that is the classic path
from the ND-500 into ND-100 memory, i.e. what RIOM/WIOM bottom out in (consistent with
`ND500-BUS-INTERFACE-REFERENCE.md` §10.2). **Still open:** the exact classic-generation microword
sequence (needs a 5200/5400 control-store listing; B30 is octobus-generation).

---

## D. Process/method items (not carve targets)

### D1. Failure attribution in a dirty tree

The RetroCore working tree carries ~50 modified files from other sessions, so "pre-existing failure"
cannot be established from it. This track asserted "4 pre-existing SHR failures unrelated" in a commit
message; that was inference. What is defensible: the SHR tests live in
`TestND500_InstructionBugValidation.cs`, untouched here, and the only cross-cutting edit was one JSON
entry.

### D2. The boot harness is flaky

`Nd500_D4_RunDomain_RealCpu_Capture` both passes and dies with "Test host process crashed" plus an
empty progress log for identical code. Never attribute a failure to a change from one run. A crash
with no exception text is usually an uncatchable `StackOverflowException` on the CPU thread, and the
CPU trace file is useless after a crash because it only flushes on close.

---

## Priority for the microcode track

1. **A1** - who builds PCB/PST. Unblocks the whole MMU model.
2. **A2 / A3** - the segment number and PSTP. Small answers, immediate effect.
3. **B1** - the swapper transport (their Q4 as well, so it pays twice).
4. **C1** - the 21B dispatch contradiction.
5. **C2** - RIOM REGISTER mode; cheap, and the location is already pinned.

---
---

# SECTION 2 — OCTOBUS / ACCP TRACK (added 2026-07-20)

**From:** the ND-100 ↔ ND-5000 **octobus transport** track (`OctobusND5000Station.cs`,
`NDBusOctobus.cs`, the SINTRAN-over-octobus boot harness).
Section 1 above is the 3022/swapper track's and is left untouched.

Same answer-route vocabulary as section 1, plus `LIVE` = boot-harness experiment,
`UNANSWERABLE` = needs an artifact we do not possess (e.g. the undumped ACCP EPROM).

## 2.0 First — four things CLOSED today, so nobody re-opens them

These were live questions in this corpus this morning. All four are now settled with evidence, and
three of them were **my own wrong claims**. Recording the retractions is the point of this register.

| Was believed | Status | Evidence |
|---|---|---|
| The swapper is control-store MICROCODE and must be modelled in C# | **RETRACTED [V]** | It is ordinary ND-500 macrocode, `SWAPPER-K01.PSEG` (38,161 B) at phys `0x06F800`, byte-identical incl. the `REV`/`-K01` strings. `ND500-D4-RUN-BLOCKER-FINDING-2026-07-19.md` §12c/§12d |
| `LDSWA` branches on CPU type (classic → 14B/21B/23B; 5000 → control store) | **DISPROVEN [V]** | `LDSWA` (143551-143621) has NO CPU-type test; its only descriptor test is bit 3 of `mem[mem[B-57]-22]` = the "swapper already loaded" done-bit. `> Loading Control Store` and `> Loading Swapper` are steps **0 and 3 of one state machine** `500IN`@075150 (done-mask complete `0o217`). The real discriminator `(mem[mem[B-56]+27] & 7)==3` is used 20+ times in seg 030 but **not** on either load path |
| Level-12 polling a `-1` slot at `033620` is "the signal nothing fills" | **RED HERRING [V]** | Instruction is `LDX I 114` (**indirect**, P-relative literal pool), not `LDX ,B 114`; `033620` is in segment `017-S3SMPIT` (generic level-12 MPIT entry), not the resident and not the ND-500 driver; the cell is `IL12Q`=`0o007265`, the **disk driver's** software queue head, where `-1` is normal |
| The swapper image arrives via ACCP/DMA transport | **WRONG [V]** | `LDSWA`→`PLSWA`(144212)→`144002` (`MON 50`, `GFMAD`/`GFDEV`/`GFSEC`) → loop `143647` → `144117` → **`MON 131` ABSTR** = ordinary **disk-controller DMA into ND-100 physical memory**, destination page from **`MON 61` FIXC5**. Whole path 143600-144400 contains only `BFILL`/`MON 50`/`MON 131`/2× 2-word `MOVEW`/`MON 43` — **no IOX, no window store loop, no mailbox, no ACCP, no SAMSON branch**. This CLOSES section 1's **B1** and my own Q4 |

**Also correcting my own citation of section 1's evidence:** I wrote in my handoff that the 3022
track "observes SINTRAN sending 21B then 23B, and it works there". Section 1 **C1** is right and I
was wrong — they verified SINTRAN *sends* it; the thing *accepting* it is their own servicer. There
is no hardware corroboration, and nothing contradicts `20,21 → MSG_ILLEG`. See **Q-MBX-09**.

## 2.1 The real ND-500(0) timeout gate [V, carved 2026-07-20]

Since the level-12/`IL12Q` reading was a red herring, here is what actually produces the message.
`N500TMR`, the ND-500 watchdog (`RP-P2-N500.NPL:300-341` @127642-127660):

```
127646   CALL RN5STATUS                     % read the ND-500 status word
127647   IF A >< ANSWER THEN                % status is NOT "ANSWER"
127652      T:=5MBBANK; X:=MAILINK; *AAX X5BRK; LDATX
127656      IF A >< 0 GO 5TMRA              % microprogram-break set -> just re-arm
127660  N5ABORT:  N5TIMOUT                  % <-- "ND-500(0) timeout"  (N5TIMOUT = 2000B)
```

plus `MP-P2-N500.NPL:362/:545`: `IF CPUAVAILABLE NBIT 5ALIVE GO FAR EN5TIMOUT`.

**Three gates, all of which must hold:** (a) the 5MPM message status word becomes `ANSWER`;
(b) `5ALIVE` is set in `CPUAVAILABLE`; (c) a level-12 **hardware** interrupt whose `IDENT PL12`
returns the ND-500 interface ident so `ITB12` (`0o153563`) yields a non-zero datafield
(seg `017-S3SMPIT` @033643-033653).

## 2.2 Cross-corpus open-question inventory

A sweep of 238 markdown files across `SINTRAN\ND500\**`, `SINTRAN\ND5000\**` and
`SINTRAN\Devices\Octobus\**` for `[?]`/`[OPEN]`/`[TC]`/`[UNVERIFIED]`/`[INFER]`/`[ASSUMPTION]`,
unchecked task boxes, and open-question prose. Deduplicated across revisions, newest phrasing
preferred, corpus-closed and retracted items excluded.

Items marked **§1** duplicate a section-1 entry and are cross-referenced rather than restated.

### MAILBOX / MESSAGE

| ID | Question | Source | Route | Blocking |
|---|---|---|---|---|
| Q-MBX-01 | Microcode semantics of MICFU `3MONO=34B` and `33MON=46B`? | `ND500-MAILBOX-MESSAGE-CATALOG.md:66,68,177` | MICROCODE | catalog: "carve before coding" |
| Q-MBX-02 | Do answer-result fields exist at message offsets 40B-47B, or is that assumption void? | `…CATALOG.md:178-181` | CARVE | **Yes** — corrects the RetroCore STATUS answer-block test plan |
| Q-MBX-03 | Is stop reason `TPSTRA=65` real? | `…CATALOG.md:182` | MICROCODE | No |
| Q-MBX-04 | Exact microcode step sequence per MICFU (currently DERIVED only) | `…CATALOG.md:183-184` | MICROCODE / MANUAL ND-05.012.01 §13 | No |
| Q-MBX-05 | Names/semantics of ~24 unnamed `MSW*` swapper fn codes, generic sub-code `66B`, handler index 23 | `…CATALOG.md:185-187,152` | CARVE | No |
| Q-MBX-06 | Numeric MICFU values of `3RMED` / `3WMEP`? | `…CATALOG.md:70,188` | CARVE | No |
| Q-MBX-07 | The swapper's physical WRITE-back primitive — only `RPHS` (read) exists, no `WPHS` | `…CATALOG.md:189` | CARVE | No |
| Q-MBX-08 | Is `MCHANDEL`/`MCHANDLE`/`5MONICO` one object? Two docs disagree | `…CATALOG.md:190`; `ND500-STATUS-AND-INDEX.md:712` | CARVE | No |
| **Q-MBX-09** | **Does the B30 image ever ACCEPT MICFU 21B (3WREG), or is `20,21 → MSG_ILLEG` the truth?** | §1 C1; `SWAPPER-START-MECHANISM-CARVE…:137`; `ND500-SERVICER-IMPLEMENTATION-REFERENCE…:246` | **MICROCODE** | **Yes** — §1 priority #4; decides register-image vs context-block swapper start |
| Q-MBX-10 | `3RMICV` answer halfword offsets, and where the CPU-parameter value comes from | `ND500-MICROCODE-INTEGRATION-ARCHITECTURE…:706` | MICROCODE | Yes — blocks S1(b) full assert |
| Q-MBX-11 | Classic (non-5800) MICFU `05`/`27B` (`3SWMESS`/`3FITRNSF`) semantics — ILLEG on 5800 [V] | `…ARCHITECTURE…:551,707` | MICROCODE (classic image) | Yes — blocks Classic table completion |
| Q-MBX-12 | How does a classic (no-OCB) machine signal CPU-unavailable? | `…ARCHITECTURE…:709` | MICROCODE / MANUAL | Yes — blocks the 3START no-CPU answer |
| Q-MBX-13 | Where does the classic 144-bit CS image carry its VERSION word (10509=0x290D / 10609=0x2971)? | `S0-SERVICER-REVIEW…:47-49` | UNANSWERABLE until Bo Göran's floppy arrives | Yes — servicer needs `MicrocodeVersion` for the classic VERSION answer |
| Q-MBX-14 | Is `MP-P2-N500.md` §7.6's answer-offset table usable? It CONFLICTS with the symbol layout | `…CATALOG.md:191` | CARVE | Yes — "do not implement from it" |
| Q-MBX-15 | Where is the `LAST-N500-MSG` ring (record size, capacity, head/tail)? Not in the 5MPM window | `CARVE-ANSWER-3022-FLAG-POLL-RING…:200,210,233,246`; `ND500-F6-MESSAGE-RING-RAW-CARVE-SEED…` | CARVE (`LIST-TABLE` inside `ND-500-MON:PROG`) | Yes — "the strongest cross-check" for the domain ladder |
| Q-MBX-16 | What does SINTRAN wait for before the first 13B (reading `$0` repeatedly)? | `…CATALOG.md:323`; `ND500-CS-LOAD-TRACE-FINDINGS…:97-99` | LIVE | No — it proceeds anyway |

### ACTIVATION / DOORBELL

| ID | Question | Source | Route | Blocking |
|---|---|---|---|---|
| Q-ACT-01 | Is TERM5's second level-12 pulse a third interrupt-latch point? | `ND500-INTERRUPT-DECISION-PLAN…:191` | CARVE / MICROCODE | Yes — the S1 harness bakes in "two interrupts per classic message" |
| Q-ACT-02 | Which of window offsets `0x108`/`0x1010`/`0x8800` are header vs ext-block vs `X5FIF` vs `MESSBUFF`? | `CARVE-ANSWER-OCTOBUS-MAILBOX-ACTIVATION…:286` | LIVE | No — self-discovery sidesteps it |
| **Q-ACT-03** | **On SAMSON, is `3RMICV`/watchdog issued BEFORE the octobus CS-load or only after microstart? If before, who answers it?** | `CARVE-ANSWER-OCTOBUS-MAILBOX-ACTIVATION…:289-293` | CARVE | **Yes** — drives whether a pre-CS-load answerer must exist; bears directly on §2.1 gate (a) |
| Q-ACT-04 | Is `SAMSON_CPU` 0- or 1-based in B30 vs SINTRAN's block slot? | `…ACTIVATION…:294` | MICROCODE | No |
| Q-ACT-05 | Does `ADRZE=000060` resolve off `N500DF` (`N500D=051767B`)? Flat resident address of ADRZERO? | `…ACTIVATION…:330,360` | CARVE then LIVE | No |
| Q-ACT-06 | What kick-word encodings does the microcode SEND (`SENKICK` builds `100102\|cpu`)? | `OCTOBUS-ND100-ND5000-REFERENCE.md:319` | MICROCODE | No |
| Q-ACT-07 | How is the octobus datafield B-pointer established at interrupt entry? | `OCTOBUS-ND100-ND5000-REFERENCE.md:320` | CARVE / LIVE | No |
| Q-ACT-08 | Is the `ITB13` slot→ident offset really "ident N at ITB13+N−1"? Why does slot 37B hold ident-40B? | `OCTOBUS-ND100-ND5000-REFERENCE.md:316` | LIVE | No |

### CS-LOAD / ACCP

| ID | Question | Source | Route | Blocking |
|---|---|---|---|---|
| Q-CSL-01 | Does SINTRAN-L use `LOCSD` or `LOCSM`, and their exact octal codes? | `CARVE-ANSWER-OCTOBUS-ACCP-SELFTEST-CSLOAD…:91,199` | LIVE | No |
| **Q-CSL-02** | **Units of the ACCP parameter pointer (byte / 16-bit word / MAR-scaled), and exactly what SINTRAN compares on the VPARP return** | `CARVE-ANSWER-OCTOBUS-CSLOAD-VERSION-CHECK…:143,156,192-200` | LIVE | **Yes — "the prime suspect" for the CS-load stall** |
| Q-CSL-03 | Why did pointer `0x18000` pass while `0x800` fails — clobbering or scaling? | `…VERSION-CHECK…:198-199` | LIVE | Yes — same stall |
| Q-CSL-04 | Are both suspected bugs live, or only the OCB fault? | `…VERSION-CHECK…:200-203` | LIVE | Yes — decides which fix |
| Q-CSL-05 | What discriminator does J04's OMD-4 receiver use for Messack vs Messnak? | `CARVE-ANSWER-OCTOBUS-CSLOAD-MICROSTART…:229` | CARVE / LIVE | No |
| Q-CSL-06 | Exact leading Messack byte value; is the selftest reply consumed by `5OMBREAD`? | `…SELFTEST-CSLOAD…:200-202` | CARVE | No |
| Q-CSL-07 | Where in J04 are the selftest-poll loop and the error strings? | `…SELFTEST-CSLOAD…:61,108,195-198` | CARVE | No |
| Q-CSL-08 | Which path prints "Wrong microprogram", and which emulator path emits FaultType 202/203? | `CARVE-ANSWER-OCTOBUS-WRONG-MICROPROGRAM…:41,168-176` | LIVE + code read | Partly |
| Q-CSL-09 | Which of `202B` (NotFatal) vs `203B` (Fatal) does the microcode pick per mismatch? | `…WRONG-MICROPROGRAM…:185-188` | MICROCODE | No |
| Q-CSL-10 | Is `CPUPAR=001741B` for model 8 exactly what SINTRAN caches? `VERSIONxx`→model map beyond 8? | `…WRONG-MICROPROGRAM…:180-184` | LIVE | No |
| Q-CSL-11 | Is the supplied `(SYSTEM)CONTROL-STORE:DATA` the CORRECT microcode for the target CPU? | `nd-500-mon/ND500-BRINGUP-BUS-INTERFACE-FEEDBACK.md:174-176` | LIVE (`COMPARE-CONTROL-STORE`) | Yes — a wrong image explains the whole "wrong microprogram" family |
| Q-CSL-12 | Exact byte layout of the ACCP EPROM (68000 firmware) | `HANDOFF-OCTOBUS-EMULATION.md:124` | **UNANSWERABLE** — never dumped (confirmed by Ronny 2026-07-20) | Would unlock full ACCP emulation |
| Q-CSL-13 | Does the ACCP really ignore all but OMD-3 multibyte in test/init mode? | `OCTOBUS-PLAN-CRITICAL-REVIEW-2026-07-20.md:341-344` | LIVE | No |
| Q-CSL-14 | Byte sub-offsets of regions 25/23/24/26 within the ACCP parameter block; how `X5OCT=0x018000` reconciles | `ACCP-REGION25-ADDRESS-HANDOFF-CARVE…:161,175,178`; `DUCS-READBACK-REGION-OWNERSHIP-CARVE…:133,141,173` | CARVE | Yes — see §2.4 note |
| Q-CSL-15 | Where is the descriptor table `P = control_block[21]` physically? Who writes slots 23-26? Do the words carry flag/length bits? | `LDDTX-REGION-RESOLUTION-CARVE…:133,150-155` | LIVE | Yes |
| Q-CSL-16 | `CNVBYADR`'s exact scale factor/body (`055034B`); runtime absolute `5FPACCPBUF` ⇒ `X5ACC`/`X5OCT` | `XMSINIT-BUFFER-GEOMETRY-CARVE…:206,211-212` | CARVE | Feeds Q-CSL-02 |

### SWAPPER  *(Q-SWP-01 CLOSED today — see §2.0)*

| ID | Question | Source | Route | Blocking |
|---|---|---|---|---|
| ~~Q-SWP-01~~ | ~~What delivers the swapper PSEG/DSEG?~~ | — | **CLOSED §2.0**: `MON 131` ABSTR disk DMA, page from `MON 61` FIXC5 | — |
| Q-SWP-02 | What is the descriptor SINTRAN DMAs to `0x6F000` (`02 C0` at +3)? | §1 B2 | CARVE | Yes — may answer Q-MMU-01/Q-SWP-03 |
| **Q-SWP-03** | **Where does the program SEGMENT number come from at 3START?** (21B sends `P=0x04`, no segment bits; runs at `0x08000004`) | §1 A2 | MICROCODE | **Yes — §1 priority #2** |
| **Q-SWP-04** | **21B register word order: high halfword first or low?** (`reg[18]=PS` is `0x48480003` or `0x00034848`) | `QUESTIONS-FOR-ND5000-MICROCODE-SWAPPER-START…:111-123` | MICROCODE | **Yes — "our biggest single unknown"** |
| Q-SWP-05 | Is `3WREG N == LREGBL N+1` the order the microcode uses? | `…SWAPPER-START…:96-109` | MICROCODE | Yes — blocks full context fidelity |
| Q-SWP-06 | What does the microcode do with PS between 3START and the first instruction? | `…SWAPPER-START…:125-136` | MICROCODE | Yes — feeds Q-MMU-04 |
| Q-SWP-07 | Is the swapper started with a register image at all, or a full context block (`LCNTXT`/`NEWCNTXT`)? | `…SWAPPER-START…:146-151` | MICROCODE | Yes — if context-block, the register-image model is wholly wrong |
| Q-SWP-08 | Why is `SWPINFO` empty at the RIOM (null deref `r2.(10)`, `r2=0`, `PC=0x0800913B`)? | §1 A5 | LIVE — capture `0x420E30` **at** the RIOM, not after RUN | Yes — the current D4 stop |
| Q-SWP-09 | Is the swapper's message buffer PRIVATE or SHARED ND-100 memory? | §1 C4 | CARVE (`SWMSG` alloc in `MSINIT`) | Yes |
| Q-SWP-10 | Is `200B` in the swapper's own error-code namespace? Is there a swapper error-code list? | §1 C6 | CARVE | No |
| Q-SWP-11 | What do the `LDSWA`/`RUNSW` bytes actually contain? *(scoped down — the CPU-type-branch framing was disproven, §2.0)* | `SWAPPER-START-MECHANISM-CARVE…:66,134,140-142` | CARVE | Partly — the (A)/(B) seam |
| Q-SWP-12 | Precise `55MESSIZE` and the classic 21B register-image layout | `…MECHANISM-CARVE…:144-146` | CARVE | Yes — for a byte-accurate mechanism-(A) start |
| Q-SWP-13 | Do RUN / LIST-ACTIVE-SEGMENTS read Table A, or a mirrored ND-100 `S500S` array? | `…MECHANISM-CARVE…:117,138` | CARVE / LIVE | Yes |
| Q-SWP-14 | Which MON service does each `$1000225xxx` PSEG descriptor select? (~151 workers) | `swapper/swapper-k01.pseg.md:332-341` | CARVE (mechanical) | No |
| Q-SWP-15 | Are the local `MON 377B` wrappers fast-path/validation code? | `swapper-k01.pseg.md:343-345` | CARVE | No |
| Q-SWP-16 | Meaning of small DSEG values 1,2,4,5,6 | `swapper-k01.dseg.md:543` | CARVE | No |
| Q-SWP-17 | Full runtime set of swapper MICFU/status codes (only `SWPFATAL 2047B` is static) | `old/SWAPPER-K01-ANALYSIS.md:448` | CARVE / LIVE | No |

### MMU / MEMORY

| ID | Question | Source | Route | Blocking |
|---|---|---|---|---|
| **Q-MMU-01** | **Who builds the PCB (32 prog + 32 data caps) and the PST for the swapper, and when?** An 8 MB scan for a PSTE naming either DMA'd page table found **0 candidates** | §1 A1; `…SWAPPER-START…:158-174` | MICROCODE (fallback CARVE of the swapper's own startup) | **Yes — the biggest. §1 priority #1** |
| **Q-MMU-02** | **Where does `PSTP` come from?** The 21B block has no PST-base entry | §1 A3 | MICROCODE | **Yes — "the single missing anchor"** |
| Q-MMU-03 | Is the MMU enabled at swapper start, or does the swapper enable it itself? | §1 A4 | MICROCODE / CARVE | Yes |
| Q-MMU-04 | Does the microcode perform the documented walk exactly (`PS→PST[PS]→PCB→cap→psn→PST[psn]→PT→PTE`, 256 B/domain)? | `…SWAPPER-START…:176-184` | MICROCODE | Yes — the whole emulated MMU rests on it |
| Q-MMU-05 | Does the microcode really test `PFN==0` as "not present"? Is the PSTE 4 B with bits 1-0 = index mode? | `…SWAPPER-START…:186-192` | MICROCODE | Yes |
| Q-MMU-06 | Correct capability set for the swapper at start; is segment 13 (`0x68000044`) meaningful? | `…SWAPPER-START…:194-212` | MICROCODE | Yes — current all-64-caps-same-tables is a declared hack |
| Q-MMU-07 | Are the two DMA'd page tables (`0x6E800`, `0x6E000`) how hardware learns the layout, or incidental? | `…SWAPPER-START…:68-80` | MICROCODE | Yes — the whole segment derivation rests on it |
| Q-MMU-08 | Segment-capability bit layout: claim A (bit13=S,12=P,11=W, 11-bit seg) or claim B (W=15,P=14,S=13, 12-bit)? | `ND500-EVIDENCE-AND-CONTRADICTIONS.md:1078-1090` | MANUAL ND-05.009.4 | Yes — contradiction C9 |
| Q-MMU-09 | Is the segment-31 indirect bit a memory PCB entry or hardwired 37B? | `ND500-SERVICER-IMPLEMENTATION-REFERENCE…:246` | MICROCODE | Yes |
| Q-MMU-10 | Port-control-register bit-6 polarity — manual contradicts itself | `…CONTRADICTIONS.md:838-840` | MANUAL | Yes |
| Q-MMU-11 | RIOM's real ND-100 address width (both emulators use a 24-bit ceiling as convention) | §1 C3 | MICROCODE / MANUAL | No — explicitly low value |
| Q-MMU-12 | Is REGISTER a legal addressing mode for RIOM operand 2? | §1 C2 | MICROCODE — location pinned: mode dispatch CS `001600-001716`, RIOM stub CS `000745` | Yes — §1 priority #5, cheap |
| Q-MMU-13 | `HDEV`'s real runtime value / true interface device number (claims span 1560B/100-120B/500B/600B) | `ND500-BUS-OCTOBUS-HW-INTERFACE.md:293-294` | CARVE / LIVE | No — treat 660 as observed |

### TRAPS

| ID | Question | Source | Route | Blocking |
|---|---|---|---|---|
| Q-TRP-01 | What exactly does the microcode place in a trap report — which word is access type, which the failing logical address, how is the segment encoded? | §1 C5 | MICROCODE | Yes — our trap fields are demonstrably wrong |
| Q-TRP-02 | The full ND-500 trap-code / stop-reason table (`TRAPDECODER`, `SPCTRAP` set) | `ND500-BUS-OCTOBUS-HW-INTERFACE.md:302-305` | CARVE | No — partial table ships by design |
| Q-TRP-03 | Is the fault-address slot at message offset `0o17` correct? (marked `[D]`) | `…SERVICER-IMPLEMENTATION-REFERENCE…:246` | CARVE / MICROCODE (`TRAP_GEN3`) | Yes |

### DOMAIN / PLACE-RUN

| ID | Question | Source | Route | Blocking |
|---|---|---|---|---|
| **Q-DOM-01** | **The FUNCS/5IFUNC subfunction NUMBERS and parameter-block layouts for PLACE-DOMAIN / RECOVER-DOMAIN** | `DOMAIN-HANDLING-ARCHITECT-BRIEFING…:51,126,140` | CARVE | **Yes** — briefing forbids implementing as fact; gates the D4 `NLL:` assert |
| Q-DOM-02 | How does the loader do the cross-user copy (`IN-NLL-XX-H02:PROG` → DOMAIN-USER)? | `…BRIEFING…:66` | CARVE | Yes — "do not model it yet" |
| Q-DOM-03 | Exact sequence command → interface kick → MON round trip for phases D/E (RUN, EXIT, Escape, CONTINUE) on both interfaces | `…BRIEFING…:70,83,86-87` | CARVE | Yes — phases D/E are all `[TC]` |
| Q-DOM-04 | Is octobus suspend/resume really `STOPMIC 034B` / `CONTMIC 035B` / `RESTMIC 036B`? | `DOMAIN-HANDLING-TWO-INTERFACE-EXPECTATION-TABLES…:106-107` | LIVE (never seen post-VPARP on the wire) | Yes — phase E model |
| Q-DOM-05 | Where is the standard-domain table (address + format)? | `…EXPECTATION-TABLES…:71` | CARVE | No |
| Q-DOM-06 | Where is the exact branch deciding "runnable state" / printing message #14? | `ND500-D4-RUN-BLOCKER-FINDING…:210-216` | LIVE (BP @ `030624`) | Yes |
| Q-DOM-07 | Segment-table stride and field encodings that RUN checks | `…D4…:322,382` | LIVE | Yes |
| Q-DOM-08 | Where is `N500M`'s executable body? Is it data reached via `CALLP`/`MCTAB[N]`, or an unidentified overlay? | `ND500-STATUS-AND-INDEX.md:1082-1094` | CARVE (trace `CALLP` @ `032201B`) | **Yes** — do not extract subfunctions until proven |
| Q-DOM-09 | Where is J04's *outer* dispatch table (command names → handlers/param counts)? | `nd-500-mon/nd-500-mon-j04.prog.md:1313,1440` | CARVE / LIVE | Yes |
| Q-DOM-10 | Identity of `004017B` (`0x080F`) at `146305` | `…j04.prog.md:523,900,949` | MANUAL / CARVE | No |
| Q-DOM-11 | Subfunctions `144B`, `150B-156B`, `160B-177B`, `036B`, `042B`, `046B`, `107B` — in J04 but absent from the table | `…j04.prog.md` §13 Q2 | CARVE | No |
| Q-DOM-12 | Are the 22 caller-less MON-60 thunks dead or dynamically dispatched? Why duplicate thunks? | `…j04.prog.md` §13 Q5,Q6 | LIVE | No |
| Q-DOM-13 | Are ERRFL (022B) return values plain codes or pointers? | `mon60-callers/022B-ERRFL/README.md:49` | CARVE | No |
| Q-DOM-14 | Can the S3SM5 MON routing table be enumerated? (53% undecodable) | `MON/ND500-MON-CALL-ROUTING-MAP.md` | CARVE | Yes — blocks the MON-60 routing model |

### OCTOBUS TRANSPORT

| ID | Question | Source | Route | Blocking |
|---|---|---|---|---|
| Q-OCT-01 | The six broadcast-type (BT) code assignments — named everywhere, enumerated nowhere | `OCTOBUS-ND100-ND5000-REFERENCE.md:318` | MANUAL (spec never found) / LIVE | No |
| Q-OCT-02 | Which emergency-code family does the ACCP decoder match — `0xA1` (241B) or `0xF1` (361B)? | `OCTOBUS-PLAN-CRITICAL-REVIEW-2026-07-20.md:351-362` | LIVE — "one byte closes it" | No |
| Q-OCT-03 | Station-side meaning of test-protocol register fns 0/2/6 (read), 3/5/7 (write) | `OCTOBUS-DEVICE-CONTROLLERS…PLAN…:1075-1079` | LIVE | No |
| Q-OCT-04 | Real Get-Domino-Information field values (currently placeholders) | `…PLAN…:1080-1082` | LIVE | No |
| Q-OCT-05 | What runs on octobus interrupt levels 10/11? `OCSTART` wires them; no consumer found | `…PLAN…:1085-1086` | CARVE | No |
| Q-OCT-06 | Is broadcast-multibyte legal? Evidence is one column-garbled figure | `…CRITICAL-REVIEW…:327-329` | MANUAL / CARVE | No |
| Q-OCT-07 | Receive-side status-register bits beyond bit 3 | `OCTOBUS-ND100-ND5000-REFERENCE.md:319` | MANUAL / LIVE | No |
| Q-OCT-08 | Power-up info message: all zeros or `376B`? Two figures disagree | `…CRITICAL-REVIEW…:364-368` | MANUAL / LIVE | No |
| Q-OCT-09 | Octobus priority counter — 4 bits or 5? | `…CRITICAL-REVIEW…:331-336` | MANUAL | No |
| Q-OCT-10 | The ND-100 octobus card's real PCB/part numbers | `…CRITICAL-REVIEW…:319-321,391-393` | MANUAL | No |
| Q-OCT-11 | DIOC OBCON receive-FIFO depth (the 16×16 figure is CPU-side only) | `…CRITICAL-REVIEW…:315-319` | LIVE (TPE test 3) | No |
| Q-OCT-12 | Does `SOCTO` zero-fill the odd byte on an odd-length body? | `…CRITICAL-REVIEW…:196-199` | CARVE | No |
| **Q-OCT-13** | **SINTRAN's runtime OMD allocation model beyond 0/3/4/`5OMDNO`** — `CONOMD` body @`040062` is uncarved | `…PLAN…:807,1071-1072`; `…CRITICAL-REVIEW…:245-248` | CARVE (else LIVE single-step `XX5CONOMD`) | **Yes — "a harness that hardcodes an assumed `5OMDNO` tests itself"** |
| **Q-OCT-14** | **Why does carved L07 MFACK read `LMFIELD` word 3 (the received BYTE COUNT, not source station), range-test it 2..6, and use it as `MOCTSTATION`?** | `…CRITICAL-REVIEW…:180-188` | CARVE, else LIVE | **Yes — until resolved the C6 ack oracle is `[OPEN]`** |
| Q-OCT-15 | Multi-pending semantics when a station sends >1 MF error record before `5OMBREAD` drains | `…CRITICAL-REVIEW…:237-240` | CARVE | Relevant to phase C |

### DEVICE CONTROLLERS (DIOC / DOMINO / SCSI)

| ID | Question | Source | Route | Blocking |
|---|---|---|---|---|
| **Q-DEV-01** | **Who creates the DIOC port descriptor, writes `PDF.DRPRT`/`DOMDF.DLPRT`, and how does DDS-DEVICES:CNFG bind station + SCSI unit + LUN?** | `…CRITICAL-REVIEW…:203-207,232-235` | CARVE (NKSE segment-105 interior) | **Yes — BLOCKING for real provisioning** |
| **Q-DEV-02** | **What initializes `DOMDF` (`DSVER`, `DOMDF+21`, header words)?** Most plausible carrier of the SCSI unit/LUN binding | `…CRITICAL-REVIEW…:208-212` | CARVE | **Yes — which SCSIHDD a request targets is currently unknowable** |
| Q-DEV-03 | Where does `DMSID`/`HOMEPORT` come from? Is the reply route `sendref=0 → HOMEPORT==DLPRT` right? (`[I]`, not `[V]`) | `…CRITICAL-REVIEW…:213-216` | CARVE | Yes |
| Q-DEV-04 | The semantic space of DOMINO status codes beyond the happy path | `…CRITICAL-REVIEW…:217-222` | MANUAL ND-814009 | Yes — faithful error injection impossible without it |
| Q-DEV-05 | How does a 68020 DIOC's BADAP DMA interpret `DCNVA` bit31? | `…CRITICAL-REVIEW…:223-226` | UNANSWERABLE without DIOC firmware | No — must stay flagged in code |
| Q-DEV-06 | Controller-side NUCLEUS library's real behaviour (TSET value, INQUEUE honor) | `…CRITICAL-REVIEW…:227-229` | UNANSWERABLE without controller firmware | No |
| Q-DEV-07 | Owner-ID convention for a controller-side actor (`CLUST` is ND-500-specific) | `…CRITICAL-REVIEW…:225-226` | CARVE | No |
| Q-DEV-08 | Byte-level request/reply formats for non-BDIO DOMINO device classes | `…PLAN…:466` | CARVE | No |
| Q-DEV-09 | PROMAN/OPCOM boot-protocol BYTES and the controller firmware images | `…PLAN…:895-899,1073-1074` | UNANSWERABLE from repo | No |
| Q-DEV-10 | Exact `DRPRT`/`DLPRT` sub-offset inside the port descriptor | `…PLAN…:938` | CARVE | Yes |
| Q-DEV-11 | Does `ENKIC=047526` resolve in any overlay? | `ND500-STATUS-AND-INDEX.md:176` | CARVE | No |
| Q-DEV-12 | Does a live SCSI DIOC station stay silent through boot (zero unsolicited frames to 10B-13B)? | `…PLAN…:854,991` | LIVE | Yes — S1-2/S4-2 gates |
| Q-DEV-13 | Mirror-pool `DSQCN` semantics / multi-outstanding-request behaviour | `SCSI-DIOC-OCTOBUS-EMULATION-PLAN…:283` | CARVE | No — deferred |

### OTHER

| ID | Question | Source | Route | Blocking |
|---|---|---|---|---|
| Q-OTH-01 | `jumpg` entry stride/format and div4 result placement | `CARVE-ANSWER-LINKER-LOAD-ERROR52-V10.md:69,99` | LIVE | No — moot for the fix |
| Q-OTH-02 | Contents of the `r.0x18` descriptor at `B003D0E2` | `…ERROR52-V5.md:47,106` | LIVE | No — superseded |
| Q-OTH-03 | Contents of floppy `211305` | `ND500-STATUS-AND-INDEX.md` §7 Q1 | UNANSWERABLE — media absent | No |
| Q-OTH-04 | Where is the CLASSIC ND-500 microcode image? Not found on `F:\ND` | `ND500-STATUS-AND-INDEX.md` §7 Q7 | UNANSWERABLE — media absent | Yes for all classic-generation work (Q-MBX-11, Q-MBX-13) |
| Q-OTH-05 | The classic (Control II / 5015) physical path from ND-500 into ND-100 memory for RIOM | §1 C7 | **PARTIALLY ANSWERED 2026-07-20** (MANUAL): microcode drives TAG-OUT 6/7 + 3022 MAR via unit-select codes — see C7 and `NDIX-KERNEL-INTERFACE-EVIDENCE-2026-07-20.md` §6; exact classic microword sequence still open | Yes — that is the generation the live case uses |
| Q-OTH-06 | `CNVWADR`/`CNVBYADR` algorithm (body not in NPL; lives in the ND-500 assembler include) | `ND500-BUS-OCTOBUS-HW-INTERFACE.md:295-297` | MICROCODE / MANUAL | See Q-CSL-16 |
| Q-OTH-07 | Meaning of the dual symbol aliases per 3022 register offset; `RCON5`/`RSTA5` write control bits beyond observed | `…HW-INTERFACE.md:298-301` | MANUAL / LIVE | No |
| Q-OTH-08 | Precise MAR → ND-100 physical address arithmetic | `ND500-BRINGUP-BUS-INTERFACE-FEEDBACK.md:177` | LIVE / MICROCODE | Possibly superseded by later RIOM fixes |
| Q-OTH-09 | Is `X500D=177745` real? `EXQUE` (srf `0o2020`) `SARG-2460B` displacement decode? | `…ARCHITECTURE…:735-741` | CARVE | No |
| Q-OTH-10 | What does the ACCP firmware do with `LSYSPAR` words 2-3 if nonzero? SINTRAN always sends 0,0 | `CARVE-ANSWER-SYSPAR-LSYSPAR-DISAMBIGUATION.md:58` | UNANSWERABLE (needs the EPROM) | No |
| Q-OTH-11 | Was J04's `132170` meant to be patched at runtime? Is `B-175` a display or a static link? | `…j04.prog.md` §13 Q7,Q8 | LIVE / another build | No |
| Q-OTH-12 | Which page table does each ND-100 addressing mode use in J04? | `…j04.prog.md` §13 Q4 | MANUAL ND-06.014 | No |
| Q-PROC-01 | Are the 4 SHR failures pre-existing? Cannot be established from a tree with ~50 foreign modified files | §1 D1 | LIVE on a clean tree | Yes for release confidence |
| Q-PROC-02 | Why does `Nd500_D4_RunDomain_RealCpu_Capture` pass and crash for identical code? | §1 D2 | LIVE | Yes — never attribute a failure to a change from one run |

## 2.3 The ten that block the most (cross-track)

1. **Q-MMU-01** — who builds the PCB/PST. Unblocks Q-MMU-02/04/06/07 and the whole MMU model.
2. **Q-MMU-02** — where `PSTP` comes from. Small answer, immediate effect.
3. **Q-SWP-03** — the program segment number at 3START.
4. **Q-SWP-04** — 21B register word order; decides what PS *is*.
5. **Q-DOM-01** — PLACE-DOMAIN / RECOVER-DOMAIN subfunction numbers; gates the D4 `NLL:` assert.
6. **Q-SWP-08** — why SWPINFO is empty at the RIOM; the literal current D4 stop.
7. **Q-CSL-02** — ACCP parameter-pointer units; "the prime suspect" for the octobus CS-load stall.
8. **Q-OCT-13** — CONOMD's runtime `5OMDNO` allocation; a harness that hardcodes it tests itself.
9. **Q-DEV-01 + Q-DEV-02** — DIOC provisioning; which SCSIHDD a request targets is unknowable.
10. **Q-MBX-09** — the 21B dispatch contradiction.

## 2.4 Octobus-track additions not in the corpus

| ID | Question | Route | Note |
|---|---|---|---|
| ~~Q-OCT-16~~ | ~~What address space is `START_MESS = 0x2000` in?~~ | **ANSWERED 2026-07-20 — see §2.6** | — |
| ~~Q-OCT-17~~ | ~~How should an emulated station DERIVE the mailbox base?~~ | **ANSWERED 2026-07-20 — see §2.6** | — |
| Q-OCT-22 | **Which ND-100 code performs the CS page-0 patch?** The manual says system parameters are patched into the first page of `CONTROL-STORE:DATA` before the ACCP burns it, but the patcher is **not** in `NPL-SOURCE/NPL/*.NPL` and not in `MON-DEBUG:PROG` — it lives in an uncarved segment | CARVE | Yes — confirms *which* CS words are patched |
| Q-OCT-23 | What is the `CNVWADR` / `CNVBYADR` algorithm? (`CNVWADR=055160` L07; resident disassembly at that address is mid-routine.) Do **not** assume it is a plain `−ADRZERO` | CARVE | Feeds Q-CSL-16 |
| Q-OCT-18 | What do ACCP interrogation commands `2`, `1`, `3` (3-, 3- and 1-word replies) mean, and what is the reply byte layout? The CPU issues these during INIT_SAMSON before reaching IDLE | UNANSWERABLE from firmware (EPROM never dumped) — needs ND-05.020.01 ACCP chapter or a live capture | Marked `[OPEN]` in the microcode track's `BOOT-STARTUP.md` |
| Q-OCT-19 | What do `STAMIC0`/`CONTMIC`/`RESTMIC` do to the sequencer — start at CS `000000` (SAMSON cold vector), at `GOIDLE` (`000016`/`000017`, warm), at an ACCP-supplied address, or resume at the current MPC? | MICROCODE | Emulator currently sets a bool and ACKs |
| Q-OCT-20 | What are the ACCP frame trailer bytes `0x0B 0xD3` in the CPUMODEL report frame? (length/checksum candidates) | UNANSWERABLE / MANUAL | `BOOT-STARTUP.md` `[OPEN]` |
| Q-OCT-21 | The region-23 addend index question is CLOSED (`base + N·8`, see `XMSINIT-BUFFER-GEOMETRY-CARVE` correction 2026-07-20) — but **do regions 23 and 25 have genuinely independent bases**, or does `phys[P+23]` == `phys[P+25]` on this machine? The live trace only proves they resolve to the same field | CARVE / LIVE | Feeds Q-CSL-14/15 |

## 2.6 ANSWERED — how the ND-5000 legitimately learns the mailbox base (carve 2026-07-20)

This closes Q-OCT-16/17 and supersedes the "two incompatible models" contradiction.

**The address space [READ].** `START_MESS = 0x2000` is a **5MPM/MFbus window-relative BYTE address**,
the same space as the LPARP pointer. Proof: the only address the ND-100 hands the ACCP was observed
on the wire as `0x00018000` (`CARVE-ANSWER-OCTOBUS-CSLOAD-MICROSTART-2026-07-18.md:67`), and
`ND-05.020.01:4346` says *"The address of the parameter area **in the MFbus memory** is given."*
`0x18000` is far below ADRZERO (`004100B` → ND-100 physical `0x420000`), so MFbus offset 0 =
ADRZERO. `START_MESS` is therefore window byte `0x2000`.

The offset that makes this work is **`ADRZERO`**, set by the *operator* via
`DEFINE-MEMORY-CONFIGURATION` — `5P-P2-MON60.NPL:587` (`026733`): `5D12=:ADRZERO`; and
`RP-P2-N500.NPL:901` (`131763`): `A+ADRZERO=:5GBFPAGE`. **[NOT FOUND]** any evidence that CS
`OFFSET` (word `000020`), `MM,PSTP`/`MM,PUWP`, or an MPM port BASE register is added to
`START_MESS`.

**How the runtime value gets in [READ + INFER].** `ND-05.017.01 HARDWARE MAINTENANCE:3961`:

> "The next step in the control store loading procedure is to load **the first page of
> CONTROL-STORE:DATA** into the transmission buffer. **Some system parameters are patched into this
> first page.** Then this page is read by the ACCP and loaded into the ND-5000 control store memory."

CS words `000020`-`000027` are in that first page. `START_MESS` (word `026`) and `SAMSON_CPU`
(word `025`) are precisely the two constants that **cannot** be static in a shipped file —
`SAMSON_CPU` is per-CPU, and a multi-CPU ND-5000 loads the same file into every CPU. The `0o20000`
/ `0` seen in the dumps are shipped placeholders. *(That `START_MESS` specifically is among the
patched words is [INFER], strong — the patcher itself is uncarved, Q-OCT-22.)*

Corroboration [INFER]: `INZ500` allocates the ACCP and HW buffers **before** the mailbox
(`5P-P2-MON60.NPL:627-644`), and those live at `0x10000`/`0x18000`. A mailbox at a literal `0x2000`
would sit *below* buffers allocated earlier — inconsistent with `5GBUFF`'s ascending scan
(`RP-P2-N500.NPL:891-932`).

**No ACCP command carries the mailbox address [READ].** The full table
(`ND-05.017.01:11018-11065`, appendix A.3, codes 15B-65B) has exactly one address-bearing command,
`LPARPNT` (021B) — and that is the CS-transfer/octobus buffer (`X5ACCPBUF`), not the mailbox.
`LSYSPAR` (16B) exists *"to tell the microprogram **where to send octobus error messages**"*
(`ND-05.020.01:4276-4290`) — station/OMD numbers, not addresses. **Candidate (c) is REFUTED.**

**The 3 system-parameter words — CONFIRMED, none is an address [READ].** Built inline in
`CON5IDENT`, `MP-P2-N500.NPL:3617-3632` (`147155`-`147166`): word1 = `5OMDNO << 8`, word2 = 0,
word3 = 0.

### The emulator rule (this is the actionable output)

> **Derive `START_MESS` and `SAMSON_CPU` from the LOADED control store — words `000026` and
> `000025` of the image the ND-100 actually pushed over LOCSM/LCS0. Do not hardcode `0x2000`, and
> do not read the on-disk `CONTROL-STORE:DATA` (the on-disk copy still holds the placeholders).**

This satisfies the "no hardcoded values if it is configurable" rule properly: the configuration
genuinely *is* delivered to the CPU, in-band, via the control-store load — which an emulator that
models the control store already has in hand.

Ranked candidates, final:

| # | Candidate | Verdict |
|---|---|---|
| **(d)** | Fixed at `START_MESS` in the CS image, **patched by the ND-100 into CS page 0 during the load** | **SUPPORTED — this is the mechanism** |
| (b) | Self-discover from the `X5ACT` `0xFFFF→0x0000` transition | Valid **fallback / cross-check** only. Init `-1` at `RP-P2-N500.NPL:751-754`; cleared by `ACT51` (`MP-P2-N500.NPL:3027`). Real hardware never does this |
| (a) | Read SINTRAN's `5FPMAILBOX`/`5MBBANK` from ND-100 memory | **Unsupported as a hardware mechanism** — no path exists from the ND-5000 to SINTRAN's resident variables. Offline validation only |
| (c) | Receive it in an ACCP command payload | **REFUTED** — no such command |

**Useful in-band pointer [READ]:** `XMSINIT` writes the converted (ND-500-space) address of each
per-CPU extension block into the mailbox itself — `RP-P2-N500.NPL:747-752` (`131171`-`131200`),
landing in `X5NACTIVEQ` in the global header. That is a *consumer* of the base, not a source.

**L07 symbol values** (`SYMBOL-1-LIST` / `SYMBOL-2-LIST` / `N500-SYMBOLS`; note only 5-char
truncations exist): `N500D=051767` (abs) · `5FPAC=011252` (abs, matches the NPL decl address) ·
`5FPHW=011253` · `5FPMA=111102` [INFER abs] · `5MBBA=004654` [INFER abs] · `ADRZE=000060`
(N500DF-relative → abs `052047B` [INFER]) · `X500D=177745` (= −33 → abs `051734B`) ·
`MAILL=000021` / `MAILI=000022` (CPU-datafield displacements) · `5EXTD=000200` ·
ext-block words `X5BEX=0`, `X5NAC=2`, `X5ACT=5`, `X5ACC=20`, `X5OCT=22`, `X5HWB=24` ·
`5OMDN=000000` · `5STAT=000017`.

## 2.7 Silent assumptions in the RetroCore C# implementation

Audit of the octobus/3022/servicer sources. Full table in the session record; the ten most dangerous,
because each is a *silent* failure — nothing logs, nothing throws, the wrong answer just propagates.

| # | Site | The silent assumption | Why it is dangerous | Route |
|---|---|---|---|---|
| 1 | `NDBusND500IF.cs:1531-1551` vs `:1064-1092` vs `:2292` | **Three contradictory 32-bit word orders in ONE file** (low-first / MSB-first / high-first) | Not a missing citation — an internal contradiction. At most one is right; the others silently swap every 32-bit address and result | MICROCODE (see Q-SWP-04) |
| 2 | `ND100Memory.cs:266-285` | Only one MPM card is ever present; `FindMemoryBank` checks the 3022 first | Both cards default to `0x420000`, so with a 3022 **and** an octobus card the 3022 silently wins every access and the ND-5000 shares nothing | emulator |
| 3 | `OctobusND5000Station.cs:316-352, 1760-1791` | CS-load read-back must be self-consistent by construction | **The emulator computes and serves the very checksum SINTRAN verifies.** The gate can never fail, so control-store corruption is undetectable by construction | MICROCODE |
| 4 | `OctobusND5000Station.cs:727-748` | The **first** `0xFFFF→0` halfword anywhere in the window is X5ACT at ext-block word 5 | Pure heuristic, latched for the whole boot. Any unrelated `-1→0` cell mis-bases the mailbox silently | now superseded by §2.6 (d); keep as cross-check |
| 5 | `ND100Machine.ND5000.cs:155` | Global header sits at MPM window offset 0 | Contradicts `START_MESS`. **Resolved by §2.6** — read CS word `000026` | MICROCODE |
| 6 | `NDBusND500IF.cs:429-496, :414-416` | `ND500Operation`, `ND500StopReason`, `DisableTagInDecodingWhenLocked` matter | All three declared and **never read or written**. Every activate is the same operation, every stop reports reason 0, TAG-IN is never gated | CARVE |
| 7 | `NDBusND500IF.cs:1556-1567, :1506-1507` | Every completion is successful | `Error` and `DMAError` are declared and **never set** — failure is bit-identical to success on both the completion and DMA paths | CARVE |
| 8 | `NDBusOctobus.cs:1815-1819` | A SCSI station exists at wire 10 on every machine | **A synthetic station registered in the device constructor for test convenience**; any config probe reports a phantom controller | CARVE |
| 9 | `NDBusOctobus.cs:1031-1070, :1079-1219` | Control/transmit/receive register bit layouts | Invented power-of-two enums with no citation — in a file that already contains a DEBUNK of an identical invented enum at `:1236`. Station-presence discovery rests on them | MANUAL |
| 10 | `Nd500MicrocodeServicer.cs:378, 491` | 16B/17B are illegal on the 5800 | Inferred ("same register family, absent from the B30 table"), not proven. **This is exactly the inference class that produced the D4 post-CS-load 5ERANSWER blocker** — the fix changed the guess rather than pinning the fact | MICROCODE (Q-MBX-09) |

Runner-up: `NDBusOctobus.cs:1488-1489` — bring-up depends on two hand-tuned tick constants
(`INBOUND_LATENCY = 8`, `INBOUND_INTERVAL = 2`), self-declared as "not datasheet-exact". Any timing
change silently reintroduces "No answer from Octobus station N".

Also worth registering: `Nd500MicrocodeServicer.cs:690, 931` gives up on the X5SEM semaphore after
10,000 attempts and **proceeds unlocked** — deliberately breaking mutual exclusion under contention,
with only a log line. And `:797-804` (`AnnounceSwapperAlive`) fabricates a MON 377B stop record so
SINTRAN believes a swapper exists that never executed.

## 2.9 Two code claims VERIFIED against the source (2026-07-20)

Checked directly in RetroCore (no agent — account weekly limit hit), so both are `[V from code read]`.

**(a) The fabricated TAG protocol is GONE from the emulator.** `ND500-EMULATOR-DISCREPANCY-AUDIT.md`
and `ND500-BUS-INTERFACE-REFERENCE.md` §14 point 5 assert the C# still implements "fabricated TAG
callbacks that must go." **Stale.** Every hit in `NDBusND500IF.cs` for `MonitorCallRequest` /
`OperationComplete` / high-level TAG codes is a **comment documenting the deletion** (`:233`, `:728`,
`:1237-1239` "has been DELETED / removed here / carve 10.3") or the legitimate `SetOperationComplete`
(STATUS-finished + level-12) path. No live callback exists. **This is the 4th "doc says the code is
broken, code is already fixed" today** — treat such audit assertions as needing verification, not as
evidence. Action: mark §14 point 5 and the discrepancy audit as resolved.

**(b) `STZ`/`STZTX` do NOT decompose into byte writes.** Both decode to a single
`WriteVirtualMemory(addr, 0, pt, WriteMode.WORD)` → `WriteMemory16(addr<<1, 0)`
(`Instructions.MemoryReference.cs:46-49, 414-417`; `CpuND100.MMS.cs:873, 1020`). Only the byte
instructions use `WriteMode.MSB/LSB`. **Consequence:** if SINTRAN clears X5ACT with `STZTX` (as the
NPL carve `MP-P2-N500.NPL:3027` @`145500` shows — *different revision*), it arrives as ONE halfword
store and the halfword hook at `ND100Memory.cs:543` catches it — which makes the byte-path machinery
at `:495-524` and its comment ("SINTRAN clears X5ACT one BYTE at a time … a halfword-only gate NEVER
sees the activation") **wrong**.

| ID | Question | Route | Blocking |
|---|---|---|---|
| **Q-OCT-24** | **Does L07 SINTRAN actually clear X5ACT with a single `STZTX` (one halfword store), or byte-at-a-time?** The carve (diff revision) says `STZTX`; the emulator comment claims an observed byte-at-a-time clear. If `STZTX`, the byte-path activation machinery is dead code built around a misobservation | **LIVE** — instruction-trace the octobus harness at the X5ACT clear; read the actual store opcode(s). Do NOT assume either way | Yes — the activation path rests on which is true |

## 2.8 NDIX cross-check (2026-07-20) — and a generation warning

The NDIX (Norsk Data Unix for the ND-500) kernel source at `E:\Dev\Ronny\NDIX-C` was reviewed
against our model.

> ### ⚠ NDIX IS CLASSIC ND-500, NOT ND-5000
> The Release-3 NDIX kernel talks to the **3022/5015** path. The interface-spec PDF whose title
> reads "…ND-5000 - ND-100 Interface Specification" is **mis-titled — it is ND-500** (confirmed by
> Ronny 2026-07-20; the "5000" was an assumption when the file was named). Likewise
> `ND-05.012.01 ND-500 Micro Program Guide` is **classic** authority.
>
> **Do not import NDIX or ND-05.012.01 facts into the ND-5000/octobus model without checking they
> are generation-independent.** This is the §4 "generation seam" trap: a shared message layer does
> not mean an identical one.

### Generation-INDEPENDENT — safe to use, and they strengthen weak items

| Finding | What it confirms |
|---|---|
| `fecall` does `phyladr` → `add2 r1,_private` → `div3 r1,$2` to produce an **ND-100 word address** (`NDIX-C\kernel\MASTER\GENERIC\locore.s:618`), and `private` **is** ADRZERO, handed to NDIX at FE_INIT | The address arithmetic in §2.6: ND-500/MFbus byte + ADRZERO → ND-100 physical, halved for word addressing. **Third independent source** (microcode, SINTRAN carve, NDIX) |
| Inbound work is a **linked descriptor chain** (`int_descr.id_next`, `iplrec.ip_next`), and `ip_next` is an **ND-100 word address** (`locore.c:791-796`, `trap.c:696`) | Our X5BEX chain-walk model, and upgrades `X5BEX is a word address [D]` (`OctobusND5000Station.cs:823`) |
| Vendor `ND-05.012.01` §13 (lines 1090-1400): message block = **6-word header** (link/status/sender/receiver/size) + function value + parameters; status lifecycle **0** free, **1** message-to-ND-500, **2** in-process, **3** answer, **4** error | Our `[V]` N5STA lifecycle — now **vendor-anchored**, not only byte-derived |
| MON **600₈** = the OMC other-machine call: `callg $0xf8000180,$4,…` = segment 31, offset `0x180`, via a `PC_IND\|PC_OMC` capability | See the two decode corrections below |

### Two corrections to our own microcode decode (applied to `MAILBOX-MICROCODE-PSEUDOCODE.md`)

| Was | Now | Why it was wrong |
|---|---|---|
| `CALL_END case 0o600: >> CALL_SWIP; // swapper-related [D]` | **OMC other-machine call.** `SWIP` is almost certainly *SWItch-Processor*, not *SWaPper* | Guessed from the mnemonic. The same doc's `CALL_MON` @`003757-61` **already** annotated 600 as the NDIX special — the file contradicted itself |
| `MICFU 42 — MSG_PRT: process/context probe [D]` | **Programmed trap** (vendor `ND-05.012.01` §13 lists `42 = programmed trap`) | `PRT` = **P**rogrammed **TR**ap. Guessed from the mnemonic |

**Both were `[D]` labels derived from a mnemonic rather than from behaviour** — the same failure mode
as `IL12Q` (§2.0) and the `LDSWA` branch. That is now **four** instances in one day of *"the name
looked like it meant X"*. Treat any surviving `[D]` that rests on a symbol name as suspect.

### Generation-SPECIFIC — do NOT apply to the 5800

- The vendor classic function table lists `16/17/20/21 = registers`, while B30 dispatches
  `20,21 → MSG_ILLEG`. **These do not conflict** — different machines. It gives `Q-MBX-09` a source
  on the classic side only, and still does not explain why SINTRAN sends 21B on the octobus path.
- *"Nothing but an activate or a terminate from the ND-100 can cause the micro program to leave the
  IDLE loop"* (`ND-05.012.01:1094`, vendor verbatim) — bears on `Q-OCT-19`, but for the **classic**
  IDLE loop.
- The whole 5015 mechanism (it is a **CPU card** on the internal CDB/XD bus; its registers are
  selected by microword **unit codes** like the ALU/MMU; TAG-OUT is a 3-bit strobe selector, TAG-IN
  a 4-bit register strobe) is classic-only. Our path is ACCP/octobus.

### New items

| ID | Question | Route | Blocking |
|---|---|---|---|
| **Q-NDX-01** | **Mine `ND-05.012.01` §13 (lines 1090-1400) in full** — a vendor spec of the mailbox protocol and function-value table that **neither track has read**, while both infer MICFU semantics | MANUAL (present in the tree) | **High value, cheap.** Directly feeds `Q-MBX-11`, `Q-MBX-13`, `Q-MBX-04` |
| Q-NDX-02 | What is the layout of NDIX's 7 `x_cxbtab` interrupt context blocks? | NDIX source | Cross-checks our SAMSON context-block **stride 256 `[D]`** (`Nd500MicrocodeServicer.cs:537`) — a wrong stride loads a neighbouring process's context |
| Q-NDX-03 | Does FE_INIT ever show a concrete observed `private` value in a log/dump/test? | NDIX source / LIVE | Would let us cross-check ADRZERO against the `0x420000` window directly |
| Q-NDX-04 | Which microword/condition sets STATUS "finished" (the classic doorbell)? | Needs a **classic-500 control store**, which we do not have (`Q-OTH-04`) | The last classic doorbell gap; both tracks and NDIX agree it is unresolved |

### Doc hygiene outcome

The NDIX review flagged two files as still carrying the fabricated TAG protocol. On inspection:
- `Developer\MON\calls\60B_N500M_Hardware_Mapping.md` — **already corrected earlier on 2026-07-20**;
  the flag was stale.
- `Operations\SINTRAN\ND500-MONITOR-CALL-ARCHITECTURE.md` — **genuinely still contaminated in the
  body**: a mermaid diagram (`:240`), an assembly comment (`:509`), the TAG-value table (`:613`) and
  **C# sample code** (`:753`, `:772`) all assert `0x01 = MON_CALL_REQUEST`. Correction header added;
  body excision in progress. The C# is the dangerous part — it reads as implementable.

## 2.5 Method note

Four claims from this track were retracted in a single day (§2.0). All four shared one failure mode:
**a plausible mechanism was asserted from partial evidence and then cited by other documents as
fact.** The specific traps, for the next reader:

- Reading an instruction's addressing mode wrong (`LDX I 114` vs `LDX ,B 114`) and then spending a
  carve resolving a symbol that was never involved.
- Assuming a PC below a segment's load base must be "the resident" — check *all* segments' bases.
- Treating two console strings printed in sequence as two arms of a branch when they were two steps
  of one state machine.
- Citing another track's emulator behaviour as hardware corroboration.

**"UNKNOWN" beats a plausible wrong answer** — restating section 1's standing rule, which this track
has now paid for independently.
