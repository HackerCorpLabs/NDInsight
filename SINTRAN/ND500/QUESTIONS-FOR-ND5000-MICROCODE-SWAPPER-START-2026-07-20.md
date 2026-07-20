# Questions for the ND-5000 microcode CPU track: SWAPPER SET-UP and START

**Date:** 2026-07-20
**Asked by:** the ND-100 <-> ND-500 (3022) emulation track in RetroCore
**Scope:** ONLY the moment SINTRAN sets up the swapper and starts it - the code placement, the
register load, and the MMU state at that instant. Not the swapper's later paging work.

## Why we are asking

On a real SINTRAN III L boot (ND-5800-provisioned image) we now get the REAL ND-500 swapper to
execute on our functional `CpuND500` interpreter. It runs its whole startup path with zero MMU
faults and parks on a monitor call, and SINTRAN prints `Fatal error from Swapper / ERROR CODE 200B`.

It only gets that far because we HAND-BUILD the MMU state at process start. That hand-building is
guesswork in places, and we would rather have it correct than merely working. Every question below
is of the form "here is exactly what we do - is that what the microcode really does?"

**How to answer:** for each question, ideally one of
- `CONFIRMED` + where in the microcode it is visible, or
- `WRONG` + what the microcode actually does, or
- `NOT IN MICROCODE` (i.e. it is software's job - then say whose).

Marking a question `UNKNOWN` is a perfectly good answer. Please do NOT guess - a plausible but wrong
answer here costs us more than no answer, because we would build on it.

---

## ALREADY ANSWERED BY CARVING - please do not spend time on these

After writing this, we carved the swapper itself (`SINTRAN/ND500/swapper/swapper-k01-pseg.asm`,
base `0x08000000`, plus `swapper-k01-deep-analysis.md` section 5.1). That settled three things:

1. **The swapper is linked at SEGMENT 1** (virtual base `0x08000000`). Its own operands prove it:
   entry `init $1000441124,$44,$17504` = `0x08024254` = the run-time stack bottom (DSEG
   `0x24254..0x26197`, 8004 bytes), and `call $1000100645` = `0x080081A5`. Both matched our live
   traps exactly. Code and data share segment 1, separated by the I/D split.
2. **Our trap diagnosis was right and SINTRAN's console print is wrong** - `0x080081A5` is a `call`
   (instruction fetch), which SINTRAN displayed as "DATA segment READ access". Question D1 stands.
3. **The `comp2` at entry is a build-tag self-check** (`REV.` / `-K01`), and the swapper executes
   `MON 0B LEAVE` if it mismatches - so a wrong-revision swapper exits rather than misbehaving.

**This SHARPENS question B3/B4 rather than closing it:** SINTRAN's 21B image sends
`P = 0x00000004` - offset only, with NO segment bits - while the code demonstrably runs at
`0x08000004`. So something supplies the segment number 1, and we do not model it. **Where does the
program segment number come from at 3START?** That is now our single most concrete register question.

---

## Context: the sequence we observe, in order

1. SINTRAN sends mailbox `14B` RESIWR transfers (44 of them) into the shared memory window:
   `0x5A000-0x6F7FF`, of which 40 of 44 pages are entirely ZERO.
2. Two of those transfers are PAGE TABLES (ascending PFN words):
   - `0x6E800` = `00DF 00E0 00E1 ...` -> `0xDF << 11` = `0x06F800`
   - `0x6E000` = `0049 004A 004B ...` -> `0x49 << 11` = `0x00024800`
   - `0x6F000` holds a small descriptor (`02 C0` at +3).
3. Console prints `> Loading Control Store`, then `> Loading Swapper`.
4. The swapper's EXECUTABLE CODE appears at physical `0x06F800` (19 dense pages), byte-for-byte
   identical to the `SWAPPER-K01.PSEG` file we have on disk (38,161 bytes). **No `14B` RESIWR ever
   names that address** - it arrives by the separate "Loading Swapper" path.
5. SINTRAN sends mailbox `21B` (3WREG) with a 44-register image, then `23B` (3START).
6. We then create MMU state ourselves and let the CPU run from P.

---

## Section A - the swapper CODE placement

### A1. Are those two DMA'd page tables how the hardware learns the swapper's layout?

**We currently do:** locate the swapper by reading those two tables - the table whose target address
was never written by any RESIWR is the PROGRAM segment (`0x06F800`, 19 pages), the one whose target
WAS written is the DATA segment (`0x00024800`, 107 pages).

**Evidence:** verified by byte-comparing `0x06F800` against `SWAPPER-K01.PSEG`.

**Question:** is that the real mechanism - does the microcode/MMU consume exactly these tables? Or
are they incidental, with the true descriptor being the `0x6F000` word (`02 C0 ...`) or something in
the control store?

**Impact if wrong:** our whole segment derivation is built on it.

### A2. What actually writes the code at `0x06F800`?

**Question:** what does "> Loading Swapper" do at the hardware level - a DMA that we are not
modelling, a control-store-driven copy, or an ND-100-side write straight into the shared window? Is
the code expected to be in the SHARED window at all, or in ND-500 local memory?

**Impact if wrong:** we may be reading the right bytes from the wrong memory, which would break as
soon as local and shared memory diverge.

---

## Section B - the REGISTERS at start (mailbox 21B, then 23B)

### B1. Is our register-block numbering right?

**We currently do:** treat the 21B image as 0-based register-block order:
```
0=P 1=L 2=B 3=R  4..7=I1..I4  8..11=A1..A4  12..15=E1..E4
16=ST1 17=ST2 18=PS 19=TOS 20=LL 21=HL 22=THA 23=CED 24=CAD
25..28=mic1..4  29..36=OTE/CTE/MTE/TEMM
```
i.e. `3WREG register N` == `LREGBL register N+1`.

**Evidence:** INFERRED from the ND-500 Reference Manual ch 16.27.2 LREGBL/SREGBL order plus the fact
that the live image has `firstRegister = 0` and P sits at byte offset 0 of the process context block.
NOT byte-verified.

**Question:** is this the order the microcode uses for 3WREG/3RREG?

### B2. Word order inside one register - THIS IS OUR BIGGEST SINGLE UNKNOWN

**We currently do:** assemble each 32-bit register from two ND-100 words as `hi << 16 | lo`
(high halfword FIRST).

**Evidence:** INFERRED, explicitly marked `[D]` in our source. Never verified.

**Question:** high halfword first, or low first?

**Why it matters concretely:** the observed live image has `reg[18] = PS`, whose two words are
`0x4848` and `0x0003`. Depending on word order that is either `0x48480003` or `0x00034848` - two
completely different values, and PS is the register the MMU walk is supposed to start from.

### B3. What is PS, and what does the microcode do with it at start?

**Observed:** `reg[18]` (PS) is the only non-trivial pointer-looking value in the whole image. The
other non-zero entries are `reg[0]=P=0x00000004`, `reg[16]=ST1=2`, `reg[25]=reg[26]=1`, and
`CED=CAD=0`. Everything else is zero.

**Question:** at 3START, does the microcode use PS to FIND the process's MMU state (e.g. `PST[PS]` ->
PCB table, per the MMU spec we have), or is PS just saved for later? What EXACTLY does the microcode
do with PS between receiving 3START and executing the first instruction?

### B4. Where does PSTP come from?

**Observed:** the 21B image contains NO PST-base register at all. Our `PSTP` register therefore stays
0, so we cannot start a table walk from it.

**Question:** how does PSTP get loaded on a real machine at swapper start? From the control store,
from a fixed physical convention, from the process context block, or is it set once at CPU init long
before any process starts?

**Impact if wrong:** this is the single missing anchor blocking a correct memory-resident MMU walk.

### B5. Is the swapper started with a register image at all, or with a context block?

**Question:** is `21B` + `23B` the real start path for the swapper on this generation, or does the
microcode instead load a full process CONTEXT BLOCK (the `LCNTXT` / `NEWCNTXT` path)? If a context
block is involved, what is in it, and does it carry the capabilities/PSTP that the register image
lacks?

---

## Section C - the MMU state at start (our weakest area)

### C1. Who builds the PCB and the PST, and when?

**This is the most important question in this document.**

**What we found:** we scanned all 8 MB of the shared window for a PSTE naming either page table
(`(0xDD << 2) | mode` or `(0xDC << 2) | mode`). **Zero matches.** So SINTRAN does not appear to build
a PST in the shared memory during PLACE-DOMAIN, and in our model that window is the only memory
SINTRAN can write.

**Question:** who creates the PCB (the 32 program + 32 data capabilities) and the PST for the
swapper, and at what point? Candidates we cannot choose between:
- the swapper builds them itself once running,
- the microcode builds them at process start (from the context block?),
- they live inside the control store / CPU-internal state and never appear in memory,
- SINTRAN builds them somewhere we have not looked (ND-500 local memory?).

**Impact:** everything below depends on this. Right now we invent capabilities by hand, which cannot
converge (see C4).

### C2. Is the documented walk the real one?

**We have a spec** (`ND500_MMU_SPECIFICATION.md`) that says: PS register -> `PST[PS]` -> PCB table
physical address -> `cap = PCB[domain].pcb_pc[seg]` (offset 0) or `pcb_dc[seg]` (offset 64) ->
`psn = cap & 0x1FFF` -> `PST[psn]` -> page table -> PTE.

**Question:** does the microcode do exactly this? Specifically: is the PCB really found via
`PST[PS]`, and is the PCB really 256 bytes per domain with `pcb_pc[32]` at offset 0 and `pcb_dc[32]`
at offset 64?

### C3. Confirm two encoding details we now rely on

- **PTE:** bit 0 = protection (0 = writable, 1 = read-only), bit 1 unused, bits 31-2 = PFN, and there
  is NO present/valid bit - "not present" is expressed as `PFN == 0`, so physical page 0 can never be
  mapped. (We verified our emulator matches the spec here, and it made an identity-mapping idea
  impossible.) Is `PFN == 0 means not present` genuinely what the microcode tests?
- **PSTE:** 4 bytes, bits 1-0 = index mode (0 = AZI direct, 1 = ASI single-level, 2 = ADI two-level),
  bits 31-2 = PFN. Correct?

### C4. Which segments should the swapper have, and with what rights?

**What we observe the swapper actually touching:**
- instruction fetch in segment 0 (from P = `0x00000004`) AND in segment 1 (VA `0x080081A5`),
- data write in segment 1 (VA `0x08024255`),
- data write in segment 0 (VA `0x00000002`),
- data write in **segment 13** (VA `0x68000044`).

**We currently do (deliberate hack, not shipped):** point all 32 program and all 32 data capabilities
at the same two page tables, because SINTRAN gives us two tables and never says which segment each
belongs to. With that, the swapper runs with zero MMU faults.

**Questions:**
- What is the correct capability set for the swapper at start - which of the 32 data segments and
  which program segments are valid, and which are writable?
- Is segment 13 meaningful (a physical-memory window? a system-table segment?), or is that access a
  symptom of us having fed the swapper bad data earlier?
- Should the swapper's code be reachable through BOTH program segment 0 and 1, as we found, or does
  the real machine map code in exactly one segment (which would mean our P or our tables are wrong)?

### C5. Is the MMU even enabled at swapper start?

**We currently do:** enable both program and data MMU as part of starting the swapper.

**Question:** does the real swapper start with translation ON, or does it begin in physical/untranslated
mode and switch the MMU on itself after building its tables? If the latter, our whole approach at
start is wrong in an easily-fixed way.

---

## Section D - two smaller verification asks

### D1. Trap report fields back to SINTRAN

We report traps to SINTRAN and it prints them, but the fields disagree with reality: for a fault our
CPU records as an INSTRUCTION fetch at VA `0x080081A5`, SINTRAN printed
`DATA segment READ access / Logical address 1 100645B`; for a segment-1 data write it printed
`Logical address 0 0B`. So our access-type and/or address fields are wrong.

**Question:** what exactly does the microcode place in the trap message - which word holds the access
type, which holds the failing logical address, and how is the segment encoded?

### D2. Error code 200B

SINTRAN prints `ERROR CODE: 200B` from the swapper. Our repo has carve notes using `200B = hwfault`
in the ND-5000/ACCP code space.

**Question:** does the SWAPPER use that same error-code namespace, so 200B here really means hardware
fault? And is there a list of the swapper's error codes?

---

## If the microcode cannot answer: what to carve instead

In priority order, the carve targets that would answer the same questions from the SINTRAN side:

1. **The swapper's own start-up code** - disassemble `SWAPPER-K01.PSEG` from its entry (PSEG+4)
   through the first few hundred bytes. This directly shows what it assumes about registers and MMU
   state on entry, and whether IT builds the PCB/PST (answers C1 and C5).
2. **SINTRAN's 5STDRIV / place-domain path around the 21B + 23B send** - shows what SINTRAN believes
   it is handing over, and whether it writes any MMU tables (answers B1-B5, C1).
3. **The `0x6F000` descriptor** (`02 C0 ...`) - the one placed structure we have not decoded; it may
   be the segment/PST descriptor that answers A1.

---

## Summary: the one question that unblocks the most

**C1 - who builds the PCB/PST for the swapper, and when.** With that answered, the MMU work becomes
a normal implementation task. Second most valuable: **B2** (register word order), because it decides
what PS actually is, and B4 (where PSTP comes from).
