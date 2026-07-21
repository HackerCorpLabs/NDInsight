# ND-5000 microcode answer: MMU walk mechanics and translation-enable at swapper start

**Date:** 2026-07-20
**Answered by:** the ND-5000 microcode-CPU track
**Image under analysis:** ND-5800 **B30** writeable-control-store microprogram (work mode 500).
Decoder: `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md`; labels
`E:\Dev\Ronny\ND5000UC\docs\MC\MICRO-5800-B30.LABE`; raw `5800-30.TEXT`;
pseudocode `E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md`.
This is the ND-5800 microengine, NOT a classic-500 144-bit image.

Questions answered: C5 (task 2.5), C2 and C3 (task 2.6), from
`E:\Dev\Ronny\NDInsight\SINTRAN\ND500\QUESTIONS-FOR-ND5000-MICROCODE-SWAPPER-START-2026-07-20.md`.

---

## TL;DR (the one thing that reframes all three questions)

**Address translation on the ND-5800 is done by dedicated HARDWARE, not by
microcode.** There are two hardware memory-management units - `IMM` (instruction
MMU) and `DMM` (data MMU) - each with its own register file:
`PSTP` (PST base pointer), `PUWP`, `PS` (process segment), `DOM`, `ADOM`, `PHS`,
`LA` (logical address), `MEM` (translated access port).

The B30 microcode's ONLY roles in translation are:
1. **Load the table-base pointers once at CPU init** (`INIT_SAM_3`: writes `MM,PUWP`
   and `MM,PSTP`).
2. **Load the per-process anchors from the context block at process start**
   (`CNTXTLOAD`: writes `MM,PS`, `MM,DOM`, `MM,ADOM`).
3. **Read those hardware registers back on an MMU exception** to build the
   page-fault / protection-violation report (`TRAP_MMSV`, `PF_*`).

Nowhere does the microcode itself read a PCB, a PSTE, or a PTE out of memory to
compute a translation. The `PST[PS] -> PCB -> cap -> PST[psn] -> pagetable -> PTE`
walk is performed inside the IMM/DMM silicon. Consequently the emulator's PTE/PSTE
bit encodings and the 256-byte PCB layout are **not verifiable from this
microcode** - they come from the NDIX-C kernel headers, which is a different
operating system from SINTRAN's ND-500 swapper (same hardware, different software).

---

## Q1 (C5 / task 2.5): Is the MMU enabled at swapper start, or does the swapper enable it itself?

### VERDICT: CONFIRMED - translation is ALWAYS ON. The swapper starts TRANSLATED. There is no software/microcode "enable translation" step, and the swapper does NOT begin untranslated.

**What the start path actually does.** Function 23 (3START) is `MSG_START`
(pseudocode line 591-593): `NEWCNTXT(); EXECUTE();` = "load context and run macro
code".

`NEWCNTXT` (014660) selects/loads the target process context via `CNTXTLOAD`
(014742). `CNTXTLOAD` streams the process context block out of ND-500 memory
(`RD,POF` DATA reads) and loads the architectural registers, ending with the MMU
anchors:

- `MICRO-5800-B30.md:6674` `015004` -> `NEW_PS_1`
- `MICRO-5800-B30.md:6705` `015043` **NEW_PS_1**: `ALU,A TYP,HW A,SC13 B,X1 D,MM,PS` -> writes the hardware **PS** register
- `MICRO-5800-B30.md:6706` `015044`: `... D,MM,PHS` -> writes the physical-segment shadow **PHS**
- `MICRO-5800-B30.md:6679` `015011` -> `NEW_CED` (015053: `... D,MM,DOM`) -> loads current executing **domain**
- `MICRO-5800-B30.md:6680` `015012` -> `NEW_CAD` (015055: `... D,MM,ADOM`) -> loads alternative **domain**

`EXECUTE` (014636) then does: clear pipeline (`AAP1,CLEAR`), `SET_RUNNING`
(014637), arm traps (`TRAP_ARM1`), toggle the instruction cache
(`DIS_IC`/`ENA_IC`, 014647/014656/014657 - `SPEC,MOD` bit BM07/BM26), and jump to
`ADR_MOD` to begin macro-code execution.

**There is no translation-enable bit set anywhere in `EXECUTE` or `CNTXTLOAD`.**
The only mode manipulation is the instruction CACHE enable/disable, which is a
different thing. Every macro-code memory reference is issued through the MMU ports
`A,IMM,MEM` (instruction fetch) and `A,DMM,MEM` / `A,DMM,LA` (data), which
translate unconditionally (68 such operands in the image, e.g.
`MICRO-5800-B30.md:2121` `004073 ALU,FZRO A,IMM,MEM`).

Physical / untranslated access is NOT a global mode; it exists only as dedicated
microcode operands used by the mailbox handlers - `RD,PHYS` (e.g.
`MICRO-5800-B30.md:6720` `015062 ... RD,PHYS`) and the `MM,PHS` physical-segment
select used by MICFU 30/31 PHYSRD/PHYSWR (pseudocode line 573-574). Macro code
(the swapper) never runs in that mode.

**Conclusion for the emulator:** enabling both program and data MMU as part of
starting the swapper is CORRECT. The swapper's very first instruction fetch at its
entry is already translated through the IMM unit, using the PS/DOM you loaded from
the context block. The swapper does NOT switch translation on itself.

**Supporting hardware-register fact.** `MM,PSTP` (the PST base pointer) is loaded
ONCE at CPU/Samson init, not per process:
- `MICRO-5800-B30.md:6536` `014572` **INIT_SAM_3**: `... D,MM,PUWP`
- `MICRO-5800-B30.md:6537` `014573`: `ALU,A A,SC13 B,X1 D,MM,PSTP`

So the PST anchor is machine-global; per-process the only translation state loaded
at start is PS/DOM/ADOM. (This directly answers the companion question B4: PSTP is
a hardware register set at CPU init, NOT carried in the 21B register image.)

---

## Q2 (C2 / task 2.6): Does the microcode perform the documented MMU walk exactly?

### VERDICT: NOT IN MICROCODE - the walk is performed by the IMM/DMM hardware. Some pieces are confirmable as consistent; the byte-level PCB layout is NOT.

The documented walk (from `ND500_MMU_SPECIFICATION.md`, itself reverse-engineered
from the NDIX-C Unix kernel, not from microcode) is:
`PS -> PST[PS] -> PCB phys addr -> cap = PCB[dom].pcb_pc[seg] (off 0) or pcb_dc[seg]
(off 64) -> psn = cap & 0x1FFF -> PST[psn] -> pagetable -> PTE`.

Point-by-point against the B30 microcode:

- **"Is the PCB found via PST[PS]?"** CONSISTENT but NOT microcode-visible. Both
  `PS` and the PST base `PSTP` are real hardware registers (PS loaded at
  `NEW_PS_1` 015043; PSTP loaded at `INIT_SAM_3` 014573; vendor guide
  `ND-05.012.01`:275 defines PS as "Process segment number and process control
  register"). The indexing arithmetic `PST[PS] -> PCB` happens inside the MMU
  silicon; no microcode reads it. So it is architecturally consistent with the
  spec, but cannot be byte-verified here.

- **"Is the PCB 256 bytes/domain with pcb_pc[32] at offset 0 and pcb_dc[32] at
  offset 64?"** NOT IN MICROCODE. The microcode never indexes a PCB. What the
  microcode DOES prove is that the **program-vs-data capability split is real**:
  there are two fully independent MMU units, `IMM` (instruction, which selects a
  program capability) and `DMM` (data, which selects a data capability), each with
  its own `PS` and `DOM` (e.g. `DMM,PS`/`IMM,PS` readbacks at
  `MICRO-5800-B30.md:5694-5696`, and the whole `LOOK_HW` readback block
  `017534-017554` enumerating `DMM,PSTP DMM,PUWP DMM,PS DMM,DOM IMM,PSTP IMM,PUWP
  IMM,PS`). That the hardware keeps program and data capabilities separate matches
  `pcb_pc[]` vs `pcb_dc[]`. The exact offsets 0 and 64, and the 256-byte stride,
  are silicon-internal and UNKNOWN from this image.

- **"psn = cap & 0x1FFF" / capability bit layout.** NOT IN MICROCODE (hardware
  decodes the capability). The fault-path bit masks I can see
  (`MMS_SIX0` 013044 masks `0xC0000000`; `PF_NORM` 013051 masks `0x1F000000` then
  XOR `0xC00000`; `MMS_PST0` 013042 tests bit `BM32`) operate on the hardware MMU
  **exception status word**, not on the in-memory capability or PSTE, so they do
  not confirm `0x1FFF`.

**Net:** the hardware does exactly the kind of walk the spec describes (PS-anchored,
program/data-split, PST-based), but the microcode is not the place where the walk
is spelled out, and the concrete struct offsets/masks cannot be validated from it.

---

## Q3 (C3 / task 2.6): Confirm the PTE and PSTE encodings

### VERDICT: NOT IN MICROCODE / UNKNOWN from this image. The in-memory PTE and PSTE are read and decoded by the IMM/DMM hardware, so the B30 microcode contains neither a PFN==0 test on a memory PTE nor a bit-0-protection / bits-1-0-index-mode decode of a memory PSTE.

- **PTE "not present == PFN==0", bit 0 = protection (0=RW, 1=RO), bits 31-2 = PFN,
  no valid bit:** NOT confirmable here. The microcode never loads a PTE from
  memory. `PROTVIOL` (013036) and `TRAP_PGF0` (via `PF_INFO_OK` 013101) are the
  microcode's handling AFTER the hardware has already raised a
  protection-violation or page-fault exception. So "PFN==0 means not present" is a
  property of the MMU silicon (and of the NDIX-C software that fills the tables);
  the B30 image neither confirms nor refutes it.

- **PSTE 4 bytes, bits 1-0 = index mode (0 AZI / 1 ASI / 2 ADI), bits 31-2 = PFN:**
  NOT confirmable here, and one CAUTION. The label `ILL_INDEX` (003134) exists and
  is reached when an index/mode field is illegal (e.g. from `001602`, `003631`,
  `004631`) - so an index-mode concept is real. But the microcode does not read a
  memory PSTE to get it. CAUTION (observation, not a conclusion): the only
  index-mode-shaped field extraction in the fault path, `MMS_SIX0` (013044), masks
  the **top two bits** (`LARG=30000000000` octal = `0xC0000000`) of a status word,
  not the bottom two. That is a HARDWARE STATUS word, not the PSTE, so it does not
  contradict "bits 1-0 of the PSTE" - but it is a reason not to assume the
  hardware's internal field order matches the software struct's bit order. Treat
  the PSTE layout as software-sourced (NDIX-C header), not microcode-verified.

---

## Cross-cutting confirmations that ARE solid from the B30 image

1. **PS is a hardware register and IS programmed at process start** (answers the
   companion B3). `NEW_PS_1` (015043) writes `MM,PS`. The vendor doc even records
   the B30 bug-fix that makes this work: `5800-30.TEXT:147-148` - *"When changing
   the PS register, this was not done in hardware."* That correction is in THIS B30
   image, so writing PS via the register block / context block now propagates to
   the MMU. (Older images did not - matching the NDIX-C comment that "the microcode
   doesn't support it yet".)

2. **PSTP/PUWP are hardware registers, loaded once at init, not per process**
   (answers B4). `INIT_SAM_3` 014572-014573.

3. **Start uses a CONTEXT BLOCK, not a bare register image** (answers B5). 3START ->
   `NEWCNTXT` -> `CNTXTLOAD` reads a process description/context block from ND-500
   memory and loads P, L, X1-X4, A1-A4, E1-E4, ST1/ST2, then PS/PHS/CED(DOM)/
   CAD(ADOM). The 44-word 21B register image SINTRAN sends is the source data that
   ends up in that context block; the MMU anchors (PS/DOM) come through it.

4. **Two MMU units, program and data, each independently anchored** - IMM
   (instruction) and DMM (data), each with PS/DOM/PSTP/PUWP/PHS/LA/MEM. This is the
   hardware realisation of the pcb_pc / pcb_dc split.

---

## What remains UNKNOWN (cannot be answered from the B30 microcode)

- The exact in-memory PCB layout: 256-byte stride, `pcb_pc[32]` at offset 0,
  `pcb_dc[32]` at offset 64. (Hardware silicon; not in microcode.)
- The exact in-memory PTE bit layout and the "PFN==0 == not present" rule.
  (Hardware reads the PTE; not in microcode.)
- The exact in-memory PSTE bit layout (index-mode bit position and PFN field).
  The microcode's fault-status masks are NOT the PSTE and must not be used to infer
  it; and `MMS_SIX0` masking the top 2 bits is a flag that field order may differ.
- The `PST[PS] -> PCB physical address` arithmetic (scale/offset). Hardware.
- Whether the SINTRAN VSX swapper uses the identical struct offsets as the NDIX-C
  kernel that `ND500_MMU_SPECIFICATION.md` was derived from. Same silicon implies
  the same hardware-visible layout, but this is an inference, not a B30 fact.

### How to actually pin the still-open encodings
Because these are hardware, the microcode will never show them. The two sources
that could confirm them are: (a) the NDIX-C / SINTRAN-VSX table-builder software
that WRITES the PSTE/PTE/PCB (which fixes the layout by construction), or
(b) a live single-step of the swapper taking a real page fault and reading back the
`DMM`/`IMM` status + `LA` + `PHYS` + `CAP` words the microcode collects in
`TRAP_MMSV`/`PF_*` (013016-013101) and comparing to the table bytes in memory.
