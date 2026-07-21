# Microcode answer to C1: Who builds the PCB (32 program + 32 data capabilities) and the PST, and WHEN?

**Date:** 2026-07-20
**Track:** ND-5000 microcode-CPU (B30 image)
**Answers:** question C1 / task 2.1 in
`SINTRAN/ND500/QUESTIONS-FOR-ND5000-MICROCODE-SWAPPER-START-2026-07-20.md`
**Image under analysis:** MICRO-5800-B30 (ND-5800 / SAMSON / Octobus generation), 16384 words.
**Every claim below cites file:line + the actual microword text. Where the microcode is
silent I say so.**

Sources:
- Disassembly: `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md`
- Field semantics: `E:\Dev\Ronny\ND5000UC\manual\MICROCODE-FIELDS.md`
- Vendor: `E:\Dev\Ronny\ND500UC\manuals\ND-05.012.01 ND-500 Micro Program Guide.md`

---

## BOTTOM LINE

**At mailbox function 23 (octal) = 3START the microcode DOES NOT build the PCB or the PST.**
It only LOADS a per-process CONTEXT BLOCK that already exists in ND-500 physical memory, and
from that block it loads the general registers plus the MMU POINTER registers (PS, DOM, ADOM,
MOD). The PST root pointer (PSTP) is loaded ONCE at CPU init from a fixed constant, not per
process. The PST and the PCB are therefore pre-existing, memory-resident tables that the MMU
walk dereferences; building them is SOFTWARE's job. The microcode cannot tell us which piece
of software - only that it is not the microcode.

---

## The trace: what fn 23 actually does

### 1. Dispatch: MICFU 23 -> MSG_START

- `MICRO-5800-B30.md:6837` `015247` **MSG_23** ... `[ADDR=MSG_START]`
- `MICRO-5800-B30.md:6839` `015251` **MSG_25** ... `[ADDR=MSG_START]`  (3TRACO shares 3START)

### 2. MSG_START body - a CPU-available gate, then load-context-and-run

- `MICRO-5800-B30.md:7111` `015671` **MSG_START** ... `[ADDR=CPU_AVAIL?]`
- `MICRO-5800-B30.md:7112` `015672` ... `COND,MZRO ... [ADDR=MSG_START1]`
- `MICRO-5800-B30.md:7114` `015674` **MSG_START1** ... `[ADDR=NEWCNTXT]`
- `MICRO-5800-B30.md:7115` `015675` ... `[ADDR=EXECUTE]`

So 3START == `if (CPU available) { NEWCNTXT(); EXECUTE(); }`. There is NO table-construction
step. NEWCNTXT is the ONLY thing that touches MMU state, and it only LOADS.

### 3. NEWCNTXT -> CNTXTLOAD reads a pre-existing context block

- `MICRO-5800-B30.md:6590` `014660` **NEWCNTXT** `A,SRF11 ... D,SC12` (SC12 := current-process index from SRF11)
- `MICRO-5800-B30.md:6594` `014664` **NEWCNTXT1** ... `[ADDR=CNTXTLOAD]`

CNTXTLOAD computes the block address and reads it:

- `MICRO-5800-B30.md:6640-6643` `014742`-`014745` **CNTXTLOAD** four x `ALU,A+B,*2 A,SC12 B,SC12 D,SC12`
  -> SC12 := process_index * 256 (stride 0o400 bytes)
- `MICRO-5800-B30.md:6644` `014746` `... D,SC14 [ADDR=OFFSET]`  (fetch the OFFSET constant)
- `MICRO-5800-B30.md:30` `000020` **OFFSET** `A,LARG LARG=00000004000 ... D,SC13` (**OFFSET = 0o4000**)
- `MICRO-5800-B30.md:6645` `014747` `ALU,A+B A,SC12 B,SC13 D,DAC,DPA` -> **DPA = 0o4000 + index*0o400** = context-block base
- `MICRO-5800-B30.md:6646` `014750` **CNTXTLOAD0** `... ADACT AA=2` (address base := DPA)
- `MICRO-5800-B30.md:6647-6650` `014751`-`014754` `A,DATA ... D,SC3/SC4/SC5/SC6 ... RD,POF` (read words P/L/status)
- `MICRO-5800-B30.md:6653` `014757` **CNTXTLOAD1** `A,SC3 ... D,IAC,P` (**P register loaded** from block)
- `MICRO-5800-B30.md:6654` `014760` `A,SC4 ... D,IAC,L` (**L register loaded**)
- `MICRO-5800-B30.md:6656-6667` `014762`-`014775` `A,DATA ... D,X1..X4,A1..A4,E1..E4 ... RD,POF` (register file loaded)

All reads are `RD,POF` = "Read physical with MMS" (`MICROCODE-FIELDS.md:1136`). The block is a
PHYSICAL-memory structure, not shared-window/mailbox data.

### 4. The MMU pointer registers are LOADED (not built) from that same block

- `MICRO-5800-B30.md:6705` `015043` **NEW_PS_1** `A,SC13 ... D,MM,PS` (**MM,PS := block value**)
- `MICRO-5800-B30.md:6706` `015044` `A,SC13 ... D,MM,PHS`
- `MICRO-5800-B30.md:6675` `015005` `A,SC12 ... D,SPEC,MOD` (**modus loaded**)
- `MICRO-5800-B30.md:6679` `015011` `A,SC3 ... [ADDR=NEW_CED]`, `:6680` `015012` `A,SC4 ... [ADDR=NEW_CAD]`
- `MICRO-5800-B30.md:6713-6714` `015053` **NEW_CED** ... `015054` `... D,MM,DOM` (**current domain loaded**)
- `MICRO-5800-B30.md:6715-6716` `015055` **NEW_CAD** ... `015056` `... D,MM,ADOM` (**alt domain loaded**)

MM,PS / MM,DOM / MM,ADOM are the MMU register file selects (`MICROCODE-FIELDS.md:442,447,452`,
also IMM twins :458,463,468). These are POINTERS the MMU walk starts from - the microcode
copies them out of the block; it never populates the tables they point at.

### 5. PSTP (the PST root) is loaded ONCE at CPU init, from a constant - answers B4 too

- `MICRO-5800-B30.md:6533` `014567` `... D,SC4 [ADDR=PSTBASE]`
- `MICRO-5800-B30.md:31` `000021` **PSTBASE** `A,LARG LARG=00000000002 ... D,SC13` (**PSTBASE = 0x02**)
- `MICRO-5800-B30.md:6536` `014572` **INIT_SAM_3** `A,SC3 ... D,MM,PUWP`
- `MICRO-5800-B30.md:6537` `014573` `A,SC13 ... D,MM,PSTP`  (**MM,PSTP := 0x02, a fixed physical anchor**)

`MM,PSTP` is `A,DMM,PSTP` in the field table (`MICROCODE-FIELDS.md:437`). It is set at INIT_SAM
and NOT written by NEWCNTXT/CNTXTLOAD/MSG_START. So the PST lives at a fixed low physical
address (PSTP=2) established at CPU init; 3START never touches it.

(A second, reset-style path `017731`-`017736` sets PSTP:=0, PUWP:=4, PS/DOM/ADOM/PHS:=1 -
`MICRO-5800-B30.md:8167-8172` - the same "load a register, do not build a table" pattern.)

Aside that matches the live capture: `MICRO-5800-B30.md:45` `000037` **MACRO_SETP**
`A,LARG LARG=00000000004 ... D,IAC,P` hard-sets P:=4, matching the observed `P = 0x00000004`.

### 6. The microcode only ever WRITES the block to SAVE context, never to build a PCB/PST

CNTXTSAVE stores the live registers back into the same 0o4000+index*0o400 block on a context
switch:

- `MICRO-5800-B30.md:6596` `014666` **CNTXTSAVE** (address arithmetic, stride as above)
- `MICRO-5800-B30.md:6608-6623` `014702`-`014721` `... A,SC3/SC4/.../E4 ... WR,POF` (register save stream)

`WR,POF` = "Write physical with MMS" (`MICROCODE-FIELDS.md:1129`). This is register save/restore
of an EXISTING block - not construction of capability tables.

### 7. Vendor cross-check (section 13.14 START)

- `ND-05.012.01 ... .md:1328-1336`: fn 23 START message data part is EMPTY (link.07/link.10
  blank). Text: "The start function is only returned as a monitor call or a trap."

The vendor block for START carries NO PCB, NO PST, NO capabilities, NO register image. It is a
pure "run it" doorbell. The classic register-image path (fn 20 read / 21 write,
`:1302-1326`) is a SEPARATE function - and on THIS B30 image functions 20 and 21 are
**MSG_ILLEG** (`MAILBOX-MICROCODE-PSEUDOCODE.md:164` table row "20,21 | MSG_ILLEG"), i.e. the
5800 does not even accept a mailbox register image.

---

## Verdict per candidate

**(a) The swapper builds the PCB/PST itself once running - NOT CONFIRMED / NOT REFUTED by the
microcode.** The microcode is silent on which software seeds the PST(@PSTP) and PCB. It is
CONSISTENT with (a): the microcode loads PSTP=2 and PS/DOM/ADOM, then EXECUTEs; if those
tables were still empty the first translated access would fault, which is exactly the class of
fault you observe. But nothing in the microcode proves the swapper is the builder rather than
SINTRAN. Deciding (a) vs (d) requires the SINTRAN-side / swapper carve, not the microcode.

**(b) The microcode builds them at process start (from a context block) - WRONG.** At 3START
the microcode's entire MMU-touching path is NEWCNTXT -> CNTXTLOAD (citations in sections 3-5),
which READS a pre-existing physical context block and LOADS pointer registers. There is no
capability-table or PST construction anywhere in MSG_START, NEWCNTXT, CNTXTLOAD, NEW_PS,
NEW_CED, or NEW_CAD. The microcode builds nothing at start.

**(c) They live in control store / CPU-internal state and never appear in memory - WRONG.**
PSTP is a physical memory pointer (loaded = 0x02 at init, `014573`); PS/DOM/ADOM are memory
pointers loaded from a physical block (`015043/015054/015056`); the block itself is read/written
with `RD,POF`/`WR,POF` = physical memory (`MICROCODE-FIELDS.md:1129,1136`). The PST and PCB are
memory-resident tables the MMU dereferences on access - not control-store/CPU-internal.

**(d) SINTRAN builds them in ND-500 LOCAL memory (not the shared window) - BEST SUPPORTED by
the microcode.** The two anchors the microcode uses are LOW PHYSICAL addresses reached with the
ND-500's own physical-with-MMS access: PST root PSTP = 0x02 (`014573` + `000021`), and the
per-process context block at physical 0o4000 + index*0o400 (`014746/014747` + `000020`). Neither
is in the ND-100 shared-memory window. That is exactly why your 8 MB shared-window scan for a
PSTE naming the swapper page tables found ZERO matches: the PST/PCB are not in the shared
window at all - they are in ND-500 physical/local memory, keyed off PSTP=2. The microcode
confirms the LOCATION class (local physical) and confirms it is not the builder; it does not
name the builder.

---

## What remains UNKNOWN (do not build past these without more carving)

1. **WHICH software writes the PST (at PSTP=0x02) and the PCB.** The microcode proves only that
   it is not the microcode. Candidates still open: the ND-100 SINTRAN side (via physical/RESIWR
   writes), the ACCP firmware (outside every carve), or the swapper's own early code. This is
   the (a)-vs-(d) split and the microcode cannot close it. Carve target: the swapper's own
   entry code (`SWAPPER-K01.PSEG` from PSEG+4) and SINTRAN's place-domain path.

2. **WHO first seeds the per-process context block at 0o4000+index*0o400 on a cold swapper
   start.** CNTXTLOAD assumes it is already populated (it loads P, L, regs, PS, DOM, ADOM, MOD
   from it). CNTXTSAVE writes it, but only after a process has run. On the 5800, fn 21 (3WREG)
   is MSG_ILLEG, so the classic mailbox register image is NOT the seeding path here. The initial
   seeding mechanism is not visible in the mailbox microcode.

3. **The exact PST -> PCB walk (question C2).** The mailbox/context code loads PSTP/PS/DOM/ADOM
   but does not itself walk PST[PS] -> PCB -> capability -> page table; that walk lives in the
   MMU-translation microcode, which is not decoded here. So the spec's "PST[PS] -> PCB (256
   bytes/domain, pcb_pc[32]@0, pcb_dc[32]@64)" is neither confirmed nor refuted by this file.

4. **Whether physical PSTP=2 and 0o4000 are visible to the ND-100 at all.** `POF` = "physical
   with MMS" is the ND-500's own physical space. How (or whether) the ND-100 reaches those
   addresses to write the tables is not established from the microcode.

---

## One-line consequence for the emulator

Stop trying to find the PST/PCB in the shared window - the microcode roots the PST at a fixed
ND-500 physical address (PSTP = 0x02, set at CPU init) and reads a per-process context block at
physical 0o4000 + index*0o400. At 3START it only loads P/L/registers/PS/DOM/ADOM/MOD from that
block and runs. If your hand-built MMU state makes the swapper run with zero faults, you are
emulating what CNTXTLOAD would have loaded - but the authoritative producer of the PST/PCB
contents is software you still have to identify (swapper vs SINTRAN), in ND-500 local memory.
