# Phase 2 (MMU anchors) - consolidated synthesis, 2026-07-20

**Full path:** `SINTRAN/ND500/PHASE2-MMU-ANCHORS-SYNTHESIS-2026-07-20.md`

Synthesises five independent agent investigations of the ND-5800 B30 microcode + the swapper's own
bytes, run 2026-07-20 to answer Phase 2 of `D4-PLAN-PHASES-AND-TASKS-2026-07-20.md`. Source docs:

- `MICROCODE-ANSWER-C1-PCB-PST-BUILDER-2026-07-20.md` (who builds PCB/PST)
- `MICROCODE-ANSWER-PSTP-AND-SEGMENT-2026-07-20.md` (PSTP + segment)
- `MICROCODE-ANSWER-21B-REGISTER-ORDER-2026-07-20.md` (register block)
- `MICROCODE-ANSWER-MMU-WALK-AND-ENABLE-2026-07-20.md` (walk + enable)
- `CARVE-SWAPPER-ENTRY-STARTUP-2026-07-20.md` (swapper entry, software-side cross-check)

**Confidence:** every headline below is stated by >=2 independent agents, and the load-bearing
dispatch claim was byte-checked by the main track directly (see [V-MAIN]).

---

## 1. CONFIRMED facts (multi-agent, coherent)

### 1a. The classic register-image start is DISABLED on this image [V-MAIN + agents 1/2/3]
Mailbox functions 16, 17, 20, 21 (register examine/deposit + 3RREG/3WREG) ALL dispatch to
`MSG_ILLEG`. **Byte-verified by the main track:** `MICRO-5800-B30.md:6834-6835` show `MSG_20` and
`MSG_21` both `[ADDR=MSG_ILLEG]`; `:6832-6833` show `MSG_16`/`MSG_17` likewise. So the microcode
NEVER reads a register block and NEVER packs two ND-100 halfwords into a 32-bit register.
**Consequence:** the emulator's 21B register-image inspection (task #15) and 20B register readback
(task #14) model a path this image rejects. The "B2 word order" question (our former "biggest single
unknown") is **MOOT on B30** - there is no packing step to have an order.

### 1b. The swapper is started with a CONTEXT BLOCK, not a register image [agents 1/2/3/4]
`MSG_23`/`MSG_25` (`:6837`,`:6839`) -> `MSG_START` -> `NEWCNTXT @014660` -> `CNTXTLOAD @014742`,
which READS a pre-existing per-process context block and loads P/L/X/A/E/PS/DOM/ADOM. `MSG_22` -> 
`MSG_STARTP0` is the watchdog/P0 arm, not a register load. This is the `LCNTXT`/`NEWCNTXT` path that
question B5 asked about - answered: **yes, context block, not bare registers.**

### 1c. PSTP = physical constant 2, set once at CPU init [agents 1/2/4, triple]
`INIT_SAM_3 @014573: MM,PSTP := SC13`, where `SC13 = PSTBASE = 2` (`@000021`). `CNTXTLOAD` never
touches PSTP; it is machine-global, not per-process, and appears in no handover message.
**EMULATOR FIX:** seed `PSTP = 2` (physical) at CPU init; stop expecting it in the 21B image.

### 1d. The segment is a separate PS register, not part of P [agents 2/4]
P carries the OFFSET only (`= 4`). The current program segment lives in a distinct `PS` register
(`= 1`), loaded from the context block (`NEW_PS_1 @015043`). Fetch VA = `(PS << 27) | P` =
`0x08000004`. Cold-start hardcodes `MM,PS:=1` (`@017733`), `IAC,P:=4` (`@000037`), `DOM/ADOM:=1`.
**EMULATOR FIX:** model PS as its own register; do NOT derive a segment from the 32-bit P.
(Note `5800-30.TEXT:147-148`: this B30 fixes an earlier bug where a PS change "was not done in
hardware" - PS now propagates, so relying on it is safe on THIS image.)

### 1e. Address translation is ON at swapper entry; the swapper never enables it [agents 4/5]
Two hardware MMU units - IMM (instruction) + DMM (data) - each with its own PSTP/PS/DOM/ADOM. The
swapper's entry code contains NO PMON/PMOF and runs translated in segment 1; it only drops data
translation transiently via DMOF (always restored by DMON, 66 pairs) to touch physical page frames
during paging. **EMULATOR: the current "enable both program+data MMU at start" is CORRECT.**

### 1f. Neither the microcode nor the swapper builds the PCB/PST [agents 1/5, both sides]
- Microcode: `MSG_START` does `NEWCNTXT(); EXECUTE()` - it constructs no capability table (agent 1).
- Swapper: its entry writes no PST/cap table and reaches physical memory via the DMOF/DMON windows,
  i.e. it does NOT own caps for that memory (agent 5).
So the PCB/PST/context block are built by **SINTRAN or the ACCP cold-start path, in ND-500 LOCAL
memory, before the first swapper instruction** - which is exactly why an 8 MB scan of the SHARED
window found zero PSTE.

---

## 2. The MMU walk itself is HARDWARE (cannot be byte-verified from B30) [agent 4]

The `PS -> PST[PS] -> PCB -> cap -> psn -> PST[psn] -> PT -> PTE` walk is performed by the IMM/DMM
silicon, not the microcode. The microcode's only MMU bit-masks (`MMS_SIX0`, `PF_NORM`) operate on the
hardware EXCEPTION-STATUS word on a fault, NOT on an in-memory PTE/PSTE.

**Therefore these encodings the emulator relies on are UNPROVEN by microcode:**
- PCB = 256 B/domain, `pcb_pc[32]@0`, `pcb_dc[32]@64`
- PSTE 4 B, bits 1-0 index mode, bits 31-2 PFN
- PTE bit 0 protection, bits 31-2 PFN, `PFN==0` = not present

**Provenance caveat (important):** `ND500_MMU_SPECIFICATION.md` is reverse-engineered from **NDIX-C
(Norsk Data Unix), a different guest OS** on the same silicon. The offsets are software-sourced and
consistent, but not proven for the SINTRAN swapper's tables. Do not upgrade them to [V]. `MMS_SIX0`
masks the TOP two bits (`0xC0000000`) of the status word - do not assume the hardware field order
matches the software struct.

---

## 3. The single remaining OPEN question

**Which software builds the context block + PCB/PST in ND-500 local memory, and where exactly?**
Two candidates, and the observed `P=4 / PS=1` values match BOTH, so they do not disambiguate:
- SINTRAN via the mailbox-23B context-load path, or
- the ACCP cold-start vector (`MACRO_STARTL @000033`).

Agent 1 places the context block at ND-500 LOCAL physical `0o4000 + index*0o400` and the PST root at
physical `2`. This is a **SINTRAN/ACCP-side carve**, and it is the natural next Phase-2 step. It also
bears on Phase 1 (the SWPINFO-empty null-deref): if the swapper reads its message/context from a
local-memory block the emulator is not populating, r2=0 at PC=0x913B could be the same gap.

---

## 4. Emulator change-list (from CONFIRMED facts only; spot-check each before coding)

Standing directive is "hack whatever is needed to make it run", so these are actionable - but the
swapper already runs to PC=0x913B with zero MMU faults under the current hand-built model, so NONE of
these is proven to move the D4 stop; they are FIDELITY fixes that also de-risk later breakage:

1. **PSTP = 2 at CPU init** (1c). Small, safe, correct.
2. **Model PS as a distinct register (=1); VA = (PS<<27)|P; P is offset-only** (1d). Medium.
3. **Keep MMU-on-at-start** (1e) - already correct, no change.
4. **Recognise 20/21 as illegal on B30** (1a) - the register-image path (tasks #14/#15) is modelling
   a disabled mechanism; the faithful model is context-block load. Larger; defer until the
   local-memory table source (section 3) is carved, or it will just move the guesswork.
5. **Do NOT harden the NDIX-sourced PCB/PSTE/PTE offsets to [V]** (section 2) - leave them flagged.

**Recommendation:** do change 1 and 2 (cheap, confirmed, correct) and leave 4 until section 3 is
answered, because switching to a context-block model without knowing who fills the context block
would replace one hand-built fiction with another.
