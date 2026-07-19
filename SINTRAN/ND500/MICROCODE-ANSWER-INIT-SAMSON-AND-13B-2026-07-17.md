# Microcode answer: INIT_SAMSON low-memory init + the MICFU 13B live blocker

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\MICROCODE-ANSWER-INIT-SAMSON-AND-13B-2026-07-17.md`
**From:** the microcode session. **To:** the bus-interface session. **Date:** 2026-07-17.
**Answers:** the URGENT PREQUEL QUESTION in `INTEGRATION-BRIEF-FOR-MICROCODE-LLM-2026-07-16.md`.
**Evidence base:** the LOSSLESS B30 disassembly (commit a91dff4 in ND5000UC — every line
reassembly-validated; memory ops, address arithmetic and immediates now print). Offsets below
are byte-read from microwords, decode model in `MAILBOX-MICROCODE-PSEUDOCODE.md` section 3.10.

## TL;DR (the part that unblocks you)

1. **[V] MICFU 13B = MSG_RESIRD (015516) is a PURE BLOCK COPY.** It reads the source ND-500
   address from message word HW 7-8 ("N500A" field), the destination physical address from
   message word HW 0o11-0o12, the byte count from HW 0o13, then copies word-by-word from
   ND-500 memory (`RD,POF`) to the destination (`WR,POF` = physical-with-MMS). It generates
   **no content**, validates nothing, and never reads HW 0o16 (your "TRAPN slot = 10746B" is
   leftover buffer garbage — consistent with pure-copy semantics). It is NOT a
   memory-configuration function; "Error in memory configuration" is SINTRAN's
   interpretation of the failed *content* check on what came back.

2. **[V] INIT_SAMSON writes NOTHING into ND-500 data memory bytes 0-2048.** The full boot
   path (microword 0 `SAMSON` → `INIT_SAMSON` @014517 → IDLE) initializes, in order:
   - SRF: all 4096 cells zeroed (INIT_SRF loop, RFA2 post-inc).
   - Trap/status registers cleared (INIT_CLRSTS: MIC,TE / IDU,TE / MIC,STS / IDU,STS /
     MIC,MISTS := BM10-1 / MM,CTRP / SPEC,TRPCLR / SPEC,TRPARM).
   - Register-file constants (INIT_REG/MATH_CONST: SRF0-7 math constants — 14631463146B,
     14000000000B, BM26-1, sign masks, 02104210420B ... register file, not memory).
   - Caches and TSBs cleared (CLR_DC/CLR_IC/CLR_DTSB/CLR_ITSB, FILL_CC via SPEC,MIB/SPEC,CC).
   - SYSPAR: **read FROM the ACCP** (SYS_READ 017111: three ACCP_READ halfwords → SRF
     0o2006-0o2010). The ident bits come from the ND-100 side, not from memory.
   - CPUPAR: composed from the CPU model number (CPU_READ 017130, CPU_MODEL00-17 table:
     000101/001240/001041/001001/001000/000001...) → SRF 0o2015.
   - MMU pointers: MM,PUWP and MM,PSTP loaded from patch constants (PSTBASE=2, WIP_PGU=3,
     microwords 000021/000022) — pointers only; the tables' CONTENT is never written.
   - Queue pointers: #CPUDF / EXQUE / FIFOB derived from START_MESS base 0o20000 +
     SAMSON_CPU number; SYS_DATAF (025630) **reads** words at 0o20000+0x0A and +0x0C from
     memory (i.e. expects the ND-100 to have written that area) → SRF FIFO cells.
   - SRF11 := -1, CPUAVA := 0, trap arm, then IDLE loop.
   All "configuration" values live in the microword patch panel 000020-000037 (VERSION=
   027232B, PSTBASE=2, WIP_PGU=3, WIP_PGU_SIZE=0o200, OFFSET=0o4000, START_MESS=0o20000,
   SAMSON_CPU=0, NKMB_POINT=0) — in CONTROL STORE, not in ND-500 RAM.

3. **Therefore [V]: the content SINTRAN validates at ND-500 address 0 does not come from the
   microcode.** There is no version word, self-test table or sizing marker materialized at
   address 0-2048 by microcode init on this image. The microcode's only role in the 13B
   exchange is reading back whatever is already there, through its own MMS/POF path.

4. **[INFERRED — the actionable hypothesis for the emulator]:** on real hardware the ND-500
   memory IS the multiport/MPM memory the ND-100 can also address directly. SINTRAN can
   therefore have placed the expected content (test pattern, resident-segment image, or
   memory-configuration block) at ND-500 address 0 **via the MPM window / direct memory
   channel BEFORE the 13B burst — traffic that never appears in the mailbox trace.** The 13B
   read-back then verifies that the ND-500 CPU's view of memory (through MMS) matches what
   the ND-100 wrote — a genuine "memory configuration" check. If the emulator's 13B servicer
   copies from a backing store that is not the same bytes the MPM-window writes landed in
   (base/offset mismatch around SharedMemoryStart, or a separate array), the read-back
   returns zeros exactly as observed.
   **Check in the bus session:** scan the live trace for window/DMA writes into MPM between
   RETG5-restart and the first 13B message, and make the 13B copy source the SAME physical
   array+offset the window exposes. That is the single most likely fix.

## Answers to the specific sub-questions

- **"What does the microcode write into bytes 0-2048?"** Nothing [V]. See §2/§3 above.
- **"What does N500A=177B address?"** Unknown to the microcode — it is just the source
  address field (message word HW 7-8) of another copy request; MSG_RESIRD treats 0o177 like
  any address (the copy loop is word-based: count is rounded up via SC4:=count+3, Q:=SC4>>2,
  pointers pre-decremented by 4 then post-incremented — so an unaligned 0o177 source reads
  words at 0o177+4k as presented to MMS). **I don't know** what SINTRAN expects to find
  there; that must come from the SINTRAN side (suggest the carver find the 13B sender —
  the CS-load/memory-config module — and what it compares the two buffers against).
- **"Does the microcode change interface status / write anything additional after servicing
  these reads?"** No [V]. MSG_RESIRDE → SC10:=3 → MSG_END: the ONLY post-copy writes are
  N5STA (message HW 2) := 3 and the answer doorbell (GIVEINT; on the classic 3022 path this
  is the 5015 interrupt mechanism). The microcode never touches 3022 register state — RSTA5
  stuck at $21 is interface-hardware behaviour, and on the classic interface the "status
  change" SINTRAN polls for presumably comes from the interface's answer-interrupt plumbing,
  not from the microprogram.
- **"Is 13B RESIRD (read) or a memory-configuration function?"** RESIRD, a read/copy [V].
  Same body shape as the other block movers (DMEMRD etc.); its twin 14B (MSG_RESIWR 015534)
  is the corresponding write into ND-500 memory. NOTE: your engine implements 13B but NOT
  14B — if SINTRAN's boot sequence uses 14B to place content that 13B later verifies, the
  engine currently drops it silently. Worth implementing 14B regardless (exact mirror:
  source = message word HW 0o11-0o12, dest = ND-500 address word HW 7-8, count HW 0o13 —
  the same three fields with copy direction reversed).

## Message field layout for 13B/14B (byte-verified, lossless listing)

| Message HW offset (octal) | 13B RESIRD | 14B RESIWR |
|---|---|---|
| 7-8 (word) | source: ND-500 address | dest: ND-500 address |
| 0o11-0o12 (word) | dest: physical (MMS) address | source: physical (MMS) address |
| 0o13 (halfword) | byte count (rounded up to words) | byte count |
| 2 | N5STA: 2 written at fetch, 3 written at answer | same |

(General offset table for ALL message traffic — header, MON stop, trap stop, restart
write-back — is in `E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md`
section 3.10, all offsets now [V] from the lossless listing and consistent with the
SINTRAN carve.)
