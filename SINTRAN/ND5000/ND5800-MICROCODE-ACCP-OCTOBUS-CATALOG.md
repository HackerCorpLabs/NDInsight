# ND-5800 Microcode: ACCP / Octobus Routine Catalog

**Source binaries**: `E:\Dev\Ronny\ND5000UC\docs\MC\MICRO-5800-B30.DATA` (+ `.LABE`), disassemblies `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md` (work mode 500, primary) and `MICRO-5800-A30.md` (work mode 406). All addresses **octal**. **[V]** = read from the listing, **[I]** = interpretation. Companion to `OCTOBUS-ND100-ND5000-REFERENCE.md` in this folder.

Only 23 microwords in B30 touch the ACCP registers directly, through four special operands:

| Operand | Dir | Meaning |
|---|---|---|
| `A,SPEC,AFLAG` (A-source 0151) | read | ACCP flag/status register |
| `A,SPEC,AOB` (A-source 0141) | read | ACCP output buffer (ACCP -> CPU) |
| `D,SPEC,AIB` (dest 041) | write | ACCP input buffer (CPU -> ACCP) |
| `A,SPEC,AOBASR` (A-source 0152) | read | AOB side/"comm" register, read only at boot immediately before AOB |

All AFLAG accesses use SLOW2 (160 ns) cycles. **BM naming is octal**: BM05=bit 5, BM11=bit 9, BM12=bit 10, BM13=bit 11, BM14=bit 12.

## 1. Primitives (B30 @ 016371-016411) [V]

- **ACCP_READ** (016371): `SC13 := BM11`; loop `AFLAG & bit9` until set; `Q/SC13 := AOB`; RETURN/POP. -> AFLAG **bit 9 = AOB has data**.
- **ACCP_WAITI** (016375): same bit-9 wait, returns AFLAG without consuming AOB.
- **ACCP_WRITE** (016402): `SC13 := BM12`; loop (INVSEQ) until `AFLAG & bit10 == 0`; `AIB := SC12`. -> AFLAG **bit 10 = AIB busy**; argument register **SC12**.
- **ACCP_WAITO** (016406): wait bit-10 clear, return.
- **ACCP_XWRITE** (016401): `RF2D := SC12` — does NOT touch AIB; appends to the register-file/memory message buffer addressed by RF2 (pointer from TRAP_OCBM2). [I] buffered assembly of outbound OCB messages fetched by the ACCP from memory.
- **ACCP_RDYW** (017073): command/response exchange: ACCP_WAITO -> set MOD bit 27 -> `SC12 := SC11`, ACCP_WRITE -> ACCP_WAITO -> clear MOD bit 27 -> ACCP_WAITI -> ADR_ATRAP; if returned AFLAG has bit 5 set, stash AFLAG->RF2D and AOB->RF2 (park async OCB word for ATRAP_CHK), else zero both. [I] "send command in SC11; if an async message lands mid-exchange, queue it."

## 2. AFLAG bit map (all test sites)

| Bit (mask) | Tested by | Meaning |
|---|---|---|
| 5 (BM05) | SCAN_ACCP1 -> TRAP_OCBAK; ACCP_RDYW | OCB kick/message pending (-> TRAP_OMESS) |
| 6 (BM06) | SCAN_ACCP2 -> TRAP_OCBA | async trap message pending (-> TRAP_ATRP) |
| 7 (BM07) | 012562 seq -> TRAP_DFC | data-fault indication |
| 8 (BM10) | TRAP_NDF -> TRAP_IFC/TRAP_NIF | instruction-fault indication |
| 9 (BM11) | ACCP_READ/WAITI | AOB data available |
| 10 (BM12) | ACCP_WRITE/WAITO, WRIT_DEV00 | AIB busy |
| 11 (BM13) | SCAN_ACCP/ATRAP_CHK -> TRAP_OTRP | "other trap" pending |
| 12 (BM14) | SCAN_ACCP -> TRAP_PWF | power-fail warning |

Bits 9/10 verified by loop polarity; the rest [I] from dispatch targets. Not documented anywhere else.

## 3. Polling spine [V]

- **SCAN_ACCP** (016554): `SC13 := AFLAG`; chain: bit12 -> TRAP_PWF, bit5 -> TRAP_OCBAK, bit6 -> TRAP_OCBA, bit11 -> TRAP_OTRP, else return. Called from IDLE_1, SEND_112 (NK send retry loop), LOCK_COM2, OCB_WAITSEX, MSG_IMEMRD10, MSG_LINK0 — every long-running loop cooperatively polls the ACCP.
- **ATRAP_CHK** (016572): consumes the two-word queue parked by ACCP_RDYW (stashed AFLAG/AOB in RF2), same bit tests, dispatching to the message-already-read entry points (bit5 -> TRAP_OMESS1 with word in RF2, bit6 -> TRAP_ATRP1, bit11 -> TRAP_OTRP). Called from IDLE and 017427.
- Dispatch plumbing: TRAP_OCBAK/OCBA/EXT load LC=8/7/6 -> TRAP_ACCP (013313) -> `MIC,VECT := LC` -> TRAP_VECT (013316): 6 -> TRAP_OTRP, 7 -> TRAP_ATRP, 8 -> TRAP_OMESS.

## 4. Inbound messages: TRAP_OMESS -> OCB_DECODE -> kicks [V]

- **TRAP_OMESS** (016412): call ACCP_READ (word -> SC13); **TRAP_OMESS1** (016413): `SC5 := SC13` (HW), compare with process-0 id (RF1); equal -> **OCB_DECODE**, else -> **OCTO_SOFT** (016540: deliver to software/process level via TRAP_PROC0).
- **OCB_DECODE** (016417) bit tests on SC5: bit7 clear -> TRAP_NOTREC 205; bit6 -> OCB_MES_E -> NOTREC 206; bit5 -> **OCB_MES_K**: `VECT := SC5 & 077`, JMPREL **OCB_DEC_K** (016430), a 64-entry kick table:
  - kick 0 -> NOTREC; **1,2 -> ACTIVATE**; **3 -> OCB_KICK03**; **4,5 -> OCB_KICK05**; **6 -> OCB_KICK06**; 7-63 -> OCB_KICK64 (UNLOCK_QUE + NOTREC 204).
  - (Note: no OCB_KICK04 exists; kick 4 shares the KICK05 handler.)
- bit5 clear -> **OCB_MES_M** (016533): drain loop `ACCP_READ; repeat while (word & BM17)==0` — **[V] bit 15 of a received word terminates a multi-word message**; then NOTREC 205.

Kick handlers (B30 only):
- **OCB_KICK03** (025522): UNLOCK_QUE; read this CPU's definition word; if flagged, write into the START_MESS region and spin in **OCB_WAITSEX** (poll memory word, SCAN_ACCP each pass); then resume EXECUTE or fall to KICK06. [I] "run/resume" kick with message-region semaphore sync.
- **OCB_KICK05** (025553, kicks 4/5): SET_IDLE, LOCK_QUE, **OCB_CLNUP**, UNLOCK_QUE, PRNOWR(0), NOTREC 204. [I] stop + clean queue.
- **OCB_KICK06/KICK06** (025561/025563): CNTXTSAVE if process loaded, SET_IDLE, OCB_CLNUP, UNLOCK_QUE, PRNOWR(SC14), IDLE. [I] forced de-schedule.
- **OCB_CLNUP** (025570): walk per-CPU message region (base from ADR_MESS/SAMSON_CPU), unlink/clear link words, MSG_CCMOVE. [I] flush this CPU's shared-message entries.

## 5. Outbound single-word kick writes [V sites, I meaning]

Pattern: `SC12 := 100000(bit15=single-word) | function code | CPU/level field (bits 8-12)` -> ACCP_WRITE.

| Site | Word | Context |
|---|---|---|
| SENKICK 025142 | `(target & mask) \| 100102` | CALL_STA_CPU kicks an idle CPU |
| 025006 | `(data & 037400 IX/8) \| 100102` | kick after swap scheduling |
| SEND_14 tail 005245 | `(dest masked) \| 100101` | end of NK SEND — kick destination |
| GIVEINT1 025441 | `(SC10 & 037400 IX/8) \| 100001` | "give interrupt" (from MSG_QUEUE_END) |

[I] codes: 001 = interrupt, 101/102 = kick variants matching inbound kicks 1/2.

## 6. Command/response channel (ACCP_RDYW users) [V]

- **SYS_READ** (017111): command 1 -> 3x ACCP_READ into system-parameter area. (= manual ch. 5.3.7 command 1 / LSYSPAR fetch.)
- **ASTS_BADAP** (017121): command 2 -> ASTS + BADAP status words. (= command 2, used by memory-error handler TRAP_GEN3B.)
- **CPU_READ** (017130): command 3 -> CPU model word -> CPU_MODEL00-17 decode -> CPUSAVE/VERSIONxx -> CPU_AVAIL/CPU_UNAVA -> CPU_MESSAGE.
- **SYS_REDEF** (016642, async-trap vector 1): re-read system parameters on ACCP request.
- **TRAP_ATRP** (016612): async-trap word subcode 0-7 via TRAP_ATRPV (016623): 1 = redefine sysparams, 2 = debug stop, 3 = debug start, others NOTREC. (= manual AMICTRAP commands 1-3, offset naming differs.)

## 7. Outbound multi-word OCB messages (TRAP_OCBM family) [V structure]

**TRAP_OCBM** (016727; from TRAP_NOTREC and CPU_MESSAGE): header word `SC3|100060`, route field `SC4 := SC3 & 037400 IX/8`, `SC4|BM02`, then per-type payload via TRAPOCB00 table (16 entries -> OCB00/01/03/07/12/20), helpers **SEND_MSG4** (32-bit value as two words) / **SEND_MSG2** (16-bit as two byte-chunks), all appended with ACCP_XWRITE into the shared message region; terminator `SC3|100040` (bit 15 = last word). **TRAP_OCBM98** emits a full crash report: process no, P (twice), register dump — matching the MP error record 5OMBREAD parses on the SINTRAN side. Message codes seen: 202B CPU available, 203B CPU unavailable, 204B-210B error/not-recognised reports (every unrecognised kick/message is reported back over the octobus).

**CPU_MESSAGE** (017301): boot-time "CPU available, model X version Y" report -> TRAP_OCBM -> UNLOCK_QUE -> IDLE. This is the message that makes SINTRAN's 5OMBREAD set `5ALIVE` ("I'm present").

## 8. Boot-time AOBASR use [V]

- **LOOK_HARD_1** (017472): STOP; read AOBASR; read AOB -> DAC,DPA (hardware-configuration word from the ACCP at power-up).
- **LOOK_SRF_1** (017657): AOBASR/AOB pairs -> DPA and RFA1; loads 0120 register-file entries; then `AIB := 0` as "SRF load complete" acknowledge.
- **READ_DEV00/WRIT_DEV00** (010540/010567): non-blocking "external device 0" programmed I/O port onto AOB/AIB for macro-level I/O.

## 9. NK nucleus (B30/work-mode-500 only) [V]

Macro-instruction handlers SEND (005057), RECVE (005247), GETINF (005315), WHOLE/MHOLE (004640+) implement the multiprocessor message nucleus. They operate on the **shared message region** (base constant START_MESS = 020000 physical) — not on AIB/AOB — using LOCK_DH/UNLOCK_DH spin-locks (via `SPEC,MOD` bits, timeout -> NKTIMEOUT) and NK_TRACE logging; their only ACCP contacts are SCAN_ACCP polling inside spin loops and the final SENKICK/ACCP_WRITE kick to wake the destination CPU. Status codes returned in X1: NKSET_IOV=101002, NKNOMESS=101003, NKILLNO=101004, NKSOUR_RANGE=101006, NKPROTVIOL=101014, NKTIMEOUT=101023, NKNOTSTART=101025, NKPORTCLOSED=101032, NKILLEG=101033. (Compare DOMINO/NUCLEUS guide: nke_KICKLOCK=101042B.)

**A30 (WM406) differences [V]**: identical ACCP primitive/command/trap layer (at shifted addresses, e.g. ACCP_READ at 015405); the NK nucleus, kick handlers 3-6, MHOLE, LOCK_DH, SENKICK are **absent** — SEND/RECVE/GETINF/WHOLE vector to ILLEG and kicks 3-6 to "not recognised".

## 10a. LOSSLESS RE-VERIFICATION SWEEP (2026-07-17)

This catalog was written 2026-07-15/16 against the pre-a91dff4 listings, which hid memory-op
direction, ALU false paths, `C,SEQ`, AA/AB/ORCON and EA-save tokens. Full re-read of every
routine above against the lossless `MICRO-5800-B30.md`. Decode key used (calibrated in
MAILBOX-MICROCODE-PSEUDOCODE.md §3.10 + spin-loop proof): **`C,SEQ` on word N = branch
condition comes from word N−1's ALU result** (proven by the junk-ALU branch words in the
ACCP_READ/WRITE spins and MSG_NEXT's −1 test); memory op on word N uses word N−1's ADACT.

**HELD unchanged [V]:** §1 primitives (ACCP_READ bit-9 wait / ACCP_WRITE bit-10-clear wait /
ACCP_XWRITE = RF2D append, no AIB / ACCP_RDYW full sequence incl. bit-5 stash trigger);
§3 dispatch plumbing LC=6/7/8 → TRAP_VECT; §4 OCB_DECODE bit ladder (no-C→205, E→206,
M-drain-until-bit15→205) and the OCB_DEC_K 64-entry table exactly as listed (0→NOTREC,
1-2→ACTIVATE, 3→KICK03, 4-5→KICK05, 6→KICK06, 7-63→KICK64=UNLOCK+204); §5 outbound kick
words (100102 SENKICK/025006, 100101 SEND_14, 100001 GIVEINT1 — SARG values now rendered);
§6 command channel; §7 TRAPOCB00 16-entry table and SEND_MSG2/4 helpers; §9 NK layer.

**CORRECTED:**
1. **§2 AFLAG bit map — the four dispatch bits were each shifted one step.** Both chains
   (SCAN_ACCP 016554+ and ATRAP_CHK 016572+) test bit N on one word and branch on it from
   the NEXT word (C,SEQ = N−1). Corrected map, consistent across both chains AND with the
   RDYW bit-5 stash being dispatched to TRAP_ATRP1:
   | Bit | OLD (wrong) | **NEW [V]** |
   |---|---|---|
   | 5 (BM05) | OCB kick/msg | **async-trap word pending (→TRAP_OCBA/ATRP; RDYW stash trigger)** |
   | 6 (BM06) | async trap | **"other trap" (→TRAP_OTRP, NOTREC 210)** |
   | 11 (BM13) | other trap | **power-fail warning (→TRAP_PWF)** |
   | 12 (BM14) | power fail | **OCB kick/message pending (→TRAP_OCBAK/OMESS)** |
   Bits 9/10 unchanged (loop-polarity-proven). Bits 7/8 (TRAP_DFC/TRAP_NDF) not re-checked —
   same shift risk, re-verify before use [?]. Emulator impact: only a future
   real-microcode-execution mode presents AFLAG to microcode; the C#-servicer replacement
   path never exposes these bits — but fix any station code that models them.
2. **TRAP_OMESS1 routing (016413-16):** the branch tests whether the PROC0 cell (srf 2013)
   is ZERO — PROC0==0 → OCB_DECODE (microcode handles), PROC0 registered → OCTO_SOFT
   (deliver word to the software process-0 handler). NOT a word==id comparison. [V structure,
   D meaning]
3. **OCB_KICK03 = the CLRKICK cache-clear protocol** (matches SINTRAN's X5CLR/SWPCLRMASK
   write + CLRKICK send [NPL-V]): read X5CLR (extension block word 0o10, ORCON 0x10) →
   MSG_CLEAR_1 (cache clear by mask) → write X5CLR back with bit 15 cleared (ack) → if
   original bit 15 set, spin OCB_WAITSEX on GLOBAL header word 0o24 (byte 0x28) until zero
   (SCAN_ACCP each pass) → resume EXECUTE if a process is loaded, else KICK06. The old
   "definition word / write into START_MESS region" description was wrong.
4. **OCB_CLNUP (025570) un-claims the in-progress message, it does not walk the region:**
   DPA := current message (ADR_MESS); check 5CPUN@−6 vs this CPU; clear MSGME (srf 2021);
   MSG_CCMOVE; **write N5STA := 1** (back to MSGN500 — returns the message to the queue
   unanswered). Kicks 4/5/6 therefore requeue, not discard, in-flight work.
5. **TRAP_OCBM header word source identified:** 016727-730 loads SC3 := srf[2006] = the
   LSYSPAR word 1 = **5OMDNO<<8** — out-of-band OCB messages (201B-210B, CPU-available
   202B) are addressed to SINTRAN's receive OMD exactly like GIVEINT's interrupt word.
6. **OCB_MES_K fast path (016424-425):** the received word is XOR-compared against the
   constant 100501B first; exact match → direct jump to ACTIVATE, bypassing the table.
   The table dispatch (VECT := word & 077) only runs for non-100501B kick words.

## 10. Summary for the emulator (ACCP contract as the microcode sees it) [I]

1. Registers: AIB, AOB, AOBASR (boot only), AFLAG (bit map §2).
2. ACCP->CPU: word streams via AOB; bit 15 marks the final word; kicks are single words with kick number in bits 0-5; async-trap words carry subcode 0-7.
3. CPU->ACCP via AIB: command numbers 1/2/3 (answers on AOB, no ATRAP), kick words `100001|level` / `100101|dest` / `100102|cpu`, boot acknowledge 0.
4. Big messages travel through shared memory (base 020000), not through AIB — the ACCP is expected to fetch/deposit them there.
