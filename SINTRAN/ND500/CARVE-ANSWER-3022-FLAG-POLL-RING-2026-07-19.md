# CARVE ANSWER: 3022 bundle — RFLAG/SFLAG, background poll block, LAST-N500-MSG ring

**Full path:** `SINTRAN/ND500/CARVE-ANSWER-3022-FLAG-POLL-RING-2026-07-19.md`
**For:** the 3022 bus-interface team. **From:** SINTRAN carving/RE. **Date:** 2026-07-19.
**Discipline:** byte-verified only; segment+octal (SINTRAN) / HEX (Ghidra); NPL is a different revision
(logic only, never authority); anything unresolved is marked **ASSUMPTION**, not guessed. The emulator's
fabricated "TAG protocol" is **not** used or confirmed anywhere below.

## Grades
**[BYTES]** read from a carved `.bin` or a live harness capture · **[SYMBOL]** from a real symbol
artifact · **[NPL]** from NPL source (different revision — logic only) · **[INFER/ASSUMPTION]** reasoned,
not evidence.

## Evidence sources used
- Caller: `SINTRAN/ND500/nd-500-mon/mon60-callers/100B-RFLAG/`, `101B-SPFLAG/` (bytes from
  `nd-500-mon-j04.prog.asm`).
- Worker dispatch: `.../re/mon-analysis/60B-N500M/60B-5IFUNC-dispatch-table.md`,
  `60B-100B-FLAGS/README.md`.
- Server table: `.../re/ND500-SYSTEM-MONITOR/FUNCS-dispatch-table.md` (dumped from `030-S3SM5.bin`).
- Handler body: `SINTRAN/NPL-SOURCE/NPL/5P-P2-MON60.NPL` (RRFLAG/WWFLAG @032453B; FT500/FF500 @140-147).
- Poll/watchdog: `SINTRAN/NPL-SOURCE/NPL/RP-P2-N500.NPL` (3RMICV build @282/384/820-822; N500TMR @305-347).
- Message layout: `ND500-MAILBOX-MESSAGE-CATALOG.md` §1-2; `swapper/N500-SYMBOLS.SYMB` (L07).
- Live seed: `ND500-F6-MESSAGE-RING-RAW-CARVE-SEED-2026-07-19.md`,
  `ND500-BUS-INTERFACE-COMMAND-LADDER-ANALYSIS-2026-07-19.md`.

---

## (a) RFLAG=100B / SFLAG=101B — DO THEY CROSS THE 3022? **NO.**

**Verdict: neither RFLAG (100B) nor SFLAG (101B) crosses the 3022 bus.** They read/write a flag word
in the target ND-500 process's **data segment**, which SINTRAN maps into ND-100 addressing via
`M1MEXY` and touches with ordinary memory instructions. There is **no IOXT to the 3022 registers, no
`LCON5:=5` ACTIVATE, no MSGHDR/PROCMSG, no MICFU** — nothing on the bus. This is a byte-anchored
confirmation of the command-ladder observation (GET/SET-FLAG produced no command-specific 3022 traffic;
the 17-event block was the background watchdog poll, question (b)).

### Byte chain

1. **Caller [BYTES]** — `nd-500-mon-j04.prog`, main interpreter `002662`:
   - RFLAG: thunk `146616 = SAA 100` → `MON 60` at call site `005264`. Two params:
     `STA ,X 6 = &(B-125)` (process number), `STA ,X 7 = &(B-127)` (flag word; preloaded `LDD 174`
     @005253, **read back** on success `005266 LDD ,B -127`).
   - SPFLAG: thunk `146621 = SAA 101` → `MON 60` at `005223`. Two params from evaluated command-line
     operands: `,X 6 = &(B-125)`, `,X 7 = &(B-127)`.
   - Contract [BYTES]: `A`=subfunction code via thunk `SAA`; param1 @gateway `,X 6`, param2 @`,X 7`;
     callsite+1 = error, callsite+2 = success.

2. **Worker dispatch [BYTES structure / NPL name]** — `5IFUNC[100B]=RRFLAG`, `5IFUNC[101B]=WWFLAG`
   (`5P-P2-MON60.NPL:1328`). The L07 `050-S3I5PIT` dispatcher @`030416B` range-checks to `177B` and
   has the `145B` valid/illegal boundary — structure byte-confirmed.

3. **Server side proves no bus op [BYTES]** — `FUNCS[100B] = FUNCS[101B] = ERRFP = 141574B`
   (dumped from `030-S3SM5.bin`, base `40000B`). `ERRFP` = "not serviced here"; the table's own note:
   *codes pointing at ERRFP are handled entirely ND-100-side and have no ND-500-side operation.* So the
   MON 60 FLAGS path never reaches `FPT2ENTRY`/`5FP2E`, never builds a 5MPM message, never drives the bus.

4. **What the handler actually does [NPL]** — `RRFLAG/WWFLAG` @`5P-P2-MON60.NPL:1516-1538`:
   ```
   IF 5D12 = -1  THEN X := 5PRDESCR                 % -1 = own process
   ELSE validate 5SWPROC < procno <= MX5PROCS, RTRES != 0; X := actual process descriptor
   X := X.MESSBUFF; *SENDE@3 LDATX
   A - 5SWPROC*2 + "F5DSG" =: T; CALL M1MEXY; T =: 5OLDSEG   % MAP the ND-500 DATA SEGMENT
   IF 5FUNCTION = 5RFLAG THEN  AD := "FF500".DS0 =: 5DD2      % READ  flag word FROM N500
   ELSE  (auth guard: only RT-prog or user SYSTEM may write another proc)
         AD := 5DD2 =: "FT500".DS0                            % WRITE flag word TO N500
   FI; T := 5OLDSEG; CALL M1MEXY                              % restore caller's segments
   IF 5RFLAG THEN X := 5P2; AD := 5DD2; CALL STDS0 FI         % copy flags to user param2
   GO FAR 5OKRET
   ```
   The two flag words [SYMBOL/NPL, `5P-P2-MON60.NPL:141-146`]:
   `DASEGSTART = 166000B` (start of ND-500 data segment), `FT500 = 166002B` (flag-word **to** N500,
   written by SFLAG), `FF500 = 166004B` (flag-word **from** N500, read by RFLAG). These are **logical
   addresses inside the process's data segment**, reached only after `M1MEXY` maps that segment; `DS0`
   is data-segment-relative addressing. No bus register, no activate.

### Parameter block (for the emulator)
| Subfn | A (thunk) | param1 @`,X 6` | param2 @`,X 7` | 5IFUNC / FUNCS |
|---|---|---|---|---|
| RFLAG | `100B` `SAA 100` | ptr → process number (`-1`=own) | ptr → flag word (server writes; caller reads back) | `RRFLAG` / **ERRFP** |
| SFLAG | `101B` `SAA 101` | ptr → process number (`-1`=own) | ptr → flag value to write | `WWFLAG` / **ERRFP** |

### Emulator test-vector (a)
- Drive `MON 60` with `A=0o100`, param1→word=`-1`, param2→a flag cell. **Expected bus trace: EMPTY** —
  no `LoadMarX2`, no `LCON5:=5 ACTIVATE`, no `MSGHDR`/`PROCMSG` for this command. The only bus events
  that may appear in the window are the incidental watchdog poll (b), byte-identical regardless of
  RFLAG/SFLAG.
- Functionally, the value comes from ND-500 **data-segment memory** at logical `FF500=166004B` /
  `FT500=166002B` of the process's data segment (the process data segment physically lives in the
  multiport/5MPM region, reached by ND-100 memory access after the page-table map — **not** by the
  message protocol).

### Divergence vs current emulator
- The emulator returns `GET-FLAG` = `0B` with no bus traffic — **bus behaviour is correct** (there is
  no round-trip to model). The gap is only that the flag value is not backed by a real per-process
  data-segment cell; with the ND-500 stopped and no data segment allocated, `0` is defensible. If/when
  the emulator maps a process data segment, RFLAG must read `data_seg + 166004B` and SFLAG write
  `data_seg + 166002B`; SFLAG on another process must enforce the RT-program / user-SYSTEM guard
  (`EENAUTHORISED`).
- **[ASSUMPTION]** L07 byte body of `RRFLAG/WWFLAG` in `050-S3I5PIT` is not yet located (README:
  "L07 body loc pending"); the read/write mechanism above is NPL + the ERRFP byte fact, not a byte
  disassembly of the L07 handler. It is consistent with all bytes we have but is marked accordingly.

---

## (b) The background poll — ReadMicroVersion = the **WATCHDOG (3RMICV)**. Site-B decode CONFIRMED.

**What issues it [NPL]:** the ND-500 **watchdog / time-out** machinery in `RP-P2-N500.NPL`. It is a
timer-driven poll on the monitor level, **not** per monitor command and **not** an RT busy-loop.

- **Build (RP:282, RP:384):** `X:=WATCHDOG; T:=5MBBANK; 3RMICV; *MICFU@3 STATX` — set `MICFU=3RMICV(=1)`
  in the WATCHDOG buffer; `MSGN500; CALL WN5STATUS` (N5STA:=1); `CALL ITO500XQ` (queue to the ND-500
  exec queue); `X=:TMRXQ; LTTMR=:TMR` (arm the timer).
- **XMSINIT (RP:820-822):** `A:=-1; *SENDE@3 STATX` ("SET SENDER=-1 IN WATCHDOG MESS"); `3RMICV;
  *MICFU@3 STATX`. So the watchdog buffer is permanently stamped **SENDE=-1, MICFU=3RMICV=1**.
- **N500TMR (RP:305-347, "called in IOF"):** on each timer tick, if the outstanding timer item is the
  WATCHDOG it reads the watchdog's `N5STA` — if **not** `ANSWER(3)` yet → `N5TIMOUT`/`RSTARTALL`
  (master-clear); if `ANSWER(3)` → **re-arm** `LTTMR=:TMR` and re-send (`XLOWACT500`). Self-perpetuating.
- **Cadence [SYMBOL]:** re-arm interval = `LTTMR = 000023B` (= 19 dec) time units (`TTMR = 177774B` = -4
  is the short/error variant). The wall-clock period depends on SINTRAN's basic time unit —
  **[ASSUMPTION]** LTTMR is the tick count, the absolute seconds are not pinned from bytes. The point:
  it is a **fixed-interval watchdog timer**, which is exactly why it lands incidentally inside random
  command capture windows (LIST-ACTIVE-PROCESSES, GET-FLAG) and is byte-identical across them.

**The MESSAGE BLOCK (site B = the WATCHDOG buffer) — architect decode CONFIRMED byte-for-byte.**
Capture `0x424130` AFTER: `FF FF FF FF 00 03 FF FF 00 00 00 00 00 01 2E 9A`. As 16-bit big-endian words
from the message base, mapped to the catalog header layout (`N500-SYMBOLS`):

| word | byte off | value | field | meaning |
|---|---|---|---|---|
| 0-1 | 0x424130 | `FFFF FFFF` | LINK/LINK2 | queue link = -1 |
| 2 | 0x424134 | `0003` | **N5STA** | **ANSWER(3)** |
| 3 | 0x424136 | `FFFF` | **SENDE** | **-1 = WATCHDOG sender** (RP:821) |
| 4 | 0x424138 | `0000` | X5CPU | — |
| 5 | 0x42413A | `0000` | X5ACT | — |
| 6 | 0x42413C | `0001` | **MICFU** | **3RMICV(1) = ReadMicroVersion** |
| 7 | 0x42413E | `2E9A` | data | `0x2E9A = 11930 = 027232B` = **microprogram version** |

- **CONFIRMED, and strengthened:** the architect's `0x2E9A=11930=MicroVersion`, `00 03=N5STA ANSWER(3)`
  are exactly right. The extra proof is **word 3 `SENDE=0xFFFF=-1`**, the XMSINIT watchdog marker — this
  is specifically **the WATCHDOG (3RMICV) answer buffer**, not merely "a ReadMicroVersion mailbox," and
  categorically **not the ring**. `3RMICV` = the watchdog per the catalog (§2) and RP source.
- Version cross-check: `027232B = 11930 = 0x2E9A` matches the ND-5800/B30 microcode `VERSION` word
  (catalog §7) and the monitor's `Micro program 11930` console line.

**"ResidentRead" (the other periodic type):** in the site-A AFTER capture the buffer was reused with
`MICFU = 0x0008 = 10B = 3RMED` (resident/absolute-memory read) and got `N5STA = 0x0004 = 5ERANSWER`.
`3RMED` is a memory read (DMA/absolute); it is **triggered by commands that read ND-500 memory** (memory
examine, the config/sizing pattern test, LIST-TABLE's own reads) — **not** the fixed watchdog timer.
Its exact per-command trigger is **[OPEN]** (not carved this pass).

### Emulator test-vector (b)
- Fire an activate whose MAR points at the watchdog buffer; the servicer must answer:
  `N5STA(word2):=0x0003`, leave `SENDE(word3)=0xFFFF`, `MICFU(word6)=0x0001`, and write the version at
  `word7 = 0x2E9A` (source it from the loaded control-store image, not a hardcode — classic-500 images
  must self-report their own 105xx/106xx; see status §7). Poll expected on the LTTMR interval,
  unsolicited, independent of any monitor command.

### Divergence vs current emulator
- The emulator already completes this round-trip cleanly (`PROCMSG processed=1`,
  `lastMICFU=ReadMicroVersion`, `ND500Finished`) — transport is correct. Two notes: (1) it decodes the
  poll generically as "ReadMicroVersion" without checking `SENDE=-1`, so it does not *label* it the
  watchdog — cosmetic. (2) `3RMED`/"ResidentRead" is answered `5ERANSWER(4)` in the site-A AFTER
  capture; whether that rejection is intended is **[OPEN]** and worth a look (it may be the same
  ND-500-memory-not-present situation as the sizing probe, in which case 5ERANSWER is fine).

---

## (c) LAST-N500-MSG ring — site A is a MESSAGE BUFFER, **not the ring**. Architect "03→04 index" REFUTED.

**Finding: site A (`0x420E30`) is a 128-word (200B) ND-500 message buffer, structurally identical to the
site-B watchdog buffer but for a real process (SENDE=1, X5CPU=1). It is NOT the LAST-N500-MSG ring, and
the `0x420E35` "03→04" is the N5STA status word flipping ANSWER→5ERANSWER on a REUSED buffer, not a ring
head/tail index.**

Site A header decode [BYTES], BEFORE `FF FF FF FF 00 03 00 01 00 01 00 00 00 01 2E 9A`:

| word | byte off | BEFORE | AFTER | field | meaning |
|---|---|---|---|---|---|
| 0-1 | 0x420E30 | `FFFF FFFF` | `FFFF FFFF` | LINK | -1 |
| 2 | 0x420E34 | `0003` | `0004` | **N5STA** | ANSWER(3) → **5ERANSWER(4)** |
| 3 | 0x420E36 | `0001` | `0001` | SENDE | sender = process 1 (**not** -1 → not watchdog) |
| 4 | 0x420E38 | `0001` | `0000` | X5CPU | CPU 1 |
| 5 | 0x420E3A | `0000` | `0000` | X5ACT | — |
| 6 | 0x420E3C | `0001` | `0008` | **MICFU** | 3RMICV(1) → **3RMED(10B) ResidentRead** |
| 7 | 0x420E3E | `2E9A` | `0801`… | data | version 11930 → new payload |

- So across `LIST-TABLE`, this **one** buffer was reused: a ReadMicroVersion answer (MICFU=1, N5STA=3)
  was overwritten by a ResidentRead (MICFU=8=10B) that got 5ERANSWER (N5STA=4). The architect's
  candidate "ring index 03→04 at 0x420E35" is the **low byte of N5STA** — a message-status change, not
  an index advance. **Refuted.**
- The "incrementing 16-byte records `11 E6, 11 F4 … 11 FB`" at `0x420E40+` are inside the **128-word
  message DATA part** (offsets ≥8B of the same buffer; a 200B message spans `0x420E30–0x420F30`). They
  are message/descriptor payload (or stale content), **not** a ring of separate records. Note the stride
  is irregular at the ends (E67→EA7 is a clean 16B, but E4C, ECA, ED7 break it), which is inconsistent
  with a fixed-record ring and consistent with structured message data.
- The tail `00 01 2E 9A` appearing at **both** sites is simply the same `MICFU=1 / version=11930` answer
  written into two different message buffers (watchdog @site B, process-1 MESSBUFF @site A) — not a
  shared ring.

**Where is the actual LAST-N500-MSG ring? [UNRESOLVED / ASSUMPTION].** I could **not** byte-locate a
"last 64 messages to ND-500" ring:
- No symbol for it in the L07 tables or the NPL tree (searched `LAST-N500`, `L5MSG`, `LI5MSG`,
  `LASTMSG`, "last 64" — zero hits). The 5IFUNC table has **no** last-message-ring subfunction (the
  nearest listing subfunctions are `133B ILI5EXQ` LIST-EXECUTION-QUEUE and `150B ILI5TQU`
  LIST-TIME-QUEUE — neither is this ring).
- `LIST-TABLE` is an **ND-500-MON:PROG monitor command** (ND-60.136 §8.10.9.1). **[ASSUMPTION]** the
  "last 64 messages" ring is a log the monitor program keeps in **its own program memory** as it sends
  MON 60 messages, rendered internally by `LIST-TABLE` — i.e. it is not a structure in the 5MPM window
  at all. Site A is therefore the wrong place to look for it; the raw MPM does not evidence it.
- **Record size / capacity(64?) / head-tail pointers: UNRESOLVED.** Not guessed. To settle it, carve
  the `LIST-TABLE` handler inside `ND-500-MON:PROG` (find the "LAST-N500-MSG" table descriptor in the
  program's own DSEG) — that binary, not the 5MPM window, is where this lives.

**Why the terminal render was empty.** `LIST-TABLE LAST-N500-MSG` printed only `> Loading Swapper` and
returned. **[ASSUMPTION, consistent with all evidence]:** with no ND-500 microengine executing real
work, essentially only watchdog/version and a few memory-probe messages have been "sent," so the
monitor's ring log is empty/near-empty; the command also tripped a control-store/swapper-load attempt
(the "> Loading Swapper" line) rather than dumping records. The ring populates only when the monitor
actually drives the ND-500. This is not a 5MPM-window artifact.

### Emulator test-vector (c)
- Do **not** assert the emulator's captured 5MPM writes against a "site-A ring." The correct
  cross-check target is the ND-500-MON program's internal log, which the harness cannot see from the
  MPM window. If a ring cross-check is wanted, it must come from disassembling `LIST-TABLE` in
  `ND-500-MON:PROG` and reading the ring out of the program's DSEG — flagged as the next carve.
- Byte-level: treat `0x420E30–0x420F30` as **one** 200B process-1 message buffer (negative-offset
  header at `MAGNO=-3` etc.), reused per poll; do not model it as 64×16B records.

### Divergence vs current emulator
- The emulator's `SnapshotMpmAccess()` captures message buffers correctly, but there is **no ring** in
  those buffers to match against `LIST-TABLE LAST-N500-MSG`. The F6 "strongest cross-check" in the
  expectation tables (`DOMAIN-HANDLING-TWO-INTERFACE-EXPECTATION-TABLES`) rests on a ring format that is
  **[TC]/unlocated**; it cannot be closed from the MPM window and should be re-scoped to the monitor
  program's own memory.

---

## Bottom line for the bus team
1. **(a)** RFLAG/SFLAG are **ND-100-side memory operations** on the process data segment
   (`FF500=166004B` / `FT500=166002B`, mapped by `M1MEXY`); **`FUNCS[100/101]=ERRFP=141574B` [BYTES]**
   proves there is **no ND-500-side op and no 3022 crossing**. Emulator "no bus traffic" is correct.
2. **(b)** The periodic poll is the **watchdog `3RMICV` (MICFU=1, SENDE=-1)**, timer-armed by `LTTMR`,
   re-sent on each `ANSWER`. **Site-B decode CONFIRMED byte-for-byte** (and it is the watchdog buffer,
   not the ring). Answer with `N5STA=3`, version `0x2E9A` at word 7.
3. **(c)** Site A is a **reused 200B message buffer**, not the LAST-N500-MSG ring; the `03→04` is
   N5STA, not an index. The ring itself is **not in the 5MPM window** and is **[UNRESOLVED]** — it lives
   in `ND-500-MON:PROG` memory; carve `LIST-TABLE` there to get the format.
