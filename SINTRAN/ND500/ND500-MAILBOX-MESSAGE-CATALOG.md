# ND-500 <-> SINTRAN 5MPM Mailbox Message Catalog

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-MAILBOX-MESSAGE-CATALOG.md`
**Compiled 2026-07-16** from a full-source sweep (NPL driver docs, system-monitor carve, swapper RE,
symbol tables, manuals). **Purpose: the specification for reimplementing the ND-500 microcode's
mailbox servicing as C# logic** in the RetroCore emulator (the emulated CpuND500 is a MACRO-
instruction emulator; microcode behavior must be code). Companion:
`ND500-WHO-ANSWERS-THE-MAILBOX.md` (who services it), `ND500-CS-LOAD-TRACE-FINDINGS-2026-07-16.md`.

**Scope:** classic ND-500 (3022/5015, DMA/MAR). Evidence grades per repo convention:
BYTES / SYMBOL / NPL (different revision - logic only) / MANUAL / INFERRED.

**Two structural cautions (load-bearing):**
- The authoritative field-offset source is `swapper\N500-SYMBOLS.SYMB` (SYMBOL) cross-checked with
  `SINTRAN\NPL-SOURCE\SYMBOLS\{L07,M06}\N500-SYMBOLS.SYMB.TXT` + NPL `AAX <sym>; LDATX` usage.
  Flat, alphabetical, 5-char truncated - a value is proven, "is a message offset" is INFERRED from
  usage. Validated via known slots STOPR=11, TRAPN=16.
- **`MP-P2-N500.md` section 7.6 "Message Buffer Fields" is a DIFFERENT, CONFLICTING table**
  (5MSFL=0, XADPR=1, FUNCV=2, ... N5STA=15) - most plausibly the ND-100 process-descriptor/MESSBUFF
  working layout, NOT the 5MPM block. **Do NOT implement from 7.6.**

## 1. Message header - field table (octal word offsets), per-direction overlay

Block lives in ND-100 physical memory, bank `5MBBANK` (XMSINIT: `5FPMAILBOX =: D:=0; AD SH 12`).
Message size `55MESSIZE = 200B` = 128 words (SYMBOL). Header = 6 words + data part (MANUAL
ND-05.012.01 sec 13). MAR carries the message's ND-100 WORD address (live-proven).

| Off | Symbol | Meaning | Grade |
|---|---|---|---|
| -1 | `5MSFL` | flags: `5IEXQUEUE`=bit15, `5SYSRES`=14, `5CPUBOUND`=13, + `5IBRK`, `52ESCSET`, `5ITMQUEUE` | SYMBOL+NPL |
| 0-1 | `LINK`/`LINK2` | forward queue link (double word, `LINK@3`) | SYMBOL+NPL |
| 2 | `N5STA` | status word (values sec 3.4); NPL reads it TWICE with cache-flush (`BSET BCM 120 DX`, "fool the cache", MP:1874) | SYMBOL+NPL |
| 3 | `SENDE` | sender; **watchdog = -1** (RP:821) | SYMBOL+NPL |
| 4 | `X5CPU` | receiver/CPU; precondition `= MPACTIVE` (MP:3061) | SYMBOL+NPL |
| 5 | `X5ACT` | size/activation; zeroed in ACT51 (MP:3027) | SYMBOL+NPL |
| 6 | `MICFU` | **the command the ND-500 executes** (sec 2) | SYMBOL+NPL |
| 7 | `N500A` | ND-500 logical address (transfer descriptor); `MESSB`=7 in the mailbox-header overlay | SYMBOL+NPL |
| 11 | `N100A`/`STOPR`/`ACPRO`/`KFLIP` | **overlay**: transfer=ND-100 phys addr; answer=stop reason; also actual-process / error-flag (MONICO) | SYMBOL+NPL, overlay INFERRED |
| 12 | `NUMPA` | number of params / write-back mask (MONICO clears; 5ACTSWAPPER writes 6; MCHANDLE write-back count) | SYMBOL+NPL |
| 13 | `NRBYT`/`MCNO`/`FUNCV` | **overlay**: transfer=byte count; MON-call answer=mon-call number; restart=function return value (MONICO) | SYMBOL+NPL |
| 14 | `MSWMC` | swapper-monitor-call subfield | SYMBOL |
| 16 | `TRAPN` | trap number (fault activation reads it, MP:762) | SYMBOL+NPL |
| 22 | `MAILINK` | exec-queue head (datafield field); `LEXQUEUE`=14 = queue length | SYMBOL |
| 37 | `SMCNO` | saved mon-call number (MCHANDLE copies MCNO here, MP:1302) | SYMBOL+NPL |
| 57 | `CNTXP` | per-message context page | SYMBOL |
| 140 | `ABUFA` | auxiliary buffer address | SYMBOL |
| 143 | `SPFLA` | special flag: nonzero -> DECOMESS jumps to that ROUTINE ADDRESS (dispatch override) | SYMBOL+NPL |
| 144 | `XADPR` | process-descriptor address (MCCO loads proc desc from it, CC:827) | SYMBOL+NPL |
| 147 | `PLINK` | backward queue link (priority double-linked list) | SYMBOL+NPL |

Housekeeping negatives (low grade, SYMBOL): `5PRIO=-5`, `5CPUN=-6`, `500TU=-10` (CPU time used).
**Offsets 11 and 13 are single slots with direction/type-dependent meaning - interpret per current
MICFU/N5STA.** (Not a contradiction: flat symbol table holds all aliases at the same value.)

## 2. ND-100 -> ND-500 message types (MICFU codes, all SYMBOL-verified)

| MICFU | Symbol | Purpose | Built at (NPL) |
|---|---|---|---|
| 1 | `3RMICV` | read microprogram version - **watchdog** | RP:282/384/822 |
| 5 | `3SWMESS` | message to swapper (SWFUN field at off 7) | MP:2876 |
| 23B | `3START` | start process | MP:2991 |
| 24B | `3MONCO` | restart after monitor call | MP:808, CC:824 (MONICO), MP:2887 |
| 25B | `3TRACO` | trap continue | MP:808 |
| 26B | `3WMONCO` | wait monitor call | MP:808 |
| 27B | `3FITRNSF` | file transfer (uses N500A/N100A/NRBYT descriptor) | MP:2991 |
| 34B | `3MONO` | mon-call variant - **semantics UNKNOWN** | (symbol only) |
| 44B | `3RPREG` | read P register - **histogram** (HIMESS) | RP:803/811 |
| 46B | `33MON` | mon-call variant - **semantics UNKNOWN** | (symbol only) |

Also referenced, numeric value unresolved: `3RMED`/`3WMEP` (MP:839 error-answer decode).

**General contract** (MANUAL ND-05.012.01 sec 13, DERIVED - microcode internals are in NO SINTRAN
source): activate -> leave IDLE -> DMA-fetch message at MAR -> `N5STA:=WAITING(2)` -> execute MICFU
-> write answer fields + `N5STA:=ANSWER(3)/5ERANSWER(4)` (preserve 160000B power-fail bits) ->
finished + level 12 (gated by CONTROL bit 0).

Per-type behavior:
- **3START**: run the process from its saved P until it stops -> answer = a stop message (sec 3).
- **3MONCO**: consume MONICO's write-back (`FUNCV`(13)=return value, `KFLIP`(11)=error flag,
  `NUMPA`(12)=0) and resume the process past its MON call. THE reactivation of every round trip.
- **3TRACO / 3WMONCO**: trap-continue / wait-variant; DECOMESS treats all four alike for STOPR dispatch.
- **3FITRNSF**: bulk transfer via N500A/N100A/NRBYT; completion may stop with `5FMOCALL`.
- **3RMICV** (watchdog, SENDE=-1): return microprogram version, `ANSWER(3)`. ND-100 side re-arms
  the timeout (`IFM500XQ`, reset TMRXQ/TMR) - the answer just proves the microcode is alive.
- **3RPREG** (HIMESS): sample ND-500 P register -> `ANSWER(3)` -> CHN5STATUS calls HISTSAMPLE.

**Answer write-back rule (what the emulated microcode must write):** `N5STA:=ANSWER(3)`;
`STOPR`(11) = stop reason; MON call: `MCNO`(13) = MON number + params in the data part; trap:
`TRAPN`(16) + `STOPR:=TRAPCODE(2)`; failure: `5ERANSWER(4)`. (NPL/DERIVED - constrained by what
SINTRAN reads, sec 3.)

## 3. ND-500 -> ND-100 (microcode-written stop/answer messages)

### 3.1 STOPR values (offset 11)
`MOCALL=1` (monitor call), `TRAPCODE=2` (trap), `5FMOCALL=3` (file-transfer MON call) - SYMBOL.
Others -> "restart ND-100 process". `TPSTRA=65` UNVERIFIED.

### 3.2 The level-12 ISR chain (BYTES: 026-S3IMPIT @ 135010/135205/135361/137206)

**5STDRIV**: CPU alive check -> CLE5STATUS mask 177377 -> if `status /\ 720B != 0` error path
(5PFAIL->BHPFAIL+KPOWDOWN, 5DMAERR->N5DMAERR, else N5IERR; all -> N500ERR -> XRSTARTALL) ->
else walk exec queue from `MAILINK` via `LINK` until -1, per non-DUMMESS message call CHN5STATUS
-> XACT500 -> WT12.

**CHN5STATUS dispatch on N5STA** (VERIFIED): ANSWER+HIMESS -> HISTSAMPLE; ANSWER+WATCHDOG ->
re-arm timeout; ANSWER other -> DECOMESS; 5ERANSWER -> DECOERRMESS; >100B -> 5RRTWT (restart
ND-100 proc); **MSGN500/WAITING on the answer path -> XTER500 (terminate! inconsistency case)**.

**DECOMESS** (verbatim NPL logic): if `SPFLA`(143) nonzero -> `A=:P` (jump to that address).
Else read MICFU: if in {3MONCO,3TRACO,3START,3WMONCO} read STOPR: MOCALL/5FMOCALL -> MCHANDLE,
TRAPCODE -> TRAPDECODER, else 5RRTWT. Else 5RRTWT. Then NXTMSG.

**MCHANDLE/MCHANDEL** (X=msg, T=stop reason, B=CPU datafield): read `MCNO`(13) -> save to
`SMCNO`(37) -> special-case `347B -> 5SERV(050211B)` -> range `L12MIN=500B..L12MAX=523B` ->
MBSUSPROC -> GOSW (byte-proven table @137625B: 500 STAPR/SWITP, 501 NSTOP, 502 SWITP, 503 NINST,
**504 NOUTS=141027 (bytes; NOT OSTRS)**, 505 GERRC, 506 5SIBM, **507 SPRIO**, 510 SWMC,
511 DVIO/NOUTS, 512/513 A5XMS/B5XMS, 514 M5TMO, 515 5MTRA, 516-523 -> NORMM=137167 unimplemented).
Out of range -> NORMM -> 5RRTWT.

### 3.3 MONICO/MCCO restart write-back (CC:359-372, VERIFIED - the emulated microcode CONSUMES this)
```
OKMONICO: T:=0 / EMONICO: T:=1 (error)
MONICO: FUNCV(13) := function value; KFLIP(11) := error flag; NUMPA(12) := 0
        MICFU(6) := 3MONCO
MCCO:   N5STA(2) := MSGN500(1)   (WN5STATUS)
        proc-desc := [XADPR(144)]; PSTAT := 5ACTIVE (clear run status)
```
Trap-restart variant (CC:1036): reason -> `5ADP3`, `NUMPA:=4`, FUNCV/KFLIP cleared, MICFU:=3MONCO.

### 3.4 N5STA values
`0` free (INFERRED) | `1 MSGN500` | `2 WAITING` | `3 ANSWER` | `4 5ERANSWER` |
swapper states: `SWPWAIT=5, SWPPING=6, PSWWAIT=7, PSW1WAIT=15` | high bits 160000B = power-fail
flags, ALWAYS preserved (`A/\160000\/MSGN500`, MP:992) | `>100B` = restart-ND-100-process class.

## 4. The swapper message family

- **ND-100 -> swapper**: `SWMSG` buffer @110054B (SYMBOL). Admin fields: `SWPFU=101, RETP2=102,
  SWPST=103, HSWPI=104, SWPIN=105` (+ SPFLA/XADPR). 5ACTSWAPPER (MP:2851-2908): `HSWPI:=&MSGTOSW`,
  `SWPFU:=SWACTIVE` (state marker, NOT the function code), **`SWPST:=SWFUN`** (from MSGTOSW off 7,
  or TRAPN on the fault path) - **the swapper dispatches on SWPST** (earlier SWPFU guess REFUTED),
  `NUMPA:=6, FUNCV:=0, MICFU:=3MONCO` -> MCCO -> XACTRDY.
- **SWFUN namespace = MSW\*** (29 codes 0..34B, SYMBOL): MSWFI=0, MSWUF=1, MSWSO=2, MSWMI=3,
  MSWMD=4, MSWIN=5, MSWPO=6, **MSWSTART=7**, MSWFO=10, MSWIP=11, **MSWPFAULT=12**, MSWME=13,
  MSWMC=14, MSWSP=15, MSWSG=16, MSWIS=17, MSWRS=20, MSWWB=23, **MSWSWAIT=24**, MSWPR=33,
  **MSWDO=34**. Only the bold four have NPL comments; the rest are truncated - semantics OPEN.
- **The swapper's own 29-entry dispatch** (BYTES, `swapper\swapper-k01-handlers.md`): function code
  at DSEG 0x240B8, bound <=34B, jump table at DSEG 0x26198. Handlers: 0 free-slot, 1 release-WS,
  2 copy-desc, 3 release-range, 4 deep swap-in/out scan, 5 init/activate WS, 6/7/16/25/26/27 shared
  generic swap request (fixed sub-code 66B via MON 377B sub-fn 2), 8 connect/page-in (RPHS),
  9 alloc+link, 10 page-fault accounting, 11/12/20 NO-OP, 13 link desc, 14/15 attach/detach,
  17 reinit tables, 18 set config, 19 swap/fix by id, 21 set desc fields, 22 page-in WS,
  23 UNKNOWN, 24 create desc, 28 perform swap (MSWDO).
- **Swapper -> ND-100 = MON 377B (N5SWAP)**, decoded by SWPDECODER (MP:913): `SWPFU <= SWFMAX=6` ->
  GOSW: 0 ESWPFATAL, 1 LNEWSWAP(argc4), **2 LSWPAGE(argc7, disk transfer - dominant)**,
  3 LPRSUSPEND, 4 LALLOPAGE(argc6), 5 LDATREADY(argc3), 6 LCLTSB(argc2). `SWPFA=2047B` -> fatal.
  **Disk work runs on the ND-100** (RT prog 5SWAP/5SWRT: reserve proc 0, ABSLI MON 131, wait
  PSW1WAIT). Swap-wait FIFO: X5MXF/X5SWF/X5SWT/X5SWB in X500DF; SWPD4 drains at PSWWAIT.

## 5. Queue / special mechanics

- Insert/remove: `ITO500XQ` (set 5IEXQUEUE, priority-insert LINK/PLINK, LEXQUEUE++; semaphore
  must be held), `IFM500XQ` (reverse), `FR5TMQ` (time queue). ND-5000: ITOFIFOQ ring.
- Walk: `MAILINK` -> `LINK@3` until -1, physical reads via 5MBBANK.
- **`DUMMESS` is the ADDRESS of a sentinel message** (set in XMSINIT), not a constant; walkers skip
  it by address compare. (`7DUMM=30` is unrelated - do not conflate.)
- Watchdog: SENDE=-1, MICFU=3RMICV; its ANSWER re-arms TMRXQ/TMR. **The emulated microcode must
  complete every operation + release the lock in finite time or SINTRAN times out and master-clears.**
- XMSINIT builds: zeroed mailbox area (5NPMAILBOX pages), per-process message buffers
  (proc-desc MESSBUFF -> its message; 55MESSIZE=200B words each), HIMESS, WATCHDOG, SWMSG,
  DUMMESS sentinel, swap FIFO. Count is install-dependent, not fixed.
- ACT50 + preconditions, TERM5/X5MCST teardown: see bus reference sec 5/9 +
  `ND500-CS-LOAD-TRACE-FINDINGS-2026-07-16.md` (MAR = WORD address; strobes on IOX READ).
- SLOCK/SUNLOCK semaphore = ND-5000 only (no-ops on classic).

## 6. UNKNOWNS - carve before coding (no guessing)

1. `3MONO=34B`, `33MON=46B` microcode semantics.
2. **RESOLVED 2026-08-10 (`CARVE-ANSWER-RESULT-BLOCKS-2026-08-10.md`):** the "answer result block
   at 40B-47B" is NOT a message structure - the FUNCS `,X 40/41/43/46/47` stores are the MON 60
   **info block** parameter records `5DD1..5DD5` (values @ 40/43/46/51/54B) + `5P1..5P5` (user
   addresses @ 42/45/50/53/56B), base `B-11 = S500DF-ZPREG = 165777B` [V bytes + L07 SYMBOL].
   The per-command mailbox ANSWER offsets (what the emulated microcode must write) are in that
   carve doc: 16B/17B register value @ msg 10B-11B, examine answers @ msg 11B-12B, 3RMICV
   version @ msg 7 + CPU parameter @ msg 10B (cached to CPUDF[20B] by CSLOA's post-load verify).
3. `TPSTRA=65` stop reason - UNVERIFIED.
4. Exact microcode step sequence per MICFU - DERIVED only; the C# model is constrained by what
   SINTRAN reads back. Pull ND-05.012.01 sec 13 full text if more fidelity is needed.
5. ~24 MSW* swapper function codes lack names/semantics.
6. Swapper generic sub-code 66B (fn 6/7/16/25/26/27): meaning to the 5SWAP receiver UNKNOWN.
7. Swapper handler idx 23 target UNKNOWN.
8. `3RMED`/`3WMEP` numeric values unresolved.
9. Swapper physical WRITE-back primitive unknown (RPHS reads only; no WPHS found).
10. `MCHANDEL` vs `MCHANDLE` vs alleged alias `5MONICO` - alias uncited.
11. **MP-P2-N500.md section 7.6 field table CONFLICTS with the symbol layout - do not implement from it.**

## 7. Microcode-side cross-check (ND-5800 MICRO-5800-B30 disassembly, added 2026-07-16)

Source: `E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md` (full pseudo-C of the
mailbox servicer with per-microword evidence grading; [V]=verified in listing, [D]=derived,
[X]=cross). **Caution: that image is the ND-5800 (SAMSON/Octobus) revision** — the SINTRAN-facing
protocol matches, but the MICFU table has generational differences (below).

**Confirmations of this catalog from the actual microcode:**
- Read order in the fetch path matches the symbol layout exactly: N5STA check (must be 1) ->
  CPU-target check (matches X5CPU=MPACTIVE precondition) -> MICFU -> vectored dispatch [V].
- `N5STA := WAITING(2)` is written unconditionally BEFORE executing the MICFU [V value].
- Answer values: success `SC10 := 3` (ANSWER), illegal/reject `SC10 := 4` (5ERANSWER) [V].
- Queue walk terminates on link = **-1** [V] (matches "LINK until -1", sec 5).
- MICFU is range-checked to 0..77B (64-entry dispatch table); bit 15 of the MICFU halfword is
  stripped before dispatch (flag, not function number) [V/D].
- `3RPREG=44B` confirmed: handler MSG_HISTOG reads the ND-500 **P register** (`A,IAC,P`) into the
  answer [V] — exactly the HIMESS histogram sample.
- `3RMICV=1` (MSG_VERSRD) answers **two** halfwords: microcode version (0o27232 in B30) + a
  CPU-parameter halfword from microcode-internal state [V]. If DECOMESS/watchdog only reads one,
  the second is don't-care — carve to decide what the emulator must write.
  - Classic ND-500 oracle (Ronny, 2026-07-17): a genuine classic image (e.g. the incoming
    10509/10609 floppy) must SELF-REPORT its version 10509/10609 through READ MICRO PROGRAM
    VERSION — i.e. the version halfword comes FROM the loaded control-store image, so the
    emulator's 3RMICV answer must be sourced from the loaded image, not a constant.
- 3START(23B) and 3TRACO(25B) dispatch to the SAME handler [V]. 3MONCO(24B) delivers the restart
  value into the process's **X1 register** before resuming macro execution [V destination register;
  that it equals FUNCV(13) is the plausible but uncarved link]. 3WMONCO(26B) additionally
  block-copies answer data into process memory before resuming [V flow].

**Resolves (partially) UNKNOWN #1 — but on the 5800 image only:**
- `3MONO=34B` -> handler **MSG_IMEMRD**: instruction-memory block READ (word-aligned only, else
  5ERANSWER). NOT a mon-call variant.
- `33MON=46B` -> handler **MSG_DUDC**: dump-dirty + clear data cache. NOT a mon-call variant.
- (Whether the classic-500 microcode assigns 34B/46B the same way is UNVERIFIED.)

**Generational discrepancies (B30/5800 vs this catalog's classic-500 scope) — do not code either
way without deciding which machine is being emulated:**
- MICFU `05` (3SWMESS) -> **MSG_ILLEG** in B30. The 5800 swapper delivery evidently does not use
  MICFU 5 (SWMSG delivery on the 5800 may ride 3MONCO/SWPST only, per sec 4's 5ACTSWAPPER which
  already sets MICFU:=3MONCO).
- MICFU `27B` (3FITRNSF) -> **MSG_ILLEG** in B30.
- Extra B30 MICFUs with no entry in sec 2 (SINTRAN may never send them, or symbols not yet found):
  10/11B DMEMRD/DMEMWR (data-memory copy in process context), 12B CACHE, 13/14B RESIRD/RESIWR,
  22B STARTP0 (start swapper/process 0), 30/31B PHYSRD/PHYSWR, 35B IMEMWR, 42B PRT, 45B CLEAR,
  47B IDLE (drop process, go idle), 50-52B UNIX-500 context ops, 70-75B trace-memory family,
  76B CACI (cache inhibit), 77B LOOKSRF (debug SRF read).

**Doorbell (5800/Octobus form):** answer completion strobes ACCP command word `100401B`; activation
arrives as OCB command `100501B` (or vector codes 1/2). On the classic 500 these correspond to the
level-12/ident path and CONTROL-activate respectively [X].

### 7b. MON-call EXIT path — DECODED in the B30 microcode (2026-07-16)

Closes this catalog's "exact microcode step sequence" gap for the outbound direction. Full
pseudo-C: `MAILBOX-MICROCODE-PSEUDOCODE.md` sec 3.8. All [V] in the B30 listing unless noted.

1. **Recognition is a TRAP, not an opcode.** A `CALLG` whose target lies in the monitor entry
   table (manual encoding `EQU 37B9+n` — "segment 31") raises an instruction-fetch trap;
   trap code 6 -> CALL_MON (@003744), trap code 7 -> CALL_DOM (cross-domain). Dispatcher:
   TRAP_MONC @012740.
2. **MCNO = low halfword of the CALLG target address** (read from NPC) — i.e. the entry-table
   layout puts the MON number in the address low bits [V read; encoding cross [X]].
3. **Parameters:** per CALLG argument, the microcode fetches the operand specifier (G,OPS) and
   writes an **(address, value) word pair into the message data part** (CALL_MON8 loop
   @003772-004000). Arg count comes from the CALLG (held in LC), bounded <= 16.
4. **Output-call fast data:** for MCNO 504B/511B/512B (= GOSW NOUTS/DVIO/A5XMSG) the user's
   buffer is inline-copied into the message before stopping (CALL_5XX/CALL_5_MATCH) — the
   ND-100 gets the bytes without a second fetch.
5. **Header write — byte-matches this catalog's consecutive slots:** saved P (word, offset
   not yet pinned), then `STOPR(11) := MOCALL(1)`, `NUMPA(12) := argc`, `MCNO(13) := n`
   (CALL_MON9 @004007-004011), then `P := L` so 3MONCO resumes after the CALLG.
6. **Delivery: the stop is an ANSWER to the process's OWN activation message** (address kept
   in microcode state since the fetch): `N5STA := ANSWER(3)` + doorbell (CALL_END9 ->
   MSG_END0). **MICFU is left unchanged** — exactly why DECOMESS accepts any of
   {3MONCO,3TRACO,3START,3WMONCO} and dispatches on STOPR.
7. **Microcode-shortcut screening before answering** (CALL_END @013613): 270B/271B/333B/335B/
   500B dump the dirty data cache first (DMA coherency; 333B = the UDMA special);
   501B/502B/600B get local start/stop/swapper assists; one NDIX case answers and KEEPS
   EXECUTING (asynchronous MON call, CALL_NDIX @025401); 117B/120B/144B/201B take a wait
   variant (CALL_WF).
8. **Restart consumption byte-anchored on the microcode side too:** on 3MONCO the microcode
   moves the function value into **X1** (@015721) and the error flag into the **K flag**
   (K,ZRO/K,ONE @015727/015731) — matching MONICO's FUNCV(13)/KFLIP(11) write-back and the
   manual's CALLG error convention ("IF K GO Error", code in W1).
9. ~~Still open: the trap-stop writer~~ **TRAP STOPS DECODED 2026-07-16** (pseudo-C doc
   sec 3.9). Summary:
   - Trap collection: TRAP_SAM gathers ALU/MIC/IDU status; trap number = IDU status low
     byte; a 5-word trap record is parked in the SRF trap area (0o40).
   - Triage (TRAP_FIND): per-process context block (base 0o4000) + system enable registers
     decide: (a) LOCAL macro trap handler via the domain's DIT (handler address table,
     per-trap enable bytes — TRAP_ENT/TRAP_START set P and resume macro execution), or
     (b) STOP to the ND-100, or (c) out-of-band OCB message.
   - The stop writes into the process's OWN message, same as a MON stop:
     `STOPR(11) := TRAPCODE(2)` (TRAP_GEN1 @013513 / TRAP_GEN4B @013571), saved P,
     `TRAPN := trap number` (TRAP_GEN4C @013574; page fault = 0o46), plus status words and
     the trap record in the data part (TRAP_GEN2/3). TRAP_END then answers
     `N5STA := 3 (ANSWER)` — **or 4 (5ERANSWER) if the run-state flag says the process
     wasn't running** — and rings the doorbell (MSG_END0).
   - A SYSTRA halfword (SRF cell 0o2004) marks "system traps": those ALSO emit an
     out-of-band OCB message type 201B built by the microcode itself through ACCP_XWRITE
     (matches manual: "message built by the microprogram, sent directly through the ACCP").
     Other OCB codes: 203B CPU-unavailable (MSG_START refusal path via CPU_AVAIL? on SRF
     cell 0o2016), 204B/205B/206B/210B protocol/not-recognized errors.
   - Page fault (0o46): if no local handler, TRAP_GEN4 stop **plus TRAP_SWAP** — a message
     to the swapper is built in the START_MESS area (0o20000): demand paging stops to the
     ND-100 and/or wakes the swapper.
   - Still open (fine-grain): TRAP_GEN3 exact word map (where TRAPN@16 + fault params land),
     TRAP_OCB00..20 per-subtype payloads, saved-P offset, DIT layout, full trap-number list.

### 7c. 13B/14B RESIRD/RESIWR field layout + INIT_SAMSON verdict (microcode answer 2026-07-17)

Source: `MICROCODE-ANSWER-INIT-SAMSON-AND-13B-2026-07-17.md` (lossless B30 listing, all [V]):

| Message word offset (octal) | 13B RESIRD | 14B RESIWR |
|---|---|---|
| 7-10B (32-bit word, hi first) | source: ND-500 address | dest: ND-500 address |
| 11B-12B (32-bit word, hi first) | dest: physical (MMS) address | source: physical (MMS) address |
| 13B (halfword) | byte count (rounded up to words) | byte count |

- Both are PURE BLOCK COPIES - no content generation, no validation; TRAPN(16B) is never read
  (the live trace's TRAPN=10746B in 13B requests is leftover buffer garbage).
- **The ND-500 address is the FULL 32-bit word at offsets 7-10B.** The live msg-5 "N500A=177B"
  therefore decodes as high half 177B -> source likely 0x7F0000 (just under the 8MB top) -
  INFERRED: a memory-SIZING probe, matching "Error in memory configuration". [I]
- **INIT_SAMSON writes NOTHING into ND-500 memory 0-2048** [V]: it clears SRF/caches/TSBs,
  reads SYSPAR from the ACCP, composes CPUPAR from the CPU model; all config constants live in
  the microword patch panel 000020-000037 (VERSION=027232B etc.) - in CONTROL STORE, not RAM.
  Whatever SINTRAN validates at ND-500 addr 0 was placed there by the ND-100 itself.
- After servicing, the microcode ONLY writes N5STA:=3 + doorbell - it never touches 3022
  register state. (Live trace: SINTRAN polls RSTA5 for ~340ms right after MICRO-START,
  reading $0, before sending the first 13B - what it waits for is an OPEN question on the
  3022/SINTRAN side, not the microcode.)
- Emulator status 2026-07-17: 13B corrected to the 32-bit source decode; 14B implemented
  (was silently dropped -> 5ERANSWER); 14B->13B round-trip unit-tested.
- **LOAD-SWAPPER wire sequence (OBSERVED live 2026-07-17):** the swapper image transfer =
  **44 x 14B RESIWR** (2048-byte blocks into ND-500 0x5A000-0x6F000 + one 256-byte block to
  0x24800; mostly zeros + code/data pages), then **one MICFU=12B MSG_CACHE** (param 147717B -
  conditional cache clears, B30 @015640) to make the freshly written code coherent, then
  **one MICFU=21B** (SENDE=1B, X5CPU=0B, N500A=0B). Rejecting ANY of these aborts the load
  SILENTLY ("Loading Swapper" repeats on every status/start-swapper, no error text;
  list-a-p shows process 1 magic 0). Emulator: 12B answered (no-op, no caches); 22B
  MSG_STARTP0 accepted; **21B still rejected = the current blocker** (2nd trace run
  2026-07-17: 21B is the ONLY 5ERANSWER; SINTRAN performs NO memory-based aliveness probe -
  the swapper-loaded verdict rides entirely on the 21B answer). NOTE: on the 5800 B30 image
  MICFU 21B dispatches to MSG_ILLEG - a CLASSIC-500 MICFU, generation-dependent.
- **MICFU 21B IDENTIFIED (2026-07-17): `3WREG` = REGISTER WRITE** (twin `3RREG`=20B =
  REGISTER READ). Symbol `3WREG=000021` [SYMBOL: N500-SYMBOLS.SYMB:4743 + L07/M06 tables];
  NPL receive dispatch N5XXC slot 21 = WRREG, "queue only", no ND-100 post-processing
  [NPL: MP-P2-N500.NPL:393-422]; layout [MANUAL: ND-05.012.01 Micro Program Guide sec 13.13]:

  | Msg word (octal) | 21B REGISTER WRITE (3WREG) |
  |---|---|
  | 6 | MICFU = 21B |
  | 7 | first register number to write (live trace: 0) |
  | 10B | number of registers |
  | 11B-12B | 32-bit physical ND-100 address of the register values |

  Semantics: DMA-read N register values from ND-100 memory into the ND-500 register block
  starting at register X - SINTRAN loading process-0's INITIAL REGISTER CONTEXT (incl. P)
  after placing the swapper image. **The answer carries NO data** - DECOMESS routes non-
  {3MONCO,3TRACO,3START,3WMONCO} answers to 5RRTWT; LDSWA only needs ANSWER(3) vs
  5ERANSWER(4). Register-image width in ND-100 memory (2 words/reg, hi first) is INFERRED -
  log offsets 10B-12B on the next live run to pin count+source.

- **MICFU 17B IDENTIFIED (2026-07-19): `3DEPR` = DEPOSIT REGISTER (`DEPRG`)** - the single-
  register write member of the ND-500 register examine/deposit family. NEW carve driven by
  the D4 boot-harness instruction trace (see 7c-bis below). Symbol `3DEPR=000017`
  [SYMBOL: N500-SYMBOLS.SYMB:3192 (L07) + N5000-SYMBOLS.SYMB:3282 (M06)]. NPL receive
  dispatch `N5XXC` @133512 slot 17 = `DEPRG` [NPL: MP-P2-N500.NPL:397, table row
  `WAMED, RNEWCO, EXARG, DEPRG` = codes 14-17]. Like 21B it is QUEUE-ONLY (falls to the
  common tail @133626 = set timeout timer + `GO FAR TOQUEUE`; no ND-100 post-processing,
  MICFU is not rewritten) [NPL: MP-P2-N500.NPL:414-422]. On the answer, `DECOMESS` sees
  MICFU=3DEPR is NOT in {3MONCO,3TRACO,3START,3WMONCO} -> routes to `5RRTWT` (restart the
  requesting ND-100 process). **The answer carries NO data on the deposit path** - the
  microcode need only set N5STA:=ANSWER(3) vs 5ERANSWER(4), identical rule to the 21B twin.

  The register family and its symbols (EXA=examine/read, DEP=deposit/write; suffix = address
  space): `3EXAD`/`3DEPD` = 06/07 (memory-descriptor), `3EXAR`/`3DEPR` = 16/17 (**ReGister**),
  `3EXAP`/`3DEPP` = 32/33 (memory-physical) [NPL: MP-P2-N500.NPL:395,397,400]. So
  **16B = `EXARG` = EXAMINE REGISTER** (read one register - the read twin of 17B; queue-only,
  same common tail) [SYMBOL: `3EXAR=000016`]. Adjacent **15B = `RNEWCO`** is NOT a register
  op but a microprogram/control-store reload-restart handler ("...FIND ANOTHER PROCESS TO
  LOAD THE MIC.PROGR.") with real body logic [NPL: MP-P2-N500.NPL:568-570; SYMBOL
  `3RESO=000015`].

  **Layout (INFERRED, register-family mirror of 20B/21B - NOT byte-pinned):** word 6 MICFU=17B,
  word 7 register number, plus a value/count field; the microcode listing that defines the
  exact single-register deposit fields lives outside this repo (`ND5000UC`, referenced only),
  so the per-register semantics are NOT asserted. For the D4 hack only the ANSWER(3) matters.

  **GENERATION:** register examine/deposit/read/write (06/07, 16/17, 20/21, 32/33) is a
  CLASSIC-500 family. DIRECT B30 evidence exists only for the 21B twin (-> MSG_ILLEG on the
  5800 image, lines 336-338); that 17B is likewise classic-only is a strong family inference,
  NOT an independently carved B30 fact. The SINTRAN SENDER (`N5XXC`, L07) is common to both
  machines and emits 17B regardless; the generational split is whether the target microcode
  honors it. Sender-of-record for the message BUILD (which stores MICFU:=17B) is NOT locatable
  in the available NPL - 3DEPR/3EXAR/3RREG/3WREG are never literally stored anywhere in the
  tree (only the N5XXC table + fall-through label reference them), so the register messages are
  built by numeric code from a table or by an ND-500-monitor command interpreter absent from
  this source set. Not guessed.

### 7c-bis. D4 boot-harness trace (2026-07-19) - 17B DEPRG is the post-CS-load blocker

Instruction-level trace + 3022 register log of the D4 `Nd500_D4_RunDomain_RealCpu_Capture`
run (offline `EnableTraceFile` + hex-aware SINTRAN-L symbol resolver; artifacts in the
scratchpad). CORRECTS the older "stuck in CS-load / Loading Swapper" framing:

1. **CS-load COMPLETES.** `RETG5:=0` (MON-60 25B MICRO-START) fires and 5CLOST (0x0200)
   CLEARS in the full run (3022 log `RETG5 RESTART/MICRO-START (0x00) -> 5CLOST CLEARED`).
   The `csStore[]` read-back model in NDBusND500IF works. The earlier "stuck forever" read
   was an artifact of a capped CPU-trace window landing mid-download.
2. **After CS-load, SINTRAN drives a repeating mailbox bring-up cycle** against one reused
   message buffer: `...N x 14B RESIWR (process-0 image) -> 12B MSG_CACHE -> 21B 3WREG
   (register block) -> 17B 3DEPR (single-register deposit) -> 1B 3RMICV (watchdog poll)`,
   then repeats.
3. **On the emulated servicer (built as `Nd500Generation.Classic`, NDBusND500IF.cs:933)**
   every message in that cycle is ACCEPTED except **17B**, which has no servicer case ->
   hits the `default` -> **5ERANSWER(4)** [Nd500MicrocodeServicer.cs:608-612]. SINTRAN gets
   the error, cannot advance the bring-up, and re-sends the whole cycle = the D4 "NO WELL
   DEFINED PROGRAM" wall now sits HERE, one step past the 21B blocker of 7c.
4. **Fix direction:** on Classic, answer 17B DEPRG with ANSWER(3) (queue-only, no data
   read-back), same contract as 21B. Whether the register value must actually be deposited
   into CpuND500 to satisfy a LATER check is untested - the immediate gate is only the
   error answer.

### 7d. The "swapper alive" contract (2026-07-17 - the M1/M2 acceptance chain)

The full chain SINTRAN needs to consider the swapper started; ND-100 side NPL/byte-cited,
swapper side byte-verified from swapper-k01-pseg.asm:

1. **21B 3WREG accepted**: copy the register block into process-0 context, N5STA:=3,
   doorbell. (Current blocker - rejecting it silently aborts LOAD-SWAPPER.)
2. **START-SWAPPER** (STSWP 54B -> RUNSW @163621) -> MSWSTART [MP-P2-N500.NPL:431-442]:
   SWMSG gets MICFU:=3START(23B), SENDE:=5SWPROC, SWPFU:=SWACTIVE, priority 300, queued.
   (22B STARTP0 is the separate watchdog variant - NOT this path.)
3. **On 3START the CPU runs process 0** from the P loaded in step 1. Swapper startup
   [BYTES, swapper-k01-pseg.asm entry @PSEG+4]: stack init -> "REV.-K01" build-tag
   self-check (mismatch -> error 32B -> MON 0B LEAVE) -> main: zero DSEG 0x240B0/0x240B4 ->
   **MON 377B (N5SWAP, CALLG seg-31 offset 377B), argc 4, first arg VALUE = 1**. The
   swapper announces itself by NO other means - no memory write, no self-built message;
   the MON-call trap IS the aliveness signal.
4. **The microcode/emulator answer to that trap** (stop contract): on the swapper's OWN
   activation message (SWMSG): STOPR(11B):=MOCALL(1), NUMPA(12B):=4, MCNO(13B):=377B,
   param addresses @40B+, param VALUES @100B+ (5DP1@101B = 1), N5STA:=3, doorbell.
5. **SINTRAN closes the loop** [NPL: MP-P2-N500.NPL 1346/912-919/933-1006/1031]:
   DECOMESS (3START+MOCALL) -> MCHANDLE -> (A=N5SWAP AND X=SWMSG) -> SWPDECODER ->
   SWPFU=1 -> LNEWSWAP -> first time (HSWPI=0) -> SWPD4: **SWMSG.N5STA := PSWWAIT(7) =
   "swapper free"** - THE observable success state. 5ACTSWAPPER hands work to the swapper
   only when it sees PSWWAIT; "Loading Swapper" stops recurring.

Magic number (list-active-processes): lives at `MAGNO` = message-buffer offset -3
[SYMBOL: MAGNO=177775, N500-SYMBOLS.SYMB:5715]; displayed via MON 60 RPROC 077B -> IRMESS
copying the raw buffer incl. negative header [NPL: 5P-P2-MON60.NPL:1500-1513]. The WRITER
of MAGNO is NOT FOUND in the NPL tree (system-monitor code, likely RESRV 15B worker in
030-S3SM5 - carve to settle); do NOT use "magic != 0" as an acceptance criterion yet.

POISONED PRIOR: `ND500-SWAPPER-LOADING-MECHANISM.md` claims the swapper is NOT loaded into
ND-500 memory - DISPROVEN by the byte-verified SWAPPER-K analysis and the live 44x14B
block-write trace. Do not cite that claim.
- **The caller/worker chain (byte-cited, nd-500-mon carve + FUNCS table):**
  LOAD-SWAPPER cmd (case 010211-010216 in nd-500-mon) -> MON 60 subfn **7B SWLOD** ("PLACE
  SWAPPER", one param = swapper segment name) -> FUNCS[007] **LDSWA @143551** in 030-S3SM5 =
  the worker that performs the whole observed wire sequence (14B loop + 12B + 21B).
  START-SWAPPER cmd (case 010217-010221) -> subfn **54B STSWP** (no params) -> FUNCS[054]
  **RUNSW @163621** "start (run) swapper" - only reached AFTER a successful load, so the
  actual start message (22B STARTP0 / 23B 3START?) was never observed: the load dies at 21B
  first.
- **SENDER FOUND + BYTE-VERIFIED (carve 2026-07-17):** the post-CS-load 13B burst is the
  `TSTMC`/`TSTPA` memory pattern test in 030-S3SM5 (52235B/52606B; called from CSLOA's tail
  @155565): write pattern into the ND-500 page via the multiport window (STATX loop),
  13B read-back, word-for-word compare; 4 patterns per page (177777B/0/125252B/052525B);
  mismatch -> EIMDCONF 2054B; the CS verify's own error is EILOCS 2103B. Msg-5 source =
  top page of the configured part = SIZING check (the 0x7F0000 inference is now grounded).
  MON 60B twins AMEMR/AMEMW (FUNCS[032], 142474/142545) build the same 13B/14B messages.
  Details: ND500-CS-LOAD-TRACE-FINDINGS-2026-07-16.md section 6a.

## Key sources
`ND500-BUS-INTERFACE-REFERENCE.md` sec 5-7; `ND500-EVIDENCE-AND-CONTRADICTIONS.md` 2.5.7/2.6.2/2.6.5;
`...\re\ND500-SYSTEM-MONITOR\ND500-5MPM-MESSAGE-AND-ACTIVATION.md` + `ND500-LEVEL12-RETURN-PATH.ASM`;
`ND500-STATUS-AND-INDEX.md` sec 2.1-2.3; `ND500-SWAPPER-ANALYSIS.md` sec 12 +
`swapper\swapper-k01-deep-analysis.md` + `swapper\swapper-k01-handlers.md`;
`ND500-BUS-OCTOBUS-HW-INTERFACE.md` sec 3; `CC-P2-N500.md` sec 13; `RP-P2-N500.md` (XMSINIT).

## 8. Carver responses to the microcode-RE requests (R1-R9), added 2026-07-16

Answers to `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVER-REQUESTS-FROM-MICROCODE-RE.md`. The
microcode (`E:\Dev\Ronny\ND5000UC\microcode\MAILBOX-MICROCODE-PSEUDOCODE.md`) proves read/write
ORDER and VALUES but not absolute offsets or consumers; this section pins them from the SINTRAN side.

**Provenance / how to read the grades below.**
- **L07 byte source:** `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\segments-ref\026-S3IMPIT\026-S3IMPIT.asm`
  (load base 32000B, big-endian .bin; sha256 `0806cd3e...`). This IS the L07 driver image.
- **L07 offset authority (SYMBOL):** `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\N500-SYMBOLS.SYMB.TXT`
  (L07-native, not s3vs-4). Every field offset below was read from this file.
- **Routine logic (NPL):** `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\MP-P2-N500.NPL` and
  `RP-P2-N500.NPL` / `CC-P2-N500.NPL`. **Address relation proven this session:** the NPL emits
  s3vs-4 addresses that map to the L07 carve with a constant **+200B shift** in the driver region
  (NPL `NOUTSTR` @140627 <-> L07 `141027` byte-confirmed; NPL GOSW dispatch @137342 <-> L07 ~137542
  byte-confirmed). So NPL gives authoritative *logic*; the L07 bytes confirm the same structure
  shifted. Grade **BYTES** = read directly in `026-S3IMPIT.asm`; **NPL** = read in the s3vs-4 NPL
  whose structure the L07 shift matches; **SYMBOL** = L07 symbol file.

### R1. MON-call parameter block offsets - VERIFIED and REFINED

**GOSW dispatch table - BYTE-CONFIRMED in L07** (`026-S3IMPIT.asm`). MCHANDEL computes
`5CMNO-L12MIN` and runs a 20-slot computed goto (`LDA;SUB;RADD SA DP` @~137542, then 20x `JMP I 60`
@137545-137570). The **pointer table is at L07 `137625B`** (20 words, MON 500B-523B). Decoded
pointer words (BYTES):

| MON | slot | ptr word | handler | note |
|---|---|---|---|---|
| 500 STAPROC | 137625 | 140356 | 140356B | |
| 501 NSTOPROC | 137626 | 140511 | 140511B | |
| 502 SWITPROC | 137627 | 140356 | **=140356B** | shares STAPROC body |
| 503 NINSTR/DVINST | 137630 | 141272 | 141272B | |
| **504 NOUTS** | 137631 | 141027 | **141027B** | matches catalog exactly |
| 505 GERRC | 137632 | 141633 | 141633B | |
| 506 5SIBMO | 137633 | 141716 | 141716B | |
| 507 SPRIO | 137634 | 142033 | 142033B | |
| 510 SWMC | 137635 | 142153 | 142153B | |
| **511 DVIO** | 137636 | 141027 | **=141027B** | **shares NOUTS body** (new) |
| 512 A5XMSG | 137637 | 142253 | 142253B | |
| 513 B5XMSG | 137640 | 142253 | **=142253B** | shares A5XMSG body |
| 514 M5TMOUT | 137641 | 140563 | 140563B | |
| 515 5MTRANS | 137642 | 143445 | 143445B | |
| 516-523 | 137643-137650 | 137651.. | -> NORMM | six stubs `JMP I` via word `137726`=**137167** (NORMM) |

So **504 NOUTS = 511 DVIO = 141027B** (one body), **512 A5XMSG = 513 B5XMSG = 142253B**,
**500 STAPROC = 502 SWITPROC = 140356B**, and **516-523 all funnel to NORMM=137167B** (BYTES-confirmed;
this refines the catalog's "516-523 -> NORMM" and adds the shared-body facts).

**Parameter offsets (SYMBOL, L07) - the `5AP1..5DP4` rows are CONFIRMED, plus 5AP5/5DP5 exist:**
`5AP1=100, 5DP1=101, 5AP2=102, 5DP2=103, 5AP3=104, 5DP3=105, 5AP4=106, 5DP4=107`, and reads/writes
of **`5AP5=110, 5DP5=111`** appear (RFRRE), so the value array extends past 4. The
`ND500-MONITOR-CALL-PARAMETER-PASSING.md` sec 2.1 rows are **verified against the L07 symbol file**
(they were previously only asserted; now L07-native SYMBOL).

**Pair order / (address,value) question - REFINED, partial refutation of the microcode [D] framing.**
As CONSUMED by the ND-100 (NPL, byte-mapped): a parameter's 32-bit VALUE lives in a
consecutive-word double **`5APn`(even)=high 16b, `5DPn`(odd)=low 16b** (`N5FUD` @137527 reads
`LDDTX` at 5AP1, checks high `IF A><0` then takes low `A:=D`; A5XMSG write-back @142673 does
`*AAX 5APn; STZTX` (zero high) `; AAX 5DPn-5APn; STATX` (store value low), with `input:/output:`
comments naming param N -> {5APn,5DPn}). Operand/buffer **ADDRESSES are a SEPARATE region**, not
adjacent to the values: `5PPA1=40, 5PPA2=42, OSTRA=44` (SYMBOL; N5FUD reads "BUFFER ADDRESS" from
`5PPA2=42`), plus I/O-descriptor fields `ABUFA=140, LBUFA=141, TODF=142`. **So the microcode's
per-arg "(address,value) word pair" is consumed as TWO separate arrays** (VALUE at 100B+, ADDRESS
at 40B+), NOT one consecutive (addr,val) pair at 100B. Whether 5APn(high) originally carried the
operand's high value-word or its address cannot be distinguished from the SINTRAN side (immediates
give high=0 either way, and write-back overwrites 5APn) - do not assert either; the CONSUMED
positions above are what the emulator must honor. **This partly resolves catalog UNKNOWN #2:** the
40B-47B region DOES have symbols (`5PPA1=40, 5PPA2=42, OSTRA=44`) - it is the parameter-address/pointer
array, not "no symbol evidence."

**Inline user buffer for 504/511/512 (R1c):** flagged by **`MIFLAG` bit `WSMC`(bit 0)** = "data
buffer is in com-buffer (by mic.prog)" (DVIO/NOUTSTR @140627: `IF MIFLAG NBIT WSMC` -> issue a
`3RMED`(10) data-memory read to DMA it; else the microcode already filled it -> go straight to the
driver). **Max length = 4000B bytes** ("because of COM.BUFFER SIZE", checked as `D>>4000` in
NOUTSTR/NINSTR/A5XMSG). The buffer is NOT inline in the 200B(=128-word) header - it is a separate
com-buffer addressed by **`ABUFA=140`** (ND-100 phys, 32-bit) / referenced via `LBUFA=141`; the
ND-500 logical data address is carried in `OSTRA=44` -> copied to `N500A=7`, byte count to
`NRBYT=13`. (Grade: SYMBOL offsets + NPL logic.)

### R2. Saved-P location - the mon/trap stop saved-P is NOT read by SINTRAN

- The only place SINTRAN reads the **ND-500 process P from a message** is `HISTSAMPLE` @135103
  (NPL): `*AAX N500A; LDDTX` -> reads the sampled P from **`N500A=7`** (32-bit double), for the
  `3RPREG`(44) histogram sample; it compares against the histogram target window. (NPL + SYMBOL.)
- **On a MON-call / trap STOP there is NO SINTRAN read of the microcode's saved-P word.** MCHANDLE,
  TRAPDECODER and DECOMESS never read it; the microcode self-resumes (`CALL_MON9` sets `IAC_P:=L`,
  i.e. P:=return-addr) so SINTRAN does not need it. The saved-P the microcode writes at
  `CALL_MON9 @004006` is therefore write-only w.r.t. the ND-100 normal path.
- The **ND-100 shadow program's** P (a different thing) is a proc-descriptor/datafield slot
  **`DPREG=0`** (`*DPREG@3 STATX`, "ADDR OF FROMESC INTO P OF PROGRAM", MP @~132341) - NOT a message
  offset and NOT the ND-500 P.
- No `XADPR(144)`-based P read exists. **Emulator consequence:** the saved-P word in a MON/trap stop
  is don't-care to SINTRAN; only the explicit `3RPREG` path needs P, delivered at message offset
  `N500A=7`.

### R3. Trap stop consumption - TRAPDECODER @135314 (NPL)

- **Read order:** `TRAPDECODER` first reads **`TRAPN=16`** (`*AAX TRAPN-1; LDDTX` -> D=trap number,
  as a double).
- **Full legal range = 0..53B.** `IF D>53 THEN ILTRAP` (unknown trap -> `WN5STATUS`(illegal) ->
  5RRTWT). So legal trap numbers are **0 through 53 octal**; anything above is rejected as ILTRAP.
- **46B = PAGE FAULT** (special-cased): routes to `ITRAPDECODER`, which (unless the message is the
  swapper's own `SWMSG`, or the swapper isn't started) builds a swap request
  `MSWPFAULT SHZ 10 + trapno` into `TRAPN` and calls `5ACTSWAPPER`. Confirms microcode page-fault=0o46.
- **All other traps (D<=53, D!=46)** take the generic error path `ITRPERR/ITRP2ERR`: it reads
  `5CPUN=-6` (CPU#, masked `/\377`), `SENDE=3` (sender=process#), `XADPR=144` -> `RTRES` (RT
  reference), builds a 4-word error record `(ETRIPRC+trapno, process#, RT-ref, CPU#)`, logs via
  `9FLER(TRPELIST,4)`, then `5RRTWT`.
- **On trap numbers 0o44 / 0o51:** SINTRAN's TRAPDECODER does **not** special-case them (only 46B is
  special) - they fall into the generic error/report path. The microcode's 44/46/51 specials
  (`TRAP_ENT` bypassing the enable byte) are a microcode-LOCAL-handler matter; by the time a trap
  STOPS to the ND-100, only 46B gets special treatment. **Named trap-number table not found in the
  driver** (traps are handled numerically); a symbolic trap-name list would have to come from a
  separate symbol set - marked UNKNOWN, not guessed.

### R4. Does SINTRAN (L07) SEND MICFU 05 (3SWMESS) or 27B (3FITRNSF)? - NO. Discrepancy resolved.

**Micfunc values (L07 SYMBOL):** `3RMIC=1, 3SWME=5, 3RMED=10, 3WMED=11, 3STAR=23, 3MONC=24,
3TRAC=25, 3WMON=26, 3FITR=27, 3PHSR=30, 3PHSW=31, 3RPRE=44` (plus `3RMEP=34, 3WMEP=35, 3MONO=34,
33MON=46`).

**Sender-side dispatch table `N5XXC` @133512 (NPL) - the ND-100 twin of the microcode `MSG_TABLE`.**
`N500C` (Level-2 monitor routine "execute commands to nd-500") reads `MICFU` from the message and
`A GOSW` into a 00-77B table. This is a **major catalog addition**; it enumerates every
micro-function SINTRAN can drive:

| # | routine | # | routine | # | routine | # | routine |
|---|---|---|---|---|---|---|---|
|00|STUPR|01|RMICVE|02|MILLFU|03|MILLFU|
|04|MILLFU|**05**|**SWMESS**|06|EXAMD|07|DEPMD|
|10|RMEMD|11|WMEMD|12|CACHE|13|RAMED|
|14|WAMED|15|RNEWCO|16|EXARG|17|DEPRG|
|20|REREG|21|WRREG|**22**|**P0START**|23|STAOPP|
|24|MONCO|25|MTRACO|26|WMONCO|**27**|**5RLBH**|
|30|MPHSREAD|31|MPHSWRITE|32|EXAMP|33|DEPMP|
|34|RMEMP|35|WMEMP|36|FQUEUE|37|RAMEP|
|40|WAMEP|41|RLIMI|42|PRTRAP|43|WLIMI|
|44|MILLFU|45|MPCLR|46|MILLFU|47|MILLFU|
|50|MILLFU|51|MILLFU|52|NKREL|53|MILLFU|
|60|CLRPROC|70-75|TRC70..TRC75|76|SCACHEMODE|77|RSCRREG|

**05 (3SWMESS) is NEVER put on the wire.** `SWMESS` @133635 (the slot-05 routine) translates a
swapper request into `3START`(23) (`*MICFU@3 STATX` @133661, MSWSTART path) or `3MONCO`(24)
(@134145, MSWSWAIT path) or `CALL 5ACTSWAPPER; GO TRACO`. Grep confirms `3SWMESS`/`3SWME` occurs
ONLY in comparisons (`IF ...=3SWMESS`, 4 sites), never stored as a MICFU. **27B (3FITRNSF) is NEVER
sent either:** its only occurrence is a comparison (`IF ...=3FITRNSF`, MP @145326); the slot-27
routine `5RLBH` @134542 releases a file-transfer buffer header and `GO MONCO` (emits 3MONCO(24)).
**=> B30's `05 -> MSG_ILLEG` and `27B -> MSG_ILLEG` are CONSISTENT with SINTRAN L07; no version
problem.** (Grade NPL, independently corroborated by the B30 microcode.)

**Senders of the "extra B30 codes" - most ARE used, and the two tables largely agree:**
`10 RMEMD / 11 WMEMD` (data-mem R/W = B30 DMEMRD/DMEMWR), `13 RAMED / 14 WAMED` (= B30 RESIRD/RESIWR),
`12 CACHE`, `22 P0START` (= B30 STARTP0, see R7), `30 MPHSREAD / 31 MPHSWRITE` (= PHYSRD/PHYSWR),
`34 RMEMP / 35 WMEMP` (= IMEMRD/IMEMWR), `42 PRTRAP` (= PRT), `45 MPCLR` (= CLEAR),
`70-75 TRC70-75` (= trace family), `76 SCACHEMODE` (= CACI), `77 RSCRREG` (= LOOKSRF). **Generational
differences:** SINTRAN N5XXC marks **46/47/50/51 as MILLFU (illegal)** whereas B30 assigns
46=DUDC / 47=IDLE / 50-51=UNIX5RE/CM; and **52 = NKREL** (nucleus release) vs B30 UNIX5REL.
Actual `3xxx` micfuncs SINTRAN transmits to the ND-500 in normal operation: **1, 10, 11, 22, 23,
24, 25, 26, 44** (plus the monitor/debug micro-functions 06-21,30-37,40-43,45,70-77 issued via
N500C for LOOK-AT etc.). **`STARTP0`/MICFU 22B sender = `P0START` (see R7).**

### R5. 3RMICV watchdog answer - SINTRAN reads NEITHER answer halfword

`CHN5STATUS` @135004, `ANSWER` + `WATCHDOG` branch (lines 135024-135036, NPL): it identifies the
message **by ADDRESS** (`IF X=WATCHDOG`), removes it from the ex-queue (`SLOCK; IFM500XQ; SUNLOCK`),
and **re-arms the timeout** (`0=:TMRXQ; LTTMR=:TMR`). **It reads NO message offsets at all** - not
the version halfword, not the second (CPU-parameter) halfword. The mere return as `N5STA=ANSWER(3)`
proves the microcode alive. **=> the second halfword is NOT consumed; no consumer of it was found
anywhere in the driver.** The monitor "read microprogram version" command (`RMICVE`, N5XXC slot 01)
just queues the message (falls into the common `...; A=:TMR; GO TOQUEUE` tail @133626); its answer
returns ANSWER -> `DECOMESS` -> MICFU=3RMICV(1) is not in {3MONCO,3TRACO,3START,3WMONCO} -> `5RRTWT`
(restart the requesting ND-100 process, which reads the version from the message). **Emulator:** for
the watchdog it suffices to set `N5STA:=ANSWER(3)`; writing the version + CPU-parameter halfwords is
harmless but unread on the watchdog path.

### R6. NUMPA write-back mask - bit k => 5AP(k+1)/5DP(k+1), BYTE/NPL-confirmed

- **Write-back primitive:** for parameter N, `*AAX 5APn; STZTX` (zero the high word) then
  `AAX 5DPn-5APn; STATX` (store the return value into the low word). Confirmed for N=2,3,4,5 in
  A5XMSG (@142673-142745) with matching `output: A=:Nth parameter` comments.
- **Mask semantics CONFIRMED:** DVINST sets `NUMPA:=4` (bit 2) and writes the returned byte count to
  `NOCHR=104B (=5AP3)` -> **bit 2 => parameter 3 (5AP3/5DP3)**, exactly "bit k => 5AP(k+1)/5DP(k+1)".
  DVIO sets `NUMPA:=100000B` (bit 15 = extended write-back to the `11xxx` DVIO slots,
  `11NOCHRET`). XMSG builds the mask from an `XMRETMASK(func)` table (@143145) and stores it to
  `NUMPA=12`. Swapper path sets `NUMPA:=6` (bits 1+2 => params 2 and 3; MP @145057 "Par #2 & par #3
  will be written").
- **Applied by:** `INSMONCO` @147334 / `MONICO` (`CC-P2-N500.NPL` @023022) store the mask to
  `NUMPA-SMCNO`/`-NUMPA` and clear FUNCV; the microcode 3MONCO restart consumes NUMPA to drive its
  conditional write-back loop. **Write-back values land at the odd `5DPn` slots (101,103,105,107,111);
  the even `5APn` (100,102,104,106,110) are zeroed.** (Grade: SYMBOL offsets + NPL logic.)

### R7. ND-5000 mailbox head + system-parameter init

- **Queue head the ND-500 polls:** the **message-communication FIFO**, described by the CPU
  datafield sub-block `N500DF.X500DF`: **`X5FIF=6`** (FIFO base pointer), **`X5HEN=3`** (get/"hente"
  index), **`X5FYL=4`** (fill index), **`X5MXF=5`** (capacity = `MX5PROCS`). `XMSINIT @131123`
  (`RP-P2-N500.NPL`) allocates the FIFO buffer inside `5MBBANK` and stores its base into `X5FIF`
  (@131635, `*AAX X5FIF-X5SWF; STDTX`), `X5MXF:=MX5PROCS` (@131637). The reader `XN500 @134723`
  (`026-S3IMPIT`) polls it: reads `X5MXF`/`X5HEN`, computes `X5FIF + HENTE*elem_size`, dereferences
  (`CNVBYADR`), and services messages until `HENTE=FYLLE`. **This is the ITOFIFOQ ring** (classic
  ND-500 uses `MAILINK` at datafield offset 22; ND-5000 uses this X500DF FIFO instead). The exact
  MPM word address is **install-dependent** (computed by XMSINIT in 5MBBANK, held in `X5FIF`), not a
  fixed constant. A separate swap-wait FIFO uses `X5SWF=51/X5SWB=52/X5SWH/X5SWM`.
- **System parameters mechanism = an Octobus "WriteSysPar" message, NOT MICFU 22B, NOT a CS
  side-table.** `CON5IDENT @147133` and `MFPREPARE @147100` (`MP-P2-N500.NPL`) build an Octobus
  multibyte message with command `MCOMMAND := CMSYSPAR SHZ 10 \/ N100IDENT` ("Send system par") and
  send it via `MBSEND`; the ACCP's ACK/NACK ("Ack/Nack answer on 'WriteSysPar' message, i.e. I'm
  present") is caught by `5OMBREAD`, which sets `CPUAVAILABLE |= 5ALIVE`. So the ND-100 hands the
  ND-5000 its ident/system parameters over Octobus at connect time. The GIVEINT interrupt words the
  microcode composes (`(ident & 037400)|100001`/`|100401`) arrive on the ND-100 as the level-12
  ident that `CON5IDENT` connected (`N100IDENT`).
- **MICFU 22B (STARTP0) sender = `P0START` @134500** (`MP-P2-N500.NPL`, N5XXC slot 22): reuses the
  `WATCHDOG` message, inserts it in the ex-queue (`ITO500XQ`), stores the micro-function value into
  **`MICFU=6`** (`*MICFU@3 STATX`), sets `N5STA:=MSGN500(1)`, activates the ND-500. (Earlier grep for
  "STARTP0" found nothing because the routine is named P0START and stores the raw code, not a named
  `3STARTP0` symbol.) Grade NPL.

### R8. OCB multibyte receivers - `5OMBREAD` @146550 (`MP-P2-N500.NPL`)

The ND-100 receive side for Octobus multibyte messages from the ACCP / 5000 microprogram /
MF-controller. It reads a record via `OMBREAD` from `OCTORING` (OMD `5OMDNO`) into `LMFIELD`, then
decodes **`ETYPE` -> `CSTS` = high byte (`ETYPE SHZ -10`), `CMICP` = low byte (`ETYPE /\ 377`)**:
- **Type 201B = HW-fault / General trap from the microprogram** (`IF CMICP=1 AND CSTS=200 OR A=201`):
  it attaches the shadow-process id (`LMFIELD.S5` -> proc desc -> `RTRES` into `LMFIELD.S4`), sets
  the error record at **`LMFIELD+2`**, size **`MMSGLENGTH+4`**. This is the consuming-end match for
  the microcode `TRAP_OCBM(0o201)` system-trap message.
- **MFACK/MFNACK** = ack of the WriteSysPar/alive message -> `CPUAVAILABLE |= 5ALIVE` (see R7).
- **Other error codes** (`CSTS/\17<17`, "known error message"): indexed into the fatal/non-fatal
  table `MPFATAL:=(1\0,0\1,0\0,0\0,0\1,1\0)`; fatal -> `XRSTARTALL`. Record at `LMFIELD+3` (or +2 for
  MF-controller), size `MMSGLENGTH+2`.
- **Payload = the error-log record** documented in the routine header: `omdsource, emainstat,
  elog1..elog4, emaster, eslave, eaddress, esyndrom` (matches the manual ND-05.020.01 HW-fault
  multibyte format). Everything is then handed to `9FLER` (error logger) with `(LMREC, LMSIZE)`.
- **Per-subtype maps for 203B/204B/205B/206B/210B are NOT individually decoded on the ND-100 side** -
  the receiver treats them generically (CSTS-indexed error record + MPFATAL classification), so the
  microcode's `TRAP_OCB00..20` payloads cannot be closed from the SINTRAN consumer beyond "an
  error-log record forwarded to 9FLER." **Address note (honest):** the request cited "OMBREAD
  @037660 in 026-S3IMPIT"; the receiver I find is `5OMBREAD` (NPL @146550 -> L07 ~146750) with a
  low-level `OMBREAD` helper. `037660B` in `026-S3IMPIT.asm` disassembles as unrelated interrupt
  code - the `037660` anchor could NOT be confirmed; use `5OMBREAD` as the receiver.

### R9. DECOERRMESS and trap-shaped (N5STA=4) messages - HANDLED SPECIALLY, not purely error

`DECOERRMESS` @135240 (the `5ERANSWER(4)` branch of CHN5STATUS) is **not** a pure error path. It
reads **`TRAPN=16`** (`*AAX TRAPN-1; LDDTX` -> D) and **`MICFU=6`** (`A`), and:
```
IF D=46 AND (MICFU in {3MONCO,3RMED,3WMEP,3RMEP,3WMONCO,3PHSREAD,3PHSWRITE,3WMED,3TRACO,3START})
   GO ITRAPDECODER          % page fault -> same swapper path as a normal trap
CALL 5RRTWT                 % otherwise restart the ND-100 process
```
So a **page-fault trap (TRAPN=46B) that returns as `5ERANSWER(4)`** with a legal page-fault micfunc
is still processed as a page fault (into `ITRAPDECODER` -> swapper), exactly reproducing the
microcode's `TRAP_END` `N5STA:=4` case (set when the run-state flag says no process was running).
Note the discriminator SINTRAN uses is **TRAPN=46 + legal MICFU, not STOPR** (DECOERRMESS never reads
STOPR). **Emulator: MUST reproduce the conditional** - a 5ERANSWER carrying a page fault is not a
throwaway error; drop the page-fault-shaped 5ERANSWER into the trap/swapper path.

**Cross-cutting note (both directions).** The SINTRAN-side N5XXC dispatch (R4) and the microcode
MSG_TABLE are now both catalogued; the mailbox header offsets used above are all L07-SYMBOL-verified
(`N5STA=2, SENDE=3, X5CPU=4, X5ACT=5, MICFU=6, N500A/H500A=7, STOPR/KFLIP/N100A=11, NUMPA=12,
MCNO/FUNCV/NRBYT=13, MSWMC=14, TRAPN=16, X5SND=30, SMCNO=37, XMICF=60, 5PPA1/PDR1=40, 5PPA2=42,
OSTRA=44, ABUFA=140, LBUFA=141, TODF=142, SPFLA=143, XADPR=144, 5AP1..5AP5=100/102/104/106/110,
5DP1..5DP5=101/103/105/107/111`), confirming section 1's table from the L07 file directly.

### 8.1 Octobus FIFO follow-ups (O1 asserts) - answers for the microcode-RE LLM, 2026-07-16

Follow-ups to R7 from `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVER-REQUESTS-FROM-MICROCODE-RE.md`
§10.5. Sources: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\XC-P2-N500.NPL` (ITOFIFOQ @030370),
`...\MP-P2-N500.NPL` (XN500, XKICK500, CON5IDENT), `...\RP-P2-N500.NPL` (XMSINIT @131123), and
`...\SYMBOLS\L07\N500-SYMBOLS.SYMB.TXT`. Grades as in §8 (BYTES = in `026-S3IMPIT.asm`; else NPL/SYMBOL).

**The X500DF ring (offsets SYMBOL, L07).** One ring in the shared 5MPM bank, in the ND-500 CPU
datafield sub-block `N500DF.X500DF`:

| off | sym | role |
|---|---|---|
| 1 | `X5NCP` | NCPU |
| 3 | `X5HEN` | "hente" = **consumer/get index** (advanced by XN500) |
| 4 | `X5FYL` | "fylle" = **producer/put index** (advanced by ITOFIFOQ) |
| 5 | `X5MXF` | ring capacity (= `MX5PROCS`) |
| 6 | `X5FIF` | **ring base pointer** (5MBBANK-relative) |
| 7 | `X5STA` | this CPU's Octobus station (shared-ext copy) |
| 14 | `X5SWM` | swap-FIFO SWMSG cell |
| 20 | `X5BTI` | ring bottom/init marker (-1) |
| 26 | `X1STA` | the ND-100's Octobus station (= `N1OCTDEST`) |
| 50/51/52 | `X5SWH/X5SWF/X5SWB` | swap-wait FIFO head/front/back |
| 54 | `X5NKS` | Nucleus-Kernel start |

**(2) FIFO element format - PROVEN.** Each element is **exactly one double-word (2 ND-100 words) =
the message's 5MBBANK word-address**, nothing else (no ident/type word). Producer `ITOFIFOQ`
(`XC-P2-N500.NPL @030370`) writes it with `CNVWADR` + `STDTX`; consumer `XN500` (`026-S3IMPIT`,
NPL @134723) reads it with `LDDTX` + `CNVBYADR` -> `N5MESSAGE`. Element stride = 2 words
(`index SH 1` in both), so `element[i]` = `X5FIF_base + i*2`.

**(1) How SINTRAN links the outgoing message (the ND-5000 equivalent of ACT50's MAR write) - PROVEN
producer + kick; one microcode-side detail flagged.** Two steps replace the classic
`IOX LMAR5 + LCON5=5`:
1. **`ITOFIFOQ @030370`** (called with the N100/N500 semaphore held, IOF): read `X5FYL`, compute
   `new=(X5FYL+1) mod X5MXF`, store `new` back to `X5FYL`, then write the **message address** into
   `element[old X5FYL] = X5FIF_base + oldFYL*2` (`STDTX`). It self-patches to `P+1; EXIT` (no-op) on a
   classic ("old") 500 - the ring path is nd5000-only; classic still uses the MAR.
2. **`XKICK500 @146322`** (`LV12KICK @146355`): sends an **Octobus kick to `5STATION`** (the ND-500's
   station) via `SKICK`. Kick types: `CLRKICK`, `IDLEKICK`, `N100KICK` (`ACT52 @145520` uses
   `N100KICK`). This is the doorbell that pulls the idle microcode out of its `#CPUDF` poll.

So SINTRAN "links" the message by appending its **address** to the `X500DF` ring at `X5FYL` and then
kicking. **FLAG (confirm microcode-side):** which exact word the `#CPUDF` poll reads to notice new
work cannot be proven from the SINTRAN side - candidates are `X5FYL` (offset 4, the fill index
SINTRAN advances) or the ring base `X5FIF` (offset 6). SINTRAN's observable act is "advance X5FYL +
write element + Octobus kick"; point `srf[#CPUDF]` at this `X500DF` structure and read the fill
index / element accordingly. (`XN500`'s own `X5HEN` walk is the ND-100 answer/retire scan - do NOT
mistake it for the microcode's consume pointer.)

**(3) X500DF location / init - `XMSINIT @131123` (`RP-P2-N500.NPL`).** `5MBBANK = 5FPMAILBOX SH 12`;
after zeroing the `5NPMAILBOX` message pages, `X500DF` = the running 5MBBANK-relative allocation
pointer (@131153). Per-CPU shared datafield extensions (`MSMLINK`, stride `5EXTDFSIZE`) are laid out
for every CPU `S5CPUDF..E5CPUDF`, reached from each CPU datafield's `MAILLINK`; for `MUDOM` (nd5000)
CPUs they carry `X5STA` (station), `X5ACC`/`X5OCT`/`X5HWB` (ACCP/Octobus/HW buffers). The
message-communication FIFO buffer is allocated in 5MBBANK at `MSIN2` (@131630): `X5FIF := base`,
`X5MXF := MX5PROCS`, `X5BTI := -1`; the swap-wait FIFO plus `X5NCP:=NCPU`, `X1STA:=N1OCTDEST`,
`X5NKS:=5NKSTART` are set alongside.

**(4) SYSPAR ident/station - mechanism PROVEN; numeric values NOT carved.** The ND-100 hands the
ND-5000 its parameters by an Octobus message `MCOMMAND := CMSYSPAR SHZ 10 \/ N100IDENT`, built by
`CON5IDENT @147133` / `MFPREPARE @147100` and sent via `MBSEND` ("Send system par."); the ACCP ACK
is caught by `5OMBREAD` -> `CPUAVAILABLE |= 5ALIVE`. Station is per-CPU `5STATION = CPUNO+FN5DEST-1`
(`RP @132221`); the ND-100's own station is `X1STA = N1OCTDEST`. The GIVEINT ident the microcode ORs
(`(SYSPAR & 037400)|100001`) derives from `N100IDENT`. **Not proven (marked, not guessed):** the
numeric values of `CMSYSPAR`, `N100IDENT`, `FN5DEST`, and the `SYSPAR` cell offset are NOT in
`N500-SYMBOLS.SYMB.TXT` - they live in the MON60/Octobus symbol set (`SYSPAR` is referenced as
`"N500DF+SYSPAR"`, a 16-word block saved/restored in `5P-P2-MON60.NPL @032702`). Pull those symbols
to pin the exact bits (item 4 is the "can wait" one).

### 8.2 O1 round 2 - the real head cell + resolved SYSPAR values, 2026-07-16

Sources: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\CC-P2-N500.NPL` (ITO500XQ @022547),
`...\MP-P2-N500.NPL` (XACTRDY/ACT50-52 @145266-145534), and the L07/swapper `N500-SYMBOLS.SYMB`
(5-char-truncated names). Offsets SYMBOL(L07); logic NPL.

**(1) SOLVED - the head cell + the work flag, and the "third write" the microcode-RE LLM predicted.**
At each nd5000 activation site the producer does **THREE** writes under the N100/N500 semaphore, then
kicks:
1. **`ITO500XQ @022547`** (the predicted third write) - the classic ex-queue insert, run on the
   nd5000 path too: it (a) sets `5MSFL` bit `5IEXQUEUE` in the message, (b) **priority-links the
   message into the ex-queue** via `LINK`(0)/`PLINK`(147) with the head at **`MAILINK`(=22B, in the
   CPU datafield)**, and (c) **`MIN LEXQUEUE`** - increments the ex-queue length counter
   **`LEXQUEUE`(=14B)**. `IFM500XQ @022665` reverses it (clear `5IEXQUEUE`, decrement `LEXQUEUE`).
2. **`ITOFIFOQ @030370`** - message address into the `X500DF` ring at `X5FYL` (§8.1).
3. **`XKICK500 @146322`** - the Octobus kick to `5STATION`.

So **the halfword work flag the microcode IDLE poll tests nonzero is `LEXQUEUE`** (ex-queue length,
CPU-datafield offset **14B**): `ITO500XQ` makes it nonzero when work is queued, `IFM500XQ` returns it
to 0 when the queue drains. **The ex-queue head the microcode walks is `MAILINK`(22B)** (walked
`LINK@3` until `-1`, exactly as SINTRAN's own `XACT500`/`N500` walk it) - this is the match for the
microcode's unexplained **`ADR_EXQUE` (srf 0o2020)**. The microcode's **`#CPUDF` (srf 0o2017)**
points at the ND-500 CPU datafield; `mem[#CPUDF + 14B] = LEXQUEUE` is the poll flag, and
`mem[#CPUDF + 22B] = MAILINK` is the queue head. (The `X500DF` FIFO of §8.1 is a *second, parallel*
structure carrying the message address for the kick path; `ITO500XQ`+`MAILINK`/`LEXQUEUE` is the
primary ex-queue.) **CONFIRM microcode-side:** that `#CPUDF` reads offset 14B (LEXQUEUE) as the flag
and 22B (MAILINK) as the head - both are now named with offsets, so this is a direct check.
`MPACTIVE=1` (the `X5CPU` precondition value).

**(2)/(4) SYSPAR values - RESOLVED (SYMBOL, L07/swapper N500-SYMBOLS, 5-char-truncated names):**
- **`CMSYSPAR = 16B`** (`CMSYS`) - the Octobus "send system parameter" command code.
- **`N100IDENT = 1`** (`N100I`) - the ND-100's Octobus ident.
- **`FN5DEST = 70B`** (`FN5DE`) - first ND-500 Octobus station; per-CPU `5STATION = CPUNO+FN5DEST-1`.
- **`SYSPAR = 111B`** (`SYSPA`) - offset of the **16-word** SYSPAR block within `N500DF` (saved/
  restored in `5P-P2-MON60.NPL @032702`, `X:=16`).
- So `CON5IDENT`/`MFPREPARE` send `MCOMMAND = CMSYSPAR SHZ 10 \/ N100IDENT = (16B<<8)|1 = 016001B`.
  The ND-100 `N500DF.SYSPAR` block (16 words @ +111B) is the source; the microcode's own
  `ADR_SYSPAR` (srf 0o2006) is the destination; `GIVEINT`'s `(SYSPAR & 037400)|100001` ident derives
  from `N100IDENT=1`.

**(3) GIVEINT ident FIFO - no separate SINTRAN ring (cheap answer).** The microcode's outbound ident
FIFO (`ADR_FIFOB`, srf 0o2002) is drained by the ACCP/Octobus **hardware** into a level-12 interrupt;
there is **no SINTRAN-side ident ring** consuming it. SINTRAN only *connects the Octobus ident* so it
receives that interrupt - `CON5IDENT @147133` -> `ECONID` with `N100IDENT(=1)` on `LV12B`. So the
producer/consumer pair is microcode-FIFO -> ACCP-hardware -> ND-100 level-12 ISR, not a shared
software ring like `X500DF`. (Nothing further to carve unless the ACCP's internal ident-buffer layout
is wanted, which is a hardware/ACCP question, not SINTRAN.)
