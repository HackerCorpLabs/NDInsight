# CARVE ANSWER — classic (CONT-STORE-10611) trap-stop writer, the mid-run S2 dispatch, and CONTROL bits 6/8 in the terminate handler

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVE-ANSWER-CLASSIC-TRAPWRITER-S2-CONTROL-2026-08-11.md`
**Date:** 2026-08-11.
**Questions answered (three):**
1. Find and read the CLASSIC image's trap-stop writer and verify/correct the per-trap
   message offsets that `CARVE-ANSWER-TRAP-RECORD-OFFSETS-AND-MMS-BITS-2026-08-11.md`
   carved from the B30/ND-5000 microcode (there marked "expected identical, INFERRED").
2. The mid-run S2 re-poll at `011066/011067`: how does a RUNNING macro program continue
   after a mid-run activate walks the queue, given the queue-empty exit ends in `JMP 011014`
   (IDLE)? Where is `011066` reached from?
3. Terminate handler (`007576+`): what does the CONTROL bit-8 "dump routine" actually do,
   what does the bit-6 path set/clear and who reads it — and which SINTRAN code writes
   CONTROL bits 6/8.

**Sources and grades:**
- **[V-MC]** `E:\Dev\Repos\Ronny\ND110Compile\ND110Compile\uCode\CONT-STORE-10611.uc` —
  the lossless round-trip disassembly of the REAL classic control store (byte-exact
  reassembly proven; see the `nd500-microcode` skill). Every microword below was read from
  this file this session. Octal addresses.
- **[V-carve prior]** `E:\Dev\Ronny\ND500UC\docs\ND500-5015-MICROCODE-INTEGRATION.md`
  (sections 2-7: interface vocabulary, TAG codes, IDLE dispatch, answer block) and the
  2026-08-11 STATUS-bit carve in the `nd500-microcode` skill — used as anchors, not
  re-derived.
- **[V-MC-B30]** `CARVE-ANSWER-TRAP-RECORD-OFFSETS-AND-MMS-BITS-2026-08-11.md` — the B30
  layout this doc compares against.
- **[V-NPL]** `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\PH-P2-RESTART.NPL`,
  `MP-P2-N500.NPL`, `CC-P2-N500.NPL` (address-stamped listings; logic evidence, different
  revision than the L07 bytes) — for the CONTROL-bit writers.
- SINTRAN symbols (`N500-SYMBOLS.SYMB`): `N5STA=2`, `X5CPU=4`, `MICFU=6`, `STOPR=11`,
  `TRAPN=16`.

---

## 0. TL;DR

1. **Trap writer found: `011271-011337`** (entered from the trap sorter via
   `011260 → 011606/011607/011637`; ends at `010537 → JMP 011405`, the shared answer
   block — as required). **Header slots agree with B30 exactly**
   (`11B`=STOPR:=2, `12B-13B` trapping P, `14B-15B` restart P, `16B`=TRAPN). The
   **trap-dependent area does NOT match B30** — the "identical layout" inference is
   **corrected**:
   - **46B page fault:** LA @ `17-20` ✓, phys seg @ `21` ✓ (mask `7777B` = 12 bits, vs
     B30's 13), but the status @ `22` is **one halfword** (a composed
     `TRAPINF<<8 | subtype` word, bit 6 = program side), NOT the B30 32-bit MMS @ 22-23.
   - **44B and 51B** (the only other traps with fault parameters): a classic-specific
     7-slot record `17-20` real (physical) address, `21-22` logical address, `23` DSTS1/2
     composite hw, `24` DSTS0 hw, `25-26` loop state, `27-30` DCINHLL, `31` composed
     status. No ASTS/BADAP (ACCP-era registers; classic has none).
   - **25B/26B/27B:** `17-20` = the LL (lower-limit) register, `21-22` = context cell
     `124B`. **45B and everything else: NO fault parameters.**
2. **The mid-run S2 poll `011066/011067` is inside the micro-trap sorter** (entry
   `011022`, the address the init code installs at `000627` — the only static reference).
   A mid-run activate does **not** strand the program: the queue walk **accepts messages
   with N5STA 1 AND 2** (`007617-007621`), so it re-dispatches the running program's own
   still-status-2 activation message; the start/continue handler calls `011473`, which
   **skips the context reload when the message's process is already the current one**, and
   control re-enters execution through `011370 → 011377-011401`
   (`DP:=P; PRF,ISAMP; PRF,START; JMPMAP PRF,PCONT`). Macro registers were never touched
   (the walk uses only AL#/AM# scratch). The queue-empty exit → IDLE is reached only when
   no start/continue message re-dispatched — i.e. when nothing should keep running.
3. **CONTROL bit 8 = "run the power-fail save sequence on this terminate"**: the terminate
   handler jumps into `010750`, the SAME body as the S2 bit-10 power-fail handler: save the
   current process's full macro register context (35B registers) into its context block,
   walk all page groups writing the DWIPGU|IWIPGU written-in-page bits as a byte table into
   ND-500 memory (an exact mirror LOADER exists at `011000-011013`, so it is save/restore
   state, not a diagnostic dump), write the 177777 marker, STATUS |= 210B
   (finished+5PFAIL), UNLOCK, IDLE. **SINTRAN's writer: the ND-100 power-fail path**
   (`PH-P2-RESTART.NPL` 5PF @ `032246`: `A:=400; *IOXT` on LCON5, then SLOC5, then TERM5).
   **CONTROL bit 6** only decides whether a terminate that aborts an in-progress
   message ALSO sets the microcode's "no current macro process" flag (`AL#10` bit 0,
   set at `011403`); its lone reader is the MICFU 44B histogram answer ("current
   process" vs 177777). **No SINTRAN code writes bit 6** — NOT FOUND in the whole NPL
   corpus (LCON5 values used: 0, 1, 5, 10B, 40B, 400B).

---

## 1. Decode conventions used (established before trusting any branch)

### 1.1 The DMA/context subroutine library (all [V-MC])

Calls are `JMPNS <addr>` (push return), returns are `POPRET`. `W,IO` strobes the 3022 bus
transaction. The 3022 MAR auto-advances per 16-bit DMA strobe (already emulator-pinned).

| Sub | Body | Effect |
|---|---|---|
| `007540` | `AL#20→IODOUT; TAG:=201; TAG:=1` (+`007564` strobe) | 3022 MAR := AL#20 |
| `007543` / `007544` | TAG 206+6 / TAG 6 | DMA read 32-bit / 16-bit → AM#20 |
| `007546` / `007550` | TAG 207+7 / TAG 7 | DMA write 32-bit / 16-bit of AM#20 |
| `007552` | TAG 0 + 200 | read MAR → AL#20 |
| `007564` | `POPRET W,IO` | the shared strobe |
| `007607` | TAG 2, OR `BM#3`, TAG 3 | STATUS: set finished |
| `007612` | AL#20:=0; MAR:=0; write AM#20 hw | the "marker at ND-100 word 0" write |
| `011527` | `AM#23 := AM#22<<10B; DP := AM#23+AM#25; read` | **AM#20 := ctx[AM#25]** — context-block read, base = process number (AM#22) shifted; one cell |
| `011542` | write twin of 011527 | **ctx[AM#25] := AM#20** |
| `010330` / `010337` | LC:=35B loop + `JMPREL` register table `010360/010422-010457` | full macro context SAVE / LOAD (L, B, R, X#0-3, AM#/AL# banks, S1, AL#16/17, LL/HL, …) |

### 1.2 Condition polarity [V by internal consistency]

A microword latches a condition (its `COND,x SET`, or — with no COND field — "ALU result
== 0"); the NEXT word's `C,SEQ` tests it, and the **`F,*` alternative fires when the
latched condition is FALSE** (so `AND + F,JMP` = "jump when the bit is SET"). Checked
against three already-byte-verified facts: the IDLE dispatch outcomes (bit10→010750,
bit5→007570, bit6→007576), the terminate handler's documented "in-progress clear → straight
to UNLOCK", and the queue walk's status filter (below). Every branch reading in this doc
uses this rule.

---

## 2. Question 1 — the classic trap-stop writer

### 2.1 Where it lives, entry and exit [V-MC]

- **Micro-trap entry = `011022`.** The only static reference in all 8192 words is the init
  word `000627/ JMP W,IOEPM AB,ORAB EX,CTF EXUNIT=2 F,ADIRC 11022,0 ;` — an external-unit
  write carrying the address as its argument. [INFERRED: this installs 011022 as the
  hardware micro-trap vector; nothing else can explain how the sorter is ever entered.]
- Sorter `011022-011167`: waits ICRDY + clears prefetch (`011022-011024`), captures
  `AM#31 := XD,DLADDR` (`011027`) and `AM#27 := TRAPINF<<8` (`011041-011042`), branches on
  TRAPINF/S1/S2 bits. MMS-fault legs collect the D-side or I-side status sets
  (`011172-011210` / `011213-011231`, detailed in 2.4) and OR a subtype code into AM#27
  (`011140-011145`: codes `6/7/10` data-side, `106/107/110` instruction-side — **the
  `+100B` = bit 6 = program side**, the classic counterpart of the B30 MMS "bit 6 =
  DATA/PROGRAM").
- Common prelude `011232-011245`: `D,TRAPCLR`, cache-control restore (`007764/011233/011234`),
  read ctx[230B] (`011236`), then four local-memory writes saving AL#23, **P**, AM#27,
  AM#31 into the DPARG-based context trap area (`011242-011245`).
- TRAPN synthesis + stop-vs-handle decision `011246-011261`:

```
011256/ ALU,ADIR A,AM#20 COND,MSGN NEXT SET ;
011257/ ALU,AND A,AL#10 B,BM#1 C,SEQ COND,MZRO JMP SET F,NEXT 7746,0 ;   % bad trapno -> dead stop 7746
011260/ CTRL143=1 ALU,ADIR A,AM#20 D,AL#24 C,SEQ JMP F,NEXT 11606,0 ;    % AL#24 := TRAPN; in mailbox service -> 12447
011261/ CTRL143=1 JMP 12447,0 ;
```

  A stop **while inside the mailbox service** (`AL#10` bit 1 set) diverts to `012447`
  (answers the current message with error paths / status 4); a stop of a **running
  program** (bit 1 clear) goes to `011606`:

```
011606/ ... A,XD,SARG B,AL#24 COND,MSGN ... 44 ;     % TRAPN > 44B ?
011607/ CTRL143=1 C,SEQ JMP F,NEXT 11271,0 ;          % yes -> writer; else check user trap handler (TE)
...
011637/ CTRL143=1 JMP 11271,0 ;                       % no local handler -> writer
```

  (TRAPN ≤ 44B first checks the process's trap-enable state via ctx[234B]/S1 and can
  dispatch the program's OWN trap handler instead of stopping — `011610-011637`, resume
  side at `012325-012340`.)

- **Exit: the writer always ends in the shared answer block.** `011321/011337` → the S2/S1
  cleanup sub `011350`; the 46B leg additionally clears ctx[230B] and reloads the MMS
  process state (`011324 → 010203 → 012544`); then

```
010537/ ALU,A+B A,XD,SARG B,AM#12 D,AL#20 JMPNS SLOW2 7540,177776 ;  % MAR := msg base (AM#12-2)
010540/ D,ICCLR JMPNS SLOW1 7543,0 ;                                 % re-read link word
010541/ CTRL143=1 ALU,ADIR A,AM#20 D,AL#21 NEXT ;                    % AL#21 := link
010542/ ALU,ADIR A,XD,SARG D,AM#37 NEXT SLOW2 177777 ;               % no trap-handler ctx
010543/ CTRL143=1 ALU,OR A,BM#0 B,AL#10 D,AL#10 JMP 11405,0 ;        % AL#10 bit0 := 1; -> ANSWER BLOCK
```

  `011405-011407` answers with `AL#25` (preloaded **3** by the writer at `011275`; error
  paths preload 4), sets finished, and the queue walk resumes from the stopped program's
  own link (`011410-011413`). **Requirement "must end in 011405": VERIFIED.**

### 2.2 The header writes [V-MC] — AGREE with B30 slot for slot

`AM#12` = address of the message's N5STA halfword = msg+2 (set at `007624` during message
intake), so `AM#12+7` = message halfword **11B**:

```
011271/ ALU,A+B A,XD,SARG B,AM#12 D,AL#20 JMPNS SLOW2 7540,7 ;   % MAR := msg hw 11B
011272/ ALU,ADIR A,XD,SARG D,AM#20 JMPNS SLOW2 7550,2 ;          % hw 11B := 2      (STOPR := TRAPCODE)
011273/ ALU,ADIR A,AM#10 D,AM#22 NEXT ;                          % AM#22 := current process
011274/ CTRL143=1 ALU,ADIR A,XD,SARG D,AM#25 JMPNS SLOW2 11527,224 ; % AM#20 := ctx[224B]
011275/ CTRL143=1 ALU,ADIR A,XD,SARG D,AL#25 JMPNS SLOW2 7546,3 ;    % hw 12B-13B := ctx[224B] (trapping P); AL#25 := 3
011276/ ALU,ADIR A,XD,SARG D,AM#25 JMPNS SLOW2 11527,240 ;           % AM#20 := ctx[240B]
011277/ JMPNS 7546,0 ;                                               % hw 14B-15B := ctx[240B] (restart P)
011300/ ALU,ADIR A,AL#24 D,AM#20 JMPNS 7550,0 ;                      % hw 16B := TRAPN
```

Both P values are staged through the process context block (cells `224B`/`240B` — the
prelude `011243` saved the live P there). [V for the writes; the "trapping vs restart"
naming per cell is INFERRED from slot position matching B30/§13.16.]

### 2.3 The per-trap class selector [V-MC]

```
011301/ ... A,XD,SARG B,AL#24 COND,MSGN ... 27 ;                 % TRAPN > 27B ?
011302/ ... COND,MSGN JMP SET F,NEXT SLOW2 11310,24 ;            % >27 -> 11310 ; else TRAPN > 24B ?
011303/ CTRL143=1 C,SEQ NEXT F,JMP 11310,0 ;                     % <=24 -> 11310 ; 25/26/27 fall through
011310/ ... COND,MZRO ... 51 ;                                   % TRAPN == 51B ?
011311/ ... C,SEQ COND,MZRO JMP SET F,NEXT SLOW2 11331,44 ;      % ==51 -> 11331 ; else ==44 ?
011312/ ... C,SEQ COND,MZRO JMP SET F,NEXT SLOW2 11331,46 ;      % ==44 -> 11331 ; else ==46 ?
011313/ C,SEQ NEXT F,JMP 11321,0 ;                               % ==46 -> 11314 ; else 11321 (NO params)
```

So: **{25B,26B,27B} → `011304`; {44B,51B} → `011331`; {46B} → `011314`; ALL OTHERS
(including 45B) → `011321` = no fault parameters.** This matches the byte-verified S3SM5
consumer grouping ({25,26,27} / {44,46,51} / rest) — and shows 45B (protect violation)
carries NO parameter block on the classic machine.

### 2.4 The parameter blocks, slot by slot [V-MC writes; register naming per mnemonic]

**Page fault, TRAPN = 46B (`011314-011316`):**

```
011314/ CTRL143=1 ALU,ADIR A,AM#31 D,AM#20 JMPNS 7546,0 ;             % hw 17-20 := AM#31 = XD,DLADDR  (fault LA, 32-bit)
011315/ ALU,AND A,XD,SARG B,AL#35 D,AM#20 JMPNS SLOW2 7550,7777 ;     % hw 21    := AL#35 & 7777B      (phys segment, 12 bits)
011316/ ALU,ADIR A,AM#27 D,AM#20 TYP,HW JMPNS 7550,0 ;                % hw 22    := AM#27               (composed status, ONE hw)
```

`AL#35` = `XD,DCINHLL` (collected at `011210`); `AM#27` = `TRAPINF<<8 | subtype` with
bit 6 = instruction side (built `011041-011042`, `011140-011145`).

**Hardware/MMS stop traps, TRAPN ∈ {44B, 51B} (`011331-011337` + shared tail `011316`):**

```
011331/ ALU,ADIR A,AL#31 D,AM#20 JMPNS 7546,0 ;    % hw 17-20 := AL#31 = XD,DRADDR|IRADDR & 0x00FFFFFF (REAL/physical addr, 24-bit)
011332/ ALU,ADIR A,AL#32 D,AM#20 JMPNS 7546,0 ;    % hw 21-22 := AL#32 = DLADDR / XD,ILADDR            (logical addr)
011333/ ALU,ADIR A,AL#33 D,AM#20 JMPNS 7550,0 ;    % hw 23    := (DSTS1 &~ mask) | (DSTS2 & mask)      (status composite, hw)
011334/ ALU,ADIR A,AL#30 D,AM#20 JMPNS 7550,0 ;    % hw 24    := DSTS0 / ISTS0                          (hw)
011335/ ALU,ADIR A,AL#34 D,AM#20 JMPNS 7546,0 ;    % hw 25-26 := AL#34 (final collect-loop mask/state)  [OPEN meaning]
011336/ CTRL143=1 ALU,ADIR A,AL#35 D,AM#20 JMPNS 7546,0 ; % hw 27-30 := DCINHLL / ICINHLL (32-bit)
011337/ CTRL143=1 JMP 11316,0 ;                    % hw 31    := AM#27 composed status (hw)
```

The values come from the D-side collector `011172-011210` (`DSTS0`, `DRADDR & 0x00FFFFFF`,
`DLADDR`, `DSTS1/DSTS2` merge loop with `DCON1`, `DCINHLL`) or its I-side twin
`011213-011231` (`ISTS0/IRADDR/ILADDR/ISTS1/ISTS2/ICON1/ICINHLL`), selected by which unit
faulted.

**Stack/limit traps, TRAPN ∈ {25B,26B,27B} (`011304-011307`):**

```
011304/ ALU,ADIR A,XD,LL D,AM#20 JMPNS SLOW2 7546,0 ;           % hw 17-20 := LL (lower-limit register)
011305/ CTRL143=1 ALU,ADIR A,XD,SARG D,AM#25 JMPNS SLOW2 11527,124 ; % AM#20 := ctx[124B]
011306/ JMPNS 7546,0 ;                                          % hw 21-22 := ctx[124B]   [OPEN: cell naming]
011307/ JMP 11321,0 ;
```

### 2.5 Verdict — classic vs B30, per slot

| link.NN | B30 (ND-5000) | Classic 10611 | Verdict |
|---|---|---|---|
| 11 | STOPR := 2 | STOPR := 2 (`011272`) | **AGREE** |
| 12-13 | trapping P | ctx[224B] (P staged at trap entry) | **AGREE** (slot+meaning) |
| 14-15 | restart P | ctx[240B] | **AGREE** |
| 16 | TRAPN | AL#24 (`011300`) | **AGREE** |
| **46B page fault** | | | |
| 17-20 | fault LA (`DMM,LA`) | fault LA (`XD,DLADDR`) | **AGREE** |
| 21 | phys seg = `CAP & 017777` (13 bits) | `DCINHLL & 7777B` (12 bits) | **AGREE position**; mask one bit narrower, different source register |
| 22(-23) | 32-bit MMS status @ 22-23 | ONE halfword @ 22: `TRAPINF<<8 \| subtype` (bit 6 = program) | **DIVERGE** — classic has no 32-bit MMS word; hw 23 not written |
| **other param traps** | GEN3 covers ALL non-46 stops | classic: ONLY 44B and 51B | **DIVERGE** (45B/trace get NO params on classic) |
| 17-20 | fault **LA** | **REAL (physical) address**, 24-bit masked | **DIVERGE** |
| 21-22 | MMS status | **logical address** | **DIVERGE** |
| 23-24 | physical address (32-bit) | 23 = DSTS1/2 composite hw; 24 = DSTS0 hw | **DIVERGE** |
| 25 / 26 | phys seg hw / WR hw | 25-26 = collect-loop state (32-bit) | **DIVERGE** |
| 27 / 30 | ASTS / BADAP (GEN3B/3C only) | 27-30 = DCINHLL (32-bit) | **DIVERGE** (classic has no ACCP) |
| 31 | — (record ends at 30) | composed status hw | **classic extra slot** |

**The B30 doc's §6 "classic layout expected identical [INFERRED]" is now settled: the
HEADER and the page-fault LA/phys-seg slots are identical; everything else in the
trap-dependent area is generation-specific.** Expected in hindsight: the classic MMS
exposes `DSTS0/1/2`, `DRADDR/DLADDR`, `DCINHLL` — not the ND-5000's single 32-bit
`DMM,STS`, and there is no ACCP to supply ASTS/BADAP. Emulator consequence: RetroCore's
`AnswerTrapStop` needs a per-generation trap-dependent area (the B30 list in that doc's §5
is correct ONLY for the octobus/ND-5000 generation).

---

## 3. Question 2 — the mid-run S2 dispatch and how a running program continues

### 3.1 Where `011066/011067` is reached from [V-MC]

Nothing jumps to `011060-011067` — it is fall-through inside the micro-trap sorter, whose
entry `011022` has exactly one static reference: init word `000627` ([I] vector install,
§2.1). The poll is the sorter's LAST resort: it is reached only when the sorter finds **no
macro trap cause** — not power fail (`011043-011044`), no TRAPINF bits 0/1/4
(`011045-011047`), no S2 bits 0-3 (`011060`), no masked S1 cause (`011062-011064`):

```
011065/ ALU,AND A,XD,S2 B,BM#5 COND,MZRO NEXT SET SLOW2 ;        % S2 & activate
011066/ CTRL143=1 ALU,AND A,XD,S2 B,BM#6 C,SEQ NEXT F,JMP SLOW2 7570,0 ;  % bit5 set -> ACTIVATE handler
011067/ C,SEQ NEXT F,JMP 7576,0 ;                                % bit6 set -> TERMINATE handler
011070/ JMP 7744,0 ;                                             % neither: dead-stop self-loop
```

So the site fires exactly when a RUNNING program is interrupted by nothing but the
ND-100's activate/terminate line (TRAPONOFF is 1 during a run — `011327/011402` set it on
every trap-handler/stop path). A trap with no recognizable cause at all dead-stops at
`007744`.

### 3.2 The resume mechanism [V-MC]

A mid-run activate enters the SAME handler (`007570`) and the same queue walk `011413` as
an IDLE activate. Three byte-facts combine into the resume:

1. **The walk accepts N5STA 1 AND 2** (`007617-007621`: compare against 1, then against
   2; only "neither" skips to the next link). So the running program's own activation
   message — parked at status 2 by the answer-in-place model — is re-dispatched through
   the MICFU table every time the walk passes it.
2. **The start/continue handlers skip the context reload for the current process.**
   `10201` (3START) / `10177` (3TRACO) → `010206` (`AL#10 := 0`) → `010210 JMPNS 11473`:

```
011473/ ALU,A-B-1 CRY,ONE A,AM#10 B,AM#22 COND,MZRO NEXT SET ;   % message process == current (AM#10)?
011474/ B,AM#10 D,AM#20 C,SEQ POPRET F,JMPNS W,EXT ... 10330,10 ; % equal -> POPRET (NO reload); else full context load 10330
```

   (`AM#22` = the message's X5CPU halfword +1, read at `007626`/`007635`; `AM#10` = the
   current process, maintained at `011476/011500`.) Then `010211 → 011575` reloads only
   the TE word from ctx[164B]/[204B]/[234B] and the MMS process state (`012544`), and
   returns to `010200/010202 → JMP 011370`.
3. **Re-entry to macro execution is `011370-011401`:**

```
011370/ ... D,AM#37 ... PRF,CLEAR ... 11340,177777 ;  % AM#37 := -1; restore DCON1/ICON1, clear S2 bit 8 (sub 11340)
011371-011373/ ...                                    % pending S1 bit4 / S2 bit6 (terminate) checked -> 7576
011374/ ALU,ADIR A,AL#16 D,S2 NEXT SLOW1 ;            % S2 := cleaned copy
011377/ ALU,ADIR A,P D,DP NEXT PRF,ISAMP SLOW1 ;      % prefetch address := P
011400/ DSEL=1514 NEXT PRF,START SLOW1 ;
011401/ JMPMAP PRF,PCONT SLOW2 ;                      % resume the macro program
```

   The queue walk touches only AL#/AM# scratch registers, never the macro register file —
   so with the reload skipped, the program continues exactly where the trap suspended it.

### 3.3 Answer to the question as posed

- There is **no saved micro return and no flag that routes the queue-empty exit back to
  execution** — `011421-011427 → JMP 011014` is unconditional, and IDLE is genuinely where
  the walk ends.
- The running program continues because the walk **never reaches queue-empty while a
  runnable process's message is chained**: the status-2 re-dispatch (fact 1) leaves the
  walk through the start/continue handler and `JMPMAP` (fact 3) before the link chain
  runs out. New status-1 messages linked ahead of it (histogram fn 44B, examine, resident
  read/write, …) are answered first, in place, then the run resumes.
- **If** the chain did NOT contain the running process's message, a mid-run activate
  would park in IDLE with the program's registers live but nothing to resume them. That
  the chain always does contain it is SINTRAN's side of the answer-in-place protocol
  [INFERRED from the protocol carves, not from ND-100 bytes this session].
- Corner cases seen in the same region: the walk re-polls terminate between messages
  (`011415/011416` → `007576`), and the resume path itself re-checks a pending terminate
  before `JMPMAP` (`011372/011373`).

### 3.4 The `AL#10` flag pair (referenced by both Q2 and Q3) [V-MC, complete reference list]

| Bit | Meaning | Set | Cleared | Read |
|---|---|---|---|---|
| 0 | "NO macro process is current" | init `007565` (:=1), stop exit `010543`, terminate-abort `011403` | taken start/continue `010206`, `010301` (:=0) | ONLY `007724-007726` (MICFU 44B histogram: answer current process AM#10, or 177777 when set) |
| 1 | "mailbox message service in progress" | `007615` (per message) | queue exit `011427`, start/continue `010206/010301` | terminate `007600`, stop sorter `011257`/`012434`, trap-handler return `012334` |

---

## 4. Question 3 — CONTROL bits 8 and 6 in the terminate handler

### 4.1 The handler, line by line [V-MC]

```
007576/ CTRL143=1 ALU,ADIR A,XD,SARG D,TAG JMPNS SLOW1 7564,4 ;   % TAG 4: read CONTROL
007577/ ALU,AND A,XD,IODIN B,BM#10 D,ICCLR NEXT SLOW1 ;           % latch CONTROL & bit 8
007600/ ALU,AND A,AL#10 B,BM#1 D,TRAPCLR C,SEQ COND,MZRO NEXT SET F,JMP SLOW1 10750,0 ;
        % bit 8 SET -> JMP 010750 (the power-fail body); also: ack trap, latch AL#10 bit1
007601/ CTRL143=1 ALU,AND A,XD,IODIN B,BM#6 C,SEQ JMP F,NEXT SLOW2 11404,0 ;
        % no message in progress -> JMP 011404 (UNLOCK, IDLE); else NEXT; latch CONTROL & bit 6
007602/ C,SEQ NEXT F,JMP 11404,0 ;    % bit 6 SET -> UNLOCK directly (skip 011403)
007603/ CTRL143=1 JMP 11403,0 ;       % bit 6 CLEAR -> 011403 first
011403/ CTRL143=1 ALU,OR A,AL#10 B,BM#0 D,AL#10 NEXT ;   % AL#10 bit0 := 1 ("no current process")
011404/ UNLOCK JMP 11427,0 ;                              % drop 5ILOCK
011427/ ...ANDCA BM#1...AL#10... JMP 11014,0 ;            % clear bit1, -> IDLE
```

### 4.2 Bit 8 — the "dump routine" is the power-fail save sequence [V-MC]

`010750` is the same body IDLE dispatches to on S2 bit 10 (power fail). What it does:

1. `010750`: `D,TRAPCLR`, call `007604` (HL/limit setup).
2. `010751`: `AM#20 := AM#10 << 10B`, call **`010330` = save the current process's FULL
   macro register context** — an LC=35B loop through the `JMPREL` register table
   (`010360`, entries `010422-010457`: L, B, R, X#0-3, AM#0-3/7/11/14/15, AL#0-3/7/11/16/17,
   S1, LL/HL, …) into the context block at `(current process << 10B)` in ND-500/MPM
   memory. **This is where the "registers are dumped": the process's own context block.**
3. `010752-010770`: walk the page groups (`AM#21` starts `100000`, `+bit15` per step,
   until bit 22 — 128 groups), each step: prime the MMS (`010562`), read
   `DWIPGU | IWIPGU` (the written-in-page bits, D and I side OR'd, `010765-010766`), and
   store ONE BYTE per group into ND-500 memory at `AM#20` (incrementing;
   base derived from the constant `-2` via sub `007763` — exact base cell [INFERRED,
   one more decode]).
   **The mirror LOADER at `011000-011013`** reads the same byte table back and reloads
   DWIPGU/IWIPGU — proving this is save/RESTORE state for the modified-page bookkeeping,
   not a diagnostic printout.
4. `010771-010775`: DMA the `177777` marker to ND-100 word 0 (sub `007612`);
   read STATUS, **`STATUS |= 210B`** (finished + 5PFAIL bit 7), `TAG := 3`,
   `JMP 011404` → UNLOCK → `011427` → IDLE.

So **CONTROL bit 8 = "treat this terminate as a power fail": save context + flush the
written-in-page table + finished/5PFAIL + unlock + idle.**

**Who sets it — [V-NPL, address-stamped]: SINTRAN's ND-100 power-fail routine.**
`PH-P2-RESTART.NPL` (5PF path, listing addresses `032204-032252`), per classic ND-500 CPU
(SAMSON skipped): read RSTA5; if locked write TERM5 and wait; then test mode `LCON5:=10`,
write STATUS back with 5POWOF set, `LCON5:=0`, then:

```
032246   A:=400; *IOXT           % LCON5 := 400B  = CONTROL bit 8
032250   T+"SLOC5-LCON5"; *IOXT  % set lock
032252   T+"TERM5-SLOC5"; *IOXT  % terminate strobe -> microcode reads CONTROL, sees bit 8
```

The ND-100 is going down, so it orders the ND-500 to execute its own power-fail save
before power dies. This is the ONLY `400B` written to LCON5 anywhere in the NPL corpus.
(Matches `ND500-BUS-INTERFACE-REFERENCE.md` §9.4, which lists the same sequence.)

### 4.3 Bit 6 — what it gates, who reads the flag, who writes the bit

- **Microcode effect [V-MC]:** consulted only when the terminate aborts an in-progress
  message (`AL#10` bit 1 set). Bit 6 CLEAR (the normal case): `011403` sets `AL#10` bit 0
  = "no macro process is current" before UNLOCK — the aborted activation is abandoned.
  Bit 6 SET: `011403` is skipped — the microcode keeps claiming the current process across
  the terminate.
- **Who reads `AL#10` bit 0 [V-MC, exhaustive]:** exactly one consumer — the MICFU 44B
  histogram answer (`007724-007726`), which reports the current process number or 177777.
  (Bit 0 is anyway cleared by the next taken start/continue at `010206/010301` and set by
  every stop exit at `010543`.)
- **Who writes CONTROL bit 6: NOT FOUND.** LCON5 values across the entire NPL corpus:
  `0`, `1`, `5` (ACT50 activate), `10B` (test mode), `40B` (disable TAG-IN decode), `400B`
  (the power-fail write above). No `100B` and no composite containing it; nd-500-mon
  (`MON-DEBUG:PROG`) has zero IOXT (prior carve). So in L07/this NPL revision bit 6 is a
  latent feature. Semantics [INFERRED, plain reading of the effect]: "terminate without
  abandoning the current process" — a stop-and-continue style terminate, presumably for
  test/maintenance software; nothing in the corpus exercises it.

---

## 5. Corrections and consequences

1. **Corrected:** the B30 carve's §6 inference "classic trap layout expected identical".
   Identical: header 11-16 and page-fault LA@17-20 + physseg@21. Different: everything
   else (§2.5 table). Do not feed the B30 §0.3 GEN3 offsets to a CLASSIC-generation
   emulator path.
2. **Refined:** the integration doc's "CONTROL bits 6/8 semantics beyond this: unknown"
   (§7/§9) — now known (§4). Bit 8's writer is the ND-100 power-fail path; bit 6 has no
   writer in the corpus.
3. **New verified anchors for the classic microcode:** micro-trap sorter entry `011022`
   (init reference `000627`), queue walk accepts N5STA∈{1,2}, `011473` current-process
   reload skip, resume block `011370-011401`, stop exit `010537-010543`, context-cell
   access subs `011527/011542` (base = process<<10B), full context save/load `010330/010337`
   (35B registers), WIP save/load pair `010750+`/`011000+`.
4. **Open:** meaning of hw 25-26 (`AL#34`) in the {44,51} record; ctx cell `124B` in the
   {25,26,27} record; the exact base of the WIP byte table; whether `000627` is
   literally the vector-install (only static evidence, no manual confirmation).

## 6. Cross-references

- `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVE-ANSWER-TRAP-RECORD-OFFSETS-AND-MMS-BITS-2026-08-11.md` (the B30 side; its §6 OPEN item on the classic writer is closed by this doc)
- `E:\Dev\Ronny\ND500UC\docs\ND500-5015-MICROCODE-INTEGRATION.md` (interface vocabulary, IDLE, answer block)
- `E:\Dev\Ronny\ND500UC\docs\ND500-CPU-DEEP-DIVE-2026-08-10.md` (dispatch/startup pins)
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-BUS-INTERFACE-REFERENCE.md` §9.2/§9.4 (XTER500, 5PF)
- Consumer side: `CARVE-ANSWER-PRSTART-STOPINFO-SOURCE-2026-08-11.md` (message hw 12B..41B → user stop block)
