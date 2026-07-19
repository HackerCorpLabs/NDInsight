# ND-500-MON-J04:PROG - Reverse Engineering Findings (Q2, Q3, Q6)

**Date:** 2026-07-08
**Binary:** F:\ND\SINTRAN-K05-XMSG-2026\FLOPPY\500\ND-500-MON-J04.PROG
(loaded in Ghidra as "ND-500-MON-J04.PROG", ND-100 :PROG loader, two banks:
BANK1 ram:0000-ff64, BANK2 0000-edd5)
**Identity:** version string at BANK2::2290: "ND-500/5000 MONITOR  Version J04"
(matches the L-release package ND-211305 requirement "version J04 or later").
**Method:** static disassembly only (Ghidra MCP). No dynamic run was done in this
session. Everything below is cited to a disassembly address, a file line, or is
explicitly marked UNVERIFIED.

Task source: [ND500-L-RELEASE-RE-TASK-HANDOFF.md](ND500-L-RELEASE-RE-TASK-HANDOFF.md)
sections 4 (Q2, Q3, Q6) and 6.

> **REVIEW OUTCOME (2026-07-08, second reviewer):**
>
> 1. Section 3 symbol values: independently re-verified against BOTH L07 and M06
>    N500-SYMBOLS.SYMB.TXT - all values and line numbers confirmed; applied to the
>    dossier (item 4/6/7, C8, C12) together with additional truncated-name finds
>    (5CPUT=7, SAMSO=3, OLD50=1, 5ALIV=15, 5NOTP=17, X5SEM=0, MAILI=22).
> 2. DUMMESS reclassification (section 3.6): CONFIRMED - DP-P2-VARIABLES.NPL:119
>    "INTEGER DUMMESS % Address of dummy msg"; set in XMSINIT at RP-P2-N500.NPL:793
>    ("EX.QUEUE HEAD IN MULTI-CPU SYSTEMS"). Applied to the dossier.
> 3. **ERROR FOUND - retry status identification (sections 2.2/4):** ECSLOAD=2032B
>    (0x041A) checks out exactly (5P-P2-MON60.NPL:66 "CONTROL STORE MUST BE
>    LOADED"). But PFECSLOAD = 2063 OCTAL = 0x0433 (5P-P2-MON60.NPL:91), NOT
>    0x080F. 0x080F is 2063 DECIMAL = 4017 octal - a base confusion: the value
>    0x080F was converted to decimal and matched against an octal symbol. Either
>    the constant at ram:ccc5 is really 0x0433 (then it IS PFECSLOAD and the hex
>    in section 2.2 is a misread) or it is really 0x080F (then its identity is
>    UNKNOWN - no 4017B symbol exists in the MON60 status table,
>    5P-P2-MON60.NPL:55-104). RECHECK the word at ram:ccc5 with the binary open.
>    Semantics favor PFECSLOAD (ECSLOAD + PFECSLOAD are the natural
>    wait-until-control-store-loaded retry pair).
> 4. Q6/Q3 structural findings accepted; applied to the master reference
>    section 11.

---

## 1. Q6 - IOX usage: the background monitor executes NO IOX at all

**Verdict: CONFIRMS spec section 3.3** (all 3022/5015 register access is confined
to resident SINTRAN; the user-side :PROG never touches the interface directly).

Evidence:

1. **IOXT (opcode 0xD10D): zero byte-pair matches in the entire program**, both
   banks, any alignment. This is decisive for extended IOX.
2. **Plain IOX (opcodes 0xE800-0xEFFF):** every word-aligned E8xx-EFxx byte pair
   examined is DATA, not an instruction - specifically PLANC pointer-table words
   whose values are addresses inside the top-of-bank runtime-library region
   (ram:d800-ff64). Examples verified in the listing:
   - ram:e8c3 / ram:e8ce: word 0xE894 - decoded by Ghidra as "IOX 0x0094" but is
     the pointer to the function entry (RADD SL,DX prologue) at ram:e894, sitting
     in the JPL data area of the thunks at ram:e8b9 and ram:e8c7.
   - ram:dcf3-dcf7: csav data area of the thunk at ram:dcf2: 0xFEC0 (csav),
     0xE93F, 0xEA71, 0xEB28 (runtime routine pointers), 0xFEDD (cret).
   - ram:1213-1216 (undefined data region): values 0xE843 and 0xEE94 match the
     known function entries FUN_ram_e843 and FUN_ram_ee94.
   - BANK2::e940-e968: stride-4 table of 0xE9xx words = BANK2 pointer table.
   - No word-aligned E8-EF hit falls inside a verified instruction stream.
3. LIMITATION (honesty note): the plain-IOX sweep is sample-based. There are a
   few hundred word-aligned E8-EF byte pairs; all inspected ones (about 15,
   chosen to cover every address cluster) are data. Combined with IOXT = 0 and
   the MON 60B interface below, the conclusion is strong but the sweep was not
   exhaustive instruction-by-instruction.

No IOX against HDEV+offset, no TAG-register access, nothing that contradicts
ND500-BUS-INTERFACE-REFERENCE.md. (Contradiction protocol: nothing to report.)

---

## 2. Q3 - Monitor-to-driver interface: MON 60B (N500M)

### 2.1 The ND-500 monitor call is MON 60B

The central driver call is at **ram:ccae: `MON 0x30` = MON 60B**, inside the
wrapper function at ram:cca4. Corroboration:

- The resident-side NPL handler source is literally named `5P-P2-MON60.NPL`
  (see ND500-EVIDENCE-AND-CONTRADICTIONS.md line 29; the file allocates the
  "MON60 buffer", ND500-IF-USAGE-DEEP-ANALYSIS.md line 838-848).
- The ND-860228-2 Monitor Calls manual's number table (manual lines 1020-1106)
  has a **gap 60B-76B** - MON 60B is not documented for user programs, which is
  consistent with a system-internal call.
- The Ghidra loader annotation names it "N500M / ND500Function (60B)". NOTE:
  this name comes from our loader spec file, not from the manual - treat the
  NAME as project-assigned; the NUMBER 60B is read from the instruction bytes.

### 2.2 Calling convention (read from ram:cca4-ccbf)

```
ram:cca4  RADD SL,DX            ; PLANC thunk entry
ram:cca5  JPL 0xccc1            ; csav (frame setup), body follows
ram:cca7  ORA I *0xccc2         ; A |= [pointer DAT_ram_da5d]
ram:cca8  STA -0x6f,B
ram:cca9  COPY SB,DA            ; A := B
ram:ccaa  AAA -0x6f             ; A := B-0x6f  = address of param block on stack
ram:ccab  STA -0x7b,B
ram:ccac  COPY SB,DA
ram:ccad  AAA -0x7b             ; A := B-0x7b = address of (pointer to block)
ram:ccae  MON 0x30              ; MON 60B - the ND-500 monitor call
ram:ccaf  JMP *0xccb1           ; error-skip convention: next word = error path
ram:ccb1  STA -0x70,B           ; save returned status (A)
ram:ccb3  LDT *0xccc4           ; T := 0x041A (= 2032B octal = ECSLOAD)
ram:ccb4  SKP DA,UEQ,ST         ; if A != 0x041A skip
ram:ccb6  LDT *0xccc5           ; T := 0x080F  (identity UNRESOLVED - see below)
ram:ccb7  SKP DA,EQL,ST         ; if A == 0x080F ...
ram:ccb9  LDX -0x7e,B
ram:ccba  STA 0x6,X             ; store status into caller structure offset 6
ram:ccbb  JPL 0xccc6            ; (wait/yield via runtime pointer)
ram:ccbd  JMP *0xccac           ; RETRY the MON 60B
```

So: **A register carries the MON 60B subfunction code; MON 60B; skip-return =
error; returned A = status code.** (The wrapper's `ORA I *0xccc2` / `AAA` prelude
combines the caller's function code with the derived stack param-block address
before the MON.) The status is stored at offset 6 of the caller's block.

**Retry statuses - CORRECTED (2026-07-08, reviewer catch).** The two compared
words are `ram:ccc4 = 0x041A` and `ram:ccc5 = 0x080F`, read directly from the
binary (hexdump at ram:ccc1: `... fe dd 04 1a 08 0f b4 78 ...`), so ram:ccc5 is
genuinely 0x080F - NOT a misread.

- **ram:ccc4 = 0x041A = 2032B octal = ECSLOAD** "CONTROL STORE MUST BE LOADED"
  (5P-P2-MON60.NPL:66). CONFIRMED (0x041A = 1050 decimal = 2032 octal).
- **ram:ccc5 = 0x080F = 2063 decimal - identity UNRESOLVED.** My earlier claim
  that this is PFECSLOAD was WRONG: it was a base-confusion. PFECSLOAD = 2063
  OCTAL = 1075 decimal = **0x0433**, which is NOT the stored word 0x080F. There
  is no 4017B (= 2063 decimal reread as octal) symbol in the MON-60 status table
  (5P-P2-MON60.NPL:40-114). So the second retry code 0x080F does not map to any
  known MON-60 status symbol and is carried as UNKNOWN.

ECSLOAD (0x041A) IS a MON-60 return code (block 2000B-2136B). The wrapper loops
while A equals ECSLOAD or 0x080F, i.e. a wait-and-retry on a
control-store-not-ready-class condition, but only the ECSLOAD half is positively
identified. These are MON-60 return codes, NOT message-status (0-4) values.

### 2.3 The MON 60B subfunction stub array (ram:ccc8-ce6d) - DECODED

The wrapper's callers form a contiguous array of 3-word stubs starting at
ram:ccc8 (the word 0xF100 directly after the wrapper's own data area):

```
F1 nn     SAA n          ; A := subfunction code n
AA 01     JMP (indirect)
CC A4     .word 0xcca4   ; -> the MON 60B wrapper
```

For codes above 177B, SAA's 8-bit operand would sign-extend, so the encoding
switches (from ram:ce6f region backwards at byte offset 19c72) to:

```
48 02     LDA (PC-rel)   ; A := following word
AA 02     JMP (indirect)
00 8x     .word code     ; 0x81..0x8C = 201B..214B
CC A4     .word 0xcca4
```

Subfunction codes present in the array (hex as read, octal in parens):
0,1,2,4,3,5,6,6,7,7,0a-12,8,9,13,14,15,3b,48,1a,1b,1e,1f,1e,1f,20,1c,1d,21-30,
32-41,43-47,49,4b,4c,4d,4f,50,51,52,42,54-5c,5e-62,64,68-6c,6e,1e,70-77,6d,
78-7b,69,6a,7c,7d,7e,7f, then 81-8c (octal 0-177, then 201B-214B). Duplicates
(6,7,1e,1f,28,69,6a) are separate user-callable entry points sharing a driver
subfunction.

### 2.4 Subfunction names: recovered from 5P-P2-MON60.NPL

The `SAA` immediates in the stub array are exactly the MON-60 function codes
defined in 5P-P2-MON60.NPL lines 165-285. Representative decode (octal code ->
symbol -> meaning), verified against that source:

| Code (oct) | Symbol | Meaning |
|---|---|---|
| 0 | RREG | read register |
| 1 | WREG | write register |
| 2 | PMREAD | read program memory |
| 3 | D5MREAD | read data memory |
| 4 | PMWRITE | write program memory |
| 5 | DMWRITE | write data memory |
| 6 | SEGLOAD | load segment |
| 7 | PLSWAPPER | place swapper |
| 10 | RREGS | read registers |
| 11 | WREGS | write registers |
| 12 | PRSTART | start program |
| 13 | FILCON | connect file |
| 14 | FILCLO | close file |
| 15 | N5RES | allocate ND-500 process |
| 16 | N5REL | release ND-500 process |
| 17 | FLIOP | list open files |
| 37 | CSLOAD | load control store |
| 40 | MEMDEF | define memory configuration |
| 41 | RSTATU | read N500/N100 communication status |
| 54 | STSWAPPER | start swapper process |
| 55 | SPLACE | start place |
| 106 | LINKTO | link to process |
| 143 | MO5RT | activate ND-500 proc. / ND-100 prog. |
| 201B-214B | (high-code funcs) | encoded via the LDA form (section 2.3) |

The full list (RREG=0 ... FUNCMAX=177B, plus the 201B+ set) is in
5P-P2-MON60.NPL. The Background Monitor's user commands therefore map 1:1 onto
MON-60 subfunction codes, and 5P-P2-MON60.NPL is the resident dispatcher for
exactly those codes - this closes the user-side half of the interface.

### 2.5 Other MON usage in the binary (for completeness)

Two-bank total of word-aligned, instruction-context MON sites examined:

| Address | MON (octal) | Manual name | Context |
|---|---|---|---|
| ram:ccae | 60B | (undocumented; N500M) | driver call, section 2.2 |
| ram:a062 | 12B | SetCommandBuffer / SETCM | real instruction (STZ / LDA -0x7a,B / MON 12B) |
| ram:bd5f | 262B | GetSystemInfo | manual line 1080 |
| ram:d5cc | (none) | -- | FALSE POSITIVE: this is DATA (word 0xD66D), not a MON instruction - verified in listing |
| ram:4090, 90c0, 9372, 93ce | 113B / 104B | CLOCK / HOLD | time + suspend |
| ram:ce9a-ce9b | 2B, 65B | OUTBT, IOUT | console output |
| ram:d90b, d90e | 3B, 4B | (ECHOM/BRKM region) | terminal mode |
| ram:eec3-eec4 | 41B, 64B | ROBJE, (64B) | file info |
| ram:f0f3-f0fc | 143B, 214B, 217B | ExecutionInfo, GetUserName, GetAllFileIndexes | user/file info |
| ram:fc70-ff60 | 34B, 33B, 312B, 317B, 204B, 7B, 50B, 65B, 76B, 43B, 117B, 23B, 35B, 30B(?) | ALTOFF, ALTON, CheckMonCall, ExecuteCommand, ... | PLANC runtime + error handler ("NO ROUTINEERROR HANDLER" string at ram:fc24) |
| ram:fcb1-fe74 | 0,1,2,3,4,43B,50B,62B,64B,66B,70B,71B,72B,73B,74B,76B,104B,113B,117B,120B,143B,204B,317B | LEAVE, INBT, OUTBT, ... RFILE, WFILE, ... | MON stub library (one PLANC wrapper per call) |

(The stub-library numbers are read from the hexdump at ram:fcb0-ff64; each `d6 xx`
is preceded by parameter loads and followed by EXIT/error-branch words.)

**Conclusion for Q3:** the user-side monitor reaches the resident ND-500 driver
exclusively through **MON 60B with a parameter-block pointer in A**; everything
else is ordinary SINTRAN file/terminal/RT monitor calls. No XMSG (MON 200B) use
was found. UNVERIFIED: full parameter-block layout (first field offsets 0..6
partially visible: offset 6 = status; block built at B-0x6f..B-0x7b on the stack).

---

## 3. Q2 - Protocol constants from the L07 symbol table (corroborates dossier item 4)

NOTE: the current dossier (ND500-EVIDENCE-AND-CONTRADICTIONS.md item 4, RESOLVED
2026-07-08) already recovered these same values from the L07+M06 symbol tables.
The tables below are an independent re-derivation and match the dossier exactly.


Source: SINTRAN/NPL-SOURCE/SYMBOLS/L07/N500-SYMBOLS.SYMB.TXT
(line numbers below). IMPORTANT CAVEATS:
- Symbol names in the table are truncated to 5 characters; the full-name mapping
  (e.g. MSGN5 -> MSGN500) is an identification, not a certainty, though each is
  unique in the file.
- These are ONE source. The handoff (section 6.2, rule 5) requires a second,
  independent source (disassembly of this PROG or a live mailbox memory dump)
  before upgrading the dossier. Status of every row: SYMBOL-TABLE VERIFIED,
  SECOND SOURCE PENDING.

### 3.1 Message status codes (dossier open item: values of MSGN500 etc.)

| Symbol (full name) | Value (octal) | Symbol-file line | Manual hint match |
|---|---|---|---|
| MSGN5 (MSGN500) | 1 | 7064 | 1 = to-ND500: MATCH |
| WAITI (WAITING) | 2 | 2180 | 2 = in-process: MATCH |
| ANSWE (ANSWER) | 3 | 2798 | 3 = answer: MATCH |
| 5ERAN (5ERANSWER) | 4 | 1541 | 4 = error: MATCH |

The manual's 0-4 scheme (0=free) is fully consistent with these values.

### 3.2 MICFU microfunction codes

| Symbol (full name) | Value (octal) | Line |
|---|---|---|
| 3RMIC (3RMICV) | 1 | 4373 |
| 3SWME (3SWMESS) | 5 | 4595 |
| 3STAR (3START) | 23 | 3191 |
| 3MONC (3MONCO) | 24 | 4372 |
| 3TRAC (3TRACO) | 25 | 4371 |
| 3WMON (3WMONCO) | 26 | 4987 |
| 3FITR (3FITRNSF) | 27 | 3190 |
| 3RPRE (3RPREG) | 44 | 4593 |

### 3.3 Stop reasons

| Symbol (full name) | Value | Line | Docs claimed |
|---|---|---|---|
| MOCAL (MOCALL) | 1 | 5640 | 1 - CONFIRMED (docs claim now matched by symbol table) |
| TRAPC (TRAPCODE) | 2 | 276 | 2 - CONFIRMED |
| 5FMOC (5FMOCALL) | 3 | 1004 | 3 - CONFIRMED |

### 3.4 Swapper states

| Symbol (full name) | Value (octal) | Line |
|---|---|---|
| SWPWA (SWPWAIT) | 5 | 4130 |
| SWPPI (SWPPING) | 6 | 4837 |
| PSWWA (PSWWAIT) | 7 | 2297 |
| PSW1W (PSW1WAIT) | 15 | 2839 |

### 3.5 Field offsets (cross-check of dossier 2.6.2)

| Symbol | Value | Line | Dossier says |
|---|---|---|---|
| N5STA | 2 | 5746 | N5STA = offset 2: MATCH |
| MICFU | 6 | 5266 | MICFU = offset 6: MATCH |

### 3.6 DUMMESS - it is a runtime ADDRESS, not a status constant

The handoff (and dossier item 4) list DUMMESS among the "unknown-value"
constants. The reason no numeric value was found is that **DUMMESS is not a
compile-time constant at all - it is a variable holding the address of the
dummy message** that heads the ND-500 mailbox linked list:

- Declaration: `INTEGER DUMMESS   % Address of dummy msg`
  (DP-P2-VARIABLES.NPL:119).
- Usage: the mailbox is walked as a linked list via the LINK word
  (`T:=5MBBANK; *LINK@3 LDDTX` ... `WHILE D><-1`) and each node is compared
  `IF X:=D><DUMMESS THEN` to SKIP the dummy node (MP-P2-N500.NPL:581-584 "Skip
  dummy msg", and 5P-P2-MON60.NPL:1813-1816). So DUMMESS is a sentinel node
  pointer, allocated during buffer init, whose value is a mailbox-bank address -
  it varies per system and is only meaningful as an address, not an octal code.

Therefore "the value of DUMMESS" is not a fixed number to be recovered from
disassembly; it is resolved dynamically. This corrects dossier item 4's "Only
DUMMESS remains unknown" from an open constant to an explained category:
DUMMESS = address of the dummy/sentinel mailbox message (found via a live
memory dump at the MAILINK head, not as a symbol value).

(Aside: `7DUMM=000030` in N500-SYMBOLS is an UNRELATED symbol - do not conflate.)

---

## 4. Patch-list candidates for ND500-EVIDENCE-AND-CONTRADICTIONS.md

(Deliverable 4 format - for review, no dossier edits made. Aligned against the
CURRENT dossier, which already RESOLVED item 4 on 2026-07-08 via the same L07/M06
symbol tables - so sections 3.1-3.5 here are CORROBORATION of an already-closed
item, not a new claim. The genuinely new evidence is Q6, Q3, and DUMMESS.)

1. Sections 3.1-3.5 (status codes, MICFU, stop reasons, swapper states, offsets):
   independent re-derivation of dossier item 4 from N500-SYMBOLS.SYMB.TXT L07.
   All values match the dossier exactly. No dossier change needed; this is a
   second reader confirming the same table.
2. **Dossier item 4 "Only DUMMESS remains unknown" -> RECLASSIFY.** DUMMESS is a
   runtime address (dummy mailbox-message sentinel), not a missing constant
   (section 3.6). Suggested dossier edit: change "Only DUMMESS remains unknown"
   to "DUMMESS is not a constant - it is the address of the dummy/sentinel
   mailbox message (DP-P2-VARIABLES.NPL:119), read live at the MAILINK head."
3. **Spec section 3.3 (no user-side IOX): CONFIRMED by shipped binary** (section
   1). IOXT count = 0; all E8-EF word-aligned candidates inspected are PLANC
   pointer data. This is new binary evidence beyond the NPL-derived spec.
4. **NEW - user-side interface fully documented (Q3).** The Background Monitor
   reaches the resident driver ONLY through MON 60B, function code in A,
   dispatched by a stub array at ram:ccc8-ce6d whose SAA immediates ARE the
   5P-P2-MON60.NPL function codes (sections 2.2-2.4). Retry status ram:ccc4 =
   0x041A = ECSLOAD (control store must be loaded); the second retry word
   ram:ccc5 = 0x080F is UNRESOLVED (NOT PFECSLOAD - see corrected section 2.2).
   Two-source result (this binary + 5P-P2-MON60.NPL). Previously the user side was
   undocumented; the resident side (MCHANDLE/DECOMESS) was already in the dossier.
5. No XMSG (MON 200B) usage found in the background monitor - consistent with the
   ND-500 path being MON 60B, not XMSG.

---

## 5. Explicitly NOT determined in this session

- DUMMESS numeric value: N/A by nature - it is a runtime address, not a
  constant (section 3.6). Its actual value can only come from a live mailbox
  dump (MAILINK head), not from static analysis.
- Full MON 60B parameter-block layout (offsets 0..6 partially visible; offset
  6 = returned status; block built at B-0x6f..B-0x7b on the stack). The message
  field offsets themselves are already verified in dossier 2.6.2 (LINK=0,
  LINK2=1, N5STA=2, SENDE=3, X5CPU=4, X5ACT=5, MICFU=6).
- A live second source for the section 3 values (dossier already treats item 4
  as resolved from the symbol tables; the handoff 6.2 mailbox-dump would add a
  third, memory-level confirmation but was not attempted here).
- Q1 (211305 floppy inventory), Q4/Q5 (SWAPPER-K:PSEG/DSEG, Place Library) - not
  in scope of this Ghidra session; those artifacts were not opened.
