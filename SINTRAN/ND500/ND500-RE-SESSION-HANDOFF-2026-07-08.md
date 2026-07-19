# ND-500 L-Release RE - Session Handoff (2026-07-08)

**Audience:** the next LLM/human continuing the ND-500 support-software RE.
**Predecessor task:** [ND500-L-RELEASE-RE-TASK-HANDOFF.md](ND500-L-RELEASE-RE-TASK-HANDOFF.md)
(read its ground rules first - they still apply, especially "no speculation as
fact" and the poisoned-priors list).

This handoff records what THIS session did, what is verified, what was corrected,
and exactly where to pick up. Every claim is cited to a file+offset or .asm line.

---

## 0. TL;DR - what got closed this session

| Handoff Q | Status after this session |
|---|---|
| Q1 (211305 floppy inventory) | NOT done - artifacts were already extracted into F:\...\FLOPPY\500; a formal FILE-INFO listing is still owed |
| Q2 (protocol constants) | Corroborated (dossier item 4 already RESOLVED); DUMMESS reclassified as an address, not a constant |
| Q3 (MON 60B user-side interface) | DONE - fully documented incl. subfunction map |
| Q4 (swapper) | DONE (static) - entry, message dispatch, MON 377B gateway, RPHS/WPHS paging, DSEG map |
| Q5 / C9 (segment capability layout) | PARTIAL - bit NAMES + address widths recovered; numeric W/P/S positions still open |
| Q6 (IOX usage) | DONE - background monitor executes NO IOX (confirms spec 3.3) |

---

## 1. Tooling / environment state at end of session

- **Ghidra:** only `PLACE-BIG-2B-C01.BRF` is open (ND-100 BRF loader). The
  `ND-500-MON-J04.PROG` program was CLOSED after Q2/Q3/Q6 - reopen it to re-check
  anything in that binary. The BRF loader works and gives named symbols.
- **ND-500 disassembler:** the swapper PSEG was disassembled OUTSIDE Ghidra into
  `old/SWAPPER-K01.PSEG.asm`. Ghidra has NO ND-500 processor module - do not
  try to disassemble PSEG/DSEG in Ghidra.
- **ND-500 instruction reference (WSL):** `/home/ronny/repos/nd500x/docs/instructions/`
  - `asm/*.md` (241 hand-written), `instructions.json`, `instructions.md`.
  - Access from Git Bash via `wsl.exe -e bash -lc '...'`.
- **Symbol table:** `swapper/N500-SYMBOLS.SYMB` == byte-identical to
  `NPL-SOURCE/SYMBOLS/L07/N500-SYMBOLS.SYMB.TXT`. Resident-side symbols
  (`NAME=octal`, 5-char truncated). Authoritative for constants/offsets, NOT for
  swapper-internal 0x08xxxxxx addresses.

---

## 2. Documents produced this session (full paths)

1. `SINTRAN/ND500/ND500-MON-RE-FINDINGS.md`
   Q2/Q3/Q6 for the Background Monitor (ND-500-MON-J04.PROG). Has a second-reviewer
   block at the top; the retry-status error it flagged is now FIXED in-body.
2. `SINTRAN/ND500/old/SWAPPER-K01-ANALYSIS.md`
   Full swapper analysis (Q4) with Mermaid diagrams, DSEG hex map, ASCII field maps.
3. `SINTRAN/ND500/ND500-PLACE-LIBRARY-C9-FINDINGS.md`
   Q5/C9 partial: capability bit-name inventory + logical-address widths.
4. `SINTRAN/ND500/swapper/` also holds the copied binaries:
   `SWAPPER-K01.PSEG`, `SWAPPER-K01.DSEG`, `SWAPPER-K01.PSEG.asm`, `N500-SYMBOLS.SYMB`.

---

## 3. Verified facts (safe to build on)

### 3.1 Background Monitor (ND-500-MON-J04.PROG, ND-100 code)
- Identity: BANK2::2290 "ND-500/5000 MONITOR  Version J04".
- **Q6:** executes NO IOX. IOXT (0xD10D) = 0 matches; all word-aligned E8-EF
  candidates are PLANC pointer data. Confirms spec 3.3 with shipped-binary evidence.
- **Q3:** reaches the resident driver ONLY via **MON 60B** (ram:ccae), function
  code in A, dispatched by a stub array ram:ccc8-ce6d whose `SAA` immediates ARE
  the 5P-P2-MON60.NPL function codes (RREG=0 ... FUNCMAX=177B). Retry word
  ram:ccc4 = 0x041A = ECSLOAD. No XMSG (MON 200B) use.

### 3.2 Swapper (SWAPPER-K01, ND-500 code)
- Two-segment ND-500 domain, I/D split, both spaces based at **0x08000000**.
  DSEG offset = data-address - 0x08000000.
- Entry (INIT prologue) self-verifies its DSEG revision string "REV.-K01" at
  0x12818 and `MON 0B` EXITT on mismatch (.asm 16-40).
- Talks to the ND-100 ONLY through the **MON 377B** gateway (`call 0xF80000FF`),
  in a "try internal call, else trap" pattern.
- Dispatches messages: function code at DSEG 0x240B8 -> `jumpg $0x8026198+`
  through the ~29-entry handler table at DSEG 0x26190 (all PSEG 0x08008xxx).
- Manages ND-500 paging with the '87 privileged instructions **RPHS** (read from
  physical segment = page-in) and **WPHS** (page-out). Register contract in the
  analysis doc.
- MON 377B code word 0x12a20 = 0x427 = **SWPFATAL (2047B)**, sent on the fatal
  path (uniquely resolved via N500-SYMBOLS.SYMB).
- DSEG is ~99% zeroed BSS; the only ASCII in it: "REV.-K01" (0x12818),
  "12:41:57" build time (0x1287c), " 254 processes" log line (0x23e88).

### 3.3 Constants (N500-SYMBOLS.SYMB, = L07)
Message status: MSGN500=1, WAITING=2, ANSWER=3, 5ERANSWER=4. Field offsets:
N5STA=2, SENDE=3, X5CPU=4, X5ACT=5, MICFU=6. Swapper states: SWPWAIT=5,
SWPPING=6, PSWWAIT=7, PSW1WAIT=15B. MICFU codes 3RMICV=1 ... 3RPREG=44B. Stop
reasons MOCALL/TRAPCODE/5FMOCALL=1/2/3. (All already in dossier item 4/6.)

---

## 4. Corrections made this session (do not regress)

1. **Retry status ram:ccc5.** Earlier draft said 0x080F = PFECSLOAD. WRONG
   (base confusion). Bytes at ram:ccc1 are `... 04 1a 08 0f b4 78`, so
   ram:ccc4=0x041A (ECSLOAD, correct) and ram:ccc5=**0x080F** definitively.
   0x080F = 2063 decimal; PFECSLOAD = 2063 OCTAL = 0x0433 - different. 0x080F
   maps to NO MON-60 status symbol -> identity UNRESOLVED. Fixed in
   ND500-MON-RE-FINDINGS.md sections 2.2 and 4.
2. **MON 377B "selector table".** Earlier called it SINTRAN monitor-call numbers.
   WRONG - it is a table of small status/operation CODES (0x427 = SWPFATAL is the
   proof). Fixed in SWAPPER-K01-ANALYSIS.md section 5.2.
3. **DUMMESS.** Not a missing constant - it is an ADDRESS variable
   (DP-P2-VARIABLES.NPL:119 "Address of dummy msg"), the mailbox dummy-message
   sentinel. Reclassified in ND500-MON-RE-FINDINGS.md 3.6.
4. **DSEG false positive.** ram:d5cc "MON 155B" in the PROG was DATA (0xD66D),
   not an instruction.

---

## 5. Open items and EXACTLY how to continue

### 5.1 Finish C9 (highest value, PLACE-BIG already open in Ghidra)
Goal: numeric bit positions of W/P/S in the ND-500 capability word.
- The bit-NAME table is at ram:20e3-2199 (NUL separates distinct status words;
  `$` separates consecutive bits). Names verified in
  ND500-PLACE-LIBRARY-C9-FINDINGS.md.
- No direct xref to the table (computed base). NEXT: find the decoder loop -
  trace the LOOK-AT-PHYSICAL-SEGMENT / "Domain information table" dump handler
  (string ram:1e31) or the routine that loads an address in 0x2100-0x2140 and
  loops a shift/`BSKP` over the capability word. Read its mask/shift -> assigns
  bit numbers. Cross-check ND-05.009.4 MMS chapters (the authoritative source
  dossier C9 said it lacked).
- Evidence already favors C9 Claim A (11-bit segment field): the MMS index
  boundary is at bit 11 ("Logical address bits 26-11 ... index level 0",
  ram:1ea2). Do NOT upgrade C9 to RESOLVED until the mask is read.

### 5.2 Swapper MON 377B code words 1/2/4/5/6
Ambiguous small values. NEXT: read what each buffer (2nd arg) contains and which
handler issues it; correlate the 7-arg call (code 2, fixed param 0x8014CF8) with
the ND-100 5SWAP ABSTR path. A live dump (5.4) will show the actual message the
ND-100 receives.

### 5.3 Swapper handler semantics
~29 handlers at DSEG 0x26190 (PSEG 0x08008xxx). Only the dispatch mechanism is
mapped. NEXT: disassemble each handler in SWAPPER-K01.PSEG.asm (search the .asm
for the addresses 0x8008xxx) and label by what MON 377B / RPHS / WPHS it performs.

### 5.4 Dynamic second source (handoff 6.2, still not attempted)
Boot SINTRAN L in RetroCore with the 3022 stub, let XMSINIT run, dump the mailbox
(MAILINK head, DUMMESS sentinel) with the DAP/MCP debugger. Confirms the section
3.3 constants at memory level AND reveals the runtime MICFU/status the swapper
writes (5.2).

### 5.5 Q1 formal inventory
Produce a FILE-INFO-style listing of 211305B02-XX-01D.img (the files are already
extracted to F:\...\FLOPPY\500, but the formal inventory deliverable is owed).

### 5.6 The 0x080F retry code
Identify what MON-60 return value 0x080F (2063 decimal) is. Not in the
5P-P2-MON60.NPL 2000B-2136B status block. Check other symbol tables / the ND-500
Loader-Monitor manual, or trace who RETURNS 0x080F on the resident side.

---

## 6. Dossier patch-list still pending review

From ND500-MON-RE-FINDINGS.md section 4 (not yet applied to the dossier by this
session's author; the second reviewer applied items 4/6/7/C8/C12 - verify before
duplicating):
- Spec 3.3 "no user-side IOX" -> CONFIRMED by shipped binary (Q6).
- New: user-side interface = MON 60B + subfunction stub array.
- C9: add the capability bit-NAME inventory + bit-11 index evidence as PARTIAL
  progress (NOT a resolution).
- New: swapper uses MON 377B + RPHS/WPHS; reports SWPFATAL on fatal path.

---

## 7. Gotchas for the next session

- ND-500 is big-endian, byte-addressed, 32-bit, descriptor-based, split I/D.
  In the .asm: `call 0x0800xxxx` = I-space code; `$0x0800xxxx` operand = D-space
  data (offset = addr - 0x08000000); `call 0xF80000xx` = MON trap.
- The symbol table names are TRUNCATED to 5 chars; unique-value matches are
  trustworthy, small-value matches (1..~50) are ambiguous - do not name from them.
- Distrust the fabricated "TAG protocol" sources (predecessor handoff 6.1). None
  of this session's findings used them.
- Nothing found this session contradicts ND500-BUS-INTERFACE-REFERENCE.md.
