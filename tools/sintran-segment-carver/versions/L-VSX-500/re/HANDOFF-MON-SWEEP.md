# HANDOFF - NC/LINKER MON-call sweep (resume point)

Goal: golden-path folder for **every** distinct MON call used by `nc-a06.dom` and
`linker-b01.dom` (the friend's two disassembled programs). **64 distinct calls.**
Every folder follows [`mon-analysis/GOLDEN-PATH.md`](mon-analysis/GOLDEN-PATH.md);
every `.pseudo.c` must be grounded in the instruction-semantics reference (below).

Authoritative worklist (all 64 with canonical names) is confirmed - see the user's
list in the session and `MON-CALL-INDEX.md`. Delivery mirror: `/mnt/d/ND/t`.

---

## State (as of last commit 4bfd392)

**DONE - all 64 NC/LINKER folders carved & committed** (real SINTRAN L bytes, honest
CODE/DATA/NOT-CARVED verdicts). The sweep (task #20) is COMPLETE. Documented-absent
(README-only, byte-proven empty/uncarved): 422B 423B (+ the pre-existing 042B 425B 426B 427B).
Honest "worker name-only / ND-500-companion-symbol only / zero-filled" documented calls:
12B 30B 71B 72B 104B 142B. Everything else is a real carved worker or dispatch thunk.

**pseudo-C**: every folder with a `.pseudo.c` is grounded in the instruction-semantics reference
(task #26 complete). Recurring bugs the re-audit fixed are catalogued below.
- ND-100 (21): 0B ExitFromProgram, 1B InByte, 2B OutByte, 12B SetCommandBuffer,
  16B GetTerminalType, 17B SetTerminalType, 30B GetOwnRTAddress, 32B OutMessage,
  53B GetSegmentEntry, 71B DisableEscape, 72B EnableEscape, 76B SetBlockSize,
  104B SuspendProgram, 142B ToErrorDevice, 214B GetUserName, 254B GetErrorDevice,
  263B GetDeviceType, 312B CheckMonCall, 317B ExecuteCommand, 321B UEAdministrator,
  336B Terminal.
- ND-500 (2): 422B GetScratchSegment, 423B CopyCapability - **no S3SM5 vector found**
  (like 425/426/427 they are likely serviced ND-100-side / vector 0x0000). Investigate
  their real dispatch before carving; do NOT fabricate a handler.

**pseudo-C RE-AUDIT (task #26) - COMPLETE.** Every folder that has a `.pseudo.c` is now
grounded in the instruction-semantics reference and its README links it (verify:
for each folder with a `*.pseudo.c`, its `README.md` greps `instruction-semantics`).
The 4 README-only stubs (042B 425B 426B 427B) have no pseudo-C and are exempt.
The re-audit found and fixed real defects in the large majority of handlers - dominant
classes: inverted skip-sense branches; bare `LDA/LDT/ADD disp` read as literals instead of
`mem[P+disp]` (P-relative); A/D and B/D register confusions; invented ND-500 domain semantics
on misaligned/undecodable bytes; wrong error-code selectors; `MIN` modeled as decrement
(it increments); a fabricated `ION()`; a hidden U+200B. Zero/uncarved worker regions were
reconciled to the honest "not byte-recoverable / name-only" standard (11B 113B 122B 123B 322B).
Also stripped disassembler-plumbing (byte-swap) wording from 8 `.ASM` headers.

---

## The instruction-semantics reference (committed 8c70d18) - MUST ground all pseudo-C

`re/instruction-semantics/`:
- `ND100-INSTRUCTION-SEMANTICS.md` - authoritative, synthesised from the nd100x **emulator**
  (`~/repos/nd100x/src/cpu`, executable ground truth) + **Ghidra SLEIGH**
  (`/mnt/e/Dev/Ronny/ghidra-nd100/ND-100/data/languages/nd100*.sinc`). Agree = VERIFIED;
  emulator wins on conflict. Source docs: `ND100-SEMANTICS-FROM-EMULATOR.md`, `-FROM-GHIDRA.md`.
- `ND500-INSTRUCTION-SEMANTICS.md` - from `~/repos/nd500x/src/cpu/instructions/`.
- ND-100 CPU manual: `/mnt/e/Dev/Ronny/nd100-markdown/docs`.

**Verified opcode facts that guessing gets WRONG (the whole point of the re-audit):**
- `RADD CLD SD DA` = `A = D` (CLD zeroes the dest operand, source=reg[D]). `COPY`=`RADD CLD`;
  `RSUB`=`RADD AD1 CM1`. Register letters: STS0 D1 P2 B3 L4 A5 T6 X7.
- **T/X transfers (STATX/LDATX/LDDTX/STDTX/STZTX/LDBTX) are 24-bit PHYSICAL, bypassing the MMU:**
  `EL = ((T & 0377) << 16) | ((X + disp3) & 0177777)`, `disp3=(operand>>3)&7`. T = the bank
  (usually `5MBBANK`, the ND-500 message buffer); X = word offset. disp3 is USUALLY 0 (X pre-set
  by AAX) but NOT always - 512B has one STATX with disp3=6. Verify per opcode.
- `RADD SA DP` (no CLD) = `P = P + A` (computed jump - drives the 512B/513B XMSG 48-slot table).
- SWAP true-exchange; LIN right-shift fills from the M/link bit; BSET BAC K-source; **STBTX is illegal**
  (never emit). Bit ops: the disassembler prints the bit number as `bn<<3` (e.g. `BSKP ONE 170` = bit 15).
- ND-500: flag `C=1` means **no-borrow** (inverted). 416B/417B (partly 412B) are mid-block/misaligned
  carves - raw bytes are ground truth but decoded mnemonics at wrong alignment are unreliable.

Real errors the re-audit already caught: 504B entry `RADD CLD SL DA` was `A=L` (guessed "compose
address in A"); 512B/513B computed jump `RADD SA DP`=`P=P+A` was mislabeled.

---

## How to carve a remaining TODO call (the proven recipe)

1. `python3 scripts/prove-mon.py <N>` (octal digits) -> `GOTAB[N]`: a stub (F16xx in
   025-S3IRPIT, load 32000B=13312dec) or `000000` fall-through. Byte-proven.
2. Find the worker: search the segment symbol tables for the mnemonic
   (`re/segments-ref/*/**.symbols.txt`). File-system workers are in 006-S3FS (load 26000B=11264dec,
   FILSYS-SYMBOLS); resident workers in commoncode (load 0, SYMBOL-1-LIST); terminal/UE too.
3. Disassemble worker (and stub) to CONTROL-FLOW CLOSURE: byte-swap the bin->LE, then
   `/home/ronny/repos/nd100-tools/nd100-dis/nd100-dis -a -o -b <loaddec> <le> | awk '$1>="lo"&&$1<="hi"'`;
   bound by the next symbol. (ND-500 S3SM5: nd500-dis on the big-endian bin, no swap.)
4. **CODE-vs-DATA / zero check (mandatory honesty):** if the worker region is all-zero it is NOT
   byte-recoverable (uncarved resident range) - flag NOT CARVED, do not call it code or data
   (see 122B/123B). If it decodes as a table, it is DATA - model documented behaviour only, flag it
   (see 322B). Only real instructions get a full pseudo-C.
5. Build the folder per GOLDEN-PATH: README.md + `<call>.ASM` (reference excerpt, no per-call .bin
   for ND-100) + `<call>.pseudo.c` (grounded in the reference - use 503B/511B as the model with the
   `ELADDR()` macro and phys[] transfers). Add a MON-CALL-INDEX.md row (status `partial` for
   fall-through, `misattributed` for stub-routed).
6. Validate: `python3 scripts/validate-mon-carves.py <folder>` (ND-100 .ASM correctly SKIPS as a
   reference excerpt); `python3 scripts/mon-status-report.py` (must show `complete`); `test -e` every
   README link. Then commit.

Batch via subagents (2 calls/agent works well) - see the agent prompts used this session (the
503B/511B/50B/43B prompts are the template). ALWAYS verify the bytes on disk yourself before
committing (sha of the carved region vs the canonical segment slice; validate closure).

---

## Anchors already resolved for some TODO calls
- 30B GetOwnRTAddress: GETRT/RTADR not in commoncode/006-S3FS symbols - search wider (may be data
  like TIME/CLOCK, or an uncarved resident routine).
- 263B GetDeviceType -> GDEVT=107104B (006-S3FS, SYMBOL-2-LIST; near GDIEN=107111B).
- 317B ExecuteCommand -> UECOM=50701B (commoncode); 321B UEAdministrator -> UEADM=65453B (commoncode).
- 0B ExitFromProgram -> LEAVE=144142B (commoncode). 32B OutMessage -> MSG=102453B (SYMBOL-2-LIST).
- 76B SetBlockSize, 53B GetSegmentEntry, 12B SetCommandBuffer: symbols ambiguous/only-ND-500 -
  search harder or expect DATA/uncarved.
- Terminal byte-I/O (1B INBT, 2B OUTBT, 16B/17B MGTTY/MSTTY, 71B/72B DESCF/EESCF, 336B IOMTY): low-level
  terminal-driver calls; workers likely in commoncode and several may be zero/uncarved - stay honest.

---

## Communication group (task #21) - COMPLETE
All comms MON calls carved & committed: 200B XMSGFunction, 201B HDLCfunction, 255B PIOCFunction,
65B ErrorMessage, 334B GetErrorMessage, 305B GetSIBASMessage, 316B SetRemoteAccess,
314B DefaultRemoteSystem, 432B SIBASFunction (+ earlier 304B 506B 512B 513B 32B 64B). Honest
CODE/DATA/NOT-CARVED/UNVERIFIED verdicts; pseudo-C reference-grounded. (Details below were the
in-progress notes.)

### (historical in-progress notes)
Scoped comms MON calls (beyond the 64): 200B XMSGFunction, 201B HDLCfunction, 255B PIOCFunction,
65B ErrorMessage, 334B GetErrorMessage, 305B GetSIBASMessage, 316B SetRemoteAccess,
314B DefaultRemoteSystem, 432B SIBASFunction. Already done earlier: 304B SendSIBASMessage, 506B
AnswerSIBAS, 512B/513B XMSGCallA/B, 32B OutMessage, 64B WarningMessage.
- DONE & committed: **200B** (XMSG - worker uncarved/documented; T=XF* codes from SINTRAN/XMSG),
  **255B** (PIOCM=114120B, 025-S3IRPIT, real code).
- NEEDS CLEANUP (folder complete but the interrupted agent left byte-swap wording in README/ASM;
  316B also has a broken README link): **305B** (worker via GOTAB[305]=111752B stub), **316B**
  (GOTAB[316]=112355B stub). Re-run the carve OR just strip the byte-swap wording + fix links, verify,
  add index rows, commit.
- INCOMPLETE (partial files on disk, finish or regenerate): **201B** (HDLC=103112B in 006-S3FS -
  has .ASM+.pseudo.c, needs README + index row - USER-FLAGGED IMPORTANT), **314B**
  (GOTAB[314]=112326B=DSI4 diagnostic stub, SRUSI is ND-500-only - has .ASM only).
- TO BUILD (empty folders): **65B** ErrorMessage (sibling of 64B ERMSG=16714B), **334B**
  GetErrorMessage (GOTAB=0 fall-through), **432B** SIBASFunction (GOTAB[432]=056524B).
Anchors: 201B->HDLC=103112B(006-S3FS); 65B->GOTAB 121345B stub + ERMSG=16714B; 305B->GOTAB 111752B;
316B->GOTAB 112355B; 314B->GOTAB 112326B(DSI4); 432B->GOTAB 056524B; 334B fall-through.

## ROUND 2 - broader documented universe (user-selected groups, "validate these first")
Scope this round (~60 calls): **group 1 FILE-SYSTEM** (121 220 231 232 233 235 236 237 252 253 274),
**group 2 DIRECTORY/USER** (44 56 57 152 170-177 213 215 216 234 241 242 243 245 246 247 250 260 275
311 405 420), **group 3 DISK-IO/PAGES** (7 10 33 34 77 157 251 270 271 272 331 333 345), **group 10
ND-500** (60 264 265 266 436 437), plus **324B OctobusFunction** + **155B GraphicFunction**.
NOT MON calls (device drivers, tasks #15/#16 - not this): floppy / SCSI / CD-ROM. User validates
this round before the remaining ~107 groups are considered.
File-system anchors (006-S3FS, load 26000B=11264dec): EXPFI=105555(231), DOPEN=103026(220),
SCROP=103031(235), SETPO=72465(236), SETTF=106043(233); GOTAB stubs: 232->066172, 236->066202,
252->066252; fall-through: 220 231 233 235 237 253 274; 121->122013 stub. Full survey/grouping was
generated from Developer/MON/calls/*.yaml vs re/mon-analysis/ (261 documented, 96 carved, 167 uncarved).

## Also pending
- **Zero-region reconciliation:** 11B/113B (TIME/CLOCK) were framed as "DATA counters" but the regions
  are all-zero (uncarved) - same as 122B/123B. Soften 11B/113B to the honest "zero, not byte-recoverable,
  name-only" standard during their pseudo-C re-audit.
- Delivery sync to `/mnt/d/ND/t` after a milestone (mirror layout; EXTRACTING link rewritten to ../..).

## Constraints (always)
Never mention any AI tool in files or commits. Full absolute paths when talking to the user. Relative
links inside markdown. "real SINTRAN L bytes" (never bare "real L bytes"). No byte-swap/little-endian
wording in .ASM or reader prose ("big-endian word" in a raw dd verify line is OK). No unicode in .c/.ASM.
Bytes are ground truth, NPL is a different revision. Verify, don't claim.

## Session-limit note
Account API session limit was hit twice this session (rolling window, resets ~hourly). Subagents die
mid-run on it; when that happens, check `git status` for partially-written files, verify soundness
(balanced braces, reference link, not truncated), finish the small gap by hand, and commit.

## Key commits (this session)
951c0cf 503/504 · dba4993 412/413/514/320 · b0105be batch2 fs · 50b6c05 batch3 fs ·
f8ea2f5 batch4 · b460cac batch5 time · 66c965e batch6 · 8c70d18 instruction-semantics reference
+ golden-path rule · 64f4e14 re-audit 503/511 · 4bfd392 re-audit 504/510/512/513/514/515.
