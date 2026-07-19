# CARVE ANSWER: the "work-in-progress" flag of the looping RT program

Answers to [QUESTION-FOR-CARVER-FS-RT-FLAG.md](QUESTION-FOR-CARVER-FS-RT-FLAG.md).
SINTRAN III L07 static carve, L-VSX-500 segment set. Every claim is tagged:

- [V] = VERIFIED from carved bytes (word values reproduced from the segment files;
  all key words re-read and listed in the evidence appendix).
- [I] = INFERRED (stated basis; not byte-proven).

IMPORTANT DELTA vs the question's premises: the live system is NOT running the same
link image as this carve. The code is the same library logic, but link addresses and
even one B-frame offset differ (details in section 6). All carve addresses below are
virtual addresses in the carved segment's own space; anchor live addresses by the hex
fingerprints given, never by constant offset.

---

## TL;DR for the live session

1. The "main loop at 0x649B" is the background command processor's LOGOUT routine
   (SYMBOL-1-LIST name BLOGO), not a file-system request dispatcher. It lives on the
   command segment (003-S3CP / 013-S3SCP), carve VA 066663B, loop body 066671B. [V]
2. The flag at B-103B is the background processor's session-state word.
   Values seen in bytes: 0, 1, 2, -1. 1 = logged in; -1 = logout/cleanup in
   progress AND the logged-out resting state; 2 = request posted (transitional);
   0 = cleared/idle. [V for the writers; semantics I from context]
3. THERE IS NO "-1 -> done" WRITER ANYWHERE. An exhaustive opcode sweep of the
   command segment for every store to ,B -103 finds 11 writers; every writer that
   stores 0 is gated on flag==2 (or "flag even"). Nothing ever transitions -1 to
   anything except LOGIN success (writes 1) or the loop itself (rewrites -1). [V]
4. Therefore the loop is not "missing a done-write": the re-run is driven by
   RE-ACTIVATION. A router at carve 050045B-050053B jumps to BLOGO on EVERY
   activation of the program while flag is 1 or -1 (via pointer cell 050200B which
   contains 066663B = BLOGO). Each activation with flag==-1 re-runs RELEASE-USER
   by design. On working hardware the cycle ends because the activation source
   stops, not because the flag changes. [V for the router; the emulator bug is
   therefore: find what keeps re-activating the program ~4/s.]
5. RLUSE (FILSYS 115020B) decrements the enter count exactly once per call, with
   NO zero guard and NO termination test. The "check call" before the decrement is
   RUSER = read-user-entry (053246B); its skip-return means "entry read OK", so it
   always proceeds. Termination was never RLUSE's job. [V]

---

## 1. Q1: what routine is the main loop? (and a poisoned prior)

[V] The loop bytes exist at exactly two places in the entire L-VSX-500 segment set:

| Segment | load base | file word offset | carve VA |
|---|---|---|---|
| 003-S3CP.bin (command segment) | 30000B | 036671B | 066671B |
| 013-S3SCP.bin (save copy) | 26000B | 040671B | 066671B |

Fingerprint searched: 21CB 01D6 49BD F201 C035 A816 (hex code words of runtime
0x649B-0x64A0; all position-independent). Both copies are byte-identical.

[V] SYMBOL-1-LIST names for this cluster land coherently on entries (same
validation style as the COMSB/UECOM/UELOG anchor for 003-S3CP):

- BLOGO = 066663B - routine entry, 6 words before the loop ("Background LOGOut")
- XBLOG = 041554B - EXACTLY the address in pointer cell 066754B that the loop's
  tail jumps to (JMP I at 066741B). This exact-match is the strongest anchor.
- LOGOU = 067101B, XLOGO = 067161B, ALOGO = 067235B, BILCM = 066755B,
  BILPA = 066757B - the rest of the logout/accounting family, all on boundaries.

[POISONED PRIOR - delete from the live session's notes] "main loop ~ CHNUS+25B" is
a numeric coincidence. FILSYS-SYMBOLS link space equals the 006-S3FS carve VA space
(proof: RRLUS=115016B and RLUSE=115020B land exactly on a twin BSET ONE SSK /
BSET ZRO SSK entry pair [V]), and the loop bytes do NOT exist anywhere in 006-S3FS
[V]. So the loop is not CHNUS and not FILSYS code at all.

[I] What the live session calls "the FILE SYSTEM RT program" is the background
command processor's logout machinery. Which RT description (FSYRT=043262B or a
background program) actually executes it is a RUNTIME question - read the RT
description the scheduler dispatches and check its segment numbers.

## 2. Q2: the state flag at B-103B

[V] It is a word in the background processor's datafield, addressed as ,B -103
(octal) by every reference, in both carve copies and in the live dump (49BD /
09BD / 01BD are LDA/STA/STZ ,B -103). No symbol list available to us names the
datafield fields; the symbol name is UNKNOWN - do not invent one.

Value semantics (writers are byte-cited in section 3; meanings [I] from context):

| Value | Meaning |
|---|---|
| 1 | logged in - BLOGO polls the request word only in this state |
| -1 | logout/cleanup in progress; ALSO the resting logged-out state |
| 2 | request posted / transition pending (set by the MODE and LOGDI posters) |
| 0 | cleared; router falls through toward fresh-session handling |

## 3. Q3: EVERY writer of the flag (complete opcode sweep)

[V] Sweep method: all 16-bit words equal to op|0675B for op in {STZ, STA, STT,
STX, STD, STF, MIN} over 003-S3CP.bin (013-S3SCP is byte-identical at the same
VAs). Result - 11 writers, nothing else:

| Carve VA | Writes | Precondition tested immediately before | Region (SYMBOL-1-LIST) |
|---|---|---|---|
| 047500B | 0 | flag >= 0 (JAN skips) AND flag == 2 | N5FU1 area |
| 050060B | 0 | flag == 2 (then a resident call via ptr 050201B=026750B) | DUSSU router |
| 060647B | 0 | flag == 2 | just after LOGIN/ELEA3 |
| 065222B | 0 | none (init/reset path; also clears ,B -141 and ,B -140) | UUELO/RDEL area |
| 066606B | 0 | flag == 2 (else error 174B); then MON 43 CLOSE loops | MLLBR |
| 067173B | 0 | flag bit0 == 0, i.e. flag in {0,2}; then gate-call FILSYS RUSCN | XLOGO |
| 061722B | 1 | login success path (gate call with inline arg 114340B = ENFUS nearby) | CCCOM |
| 066725B | -1 | flag != 1 on loop entry (the observed re-assert) | BLOGO |
| 067010B | -1 | terminal path: then MON 200 XMSG x2, clears element words, clears a bit in MEM[MEM[4007B]+1], MON 105 ABORT (self-terminate) | BLOGO tail family |
| 102677B | 2 | only if flag == 0; stores request word first | MODE poster |
| 103001B | 2 | unconditional; stores request word first | LOGDI poster |

[V] Conclusion: there is no instruction in the command segment that moves the flag
from -1 to a "done" value. The only exits from -1 are: LOGIN writes 1 (061722B),
or the program self-terminates with the flag still -1 (067007B-067041B: SAA -1;
STA ,B -103; ...; MON 105). -1 is the intended resting value after logout.

### What actually drives the re-runs [V]

The router (in the DUSSU region):

```
050045  044675  LDA ,B -103        ; flag
050046  171001  SAT 1
050047  142065  SKP IF DA UEQ ST   ; skip if flag != 1
050050  124003  JMP -> 050053      ; flag == 1
050051  171377  SAT -1
050052  142065  SKP IF DA UEQ ST   ; skip if flag != -1
050053  125125  JMP I 125          ; -> ptr 050200B = 066663B = BLOGO
050054  171002  SAT 2
050055  140065  SKP IF DA EQL ST
050056  124041  JMP -> 050117      ; flag == 0 path
050057  135122  JPL I 122          ; flag == 2: call via ptr 050201B = 026750B (resident)
050060  000675  STZ ,B -103        ; then flag := 0, fall through
```

Hex fingerprint for live anchoring: 49BD F201 C435 A803 F2FF C435 AA55.

Every activation of the program that reaches this router with flag == -1 jumps to
BLOGO, which re-asserts -1, gate-calls RLUSE (inline argument word 066734B =
115020B = RLUSE [V]), then jumps out to XBLOG (041554B). XBLOG scans a chain
(head loaded indirectly from resident cell 004007B [V, carve-copy literal]) and
conditionally re-starts an RT program with MON 100 at 041617B [V] when the word at
a resident cell (carve-copy literal 004153B) is nonzero. Guard conditions of the
router entry (050024B-050044B, bit tests on globals) are only partially decoded [I].

### Live breakpoint recipe

1. Find the router by the hex fingerprint above; breakpoint the JMP I (AA55 word).
   Confirm every ~250ms activation flows flag==-1 -> BLOGO.
2. Breakpoint their XBLOG-equivalent MON 100 (fingerprint 5810 4C00 B205 1983
   490D CC1D D640) and the load of the activation cell just before it - find WHO
   keeps making the program runnable (that is the actual emulator bug; suspects:
   a timer element never cleared, or an element word among X+15/X+22/X+26/X2+23/
   X2+24 that BLOGO clears but something re-sets).
3. The flag itself will never change - stop watching it for a "done" write; none
   exists.

## 4. Q4: RELEASE-USER internals and intended termination

[V] The decrement block is inside RLUSE (FILSYS/006-S3FS space):

```
115016  174220  RRLUS: BSET ONE SSK      ; twin entry
115020  174020  RLUSE: BSET ZRO SSK      ; twin entry
...
115035  JPL I -> 053174B TUSEN            ; look up user; error -> exit A := -12B
...
115055  JPL I -> 053246B RUSER            ; READ the user entry (the live "check
                                          ; call"); skip-return = read OK
115057  046025  LDA ,X 25                 ; user entry word +25B (flag+enter count)
115060  172777  AAA -1                    ; decrement
115061  070124  AND <lit>                 ; keep low byte
115062  146151  RADD CLD SA DD
115063  046025  LDA ,X 25
115064  070122  AND <lit>                 ; keep high byte
115065  146015  RADD SD DA
115066  006025  STA ,X 25                 ; store back - NO zero guard
115067  044407  LDA ,B 7
115070  JPL I -> 053410B WUSER            ; WRITE the user entry back
```

Answers to the specific alternatives in the question:

- Stop at enter-count == 0? NO. There is no zero test anywhere around the
  decrement. [V]
- Error return from the check call? The check call is RUSER = read-user-entry
  (053246B); it fails only if the entry cannot be read. It is not a termination
  condition and on a healthy volume it always proceeds. [V for identity;
  "not a termination condition" I]
- Iteration counting by the RT program? NO counter found; the repetition is purely
  activation-driven through the 050045B router. [V for the router; absence of a
  counter is V within the swept code, I globally]

So the intended termination of a logout's release is: RLUSE runs ONCE per posted
logout; the program then rests with flag == -1 until the next LOGIN. A "stale
enter count" from a crash is simply decremented by one at the next logout - there
is no scan-until-zero loop in this code. [I on intent, V on mechanics]

Note: RUSCN (FILSYS 106562B, gate-called from XLOGO with inline arg 067175B =
106562B [V]) is the INCREMENT twin - same read-modify-write shape on entry word
+25B with AAA 1 at 106636B-106645B [V].

## 5. Q5: the 0120B / 0121B comparison

[V] What the loop compares: element word X2+23 (X2 = MEM[MEM[B-146]+12]) against
constants 120B then 121B (SAT 120 / SAT 121 at 066707B/066712B); no match -> error
code 14B; match -> gate call with inline arg 066720B = 106562B = RUSCN (the
enter-count INCREMENT).

[V] What gets stored in that word: the posters write A into X2+23 and 1 into
X2+24, then set flag := 2. In the MODE poster the value stored when flag != 1 is
,B -147 - the same cell used as the logical device number for MON 13 CIBUF at
047462B-047463B. So the "request word" carries a LOGICAL DEVICE NUMBER, not a
command code.

[I] Therefore 120B and 121B are the logical device numbers of the two units
allowed to post this request (batch/MODE processors is the natural fit, since the
posters are the MODE and LOGDI routines and the accepted action is RUSCN =
enter-count increment for a job starting under a user). NOT byte-proven which
devices 120B/121B are - do not treat the batch identification as fact.

## 6. Live image vs carve: they are different builds [V]

- The live loop dump and the carve agree on every position-independent word but
  differ in P-relative literal displacements (e.g. live 4826 vs carve 044045B at
  loop+6).
- In RLUSE the live block's B-frame offsets are all one less than the carve's
  (live STX ,B 8 / LDT ,B 7 / LDA ,B 6 vs carve STX ,B 11 / LDT ,B 10 / LDA ,B 7)
  and the live AND literals sit at 7050/704E vs carve 070124B/070122B.
- In-page offsets of the loop differ (live 0x9B vs carve 0x1B9 within a 1KW page),
  so the executing image cannot be a page-mapped view of either carved file.

Conclusion: the boot disk's SINTRAN generation is not the generation this carve
came from. Same sources, different link. Anchor everything by fingerprint.

## 7. Honest gaps

- Which RT description dispatches this code at boot (FSYRT? a background
  processor?) - runtime question.
- Who sets the activation cell (carve literal 004153B) and the chain at 004007B -
  resident data, outside the swept segments.
- Router entry guards 050024B-050044B and the XBLOG scan loop are only partially
  decoded.
- The identity of devices 120B/121B - inferred, not proven.
- FILSYS symbol for the flag word - unknown; the datafield field lists are not in
  our symbol files.

## Appendix: evidence register (all words re-read from the .bin files)

003-S3CP.bin, load base 30000B (word offset = VA - 30000B):

| VA | word | meaning |
|---|---|---|
| 050053B | 125125B | router JMP I -> BLOGO |
| 050200B | 066663B | pointer cell = BLOGO |
| 061721B | 170401B | SAA 1 (login) |
| 061722B | 004675B | STA ,B -103 (flag := 1) |
| 066724B | 170777B | SAA -1 |
| 066725B | 004675B | STA ,B -103 (flag := -1, the loop) |
| 066734B | 115020B | inline gate arg = RLUSE |
| 067005B | 115020B | inline gate arg = RLUSE (abort path) |
| 067274B | 115020B | inline gate arg = RLUSE (LOGOU path) |
| 067010B | 004675B | STA ,B -103 (flag := -1, abort path) |
| 041617B | 153100B | MON 100 (XBLOG re-start) |
| 102676B | 170402B | SAA 2 (MODE poster) |
| 103000B | 170402B | SAA 2 (LOGDI poster) |

006-S3FS.bin, load base 26000B:

| VA | word | meaning |
|---|---|---|
| 115016B | 174220B | RRLUS entry (BSET ONE SSK) |
| 115020B | 174020B | RLUSE entry (BSET ZRO SSK) |
| 115057B | 046025B | LDA ,X 25 (enter-count word) |
| 115060B | 172777B | AAA -1 |
| 115066B | 006025B | STA ,X 25 (no zero guard) |
| 115176B | 053174B | pointer -> TUSEN |
| 115204B | 053246B | pointer -> RUSER (the "check call") |
| 115207B | 053410B | pointer -> WUSER |

Symbol sources: SYMBOL-1-LIST / SYMBOL-2-LIST / FILSYS-SYMBOLS, all L07, in
[../../NPL-SOURCE/SYMBOLS/L07/](../../NPL-SOURCE/SYMBOLS/L07/).
