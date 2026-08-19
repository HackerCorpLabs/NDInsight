# CARVE ANSWER — FPRSTART's 200B "stop (trap) info" block: the source is message halfwords 12B..41B, copied by the System Monitor's trap-stop builder

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVE-ANSWER-PRSTART-STOPINFO-SOURCE-2026-08-11.md`
**Date:** 2026-08-11.
**Question:** `CARVE-ANSWER-RESULT-BLOCKS-2026-08-10.md` §2 pinned RUNN's (MON 60B fn 012)
answer as `5DD1 -> [5P1]` (stop reason) plus `TOUSMOVE(dest=5P2, len=200B)` described as
"stop (trap) info". WHICH source buffer/words feed that 200B copy, and where is it built?

**Sources and grades:**
- **[V]** `tools\sintran-segment-carver\versions\L-VSX-500\segments\030-S3SM5.bin` (base
  `40000B`, big-endian) + `re\030-S3SM5.dis` — the ND-500 System Monitor. Every pool word
  cited was re-read from the raw bytes this session (§5).
- **[NPL]** `SINTRAN\NPL-SOURCE\NPL\5P-P2-MON60.NPL` (address-stamped listing) — the MON 60B
  worker side (`FPRSTART`, `TOUSMOVE`, `5PT2RET`).
- Frame map and helper identities: `CARVE-ANSWER-RESULT-BLOCKS-2026-08-10.md` §1
  (`B-11` = info block, `B-67` = message window address, `B-65` = ABUFA).

---

## 0. TL;DR

**The 200B copy's source is the MON 60 com-buffer** (`TOUSMOVE` always copies from the
buffer window at `LOGBADR` [NPL 027633-027647]), **and the buffer is filled by the System
Monitor's trap-stop builder at `064415-064426` in `030-S3SM5`: a `MOVEW` of `30B` (=24
decimal) halfwords from MESSAGE offset `12B` into buffer word 0.** [V]

So the user's 200B (=128-byte, 64-word) "stop (trap) info" block is a verbatim copy of the
activation-message trap record starting at the trapping-P field:

| block word (user side) | = message hw | content (trap stop; see the offsets carve) |
|---|---|---|
| 0-1 | 12B-13B | trapping P (32-bit) |
| 2-3 | 14B-15B | restart P (32-bit) |
| 4 | 16B | TRAPN |
| 5-6 | 17B-20B | fault LA (both trap classes) |
| 7 | 21B | page fault: physical segment / GEN3 class: MMS status hi |
| 8-9 | 22B-23B | page fault: MMS status / GEN3 class: MMS lo + phys addr hi |
| 10-14 | 24B-30B | GEN3 class: phys addr lo, phys seg, WR, ASTS, BADAP |
| 15-23 | 31B-41B | rest of the copied window (moncall-parameter region head) |
| 24-63 | — | **NOT written by this path** — stale com-buffer content [V: no clear in the builder] |

Per-trap word identities: `CARVE-ANSWER-TRAP-RECORD-OFFSETS-AND-MMS-BITS-2026-08-11.md`.

**Bonus byte-pin:** the RUNN "stop reason" `5DD1` is **`(0, TRAPN)`** for trap stops —
`5D12 := A` (= msg[16B], carried from the builder) and `5D11 := 0` at `065141-065143` [V].

---

## 1. The chain, end to end

```
caller ND-500-MON               MON 60B fn 012 (RUNN), params incl. 5P1 (stop reason addr),
                                5P2 (200B stop-info buffer addr)
 -> N500M / 5IFUNC[012]=5NOPAR  no pre-marshalling [NPL 031405 table]
 -> FPT2ENTRY -> 5FP2E@142231   the ND-500 System Monitor (ND-100 code in 030-S3SM5)
 -> FUNCS[012] = PROGS@146451   builds + sends the 3START mailbox message (SAA 23 @146702,
                                helper 063007, send+WAIT 104236) — the wait spans the whole
                                ND-500 run; the process's STOP releases it
 -> STOPR dispatch @146726-742  A := msg[11B]; must be 1..3, else N5FAT@043660;
                                P += A-1 -> 1:064451  2:064300  3:064451
 -> trap-stop builder @064300   (STOPR=2) repackage: com-buffer := msg[12B..41B]  <- THE FILL
 -> common exit @065141         5DD1 := (0, TRAPN); ZAREG := 0; skip-return
 <- 5PT2RET @034737 [NPL]       copies S500DF back to N500DF, SUPDWINDOW@034766 sets the
                                data-buffer window so LOGBADR -> the com-buffer
 -> FUNCS[012] = FPRSTART       @035137: 5DD1 -> [5P1] (STDS0);
    [NPL]                       @035142: TOUSMOVE(dest=5P2, len=200B bytes) from LOGBADR
```

Two structures named alike, kept apart as in the 2026-08-10 carve: sysmon dispatch
`FUNCS@142031B` (does the work) vs the NPL post-return table `FUNCS@034534` (copies results).

## 2. The STOPR dispatch in PROGS [V]

```
146726  LDX ,B -67          ; X := message (window address)
146727  LDA ,X 11           ; A := msg[11B] = STOPR
146730  SKP IF 0 LST SA     ; A > 0 ?
146731  JMP 146735          ;   no  -> N5FAT (fatal log) via JPL I 21
146732  SAT 3
146733  SKP IF DT MLST SA   ; A > 3 ?
146734  JMP 146736          ;   no  -> dispatch
146735  JPL I 21            ; -> M[146756] = 043660 = N5FAT  (illegal stop reason)
146736  AAA -1
146737  RADD SA DP          ; P += STOPR-1
146740  JMP I 17            ; STOPR=1 -> M[146757] = 064451   (moncall stop)
146741  JMP I 17            ; STOPR=2 -> M[146760] = 064300   (TRAP stop)
146742  JMP I 15            ; STOPR=3 -> M[146757] = 064451   (terminate)
```

Pool words byte-verified: `M[146756]=043660` (`N5FAT` per L07 symbols), `M[146757]=064451`,
`M[146760]=064300`.

## 3. The trap-stop builder at 064300 [V]

Frame registers per the 5FP2E frame map: `B-67` = message window address, `B-65` = ABUFA
(com-buffer ND-100 physical address, 32-bit), `B-11` = info block (`S500DF-ZPREG`).

```
064300  LDA I 134 / LDT ,B -55 / SKP UEQ    ; M[064434]=111075: special-case cell vs B-55
064304-064352                                ; TRAPN grouping {25,26,27}->11B  {44,46,51}->17B
                                             ; {45}->20B else 5B; 45B also clears a bit in
                                             ; msg[30B]; JPL I 56 -> M[064440]=062453 (accounting)
064364  LDA I 55 / JAZ 064415                ; M[064441]=004004: console-mode flag; when set AND
064366-064414                                ; 5FUNC(,X 20)==145B (SSTDOM): print the trap on the
                                             ; console — LDD ,X 12 (trapping P) printed, then
                                             ; MON 64 (ERMSG) with code TRAPN | 7600B
                                             ; (M[064436]=007600 — the 76xxB trap-message family)
064415  LDD ,B -65                           ; D := ABUFA (com-buffer ND-100 physical addr)
064416  JPL I 32                             ; -> M[064450]=036003 (resident): phys -> window addr
064417  STA ,B -66                           ; B-66 := com-buffer window address
064420  LDT ,B -66                           ; T := DESTINATION = buffer word 0
064421  LDA ,B -67
064422  ADD 15                               ; + M[064437]=000012
064423  RADD CLD SA DD                       ; D := SOURCE = message + 12B
064424  SAA 30                               ; A := 30B = 24 words
064425  RADD CLD SA DL                       ; L := count
064426  MOVEW                                ; *** copy msg[12B..41B] -> buffer[0..27B] ***
064427  LDX ,B -67
064430  LDA ,X -6                            ; msg[-6]  (negative header part)
064431  STA ,X 5                             ; msg[5B] := msg[-6]  (data-part size restore)
064432  LDA ,X 16                            ; A := TRAPN  (carried to the exit)
064433  JMP I 14                             ; -> M[064447]=065141 common exit
```

Common exit `065141-065145` [V]:

```
065141  LDX ,B -11          ; X := info block
065142  STA ,X 41           ; 5D12 := A = TRAPN
065143  STZ ,X 40           ; 5D11 := 0        => 5DD1 = (0, TRAPN) = the "stop reason"
065144  STZ ,X 12           ; ZAREG := 0 (no error)
065145  MIN ,X 7            ; ZPREG skip-return bump
```

**No instruction between `064415` and the exit clears buffer words 24..63** — the tail of
the 200B block the user receives is whatever the com-buffer last held. [V]

## 4. The worker-side copy [NPL, address-stamped]

- `TOUSMOVE @027633`: "copy words to the user's area FROM THE MON60 BUFFER. The MON60 buffer
  is set up in the data-buffer window … the logical addr is found in LOGBADR" — source is
  ALWAYS `LOGBADR` (027637-027643, `MOVUS K:=1`).
- `5PT2RET @034737`: after the sysmon returns, `CALL SUPDWINDOW` (034766, "SET WINDOW ADDR
  TO MON60 BUFFER") re-establishes that window before the post-return `FUNCS` dispatch.
- `FPRSTART @035137`: `5DD1 -> [5P1]` (STDS0), then `T:=5P2; A:=200; CALL TOUSMOVE` —
  200B BYTES = 64 words from buffer word 0.
- `FREL5 @035156` re-enters `IFPRSTART` when the released process sits in BREAK state
  (035206) — the same two copies serve REL5's break-state answer, so this source analysis
  covers row `016/116` of the result-blocks carve too.

## 5. Byte-verification appendix

Read directly from `030-S3SM5.bin` (python struct read, word = (addr-40000B)*2, big-endian),
2026-08-11 — all match the `.dis`:

```
M[146727]=046011 (LDA ,X 11)      M[146740]=125017  M[146741]=125017  M[146742]=125015
M[146756]=043660  M[146757]=064451  M[146760]=064300
M[064415]=024713 (LDD ,B -65)     M[064421]=044711  M[064422]=060015  M[064424]=170430 (SAA 30)
M[064426]=143104 (MOVEW)          M[064430]=046372  M[064431]=006005  M[064432]=046016
M[064434]=111075  M[064436]=007600  M[064437]=000012  M[064441]=004004  M[064450]=036003
M[065141]=054767  M[065142]=006041 (STA ,X 41)  M[065143]=002040 (STZ ,X 40)
```

## 6. Open (marked, not guessed)

- The moncall/terminate builder at `064451` (STOPR=1/3): which words it puts in the buffer
  and what 5DD1 holds there (the trap path is what was asked). [OPEN]
- `M[111075]` (the `LDA I 134` special-case cell at 064300) and the accounting callee
  `062453`. [OPEN]
- Whether any OTHER writer ever pre-fills buffer words 24..63 before a trap stop (nothing in
  this path does). [OPEN]

## 7. Consequences for the emulator

The ND-500 Monitor's user-visible stop block is a **shifted view of the activation message**
(block word N = message hw N+12B). If RetroCore's `AnswerTrapStop` writes the message per
`CARVE-ANSWER-TRAP-RECORD-OFFSETS-AND-MMS-BITS-2026-08-11.md`, the monitor's trap report
fields come out right with NO extra servicer work: SINTRAN only relays. The stop reason the
caller sees in `[5P1]` is `(0, TRAPN)`, not the raw STOPR.
