; ============================================================================
; MON 162B (octal) OutString - real SINTRAN L disassembly
;
; MON 162B is a FALL-THROUGH call: GOTAB[162] = 000000 (byte-proven), so there
; is NO per-call level-14 entry stub to list. Dispatch drops into the resident
; MFELL/CALLPROC second-level path (uncarved) which reaches the worker below.
; Hence a single labelled region:
;   B. OUTST out-string worker body   OUTST @41013B  in segment 006-S3FS (load 26000B)
;      The body writes a string of characters to a peripheral file: it loops
;      GETCH (@30062B, get next character) -> SOUTB (@31030B, output one byte),
;      which is the byte-level reason OUTST is the OutString worker.
;   (the fall-through -> OUTST bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; The worker at 41013B shares its output loop (41030..41061B) and its link-cell
; table (41062..41070B) with the sibling entry SRTOU (@41022B); the SRTOU entry
; body (41022..41027B) is NOT part of MON 162B and is omitted here.
; Addresses and values are octal.
; ============================================================================

; ---- Region B: OUTST out-string worker (006-S3FS, load 26000B) ----
41013  021047  	STD I 47		; OUTST entry: stash caller D (params) via link cell 41064
41014  146145  	RADD CLD SL DA
41015  146131  	RADD CLD SB DD
41016  170007  	SAB 7			; establish local frame (B), 7 words
41017  135044  	JPL I 44		; -> 41063 (link cell = 003752, resident prologue worker)
41020  045044  	LDA I 44		; load via link cell 41064
41021  124007  	JMP 7		; -> 41030 (join shared output loop)

; ---- shared output loop (41030..41061) - reached by OUTST (41021 JMP) and by
;      the sibling entry SRTOU (fall-through from 41027) ----
41030  004406  	STA ,B 6		; store current char/arg -> B+6
41031  146101  	RADD CLD 0 DD
41032  050401  	LDT ,B 1		; loop top: T = remaining count (B+1)
41033  173377  	AAT -1			; T -= 1
41034  141016  	SKP IF DT GRE SD
41035  124023  	JMP 23		; -> 41060 (count exhausted -> unwind + return)
41036  146116  	RADD CLD SD DT
41037  135026  	JPL I 26		; -> 41065 (link cell = 030062, GETCH get next character)
41040  171047  	SAT 47
41041  142065  	SKP IF DA UEQ ST
41042  124016  	JMP 16		; -> 41060 (terminator -> unwind + return)
41043  171044  	SAT 44
41044  140065  	SKP IF DA EQL ST
41045  124007  	JMP 7		; -> 41054
41046  050406  	LDT ,B 6
41047  044017  	LDA 17
41050  135017  	JPL I 17		; -> 41067 (link cell = 031030, SOUTB output one byte)
41051  170412  	SAA 12
41052  135015  	JPL I 15		; -> 41067 (link cell = 031030, SOUTB output one byte)
41053  124003  	JMP 3		; -> 41056
41054  050406  	LDT ,B 6
41055  135012  	JPL I 12		; -> 41067 (link cell = 031030, SOUTB output one byte)
41056  146401  	RADD AD1 0 DD
41057  124353  	JMP -25		; -> 41032 (loop back to next character)
41060  170771  	SAA -7			; unwind 7-word frame (matches SAB 7)
41061  125007  	JMP I 7		; -> 41070 (link cell = 003776, resident return cell)

; ---- link cells (DATA, not code) 41062..41070 - shared table ----
; nd100-dis renders these pointer words as bogus instructions; their contents
; are the real worker addresses (resolved below).
41062  147417  	ROP NOOP		; (data) padding
41063  003752  	STZ I ,B ,X -26		; = 003752  (resident prologue worker; used by OUTST 41017)
41064  146273  	RADD CM1 SX DB		; (data) indirect load/store slot (used by OUTST 41013/41020)
41065  030062  	STF 62			; = 030062  (GETCH get next character; used by 41037)
41066  000215  	STZ -163		; (data)
41067  031030  	STF I 30		; = 031030  (SOUTB output one byte; used by 41050/41052/41055)
41070  003776  	STZ I ,B ,X -2		; = 003776  (resident return cell; used by OUTST 41061)
