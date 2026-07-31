; ============================================================================
; MON 345B (octal) MTAFunction - real SINTRAN L disassembly
;
; MON 345B is an ND-100 call (345B < 400B) with a ZERO GOTAB slot, i.e. a
; level-14 FALL-THROUGH.  There is therefore NO per-call entry stub to list;
; the level-14 handler is reached through the resident MFELL/CALLPROC path
; (uncarved).
;
;   GOTAB[345B] = 000000  (fall-through)  in resident commoncode
;
; The terminal-line MTA handler (manual short name MTAD; connect/disconnect a
; terminal line to a datafield) exists as real code in overlay 025-S3IRPIT near
; the MTSTA symbol, but the fall-through bridge to it is uncarved and the exact
; worker cannot be followed from these bytes.  MTSTA is shown below as a CANDIDATE
; (real bytes) - it is NOT a proven MON 345B worker (see README Honest caveats).
;
; Extracted verbatim from the byte-identity-checked whole-segment disassembly
; ../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm .  Addresses and values octal.
; >>> NAME lines are in-range SYMBOL-2-LIST labels.
; ============================================================================

; ---- Dispatch head: GOTAB[345] = 000000 (fall-through) ----
; The GOTAB word for MON 345B is literally zero; there is no entry stub to
; disassemble.  Dispatch enters the resident MFELL/CALLPROC handler (uncarved).

; ---- CANDIDATE terminal-line worker MTSTA (025-S3IRPIT, load 32000B) ----
; Real bytes: a terminal-line status routine driving device status via STATX
; (physical device-status transfer).  Attribution to MON 345B is by SYMBOL/name
; family only (MTSTA = MTa STArt; yaml handler MTSTART) - link NOT byte-proven.
>>> MTSTA(SYMBOL-2-LIST) = 64636B
064636  135026  	JPL I 26		; -> [064664] worker (pointer word)
064637  044421  	LDA ,B 21		; A = ,B 21 (line status word)
064640  143304  	STATX			; phys[EL] = A : write terminal-line status
064641  044423  	LDA ,B 23		; A = ,B 23
064642  143314  	STATX 1			; phys[EL] = A : write second status word
064643  124003  	JMP 3		; -> 064646
064644  044021  	LDA 21			; A = mem[064665B]
064645  125442  	JMP I ,B 42		; -> [064707] return
064646  146105  	RADD CLD 0 DA		; A = 0
064647  125442  	JMP I ,B 42		; -> [064711] return
; ---- 064650B onward: pointer / data words and neighbouring routines ----
; (CDF05=064642 and later symbols; not part of a proven MON 345B body)
