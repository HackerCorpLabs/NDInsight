; ============================================================================
;  MSTOP  ->  MON 60 subfunction 034B  (MICRO STOP)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 034B = 0x1C = 28 dec.  Thunk 146453 (SAA 34; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  yaml params: (none).
;  TWO call sites (both take no params).
; ============================================================================

; ---------------------------------------------------------------------------
;  Site A : 006312  -- CASE inside command interpreter ENTER 002662
;           (framesize 000331=217).  Case body 006312-006314.
;           Shared error 002673 (pool 006130), loop 010613 (pool 006343).
; ---------------------------------------------------------------------------
006312  135037  	JPL I 37		; -> ptr 006351 = thunk 146453  MON60 MSTOP  (no params)
006313  135215  	JPL I -163		; callsite+1 ERROR   -> ptr 006130 = 002673
006314  125027  	JMP I 27		; callsite+2 SUCCESS -> ptr 006343 = 010613 (command loop)
   ; pool: 006130=002673, 006343=010613, 006351=146453 (thunk MSTOP)

; ---------------------------------------------------------------------------
;  Site B : 122512  in standalone ENTER-routine 122507 (framesize 000000 = 0 dec).
;           This routine issues MSTOP then (on success, fall-through) MSTCL 035B.
; ---------------------------------------------------------------------------
122507  146547  	RADD AD1 CLD SL DX	; ENTER prologue: X := L+1
122510  135007  	JPL I 7			; -> ptr 122517 = 177300 ENTER
122511  000000  	<framesize 0>
122512  135006  	JPL I 6			; -> ptr 122520 = thunk 146453  MON60 MSTOP  (no params)
122513  135006  	JPL I 6			; callsite+1 ERROR   -> ptr 122521 = 177327 LEAVE(value)
122514  135006  	JPL I 6			; callsite+2 SUCCESS = the MSTCL 035B call (see 035B-MSTCL)
   ; pool: 122517=177300, 122520=146453 (thunk MSTOP), 122521=177327,
   ;       122522=146456 (thunk MSTCL), 122523=177335
