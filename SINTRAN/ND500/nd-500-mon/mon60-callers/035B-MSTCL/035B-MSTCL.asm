; ============================================================================
;  MSTCL  ->  MON 60 subfunction 035B  (MASTER CLEAR)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 035B = 0x1D = 29 dec.  Thunk 146456 (SAA 35; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  yaml params: (none).
;  THREE call sites (all take no params).
; ============================================================================

; ---------------------------------------------------------------------------
;  Site A : 005736  -- CASE inside command interpreter ENTER 002662 (fs 000331).
;           Case body 005736-005740.  Error 002673 (pool 005702), loop 010613 (pool 005731).
; ---------------------------------------------------------------------------
005736  135173  	JPL I 173		; -> ptr 006131 = thunk 146456  MON60 MSTCL  (no params)
005737  135343  	JPL I -35		; callsite+1 ERROR   -> ptr 005702 = 002673
005740  125371  	JMP I -7		; callsite+2 SUCCESS -> ptr 005731 = 010613 (command loop)

; ---------------------------------------------------------------------------
;  Site B : 005744  -- CASE inside the same interpreter ENTER 002662.
;           Case body 005744-005746.
; ---------------------------------------------------------------------------
005744  135165  	JPL I 165		; -> ptr 006131 = thunk 146456  MON60 MSTCL  (no params)
005745  135335  	JPL I -43		; callsite+1 ERROR   -> ptr 005702 = 002673
005746  125363  	JMP I -15		; callsite+2 SUCCESS -> ptr 005731 = 010613 (command loop)
   ; pool: 005702=002673, 005731=010613, 006131=146456 (thunk MSTCL)

; ---------------------------------------------------------------------------
;  Site C : 122514  in standalone ENTER-routine 122507 (framesize 000000 = 0 dec).
;           Reached as the fall-through SUCCESS of MSTOP 034B at 122512 (see 034B-MSTOP).
; ---------------------------------------------------------------------------
122514  135006  	JPL I 6			; -> ptr 122522 = thunk 146456  MON60 MSTCL  (no params)
122515  135004  	JPL I 4			; callsite+1 ERROR   -> ptr 122521 = 177327 LEAVE(value)
122516  135005  	JPL I 5			; callsite+2 SUCCESS -> ptr 122523 = 177335 LEAVE-SKIP
   ; pool: 122521=177327, 122522=146456 (thunk MSTCL), 122523=177335
