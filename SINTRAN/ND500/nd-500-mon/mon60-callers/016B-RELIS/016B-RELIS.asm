; ============================================================================
;  016B-RELIS  ->  MON 60 subfunction RELIS = 16B (0x0E = 14)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146362 SAA 16 ; 146363 JMP I 1 ; 146364 = 146244 (gateway).
;  Two call sites: 001126 (routine 001072) and 010322 (interpreter case).
; ============================================================================

; ---- Call site 001126 : standalone ENTER-routine 001072 (framesize 000001) --
; NOTE: 001120-001121 (STA ,X 6) belongs to the PRECEDING call at 001122, not to
; RELIS.  RELIS at 001126 stores NO parameter of its own into the block; it is the
; success continuation of the 001124 call and issues RELIS directly.
001120  054602  	LDX ,B -176		; (param setup for the 001122 call, NOT RELIS)
001121  006006  	STA ,X 6		; (param for 001122 call)
001122  135021  	JPL I 21		; -> 001143  (call routine 171557)
001123  134353  	JPL -25			; -> 001076  (its error/loop)
001124  135020  	JPL I 20		; -> 001144  (call routine 001052)
001125  134351  	JPL -27			; -> 001076  (its error/loop)
001126  135017  	JPL I 17		; -> 001145  MON60 RELIS (16B) thunk=146362
001127  134347  	JPL -31			; -> 001076  callsite+1 = ERROR (loop head 001076)
001130  170401  	SAA 1			; callsite+2 = SUCCESS
001145  146362  	<thunk RELIS (SAA 16)>	; bank1[001145] = 146362

; ---- Call site 010322 : CASE inside command interpreter 002662 (002662-010634)
010320  170401  	SAA 1			; A := 1
010321  005352  	STA I -26		; store A -> [010273] (a flag; NOT the MON60 block)
010322  135172  	JPL I 172		; -> 010514  MON60 RELIS (16B) thunk=146362
010323  135333  	JPL I -45		; -> 010256  callsite+1 = ERROR (ptr[010256]=007500)
010324  044171  	LDA 171			; callsite+2 = SUCCESS (start of next case's code)
010514  146362  	<thunk RELIS (SAA 16)>	; bank1[010514] = 146362
