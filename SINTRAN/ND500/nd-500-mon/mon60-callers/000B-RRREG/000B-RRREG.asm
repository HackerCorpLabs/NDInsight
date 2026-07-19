; ============================================================================
;  000B-RRREG  ->  MON 60 subfunction RRREG = 0B (0x00 = 0)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  MON 60 reaches the ND-500 through the single gateway at 146244 (MON 60 at
;  146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp]) holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n is
;  the subfunction code.
;  Return convention (PROVEN, prog.md sec 5.4): callsite+1 = ERROR (direct
;  return), callsite+2 = SUCCESS (skip return).
;  Thunk (verified): 146310 SAA 0 ; 146311 JMP I 1 ; 146312 = 146244 (gateway).
; ============================================================================
;  Call site 013143, inside standalone ENTER-routine 013100 (framesize 000013).
; ----------------------------------------------------------------------------

013100  146547  	RADD AD1 CLD SL DX	; ENTER prologue of the enclosing routine
013101  135061  	JPL I 61		; -> 013162  ptr=177300 (ENTER)
013102  000013  	STZ 13			; <inline framesize = 000013>
; ... routine body ...
013132  024037  	LDD 37			; D := 32-bit constant from pool [013171]
013133  020615  	STD ,B -163		;   B-163 := selector value  (register #, INFERRED)
013134  146135  	RADD CLD SB DA		; A := B
013135  172615  	AAA -163		; A := &(B-163)
013136  054602  	LDX ,B -176		; X := stack top (base of gateway frame)
013137  006006  	STA ,X 6		; param1 := &(B-163)   [ptr to register number]
013140  146135  	RADD CLD SB DA		; A := B
013141  172617  	AAA -161		; A := &(B-161)
013142  006007  	STA ,X 7		; param2 := &(B-161)   [ptr to result buffer]
013143  135030  	JPL I 30		; -> 013173  MON60 RRREG (0B) thunk=146310
013144  135022  	JPL I 22		; -> 013166  callsite+1 = ERROR
013145  044614  	LDA ,B -164		; callsite+2 = SUCCESS: consume returned data
; ---- local pointer/constant pool (partial) ----
013166  ......  	<error-path pointer>
013173  146310  	<thunk RRREG (SAA 0)>	; bank1[013173] = 146310
