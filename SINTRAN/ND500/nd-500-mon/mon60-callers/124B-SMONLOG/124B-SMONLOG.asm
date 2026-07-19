; ============================================================================
;  SMONLOG  ->  MON 60 subfunction SMONLOG = 124B (0x54 = 84 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  Enclosing routine : command interpreter ENTER-routine 002662 (framesize
;  000331), spanning 002662..010634.  Call site = 007312.
;  Thunk VERIFIED: bank1[007404]=146676 ; 146676 SAA 124 ; 146677 JMP I 1 ;
;  146700 146244 (gateway).

; ---- SMONLOG command sequence (007277..007314) ----
007277  034665  	LDF ,B -113		; F := filename descriptor (3 words) from B-113
007300  054602  	LDX ,B -176
007301  032006  	STF ,X 6		; (param for helper 002222) := filename F
007302  135101  	JPL I 101		; -> ptr 007403 = routine 002222 (filename resolve; INFERRED)
007303  135266  	JPL I -112		; 002222 err -> ptr 007171 = 002673 (interp error)
007304  154760  	SAD SHR 20		; D := (returned value) >> 16
007305  020651  	STD ,B -127		; local(B-127) := D
007306  146135  	RADD CLD SB DA		; A := B
007307  172651  	AAA -127		; A := &local(B-127)
007310  054602  	LDX ,B -176		; X := gateway param base
007311  006006  	STA ,X 6		; MON60 param1 := &local(B-127)
007312  135072  	JPL I 72		; -> ptr 007404 = thunk 146676  SMONLOG (124B)  *** MON 60 ***
007313  135256  	JPL I -122		; callsite+1 ERROR -> ptr 007171 = 002673 (interp error)
007314  125062  	JMP I 62		; callsite+2 SUCCESS -> ptr 007376 = 010613 (command loop)
; ---- relevant pointer pool words (data) ----
007171  002673  	<ptr>  -> routine 002673  (interpreter error reporter)
007376  010613  	<ptr>  -> 010613  (command loop)
007403  002222  	<ptr>  -> routine 002222  (helper; INFERRED = filename resolve)
007404  146676  	<ptr>  -> thunk 146676 (SMONLOG 124B)
