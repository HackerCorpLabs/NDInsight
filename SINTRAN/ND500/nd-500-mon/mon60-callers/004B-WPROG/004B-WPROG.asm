; ============================================================================
;  004B-WPROG  ->  MON 60 subfunction WPROG = 4B (0x04 = 4)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146321 SAA 4 ; 146322 JMP I 1 ; 146323 = 146244 (gateway).
; ----------------------------------------------------------------------------
;  Call site 055702, inside standalone ENTER-routine 055255 (framesize 000302).

055656  146135  	RADD CLD SB DA		; A := B
055657  172610  	AAA -170		; A := &(B-170)
055660  054602  	LDX ,B -176		; X := gateway frame base
055661  006006  	STA ,X 6		; param1 := &(B-170)  [logical PM address, INFERRED]
055662  146135  	RADD CLD SB DA
055663  172612  	AAA -166		; A := &(B-166)
055664  006007  	STA ,X 7		; param2 := &(B-166)  [count, INFERRED]
055665  034623  	LDF ,B -155		; F := local(B-155)  [source data, INFERRED]
055666  124013  	JMP 13			; -> 055701 (skip an unrelated block)
055701  032010  	STF ,X 10		; param3 := F register (3-word)
055702  135124  	JPL I 124		; -> 056026  MON60 WPROG (4B) thunk=146321
055703  135124  	JPL I 124		; -> 056027  callsite+1 = ERROR (ptr=177327 LEAVE-value)
055704  001124  	STZ I 124		; callsite+2 = SUCCESS
055705  124120  	JMP 120			; -> 056025
056026  146321  	<thunk WPROG (SAA 4)>	; bank1[056026] = 146321
056027  177327  	<-> LEAVE(value)  (error return propagates to caller)
