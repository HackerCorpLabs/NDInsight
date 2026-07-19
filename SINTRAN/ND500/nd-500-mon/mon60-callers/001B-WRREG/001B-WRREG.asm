; ============================================================================
;  001B-WRREG  ->  MON 60 subfunction WRREG = 1B (0x01 = 1)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway at 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146313 SAA 1 ; 146314 JMP I 1 ; 146315 = 146244 (gateway).
; ----------------------------------------------------------------------------
;  Call site 052642, inside standalone ENTER-routine 052605 (framesize 000005).

052605  146547  	RADD AD1 CLD SL DX	; ENTER prologue of the enclosing routine
052606  135045  	JPL I 45		; -> 052653  ptr=177300 (ENTER)
052607  000005  	STZ 5			; <inline framesize = 000005>
; ... routine body ...
052630  044606  	LDA ,B -172		; A := local(B-172)
052631  154760  	SAD SHR 20		; D := A >> 020  (extract high half, INFERRED)
052632  020611  	STD ,B -167		;   B-167 := register number (INFERRED)
052633  146135  	RADD CLD SB DA		; A := B
052634  172611  	AAA -167		; A := &(B-167)
052635  054602  	LDX ,B -176		; X := gateway frame base
052636  006006  	STA ,X 6		; param1 := &(B-167)  [ptr to register number]
052637  146135  	RADD CLD SB DA		; A := B
052640  172607  	AAA -171		; A := &(B-171)
052641  006007  	STA ,X 7		; param2 := &(B-171)  [ptr to value]
052642  135017  	JPL I 17		; -> 052661  MON60 WRREG (1B) thunk=146313
052643  135014  	JPL I 14		; -> 052657  callsite+1 = ERROR (ptr=177327 LEAVE-value)
052644  024611  	LDD ,B -167		; callsite+2 = SUCCESS
; ---- pointer pool ----
052657  177327  	<-> LEAVE(value)  (error return propagates to caller)
052661  146313  	<thunk WRREG (SAA 1)>	; bank1[052661] = 146313
