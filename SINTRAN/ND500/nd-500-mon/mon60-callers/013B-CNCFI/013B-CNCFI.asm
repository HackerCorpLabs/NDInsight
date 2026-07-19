; ============================================================================
;  013B-CNCFI  ->  MON 60 subfunction CNCFI = 13B (0x0B = 11)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146351 SAA 13 ; 146352 JMP I 1 ; 146353 = 146244 (gateway).
; ----------------------------------------------------------------------------
;  Call site 036440, inside standalone ENTER-routine 036374 (framesize 000022).

036374  146547  	RADD AD1 CLD SL DX	; ENTER prologue of the enclosing routine
036375  135070  	JPL I 70		; -> 036465 (ptr -> 177300 ENTER)
036376  000022  	STZ 22			; <inline framesize = 000022>
; ... body ...
036422  044616  	LDA ,B -162
036423  054602  	LDX ,B -176		; X := gateway frame base
036424  006006  	STA ,X 6		; param1 := local(B-162)
036425  146135  	RADD CLD SB DA
036426  172626  	AAA -152
036427  006007  	STA ,X 7		; param2 := &(B-152)
036430  044621  	LDA ,B -157
036431  006010  	STA ,X 10		; param3 := local(B-157)
036432  146135  	RADD CLD SB DA
036433  172611  	AAA -167
036434  006011  	STA ,X 11		; param4 := &(B-167)
036435  146135  	RADD CLD SB DA
036436  172624  	AAA -154
036437  006012  	STA ,X 12		; param5 := &(B-154)
036440  135034  	JPL I 34		; -> 036474  MON60 CNCFI (13B) thunk=146351
036441  135027  	JPL I 27		; -> 036470  callsite+1 = ERROR (ptr=177327 LEAVE-value)
036442  024611  	LDD ,B -167		; callsite+2 = SUCCESS
036470  177327  	<-> LEAVE(value)  (error return propagates to caller)
036474  146351  	<thunk CNCFI (SAA 13)>	; bank1[036474] = 146351
