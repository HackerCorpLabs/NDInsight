; ============================================================================
;  002B-RPROG  ->  MON 60 subfunction RPROG = 2B (0x02 = 2)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146316 SAA 2 ; 146317 JMP I 1 ; 146320 = 146244 (gateway).
;  Two call sites: 022440 (routine 022310) and 056341 (routine 056042).
; ============================================================================

; ---- Call site 022440 : ENTER-routine 022310 (framesize 000014) ------------
022310  146547  	RADD AD1 CLD SL DX	; ENTER prologue of enclosing routine
022311  135001  	JPL I 1			; (ptr -> 177300 ENTER)
022312  000014  	STZ 14			; <inline framesize = 000014>
; ... body ...
022421  024104  	LDD 104			; D := 32-bit const [022525] (logical PM addr, INFERRED)
022422  020614  	STD ,B -164
022423  146135  	RADD CLD SB DA
022424  172614  	AAA -164
022425  054602  	LDX ,B -176		; X := gateway frame base
022426  006006  	STA ,X 6		; param1 := &(B-164)
022427  044067  	LDA 67			; A := const [022516]
022430  006007  	STA ,X 7		; param2 := const value
022431  044076  	LDA 76
022432  144151  	SWAP CLD SA DD
022433  050075  	LDT 75
022434  032010  	STF ,X 10		; param3 := F register (3-word)
022435  146135  	RADD CLD SB DA
022436  172610  	AAA -170
022437  006013  	STA ,X 13		; param@offset13 := &(B-170)
022440  135071  	JPL I 71		; -> 022531  MON60 RPROG (2B) thunk=146316
022441  134254  	JPL -124		; -> 022315  callsite+1 = ERROR (local block 146147)
022442  124157  	JMP 157			; -> 022621  callsite+2 = SUCCESS
022531  146316  	<thunk RPROG (SAA 2)>	; bank1[022531] = 146316

; ---- Call site 056341 : ENTER-routine 056042 (framesize 000050) ------------
056321  044324  	LDA -54			; A := value at [056245] (INFERRED selector/const)
056322  054602  	LDX ,B -176		; X := gateway frame base
056323  006006  	STA ,X 6		; param1 := that value
056324  146135  	RADD CLD SB DA
056325  172615  	AAA -163
056326  006007  	STA ,X 7		; param2 := &(B-163)
056327  170400  	SAA 0
056330  146151  	RADD CLD SA DD		; D := 0
056331  044610  	LDA ,B -170
056332  050125  	LDT 125
056333  030635  	STF ,B -143
056334  034635  	LDF ,B -143
056335  032010  	STF ,X 10		; param3 := F register (3-word)
056336  146135  	RADD CLD SB DA
056337  172613  	AAA -165
056340  006013  	STA ,X 13		; param@offset13 := &(B-165)
056341  135117  	JPL I 117		; -> 056460  MON60 RPROG (2B) thunk=146316
056342  135276  	JPL I -102		; -> 056240  callsite+1 = ERROR
056343  125107  	JMP I 107		; -> 056452  callsite+2 = SUCCESS
056460  146316  	<thunk RPROG (SAA 2)>	; bank1[056460] = 146316
