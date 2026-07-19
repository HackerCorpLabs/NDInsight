; ============================================================================
;  003B-RDATA  ->  MON 60 subfunction RDATA = 3B (0x03 = 3)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146324 SAA 3 ; 146325 JMP I 1 ; 146326 = 146244 (gateway).
;  Three call sites: 022462 (rtn 022310), 055211 (rtn 055151), 056723 (rtn 056042).
; ============================================================================

; ---- Call site 022462 : ENTER-routine 022310 (framesize 000014) ------------
022443  024067  	LDD 67			; D := 32-bit const (logical DM addr, INFERRED)
022444  020614  	STD ,B -164
022446  172614  	AAA -164
022447  054602  	LDX ,B -176		; X := gateway frame base
022450  006006  	STA ,X 6		; param1 := &(B-164)
022451  044045  	LDA 45
022452  006007  	STA ,X 7		; param2 := const
022453  044054  	LDA 54
022454  144151  	SWAP CLD SA DD
022455  050053  	LDT 53
022456  032010  	STF ,X 10		; param3 := F register (3-word)
022457  146135  	RADD CLD SB DA
022460  172610  	AAA -170
022461  006013  	STA ,X 13		; param@offset13 := &(B-170)
022462  135052  	JPL I 52		; -> 022534  MON60 RDATA (3B) thunk=146324
022463  134232  	JPL -146		; -> 022315  callsite+1 = ERROR (local block)
022464  124135  	JMP 135			; -> 022621  callsite+2 = SUCCESS
022534  146324  	<thunk RDATA (SAA 3)>	; bank1[022534] = 146324

; ---- Call site 055211 : ENTER-routine 055151 (framesize 000013) ------------
055170  146135  	RADD CLD SB DA
055171  172613  	AAA -165
055172  054602  	LDX ,B -176
055173  006006  	STA ,X 6		; param1 := &(B-165)
055174  146135  	RADD CLD SB DA
055175  172606  	AAA -172
055176  006007  	STA ,X 7		; param2 := &(B-172)
055177  170400  	SAA 0
055200  146151  	RADD CLD SA DD
055201  044615  	LDA ,B -163
055202  050610  	LDT ,B -170
055203  030616  	STF ,B -162
055204  034616  	LDF ,B -162
055205  032010  	STF ,X 10		; param3 := F register (3-word)
055206  146135  	RADD CLD SB DA
055207  172613  	AAA -165
055210  006013  	STA ,X 13		; param@offset13 := &(B-165)
055211  135007  	JPL I 7			; -> 055220  MON60 RDATA (3B) thunk=146324
055212  135004  	JPL I 4			; -> 055216  callsite+1 = ERROR (ptr=177327)
055213  135006  	JPL I 6			; -> 055221  callsite+2 = SUCCESS (ptr=177335)
055220  146324  	<thunk RDATA (SAA 3)>	; bank1[055220] = 146324

; ---- Call site 056723 : ENTER-routine 056042 (framesize 000050) ------------
056703  044374  	LDA -4			; A := value at [056677] (INFERRED selector/const)
056704  054602  	LDX ,B -176
056705  006006  	STA ,X 6		; param1 := that value
056706  146135  	RADD CLD SB DA
056707  172615  	AAA -163
056710  006007  	STA ,X 7		; param2 := &(B-163)
056711  170400  	SAA 0
056712  146151  	RADD CLD SA DD
056713  044610  	LDA ,B -170
056714  050114  	LDT 114
056715  030635  	STF ,B -143
056716  034635  	LDF ,B -143
056717  032010  	STF ,X 10		; param3 := F register (3-word)
056720  146135  	RADD CLD SB DA
056721  172613  	AAA -165
056722  006013  	STA ,X 13		; param@offset13 := &(B-165)
056723  135106  	JPL I 106		; -> 057031  MON60 RDATA (3B) thunk=146324
056724  134261  	JPL -117		; -> 056605  callsite+1 = ERROR
056725  054606  	LDX ,B -172		; callsite+2 = SUCCESS
057031  146324  	<thunk RDATA (SAA 3)>	; bank1[057031] = 146324
