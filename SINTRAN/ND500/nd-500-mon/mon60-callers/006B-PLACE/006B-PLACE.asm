; ============================================================================
;  006B-PLACE  ->  MON 60 subfunction PLACE = 6B (0x06 = 6)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  NOTE: 006B has TWO thunks in the table - 146332 (no caller) and 146335 (used).
;  Thunk (verified): 146335 SAA 6 ; 146336 JMP I 1 ; 146337 = 146244 (gateway).
;  Both call sites live in standalone ENTER-routine 041730 (framesize 000000).
; ============================================================================

; ---- Call site 042230 : routine 041730 -------------------------------------
042210  044616  	LDA ,B -162		; A := local(B-162)
042211  054602  	LDX ,B -176		; X := gateway frame base
042212  006006  	STA ,X 6		; param1 := local(B-162)
042213  146135  	RADD CLD SB DA
042214  172651  	AAA -127
042215  006007  	STA ,X 7		; param2 := &(B-127)
042216  146135  	RADD CLD SB DA
042217  172623  	AAA -155
042220  006010  	STA ,X 10		; param3 := &(B-155)
042221  146135  	RADD CLD SB DA
042222  172621  	AAA -157
042223  006011  	STA ,X 11		; param4 := &(B-157)
042224  170411  	SAA 11
042225  144151  	SWAP CLD SA DD
042226  050643  	LDT ,B -135
042227  032012  	STF ,X 12		; param5 := F register (3-word)
042230  135074  	JPL I 74		; -> 042324  MON60 PLACE (6B) thunk=146335
042231  134303  	JPL -75			; -> 042134  callsite+1 = ERROR (local block 146147)
042232  000645  	STZ ,B -133		; callsite+2 = SUCCESS
042324  146335  	<thunk PLACE (SAA 6)>	; bank1[042324] = 146335

; ---- Call site 042535 : routine 041730 -------------------------------------
042515  044616  	LDA ,B -162
042516  054602  	LDX ,B -176
042517  006006  	STA ,X 6		; param1 := local(B-162)
042520  146135  	RADD CLD SB DA
042521  172651  	AAA -127
042522  006007  	STA ,X 7		; param2 := &(B-127)
042523  146135  	RADD CLD SB DA
042524  172623  	AAA -155
042525  006010  	STA ,X 10		; param3 := &(B-155)
042526  146135  	RADD CLD SB DA
042527  172621  	AAA -157
042530  006011  	STA ,X 11		; param4 := &(B-157)
042531  170411  	SAA 11
042532  144151  	SWAP CLD SA DD
042533  050643  	LDT ,B -135
042534  032012  	STF ,X 12		; param5 := F register (3-word)
042535  135034  	JPL I 34		; -> 042571  MON60 PLACE (6B) thunk=146335
042536  134260  	JPL -120		; -> 042416  callsite+1 = ERROR (local block 146147)
042537  054642  	LDX ,B -136		; callsite+2 = SUCCESS
042571  146335  	<thunk PLACE (SAA 6)>	; bank1[042571] = 146335
