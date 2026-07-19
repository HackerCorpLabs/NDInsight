; ============================================================================
;  WRICS  ->  MON 60 subfunction 024B  (WRITE CONTROL STORE)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 024B = 0x14 = 20 dec.  Thunk 146412 (SAA 24; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  yaml params: <CS addr.> <no of 16 bit words> <data-area>.
; ============================================================================
;  Call site 123420  in standalone ENTER-routine 123343 (framesize 000035 = 29 dec).
;  Verified: 123420 JPL I 64 -> ptr 123504, bank1[123504]=146412 = SAA 24.

123407  146135  	RADD CLD SB DA		; A := B
123410  172616  	AAA -162		; A := B-162
123411  054602  	LDX ,B -176		; X := stack top
123412  006006  	STA ,X 6		; param1 := &(B-162) = <CS addr.>
123413  146135  	RADD CLD SB DA
123414  172614  	AAA -164		; A := B-164
123415  006007  	STA ,X 7		; param2 := &(B-164) = <no of 16 bit words>
123416  034607  	LDF ,B -171		; F := descriptor at B-171
123417  032010  	STF ,X 10		; param3 := F = <data-area> (3 words)
123420  135064  	JPL I 64		; -> ptr 123504 = thunk 146412  MON60 WRICS
123421  135061  	JPL I 61		; callsite+1 ERROR   -> ptr 123502 = 177327 LEAVE(value)
123422  045063  	LDA I 63		; callsite+2 SUCCESS (continues)

; ---- pool ----
123502  177327  	<ptr> 177327		; -> LEAVE(value)
123504  146412  	<ptr> 146412		; -> thunk WRICS (SAA 24)
