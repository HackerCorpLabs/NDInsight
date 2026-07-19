; ============================================================================
;  WRREG_BLOCK  ->  MON 60 subfunction 011B  (WRITE REGISTERS, block form)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 011B = 0x09 = 9 dec.  Thunk 146404 (SAA 11; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  Standalone ENTER-routine 052703 (framesize 000003 = 3 dec).
;  Verified: 052711 JPL I 6 -> ptr 052717, bank1[052717]=146404 = SAA 11.

052703  146547  	RADD AD1 CLD SL DX	; ENTER prologue: X := L+1
052704  135012  	JPL I 12		; -> ptr 052716 = 177300 ENTER
052705  000003  	<framesize 3>
052706  034606  	LDF ,B -172		; F := caller <register block> (local B-172)
052707  054602  	LDX ,B -176		; X := stack top
052710  032006  	STF ,X 6		; param1 := F = <register block>
052711  135006  	JPL I 6			; -> ptr 052717 = thunk 146404  MON60 WRREG_BLOCK
052712  135006  	JPL I 6			; callsite+1 ERROR   -> ptr 052720 = 177327 LEAVE(value)
052713  170401  	SAA 1			; callsite+2 SUCCESS
052714  005005  	STA I 5
052715  135005  	JPL I 5			; -> ptr 052722 = 177335 LEAVE-SKIP

; ---- pointer pool (data) ----
052716  177300  	<ptr> 177300		; -> ENTER
052717  146404  	<ptr> 146404		; -> thunk WRREG_BLOCK (SAA 11)
052720  177327  	<ptr> 177327		; -> LEAVE(value)
052722  177335  	<ptr> 177335		; -> LEAVE-SKIP
