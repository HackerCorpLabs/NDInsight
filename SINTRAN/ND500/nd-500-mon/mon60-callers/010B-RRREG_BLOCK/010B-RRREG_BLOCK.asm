; ============================================================================
;  RRREG_BLOCK  ->  MON 60 subfunction 010B  (READ ALL REGISTERS, block form)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Subfunction code 010B = 0x08 = 8 dec.  Thunk 146401 (SAA 10; JMP I 1; 146244).
;  Mechanism: JPL I <disp> -> ptr word bank1[P+disp] = thunk addr -> gateway 146244
;  -> MON 60 at 146256.  Convention (PROVEN, prog.md sec 4.5/5.4):
;    callsite+1 = ERROR (direct return), callsite+2 = SUCCESS (skip return).
; ============================================================================
;  Standalone ENTER-routine 052522 (framesize 000001 = 1 dec).
;  Reads all ND-500 registers as one block into a caller descriptor.
;  Verified: 052563 JPL I 20 -> ptr 052603, bank1[052603]=146401 = SAA 10.

052522  146547  	RADD AD1 CLD SL DX	; ENTER prologue: X := L+1
052523  135044  	JPL I 44		; -> ptr 052567 = 177300 ENTER
052524  000001  	<framesize 1>
052525  045043  	LDA I 43
052526  131026  	JAZ 26			; -> 052554
   ; ... register-scan loop 052527-052553 (fills caller buffer) ...
052554  045025  	LDA I 25
052555  131011  	JAZ 11			; -> 052566
052556  170477  	SAA 77			; build register-block descriptor
052557  144151  	SWAP CLD SA DD
052560  050022  	LDT 22
052561  054602  	LDX ,B -176		; X := stack top (gateway param base)
052562  032006  	STF ,X 6		; param1 := F = <register block> descriptor
052563  135020  	JPL I 20		; -> ptr 052603 = thunk 146401  MON60 RRREG_BLOCK
052564  135011  	JPL I 11		; callsite+1 ERROR   -> ptr 052575 = 177327 LEAVE(value)
052565  001014  	STZ I 14		; callsite+2 SUCCESS
052566  135016  	JPL I 16		; -> ptr 052604 = 177335 LEAVE-SKIP

; ---- pointer/constant pool (data, NOT code) ----
052574  176624  	<pool>			; (BLDA image)
052575  177327  	<ptr> 177327		; -> LEAVE(value)
052576  176262  	<pool>
052603  146401  	<ptr> 146401		; -> thunk RRREG_BLOCK (SAA 10)
052604  177335  	<ptr> 177335		; -> LEAVE-SKIP
