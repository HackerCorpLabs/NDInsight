; ============================================================================
;  PLADBG  ->  MON 60 subfunction PLADBG = 134B (0x5C = 92 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  PLACE DEBUGGER.  Enclosing routine : ENTER-routine 002527 (framesize 000075
;  = 61 dec).
;    (002527 RADD AD1 CLD SL DX ; 002530 JPL I 70 -> ptr 002620=177300 ENTER)
;  Call site = 002560.
;  Thunk VERIFIED: bank1[002624]=146726 ; 146726 SAA 134 ; 146727 JMP I 1 ;
;  146730 146244 (gateway).

; ---- PLADBG main-path call sequence (002552..002562) ----
002552  146135  	RADD CLD SB DA		; A := B
002553  172611  	AAA -167		; A := &local(B-167)
002554  054602  	LDX ,B -176		; X := gateway param base
002555  006006  	STA ,X 6		; MON60 param1 := &local(B-167)
002556  034606  	LDF ,B -172		; F := descriptor (3 words) from input B-172
002557  032007  	STF ,X 7		; MON60 param2 := descriptor (F, 3 words)
002560  135044  	JPL I 44		; -> ptr 002624 = thunk 146726  PLADBG (134B)  *** MON 60 ***
002561  135041  	JPL I 41		; callsite+1 ERROR -> ptr 002622 = 177327 LEAVE(value)
002562  045043  	LDA I 43		; callsite+2 SUCCESS (continues in-line)
; ---- relevant pointer pool words (data) ----
002622  177327  	<ptr>  -> 177327  LEAVE(value)  (error return of routine 002527)
002624  146726  	<ptr>  -> thunk 146726 (PLADBG 134B)
; NOTE: an alternate branch at 002536..002546 (taken when local(X+2) < 0) sets
;   param1 := 1 and param2 := F@(local(B-165)+3), then calls routine 043011 via
;   ptr 002621 - that is NOT a MON 60 thunk and is a different code path.
