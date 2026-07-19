; ============================================================================
;  XMONLOG  ->  MON 60 subfunction XMONLOG = 126B (0x56 = 86 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  Enclosing routine : command interpreter ENTER-routine 002662 (framesize
;  000331), spanning 002662..010634.  Call site = 007320.
;  Thunk VERIFIED: bank1[007406]=146704 ; 146704 SAA 126 ; 146705 JMP I 1 ;
;  146706 146244 (gateway).

; ---- preceding command (PRINT-MONCALL-LOG via routine 111217; NOT this cmd) ----
007315  135070  	JPL I 70		; -> ptr 007405 = routine 111217 (PMONLOG print routine)
007316  135253  	JPL I -125		; err -> ptr 007171 = 002673
007317  125057  	JMP I 57		; ok  -> ptr 007376 = 010613
; ---- XMONLOG command (007320..007322) ----
007320  135066  	JPL I 66		; -> ptr 007406 = thunk 146704  XMONLOG (126B)  *** MON 60 ***
007321  135250  	JPL I -130		; callsite+1 ERROR -> ptr 007171 = 002673 (interp error)
007322  125054  	JMP I 54		; callsite+2 SUCCESS -> ptr 007376 = 010613 (command loop)
; ---- relevant pointer pool words (data) ----
007171  002673  	<ptr>  -> routine 002673  (interpreter error reporter)
007376  010613  	<ptr>  -> 010613  (command loop)
007406  146704  	<ptr>  -> thunk 146704 (XMONLOG 126B)
; NOTE: NO 'LDX ,B -176 / STA ,X n' parameter stores precede 007320.
;       XMONLOG is issued with no MON 60 input parameters marshalled.
