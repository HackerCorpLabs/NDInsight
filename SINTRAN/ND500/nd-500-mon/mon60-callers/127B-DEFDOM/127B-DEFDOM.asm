; ============================================================================
;  DEFDOM  ->  MON 60 subfunction DEFDOM = 127B (0x57 = 87 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  DEFINE STANDARD DOMAIN (operator command DEFINE-STANDARD-DOMAIN; see
;  mon60-callers/INDEX.md sec 2.1).  Standard-domain family: 127B DEFDOM
;  (define), 130B START (carved as START-STANDARD-DOMAIN), 131B DELDOM (delete).
;  Enclosing routine : ENTER-routine 045463 (framesize 003116 = 1614 dec).
;    (045463 RADD AD1 CLD SL DX ; 045464 JPL I 175 -> ptr 045661=177300 ENTER)
;  Call site = 046056.
;  Thunk VERIFIED: bank1[046105]=146707 ; 146707 SAA 127 ; 146710 JMP I 1 ;
;  146711 146244 (gateway).

; ---- DEFDOM call sequence (046053..046060) ----
046053  044424  	LDA ,B 24		; A := local(B+24)  (domain-definition operand; INFERRED)
046054  054602  	LDX ,B -176		; X := gateway param base
046055  006006  	STA ,X 6		; MON60 param1 := local(B+24)
046056  135027  	JPL I 27		; -> ptr 046105 = thunk 146707  DEFDOM (127B)  *** MON 60 ***
046057  135021  	JPL I 21		; callsite+1 ERROR -> ptr 046100 = routine 045511
046060  124010  	JMP 10		; callsite+2 SUCCESS -> 046070
; ---- relevant pointer pool words (data) ----
046100  045511  	<ptr>  -> routine 045511  (error handler)
046105  146707  	<ptr>  -> thunk 146707 (DEFDOM 127B)
; NOTE: only slot 6 is written adjacent to the call; no ,X 7/10 stores here.
