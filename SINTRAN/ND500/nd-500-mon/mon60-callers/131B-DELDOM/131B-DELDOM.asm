; ============================================================================
;  DELDOM  ->  MON 60 subfunction DELDOM = 131B (0x59 = 89 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  DELETE STANDARD DOMAIN.  Standard-domain family: 127B DEFDOM (define),
;  130B START (already carved as START-STANDARD-DOMAIN), 131B DELDOM (delete).
;  Enclosing routine : command interpreter ENTER-routine 002662 (framesize
;  000331), spanning 002662..010634.  Call site = 007347.
;  Thunk VERIFIED: bank1[007411]=146715 ; 146715 SAA 131 ; 146716 JMP I 1 ;
;  146717 146244 (gateway).

; ---- DELDOM command sequence (007344..007351) ----
007344  034665  	LDF ,B -113		; F := domain-name descriptor (3 words) from B-113
007345  054602  	LDX ,B -176		; X := gateway param base
007346  032006  	STF ,X 6		; MON60 param1 := domain name (F, 3 words)
007347  135042  	JPL I 42		; -> ptr 007411 = thunk 146715  DELDOM (131B)  *** MON 60 ***
007350  135221  	JPL I -157		; callsite+1 ERROR -> ptr 007171 = 002673 (interp error)
007351  125025  	JMP I 25		; callsite+2 SUCCESS -> ptr 007376 = 010613 (command loop)
; ---- relevant pointer pool words (data) ----
007171  002673  	<ptr>  -> routine 002673  (interpreter error reporter)
007376  010613  	<ptr>  -> 010613  (command loop)
007411  146715  	<ptr>  -> thunk 146715 (DELDOM 131B)
