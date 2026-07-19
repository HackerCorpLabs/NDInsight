; ============================================================================
;  SETBLK  ->  MON 60 subfunction SETBLK = 141B (0x61 = 97 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  Purpose (SUBFUNCTION-TABLE.md row 141, dispatch 5NOPAR): set block size of a
;  file.  Operator command SET-BLOCK-SIZE (INDEX.md sec 2.2).
;  Enclosing routine : command interpreter ENTER-routine 002662 (framesize
;  000331), spanning 002662..010634.  Call site = 005154.
;  Thunk VERIFIED: bank1[005240]=146742 ; 146742 SAA 141 ; 146743 JMP I 1 ;
;  146744 146244 (gateway).

; ---- SETBLK command case (005131..005156) ----
005131  020505  	STD ,B 105		; local(B+105) := operand1 (32-bit value in D)
005132  146135  	RADD CLD SB DA
005133  172505  	AAA 105			; A := &local(B+105)
005134  054602  	LDX ,B -176
005135  006006  	STA ,X 6		; MON60 param1 := &local(B+105)
005136  146175  	RADD CLD SX DA		; A := X (stack top)
005137  172407  	AAA 7			; A := X+7
005140  004602  	STA ,B -176		; advance stack top +7 (frame for the nested helper call)
005141  170401  	SAA 1			; operand selector = 1
005142  135257  	JPL I -121		; -> ptr 005021 = numeric-operand evaluator (INFERRED); returns D
005143  135254  	JPL I -124		; helper err -> ptr 005017 = 002673 (interp error)
005144  020507  	STD ,B 107		; local(B+107) := operand2 (returned in D)
005145  044602  	LDA ,B -176
005146  172771  	AAA -7			; restore stack top -7
005147  004602  	STA ,B -176
005150  146135  	RADD CLD SB DA
005151  172507  	AAA 107			; A := &local(B+107)
005152  054602  	LDX ,B -176
005153  006007  	STA ,X 7		; MON60 param2 := &local(B+107)
005154  135064  	JPL I 64		; -> ptr 005240 = thunk 146742  SETBLK (141B)  *** MON 60 ***
005155  135242  	JPL I -136		; callsite+1 ERROR -> ptr 005017 = 002673 (interp error)
005156  125054  	JMP I 54		; callsite+2 SUCCESS -> ptr 005232 = 010613 (command loop)
; ---- relevant pointer pool words (data) ----
005017  002673  	<ptr>  -> routine 002673  (interpreter error reporter)
005232  010613  	<ptr>  -> 010613  (command loop)
005240  146742  	<ptr>  -> thunk 146742 (SETBLK 141B)
