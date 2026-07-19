; ============================================================================
;  SETOUT  ->  MON 60 subfunction SETOUT = 120B (0x50 = 80 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  Enclosing routine : the big command interpreter ENTER-routine 002662
;  (framesize 000331), spanning 002662..010634 (same routine that holds
;  LOAD-CONTROL-STORE).  SETOUT is one command CASE.  Call site = 003573.
;  Thunk VERIFIED: bank1[003751]=146662 ; 146662 SAA 120 ; 146663 JMP I 1 ;
;  146664 146244 (gateway).

; ---- SETOUT command case (003556..003576) ----
003556  054602  	LDX ,B -176
003557  046006  	LDA ,X 6
003560  005362  	STA I -16		; (prior handling; stores A via indirect pointer)
003561  124003  	JMP 3		; -> 003564
003562  170401  	SAA 1
003563  005357  	STA I -21
003564  045356  	LDA I -22		; A := indirect load (P-rel ptr @003542)
003565  154760  	SAD SHR 20		; D := D >> 16   (extract a 16-bit value)
003566  020651  	STD ,B -127		; local(B-127) := D
003567  146135  	RADD CLD SB DA		; A := B
003570  172651  	AAA -127		; A := &local(B-127)
003571  054602  	LDX ,B -176		; X := stack top (gateway param base)
003572  006006  	STA ,X 6		; MON60 param1 := &local(B-127)
003573  135156  	JPL I 156		; -> ptr 003751 = thunk 146662  SETOUT (120B)  *** MON 60 ***
003574  135154  	JPL I 154		; callsite+1 ERROR -> ptr 003750 = 002673 (interp error reporter)
003575  125332  	JMP I -46		; callsite+2 SUCCESS -> 003527 (command continue)
003576  125331  	JMP I -47		; -> 003527
; ---- relevant pointer pool words (data) ----
003747  171036  	<ptr>  (used by earlier calls in this region)
003750  002673  	<ptr>  -> routine 002673  (interpreter error reporter)
003751  146662  	<ptr>  -> thunk 146662 (SETOUT 120B)
