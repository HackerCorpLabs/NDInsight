; ============================================================================
;  014B-CLSFI  ->  MON 60 subfunction CLSFI = 14B (0x0C = 12)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146354 SAA 14 ; 146355 JMP I 1 ; 146356 = 146244 (gateway).
; ----------------------------------------------------------------------------
;  Call site 005123, a CASE inside the giant command interpreter ENTER-routine
;  at 002662 (framesize 000331, spans 002662-010634).  Case = 005113-005125.
;  Shared helpers used by the case: 002003 (numeric-arg evaluator, via ptr 005021),
;  002673 (shared error reporter, via ptr 005017), 010613 (command loop, via ptr 005232).

005113  170400  	SAA 0			; select operand 0 for the evaluator
005114  135305  	JPL I -73		; -> 005021  ptr[005021]=002003  (evaluate numeric arg)
005115  135302  	JPL I -76		; -> 005017  ptr[005017]=002673  (arg-error path)
005116  020661  	STD ,B -117		; B-117 := evaluated value (file/connect number, INFERRED)
005117  146135  	RADD CLD SB DA
005120  172661  	AAA -117
005121  054602  	LDX ,B -176		; X := gateway frame base
005122  006006  	STA ,X 6		; param1 := &(B-117)
005123  135114  	JPL I 114		; -> 005237  MON60 CLSFI (14B) thunk=146354
005124  135273  	JPL I -105		; -> 005017  callsite+1 = ERROR (-> 002673)
005125  125105  	JMP I 105		; -> 005232  callsite+2 = SUCCESS (-> 010613 loop)
005237  146354  	<thunk CLSFI (SAA 14)>	; bank1[005237] = 146354
