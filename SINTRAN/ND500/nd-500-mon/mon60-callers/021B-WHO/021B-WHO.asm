; ============================================================================
;  021B-WHO  ->  MON 60 subfunction WHO = 21B (0x11 = 17)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146373 SAA 21 ; 146374 JMP I 1 ; 146375 = 146244 (gateway).
;  Two call sites, both inside the command interpreter 002662 (002662-010634).
;  Neither stores a parameter into the block.
; ============================================================================

; ---- Call site 005165 : simple CASE 005165-005167 --------------------------
005165  135056  	JPL I 56		; -> 005243  MON60 WHO (21B) thunk=146373
005166  135231  	JPL I -147		; -> 005017  callsite+1 = ERROR (-> 002673)
005167  125043  	JMP I 43		; -> 005232  callsite+2 = SUCCESS (-> 010613 loop)
005243  146373  	<thunk WHO (SAA 21)>	; bank1[005243] = 146373

; ---- Call site 007445 : local sub-block 007434-007447 (L = return link) -----
; This block is called (via JMP with L set) from within the interpreter; it
; returns through JMP I ,B 111.  WHO is issued only if the result of the prior
; call (007437) equals a constant.
007434  146147  	RADD CLD SL DX		; X := L (return link)
007435  014511  	STX ,B 111		; B+111 := return link (for JMP I ,B 111 later)
007436  004605  	STA ,B -173		; save A into result slot
007437  135152  	JPL I 152		; -> 007611  (call a routine; result in A)
007440  135146  	JPL I 146		; -> 007606  (its error return)
007441  044605  	LDA ,B -173		; A := saved value
007442  050150  	LDT 150			; T := const [007612]
007443  140065  	SKP IF DA EQL ST	; skip next if A == const
007444  124003  	JMP 3			; -> 007447 (A != const: skip WHO)
007445  135146  	JPL I 146		; -> 007613  MON60 WHO (21B) thunk=146373
007446  135140  	JPL I 140		; -> 007606  callsite+1 = ERROR
007447  125511  	JMP I ,B 111		; -> 007560  callsite+2 = SUCCESS (return via saved link)
007613  146373  	<thunk WHO (SAA 21)>	; bank1[007613] = 146373
