; ============================================================================
;  017B-LISOP  ->  MON 60 subfunction LISOP = 17B (0x0F = 15)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146365 SAA 17 ; 146366 JMP I 1 ; 146367 = 146244 (gateway).
; ----------------------------------------------------------------------------
;  Call site 005157, a CASE inside the command interpreter ENTER-routine 002662
;  (framesize 000331, spans 002662-010634).  Case = 005157-005161.
;  ptr[005017]=002673 (shared error reporter); ptr[005232]=010613 (command loop).
;  This case stores NO parameter into the block: LISOP is issued directly.

005157  135062  	JPL I 62		; -> 005241  MON60 LISOP (17B) thunk=146365
005160  135237  	JPL I -141		; -> 005017  callsite+1 = ERROR (-> 002673)
005161  125051  	JMP I 51		; -> 005232  callsite+2 = SUCCESS (-> 010613 loop)
005241  146365  	<thunk LISOP (SAA 17)>	; bank1[005241] = 146365
