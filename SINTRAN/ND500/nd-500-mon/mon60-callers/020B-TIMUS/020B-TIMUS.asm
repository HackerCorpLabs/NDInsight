; ============================================================================
;  020B-TIMUS  ->  MON 60 subfunction TIMUS = 20B (0x10 = 16)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146370 SAA 20 ; 146371 JMP I 1 ; 146372 = 146244 (gateway).
; ----------------------------------------------------------------------------
;  Call site 005162, a CASE inside the command interpreter ENTER-routine 002662
;  (framesize 000331, spans 002662-010634).  Case = 005162-005164.
;  ptr[005017]=002673 (shared error reporter); ptr[005232]=010613 (command loop).
;  This case stores NO parameter into the block: TIMUS is issued directly.

005162  135060  	JPL I 60		; -> 005242  MON60 TIMUS (20B) thunk=146370
005163  135234  	JPL I -144		; -> 005017  callsite+1 = ERROR (-> 002673)
005164  125046  	JMP I 46		; -> 005232  callsite+2 = SUCCESS (-> 010613 loop)
005242  146370  	<thunk TIMUS (SAA 20)>	; bank1[005242] = 146370
