; ============================================================================
;  SPREL  ->  MON 60 subfunction 044B  (RELEASE ND-500 CPU/SYSTEM FROM SPECIAL USE)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 044B = 0x24 = 36 dec.  Thunk 146472 (SAA 44; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  yaml params: (none).
; ============================================================================
;  Call site 007505  -- CASE inside command interpreter ENTER 002662 (fs 000331=217).
;           Case body 007477-007507.  Error target 007500 (local code),
;           success target 007376 (= 010613, command loop).
;           Verified: 007505 JPL I 120 -> ptr 007625, bank1[007625]=146472 = SAA 44.

007477  124006  	JMP 6			; (entry into this case) -> 007505
007500  146147  	RADD CLD SL DX		; <-- local error handler for this case
007501  014514  	STX ,B 114
007502  004605  	STA ,B -173
007503  135116  	JPL I 116		; -> ptr 007621
007504  125514  	JMP I ,B 114		; -> ptr 007620
007505  135120  	JPL I 120		; -> ptr 007625 = thunk 146472  MON60 SPREL  (no params)
007506  134372  	JPL -6			; callsite+1 ERROR   -> 007500 (local error handler)
007507  125267  	JMP I -111		; callsite+2 SUCCESS -> ptr 007376 = 010613 (command loop)
   ; pool: 007376=010613, 007625=146472 (thunk SPREL)
