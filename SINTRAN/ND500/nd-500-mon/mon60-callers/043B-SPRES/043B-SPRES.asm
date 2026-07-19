; ============================================================================
;  SPRES  ->  MON 60 subfunction 043B  (RESERVE ND-500 CPU/SYSTEM FOR SPECIAL USE)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 043B = 0x23 = 35 dec.  Thunk 146467 (SAA 43; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  yaml params: (none)  -- BUT this binary stores one word into ,X 6 (see NOTE).
; ============================================================================
;  Call site 007474  -- CASE inside command interpreter ENTER 002662 (fs 000331=217).
;           Case body 007450-007476.  Error target 007434 (local code),
;           success target 007376 (= 010613, command loop).
;           Verified: 007474 JPL I 130 -> ptr 007624, bank1[007624]=146467 = SAA 43.

007464  135134  	JPL I 134		; -> ptr 007620   (sub-call; role not traced)
007465  135134  	JPL I 134		; -> ptr 007621   (its error path)
007466  131403  	JAF 3			; -> 007471
007467  024133  	LDD 133			; D := value (P-relative)
007470  020651  	STD ,B -127		; -> local B-127
007471  024651  	LDD ,B -127		; D := local B-127
007472  054602  	LDX ,B -176		; X := stack top
007473  022006  	STD ,X 6		; ,X 6 := (B-127)   [see NOTE - yaml lists no params]
007474  135130  	JPL I 130		; -> ptr 007624 = thunk 146467  MON60 SPRES
007475  134337  	JPL -41			; callsite+1 ERROR   -> 007434 (local error handler)
007476  125300  	JMP I -100		; callsite+2 SUCCESS -> ptr 007376 = 010613 (command loop)
   ; pool: 007376=010613, 007624=146467 (thunk SPRES); 007434 = local error-handler code
