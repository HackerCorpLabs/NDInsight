; ============================================================================
;  LIST-STANDARD-DOMAINS  ->  MON 60 subfunction LSTDOM = 132B (0x5A)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.  Annotations below are produced mechanically by
;  resolving each pointer word against the verified thunk table.
;  Convention (proven, prog.md sec 4/5): callsite+1 = ERROR (direct return),
;  callsite+2 = SUCCESS (skip return).
; ============================================================================
;  Command case inside interpreter ENTER-routine 002662 (framesize 000331).
;  NPL purpose of 132B: LIST STANDARD DOMAINS.  MON 60: LSTDOM (132B) at 007352
;  (no parameters).  The immediately preceding case is DELETE STANDARD DOMAIN
;  (DELDOM 131B) at 007347 (param: name @B-113); shown for context.
;    131B = DELETE STANDARD DOMAIN (IDLSYDOM)
;    132B = LIST STANDARD DOMAINS
;  Error -> internal error routine 002673 (007171 pool word);
;  Success -> command loop 010613 (007376 pool word).  Pool 007404..007414.

; ---- DELDOM(131B)@007347 + LIST-STANDARD-DOMAINS LSTDOM(132B)@007352 (007344-007354) ----
007344  034665  	LDF ,B -113
007345  054602  	LDX ,B -176
007346  032006  	STF ,X 6
007347  135042  	JPL I 42		; -> 007411  MON60 DELDOM (131B) thunk=146715
007350  135221  	JPL I -157		; -> 007171  ptr[007171]=002673 -> routine 002673
007351  125025  	JMP I 25		; -> 007376  ptr[007376]=010613 -> routine 010613
007352  135040  	JPL I 40		; -> 007412  MON60 LSTDOM (132B) thunk=146720
007353  135216  	JPL I -162		; -> 007171  ptr[007171]=002673 -> routine 002673
007354  125022  	JMP I 22		; -> 007376  ptr[007376]=010613 -> routine 010613

; ---- shared pointer pool (007411=DELDOM thunk, 007412=LSTDOM thunk) (007404-007414) ----
007404  146676  	RSUB SX DT
007405  111217  	FMU I -161
007406  146704  	RADD AD1 CM1 CLD 0 DL
007407  070160  	AND 160
007410  045463  	LDA I ,B 63
007411  146715  	RADD AD1 CM1 CLD SD DA
007412  146720  	RADD AD1 CM1 CLD SP 0
007413  111430  	FMU I ,B 30
007414  135171  	JPL I 171		; -> 007605  ptr[007605]=111604 -> routine 111604
