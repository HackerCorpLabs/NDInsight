; ============================================================================
;  022B-ERRFL  ->  MON 60 subfunction ERRFL = 22B (0x12 = 18)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146376 SAA 22 ; 146377 JMP I 1 ; 146400 = 146244 (gateway).
;  Two call sites, both CASES inside the command interpreter 002662 (002662-010634).
;  Each stores ONE constant into param slot ,X 6, then issues ERRFL.  The two
;  cases differ only in that constant (010636 vs 010634).
;  ptr[005017]=002673 (shared error reporter); ptr[005232]=010613 (command loop).
; ============================================================================

; ---- Call site 005173 : CASE 005170-005175 --------------------------------
005170  044054  	LDA 54			; A := [005244] = 010636  (P-relative direct load)
005171  054602  	LDX ,B -176		; X := gateway frame base
005172  006006  	STA ,X 6		; param1 := 010636  (error-flag value, INFERRED)
005173  135052  	JPL I 52		; -> 005245  MON60 ERRFL (22B) thunk=146376
005174  135223  	JPL I -155		; -> 005017  callsite+1 = ERROR (-> 002673)
005175  125035  	JMP I 35		; -> 005232  callsite+2 = SUCCESS (-> 010613 loop)

; ---- Call site 005201 : CASE 005176-005203 --------------------------------
005176  044050  	LDA 50			; A := [005246] = 010634  (P-relative direct load)
005177  054602  	LDX ,B -176
005200  006006  	STA ,X 6		; param1 := 010634  (error-flag value, INFERRED)
005201  135044  	JPL I 44		; -> 005245  MON60 ERRFL (22B) thunk=146376
005202  135215  	JPL I -163		; -> 005017  callsite+1 = ERROR (-> 002673)
005203  125027  	JMP I 27		; -> 005232  callsite+2 = SUCCESS (-> 010613 loop)

; ---- pool ----
005244  010636  	<constant 010636>	; loaded by 005170 LDA 54
005245  146376  	<thunk ERRFL (SAA 22)>	; bank1[005245] = 146376
005246  010634  	<constant 010634>	; loaded by 005176 LDA 50
