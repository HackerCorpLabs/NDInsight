; ============================================================================
;  172B READ-HW-SCRATCH-REGISTER-FILE  ->  MON 60 subfunction 172B (0x7A = 122 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147041 : SAA 172 ; JMP I 1 ; 146244  (verbatim .asm line 52849-52851).
; ============================================================================

; PARAM SETUP for CALL SITE 056700, in ENTER-routine 056042 (framesize 000050)
056621  020646  	STD ,B -132
056622  146135  	RADD CLD SB DA
056623  172646  	AAA -132
056624  054602  	LDX ,B -176
056625  006006  	STA ,X 6
056626  146175  	RADD CLD SX DA
056627  172407  	AAA 7
056630  004602  	STA ,B -176
056631  024044  	LDD 44
056632  054602  	LDX ,B -176
056633  022006  	STD ,X 6
056634  025043  	LDD I 43
056635  135037  	JPL I 37		; -> 056674
056636  134347  	JPL -31		; -> 056605
056637  020650  	STD ,B -130
056640  044602  	LDA ,B -176
056641  172771  	AAA -7
056642  004602  	STA ,B -176
056643  146135  	RADD CLD SB DA
056644  172650  	AAA -130
056645  054602  	LDX ,B -176
056646  006007  	STA ,X 7
056647  170400  	SAA 0
056650  146151  	RADD CLD SA DD
056651  044610  	LDA ,B -170
056652  050205  	LDT -173
056653  030635  	STF ,B -143
056654  034635  	LDF ,B -143
056655  032010  	STF ,X 10
056656  124022  	JMP 22		; -> 056700

; CALL SITE 056700 (reached via 056656 JMP 22)
056700  135127  	JPL I 127		; -> 057027
056701  134304  	JPL -74		; -> 056605
056702  124023  	JMP 23		; -> 056725

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Authoritative purpose: READ HW SCRATCH REGISTER FILE - SUBFUNCTION-TABLE.md. Server handler 5NOPAR (generic).
; Three MON60 params (block base X = B-176): 056623 AAA -132 / 056625 STA ,X 6 = &(B-132); 056643 AAA -130 / 056646 STA ,X 7 = &(B-130); 056653-056655 STF ,X 10 = param3 (3-word float, built from B-143). PROVEN stores; field semantics INFERRED.
; (An intermediate helper call at 056635 JPL I 37 -> ptr 056674=174537 (a runtime helper, not a thunk) sits between the param stores; the caller grows/shrinks the stack top by 7 around it at 056626-056630 / 056640-056642, so the 172B param slots are restored to the same block. PROVEN stack arithmetic.)
; err 056701 JPL -74 -> 056605; ok 056702 JMP 23 -> 056725.
; octal 172 = 0x7A = 122 decimal.
