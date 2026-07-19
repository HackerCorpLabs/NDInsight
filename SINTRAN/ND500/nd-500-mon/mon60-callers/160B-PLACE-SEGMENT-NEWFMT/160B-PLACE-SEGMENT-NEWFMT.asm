; ============================================================================
;  160B PLACE-SEGMENT-NEWFMT (IN5SEGLOAD)  ->  MON 60 subfunction 160B (0x70 = 112 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147000 : SAA 160 ; JMP I 1 ; 146244  (verbatim .asm line 52816-52818).
; ============================================================================

; CALL SITE 1: 060562, in ENTER-routine 060374 (framesize 000004)
060544  045064  	LDA I 64
060545  006001  	STA ,X 1
060546  044064  	LDA 64
060547  054602  	LDX ,B -176
060550  006006  	STA ,X 6
060551  146135  	RADD CLD SB DA
060552  172630  	AAA -150
060553  006007  	STA ,X 7
060554  044057  	LDA 57
060555  006010  	STA ,X 10
060556  170436  	SAA 36
060557  144151  	SWAP CLD SA DD
060560  050054  	LDT 54
060561  032011  	STF ,X 11
060562  135053  	JPL I 53		; -> 060635
060563  125623  	JMP I ,B -155		; -> 060406
060564  044613  	LDA ,B -165

; CALL SITE 2: 061135, in ENTER-routine 060374 (framesize 000004)
061117  045146  	LDA I 146
061120  006001  	STA ,X 1
061121  044142  	LDA 142
061122  054602  	LDX ,B -176
061123  006006  	STA ,X 6
061124  146135  	RADD CLD SB DA
061125  172630  	AAA -150
061126  006007  	STA ,X 7
061127  044135  	LDA 135
061130  006010  	STA ,X 10
061131  170436  	SAA 36
061132  144151  	SWAP CLD SA DD
061133  050133  	LDT 133
061134  032011  	STF ,X 11
061135  135132  	JPL I 132		; -> 061267
061136  125623  	JMP I ,B -155		; -> 060761
061137  044613  	LDA ,B -165

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Authoritative purpose: LOAD (PLACE) ONE SEGMENT (NEW DOMAIN FORMAT) (IN5SEGLOAD) - SUBFUNCTION-TABLE.md. Server handler IN5SEGLOAD.
; Site1 (060562) params: 060547 LDX ,B-176; 060550 STA ,X 6 = param1 (A from 060546 LDA 64); 060553 STA ,X 7 = &(B-150); 060555 STA ,X 10 = param3 (A from 060554 LDA 57); 060561 STF ,X 11 = param4 (3-word float, 060556 SAA 36/060560 LDT 54). PROVEN stores; field semantics INFERRED (segment descriptor).
; Site1 err 060563 JMP I ,B-155 -> ptr 060406=032006 (frame-relative error exit); ok 060564 continues.
; Site2 (061135) params: 061122 STA ,X 6 (A from 061121 LDA 142); 061126 STA ,X 7 = &(B-150); 061130 STA ,X 10 (A from 061127 LDA 135); 061134 STF ,X 11 (3-word float). PROVEN.
; Site2 err 061136 JMP I ,B-155 -> ptr 060761=047060; ok 061137 continues.
; octal 160 = 0x70 = 112 decimal.
