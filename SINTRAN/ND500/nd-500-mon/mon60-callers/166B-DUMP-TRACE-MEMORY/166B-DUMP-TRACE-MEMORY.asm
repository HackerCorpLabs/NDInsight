; ============================================================================
;  166B DUMP-TRACE-MEMORY  ->  MON 60 subfunction 166B (0x76 = 118 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147022 : SAA 166 ; JMP I 1 ; 146244  (verbatim .asm line 52834-52836).
; ============================================================================

; CALL SITE: 140654, in ENTER-routine 140575 (framesize 000014) - chained sequence
140642  024105  	LDD 105
140643  020616  	STD ,B -162
140644  146135  	RADD CLD SB DA
140645  172616  	AAA -162
140646  054602  	LDX ,B -176
140647  006006  	STA ,X 6
140650  044101  	LDA 101
140651  144151  	SWAP CLD SA DD
140652  050100  	LDT 100
140653  032007  	STF ,X 7
140654  135077  	JPL I 77		; -> 140753
140655  134324  	JPL -54		; -> 140601

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Authoritative purpose: DUMP-TRACE-MEMORY - SUBFUNCTION-TABLE.md. Server handler 5NOPAR (generic).
; Two MON60 params: 140642 LDD 105 -> 140643 STD ,B-162; 140645 AAA -162; 140646 STA ,X 6 = param1 := &(B-162). 140650 LDA 101 / 140651 SWAP CLD SA DD / 140652 LDT 100 / 140653 STF ,X 7 = param2 (3-word float T,A,D). PROVEN stores; field semantics INFERRED.
; Inside the chained sequence of routine 140575; reached as success of the preceding 63->140755 call at 140672. err 140655 -> 140601 (inner error handler); ok 140656 (LDA I 62...) continues the sequence.
; octal 166 = 0x76 = 118 decimal.
