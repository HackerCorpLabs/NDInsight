; ============================================================================
;  167B UNDOC (server dispatch 5NOPAR)  ->  MON 60 subfunction 167B (0x77 = 119 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147025 : SAA 167 ; JMP I 1 ; 146244  (verbatim .asm line 52837-52839).
; ============================================================================

; CALL SITE 1: 140621, in ENTER-routine 140575 (framesize 000014) - chained sequence
140621  135123  	JPL I 123		; -> 140744
140622  134357  	JPL -21		; -> 140601
140623  170407  	SAA 7

; CALL SITE 2: 143057, standalone parameterless wrapper ENTER-routine 143054 (framesize 000000)
143054  146547  	RADD AD1 CLD SL DX
143055  135005  	JPL I 5		; -> 143062
143056  000000  	STZ 0
143057  135004  	JPL I 4		; -> 143063
143060  135004  	JPL I 4		; -> 143064
143061  135004  	JPL I 4		; -> 143065
143062  177300  	BAND 
143063  147025  	RADD ADC SP DA
143064  177327  	BAND 120 DX
143065  177335  	BAND 130 DA

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Undocumented: no FUNCTION= comment; dispatch generic 5NOPAR. Purpose UNKNOWN.
; Site1 (140621) sits in the chained sequence of routine 140575: it is the SUCCESS (callsite+2) target of the 165B call at 140617. No parameter store is emitted between 140617 and 140621, so 167B here takes no freshly-marshalled parameter (generic dispatch). PROVEN (no STA ,X between).
; Site1 err 140622 -> 140601 (inner error handler); ok 140623 (SAA 7...) continues the sequence.
; Site2 (143057): parameterless wrapper; err 143060 -> ptr 143064=177327; ok 143061 -> ptr 143065=177335.
; 143062-143065 = pointer pool (177300, 147025 thunk, 177327, 177335).
; octal 167 = 0x77 = 119 decimal.
