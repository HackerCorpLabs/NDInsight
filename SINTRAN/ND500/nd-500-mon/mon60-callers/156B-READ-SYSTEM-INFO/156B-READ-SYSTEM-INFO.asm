; ============================================================================
;  156B READ-SYSTEM-INFO  ->  MON 60 subfunction 156B (0x6E = 110 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 146772 : SAA 156 ; JMP I 1 ; 146244  (verbatim .asm line 52810-52812).
; ============================================================================

; CALL SITE: 143074, standalone ENTER-routine 143066 (framesize 000000)
143066  146547  	RADD AD1 CLD SL DX
143067  135010  	JPL I 10		; -> 143077
143070  000000  	STZ 0
143071  044007  	LDA 7
143072  054602  	LDX ,B -176
143073  006006  	STA ,X 6
143074  135005  	JPL I 5		; -> 143101
143075  135005  	JPL I 5		; -> 143102
143076  135005  	JPL I 5		; -> 143103
143077  177300  	BAND 
143100  136266  	JPL ,X -112		; -> 142766
143101  146772  	RADD AD1 CM1 CLD SX DP
143102  177327  	BAND 120 DX
143103  177335  	BAND 130 DA

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Authoritative purpose: READ SYSTEM INFO - SUBFUNCTION-TABLE.md. Server handler 5NOPAR (generic).
; Tiny wrapper (0 locals). One MON60 param: 143071 LDA 7 loads one word; 143073 STA ,X 6 = param1. PROVEN store; the source of 'LDA 7' (word 044007) not resolved to a named object: INFERRED.
; err 143075 -> ptr 143102=177327 (LEAVE error); ok 143076 -> ptr 143103=177335 (LEAVE-SKIP success).
; 143077-143103 is this wrapper's pointer pool (177300 ENTER, 146772 thunk, 177327, 177335).
; octal 156 = 0x6E = 110 decimal.
