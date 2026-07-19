; ============================================================================
;  170B READ-CPU-TYPE-AND-MIC-VERSION  ->  MON 60 subfunction 170B (0x78 = 120 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147033 : SAA 170 ; JMP I 1 ; 146244  (verbatim .asm line 52843-52845).
; ============================================================================

; CALL SITE: 143134, in ENTER-routine 143104 (framesize 000004)
143126  044017  	LDA 17
143127  054602  	LDX ,B -176
143130  006006  	STA ,X 6
143131  044014  	LDA 14
143132  172402  	AAA 2
143133  006007  	STA ,X 7
143134  135012  	JPL I 12		; -> 143146
143135  134353  	JPL -25		; -> 143110
143136  170401  	SAA 1

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Authoritative purpose: READ ND-500 CPU-TYPE AND MIC.VERSION - SUBFUNCTION-TABLE.md. Server handler 5NOPAR (generic).
; Two MON60 params: 143126 LDA 17; 143127 LDX ,B-176; 143130 STA ,X 6 = param1. 143131 LDA 14 / 143132 AAA 2 / 143133 STA ,X 7 = param2 (a buffer address, value 14-word + 2). PROVEN stores; field semantics INFERRED (CPU-type / mic-version return buffers).
; err 143135 JPL -25 -> 143110 (inner error handler in this routine); ok 143136 (SAA 1...) continues.
; octal 170 = 0x78 = 120 decimal.
