; ============================================================================
;  173B SET-CPU-STATUS (ICPUSTAT)  ->  MON 60 subfunction 173B (0x7B = 123 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147044 : SAA 173 ; JMP I 1 ; 146244  (verbatim .asm line 52852-52854).
; ============================================================================

; CALL SITE: 032605, in ENTER-routine 032442 (framesize 000046)
032567  054606  	LDX ,B -172
032570  173403  	AAX 3
032571  046000  	LDA ,X 0
032572  054602  	LDX ,B -176
032573  006006  	STA ,X 6
032574  146135  	RADD CLD SB DA
032575  172641  	AAA -137
032576  006007  	STA ,X 7
032577  146135  	RADD CLD SB DA
032600  172637  	AAA -141
032601  006010  	STA ,X 10
032602  146135  	RADD CLD SB DA
032603  172643  	AAA -135
032604  006011  	STA ,X 11
032605  135010  	JPL I 10		; -> 032615
032606  135004  	JPL I 4		; -> 032612
032607  135007  	JPL I 7		; -> 032616

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Authoritative purpose: SET CPU STATUS (ICPUSTAT) - SUBFUNCTION-TABLE.md. Server handler ICPUSTAT.
; FOUR MON60 params: 032567 LDX ,B-172 / 032570 AAX 3 / 032571 LDA ,X 0 / 032573 STA ,X 6 = param1 (word at (B-172)+3); 032575 AAA -137 / 032576 STA ,X 7 = &(B-137); 032600 AAA -141 / 032601 STA ,X 10 = &(B-141); 032603 AAA -135 / 032604 STA ,X 11 = &(B-135). PROVEN stores; field semantics INFERRED (CPU status word + return buffers).
; err 032606 -> ptr 032612=177327 (LEAVE error); ok 032607 -> ptr 032616=177335 (LEAVE-SKIP).
; 032610-032616 region includes this routine's pointer pool (177300, 177327, 147044 thunk, 177335).
; octal 173 = 0x7B = 123 decimal.
