; ============================================================================
;  163B UNDOC (server dispatch 5NOPAR)  ->  MON 60 subfunction 163B (0x73 = 115 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147011 : SAA 163 ; JMP I 1 ; 146244  (verbatim .asm line 52825-52827).
; ============================================================================

; CALL SITE 1: 141002, in ENTER-routine 140763 (framesize 000032)
140774  171400  	SAX 0
140775  050621  	LDT ,B -157
140776  142200  	LBYT
140777  171131  	SAT 131
141000  140065  	SKP IF DA EQL ST
141001  124003  	JMP 3		; -> 141004
141002  135157  	JPL I 157		; -> 141161
141003  134364  	JPL -14		; -> 140767
141004  170401  	SAA 1

; CALL SITE 2: 143045, standalone parameterless wrapper ENTER-routine 143042 (framesize 000000)
143042  146547  	RADD AD1 CLD SL DX
143043  135005  	JPL I 5		; -> 143050
143044  000000  	STZ 0
143045  135004  	JPL I 4		; -> 143051
143046  135004  	JPL I 4		; -> 143052
143047  135004  	JPL I 4		; -> 143053
143050  177300  	BAND 
143051  147011  	RADD ADC SD DD
143052  177327  	BAND 120 DX
143053  177335  	BAND 130 DA

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Undocumented: no FUNCTION= comment; dispatch generic 5NOPAR. Purpose UNKNOWN.
; Site1 (141002): guarded by a byte test 140775 LDT ,B-157 / 140776 LBYT / 140777 SAT 131 / 141000 SKP IF DA EQL ST. NO parameter store precedes the call -> no-parameter dispatch. PROVEN.
; Site1 err 141003 -> 140767 (inner error/leaf handler, ptr 141161=147011 confirms thunk); ok 141004 (SAA 1...) continues.
; Site2 (143045): parameterless wrapper; err 143046 -> ptr 143052=177327; ok 143047 -> ptr 143053=177335.
; 143050-143053 = pointer pool (177300, 147011 thunk, 177327, 177335).
; octal 163 = 0x73 = 115 decimal.
