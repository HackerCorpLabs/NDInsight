; ============================================================================
;  RDSWP  ->  MON 60 subfunction RDSWP = 121B (0x51 = 81 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  Three call sites, three distinct enclosing ENTER-routines.  All three
;  marshal FOUR parameters (slots 6,7,10,11) - a consistent RDSWP signature.
;  Thunk VERIFIED (all three point to it):
;    146665 SAA 121 ; 146666 JMP I 1 ; 146667 146244 (gateway).

; ============================================================================
;  CALL SITE 1 : 073152   enclosing ENTER-routine 073115 (framesize 000336)
;    (073115 RADD AD1 CLD SL DX ; 073116 JPL I 173 -> ptr 073311=177300 ENTER)
; ============================================================================
073137  044155  	LDA 155		; A := pooled word (P-rel EA = 073314)
073140  054602  	LDX ,B -176		; X := gateway param base
073141  006006  	STA ,X 6		; param1 := that word
073142  044153  	LDA 153		; A := pooled word (P-rel EA = 073315)
073143  006007  	STA ,X 7		; param2 := that word
073144  146135  	RADD CLD SB DA
073145  172656  	AAA -122		; A := &local(B-122)
073146  006010  	STA ,X 10		; param3 := &local(B-122)
073147  146135  	RADD CLD SB DA
073150  172650  	AAA -130		; A := &local(B-130)
073151  006011  	STA ,X 11		; param4 := &local(B-130)
073152  135144  	JPL I 144		; -> ptr 073316 = thunk 146665  RDSWP (121B)  *** MON 60 ***
073153  135140  	JPL I 140		; callsite+1 ERROR -> ptr 073313 = 177327 LEAVE(value)
073154  146137  	RADD CLD SB DX		; callsite+2 SUCCESS (continues in-line)

; ============================================================================
;  CALL SITE 2 : 074310   enclosing ENTER-routine 074267 (framesize 000007)
;    (074267 RADD AD1 CLD SL DX ; 074270 JPL I 40 -> ptr 074330=177300 ENTER)
; ============================================================================
074272  045037  	LDA I 37		; (guard load)
074273  131420  	JAF 20		; -> 074313  (alternate path if flag)
074274  146135  	RADD CLD SB DA
074275  172606  	AAA -172		; A := &local(B-172)
074276  054602  	LDX ,B -176
074277  006006  	STA ,X 6		; param1 := &local(B-172)
074300  146135  	RADD CLD SB DA
074301  172610  	AAA -170		; A := &local(B-170)
074302  006007  	STA ,X 7		; param2 := &local(B-170)
074303  044612  	LDA ,B -166		; A := local(B-166) value
074304  006010  	STA ,X 10		; param3 := local(B-166)
074305  146135  	RADD CLD SB DA
074306  172613  	AAA -165		; A := &local(B-165)
074307  006011  	STA ,X 11		; param4 := &local(B-165)
074310  135022  	JPL I 22		; -> ptr 074332 = thunk 146665  RDSWP (121B)  *** MON 60 ***
074311  135022  	JPL I 22		; callsite+1 ERROR -> ptr 074333 = 177327 LEAVE(value)
074312  124015  	JMP 15		; callsite+2 SUCCESS -> 074327

; ============================================================================
;  CALL SITE 3 : 107515   enclosing ENTER-routine 103722 (framesize 000605)
;    (103722 RADD AD1 CLD SL DX ; 103723 JPL I 173 -> ptr 104116=177300 ENTER)
;    NOTE: no ENTER prologue appears between 103722 and 107515 - this is a
;    large routine and 107515 lies within it (PROVEN by scan).
; ============================================================================
107504  044173  	LDA 173		; A := pooled word (P-rel EA = 107677)
107505  054602  	LDX ,B -176
107506  006006  	STA ,X 6		; param1 := that word
107507  044171  	LDA 171		; A := pooled word (P-rel EA = 107700)
107510  006007  	STA ,X 7		; param2 := that word
107511  044634  	LDA ,B -144		; A := local(B-144) value
107512  006010  	STA ,X 10		; param3 := local(B-144)
107513  044166  	LDA 166		; A := pooled word (P-rel EA = 107701)
107514  006011  	STA ,X 11		; param4 := that word
107515  135165  	JPL I 165		; -> ptr 107702 = thunk 146665  RDSWP (121B)  *** MON 60 ***
107516  125643  	JMP I ,B -135		; callsite+1 ERROR -> 107361 (frame-relative dispatch)
107517  024164  	LDD 164		; callsite+2 SUCCESS (continues in-line)
