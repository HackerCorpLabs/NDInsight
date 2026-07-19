; ============================================================================
;  165B UNDOC (server dispatch 5NOPAR)  ->  MON 60 subfunction 165B (0x75 = 117 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147017 : SAA 165 ; JMP I 1 ; 146244  (verbatim .asm line 52831-52833).
; ============================================================================

; CALL SITE 1: 140617, in ENTER-routine 140575 (framesize 000014) - a chained subfunction sequence
140606  170451  	SAA 51
140607  004606  	STA ,B -172
140610  170500  	SAA 100
140611  005127  	STA I 127
140612  054602  	LDX ,B -176
140613  006006  	STA ,X 6
140614  044125  	LDA 125
140615  135125  	JPL I 125		; -> 140742
140616  134363  	JPL -15		; -> 140601
140617  135124  	JPL I 124		; -> 140743
140620  134361  	JPL -17		; -> 140601
140621  135123  	JPL I 123		; -> 140744
140622  134357  	JPL -21		; -> 140601

; CALL SITE 2: 141322, standalone parameterless wrapper ENTER-routine 141317 (framesize 000000)
141317  146547  	RADD AD1 CLD SL DX
141320  135005  	JPL I 5		; -> 141325
141321  000000  	STZ 0
141322  135004  	JPL I 4		; -> 141326
141323  135004  	JPL I 4		; -> 141327
141324  135004  	JPL I 4		; -> 141330
141325  177300  	BAND 
141326  147017  	RADD ADC SD DX
141327  177327  	BAND 120 DX
141330  177335  	BAND 130 DA

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Undocumented: no FUNCTION= comment; dispatch generic 5NOPAR. Purpose UNKNOWN.
; Routine 140575 issues a SEQUENCE of subfunctions; the SUCCESS (callsite+2) of one call is the next call, and every failure branches to inner error handler 140601 (140605 JMP I ,B-164 -> 140421). PROVEN structure.
; Site1 (140617): the nearest preceding parameter store is 140612 LDX ,B-176 / 140613 STA ,X 6 (param1 := A, A from 140610 SAA 100 / 140611 STA I 127). Because this is a chained sequence over shared slots, the exact parameter attributable to 165B vs the neighbouring calls is not separable; treat as generic dispatch. INFERRED.
; Site1 err 140620 -> 140601 (inner error handler); ok 140621 = the next call (167B).
; Site2 (141322): parameterless wrapper, no param stores; err 141323 -> ptr 141327=177327; ok 141324 -> ptr 141330=177335.
; octal 165 = 0x75 = 117 decimal.
