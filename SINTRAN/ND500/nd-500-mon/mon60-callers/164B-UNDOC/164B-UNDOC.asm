; ============================================================================
;  164B UNDOC (server dispatch 5NOPAR)  ->  MON 60 subfunction 164B (0x74 = 116 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147014 : SAA 164 ; JMP I 1 ; 146244  (verbatim .asm line 52828-52830).
; ============================================================================

; CALL SITE: 141310, standalone ENTER-routine 141305 (framesize 000000)
141305  146547  	RADD AD1 CLD SL DX
141306  135005  	JPL I 5		; -> 141313
141307  000000  	STZ 0
141310  135004  	JPL I 4		; -> 141314
141311  135004  	JPL I 4		; -> 141315
141312  135004  	JPL I 4		; -> 141316
141313  177300  	BAND 
141314  147014  	RADD ADC SD DL
141315  177327  	BAND 120 DX
141316  177335  	BAND 130 DA

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Undocumented: no FUNCTION= comment; dispatch generic 5NOPAR. Purpose UNKNOWN.
; Parameterless wrapper (0 locals): 141305 ENTER; 141310 issues the subfunction with NO parameter stores; then LEAVE.
; err 141311 -> ptr 141315=177327 (LEAVE error); ok 141312 -> ptr 141316=177335 (LEAVE-SKIP).
; 141313-141316 = pointer pool (177300 ENTER, 147014 thunk, 177327, 177335).
; octal 164 = 0x74 = 116 decimal.
