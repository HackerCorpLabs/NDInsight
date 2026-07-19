; ============================================================================
;  171B UNDOC (server dispatch 5NOPAR)  ->  MON 60 subfunction 171B (0x79 = 121 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147036 : SAA 171 ; JMP I 1 ; 146244  (verbatim .asm line 52846-52848).
; ============================================================================

; CALL SITE: 123671, in ENTER-routine 123577 (framesize 000064)
123660  154760  	SAD SHR 20
123661  020620  	STD ,B -160
123662  146135  	RADD CLD SB DA
123663  172622  	AAA -156
123664  054602  	LDX ,B -176
123665  006006  	STA ,X 6
123666  146135  	RADD CLD SB DA
123667  172620  	AAA -160
123670  006007  	STA ,X 7
123671  135112  	JPL I 112		; -> 124003
123672  135127  	JPL I 127		; -> 124021
123673  124124  	JMP 124		; -> 124017

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Undocumented: no FUNCTION= comment; dispatch generic 5NOPAR. Purpose UNKNOWN.
; Two pointer params: 123663 AAA -156 / 123665 STA ,X 6 = &(B-156); 123667 AAA -160 / 123670 STA ,X 7 = &(B-160). PROVEN stores; meaning UNKNOWN.
; (A separate, non-MON60 helper call precedes at 123656 JPL I 123 -> ptr 124001=053270, not a thunk; role not traced.)
; err 123672 -> ptr 124021=177327 (LEAVE error); ok 123673 (JMP 124 -> 124017) continues.
; octal 171 = 0x79 = 121 decimal.
