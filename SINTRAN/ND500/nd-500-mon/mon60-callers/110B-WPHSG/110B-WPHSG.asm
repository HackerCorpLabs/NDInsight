; ============================================================================
;  WPHSG  ->  MON 60 subfunction 110B  (WRITE INTO A PHYSICAL SEGMENT)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 110B = 0x48 = 72 dec.  Thunk 146423 (SAA 110; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  yaml params: <segm no.> <ND-500 address> <no. of bytes> <data area>.
; ============================================================================
;  Call site 055736  in standalone ENTER-routine 055255 (framesize 000302 = 194 dec).
;  Shared error pool 056027 (177327 LEAVE-value); success pool 056025 -> 056041 (177335).
;  Verified: 055736 JPL I 74 -> ptr 056032, bank1[056032]=146423 = SAA 110.

055722  044606  	LDA ,B -172		; A := local B-172
055723  172405  	AAA 5			; A := (B-172)+5
055724  054602  	LDX ,B -176		; X := stack top
055725  006006  	STA ,X 6		; param1 := (B-172)+5 = <segm no.>
055726  146135  	RADD CLD SB DA
055727  172612  	AAA -166		; A := B-166
055730  006007  	STA ,X 7		; param2 := &(B-166) = <ND-500 address>
055731  146135  	RADD CLD SB DA
055732  172610  	AAA -170		; A := B-170
055733  006010  	STA ,X 10		; param3 := &(B-170) = <no. of bytes>
055734  034623  	LDF ,B -155		; F := descriptor at B-155
055735  032011  	STF ,X 11		; param4 := F = <data area> (3 words)
055736  135074  	JPL I 74		; -> ptr 056032 = thunk 146423  MON60 WPHSG
055737  135070  	JPL I 70		; callsite+1 ERROR   -> ptr 056027 = 177327 LEAVE(value)
055740  124065  	JMP 65			; callsite+2 SUCCESS -> 056025 (JPL I 14 -> 056041 = 177335 LEAVE-SKIP)
   ; pool: 056027=177327, 056032=146423 (thunk WPHSG), 056041=177335
