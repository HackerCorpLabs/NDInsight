; ============================================================================
;  ABSMW  ->  MON 60 subfunction 033B  (PHYSICAL DATA MEMORY WRITE / abs mem write)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 033B = 0x1B = 27 dec.  Thunk 146431 (SAA 33; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  yaml params: <no. of bytes> <ND-500 addr.> <data area>.
; ============================================================================
;  Call site 055717  in standalone ENTER-routine 055255 (framesize 000302 = 194 dec).
;  Same routine as WPHSG (055736); shared error pool 056027 (177327), success 056025.
;  Verified: 055717 JPL I 112 -> ptr 056031, bank1[056031]=146431 = SAA 33.

055706  146135  	RADD CLD SB DA		; A := B
055707  172610  	AAA -170		; A := B-170
055710  054602  	LDX ,B -176		; X := stack top
055711  006006  	STA ,X 6		; param1 := &(B-170) = <no. of bytes>
055712  146135  	RADD CLD SB DA
055713  172612  	AAA -166		; A := B-166
055714  006007  	STA ,X 7		; param2 := &(B-166) = <ND-500 addr.>
055715  034623  	LDF ,B -155		; F := descriptor at B-155
055716  032010  	STF ,X 10		; param3 := F = <data area> (3 words)
055717  135112  	JPL I 112		; -> ptr 056031 = thunk 146431  MON60 ABSMW
055720  135107  	JPL I 107		; callsite+1 ERROR   -> ptr 056027 = 177327 LEAVE(value)
055721  124104  	JMP 104			; callsite+2 SUCCESS -> 056025 (JPL I 14 -> 056041 = 177335 LEAVE-SKIP)
   ; pool: 056027=177327, 056031=146431 (thunk ABSMW), 056041=177335
