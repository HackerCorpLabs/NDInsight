; ============================================================================
;  RPHSG  ->  MON 60 subfunction 073B  (READ FROM A PHYSICAL SEGMENT)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 073B = 0x3B = 59 dec.  Thunk 146420 (SAA 73; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  yaml params: <phys.segment no.> <address> <no. of bytes> <array>.
; ============================================================================
;  Call site 056407  in standalone ENTER-routine 056042 (framesize 000050 = 40 dec).
;  Verified: 056407 JPL I 53 -> ptr 056462, bank1[056462]=146420 = SAA 73.

056367  044606  	LDA ,B -172		; A := local B-172
056370  172405  	AAA 5			; A := (B-172)+5
056371  054602  	LDX ,B -176		; X := stack top
056372  006006  	STA ,X 6		; param1 := (B-172)+5 = <phys.segment no.>
056373  146135  	RADD CLD SB DA
056374  172615  	AAA -163		; A := B-163
056375  006007  	STA ,X 7		; param2 := &(B-163) = <address>
056376  044247  	LDA -131		; A := <no. of bytes> (P-relative)
056377  006010  	STA ,X 10		; param3 := <no. of bytes> (by value)
056400  170400  	SAA 0
056401  146151  	RADD CLD SA DD
056402  044610  	LDA ,B -170
056403  050054  	LDT 54
056404  030635  	STF ,B -143		; build <array> descriptor into B-143
056405  034635  	LDF ,B -143
056406  032011  	STF ,X 11		; param4 := F = <array> (3 words)
056407  135053  	JPL I 53		; -> ptr 056462 = thunk 146420  MON60 RPHSG
056410  135230  	JPL I -150		; callsite+1 ERROR   -> ptr 056240
056411  125041  	JMP I 41		; callsite+2 SUCCESS -> ptr 056452
   ; pool: 056462=146420 (thunk RPHSG)
