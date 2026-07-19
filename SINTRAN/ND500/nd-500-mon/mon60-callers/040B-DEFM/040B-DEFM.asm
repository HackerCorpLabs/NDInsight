; ============================================================================
;  DEFM  ->  MON 60 subfunction 040B  (DEFINE MEMORY CONFIGURATION)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 040B = 0x20 = 32 dec.  Thunk 146450 (SAA 40; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  yaml params: <start page> <no. of memory parts> <part array>.
; ============================================================================
;  Call site 135361  in standalone ENTER-routine 134731 (framesize 000113 = 75 dec).
;  Verified: 135361 JPL I 3 -> ptr 135364, bank1[135364]=146450 = SAA 40.

135333  146135  	RADD CLD SB DA		; A := B
135334  172606  	AAA -172		; A := B-172
135335  054602  	LDX ,B -176		; X := stack top
135336  006006  	STA ,X 6		; param1 := &(B-172) = <start page>
135337  146135  	RADD CLD SB DA
135340  172623  	AAA -155		; A := B-155
135341  006007  	STA ,X 7		; param2 := &(B-155) = <no. of memory parts>
135342  170416  	SAA 16			; build <part array> descriptor
135343  144151  	SWAP CLD SA DD
135344  050625  	LDT ,B -153
135345  032010  	STF ,X 10		; param3 := F = <part array> (3 words)
135346  124013  	JMP 13			; jump over pool -> 135361
   ; ---- pointer pool (data) 135347-135360 ----
135347  002003  	<ptr> 002003
135364  146450  	<ptr> 146450		; -> thunk DEFM (SAA 40)
135365  177335  	<ptr> 177335		; -> LEAVE-SKIP
   ; ---- the call ----
135361  135003  	JPL I 3			; -> ptr 135364 = thunk 146450  MON60 DEFM
135362  134341  	JPL -37			; callsite+1 ERROR   -> 135323 (local error code)
135363  135002  	JPL I 2			; callsite+2 SUCCESS -> ptr 135365 = 177335 LEAVE-SKIP
