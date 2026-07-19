; ============================================================================
;  REACS  ->  MON 60 subfunction 023B  (READ CONTROL STORE)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
;  Subfunction code 023B = 0x13 = 19 dec.  Thunk 146407 (SAA 23; JMP I 1; 146244).
;  Convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  yaml params: <CS addr.> <no of 16 bit words> <data-area>.
;  TWO call sites, in two different standalone ENTER routines.
; ============================================================================

; ---------------------------------------------------------------------------
;  Call site A : 123556  in ENTER-routine 123515 (framesize 000010 = 8 dec)
; ---------------------------------------------------------------------------
123545  146135  	RADD CLD SB DA		; A := B
123546  172614  	AAA -164		; A := B-164
123547  054602  	LDX ,B -176		; X := stack top
123550  006006  	STA ,X 6		; param1 := &(B-164) = <CS addr.>
123551  146135  	RADD CLD SB DA
123552  172612  	AAA -166		; A := B-166
123553  006007  	STA ,X 7		; param2 := &(B-166) = <no of 16 bit words>
123554  034607  	LDF ,B -171		; F := descriptor at B-171
123555  032010  	STF ,X 10		; param3 := F = <data-area> (3 words)
123556  135015  	JPL I 15		; -> ptr 123573 = thunk 146407  MON60 REACS
123557  135015  	JPL I 15		; callsite+1 ERROR   -> ptr 123574 = 177327 LEAVE(value)
123560  045011  	LDA I 11		; callsite+2 SUCCESS (continues)
   ; pool: 123573=146407 (thunk REACS), 123574=177327 (LEAVE-value), 123575=?, 123576=177335

; ---------------------------------------------------------------------------
;  Call site B : 124201  in ENTER-routine 124023 (framesize 001724 = 996 dec)
; ---------------------------------------------------------------------------
124164  146135  	RADD CLD SB DA
124165  172625  	AAA -153		; A := B-153
124166  054602  	LDX ,B -176
124167  006006  	STA ,X 6		; param1 := &(B-153) = <CS addr.>
124170  146135  	RADD CLD SB DA
124171  172627  	AAA -151		; A := B-151
124172  006007  	STA ,X 7		; param2 := &(B-151) = <no of 16 bit words>
124173  044622  	LDA ,B -156
124174  144151  	SWAP CLD SA DD
124175  050624  	LDT ,B -154
124176  030632  	STF ,B -146		; build <data-area> descriptor into B-146
124177  034632  	LDF ,B -146
124200  032010  	STF ,X 10		; param3 := F = <data-area> (3 words)
124201  135031  	JPL I 31		; -> ptr 124232 = thunk 146407  MON60 REACS
124202  135020  	JPL I 20		; callsite+1 ERROR   -> ptr 124222 = 177327 LEAVE(value)
124203  045022  	LDA I 22		; callsite+2 SUCCESS (continues; 124214 JMP -> 124233=177335)
   ; pool: 124222=177327 (LEAVE-value), 124232=146407 (thunk REACS), 124233=177335 (LEAVE-SKIP)
