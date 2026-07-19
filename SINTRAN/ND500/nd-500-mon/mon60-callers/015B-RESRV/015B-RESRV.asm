; ============================================================================
;  015B-RESRV  ->  MON 60 subfunction RESRV = 15B (0x0D = 13)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146357 SAA 15 ; 146360 JMP I 1 ; 146361 = 146244 (gateway).
;  Two call sites: 010333 (interpreter case) and 011142 (standalone routine 011043).
; ============================================================================

; ---- Call site 010333 : CASE inside command interpreter 002662 (002662-010634)
010324  044171  	LDA 171			; A := const [010515]  (INFERRED: a mode/type value)
010325  054602  	LDX ,B -176		; X := gateway frame base
010326  006006  	STA ,X 6		; param1 := that const
010327  170412  	SAA 12			; A := 012
010330  144151  	SWAP CLD SA DD		; D := 012
010331  050664  	LDT ,B -114		; T := local(B-114)
010332  032007  	STF ,X 7		; param2 := F register (3-word: T,A,D=012)
010333  135163  	JPL I 163		; -> 010516  MON60 RESRV (15B) thunk=146357
010334  135322  	JPL I -56		; -> 010256  callsite+1 = ERROR (ptr[010256]=007500)
; (010335 = callsite+2 = SUCCESS)
010516  146357  	<thunk RESRV (SAA 15)>	; bank1[010516] = 146357

; ---- Call site 011142 : standalone ENTER-routine 011043 (framesize 000236) --
011132  000607  	STZ ,B -171
011133  044121  	LDA 121			; A := const [011254]
011134  054602  	LDX ,B -176
011135  006006  	STA ,X 6		; param1 := that const
011136  170412  	SAA 12			; A := 012
011137  144151  	SWAP CLD SA DD		; D := 012
011140  050606  	LDT ,B -172		; T := local(B-172)
011141  032007  	STF ,X 7		; param2 := F register (3-word: T,A,D=012)
011142  135113  	JPL I 113		; -> 011255  MON60 RESRV (15B) thunk=146357
011143  134351  	JPL -27			; -> 011114  callsite+1 = ERROR (local block 146147)
; (011144 = callsite+2 = SUCCESS)
011255  146357  	<thunk RESRV (SAA 15)>	; bank1[011255] = 146357
