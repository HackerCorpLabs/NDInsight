; ============================================================================
;  012B-RUNN  ->  MON 60 subfunction RUNN = 12B (0x0A = 10)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, BANK 1, base 0.  MON 60 gateway 146244 (MON 60 146256).
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
;  Thunk (verified): 146346 SAA 12 ; 146347 JMP I 1 ; 146350 = 146244 (gateway).
;  Both call sites in standalone ENTER-routine 030515 (framesize 000011).
;  This is the end-to-end verification example from prog.md section 5.5.
; ============================================================================

; ---- Call site 030635 : routine 030515 -------------------------------------
030624  146135  	RADD CLD SB DA		; A := B
030625  172611  	AAA -167		; A := &(B-167)
030626  054602  	LDX ,B -176		; X := gateway frame base
030627  006006  	STA ,X 6		; param1 := &(B-167)  [<stop reason>]
030630  044607  	LDA ,B -171		; A := local(B-171)
030631  006007  	STA ,X 7		; param2 := local(B-171)
030632  146135  	RADD CLD SB DA
030633  172613  	AAA -165
030634  006010  	STA ,X 10		; param3 := &(B-165)  [<returned trap info>]
030635  135110  	JPL I 110		; -> 030745  MON60 RUNN (12B) thunk=146346
030636  134263  	JPL -115		; -> 030521  callsite+1 = ERROR (local block 146147)
030637  024611  	LDD ,B -167		; callsite+2 = SUCCESS: read <stop reason>
030745  146346  	<thunk RUNN (SAA 12)>	; bank1[030745] = 146346

; ---- Call site 030737 : routine 030515 (params set at 030673, JMP 33 here) --
030673  146135  	RADD CLD SB DA
030674  172611  	AAA -167
030675  054602  	LDX ,B -176
030676  006006  	STA ,X 6		; param1 := &(B-167)  [<stop reason>]
030677  044607  	LDA ,B -171
030700  006007  	STA ,X 7		; param2 := local(B-171)
030701  146135  	RADD CLD SB DA
030702  172613  	AAA -165
030703  006010  	STA ,X 10		; param3 := &(B-165)  [<returned trap info>]
030704  124033  	JMP 33			; -> 030737
030737  135375  	JPL I -3		; -> 030734  MON60 RUNN (12B) thunk=146346
030740  135006  	JPL I 6			; -> 030746  callsite+1 = ERROR (ptr=030521)
030741  024611  	LDD ,B -167		; callsite+2 = SUCCESS: read <stop reason>
030734  146346  	<thunk RUNN (SAA 12)>	; bank1[030734] = 146346
