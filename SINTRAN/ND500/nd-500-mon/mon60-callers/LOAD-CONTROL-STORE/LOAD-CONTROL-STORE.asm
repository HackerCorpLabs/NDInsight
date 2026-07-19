; ============================================================================
;  LOAD-CONTROL-STORE  ->  MON 60 subfunction LDCS = 37B (0x1F)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.  Annotations below are produced mechanically by
;  resolving each pointer word against the verified thunk table.
;  Convention (proven, prog.md sec 4/5): callsite+1 = ERROR (direct return),
;  callsite+2 = SUCCESS (skip return).
; ============================================================================
;  Command case inside the main command interpreter ENTER-routine at 002662
;  (framesize 000331).  Case start 006064, ends 006116 (JMP to command loop).
;  MON 60 issued: LDCS (37B) at 006114.  No file I/O is done here: the file
;  name is merely passed to SINTRAN, whose ICSLOAD handler reads the file.
;  Error 006115 -> internal error routine 002673; Success 006116 -> loop 010613.
;  Helper 002003 = command-line numeric-argument evaluator (own ENTER routine).

; ---- LOAD-CONTROL-STORE command case + local pointer pool (006064-006147) ----
006064  170401  	SAA 1
006065  135047  	JPL I 47		; -> 006134  ptr[006134]=002003 -> routine 002003
006066  135214  	JPL I -164		; -> 005702  ptr[005702]=002673 -> routine 002673
006067  020505  	STD ,B 105
006070  146135  	RADD CLD SB DA
006071  172505  	AAA 105
006072  054602  	LDX ,B -176
006073  006006  	STA ,X 6
006074  146175  	RADD CLD SX DA
006075  172407  	AAA 7
006076  004602  	STA ,B -176
006077  170402  	SAA 2
006100  135034  	JPL I 34		; -> 006134  ptr[006134]=002003 -> routine 002003
006101  135201  	JPL I -177		; -> 005702  ptr[005702]=002673 -> routine 002673
006102  020507  	STD ,B 107
006103  044602  	LDA ,B -176
006104  172771  	AAA -7
006105  004602  	STA ,B -176
006106  146135  	RADD CLD SB DA
006107  172507  	AAA 107
006110  054602  	LDX ,B -176
006111  006007  	STA ,X 7
006112  034665  	LDF ,B -113
006113  032010  	STF ,X 10
006114  135032  	JPL I 32		; -> 006146  MON60 LDCS (37B) thunk=146445
006115  135013  	JPL I 13		; -> 006130  ptr[006130]=002673 -> routine 002673
006116  125213  	JMP I -165		; -> 005731  ptr[005731]=010613 -> routine 010613
006117  034665  	LDF ,B -113
006120  054602  	LDX ,B -176
006121  032006  	STF ,X 6
006122  146175  	RADD CLD SX DA
006123  172411  	AAA 11
006124  004602  	STA ,B -176
006125  170401  	SAA 1
006126  124021  	JMP 21		; -> 006147
006127  146775  	RADD AD1 CM1 CLD SX DA
006130  002673  	STZ ,X ,B -105
006131  146456  	RADD AD1 SA DT
006132  147030  	RADD ADC SB 0
006133  001757  	STZ I ,B -21
006134  002003  	STZ ,X 3
006135  140763  	USER2
006136  141305  	USER4
006137  141317  	USER4
006140  143054  	SKP IF DL LST SA
006141  143042  	SKP IF DP LST SL
006142  140575  	USER1
006143  141331  	USER4
006144  142222  	LBYT
006145  142552  	USER9
006146  146445  	RADD AD1 SL DA
006147  135365  	JPL I -13		; -> 006134  ptr[006134]=002003 -> routine 002003
