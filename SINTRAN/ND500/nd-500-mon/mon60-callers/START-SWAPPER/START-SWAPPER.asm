; ============================================================================
;  START-SWAPPER  ->  MON 60 subfunction STSWP = 54B (0x2C)
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
;  Command case inside interpreter ENTER-routine 002662 (framesize 000331).
;  STSWP case = 010217..010221 (no parameters).  MON 60: STSWP (54B) at 010217.
;  Adjacent, SEPARATE command cases in the same dispatch region:
;    G500P (52B, GIVE-N500-PAGES)  case 010222..010234, MON 60 at 010232
;    T500P (53B, TAKE-N500-PAGES)  case 010235..010247, MON 60 at 010245
;  These are three distinct operator commands, each reached from the command
;  dispatch, each with its own error/success tails - NOT one handler.
;  Error -> routine 007500 (010043 pool word); Success -> loop 010613.

; ---- START-SWAPPER (STSWP) + adjacent GIVE/TAKE-PAGES cases (010217-010247) ----
010217  135067  	JPL I 67		; -> 010306  MON60 STSWP (54B) thunk=146525
010220  135223  	JPL I -155		; -> 010043  ptr[010043]=007500 -> routine 007500
010221  125230  	JMP I -150		; -> 010051  ptr[010051]=010613 -> routine 010613
010222  170400  	SAA 0
010223  135054  	JPL I 54		; -> 010277  ptr[010277]=002003 -> routine 002003
010224  135217  	JPL I -161		; -> 010043  ptr[010043]=007500 -> routine 007500
010225  020505  	STD ,B 105
010226  146135  	RADD CLD SB DA
010227  172505  	AAA 105
010230  054602  	LDX ,B -176
010231  006006  	STA ,X 6
010232  135055  	JPL I 55		; -> 010307  MON60 G500P (52B) thunk=146517
010233  135210  	JPL I -170		; -> 010043  ptr[010043]=007500 -> routine 007500
010234  125215  	JMP I -163		; -> 010051  ptr[010051]=010613 -> routine 010613
010235  170400  	SAA 0
010236  135041  	JPL I 41		; -> 010277  ptr[010277]=002003 -> routine 002003
010237  135204  	JPL I -174		; -> 010043  ptr[010043]=007500 -> routine 007500
010240  020505  	STD ,B 105
010241  146135  	RADD CLD SB DA
010242  172505  	AAA 105
010243  054602  	LDX ,B -176
010244  006006  	STA ,X 6
010245  135043  	JPL I 43		; -> 010310  MON60 T500P (53B) thunk=146522
010246  135010  	JPL I 10		; -> 010256  ptr[010256]=007500 -> routine 007500
010247  125202  	JMP I -176		; -> 010051  ptr[010051]=010613 -> routine 010613

; ---- shared pointer pool (010306=STSWP,010307=G500P,010310=T500P) (010304-010311) ----
010304  103722  	FAD I ,B ,X -56
010305  146340  	RADD CM1 CLD SL 0
010306  146525  	RADD AD1 CLD SP DA
010307  146517  	RADD AD1 CLD SD DX
010310  146522  	RADD AD1 CLD SP DP
010311  002017  	STZ ,X 17
