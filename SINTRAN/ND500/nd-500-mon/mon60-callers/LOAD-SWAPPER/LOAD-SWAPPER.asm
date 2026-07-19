; ============================================================================
;  LOAD-SWAPPER  ->  MON 60 subfunction SWLOD = 7B (0x07)
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
;  SWLOD case = 010211..010216.  MON 60 issued: SWLOD (7B) at 010214.
;  NPL purpose of 7B: PLACE SWAPPER.  Parameter: swapper segment name (F reg,
;  3 words) at B-113.  Error 010215 -> routine 007500; Success 010216 -> 010613.
;  The immediately following case (010217) is a SEPARATE command, START-SWAPPER
;  (STSWP 54B); shown here for context only.  Pool 010305 holds the SWLOD thunk.

; ---- LOAD-SWAPPER case (010211-010216) + adjacent STSWP case (010211-010221) ----
010211  034665  	LDF ,B -113
010212  054602  	LDX ,B -176
010213  032006  	STF ,X 6
010214  135071  	JPL I 71		; -> 010305  MON60 SWLOD (7B) thunk=146340
010215  135226  	JPL I -152		; -> 010043  ptr[010043]=007500 -> routine 007500
010216  125233  	JMP I -145		; -> 010051  ptr[010051]=010613 -> routine 010613
010217  135067  	JPL I 67		; -> 010306  MON60 STSWP (54B) thunk=146525
010220  135223  	JPL I -155		; -> 010043  ptr[010043]=007500 -> routine 007500
010221  125230  	JMP I -150		; -> 010051  ptr[010051]=010613 -> routine 010613

; ---- shared local pointer pool (010305=SWLOD thunk, 010306=STSWP thunk) (010304-010311) ----
010304  103722  	FAD I ,B ,X -56
010305  146340  	RADD CM1 CLD SL 0
010306  146525  	RADD AD1 CLD SP DA
010307  146517  	RADD AD1 CLD SD DX
010310  146522  	RADD AD1 CLD SP DP
010311  002017  	STZ ,X 17
