; ============================================================================
;  SPFLAG  ->  MON 60 subfunction 101B = 0x41 = 65 dec
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  Subfn purpose (authoritative, 5P-P2-MON60.NPL): WRITE FLAGS INTO ND-500 DATA SEGMENT
;  Server handler (5IFUNC dispatch): WWFLAG
;
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway 146244 (MON 60
;  at 146256). Thunk 146621: SAA 101 / JMP I 1 / 146244 (verified below).
;  Call idiom (PROVEN, prog.md sec 4/5): LDX ,B -176 sets X = gateway frame top;
;  STx ,X 6/7/10/11 stores MON 60 params 1/2/3/4; JPL I <disp> -> pointer word
;  holding the thunk address. Return polarity (PROVEN, prog.md sec 5.4):
;  callsite+1 = ERROR (direct), callsite+2 = SUCCESS (skip).
;  Annotation "; EA=.. [..]=.." resolves each JPL/JMP I pointer word from bytes.
; ============================================================================

; ---- thunk 146621 (verified from bytes) ----
146621  170501  SAA 101
146622  125001  JMP I 1		; EA=146623 [146623]=146244
146623  146244  RADD CM1 SL DL

; ---- call site 005223 in main interpreter routine 002662 (framesize 000331): 2-param ----
005204  170400  SAA 0
005205  135214  JPL I -164		; EA=005021 [005021]=002003
005206  135211  JPL I -167		; EA=005017 [005017]=002673
005207  020653  STD ,B -125
005210  170401  SAA 1
005211  135210  JPL I -170		; EA=005021 [005021]=002003
005212  135205  JPL I -173		; EA=005017 [005017]=002673
005213  020651  STD ,B -127
005214  146135  RADD CLD SB DA
005215  172653  AAA -125
005216  054602  LDX ,B -176
005217  006006  STA ,X 6
005220  146135  RADD CLD SB DA
005221  172651  AAA -127
005222  006007  STA ,X 7
005223  135024  JPL I 24		; EA=005247 [005247]=146621  thunk 146621=SAA 101 -> MON60
005224  135004  JPL I 4		; EA=005230 [005230]=002673
005225  125005  JMP I 5		; EA=005232 [005232]=010613
005226  170400  SAA 0
