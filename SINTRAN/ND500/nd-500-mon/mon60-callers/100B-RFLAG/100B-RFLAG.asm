; ============================================================================
;  RFLAG  ->  MON 60 subfunction 100B = 0x40 = 64 dec
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  Subfn purpose (authoritative, 5P-P2-MON60.NPL): READ FLAGS FROM ND-500 DATA SEGMENT
;  Server handler (5IFUNC dispatch): RRFLAG
;
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway 146244 (MON 60
;  at 146256). Thunk 146616: SAA 100 / JMP I 1 / 146244 (verified below).
;  Call idiom (PROVEN, prog.md sec 4/5): LDX ,B -176 sets X = gateway frame top;
;  STx ,X 6/7/10/11 stores MON 60 params 1/2/3/4; JPL I <disp> -> pointer word
;  holding the thunk address. Return polarity (PROVEN, prog.md sec 5.4):
;  callsite+1 = ERROR (direct), callsite+2 = SUCCESS (skip).
;  Annotation "; EA=.. [..]=.." resolves each JPL/JMP I pointer word from bytes.
; ============================================================================

; ---- thunk 146616 (verified from bytes) ----
146616  170500  SAA 100
146617  125001  JMP I 1		; EA=146620 [146620]=146244
146620  146244  RADD CM1 SL DL

; ---- call site 005264 in main interpreter routine 002662 (framesize 000331): 2-param ----
005253  024174  LDD 174
005254  020651  STD ,B -127
005255  146135  RADD CLD SB DA
005256  172653  AAA -125
005257  054602  LDX ,B -176
005260  006006  STA ,X 6
005261  146135  RADD CLD SB DA
005262  172651  AAA -127
005263  006007  STA ,X 7
005264  135165  JPL I 165		; EA=005451 [005451]=146616  thunk 146616=SAA 100 -> MON60
005265  135343  JPL I -35		; EA=005230 [005230]=002673
005266  024651  LDD ,B -127
005267  054602  LDX ,B -176
