; ============================================================================
;  LNKPR  ->  MON 60 subfunction 106B = 0x46 = 70 dec
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  Subfn purpose (authoritative, 5P-P2-MON60.NPL): (link to process)
;  Server handler (5IFUNC dispatch): 5NOPAR
;
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway 146244 (MON 60
;  at 146256). Thunk 146635: SAA 106 / JMP I 1 / 146244 (verified below).
;  Call idiom (PROVEN, prog.md sec 4/5): LDX ,B -176 sets X = gateway frame top;
;  STx ,X 6/7/10/11 stores MON 60 params 1/2/3/4; JPL I <disp> -> pointer word
;  holding the thunk address. Return polarity (PROVEN, prog.md sec 5.4):
;  callsite+1 = ERROR (direct), callsite+2 = SUCCESS (skip).
;  Annotation "; EA=.. [..]=.." resolves each JPL/JMP I pointer word from bytes.
; ============================================================================

; ---- thunk 146635 (verified from bytes) ----
146635  170506  SAA 106
146636  125001  JMP I 1		; EA=146637 [146637]=146244
146637  146244  RADD CM1 SL DL

; ---- call site 006711 in main interpreter routine 002662 (framesize 000331): 1-param ----
006701  170400  SAA 0
006702  135251  JPL I -127		; EA=006553 [006553]=002003
006703  135251  JPL I -127		; EA=006554 [006554]=002673
006704  020661  STD ,B -117
006705  146135  RADD CLD SB DA
006706  172661  AAA -117
006707  054602  LDX ,B -176
006710  006006  STA ,X 6
006711  135057  JPL I 57		; EA=006770 [006770]=146635  thunk 146635=SAA 106 -> MON60
006712  135242  JPL I -136		; EA=006554 [006554]=002673
006713  001056  STZ I 56
006714  125052  JMP I 52		; EA=006766 [006766]=010613
