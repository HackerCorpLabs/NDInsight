; ============================================================================
;  SLOGA  ->  MON 60 subfunction 115B = 0x4D = 77 dec
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  Subfn purpose (authoritative, 5P-P2-MON60.NPL): START PROCESS-LOG-ALL
;  Server handler (5IFUNC dispatch): ISTLAPR
;
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway 146244 (MON 60
;  at 146256). Thunk 146654: SAA 115 / JMP I 1 / 146244 (verified below).
;  Call idiom (PROVEN, prog.md sec 4/5): LDX ,B -176 sets X = gateway frame top;
;  STx ,X 6/7/10/11 stores MON 60 params 1/2/3/4; JPL I <disp> -> pointer word
;  holding the thunk address. Return polarity (PROVEN, prog.md sec 5.4):
;  callsite+1 = ERROR (direct), callsite+2 = SUCCESS (skip).
;  Annotation "; EA=.. [..]=.." resolves each JPL/JMP I pointer word from bytes.
; ============================================================================

; ---- thunk 146654 (verified from bytes) ----
146654  170515  SAA 115
146655  125001  JMP I 1		; EA=146656 [146656]=146244
146656  146244  RADD CM1 SL DL

; ---- call site 110143 in routine 110055 (framesize 001137) - no input params ----
110140  171005  SAT 5
110141  140065  SKP IF DA EQL ST
110142  124013  JMP 13
110143  135120  JPL I 120		; EA=110263 [110263]=146654  thunk 146654=SAA 115 -> MON60
110144  134324  JPL -54
110145  170424  SAA 24
110146  005116  STA I 116
