; ============================================================================
;  WSYSP  ->  MON 60 subfunction 104B = 0x44 = 68 dec
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  Subfn purpose (authoritative, 5P-P2-MON60.NPL): WRITE SYSTEM PARAMETERS
;  Server handler (5IFUNC dispatch): IWSYSP
;
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway 146244 (MON 60
;  at 146256). Thunk 146627: SAA 104 / JMP I 1 / 146244 (verified below).
;  Call idiom (PROVEN, prog.md sec 4/5): LDX ,B -176 sets X = gateway frame top;
;  STx ,X 6/7/10/11 stores MON 60 params 1/2/3/4; JPL I <disp> -> pointer word
;  holding the thunk address. Return polarity (PROVEN, prog.md sec 5.4):
;  callsite+1 = ERROR (direct), callsite+2 = SUCCESS (skip).
;  Annotation "; EA=.. [..]=.." resolves each JPL/JMP I pointer word from bytes.
; ============================================================================

; ---- thunk 146627 (verified from bytes) ----
146627  170504  SAA 104
146630  125001  JMP I 1		; EA=146631 [146631]=146244
146631  146244  RADD CM1 SL DL

; ---- call site 073354 in routine 073115 (framesize 000336; the LIST/SET-SYSTEM-PARAMETERS handler; already carved as SET-SYSTEM-PARAMETERS - see LIST-SYSTEM-PARAMETERS/) ----
073347  146135  RADD CLD SB DA
073350  172611  AAA -167
073351  172410  AAA 10
073352  054602  LDX ,B -176
073353  006006  STA ,X 6
073354  135026  JPL I 26		; EA=073402 [073402]=146627  thunk 146627=SAA 104 -> MON60
073355  135336  JPL I -42		; EA=073313 [073313]=177327
073356  146135  RADD CLD SB DA
