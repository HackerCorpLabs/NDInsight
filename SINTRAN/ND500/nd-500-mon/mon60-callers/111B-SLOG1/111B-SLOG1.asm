; ============================================================================
;  SLOG1  ->  MON 60 subfunction 111B = 0x49 = 73 dec
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  Subfn purpose (authoritative, 5P-P2-MON60.NPL): START PROCESS LOG ONE
;  Server handler (5IFUNC dispatch): ISTAPRLOG
;
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway 146244 (MON 60
;  at 146256). Thunk 146643: SAA 111 / JMP I 1 / 146244 (verified below).
;  Call idiom (PROVEN, prog.md sec 4/5): LDX ,B -176 sets X = gateway frame top;
;  STx ,X 6/7/10/11 stores MON 60 params 1/2/3/4; JPL I <disp> -> pointer word
;  holding the thunk address. Return polarity (PROVEN, prog.md sec 5.4):
;  callsite+1 = ERROR (direct), callsite+2 = SUCCESS (skip).
;  Annotation "; EA=.. [..]=.." resolves each JPL/JMP I pointer word from bytes.
; ============================================================================

; ---- thunk 146643 (verified from bytes) ----
146643  170511  SAA 111
146644  125001  JMP I 1		; EA=146645 [146645]=146244
146645  146244  RADD CM1 SL DL

; ---- call site 110161 in routine 110055 (framesize 001137): 1-param ----
110155  146135  RADD CLD SB DA
110156  172607  AAA -171
110157  054602  LDX ,B -176
110160  006006  STA ,X 6
110161  135105  JPL I 105		; EA=110266 [110266]=146643  thunk 146643=SAA 111 -> MON60
110162  134306  JPL -72
110163  170415  SAA 15
110164  005100  STA I 100
