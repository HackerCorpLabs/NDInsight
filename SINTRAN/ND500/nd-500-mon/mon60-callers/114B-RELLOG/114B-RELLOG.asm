; ============================================================================
;  RELLOG  ->  MON 60 subfunction 114B = 0x4C = 76 dec
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  Subfn purpose (authoritative, 5P-P2-MON60.NPL): STOP LOGGING AND RELEASE LOGGING FACILITY
;  Server handler (5IFUNC dispatch): IRELLOG
;
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway 146244 (MON 60
;  at 146256). Thunk 146651: SAA 114 / JMP I 1 / 146244 (verified below).
;  Call idiom (PROVEN, prog.md sec 4/5): LDX ,B -176 sets X = gateway frame top;
;  STx ,X 6/7/10/11 stores MON 60 params 1/2/3/4; JPL I <disp> -> pointer word
;  holding the thunk address. Return polarity (PROVEN, prog.md sec 5.4):
;  callsite+1 = ERROR (direct), callsite+2 = SUCCESS (skip).
;  Annotation "; EA=.. [..]=.." resolves each JPL/JMP I pointer word from bytes.
; ============================================================================

; ---- thunk 146651 (verified from bytes) ----
146651  170514  SAA 114
146652  125001  JMP I 1		; EA=146653 [146653]=146244
146653  146244  RADD CM1 SL DL

; ---- call site 006676 in main interpreter routine 002662 (framesize 000331) - no input params ----
006673  135072  JPL I 72		; EA=006765 [006765]=110055
006674  135260  JPL I -120		; EA=006554 [006554]=002673
006675  125071  JMP I 71		; EA=006766 [006766]=010613
006676  135071  JPL I 71		; EA=006767 [006767]=146651  thunk 146651=SAA 114 -> MON60
006677  135255  JPL I -123		; EA=006554 [006554]=002673
006700  125066  JMP I 66		; EA=006766 [006766]=010613
006701  170400  SAA 0
