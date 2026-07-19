; ============================================================================
;  SPRNM  ->  MON 60 subfunction 074B = 0x3C = 60 dec
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  Subfn purpose (authoritative, 5P-P2-MON60.NPL): SET NAME ON CURRENT PROCESS
;  Server handler (5IFUNC dispatch): ISPRNM
;
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway 146244 (MON 60
;  at 146256). Thunk 146602: SAA 74 / JMP I 1 / 146244 (verified below).
;  Call idiom (PROVEN, prog.md sec 4/5): LDX ,B -176 sets X = gateway frame top;
;  STx ,X 6/7/10/11 stores MON 60 params 1/2/3/4; JPL I <disp> -> pointer word
;  holding the thunk address. Return polarity (PROVEN, prog.md sec 5.4):
;  callsite+1 = ERROR (direct), callsite+2 = SUCCESS (skip).
;  Annotation "; EA=.. [..]=.." resolves each JPL/JMP I pointer word from bytes.
; ============================================================================

; ---- thunk 146602 (verified from bytes) ----
146602  170474  SAA 74
146603  125001  JMP I 1		; EA=146604 [146604]=146244
146604  146244  RADD CM1 SL DL

; ---- call site 010112 in main interpreter routine 002662 (framesize 000331) ----
010107  034665  LDF ,B -113
010110  054602  LDX ,B -176
010111  032006  STF ,X 6
010112  135151  JPL I 151		; EA=010263 [010263]=146602  thunk 146602=SAA 74 -> MON60
010113  135330  JPL I -50		; EA=010043 [010043]=007500
010114  125335  JMP I -43		; EA=010051 [010051]=010613
010115  135147  JPL I 147		; EA=010264 [010264]=002636
