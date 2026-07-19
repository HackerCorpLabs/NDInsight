; ============================================================================
;  GSGTE  ->  MON 60 subfunction 072B = 0x3A = 58 dec
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  Subfn purpose (authoritative, 5P-P2-MON60.NPL): READ A PHYS.SEGMENT TABLE ENTRY FROM SYS.MON
;  Server handler (5IFUNC dispatch): 5NOPAR
;
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway 146244 (MON 60
;  at 146256). Thunk 146574: SAA 72 / JMP I 1 / 146244 (verified below).
;  Call idiom (PROVEN, prog.md sec 4/5): LDX ,B -176 sets X = gateway frame top;
;  STx ,X 6/7/10/11 stores MON 60 params 1/2/3/4; JPL I <disp> -> pointer word
;  holding the thunk address. Return polarity (PROVEN, prog.md sec 5.4):
;  callsite+1 = ERROR (direct), callsite+2 = SUCCESS (skip).
;  Annotation "; EA=.. [..]=.." resolves each JPL/JMP I pointer word from bytes.
; ============================================================================

; ---- thunk 146574 (verified from bytes) ----
146574  170472  SAA 72
146575  125001  JMP I 1		; EA=146576 [146576]=146244
146576  146244  RADD CM1 SL DL

; ---- call site 110447 in routine 110365 (framesize 002250): 2-param call, error 110450 / success 110451 ----
110432  146147  RADD CLD SL DX
110433  014446  STX ,B 46
110434  004605  STA ,B -173
110435  044133  LDA 133
110436  135125  JPL I 125		; EA=110563 [110563]=177327
110437  125446  JMP I ,B 46
110440  146135  RADD CLD SB DA
110441  172434  AAA 34
110442  054602  LDX ,B -176
110443  006006  STA ,X 6
110444  146135  RADD CLD SB DA
110445  172736  AAA -42
110446  006007  STA ,X 7
110447  135122  JPL I 122		; EA=110571 [110571]=146574  thunk 146574=SAA 72 -> MON60
110450  134362  JPL -16
110451  170401  SAA 1
110452  004441  STA ,B 41
