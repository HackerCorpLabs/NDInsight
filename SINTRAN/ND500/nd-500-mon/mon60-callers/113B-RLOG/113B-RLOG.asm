; ============================================================================
;  RLOG  ->  MON 60 subfunction 113B = 0x4B = 75 dec
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  Subfn purpose (authoritative, 5P-P2-MON60.NPL): READ LOG DATA (PRINT LOG INFO)
;  Server handler (5IFUNC dispatch): IPRILOG
;
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway 146244 (MON 60
;  at 146256). Thunk 146646: SAA 113 / JMP I 1 / 146244 (verified below).
;  Call idiom (PROVEN, prog.md sec 4/5): LDX ,B -176 sets X = gateway frame top;
;  STx ,X 6/7/10/11 stores MON 60 params 1/2/3/4; JPL I <disp> -> pointer word
;  holding the thunk address. Return polarity (PROVEN, prog.md sec 5.4):
;  callsite+1 = ERROR (direct), callsite+2 = SUCCESS (skip).
;  Annotation "; EA=.. [..]=.." resolves each JPL/JMP I pointer word from bytes.
; ============================================================================

; ---- thunk 146646 (verified from bytes) ----
146646  170513  SAA 113
146647  125001  JMP I 1		; EA=146650 [146650]=146244
146650  146244  RADD CM1 SL DL

; ---- call site 110116 in routine 110055 (framesize 001137): 2-param (,X6 value + ,X7 F-descriptor) ----
110107  044144  LDA 144
110110  054602  LDX ,B -176
110111  006006  STA ,X 6
110112  044142  LDA 142
110113  144151  SWAP CLD SA DD
110114  050642  LDT ,B -136
110115  032007  STF ,X 7
110116  135137  JPL I 137		; EA=110255 [110255]=146646  thunk 146646=SAA 113 -> MON60
110117  134351  JPL -27
110120  000644  STZ ,B -134
110121  135135  JPL I 135		; EA=110256 [110256]=106633

; ---- call site 110243 in routine 110055: 2-param ----
110234  044040  LDA 40
110235  054602  LDX ,B -176
110236  006006  STA ,X 6
110237  044015  LDA 15
110240  144151  SWAP CLD SA DD
110241  050642  LDT ,B -136
110242  032007  STF ,X 7
110243  135066  JPL I 66		; EA=110331 [110331]=146646  thunk 146646=SAA 113 -> MON60
110244  134224  JPL -154
110245  170777  SAA -1
110246  004644  STA ,B -134

; ---- call site 110310 in routine 110055: 2-param ----
110301  044373  LDA -5
110302  054602  LDX ,B -176
110303  006006  STA ,X 6
110304  044350  LDA -30
110305  144151  SWAP CLD SA DD
110306  050642  LDT ,B -136
110307  032007  STF ,X 7
110310  135345  JPL I -33		; EA=110255 [110255]=146646  thunk 146646=SAA 113 -> MON60
110311  135016  JPL I 16		; EA=110327 [110327]=110070
110312  044661  LDA ,B -117
110313  004644  STA ,B -134
