; ============================================================================
;  USYST  ->  MON 60 subfunction 075B = 0x3D = 61 dec
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  Subfn purpose (authoritative, 5P-P2-MON60.NPL): CHECK IF CURRENT USER IS USER SYSTEM
;  Server handler (5IFUNC dispatch): ITSTUSER
;
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway 146244 (MON 60
;  at 146256). Thunk 146605: SAA 75 / JMP I 1 / 146244 (verified below).
;  Call idiom (PROVEN, prog.md sec 4/5): LDX ,B -176 sets X = gateway frame top;
;  STx ,X 6/7/10/11 stores MON 60 params 1/2/3/4; JPL I <disp> -> pointer word
;  holding the thunk address. Return polarity (PROVEN, prog.md sec 5.4):
;  callsite+1 = ERROR (direct), callsite+2 = SUCCESS (skip).
;  Annotation "; EA=.. [..]=.." resolves each JPL/JMP I pointer word from bytes.
; ============================================================================

; ---- thunk 146605 (verified from bytes) ----
146605  170475  SAA 75
146606  125001  JMP I 1		; EA=146607 [146607]=146244
146607  146244  RADD CM1 SL DL

; ---- call site 006325 in main interpreter routine 002662 (framesize 000331) - no input params ----
006322  135030  JPL I 30		; EA=006352 [006352]=123577
006323  135205  JPL I -173		; EA=006130 [006130]=002673
006324  125017  JMP I 17		; EA=006343 [006343]=010613
006325  135026  JPL I 26		; EA=006353 [006353]=146605  thunk 146605=SAA 75 -> MON60
006326  135202  JPL I -176		; EA=006130 [006130]=002673
006327  024025  LDD 25
006330  020505  STD ,B 105

; ---- call site 006411 in main interpreter routine 002662 - no input params ----
006406  135147  JPL I 147		; EA=006555 [006555]=146632  thunk 146632=SAA 105 -> MON60
006407  135145  JPL I 145		; EA=006554 [006554]=002673
006410  125333  JMP I -45		; EA=006343 [006343]=010613
006411  135342  JPL I -36		; EA=006353 [006353]=146605  thunk 146605=SAA 75 -> MON60
006412  135142  JPL I 142		; EA=006554 [006554]=002673
006413  024143  LDD 143
006414  020505  STD ,B 105

; ---- call site 006447 in main interpreter routine 002662 - no input params ----
006444  135111  JPL I 111		; EA=006555 [006555]=146632  thunk 146632=SAA 105 -> MON60
006445  135107  JPL I 107		; EA=006554 [006554]=002673
006446  125275  JMP I -103		; EA=006343 [006343]=010613
006447  135304  JPL I -74		; EA=006353 [006353]=146605  thunk 146605=SAA 75 -> MON60
006450  135104  JPL I 104		; EA=006554 [006554]=002673
006451  024111  LDD 111
006452  020505  STD ,B 105

; ---- call site 073461 in routine 073412 (framesize 000223) - no input params ----
073457  044045  LDA 45
073460  135045  JPL I 45		; EA=073525 [073525]=177327
073461  135050  JPL I 50		; EA=073531 [073531]=146605  thunk 146605=SAA 75 -> MON60
073462  135043  JPL I 43		; EA=073525 [073525]=177327
073463  146135  RADD CLD SB DA
073464  172612  AAA -166
