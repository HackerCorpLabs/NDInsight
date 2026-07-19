; ============================================================================
;  050B-TESTF  ->  MON 60 subfunction TESTF = 50B (0x28 = 40 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146511  170450  SAA 50      (subfunction 50B)
;    146512  125001  JMP I 1     (-> gateway pointer)
;    146513  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): (test function)
;  NPL server handler: 5NOPAR
; ============================================================================


; ---- TESTF call site 1 at 007740 ----
; enclosing ENTER routine 002662 (framesize 000331) - the command interpreter
; 007733 LDX ,B -176 ; 007734 STA ,X 6 = param 1
; 007735 RADD SB DA ; 007736 AAA -127 ; 007737 STA ,X 7 = param 2 = &(B-127)
; 007740 JPL I 106 -> ptr 010046 = thunk 146511 = SAA 50
007725  024613  LDD ,B -165
007726  020651  STD ,B -127
007727  054611  LDX ,B -167
007730  146077  RADD SX DX
007731  044114  LDA 114
007732  146075  RADD SX DA
007733  054602  LDX ,B -176
007734  006006  STA ,X 6
007735  146135  RADD CLD SB DA
007736  172651  AAA -127
007737  006007  STA ,X 7
007740  135106  JPL I 106		; -> 010046
007741  135102  JPL I 102		; -> 010043
007742  124006  JMP 6		; -> 007750

; ---- TESTF call site 2 at 007762 ----
; enclosing ENTER routine 002662 (framesize 000331) - the command interpreter
; 007753 RADD SB DA ; 007754 AAA -127 ; 007756 STA ,X 6 = &(B-127)
; 007757 RADD SB DA ; 007760 AAA -127 ; 007761 STA ,X 7 = &(B-127)
; 007762 JPL I 64 -> ptr 010046 = thunk 146511 = SAA 50
007751  024101  LDD 101
007752  020651  STD ,B -127
007753  146135  RADD CLD SB DA
007754  172651  AAA -127
007755  054602  LDX ,B -176
007756  006006  STA ,X 6
007757  146135  RADD CLD SB DA
007760  172651  AAA -127
007761  006007  STA ,X 7
007762  135064  JPL I 64		; -> 010046
007763  135060  JPL I 60		; -> 010043
007764  125065  JMP I 65		; -> 010051
