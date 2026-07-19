; ============================================================================
;  057B-MPVER  ->  MON 60 subfunction MPVER = 57B (0x2F = 47 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146536  170457  SAA 57      (subfunction 57B)
;    146537  125001  JMP I 1     (-> gateway pointer)
;    146540  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): READ MICRO PROGRAM VERSION
;  NPL server handler: 5NOPAR
; ============================================================================


; ---- MPVER call site 1 at 005577 ----
; enclosing ENTER routine 002662 (framesize 000331) - the command interpreter
; 005573 RADD SB DA ; 005574 AAA -127 ; 005575 LDX ,B -176 ; 005576 STA ,X 6 = &(B-127)
; 005577 JPL I 112 -> ptr 005711 = thunk 146536 = SAA 57
005573  146135  RADD CLD SB DA
005574  172651  AAA -127
005575  054602  LDX ,B -176
005576  006006  STA ,X 6
005577  135112  JPL I 112		; -> 005711
005600  135246  JPL I -132		; -> 005446
005601  124022  JMP 22		; -> 005623

; ---- MPVER call site 2 at 132132 ----
; enclosing ENTER routine 132124 (framesize 000000) - a small standalone ENTER routine
; 132127 LDA 24 ; 132130 LDX ,B -176 ; 132131 STA ,X 6 = value from LDA 24
; 132132 JPL I 22 -> ptr 132154 = thunk 146536 = SAA 57
132127  044024  LDA 24
132130  054602  LDX ,B -176
132131  006006  STA ,X 6
132132  135022  JPL I 22		; -> 132154
132133  135022  JPL I 22		; -> 132155
132134  170426  SAA 26
