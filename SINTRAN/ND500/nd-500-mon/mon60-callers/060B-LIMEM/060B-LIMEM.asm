; ============================================================================
;  060B-LIMEM  ->  MON 60 subfunction LIMEM = 60B (0x30 = 48 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146541  170460  SAA 60      (subfunction 60B)
;    146542  125001  JMP I 1     (-> gateway pointer)
;    146543  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): LIST MEMORY CONFIGURATION
;  NPL server handler: 5NOPAR
; ============================================================================


; ---- MEMORY-CONFIGURATION, MON 60 LIMEM (60B) at 135532 ----
; enclosing ENTER routine 135502 (framesize 000060) - a standalone ENTER routine
; 135524 SAA -1 ; 135525 STA ,B -127 (B-127 := -1)
; 135526 RADD SB DA ; 135527 AAA -164 ; 135530 LDX ,B -176 ; 135531 STA ,X 6 = &(B-164)
; 135532 JPL I 144 -> ptr 135676 = thunk 146541 = SAA 60
135524  170777  SAA -1
135525  004651  STA ,B -127
135526  146135  RADD CLD SB DA
135527  172614  AAA -164
135530  054602  LDX ,B -176
135531  006006  STA ,X 6
135532  135144  JPL I 144		; -> 135676
135533  135144  JPL I 144		; -> 135677
135534  146135  RADD CLD SB DA
