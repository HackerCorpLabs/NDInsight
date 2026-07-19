; ============================================================================
;  053B-T500P  ->  MON 60 subfunction T500P = 53B (0x2B = 43 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146522  170453  SAA 53      (subfunction 53B)
;    146523  125001  JMP I 1     (-> gateway pointer)
;    146524  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): (take ND-500 pages)
;  NPL server handler: 5NOPAR
; ============================================================================


; ---- TAKE-N500-PAGES, MON 60 T500P (53B) at 010245 ----
; enclosing ENTER routine 002662 (framesize 000331) - the command interpreter
; 010235 SAA 0 ; 010236 JPL I 41 -> routine 002003 (numeric-arg evaluator) -> D
; 010237 JPL I -174 -> 007500 if the evaluator fails
; 010240 STD ,B 105 ; 010241 RADD SB DA ; 010242 AAA 105 ; 010244 STA ,X 6 = &(B+105)
; 010245 JPL I 43 -> ptr 010310 = thunk 146522 = SAA 53
010235  170400  SAA 0
010236  135041  JPL I 41		; -> 010277
010237  135204  JPL I -174		; -> 010043
010240  020505  STD ,B 105
010241  146135  RADD CLD SB DA
010242  172505  AAA 105
010243  054602  LDX ,B -176
010244  006006  STA ,X 6
010245  135043  JPL I 43		; -> 010310
010246  135010  JPL I 10		; -> 010256
010247  125202  JMP I -176		; -> 010051
