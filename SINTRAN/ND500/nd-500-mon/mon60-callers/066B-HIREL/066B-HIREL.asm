; ============================================================================
;  066B-HIREL  ->  MON 60 subfunction HIREL = 66B (0x36 = 54 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146560  170466  SAA 66      (subfunction 66B)
;    146561  125001  JMP I 1     (-> gateway pointer)
;    146562  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): STOP AND RELEASE HISTOGRAM
;  NPL server handler: IRELHIST
; ============================================================================


; ---- HIREL call site 1 at 010443 (interpreter) ----
; enclosing ENTER routine 002662 (framesize 000331) - the command interpreter
; No ,X parameter store immediately precedes 010443 (bare subfunction call)
; 010443 JPL I 62 -> ptr 010525 = thunk 146560 = SAA 66
010443  135062  JPL I 62		; -> 010525
010444  135212  JPL I -166		; -> 010256
010445  124146  JMP 146		; -> 010613

; ---- HIREL call site 2 at 110130 ----
; enclosing ENTER routine 110055 (framesize 001137) - a standalone ENTER routine
; 110125 LDD ,B -171 ; 110126 RADD CLD SD DA ; 110127 STA I 131 (A stored via indirect pointer at P+131 to a global)
; 110130 JPL I 131 -> ptr 110261 = thunk 146560 = SAA 66
; No ,X gateway-block store precedes 110130
110125  024607  LDD ,B -171
110126  146115  RADD CLD SD DA
110127  005131  STA I 131
110130  135131  JPL I 131		; -> 110261
110131  134337  JPL -41		; -> 110070
110132  044606  LDA ,B -172
