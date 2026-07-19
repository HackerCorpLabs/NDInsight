; ============================================================================
;  056B-EPLAC  ->  MON 60 subfunction EPLAC = 56B (0x2E = 46 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146533  170456  SAA 56      (subfunction 56B)
;    146534  125001  JMP I 1     (-> gateway pointer)
;    146535  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): END-PLACE
;  NPL server handler: IEPLACE
; ============================================================================


; ---- EPLAC call site 1 at 044062 ----
; enclosing ENTER routine 043011 (framesize 000717) - a standalone ENTER routine
; 044055 SAA 77 ; 044056 SWAP CLD SA DD ; 044060 LDX ,B -176 ; 044061 STF ,X 6 = F (3 words)
; 044062 JPL I 35 -> ptr 044117 = thunk 146533 = SAA 56
044055  170477  SAA 77
044056  144151  SWAP CLD SA DD
044057  050476  LDT ,B 76
044060  054602  LDX ,B -176
044061  032006  STF ,X 6
044062  135035  JPL I 35		; -> 044117
044063  135212  JPL I -166		; -> 043675
044064  124010  JMP 10		; -> 044074

; ---- EPLAC call site 2 at 063342 ----
; enclosing ENTER routine 062257 (framesize 000544) - a standalone ENTER routine
; 063335 SAA 77 ; 063336 SWAP CLD SA DD ; 063340 LDX ,B -176 ; 063341 STF ,X 6 = F (3 words)
; 063342 JPL I 23 -> ptr 063365 = thunk 146533 = SAA 56
063335  170477  SAA 77
063336  144151  SWAP CLD SA DD
063337  050025  LDT 25
063340  054602  LDX ,B -176
063341  032006  STF ,X 6
063342  135023  JPL I 23		; -> 063365
063343  135056  JPL I 56		; -> 063421
063344  044700  LDA ,B -100
