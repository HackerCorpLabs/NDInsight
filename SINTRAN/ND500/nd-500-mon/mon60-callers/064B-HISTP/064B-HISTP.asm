; ============================================================================
;  064B-HISTP  ->  MON 60 subfunction HISTP = 64B (0x34 = 52 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146552  170464  SAA 64      (subfunction 64B)
;    146553  125001  JMP I 1     (-> gateway pointer)
;    146554  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): STOP HISTOGRAM
;  NPL server handler: ISTOHIAT
; ============================================================================


; ---- STOP-HISTOGRAM, MON 60 HISTP (64B) at 010435 ----
; enclosing ENTER routine 002662 (framesize 000331) - the command interpreter
; No ,X parameter store immediately precedes 010435 (bare subfunction call)
; 010435 JPL I 66 -> ptr 010523 = thunk 146552 = SAA 64
010432  135070  JPL I 70		; -> 010522
010433  135223  JPL I -155		; -> 010256
010434  124157  JMP 157		; -> 010613
010435  135066  JPL I 66		; -> 010523
010436  135220  JPL I -160		; -> 010256
010437  124154  JMP 154		; -> 010613
