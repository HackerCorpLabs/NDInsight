; ============================================================================
;  063B-HISTA  ->  MON 60 subfunction HISTA = 63B (0x33 = 51 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146547  170463  SAA 63      (subfunction 63B)
;    146550  125001  JMP I 1     (-> gateway pointer)
;    146551  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): START HISTOGRAM
;  NPL server handler: ISTAHIST
; ============================================================================


; ---- START-HISTOGRAM, MON 60 HISTA (63B) at 010440 ----
; enclosing ENTER routine 002662 (framesize 000331) - the command interpreter
; No ,X parameter store immediately precedes 010440 (bare subfunction call)
; 010440 JPL I 64 -> ptr 010524 = thunk 146547 = SAA 63
010435  135066  JPL I 66		; -> 010523
010436  135220  JPL I -160		; -> 010256
010437  124154  JMP 154		; -> 010613
010440  135064  JPL I 64		; -> 010524
010441  135215  JPL I -163		; -> 010256
010442  124151  JMP 151		; -> 010613
