; ============================================================================
;  047B-DELSW  ->  MON 60 subfunction DELSW = 47B (0x27 = 39 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146503  170447  SAA 47      (subfunction 47B)
;    146504  125001  JMP I 1     (-> gateway pointer)
;    146505  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): DELETE SWAP FILE
;  NPL server handler: IDELSWAP
; ============================================================================


; ---- DELETE-SWAP-FILE case, MON 60 DELSW (47B) at 007430 ----
; enclosing ENTER routine 002662 (framesize 000331) - the command interpreter
; 007427 STF ,X 6 = file-name descriptor (F, 3 words -> 6/7/10)
; 007430 JPL I 160 -> ptr 007610 = thunk 146503 = SAA 47
; 007431 callsite+1 = ERROR -> 002673
; 007432 callsite+2 = SUCCESS -> 010613 (command loop)
007425  034665  LDF ,B -113
007426  054602  LDX ,B -176
007427  032006  STF ,X 6
007430  135160  JPL I 160		; -> 007610
007431  135155  JPL I 155		; -> 007606
007432  125344  JMP I -34		; -> 007376
