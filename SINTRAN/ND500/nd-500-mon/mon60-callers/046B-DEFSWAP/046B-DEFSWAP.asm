; ============================================================================
;  046B-DEFSWAP  ->  MON 60 subfunction DEFSWAP = 46B (0x26 = 38 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146500  170446  SAA 46      (subfunction 46B)
;    146501  125001  JMP I 1     (-> gateway pointer)
;    146502  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): DEFINE SWAP FILE
;  NPL server handler: IDEFSWAP
; ============================================================================


; ---- DEFINE-SWAP-FILE case, MON 60 DEFSWAP (46B) at 007422 ----
; enclosing ENTER routine 002662 (framesize 000331) - the command interpreter
; 007421 STF ,X 6 = file-name descriptor (F, 3 words -> 6/7/10)
; 007422 JPL I 165 -> ptr 007607 = thunk 146500 = SAA 46
; 007423 callsite+1 = ERROR -> 002673
; 007424 callsite+2 = SUCCESS -> 010613 (command loop)
007417  034665  LDF ,B -113
007420  054602  LDX ,B -176
007421  032006  STF ,X 6
007422  135165  JPL I 165		; -> 007607
007423  135163  JPL I 163		; -> 007606
007424  125352  JMP I -26		; -> 007376
