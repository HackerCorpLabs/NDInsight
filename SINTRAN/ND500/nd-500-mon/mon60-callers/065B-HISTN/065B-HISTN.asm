; ============================================================================
;  065B-HISTN  ->  MON 60 subfunction HISTN = 65B (0x35 = 53 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146555  170465  SAA 65      (subfunction 65B)
;    146556  125001  JMP I 1     (-> gateway pointer)
;    146557  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): READ HISTOGRAM
;  NPL server handler: IREAHIST
; ============================================================================


; ---- READ-HISTOGRAM, MON 60 HISTN (65B) at 040437 ----
; enclosing ENTER routine 040422 (framesize 000243) - a standalone ENTER routine
; 040432 SAA 100 ; 040433 SWAP CLD SA DD ; 040434 LDT ,B -162
; 040435 LDX ,B -176 ; 040436 STF ,X 6 = F-register descriptor (3 words)
; 040437 JPL I 163 -> ptr 040622 = thunk 146555 = SAA 65
040432  170500  SAA 100
040433  144151  SWAP CLD SA DD
040434  050616  LDT ,B -162
040435  054602  LDX ,B -176
040436  032006  STF ,X 6
040437  135163  JPL I 163		; -> 040622
040440  135163  JPL I 163		; -> 040623
040441  025163  LDD I 163
