; ============================================================================
;  070B-GPRTE  ->  MON 60 subfunction GPRTE = 70B (0x38 = 56 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146566  170470  SAA 70      (subfunction 70B)
;    146567  125001  JMP I 1     (-> gateway pointer)
;    146570  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): READ A PROCESS TABLE ENTRY FROM THE SYS.MON
;  NPL server handler: 5NOPAR
; ============================================================================


; ---- GPRTE call site 1 at 073472 ----
; enclosing ENTER routine 073412 (framesize 000223) - a standalone ENTER routine
; 073463 RADD SB DA ; 073464 AAA -166 ; 073466 STA ,X 6 = &(B-166)
; 073467 RADD SB DA ; 073470 AAA -75 ; 073471 STA ,X 7 = &(B-75)
; 073472 JPL I 40 -> ptr 073532 = thunk 146566 = SAA 70
073463  146135  RADD CLD SB DA
073464  172612  AAA -166
073465  054602  LDX ,B -176
073466  006006  STA ,X 6
073467  146135  RADD CLD SB DA
073470  172703  AAA -75
073471  006007  STA ,X 7
073472  135040  JPL I 40		; -> 073532
073473  135032  JPL I 32		; -> 073525
073474  044430  LDA ,B 30

; ---- GPRTE call site 2 at 074030 ----
; enclosing ENTER routine 074013 (framesize 000126) - a standalone ENTER routine
; 074021 RADD SB DA ; 074022 AAA -172 ; 074024 STA ,X 6 = &(B-172)
; 074025 RADD SB DA ; 074026 AAA -170 ; 074027 STA ,X 7 = &(B-170)
; 074030 JPL I 51 -> ptr 074101 = thunk 146566 = SAA 70
074021  146135  RADD CLD SB DA
074022  172606  AAA -172
074023  054602  LDX ,B -176
074024  006006  STA ,X 6
074025  146135  RADD CLD SB DA
074026  172610  AAA -170
074027  006007  STA ,X 7
074030  135051  JPL I 51		; -> 074101
074031  135051  JPL I 51		; -> 074102
074032  044610  LDA ,B -170

; ---- GPRTE call site 3 at 110410 ----
; enclosing ENTER routine 110365 (framesize 002250) - a standalone ENTER routine
; 110402 LDA 162 ; 110403 LDX ,B -176 ; 110404 STA ,X 6 = value from LDA 162
; 110405 RADD SB DA ; 110406 AAA -166 ; 110407 STA ,X 7 = &(B-166)
; 110410 JPL I 155 -> ptr 110565 = thunk 146566 = SAA 70
110402  044162  LDA 162
110403  054602  LDX ,B -176
110404  006006  STA ,X 6
110405  146135  RADD CLD SB DA
110406  172612  AAA -166
110407  006007  STA ,X 7
110410  135155  JPL I 155		; -> 110565
110411  134363  JPL -15		; -> 110374
110412  000441  STZ ,B 41
