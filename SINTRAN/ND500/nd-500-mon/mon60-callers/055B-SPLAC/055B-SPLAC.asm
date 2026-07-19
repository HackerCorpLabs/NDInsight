; ============================================================================
;  055B-SPLAC  ->  MON 60 subfunction SPLAC = 55B (0x2D = 45 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146530  170455  SAA 55      (subfunction 55B)
;    146531  125001  JMP I 1     (-> gateway pointer)
;    146532  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): START-PLACE
;  NPL server handler: ISPLACE
; ============================================================================


; ---- SPLAC call site 1 at 043552 ----
; enclosing ENTER routine 043011 (framesize 000717) - a standalone ENTER routine
; 043545 LDA ,B -163 ; 043546 JAZ 4 -> 043552 : if flag==0 take SPLAC, else SRESPL(140B)@043547
; 043552 JPL I 124 -> ptr 043676 = thunk 146530 = SAA 55
; 043553 callsite+1 = ERROR -> 043321 ; 043554 callsite+2 = SUCCESS
043545  044615  LDA ,B -163
043546  131004  JAZ 4		; -> 043552
043547  135125  JPL I 125		; -> 043674
043550  135125  JPL I 125		; -> 043675
043551  124003  JMP 3		; -> 043554
043552  135124  JPL I 124		; -> 043676
043553  135122  JPL I 122		; -> 043675
043554  000674  STZ ,B -104

; ---- SPLAC call site 2 at 063065 ----
; enclosing ENTER routine 062257 (framesize 000544) - a standalone ENTER routine
; 063060 LDA ,B -165 ; 063061 JAZ 4 -> 063065 : if flag==0 take SPLAC, else SRESPL(140B)@063062
; 063065 JPL I 65 -> ptr 063152 = thunk 146530 = SAA 55
; 063066 callsite+1 = ERROR -> 062446 ; 063067 callsite+2 = SUCCESS
063060  044613  LDA ,B -165
063061  131004  JAZ 4		; -> 063065
063062  135067  JPL I 67		; -> 063151
063063  135055  JPL I 55		; -> 063140
063064  124003  JMP 3		; -> 063067
063065  135065  JPL I 65		; -> 063152
063066  135052  JPL I 52		; -> 063140
063067  000675  STZ ,B -103
