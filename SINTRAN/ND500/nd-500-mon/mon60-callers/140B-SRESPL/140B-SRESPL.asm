; ============================================================================
;  SRESPL  ->  MON 60 subfunction SRESPL = 140B (0x60 = 96 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  Subfunction 140B is UNDOCUMENTED in the NPL source (SUBFUNCTION-TABLE.md
;  row 140: dispatch 5NOPAR, no verbatim FUNCTION= comment).  'SRESPL' is only
;  the yaml/thunk client label - no invented meaning is asserted here.
;  Two call sites, each the "flag set" arm of an if/else whose "flag clear" arm
;  calls SPLAC (055B, START-PLACE) instead - so SRESPL sits in a PLACE context.
;  Thunk VERIFIED (both sites point to it):
;    146737 SAA 140 ; 146740 JMP I 1 ; 146741 146244 (gateway).

; ============================================================================
;  CALL SITE 1 : 043547   enclosing ENTER-routine 043011 (framesize 000717)
;    (043011 RADD AD1 CLD SL DX ; 043012 JPL I 171 -> ptr 043203=177300 ENTER)
; ============================================================================
043545  044615  	LDA ,B -163		; A := flag at local(B-163)
043546  131004  	JAZ 4		; -> 043552  ; if flag==0 -> SPLAC (055B) arm
; ---- flag != 0 : SRESPL (140B) ----
043547  135125  	JPL I 125		; -> ptr 043674 = thunk 146737  SRESPL (140B)  *** MON 60 ***
043550  135125  	JPL I 125		; callsite+1 ERROR -> ptr 043675 = routine 043321
043551  124003  	JMP 3		; callsite+2 SUCCESS -> 043554
; ---- flag == 0 : START-PLACE (055B) arm ----
043552  135124  	JPL I 124		; -> ptr 043676 = thunk 146530  SPLAC (055B)
043553  135122  	JPL I 122		; err -> ptr 043675 = routine 043321
043554  000674  	STZ ,B -104		; (continues)
; ---- relevant pointer pool words (data) ----
043674  146737  	<ptr>  -> thunk 146737 (SRESPL 140B)
043675  043321  	<ptr>  -> routine 043321  (error handler)
043676  146530  	<ptr>  -> thunk 146530 (SPLAC 055B)
; NOTE: NO 'LDX ,B -176 / STA ,X n' parameter stores precede 043547.

; ============================================================================
;  CALL SITE 2 : 063062   enclosing ENTER-routine 062257 (framesize 000544)
;    (062257 RADD AD1 CLD SL DX ; 062260 JPL I 174 -> ptr 062454=177300 ENTER)
; ============================================================================
063060  044613  	LDA ,B -165		; A := flag at local(B-165)
063061  131004  	JAZ 4		; -> 063065  ; if flag==0 -> SPLAC (055B) arm
; ---- flag != 0 : SRESPL (140B) ----
063062  135067  	JPL I 67		; -> ptr 063151 = thunk 146737  SRESPL (140B)  *** MON 60 ***
063063  135055  	JPL I 55		; callsite+1 ERROR -> ptr 063140 = routine 062446
063064  124003  	JMP 3		; callsite+2 SUCCESS -> 063067
; ---- flag == 0 : START-PLACE (055B) arm ----
063065  135065  	JPL I 65		; -> ptr 063152 = thunk 146530  SPLAC (055B)
063066  135052  	JPL I 52		; err -> ptr 063140 = routine 062446
063067  000675  	STZ ,B -103		; (continues)
; ---- relevant pointer pool words (data) ----
063140  062446  	<ptr>  -> routine 062446  (error handler)
063151  146737  	<ptr>  -> thunk 146737 (SRESPL 140B)
063152  146530  	<ptr>  -> thunk 146530 (SPLAC 055B)
; NOTE: NO 'LDX ,B -176 / STA ,X n' parameter stores precede 063062.
