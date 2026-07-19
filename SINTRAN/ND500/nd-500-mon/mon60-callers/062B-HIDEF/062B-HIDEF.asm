; ============================================================================
;  062B-HIDEF  ->  MON 60 subfunction HIDEF = 62B (0x32 = 50 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word holds a thunk
;  address (146310..147070) is a MON 60 subfunction call; the thunk's SAA n
;  is the subfunction code.
;  Thunk for this subfunction (PROVEN, bytes read):
;    146544  170462  SAA 62      (subfunction 62B)
;    146545  125001  JMP I 1     (-> gateway pointer)
;    146546  146244  <ptr to gateway 146244>
;  Return convention (PROVEN, prog.md sec 4.5/5.4): callsite+1 = ERROR (direct),
;  callsite+2 = SUCCESS (skip).
;  NPL purpose (authoritative, 5P-P2-MON60.NPL): DEFINE HISTOGRAM
;  NPL server handler: IDEFHIST
; ============================================================================


; ---- DEFINE-HISTOGRAM, MON 60 HIDEF (62B) at 040133 ----
; enclosing ENTER routine 040050 (framesize 000010) - a standalone ENTER routine
; 040122 RADD SB DA ; 040123 AAA -172 ; 040125 STA ,X 6 = &(B-172)
; 040126 LDA 15 ; 040127 STA ,X 7 = value from LDA 15
; 040130 RADD SB DA ; 040131 AAA -166 ; 040132 STA ,X 10 = &(B-166)
; 040133 JPL I 13 -> ptr 040146 = thunk 146544 = SAA 62
040122  146135  RADD CLD SB DA
040123  172606  AAA -172
040124  054602  LDX ,B -176
040125  006006  STA ,X 6
040126  044015  LDA 15
040127  006007  STA ,X 7
040130  146135  RADD CLD SB DA
040131  172612  AAA -166
040132  006010  STA ,X 10
040133  135013  JPL I 13		; -> 040146
040134  135006  JPL I 6		; -> 040142
040135  135012  JPL I 12		; -> 040147
