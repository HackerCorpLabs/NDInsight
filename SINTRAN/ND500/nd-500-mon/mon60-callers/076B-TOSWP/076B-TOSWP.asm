; ============================================================================
;  TOSWP  ->  MON 60 subfunction 076B = 0x3E = 62 dec
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  Subfn purpose (authoritative, 5P-P2-MON60.NPL): MESSAGE TO SWAPPER
;  Server handler (5IFUNC dispatch): ITOSWP
;
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway 146244 (MON 60
;  at 146256). Thunk 146610: SAA 76 / JMP I 1 / 146244 (verified below).
;  Call idiom (PROVEN, prog.md sec 4/5): LDX ,B -176 sets X = gateway frame top;
;  STx ,X 6/7/10/11 stores MON 60 params 1/2/3/4; JPL I <disp> -> pointer word
;  holding the thunk address. Return polarity (PROVEN, prog.md sec 5.4):
;  callsite+1 = ERROR (direct), callsite+2 = SUCCESS (skip).
;  Annotation "; EA=.. [..]=.." resolves each JPL/JMP I pointer word from bytes.
; ============================================================================

; ---- thunk 146610 (verified from bytes) ----
146610  170476  SAA 76
146611  125001  JMP I 1		; EA=146612 [146612]=146244
146612  146244  RADD CM1 SL DL

; ---- call site 073362 in routine 073115 (framesize 000336; the LIST/SET-SYSTEM-PARAMETERS handler - see LIST-SYSTEM-PARAMETERS/) ----
073356  146135  RADD CLD SB DA
073357  172611  AAA -167
073360  054602  LDX ,B -176
073361  006006  STA ,X 6
073362  135022  JPL I 22		; EA=073404 [073404]=146610  thunk 146610=SAA 76 -> MON60
073363  135330  JPL I -50		; EA=073313 [073313]=177327
073364  135344  JPL I -34		; EA=073330 [073330]=177335
073365  124013  JMP 13

; ---- call site 073675 in routine 073535 (framesize 000030) ----
073671  146135  RADD CLD SB DA
073672  172620  AAA -160
073673  054602  LDX ,B -176
073674  006006  STA ,X 6
073675  135014  JPL I 14		; EA=073711 [073711]=146610  thunk 146610=SAA 76 -> MON60
073676  135005  JPL I 5		; EA=073703 [073703]=177327
073677  135013  JPL I 13		; EA=073712 [073712]=177335
073700  177300  BAND

; ---- call site 073741 in routine 073713 (framesize 000016) ----
073735  146135  RADD CLD SB DA
073736  172612  AAA -166
073737  054602  LDX ,B -176
073740  006006  STA ,X 6
073741  135007  JPL I 7		; EA=073750 [073750]=146610  thunk 146610=SAA 76 -> MON60
073742  135005  JPL I 5		; EA=073747 [073747]=177327
073743  135006  JPL I 6		; EA=073751 [073751]=177335
073744  177300  BAND

; ---- call site 074003 in routine 073752 (framesize 000025) ----
073777  146135  RADD CLD SB DA
074000  172620  AAA -160
074001  054602  LDX ,B -176
074002  006006  STA ,X 6
074003  135006  JPL I 6		; EA=074011 [074011]=146610  thunk 146610=SAA 76 -> MON60
074004  135004  JPL I 4		; EA=074010 [074010]=177327
074005  135005  JPL I 5		; EA=074012 [074012]=177335
074006  177300  BAND

; ---- call site 107434 in routine 103722 (framesize 000605) ----
107430  146135  RADD CLD SB DA
107431  172613  AAA -165
107432  054602  LDX ,B -176
107433  006006  STA ,X 6
107434  135043  JPL I 43		; EA=107477 [107477]=146610  thunk 146610=SAA 76 -> MON60
107435  125643  JMP I ,B -135
107436  170420  SAA 20
107437  144151  SWAP CLD SA DD
