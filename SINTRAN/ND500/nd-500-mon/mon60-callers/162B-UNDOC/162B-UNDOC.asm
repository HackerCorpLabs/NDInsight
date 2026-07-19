; ============================================================================
;  162B UNDOC (server dispatch 5NOPAR)  ->  MON 60 subfunction 162B (0x72 = 114 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147006 : SAA 162 ; JMP I 1 ; 146244  (verbatim .asm line 52822-52824).
; ============================================================================

; CALL SITE: 141266, in ENTER-routine 140763 (framesize 000032)
141250  135032  	JPL I 32		; -> 141302
141251  146135  	RADD CLD SB DA
141252  172624  	AAA -154
141253  054602  	LDX ,B -176
141254  006006  	STA ,X 6
141255  146135  	RADD CLD SB DA
141256  172626  	AAA -152
141257  006007  	STA ,X 7
141260  146135  	RADD CLD SB DA
141261  172630  	AAA -150
141262  006010  	STA ,X 10
141263  146135  	RADD CLD SB DA
141264  172632  	AAA -146
141265  006011  	STA ,X 11
141266  135015  	JPL I 15		; -> 141303
141267  135013  	JPL I 13		; -> 141302
141270  135014  	JPL I 14		; -> 141304

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Undocumented: no FUNCTION= comment; dispatch generic 5NOPAR. Purpose UNKNOWN.
; FOUR pointer parameters, all local addresses: 141254 STA ,X 6 = &(B-154); 141257 STA ,X 7 = &(B-152); 141262 STA ,X 10 = &(B-150); 141265 STA ,X 11 = &(B-146). PROVEN stores; per-field meaning UNKNOWN (undocumented).
; err 141267 -> ptr 141302=140767 (inner error handler); ok 141270 -> ptr 141304=177335 (LEAVE-SKIP).
; octal 162 = 0x72 = 114 decimal.
