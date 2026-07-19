; ============================================================================
;  154B DEBUG-SWAPPER <ON/OFF>  ->  MON 60 subfunction 154B (0x6C = 108 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 146767 : SAA 154 ; JMP I 1 ; 146244  (verbatim .asm line 52807-52809).
; ============================================================================

; CALL SITE: 010133, in command interpreter ENTER-routine 002662 (framesize 000331)
010115  135147  	JPL I 147		; -> 010264
010116  135325  	JPL I -53		; -> 010043
010117  131004  	JAZ 4		; -> 010123
010120  024145  	LDD 145
010121  020651  	STD ,B -127
010122  124005  	JMP 5		; -> 010127
010123  135144  	JPL I 144		; -> 010267
010124  135317  	JPL I -61		; -> 010043
010125  024143  	LDD 143
010126  020651  	STD ,B -127
010127  146135  	RADD CLD SB DA
010130  172651  	AAA -127
010131  054602  	LDX ,B -176
010132  006006  	STA ,X 6
010133  135137  	JPL I 137		; -> 010272
010134  135307  	JPL I -71		; -> 010043
010135  001136  	STZ I 136

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Authoritative purpose: DEBUG SWAPPER <ON/OFF> - SUBFUNCTION-TABLE.md. Server handler 5NOPAR (generic).
; One MON60 param. 010117 JAZ selects one of two constants into local B-127: 010120 LDD 145 (path A) or 010123 JPL I 144 -> ptr 010267=035034 helper + 010125 LDD 143 (path B). 010132 STA ,X 6 = param1 := &(B-127). PROVEN stores.
; Semantic: param = ON/OFF flag (matches the '<ON/OFF>' purpose): INFERRED.
; err 010134 -> ptr 010043=007500 (leaf error handler, role INFERRED); success 010135 falls through (STZ I 136, no jump).
; octal 154 = 0x6C = 108 decimal.
