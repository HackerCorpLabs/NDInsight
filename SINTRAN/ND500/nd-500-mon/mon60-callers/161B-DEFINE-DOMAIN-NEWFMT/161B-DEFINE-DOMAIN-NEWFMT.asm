; ============================================================================
;  161B DEFINE-DOMAIN-NEWFMT (INDFSYDOM)  ->  MON 60 subfunction 161B (0x71 = 113 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147003 : SAA 161 ; JMP I 1 ; 146244  (verbatim .asm line 52819-52821).
; ============================================================================

; CALL SITE: 071025, in ENTER-routine 070160 (framesize 002172)
071000  044625  	LDA ,B -153
071001  054602  	LDX ,B -176
071002  006006  	STA ,X 6
071003  034606  	LDF ,B -172
071004  032007  	STF ,X 7
071005  044012  	LDA 12
071006  124012  	JMP 12		; -> 071020
071007  000016  	STZ 16
071010  053441  	LDT I ,B ,X 41
071011  000452  	STZ ,B 52
071012  066025  	SUB ,X 25
071013  000016  	STZ 16
071014  000452  	STZ ,B 52
071015  067431  	SUB I ,B ,X 31
071016  070545  	AND ,B 145
071017  000722  	STZ ,B -56
071020  135020  	JPL I 20		; -> 071040
071021  135020  	JPL I 20		; -> 071041
071022  044616  	LDA ,B -162
071023  054602  	LDX ,B -176
071024  006006  	STA ,X 6
071025  135015  	JPL I 15		; -> 071042
071026  135013  	JPL I 13		; -> 071041
071027  044626  	LDA ,B -152

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Authoritative purpose: DEFINE STANDARD DOMAIN (NEW DOMAIN FORMAT) (INDFSYDOM) - SUBFUNCTION-TABLE.md. Server handler INDFSYDOM.
; Two MON60 params: 071000 LDA ,B-153; 071001 LDX ,B-176; 071002 STA ,X 6 = param1 (value at B-153). 071003 LDF ,B-172; 071004 STF ,X 7 = param2 (3-word float from B-172). PROVEN stores; field semantics INFERRED.
; err 071026 -> ptr 071041=070175 (leaf error handler); ok 071027 (LDA ,B-152...) continues in the routine.
; octal 161 = 0x71 = 113 decimal.
