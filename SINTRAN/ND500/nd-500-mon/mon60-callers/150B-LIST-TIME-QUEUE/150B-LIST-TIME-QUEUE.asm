; ============================================================================
;  150B LIST-TIME-QUEUE (ILI5TQU)  ->  MON 60 subfunction 150B (0x68 = 104 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 146753 : SAA 150 ; JMP I 1 ; 146244  (verbatim .asm line 52795-52797).
; ============================================================================

; CALL SITE: 111614, standalone ENTER-routine 111604 (framesize 000004)
111604  146547  	RADD AD1 CLD SL DX
111605  135114  	JPL I 114		; -> 111721
111606  000004  	STZ 4
111607  044606  	LDA ,B -172
111610  005112  	STA I 112
111611  044112  	LDA 112
111612  054602  	LDX ,B -176
111613  006006  	STA ,X 6
111614  135110  	JPL I 110		; -> 111724
111615  135110  	JPL I 110		; -> 111725
111616  170441  	SAA 41

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Authoritative purpose: LIST ND-500 TIME-QUEUE (ILI5TQU) - SUBFUNCTION-TABLE.md. Server handler ILI5TQU.
; Small standalone routine. One MON60 param: 111607 LDA ,B-172 (routine incoming local); 111610 STA I 112 / 111611 LDA 112 (via pointer word at 112); 111613 STA ,X 6 = param1. PROVEN stores; source-word semantics (indirection through 112) not fully traced: INFERRED.
; err 111615 -> ptr 111725=177327 (LEAVE with error); success 111616 (SAA 41...) falls through and continues in the routine.
; octal 150 = 0x68 = 104 decimal.
