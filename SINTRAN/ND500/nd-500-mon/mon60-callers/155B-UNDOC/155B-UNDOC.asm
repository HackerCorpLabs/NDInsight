; ============================================================================
;  155B UNDOC (server dispatch 5NOPAR)  ->  MON 60 subfunction 155B (0x6D = 109 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 147030 : SAA 155 ; JMP I 1 ; 146244  (verbatim .asm line 52840-52842).
; ============================================================================

; CALL SITE: 005741, in command interpreter ENTER-routine 002662 (framesize 000331)
005741  135171  	JPL I 171		; -> 006132
005742  135340  	JPL I -40		; -> 005702
005743  125366  	JMP I -12		; -> 005731

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Undocumented: 5P-P2-MON60.NPL marks codes 155-167 'free for patching'; no FUNCTION= comment; dispatch generic 5NOPAR. Purpose UNKNOWN.
; NO parameter stores precede the call (005740 is a JMP). Caller marshals no parameters -> generic no-parameter server dispatch. PROVEN.
; err 005742 -> ptr 005702=002673 (error reporter); ok 005743 -> ptr 005731=010613 (command loop).
; octal 155 = 0x6D = 109 decimal.
