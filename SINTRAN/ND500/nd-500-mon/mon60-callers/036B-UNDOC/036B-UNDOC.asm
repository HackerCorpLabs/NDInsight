; ============================================================================
;  036B UNDOC (server dispatch 5NOPAR)  ->  MON 60 subfunction 036B (0x1E = 30 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 146775 : SAA 036 ; JMP I 1 ; 146244  (verbatim .asm line 52813-52815).
; ============================================================================

; CALL SITE: 005733, in command interpreter ENTER-routine 002662 (framesize 000331)
005731  010613  	STT ,B -165
005732  143171  	MOVEW
005733  135174  	JPL I 174		; -> 006127
005734  135346  	JPL I -32		; -> 005702
005735  124003  	JMP 3		; -> 005740
005736  135173  	JPL I 173		; -> 006131
005737  135343  	JPL I -35		; -> 005702
005740  125371  	JMP I -7		; -> 005731

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Undocumented: no FUNCTION= comment in 5P-P2-MON60.NPL; dispatch is generic 5NOPAR. Purpose UNKNOWN.
; Thunk note: three 036B thunks exist (146434 / 146442 / 146775); only 146775 has a resolvable caller (this one).
; NO parameter stores precede the call: 005731/005732 are pool/mis-decoded data words (see prog.md sec 9.1), and no 'STA ,X n' appears in 005727-005732. Caller marshals no parameters -> consistent with generic no-parameter server dispatch. PROVEN (absence of STA ,X before 005733).
; err 005734 -> ptr 005702=002673 (interpreter error reporter); ok 005735 -> 005740 (005740 JMP I -7 -> ptr 005731=010613 command loop).
; octal 036 = 0x1E = 30 decimal.
