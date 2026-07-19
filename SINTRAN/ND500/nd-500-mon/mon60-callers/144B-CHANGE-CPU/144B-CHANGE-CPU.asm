; ============================================================================
;  144B CHANGE-CPU (ICHACPU)  ->  MON 60 subfunction 144B (0x64 = 100 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Every MON 60 reaches the ND-500 through the single gateway at 146244
;  (MON 60 at 146256).  A 'JPL I <disp>' whose pointer word (bank1[P+disp])
;  holds a thunk address is a MON 60 call; the thunk's SAA n is the subfn code.
;  Return convention (PROVEN, prog.md 5.4): callsite+1 = ERROR (direct),
;                                           callsite+2 = SUCCESS (skip).
;  Thunk 146750 : SAA 144 ; JMP I 1 ; 146244  (verbatim .asm line 52792-52794).
; ============================================================================

; CALL SITE 1: 010345, in command interpreter ENTER-routine 002662 (framesize 000331)
010335  170400  	SAA 0
010336  135341  	JPL I -37		; -> 010277
010337  135317  	JPL I -61		; -> 010256
010340  020651  	STD ,B -127
010341  146135  	RADD CLD SB DA
010342  172651  	AAA -127
010343  054602  	LDX ,B -176
010344  006006  	STA ,X 6
010345  135152  	JPL I 152		; -> 010517
010346  135310  	JPL I -70		; -> 010256
010347  125144  	JMP I 144		; -> 010513

; CALL SITE 2: 011231, in ENTER-routine 011043 (framesize 000236)
011220  171001  	SAT 1
011221  140065  	SKP IF DA EQL ST
011222  124013  	JMP 13		; -> 011235
011223  024620  	LDD ,B -160
011224  020626  	STD ,B -152
011225  146135  	RADD CLD SB DA
011226  172626  	AAA -152
011227  054602  	LDX ,B -176
011230  006006  	STA ,X 6
011231  135034  	JPL I 34		; -> 011265
011232  134262  	JPL -116		; -> 011114
011233  124144  	JMP 144		; -> 011377

; ---------------------------------------------------------------------------
; ANNOTATIONS  (instruction words above are verbatim from source)
; ---------------------------------------------------------------------------
; Authoritative purpose: CHANGE CPU (ICHACPU) - SUBFUNCTION-TABLE.md / 5P-P2-MON60.NPL. Server handler ICHACPU.
; Site1 (010345): one MON60 param. 010336 JPL I -37 -> ptr 010277=002003 = numeric-arg evaluator, returns value in D; 010340 STD ,B-127; 010344 STA ,X 6 = param1 := &(B-127). PROVEN. Value = CPU number: INFERRED.
; Site1 err 010346 -> ptr 010256=007500 (leaf error handler, role INFERRED); ok 010347 -> ptr 010513=010613 (command loop, PROVEN).
; Site2 (011231): guarded by 011221 SKP IF DA EQL ST. 011223 LDD ,B-160 -> 011224 STD ,B-152; 011230 STA ,X 6 = param1 := &(B-152). PROVEN.
; Site2 err 011232 -> 011114 (leaf handler, role INFERRED); ok 011233 -> 011377 (continues in routine).
; octal 144 = 0x64 = 100 decimal.
