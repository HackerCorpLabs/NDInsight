; ============================================================================
;  GPSGE  ->  MON 60 subfunction GPSGE = 102B (0x42 = 66 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  Authoritative purpose (SUBFUNCTION-TABLE.md, handler IFORGET):
;    STOP ND-500 SYSTEM (ABORT ALL ACTIVE PROCS, RELEASE MON60 BUFFERS).
;    NOTE: 102B is NOT a status/get call - the yaml client name GPSGE is
;    retained here only as the thunk label.
;  Enclosing routine : command interpreter ENTER-routine 002662 (framesize
;  000331), spanning 002662..010634.  Call site = 007514.
;  Thunk VERIFIED: bank1[007630]=146673 ; 146673 SAA 102 ; 146674 JMP I 1 ;
;  146675 146244 (gateway).

; ---- GPSGE (stop-system) command sequence (007507..007516) ----
007507  170400  	SAA 0
007510  170401  	SAA 1
007511  005115  	STA I 115		; store 1 via indirect pointer (flag set; INFERRED)
007512  170401  	SAA 1
007513  005114  	STA I 114		; store 1 via indirect pointer (flag set; INFERRED)
007514  135114  	JPL I 114		; -> ptr 007630 = thunk 146673  GPSGE (102B)  *** MON 60 ***
007515  134363  	JPL -15		; callsite+1 ERROR -> 007500 (local error handler)
007516  125260  	JMP I -120		; callsite+2 SUCCESS -> ptr 007376 = 010613 (command loop)
; ---- relevant pointer pool words (data) ----
007376  010613  	<ptr>  -> 010613  (command loop within routine 002662)
007630  146673  	<ptr>  -> thunk 146673 (GPSGE 102B)
; NOTE: NO 'LDX ,B -176 / STA ,X n' parameter stores precede 007514.
;       GPSGE is issued with no MON 60 input parameters marshalled.
