; ============================================================================
;  ACTIV  ->  MON 60 subfunction ACTIV = 136B (0x5E = 94 dec)
; ============================================================================
;  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, runs on ND-100)
;  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
;  All addresses OCTAL, ND-100 word addresses, BANK 1 (program), base 0.
;  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
; ============================================================================
;  ACTIVATE STOPPED PROCESS.  Enclosing routine : command interpreter
;  ENTER-routine 002662 (framesize 000331), spanning 002662..010634.
;  Call site = 010610.
;  Thunk VERIFIED: bank1[010632]=146731 ; 146731 SAA 136 ; 146732 JMP I 1 ;
;  146733 146244 (gateway).

; ---- ACTIV command sequence (010573..010613) ----
010573  124012  	JMP 12		; -> 010605  (skip the local error handler below)
; ---- local error handler for this case (itself issues a MON 60 at 010601) ----
010574  146147  	RADD CLD SL DX		; (error handler entry: X := L+1)
010575  014516  	STX ,B 116
010576  004605  	STA ,B -173
010577  054602  	LDX ,B -176
010600  006006  	STA ,X 6
010601  135027  	JPL I 27		; -> ptr 010630 (secondary MON 60; not this call)
010602  135310  	JPL I -70		; -> ptr 010512
010603  135026  	JPL I 26		; -> ptr 010631
010604  125516  	JMP I ,B 116		; -> 010722
; ---- ACTIV main path ----
010605  034665  	LDF ,B -113		; F := process/domain-name descriptor (3 words) from B-113
010606  054602  	LDX ,B -176		; X := gateway param base
010607  032006  	STF ,X 6		; MON60 param1 := name (F, 3 words)
010610  135022  	JPL I 22		; -> ptr 010632 = thunk 146731  ACTIV (136B)  *** MON 60 ***
010611  134363  	JPL -15		; callsite+1 ERROR -> 010574 (local error handler)
010612  124001  	JMP 1		; callsite+2 SUCCESS -> 010613 (command loop area)
010613  044020  	LDA 20			; (command loop continues)
; ---- relevant pointer pool word (data) ----
010632  146731  	<ptr>  -> thunk 146731 (ACTIV 136B)
