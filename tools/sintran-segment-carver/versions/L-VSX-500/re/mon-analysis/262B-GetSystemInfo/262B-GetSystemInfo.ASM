; ============================================================================
; MON 262B (octal) GetSystemInfo (CPUST) - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. The previous version of this folder declared the call
; "misattributed", claiming GOTAB[262B]=066262B routed to an F1737 "compiler
; dispatch descriptor" in 025-S3IRPIT with the F1737->CPUST hop unproven. That
; was the wrong dispatch model. MON calls take their worker from MCTAB@005620B
; (segment 044-S3IDPIT), NOT from that GOTAB (which is MFELL for 224 of 256
; calls). MCTAB[262B] = 063022B = CPUST, and that worker is carved in 006-S3FS.
;
; Dispatch (all hops carved; see README):
;   MON 262B -> ENT14 072167B -> GOTAB[262B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[262B] @006102B = 063022B = CPUST -> worker (below).
;
; Worker home: 006-S3FS (load 26000B), symbol CPUST=063022B (FILSYS-SYMBOLS).
; Addresses and values are octal.
; ============================================================================

; ---- GOTAB[262B] slot (026-S3IMPIT, load 32000B) ----
071515  072114  	; GOTAB[262B] = MFELL = 072114B   (dd: byteoff 32410 = 74 4c)

; ---- MCTAB[262B] slot (044-S3IDPIT, load 4000B) ----
006102  063022  	; MCTAB[262B] = CPUST = 063022B   (dd: byteoff 2180 = 66 12)

; ---- CPUST worker body (006-S3FS, load 26000B) ----
; Entry byte dd-verified: byteoff (063022-026000)*2 = 29732 -> 22 4e = 021116 (STD I 116).
; Full closed body 063022B-063301B is in ../../segments-ref/006-S3FS/006-S3FS.asm;
; the prologue, parameter check and the MOVEW block copies are excerpted here.
063022  021116  	STD I 116		; prologue: save regs / set frame
063023  146145  	RADD CLD SL DA		; A := L (return link)
063024  146131  	RADD CLD SB DD		; D := B
063025  170043  	SAB 43			; new frame base offset
063026  135113  	JPL I 113		; -> 063141 (FILSYS entry helper)
063027  014411  	STX ,B 11		; stash caller X
063030  044402  	LDA ,B 2		; A := Number parameter
063031  142005  	SKP IF DA UEQ 0
063032  125110  	JMP I 110		; -> 063142 (error return)
063033  045110  	LDA I 110
063034  051365  	LDT I -13
063035  140065  	SKP IF DA EQL ST
063036  125106  	JMP I 106		; -> 063144 (error path)
063037  044402  	LDA ,B 2
063040  050105  	LDT 105			; upper bound
063041  141065  	SKP IF DA GRE ST
063042  124003  	JMP 3			; -> 063045
063043  050103  	LDT 103			; lower bound
063044  141056  	SKP IF DT GRE SA
063045  125077  	JMP I 77		; -> 063144 (out-of-range error)
063046  170411  	SAA 11
063047  146154  	RADD CLD SA DL		; L := source pointer
063050  044077  	LDA 77
063051  146151  	RADD CLD SA DD		; D := dest pointer
063052  171025  	SAT 25			; word count
063053  146036  	RADD SB DT
063054  143101  	MOVEW			; block copy #1 -> caller buffer
063055  170403  	SAA 3
063056  146154  	RADD CLD SA DL
063057  044071  	LDA 71
063060  146151  	RADD CLD SA DD
063061  171037  	SAT 37			; word count
063062  146036  	RADD SB DT
063063  143101  	MOVEW			; block copy #2 -> caller buffer
063064  171425  	SAX 25
063065  146037  	RADD SB DX
063066  170437  	SAA 37
; ... continues to close at 063301B (see segments-ref for the full listing) ...
