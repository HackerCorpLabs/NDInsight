; ============================================================================
; MON 314B (octal) DefaultRemoteSystem (SDRUS) - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. The previous version declared the worker "misattributed"
; / "not recoverable" (GOTAB[314]=112326B -> DSI4; only the ND-500 SRUSI=125473B
; matched). That was the wrong dispatch model. MON calls take their worker from
; MCTAB@005620B (segment 044-S3IDPIT), NOT from that GOTAB (MFELL for 224 of 256
; calls). MCTAB[314B] = 111501B = SDRUS, real carved ND-100 code in 006-S3FS.
;
; Dispatch (all hops carved; see README):
;   MON 314B -> ENT14 072167B -> GOTAB[314B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[314B] @006134B = 111501B = SDRUS -> worker (below).
;
; Worker home: 006-S3FS (load 26000B), symbol SDRUS=111501B (FILSYS-SYMBOLS).
; Addresses and values are octal.
; ============================================================================

; ---- GOTAB[314B] slot (026-S3IMPIT, load 32000B) ----
071547  072114  	; GOTAB[314B] = MFELL = 072114B   (dd: byteoff 32462 = 74 4c)

; ---- MCTAB[314B] slot (044-S3IDPIT, load 4000B) ----
006134  111501  	; MCTAB[314B] = SDRUS = 111501B   (dd: byteoff 2232 = 93 41)

; ---- SDRUS worker body (006-S3FS, load 26000B) ----
; Entry byte dd-verified: byteoff (111501-026000)*2 = 52866 -> 22 29 = 021051 (STD I 51).
111501  021051  	STD I 51		; prologue: save regs / set frame
111502  146145  	RADD CLD SL DA		; A := L (return link)
111503  146131  	RADD CLD SB DD		; D := B
111504  170047  	SAB 47			; new frame base offset
111505  135046  	JPL I 46		; -> 111553 (FILSYS/COSMOS entry helper)
111506  044046  	LDA 46
111507  146035  	RADD SB DA		; form frame address
111510  171020  	SAT 20
111511  135044  	JPL I 44		; -> 111555 (stage helper)
111512  124036  	JMP 36			; -> 111550 (error tail)
111513  044043  	LDA 43
111514  146035  	RADD SB DA
111515  054401  	LDX ,B 1		; operand #1
111516  135037  	JPL I 37		; -> 111555
111517  124031  	JMP 31			; -> 111550
111520  044037  	LDA 37
111521  146035  	RADD SB DA
111522  054402  	LDX ,B 2		; operand #2
111523  135032  	JPL I 32		; -> 111555
111524  124024  	JMP 24			; -> 111550
111525  044033  	LDA 33
111526  146035  	RADD SB DA
111527  054403  	LDX ,B 3		; operand #3
111530  135025  	JPL I 25		; -> 111555
111531  124017  	JMP 17			; -> 111550
111532  054022  	LDX 22			; assemble the default-remote record
111533  146037  	RADD SB DX
111534  050022  	LDT 22
111535  146036  	RADD SB DT
111536  044022  	LDA 22
111537  146035  	RADD SB DA
111540  146151  	RADD CLD SA DD
111541  044016  	LDA 16
111542  146035  	RADD SB DA
111543  135016  	JPL I 16		; -> 111561 (commit the record)
111544  124004  	JMP 4			; success -> 111550
111545  040404  	MIN ,B 4		; error: bump error counter
111546  170731  	SAA -47			; error code
; ... error/return tail at 111550B (see segments-ref for the full listing) ...
