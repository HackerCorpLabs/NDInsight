; ============================================================================
; MON 077B (octal) SetStartBlock (SETBC) - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. The previous version declared the call "misattributed"
; (GOTAB[077B]=121516B -> F1646 stub) and named the worker SETBL=030164B. That
; was the wrong dispatch model. MON calls take their worker from MCTAB@005620B
; (segment 044-S3IDPIT), NOT from that GOTAB (MFELL for 224 of 256 calls).
; MCTAB[077B] = 103735B = SETBC, carved in 006-S3FS, first of the SETBC/SBSIZ/
; RMAX file-parameter family.
;
; Dispatch (all hops carved; see README):
;   MON 077B -> ENT14 072167B -> GOTAB[077B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[077B] @005717B = 103735B = SETBC -> worker (below).
;
; Worker home: 006-S3FS (load 26000B), symbol SETBC=103735B (FILSYS-SYMBOLS).
; Addresses and values are octal.
; ============================================================================

; ---- GOTAB[077B] slot (026-S3IMPIT, load 32000B) ----
071332  072114  	; GOTAB[077B] = MFELL = 072114B   (dd: byteoff 32180 = 74 4c)

; ---- MCTAB[077B] slot (044-S3IDPIT, load 4000B) ----
005717  103735  	; MCTAB[077B] = SETBC = 103735B   (dd: byteoff 1950 = 87 dd)

; ---- SETBC worker body (006-S3FS, load 26000B) ----
; Entry byte dd-verified: byteoff (103735-026000)*2 = 47034 -> 22 36 = 021066 (STD I 66).
103735  021066  	STD I 66		; prologue: save regs / set frame
103736  146145  	RADD CLD SL DA		; A := L (return link)
103737  146131  	RADD CLD SB DD		; D := B
103740  170006  	SAB 6			; new frame base offset
103741  135063  	JPL I 63		; -> 104024 (open-file lookup helper)
103742  011063  	STT I 63		; store resolved control-block pointer
103743  135066  	JPL I 66		; -> 104031 (set-position helper)
103744  124004  	JMP 4			; success -> 103750
103745  040404  	MIN ,B 4		; error: bump error counter
103746  170772  	SAA -6			; error code
103747  125061  	JMP I 61		; -> 104030 (file-error return)
103750  004402  	STA ,B 2		; store result / status
103751  124375  	JMP -3			; -> 103746 (shared return tail)
; ---- sibling SBSIZ follows at 103752B (STD I 51); RMAX at 103767B ----
