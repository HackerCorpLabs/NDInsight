; ============================================================================
; MON 316B (octal) SetRemoteAccess (SLRMO) - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. The previous version declared the worker "misattributed"
; / "not recoverable" (GOTAB[316]=112355B -> DSI5). That was the wrong dispatch
; model. MON calls take their worker from MCTAB@005620B (segment 044-S3IDPIT),
; NOT from that GOTAB (MFELL for 224 of 256 calls). MCTAB[316B] = 027530B =
; SLRMO, real carved ND-100 code in 006-S3FS.
;
; Dispatch (all hops carved; see README):
;   MON 316B -> ENT14 072167B -> GOTAB[316B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[316B] @006136B = 027530B = SLRMO -> worker (below).
;
; Worker home: 006-S3FS (load 26000B), symbol SLRMO=027530B (FILSYS-SYMBOLS).
; SLRMO is short and flows into RSCAL at 027542B.
; Addresses and values are octal.
; ============================================================================

; ---- GOTAB[316B] slot (026-S3IMPIT, load 32000B) ----
071551  072114  	; GOTAB[316B] = MFELL = 072114B   (dd: byteoff 32466 = 74 4c)

; ---- MCTAB[316B] slot (044-S3IDPIT, load 4000B) ----
006136  027530  	; MCTAB[316B] = SLRMO = 027530B   (dd: byteoff 2236 = 2f 58)

; ---- SLRMO worker body (006-S3FS, load 26000B) ----
; Entry byte dd-verified: byteoff (027530-026000)*2 = 1712 -> ba 06 = 135006 (JPL I 6).
027530  135006  	JPL I 6			; -> 027536 (helper: clears -4/4, returns state)
027531  044006  	LDA 6			; A := state word
027532  131003  	JAZ 3			; -> 027535 (state zero -> skip store)
027533  044412  	LDA ,B 12		; A := Mode argument (0=off, 1=on)
027534  005004  	STA I 4			; store Mode through resident pointer @4
027535  125004  	JMP I 4			; -> 027541 (return)
027536  000374  	STZ -4			; helper: zero the two work cells
027537  000004  	STZ 4
027540  146345  	RADD CM1 CLD SL DA	; A := -1 - (link) -> state word
027541  010341  	STT -37			; (return; RSCAL follows at 027542B)
