; ============================================================================
; MON 11B (octal) TIME / GetBasicTime - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. The previous version of this folder was built on the
; disproven dispatch model (GOTAB as the monitor-call table); it claimed
; GOTAB[11B]=120427B -> P04R2 and a "zero-filled TIME region". All of that was
; an artefact of the wrong model. MON calls dispatch through MCTAB @005620B.
;
; Dispatch (all hops carved; see README):
;   MON 11B -> ENT14 072167B -> GOTAB[11B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[11B] @005631B = 040747B = TIME -> worker (below).
;
; Worker home: 003-S3CP (load 30000B), symbol TIME=040747B (SYMBOL-1-LIST).
; TIME is one of a 5-entry clock family (TIME/CLOCK/XPERC/PERCE/DPERC) that
; share a common body; each entry is a short mode-select stub.
; Addresses and values are octal.
; ============================================================================

; ---- MCTAB[11B] slot (044-S3IDPIT, load 4000B) ----
005631  040747  	; MCTAB[11B] = TIME = 040747B   (dd: byteoff 1842 = 41 e7)

; ---- TIME worker entry (003-S3CP, load 30000B) ----
; Entry byte dd-verified: byteoff (040747-030000)*2 = 9166 -> 41 a3 = 040643.
040747  040643  	MIN ,B -135		; bump the family mode/selector counter
040750  026777  	LDD ,X ,B -1		; load a double word
; --- shared prologue SM2DE=040751B (also entered by the sibling clock calls) ---
040751  146141  	RADD CLD SL DD		; D := L (save return link)
040752  135741  	JPL I ,B -37		; -> common timer body @040713B (TMOUT)
040753  146151  	RADD CLD SA DD		; stage result
040754  146105  	RADD CLD 0 DA
040755  171012  	SAT 12			; unit code
; 040756 = CLOCK boundary (next family entry); not part of TIME.
