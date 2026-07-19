; ============================================================================
; MON 113B (octal) CLOCK / GetCurrentTime - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. Previous version used the disproven dispatch model
; (GOTAB as the monitor-call table + an uncarved CALLPROC bridge). MON calls
; dispatch through MCTAB @005620B.
;
; Dispatch (all hops carved; see README):
;   MON 113B -> ENT14 072167B -> GOTAB[113B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[113B] @005733B = 040756B = CLOCK -> worker (below).
;
; Worker home: 003-S3CP (load 30000B), symbol CLOCK=040756B (SYMBOL-1-LIST).
; CLOCK is a clock-family entry that runs the calendar/time-of-day conversion,
; using the adjacent XPERC/PERCE/DPERC decimal-breakdown code (040766B+) and a
; decimal-powers table (see HOLD folder). Addresses and values are octal.
; ============================================================================

; ---- MCTAB[113B] slot (044-S3IDPIT, load 4000B) ----
005733  040756  	; MCTAB[113B] = CLOCK = 040756B   (dd: byteoff 1974 = 41 ee)

; ---- CLOCK worker entry (003-S3CP, load 30000B) ----
; Entry byte dd-verified: byteoff (040756-030000)*2 = 9180 -> c3 b0 = 141660.
040756  141660  	RDIV ST			; divide raw clock -> quotient/remainder
040757  146117  	RADD CLD SD DX		; X := quotient
040760  172460  	AAA 60
040761  135005  	JPL I 5			; -> 040766B (XPERC decimal-breakdown helper)
040762  170460  	SAA 60
040763  146075  	RADD SX DA		; A := X
040764  135002  	JPL I 2			; -> 040766B (next field)
; --- CONKI=040765B: common tail for this entry ---
040765  125742  	JMP I ,B -36		; -> 040727B (return via shared body)
; 040766 = XPERC boundary (shared decimal-breakdown code); see README.
