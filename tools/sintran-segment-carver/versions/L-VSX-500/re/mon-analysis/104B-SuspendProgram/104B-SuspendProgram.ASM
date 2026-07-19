; ============================================================================
; MON 104B (octal) HOLD / SuspendProgram - real SINTRAN L07 disassembly
;
; OVERLAY RESOLVED 2026-07-13. An intermediate pass flagged this worker as a
; "data table" because it read 040645B in the 003-S3CP overlay - where those
; bytes are a powers-of-ten CONSTANT table (100000000, 10^7 .. 10^0), used by
; the neighbouring clock/decimal code. That was the WRONG overlay. The real HOLD
; worker is coherent code in 026-S3IMPIT (the monitor-PIT overlay), which is the
; view the monitor level runs in (CALLP dispatches there).
;
; Dispatch (all hops carved; see README):
;   MON 104B -> ENT14 072167B -> GOTAB[104B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[104B] @005724B = 040645B = HOLD -> worker (below).
;
; Worker home: 026-S3IMPIT (load 32000B), symbol HOLD=040645B (SYMBOL-1-LIST).
; Same bytes in 017-S3SMPIT (the swap twin). Addresses/values octal.
; ============================================================================

; ---- MCTAB[104B] slot (044-S3IDPIT, load 4000B) ----
005724  040645  	; MCTAB[104B] = HOLD = 040645B   (dd: byteoff 1960 = 41 a5)

; ---- HOLD worker body (026-S3IMPIT, load 32000B) ----
; Entry byte dd-verified: byteoff (040645-032000)*2 = 6986 -> 48 11 = 044021.
; A 5-way priority-ladder: each entry point loads a different priority level into
; A, then all converge on the common tail that sets B/L, enables interrupts and
; EXITs (returns to the scheduler = suspend/yield).
040645  044021  	LDA 21			; entry: priority 21B
040646  124010  	JMP 10			; -> 040656 (common tail)
040647  044020  	LDA 20			; entry: priority 20B
040650  124006  	JMP 6			; -> 040656
040651  044017  	LDA 17			; entry: priority 17B
040652  124004  	JMP 4			; -> 040656
040653  044016  	LDA 16			; entry: priority 16B
040654  124002  	JMP 2			; -> 040656
040655  044015  	LDA 15			; entry: priority 15B
040656  050263  	LDT -115		; --- common tail ---
040657  146163  	RADD CLD ST DB		; B := T (restore/set base)
040660  050263  	LDT -115
040661  146164  	RADD CLD ST DL		; L := T (set return link)
040662  150402  	ION			; interrupts on
040663  146142  	EXIT			; return (to scheduler)
