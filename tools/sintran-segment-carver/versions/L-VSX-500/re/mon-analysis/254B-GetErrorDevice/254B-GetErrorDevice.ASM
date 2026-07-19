;=============================================================================
; MON 254B (octal) GetErrorDevice (GERDV) - real SINTRAN L07 disassembly
;
; Returns the logical device number of the error device (the terminal that
; outputs system errors and RT program messages - normally the console). If the
; error device is reserved by an RT program the RT description address is also
; returned.
;
; CORRECTED 2026-07-13. The previous version of this folder used the WRONG
; dispatch model: it read a "GOTAB" out of SINTRAN-DATA_commoncode, got
; GOTAB[254]=066246B -> an F1734 "entry stub", and declared the GERDV worker
; "misattributed" (attached by symbol NAME only, across an uncarved CALLPROC).
; That GOTAB-in-commoncode table is fiction (its slot 0 is 000000, not the real
; GOTAB slot-0 MFELL 072114B). MON calls dispatch through MCTAB @ 005620B in
; segment 044-S3IDPIT (load 4000B), indexed directly by N. MCTAB[254B] = 102525B
; = GERDV - proving the SAME worker the old folder found by name, now directly
; from the table slot. No F1734 stub is on the real path.
;
; Dispatch (all hops carved; see README):
;   MON 254B -> ENT14 072167B -> GOTAB[254B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[254B] @006074B = 102525B = GERDV -> worker (below).
;
; Worker home: 025-S3IRPIT (load 32000B), symbol GERDV=102525B (SYMBOL-2-LIST).
; The block is bounded by the next symbol HIL14=102555B (24 words); the JPL I
; link cells (102557/102561/102562) lie past it in the following HIL14 region.
; Addresses and values are octal.
;=============================================================================

; ---- MCTAB[254B] slot (044-S3IDPIT, load 4000B) ----
; MCTAB base 005620B + 254 = 006074B. dd byteoff (06074-04000)*2 = 2168 -> 85 55 = 102525B.
006074  102525  	; MCTAB[254B] = GERDV = 102525B

; ---- GERDV worker body (025-S3IRPIT, load 32000B) ----
; GERDV @102525B. Entry byte dd-verified: byteoff (0102525-032000)*2 = 41642 -> ba 1a = 135032B.
; Enables the interrupt-enable bits, reads the error-device descriptor through an
; indirect pointer, calls shared resident workers, and stores the logical device
; number and the RT description address back into the caller's B-frame.
102525  135032  	JPL I 32		; -> [102557]  call resident worker (prologue)
102526  170404  	SAA 4		; A = 4
102527  150307  	MST PIE		; PIE |= (A & mask) - enable interrupt-enable bits
102530  055030  	LDX I 30		; X = mem[[102560]]  - error-device descriptor pointer
102531  046015  	LDA ,X 15		; A = mem[X+15]
102532  146151  	RADD CLD SA DD	; D = A  (copy)
102533  002015  	STZ ,X 15		; mem[X+15] = 0
102534  046021  	LDA ,X 21		; A = mem[X+21]
102535  004421  	STA ,B 21		; mem[B+21] = A
102536  002021  	STZ ,X 21		; mem[X+21] = 0
102537  171005  	SAT 5		; T = 5
102540  135021  	JPL I 21		; -> [102561]  call resident worker
102541  045363  	LDA I -15		; A = mem[[102524]]  (indirect)
102542  144015  	SWAP SD DA		; exchange D <-> A  (full exchange)
102543  006015  	STA ,X 15		; mem[X+15] = A
102544  135015  	JPL I 15		; -> [102561]  call resident worker
102545  146115  	RADD CLD SD DA	; A = D  (copy)
102546  004412  	STA ,B 12		; mem[B+12] = A  - logical error-device number (ErrorDevice out)
102547  135013  	JPL I 13		; -> [102562]  call resident worker
102550  146117  	RADD CLD SD DX	; X = D  (copy)
102551  046001  	LDA ,X 1		; A = mem[X+1]
102552  004413  	STA ,B 13		; mem[B+13] = A  - RT description address (RTProgram out)
102553  044421  	LDA ,B 21		; A = mem[B+21]
102554  055004  	LDX I 4		; X = mem[[102560]]
; 102555 = HIL14 boundary (next symbol); not part of this worker.
