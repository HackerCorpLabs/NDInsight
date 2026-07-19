; ============================================================================
; MON 114B (octal) TUSED / GetTimeUsed - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. Previous version used the disproven dispatch model
; (GOTAB as the monitor-call table + uncarved CALLPROC bridge). MON calls
; dispatch through MCTAB @005620B.
;
; Dispatch (all hops carved; see README):
;   MON 114B -> ENT14 072167B -> GOTAB[114B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[114B] @005734B = 041303B = TUSED -> worker (below).
;
; Worker home: 003-S3CP (load 30000B), symbol TUSED=041303B (SYMBOL-1-LIST).
; NOTE: address 041303B is overlay-dependent (003/013 differ from 070/071); the
; 003-S3CP overlay is the coherent command/monitor code shown here.
; Addresses and values are octal.
; ============================================================================

; ---- MCTAB[114B] slot (044-S3IDPIT, load 4000B) ----
005734  041303  	; MCTAB[114B] = TUSED = 041303B   (dd: byteoff 1976 = 42 c3)

; ---- TUSED worker entry (003-S3CP, load 30000B) ----
; Entry byte dd-verified: byteoff (041303-030000)*2 = 9606 -> c4 35 = 142065.
041303  142065  	SKP IF DA UEQ ST	; arg check A vs T
041304  124003  	JMP 3			; -> 041307 (ok)
041305  170547  	SAA 147			; error code
041306  125146  	JMP I 146		; -> 041454 (error/return)
041307  146157  	RADD CLD SA DX		; X := selector
041310  052012  	LDT ,X 12
041311  010602  	STT ,B -176		; stage a field
041312  054143  	LDX 143
041313  135143  	JPL I 143		; -> 041456 (helper)
041314  125742  	JMP I ,B -36		; 1NOTO: return via shared body
; --- multi-way selector arms (1FU2/1FU3/STERM/1FU4/1FU5) each load a table
;     pointer and JPL I -> 041456B, returning via JMP I ,B -36. See 003-S3CP.asm.
041344  173413  	AAX 13
041345  150401  	IOF			; interrupts OFF around the accounting read
041346  146105  	RADD CLD 0 DA
041347  050113  	LDT 113
041350  135102  	JPL I 102		; -> 041452
041361  150402  	ION			; interrupts ON
041362  044631  	LDA ,B -147
; 041303B-041373B is the TUSED body; full listing in 003-S3CP.asm.
