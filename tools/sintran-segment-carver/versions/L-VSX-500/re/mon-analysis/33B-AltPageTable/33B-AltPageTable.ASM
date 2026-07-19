; ============================================================================
; MON 033B (octal) MALTN / AltPageTable - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. Previous version used the disproven dispatch model
; (GOTAB as the monitor-call table + uncarved CALLPROC bridge) and named the
; worker ALTON. MON calls dispatch through MCTAB @005620B; the worker is MALTN.
;
; Dispatch (all hops carved; see README):
;   MON 033B -> ENT14 072167B -> GOTAB[033B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[033B] @005653B = 037246B = MALTN -> worker (below).
;
; Worker home: 003-S3CP (load 30000B), symbol MALTN=037246B (SYMBOL-1-LIST).
; MALTN heads a small family (MALTN/SKICK/SIDEN) sharing a page-table body.
; Addresses and values are octal.
; ============================================================================

; ---- MCTAB[033B] slot (044-S3IDPIT, load 4000B) ----
005653  037246  	; MCTAB[033B] = MALTN = 037246B   (dd: byteoff 1878 = 3e a6)

; ---- MALTN worker entry (003-S3CP, load 30000B) ----
; Entry byte dd-verified: byteoff (037246-030000)*2 = 7500 -> 50 54 = 050124.
037246  050124  	LDT 124			; page-table selector / bank
037247  042515  	MIN ,X ,B 115		; bump a page-table bookkeeping field
037250  041105  	MIN I 105
037251  051040  	LDT I 40
037252  023400  	STD I ,B ,X 0		; write a double word into a page-table slot
037253  047503  	LDA I ,B ,X 103		; read back a status word
; --- sibling entries into the same body ---
037254  052117  	; SKICK entry (LDT ,X 117 ...)
037256  051040  	; SIDEN entry (LDT I 40 ...)
; --- shared page-table body (prologue @037272B) ---
037272  146146  	RADD CLD SL DT		; save the return link
037273  010600  	STT ,B -200
037274  146116  	RADD CLD SD DT
037275  010603  	STT ,B -175		; staged operand
037276  004601  	STA ,B -177		; staged operand
037277  146101  	RADD CLD 0 DD
037300  172766  	AAA -12			; normalize / count loop
037301  130403  	JAN 3			; -> 037304
037302  146401  	RADD AD1 0 DD
037303  124375  	JMP -3			; -> 037300
037304  146115  	RADD CLD SD DA
; 037305B+: MPY 74 (index*entry-size) / JPL I 75 -> 037404B helper; see 003-S3CP.asm.
