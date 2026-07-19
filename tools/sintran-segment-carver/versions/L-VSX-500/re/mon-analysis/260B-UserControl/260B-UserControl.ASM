; ============================================================================
; MON 260B (octal) USCNT / UserControl - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. Earlier folders modelled MON dispatch through a fictional
; "GOTAB" monitor-call table and treated the worker as name-only / not analysable.
; Monitor calls are dispatched through MCTAB @005620B (segment 044-S3IDPIT).
; MCTAB[260B] = 047120B = USCNT, and that worker IS carved - in 003-S3CP
; (load 30000B), heading a user/system-accounting family USCNT / SYCNT / NRETM /
; NCHBU.
;
; Dispatch (all hops carved; see README):
;   MON 260B -> ENT14 072167B -> GOTAB[260B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[260B] @006100B = 047120B = USCNT -> worker (below).
;
; Worker home: 003-S3CP (load 30000B), symbol USCNT=047120B (SYMBOL-1-LIST).
; Addresses and values are octal.
; ============================================================================

; ---- MCTAB[260B] slot (044-S3IDPIT, load 4000B) ----
006100  047120  	; MCTAB[260B] = USCNT = 047120B   (dd: byteoff 2176 = 4e 50)

; ---- USCNT worker body (003-S3CP, load 30000B) ----
; Entry byte dd-verified: byteoff (047120-030000)*2 = 15520 -> 45 4d = 042515.
047120  042515  	MIN ,X ,B 115		; entry: increment a per-user counter
047121  020105  	STD 105
047122  051122  	LDT I 122
047123  047522  	LDA I ,B ,X 122
047124  023400  	STD I ,B ,X 0
047125  051531  	LDT I ,B 131		; SYCNT: system-side counter update
047126  051524  	LDT I ,B 124
047127  042515  	MIN ,X ,B 115
047130  020125  	STD 125
047131  047101  	LDA I ,X 101
047132  053101  	LDT I ,X 101
047133  044514  	LDA ,B 114
047134  040502  	MIN ,B 102		; NRETM
047135  046105  	LDA ,X 105
047136  022047  	STD ,X 47
047137  022125  	STD ,X 125
047140  051505  	LDT I ,B 105
047141  051040  	LDT I 40
047142  041122  	MIN I 122
047143  042501  	MIN ,X ,B 101
047144  045440  	LDA I ,B 40
047145  040524  	MIN ,B 124
047146  020047  	STD 47
047147  153000  	MON 0			; ExitFromProgram (0B) - taken on one branch
047150  022040  	STD ,X 40
047151  025052  	LDD I 52
047152  025040  	LDD I 40
047153  047104  	LDA I ,X 104
047154  026465  	LDD ,X ,B 65
047155  030060  	STF 60
047156  020125  	STD 125			; NCHBU
047157  051505  	LDT I ,B 105
047160  051040  	LDT I 40
047161  041122  	MIN I 122
047162  042501  	MIN ,X ,B 101
047163  045440  	LDA I ,B 40
047164  025052  	LDD I 52
047165  025044  	LDD I 44
047166  023400  	STD I ,B ,X 0
; 047167 = STSUP boundary (next symbol) follows.
