; ============================================================================
; MON 122B (octal) RESRV / ReserveResource - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. Earlier folders modelled MON dispatch through a fictional
; "GOTAB" monitor-call table and declared workers absent / not-carved (it read
; RESRV out of SINTRAN-DATA_commoncode, where 037103B is zero-filled - the WRONG
; overlay). Monitor calls are dispatched through MCTAB @005620B (segment
; 044-S3IDPIT). MCTAB[122B] = 037103B = RESRV, and that worker IS carved - in
; 003-S3CP (load 30000B), alongside its sibling RELES (037156B).
;
; Dispatch (all hops carved; see README):
;   MON 122B -> ENT14 072167B -> GOTAB[122B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[122B] @005742B = 037103B = RESRV -> worker (below).
;
; Worker home: 003-S3CP (load 30000B), symbol RESRV=037103B (SYMBOL-1-LIST).
; Addresses and values are octal.
; ============================================================================

; ---- MCTAB[122B] slot (044-S3IDPIT, load 4000B) ----
005742  037103  	; MCTAB[122B] = RESRV = 037103B   (dd: byteoff 1988 = 3e 43)

; ---- RESRV worker body (003-S3CP, load 30000B) ----
; Entry byte dd-verified: byteoff (037103-030000)*2 = 7302 -> 3d fe = 036776.
037103  036776  	LDF ,X ,B -2		; entry (register/context preamble)
037104  033456  	STF I ,B ,X 56
037105  037007  	LDF I ,X 7
037106  035351  	LDF I -27
037107  027417  	LDD I ,B ,X 17
037110  045053  	LDA I 53		; EXECC: load a flag word
037111  131402  	JAF 2			; -> 037113 (branch if flag false)
037112  146142  	EXIT			; early return
037113  146146  	RADD CLD SL DT		; T := L (save link)
037114  170402  	SAA 2
037115  004637  	STA ,B -141
037116  044046  	LDA 46
037117  146151  	RADD CLD SA DD		; D := A (command-string pointer)
037120  135045  	JPL I 45		; -> 037165 helper (build command line)
037121  146164  	RADD CLD ST DL		; L := T (restore link)
037122  044044  	LDA 44
037123  146151  	RADD CLD SA DD
037124  153070  	MON 70			; CallCommand (70B): run a RESERVE command
037125  146107  	RADD CLD 0 DX		; X := 0
037126  171177  	SAT 177			; byte-scan loop start: T := 177B
037127  141076  	SKP IF DT GRE SX
037130  124010  	JMP 10			; -> 037140
037131  146116  	RADD CLD SD DT
037132  142200  	LBYT			; read a byte from the string
037133  171047  	SAT 47			; compare it to 47B
037134  142065  	SKP IF DA UEQ ST
037135  124003  	JMP 3			; -> 037140
037136  173401  	AAX 1
037137  124367  	JMP -11			; -> 037126 (loop)
037140  146175  	RADD CLD SX DA
037141  172402  	AAA 2
037142  156577  	SHA ZIN SHR 1
037143  146015  	RADD SD DA
037144  146151  	RADD CLD SA DD
037145  146156  	RADD CLD SA DT
037146  146107  	RADD CLD 0 DX
037147  142200  	LBYT			; PRLS: second byte scan
037150  171047  	SAT 47
037151  142065  	SKP IF DA UEQ ST
037152  124004  	JMP 4			; -> 037156 (= RELES boundary)
037153  146115  	RADD CLD SD DA
037154  153070  	MON 70			; CallCommand (70B) again
037155  124350  	JMP -30			; -> 037125 (repeat)
; 037156 = RELES (MON 123B) boundary - next routine, not part of RESRV.
