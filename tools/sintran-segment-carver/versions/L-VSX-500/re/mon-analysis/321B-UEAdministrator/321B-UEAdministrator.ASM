; ============================================================================
; MON 321B (octal) UEADM / UEAdministrator - real SINTRAN L07 disassembly
;
; CORRECTED 2026-07-13. The previous version of this folder declared the worker
; "not carved / zero-filled" - it looked at 065453B in SINTRAN-DATA_commoncode
; (which is zeros there) and gave up. That was the wrong overlay. The real worker
; is carved in 003-S3CP, the SAME segment as UEADM's User-Environment siblings
; UECOM (050701B) / UELOG (050726B) / SETOL (050666B).
;
; Dispatch (all hops carved; see README):
;   MON 321B -> ENT14 072167B -> GOTAB[321B]=MFELL 072114B -> level switch to
;   CALLP 032201B -> MCTAB[321B] @006141B = 065453B = UEADM -> worker (below).
;
; Worker home: 003-S3CP (load 30000B), symbol UEADM=065453B (SYMBOL-1-LIST).
; Addresses and values are octal.
; ============================================================================

; ---- MCTAB[321B] slot (044-S3IDPIT, load 4000B) ----
006141  065453  	; MCTAB[321B] = UEADM = 065453B   (dd: byteoff 2242 = 153 053)

; ---- UEADM worker body (003-S3CP, load 30000B) ----
; Entry byte dd-verified: byteoff (065453-030000)*2 = 30294 -> cc 5f = 146137.
065453  146137  	RADD CLD SB DX		; X := B (frame/param base)
065454  044137  	LDA 137
065455  146153  	RADD CLD SA DB
065456  014612  	STX ,B -166		; save the param base in the frame
065457  146145  	RADD CLD SL DA		; A := L (return link)
065460  004611  	STA ,B -167		; save the return link
065461  046012  	LDA ,X 12		; A := param[12] (a sub-function selector)
065462  171001  	SAT 1
065463  141065  	SKP IF DA GRE ST	; selector range-check (low bound)
065464  124004  	JMP 4			; -> 065470
065465  171010  	SAT 10
065466  143056  	SKP IF DT LST SA	; selector range-check (high bound)
065467  124005  	JMP 5			; -> 065474
065470  044124  	LDA 124			; out-of-range -> error code
065471  006012  	STA ,X 12
065472  125123  	JMP I 123		; -> error/return path (@065615)
065473  124012  	JMP 12			; -> 065505
065474  004610  	STA ,B -170		; in-range: stash the selector
065475  046010  	LDA ,X 10
065476  004602  	STA ,B -176
065477  052044  	LDT ,X 44
065500  010600  	STT ,B -200
065501  046032  	LDA ,X 32
065502  055114  	LDX I 114
065503  006021  	STA ,X 21
065504  054612  	LDX ,B -166		; restore the param base
065505  044112  	LDA 112
065506  146106  	RADD CLD 0 DT
065507  135111  	JPL I 111		; -> helper @065620 (user-table access)
065510  144400  	RAND 0 0
065511  044610  	LDA ,B -170		; reload the selector
065512  171010  	SAT 10
065513  142065  	SKP IF DA UEQ ST
065514  125105  	JMP I 105		; selector==10 -> return path (@065621)
065515  046017  	LDA ,X 17
065516  156571  	SHA ZIN SHR 7		; extract a bit-field (>>7)
065517  070103  	AND 103
065520  004605  	STA ,B -173
065521  046017  	LDA ,X 17
065522  156565  	SHA ZIN SHR 13		; extract a bit-field (>>11 dec)
065523  070077  	AND 77
065524  004606  	STA ,B -172
065525  044600  	LDA ,B -200
065526  004601  	STA ,B -177
065527  120074  	MPY 74			; index * entry-size
065530  061074  	ADD I 74
065531  146157  	RADD CLD SA DX
065532  051073  	LDT I 73
065533  143330  	LDATX 3			; physical read of a user-table entry
065534  050072  	LDT 72
; --- internal label MLSTO=065535B; the code flows straight through it ---
065535  140065  	SKP IF DA EQL ST
065536  124010  	JMP 10			; -> 065546
065537  044605  	LDA ,B -173
065540  050606  	LDT ,B -172
065541  142065  	SKP IF DA UEQ ST
065542  124004  	JMP 4			; -> 065546
065543  044601  	LDA ,B -177
065544  174375  	BSET ONE 170 DA
065545  004601  	STA ,B -177
065546  000603  	STZ ,B -175
065547  054612  	LDX ,B -166
065550  046011  	LDA ,X 11
065551  004606  	STA ,B -172
065552  171001  	SAT 1
065553  140065  	SKP IF DA EQL ST
065554  124002  	JMP 2			; -> 065556
065555  124023  	JMP 23			; -> 065600
065556  131404  	JAF 4			; -> 065562
065557  170401  	SAA 1
065560  004606  	STA ,B -172
065561  124017  	JMP 17			; -> 065600
065562  135045  	JPL I 45		; -> helper @065627
065563  131402  	JAF 2			; -> 065565
065564  146115  	RADD CLD SD DA
065565  131403  	JAF 3			; -> 065570
065566  170574  	SAA 174
065567  125041  	JMP I 41		; -> 065630
065570  146157  	RADD CLD SA DX
065571  135040  	JPL I 40		; -> 065631
065572  125040  	JMP I 40		; -> 065632
065573  050040  	LDT 40
065574  135040  	JPL I 40		; -> 065634
065575  146157  	RADD CLD SA DX
065576  046012  	LDA ,X 12
065577  004600  	STA ,B -200
065600  044610  	LDA ,B -170
065601  146052  	RADD SA DP		; computed dispatch (A -> P): a jump table
065602  124037  	JMP 37			; -> 065641
065603  125032  	JMP I 32		; -> 065635
065604  125032  	JMP I 32		; -> 065636
065605  124034  	JMP 34			; -> 065641
065606  124033  	JMP 33			; -> 065641
065607  125030  	JMP I 30		; -> 065637
065610  125030  	JMP I 30		; -> 065640
065611  124136  	JMP 136			; -> 065747
065612  125007  	JMP I 7			; -> 065621

; ---- pointer/vector table for the JPL I / JMP I above (data words) ----
065613  144200  	; (data)
065614  000201  	; (data)
065615  066647  	; ptr  (target of JMP I 123 @065472)
065616  004007  	; (data)
065617  066441  	; ptr
065620  030130  	; ptr  (target of JPL I 111 @065507)
065621  066552  	; ptr  (target of JMP I 105 / JMP I 7)
065622  000017  	; (data)
065623  000010  	; (data)
065624  004321  	; (data)
065625  004320  	; (data)
065626  000200  	; (data)
065627  010376  	; ptr  (target of JPL I 45 @065562)
065630  066637  	; ptr
065631  037572  	; ptr
065632  066642  	; ptr
065633  000024  	; (data)
065634  004475  	; ptr
065635  066455  	; ptr
065636  066332  	; ptr
065637  066413  	; ptr
065640  066521  	; ptr
065641  050601  	LDT ,B -177		; common tail: reload saved regs and return
065642  054602  	LDX ,B -176
