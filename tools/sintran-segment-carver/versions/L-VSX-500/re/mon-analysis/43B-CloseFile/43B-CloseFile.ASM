; ============================================================================
; MON 43B (octal) CloseFile - real SINTRAN L disassembly
; Two regions (the dispatch is 3-layer; see README Code location):
;   A. GOTAB[43] level-14 entry stub  F1630 @121050B  in segment 025-S3IRPIT (load 32000B)
;   B. CLOSF file-close worker body    CLOSF @123741B  in segment 006-S3FS   (load 26000B)
;   (the A->B bridge is the uncarved resident CALLPROC - not in these bytes)
;
; Region A is bounded strictly to the next symbol F1631=121075B (21 words). Two
; direct branches leave the stub into shared level-14 tail code in sibling F16xx
; stubs (121162B in F1633's area, 121201B in F1634's area) - shared code, NOT the
; CLOSF worker, and NOT swallowed here (see README Honest caveats).
;
; Region B: words 123767B..123777B are a POINTER TABLE (link cells), not code;
; nd100-dis renders them as bogus ROP/STZ/SUB instructions. Their contents are the
; real worker addresses (resolved in trailing comments). CLOSF calls FCLOS
; (@67612B) - the file-close primitive - which is the byte-level reason CLOSF is
; the CloseFile worker.
; Addresses and values are octal.
; ============================================================================

; ---- Region A: GOTAB[43] entry stub (025-S3IRPIT, load 32000B) ----
121050  146755  	RADD AD1 CM1 CLD SA DA
121051  172777  	AAA -1
121052  131402  	JAF 2		; -> 121054
121053  124003  	JMP 3		; -> 121056
121054  124106  	JMP 106		; -> 121162 (shared level-14 tail in sibling stub; uncarved link)
121055  124017  	JMP 17		; -> 121074
121056  135123  	JPL I 123		; -> 121201 (shared level-14 routine in sibling stub)
121057  124016  	JMP 16		; -> 121075 (F1631, next stub)
121060  171401  	SAX 1
121061  140075  	SKP IF DA EQL SX
121062  124010  	JMP 10		; -> 121072
121063  140006  	SKP IF DT EQL 0
121064  124003  	JMP 3		; -> 121067
121065  124371  	JMP -7		; -> 121056
121066  124003  	JMP 3		; -> 121071
121067  146165  	RADD CLD ST DA
121070  124072  	JMP 72		; -> 121162 (shared level-14 tail in sibling stub; uncarved link)
121071  124003  	JMP 3		; -> 121074
121072  170403  	SAA 3			; error 3
121073  124002  	JMP 2		; -> 121075 (F1631, next stub)
121074  124362  	JMP -16		; -> 121056

; ---- Region B: CLOSF file-close worker (006-S3FS, load 26000B) ----
123741  021026  	STD I 26		; CLOSF entry: stash caller D (file number)
123742  146145  	RADD CLD SL DA
123743  146131  	RADD CLD SB DD
123744  170006  	SAB 6			; establish local frame (B), 6 words
123745  135023  	JPL I 23		; -> 123770 (link cell = 003752, resident prologue worker)
123746  044023  	LDA 23
123747  135023  	JPL I 23		; -> 123772 (link cell = 044777, CLPAR parse)
123750  124013  	JMP 13		; -> 123763 (store-status exit)
123751  045022  	LDA I 22
123752  135022  	JPL I 22		; -> 123774 (link cell = 010500, resident worker)
123753  146116  	RADD CLD SD DT
123754  135021  	JPL I 21		; -> 123775 (link cell = 067612, FCLOS file-close primitive)
123755  124010  	JMP 10		; -> 123765
123756  040404  	MIN ,B 4
123757  045014  	LDA I 14
123760  135016  	JPL I 16		; -> 123776 (link cell = 010506, resident worker)
123761  170772  	SAA -6			; error path leader
123762  125015  	JMP I 15		; -> 123777 (link cell = 003776, resident return)
123763  004402  	STA ,B 2		; store status word -> caller slot B+2
123764  124375  	JMP -3		; -> 123761 (fall to resident return)
123765  004402  	STA ,B 2		; store status word -> caller slot B+2
123766  124371  	JMP -7		; -> 123757
; ---- link cells (DATA, not code) 123767..123777 ----
123767  147417  	ROP NOOP		; (data) padding
123770  003752  	STZ I ,B ,X -26		; (data) link -> 003752 (resident prologue worker)
123771  043722  	MIN I ,B ,X -56		; (data)
123772  044777  	LDA ,B -1		; (data) link -> 044777 (CLPAR parameter parse)
123773  146302  	RADD CM1 CLD 0 DP	; (data)
123774  010500  	STT ,B 100		; (data) link -> 010500 (resident worker)
123775  067612  	SUB I ,B ,X -166	; (data) link -> 067612 (FCLOS file-close primitive)
123776  010506  	STT ,B 106		; (data) link -> 010506 (resident worker)
123777  003776  	STZ I ,B ,X -2		; (data) link -> 003776 (resident return)
