; ============================================================================
; MON 236B (octal) SetPermanentOpen - real SINTRAN L disassembly
;
; MON 236B has a NON-ZERO GOTAB vector: GOTAB[236] = 066202B (byte-proven),
; which lands on symbol F1725 inside the resident RPIT image 025-S3IRPIT
; (load 32000B). This is the same F17xx GOTAB-stub cluster used by 254B
; (F1734=066246). The functional worker is SETPO in 006-S3FS (load 26000B).
;
; Two labelled regions:
;   A. GOTAB[236] dispatch stub  F1725 @066202B in segment 025-S3IRPIT (RPIT image)
;      Real bytes, but a fragment of resident dispatch code (the routine
;      continues past the next symbol F1726=066206B); it does NOT contain a
;      static pointer to SETPO - the stub -> worker hop is the uncarved
;      resident CALLPROC bridge (see README Honest caveats).
;   B. SETPO SetPermanentOpen worker  SETPO @072465B in segment 006-S3FS
;      Window 072465B..072617B = 89 words (up to but not including SMAXB=072620B).
;      The file must already be open: the body scans the open-file table for the
;      caller's file number and SETS a permanent-open bit in the file entry
;      (072576: BSET ONE bit15). It does NOT call FOPEN - nothing is opened here.
;
; Words 072605B..072617B are a POINTER TABLE (link cells), not code; nd100-dis
; renders them as bogus instructions. Their contents are the real worker
; addresses (resolved in trailing comments). Addresses/values octal.
; ============================================================================

; ---- Region A: GOTAB[236] dispatch stub F1725 (025-S3IRPIT, load 32000B) ----
; Fragment only (bounded by next symbol F1726=066206B); resident dispatch code.
066202  171026  	SAT 26			; F1725 (= GOTAB[236] target)
066203  141660  	RDIV ST
066204  121057  	MPY I 57
066205  120057  	MPY 57			; (routine continues past F1726=066206B, uncarved)

; ---- Region B: SETPO SetPermanentOpen worker (006-S3FS, load 26000B) ----
072465  021120  	STD I 120		; SETPO entry: stash caller D
072466  146145  	RADD CLD SL DA
072467  146131  	RADD CLD SB DD
072470  170007  	SAB 7			; small local frame (B), 7 words
072471  135115  	JPL I 115		; -> 072606 (link cell = 003752, resident prologue worker)
072472  050402  	LDT ,B 2		; T = caller file number (stashed at B+2)
072473  170500  	SAA 100			; A = 100 (open-file-table range check limit)
072474  141056  	SKP IF DT GRE SA
072475  124004  	JMP 4			; -> 072501
072476  061111  	ADD I 111
072477  141056  	SKP IF DT GRE SA
072500  124056  	JMP 56			; -> 072556 (out-of-range -> error)
072501  054107  	LDX 107			; X = open-file-table base
072502  046000  	LDA ,X 0		; scan loop: read table entry
072503  131403  	JAF 3			; -> 072506
072504  046001  	LDA ,X 1
072505  131044  	JAZ 44			; -> 072551
072506  014406  	STX ,B 6		; save table cursor
072507  146157  	RADD CLD SA DX
072510  046003  	LDA ,X 3
072511  175365  	BSKP ONE 160 DA		; test entry flag bit (160>>3 = bit 14)
072512  124015  	JMP 15			; -> 072527
072513  046007  	LDA ,X 7
072514  175305  	BSKP ONE 100 DA		; test entry flag bit (100>>3 = bit 8)
072515  124012  	JMP 12			; -> 072527
072516  046015  	LDA ,X 15
072517  060072  	ADD 72
072520  050402  	LDT ,B 2
072521  140065  	SKP IF DA EQL ST	; does this entry match the caller file number?
072522  124004  	JMP 4			; -> 072526
072523  054406  	LDX ,B 6
072524  026000  	LDD ,X 0
072525  124033  	JMP 33			; -> 072560 (match -> mark permanent)
072526  124022  	JMP 22			; -> 072550
072527  135063  	JPL I 63		; -> 072612 (link cell)
072530  135063  	JPL I 63		; -> 072613 (link cell)
072531  124052  	JMP 52			; -> 072603 (store-status exit)
072532  046044  	LDA ,X 44
072533  175005  	BSKP ZRO 0 DA
072534  124003  	JMP 3			; -> 072537
072535  175215  	BSKP ONE 10 DA
072536  124011  	JMP 11			; -> 072547
072537  046045  	LDA ,X 45
072540  050402  	LDT ,B 2
072541  140065  	SKP IF DA EQL ST
072542  124005  	JMP 5			; -> 072547
072543  135051  	JPL I 51		; -> 072614 (link cell)
072544  054406  	LDX ,B 6
072545  026000  	LDD ,X 0
072546  124012  	JMP 12			; -> 072560
072547  135045  	JPL I 45		; -> 072614 (link cell)
072550  054406  	LDX ,B 6
072551  173402  	AAX 2			; advance to next table entry
072552  050043  	LDT 43
072553  140067  	SKP IF DX EQL ST	; end of table?
072554  124326  	JMP -52			; -> 072502 (loop back)
072555  124023  	JMP 23			; -> 072600
072556  044402  	LDA ,B 2
072557  135037  	JPL I 37		; -> 072616 (link cell)
072560  131405  	JAF 5			; -> 072565 (match path: mark permanent)
072561  140001  	SKP IF DD EQL 0
072562  124003  	JMP 3			; -> 072565
072563  170532  	SAA 132			; error 132
072564  124017  	JMP 17			; -> 072603 (store-status exit)
072565  131402  	JAF 2			; -> 072567
072566  146115  	RADD CLD SD DA
072567  146157  	RADD CLD SA DX		; X = file-entry address
072570  046003  	LDA ,X 3
072571  175165  	BSKP ZRO 160 DA		; check entry flag (160>>3 = bit 14)
072572  124003  	JMP 3			; -> 072575
072573  170533  	SAA 133			; error 133
072574  124007  	JMP 7			; -> 072603 (store-status exit)
072575  046007  	LDA ,X 7		; load file-entry word 7
072576  174375  	BSET ONE 170 DA		; SET permanent-open bit (170>>3 = bit 15)
072577  006007  	STA ,X 7		; write it back -> file entry marked permanent
072600  040404  	MIN ,B 4		; success flag++
072601  170771  	SAA -7
072602  125015  	JMP I 15		; -> 072617 (link cell = 003776, resident return)
072603  004402  	STA ,B 2		; store status word -> caller slot B+2
072604  124375  	JMP -3			; -> 072601 (fall to resident return)
; ---- link cells (DATA, not code) 072605..072617 ----
072605  147417  	ROP NOOP		; (data) padding
072606  003752  	STZ I ,B ,X -26		; (data) link -> 003752 (resident prologue worker)
072607  147514  	ROP NOOP		; (data)
072610  147515  	ROP NOOP		; (data)
072611  003100  	STZ I ,X 100		; (data) link -> 003100
072612  071413  	AND I ,B 13		; (data) link -> 071413
072613  055566  	LDX I ,B 166		; (data) link -> 055566
072614  056307  	LDX ,X -71		; (data) link -> 056307
072615  147715  	ROP NOOP		; (data)
072616  010376  	STT -2			; (data) link -> 010376
072617  003776  	STZ I ,B ,X -2		; (data) link -> 003776 (resident return)
