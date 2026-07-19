; ============================================================================
; MON 257B (octal) OpenFileInfo - real SINTRAN L disassembly
;
; MON 257B is a FALL-THROUGH call: GOTAB[257] = 000000 (byte-proven), so there
; is NO per-call level-14 entry stub to list. Dispatch drops into the resident
; MFELL/CALLPROC second-level path (uncarved) which reaches the worker below.
; Hence a single labelled region:
;   B. FOPEN open-file-info worker body   FOPEN @67432B  in segment 006-S3FS (load 26000B)
;      Window 67432B..67571B = 96 words (up to but not including SPCLO=67572B).
;      The body looks up an open file by name/type and returns its file number
;      and access; it JPL-calls FCON (@67002B) - a file-system connect/context
;      helper (byte-resolved link cell 67565B).
;   (the fall-through -> FOPEN bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; Words 67561B..67571B are a POINTER TABLE (link cells), not code; nd100-dis
; renders them as bogus ROP/STZ/SUB instructions. Their contents are the real
; worker addresses (resolved in the trailing comments and README walkthrough).
; Addresses and values are octal.
; ============================================================================

; ---- Region B: FOPEN open-file-info worker (006-S3FS, load 26000B) ----
067432  021127  	STD I 127		; FOPEN entry: stash caller D (params)
067433  146145  	RADD CLD SL DA
067434  146131  	RADD CLD SB DD
067435  170006  	SAB 6			; establish local frame (B), 6 words
067436  135124  	JPL I 124		; -> 067562 (link cell = 003752, resident prologue worker)
067437  054124  	LDX 124
067440  170500  	SAA 100
067441  146151  	RADD CLD SA DD
067442  046000  	LDA ,X 0		; open-file-table scan: read table entry
067443  131403  	JAF 3			; -> 067446
067444  046001  	LDA ,X 1
067445  131012  	JAZ 12			; -> 067457 (empty slot -> match/return path)
067446  173402  	AAX 2
067447  050115  	LDT 115
067450  140067  	SKP IF DX EQL ST
067451  124004  	JMP 4			; -> 067455
067452  170522  	SAA 122			; error 122 (table exhausted / not found)
067453  124104  	JMP 104			; -> 067557 (store-status exit)
067454  124003  	JMP 3			; -> 067457
067455  146401  	RADD AD1 0 DD
067456  124364  	JMP -14			; -> 067442 (loop next table entry)
067457  050401  	LDT ,B 1		; matched: fetch caller name/type args
067460  044402  	LDA ,B 2
067461  054400  	LDX ,B 0
067462  135103  	JPL I 103		; -> 067565 (link cell = 067002, FCON connect/context helper)
067463  124074  	JMP 74			; -> 067557 (store-status exit)
067464  170500  	SAA 100
067465  141051  	SKP IF DD GRE SA
067466  124064  	JMP 64			; -> 067552
067467  061077  	ADD I 77
067470  143051  	SKP IF DD LST SA
067471  124061  	JMP 61			; -> 067552
067472  146115  	RADD CLD SD DA
067473  172700  	AAA -100
067474  120073  	MPY 73
067475  060066  	ADD 66
067476  146157  	RADD CLD SA DX
067477  056001  	LDX ,X 1
067500  133052  	JXZ 52			; -> 067552
067501  052006  	LDT ,X 6
067502  175226  	BSKP ONE 20 DT
067503  124047  	JMP 47			; -> 067552
067504  054064  	LDX 64
067505  052000  	LDT ,X 0
067506  140006  	SKP IF DT EQL 0
067507  124017  	JMP 17			; -> 067526
067510  052001  	LDT ,X 1
067511  140006  	SKP IF DT EQL 0
067512  124014  	JMP 14			; -> 067526
067513  146176  	RADD CLD SX DT
067514  146157  	RADD CLD SA DX
067515  026000  	LDD ,X 0
067516  002000  	STZ ,X 0
067517  002001  	STZ ,X 1
067520  146167  	RADD CLD ST DX
067521  022000  	STD ,X 0
067522  170520  	SAA 120			; access/result code 120
067523  004402  	STA ,B 2		; store status word -> caller slot B+2
067524  040404  	MIN ,B 4
067525  124030  	JMP 30			; -> 067555 (return)
067526  054042  	LDX 42
067527  173402  	AAX 2
067530  052000  	LDT ,X 0
067531  140006  	SKP IF DT EQL 0
067532  124020  	JMP 20			; -> 067552
067533  052001  	LDT ,X 1
067534  140006  	SKP IF DT EQL 0
067535  124015  	JMP 15			; -> 067552
067536  146176  	RADD CLD SX DT
067537  146157  	RADD CLD SA DX
067540  026000  	LDD ,X 0
067541  002000  	STZ ,X 0
067542  002001  	STZ ,X 1
067543  146167  	RADD CLD ST DX
067544  022000  	STD ,X 0
067545  170520  	SAA 120			; access/result code 120
067546  172401  	AAA 1
067547  004402  	STA ,B 2		; store status word -> caller slot B+2
067550  040404  	MIN ,B 4
067551  124004  	JMP 4			; -> 067555 (return)
067552  146115  	RADD CLD SD DA
067553  004402  	STA ,B 2		; store status word -> caller slot B+2
067554  040404  	MIN ,B 4
067555  170772  	SAA -6			; return leader
067556  125013  	JMP I 13		; -> 067571 (link cell = 003776, resident return)
067557  004402  	STA ,B 2		; store status word -> caller slot B+2 (error path)
067560  124375  	JMP -3			; -> 067555 (fall to resident return)
; ---- link cells (DATA, not code) 067561..067571 ----
067561  147417  	ROP NOOP		; (data) padding
067562  003752  	STZ I ,B ,X -26		; (data) link -> 003752 (resident prologue worker)
067563  147515  	ROP NOOP		; (data)
067564  147715  	ROP NOOP		; (data)
067565  067002  	SUB I ,X 2		; (data) link -> 067002 (FCON connect/context helper)
067566  147514  	ROP NOOP		; (data)
067567  000002  	STZ 2			; (data)
067570  147555  	ROP NOOP		; (data)
067571  003776  	STZ I ,B ,X -2		; (data) link -> 003776 (resident return)
