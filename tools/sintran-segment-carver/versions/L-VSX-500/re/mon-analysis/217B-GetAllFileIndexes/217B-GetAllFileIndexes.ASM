; ============================================================================
; MON 217B (octal) GetAllFileIndexes - real SINTRAN L disassembly
;
; MON 217B is a FALL-THROUGH call: GOTAB[217] = 000000 (byte-proven), so there
; is NO per-call level-14 entry stub to list. Dispatch drops into the resident
; MFELL/CALLPROC second-level path (uncarved) which reaches the worker below.
; Hence a single labelled region:
;   B. GUIOI file-index worker body   GUIOI @105432B  in segment 006-S3FS (load 26000B)
;      Window 105432B..105551B = 120 words (up to but not including SFACC=105552B).
;      The body reads an open file's index triple (directory / user / object
;      index) out of the open-file table: it JPL-calls FOPTB (@101043B) - the
;      File Open TaBle - which is the byte-level reason GUIOI is the
;      GetAllFileIndexes worker.
;   (the fall-through -> GUIOI bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; Words 105536B..105551B are a POINTER TABLE (link cells), not code; nd100-dis
; renders them as bogus ROP/STZ/... instructions. Their contents are the real
; worker addresses (resolved in the trailing comments and README walkthrough).
; Addresses and values are octal.
; ============================================================================

; ---- Region B: GUIOI file-index worker (006-S3FS, load 26000B) ----
105432  021104  	STD I 104		; GUIOI entry: stash caller D (file number)
105433  146145  	RADD CLD SL DA
105434  146131  	RADD CLD SB DD
105435  170037  	SAB 37			; establish local frame (B), 37 words
105436  135101  	JPL I 101		; -> 105537 (link cell = 003752, resident prologue worker)
105437  044402  	LDA ,B 2
105440  005100  	STA I 100
105441  135100  	JPL I 100		; -> 105541 (link cell = 010376, resident worker)
105442  131402  	JAF 2		; -> 105444
105443  146115  	RADD CLD SD DA
105444  146157  	RADD CLD SA DX
105445  131403  	JAF 3		; -> 105450
105446  170532  	SAA 132			; error 132 (bad file number)
105447  124065  	JMP 65		; -> 105534 (store-status exit)
105450  046003  	LDA ,X 3		; index the open-file table entry
105451  175165  	BSKP ZRO 160 DA
105452  124003  	JMP 3		; -> 105455
105453  135067  	JPL I 67		; -> 105542 (link cell = 101043, FOPTB file open table)
105454  124057  	JMP 57		; -> 105533 (error 133 exit)
105455  046007  	LDA ,X 7
105456  175305  	BSKP ONE 100 DA
105457  124033  	JMP 33		; -> 105512 (remote/other path)
105460  046003  	LDA ,X 3
105461  175365  	BSKP ONE 160 DA
105462  124030  	JMP 30		; -> 105512
105463  146176  	RADD CLD SX DT
105464  055057  	LDX I 57
105465  012024  	STT ,X 24
105466  044056  	LDA 56
105467  146035  	RADD SB DA
105470  006025  	STA ,X 25
105471  004436  	STA ,B 36
105472  044053  	LDA 53
105473  146035  	RADD SB DA
105474  006026  	STA ,X 26
105475  171416  	SAX 16
105476  135050  	JPL I 50		; -> 105546 (link cell = 020274, resident worker)
105477  124035  	JMP 35		; -> 105534 (store-status exit)
105500  044403  	LDA ,B 3
105501  054436  	LDX ,B 36
105502  171053  	SAT 53
105503  135044  	JPL I 44		; -> 105547 (link cell = 031072, SUCPS)
105504  124030  	JMP 30		; -> 105534 (store-status exit)
105505  044406  	LDA ,B 6
105506  004401  	STA ,B 1
105507  044407  	LDA ,B 7
105510  004400  	STA ,B 0
105511  124017  	JMP 17		; -> 105530 (success return)
105512  052015  	LDT ,X 15		; build the returned index triple
105513  156010  	SHT ZIN 10
105514  046016  	LDA ,X 16
105515  156570  	SHA ZIN SHR 10
105516  146056  	RADD SA DT
105517  010401  	STT ,B 1		; dir/user index -> B+1
105520  046006  	LDA ,X 6
105521  156564  	SHA ZIN SHR 14
105522  156410  	SHA ZIN 10
105523  146156  	RADD CLD SA DT
105524  046016  	LDA ,X 16
105525  070023  	AND 23
105526  146065  	RADD ST DA
105527  004400  	STA ,B 0		; object index -> B+0
105530  040404  	MIN ,B 4		; success return leader
105531  170741  	SAA -37
105532  125017  	JMP I 17		; -> 105551 (link cell = 003776, resident return)
105533  170533  	SAA 133			; error 133 (file not open / not found)
105534  004402  	STA ,B 2		; store status word -> caller slot B+2
105535  124374  	JMP -4		; -> 105531 (fall to resident return)
; ---- link cells (DATA, not code) 105536..105551 ----
105536  147417  	ROP NOOP		; (data) padding
105537  003752  	STZ I ,B ,X -26		; (data) link -> 003752 (resident prologue worker)
105540  147505  	ROP NOOP		; (data)
105541  010376  	STT -2			; (data) link -> 010376 (resident worker)
105542  101043  	FAD I 43		; (data) link -> 101043 (FOPTB file open table)
105543  147421  	ROP NOOP		; (data)
105544  000010  	STZ 10			; (data)
105545  000006  	STZ 6			; (data)
105546  020274  	STD -104		; (data) link -> 020274 (resident worker)
105547  031072  	STF I 72		; (data) link -> 031072 (SUCPS)
105550  000377  	STZ -1			; (data)
105551  003776  	STZ I ,B ,X -2		; (data) link -> 003776 (resident return)
