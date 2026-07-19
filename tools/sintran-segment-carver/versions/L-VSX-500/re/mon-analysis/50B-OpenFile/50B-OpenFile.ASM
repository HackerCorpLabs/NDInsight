; ============================================================================
; MON 50B (octal) OpenFile - real SINTRAN L disassembly
;
; MON 50B is a FALL-THROUGH call: GOTAB[50] = 000000 (byte-proven), so there
; is NO per-call level-14 entry stub to list. Dispatch drops into the resident
; MFELL/CALLPROC second-level path (uncarved) which reaches the worker below.
; Hence a single labelled region:
;   B. OPENF file-open worker body   OPENF @123525B  in segment 006-S3FS (load 26000B)
;      Window 123525B..123637B = 113 words (up to but not including CONNF=123640B).
;      The body opens a file: it JPL-calls FOPEN (@67432B) - the file-open
;      primitive - which is the byte-level reason OPENF is the OpenFile worker.
;   (the fall-through -> OPENF bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; Words 123620B..123637B are a POINTER TABLE (link cells), not code; nd100-dis
; renders them as bogus ROP/STZ/SUB instructions. Their contents are the real
; worker addresses (resolved in the trailing comments and README walkthrough).
; Addresses and values are octal.
; ============================================================================

; ---- Region B: OPENF file-open worker (006-S3FS, load 26000B) ----
123525  021073  	STD I 73		; OPENF entry: stash caller D (params)
123526  146145  	RADD CLD SL DA
123527  146131  	RADD CLD SB DD
123530  170110  	SAB 110			; establish local frame (B), 110 words
123531  135070  	JPL I 70		; -> 123621 (link cell = 003752, resident prologue worker)
123532  044070  	LDA 70
123533  054070  	LDX 70
123534  146037  	RADD SB DX
123535  050067  	LDT 67
123536  135067  	JPL I 67		; -> 123625 (link cell = 044777, CLPAR parse)
123537  124053  	JMP 53		; -> 123612 (store-status exit)
123540  044066  	LDA 66
123541  135064  	JPL I 64		; -> 123625 (link cell = 044777, CLPAR parse)
123542  124050  	JMP 50		; -> 123612 (store-status exit)
123543  146151  	RADD CLD SA DD
123544  146107  	RADD CLD 0 DX
123545  171011  	SAT 11			; scan open-file table (max 11B=9? entries loop)
123546  141076  	SKP IF DT GRE SX
123547  124006  	JMP 6		; -> 123555
123550  047057  	LDA I ,X 57
123551  142015  	SKP IF DA UEQ SD
123552  124005  	JMP 5		; -> 123557 (match found)
123553  173401  	AAX 1
123554  124371  	JMP -7		; -> 123545 (loop)
123555  170504  	SAA 104			; error 104 (no free / not found)
123556  124034  	JMP 34		; -> 123612 (store-status exit)
123557  045051  	LDA I 51
123560  135051  	JPL I 51		; -> 123631 (link cell = 010500, resident worker)
123561  146176  	RADD CLD SX DT
123562  044050  	LDA 50
123563  054040  	LDX 40
123564  146037  	RADD SB DX
123565  135046  	JPL I 46		; -> 123633 (link cell = 067432, FOPEN file-open primitive)
123566  124026  	JMP 26		; -> 123614
123567  146151  	RADD CLD SA DD
123570  045040  	LDA I 40
123571  135043  	JPL I 43		; -> 123634 (link cell = 010506, resident worker)
123572  146115  	RADD CLD SD DA
123573  135042  	JPL I 42		; -> 123635 (link cell = 040730, OUTRC)
123574  043111  	MIN I ,X 111
123575  046105  	LDA ,X 105
123576  020116  	STD 116
123577  052515  	LDT ,X ,B 115
123600  041105  	MIN I 105
123601  051040  	LDT I 40
123602  044523  	LDA ,B 123
123603  020047  	STD 47
123604  135032  	JPL I 32		; -> 123636 (link cell = 040336, OCTAL)
123605  135030  	JPL I 30		; -> 123635 (link cell = 040730, OUTRC)
123606  022047  	STD ,X 47
123607  040404  	MIN ,B 4
123610  170670  	SAA -110		; error path leader
123611  125026  	JMP I 26		; -> 123637 (link cell = 003776, resident return)
123612  004402  	STA ,B 2		; store status word -> caller slot B+2
123613  124375  	JMP -3		; -> 123610 (fall to resident return)
123614  004402  	STA ,B 2		; store status word -> caller slot B+2
123615  045013  	LDA I 13
123616  135016  	JPL I 16		; -> 123634 (link cell = 010506, resident worker)
123617  124371  	JMP -7		; -> 123610 (fall to resident return)
; ---- link cells (DATA, not code) 123620..123637 ----
123620  147417  	ROP NOOP		; (data) padding
123621  003752  	STZ I ,B ,X -26		; (data) link -> 003752 (resident prologue worker)
123622  043465  	MIN I ,B ,X 65		; (data)
123623  000006  	STZ 6			; (data)
123624  000203  	STZ -175		; (data)
123625  044777  	LDA ,B -1		; (data) link -> 044777 (CLPAR parameter parse)
123626  043676  	MIN I ,B ,X -102	; (data)
123627  010060  	STT 60			; (data)
123630  146302  	RADD CM1 CLD 0 DP	; (data)
123631  010500  	STT ,B 100		; (data) link -> 010500 (resident worker)
123632  007772  	STA I ,B ,X -6		; (data)
123633  067432  	SUB I ,B ,X 32		; (data) link -> 067432 (FOPEN file-open primitive)
123634  010506  	STT ,B 106		; (data) link -> 010506 (resident worker)
123635  040730  	MIN ,B -50		; (data) link -> 040730 (OUTRC)
123636  040336  	MIN -42			; (data) link -> 040336 (OCTAL)
123637  003776  	STZ I ,B ,X -2		; (data) link -> 003776 (resident return)
