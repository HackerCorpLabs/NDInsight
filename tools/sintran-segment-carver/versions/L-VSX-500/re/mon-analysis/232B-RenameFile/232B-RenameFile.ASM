; ============================================================================
; MON 232B (octal) RenameFile - real SINTRAN L disassembly
; Three regions (the dispatch is 3-layer; see README Code location):
;   A. GOTAB[232] level-14 entry  F1723 @66172B  in segment 025-S3IRPIT (load 32000B)
;   B. MRNFI rename entry  MRNFI @106060B  in segment 006-S3FS (load 26000B)
;   C. shared MDLFI dispatcher body (joined at 106065)  in the same segment
;   (the A->B bridge is the uncarved resident CALLPROC - not in these bytes)
; Addresses and values are octal.
;
; Region A is the 4-word F1723 entry vectored from GOTAB[232]; it is an entry point
; into a shared 025-S3IRPIT level-14 handler (a device/segment table min/max scan)
; whose tail ends in EXIT at 66261B. It does NOT itself branch to the MRNFI worker -
; that transfer is the uncarved resident CALLPROC hop (see README Honest caveats).
;
; Region B/C: MRNFI (SSM=0, SSK=1) is a sibling entry of the shared directory-entry
; dispatcher (the same MDLFI body documented for MON 54B DeleteFile). It joins at
; 106065; the SSK/SSM fold (106072-106106) yields operation mode 1, and the mode
; ladder (106116-106170) calls the mode-1 worker 106145 JPL I 40 -> 106205 = MRENF
; (Modify REName File) at 117352.
;
; Words 106176-106211 are a POINTER TABLE (link cells), not code.
; ============================================================================

; ---- Region A: GOTAB[232] entry stub  F1723 (025-S3IRPIT, load 32000B) ----
;      (4 words, bounded to the next symbol F1724 = 66176B; part of the shared
;       level-14 handler that ends in EXIT at 66261B)
066172  004020  	STA 20			; F1723 entry
066173  000274  	STZ -104
066174  004250  	STA -130
066175  004246  	STA -132		; (F1724 = 66176 follows)

; ---- Region B: MRNFI rename entry (006-S3FS, load 26000B) ----
106060  174070  	BSET ZRO SSM		; MRNFI entry: SSM=0
106061  174220  	BSET ONE SSK		;              SSK=1   -> mode 1
106062  124003  	JMP 3		; -> 106065 (join shared dispatcher)
106063  174070  	BSET ZRO SSM		; (MDLFI sibling entry, SSM=0)
106064  174020  	BSET ZRO SSK		; (MDLFI sibling entry, SSK=0)

; ---- Region C: shared MDLFI dispatcher body (006-S3FS) ----
106065  021111  	STD I 111		; -> 106176 stash caller D
106066  146145  	RADD CLD SL DA		; A = L
106067  146131  	RADD CLD SB DD		; D = B
106070  170125  	SAB 125			; establish local frame (B), 125B words
106071  135106  	JPL I 106		; -> 106177 (link cell = 003752, resident prologue)
106072  175270  	BSKP ONE SSM		; skip if SSM==1 (MRNFI: SSM=0, no skip)
106073  124007  	JMP 7		; -> 106102
106074  175220  	BSKP ONE SSK
106075  124003  	JMP 3		; -> 106100
106076  170403  	SAA 3
106077  124002  	JMP 2		; -> 106101
106100  170402  	SAA 2
106101  124006  	JMP 6		; -> 106107
106102  175220  	BSKP ONE SSK		; skip if SSK==1 (MRNFI: SSK=1, skip)
106103  124003  	JMP 3		; -> 106106
106104  170401  	SAA 1			; A = 1  (MRNFI => mode 1)
106105  124002  	JMP 2		; -> 106107
106106  146105  	RADD CLD 0 DA		; A = 0
106107  004523  	STA ,B 123		; B+123 = mode; MRNFI => 1
106110  044070  	LDA 70
106111  146035  	RADD SB DA
106112  050067  	LDT 67
106113  054400  	LDX ,B 0
106114  135066  	JPL I 66		; -> 106202 (link cell = 031075, directory lookup)
106115  124057  	JMP 57		; -> 106174 (store-status exit)
106116  044523  	LDA ,B 123		; mode
106117  171001  	SAT 1
106120  140065  	SKP IF DA EQL ST	; mode == 1 ?  (RenameFile)
106121  124007  	JMP 7		; -> 106130
106122  044061  	LDA 61
106123  146035  	RADD SB DA
106124  171026  	SAT 26
106125  054402  	LDX ,B 2
106126  135054  	JPL I 54		; -> 106202 (link cell = 031075, directory lookup)
106127  124045  	JMP 45		; -> 106174 (store-status exit)
106130  054050  	LDX 50
106131  146037  	RADD SB DX
106132  044523  	LDA ,B 123		; mode
106133  131404  	JAF 4		; -> 106137 (mode != 0)
106134  135050  	JPL I 50		; -> 106204 (link cell = 120067, MODLF) mode 0
106135  124037  	JMP 37		; -> 106174 (store-status exit)
106136  124033  	JMP 33		; -> 106171 (success)
106137  044523  	LDA ,B 123		; mode
106140  171001  	SAT 1
106141  140065  	SKP IF DA EQL ST	; mode == 1 ?
106142  124006  	JMP 6		; -> 106150
106143  044040  	LDA 40
106144  146035  	RADD SB DA
106145  135040  	JPL I 40		; -> 106205 (link cell = 117352, MRENF rename) mode 1 <- RenameFile
106146  124026  	JMP 26		; -> 106174 (store-status exit)
106147  124022  	JMP 22		; -> 106171 (success)
106150  044523  	LDA ,B 123		; mode
106151  171002  	SAT 2
106152  140065  	SKP IF DA EQL ST	; mode == 2 ?
106153  124005  	JMP 5		; -> 106160
106154  044402  	LDA ,B 2
106155  135031  	JPL I 31		; -> 106206 (link cell = 120470, MSPER) mode 2
106156  124016  	JMP 16		; -> 106174 (store-status exit)
106157  124012  	JMP 12		; -> 106171 (success)
106160  044523  	LDA ,B 123		; mode
106161  171003  	SAT 3
106162  140065  	SKP IF DA EQL ST	; mode == 3 ?
106163  124004  	JMP 4		; -> 106167
106164  135023  	JPL I 23		; -> 106207 (link cell = 120236, MSTMP) mode 3
106165  124007  	JMP 7		; -> 106174 (store-status exit)
106166  124003  	JMP 3		; -> 106171 (success)
106167  135021  	JPL I 21		; -> 106210 (link cell = 120233, MSTRM) tail
106170  124004  	JMP 4		; -> 106174 (store-status exit)
; ---- success exit + status return ----
106171  040404  	MIN ,B 4		; bump status
106172  170653  	SAA -125
106173  125016  	JMP I 16		; -> 106211 (link cell = 003776, resident return)
106174  004402  	STA ,B 2		; store status word -> caller slot B+2
106175  124375  	JMP -3		; -> 106172
; ---- link cells (DATA, not code) 106176..106211 ----
106176  147417  	ROP NOOP		; (data) pad
106177  003752  	STZ I ,B ,X -26		; (data) link -> 003752 (resident prologue)
106200  000021  	STZ 21			; (data)
106201  000203  	STZ -175		; (data)
106202  031075  	STF I 75		; (data) link -> 031075 (directory lookup)
106203  000006  	STZ 6			; (data)
106204  120067  	MPY 67			; (data) link -> 120067 (MODLF)
106205  117352  	FDV I ,X -26		; (data) link -> 117352 (MRENF rename) <- RenameFile
106206  120470  	MPY ,B 70		; (data) link -> 120470 (MSPER)
106207  120236  	MPY -142		; (data) link -> 120236 (MSTMP)
106210  120233  	MPY -145		; (data) link -> 120233 (MSTRM)
106211  003776  	STZ I ,B ,X -2		; (data) link -> 003776 (resident return)
