; ============================================================================
; MON 233B (octal) SetTemporaryFile - real SINTRAN L disassembly
; Two labelled regions (dispatch is 3-layer; GOTAB[233] = 000000 = fall-through,
; no stub):
;   A. SETTF set-temporary entry   SETTF @106043B  in segment 006-S3FS (load 26000B)
;   B. shared MDLFI dispatcher body (joined at 106107)  in the same segment
;   (the MON 233 -> worker bridge is the uncarved resident MFELL/CALLPROC - not in
;    these bytes; there is no per-call GOTAB entry stub because the slot is zero)
; Addresses and values are octal.
;
; SETTF presets the operation mode directly (SAA 4) and jumps into the shared
; directory-entry dispatcher at 106107 (the STA ,B 123 mode-store), bypassing the
; SSK/SSM fold that the DeleteFile-family entries use. That shared body is the same
; MDLFI dispatcher documented for MON 54B DeleteFile and MON 232B RenameFile; its
; mode ladder (106116-106170) dispatches through the link cells at 106204-106210
; to workers MODLF/MRENF/MSPER/MSTMP/MSTRM. Mode 4 matches none of the ladder's
; 0..3 tests, so it falls to the tail worker 106167 JPL I 21 -> 106210 = MSTRM
; (120233).
;
; Words 106176-106211 are a POINTER TABLE (link cells), not code.
; ============================================================================

; ---- Region A: SETTF set-temporary entry (006-S3FS, load 26000B) ----
106043  021133  	STD I 133		; -> 106176 stash caller D
106044  146145  	RADD CLD SL DA		; A = L
106045  146131  	RADD CLD SB DD		; D = B
106046  170125  	SAB 125			; establish local frame (B), 125B words
106047  135130  	JPL I 130		; -> 106177 (link cell = 003752, resident prologue)
106050  170404  	SAA 4			; A = 4  (operation mode = 4)
106051  124036  	JMP 36		; -> 106107 (join shared dispatcher at mode-store)

; ---- Region B: shared MDLFI dispatcher body (006-S3FS) ----
;      (106065-106106 is the SSK/SSM fold used by the DeleteFile-family entries;
;       SETTF bypasses it and joins at 106107 with mode already = 4)
106065  021111  	STD I 111		; (MDLFI-family entry path) stash caller D
106066  146145  	RADD CLD SL DA
106067  146131  	RADD CLD SB DD
106070  170125  	SAB 125
106071  135106  	JPL I 106		; -> 106177 (link cell = 003752, resident prologue)
106072  175270  	BSKP ONE SSM
106073  124007  	JMP 7		; -> 106102
106074  175220  	BSKP ONE SSK
106075  124003  	JMP 3		; -> 106100
106076  170403  	SAA 3
106077  124002  	JMP 2		; -> 106101
106100  170402  	SAA 2
106101  124006  	JMP 6		; -> 106107
106102  175220  	BSKP ONE SSK
106103  124003  	JMP 3		; -> 106106
106104  170401  	SAA 1
106105  124002  	JMP 2		; -> 106107
106106  146105  	RADD CLD 0 DA		; A = 0
106107  004523  	STA ,B 123		; <- SETTF joins here; B+123 = mode (SETTF => 4)
106110  044070  	LDA 70
106111  146035  	RADD SB DA
106112  050067  	LDT 67
106113  054400  	LDX ,B 0
106114  135066  	JPL I 66		; -> 106202 (link cell = 031075, directory lookup)
106115  124057  	JMP 57		; -> 106174 (store-status exit)
106116  044523  	LDA ,B 123		; mode
106117  171001  	SAT 1
106120  140065  	SKP IF DA EQL ST	; mode == 1 ?
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
106145  135040  	JPL I 40		; -> 106205 (link cell = 117352, MRENF) mode 1
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
106167  135021  	JPL I 21		; -> 106210 (link cell = 120233, MSTRM) tail; SETTF mode 4 lands here
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
106205  117352  	FDV I ,X -26		; (data) link -> 117352 (MRENF)
106206  120470  	MPY ,B 70		; (data) link -> 120470 (MSPER)
106207  120236  	MPY -142		; (data) link -> 120236 (MSTMP)
106210  120233  	MPY -145		; (data) link -> 120233 (MSTRM) <- SetTemporaryFile mode 4
106211  003776  	STZ I ,B ,X -2		; (data) link -> 003776 (resident return)
