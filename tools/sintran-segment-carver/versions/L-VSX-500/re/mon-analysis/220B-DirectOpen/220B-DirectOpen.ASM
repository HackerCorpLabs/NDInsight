; ============================================================================
; MON 220B (octal) DirectOpen - real SINTRAN L disassembly
;
; MON 220B is a FALL-THROUGH call: GOTAB[220] = 000000 (byte-proven), so there
; is NO per-call level-14 entry stub to list. Dispatch drops into the resident
; MFELL/CALLPROC second-level path (uncarved) which reaches the worker below.
;
; The open family is a set of THREE-WORD flag-fork preludes that all jump into
; ONE shared OPEN body at 103041B:
;   DOPEN @103026B (this call, MON 220)  set M=1 K=1
;   SCROP @103031B (MON 235 ScratchOpen) set M=1 K=0
;   OPFIL @103034B (open-file-family)    set M=0 K=0
;   OLDOP @103037B (old-open-family)     set M=0 K=1
; Each prelude sets/clears the STS M (SSM) and K (SSK) flag bits then JMPs to
; the common body; the body reads those two bits back to select the access mode.
;
; Two labelled regions:
;   A. DOPEN flag-fork prelude   DOPEN @103026B in segment 006-S3FS (load 26000B)
;   B. shared OPEN worker body   103041B..103347B in segment 006-S3FS
;      The body opens a file: it JPL-calls FOPEN (@67432B) - the file-open
;      primitive (link cell 103341B) - which is the byte-level reason this is
;      the open worker. Body closes into the resident return (003776B).
;   (the fall-through -> DOPEN bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; Words 103234B..103266B and 103335B..103347B are POINTER TABLES (link cells),
; not code; nd100-dis renders them as bogus instructions. Their contents are the
; real worker addresses (resolved in trailing comments). Addresses/values octal.
; ============================================================================

; ---- Region A: DOPEN flag-fork prelude (006-S3FS, load 26000B) ----
103026  174270  	BSET ONE SSM		; DOPEN entry (MON 220): STS M bit := 1
103027  174220  	BSET ONE SSK		; STS K bit := 1   (M=1,K=1 selects DirectOpen mode)
103030  124011  	JMP 11			; -> 103041 (shared OPEN body)

; ---- Region B: shared OPEN worker body (006-S3FS, load 26000B) ----
103041  021173  	STD I 173		; OPEN entry: stash caller D (params); all 4 variants land here
103042  146145  	RADD CLD SL DA
103043  146131  	RADD CLD SB DD
103044  170150  	SAB 150			; establish local frame (B), 150 words
103045  135170  	JPL I 170		; -> 103235 (link cell = 003752, resident prologue worker)
103046  175270  	BSKP ONE SSM		; read back the M fork flag set by the prelude
103047  124007  	JMP 7			; -> 103056
103050  175220  	BSKP ONE SSK		; read back the K fork flag
103051  124003  	JMP 3			; -> 103054
103052  170777  	SAA -1			; M=1,K=1 (DirectOpen): mode = -1
103053  124002  	JMP 2			; -> 103055
103054  170402  	SAA 2			; M=1,K=0 (ScratchOpen): mode = 2
103055  124006  	JMP 6			; -> 103063
103056  175220  	BSKP ONE SSK
103057  124003  	JMP 3			; -> 103062
103060  170401  	SAA 1			; M=0,K=1 (OldOpen): mode = 1
103061  124002  	JMP 2			; -> 103063
103062  146105  	RADD CLD 0 DA		; M=0,K=0 (OpenFile): mode = 0
103063  004547  	STA ,B 147		; store selected access mode -> frame B+147
103064  045152  	LDA I 152
103065  004545  	STA ,B 145
103066  045151  	LDA I 151
103067  004546  	STA ,B 146
103070  045150  	LDA I 150
103071  004544  	STA ,B 144
103072  054400  	LDX ,B 0
103073  133050  	JXZ 50			; -> 103143
103074  044547  	LDA ,B 147
103075  171001  	SAT 1
103076  140065  	SKP IF DA EQL ST
103077  124037  	JMP 37			; -> 103136
103100  171005  	SAT 5
103101  044140  	LDA 140
103102  146035  	RADD SB DA
103103  135137  	JPL I 137		; -> 103242 (link cell)
103104  125137  	JMP I 137		; -> 103243 (link cell = 103313, exit label)
103105  146157  	RADD CLD SA DX
103106  046002  	LDA ,X 2
103107  066000  	SUB ,X 0
103110  156401  	SHA ZIN 1
103111  146156  	RADD CLD SA DT
103112  046004  	LDA ,X 4
103113  175025  	BSKP ZRO 20 DA
103114  124002  	JMP 2			; -> 103116
103115  173377  	AAT -1
103116  056000  	LDX ,X 0
103117  173401  	AAX 1
103120  141006  	SKP IF DT GRE 0
103121  124004  	JMP 4			; -> 103125
103122  170577  	SAA 177
103123  143065  	SKP IF DA LST ST
103124  124002  	JMP 2			; -> 103126
103125  050117  	LDT 117
103126  044113  	LDA 113
103127  146035  	RADD SB DA
103130  135115  	JPL I 115		; -> 103245 (link cell)
103131  124162  	JMP 162			; -> 103313 (store-status exit)
103132  146157  	RADD CLD SA DX
103133  170447  	SAA 47
103134  135112  	JPL I 112		; -> 103246 (link cell)
103135  124006  	JMP 6			; -> 103143
103136  044103  	LDA 103
103137  146035  	RADD SB DA
103140  050104  	LDT 104
103141  135104  	JPL I 104		; -> 103245 (link cell)
103142  124151  	JMP 151			; -> 103313 (store-status exit)
103143  044104  	LDA 104
103144  146035  	RADD SB DA
103145  171004  	SAT 4
103146  054402  	LDX ,B 2
103147  135076  	JPL I 76		; -> 103245 (link cell)
103150  124143  	JMP 143			; -> 103313 (store-status exit)
103151  146157  	RADD CLD SA DX
103152  170447  	SAA 47
103153  135073  	JPL I 73		; -> 103246 (link cell)
103154  044400  	LDA ,B 0
103155  131414  	JAF 14			; -> 103171
103156  055072  	LDX I 72
103157  046001  	LDA ,X 1
103160  175205  	BSKP ONE 0 DA
103161  124010  	JMP 10			; -> 103171
103162  045067  	LDA I 67
103163  131406  	JAF 6			; -> 103171
103164  135066  	JPL I 66		; -> 103252 (link cell)
103165  054054  	LDX 54
103166  146037  	RADD SB DX
103167  044055  	LDA 55
103170  135063  	JPL I 63		; -> 103253 (link cell)
103171  054050  	LDX 50
103172  146037  	RADD SB DX
103173  146171  	RADD CLD SX DD
103174  044060  	LDA 60
103175  050047  	LDT 47
103176  135057  	JPL I 57		; -> 103255 (link cell)
103177  124114  	JMP 114			; -> 103313 (store-status exit)
103200  135056  	JPL I 56		; -> 103256 (link cell)
103201  124002  	JMP 2			; -> 103203
103202  124065  	JMP 65			; -> 103267 (finalize path)
103203  044547  	LDA ,B 147
103204  171377  	SAT -1
103205  140065  	SKP IF DA EQL ST
103206  124003  	JMP 3			; -> 103211
103207  135050  	JPL I 50		; -> 103257 (link cell)
103210  000547  	STZ ,B 147
103211  044547  	LDA ,B 147
103212  130055  	JAP 55			; -> 103267 (finalize path)
103213  170444  	SAA 44
103214  171400  	SAX 0
103215  006520  	STA ,X ,B 120
103216  050023  	LDT 23
103217  146036  	RADD SB DT
103220  054040  	LDX 40
103221  146037  	RADD SB DX
103222  135037  	JPL I 37		; -> 103261 (link cell)
103223  124103  	JMP 103			; -> 103326
103224  045036  	LDA I 36
103225  135036  	JPL I 36		; -> 103263 (link cell = 010500, resident worker)
103226  135036  	JPL I 36		; -> 103264 (link cell)
103227  124101  	JMP 101			; -> 103330
103230  045032  	LDA I 32
103231  135034  	JPL I 34		; -> 103265 (link cell = 010506, resident worker)
103232  135034  	JPL I 34		; -> 103266 (link cell)
103233  124034  	JMP 34			; -> 103267 (finalize path)
; ---- pointer table 1 (DATA, not code) 103234..103266 ----
103234  147417  	ROP NOOP		; (data) padding
103235  003752  	STZ I ,B ,X -26		; (data) link -> 003752 (resident prologue worker)
103236  146275  	RADD CM1 SX DA		; (data)
103237  146276  	RADD CM1 SX DT		; (data)
103240  146274  	RADD CM1 SX DL		; (data)
103241  000006  	STZ 6			; (data)
103242  031067  	STF I 67		; (data) link -> 031067
103243  103313  	FAD I ,X -65		; (data) link -> 103313 (exit label, in-body)
103244  000203  	STZ -175		; (data)
103245  031075  	STF I 75		; (data) link -> 031075
103246  030100  	STF 100			; (data) link -> 030100
103247  000115  	STZ 115			; (data)
103250  146301  	RADD CM1 CLD 0 DD	; (data)
103251  144075  	SWAP SX DA		; (data)
103252  030245  	STF -133		; (data) link -> 030245
103253  041413  	MIN I ,B 13		; (data) link -> 041413
103254  043465  	MIN I ,B ,X 65		; (data)
103255  046231  	LDA ,X -147		; (data) link -> 046231
103256  061451  	ADD I ,B 51		; (data) link -> 061451
103257  053114  	LDT I ,X 114		; (data) link -> 053114
103260  000120  	STZ 120			; (data)
103261  043100  	MIN I ,X 100		; (data) link -> 043100
103262  120027  	MPY 27			; (data)
103263  010500  	STT ,B 100		; (data) link -> 010500 (resident worker)
103264  114350  	FDV -30			; (data) link -> 114350
103265  010506  	STT ,B 106		; (data) link -> 010506 (resident worker)
103266  101127  	FAD I 127		; (data) link -> 101127
; ---- finalize / status-store CODE resumes 103267..103334 ----
103267  044547  	LDA ,B 147
103270  172776  	AAA -2
103271  146151  	RADD CLD SA DD
103272  054043  	LDX 43
103273  146037  	RADD SB DX
103274  050401  	LDT ,B 1
103275  044547  	LDA ,B 147
103276  130002  	JAP 2			; -> 103300
103277  174376  	BSET ONE 170 DT		; set bit 15 of T (170>>3 = bit 15)
103300  044036  	LDA 36
103301  146035  	RADD SB DA
103302  135035  	JPL I 35		; -> 103337 (link cell)
103303  140001  	SKP IF DD EQL 0
103304  124004  	JMP 4			; -> 103310
103305  135033  	JPL I 33		; -> 103340 (link cell)
103306  124005  	JMP 5			; -> 103313 (store-status exit)
103307  124003  	JMP 3			; -> 103312
103310  135031  	JPL I 31		; -> 103341 (link cell = 067432, FOPEN file-open primitive)
103311  124002  	JMP 2			; -> 103313 (store-status exit)
103312  040404  	MIN ,B 4		; success flag++
103313  004402  	STA ,B 2		; store status word -> caller slot B+2
103314  044544  	LDA ,B 144
103315  005025  	STA I 25
103316  044545  	LDA ,B 145
103317  005024  	STA I 24
103320  044546  	LDA ,B 146
103321  005023  	STA I 23
103322  170630  	SAA -150
103323  125022  	JMP I 22		; -> 103345 (link cell = 003776, resident return)
103324  170553  	SAA 153
103325  124366  	JMP -12			; -> 103313 (store-status exit)
103326  130741  	JAN -37			; -> 103267 (finalize path)
103327  124364  	JMP -14			; -> 103313 (store-status exit)
103330  146157  	RADD CLD SA DX
103331  045015  	LDA I 15
103332  135015  	JPL I 15		; -> 103347 (link cell)
103333  146175  	RADD CLD SX DA
103334  124357  	JMP -21			; -> 103313 (store-status exit)
; ---- pointer table 2 (DATA, not code) 103335..103347 ----
103335  000006  	STZ 6			; (data)
103336  000115  	STZ 115			; (data)
103337  030205  	STF -173		; (data) link -> 030205
103340  072163  	AND ,X 163		; (data) link -> 072163
103341  067432  	SUB I ,B ,X 32		; (data) link -> 067432 (FOPEN file-open primitive)
103342  146274  	RADD CM1 SX DL		; (data)
103343  146275  	RADD CM1 SX DA		; (data)
103344  146276  	RADD CM1 SX DT		; (data)
103345  003776  	STZ I ,B ,X -2		; (data) link -> 003776 (resident return)
103346  120027  	MPY 27			; (data) link -> 120027
103347  010506  	STT ,B 106		; (data) link -> 010506 (resident worker)
