;=============================================================================
; MON 234B (octal) SetPeripheralName (SPEFI) - real SINTRAN L disassembly
;
; Defines a peripheral file: connects a file name to the logical device number of a
; peripheral (e.g. a printer). The file name should exist in advance with no file type.
;
; Dispatch is 3-layer (see README Code location). Two labelled regions:
;   A. GOTAB[234] level-14 entry stub  F1724 @066176B in segment 025-S3IRPIT (load
;      32000B). Window 066176B..066200B = 3 words, bounded strictly to the next symbol
;      DV200/EN000=066201B. A tiny compiler entry stub sitting in a shared stub block;
;      the real transfer to the SPEFI worker is the resident CALLPROC (uncarved).
;   B. SPEFI peripheral-file worker  SPEFI @106055B in segment 006-S3FS (load 26000B).
;      The body runs 106055B..106176B (code, 106176B a one-word ROP NOOP pad) with an
;      11-word pointer table 106177B..106211B (link cells, DATA). The next symbol that
;      is a real routine is RSPQE=106212B.
;   (the A->B bridge is the uncarved resident CALLPROC - NOT present in these bytes;
;    region B IS real executable code; the A->B link is by symbol NAME.)
;
; SPEFI has a THREE-entry idiom encoding an initial function selector in the SSM/SSK
; skip flags: SPEFI=106055B (SSM=1,SSK=0), MRNFI=106060B (SSM=0,SSK=1), MDLFI=106063B
; (SSM=0,SSK=0). All join the common body at 106065B, which derives a function code in
; B+123 (106072B..106106B) and then dispatches by that code (106116B..106170B) to one
; of several resident workers. SPEFI (Set PEripheral FIle) is a FILSYS-SYMBOLS symbol
; in the file-system segment 006-S3FS; the neighbouring FILSYS symbols in this window
; (MRNFI, MDLFI, UKPAR, USUNI, ...) are interior labels of this one body.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
;=============================================================================

; ---- Region A: GOTAB[234] entry stub F1724 (025-S3IRPIT, load 32000B) ----
; F1724 = 066176B (SYMBOL-2-LIST); next symbol DV200/EN000 = 066201B, so the stub is
; 3 words. GOTAB[234] = 066176B (byte-proven). It sits in a shared stub block; the real
; transfer to SPEFI is the resident CALLPROC. A static decode cannot follow it.
066176  004420  	STA ,B 20	; mem[B+20] = A
066177  065063  	SUB I 63	; A = A - mem[[066263]]
066200  146151  	RADD CLD SA DD	; D = A
; 066201 = DV200/EN000 boundary (next symbol); not part of this stub.

; ---- Region B: SPEFI peripheral-file worker (006-S3FS, load 26000B) ----
106055  174270  	BSET ONE SSM	; SPEFI entry (SetPeripheralName): SSM = 1, SSK = 0
106056  174020  	BSET ZRO SSK
106057  124006  	JMP 6		; -> 106065
106060  174070  	BSET ZRO SSM	; MRNFI alt entry: SSM = 0, SSK = 1
106061  174220  	BSET ONE SSK
106062  124003  	JMP 3		; -> 106065
106063  174070  	BSET ZRO SSM	; MDLFI alt entry: SSM = 0, SSK = 0
106064  174020  	BSET ZRO SSK
106065  021111  	STD I 111	; store A/D pair via [link cell] - save incoming
106066  146145  	RADD CLD SL DA	; A = L  (copy return link)
106067  146131  	RADD CLD SB DD	; D = B  (copy frame pointer)
106070  170125  	SAB 125		; B = 125
106071  135106  	JPL I 106	; -> [106177]  call resident prologue worker
106072  175270  	BSKP ONE SSM	; skip next if SSM set (SPEFI path)
106073  124007  	JMP 7		; -> 106102
106074  175220  	BSKP ONE SSK	; skip next if SSK set
106075  124003  	JMP 3		; -> 106100
106076  170403  	SAA 3		; A = 3   (function code)
106077  124002  	JMP 2		; -> 106101
106100  170402  	SAA 2		; A = 2   (function code)
106101  124006  	JMP 6		; -> 106107
106102  175220  	BSKP ONE SSK	; skip next if SSK set
106103  124003  	JMP 3		; -> 106106
106104  170401  	SAA 1		; A = 1   (function code)
106105  124002  	JMP 2		; -> 106107
106106  146105  	RADD CLD 0 DA	; A = 0   (function code)
106107  004523  	STA ,B 123	; mem[B+123] = A  (store function code)
106110  044070  	LDA 70		; A = mem[P+70]
106111  146035  	RADD SB DA	; A = A + B
106112  050067  	LDT 67		; T = mem[P+67]
106113  054400  	LDX ,B 0	; X = mem[B+0]
106114  135066  	JPL I 66	; -> [106202]  call resident worker (parse name)
106115  124057  	JMP 57		; -> 106174  (error tail)
106116  044523  	LDA ,B 123	; A = mem[B+123]  (function code)
106117  171001  	SAT 1		; T = 1
106120  140065  	SKP IF DA EQL ST	; skip if function code == 1
106121  124007  	JMP 7		; -> 106130
106122  044061  	LDA 61		; A = mem[P+61]
106123  146035  	RADD SB DA	; A = A + B
106124  171026  	SAT 26		; T = 26
106125  054402  	LDX ,B 2	; X = mem[B+2]
106126  135054  	JPL I 54	; -> [106202]  call resident worker
106127  124045  	JMP 45		; -> 106174  (error tail)
106130  054050  	LDX 50		; X = mem[P+50]
106131  146037  	RADD SB DX	; X = X + B
106132  044523  	LDA ,B 123	; A = mem[B+123]  (function code)
106133  131404  	JAF 4		; -> 106137  (jump if function code != 0)
106134  135050  	JPL I 50	; -> [106204]  call resident worker (code 0)
106135  124037  	JMP 37		; -> 106174  (error tail)
106136  124033  	JMP 33		; -> 106171  (success)
106137  044523  	LDA ,B 123	; A = mem[B+123]
106140  171001  	SAT 1		; T = 1
106141  140065  	SKP IF DA EQL ST	; skip if code == 1
106142  124006  	JMP 6		; -> 106150
106143  044040  	LDA 40		; A = mem[P+40]
106144  146035  	RADD SB DA	; A = A + B
106145  135040  	JPL I 40	; -> [106205]  call resident worker (code 1)
106146  124026  	JMP 26		; -> 106174  (error tail)
106147  124022  	JMP 22		; -> 106171  (success)
106150  044523  	LDA ,B 123	; A = mem[B+123]
106151  171002  	SAT 2		; T = 2
106152  140065  	SKP IF DA EQL ST	; skip if code == 2
106153  124005  	JMP 5		; -> 106160
106154  044402  	LDA ,B 2	; A = mem[B+2]
106155  135031  	JPL I 31	; -> [106206]  call resident worker (code 2)
106156  124016  	JMP 16		; -> 106174  (error tail)
106157  124012  	JMP 12		; -> 106171  (success)
106160  044523  	LDA ,B 123	; A = mem[B+123]
106161  171003  	SAT 3		; T = 3
106162  140065  	SKP IF DA EQL ST	; skip if code == 3
106163  124004  	JMP 4		; -> 106167
106164  135023  	JPL I 23	; -> [106207]  call resident worker (code 3)
106165  124007  	JMP 7		; -> 106174  (error tail)
106166  124003  	JMP 3		; -> 106171  (success)
106167  135021  	JPL I 21	; -> [106210]  call resident worker (default)
106170  124004  	JMP 4		; -> 106174  (error tail)
106171  040404  	MIN ,B 4	; mem[B+4]++ ; skip next if it wraps to 0 (success advance)
106172  170653  	SAA -125	; A = -125  (error code)
106173  125016  	JMP I 16	; -> [106211]  indirect return
106174  004402  	STA ,B 2	; mem[B+2] = A  (error tail: store error code)
106175  124375  	JMP -3		; -> 106172
106176  147417  	ROP NOOP	; one-word pad (closes the code region)
; ---- link cells (DATA, not code) 106177..106211 ----
106177  003752  	STZ I ,B ,X -26	; pointer word (read by JPL I 106 at 106071)
106200  000021  	STZ 21		; constant/pointer cell
106201  000203  	STZ -175	; constant/pointer cell
106202  031075  	STF I 75	; pointer word (read by JPL I 66/54 at 106114/106126)
106203  000006  	STZ 6		; constant/pointer cell
106204  120067  	MPY 67		; pointer word (read by JPL I 50 at 106134)
106205  117352  	FDV I ,X -26	; pointer word (read by JPL I 40 at 106145)
106206  120470  	MPY ,B 70	; pointer word (read by JPL I 31 at 106155)
106207  120236  	MPY -142	; pointer word (read by JPL I 23 at 106164)
106210  120233  	MPY -145	; pointer word (read by JPL I 21 at 106167)
106211  003776  	STZ I ,B ,X -2	; pointer word (read by JMP I 16 at 106173, return)
; 106212 = RSPQE boundary (next symbol, a different routine); not part of this worker.
