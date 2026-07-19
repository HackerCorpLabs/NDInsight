; ============================================================================
; MON 6B (octal) WriteScratchFile - real SINTRAN L disassembly
; ----------------------------------------------------------------------------
; Dispatch note: GOTAB[006] = 000000 (fall-through) - prove-mon.py 006.
;   Unlike its read twin MON 5B (whose GOTAB[005] = 120355B is a real F1611
;   entry stub in 025-S3IRPIT), MON 6B has NO level-14 GOTAB stub. It is a
;   fall-through call resolved at run time by the uncarved resident
;   CALLPROC / MFELL second-level dispatch. So there is only ONE code region
;   to disassemble here: the worker body.
;
; Region: WDISK/RDISK shared file read/write worker body
;         entry WDISK @102023B (write), twin RDISK @102021B (read)
;         segment 006-S3FS (load base 26000B, decimal 11264)
;   The A->body bridge (CALLPROC) is the uncarved resident code - not shown.
;
; Byte offset of 102023B in 006-S3FS.bin = (102023 - 26000) octal * 2
;   = 54023B words = 22547 dec words -> byte 45094 (big-endian, as carved).
; Disassembled from the canonical 006-S3FS.bin.
; Addresses and values are octal.
; ============================================================================

; ---- RDISK/WDISK shared worker body (006-S3FS) ----
; 102021 = RDISK entry (read: SSK:=0); 102023 = WDISK entry (write: SSK:=1).
102021  174020  	BSET ZRO SSK		; RDISK entry: SSK := 0 (READ)
102022  124002  	JMP 2		; -> 102024 (skip the write-set)
102023  174220  	BSET ONE SSK		; WDISK entry (MON 6B): SSK := 1 (WRITE)
102024  021073  	STD I 73		; save D via ptr word 73
102025  146145  	RADD CLD SL DA		; A := L (return link)
102026  146131  	RADD CLD SB DD		; D := B (data-frame base)
102027  170006  	SAB 6
102030  135070  	JPL I 70		; -> ptr 102120 = SPUSH (003752): reg save
102031  054070  	LDX 70
102032  050070  	LDT 70
102033  140067  	SKP IF DX EQL ST	; loop guard
102034  124003  	JMP 3		; -> 102037
102035  170537  	SAA 137		; A := 137 (error/count constant)
102036  124057  	JMP 57		; -> 102115 (exit path)
102037  146171  	RADD CLD SX DD
102040  175220  	BSKP ONE SSK		; branch on WRITE flag
102041  124003  	JMP 3		; -> 102044
102042  056001  	LDX ,X 1		; write-side field
102043  124002  	JMP 2		; -> 102045
102044  056000  	LDX ,X 0		; read-side field
102045  046003  	LDA ,X 3
102046  175365  	BSKP ONE 160 DA
102047  124006  	JMP 6		; -> 102055
102050  175175  	BSKP ZRO 170 DA
102051  124004  	JMP 4		; -> 102055
102052  046007  	LDA ,X 7
102053  175125  	BSKP ZRO 120 DA
102054  124004  	JMP 4		; -> 102060
102055  146117  	RADD CLD SD DX
102056  173402  	AAX 2
102057  124353  	JMP -25		; -> 102032 (loop back)
102060  146115  	RADD CLD SD DA
102061  064040  	SUB 40
102062  156577  	SHA ZIN SHR 1		; scale (block -> word/page arithmetic)
102063  172500  	AAA 100
102064  005037  	STA I 37
102065  052014  	LDT ,X 14
102066  044401  	LDA ,B 1		; data-frame parameter
102067  172401  	AAA 1
102070  131010  	JAZ 10		; -> 102100
102071  172777  	AAA -1
102072  135032  	JPL I 32		; -> ptr 102124 = 033740 (block/word helper)
102073  156601  	SAD ZIN 1
102074  146201  	RADD CM1 0 DD
102075  147155  	RADD ADC CLD SA DA
102076  172777  	AAA -1
102077  022017  	STD ,X 17
102100  044405  	LDA ,B 5		; data-frame parameter
102101  146151  	RADD CLD SA DD
102102  044400  	LDA ,B 0
102103  175220  	BSKP ONE SSK		; *** READ vs WRITE decision ***
102104  124004  	JMP 4		; -> 102110 (read path)
102105  135020  	JPL I 20		; -> ptr 102125 = FWRT (100130): FILE-WRITE <-- WDISK
102106  124007  	JMP 7		; -> 102115 (exit)
102107  124003  	JMP 3		; -> 102112
102110  135016  	JPL I 16		; -> ptr 102126 = FREA (077542): FILE-READ  <-- RDISK
102111  124004  	JMP 4		; -> 102115 (exit)
102112  040404  	MIN ,B 4		; counter bump
102113  170772  	SAA -6
102114  125013  	JMP I 13		; -> ptr 102127 = SPOP (003776): reg restore + return
102115  004402  	STA ,B 2		; store result/error into frame field 2
102116  124375  	JMP -3		; -> 102113 (SPOP return)

; ---- indirect-call pointer table (data words) ----
102117  147417  	ROP NOOP		; (unreferenced data word)
102120  003752  	STZ I ,B ,X -26		; = 003752 = SPUSH ptr  (used by JPL I 70 @102030)
102121  147515  	ROP NOOP		; (unreferenced data word)
102122  147715  	ROP NOOP		; (unreferenced data word)
102123  147505  	ROP NOOP		; (unreferenced data word)
102124  033740  	STF I ,B ,X -40		; = 033740 = B4INW/ATMUL ptr (JPL I 32 @102072) - ambiguous
102125  100130  	FAD 130		; = 100130 = FWRT ptr   (JPL I 20 @102105)
102126  077542  	ORA I ,B ,X 142		; = 077542 = FREA ptr   (JPL I 16 @102110)
102127  003776  	STZ I ,B ,X -2		; = 003776 = SPOP ptr   (JMP I 13 @102114)

; ---- adjacent shared RFILE/WFILE body (included by carve closure) ----
; Present because the carve took the window to control-flow closure of the
; pointer-table span; NO direct branch from the WDISK body above enters here.
102130  174020  	BSET ZRO SSK		; RFILE: read
102131  124002  	JMP 2		; -> 102133
102132  174220  	BSET ONE SSK		; WFILE: write
102133  021156  	STD I 156
102134  146145  	RADD CLD SL DA
102135  146131  	RADD CLD SB DD
102136  170017  	SAB 17
102137  135153  	JPL I 153		; -> ptr 102312 (outside window)
102140  054405  	LDX ,B 5
102141  046023  	LDA ,X 23
102142  004413  	STA ,B 13
102143  046022  	LDA ,X 22
102144  004412  	STA ,B 12
102145  046024  	LDA ,X 24
102146  004416  	STA ,B 16
102147  046020  	LDA ,X 20
102150  005143  	STA I 143
102151  135143  	JPL I 143		; -> ptr 102314 (outside window)
102152  175220  	BSKP ONE SSK
102153  124003  	JMP 3		; -> 102156
102154  146117  	RADD CLD SD DX
102155  124002  	JMP 2		; -> 102157
102156  146157  	RADD CLD SA DX
102157  014410  	STX ,B 10
102160  140007  	SKP IF DX EQL 0
102161  124012  	JMP 12		; -> 102173
102162  131407  	JAF 7		; -> 102171
102163  140001  	SKP IF DD EQL 0
102164  124003  	JMP 3		; -> 102167
102165  170532  	SAA 132
102166  124002  	JMP 2		; -> 102170
102167  170526  	SAA 126
102170  124002  	JMP 2		; -> 102172
102171  170525  	SAA 125
102172  125123  	JMP I 123		; -> ptr 102315 (outside window)
102173  046003  	LDA ,X 3		; last word of carve window
; ============================================================================
