; ============================================================================
; MON 333B (octal) DMAFunction - real SINTRAN L disassembly
;
; MON 333B is an ND-100 call (333B < 400B).  GOTAB[333B] = 112561B, which is the
; DIA11 entry stub in overlay 025-S3IRPIT (load base 32000B).  The stub is a
; DMA-device level-14 entry: it saves the caller register block and switches
; page context (TRR PCR) before the DMA worker runs.
;
;   Region A  DIA11 entry stub  025-S3IRPIT @112561B  (GOTAB target)
;             span 112561B..112615B
;   Region B  UDMA worker body  025-S3IRPIT @110770B  (DMA function dispatcher)
;             span 110770B..111120B code + 111121B..111130B pointer/data table
;
; The DIA11 -> UDMA link crosses the TRR PCR context switch / resident CALLPROC
; bridge (uncarved); UDMA is attributed by symbol + the function-code decoder
; whose codes match the DMAFunction contract (see README Honest caveats).
;
; Extracted verbatim from the byte-identity-checked whole-segment disassembly
; ../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm .  Addresses and values octal.
; >>> NAME lines are in-range SYMBOL-2-LIST labels.
; ============================================================================

; ---- Region A: DIA11 DMA-device entry stub (025-S3IRPIT, load 32000B) ----
>>> DIA11(SYMBOL-2-LIST) = 112561B
112561  150401  	IOF			; interrupts off (critical section)
112562  150103  	TRR PCR			; switch page control register (context)
112563  055076  	LDX I 76		; X = mem[112661B] worker/block pointer
112564  006017  	STA ,X 17		; save caller A into register block slot 17
112565  150402  	ION			; interrupts on
112566  124005  	JMP 5		; -> 112573
112567  044422  	LDA ,B 22
112570  060424  	ADD ,B 24
112571  172777  	AAA -1			; A = A - 1
112572  131045  	JAZ 45		; -> 112637 (shared disk-stub tail)
112573  044422  	LDA ,B 22		; A = device/status word
112574  060424  	ADD ,B 24
112575  174375  	BSET ONE 170 DA		; A bit15 := 1 (flag)
112576  004412  	STA ,B 12		; store status word (,B 12)
112577  124031  	JMP 31		; -> 112630 (shared disk-stub tail)
112600  060425  	ADD ,B 25
112601  124002  	JMP 2		; -> 112603
112602  154410  	SHA 10			; A <<= 8
112603  146156  	RADD CLD SA DT		; T = A
112604  044421  	LDA ,B 21
112605  146154  	RADD CLD SA DL		; L = A
112606  044420  	LDA ,B 20		; A = param word (,B 20)
112607  055052  	LDX I 52		; X = worker block pointer
112610  006017  	STA ,X 17
112611  150103  	TRR PCR
112612  146147  	RADD CLD SL DX		; X = L
112613  012000  	STT ,X 0		; save T (MON number) into block slot 0
112614  044046  	LDA 46
112615  150401  	IOF			; (falls into DSI6 region)

; ---- Region B: UDMA DMA-function worker (025-S3IRPIT, load 32000B) ----
; Function-code decoder: builds a device control word by matching FuncCode.
>>> UDMA(SYMBOL-2-LIST) = 110770B
110770  135131  	JPL I 131		; -> [111121] setup worker (pointer word)
110771  044411  	LDA ,B 11
110772  135130  	JPL I 130		; -> [111122]
110773  142005  	SKP IF DA UEQ 0		; skip if A != 0
110774  125127  	JMP I 127		; -> [111123] error/return
110775  146116  	RADD CLD SD DT		; T = D
110776  010424  	STT ,B 24
110777  146157  	RADD CLD SA DX		; X = A
111000  046003  	LDA ,X 3
111001  175325  	BSKP ONE 120 DA		; if A bit10 set skip
111002  125122  	JMP I 122		; -> [111124]
111003  170404  	SAA 4
111004  150307  	MST PIE			; enable named interrupt bits by A
111005  045120  	LDA I 120
111006  131010  	JAZ 10		; -> 111016
111007  146137  	RADD CLD SB DX		; X = B
111010  173407  	AAX 7			; X = X + 7
111011  152640  	LRB 4			; load 8-word register block at X (alt PT)
111012  054114  	LDX 114
111013  152442  	SRB 40			; store 8-word register block at X (alt PT)
111014  146135  	RADD CLD SB DA		; A = B
111015  005112  	STA I 112
111016  170400  	SAA 0
111017  146151  	RADD CLD SA DD		; D = A (clear control word)
111020  044420  	LDA ,B 20		; A = FuncCode (,B 20)
111021  131404  	JAF 4		; -> 111025 (FuncCode != 0)
111022  174201  	BSET ONE 0 DD		; func 0: D bit0 := 1
111023  174221  	BSET ONE 20 DD		;         D bit2 := 1
111024  124105  	JMP 105		; -> 111131 (merge)
111025  171001  	SAT 1
111026  140065  	SKP IF DA EQL ST	; FuncCode == 1 ?
111027  124003  	JMP 3		; -> 111032
111030  174211  	BSET ONE 10 DD		; func 1: D bit1 := 1
111031  124100  	JMP 100		; -> 111131
111032  171002  	SAT 2
111033  142065  	SKP IF DA UEQ ST	; FuncCode != 2 ?
111034  124004  	JMP 4		; -> 111040
111035  171003  	SAT 3
111036  140065  	SKP IF DA EQL ST	; FuncCode == 3 ?
111037  124003  	JMP 3		; -> 111042
111040  174231  	BSET ONE 30 DD		; func 2/3: D bit3 := 1
111041  124070  	JMP 70		; -> 111131
111042  171020  	SAT 20
111043  142065  	SKP IF DA UEQ ST	; FuncCode != 20 ?
111044  124004  	JMP 4		; -> 111050
111045  171024  	SAT 24
111046  140065  	SKP IF DA EQL ST	; FuncCode == 24 ?
111047  124004  	JMP 4		; -> 111053
111050  174221  	BSET ONE 20 DD		; func 20/24: D bit2 := 1
111051  174241  	BSET ONE 40 DD		;             D bit4 := 1
111052  124057  	JMP 57		; -> 111131
111053  171054  	SAT 54
111054  142065  	SKP IF DA UEQ ST	; FuncCode != 54 ?
111055  124004  	JMP 4		; -> 111061
111056  171056  	SAT 56
111057  140065  	SKP IF DA EQL ST	; FuncCode == 56 ?
111060  124003  	JMP 3		; -> 111063
111061  174251  	BSET ONE 50 DD		; func 54/56: D bit5 := 1
111062  124047  	JMP 47		; -> 111131
111063  171055  	SAT 55
111064  142065  	SKP IF DA UEQ ST	; FuncCode != 55 ?
111065  124004  	JMP 4		; -> 111071
111066  171057  	SAT 57
111067  140065  	SKP IF DA EQL ST	; FuncCode == 57 ?
111070  124003  	JMP 3		; -> 111073
111071  174261  	BSET ONE 60 DD		; func 55/57: D bit6 := 1
111072  124037  	JMP 37		; -> 111131
111073  171007  	SAT 7
111074  142065  	SKP IF DA UEQ ST	; FuncCode != 7 ?
111075  124004  	JMP 4		; -> 111101
111076  171062  	SAT 62
111077  140065  	SKP IF DA EQL ST	; FuncCode == 62 ?
111100  124003  	JMP 3		; -> 111103
111101  174221  	BSET ONE 20 DD		; func 7/62: D bit2 := 1
111102  124027  	JMP 27		; -> 111131
111103  171021  	SAT 21
111104  142065  	SKP IF DA UEQ ST	; FuncCode != 21 ?
111105  124012  	JMP 12		; -> 111117 (unrecognised)
111106  171064  	SAT 64
111107  142065  	SKP IF DA UEQ ST	; FuncCode != 64 ?
111110  124007  	JMP 7		; -> 111117
111111  171065  	SAT 65
111112  142065  	SKP IF DA UEQ ST	; FuncCode != 65 ?
111113  124004  	JMP 4		; -> 111117
111114  171070  	SAT 70
111115  140065  	SKP IF DA EQL ST	; FuncCode == 70 ?
111116  124002  	JMP 2		; -> 111120
111117  124012  	JMP 12		; -> 111131 (unrecognised: merge with no bits set)
111120  125010  	JMP I 10		; -> [111130] (func 21/64/65/70 worker)
; ---- 111121B..111130B: JPL-I pointer / data table (disassembles as bogus code) ----
111121  000342  	; pointer word (000342)
111122  010376  	; pointer word (010376)
111130  111364  	; pointer word (111364)
; ---- shared status-store tail (control-flow closure boundary at 111131) ----
111131  146115  	RADD CLD SD DA		; A = D  (the built device control word)
111132  004435  	STA ,B 35		; store control word to caller datafield
