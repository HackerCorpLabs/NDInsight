; ============================================================================
; MON 331B (octal) DiskMirroring - real SINTRAN L disassembly
;
; MON 331B is an ND-100 call (331B < 400B).  GOTAB[331B] = 112532B, which is the
; DIA10 entry stub in overlay 025-S3IRPIT (load base 32000B).  The stub is a
; disk-device level-14 entry: it saves the caller register block and switches
; page context (TRR PCR) before the reservation/mirror worker runs.
;
;   Region A  DIA10 entry stub   025-S3IRPIT @112532B  (GOTAB target)
;             span 112532B..112560B
;   Region B  MSYSU worker body  025-S3IRPIT @132567B  (disk-mirror dispatcher)
;             span 132567B..132733B code + 132734B..132765B pointer/data table
;
; The DIA10 -> MSYSU link crosses the TRR PCR context switch / resident CALLPROC
; bridge (uncarved); MSYSU is attributed by symbol + the 16-way sub-function
; dispatch that matches the DiskMirroring contract (see README Honest caveats).
;
; Extracted verbatim from the byte-identity-checked whole-segment disassembly
; ../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm .  Addresses and values octal.
; >>> NAME lines are in-range SYMBOL-2-LIST labels.
; ============================================================================

; ---- Region A: DIA10 disk-device entry stub (025-S3IRPIT, load 32000B) ----
>>> DIA10(SYMBOL-2-LIST) = 112532B
112532  006017  	STA ,X 17		; save caller A into register block slot 17
112533  150103  	TRR PCR			; switch page control register (context)
112534  146147  	RADD CLD SL DX		; X = L (return link)
112535  012000  	STT ,X 0		; save T (MON number) into block slot 0
112536  044124  	LDA 124			; = mem[112663B] (P-rel constant)
112537  150401  	IOF			; interrupts off (critical section)
112540  150103  	TRR PCR
112541  055120  	LDX I 120		; X = mem[112661B] (worker/block pointer)
112542  006017  	STA ,X 17
112543  150402  	ION			; interrupts on
112544  044422  	LDA ,B 22
112545  004412  	STA ,B 12		; store status word (,B 12)
112546  124062  	JMP 62		; -> 112630 (shared disk-stub tail)
112547  050425  	LDT ,B 25
112550  044421  	LDA ,B 21
112551  146154  	RADD CLD SA DL		; L = A
112552  044420  	LDA ,B 20		; A = sub-function / param word (,B 20)
112553  055106  	LDX I 106		; X = mem[112661B] worker pointer
112554  006017  	STA ,X 17
112555  150103  	TRR PCR
112556  146147  	RADD CLD SL DX
112557  012000  	STT ,X 0
112560  044102  	LDA 102			; (falls into DIA11 region, MON 333B)

; ---- Region B: MSYSU disk-mirror worker (025-S3IRPIT, load 32000B) ----
; 16-way DIMIR sub-function dispatcher (manual section 2.14).
>>> MSYSU(SYMBOL-2-LIST) = 132567B
132567  135145  	JPL I 145		; -> [132734] setup worker (pointer word)
132570  170404  	SAA 4
132571  150307  	MST PIE			; enable named interrupt bits by A
132572  054422  	LDX ,B 22
132573  135142  	JPL I 142		; -> [132735]
132574  004423  	STA ,B 23
132575  054422  	LDX ,B 22
132576  170400  	SAA 0
132577  135137  	JPL I 137		; -> [132736]
132600  170777  	SAA -1
132601  004425  	STA ,B 25
132602  045135  	LDA I 135		; A = mem[[132737]] mirror-flag word
132603  131015  	JAZ 15		; -> 132620 (flag zero: skip)
132604  054134  	LDX 134			; X = mem[132740B]
132605  046237  	LDA ,X -141
132606  131404  	JAF 4		; -> 132612
132607  044132  	LDA 132			; A = mem[132741B] error code
132610  004412  	STA ,B 12		; store status
132611  125131  	JMP I 131		; -> [132742] error exit
132612  135131  	JPL I 131		; -> [132743]
132613  050131  	LDT 131			; T = mem[132744B]
132614  135131  	JPL I 131		; -> [132745]
132615  175215  	BSKP ONE 10 DA		; if A bit1 set skip
132616  124002  	JMP 2		; -> 132620
132617  000425  	STZ ,B 25
132620  044420  	LDA ,B 20		; A = sub-function index
132621  171017  	SAT 17			; T = 17 (max index)
132622  141456  	SKP IF DT MGRE SA	; skip if 17 >= index (index <= 17)
132623  124022  	JMP 22		; -> 132645 (out-of-range default)
132624  146052  	RADD SA DP		; P = P + A : computed jump into table below
132625  124020  	JMP 20		; -> 132645  (index 0 -> default)
132626  124021  	JMP 21		; -> 132647  (index 1)
132627  124023  	JMP 23		; -> 132652  (index 2)
132630  124025  	JMP 25		; -> 132655  (index 3)
132631  124027  	JMP 27		; -> 132660  (index 4)
132632  124031  	JMP 31		; -> 132663  (index 5)
132633  124033  	JMP 33		; -> 132666  (index 6)
132634  124035  	JMP 35		; -> 132671  (index 7)
132635  124037  	JMP 37		; -> 132674  (index 10)
132636  124040  	JMP 40		; -> 132676  (index 11)
132637  124042  	JMP 42		; -> 132701  (index 12)
132640  124044  	JMP 44		; -> 132704  (index 13)
132641  124046  	JMP 46		; -> 132707  (index 14)
132642  124050  	JMP 50		; -> 132712  (index 15)
132643  124052  	JMP 52		; -> 132715  (index 16)
132644  124054  	JMP 54		; -> 132720  (index 17)
132645  044101  	LDA 101			; default: A = mem[132747B]
132646  124060  	JMP 60		; -> 132726 (status store)
132647  135100  	JPL I 100		; -> [132747] sub-function 1 worker
132650  124056  	JMP 56		; -> 132726
132651  124052  	JMP 52		; -> 132723
132652  135076  	JPL I 76		; -> [132750] sub-function 2 worker
132653  124053  	JMP 53		; -> 132726
132654  124047  	JMP 47		; -> 132723
132655  135074  	JPL I 74		; -> [132751] sub-function 3 worker
132656  124050  	JMP 50		; -> 132726
132657  124044  	JMP 44		; -> 132723
132660  135072  	JPL I 72		; -> [132752] sub-function 4 worker
132661  124045  	JMP 45		; -> 132726
132662  124041  	JMP 41		; -> 132723
132663  135070  	JPL I 70		; -> [132753] sub-function 5 worker
132664  124042  	JMP 42		; -> 132726
132665  124037  	JMP 37		; -> 132724
132666  135066  	JPL I 66		; -> [132754] sub-function 6 worker
132667  124037  	JMP 37		; -> 132726
132670  124034  	JMP 34		; -> 132724
132671  135064  	JPL I 64		; -> [132755] sub-function 7 worker
132672  124034  	JMP 34		; -> 132726
132673  124030  	JMP 30		; -> 132723
132674  135062  	JPL I 62		; -> [132756] sub-function 10 worker
132675  124026  	JMP 26		; -> 132723
132676  135061  	JPL I 61		; -> [132757] sub-function 11 worker
132677  124027  	JMP 27		; -> 132726
132700  124024  	JMP 24		; -> 132724
132701  135057  	JPL I 57		; -> [132760] sub-function 12 worker
132702  124024  	JMP 24		; -> 132726
132703  124020  	JMP 20		; -> 132723
132704  135055  	JPL I 55		; -> [132761] sub-function 13 worker
132705  124021  	JMP 21		; -> 132726
132706  124015  	JMP 15		; -> 132723
132707  135053  	JPL I 53		; -> [132762] sub-function 14 worker
132710  124016  	JMP 16		; -> 132726
132711  124013  	JMP 13		; -> 132724
132712  135051  	JPL I 51		; -> [132763] sub-function 15 worker
132713  124013  	JMP 13		; -> 132726
132714  124007  	JMP 7		; -> 132723
132715  135047  	JPL I 47		; -> [132764] sub-function 16 worker
132716  124010  	JMP 10		; -> 132726
132717  124004  	JMP 4		; -> 132723
132720  135045  	JPL I 45		; -> [132765] sub-function 17 worker
132721  124005  	JMP 5		; -> 132726
132722  124001  	JMP 1		; -> 132723
132723  170400  	SAA 0			; success status = 0
132724  040407  	MIN ,B 7		; increment counter ,B 7 (wraps? skip on zero)
132725  144400  	RAND 0 0		; nop-ish (dr=0 result suppressed)
132726  004412  	STA ,B 12		; store status word to caller (,B 12)
132727  044425  	LDA ,B 25
132730  143050  	SKP IF 0 LST SA		; skip if 0 < A (A > 0)
132731  124002  	JMP 2		; -> 132733
132732  135034  	JPL I 34		; -> [132766]
132733  125007  	JMP I 7		; -> [132742] return via caller link word
; ---- 132734B..132765B: JPL-I pointer / data table (disassembles as bogus code) ----
; These are the indirect-call pointer words used by the JPL I / JMP I above; the
; final callees live outside this carve (uncarved resident disk/mirror workers).
132734  000336  	; pointer word (000336)
132735  134430  	; pointer word (134430)
132736  134444  	; pointer word (134444)
132742  005003  	; caller return-link pointer word
132747  133040  	; sub-function worker pointer word (index 1)
132765  132767  	; last table word (control-flow closure boundary)
