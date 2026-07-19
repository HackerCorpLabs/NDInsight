; ============================================================================
; MON 67B (octal) OutBufferSpace (OSIZE) - real SINTRAN L disassembly
; Two regions (the dispatch is 3-layer; see README Code location):
;   A. GOTAB[67] level-14 entry stub  F1642 @121372B in segment 025-S3IRPIT (load 32000B)
;   B. OSIZE buffer-scan worker body  OSIZE @044231B in resident SINTRAN-DATA_commoncode (load 0)
;   (the A->B bridge is the uncarved resident CALLPROC - NOT present in these bytes;
;    region B is a candidate worker, NOT statically reachable from F1642 - see README caveats)
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
; ============================================================================

; ---- Region A: GOTAB[67] entry stub F1642 (025-S3IRPIT, load 32000B) ----
; F1642 = 121372B (SYMBOL-2-LIST); next symbol F1643 = 121417B, so the stub is 21 words.
; 121400 MST PIE / 121401 EXIT is a monitor entry/exit wrapper; the real work happens
; via an indirect JPL I whose pointer is runtime-populated in this RPIT image.
; 121411..121416 decode as instructions but are constant/pointer words (data).
121372  146157  	RADD CLD SA DX
121373  044330  	LDA -50
121374  146153  	RADD CLD SA DB
121375  044331  	LDA -47
121376  146154  	RADD CLD SA DL
121377  170404  	SAA 4
121400  150307  	MST PIE			; enable interrupts
121401  146142  	EXIT			; monitor/subroutine return
121402  171002  	SAT 2
121403  140065  	SKP IF DA EQL ST
121404  124004  	JMP 4		; -> 121410  (DIRECT, in-overlay)
121405  135010  	JPL I 10		; -> deref [121415]  (INDIRECT, runtime pointer)
121406  144400  	RAND 0 0
121407  124334  	JMP -44		; -> 121343  (DIRECT, in-overlay, backward)
121410  124361  	JMP -17		; -> 121371  (DIRECT, in-overlay, backward)
121411  017622  	STX I ,B ,X -156	; APPEARS TO BE DATA
121412  061160  	ADD I 160		; APPEARS TO BE DATA
121413  000372  	STZ -6			; APPEARS TO BE DATA
121414  116374  	FDV ,X -4		; APPEARS TO BE DATA
121415  116036  	FDV ,X 36		; pointer word read by JPL I at 121405 = 116036B
121416  061157  	ADD I 157		; APPEARS TO BE DATA
121417  000000  	STZ 0			; F1643 boundary; 121417.. = 000000 padding

; ---- Region B: OSIZE buffer-scan worker (SINTRAN-DATA_commoncode, load 0) ----
; OSIZE = 044231B (SYMBOL-1-LIST). Per-device scan loop computing free output-buffer
; space via IOXT device I/O and interrupt-guarded (POF/PON) double-word pointer reads.
; CANDIDATE worker: real SINTRAN L bytes, but NOT byte-linked to F1642 (see README).
; 044343..044352 is a data/pointer block (JMP 10 at 044343 jumps over it).
044231  060730  	ADD ,B -50
044232  061205  	ADD I -173
044233  001777  	STZ I ,B -1
044234  000776  	STZ ,B -2
044235  044107  	LDA 107
044236  150105  	TRR IIE
044237  150005  	TRA IIC
044240  054776  	LDX ,B -2			; loop head: X = index
044241  027104  	LDD I ,X 104		; load double-word device-table entry
044242  171377  	SAT -1
044243  142065  	SKP IF DA UEQ ST		; end-of-table sentinel test
044244  124077  	JMP 77		; -> 044343  (exit loop -> data/return block)
044245  131072  	JAZ 72		; -> 044337  (skip entry -> loop advance)
044246  020745  	STD ,B -33			; save device-table pointer
044247  146157  	RADD CLD SA DX
044250  052375  	LDT ,X -3
044251  173002  	AAT 2
044252  150415  	IOXT			; device I/O: read buffer status
044253  146151  	RADD CLD SA DD
044254  150005  	TRA IIC
044255  131027  	JAZ 27		; -> 044304  (branch on device class)
044256  044746  	LDA ,B -32
044257  070067  	AND 67
044260  146156  	RADD CLD SA DT
044261  044746  	LDA ,B -32
044262  156572  	SHA ZIN SHR 6
044263  146157  	RADD CLD SA DX
044264  056417  	LDX ,X ,B 17
044265  146067  	RADD ST DX
044266  146467  	RADD AD1 ST DX
044267  150404  	POF			; interrupts off
044270  026000  	LDD ,X 0			; read buffer descriptor (double word)
044271  150410  	PON			; interrupts on
044272  050745  	LDT ,B -33
044273  140065  	SKP IF DA EQL ST
044274  124007  	JMP 7		; -> 044303
044275  140061  	SKP IF DD EQL ST
044276  124005  	JMP 5		; -> 044303
044277  150404  	POF
044300  002000  	STZ ,X 0			; buffer empty: zero the pair
044301  002001  	STZ ,X 1
044302  150410  	PON
044303  124034  	JMP 34		; -> 044337
044304  175261  	BSKP ONE 60 DD
044305  124011  	JMP 11		; -> 044316
044306  146115  	RADD CLD SD DA
044307  156570  	SHA ZIN SHR 10
044310  156406  	SHA ZIN 6
044311  146151  	RADD CLD SA DD
044312  170477  	SAA 77
044313  054745  	LDX ,B -33
044314  006027  	STA ,X 27
044315  124014  	JMP 14		; -> 044331
044316  175301  	BSKP ONE 100 DD
044317  124002  	JMP 2		; -> 044321
044320  135027  	JPL I 27		; -> deref [044347]  (INDIRECT, pointer block)
044321  146115  	RADD CLD SD DA
044322  156570  	SHA ZIN SHR 10
044323  070025  	AND 25
044324  156406  	SHA ZIN 6
044325  146151  	RADD CLD SA DD
044326  044023  	LDA 23
044327  054745  	LDX ,B -33
044330  006027  	STA ,X 27
044331  146015  	RADD SD DA			; assemble free-byte count into A
044332  144015  	SWAP SD DA
044333  054776  	LDX ,B -2
044334  174000  	BSET ZRO SSPTM
044335  023015  	STD I ,X 15		; store result
044336  174200  	BSET ONE SSPTM
044337  044776  	LDA ,B -2			; loop advance: index += 2
044340  172402  	AAA 2
044341  004776  	STA ,B -2
044342  124276  	JMP -102		; -> 044240  (back to loop head)
044343  124010  	JMP 10		; -> 044353  (jump over data block)
044344  000200  	STZ -200		; DATA
044345  171115  	SAT 115		; DATA
044346  000077  	STZ 77			; DATA
044347  000215  	STZ -163		; DATA: pointer word = 000215B (deref by JPL I 044320)
044350  000375  	STZ -3			; DATA
044351  000377  	STZ -1			; DATA
044352  036127  	LDF ,X 127		; DATA
044353  045046  	LDA I 46			; tail block (UNVERIFIED role; sibling scan)
044354  131037  	JAZ 37		; -> 044413
044355  171400  	SAX 0
044356  047741  	LDA I ,B ,X -37
044357  131433  	JAF 33		; -> 044412
044360  045042  	LDA I 42
044361  131411  	JAF 11		; -> 044372
044362  044611  	LDA ,B -167
044363  055036  	LDX I 36
044364  146106  	RADD CLD 0 DT
044365  135036  	JPL I 36		; -> 044423
044366  135036  	JPL I 36		; -> 044424
044367  065032  	SUB I 32
044370  172401  	AAA 1
044371  005031  	STA I 31
044372  146151  	RADD CLD SA DD
044373  044032  	LDA 32
044374  065025  	SUB I 25
044375  060031  	ADD 31
044376  051023  	LDT I 23
044377  146107  	RADD CLD 0 DX
044400  142006  	SKP IF DT UEQ 0
044401  124007  	JMP 7		; -> 044410
044402  023741  	STD I ,B ,X -37
044403  172401  	AAA 1
044404  146401  	RADD AD1 0 DD
044405  173402  	AAX 2
044406  173377  	AAT -1
044407  124371  	JMP -7		; -> 044400
044410  170777  	SAA -1
044411  007741  	STA I ,B ,X -37
044412  124006  	JMP 6		; -> 044420
044413  171400  	SAX 0
044414  047741  	LDA I ,B ,X -37
044415  131403  	JAF 3		; -> 044420
044416  170777  	SAA -1
044417  007741  	STA I ,B ,X -37
044420  124007  	JMP 7		; -> 044427
044421  117552  	FDV I ,B ,X 152
044422  117553  	FDV I ,B ,X 153
044423  057652  	LDX I ,B ,X -126
044424  000215  	STZ -163
044425  000200  	STZ -200
044426  161000  	IOT 1000
044427  146107  	RADD CLD 0 DX
