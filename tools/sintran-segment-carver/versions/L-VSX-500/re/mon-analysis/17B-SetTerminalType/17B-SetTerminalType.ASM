; ============================================================================
; MON 17B (octal) SetTerminalType (MSTTY) - real SINTRAN L disassembly
; Sets the type of a terminal - how SINTRAN III handles it (see appendix H).
; Input: T = logical device number, A = terminal type. Public background users
; may only set the type for their own terminal.
; Two regions (dispatch is 3-layer; see README Code location):
;   A. GOTAB[17] level-14 entry stub  F1616 @120526B in segment 025-S3IRPIT (load 32000B)
;   B. MSTTY set-terminal-type worker  MSTTY @044626B in resident SINTRAN-DATA_commoncode (load 0)
;   (the A->B bridge is the uncarved resident CALLPROC - NOT present in these bytes.)
;
; MSTTY is the SET entry into a large shared terminal-service module. It is a
; 2-word prefix (SAA 20 / STA I 146) that presets a mode/opcode global (via link
; cell 044775 -> resident cell 000072) and then FALLS THROUGH into MGTTY = 044630B
; (MON 16B GetTerminalType). So 17B (set) and 16B (get) share ONE body, forked only
; by this MSTTY prefix. Identity here rests on the symbol NAME MSTTY (= the 17B
; short name in the manual) plus the fall-through; the F1616 -> MSTTY hop crosses
; the uncarved resident CALLPROC (see README caveats). Addresses/values are octal.
; ============================================================================

; ---- Region A: GOTAB[17] entry stub F1616 (025-S3IRPIT, load 32000B) ----
; F1616 = 120526B (SYMBOL-2-LIST); next symbol CSUMO = 120546B, so the stub is 16 words.
; A compiler entry stub (same shape as F1610/F1644): it loads through indirect pointer
; words and re-enters via an indirect JMP (LDX I 7 -> [120537], LDX I 10 -> [120540],
; JMP I 10 -> [120543]) whose pointer targets are populated at runtime; a static decode
; cannot resolve them. Words 120535-120545 decode as instructions but are the stub's
; constant/pointer cells (data). The real transfer to MSTTY is a runtime pointer /
; the resident CALLPROC.
120526  054332  	LDX -46
120527  146171  	RADD CLD SX DD
120530  055007  	LDX I 7			; indirect load via pointer word [120537]
120531  146173  	RADD CLD SX DB
120532  055010  	LDX I 10		; indirect load via pointer word [120540]
120533  125010  	JMP I 10		; -> deref [120543]  (INDIRECT, runtime pointer)
120534  124353  	JMP -25			; -> 120507  (DIRECT, out of stub head)
120535  061160  	ADD I 160		; constant/pointer cell (data)
120536  116234  	FDV ,X -144		; constant/pointer cell (data)
120537  061157  	ADD I 157		; pointer word (read by LDX I 7 at 120530)
120540  116665  	FDV ,X ,B -113		; pointer word (read by LDX I 10 at 120532)
120541  116036  	FDV ,X 36		; constant/pointer cell (data)
120542  004007  	STA 7			; constant/pointer cell (data)
120543  117107  	FDV I ,X 107		; pointer word (read by JMP I 10 at 120533)
120544  000000  	STZ 0			; constant cell (data)
120545  000000  	STZ 0			; constant cell (data)
; 120546 = CSUMO boundary (next symbol); not part of this stub.

; ---- Region B: MSTTY worker / shared body (SINTRAN-DATA_commoncode, load 0) ----
; MSTTY = 044626B (SYMBOL-1-LIST). This IS real executable code. The 2-word SET
; prefix presets the mode global, then falls through into the MGTTY shared body.
044626  170420  	SAA 20			; MSTTY entry: A := 20B (set-mode/opcode value)
044627  005146  	STA I 146		; -> mem[[044775]] = mem[000072] = 20B (mode global)
; ---- falls through into MGTTY = 044630B (the shared get/set body) ----
044630  044147  	LDA 147			; MGTTY: A = mem[044777] (nonzero mode const)
044631  005147  	STA I 147		; -> mem[[045000]] store (mode marker)
044632  044147  	LDA 147			; reload the same const
044633  004564  	STA ,B 164		; B+164 := const (set/get discriminator, nonzero)
044634  124003  	JMP 3			; -> 044637 (skip the flag-clear prologue)
044635  000564  	STZ ,B 164		; (alt prologue from STMT1 sibling) B+164 := 0
044636  001142  	STZ I 142		; (alt prologue) clear a working cell
044637  174000  	BSET ZRO SSPTM		; clear page-map protect bit (bracket priv access)
044640  055135  	LDX I 135		; X = mem[[044775]] = mem[000072] (mode/param global)
044641  057141  	LDX I ,X 141		; X = mem[ X + ... ] (terminal datafield ptr)
044642  014347  	STX -31			; save datafield pointer
044643  046001  	LDA ,X 1		; A = datafield[X+1]
044644  174200  	BSET ONE SSPTM		; restore page-map protect bit
044645  135136  	JPL I 136		; -> [045003] (link cell = 042146, terminal helper)
044646  146175  	RADD CLD SX DA		; A = X (copy result index)
044647  174000  	BSET ZRO SSPTM
044650  054341  	LDX -37			; X = mem[044611] (reload saved ptr)
044651  052000  	LDT ,X 0		; T = datafield[X+0]
044652  174200  	BSET ONE SSPTM
044653  131003  	JAZ 3			; if A==0 -> 044656
044654  142065  	SKP IF DA UEQ ST	; if A != T PC++
044655  124002  	JMP 2			; -> 044657
044656  135120  	JPL I 120		; -> [044776] (link cell = 000215, resident routine)
044657  004730  	STA ,B -50		; save A to frame
044660  146157  	RADD CLD SA DX		; X = A
044661  044564  	LDA ,B 164		; A = B+164 (the set/get discriminator)
044662  131004  	JAZ 4			; if discriminator==0 (get) -> 044666
044663  046027  	LDA ,X 27		; (SET path) A = datafield[X+27]
044664  174205  	BSET ONE 0 DA		; A |= 1  (mark terminal-type changed)
044665  006027  	STA ,X 27		; datafield[X+27] = A  (write terminal datafield)
044666  174000  	BSET ZRO SSPTM
044667  054322  	LDX -56			; X = mem[044624]
044670  046003  	LDA ,X 3		; A = datafield[X+3]
044671  004321  	STA -57			; store
044672  046002  	LDA ,X 2		; A = datafield[X+2]
044673  174200  	BSET ONE SSPTM
044674  004503  	STA ,B 103		; save
044675  174000  	BSET ZRO SSPTM
044676  046001  	LDA ,X 1		; A = datafield[X+1]
044677  174200  	BSET ONE SSPTM
044700  171400  	SAX 0			; X := 0
044701  007103  	STA I ,X 103		; indirect store
044702  055073  	LDX I 73		; X = mem[[045000 area]]
044703  057102  	LDX I ,X 102
044704  052000  	LDT ,X 0		; T = datafield[X+0]
; ---- T1P04: terminal-type compute (RDIV) ----
044705  044101  	LDA 101			; T1P04: A = mem[045006 area]
044706  146151  	RADD CLD SA DD		; D = A
044707  146105  	RADD CLD 0 DA		; A := 0  (build 32-bit A:D dividend)
044710  141660  	RDIV ST			; A = (A:D)/T ; D = remainder (type index calc)
044711  142001  	SKP IF DD UEQ 0		; if D != 0 PC++
044712  124002  	JMP 2			; -> 044714
044713  135063  	JPL I 63		; -> [044776] (link cell = 000215, resident routine)
044714  171400  	SAX 0
044715  007072  	STA I ,X 72		; store computed value
044716  004734  	STA ,B -44
044717  120071  	MPY 71			; A = A * mem[045010 area]
044720  060564  	ADD ,B 164		; A += B+164 (discriminator-weighted)
044721  171401  	SAX 1
044722  007067  	STA I ,X 67
044723  044730  	LDA ,B -50
044724  146153  	RADD CLD SA DB		; B = A
044725  055050  	LDX I 50
044726  047057  	LDA I ,X 57
044727  171400  	SAX 0
044730  006430  	STA ,X ,B 30
044731  171400  	SAX 0
044732  047052  	LDA I ,X 52
044733  171404  	SAX 4
044734  007050  	STA I ,X 50
044735  171400  	SAX 0
044736  047054  	LDA I ,X 54
044737  171404  	SAX 4
044740  007052  	STA I ,X 52
044741  171400  	SAX 0
044742  047045  	LDA I ,X 45
044743  171404  	SAX 4			; NOWAI
044744  007043  	STA I ,X 43
044745  054046  	LDX 46
044746  062164  	ADD ,X 164		; TNOWA
044747  171411  	SAX 11			; DKICK
044750  007041  	STA I ,X 41
044751  044241  	LDA -137
044752  050042  	LDT 42
044753  142065  	SKP IF DA UEQ ST
044754  124004  	JMP 4			; -> 044760
044755  050040  	LDT 40
044756  140065  	SKP IF DA EQL ST
044757  124010  	JMP 10			; -> 044767
044760  171042  	SAT 42
044761  171401  	SAX 1
044762  146105  	RADD CLD 0 DA		; A := 0
044763  146151  	RADD CLD SA DD		; D := 0
044764  135226  	JPL I -152		; -> [044612] (shared terminal-message helper; below window)
044765  135011  	JPL I 11		; -> [044776] (link cell = 000215, resident routine)
044766  124376  	JMP -2			; -> 044764 (in-body wait loop)
044767  171400  	SAX 0			; X := 0
044770  056430  	LDX ,X ,B 30
044771  044022  	LDA 22
044772  146153  	RADD CLD SA DB		; B = A
044773  014731  	STX ,B -47
044774  124022  	JMP 22			; -> 045016 (PL010, module continuation; past this window)

; ---- link cells + constants (DATA) 044775..045015 - table for this routine ----
; nd100-dis renders these pointer/constant words as bogus instructions; the real
; contents are the worker addresses / constants (resolved below).
044775  000072  	STZ 72			; = 000072  (mode/param global ptr; used by 044627/044640)
044776  000215  	STZ -163		; = 000215  (resident routine; used by 044656/044713/044765)
044777  040000  	MIN 0			; = 040000  (mode constant loaded by MGTTY 044630/044632)
045000  004367  	STA -11			; = 004367  (mode cell ptr; used by 044631/044636)
045001  100000  	FAD 0			; = 100000  (constant)
045002  041440  	MIN I ,B 40		; = 041440  (constant)
045003  042146  	MIN ,X 146		; = 042146  (terminal helper; used by 044645 JPL I 136)
045004  004441  	STA ,B 41		; = 004441  (constant)
045005  031371  	STF I -7		; = 031371  (constant)
045006  002000  	STZ ,X 0		; = 002000  (constant; used by 044705)
045007  004461  	STA ,B 61		; = 004461  (constant)
045010  000200  	STZ -200		; = 000200  (constant; used by 044717 MPY)
045011  004023  	STA 23			; = 004023  (constant)
045012  004451  	STA ,B 51		; = 004451  (constant)
045013  170400  	SAA 0			; = 170400  (constant)
045014  074436  	ORA ,B 36		; = 074436  (constant)
045015  102020  	FAD ,X 20		; = 102020  (constant)
; 045016 = PL010 (SYMBOL-2): shared-module continuation; NOT carved in this window.
