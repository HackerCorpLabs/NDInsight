; ============================================================================
; MON 16B (octal) GetTerminalType (MGTTY) - real SINTRAN L disassembly
; Gets the terminal type - how SINTRAN III handles a particular terminal
; (see appendix H for the type numbers). Input: T = logical device number;
; output: A = terminal type.
;
; Dispatch is a FALL-THROUGH: GOTAB[16] = 000000 (no per-call vector); the call
; is routed by the resident MFELL/CALLPROC mechanism - NOT present in these bytes.
; The named worker is MGTTY = 044630B in resident SINTRAN-DATA_commoncode (load 0).
;
; MGTTY is a GET entry into a large shared terminal-service module. Its immediate
; sibling MSTTY = 044626B (MON 17B SetTerminalType) sits two words BELOW it and
; FALLS THROUGH into MGTTY: MSTTY only presets a mode/opcode global (SAA 20 /
; STA I 146, via link cell 044775 -> resident cell 000072) and then executes the
; SAME body. So 16B (get) and 17B (set) share one body, forked only by that
; 2-word MSTTY prefix. Identity here rests on the symbol NAME MGTTY (= the 16B
; short name in the manual); the fall-through -> worker link crosses the uncarved
; resident CALLPROC (see README caveats). Addresses and values are octal.
; ============================================================================

; ---- sibling SET entry MSTTY (044626B) - shown for the shared-body fork ----
; NOT part of MON 16B. MSTTY presets the mode global then falls through to MGTTY.
; 044626  170420  	SAA 20			; A := 20B (set-mode/opcode value)
; 044627  005146  	STA I 146		; -> mem[[044775]] = mem[000072] = 20B (mode global)

; ---- Region: MGTTY worker / shared body (SINTRAN-DATA_commoncode, load 0) ----
; MGTTY = 044630B (SYMBOL-1-LIST). This IS real executable code. It is the GET
; entry into the shared terminal-service module; the 044635-044636 alternate
; prologue is reached only from the STMT1 sibling (044625 JMP 10), not from here.
044630  044147  	LDA 147			; MGTTY entry: A = mem[044777] (nonzero mode const)
044631  005147  	STA I 147		; -> mem[[045000]] store (get-mode marker)
044632  044147  	LDA 147			; reload the same const
044633  004564  	STA ,B 164		; B+164 := const (set/get discriminator, nonzero)
044634  124003  	JMP 3			; -> 044637 (skip the flag-clear prologue)
044635  000564  	STZ ,B 164		; (alt prologue from STMT1 sibling) B+164 := 0
044636  001142  	STZ I 142		; (alt prologue) clear a working cell
044637  174000  	BSET ZRO SSPTM		; clear page-map protect bit (bracket priv access)
044640  055135  	LDX I 135		; X = mem[[044775]] = mem[000072] (mode/param global)
044641  057141  	LDX I ,X 141		; X = mem[ X + mem[044776 area] ] (terminal datafield ptr)
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
044663  046027  	LDA ,X 27		; (set path) A = datafield[X+27]
044664  174205  	BSET ONE 0 DA		; A |= 1  (mark datafield changed)
044665  006027  	STA ,X 27		; datafield[X+27] = A
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
045000  004367  	STA -11			; = 004367  (get-mode cell ptr; used by 044631/044636)
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
