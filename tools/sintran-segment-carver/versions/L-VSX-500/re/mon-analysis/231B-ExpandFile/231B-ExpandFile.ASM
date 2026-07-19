; ============================================================================
; MON 231B (octal) ExpandFile - real SINTRAN L disassembly
; One functional region (dispatch is 3-layer; GOTAB[231] = 000000 = fall-through,
; no stub):
;   EXPFI expand-file entry + shared create/allocate dispatcher body
;   EXPFI @105555B  in segment 006-S3FS (load 26000B)
;   (the MON 231 -> worker bridge is the uncarved resident MFELL/CALLPROC - not in
;    these bytes; there is no per-call GOTAB entry stub because the slot is zero)
; Addresses and values are octal.
;
; EXPFI shares one dispatcher body with three sibling directory-allocation entries
; that enter through short stubs just above and join the shared code at 105564;
; each sets a different SSK/SSM skip-flag pair selecting the operation:
;   105552 SFACC  SSM=1 SSK=1  -> mode 3
;   105555 EXPFI  SSM=1 SSK=0  -> mode 2   ExpandFile
;   105560 CRALN  SSM=0 SSK=1  -> mode 1
;   105562 CRALF  SSM=0 SSK=0  -> mode 0
; The mode value (0..3) is folded from SSM/SSK at 105571-105574 and stored at
; B+123; the second-phase ladder (105721-105776) dispatches through the link cells
; at 106021-106042 to workers MSFLA / MEXFI / MCRFI / MCRNW / MALFI / MALNE. For
; ExpandFile the mode is 2 -> MEXFI (Modify EXpand FIle) at 106032 = 116623.
; The exit link cell 106042 = 003776 is the resident return.
;
; Words 105664-105677 and 106021-106042 are POINTER TABLES (link cells), not code;
; nd100-dis renders them as bogus STZ/STF/MPY/FDV/... instructions. Their contents
; are the real worker addresses (resolved in trailing comments).
; ============================================================================

; ---- Sibling entry stubs (SSK/SSM preset, join at 105564) ----
105552  174270  	BSET ONE SSM		; SFACC entry  (mode 3)
105553  174220  	BSET ONE SSK
105554  124010  	JMP 10		; -> 105564
105555  174270  	BSET ONE SSM		; EXPFI entry  (mode 2)  ExpandFile
105556  174020  	BSET ZRO SSK
105557  124005  	JMP 5		; -> 105564
105560  174220  	BSET ONE SSK		; CRALN entry  (mode 1)
105561  124002  	JMP 2		; -> 105563
105562  174020  	BSET ZRO SSK		; CRALF entry  (mode 0)
105563  174070  	BSET ZRO SSM

; ---- Shared create/allocate dispatcher body ----
105564  021077  	STD I 77		; -> 105663 (link cell area) stash caller D
105565  146145  	RADD CLD SL DA		; A = L
105566  146131  	RADD CLD SB DD		; D = B
105567  170145  	SAB 145			; establish local frame (B), 145B words
105570  135074  	JPL I 74		; -> 105664 (link cell = 003752, resident prologue)
105571  146105  	RADD CLD 0 DA		; A = 0
105572  157402  	SHA LIN 2		; A <<= 2 (M-bit link fill)  fold SSM
105573  174605  	BSET BAC 0 DA		; A bit0 = K             fold SSK
105574  004523  	STA ,B 123		; B+123 = operation mode (0..3); EXPFI => 2
105575  045070  	LDA I 70		; -> 105665 param via link
105576  004540  	STA ,B 140
105577  045067  	LDA I 67		; -> 105666
105600  004541  	STA ,B 141
105601  045066  	LDA I 66		; -> 105667
105602  004542  	STA ,B 142
105603  044065  	LDA 65
105604  146035  	RADD SB DA
105605  004543  	STA ,B 143
105606  044063  	LDA 63
105607  146035  	RADD SB DA
105610  004544  	STA ,B 144
105611  044061  	LDA 61
105612  146035  	RADD SB DA
105613  004535  	STA ,B 135
105614  044057  	LDA 57
105615  146035  	RADD SB DA
105616  004536  	STA ,B 136
105617  044055  	LDA 55
105620  146035  	RADD SB DA
105621  004537  	STA ,B 137
105622  054400  	LDX ,B 0
105623  044543  	LDA ,B 143
105624  050051  	LDT 51
105625  135051  	JPL I 51		; -> 105676 (link cell = 031075, USCPS set-user-context)
105626  124162  	JMP 162		; -> 106010 (store-status exit)
105627  044523  	LDA ,B 123		; mode
105630  171003  	SAT 3
105631  143065  	SKP IF DA LST ST	; skip if mode < 3
105632  124007  	JMP 7		; -> 105641
105633  054401  	LDX ,B 1
105634  044544  	LDA ,B 144
105635  171002  	SAT 2
105636  135041  	JPL I 41		; -> 105677 (link cell = 031067, USCPB set-user-context/B)
105637  124151  	JMP 151		; -> 106010 (store-status exit)
105640  124020  	JMP 20		; -> 105660
105641  054402  	LDX ,B 2
105642  044535  	LDA ,B 135
105643  171006  	SAT 6
105644  135032  	JPL I 32		; -> 105676 (link cell = 031075, USCPS)
105645  124143  	JMP 143		; -> 106010 (store-status exit)
105646  054403  	LDX ,B 3
105647  044536  	LDA ,B 136
105650  171006  	SAT 6
105651  135025  	JPL I 25		; -> 105676 (link cell = 031075, USCPS)
105652  124136  	JMP 136		; -> 106010 (store-status exit)
105653  054401  	LDX ,B 1
105654  044537  	LDA ,B 137
105655  171006  	SAT 6
105656  135020  	JPL I 20		; -> 105676 (link cell = 031075, USCPS)
105657  124131  	JMP 131		; -> 106010 (store-status exit)
105660  135020  	JPL I 20		; -> 105700 (in-body target)
105661  124040  	JMP 40		; -> 105721 (second-phase mode dispatch)
105662  124017  	JMP 17		; -> 105701
105663  147417  	ROP NOOP		; (data) pad / link-cell area follows
; ---- link cells (DATA, not code) 105664..105677 ----
105664  003752  	STZ I ,B ,X -26		; (data) link -> 003752 (resident prologue worker)
105665  146274  	RADD CM1 SX DL		; (data)
105666  146275  	RADD CM1 SX DA		; (data)
105667  146276  	RADD CM1 SX DT		; (data)
105670  000006  	STZ 6			; (data)
105671  000121  	STZ 121			; (data)
105672  000124  	STZ 124			; (data)
105673  000127  	STZ 127			; (data)
105674  000132  	STZ 132			; (data)
105675  000203  	STZ -175		; (data)
105676  031075  	STF I 75		; (data) link -> 031075 (USCPS)
105677  031067  	STF I 67		; (data) link -> 031067 (USCPB)

105700  053114  	LDT I ,X 114
105701  170420  	SAA 20
105702  171400  	SAX 0
105703  006510  	STA ,X ,B 110
105704  050115  	LDT 115
105705  146036  	RADD SB DT
105706  054114  	LDX 114
105707  146037  	RADD SB DX
105710  135113  	JPL I 113		; -> 106023 (link cell = 043100, SEPUS)
105711  124101  	JMP 101		; -> 106012
105712  045112  	LDA I 112		; -> 106024
105713  135112  	JPL I 112		; -> 106025 (link cell -> 010500, resident)
105714  135112  	JPL I 112		; -> 106026 (link cell = 114350, MENSY)
105715  124077  	JMP 77		; -> 106014
105716  045106  	LDA I 106		; -> 106024
105717  135110  	JPL I 110		; -> 106027 (link cell -> 010506, resident)
105720  135110  	JPL I 110		; -> 106030 (link cell = 101127, DECTS)
; ---- second-phase mode dispatch (105721-105776) ----
105721  044523  	LDA ,B 123		; mode
105722  171003  	SAT 3
105723  140065  	SKP IF DA EQL ST	; mode == 3 ?
105724  124011  	JMP 11		; -> 105735
105725  054543  	LDX ,B 143
105726  044536  	LDA ,B 136
105727  146151  	RADD CLD SA DD		; D = A
105730  044535  	LDA ,B 135
105731  050537  	LDT ,B 137
105732  135077  	JPL I 77		; -> 106031 (link cell = 120752, MSFLA set-file-alloc) mode 3
105733  124055  	JMP 55		; -> 106010 (store-status exit)
105734  124043  	JMP 43		; -> 105777 (success)
105735  044523  	LDA ,B 123		; mode
105736  171002  	SAT 2
105737  140065  	SKP IF DA EQL ST	; mode == 2 ?  (ExpandFile)
105740  124006  	JMP 6		; -> 105746
105741  054543  	LDX ,B 143
105742  050544  	LDT ,B 144
105743  135067  	JPL I 67		; -> 106032 (link cell = 116623, MEXFI expand-file) mode 2
105744  124044  	JMP 44		; -> 106010 (store-status exit)
105745  124032  	JMP 32		; -> 105777 (success)
105746  044402  	LDA ,B 2		; modes 0/1: create/allocate
105747  131415  	JAF 15		; -> 105764
105750  044403  	LDA ,B 3
105751  131413  	JAF 13		; -> 105764
105752  054543  	LDX ,B 143
105753  050544  	LDT ,B 144
105754  044523  	LDA ,B 123
105755  131404  	JAF 4		; -> 105761
105756  135055  	JPL I 55		; -> 106033 (link cell = 115430, MCRFI create-file)
105757  124031  	JMP 31		; -> 106010 (store-status exit)
105760  124003  	JMP 3		; -> 105763
105761  135053  	JPL I 53		; -> 106034 (link cell = 115433, MCRNW create-new)
105762  124026  	JMP 26		; -> 106010 (store-status exit)
105763  124014  	JMP 14		; -> 105777 (success)
105764  054543  	LDX ,B 143
105765  050544  	LDT ,B 144
105766  044523  	LDA ,B 123
105767  131405  	JAF 5		; -> 105774
105770  024402  	LDD ,B 2
105771  135044  	JPL I 44		; -> 106035 (link cell = 116130, MALFI allocate-file)
105772  124016  	JMP 16		; -> 106010 (store-status exit)
105773  124004  	JMP 4		; -> 105777 (success)
105774  024402  	LDD ,B 2
105775  135041  	JPL I 41		; -> 106036 (link cell = 116133, MALNE allocate-new)
105776  124012  	JMP 12		; -> 106010 (store-status exit)
; ---- success exit + status return (105777-106020) ----
105777  040404  	MIN ,B 4		; bump status
106000  044540  	LDA ,B 140
106001  005036  	STA I 36		; -> 106037
106002  044541  	LDA ,B 141
106003  005035  	STA I 35		; -> 106040
106004  044542  	LDA ,B 142
106005  005034  	STA I 34		; -> 106041
106006  170633  	SAA -145
106007  125033  	JMP I 33		; -> 106042 (link cell = 003776, resident return)
; ---- error path: store status word (106010-106020) ----
106010  004402  	STA ,B 2		; store status -> caller slot B+2
106011  124367  	JMP -11		; -> 106000
106012  130707  	JAN -71		; -> 105721
106013  124375  	JMP -3		; -> 106010
106014  146157  	RADD CLD SA DX		; X = A
106015  045007  	LDA I 7			; -> 106024
106016  135011  	JPL I 11		; -> 106027 (link cell -> 010506, resident)
106017  146175  	RADD CLD SX DA		; A = X
106020  124370  	JMP -10		; -> 106010
; ---- link cells (DATA, not code) 106021..106042 ----
106021  000006  	STZ 6			; (data)
106022  000110  	STZ 110			; (data)
106023  043100  	MIN I ,X 100		; (data) link -> 043100 (SEPUS)
106024  120027  	MPY 27			; (data)
106025  010500  	STT ,B 100		; (data) link -> 010500 (resident/aux)
106026  114350  	FDV -30			; (data) link -> 114350 (MENSY)
106027  010506  	STT ,B 106		; (data) link -> 010506 (resident/aux)
106030  101127  	FAD I 127		; (data) link -> 101127 (DECTS)
106031  120752  	MPY ,B -26		; (data) link -> 120752 (MSFLA)
106032  116623  	FDV ,X ,B -155		; (data) link -> 116623 (MEXFI expand-file) <- ExpandFile
106033  115430  	FDV I ,B 30		; (data) link -> 115430 (MCRFI)
106034  115433  	FDV I ,B 33		; (data) link -> 115433 (MCRNW)
106035  116130  	FDV ,X 130		; (data) link -> 116130 (MALFI)
106036  116133  	FDV ,X 133		; (data) link -> 116133 (MALNE)
106037  146274  	RADD CM1 SX DL		; (data)
106040  146275  	RADD CM1 SX DA		; (data)
106041  146276  	RADD CM1 SX DT		; (data)
106042  003776  	STZ I ,B ,X -2		; (data) link -> 003776 (resident return)
