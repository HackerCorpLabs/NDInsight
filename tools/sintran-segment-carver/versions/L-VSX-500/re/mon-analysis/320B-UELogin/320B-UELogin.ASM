; ============================================================================
; MON 320B (octal) UELogin - real SINTRAN L disassembly
; User Environment login (manual section 2.14).
;
; Two regions (the dispatch is 3-layer; see README Code location):
;   A. GOTAB[320] target stub   DSI6 @112616B  in segment 025-S3IRPIT (load 32000B)
;   B. UE-login worker body      UELOG @050726B in resident commoncode  (load 0)
;   (the A->B bridge is the uncarved resident CALLPROC - not in these bytes)
;
; Region A is what GOTAB[320] byte-resolves to, but its content is device /
; interrupt-service code (IOF / ION / MCL PIE / TRR PCR), NOT login code - so the
; GOTAB[320] index is MISATTRIBUTED for this call (see README Honest caveats).
; Region B is the routine actually named UELOG in the version-matched L07 symbol
; table, sitting inside the UE-* cluster (UECHE, UECOM, UELOG) - the real body.
;
; Addresses and values are octal.
; ============================================================================

; ---- Region A: GOTAB[320] target stub  DSI6 @112616B (025-S3IRPIT) ----
; 23 words, bounded by the next symbol DSI7=112645B. Device/interrupt code -
; unrelated to UE login; documents what the 320 index resolves to, not a body.
112616  150103  	TRR PCR
112617  055042  	LDX I 42
112620  006017  	STA ,X 17
112621  150402  	ION
112622  044422  	LDA ,B 22
112623  060424  	ADD ,B 24
112624  172401  	AAA 1
112625  174365  	BSET ONE 160 DA
112626  004412  	STA ,B 12
112627  124001  	JMP 1		; -> 112630
112630  044426  	LDA ,B 26
112631  135032  	JPL I 32		; -> 112663
112632  150402  	ION
112633  045031  	LDA I 31
112634  131002  	JAZ 2		; -> 112636
112635  135030  	JPL I 30		; -> 112665
112636  125030  	JMP I 30		; -> 112666
112637  170404  	SAA 4
112640  150401  	IOF
112641  150207  	MCL PIE
112642  054426  	LDX ,B 26
112643  144037  	SWAP SB DX
112644  146171  	RADD CLD SX DD

; ---- Region B: UE-login worker body  UELOG @050726B (resident commoncode) ----
; 64 words, bounded by the next symbol MYGTL=051026B. The routine begins with a
; table-scan loop (050726-050747), holds an embedded pointer/data block
; (050751-050776, data-before/among-code pattern - NOT instructions), then a
; second code stretch (050777-051025). Real SINTRAN L bytes.
050726  173410  	AAX 10
050727  173410  	AAX 10
050730  040757  	MIN ,B -21
050731  124014  	JMP 14		; -> 050745
050732  045036  	LDA I 36
050733  131412  	JAF 12		; -> 050745
050734  051033  	LDT I 33
050735  143350  	LDATX 5
050736  174215  	BSET ONE 10 DA
050737  143354  	STATX 5
050740  173410  	AAX 10
050741  143350  	LDATX 5
050742  174215  	BSET ONE 10 DA
050743  143354  	STATX 5
050744  173410  	AAX 10
050745  040404  	MIN ,B 4
050746  040404  	MIN ,B 4
050747  124340  	JMP -40		; -> 050707
; --- 050751..050776: embedded pointer/data block (data, not code) ---
050750  124177  	JMP 177		; -> 051147
050751  057655  	LDX I ,B ,X -123
050752  000215  	STZ -163
050753  005303  	STA I -75
050754  005304  	STA I -74
050755  061165  	ADD I 165
050756  063461  	ADD I ,B ,X 61
050757  063657  	ADD I ,B ,X -121
050760  063172  	ADD I ,X 172
050761  005115  	STA I 115
050762  117552  	FDV I ,B ,X 152
050763  115542  	FDV I ,B 142
050764  000422  	STZ ,B 22
050765  000010  	STZ 10
050766  004321  	STA -57
050767  004320  	STA -60
050770  000071  	STZ 71
050771  001022  	STZ I 22
050772  000000  	STZ 0
050773  000000  	STZ 0
050774  000000  	STZ 0
050775  000000  	STZ 0
050776  000000  	STZ 0
; --- 050777..051025: second code stretch ---
050777  020376  	STD -2
051000  146135  	RADD CLD SB DA
051001  004373  	STA -5
051002  044137  	LDA 137
051003  004367  	STA -11
051004  044366  	LDA -12
051005  050135  	LDT 135
051006  143465  	SKP IF DA MLST ST
051007  124015  	JMP 15		; -> 051024
051010  054362  	LDX -16
051011  046001  	LDA ,X 1
051012  146153  	RADD CLD SA DB
051013  131005  	JAZ 5		; -> 051020
051014  044361  	LDA -17
051015  020674  	STD ,B -104
051016  170410  	SAA 10
051017  146051  	RADD SA DD
051020  044352  	LDA -26
051021  172402  	AAA 2
051022  004350  	STA -30
051023  124361  	JMP -17		; -> 051004
051024  024351  	LDD -27
051025  050116  	LDT 116
