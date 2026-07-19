; ============================================================================
; MON 244B (octal) GetDirEntry - real SINTRAN L disassembly
; Two regions (the dispatch is 3-layer; see README Code location):
;   A. GOTAB[244] level-14 entry stub  DFRRT @44226B  in segment 025-S3IRPIT (load 32000B)
;   B. GDIEN Get-Directory-Entry worker GDIEN @107111B in segment 006-S3FS   (load 26000B)
;   (the A->B bridge is the uncarved resident CALLPROC - not in these bytes)
;
; Region A is bounded strictly to the next symbol PL006=44234B (6 words). All of
; its direct branches leave the stub into shared level-14 tail code in the same
; segment (44254B, 44263B, 44264B) - shared code, NOT the GDIEN worker, and NOT
; swallowed here (see README Honest caveats). The value 107111 (GDIEN) does not
; occur anywhere in 025-S3IRPIT, so the stub->worker step is the uncarved bridge.
;
; Region B: GDIEN @107111B is a 3-word flag-setting ENTRY that joins a body
; shared with two sibling entries - WDIEN @107106B (Write Dir Entry, SSM=1) and
; GNAEN @107114B (Get Name Entry, SSK=1). GDIEN sets SSK=0 and SSM=0 (get a
; directory entry, read not write) then joins the common body at 107116B. The
; body calls GDIRA (Get Directory Address @30225B) and GNAMA (Get Name Address
; @30235B) - the directory-entry access primitives - which is the byte-level
; reason GDIEN is the GetDirEntry worker.
;
; Region B contains TWO pointer tables (link cells), not code: 107230B..107247B
; and 107366B..107400B. nd100-dis renders those pointer words as bogus
; instructions; their contents are the real worker addresses (resolved in the
; trailing comments). Addresses and values are octal.
; ============================================================================

; ---- Region A: GOTAB[244] entry stub (025-S3IRPIT, load 32000B) ----
044226  144400  	RAND 0 0		; DFRRT entry: clear A (RAND 0,0)
044227  124025  	JMP 25		; -> 044254 (shared level-14 tail; uncarved link)
044230  124024  	JMP 24		; -> 044254 (shared level-14 tail; uncarved link)
044231  135032  	JPL I 32		; -> 044263 (shared level-14 routine, sibling area)
044232  135032  	JPL I 32		; -> 044264 (shared level-14 routine, sibling area)
044233  124021  	JMP 21		; -> 044254 (shared level-14 tail; uncarved link)

; ---- Region B: GDIEN Get-Directory-Entry worker (006-S3FS, load 26000B) ----
; sibling entries (NOT part of GDIEN, shown for context only):
;   107106 WDIEN  BSET ONE SSM / BSET ZRO SSK / JMP 6 -> 107116  (Write Dir Entry)
;   107114 GNAEN  BSET ONE SSK / BSET ZRO SSM /        -> 107116  (Get Name Entry)
107111  174020  	BSET ZRO SSK		; GDIEN entry: SSK=0 (directory entry, not name)
107112  174070  	BSET ZRO SSM		; SSM=0 (get/read, not write)
107113  124003  	JMP 3		; -> 107116 (join shared body)
; ---- shared body (entered by GDIEN/WDIEN/GNAEN) ----
107116  021112  	STD I 112		; stash caller D (parameter block)
107117  146145  	RADD CLD SL DA
107120  146131  	RADD CLD SB DD
107121  170106  	SAB 106			; establish local frame (B), 106 words
107122  135107  	JPL I 107		; -> 107231 (link cell = 003752, resident prologue worker)
107123  175270  	BSKP ONE SSM		; latch write flag ...
107124  124004  	JMP 4		; -> 107130
107125  170401  	SAA 1
107126  004504  	STA ,B 104		; ... -> B+104 (write flag; 0 for GDIEN)
107127  124002  	JMP 2		; -> 107131
107130  000504  	STZ ,B 104
107131  175220  	BSKP ONE SSK		; latch name flag ...
107132  124004  	JMP 4		; -> 107136
107133  170401  	SAA 1
107134  004505  	STA ,B 105		; ... -> B+105 (name flag; 0 for GDIEN)
107135  124002  	JMP 2		; -> 107137
107136  000505  	STZ ,B 105
107137  044505  	LDA ,B 105
107140  131451  	JAF 51		; -> 107211 (get-entry path)
107141  044504  	LDA ,B 104
107142  131447  	JAF 47		; -> 107211
107143  175376  	BSKP ONE 170 DT
107144  124045  	JMP 45		; -> 107211
107145  044065  	LDA 65
107146  146035  	RADD SB DA
107147  171105  	SAT 105
107150  054403  	LDX ,B 3
107151  135062  	JPL I 62		; -> 107233 (link cell = 031075, USCPS)
107152  125062  	JMP I 62		; -> 107234 (link cell = 107364, store-status exit)
107153  146151  	RADD CLD SA DD
107154  146157  	RADD CLD SA DX
107155  044060  	LDA 60
107156  135060  	JPL I 60		; -> 107236 (link cell = 046231, FLPAR)
107157  125055  	JMP I 55		; -> 107234 (link cell = 107364, store-status exit)
107160  050401  	LDT ,B 1
107161  174176  	BSET ZRO 170 DT
107162  010503  	STT ,B 103
107163  044054  	LDA 54
107164  146035  	RADD SB DA
107165  055053  	LDX I 53
107166  006024  	STA ,X 24
107167  044043  	LDA 43
107170  146035  	RADD SB DA
107171  006025  	STA ,X 25
107172  044047  	LDA 47
107173  146035  	RADD SB DA
107174  006026  	STA ,X 26
107175  171433  	SAX 33
107176  135044  	JPL I 44		; -> 107242 (link cell = 020274, resident worker)
107177  124165  	JMP 165		; -> 107364 (store-status exit)
107200  044400  	LDA ,B 0
107201  054040  	LDX 40
107202  146037  	RADD SB DX
107203  171025  	SAT 25
107204  135037  	JPL I 37		; -> 107243 (link cell = 031064, SUCPB)
107205  124157  	JMP 157		; -> 107364 (store-status exit)
107206  044501  	LDA ,B 101
107207  004402  	STA ,B 2		; store status -> caller slot B+2
107210  124150  	JMP 150		; -> 107360 (return)
107211  044505  	LDA ,B 105		; get-entry: branch on name flag
107212  131405  	JAF 5		; -> 107217
107213  135031  	JPL I 31		; -> 107244 (link cell = 030225, GDIRA get directory address)
107214  171025  	SAT 25
107215  044030  	LDA 30
107216  124004  	JMP 4		; -> 107222
107217  135027  	JPL I 27		; -> 107246 (link cell = 030235, GNAMA get name address)
107220  171016  	SAT 16
107221  044026  	LDA 26
107222  014502  	STX ,B 102
107223  146675  	RSUB SX DA
107224  141050  	SKP IF 0 GRE SA
107225  124023  	JMP 23		; -> 107250
107226  170574  	SAA 174			; error 174
107227  125005  	JMP I 5		; -> 107234 (link cell = 107364, store-status exit)
; ---- pointer table (DATA, not code) 107230..107247 ----
107230  147417  	ROP NOOP		; (data) padding
107231  003752  	STZ I ,B ,X -26		; (data) link -> 003752 (resident prologue worker)
107232  000006  	STZ 6			; (data)
107233  031075  	STF I 75		; (data) link -> 031075 (USCPS)
107234  107364  	FSB I ,X -14		; (data) link -> 107364 (store-status exit, in-body)
107235  043465  	MIN I ,B ,X 65		; (data)
107236  046231  	LDA ,X -147		; (data) link -> 046231 (FLPAR)
107237  000103  	STZ 103			; (data)
107240  147421  	ROP NOOP		; (data)
107241  000051  	STZ 51			; (data)
107242  020274  	STD -104		; (data) link -> 020274 (resident worker)
107243  031064  	STF I 64		; (data) link -> 031064 (SUCPB)
107244  030225  	STF -153		; (data) link -> 030225 (GDIRA get directory address)
107245  132263  	JPC -115		; (data)
107246  030235  	STF -143		; (data) link -> 030235 (GNAMA get name address)
107247  130140  	JAP 140			; (data)
; ---- body resumes ----
107250  044504  	LDA ,B 104		; branch on write flag
107251  131430  	JAF 30		; -> 107301 (get-entry format path)
107252  044114  	LDA 114
107253  146035  	RADD SB DA
107254  135113  	JPL I 113		; -> 107367 (link cell = 001224, resident worker)
107255  044400  	LDA ,B 0
107256  054110  	LDX 110
107257  146037  	RADD SB DX
107260  135110  	JPL I 110		; -> 107370 (link cell = 031064, SUCPB)
107261  124103  	JMP 103		; -> 107364 (store-status exit)
107262  044505  	LDA ,B 105
107263  131415  	JAF 15		; -> 107300
107264  050401  	LDT ,B 1
107265  135104  	JPL I 104		; -> 107371 (link cell = 050323, GNFLA)
107266  124076  	JMP 76		; -> 107364 (store-status exit)
107267  175125  	BSKP ZRO 120 DA
107270  124003  	JMP 3		; -> 107273
107271  175265  	BSKP ONE 60 DA
107272  124004  	JMP 4		; -> 107276
107273  170401  	SAA 1
107274  004402  	STA ,B 2
107275  124003  	JMP 3		; -> 107300
107276  170403  	SAA 3
107277  004402  	STA ,B 2
107300  124060  	JMP 60		; -> 107360 (return)
107301  046003  	LDA ,X 3		; format the returned directory entry
107302  146151  	RADD CLD SA DD
107303  135067  	JPL I 67		; -> 107372 (link cell = 010500, resident worker)
107304  046000  	LDA ,X 0
107305  144015  	SWAP SD DA
107306  135065  	JPL I 65		; -> 107373 (link cell = 010506, resident worker)
107307  175171  	BSKP ZRO 170 DD
107310  124003  	JMP 3		; -> 107313
107311  170426  	SAA 26
107312  124052  	JMP 52		; -> 107364 (store-status exit)
107313  175131  	BSKP ZRO 130 DD
107314  124003  	JMP 3		; -> 107317
107315  044057  	LDA 57
107316  124046  	JMP 46		; -> 107364 (store-status exit)
107317  146161  	RADD CLD ST DD
107320  050401  	LDT ,B 1
107321  135050  	JPL I 50		; -> 107371 (link cell = 050323, GNFLA)
107322  124042  	JMP 42		; -> 107364 (store-status exit)
107323  146116  	RADD CLD SD DT
107324  175235  	BSKP ONE 30 DA
107325  124003  	JMP 3		; -> 107330
107326  170470  	SAA 70
107327  124035  	JMP 35		; -> 107364 (store-status exit)
107330  054400  	LDX ,B 0
107331  044035  	LDA 35
107332  146035  	RADD SB DA
107333  135042  	JPL I 42		; -> 107375 (link cell = 031067, USCPB)
107334  124030  	JMP 30		; -> 107364 (store-status exit)
107335  171400  	SAX 0
107336  046451  	LDA ,X ,B 51
107337  175175  	BSKP ZRO 170 DA
107340  124004  	JMP 4		; -> 107344
107341  175335  	BSKP ONE 130 DA
107342  124002  	JMP 2		; -> 107344
107343  044033  	LDA 33
107344  175135  	BSKP ZRO 130 DA
107345  124004  	JMP 4		; -> 107351
107346  175135  	BSKP ZRO 130 DA
107347  124002  	JMP 2		; -> 107351
107350  044026  	LDA 26
107351  054015  	LDX 15
107352  146037  	RADD SB DX
107353  044502  	LDA ,B 102
107354  135013  	JPL I 13		; -> 107367 (link cell = 001224, resident worker)
107355  050401  	LDT ,B 1
107356  135021  	JPL I 21		; -> 107377 (link cell = 047716, WDIRE write dir entry)
107357  124005  	JMP 5		; -> 107364 (store-status exit)
107360  040404  	MIN ,B 4		; success return leader
107361  170672  	SAA -106
107362  125016  	JMP I 16		; -> 107400 (link cell = 003776, resident return)
107363  170553  	SAA 153			; error 153
107364  004402  	STA ,B 2		; store status word -> caller slot B+2
107365  124374  	JMP -4		; -> 107361 (fall to resident return)
; ---- pointer table (DATA, not code) 107366..107400 ----
107366  000051  	STZ 51			; (data)
107367  001224  	STZ I -154		; (data) link -> 001224 (resident worker)
107370  031064  	STF I 64		; (data) link -> 031064 (SUCPB)
107371  050323  	LDT -55			; (data) link -> 050323 (GNFLA)
107372  010500  	STT ,B 100		; (data) link -> 010500 (resident worker)
107373  010506  	STT ,B 106		; (data) link -> 010506 (resident worker)
107374  000251  	STZ -127		; (data)
107375  031067  	STF I 67		; (data) link -> 031067 (USCPB)
107376  000317  	STZ -61			; (data)
107377  047716  	LDA I ,B ,X -62		; (data) link -> 047716 (WDIRE write dir entry)
107400  003776  	STZ I ,B ,X -2		; (data) link -> 003776 (resident return)
