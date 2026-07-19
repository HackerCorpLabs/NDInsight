; ============================================================================
; MON 243B (octal) GetDirNameIndex - real SINTRAN L disassembly
; ONE region (dispatch is fall-through 3-layer; see README Code location):
;   GOTAB[243B] = 000000 (fall-through, no per-call stub in 025-S3IRPIT)
;   FDINA Find-DIrectory-NAme worker  FDINA @106734B  in segment 006-S3FS
;   (load 26000B).  (the fall-through -> worker bridge is the uncarved resident
;   MFELL / CALLPROC - not in these bytes.)
;
; FDINA is the byte-level GetDirNameIndex worker: it parses the directory name
; string (FLPAR parse, REMCH remote-char), then resolves the directory index via
; GDIRI (Get DIRectory Index @47402B) and reaches the directory/name tables via
; GDIRT (@50124B), GDFKN / GDEFD, returning the directory index and the name
; index (the device description of the disk). Bounded strictly to the next
; FILSYS symbol WDIEN = 107106B.
;
; The region contains POINTER TABLES (link cells), not code: nd100-dis renders
; those pointer words as bogus instructions; their contents are the real worker
; addresses (marked "(data) link -> ..." with the resolved symbol). Addresses
; and values are octal.
; ============================================================================

; ---- FDINA GetDirNameIndex worker (006-S3FS, load 26000B) ----
106734  174020  	BSET ZRO SSK
106735  021123  	STD I 123
106736  146145  	RADD CLD SL DA
106737  146131  	RADD CLD SB DD
106740  170075  	SAB 75
106741  135120  	JPL I 120      ; -> 107061 [107061]=003752 resident (below 26000B load base)
106742  175220  	BSKP ONE SSK
106743  124004  	JMP 4
106744  170401  	SAA 1
106745  004474  	STA ,B 74
106746  124002  	JMP 2
106747  000474  	STZ ,B 74
106750  044112  	LDA 112
106751  146035  	RADD SB DA
106752  004471  	STA ,B 71
106753  054400  	LDX ,B 0
106754  171146  	SAT 146
106755  135106  	JPL I 106      ; -> 107063 [107063]=031075 USCPS
106756  124100  	JMP 100
106757  146151  	RADD CLD SA DD
106760  146157  	RADD CLD SA DX
106761  044103  	LDA 103
106762  135103  	JPL I 103      ; -> 107065 [107065]=046231 FLPAR
106763  124073  	JMP 73
106764  044474  	LDA ,B 74
106765  131421  	JAF 21
106766  045100  	LDA I 100
106767  146151  	RADD CLD SA DD
106770  135077  	JPL I 77       ; -> 107067 [107067]=010500 resident (below 26000B load base)
106771  054071  	LDX 71
106772  146037  	RADD SB DX
106773  135075  	JPL I 75       ; -> 107070 [107070]=047402 GDIRI
106774  124056  	JMP 56
106775  146115  	RADD CLD SD DA
106776  135073  	JPL I 73       ; -> 107071 [107071]=010506 resident (below 26000B load base)
106777  010401  	STT ,B 1
107000  054072  	LDX 72
107001  135072  	JPL I 72       ; -> 107073 [107073]=050124 GDIRT
107002  124054  	JMP 54
107003  070071  	AND 71
107004  004402  	STA ,B 2
107005  124031  	JMP 31
107006  044067  	LDA 67
107007  054471  	LDX ,B 71
107010  135066  	JPL I 66       ; -> 107076 [107076]=061451 REMCH
107011  124015  	JMP 15
107012  146176  	RADD CLD SX DT
107013  055064  	LDX I 64
107014  012024  	STT ,X 24
107015  044063  	LDA 63
107016  146035  	RADD SB DA
107017  006025  	STA ,X 25
107020  171434  	SAX 34
107021  135060  	JPL I 60       ; -> 107101 [107101]=020274 resident (below 26000B load base)
107022  124034  	JMP 34
107023  050472  	LDT ,B 72
107024  044473  	LDA ,B 73
107025  124007  	JMP 7
107026  054034  	LDX 34
107027  146037  	RADD SB DX
107030  135052  	JPL I 52       ; -> 107102 [107102]=054527 GMUSI
107031  124025  	JMP 25
107032  135051  	JPL I 51       ; -> 107103 [107103]=054341 GDFKN
107033  124006  	JMP 6
107034  010401  	STT ,B 1
107035  004402  	STA ,B 2
107036  040404  	MIN ,B 4
107037  170703  	SAA -75
107040  125044  	JMP I 44       ; -> 107104 [107104]=003776 resident (below 26000B load base)
107041  171473  	SAX 73
107042  140075  	SKP IF DA EQL SX
107043  124007  	JMP 7
107044  146165  	RADD CLD ST DA
107045  135040  	JPL I 40       ; -> 107105 [107105]=055263 GDEFD
107046  124002  	JMP 2
107047  124365  	JMP -13
107050  014402  	STX ,B 2
107051  124366  	JMP -12
107052  004402  	STA ,B 2
107053  146115  	RADD CLD SD DA
107054  135015  	JPL I 15       ; -> 107071 [107071]=010506 resident (below 26000B load base)
107055  124362  	JMP -16
107056  004402  	STA ,B 2
107057  124360  	JMP -20
107060  147417  	ROP NOOP
107061  003752  	(data) link -> 003752  resident (below 26000B load base)
107062  000006  	STZ 6
107063  031075  	(data) link -> 031075  USCPS
107064  043465  	MIN I ,B ,X 65
107065  046231  	(data) link -> 046231  FLPAR
107066  120027  	MPY 27
107067  010500  	(data) link -> 010500  resident (below 26000B load base)
107070  047402  	(data) link -> 047402  GDIRI
107071  010506  	(data) link -> 010506  resident (below 26000B load base)
107072  000002  	STZ 2
107073  050124  	(data) link -> 050124  GDIRT
107074  000377  	STZ -1
107075  023447  	STD I ,B ,X 47
107076  061451  	(data) link -> 061451  REMCH
107077  147421  	ROP NOOP
107100  000072  	STZ 72
107101  020274  	(data) link -> 020274  resident (below 26000B load base)
107102  054527  	(data) link -> 054527  GMUSI
107103  054341  	(data) link -> 054341  GDFKN
107104  003776  	(data) link -> 003776  resident (below 26000B load base)
107105  055263  	(data) link -> 055263  GDEFD
