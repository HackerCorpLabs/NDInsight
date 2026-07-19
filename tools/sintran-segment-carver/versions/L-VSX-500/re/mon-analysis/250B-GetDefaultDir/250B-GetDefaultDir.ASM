;=============================================================================
; MON 250B (octal) GetDefaultDir (FDFDI) - real SINTRAN L disassembly
;
; Gets the user's default directory: the directory index and the user index are
; returned for the named user (the name may identify a remote user). All users,
; all programs, ND-100 and ND-500.
;
; Dispatch is 3-layer (see README Code location). Two labelled regions:
;   A. GOTAB[250] level-14 entry stub  F1732 @066242B in segment 025-S3IRPIT (load 32000B)
;      Window 066242B..066245B = 4 words, bounded strictly to the next symbol
;      F1734=066246B. A tiny compiler entry stub sitting in a shared stub block; the
;      real transfer to the FDFDI worker is the resident CALLPROC (uncarved).
;   B. FDFDI find-default-directory worker  FDFDI @106732B in segment 006-S3FS (load 26000B)
;      Window 106732B..107105B = 108 words, bounded strictly to the next symbol
;      WDIEN=107106B. Words 106732B..107057B are code; 107060B is a one-word ROP
;      NOOP pad; 107061B..107105B are a pointer table (link cells, DATA).
;   (the A->B bridge is the uncarved resident CALLPROC - NOT present in these bytes;
;    region B IS real executable code; the A->B link is by symbol NAME.)
;
; FDFDI has a two-entry idiom: FDFDI=106732B sets the SSK skip flag, the sibling
; FDINA=106734B clears it; both join the common body at 106735B and the flag
; (106742B BSKP ONE SSK) selects whether a name is decoded. FDFDI (Find DeFault
; DIrectory) is a FILSYS-SYMBOLS symbol in the file-system segment 006-S3FS.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
;=============================================================================

; ---- Region A: GOTAB[250] entry stub F1732 (025-S3IRPIT, load 32000B) ----
; F1732 = 066242B (SYMBOL-2-LIST); next symbol F1734 = 066246B, so the stub is 4 words.
; GOTAB[250] = 066242B (byte-proven). The 4 words land inside a shared stub block
; (F1734 directly after is the MON 254B GetErrorDevice stub); they are not a
; self-contained named handler and the real transfer to the worker is the resident
; CALLPROC. A static decode cannot follow it.
066242  052376  	LDT ,X -2	; T = mem[X-2]
066243  141065  	SKP IF DA GRE ST	; skip next if (signed) A > T
066244  124005  	JMP 5		; -> 066251
066245  044422  	LDA ,B 22	; A = mem[B+22]
; 066246 = F1734 boundary (next symbol, MON 254B stub); not part of this stub.

; ---- Region B: FDFDI find-default-directory worker (006-S3FS, load 26000B) ----
106732  174220  	BSET ONE SSK	; FDFDI entry: set skip flag (SSK = 1)
106733  124002  	JMP 2		; -> 106735
106734  174020  	BSET ZRO SSK	; FDINA alt entry: clear skip flag (SSK = 0)
106735  021123  	STD I 123	; store A/D pair via [link cell] - save incoming
106736  146145  	RADD CLD SL DA	; A = L  (copy return link)
106737  146131  	RADD CLD SB DD	; D = B  (copy frame pointer)
106740  170075  	SAB 75		; B = 75
106741  135120  	JPL I 120	; -> [107061]  call resident prologue worker
106742  175220  	BSKP ONE SSK	; skip next if SSK set (FDFDI vs FDINA entry)
106743  124004  	JMP 4		; -> 106747
106744  170401  	SAA 1		; A = 1
106745  004474  	STA ,B 74	; mem[B+74] = 1  (decode-name flag)
106746  124002  	JMP 2		; -> 106750
106747  000474  	STZ ,B 74	; mem[B+74] = 0
106750  044112  	LDA 112		; A = mem[P+112]
106751  146035  	RADD SB DA	; A = A + B
106752  004471  	STA ,B 71	; mem[B+71] = A  (name buffer pointer)
106753  054400  	LDX ,B 0	; X = mem[B+0]
106754  171146  	SAT 146		; T = 146
106755  135106  	JPL I 106	; -> [107063]  call resident worker (parse user name)
106756  124100  	JMP 100		; -> 107056  (error tail)
106757  146151  	RADD CLD SA DD	; D = A  (copy)
106760  146157  	RADD CLD SA DX	; X = A  (copy)
106761  044103  	LDA 103		; A = mem[P+103]
106762  135103  	JPL I 103	; -> [107065]  call resident worker
106763  124073  	JMP 73		; -> 107056  (error tail)
106764  044474  	LDA ,B 74	; A = mem[B+74]  (decode-name flag)
106765  131421  	JAF 21		; -> 107006  (jump if flag false: no name given)
106766  045100  	LDA I 100	; A = mem[[107066]]  (indirect)
106767  146151  	RADD CLD SA DD	; D = A  (copy)
106770  135077  	JPL I 77	; -> [107067]  call resident worker (find named user)
106771  054071  	LDX 71		; X = mem[P+71]
106772  146037  	RADD SB DX	; X = X + B
106773  135075  	JPL I 75	; -> [107070]  call resident worker
106774  124056  	JMP 56		; -> 107052  (error tail)
106775  146115  	RADD CLD SD DA	; A = D  (copy)
106776  135073  	JPL I 73	; -> [107071]  call resident worker
106777  010401  	STT ,B 1	; mem[B+1] = T
107000  054072  	LDX 72		; X = mem[P+72]
107001  135072  	JPL I 72	; -> [107073]  call resident worker
107002  124054  	JMP 54		; -> 107056  (error tail)
107003  070071  	AND 71		; A &= mem[P+71]
107004  004402  	STA ,B 2	; mem[B+2] = A
107005  124031  	JMP 31		; -> 107036  (join success tail)
107006  044067  	LDA 67		; A = mem[P+67]  (no-name path)
107007  054471  	LDX ,B 71	; X = mem[B+71]
107010  135066  	JPL I 66	; -> [107076]  call resident worker (use current user)
107011  124015  	JMP 15		; -> 107026
107012  146176  	RADD CLD SX DT	; T = X  (copy)
107013  055064  	LDX I 64	; X = mem[[107077]]
107014  012024  	STT ,X 24	; mem[X+24] = T
107015  044063  	LDA 63		; A = mem[P+63]
107016  146035  	RADD SB DA	; A = A + B
107017  006025  	STA ,X 25	; mem[X+25] = A
107020  171434  	SAX 34		; X = 34
107021  135060  	JPL I 60	; -> [107101]  call resident worker
107022  124034  	JMP 34		; -> 107056  (error tail)
107023  050472  	LDT ,B 72	; T = mem[B+72]  (directory index out)
107024  044473  	LDA ,B 73	; A = mem[B+73]  (user index out)
107025  124007  	JMP 7		; -> 107034  (join return)
107026  054034  	LDX 34		; X = mem[P+34]
107027  146037  	RADD SB DX	; X = X + B
107030  135052  	JPL I 52	; -> [107102]  call resident worker
107031  124025  	JMP 25		; -> 107056  (error tail)
107032  135051  	JPL I 51	; -> [107103]  call resident worker
107033  124006  	JMP 6		; -> 107041
107034  010401  	STT ,B 1	; mem[B+1] = T  (return directory index)
107035  004402  	STA ,B 2	; mem[B+2] = A  (return user index)
107036  040404  	MIN ,B 4	; mem[B+4]++ ; skip next if it wraps to 0 (success advance)
107037  170703  	SAA -75		; A = -75  (error code)
107040  125044  	JMP I 44	; -> [107104]  indirect return
107041  171473  	SAX 73		; X = 73
107042  140075  	SKP IF DA EQL SX	; skip if A == X
107043  124007  	JMP 7		; -> 107052  (error tail)
107044  146165  	RADD CLD ST DA	; A = T  (copy)
107045  135040  	JPL I 40	; -> [107105]  call resident worker
107046  124002  	JMP 2		; -> 107050
107047  124365  	JMP -13		; -> 107034
107050  014402  	STX ,B 2	; mem[B+2] = X
107051  124366  	JMP -12		; -> 107037
107052  004402  	STA ,B 2	; mem[B+2] = A  (error tail entry)
107053  146115  	RADD CLD SD DA	; A = D  (copy)
107054  135015  	JPL I 15	; -> [107071]  call resident worker
107055  124362  	JMP -16		; -> 107037
107056  004402  	STA ,B 2	; mem[B+2] = A  (error tail entry)
107057  124360  	JMP -20		; -> 107037
107060  147417  	ROP NOOP	; one-word pad (closes the code region)
; ---- link cells (DATA, not code) 107061..107105 ----
107061  003752  	STZ I ,B ,X -26	; pointer word (read by JPL I 120 at 106741)
107062  000006  	STZ 6		; constant/pointer cell
107063  031075  	STF I 75	; pointer word (read by JPL I 106 at 106755)
107064  043465  	MIN I ,B ,X 65	; constant/pointer cell
107065  046231  	LDA ,X -147	; pointer word (read by JPL I 103 at 106762)
107066  120027  	MPY 27		; constant/pointer cell (read by LDA I 100 at 106766)
107067  010500  	STT ,B 100	; pointer word (read by JPL I 77 at 106770)
107070  047402  	LDA I ,B ,X 2	; pointer word (read by JPL I 75 at 106773)
107071  010506  	STT ,B 106	; pointer word (read by JPL I 73/15 at 106776/107054)
107072  000002  	STZ 2		; constant/pointer cell
107073  050124  	LDT 124		; pointer word (read by JPL I 72 at 107001)
107074  000377  	STZ -1		; constant/pointer cell
107075  023447  	STD I ,B ,X 47	; constant/pointer cell (read by LDX I 64 at 107013)
107076  061451  	ADD I ,B 51	; pointer word (read by JPL I 66 at 107010)
107077  147421  	ROP NOOP	; constant/pointer cell (read by LDX I 64 at 107013)
107100  000072  	STZ 72		; constant/pointer cell
107101  020274  	STD -104	; pointer word (read by JPL I 60 at 107021)
107102  054527  	LDX ,B 127	; pointer word (read by JPL I 52 at 107030)
107103  054341  	LDX -37		; pointer word (read by JPL I 51 at 107032)
107104  003776  	STZ I ,B ,X -2	; pointer word (read by JMP I 44 at 107040)
107105  055263  	LDX I -115	; pointer word (read by JPL I 40 at 107045)
; 107106 = WDIEN boundary (next symbol); not part of this worker.
