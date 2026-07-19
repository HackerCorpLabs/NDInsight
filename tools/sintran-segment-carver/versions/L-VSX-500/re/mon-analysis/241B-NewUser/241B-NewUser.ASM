;=============================================================================
; MON 241B (octal) NewUser (SUSCN) - real SINTRAN L disassembly
;
; Switches the user name you are logged in under (like logging out and logging in
; as another user); your program continues under the new user name. Restore the old
; name with OldUser (MON 242B). From the ND-100 you may execute NewUser more than
; once without OldUser between. User RT and user SYSTEM only; background programs.
;
; MON 241B is a FALL-THROUGH call: GOTAB[241] = 000000 (byte-proven), so there is
; NO per-call level-14 entry stub. Dispatch drops into the resident MFELL/CALLPROC
; second-level path (uncarved) which reaches the worker below. One labelled region:
;   SUSCN switch-user worker body   SUSCN @106377B in segment 006-S3FS (load 26000B)
;      Window 106377B..106561B = 115 words, bounded strictly to the next symbol
;      RUSCN=106562B (the OldUser worker). Words 106377B..106527B are code; 106530B
;      is a one-word ROP NOOP pad; 106531B..106561B are a pointer table (link cells,
;      DATA) that the JPL I / JMP I indirections dereference.
;   (the fall-through -> SUSCN bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; SUSCN (Switch USer, Change Name) is a FILSYS-SYMBOLS symbol in the file-system
; segment 006-S3FS; it sits directly before its twin RUSCN (OldUser, MON 242B).
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
;=============================================================================

; ---- SUSCN switch-user (NewUser) worker (006-S3FS, load 26000B) ----
106377  021131  	STD I 131	; store A/D pair via [link cell] - save incoming
106400  146145  	RADD CLD SL DA	; A = L  (copy return link)
106401  146131  	RADD CLD SB DD	; D = B  (copy frame pointer)
106402  170032  	SAB 32		; B = 32
106403  135126  	JPL I 126	; -> [106531]  call resident prologue worker
106404  135130  	JPL I 130	; -> [106534]  call resident worker
106405  124103  	JMP 103		; -> 106510  (error tail)
106406  054400  	LDX ,B 0	; X = mem[B+0]
106407  044123  	LDA 123		; A = mem[P+123]
106410  146035  	RADD SB DA	; A = A + B
106411  171044  	SAT 44		; T = 44
106412  135123  	JPL I 123	; -> [106535]  call resident worker (validate user name)
106413  124075  	JMP 75		; -> 106510  (error tail)
106414  146151  	RADD CLD SA DD	; D = A  (copy)
106415  146157  	RADD CLD SA DX	; X = A  (copy)
106416  044120  	LDA 120		; A = mem[P+120]
106417  171044  	SAT 44		; T = 44
106420  135117  	JPL I 117	; -> [106537]  call resident worker
106421  124067  	JMP 67		; -> 106510  (error tail)
106422  045116  	LDA I 116	; A = mem[[106540]]  (indirect)
106423  171377  	SAT -1		; T = -1
106424  140065  	SKP IF DA EQL ST	; skip if A == T
106425  124011  	JMP 11		; -> 106436
106426  045113  	LDA I 113	; A = mem[[106541]]
106427  156410  	SHA ZIN 10	; A <<= 8  (logical left shift, zero fill)
106430  061112  	ADD I 112	; A += mem[[106542]]
106431  005107  	STA I 107	; mem[[106540]] = A
106432  045111  	LDA I 111	; A = mem[[106543]]
106433  005111  	STA I 111	; mem[[106543]] = A
106434  045111  	LDA I 111	; A = mem[[106545]]
106435  005111  	STA I 111	; mem[[106546]] = A
106436  045111  	LDA I 111	; A = mem[[106547]]
106437  175245  	BSKP ONE 40 DA	; skip if selected bit of A set
106440  124005  	JMP 5		; -> 106445
106441  044402  	LDA ,B 2	; A = mem[B+2]
106442  131403  	JAF 3		; -> 106445  (jump if flag false)
106443  170425  	SAA 25		; A = 25   (error code)
106444  124044  	JMP 44		; -> 106510  (error tail)
106445  045101  	LDA I 101	; A = mem[[106547]]
106446  171001  	SAT 1		; T = 1
106447  140065  	SKP IF DA EQL ST	; skip if A == T  (user RT test)
106450  124014  	JMP 14		; -> 106464
106451  054061  	LDX 61		; X = mem[P+61]
106452  146037  	RADD SB DX	; X = X + B
106453  135075  	JPL I 75	; -> [106550]  call resident worker
106454  124052  	JMP 52		; -> 106526  (error tail)
106455  146165  	RADD CLD ST DA	; A = T  (copy)
106456  135073  	JPL I 73	; -> [106551]  call resident worker
106457  124047  	JMP 47		; -> 106526  (error tail)
106460  140065  	SKP IF DA EQL ST	; skip if A == T
106461  124003  	JMP 3		; -> 106464
106462  170425  	SAA 25		; A = 25   (error code)
106463  124043  	JMP 43		; -> 106526  (error tail)
106464  045055  	LDA I 55	; A = mem[[106541]]
106465  004430  	STA ,B 30	; mem[B+30] = A  (save name word 0)
106466  045054  	LDA I 54	; A = mem[[106542]]
106467  004431  	STA ,B 31	; mem[B+31] = A  (save name word 1)
106470  135062  	JPL I 62	; -> [106552]  call resident worker
106471  135062  	JPL I 62	; -> [106553]  call resident worker
106472  124031  	JMP 31		; -> 106523  (error tail)
106473  054037  	LDX 37		; X = mem[P+37]
106474  146037  	RADD SB DX	; X = X + B
106475  044402  	LDA ,B 2	; A = mem[B+2]
106476  135056  	JPL I 56	; -> [106554]  call resident worker (install new user)
106477  124013  	JMP 13		; -> 106512  (error tail)
106500  004402  	STA ,B 2	; mem[B+2] = A  (returned status: 0/1/2)
106501  005044  	STA I 44	; mem[[106545]] = A
106502  005053  	STA I 53	; mem[[106555]] = A
106503  045040  	LDA I 40	; A = mem[[106543]]
106504  005052  	STA I 52	; mem[[106556]] = A
106505  040404  	MIN ,B 4	; mem[B+4]++ ; skip next if it wraps to 0 (success advance)
106506  170746  	SAA -32		; A = -32  (error code)
106507  125050  	JMP I 50	; -> [106557]  indirect return
106510  004402  	STA ,B 2	; mem[B+2] = A  (error tail entry)
106511  124375  	JMP -3		; -> 106506
106512  004402  	STA ,B 2	; mem[B+2] = A  (error tail: restore old name)
106513  044430  	LDA ,B 30	; A = mem[B+30]  (saved name word 0)
106514  005025  	STA I 25	; mem[[106541]] = A
106515  044431  	LDA ,B 31	; A = mem[B+31]  (saved name word 1)
106516  005024  	STA I 24	; mem[[106542]] = A
106517  135041  	JPL I 41	; -> [106560]  call resident worker
106520  135041  	JPL I 41	; -> [106561]  call resident worker
106521  124365  	JMP -13		; -> 106506
106522  124364  	JMP -14		; -> 106506
106523  004402  	STA ,B 2	; mem[B+2] = A  (error tail)
106524  135034  	JPL I 34	; -> [106560]  call resident worker
106525  124361  	JMP -17		; -> 106506
106526  004402  	STA ,B 2	; mem[B+2] = A  (error tail)
106527  124357  	JMP -21		; -> 106506
106530  147417  	ROP NOOP	; one-word pad (closes the code region)
; ---- link cells (DATA, not code) 106531..106561 ----
106531  003752  	STZ I ,B ,X -26	; pointer word (read by JPL I 126 at 106403)
106532  000006  	STZ 6		; constant/pointer cell
106533  030104  	STF 104		; constant/pointer cell
106534  053114  	LDT I ,X 114	; pointer word (read by JPL I 130 at 106404)
106535  031075  	STF I 75	; pointer word (read by JPL I 123 at 106412)
106536  043334  	MIN I ,X -44	; constant/pointer cell
106537  046231  	LDA ,X -147	; pointer word (read by JPL I 117 at 106420)
106540  146300  	RADD CM1 CLD 0 0	; constant/pointer cell
106541  146275  	RADD CM1 SX DA	; constant/pointer cell
106542  146276  	RADD CM1 SX DT	; constant/pointer cell
106543  146274  	RADD CM1 SX DL	; constant/pointer cell
106544  146277  	RADD CM1 SX DX	; constant/pointer cell
106545  144037  	SWAP SB DX	; constant/pointer cell
106546  144134  	SWAP CLD SB DL	; constant/pointer cell
106547  004266  	STA -112	; constant/pointer cell
106550  054527  	LDX ,B 127	; pointer word (read by JPL I 75 at 106453)
106551  055540  	LDX I ,B 140	; pointer word (read by JPL I 73 at 106456)
106552  101127  	FAD I 127	; pointer word (read by JPL I 62 at 106470)
106553  101201  	FAD I -177	; pointer word (read by JPL I 62 at 106471)
106554  114353  	FDV -25		; pointer word (read by JPL I 56 at 106476)
106555  144040  	SWAP SL 0	; constant/pointer cell
106556  144111  	SWAP CLD SD DD	; constant/pointer cell
106557  003776  	STZ I ,B ,X -2	; pointer word (read by JMP I 50 at 106507)
106560  101120  	FAD I 120	; pointer word (read by JPL I 41 at 106517/106524)
106561  101177  	FAD I 177	; pointer word (read by JPL I 41 at 106520)
; 106562 = RUSCN boundary (OldUser worker, next symbol); not part of this worker.
