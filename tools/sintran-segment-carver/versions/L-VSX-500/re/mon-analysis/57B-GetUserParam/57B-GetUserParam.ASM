;=============================================================================
; MON 57B (octal) GetUserParam (PAGEI) - real SINTRAN L disassembly
;
; Gets the 5 user parameters of a background program (why the last program
; terminated). SetUserParam (MON 56B) sets them. Background programs only.
;
; Dispatch is 3-layer (see README Code location). Two labelled regions:
;   A. GOTAB[57] level-14 entry stub  F1636 @121246B in segment 025-S3IRPIT (load
;      32000B). Window 121246B..121270B, bounded strictly to the next symbol
;      P36R1=121271B. This is a shared stub block whose internal branches leave the
;      window (not a self-contained named handler); the real transfer to the MPAGE
;      worker is the resident CALLPROC (uncarved).
;   B. MPASE/MPAGE set/get-user-param worker  MPAGE @102365B in segment 025-S3IRPIT
;      (load 32000B). The body runs 102363B..102406B (code) with a 4-word pointer
;      table 102407B..102412B (link cells, DATA); the next symbol RERRP=102413B is a
;      different routine (the MON 207B read-error-parameters handler).
;   (the A->B bridge is the uncarved resident CALLPROC - NOT present in these bytes;
;    region B IS real executable code; the A->B link is by symbol NAME.)
;
; This is a SHARED set/get body with two entries selected by the SSK skip flag:
; MPASE=102363B clears SSK (SET, MON 56B); MPAGE=102365B (this call) sets SSK (GET).
; Both join the common body at 102366B; 102371B (BSKP ONE SSK) selects the get
; direction of the 5-word copy. MPASE/MPAGE are SYMBOL-2-LIST symbols in the
; second-level monitor segment 025-S3IRPIT.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
;=============================================================================

; ---- Region A: GOTAB[57] entry stub F1636 (025-S3IRPIT, load 32000B) ----
; F1636 = 121246B (SYMBOL-2-LIST); next symbol P36R1 = 121271B. GOTAB[57] = 121246B
; (byte-proven). The block below sits in a shared stub region: its JMP targets
; (121316B, 121237B) fall outside this window, so it is not a self-contained handler
; and the real transfer to MPAGE is the resident CALLPROC. A static decode cannot
; follow it.
121246  173401  	AAX 1		; X = X + 1
121247  144017  	SWAP SD DX	; exchange D and X
121250  053051  	LDT I ,X 51	; T = mem[mem[X+51]]
121251  140065  	SKP IF DA EQL ST	; skip next if A == T
121252  124044  	JMP 44		; -> 121316  (leaves this window)
121253  173401  	AAX 1		; X = X + 1
121254  124363  	JMP -15		; -> 121237  (leaves this window)
; 121255..121270 continue the shared stub block up to P36R1=121271B (not the handler).

; ---- Region B: MPASE/MPAGE set/get-user-param worker (025-S3IRPIT, load 32000B) ----
102363  174020  	BSET ZRO SSK	; MPASE entry (SetUserParam 56B): clear skip flag (SSK = 0, SET)
102364  124002  	JMP 2		; -> 102366
102365  174220  	BSET ONE SSK	; MPAGE entry (GetUserParam 57B, this call): set skip flag (SSK = 1, GET)
102366  135021  	JPL I 21	; -> [102407]  call resident prologue worker (get array address)
102367  170404  	SAA 4		; A = 4
102370  150307  	MST PIE		; mask-set PIE (enter monitor level)
102371  175220  	BSKP ONE SSK	; skip next if SSK set (GET path - this call)
102372  124005  	JMP 5		; -> 102377  (SET path)
102373  044015  	LDA 15		; A = mem[P+15] = [102410] = USPAR constant
102374  146151  	RADD CLD SA DD	; D = A  (GET: D = "USPAR")
102375  050420  	LDT ,B 20	; T = mem[B+20]  (GET: T = user array D0)
102376  124004  	JMP 4		; -> 102402
102377  044420  	LDA ,B 20	; A = mem[B+20]  (SET: user array D0)
102400  146151  	RADD CLD SA DD	; D = A  (SET: D = D0)
102401  050007  	LDT 7		; T = mem[P+7] = [102410] = USPAR constant (SET: T = "USPAR")
102402  171405  	SAX 5		; X = 5  (5 words to copy)
102403  044417  	LDA ,B 17	; A = mem[B+17]  (A = OLDPAGE)
102404  135005  	JPL I 5		; -> [102411]  call MOVUS (copy 5 words user <-> system segment)
102405  000412  	STZ ,B 12	; mem[B+12] = 0  (A = 0 on return)
102406  125004  	JMP I 4		; -> [102412]  indirect return
; ---- link cells (DATA, not code) 102407..102412 ----
102407  000350  	STZ -30		; pointer word (read by JPL I 21 at 102366)
102410  145445  	RORA SL DA	; constant USPAR = 145445 (read by LDA 15/LDT 7 at 102373/102401)
102411  001164  	STZ I 164	; pointer word (read by JPL I 5 at 102404, MOVUS)
102412  010341  	STT -37		; pointer word (read by JMP I 4 at 102406, return)
; 102413 = RERRP boundary (next symbol, MON 207B handler); not part of this worker.
