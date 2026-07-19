; ============================================================================
; MON 45B (octal) DefineBreakpoint - real SINTRAN L disassembly
; Byte-verified GOTAB[45] dispatch target + its general-path worker.
; Two regions (both in segment 025-S3IRPIT, load 32000B):
;   A. GOTAB[45] level-14 entry stub  F1631 @121075B..121121B (21 words)
;   B. general-path worker body       PUTPO @115720B..115737B (16 words)
;      (reached via 121121 JPL I 65 -> ptr@121206 = 115720)
; Worker NAMES (F1631/PUTPO/SNDRE) are SYMBOL-2-LIST labels - HINTS only;
; treat the octal addresses as truth. Worker BEHAVIOUR is UNVERIFIED.
; See README "Honest caveats": the older DEBUGGER/BRPNT carve was MISATTRIBUTED.
; All addresses and values are octal.
; ============================================================================

; ---- Region A: GOTAB[45] entry stub  F1631 (025-S3IRPIT, load 32000B) ----
121075  130500  	JAN 100		; -> 121175  (A negative -> alternate exit)
121076  171003  	SAT 3		; T := 3
121077  140065  	SKP IF DA EQL ST	; skip next if A == 3
121100  124005  	JMP 5		; -> 121105  (A != 3: general slot path)
121101  135101  	JPL I 101		; -> ptr@121202 = 121640 (SNDRE; A==3 worker)
121102  144400  	RAND 0 0
121103  044100  	LDA 100
121104  124071  	JMP 71		; -> 121175
121105  045077  	LDA I 77		; -> ptr@121204 = 061160 (data ptr)
121106  146153  	RADD CLD SA DB
121107  055076  	LDX I 76		; -> ptr@121205 = 061157 (table base)
121110  046014  	LDA ,X 14		; A := slot field 14
121111  131011  	JAZ 11		; -> 121122  (empty slot -> next stub)
121112  002014  	STZ ,X 14		; clear slot fields 14/46/4/2
121113  002046  	STZ ,X 46
121114  002004  	STZ ,X 4
121115  002002  	STZ ,X 2
121116  146156  	RADD CLD SA DT
121117  026015  	LDD ,X 15		; load double-word from slot
121120  146167  	RADD CLD ST DX
121121  135065  	JPL I 65		; -> ptr@121206 = 115720 (PUTPO; general worker)

; ---- Shared pointer-word block (data) 121202B..121206B (used by F1631) ----
; disassembler renders these as instructions; they are POINTER WORDS.
121202  121640  	; ptr: SNDRE   (target of 121101 JPL I 101)
121203  000315  	; ptr/data
121204  061160  	; ptr: data   (target of 121105 LDA I 77)
121205  061157  	; ptr: table  (target of 121107 LDX I 76)
121206  115720  	; ptr: PUTPO  (target of 121121 JPL I 65)

; ---- Region B: general-path worker  PUTPO (025-S3IRPIT, load 32000B) ----
; Reached from F1631 121121 JPL I 65 -> 115720. Behaviour UNVERIFIED.
; IOF/ION brackets an I/O-off critical section that edits an X-indexed table
; (STDTX/STATX). Consistent with a message/queue-send family member; the exact
; DefineBreakpoint semantics are NOT provable from these bytes.
115720  150401  	IOF			; interrupts/I-O off (enter critical section)
115721  133015  	JXZ 15		; -> 115736  (X == 0 -> skip edit, just ION/EXIT)
115722  014375  	STX -3
115723  146156  	RADD CLD SA DT
115724  146117  	RADD CLD SD DX
115725  024405  	LDD ,B 5
115726  143306  	STDTX			; store double at ,X
115727  173402  	AAX 2
115730  044367  	LDA -11
115731  143304  	STATX			; store A at ,X
115732  146165  	RADD CLD ST DA
115733  173776  	AAX -2
115734  146171  	RADD CLD SX DD
115735  020405  	STD ,B 5
115736  150402  	ION			; interrupts/I-O on (leave critical section)
115737  146142  	EXIT
