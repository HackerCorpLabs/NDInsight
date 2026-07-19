; ============================================================================
; MON 324B (octal) OctobusFunction - real SINTRAN L disassembly
;
; MON 324B is an ND-100 call (324B < 400B).  GOTAB[324B] = 112674B, which is the
; DSI8 entry stub in overlay 025-S3IRPIT (load base 32000B).  DSI8 is the
; Octobus level-14 handler: it validates the caller argument, folds the device
; number, and dispatches the Octobus operation through indirect worker pointer
; words ([113003B]..[113013B]) that live at the end of the DSI stub group.
;
;   Region A  DSI8 Octobus handler  025-S3IRPIT @112674B  (GOTAB target)
;             span 112674B..112722B (23 words)
;   Region B  DSI-group pointer/data words  025-S3IRPIT @113003B..113013B
;             (the JPL-I / JMP-I targets used by DSI8; final callees uncarved)
;
; The Octobus device I/O routines reached through those pointer words are NOT in
; any carved segment (resident/runtime Octobus driver) - see README Honest caveats.
;
; Extracted verbatim from the byte-identity-checked whole-segment disassembly
; ../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm .  Addresses and values octal.
; >>> NAME lines are in-range SYMBOL-2-LIST labels.
; ============================================================================

; ---- Region A: DSI8 Octobus level-14 handler (025-S3IRPIT, load 32000B) ----
>>> DSI8(SYMBOL-2-LIST) = 112674B
112674  054412  	LDX ,B 12		; X = ,B 12 (caller status/param slot)
112675  135106  	JPL I 106		; -> [113003] setup worker (pointer word)
112676  046001  	LDA ,X 1		; A = ,X 1 (function/param)
112677  135105  	JPL I 105		; -> [113004]
112700  004421  	STA ,B 21		; store result to ,B 21
112701  044417  	LDA ,B 17		; A = ,B 17 (device number)
112702  070103  	AND 103			; A &= mem[113005B] (device-number mask)
112703  060103  	ADD 103			; A += mem[113006B]  (fold/offset)
112704  004423  	STA ,B 23		; store folded device selector to ,B 23
112705  135102  	JPL I 102		; -> [113007] Octobus operation worker
112706  142001  	SKP IF DD UEQ 0		; skip if D != 0 (result present)
112707  125101  	JMP I 101		; -> [113010] no-result return
112710  146117  	RADD CLD SD DX		; X = D (result pointer)
112711  046001  	LDA ,X 1
112712  051077  	LDT I 77		; T = mem[[113011]]
112713  142065  	SKP IF DA UEQ ST	; skip if A != T
112714  124003  	JMP 3		; -> 112717
112715  170405  	SAA 5			; A = 5 (status code)
112716  125074  	JMP I 74		; -> [113012] status return
112717  046003  	LDA ,X 3
112720  175135  	BSKP ZRO 130 DA		; skip if A bit11 == 0
112721  125072  	JMP I 72		; -> [113013] return
112722  045072  	LDA I 72		; (falls into DSI9 region, MON boundary)

; ---- Region B: DSI-group pointer / data words (disassemble as bogus code) ----
; The JPL-I / JMP-I targets above; the final Octobus device workers live outside
; this carve (resident/runtime Octobus driver, uncarved).
113003  004177  	; setup worker pointer word
113004  004116  	; worker pointer word
113007  112130  	; Octobus operation worker pointer word
113010  112247  	; no-result return pointer word
113012  112250  	; status return pointer word
113013  002721  	; return pointer word (control-flow closure boundary)
