; ============================================================================
; MON 155B (octal) GraphicFunction - real SINTRAN L disassembly
;
; MON 155B is an ND-100 call (155B < 400B).  GOTAB[155B] = 122461B, which is the
; label F1675 INSIDE the graphic-function dispatcher cluster F1674..F1677 in
; overlay 025-S3IRPIT (load base 32000B).  The cluster is the real ND-100
; graphic peripheral handler: it validates the Func code, then a computed jump
; (LDA ,B -2 / RADD SA DP) selects one of the graphic operations (PLOT / PLOTS /
; NEWP and coordinate variants), each of which drives graphic device registers
; through IRW/IRR after a JPL-I worker call.
;
;   Region A  Graphic dispatcher cluster  025-S3IRPIT @122434B..122557B
;             GOTAB entry point = F1675 = 122461B
;   Region B  Graphic worker pointer words  025-S3IRPIT @122616B..122621B
;             (the JPL-I targets; final callees uncarved)
;
; The graphic device workers reached through the JPL-I pointer words are NOT in
; any carved segment (resident/runtime graphic driver) - see README Honest caveats.
;
; Extracted verbatim from the byte-identity-checked whole-segment disassembly
; ../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm .  Addresses and values octal.
; >>> NAME lines are in-range SYMBOL-2-LIST labels.
; ============================================================================

; ---- Region A: graphic-function dispatcher (025-S3IRPIT, load 32000B) ----
>>> F1674(SYMBOL-2-LIST) = 122434B
122434  135162  	JPL I 162		; -> [122616] setup worker (pointer word)
122435  124152  	JMP 152		; -> 122607 (common tail)
122436  044775  	LDA ,B -3
122437  131403  	JAF 3		; -> 122442
122440  044157  	LDA 157			; A = mem[122617B] error code
122441  124146  	JMP 146		; -> 122607
122442  044404  	LDA ,B 4		; A = Func code (,B 4)
122443  171001  	SAT 1
122444  142065  	SKP IF DA UEQ ST	; skip if Func != 1
122445  124010  	JMP 10		; -> 122455
122446  135152  	JPL I 152		; -> [122620] Func 1 setup worker
122447  124114  	JMP 114		; -> 122563 (common tail)
122450  171401  	SAX 1
122451  142075  	SKP IF DA UEQ SX	; skip if Func != 1 (via X)
122452  124003  	JMP 3		; -> 122455
122453  170403  	SAA 3
122454  124107  	JMP 107		; -> 122563
122455  044423  	LDA ,B 23		; A = ,B 23 (device/state)
122456  131003  	JAZ 3		; -> 122461 (F1675 entry)
122457  171377  	SAT -1
122460  142065  	SKP IF DA UEQ ST
; >>> F1675 = 122461B  (GOTAB[155] entry point)
>>> F1675(SYMBOL-2-LIST) = 122461B
122461  124365  	JMP -13		; -> 122446 (enter Func-1 setup path)
122462  044776  	LDA ,B -2		; A = Code parameter (,B -2)
122463  146052  	RADD SA DP		; P = P + A : computed jump into table below
122464  124010  	JMP 10		; -> 122474  (code 0)
122465  124015  	JMP 15		; -> 122502  (code 1)
122466  124023  	JMP 23		; -> 122511  (code 2)
122467  124030  	JMP 30		; -> 122517  (code 3)
122470  124036  	JMP 36		; -> 122526  (code 4)
122471  124043  	JMP 43		; -> 122534  (code 5)
122472  124051  	JMP 51		; -> 122543  (code 6)
122473  124056  	JMP 56		; -> 122551  (code 7)
122474  135125  	JPL I 125		; -> [122621] code-0 worker
122475  124066  	JMP 66		; -> 122563
122476  004771  	STA ,B -7		; store operand
122477  156410  	SHA ZIN 10		; A <<= 8 (logical)
122500  153415  	IRW 10 DA		; reg[level 1][A] = A : write graphic device reg (A)
122501  040776  	MIN ,B -2
122502  135117  	JPL I 117		; -> [122621] code-1 worker
122503  124060  	JMP 60		; -> 122563
122504  004771  	STA ,B -7
122505  153615  	IRR 10 DA		; A = reg[level 1][A] : read graphic device reg (A)
122506  074771  	ORA ,B -7		; A |= ,B -7
122507  153415  	IRW 10 DA		; write graphic device reg (A)
122510  040776  	MIN ,B -2
122511  135110  	JPL I 110		; -> [122621] code-2 worker
122512  124051  	JMP 51		; -> 122563
122513  004771  	STA ,B -7
122514  156410  	SHA ZIN 10
122515  153411  	IRW 10 DD		; write graphic device reg (D)
122516  040776  	MIN ,B -2
122517  135102  	JPL I 102		; -> [122621] code-3 worker
122520  124043  	JMP 43		; -> 122563
122521  004771  	STA ,B -7
122522  153611  	IRR 10 DD		; read graphic device reg (D)
122523  074771  	ORA ,B -7
122524  153411  	IRW 10 DD
122525  040776  	MIN ,B -2
122526  135073  	JPL I 73		; -> [122621] code-4 worker
122527  124034  	JMP 34		; -> 122563
122530  004771  	STA ,B -7
122531  156410  	SHA ZIN 10
122532  153414  	IRW 10 DL		; write graphic device reg (L)
122533  040776  	MIN ,B -2
122534  135065  	JPL I 65		; -> [122621] code-5 worker
122535  124026  	JMP 26		; -> 122563
122536  004771  	STA ,B -7
122537  153614  	IRR 10 DL		; read graphic device reg (L)
122540  074771  	ORA ,B -7
122541  153414  	IRW 10 DL
122542  040776  	MIN ,B -2
122543  135056  	JPL I 56		; -> [122621] code-6 worker
122544  124017  	JMP 17		; -> 122563
122545  004771  	STA ,B -7
122546  156410  	SHA ZIN 10
122547  153417  	IRW 10 DX		; write graphic device reg (X)
122550  040776  	MIN ,B -2
122551  135050  	JPL I 50		; -> [122621] code-7 worker
122552  124011  	JMP 11		; -> 122563
122553  004771  	STA ,B -7
122554  153617  	IRR 10 DX		; read graphic device reg (X)
122555  074771  	ORA ,B -7
122556  153417  	IRW 10 DX
122557  000776  	STZ ,B -2		; clear ,B -2 (common tail merge)

; ---- Region B: graphic worker pointer words (disassemble as bogus code) ----
; The JPL-I targets above; final graphic-device workers live outside this carve.
122616  117143  	; setup worker pointer word
122617  000316  	; error-code constant
122620  116276  	; Func-1 setup worker pointer word
122621  116747  	; code-worker pointer word (control-flow closure boundary)
