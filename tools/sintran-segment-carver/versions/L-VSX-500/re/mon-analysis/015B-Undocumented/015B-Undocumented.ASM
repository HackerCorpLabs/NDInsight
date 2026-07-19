; ============================================================================
; MON 15B (octal) Undocumented - real SINTRAN L disassembly
; MON 15B is NOT listed in the SINTRAN III Monitor Calls manual. Only the code
; is recovered; the high-level purpose is unknown.
;
; Two regions (the dispatch is 3-layer; see README Code location):
;   A. GOTAB[15] level-14 entry stub  F1615 @120501B  in segment 025-S3IRPIT (load 32000B)
;   B. First shared S3RPIT worker      @116234B         in segment 025-S3IRPIT (load 32000B)
;      (the stub reaches worker B via the pointer word @120536 = 116234B)
;
; The stub also calls two more shared S3RPIT workers via pointer words:
;   @120540 = 116665B  (JPL I 33)   @120541 = 116036B  (JPL I 23)
; and dereferences a data word @120537 = 061157B (LDX I 34 / LDX I 30).
; These shared workers sit BELOW the level-14 dispatch block and are entered by
; many stubs; only worker B (116234B, the first call) is listed in full here.
;
; All addresses and values are octal. Disassembled from 025-S3IRPIT.bin;
; addresses are the real octal load addresses via -b 13312 (= 32000B load base).
; ============================================================================

; ---- Region A: GOTAB[15] entry stub  F1615 @120501B .. 120525B (21 words) ----
; Next symbol F1616=120526B bounds the stub. Ends inside the shared dispatch
; block; DIRECT (JMP n) targets all land inside the block.
120501  135035  	JPL I 35		; -> ptr@120536 = 116234B  (call worker B)
120502  124011  	JMP 11		; -> 120513   (normal return from worker B)
120503  055034  	LDX I 34		; -> ptr@120537 = 061157B  (X := data word)
120504  046361  	LDA ,X -17
120505  135033  	JPL I 33		; -> ptr@120540 = 116665B  (call worker C)
120506  124001  	JMP 1		; -> 120507
120507  055030  	LDX I 30		; -> ptr@120537 = 061157B
120510  044347  	LDA -31		; -> data@120457 = 000000B
120511  146153  	RADD CLD SA DB
120512  125347  	JMP I -31		; -> ptr@120461 = 000000B  (runtime-populated; see README)
120513  171002  	SAT 2
120514  140065  	SKP IF DA EQL ST
120515  124004  	JMP 4		; -> 120521
120516  135023  	JPL I 23		; -> ptr@120541 = 116036B  (call worker D)
120517  144400  	RAND 0 0
120520  124357  	JMP -21		; -> 120477
120521  171001  	SAT 1
120522  140065  	SKP IF DA EQL ST
120523  124011  	JMP 11		; -> 120534
120524  175220  	BSKP ONE SSK
120525  124007  	JMP 7		; -> 120534

; ---- Pointer / data words used by Region A (data, not code) ----
; nd100-dis renders these as FDV/ADD/STZ; the raw octal IS the target address.
120457  000000  	; LDA -31 data word   = 000000B
120461  000000  	; JMP I -31 pointer   = 000000B  (ZERO - runtime-populated)
120536  116234  	; JPL I 35 pointer    -> worker B @116234B
120537  061157  	; LDX I 34/30 pointer -> data   @061157B
120540  116665  	; JPL I 33 pointer    -> worker C @116665B
120541  116036  	; JPL I 23 pointer    -> worker D @116036B

; ---- Region B: first shared S3RPIT worker  @116234B (JPL I 35 target) ----
; Shared level-14 worker called by many stubs. Its full extent is not cleanly
; bounded (no bounding symbol; multiple return paths); shown here from entry
; through its first two return sites for orientation. UNVERIFIED extent.
116234  004375  	STA -3
116235  146145  	RADD CLD SL DA
116236  004375  	STA -3
116237  010373  	STT -5
116240  044414  	LDA ,B 14
116241  131403  	JAF 3		; -> 116244
116242  135032  	JPL I 32		; -> ptr@116274
116243  124027  	JMP 27		; -> 116272
116244  050366  	LDT -12
116245  044417  	LDA ,B 17
116246  175205  	BSKP ONE 0 DA
116247  124003  	JMP 3		; -> 116252
116250  173003  	AAT 3
116251  124002  	JMP 2		; -> 116253
116252  173002  	AAT 2
116253  044446  	LDA ,B 46
116254  143065  	SKP IF DA LST ST
116255  124003  	JMP 3		; -> 116260
116256  170402  	SAA 2
116257  125354  	JMP I -24		; -> ptr@116233  (return path 1)
116260  044351  	LDA -27
116261  156410  	SHA ZIN 10
116262  135013  	JPL I 13		; -> ptr@116275
116263  044417  	LDA ,B 17
116264  172777  	AAA -1
116265  004402  	STA ,B 2
116266  044343  	LDA -35
116267  004404  	STA ,B 4
